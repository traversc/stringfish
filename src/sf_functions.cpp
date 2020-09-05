#include <unordered_map>
#include <fstream>

// xxhash can be used as "header only" library
// v0.74 XXH3 doesn't compile on Solaris due to restrict keyword? -- check back later
#include "xxhash/xxhash.c"

// Parallel stuff
#include <RcppParallel.h>
#include <atomic>

#if RCPP_PARALLEL_USE_TBB
#include <tbb/concurrent_unordered_map.h>
#include <tbb/task_arena.h>
#endif

#include <Rcpp.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Riconv.h>

#ifndef PCRE2_CODE_UNIT_WIDTH
#define PCRE2_CODE_UNIT_WIDTH 8
#endif
#ifndef HAVE_CONFIG_H
#define HAVE_CONFIG_H
#endif
#include "PCRE2/pcre2.h"

#include "sf_altrep.h"

using namespace Rcpp;
using namespace RcppParallel;

////////////////////////////////////////////////////////////////////////////////
// constants
static constexpr uint32_t R_MAX_INT = 2147483647; // R max (int) vector length

// is_utf8_locale controls whether CE_NATIVE strings are iconv'ed to utf-8 strings
// or used as is. Set during R package initialization
// but if R package isn't loaded, default of "false" is fine
static bool is_utf8_locale = false;
// [[Rcpp::export(rng = false)]]
void set_is_utf8_locale() {is_utf8_locale = true;}
// [[Rcpp::export(rng = false)]]
void unset_is_utf8_locale() {is_utf8_locale = false;}


////////////////////////////////////////////////////////////////////////////////
// tbb helper functions

#if RCPP_PARALLEL_USE_TBB
// inline void parallelFor2(std::size_t begin, std::size_t end, Worker& worker, std::size_t grainSize = 1, int nthreads = 1) {
//   int max_threads = tbb::task_scheduler_init::default_num_threads();
//   if(nthreads > max_threads) nthreads = max_threads;
//   tbb::task_arena limited(nthreads);
//   tbb::task_group tg;
//   limited.execute([&]{
//     tg.run([&]{
//       parallelFor(begin, end, worker, grainSize);
//     });
//   });
//   limited.execute([&]{ tg.wait(); });
// }

inline void parallelFor2(std::size_t begin, std::size_t end, Worker& worker, std::size_t grainSize = 1, int nthreads = 1) {
  parallelFor(begin, end, worker, grainSize);
}
#endif


// [[Rcpp::export(rng = false)]]
bool is_tbb() {
#if RCPP_PARALLEL_USE_TBB
  return true;
#endif
  return false;
}


////////////////////////////////////////////////////////////////////////////////
// iconv helper class

struct iconv_wrapper {
  const char * to;
  const char * from;
  void * cd;
  iconv_wrapper() : to(nullptr), from(nullptr), cd(nullptr) {
    // std::cout << "a";
  }
  iconv_wrapper(const char * to, const char * from) : to(to), from(from), cd(Riconv_open(to,from)) {
    // std::cout << "b";
  }
  std::pair<bool, std::string> convertToString(const char * ptr, size_t len) {
    std::string outstring;
    outstring.resize(len * 4);
    size_t outlen = outstring.size();
    char * outptr =  &outstring[0];
    size_t res = Riconv(cd, &ptr, &len, &outptr, &outlen);
    if(res == (size_t)(-1)) return std::make_pair(false, "");
    size_t bytes_written = outstring.size() - outlen;
    outstring.resize(bytes_written);
    return std::make_pair(true, outstring);
  }
  std::pair<bool, std::string> convertToString(const char * ptr) {
    size_t len = strlen(ptr);
    std::string outstring;
    outstring.resize(len * 4);
    size_t outlen = outstring.size();
    char * outptr =  &outstring[0];
    size_t res = Riconv(cd, &ptr, &len, &outptr, &outlen);
    if(res == (size_t)(-1)) return std::make_pair(false, "");
    size_t bytes_written = outstring.size() - outlen;
    outstring.resize(bytes_written);
    return std::make_pair(true, outstring);
  }
  bool convert(const char * ptr, size_t len, std::string& outstring) {
    outstring.resize(len * 4);
    size_t outlen = outstring.size();
    char * outptr =  &outstring[0];
    size_t res = Riconv(cd, &ptr, &len, &outptr, &outlen);
    if(res == (size_t)(-1)) return false;
    size_t bytes_written = outstring.size() - outlen;
    outstring.resize(bytes_written);
    return true;
  }
  bool convert(const char * ptr, std::string& outstring) {
    size_t len = strlen(ptr);
    outstring.resize(len * 4);
    size_t outlen = outstring.size();
    char * outptr =  &outstring[0];
    size_t res = Riconv(cd, &ptr, &len, &outptr, &outlen);
    if(res == (size_t)(-1)) return false;
    size_t bytes_written = outstring.size() - outlen;
    outstring.resize(bytes_written);
    return true;
  }
  iconv_wrapper(const iconv_wrapper& other) { // copy constructor
    // std::cout << "c";
    to = other.to;
    from = other.from;
    if(to != nullptr) {
      cd = Riconv_open(to, from);
    } else {
      cd = nullptr;
    }
  }
  iconv_wrapper(iconv_wrapper && other) { // move constructor
    // std::cout << "d";
    to = other.to;
    from = other.from;
    cd = other.cd;
    other.cd = nullptr;
  }
  iconv_wrapper & operator=(const iconv_wrapper & other) { // copy assignment
    // std::cout << "e";
    if(&other == this) return *this;
    if(cd != nullptr) Riconv_close(cd);
    to = other.to;
    from = other.from;
    if(to != nullptr) {
      cd = Riconv_open(to, from);
    } else {
      cd = nullptr;
    }
    return *this;
  }
  iconv_wrapper & operator=(iconv_wrapper && other) { // move assignment
    // std::cout << "f";
    if(&other == this) return *this;
    if(cd != nullptr) Riconv_close(cd);
    to = other.to;
    from = other.from;
    cd = other.cd;
    other.cd = nullptr;
    return *this;
  }
  ~iconv_wrapper() {
    // std::cout << "g";
    if(cd != nullptr) Riconv_close(cd);
  }
};

static const std::string iconv_utf8_string = "UTF-8";
static const std::string iconv_latin1_string = "latin1";
static const std::string iconv_native_string = "";

////////////////////////////////////////////////////////////////////////////////
// pcre helper classes

struct pcre2_match_wrapper {
  pcre2_code * re;
  pcre2_match_data * match_data;
  pcre2_match_wrapper(const char * pattern_ptr, bool utf8, bool literal = false) {
    int errorcode;
    PCRE2_SIZE erroroffset;
    uint32_t flags = (utf8 ? PCRE2_UTF : 0) | (literal ? PCRE2_LITERAL : 0);
    re = pcre2_compile((PCRE2_SPTR)pattern_ptr, // pattern
                       PCRE2_ZERO_TERMINATED, // length
                       flags,
                       &errorcode, // error reporting
                       &erroroffset, // error reporting
                       NULL // compile context
    );
    if(re == NULL) {
      PCRE2_UCHAR buffer[256];
      pcre2_get_error_message(errorcode, buffer, sizeof(buffer));
      throw std::runtime_error("PCRE2 pattern error: " + std::to_string((int)erroroffset) + std::string((char*)buffer));
    }
    pcre2_jit_compile(re, PCRE2_JIT_COMPLETE);
    match_data = pcre2_match_data_create_from_pattern(re, NULL);
  }
  pcre2_match_wrapper() : re(nullptr), match_data(nullptr) {}
  
  pcre2_match_wrapper(const pcre2_match_wrapper& other) : 
    re(pcre2_code_copy_with_tables(other.re)), match_data(pcre2_match_data_create_from_pattern(other.re, NULL)) { // copy constructor
    pcre2_jit_compile(re, PCRE2_JIT_COMPLETE);
  }
  pcre2_match_wrapper(pcre2_match_wrapper && other) { // move constructor
    re = other.re;
    match_data = other.match_data;
    other.re = nullptr;
    other.match_data = nullptr;
  }
  pcre2_match_wrapper & operator=(const pcre2_match_wrapper & other) { // copy assignment
    if(&other == this) return *this;
    if(re != nullptr) pcre2_code_free(re);
    re = pcre2_code_copy_with_tables(other.re);
    pcre2_jit_compile(re, PCRE2_JIT_COMPLETE);
    match_data = pcre2_match_data_create_from_pattern(re, NULL);
    return *this;
  }
  pcre2_match_wrapper & operator=(pcre2_match_wrapper && other) { // move assignment
    if(&other == this) return *this;
    if(re != nullptr) pcre2_code_free(re);
    if(match_data != nullptr) pcre2_match_data_free(match_data);
    re = other.re;
    match_data = other.match_data;
    other.re = nullptr;
    other.match_data = nullptr;
    return *this;
  }
  
  ~pcre2_match_wrapper() {
    if(re != nullptr) pcre2_code_free(re);
    if(match_data != nullptr) pcre2_match_data_free(match_data);
  }
  inline int match(const char * subject_ptr, const int len) {
    int rc = pcre2_jit_match(re, // compiled pattern
                         (PCRE2_SPTR)subject_ptr, // subject
                         PCRE2_SIZE(len), // PCRE2_ZERO_TERMINATED, // length
                         0, // start offset
                         0, // options
                         match_data,  // match data block
                         NULL // match context
    );
    if(rc == PCRE2_ERROR_NOMATCH) {
      return 0;
    } else if(rc < 0) {
      throw std::runtime_error("error matching string");
    } else {
      return 1;
    }
  }
  inline int match_for_split(const char * subject_ptr, const int len) {
    int rc = pcre2_jit_match(re, // compiled pattern
                         (PCRE2_SPTR)subject_ptr, // subject
                         PCRE2_SIZE(len), // PCRE2_ZERO_TERMINATED, // length
                         0, // start offset
                         PCRE2_NOTEMPTY_ATSTART, // disallows empty match at the start, so while loop doesn't go infinitely
                         match_data,  // match data block
                         NULL // match context
    );
    if(rc == PCRE2_ERROR_NOMATCH) {
      return 0;
    } else if(rc < 0) {
      throw std::runtime_error("error matching string");
    } else {
      return 1;
    }
  }
  inline PCRE2_SIZE * match_ovector() {
    return pcre2_get_ovector_pointer(match_data);
  }
};

////////////////////////////////////////////////////////////////////////////////

struct pcre2_sub_wrapper {
  pcre2_code * re;
  pcre2_match_data * match_data;
  PCRE2_SPTR replacement;
  std::vector<char> output;
  pcre2_sub_wrapper(const char * pattern_ptr, const char * replacement_ptr, bool utf8, bool literal = false) : 
    output(std::vector<char>(20)) {
    int errorcode;
    PCRE2_SIZE erroroffset;
    uint32_t flags = (utf8 ? PCRE2_UTF : 0) | (literal ? PCRE2_LITERAL : 0);
    re = pcre2_compile((PCRE2_SPTR)pattern_ptr, // pattern
                       PCRE2_ZERO_TERMINATED, // length
                       flags,
                       &errorcode, // error reporting
                       &erroroffset, // error reporting
                       NULL // compile context
    );
    if(re == NULL) {
      PCRE2_UCHAR buffer[256];
      pcre2_get_error_message(errorcode, buffer, sizeof(buffer));
      throw std::runtime_error("PCRE2 pattern error: " + std::to_string((int)erroroffset) + std::string((char*)buffer));
    }
    pcre2_jit_compile(re, PCRE2_JIT_COMPLETE);
    match_data = pcre2_match_data_create_from_pattern(re, NULL);
    replacement = (PCRE2_SPTR)replacement_ptr;
  }
  pcre2_sub_wrapper() : re(nullptr), match_data(nullptr), replacement(nullptr) {}
  pcre2_sub_wrapper & operator=(const pcre2_sub_wrapper & other) { // copy assignment
    if(&other == this) return *this;
    re = pcre2_code_copy_with_tables(other.re);
    pcre2_jit_compile(re, PCRE2_JIT_COMPLETE);
    output = other.output;
    match_data = pcre2_match_data_create_from_pattern(re, NULL);
    replacement = other.replacement;
    return *this;
  }
  pcre2_sub_wrapper & operator=(pcre2_sub_wrapper && other) { // move assignment
    if(&other == this) return *this;
    re = other.re;
    replacement = other.replacement;
    output = std::move(other.output);
    match_data = other.match_data;
    other.re = nullptr;
    other.match_data = nullptr;
    return *this;
  }
  pcre2_sub_wrapper(const pcre2_sub_wrapper& other) { // copy constructor
    re = pcre2_code_copy_with_tables(other.re);
    pcre2_jit_compile(re, PCRE2_JIT_COMPLETE);
    output = other.output;
    match_data = pcre2_match_data_create_from_pattern(re, NULL);
    replacement = other.replacement;
  }
  pcre2_sub_wrapper(pcre2_sub_wrapper && other) { // move constructor
    re = other.re;
    replacement = other.replacement;
    output = std::move(other.output);
    match_data = other.match_data;
    other.re = nullptr;
    other.match_data = nullptr;
  }
  ~pcre2_sub_wrapper() {
    if(re != nullptr) pcre2_code_free(re);
    if(match_data != nullptr) pcre2_match_data_free(match_data);
  }
  const char * gsub(const char * subject_ptr) {
    PCRE2_SIZE output_len = (PCRE2_SIZE)(output.size() - 1);
    int rc = pcre2_substitute(
      re,                       // Points to the compiled pattern
      (PCRE2_SPTR)subject_ptr,  // Points to the subject string
      PCRE2_ZERO_TERMINATED,    // Length of the subject string
      0,                        // Offset in the subject at which to start matching
      PCRE2_SUBSTITUTE_GLOBAL | PCRE2_SUBSTITUTE_OVERFLOW_LENGTH, // Option bits
      match_data,                     // Points to a match data block, or is NULL
      NULL,                     // Points to a match context, or is NULL
      replacement,              // Points to the replacement string
      PCRE2_ZERO_TERMINATED,    // Length of the replacement string
      (PCRE2_UCHAR*)output.data(), //  Points to the output buffer
      &output_len
    );
    // std::cout << PCRE2_ERROR_NOMEMORY << std::endl;
    // std::cout << rc << " " << output_len << std::endl;
    // std::string errmsg;
    // errmsg.resize(300);
    // pcre2_get_error_message(rc, (PCRE2_UCHAR *)&errmsg[0], (PCRE2_SIZE)300);
    // std::cout << errmsg << std::endl;
    if(rc == PCRE2_ERROR_NOMEMORY) {
      output.resize(output_len + 1);
      rc = pcre2_substitute(
        re,                       // Points to the compiled pattern
        (PCRE2_SPTR)subject_ptr,  // Points to the subject string
        PCRE2_ZERO_TERMINATED,    // Length of the subject string
        0,                        // Offset in the subject at which to start matching
        PCRE2_SUBSTITUTE_GLOBAL,  // Option bits
        match_data,                // Points to a match data block, or is NULL
        NULL,                     // Points to a match context, or is NULL
        replacement,              // Points to the replacement string
        PCRE2_ZERO_TERMINATED,    // Length of the replacement string
        (PCRE2_UCHAR*)output.data(), //  Points to the output buffer
        &output_len
      );
    }
    if(rc < 0) {
      throw std::runtime_error("error matching string: check for matching encoding and proper regex syntax");
    }
    return output.data();
  }
};

////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export(rng = false)]]
std::string get_string_type(SEXP x) {
  rstring_type rx = get_rstring_type_internal(x);
  switch(rx) {
  case rstring_type::NORMAL:
    return "normal vector";
  case rstring_type::SF_VEC:
    return "stringfish vector";
  case rstring_type::SF_VEC_MATERIALIZED:
    return "stringfish vector (materialized)";
  case rstring_type::OTHER_ALT_REP:
    return "other alt-rep vector";
  default:
    throw std::runtime_error("get_string_type error");
  }
}

////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export(rng = false)]]
SEXP materialize(SEXP x) {
  if(ALTREP(x)) {
    ALTVEC_DATAPTR(x);
  }
  return x;
}


////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export(rng = false)]]
SEXP sf_vector(size_t len) {
  auto ret = new sf_vec_data(len);
  return sf_vec::Make(ret, true);
}


////////////////////////////////////////////////////////////////////////////////

// C only export manually - no checking of valid object is done here
sf_vec_data & sf_vec_data_ref(SEXP x) {
  return *reinterpret_cast<sf_vec_data*>(R_ExternalPtrAddr(R_altrep_data1(x)));
}

////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export(rng = false)]]
void sf_assign(SEXP x, size_t i, SEXP e) {
  if(TYPEOF(e) != STRSXP || Rf_xlength(e) != 1) {
    throw std::runtime_error("e must be a string of length 1");
  }
  if(i == 0) {
    throw std::runtime_error("i must be > 0");
  }
  i--;
  rstring_type rx = get_rstring_type_internal(x);
  switch(rx) {
  case rstring_type::SF_VEC:
  {
    sf_vec_data & ref = sf_vec_data_ref(x);
    ref[i] = sfstring(STRING_ELT(e,0));
  }
    break;
  case rstring_type::SF_VEC_MATERIALIZED:
    SET_STRING_ELT(R_altrep_data2(x), i, STRING_ELT(e,0));
    break;
  case rstring_type::NORMAL:
  case rstring_type::OTHER_ALT_REP:
  default:
    SET_STRING_ELT(x, i, STRING_ELT(e,0));
    break;
  }
}

////////////////////////////////////////////////////////////////////////////////

#if RCPP_PARALLEL_USE_TBB
struct iconv_worker : public Worker {
  tbb::enumerable_thread_specific<iconv_wrapper> iw;
  cetype_t encoding;
  RStringIndexer * rsi;
  sf_vec_data & ref;
  iconv_worker(iconv_wrapper iw, cetype_t encoding, RStringIndexer * rsi, sf_vec_data & ref) : 
    iw(iw), encoding(encoding), rsi(rsi), ref(ref) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    tbb::enumerable_thread_specific<iconv_wrapper>::reference iw_local = iw.local();
    for(size_t i=begin; i<end; i++) {
      auto q = rsi->getCharLenCE(i);
      if(q.ptr == nullptr) {
        ref[i] = sfstring(NA_STRING);
      } else {
        auto sc = iw_local.convertToString(q.ptr, q.len);
        if(!sc.first) {
          ref[i] = sfstring(NA_STRING);
        } else {
          ref[i] = sfstring(std::move(sc.second), encoding);
        }
      }
    }
  }
};
#endif

// [[Rcpp::export(rng = false)]]
SEXP sf_iconv(SEXP x, const std::string from, const std::string to, int nthreads=1) {
  cetype_t encoding;
  if(to == "UTF-8") {
    encoding = CE_UTF8;
  } else if(to == "latin1") {
    encoding = CE_LATIN1;
  } else {
    encoding = CE_NATIVE;
  }
  iconv_wrapper iw(to.c_str(), from.c_str());
  RStringIndexer rsi(x);
  size_t len = rsi.size();
  SEXP ret = PROTECT(sf_vector(len));
  sf_vec_data & ref = sf_vec_data_ref(ret);
  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    iconv_worker w(iw, encoding, &rsi, ref);
    parallelFor2(0, len, w, 100, nthreads);
#else
    throw std::runtime_error("RcppParallel TBB not supported");
#endif
  }
  for(size_t i=0; i<len; i++) {
    auto q = rsi.getCharLenCE(i);
    if(q.ptr == nullptr) {
      ref[i] = sfstring(NA_STRING);
    } else {
      auto sc = iw.convertToString(q.ptr, q.len);
      if(!sc.first) {
        ref[i] = sfstring(NA_STRING);
      } else {
        ref[i] = sfstring(std::move(sc.second), encoding);
      }
    }
  }
  UNPROTECT(1);
  return ret;
}

////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export(rng = false)]]
SEXP convert_to_sf(SEXP x) {
  size_t len = Rf_xlength(x);
  SEXP ret = PROTECT(sf_vector(len));
  sf_vec_data & ref = sf_vec_data_ref(ret);
  for(size_t i=0; i<len; i++) {
    ref[i] = sfstring(STRING_ELT(x,i));
  }
  UNPROTECT(1);
  return ret;
}

////////////////////////////////////////////////////////////////////////////////
#if RCPP_PARALLEL_USE_TBB
struct nchar_worker : public Worker {
  RStringIndexer * rsi;
  int * optr;
  const std::string type;
  nchar_worker(RStringIndexer * r, int * o, const std::string t) : 
    rsi(r), optr(o), type(t) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    if(type == "chars") {
      for(size_t i=begin; i<end; i++) {
        auto q = rsi->getCharLenCE(i);
        if(q.ptr == nullptr) {
          optr[i] = NA_INTEGER;
        } else if(q.enc == CE_UTF8) {
          optr[i] = code_points(q.ptr);
        } else {
          optr[i] = static_cast<int>( strlen(q.ptr) );
        }
      }
    } else if(type == "bytes") {
      for(size_t i=begin; i<end; i++) {
        auto q = rsi->getCharLenCE(i);
        if(q.ptr == nullptr) {
          optr[i] = NA_INTEGER;
        } else {
          optr[i] = static_cast<int>( strlen(q.ptr) );
        }
      }
    }
  }
};
#endif

// [[Rcpp::export(rng = false)]]
IntegerVector sf_nchar(SEXP x, const std::string type = "chars", const int nthreads = 1) {
  if((type != "chars") && (type != "bytes")) {
    throw std::runtime_error("type must be chars or bytes");
  }
  RStringIndexer rsi(x);
  size_t len = rsi.size();
  IntegerVector ret(len);
  int * optr = INTEGER(ret);
  
  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    nchar_worker w(&rsi, optr, type);
    parallelFor2(0, len, w, 100, nthreads);
#else
    throw std::runtime_error("RcppParallel TBB not supported");
#endif
  } else {
    if(type == "chars") {
      for(size_t i=0; i<len; i++) {
        auto q = rsi.getCharLenCE(i);
        if(q.ptr == nullptr) {
          optr[i] = NA_INTEGER;
        } else if(q.enc == CE_UTF8) {
          optr[i] = code_points(q.ptr);
        } else {
          optr[i] = static_cast<int>( strlen(q.ptr) );
        }
      }
    } else if(type == "bytes") {
      for(size_t i=0; i<len; i++) {
        auto q = rsi.getCharLenCE(i);
        if(q.ptr == nullptr) {
          optr[i] = NA_INTEGER;
        } else {
          optr[i] = static_cast<int>( strlen(q.ptr) );
        }
      }
    }
  }
  return ret;
}

////////////////////////////////////////////////////////////////////////////////

inline sfstring sf_substr_internal(const char * x, const int len, const cetype_t type, int start, int stop) {
  if(x == nullptr) return sfstring(NA_STRING);
  if(len == 0) return sfstring("", CE_NATIVE);
  if(start > len) return sfstring("", CE_NATIVE);
  
  if(type == CE_UTF8) {
    int clen = code_points(x);
    if(start > clen) return sfstring("", CE_NATIVE);
    // for utf-8, stop is one-based
    // we want the end of the last byte for the last character so we overshoot 
    // by one byte to find the boundary
    if(start < 0 || stop < 0) {
      start = start < 0 ? (clen+start) : (start-1);
      stop = stop < 0 ? (clen+stop+1) : (stop); // zero based
    } else {
      start = start-1;
      // stop = stop;
    }
    
    if((stop-1) < start) return sfstring("", type);
    if(stop < 1) return sfstring("", type);
    if(start < 0) start = 0;
    
    const char * p = x;
    int start_byte = 0;
    int outlen = 0;
    int count = 0;
    while(count <= start) {
      if(*p == 0) return sfstring("", type);
      count += ((*p & 0xc0) != 0x80); // b11000000, b10000000
      p++;
      start_byte++;
    }
    while(count <= stop) {
      if(*p == 0) {
        outlen++;
        break;
      }
      count += ((*p & 0xc0) != 0x80); // b11000000, b10000000
      p++;
      outlen++;
    }
    return sfstring(std::string(x + start_byte - 1, outlen), type);
  } else {
    start = start < 0 ? (len+start) : (start-1); // zero based
    stop = stop < 0 ? (len+stop) : (stop-1); // zero based
    if(stop < start) return sfstring("", CE_NATIVE);
    if(stop >= len) stop = len - 1;
    if(stop < 0) return sfstring("", CE_NATIVE);
    if(start < 0) start = 0;
    return sfstring(std::string(x + start, stop - start + 1), type);
  }
}

struct substr_worker : public Worker {
  const std::string type;
  RStringIndexer * rsi;
  size_t start_size;
  size_t stop_size;
  int * start_ptr;
  int * stop_ptr;
  sf_vec_data & ref;
  substr_worker(RStringIndexer * r, size_t start_s, size_t stop_s, int * start_p, int * stop_p, sf_vec_data & ref) : 
    rsi(r), start_size(start_s), stop_size(stop_s), start_ptr(start_p), stop_ptr(stop_p), ref(ref) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for(size_t i=begin; i<end; i++) {
      auto q = rsi->getCharLenCE(i);
      ref[i] = sf_substr_internal(q.ptr, q.len, q.enc, start_ptr[start_size == 1 ? 0 : i], stop_ptr[stop_size == 1 ? 0 : i]);
    }
  }
};

// [[Rcpp::export(rng = false)]]
SEXP sf_substr(SEXP x, IntegerVector start, IntegerVector stop, const int nthreads = 1) {
  size_t start_size = Rf_xlength(start);
  size_t stop_size = Rf_xlength(stop);
  int * start_ptr = INTEGER(start);
  int * stop_ptr = INTEGER(stop);
  
  for(size_t i=0; i<start_size; i++) {
    if(start_ptr[i] == NA_INTEGER) throw std::runtime_error("no NA start values allowed");
  }
  
  for(size_t i=0; i<start_size; i++) {
    if(stop_ptr[i] == NA_INTEGER) throw std::runtime_error("no NA stop values allowed");
  }
  
  RStringIndexer rsi(x);
  size_t len = rsi.size();
  if(start_size != len && start_size != 1) throw std::runtime_error("length of start must be 1 or the same as x");
  if(stop_size != len && stop_size != 1) throw std::runtime_error("length of stop must be 1 or the same as x");
  SEXP ret = PROTECT(sf_vector(len));
  sf_vec_data & ref = sf_vec_data_ref(ret);
  
  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    substr_worker w(&rsi, start_size, stop_size, start_ptr, stop_ptr, ref);
    parallelFor2(0, len, w, 100, nthreads);
#else
    throw std::runtime_error("RcppParallel TBB not supported");
#endif
  } else {
    for(size_t i=0; i<len; i++) {
      auto q = rsi.getCharLenCE(i);
      ref[i] = sf_substr_internal(q.ptr, q.len, q.enc, start_ptr[start_size == 1 ? 0 : i], stop_ptr[stop_size == 1 ? 0 : i]);
    }
  }
  UNPROTECT(1);
  return ret;
}

////////////////////////////////////////////////////////////////////////////////

struct paste_worker : public Worker {
  size_t dotlen;
  std::string & sep_string;
  std::vector<RStringIndexer> & rs;
  std::vector<size_t> & lens;
  std::vector<RStringIndexer::rstring_info> & singles;
  sf_vec_data & ref;

  paste_worker(size_t dotlen, std::string& sep_string, std::vector<RStringIndexer> & rs, 
               std::vector<size_t> & lens, std::vector<RStringIndexer::rstring_info> & singles,
               sf_vec_data & ref) : 
    dotlen(dotlen), sep_string(sep_string), rs(rs), lens(lens), singles(singles), ref(ref) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for(size_t i=begin; i<end; i++) {
      std::string temp;
      cetype_t enc = CE_NATIVE;
      bool is_na = false;
      for(size_t j=0; j<dotlen; j++) {
        RStringIndexer::rstring_info q = lens[j] == 1 ? singles[j] : rs[j].getCharLenCE(i);
        if(q.ptr == nullptr) {
          is_na = true;
          break;
        } else {
          enc = choose_enc(enc, q.enc);
          temp += std::string(q.ptr, q.len);
          if(j < (dotlen-1)) temp += sep_string;
        }
      }
      if(is_na) {
        ref[i] = sfstring(NA_STRING);
      } else {
        ref[i] = sfstring(temp, enc);
      }
    }
  }
};

// Would it be better to upgrade everything to UTF-8?
// Called by sf_paste -- valid input already determined
// [[Rcpp::export(rng = false)]]
SEXP c_sf_paste(List dots, SEXP sep, const int nthreads = 1) {

  RStringIndexer sr(sep);
  if(sr.size() != 1) throw std::runtime_error("sep should be a character vector of length 1");
  auto sr_element = sr.getCharLenCE(0);
  
  std::string sep_string(sr_element.ptr, sr_element.len);
  size_t maxlen = 1;
  size_t dotlen = Rf_xlength(dots);
  std::vector<RStringIndexer> rs;
  std::vector<size_t> lens(dotlen);
  std::vector<RStringIndexer::rstring_info> singles(dotlen);
  for(size_t i=0; i<dotlen; i++) {
    SEXP d = dots[i];
    rs.emplace_back(d); // no copy constructor called, since it s deleted
    lens[i] = rs[i].size();
    if(lens[i] == 1) {
      singles[i] = rs[i].getCharLenCE(0);
    }
    if(maxlen == 1 && lens[i] > 1) {
      maxlen = lens[i];
    }
  }
  
  SEXP ret = PROTECT(sf_vector(maxlen));
  sf_vec_data & ref = sf_vec_data_ref(ret);
  
  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    paste_worker w(dotlen, sep_string, rs, lens, singles, ref);
    parallelFor2(0,maxlen,w, 100, nthreads);
#else
    throw std::runtime_error("RcppParallel TBB not supported");
#endif
  } else {
    for(size_t i=0; i<maxlen; i++) {
      std::string temp;
      cetype_t enc = CE_NATIVE;
      bool is_na = false;
      for(size_t j=0; j<dotlen; j++) {
        RStringIndexer::rstring_info q = lens[j] == 1 ? singles[j] : rs[j].getCharLenCE(i);
        if(q.ptr == nullptr) {
          is_na = true;
          break;
        } else {
          enc = choose_enc(enc, q.enc);
          temp += std::string(q.ptr, q.len);
          if(j < (dotlen-1)) temp += sep_string;
        }
      }
      if(is_na) {
        ref[i] = sfstring(NA_STRING);
      } else {
        ref[i] = sfstring(temp, enc);
      }
    }
  }
  UNPROTECT(1);
  return ret;
}

////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export(rng = false)]]
SEXP sf_collapse(SEXP x, SEXP collapse) {
  RStringIndexer cr(collapse);
  if(cr.size() != 1) throw std::runtime_error("collapse should be a character vector of length 1");
  auto cr_element = cr.getCharLenCE(0);
  std::string collapse_string(cr_element.ptr, cr_element.len);
  
  RStringIndexer xr(x);
  size_t len = xr.size();
  std::string temp;
  cetype_t enc = cr_element.enc;
  for(size_t i=0; i<len; i++) {
    auto q = xr.getCharLenCE(i);
    if(q.ptr == nullptr) return NA_STRING;
    enc = choose_enc(enc, q.enc);
    temp += std::string(q.ptr, q.len);
    if(i < (len-1)) temp += collapse_string;
  }
  // since the return length is 1, there's no point using ALT-REP
  SEXP ret = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(ret, 0, Rf_mkCharLenCE(temp.c_str(), temp.size(), enc));
  UNPROTECT(1);
  return(ret);
}

////////////////////////////////////////////////////////////////////////////////
// [[Rcpp::export(rng = false)]]
SEXP sf_readLines(const std::string file, const std::string encoding = "UTF-8") {
  SEXP ret = PROTECT(sf_vector(0));
  sf_vec_data & ref = sf_vec_data_ref(ret);
  cetype_t enc;
  if(encoding == "UTF-8") {
    enc = CE_UTF8;
  } else if(encoding == "latin1") {
    enc = CE_LATIN1;
  } else if(encoding == "bytes") {
    enc = CE_BYTES;
  } else {
    enc = CE_NATIVE;
  }
  std::ifstream myFile(R_ExpandFileName(file.c_str()), std::ios::in);
  if(!myFile) {
    throw std::runtime_error("Failed to open " + file + ". Check file path.");
  }
  
  // possibly better way of handling line endings: https://stackoverflow.com/q/6089231/2723734
  std::string str;
  while(std::getline(myFile, str)) {
    if(str.size() > 0 && str.back() == '\r') {
      str.resize(str.size() - 1);
    }
    ref.push_back(sfstring(str, enc));
  }
  UNPROTECT(1);
  return ret;
}

////////////////////////////////////////////////////////////////////////////////
// [[Rcpp::export(rng = false)]]
void sf_writeLines(SEXP text, const std::string file, const std::string sep = "\n", const std::string na_value = "NA", const std::string encode_mode = "UTF-8") {
  if((encode_mode != "UTF-8") && (encode_mode != "byte")) {
    throw std::runtime_error("encode_mode must be byte or UTF-8");
  }
  std::ofstream myFile(R_ExpandFileName(file.c_str()), std::ios::out | std::ios::binary);
  if(!myFile) {
    throw std::runtime_error("Failed to open " + file + ". Check file path.");
  }
  
  iconv_wrapper iw_latin1;
  iconv_wrapper iw_native;
  if((encode_mode == "UTF-8")) {
    iw_latin1 = iconv_wrapper("UTF-8", "latin1");
    if(!is_utf8_locale) {iw_native = iconv_wrapper("UTF-8", "");}
  }
  
  RStringIndexer xr(text);
  size_t len = xr.size();
  
  for(size_t i=0; i<len; i++) {
    auto q = xr.getCharLenCE(i);
    if(q.ptr == nullptr) {
      myFile.write(na_value.c_str(), na_value.size());
    } else {
      if(encode_mode == "byte") {
        myFile.write(q.ptr, q.len);
      } else { // UTF-8
        if(q.enc == CE_NATIVE) {
          if(!is_utf8_locale && !xr.is_ASCII(i)) {
            auto iw = iw_native.convertToString(q.ptr, q.len);
            if(!iw.first) {
              myFile.write(na_value.c_str(), na_value.size());
            } else {
              myFile.write(iw.second.c_str(), iw.second.size());
            }
          } else {
            myFile.write(q.ptr, q.len);
          }
        } else if(q.enc == CE_LATIN1) {
          auto iw = iw_latin1.convertToString(q.ptr, q.len);
          if(!iw.first) {
            myFile.write(na_value.c_str(), na_value.size());
          } else {
            myFile.write(iw.second.c_str(), iw.second.size());
          }
        } else {
          myFile.write(q.ptr, q.len);
        }
      }
    }
    myFile.write(sep.c_str(), sep.size());
  }
  
}


////////////////////////////////////////////////////////////////////////////////

struct grepl_worker : public Worker {
  const std::string encode_mode;
  tbb::enumerable_thread_specific<iconv_wrapper> iw_latin1;
  tbb::enumerable_thread_specific<iconv_wrapper> iw_native;
  tbb::enumerable_thread_specific<pcre2_match_wrapper> pm;
  RStringIndexer * cr;
  int * outptr;
  
  grepl_worker(const std::string encode_mode, iconv_wrapper iw_latin1, iconv_wrapper iw_native, 
               const pcre2_match_wrapper & pm, RStringIndexer * cr, int * outptr) : 
    encode_mode(encode_mode), iw_latin1(iw_latin1), iw_native(iw_native), pm(pm), cr(cr), outptr(outptr) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    tbb::enumerable_thread_specific<pcre2_match_wrapper>::reference pm_local = pm.local();
    tbb::enumerable_thread_specific<iconv_wrapper>::reference iw_latin1_local = iw_latin1.local();
    tbb::enumerable_thread_specific<iconv_wrapper>::reference iw_native_local = iw_native.local();
    for(size_t i=begin; i<end; i++) {
      auto q = cr->getCharLenCE(i);
      if(q.ptr == nullptr) {
        outptr[i] = NA_LOGICAL;
        continue;
      }
      if(encode_mode == "UTF-8") {
        outptr[i] = pm_local.match(q.ptr, q.len);
      } else if(encode_mode == "byte") {
        outptr[i] = pm_local.match(q.ptr, q.len);
      } else { // auto - upscale everything to UTF-8
        if(q.enc == CE_NATIVE) {
          if(!is_utf8_locale && !cr->is_ASCII(i)) {
            auto istr = iw_native_local.convertToString(q.ptr, q.len);
            if(!istr.first) {
              outptr[i] = NA_LOGICAL;
            } else {
              outptr[i] = pm_local.match(istr.second.c_str(), istr.second.size());
            }
          } else {
            outptr[i] = pm_local.match(q.ptr, q.len);
          }
        } else if(q.enc == CE_LATIN1) {
          auto istr = iw_latin1_local.convertToString(q.ptr, q.len);
          if(!istr.first) {
            outptr[i] = NA_LOGICAL;
          } else {
            outptr[i] = pm_local.match(istr.second.c_str(), istr.second.size());
          }
        } else {
          outptr[i] = pm_local.match(q.ptr, q.len);
        }
      }
    }
  }
};

// [[Rcpp::export(rng = false)]]
LogicalVector sf_grepl(SEXP subject, SEXP pattern, const std::string encode_mode = "auto", const bool fixed = false ,const int nthreads = 1) {
  if(encode_mode != "auto" && encode_mode != "byte" && encode_mode != "UTF-8") {
    throw std::runtime_error("encode_mode must be auto, byte or UTF-8");
  }
  
  SEXP pattern_element = STRING_ELT(pattern, 0);
  const char * pattern_ptr = CHAR(pattern_element);
  cetype_t pattern_enc = Rf_getCharCE(pattern_element);
  
  iconv_wrapper iw_latin1;
  iconv_wrapper iw_native;
  pcre2_match_wrapper pm;
  std::string pattern_str;
  if((encode_mode == "auto")) {
    iw_latin1 = iconv_wrapper("UTF-8", "latin1");
    if(!is_utf8_locale) {iw_native = iconv_wrapper("UTF-8", "");}
    
    if( (!is_utf8_locale && (pattern_enc == CE_NATIVE) && !IS_ASCII(pattern_element)) ) {
      pattern_str = iw_native.convertToString(pattern_ptr).second;
      pattern_ptr = pattern_str.c_str();
    } else if(pattern_enc == CE_LATIN1) {
      pattern_str = iw_latin1.convertToString(pattern_ptr).second;
      pattern_ptr = pattern_str.c_str();
    }
    pm = pcre2_match_wrapper(pattern_ptr, true, fixed);
  } else if(encode_mode == "UTF-8") {
    pm = pcre2_match_wrapper(pattern_ptr, true, fixed);
  }else if(encode_mode == "byte") {
    pm = pcre2_match_wrapper(pattern_ptr, false, fixed);
  } else {
    throw std::runtime_error("encode_mode must be auto, byte or UTF-8");
  }
  
  RStringIndexer cr(subject);
  size_t len = cr.size();
  LogicalVector ret(len);
  int * outptr = LOGICAL(ret);
  
  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    grepl_worker w(encode_mode, iw_latin1, iw_native, pm, &cr, outptr);
    parallelFor2(0, len, w, 100, nthreads);
#else
    throw std::runtime_error("RcppParallel TBB not supported");
#endif
  } else {
    std::string outstring;
    for(size_t i=0; i<len; i++) {
      auto q = cr.getCharLenCE(i);
      if(q.ptr == nullptr) {
        outptr[i] = NA_LOGICAL;
        continue;
      }
      // outptr[i] = p.match(q.ptr);
      if(encode_mode == "UTF-8") {
        outptr[i] = pm.match(q.ptr, q.len);
      } else if(encode_mode == "byte") {
        outptr[i] = pm.match(q.ptr, q.len);
      } else { // auto - upscale everything to UTF-8
        if(q.enc == CE_NATIVE) {
          if(!is_utf8_locale && !cr.is_ASCII(i)) {
            bool iw = iw_native.convert(q.ptr, q.len, outstring);
            if(!iw) {
              outptr[i] = NA_LOGICAL;
            } else {
              outptr[i] = pm.match(outstring.c_str(), outstring.size());
            }
          } else {
            outptr[i] = pm.match(q.ptr, q.len);
          }
        } else if(q.enc == CE_LATIN1) {
          bool iw = iw_latin1.convert(q.ptr, q.len, outstring);
          if(!iw) {
            outptr[i] = NA_LOGICAL;
          } else {
            outptr[i] = pm.match(outstring.c_str(), outstring.size());
          }
        } else {
          outptr[i] = pm.match(q.ptr, q.len);
        }
      }
    }
  }
  return ret;
}

////////////////////////////////////////////////////////////////////////////////

void sf_split_internal(sf_vec_data & ref, pcre2_match_wrapper & p, const char * sptr, int len, cetype_t enc) {
  int rc = 0;
  bool is_zero_match = false;
  while((rc = p.match_for_split(sptr, len)) && (*sptr != 0)) {
    PCRE2_SIZE * ovec = p.match_ovector();
    ref.emplace_back(sptr, ovec[0], enc);
    sptr += ovec[1];
    len -= ovec[1];
    if(ovec[0] == ovec[1]) is_zero_match = true;
  }
  if(!is_zero_match) { // prevents empty match at the end of string
    ref.emplace_back(sptr, enc);
  }
}

#if RCPP_PARALLEL_USE_TBB
struct split_worker : public Worker {
  const std::string encode_mode;
  cetype_t pattern_enc;
  tbb::enumerable_thread_specific<iconv_wrapper> iw_latin1;
  tbb::enumerable_thread_specific<iconv_wrapper> iw_native;
  tbb::enumerable_thread_specific<pcre2_match_wrapper> pm;
  std::vector<sf_vec_data*> refs;
  RStringIndexer * cr;
  
  split_worker(const std::string encode_mode, cetype_t pattern_enc, iconv_wrapper iw_latin1, iconv_wrapper iw_native, pcre2_match_wrapper pm,
               std::vector<sf_vec_data*> refs, RStringIndexer * cr) : 
    encode_mode(encode_mode), pattern_enc(pattern_enc), iw_latin1(iw_latin1), iw_native(iw_native), 
    pm(pm), refs(refs), cr(cr) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    tbb::enumerable_thread_specific<iconv_wrapper>::reference iw_latin1_local = iw_latin1.local();
    tbb::enumerable_thread_specific<iconv_wrapper>::reference iw_native_local = iw_native.local();
    tbb::enumerable_thread_specific<pcre2_match_wrapper>::reference pm_local = pm.local();
    std::string outstring;
    for(size_t i=begin; i<end; i++) {
      auto & ref = *refs[i];
      auto q = cr->getCharLenCE(i);
      if(q.ptr == nullptr) {
        ref.emplace_back(NA_STRING);
        continue;
      }
      if(encode_mode == "UTF-8") {
        sf_split_internal(ref, pm_local, q.ptr, q.len, CE_UTF8);
      } else if(encode_mode == "byte") {
        cetype_t new_enc = choose_enc(q.enc, pattern_enc);
        sf_split_internal(ref, pm_local, q.ptr, q.len, new_enc);
      } else { // auto
        if(q.enc == CE_NATIVE) {
          if(!is_utf8_locale && !cr->is_ASCII(i)) {
            bool iw = iw_native_local.convert(q.ptr, q.len, outstring);
            if(!iw) {
              ref.emplace_back(NA_STRING);
            } else {
              sf_split_internal(ref, pm_local, outstring.c_str(), outstring.size(), CE_UTF8);
            }
          } else {
            sf_split_internal(ref, pm_local, q.ptr, q.len, CE_UTF8);
          }
        } else if(q.enc == CE_LATIN1) {
          bool iw = iw_latin1_local.convert(q.ptr, q.len, outstring);
          if(!iw) {
            ref.emplace_back(NA_STRING);
          } else {
            sf_split_internal(ref, pm_local, outstring.c_str(), outstring.size(), CE_UTF8);
          }
        } else {
          sf_split_internal(ref, pm_local, q.ptr, q.len, CE_UTF8);
        }
      }
    }
  }
};
#endif

// [[Rcpp::export(rng = false)]]
SEXP sf_split(SEXP subject, SEXP split, const std::string encode_mode = "auto", const bool fixed = false, const int nthreads = 1) {
  
  SEXP pattern_element = STRING_ELT(split, 0);
  cetype_t pattern_enc = Rf_getCharCE(pattern_element);
  const char * pattern_ptr = CHAR(pattern_element);
  std::string pattern_str;
  
  iconv_wrapper iw_latin1;
  iconv_wrapper iw_native;
  pcre2_match_wrapper pm;
  
  if((encode_mode == "auto")) {
    iw_latin1 = iconv_wrapper("UTF-8", "latin1");
    if(!is_utf8_locale) {iw_native = iconv_wrapper("UTF-8", "");}
    if( (!is_utf8_locale && (pattern_enc == CE_NATIVE) && !IS_ASCII(pattern_element)) ) {
      pattern_str = iw_native.convertToString(pattern_ptr).second;
      pattern_ptr = pattern_str.c_str();
    } else if(pattern_enc == CE_LATIN1) {
      pattern_str = iw_latin1.convertToString(pattern_ptr).second;
      pattern_ptr = pattern_str.c_str();
    }
    pm = pcre2_match_wrapper(pattern_ptr, true, fixed);
  } else if(encode_mode == "UTF-8") {
    pm = pcre2_match_wrapper(pattern_ptr, true, fixed);
  }else if(encode_mode == "byte") {
    pm = pcre2_match_wrapper(pattern_ptr, false, fixed);
  } else {
    throw std::runtime_error("encode_mode must be auto, byte or UTF-8");
  }
  
  RStringIndexer cr(subject);
  size_t len = cr.size();
  SEXP ret = PROTECT(Rf_allocVector(VECSXP, len));
  
  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    std::vector<sf_vec_data*> refs(len);
    for(size_t i=0; i<len; i++) {
      SEXP svec = PROTECT(sf_vector(0));
      SET_VECTOR_ELT(ret, i, svec);
      UNPROTECT(1);
      refs[i] = &sf_vec_data_ref(svec);
    }
    split_worker w(encode_mode, pattern_enc, iw_latin1, iw_native, pm, std::move(refs), &cr);
    parallelFor2(0, len, w, 100, nthreads);
#else
    throw std::runtime_error("RcppParallel TBB not supported");
#endif
  } else {
    std::string outstring;
    for(size_t i=0; i<len; i++) {
      auto q = cr.getCharLenCE(i);
      SEXP svec = PROTECT(sf_vector(0));
      SET_VECTOR_ELT(ret, i, svec);
      UNPROTECT(1);
      auto & ref = sf_vec_data_ref(svec);
      if(q.ptr == nullptr) {
        ref.emplace_back(NA_STRING);
        continue;
      }
      if(encode_mode == "UTF-8") {
        sf_split_internal(ref, pm, q.ptr, q.len, CE_UTF8);
      } else if(encode_mode == "byte") {
        cetype_t new_enc = choose_enc(q.enc, pattern_enc);
        sf_split_internal(ref, pm, q.ptr,  q.len,new_enc);
      } else { // auto
        if(q.enc == CE_NATIVE) {
          if(!is_utf8_locale && !cr.is_ASCII(i)) {
            bool iw = iw_native.convert(q.ptr, q.len, outstring);
            if(!iw) {
              ref.emplace_back(NA_STRING);
            } else {
              sf_split_internal(ref, pm, outstring.c_str(), outstring.size(), CE_UTF8);
            }
          } else {
            sf_split_internal(ref, pm, q.ptr, q.len, CE_UTF8);
          }
        } else if(q.enc == CE_LATIN1) {
          bool iw = iw_latin1.convert(q.ptr, q.len, outstring);
          if(!iw) {
            ref.emplace_back(NA_STRING);
          } else {
            sf_split_internal(ref, pm, outstring.c_str(), outstring.size(), CE_UTF8);
          }
        } else {
          sf_split_internal(ref, pm, q.ptr, q.len, CE_UTF8);
        }
      }
    }
  }
  UNPROTECT(1);
  return ret;
}

////////////////////////////////////////////////////////////////////////////////


struct gsub_worker : public Worker {
  const std::string encode_mode;
  tbb::enumerable_thread_specific<iconv_wrapper> iw_latin1;
  tbb::enumerable_thread_specific<iconv_wrapper> iw_native;
  tbb::enumerable_thread_specific<pcre2_sub_wrapper> ps;
  cetype_t pattern_enc;
  cetype_t replacement_enc;
  RStringIndexer * cr;
  sf_vec_data & ref;
  
  gsub_worker(const std::string encode_mode, iconv_wrapper iw_latin1, iconv_wrapper iw_native, 
               const pcre2_sub_wrapper & ps, cetype_t pattern_enc, cetype_t replacement_enc, RStringIndexer * cr, sf_vec_data & ref) : 
    encode_mode(encode_mode), iw_latin1(iw_latin1), iw_native(iw_native), ps(ps), pattern_enc(pattern_enc), replacement_enc(replacement_enc), 
    cr(cr), ref(ref) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    tbb::enumerable_thread_specific<pcre2_sub_wrapper>::reference ps_local = ps.local();
    tbb::enumerable_thread_specific<iconv_wrapper>::reference iw_latin1_local = iw_latin1.local();
    tbb::enumerable_thread_specific<iconv_wrapper>::reference iw_native_local = iw_native.local();
    std::string outstring;
    for(size_t i=begin; i<end; i++) {
      auto q = cr->getCharLenCE(i);
      if(q.ptr == nullptr) {
        ref[i] = sfstring(NA_STRING);
        continue;
      }
      if(encode_mode == "UTF-8") {
        ref[i] = sfstring(ps_local.gsub(q.ptr), CE_UTF8);
      } else if(encode_mode == "byte") {
        cetype_t output_enc = choose_enc(q.enc, pattern_enc, replacement_enc);
        ref[i] = sfstring(ps_local.gsub(q.ptr), output_enc);
      } else { //auto
        if(q.enc == CE_NATIVE) {
          if(!is_utf8_locale && !cr->is_ASCII(i)) {
            bool iw = iw_native_local.convert(q.ptr, q.len, outstring);
            if(!iw) {
              ref[i] = sfstring(NA_STRING);
            } else {
              ref[i] = sfstring(ps_local.gsub(outstring.c_str()), CE_UTF8);
            }
          } else {
            ref[i] = sfstring(ps_local.gsub(q.ptr), CE_UTF8);
          }
        } else if(q.enc == CE_LATIN1) {
          bool iw = iw_latin1_local.convert(q.ptr, q.len, outstring);
          if(!iw) {
            ref[i] = sfstring(NA_STRING);
          } else {
            ref[i] = sfstring(ps_local.gsub(outstring.c_str()), CE_UTF8);
          }
        } else {
          ref[i] = sfstring(ps_local.gsub(q.ptr), CE_UTF8);
        }
      }
    }
  }
};


// [[Rcpp::export(rng = false)]]
SEXP sf_gsub(SEXP subject, SEXP pattern, SEXP replacement, const std::string encode_mode = "auto", const bool fixed = false, const int nthreads = 1) {
  SEXP pattern_element = STRING_ELT(pattern, 0);
  cetype_t pattern_enc = Rf_getCharCE(pattern_element);
  const char * pattern_ptr = CHAR(pattern_element);
  std::string pattern_str; // if we need to re-cast as a UTF-8
  
  SEXP replacement_element = STRING_ELT(replacement, 0);
  cetype_t replacement_enc = Rf_getCharCE(replacement_element);
  const char * replacement_ptr = CHAR(replacement_element);
  std::string replacement_str;
  
  iconv_wrapper iw_latin1;
  iconv_wrapper iw_native;
  pcre2_sub_wrapper ps;
  
  if((encode_mode == "auto")) {
    iw_latin1 = iconv_wrapper("UTF-8", "latin1");
    if(!is_utf8_locale) {iw_native = iconv_wrapper("UTF-8", "");}
    
    if( (!is_utf8_locale && (pattern_enc == CE_NATIVE) && !IS_ASCII(pattern_element)) ) {
      pattern_str = iw_native.convertToString(pattern_ptr).second;
      pattern_ptr = pattern_str.c_str();
    } else if(pattern_enc == CE_LATIN1) {
      pattern_str = iw_latin1.convertToString(pattern_ptr).second;
      pattern_ptr = pattern_str.c_str();
    }
    if( (!is_utf8_locale && (replacement_enc == CE_NATIVE) && !IS_ASCII(replacement_element)) ) {
      replacement_str = iw_native.convertToString(replacement_ptr).second;
      replacement_ptr = replacement_str.c_str();
    } else if(replacement_enc == CE_LATIN1) {
      replacement_str = iw_latin1.convertToString(replacement_ptr).second;
      replacement_ptr = replacement_str.c_str();
    }
    ps = pcre2_sub_wrapper(pattern_ptr, replacement_ptr, true, fixed);
  } else if(encode_mode == "UTF-8") {
    ps = pcre2_sub_wrapper(pattern_ptr, replacement_ptr, true, fixed);
  }else if(encode_mode == "byte") {
    ps = pcre2_sub_wrapper(pattern_ptr, replacement_ptr, false, fixed);
  } else {
    throw std::runtime_error("encode_mode must be auto, byte or UTF-8");
  }
  
  RStringIndexer cr(subject);
  size_t len = cr.size();
  SEXP ret = PROTECT(sf_vector(len));
  sf_vec_data & ref = sf_vec_data_ref(ret);
  
  
  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    gsub_worker w(encode_mode, iw_latin1, iw_native, ps, pattern_enc, replacement_enc, &cr, ref);
    parallelFor2(0, len, w, 100, nthreads);
#else
    throw std::runtime_error("RcppParallel TBB not supported");
#endif
  } else {
    std::string outstring;
    for(size_t i=0; i<len; i++) {
      auto q = cr.getCharLenCE(i);
      if(q.ptr == nullptr) {
        ref[i] = sfstring(NA_STRING);
        continue;
      }
      if(encode_mode == "UTF-8") {
        ref[i] = sfstring(ps.gsub(q.ptr), CE_UTF8);
      } else if(encode_mode == "byte") {
        cetype_t output_enc = choose_enc(q.enc, pattern_enc, replacement_enc);
        ref[i] = sfstring(ps.gsub(q.ptr), output_enc);
      } else { //auto
        if(q.enc == CE_NATIVE) {
          if(!is_utf8_locale && !cr.is_ASCII(i)) {
            bool iw = iw_native.convert(q.ptr, q.len, outstring);
            if(!iw) {
              ref[i] = sfstring(NA_STRING);
            } else {
              ref[i] = sfstring(ps.gsub(outstring.c_str()), CE_UTF8);
            }
          } else {
            ref[i] = sfstring(ps.gsub(q.ptr), CE_UTF8);
          }
        } else if(q.enc == CE_LATIN1) {
          bool iw = iw_latin1.convert(q.ptr, q.len, outstring);
          if(!iw) {
            ref[i] = sfstring(NA_STRING);
          } else {
            ref[i] = sfstring(ps.gsub(outstring.c_str()), CE_UTF8);
          }
        } else {
          ref[i] = sfstring(ps.gsub(q.ptr), CE_UTF8);
        }
      }
    }
  }
  UNPROTECT(1);
  return ret;
}

////////////////////////////////////////////////////////////////////////////////
// [[Rcpp::export]] // RNG needed
SEXP random_strings(const int N, const int string_size = 50, 
                    std::string charset = "abcdefghijklmnopqrstuvwxyz", 
                    std::string vector_mode = "stringfish") {
  if(vector_mode == "stringfish") {
    SEXP ret = PROTECT(sf_vector(N));
    sf_vec_data & ref = sf_vec_data_ref(ret);
    std::string str;
    str.resize(string_size);
    for(int i=0; i<N; i++) {
      std::vector<int> r = Rcpp::as< std::vector<int> >(Rcpp::sample(charset.size(), string_size, true, R_NilValue, false));
      for(int j=0; j<string_size; j++) str[j] = charset[r[j]];
      ref[i] = sfstring(str, CE_NATIVE);
    }
    UNPROTECT(1);
    return ret;
  } else if(vector_mode == "normal") {
    CharacterVector ret(N);
    std::string str;
    str.resize(string_size);
    for(int i=0; i<N; i++) {
      std::vector<int> r = Rcpp::as< std::vector<int> >(Rcpp::sample(charset.size(), string_size, true, R_NilValue, false));
      for(int j=0; j<string_size; j++) str[j] = charset[r[j]];
      ret[i] = str;
    }
    return ret;
  } else {
    throw std::runtime_error("String vector_mode must be 'normal' or 'stringfish'");
  }
}

////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export(rng = false)]]
SEXP sf_tolower(SEXP x) {
  RStringIndexer xr(x);
  size_t len = xr.size();
  SEXP ret = PROTECT(sf_vector(len));
  sf_vec_data & ref = sf_vec_data_ref(ret);
  std::string temp;
  for(size_t i=0; i<len; i++) {
    auto q = xr.getCharLenCE(i);
    temp.resize(q.len);
    for(int j=0; j<q.len; j++) {
      if((q.ptr[j] >= 65) & (q.ptr[j] <= 90)) { // char j is upper case
        temp[j] = q.ptr[j] + 32;
      } else {
        temp[j] = q.ptr[j];
      }
    }
    ref[i] = sfstring(temp, q.enc);
  }
  UNPROTECT(1);
  return ret;
}

// [[Rcpp::export(rng = false)]]
SEXP sf_toupper(SEXP x) {
  RStringIndexer xr(x);
  size_t len = xr.size();
  SEXP ret = PROTECT(sf_vector(len));
  sf_vec_data & ref = sf_vec_data_ref(ret);
  std::string temp;
  for(size_t i=0; i<len; i++) {
    auto q = xr.getCharLenCE(i);
    temp.resize(q.len);
    for(int j=0; j<q.len; j++) {
      if((q.ptr[j] >= 97) & (q.ptr[j] <= 122)) { // char j is lower case
        temp[j] = q.ptr[j] - 32;
      } else {
        temp[j] = q.ptr[j];
      }
    }
    ref[i] = sfstring(temp, q.enc);
  }
  UNPROTECT(1);
  return ret;
}

////////////////////////////////////////////////////////////////////////////////

struct rstring_info_hash { 
  size_t operator()(const RStringIndexer::rstring_info & s) const
  { 
    return XXH64(s.ptr, s.len, 0);
  }
};
using tbb_rstring_map = tbb::concurrent_unordered_map<RStringIndexer::rstring_info, tbb::atomic<int>, rstring_info_hash>;

// hash filler
struct hash_fill_worker : public Worker {
  tbb_rstring_map * table_hash;
  RStringIndexer * fillit;
  hash_fill_worker(tbb_rstring_map * t, RStringIndexer * f) : 
    table_hash(t), fillit(f) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      auto q = fillit->getCharLenCE(i);
      auto p = table_hash->insert(tbb_rstring_map::value_type(q,i));
      if(!p.second) {
        int oldval = i;
        do {
          oldval = p.first->second.compare_and_swap(i,oldval);
        } while (oldval > static_cast<int>(i));
      }
    }
  }
};

// hash_searcher
struct hash_search_worker : public Worker {
  tbb_rstring_map * table_hash;
  RStringIndexer * searchit;
  int * output;
  hash_search_worker(tbb_rstring_map * t, RStringIndexer * s, int * o) : 
    table_hash(t), searchit(s), output(o) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      auto q = searchit->getCharLenCE(i);
      auto it = table_hash->find(q);
      if(it != table_hash->end()) {
        output[i] = it->second + 1;
      } else {
        output[i] = NA_INTEGER;
      }
    }
  }
};


// [[Rcpp::export(rng = false)]]
IntegerVector sf_match(SEXP x, SEXP table, const int nthreads = 1) {
  RStringIndexer cr(table);
  size_t len = cr.size();
  if(len > R_MAX_INT) throw std::runtime_error("long vectors not supported");
  
  RStringIndexer xr(x);
  size_t xlen = xr.size();
  IntegerVector ret(xlen);
  int * retp = INTEGER(ret);
  
  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    tbb_rstring_map table_hash;
    hash_fill_worker w1(&table_hash, &cr);
    parallelFor2(0, len, w1, 100, nthreads);
    hash_search_worker w2(&table_hash, &xr, retp);
    parallelFor2(0, xlen, w2, 100, nthreads);
#else
    throw std::runtime_error("RcppParallel TBB not supported");
#endif
  } else {
    std::unordered_map<RStringIndexer::rstring_info, int, rstring_info_hash> table_hash;
    for(size_t i=0; i<len; i++) {
      auto q = cr.getCharLenCE(i);
      table_hash.insert(std::make_pair(q, static_cast<int>(i)));
    }
    for(size_t i=0; i<xlen; i++) {
      auto q = xr.getCharLenCE(i);
      auto it = table_hash.find(q);
      if(it != table_hash.end()) {
        retp[i] = it->second + 1;
      } else {
        retp[i] = NA_INTEGER;
      }
    }
  }
  return ret;
}

////////////////////////////////////////////////////////////////////////////////

struct compare_worker : public Worker {
  RStringIndexer & xr;
  RStringIndexer & yr;
  const size_t xlen;
  const size_t ylen;
  int * out;
  compare_worker(RStringIndexer & xr, RStringIndexer & yr, const size_t xlen, const size_t ylen, int * out) : 
    xr(xr), yr(yr), xlen(xlen), ylen(ylen), out(out) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      auto qx = xr.getCharLenCE(xlen == 1 ? 0 : i);
      if(qx.ptr == nullptr) {
        out[i] = NA_LOGICAL;
        continue;
      }
      auto qy = yr.getCharLenCE(ylen == 1 ? 0 : i);
      if(qx.ptr == nullptr) {
        out[i] = NA_LOGICAL;
        continue;
      }
      if(qx == qy) out[i] = 1;
    }
  }
};


// [[Rcpp::export(rng = false)]]
LogicalVector sf_compare(SEXP x, SEXP y, const int nthreads = 1) {
  RStringIndexer xr(x);
  RStringIndexer yr(y);
  size_t xlen = xr.size();
  size_t ylen = yr.size();
  if((xlen == 0) || (ylen == 0) || ((xlen != 1) && (ylen != 1) && (xlen != ylen))) {
    throw std::runtime_error("x and y must be length 1 or the same length > 0");
  }
  size_t outlen = std::max(xlen, ylen);
  LogicalVector ret(outlen);
  int * out = INTEGER(ret);

  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    compare_worker w(xr, yr, xlen, ylen, out);
    parallelFor2(0, xlen, w, 100, nthreads);
#else
    throw std::runtime_error("RcppParallel TBB not supported");
#endif
  } else {
    for(size_t i=0; i<outlen; i++) {
      auto qx = xr.getCharLenCE(xlen == 1 ? 0 : i);
      if(qx.ptr == nullptr) {
        out[i] = NA_LOGICAL;
        continue;
      }
      auto qy = yr.getCharLenCE(ylen == 1 ? 0 : i);
      if(qx.ptr == nullptr) {
        out[i] = NA_LOGICAL;
        continue;
      }
      if(qx == qy) out[i] = 1;
    }
  }
  return ret;
}

////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export(rng = false)]]
SEXP c_sf_concat(SEXP x) {
  size_t xlen = Rf_xlength(x);
  std::vector<RStringIndexer> indexers(xlen);
  std::vector<size_t> sizes(xlen);
  size_t total_size = 0;
  for(size_t i=0; i<xlen; i++) {
    SEXP xi = VECTOR_ELT(x, i);
    indexers[i] = RStringIndexer(xi);
    sizes[i] = indexers[i].size();
    total_size += sizes[i];
  }
  SEXP ret = PROTECT(sf_vector(total_size));
  auto & ref = sf_vec_data_ref(ret);
  size_t count = 0;
  for(size_t i=0; i<xlen; i++) {
    switch(indexers[i].getType()) {
    case rstring_type::SF_VEC:
    {
      auto & rp = *reinterpret_cast<sf_vec_data*>(indexers[i].getData());
      for(auto && e : rp) {
        ref[count++] = e;
      }
    }
      break;
    default:
    {
      SEXP rp = reinterpret_cast<SEXP>(indexers[i].getData());
      for(size_t j = 0; j < sizes[i]; j++) {
        ref[count++] = sfstring(STRING_ELT(rp, j));
      }
    }
      break;
    }
  }
  UNPROTECT(1);
  return ret;
}

////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::init]]
void sf_export_functions(DllInfo* dll) {
  R_RegisterCCallable("stringfish", "get_string_type", (DL_FUNC) &get_string_type);
  R_RegisterCCallable("stringfish", "materialize", (DL_FUNC) &materialize);
  R_RegisterCCallable("stringfish", "sf_vector", (DL_FUNC) &sf_vector);
  R_RegisterCCallable("stringfish", "sf_vec_data_ref", (DL_FUNC) &sf_vec_data_ref);
  R_RegisterCCallable("stringfish", "sf_assign", (DL_FUNC) &sf_assign);
  R_RegisterCCallable("stringfish", "sf_iconv", (DL_FUNC) &sf_iconv);
  R_RegisterCCallable("stringfish", "convert_to_sf", (DL_FUNC) &convert_to_sf);
  R_RegisterCCallable("stringfish", "sf_nchar", (DL_FUNC) &sf_nchar);
  // R_RegisterCCallable("stringfish", "sfstring sf_substr_internal", (DL_FUNC) &sfstring sf_substr_internal);
  R_RegisterCCallable("stringfish", "sf_substr", (DL_FUNC) &sf_substr);
  R_RegisterCCallable("stringfish", "c_sf_paste", (DL_FUNC) &c_sf_paste);
  R_RegisterCCallable("stringfish", "sf_collapse", (DL_FUNC) &sf_collapse);
  R_RegisterCCallable("stringfish", "sf_readLines", (DL_FUNC) &sf_readLines);
  R_RegisterCCallable("stringfish", "sf_writeLines", (DL_FUNC) &sf_writeLines);
  R_RegisterCCallable("stringfish", "sf_grepl", (DL_FUNC) &sf_grepl);
  R_RegisterCCallable("stringfish", "sf_split", (DL_FUNC) &sf_split);
  R_RegisterCCallable("stringfish", "sf_gsub", (DL_FUNC) &sf_gsub);
  R_RegisterCCallable("stringfish", "random_strings", (DL_FUNC) &random_strings);
  R_RegisterCCallable("stringfish", "sf_toupper", (DL_FUNC) &sf_toupper);
  R_RegisterCCallable("stringfish", "sf_tolower", (DL_FUNC) &sf_tolower);
  R_RegisterCCallable("stringfish", "sf_match", (DL_FUNC) &sf_match);
}

// END OF SF_FUNCTIONS.CPP


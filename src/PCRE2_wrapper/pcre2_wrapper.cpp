#include <utility>
#include <cstdint>
#include <vector>
#include <string>
#include <stdexcept>

#include "pcre2_wrapper.h"

namespace sf {

pcre2_match_wrapper::pcre2_match_wrapper(const char * pattern_ptr, bool utf8, bool literal) {
  int errorcode;
  PCRE2_SIZE erroroffset;
  uint32_t flags = (utf8 ? PCRE2_UTF : 0) | (literal ? PCRE2_LITERAL : 0);
  // PCRE2_NO_AUTO_POSSESS // auto_possesify ERR80 on solaris
#if defined(sun) || defined(__sun)
  if(!literal) flags = flags | PCRE2_NO_AUTO_POSSESS;
#endif
  re = CALL_pcre2_compile((PCRE2_SPTR)pattern_ptr, // pattern
                     PCRE2_ZERO_TERMINATED, // length
                     flags,
                     &errorcode, // error reporting
                     &erroroffset, // error reporting
                     NULL // compile context
  );
  if(re == NULL) {
    PCRE2_UCHAR buffer[256];
    CALL_pcre2_get_error_message(errorcode, buffer, sizeof(buffer));
    throw std::runtime_error("PCRE2 pattern error " + std::to_string((int)errorcode) + ": " + std::string((char*)buffer));
  }
#ifdef SUPPORT_JIT
  CALL_pcre2_jit_compile(re, PCRE2_JIT_COMPLETE);
  // errorcode = CALL_pcre2_jit_compile(re, PCRE2_JIT_COMPLETE);
  // if(errorcode < 0) {
  //  if(errorcode == PCRE2_ERROR_JIT_BADOPTION) throw std::runtime_error("PCRE2_ERROR_JIT_BADOPTION");
  //  if(errorcode == PCRE2_ERROR_NOMEMORY) throw std::runtime_error("PCRE2_ERROR_NOMEMORY");
  // }
#endif
  match_data = CALL_pcre2_match_data_create_from_pattern(re, NULL);
}
pcre2_match_wrapper::pcre2_match_wrapper() : re(nullptr), match_data(nullptr) {}
  
pcre2_match_wrapper::pcre2_match_wrapper(const pcre2_match_wrapper& other) : 
  re(CALL_pcre2_code_copy_with_tables(other.re)), match_data(CALL_pcre2_match_data_create_from_pattern(other.re, NULL)) { // copy constructor
#ifdef SUPPORT_JIT
  CALL_pcre2_jit_compile(re, PCRE2_JIT_COMPLETE);
#endif
}
pcre2_match_wrapper::pcre2_match_wrapper(pcre2_match_wrapper && other) { // move constructor
  re = other.re;
  match_data = other.match_data;
  other.re = nullptr;
  other.match_data = nullptr;
}
pcre2_match_wrapper & pcre2_match_wrapper::operator=(const pcre2_match_wrapper & other) { // copy assignment
  if(&other == this) return *this;
  if(re != nullptr) CALL_pcre2_code_free(re);
  re = CALL_pcre2_code_copy_with_tables(other.re);
#ifdef SUPPORT_JIT
  CALL_pcre2_jit_compile(re, PCRE2_JIT_COMPLETE);
#endif
  match_data = CALL_pcre2_match_data_create_from_pattern(re, NULL);
  return *this;
}
pcre2_match_wrapper & pcre2_match_wrapper::operator=(pcre2_match_wrapper && other) { // move assignment
  if(&other == this) return *this;
  if(re != nullptr) CALL_pcre2_code_free(re);
  if(match_data != nullptr) CALL_pcre2_match_data_free(match_data);
  re = other.re;
  match_data = other.match_data;
  other.re = nullptr;
  other.match_data = nullptr;
  return *this;
}
  
  pcre2_match_wrapper::~pcre2_match_wrapper() {
  if(re != nullptr) CALL_pcre2_code_free(re);
  if(match_data != nullptr) CALL_pcre2_match_data_free(match_data);
}
int pcre2_match_wrapper::match(const char * subject_ptr, const int len) {
#ifdef SUPPORT_JIT
  int rc = CALL_pcre2_jit_match(re, // compiled pattern
                           (PCRE2_SPTR)subject_ptr, // subject
                           PCRE2_SIZE(len), // PCRE2_ZERO_TERMINATED, // length
                           0, // start offset
                           0, // options
                           match_data,  // match data block
                           NULL // match context
  );
#else
  int rc = CALL_pcre2_match(re, // compiled pattern
                       (PCRE2_SPTR)subject_ptr, // subject
                       PCRE2_SIZE(len), // PCRE2_ZERO_TERMINATED, // length
                       0, // start offset
                       0, // options
                       match_data,  // match data block
                       NULL // match context
  );
#endif
  if(rc == PCRE2_ERROR_NOMATCH) {
    return 0;
  } else if(rc < 0) {
    throw std::runtime_error("error matching string");
  } else {
    return 1;
  }
}
int pcre2_match_wrapper::match_get_interval(const char * subject_ptr, const int len, size_t & begin, size_t & end) {
#ifdef SUPPORT_JIT
  int rc = CALL_pcre2_jit_match(re, // compiled pattern
                           (PCRE2_SPTR)subject_ptr, // subject
                           PCRE2_SIZE(len), // PCRE2_ZERO_TERMINATED, // length
                           0, // start offset
                           PCRE2_NOTEMPTY_ATSTART, // disallows empty match at the start, so while loop doesn't go infinitely
                           match_data,  // match data block
                           NULL // match context
  );
#else
  int rc = CALL_pcre2_match(re, // compiled pattern
                       (PCRE2_SPTR)subject_ptr, // subject
                       PCRE2_SIZE(len), // PCRE2_ZERO_TERMINATED, // length
                       0, // start offset
                       PCRE2_NOTEMPTY_ATSTART, // disallows empty match at the start, so while loop doesn't go infinitely
                       match_data,  // match data block
                       NULL // match context
  );
#endif
  if(rc == PCRE2_ERROR_NOMATCH) {
    return 0;
  } else if(rc < 0) {
    throw std::runtime_error("error matching string");
  } else {
    PCRE2_SIZE * ovec = CALL_pcre2_get_ovector_pointer(match_data);
    begin = ovec[0];
    end = ovec[1];
    return 1;
  }
}


////////////////////////////////////////////////////////////////////////////////

pcre2_sub_wrapper::pcre2_sub_wrapper(const char * pattern_ptr, const char * replacement_ptr, bool utf8, bool literal) : 
  output(std::vector<char>(20)) {
  int errorcode;
  PCRE2_SIZE erroroffset;
  uint32_t flags = (utf8 ? PCRE2_UTF : 0) | (literal ? PCRE2_LITERAL : 0);
  // PCRE2_NO_AUTO_POSSESS // auto_possesify ERR80 on solaris
#if defined(sun) || defined(__sun)
  flags = flags | PCRE2_NO_AUTO_POSSESS;
#endif
  re = CALL_pcre2_compile((PCRE2_SPTR)pattern_ptr, // pattern
                     PCRE2_ZERO_TERMINATED, // length
                     flags,
                     &errorcode, // error reporting
                     &erroroffset, // error reporting
                     NULL // compile context
  );
  if(re == NULL) {
    PCRE2_UCHAR buffer[256];
    CALL_pcre2_get_error_message(errorcode, buffer, sizeof(buffer));
    throw std::runtime_error("PCRE2 pattern error " + std::to_string((int)erroroffset) + ": " + std::string((char*)buffer));
  }
#ifdef SUPPORT_JIT
  CALL_pcre2_jit_compile(re, PCRE2_JIT_COMPLETE);
#endif
  match_data = CALL_pcre2_match_data_create_from_pattern(re, NULL);
  replacement = (PCRE2_SPTR)replacement_ptr;
}
pcre2_sub_wrapper::pcre2_sub_wrapper() : re(nullptr), match_data(nullptr), replacement(nullptr) {}
pcre2_sub_wrapper & pcre2_sub_wrapper::operator=(const pcre2_sub_wrapper & other) { // copy assignment
  if(&other == this) return *this;
  re = CALL_pcre2_code_copy_with_tables(other.re);
#ifdef SUPPORT_JIT
  CALL_pcre2_jit_compile(re, PCRE2_JIT_COMPLETE);
#endif
  output = other.output;
  match_data = CALL_pcre2_match_data_create_from_pattern(re, NULL);
  replacement = other.replacement;
  return *this;
}
pcre2_sub_wrapper & pcre2_sub_wrapper::operator=(pcre2_sub_wrapper && other) { // move assignment
  if(&other == this) return *this;
  re = other.re;
  replacement = other.replacement;
  output = std::move(other.output);
  match_data = other.match_data;
  other.re = nullptr;
  other.match_data = nullptr;
  return *this;
}
pcre2_sub_wrapper::pcre2_sub_wrapper(const pcre2_sub_wrapper& other) { // copy constructor
  re = CALL_pcre2_code_copy_with_tables(other.re);
#ifdef SUPPORT_JIT
  CALL_pcre2_jit_compile(re, PCRE2_JIT_COMPLETE);
#endif
  output = other.output;
  match_data = CALL_pcre2_match_data_create_from_pattern(re, NULL);
  replacement = other.replacement;
}
pcre2_sub_wrapper::pcre2_sub_wrapper(pcre2_sub_wrapper && other) { // move constructor
  re = other.re;
  replacement = other.replacement;
  output = std::move(other.output);
  match_data = other.match_data;
  other.re = nullptr;
  other.match_data = nullptr;
}
pcre2_sub_wrapper::~pcre2_sub_wrapper() {
  if(re != nullptr) CALL_pcre2_code_free(re);
  if(match_data != nullptr) CALL_pcre2_match_data_free(match_data);
}
const char * pcre2_sub_wrapper::gsub(const char * subject_ptr) {
  PCRE2_SIZE output_len = (PCRE2_SIZE)(output.size() - 1);
  int rc = CALL_pcre2_substitute(
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
  // CALL_pcre2_get_error_message(rc, (PCRE2_UCHAR *)&errmsg[0], (PCRE2_SIZE)300);
  // std::cout << errmsg << std::endl;
  if(rc == PCRE2_ERROR_NOMEMORY) {
    output.resize(output_len + 1);
    rc = CALL_pcre2_substitute(
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

std::pair<int, bool> pcre2_info() {
  int ver = PCRE2_MAJOR * 100 + PCRE2_MINOR;
  #ifdef PCRE2_BUNDLED
  return std::make_pair(ver, pcre2_is_bundled() == 1 ? true : false);
  #else
  return std::make_pair(ver, false);
  #endif
}

} // namespace


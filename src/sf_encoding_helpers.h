#ifndef SF_ENCODING_HELPERS_H
#define SF_ENCODING_HELPERS_H

#include <Rcpp.h>
#include <R_ext/Riconv.h>
#include <algorithm>
#include <cerrno>
#include <cstring>
#include <string>

#include "../inst/include/sf_internal.h"
#include "../inst/include/rstring_indexer.h"

bool sf_internal_is_utf8_locale() noexcept;

enum class iconv_encoding_name : uint8_t {
  utf8 = 0,
  latin1 = 1,
  native = 2
};

inline constexpr const char * iconv_utf8_string = "UTF-8";
inline constexpr const char * iconv_latin1_string = "latin1";
inline constexpr const char * iconv_native_string = "";

inline constexpr const char * iconv_encoding_string(iconv_encoding_name x) {
  switch(x) {
  case iconv_encoding_name::utf8:
    return iconv_utf8_string;
  case iconv_encoding_name::latin1:
    return iconv_latin1_string;
  case iconv_encoding_name::native:
  default:
    return iconv_native_string;
  }
}

struct iconv_wrapper {
  const char * to;
  const char * from;
  void * cd;
  iconv_wrapper() : to(nullptr), from(nullptr), cd(nullptr) {}
  iconv_wrapper(iconv_encoding_name to, iconv_encoding_name from) :
    iconv_wrapper(iconv_encoding_string(to), iconv_encoding_string(from)) {}
  iconv_wrapper(const char * to, const char * from) : to(to), from(from), cd(nullptr) {
    if(this->to != nullptr || this->from != nullptr) {
      cd = Riconv_open(this->to, this->from);
    }
  }
  bool is_initialized() const noexcept {
    return cd != nullptr;
  }
  void reset_state() {
    if(cd != nullptr) {
      Riconv(cd, nullptr, nullptr, nullptr, nullptr);
    }
  }
  std::pair<bool, std::string> convertToString(const char * ptr, size_t len) {
    std::string outstring;
    if(!convert(ptr, len, outstring)) {
      return std::make_pair(false, std::string());
    }
    return std::make_pair(true, std::move(outstring));
  }
  bool convert(const char * ptr, size_t len, std::string& outstring) {
    if(cd == nullptr) {
      return false;
    }
    reset_state();
    outstring.assign(std::max<size_t>(len * 2 + 16, 16), '\0');
    const char * inptr = ptr;
    size_t inlen = len;
    size_t written = 0;
    while(true) {
      char * outptr = outstring.data() + written;
      size_t outlen = outstring.size() - written;
      size_t res = Riconv(cd, &inptr, &inlen, &outptr, &outlen);
      written = outstring.size() - outlen;
      if(res != static_cast<size_t>(-1)) {
        outstring.resize(written);
        reset_state();
        return true;
      }
      if(errno == E2BIG) {
        const size_t grow_by = std::max<size_t>(len + 16, outstring.size());
        outstring.resize(outstring.size() + grow_by);
        continue;
      }
      reset_state();
      outstring.clear();
      return false;
    }
  }
  iconv_wrapper(const iconv_wrapper& other) : to(other.to), from(other.from), cd(nullptr) {
    if(other.cd != nullptr) {
      cd = Riconv_open(to, from);
    }
  }
  iconv_wrapper(iconv_wrapper && other) noexcept : to(other.to), from(other.from), cd(other.cd) {
    other.to = nullptr;
    other.from = nullptr;
    other.cd = nullptr;
  }
  iconv_wrapper & operator=(const iconv_wrapper & other) {
    if(&other == this) return *this;
    if(cd != nullptr) Riconv_close(cd);
    to = other.to;
    from = other.from;
    if(other.cd != nullptr) {
      cd = Riconv_open(to, from);
    } else {
      cd = nullptr;
    }
    return *this;
  }
  iconv_wrapper & operator=(iconv_wrapper && other) noexcept {
    if(&other == this) return *this;
    if(cd != nullptr) Riconv_close(cd);
    to = other.to;
    from = other.from;
    cd = other.cd;
    other.to = nullptr;
    other.from = nullptr;
    other.cd = nullptr;
    return *this;
  }
  ~iconv_wrapper() {
    if(cd != nullptr) Riconv_close(cd);
  }
};

namespace sfenc {
using rstring_info = RStringIndexer::rstring_info;

enum class convert_state : uint8_t {
  unfilled = 0,
  utf8_cached = 1,
  failed = 2
};

inline cetype_t_ext resolve_ascii_or_utf8(const char * ptr, int len) {
  return checkAscii(ptr, static_cast<size_t>(len)) ? cetype_t_ext::CE_ASCII : cetype_t_ext::CE_UTF8;
}

struct iconv_text_normalizer {
  size_t failures = 0;
  iconv_wrapper latin1_to_utf8;
  iconv_wrapper native_to_utf8;

  iconv_wrapper & latin1() {
    if(!latin1_to_utf8.is_initialized()) {
      latin1_to_utf8 = iconv_wrapper(iconv_encoding_name::utf8, iconv_encoding_name::latin1);
    }
    return latin1_to_utf8;
  }

  iconv_wrapper & native() {
    if(!native_to_utf8.is_initialized()) {
      native_to_utf8 = iconv_wrapper(iconv_encoding_name::utf8, iconv_encoding_name::native);
    }
    return native_to_utf8;
  }

  bool normalize(rstring_info & q, std::string & utf8_owned) {
    if(q.ptr == nullptr || q.enc == cetype_t_ext::CE_NA) {
      q = rstring_info{};
      return true;
    }
    switch(q.enc) {
    case cetype_t_ext::CE_ASCII:
    case cetype_t_ext::CE_UTF8:
    case cetype_t_ext::CE_BYTES:
      return true;
    case cetype_t_ext::CE_ASCII_OR_UTF8:
      q.enc = resolve_ascii_or_utf8(q.ptr, q.len);
      return true;
    case cetype_t_ext::CE_LATIN1:
      if(latin1().convert(q.ptr, static_cast<size_t>(q.len), utf8_owned)) {
        if(!check_r_string_len(utf8_owned.size())) {
          throw std::runtime_error("normalized string size exceeds R string size");
        }
        q = rstring_info{utf8_owned.c_str(), static_cast<int>(utf8_owned.size()), cetype_t_ext::CE_UTF8};
        return true;
      }
      ++failures;
      q = rstring_info{};
      return false;
    case cetype_t_ext::CE_NATIVE:
      if(checkAscii(q.ptr, static_cast<size_t>(q.len))) {
        q.enc = cetype_t_ext::CE_ASCII;
        return true;
      }
      if(sf_internal_is_utf8_locale()) {
        q.enc = cetype_t_ext::CE_UTF8;
        return true;
      }
      if(native().convert(q.ptr, static_cast<size_t>(q.len), utf8_owned)) {
        if(!check_r_string_len(utf8_owned.size())) {
          throw std::runtime_error("normalized string size exceeds R string size");
        }
        q = rstring_info{utf8_owned.c_str(), static_cast<int>(utf8_owned.size()), cetype_t_ext::CE_UTF8};
        return true;
      }
      ++failures;
      q = rstring_info{};
      return false;
    default:
      q = rstring_info{};
      return false;
    }
  }
};

struct string_ref {
  const char * raw_bytes = nullptr;
  int raw_len = 0;
  cetype_t_ext enc = cetype_t_ext::CE_NA;
  std::string utf8_cache;
  convert_state utf8_state = convert_state::unfilled;

  string_ref() = default;
  explicit string_ref(const rstring_info & q) :
    raw_bytes(q.ptr), raw_len(q.len), enc(q.enc) {}

  rstring_info raw_rstring_info() const {
    return rstring_info{raw_bytes, raw_len, enc};
  }

  bool get_rstring_info(iconv_text_normalizer & norm, rstring_info & out) {
    if(raw_bytes == nullptr || enc == cetype_t_ext::CE_NA) {
      out = rstring_info{};
      return true;
    }
    switch(enc) {
    case cetype_t_ext::CE_ASCII:
    case cetype_t_ext::CE_UTF8:
    case cetype_t_ext::CE_BYTES:
      out = rstring_info{raw_bytes, raw_len, enc};
      return true;
    case cetype_t_ext::CE_ASCII_OR_UTF8:
      out = rstring_info{raw_bytes, raw_len, resolve_ascii_or_utf8(raw_bytes, raw_len)};
      return true;
    case cetype_t_ext::CE_NATIVE:
      if(checkAscii(raw_bytes, static_cast<size_t>(raw_len))) {
        out = rstring_info{raw_bytes, raw_len, cetype_t_ext::CE_ASCII};
        return true;
      }
      if(sf_internal_is_utf8_locale()) {
        out = rstring_info{raw_bytes, raw_len, cetype_t_ext::CE_UTF8};
        return true;
      }
      break;
    case cetype_t_ext::CE_LATIN1:
      break;
    default:
      out = rstring_info{};
      return false;
    }

    if(utf8_state == convert_state::failed) {
      out = rstring_info{};
      return false;
    }
    if(utf8_state == convert_state::utf8_cached) {
      if(!check_r_string_len(utf8_cache.size())) {
        throw std::runtime_error("normalized string size exceeds R string size");
      }
      out = rstring_info{utf8_cache.c_str(), static_cast<int>(utf8_cache.size()), cetype_t_ext::CE_UTF8};
      return true;
    }

    bool ok = false;
    if(enc == cetype_t_ext::CE_LATIN1) {
      ok = norm.latin1().convert(raw_bytes, static_cast<size_t>(raw_len), utf8_cache);
    } else if(enc == cetype_t_ext::CE_NATIVE) {
      ok = norm.native().convert(raw_bytes, static_cast<size_t>(raw_len), utf8_cache);
    }
    if(!ok) {
      ++norm.failures;
      utf8_state = convert_state::failed;
      out = rstring_info{};
      return false;
    }
    utf8_state = convert_state::utf8_cached;
    if(!check_r_string_len(utf8_cache.size())) {
      throw std::runtime_error("normalized string size exceeds R string size");
    }
    out = rstring_info{utf8_cache.c_str(), static_cast<int>(utf8_cache.size()), cetype_t_ext::CE_UTF8};
    return true;
  }
};

inline SEXP make_char(const sfstring & x) {
  if(x.encoding == cetype_t_ext::CE_NA) {
    return NA_STRING;
  }
  if(!check_r_string_len(x.sdata.size())) {
    throw std::runtime_error("string size exceeds R string size");
  }
  return Rf_mkCharLenCE(x.sdata.c_str(), static_cast<int>(x.sdata.size()), to_base_encoding(x.encoding));
}

inline SEXP make_char(const string_record & x) {
  if(x.encoding == cetype_t_ext::CE_NA) {
    return NA_STRING;
  }
  if(!check_r_string_len(x.len)) {
    throw std::runtime_error("string size exceeds R string size");
  }
  return Rf_mkCharLenCE(x.ptr, static_cast<int>(x.len), to_base_encoding(x.encoding));
}

inline void warn_failed_normalization(const std::string & operation, size_t failures) {
  if(failures > 0) {
    Rcpp::warning("%s: %zu strings could not be normalized to UTF-8 and were set to NA", operation.c_str(), failures);
  }
}

inline sfstring make_text_storage(std::string x, bool all_ascii) {
  return sfstring(std::move(x), all_ascii ? cetype_t_ext::CE_ASCII : cetype_t_ext::CE_UTF8);
}
}

#endif

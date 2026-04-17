#ifndef STRINGFISH_SF_INTERNAL_BASE_H
#define STRINGFISH_SF_INTERNAL_BASE_H

#include <Rcpp.h>
#include <R_ext/Rdynload.h>
#include <Rversion.h>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <iostream>
#include <limits>
#include <memory>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#if defined (__AVX2__)
#include "immintrin.h"
inline bool checkAscii(const void * ptr, size_t len) {
  const uint8_t * p8 = reinterpret_cast<const uint8_t*>(ptr);
  size_t i=0;
  if(len >= 32) {
    __m256i sum = _mm256_setzero_si256();
    for(; i+32<len; i+=32) {
      __m256i load = _mm256_lddqu_si256(reinterpret_cast<const __m256i*>(p8+i));
      sum = _mm256_or_si256(sum, load);
    }
    int msb = _mm256_movemask_epi8(sum);
    if(msb != 0) return false;
  }
  if(len >= i+16) {
    __m128i load = _mm_lddqu_si128(reinterpret_cast<const __m128i*>(p8+i));
    int msb = _mm_movemask_epi8(load);
    if(msb != 0) return false;
    i += 16;
  }
  for(; i<len; ++i) {
    if(p8[i] > 127) {
      return false;
    }
  }
  return true;
}
#else
inline bool checkAscii(const void * ptr, size_t len) {
  const uint8_t * qp = reinterpret_cast<const uint8_t*>(ptr);
  for(size_t j=0; j<len; j++) {
    if(qp[j] > 127) {
      return false;
    }
  }
  return true;
}
#endif

enum class cetype_t_ext : uint8_t {
  CE_NATIVE  = 0,
  CE_UTF8    = 1,
  CE_LATIN1  = 2,
  CE_BYTES   = 3,
  CE_SYMBOL  = 5,
  CE_ANY     = 99,
  CE_ASCII_OR_UTF8 = 100,
  CE_ASCII   = 254,
  CE_NA      = 255
};

inline bool IS_ASCII(SEXP x) {
#if (R_VERSION >= R_Version(4, 5, 0))
  return Rf_charIsASCII(x);
#else
  return checkAscii(CHAR(x), Rf_xlength(x));
#endif
}

inline cetype_t to_base_encoding(cetype_t_ext enc) {
  switch(enc) {
  case cetype_t_ext::CE_ASCII:
    return CE_NATIVE;
  case cetype_t_ext::CE_UTF8:
  case cetype_t_ext::CE_ASCII_OR_UTF8:
    return CE_UTF8;
  case cetype_t_ext::CE_LATIN1:
    return CE_LATIN1;
  case cetype_t_ext::CE_BYTES:
    return CE_BYTES;
  default:
    return CE_NATIVE;
  }
}

inline uint32_t checked_sf_size(size_t size, const char * what = "string size") {
  if(size > static_cast<size_t>(std::numeric_limits<uint32_t>::max())) {
    throw std::runtime_error(std::string(what) + " exceeds uint32_t storage");
  }
  return static_cast<uint32_t>(size);
}

inline constexpr uint32_t R_STRING_SIZE_MAX = static_cast<uint32_t>(std::numeric_limits<int>::max());

template<typename POD>
inline bool check_r_string_len(const POD value) noexcept {
  if constexpr(std::is_signed_v<POD>) {
    return value >= 0 && static_cast<uint64_t>(value) <= static_cast<uint64_t>(R_STRING_SIZE_MAX);
  } else {
    return static_cast<uint64_t>(value) <= static_cast<uint64_t>(R_STRING_SIZE_MAX);
  }
}

inline cetype_t_ext reinterpret_input_encoding(const char * ptr, size_t len, cetype_t enc) {
  if(ptr == nullptr) {
    return cetype_t_ext::CE_NA;
  }
  if(enc == CE_BYTES) {
    return cetype_t_ext::CE_BYTES;
  }
  if(checkAscii(ptr, len)) {
    return cetype_t_ext::CE_ASCII;
  }
  switch(enc) {
  case CE_LATIN1:
    return cetype_t_ext::CE_LATIN1;
  case CE_NATIVE:
    return cetype_t_ext::CE_NATIVE;
  case CE_UTF8:
  default:
    return cetype_t_ext::CE_UTF8;
  }
}

inline cetype_t_ext reinterpret_input_encoding(SEXP x) {
  if(x == NA_STRING) {
    return cetype_t_ext::CE_NA;
  }
  return reinterpret_input_encoding(CHAR(x), static_cast<size_t>(LENGTH(x)), Rf_getCharCE(x));
}

namespace sfcalc {
inline size_t clamp_size(size_t value, size_t min_value, size_t max_value) noexcept {
  return std::min(std::max(value, min_value), max_value);
}

inline size_t round_up(size_t value, size_t multiple) noexcept {
  if(value == 0 || multiple == 0) {
    return value;
  }
  size_t rem = value % multiple;
  return rem == 0 ? value : (value + (multiple - rem));
}

inline size_t next_power_of_two(size_t value) noexcept {
  if(value <= 1) {
    return 1;
  }
  size_t out = 1;
  while(out < value && out <= (std::numeric_limits<size_t>::max() >> 1)) {
    out <<= 1;
  }
  return out < value ? value : out;
}
}

enum class rstring_type : uint8_t {
    NORMAL                    = 0,
    SF_VEC                    = 1,
    SF_VEC_MATERIALIZED       = 2,
    slice_store            = 3,
    slice_store_MATERIALIZED = 4,
    OTHER_ALT_REP             = 5
};

inline int code_points(const char * p, int len) {
  int count = 0;
  for(int i = 0; i < len; ++i) {
    count += ((p[i] & 0xc0) != 0x80);
  }
  return count;
}

#endif

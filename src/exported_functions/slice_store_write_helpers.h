#ifndef STRINGFISH_EXPORTED_FUNCTIONS_SLICE_STORE_WRITE_HELPERS_H
#define STRINGFISH_EXPORTED_FUNCTIONS_SLICE_STORE_WRITE_HELPERS_H

#include "../sf_encoding_helpers.h"

template <typename Sink>
inline void slice_store_write(Sink & ref, size_t idx, const char * ptr, size_t len, cetype_t_ext enc) {
  char * dest = ref.reserve(idx, len, enc);
  if(dest != nullptr && len > 0) {
    std::memcpy(dest, ptr, len);
  }
}

template <typename Sink>
inline void slice_store_write(Sink & ref, size_t idx, const std::string & value, cetype_t_ext enc) {
  slice_store_write(ref, idx, value.data(), value.size(), enc);
}

template <typename Sink>
inline void slice_store_write_na(Sink & ref, size_t idx) {
  ref.reserve(idx, 0, cetype_t_ext::CE_NA);
}

template <typename Sink>
inline void slice_store_write(Sink & ref, size_t idx, const char * ptr, size_t len, cetype_t enc) {
  ref.assign(idx, ptr, len, enc);
}

template <typename Sink>
inline void slice_store_write(Sink & ref, size_t idx, const sfstring & value) {
  slice_store_write(ref, idx, value.data(), value.size(), value.encoding);
}

template <typename Sink>
inline void slice_store_write(Sink & ref, size_t idx, SEXP value) {
  if(value == NA_STRING) {
    slice_store_write_na(ref, idx);
    return;
  }
  ref.assign(idx, CHAR(value), static_cast<size_t>(LENGTH(value)), Rf_getCharCE(value));
}

#endif

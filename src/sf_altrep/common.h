#ifndef STRINGFISH_SF_ALTREP_COMMON_H
#define STRINGFISH_SF_ALTREP_COMMON_H

#include "../../inst/include/sf_internal.h"
#include "../sf_encoding_helpers.h"

#include <R_ext/Altrep.h>

struct sf_serialized_state_layout {
  size_t n = 0;
  const unsigned char * size_offset = nullptr;
  const unsigned char * enc_offset = nullptr;
  const unsigned char * data_offset = nullptr;
  const unsigned char * data_end = nullptr;
};

inline size_t sf_checked_add_size(size_t lhs, size_t rhs, const char * what) {
  if(lhs > std::numeric_limits<size_t>::max() - rhs) {
    throw std::runtime_error(std::string(what) + " overflow");
  }
  return lhs + rhs;
}

inline size_t sf_checked_mul_size(size_t lhs, size_t rhs, const char * what) {
  if(lhs != 0 && rhs > std::numeric_limits<size_t>::max() / lhs) {
    throw std::runtime_error(std::string(what) + " overflow");
  }
  return lhs * rhs;
}

inline size_t sf_checked_serialized_state_length(size_t n, size_t payload_bytes) {
  const size_t sizes_bytes = sf_checked_mul_size(n, sizeof(int32_t), "serialized_state");
  const size_t enc_bytes = sf_checked_mul_size(n, sizeof(uint8_t), "serialized_state");
  const size_t header_bytes = sf_checked_add_size(
    sf_checked_add_size(sizeof(uint64_t), sizes_bytes, "serialized_state"),
    enc_bytes,
    "serialized_state"
  );
  const size_t total_bytes = sf_checked_add_size(header_bytes, payload_bytes, "serialized_state");
  if(total_bytes > static_cast<size_t>(std::numeric_limits<R_xlen_t>::max())) {
    throw std::runtime_error("serialized_state length exceeds R_xlen_t");
  }
  return total_bytes;
}

template <typename Fun>
inline SEXP sf_altrep_sexp_guard(const char * op, Fun fun) {
  try {
    return fun();
  } catch(const std::exception & e) {
    Rf_error("stringfish ALTREP %s: %s", op, e.what());
  } catch(...) {
    Rf_error("stringfish ALTREP %s: unknown C++ exception", op);
  }
}

inline sf_serialized_state_layout sf_parse_serialized_state(SEXP serialized_state) {
  if(TYPEOF(serialized_state) != RAWSXP) {
    throw std::runtime_error("invalid serialized_state type");
  }

  const size_t raw_len = static_cast<size_t>(Rf_xlength(serialized_state));
  if(raw_len < sizeof(uint64_t)) {
    throw std::runtime_error("serialized_state is truncated");
  }

  const unsigned char * serialized_ptr = RAW(serialized_state);
  uint64_t n64 = 0;
  std::memcpy(&n64, serialized_ptr, sizeof(uint64_t));
  if(n64 > static_cast<uint64_t>(std::numeric_limits<size_t>::max())) {
    throw std::runtime_error("serialized_state length exceeds size_t");
  }
  if(n64 > static_cast<uint64_t>(std::numeric_limits<R_xlen_t>::max())) {
    throw std::runtime_error("serialized_state length exceeds R_xlen_t");
  }

  const size_t n = static_cast<size_t>(n64);
  const size_t sizes_bytes = sf_checked_mul_size(n, sizeof(int32_t), "serialized_state header");
  const size_t enc_bytes = sf_checked_mul_size(n, sizeof(uint8_t), "serialized_state header");
  const size_t header_bytes = sf_checked_add_size(
    sf_checked_add_size(sizeof(uint64_t), sizes_bytes, "serialized_state header"),
    enc_bytes,
    "serialized_state header"
  );

  if(raw_len < header_bytes) {
    throw std::runtime_error("serialized_state header is truncated");
  }

  sf_serialized_state_layout layout;
  layout.n = n;
  layout.size_offset = serialized_ptr + sizeof(uint64_t);
  layout.enc_offset = layout.size_offset + sizes_bytes;
  layout.data_offset = layout.enc_offset + enc_bytes;
  layout.data_end = serialized_ptr + raw_len;
  return layout;
}

inline const char * sf_checked_serialized_payload(const unsigned char *& data_offset,
                                                  const unsigned char * data_end,
                                                  size_t len) {
  const size_t remaining = static_cast<size_t>(data_end - data_offset);
  if(len > remaining) {
    throw std::runtime_error("serialized_state payload is truncated");
  }
  const char * ptr = reinterpret_cast<const char *>(data_offset);
  data_offset += len;
  return ptr;
}

#endif

#ifndef STRINGFISH_SF_INTERNAL_STRING_TYPES_H
#define STRINGFISH_SF_INTERNAL_STRING_TYPES_H

#include "base.h"

struct sfstring {
  std::string sdata;
  cetype_t_ext encoding;
  sfstring(const char * ptr, int len, cetype_t_ext enc) :
    sdata(ptr, static_cast<size_t>(len)),
    encoding(enc) {}
  sfstring(std::string x, cetype_t_ext enc) : sdata(std::move(x)), encoding(enc) {
    if(!check_r_string_len(sdata.size())) {
      throw std::runtime_error("string size exceeds R string size");
    }
  }
  sfstring(const char * ptr, cetype_t_ext enc) : sdata(ptr), encoding(enc) {
    if(!check_r_string_len(sdata.size())) {
      throw std::runtime_error("string size exceeds R string size");
    }
  }
  sfstring(std::string x, cetype_t enc) : sdata(std::move(x)), encoding(reinterpret_input_encoding(sdata.data(), sdata.size(), enc)) {
    if(!check_r_string_len(sdata.size())) {
      throw std::runtime_error("string size exceeds R string size");
    }
  }
  sfstring(const char * ptr, cetype_t enc) : sdata(ptr), encoding(reinterpret_input_encoding(sdata.data(), sdata.size(), enc)) {
    if(!check_r_string_len(sdata.size())) {
      throw std::runtime_error("string size exceeds R string size");
    }
  }
  sfstring(const char * ptr, int len, cetype_t enc) :
    sdata(ptr, static_cast<size_t>(len)),
    encoding(reinterpret_input_encoding(sdata.data(), sdata.size(), enc)) {}
  sfstring(size_t size) {
    if(!check_r_string_len(size)) {
      throw std::runtime_error("string size exceeds R string size");
    }
    sdata = std::string();
    sdata.resize(size);
  }
  sfstring(SEXP x) {
    if(x == NA_STRING) {
      encoding = cetype_t_ext::CE_NA;
      return;
    }
    sdata = std::string(CHAR(x), LENGTH(x));
    encoding = reinterpret_input_encoding(x);
  }
  sfstring() : sdata(""), encoding(cetype_t_ext::CE_ASCII) {}
  sfstring(const sfstring & other) : sdata(other.sdata), encoding(other.encoding) {}
  inline const char * data() const noexcept {
    return sdata.data();
  }
  inline size_t size() const noexcept {
    return sdata.size();
  }
};

using sf_vec_data = std::vector<sfstring>; // underlying data type for sf_vec ALTREP class

// internal record for slice_store
struct string_record {
  const char * ptr = nullptr;
  uint32_t len = 0;
  cetype_t_ext encoding = cetype_t_ext::CE_NA;

  inline bool is_NA() const noexcept {
    return encoding == cetype_t_ext::CE_NA;
  }
};

#endif

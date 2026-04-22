#ifndef STRINGFISH_SF_INTERNAL_STRING_TYPES_H
#define STRINGFISH_SF_INTERNAL_STRING_TYPES_H

#include "base.h"
#include "../simple_array/small_array.h"

static_assert(sizeof(std::string) > (sizeof(char *) + sizeof(size_t)),
              "std::string is unexpectedly small for sfstring storage sizing");

inline constexpr size_t sfstring_small_array_stack_size =
  sizeof(std::string) - sizeof(char *) - sizeof(size_t);

using sfstring_storage = trqwe::small_array<
  char,
  std::allocator<char>,
  size_t,
  std::integral_constant<size_t, sfstring_small_array_stack_size>
>;

static_assert(sizeof(sfstring_storage) == sizeof(std::string),
              "sfstring small_array storage should match std::string object size");

struct sfstring {
  sfstring_storage sdata;
  cetype_t_ext encoding;
private:
  static sfstring_storage make_storage(const char * ptr, size_t len) {
    return sfstring_storage(ptr, len);
  }
  static sfstring_storage make_storage(const std::string & x) {
    return make_storage(x.data(), x.size());
  }
public:
  sfstring(const char * ptr, int len, cetype_t_ext enc) :
    sdata(make_storage(ptr, static_cast<size_t>(len))),
    encoding(enc) {}
  sfstring(std::string x, cetype_t_ext enc) : sdata(make_storage(x)), encoding(enc) {
    if(!check_r_string_len(sdata.size())) {
      throw std::runtime_error("string size exceeds R string size");
    }
  }
  sfstring(const char * ptr, cetype_t_ext enc) : sdata(make_storage(ptr, std::strlen(ptr))), encoding(enc) {
    if(!check_r_string_len(sdata.size())) {
      throw std::runtime_error("string size exceeds R string size");
    }
  }
  sfstring(std::string x, cetype_t enc) :
    sdata(make_storage(x)),
    encoding(reinterpret_input_encoding(sdata.data(), sdata.size(), enc)) {
    if(!check_r_string_len(sdata.size())) {
      throw std::runtime_error("string size exceeds R string size");
    }
  }
  sfstring(const char * ptr, cetype_t enc) :
    sdata(make_storage(ptr, std::strlen(ptr))),
    encoding(reinterpret_input_encoding(sdata.data(), sdata.size(), enc)) {
    if(!check_r_string_len(sdata.size())) {
      throw std::runtime_error("string size exceeds R string size");
    }
  }
  sfstring(const char * ptr, int len, cetype_t enc) :
    sdata(make_storage(ptr, static_cast<size_t>(len))),
    encoding(reinterpret_input_encoding(sdata.data(), sdata.size(), enc)) {}
  sfstring(size_t size) : sdata(size), encoding(cetype_t_ext::CE_ASCII) {
    if(!check_r_string_len(size)) {
      throw std::runtime_error("string size exceeds R string size");
    }
  }
  sfstring(size_t size, cetype_t_ext enc) : sdata(size), encoding(enc) {
    if(!check_r_string_len(size)) {
      throw std::runtime_error("string size exceeds R string size");
    }
  }
  sfstring(SEXP x) : sdata(), encoding(cetype_t_ext::CE_NA) {
    if(x == NA_STRING) {
      return;
    }
    sdata = make_storage(CHAR(x), static_cast<size_t>(LENGTH(x)));
    encoding = reinterpret_input_encoding(x);
  }
  sfstring() : sdata(), encoding(cetype_t_ext::CE_ASCII) {}
  sfstring(const sfstring & other) : sdata(other.sdata), encoding(other.encoding) {}
  inline const char * data() const noexcept {
    return sdata.data();
  }
  inline size_t size() const noexcept {
    return sdata.size();
  }
};

using sf_vec_data = std::vector<sfstring>; // underlying data type for sf_vec ALTREP class

// Shared internal string view used by slice_store and RStringIndexer.
struct rstring_info {
  const char * ptr = nullptr;
  int len = 0;
  cetype_t_ext enc = cetype_t_ext::CE_NA;

  rstring_info() = default;
  rstring_info(const char * p, int l, cetype_t_ext e) : ptr(p), len(l), enc(e) {}

  inline bool is_NA() const noexcept {
    return enc == cetype_t_ext::CE_NA;
  }

  inline bool operator==(const rstring_info & other) const {
    if((ptr == nullptr) && (other.ptr == nullptr)) return true;
    if((ptr == nullptr) || (other.ptr == nullptr)) return false;
    if((ptr == other.ptr) && (len == other.len) && (enc == other.enc)) return true;
    if((len != other.len) || (enc != other.enc)) return false;
    return std::memcmp(ptr, other.ptr, static_cast<size_t>(len)) == 0;
  }
};

#endif

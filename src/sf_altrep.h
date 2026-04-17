#ifndef SF_ALTREP_H
#define SF_ALTREP_H

#include "sf_altrep/sf_vec_altrep.h"
#include "sf_altrep/slice_st_altrep.h"

inline size_t checked_character_input(SEXP x, const char * arg_name) {
  if(TYPEOF(x) != STRSXP) {
    throw std::runtime_error(std::string(arg_name) + " must be a character vector");
  }
  return static_cast<size_t>(Rf_xlength(x));
}

inline void check_altrep_len(size_t len) {
  if(len > static_cast<size_t>(std::numeric_limits<R_xlen_t>::max())) {
    throw std::runtime_error("vector length exceeds R_xlen_t");
  }
}

inline sf_vec_data & sf_vec_data_ref(SEXP x) {
  return *reinterpret_cast<sf_vec_data*>(R_ExternalPtrAddr(R_altrep_data1(x)));
}

inline slice_store_data & slice_store_data_ref(SEXP x) {
  return *reinterpret_cast<slice_store_data*>(R_ExternalPtrAddr(R_altrep_data1(x)));
}

inline SEXP sf_vector_create(size_t len) {
  check_altrep_len(len);
  auto * ret = new sf_vec_data(len);
  return sf_vec::Make(ret, true);
}

inline SEXP sf_vector(size_t len) {
  return sf_vector_create(len);
}

inline SEXP slice_store_create(size_t len) {
  check_altrep_len(len);
  auto * ret = new slice_store_data(len);
  return slice_st::Make(ret, true);
}

inline SEXP slice_store_create_with_size(size_t len, size_t initial_slice_size) {
  check_altrep_len(len);
  auto * ret = new slice_store_data(len, initial_slice_size);
  return slice_st::Make(ret, true);
}

inline SEXP convert_to_sf_vector(SEXP x, size_t length_out) {
  const size_t x_len = checked_character_input(x, "x");
  if(length_out > 0 && x_len < 1) {
    throw std::runtime_error("x must have length >= 1 when length_out is > 0");
  }

  SEXP ret = PROTECT(sf_vector_create(length_out));
  sf_vec_data & ref = sf_vec_data_ref(ret);

  RStringIndexer data_indexer(x);
  sfenc::iconv_text_normalizer norm;
  std::string utf8_owned;
  for(size_t i = 0; i < length_out; ++i) {
    sfenc::rstring_info view = data_indexer.getCharLenCE(i % x_len);
    utf8_owned.clear();
    if(!norm.normalize(view, utf8_owned) || view.ptr == nullptr) {
      ref[i] = sfstring(NA_STRING);
    } else {
      ref[i] = sfstring(view.ptr, view.len, view.enc);
    }
  }
  sfenc::warn_failed_normalization("convert_to_sf_vector", norm.failures);
  UNPROTECT(1);
  return ret;
}

inline SEXP convert_to_slice_store(SEXP x, size_t length_out) {
  const size_t x_len = checked_character_input(x, "x");
  if(length_out > 0 && x_len < 1) {
    throw std::runtime_error("x must have length >= 1 when length_out is > 0");
  }

  const size_t max_initial_slice_size =
    static_cast<size_t>(std::numeric_limits<uint32_t>::max()) - (slice_store_data::slice_alignment - 1);
  size_t initial_slice_size = 0;
  if(length_out > 0) {
    RStringIndexer size_indexer(x);
    sfenc::iconv_text_normalizer size_norm;
    std::string size_utf8_owned;
    for(size_t i = 0; i < length_out; ++i) {
      sfenc::rstring_info view = size_indexer.getCharLenCE(i % x_len);
      size_utf8_owned.clear();
      if(!size_norm.normalize(view, size_utf8_owned) || view.ptr == nullptr) {
        continue;
      }
      const size_t view_len = static_cast<size_t>(view.len);
      if(initial_slice_size > max_initial_slice_size - view_len) {
        initial_slice_size = max_initial_slice_size;
        break;
      }
      initial_slice_size += view_len;
    }
  }

  SEXP ret = PROTECT(slice_store_create_with_size(length_out, initial_slice_size));
  slice_store_data & ref = slice_store_data_ref(ret);

  RStringIndexer data_indexer(x);
  sfenc::iconv_text_normalizer norm;
  std::string utf8_owned;
  for(size_t i = 0; i < length_out; ++i) {
    sfenc::rstring_info view = data_indexer.getCharLenCE(i % x_len);
    utf8_owned.clear();
    if(!norm.normalize(view, utf8_owned) || view.ptr == nullptr) {
      ref.reserve(i, 0, cetype_t_ext::CE_NA);
    } else {
      ref.assign(i, view.ptr, static_cast<size_t>(view.len), view.enc);
    }
  }
  sfenc::warn_failed_normalization("convert_to_slice_store", norm.failures);
  UNPROTECT(1);
  return ret;
}

R_altrep_class_t sf_vec::class_t;
R_altrep_class_t slice_st::class_t;

// [[Rcpp::init]]
void init_stringfish(DllInfo* dll){
  sf_vec::Init(dll);
  slice_st::Init(dll);
}

#endif

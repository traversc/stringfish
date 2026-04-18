#ifndef SF_EXTERNAL_H
#define SF_EXTERNAL_H

#include <R_ext/Rdynload.h>
#include <Rcpp.h>
#include "sf_internal.h"
#include "rstring_indexer.h"

namespace sfext_detail {
template <typename Fn>
inline Fn get_callable(const char * name) {
  return reinterpret_cast<Fn>(R_GetCCallable("stringfish", name));
}
}

inline std::string get_string_type(SEXP x) {
  static auto fun = sfext_detail::get_callable<std::string(*)(SEXP)>("get_string_type");
  return fun(x);
}

inline SEXP materialize(SEXP x) {
  static auto fun = sfext_detail::get_callable<SEXP(*)(SEXP)>("materialize");
  return fun(x);
}

inline SEXP sf_vector(size_t len) {
  static auto fun = sfext_detail::get_callable<SEXP(*)(size_t)>("sf_vector");
  return fun(len);
}

inline SEXP sf_vector_create(size_t len) {
  static auto fun = sfext_detail::get_callable<SEXP(*)(size_t)>("sf_vector_create");
  return fun(len);
}

inline sf_vec_data & sf_vec_data_ref(SEXP x) {
  static auto fun = sfext_detail::get_callable<sf_vec_data &(*)(SEXP)>("sf_vec_data_ref");
  return fun(x);
}

inline SEXP slice_store_create(size_t len) {
  static auto fun = sfext_detail::get_callable<SEXP(*)(size_t)>("slice_store_create");
  return fun(len);
}

inline SEXP slice_store_create_with_size(size_t len, size_t initial_slice_size) {
  static auto fun = sfext_detail::get_callable<SEXP(*)(size_t, size_t)>("slice_store_create_with_size");
  return fun(len, initial_slice_size);
}

inline slice_store_data & slice_store_data_ref(SEXP x) {
  static auto fun = sfext_detail::get_callable<slice_store_data &(*)(SEXP)>("slice_store_data_ref");
  return fun(x);
}

inline void sf_assign(SEXP x, size_t i, SEXP e) {
  static auto fun = sfext_detail::get_callable<void(*)(SEXP, size_t, SEXP)>("sf_assign");
  fun(x, i, e);
}

inline SEXP sf_iconv(SEXP x, const std::string from, const std::string to, int nthreads = 1) {
  static auto fun = sfext_detail::get_callable<SEXP(*)(SEXP, const std::string, const std::string, int)>("sf_iconv");
  return fun(x, from, to, nthreads);
}

inline SEXP convert_to_sf_vector(SEXP x, size_t length_out) {
  static auto fun = sfext_detail::get_callable<SEXP(*)(SEXP, size_t)>("convert_to_sf_vector");
  return fun(x, length_out);
}

inline SEXP convert_to_sf(SEXP x) {
  static auto fun = sfext_detail::get_callable<SEXP(*)(SEXP)>("convert_to_sf");
  return fun(x);
}

inline SEXP convert_to_slice_store(SEXP x, size_t length_out) {
  static auto fun = sfext_detail::get_callable<SEXP(*)(SEXP, size_t)>("convert_to_slice_store");
  return fun(x, length_out);
}

inline SEXP sf_readLines(const std::string filename, const std::string encoding = "UTF-8") {
  static auto fun = sfext_detail::get_callable<SEXP(*)(const std::string, const std::string)>("sf_readLines");
  return fun(filename, encoding);
}

inline void sf_writeLines(SEXP text, const std::string file, const std::string sep = "\n",
                          const std::string na_value = "NA", const std::string encode_mode = "UTF-8") {
  static auto fun = sfext_detail::get_callable<
    void(*)(SEXP, const std::string, const std::string, const std::string, const std::string)
  >("sf_writeLines");
  fun(text, file, sep, na_value, encode_mode);
}

inline Rcpp::IntegerVector sf_nchar(SEXP obj, const std::string type = "chars", int nthreads = 1) {
  static auto fun = sfext_detail::get_callable<Rcpp::IntegerVector(*)(SEXP, const std::string, int)>("sf_nchar");
  return fun(obj, type, nthreads);
}

inline sfstring sf_substr_internal(const char * x, const int len, const cetype_t_ext type, int start, int stop) {
  static auto fun = sfext_detail::get_callable<sfstring(*)(const char *, const int, const cetype_t_ext, int, int)>(
    "sf_substr_internal"
  );
  return fun(x, len, type, start, stop);
}

inline SEXP sf_substr(SEXP x, Rcpp::IntegerVector start, Rcpp::IntegerVector stop, int nthreads = 1) {
  static auto fun = sfext_detail::get_callable<SEXP(*)(SEXP, Rcpp::IntegerVector, Rcpp::IntegerVector, int)>("sf_substr");
  return fun(x, start, stop, nthreads);
}

inline SEXP c_sf_paste(Rcpp::List dots, SEXP sep, int nthreads = 1) {
  static auto fun = sfext_detail::get_callable<SEXP(*)(Rcpp::List, SEXP, int)>("c_sf_paste");
  return fun(dots, sep, nthreads);
}

inline SEXP sf_collapse(SEXP x, SEXP collapse) {
  static auto fun = sfext_detail::get_callable<SEXP(*)(SEXP, SEXP)>("sf_collapse");
  return fun(x, collapse);
}

inline Rcpp::LogicalVector sf_grepl(SEXP subject, SEXP pattern, const std::string encode_mode = "auto",
                                    const bool fixed = false, int nthreads = 1) {
  static auto fun = sfext_detail::get_callable<Rcpp::LogicalVector(*)(SEXP, SEXP, const std::string, const bool, int)>(
    "sf_grepl"
  );
  return fun(subject, pattern, encode_mode, fixed, nthreads);
}

inline SEXP sf_split(SEXP subject, SEXP split, const std::string encode_mode = "auto",
                     const bool fixed = false, int nthreads = 1) {
  static auto fun = sfext_detail::get_callable<SEXP(*)(SEXP, SEXP, const std::string, const bool, int)>("sf_split");
  return fun(subject, split, encode_mode, fixed, nthreads);
}

inline SEXP sf_gsub(SEXP subject, SEXP pattern, SEXP replacement, const std::string encode_mode = "auto",
                    const bool fixed = false, int nthreads = 1) {
  static auto fun = sfext_detail::get_callable<SEXP(*)(SEXP, SEXP, SEXP, const std::string, const bool, int)>("sf_gsub");
  return fun(subject, pattern, replacement, encode_mode, fixed, nthreads);
}

inline SEXP random_strings(size_t N, Rcpp::IntegerVector string_size,
                           const std::string charset = "abcdefghijklmnopqrstuvwxyz",
                           const std::string vector_mode = "stringfish") {
  static auto fun = sfext_detail::get_callable<SEXP(*)(size_t, Rcpp::IntegerVector, std::string, std::string)>(
    "random_strings"
  );
  return fun(N, string_size, charset, vector_mode);
}

inline SEXP sf_toupper(SEXP x) {
  static auto fun = sfext_detail::get_callable<SEXP(*)(SEXP)>("sf_toupper");
  return fun(x);
}

inline SEXP sf_tolower(SEXP x) {
  static auto fun = sfext_detail::get_callable<SEXP(*)(SEXP)>("sf_tolower");
  return fun(x);
}

inline Rcpp::IntegerVector sf_match(SEXP x, SEXP table, int nthreads = 1) {
  static auto fun = sfext_detail::get_callable<Rcpp::IntegerVector(*)(SEXP, SEXP, int)>("sf_match");
  return fun(x, table, nthreads);
}

inline Rcpp::LogicalVector sf_compare(SEXP x, SEXP y, int nthreads = 1) {
  static auto fun = sfext_detail::get_callable<Rcpp::LogicalVector(*)(SEXP, SEXP, int)>("sf_compare");
  return fun(x, y, nthreads);
}

#endif // include guard

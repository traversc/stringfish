#ifndef SF_EXTERNAL_H
#define SF_EXTERNAL_H

#include <R_ext/Rdynload.h>
#include <Rcpp.h>
#include "sf_internal.h"
using namespace Rcpp;

std::string get_string_type(SEXP x) {static std::string(*fun)(SEXP) = (std::string(*)(SEXP)) R_GetCCallable("stringfish", "get_string_type");return fun(x);}
SEXP materialize(SEXP x) {static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("stringfish", "materialize");return fun(x);}
SEXP sf_vector(size_t len) {static SEXP(*fun)(size_t) = (SEXP(*)(size_t)) R_GetCCallable("stringfish", "sf_vector");return fun(len);}
sf_vec_data & sf_vec_data_ref(SEXP x) {static sf_vec_data &(*fun)(SEXP) = (sf_vec_data &(*)(SEXP)) R_GetCCallable("stringfish", "sf_vec_data_ref");return fun(x);}
void sf_assign(SEXP x, size_t i, SEXP e) {static void(*fun)(SEXP, size_t, SEXP) = (void(*)(SEXP, size_t, SEXP)) R_GetCCallable("stringfish", "sf_assign");return fun(x, i, e);}
SEXP sf_iconv(SEXP x, std::string from, std::string to) {static SEXP(*fun)(SEXP, std::string, std::string) = (SEXP(*)(SEXP, std::string, std::string)) R_GetCCallable("stringfish", "sf_iconv");return fun(x, from, to);}
SEXP convert_to_sf(SEXP x) {static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("stringfish", "convert_to_sf");return fun(x);}
IntegerVector sf_nchar(SEXP obj, std::string type = "chars") {static IntegerVector(*fun)(SEXP, std::string) = (IntegerVector(*)(SEXP, std::string)) R_GetCCallable("stringfish", "sf_nchar");return fun(obj, type);}
sfstring sf_substr_internal(const char * x, const int len, const cetype_t type, int start, int stop) {static sfstring(*fun)(const char *, const int, const cetype_t, int, int) = (sfstring(*)(const char *, const int, const cetype_t, int, int)) R_GetCCallable("stringfish", "sf_substr_internal");return fun(x, len, type, start, stop);}
SEXP sf_substr(SEXP x, IntegerVector start, IntegerVector stop) {static SEXP(*fun)(SEXP, IntegerVector, IntegerVector) = (SEXP(*)(SEXP, IntegerVector, IntegerVector)) R_GetCCallable("stringfish", "sf_substr");return fun(x, start, stop);}
SEXP c_sf_paste(List dots, SEXP sep) {static SEXP(*fun)(List, SEXP) = (SEXP(*)(List, SEXP)) R_GetCCallable("stringfish", "c_sf_paste");return fun(dots, sep);}
SEXP sf_collapse(SEXP x, SEXP collapse) {static SEXP(*fun)(SEXP, SEXP) = (SEXP(*)(SEXP, SEXP)) R_GetCCallable("stringfish", "sf_collapse");return fun(x, collapse);}
SEXP sf_readLines(std::string filename, std::string encoding = "UTF-8") {static SEXP(*fun)(std::string, std::string) = (SEXP(*)(std::string, std::string)) R_GetCCallable("stringfish", "sf_readLines");return fun(filename, encoding);}
LogicalVector sf_grepl(SEXP subject, SEXP pattern, std::string encode_mode = "auto") {static LogicalVector(*fun)(SEXP, SEXP, std::string) = (LogicalVector(*)(SEXP, SEXP, std::string)) R_GetCCallable("stringfish", "sf_grepl");return fun(subject, pattern, encode_mode);}
SEXP sf_gsub(SEXP subject, SEXP pattern, SEXP replacement, std::string encode_mode = "auto") {static SEXP(*fun)(SEXP, SEXP, SEXP, std::string) = (SEXP(*)(SEXP, SEXP, SEXP, std::string)) R_GetCCallable("stringfish", "sf_gsub");return fun(subject, pattern, replacement, encode_mode);}
SEXP random_strings(const int N, const int string_size = 50, std::string charset = "abcdefghijklmnopqrstuvwxyz", std::string vector_mode = "stringfish") {static SEXP(*fun)(const int, const int, std::string, std::string) = (SEXP(*)(const int, const int, std::string, std::string)) R_GetCCallable("stringfish", "sf_random_strings");return fun(N, string_size, charset, vector_mode);}



#endif // include guard
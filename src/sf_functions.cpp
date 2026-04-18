#define STRINGFISH_INTERNAL_BUILD

#include <Rcpp.h>
#include <RcppParallel.h>
// TRUE and FALSE is defined in a windows header as 1 and 0; conflicts with #define in R headers
#ifdef TRUE
#undef TRUE
#endif
#ifdef FALSE
#undef FALSE
#endif
#if RCPP_PARALLEL_USE_TBB
#include <tbb/concurrent_unordered_map.h>
#include <tbb/task_arena.h>
#include <tbb/enumerable_thread_specific.h>
#endif
#include <R_ext/Rdynload.h>
#include <R_ext/Riconv.h>
#include <atomic>
#include <unordered_map>
#include <fstream>
#include <optional>
#include <algorithm>
#include <cerrno>
#include <climits>
#include <cmath>

#include "../inst/include/sf_internal.h"
#include "PCRE2_wrapper/pcre2_wrapper.h"
#include "xxhash/xxhash.c"
#include "sf_altrep.h"
#include "exported_functions/slice_store_write_helpers.h"

using namespace Rcpp;
using namespace RcppParallel;

#include "sf_utility.h"

// [[Rcpp::export(rng = false)]]
void set_is_utf8_locale();

// [[Rcpp::export(rng = false)]]
void unset_is_utf8_locale();

// [[Rcpp::export(rng = false)]]
bool get_is_utf8_locale();

// [[Rcpp::export(rng = false)]]
bool is_tbb();

// [[Rcpp::export(rng = false)]]
void check_simd();

// [[Rcpp::export(rng = false)]]
List get_pcre2_info();

// [[Rcpp::export(rng = false)]]
std::string get_string_type(SEXP x);

// [[Rcpp::export(rng = false)]]
SEXP materialize(SEXP x);

// [[Rcpp::export(rng = false)]]
SEXP sf_vector(size_t len);

// [[Rcpp::export(rng = false)]]
SEXP sf_vector_create(size_t len);

// [[Rcpp::export(rng = false)]]
SEXP slice_store_create(size_t len);

// [[Rcpp::export(rng = false)]]
SEXP slice_store_create_with_size(size_t len, size_t initial_slice_size);

sf_vec_data & sf_vec_data_ref(SEXP x);
slice_store_data & slice_store_data_ref(SEXP x);

// [[Rcpp::export(rng = false)]]
void sf_assign(SEXP x, size_t i, SEXP e);

// [[Rcpp::export(rng = false, signature = {x, from, to, nthreads = getOption("stringfish.nthreads", 1L)})]]
SEXP sf_iconv(SEXP x, const std::string from, const std::string to, int nthreads = 1);

// [[Rcpp::export(rng = false, signature = {x, length_out = length(x)})]]
SEXP convert_to_sf_vector(SEXP x, size_t length_out);

// [[Rcpp::export(rng = false)]]
SEXP convert_to_sf(SEXP x);

// [[Rcpp::export(rng = false, signature = {x, length_out = length(x)})]]
SEXP convert_to_slice_store(SEXP x, size_t length_out);

// [[Rcpp::export(rng = false, signature = {x, type = "chars", nthreads = getOption("stringfish.nthreads", 1L)})]]
IntegerVector sf_nchar(SEXP x, const std::string type = "chars", const int nthreads = 1);

// [[Rcpp::export(rng = false, signature = {x, start, stop, nthreads = getOption("stringfish.nthreads", 1L)})]]
SEXP sf_substr(SEXP x, IntegerVector start, IntegerVector stop, const int nthreads = 1);

// [[Rcpp::export(rng = false)]]
SEXP c_sf_paste(List dots, SEXP sep, const int nthreads = 1);

// [[Rcpp::export(rng = false)]]
SEXP sf_collapse(SEXP x, SEXP collapse);

// [[Rcpp::export(rng = false)]]
SEXP sf_readLines(const std::string file, const std::string encoding = "UTF-8");

// [[Rcpp::export(rng = false)]]
void sf_writeLines(SEXP text, const std::string file, const std::string sep = "\n", const std::string na_value = "NA", const std::string encode_mode = "UTF-8");

// [[Rcpp::export(rng = false, signature = {subject, pattern, encode_mode = "auto", fixed = FALSE, nthreads = getOption("stringfish.nthreads", 1L)})]]
LogicalVector sf_grepl(SEXP subject, SEXP pattern, const std::string encode_mode = "auto", const bool fixed = false, const int nthreads = 1);

// [[Rcpp::export(rng = false, signature = {subject, split, encode_mode = "auto", fixed = FALSE, nthreads = getOption("stringfish.nthreads", 1L)})]]
SEXP sf_split(SEXP subject, SEXP split, const std::string encode_mode = "auto", const bool fixed = false, const int nthreads = 1);

// [[Rcpp::export(rng = false, signature = {subject, pattern, replacement, encode_mode = "auto", fixed = FALSE, nthreads = getOption("stringfish.nthreads", 1L)})]]
SEXP sf_gsub(SEXP subject, SEXP pattern, SEXP replacement, const std::string encode_mode = "auto", const bool fixed = false, const int nthreads = 1);

// [[Rcpp::export(signature = {N, string_size = 50L, charset = "abcdefghijklmnopqrstuvwxyz", vector_mode = "stringfish"})]]
SEXP random_strings(size_t N, IntegerVector string_size, std::string charset = "abcdefghijklmnopqrstuvwxyz", std::string vector_mode = "stringfish");

// [[Rcpp::export(rng = false)]]
SEXP sf_tolower(SEXP x);

// [[Rcpp::export(rng = false)]]
SEXP sf_toupper(SEXP x);

// [[Rcpp::export(rng = false, signature = {x, table, nthreads = getOption("stringfish.nthreads", 1L)})]]
IntegerVector sf_match(SEXP x, SEXP table, const int nthreads = 1);

// [[Rcpp::export(rng = false, signature = {x, y, nthreads = getOption("stringfish.nthreads", 1L)})]]
LogicalVector sf_compare(SEXP x, SEXP y, const int nthreads = 1);

// [[Rcpp::export(rng = false)]]
SEXP c_sf_concat(SEXP x);

#include "exported_functions/sf_assign.h"
#include "exported_functions/sf_iconv.h"
#include "exported_functions/sf_nchar.h"
#include "exported_functions/sf_substr.h"
#include "exported_functions/c_sf_paste.h"
#include "exported_functions/sf_collapse.h"
#include "exported_functions/sf_readLines.h"
#include "exported_functions/sf_writeLines.h"
#include "exported_functions/sf_grepl.h"
#include "exported_functions/sf_split.h"
#include "exported_functions/sf_gsub.h"
#include "exported_functions/random_strings.h"
#include "exported_functions/sf_tolower.h"
#include "exported_functions/sf_toupper.h"
#include "exported_functions/sf_match.h"
#include "exported_functions/sf_compare.h"
#include "exported_functions/c_sf_concat.h"

// [[Rcpp::init]]
void sf_export_functions(DllInfo* /* dll */) {
  auto register_fn = [](const char * name, DL_FUNC fun) {
    R_RegisterCCallable("stringfish", name, fun);
  };

  // Core helpers
  register_fn("get_string_type", (DL_FUNC) &get_string_type);
  register_fn("get_rstring_type_export", (DL_FUNC) &get_rstring_type_export);
  register_fn("materialize", (DL_FUNC) &materialize);

  // ALTREP constructors and accessors
  register_fn("sf_vector", (DL_FUNC) &sf_vector);
  register_fn("sf_vector_create", (DL_FUNC) &sf_vector_create);
  register_fn("sf_vec_data_ref", (DL_FUNC) &sf_vec_data_ref);
  register_fn("slice_store_create", (DL_FUNC) &slice_store_create);
  register_fn("slice_store_create_with_size", (DL_FUNC) &slice_store_create_with_size);
  register_fn("slice_store_data_ref", (DL_FUNC) &slice_store_data_ref);
  register_fn("sf_assign", (DL_FUNC) &sf_assign);

  // Converters and string operations
  register_fn("sf_iconv", (DL_FUNC) &sf_iconv);
  register_fn("convert_to_sf", (DL_FUNC) &convert_to_sf);
  register_fn("convert_to_sf_vector", (DL_FUNC) &convert_to_sf_vector);
  register_fn("convert_to_slice_store", (DL_FUNC) &convert_to_slice_store);
  register_fn("sf_nchar", (DL_FUNC) &sf_nchar);
  register_fn("sf_substr_internal", (DL_FUNC) &sfexport::sf_substr_internal);
  register_fn("sf_substr", (DL_FUNC) &sf_substr);
  register_fn("c_sf_paste", (DL_FUNC) &c_sf_paste);
  register_fn("sf_collapse", (DL_FUNC) &sf_collapse);
  register_fn("sf_grepl", (DL_FUNC) &sf_grepl);
  register_fn("sf_split", (DL_FUNC) &sf_split);
  register_fn("sf_gsub", (DL_FUNC) &sf_gsub);
  register_fn("sf_toupper", (DL_FUNC) &sf_toupper);
  register_fn("sf_tolower", (DL_FUNC) &sf_tolower);
  register_fn("sf_match", (DL_FUNC) &sf_match);
  register_fn("sf_compare", (DL_FUNC) &sf_compare);
  register_fn("c_sf_concat", (DL_FUNC) &c_sf_concat);

  // I/O and generators
  register_fn("sf_readLines", (DL_FUNC) &sf_readLines);
  register_fn("sf_writeLines", (DL_FUNC) &sf_writeLines);
  register_fn("random_strings", (DL_FUNC) &random_strings);
  register_fn("sf_random_strings", (DL_FUNC) &random_strings);
}

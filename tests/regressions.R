suppressPackageStartupMessages(library(stringfish))

regression_external_source <- '
// [[Rcpp::depends(stringfish)]]
// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include "sf_external.h"
using namespace Rcpp;

// [[Rcpp::export]]
List sf_external_regressions(SEXP x, int nthreads = 1) {
  return List::create(
    _["nchar"] = sf_nchar(x, "chars", nthreads),
    _["compare"] = sf_compare(x, x, nthreads),
    _["substr"] = sf_substr(x, IntegerVector::create(1), IntegerVector::create(1), nthreads),
    _["random"] = random_strings(4, IntegerVector::create(3), "ab", "normal")
  );
}

// [[Rcpp::export]]
IntegerVector sf_external_encoding_regressions(SEXP x) {
  if(TYPEOF(x) != STRSXP || Rf_xlength(x) != 1) {
    stop("x must be a character vector of length 1");
  }
  const char native_utf8[] = "\\xC3\\xA9";
  RStringIndexer direct_indexer(x);

  SEXP vec_bytes = PROTECT(sf_vector(1));
  sf_vec_data_ref(vec_bytes)[0] = sfstring("abc", 3, CE_BYTES);
  RStringIndexer vec_bytes_indexer(vec_bytes);

  SEXP store_bytes = PROTECT(slice_store_create(1));
  slice_store_data_ref(store_bytes).assign(0, "abc", 3, CE_BYTES);
  RStringIndexer store_bytes_indexer(store_bytes);

  SEXP vec_native = PROTECT(sf_vector(1));
  sf_vec_data_ref(vec_native)[0] = sfstring(native_utf8, 2, CE_NATIVE);
  RStringIndexer vec_native_indexer(vec_native);

  SEXP store_native = PROTECT(slice_store_create(1));
  slice_store_data_ref(store_native).assign(0, native_utf8, 2, CE_NATIVE);
  RStringIndexer store_native_indexer(store_native);

  IntegerVector out = IntegerVector::create(
    _["reinterpret_ascii_bytes"] = static_cast<int>(reinterpret_input_encoding("abc", 3, CE_BYTES)),
    _["sfstring_ascii_bytes"] = static_cast<int>(sfstring("abc", 3, CE_BYTES).encoding),
    _["sf_vec_ascii_bytes"] = static_cast<int>(vec_bytes_indexer.getCharLenCE(0).enc),
    _["slice_store_ascii_bytes"] = static_cast<int>(store_bytes_indexer.getCharLenCE(0).enc),
    _["reinterpret_utf8_native"] = static_cast<int>(reinterpret_input_encoding(native_utf8, 2, CE_NATIVE)),
    _["sfstring_utf8_native"] = static_cast<int>(sfstring(native_utf8, 2, CE_NATIVE).encoding),
    _["sf_vec_utf8_native"] = static_cast<int>(vec_native_indexer.getCharLenCE(0).enc),
    _["slice_store_utf8_native"] = static_cast<int>(store_native_indexer.getCharLenCE(0).enc),
    _["direct_utf8_native"] = static_cast<int>(direct_indexer.getCharLenCE(0).enc)
  );
  UNPROTECT(4);
  return out;
}

// [[Rcpp::export]]
IntegerVector sf_external_get_encodings(SEXP x) {
  RStringIndexer indexer(x);
  const size_t len = indexer.size();
  IntegerVector out(len);
  for(size_t i = 0; i < len; ++i) {
    out[static_cast<R_xlen_t>(i)] = static_cast<int>(indexer.getCharLenCE(i).enc);
  }
  return out;
}

// [[Rcpp::export]]
List sf_external_make_storage_vectors() {
  const char utf8_e[] = "\\xC3\\xA9";
  const char latin1_e[] = "\\xE9";

  SEXP vec = PROTECT(sf_vector(6));
  sf_vec_data & vec_data = sf_vec_data_ref(vec);
  vec_data[0] = sfstring("abc", 3, cetype_t_ext::CE_ASCII);
  vec_data[1] = sfstring(utf8_e, 2, cetype_t_ext::CE_ASCII_OR_UTF8);
  vec_data[2] = sfstring(utf8_e, 2, cetype_t_ext::CE_NATIVE);
  vec_data[3] = sfstring(latin1_e, 1, cetype_t_ext::CE_LATIN1);
  vec_data[4] = sfstring("abc", 3, cetype_t_ext::CE_BYTES);
  vec_data[5] = sfstring(NA_STRING);

  SEXP store = PROTECT(slice_store_create_with_size(6, 256));
  slice_store_data & store_data = slice_store_data_ref(store);
  store_data.assign(0, "abc", 3, cetype_t_ext::CE_ASCII);
  store_data.assign(1, utf8_e, 2, cetype_t_ext::CE_ASCII_OR_UTF8);
  store_data.assign(2, utf8_e, 2, cetype_t_ext::CE_NATIVE);
  store_data.assign(3, latin1_e, 1, cetype_t_ext::CE_LATIN1);
  store_data.assign(4, "abc", 3, cetype_t_ext::CE_BYTES);
  store_data.assign(5, nullptr, 0, cetype_t_ext::CE_NA);

  List out = List::create(
    _["vec"] = vec,
    _["slice"] = store
  );
  UNPROTECT(2);
  return out;
}

// [[Rcpp::export]]
List sf_external_slice_store_state(SEXP x) {
  slice_store_data & data = slice_store_data_ref(x);
  return List::create(
    _["initial_slice_size"] = data.initial_slice_size_override.has_value() ?
      Rcpp::wrap(static_cast<int>(*data.initial_slice_size_override)) :
      Rcpp::wrap(NA_INTEGER),
    _["slice_count"] = static_cast<int>(data.slices.size()),
    _["current_slice_capacity"] = static_cast<int>(data.current_slice_capacity)
  );
}
'

is_solaris <- function() {
  grepl("SunOS", Sys.info()[["sysname"]])
}

can_source_cpp <- !is_solaris()

compile_regressions <- function() {
  if (!can_source_cpp) {
    return(invisible(NULL))
  }
  if (exists("sf_external_regressions", mode = "function", inherits = FALSE)) {
    return(invisible(NULL))
  }

  R_TESTS <- Sys.getenv("R_TESTS")
  if (nzchar(R_TESTS)) {
    R_TESTS_absolute <- normalizePath(R_TESTS)
    Sys.setenv(R_TESTS = R_TESTS_absolute)
  }
  Rcpp::sourceCpp(code = regression_external_source)
  if (nzchar(R_TESTS)) {
    Sys.setenv(R_TESTS = R_TESTS)
  }
  invisible(NULL)
}

encode_native_uint <- function(x, size) {
  remaining <- as.numeric(x)
  out <- as.raw(integer(size))
  for (i in seq_len(size)) {
    byte <- as.integer(remaining %% 256)
    out[[if (.Platform$endian == "little") i else size - i + 1L]] <- as.raw(byte)
    remaining <- floor(remaining / 256)
  }
  out
}

find_raw_subsequence <- function(haystack, needle) {
  stopifnot(is.raw(haystack), is.raw(needle), length(needle) > 0L)
  limit <- length(haystack) - length(needle) + 1L
  if (limit < 1L) {
    stop("raw subsequence longer than input")
  }
  for (i in seq_len(limit)) {
    if (identical(haystack[i:(i + length(needle) - 1L)], needle)) {
      return(i)
    }
  }
  stop("raw subsequence not found")
}

mutate_serialized_state <- function(x, expected_state, mutate) {
  serialized <- serialize(x, NULL, xdr = FALSE)
  start <- find_raw_subsequence(serialized, expected_state)
  state_idx <- start:(start + length(expected_state) - 1L)
  mutated_state <- mutate(serialized[state_idx])
  stopifnot(is.raw(mutated_state), length(mutated_state) == length(expected_state))
  serialized[state_idx] <- mutated_state
  serialized
}

run_regressions <- function(nthreads = 1L) {
  x <- convert_to_sf_vector(c("abcdef", "ghijkl"))
  y <- sf_substr(x, c(1L, 2L), 1L, nthreads = nthreads)
  stopifnot(string_identical(y, c("a", "")))
  materialize(x)
  y2 <- sf_substr(x, c(1L, 2L), 1L, nthreads = nthreads)
  stopifnot(string_identical(y2, c("a", "")))

  err <- tryCatch({
    stringfish:::sf_assign(sf_vector(2), 3, "x")
    NULL
  }, error = identity)
  stopifnot(inherits(err, "error"))

  collapsed <- sf_collapse(c("a", NA_character_), ",")
  stopifnot(is.character(collapsed), length(collapsed) == 1L, is.na(collapsed[[1]]))
  stopifnot(identical(sf_paste(nthreads = nthreads), ""))
  stopifnot(identical(sf_paste("a", "b", sep = NA_character_, nthreads = nthreads), NA_character_))
  stopifnot(identical(sf_collapse(c("a", "b"), NA_character_), NA_character_))

  stopifnot(string_identical(sf_tolower(c("ABC", NA_character_)), c("abc", NA_character_)))
  stopifnot(string_identical(sf_toupper(c("abc", NA_character_)), c("ABC", NA_character_)))

  stopifnot(identical(sf_compare("a", c("a", "b", NA_character_), nthreads = nthreads), c(TRUE, FALSE, NA)))
  stopifnot(identical(sf_compare(c("a", "b", NA_character_), "a", nthreads = nthreads), c(TRUE, FALSE, NA)))

  latin1_word <- iconv("café", "UTF-8", "latin1")
  Encoding(latin1_word) <- "latin1"
  bytes_word <- latin1_word
  Encoding(bytes_word) <- "bytes"
  normalized <- convert_to_sf_vector(latin1_word)
  stopifnot(identical(normalized, "café"), identical(Encoding(normalized), "UTF-8"))
  stopifnot(identical(sf_compare(c("café", bytes_word, "café"), latin1_word, nthreads = nthreads), c(TRUE, TRUE, TRUE)))

  tmp_latin1 <- tempfile(fileext = ".txt")
  con <- file(tmp_latin1, open = "wb")
  writeBin(c(charToRaw(latin1_word), as.raw(0x0a)), con)
  close(con)
  on.exit(unlink(tmp_latin1), add = TRUE)
  read_back <- sf_readLines(tmp_latin1, encoding = "latin1")
  stopifnot(identical(read_back, "café"), identical(Encoding(read_back), "UTF-8"))

  err <- tryCatch({
    sf_grepl("a", c("a", "b"), nthreads = nthreads)
    NULL
  }, error = identity)
  stopifnot(inherits(err, "error"))
  err <- tryCatch({
    sf_split("a", c("a", "b"), nthreads = nthreads)
    NULL
  }, error = identity)
  stopifnot(inherits(err, "error"))
  err <- tryCatch({
    sf_gsub("a", "a", c("x", "y"), nthreads = nthreads)
    NULL
  }, error = identity)
  stopifnot(inherits(err, "error"))

  subset_x <- convert_to_sf_vector(c("a", "b", "c"))
  stopifnot(string_identical(subset_x[c(3L, 1L, NA_integer_, 4L)], c("c", "a", NA_character_, NA_character_)))
  stopifnot(get_string_type(subset_x[c(3L, 1L)]) == "stringfish vector")
  materialize(subset_x)
  stopifnot(string_identical(subset_x[c(3L, 1L, NA_integer_, 4L)], c("c", "a", NA_character_, NA_character_)))
  stopifnot(get_string_type(subset_x[c(3L, 1L)]) == "stringfish vector")

  stopifnot(identical(
    materialize(sf_split("abc", "(?<=a)", nthreads = nthreads)[[1L]]),
    strsplit("abc", "(?<=a)", perl = TRUE)[[1L]]
  ))
  stopifnot(identical(
    materialize(sf_split("abc", "^", nthreads = nthreads)[[1L]]),
    c("", "abc")
  ))
  stopifnot(identical(
    materialize(sf_split("abc", "$", nthreads = nthreads)[[1L]]),
    c("abc", "")
  ))

  tmp_bytes <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp_bytes), add = TRUE)
  bytes_word <- rawToChar(as.raw(0xE9))
  Encoding(bytes_word) <- "bytes"
  sf_writeLines(bytes_word, tmp_bytes, encode_mode = "UTF-8")
  stopifnot(identical(readBin(tmp_bytes, "raw", n = 2L), as.raw(c(0xE9, 0x0A))))

  if (can_source_cpp) {
    out <- sf_external_regressions(c("ab", NA_character_), nthreads)
    stopifnot(identical(out$nchar, c(2L, NA_integer_)))
    stopifnot(identical(out$compare, c(TRUE, NA)))
    stopifnot(string_identical(out$substr, c("a", NA_character_)))
    stopifnot(length(out$random) == 4L)

    native_word <- "é"
    Encoding(native_word) <- "unknown"
    stopifnot(identical(Encoding(native_word), "unknown"))
    enc <- sf_external_encoding_regressions(native_word)
    stopifnot(identical(unname(enc), c(3L, 3L, 3L, 3L, 0L, 0L, 0L, 0L, 0L)))

    sized_store <- stringfish:::slice_store_create_with_size(4L, 256L)
    sized_state <- sf_external_slice_store_state(sized_store)
    stopifnot(identical(sized_state$initial_slice_size, 256L))
    stopifnot(identical(sized_state$slice_count, 1L))
    stopifnot(identical(sized_state$current_slice_capacity, 256L))

    stored <- sf_external_make_storage_vectors()
    vec_before <- sf_external_get_encodings(stored$vec)
    slice_before <- sf_external_get_encodings(stored$slice)
    vec_after <- unserialize(serialize(stored$vec, NULL))
    slice_after <- unserialize(serialize(stored$slice, NULL))
    stopifnot(identical(sf_external_get_encodings(vec_after), vec_before))
    stopifnot(identical(sf_external_get_encodings(slice_after), slice_before))
    stopifnot(identical(get_string_type(vec_after), "stringfish vector"))
    stopifnot(identical(get_string_type(slice_after), "stringfish slice store"))

    serialized_state_abc <- c(
      encode_native_uint(1L, 8L),
      encode_native_uint(3L, 4L),
      as.raw(254L),
      charToRaw("abc")
    )
    truncated_serialized <- mutate_serialized_state(
      convert_to_sf_vector("abc"),
      serialized_state_abc,
      function(state) {
        state[[1L]] <- as.raw(0x02)
        state
      }
    )
    invalid_encoding_serialized <- mutate_serialized_state(
      convert_to_slice_store("abc"),
      serialized_state_abc,
      function(state) {
        state[[13L]] <- as.raw(0x04)
        state
      }
    )
    err <- tryCatch({
      unserialize(truncated_serialized)
      NULL
    }, error = identity)
    stopifnot(inherits(err, "error"))
    err <- tryCatch({
      unserialize(invalid_encoding_serialized)
      NULL
    }, error = identity)
    stopifnot(inherits(err, "error"))
    stopifnot(grepl("ALTREP", conditionMessage(err), fixed = TRUE))
  }
}

compile_regressions()
run_regressions(1L)
if (stringfish:::is_tbb()) {
  run_regressions(4L)
}

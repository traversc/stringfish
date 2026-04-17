suppressPackageStartupMessages(library(stringfish))

alternate_case_source <- '
// [[Rcpp::depends(stringfish)]]
// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include "sf_external.h"
using namespace Rcpp;

// [[Rcpp::export]]
SEXP sf_alternate_case(SEXP x) {
  RStringIndexer r(x);
  size_t len = r.size();
  SEXP output = PROTECT(sf_vector(len));
  sf_vec_data & output_data = sf_vec_data_ref(output);
  size_t i = 0;
  for(auto e : r) {
    if(e.ptr == nullptr) {
      i++; // increment output index
      continue;
    }
    std::string temp(e.len, \'\\0\');
    bool case_switch = false;
    for(int j=0; j<e.len; j++) {
      if((e.ptr[j] >= 65) & (e.ptr[j] <= 90)) {
        if((case_switch = !case_switch)) {
          temp[j] = e.ptr[j] + 32;
          continue;
        }
      } else if((e.ptr[j] >= 97) & (e.ptr[j] <= 122)) {
        if(!(case_switch = !case_switch)) {
          temp[j] = e.ptr[j] - 32;
          continue;
        }
      } else if(e.ptr[j] == 32) {
        case_switch = false;
      }
      temp[j] = e.ptr[j];
    }
    output_data[i] = sfstring(temp, e.enc);
    i++;
  }
  UNPROTECT(1);
  return output;
}
'


is_solaris <- function() {
  grepl('SunOS',Sys.info()['sysname'])
}

can_source_cpp <- !is_solaris()

myfile <- tempfile()
print(myfile)

# For a test set, we are using the 500 most common Icelandic words
# This is a pretty good test set because all Icelandic words can be encoded as either UTF-8 or latin1. It also contains a mix of ASCII and non-ASCII strings
# https://en.wikipedia.org/wiki/ISO/IEC_8859-1
test_words_file <- system.file("icelandic_words_500_utf8.txt", package = "stringfish")
if (!nzchar(test_words_file)) {
  test_words_file <- file.path("inst", "icelandic_words_500_utf8.txt")
}
stopifnot(file.exists(test_words_file))
i500_utf8 <- readLines(test_words_file, encoding = "UTF-8", warn = FALSE)
i500_latin1 <- iconv(i500_utf8, "UTF-8", "latin1")

catn <- function(...) {
  cat(..., "\n")
}

split_list_identical <- function(x, y) {
  stopifnot(length(x) == length(y))
  all(vapply(seq_along(y), function(i) {
    string_identical(x[[i]], y[[i]])
  }, logical(1)))
}

to_latin1_list <- function(x) {
  lapply(x, function(z) iconv(z, from = "UTF-8", to = "latin1"))
}

ntests <- 50

use_tbb <- stringfish:::is_tbb()
cat("using thread building blocks?", use_tbb, "\n")
if(use_tbb) {
  nthreads <- c(1,4)
} else {
  nthreads <- 1
}

print(sessionInfo())
print(utils::localeToCharset())

if(can_source_cpp) {
  R_TESTS <- Sys.getenv("R_TESTS") # startup.Rs
  if (nzchar(R_TESTS)) {
    R_TESTS_absolute <- normalizePath(R_TESTS)
    Sys.setenv(R_TESTS = R_TESTS_absolute)
  }
  Rcpp::sourceCpp(code = alternate_case_source)
  if (nzchar(R_TESTS)) Sys.setenv(R_TESTS = R_TESTS)
} else {
  catn("skipping sourceCpp-backed tests")
}
for(.j in 1:3) {
  cat("iteration", .j, "\n")
  if(.j %% 2 == 0) {
    stringfish:::set_is_utf8_locale()
  } else {
    stringfish:::unset_is_utf8_locale()
  }
  for(nt in nthreads) {
    cat("number of threads", nt, "\n")

    catn("test altrep serialization (base::serialize)")
    x <- convert_to_sf_vector(sample(c(i500_utf8, i500_latin1), size = length(i500_utf8) + length(i500_latin1), replace=TRUE))
    y <- unserialize(serialize(x, NULL))
    stopifnot(string_identical(x,y))
    stopifnot(get_string_type(y) == "stringfish vector")
    x_slice <- convert_to_slice_store(sample(c(i500_utf8, i500_latin1), size = length(i500_utf8) + length(i500_latin1), replace=TRUE))
    y_slice <- unserialize(serialize(x_slice, NULL))
    stopifnot(string_identical(x_slice, y_slice))
    stopifnot(get_string_type(y_slice) == "stringfish slice store")

    catn("sf_assign")
    for(. in 1:ntests) {
      # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
      x <- sf_vector(10)
      y <- character(10)
      for(i in 1:10) {
        new_str <- sample(c(i500_utf8,i500_latin1),1)
        if(.j %% 2 == 1) materialize(x)
        stringfish:::sf_assign(x, i, new_str)
        y[i] <- new_str
      }
      stopifnot(string_identical(x,y))
    }
      
      catn("sf_iconv")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        x <- sf_iconv(i500_latin1, "latin1", "UTF-8")
        if(.j %% 2 == 1) materialize(x)
        y <- sf_iconv(i500_utf8, "UTF-8", "latin1")
        stopifnot(string_identical(x, i500_utf8))
        stopifnot(string_identical(y, i500_latin1))
        y <- sf_iconv(convert_to_sf_vector(i500_utf8), "UTF-8", "latin1")
        x <- sf_iconv(y, "latin1", "UTF-8")
        stopifnot(string_identical(x, i500_utf8))
        stopifnot(string_identical(y, i500_latin1))
      }
      
      catn("sf_nchar")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        x <- sf_iconv(i500_utf8, "UTF-8", "latin1")
        if(.j %% 2 == 1) materialize(x)
        y <- convert_to_sf_vector(i500_utf8)
        stopifnot( identical(sf_nchar(x, nthreads = nt), nchar(x)) )
        stopifnot( identical(sf_nchar(y, nthreads = nt), nchar(y)) )
        stopifnot( identical(sf_nchar(x, nthreads = nt), nchar(i500_latin1)) )
        stopifnot( identical(sf_nchar(y, nthreads = nt), nchar(i500_utf8)) )
        stopifnot( identical(sf_nchar(x, type = "bytes", nthreads = nt), nchar(i500_latin1, type = "bytes")) )
        stopifnot( identical(sf_nchar(y, type = "bytes", nthreads = nt), nchar(i500_utf8, type = "bytes")) )
      }
      
      catn("sf_substr")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        start <- sample(-10:10, size=1)
        if(start < 0) {
          rstart <- sf_nchar(i500_latin1, nthreads = nt) + start + 1
        } else {
          rstart <- start
        }
        stop <- sample(-10:10, size=1)
        if(stop < 0) {
          rstop <- sf_nchar(i500_latin1, nthreads = nt) + stop + 1
        } else {
          rstop <- stop
        }
        x <- sf_substr(i500_latin1, start, stop, nthreads = nt)
        if(.j %% 2 == 1) materialize(x)
        y <- substr(i500_latin1, rstart, rstop)
        x2 <- sf_substr(i500_utf8, start, stop, nthreads = nt)
        y2 <- substr(i500_utf8, rstart, rstop)
        stopifnot(string_identical(x, y))
        stopifnot(string_identical(x2, y2))
      }
      
      catn("sf_collapse")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        x <- sf_collapse(i500_latin1, collapse = ":::")
        if(.j %% 2 == 1) materialize(x)
        y <- paste0(i500_latin1, collapse = ":::")
        # stopifnot(string_identical(x, y)) # paste0 converts to UTF-8 -- doesn't respect encoding
        stopifnot(x == y)
        x <- sf_collapse(i500_latin1, collapse = ",")
        if(.j %% 2 == 1) materialize(x)
        y <- paste0(i500_latin1, collapse = ",")
        stopifnot(x == y)
        x <- sf_collapse(i500_utf8, collapse = ":::")
        if(.j %% 2 == 1) materialize(x)
        y <- paste0(i500_utf8, collapse = ":::")
        stopifnot(x == y)
        x <- sf_collapse(i500_utf8, collapse = ",")
        if(.j %% 2 == 1) materialize(x)
        y <- paste0(i500_utf8, collapse = ",")
        stopifnot(x == y)
      }
      
      catn("sf_paste")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        x <- do.call(paste, c(as.list(i500_latin1), sep=":::"))
        y <- do.call(sf_paste, c(as.list(i500_latin1), sep=":::", nthreads = nt))
        if(.j %% 2 == 1) materialize(y)
        stopifnot(x == y)
        x <- do.call(paste, c(as.list(i500_latin1), sep=":::"))
        y <- do.call(sf_paste, c(as.list(i500_latin1), sep=":::", nthreads = nt))
        if(.j %% 2 == 1) materialize(y)
        stopifnot(x == y)
        x <- do.call(paste, c(as.list(i500_utf8), sep=","))
        y <- do.call(sf_paste, c(as.list(i500_utf8), sep=",", nthreads = nt))
        if(.j %% 2 == 1) materialize(y)
        stopifnot(x == y)
        x <- do.call(paste, c(as.list(i500_utf8), sep=","))
        y <- do.call(sf_paste, c(as.list(i500_utf8), sep=",", nthreads = nt))
        if(.j %% 2 == 1) materialize(y)
        stopifnot(x == y)
      }
      
      catn("sf_readLines")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        writeLines(i500_utf8, con=myfile, useBytes=T)
        x <- sf_readLines(myfile, encoding = "UTF-8")
        if(.j %% 2 == 1) materialize(x)
        y <- readLines(myfile); Encoding(y) <- "UTF-8"
        stopifnot(string_identical(x, y))
        writeLines(i500_latin1, con=myfile)
        x <- sf_readLines(myfile, encoding = "latin1")
        if(.j %% 2 == 1) materialize(x)
        y <- readLines(myfile); Encoding(y) <- "latin1"
        stopifnot(string_identical(x, y))
      }

      catn("sf_writeLines")
      for(. in 1:ntests) {
        write_utf8 <- tempfile()
        on.exit(unlink(write_utf8), add = TRUE)
        sf_writeLines(i500_utf8, write_utf8, encode_mode = "UTF-8")
        y <- readLines(write_utf8, warn = FALSE, encoding = "UTF-8")
        Encoding(y) <- "UTF-8"
        stopifnot(string_identical(y, i500_utf8))

        write_bytes <- tempfile()
        on.exit(unlink(write_bytes), add = TRUE)
        bytes_word <- rawToChar(as.raw(0xE9))
        Encoding(bytes_word) <- "bytes"
        sf_writeLines(bytes_word, write_bytes, encode_mode = "UTF-8")
        stopifnot(identical(readBin(write_bytes, "raw", n = 2L), as.raw(c(0xE9, 0x0A))))
      }
      
      catn("sf_grepl")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        p <- rawToChar(as.raw(c(0x5e, 0xc3, 0xb6, 0x2e, 0x2b)))
        Encoding(p) <- "UTF-8"
        p2 <- rawToChar(as.raw(c(0x5e, 0xf6, 0x2e, 0x2b)))
        Encoding(p2) <- "latin1"
        stopifnot(all(sf_grepl(i500_utf8, p, nthreads = nt) == grepl(p, i500_utf8)))
        stopifnot(all(sf_grepl(i500_latin1, p2, nthreads = nt) == grepl(p2, i500_latin1)))
        
        stopifnot(sf_grepl(i500_utf8, "[a-f]", nthreads = nt) == grepl("[a-f]", i500_utf8))
        stopifnot(sf_grepl(i500_latin1, "[a-f]", nthreads = nt) == grepl("[a-f]", i500_latin1))
      }
      
      catn("sf_grepl fixed")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        p <- rawToChar(as.raw(c(0xc3, 0xb6)))
        Encoding(p) <- "UTF-8"
        p2 <- rawToChar(as.raw(c(0xf6)))
        Encoding(p2) <- "latin1"
        stopifnot(all(sf_grepl(i500_utf8, p, fixed = T, nthreads = nt) == grepl(p, i500_utf8)))
        stopifnot(all(sf_grepl(i500_latin1, p2, fixed = T,  nthreads = nt) == grepl(p2, i500_latin1)))
        
        stopifnot(sf_grepl(i500_utf8, "[a-f]", nthreads = nt) == grepl("[a-f]", i500_utf8))
        stopifnot(sf_grepl(i500_latin1, "[a-f]", nthreads = nt) == grepl("[a-f]", i500_latin1))
      }
      
      catn("sf_gsub")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        p <- rawToChar(as.raw(c(0x5e, 0xc3, 0xb6, 0x2e, 0x2b, 0x28, 0x2e, 0x29, 0x24)))
        Encoding(p) <- "UTF-8"
        p2 <- rawToChar(as.raw(c(0x5e, 0xf6, 0x2e, 0x2b, 0x28, 0x2e, 0x29, 0x24)))
        Encoding(p2) <- "latin1"
        stopifnot(all(sf_gsub(i500_utf8, p, "$1", nthreads = nt) == gsub(p, "\\1", i500_utf8)))
        stopifnot(all(sf_gsub(i500_latin1, p2, "$1", nthreads = nt) == gsub(p2, "\\1", i500_latin1)))
        
        p <- "^h.+(.)$"
        stopifnot(all(sf_gsub(i500_utf8, p, "$1", nthreads = nt) == gsub(p, "\\1", i500_utf8)))
        stopifnot(all(sf_gsub(i500_latin1, p, "$1", nthreads = nt) == gsub(p, "\\1", i500_latin1)))
      }
      
      catn("sf_split")
      for(. in 1:ntests) {
        x <- sf_split(i500_utf8, "", nthreads = nt)
        stopifnot(split_list_identical(x, strsplit(i500_utf8, "", perl = TRUE)))

        x <- sf_split(rep("", 1e3), "a", nthreads=nt)
        stopifnot(all(x == ""))

        x <- sf_split(rep("", 1e3), "", nthreads=nt)
        stopifnot(all(x == ""))

        x <- sf_split(rep("abcde", 1e3), "f", nthreads=nt)
        stopifnot(all(x == "abcde"))

        utf8_subject <- c("café", "éclair", "plain", "é", "été")
        utf8_expected <- list(c("caf", ""), c("", "clair"), "plain", c("", ""), c("", "t", ""))
        x <- sf_split(utf8_subject, "é", nthreads = nt)
        stopifnot(split_list_identical(x, utf8_expected))

        utf8_regex_subject <- c("héj", "méow", "plain", "éx")
        utf8_regex_expected <- list(c("h", ""), c("m", "w"), "plain", c("", ""))
        x <- sf_split(utf8_regex_subject, "é.", nthreads = nt)
        stopifnot(split_list_identical(x, utf8_regex_expected))

        latin1_subject <- iconv(utf8_regex_subject, from = "UTF-8", to = "latin1")
        latin1_expected <- to_latin1_list(utf8_regex_expected)
        latin1_pattern <- iconv("é.", from = "UTF-8", to = "latin1")
        x <- sf_split(latin1_subject, latin1_pattern, nthreads = nt)
        x <- lapply(x, sf_iconv, from = "UTF-8", to = "latin1")
        stopifnot(split_list_identical(x, latin1_expected))

        x <- sf_split(latin1_subject, latin1_pattern, encode_mode = "byte", nthreads = nt)
        x <- lapply(x, function(z) {
          z <- materialize(z)
          Encoding(z) <- "latin1"
          z
        })
        stopifnot(split_list_identical(x, latin1_expected))

        zero_width <- materialize(sf_split("abc", "(?<=a)", nthreads = nt)[[1L]])
        stopifnot(identical(zero_width, strsplit("abc", "(?<=a)", perl = TRUE)[[1L]]))

        zero_width_start <- materialize(sf_split("abc", "^", nthreads = nt)[[1L]])
        stopifnot(identical(zero_width_start, c("", "abc")))

        zero_width_end <- materialize(sf_split("abc", "$", nthreads = nt)[[1L]])
        stopifnot(identical(zero_width_end, c("abc", "")))
      }
      
      catn("sf_toupper and sf_tolower")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        x1 <- sf_toupper(i500_latin1)
        x2 <- sf_toupper(i500_utf8)
        y1 <- sf_tolower(i500_latin1)
        if(.j %% 2 == 1) materialize(y1)
        y2 <- sf_tolower(i500_utf8)
        if(.j %% 2 == 1) materialize(y2)
        z1 <- sf_tolower(x1)
        z2 <- sf_tolower(x2)
        stopifnot(string_identical(z1, i500_latin1))
        stopifnot(string_identical(z2, i500_utf8))
        stopifnot(string_identical(y1, i500_latin1))
        stopifnot(string_identical(y2, i500_utf8))
        # base R functions also convert Unicode characters to upper
        # stopifnot(string_identical(x1, iconv(toupper(i500_latin1),"UTF-8", "latin1")))
        # stopifnot(string_identical(x2, toupper(i500_utf8)))
      }
      
      if(can_source_cpp) {
        catn("Rcpp test with sf_alternate_case")
        for(. in 1:ntests) {
          x <- c("hello world", "HELLO WORLD")
          string_identical(sf_alternate_case(x), c("hElLo wOrLd", "hElLo wOrLd"))
        }
        
        catn("sf_trim")
        for(. in 1:ntests) {
          # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
          x <- sf_trim(sf_paste("\t", i500_utf8, " \n"))
          if(.j %% 2 == 1) materialize(x)
          stopifnot(string_identical(x, i500_utf8))
          
          x <- sf_trim(sf_iconv(sf_paste("\t", i500_utf8, " \n"), from = "UTF-8", to = "latin1"), encode_mode = "byte")
          x <- materialize(x)
          Encoding(x) <- "latin1"
          stopifnot(string_identical(x, i500_latin1))
        }
      }
      
      # Disable check due to https://bugs.r-project.org/show_bug.cgi?id=18211
      catn("sf_match")
      # gctorture(TRUE)
      for(. in 1:ntests) {
        i500_utf8_shuffled <- c(NA_character_, i500_utf8[sample(length(i500_utf8))][-1])
        temp <- c(i500_utf8, NA_character_)
        x <- sf_match(temp, i500_utf8_shuffled)
        # y <- match(temp, i500_utf8_shuffled)
        # stopifnot(identical(x,y))
        i500_latin1_shuffled <- c(NA_character_, i500_latin1[sample(length(i500_latin1))][-1])
        temp <- c(i500_latin1, NA_character_)
        x <- sf_match(c(i500_latin1, NA_character_), i500_latin1_shuffled)
        # y <- match(temp, i500_latin1_shuffled)
        # stopifnot(identical(x,y))
      }
      # gctorture(FALSE)
      
      catn("sf_compare")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        i500_utf8_shuffled <- i500_utf8
        i500_utf8_shuffled[sample(length(i500_utf8), size = 100)] <- ""
        x <- sf_compare(c(i500_utf8, NA_character_), c(i500_utf8_shuffled, NA_character_))
        y <- c(i500_utf8, NA_character_) == c(i500_utf8_shuffled, NA_character_)
        stopifnot(identical(x,y))
        
        i500_latin1_shuffled <- i500_latin1
        i500_latin1_shuffled[sample(length(i500_latin1), size = 100)] <- ""
        x <- sf_compare(c(i500_latin1, NA_character_), c(i500_latin1_shuffled, NA_character_))
        y <- c(i500_latin1, NA_character_) == c(i500_latin1_shuffled, NA_character_)
        stopifnot(identical(x,y))
      }

      catn("sf_concat")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        i500_utf8_shuffled <- i500_utf8
        i500_utf8_shuffled[sample(length(i500_utf8), size = 100)] <- ""
        x <- sfc(sfc(i500_utf8, NA_character_), sfc(i500_utf8_shuffled, NA_character_), character(0))
        if(.j %% 2 == 1) materialize(x)
        y <- sfc(sfc(convert_to_sf_vector(i500_utf8), NA_character_), sfc(convert_to_sf_vector(i500_utf8_shuffled), NA_character_), character(0))
        z <- c(c(convert_to_sf_vector(i500_utf8), NA_character_), c(convert_to_sf_vector(i500_utf8_shuffled), NA_character_), character(0))
        z2 <- c(c(i500_utf8, NA_character_), c(i500_utf8_shuffled, NA_character_), character(0))
        stopifnot(string_identical(x,y))
        stopifnot(string_identical(x,z))
        stopifnot(string_identical(x,z2))
        stopifnot(string_identical(x,y))
        stopifnot(identical(sfc(character(0)), character(0)))
      }
      print(gc())
    }
  }
print("end")

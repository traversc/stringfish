# stringfish - Quick Serialization of R Objects
# Copyright (C) 2020-present Travers Ching
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# 
# You can contact the author at:
#   https://github.com/traversc/stringfish


#' sf_vector
#' 
#' Creates a new empty stringfish vector
#' @usage sf_vector(len)
#' @param len length of the new vector
#' @return A new empty stringfish vector
#' @details 
#' This is a backwards-compatible alias for `sf_vector_create(len)`.
#' It creates a new stringfish vector, an alt-rep character vector backed by a C++ "std::vector" as the internal memory representation.
#' The vector type is "sfstring", which is a simple C++ class containing a "std::string" and a single byte (uint8_t) representing the encoding. 
#' @examples
#' x <- sf_vector(10)
#' sf_assign(x, 1, "hello world")
#' sf_assign(x, 2, "another string")
#' @name sf_vector
NULL

#' sf_vector_create
#' 
#' Creates a new empty stringfish vector
#' @usage sf_vector_create(len)
#' @param len length of the new vector
#' @return A new stringfish vector
#' @details 
#' This function creates a new empty `sf_vec`-backed stringfish vector.
#' If you want to fill the vector from character data, use `convert_to_sf_vector`.
#' @examples
#' x <- sf_vector_create(4)
#' @name sf_vector_create
NULL

#' slice_store_create
#'
#' Creates a new empty slice-store-backed stringfish vector
#' @usage slice_store_create(len)
#' @param len length of the new vector
#' @return A new slice-store-backed stringfish vector
#' @details
#' This function creates a new stringfish vector backed by `slice_store`,
#' which stores string bytes in append-only slices plus per-element records.
#' If you want to fill the vector from character data, use `convert_to_slice_store`.
#' @examples
#' x <- slice_store_create(4)
#' @name slice_store_create
NULL

#' slice_store_create_with_size
#'
#' Creates a new empty slice-store-backed stringfish vector with a fixed initial slice size
#' @usage slice_store_create_with_size(len, initial_slice_size)
#' @param len length of the new vector
#' @param initial_slice_size Initial size of the first underlying `slice_store` slice.
#' @return A new slice-store-backed stringfish vector
#' @details
#' This function creates a new stringfish vector backed by `slice_store`,
#' and uses `initial_slice_size` for the first slice allocation instead of the default heuristic.
#' If you want to fill the vector from character data, use `convert_to_slice_store`.
#' @examples
#' x <- slice_store_create_with_size(4, 256)
#' @name slice_store_create_with_size
NULL

#' sf_assign
#' 
#' Assigns a new string to a stringfish vector or any other character vector
#' @usage sf_assign(x, i, e)
#' @param x the vector
#' @param i the index to assign to
#' @param e the new string to replace at i in x
#' @return No return value, the function assigns an element to an existing stringfish vector
#' @details 
#' A function to assign a new element to an existing character vector. If the the vector is a stringfish vector, it does so without materialization. 
#' @examples
#' x <- sf_vector(10)
#' sf_assign(x, 1, "hello world")
#' sf_assign(x, 2, "another string")
#' @name sf_assign
NULL

#' get_string_type
#' 
#' Returns the type of the character vector
#' @usage get_string_type(x)
#' @param x the vector
#' @return The type of vector
#' @details 
#' A function that returns the type of character vector. Possible values are "normal vector", "stringfish vector",
#' "stringfish vector (materialized)", "stringfish slice store", "stringfish slice store (materialized)"
#' or "other alt-rep vector"
#' @examples
#' x <- sf_vector(10)
#' get_string_type(x) # returns "stringfish vector"
#' x <- character(10)
#' get_string_type(x) # returns "normal vector"
#' @name get_string_type
NULL

#' materialize
#' 
#' Materializes an alt-rep object
#' @usage materialize(x)
#' @param x An alt-rep object
#' @return x
#' @details 
#' Materializes any alt-rep object and then returns it. 
#' Note: the object is materialized regardless of whether the return value is assigned to a variable. 
#' @examples
#' x <- sf_vector(10)
#' sf_assign(x, 1, "hello world")
#' sf_assign(x, 2, "another string")
#' x <- materialize(x)
#' @name materialize
NULL

#' convert_to_sf_vector
#' 
#' Converts a character vector to an `sf_vec`-backed stringfish vector
#' @usage convert_to_sf_vector(x, length.out = length(x))
#' @param x A character vector
#' @param length.out Optional output length used to recycle `x`
#' @return The converted character vector
#' @details 
#' Converts a character vector to a stringfish vector backed by `sf_vec`.
#' If `length.out` is supplied, `x` is recycled to that length before conversion.
#' The opposite of `materialize`.
#' @examples
#' x <- convert_to_sf_vector(letters)
#' @name convert_to_sf_vector
NULL

#' convert_to_slice_store
#' 
#' Converts a character vector to a slice-store-backed stringfish vector
#' @usage convert_to_slice_store(x, length.out = length(x))
#' @param x A character vector
#' @param length.out Optional output length used to recycle `x`
#' @return The converted character vector
#' @details
#' Converts a character vector to a stringfish vector backed by `slice_store`.
#' If `length.out` is supplied, `x` is recycled to that length before conversion.
#' The converter pre-sizes the first `slice_store` slice from the normalized string bytes when possible.
#' The opposite of `materialize`.
#' @examples
#' x <- convert_to_slice_store(letters)
#' @name convert_to_slice_store
NULL

#' sf_iconv
#' 
#' Converts encoding of one character vector to another
#' @usage sf_iconv(x, from, to, nthreads = getOption("stringfish.nthreads", 1L))
#' @param x An alt-rep object
#' @param from the encoding to assume of `x`
#' @param nthreads Number of threads to use
#' @param to the new encoding
#' @return the converted character vector as a stringfish vector
#' @details 
#' This is an analogue to the base R function `iconv`. It converts a string from one encoding (e.g. latin1 or UTF-8) to another
#' @seealso iconv
#' @examples
#' x <- "fa\xE7ile"
#' Encoding(x) <- "latin1"
#' sf_iconv(x, "latin1", "UTF-8")
#' @name sf_iconv
NULL

#' sf_nchar
#' 
#' Counts the number of characters in a character vector
#' @usage sf_nchar(x, type = "chars", nthreads = getOption("stringfish.nthreads", 1L))
#' @param x A character vector
#' @param type The type of counting to perform ("chars" or "bytes", default: "chars")
#' @param nthreads Number of threads to use
#' @return An integer vector of the number of characters
#' @details 
#' Returns the number of characters per string. The type of counting only matters for UTF-8 strings, where a character can be represented by multiple bytes. 
#' @seealso nchar
#' @examples
#' x <- "fa\xE7ile"
#' Encoding(x) <- "latin1"
#' x <- sf_iconv(x, "latin1", "UTF-8")
#' @name sf_nchar
NULL

#' sf_substr
#' 
#' Extracts substrings from a character vector
#' @usage sf_substr(x, start, stop, nthreads = getOption("stringfish.nthreads", 1L))
#' @param x A character vector
#' @param start The begining to extract from
#' @param stop The end to extract from
#' @param nthreads Number of threads to use
#' @return A stringfish vector of substrings
#' @details 
#' This works the same way as `substr`, but in addition allows negative indexing. 
#' Negative indicies count backwards from the end of the string, with -1 being the last character. 
#' @seealso substr
#' @examples
#' x <- c("fa\xE7ile", "hello world")
#' Encoding(x) <- "latin1"
#' x <- sf_iconv(x, "latin1", "UTF-8")
#' sf_substr(x, 4, -1) # extracts from the 4th character to the last
#' ## [1] "ile"  "lo world"
#' @name sf_substr
NULL

#' sf_collapse
#' 
#' Pastes a series of strings together separated by the `collapse` parameter
#' @usage sf_collapse(x, collapse)
#' @param x A character vector
#' @param collapse A single string
#' @return A single string with all values in `x` pasted together, separated by `collapse`.
#' @details 
#' This works the same way as `paste0(x, collapse=collapse)`
#' @seealso paste0, paste
#' @examples
#' x <- c("hello", "\\xe4\\xb8\\x96\\xe7\\x95\\x8c")
#' Encoding(x) <- "UTF-8"
#' sf_collapse(x, " ") # "hello world" in Japanese
#' sf_collapse(letters, "") # returns the alphabet
#' @name sf_collapse
NULL

#' sf_paste
#' 
#' Pastes a series of strings together
#' @usage sf_paste(..., sep = "", nthreads = getOption("stringfish.nthreads", 1L))
#' @param ... Any number of character vector strings
#' @param sep The seperating string between strings
#' @param nthreads Number of threads to use
#' @return A character vector where elements of the arguments are pasted together
#' @details 
#' This works the same way as `paste0(..., sep=sep)`
#' @seealso paste0, paste
#' @examples
#' x <- letters
#' y <- LETTERS
#' sf_paste(x,y, sep = ":")
#' @name sf_paste
NULL

#' sf_readLines
#' 
#' A function that reads a file line by line
#' @usage sf_readLines(file, encoding = "UTF-8")
#' @param file The file name
#' @param encoding The encoding to use (Default: UTF-8)
#' @return A stringfish vector of the lines in a file
#' @details 
#' A function for reading in text data using `std::ifstream`.
#' @seealso readLines
#' @examples
#' file <- tempfile()
#' sf_writeLines(letters, file)
#' sf_readLines(file)
#' @name sf_readLines
NULL

#' sf_writeLines
#' 
#' A function that writes text line by line
#' @usage sf_writeLines(text, file, sep = "\n", na_value = "NA", encode_mode = "UTF-8")
#' @param text A character to write to file
#' @param file Name of the file to write to
#' @param sep The line separator character(s)
#' @param na_value What to write in case of a NA string
#' @param encode_mode "UTF-8" or "byte". If "UTF-8", text strings are normalized to UTF-8 while `CE_BYTES` strings are written as raw bytes.
#' @details 
#' A function for writing text data using `std::ofstream`.
#' @seealso writeLines
#' @examples
#' file <- tempfile()
#' sf_writeLines(letters, file)
#' sf_readLines(file)
#' @name sf_writeLines
NULL

#' sf_grepl
#' 
#' A function that matches patterns and returns a logical vector
#' @usage sf_grepl(subject, pattern, encode_mode = "auto", fixed = FALSE, 
#' nthreads = getOption("stringfish.nthreads", 1L))
#' @param subject The subject character vector to search
#' @param pattern The pattern to search for
#' @param encode_mode "auto", "UTF-8" or "byte". Determines multi-byte (UTF-8) characters or single-byte characters are used.
#' @param fixed determines whether the pattern parameter should be interpreted literally or as a regular expression
#' @param nthreads Number of threads to use
#' @return A logical vector with the same length as subject
#' @details 
#' The function uses the PCRE2 library, which is also used internally by R. 
#' The encoding is based on the pattern string (or forced via the encode_mode parameter). 
#' Note: the order of paramters is switched compared to the `grepl` base R function, with subject being first. 
#' @seealso grepl
#' @examples
#' x <- sf_vector(10)
#' sf_assign(x, 1, "hello world")
#' pattern <- "^hello"
#' sf_grepl(x, pattern)
#' @name sf_grepl
NULL

#' sf_gsub
#' 
#' A function that performs pattern substitution
#' @usage sf_gsub(subject, pattern, replacement, encode_mode = "auto", fixed = FALSE, 
#' nthreads = getOption("stringfish.nthreads", 1L))
#' @param subject The subject character vector to search
#' @param pattern The pattern to search for
#' @param replacement The replacement string
#' @param encode_mode "auto", "UTF-8" or "byte". Determines multi-byte (UTF-8) characters or single-byte characters are used.
#' @param fixed determines whether the pattern parameter should be interpreted literally or as a regular expression
#' @param nthreads Number of threads to use
#' @return A stringfish vector of the replacement string
#' @details 
#' The function uses the PCRE2 library, which is also used internally by R. However, syntax may be slightly different. 
#' E.g.: capture groups: "\1" in R, but "$1" in PCRE2 (as in Perl). 
#' The encoding of the output is determined by the pattern (or forced using encode_mode parameter) and encodings should be compatible. 
#' E.g: mixing ASCII and UTF-8 is okay, but not UTF-8 and latin1. 
#' Note: the order of paramters is switched compared to the `gsub` base R function, with subject being first. 
#' @seealso gsub
#' @examples
#' x <- "hello world"
#' pattern <- "^hello (.+)"
#' replacement <- "goodbye $1"
#' sf_gsub(x, pattern, replacement)
#' @name sf_gsub
NULL

#' random_strings
#' 
#' A function that generates random strings
#' @usage random_strings(N, string_size = 50L, charset = "abcdefghijklmnopqrstuvwxyz",
#'                       vector_mode = "stringfish")
#' @param N The number of strings to generate
#' @param string_size Either a single non-negative integer applied to every output
#' string, or a non-negative integer vector of length `N`.
#' @param charset The characters used to generate the random strings (default: abcdefghijklmnopqrstuvwxyz)
#' @param vector_mode The type of character vector to generate (either stringfish or normal, default: stringfish)
#' @return A character vector of the random strings
#' @details 
#' A convenience function for generating test strings.
#' @examples
#' set.seed(1)
#' x <- random_strings(1e6, 80L, "ACGT", vector_mode = "stringfish")
#' y <- random_strings(4, c(1L, 2L, 4L, 8L), "ACGT")
#' @name random_strings
NULL

#' sf_toupper
#' 
#' A function converting a string to all uppercase
#' @usage sf_toupper(x)
#' @param x A character vector
#' @return A stringfish vector where all lowercase is converted to uppercase
#' @details 
#' Note: the function only converts ASCII characters. 
#' @seealso toupper
#' @examples
#' x <- letters
#' sf_toupper(x)
#' @name sf_toupper
NULL

#' sf_tolower
#' 
#' A function converting a string to all lowercase
#' @usage sf_tolower(x)
#' @param x A character vector
#' @return A stringfish vector where all uppercase is converted to lowercase
#' @details 
#' Note: the function only converts ASCII characters. 
#' @seealso tolower
#' @examples
#' x <- LETTERS
#' sf_tolower(x)
#' @name sf_tolower
NULL

#' sf_starts
#' 
#' A function for detecting a pattern at the start of a string
#' @usage sf_starts(subject, pattern, ...)
#' @param subject A character vector
#' @param pattern A string to look for at the start
#' @param ... Parameters passed to sf_grepl
#' @return A logical vector true if there is a match, false if no match, NA is the subject was NA
#' @seealso startsWith, sf_ends
#' @examples
#' x <- c("alpha", "beta", "gamma", "delta", "epsilon")
#' sf_starts(x, "a")
#' @name sf_starts
NULL

#' sf_ends
#' 
#' A function for detecting a pattern at the end of a string
#' @usage sf_ends(subject, pattern, ...)
#' @param subject A character vector
#' @param pattern A string to look for at the start
#' @param ... Parameters passed to sf_grepl
#' @return A logical vector true if there is a match, false if no match, NA is the subject was NA
#' @seealso endsWith, sf_starts
#' @examples
#' x <- c("alpha", "beta", "gamma", "delta", "epsilon")
#' sf_ends(x, "a")
#' @name sf_ends
NULL

#' sf_trim
#' 
#' A function to remove leading/trailing whitespace
#' @usage sf_trim(subject, which = c("both", "left", "right"), whitespace = "[ \\\\t\\\\r\\\\n]", ...)
#' @param subject A character vector
#' @param which "both", "left", or "right" determines which white space is removed
#' @param whitespace Whitespace characters (default: "[ \\\\t\\\\r\\\\n]")
#' @param ... Parameters passed to sf_gsub
#' @return A stringfish vector of trimmed whitespace
#' @seealso trimws
#' @examples
#' x <- c(" alpha ", " beta", " gamma ", "delta ", "epsilon ")
#' sf_trim(x)
#' @name sf_trim
NULL

#' sf_match
#' 
#' Returns a vector of the positions of x in table
#' @usage sf_match(x, table, nthreads = getOption("stringfish.nthreads", 1L))
#' @param x A character vector to search for in table
#' @param table A character vector to be matched against x
#' @param nthreads Number of threads to use
#' @return An integer vector of the indicies of each x element's position in table
#' @seealso match
#' @details Note: similarly to the base R function, long "table" vectors are not supported. This is due to the maximum integer value that can be returned (`.Machine$integer.max`)
#' @examples
#' sf_match("c", letters)
#' @name sf_match
NULL

#' sf_split
#' 
#' A function to split strings by a delimiter
#' @usage sf_split(subject, split, encode_mode = "auto", fixed = FALSE, 
#' nthreads = getOption("stringfish.nthreads", 1L))
#' @param subject A character vector
#' @param split A delimiter to split the string by
#' @param encode_mode "auto", "UTF-8" or "byte". Determines multi-byte (UTF-8) characters or single-byte characters are used.
#' @param fixed determines whether the split parameter should be interpreted literally or as a regular expression
#' @param nthreads Number of threads to use
#' @return A list of stringfish character vectors
#' @seealso strsplit
#' @examples
#' sf_split(datasets::state.name, "\\s") # split U.S. state names by any space character
#' @name sf_split
NULL

#' sf_compare
#' 
#' Returns a logical vector testing equality of strings from two string vectors
#' @usage sf_compare(x, y, nthreads = getOption("stringfish.nthreads", 1L))
#' @param x A character vector of length 1 or the same non-zero length as y
#' @param y Another character vector of length 1 or the same non-zero length as y
#' @param nthreads Number of threads to use
#' @return A logical vector
#' @details Note: the function tests semantic string equality. Non-byte text is normalized to a UTF-8 working representation, while `CE_BYTES` strings are compared byte-for-byte.
#' @examples
#' sf_compare(letters, "a")
#' @name sf_compare
NULL

#' @rdname sf_compare
sf_equals <- sf_compare

#' string_identical
#' 
#' Compare strings semantically or exactly
#' @usage string_identical(x, y, mode = c("semantic", "exact"))
#' @param x A character vector
#' @param y Another character vector to compare to x
#' @param mode Either `"semantic"` to compare text after normalizing non-byte
#' strings to UTF-8, or `"exact"` to additionally require matching encoding.
#' Strings marked as `"bytes"` are always compared exactly.
#' @return TRUE if strings are identical under the selected comparison mode
#' @seealso identical
#' @examples
#' x <- "fa\xE7ile"
#' Encoding(x) <- "latin1"
#' y <- iconv(x, "latin1", "UTF-8")
#' identical(x, y) # TRUE
#' string_identical(x, y) # TRUE
#' string_identical(x, y, mode = "exact") # FALSE
#' @name string_identical
NULL

#' sf_concat
#' 
#' Appends vectors together
#' @usage sf_concat(...)
#' @param ... Any number of vectors, coerced to character vector if necessary
#' @return A concatenated stringfish vector
#' @examples
#' sf_concat(letters, 1:5)
#' @name sf_concat
NULL

#' @rdname sf_concat
sfc <- sf_concat

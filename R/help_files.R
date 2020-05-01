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
#' Creates a new stringfish vector 
#' @usage sf_vector(len)
#' @param len length of the new vector
#' @return A new (empty) stringfish vector
#' @details 
#' This function creates a new stringfish vector, an alt-rep character vector backed by a C++ "std::vector" as the internal memory representation. 
#' The vector type is "sfstring", which is a simple C++ class containing a "std::string" and a single byte (uint8_t) representing the encoding. 
#' @examples 
#' x <- sf_vector(10)
#' sf_assign(x, 1, "hello world")
#' sf_assign(x, 2, "another string")
#' @name sf_vector
NULL

#' sf_assign
#' 
#' Assigns a new string to a stringfish vector or any other character vector
#' @usage sf_assign(x, i, e)
#' @param x the vector
#' @param i the index to assign to
#' @param e the new string to replace at i in x
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
#' A function that returns the type of character vector. Possible values are "normal vector", "stringfish vector", "stringfish vector (materialized)" or "other alt-rep vector"
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

#' convert_to_sf
#' 
#' Converts a character vector to a stringfish vector
#' @usage convert_to_sf(x)
#' @param x A character vector
#' @return The converted character vector
#' @details 
#' Converts a character vector to a stringfish vector. The opposite of `materialize`.
#' @examples 
#' x <- convert_to_sf(letters)
#' @name convert_to_sf
NULL

#' sf_iconv
#' 
#' Converts encoding of one character vector to another
#' @usage sf_iconv(x, from, to)
#' @param x An alt-rep object
#' @param from the encoding to assume of `x`
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
#' @usage sf_nchar(x, type = "chars")
#' @param x A character vector
#' @param type The type of counting to perform ("chars" or "bytes", default: "chars")
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
#' @usage sf_substr(x, start, stop)
#' @param x A character vector
#' @param start The begining to extract from
#' @param stop The end to extract from
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
#' @usage sf_paste(..., sep = "")
#' @param ... Any number of character vector strings
#' @param sep The seperating string between strings
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
#' writeLines(letters, file)
#' sf_readLines(file)
#' @name sf_readLines
NULL


#' sf_grepl
#' 
#' A function that matches patterns and returns a logical vector
#' @usage sf_grepl(subject, pattern, encode_mode = "auto")
#' @param subject The subject character vector to search
#' @param pattern The pattern to search for
#' @param encode_mode The encoding type to use (UTF-8, latin1, bytes, native or auto)
#' @return A logical vector with the same length as subject
#' @details 
#' The function uses the PCRE2 library, which is also used internally by R. 
#' The encoding is based on the pattern string (or forced via the encode_mode parameter). 
#' Note: the order of paramters is switched compared to the `grepl` base R function, with subject being first. 
#' See also: https://www.pcre.org/current/doc/html/pcre2api.html for more documentation on match syntax. 
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
#' @usage sf_gsub(subject, pattern, replacement, encode_mode = "auto")
#' @param subject The subject character vector to search
#' @param pattern The pattern to search for
#' @param replacement The replacement string
#' @param encode_mode The encoding type to use (UTF-8, latin1, bytes, native or auto)
#' @return A stringfish vector of the replacement string
#' @details 
#' The function uses the PCRE2 library, which is also used internally by R. However, syntax may be slightly different. 
#' E.g.: capture groups: "\1" in R, but "$1" in PCRE2 (as in Perl). 
#' The encoding of the output is determined by the pattern (or forced using encode_mode parameter) and encodings should be compatible. 
#' E.g: mixing ASCII and UTF-8 is okay, but not UTF-8 and latin1. 
#' Note: the order of paramters is switched compared to the `gsub` base R function, with subject being first. 
#' See also: https://www.pcre.org/current/doc/html/pcre2api.html for more documentation on match syntax. 
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
#' @usage random_strings(N, string_size = 50, charset = "abcdefghijklmnopqrstuvwxyz", 
#'                       vector_mode = "stringfish")
#' @param N The number of strings to generate
#' @param string_size The length of the strings
#' @param charset The characters used to generate the random strings (default: abcdefghijklmnopqrstuvwxyz)
#' @param vector_mode The type of character vector to generate (either stringfish or normal, default: stringfish)
#' @return A character vector of the random strings
#' @details 
#' The function uses the PCRE2 library, which is also used internally by R. 
#' Note: the order of paramters is switched compared to the `gsub` base R function, with subject being first. 
#' See also: https://www.pcre.org/current/doc/html/pcre2api.html for more documentation on match syntax. 
#' @seealso gsub
#' @examples 
#' set.seed(1)
#' x <- random_strings(1e6, 80, "ACGT", vector_mode = "stringfish")
#' @name random_strings
NULL

# not yet implemented:
# sf_grep
# sf_writeLines
# sf_sprintf
# sf_subset
# sf_strsplit
# sf_reverse
# sf_encoding / sf_set_encoding
# sf_equal
# sf_match
# sf_table
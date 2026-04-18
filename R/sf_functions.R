sf_vector <- function(len) {
  sf_vector_create(len)
}

sf_vector_create <- function(len) {
  .Call(`_stringfish_sf_vector_create`, len)
}

slice_store_create <- function(len) {
  .Call(`_stringfish_slice_store_create`, len)
}

slice_store_create_with_size <- function(len, initial_slice_size) {
  if(!is.numeric(initial_slice_size) ||
     length(initial_slice_size) != 1L ||
     is.na(initial_slice_size) ||
     initial_slice_size < 0 ||
     initial_slice_size != floor(initial_slice_size)) {
    stop("initial_slice_size must be a non-negative whole number")
  }
  .Call(`_stringfish_slice_store_create_with_size`, len, initial_slice_size)
}

convert_to_sf_vector <- function(x, length.out = length(x)) {
  if(!is.character(x)) {
    stop("x must be a character vector")
  }
  x_len <- length(x)
  if(length(length.out) != 1L ||
     is.na(length.out) ||
     length.out < 0 ||
     length.out != floor(length.out)) {
    stop("length.out must be a non-negative whole number")
  }
  if(length.out > 0L && x_len < 1L) {
    stop("x must have length >= 1 when length.out is > 0")
  }
  .Call(`_stringfish_convert_to_sf_vector`, x, length.out)
}

convert_to_slice_store <- function(x, length.out = length(x)) {
  if(!is.character(x)) {
    stop("x must be a character vector")
  }
  x_len <- length(x)
  if(length(length.out) != 1L ||
     is.na(length.out) ||
     length.out < 0 ||
     length.out != floor(length.out)) {
    stop("length.out must be a non-negative whole number")
  }
  if(length.out > 0L && x_len < 1L) {
    stop("x must have length >= 1 when length.out is > 0")
  }
  .Call(`_stringfish_convert_to_slice_store`, x, length.out)
}

sf_paste <- function(..., sep="", nthreads = getOption("stringfish.nthreads", 1L)) {
  if(!is.character(sep) || length(sep) != 1) {
    stop("sep should be a character vector of length 1")
  }
  dots <- list(...)
  len <- -1
  for(i in seq_along(dots)) {
    if(!is.character(dots[[i]])) {
      dots[[i]] <- as.character(dots[[i]])
    }
    li <- length(dots[[i]])
    if(li == 0) stop("argument cannot be of length zero")
    if(li == 1) next
    if(len == -1) {
      len <- li
    } else {
      if(li != len) stop("All arguments should be the same length or length 1")
    }
  }
  c_sf_paste(dots, sep, nthreads)
}

sf_concat <- function(...) {
  dots <- list(...)
  for(i in seq_along(dots)) {
    if(!is.character(dots[[i]])) dots[[i]] <- as.character(dots[[i]])
  }
  c_sf_concat(dots)
}

sf_starts <- function(subject, pattern, ...) {
  pattern <- paste0("^", pattern)
  sf_grepl(subject, pattern, ...)
}

sf_ends <- function(subject, pattern, ...) {
  pattern <- paste0(pattern, "$")
  sf_grepl(subject, pattern, ...)
}

sf_trim <- function(subject, which = c("both", "left", "right"), whitespace = "[ \\t\\r\\n]", ...) {
  which <- match.arg(which)
  if(which == "both") {
    sf_gsub(sf_gsub(subject, paste0("^", whitespace,"+"), "", ...), paste0(whitespace, "+", "$"), "", ...)
  } else if(which == "left") {
    sf_gsub(subject, paste0("^", whitespace, "+"), "", ...)
  } else {
    sf_gsub(subject, paste0(whitespace, "+", "$"), "", ...)
  }
}

string_identical <- function(x, y, mode = c("semantic", "exact")) {
  stopifnot(is.character(x))
  stopifnot(is.character(y))
  mode <- match.arg(mode)
  if(length(x) != length(y)) return(FALSE)
  na_x <- is.na(x)
  na_y <- is.na(y)
  if(!identical(na_x, na_y)) return(FALSE)
  if(all(na_x)) return(TRUE) # correctly catches zero length as well
  x <- x[!na_x]
  y <- y[!na_y]

  if(mode == "exact") {
    if(any(nchar(x) != nchar(y))) return(FALSE)
    if(!all(Encoding(x) == Encoding(y))) return(FALSE)
    if(any(x != y)) return(FALSE)
    return(TRUE)
  }

  x_bytes <- Encoding(x) == "bytes"
  y_bytes <- Encoding(y) == "bytes"
  if(any(x_bytes | y_bytes)) {
    if(!identical(x_bytes, y_bytes)) return(FALSE)
    if(any(x_bytes)) {
      ok_bytes <- mapply(identical, as.list(x[x_bytes]), as.list(y[y_bytes]), USE.NAMES = FALSE)
      if(!all(ok_bytes)) return(FALSE)
    }
  }
  if(all(x_bytes)) return(TRUE)
  identical(unname(enc2utf8(x[!x_bytes])), unname(enc2utf8(y[!y_bytes])))
}

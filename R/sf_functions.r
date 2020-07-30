sf_paste <- function(..., sep="", nthreads=1) {
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

string_identical <- function(x, y) {
  stopifnot(is.character(x))
  stopifnot(is.character(y))
  if(length(x) != length(y)) return(F)
  na_x <- is.na(x)
  na_y <- is.na(y)
  stopifnot(identical(na_x,na_y))
  if(all(na_x)) return(T) # correctly catches zero length as well
  not_na <- !na_x
  if(any(nchar(x[not_na]) != nchar(y[not_na]))) return(F)
  if(!all(Encoding(x[not_na]) == Encoding(y[not_na]))) return(F)
  if(any(x[not_na] != y[not_na])) return(F)
  return(T)
}


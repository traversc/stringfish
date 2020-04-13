sf_paste <- function(..., sep="") {
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
  c_sf_paste(dots, sep)
}

inspect <- function(x) {
  .Internal(inspect(x))
}
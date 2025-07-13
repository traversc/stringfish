.onAttach <- function(libname, pkgname) {
  # maybe we should check this at compile time somehow?
  if(identical(utils::localeToCharset()[1], "UTF-8")) set_is_utf8_locale()
}

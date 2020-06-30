.onAttach <- function(libname, pkgname) {
  # packageStartupMessage("")
  if(localeToCharset()[1] == "UTF-8") set_is_utf8_locale()
}

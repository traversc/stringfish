.onAttach <- function(libname, pkgname) {
  # Runtime locale state is only used by the explicit normalization layer.
  if(identical(utils::localeToCharset()[1], "UTF-8")) set_is_utf8_locale()
}

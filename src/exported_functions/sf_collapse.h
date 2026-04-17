SEXP sf_collapse(SEXP x, SEXP collapse) {
  RStringIndexer cr(collapse);
  if(cr.size() != 1) throw std::runtime_error("collapse should be a character vector of length 1");
  sfenc::string_ref collapse_ref(cr.getCharLenCE(0));
  if(collapse_ref.raw_bytes == nullptr) {
    SEXP ret = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(ret, 0, NA_STRING);
    UNPROTECT(1);
    return ret;
  }

  RStringIndexer xr(x);
  size_t len = xr.size();
  std::string temp;
  sfenc::iconv_text_normalizer norm;
  sfenc::rstring_info collapse_text;
  bool byte_forced = collapse_ref.enc == cetype_t_ext::CE_BYTES;
  if(!byte_forced) {
    if(!collapse_ref.get_rstring_info(norm, collapse_text)) {
      SEXP ret = PROTECT(Rf_allocVector(STRSXP, 1));
      SET_STRING_ELT(ret, 0, NA_STRING);
      UNPROTECT(1);
      sfenc::warn_failed_normalization("sf_collapse", norm.failures);
      return ret;
    }
  }
  bool all_ascii = !byte_forced && collapse_text.enc == cetype_t_ext::CE_ASCII;
  std::string utf8_owned;
  for(size_t i=0; i<len; ++i) {
    auto q = xr.getCharLenCE(i);
    if(q.ptr == nullptr) {
      SEXP ret = PROTECT(Rf_allocVector(STRSXP, 1));
      SET_STRING_ELT(ret, 0, NA_STRING);
      UNPROTECT(1);
      sfenc::warn_failed_normalization("sf_collapse", norm.failures);
      return ret;
    }
    if(q.enc == cetype_t_ext::CE_BYTES) {
      byte_forced = true;
    }
  }
  for(size_t i=0; i<len; ++i) {
    auto q = xr.getCharLenCE(i);
    if(byte_forced) {
      temp.append(q.ptr, q.len);
      if(i < (len-1)) temp.append(collapse_ref.raw_bytes, collapse_ref.raw_len);
      continue;
    }
    sfenc::rstring_info view = q;
    utf8_owned.clear();
    if(!norm.normalize(view, utf8_owned)) {
      SEXP ret = PROTECT(Rf_allocVector(STRSXP, 1));
      SET_STRING_ELT(ret, 0, NA_STRING);
      UNPROTECT(1);
      sfenc::warn_failed_normalization("sf_collapse", norm.failures);
      return ret;
    }
    temp.append(view.ptr, view.len);
    all_ascii = all_ascii && (view.enc == cetype_t_ext::CE_ASCII);
    if(i < (len-1)) temp.append(collapse_text.ptr, collapse_text.len);
  }
  SEXP ret = PROTECT(Rf_allocVector(STRSXP, 1));
  if(byte_forced) {
    if(!check_r_string_len(temp.size())) {
      throw std::runtime_error("string size exceeds R string size");
    }
    SET_STRING_ELT(ret, 0, Rf_mkCharLenCE(temp.c_str(), static_cast<int>(temp.size()), CE_BYTES));
  } else {
    if(!check_r_string_len(temp.size())) {
      throw std::runtime_error("string size exceeds R string size");
    }
    SET_STRING_ELT(ret, 0, Rf_mkCharLenCE(temp.c_str(), static_cast<int>(temp.size()), all_ascii ? CE_NATIVE : CE_UTF8));
  }
  sfenc::warn_failed_normalization("sf_collapse", norm.failures);
  UNPROTECT(1);
  return ret;
}

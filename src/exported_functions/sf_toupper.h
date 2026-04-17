SEXP sf_toupper(SEXP x) {
  RStringIndexer xr(x);
  size_t len = xr.size();
  SEXP ret = PROTECT(slice_store_create(len));
  slice_store_data & ref = slice_store_data_ref(ret);
  std::string temp;
  sfenc::iconv_text_normalizer norm;
  std::string utf8_owned;
  for(size_t i=0; i<len; ++i) {
    auto q = xr.getCharLenCE(i);
    if(q.ptr == nullptr) {
      slice_store_write_na(ref, i);
      continue;
    }
    sfenc::rstring_info view = q;
    if(q.enc != cetype_t_ext::CE_BYTES) {
      utf8_owned.clear();
      if(!norm.normalize(view, utf8_owned)) {
        slice_store_write_na(ref, i);
        continue;
      }
    }
    temp.resize(view.len);
    for(int j=0; j<view.len; ++j) {
      if((view.ptr[j] >= 97) && (view.ptr[j] <= 122)) {
        temp[j] = view.ptr[j] - 32;
      } else {
        temp[j] = view.ptr[j];
      }
    }
    if(!check_r_string_len(temp.size())) {
      throw std::runtime_error("string size exceeds R string size");
    }
    slice_store_write(
      ref, i, temp,
      view.enc == cetype_t_ext::CE_BYTES ? cetype_t_ext::CE_BYTES :
        (checkAscii(temp.data(), temp.size()) ? cetype_t_ext::CE_ASCII : cetype_t_ext::CE_UTF8)
    );
  }
  sfenc::warn_failed_normalization("sf_toupper", norm.failures);
  UNPROTECT(1);
  return ret;
}

SEXP sf_readLines(const std::string file, const std::string encoding) {
  SEXP ret = PROTECT(slice_store_create(0));
  slice_store_data & ref = slice_store_data_ref(ret);
  cetype_t enc;
  if(encoding == "UTF-8") {
    enc = CE_UTF8;
  } else if(encoding == "latin1") {
    enc = CE_LATIN1;
  } else if(encoding == "bytes") {
    enc = CE_BYTES;
  } else {
    enc = CE_NATIVE;
  }
  std::ifstream myFile(R_ExpandFileName(file.c_str()), std::ios::in);
  if(!myFile) {
    throw std::runtime_error("Failed to open " + file + ". Check file path.");
  }

  std::string str;
  sfenc::iconv_text_normalizer norm;
  std::string utf8_owned;
  while(std::getline(myFile, str)) {
    if(str.size() > 0 && str.back() == '\r') {
      str.resize(str.size() - 1);
    }
    if(!check_r_string_len(str.size())) {
      throw std::runtime_error("string size exceeds R string size");
    }
    if(enc == CE_BYTES) {
      ref.emplace_back(str.data(), static_cast<int>(str.size()), cetype_t_ext::CE_BYTES);
    } else {
      sfenc::rstring_info view(str.data(), static_cast<int>(str.size()), static_cast<cetype_t_ext>(enc));
      utf8_owned.clear();
      if(!norm.normalize(view, utf8_owned)) {
        ref.emplace_back(NA_STRING);
      } else {
        ref.emplace_back(view.ptr, view.len, view.enc);
      }
    }
  }
  sfenc::warn_failed_normalization("sf_readLines", norm.failures);
  UNPROTECT(1);
  return ret;
}

void sf_writeLines(SEXP text, const std::string file, const std::string sep, const std::string na_value, const std::string encode_mode) {
  if((encode_mode != "UTF-8") && (encode_mode != "byte")) {
    throw std::runtime_error("encode_mode must be byte or UTF-8");
  }
  std::ofstream myFile(R_ExpandFileName(file.c_str()), std::ios::out | std::ios::binary);
  if(!myFile) {
    throw std::runtime_error("Failed to open " + file + ". Check file path.");
  }

  RStringIndexer xr(text);
  size_t len = xr.size();
  sfenc::iconv_text_normalizer norm;
  std::string utf8_owned;

  for(size_t i=0; i<len; ++i) {
    auto q = xr.getCharLenCE(i);
    if(q.ptr == nullptr) {
      myFile.write(na_value.c_str(), na_value.size());
    } else {
      if(encode_mode == "byte") {
        myFile.write(q.ptr, q.len);
      } else {
        if(q.enc == cetype_t_ext::CE_BYTES) {
          myFile.write(q.ptr, q.len);
        } else {
          sfenc::rstring_info view = q;
          utf8_owned.clear();
          if(!norm.normalize(view, utf8_owned)) {
            myFile.write(na_value.c_str(), na_value.size());
          } else {
            myFile.write(view.ptr, view.len);
          }
        }
      }
    }
    myFile.write(sep.c_str(), sep.size());
  }
  if(encode_mode == "UTF-8") {
    sfenc::warn_failed_normalization("sf_writeLines", norm.failures);
  }
}

SEXP random_strings(size_t N, IntegerVector string_size, std::string charset, std::string vector_mode) {
  if(N > static_cast<size_t>(std::numeric_limits<R_xlen_t>::max())) {
    throw std::runtime_error("N exceeds R_xlen_t");
  }
  const R_xlen_t n = static_cast<R_xlen_t>(N);
  const R_xlen_t string_size_len = Rf_xlength(string_size);
  if(string_size_len != 1 && string_size_len != n) {
    throw std::runtime_error("string_size must have length 1 or length N");
  }
  if(charset.size() > static_cast<size_t>(std::numeric_limits<int>::max())) {
    throw std::runtime_error("charset is too large");
  }

  auto get_string_size = [&](R_xlen_t i) {
    const int size_i = string_size[string_size_len == 1 ? 0 : i];
    if(size_i == NA_INTEGER) {
      throw std::runtime_error("string_size must not contain NA");
    }
    if(size_i < 0) {
      throw std::runtime_error("string_size must contain non-negative values");
    }
    return size_i;
  };

  if(charset.empty()) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(get_string_size(i) > 0) {
        throw std::runtime_error("charset must not be empty when string_size > 0");
      }
    }
  }

  std::string str;
  const int charset_len = static_cast<int>(charset.size());
  const bool ascii_output = charset.empty() || checkAscii(charset.data(), charset.size());
  if(vector_mode == "stringfish") {
    SEXP ret = PROTECT(slice_store_create(N));
    slice_store_data & ref = slice_store_data_ref(ret);
    for(size_t i = 0; i < N; ++i) {
      const int size_i = get_string_size(static_cast<R_xlen_t>(i));
      str.resize(static_cast<size_t>(size_i));
      if(size_i > 0) {
        std::vector<int> r = Rcpp::as< std::vector<int> >(
          Rcpp::sample(charset_len, size_i, true, R_NilValue, false)
        );
        for(int j = 0; j < size_i; ++j) {
          str[static_cast<size_t>(j)] = charset[static_cast<size_t>(r[j])];
        }
      }
      slice_store_write(
        ref, i, str.data(), static_cast<size_t>(size_i),
        ascii_output ? cetype_t_ext::CE_ASCII : cetype_t_ext::CE_BYTES
      );
    }
    UNPROTECT(1);
    return ret;
  }
  if(vector_mode == "normal") {
    CharacterVector ret(n);
    for(size_t i = 0; i < N; ++i) {
      const int size_i = get_string_size(static_cast<R_xlen_t>(i));
      str.resize(static_cast<size_t>(size_i));
      if(size_i > 0) {
        std::vector<int> r = Rcpp::as< std::vector<int> >(
          Rcpp::sample(charset_len, size_i, true, R_NilValue, false)
        );
        for(int j = 0; j < size_i; ++j) {
          str[static_cast<size_t>(j)] = charset[static_cast<size_t>(r[j])];
        }
      }
      SET_STRING_ELT(
        ret, static_cast<R_xlen_t>(i),
        Rf_mkCharLenCE(str.data(), size_i, ascii_output ? CE_NATIVE : CE_BYTES)
      );
    }
    return ret;
  }
  throw std::runtime_error("String vector_mode must be 'normal' or 'stringfish'");
}

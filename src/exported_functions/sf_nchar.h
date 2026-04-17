namespace sfexport {
inline int sf_nchar_value(const sfenc::rstring_info & q, bool chars_mode,
                          sfenc::iconv_text_normalizer & norm, std::string & utf8_owned) {
  if(q.ptr == nullptr) {
    return NA_INTEGER;
  }
  if(!chars_mode || q.enc == cetype_t_ext::CE_BYTES) {
    return q.len;
  }
  sfenc::rstring_info view = q;
  utf8_owned.clear();
  if(!norm.normalize(view, utf8_owned)) {
    return NA_INTEGER;
  }
  return view.enc == cetype_t_ext::CE_UTF8 ? code_points(view.ptr, view.len) : view.len;
}

#if RCPP_PARALLEL_USE_TBB
struct sf_nchar_worker : public RcppParallel::Worker {
  RStringIndexer * rsi;
  int * output;
  bool chars_mode;
  std::atomic<size_t> * failures;

  sf_nchar_worker(RStringIndexer * rsi, int * output, bool chars_mode, std::atomic<size_t> * failures) :
    rsi(rsi), output(output), chars_mode(chars_mode), failures(failures) {}

  void operator()(std::size_t begin, std::size_t end) {
    sfenc::iconv_text_normalizer norm;
    std::string utf8_owned;
    for(size_t i = begin; i < end; ++i) {
      output[i] = sf_nchar_value(rsi->getCharLenCE(i), chars_mode, norm, utf8_owned);
    }
    if(chars_mode) {
      failures->fetch_add(norm.failures);
    }
  }
};
#endif
}

IntegerVector sf_nchar(SEXP x, const std::string type, const int nthreads) {
  if((type != "chars") && (type != "bytes")) {
    throw std::runtime_error("type must be chars or bytes");
  }
  RStringIndexer rsi(x);
  size_t len = rsi.size();
  IntegerVector ret(len);
  int * optr = INTEGER(ret);

  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    std::atomic<size_t> failures(0);
    sfexport::sf_nchar_worker w(&rsi, optr, type == "chars", &failures);
    parallelFor(0, len, w, 100, nthreads);
    if(type == "chars") {
      sfenc::warn_failed_normalization("sf_nchar", failures.load());
    }
#else
    throw std::runtime_error("RcppParallel TBB not supported");
#endif
  } else {
    sfenc::iconv_text_normalizer norm;
    std::string utf8_owned;
    const bool chars_mode = type == "chars";
    for(size_t i = 0; i < len; ++i) {
      optr[i] = sfexport::sf_nchar_value(rsi.getCharLenCE(i), chars_mode, norm, utf8_owned);
    }
    if(chars_mode) {
      sfenc::warn_failed_normalization("sf_nchar", norm.failures);
    }
  }
  return ret;
}

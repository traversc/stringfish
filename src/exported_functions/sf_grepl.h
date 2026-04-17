namespace sfexport {
inline int sf_grepl_value(const std::string & encode_mode, bool pattern_is_na, bool has_text_pm, cetype_t_ext pattern_enc,
                          sf::pcre2_match_wrapper & byte_pm, sf::pcre2_match_wrapper & text_pm,
                          const sfenc::rstring_info & q,
                          sfenc::iconv_text_normalizer & norm, std::string & utf8_owned) {
  if(pattern_is_na || q.ptr == nullptr) {
    return NA_LOGICAL;
  }
  const bool byte_mode = (encode_mode == "byte") ||
    (q.enc == cetype_t_ext::CE_BYTES) ||
    (pattern_enc == cetype_t_ext::CE_BYTES);
  if(byte_mode) {
    return byte_pm.match(q.ptr, q.len);
  }
  if(!has_text_pm) {
    return NA_LOGICAL;
  }
  sfenc::rstring_info view = q;
  utf8_owned.clear();
  if(!norm.normalize(view, utf8_owned)) {
    return NA_LOGICAL;
  }
  return text_pm.match(view.ptr, view.len);
}

#if RCPP_PARALLEL_USE_TBB
struct sf_grepl_worker : public RcppParallel::Worker {
  const std::string encode_mode;
  const bool pattern_is_na;
  const bool has_text_pm;
  const cetype_t_ext pattern_enc;
  tbb::enumerable_thread_specific<sf::pcre2_match_wrapper> byte_pm;
  tbb::enumerable_thread_specific<sf::pcre2_match_wrapper> text_pm;
  RStringIndexer * cr;
  int * output;
  std::atomic<size_t> * failures;

  sf_grepl_worker(const std::string & encode_mode, bool pattern_is_na, bool has_text_pm, cetype_t_ext pattern_enc,
                  const sf::pcre2_match_wrapper & byte_pm, const sf::pcre2_match_wrapper & text_pm,
                  RStringIndexer * cr, int * output, std::atomic<size_t> * failures) :
    encode_mode(encode_mode), pattern_is_na(pattern_is_na), has_text_pm(has_text_pm), pattern_enc(pattern_enc),
    byte_pm(byte_pm), text_pm(text_pm), cr(cr), output(output), failures(failures) {}

  void operator()(std::size_t begin, std::size_t end) {
    auto & byte_pm_local = byte_pm.local();
    auto & text_pm_local = text_pm.local();
    sfenc::iconv_text_normalizer norm;
    std::string utf8_owned;
    for(size_t i = begin; i < end; ++i) {
      output[i] = sf_grepl_value(
        encode_mode, pattern_is_na, has_text_pm, pattern_enc,
        byte_pm_local, text_pm_local, cr->getCharLenCE(i), norm, utf8_owned
      );
    }
    failures->fetch_add(norm.failures);
  }
};
#endif
}

LogicalVector sf_grepl(SEXP subject, SEXP pattern, const std::string encode_mode, const bool fixed, const int nthreads) {
  if(encode_mode != "auto" && encode_mode != "byte" && encode_mode != "UTF-8") {
    throw std::runtime_error("encode_mode must be auto, byte or UTF-8");
  }
  if(TYPEOF(pattern) != STRSXP || Rf_xlength(pattern) != 1) {
    throw std::runtime_error("pattern should be a character vector of length 1");
  }
  RStringIndexer pattern_indexer(pattern);
  sfenc::string_ref pattern_ref(pattern_indexer.getCharLenCE(0));
  const bool pattern_is_na = pattern_ref.raw_bytes == nullptr;
  sf::pcre2_match_wrapper byte_pm;
  sf::pcre2_match_wrapper text_pm;
  bool has_text_pm = false;
  sfenc::iconv_text_normalizer pattern_norm;
  if(!pattern_is_na) {
    byte_pm = sf::pcre2_match_wrapper(pattern_ref.raw_bytes, pattern_ref.raw_len, false, fixed);
    if(encode_mode != "byte" && pattern_ref.enc != cetype_t_ext::CE_BYTES) {
      sfenc::rstring_info pattern_text;
      if(pattern_ref.get_rstring_info(pattern_norm, pattern_text)) {
        text_pm = sf::pcre2_match_wrapper(pattern_text.ptr, pattern_text.len, true, fixed);
        has_text_pm = true;
      }
    }
  }

  RStringIndexer cr(subject);
  size_t len = cr.size();
  LogicalVector ret(len);
  int * outptr = LOGICAL(ret);

  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    std::atomic<size_t> failures(0);
    sfexport::sf_grepl_worker w(encode_mode, pattern_is_na, has_text_pm, pattern_ref.enc, byte_pm, text_pm, &cr, outptr, &failures);
    parallelFor(0, len, w, 100, nthreads);
    sfenc::warn_failed_normalization("sf_grepl", pattern_norm.failures + failures.load());
#else
    throw std::runtime_error("RcppParallel TBB not supported");
#endif
  } else {
    sfenc::iconv_text_normalizer norm;
    std::string utf8_owned;
    for(size_t i=0; i<len; ++i) {
      outptr[i] = sfexport::sf_grepl_value(
        encode_mode, pattern_is_na, has_text_pm, pattern_ref.enc,
        byte_pm, text_pm, cr.getCharLenCE(i), norm, utf8_owned
      );
    }
    sfenc::warn_failed_normalization("sf_grepl", pattern_norm.failures + norm.failures);
  }
  return ret;
}

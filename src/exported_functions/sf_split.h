namespace sfexport {
inline size_t sf_split_next_offset(const char * ptr, size_t remaining, cetype_t_ext enc) {
  if(remaining == 0) {
    return 0;
  }
  if(enc != cetype_t_ext::CE_UTF8) {
    return 1;
  }
  const unsigned char lead = static_cast<unsigned char>(ptr[0]);
  if((lead & 0x80U) == 0) {
    return 1;
  }
  if((lead & 0xE0U) == 0xC0U && remaining >= 2) {
    return 2;
  }
  if((lead & 0xF0U) == 0xE0U && remaining >= 3) {
    return 3;
  }
  if((lead & 0xF8U) == 0xF0U && remaining >= 4) {
    return 4;
  }
  return 1;
}

inline void sf_split_empty_pattern(slice_store_data & ref, const char * sptr, int len, cetype_t_ext enc) {
  if(len == 0) {
    ref.append_trusted(sptr, 0, enc);
    return;
  }
  size_t offset = 0;
  while(offset < static_cast<size_t>(len)) {
    const size_t chunk_len = sf_split_next_offset(sptr + offset, static_cast<size_t>(len) - offset, enc);
    const cetype_t_ext chunk_enc =
      enc == cetype_t_ext::CE_BYTES ? cetype_t_ext::CE_BYTES :
      (checkAscii(sptr + offset, chunk_len) ? cetype_t_ext::CE_ASCII : cetype_t_ext::CE_UTF8);
    ref.append_trusted(sptr + offset, chunk_len, chunk_enc);
    offset += chunk_len;
  }
}

inline void sf_split_append_matches(slice_store_data & ref, sf::pcre2_match_wrapper & p,
                                    const char * sptr, int len, cetype_t_ext enc) {
  if(len == 0) {
    ref.append_trusted(sptr, 0, enc);
    return;
  }
  size_t chunk_start = 0;
  size_t search_offset = 0;
  while(search_offset <= static_cast<size_t>(len)) {
    size_t begin = 0;
    size_t end = 0;
    if(!p.match_get_interval_from(sptr, len, search_offset, 0, begin, end)) {
      break;
    }
    ref.append_trusted(sptr + chunk_start, begin - chunk_start, enc);
    if(begin != end) {
      chunk_start = end;
      search_offset = end;
      continue;
    }
    chunk_start = begin;
    if(begin == static_cast<size_t>(len)) {
      break;
    }
    search_offset = begin + sf_split_next_offset(
      sptr + begin,
      static_cast<size_t>(len) - begin,
      enc
    );
  }
  ref.append_trusted(sptr + chunk_start, static_cast<size_t>(len) - chunk_start, enc);
}

inline void sf_split_one(slice_store_data & ref, const std::string & encode_mode, bool pattern_is_na,
                         bool pattern_is_empty, bool has_text_pm, cetype_t_ext pattern_enc,
                         sf::pcre2_match_wrapper & byte_pm, sf::pcre2_match_wrapper & text_pm,
                         const sfenc::rstring_info & q,
                         sfenc::iconv_text_normalizer & norm, std::string & utf8_owned) {
  if(pattern_is_na || q.ptr == nullptr) {
    ref.emplace_back(NA_STRING);
    return;
  }
  const bool byte_mode = (encode_mode == "byte") ||
    (q.enc == cetype_t_ext::CE_BYTES) ||
    (pattern_enc == cetype_t_ext::CE_BYTES);
  if(byte_mode) {
    if(pattern_is_empty) {
      sf_split_empty_pattern(ref, q.ptr, q.len, cetype_t_ext::CE_BYTES);
      return;
    }
    sf_split_append_matches(ref, byte_pm, q.ptr, q.len, cetype_t_ext::CE_BYTES);
    return;
  }
  if(!has_text_pm) {
    ref.emplace_back(NA_STRING);
    return;
  }
  sfenc::rstring_info view = q;
  utf8_owned.clear();
  if(!norm.normalize(view, utf8_owned)) {
    ref.emplace_back(NA_STRING);
    return;
  }
  if(pattern_is_empty) {
    sf_split_empty_pattern(
      ref, view.ptr, view.len,
      view.enc == cetype_t_ext::CE_ASCII ? cetype_t_ext::CE_ASCII : cetype_t_ext::CE_UTF8
    );
    return;
  }
  sf_split_append_matches(
    ref, text_pm, view.ptr, view.len,
    view.enc == cetype_t_ext::CE_ASCII ? cetype_t_ext::CE_ASCII : cetype_t_ext::CE_UTF8
  );
}

#if RCPP_PARALLEL_USE_TBB
struct sf_split_worker : public RcppParallel::Worker {
  const std::string encode_mode;
  const bool pattern_is_na;
  const bool pattern_is_empty;
  const bool has_text_pm;
  const cetype_t_ext pattern_enc;
  tbb::enumerable_thread_specific<sf::pcre2_match_wrapper> byte_pm;
  tbb::enumerable_thread_specific<sf::pcre2_match_wrapper> text_pm;
  std::vector<slice_store_data*> refs;
  RStringIndexer * cr;
  std::atomic<size_t> * failures;

  sf_split_worker(const std::string & encode_mode, bool pattern_is_na, bool pattern_is_empty,
                  bool has_text_pm, cetype_t_ext pattern_enc,
                  const sf::pcre2_match_wrapper & byte_pm, const sf::pcre2_match_wrapper & text_pm,
                  std::vector<slice_store_data*> refs, RStringIndexer * cr, std::atomic<size_t> * failures) :
    encode_mode(encode_mode), pattern_is_na(pattern_is_na), pattern_is_empty(pattern_is_empty),
    has_text_pm(has_text_pm), pattern_enc(pattern_enc),
    byte_pm(byte_pm), text_pm(text_pm), refs(std::move(refs)), cr(cr), failures(failures) {}

  void operator()(std::size_t begin, std::size_t end) {
    auto & byte_pm_local = byte_pm.local();
    auto & text_pm_local = text_pm.local();
    sfenc::iconv_text_normalizer norm;
    std::string utf8_owned;
    for(size_t i = begin; i < end; ++i) {
      sf_split_one(
        *refs[i], encode_mode, pattern_is_na, pattern_is_empty, has_text_pm, pattern_enc,
        byte_pm_local, text_pm_local, cr->getCharLenCE(i), norm, utf8_owned
      );
    }
    failures->fetch_add(norm.failures);
  }
};
#endif
}

SEXP sf_split(SEXP subject, SEXP split, const std::string encode_mode, const bool fixed, const int nthreads) {
  if(encode_mode != "auto" && encode_mode != "byte" && encode_mode != "UTF-8") {
    throw std::runtime_error("encode_mode must be auto, byte or UTF-8");
  }
  if(TYPEOF(split) != STRSXP || Rf_xlength(split) != 1) {
    throw std::runtime_error("split should be a character vector of length 1");
  }
  RStringIndexer pattern_indexer(split);
  sfenc::string_ref pattern_ref(pattern_indexer.getCharLenCE(0));
  const bool pattern_is_na = pattern_ref.raw_bytes == nullptr;
  const bool pattern_is_empty = !pattern_is_na && pattern_ref.raw_len == 0;
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
  SEXP ret = PROTECT(Rf_allocVector(VECSXP, len));

  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    std::vector<slice_store_data*> refs(len);
    for(size_t i=0; i<len; ++i) {
      SEXP svec = PROTECT(slice_store_create(0));
      SET_VECTOR_ELT(ret, i, svec);
      UNPROTECT(1);
      refs[i] = &slice_store_data_ref(svec);
    }
    std::atomic<size_t> failures(0);
    sfexport::sf_split_worker w(
      encode_mode, pattern_is_na, pattern_is_empty, has_text_pm, pattern_ref.enc,
      byte_pm, text_pm, std::move(refs), &cr, &failures
    );
    parallelFor(0, len, w, 100, nthreads);
    sfenc::warn_failed_normalization("sf_split", pattern_norm.failures + failures.load());
#else
    throw std::runtime_error("RcppParallel TBB not supported");
#endif
  } else {
    sfenc::iconv_text_normalizer norm;
    std::string utf8_owned;
    for(size_t i=0; i<len; ++i) {
      SEXP svec = PROTECT(slice_store_create(0));
      SET_VECTOR_ELT(ret, i, svec);
      UNPROTECT(1);
      auto & ref = slice_store_data_ref(svec);
      sfexport::sf_split_one(
        ref, encode_mode, pattern_is_na, pattern_is_empty, has_text_pm, pattern_ref.enc,
        byte_pm, text_pm, cr.getCharLenCE(i), norm, utf8_owned
      );
    }
    sfenc::warn_failed_normalization("sf_split", pattern_norm.failures + norm.failures);
  }
  UNPROTECT(1);
  return ret;
}

namespace sfexport {
template <typename Sink>
inline void sf_gsub_write_one(Sink & sink, size_t i, const std::string & encode_mode,
                              bool pattern_is_na, bool replacement_is_na, bool has_text_ps,
                              cetype_t_ext pattern_enc, cetype_t_ext replacement_enc,
                              sf::pcre2_sub_wrapper & byte_ps, sf::pcre2_sub_wrapper & text_ps,
                              const sfenc::rstring_info & q,
                              sfenc::iconv_text_normalizer & norm, std::string & utf8_owned) {
  if(pattern_is_na || replacement_is_na || q.ptr == nullptr) {
    slice_store_write_na(sink, i);
    return;
  }
  const bool byte_mode = (encode_mode == "byte") ||
    (q.enc == cetype_t_ext::CE_BYTES) ||
    (pattern_enc == cetype_t_ext::CE_BYTES) ||
    (replacement_enc == cetype_t_ext::CE_BYTES);
  if(byte_mode) {
    auto result = byte_ps.gsub(q.ptr, q.len);
    slice_store_write(sink, i, result.ptr, static_cast<size_t>(result.len), cetype_t_ext::CE_BYTES);
    return;
  }
  if(!has_text_ps) {
    slice_store_write_na(sink, i);
    return;
  }
  sfenc::rstring_info view = q;
  utf8_owned.clear();
  if(!norm.normalize(view, utf8_owned)) {
    slice_store_write_na(sink, i);
    return;
  }
  auto result = text_ps.gsub(view.ptr, view.len);
  slice_store_write(sink, i, result.ptr, static_cast<size_t>(result.len), cetype_t_ext::CE_UTF8);
}

#if RCPP_PARALLEL_USE_TBB
struct sf_gsub_worker : public RcppParallel::Worker {
  const std::string encode_mode;
  const bool pattern_is_na;
  const bool replacement_is_na;
  const bool has_text_ps;
  const cetype_t_ext pattern_enc;
  const cetype_t_ext replacement_enc;
  tbb::enumerable_thread_specific<sf::pcre2_sub_wrapper> byte_ps;
  tbb::enumerable_thread_specific<sf::pcre2_sub_wrapper> text_ps;
  RStringIndexer * cr;
  tbb::enumerable_thread_specific<slice_store_shard> * shards;
  std::atomic<size_t> * failures;

  sf_gsub_worker(const std::string & encode_mode, bool pattern_is_na, bool replacement_is_na, bool has_text_ps,
                 cetype_t_ext pattern_enc, cetype_t_ext replacement_enc,
                 const sf::pcre2_sub_wrapper & byte_ps, const sf::pcre2_sub_wrapper & text_ps,
                 RStringIndexer * cr, tbb::enumerable_thread_specific<slice_store_shard> * shards,
                 std::atomic<size_t> * failures) :
    encode_mode(encode_mode), pattern_is_na(pattern_is_na), replacement_is_na(replacement_is_na),
    has_text_ps(has_text_ps), pattern_enc(pattern_enc), replacement_enc(replacement_enc),
    byte_ps(byte_ps), text_ps(text_ps), cr(cr), shards(shards), failures(failures) {}

  void operator()(std::size_t begin, std::size_t end) {
    auto & byte_ps_local = byte_ps.local();
    auto & text_ps_local = text_ps.local();
    sfenc::iconv_text_normalizer norm;
    std::string utf8_owned;
    auto & shard = shards->local();
    for(size_t i = begin; i < end; ++i) {
      sf_gsub_write_one(
        shard, i, encode_mode, pattern_is_na, replacement_is_na, has_text_ps,
        pattern_enc, replacement_enc, byte_ps_local, text_ps_local, cr->getCharLenCE(i), norm, utf8_owned
      );
    }
    failures->fetch_add(norm.failures);
  }
};
#endif
}

SEXP sf_gsub(SEXP subject, SEXP pattern, SEXP replacement, const std::string encode_mode, const bool fixed, const int nthreads) {
  if(encode_mode != "auto" && encode_mode != "byte" && encode_mode != "UTF-8") {
    throw std::runtime_error("encode_mode must be auto, byte or UTF-8");
  }
  if(TYPEOF(pattern) != STRSXP || Rf_xlength(pattern) != 1) {
    throw std::runtime_error("pattern should be a character vector of length 1");
  }
  if(TYPEOF(replacement) != STRSXP || Rf_xlength(replacement) != 1) {
    throw std::runtime_error("replacement should be a character vector of length 1");
  }
  RStringIndexer pattern_indexer(pattern);
  RStringIndexer replacement_indexer(replacement);
  sfenc::string_ref pattern_ref(pattern_indexer.getCharLenCE(0));
  sfenc::string_ref replacement_ref(replacement_indexer.getCharLenCE(0));
  const bool pattern_is_na = pattern_ref.raw_bytes == nullptr;
  const bool replacement_is_na = replacement_ref.raw_bytes == nullptr;
  sf::pcre2_sub_wrapper byte_ps;
  sf::pcre2_sub_wrapper text_ps;
  bool has_text_ps = false;
  sfenc::iconv_text_normalizer scalar_norm;
  if(!pattern_is_na && !replacement_is_na) {
    byte_ps = sf::pcre2_sub_wrapper(
      pattern_ref.raw_bytes, pattern_ref.raw_len, replacement_ref.raw_bytes, replacement_ref.raw_len, false, fixed
    );
    if(encode_mode != "byte" &&
       pattern_ref.enc != cetype_t_ext::CE_BYTES &&
       replacement_ref.enc != cetype_t_ext::CE_BYTES) {
      sfenc::rstring_info pattern_text;
      sfenc::rstring_info replacement_text;
      if(pattern_ref.get_rstring_info(scalar_norm, pattern_text) &&
         replacement_ref.get_rstring_info(scalar_norm, replacement_text)) {
        text_ps = sf::pcre2_sub_wrapper(
          pattern_text.ptr, pattern_text.len, replacement_text.ptr, replacement_text.len, true, fixed
        );
        has_text_ps = true;
      }
    }
  }

  RStringIndexer cr(subject);
  size_t len = cr.size();
  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    std::vector<string_record> records(len);
    tbb::enumerable_thread_specific<slice_store_shard> shards([&records] {
      return slice_store_shard(records);
    });
    std::atomic<size_t> failures(0);
    sfexport::sf_gsub_worker w(encode_mode, pattern_is_na, replacement_is_na, has_text_ps,
                               pattern_ref.enc, replacement_ref.enc, byte_ps, text_ps, &cr, &shards, &failures);
    parallelFor(0, len, w, 100, nthreads);
    sfenc::warn_failed_normalization("sf_gsub", scalar_norm.failures + failures.load());
    SEXP ret = PROTECT(slice_st::Make(new slice_store_data(std::move(records), shards), true));
    UNPROTECT(1);
    return ret;
#else
    throw std::runtime_error("RcppParallel TBB not supported");
#endif
  } else {
    SEXP ret = PROTECT(slice_store_create(len));
    slice_store_data & ref = slice_store_data_ref(ret);
    sfenc::iconv_text_normalizer norm;
    std::string utf8_owned;
    for(size_t i=0; i<len; ++i) {
      sfexport::sf_gsub_write_one(
        ref, i, encode_mode, pattern_is_na, replacement_is_na, has_text_ps,
        pattern_ref.enc, replacement_ref.enc, byte_ps, text_ps, cr.getCharLenCE(i), norm, utf8_owned
      );
    }
    sfenc::warn_failed_normalization("sf_gsub", scalar_norm.failures + norm.failures);
    UNPROTECT(1);
    return ret;
  }
}

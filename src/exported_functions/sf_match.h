namespace sfexport {
struct byte_span {
  const char * ptr;
  int len;
  byte_span() : ptr(nullptr), len(0) {}
  byte_span(const char * ptr, int len) : ptr(ptr), len(len) {}
  bool operator==(const byte_span & other) const {
    if(ptr == nullptr || other.ptr == nullptr) {
      return ptr == other.ptr;
    }
    return len == other.len && std::memcmp(ptr, other.ptr, len) == 0;
  }
};

struct byte_span_hash {
  size_t operator()(const byte_span & s) const {
    if(s.ptr == nullptr) {
      return 0;
    }
    return XXH3_64bits(s.ptr, s.len);
  }
};

static constexpr int R_INT_MAX = 2147483647;
inline int sf_match_value(const std::unordered_map<byte_span, int, byte_span_hash> & raw_map,
                          const std::unordered_map<byte_span, int, byte_span_hash> & text_map,
                          const sfenc::rstring_info & q,
                          sfenc::iconv_text_normalizer & norm, std::string & utf8_owned) {
  if(q.ptr == nullptr) {
    return NA_INTEGER;
  }
  int best = R_INT_MAX;
  auto raw_it = raw_map.find(byte_span(q.ptr, q.len));
  if(raw_it != raw_map.end()) {
    best = std::min(best, raw_it->second);
  }
  if(q.enc != cetype_t_ext::CE_BYTES) {
    sfenc::rstring_info view = q;
    utf8_owned.clear();
    if(!norm.normalize(view, utf8_owned)) {
      return best == R_INT_MAX ? NA_INTEGER : (best + 1);
    }
    auto text_it = text_map.find(byte_span(view.ptr, view.len));
    if(text_it != text_map.end()) {
      best = std::min(best, text_it->second);
    }
  }
  return best == R_INT_MAX ? NA_INTEGER : (best + 1);
}

#if RCPP_PARALLEL_USE_TBB
struct sf_match_worker : public RcppParallel::Worker {
  const std::unordered_map<byte_span, int, byte_span_hash> * raw_map;
  const std::unordered_map<byte_span, int, byte_span_hash> * text_map;
  RStringIndexer * searchit;
  int * output;
  std::atomic<size_t> * failures;

  sf_match_worker(const std::unordered_map<byte_span, int, byte_span_hash> * raw_map,
                  const std::unordered_map<byte_span, int, byte_span_hash> * text_map,
                  RStringIndexer * searchit, int * output, std::atomic<size_t> * failures) :
    raw_map(raw_map), text_map(text_map), searchit(searchit), output(output), failures(failures) {}

  void operator()(std::size_t begin, std::size_t end) {
    sfenc::iconv_text_normalizer norm;
    std::string utf8_owned;
    for(size_t i = begin; i < end; ++i) {
      output[i] = sf_match_value(*raw_map, *text_map, searchit->getCharLenCE(i), norm, utf8_owned);
    }
    failures->fetch_add(norm.failures);
  }
};
#endif
}

IntegerVector sf_match(SEXP x, SEXP table, const int nthreads) {
  RStringIndexer cr(table);
  size_t len = cr.size();
  if(len > static_cast<size_t>(sfexport::R_INT_MAX)) throw std::runtime_error("long vectors not supported");
  
  RStringIndexer xr(x);
  size_t xlen = xr.size();
  IntegerVector ret(xlen);
  int * retp = INTEGER(ret);
  std::unordered_map<sfexport::byte_span, int, sfexport::byte_span_hash> raw_map;
  std::unordered_map<sfexport::byte_span, int, sfexport::byte_span_hash> text_map;
  std::vector<std::string> table_text_owned(len);
  sfenc::iconv_text_normalizer table_norm;
  for(size_t i=0; i<len; ++i) {
    auto q = cr.getCharLenCE(i);
    if(q.ptr == nullptr) {
      continue;
    }
    raw_map.emplace(sfexport::byte_span(q.ptr, q.len), static_cast<int>(i));
    if(q.enc != cetype_t_ext::CE_BYTES) {
      sfenc::rstring_info view = q;
      if(table_norm.normalize(view, table_text_owned[i])) {
        text_map.emplace(sfexport::byte_span(view.ptr, view.len), static_cast<int>(i));
      }
    }
  }
  
  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    std::atomic<size_t> failures(0);
    sfexport::sf_match_worker w(&raw_map, &text_map, &xr, retp, &failures);
    parallelFor(0, xlen, w, 100, nthreads);
    sfenc::warn_failed_normalization("sf_match", table_norm.failures + failures.load());
#else
    throw std::runtime_error("RcppParallel TBB not supported");
#endif
  } else {
    sfenc::iconv_text_normalizer query_norm;
    std::string utf8_owned;
    for(size_t i=0; i<xlen; ++i) {
      retp[i] = sfexport::sf_match_value(raw_map, text_map, xr.getCharLenCE(i), query_norm, utf8_owned);
    }
    sfenc::warn_failed_normalization("sf_match", table_norm.failures + query_norm.failures);
  }
  return ret;
}

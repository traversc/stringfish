namespace sfexport {
template <typename Sink>
inline void sf_iconv_write_one(Sink & sink, size_t i, RStringIndexer & rsi, iconv_wrapper & iw, cetype_t_ext encoding) {
  auto q = rsi.getCharLenCE(i);
  if(q.ptr == nullptr) {
    slice_store_write_na(sink, i);
    return;
  }
  auto sc = iw.convertToString(q.ptr, q.len);
  if(!sc.first) {
    slice_store_write_na(sink, i);
    return;
  }
  slice_store_write(sink, i, sc.second, encoding);
}

#if RCPP_PARALLEL_USE_TBB
struct sf_iconv_worker : public RcppParallel::Worker {
  tbb::enumerable_thread_specific<iconv_wrapper> iw;
  cetype_t_ext encoding;
  RStringIndexer * rsi;
  tbb::enumerable_thread_specific<slice_store_shard> * shards;

  sf_iconv_worker(iconv_wrapper iw, cetype_t_ext encoding, RStringIndexer * rsi,
                  tbb::enumerable_thread_specific<slice_store_shard> * shards) :
    iw(iw), encoding(encoding), rsi(rsi), shards(shards) {}

  void operator()(std::size_t begin, std::size_t end) {
    auto & iw_local = iw.local();
    auto & shard = shards->local();
    for(size_t i = begin; i < end; ++i) {
      sf_iconv_write_one(shard, i, *rsi, iw_local, encoding);
    }
  }
};
#endif
}

SEXP sf_iconv(SEXP x, const std::string from, const std::string to, int nthreads) {
  cetype_t encoding;
  if(to == iconv_utf8_string) {
    encoding = CE_UTF8;
  } else if(to == iconv_latin1_string) {
    encoding = CE_LATIN1;
  } else {
    encoding = CE_NATIVE;
  }
  iconv_wrapper iw(to.c_str(), from.c_str());
  RStringIndexer rsi(x);
  size_t len = rsi.size();
  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    std::vector<string_record> records(len);
    tbb::enumerable_thread_specific<slice_store_shard> shards([&records] {
      return slice_store_shard(records);
    });
    sfexport::sf_iconv_worker w(iw, static_cast<cetype_t_ext>(encoding), &rsi, &shards);
    parallelFor(0, len, w, 100, nthreads);
    SEXP ret = PROTECT(slice_st::Make(new slice_store_data(std::move(records), shards), true));
    UNPROTECT(1);
    return ret;
#else
    throw std::runtime_error("RcppParallel TBB not supported");
#endif
  } else {
    SEXP ret = PROTECT(slice_store_create(len));
    slice_store_data & ref = slice_store_data_ref(ret);
    for(size_t i=0; i<len; ++i) {
      sfexport::sf_iconv_write_one(ref, i, rsi, iw, static_cast<cetype_t_ext>(encoding));
    }
    UNPROTECT(1);
    return ret;
  }
}

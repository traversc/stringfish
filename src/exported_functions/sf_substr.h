namespace sfexport {
inline sfstring sf_substr_internal(const char * x, const int len, const cetype_t_ext type, int start, int stop) {
  if(x == nullptr) return sfstring(NA_STRING);
  if(len < 0) {
    throw std::runtime_error("string length must be >= 0");
  }
  auto make_output = [&](const char * ptr, int out_len) {
    return sfstring(ptr, out_len, type);
  };
  if(len == 0) return make_output("", 0);
  if(start > len) return make_output("", 0);

  if(type == cetype_t_ext::CE_UTF8 || type == cetype_t_ext::CE_ASCII_OR_UTF8) {
    int clen = code_points(x, len);
    if(start > clen) return make_output("", 0);
    if(start < 0) {
      start = clen + start;
    } else {
      start = start - 1;
    }
    if(stop < 0) {
      stop = clen + stop;
    } else {
      stop = stop - 1;
    }

    if(stop < start || stop < 0) return make_output("", 0);
    if(start < 0) start = 0;
    if(start >= clen) return make_output("", 0);
    if(stop >= clen) stop = clen - 1;

    int start_byte = -1;
    int end_byte = len;
    int code_point_index = 0;
    for(int byte_index = 0; byte_index < len; ++byte_index) {
      const unsigned char ch = static_cast<unsigned char>(x[byte_index]);
      if((ch & 0xc0) == 0x80) {
        continue;
      }
      if(code_point_index == start) {
        start_byte = byte_index;
      }
      if(code_point_index == stop + 1) {
        end_byte = byte_index;
        break;
      }
      ++code_point_index;
    }
    if(start_byte < 0 || end_byte < start_byte) {
      return make_output("", 0);
    }
    return make_output(x + start_byte, end_byte - start_byte);
  }

  start = start < 0 ? (len + start) : (start - 1);
  stop = stop < 0 ? (len + stop) : (stop - 1);
  if(stop < start) return make_output("", 0);
  if(stop >= len) stop = len - 1;
  if(stop < 0) return make_output("", 0);
  if(start < 0) start = 0;
  return make_output(x + start, stop - start + 1);
}

inline sfstring sf_substr_value(const sfenc::rstring_info & q, int start, int stop,
                                sfenc::iconv_text_normalizer & norm, std::string & utf8_owned) {
  if(q.ptr == nullptr) {
    return sfstring(NA_STRING);
  }
  if(q.enc == cetype_t_ext::CE_BYTES) {
    return sf_substr_internal(q.ptr, q.len, cetype_t_ext::CE_BYTES, start, stop);
  }
  sfenc::rstring_info view = q;
  utf8_owned.clear();
  if(!norm.normalize(view, utf8_owned)) {
    return sfstring(NA_STRING);
  }
  return sf_substr_internal(view.ptr, view.len, view.enc, start, stop);
}

template <typename Sink>
inline void sf_substr_write_one(Sink & sink, size_t i, RStringIndexer & rsi,
                                size_t start_size, size_t stop_size, int * start_ptr, int * stop_ptr,
                                sfenc::iconv_text_normalizer & norm, std::string & utf8_owned) {
  slice_store_write(
    sink, i,
    sf_substr_value(
      rsi.getCharLenCE(i),
      start_ptr[start_size == 1 ? 0 : i],
      stop_ptr[stop_size == 1 ? 0 : i],
      norm, utf8_owned
    )
  );
}

#if RCPP_PARALLEL_USE_TBB
struct sf_substr_worker : public RcppParallel::Worker {
  RStringIndexer * rsi;
  size_t start_size;
  size_t stop_size;
  int * start_ptr;
  int * stop_ptr;
  tbb::enumerable_thread_specific<slice_store_shard> * shards;
  std::atomic<size_t> * failures;

  sf_substr_worker(RStringIndexer * rsi, size_t start_size, size_t stop_size, int * start_ptr, int * stop_ptr,
                   tbb::enumerable_thread_specific<slice_store_shard> * shards, std::atomic<size_t> * failures) :
    rsi(rsi), start_size(start_size), stop_size(stop_size), start_ptr(start_ptr), stop_ptr(stop_ptr),
    shards(shards), failures(failures) {}

  void operator()(std::size_t begin, std::size_t end) {
    sfenc::iconv_text_normalizer norm;
    std::string utf8_owned;
    auto & shard = shards->local();
    for(size_t i = begin; i < end; ++i) {
      sf_substr_write_one(shard, i, *rsi, start_size, stop_size, start_ptr, stop_ptr, norm, utf8_owned);
    }
    failures->fetch_add(norm.failures);
  }
};
#endif
}

SEXP sf_substr(SEXP x, IntegerVector start, IntegerVector stop, const int nthreads) {
  size_t start_size = Rf_xlength(start);
  size_t stop_size = Rf_xlength(stop);
  int * start_ptr = INTEGER(start);
  int * stop_ptr = INTEGER(stop);

  for(size_t i=0; i<start_size; ++i) {
    if(start_ptr[i] == NA_INTEGER) throw std::runtime_error("no NA start values allowed");
  }

  for(size_t i=0; i<stop_size; ++i) {
    if(stop_ptr[i] == NA_INTEGER) throw std::runtime_error("no NA stop values allowed");
  }

  RStringIndexer rsi(x);
  size_t len = rsi.size();
  if(start_size != len && start_size != 1) throw std::runtime_error("length of start must be 1 or the same as x");
  if(stop_size != len && stop_size != 1) throw std::runtime_error("length of stop must be 1 or the same as x");

  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    std::vector<string_record> records(len);
    tbb::enumerable_thread_specific<slice_store_shard> shards([&records] {
      return slice_store_shard(records);
    });
    std::atomic<size_t> failures(0);
    sfexport::sf_substr_worker w(&rsi, start_size, stop_size, start_ptr, stop_ptr, &shards, &failures);
    parallelFor(0, len, w, 100, nthreads);
    sfenc::warn_failed_normalization("sf_substr", failures.load());
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
      sfexport::sf_substr_write_one(ref, i, rsi, start_size, stop_size, start_ptr, stop_ptr, norm, utf8_owned);
    }
    sfenc::warn_failed_normalization("sf_substr", norm.failures);
    UNPROTECT(1);
    return ret;
  }
}

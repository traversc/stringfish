namespace sfexport {
inline sfstring sf_paste_value(size_t i, size_t dotlen, sfenc::string_ref & sep_ref,
                               std::vector<RStringIndexer> & rs, std::vector<size_t> & lens,
                               std::vector<sfenc::string_ref> & singles,
                               sfenc::iconv_text_normalizer & norm, std::string & utf8_owned) {
  std::string temp;
  bool is_na = false;
  bool byte_forced = (dotlen > 1) && (sep_ref.enc == cetype_t_ext::CE_BYTES);
  sfenc::rstring_info sep_text;
  if(dotlen > 1 && sep_ref.raw_bytes == nullptr) {
    return sfstring(NA_STRING);
  }
  for(size_t j = 0; j < dotlen; ++j) {
    sfenc::rstring_info raw = lens[j] == 1 ? singles[j].raw_rstring_info() : rs[j].getCharLenCE(i);
    if(raw.ptr == nullptr) {
      is_na = true;
      break;
    }
    if(raw.enc == cetype_t_ext::CE_BYTES) {
      byte_forced = true;
    }
  }
  if(is_na) {
    return sfstring(NA_STRING);
  }
  if(byte_forced) {
    for(size_t j = 0; j < dotlen; ++j) {
      sfenc::rstring_info raw = lens[j] == 1 ? singles[j].raw_rstring_info() : rs[j].getCharLenCE(i);
      temp.append(raw.ptr, raw.len);
      if(j < (dotlen - 1)) {
        temp.append(sep_ref.raw_bytes, sep_ref.raw_len);
      }
    }
    return sfstring(std::move(temp), cetype_t_ext::CE_BYTES);
  }

  bool all_ascii = true;
  if(dotlen > 1) {
    if(!sep_ref.get_rstring_info(norm, sep_text)) {
      return sfstring(NA_STRING);
    }
    all_ascii = sep_text.enc == cetype_t_ext::CE_ASCII;
  }
  for(size_t j = 0; j < dotlen; ++j) {
    sfenc::rstring_info view;
    if(lens[j] == 1) {
      if(!singles[j].get_rstring_info(norm, view)) {
        return sfstring(NA_STRING);
      }
    } else {
      view = rs[j].getCharLenCE(i);
      utf8_owned.clear();
      if(!norm.normalize(view, utf8_owned)) {
        return sfstring(NA_STRING);
      }
    }
    temp.append(view.ptr, view.len);
    all_ascii = all_ascii && (view.enc == cetype_t_ext::CE_ASCII);
    if(j < (dotlen - 1)) {
      temp.append(sep_text.ptr, sep_text.len);
    }
  }
  return sfenc::make_text_storage(std::move(temp), all_ascii);
}

template <typename Sink>
inline void sf_paste_write_one(Sink & sink, size_t i, size_t dotlen, sfenc::string_ref & sep_ref,
                               std::vector<RStringIndexer> & rs, std::vector<size_t> & lens,
                               std::vector<sfenc::string_ref> & singles,
                               sfenc::iconv_text_normalizer & norm, std::string & utf8_owned) {
  slice_store_write(sink, i, sf_paste_value(i, dotlen, sep_ref, rs, lens, singles, norm, utf8_owned));
}

#if RCPP_PARALLEL_USE_TBB
struct sf_paste_worker : public RcppParallel::Worker {
  size_t dotlen;
  sfenc::string_ref sep_ref;
  std::vector<RStringIndexer> & rs;
  std::vector<size_t> & lens;
  std::vector<sfenc::string_ref> singles;
  tbb::enumerable_thread_specific<slice_store_shard> * shards;
  std::atomic<size_t> * failures;

  sf_paste_worker(size_t dotlen, sfenc::string_ref sep_ref, std::vector<RStringIndexer> & rs,
                  std::vector<size_t> & lens, std::vector<sfenc::string_ref> singles,
                  tbb::enumerable_thread_specific<slice_store_shard> * shards, std::atomic<size_t> * failures) :
    dotlen(dotlen), sep_ref(std::move(sep_ref)), rs(rs), lens(lens), singles(std::move(singles)),
    shards(shards), failures(failures) {}

  void operator()(std::size_t begin, std::size_t end) {
    sfenc::iconv_text_normalizer norm;
    auto sep_local = sep_ref;
    auto singles_local = singles;
    std::string utf8_owned;
    auto & shard = shards->local();
    for(size_t i = begin; i < end; ++i) {
      sf_paste_write_one(shard, i, dotlen, sep_local, rs, lens, singles_local, norm, utf8_owned);
    }
    failures->fetch_add(norm.failures);
  }
};
#endif
}

SEXP c_sf_paste(List dots, SEXP sep, const int nthreads) {

  RStringIndexer sr(sep);
  if(sr.size() != 1) throw std::runtime_error("sep should be a character vector of length 1");
  sfenc::string_ref sep_ref(sr.getCharLenCE(0));
  size_t maxlen = 1;
  size_t dotlen = Rf_xlength(dots);
  std::vector<RStringIndexer> rs;
  rs.reserve(dotlen);
  std::vector<size_t> lens(dotlen);
  std::vector<sfenc::string_ref> singles(dotlen);
  for(size_t i=0; i<dotlen; ++i) {
    SEXP d = dots[i];
    rs.emplace_back(d);
    lens[i] = rs[i].size();
    if(lens[i] == 1) {
      singles[i] = sfenc::string_ref(rs[i].getCharLenCE(0));
    }
    if(maxlen == 1 && lens[i] > 1) {
      maxlen = lens[i];
    }
  }

  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    std::vector<string_record> records(maxlen);
    tbb::enumerable_thread_specific<slice_store_shard> shards([&records] {
      return slice_store_shard(records);
    });
    std::atomic<size_t> failures(0);
    sfexport::sf_paste_worker w(dotlen, std::move(sep_ref), rs, lens, singles, &shards, &failures);
    parallelFor(0, maxlen, w, 100, nthreads);
    sfenc::warn_failed_normalization("sf_paste", failures.load());
    SEXP ret = PROTECT(slice_st::Make(new slice_store_data(std::move(records), shards), true));
    UNPROTECT(1);
    return ret;
#else
    throw std::runtime_error("RcppParallel TBB not supported");
#endif
  } else {
    SEXP ret = PROTECT(slice_store_create(maxlen));
    slice_store_data & ref = slice_store_data_ref(ret);
    sfenc::iconv_text_normalizer norm;
    std::string utf8_owned;
    for(size_t i=0; i<maxlen; ++i) {
      sfexport::sf_paste_write_one(ref, i, dotlen, sep_ref, rs, lens, singles, norm, utf8_owned);
    }
    sfenc::warn_failed_normalization("sf_paste", norm.failures);
    UNPROTECT(1);
    return ret;
  }
}

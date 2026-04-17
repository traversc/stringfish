SEXP c_sf_concat(SEXP x) {
  if(TYPEOF(x) != VECSXP) {
    throw std::runtime_error("x must be a list");
  }
  size_t xlen = Rf_xlength(x);
  std::vector<RStringIndexer> indexers(xlen);
  std::vector<size_t> sizes(xlen);
  size_t total_size = 0;
  for(size_t i=0; i<xlen; ++i) {
    SEXP xi = VECTOR_ELT(x, i);
    indexers[i] = RStringIndexer(xi);
    sizes[i] = indexers[i].size();
    total_size += sizes[i];
  }
  SEXP ret = PROTECT(slice_store_create(total_size));
  auto & ref = slice_store_data_ref(ret);
  size_t count = 0;
  for(size_t i=0; i<xlen; ++i) {
    for(size_t j = 0; j < sizes[i]; ++j) {
      auto q = indexers[i].getCharLenCE(j);
      if(q.ptr == nullptr) {
        slice_store_write_na(ref, count++);
      } else {
        slice_store_write(ref, count++, q.ptr, static_cast<size_t>(q.len), q.enc);
      }
    }
  }
  UNPROTECT(1);
  return ret;
}

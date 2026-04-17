namespace sfexport {
inline int sf_compare_value(size_t i, RStringIndexer & xr, RStringIndexer & yr, size_t xlen, size_t ylen,
                            std::optional<sfenc::string_ref> & x_scalar, std::optional<sfenc::string_ref> & y_scalar,
                            sfenc::iconv_text_normalizer & norm, std::string & x_owned, std::string & y_owned) {
  auto qx = xr.getCharLenCE(xlen == 1 ? 0 : i);
  if(qx.ptr == nullptr) {
    return NA_LOGICAL;
  }
  auto qy = yr.getCharLenCE(ylen == 1 ? 0 : i);
  if(qy.ptr == nullptr) {
    return NA_LOGICAL;
  }
  if(qx.enc == cetype_t_ext::CE_BYTES || qy.enc == cetype_t_ext::CE_BYTES) {
    return (qx.len == qy.len && std::memcmp(qx.ptr, qy.ptr, qx.len) == 0) ? TRUE : FALSE;
  }

  sfenc::rstring_info vx;
  sfenc::rstring_info vy;
  bool okx;
  bool oky;
  if(x_scalar.has_value()) {
    okx = x_scalar->get_rstring_info(norm, vx);
  } else {
    vx = qx;
    x_owned.clear();
    okx = norm.normalize(vx, x_owned);
  }
  if(y_scalar.has_value()) {
    oky = y_scalar->get_rstring_info(norm, vy);
  } else {
    vy = qy;
    y_owned.clear();
    oky = norm.normalize(vy, y_owned);
  }
  if(!okx || !oky) {
    return NA_LOGICAL;
  }
  return (vx.len == vy.len && std::memcmp(vx.ptr, vy.ptr, vx.len) == 0) ? TRUE : FALSE;
}

#if RCPP_PARALLEL_USE_TBB
struct sf_compare_worker : public RcppParallel::Worker {
  RStringIndexer & xr;
  RStringIndexer & yr;
  const size_t xlen;
  const size_t ylen;
  int * output;
  std::atomic<size_t> * failures;

  sf_compare_worker(RStringIndexer & xr, RStringIndexer & yr, size_t xlen, size_t ylen,
                    int * output, std::atomic<size_t> * failures) :
    xr(xr), yr(yr), xlen(xlen), ylen(ylen), output(output), failures(failures) {}

  void operator()(std::size_t begin, std::size_t end) {
    sfenc::iconv_text_normalizer norm;
    std::string x_owned;
    std::string y_owned;
    std::optional<sfenc::string_ref> x_scalar;
    std::optional<sfenc::string_ref> y_scalar;
    if(xlen == 1) x_scalar.emplace(xr.getCharLenCE(0));
    if(ylen == 1) y_scalar.emplace(yr.getCharLenCE(0));
    for(size_t i = begin; i < end; ++i) {
      output[i] = sf_compare_value(i, xr, yr, xlen, ylen, x_scalar, y_scalar, norm, x_owned, y_owned);
    }
    failures->fetch_add(norm.failures);
  }
};
#endif
}

LogicalVector sf_compare(SEXP x, SEXP y, const int nthreads) {
  RStringIndexer xr(x);
  RStringIndexer yr(y);
  size_t xlen = xr.size();
  size_t ylen = yr.size();
  if((xlen == 0) || (ylen == 0) || ((xlen != 1) && (ylen != 1) && (xlen != ylen))) {
    throw std::runtime_error("x and y must be length 1 or the same length > 0");
  }
  size_t outlen = std::max(xlen, ylen);
  LogicalVector ret(outlen);
  int * out = INTEGER(ret);

  if(nthreads > 1) {
#if RCPP_PARALLEL_USE_TBB
    std::atomic<size_t> failures(0);
    sfexport::sf_compare_worker w(xr, yr, xlen, ylen, out, &failures);
    parallelFor(0, outlen, w, 100, nthreads);
    sfenc::warn_failed_normalization("sf_compare", failures.load());
#else
    throw std::runtime_error("RcppParallel TBB not supported");
#endif
  } else {
    sfenc::iconv_text_normalizer norm;
    std::string x_owned;
    std::string y_owned;
    std::optional<sfenc::string_ref> x_scalar;
    std::optional<sfenc::string_ref> y_scalar;
    if(xlen == 1) x_scalar.emplace(xr.getCharLenCE(0));
    if(ylen == 1) y_scalar.emplace(yr.getCharLenCE(0));
    for(size_t i=0; i<outlen; ++i) {
      out[i] = sfexport::sf_compare_value(i, xr, yr, xlen, ylen, x_scalar, y_scalar, norm, x_owned, y_owned);
    }
    sfenc::warn_failed_normalization("sf_compare", norm.failures);
  }
  return ret;
}

void sf_assign(SEXP x, size_t i, SEXP e) {
  if(TYPEOF(e) != STRSXP || Rf_xlength(e) != 1) {
    throw std::runtime_error("e must be a string of length 1");
  }
  if(i == 0) {
    throw std::runtime_error("i must be > 0");
  }
  size_t len = Rf_xlength(x);
  if(i > len) {
    throw std::runtime_error("i must be <= length(x)");
  }
  i--;
  rstring_type rx = get_rstring_type(x);
  switch(rx) {
  case rstring_type::SF_VEC:
  {
    sf_vec_data & ref = sf_vec_data_ref(x);
    ref[i] = sfstring(STRING_ELT(e,0));
  }
    break;
  case rstring_type::slice_store:
  {
    slice_store_data & ref = slice_store_data_ref(x);
    slice_store_write(ref, i, STRING_ELT(e,0));
  }
    break;
  case rstring_type::SF_VEC_MATERIALIZED:
  case rstring_type::slice_store_MATERIALIZED:
    SET_STRING_ELT(R_altrep_data2(x), i, STRING_ELT(e,0));
    break;
  case rstring_type::NORMAL:
  case rstring_type::OTHER_ALT_REP:
  default:
    SET_STRING_ELT(x, i, STRING_ELT(e,0));
    break;
  }
}

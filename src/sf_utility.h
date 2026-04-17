#ifndef STRINGFISH_SF_UTILITY_H
#define STRINGFISH_SF_UTILITY_H

// Runtime locale state only used by the explicit normalization layer.
// Installed headers must not depend on this value.
static bool is_utf8_locale = false;

bool sf_internal_is_utf8_locale() noexcept {
  return is_utf8_locale;
}

void set_is_utf8_locale() {
  is_utf8_locale = true;
}

void unset_is_utf8_locale() {
  is_utf8_locale = false;
}

bool get_is_utf8_locale() {
  return is_utf8_locale;
}

// Runtime ALTREP class discrimination for stringfish-owned objects.
rstring_type get_rstring_type(SEXP obj) {
  if(TYPEOF(obj) != STRSXP) {
    throw std::runtime_error("Object not an Character Vector");
  }
  if(!ALTREP(obj)) {
    return rstring_type::NORMAL;
  }
  if(R_altrep_inherits(obj, sf_vec::class_t)) {
    return DATAPTR_OR_NULL(obj) == nullptr ? rstring_type::SF_VEC : rstring_type::SF_VEC_MATERIALIZED;
  }
  if(R_altrep_inherits(obj, slice_st::class_t)) {
    return DATAPTR_OR_NULL(obj) == nullptr ? rstring_type::slice_store : rstring_type::slice_store_MATERIALIZED;
  }
  return rstring_type::OTHER_ALT_REP;
}

uint8_t get_rstring_type_export(SEXP obj) {
  return static_cast<uint8_t>(get_rstring_type(obj));
}

bool is_tbb() {
#if RCPP_PARALLEL_USE_TBB
  return true;
#endif
  return false;
}

void check_simd() {
#if defined (__AVX2__)
  Rcpp::Rcerr << "AVX2" << std::endl;
#elif defined (__SSE2__)
  Rcpp::Rcerr << "SSE2" << std::endl;
#else
  Rcpp::Rcerr << "no SIMD" << std::endl;
#endif
}

List get_pcre2_info() {
  auto result = sf::pcre2_info();
  return List::create(
    _["pcre2_header_version"] = IntegerVector::create(result.first),
    _["is_bundled"] = LogicalVector::create(result.second)
  );
}

std::string get_string_type(SEXP x) {
  rstring_type rx = get_rstring_type(x);
  switch(rx) {
  case rstring_type::NORMAL:
    return "normal vector";
  case rstring_type::SF_VEC:
    return "stringfish vector";
  case rstring_type::SF_VEC_MATERIALIZED:
    return "stringfish vector (materialized)";
  case rstring_type::slice_store:
    return "stringfish slice store";
  case rstring_type::slice_store_MATERIALIZED:
    return "stringfish slice store (materialized)";
  case rstring_type::OTHER_ALT_REP:
    return "other alt-rep vector";
  default:
    throw std::runtime_error("get_string_type error");
  }
}

SEXP materialize(SEXP x) {
  if(ALTREP(x)) {
    DATAPTR_RO(x);
  }
  return x;
}

#endif

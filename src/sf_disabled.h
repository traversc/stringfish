#ifndef SF_DISABLED_H
#define SF_DISABLED_H

#define NO_ALTREP_SUPPORT() throw std::runtime_error("ALTREP not supported in R < 3.5")
void init_stringfish(DllInfo* dll) {(void)0;} // no op; the init attribute still gets read in sf_altrep.h.
void sf_export_functions(DllInfo* dll) {(void)0;}

std::string get_string_type(SEXP x) {NO_ALTREP_SUPPORT();}
SEXP materialize(SEXP x) {NO_ALTREP_SUPPORT();}
SEXP sf_vector(size_t len) {NO_ALTREP_SUPPORT();}
void sf_assign(SEXP x, size_t i, SEXP e) {NO_ALTREP_SUPPORT();}
SEXP sf_iconv(SEXP x, const std::string from, const std::string to, int nthreads=1) {NO_ALTREP_SUPPORT();}
convert_to_sf(SEXP x) {NO_ALTREP_SUPPORT();}
IntegerVector sf_nchar(SEXP x, const std::string type = "chars", const int nthreads = 1) {NO_ALTREP_SUPPORT();}
SEXP sf_substr(SEXP x, IntegerVector start, IntegerVector stop, const int nthreads = 1) {NO_ALTREP_SUPPORT();}
SEXP c_sf_paste(List dots, SEXP sep, const int nthreads = 1) {NO_ALTREP_SUPPORT();}
SEXP sf_collapse(SEXP x, SEXP collapse) {NO_ALTREP_SUPPORT();}
SEXP sf_readLines(const std::string file, const std::string encoding = "UTF-8") {NO_ALTREP_SUPPORT();}
sf_writeLines(SEXP text, const std::string file, const std::string sep = "\n", const std::string na_value = "NA", const std::string encode_mode = "UTF-8") {NO_ALTREP_SUPPORT();}
LogicalVector sf_grepl(SEXP subject, SEXP pattern, const std::string encode_mode = "auto", const bool fixed = false ,const int nthreads = 1) {NO_ALTREP_SUPPORT();}
SEXP sf_split(SEXP subject, SEXP split, const std::string encode_mode = "auto", const bool fixed = false, const int nthreads = 1) {NO_ALTREP_SUPPORT();}
SEXP sf_gsub(SEXP subject, SEXP pattern, SEXP replacement, const std::string encode_mode = "auto", const bool fixed = false, const int nthreads = 1) {NO_ALTREP_SUPPORT();}
SEXP random_strings(const int N, const int string_size = 50, 
                    std::string charset = "abcdefghijklmnopqrstuvwxyz", 
                    std::string vector_mode = "stringfish") {
  if(vector_mode == "stringfish") Rcpp::warning("ALTREP not supported in R < 3.5");
    CharacterVector ret(N);
  std::string str;
  str.resize(string_size);
  for(int i=0; i<N; ++i) {
    std::vector<int> r = Rcpp::as< std::vector<int> >(Rcpp::sample(charset.size(), string_size, true, R_NilValue, false));
    for(int j=0; j<string_size; j++) str[j] = charset[r[j]];
    ret[i] = str;
  }
  return ret;
}
SEXP sf_tolower(SEXP x) {NO_ALTREP_SUPPORT();}
SEXP sf_toupper(SEXP x) {NO_ALTREP_SUPPORT();}
IntegerVector sf_match(SEXP x, SEXP table, const int nthreads = 1) {NO_ALTREP_SUPPORT();}
LogicalVector sf_compare(SEXP x, SEXP y, const int nthreads = 1) {NO_ALTREP_SUPPORT();}
SEXP c_sf_concat(SEXP x) {NO_ALTREP_SUPPORT();}



#endif
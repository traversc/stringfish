#ifndef PCRE2_WRAPPER_H
#define PCRE2_WRAPPER_H

// defined in Makevars
// #ifndef PCRE2_CODE_UNIT_WIDTH
// #define PCRE2_CODE_UNIT_WIDTH 8
// #endif
// #ifndef HAVE_CONFIG_H
// #define HAVE_CONFIG_H
// #endif
#include "pcre2.h"

namespace sf {

struct pcre2_match_wrapper {
  pcre2_code * re;
  pcre2_match_data * match_data;
  pcre2_match_wrapper(const char * pattern_ptr, bool utf8, bool literal = false);
  pcre2_match_wrapper();
  pcre2_match_wrapper(const pcre2_match_wrapper& other);
  pcre2_match_wrapper(pcre2_match_wrapper && other);
  pcre2_match_wrapper & operator=(const pcre2_match_wrapper & other);
  pcre2_match_wrapper & operator=(pcre2_match_wrapper && other);
  ~pcre2_match_wrapper();
  int match(const char * subject_ptr, const int len);
  int match_get_interval(const char * subject_ptr, const int len, size_t & begin, size_t & end);
  PCRE2_SIZE * match_ovector();
};

struct pcre2_sub_wrapper {
  pcre2_code * re;
  pcre2_match_data * match_data;
  PCRE2_SPTR replacement;
  std::vector<char> output;
  pcre2_sub_wrapper(const char * pattern_ptr, const char * replacement_ptr, bool utf8, bool literal = false);
  pcre2_sub_wrapper();
  pcre2_sub_wrapper & operator=(const pcre2_sub_wrapper & other);
  pcre2_sub_wrapper & operator=(pcre2_sub_wrapper && other);
  pcre2_sub_wrapper(const pcre2_sub_wrapper& other);
  pcre2_sub_wrapper(pcre2_sub_wrapper && other);
  ~pcre2_sub_wrapper();
  const char * gsub(const char * subject_ptr);
};

} // namespace

#endif // include guard

#ifndef PCRE2_WRAPPER_H
#define PCRE2_WRAPPER_H

#include <utility>
#include "pcre2.h"

#ifdef PCRE2_BUNDLED
  #define CALL_pcre2_callout_enumerate bundled_pcre2_callout_enumerate
  #define CALL_pcre2_code_copy bundled_pcre2_code_copy
  #define CALL_pcre2_code_copy_with_tables bundled_pcre2_code_copy_with_tables
  #define CALL_pcre2_code_free bundled_pcre2_code_free
  #define CALL_pcre2_compile bundled_pcre2_compile
  #define CALL_pcre2_compile_context_copy bundled_pcre2_compile_context_copy
  #define CALL_pcre2_compile_context_create bundled_pcre2_compile_context_create
  #define CALL_pcre2_compile_context_free bundled_pcre2_compile_context_free
  #define CALL_pcre2_config bundled_pcre2_config
  #define CALL_pcre2_convert_context_copy bundled_pcre2_convert_context_copy
  #define CALL_pcre2_convert_context_create bundled_pcre2_convert_context_create
  #define CALL_pcre2_convert_context_free bundled_pcre2_convert_context_free
  #define CALL_pcre2_converted_pattern_free bundled_pcre2_converted_pattern_free
  #define CALL_pcre2_dfa_match bundled_pcre2_dfa_match
  #define CALL_pcre2_general_context_copy bundled_pcre2_general_context_copy
  #define CALL_pcre2_general_context_create bundled_pcre2_general_context_create
  #define CALL_pcre2_general_context_free bundled_pcre2_general_context_free
  #define CALL_pcre2_get_error_message bundled_pcre2_get_error_message
  #define CALL_pcre2_get_mark bundled_pcre2_get_mark
  #define CALL_pcre2_get_match_data_size bundled_pcre2_get_match_data_size
  #define CALL_pcre2_get_ovector_pointer bundled_pcre2_get_ovector_pointer
  #define CALL_pcre2_get_ovector_count bundled_pcre2_get_ovector_count
  #define CALL_pcre2_get_startchar bundled_pcre2_get_startchar
  #define CALL_pcre2_jit_compile bundled_pcre2_jit_compile
  #define CALL_pcre2_jit_match bundled_pcre2_jit_match
  #define CALL_pcre2_jit_free_unused_memory bundled_pcre2_jit_free_unused_memory
  #define CALL_pcre2_jit_stack_assign bundled_pcre2_jit_stack_assign
  #define CALL_pcre2_jit_stack_create bundled_pcre2_jit_stack_create
  #define CALL_pcre2_jit_stack_free bundled_pcre2_jit_stack_free
  #define CALL_pcre2_maketables bundled_pcre2_maketables
  #define CALL_pcre2_maketables_free bundled_pcre2_maketables_free
  #define CALL_pcre2_match bundled_pcre2_match
  #define CALL_pcre2_match_context_copy bundled_pcre2_match_context_copy
  #define CALL_pcre2_match_context_create bundled_pcre2_match_context_create
  #define CALL_pcre2_match_context_free bundled_pcre2_match_context_free
  #define CALL_pcre2_match_data_create bundled_pcre2_match_data_create
  #define CALL_pcre2_match_data_create_from_pattern bundled_pcre2_match_data_create_from_pattern
  #define CALL_pcre2_match_data_free bundled_pcre2_match_data_free
  #define CALL_pcre2_pattern_convert bundled_pcre2_pattern_convert
  #define CALL_pcre2_pattern_info bundled_pcre2_pattern_info
  #define CALL_pcre2_serialize_decode bundled_pcre2_serialize_decode
  #define CALL_pcre2_serialize_encode bundled_pcre2_serialize_encode
  #define CALL_pcre2_serialize_free bundled_pcre2_serialize_free
  #define CALL_pcre2_serialize_get_number_of_codes bundled_pcre2_serialize_get_number_of_codes
  #define CALL_pcre2_set_bsr bundled_pcre2_set_bsr
  #define CALL_pcre2_set_callout bundled_pcre2_set_callout
  #define CALL_pcre2_set_character_tables bundled_pcre2_set_character_tables
  #define CALL_pcre2_set_compile_extra_options bundled_pcre2_set_compile_extra_options
  #define CALL_pcre2_set_compile_recursion_guard bundled_pcre2_set_compile_recursion_guard
  #define CALL_pcre2_set_depth_limit bundled_pcre2_set_depth_limit
  #define CALL_pcre2_set_glob_escape bundled_pcre2_set_glob_escape
  #define CALL_pcre2_set_glob_separator bundled_pcre2_set_glob_separator
  #define CALL_pcre2_set_heap_limit bundled_pcre2_set_heap_limit
  #define CALL_pcre2_set_match_limit bundled_pcre2_set_match_limit
  #define CALL_pcre2_set_max_pattern_length bundled_pcre2_set_max_pattern_length
  #define CALL_pcre2_set_newline bundled_pcre2_set_newline
  #define CALL_pcre2_set_parens_nest_limit bundled_pcre2_set_parens_nest_limit
  #define CALL_pcre2_set_offset_limit bundled_pcre2_set_offset_limit
  #define CALL_pcre2_set_substitute_callout bundled_pcre2_set_substitute_callout
  #define CALL_pcre2_substitute bundled_pcre2_substitute
  #define CALL_pcre2_substring_copy_byname bundled_pcre2_substring_copy_byname
  #define CALL_pcre2_substring_copy_bynumber bundled_pcre2_substring_copy_bynumber
  #define CALL_pcre2_substring_free bundled_pcre2_substring_free
  #define CALL_pcre2_substring_get_byname bundled_pcre2_substring_get_byname
  #define CALL_pcre2_substring_get_bynumber bundled_pcre2_substring_get_bynumber
  #define CALL_pcre2_substring_length_byname bundled_pcre2_substring_length_byname
  #define CALL_pcre2_substring_length_bynumber bundled_pcre2_substring_length_bynumber
  #define CALL_pcre2_substring_list_get bundled_pcre2_substring_list_get
  #define CALL_pcre2_substring_list_free bundled_pcre2_substring_list_free
  #define CALL_pcre2_substring_nametable_scan bundled_pcre2_substring_nametable_scan
  #define CALL_pcre2_substring_number_from_name bundled_pcre2_substring_number_from_name
#else
  #define CALL_pcre2_callout_enumerate pcre2_callout_enumerate
  #define CALL_pcre2_code_copy pcre2_code_copy
  #define CALL_pcre2_code_copy_with_tables pcre2_code_copy_with_tables
  #define CALL_pcre2_code_free pcre2_code_free
  #define CALL_pcre2_compile pcre2_compile
  #define CALL_pcre2_compile_context_copy pcre2_compile_context_copy
  #define CALL_pcre2_compile_context_create pcre2_compile_context_create
  #define CALL_pcre2_compile_context_free pcre2_compile_context_free
  #define CALL_pcre2_config pcre2_config
  #define CALL_pcre2_convert_context_copy pcre2_convert_context_copy
  #define CALL_pcre2_convert_context_create pcre2_convert_context_create
  #define CALL_pcre2_convert_context_free pcre2_convert_context_free
  #define CALL_pcre2_converted_pattern_free pcre2_converted_pattern_free
  #define CALL_pcre2_dfa_match pcre2_dfa_match
  #define CALL_pcre2_general_context_copy pcre2_general_context_copy
  #define CALL_pcre2_general_context_create pcre2_general_context_create
  #define CALL_pcre2_general_context_free pcre2_general_context_free
  #define CALL_pcre2_get_error_message pcre2_get_error_message
  #define CALL_pcre2_get_mark pcre2_get_mark
  #define CALL_pcre2_get_match_data_size pcre2_get_match_data_size
  #define CALL_pcre2_get_ovector_pointer pcre2_get_ovector_pointer
  #define CALL_pcre2_get_ovector_count pcre2_get_ovector_count
  #define CALL_pcre2_get_startchar pcre2_get_startchar
  #define CALL_pcre2_jit_compile pcre2_jit_compile
  #define CALL_pcre2_jit_match pcre2_jit_match
  #define CALL_pcre2_jit_free_unused_memory pcre2_jit_free_unused_memory
  #define CALL_pcre2_jit_stack_assign pcre2_jit_stack_assign
  #define CALL_pcre2_jit_stack_create pcre2_jit_stack_create
  #define CALL_pcre2_jit_stack_free pcre2_jit_stack_free
  #define CALL_pcre2_maketables pcre2_maketables
  #define CALL_pcre2_maketables_free pcre2_maketables_free
  #define CALL_pcre2_match pcre2_match
  #define CALL_pcre2_match_context_copy pcre2_match_context_copy
  #define CALL_pcre2_match_context_create pcre2_match_context_create
  #define CALL_pcre2_match_context_free pcre2_match_context_free
  #define CALL_pcre2_match_data_create pcre2_match_data_create
  #define CALL_pcre2_match_data_create_from_pattern pcre2_match_data_create_from_pattern
  #define CALL_pcre2_match_data_free pcre2_match_data_free
  #define CALL_pcre2_pattern_convert pcre2_pattern_convert
  #define CALL_pcre2_pattern_info pcre2_pattern_info
  #define CALL_pcre2_serialize_decode pcre2_serialize_decode
  #define CALL_pcre2_serialize_encode pcre2_serialize_encode
  #define CALL_pcre2_serialize_free pcre2_serialize_free
  #define CALL_pcre2_serialize_get_number_of_codes pcre2_serialize_get_number_of_codes
  #define CALL_pcre2_set_bsr pcre2_set_bsr
  #define CALL_pcre2_set_callout pcre2_set_callout
  #define CALL_pcre2_set_character_tables pcre2_set_character_tables
  #define CALL_pcre2_set_compile_extra_options pcre2_set_compile_extra_options
  #define CALL_pcre2_set_compile_recursion_guard pcre2_set_compile_recursion_guard
  #define CALL_pcre2_set_depth_limit pcre2_set_depth_limit
  #define CALL_pcre2_set_glob_escape pcre2_set_glob_escape
  #define CALL_pcre2_set_glob_separator pcre2_set_glob_separator
  #define CALL_pcre2_set_heap_limit pcre2_set_heap_limit
  #define CALL_pcre2_set_match_limit pcre2_set_match_limit
  #define CALL_pcre2_set_max_pattern_length pcre2_set_max_pattern_length
  #define CALL_pcre2_set_newline pcre2_set_newline
  #define CALL_pcre2_set_parens_nest_limit pcre2_set_parens_nest_limit
  #define CALL_pcre2_set_offset_limit pcre2_set_offset_limit
  #define CALL_pcre2_set_substitute_callout pcre2_set_substitute_callout
  #define CALL_pcre2_substitute pcre2_substitute
  #define CALL_pcre2_substring_copy_byname pcre2_substring_copy_byname
  #define CALL_pcre2_substring_copy_bynumber pcre2_substring_copy_bynumber
  #define CALL_pcre2_substring_free pcre2_substring_free
  #define CALL_pcre2_substring_get_byname pcre2_substring_get_byname
  #define CALL_pcre2_substring_get_bynumber pcre2_substring_get_bynumber
  #define CALL_pcre2_substring_length_byname pcre2_substring_length_byname
  #define CALL_pcre2_substring_length_bynumber pcre2_substring_length_bynumber
  #define CALL_pcre2_substring_list_get pcre2_substring_list_get
  #define CALL_pcre2_substring_list_free pcre2_substring_list_free
  #define CALL_pcre2_substring_nametable_scan pcre2_substring_nametable_scan
  #define CALL_pcre2_substring_number_from_name pcre2_substring_number_from_name
#endif


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

std::pair<int, bool> pcre2_info();

} // namespace

#endif // include guard

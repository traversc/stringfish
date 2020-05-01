// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(stringfish)]]
#include <Rcpp.h>
#include "sf_external.h"
using namespace Rcpp;

// [[Rcpp::export]]
SEXP sf_alternate_case(SEXP x) {
  // Iterate through a character vector using the RStringIndexer class
  // If the input vector x is a stringfish character vector it will do so without materialization
  RStringIndexer r(x);
  size_t len = r.size();
  
  // Create an output stringfish vector
  // Like all R objects, it must be protected from garbage collection
  SEXP output = PROTECT(sf_vector(len));
  
  // Obtain a reference to the underlying output data
  sf_vec_data & output_data = sf_vec_data_ref(output);
  
  // You can use range based for loop via an iterator class that returns RStringIndexer::rstring_info e
  // rstring info is a struct containing const char * ptr (null terminated), int len, and cetype_t enc
  // a NA string is represented by a nullptr
  // Alternatively, access the data via the function r.getCharLenCE(i)
  size_t i = 0;
  for(auto e : r) {
    // check if string is NA and go to next if it is
    if(e.ptr == nullptr) {
      i++; // increment output index
      continue;
    }
    // create a temporary output string and process the results
    std::string temp(e.len, '\0');
    bool case_switch = false;
    for(int j=0; j<e.len; j++) {
      if((e.ptr[j] >= 65) & (e.ptr[j] <= 90)) { // char j is upper case
        if((case_switch = !case_switch)) { // check if we should convert to lower case
          temp[j] = e.ptr[j] + 32;
          continue;
        }
      } else if((e.ptr[j] >= 97) & (e.ptr[j] <= 122)) { // char j is lower case
        if(!(case_switch = !case_switch)) { // check if we should convert to upper case
          temp[j] = e.ptr[j] - 32;
          continue;
        }
      } else if(e.ptr[j] == 32) {
        case_switch = false;
      }
      temp[j] = e.ptr[j];
    }
    
    // Create a new vector element sfstring and insert the processed string into the stringfish vector
    // sfstring has three constructors, 1) taking a std::string and encoding, 
    // 2) a char pointer and encoding, or 3) a CHARSXP object (e.g. sfstring(NA_STRING))
    output_data[i] = sfstring(temp, e.enc);
    i++; // increment output index
  }
  // Finally, call unprotect and return result
  UNPROTECT(1);
  return output;
}

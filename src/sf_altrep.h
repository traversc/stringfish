#ifndef SF_ALTREP_H
#define SF_ALTREP_H

#include <iostream>
#include <string>
#include <vector>
#include "../inst/include/sf_internal.h"

// load alt-rep header -- see altrepisode package by Romain Francois
#if R_VERSION < R_Version(3, 6, 0)
#define class klass
extern "C" {
#include <R_ext/Altrep.h>
}
#undef class
#else
#include <R_ext/Altrep.h>
#endif

// instead of defining a set of free functions, we structure them
// together in a struct
struct sf_vec {
  static R_altrep_class_t class_t;
  static SEXP Make(sf_vec_data* data, bool owner){
    SEXP xp = PROTECT(R_MakeExternalPtr(data, R_NilValue, R_NilValue));
    if (owner) {
      R_RegisterCFinalizerEx(xp, sf_vec::Finalize, TRUE);
    }
    SEXP res = R_new_altrep(class_t, xp, R_NilValue);
    UNPROTECT(1);
    return res;
  }
  
  // finalizer for the external pointer
  static void Finalize(SEXP xp) {
    sf_vec_data * ptr = reinterpret_cast<sf_vec_data*>(R_ExternalPtrAddr(xp));
    if(ptr == nullptr) return;
    // else
    delete ptr;
    R_ClearExternalPtr(xp); // sets to NULL, so when static cast should become nullptr, right?
  }
  
  // get the std::vector<string>* from the altrep object `x`
  static sf_vec_data* Ptr(SEXP vec) {
    return reinterpret_cast<sf_vec_data*>(R_ExternalPtrAddr(R_altrep_data1(vec)));
  }
  
  // same, but as a reference, for convenience
  static sf_vec_data& Get(SEXP vec) {
    return *reinterpret_cast<sf_vec_data*>(R_ExternalPtrAddr(R_altrep_data1(vec)));
  }
  
  // ALTREP methods -------------------
  
  // The length of the object
  static R_xlen_t Length(SEXP vec){
    SEXP data2 = R_altrep_data2(vec);
    if (data2 != R_NilValue) {
      return Rf_xlength(data2);
    }
    
    return Get(vec).size();
  }
  
  // What gets printed when .Internal(inspect()) is used
  static Rboolean Inspect(SEXP x, int pre, int deep, int pvec, void (*inspect_subtree)(SEXP, int, int, int)){
    bool materialized = Dataptr_or_null(x) != nullptr;
    Rprintf("stringfish (len=%d, ptr=%p)\n", Length(x), Ptr(x));
    if(materialized) {
      Rprintf("materialized\n");
    } else {
      Rprintf("not materialized\n");
    }
    return TRUE;
  }
  
  // ALTVEC methods ------------------
  static SEXP Materialize(SEXP vec) {
    SEXP data2 = R_altrep_data2(vec);
    if (data2 != R_NilValue) {
      return data2;
    }
    R_xlen_t n = Length(vec);
    data2 = PROTECT(Rf_allocVector(STRSXP, n));
    
    auto data1 = Get(vec);
    for (R_xlen_t i = 0; i < n; i++) {
      if(data1[i].encoding == cetype_t_ext::CE_NA) {
        SET_STRING_ELT(data2, i, NA_STRING);
      } else {
        SET_STRING_ELT(data2, i, Rf_mkCharLenCE(data1[i].sdata.c_str(), data1[i].sdata.size(), static_cast<cetype_t>(data1[i].encoding)));
      }
      // switch(data1.encodings[i]) {
      // case 0:
      //   SET_STRING_ELT(data2, i, Rf_mkCharLenCE(data1.strings[i].data(), data1.strings[i].size(), CE_NATIVE) );
      //   break;
      // case 1:
      //   SET_STRING_ELT(data2, i, Rf_mkCharLenCE(data1.strings[i].data(), data1.strings[i].size(), CE_UTF8) );
      //   break;
      // case 2:
      //   SET_STRING_ELT(data2, i, Rf_mkCharLenCE(data1.strings[i].data(), data1.strings[i].size(), CE_LATIN1) );
      //   break;
      // case 3:
      //   SET_STRING_ELT(data2, i, Rf_mkCharLenCE(data1.strings[i].data(), data1.strings[i].size(), CE_BYTES) );
      //   break;
      // default:
      //   SET_STRING_ELT(data2, i, NA_STRING);
      // break;
      // }
    }
    
    R_set_altrep_data2(vec, data2);
    Finalize(R_altrep_data1(vec));
    UNPROTECT(1);
    return data2;
  }
  
  // This is guaranteed to never allocate (in the R sense)
  static const void* Dataptr_or_null(SEXP vec) {
    SEXP data2 = R_altrep_data2(vec);
    if (data2 == R_NilValue) return nullptr;
    return STDVEC_DATAPTR(data2);
  }
  
  // same in this case, writeable is ignored
  static void* Dataptr(SEXP vec, Rboolean writeable) {
    return STDVEC_DATAPTR(Materialize(vec));
  }
  
  
  // ALTSTRING methods -----------------
  // the element at the index `i`
  // this does not do bounds checking because that's expensive, so
  // the caller must take care of that
  static SEXP string_Elt(SEXP vec, R_xlen_t i){
    SEXP data2 = R_altrep_data2(vec);
    if(data2 != R_NilValue) {
      return STRING_ELT(data2, i);
    }
    sf_vec_data & data1 = Get(vec);
    if(data1[i].encoding == cetype_t_ext::CE_NA) {
      return NA_STRING;
    } else {
      return Rf_mkCharLenCE(data1[i].sdata.c_str(), data1[i].sdata.size(), static_cast<cetype_t>(data1[i].encoding));
    }
    // case 0:
    //   return Rf_mkCharLenCE(data1.strings[i].data(), data1.strings[i].size(), CE_NATIVE);
    // case 1:
    //   return Rf_mkCharLenCE(data1.strings[i].data(), data1.strings[i].size(), CE_UTF8);
    // case 2:
    //   return Rf_mkCharLenCE(data1.strings[i].data(), data1.strings[i].size(), CE_LATIN1);
    // case 3:
    //   return Rf_mkCharLenCE(data1.strings[i].data(), data1.strings[i].size(), CE_BYTES);
    // default: // case 4+
    //   return NA_STRING;
    // break;
  }
  
  static void string_Set_elt(SEXP vec, R_xlen_t i, SEXP new_val) {
    SEXP data2 = R_altrep_data2(vec);
    if(data2 != R_NilValue) {
      SET_STRING_ELT(new_val, i, data2);
      return;
    }
    auto data1 = Get(vec);
    data1[i] = sfstring(new_val);
  }
  
  static int no_NA(SEXP vec) {
    SEXP data2 = R_altrep_data2(vec);
    R_xlen_t len = Length(vec);
    if (data2 != R_NilValue) {
      for(R_xlen_t i = 0; i < len; i++) {
        if(STRING_ELT(data2, i) == NA_STRING) return 0;
      }
      return 1;
    }
    
    auto data1 = Get(vec);
    for(size_t i=0; i<data1.size(); i++) {
      if(data1[i].encoding == cetype_t_ext::CE_NA) return 0;
    }
    return 1;
  }
  
  // -------- initialize the altrep class with the methods above
  static void Init(DllInfo* dll){
    class_t = R_make_altstring_class("__sf_vec__", "stringfish", dll);
    
    // altrep
    R_set_altrep_Length_method(class_t, Length);
    R_set_altrep_Inspect_method(class_t, Inspect);
    
    // altvec
    R_set_altvec_Dataptr_method(class_t, Dataptr);
    R_set_altvec_Dataptr_or_null_method(class_t, Dataptr_or_null);
    
    // altstring
    R_set_altstring_Elt_method(class_t, string_Elt);
    R_set_altstring_Set_elt_method(class_t, string_Set_elt);
    R_set_altstring_No_NA_method(class_t, no_NA);
  }
};

// static initialization of stringfish::class_t
R_altrep_class_t sf_vec::class_t;

// [[Rcpp::init]]
void init_stringfish(DllInfo* dll){
  sf_vec::Init(dll);
}

#endif // include guard

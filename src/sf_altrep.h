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
    Rprintf("stringfish (len=%llu, ptr=%p)\n", static_cast<unsigned long long int>(Length(x)), static_cast<void*>(Ptr(x)));
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
    auto & data1 = Get(vec);
    R_xlen_t n = data1.size();
    data2 = PROTECT(Rf_allocVector(STRSXP, n));
    for (R_xlen_t i = 0; i < n; ++i) {
      if(data1[i].encoding == cetype_t_ext::CE_NA) {
        SET_STRING_ELT(data2, i, NA_STRING);
      } else if(data1[i].encoding == cetype_t_ext::CE_ASCII) {
        SET_STRING_ELT(data2, i, Rf_mkCharLenCE(data1[i].sdata.c_str(), data1[i].sdata.size(), CE_NATIVE));
      } else {
        SET_STRING_ELT(data2, i, Rf_mkCharLenCE(data1[i].sdata.c_str(), data1[i].sdata.size(), static_cast<cetype_t>(data1[i].encoding)));
      }
    }

    R_set_altrep_data2(vec, data2);
    Finalize(R_altrep_data1(vec)); // clear ext pointer
    UNPROTECT(1);
    return data2;
  }

  // This is guaranteed to never allocate (in the R sense)
  static const void* Dataptr_or_null(SEXP vec) {
    SEXP data2 = R_altrep_data2(vec);
    if (data2 == R_NilValue) return nullptr;
    return DATAPTR_RO(data2);
  }

  // Signature requires a non-const pointer, so we use const_cast on DATAPTR_RO
  // since DATAPTR is no longer part of the API. This approach is used by arrow.
  static void* Dataptr(SEXP vec, Rboolean writeable) {
    return const_cast<void*>(DATAPTR_RO(Materialize(vec)));
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
    } else if(data1[i].encoding == cetype_t_ext::CE_ASCII) {
      return Rf_mkCharLenCE(data1[i].sdata.c_str(), data1[i].sdata.size(), CE_NATIVE);
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
    auto & data1 = Get(vec);
    data1[i] = sfstring(new_val);
  }

  static int no_NA(SEXP vec) {
    SEXP data2 = R_altrep_data2(vec);
    R_xlen_t len = Length(vec);
    if (data2 != R_NilValue) {
      for(R_xlen_t i = 0; i < len; ++i) {
        if(STRING_ELT(data2, i) == NA_STRING) return 0;
      }
      return 1;
    }

    auto & data1 = Get(vec);
    for(size_t i=0; i<data1.size(); ++i) {
      if(data1[i].encoding == cetype_t_ext::CE_NA) return 0;
    }
    return 1;
  }

  static SEXP Extract_subset(SEXP x, SEXP indx, SEXP call) {
    SEXP data2 = R_altrep_data2(x);
    if(data2 != nullptr) {
      return nullptr;
    }

    size_t len = Rf_xlength(indx);
    sf_vec_data & ref = Get(x);
    sf_vec_data * out = new sf_vec_data(len);
    sf_vec_data & outref = *out;
    if(TYPEOF(indx) == INTSXP) {
      int * idx = INTEGER(indx);
      for(size_t i=0; i<len; ++i) {
        int idx_i = idx[i]; // 1 based
        if( (static_cast<size_t>(idx_i) > ref.size()) || (idx_i == NA_INTEGER) ) {
          outref[i] = sfstring(NA_STRING);
        } else {
          outref[i] = sfstring(ref[idx_i - 1]);
        }
      }
    } else if(TYPEOF(indx) == REALSXP) {
      double * idx = REAL(indx);
      for(size_t i=0; i<len; ++i) {
        double idx_i = idx[i]; // 1 based
        if((static_cast<size_t>(idx_i) > ref.size()) || (idx[i] == NA_REAL) ) {
          outref[i] = sfstring(NA_STRING);
        } else {
          outref[i] = sfstring(ref[static_cast<size_t>(idx_i) - 1]);
        }
      }
    } else {
      throw std::runtime_error("invalid indx type in Extract_subset method");
    } // no other type should be possible
    return Make(out, true);
  }

  // serialization
  static SEXP Serialized_state(SEXP vec) {
    SEXP data2 = R_altrep_data2(vec);
    if(data2 == R_NilValue) { // unmaterialized
      auto & data1 = Get(vec);
      uint64_t n = data1.size();
      uint64_t total_size = 0;
      for (uint64_t i = 0; i < n; ++i) {
        total_size += data1[i].sdata.size();
      }
      // 8 bytes - vector length
      // n * 4 bytes - string sizes (use int32_t)
      // n * 1 bytes - encoding (cetype_t_ext : uint8_t)
      // no need to protect since no additional alloc
      SEXP serialized_state = Rf_allocVector(RAWSXP, sizeof(uint64_t) + (sizeof(int) + sizeof(uint8_t)) * n + total_size);
      unsigned char * serialized_ptr = RAW(serialized_state);

      // write vector length
      std::memcpy(serialized_ptr, &n, sizeof(uint64_t));
      unsigned char * current_offset = serialized_ptr + sizeof(uint64_t);

      // write sizes
      for(uint64_t i=0; i<n; ++i) {
        int32_t size = data1[i].sdata.size();
        std::memcpy(current_offset, &size, sizeof(int32_t));
        current_offset += sizeof(int32_t);
      }

      // write encodings
      for(uint64_t i=0; i<n; ++i) {
        uint8_t encoding = static_cast<uint8_t>(data1[i].encoding);
        std::memcpy(current_offset, &encoding, sizeof(uint8_t));
        current_offset += sizeof(uint8_t);
      }

      // write strings
      for(uint64_t i=0; i<n; ++i) {
        std::memcpy(current_offset, data1[i].sdata.data(), data1[i].sdata.size());
        current_offset += data1[i].sdata.size();
      }

      return serialized_state;
    } else { // materialized
      return data2;
    }
  }

  static SEXP Unserialize(SEXP /* class */, SEXP serialized_state) {
    if(TYPEOF(serialized_state) == STRSXP) {
      return serialized_state; // was materialized
    } else if(TYPEOF(serialized_state) == RAWSXP) { // wasn't materialized
      unsigned char * serialized_ptr = RAW(serialized_state);
      uint64_t n;
      std::memcpy(&n, serialized_ptr, sizeof(uint64_t));
      sf_vec_data * ret = new sf_vec_data(n);

      unsigned char * size_offset = serialized_ptr + sizeof(uint64_t); // offset to size of strings
      cetype_t_ext * enc_offset = reinterpret_cast<cetype_t_ext*>(size_offset + n * sizeof(int32_t)); // offset to encoding of strings
      char * data_offset = reinterpret_cast<char * >(enc_offset + n * sizeof(uint8_t)); // offset to data of strings

      for(uint64_t i=0; i<n; ++i) {
        int32_t size;
        std::memcpy(&size, size_offset, sizeof(int32_t));
        cetype_t_ext encoding = *enc_offset;
        (*ret)[i] = sfstring(data_offset, size, encoding);
        size_offset += sizeof(int32_t);
        enc_offset += sizeof(uint8_t);
        data_offset += size;
      }
      return sf_vec::Make(ret, true);
    } else {
      throw std::runtime_error("invalid serialized_state type");
    }

  }

  // -------- initialize the altrep class with the methods above
  static void Init(DllInfo* dll){
    class_t = R_make_altstring_class("__sf_vec__", "stringfish", dll);

    // altrep
    R_set_altrep_Serialized_state_method(class_t, Serialized_state);
    R_set_altrep_Unserialize_method(class_t, Unserialize);

    R_set_altrep_Length_method(class_t, Length);
    R_set_altrep_Inspect_method(class_t, Inspect);

    // altvec
    R_set_altvec_Dataptr_method(class_t, Dataptr);
    R_set_altvec_Dataptr_or_null_method(class_t, Dataptr_or_null);

    // altstring
    R_set_altstring_Elt_method(class_t, string_Elt);
    R_set_altstring_Set_elt_method(class_t, string_Set_elt);
    R_set_altstring_No_NA_method(class_t, no_NA);

    // subset method for e.g. head, tail, etc
    R_set_altvec_Extract_subset_method(class_t, Extract_subset);
  }
};

// static initialization of stringfish::class_t
R_altrep_class_t sf_vec::class_t;

// [[Rcpp::init]]
void init_stringfish(DllInfo* dll){
  sf_vec::Init(dll);
}

#endif // include guard

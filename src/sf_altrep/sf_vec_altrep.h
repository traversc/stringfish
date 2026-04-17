#ifndef STRINGFISH_SF_VEC_ALTREP_H
#define STRINGFISH_SF_VEC_ALTREP_H

#include "common.h"

struct sf_vec {
  static R_altrep_class_t class_t;

  static SEXP Make(sf_vec_data* data, bool owner){
    SEXP xp = PROTECT(R_MakeExternalPtr(data, R_NilValue, R_NilValue));
    if(owner) {
      R_RegisterCFinalizerEx(xp, sf_vec::Finalize, TRUE);
    }
    SEXP res = R_new_altrep(class_t, xp, R_NilValue);
    UNPROTECT(1);
    return res;
  }

  static void Finalize(SEXP xp) {
    sf_vec_data * ptr = reinterpret_cast<sf_vec_data*>(R_ExternalPtrAddr(xp));
    if(ptr == nullptr) return;
    delete ptr;
    R_ClearExternalPtr(xp);
  }

  static sf_vec_data* Ptr(SEXP vec) {
    return reinterpret_cast<sf_vec_data*>(R_ExternalPtrAddr(R_altrep_data1(vec)));
  }

  static sf_vec_data& Get(SEXP vec) {
    return *reinterpret_cast<sf_vec_data*>(R_ExternalPtrAddr(R_altrep_data1(vec)));
  }

  static R_xlen_t Length(SEXP vec){
    SEXP data2 = R_altrep_data2(vec);
    if(data2 != R_NilValue) {
      return Rf_xlength(data2);
    }
    return Get(vec).size();
  }

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

  static SEXP Materialize(SEXP vec) {
    SEXP data2 = R_altrep_data2(vec);
    if(data2 != R_NilValue) {
      return data2;
    }
    auto & data1 = Get(vec);
    R_xlen_t n = data1.size();
    data2 = PROTECT(Rf_allocVector(STRSXP, n));
    for(R_xlen_t i = 0; i < n; ++i) {
      SET_STRING_ELT(data2, i, sfenc::make_char(data1[i]));
    }

    R_set_altrep_data2(vec, data2);
    Finalize(R_altrep_data1(vec));
    UNPROTECT(1);
    return data2;
  }

  static SEXP DuplicateEX(SEXP vec, Rboolean /* deep */) {
    SEXP data2 = R_altrep_data2(vec);
    if(data2 != R_NilValue) {
      const R_xlen_t n = Rf_xlength(data2);
      auto * out = new sf_vec_data(static_cast<size_t>(n));
      for(R_xlen_t i = 0; i < n; ++i) {
        (*out)[static_cast<size_t>(i)] = sfstring(STRING_ELT(data2, i));
      }
      return Make(out, true);
    }
    auto * out = new sf_vec_data(Get(vec));
    return Make(out, true);
  }

  static SEXP Duplicate(SEXP vec, Rboolean deep) {
    return DuplicateEX(vec, deep);
  }

  static const void* Dataptr_or_null(SEXP vec) {
    SEXP data2 = R_altrep_data2(vec);
    if(data2 == R_NilValue) return nullptr;
    return DATAPTR_RO(data2);
  }

  static void* Dataptr(SEXP vec, Rboolean writeable) {
    return const_cast<void*>(DATAPTR_RO(Materialize(vec)));
  }

  static SEXP string_Elt(SEXP vec, R_xlen_t i){
    SEXP data2 = R_altrep_data2(vec);
    if(data2 != R_NilValue) {
      return STRING_ELT(data2, i);
    }
    sf_vec_data & data1 = Get(vec);
    return sfenc::make_char(data1[i]);
  }

  static void string_Set_elt(SEXP vec, R_xlen_t i, SEXP new_val) {
    SEXP data2 = R_altrep_data2(vec);
    if(data2 != R_NilValue) {
      SET_STRING_ELT(data2, i, new_val);
      return;
    }
    auto & data1 = Get(vec);
    data1[i] = sfstring(new_val);
  }

  static int no_NA(SEXP vec) {
    SEXP data2 = R_altrep_data2(vec);
    R_xlen_t len = Length(vec);
    if(data2 != R_NilValue) {
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
    return sf_altrep_sexp_guard("sf_vec::Extract_subset", [&]() -> SEXP {
      (void)call;
      SEXP data2 = R_altrep_data2(x);
      auto set_from_index = [&](double idx_i) {
        if(ISNA(idx_i) || idx_i <= 0) {
          return sfstring(NA_STRING);
        }
        R_xlen_t zero_based = static_cast<R_xlen_t>(idx_i) - 1;
        if(data2 != R_NilValue) {
          if(zero_based >= Rf_xlength(data2)) {
            return sfstring(NA_STRING);
          }
          return sfstring(STRING_ELT(data2, zero_based));
        }
        auto & ref = Get(x);
        if(zero_based >= static_cast<R_xlen_t>(ref.size())) {
          return sfstring(NA_STRING);
        }
        return ref[static_cast<size_t>(zero_based)];
      };

      if(TYPEOF(indx) == LGLSXP) {
        const int * idx = LOGICAL(indx);
        const R_xlen_t idx_len = Rf_xlength(indx);
        const R_xlen_t xlen = data2 != R_NilValue ? Rf_xlength(data2) : static_cast<R_xlen_t>(Get(x).size());
        if(idx_len == 0) {
          return Make(new sf_vec_data(), true);
        }

        R_xlen_t out_len = 0;
        for(R_xlen_t i = 0; i < xlen; ++i) {
          int idx_i = idx[i % idx_len];
          if(idx_i == TRUE || idx_i == NA_LOGICAL) {
            ++out_len;
          }
        }

        sf_vec_data * out = new sf_vec_data(static_cast<size_t>(out_len));
        sf_vec_data & outref = *out;
        R_xlen_t out_i = 0;
        for(R_xlen_t i = 0; i < xlen; ++i) {
          int idx_i = idx[i % idx_len];
          if(idx_i == TRUE) {
            outref[static_cast<size_t>(out_i++)] = set_from_index(static_cast<double>(i + 1));
          } else if(idx_i == NA_LOGICAL) {
            outref[static_cast<size_t>(out_i++)] = sfstring(NA_STRING);
          }
        }
        return Make(out, true);
      }

      const R_xlen_t len = Rf_xlength(indx);
      sf_vec_data * out = new sf_vec_data(static_cast<size_t>(len));
      sf_vec_data & outref = *out;
      if(TYPEOF(indx) == INTSXP) {
        const int * idx = INTEGER(indx);
        for(R_xlen_t i = 0; i < len; ++i) {
          outref[static_cast<size_t>(i)] = set_from_index(idx[i]);
        }
      } else if(TYPEOF(indx) == REALSXP) {
        const double * idx = REAL(indx);
        for(R_xlen_t i = 0; i < len; ++i) {
          outref[static_cast<size_t>(i)] = set_from_index(idx[i]);
        }
      } else {
        delete out;
        throw std::runtime_error("invalid indx type in Extract_subset method");
      }
      return Make(out, true);
    });
  }

  static SEXP Serialized_state(SEXP vec) {
    return sf_altrep_sexp_guard("sf_vec::Serialized_state", [&]() -> SEXP {
      SEXP data2 = R_altrep_data2(vec);
      if(data2 == R_NilValue) {
        auto & data1 = Get(vec);
        const size_t n = data1.size();
        size_t total_size = 0;
        for(size_t i = 0; i < n; ++i) {
          if(!check_r_string_len(data1[i].sdata.size())) {
            throw std::runtime_error("serialized string size exceeds R string size");
          }
          total_size = sf_checked_add_size(total_size, data1[i].sdata.size(), "serialized_state payload");
        }
        const size_t total_bytes = sf_checked_serialized_state_length(n, total_size);
        SEXP serialized_state = Rf_allocVector(RAWSXP, static_cast<R_xlen_t>(total_bytes));
        unsigned char * serialized_ptr = RAW(serialized_state);
        const uint64_t n64 = static_cast<uint64_t>(n);

        std::memcpy(serialized_ptr, &n64, sizeof(uint64_t));
        unsigned char * current_offset = serialized_ptr + sizeof(uint64_t);

        for(size_t i = 0; i < n; ++i) {
          const int32_t size = static_cast<int32_t>(data1[i].sdata.size());
          std::memcpy(current_offset, &size, sizeof(int32_t));
          current_offset += sizeof(int32_t);
        }

        for(size_t i = 0; i < n; ++i) {
          const uint8_t encoding = static_cast<uint8_t>(data1[i].encoding);
          std::memcpy(current_offset, &encoding, sizeof(uint8_t));
          current_offset += sizeof(uint8_t);
        }

        for(size_t i = 0; i < n; ++i) {
          std::memcpy(current_offset, data1[i].sdata.data(), data1[i].sdata.size());
          current_offset += data1[i].sdata.size();
        }

        return serialized_state;
      }
      return data2;
    });
  }

  static SEXP Unserialize(SEXP /* class */, SEXP serialized_state) {
    return sf_altrep_sexp_guard("sf_vec::Unserialize", [&]() -> SEXP {
      if(TYPEOF(serialized_state) == STRSXP) {
        return serialized_state;
      }
      sf_serialized_state_layout layout = sf_parse_serialized_state(serialized_state);
      std::unique_ptr<sf_vec_data> ret(new sf_vec_data(layout.n));

      const unsigned char * size_offset = layout.size_offset;
      const unsigned char * enc_offset = layout.enc_offset;
      const unsigned char * data_offset = layout.data_offset;

      for(size_t i = 0; i < layout.n; ++i) {
        uint32_t size = 0;
        std::memcpy(&size, size_offset, sizeof(int32_t));
        size_offset += sizeof(int32_t);
        if(!check_r_string_len(size)) {
          throw std::runtime_error("serialized string size exceeds R string size");
        }

        const cetype_t_ext encoding = static_cast<cetype_t_ext>(*enc_offset++);
        const char * payload = sf_checked_serialized_payload(data_offset, layout.data_end, static_cast<size_t>(size));

        switch(encoding) {
        case cetype_t_ext::CE_ASCII:
        case cetype_t_ext::CE_UTF8:
        case cetype_t_ext::CE_ASCII_OR_UTF8:
        case cetype_t_ext::CE_NATIVE:
        case cetype_t_ext::CE_LATIN1:
        case cetype_t_ext::CE_BYTES:
          (*ret)[i] = sfstring(payload, static_cast<int>(size), encoding);
          break;
        case cetype_t_ext::CE_NA:
          if(size != 0) {
            throw std::runtime_error("serialized NA string must have zero length");
          }
          (*ret)[i] = sfstring(NA_STRING);
          break;
        default:
          throw std::runtime_error("invalid string encoding in serialized_state");
        }
      }

      if(data_offset != layout.data_end) {
        throw std::runtime_error("serialized_state has trailing bytes");
      }
      return sf_vec::Make(ret.release(), true);
    });
  }

  static void Init(DllInfo* dll){
    class_t = R_make_altstring_class("__sf_vec__", "stringfish", dll);

    R_set_altrep_Serialized_state_method(class_t, Serialized_state);
    R_set_altrep_Unserialize_method(class_t, Unserialize);
    R_set_altrep_DuplicateEX_method(class_t, DuplicateEX);
    R_set_altrep_Duplicate_method(class_t, Duplicate);

    R_set_altrep_Length_method(class_t, Length);
    R_set_altrep_Inspect_method(class_t, Inspect);

    R_set_altvec_Dataptr_method(class_t, Dataptr);
    R_set_altvec_Dataptr_or_null_method(class_t, Dataptr_or_null);

    R_set_altstring_Elt_method(class_t, string_Elt);
    R_set_altstring_Set_elt_method(class_t, string_Set_elt);
    R_set_altstring_No_NA_method(class_t, no_NA);

    R_set_altvec_Extract_subset_method(class_t, Extract_subset);
  }
};

#endif

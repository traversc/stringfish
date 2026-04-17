#ifndef STRINGFISH_RSTRING_INDEXER_H
#define STRINGFISH_RSTRING_INDEXER_H

#include "sf_internal.h"

#if defined(STRINGFISH_INTERNAL_BUILD)
rstring_type get_rstring_type(SEXP obj);
static inline rstring_type get_rstring_type_internal(SEXP obj) {
  return get_rstring_type(obj);
}
#else
static inline rstring_type get_rstring_type_internal(SEXP obj) {
  if(TYPEOF(obj) != STRSXP) {
    throw std::runtime_error("Object not an Character Vector");
  }
  static auto fun = reinterpret_cast<uint8_t(*)(SEXP)>(
    R_GetCCallable("stringfish", "get_rstring_type_export")
  );
  return static_cast<rstring_type>(fun(obj));
}
#endif

class RStringIndexer {
private:
  size_t len;
  const SEXP * sexp_ptr;
  sf_vec_data * sfp;
  slice_store_data * ssp;

  inline void set_direct(SEXP obj) {
    sexp_ptr = STRING_PTR_RO(obj);
    len = Rf_xlength(obj);
    sfp = nullptr;
    ssp = nullptr;
  }

  inline void set_sf_vec(SEXP obj) {
    sexp_ptr = nullptr;
    sfp = reinterpret_cast<sf_vec_data*>(R_ExternalPtrAddr(R_altrep_data1(obj)));
    len = sfp->size();
    ssp = nullptr;
  }

  inline void set_slice_store(SEXP obj) {
    sexp_ptr = nullptr;
    sfp = nullptr;
    ssp = reinterpret_cast<slice_store_data*>(R_ExternalPtrAddr(R_altrep_data1(obj)));
    len = ssp->records.size();
  }

public:
  struct rstring_info {
    const char * ptr;
    int len;
    cetype_t_ext enc;
    rstring_info(const char * p, const int l, const cetype_t_ext e) : ptr(p), len(l), enc(e) {}
    rstring_info() : ptr(nullptr), len(0), enc(cetype_t_ext::CE_NA) {}
    rstring_info(const rstring_info & other) : ptr(other.ptr), len(other.len), enc(other.enc) {}
    bool operator==(const rstring_info & other) const {
      if((ptr == nullptr) && (other.ptr == nullptr)) return true;
      if((ptr == nullptr) || (other.ptr == nullptr)) return false;
      if((ptr == other.ptr) && (len == other.len) && (enc == other.enc)) return true;
      if((len != other.len) || (enc != other.enc)) return false;
      return memcmp(ptr, other.ptr, len) == 0;
    }
  };

  class iterator {
  private:
    RStringIndexer const * rsi;
    size_t idx;
  public:
    iterator(RStringIndexer const * r, size_t i) : rsi(r), idx(i) {}
    iterator operator++() { idx++; return *this; }
    bool operator!=(const iterator & other) const {
      return idx != other.idx;
    }
    rstring_info operator*() const {
      return rsi->getCharLenCE(idx);
    }
    inline size_t index() const {
      return idx;
    }
  };

  RStringIndexer(SEXP obj) {
    switch(get_rstring_type_internal(obj)) {
    case rstring_type::NORMAL:
      set_direct(obj);
      break;
    case rstring_type::OTHER_ALT_REP:
      DATAPTR_RO(obj);
      set_direct(obj);
      break;
    case rstring_type::SF_VEC_MATERIALIZED:
      set_direct(R_altrep_data2(obj));
      break;
    case rstring_type::slice_store_MATERIALIZED:
      set_direct(R_altrep_data2(obj));
      break;
    case rstring_type::SF_VEC:
      set_sf_vec(obj);
      break;
    case rstring_type::slice_store:
      set_slice_store(obj);
      break;
    default:
      throw std::runtime_error("incorrect RStringIndexer constructor");
    }
  }

  RStringIndexer() : len(0), sexp_ptr(nullptr), sfp(nullptr), ssp(nullptr) {}

  bool is_NA(size_t i) const {
    if(sfp != nullptr) {
      return (*sfp)[i].encoding == cetype_t_ext::CE_NA;
    }
    if(ssp != nullptr) {
      return ssp->records[i].is_NA();
    }
    return sexp_ptr[i] == NA_STRING;
  }

  bool is_ASCII(size_t i) const {
    if(sfp != nullptr) {
      return (*sfp)[i].encoding == cetype_t_ext::CE_ASCII;
    }
    if(ssp != nullptr) {
      const string_record & rec = ssp->records[i];
      if(rec.is_NA()) {
        return false;
      }
      if(rec.encoding == cetype_t_ext::CE_ASCII) {
        return true;
      }
      if(rec.encoding == cetype_t_ext::CE_ASCII_OR_UTF8) {
        return checkAscii(rec.ptr, static_cast<size_t>(rec.len));
      }
      return false;
    }
    SEXP xi = sexp_ptr[i];
    return xi != NA_STRING && IS_ASCII(xi);
  }

  rstring_info getCharLenCE(size_t i) const {
    if(sfp == nullptr && ssp == nullptr) {
      SEXP xi = sexp_ptr[i];
      if(xi == NA_STRING) {
        return rstring_info(nullptr, 0, cetype_t_ext::CE_NA);
      }
      return rstring_info(CHAR(xi), LENGTH(xi), reinterpret_input_encoding(xi));
    }
    if(sfp != nullptr) {
      cetype_t_ext st = (*sfp)[i].encoding;
      if(st == cetype_t_ext::CE_NA) {
        return rstring_info(nullptr, 0, cetype_t_ext::CE_NA);
      }
      return rstring_info((*sfp)[i].sdata.c_str(), static_cast<int>((*sfp)[i].sdata.size()), st);
    }
    const string_record & rec = ssp->records[i];
    if(rec.is_NA()) {
      return rstring_info(nullptr, 0, cetype_t_ext::CE_NA);
    }
    return rstring_info(rec.ptr, static_cast<int>(rec.len), rec.encoding);
  }

  inline const SEXP * getDirectPtr() const {
    return sexp_ptr;
  }

  inline sf_vec_data * getSFData() const {
    return sfp;
  }

  inline slice_store_data * getSliceStoreData() const {
    return ssp;
  }

  inline size_t size() const {
    return len;
  }

  iterator begin() const {
    return iterator(this, 0);
  }

  iterator end() const {
    return iterator(this, len);
  }
};

#endif

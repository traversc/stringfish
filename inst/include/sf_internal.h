#ifndef SF_INTERNAL_H
#define SF_INTERNAL_H
#include <Rcpp.h>
#include <iostream>
#include <string>
#include <vector>


inline bool checkAscii(const void * ptr, size_t len) {
  const uint8_t * qp = reinterpret_cast<const uint8_t*>(ptr);
  for(size_t j=0; j<len; j++) {
    if(qp[j] > 127) {
      return false;
      break;
    }
  }
  return true;
}

// defined in Rinternals.h, very unlikely to change
// typedef enum {
//   CE_NATIVE = 0,
//   CE_UTF8   = 1,
//   CE_LATIN1 = 2,
//   CE_BYTES  = 3,
//   CE_SYMBOL = 5,
//   CE_ANY    =99
// } cetype_t;

enum class cetype_t_ext : uint8_t {
  CE_NATIVE  = 0,
  CE_UTF8    = 1,
  CE_LATIN1  = 2,
  CE_BYTES   = 3,
  CE_SYMBOL  = 5, // this isn't actually used in getCharCE
  CE_ANY     = 99,
  CE_NA      = 255 // Easier to keep NA values here
};

// cetype_t getCharCE(SEXP x)
// {
//   if(TYPEOF(x) != CHARSXP)
//     error(_("'%s' must be called on a CHARSXP"), "getCharCE");
//   if(IS_UTF8(x)) return CE_UTF8;
//   else if(IS_LATIN1(x)) return CE_LATIN1;
//   else if(IS_BYTES(x)) return CE_BYTES;
//   else return CE_NATIVE;
// }

// ?Encoding
// "paste and sprintf return elements marked as bytes if any 
// of the corresponding inputs is marked as bytes, 
// and otherwise marked as UTF-8 of any of the inputs is marked as UTF-8.
// CE_BYTE >> CE_UTF8 >> CE_LATIN1 >> CE_NATIVE"
inline cetype_t choose_enc(cetype_t x, cetype_t y) {
  if(x == CE_BYTES || y == CE_BYTES) return (CE_BYTES);
  if(x == CE_UTF8 || y == CE_UTF8) return (CE_UTF8);
  if(x == CE_LATIN1 || y == CE_LATIN1) return (CE_LATIN1);
  // if(x == CE_NATIVE || y == CE_NATIVE) return (CE_NATIVE);
  return CE_NATIVE;
}

inline cetype_t choose_enc(cetype_t x, cetype_t y, cetype_t z) {
  if(x == CE_BYTES || y == CE_BYTES || z == CE_BYTES) return (CE_BYTES);
  if(x == CE_UTF8 || y == CE_UTF8 || z == CE_UTF8) return (CE_UTF8);
  if(x == CE_LATIN1 || y == CE_LATIN1 || z == CE_LATIN1) return (CE_LATIN1);
  // if(x == CE_NATIVE || y == CE_NATIVE || z == CE_NATIVE) return (CE_NATIVE);
  return CE_NATIVE;
}

struct sfstring {
  std::string sdata;
  cetype_t_ext encoding;
  sfstring(std::string x, cetype_t enc) : sdata(x) {
    if((enc == CE_NATIVE) || checkAscii(sdata.c_str(), sdata.size())) {
      encoding = cetype_t_ext::CE_NATIVE; // to keep the same as R
    } else {
      encoding = static_cast<cetype_t_ext>(enc);
    }
  }
  sfstring(const char * ptr, cetype_t enc) {
    size_t len = strlen(ptr);
    sdata = std::string(ptr);
    if((enc == CE_NATIVE) || checkAscii(ptr, len)) {
      encoding = cetype_t_ext::CE_NATIVE; // to keep the same as R
    } else {
      encoding = static_cast<cetype_t_ext>(enc);
    }
  }
  sfstring(SEXP x) {
    if(x == NA_STRING) {
      encoding = cetype_t_ext::CE_NA;
    } else {
      sdata = std::string(CHAR(x));
      encoding = static_cast<cetype_t_ext>(Rf_getCharCE(x));
    }
  }
  sfstring() : sdata(""), encoding(cetype_t_ext::CE_NATIVE) {}
  // constructor with no ascii check, when you are sure of the correct encoding
  inline static sfstring nocheck(std::string x, cetype_t enc) {
    return sfstring{x, enc};
  }
};
using sf_vec_data = std::vector<sfstring>;

enum class rstring_type : uint8_t {
    NORMAL               = 0,
    SF_VEC               = 1,
    SF_VEC_MATERIALIZED  = 2,
    // SF_MMAP              = 3,
    // SF_MMAP_MATERIALIZED = 4,
    OTHER_ALT_REP        = 3
};

rstring_type get_rstring_type_internal(SEXP obj) {
  if(TYPEOF(obj) != STRSXP) throw std::runtime_error("Object not an Character Vector");
  if(ALTREP(obj)) {
    SEXP pclass = ATTRIB(ALTREP_CLASS(obj));
    std::string classname = std::string(CHAR(PRINTNAME(CAR(pclass))));
    if(classname == "__sf_vec__") {
      if(DATAPTR_OR_NULL(obj) == nullptr) {
        return rstring_type::SF_VEC;
      } else {
        return rstring_type::SF_VEC_MATERIALIZED;
      }
    } else {
      return rstring_type::OTHER_ALT_REP;
    }
  } else {
    return rstring_type::NORMAL;
  }
}

class RStringIndexer {
private:
  size_t len;
  rstring_type type;
  void * dptr;
public:
  struct rstring_info {
    const char * ptr;
    int len;
    cetype_t enc;
    rstring_info(const char * p, const int l, const cetype_t e) : ptr(p), len(l), enc(e) {}
    rstring_info() : ptr(nullptr), len(0), enc(CE_NATIVE) {}
    rstring_info(const rstring_info & other) : ptr(other.ptr), len(other.len), enc(other.enc) {}
  };
  class iterator {
  private:
    RStringIndexer const * rsi;
    size_t idx;
  public:
    iterator(RStringIndexer const * r, size_t i) : rsi(r), idx(i) {}
    iterator operator++() { idx++; return *this; }
    bool operator!=(const iterator & other) const { 
      return idx != other.idx; // should we check for the same base object (rsi)?
    }
    rstring_info operator*() const { 
      return rsi->getCharLenCE(idx);
    }
    inline size_t index() const {
      return idx;
    }
  };
  RStringIndexer(SEXP obj) {
    type = get_rstring_type_internal(obj);
    switch(type) {
    case rstring_type::NORMAL:
      dptr = obj;
      len = Rf_xlength(obj);
      break;
    case rstring_type::SF_VEC_MATERIALIZED:
    case rstring_type::OTHER_ALT_REP:
      dptr = DATAPTR(obj); // should materialize the object if not already
      len = Rf_xlength(reinterpret_cast<SEXP>(dptr));
      break;
    case rstring_type::SF_VEC:
      // sf_vec_data * ptr = reinterpret_cast<sf_vec_data*>(  );
      dptr = R_ExternalPtrAddr(R_altrep_data1(obj));
      len = reinterpret_cast<sf_vec_data*>(dptr)->size();
      break;
    default:
      throw std::runtime_error("incorrect RStringIndexer constructor");
    }
  }
  rstring_info getCharLenCE(size_t i) const {
    switch(type) {
    case rstring_type::NORMAL:
    case rstring_type::SF_VEC_MATERIALIZED:
    case rstring_type::OTHER_ALT_REP:
    {
      SEXP xi = STRING_ELT(reinterpret_cast<SEXP>(dptr), i);
      if(xi == NA_STRING) {
        return rstring_info(nullptr, 0, CE_NATIVE);
      }
      return rstring_info(CHAR(xi), strlen(CHAR(xi)), Rf_getCharCE(xi));
    }
    case rstring_type::SF_VEC:
    {
      sf_vec_data & sfp = *reinterpret_cast<sf_vec_data*>(dptr);
      cetype_t_ext st = sfp[i].encoding;
      if(st == cetype_t_ext::CE_NA) {
        return rstring_info(nullptr, 0, CE_NATIVE);
      }
      return rstring_info(sfp[i].sdata.c_str(), sfp[i].sdata.size(), static_cast<cetype_t>(sfp[i].encoding));
    }
    default:
      throw std::runtime_error("getCharLenCE error");
    }
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

// C helper functions

// helper utf-8 nb of chars // https://stackoverflow.com/a/3586973/2723734
inline int code_points(const char * p) {
  // const uint8_t * p = reinterpret_cast<const uint8_t*>(x); // not necessary to cast because bitwise operations promote to int
  int count = 0;
  for (; *p != 0; ++p) {
    count += ((*p & 0xc0) != 0x80); // b11000000, b10000000
  }
  return count;
}


#endif // end include guard
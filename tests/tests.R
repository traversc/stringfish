if (
  require("stringfish", quietly = TRUE) &&
  require("qs2",        quietly = TRUE) &&
  require("dplyr",      quietly = TRUE) &&
  require("Rcpp",       quietly = TRUE) &&
  require("stringr",    quietly = TRUE) &&
  require("rlang",      quietly = TRUE)
) {

alternate_case_source <- '
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(stringfish)]]
#include <Rcpp.h>
#include "sf_external.h"
using namespace Rcpp;

// [[Rcpp::export]]
SEXP sf_alternate_case(SEXP x) {
  RStringIndexer r(x);
  size_t len = r.size();
  SEXP output = PROTECT(sf_vector(len));
  sf_vec_data & output_data = sf_vec_data_ref(output);
  size_t i = 0;
  for(auto e : r) {
    if(e.ptr == nullptr) {
      i++; // increment output index
      continue;
    }
    std::string temp(e.len, \'\\0\');
    bool case_switch = false;
    for(int j=0; j<e.len; j++) {
      if((e.ptr[j] >= 65) & (e.ptr[j] <= 90)) {
        if((case_switch = !case_switch)) {
          temp[j] = e.ptr[j] + 32;
          continue;
        }
      } else if((e.ptr[j] >= 97) & (e.ptr[j] <= 122)) {
        if(!(case_switch = !case_switch)) {
          temp[j] = e.ptr[j] - 32;
          continue;
        }
      } else if(e.ptr[j] == 32) {
        case_switch = false;
      }
      temp[j] = e.ptr[j];
    }
    output_data[i] = sfstring(temp, e.enc);
    i++;
  }
  UNPROTECT(1);
  return output;
}
'


altrep_support <- function() {
  getRversion() >= "3.5"
}

is_solaris <- function() {
  grepl('SunOS',Sys.info()['sysname'])
}

myfile <- tempfile()
print(myfile)

# i500_utf8 <- readRDS("/tmp/temp.rds")
# i500_latin1 <- iconv(i500_utf8, "UTF-8", "latin1")

# For a test set, we are using the 500 most common Icelandic words
# This is a pretty good test set because all Icelandic words can be encoded as either UTF-8 or latin1. It also contains a mix of ASCII and non-ASCII strings
# https://www.101languages.net/french/most-common-french-words/
# https://en.wikipedia.org/wiki/ISO/IEC_8859-1
# qs2::encode_source(x[1:500])
i500_utf8 <- qs2::decode_source(c("unjXVBZLQAAAAAAAAANJ.Lm|T)SQ3YIAC'><F{v83IWFDt]4*/oIi6~[`R}I$}pqGVJ;WLP99km8A4v,_6v+,&|My=(t'>5@U2;1QD+}LB,J~V83BA1(vAL^", 
                                  "n9+YYp&Gt]C(_l@8X~K8A7lk_*?oh7*I?x4*WC[}E0NeD(J(['@Vdd#1}0T,Z]+?s66lC2yo^{0So6&EHO.kixF7iLXhQ,AASd=e1[cIp$&=q.Jh0r[*>;Jl", 
                                  "'_=z?mMdr`OS[ag~Ow#Vvv,:`63Z]npDs%[@,Jd9?`sQ~`B4T`(/*]m2ZkcGq]{TzUTY>E%*T'Oa8(W>Mt4E7KM{+xtzc)izEA^y%Z!WfGDX&Tnz<IIYKC%Z", 
                                  "PRAAoz)A|9htmo36Kc.//f.x~r3[ZYSf``;a1c}oX0+K:5rLA;*iKn}_!q42b$a2Z{+lE[U5]0~Qqw655.B[@@`@ni2cw>N9UJAblw>@woH_u;k/aFIs'bqP", 
                                  "MpclH1U{0T1w4xxl&y}6P+!W;D`z71A7?mr:FsXZdQ@,/;9/BEe[,e91!plP_H=d5iqGIGZe<dekTJIxAr{c!`PSdN6LW*Kg1m3HVgbM.=596bRN&fo7P1?m", 
                                  "~7Jaj]yCZ7y.>G_bIr7`/;bP@s$&kyv[)S|kR/=L?*k/z1>XjP7s4#mtx*tvLK3Dps&So|%lR{a3xbkRr0FPH3hXOD;IA7A(9W1/A/m&;1B?i50nftGQR)fc", 
                                  "5J{EoNDdC!uHQ?>SP+k}+:=Ro2je^@nyrdzv50GGb[zlw*XiY$O{lxy)S]`*o=R5ix2J|1!V79;d^]Mr.?+wGP1_?iY{[le$b_Xd^+Lp0t>iOhs#qI]kT{AJ", 
                                  "D;,Tbd#wm:Ffv<Wi!Yv0s:Mrpsnuvsrq72`hFh&/H^d/N]LyJ#;P@ar.YlI0BgMoppTV>wV,PCDIxsb04xKa_oqbe@j;+:%c`NWmWNa<|d4*X3*X{fg@=bTK", 
                                  "e1ZvEOTeiM<j~|M4Rb3=xu.UB7ywzp75wZjy%f@D_*;5741JZ]a0<kw5z]zfpC?Q/=<V.401=`vO87xRnd&y&)]#RO^fw!xXoR_*~o)SSMP<kgNS=q]wc`ld", 
                                  "1A*0]dx%2c?Z:~jL)F6FCKgL7:V(%FmR>>g}>]:&xZ?>KgkCx>MTk97K,%yBjtl?+r_[8YC6h=6y,KAe<[|$_5JB<B;Q[%T)JOIFFop_,J}mHqg&yc&|V;NJ", 
                                  "N(%q!)WaxqW8(qP.{0p5STxfF/%GsRa`zm7#n8t)ox~,n5V)%(@eNoJ3mM<W.C>|O*cc7awccdo}x<cs(9E>rF!.!SQW*r;DL5Oi5x;]X<PcoNsjK!p9kX:j", 
                                  "^cAQvbxHiM]S'=p~g;0W5zB|$47Y;DaSmDv`x|i%?kOb=#yQviVWvj2@3RB*K}&9`}FOWbyqe[BAQV`UP4@Mbg!=v])siLslo(Cc|5Z3)6CV:ilItRbV,+!]", 
                                  ":$dCm%}f'j^$J&|ajl1urW/n1*zG',x|{pig![g:x(oTSsXih5p&Pn/C!FiN,H~[+]`rL:.]0{Ita,9p5aj?e%lKnhV:l@hH|$or'cK}hJdnk}wKx&|5fL/:", 
                                  "i5;@bfeFyP+ratXZzOA,a/]wN{UbLZNa.MNZO&L6w.jI!K@@1a&OR%yWJU02vm?u|S{asF=#+[3L,|sCHEkIviGnTi=8`c93G6.9G,|?hx;A$:HN~)aVb.6}", 
                                  "~4qrJ<pxsG,L|Zf2n+E?#|jz)0wJh+'8b!ROYD%[vq?=^r#@K'Hn#T&n*1d;3$uiQn@7(d/|lFC5iQ&7GF5fq~r15A?*wkwq!BC*Q[/E8HtITa5GnwYRoM<+", 
                                  "y(TXaH'O8.>6SN0_+;,8n_*hZ,[>X1_J`lRS_O6shEh_=w)'_Etb!HXFMtLaA@*K:>LW|rjBv!@6C~npr!Y#NnF.y['xEKdrriPlf7|x[n33ArMP&Zp@Wg`M", 
                                  "jv}VMV1v(W,{!%h~n=&.5mM`cr%CBREl(TyX2cR>Tf8X?t44rUF{.3s}16_itB#xgaNwtZ)>X1C3S#'#@+p=J*_*2LK00GR$BM(.ikgxZGqj]{JxO1c_DRuL", 
                                  "d^N<8(LR{WaUlJW,7}o1z1LG#E)cy!cVI`+pahs]}^Lht+~tDjuE;F}2do6S2^+9B]Ktih[uo?*qWbK$zcTgmi@OL.~hQJE|nrql(_(BqUZxqPz?ZYi+lK6~", 
                                  "kqgAh$}/4@V!82GKAZy3MU6@Qm*thE|oZr)ql]VGSR&+]T+c<C&eI;Rr@Zm,sgWx;}ZF>lIi['`RDsoL0>)P/sWX'G{u0wY>yn@QWL[_)%ZAW.{gK%(l7?)9", 
                                  "9#J0C]@M'X~^.<l2cx0Fq1)EcH+Cnr8FU!cLj'?a7{?m:Uu=tQxo/goUZ{fskj^=U@YXcH{q}TqpQOmjQ6~l1qb;s0]XrEPfFVb3EOu7DFd.Or/X.tqc+JDk", 
                                  ":hLovNjO25v5eBW4Ft$hESt&_zIqNic`lk(dPSWrz#q((B(DW@I_1wP5_$*6ZBxr9LgTn#C*P:H3Vn&wLT6D&PDw?u3LO,K&P:&B*B=z?I#Si{1.hD=L+%&s", 
                                  "{}wn6w8>cd1FJ`<GCzg+eW3[Z[[2DvEL]QgOCslM6;YnyoP$]gr&n^#u1yaP/84i^hv]{TF+~OB^GZ|,;P6{o7Ux.bxvSA$PaNspVRL**z#GW_:xRdaP"))
i500_latin1 <- iconv(i500_utf8, "UTF-8", "latin1")

utf8_chars <- i500_utf8 %>% strsplit("") %>% unlist %>% unique
latin1_chars <- iconv(utf8_chars, from="UTF-8", to="latin1")

catn <- function(...) {
  cat(..., "\n")
}
ntests <- 50

use_tbb <- stringfish:::is_tbb()
cat("using thread building blocks?", use_tbb, "\n")
if(use_tbb) {
  nthreads <- c(1,4)
} else {
  nthreads <- 1
}

print(sessionInfo())
print(utils::localeToCharset())

if(!altrep_support()) {
  cat("ALT-REP not supported (R < 3.5)")
} else {
  if(!is_solaris()) {
    R_TESTS <- Sys.getenv("R_TESTS") # startup.Rs
    if (nzchar(R_TESTS)) {
      R_TESTS_absolute <- normalizePath(R_TESTS)
      Sys.setenv(R_TESTS = R_TESTS_absolute)
    }
    sourceCpp(code = alternate_case_source)
    if (nzchar(R_TESTS)) Sys.setenv(R_TESTS = R_TESTS)
  }
  for(.j in 1:4) {
    cat("iteration", .j, "\n")
    if(.j %% 2 == 0) {
      stringfish:::set_is_utf8_locale()
    } else {
      stringfish:::unset_is_utf8_locale()
    }
    for(nt in nthreads) {
      cat("number of threads", nt, "\n")

      catn("test altrep serialization (base::serialize)")
      x <- convert_to_sf(sample(c(i500_utf8, i500_latin1), size = length(i500_utf8) + length(i500_latin1), replace=TRUE))
      y <- unserialize(serialize(x, NULL))
      stopifnot(string_identical(x,y))
      stopifnot(get_string_type(y) == "stringfish vector")

      catn("test altrep serialization (qs2::qd_serialize)")
      x <- convert_to_sf(sample(i500_utf8, size = length(i500_utf8), replace=TRUE))
      y <- qs2::qd_deserialize(qs2::qd_serialize(x), use_alt_rep = TRUE)
      stopifnot(string_identical(x,y))
      stopifnot(get_string_type(y) == "stringfish vector")

      catn("sf_assign")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        x <- sf_vector(10)
        y <- character(10)
        for(i in 1:10) {
          new_str <- sample(c(i500_utf8,i500_latin1),1)
          if(.j %% 2 == 1) materialize(x)
          stringfish:::sf_assign(x, i, new_str)
          y[i] <- new_str
        }
        stopifnot(string_identical(x,y))
      }
      
      catn("sf_iconv")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        x <- sf_iconv(i500_latin1, "latin1", "UTF-8")
        if(.j %% 2 == 1) materialize(x)
        y <- sf_iconv(i500_utf8, "UTF-8", "latin1")
        stopifnot(string_identical(x, i500_utf8))
        stopifnot(string_identical(y, i500_latin1))
        x <- sf_iconv(convert_to_sf(i500_latin1), "latin1", "UTF-8")
        y <- sf_iconv(convert_to_sf(i500_utf8), "UTF-8", "latin1")
        stopifnot(string_identical(x, i500_utf8))
        stopifnot(string_identical(y, i500_latin1))
      }
      
      catn("sf_nchar")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        x <- convert_to_sf(i500_latin1)
        if(.j %% 2 == 1) materialize(x)
        y <- convert_to_sf(i500_utf8)
        stopifnot( identical(sf_nchar(x, nthreads = nt), nchar(x)) )
        stopifnot( identical(sf_nchar(y, nthreads = nt), nchar(y)) )
        stopifnot( identical(sf_nchar(x, nthreads = nt), nchar(i500_latin1)) )
        stopifnot( identical(sf_nchar(y, nthreads = nt), nchar(i500_utf8)) )
        stopifnot( identical(sf_nchar(x, type = "bytes", nthreads = nt), nchar(i500_latin1, type = "bytes")) )
        stopifnot( identical(sf_nchar(y, type = "bytes", nthreads = nt), nchar(i500_utf8, type = "bytes")) )
      }
      
      catn("sf_substr")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        start <- sample(-10:10, size=1)
        if(start < 0) {
          rstart <- sf_nchar(i500_latin1, nthreads = nt) + start + 1
        } else {
          rstart <- start
        }
        stop <- sample(-10:10, size=1)
        if(stop < 0) {
          rstop <- sf_nchar(i500_latin1, nthreads = nt) + stop + 1
        } else {
          rstop <- stop
        }
        x <- sf_substr(i500_latin1, start, stop, nthreads = nt)
        if(.j %% 2 == 1) materialize(x)
        y <- substr(i500_latin1, rstart, rstop)
        x2 <- sf_substr(i500_utf8, start, stop, nthreads = nt)
        y2 <- substr(i500_utf8, rstart, rstop)
        stopifnot(string_identical(x, y))
        stopifnot(string_identical(x2, y2))
      }
      
      catn("sf_collapse")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        x <- sf_collapse(i500_latin1, collapse = ":::")
        if(.j %% 2 == 1) materialize(x)
        y <- paste0(i500_latin1, collapse = ":::")
        # stopifnot(string_identical(x, y)) # paste0 converts to UTF-8 -- doesn't respect encoding
        stopifnot(x == y)
        x <- sf_collapse(i500_latin1, collapse = ",")
        if(.j %% 2 == 1) materialize(x)
        y <- paste0(i500_latin1, collapse = ",")
        stopifnot(x == y)
        x <- sf_collapse(i500_utf8, collapse = ":::")
        if(.j %% 2 == 1) materialize(x)
        y <- paste0(i500_utf8, collapse = ":::")
        stopifnot(x == y)
        x <- sf_collapse(i500_utf8, collapse = ",")
        if(.j %% 2 == 1) materialize(x)
        y <- paste0(i500_utf8, collapse = ",")
        stopifnot(x == y)
      }
      
      catn("sf_paste")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        x <- do.call(paste, c(as.list(i500_latin1), sep=":::"))
        y <- do.call(sf_paste, c(as.list(i500_latin1), sep=":::", nthreads = nt))
        if(.j %% 2 == 1) materialize(y)
        stopifnot(x == y)
        x <- do.call(paste, c(as.list(i500_latin1), sep=":::"))
        y <- do.call(sf_paste, c(as.list(i500_latin1), sep=":::", nthreads = nt))
        if(.j %% 2 == 1) materialize(y)
        stopifnot(x == y)
        x <- do.call(paste, c(as.list(i500_utf8), sep=","))
        y <- do.call(sf_paste, c(as.list(i500_utf8), sep=",", nthreads = nt))
        if(.j %% 2 == 1) materialize(y)
        stopifnot(x == y)
        x <- do.call(paste, c(as.list(i500_utf8), sep=","))
        y <- do.call(sf_paste, c(as.list(i500_utf8), sep=",", nthreads = nt))
        if(.j %% 2 == 1) materialize(y)
        stopifnot(x == y)
      }
      
      catn("sf_readLines")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        writeLines(i500_utf8, con=myfile, useBytes=T)
        x <- sf_readLines(myfile, encoding = "UTF-8")
        if(.j %% 2 == 1) materialize(x)
        y <- readLines(myfile); Encoding(y) <- "UTF-8"
        stopifnot(string_identical(x, y))
        writeLines(i500_latin1, con=myfile)
        x <- sf_readLines(myfile, encoding = "latin1")
        if(.j %% 2 == 1) materialize(x)
        y <- readLines(myfile); Encoding(y) <- "latin1"
        stopifnot(string_identical(x, y))
      }
      
      catn("sf_grepl")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        p <- rawToChar(as.raw(c(0x5e, 0xc3, 0xb6, 0x2e, 0x2b)))
        Encoding(p) <- "UTF-8"
        p2 <- rawToChar(as.raw(c(0x5e, 0xf6, 0x2e, 0x2b)))
        Encoding(p2) <- "latin1"
        stopifnot(all(sf_grepl(i500_utf8, p, nthreads = nt) == grepl(p, i500_utf8)))
        stopifnot(all(sf_grepl(i500_latin1, p2, nthreads = nt) == grepl(p2, i500_latin1)))
        
        stopifnot(sf_grepl(i500_utf8, "[a-f]", nthreads = nt) == grepl("[a-f]", i500_utf8))
        stopifnot(sf_grepl(i500_latin1, "[a-f]", nthreads = nt) == grepl("[a-f]", i500_latin1))
      }
      
      catn("sf_grepl fixed")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        p <- rawToChar(as.raw(c(0xc3, 0xb6)))
        Encoding(p) <- "UTF-8"
        p2 <- rawToChar(as.raw(c(0xf6)))
        Encoding(p2) <- "latin1"
        stopifnot(all(sf_grepl(i500_utf8, p, fixed = T, nthreads = nt) == grepl(p, i500_utf8)))
        stopifnot(all(sf_grepl(i500_latin1, p2, fixed = T,  nthreads = nt) == grepl(p2, i500_latin1)))
        
        stopifnot(sf_grepl(i500_utf8, "[a-f]", nthreads = nt) == grepl("[a-f]", i500_utf8))
        stopifnot(sf_grepl(i500_latin1, "[a-f]", nthreads = nt) == grepl("[a-f]", i500_latin1))
      }
      
      catn("sf_gsub")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        p <- rawToChar(as.raw(c(0x5e, 0xc3, 0xb6, 0x2e, 0x2b, 0x28, 0x2e, 0x29, 0x24)))
        Encoding(p) <- "UTF-8"
        p2 <- rawToChar(as.raw(c(0x5e, 0xf6, 0x2e, 0x2b, 0x28, 0x2e, 0x29, 0x24)))
        Encoding(p2) <- "latin1"
        stopifnot(all(sf_gsub(i500_utf8, p, "$1", nthreads = nt) == gsub(p, "\\1", i500_utf8)))
        stopifnot(all(sf_gsub(i500_latin1, p2, "$1", nthreads = nt) == gsub(p2, "\\1", i500_latin1)))
        
        p <- "^h.+(.)$"
        stopifnot(all(sf_gsub(i500_utf8, p, "$1", nthreads = nt) == gsub(p, "\\1", i500_utf8)))
        stopifnot(all(sf_gsub(i500_latin1, p, "$1", nthreads = nt) == gsub(p, "\\1", i500_latin1)))
      }
      
      catn("sf_split")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        # catn("n = ", .)
        # print("sf_split_1")
        # empty split is a special case
        split <- ""
        x <- sf_split(i500_utf8, split, nthreads = nt)
        y <- stringr::str_split(i500_utf8, split)
        r <- sapply(1:length(y), function(i) {
          string_identical(x[[i]], y[[i]])
        })
        stopifnot(all(r))
        
        # print("sf_split_2")
        # empty subject
        x <- sf_split(rep("", 1e3), "a", nthreads=nt)
        stopifnot(all(x == ""))
        
        # print("sf_split_3")
        # empty subject, empty split
        x <- sf_split(rep("", 1e3), "", nthreads=nt)
        stopifnot(all(x == ""))
        
        # print("sf_split_4")
        # split not in subject
        x <- sf_split(rep("abcde", 1e3), "f", nthreads=nt)
        stopifnot(all(x == "abcde"))
        
        # print("sf_split_5")
        # single character split, including UTF-8
        split <- sf_paste(sample(utf8_chars,1))
        x <- sf_split(i500_utf8, split, nthreads = nt)
        y <- stringr::str_split(i500_utf8, split)
        r <- sapply(1:length(y), function(i) {
          string_identical(x[[i]], y[[i]])
        })
        stopifnot(all(r))
        
        # print("sf_split_6")
        # split with regex
        split <- sf_paste(sample(utf8_chars,1), ".")
        x <- sf_split(i500_utf8, split, nthreads = nt)
        y <- stringr::str_split(i500_utf8, split)
        r <- sapply(1:length(y), function(i) {
          string_identical(x[[i]], y[[i]])
        })
        stopifnot(all(r))
        
        # print("sf_split_7")
        split <- sf_paste(sample(utf8_chars,1), ".")
        split_latin1 <- sf_iconv(split, from = "UTF-8", to = "latin1")
        x <- sf_split(i500_latin1, split_latin1, nthreads = nt)
        y <- stringr::str_split(i500_latin1, split_latin1)
        x <- lapply(x, sf_iconv, from = "UTF-8", to = "latin1")
        y <- lapply(y, iconv, from = "UTF-8", to = "latin1")
        r <- sapply(1:length(y), function(i) {
          string_identical(x[[i]], y[[i]])
        })
        stopifnot(all(r))
        
        # print("sf_split_8")
        split_latin1 <- sf_iconv(split, from = "UTF-8", to = "latin1")
        x <- sf_split(i500_latin1, split_latin1, encode_mode = "byte", nthreads = nt)
        y <- stringr::str_split(i500_latin1, split_latin1)
        y <- lapply(y, iconv, from = "UTF-8", to = "latin1")
        r <- sapply(1:length(y), function(i) {
          string_identical(x[[i]], y[[i]])
        })
        stopifnot(all(r))
      }
      
      catn("sf_toupper and sf_tolower")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        x1 <- sf_toupper(i500_latin1)
        x2 <- sf_toupper(i500_utf8)
        y1 <- sf_tolower(i500_latin1)
        if(.j %% 2 == 1) materialize(y1)
        y2 <- sf_tolower(i500_utf8)
        if(.j %% 2 == 1) materialize(y2)
        z1 <- sf_tolower(x1)
        z2 <- sf_tolower(x2)
        stopifnot(string_identical(z1, i500_latin1))
        stopifnot(string_identical(z2, i500_utf8))
        stopifnot(string_identical(y1, i500_latin1))
        stopifnot(string_identical(y2, i500_utf8))
        # base R functions also convert Unicode characters to upper
        # stopifnot(string_identical(x1, iconv(toupper(i500_latin1),"UTF-8", "latin1")))
        # stopifnot(string_identical(x2, toupper(i500_utf8)))
      }
      
      if(!is_solaris()) {
        catn("Rcpp test with sf_alternate_case")
        for(. in 1:ntests) {
          x <- c("hello world", "HELLO WORLD")
          string_identical(sf_alternate_case(x), c("hElLo wOrLd", "hElLo wOrLd"))
        }
        
        catn("sf_trim")
        for(. in 1:ntests) {
          # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
          x <- sf_trim(sf_paste("\t", i500_utf8, " \n"))
          if(.j %% 2 == 1) materialize(x)
          stopifnot(string_identical(x, i500_utf8))
          
          x <- sf_trim(sf_paste("\t", i500_latin1, " \n"), encode_mode = "byte")
          if(.j %% 2 == 1) materialize(x)
          stopifnot(string_identical(x, i500_latin1))
        }
      }
      
      # Disable check due to https://bugs.r-project.org/show_bug.cgi?id=18211
      catn("sf_match")
      # gctorture(TRUE)
      for(. in 1:ntests) {
        i500_utf8_shuffled <- c(NA_character_, i500_utf8[sample(length(i500_utf8))][-1])
        temp <- c(i500_utf8, NA_character_)
        x <- sf_match(temp, i500_utf8_shuffled)
        # y <- match(temp, i500_utf8_shuffled)
        # stopifnot(identical(x,y))
        i500_latin1_shuffled <- c(NA_character_, i500_latin1[sample(length(i500_latin1))][-1])
        temp <- c(i500_latin1, NA_character_)
        x <- sf_match(c(i500_latin1, NA_character_), i500_latin1_shuffled)
        # y <- match(temp, i500_latin1_shuffled)
        # stopifnot(identical(x,y))
      }
      # gctorture(FALSE)
      
      catn("sf_compare")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        i500_utf8_shuffled <- i500_utf8
        i500_utf8_shuffled[sample(length(i500_utf8), size = 100)] <- ""
        x <- sf_compare(c(i500_utf8, NA_character_), c(i500_utf8_shuffled, NA_character_))
        y <- c(i500_utf8, NA_character_) == c(i500_utf8_shuffled, NA_character_)
        stopifnot(identical(x,y))
        
        i500_latin1_shuffled <- i500_latin1
        i500_latin1_shuffled[sample(length(i500_latin1), size = 100)] <- ""
        x <- sf_compare(c(i500_latin1, NA_character_), c(i500_latin1_shuffled, NA_character_))
        y <- c(i500_latin1, NA_character_) == c(i500_latin1_shuffled, NA_character_)
        stopifnot(identical(x,y))
      }
      
      catn("sf_concat")
      for(. in 1:ntests) {
        # if(. == 1) {gctorture(TRUE)} else {gctorture(FALSE)}
        i500_utf8_shuffled <- i500_utf8
        i500_utf8_shuffled[sample(length(i500_utf8), size = 100)] <- ""
        x <- sfc(sfc(i500_utf8, NA_character_), sfc(i500_utf8_shuffled, NA_character_), character(0))
        if(.j %% 2 == 1) materialize(x)
        y <- sfc(sfc(sf_convert(i500_utf8), NA_character_), sfc(sf_convert(i500_utf8_shuffled), NA_character_), character(0))
        z <- c(c(sf_convert(i500_utf8), NA_character_), c(sf_convert(i500_utf8_shuffled), NA_character_), character(0))
        z2 <- c(c(i500_utf8, NA_character_), c(i500_utf8_shuffled, NA_character_), character(0))
        stopifnot(string_identical(x,y))
        stopifnot(string_identical(x,z))
        stopifnot(string_identical(x,z2))
        stopifnot(string_identical(x,y))
        stopifnot(identical(sfc(character(0)), character(0)))
      }
      print(gc())
    }
  }
}
print("end")

}
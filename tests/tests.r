suppressMessages(library(stringfish, quietly = T))
suppressMessages(library(qs, quietly = T))
suppressMessages(library(dplyr))
suppressMessages(library(Rcpp, quietly = T))

myfile <- tempfile()
# qserialize(readLines("~/GoogleDrive/stringfish/tests/tests.cpp"), preset = "custom", compress_level = 22) %>% base91_encode %>% catquo
qdeserialize(base91_decode('un]"BAAA@QRtHACAAAAAAAY4*AAAv7#aT)WujXRAH#x:&(C@GA*7Phnmjzl;:s3qU>"2Etf<z(=l>C^"X/oWv&e^2CUQT3H7z(lA*_9K2FN8awk&A5{]$,k=`+eNCh$#^5Q*}C"f6s]3.&l9+?L$ZOwXDcewq*Ot.ot;j0L+R%(.K[.&&cI3S}i>.^"J%GRhNYL]Fv<E_~++zWrdZPZI.]s$hOu#^6/E=k;(A3sQ)X=k#Fg/XzFJ[u]_4(4x=jc*z<XH.H+Jb}Y0*dwq1/3T@uJWdmNK/N5qZKTI^|H>$c)&3]ab#,@_fS]|/*`<?|3l$,V+Q|%@}58}[54/25r3Z:Y+srvSJw#[~({|SRkrW%nb/H6je^0qMS{}H`Ngey3dlE?u!c%L{$^v1Wy|mdFKWX{p]kT2./ozv<_nDoz.hkX>40H|6Tz^DP$+;o.$49Eo+i%;=g|Ks[n`"uj6MJuHt.=98RFV/Fk9^4XH"~qfpETtIu}Vm2A"B21JHdi0%2A[R,/MG4Tdp~)B{$@&Th8RSV.?V6G[>(Kxl68&B"E~bNY&xhS0Zs"xa%zE@~^rq9l]B!2_,s>d%:wP;1+H#KLSafoRg%8Tvt@0,wT_v/_n8R=.WVH}]oG&p@1|{F3oE#x!V~:_;2yI2TdzL%~CnMC/GI:as8{(6@0vl~4ivK`*bEzBA2CeNzk?4`X`t2"dqxMmDH8y{ONXLSXZ3W,7bv.H)Oiflr;u_apCApb&x^g`/J!UiV]fDM`jnS=j>yv)>~abGRJ*gP:CYL]tLV"vRd]&>7>D;$V(Fb,M;j.V)PCi,:@.cDmWLly$b{Y_9|ye_v.QT&x9F?1hlq7dX`6X4x1dZPK1n~*!bHJ#)%DZDAaITG)L:LJ.pNsAnGW""FUE1X1CR@K.}z{CTto48LWhSK8ozX85&6VFnW$G3t1xInMjv@|e99U#jQWUIXUYaS$Olipt`[Dm*4ms_Ws6CB|jkpZ]mv~~J<"=nNwHwisOnf#Kq9xkbOkD^$/<M.3UC_&=z9>Kp1X%frR;22h:hM3fCM>krGF:s)23nU?L~V[J=.Mq4KLg&I%&d}Om|sJ/0qFEcxCFe]LhAYgFS*J<]Fi1$qL?w8|vy;H>WSbE[WmTtqNt)DZZ#tOqs1.YI8w_To[1{clMaCdpsH0TUfh*iRq2}46PvbAy`_ynO=3)(7({_]dLG_tD[a[h0vrSPvLgpa&n7]xJV,sJrcEtV*)Uye&kV_s@CB^I0$i$(d$T&e]i`;MBq{i"Nt!YB=,8Iw}n#@bC*F^iX]s{KF+#>wQEN0Tc<ZL5#1X74XsG[WY/(mqj8;,/Dm{),#*HLV0OxWt)=7)7P}d~kMW~aq"w.qu~7m6o,0(|p]S,:.?$KqB:UIl5T$o:Nn5wzj9B5;R@|:}ju6W')) %>% writeLines(con = paste0(myfile, ".cpp"))
sourceCpp(paste0(myfile, ".cpp"))

# For a test set, we are using the 500 most common Icelandic words
# This is a pretty good test set because all Icelandic words can be encoded as either UTF-8 or latin1. It also contains a mix of ASCII and non-ASCII strings
# https://www.101languages.net/french/most-common-french-words/
# https://en.wikipedia.org/wiki/ISO/IEC_8859-1
# qserialize(x[1:500], preset = "custom", compress_level = 22, check_hash=F) %>% base91_encode %>% catquo
i500_utf8 <- qdeserialize(base91_decode('un]"BAAA@QBtHACAAAAAAAY4TBAABdk1kure+B3AwoFYdc"lM?hVjfxq5F^5OXd&}iic*u6aS*TsW0{Ur6@MdueBh+g1us}b/1]FhtGOG"/1xj=x/iL}2,4f#H8v+r_qDxuL3U2jD!9.@z%wUvNyLRYr4SugnxE`=hw2L+NI<X8w<q*MV&?(?V:FE!?+CimfUy<&u/Bk7xer7/u;syo9Cu:qx7J57?*_/6I@)xs[J;fXOK0B_l"[4EKyF#:{7z_>g.MhpqTM[&1_P.E@i&,j(3{?d|qu!g,fl:gUcJ;U#KwdN,_?zyzU~K@dwS}Evq&qWYNl(?xqk$j/L#s#{3^H)BXxo`t["IA+j+JLI*yEkS0<p4a3PV6<~N!f3v7HyE$j@/I@W0d[sy)p&z)?(k>qJoTLq#eE)}t8G:F`@bQJHYvHND{eS"@QH)m6Rgv,|a)@nV=!*%=<jElUsK5k}4,axCwZ~NB(+e|XA1OKDJ^@koTm4YAx#I8oCQOq}fyfqL>?loMXRva2k*4.V&`n?1j~MzQ1J[9t?IH1jV})PLT^,GA0J#8|VB=n9=cy91B.#J$m{SgJ=mk(5x*ml:j%u.(+_Nk8#/@w]C~PyEVk+tlU.iicD2hk^dYc2am&6_@ges.x$lbUZcTidf.IoKPnSUJp!gab|bS61!4U_u&XTPnEo%eUP&0J.OisuVyZ:|}b|Nl1[pZ:.7wOOS+*I`ojDvw6E#rq"){8i7Pt{.auL+`GVyZ&w!NKtp3k09J+K?;<:}#O/9+N#5DYF<{|cy7P_*9GzY(f4<qR$a&EaGeQ.fIo7_o#X?"_M$d[S]3;vJzM8Eg!;x(]~Nzn<d9`rCeJwn"5zd(npX_UA)yk=F!ue51ac;#>j6wnSlb,/9|fc|Um|^rb7+nr$w0P^x^nKZvO{Sg%@8{?VwNQgF<+cb@)Tf$3Zu(Oou,fN=EpfnqLI{s3$sp62;L!~x_*]5}`cIwJKDXWgY,/09Ft*;xn2,0KDrz/F.S[:)YJ:vsn{?EgMh`)#ss)1OiU9uAdZ6|bH+m6+x]?y;pq7ralfX;>Y?_h<YRdh7we_WP0{8|ft_oAbTodGh.cT9j&~5EgOVX.njZtoMu>f?0f~>9W0Y?K4+t#nNS(qOKox0"x>Tv|WP4vw|~XTlA]=SNJXdDpQdJbv*PL&8yGGEI%2$kqO1S)1i6?BmI,fTJ:=it_P,2.Mb/DeHFKMrTyB&oUjmpJ/SM^/A<{>g1HTVevQ7MeXcr)st{>AD;OQ)5Vwst^1xwdMa?g>?|yFSh9za`XtRat<xL@Pe7boGp6TaNQbu:bPFO7^S}CuzeR~YN5/_r04C:DOQ;KV?"xbkeU%OE814d~wg;FngvPgD$MCo"Oy|V)?6UXQJ!jPK0VqLjoCo9o[nxCOw"S3?;x$(24kGK,q+IkGAgAuW3L{C+WvL;^7tTH9C|2ID"V*3"az8.U&4{RW)2E5(NcJ#q=`[a7XzsUi;FJMv;G]Z@(mj4"_^5)il{eTT"/`r8)QuwW]Wlzm.XMI|$W~$OZ<L6>hV*2M.F(!o/e?&CEF>PK)z:prsTfugRCp(=%K!%hu.BL;*5tkY60XaMxD7rTc^rGIM#0,c*OD$^?,*2vlB%p11+;_gq]CPyXF4P:F#|WW}6nH,_z~l7bz[1n:m|]Nh[>k)A6Y).!U>w"AC}z1EB@AQ)qsB)oX2+N<a&e+@uCjenbJDZ~kqY7J)i7*+y/4B:pR/MbS{|Pgoi<4yzrymz2u(Hqg(`GNdLS{674]i^NIIHz$MVzw/&iJbn^4r0$)4G_<"A:yq<41d^+l/D^11}vs#LY|)1ghwz.6@&qE0!E[GCK[!"HWZxw1{N%IM2[W7X.A&L)u=E;GjSKo;l&}7W/*g<;E,tv[ZFzXCzd47&afxl]96H/Z?[i&YvmbxiMJeCG^Aaa|?j%`2*,@i#1#W!xfvG@/h+Fi{oGa;|c51I8C8wd:HDBosoF)n$IHioJa`RX.b{/KMM{y~N>}w]SjR<!KXER}h`e]WqHRV23+V;}SIx=N<#*xddZ;%wo?L|HnnOppxwc_[Lh.DG+`X4Nc1v{4;0]5$ufCd$vk}b+eLW,PsPc8E$GKSG$R:VznhU$t,kTg+?f"Z<R~:M=B9igR+DVzULgg8$!2^~T"RcU9:ln<8Jd4i$50@^s1{R9lVwJ^Pll*t>TN!=Lee,!77)G){k&p"C)_}`H&B|^B'))
i500_latin1 <- iconv(i500_utf8, "UTF-8", "latin1")

str_identical <- function(x, y) {
  if(length(x) != length(y)) return(F)
  if(any(nchar(x) != nchar(y))) return(F)
  if(!all(Encoding(x) == Encoding(y))) return(F)
  for(i in 1:length(x)) {
    if(any(charToRaw(x[i]) != charToRaw(y[i]))) return(F)
  }
  return(T)
}
catn <- function(...) {
  cat(..., "\n")
}
ntests <- 10

catn("sf_assign")
for(. in 1:ntests) {
  x <- sf_vector(10)
  y <- character(10)
  for(i in 1:10) {
    new_str <- sample(c(i500_utf8,i500_latin1),1)
    stringfish:::sf_assign(x, i, new_str)
    y[i] <- new_str
  }
  stopifnot(str_identical(x,y))
}

catn("sf_iconv")
for(. in 1:ntests) {
  x <- sf_iconv(i500_latin1, "latin1", "UTF-8")
  y <- sf_iconv(i500_utf8, "UTF-8", "latin1")
  stopifnot(str_identical(x, i500_utf8))
  stopifnot(str_identical(y, i500_latin1))
  x <- sf_iconv(convert_to_sf(i500_latin1), "latin1", "UTF-8")
  y <- sf_iconv(convert_to_sf(i500_utf8), "UTF-8", "latin1")
  stopifnot(str_identical(x, i500_utf8))
  stopifnot(str_identical(y, i500_latin1))
}

catn("sf_nchar")
for(. in 1:ntests) {
  x <- convert_to_sf(i500_latin1)
  y <- convert_to_sf(i500_utf8)
  stopifnot( identical(sf_nchar(x), nchar(x)) )
  stopifnot( identical(sf_nchar(y), nchar(y)) )
  stopifnot( identical(sf_nchar(x), nchar(i500_latin1)) )
  stopifnot( identical(sf_nchar(y), nchar(i500_utf8)) )
  stopifnot( identical(sf_nchar(x, type = "bytes"), nchar(i500_latin1, type = "bytes")) )
  stopifnot( identical(sf_nchar(y, type = "bytes"), nchar(i500_utf8, type = "bytes")) )
}

catn("sf_substr")
for(. in 1:ntests) {
  start <- sample(-10:10, size=1)
  if(start < 0) {
    rstart <- sf_nchar(i500_latin1) + start + 1
  } else {
    rstart <- start
  }
  stop <- sample(-10:10, size=1)
  if(stop < 0) {
    rstop <- sf_nchar(i500_latin1) + stop + 1
  } else {
    rstop <- stop
  }
  x <- sf_substr(i500_latin1, start, stop)
  y <- substr(i500_latin1, rstart, rstop)
  x2 <- sf_substr(i500_utf8, start, stop)
  y2 <- substr(i500_utf8, rstart, rstop)
  stopifnot(str_identical(x, y))
  stopifnot(str_identical(x2, y2))
}

catn("sf_collapse")
for(. in 1:ntests) {
  x <- sf_collapse(i500_latin1, collapse = ":::")
  y <- paste0(i500_latin1, collapse = ":::")
  # stopifnot(str_identical(x, y)) # paste0 converts to UTF-8 -- doesn't respect encoding
  stopifnot(x == y)
  x <- sf_collapse(i500_latin1, collapse = ",")
  y <- paste0(i500_latin1, collapse = ",")
  stopifnot(x == y)
  x <- sf_collapse(i500_utf8, collapse = ":::")
  y <- paste0(i500_utf8, collapse = ":::")
  stopifnot(x == y)
  x <- sf_collapse(i500_utf8, collapse = ",")
  y <- paste0(i500_utf8, collapse = ",")
  stopifnot(x == y)
}

catn("sf_paste")
for(. in 1:ntests) {
  x <- do.call(paste, c(as.list(i500_latin1), sep=":::"))
  y <- do.call(sf_paste, c(as.list(i500_latin1), sep=":::"))
  stopifnot(x == y)
  x <- do.call(paste, c(as.list(i500_latin1), sep=":::"))
  y <- do.call(sf_paste, c(as.list(i500_latin1), sep=":::"))
  stopifnot(x == y)
  x <- do.call(paste, c(as.list(i500_utf8), sep=","))
  y <- do.call(sf_paste, c(as.list(i500_utf8), sep=","))
  stopifnot(x == y)
  x <- do.call(paste, c(as.list(i500_utf8), sep=","))
  y <- do.call(sf_paste, c(as.list(i500_utf8), sep=","))
  stopifnot(x == y)
}

catn("sf_readLines")
for(. in 1:ntests) {
  writeLines(i500_utf8, con=myfile)
  x <- sf_readLines(myfile, encoding = "UTF-8")
  y <- readLines(myfile); Encoding(y) <- "UTF-8"
  stopifnot(str_identical(x, y))
  writeLines(i500_latin1, con=myfile)
  x <- sf_readLines(myfile, encoding = "latin1")
  y <- readLines(myfile); Encoding(y) <- "latin1"
  stopifnot(str_identical(x, y))
}


catn("sf_grepl")
for(. in 1:ntests) {
  p <- rawToChar(as.raw(c(0x5e, 0xc3, 0xb6, 0x2e, 0x2b)))
  Encoding(p) <- "UTF-8"
  p2 <- rawToChar(as.raw(c(0x5e, 0xf6, 0x2e, 0x2b)))
  Encoding(p2) <- "latin1"
  stopifnot(all(sf_grepl(i500_utf8, p) == grepl(p, i500_utf8)))
  stopifnot(all(sf_grepl(i500_latin1, p2) == grepl(p2, i500_latin1)))
  
  stopifnot(sf_grepl(i500_utf8, "[a-f]") == grepl("[a-f]", i500_utf8))
  stopifnot(sf_grepl(i500_latin1, "[a-f]") == grepl("[a-f]", i500_latin1))
}

catn("sf_gsub")
for(. in 1:ntests) {
  p <- rawToChar(as.raw(c(0x5e, 0xc3, 0xb6, 0x2e, 0x2b, 0x28, 0x2e, 0x29, 0x24)))
  Encoding(p) <- "UTF-8"
  p2 <- rawToChar(as.raw(c(0x5e, 0xf6, 0x2e, 0x2b, 0x28, 0x2e, 0x29, 0x24)))
  Encoding(p2) <- "latin1"
  stopifnot(all(sf_gsub(i500_utf8, p, "$1") == gsub(p, "\\1", i500_utf8)))
  stopifnot(all(sf_gsub(i500_latin1, p2, "$1") == gsub(p2, "\\1", i500_latin1)))
}

catn("Rcpp test with sf_alternate_case")
for(. in 1:ntests) {
  x <- c("hello world", "HELLO WORLD")
  str_identical(sf_alternate_case(x), c("hElLo wOrLd", "hElLo wOrLd"))
}
AC_INIT(stringfish, 0.13.2 traversc@gmail.com)
AC_PATH_PROG([PKGCONF],[pkg-config],[],[$PATH:/usr/local/bin:ext/bin:ext:/sw/bin:/opt/bin:/opt/local/bin])

echo "stringfish configure script"
########################################################
### Predefined compile strings for different cases

ADD_LIBS=""
INCLUDE_PATHS=""
LIBPCRE2=""

########################################################
### Configure args

AC_ARG_WITH([pcre2-force-compile],
            AC_HELP_STRING([--with-pcre2-force-compile],
                           [Force compilation of bundled pcre2 source files]),
            [pcre2_force_compile="true"])
            
AC_ARG_WITH([pcre2-include],
            AC_HELP_STRING([--with-pcre2-include=INCLUDE_PATH],
                           [the location of pcre2 header files]),
            [pcre2_include_path=$withval])
            
AC_ARG_WITH([pcre2-lib],
            AC_HELP_STRING([--with-pcre2-lib=LIB_PATH],
                           [the location of pcre2 library files]),
            [pcre2_lib_path=$withval])
            
AC_ARG_WITH([simd],
            AC_HELP_STRING([--with-simd],
                           [Manually select SIMD support (options: AVX2)]),
            [with_simd=$withval])
       

########################################################
#### Version value function

getVersion()
{
VERSION_STRING=$1
MAJOR=`echo $VERSION_STRING | cut -d. -f1`
MINOR=`echo $VERSION_STRING | cut -d. -f2`
echo $(($MAJOR*1000+$MINOR))
}

########################################################
#### Check for GCC version and add -mshstk cflags for GCC 8+
#### ver 0.13.2 -- no longer in use

# echo "Testing for C compiler version"
# echo "R_HOME: $R_HOME"
# : ${R_HOME=`R RHOME`}
# echo "R_HOME: $R_HOME"
# if test -z "${R_HOME}"; then
#   echo "could not determine R_HOME"
#   exit 1
# fi
# CC=`"${R_HOME}/bin/R" CMD config CC`
# echo "C compiler command: $CC"

# AC_LANG(C)
# AX_CHECK_COMPILE_FLAG([-mshstk],[MSHSTK_FLAG_AVAIL=yes])

# if test xx$MSHSTK_FLAG_AVAIL = "xxyes"; then
#   ADD_CFLAGS="${ADD_CFLAGS} -mshstk"
# fi

# AC_LANG(C)
# AX_COMPILER_VENDOR
# AX_COMPILER_VERSION
# echo "C compiler vendor: $ax_cv_c_compiler_vendor"
# echo "C compiler version: $ax_cv_c_compiler_version" # note: The version is completely wrong for Mac LLVM

# if test xx$ax_cv_c_compiler_vendor = "xxgnu"; then
#   CCVER=`${CC} -dumpversion | cut -f 1 -d "."`
#   echo "gcc dumpversion: $CCVER"
#   if test "${CCVER}" -ge 8; then
#     ADD_CFLAGS="${ADD_CFLAGS} -mshstk"
#   fi
# elif test xx$ax_cv_c_compiler_vendor == "xxclang"; then
#     CCVER=`${CC} -dumpversion | cut -f 1 -d "."`
#   echo "clang dumpversion: $CCVER"
#   if test "${CCVER}" -ge 9; then
#     ADD_CFLAGS="${ADD_CFLAGS} -mshstk"
#   fi
# fi


########################################################
#### PCRE2 library paths

if test xx$pcre2_force_compile = "xxtrue"; then
  echo "Compiling PCRE2 from source due to --with-pcre2-force-compile"
  COMPILE_PCRE2="true"
elif test "xx$pcre2_include_path" != "xx"; then
  echo "Using user-defined pcre2 install paths"
    ADD_LIBS="${ADD_LIBS} -L${pcre2_lib_path}"
    INCLUDE_PATHS="${INCLUDE_PATHS} -I${pcre2_include_path}"
    COMPILE_PCRE2="false"
elif test "xx$PKGCONF" != "xx"; then
  if "${PKGCONF}" --exists libpcre2-8; then
    VERSION_STRING=`${PKGCONF} --modversion libpcre2-8`
    VER=`getVersion ${VERSION_STRING}`
    if test "${VER}" -ge 10035; then
      echo "PCRE2 ${VERSION_STRING} dynamic library detected -- skipping PCRE2 compilation"
      pcre2_lib_path=`"${PKGCONF}" --libs libpcre2-8`
      pcre2_include_path=`"${PKGCONF}" --cflags-only-I libpcre2-8`
      ADD_LIBS="${ADD_LIBS} ${pcre2_lib_path}"
      INCLUDE_PATHS="${INCLUDE_PATHS} ${pcre2_include_path}"
      COMPILE_PCRE2="false"
    else
      echo "PCRE2 ${VERSION_STRING} dynamic library detected but is lower than bundled version (10.35) -- compiling from source"
      COMPILE_PCRE2="true"
    fi
  else
    echo "PCRE2 dynamic library not detected -- compiling from source"
    COMPILE_PCRE2="true"
  fi
else
  echo "pkg-confg not detected -- compiling from source"
  COMPILE_PCRE2="true"
fi

if test xx$COMPILE_PCRE2 = "xxtrue"; then
  LIBPCRE2="\$(LIBPCRE2)"
fi

if test xx$with_simd = "xxAVX2"; then
  echo "Using AVX2"
  INCLUDE_PATHS="$INCLUDE_PATHS -mavx2 -msse3 -msse2"
# elif test xx$with_simd = "xxSSE3"; then
#   echo "Using SSE3"
#   INCLUDE_PATHS="$INCLUDE_PATHS -msse3 -msse2"
fi

echo $ADD_LIBS
echo $INCLUDE_PATHS
echo $LIBPCRE2

AC_SUBST([ADD_LIBS], $ADD_LIBS)
AC_SUBST([INCLUDE_PATHS], $INCLUDE_PATHS)
AC_SUBST([LIBPCRE2], $LIBPCRE2)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

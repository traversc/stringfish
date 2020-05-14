AC_INIT(stringfish, 0.11, traversc@gmail.com)
AC_PATH_PROG([PKGCONF],[pkg-config],[],[$PATH:/usr/local/bin:ext/bin:ext:/sw/bin:/opt/bin:/opt/local/bin])

########################################################
### Predefined compile strings for different cases

LIBS="-L."
INCLUDE_PATHS=""
PCRE2_COMPILE_STRING='LIBPCRE2 = PCRE2/pcre2_chartables.o \
	PCRE2/pcre2_auto_possess.o \
	PCRE2/pcre2_compile.o \
	PCRE2/pcre2_config.o \
	PCRE2/pcre2_context.o \
	PCRE2/pcre2_convert.o \
	PCRE2/pcre2_dfa_match.o \
	PCRE2/pcre2_error.o \
	PCRE2/pcre2_extuni.o \
	PCRE2/pcre2_find_bracket.o \
	PCRE2/pcre2_jit_compile.o \
	PCRE2/pcre2_maketables.o \
	PCRE2/pcre2_match.o \
	PCRE2/pcre2_match_data.o \
	PCRE2/pcre2_newline.o \
	PCRE2/pcre2_ord2utf.o \
	PCRE2/pcre2_pattern_info.o \
	PCRE2/pcre2_script_run.o \
	PCRE2/pcre2_serialize.o \
	PCRE2/pcre2_string_utils.o \
	PCRE2/pcre2_study.o \
	PCRE2/pcre2_substitute.o \
	PCRE2/pcre2_substring.o \
	PCRE2/pcre2_tables.o \
	PCRE2/pcre2_ucd.o \
	PCRE2/pcre2_valid_utf.o \
	PCRE2/pcre2_xclass.o

$(SHLIB): libSFPCRE2.a

libSFPCRE2.a: $(LIBPCRE2)
	$(AR) rcs libSFPCRE2.a $(LIBPCRE2)

clean:
	rm -f $(SHLIB) $(OBJECTS) $(LIBPCRE2) libSFPCRE2.a'
	
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
                           [Manually select SIMD support (options: AVX2, SSE2)]),
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
#### Compile PCRE2 checks

if test xx$pcre2_force_compile = "xxtrue"; then
  echo "Compiling PCRE2 from source due to --with-pcre2-force-compile"
  COMPILE_PCRE2="true"
elif test "xx$pcre2_include_path" != "xx"; then
  echo "Using user-defined pcre2 install paths"
    LIBS="${LIBS} -L${pcre2_lib_path}"
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
      LIBS="${LIBS} ${pcre2_lib_path}"
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
  INCLUDE_PATHS="${INCLUDE_PATHS} -DPCRE2_CODE_UNIT_WIDTH=8 -DHAVE_CONFIG_H -IPCRE2"
  COMPILE_STRING=$PCRE2_COMPILE_STRING
  LIBS="${LIBS} -lSFPCRE2"
else
  COMPILE_STRING=""
fi

if test xx$with_simd = "xxAVX2"; then
  echo "Using AVX2"
  INCLUDE_PATHS="$INCLUDE_PATHS -mavx2 -msse2"
elif test xx$with_simd = "xxSSE2"; then
  echo "Using SSE2"
  INCLUDE_PATHS="$INCLUDE_PATHS -msse2"
fi

echo $LIBS
echo $INCLUDE_PATHS
echo $COMPILE_STRING

AC_SUBST([COMPILE_STRING], $COMPILE_STRING)
AC_SUBST([INCLUDE_PATHS], $INCLUDE_PATHS)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

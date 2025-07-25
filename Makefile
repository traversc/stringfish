SHELL   := /bin/bash
PACKAGE := $(shell perl -aF: -ne 'print, exit if s/^Package:\s+//' DESCRIPTION)
VERSION := $(shell perl -aF: -ne 'print, exit if s/^Version:\s+//' DESCRIPTION)
BUILD   := $(PACKAGE)_$(VERSION).tar.gz

RHUB_ALL_PLATFORMS := c( \
  "linux",      "m1-san",      "macos",        "macos-arm64",  "windows", \
  "atlas",      "c23",         "clang-asan",   "clang-ubsan",  "clang16", \
  "clang17",    "clang18",     "clang19",      "clang20",      "donttest", \
  "gcc-asan",   "gcc13",       "gcc14",        "gcc15",        "intel", \
  "mkl",        "nold",        "noremap",      "nosuggests",   "rchk", \
  "ubuntu-clang","ubuntu-gcc12","ubuntu-next", "ubuntu-release","valgrind" \
)

.PHONY: doc build install test vignette $(BUILD)

check: $(BUILD)
	R CMD check --as-cran $<

check-no-vignette: $(BUILD)
	R CMD check --as-cran --no-build-vignettes $<

check-rhub: $(BUILD)
	Rscript -e 'rhub::rhub_check(platform = $(RHUB_ALL_PLATFORMS))'

compile:
	find src/ -type f -exec chmod 644 {} \;
	Rscript -e "library(Rcpp); compileAttributes('.');"
	# Rscript -e "devtools::load_all(); roxygen2::roxygenise('.');"
	find . -iname "*.a" -exec rm {} \;
	find . -iname "*.o" -exec rm {} \;
	find . -iname "*.so" -exec rm {} \;

build:
	autoconf
	chmod 755 cleanup
	chmod 755 configure
	find src/ -type f -exec chmod 644 {} \;
	chmod 644 ChangeLog DESCRIPTION Makefile NAMESPACE README.md
	./configure
	./cleanup
	Rscript -e "library(Rcpp); compileAttributes('.');"
	Rscript -e "devtools::load_all(); roxygen2::roxygenise('.');"
	# rm -f R/RcppExports.R
	find . -iname "*.a" -exec rm {} \;
	find . -iname "*.o" -exec rm {} \;
	find . -iname "*.so" -exec rm {} \;
	R CMD build .

install:
	autoconf
	chmod 755 cleanup
	chmod 755 configure
	find src/ -type f -exec chmod 644 {} \;
	chmod 644 ChangeLog DESCRIPTION Makefile NAMESPACE README.md
	./configure
	./cleanup
	Rscript -e "library(Rcpp); compileAttributes('.');"
	Rscript -e "devtools::load_all(); roxygen2::roxygenise('.');"
	# rm -f R/RcppExports.R
	find . -iname "*.a" -exec rm {} \;
	find . -iname "*.o" -exec rm {} \;
	find . -iname "*.so" -exec rm {} \;
	R CMD build . # --no-build-vignettes
	R CMD INSTALL $(BUILD) --configure-args="--with-simd=AVX2" # --with-pcre2-force-compile"

vignette:
	Rscript -e "rmarkdown::render(input='vignettes/vignette.rmd', output_format='html_vignette')"
	IS_GITHUB=Yes Rscript -e "rmarkdown::render(input='vignettes/vignette.rmd', output_file='../README.md', output_format=rmarkdown::github_document(html_preview=FALSE))"; unset IS_GITHUB

test:
	Rscript tests/tests.R
	Rscript inst/extra_tests/benchmark_test.R 5


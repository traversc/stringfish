SHELL   := /bin/bash
PACKAGE := $(shell perl -aF: -ne 'print, exit if s/^Package:\s+//' DESCRIPTION)
VERSION := $(shell perl -aF: -ne 'print, exit if s/^Version:\s+//' DESCRIPTION)
BUILD   := $(PACKAGE)_$(VERSION).tar.gz

.PHONY: doc build install test vignette $(BUILD)

check: $(BUILD)
	R CMD check --as-cran $<

check-no-vignette: $(BUILD)
	R CMD check --as-cran --no-build-vignettes $<

check-rhub: $(BUILD)
	Rscript -e 'rhub::check("$(BUILD)", platform = c("ubuntu-gcc-devel", "windows-x86_64-devel", "solaris-x86-patched", "macos-m1-bigsur-release"))'

check-solaris: $(BUILD)
	Rscript -e 'rhub::check("$(BUILD)", platform = c("solaris-x86-patched"))'
	
check-m1: $(BUILD)
	Rscript -e 'rhub::check("$(BUILD)", platform = c("macos-m1-bigsur-release"))'

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
	rm -f R/RcppExports.R
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
	rm -f R/RcppExports.R
	find . -iname "*.a" -exec rm {} \;
	find . -iname "*.o" -exec rm {} \;
	find . -iname "*.so" -exec rm {} \;
	R CMD build . # --no-build-vignettes
	R CMD INSTALL $(BUILD) --configure-args="--with-simd=AVX2 --with-pcre2-force-compile"

vignette:
	Rscript -e "rmarkdown::render(input='vignettes/vignette.rmd', output_format='all')"
	mv vignettes/vignette.md README.md
	sed -r -i 's/\((.+)\.png/\(vignettes\/\1\.png/' README.md

test:
	Rscript tests/tests.r
	Rscript inst/extra_tests/benchmark_test.r 5


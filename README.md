stringfish
================

<!-- <img src="hex.png" width = "130" height = "150" align="right" style="border:0px;padding:15px"> -->

[![Build
Status](https://travis-ci.org/traversc/stringfish.svg)](https://travis-ci.org/traversc/stringfish)

`stringfish` is a framework for performing string and sequence
operations using the `alt-rep` system to speed up the comptuation of
common string operations.

The ultimate goal of the package is to unify `alt-rep` string
implementations under a common framework.

The `alt-rep` system (new as of R 3.5.0) allows package developers to
represent R objects using their own custom memory layout, completely
invisible to the user. `stringfish` represents string data as a simple
C++/STL vector, which is very fast and lightweight.

Using normal R functions to process string data (e.g. `substr`, `gsub`,
`paste`, etc.) causes “materialization” of `alt-rep` vectors to normal R
data, which can be a slow process. Therefore, in order to take full
advantage of the `alt-rep` framework, string processing functions need
to be re-written to be `alt-rep` aware. This package hopes to fulfill
that purpose.

## Example

Below is an example that imitates common bioinformatic processing.
First, we read in a bunch of sequencing data (in this case, 1 million
randomly generated amino acid sequences) and then uses `sf_substr` to
chop off two amino acids at the beginning and end of each sequence. The
last operation then uses `sf_grepl` to count the number of “RS” motifs,
a common T-cell Receptor *Influenza* binding motif.

``` r
library(stringfish)
x <- sf_random_strings(1e6, string_size = 20, charset = "ARNDCQEGHILKMFPSTWYV")
x1 <- sf_substr(x, 3, -3)
s1 <- sum(sf_grepl(x, "RS", encode_mode = "byte"))
```

Doing the same thing in base R for
comparison:

``` r
x <- sf_random_strings(1e6, string_size = 20, charset = "ARNDCQEGHILKMFPSTWYV", mode = "normal")
x0 <- substr(x, 3, 18)
s0 <- sum(grepl("RS", x))
```

## Benchmark

A quick benchmark comparing the above example done with `stringfish` or
base R.

![](vignettes/bench_v1.png "bench_v1")

## Installation:

``` r
remotes::install_github("traversc/stringfish")
```

## Currently implemented functions

A list of implemented `stringfish` function and analogous base R
function:

  - `sf_iconv` (`iconv`)
  - `sf_nchar` (`nchar`)
  - `sf_substr` (`substr`)
  - `sf_paste` (`paste0`)
  - `sf_collapse` (`paste0`)
  - `sf_readLines` (`readLines`)
  - `sf_grepl` (`grepl`)
  - `sf_gsub` (`gsub`)

Utility functions:

  - `convert_to_sf` – converts a character vector to a `stringfish`
    vector
  - `get_string_type` – determines string type (whether `alt-rep` or
    normal)
  - `materialize` – converts any `alt-rep` object into a normal R object
  - `new_sf_vec` – creates a new and empty `stringfish` vector
  - `sf_random_strings` – creates a random strings as either a
    `stringfish` or normal R vector

`stringfish` functions are not intended to exactly replicate their base
R analogues. One systematic difference is `stringfish` does minimal
encoding checks and no re-encoding. Therefore, to combine `latin1` and
`UTF-8` encoded strings, first use `sf_iconv`. Another difference is
that `subject` parameters are always the first argument, to be easier to
use in pipes (`%>%`). E.g., `gsub(pattern, replacement, subject)`
becomes `gsub(subject ,pattern, replacement)`.

## Extensibility

`stringfish` as a framework is intended to be easily extensible.
Stringfish vectors can be worked into `Rcpp` scripts or even into other
packages (see the `qs` package for an example).

Below is a detailed `Rcpp` script example that creates a function to
altnerate upper and lower case of strings.

``` c
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(stringfish)]]
#include <Rcpp.h>
#include "sf_external.h"
using namespace Rcpp;

// [[Rcpp::export]]
SEXP sf_alternate_case(SEXP x) {
  // Iterate through a character vector using the RStringIndexer class
  // If the input vector x is a stringfish character vector it will do so without materialization
  RStringIndexer r(x);
  size_t len = r.size();
  
  // Create an output stringfish vector
  // Like all R objects, it must be protected from garbage collection
  SEXP output = PROTECT(sf_vector(len));
  
  // Obtain a reference to the underlying output data
  sf_vec_data & output_data = sf_vec_data_ref(output);
  
  // You can use range based for loop via an iterator class that returns RStringIndexer::rstring_info e
  // rstring info is a struct containing const char * ptr (null terminated), int len, and cetype_t enc
  // a NA string is represented by a nullptr
  // Alternatively, access the data via the function r.getCharLenCE(i)
  size_t i = 0;
  for(auto e : r) {
    // check if string is NA and go to next if it is
    if(e.ptr == nullptr) {
      i++; // increment output index
      continue;
    }
    // create a temporary output string and process the results
    std::string temp(e.len, '\0');
    bool case_switch = false;
    for(int j=0; j<e.len; j++) {
      if((e.ptr[j] >= 65) & (e.ptr[j] <= 90)) { // char j is upper case
        if((case_switch = !case_switch)) { // check if we should convert to lower case
          temp[j] = e.ptr[j] + 32;
          continue;
        }
      } else if((e.ptr[j] >= 97) & (e.ptr[j] <= 122)) { // char j is lower case
        if(!(case_switch = !case_switch)) { // check if we should convert to upper case
          temp[j] = e.ptr[j] - 32;
          continue;
        }
      } else if(e.ptr[j] == 32) {
        case_switch = false;
      }
      temp[j] = e.ptr[j];
    }
    
    // Create a new vector element sfstring and insert the processed string into the stringfish vector
    // sfstring has three constructors, 1) taking a std::string and encoding, 
    // 2) a char pointer and encoding, or 3) a CHARSXP object (e.g. sfstring(NA_STRING))
    output_data[i] = sfstring(temp, e.enc);
    i++; // increment output index
  }
  // Finally, call unprotect and return result
  UNPROTECT(1);
  return output;
}
```

Example function call:

``` r
sf_alternate_case("hello world") 
[1] "hElLo wOrLd"
```

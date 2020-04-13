# altstrings

A universal platform for altrep strings backed by std::vector.  

# TODO:

* Export inst folder
* substring
* string_reverse
* nchar
* Implement PCRE2
* gsub
* grep
* grepl
* package example
* Makevars
* Configure
* Makevars.win - use environment variables?
* * Example https://github.com/cran/Cairo/blob/master/src/Makevars.win
* * Example https://github.com/ecrc/exageostat/tree/d6f78f67cd43a5145157e667c5bb39521343aacd
* Test package example

* Read me file example and bench


``` r
library(altstrings)
```


## Windows install of PCRE2
```
sh
autoreconf -f -i
./configure --enable-jit
make

copy `.dll` in `.libs` and `*.h` in `src` to a convienent place.  
```


## Windows install of MSYS2
```
pacman -Syu
pacman -S base-devel
pacman -S mingw-w64-x86_64-gcc
autoreconf -f -i
./configure --enable-jit
make

copy `.dll` in `.libs` and `*.h` in `src` to a convienent place.  
```
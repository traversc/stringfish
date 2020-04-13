library(stringfish)
# to do -- fill in more stuff

x <- sf_random_strings(1e6, string_size = 20, charset = "ARNDCQEGHILKMFPSTWYV")
x0 <- substr(x, 3, 18)
x1 <- sf_substr(x, 3, -3)
s0 <- sum(grepl("RS", x))
s1 <- sum(sf_grepl(x, "RS", encode_mode = "byte"))

stopifnot(identical(x0, x1))
stopifnot(identical(s0, s1))
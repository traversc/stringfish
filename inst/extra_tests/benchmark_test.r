suppressMessages(library(stringfish, quietly = T))
suppressMessages(library(qs, quietly = T))
suppressMessages(library(dplyr, quietly = T))
suppressMessages(library(microbenchmark, quietly = T))
suppressMessages(library(stringi, quietly = T))
suppressMessages(library(tools, quietly = T))


nt <- 4

temp <- tempfile()

catn <- function(...) {
  cat(..., "\n")
}

args <- commandArgs(T)
if(length(args) == 0) {
  n <- 3
} else {
  n <- as.integer(args[1])
}

# enwik8 and enwik9 from http://prize.hutter1.net/
# First 1e8 lines of wikipedia (UTF-8 encoded)
test_file <- "~/enwik8"
if(!file.exists(test_file)) {
  test_folder <- tempdir()
  zip_file <- tempfile()
  options(timeout = 1000)
  download.file("https://data.deepai.org/enwik8.zip", zip_file)
  unzip(zip_file, "enwik8", exdir = test_folder, overwrite=T)
  test_file <- paste0(test_folder, "/enwik8")
}

catn("readLines")
readlines_bench <- microbenchmark(
  base_R = readLines(test_file, encoding = "UTF-8", warn = F),
  stringi = stringi::stri_read_lines(test_file, encoding = "UTF-8"),
  stringfish = sf_readLines(test_file),
  stringfish_materialized = materialize(sf_readLines(test_file)),
  times=n, setup = gc(full=TRUE))

enwik8 <- readLines(test_file, encoding = "UTF-8", warn = F)
qsave(enwik8, temp)
rm(enwik8)
gc(full=TRUE)

catn("qread")
qread_bench <- microbenchmark(
  # base_R = system(sprintf('Rscript -e "x <- qs::qread(\\"%s\\")"', temp)),
  # stringfish = system(sprintf('Rscript -e "x <- qs::qread(\\"%s\\", use_alt_rep=T)"', temp)),
  base_R = qread(temp),
  stringfish = qread(temp, use_alt_rep=T),
  times=n, setup = {gc(full=TRUE)})
unlink(temp)

enwik8 <- readLines(test_file, encoding = "UTF-8", warn = F)
enwik8_sf <- sf_readLines(test_file)

# ewc <- convert_to_sf(enwik8)

set.seed(1)
enwik8_shuffled <- sample(enwik8)
enwik8_sf_shuffled <- convert_to_sf(enwik8_shuffled) # no sf_sample method yet

catn("writeLines")
writeLines_bench <- microbenchmark(
  base_R = writeLines(enwik8, temp),
  stringi = stringi::stri_write_lines(enwik8, temp, encoding = "UTF-8"),
  stringfish = sf_writeLines(enwik8_sf, temp),
  times=n, setup = {unlink(temp); gc(full=TRUE)})

if(Sys.info()["sysname"] != "Windows") {
writeLines(enwik8, temp)
x <- tools::md5sum(temp)
sf_writeLines(enwik8_sf, temp)
y <- tools::md5sum(temp)
stopifnot(identical(x,y))
unlink(temp)
}

stopifnot(string_identical(enwik8, enwik8_sf))
stopifnot(string_identical(enwik8_shuffled, enwik8_sf_shuffled))

catn("qsave")
qsave_bench <- microbenchmark(
  base_R = qsave(enwik8, temp),
  stringfish = qsave(enwik8_sf, temp),
  times=n, setup = {unlink(temp); gc(full=TRUE)})

qsave(enwik8, temp)
x <- tools::md5sum(temp)
qsave(enwik8_sf, temp)
y <- tools::md5sum(temp)
stopifnot(identical(x,y))
x <- qread(temp)
y <- qread(temp, use_alt_rep=T)
stopifnot(string_identical(enwik8, x))
stopifnot(string_identical(enwik8, y))
rm(x, y)
gc(full=TRUE)

catn("substr")
substr_bench <- microbenchmark(
  base_R = substr(enwik8, 10, 60),
  stringi = stringi::stri_sub(enwik8, 10, 60),
  stringfish = sf_substr(enwik8_sf, 10, 60),
  stringfish_mt = sf_substr(enwik8_sf, 10, 60, nthreads=nt),
  stringfish_materialized = materialize(sf_substr(enwik8_sf, 10, 60)),
  times=n, setup = gc(full=TRUE))

x <- substr(enwik8, 10, 50)
y <- sf_substr(enwik8_sf, 10, 50)
stopifnot(string_identical(x, y))
y <- sf_substr(enwik8_sf, 10, 50, nthreads=nt)
stopifnot(string_identical(x, y))
rm(x, y)
gc(full=TRUE)

catn("paste")
paste_bench <- microbenchmark(
  base_R = paste0(enwik8, enwik8_shuffled),
  stringi = stringi::stri_paste(enwik8, enwik8_shuffled),
  stringfish = sf_paste(enwik8_sf, enwik8_sf_shuffled),
  stringfish_mt = sf_paste(enwik8_sf, enwik8_sf_shuffled, nthreads=nt),
  stringfish_materialized = materialize(sf_paste(enwik8_sf, enwik8_sf)),
  times=n, setup = gc(full=TRUE))

x <- paste0(enwik8, enwik8_shuffled)
y <- sf_paste(enwik8_sf, enwik8_sf_shuffled)
stopifnot(string_identical(x, y))
y <- sf_paste(enwik8_sf, enwik8_sf_shuffled, nthreads=nt)
stopifnot(string_identical(x, y))
rm(x, y)
gc(full=TRUE)

catn("grepl")
grepl_bench <- microbenchmark(
  base_R = grepl("<title>.+</title>", enwik8),
  stringi = stringi::stri_detect(enwik8, regex="<title>.+</title>"),
  stringfish = sf_grepl(enwik8_sf, "<title>.+</title>"),
  stringfish_mt = sf_grepl(enwik8_sf, "<title>.+</title>", nthreads=nt),
  times=n, setup = gc(full=TRUE))

x <- grepl("<title>.+</title>", enwik8)
y <- sf_grepl(enwik8_sf, "<title>.+</title>")
stopifnot(identical(x, y))
y <- sf_grepl(enwik8_sf, "<title>.+</title>", nthreads=nt)
stopifnot(identical(x, y))
rm(x, y)
gc(full=TRUE)

catn("gsub")
gsub_bench <- microbenchmark(
  base_R = gsub("^.*<title>(.+)</title>.*$", "\\1", enwik8),
  stringi = stringi::stri_replace_all_regex(enwik8, "^.*<title>(.+)</title>.*$", "$1"),
  stringfish = sf_gsub(enwik8_sf, "^.*<title>(.+)</title>.*$", "$1"),
  stringfish_mt = sf_gsub(enwik8_sf, "^.*<title>(.+)</title>.*$", "$1", nthreads=nt),
  stringfish_materialized = materialize(sf_gsub(enwik8_sf, "^.*<title>(.+)</title>.*$", "$1")),
  times=n, setup = gc(full=TRUE))

x <- gsub("^.*<title>(.+)</title>.*$", "\\1", enwik8)
y <- sf_gsub(enwik8_sf, "^.*<title>(.+)</title>.*$", "$1")
stopifnot(string_identical(x, y))
y <- sf_gsub(enwik8_sf, "^.*<title>(.+)</title>.*$", "$1", nthreads=nt)
stopifnot(string_identical(x, y))
rm(x, y)
gc(full=TRUE)

catn("strsplit")
strsplit_bench <- microbenchmark(
  base_R = strsplit(enwik8, split=",|;"),
  stringi = stringi::stri_split(enwik8, regex=",|;"),
  stringfish = sf_split(enwik8_sf, split = ",|;"),
  stringfish_mt = sf_split(enwik8_sf, split = ",|;", nthreads=nt),
  stringfish_materialized = lapply(sf_split(enwik8_sf, split = ",|;"), materialize),
  times=n, setup = gc(full=TRUE))

x <- stringi::stri_split(enwik8, regex=",|;")
y <- sf_split(enwik8_sf, split = ",|;")
stopifnot(identical(x, y))
y <- sf_split(enwik8_sf, split = ",|;", nthreads=nt)
stopifnot(identical(x, y))
rm(x, y)
gc(full=TRUE)

catn("match")
match_bench <- microbenchmark(
  base_R = match(enwik8_shuffled, enwik8),
  stringfish = sf_match(enwik8_sf_shuffled, enwik8_sf),
  stringfish_mt = sf_match(enwik8_sf_shuffled, enwik8_sf, nthreads=nt),
  times=n, setup = gc(full=TRUE))

x <- match(enwik8_shuffled, enwik8)
y <- sf_match(enwik8_sf_shuffled, enwik8_sf)
stopifnot(identical(x, y))
y <- sf_match(enwik8_sf_shuffled, enwik8_sf, nthreads=nt)
stopifnot(identical(x, y))
rm(x, y)
gc(full=TRUE)

catn("trimws")
trimws_bench <- microbenchmark(
  base_R = trimws(enwik8),
  stringi = stringi::stri_trim(enwik8),
  stringfish = sf_trim(enwik8_sf),
  stringfish_mt = sf_trim(enwik8_sf, nthreads=nt),
  times=n, setup = gc(full=TRUE))

x <- trimws(enwik8)
y <- sf_trim(enwik8_sf)
stopifnot(string_identical(x, y))
y <- sf_trim(enwik8_sf, nthreads=nt)
stopifnot(string_identical(x, y))
rm(x, y)
gc(full=TRUE)

catn("nchar")
nchar_bench <- microbenchmark(
  base_R = nchar(enwik8),
  stringi = stringi::stri_length(enwik8),
  stringfish = sf_nchar(enwik8_sf),
  stringfish_mt = sf_nchar(enwik8_sf, nthreads=nt),
  times=n, setup = gc(full=TRUE))

x <- nchar(enwik8)
y <- sf_nchar(enwik8_sf)
stopifnot(identical(x, y))
y <- sf_nchar(enwik8_sf, nthreads=nt)
stopifnot(identical(x, y))
rm(x, y)
gc(full=TRUE)


# plot code -- run interactively
if(F) {
  library(ggplot2)
  library(hrbrthemes)
  
df <- rbind(
  qread_bench %>% 
    as.data.frame %>% mutate(op = "qs::qread"),
  qsave_bench %>% 
    as.data.frame %>% mutate(op = "qs::qsave"),
  readlines_bench %>% 
    as.data.frame %>% mutate(op = "readLines"),
  writeLines_bench %>% 
    as.data.frame %>% mutate(op = "writeLines"),
  paste_bench %>% 
    as.data.frame %>% mutate(op = "paste"),
  grepl_bench %>% 
    as.data.frame %>% mutate(op = "grepl"),
  gsub_bench %>% 
    as.data.frame %>% mutate(op = "gsub"),
  substr_bench %>% 
    as.data.frame %>% mutate(op = "substr"),
  strsplit_bench %>% 
    as.data.frame %>% mutate(op = "strsplit"),
  match_bench %>% 
    as.data.frame %>% mutate(op = "match"),
  trimws_bench %>% 
    as.data.frame %>% mutate(op = "trimws"),
  nchar_bench %>% 
    as.data.frame %>% mutate(op = "nchar"))

df$op <- factor(df$op, levels = 
    c("nchar", "grepl", "gsub", "substr", "paste", "strsplit", "match", "trimws",  "writeLines", "readLines", "qs::qsave",  "qs::qread"))

  dfs <- df %>% 
    # filter(expr %in% c("base_R", "stringfish", "stringfish_mt")) %>% # comment out this line to include stringi
    mutate(expr = as.character(expr)) %>%
    mutate(expr = case_when(expr == "base_R" ~ "Base R",
                            expr == "stringfish" ~ "stringfish (1 thread)", 
                            expr == "stringfish_materialized" ~ "stringfish (materialized)",
                            expr == "stringfish_mt" ~ "stringfish (4 threads)",
                            T ~ expr)) %>% 
    group_by(op, expr) %>% 
    summarize(time = mean(time)) %>% 
    group_by(op) %>%
    mutate(speed = time[expr == "Base R"] / time)
  g <- ggplot(dfs, aes(x = op, y = speed, fill = expr)) + 
    geom_bar(color = "black", stat = "identity", position = position_dodge(preserve = "single")) + 
    geom_hline(aes(yintercept=1), lty=2, color = "blue") + 
    theme_ipsum_rc() +
    theme(plot.margin = unit(rep(.5,4), "lines"), 
          # legend.position="bottom",
          legend.key.size = unit(1, 'lines'), 
          legend.box.just = "left",
          legend.text = element_text(margin = margin(r = 1, unit = "lines")),
          axis.title.x = element_text(size=rel(1.3)),
          axis.text.x = element_text(size=rel(0.9)), 
          axis.title.y = element_text(size=rel(1.3))) + 
    labs(x = NULL, y = "Speed relative to base R", fill = NULL) + 
    scale_y_continuous(trans = "log1p", breaks = c(0,1,2.5,5,10,20,40,80)) + 
    # scale_fill_discrete(labels = c("Base R", "stringfish (1 thread)", "stringfish (4 threads)")) +
    theme(legend.position = "bottom") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1))
  
  ggsave(g, file = "vignettes/bench_v2.png", width=6.5*1.25, height=3.5*1.25, dpi=600)
}







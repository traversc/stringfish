suppressMessages(library(stringfish, quietly = T))
# suppressMessages(library(qs, quietly = T))
suppressMessages(library(dplyr, quietly = T))
suppressMessages(library(microbenchmark, quietly = T))
suppressMessages(library(stringi, quietly = T))
suppressMessages(library(tools, quietly = T))

catn <- function(...) {
  cat(..., "\n")
}

args <- commandArgs(T)
if(length(args) == 0) {
  n <- 1
} else {
  n <- as.integer(args[1])
}

# enwik8 and enwik9 from http://prize.hutter1.net/
# First 1e8 lines of wikipedia (UTF-8 encoded)
test_file <- "~/enwik8"
if(!file.exists(test_file)) {
  test_folder <- tempdir()
  zip_file <- tempfile()
  download.file("http://mattmahoney.net/dc/enwik8.zip", zip_file)
  unzip(zip_file, "enwik8", exdir = test_folder, overwrite=T)
  test_file <- paste0(test_folder, "/enwik8")
}

catn("readLines")
readlines_bench <- microbenchmark(
  base_R = readLines(test_file, encoding = "UTF-8", warn = F),
  stringi = stringi::stri_read_lines(test_file, encoding = "UTF-8"),
  stringfish = stringfish::sf_readLines(test_file),
  stringfish_materialized = stringfish::materialize(stringfish::sf_readLines(test_file)),
  times=n, setup = gc())

enwik8 <- readLines(test_file, encoding = "UTF-8", warn = F)
enwik8_sf <- stringfish::sf_readLines(test_file)

# ewc <- convert_to_sf(enwik8)

set.seed(1)
enwik8_shuffled <- sample(enwik8)
enwik8_sf_shuffled <- stringfish::convert_to_sf(enwik8_shuffled) # no sf_sample method yet

temp <- tempfile()

catn("writeLines")
writeLines_bench <- microbenchmark(
  base_R = writeLines(enwik8, temp),
  stringi = stringi::stri_write_lines(enwik8, temp, encoding = "UTF-8"),
  stringfish = stringfish::sf_writeLines(enwik8_sf, temp),
  times=n, setup = {unlink(temp); gc()})

writeLines(enwik8, temp)
x <- tools::md5sum(temp)
stringfish::sf_writeLines(enwik8_sf, temp)
y <- tools::md5sum(temp)
stopifnot(identical(x,y))
unlink(temp)
rm(x, y)

stopifnot(string_identical(enwik8, enwik8_sf))
stopifnot(string_identical(enwik8_shuffled, enwik8_sf_shuffled))

catn("substr")
substr_bench <- microbenchmark(
  base_R = substr(enwik8, 1, 100),
  stringi = stringi::stri_sub(enwik8, 1, 100),
  stringfish = stringfish::sf_substr(enwik8_sf, 1, 100),
  stringfish_materialized = stringfish::materialize(stringfish::sf_substr(enwik8_sf, 1, 100)),
  times=n, setup = gc())

x <- substr(enwik8, 1, 100)
y <- stringfish::sf_substr(enwik8_sf, 1, 100)
stopifnot(string_identical(x, y))
rm(x, y)
gc()

catn("paste")
paste_bench <- microbenchmark(
  base_R = paste0(enwik8, enwik8),
  stringi = stringi::stri_paste(enwik8, enwik8),
  stringfish = stringfish::sf_paste(enwik8_sf, enwik8_sf),
  stringfish_materialized = stringfish::materialize(stringfish::sf_paste(enwik8_sf, enwik8_sf)),
  times=n, setup = gc())

x <- paste0(enwik8, enwik8)
y <- stringi::stri_paste(enwik8, enwik8)
stopifnot(string_identical(x, y))
rm(x, y)
gc()

catn("grepl")
grepl_bench <- microbenchmark(
  base_R = grepl("<title>.+</title>", enwik8),
  stringi = stringi::stri_detect(enwik8, regex="<title>.+</title>"),
  stringfish = stringfish::sf_grepl(enwik8_sf, "<title>.+</title>"),
  times=n, setup = gc())

x <- grepl("<title>.+</title>", enwik8)
y <- stringfish::sf_grepl(enwik8_sf, "<title>.+</title>")
stopifnot(identical(x, y))
rm(x, y)
gc()

catn("gsub")
gsub_bench <- microbenchmark(
  base_R = gsub("^.*<title>(.+)</title>.*$", "\\1", enwik8),
  stringi = stringi::stri_replace_all_regex(enwik8, "^.*<title>(.+)</title>.*$", "$1"),
  stringfish = stringfish::sf_gsub(enwik8_sf, "^.*<title>(.+)</title>.*$", "$1"),
  stringfish_materialized = stringfish::materialize(stringfish::sf_gsub(enwik8_sf, "^.*<title>(.+)</title>.*$", "$1")),
  times=n, setup = gc())

x <- gsub("^.*<title>(.+)</title>.*$", "\\1", enwik8)
y <- stringfish::sf_gsub(enwik8_sf, "^.*<title>(.+)</title>.*$", "$1")
stopifnot(string_identical(x, y))
rm(x, y)
gc()

catn("strsplit")
strsplit_bench <- microbenchmark(
  base_R = strsplit(enwik8, split=",|;"),
  stringi = stringi::stri_split(enwik8, regex=",|;"),
  stringfish = stringfish::sf_split(enwik8_sf, split = ",|;"),
  stringfish_materialized = lapply(stringfish::sf_split(enwik8_sf, split = ",|;"), materialize),
  times=n, setup = gc())

x <- stringi::stri_split(enwik8, regex=",|;")
y <- stringfish::sf_split(enwik8_sf, split = ",|;")
stopifnot(identical(x, y))
rm(x, y)
gc()

catn("match")
match_bench <- microbenchmark(
  base_R = match(enwik8_shuffled, enwik8),
  stringfish = stringfish::sf_match(enwik8_sf_shuffled, enwik8_sf),
  times=n, setup = gc())

x <- match(enwik8_shuffled, enwik8)
y <- stringfish::sf_match(enwik8_sf_shuffled, enwik8_sf)
stopifnot(identical(x, y))
rm(x, y)
gc()

catn("trimws")
trimws_bench <- microbenchmark(
  base_R = trimws(enwik8),
  stringi = stringi::stri_trim(enwik8),
  stringfish = stringfish::sf_trim(enwik8_sf),
  times=n, setup = gc())

x <- trimws(enwik8)
y <- sf_trim(enwik8_sf)
stopifnot(string_identical(x, y))
rm(x, y)
gc()

catn("nchar")
nchar_bench <- microbenchmark(
  base_R = nchar(enwik8),
  stringi = stringi::stri_length(enwik8),
  stringfish = stringfish::sf_nchar(enwik8_sf),
  times=n, setup = gc())

x <- nchar(enwik8)
y <- stringfish::sf_nchar(enwik8_sf)
stopifnot(identical(x, y))
rm(x, y)
gc()


# plot code -- run interactively
if(F) {
  library(ggplot2)
  library(hrbrthemes)
  
df <- rbind(
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
  match_bench %>% 
    as.data.frame %>% mutate(op = "match"),
  trimws_bench %>% 
    as.data.frame %>% mutate(op = "trimws"),
  nchar_bench %>% 
    as.data.frame %>% mutate(op = "nchar"))

  dfs <- df %>% 
    filter(expr %in% c("base_R", "stringfish")) %>%
    group_by(op, expr) %>% 
    summarize(time = mean(time)) %>% 
    group_by(op) %>%
    mutate(speed = time[expr == "base_R"] / time)
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
    labs(x = NULL, y = "Speed (relative to base R)", fill = NULL) + 
    scale_y_continuous(trans = "log1p", breaks = c(0,1,2.5,5,10,20)) + 
    # theme(legend.position = "bottom") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  
  ggsave(g, file = "vignettes/bench_v2.png", width=6.5*1.25, height=3.5*1.25, dpi=600)
}







library(tidyverse)
library(janitor)
library(skimr)
library(naniar)
library(glue)
library(broom)

profile_shape <- function(df) {
  paste0(nrow(df), " rows x ", ncol(df), " columns")
}

profile_types <- function(df) {
  types <- sapply(df, class)
  paste(paste0(names(types), ": ", types), collapse = "\n")
  # last line: your nested paste call, without cat()
}

profile_missing <- function(df){
  missing <- sapply(df, function(x) sum(is.na(x)))
  paste(paste0(names(missing), ":", missing), collapse = "\n")
}

profile_duplicates <- function(df){
  paste0(sum(duplicated(df)), " duplicate rows")
}

profile_numeric <- function(df) {
  fmt <- function(v) format(v, big.mark = ",", scientific = FALSE)
  num <- df[sapply(df, is.numeric)] 
  if (ncol(num) == 0) return("no numeric columns")
  lines <- sapply(num, function(x) {
    paste0("min ", fmt(min(x, na.rm = TRUE)),
           ", median ", fmt(median(x, na.rm = TRUE)),
           ", mean ", fmt(round(mean(x, na.rm = TRUE))),
           ", max ", fmt(max(x, na.rm = TRUE)))
  })
  paste(paste0(names(lines), ": ", lines), collapse = "\n")
}
profile_one_cat <- function(x, n = 5) {
  t <- sort(table(x), decreasing = TRUE)
  lines <- paste0(names(head(t, n)), ": ", head(t, n))
  if ( length(t) > n){
    lines <- c(lines, paste0("... and ", length(t) - n, " more values"))
  }
  paste(lines, collapse = "\n")
}


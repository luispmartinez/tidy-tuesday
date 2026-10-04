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

profile_categorical <- function(df){
  cat_cols <- df[sapply(df, function(x) is.character(x) || is.factor(x))]
  if (ncol(cat_cols) == 0) return ("no categorical columns")
  blocks <- sapply(cat_cols, profile_one_cat)
  labeled <- paste0(names(blocks), "\n", blocks)
  paste(labeled, collapse = "\n\n")
}

keep_col <- function(x, min_category_size = 10) {
            length(x)/ n_distinct(x) >=min_category_size
            # return TRUE if the column should be grouped, FALSE if skipped
     }

profile_groups <- function(df, min_category_size = 10 ){
  num <- df[sapply(df, is.numeric)]
  if (ncol(num) == 0) return("no numeric columns")
  cat_cols <- df[sapply(df, function(x) is.character(x) || is.factor(x))]
  if (ncol(cat_cols) == 0) return ("no categorical columns")
  groupable <- cat_cols[ sapply(cat_cols, keep_col, min_category_size = min_category_size) ]
  if (ncol(groupable) == 0) return("no categorical columns with enough rows")
  pairs <- crossing(cat_col = names(groupable), num_col = names(num))
  blocks <- map2_chr(pairs$cat_col, pairs$num_col, function(cat_name, num_name) profile_one_group(df, cat_name, num_name, min_category_size))
  headings <- paste0(pairs$cat_col, " x ", pairs$num_col)
  labeled <- paste0(headings, "\n", blocks)
  paste(labeled, collapse = "\n\n")
}

profile_one_group <- function(df, cat_col, num_col, min_category_size = 10) {
  s <- df |> group_by(.data[[cat_col]]) |>
    summarise(n = n(), mean_value = mean(.data[[num_col]], na.rm = TRUE))
  kept <- s |> filter(n >= min_category_size)
  if (nrow(kept) == 0) return(paste0("no groups with at least ", min_category_size, " rows"))
  lines <- paste0(
    kept[[cat_col]], " (n=", kept$n, 
    "): mean ", format(round(kept$mean_value), 
                       big.mark = ",", scientific = FALSE, trim = TRUE))
  count <- nrow(s) - nrow(kept)
  if (count > 0) {
    lines <- c(lines, paste0(count, " groups with fewer than ", min_category_size, " rows not shown"))
  }
  paste(lines, collapse = "\n")
}


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
  note <- "means are taken over rows; an entity that appears in several rows counts once per row"
  paste(c(labeled, note), collapse = "\n\n")
}

profile_one_group <- function(df, cat_col, num_col, min_category_size = 10) {
  s <- df |> group_by(.data[[cat_col]]) |>
    summarise(n = n(), mean_value = mean(.data[[num_col]], na.rm = TRUE))
  kept <- s |> filter(n >= min_category_size) |> arrange(desc(n))
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

big_enough <- function(x, min_category_size = 10) {
  counts <- table(x)
  keep <-  counts >= min_category_size
  names(counts)[keep]
}

profile_one_cross <- function(df, cat_a, cat_b, min_category_size = 10, resid_cutoff = 5) {
  keep_a <- big_enough(df[[cat_a]], min_category_size)
  keep_b <- big_enough(df[[cat_b]], min_category_size)
  small <- df |> filter(.data[[cat_a]] %in% keep_a, .data[[cat_b]] %in% keep_b)
  if (length(keep_a) < 2 || length(keep_b) < 2) return (paste0("fewer than two categories with at least ", min_category_size, " rows in ", cat_a, " or ", cat_b))
  tab <- table(small[[cat_a]], small[[cat_b]])
  res <- suppressWarnings(stats::chisq.test(tab))
  weak <- mean(res$expected < 5)
  p_note <- "chi-square p-value"            # label for the normal case
  p_value <- res$p.value
  if (weak > 0.2) {
    set.seed(1)
    sim <- stats::chisq.test(tab, simulate.p.value = TRUE)
    p_value <- sim$p.value
    p_note <- "simulated p-value"
  }
  strongest <- which(abs(res$stdres) > resid_cutoff, arr.ind = TRUE)
  zero_share <- mean(tab == 0)
  weak_share <- weak
  share <- round(mean(tab == 0) * 100)
  line1 <- paste0(
    nrow(tab), " x ",
    ncol(tab),
    " (", cat_a, " x ", cat_b, ")"
  )
  line2 <- paste0(
    nrow(small), " of ",
    nrow(df), " rows kept"
  )
  line3 <- paste0(
    round(zero_share * 100), "% of cells are zero; ",
    round(weak * 100), "% have expected count under 5"
  )
  lines <- c(line1, line2, line3)
  paste(lines, collapse = "\n")
}

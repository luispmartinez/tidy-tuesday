# =============================================================================
# eda_toolkit.R
# Stage 1 profiling for any table: describe the file as it is, change nothing.
#
# How the pieces fit together
#   profile_*()      one report section each; every one returns ONE text string
#   profile_one_*()  helpers that handle a single column or pair; the matching
#                    profile_*() loops over them and labels each block
#   build_report()   runs all ten sections; a failing section never stops the rest
#   write_report()   saves the result as a Markdown file
#
# The ten sections, in report order
#   1 Shape            2 Column types       3 Missing values    4 Duplicate rows
#   5 Numeric summary  6 Categorical summary 7 Group means      8 Categorical x categorical
#   9 Numeric bivariate  10 OLS
#
# Settings that mean the same thing everywhere
#   min_category_size  a group needs at least this many rows to be summarized (default 10)
#   exclude_columns    column names to leave out of the sections that analyze columns
#                      (shape, types, missing and duplicates always see every column)
#
# Guards: when a section can't run (no numeric columns, too few rows, ...) it
# returns a plain message, so the report says why instead of failing.
#
# Usage: see the bottom of the file. scan_and_profile.R calls these functions
# for every file in a drop folder.
# =============================================================================

# Packages
#   tidyverse: the pipe, dplyr verbs, tibble, crossing(), map2_chr()
#   broom: tidy model summaries (used as broom:: in the OLS section)
library(tidyverse)
library(broom)

# ---------------------------------------------------------------------------
# Whole-table checks: shape, column types, missing values, duplicates
# ---------------------------------------------------------------------------

# Shape: how many rows and columns the table has.
# Returns e.g. "796 rows x 4 columns".
profile_shape <- function(df) {
  paste0(nrow(df), " rows x ", ncol(df), " columns")
}

# Column types: one "name: type" line per column.
# class(x)[1] keeps only the first class, because date-time columns have two
# (POSIXct, POSIXt) and would otherwise break the output.
profile_types <- function(df) {
  types <- sapply(df, function(x) class(x)[1])
  paste(paste0(names(types), ": ", types), collapse = "\n")
  # (the paste() above is the return value: one string, one line per column)
}

# Missing values: the number of NA values in each column, one line per column.
profile_missing <- function(df){
  missing <- sapply(df, function(x) sum(is.na(x)))
  paste(paste0(names(missing), ":", missing), collapse = "\n")
}

# Duplicate rows: rows identical to an earlier row in every column.
# Counts the extra copies, so the first occurrence is not counted.
profile_duplicates <- function(df){
  paste0(sum(duplicated(df)), " duplicate rows")
}

# ---------------------------------------------------------------------------
# One column at a time: numeric and categorical summaries
# ---------------------------------------------------------------------------

# Numeric summary: min, median, mean and max for each numeric column.
# Numbers go through fmt_num(), so large and small values both read cleanly.
# na.rm = TRUE keeps a few missing values from turning a whole line into NA.
profile_numeric <- function(df, exclude_columns = NULL) {
  # numeric columns only, minus any excluded ones
  num <- numeric_cols(df, exclude_columns) 
  if (ncol(num) == 0) return("no numeric columns")
  lines <- sapply(num, function(x) {
    paste0("min ", fmt_num(min(x, na.rm = TRUE)),
           ", median ", fmt_num(median(x, na.rm = TRUE)),
           ", mean ", fmt_num(mean(x, na.rm = TRUE)),
           ", max ", fmt_num(max(x, na.rm = TRUE)))
  })
  paste(paste0(names(lines), ": ", lines), collapse = "\n")
}
# Helper for profile_categorical: the n most common values of ONE column, with
# counts. When there are more values than n, adds a "... and K more values" line.
profile_one_cat <- function(x, n = 5) {
  # table() counts each value; sort() puts the most common first
  t <- sort(table(x), decreasing = TRUE)
  lines <- paste0(names(head(t, n)), ": ", head(t, n))
  if ( length(t) > n){
    lines <- c(lines, paste0("... and ", length(t) - n, " more values"))
  }
  paste(lines, collapse = "\n")
}

# Categorical summary: the most common values of every text (or factor) column.
# Each column gets its own labeled block, separated by a blank line.
profile_categorical <- function(df, exclude_columns = NULL){
  # keep text and factor columns, then drop any excluded ones
  cat_cols <- df[sapply(df, function(x) is.character(x) || is.factor(x))]
  cat_cols <- cat_cols[!names(cat_cols) %in% exclude_columns]
  if (ncol(cat_cols) == 0) return ("no categorical columns")
  blocks <- sapply(cat_cols, profile_one_cat)
  labeled <- paste0(names(blocks), "\n", blocks)
  paste(labeled, collapse = "\n\n")
}

# ---------------------------------------------------------------------------
# Relationships between columns (group means, crosstab, correlation) and the
# shared helpers they use
# ---------------------------------------------------------------------------

# Helper: should this categorical column be grouped at all?
# TRUE when the average group would have at least min_category_size rows
# (rows divided by distinct values). Skips ID-like columns that have about one
# row per value, where group summaries would mean nothing.
keep_col <- function(x, min_category_size = 10) {
            length(x)/ n_distinct(x) >=min_category_size
            # return TRUE if the column should be grouped, FALSE if skipped
     }

# Group means: the mean of each numeric column within each category of each
# groupable categorical column (every categorical x numeric pairing).
# Ends with a note that means are taken over rows, so something repeated across
# several rows counts once per row.
profile_groups <- function(df, min_category_size = 10, exclude_columns = NULL ){
  num <- numeric_cols(df, exclude_columns)
  if (ncol(num) == 0) return("no numeric columns")
  cat_cols <- df[sapply(df, function(x) is.character(x) || is.factor(x))]
  cat_cols <- cat_cols[!names(cat_cols) %in% exclude_columns]
  if (ncol(cat_cols) == 0) return ("no categorical columns")
  # only categorical columns with enough rows per category (see keep_col)
  groupable <- cat_cols[ sapply(cat_cols, keep_col, min_category_size = min_category_size) ]
  if (ncol(groupable) == 0) return("no categorical columns with enough rows")
  # crossing() builds every categorical x numeric pairing
  pairs <- crossing(cat_col = names(groupable), num_col = names(num))
  blocks <- map2_chr(pairs$cat_col, pairs$num_col, function(cat_name, num_name) profile_one_group(df, cat_name, num_name, min_category_size))
  headings <- paste0(pairs$cat_col, " x ", pairs$num_col)
  labeled <- paste0(headings, "\n", blocks)
  note <- "means are taken over rows; an entity that appears in several rows counts once per row"
  paste(c(labeled, note), collapse = "\n\n")
}

# Helper for profile_groups: the mean of ONE numeric column by ONE categorical
# column. Lists groups with at least min_category_size rows, largest first, plus
# a line saying how many smaller groups were left out.
profile_one_group <- function(df, cat_col, num_col, min_category_size = 10) {
  # .data[[name]] lets column names arrive as text
  s <- df |> group_by(.data[[cat_col]]) |>
    summarise(n = n(), mean_value = mean(.data[[num_col]], na.rm = TRUE))
  # keep only groups big enough to trust, largest first
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

# Helper: the category values that have at least min_category_size rows.
# Returns their names. The crosstab and OLS sections use it to drop tiny categories.
big_enough <- function(x, min_category_size = 10) {
  counts <- table(x)
  keep <-  counts >= min_category_size
  names(counts)[keep]
}

# Helper for profile_crosstab: chi-square test of association between TWO
# categorical columns.
# Steps: shrink both columns to their larger categories, build the count table,
# run the test, then report the table size, rows kept, sparsity, the test result
# and the cells that depart most from independence.
# resid_cutoff: cells with an adjusted residual beyond +/- this value are listed.
# exclude_columns is accepted but not used here; exclusion happens in
# profile_crosstab before this is called.
profile_one_cross <- function(df, cat_a, cat_b, min_category_size = 10, resid_cutoff = 5, exclude_columns = NULL) {
  keep_a <- big_enough(df[[cat_a]], min_category_size)
  keep_b <- big_enough(df[[cat_b]], min_category_size)
  # keep only rows whose two categories are both big enough
  small <- df |> filter(.data[[cat_a]] %in% keep_a, .data[[cat_b]] %in% keep_b)
  # a table needs at least two categories on each side
  if (length(keep_a) < 2 || length(keep_b) < 2) return (paste0("fewer than two categories with at least ", min_category_size, " rows in ", cat_a, " or ", cat_b))
  tab <- table(small[[cat_a]], small[[cat_b]])
  # warnings are suppressed because the report prints its own sparsity note
  res <- suppressWarnings(stats::chisq.test(tab))
  # share of cells with an expected count under 5; when high, the usual p-value is unreliable
  weak <- mean(res$expected < 5)
  p_note <- "chi-square p-value"            # label for the normal case
  p_value <- res$p.value
  # sparse table: use a simulated p-value instead (set.seed makes it repeatable)
  if (weak > 0.2) {
    set.seed(1)
    sim <- stats::chisq.test(tab, simulate.p.value = TRUE)
    p_value <- sim$p.value
    p_note <- "simulated p-value"
  }
  zero_share <- mean(tab == 0)
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
  p_text <- format_p(p_value)
  # cells whose adjusted residual is large: more or fewer rows than independence predicts
  strongest <- which(abs(res$stdres) > resid_cutoff, arr.ind = TRUE)
  line4 <- paste0(
    "chi-square = ",round(res$statistic, 1), " (df = ",
    res$parameter, "); ", p_note, p_text
  )
  lines <- c(line1, line2, line3, line4)
  heading <- paste0("cells beyond +/-", resid_cutoff, " (adjusted residual)")
  if (weak > 0.2) {
    lines <- c(lines, "table is sparse; treat the test as a rough guide, and rows are not independent.")
  }
  # list the strongest cells, largest first; otherwise say none passed the cutoff
  if (nrow(strongest) > 0) {
    idx <- strongest
    cells <- tibble(
      fam  = rownames(res$stdres)[idx[, "row"]],
      ctry = colnames(res$stdres)[idx[, "col"]],
      z    = res$stdres[idx]
    ) |> arrange(desc(abs(z)))
    # positive residual = more rows than expected, negative = fewer
    dir <- ifelse(cells$z > 0, "more", "fewer")
    zs <- sprintf("%+.1f", cells$z) 
    cell_lines <-paste0(cells$fam, " x ", cells$ctry, ": ", zs, " (", dir, ")" )
    lines <- c(lines, heading, cell_lines)
  } else {
    lines <- c(lines, paste0("no cells beyond +/-", resid_cutoff))
  }
  paste(lines, collapse = "\n")
}

# Categorical x categorical: runs profile_one_cross() for every pair of groupable
# categorical columns. Needs at least two usable columns.
profile_crosstab <- function(df, min_category_size = 10, exclude_columns = NULL) {
  cat_cols <- df[sapply(df, function(x) is.character(x) || is.factor(x))]
  cat_cols <- cat_cols[!names(cat_cols) %in% exclude_columns]
  if (ncol(cat_cols) < 2) return("fewer than two categorical columns")
  groupable <- cat_cols[sapply(cat_cols, keep_col, min_category_size = min_category_size)]
  if (ncol(groupable) < 2) return("fewer than two categorical columns with enough rows")
  # combn() gives each unordered pair once (no column against itself, no repeats); each pair is one column of the matrix
  pairs <- combn(names(groupable), 2)
  blocks <- sapply(seq_len(ncol(pairs)), function(i) {
    profile_one_cross(df, pairs[1, i], pairs[2, i], min_category_size)
  })
  headings <- paste0(pairs[1, ], " x ", pairs[2, ])
  labeled <- paste0(headings, "\n", blocks)
  paste(labeled, collapse = "\n\n")
}

# Helper for profile_cor: Pearson correlation between TWO numeric columns, with
# the sample size and a p-value. Rows missing either value are dropped first.
# Returns a message instead of a number when no correlation can be computed
# (fewer than 3 complete rows, or one column never changes).
profile_one_cor <- function(df, num_a, num_b) {
  pair <- na.omit(df[c(num_a, num_b)])
  if (nrow(pair)<3) return(paste0(num_a, " x ", num_b, ": fewer than 3 complete rows"))
  if (sd(pair[[1]]) == 0 || sd(pair[[2]]) == 0) return(paste0(num_a, " x ", num_b, ": one column is constant; correlation undefined"))
  ct <- cor.test(pair[[1]], pair[[2]])
  # cor.test() has n - 2 degrees of freedom, so n = parameter + 2
  n <- ct$parameter + 2
  p_text <- format_p(ct$p.value)
  paste0(num_a, " x ", num_b, ": r = ", round(unname(ct$estimate), 2), " (n = ", unname(n), ", p", p_text, ")" )
}

# Numeric bivariate: correlation for every pair of numeric columns.
# Needs at least two numeric columns.
profile_cor <- function(df, exclude_columns = NULL) {
  num <- numeric_cols(df, exclude_columns)
  if (ncol(num) < 2) return("fewer than two numeric columns")
  pairs <- combn(names(num), 2)
  lines <- sapply(seq_len(ncol(pairs)), function(i) {
    profile_one_cor(df, pairs[1, i], pairs[2, i])
  })
  paste(lines, collapse = "\n")
}

# Helper: formats a p-value for the report. Very small values print as
# "< 0.0005", because simulation and rounding cannot support more precision.
# The text starts with a space so it can be pasted straight after a label:
# paste0("p", format_p(p)) gives "p < 0.0005" or "p = 0.0123".
format_p <- function(p) {
  if (is.na(p)) return(" = undefined")
  if (p < 0.0005) " < 0.0005" else paste0(" = ", format(round(p, 4), scientific = FALSE))
}

# Helper: formats numbers for the report.
# Whole numbers keep every digit (no scientific notation); decimals get four
# significant digits. sign = TRUE adds a leading + to positive numbers.
# 4 significant digits, thousands separators, explicit sign when asked
fmt_num <- function(x, sign = FALSE) {
  out <- formatC(x, format = "fg", digits = 4, big.mark = ",")
  if (sign) out <- ifelse(x >= 0, paste0("+", out), out)
  trimws(out)
}

# Helper: the numeric columns of a table, minus any named in exclude_columns.
# Every section that needs the numeric side uses it.
numeric_cols <- function(df, exclude_columns = NULL) {
  num <- df[sapply(df, is.numeric)]
  num[!names(num) %in% exclude_columns]
}


# ---------------------------------------------------------------------------
# OLS: one-way model per (categorical, numeric) pair, num ~ cat
# ---------------------------------------------------------------------------

# Helper for profile_ols: one-way model "numeric ~ categorical" for ONE pair.
# Reports R-squared, the F-test, and each category's difference from a reference
# group (the first level alphabetically).
# max_terms: how many group differences to list, largest first.
# skew_ratio: add a caveat when mean / median of the outcome exceeds this.
profile_one_ols <- function(df, cat_col, num_col, min_category_size = 10, max_terms = 10, skew_ratio = 3) {
  # shrink to categories with enough rows, as in the crosstab
  keep <- big_enough(df[[cat_col]], min_category_size)
  if (length(keep) < 2) {
    return(paste0("fewer than two categories with at least ", min_category_size, " rows"))
  }
  # keep just the two columns the model needs, and drop rows missing either
  small <- df |>
    filter(.data[[cat_col]] %in% keep) |>
    select(all_of(c(cat_col, num_col))) |>
    stats::na.omit()
  small[[cat_col]] <- factor(small[[cat_col]])     # drops levels that no longer appear
  # levs[1] is the reference group; every other level gets its own coefficient
  levs <- levels(small[[cat_col]])
  # guards: a model needs two or more categories and an outcome that varies
  if (length(levs) < 2) return("fewer than two categories left after removing missing values")
  if (sd(small[[num_col]]) == 0) return("numeric column is constant; model undefined")
  
  # backticks let column names with spaces or a leading digit (like 2004-05) work in a formula
  f <- reformulate(paste0("`", cat_col, "`"), response = paste0("`", num_col, "`"))
  # fit the model; glance() is a one-row model summary, tidy() has one row per coefficient
  fit <- stats::lm(f, data = small)
  g <- broom::glance(fit)
  co <- broom::tidy(fit) |> filter(term != "(Intercept)")
  co$level <- levs[-1]       # treatment contrasts: one row per non-reference level, in level order
  
  line1 <- paste0(nrow(small), " rows, ", length(levs), " groups; reference group: ", levs[1],
                  " (mean ", fmt_num(unname(stats::coef(fit)[1])), ")")
  line2 <- paste0("R-squared = ", round(g$r.squared, 3), " (adjusted ", round(g$adj.r.squared, 3), "); ",
                  "F(", g$df, ", ", g$df.residual, ") = ", fmt_num(g$statistic), ", p", format_p(g$p.value))
  # crude skew screen: a mean far above the median means a few large values pull the average
  x <- small[[num_col]]
  skew_line <- NULL
  if (median(x) > 0 && mean(x) / median(x) > skew_ratio) {
    skew_line <- "outcome is highly skewed; a few large values can drive R-squared"
  }
  # list the biggest differences first, capped at max_terms
  co <- co |> arrange(desc(abs(estimate)))
  shown <- head(co, max_terms)
  coef_lines <- paste0("  ", shown$level, " vs ", levs[1], ": ", fmt_num(shown$estimate, sign = TRUE),
                       " (SE ", fmt_num(shown$std.error), ", p", sapply(shown$p.value, format_p), ")")
  if (nrow(co) > max_terms) {
    coef_lines <- c(coef_lines, paste0("  ... and ", nrow(co) - max_terms, " more groups"))
  }
  paste(c(line1, line2, "differences from reference group:", coef_lines, skew_line), collapse = "\n")
}

# OLS: runs profile_one_ols() for every categorical x numeric pairing.
# Each model has one predictor, so effects are not adjusted for each other
# (the closing note says so).
profile_ols <- function(df, min_category_size = 10, exclude_columns = NULL, skew_ratio = 3) {
  num <- numeric_cols(df, exclude_columns)
  if (ncol(num) == 0) return("no numeric columns")
  cat_cols <- df[sapply(df, function(x) is.character(x) || is.factor(x))]
  cat_cols <- cat_cols[!names(cat_cols) %in% exclude_columns]
  if (ncol(cat_cols) == 0) return("no categorical columns")
  groupable <- cat_cols[sapply(cat_cols, keep_col, min_category_size = min_category_size)]
  if (ncol(groupable) == 0) return("no categorical columns with enough rows")
  pairs <- crossing(cat_col = names(groupable), num_col = names(num))
  blocks <- map2_chr(pairs$cat_col, pairs$num_col, function(cat_name, num_name) {
    profile_one_ols(df, cat_name, num_name, min_category_size, skew_ratio = skew_ratio)
  })
  headings <- paste0(pairs$cat_col, " -> ", pairs$num_col)
  labeled <- paste0(headings, "\n", blocks)
  note <- paste0("each model has one categorical predictor, so effects are not adjusted for each other; ",
                 "rows are treated as independent; p-values are not adjusted for multiple comparisons")
  paste(c(labeled, note), collapse = "\n\n")
}

# ---------------------------------------------------------------------------
# Report: run every step, write one Markdown file
# ---------------------------------------------------------------------------

# Runs all ten sections on one table and returns a report object.
# name is used for the report title; min_category_size and exclude_columns are
# passed to the sections that use them.
build_report <- function(df, name = "dataset", min_category_size = 10, exclude_columns = NULL) {
  # each step is wrapped in a function so it can run inside tryCatch() below
  steps <- list(
    "Shape"                      = function() profile_shape(df),
    "Column types"               = function() profile_types(df),
    "Missing values"             = function() profile_missing(df),
    "Duplicate rows"             = function() profile_duplicates(df),
    "Numeric summary"            = function() profile_numeric(df, exclude_columns),
    "Categorical summary"        = function() profile_categorical(df, exclude_columns),
    "Group means"                = function() profile_groups(df, min_category_size, exclude_columns),
    "Categorical x categorical"  = function() profile_crosstab(df, min_category_size, exclude_columns),
    "Numeric bivariate"          = function() profile_cor(df, exclude_columns),
    "OLS"                        = function() profile_ols(df, min_category_size, exclude_columns)
  )
  # one failing step shouldn't lose the rest of the report
  sections <- lapply(steps, function(f) {
    tryCatch(f(), error = function(e) paste0("step failed: ", cli::ansi_strip(conditionMessage(e))))
  })
  # the class marks the object; write_report() reads its name and sections
  structure(list(name = name, sections = sections), class = "eda_report")
}

# Writes a report object to a Markdown file: a title, a timestamp, then one "##"
# heading per section with its text in a code fence.
# Creates the output folder if needed. Returns the path invisibly.
write_report <- function(report, path) {
  # plain-text sections go in code fences so line breaks survive Markdown rendering
  body <- paste0("## ", names(report$sections), "\n\n```\n", unlist(report$sections), "\n```\n")
  text <- c(paste0("# EDA Report: ", report$name),
            paste0("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M")),
            "",
            body)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(paste(text, collapse = "\n"), path)
  invisible(path)
}

# Usage:
#   report <- build_report(df, name = "my_data", min_category_size = 10, exclude_columns = c("id"))
#   write_report(report, "output/my_data_eda.md")


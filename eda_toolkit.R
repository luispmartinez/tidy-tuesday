library(tidyverse)
library(broom)

profile_shape <- function(df) {
  paste0(nrow(df), " rows x ", ncol(df), " columns")
}

profile_types <- function(df) {
  types <- sapply(df, function(x) class(x)[1])
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

profile_numeric <- function(df, exclude_columns = NULL) {
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
profile_one_cat <- function(x, n = 5) {
  t <- sort(table(x), decreasing = TRUE)
  lines <- paste0(names(head(t, n)), ": ", head(t, n))
  if ( length(t) > n){
    lines <- c(lines, paste0("... and ", length(t) - n, " more values"))
  }
  paste(lines, collapse = "\n")
}

profile_categorical <- function(df, exclude_columns = NULL){
  cat_cols <- df[sapply(df, function(x) is.character(x) || is.factor(x))]
  cat_cols <- cat_cols[!names(cat_cols) %in% exclude_columns]
  if (ncol(cat_cols) == 0) return ("no categorical columns")
  blocks <- sapply(cat_cols, profile_one_cat)
  labeled <- paste0(names(blocks), "\n", blocks)
  paste(labeled, collapse = "\n\n")
}

keep_col <- function(x, min_category_size = 10) {
            length(x)/ n_distinct(x) >=min_category_size
            # return TRUE if the column should be grouped, FALSE if skipped
     }

profile_groups <- function(df, min_category_size = 10, exclude_columns = NULL ){
  num <- numeric_cols(df, exclude_columns)
  if (ncol(num) == 0) return("no numeric columns")
  cat_cols <- df[sapply(df, function(x) is.character(x) || is.factor(x))]
  cat_cols <- cat_cols[!names(cat_cols) %in% exclude_columns]
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

profile_one_cross <- function(df, cat_a, cat_b, min_category_size = 10, resid_cutoff = 5, exclude_columns = NULL) {
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
  if (nrow(strongest) > 0) {
    idx <- strongest
    cells <- tibble(
      fam  = rownames(res$stdres)[idx[, "row"]],
      ctry = colnames(res$stdres)[idx[, "col"]],
      z    = res$stdres[idx]
    ) |> arrange(desc(abs(z)))
    dir <- ifelse(cells$z > 0, "more", "fewer")
    zs <- sprintf("%+.1f", cells$z) 
    cell_lines <-paste0(cells$fam, " x ", cells$ctry, ": ", zs, " (", dir, ")" )
    lines <- c(lines, heading, cell_lines)
  } else {
    lines <- c(lines, paste0("no cells beyond +/-", resid_cutoff))
  }
  paste(lines, collapse = "\n")
}

profile_crosstab <- function(df, min_category_size = 10, exclude_columns = NULL) {
  cat_cols <- df[sapply(df, function(x) is.character(x) || is.factor(x))]
  cat_cols <- cat_cols[!names(cat_cols) %in% exclude_columns]
  if (ncol(cat_cols) < 2) return("fewer than two categorical columns")
  groupable <- cat_cols[sapply(cat_cols, keep_col, min_category_size = min_category_size)]
  if (ncol(groupable) < 2) return("fewer than two categorical columns with enough rows")
  pairs <- combn(names(groupable), 2)
  blocks <- sapply(seq_len(ncol(pairs)), function(i) {
    profile_one_cross(df, pairs[1, i], pairs[2, i], min_category_size)
  })
  headings <- paste0(pairs[1, ], " x ", pairs[2, ])
  labeled <- paste0(headings, "\n", blocks)
  paste(labeled, collapse = "\n\n")
}

profile_one_cor <- function(df, num_a, num_b) {
  pair <- na.omit(df[c(num_a, num_b)])
  if (nrow(pair)<3) return(paste0(num_a, " x ", num_b, ": fewer than 3 complete rows"))
  if (sd(pair[[1]]) == 0 || sd(pair[[2]]) == 0) return(paste0(num_a, " x ", num_b, ": one column is constant; correlation undefined"))
  ct <- cor.test(pair[[1]], pair[[2]])
  n <- ct$parameter + 2
  p_text <- format_p(ct$p.value)
  paste0(num_a, " x ", num_b, ": r = ", round(unname(ct$estimate), 2), " (n = ", unname(n), ", p", p_text, ")" )
}

profile_cor <- function(df, exclude_columns = NULL) {
  num <- numeric_cols(df, exclude_columns)
  if (ncol(num) < 2) return("fewer than two numeric columns")
  pairs <- combn(names(num), 2)
  lines <- sapply(seq_len(ncol(pairs)), function(i) {
    profile_one_cor(df, pairs[1, i], pairs[2, i])
  })
  paste(lines, collapse = "\n")
}

format_p <- function(p) {
  if (is.na(p)) return(" = undefined")
  if (p < 0.0005) " < 0.0005" else paste0(" = ", format(round(p, 4), scientific = FALSE))
}

# 4 significant digits, thousands separators, explicit sign when asked
fmt_num <- function(x, sign = FALSE) {
  out <- formatC(x, format = "fg", digits = 4, big.mark = ",")
  if (sign) out <- ifelse(x >= 0, paste0("+", out), out)
  trimws(out)
}

numeric_cols <- function(df, exclude_columns = NULL) {
  num <- df[sapply(df, is.numeric)]
  num[!names(num) %in% exclude_columns]
}


# ---------------------------------------------------------------------------
# OLS: one-way model per (categorical, numeric) pair, num ~ cat
# ---------------------------------------------------------------------------

profile_one_ols <- function(df, cat_col, num_col, min_category_size = 10, max_terms = 10, skew_ratio = 3) {
  keep <- big_enough(df[[cat_col]], min_category_size)
  if (length(keep) < 2) {
    return(paste0("fewer than two categories with at least ", min_category_size, " rows"))
  }
  small <- df |>
    filter(.data[[cat_col]] %in% keep) |>
    select(all_of(c(cat_col, num_col))) |>
    stats::na.omit()
  small[[cat_col]] <- factor(small[[cat_col]])     # drops levels that no longer appear
  levs <- levels(small[[cat_col]])
  if (length(levs) < 2) return("fewer than two categories left after removing missing values")
  if (sd(small[[num_col]]) == 0) return("numeric column is constant; model undefined")
  
  f <- reformulate(paste0("`", cat_col, "`"), response = paste0("`", num_col, "`"))
  fit <- stats::lm(f, data = small)
  g <- broom::glance(fit)
  co <- broom::tidy(fit) |> filter(term != "(Intercept)")
  co$level <- levs[-1]       # treatment contrasts: one row per non-reference level, in level order
  
  line1 <- paste0(nrow(small), " rows, ", length(levs), " groups; reference group: ", levs[1],
                  " (mean ", fmt_num(unname(stats::coef(fit)[1])), ")")
  line2 <- paste0("R-squared = ", round(g$r.squared, 3), " (adjusted ", round(g$adj.r.squared, 3), "); ",
                  "F(", g$df, ", ", g$df.residual, ") = ", fmt_num(g$statistic), ", p", format_p(g$p.value))
  x <- small[[num_col]]
  skew_line <- NULL
  if (median(x) > 0 && mean(x) / median(x) > skew_ratio) {
    skew_line <- "outcome is highly skewed; a few large values can drive R-squared"
  }
  co <- co |> arrange(desc(abs(estimate)))
  shown <- head(co, max_terms)
  coef_lines <- paste0("  ", shown$level, " vs ", levs[1], ": ", fmt_num(shown$estimate, sign = TRUE),
                       " (SE ", fmt_num(shown$std.error), ", p", sapply(shown$p.value, format_p), ")")
  if (nrow(co) > max_terms) {
    coef_lines <- c(coef_lines, paste0("  ... and ", nrow(co) - max_terms, " more groups"))
  }
  paste(c(line1, line2, "differences from reference group:", coef_lines, skew_line), collapse = "\n")
}

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

build_report <- function(df, name = "dataset", min_category_size = 10, exclude_columns = NULL) {
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
  structure(list(name = name, sections = sections), class = "eda_report")
}

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


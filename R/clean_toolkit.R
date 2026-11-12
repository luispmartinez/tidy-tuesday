# R/clean_toolkit.R
# Reusable cleaning helpers: clean_text, split_range, top_repeated, write_clean. Source after eda_toolkit.R.
# Needs: dplyr, stringr, tibble (all in the tidyverse).

NA_STRINGS <- c("not applicable", "not applicable.", "n/a", "na", "none", "null", "")

# --- Cleaning ---------------------------------------------------------------
# For every character column:
#   1. trim and collapse whitespace
#   2. turn placeholder strings ("Not applicable.", "n/a", "") into NA
#   3. if every remaining value is a plain number, convert the column to numeric
#   4. otherwise, if the column is categorical (<= max_levels distinct values after
#      step 2): standardize spacing around "/" and ",", then lowercase.
#      Free text and names (description, taxonomic_name) keep their original form.
# Returns list(data = cleaned data, log = one row per changed column).
clean_text <- function(data, na_strings = NA_STRINGS, max_levels = 15,
                       exclude = character(0)) {
  log <- list()
  for (col in setdiff(names(data), exclude)) {
    x <- data[[col]]
    if (!is.character(x)) next
    before_levels <- dplyr::n_distinct(x, na.rm = TRUE)
    before_na     <- sum(is.na(x))

    x <- stringr::str_squish(x)
    squished_levels <- dplyr::n_distinct(x, na.rm = TRUE)
    x[tolower(x) %in% na_strings] <- NA_character_
    placeholder_levels <- squished_levels - dplyr::n_distinct(x, na.rm = TRUE)  # distinct placeholder strings removed

    # a column whose every non-missing value is a plain number becomes numeric
    vals <- x[!is.na(x)]
    to_numeric  <- length(vals) > 0 && all(stringr::str_detect(vals, "^-?\\d+(\\.\\d+)?$"))
    categorical <- !to_numeric && dplyr::n_distinct(x, na.rm = TRUE) <= max_levels

    if (to_numeric) {
      x <- as.numeric(x)
    } else if (categorical) {
      x <- stringr::str_replace_all(x, "\\s*/\\s*", "/")     # "a/ b" -> "a/b"
      x <- stringr::str_replace_all(x, "\\s*,\\s*", ", ")    # "a ,b" -> "a, b"
      x <- tolower(x)
    }

    after_levels <- dplyr::n_distinct(x, na.rm = TRUE)
    after_na     <- sum(is.na(x))
    if (to_numeric || after_levels != before_levels || after_na != before_na) {
      log[[col]] <- tibble::tibble(
        column = col, na_added = after_na - before_na,
        levels_before = before_levels, levels_after = after_levels,
        lowercased = categorical, to_numeric = to_numeric,
        placeholder_levels = placeholder_levels
      )
    }
    data[[col]] <- x
  }
  list(data = data, log = dplyr::bind_rows(log))
}

# --- Optional: split range text ("7-30", "60-", "26") into _min / _max ----------
split_range <- function(data, cols) {
  for (col in cols) {
    m <- stringr::str_match(
      data[[col]], "^\\s*(\\d+\\.?\\d*)\\s*(-\\s*(\\d+\\.?\\d*)?)?\\s*$")
    lo <- as.numeric(m[, 2])
    hi <- ifelse(is.na(m[, 3]), lo, as.numeric(m[, 4]))  # single value: min = max
    data[[paste0(col, "_min")]] <- lo
    data[[paste0(col, "_max")]] <- hi
  }
  data
}

# --- Spot placeholders -------------------------------------------------------
# Most repeated values in one column, with their share of all rows.
# A huge value repeated many times (e.g. 999,999,999,999) is the usual sign of a
# numeric placeholder. Decide the rule yourself in the dataset's clean script.
top_repeated <- function(df, col, n = 10) {
  df |>
    dplyr::count(value = .data[[col]], sort = TRUE, name = "rows") |>
    dplyr::slice_head(n = n) |>
    dplyr::mutate(share = round(rows / nrow(df), 4))
}

# --- Write the clean CSV and verify it ---------------------------------------
# Creates the folder, writes the file, reads it back and checks rows and columns
# match. Returns the path invisibly.
write_clean <- function(df, folder, filename) {
  folder <- path.expand(folder)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(folder, filename)
  readr::write_csv(df, path)
  check <- readr::read_csv(path, show_col_types = FALSE)
  stopifnot(
    "written file row count does not match"    = nrow(check) == nrow(df),
    "written file column count does not match" = ncol(check) == ncol(df)
  )
  invisible(path)
}

# Step 1: Load the toolkit
# (source("eda_toolkit.R"), first line, so the tidyverse and the report functions exist)
source("eda_toolkit.R")

# Step 2: Settings
# (DROP_FOLDER, REPORT_FOLDER, SKIP_EXISTING; the values you change from week to week)
DROP_FOLDER   <- "2026-01-13/drop"
REPORT_FOLDER <- "reports"
SKIP_EXISTING <- TRUE

# Step 3: Read one input
# (read_input: a CSV, or one sheet of a workbook)
read_input <- function(path, sheet = NULL) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv"){
    read_csv(path, show_col_types = FALSE)
  } else if (ext %in% c("xlsx", "xls")){
    readxl::read_excel(path, sheet = sheet)
  } else {
    stop(paste0("unsupported file type '", ext, "': ", path))
  }
}

# Step 4: List the inputs in a file
# (list_sheets: NA for a CSV, sheet names for a workbook, nothing for anything else)
list_sheets <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv"){
    NA_character_
  } else if (ext %in% c("xlsx", "xls")){
    readxl::excel_sheets(path)
  } else {
    character(0)
  }
}

# Step 5: Name the report
# (report_name: file stem, plus a cleaned sheet name for workbooks)
report_name <- function(path, sheet = NA_character_) {
  stem <- gsub("[^A-Za-z0-9_]+", "_", tools::file_path_sans_ext(basename(path)))
  if (is.na(sheet)) return (stem)
  cleaned <- gsub("[^A-Za-z0-9_]+", "_", sheet)
  paste0(stem, "__", cleaned)
}

# Step 6: Profile one input
# (profile_input: skip if the report exists, otherwise read, build, write; returns a status)
profile_input <- function(path, sheet = NA_character_, out_dir = "reports", skip_existing = TRUE) {
  name <- report_name(path, sheet)
  out_path <- file.path(out_dir, paste0(name, "_eda.md"))
  if (file.exists(out_path) && skip_existing) return("skipped")
  tryCatch({
    data <- read_input(path, sheet = if (is.na(sheet)) NULL else sheet)
    if (nrow(data) == 0 || ncol(data) == 0) stop("input has no data")
    report <- build_report(data, name)
    write_report(report, out_path)
    "written"
  }, error = function(e) paste0("failed: ", cli::ansi_strip(conditionMessage(e))))
}

# Step 7: Find the files
# (list.files with the csv/xls/xlsx pattern, then drop Excel's ~$ lock files)
files <- list.files(DROP_FOLDER , pattern = "\\.(csv|xlsx?)$", full.names = TRUE, ignore.case = TRUE)
files <- files[!startsWith(basename(files), "~$")]

# Step 8: Build the inputs table
# (one row per file, or per sheet for workbooks: tibble, map, unnest)
inputs <- tibble(path = files) |> mutate(sheet = map(path, list_sheets)) |> unnest(sheet)

# Step 9: Run every input
# (map2_chr over the table, calling profile_input once per row; keep the statuses)
status <- map2_chr(inputs$path, inputs$sheet, function(p, s) profile_input(p, s, REPORT_FOLDER, SKIP_EXISTING))

# Step 10: Summary
# (how many written, skipped and failed, and the failure messages)
print(table(sub(":.*", "", status)))
print(inputs |> mutate(status = status) |> filter(startsWith(status, "failed")))

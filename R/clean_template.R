# clean_template.R
# Copy to clean_<dataset>.R, then fill in every line marked CHANGE ME.
# Run top to bottom; steps 5 and 6 are meant to be looked at before you decide.

# Step 1: Load packages and the toolkit
library(tidyverse)
source("R/clean_toolkit.R")

# Step 2: Settings
INPUT_PATH    <- "YYYY-MM-DD/drop/file.csv"       # CHANGE ME
OUTPUT_FOLDER <- "~/TableauData/Tidy Tuesday"
OUTPUT_FILE   <- "dataset_clean.csv"              # CHANGE ME
CLEAN_EXCLUDE <- character(0)                     # columns clean_text() must leave alone (IDs, codes)
EXPECTED_ROWS <- NA                               # CHANGE ME: rows in the raw file, from the first run

# Step 3: Read the raw file
if (!file.exists(INPUT_PATH)) stop("Input not found: ", INPUT_PATH)
df <- read_csv(INPUT_PATH, show_col_types = FALSE)
if (nrow(df) == 0) stop("input has no data")
cat("raw rows:", nrow(df), " columns:", ncol(df), "\n")

# Step 4: Text hygiene (whitespace, placeholder strings to NA, case variants)
cleaned <- clean_text(df, exclude = CLEAN_EXCLUDE)
print(cleaned$log)                                # read this: is every change what you want?
d <- cleaned$data

# Step 5: Duplicates
# Q: how many exact duplicate rows are there, and should they go?
cat("duplicate rows:", sum(duplicated(d)), "\n")
d <- d |> distinct()

# Step 6: Placeholder numbers
# Q: which values repeat suspiciously often? Where do you draw the line?
print(top_repeated(d, "COLUMN"))                  # CHANGE ME: a numeric column
d <- d |> mutate(
  placeholder_flag = FALSE                        # CHANGE ME: your rule, e.g. COLUMN >= 999999999999
)

# Step 7: Derived columns
# Q: what does the chart need that the raw data does not have? (log scale, share, bucket)
# d <- d |> mutate(...)

# Step 8: Checks
# Q: what must be true after cleaning? Fill in the numbers from your own inspection.
if (is.na(EXPECTED_ROWS)) stop("Set EXPECTED_ROWS (rows in the raw file) before running the checks")
stopifnot(
  "drop file has a different row count" = nrow(df) == EXPECTED_ROWS
  # "row count after cleaning is wrong"   = nrow(d) == ...,
  # "key column has missing values"       = !anyNA(d$KEY),
  # "flag count is wrong"                 = sum(d$placeholder_flag) == ...
)

# Step 9: Write the clean CSV (writes, reads back, checks rows and columns)
path <- write_clean(d, OUTPUT_FOLDER, OUTPUT_FILE)
cat("wrote", path, "\n")

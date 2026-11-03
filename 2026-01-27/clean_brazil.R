# Step 1: Load packages
library(tidyverse)

# Step 2: Settings
INPUT_PATH    <- "2026-01-27/drop/companies.csv"
OUTPUT_FOLDER <- "~/TableauData/Tidy Tuesday"

# Step 3: Read the raw file
df <- read_csv(INPUT_PATH, show_col_types = FALSE)

# Step 4: Remove exact duplicate rows
# Q: the profile said 0 duplicate rows. Do you still want this step? Why or why not?
d <- df |> distinct()

# Step 5: Flag placeholder capital
# Q: capital_stock has a median of 400,000 but a max of 999,999,999,999.
#    How would you list the most repeated capital_stock values to spot placeholders?
#    (hint: count(), sort = TRUE)
# Q: which values do you treat as placeholders, and where do you draw the line?
d <- d |> mutate(
  capital_flag = (str_detect(company_name, "INCORPORADORA") & capital_stock >= 2e10) |
    (capital_stock >= 999999999999)
)

# Step 6: Add a log10 capital column
# Q: why log10 instead of the raw value for a chart? What happens to a zero?
d <- d |> mutate(log_capital = log10(capital_stock))

# Step 7: Checks
# Q: what must be true about the data after cleaning?
#    (row count, no missing values, flagged rows, the number of company_size groups)
stopifnot(
  "row count is not 141332"         = nrow(d) == 141332,
  "capital_stock has missing"    = !anyNA(d$capital_stock),
  "company_size count is not 3" = n_distinct(d$company_size) == 3,
  "drop file has a different row count" = nrow(df) == 141332,
  "flag count is not 233" = sum(d$capital_flag) == 233
)

# Step 8: Write the clean CSV
dir.create(OUTPUT_FOLDER, recursive = TRUE, showWarnings = FALSE)
write_csv(d, file.path(OUTPUT_FOLDER, "brazil_companies_clean.csv"))
check <- read_csv(file.path(OUTPUT_FOLDER, "brazil_companies_clean.csv"), show_col_types = FALSE)
stopifnot(
  "written file does not match" = nrow(check) == nrow(d),
  "written file does not have 8 columns" = ncol(check) == 8
)

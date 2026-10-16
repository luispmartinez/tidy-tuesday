
# Step 1: Load packages
library(tidyverse)


# Step 2: Settings
INPUT_PATH <- '2026-01-20/drop/apod.csv'

# Step 3: Read the file
apod <- read_csv(INPUT_PATH, show_col_types = FALSE)

# Step 4: Derive columns (mutate)
apod <- apod |> mutate(
  year          = year(date),               # lubridate
  month         = month(date, label = TRUE),
  n_words       = str_count(explanation, "\\S+"),  # stringr: count words
  has_copyright = !is.na(copyright),
  has_hd        = !is.na(hdurl),
  is_video      = media_type == "video"
)

# Step 5: Flag re-runs
# (group_by explanation, count rows per group; is_rerun = TRUE when n > 1)
apod <- apod |> group_by(explanation) |> mutate(n_runs = n(), is_rerun = n_runs > 1) |> ungroup()

# Step 6: Save, then re-profile
write_csv(apod, file.path("2026-01-20", "drop", "apod_prepped.csv"))

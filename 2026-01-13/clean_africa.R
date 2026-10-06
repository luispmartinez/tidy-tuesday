# Step 1: Load packages
library(tidyverse)

# Step 2: Settings
INPUT_PATH <- '2026-01-13/drop/africa.csv'
OUTPUT_FOLDER <- '~/TableauData/Tidy Tuesday'

# Step 3: Read the raw file
df <- read_csv(INPUT_PATH, show_col_types = FALSE)
# Step 4: Remove exact duplicate rows
d <- df |> distinct()
# Step 5: Give each language a unique id
d <- df |> distinct() |> group_by(language) |> mutate(n_counts = n_distinct(native_speakers)) |> ungroup()
d <- d |> mutate(
  language_id = if_else(n_counts > 1,
                        paste0(language, " (", format(native_speakers, big.mark = ",", trim = TRUE), ")"),
                        language)
)
# Step 6: Clean the family column

# Step 7: Checks
# Step 8: Write the clean CSV
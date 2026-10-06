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
d <- d |> group_by(language) |> mutate(n_counts = n_distinct(native_speakers)) |> ungroup()
d <- d |> mutate(
  language = if_else(n_counts > 1,
                        paste0(language, " (", format(native_speakers, big.mark = ",", trim = TRUE), ")"),
                        language)
)
# Step 6: Clean the family column
d <- d |> mutate(family_raw = family)
d <- d |> mutate(
  family = case_when(
    family == "Afro-Asiatic" ~ "Afroasiatic",
    family %in% c("Arabic-based","English", "French", "Kongo-based","Portuguese" ) ~ "Creole / pidgin",
    family == "Language" ~ "Unclassified",
    .default = family
  )
)
# Step 7: Checks
stopifnot(
  "row count is not 762" = nrow(d) == 762,
  "language_id has missing values" = !anyNA(d$language_id),
  "family has missing values"      = !anyNA(d$family),
  "country has missing values"     = !anyNA(d$country),
  "native_speakers has missing values" = !anyNA(d$native_speakers),
  "a language appears more than once in a country" = nrow(count(d, language_id, country) |> filter(n > 1)) == 0,
  "a language_id maps to more than one speaker count" = nrow(d |> group_by(language_id) |> summarise(k = n_distinct(native_speakers)) |> filter(k > 1)) == 0,
  "an old family label is still in family" = !any(d$family %in% c("Afro-Asiatic", "Language", "English", "French", "Portuguese", "Arabic-based", "Kongo-based")),
  "family count is not 12" = n_distinct(d$family) == 12,
  "drop file has a different row count" = nrow(df) == 796
)
# Step 8: Write the clean CSV
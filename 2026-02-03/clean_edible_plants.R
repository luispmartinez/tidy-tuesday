# clean_edible_plants.R
# Every ___ is a blank for you to fill in. Each Q: is a question to answer first.
# A ___ is not valid R, so "Source" will stop at the first blank on purpose.
# Work down the file one step at a time (select the lines, Cmd+Enter) and look at what
# each step prints before you write the next one.

# Step 1: Load packages and the toolkit
library(tidyverse)
source("R/clean_toolkit.R")
# Q: clean_toolkit.R holds four helpers. Open it and name the ones this script will use.

# Step 2: Settings
INPUT_PATH <- "2026-02-03/drop/edible_plants.csv"
OUTPUT_FOLDER <- "~/TableauData/Edible Plants/work"
OUTPUT_FILE   <- "edible_plants_clean.csv"
CLEAN_EXCLUDE <- c("harvest_status")   # real label we wrote; clean_text() would read "not applicable" as a placeholder
if (!file.exists(INPUT_PATH)) stop("Input not found: ", INPUT_PATH)
# Q: clean_text() turns strings like "none" or "n/a" into NA. Does any column in this data
#    have a real value that looks like a placeholder? What does the cleaning log tell you?
#    (hint: if the answer is no, what is the "empty" value for a vector of column names?)

# Step 3: Read the raw file
df <- read_csv(INPUT_PATH, show_col_types = FALSE)
cat("raw rows:", nrow(df), " columns:", ncol(df), "\n")
EXPECTED_ROWS <- 140
# Q: where in the EDA report would you look to confirm how many rows the raw file has?

# the 30 fruit/nut plants say "Not applicable." in all three germination/harvest columns;
# clean_text() would turn that into NA, so record it first
df <- df |> mutate(
  harvest_status = case_when(
    str_to_lower(str_squish(days_harvest)) == "not applicable." ~ "not applicable",
    is.na(days_harvest)                                         ~ "unknown",
    TRUE                                                        ~ "reported"
  )
)

# the 30 "Not applicable." rows must be the same plants in all three columns,
# or one harvest_status column cannot stand in for all three
rows_h <- which(str_to_lower(str_squish(df$days_harvest))           == "not applicable.")
rows_g <- which(str_to_lower(str_squish(df$days_germination))       == "not applicable.")
rows_t <- which(str_to_lower(str_squish(df$temperature_germination)) == "not applicable.")

stopifnot(
  "germination days differ from harvest days"        = identical(rows_h, rows_g),
  "germination temperature differs from harvest days" = identical(rows_h, rows_t),
  "expected 30 not-applicable rows"                   = length(rows_h) == 30
)

# Step 4: Text hygiene
cleaned <- clean_text(df, exclude = CLEAN_EXCLUDE)
print(cleaned$log)
d <- cleaned$data
# Q: the log shows 7 columns changed. For each one, can you say in a sentence what
#    changed and why it is safe? Which three columns gained 30 NAs, and where did those
#    NAs come from?

# Step 5: Duplicates
d <- d |> distinct()
# Q: the profile said 0 duplicate rows. Do you still want this step? Why or why not?
#    (hint: which function removes exact duplicate rows? You used it in clean_brazil.R)

# Step 6: Fix known typos and stray values
# feverfew: documented short-lived perennial; "annual/perennial" resolved to perennial.
# rosemary: "shrub" is a plant form, not a season, so blank instead of guess
# season = life cycle only. Resolved when documented (feverfew), blanked when not (rosemary).
d <- d |> mutate(season = str_replace_all(season, "perrenial|perannial", "perennial"))
d <- d |> mutate(
  season = case_when(
    season == "shrub"             ~ NA_character_,
    season %in% c("perennial evergreen", "semi-evergreen perennial", "annual/perennial") ~ "perennial",
    season %in% c("biennial, grown as annual", "biennial, grown as an annual") ~ "biennial",
    TRUE                          ~ season
  )
)
# nutrients: potassium note moved to nutrients_note, 
# level set to "high", and # "medium to high" left as is: mapping it would be a guess.
d <- d |> mutate(
  nutrients_note = if_else(str_detect(nutrients, "^high potassium"), nutrients, NA_character_),
  nutrients = if_else(str_detect(nutrients, "^high potassium"), "high", nutrients)
)

# cultivation: source labels conflict within a species or have spelling variants; fix when the data itself shows the answer
# cabbage: Brassica oleracea labeled legume, same species as the other brassicas
# horseradish: "brassicas" is a spelling variant of "brassica"
# tomatoes: "solanum" is a genus inside Solanaceae; aubergine and potato are labeled solanaceae
# beetroot: Beta vulgaris labeled umbelliferae; chard and spinach, same species, are chenopodiaceae
d <- d |> mutate(
  cultivation = case_when(
    taxonomic_name == "Brassica oleracea" & cultivation == "legume" ~ "brassica",    # the cabbage row
    cultivation == "brassicas"          ~ "brassica",    # horseradish spelling
    cultivation == "solanum"          ~ "solanaceae",    # the two tomatoes
    str_detect(taxonomic_name, "^Beta vulgar") & cultivation == "umbelliferae" ~ "chenopodiaceae",    # beetroot
    TRUE                          ~ cultivation
  )
)

# Q: "medium to high" appears once. The profile ignores groups under 10 rows. Does it
#    need fixing here? What would you lose or gain by touching it?

# Step 7: Flag, then split, the numeric-looking text

# Rhubarb: days_harvest is "x", a placeholder, not a harvest time, so blank instead of guess
d <- d |> mutate(days_harvest = if_else(days_harvest == "x", NA_character_, days_harvest))
# Q: harvest_continual will be NA wherever days_harvest is NA. Is that what you want?
#    What should a plant with no harvest data get: FALSE, NA, or something else? 
# NA rows are the 30 “not applicable” and the 27 unknown, plus Rhubarb. harvest_status already tells those groups apart, so NA in the flag is fine.

# Q: days_harvest has two values that are not numbers or ranges: "continual" and "x".
#    Run count(d, days_harvest) # "continual" is a real answer (Chives), kept as harvest_continual
#    Which is which, and what happens to each when split_range() runs?  
d <- d |> mutate(
  harvest_continual = days_harvest == "continual"
)

# Q: which four columns did the Summary flag as numeric-looking text?
#    (hint: it is the line starting "Numeric-looking text" at the top of the report)
RANGE_COLS <- c("temperature_germination", "temperature_growing", "days_germination", "days_harvest")
d <- split_range(d, RANGE_COLS)
# Q: split_range() adds _min and _max columns and keeps the originals. For a single
#    value like "26", what are min and max? For "60-", what are they? Check with
#    select(d, days_harvest, days_harvest_min, days_harvest_max) |> distinct().

# Step 8: Drop columns
# drop columns 90%+ missing (too sparse to chart); drop original text ranges, _min/_max replace them
too_empty <- names(d)[colMeans(is.na(d)) >= 0.9]
cat("dropping:", too_empty, "\n")
# non-missing values per text column, saved before the drop; Step 9 compares the _min columns to these
text_counts <- sapply(d[RANGE_COLS], function(x) sum(!is.na(x)))
d <- d |> select(-all_of(c(too_empty, RANGE_COLS)))

stopifnot(
  "range columns are missing" = all(c(paste0(RANGE_COLS, "_min"), paste0(RANGE_COLS, "_max")) %in% names(d)),
  "min is above max in a range" = all(min_ok)
)

min_ok <- sapply(RANGE_COLS, function(col) {
  all(d[[paste0(col, "_min")]] <= d[[paste0(col, "_max")]], na.rm = TRUE)
})

other_cols <- c("temperature_germination", "temperature_growing", "days_germination")
lost <- sapply(other_cols, function(col) sum(!is.na(d[[paste0(col, "_min")]])) - unname(text_counts[col]))
# 9: Checks
# the number to compare against is text_counts, saved before the split
# na.rm = TRUE is needed because the open-ended ranges (60-) have a min and a NA max. It would hide a max that’s NA for any other reason.
# it has an answer in the flag but no number to split
disagree  <- which(is.na(d$harvest_continual) != is.na(d$days_harvest_min))      # rows where the flag's NA status differs from days_harvest_min's
continual <- which(d$harvest_continual)      # rows where the flag is TRUE
# cultivation: ten labels, from count(d, cultivation)
cultivation_ok <- c("allium", "brassica", "chenopodiaceae", "cucurbit", "lamiaceae", "legume", "miscellaneous", "salad", "solanaceae", "umbelliferae")
is_brassica <- str_detect(d$taxonomic_name, "^Brassica")

stopifnot(
  "cultivation has an unexpected value"         = all(d$cultivation %in% c(cultivation_ok, NA)),
  "no Brassica plants found"                    = any(is_brassica),
  "a Brassica plant has a different cultivation" = all(d$cultivation[is_brassica] == "brassica")
)

is_beta <- str_detect(d$taxonomic_name, "^Beta vulgar")

stopifnot(
  "no Beta plants found"                      = sum(is_beta) == 3,
  "Beta plants carry more than one cultivation" = n_distinct(d$cultivation[is_beta]) == 1
)

stopifnot(
  "split_range lost values" = all(lost == 0),
  
  # row counts
  "drop file has a different row count" = nrow(df) == EXPECTED_ROWS,
  "row count changed during cleaning"   = nrow(d) == nrow(df),
  
  # allowed values
  "season has an unexpected value" = all(d$season %in% c("annual", "biennial", "perennial", NA)),
  "nutrients has an unexpected value"   = all(d$nutrients %in% c("high", "medium", "medium to high", "low")),
  "harvest_status has an unexpected value" = all(d$harvest_status %in% c("reported", "unknown", "not applicable")),
  
  # the flag and the status must agree with the data
  "flag and days_harvest_min disagree somewhere other than continual" = identical(disagree, continual),
  
  # split_range lost nothing
  "days_harvest_min lost values" =
    sum(!is.na(d$days_harvest_min)) == text_counts["days_harvest"] - length(continual),
  
  # ranges and pH
  "ph range is inverted"        = all(d$preferred_ph_lower <= d$preferred_ph_upper),
  
  # nothing left unfilled
  "unfilled blank left in the data" = !any(d == "___", na.rm = TRUE)
)

# Q: for the last check, which number do you compare against, and where does it come
#    from? (hint: count the non-missing values in the text column BEFORE splitting it)
# Q: the min/max check uses na.rm = TRUE. Why is that needed, and what does it hide?

# Step 10: Write the clean CSV (writes, reads back, checks rows and columns)
path <- write_clean(d, OUTPUT_FOLDER, OUTPUT_FILE)
cat("wrote", path, "\n")
# Q: the read-back check compares rows and columns. What could still be wrong in the
#    written file that it would not catch? (hint: think about types)

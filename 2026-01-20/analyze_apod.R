# Step 1: Load packages
library(tidyverse)
library(quanteda.textstats)
library(tidytext)

# Step 2: Settings
INPUT_PATH <- '2026-01-20/drop/apod_prepped.csv'

# Step 3: Read the file
apod <- read_csv(INPUT_PATH, show_col_types = FALSE)

# Step 4: Clean credits and count appearances
apod <- apod |> mutate(credit_clean = str_squish(copyright))   # stringr: trim + collapse inner whitespace

# Step 5: Count appearances per photographer
credit_counts <- apod |> filter(!is.na(credit_clean)) |> count(credit_clean, sort = TRUE)

# Step 6: Bucket photographers by appearances
credit_counts <- credit_counts |> mutate(
  bucket = cut(n, breaks = c(0, 1, 5, 20, Inf), labels = c("1", "2-5", "6-20", "21+"))
)
credit_counts |> count(bucket)

# Step 7: Share of credited rows by bucket
apod <- apod |> left_join(credit_counts |> select(credit_clean, bucket), by = "credit_clean")
apod |> filter(!is.na(bucket)) |> count(bucket) |> mutate(share = n / sum(n))

# Step 8: Readability per caption
readability <- textstat_readability(apod$explanation, measure = "Flesch.Kincaid")
apod <- apod |> mutate(fk_grade = readability$Flesch.Kincaid)

# Step 9: Compare credited vs uncredited
apod |> group_by(has_copyright) |> summarise(n = n(), mean_fk = mean(fk_grade), median_fk = median(fk_grade))

# Step 10: Is the gap more than noise, and does it hold by era?
t.test(fk_grade ~ has_copyright, data = apod)
apod |> group_by(year, has_copyright) |> summarise(mean_fk = mean(fk_grade), .groups = "drop")

# Step 11: Readability by year, credited vs uncredited
by_year <- apod |> group_by(year, has_copyright) |> summarise(mean_fk = mean(fk_grade), .groups = "drop")
ggplot(by_year, aes(x = year, y = mean_fk, colour = has_copyright)) + geom_line()

# Step 12: Distinctive words, credited vs uncredited
words <- apod |>
  select(has_copyright, explanation) |>
  unnest_tokens(word, explanation) |>
  anti_join(stop_words, by = "word")

word_counts <- words |> count(has_copyright, word)
word_counts |> bind_tf_idf(word, has_copyright, n) |> group_by(has_copyright) |> slice_max(tf_idf, n = 15)

# Step 13: Group credits by type
agency_pattern <- "NASA|ESA|ESO|JAXA|JPL|STScI|Caltech"

apod <- apod |> mutate(
  credit_type = case_when(
    is.na(credit_clean)                              ~ "No credit",
    str_detect(credit_clean,agency_pattern)           ~ "Agency named",
    TRUE                                    ~ "Person or other"
  )
)
apod |> count(credit_type)

# Step 14: Check the agency group, then compare readability by credit type
apod |> filter(credit_type == "Agency named") |> count(credit_clean, sort = TRUE) |> print(n = 20)

apod |> group_by(credit_type) |> summarise(n = n(), mean_fk = mean(fk_grade), median_fk = median(fk_grade))

# Step 15: Tag captions
mission_pattern <- "Hubble|Cassini|Curiosity|rover|spacecraft|orbiter|HiRISE|Juno|Voyager|Apollo"
gear_pattern    <- "telephoto|tripod|lens|exposure|camera|panorama"

# Step 16: Flag captions
apod <- apod |> mutate(
  is_mission = str_detect(explanation, regex(mission_pattern, ignore_case = TRUE)),
  is_gear    = str_detect(explanation, regex(gear_pattern, ignore_case = TRUE))
)

# Step 17: Share of captions tagged, by credit type
apod |> group_by(credit_type) |> summarise(n = n(), pct_mission = mean(is_mission), pct_gear = mean(is_gear))

# Step 18: Export tables
OUTPUT_FOLDER <- path.expand("~/TableauData/Tidy Tuesday")
dir.create(OUTPUT_FOLDER, showWarnings = FALSE, recursive = TRUE)

bucket_shares <- apod |> filter(!is.na(bucket)) |> count(bucket) |> mutate(share = n / sum(n))
write_csv(bucket_shares, file.path(OUTPUT_FOLDER, "bucket_shares.csv"))

# Step 19: Readability by year and credit type
readability_by_year <- apod |> group_by(year, credit_type) |> summarise(mean_fk = mean(fk_grade), n = n(), .groups = "drop")
write_csv(readability_by_year, file.path(OUTPUT_FOLDER, "readability_by_year.csv"))

# Step 20: Top photographers
top_photographers <- credit_counts |> slice_max(n, n = 15)
write_csv(top_photographers, file.path(OUTPUT_FOLDER, "top_photographers.csv"))

# Step 21: Tag shares by credit type
tag_shares <- apod |> group_by(credit_type) |> summarise(n = n(), pct_mission = mean(is_mission), pct_gear = mean(is_gear))
write_csv(tag_shares, file.path(OUTPUT_FOLDER, "tag_shares.csv"))

# Step 22: bucket people
bucket_people <- credit_counts |> count(bucket, name = "photographers") |> mutate(share_photographers = photographers / sum(photographers))
write_csv(bucket_people, file.path(OUTPUT_FOLDER, "bucket_people.csv"))

stopifnot(nrow(apod) == 6888)
stopifnot(sum(apod$credit_type == "No credit") == 2699)
stopifnot(sum(apod$has_copyright) == 4189)
stopifnot(abs(bucket_shares |> filter(bucket == "21+") |> pull(share) - 0.123) < 0.001)
stopifnot(nrow(credit_counts |> filter(bucket == "21+")) == 14)
stopifnot(sum(bucket_people$photographers) == 2045)
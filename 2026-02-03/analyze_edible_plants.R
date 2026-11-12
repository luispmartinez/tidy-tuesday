# analyze_edible_plants.R : Stage 2. Dig into the hints in the clean file's EDA Summary, choose the angle.
# Output: small summary tables in WORK_FOLDER, one per finding, for build_edible_plants.R to read.
# Blanks are marked ___ . Q: lines are for you to answer before you fill them in.
# A ___ is not valid R, so "Source" stops at the first blank on purpose. Work down one step at a time.

# Step 1: Load packages
library(tidyverse)
library(broom)

# Step 2: Settings
WORK_FOLDER    <- path.expand("~/TableauData/Edible Plants/work")   # clean file and analysis tables
TABLEAU_FOLDER <- path.expand("~/TableauData/Edible Plants")        # canvas only (build writes here)
WORK_PATH <- file.path(WORK_FOLDER, "edible_plants_clean.csv")
# intermediates go in WORK_FOLDER; only the canvas goes in TABLEAU_FOLDER
write_table <- function(df, name) {
  out <- file.path(WORK_FOLDER, paste0(name, ".csv"))
  tmp <- paste0(out, ".tmp")
  write_csv(df, tmp, na = "")
  ok <- file.rename(tmp, out)
  stopifnot("rename failed, file not written" = ok)
  invisible(out)
}
# Q: this is your Brazil helper with one change, na = "". Why does that matter for a file Tableau reads?

# Step 3: Read the clean file
d <- read_csv(WORK_PATH, show_col_types = FALSE)
glimpse(d)
# Q: harvest_continual and the _min/_max columns come back as which types? Is that what Step 8
#    of clean_edible_plants.R intended? (Check before you analyze anything.)

# Step 4: Is nutrient need tied to the 30 fruit and nut plants?
# The Summary's top text finding was nutrients x harvest_status (V = 0.41).
# Q: before you run it, which nutrient level do you expect fruit and nut plants to lean toward, and why?
d |> count(harvest_status, nutrients) |> group_by(harvest_status) |> mutate(share = n / sum(n))
# Q: the Report's standout cell was "medium x not applicable". Do the shares show it?
#    Is it all 30 plants, or a few? What does a share mean when a group has 30 rows?

# Step 5: Does nutrient need line up with growing temperature?
# The Summary: nutrients -> temperature_growing_max, R-squared 0.17, smallest group 15, p = 0.0089.
# Q: why the median and not the mean here? What do the 25th and 75th percentiles add?
d |> filter(!is.na(temperature_growing_max)) |> group_by(nutrients) |>
  summarise(n = n(), p25 = quantile(temperature_growing_max, .25), median = median(temperature_growing_max), p75 = quantile(temperature_growing_max, .75))
# Q: n counts plants with a temperature value, not plants. Which group is smallest? Is it enough to trust?

# Step 6: Is the temperature gap about nutrients, or about plant family?
# 0 of the 55 plants with a growing temperature are fruit/nut plants (checked with count(harvest_status)),
# so that block cannot explain the gap.
# Q: before you run it, which families do you expect among the heavy feeders, and why?
d |> filter(!is.na(temperature_growing_max)) |> count(nutrients, cultivation)
# Q: which family or two make up most of the `high` group? What would that mean for the hint
#    "nutrients explain temperature"?

# Step 7: skipped. Step 6 showed the temperature gap follows plant family (2 to 15 plants per family),
#         so a test against nutrients would be confounded.


# Step 8: skipped. Temperature is not part of the nutrients angle; revisit only if the angle changes.


# Step 9: What else do the 30 "not applicable" plants share?
# Q: pick two columns to check before you run this. Why those two?
d |> filter(harvest_status == "not applicable") |> count(cultivation, sort = TRUE)
d |> filter(harvest_status == "not applicable") |> count(common_name, sort = TRUE)
# Q: if all 30 look alike on a column, is that a finding or a definition of the group?

# Step 9b: plant_type, assigned by me (not in the source).
# herb = grown for leaves, flowers or seeds, used for flavor, tea or medicine
# fruit_nut = the source's fruit and nut block plus Strawberry; everything else is vegetable
fruit_nut  <- c("Almond", "Apple", "Apricot", "Blackberry", "Blackcurrant", 
                "Blueberry", "Cherry", "Cranberry", "Fig", "Gooseberry", "Grape", 
                "Hazel", "Kiwi", "Lemon", "Lime", "Loganberry", "Nectarine", 
                "Orange", "Peach", "Pear", "Persimmon", "Plum", "Pomagranate", 
                "Raspberry", "Redcurrant", "Walnut", "White currant", "Chestnut", 
                "Honey Berry", "Strawberry")     # paste the 30 names, each in quotes
herbs <- c("Basil", "Bay", "Borage", "Chamomile (German chamomile)",
           "Calendula", "Catnip", "Chamomile (Roman chamomile)", "Valerian",
           "Comfrey", "Coriander", "Dill", "Thyme", "Feverfew", "Lemon Grass",
           "Marjoram", "Mint", "Oregano", "Parsley", "Rosemary", "Sage",
           "Tarragon (French)", "Lemon balm", "Hops", "Chives")   # paste the 24 names
borderline <- c("Chives", "Hops", "Fennel", "Horseradish", "Lemon Grass")

check_names <- function(x, label) {
  not_in_data <- setdiff(x, d$common_name)
  listed_twice <- x[duplicated(x)]
  cat(label, ":", length(x), "names;", length(not_in_data), "not in data;", length(listed_twice), "duplicated\n")
  if (length(not_in_data) > 0) print(not_in_data)
  length(not_in_data) == 0 && length(listed_twice) == 0
}

stopifnot(
  "fruit_nut list has a bad name"  = check_names(fruit_nut, "fruit_nut"),
  "herbs list has a bad name"      = check_names(herbs, "herbs"),
  "fruit_nut must have 30 names"   = length(fruit_nut) == 30,
  "herbs must have 24 names"       = length(herbs) == 24,
  "a plant is on both lists"       = length(intersect(fruit_nut, herbs)) == 0
)

d <- d |> mutate(
  plant_type = case_when(
    common_name %in% fruit_nut ~ "fruit and nut",
    common_name %in% herbs     ~ "herb",
    TRUE                       ~ "other"
  )
)
count(d, plant_type)    # predict 30, 24, 86


# Step 9c: nutrient shares for the two groups the claim covers (fruit and nut, herb)
d |> filter(plant_type != "other") |> count(plant_type, nutrients) |> group_by(plant_type) |> mutate(share = n / sum(n))

# does "8 in 10" survive? share of herbs that are low, with the borderline plants in and out
herb_low <- function(herb_names) {
  d |> filter(common_name %in% herb_names) |> summarise(s = mean(nutrients == "low")) |> pull(s)
}
herb_low(herbs); herb_low(setdiff(herbs, borderline)); herb_low(union(herbs, borderline))

share_of <- function(names, level) {
  d |> filter(common_name %in% names) |> summarise(s = mean(nutrients == level)) |> pull(s)
}
share_of(fruit_nut, "medium")
share_of(c(fruit_nut, "Hops"), "medium")
# floor for herbs: 0.81 (all five borderline plants counted as herbs)
# floor for fruit and nut: 0.81 (Hops counted as fruit and nut)

# Step 10: Choose the angle
# claim: Eight in ten herbs ask the soil for a pinch. Eight in ten fruit and nut plants ask for a scoop.
# resting on: Step 9c (25 of 30 and 21 of 24) and the Step 9d floors (0.81 both).
# throw it out if: either share falls under 0.8. One re-rated plant does it in the harshest variant,
#   two in the base lists. Step 12 checks both floors.
ANGLE <- "Eight in ten herbs ask the soil for a pinch. Eight in ten fruit and nut plants ask for a scoop."

# Step 11: export for build. Analyze decides the order, the words and the labels; build only draws.
dir.create(WORK_FOLDER, showWarnings = FALSE, recursive = TRUE)

level_order <- c("low", "medium", "medium to high", "high")
level_words <- c(low = "a pinch", medium = "a scoop", `medium to high` = "in between", high = "a heap")

sun_labels <- c("full sun" = "full sun", "full sun/partial shade" = "sun or shade",
                "partial shade" = "part shade", "full sun/partial shade/full shade" = "any light")

plants <- d |>
  mutate(
    level_rank = match(nutrients, level_order),                      # ordinal: places tones, never summed
    level_word = unname(level_words[nutrients]),
    ph_label   = paste0(sprintf("%.1f", preferred_ph_lower), " to ", sprintf("%.1f", preferred_ph_upper)),
    sun_label = unname(sun_labels[sunlight]),
  ) |>
  arrange(plant_type, level_rank, str_to_lower(common_name)) |>
  group_by(plant_type, nutrients) |>
  mutate(dot_order = row_number()) |>                                # alphabetical within each type and level
  ungroup() |>
  select(common_name, taxonomic_name, plant_type, nutrients, level_rank, level_word,
         dot_order, water, sunlight, sun_label, ph_label, season, description)
write_table(plants, "plants")

counts <- d |> count(plant_type, nutrients, name = "plants")
write_table(counts, "counts")

# every distinct string the Python glyph step must draw, once (it uppercases the display layers itself)
glyph_text <- bind_rows(
  tibble(layer = "name",        text = plants$common_name),
  tibble(layer = "water",       text = plants$water),
  tibble(layer = "ph",          text = plants$ph_label),
  tibble(layer = "season",      text = plants$season),
  tibble(layer = "level",       text = plants$level_word),
  tibble(layer = "type",        text = plants$plant_type),
  tibble(layer = "description", text = plants$description),
  tibble(layer = "sun",         text = plants$sun_label),
) |> filter(!is.na(text)) |> distinct()
write_table(glyph_text, "glyph_text")
count(glyph_text, layer)   # read this: name 140, type 3, level 4, description 129 (18 plants share 7 descriptions); the rest are yours to note

# Step 12: Checks
dot_ok <- plants |>
  group_by(plant_type, nutrients) |>
  summarise(ok = identical(sort(dot_order), seq_len(n())), .groups = "drop") |>
  pull(ok)

stopifnot(
  # the plants table
  "plants must have 140 rows"              = nrow(plants) == 140,
  "common_name must be unique"             = n_distinct(plants$common_name) == 140,
  "level_rank has a missing value"         = !anyNA(plants$level_rank),
  "level_word has a missing value"         = !anyNA(plants$level_word),
  "unexpected plant_type"                  = all(plants$plant_type %in% c("fruit and nut", "herb", "other")),
  "unexpected nutrients level"             = all(plants$nutrients %in% level_order),
  "dot_order has a gap or a repeat"        = length(dot_ok) > 0 && all(dot_ok),
  # the counts the picture is built on
  "fruit and nut must be 30"               = sum(plants$plant_type == "fruit and nut") == 30,
  "herb must be 24"                        = sum(plants$plant_type == "herb") == 24,
  "other must be 86"                       = sum(plants$plant_type == "other") == 86,
  "counts must sum to 140"                 = sum(counts$plants) == 140,
  # the two cells the claim rests on. identical() fails on a missing cell; == on an empty result would pass
  "fruit and nut x medium must be 25"      = identical(counts$plants[counts$plant_type == "fruit and nut" & counts$nutrients == "medium"], 25L),
  "herb x low must be 21"                  = identical(counts$plants[counts$plant_type == "herb" & counts$nutrients == "low"], 21L),
  # the glyph text the Python step reads
  "glyph_text has a missing string"        = !anyNA(glyph_text$text),
  "glyph_text name layer must be 140"      = sum(glyph_text$layer == "name") == 140,
  "every plant must have a description"    = !anyNA(plants$description),
  "glyph_text description layer must hold each distinct description" = sum(glyph_text$layer == "description") == n_distinct(plants$description),
  "glyph_text level layer must be 4"       = sum(glyph_text$layer == "level") == 4,
  "glyph_text type layer must be 3"        = sum(glyph_text$layer == "type") == 3,
  # direction checks: the claim's floors. If either falls under 0.8, "8 in 10" stops being true
  "herb floor fell below 0.8"              = share_of(union(herbs, borderline), "low") >= 0.8,
  "fruit and nut floor fell below 0.8"     = share_of(c(fruit_nut, "Hops"), "medium") >= 0.8,
  "every sunlight value has a label" = !anyNA(plants$sun_label),
  "there must be four sun labels"    = sum(glyph_text$layer == "sun") == 4,
  "plants must carry sun_label"          = "sun_label" %in% names(plants),
  "every sunlight value has a label"     = !anyNA(plants$sun_label),
  "the sun layer must hold the labels"   = setequal(glyph_text$text[glyph_text$layer == "sun"], unname(sun_labels))
)
# Why a direction check: a count check says the table is the right size. A floor check says the finding
# is still true. A mislabeled plant can keep every count right and still break the claim.
# Then break one check on purpose (change 140 to 139, or a floor to 0.9), confirm the message, put it back.
cat("analyze done:", nrow(plants), "plants,", nrow(glyph_text), "distinct strings for the glyph step\n")

# Step 1: Load packages
library(tidyverse)

# Step 2: Settings
CLEAN_PATH  <- "~/TableauData/Tidy Tuesday/africa_clean.csv"
LOOKUP_PATH <- "~/TableauData/Tidy Tuesday/africa_countries.csv"
PITCH     <- 2      # x units between countries
BAR_W     <- 1.3    # bar width in x units
MAX_HALF  <- 40     # half-height, in y units, of a bar with TOP_COUNT languages
TOP_COUNT <- 100    # the top gridline
K         <- MAX_HALF / sqrt(TOP_COUNT)   # square-root scale
GAP       <- 0.15   # visible gap between stacked segments
FAMILY_ORDER <- c("Niger–Congo", "Nilo-Saharan", "Afroasiatic", "Indo-European", "Ubangian", "Other")
TICKS   <- c(1, 5, 10, 25, 50, 100)
X_MIN   <- 0
LABEL_X <- -1.5
BAND_PAD  <- 3      # band extends this far above and below the tallest bar
LABEL_GAP <- 8      # zone labels sit this far below the bands
TITLE_Y     <- MAX_HALF + BAND_PAD + 16
SUB_Y       <- MAX_HALF + BAND_PAD + 8
LEGEND_Y    <- -(MAX_HALF + BAND_PAD + LABEL_GAP + 8)
FOOT_Y      <- -(MAX_HALF + BAND_PAD + LABEL_GAP + 17)
LEGEND_STEP <- 15
CANVAS_PATH <- "~/TableauData/Tidy Tuesday/africa_canvas.csv"
LAT_SCALE   <- 0.5
LNG_SCALE   <- 1

# Step 3: Read the cleaned data and the country lookup
d      <- read_csv(CLEAN_PATH, show_col_types = FALSE)
lookup <- read_csv(LOOKUP_PATH, show_col_types = FALSE)
X_MAX   <- (nrow(lookup) + 1) * PITCH     # 104

# Step 4: Count languages per country by family group
d <- d |> mutate(
  family_group = case_when(
    family %in% c("Niger–Congo", "Nilo-Saharan", "Afroasiatic", "Indo-European", "Ubangian") ~ family,
    .default = "Other"
  )
)
counts <- d |> count(country, family_group)
# 5a: one row per country: total, half-height, x position
countries <- counts |> group_by(country) |> summarise(total = sum(n)) |>
  left_join(lookup, by = "country") |>
  mutate(half = K * sqrt(total), x = order * PITCH)
# 5b: one row per segment, with start and end as fractions of the bar
segments <- counts |>
  mutate(family_group = factor(family_group, levels = FAMILY_ORDER)) |>
  arrange(country, family_group) |>
  group_by(country) |>
  mutate(end_frac = cumsum(n) / sum(n), start_frac = lag(end_frac, default = 0), is_first = row_number() == 1) |>
  ungroup() |>
  left_join(select(countries, country, half, x), by = "country")
# 5c: one row per rectangle, with its bottom and top edge
# y_in / y_out: where the segment starts and ends above the center line,
# pulled back by half the gap so neighbors don't touch
rects <- segments |> mutate(
  y_in  = half * start_frac + if_else(is_first, 0, GAP / 2),
  y_out = half * end_frac   - if_else(end_frac == 1, 0, GAP / 2)
)
core  <- rects |> filter(is_first)  |> mutate(part = "core", y_low = -y_out, y_high = y_out)
upper <- rects |> filter(!is_first) |> mutate(part = "up",   y_low = y_in, y_high = y_out)
lower <- rects |> filter(!is_first) |> mutate(part = "down", y_low = -y_out, y_high = -y_in)
rect_rows <- bind_rows(core, upper, lower)
# 5d: expand each rectangle into five corner points
# sx: -1 = left edge, 1 = right edge; sy: 0 = bottom edge, 1 = top edge
corner_tbl <- tibble(point_id = 1:5, sx = c(-1, 1, 1, -1, -1), sy = c(0, 0, 1, 1, 0))

bars <- rect_rows |>
  left_join(select(countries, country, label), by = "country") |>
  mutate(segment_id = paste(country, family_group, part, sep = "|")) |>
  cross_join(corner_tbl) |>
  mutate(
    layer    = "bars",
    key      = paste0(segment_id, "_", point_id),
    dot_x    = x + sx * BAR_W / 2,
    dot_y    = if_else(sy == 0, y_low, y_high),
    tooltips = paste0(label, ": ", family_group, ", ", n, " languages")
  ) |>
  select(layer, key, segment_id, point_id, dot_x, dot_y, family_group, tooltips)
# Step 6: Axis gridlines and labels
grid <- crossing(tick = c(0, TICKS), side = c(-1, 1)) |>
  filter(!(tick == 0 & side == -1)) |>                    # one center line, not two
  mutate(line_id = paste0("grid_", tick, "_", side),
         y = side * K * sqrt(tick))                                   # K * sqrt(tick)

grid_lines <- grid |>
  cross_join(tibble(point_id = 1:2, dot_x = c(X_MIN, X_MAX))) |>
  mutate(layer = "gridlines", key = paste0(line_id, "_", point_id), dot_y = y) |>
  select(layer, key, segment_id = line_id, point_id, dot_x, dot_y)

grid_labels <- grid |> filter(tick > 0) |>
  mutate(layer = "axis_labels", key = paste0("label_", line_id), dot_x = LABEL_X, dot_y = y, text = as.character(tick)) |>
  select(layer, key, dot_x, dot_y, text)
# Step 7: Zone bands and labels
zone_span <- lookup |> group_by(zone) |>
  summarise(first = min(order), last = max(order)) |>
  arrange(first) |>
  mutate(zone_no = row_number(),
         x_left  = (first - 0.5) * PITCH,        # (first - 0.5) * PITCH
         x_right = (last + 0.5) * PITCH,        # (last + 0.5) * PITCH
         x_mid   = (x_left + x_right) / 2,
         short   = case_when(zone == "North and Sahara" ~ "North",
                             zone == "Horn and Nile belt" ~ "Horn–Nile",
                             .default = zone))

bands <- zone_span |> filter(zone_no %% 2 == 1) |>
  cross_join(tibble(point_id = 1:5, sx = c(0, 1, 1, 0, 0), sy = c(0, 0, 1, 1, 0))) |>
  mutate(layer = "bands", segment_id = paste0("band_", zone_no),
         key = paste0(segment_id, "_", point_id),
         dot_x = if_else(sx == 0, x_left, x_right),
         dot_y = if_else(sy == 0, -(MAX_HALF + BAND_PAD), MAX_HALF + BAND_PAD)) |>
  select(layer, key, segment_id, point_id, dot_x, dot_y)

zone_labels <- zone_span |>
  mutate(layer = "zone_labels", key = paste0("zone_", zone_no),
         dot_x = x_mid, dot_y = -(MAX_HALF + BAND_PAD + LABEL_GAP), text = short) |>      # -(MAX_HALF + BAND_PAD + LABEL_GAP)
  select(layer, key, dot_x, dot_y, text)
# Step 8: Title, legend and footnote
titles <- tibble(
  layer = "title", key = c("title_main", "title_sub"),
  dot_x = X_MIN, dot_y = c(TITLE_Y, SUB_Y),
  text = c("Niger–Congo languages fill the west and south",
           "A belt of Nilo-Saharan and Afroasiatic languages stands out from Chad to Ethiopia"),
  is_emphasis = c(TRUE, FALSE))

footnote <- tibble(layer = "footer", key = "footer_source", dot_x = X_MIN, dot_y = FOOT_Y,
                   text = "Source: Wikipedia “Languages of Africa” via TidyTuesday, 2026-01-13. Languages with no published speaker count are excluded.",
                   is_emphasis = FALSE)

legend_items <- tibble(family_group = FAMILY_ORDER, i = 1:6, x0 = X_MIN + (i - 1) * LEGEND_STEP)

legend_swatches <- legend_items |>
  cross_join(tibble(point_id = 1:5, sx = c(0, 1, 1, 0, 0), sy = c(0, 0, 1, 1, 0))) |>
  mutate(layer = "legend_swatches", segment_id = paste0("legend_", i), key = paste0(segment_id, "_", point_id),
         dot_x = x0 + sx * 1.6, dot_y = LEGEND_Y + sy * 3) |>
  select(layer, key, segment_id, point_id, dot_x, dot_y, family_group)

legend_labels <- legend_items |>
  mutate(layer = "legend_labels", key = paste0("legend_label_", i), dot_x = x0 + 2.2, dot_y = LEGEND_Y + 1.5, text = family_group) |>
  select(layer, key, dot_x, dot_y, text)
# Step 9: Combine and verify
canvas <- bind_rows(bars, grid_lines, grid_labels, bands, zone_labels, titles, footnote, legend_swatches, legend_labels) |> # all nine layer tables
  mutate(is_emphasis = coalesce(is_emphasis, FALSE))

stopifnot(
  "canvas row count is not 939"        = nrow(canvas) == 939,
  "duplicate keys in the canvas"       = anyDuplicated(canvas$key) == 0,
  "missing coordinates in the canvas"  = !anyNA(canvas$dot_x) && !anyNA(canvas$dot_y),
  "bar row count is not 835"           = sum(canvas$layer == "bars") == 835
)

canvas <- canvas |> mutate(dot_y = dot_y * LAT_SCALE, dot_x = dot_x * LNG_SCALE)
# Step 10: Write the canvas CSV
write_csv(canvas, CANVAS_PATH)
check <- read_csv(CANVAS_PATH, show_col_types = FALSE)
stopifnot("written canvas does not have 939 rows" = nrow(check) == 939)


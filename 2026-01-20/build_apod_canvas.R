# Step 1: Load packages
library(tidyverse)

# Step 2: Settings
DATA_FOLDER <- path.expand("~/TableauData/Tidy Tuesday")
CANVAS_W <- 1300
CANVAS_H <- 805
PALETTE <- c(ground = "#0B3D91", mission = "#7FD6F0", people = "#FFC24D")  # extend with the rest

text_w <- function(s) {
  narrow <- str_count(s, "[iltfjr.,' I]")
  wide   <- str_count(s, "[mwMW]")
  normal <- nchar(s) - narrow - wide
  0.5 * narrow + 1.6 * wide + normal
}

CHAR_PX_BY_LAYER <- c(title = 9.5, subtitle = 6.8, legend_people = 6.6, legend_rows = 6.6,
                      value_label = 9.5, name_label = 7.3, note = 7.0,
                      eyebrow = 9.1, headline = 32, subhead = 9.6, footnote = 6.3)
LEFT_ANCHORED    <- names(CHAR_PX_BY_LAYER)

# Step 3: Read the four exports
bucket_shares       <- read_csv(file.path(DATA_FOLDER, "bucket_shares.csv"), show_col_types = FALSE)
readability_by_year <- read_csv(file.path(DATA_FOLDER, "readability_by_year.csv"), show_col_types = FALSE)
top_photographers <- read_csv(file.path(DATA_FOLDER, "top_photographers.csv"), show_col_types = FALSE)
tag_shares <- read_csv(file.path(DATA_FOLDER, "tag_shares.csv"), show_col_types = FALSE )
bucket_people <- read_csv(file.path(DATA_FOLDER, "bucket_people.csv"), show_col_types = FALSE)

# Step 4: Layout constants
PAD_X <- 32; PAD_TOP <- 30; HEADER_H <- 124; FOOTER_H <- 50; GAP <- 14
GRID_TOP <- PAD_TOP + HEADER_H + GAP
GRID_H   <- CANVAS_H - GRID_TOP - GAP - FOOTER_H - 22   # bottom padding
PANEL_W  <- (CANVAS_W - 2 * PAD_X - GAP) / 2
PANEL_H  <- (GRID_H - GAP) / 2
TITLE_Y <- 17;  SUB_Y <- 40     # in Step 4, with the other layout constants

panels <- tibble(
  panel = c("long_tail", "top_photographers", "mission_gear", "readability"),
  x0    = c(PAD_X, PAD_X + PANEL_W + GAP,PAD_X, PAD_X + PANEL_W + GAP),
  y0    = c(GRID_TOP, GRID_TOP, GRID_TOP + PANEL_H + GAP, GRID_TOP + PANEL_H + GAP)
)

# Step 5: Panel frames (one closed 5-point line per panel)
corners <- tibble(pt = 1:5, dx = c(0, 1, 1, 0, 0), dy = c(0, 0, 1, 1, 0))

frames <- panels |> cross_join(corners) |> mutate(
  x    = x0 + dx * PANEL_W,
  y_px = y0 + dy * PANEL_H,
  y    = CANVAS_H - y_px,
  layer = "frame"
)

stopifnot(nrow(frames) == 20)
stopifnot(all(frames$x >= 0 & frames$x <= CANVAS_W), all(frames$y >= 0 & frames$y <= CANVAS_H))

# Step 6: Panel 1 tiers
TIER_ORDER <- c("1", "2-5", "6-20", "21+")

tiers <- bucket_people |>
  left_join(bucket_shares, by = "bucket") |>
  mutate(bucket = factor(bucket, levels = TIER_ORDER)) |>
  arrange(bucket) |>
  mutate(tier_i = row_number(),
         pct_people = share_photographers * 100,
         pct_rows   = share * 100)
stopifnot(nrow(tiers) == 4, !anyNA(tiers$bucket))

# Step 7: Panel 1 bars
p1         <- panels |> filter(panel == "long_tail")
BAR_X0     <- p1$x0 + 74
BAR_MAX_PX <- 360
PX_PER_PCT <- BAR_MAX_PX / max(tiers$pct_people, tiers$pct_rows)   # one shared scale
ROW0_Y     <- p1$y0 + 88
TIER_PITCH <- 41
BAR_H <- 12

bar_base <- tiers |>
  select(bucket, tier_i, pct_people, pct_rows, photographers, n) |>
  pivot_longer(c(pct_people, pct_rows), names_to = "measure", values_to = "pct") |>
  mutate(
    id    = paste(bucket, measure),
    y_mid = ROW0_Y + (tier_i - 1) * TIER_PITCH + if_else(measure == "pct_people", 7, 25),
    len   = pct * PX_PER_PCT,
    x_l   = BAR_X0,
    x_r   = BAR_X0 + len,
    tip   = if_else(
      measure == "pct_people",
      paste0(format(photographers, big.mark = ",", trim = TRUE), " photographers appear ", bucket,
             if_else(bucket == "1", " time", " times"), ": ", round(pct, 1), "% of all photographers"),
      paste0("Photographers with ", bucket, if_else(bucket == "1", " appearance", " appearances"),
             " fill ", format(n, big.mark = ",", trim = TRUE), " credited rows: ", round(pct, 1), "% of credited rows"))
  )

corners_bar <- tibble(pt = 1:4, side = c("l", "r", "r", "l"), top = c(FALSE, FALSE, TRUE, TRUE))

bars_p1 <- bar_base |> cross_join(corners_bar) |> mutate(
  x = if_else(side == "l", x_l, x_r),
  y = CANVAS_H - (y_mid + if_else(top, -BAR_H /2, BAR_H / 2)),
  layer = "bar"
)
stopifnot(any(str_detect(bars_p1$tip, "^1,434 photographers appear 1 time")),
          any(str_detect(bars_p1$tip, "^14 photographers appear 21\\+ times")))

# Step 8: Value labels for Panel 1
labels_p1 <- bar_base |> mutate(
  x     = x_r + 10,
  y = CANVAS_H - y_mid,
  label = if_else(pct >= 10, paste0(round(pct), "%"), paste0(round(pct, 1), "%")),
  layer = "value_label"
)

stopifnot(nrow(bar_base) == 8, nrow(bars_p1) == 32)
w <- bars_p1 |> group_by(id) |> summarise(w = diff(range(x)), len = first(len), .groups = "drop")
stopifnot(all(abs(w$w - w$len) < 1e-6))

# Step 9: Tier labels for Panel 1
tier_labels <- tiers |> transmute(
  bucket, tier_i,
  label = as.character(bucket),
  x     = p1$x0 + 40,
  y_px  = ROW0_Y + (tier_i - 1) * TIER_PITCH + 16,
  y     = CANVAS_H - y_px,
  layer = "tier_label"
)

# Step 10: Panel 1 text
TEXT_X  <- p1$x0 + 18

n_people <- format(sum(bucket_people$photographers), big.mark = ",")
n_rows   <- format(sum(bucket_shares$n), big.mark = ",")

legend_people <- paste0("photographers (", n_people, ")")

text_p1 <- tibble(
  layer = c("title", "subtitle", "legend_people", "legend_rows"),
  label = c("Seven in ten photographers appear once",
            "Share of photographers vs share of credited rows, by number of appearances",
            legend_people,
            paste0("credited rows (", n_rows, ")")),
  x = c(TEXT_X, TEXT_X, TEXT_X + 16,
        TEXT_X + 16 + unname(CHAR_PX_BY_LAYER["legend_people"]) * text_w(legend_people) + 32),
  y_px = p1$y0 + c(TITLE_Y, SUB_Y, 60, 60)
) |> mutate(y = CANVAS_H - y_px)
stopifnot(round(tiers$pct_people[1]) == 70)

# Step 11: Legend swatches for Panel 1
swatches_p1 <- text_p1 |>
  filter(str_starts(layer, "legend")) |>
  transmute(
    color_key = if_else(layer == "legend_people", "photographers", "credited_rows"),
    layer     = "legend_swatch",
    x         = x - 11,
    y         = y
  )

# Step 12: Panel 2 constants and base table
p2      <- panels |> filter(panel == "top_photographers")
NAME_X  <- p2$x0 + 18
BAR2_X0 <- p2$x0 + 178
BAR2_MAX <- 235
ROW2_0  <- p2$y0 + 84
PITCH2  <- 26
BAR2_H  <- 6

top6 <- top_photographers |> arrange(desc(n), credit_clean) |> slice_head(n = 6) |> mutate(rank_i = row_number())
PX_PER_APP <- BAR2_MAX / max(top6$n)

bar2_base <- top6 |> mutate(
  id    = credit_clean,
  y_mid = ROW2_0 + (rank_i - 1) * PITCH2,
  len   = n * PX_PER_APP,
  x_l   = BAR2_X0,
  x_r   = BAR2_X0 + len,
  tip = paste0(credit_clean, ": ", n, " appearances, ", round(100 * n / sum(bucket_shares$n), 1), "% of credited rows")
)
stopifnot(nrow(bar2_base) == 6, max(bar2_base$len) <= BAR2_MAX + 1e-9)

# Step 13: Panel 2 bar corners
bars_p2 <- bar2_base |> cross_join(corners_bar) |> mutate(
  x         = if_else(side == "l", x_l, x_r),
  y         = CANVAS_H - (y_mid + if_else(top, -BAR2_H  / 2, BAR2_H  / 2)),
  color_key = "appearances",
  layer     = "bar"
)
stopifnot(nrow(bars_p2) == 24)

# Step 14: Panel 2 labels
names_p2 <- bar2_base |> transmute(
  layer = "name_label",
  id    = paste0("name_", credit_clean),
  pt    = 1L,
  label = credit_clean,
  x     = NAME_X,
  y     = CANVAS_H - y_mid
)

counts_p2 <- bar2_base |> transmute(
  layer = "value_label",
  id    = paste0("count_", credit_clean),
  pt    = 1L,
  label = as.character(n),
  x     = x_r + 10,
  y     = CANVAS_H - y_mid
)

# Step 15: Panel 2 text
n_core     <- tiers |> filter(bucket == "21+") |> pull(photographers)   # photographers in the top tier
share_core <- tiers |> filter(bucket == "21+") |> pull(share)   # their share of credited rows
n_more     <- n_core - nrow(top6)
stopifnot(n_core == 14, round(share_core * 100) == 12, n_more == 8)

text_p2 <- tibble(
  layer = c("title", "subtitle", "ban", "ban_caption", "ban_caption", "note"),
  id    = c("title_p2", "subtitle_p2", "ban_p2", "ban_caption_p2a", "ban_caption_p2b", "note_p2"),
  label = c(paste0("A core of ", n_core, " fills ", round(share_core * 100), "% of credits"),
            "Appearances, most-featured photographers. Run-together credits make these counts a floor.",
            as.character(n_core), "photographers with", "21+ appearances",
            paste0("+ ", n_more, " more with 21 or more appearances")),
  x     = c(p2$x0 + 18, p2$x0 + 18, p2$x0 + 530, p2$x0 + 530, p2$x0 + 530, NAME_X),
  y_px  = c(p2$y0 + TITLE_Y, p2$y0 + SUB_Y, p2$y0 + 128, p2$y0 + 178, p2$y0 + 194, ROW2_0 + 5 * PITCH2 + 24)
) |> mutate(pt = 1L, y = CANVAS_H - y_px)

# Step 16: Panel 4 plot area and scales
p4      <- panels |> filter(panel == "readability")
PLOT_X0 <- p4$x0 + 52;  PLOT_X1 <- p4$x0 + PANEL_W - 24
PLOT_Y0 <- p4$y0 + 74;  PLOT_Y1 <- p4$y0 + PANEL_H - 34    # top and bottom, in pixels
Y_MIN   <- 10.5;        Y_MAX   <- 13
yr_rng  <- range(readability_by_year$year)

x_of <- function(yr) PLOT_X0 + (yr - yr_rng[1]) / diff(yr_rng) * (PLOT_X1 - PLOT_X0)
y_of <- function(v)  PLOT_Y1 - (v - Y_MIN) / (Y_MAX - Y_MIN) * (PLOT_Y1 - PLOT_Y0)

# Step 17: Panel 4 lines
LINE_TYPES <- c("No credit", "Person or other")
ry <- readability_by_year |> filter(credit_type %in% LINE_TYPES)
stopifnot(min(ry$mean_fk) >= Y_MIN, max(ry$mean_fk) <= Y_MAX, min(ry$n) >= 100)

lines_p4 <- ry |> arrange(credit_type, year) |> transmute(
  layer     = "line",
  id        = credit_type,
  pt        = year,
  x         = x_of(year),
  y         = CANVAS_H - y_of(mean_fk),
  color_key = if_else(credit_type == "No credit", "no_credit", "person_credited"),
  tip = paste0(year, ", ", if_else(credit_type == "No credit", "No credit", "Person credited"),
               ": mean grade ", formatC(mean_fk, format = "f", digits = 1), " from ", format(n, big.mark = ",", trim = TRUE), " captions")
)
stopifnot(nrow(lines_p4) == 38)
stopifnot(all(str_detect(lines_p4$tip, "mean grade \\d+\\.\\d ")))

# Panel 4 text (place after `ry` and `p4` exist, above bind_rows)
chk <- ry |> select(year, credit_type, mean_fk) |> pivot_wider(names_from = credit_type, values_from = mean_fk)
stopifnot(all(chk[["Person or other"]] < chk[["No credit"]]))
stopifnot(all(ry |> group_by(credit_type) |> slice_max(mean_fk, n = 1) |> pull(year) %in% c(2008, 2009)))

text_p4 <- tibble(
  layer = c("title", "subtitle"), id = c("title_p4", "subtitle_p4"), pt = 1L,
  label = c("Credited captions read simpler, every year",
            "Mean Flesch-Kincaid grade by year. Both peak in 2008–2009, then drift down."),
  x = p4$x0 + 18, y = CANVAS_H - (p4$y0 + c(TITLE_Y, SUB_Y))
)

# Step 18: Panel 4 gridlines and axis labels
GRID_VALS  <- seq(11, 12.5, by = 0.5)
YEAR_TICKS <- c(2007, 2010, 2015, 2020, 2025)
stopifnot(all(YEAR_TICKS >= yr_rng[1] & YEAR_TICKS <= yr_rng[2]))

grid_p4 <- tibble(v = GRID_VALS) |> cross_join(tibble(pt = 1:2)) |> mutate(
  layer = "grid",
  id    = paste0("grid_", v),
  x     = if_else(pt == 1, PLOT_X0, PLOT_X1),
  y     = CANVAS_H - y_of(v)
)

grid_labels <- tibble(v = GRID_VALS) |> transmute(
  layer = "grid_label", id = paste0("gl_", v), pt = 1L,
  label = formatC(v, format = "f", digits = 1),
  x     = PLOT_X0 - 20,
  y     = CANVAS_H - y_of(v)
)

year_labels <- tibble(yr = YEAR_TICKS) |> transmute(
  layer = "year_label", id = paste0("yl_", yr), pt = 1L,
  label = as.character(yr),
  x     = x_of(yr),
  y     = CANVAS_H - (PLOT_Y1 + 16)
)


# Step 19: Panel 4 end-of-line labels (right-aligned to the line end)
END_CHAR_PX <- 7.0
end_labels <- ry |> filter(year == max(year)) |> transmute(
  layer = "end_label",
  id    = paste0("end_", credit_type),
  pt    = 1L,
  label = if_else(credit_type == "No credit", "No credit", "Person credited"),
  x     = x_of(year) - 4 - END_CHAR_PX * text_w(label) / 2,
  y     = CANVAS_H - (y_of(mean_fk) + if_else(credit_type == "No credit", -26, 26))
)

# Step 20: Mask primitives and the two icon shapes (shape space: 300 wide, 250 tall, y counts down)
in_rect <- function(x, y, x1, y1, x2, y2) x >= x1 & x <= x2 & y >= y1 & y <= y2
in_ell  <- function(x, y, cx, cy, rx, ry) ((x - cx) / rx)^2 + ((y - cy) / ry)^2 <= 1

in_rrect <- function(x, y, x1, y1, x2, y2, r) {
  base   <- in_rect(x, y, x1, y1, x2, y2)
  corner <- (x < x1 + r | x > x2 - r) & (y < y1 + r | y > y2 - r)
  cx     <- if_else(x < x1 + r, x1 + r, x2 - r)
  cy     <- if_else(y < y1 + r, y1 + r, y2 - r)
  base & (!corner | in_ell(x, y, cx, cy, r, r))
}

inside_camera <- function(x, y) {
  body <- in_rrect(x, y, 0, 60, 300, 240, 25) | in_rect(x, y, 90, 25, 190, 60)
  hole <- in_ell(x, y, 150, 150, 65, 65)     | in_rect(x, y, 235, 80, 275, 105)
  (body & !hole) | in_ell(x, y, 150, 150, 28, 28)
}

inside_probe <- function(x, y) {
  body <- in_rrect(x, y, 120, 60, 180, 140, 12)
  solar <- in_rect(x, y, 0, 80, 105, 120 )    | in_rect(x, y, 195, 80, 300, 120)
  links <- in_rect(x, y, 105, 95, 120, 105)  | in_rect(x, y, 180, 95, 195, 105)
  dish <- in_ell(x, y, 150, 40, 22, 22)       # center x, center y, x-radius, y-radius
  stem <- in_rect(x, y, 147, 40, 153, 62)
  body | solar | links | dish | stem
}

g <- expand_grid(x = seq(0, 300, by = 2), y = seq(0, 250, by = 2))
stopifnot(sum(inside_camera(g$x, g$y)) == 11456, sum(inside_probe(g$x, g$y)) == 3878)


# Step 21: Dot grids (one icon = DOTS dots; STEP canvas px between dots)
STEP <- 4;  DOTS <- 300

dots_at <- function(inside, k) {
  expand_grid(i = 0:ceiling(300 * k / STEP), j = 0:ceiling(250 * k / STEP)) |>
    mutate(px = i * STEP, py = j * STEP) |>
    filter(inside(px / k, py / k)) |>
    select(px, py)
}

find_k <- function(inside) {
  ks  <- seq(0.15, 0.9, by = 0.001)
  cnt <- map_int(ks, ~ nrow(dots_at(inside, .x)))
  ks[which(cnt >= DOTS)[1]]
}

icon_dots <- function(inside, k) {
  dots_at(inside, k) |>
    mutate(r = sqrt((px - mean(px))^2 + (py - mean(py))^2)) |>
    slice_min(r, n = DOTS, with_ties = FALSE) |>
    select(px, py)
}

K_CAMERA <- find_k(inside_camera)
K_PROBE  <- find_k(inside_probe)
stopifnot(nrow(icon_dots(inside_camera, K_CAMERA)) == DOTS, nrow(icon_dots(inside_probe, K_PROBE)) == DOTS)

# Step 22: Icons, lit dots and placement
p3          <- panels |> filter(panel == "mission_gear")
ICON_BOTTOM <- p3$y0 + 190

icons <- tag_shares |>
  filter(credit_type %in% LINE_TYPES) |>
  pivot_longer(c(pct_mission, pct_gear), names_to = "tag", values_to = "share") |>
  mutate(shape = if_else(tag == "pct_mission", "probe", "camera"),
         icon  = paste(shape, credit_type),
         n_lit = round(share * DOTS),
         tip   = paste0(n_lit, " of ", DOTS, " dots lit. ", round(share * 100, 1), "% of ",
                        format(n, big.mark = ",", trim = TRUE), " ", credit_type, " captions name ",
                        if_else(shape == "probe", "a mission or spacecraft", "camera gear"))) |>
  arrange(factor(shape, c("probe", "camera")), factor(credit_type, LINE_TYPES)) |>
  mutate(cx = p3$x0 + c(98, 274, 434, 550))

K_BY_SHAPE    <- c(probe = K_PROBE, camera = K_CAMERA)
MASK_BY_SHAPE <- list(probe = inside_probe, camera = inside_camera)

dots_p3 <- icons |>
  mutate(d = map(shape, ~ icon_dots(MASK_BY_SHAPE[[.x]], K_BY_SHAPE[[.x]]))) |>
  unnest(d) |>
  group_by(icon) |>
  mutate(x_px = cx + (px - (max(px) + min(px)) / 2),
         y_px = ICON_BOTTOM - (max(py) - py)) |>
  arrange(icon, desc(py), px, .by_group = TRUE) |>
  mutate(rank = row_number(), lit = rank <= n_lit) |>
  ungroup() |>
  transmute(layer = "icon_dot", id = paste(icon, rank), pt = 1L,
            x = x_px, y = CANVAS_H - y_px,
            color_key = if_else(lit, if_else(shape == "probe", "mission", "gear"), "unlit"),
            tip)
stopifnot(nrow(dots_p3) == 1200, sum(dots_p3$color_key == "mission") == 200, sum(dots_p3$color_key == "gear") == 139)

# Step 23: Panel 3 text
icon_text <- icons |>
  mutate(grp = if_else(shape == "probe", "mission", "gear")) |>
  transmute(
    id = icon, cx, grp, share, n,
    pct  = paste0(round(share * 100), "%"),
    name = credit_type,
    cnt = paste0(format(n, big.mark = ",", trim = TRUE), " captions")
  ) |>
  pivot_longer(c(pct, name, cnt), names_to = "kind", values_to = "label") |>
  mutate(layer = c(pct = "pct_label", name = "icon_name", cnt = "icon_count")[kind],
         pt = 1L, x = cx,
         y_px = ICON_BOTTOM + c(pct = 28, name = 50, cnt = 64)[kind],
         y = CANVAS_H - y_px,
         id = paste0(layer, "_", id),
         color_key = if_else(kind == "pct", grp, NA_character_)) |>
  select(layer, id, pt, x, y, label, color_key)

head_p3 <- tibble(
  layer = "group_head", pt = 1L,
  id    = c("head_mission", "head_gear"),
  label = c("MISSION OR SPACECRAFT", "CAMERA GEAR"),
  x = c(mean(icons$cx[1:2]), mean(icons$cx[3:4])),
  y     = CANVAS_H - (p3$y0 + 100),
  color_key = c("mission", "gear")
)

text_p3 <- tibble(
  layer = c("title", "subtitle"), id = c("title_p3", "subtitle_p3"), pt = 1L,
  label = c("Six in ten uncredited captions name a mission",
            "Lit dots = share of captions naming a mission (blue) or camera gear (amber)"),
  x = p3$x0 + 18, y = CANVAS_H - (p3$y0 + c(TITLE_Y, SUB_Y))
)
stopifnot(round(icons$share[icons$icon == "probe No credit"] * 10) == 6)

# Step 24: Header and footer text
HEADLINE <- "7 IN 10 APOD PHOTOGRAPHERS APPEAR ONCE"   # states the finding; 45 characters or fewer
SUBHEAD  <- "appearances → most featured → mission vs camera gear → writing level"   # one line: how to read the dashboard
EYEBROW  <- "TIDY TUESDAY · 2026-01-20 · NASA ASTRONOMY PICTURE OF THE DAY"
FOOT_LINES <- c(
  "No view counts exist, so “featured” is not “popular”. Mission and gear tags are keyword matches; “camera” and “exposure” also appear in mission captions, so the gear share is an upper bound.",
  "Flesch-Kincaid measures sentence and syllable load, not difficulty. Agency-credited captions are left out of the line chart (about five a year).",
  "Source: NASA Astronomy Picture of the Day via TidyTuesday, 2026-01-20 · Created by Luis Pablo Martinez"
)
FOOT_TOP <- GRID_TOP + GRID_H + GAP    # top of the footer zone, in pixel rows

rect_poly <- function(id, x1, y1, x2, y2) {   # corners in canvas pixels; y1 is the top edge
  tibble(layer = "accent", id = id, pt = 1:4,
         x = c(x1, x2, x2, x1), y_px = c(y2, y2, y1, y1)) |>
    mutate(y = CANVAS_H - y_px, color_key = "accent") |>
    select(layer, id, pt, x, y, color_key)
}

accents <- bind_rows(
  rect_poly("chip", PAD_X, PAD_TOP - 2, PAD_X + 56, PAD_TOP + 3),
  rect_poly("rule", PAD_X, FOOT_TOP - 2, CANVAS_W - PAD_X, FOOT_TOP)
)

text_hf <- tibble(
  layer = c("eyebrow", "headline", "subhead", rep("footnote", 3)),
  id    = c("eyebrow", "headline", "subhead", paste0("foot_", 1:3)),
  label = c(EYEBROW, HEADLINE, SUBHEAD, FOOT_LINES),
  x     = PAD_X,
  y_px  = c(PAD_TOP + 18, PAD_TOP + 58, PAD_TOP + 104, FOOT_TOP + c(12, 28, 44))
) |> mutate(pt = 1L, y = CANVAS_H - y_px)
stopifnot(round(tiers$pct_people[1] / 10) == 7)
stopifnot(all(unname(CHAR_PX_BY_LAYER[text_hf$layer]) * text_w(text_hf$label) <= CANVAS_W - 2 * PAD_X))

# Step 25: Assemble the canvas
canvas <- bind_rows(
  frames      |> transmute(layer, id = panel, pt, x, y),
  bars_p1     |> transmute(layer, id = paste(bucket, measure), pt, x, y,
                           color_key = if_else(measure == "pct_people", "photographers", "credited_rows"), tip),
  labels_p1   |> transmute(layer, id = paste(bucket, measure), pt = 1L, x, y, label),
  tier_labels |> transmute(layer, id = as.character(bucket), pt = 1L, x, y, label),
  text_p1     |> transmute(layer, id = layer, pt = 1L, x, y, label),
  swatches_p1 |> transmute(layer, id = color_key, pt = 1L, x, y, color_key),
  bars_p2    |> transmute(layer, id, pt, x, y, color_key, tip),
  names_p2   |> select(layer, id, pt, x, y, label),
  counts_p2  |> select(layer, id, pt, x, y, label),
  text_p2    |> select(layer, id, pt, x, y, label),
  lines_p4    |> select(layer, id, pt, x, y, color_key, tip),
  grid_p4     |> select(layer, id, pt, x, y),
  grid_labels,
  year_labels,
  end_labels,
  dots_p3,
  icon_text,
  head_p3,
  text_p3,
  text_hf |> select(layer, id, pt, x, y, label),
  text_p4,
  accents
)
stopifnot(nrow(canvas) == 1401)
stopifnot(all(!is.na(canvas$tip[canvas$layer %in% c("bar", "line", "icon_dot")])),
          sum(!is.na(canvas$tip)) == 1294)

# Step 26: Left-anchor the text layers

canvas <- canvas |> mutate(
  x = if_else(layer %in% LEFT_ANCHORED,
              x + unname(CHAR_PX_BY_LAYER[layer]) * text_w(label) / 2,
              x)
)

stopifnot(all(unname(CHAR_PX_BY_LAYER["title"]) * text_w(canvas$label[canvas$layer == "title"]) <= PANEL_W - 36))
stopifnot(sum(canvas$layer == "bar") == 56, sum(canvas$layer == "value_label") == 14,
          sum(canvas$layer == "name_label") == 6, sum(canvas$layer == "title") == 4,
          sum(canvas$layer == "subtitle") == 4, sum(canvas$layer == "ban_caption") == 2,
          sum(canvas$layer == "note") == 1, sum(canvas$layer == "line") == 38,
          sum(canvas$layer == "grid") == 8, sum(canvas$layer == "grid_label") == 4,
          sum(canvas$layer == "year_label") == 5, sum(canvas$layer == "end_label") == 2,
          sum(canvas$layer == "icon_dot") == 1200, sum(canvas$layer == "pct_label") == 4,
          sum(canvas$layer == "icon_name") == 4, sum(canvas$layer == "icon_count") == 4,
          sum(canvas$layer == "group_head") == 2,
          sum(canvas$layer == "eyebrow") == 1,
          sum(canvas$layer == "headline") == 1,
          sum(canvas$layer == "subhead") == 1,
          sum(canvas$layer == "footnote") == 3,
          sum(canvas$layer == "accent") == 8)

expected <- c(ban = 1, ban_caption = 2, bar = 56, end_label = 2, eyebrow = 1, footnote = 3,
              frame = 20, grid = 8, grid_label = 4, group_head = 2, headline = 1,
              icon_count = 4, icon_dot = 1200, icon_name = 4, legend_people = 1,
              legend_rows = 1, legend_swatch = 2, line = 38, name_label = 6, note = 1,
              pct_label = 4, subhead = 1, subtitle = 4, tier_label = 4, title = 4,
              value_label = 14, year_label = 5, accent = 8)
got <- table(canvas$layer)
stopifnot(setequal(names(got), names(expected)), all(got[names(expected)] == expected))


# Step 27: Convert canvas pixels to degrees for MAKEPOINT
DEG_PX <- 40    # canvas pixels per degree
canvas <- canvas |> mutate(
  x = (x - CANVAS_W / 2) / DEG_PX,
  y = (y - CANVAS_H / 2) / DEG_PX
)
stopifnot(all(abs(canvas$x) <= 180), all(abs(canvas$y) <= 90))


write_csv(canvas, file.path(DATA_FOLDER, "apod_canvas.csv"), na = "")
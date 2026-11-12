# build_edible_plants_canvas.R : Stage 3. Every mark on the poster becomes rows Tableau can draw.
# Reads  (WORK_FOLDER):    plants.csv, counts.csv        from analyze_edible_plants.R
# Writes (TABLEAU_FOLDER): edible_plants_canvas.csv      static scene. Columns: layer, poly_id, point_order, x, y, color, tip
#                          plant_dots.csv                one row per plant: dot position, tone, card fields
#                          glyph_atlas.csv               each character's outline, drawn once
#                          card_layout.csv               one row per character of card text, per plant
# No Python step: the atlas comes from glyph_rings() in glyph_helpers.R (it already bridges the holes).
# Tableau calcs for the card:   x = [gx] + [x_off]      y = [gy] + [y_off]      (both y UP: no minus sign)
#   The earlier test workbook used the Python atlas, which was y DOWN and needed y = -([gy] + [y_off]).
# Blanks are marked ___ and TODO. Q: lines are for you to answer before you fill them in.
# Rule: analyze decides, build draws. Nothing here should recompute a share or a count.

# Step 1: Load packages
library(tidyverse)
library(systemfonts)   # glyph outlines and text shaping

# Step 2: Settings
W <- 1300; H <- 805
DEG_PX <- 40           # canvas pixels per degree, same as Brazil and APOD
WORK_FOLDER    <- path.expand("~/TableauData/Edible Plants/work")
TABLEAU_FOLDER <- path.expand("~/TableauData/Edible Plants")      # only files Tableau opens go here
ROW_CEILING <- 120000                                              # vertex budget tripwire
SEED <- 2026                                                       # the hand-spread look only. It never decides which plant sits where
S      <- 26                                                       # dot spacing in px
GROUND <- 596                                                      # screen y of the ground line
PAL  <- c(wall = "#0F0F10", paper = "#FCFCFB", ink = "#0D0D0D", red = "#C8322F", soil = "#2F2218", shadow = "#000000")
TONE <- c(low = "#7C5CFF", medium = "#FF9D3D", `medium to high` = "#19E3D5", high = "#FFE45E")
MOUND_CX <- c("fruit and nut" = 230, "herb" = 520, "other" = 840)
CARD <- list(x = 100, y = 196, w = 560, h = 274)                   # top-left of the card, in px. Take them from board L
# fonts for glyph_helpers.R: these four globals must exist before check_helper_globals()
font_paths <- c(anton  = path.expand("~/Library/Fonts/Anton-Regular.ttf"),
                body   = path.expand("~/Library/Fonts/BarlowCondensed-SemiBold.ttf"),
                marker = path.expand("~/Library/Fonts/PermanentMarker-Regular.ttf"))
tol_for <- c(anton = 0.2, body = 3, marker = 2)                  # curve tolerance at size 100 for the static lettering
BASE    <- 1.0
source("R/glyph_helpers.R"); check_helper_globals()
# Q: DEG_PX = 40 makes the 1300 px canvas 32.5 degrees wide. A map wraps at 180 degrees of longitude.
#    How far from the wrap is this canvas, and why did the unscaled test (480 px lines) collide with itself?


# Step 3: Read the hand-off
plants <- read_csv(file.path(WORK_FOLDER, "plants.csv"), show_col_types = FALSE)
counts <- read_csv(file.path(WORK_FOLDER, "counts.csv"), show_col_types = FALSE)
# Q: which numbers on the poster come from `counts`, and which from `plants`? Name them before you draw.

# Step 4: What is on the poster, and where does each number come from?
n_type <- table(plants$plant_type)
cell   <- function(tp, lvl) counts$plants[counts$plant_type == tp & counts$nutrients == lvl]
stopifnot(
  "fruit and nut medium must be 25" = identical(as.integer(cell("fruit and nut", "medium")), 25L),
  "herb low must be 21"             = identical(as.integer(cell("herb", "low")), 21L),
  "plants per mound must be 30 24 86" = identical(as.integer(n_type[c("fruit and nut", "herb", "other")]), c(30L, 24L, 86L))
)
# Q: the captions say "25 OF 30", "21 OF 24" and "86 PLANTS". Which do you write from the data,
#    and which would you be tempted to type? Why is typing the risk?

# Step 5: The glyph atlas, in R. Each distinct character is drawn once, at its own size
# one row per font key. tol_px is the outline error you accept ON SCREEN, in pixels, at that size

FONT_KEYS <- tribble(
  ~key,   ~font,   ~size, ~upper, ~tol_px,
  "name", "anton", 46,    TRUE,   0.5,
  "fact", "anton", 20,    TRUE,   0.5,
  "body", "body",  16,    FALSE,  0.5
)
LAYER_KEY <- c(name = "name", level = "fact", type = "fact", water = "fact", sun = "fact", ph = "fact",
               season = "fact", description = "body")
layer_text <- function(layer) switch(layer, name = plants$common_name, level = plants$level_word, type = plants$plant_type,
                                     water = plants$water, sun = plants$sun_label, ph = plants$ph_label,
                                     season = plants$season, description = plants$description)

EXTRA <- list(body = "CLOSE ×")           # in chars_for(): txt <- c(txt, EXTRA[[key]]) when key is "body"

chars_for <- function(key) {
  cfg <- FONT_KEYS[FONT_KEYS$key == key, ]
  txt <- unlist(map(names(LAYER_KEY)[LAYER_KEY == key], layer_text))
  txt <- c(txt, EXTRA[[key]])               # "CLOSE ×" for the body key; NULL for the others
  txt <- txt[!is.na(txt)]
  if (cfg$upper) txt <- str_to_upper(txt)
  sort(unique(unlist(strsplit(txt, ""))))
}

atlas_for <- function(key, tol_px = NULL) {
  cfg <- FONT_KEYS[FONT_KEYS$key == key, ]
  if (is.null(tol_px)) tol_px <- cfg$tol_px
  tol <- tol_px * 100 / cfg$size
  map_dfr(setdiff(chars_for(key), " "), function(ch) {
    rings <- glyph_rings(ch, font_paths[[cfg$font]], tol)
    map_dfr(seq_along(rings), function(k) {
      m <- rings[[k]]
      tibble(glyph_key = paste0(key, ":", utf8ToInt(ch)), ring = k, point_order = seq_len(nrow(m)),
             gx = m[, 1] * cfg$size / 100 / DEG_PX,
             gy = m[, 2] * cfg$size / 100 / DEG_PX)
    })
  })
}




# then the advances block and the "a character has no outline" check, as in the skeleton


                                                                # line: 2.2 lines on average, 3 at most (17 px reaches 4)
# glyph_rings() measures at size 100, so its `tol` is in size-100 units: tol = tol_px * 100 / size



atlas <- map_dfr(FONT_KEYS$key, atlas_for)
# advance width of each character, in px at its own size. One character at a time, so a ligature can never change the count
advances <- map_dfr(FONT_KEYS$key, function(key) {
  cfg <- FONT_KEYS[FONT_KEYS$key == key, ]
  tibble(ch = chars_for(key)) |>
    mutate(glyph_key = paste0(key, ":", map_int(ch, utf8ToInt)),
           advance_px = map_dbl(ch, ~ shape_string(.x, path = font_paths[[cfg$font]], size = 100)$metrics$width) * cfg$size / 100)
})
cat("atlas rows:", nrow(atlas), " distinct glyphs:", n_distinct(atlas$glyph_key), "\n")
# Q: the Python test gave about 1,900 atlas rows for these three keys. If yours is far higher, which setting moves it?
# Q: glyph_rings() asserts that holes bridge cleanly. What would a missing bridge look like in Tableau?
stopifnot("a character has no outline and is not a space" =
            length(setdiff(advances$glyph_key[advances$ch != " "], atlas$glyph_key)) == 0)

# Step 6: Mounds. The dot order is a rule, not a layout accident
rows_for <- function(n) {                                          # widths of the rows, bottom first: a pile that narrows by one
  w0 <- 1; while (w0 * (w0 + 1) / 2 < n) w0 <- w0 + 1
  rows <- w0:1; excess <- sum(rows) - n
  while (excess > 0) {
    last <- length(rows)
    if (rows[last] <= excess) { excess <- excess - rows[last]; rows <- rows[-last] }
    else { rows[last] <- rows[last] - excess; excess <- 0 }
  }
  stopifnot(sum(rows) == n)
  rows
}
lattice <- function(n, cx) {                                       # slots in screen px, bottom row first, left to right
  rows <- rows_for(n)
  map_dfr(seq_along(rows), function(j) {
    tibble(row = j, col = seq_len(rows[j]),
           x = cx + (seq_len(rows[j]) - (rows[j] + 1) / 2) * S,
           y = GROUND - 8 - S / 2 - (j - 1) * S * 0.866)
  })
}
dots <- map_dfr(names(MOUND_CX), function(tp) {
  p <- plants |> filter(plant_type == tp) |> arrange(level_rank, dot_order)   # which column breaks ties inside a level?
  bind_cols(p, lattice(nrow(p), MOUND_CX[[tp]]))
})



# Q: why does sorting plants by level_rank and slots bottom-up put `low` at the bottom of every mound?
# Q: how would you prove each plant got exactly one slot, and each slot exactly one plant?
set.seed(SEED)
dots <- dots |> mutate(x = x + runif(n(), -0.2, 0.2) * S, y = y + runif(n(), -0.16, 0.16) * S)
# Q: the jitter comes AFTER the assignment. What would break if it came before and the seed changed?

# Step 7: Static layers. Pixels first (y flipped with flip_y), degrees at the very end, as in Brazil
rounded_rect <- function(id, layer, x0, y0, x1, y1, r, color, tip = "", steps = 8) {
  corner <- function(cx, cy, t0) { t <- seq(t0, t0 + pi / 2, length.out = steps + 1); tibble(x = cx + r * cos(t), y = cy + r * sin(t)) }
  p <- bind_rows(corner(x0 + r, y0 + r, pi), corner(x1 - r, y0 + r, 1.5 * pi),
                 corner(x1 - r, y1 - r, 0),  corner(x0 + r, y1 - r, 0.5 * pi))
  tibble(layer = layer, poly_id = id, point_order = seq_len(nrow(p)), x = p$x, y = flip_y(p$y), color = color, tip = tip)
}
# Q: one rounded rectangle is 4 * (steps + 1) rows. The card draws a body, a border, a red bar and a shadow.
#    How many rows is the rounded card?
rotate_rows <- function(rows, cx, cy, deg) {                       # y is UP here, so a positive angle is counterclockwise
  t <- deg * pi / 180
  rows |> mutate(x2 = cx + (x - cx) * cos(t) - (y - cy) * sin(t),
                 y2 = cy + (x - cx) * sin(t) + (y - cy) * cos(t)) |> mutate(x = x2, y = y2) |> select(-x2, -y2)
}
poster <- rounded_rect("poster", "poster", 60, 30, 1240, 770, r = 4, color = PAL[["paper"]])
# 7b: mounds. TODO: port mound2() from the sketch. For each mound: the topmost dot edge in 10 px bins,
#     smoothed with a moving average and a little noise, closed along the ground. Then the grain and the crumbs.
#     Q: the specks are the first thing to drop if clicks get slow. How many rows are they?

mound_poly <- function(tp) {
  d  <- dots |> filter(plant_type == tp)                  # screen px, already jittered
  cx <- mean(range(d$x))
  hw <- diff(range(d$x)) / 2 + S / 2 + 12                 # half-width with a margin
  xs <- seq(cx - hw, cx + hw, by = 10)                   # bin width in px. The sketch used 10
  env <- map_dbl(xs, function(xb) {                       # the top edge at each bin
    near <- d[abs(d$x - xb) < S * 0.8, ]
    if (nrow(near) == 0) return(GROUND)
    min(near$y - S * 0.5 - 3 + (abs(near$x - xb) / (S * 0.8))^2 * 10)
  })
  # smooth with a 1-2-1 average, add a little noise, and pin both ends to the ground
  env <- as.numeric(stats::filter(env, c(1, 2, 1) / 4, sides = 2))
  env[is.na(env)] <- GROUND
  env <- env + runif(length(env), -3, 3)              # how much wobble, in px?
  env[c(1, length(env))] <- GROUND
  tibble(layer = "mound", poly_id = paste0("mound_", tp),
         point_order = seq_len(length(xs) + 2),
         x = c(xs, max(xs), min(xs)),
         y = flip_y(c(env, GROUND, GROUND)),
         color = PAL[["soil"]], tip = "")
}
set.seed(SEED)
mounds <- map_dfr(names(MOUND_CX), mound_poly)


# 7c: display lettering with draw_labels(). The label table is: id, string, x, top, size, font, color, align
# TODO: "PLANT TYPE" runs down the right edge: draw it horizontally, then rotate_rows(..., deg = -90) about its center.
#       "appetite" and "for soil" use font "marker" and tilt about 7 degrees counterclockwise.
#       Q: which of these could be an image object instead, at zero rows?

speck_poly <- function(tp, per_px2 = 0.0045) {                      # specks per square px; the sketch used 0.0045
  m   <- mounds |> filter(poly_id == paste0("mound_", tp))
  n   <- nrow(m) - 2                                              # the last two rows are the ground corners
  ex  <- m$x[seq_len(n)]; ey <- flip_y(m$y[seq_len(n)])          # the top edge, back in screen px
  area <- sum(diff(ex) * (GROUND - ey[-1]))
  k   <- round(area * per_px2)
  x   <- runif(k, min(ex) + 6, max(ex) - 6)
  top <- approx(ex, ey, xout = x)$y
  keep <- GROUND - top > 8
  x <- x[keep]; top <- top[keep]
  y <- runif(length(x), top + 5, GROUND - 3)
  map_dfr(seq_along(x), function(i)
    circle_poly(paste0("speck_", str_replace_all(tp, " ", "_"), "_", i), "grain", x[i], flip_y(y[i]),
                r = runif(1, 1, 2), color = if (runif(1) < 0.55) "#5C4430" else "#1B130C", n = 6))
}
set.seed(SEED)
grain <- map_dfr(names(MOUND_CX), speck_poly)


nrow(grain)

mounds <- bind_rows(mounds, grain)

eight <- floor(10 * min(cell("herb", "low") / n_type[["herb"]],
                        cell("fruit and nut", "medium") / n_type[["fruit and nut"]]))
others <- counts |> filter(plant_type == "other") |> mutate(share = plants / sum(plants))
stopifnot("the payoff says 8 in 10"                 = eight == 8,
          "'about a third each' is no longer true"  = nrow(others) == 3 && all(others$share > 0.25 & others$share < 0.40))
n_between <- sum(plants$nutrients == "medium to high")

L <- tribble(
  ~id,       ~string,                                                     ~x,  ~top, ~size, ~font,   ~color,           ~align,
  "eyebrow", "TIDY TUESDAY · 2026-02-03 · EDIBLE PLANTS",                100,  60,   16,   "body",  "#444444",        "left",
  "howto_1", "One dot is one plant. A mound's size is how many plants of that type.", 100, 92, 18, "body", PAL[["ink"]], "left",
  "howto_2", "Bottom to top, the layers climb from a pinch to a heap. Click a dot for the plant.", 100, 114, 18, "body", PAL[["ink"]], "left",
  "n_fn",    paste(cell("fruit and nut", "medium"), "OF", n_type[["fruit and nut"]]), 230, 610, 54, "anton", PAL[["ink"]], "centre",
  "n_herb",  paste(cell("herb", "low"), "OF", n_type[["herb"]]),                      520, 610, 54, "anton", PAL[["ink"]], "centre",
  "n_other", paste(n_type[["other"]], "PLANTS"),                                      840, 610, 54, "anton", PAL[["ink"]], "centre"
  # TODO: the three captions under the numbers, the key labels, the payoff, the disclaimer
)
L2 <- tribble(
  ~id,         ~string,                                                  ~x,  ~top, ~size, ~font,  ~color,        ~align,
  "cap_fn",    "fruit and nut plants ask for a scoop",                  230, 668,  19, "body", PAL[["ink"]],  "centre",
  "cap_herb",  "herbs ask for a pinch",                                 520, 668,  19, "body", PAL[["ink"]],  "centre",
  "cap_other", "every level, about a third each",                       840, 668,  19, "body", PAL[["ink"]],  "centre",
  "key_1",     "A PINCH · LOW NEED",                                    132, 166,  17, "body", PAL[["ink"]],  "left",
  "key_2",     "A SCOOP · MEDIUM",                                      367, 166,  17, "body", PAL[["ink"]],  "left",
  "key_3",     "A HEAP · HIGH",                                         602, 166,  17, "body", PAL[["ink"]],  "left",
  "key_4",     paste0("IN BETWEEN: ", n_between, " ", if (n_between == 1) "PLANT" else "PLANTS"),
  837, 166,  17, "body", PAL[["ink"]],  "left",
  "payoff",    str_to_upper(paste0(eight, " in 10 herbs ask for a pinch. ", eight, " in 10 fruit and nut plants ask for a scoop.")),
  100, 704,  28, "anton", PAL[["ink"]], "left",
  "disclaimer","Pinch, scoop and heap are plain words for the source’s low, medium and high. The source does not say how its ratings were set.",
  100, 740,  14, "body", "#444444",     "left",
  "source",    "Source: TidyTuesday 2026-02-03, edible plants. • Created by Luis Pablo Martinez",
  650, 778,  14, "body", "#BDBDBD",     "centre"
)

wall <- rect_poly("wall", "wall", 0, 0, W, H, PAL[["wall"]])

L <- bind_rows(L, L2)
labels <- draw_labels(L, layer = "lettering")

key_dots <- tibble(layer = "dots", poly_id = paste0("keydot_", 1:4), point_order = 1L,
                   x = 100 + (0:3) * 235 + 11, y = flip_y(164 + 11),
                   color = TONE[c("low", "medium", "high", "medium to high")], tip = "", plant = NA_character_)
tilt <- function(rows, deg) rotate_rows(rows, mean(range(rows$x)), mean(range(rows$y)), deg)
title_v  <- text_poly("plant_type", "lettering", "PLANT TYPE", 1165, 455, 150, font_paths[["anton"]], PAL[["ink"]],
                      align = "centre", tol = tol_for[["anton"]]) |> rotate_rows(1165, flip_y(400), -90)
appetite <- text_poly("appetite", "lettering", "appetite", 690, 150, 96, font_paths[["marker"]], PAL[["red"]],
                      tol = tol_for[["marker"]]) |> tilt(7)
for_soil <- text_poly("for_soil", "lettering", "for soil", 930, 228, 40, font_paths[["marker"]], PAL[["red"]],
                      tol = tol_for[["marker"]]) |> tilt(7)

lettering <- bind_rows(labels, title_v, appetite, for_soil)     # key_dots leave this

scene <- bind_rows(wall, poster, mounds, lettering)
pv <- ggplot() + scale_fill_identity() + coord_equal()
for (ly in c("wall", "poster", "mound", "grain", "lettering")) {
  pv <- pv + geom_polygon(data = filter(scene, layer == ly), aes(x, y, group = poly_id, fill = color))
}
pv + geom_point(data = dots, aes(x, flip_y(y)), color = "white", size = 1.5)

nrow(lettering)
nrow(draw_labels(L2 |> filter(id == "disclaimer")))
# Step 8: Dots (marks, one row per plant) and the card


# card text. Layout in px at the font's own size, then to degrees. Positions are baseline-left per character
layout_text <- function(text, key, max_px, line_px) {              # one row per non-space character
  if (is.na(max_px)) max_px <- Inf          # one-line layers never wrap
  adv <- advances |> filter(startsWith(glyph_key, paste0(key, ":")))
  width_of <- function(s) sum(adv$advance_px[match(paste0(key, ":", map_int(strsplit(s, "")[[1]], utf8ToInt)), adv$glyph_key)])
  gap <- width_of(" ")
  x <- 0; line <- 0; idx <- 0; out <- list()
  for (word in strsplit(text, " ")[[1]]) {
    w <- width_of(word)
    if (x > 0 && x + gap + w > max_px) { line <- line + 1; x <- 0 }   # which setting says when to wrap?
    for (ch in strsplit(word, "")[[1]]) {
      out[[length(out) + 1]] <- tibble(idx = idx, char = ch, glyph_key = paste0(key, ":", utf8ToInt(ch)),
                                       x_px = x, y_px = line * line_px)    # what spaces the lines?
      x <- x + width_of(ch); idx <- idx + 1
    }
    x <- x + gap; idx <- idx + 1
  }
  bind_rows(out)
}
# one origin per layer, in px from the card's top-left. Read them off board L
# The description is Barlow at 16 px: max_px about 500, line_px about 1.3 to 1.4 times the size (22), and room for 3 lines.
# Q: the sketch card was drawn for 3 lines at 19 px. How tall must the card be now, and where does that number live?
CARD_LAYERS <- tribble(
  ~layer,        ~dx, ~dy, ~max_px,
  "name",        24,  58, NA,
  "level",       24,  92, NA,
  "type",        330,  92, NA,
  "water",       24,  146, NA,
  "sun",         155,  146, NA,
  "ph",          286,  146, NA,
  "season",      417,  146, NA,
  "description", 24,  200, 512
)
pieces <- list(
  card_shadow = rounded_rect("s", "x", CARD$x + 6, CARD$y + 6, CARD$x + CARD$w + 6, CARD$y + CARD$h + 6, r = 12, color = ""),
  card_body   = rounded_rect("b", "x", CARD$x, CARD$y, CARD$x + CARD$w, CARD$y + CARD$h, r = 12, color = ""),
  card_bar    = rounded_rect("r", "x", CARD$x, CARD$y + CARD$h - 8, CARD$x + CARD$w, CARD$y + CARD$h, r = 4, color = ""),
  closehit    = rounded_rect("h", "x", CARD$x + CARD$w - 128, CARD$y + 8, CARD$x + CARD$w - 8, CARD$y + 52, r = 4, color = ""))


card_layout <- map_dfr(seq_len(nrow(plants)), function(i) {
  map_dfr(seq_len(nrow(CARD_LAYERS)), function(j) {
    lay <- CARD_LAYERS[j, ]; key <- LAYER_KEY[[lay$layer]]; cfg <- FONT_KEYS[FONT_KEYS$key == key, ]
    txt <- layer_text(lay$layer)[i]
    if (is.na(txt)) return(NULL)
    if (cfg$upper) txt <- str_to_upper(txt)
    layout_text(txt, key, lay$max_px, line_px = 22) |>
      mutate(plant = plants$common_name[i], layer = lay$layer, when = "plant", color = "#F4F4F4",
             x_off = (CARD$x + lay$dx + x_px - W / 2) / DEG_PX,
             y_off = (flip_y(CARD$y + lay$dy + y_px) - H / 2) / DEG_PX)
  })
}) |> select(plant, layer, idx, char, glyph_key, x_off, y_off, when, color)
# Q: check y_off by hand. A baseline 100 px below the top of the canvas: what should lat be?
# fixed decimals with sprintf, so Tableau can never type an offset as integer. (format() pads with spaces: avoid it)

frame_atlas <- imap_dfr(pieces, function(p, nm)
  tibble(glyph_key = paste0("frame:", nm), ring = 1L, point_order = seq_len(nrow(p)),
         gx = (p$x - CARD$x) / DEG_PX, gy = (p$y - flip_y(CARD$y)) / DEG_PX))
atlas <- bind_rows(atlas, frame_atlas)         # before the sprintf formatting of gx and gy

# in the card_layout block: add  when = "plant", color = "#F4F4F4"  to the mutate()
card_origin_x <- (CARD$x - W / 2) / DEG_PX
card_origin_y <- (flip_y(CARD$y) - H / 2) / DEG_PX
chrome <- tibble(plant = NA_character_, layer = names(pieces), idx = 0L, char = "", glyph_key = paste0("frame:", names(pieces)),
                 x_off = card_origin_x, y_off = card_origin_y, when = "open",
                 color = c(PAL[["shadow"]], PAL[["ink"]], PAL[["red"]], PAL[["ink"]])) |>
  mutate(plant = if_else(layer == "closehit", "none", NA_character_))

adv_b   <- advances |> filter(startsWith(glyph_key, "body:"))
close_w <- sum(adv_b$advance_px[match(paste0("body:", map_int(strsplit("CLOSE ×", "")[[1]], utf8ToInt)), adv_b$glyph_key)])
close_lay <- layout_text("CLOSE ×", "body", Inf, 22) |>
  mutate(plant = "none", layer = "closebtn", when = "open", color = "#F4F4F4",
         x_off = (CARD$x + CARD$w - 24 - close_w + x_px - W / 2) / DEG_PX,
         y_off = (flip_y(CARD$y + 58) - H / 2) / DEG_PX) |>
  select(any_of(names(chrome)))

card_layout <- bind_rows(card_layout, chrome, close_lay)     # then the sprintf formatting of x_off and y_off
card_layout <- card_layout |> mutate(across(c(x_off, y_off), ~ sprintf("%.4f", .x)))
atlas       <- atlas       |> mutate(across(c(gx, gy),       ~ sprintf("%.5f", .x)))

preview_card <- function(who) {
  lay <- card_layout |> filter(plant == who) |> mutate(across(c(x_off, y_off), as.numeric))
  at  <- atlas |> mutate(across(c(gx, gy), as.numeric))
  lay |> inner_join(at, by = "glyph_key", relationship = "many-to-many") |>
    mutate(x = gx + x_off, y = gy + y_off, pid = paste(layer, idx, ring))
}


# Q: `sun` can read "FULL SUN/PARTIAL SHADE/FULL SHADE", far wider than a quarter of the card at 20 px.
#    A smaller size, a row to itself, or a shorter label. Which, and where does that decision live?

# dots: one row each, in the canvas, with the plant name as the click field
dot_rows <- dots |>
  transmute(layer = "dots", poly_id = paste0("dot_", common_name), point_order = 1L,
            x, y = flip_y(y), color = TONE[nutrients], tip = paste0(common_name, " · ", level_word, " · ", plant_type),
            plant = common_name)

# Step 9: Assemble, then convert pixels to degrees once
canvas    <- bind_rows(wall, poster, mounds, lettering, dot_rows, key_dots)
# Q: poly_id must be unique across layers. How do you check that, and what breaks in Tableau if two layers share one?
canvas     <- canvas     |> mutate(x = (x - W / 2) / DEG_PX, y = (y - H / 2) / DEG_PX)


# Step 10: Checks
dup_layer  <- canvas |> group_by(poly_id) |> summarise(n_layers = n_distinct(layer)) |> filter(n_layers > 1)
dup_within <- canvas |> count(poly_id, point_order) |> filter(n > 1)
stopifnot(
  "over the row ceiling"                  = nrow(canvas) +  nrow(card_layout) + nrow(atlas) <= ROW_CEILING,
  "missing coordinates"                   = !anyNA(canvas$x) && !anyNA(canvas$y),
  "canvas outside valid lat/lon"          = all(abs(canvas$x) <= 180) && all(abs(canvas$y) <= 90),
  "a poly_id appears in two layers"       = nrow(dup_layer) == 0,
  "two shapes share one poly_id"          = nrow(dup_within) == 0,
  "two plants share a slot"               = nrow(distinct(dots, plant_type, row, col)) == 140,
  "a plant has no name text"              = setequal(unique(card_layout$plant[card_layout$layer == "name"]), plants$common_name),
  "a plant has no description text"       = setequal(unique(card_layout$plant[card_layout$layer == "description"]), plants$common_name),
  "an offset has no decimal point"        = all(grepl(".", card_layout$x_off, fixed = TRUE)) && all(grepl(".", card_layout$y_off, fixed = TRUE)),
  "layout uses a glyph the atlas lacks"   = all(card_layout$glyph_key %in% atlas$glyph_key),
  "atlas built at the wrong scale" = max(abs(as.numeric(atlas$gx[!startsWith(atlas$glyph_key, "frame:")]))) < 2 * 46 / DEG_PX
)
# Q: add one check about what the poster SAYS. For example: in every mound, the lowest dot is a `low` plant.
# Then break one check on purpose, confirm the message, and put it back.


is_key  <- startsWith(canvas$poly_id, "keydot_")
holders <- canvas$layer == "dots" & !is_key

stopifnot(
  "chrome rows must have no plant, except the close hit area" = sum(is.na(card_layout$plant)) == 3,
  "the close button must carry the sentinel" = sum(card_layout$plant == "none", na.rm = TRUE) == 7,
  "every glyph is in the atlas"                               = all(card_layout$glyph_key %in% atlas$glyph_key)
)

# Step 11: Write (temp file then rename, to guard against iCloud sync)
write_atomic <- function(df, name) {
  out <- file.path(TABLEAU_FOLDER, name); tmp <- paste0(out, ".tmp")
  write_csv(df, tmp, na = "")
  stopifnot("rename failed, file not written" = file.rename(tmp, out))
  invisible(out)
}
write_atomic(canvas,      "edible_plants_canvas.csv")
write_atomic(atlas,       "glyph_atlas.csv")
write_atomic(card_layout, "card_layout.csv")
cat("build done:", nrow(canvas), "canvas rows,", "dots,", nrow(card_layout), "layout rows,", nrow(atlas), "atlas rows\n")

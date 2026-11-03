# build_brazil_canvas.R : Stage 3. Every mark on the v3 sketch becomes polygon rows.
# Output: one long CSV. Columns: layer, poly_id, point_order, x, y, color, tip
# Blanks are marked ___ and TODO. Q: lines are for you to answer before you fill them in.

# Step 1: Load packages
library(tidyverse)
library(systemfonts)   # glyph outlines for text-as-polygons

# Step 2: Settings
W <- 1300; H <- 805
DEG_PX <- 40    # canvas pixels per degree (same as APOD)
IN_FOLDER   <- path.expand("~/TableauData/Tidy Tuesday")
OUT_FILE    <- file.path(IN_FOLDER, "brazil_canvas.csv")
ROW_CEILING <- 120000                       # vertex budget tripwire
PAL <- c(ink = "#0D0B12", bone = "#F4EFE6", dim = "#B9B3C4", pink = "#FF2E93", cyan = "#19E3FF")
FONT_DISPLAY <- path.expand("~/Library/Fonts/Anton-Regular.ttf")                         # path to Anton
FONT_BODY <- path.expand("~/Library/Fonts/BarlowCondensed-SemiBold.ttf")
FONT_BOLD <- path.expand("~/Library/Fonts/BarlowCondensed-Bold.ttf")
font_paths <- c(anton = FONT_DISPLAY, bold = FONT_BOLD, body = FONT_BODY)
tol_for    <- c(anton = 0.2, bold = 1, body = 1)
BASE       <- 1.0
source("R/glyph_helpers.R"); check_helper_globals()

# Step 3: Read the Stage 2 hand-off
size_summary <- read_csv(file.path(IN_FOLDER, "size_summary.csv"), show_col_types = FALSE)
flag_share <- read_csv(file.path(IN_FOLDER, "flag_share.csv"), show_col_types = FALSE)
size_flag <- read_csv(file.path(IN_FOLDER, "size_flag.csv"), show_col_types = FALSE)
headline <- read_csv(file.path(IN_FOLDER, "headline.csv"), show_col_types = FALSE)
val <- function(tbl, size, col) tbl[[col]][tbl$company_size == size]
round_share <- read_csv(file.path(IN_FOLDER, "round_share.csv"), show_col_types = FALSE)
shift_summary <- read_csv(file.path(IN_FOLDER, "shift_summary.csv"), show_col_types = FALSE)
# Q: size_summary has n, p25, median, p75. Which numbers on the v3 sketch are NOT in it yet?
# Q: which script should compute them, this one or Analyze_brazil.R? Why?


# Step 6: Layer "data" (bars, ticks, dots; the only layer with tooltips)
other <- size_summary |> filter(company_size == "other")
STRIP <- list(axis_x = 40, px_per_decade = 260, v0 = 100000)
ROW_TOP <- c("micro-enterprise" = 390, "small-enterprise" = 510, "other" = 630)  # screen y of each bar's top edge
BAR_H   <- 24
other <- size_summary |> filter(company_size == "other")
money <- function(v) paste0("R$", format(round(v), big.mark = ",", scientific = FALSE, trim = TRUE))
money_m    <- function(v) paste0("R$", round(v / 1e6), "M")
MUTED <- colorRampPalette(c(PAL[["ink"]], PAL[["bone"]]))(11)[5]   # bone at 40% over ink
TICK_W <- 4     # px wide
TICK_H <- 40    # px tall; the bar is 24 tall, so the tick sticks out above and below


SIZE_LABEL <- c("micro-enterprise" = "Micro", "small-enterprise" = "Small", "other" = "Other")
BAR_COLOR <- c("micro-enterprise" = MUTED, "small-enterprise" = MUTED, "other" = PAL[["pink"]])

strip_bar <- function(r) {
  key <- r$company_size
  rect_poly(id = paste0("bar_", key), layer = "data",
            x0 = x_log(r$p25, STRIP$axis_x, STRIP$px_per_decade, STRIP$v0),
            x1 = x_log(r$p75, STRIP$axis_x, STRIP$px_per_decade, STRIP$v0),
            y0 = flip_y(ROW_TOP[[key]] + BAR_H),
            y1 = flip_y(ROW_TOP[[key]]),
            color = BAR_COLOR[[key]],
            tip = paste(paste0(toupper(SIZE_LABEL[[key]]), " · ", format(r$n, big.mark = ","), " FIRMS"),
                        paste0(money(r$p25), " to ", money(r$p75)),
                        paste0("The middle half of firms · median ", money(r$median)), sep = "||")
)}

bars_layer <- bind_rows(lapply(split(size_summary, size_summary$company_size), strip_bar))
tick_for <- function(r) {
  key <- r$company_size
  cx  <- x_log(r$median, STRIP$axis_x, STRIP$px_per_decade, STRIP$v0)   # the median's x position
  pad <- (TICK_H - BAR_H) / 2                                            # how far the tick overshoots the bar
  rect_poly(id = paste0("tick_", key), layer = "data",
            x0 = cx - TICK_W / 2, x1 = cx + TICK_W / 2,
            y0 = flip_y(ROW_TOP[[key]] + BAR_H + pad), y1 = flip_y(ROW_TOP[[key]] - pad),
            color = PAL[["bone"]],
            tip = paste(paste0(toupper(SIZE_LABEL[[key]]), " · MEDIAN"), money(r$median),
                        "Half the firms hold less, half hold more", sep = "||")
)}

tick_layer <- bind_rows(lapply(split(size_summary, size_summary$company_size), tick_for))

PROP  <- list(x = 680, w = 580, h = 26, top_firms = 384, top_capital = 436)   # screen y of each bar's top
TRACK <- colorRampPalette(c(PAL[["ink"]], PAL[["bone"]]))(101)[15]            # bone at 14% over ink

tip_firms <- paste("PLACEHOLDER FIRMS",
                   paste0(format(flag_share$n_flagged, big.mark = ","), " of ", format(flag_share$n_firms, big.mark = ",")),
                   paste0(round(flag_share$pct_firms * 100, 2), "% of all companies"), sep = "||")
tip_capital <- paste("DECLARED CAPITAL",
                     paste0("R$", round(flag_share$flagged_capital_t), "T of R$", round(flag_share$total_capital_t), "T"),
                     paste0(round(flag_share$pct_capital * 100), "% sits in the ", flag_share$n_flagged), sep = "||")

prop_bar <- function(id, pct, top, tip, floor_px = 3) {
  fill_w <- max(pct * PROP$w, floor_px)
  rect_poly(id = id, layer = "data",
            x0 = PROP$x, x1 = PROP$x + fill_w,
            y0 = flip_y(top + PROP$h), y1 = flip_y(top),
            color = PAL[["cyan"]], tip = tip)
}

prop_layer <- bind_rows(
  prop_bar("prop_firms",   flag_share$pct_firms,   PROP$top_firms,   tip_firms),
  prop_bar("prop_capital", flag_share$pct_capital, PROP$top_capital, tip_capital))

DUMB <- list(axis_x = 770, axis_end = 1210, decades = 2, v0 = 10000000, r = 8)
DUMB$px_per_decade <- (DUMB$axis_end - DUMB$axis_x) / DUMB$decades
DUMB_Y <- c("micro-enterprise" = 582, "small-enterprise" = 628, "other" = 674)   # screen y of each dot centre
DOT_COLOR <- c("micro-enterprise" = PAL[["bone"]], "small-enterprise" = PAL[["bone"]], "other" = PAL[["pink"]])

dumbbell_for <- function(r) {
  key <- r$company_size
  cy  <- DUMB_Y[[key]]
  xw  <- x_log(r$mean_without, DUMB$axis_x, DUMB$px_per_decade, DUMB$v0)
  xa  <- x_log(r$mean_with,    DUMB$axis_x, DUMB$px_per_decade, DUMB$v0)
  has_ring <- r$n_flagged > 0
  
  tip_wo   <- paste(paste0(toupper(SIZE_LABEL[[key]]), if (has_ring) " · WITHOUT THE 233" else " · MEAN CAPITAL"),
                    money(r$mean_without), "Mean declared capital per firm", sep = "||")
  tip_ring <- paste(paste0(toupper(SIZE_LABEL[[key]]), " · WITH THE 233"),
                    money(r$mean_with), "Mean declared capital per firm", sep = "||")
  tip_gap  <- paste(paste0(toupper(SIZE_LABEL[[key]]), " · THE GAP"),
                    paste0(money_m(r$mean_without), " to ", money_m(r$mean_with)),
                    "Mean capital, without then with the 233", sep = "||")
  
  parts <- list(circle_poly(paste0("dumb_", key, "_dot"), "data", xw, flip_y(cy), DUMB$r,
                            DOT_COLOR[[key]], tip = tip_wo))
  if (has_ring) {
    parts <- c(
      list(rect_poly(paste0("dumb_", key, "_line"), "data",
                     x0 = min(xw, xa), x1 = max(xw, xa),
                     y0 = flip_y(cy - 1.5), y1 = flip_y(cy + 1.5),
                     color = MUTED, tip = tip_gap)),
      parts,
      list(circle_poly(paste0("dumb_", key, "_ring_out"), "data", xa, flip_y(cy), DUMB$r, PAL[["cyan"]],
                       tip = tip_ring),
           circle_poly(paste0("dumb_", key, "_ring_in"), "data", xa, flip_y(cy), DUMB$r - 3, PAL[["ink"]],
                       tip = tip_ring)))
  }
  bind_rows(parts)
}

dumb_layer <- bind_rows(lapply(split(size_flag, size_flag$company_size), dumbbell_for))
data_layer <- bind_rows(bars_layer, tick_layer, prop_layer, dumb_layer)

# Step 7: Layer "back" (under everything)


DASH   <- list(y_start = 380, y_end = 665, dash = 8, gap = 6, w = 2)

GRID_BRIGHT <- colorRampPalette(c(PAL[["ink"]], PAL[["bone"]]))(101)[61]   # bone at 60% over ink

n_dash <- floor((DASH$y_end - DASH$y_start + DASH$gap) / (DASH$dash + DASH$gap))
x_1m   <- x_log(1000000, STRIP$axis_x, STRIP$px_per_decade, STRIP$v0)

dash_layer <- bind_rows(lapply(seq_len(n_dash), function(k) {
  start <- DASH$y_start + (k - 1) * (DASH$dash + DASH$gap)                     # screen y of this dash's top
  rect_poly(id = paste0("dash_", k), layer = "back",
            x0 = x_1m - DASH$w / 2, x1 = x_1m + DASH$w / 2,
            y0 = flip_y(start + DASH$dash), y1 = flip_y(start),
            color = GRID_BRIGHT)
}))

TRACK_STRIP <- colorRampPalette(c(PAL[["ink"]], PAL[["bone"]]))(101)[7]    # bone at 6% over ink
TRACK_PROP  <- colorRampPalette(c(PAL[["ink"]], PAL[["bone"]]))(101)[15]   # bone at 14% over ink
strip_end   <- STRIP$axis_x + 2 * STRIP$px_per_decade                      # right end of the strip axis

strip_tracks <- bind_rows(lapply(names(ROW_TOP), function(key) {
  rect_poly(id = paste0("track_", key), layer = "back",
            x0 = STRIP$axis_x, x1 = strip_end ,
            y0 = flip_y(ROW_TOP[[key]] + BAR_H), y1 = flip_y(ROW_TOP[[key]]),
            color = TRACK_STRIP)
}))

prop_tracks <- bind_rows(
  rect_poly("track_prop_firms", "back", x0 = PROP$x, x1 = PROP$x + PROP$w,
            y0 = flip_y(PROP$top_firms + PROP$h), y1 = flip_y(PROP$top_firms), color = TRACK_PROP),
  rect_poly("track_prop_capital", "back", x0 = PROP$x, x1 = PROP$x + PROP$w,
            y0 = flip_y(PROP$top_capital + PROP$h), y1 = flip_y(PROP$top_capital), color = TRACK_PROP))
STRIPE_H <- 10
stripe <- rect_poly("stripe_top", "back", x0 = 0, x1 = W, y0 = flip_y(STRIPE_H), y1 = flip_y(0), color = PAL[["pink"]])


GRID_FAINT <- colorRampPalette(c(PAL[["ink"]], PAL[["bone"]]))(101)[21]   # bone at 20% over ink

grid_xs <- c(
  x_log(100000,   STRIP$axis_x, STRIP$px_per_decade, STRIP$v0),
  x_log(10000000, STRIP$axis_x, STRIP$px_per_decade, STRIP$v0),
  sapply(c(1e7, 1e8, 1e9), function(v) x_log(v, DUMB$axis_x, DUMB$px_per_decade, DUMB$v0)))

GRID <- list(strip_top = 380, strip_end = 665, dumb_top = 548, dumb_end = 690, w = 1)

grid_rect <- function(x, top, end, id) {
  rect_poly(id = id, layer = "back",
            x0 = x - GRID$w / 2, x1 = x + GRID$w / 2,
            y0 = flip_y(end), y1 = flip_y(top),
            color = GRID_FAINT)
}

strip_grid <- bind_rows(mapply(grid_rect, grid_xs[1:2], GRID$strip_top, GRID$strip_end,
                               paste0("grid_strip_", 1:2), SIMPLIFY = FALSE))
dumb_grid  <- bind_rows(mapply(grid_rect, grid_xs[3:5], GRID$dumb_top,  GRID$dumb_end,
                               paste0("grid_dumb_", 1:3), SIMPLIFY = FALSE))

# Step 8: Layer "text" (no tooltips)
SHADOW <- 5

LABELS_HEAD <- tribble(
  ~id,        ~string,                                          ~x, ~top, ~size, ~font,   ~color,           ~align,
  "kicker",   "TIDYTUESDAY · 2026-01-27 · BRAZILIAN COMPANIES", 40,  28,   16,    "bold",  PAL[["pink"]],    "left",
  "headline", paste0("OTHER FIRMS RUN ", headline$median_ratio_small_label, "× THE CAPITAL"), 40,  48,   78,    "anton", PAL[["bone"]],    "left"
)

head_text   <- draw_labels(LABELS_HEAD)
head_shadow <- draw_labels(LABELS_HEAD |> filter(id == "headline") |>
                             mutate(id = "headline_sh", x = x + SHADOW, top = top + SHADOW, color = PAL[["pink"]]),
                           layer = "back")
money_short <- function(v) {
  ifelse(v >= 1e6,
         paste0("R$", floor(v / 10000 + 0.5) / 100, "M"),
         paste0("R$", round(v / 1e3), "K"))
}
LABEL_GAP <- 24
strip_labels <- bind_rows(lapply(split(size_summary, size_summary$company_size), function(r) {
  key <- r$company_size
  b   <- ROW_TOP[[key]] - 80                        # top of this strip's label block
  hot <- if (key == "other") PAL[["pink"]] else PAL[["bone"]]
  tibble(
    id     = paste0(c("name_", "pct_", "median_"), key),
    string = c(paste0(toupper(SIZE_LABEL[[key]]), " · ", format(r$n, big.mark = ","), " FIRMS"),
               paste0(round(r$pct_over_1m * 100), "% hold over R$1M"),
               money_short(r$median)),
    x      = c(STRIP$axis_x, strip_end, STRIP$axis_x),
    top    = c(b, b, b +LABEL_GAP),
    size   = c(18, 16, 44),
    font   = c("body", "body", "anton"),
    color  = c(if (key == "other") PAL[["pink"]] else PAL[["dim"]], hot, hot),
    align  = c("left", "right", "left"))
}))
strip_text <- draw_labels(strip_labels)

P <- 680   # left edge of the right panel
# TODO: flag thresholds (R$1T, R$20B) are typed here; they live in clean_brazil.R
LABELS_233 <- tribble(
  ~id,          ~string,                                   ~x,    ~top, ~size, ~font,   ~color,          ~align,
  "n233",       as.character(flag_share$n_flagged),        P,     222,  130,   "anton", PAL[["cyan"]],   "left",
  "n233_note",  "Capital near R$1T, or R$20B+ at incorporadora firms.", 900, 276, 19, "body", PAL[["dim"]], "left",
  "n233_title", "PLACEHOLDER FIRMS",                       900,   240,  28,    "anton", PAL[["bone"]],   "left",
  "lab_firms",  "SHARE OF COMPANIES",                      P,     366,  14,    "body",  PAL[["dim"]],    "left",
  "val_firms",  paste0(round(flag_share$pct_firms * 100, 2), "% · ", format(flag_share$n_flagged, big.mark = ","), " of ",
                       format(flag_share$n_firms, big.mark = ","), " firms"),
  P + 14, 388, 18,   "body",  PAL[["bone"]],   "left",
  "lab_capital","SHARE OF DECLARED CAPITAL",               P,     418,  14,    "body",  PAL[["dim"]],    "left",
  "val_capital",paste0(round(flag_share$pct_capital * 100), "% · R$", round(flag_share$flagged_capital_t),
                       "T of R$", round(flag_share$total_capital_t), "T declared"),
  P + 14, 440, 18,   "bold",  PAL[["ink"]],    "left",
  "sit", paste0("Where they sit: ", val(size_flag, "small-enterprise", "n_flagged"), " small · ",
                val(size_flag, "other", "n_flagged"), " other · ",
                val(size_flag, "micro-enterprise", "n_flagged"), " micro"),   P,     472,  18,   "body",  PAL[["cyan"]],   "left"
)

text_233   <- draw_labels(LABELS_233)
shadow_233 <- draw_labels(LABELS_233 |> filter(id == "n233") |>
                            mutate(id = "n233_sh", x = x + SHADOW, top = top + SHADOW, color = PAL[["pink"]]),
                          layer = "back")


# FILE: the remaining text labels (positions come from the v3 sketch; numbers come from Stage 2 tables)
# Needs: headline, round_share, flag_share, size_flag read in near the top, plus draw_labels(), STRIP, DUMB,
#        DUMB_Y, DOT_COLOR, x_1m, strip_end, money_short() from earlier steps.

# Two small formatters for round axis values and whole-million dumbbell labels
money_axis <- function(v) ifelse(v >= 1e9, paste0("R$", v / 1e9, "B"),
                                 ifelse(v >= 1e6, paste0("R$", v / 1e6, "M"), paste0("R$", v / 1e3, "K")))

shift_os <- shift_summary |> filter(higher == "other", lower == "small-enterprise")
stopifnot("expected one Other vs Small row"   = nrow(shift_os) == 1,
          "footnote says p < 0.001, but it isn't" = shift_os$p.value < 0.001)
test_note <- paste0("Rank tests: a random Other firm out-declares a random Small one about ",
                    round(shift_os$prob_higher * 100), "% of the time (estimated shift about ",
                    money(shift_os$estimate), ", p < 0.001).")
# 1. Fixed-position labels. Strings with numbers are built from tables, not typed.
LABELS_FIXED <- tribble(
  ~id,          ~string, ~x, ~top, ~size, ~font, ~color, ~align,
  "sub",        paste0("Declared capital of ", format(flag_share$n_firms, big.mark = ","),
                       " Brazilian companies, split by size class. Left: how capital differs by size. Right: what ",
                       flag_share$n_flagged, " placeholders do to the average."),
  40, 140, 20, "body", PAL[["dim"]], "left",
  "beat1",      "1 · THE SHIFT",                                40,  205, 15, "bold", PAL[["dim"]],  "left",
  "title1",     "The whole group slides right",                 40,  224, 32, "bold", PAL[["bone"]], "left",
  "key1",       "Each bar spans the middle half of firms (25th to 75th percentile).",
  40,  262, 15, "body", PAL[["dim"]],  "left",
  "key2",       "The tick marks the median. Placeholders removed.",
  40,  281, 15, "body", PAL[["dim"]],  "left",
  "spike",      paste0("1 in ", round_share$one_in, " firms declares exactly R$200,000."),   # TODO: 200,000 is typed; it lives in Step 15
  40,  716, 20, "body", PAL[["bone"]], "left",
  "beat2",      "2 · THE OUTLAWS",                              680, 205, 15, "bold", PAL[["dim"]],  "left",
  "dumb_title", "MEAN CAPITAL BY SIZE",                         680, 508, 14, "body", PAL[["dim"]],  "left",
  "takeaway",   paste0("With the ", flag_share$n_flagged, ", small beats other. Without them, other leads ",
                       headline$mean_ratio_small_label, " to 1."),                          680, 716, 20, "body", PAL[["bone"]], "left",
  "foot1",      "Size is set by revenue, so a capital gap may partly reflect the definitions. Medians, not means: the data is skewed and full of round amounts.",
  40,  750, 14, "body", PAL[["dim"]],  "left",
  "foot_test",  test_note,
  40,  766, 14, "body", PAL[["dim"]],  "left",
  "foot2",      "Source: Open data CNPJ via TidyTuesday · Created by Luis Pablo Martinez",
  40,  782, 14, "body", PAL[["dim"]],  "left"
)

# 2. Axis labels: the text AND the position both come from the axis definitions
axis_labels <- bind_rows(
  tibble(id = paste0("ax_s_", 1:3), string = money_axis(STRIP$v0 * 10^(0:2)),
         x = c(STRIP$axis_x, x_1m, strip_end), top = 670, size = 14,
         font = c("body", "bold", "body"), color = c(PAL[["dim"]], PAL[["bone"]], PAL[["dim"]]),
         align = c("left", "centre", "right")),
  tibble(id = paste0("ax_d_", 1:3), string = money_axis(DUMB$v0 * 10^(0:2)),
         x = DUMB$axis_x + (0:2) * DUMB$px_per_decade, top = 692, size = 14,
         font = "body", color = PAL[["dim"]], align = "centre"))

# 3. Dumbbell row names and value labels, positioned from the dot positions
dumb_names <- tibble(id = paste0("dn_", names(DUMB_Y)), string = toupper(SIZE_LABEL[names(DUMB_Y)]),
                     x = 680, top = DUMB_Y - 11, size = 20, font = "body",
                     color = c(PAL[["bone"]], PAL[["bone"]], PAL[["pink"]]), align = "left")

dumb_values <- bind_rows(lapply(split(size_flag, size_flag$company_size), function(r) {
  key <- r$company_size
  out <- tibble(id = paste0("dv_wo_", key), string = money_m(r$mean_without),
                x = x_log(r$mean_without, DUMB$axis_x, DUMB$px_per_decade, DUMB$v0),
                top = DUMB_Y[[key]] - 30, size = 17, font = "body", color = DOT_COLOR[[key]], align = "centre")
  if (r$n_flagged > 0) {
    out <- bind_rows(out, tibble(id = paste0("dv_w_", key), string = money_m(r$mean_with),
                                 x = x_log(r$mean_with, DUMB$axis_x, DUMB$px_per_decade, DUMB$v0),
                                 top = DUMB_Y[[key]] - 30, size = 17, font = "bold",
                                 color = PAL[["cyan"]], align = "centre"))
  }
  out
}))

labels_rest <- bind_rows(LABELS_FIXED, axis_labels, dumb_names, dumb_values)
rest_text   <- draw_labels(labels_rest)

LEG <- list(x = 680, top = 526, size = 18, r = 6, gap = 20, pad = 12)
leg_cy  <- LEG$top + 0.55 * LEG$size            # screen y of the symbol centres (estimate, tune in Tableau)
w_with  <- shape_string("with the 233", path = FONT_BODY, size = LEG$size)$metrics$width

x_text1 <- LEG$x + 2 * LEG$r + LEG$pad          # first phrase starts after the ring
x_dot   <- x_text1 + w_with + LEG$gap + LEG$r                                  # centre of the second symbol
x_text2 <- x_dot + LEG$r + LEG$pad

legend_shapes <- bind_rows(
  circle_poly("leg_ring_out", "text", LEG$x + LEG$r, flip_y(leg_cy), LEG$r,     PAL[["cyan"]]),
  circle_poly("leg_ring_in",  "text", LEG$x + LEG$r, flip_y(leg_cy), LEG$r - 2, PAL[["ink"]]),
  circle_poly("leg_dot",      "text", x_dot,         flip_y(leg_cy), LEG$r,     PAL[["bone"]]))

legend_text <- draw_labels(tribble(
  ~id,       ~string,          ~x,      ~top,    ~size,    ~font,  ~color,         ~align,
  "leg_wi",  "with the 233",   x_text1, LEG$top, LEG$size, "body", PAL[["cyan"]],  "left",
  "leg_wo",  "without them",   x_text2, LEG$top, LEG$size, "body", PAL[["bone"]],  "left"))

text_layer <- bind_rows(
  # Q: which strings are longest? What would you cut first if the row count is too high?
)

# Step 9: Assemble
text_layer <- bind_rows(head_text, strip_text, text_233, rest_text, legend_text, legend_shapes)
back_layer <- bind_rows(strip_tracks, prop_tracks, strip_grid, dumb_grid, dash_layer, stripe,
                        head_shadow, shadow_233)
canvas     <- bind_rows(back_layer, data_layer, text_layer)
nrow(canvas)
# Q: poly_id must be unique across layers. How do you check that, and what breaks in Tableau if not?

# Step 10: Checks
# Q: what must be true about the canvas, and which numbers should never change?
dup_layer  <- canvas |> group_by(poly_id) |> summarise(n_layers = n_distinct(layer)) |> filter(n_layers > 1)
dup_within <- canvas |> count(poly_id, point_order) |> filter(n > 1)
tiny       <- canvas |> group_by(poly_id) |>
  summarise(w = max(x) - min(x), h = max(y) - min(y)) |> filter((w == 0 | h == 0))
bad_tips   <- canvas |> filter(layer == "data") |>
  mutate(parts = str_count(tip, fixed("||"))) |>
  filter(is.na(parts) | parts != 2)
stopifnot(
  "row count over ceiling"                = nrow(canvas) <= ROW_CEILING,
  "missing coordinates"                   = !anyNA(canvas$x) && !anyNA(canvas$y),
  "layer count is not 3"                  = n_distinct(canvas$layer) == 3,
  "a poly_id appears in two layers"       = nrow(dup_layer) == 0,
  "two shapes share one poly_id"          = nrow(dup_within) == 0,
  "a shape has zero width or height"      = nrow(tiny) == 0)
# Step 10 (continued): checks that tie the canvas back to the Stage 2 tables
tick_x <- tick_layer |> group_by(poly_id) |> summarise(cx = mean(x))
stopifnot(
  "Other median should sit right of Small median" =
    tick_x$cx[tick_x$poly_id == "tick_other"] > tick_x$cx[tick_x$poly_id == "tick_small-enterprise"],
  "233 text and dumbbell counts disagree" =
    flag_share$n_flagged == sum(size_flag$n_flagged))
must_have <- c("track_other", "track_prop_capital", "grid_strip_1", "grid_dumb_1", "dash_1", "stripe_top",
               "headline_sh", "n233_sh", "bar_other", "tick_other", "prop_capital", "dumb_other_ring_in",
               "headline", "median_other", "n233", "takeaway", "leg_wi", "leg_dot", "foot_test")
have      <- unique(sub("\\|.*$", "", canvas$poly_id))
stopifnot("canvas is missing expected shapes" = all(must_have %in% have))

canvas <- canvas |> mutate(x = (x - W / 2) / DEG_PX, y = (y - H / 2) / DEG_PX)
stopifnot("canvas outside valid lat/lon" = all(abs(canvas$x) <= 180), all(abs(canvas$y) <= 90))
stopifnot("every data tip has 3 parts" =
            all(str_count(canvas$tip[canvas$layer == "data"], fixed("||")) == 2))
# Step 11: Write (temp file then rename, to guard against iCloud sync)
tmp <- paste0(OUT_FILE, ".tmp")
write_csv(canvas, tmp)
file.rename(tmp, OUT_FILE)

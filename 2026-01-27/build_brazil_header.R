# Step 1: Load packages and the glyph toolkit
library(tidyverse)
source("R/title_glyphs.R")

# Step 2: Settings
CANVAS_W <- 1300
CANVAS_H <- 805
MARGIN   <- 60
K        <- 0.01      # degrees per pixel
FONT_PATH     <- "../toolkit/fonts/BarlowCondensed-SemiBold.ttf"   # the same TTF the Python build uses
OUTPUT_FOLDER <- path.expand("~/TableauData/Tidy Tuesday")
ROWS <- c(kicker = 40, headline = 105, subtitle = 135, rule = 715, footnote = 740)
PALETTE <- c(ground = "#101010", text = "#F0F0F0", muted = "#8A8A8A", faint = "#4A4A4A", story = "#F0997B")
PANE <- c(header = "glyph", header_text = "label", footer = "label", f_rule = "line_thin")

# Step 3: Open the font
FONT <- load_title_font(FONT_PATH)

# Step 4: The words
# Q: which finding is the headline? (45 characters or fewer)
# Q: which one word carries the story, and so gets the story color?
KICKER        <- "TIDY TUESDAY · 2026-01-27 · BRAZILIAN COMPANIES"
HEADLINE_TEXT <- "___"
STORY_WORD    <- "___"
SUBTITLE      <- "___"
FOOT_LINES    <- c("___", "___")

# Step 5: Size the headline
# Q: how wide is the space between the two margins? What happens if max_w is too big?
HEADLINE_PX <- fit_size(FONT, HEADLINE_TEXT, max_w = ___, max_px = 64)

# Step 6: Place the headline word by word
# Q: each word starts where the last one ended, plus one space. What does cumsum() give you here?
words    <- str_split_1(HEADLINE_TEXT, " ")
space    <- glyph_width(FONT, " ", HEADLINE_PX)
widths   <- map_dbl(words, ~ glyph_width(FONT, .x, HEADLINE_PX))
x_starts <- MARGIN + cumsum(c(0, widths + space))[seq_along(words)]
stopifnot("story word is not in the headline" = STORY_WORD %in% words)

headline <- imap(words, function(w, i) {
  is_story <- w == STORY_WORD
  add_glyphs(FONT, "header", paste0("headline_", i), w, x_starts[i], ROWS[["headline"]],
             HEADLINE_PX, if (is_story) PALETTE[["story"]] else PALETTE[["text"]], is_story = is_story)
}) |> list_rbind()

# Step 7: Kicker, subtitle and footer
# Q: add_label() centers text on a row. What row number does the footer's second line sit on?
rows <- bind_rows(
  headline,
  add_label(FONT, "header_text", "kicker",   KICKER,   MARGIN, ROWS[["kicker"]],   14, PALETTE[["muted"]]),
  add_label(FONT, "header_text", "subtitle", SUBTITLE, MARGIN, ROWS[["subtitle"]], 14, PALETTE[["muted"]]),
  add_poly("f_rule", "rule", c(MARGIN, CANVAS_W - MARGIN), c(ROWS[["rule"]], ROWS[["rule"]]), PALETTE[["faint"]]),
  add_label(FONT, "footer", "line1", FOOT_LINES[1], MARGIN, ROWS[["footnote"]],      13, PALETTE[["muted"]]),
  add_label(FONT, "footer", "line2", FOOT_LINES[2], MARGIN, ROWS[["footnote"]] + 18, 13, PALETTE[["muted"]])
) |> mutate(pane = PANE[layer])

# Step 8: Checks
# Q: which facts must stay true if you change a word or a size?
stopifnot(
  "a layer has no pane"          = !anyNA(rows$pane),
  "duplicate polygon points"     = !anyDuplicated(rows[c("poly_id", "point_order")]),
  "text crosses the left margin"  = min(rows$lon) >= MARGIN * K - 0.05,
  "text crosses the right margin" = max(rows$lon) <= (CANVAS_W - MARGIN) * K,
  "something is below the canvas" = min(rows$lat) >= 0,
  "something is above the canvas" = max(rows$lat) < CANVAS_H * K,
  "polygon count is not ___"      = n_distinct(rows$poly_id) == ___
)

# Step 9: Preview
# Q: what would you look for in the preview that the checks cannot tell you?
preview <- ggplot(rows, aes(lon, lat, group = poly_id, fill = color)) +
  geom_polygon(data = filter(rows, layer != "f_rule")) +
  geom_path(data = filter(rows, layer == "f_rule"), aes(colour = color), linewidth = 0.4) +
  scale_fill_identity() + scale_colour_identity() +
  coord_equal(xlim = c(0, CANVAS_W * K), ylim = c(0, CANVAS_H * K), expand = FALSE) +
  theme_void() + theme(plot.background = element_rect(fill = PALETTE[["ground"]], colour = NA))
ggsave("2026-01-27/header_preview.png", preview, width = 13, height = 8.05, dpi = 100)

# Step 10: Write the header canvas
dir.create(OUTPUT_FOLDER, showWarnings = FALSE, recursive = TRUE)
write_csv(rows |> mutate(across(c(lon, lat), ~ round(.x, 4))), file.path(OUTPUT_FOLDER, "brazil_header_canvas.csv"))

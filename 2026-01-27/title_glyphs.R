# =============================================================================
# title_glyphs.R
# Turn text into polygon points, so a Tableau canvas can draw it with MAKEPOINT.
# R twin of title_glyphs.py (TitleFont).
#
# How the pieces fit together
#   load_title_font()   open a TTF file and measure its cap height
#   glyph_width()       how wide a string is, in canvas pixels
#   fit_size()          the biggest size (up to max_px) that fits max_w
#   glyph_rings()       the outline points of a string, in canvas pixels (y down)
#   add_glyphs()        glyph_rings() + color + degrees, ready to bind into the canvas
#   add_label()         add_glyphs() placed by left / center / right edge and vertical middle
#   add_poly()          any polyline or polygon from pixel points (rules, arrows)
#
# Letters with holes (O, 6, 8, %) come out as ONE ring each: the hole is joined to
# the outside by a zero-width bridge, so the polygon draws the hole as empty.
#
# Needs systemfonts >= 1.2.0 (glyph_outline). Expects these constants from the
# calling script: CANVAS_H (pixels) and K (degrees per pixel).
# =============================================================================

library(tidyverse)
library(systemfonts)

# Open a font: keep its path and its cap height as a fraction of the size.
load_title_font <- function(path) {
  path <- path.expand(path)
  stopifnot("font file not found" = file.exists(path))
  h <- glyph_outline(shape_string("H", path = path, size = 1000)$shape$index,
                     path = path, size = 1000)
  list(path = path, cap = max(h$y) / 1000)
}

# Lay a string out: one row per glyph with its x offset, plus the total advance width.
shape_text <- function(font, text, px, tracking = 0) {
  s  <- shape_string(text, path = font$path, size = px)
  sh <- s$shape |> mutate(x_offset = x_offset + (row_number() - 1) * tracking * px)
  stopifnot("font lacks a glyph for this text" = !any(sh$index == 0))
  list(shape = sh, width = s$metrics$pen_x + max(nrow(sh) - 1, 0) * tracking * px)
}

glyph_width <- function(font, text, px, tracking = 0) shape_text(font, text, px, tracking)$width

# Biggest size that fits: text widths scale with size, so measure once at 100 px.
fit_size <- function(font, texts, max_w, max_px) {
  w100 <- max(map_dbl(texts, ~ glyph_width(font, .x, 100)))
  min(max_px, max_w * 100 / w100)
}

# Signed area and point-in-polygon, for telling holes from outsides.
ring_area <- function(m) sum(m[, 1] * c(m[-1, 2], m[1, 2]) - c(m[-1, 1], m[1, 1]) * m[, 2]) / 2

in_ring <- function(px, py, m) {
  x <- m[, 1]; y <- m[, 2]; xj <- c(x[-1], x[1]); yj <- c(y[-1], y[1])
  sum(((y > py) != (yj > py)) & (px < (xj - x) * (py - y) / (yj - y) + x)) %% 2 == 1
}

# Join a hole to its outside through the two closest points.
bridge <- function(shell, hole) {
  d <- outer(shell[, 1], hole[, 1], "-")^2 + outer(shell[, 2], hole[, 2], "-")^2
  ij <- which(d == min(d), arr.ind = TRUE)[1, ]
  i <- ij[[1]]; j <- ij[[2]]
  rbind(shell[1:i, , drop = FALSE],
        hole[c(j:nrow(hole), seq_len(j - 1)), , drop = FALSE],
        hole[j, , drop = FALSE],
        shell[i:nrow(shell), , drop = FALSE])
}

# One glyph's contours (list of n x 2 matrices) in, one ring per outside out.
merge_holes <- function(contours) {
  if (length(contours) == 1) return(contours)
  area  <- map_dbl(contours, ring_area)
  big   <- which.max(abs(area))
  is_hole <- sign(area) != sign(area[big])
  shells <- contours[!is_hole]
  s_area <- abs(area[!is_hole])
  for (h in contours[is_hole]) {
    inside <- which(map_lgl(shells, ~ in_ring(h[1, 1], h[1, 2], .x)))
    if (length(inside) == 0) { shells <- c(shells, list(h)); s_area <- c(s_area, abs(ring_area(h))); next }
    k <- inside[which.min(s_area[inside])]
    shells[[k]] <- bridge(shells[[k]], h)
  }
  shells
}

# Outline points of a string, in canvas pixels (y down), one ring_id per ring.
glyph_rings <- function(font, text, x_left, baseline_y, px, tracking = 0, tolerance = 0.25) {
  sh <- shape_text(font, text, px, tracking)$shape
  o  <- glyph_outline(sh$index, path = font$path, size = px, tolerance = tolerance)
  if (nrow(o) == 0) return(tibble(ring_id = character(), point_order = integer(), x = numeric(), y = numeric()))
  o |>
    mutate(x = x_left + x + sh$x_offset[glyph],
           y = baseline_y - y - sh$y_offset[glyph]) |>
    group_split(glyph, .keep = TRUE) |>
    map(function(g) {
      contours <- g |> group_split(contour) |> map(~ cbind(.x$x, .x$y))
      rings <- merge_holes(contours)
      imap(rings, ~ tibble(ring_id = paste0("g", g$glyph[1], "_", .y),
                           point_order = seq_len(nrow(.x)) - 1L, x = .x[, 1], y = .x[, 2]))
    }) |>
    list_flatten() |>
    bind_rows()
}

to_canvas <- function(x, y_screen, canvas_h = CANVAS_H, k = K) {
  tibble(lon = x * k, lat = (canvas_h - y_screen) * k)    # screen y runs down, latitude runs up
}

# Text as polygon rows. Same columns as the Python canvas.
add_glyphs <- function(font, layer, text_id, text, x_left, baseline_y, size_px, color,
                       tracking = 0, is_story = FALSE, tooltip = "", tolerance = 0.25) {
  rings <- glyph_rings(font, text, x_left, baseline_y, size_px, tracking, tolerance)
  bind_cols(rings, to_canvas(rings$x, rings$y)) |>
    transmute(layer, poly_id = paste(layer, text_id, ring_id, sep = "|"), point_order, lon, lat,
              color, is_story, label = "", tooltip)
}

# Text placed by its left / center / right edge and its vertical middle.
add_label <- function(font, layer, text_id, text, x, y_mid, px, color, align = "left",
                      is_story = FALSE, tolerance = 0.5) {
  w <- glyph_width(font, text, px)
  x_left <- switch(align, left = x, center = x - w / 2, right = x - w)
  add_glyphs(font, layer, text_id, text, x_left, y_mid + font$cap * px / 2, px, color,
             is_story = is_story, tolerance = tolerance)
}

# Any polyline or point from pixel coordinates: a list of (x, y) rows.
add_poly <- function(layer, poly_id, x, y, color, is_story = FALSE, tooltip = "") {
  tibble(layer, poly_id = paste(layer, poly_id, sep = "|"), point_order = seq_along(x) - 1L,
         to_canvas(x, y), color, is_story, label = "", tooltip)
}

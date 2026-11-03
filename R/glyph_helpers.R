# glyph_helpers.R : shared toolkit for the TidyTuesday canvas scripts.
# Every mark becomes polygon rows: layer, poly_id, point_order, x, y, color, tip. Text is drawn as glyph outlines.
#
# Use it like this, in the calling script:
#   1. library(tidyverse); library(systemfonts)
#   2. define H, font_paths, tol_for and BASE (see below)
#   3. source("glyph_helpers.R"); check_helper_globals()
#
# The functions read these globals when they RUN, not when this file is sourced:
#   H           canvas height in pixels (flip_y)
#   font_paths  named vector of font file paths, e.g. c(anton = ..., bold = ..., body = ...)
#   tol_for     named vector of curve tolerances, same names as font_paths (a larger number means fewer vertices)
#   BASE        baseline ratio: a label's baseline sits at top + BASE * size (draw_labels)

check_helper_globals <- function() {
  need    <- c("H", "font_paths", "tol_for", "BASE")
  missing <- need[!vapply(need, exists, logical(1))]
  if (length(missing) > 0) stop("define before using glyph_helpers.R: ", paste(missing, collapse = ", "))
  stopifnot("font_paths and tol_for need the same names" = setequal(names(font_paths), names(tol_for)))
  invisible(TRUE)
}

# ---- Position helpers -------------------------------------------------------
# log axis: the value v0 sits at pixel axis_x, and one tenfold step is px_per_decade pixels long

x_log <- function(v, axis_x, px_per_decade, v0) axis_x + (log10(v) - log10(v0)) * px_per_decade
# screen y grows down, Tableau y grows up: flip once, at the point where a shape is built
flip_y <- function(y_screen) H - y_screen

# ---- Shape primitives (each returns one polygon as rows) ----------------------
rect_poly <- function(id, layer, x0, y0, x1, y1, color, tip = "") {
  tibble(layer = layer, poly_id = id, point_order = 1:4,
         x = c(x0, x1, x1, x0), y = c(y0, y0, y1, y1),
         color = color, tip = tip)
}

circle_poly <- function(id, layer, cx, cy, r, color, tip = "", n = 24) {
  theta <- seq(0, 2 * pi, length.out = n + 1)[-1]
  # angle 0 and 2*pi are the same point, so drop the first to avoid repeating it
  tibble(layer = layer, poly_id = id, point_order = seq_len(n),
         x = cx + r * cos(theta),
         y = cy + r * sin(theta),
         color = color, tip = tip)
}

# ---- Glyph geometry: turn a letter outline (with holes) into one ring Tableau can fill ----
signed_area <- function(x, y) {
  x2 <- c(x[-1], x[1])   # each point's next neighbour, wrapping round
  y2 <- c(y[-1], y[1])
  0.5 * sum(x * y2 - x2 * y)
}

ccw <- function(a, b, p) (p[2] - a[2]) * (b[1] - a[1]) - (p[1] - a[1])* (b[2] - a[2])

opposite <- function(u, v, eps) (u > eps & v < -eps) | (u < -eps & v > eps)

crosses <- function(a, b, p, q, eps = 1e-9) {
  d1 <- ccw(a, b, p)
  d2 <- ccw(a, b, q)
  d3 <- ccw(p, q, a)
  d4 <- ccw(p, q, b)
  opposite(d1, d2, eps) & opposite(d3, d4, eps)
}

inside <- function(px, py, rx, ry) {
  rx2 <- c(rx[-1], rx[1])    # each vertex's next neighbour, wrapping round
  ry2 <- c(ry[-1], ry[1])
  hit <- (ry > py) != (ry2 > py) & px < rx + (py- ry)/ (ry2 - ry) * (rx2 - rx)
  sum(hit) %% 2 == 1
}

edges_of <- function(rings) {
  do.call(rbind, lapply(rings, function(m) {
    n <- nrow(m)
    cbind(m, m[c(2:n, 1), , drop = FALSE])    # each point beside the next one
  }))
}

any_cross <- function(a, b, edges) {
  for (k in seq_len(nrow(edges))) {
    if (crosses(a, b, edges[k, 1:2], edges[k, 3:4])) return(TRUE)
  }
  FALSE
}

bridge <- function(outer, holes) {
  ring <- outer                                   # a 2-column matrix of points
  ord <- order(-sapply(holes, function(h) max(h[, 1])))   # rightmost hole first
  holes <- holes[ord]
  for (k in seq_along(holes)) {
    hole   <- holes[[k]]
    others <- holes[-seq_len(k)]                  # holes still to be bridged
    edges  <- edges_of(c(list(ring, hole), others))
    pairs  <- expand.grid(i = seq_len(nrow(ring)), j = seq_len(nrow(hole)))
    pairs$d <- sqrt((ring[pairs$i, 1] - hole[pairs$j, 1])^2 + (ring[pairs$i, 2] - hole[pairs$j, 2])^2)                           # distance for each pairing
    pairs  <- pairs[order(pairs$d), ]
    done <- FALSE
    for (r in seq_len(nrow(pairs))) {
      i <- pairs$i[r]; j <- pairs$j[r]
      if (!any_cross(ring[i, ], hole[j, ], edges)) {
        ring <- rbind(ring[seq_len(i), , drop = FALSE], hole[j:nrow(hole), , drop = FALSE], 
                      hole[seq_len(j - 1), , drop = FALSE], hole[j, , drop = FALSE], 
                      ring[i, , drop = FALSE], ring[-seq_len(i), , drop = FALSE])
        done <- TRUE
        break
      }
    }
    stopifnot("no clear bridge found for a hole" = done)
  }
  ring
}

glyph_rings <- function(ch, font_path, tol = 0.2) {
  s  <- shape_string(ch, path = font_path, size = 100)
  ol <- glyph_outline(s$shape$index, font_path, size = 100, tolerance = tol)
  if (nrow(ol) == 0) return(list())               # a space has no outline

  rings <- lapply(split(ol, ol$contour), function(g) cbind(g$x, g$y))
  areas <- unname(sapply(rings, function(m) signed_area(m[, 1], m[, 2])))
  outer_sign <- sign(areas[which.max(abs(areas))])
  is_outer <- sign(areas) == outer_sign
  
  outers <- rings[is_outer]
  holes  <- rings[!is_outer]
  
  # which outer ring owns each hole? one number per hole
  owner <- sapply(holes, function(h) {
    cands <- which(sapply(outers, function(o) inside(h[1, 1], h[1, 2], o[, 1], o[, 2])))
    stopifnot("a hole sits inside no outer ring" = length(cands) > 0)
    cands[which.min(abs(areas[is_outer][cands]))]
  })
  
  lapply(seq_along(outers), function(k) {
    mine <- holes[which(owner == k)]
    ring <- if (length(mine) > 0) bridge(outers[[k]], mine) else outers[[k]]
    want <- areas[is_outer][k] + sum(areas[!is_outer][which(owner == k)])
    stopifnot("bridged area differs from outer plus holes" = isTRUE(all.equal(signed_area(ring[, 1], ring[, 2]), want)))
    ring
  })
}

# ---- Text as polygons --------------------------------------------------------
text_poly <- function(id, layer, string, x, y, size, font_path, color,
                      align = "left", tol = 0.2) {
  stopifnot("align must be left, centre or right" = align %in% c("left", "centre", "right"))
  sh    <- shape_string(string, path = font_path, size = 100)
  chars <- strsplit(string, "")[[1]]
  stopifnot("shaping changed the glyph count (a ligature?)" = nrow(sh$shape) == length(chars))
  s <- size / 100                     # everything was measured at size 100
  w <- sh$metrics$width * s           # the string's width at this size
  x_start <- switch(align, left = x, centre = x- w/2, right = x -w)
  
  rows <- lapply(seq_along(chars), function(n) {
    rings <- glyph_rings(chars[n], font_path, tol)
    lapply(seq_along(rings), function(k) {
      m <- rings[[k]]                 # this ring's points, at size 100
      tibble(layer = layer, poly_id = paste(id, n, k, sep = "|"),
             point_order = seq_len(nrow(m)),
             x = x_start + (sh$shape$x_offset[n] + m[, 1]) * s,
             y = flip_y(y - m[,2] * s),
             color = color, tip = "")
    })
  })
  bind_rows(unlist(rows, recursive = FALSE))
}

# draw every row of a label table (id, string, x, top, size, font, color, align); `top` is a screen y
# and the baseline lands at top + BASE * size
draw_labels <- function(L, layer = "text") {
  pmap_dfr(L, function(id, string, x, top, size, font, color, align, ...) {
    text_poly(id, layer, string, x, top + BASE * size, size,
              font_paths[[font]], color, align, tol_for[[font]])
  })
}

# P062 - diagram_plot: flow diagram built from diagram_box / diagram_line components
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---- %diagram_box -----------------------------------------------------------
diagram_box <- function(boxid, left_x, top_y, right_x, bottom_y, text,
                        text_just = "center", text_size = 10,
                        text_color = "black", linecolor = "black",
                        linepattern = "solid", linethickness = 1) {
  # Corner coordinates of the box (polygon vertices)
  box <- data.frame(
    DIAGID = as.character(boxid),
    boxid  = boxid,
    box_x  = c(left_x, right_x, right_x, left_x),
    box_y  = c(top_y, top_y, bottom_y, bottom_y),
    stringsAsFactors = FALSE
  )

  # Text position from the intersection of the diagonals
  x1 <- left_x;  y1 <- top_y
  x2 <- right_x; y2 <- top_y
  x3 <- right_x; y3 <- bottom_y
  x4 <- left_x;  y4 <- bottom_y
  den <- (x1 - x3) * (y2 - y4) - (y1 - y3) * (x2 - x4)
  text_x <- NA_real_; text_y <- NA_real_; text_x2 <- NA_real_; text_y2 <- NA_real_
  t1 <- NA_character_; t2 <- NA_character_
  if (den == 0) {
    message("WARNING: The diagonals are parallel or coincide. A quadrilateral cannot be formed.")
  } else {
    num1 <- x1 * y3 - y1 * x3
    num2 <- x2 * y4 - y2 * x4
    if (toupper(text_just) != "LEFT") {
      text_x <- (num1 * (x2 - x4) - (x1 - x3) * num2) / den
      text_y <- (num1 * (y2 - y4) - (y1 - y3) * num2) / den
      t1 <- text
    } else {
      text_x2 <- x1 + 1
      text_y2 <- (num1 * (y2 - y4) - (y1 - y3) * num2) / den
      t2 <- text
    }
  }
  txt <- data.frame(
    text_x = text_x, text_y = text_y, text_x2 = text_x2, text_y2 = text_y2,
    text = t1, text2 = t2, sizeresponse = text_size,
    x1 = x1, y1 = y1, x2 = x2, y2 = y2, x3 = x3, y3 = y3, x4 = x4, y4 = y4,
    DIAGID = as.character(boxid), stringsAsFactors = FALSE
  )
  style <- data.frame(
    id = "DIAGID", value = as.character(boxid),
    textcolor = text_color, linecolor = linecolor,
    linepattern = linepattern, linethickness = linethickness,
    stringsAsFactors = FALSE
  )
  list(box = box, text = txt, style = style)
}

# ---- %diagram_line ----------------------------------------------------------
diagram_line <- function(lineid, start_x, start_y, end_x, end_y,
                         midpoint_x = NULL, midpoint_y = NULL,
                         linecolor = "black", linepattern = "solid",
                         linethickness = 1) {
  xs <- c(start_x, if (!is.null(midpoint_x) && !is.null(midpoint_y)) midpoint_x, end_x)
  ys <- c(start_y, if (!is.null(midpoint_x) && !is.null(midpoint_y)) midpoint_y, end_y)
  ln <- data.frame(
    `_LINEID` = as.character(lineid), lineid = lineid,
    line_x = xs, line_y = ys, check.names = FALSE, stringsAsFactors = FALSE
  )
  style <- data.frame(
    id = "_LINEID", value = as.character(lineid),
    linecolor = linecolor, linepattern = linepattern,
    linethickness = linethickness, stringsAsFactors = FALSE
  )
  list(line = ln, style = style)
}

# ---- Build components (driver usage example) --------------------------------
b1 <- diagram_box(boxid = 1, left_x = 30, top_y = 100, right_x = 70, bottom_y = 90,
                  text = "Assessed for eligibility (n=100)")
l1 <- diagram_line(lineid = 1, start_x = 50, start_y = 90, end_x = 50, end_y = 55)
b2 <- diagram_box(boxid = 2, left_x = 37, top_y = 55, right_x = 63, bottom_y = 45,
                  text = "Randomized (n=90)")

# ---- %diagram_plot ----------------------------------------------------------
# SET diagram_: reads datasets in alphabetical name order:
#   DIAGRAM_BOX_1, DIAGRAM_BOX_2, DIAGRAM_BOX_TEXT_1, DIAGRAM_BOX_TEXT_2, DIAGRAM_LINE_1
plot_diag <- bind_rows(b1$box, b2$box, b1$text, b2$text, l1$line)
col_order <- c("text", "text2", "DIAGID", "boxid", "box_x", "box_y",
               "text_x", "text_y", "text_x2", "text_y2", "sizeresponse",
               "x1", "y1", "x2", "y2", "x3", "y3", "x4", "y4",
               "_LINEID", "lineid", "line_x", "line_y")
plot_diag <- plot_diag[, col_order]

# Attribute map (diag_style_* then line_style_*)
plot_diag_attr <- bind_rows(b1$style, b2$style, l1$style)

write.csv(plot_diag, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

# ---- Plot -------------------------------------------------------------------
boxes  <- plot_diag %>% filter(!is.na(boxid))
texts  <- plot_diag %>% filter(!is.na(text_x))
texts2 <- plot_diag %>% filter(!is.na(text_x2))
lines  <- plot_diag %>% filter(!is.na(lineid))

split_txt <- function(x) gsub("#", "\n", x, fixed = TRUE)

p <- ggplot() +
  geom_polygon(data = boxes, aes(x = box_x, y = box_y, group = boxid),
               fill = NA, colour = "black", linewidth = 0.5) +
  geom_path(data = lines, aes(x = line_x, y = line_y, group = lineid),
            colour = "black", linewidth = 0.5,
            arrow = arrow(type = "closed", length = unit(0.12, "inches")))

if (nrow(texts) > 0) {
  p <- p + geom_text(data = texts,
                     aes(x = text_x, y = text_y, label = split_txt(text)),
                     size = texts$sizeresponse / .pt, family = "mono",
                     colour = "black")
}
if (nrow(texts2) > 0) {
  p <- p + geom_text(data = texts2,
                     aes(x = text_x2, y = text_y2, label = split_txt(text2)),
                     size = texts2$sizeresponse / .pt, family = "mono",
                     hjust = 0, colour = "black")
}

p <- p +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", colour = NA))

ggsave(file.path(out, "sgplot.png"), p, width = 8, height = 6, dpi = 96)

message("P062 done: sgplot.png and sgplot.csv written to ", out)

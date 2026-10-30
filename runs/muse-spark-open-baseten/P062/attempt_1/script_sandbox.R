# P062_diagram_plot.sas -> R translation
# Creates flow diagram with 2 boxes and 1 connecting line (CONSORT-style example)
# Outputs: sgplot.png + CSV of plotted data

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(grid)
})

# Helper: SAS round half away from zero (not strictly needed here but kept for fidelity)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-12)
  posneg * z / 10^digits
}

# -------------------------------------------------------------------------
# diagram_box equivalent
# Creates:
#   diagram_box_<boxid>      : 4 corner coordinates
#   diagram_box_text_<boxid> : text position(s)
#   diag_style_<boxid>       : style attributes
# -------------------------------------------------------------------------
diagram_box <- function(boxid = 1, left_x = 10, top_y = 100, right_x = 30, bottom_y = 90,
                        text = "", text_just = "center", text_size = 10, text_color = "black",
                        linecolor = "black", linepattern = "solid", linethickness = 1) {
  DIAGID <- as.character(boxid)

  # box polygon (4 corners, closed by geom_polygon)
  box_df <- data.frame(
    DIAGID = DIAGID,
    boxid  = as.numeric(boxid),
    box_x  = c(left_x, right_x, right_x, left_x),
    box_y  = c(top_y,  top_y,  bottom_y, left_x * 0 + bottom_y), # keep numeric
    stringsAsFactors = FALSE
  )
  # correct bottom_y for 4th point (avoid vector recycling confusion)
  box_df$box_y[4] <- bottom_y

  # text position: intersection of diagonals = center of rectangle
  # SAS computes via diagonal intersection; for axis-aligned rectangle this is midpoint
  x1 <- left_x;  y1 <- top_y
  x2 <- right_x; y2 <- top_y
  x3 <- right_x; y3 <- bottom_y
  x4 <- left_x;  y4 <- bottom_y
  den <- (x1 - x3) * (y2 - y4) - (y1 - y3) * (x2 - x4)
  if (den == 0) {
    warning("The diagonals are parallel or coincide. A quadrilateral cannot be formed.")
    text_x <- NA_real_; text_y <- NA_real_; text_x2 <- NA_real_; text_y2 <- NA_real_
  } else {
    num1 <- (x1 * y3 - y1 * x3)
    num2 <- (x2 * y4 - y2 * x4)
    cx <- (num1 * (x2 - x4) - (x1 - x3) * num2) / den
    cy <- (num1 * (y2 - y4) - (y1 - y3) * num2) / den
    if (toupper(text_just) != "LEFT") {
      text_x <- cx; text_y <- cy; text_x2 <- NA_real_; text_y2 <- NA_real_
    } else {
      text_x <- NA_real_; text_y <- NA_real_; text_x2 <- x1 + 1; text_y2 <- cy
    }
  }

  if (toupper(text_just) != "LEFT") {
    text_df <- data.frame(
      DIAGID = DIAGID,
      text_x = text_x, text_y = text_y,
      text_x2 = NA_real_, text_y2 = NA_real_,
      text = as.character(text), text2 = NA_character_,
      sizeresponse = as.numeric(text_size),
      stringsAsFactors = FALSE
    )
  } else {
    text_df <- data.frame(
      DIAGID = DIAGID,
      text_x = NA_real_, text_y = NA_real_,
      text_x2 = text_x2, text_y2 = text_y2,
      text = NA_character_, text2 = as.character(text),
      sizeresponse = as.numeric(text_size),
      stringsAsFactors = FALSE
    )
  }

  style_df <- data.frame(
    id = "DIAGID",
    value = as.character(boxid),
    textcolor = as.character(text_color),
    linecolor = as.character(linecolor),
    linepattern = as.character(linepattern),
    linethickness = as.numeric(linethickness),
    stringsAsFactors = FALSE
  )

  list(box = box_df, text = text_df, style = style_df)
}

# -------------------------------------------------------------------------
# diagram_line equivalent
# -------------------------------------------------------------------------
diagram_line <- function(lineid = 1, start_x = 30, start_y = 90,
                         midpoint_x = NULL, midpoint_y = NULL,
                         end_x = 30, end_y = 60,
                         linecolor = "black", linepattern = "solid", linethickness = 1) {
  LINEID <- as.character(lineid)
  has_mid <- !is.null(midpoint_x) && !is.null(midpoint_y) &&
             !is.na(midpoint_x) && !is.na(midpoint_y) &&
             nzchar(as.character(midpoint_x)) && nzchar(as.character(midpoint_y))

  if (has_mid) {
    line_df <- data.frame(
      `_LINEID` = rep(LINEID, 3),
      lineid = rep(as.numeric(lineid), 3),
      line_x = c(as.numeric(start_x), as.numeric(midpoint_x), as.numeric(end_x)),
      line_y = c(as.numeric(start_y), as.numeric(midpoint_y), as.numeric(end_y)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  } else {
    line_df <- data.frame(
      `_LINEID` = rep(LINEID, 2),
      lineid = rep(as.numeric(lineid), 2),
      line_x = c(as.numeric(start_x), as.numeric(end_x)),
      line_y = c(as.numeric(start_y), as.numeric(end_y)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }

  style_df <- data.frame(
    id = "_LINEID",
    value = as.character(lineid),
    linecolor = as.character(linecolor),
    linepattern = as.character(linepattern),
    linethickness = as.numeric(linethickness),
    stringsAsFactors = FALSE
  )

  list(line = line_df, style = style_df)
}

# -------------------------------------------------------------------------
# Build the header's usage example (as SAS driver does)
# -------------------------------------------------------------------------
b1 <- diagram_box(boxid = 1, left_x = 30, top_y = 100, right_x = 70, bottom_y = 90,
                  text = "Assessed for eligibility (n=100)")
l1 <- diagram_line(lineid = 1, start_x = 50, start_y = 90, end_x = 50, end_y = 55)
b2 <- diagram_box(boxid = 2, left_x = 37, top_y = 55, right_x = 63, bottom_y = 45,
                  text = "Randomized (n=90)")

# Combine for plot_diag (SAS: set diagram_:)
# Use bind_rows with fill to mimic SAS SET stacking different columns
plot_diag <- dplyr::bind_rows(b1$box, b2$box, b1$text, b2$text, l1$line)

# Combine attributes (SAS: set diag_style_: line_style_:)
plot_diag_attr <- dplyr::bind_rows(b1$style, b2$style, l1$style)

# Prepare data subsets for ggplot
boxes <- dplyr::bind_rows(b1$box, b2$box)
texts_center <- dplyr::bind_rows(b1$text, b2$text) %>%
  dplyr::filter(!is.na(text) & nzchar(text)) %>%
  dplyr::mutate(text_plot = gsub("#", "\n", text, fixed = TRUE))

texts_left <- dplyr::bind_rows(b1$text, b2$text) %>%
  dplyr::filter(!is.na(text2) & nzchar(text2)) %>%
  dplyr::mutate(text2_plot = gsub("#", "\n", text2, fixed = TRUE))

lines <- l1$line

# For CSV: create a tidy representation of all plotted elements
# Keep original SAS variable names where possible
plot_diag_csv <- plot_diag
# Ensure consistent column order for CSV
# Write plot_diag and attributes
write.csv(plot_diag, file.path(out, "plot_diag.csv"), row.names = FALSE, na = "")
write.csv(plot_diag_attr, file.path(out, "plot_diag_attr.csv"), row.names = FALSE, na = "")

# Also write a dedicated sgplot CSV (plotted data) as required
# Include boxes, texts, lines in one file with type indicator
sgplot_data <- dplyr::bind_rows(
  boxes %>% mutate(type = "box"),
  texts_center %>% mutate(type = "text_center"),
  texts_left %>% mutate(type = "text_left"),
  lines %>% mutate(type = "line")
)
# For the expected name, write both sgplot.csv and sgplot_data.csv
write.csv(sgplot_data, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")
write.csv(sgplot_data, file.path(out, "sgplot_data.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Plot: replicate PROC SGPLOT polygon + text + series with arrows
# -------------------------------------------------------------------------
# Map text size: SAS sizeresponse 10pt -> ggplot size ~ 10 * 0.35 = 3.5mm approx
# Use size in mm via geom_text size (pt). Keep 4 for readability.
p <- ggplot() +
  geom_polygon(data = boxes,
               aes(x = box_x, y = box_y, group = boxid),
               fill = "white", colour = "black", linewidth = 0.6) +
  geom_text(data = texts_center,
            aes(x = text_x, y = text_y, label = text_plot),
            family = "Courier", size = 3.5, colour = "black",
            lineheight = 0.9) +
  # left-justified texts (not used in this example but kept for completeness)
  { if (nrow(texts_left) > 0)
      geom_text(data = texts_left,
                aes(x = text_x2, y = text_y2, label = text2_plot),
                family = "Courier", size = 3.5, colour = "black",
                hjust = 0, lineheight = 0.9)
    else NULL } +
  geom_path(data = lines,
            aes(x = line_x, y = line_y, group = lineid),
            colour = "black", linewidth = 0.6,
            arrow = arrow(type = "closed", length = unit(0.25, "cm"), angle = 25)) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_blank(),
    plot.margin = margin(10, 10, 10, 10, "pt")
  )

# Save figure as PNG (SAS SGPlot.png -> sgplot.png)
ggsave(filename = file.path(out, "sgplot.png"), plot = p,
       width = 8, height = 6, units = "in", dpi = 300, bg = "white")
# Also save as SGPlot.png for case-sensitive check
ggsave(filename = file.path(out, "SGPlot.png"), plot = p,
       width = 8, height = 6, units = "in", dpi = 300, bg = "white")

cat("Outputs written to:", out, "\n")
cat(" - sgplot.png / SGPlot.png\n")
cat(" - sgplot.csv (plotted data)\n")
cat(" - plot_diag.csv, plot_diag_attr.csv\n")

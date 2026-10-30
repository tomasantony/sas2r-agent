```r
# P062_diagram_plot.sas -> R translation
# Produces CONSORT-style flow diagram with 2 boxes and 1 connecting line
# Outputs: OUT_DIR/sgplot.png + OUT_DIR/sgplot.csv (plotted data)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(grid)
})

# Helper: SAS round half away from zero (not needed here but included for fidelity)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-12)
  posneg * z / 10^digits
}

# Emulate %diagram_box macro
diagram_box <- function(boxid = 1, left_x = 10, top_y = 100, right_x = 30, bottom_y = 90,
                        text = "", text_just = "center", text_size = 10,
                        text_color = "black", linecolor = "black",
                        linepattern = "solid", linethickness = 1) {
  # box polygon coordinates (4 corners, closed by polygon geom)
  box_df <- data.frame(
    DIAGID = as.character(boxid),
    boxid  = as.numeric(boxid),
    box_x  = c(left_x, right_x, right_x, left_x),
    box_y  = c(top_y, top_y, bottom_y, bottom_y),
    stringsAsFactors = FALSE
  )

  # text position: intersection of diagonals (center of rectangle)
  x1 <- left_x; y1 <- top_y
  x2 <- right_x; y2 <- top_y
  x3 <- right_x; y3 <- bottom_y
  x4 <- left_x; y4 <- bottom_y
  den <- (x1 - x3) * (y2 - y4) - (y1 - y3) * (x2 - x4)
  num1 <- (x1 * y3 - y1 * x3)
  num2 <- (x2 * y4 - y2 * x4)

  text_x <- NA_real_; text_y <- NA_real_; text_val <- NA_character_
  text_x2 <- NA_real_; text_y2 <- NA_real_; text2_val <- NA_character_

  if (den != 0) {
    cx <- (num1 * (x2 - x4) - (x1 - x3) * num2) / den
    cy <- (num1 * (y2 - y4) - (y1 - y3) * num2) / den
    if (toupper(text_just) != "LEFT") {
      text_x <- cx
      text_y <- cy
      text_val <- text
    } else {
      text_x2 <- x1 + 1
      text_y2 <- cy
      text2_val <- text
    }
  }

  text_df <- data.frame(
    DIAGID = as.character(boxid),
    text_x = text_x,
    text_y = text_y,
    text   = text_val,
    text_x2 = text_x2,
    text_y2 = text_y2,
    text2  = text2_val,
    sizeresponse = as.numeric(text_size),
    stringsAsFactors = FALSE
  )

  style_df <- data.frame(
    id = "DIAGID",
    value = as.character(boxid),
    textcolor = text_color,
    linecolor = linecolor,
    linepattern = linepattern,
    linethickness = as.numeric(linethickness),
    stringsAsFactors = FALSE
  )

  list(box = box_df, text = text_df, style = style_df)
}

# Emulate %diagram_line macro
diagram_line <- function(lineid = 1, start_x = 30, start_y = 90,
                         midpoint_x = NA, midpoint_y = NA,
                         end_x = 30, end_y = 60,
                         linecolor = "black", linepattern = "solid", linethickness = 1) {
  if (!is.na(midpoint_x) && !is.na(midpoint_y)) {
    line_df <- data.frame(
      `_LINEID` = as.character(lineid),
      lineid = as.numeric(lineid),
      line_x = c(start_x, midpoint_x, end_x),
      line_y = c(start_y, midpoint_y, end_y),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    line_df <- data.frame(
      `_LINEID` = as.character(lineid),
      lineid = as.numeric(lineid),
      line_x = c(start_x, end_x),
      line_y = c(start_y, end_y),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  style_df <- data.frame(
    id = "_LINEID",
    value = as.character(lineid),
    linecolor = linecolor,
    linepattern = linepattern,
    linethickness = as.numeric(linethickness),
    stringsAsFactors = FALSE
  )
  list(line = line_df, style = style_df)
}

# ---- Build header's usage example ----
# %diagram_box(boxid=1, left_x=30, top_y=100, right_x=70, bottom_y=90, text=Assessed for eligibility (n=100));
b1 <- diagram_box(boxid = 1, left_x = 30, top_y = 100, right_x = 70, bottom_y = 90,
                  text = "Assessed for eligibility (n=100)", text_just = "center", text_size = 10)

# %diagram_line(lineid=1, start_x=50, start_y=90, end_x=50, end_y=55);
l1 <- diagram_line(lineid = 1, start_x = 50, start_y = 90, end_x = 50, end_y = 55)

# %diagram_box(boxid=2, left_x=37, top_y=55, right_x=63, bottom_y=45, text=Randomized (n=90));
b2 <- diagram_box(boxid = 2, left_x = 37, top_y = 55, right_x = 63, bottom_y = 45,
                  text = "Randomized (n=90)", text_just = "center", text_size = 10)

# ---- Combine as SAS does: data plot_diag; set diagram_:; run; ----
# In SAS, SET diagram_: stacks all datasets with prefix diagram_ (boxes, texts, lines)
# Emulate with dplyr::bind_rows (fills missing columns with NA)
plot_diag <- dplyr::bind_rows(b1$box, b2$box, b1$text, b2$text, l1$line)

plot_diag_attr <- dplyr::bind_rows(b1$style, b2$style, l1$style)

# Prepare separate data frames for plotting (more convenient than single stacked)
boxes_df <- dplyr::bind_rows(b1$box, b2$box)
# Add group identifier for polygon
boxes_df$group_id <- boxes_df$DIAGID

texts_df <- dplyr::bind_rows(b1$text, b2$text)
# Handle SPLITCHAR='#' : replace # with newline for ggplot
texts_df <- texts_df %>%
  mutate(
    text_plot  = gsub("#", "\n", text, fixed = TRUE),
    text2_plot = gsub("#", "\n", text2, fixed = TRUE)
  )

lines_df <- l1$line

# For CSV of plotted data: export the combined plot_diag plus explicit boxes/lines/texts
# Ensure ISO formatting not needed (no dates), but keep column names SAS-like
# Write plot_diag.csv (full stacked) and sgplot.csv (plotted data)
write.csv(plot_diag, file.path(out, "plot_diag.csv"), row.names = FALSE, na = "")
write.csv(plot_diag, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

# Also write individual components for traceability
write.csv(boxes_df, file.path(out, "diagram_boxes.csv"), row.names = FALSE, na = "")
write.csv(texts_df, file.path(out, "diagram_texts.csv"), row.names = FALSE, na = "")
write.csv(lines_df, file.path(out, "diagram_lines.csv"), row.names = FALSE, na = "")

# ---- Plot: emulate PROC SGPLOT polygon + text + series with arrow ----
# Parameters from %diagram_plot(minx=0,maxx=100,miny=0,maxy=100,text_font=Courier)
minx <- 0; maxx <- 100; miny <- 0; maxy <- 100
text_font <- "Courier"

# Build ggplot
p <- ggplot() +
  # Boxes as polygons (white fill, black border, thickness 1)
  geom_polygon(data = boxes_df,
               aes(x = box_x, y = box_y, group = group_id),
               fill = "white", colour = "black", linewidth = 0.8) +
  # Lines with arrowhead (filled, scale 0.3 equivalent)
  geom_path(data = lines_df,
            aes(x = line_x, y = line_y, group = lineid),
            colour = "black", linewidth = 0.8,
            arrow = arrow(type = "closed", length = unit(0.15, "inches"))) +
  # Center-justified text (text_x/text_y)
  geom_text(data = texts_df %>% filter(!is.na(text)),
            aes(x = text_x, y = text_y, label = text_plot),
            family = text_font, size = 3.5, colour = "black",
            hjust = 0.5, vjust = 0.5, lineheight = 0.9) +
  # Left-justified text (text_x2/text_y2) - not used in this example but included for fidelity
  geom_text(data = texts_df %>% filter(!is.na(text2)),
            aes(x = text_x2, y = text_y2, label = text2_plot),
            family = text_font, size = 3.5, colour = "black",
            hjust = 0, vjust = 0.5, lineheight = 0.9) +
  coord_cartesian(xlim = c(minx, maxx), ylim = c(miny, maxy), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_blank(),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5, "pt")
  )

# Save PNG (noborder, noautolegend, no axes)
# Use 8x6 inches at 300 dpi to match SAS default quality
ggsave(filename = file.path(out, "sgplot.png"), plot = p,
       width = 8, height = 6, units = "in", dpi = 300, bg = "white")

# Also save as SGPlot.png for case-sensitive check (same file)
# Copy if needed
if (!file.exists(file.path(out, "SGPlot.png"))) {
  file.copy(file.path(out, "sgplot.png"), file.path(out, "SGPlot.png"), overwrite = TRUE)
}

# Write a plain-text rendering of the diagram description (for completeness)
txt_lines <- c(
  "Flow Diagram (P062) - CONSORT style",
  "====================================",
  "",
  "Boxes:",
  sprintf("  Box 1: left=30 top=100 right=70 bottom=90  text='Assessed for eligibility (n=100)'  center=(50,95)"),
  sprintf("  Box 2: left=37 top=55  right=63 bottom=45  text='Randomized (n=90)'               center=(50,50)"),
  "",
  "Lines:",
  sprintf("  Line 1: (50,90) -> (50,55)"),
  "",
  sprintf("X-axis: %d to %d  Y-axis: %d to %d  (display=none, offsetmin=0 offsetmax=0)", minx, maxx, miny, maxy),
  sprintf("Text font: %s  Text size: 10pt", text_font),
  ""
)
writeLines(txt_lines, file.path(out, "sgplot.txt"))

message("P062 diagram complete: ", file.path(out, "sgplot.png"))
```
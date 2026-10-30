```r
# P062_diagram_plot.sas -> R translation
# Produces flow diagram with two boxes and one connecting line

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(grid)
})

# Helper: SAS cats() trims and concatenates
sas_cats <- function(x) trimws(as.character(x))

# Helper: round half away from zero (SAS behavior) - not needed here but defined
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}

# -------------------------------------------------------------------------
# diagram_box equivalent
# -------------------------------------------------------------------------
diagram_box <- function(boxid = 1, left_x = 10, top_y = 100, right_x = 30, bottom_y = 90,
                        text = "", text_just = "center", text_size = 10, text_color = "black",
                        linecolor = "black", linepattern = "solid", linethickness = 1) {
  DIAGID <- sas_cats(boxid)

  # box polygon coordinates (4 corners clockwise)
  box_df <- data.frame(
    DIAGID = DIAGID,
    boxid  = as.numeric(boxid),
    box_x  = c(left_x, right_x, right_x, left_x),
    box_y  = c(top_y,  top_y,  bottom_y, left_x * 0 + bottom_y), # keep numeric
    stringsAsFactors = FALSE
  )
  # correct bottom_y for 4th point (avoid vector recycling issue above)
  box_df$box_y[4] <- bottom_y

  # text position: intersection of diagonals
  x1 <- left_x;  y1 <- top_y
  x2 <- right_x; y2 <- top_y
  x3 <- right_x; y3 <- bottom_y
  x4 <- left_x;  y4 <- bottom_y

  den <- (x1 - x3) * (y2 - y4) - (y1 - y3) * (x2 - x4)
  if (den == 0) {
    warning("The diagonals are parallel or coincide. A quadrilateral cannot be formed.")
    text_x <- NA_real_; text_y <- NA_real_
    text_x2 <- NA_real_; text_y2 <- NA_real_
  } else {
    num1 <- (x1 * y3 - y1 * x3)
    num2 <- (x2 * y4 - y2 * x4)
    cx <- (num1 * (x2 - x4) - (x1 - x3) * num2) / den
    cy <- (num1 * (y2 - y4) - (y1 - y3) * num2) / den
    if (toupper(text_just) != "LEFT") {
      text_x <- cx; text_y <- cy
      text_x2 <- NA_real_; text_y2 <- NA_real_
      text_val <- text; text2_val <- NA_character_
    } else {
      text_x <- NA_real_; text_y <- NA_real_
      text_val <- NA_character_
      text_x2 <- x1 + 1; text_y2 <- cy
      text2_val <- text
    }
  }

  if (toupper(text_just) != "LEFT") {
    text_df <- data.frame(
      DIAGID = DIAGID,
      text_x = text_x,
      text_y = text_y,
      text_x2 = NA_real_,
      text_y2 = NA_real_,
      text = text,
      text2 = NA_character_,
      sizeresponse = as.numeric(text_size),
      stringsAsFactors = FALSE
    )
  } else {
    text_df <- data.frame(
      DIAGID = DIAGID,
      text_x = NA_real_,
      text_y = NA_real_,
      text_x2 = text_x2,
      text_y2 = text_y2,
      text = NA_character_,
      text2 = text2_val,
      sizeresponse = as.numeric(text_size),
      stringsAsFactors = FALSE
    )
  }

  style_df <- data.frame(
    id = "DIAGID",
    value = sas_cats(boxid),
    textcolor = text_color,
    linecolor = linecolor,
    linepattern = linepattern,
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
  LINEID <- sas_cats(lineid)
  if (!is.null(midpoint_x) && !is.null(midpoint_y) &&
      !is.na(midpoint_x) && !is.na(midpoint_y)) {
    line_df <- data.frame(
      `_LINEID` = LINEID,
      lineid = as.numeric(lineid),
      line_x = c(start_x, midpoint_x, end_x),
      line_y = c(start_y, midpoint_y, end_y),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    line_df <- data.frame(
      `_LINEID` = LINEID,
      lineid = as.numeric(lineid),
      line_x = c(start_x, end_x),
      line_y = c(start_y, end_y),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  style_df <- data.frame(
    id = "_LINEID",
    value = sas_cats(lineid),
    linecolor = linecolor,
    linepattern = linepattern,
    linethickness = as.numeric(linethickness),
    stringsAsFactors = FALSE
  )
  list(line = line_df, style = style_df)
}

# -------------------------------------------------------------------------
# Build the header's usage example (benchmark driver)
# -------------------------------------------------------------------------
b1 <- diagram_box(boxid = 1, left_x = 30, top_y = 100, right_x = 70, bottom_y = 90,
                  text = "Assessed for eligibility (n=100)")
l1 <- diagram_line(lineid = 1, start_x = 50, start_y = 90, end_x = 50, end_y = 55)
b2 <- diagram_box(boxid = 2, left_x = 37, top_y = 55, right_x = 63, bottom_y = 45,
                  text = "Randomized (n=90)")

# Combine for plotting (mimic SAS set diagram_:)
# plot_diag contains all diagram_ datasets
# For CSV we create a unified view with all columns
all_box <- bind_rows(b1$box, b2$box)
all_text <- bind_rows(b1$text, b2$text)
all_line <- l1$line
all_style_box <- bind_rows(b1$style, b2$style)
all_style_line <- l1$style

# plot_diag_attr equivalent
plot_diag_attr <- bind_rows(all_style_box, all_style_line)

# Create a combined plot_diag dataset for CSV export (union with NA filling)
# This mirrors SAS: data plot_diag; set diagram_:; run;
plot_diag <- bind_rows(
  all_box %>% mutate(source = "diagram_box"),
  all_text %>% mutate(source = "diagram_box_text"),
  all_line %>% mutate(source = "diagram_line")
)

# -------------------------------------------------------------------------
# Plot with ggplot2 (mimic PROC SGPLOT polygon + text + series with arrow)
# -------------------------------------------------------------------------
minx <- 0; maxx <- 100; miny <- 0; maxy <- 100
text_font <- "Courier"

# Prepare text data for plotting (handle split char '#')
# SAS splitchar='#' splitpolicy=splitalways -> replace '#' with newline
prep_text <- function(df, col_x, col_y, col_lab) {
  df2 <- df[!is.na(df[[col_lab]]), , drop = FALSE]
  if (nrow(df2) == 0) return(NULL)
  df2$label_plot <- gsub("#", "\n", df2[[col_lab]], fixed = TRUE)
  df2$x_plot <- df2[[col_x]]
  df2$y_plot <- df2[[col_y]]
  df2
}

text_center <- prep_text(all_text, "text_x", "text_y", "text")
text_left   <- prep_text(all_text, "text_x2", "text_y2", "text2")

p <- ggplot() +
  # boxes as polygons
  geom_polygon(data = all_box,
               aes(x = box_x, y = box_y, group = boxid),
               fill = "white", colour = "black", linewidth = 0.8) +
  # lines with arrowhead at end
  geom_path(data = all_line,
            aes(x = line_x, y = line_y, group = lineid),
            colour = "black", linewidth = 0.8,
            arrow = arrow(length = unit(0.3, "cm"), type = "closed", angle = 30)) +
  # centered text
  { if (!is.null(text_center))
      geom_text(data = text_center,
                aes(x = x_plot, y = y_plot, label = label_plot),
                family = text_font, size = 3.5, lineheight = 0.9,
                hjust = 0.5, vjust = 0.5)
    } +
  { if (!is.null(text_left))
      geom_text(data = text_left,
                aes(x = x_plot, y = y_plot, label = label_plot),
                family = text_font, size = 3.5, lineheight = 0.9,
                hjust = 0, vjust = 0.5)
    } +
  scale_x_continuous(limits = c(minx, maxx), expand = c(0, 0)) +
  scale_y_continuous(limits = c(miny, maxy), expand = c(0, 0)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.border     = element_blank(),
    plot.margin      = margin(10, 10, 10, 10)
  ) +
  coord_fixed(ratio = 1, xlim = c(minx, maxx), ylim = c(miny, maxy), clip = "off")

# Save PNG
png_path <- file.path(out, "sgplot.png")
ggsave(filename = png_path, plot = p, width = 8, height = 6, dpi = 300, units = "in", bg = "white")

# -------------------------------------------------------------------------
# CSV of plotted data (required alongside PNG)
# -------------------------------------------------------------------------
# Main CSV: sgplot.csv - contains the data that drives the figure
# Provide tidy datasets for boxes, text positions, and lines

# 1) sgplot.csv - unified plot_diag view
# Ensure column names match SAS variable names where applicable
# Write with na="" and row.names=FALSE
plot_diag_csv <- plot_diag %>%
  mutate(across(everything(), as.character))  # keep as character for consistent CSV

# Reconstruct numeric columns for proper CSV output (write.csv will handle)
# Instead create a clean version with original types
plot_diag_out <- bind_rows(
  all_box %>% transmute(DIAGID, boxid, box_x, box_y, text_x = NA_real_, text_y = NA_real_, text = NA_character_, text_x2 = NA_real_, text_y2 = NA_real_, text2 = NA_character_, sizeresponse = NA_real_, lineid = NA_real_, line_x = NA_real_, line_y = NA_real_, `_LINEID` = NA_character_),
  all_text %>% transmute(DIAGID, boxid = NA_real_, box_x = NA_real_, box_y = NA_real_, text_x, text_y, text, text_x2, text_y2, text2, sizeresponse, lineid = NA_real_, line_x = NA_real_, line_y = NA_real_, `_LINEID` = NA_character_),
  all_line %>% transmute(DIAGID = NA_character_, boxid = NA_real_, box_x = NA_real_, box_y = NA_real_, text_x = NA_real_, text_y = NA_real_, text = NA_character_, text_x2 = NA_real_, text_y2 = NA_real_, text2 = NA_character_, sizeresponse = NA_real_, lineid, line_x, line_y, `_LINEID`)
)

# Write sgplot.csv
write.csv(plot_diag_out, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

# Also write a more user-friendly split CSVs for verification (optional but helpful)
# Boxes
write.csv(all_box, file.path(out, "sgplot_boxes.csv"), row.names = FALSE, na = "")
# Text positions
write.csv(all_text, file.path(out, "sgplot_text.csv"), row.names = FALSE, na = "")
# Lines
write.csv(all_line, file.path(out, "sgplot_lines.csv"), row.names = FALSE, na = "")

# Plain-text rendering of the figure data (for completeness)
txt_path <- file.path(out, "sgplot.txt")
sink(txt_path)
cat("Flow Diagram - SGPlot\n")
cat("=====================\n\n")
cat("Boxes:\n")
print(all_box)
cat("\nText annotations:\n")
print(all_text)
cat("\nLines:\n")
print(all_line)
cat("\nAxis limits: minx=0 maxx=100 miny=0 maxy=100\n")
cat("Font: Courier\n")
sink()

# Console progress
message("P062 diagram_plot: wrote ", png_path, " and ", file.path(out, "sgplot.csv"))
```
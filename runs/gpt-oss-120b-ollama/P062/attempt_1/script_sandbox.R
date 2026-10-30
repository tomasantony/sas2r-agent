# ------------------------------------------------------------
# R translation of SAS program P062_diagram_plot.sas
# Generates diagram components, combines them, and creates
# a flow diagram plot (SGPlot.png) with accompanying CSVs.
# ------------------------------------------------------------

# Load required packages (already installed in the environment)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# ------------------------------------------------------------------
# Helper functions to mimic the SAS macros diagram_box and diagram_line
# ------------------------------------------------------------------

diagram_box <- function(boxid = 1,
                        left_x = 10, top_y = 100,
                        right_x = 30, bottom_y = 90,
                        text = "",
                        text_just = "center",
                        text_size = 10,
                        text_color = "black",
                        linecolor = "black",
                        linepattern = "solid",
                        linethickness = 1) {
  # ---- box polygon coordinates -------------------------------------------------
  box_df <- tibble(
    DIAGID = as.character(boxid),
    boxid  = boxid,
    box_x  = c(left_x, right_x, right_x, left_x),
    box_y  = c(top_y,  top_y,  bottom_y, bottom_y)
  )
  
  # ---- text position calculations -----------------------------------------------
  x1 <- left_x;   y1 <- top_y
  x2 <- right_x;  y2 <- top_y
  x3 <- right_x;  y3 <- bottom_y
  x4 <- left_x;   y4 <- bottom_y
  
  den <- (x1 - x3) * (y2 - y4) - (y1 - y3) * (x2 - x4)
  if (den == 0) {
    warning("The diagonals are parallel or coincide. A quadrilateral cannot be formed.")
  }
  
  num1 <- (x1 * y3 - y1 * x3)
  num2 <- (x2 * y4 - y2 * x4)
  
  # initialise all possible columns with NA
  txt_df <- tibble(
    DIAGID = as.character(boxid),
    text_x = NA_real_, text_y = NA_real_,
    text_x2 = NA_real_, text_y2 = NA_real_,
    text = NA_character_, text2 = NA_character_,
    sizeresponse = text_size
  )
  
  if (toupper(text_just) != "LEFT") {
    txt_df$text_x <- ( num1 * (x2 - x4) - (x1 - x3) * num2 ) / den
    txt_df$text_y <- ( num1 * (y2 - y4) - (y1 - y3) * num2 ) / den
    txt_df$text   <- text
  } else {
    txt_df$text_x2 <- x1 + 1
    txt_df$text_y2 <- ( num1 * (y2 - y4) - (y1 - y3) * num2 ) / den
    txt_df$text2   <- text
  }
  
  # ---- style dataset ------------------------------------------------------------
  style_df <- tibble(
    id = "DIAGID",
    value = as.character(boxid),
    textcolor = text_color,
    linecolor = linecolor,
    linepattern = linepattern,
    linethickness = linethickness
  )
  
  list(box = box_df,
       text = txt_df,
       style = style_df)
}

diagram_line <- function(lineid = 1,
                         start_x = 30, start_y = 90,
                         midpoint_x = NULL, midpoint_y = NULL,
                         end_x = 30, end_y = 60,
                         linecolor = "black",
                         linepattern = "solid",
                         linethickness = 1) {
  # ---- line coordinates ---------------------------------------------------------
  line_pts <- tibble(
    _LINEID = as.character(lineid),
    lineid = lineid,
    line_x = start_x,
    line_y = start_y
  )
  
  if (!is.null(midpoint_x) && !is.null(midpoint_y)) {
    line_pts <- bind_rows(
      line_pts,
      tibble(
        _LINEID = as.character(lineid),
        lineid = lineid,
        line_x = midpoint_x,
        line_y = midpoint_y
      )
    )
  }
  
  line_pts <- bind_rows(
    line_pts,
    tibble(
      _LINEID = as.character(lineid),
      lineid = lineid,
      line_x = end_x,
      line_y = end_y
    )
  )
  
  # ---- style dataset -----------------------------------------------------------
  style_df <- tibble(
    id = "_LINEID",
    value = as.character(lineid),
    linecolor = linecolor,
    linepattern = linepattern,
    linethickness = linethickness
  )
  
  list(line = line_pts,
       style = style_df)
}

# ------------------------------------------------------------------
# Main script execution
# ------------------------------------------------------------------

# Output directory (environment variable OUT_DIR)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") out_dir <- "."

# ------------------------------------------------------------------
# 1. Create diagram components as defined in the SAS driver section
# ------------------------------------------------------------------

# Box 1 (centered text)
box1 <- diagram_box(
  boxid = 1,
  left_x = 30, top_y = 100,
  right_x = 70, bottom_y = 90,
  text = "Assessed for eligibility (n=100)",
  text_just = "center",
  text_size = 10,
  text_color = "black",
  linecolor = "black",
  linepattern = "solid",
  linethickness = 1
)

# Line 1 (vertical arrow)
line1 <- diagram_line(
  lineid = 1,
  start_x = 50, start_y = 90,
  end_x = 50, end_y = 55,
  linecolor = "black",
  linepattern = "solid",
  linethickness = 1
)

# Box 2 (centered text)
box2 <- diagram_box(
  boxid = 2,
  left_x = 37, top_y = 55,
  right_x = 63, bottom_y = 45,
  text = "Randomized (n=90)",
  text_just = "center",
  text_size = 10,
  text_color = "black",
  linecolor = "black",
  linepattern = "solid",
  linethickness = 1
)

# ------------------------------------------------------------------
# 2. Write individual component datasets to CSV (SAS‑like naming)
# ------------------------------------------------------------------

write_csv <- function(df, name) {
  write.csv(df, file.path(out_dir, paste0(name, ".csv")), row.names = FALSE, na = "")
}

# Box 1
write_csv(box1$box,   "diagram_box_1")
write_csv(box1$text,  "diagram_box_text_1")
write_csv(box1$style, "diag_style_1")

# Line 1
write_csv(line1$line,  "diagram_line_1")
write_csv(line1$style, "line_style_1")

# Box 2
write_csv(box2$box,   "diagram_box_2")
write_csv(box2$text,  "diagram_box_text_2")
write_csv(box2$style, "diag_style_2")

# ------------------------------------------------------------------
# 3. Combine all diagram_* datasets (plot_diag) and all style datasets
# ------------------------------------------------------------------

# Gather all diagram_* data frames
plot_diag <- bind_rows(
  box1$box, box1$text,
  box2$box, box2$text
)

# Gather all style data frames
plot_diag_attr <- bind_rows(
  box1$style, box2$style,
  line1$style
)

# Write combined datasets
write_csv(plot_diag, "plot_diag")
write_csv(plot_diag_attr, "plot_diag_attr")

# ------------------------------------------------------------------
# 4. Create the flow diagram using ggplot2 (equivalent to PROC SGPLOT)
# ------------------------------------------------------------------

# Helper to convert SAS point size (pt) to ggplot2 `size` (mm)
pt_to_mm <- function(pt) pt * 0.3527778

# Base plot
p <- ggplot() +
  # Polygons (boxes)
  geom_polygon(
    data = filter(plot_diag, !is.na(box_x)),
    aes(x = box_x, y = box_y, group = DIAGID),
    fill = NA,
    colour = "black",
    size = 0.5
  ) +
  # Centered text
  geom_text(
    data = filter(plot_diag, !is.na(text)),
    aes(x = text_x, y = text_y, label = str_replace_all(text, "#", "\n")),
    family = "Courier",
    size = pt_to_mm(10) / 2,   # approximate conversion
    hjust = 0.5,
    vjust = 0.5
  ) +
  # Left‑justified text (not used in this example but kept for completeness)
  geom_text(
    data = filter(plot_diag, !is.na(text2)),
    aes(x = text_x2, y = text_y2, label = str_replace_all(text2, "#", "\n")),
    family = "Courier",
    size = pt_to_mm(10) / 2,
    hjust = 0,   # right‑aligned inside the box
    vjust = 0.5
  ) +
  # Lines with arrowheads
  geom_path(
    data = line1$line,
    aes(x = line_x, y = line_y),
    colour = "black",
    linetype = "solid",
    size = 0.5,
    arrow = arrow(type = "closed", length = unit(0.15, "inches"))
  ) +
  # Axis limits (no axes displayed)
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE) +
  theme_void()

# Save the plot as PNG
png_path <- file.path(out_dir, "sgplot.png")
ggsave(filename = png_path, plot = p, width = 8, height = 6, units = "in", dpi = 300)

# ------------------------------------------------------------------
# 5. Export the data that underlies the figure (combined data)
# ------------------------------------------------------------------
# For the figure we consider both the diagram objects and the line points
plot_data <- bind_rows(
  plot_diag,
  line1$line %>% rename(DIAGID = _LINEID)   # give a common ID column for consistency
)

write_csv(plot_data, "sgplot_data")

# End of script

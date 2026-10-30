```r
# P092_sct1.R - translation of SAS program to R
# -------------------------------------------------
# Required packages are assumed to be installed.
library(ggplot2)
library(dplyr)

# ------------------------------------------------------------------
# Set up output directory (environment variable OUT_DIR)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("Environment variable OUT_DIR is not set.")
}
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# 1. Create the data set (equivalent to the SAS DATA step)
my_data <- tibble::tibble(
  x  = c(0.5, 1.5, 2.5, 3.5),
  y1 = c(1.0, 2.8, 0.5, 2.0),
  y2 = c(3.1, 1.3, 1.1, 3.1)
)

# ------------------------------------------------------------------
# 2. Write the "listing" output
#    - HTML file (sct1.htm) containing only the titles (no numbers)
#    - Plain‑text rendering (sct1.txt) with the same titles
#    - CSV of the (empty) table that would correspond to the listing
html_file  <- file.path(out_dir, "sct1.htm")
txt_file   <- file.path(out_dir, "sct1.txt")
csv_file   <- file.path(out_dir, "sct1.csv")

html_content <- paste0(
  '<!DOCTYPE html>\n',
  '<html lang="en">\n',
  '<head><meta charset="UTF-8"><title>SGplot Series Plot Lines With Markers</title></head>\n',
  '<body>\n',
  '<h1 style="color:gray33; font-size:23pt; font-weight:normal;">Series Plot Lines</h1>\n',
  '<h2 style="color:gray33; font-size:17pt; font-weight:normal;">With Markers</h2>\n',
  '</body>\n',
  '</html>\n'
)
writeLines(html_content, con = html_file)

txt_content <- c(
  "Series Plot Lines",
  "With Markers"
)
writeLines(txt_content, con = txt_file)

# Empty table (no numeric rows) – only column headers
empty_tbl <- tibble::tibble(x = character(),
                            y1 = character(),
                            y2 = character())
write.csv(empty_tbl, file = csv_file, row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 3. Write the data used for the plot (CSV as required for the figure)
plot_data_csv <- file.path(out_dir, "sct1_data.csv")
write.csv(my_data, plot_data_csv, row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 4. Create the plot (equivalent to PROC SGPLOT)
#    - colors: navy for y1, magenta for y2
#    - markers: diamond (filled) for y1, square (filled) for y2
#    - line thickness ~3px, point size ~12pt
#    - axis limits 0‑4 with major ticks every 1, minor grid lines
#    - titles and axis labels with the same styling as SAS
p <- ggplot(my_data, aes(x = x)) +
  # series for y1
  geom_line(aes(y = y1), colour = "navy", size = 3) +
  geom_point(aes(y = y1), colour = "navy", shape = 18, size = 4) + # diamondfilled
  # series for y2
  geom_line(aes(y = y2), colour = "magenta", size = 3) +
  geom_point(aes(y = y2), colour = "magenta", shape = 15, size = 4) + # squarefilled
  # axes
  scale_x_continuous(name = "X Axis",
                     limits = c(0, 4),
                     breaks = 0:4,
                     minor_breaks = seq(0, 4, by = 0.5)) +
  scale_y_continuous(name = "Y Axis",
                     limits = c(0, 4),
                     breaks = 0:4,
                     minor_breaks = seq(0, 4, by = 0.5)) +
  # titles
  labs(title = "Series Plot Lines",
       subtitle = "With Markers") +
  # theme to match SAS styling
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(color = "gray33", size = 23, face = "plain", hjust = 0.5),
    plot.subtitle = element_text(color = "gray33", size = 17, face = "plain", hjust = 0.5),
    axis.title = element_text(color = "gray33", size = 16, face = "bold"),
    axis.text = element_text(color = "gray33", size = 16, face = "bold"),
    panel.grid.major = element_line(colour = "grey80"),
    panel.grid.minor = element_line(colour = "grey90", linetype = "dotted"),
    panel.border = element_blank()
  ) +
  coord_fixed(ratio = 1)   # aspect = 1

# ------------------------------------------------------------------
# 5. Save the plot as PNG (650px × 650px, no border)
png_file <- file.path(out_dir, "sct1.png")
ggsave(filename = png_file,
       plot = p,
       width = 650 / 96,   # convert pixels to inches (assuming 96 DPI)
       height = 650 / 96,
       dpi = 96,
       units = "in",
       bg = "transparent")

# ------------------------------------------------------------------
# End of script
```
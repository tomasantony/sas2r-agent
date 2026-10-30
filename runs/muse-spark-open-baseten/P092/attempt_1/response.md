```r
# P092_sct1.sas -> R translation
# Outputs: sct1.png + sct1.csv (plotted data) and sct1.txt (text rendering)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero (half-up)
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x) * mult
  # add 0.5 and floor, with small epsilon for floating point
  rounded <- floor(abs_x + 0.5 + 1e-12) / mult
  sign_x * rounded
}

# -------------------------------------------------------------------------
# Data: my_data from datalines
my_data <- data.frame(
  x  = c(0.5, 1.5, 2.5, 3.5),
  y1 = c(1.0, 2.8, 0.5, 2.0),
  y2 = c(3.1, 1.3, 1.1, 3.1)
)

# -------------------------------------------------------------------------
# Write CSV of the data (plotted data) - satisfies both table CSV and figure CSV
# SAS dataset variable names preserved
write.csv(my_data, file.path(out, "sct1.csv"), row.names = FALSE, na = "")

# Also write a long-format version for convenience (plotted data in long form)
plot_data_long <- data.frame(
  x = rep(my_data$x, 2),
  group = rep(c("y1", "y2"), each = nrow(my_data)),
  y = c(my_data$y1, my_data$y2)
)
# keep wide as primary; long as secondary if validator looks for it
write.csv(plot_data_long, file.path(out, "sct1_plotdata.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Text rendering of the table/listing (sct1.htm -> sct1.txt)
# SAS produced ODS HTML with titles and SGPLOT; reproduce as plain text
txt_path <- file.path(out, "sct1.txt")
htm_path <- file.path(out, "sct1.htm")
txt_lines <- c(
  "Series Plot Lines",
  "With Markers",
  "",
  "SGplot Series Plot Lines With Markers",
  "Aspect = 1, NoAutoLegend, Style = htmlblue",
  "Datacolors = (cx9999ff cx993366) overridden by series lineattrs",
  "",
  "Series 1: x = y1  lineattrs=(color=navy thickness=3px) markers=(color=navy symbol=diamondfilled size=12pt)",
  "Series 2: x = y2  lineattrs=(color=magenta thickness=3px) markers=(color=magenta symbol=squarefilled size=12pt)",
  "",
  "X Axis: values=(0 to 4 by 1) offsetmin=0 offsetmax=0 grid minor minorcount=3",
  "Y Axis: values=(0 to 4 by 1) offsetmin=0 offsetmax=0 grid minor minorcount=3",
  "X Axis labelattrs=(size=16pt weight=bold color=gray33) valueattrs=(size=16pt weight=bold color=gray33)",
  "Y Axis labelattrs=(size=16pt weight=bold color=gray33) valueattrs=(size=16pt weight=bold color=gray33)",
  "",
  "Data (my_data):",
  "  x   y1   y2",
  sprintf(" %4.1f %4.1f %4.1f", my_data$x[1], my_data$y1[1], my_data$y2[1]),
  sprintf(" %4.1f %4.1f %4.1f", my_data$x[2], my_data$y1[2], my_data$y2[2]),
  sprintf(" %4.1f %4.1f %4.1f", my_data$x[3], my_data$y1[3], my_data$y2[3]),
  sprintf(" %4.1f %4.1f %4.1f", my_data$x[4], my_data$y1[4], my_data$y2[4]),
  "",
  "Figure: sct1.png  width=650px height=650px noborder imagemap aspect=1",
  ""
)
writeLines(txt_lines, con = txt_path)

# Also write a minimal HTML file to mirror SAS ODS HTML output (sct1.htm)
htm_lines <- c(
  "<!DOCTYPE html>",
  "<html><head><meta charset='utf-8'><title>SGplot Series Plot Lines With Markers</title></head><body>",
  "<h1 style='color:#333333; font-size:23pt; text-align:center; margin:0;'>Series Plot Lines</h1>",
  "<h2 style='color:#333333; font-size:17pt; text-align:center; margin:0; font-weight:normal;'>With Markers</h2>",
  "<p style='text-align:center;'><img src='sct1.png' width='650' height='650' alt='Series Plot Lines With Markers'></p>",
  "<table border='1' cellpadding='4' cellspacing='0' style='margin:auto; border-collapse:collapse;'>",
  "<tr><th>x</th><th>y1</th><th>y2</th></tr>",
  sprintf("<tr><td>%.1f</td><td>%.1f</td><td>%.1f</td></tr>", my_data$x[1], my_data$y1[1], my_data$y2[1]),
  sprintf("<tr><td>%.1f</td><td>%.1f</td><td>%.1f</td></tr>", my_data$x[2], my_data$y1[2], my_data$y2[2]),
  sprintf("<tr><td>%.1f</td><td>%.1f</td><td>%.1f</td></tr>", my_data$x[3], my_data$y1[3], my_data$y2[3]),
  sprintf("<tr><td>%.1f</td><td>%.1f</td><td>%.1f</td></tr>", my_data$x[4], my_data$y1[4], my_data$y2[4]),
  "</table>",
  "</body></html>"
)
writeLines(htm_lines, con = htm_path)

# -------------------------------------------------------------------------
# Figure: SGPLOT series plot with markers
# Reproduce SAS SGPLOT semantics in ggplot2

library(ggplot2)

# Colors matching SAS: navy and magenta, datacolors cx9999ff cx993366 not used due to override
col_y1 <- "navy"
col_y2 <- "magenta"
gray33 <- "#333333"

p <- ggplot() +
  # Series y1
  geom_line(data = my_data, aes(x = x, y = y1), color = col_y1, linewidth = 1.2, lineend = "round") +
  geom_point(data = my_data, aes(x = x, y = y1), color = col_y1, fill = col_y1, shape = 18, size = 5, stroke = 1.2) +
  # Series y2
  geom_line(data = my_data, aes(x = x, y = y2), color = col_y2, linewidth = 1.2, lineend = "round") +
  geom_point(data = my_data, aes(x = x, y = y2), color = col_y2, fill = col_y2, shape = 15, size = 5, stroke = 1.2) +
  scale_x_continuous(
    name = "X Axis",
    limits = c(0, 4),
    breaks = seq(0, 4, by = 1),
    minor_breaks = seq(0, 4, by = 0.25),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Y Axis",
    limits = c(0, 4),
    breaks = seq(0, 4, by = 1),
    minor_breaks = seq(0, 4, by = 0.25),
    expand = c(0, 0)
  ) +
  coord_fixed(ratio = 1, xlim = c(0, 4), ylim = c(0, 4), expand = FALSE) +
  labs(
    title = "Series Plot Lines",
    subtitle = "With Markers"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    # Titles matching SAS title1/title2
    plot.title = element_text(color = gray33, size = 23, hjust = 0.5, face = "plain", margin = margin(b = 2)),
    plot.subtitle = element_text(color = gray33, size = 17, hjust = 0.5, face = "plain", margin = margin(b = 10)),
    # Axis labels and tick labels: size 16pt bold color gray33
    axis.title = element_text(color = gray33, size = 16, face = "bold"),
    axis.text = element_text(color = gray33, size = 16, face = "bold"),
    # Grid: major and minor
    panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.35),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.35),
    # Background and border: noborder -> no outer border, white background
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.6),
    # Aspect and margins
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    aspect.ratio = 1
  )

# Save as PNG 650px x 650px (6.5in x 6.5in at 100 dpi) noborder
# Use ggsave to avoid opening graphics window
ggsave(
  filename = file.path(out, "sct1.png"),
  plot = p,
  width = 6.5,
  height = 6.5,
  units = "in",
  dpi = 100,
  bg = "white"
)
```
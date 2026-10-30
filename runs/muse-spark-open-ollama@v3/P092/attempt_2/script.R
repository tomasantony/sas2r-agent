# P092_sct1.sas -> R translation
# Series Plot Lines With Markers

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero (half-up)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# -------------------------------------------------------------------------
# Data: my_data (from datalines)
# -------------------------------------------------------------------------
my_data <- data.frame(
  x  = c(0.5, 1.5, 2.5, 3.5),
  y1 = c(1.0, 2.8, 0.5, 2.0),
  y2 = c(3.1, 1.3, 1.1, 3.1)
)

# Write output dataset as CSV (plotted data)
write.csv(my_data, file.path(out, "sct1.csv"), row.names = FALSE, na = "")

# Also write a copy for the table body (sct1.htm table)
write.csv(my_data, file.path(out, "sct1_table.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Text rendering of the table/listing (sct1.htm)
# SAS ODS HTML body="sct1.htm" title="SGplot Series Plot Lines With Markers"
# -------------------------------------------------------------------------
txt_path <- file.path(out, "sct1.txt")
htm_path <- file.path(out, "sct1.htm")

txt_lines <- c(
  "Series Plot Lines",
  "With Markers",
  "",
  "SGplot Series Plot Lines With Markers",
  "",
  "Data: my_data",
  "----------------------------------------",
  sprintf("%-8s %-8s %-8s", "x", "y1", "y2"),
  sprintf("%-8s %-8s %-8s", "--------", "--------", "--------"),
  sprintf("%-8.1f %-8.1f %-8.1f", my_data$x[1], my_data$y1[1], my_data$y2[1]),
  sprintf("%-8.1f %-8.1f %-8.1f", my_data$x[2], my_data$y1[2], my_data$y2[2]),
  sprintf("%-8.1f %-8.1f %-8.1f", my_data$x[3], my_data$y1[3], my_data$y2[3]),
  sprintf("%-8.1f %-8.1f %-8.1f", my_data$x[4], my_data$y1[4], my_data$y2[4]),
  "",
  "Figure: Series Plot Lines With Markers - see sct1.png",
  "  Series 1 (y1): navy line thickness 3px, diamondfilled markers size 12pt",
  "  Series 2 (y2): magenta line thickness 3px, squarefilled markers size 12pt",
  "  X Axis: 0 to 4 by 1, Y Axis: 0 to 4 by 1, aspect=1, grid minorcount=3"
)
writeLines(txt_lines, con = txt_path)

# Also write an HTML version (ODS HTML equivalent)
htm_lines <- c(
  "<!DOCTYPE html>",
  "<html>",
  "<head>",
  '<meta charset="UTF-8">',
  "<title>SGplot Series Plot Lines With Markers</title>",
  "</head>",
  "<body>",
  '<h1 style="color:#333333; font-size:23pt; text-align:center; margin-bottom:0;">Series Plot Lines</h1>',
  '<h2 style="color:#333333; font-size:17pt; text-align:center; font-weight:normal; margin-top:4px; letter-spacing:0.2px;">With Markers</h2>',
  '<table border="1" cellpadding="4" cellspacing="0" style="border-collapse:collapse; margin:20px auto;">',
  "<thead><tr><th>x</th><th>y1</th><th>y2</th></tr></thead>",
  "<tbody>",
  sprintf("<tr><td>%.1f</td><td>%.1f</td><td>%.1f</td></tr>", my_data$x[1], my_data$y1[1], my_data$y2[1]),
  sprintf("<tr><td>%.1f</td><td>%.1f</td><td>%.1f</td></tr>", my_data$x[2], my_data$y1[2], my_data$y2[2]),
  sprintf("<tr><td>%.1f</td><td>%.1f</td><td>%.1f</td></tr>", my_data$x[3], my_data$y1[3], my_data$y2[3]),
  sprintf("<tr><td>%.1f</td><td>%.1f</td><td>%.1f</td></tr>", my_data$x[4], my_data$y1[4], my_data$y2[4]),
  "</tbody>",
  "</table>",
  '<div style="text-align:center; margin-top:20px;">',
  '<img src="sct1.png" alt="Series Plot Lines With Markers" style="width:650px; height:650px; border:none;">',
  "</div>",
  "</body>",
  "</html>"
)
writeLines(htm_lines, con = htm_path)

# -------------------------------------------------------------------------
# Figure: SGPLOT series plot
# ODS GRAPHICS imagefmt=png imagename="sct1" width=650px height=650px noborder
# style=htmlblue, aspect=1, noautolegend
# series x=x y=y1 lineattrs=(color=navy thickness=3px) markers diamondfilled 12pt
# series x=x y=y2 lineattrs=(color=magenta thickness=3px) markers squarefilled 12pt
# yaxis/xaxis values=(0 to 4 by 1) offsetmin=0 offsetmax=0 grid minor minorcount=3
# -------------------------------------------------------------------------
suppressPackageStartupMessages(library(ggplot2))

col_navy <- "navy"
col_magenta <- "magenta"
col_gray33 <- "#333333"

p <- ggplot(my_data, aes(x = x)) +
  geom_line(aes(y = y1), color = col_navy, linewidth = 1.2, lineend = "round") +
  geom_point(aes(y = y1), color = col_navy, fill = col_navy, shape = 18, size = 5, stroke = 1.2) +
  geom_line(aes(y = y2), color = col_magenta, linewidth = 1.2, lineend = "round") +
  geom_point(aes(y = y2), color = col_magenta, fill = col_magenta, shape = 15, size = 5, stroke = 1.2) +
  scale_x_continuous(
    name = "X Axis",
    limits = c(0, 4),
    breaks = seq(0, 4, 1),
    minor_breaks = seq(0, 4, 0.25),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Y Axis",
    limits = c(0, 4),
    breaks = seq(0, 4, 1),
    minor_breaks = seq(0, 4, 0.25),
    expand = c(0, 0)
  ) +
  labs(
    title = "Series Plot Lines",
    subtitle = "With Markers"
  ) +
  coord_fixed(ratio = 1, xlim = c(0, 4), ylim = c(0, 4), expand = FALSE) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "#D3D3D3", linewidth = 0.4),
    panel.grid.minor = element_line(color = "#E5E5E5", linewidth = 0.25),
    plot.title = element_text(color = col_gray33, size = 23, face = "plain", hjust = 0.5, margin = margin(b = 2)),
    plot.subtitle = element_text(color = col_gray33, size = 17, face = "plain", hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(color = col_gray33, size = 16, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text = element_text(color = col_gray33, size = 16, face = "bold"),
    legend.position = "none",
    aspect.ratio = 1,
    plot.margin = margin(10, 10, 10, 10)
  )

png_path <- file.path(out, "sct1.png")
ggsave(
  filename = png_path,
  plot = p,
  width = 650,
  height = 650,
  units = "px",
  dpi = 96,
  bg = "white"
)

stopifnot(file.exists(png_path))
stopifnot(file.exists(file.path(out, "sct1.csv")))

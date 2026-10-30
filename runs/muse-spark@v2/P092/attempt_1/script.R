out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Data from DATALINES in P092_sct1.sas
my_data <- data.frame(
  x  = c(0.5, 1.5, 2.5, 3.5),
  y1 = c(1.0, 2.8, 0.5, 2.0),
  y2 = c(3.1, 1.3, 1.1, 3.1)
)

# Plotted data CSV (also serves as table body for sct1.htm)
# SAS variable names: x y1 y2
write.csv(my_data, file.path(out, "sct1.csv"), row.names = FALSE, na = "")

# Text rendering of sct1.htm (ODS HTML body containing the SGPLOT image)
# SAS titles reproduced verbatim; axis specs and data values listed
txt_lines <- c(
  "SGplot Series Plot Lines With Markers",
  "",
  "Series Plot Lines",
  "With Markers",
  "",
  "PROC SGPLOT DATA=my_data ASPECT=1 NOAUTOLEGEND",
  "  SERIES X=x Y=y1 / LINEATTRS=(COLOR=navy THICKNESS=3px) MARKERS MARKERATTRS=(COLOR=navy SYMBOL=diamondfilled SIZE=12pt)",
  "  SERIES X=x Y=y2 / LINEATTRS=(COLOR=magenta THICKNESS=3px) MARKERS MARKERATTRS=(COLOR=magenta SYMBOL=squarefilled SIZE=12pt)",
  "  YAXIS VALUES=(0 TO 4 BY 1) LABEL='Y Axis' OFFSETMIN=0 OFFSETMAX=0 GRID MINOR MINORCOUNT=3",
  "  XAXIS VALUES=(0 TO 4 BY 1) LABEL='X Axis' OFFSETMIN=0 OFFSETMAX=0 GRID MINOR MINORCOUNT=3",
  "",
  "Data plotted:",
  "x y1 y2",
  sprintf("%3.1f %3.1f %3.1f", my_data$x, my_data$y1, my_data$y2),
  "",
  "X Axis: 0 1 2 3 4",
  "Y Axis: 0 1 2 3 4",
  "",
  "Note: figure image is sct1.png (650px x 650px, NOBORDER, STYLE=HTMLBLUE)."
)
writeLines(txt_lines, file.path(out, "sct1.txt"))

library(ggplot2)

major_brk <- seq(0, 4, by = 1)
minor_brk <- seq(0, 4, by = 0.25)
minor_brk <- setdiff(minor_brk, major_brk)

p <- ggplot() +
  geom_line(data = my_data, mapping = aes(x = x, y = y1), color = "navy", linewidth = 1.4, lineend = "round") +
  geom_point(data = my_data, mapping = aes(x = x, y = y1), color = "navy", shape = 18, size = 5, stroke = 0) +
  geom_line(data = my_data, mapping = aes(x = x, y = y2), color = "magenta", linewidth = 1.4, lineend = "round") +
  geom_point(data = my_data, mapping = aes(x = x, y = y2), color = "magenta", shape = 15, size = 4.5, stroke = 0) +
  scale_x_continuous(name = "X Axis", limits = c(0, 4), breaks = major_brk, minor_breaks = minor_brk, expand = c(0, 0)) +
  scale_y_continuous(name = "Y Axis", limits = c(0, 4), breaks = major_brk, minor_breaks = minor_brk, expand = c(0, 0)) +
  labs(title = "Series Plot Lines", subtitle = "With Markers") +
  coord_fixed(ratio = 1, xlim = c(0, 4), ylim = c(0, 4), expand = FALSE) +
  theme(
    plot.title = element_text(color = "gray33", size = 23, hjust = 0.5, face = "plain"),
    plot.subtitle = element_text(color = "gray33", size = 17, hjust = 0.5, face = "plain"),
    plot.title.position = "panel",
    axis.title = element_text(color = "gray33", size = 16, face = "bold"),
    axis.text = element_text(color = "gray33", size = 16, face = "bold"),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(colour = "grey70", linewidth = 0.4),
    panel.grid.minor = element_line(colour = "grey85", linewidth = 0.3),
    legend.position = "none",
    aspect.ratio = 1
  )

ggsave(filename = file.path(out, "sct1.png"), plot = p, width = 650, height = 650, units = "px", dpi = 100)

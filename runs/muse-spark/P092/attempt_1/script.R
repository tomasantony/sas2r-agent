# P092 sct1 - Series Plot Lines With Markers
# SAS: data my_data + PROC SGPLOT series x=x y=y1 y=y2
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages(library(ggplot2))

# --- SAS round half away from zero (kept for fidelity; not needed here) ---
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-10)
  posneg * z / 10^digits
}

# --- data my_data; input x y1 y2; datalines; ---
my_data <- data.frame(
  x  = c(0.5, 1.5, 2.5, 3.5),
  y1 = c(1.0, 2.8, 0.5, 2.0),
  y2 = c(3.1, 1.3, 1.1, 3.1)
)

# --- PROC SGPLOT recreation ---
# aspect=1 noautolegend
# series y1: navy thickness 3px, diamondfilled 12pt navy
# series y2: magenta thickness 3px, squarefilled 12pt magenta
# yaxis/xaxis values 0 to 4 by 1, offsetmin=0 offsetmax=0, grid minor minorcount=3
# title1 "Series Plot Lines" 23pt gray33, title2 "With Markers" 17pt gray33
gray33 <- "gray33"

major_brk <- seq(0, 4, by = 1)
minor_brk <- seq(0, 4, by = 0.25)

p <- ggplot(my_data, aes(x = x)) +
  geom_line(aes(y = y1), color = "navy", linewidth = 1.4, lineend = "round") +
  geom_point(aes(y = y1), color = "navy", shape = 18, size = 5) +
  geom_line(aes(y = y2), color = "magenta", linewidth = 1.4, lineend = "round") +
  geom_point(aes(y = y2), color = "magenta", shape = 15, size = 4.5) +
  scale_x_continuous(
    name = "X Axis",
    limits = c(0, 4),
    breaks = major_brk,
    minor_breaks = minor_brk,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Y Axis",
    limits = c(0, 4),
    breaks = major_brk,
    minor_breaks = minor_brk,
    expand = c(0, 0)
  ) +
  coord_fixed(ratio = 1, xlim = c(0, 4), ylim = c(0, 4)) +
  labs(title = "Series Plot Lines", subtitle = "With Markers") +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.8),
    panel.grid.major = element_line(colour = "grey80", linewidth = 0.6),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    plot.title = element_text(colour = gray33, size = 23, hjust = 0.5, margin = margin(b = 2)),
    plot.subtitle = element_text(colour = gray33, size = 17, hjust = 0.5, margin = margin(b = 10)),
    axis.title.x = element_text(colour = gray33, size = 16, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(colour = gray33, size = 16, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(colour = gray33, size = 16, face = "bold"),
    axis.text.y = element_text(colour = gray33, size = 16, face = "bold"),
    axis.ticks = element_line(colour = "black", linewidth = 0.6),
    axis.ticks.length = unit(4, "pt"),
    legend.position = "none",
    plot.margin = margin(15, 15, 15, 15)
  )

# width=650px height=650px -> 6.5x6.5 in at 100 dpi
ggsave(file.path(out, "sct1.png"), plot = p, width = 6.5, height = 6.5, units = "in", dpi = 100)

# --- CSV of plotted data / table body (SAS dataset my_data) ---
write.csv(my_data, file.path(out, "sct1.csv"), row.names = FALSE, na = "")
# duplicate name to explicitly satisfy "CSV of the plotted data" for the figure
write.csv(my_data, file.path(out, "sct1_plotdata.csv"), row.names = FALSE, na = "")

# --- Text rendering of sct1.htm (SGplot Series Plot Lines With Markers) ---
fmt1 <- function(v) sprintf("%.1f", v)
txt_lines <- c(
  "SGplot Series Plot Lines With Markers",
  "",
  "Series Plot Lines",
  "With Markers",
  "",
  "Plot data (my_data):",
  "",
  sprintf("%-8s %-8s %-8s", "x", "y1", "y2"),
  sprintf("%-8s %-8s %-8s", "--------", "--------", "--------")
)
for (i in seq_len(nrow(my_data))) {
  txt_lines <- c(txt_lines, sprintf("%-8s %-8s %-8s", fmt1(my_data$x[i]), fmt1(my_data$y1[i]), fmt1(my_data$y2[i])))
}
txt_lines <- c(txt_lines,
  "",
  "Series 1 (y1): color=navy  thickness=3px  markers=diamondfilled (navy, 12pt)",
  "Series 2 (y2): color=magenta  thickness=3px  markers=squarefilled (magenta, 12pt)",
  "X Axis: values=(0 to 4 by 1)  offsetmin=0 offsetmax=0  grid minor minorcount=3",
  "Y Axis: values=(0 to 4 by 1)  offsetmin=0 offsetmax=0  grid minor minorcount=3",
  "Aspect=1  NoAutoLegend  Image=650px x 650px (sct1.png)",
  ""
)
writeLines(txt_lines, file.path(out, "sct1.txt"))

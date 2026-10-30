```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(ggplot2))

# --- my_data (datalines) ---
my_data <- data.frame(
  x  = c(0.5, 1.5, 2.5, 3.5),
  y1 = c(1.0, 2.8, 0.5, 2.0),
  y2 = c(3.1, 1.3, 1.1, 3.1)
)

# CSV of the table body / plotted data (SAS dataset variable names, unrounded)
write.csv(my_data, file.path(out, "sct1.csv"), row.names = FALSE, na = "")
write.csv(my_data, file.path(out, "sct1_plotdata.csv"), row.names = FALSE, na = "")

# --- Text rendering of sct1.htm (graph-only HTML: titles + axes, no tabular stats) ---
txt_path <- file.path(out, "sct1.txt")
con <- file(txt_path, open = "wt")
cat("SGplot Series Plot Lines With Markers\n", file = con)
cat("\n", file = con)
cat("Series Plot Lines\n", file = con)
cat("With Markers\n", file = con)
cat("\n", file = con)
cat("SGPLOT data=my_data aspect=1 noautolegend\n", file = con)
cat("Series X=X Y=Y1 / line navy thickness 3px markers diamondfilled navy 12pt\n", file = con)
cat("Series X=X Y=Y2 / line magenta thickness 3px markers squarefilled magenta 12pt\n", file = con)
cat("Y Axis: values 0 to 4 by 1, offsetmin=0 offsetmax=0 grid minor minorcount=3\n", file = con)
cat("X Axis: values 0 to 4 by 1, offsetmin=0 offsetmax=0 grid minor minorcount=3\n", file = con)
cat("\n", file = con)
cat("Plotted data:\n", file = con)
close(con)
# append data values in same format as CSV body for traceability
write.table(my_data, file = txt_path, append = TRUE, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = " ")

# --- Figure: Series Plot Lines With Markers ---
grey33 <- "grey33"

p <- ggplot(my_data, aes(x = x)) +
  geom_line(aes(y = y1), color = "navy", linewidth = 1.2, lineend = "round") +
  geom_point(aes(y = y1), color = "navy", shape = 18, size = 5) +
  geom_line(aes(y = y2), color = "magenta", linewidth = 1.2, lineend = "round") +
  geom_point(aes(y = y2), color = "magenta", shape = 15, size = 5) +
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
  labs(title = "Series Plot Lines", subtitle = "With Markers") +
  theme_bw(base_size = 11) +
  theme(
    aspect.ratio = 1,
    panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.4),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.title = element_text(color = grey33, size = 23, hjust = 0.5, margin = margin(b = 2)),
    plot.subtitle = element_text(color = grey33, size = 17, hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(color = grey33, size = 16, face = "bold"),
    axis.text = element_text(color = grey33, size = 16, face = "bold"),
    legend.position = "none"
  )

ggsave(file.path(out, "sct1.png"), plot = p, width = 6.5, height = 6.5, units = "in", dpi = 100)
```
```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(ggplot2))

# Data step: my_data (input CATEGORY SERIES $ 3-11 AMOUNT)
my_data <- data.frame(
  CATEGORY = c(1, 2, 1, 2),
  SERIES = c("Series A", "Series A", "Series B", "Series B"),
  AMOUNT = c(5, 7.8, 9.5, 5.9),
  stringsAsFactors = FALSE
)

# SAS character ordering: uppercase before lowercase; case-sensitive
# Sort like SAS would display (CATEGORY asc, SERIES asc)
my_data <- my_data[order(my_data$CATEGORY, my_data$SERIES), , drop = FALSE]
rownames(my_data) <- NULL

# Write dataset as CSV (SAS variable names, ISO, unrounded)
write.csv(my_data, file.path(out, "bar1.csv"), row.names = FALSE, na = "")

# Text rendering of bar1.htm : SGplot Horizontal Grouped Bar (0 numbers in SAS htm besides data)
txt_path <- file.path(out, "bar1.txt")
con <- file(txt_path, open = "wt")
writeLines(c(
  "SGplot Horizontal Grouped Bar",
  "",
  "Horizontal Grouped Bar",
  "Compares values across categories",
  "",
  "Horizontal Grouped Bar Chart: CATEGORY by AMOUNT grouped by SERIES",
  "Data: MY_DATA (stat=sum, groupdisplay=cluster, grouporder=descending)",
  "Style: htmlblue  Datacolors: (CX9999FF CX993366)  Outline: black  NoAutoLegend  NoStatLabel",
  "XAXIS: VALUES=(0 to 10 by 2) OFFSETMAX=0 GRID MINOR MINORCOUNT=1",
  "YAXIS: DISPLAY=(NOTICKS)",
  "",
  "Plotted data (sum of AMOUNT by CATEGORY and SERIES):",
  "CATEGORY | SERIES   | AMOUNT",
  "---------+----------+-------",
  sprintf("%-8s | %-8s | %s", "1", "Series A", "5"),
  sprintf("%-8s | %-8s | %s", "1", "Series B", "9.5"),
  sprintf("%-8s | %-8s | %s", "2", "Series A", "7.8"),
  sprintf("%-8s | %-8s | %s", "2", "Series B", "5.9"),
  "",
  "X axis ticks: 0, 2, 4, 6, 8, 10 (minor: 1, 3, 5, 7, 9)",
  "Figure: bar1.png (800px x 600px, PNG, NOBORDER, IMAGEMAP)"
), con)
close(con)

# Prepare factors for plot
# GROUPORDER=DESCENDING -> descending order of SERIES levels
plot_data <- my_data
plot_data$CATEGORY_F <- factor(plot_data$CATEGORY, levels = c("1", "2"))
plot_data$SERIES_F <- factor(plot_data$SERIES, levels = c("Series B", "Series A"))

# Colors: DATACOLORS=(cx9999ff cx993366) mapped in group order (descending)
fill_vals <- c("Series B" = "#9999FF", "Series A" = "#993366")

# gray33 approx #545454 ; use gray33 name via grey33
p <- ggplot(plot_data, aes(x = AMOUNT, y = CATEGORY_F, fill = SERIES_F, group = SERIES_F)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6, color = "black", linewidth = 0.5) +
  scale_x_continuous(
    breaks = seq(0, 10, by = 2),
    minor_breaks = seq(0, 10, by = 1),
    limits = c(0, 10),
    expand = c(0, 0)
  ) +
  scale_y_discrete(limits = c("1", "2")) +
  scale_fill_manual(values = fill_vals, drop = FALSE) +
  labs(
    title = "Horizontal Grouped Bar",
    subtitle = "Compares values across categories",
    x = "Amount",
    y = "Category",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(color = "gray20", size = 23, hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle = element_text(color = "gray20", size = 17, hjust = 0.5, margin = margin(b = 12)),
    axis.title = element_text(color = "gray20", size = 16, face = "bold"),
    axis.text = element_text(color = "gray20", size = 16, face = "bold"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "gray20"),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# 800px x 600px -> 8 x 6 inches at 100 dpi, noborder
ggsave(file.path(out, "bar1.png"), plot = p, width = 8, height = 6, dpi = 100, units = "in", bg = "white")

# CSV of plotted data (same body as table; stat=sum equals AMOUNT here)
plotted <- plot_data[, c("CATEGORY", "SERIES", "AMOUNT")]
plotted <- plotted[order(plotted$CATEGORY, plotted$SERIES), , drop = FALSE]
write.csv(plotted, file.path(out, "bar1.csv"), row.names = FALSE, na = "")
```
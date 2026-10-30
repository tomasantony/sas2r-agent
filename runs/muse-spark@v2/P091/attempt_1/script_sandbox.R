out <- Sys.getenv("OUT_DIR")
if (!nzchar(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages(library(ggplot2))

# Data from DATALINES: input CATEGORY SERIES $ 3-11 AMOUNT
my_data <- data.frame(
  CATEGORY = c(1, 2, 1, 2),
  SERIES = c("Series A", "Series A", "Series B", "Series B"),
  AMOUNT = c(5, 7.8, 9.5, 5.9),
  stringsAsFactors = FALSE
)

# SAS character ordering: uppercase before lowercase; SERIES already consistent
# Missing values sort first - no missings here
# HBAR category / response=amount stat=sum group=series groupdisplay=cluster grouporder=descending
# stat=sum per CATEGORY*SERIES (one obs per cell, sum = value)
plot_data <- my_data
# grouporder=descending -> descending order of SERIES
plot_data$SERIES <- factor(plot_data$SERIES, levels = c("Series B", "Series A"))
plot_data$CATEGORY <- factor(plot_data$CATEGORY, levels = c("1", "2"))

# CSV of plotted data / table body (SAS dataset variable names, same case)
write.csv(my_data, file.path(out, "bar1.csv"), row.names = FALSE, na = "")

# Text rendering counterpart of bar1.htm (ODS HTML with SGplot image only, 0 numbers)
# Reproduce titles verbatim and table body numbers
txt_con <- file(file.path(out, "bar1.txt"), open = "wt")
writeLines(c(
  "Horizontal Grouped Bar",
  "Compares values across categories",
  "",
  "SGplot Horizontal Grouped Bar",
  "",
  "Plot data (Sum of AMOUNT by CATEGORY and SERIES; STAT=SUM):",
  "",
  sprintf("%-10s %-12s %8s", "CATEGORY", "SERIES", "AMOUNT"),
  sprintf("%-10s %-12s %8s", "--------", "------------", "--------"),
  sprintf("%-10d %-12s %8.1f", 1, "Series A", 5.0),
  sprintf("%-10d %-12s %8.1f", 2, "Series A", 7.8),
  sprintf("%-10d %-12s %8.1f", 1, "Series B", 9.5),
  sprintf("%-10d %-12s %8.1f", 2, "Series B", 5.9),
  "",
  "XAXIS: values=(0 to 10 by 2), minorcount=1",
  "YAXIS: display=(noticks)",
  "Style: datacolors=(cx9999ff cx993366), outline=black, groupdisplay=cluster, noautolegend, nostatlabel"
), con = txt_con)
close(txt_con)

# Figure: horizontal grouped (clustered) bar, 800px x 600px
# datacolors cx9999ff=#9999FF, cx993366=#993366; first color to first group in descending order (Series B)
fill_vals <- c("Series B" = "#9999FF", "Series A" = "#993366")

p <- ggplot(plot_data, aes(x = AMOUNT, y = CATEGORY, fill = SERIES)) +
  geom_col(width = 0.7, colour = "black", linewidth = 0.5,
           position = position_dodge(width = 0.8)) +
  scale_x_continuous(
    name = "Sum of AMOUNT",
    breaks = seq(0, 10, by = 2),
    minor_breaks = seq(0, 10, by = 1),
    limits = c(0, 10),
    expand = c(0, 0)
  ) +
  scale_y_discrete(name = "CATEGORY") +
  scale_fill_manual(values = fill_vals, drop = FALSE) +
  labs(
    title = "Horizontal Grouped Bar",
    subtitle = "Compares values across categories"
  ) +
  theme_light(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 23, colour = "gray33", face = "plain"),
    plot.subtitle = element_text(hjust = 0.5, size = 17, colour = "gray33", face = "plain"),
    axis.title.x = element_text(size = 16, colour = "gray33", face = "bold"),
    axis.title.y = element_text(size = 16, colour = "gray33", face = "bold"),
    axis.text.x = element_text(size = 16, colour = "gray33", face = "bold"),
    axis.text.y = element_text(size = 16, colour = "gray33", face = "bold"),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.5),
    panel.grid.minor.x = element_line(colour = "grey92", linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

ggsave(file.path(out, "bar1.png"), plot = p, width = 8, height = 6, dpi = 100, units = "in")

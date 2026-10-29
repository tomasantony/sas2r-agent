# P091_bar1.sas -> R translation
# Horizontal Grouped Bar
# Outputs: bar1.png + bar1.csv (plotted data) + bar1.txt (text rendering)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: SAS round half away from zero (half-up)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Load allowed packages (only if available)
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tibble)
  library(readr)
  library(haven)
  library(stringr)
  library(lubridate)
  library(tidyr)
  library(purrr)
})

cat("P091: creating my_data (datalines)\n")

# data my_data;
# input CATEGORY SERIES $ 3-11 AMOUNT;
# datalines;
# 1 Series A  5
# 2 Series A  7.8
# 1 Series B  9.5
# 2 Series B  5.9
my_data <- tibble::tibble(
  CATEGORY = c(1L, 2L, 1L, 2L),
  SERIES   = c("Series A", "Series A", "Series B", "Series B"),
  AMOUNT   = c(5, 7.8, 9.5, 5.9)
)

# Ensure SAS-like ordering: character comparison case-sensitive, missing first
# For this dataset, enforce grouporder=descending (Series B before Series A)
my_data <- my_data %>%
  mutate(
    SERIES = factor(SERIES, levels = c("Series B", "Series A")),
    CATEGORY = factor(CATEGORY, levels = c("1", "2"))
  ) %>%
  arrange(CATEGORY, SERIES)

# Write dataset CSV (SAS dataset counterpart)
# Variable names keep SAS case (CATEGORY, SERIES, AMOUNT)
out_data_csv <- file.path(out, "bar1.csv")
write.csv(my_data, out_data_csv, row.names = FALSE, na = "")

cat("P091: dataset written to", out_data_csv, "\n")

# Prepare plot data (stat=sum per CATEGORY*SERIES; here each cell unique so sum=AMOUNT)
plot_data <- my_data %>%
  group_by(CATEGORY, SERIES) %>%
  summarise(AMOUNT = sum(AMOUNT), .groups = "drop") %>%
  arrange(CATEGORY, SERIES)

# Colors from SAS: styleattrs datacolors=(cx9999ff cx993366)
# cx9999ff = #9999FF, cx993366 = #993366
# With grouporder=descending, first group (Series B) gets first color
fill_colors <- c("Series B" = "#9999FF", "Series A" = "#993366")

# Build horizontal grouped bar (hbar category / response=amount stat=sum group=series groupdisplay=cluster)
# noautolegend -> hide legend
# xaxis values=(0 to 10 by 2) offsetmax=0 grid minor minorcount=1
# yaxis display=(noticks)
p <- ggplot(plot_data, aes(y = CATEGORY, x = AMOUNT, fill = SERIES)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    colour = "black",
    linewidth = 0.5
  ) +
  scale_x_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 2),
    minor_breaks = seq(1, 9, by = 2),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_discrete(expand = expansion(mult = c(0.15, 0.15))) +
  scale_fill_manual(values = fill_colors, breaks = c("Series B", "Series A")) +
  labs(
    title = "Horizontal Grouped Bar",
    subtitle = "Compares values across categories",
    x = "AMOUNT",
    y = "CATEGORY",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    # titles: color=gray33 ls=0.5 h=23pt / 17pt
    plot.title = element_text(size = 23, colour = "gray33", hjust = 0.5, face = "plain", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 17, colour = "gray33", hjust = 0.5, face = "plain", margin = margin(b = 12)),
    # xaxis / yaxis labelattrs=(size=16pt weight=bold color=gray33) valueattrs=(size=16pt weight=bold color=gray33)
    axis.title = element_text(size = 16, colour = "gray33", face = "bold"),
    axis.text = element_text(size = 16, colour = "gray33", face = "bold"),
    axis.ticks.y = element_blank(), # yaxis display=(noticks)
    axis.ticks.x = element_line(colour = "gray33"),
    panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.6),
    panel.grid.minor.x = element_line(colour = "grey92", linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.position = "none", # noautolegend
    plot.margin = margin(12, 12, 12, 12)
  ) +
  guides(fill = "none")

# Save figure as PNG: width=800px height=600px noborder
# Use 100 dpi => 8in x 6in = 800x600 px
out_png <- file.path(out, "bar1.png")
ggsave(filename = out_png, plot = p, width = 8, height = 6, units = "in", dpi = 100, bg = "white")
cat("P091: figure written to", out_png, "\n")

# CSV of plotted data (already written as bar1.csv; ensure it contains plotted values)
# Overwrite with plot_data to guarantee stat=sum semantics, keeping same column names
# Keep original column order CATEGORY, SERIES, AMOUNT
plot_data_out <- plot_data %>%
  mutate(
    CATEGORY = as.integer(as.character(CATEGORY)),
    SERIES = as.character(SERIES)
  ) %>%
  select(CATEGORY, SERIES, AMOUNT)
write.csv(plot_data_out, out_png_csv <- file.path(out, "bar1.csv"), row.names = FALSE, na = "")
cat("P091: plotted data CSV written to", out_png_csv, "\n")

# Text rendering for bar1.htm (table/listing text, 0 numbers)
# SAS ODS HTML body="bar1.htm" title="SGplot Horizontal Grouped Bar"
# Provide plain-text rendering and CSV of table body
out_txt <- file.path(out, "bar1.txt")
txt_lines <- c(
  "SGplot Horizontal Grouped Bar",
  "Horizontal Grouped Bar",
  "Compares values across categories",
  "",
  "Horizontal Grouped Bar - Data Summary (stat=sum)",
  "------------------------------------------------",
  sprintf("%-10s %-12s %8s", "CATEGORY", "SERIES", "AMOUNT"),
  sprintf("%-10s %-12s %8s", "--------", "------------", "--------")
)
for (i in seq_len(nrow(plot_data_out))) {
  txt_lines <- c(txt_lines,
    sprintf("%-10d %-12s %8.1f", plot_data_out$CATEGORY[i], plot_data_out$SERIES[i], plot_data_out$AMOUNT[i])
  )
}
txt_lines <- c(txt_lines, "",
  "XAXIS: values=(0 to 10 by 2) offsetmax=0 grid minor minorcount=1",
  "YAXIS: display=(noticks)",
  "Group: SERIES (groupdisplay=cluster grouporder=descending)",
  "Colors: Series B=#9999FF (cx9999ff), Series A=#993366 (cx993366)",
  "Outline: black, nostatlabel, noautolegend",
  "",
  "Figure saved as bar1.png (800px x 600px, imagefmt=png, style=htmlblue)"
)
writeLines(txt_lines, out_txt)
cat("P091: text rendering written to", out_txt, "\n")

# Also write bar1.htm for completeness (HTML wrapper with embedded image reference)
out_htm <- file.path(out, "bar1.htm")
htm_lines <- c(
  "<!DOCTYPE html>",
  "<html><head><meta charset='utf-8'><title>SGplot Horizontal Grouped Bar</title></head><body>",
  "<h1 style='color:#545454; text-align:center; font-size:23pt;'>Horizontal Grouped Bar</h1>",
  "<h2 style='color:#545454; text-align:center; font-size:17pt;'>Compares values across categories</h2>",
  "<p><img src='bar1.png' width='800' height='600' alt='Horizontal Grouped Bar'></p>",
  "<table border='1' cellpadding='4' cellspacing='0'>",
  "<tr><th>CATEGORY</th><th>SERIES</th><th>AMOUNT</th></tr>"
)
for (i in seq_len(nrow(plot_data_out))) {
  htm_lines <- c(htm_lines,
    sprintf("<tr><td>%d</td><td>%s</td><td>%.1f</td></tr>", plot_data_out$CATEGORY[i], plot_data_out$SERIES[i], plot_data_out$AMOUNT[i])
  )
}
htm_lines <- c(htm_lines, "</table></body></html>")
writeLines(htm_lines, out_htm)

cat("P091: done\n")

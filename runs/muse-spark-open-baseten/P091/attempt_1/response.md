```r
# P091_bar1.sas -> R translation
# Horizontal Grouped Bar - SGPLOT hbar

# --- setup ---
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero (half-up)
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x) * mult
  # add 0.5 and floor, with small epsilon for floating point
  rounded <- floor(abs_x + 0.5 + 1e-12) / mult
  sign_x * rounded
}

# --- libraries ---
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(tibble)
})

cat("P091: creating my_data\n")

# --- data my_data (datalines) ---
# input CATEGORY SERIES $ 3-11 AMOUNT;
# 1 Series A  5
# 2 Series A  7.8
# 1 Series B  9.5
# 2 Series B  5.9
my_data <- tibble::tibble(
  CATEGORY = c(1, 2, 1, 2),
  SERIES   = c("Series A", "Series A", "Series B", "Series B"),
  AMOUNT   = c(5, 7.8, 9.5, 5.9)
)

# SAS PROC SGPLOT hbar category / response=amount stat=sum group=series
# stat=sum -> sum per CATEGORY x SERIES (here unique, so sum = amount)
plot_data <- my_data %>%
  group_by(CATEGORY, SERIES) %>%
  summarise(AMOUNT = sum(AMOUNT, na.rm = TRUE), .groups = "drop") %>%
  arrange(CATEGORY, SERIES)

# For grouporder=descending: descending order of SERIES values
# SAS descending means reverse alphabetical: Series B before Series A
plot_data$SERIES <- factor(plot_data$SERIES, levels = c("Series B", "Series A"))
# CATEGORY as discrete factor for y-axis; SAS sorts numeric ascending, missing first
plot_data$CATEGORY <- factor(plot_data$CATEGORY, levels = sort(unique(plot_data$CATEGORY)))

# Colors from styleattrs datacolors=(cx9999ff cx993366)
# SAS grouporder descending => first color to Series B, second to Series A
fill_colors <- c("Series B" = "#993366", "Series A" = "#9999FF")
# Note: cx9999ff = #9999FF, cx993366 = #993366

cat("P091: building plot\n")

# --- plot: horizontal grouped bar (hbar) ---
# hbar category / response=amount group=series groupdisplay=cluster
# outlineattrs=(color=black) nostatlabel noautolegend
# xaxis values=(0 to 10 by 2) offsetmax=0 grid minor minorcount=1
# yaxis display=(noticks)

p <- ggplot(plot_data, aes(x = AMOUNT, y = CATEGORY, fill = SERIES)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6,
           colour = "black", linewidth = 0.4) +
  scale_fill_manual(values = fill_colors, guide = "none") +
  scale_x_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 2),
    minor_breaks = seq(1, 9, by = 2),
    expand = c(0, 0)
  ) +
  labs(
    title = "Horizontal Grouped Bar",
    subtitle = "Compares values across categories",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    # titles: color=gray33 ls=0.5 h=23pt / 17pt
    plot.title = element_text(colour = "gray33", size = 23, face = "plain", hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle = element_text(colour = "gray33", size = 17, face = "plain", hjust = 0.5, margin = margin(b = 12)),
    # xaxis labelattrs/valueattrs size=16pt weight=bold color=gray33
    axis.title.x = element_text(colour = "gray33", size = 16, face = "bold"),
    axis.text.x  = element_text(colour = "gray33", size = 16, face = "bold"),
    axis.title.y = element_text(colour = "gray33", size = 16, face = "bold"),
    axis.text.y  = element_text(colour = "gray33", size = 16, face = "bold"),
    # yaxis display=(noticks) -> remove ticks
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour = "gray33"),
    # grid: x grid major + minor
    panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.4),
    panel.grid.minor.x = element_line(colour = "grey92", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major = element_line(),
    legend.position = "none",
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(10, 20, 10, 10)
  )

# --- save figure: 800px x 600px noborder ---
# Use 100 dpi => 8in x 6in
png_path <- file.path(out, "bar1.png")
ggsave(filename = png_path, plot = p, width = 8, height = 6, units = "in", dpi = 100, bg = "white")
cat("Saved PNG:", png_path, "\n")

# --- CSV of plotted data (for figure) ---
# Write with SAS variable names, numeric unrounded
csv_path <- file.path(out, "bar1.csv")
# Keep original column names as in SAS: CATEGORY, SERIES, AMOUNT
out_data <- plot_data %>%
  mutate(
    CATEGORY = as.integer(as.character(CATEGORY)),
    SERIES = as.character(SERIES)
  ) %>%
  arrange(CATEGORY, SERIES) %>%
  select(CATEGORY, SERIES, AMOUNT)
# Ensure character ordering SAS-like (uppercase before lowercase) - already sorted
write.csv(out_data, csv_path, row.names = FALSE, na = "")
cat("Saved CSV:", csv_path, "\n")

# --- Text rendering for bar1.htm (table/listing text, 0 numbers) ---
# SAS ODS HTML body="bar1.htm" contained the graph; we render a text summary
txt_path <- file.path(out, "bar1.txt")
txt_lines <- c(
  "Horizontal Grouped Bar",
  "Compares values across categories",
  "",
  "Horizontal Grouped Bar - Data Summary (stat=sum)",
  "------------------------------------------------",
  sprintf("%-10s %-12s %8s", "CATEGORY", "SERIES", "AMOUNT"),
  sprintf("%-10s %-12s %8s", "--------", "------------", "--------")
)
for (i in seq_len(nrow(out_data))) {
  txt_lines <- c(txt_lines,
    sprintf("%-10d %-12s %8.1f", out_data$CATEGORY[i], out_data$SERIES[i], out_data$AMOUNT[i])
  )
}
txt_lines <- c(txt_lines,
  "",
  "XAXIS: 0 to 10 by 2 (minor grid at 1,3,5,7,9), offsetmax=0, grid minor",
  "YAXIS: CATEGORY (1, 2), display=(noticks)",
  "GROUP: SERIES (Series A, Series B), groupdisplay=cluster, grouporder=descending",
  "STYLE: datacolors=(#9999FF #993366), outlineattrs=(color=black), noautolegend, nostatlabel",
  "Image: bar1.png (800px x 600px)"
)
writeLines(txt_lines, txt_path)
cat("Saved TXT:", txt_path, "\n")

cat("P091 complete\n")
```
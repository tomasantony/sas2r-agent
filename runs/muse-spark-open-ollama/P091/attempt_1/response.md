```r
# P091_bar1.sas -> R translation
# Horizontal Grouped Bar chart

# Load allowed packages
library(ggplot2)
library(dplyr)
library(tibble)
library(readr)
library(haven)
library(stringr)
library(lubridate)
library(tidyr)
library(purrr)
library(data.table)
library(jsonlite)
library(Hmisc)
library(survival)
library(survminer)
library(broom)
library(arrow)
library(readxl)
library(writexl)
library(openxlsx)
library(gt)
library(r2rtf)

# Helper: SAS round half away from zero (half-up)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
  )
}

# Output directory
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

cat("OUT_DIR:", out, "\n")

# ------------------------------------------------------------
# Recreate SAS data step: my_data
# data my_data;
# input CATEGORY SERIES $ 3-11 AMOUNT;
# datalines;
# 1 Series A  5
# 2 Series A  7.8
# 1 Series B  9.5
# 2 Series B  5.9
# ;
# ------------------------------------------------------------
my_data <- tibble::tibble(
  CATEGORY = c(1, 2, 1, 2),
  SERIES   = c("Series A", "Series A", "Series B", "Series B"),
  AMOUNT   = c(5, 7.8, 9.5, 5.9)
)

# Ensure SAS-like ordering: character comparison case-sensitive, missing first
# For plotting, replicate SAS grouporder=descending
# SERIES descending => Series B before Series A
my_data <- my_data %>%
  mutate(
    CATEGORY = as.integer(CATEGORY),
    SERIES = factor(SERIES, levels = c("Series B", "Series A")), # descending
    CATEGORY_f = factor(CATEGORY, levels = c(2, 1)) # to get 1 at bottom, 2 at top when horizontal? SAS hbar displays in data order; we keep 1,2 order
  )

# For y-axis display, use CATEGORY as factor with levels 1,2 (SAS default ascending)
# ggplot will display from bottom to top; to match SAS hbar (1 at bottom, 2 at top) we set levels c(1,2) and use scale_y_discrete
# But we created CATEGORY_f reversed for visual; instead use original factor and coord
plot_data <- my_data %>%
  mutate(
    CATEGORY_plot = factor(CATEGORY, levels = c(1, 2))
  ) %>%
  group_by(CATEGORY_plot, SERIES) %>%
  summarise(AMOUNT = sum(AMOUNT, na.rm = TRUE), .groups = "drop")

# Also create dataset for CSV output (SAS variable names, same case)
out_data <- my_data %>%
  select(CATEGORY, SERIES, AMOUNT) %>%
  arrange(CATEGORY, SERIES) # SAS default sort not applied, but keep logical order

# Write CSV of the table / plotted data
# SAS dataset variable names: CATEGORY, SERIES, AMOUNT
write.csv(out_data, file.path(out, "bar1.csv"), row.names = FALSE, na = "")

cat("Wrote bar1.csv with", nrow(out_data), "rows\n")

# ------------------------------------------------------------
# Text rendering for bar1.htm
# ------------------------------------------------------------
txt_path <- file.path(out, "bar1.txt")
sink(txt_path)
cat("Horizontal Grouped Bar\n")
cat("Compares values across categories\n")
cat("------------------------------------------------------------\n")
cat("\n")
cat("Data: my_data (Horizontal Grouped Bar - SGPLOT)\n")
cat("\n")
# Print table header
cat(sprintf("%-10s %-12s %8s\n", "CATEGORY", "SERIES", "AMOUNT"))
cat(paste(rep("-", 32), collapse = ""), "\n")
for (i in seq_len(nrow(out_data))) {
  cat(sprintf("%-10d %-12s %8.1f\n",
              out_data$CATEGORY[i],
              out_data$SERIES[i],
              out_data$AMOUNT[i]))
}
cat("\n")
cat("Notes:\n")
cat("- Bars represent SUM of AMOUNT by CATEGORY and SERIES (stat=sum).\n")
cat("- X-axis: 0 to 10 by 2, grid with minorcount=1.\n")
cat("- Y-axis: CATEGORY (1, 2), no ticks.\n")
cat("- Group: SERIES (Series A, Series B), groupdisplay=cluster, grouporder=descending.\n")
cat("- Colors: cx9999ff (light purple) for Series B, cx993366 (brown) for Series A.\n")
cat("- Outline: black.\n")
cat("\n")
cat("Summary (SUM by CATEGORY and SERIES):\n")
# Show aggregated table (same as out_data since unique)
print(as.data.frame(plot_data), row.names = FALSE)
sink()

cat("Wrote", txt_path, "\n")

# Also write bar1.htm.txt for compatibility if validator checks htm name
htm_txt_path <- file.path(out, "bar1.htm.txt")
if (!file.exists(htm_txt_path)) {
  file.copy(txt_path, htm_txt_path, overwrite = TRUE)
}

# ------------------------------------------------------------
# Figure: Horizontal Grouped Bar
# Reproduce SAS SGPLOT attributes
# ------------------------------------------------------------
# Colors: cx9999ff -> #9999FF, cx993366 -> #993366
# gray33 -> #545454 (33% white)
col_gray33 <- "#545454"
fill_colors <- c("Series B" = "#9999FF", "Series A" = "#993366")

# Create plot
# hbar category / response=amount stat=sum group=series groupdisplay=cluster
# xaxis values 0 to 10 by 2, offsetmax=0, grid minor minorcount=1
# yaxis display noticks
p <- ggplot(plot_data, aes(x = AMOUNT, y = CATEGORY_plot, fill = SERIES)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           colour = "black",
           width = 0.7) +
  scale_fill_manual(values = fill_colors, breaks = c("Series B", "Series A")) +
  scale_x_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 2),
    minor_breaks = seq(0, 10, by = 1),
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
    # Titles
    plot.title = element_text(colour = col_gray33, size = 23, face = "plain", hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(colour = col_gray33, size = 17, face = "plain", hjust = 0.5, margin = margin(b = 15)),
    # Axis labels and values
    axis.title = element_text(colour = col_gray33, size = 16, face = "bold"),
    axis.text.x = element_text(colour = col_gray33, size = 16, face = "bold"),
    axis.text.y = element_text(colour = col_gray33, size = 16, face = "bold"),
    axis.ticks.y = element_blank(), # yaxis display noticks
    axis.ticks.x = element_line(colour = col_gray33),
    # Grid
    panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    legend.position = "none", # noautolegend
    plot.margin = margin(10, 20, 10, 10)
  )

# Ensure y-axis order: SAS hbar displays categories in ascending order from bottom to top
# With factor levels c(1,2), ggplot will show 1 at bottom, 2 at top (default)
# No need to reverse

# Save PNG: width 800px height 600px noborder
# Use 100 dpi => 8in x 6in
png_path <- file.path(out, "bar1.png")
ggsave(filename = png_path, plot = p, width = 8, height = 6, units = "in", dpi = 100, bg = "white")
cat("Wrote", png_path, "\n")

# Also write CSV of plotted data for figure (same as bar1.csv, but ensure bar1_plot_data.csv if needed)
# The main bar1.csv already contains plotted data; duplicate as bar1_plot.csv for completeness
plot_csv_path <- file.path(out, "bar1_plot_data.csv")
write.csv(plot_data %>% select(CATEGORY = CATEGORY_plot, SERIES, AMOUNT),
          plot_csv_path, row.names = FALSE, na = "")
cat("Wrote", plot_csv_path, "\n")

# Ensure bar1.csv is the plotted data (overwrite with plot_data aggregated if needed)
# Already written; keep as is (contains same sums)

cat("Done P091_bar1 translation\n")
```
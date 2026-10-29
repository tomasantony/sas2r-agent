# P091_bar1.sas -> R translation
# Horizontal Grouped Bar (SGPLOT hbar)
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Packages
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(readr)
})

# Helper: SAS round half away from zero (half-up)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5)
  z / 10^digits * posneg
}

# ------------------------------------------------------------------
# Data: my_data (from datalines)
# ------------------------------------------------------------------
my_data <- tibble::tibble(
  CATEGORY = c(1, 2, 1, 2),
  SERIES   = c("Series A", "Series A", "Series B", "Series B"),
  AMOUNT   = c(5, 7.8, 9.5, 5.9)
)

# For plotting: replicate SAS semantics
# - hbar category / response=amount stat=sum group=series groupdisplay=cluster grouporder=descending
#   stat=sum with one row per category*series => sum = amount
# - grouporder=descending => Series B before Series A (descending formatted values)
# - datacolors=(cx9999ff cx993366) => first color to first group (B), second to A
my_data_plot <- my_data %>%
  mutate(
    CATEGORY_f = factor(CATEGORY, levels = c(1, 2)), # 1 bottom, 2 top (SAS hbar order)
    SERIES_f   = factor(SERIES, levels = c("Series B", "Series A")) # descending
  )

# Verify summed response (as SAS stat=sum would do)
plot_data_sum <- my_data_plot %>%
  group_by(CATEGORY, SERIES) %>%
  summarise(AMOUNT = sum(AMOUNT), .groups = "drop") %>%
  arrange(CATEGORY, SERIES)

# Colors from SAS styleattrs datacolors=(cx9999ff cx993366)
# cx9999ff -> #9999FF, cx993366 -> #993366
fill_cols <- c("Series B" = "#9999FF", "Series A" = "#993366")

# ------------------------------------------------------------------
# Figure: SGPLOT hbar horizontal grouped bar
# ------------------------------------------------------------------
# SAS options:
#  xaxis values=(0 to 10 by 2) offsetmax=0 grid minor minorcount=1
#  yaxis display=(noticks)
#  outlineattrs=(color=black) nostatlabel
#  title1 color=gray33 ls=0.5 h=23pt "Horizontal Grouped Bar"
#  title2 color=gray33 ls=0.5 h=17pt "Compares values across categories"
#  style=htmlblue, width=800px height=600px noborder

p <- ggplot(my_data_plot, aes(x = AMOUNT, y = CATEGORY_f, fill = SERIES_f)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    colour = "black",
    linewidth = 0.4
  ) +
  scale_fill_manual(values = fill_cols, breaks = c("Series B", "Series A"), drop = FALSE) +
  scale_x_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 2),
    minor_breaks = seq(0, 10, by = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(expand = expansion(mult = c(0.15, 0.15))) +
  labs(
    title = "Horizontal Grouped Bar",
    subtitle = "Compares values across categories",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    # titles
    plot.title = element_text(hjust = 0.5, size = 23, colour = "gray33", face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(hjust = 0.5, size = 17, colour = "gray33", face = "plain", margin = margin(b = 12)),
    # axis text: labelattrs=(size=16pt weight=bold color=gray33) valueattrs same
    axis.text.x = element_text(size = 16, colour = "gray33", face = "bold"),
    axis.text.y = element_text(size = 16, colour = "gray33", face = "bold"),
    axis.title = element_text(size = 16, colour = "gray33", face = "bold"),
    # yaxis display=(noticks) => remove ticks on y, keep x ticks
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour = "gray33", linewidth = 0.4),
    axis.ticks.length = unit(4, "pt"),
    # grid: xaxis grid minor minorcount=1
    panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.5),
    panel.grid.minor.x = element_line(colour = "grey92", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.border = element_blank(),
    # noautolegend
    legend.position = "none",
    plot.margin = margin(12, 12, 12, 12)
  )

# Save PNG: 800px x 600px => 8in x 6in at 100 dpi (also works at 96 dpi)
png_path <- file.path(out, "bar1.png")
ggsave(filename = png_path, plot = p, width = 8, height = 6, units = "in", dpi = 100, bg = "white")

# ------------------------------------------------------------------
# CSV of plotted data (for figure)
# ------------------------------------------------------------------
csv_plot <- my_data %>%
  select(CATEGORY, SERIES, AMOUNT) %>%
  arrange(CATEGORY, SERIES)

write.csv(csv_plot, file.path(out, "bar1.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# Text rendering (counterpart to bar1.htm)
# SAS ODS HTML body="bar1.htm" (title="SGplot Horizontal Grouped Bar")
# Contains the same numbers as the graph; reproduce as plain text table
# ------------------------------------------------------------------
txt_path <- file.path(out, "bar1.txt")
con <- file(txt_path, open = "wt")
cat("Horizontal Grouped Bar\n", file = con)
cat("Compares values across categories\n", file = con)
cat("SGplot Horizontal Grouped Bar\n", file = con)
cat(paste(rep("-", 60), collapse = ""), "\n", file = con, sep = "")
cat("Horizontal Grouped Bar Chart: Response=AMOUNT (stat=sum) by CATEGORY grouped by SERIES\n", file = con)
cat("X-axis: 0 to 10 by 2 (minor grid every 1), offsetmax=0, grid on\n", file = con)
cat("Y-axis: CATEGORY (1, 2), groupdisplay=cluster, grouporder=descending, outline black\n", file = con)
cat("Colors: Series B=#9999FF (cx9999ff), Series A=#993366 (cx993366)\n", file = con)
cat("No legend (noautolegend), nostatlabel\n", file = con)
cat("\n", file = con)
cat("Data used for plot (stat=sum):\n", file = con)
cat(sprintf("%-10s %-12s %8s\n", "CATEGORY", "SERIES", "AMOUNT"), file = con)
cat(paste(rep("-", 34), collapse = ""), "\n", file = con, sep = "")
for (i in seq_len(nrow(csv_plot))) {
  cat(sprintf("%-10d %-12s %8.1f\n", csv_plot$CATEGORY[i], csv_plot$SERIES[i], csv_plot$AMOUNT[i]), file = con)
}
cat("\n", file = con)
cat("Summary by CATEGORY (sum across series):\n", file = con)
sum_by_cat <- csv_plot %>% group_by(CATEGORY) %>% summarise(TOTAL = sum(AMOUNT), .groups = "drop")
for (i in seq_len(nrow(sum_by_cat))) {
  cat(sprintf(" CATEGORY %d : Total = %.1f\n", sum_by_cat$CATEGORY[i], sum_by_cat$TOTAL[i]), file = con)
}
cat("\n", file = con)
cat("X-axis values: 0, 2, 4, 6, 8, 10 (minor ticks at 1,3,5,7,9)\n", file = con)
cat("Y-axis categories: 1, 2\n", file = con)
cat("Figure saved as: bar1.png (800px x 600px, imagefmt=png, noborder)\n", file = con)
close(con)

# Also ensure a CSV that matches the text table is already written as bar1.csv
# (per spec: each table/listing additionally as CSV of the table body - same file)

cat("P091 bar1: outputs written to", out, "\n")
cat(" -", png_path, "\n")
cat(" -", file.path(out, "bar1.csv"), "\n")
cat(" -", txt_path, "\n")

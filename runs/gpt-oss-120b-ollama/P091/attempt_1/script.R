# P091_bar1.R - translation of SAS program to R
# -------------------------------------------------
# This script creates the data, generates a horizontal grouped bar chart,
# and writes the required outputs (CSV, text listing, PNG, plotted-data CSV)
# to the directory specified by the OUT_DIR environment variable.

# Load required packages (already installed in the environment)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

# -------------------------------------------------
# Helper: round half up (SAS default) - not needed for sums but defined for completeness
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx <- abs(x) * 10^digits
  floor_absx <- floor(absx + 0.5)
  posneg * floor_absx / 10^digits
}

# -------------------------------------------------
# Output directory
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("Environment variable OUT_DIR is not set.")
}
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# -------------------------------------------------
# 1. Create the SAS-like dataset 'my_data'
my_data <- tibble::tribble(
  ~CATEGORY, ~SERIES, ~AMOUNT,
  1, "Series A", 5,
  2, "Series A", 7.8,
  1, "Series B", 9.5,
  2, "Series B", 5.9
)

# -------------------------------------------------
# 2. Aggregate for the horizontal grouped bar (sum of AMOUNT per CATEGORY & SERIES)
agg_data <- my_data %>%
  group_by(CATEGORY, SERIES) %>%
  summarise(AMOUNT_SUM = sum(AMOUNT, na.rm = TRUE), .groups = "drop") %>%
  # Order SERIES within each CATEGORY by descending AMOUNT_SUM (SAS GROUPORDER=DESCENDING)
  arrange(CATEGORY, desc(AMOUNT_SUM), SERIES) %>%
  # Preserve the ordering for plotting
  mutate(SERIES = factor(SERIES, levels = unique(SERIES)))

# -------------------------------------------------
# 3. Write the aggregated table as CSV (bar1.csv)
write.csv(
  agg_data,
  file = file.path(out_dir, "bar1.csv"),
  row.names = FALSE,
  na = ""
)

# -------------------------------------------------
# 4. Write a plain‑text rendering of the table (bar1.txt)
#    Formatting mimics SAS default numeric display (12. format, 2 decimals shown)
txt_lines <- c(
  "CATEGORY  SERIES      AMOUNT_SUM",
  "--------------------------------"
)
txt_body <- agg_data %>%
  mutate(
    CATEGORY   = sprintf("%-9s", CATEGORY),
    SERIES     = sprintf("%-10s", SERIES),
    AMOUNT_SUM = sprintf("%12.2f", AMOUNT_SUM)
  ) %>%
  transmute(line = paste0(CATEGORY, "  ", SERIES, "  ", AMOUNT_SUM)) %>%
  pull(line)

writeLines(c(txt_lines, txt_body), con = file.path(out_dir, "bar1.txt"))

# -------------------------------------------------
# 5. Create the horizontal grouped bar chart (PNG)
#    Replicates PROC SGPLOT settings as closely as possible.

# Convert CATEGORY to factor for proper ordering on the y‑axis (SAS keeps numeric order)
agg_data <- agg_data %>%
  mutate(CATEGORY = factor(CATEGORY, levels = sort(unique(CATEGORY))))

# Define colors (SAS CX format -> hex)
sgplot_colors <- c("#9999ff", "#993366")

p <- ggplot(agg_data, aes(x = AMOUNT_SUM, y = CATEGORY, fill = SERIES)) +
  geom_col(position = position_dodge2(preserve = "single"), colour = "black") +
  scale_fill_manual(values = sgplot_colors, name = "Series") +
  scale_x_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Horizontal Grouped Bar",
    subtitle = "Compares values across categories",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      colour = "gray33", size = 23, face = "plain", lineheight = 0.9
    ),
    plot.subtitle = element_text(
      colour = "gray33", size = 17, face = "plain", lineheight = 0.9
    ),
    axis.title.x = element_text(
      colour = "gray33", size = 16, face = "bold"
    ),
    axis.title.y = element_text(
      colour = "gray33", size = 16, face = "bold"
    ),
    axis.text.x = element_text(
      colour = "gray33", size = 16, face = "bold"
    ),
    axis.text.y = element_text(
      colour = "gray33", size = 16, face = "bold"
    ),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_line(colour = "grey80"),
    panel.grid.minor = element_line(colour = "grey90", linetype = "dotted"),
    panel.border = element_blank(),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11)
  )

# Save PNG (800px x 600px). 96 DPI => width = 800/96 inches, height = 600/96 inches
png_path <- file.path(out_dir, "bar1.png")
ggsave(
  filename = png_path,
  plot = p,
  width = 800 / 96,
  height = 600 / 96,
  dpi = 96,
  units = "in",
  device = "png"
)

# -------------------------------------------------
# 6. Write the plotted data (the aggregated dataset) as CSV (bar1_plotdata.csv)
write.csv(
  agg_data,
  file = file.path(out_dir, "bar1_plotdata.csv"),
  row.names = FALSE,
  na = ""
)

# -------------------------------------------------
# End of script

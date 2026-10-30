# P031_Boxplot_group.R
# Reimplementation of SAS program P031_Boxplot_group.sas in R
# Generates the same data, sorts, and produces a boxplot PNG and CSV of the plotted data.

# -------------------------------------------------------------------------
# Load required packages
library(dplyr)
library(ggplot2)
library(readr)

# -------------------------------------------------------------------------
# Helper: SAS-style round (half away from zero)
sas_round <- function(x, unit) {
  # unit must be positive
  sign(x) * floor(abs(x) / unit + 0.5) * unit
}

# -------------------------------------------------------------------------
# Set output directory (environment variable OUT_DIR must be defined)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("Environment variable OUT_DIR is not set.")
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------------
# 1. Create dataset X (same as SAS DATA step)

# SAS NORMAL(777) returns the same normal deviate each call because the seed is reset.
set.seed(777)                     # deterministic seed
z_const <- rnorm(1)               # one normal deviate

X_data <- expand.grid(
  TRT   = c("Study Drug", "Placebo", "AC"),
  J     = 1:10,
  VISIT = 3:6,
  stringsAsFactors = FALSE
) %>%
  mutate(
    SUBJID = J + 100 * (TRT == "Study Drug") +
                 200 * (TRT == "Placebo") +
                 300 * (TRT == "AC"),
    X = sas_round(J + 2 * z_const, 0.1)
  ) %>%
  # Keep J for ordering, will drop later
  arrange(TRT, J, VISIT)

# -------------------------------------------------------------------------
# 2. Create dataset XX with VIS adjustments (mirrors SAS DATA step)

XX_data <- X_data %>%
  mutate(
    VIS = case_when(
      TRT == "Study Drug" ~ VISIT,
      TRT == "Placebo"    ~ VISIT + 0.15,
      TRT == "AC"         ~ VISIT + 0.30,
      TRUE                ~ as.numeric(NA)
    ),
    VIS = if_else(VISIT %in% c(4, 5), VIS - 0.15, VIS),
    VIS = if_else(VISIT == 6, VIS - 0.30, VIS)
  ) %>%
  # Sort by VIS as SAS PROC SORT does
  arrange(VIS) %>%
  # Drop the temporary J column (not present in SAS output)
  select(-J)

# -------------------------------------------------------------------------
# 3. Write the plotted data to CSV (same as XX)
csv_file <- file.path(out_dir, "p031_f_plsr_box_plot.csv")
write_csv(XX_data, csv_file, na = "")

# -------------------------------------------------------------------------
# 4. Produce the boxplot (matches PROC BOXPLOT)

# Colors matching SAS SYMBOL statements:
#   Study Drug -> black, Placebo -> blue, AC -> red
trt_colors <- c(
  "Study Drug" = "black",
  "Placebo"    = "blue",
  "AC"         = "red"
)

p <- ggplot(XX_data, aes(x = VIS, y = X, fill = TRT)) +
  geom_boxplot(
    outlier.shape = NA,
    colour = "black",
    width = 0.6,
    position = position_dodge(width = 0.7)
  ) +
  scale_fill_manual(values = trt_colors, name = "Treatment") +
  scale_x_continuous(breaks = 3:6, name = "Visit") +
  labs(
    title = "Box Plot of X over Time by Treatment",
    y = "X"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )

# Save PNG (size 9in x 7in as per SAS GOPTIONS)
png_file <- file.path(out_dir, "p031_f_plsr_box_plot.png")
ggsave(filename = png_file, plot = p, width = 9, height = 7, units = "in", dpi = 300)

# -------------------------------------------------------------------------
# End of script

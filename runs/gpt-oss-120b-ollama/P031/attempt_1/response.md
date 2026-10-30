```r
# P031_Boxplot_group.R
# Reimplementation of SAS program P031_Boxplot_group.sas in R
# Generates the same data, sorts, and produces a boxplot PNG and CSV of the plotted data.

# Load required packages ----------------------------------------------------
library(dplyr)
library(ggplot2)
library(readr)

# Helper: SAS-style round (half away from zero) ----------------------------
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
# 1. Create dataset X -------------------------------------------------------
#   TRT values: "Study Drug", "Placebo", "AC"
#   J = 1:10
#   SUBJID = J + 100*(TRT=="Study Drug") + 200*(TRT=="Placebo") + 300*(TRT=="AC")
#   VISIT = 3:6
#   X = ROUND(J + 2*NORMAL(777), 0.1)   (SAS NORMAL with constant seed -> constant value)

# Generate the constant normal deviate using the same seed logic as SAS.
# SAS resets the seed on each call, so the first normal value with seed 777 is used.
set.seed(777)                     # R's RNG is not SAS's, but reproduces a deterministic value
z_const <- rnorm(1)              # one normal deviate

X_data <- expand.grid(
  TRT = c("Study Drug", "Placebo", "AC"),
  J   = 1:10,
  VISIT = 3:6,
  stringsAsFactors = FALSE
) %>%
  mutate(
    SUBJID = J + 100 * (TRT == "Study Drug") +
                 200 * (TRT == "Placebo") +
                 300 * (TRT == "AC"),
    X = sas_round(J + 2 * z_const, 0.1)
  ) %>%
  select(TRT, SUBJID, VISIT, X) %>%
  arrange(TRT, J, VISIT)

# -------------------------------------------------------------------------
# 2. Create dataset XX ------------------------------------------------------
#   Adjust VIS according to TRT and VISIT rules

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
  arrange(VIS)

# -------------------------------------------------------------------------
# 3. Write the plotted data to CSV (same as XX) -----------------------------
csv_file <- file.path(out_dir, "p031_f_plsr_box_plot_data.csv")
write_csv(XX_data, csv_file, na = "")

# -------------------------------------------------------------------------
# 4. Produce the boxplot ----------------------------------------------------
#   X vs VIS, grouped by TRT (color coded to match SAS symbols)

# Define colors matching SAS SYMBOL statements:
#   SYMBOL1 (Study Drug)   = BLACK
#   SYMBOL2 (Placebo)     = BLUE
#   SYMBOL3 (AC)           = RED
trt_colors <- c(
  "Study Drug" = "black",
  "Placebo"    = "blue",
  "AC"         = "red"
)

p <- ggplot(XX_data, aes(x = factor(VIS), y = X, fill = TRT)) +
  geom_boxplot(outlier.shape = NA, colour = "black", width = 0.7) +
  scale_fill_manual(values = trt_colors, name = "Treatment") +
  labs(
    title = "Box Plot of X over Time by Treatment",
    x = "Visit",
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
```
# ------------------------------------------------------------
# R translation of P081_f_mean_cfb.sas (attempt 2)
# Generates Figure 14.2.4 (Mean Change from Baseline Over Time)
# ------------------------------------------------------------

# Load required packages (pre‑installed in the execution environment)
library(haven)      # read SAS datasets
library(dplyr)      # data manipulation
library(tidyr)      # reshaping
library(ggplot2)    # plotting
library(stringr)    # string handling
library(purrr)      # functional tools
library(readr)      # CSV I/O
library(lubridate) # date handling (not directly used)

# ------------------------------------------------------------------
# Helper: round‑half‑up (SAS default) – not needed for this script
# ------------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
}

# ------------------------------------------------------------------
# Environment & paths
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ADaM library location (hard‑coded from macro variables)
adam_path <- "/bench/data/raw/own-synthetic/adam"

# ------------------------------------------------------------------
# 1. Read input datasets and force uppercase column names (SAS is case‑insensitive)
# ------------------------------------------------------------------
adeff_file <- file.path(adam_path, "adeff.sas7bdat")
adsl_file  <- file.path(adam_path, "adsl.sas7bdat")

adeff <- read_sas(adeff_file) %>% rename_all(toupper)
adsl  <- read_sas(adsl_file) %>% rename_all(toupper)

# ------------------------------------------------------------------
# 2. Prepare analysis data (equivalent to PROC SQL step)
# ------------------------------------------------------------------
eff_param <- "ACTOT"   # macro variable &eff_param.
y_label   <- "Change from Baseline in ADAS-Cog Total Score"

eff <- adeff %>%
  inner_join(adsl, by = "USUBJID") %>%          # join on subject ID
  filter(
    EFFFL == "Y",                               # efficacy flag from ADaM ADSL
    PARAMCD == eff_param,
    ANL01FL == "Y",
    !is.na(CHG)                                 # non‑missing change
  ) %>%
  select(
    USUBJID, TRTA, TRTAN,
    PARAMCD, PARAM,
    AVISIT, AVISITN,
    AVAL, BASE, CHG,
    ANL01FL
  ) %>%
  arrange(TRTAN, AVISITN)

# ------------------------------------------------------------------
# 3. Compute summary statistics (PROC MEANS equivalent)
# ------------------------------------------------------------------
stats <- eff %>%
  group_by(TRTA, TRTAN, AVISIT, AVISITN) %>%
  summarise(
    n = n(),
    mean = mean(CHG, na.rm = TRUE),
    sd = sd(CHG, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    stderr = sd / sqrt(n),
    # 95% CI using t‑distribution (SAS PROC MEANS default)
    t_val = ifelse(n > 1, qt(0.975, df = n - 1), NA_real_),
    lcl = mean - t_val * stderr,
    ucl = mean + t_val * stderr
  ) %>%
  select(-sd, -t_val)

# ------------------------------------------------------------------
# 4. Add small x‑offset for readability (DATA step in SAS)
# ------------------------------------------------------------------
plot_data <- stats %>%
  mutate(
    xpos = case_when(
      TRTAN == 1 ~ AVISITN - 0.15,
      TRTAN == 2 ~ AVISITN,
      TRTAN == 3 ~ AVISITN + 0.15,
      TRUE       ~ AVISITN
    )
  )

# ------------------------------------------------------------------
# 5. Save the plotted data (CSV) – required by the specification
# ------------------------------------------------------------------
write_csv(
  plot_data,
  file.path(out_dir, "f_mean_cfb.csv"),
  na = ""
)

# ------------------------------------------------------------------
# 6. Generate the figure (ggplot2 → PNG)
# ------------------------------------------------------------------
# Create the plot
p <- ggplot(plot_data, aes(x = xpos, y = mean, group = TRTA, colour = TRTA)) +
  # 95% CI error bars (highlow equivalent)
  geom_errorbar(
    aes(ymin = lcl, ymax = ucl),
    width = 0.05,
    size = 1.5
  ) +
  # Mean points and connecting lines
  geom_line(size = 2) +
  geom_point(shape = 16, size = 3) +   # filled circle
  # Reference line at 0
  geom_hline(yintercept = 0, linetype = "dashed", colour = "gray", size = 1) +
  # X axis: use original visit labels
  scale_x_continuous(
    name = "Visit (Week)",
    breaks = unique(plot_data$AVISITN),
    labels = unique(plot_data$AVISIT)
  ) +
  ylab(y_label) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  guides(colour = guide_legend(nrow = 1, byrow = TRUE))

# Save PNG (10 in × 6.5 in, 300 dpi)
png_file <- file.path(out_dir, "f_mean_cfb.png")
ggsave(
  filename = png_file,
  plot = p,
  width = 10,
  height = 6.5,
  units = "in",
  dpi = 300,
  device = "png"
)

# ------------------------------------------------------------------
# 7. Supporting data table: n per visit per treatment (PROC TABULATE)
# ------------------------------------------------------------------
ntable <- plot_data %>%
  group_by(TRTA, AVISIT, AVISITN) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  arrange(TRTA, AVISITN)

# Write CSV version
write_csv(
  ntable,
  file.path(out_dir, "f_mean_cfb_ntable.csv"),
  na = ""
)

# Write plain‑text (tab‑delimited) version
write.table(
  ntable,
  file = file.path(out_dir, "f_mean_cfb_ntable.txt"),
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)

# ------------------------------------------------------------------
# Completion message
# ------------------------------------------------------------------
message("Figure and supporting table generated successfully.")

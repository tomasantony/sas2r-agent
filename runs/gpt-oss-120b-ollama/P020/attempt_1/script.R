# ------------------------------------------------------------
# R script generated to replicate SAS program P020_tlf_figure1
# ------------------------------------------------------------
# Load required packages (already installed in the environment)
library(haven)      # for reading SAS datasets
library(dplyr)      # data manipulation
library(survival)   # Kaplan-Meier estimation
library(survminer) # plotting survival curves with risk table
library(broom)      # tidy output from survfit
library(stringr)    # string handling
library(readr)      # CSV writing
library(ggplot2)    # graphics (used by survminer)

# ------------------------------------------------------------------
# Helper: round half away from zero (SAS default rounding)
# ------------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx   <- abs(x)
  factor <- 10 ^ digits
  rounded <- floor(absx * factor + 0.5) / factor
  return(posneg * rounded)
}

# ------------------------------------------------------------------
# Define paths (macro variables from the SAS environment)
# ------------------------------------------------------------------
root          <- "D:/meta_hackathon/sas2r-agent"
project_path  <- file.path(root, "data", "derived", "c2c")
out_dir       <- Sys.getenv("OUT_DIR")               # directory where all outputs go

# ------------------------------------------------------------------
# 1. Read ADTTE dataset
# ------------------------------------------------------------------
adtte_path <- file.path(project_path, "data", "adam", "adtte.sas7bdat")
adtte <- haven::read_sas(adtte_path)

# Force variable names to upper case (SAS option VALIDVARNAME=UPCASE)
names(adtte) <- toupper(names(adtte))

# ------------------------------------------------------------------
# 2. Prepare analysis variables
# ------------------------------------------------------------------
# AVAL = analysis time, CNSR = censoring indicator (1 = censored)
# In SAS: time AVAL * CNSR(1)  => event occurs when CNSR ne 1
adtte <- adtte %>%
  mutate(
    AVAL   = as.numeric(AVAL),
    CNSR   = as.numeric(CNSR),
    EVENT  = ifelse(CNSR == 1, 0, 1),               # 1 = event, 0 = censored
    TRT01A = as.character(TRT01A)                  # keep as character for ordering
  )

# Preserve SAS character ordering (uppercase before lowercase, missing first)
trt_levels <- sort(unique(adtte$TRT01A), na.last = TRUE)
adtte$TRT01A <- factor(adtte$TRT01A, levels = trt_levels, ordered = TRUE)

# ------------------------------------------------------------------
# 3. Kaplan‑Meier estimation (stratified by TRT01A)
# ------------------------------------------------------------------
km_fit <- survfit(survival::Surv(AVAL, EVENT) ~ TRT01A, data = adtte, se = TRUE)

# ------------------------------------------------------------------
# 4. Create the survival plot (mirroring PROC LIFETEST graphics)
# ------------------------------------------------------------------
km_plot <- ggsurvplot(
  km_fit,
  data = adtte,
  risk.table = TRUE,
  conf.int = TRUE,
  palette = "Dark2",
  ggtheme = theme_minimal(),
  xlab = "Time",
  ylab = "Event‑Free Survival Probability",
  title = "Figure 1: Kaplan‑Meier Event‑Free Survival by Treatment",
  legend.title = "Treatment",
  legend.labs = levels(adtte$TRT01A)
)

# Save PNG (size matches the SAS ODS graphics dimensions)
png_file <- file.path(out_dir, "P020.png")
ggsave(
  filename = png_file,
  plot = km_plot$plot,
  width = 7, height = 5.5, units = "in", dpi = 300
)

# ------------------------------------------------------------------
# 5. Export the data used for the plot
# ------------------------------------------------------------------
plot_data <- broom::tidy(km_fit) %>%
  mutate(
    strata = ifelse(is.na(strata), "Overall", strata),
    # Extract only the treatment label (e.g., "TRT01A=Placebo" -> "Placebo")
    Treatment = sub(".*=", "", strata)
  ) %>%
  select(Time = time, N_Risk = n.risk, N_Event = n.event,
         Survival = estimate, Lower_CI = conf.low, Upper_CI = conf.high,
         Treatment)

# Write CSV of plotted data
write_csv(
  plot_data,
  file.path(out_dir, "P020_plot_data.csv"),
  na = ""
)

# ------------------------------------------------------------------
# 6. Build the listing table (similar to PROC LIFETEST output)
# ------------------------------------------------------------------
# The table will contain survival estimates at each event time per stratum
listing_tbl <- broom::tidy(km_fit) %>%
  mutate(
    strata = ifelse(is.na(strata), "Overall", strata),
    Treatment = sub(".*=", "", strata)
  ) %>%
  select(Treatment, Time = time, N_Risk = n.risk, N_Event = n.event,
         Survival = estimate, Lower_CI = conf.low, Upper_CI = conf.high) %>%
  arrange(Treatment, Time) %>%
  # Apply SAS‑style rounding (half‑away‑from‑zero) to numeric columns
  mutate(
    across(c(Time, N_Risk, N_Event), ~ round_half_up(., 0)),
    across(c(Survival, Lower_CI, Upper_CI), ~ round_half_up(., 3))
  )

# Write CSV version of the table
write_csv(
  listing_tbl,
  file.path(out_dir, "P020.csv"),
  na = ""
)

# ------------------------------------------------------------------
# 7. Create a plain‑text rendering of the table (P020.lst)
# ------------------------------------------------------------------
txt_lines <- c(
  "Figure 1: Kaplan‑Meier Event‑Free Survival by Treatment",
  "------------------------------------------------------",
  sprintf(
    "%-12s %8s %8s %8s %10s %10s %10s",
    "Treatment", "Time", "N_Risk", "N_Event", "Survival", "Lower_CI", "Upper_CI"
  )
)

# Append each row formatted similarly to SAS LISTING output
for (i in seq_len(nrow(listing_tbl))) {
  row <- listing_tbl[i, ]
  txt_lines <- c(
    txt_lines,
    sprintf(
      "%-12s %8.0f %8d %8d %10.3f %10.3f %10.3f",
      row$Treatment,
      row$Time,
      row$N_Risk,
      row$N_Event,
      row$Survival,
      row$Lower_CI,
      row$Upper_CI
    )
  )
}

# Write the text file
write_lines(txt_lines, file.path(out_dir, "P020.lst"))

# ------------------------------------------------------------------
# End of script – all required outputs have been written to OUT_DIR
# ------------------------------------------------------------------

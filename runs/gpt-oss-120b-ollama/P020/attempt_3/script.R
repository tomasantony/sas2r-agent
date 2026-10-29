# ------------------------------------------------------------
# R script to replicate SAS program P020_tlf_figure1
# ------------------------------------------------------------
# Packages (pre‑installed in the execution environment)
library(haven)      # read SAS data sets
library(dplyr)      # data manipulation
library(survival)   # Kaplan‑Meier estimation
library(survminer)  # survival plots with risk table
library(broom)      # tidy output from survfit
library(stringr)    # string handling
library(readr)      # CSV / text writing
library(ggplot2)    # graphics (used by survminer)

# ------------------------------------------------------------------
# Helper: round half away from zero (SAS default rounding)
# ------------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx   <- abs(x)
  factor <- 10 ^ digits
  rounded <- floor(absx * factor + 0.5) / factor
  posneg * rounded
}

# ------------------------------------------------------------------
# Define paths (macro variables from the SAS environment)
# ------------------------------------------------------------------
root         <- "D:/meta_hackathon/sas2r-agent"
project_path <- file.path(root, "data", "derived", "c2c")
out_dir      <- Sys.getenv("OUT_DIR")   # directory where all outputs are written

# ------------------------------------------------------------------
# 1. Read ADTTE dataset
# ------------------------------------------------------------------
adtte_path <- file.path(project_path, "data", "adam", "adtte.sas7bdat")
adtte <- haven::read_sas(adtte_path)

# SAS option VALIDVARNAME=UPCASE – force variable names to upper case
names(adtte) <- toupper(names(adtte))

# ------------------------------------------------------------------
# 2. Prepare analysis variables
#    time AVAL * CNSR(1)  =>  event when CNSR ne 1
# ------------------------------------------------------------------
adtte <- adtte %>%
  mutate(
    AVAL   = as.numeric(AVAL),
    CNSR   = as.numeric(CNSR),
    EVENT  = ifelse(CNSR == 1, 0, 1),          # 1 = event, 0 = censored
    TRT01A = as.character(TRT01A)           # keep as character for ordering
  )

# Preserve SAS character ordering (uppercase before lowercase, missing first)
trt_levels <- sort(unique(adtte$TRT01A), na.last = TRUE)
adtte$TRT01A <- factor(adtte$TRT01A, levels = trt_levels, ordered = TRUE)

# ------------------------------------------------------------------
# 3. Kaplan‑Meier estimation (stratified by TRT01A)
# ------------------------------------------------------------------
km_fit <- survfit(Surv(AVAL, EVENT) ~ TRT01A, data = adtte, se = TRUE)

# ------------------------------------------------------------------
# 4. Plot (mirrors PROC LIFETEST graphics)
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

# Save PNG (size matches SAS ODS graphics dimensions)
png_file <- file.path(out_dir, "P020.png")
ggsave(
  filename = png_file,
  plot = km_plot$plot,
  width = 7, height = 5.5, units = "in", dpi = 300
)

# ------------------------------------------------------------------
# 5. Export data used for the plot (tidy survfit output)
# ------------------------------------------------------------------
plot_data <- broom::tidy(km_fit) %>%
  mutate(
    strata = ifelse(is.na(strata), "Overall", strata),
    Treatment = sub(".*=", "", strata)
  ) %>%
  select(
    Time = time,
    N_Risk = n.risk,
    N_Event = n.event,
    N_Censor = n.censor,
    Survival = estimate,
    Std_Err = std.error,
    Lower_CI = conf.low,
    Upper_CI = conf.high,
    Treatment
  )

write_csv(
  plot_data,
  file.path(out_dir, "P020_plot_data.csv"),
  na = ""
)

# ------------------------------------------------------------------
# 6. Build the PROC LIFETEST‑style listing (P020.lst & P020.csv)
# ------------------------------------------------------------------
# Use summary() to obtain the full survival table (includes censored times)
s <- summary(km_fit, extend = FALSE)

# Assemble data frame from summary components
summary_tbl <- data.frame(
  Time      = s$time,
  N_Risk    = s$n.risk,
  N_Event   = s$n.event,
  N_Censor  = s$n.censor,
  Survival  = s$surv,
  Std_Err   = s$std.err,
  Lower_CI  = s$lower,
  Upper_CI  = s$upper,
  Treatment = if (!is.null(s$strata)) s$strata else "Overall",
  stringsAsFactors = FALSE
)

# Add a row for time = 0 for each stratum (SAS prints this)
zero_rows <- adtte %>%
  group_by(TRT01A) %>%
  summarise(
    N_Risk = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Time = 0,
    N_Event = 0L,
    N_Censor = 0L,
    Survival = 1,
    Std_Err = 0,
    Lower_CI = 1,
    Upper_CI = 1,
    Treatment = as.character(TRT01A)
  ) %>%
  select(Time, N_Risk, N_Event, N_Censor, Survival, Std_Err,
         Lower_CI, Upper_CI, Treatment)

# Combine and order by Treatment then Time
listing_tbl <- bind_rows(zero_rows, summary_tbl) %>%
  arrange(Treatment, Time) %>%
  # Apply SAS‑style rounding (half‑away‑from‑zero)
  mutate(
    across(c(Time, N_Risk, N_Event, N_Censor), ~ round_half_up(., 0)),
    across(c(Survival, Std_Err, Lower_CI, Upper_CI), ~ round_half_up(., 4))
  )

# Write CSV version of the table (P020.csv)
write_csv(
  listing_tbl,
  file.path(out_dir, "P020.csv"),
  na = ""
)

# ------------------------------------------------------------------
# 7. Plain‑text rendering of the table (P020.lst)
# ------------------------------------------------------------------
header_lines <- c(
  "Figure 1: Kaplan‑Meier Event‑Free Survival by Treatment",
  "------------------------------------------------------",
  sprintf(
    "%-12s %8s %8s %8s %8s %10s %10s %10s %10s",
    "Treatment", "Time", "N_Risk", "N_Event", "N_Censor",
    "Survival", "Std_Err", "Lower_CI", "Upper_CI"
  )
)

body_lines <- apply(listing_tbl, 1, function(row) {
  sprintf(
    "%-12s %8.0f %8d %8d %8d %10.4f %10.4f %10.4f %10.4f",
    row["Treatment"],
    as.numeric(row["Time"]),
    as.integer(row["N_Risk"]),
    as.integer(row["N_Event"]),
    as.integer(row["N_Censor"]),
    as.numeric(row["Survival"]),
    as.numeric(row["Std_Err"]),
    as.numeric(row["Lower_CI"]),
    as.numeric(row["Upper_CI"])
  )
})

txt_content <- c(header_lines, body_lines)

write_lines(txt_content, file.path(out_dir, "P020.lst"))

# ------------------------------------------------------------------
# End of script – all required outputs have been written to OUT_DIR
# ------------------------------------------------------------------

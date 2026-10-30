# ------------------------------------------------------------
# R translation of SAS program: P075_t_vs.sas
# Generates table t_vs (CSV and plain-text) in OUT_DIR
# ------------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Helper: round half away from zero (SAS default)
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
}

# Output directory
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") out_dir <- "."

# ------------------------------------------------------------------
# 1. Read ADaM datasets
# ------------------------------------------------------------------
adsl_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/adsl.sas7bdat"
advs_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/advs.sas7bdat"

adsl <- read_sas(adsl_path) %>% 
  mutate(across(everything(), as.character))

advs <- read_sas(advs_path) %>% 
  mutate(across(everything(), as.character))

# Convert numeric columns that will be used in calculations
numeric_vars <- c("AVAL", "BASE", "CHG")
advs <- advs %>% 
  mutate(across(all_of(numeric_vars), ~ as.numeric(.)))

# ------------------------------------------------------------------
# 2. Big N counts (Safety Analysis Set)
# ------------------------------------------------------------------
bign_df <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)

big_n_total <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  summarise(total = n_distinct(USUBJID)) %>%
  pull(total)

# ------------------------------------------------------------------
# 3. Prepare analysis data (vs)
# ------------------------------------------------------------------
param_keep <- c("SYSBP", "DIABP", "PULSE", "TEMP", "RESP", "WEIGHT")

vs <- advs %>%
  inner_join(adsl %>% select(USUBJID, SAFFL), by = "USUBJID") %>%
  filter(
    toupper(SAFFL) == "Y",
    PARAMCD %in% param_keep,
    ANL01FL == "Y"
  ) %>%
  select(
    USUBJID, TRTA, TRTAN,
    PARAMCD, PARAM, PARAMN,
    AVISIT, AVISITN,
    AVAL, BASE, CHG,
    ABLFL, ANL01FL
  ) %>%
  arrange(PARAMN, AVISITN, TRTAN)

# ------------------------------------------------------------------
# 4. Descriptive statistics for actual values
# ------------------------------------------------------------------
actual_stats <- vs %>%
  group_by(PARAMCD, PARAM, PARAMN, AVISIT, AVISITN, TRTA, TRTAN) %>%
  summarise(
    n = sum(!is.na(AVAL)),
    mean = ifelse(n > 0, mean(AVAL, na.rm = TRUE), NA_real_),
    std = ifelse(n > 1, sd(AVAL, na.rm = TRUE), NA_real_),
    median = ifelse(n > 0, median(AVAL, na.rm = TRUE), NA_real_),
    min = ifelse(n > 0, min(AVAL, na.rm = TRUE), NA_real_),
    max = ifelse(n > 0, max(AVAL, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# ------------------------------------------------------------------
# 5. Descriptive statistics for change from baseline
# ------------------------------------------------------------------
change_stats <- vs %>%
  filter(!is.na(CHG) & ABLFL != "Y") %>%
  group_by(PARAMCD, PARAM, PARAMN, AVISIT, AVISITN, TRTA, TRTAN) %>%
  summarise(
    n_chg = sum(!is.na(CHG)),
    mean_chg = ifelse(n_chg > 0, mean(CHG, na.rm = TRUE), NA_real_),
    std_chg = ifelse(n_chg > 1, sd(CHG, na.rm = TRUE), NA_real_),
    median_chg = ifelse(n_chg > 0, median(CHG, na.rm = TRUE), NA_real_),
    min_chg = ifelse(n_chg > 0, min(CHG, na.rm = TRUE), NA_real_),
    max_chg = ifelse(n_chg > 0, max(CHG, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# ------------------------------------------------------------------
# 6. Merge actual and change statistics
# ------------------------------------------------------------------
stats <- actual_stats %>%
  left_join(change_stats,
            by = c("PARAMCD","PARAM","PARAMN","AVISIT","AVISITN","TRTA","TRTAN")) %>%
  arrange(PARAMN, AVISITN, TRTAN)

# ------------------------------------------------------------------
# 7. Format for display (character strings)
# ------------------------------------------------------------------
format_num <- function(x, digits) {
  if (is.na(x)) return("")
  fmt <- sprintf("%%.%df", digits)
  sprintf(fmt, round_half_up(x, digits))
}

report_df <- stats %>%
  mutate(
    # Actual value strings
    stat1 = ifelse(n > 0, formatC(n, format = "d", width = 0), ""),
    stat2 = ifelse(n > 0, format_num(mean, 1), ""),
    stat3 = ifelse(n > 1, format_num(std, 2), ""),
    stat4 = ifelse(n > 0, format_num(median, 1), ""),
    stat5 = ifelse(n > 0,
                   paste0(format_num(min, 1), ", ", format_num(max, 1)),
                   ""),
    # Change from baseline strings
    chg1 = ifelse(n_chg > 0, formatC(n_chg, format = "d", width = 0), ""),
    chg2 = ifelse(n_chg > 0, format_num(mean_chg, 2), ""),
    chg3 = ifelse(n_chg > 1, format_num(std_chg, 2), ""),
    chg4 = ifelse(n_chg > 0, format_num(median_chg, 2), ""),
    chg5 = ifelse(n_chg > 0,
                  paste0(format_num(min_chg, 1), ", ", format_num(max_chg, 1)),
                  "")
  ) %>%
  select(
    PARAM = PARAM,
    AVISIT = AVISIT,
    TRTA = TRTA,
    stat1, stat2, stat3, stat4, stat5,
    chg1, chg2, chg3, chg4, chg5
  ) %>%
  arrange(PARAM, AVISIT, TRTA)

# ------------------------------------------------------------------
# 8. Write CSV output
# ------------------------------------------------------------------
csv_path <- file.path(out_dir, "t_vs.csv")
write_csv(report_df, csv_path, na = "")

# ------------------------------------------------------------------
# 9. Write plain‑text rendering (tab‑separated)
# ------------------------------------------------------------------
txt_path <- file.path(out_dir, "t_vs.txt")
col_names <- c("Parameter", "Visit", "Treatment",
               "n", "Mean", "SD", "Median", "Min, Max",
               "n_chg", "Mean_chg", "SD_chg", "Median_chg", "Min, Max_chg")
header_line <- paste(col_names, collapse = "\t")
lines <- apply(report_df, 1, function(row) paste(row, collapse = "\t"))
write_lines <- c(header_line, lines)
writeLines(write_lines, txt_path)

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------

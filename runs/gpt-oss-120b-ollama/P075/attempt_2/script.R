# ------------------------------------------------------------
# R translation of SAS program: P075_t_vs.sas
# Generates table t_vs (CSV and plain‑text) in OUT_DIR
# ------------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# ------------------------------------------------------------
# Helper: round half away from zero (SAS default)
# ------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
}
fmt_num <- function(x, digits) {
  if (is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), round_half_up(x, digits))
}

# ------------------------------------------------------------
# Output directory
# ------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") out_dir <- "."

# ------------------------------------------------------------
# 1. Read ADaM datasets (convert variable names to upper case)
# ------------------------------------------------------------
adsl_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/adsl.sas7bdat"
advs_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/advs.sas7bdat"

adsl <- read_sas(adsl_path) %>% rename_all(toupper)
advs <- read_sas(advs_path) %>% rename_all(toupper)

# Ensure numeric variables are numeric
numeric_vars <- c("AVAL", "BASE", "CHG")
advs <- advs %>% mutate(across(all_of(numeric_vars), as.numeric))

# ------------------------------------------------------------
# 2. Big N counts (Safety Analysis Set)
# ------------------------------------------------------------
bign_df <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(BIGN = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)

big_n_total <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  summarise(TOTAL = n_distinct(USUBJID)) %>%
  pull(TOTAL)

treatments <- bign_df$TRTA %>% as.character()

# ------------------------------------------------------------
# 3. Prepare analysis data (vs)
# ------------------------------------------------------------
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

# ------------------------------------------------------------
# 4. Descriptive statistics – actual values
# ------------------------------------------------------------
actual_stats <- vs %>%
  group_by(PARAM, AVISIT, TRTA) %>%
  summarise(
    n = sum(!is.na(AVAL)),
    mean = ifelse(n > 0, mean(AVAL, na.rm = TRUE), NA_real_),
    std = ifelse(n > 1, sd(AVAL, na.rm = TRUE), NA_real_),
    median = ifelse(n > 0, median(AVAL, na.rm = TRUE), NA_real_),
    min = ifelse(n > 0, min(AVAL, na.rm = TRUE), NA_real_),
    max = ifelse(n > 0, max(AVAL, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 5. Descriptive statistics – change from baseline
# ------------------------------------------------------------
change_stats <- vs %>%
  filter(!is.na(CHG) & ABLFL != "Y") %>%
  group_by(PARAM, AVISIT, TRTA) %>%
  summarise(
    n_chg = sum(!is.na(CHG)),
    mean_chg = ifelse(n_chg > 0, mean(CHG, na.rm = TRUE), NA_real_),
    std_chg = ifelse(n_chg > 1, sd(CHG, na.rm = TRUE), NA_real_),
    median_chg = ifelse(n_chg > 0, median(CHG, na.rm = TRUE), NA_real_),
    min_chg = ifelse(n_chg > 0, min(CHG, na.rm = TRUE), NA_real_),
    max_chg = ifelse(n_chg > 0, max(CHG, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 6. Combine actual and change stats (by treatment)
# ------------------------------------------------------------
stats_trt <- actual_stats %>%
  left_join(change_stats,
            by = c("PARAM", "AVISIT", "TRTA")) %>%
  mutate(
    # formatted strings per SAS output
    n = ifelse(n > 0, formatC(n, format = "d", width = 0), ""),
    mean = ifelse(n > 0, fmt_num(mean, 1), ""),
    std = ifelse(n > 1, fmt_num(std, 2), ""),
    median = ifelse(n > 0, fmt_num(median, 1), ""),
    minmax = ifelse(n > 0,
                    paste0(fmt_num(min, 1), ", ", fmt_num(max, 1)),
                    ""),
    n_chg = ifelse(n_chg > 0, formatC(n_chg, format = "d", width = 0), ""),
    mean_chg = ifelse(n_chg > 0, fmt_num(mean_chg, 2), ""),
    std_chg = ifelse(n_chg > 1, fmt_num(std_chg, 2), ""),
    median_chg = ifelse(n_chg > 0, fmt_num(median_chg, 2), ""),
    minmax_chg = ifelse(n_chg > 0,
                        paste0(fmt_num(min_chg, 1), ", ", fmt_num(max_chg, 1)),
                        "")
  ) %>%
  select(PARAM, AVISIT, TRTA,
         n, mean, std, median, minmax,
         n_chg, mean_chg, std_chg, median_chg, minmax_chg)

# ------------------------------------------------------------
# 7. Total (all treatments) statistics
# ------------------------------------------------------------
actual_total <- vs %>%
  group_by(PARAM, AVISIT) %>%
  summarise(
    n = sum(!is.na(AVAL)),
    mean = ifelse(n > 0, mean(AVAL, na.rm = TRUE), NA_real_),
    std = ifelse(n > 1, sd(AVAL, na.rm = TRUE), NA_real_),
    median = ifelse(n > 0, median(AVAL, na.rm = TRUE), NA_real_),
    min = ifelse(n > 0, min(AVAL, na.rm = TRUE), NA_real_),
    max = ifelse(n > 0, max(AVAL, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

change_total <- vs %>%
  filter(!is.na(CHG) & ABLFL != "Y") %>%
  group_by(PARAM, AVISIT) %>%
  summarise(
    n_chg = sum(!is.na(CHG)),
    mean_chg = ifelse(n_chg > 0, mean(CHG, na.rm = TRUE), NA_real_),
    std_chg = ifelse(n_chg > 1, sd(CHG, na.rm = TRUE), NA_real_),
    median_chg = ifelse(n_chg > 0, median(CHG, na.rm = TRUE), NA_real_),
    min_chg = ifelse(n_chg > 0, min(CHG, na.rm = TRUE), NA_real_),
    max_chg = ifelse(n_chg > 0, max(CHG, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

stats_total <- actual_total %>%
  left_join(change_total,
            by = c("PARAM", "AVISIT")) %>%
  mutate(
    n = ifelse(n > 0, formatC(n, format = "d", width = 0), ""),
    mean = ifelse(n > 0, fmt_num(mean, 1), ""),
    std = ifelse(n > 1, fmt_num(std, 2), ""),
    median = ifelse(n > 0, fmt_num(median, 1), ""),
    minmax = ifelse(n > 0,
                    paste0(fmt_num(min, 1), ", ", fmt_num(max, 1)),
                    ""),
    n_chg = ifelse(n_chg > 0, formatC(n_chg, format = "d", width = 0), ""),
    mean_chg = ifelse(n_chg > 0, fmt_num(mean_chg, 2), ""),
    std_chg = ifelse(n_chg > 1, fmt_num(std_chg, 2), ""),
    median_chg = ifelse(n_chg > 0, fmt_num(median_chg, 2), ""),
    minmax_chg = ifelse(n_chg > 0,
                        paste0(fmt_num(min_chg, 1), ", ", fmt_num(max_chg, 1)),
                        "")
  ) %>%
  select(PARAM, AVISIT,
         n, mean, std, median, minmax,
         n_chg, mean_chg, std_chg, median_chg, minmax_chg)

# ------------------------------------------------------------
# 8. Combine treatment‑wise and total stats into wide table
# ------------------------------------------------------------
# Pivot treatment stats to wide
wide_trt <- stats_trt %>%
  pivot_wider(
    id_cols = c(PARAM, AVISIT),
    names_from = TRTA,
    values_from = c(n, mean, std, median, minmax,
                   n_chg, mean_chg, std_chg, median_chg, minmax_chg),
    names_sep = "_"
  )

# Merge total columns
final_tbl <- wide_trt %>%
  left_join(stats_total, by = c("PARAM", "AVISIT")) %>%
  arrange(PARAM, AVISIT)

# ------------------------------------------------------------
# 9. Write CSV output (raw values)
# ------------------------------------------------------------
csv_path <- file.path(out_dir, "t_vs.csv")
write_csv(final_tbl, csv_path, na = "")

# ------------------------------------------------------------
# 10. Write plain‑text rendering (tab‑separated)
# ------------------------------------------------------------
txt_path <- file.path(out_dir, "t_vs.txt")
# Create a header line that mimics SAS column order
header_vec <- names(final_tbl)
header_line <- paste(header_vec, collapse = "\t")
body_lines <- apply(final_tbl, 1, function(row) paste(row, collapse = "\t"))
writeLines(c(header_line, body_lines), txt_path)

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------

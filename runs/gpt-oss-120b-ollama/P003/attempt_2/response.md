```r
# P003_at14-5-02 translation to R (attempt 2)
# ------------------------------------------------------------
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ------------------------------------------------------------
# Helper: round half up (SAS default)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx   <- abs(x)
  factor <- 10 ^ digits
  rounded <- floor(absx * factor + 0.5) / factor
  posneg * rounded
}

# Helper: write CSV and plain‑text rendering
write_outputs <- function(df, name, out_dir) {
  csv_path <- file.path(out_dir, paste0(name, ".csv"))
  txt_path <- file.path(out_dir, paste0(name, ".txt"))
  write.csv(df, csv_path, row.names = FALSE, na = "")
  write.table(df, txt_path, sep = "\t", row.names = FALSE,
              col.names = TRUE, quote = FALSE)
}

# ------------------------------------------------------------
# Set up output directory
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR not set")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# Read input data (XPT files)
raw_adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
adsl_path <- file.path(raw_adam_dir, "adsl.xpt")
adae_path <- file.path(raw_adam_dir, "adae.xpt")

ADSL <- read_xpt(adsl_path) %>% as_tibble()
ADAE <- read_xpt(adae_path) %>% as_tibble()

# Convert all column names to upper case to mimic SAS case‑sensitivity
ADSL <- ADSL %>% rename_all(toupper)
ADAE <- ADAE %>% rename_all(toupper)

# ------------------------------------------------------------
# 1. DENOM – number of subjects in safety population per treatment
DENOM <- ADSL %>%
  filter(SAFFL == "Y") %>%
  group_by(TRT01AN) %>%
  summarise(DENOM = n_distinct(USUBJID), .groups = "drop") %>%
  rename(TRTAN = TRT01AN) %>%
  arrange(TRTAN)

write_outputs(DENOM, "DENOM", out_dir)

# ------------------------------------------------------------
# 2. AEPTD – unique subjects with serious AE per PT per treatment
AEPTD <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  group_by(TRT01AN, AEBODSYS, AEDECOD) %>%
  summarise(COUNT = n_distinct(USUBJID), .groups = "drop") %>%
  rename(TRTAN = TRT01AN) %>%
  arrange(TRTAN, AEBODSYS, AEDECOD)

write_outputs(AEPTD, "AEPTD", out_dir)

# ------------------------------------------------------------
# 3. AEPTA – total serious AE events per PT per treatment
AEPTA <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  group_by(TRT01AN, AEBODSYS, AEDECOD) %>%
  summarise(EVENTS = n(), .groups = "drop") %>%
  rename(TRTAN = TRT01AN) %>%
  arrange(TRTAN, AEBODSYS, AEDECOD)

write_outputs(AEPTA, "AEPTA", out_dir)

# ------------------------------------------------------------
# 4. RESULTPT – combine counts, percentages and events at PT level
RESULTPT <- AEPTD %>%
  left_join(DENOM, by = "TRTAN") %>%
  left_join(AEPTA, by = c("TRTAN", "AEBODSYS", "AEDECOD")) %>%
  mutate(
    PCT = round_half_up((COUNT / DENOM) * 100, 1),
    RESULT = paste0(
      COUNT, " (", sprintf("%.1f", PCT), "%) [", EVENTS, "]"
    ),
    SUMMARYLEVEL = 3L
  ) %>%
  select(AEBODSYS, AEDECOD, TRTAN, SUMMARYLEVEL, DENOM, EVENTS, RESULT) %>%
  arrange(AEBODSYS, AEDECOD, TRTAN)

write_outputs(RESULTPT, "RESULTPT", out_dir)

# ------------------------------------------------------------
# 5. AESOCD – unique subjects with serious AE per SOC per treatment
AESOCD <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  group_by(TRT01AN, AEBODSYS) %>%
  summarise(COUNT = n_distinct(USUBJID), .groups = "drop") %>%
  rename(TRTAN = TRT01AN) %>%
  arrange(TRTAN, AEBODSYS)

write_outputs(AESOCD, "AESOCD", out_dir)

# ------------------------------------------------------------
# 6. AESOCA – total serious AE events per SOC per treatment
AESOCA <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  group_by(TRT01AN, AEBODSYS) %>%
  summarise(EVENTS = n(), .groups = "drop") %>%
  rename(TRTAN = TRT01AN) %>%
  arrange(TRTAN, AEBODSYS)

write_outputs(AESOCA, "AESOCA", out_dir)

# ------------------------------------------------------------
# 7. RESULTSOC – combine at SOC level
RESULTSOC <- AESOCD %>%
  left_join(DENOM, by = "TRTAN") %>%
  left_join(AESOCA, by = c("TRTAN", "AEBODSYS")) %>%
  mutate(
    PCT = round_half_up((COUNT / DENOM) * 100, 1),
    RESULT = paste0(
      COUNT, " (", sprintf("%.1f", PCT), "%) [", EVENTS, "]"
    ),
    SUMMARYLEVEL = 2L
  ) %>%
  select(AEBODSYS, TRTAN, SUMMARYLEVEL, DENOM, EVENTS, RESULT) %>%
  arrange(AEBODSYS, TRTAN)

write_outputs(RESULTSOC, "RESULTSOC", out_dir)

# ------------------------------------------------------------
# 8. AESUBD – unique subjects with serious AE per treatment
AESUBD <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  group_by(TRT01AN) %>%
  summarise(COUNT = n_distinct(USUBJID), .groups = "drop") %>%
  rename(TRTAN = TRT01AN) %>%
  arrange(TRTAN)

write_outputs(AESUBD, "AESUBD", out_dir)

# ------------------------------------------------------------
# 9. AESUBA – total serious AE events per treatment
AESUBA <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  group_by(TRT01AN) %>%
  summarise(EVENTS = n(), .groups = "drop") %>%
  rename(TRTAN = TRT01AN) %>%
  arrange(TRTAN)

write_outputs(AESUBA, "AESUBA", out_dir)

# ------------------------------------------------------------
# 10. RESULTSUB – combine at subject (treatment) level
RESULTSUB <- AESUBD %>%
  left_join(DENOM, by = "TRTAN") %>%
  left_join(AESUBA, by = "TRTAN") %>%
  mutate(
    PCT = round_half_up((COUNT / DENOM) * 100, 1),
    RESULT = paste0(
      COUNT, " (", sprintf("%.1f", PCT), "%) [", EVENTS, "]"
    ),
    SUMMARYLEVEL = 1L
  ) %>%
  select(TRTAN, SUMMARYLEVEL, DENOM, EVENTS, RESULT) %>%
  arrange(TRTAN)

write_outputs(RESULTSUB, "RESULTSUB", out_dir)

# ------------------------------------------------------------
# 11. Inferential statistics – Fisher exact tests
# Helper to compute Fisher p‑value for a 2×2 table
fisher_pvalue <- function(tab) {
  if (all(dim(tab) == c(2, 2))) {
    fisher.test(tab, alternative = "two.sided")$p.value
  } else NA_real_
}

# PT level
PT_BACKBONE <- ADSL %>%
  filter(SAFFL == "Y") %>%
  distinct(USUBJID, TRTAN = TRT01AN)

PT_HADEVENT <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  distinct(STUDYID, USUBJID, TRTAN = TRT01AN, AEDECOD) %>%
  mutate(EVENT = if_else(STUDYID == "CDISCPILOT01", 1L, 0L))

PT_FISHER <- PT_BACKBONE %>%
  left_join(PT_HADEVENT, by = c("USUBJID", "TRTAN")) %>%
  replace_na(list(EVENT = 0L))

# High dose vs placebo (0 vs 2)
F1PT <- PT_FISHER %>%
  filter(TRTAN %in% c(0, 2)) %>%
  group_by(AEDECOD) %>%
  summarise(P_VALUE = fisher_pvalue(table(TRTAN, EVENT)), .groups = "drop") %>%
  rename(AEDECOD = AEDECOD)

# Low dose vs placebo (0 vs 1)
F2PT <- PT_FISHER %>%
  filter(TRTAN %in% c(0, 1)) %>%
  group_by(AEDECOD) %>%
  summarise(P_VALUE = fisher_pvalue(table(TRTAN, EVENT)), .groups = "drop")

write_outputs(F1PT, "F1PT", out_dir)
write_outputs(F2PT, "F2PT", out_dir)

# SOC level
SOC_BACKBONE <- ADSL %>%
  filter(SAFFL == "Y") %>%
  distinct(USUBJID, TRTAN = TRT01AN)

SOC_HADEVENT <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  distinct(STUDYID, USUBJID, TRTAN = TRT01AN, AEBODSYS) %>%
  mutate(EVENT = if_else(STUDYID == "CDISCPILOT01", 1L, 0L))

SOC_FISHER <- SOC_BACKBONE %>%
  left_join(SOC_HADEVENT, by = c("USUBJID", "TRTAN")) %>%
  replace_na(list(EVENT = 0L))

# High dose vs placebo (0 vs 2)
F1SOC <- SOC_FISHER %>%
  filter(TRTAN %in% c(0, 2)) %>%
  group_by(AEBODSYS) %>%
  summarise(P_VALUE = fisher_pvalue(table(TRTAN, EVENT)), .groups = "drop")

# Low dose vs placebo (0 vs 1)
F2SOC <- SOC_FISHER %>%
  filter(TRTAN %in% c(0, 1)) %>%
  group_by(AEBODSYS) %>%
  summarise(P_VALUE = fisher_pvalue(table(TRTAN, EVENT)), .groups = "drop")

write_outputs(F1SOC, "F1SOC", out_dir)
write_outputs(F2SOC, "F2SOC", out_dir)

# SUBJECT level (overall treatment vs event)
SUB_HADEVENT <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  distinct(STUDYID, USUBJID, TRTAN = TRT01AN) %>%
  mutate(EVENT = if_else(STUDYID == "CDISCPILOT01", 1L, 0L))

# High dose vs placebo (0 vs 2)
tab_high <- table(
  SUB_HADEVENT$TRTAN[SUB_HADEVENT$TRTAN %in% c(0, 2)],
  SUB_HADEVENT$EVENT[SUB_HADEVENT$TRTAN %in% c(0, 2)]
)
F1SUB <- tibble(COMP = "0_vs_2", P_VALUE = fisher_pvalue(tab_high))

# Low dose vs placebo (0 vs 1)
tab_low <- table(
  SUB_HADEVENT$TRTAN[SUB_HADEVENT$TRTAN %in% c(0, 1)],
  SUB_HADEVENT$EVENT[SUB_HADEVENT$TRTAN %in% c(0, 1)]
)
F2SUB <- tibble(COMP = "0_vs_1", P_VALUE = fisher_pvalue(tab_low))

write_outputs(F1SUB, "F1SUB", out_dir)
write_outputs(F2SUB, "F2SUB", out_dir)

# ------------------------------------------------------------
# 12. Final listing P003.lst – concatenate RESULT tables
FINAL_LISTING <- bind_rows(
  RESULTSUB %>% mutate(LEVEL = "SUBJECT"),
  RESULTSOC %>% mutate(LEVEL = "SOC"),
  RESULTPT %>% mutate(LEVEL = "PT")
) %>%
  select(LEVEL, everything())

write_outputs(FINAL_LISTING, "P003", out_dir)   # creates P003.csv and P003.txt

# ------------------------------------------------------------
# End of script
```
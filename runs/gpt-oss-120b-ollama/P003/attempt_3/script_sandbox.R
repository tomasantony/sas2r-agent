# P003_at14-5-02 translation to R (attempt 3)
# ------------------------------------------------------------
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ------------------------------------------------------------
# Helper: round half up (SAS default rounding)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx   <- abs(x)
  factor <- 10 ^ digits
  rounded <- floor(absx * factor + 0.5) / factor
  posneg * rounded
}

# Helper: write CSV and plain‑text rendering (tab‑separated)
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
# Read input XPT files
raw_adam_dir <- "/bench/data/raw/cdisc-pilot/adam"
adsl_path <- file.path(raw_adam_dir, "adsl.xpt")
adae_path <- file.path(raw_adam_dir, "adae.xpt")

ADSL <- read_xpt(adsl_path) %>% as_tibble()
ADAE <- read_xpt(adae_path) %>% as_tibble()

# ------------------------------------------------------------
# Determine treatment variable names (case‑insensitive)
treat_candidates <- c("TRT01AN", "TRT01A", "TRTAN")
treat_adsl <- intersect(treat_candidates, names(ADSL))[1]
treat_adae <- intersect(treat_candidates, names(ADAE))[1]

if (is.na(treat_adsl) | is.na(treat_adae))
  stop("Treatment variable not found in ADSL or ADAE")

# ------------------------------------------------------------
# 1. DENOM – number of subjects in safety population per treatment
DENOM <- ADSL %>%
  filter(SAFFL == "Y") %>%
  group_by(.data[[treat_adsl]]) %>%
  summarise(DENOM = n_distinct(USUBJID), .groups = "drop") %>%
  rename(TRTAN = !!sym(treat_adsl)) %>%
  arrange(TRTAN)

write_outputs(DENOM, "DENOM", out_dir)

# ------------------------------------------------------------
# 2. AEPTD – unique subjects with serious AE per PT per treatment
AEPTD <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  group_by(.data[[treat_adae]], AEBODSYS, AEDECOD) %>%
  summarise(COUNT = n_distinct(USUBJID), .groups = "drop") %>%
  rename(TRTAN = !!sym(treat_adae)) %>%
  arrange(TRTAN, AEBODSYS, AEDECOD)

write_outputs(AEPTD, "AEPTD", out_dir)

# ------------------------------------------------------------
# 3. AEPTA – total serious AE events per PT per treatment
AEPTA <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  group_by(.data[[treat_adae]], AEBODSYS, AEDECOD) %>%
  summarise(EVENTS = n(), .groups = "drop") %>%
  rename(TRTAN = !!sym(treat_adae)) %>%
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
  group_by(.data[[treat_adae]], AEBODSYS) %>%
  summarise(COUNT = n_distinct(USUBJID), .groups = "drop") %>%
  rename(TRTAN = !!sym(treat_adae)) %>%
  arrange(TRTAN, AEBODSYS)

write_outputs(AESOCD, "AESOCD", out_dir)

# ------------------------------------------------------------
# 6. AESOCA – total serious AE events per SOC per treatment
AESOCA <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  group_by(.data[[treat_adae]], AEBODSYS) %>%
  summarise(EVENTS = n(), .groups = "drop") %>%
  rename(TRTAN = !!sym(treat_adae)) %>%
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
  group_by(.data[[treat_adae]]) %>%
  summarise(COUNT = n_distinct(USUBJID), .groups = "drop") %>%
  rename(TRTAN = !!sym(treat_adae)) %>%
  arrange(TRTAN)

write_outputs(AESUBD, "AESUBD", out_dir)

# ------------------------------------------------------------
# 9. AESUBA – total serious AE events per treatment
AESUBA <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  group_by(.data[[treat_adae]]) %>%
  summarise(EVENTS = n(), .groups = "drop") %>%
  rename(TRTAN = !!sym(treat_adae)) %>%
  arrange(TRTAN)

write_outputs(AESUBA, "AESUBA", out_dir)

# ------------------------------------------------------------
# 10. RESULTSUB – combine at treatment (subject) level
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
  if (all(dim(tab) == c(2, 2))) fisher.test(tab, alternative = "two.sided")$p.value else NA_real_
}

# ---- PT level ----------------------------------------------------
PT_BACKBONE <- ADSL %>%
  filter(SAFFL == "Y") %>%
  distinct(USUBJID, TRTAN = .data[[treat_adsl]])

PT_HADEVENT <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  distinct(STUDYID, USUBJID, TRTAN = .data[[treat_adae]], AEDECOD) %>%
  mutate(EVENT = if_else(STUDYID == "CDISCPILOT01", 1L, 0L))

PT_FISHER <- PT_BACKBONE %>%
  left_join(PT_HADEVENT, by = c("USUBJID", "TRTAN")) %>%
  replace_na(list(EVENT = 0L))

# High dose vs placebo (0 vs 2)
F1PT <- PT_FISHER %>%
  filter(TRTAN %in% c(0, 2)) %>%
  group_by(AEDECOD) %>%
  summarise(P_VALUE = fisher_pvalue(table(TRTAN, EVENT)), .groups = "drop")

# Low dose vs placebo (0 vs 1)
F2PT <- PT_FISHER %>%
  filter(TRTAN %in% c(0, 1)) %>%
  group_by(AEDECOD) %>%
  summarise(P_VALUE = fisher_pvalue(table(TRTAN, EVENT)), .groups = "drop")

write_outputs(F1PT, "F1PT", out_dir)
write_outputs(F2PT, "F2PT", out_dir)

# ---- SOC level ---------------------------------------------------
SOC_BACKBONE <- ADSL %>%
  filter(SAFFL == "Y") %>%
  distinct(USUBJID, TRTAN = .data[[treat_adsl]])

SOC_HADEVENT <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  distinct(STUDYID, USUBJID, TRTAN = .data[[treat_adae]], AEBODSYS) %>%
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

# ---- SUBJECT level -----------------------------------------------
SUB_HADEVENT <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  distinct(STUDYID, USUBJID, TRTAN = .data[[treat_adae]]) %>%
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

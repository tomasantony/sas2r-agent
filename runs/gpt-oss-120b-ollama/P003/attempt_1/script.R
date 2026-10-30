# P003_at14-5-02 translation to R
# ------------------------------------------------------------
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ------------------------------------------------------------
# Helper functions ------------------------------------------------
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx <- abs(x)
  factor <- 10 ^ digits
  rounded <- floor(absx * factor + 0.5) / factor
  posneg * rounded
}

# Write a data frame as CSV (SAS style) and as a plain‑text table
write_outputs <- function(df, name, out_dir) {
  csv_path <- file.path(out_dir, paste0(name, ".csv"))
  txt_path <- file.path(out_dir, paste0(name, ".txt"))
  write.csv(df, csv_path, row.names = FALSE, na = "")
  # plain‑text rendering: tab‑separated, same formatting as SAS PROC PRINT
  write.table(df, txt_path, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
}

# ------------------------------------------------------------
# Set up directories ------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# Read input data ---------------------------------------------------
# Paths are taken from the macro variables described in the SAS program
raw_adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
adsl_path   <- file.path(raw_adam_dir, "adsl.xpt")
adae_path   <- file.path(raw_adam_dir, "adae.xpt")

ADSL <- read_xpt(adsl_path) %>% as_tibble()
ADAE <- read_xpt(adae_path) %>% as_tibble()

# ------------------------------------------------------------
# 1. DENOM – number of subjects in safety population per treatment
DENOM <- ADSL %>%
  filter(SAFFL == "Y") %>%
  group_by(TRT01AN) %>%
  summarise(denom = n_distinct(USUBJID), .groups = "drop") %>%
  rename(TRTAN = TRT01AN) %>%
  arrange(TRTAN)

write_outputs(DENOM, "DENOM", out_dir)

# ------------------------------------------------------------
# 2. AEPTD – unique subjects with serious AE per PT per treatment
AEPTD <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  group_by(TRTAN = TRT01AN, AEBODSYS, AEDECOD) %>%
  summarise(count = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN, AEBODSYS, AEDECOD)

write_outputs(AEPTD, "AEPTD", out_dir)

# ------------------------------------------------------------
# 3. AEPTA – total number of serious AE events per PT per treatment
AEPTA <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  group_by(TRTAN = TRT01AN, AEBODSYS, AEDECOD) %>%
  summarise(events = n(), .groups = "drop") %>%
  arrange(TRTAN, AEBODSYS, AEDECOD)

write_outputs(AEPTA, "AEPTA", out_dir)

# ------------------------------------------------------------
# 4. RESULTPT – combine counts, percentages and events at PT level
RESULTPT <- AEPTD %>%
  left_join(DENOM, by = "TRTAN") %>%
  left_join(AEPTA, by = c("TRTAN", "AEBODSYS", "AEDECOD")) %>%
  mutate(
    percent = round_half_up((count / denom) * 100, 1),
    RESULT = paste0(
      count, " (", sprintf("%.1f", percent), "%) [", events, "]"
    ),
    SUMMARYLEVEL = 3L
  ) %>%
  select(AEBODSYS, AEDECOD, TRTAN, SUMMARYLEVEL, denom, events, RESULT) %>%
  arrange(AEBODSYS, AEDECOD, TRTAN)

write_outputs(RESULTPT, "RESULTPT", out_dir)

# ------------------------------------------------------------
# 5. AESOCD – unique subjects with serious AE per SOC per treatment
AESOCD <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  group_by(TRTAN = TRT01AN, AEBODSYS) %>%
  summarise(count = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN, AEBODSYS)

write_outputs(AESOCD, "AESOCD", out_dir)

# ------------------------------------------------------------
# 6. AESOCA – total serious AE events per SOC per treatment
AESOCA <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  group_by(TRTAN = TRT01AN, AEBODSYS) %>%
  summarise(events = n(), .groups = "drop") %>%
  arrange(TRTAN, AEBODSYS)

write_outputs(AESOCA, "AESOCA", out_dir)

# ------------------------------------------------------------
# 7. RESULTSOC – combine at SOC level
RESULTSOC <- AESOCD %>%
  left_join(DENOM, by = "TRTAN") %>%
  left_join(AESOCA, by = c("TRTAN", "AEBODSYS")) %>%
  mutate(
    percent = round_half_up((count / denom) * 100, 1),
    RESULT = paste0(
      count, " (", sprintf("%.1f", percent), "%) [", events, "]"
    ),
    SUMMARYLEVEL = 2L
  ) %>%
  select(AEBODSYS, TRTAN, SUMMARYLEVEL, denom, events, RESULT) %>%
  arrange(AEBODSYS, TRTAN)

write_outputs(RESULTSOC, "RESULTSOC", out_dir)

# ------------------------------------------------------------
# 8. AESUBD – unique subjects with serious AE per treatment
AESUBD <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  group_by(TRTAN = TRT01AN) %>%
  summarise(count = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN)

write_outputs(AESUBD, "AESUBD", out_dir)

# ------------------------------------------------------------
# 9. AESUBA – total serious AE events per treatment
AESUBA <- ADAE %>%
  filter(TRTEMFL == "Y", AESER == "Y") %>%
  group_by(TRTAN = TRT01AN) %>%
  summarise(events = n(), .groups = "drop") %>%
  arrange(TRTAN)

write_outputs(AESUBA, "AESUBA", out_dir)

# ------------------------------------------------------------
# 10. RESULTSUB – combine at subject level
RESULTSUB <- AESUBD %>%
  left_join(DENOM, by = "TRTAN") %>%
  left_join(AESUBA, by = "TRTAN") %>%
  mutate(
    percent = round_half_up((count / denom) * 100, 1),
    RESULT = paste0(
      count, " (", sprintf("%.1f", percent), "%) [", events, "]"
    ),
    SUMMARYLEVEL = 1L
  ) %>%
  select(TRTAN, SUMMARYLEVEL, denom, events, RESULT) %>%
  arrange(TRTAN)

write_outputs(RESULTSUB, "RESULTSUB", out_dir)

# ------------------------------------------------------------
# 11. Inferential statistics – Fisher exact tests
# Helper to run Fisher for a given grouping variable and treatment pairs
run_fisher <- function(df_backbone, df_hadevent, group_var, trt_pairs) {
  # Create backbone: all subjects × all levels of group_var
  backbone <- df_backbone %>%
    distinct(USUBJID, TRTAN) %>%
    distinct() %>%
    inner_join(
      df_hadevent %>% distinct(!!sym(group_var)),
      by = character()
    )
  # HADEVENT: observed events
  hadevent <- df_hadevent %>%
    mutate(EVENT = if_else(STUDYID == "CDISCPILOT01", 1L, 0L))
  # Merge to get EVENT flag (0 if missing)
  fisher_df <- backbone %>%
    left_join(hadevent, by = c("USUBJID", "TRTAN", group_var)) %>%
    mutate(EVENT = replace_na(EVENT, 0L))
  # Run Fisher for each level of group_var and each treatment pair
  results <- map_dfr(trt_pairs, function(pair) {
    trt_a <- pair[1]; trt_b <- pair[2]
    sub <- fisher_df %>%
      filter(TRTAN %in% c(trt_a, trt_b))
    # Split by level of group_var
    sub %>%
      group_by(!!sym(group_var)) %>%
      summarise(
        pvalue = {
          tab <- table(TRTAN, EVENT)
          if (all(dim(tab) == c(2,2))) {
            fisher.test(tab, alternative = "two.sided")$p.value
          } else {
            NA_real_
          }
        },
        .groups = "drop"
      ) %>%
      mutate(
        TRTAN_PAIR = paste0(trt_a, "_vs_", trt_b),
        .by = group_var
      )
  })
  results
}

# Prepare backbone and hadevent data sets
BACKBONE  <- ADSL %>% filter(SAFFL == "Y") %>% distinct(USUBJID, TRTAN = TRT01AN)
BACKBONE1 <- BACKBONE
BACKBONE2 <- BACKBONE

HADEVENT  <- ADAE %>% filter(TRTEMFL == "Y", AESER == "Y") %>%
  distinct(STUDYID, USUBJID, TRTAN = TRT01AN, AEDECOD)

HADEVENT1 <- ADAE %>% filter(TRTEMFL == "Y", AESER == "Y") %>%
  distinct(STUDYID, USUBJID, TRTAN = TRT01AN, AEBODSYS)

HADEVENT2 <- ADAE %>% filter(TRTEMFL == "Y", AESER == "Y") %>%
  distinct(STUDYID, USUBJID, TRTAN = TRT01AN)

# PT level Fisher tests
pt_fisher <- run_fisher(
  df_backbone = BACKBONE,
  df_hadevent = HADEVENT,
  group_var = "AEDECOD",
  trt_pairs = list(c(0,2), c(0,1))
)

# Rename columns to match SAS output style
F1PT <- pt_fisher %>% filter(TRTAN_PAIR == "0_vs_2") %>% select(AEDECOD, P_VALUE = pvalue)
F2PT <- pt_fisher %>% filter(TRTAN_PAIR == "0_vs_1") %>% select(AEDECOD, P_VALUE = pvalue)

write_outputs(F1PT, "F1PT", out_dir)
write_outputs(F2PT, "F2PT", out_dir)

# SOC level Fisher tests
soc_fisher <- run_fisher(
  df_backbone = BACKBONE1,
  df_hadevent = HADEVENT1,
  group_var = "AEBODSYS",
  trt_pairs = list(c(0,2), c(0,1))
)

F1SOC <- soc_fisher %>% filter(TRTAN_PAIR == "0_vs_2") %>% select(AEBODSYS, P_VALUE = pvalue)
F2SOC <- soc_fisher %>% filter(TRTAN_PAIR == "0_vs_1") %>% select(AEBODSYS, P_VALUE = pvalue)

write_outputs(F1SOC, "F1SOC", out_dir)
write_outputs(F2SOC, "F2SOC", out_dir)

# SUBJECT level Fisher tests
sub_fisher <- run_fisher(
  df_backbone = BACKBONE2,
  df_hadevent = HADEVENT2,
  group_var = "TRTAN",   # no by‑group variable; we just need overall p‑value
  trt_pairs = list(c(0,2), c(0,1))
)

# Because group_var is TRTAN, the result will have one row per TRTAN (which is not useful);
# Instead compute directly on the 2×2 table of treatment vs EVENT
calc_overall_fisher <- function(trt_a, trt_b) {
  tab <- table(
    ADAE %>% filter(TRTEMFL == "Y", AESER == "Y", TRT01AN %in% c(trt_a, trt_b)) %>%
      mutate(TRTAN = TRT01AN, EVENT = if_else(STUDYID == "CDISCPILOT01", 1L, 0L)) %>%
      select(TRTAN, EVENT)
  )
  if (all(dim(tab) == c(2,2))) fisher.test(tab)$p.value else NA_real_
}
F1SUB <- tibble(
  COMP = "0_vs_2",
  P_VALUE = calc_overall_fisher(0,2)
)
F2SUB <- tibble(
  COMP = "0_vs_1",
  P_VALUE = calc_overall_fisher(0,1)
)

write_outputs(F1SUB, "F1SUB", out_dir)
write_outputs(F2SUB, "F2SUB", out_dir)

# ------------------------------------------------------------
# 12. Assemble final listing (P003.lst)
# Concatenate the three RESULT tables in order of SUMMARYLEVEL
FINAL_LISTING <- bind_rows(
  RESULTSUB %>% mutate(Level = "SUBJECT"),
  RESULTSOC %>% mutate(Level = "SOC"),
  RESULTPT %>% mutate(Level = "PT")
) %>%
  select(Level, everything())

write_outputs(FINAL_LISTING, "P003", out_dir)   # writes P003.csv and P003.txt

# ------------------------------------------------------------
# End of script

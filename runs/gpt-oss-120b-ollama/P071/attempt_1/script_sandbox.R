# t_disp.R - translation of SAS program P071_t_disp.sas
# ----------------------------------------------------
# Reads ADaM ADSL dataset, creates Subject Disposition table,
# and writes CSV and plain‑text versions to OUT_DIR.

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ------------------------------------------------------------------
# Helper: round half up (SAS style) to given number of decimal places
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
}

# ------------------------------------------------------------------
# Paths
adam_dir   <- "/bench/data/raw/own-synthetic/adam"
adsl_path  <- file.path(adam_dir, "adsl.sas7bdat")   # ADaM ADSL dataset
out_dir    <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# ------------------------------------------------------------------
# 1. Load ADSL
adsl <- read_sas(adsl_path)

# Ensure character variables are plain strings
adsl <- adsl %>% mutate(across(where(is.character), ~ as.character(.)))

# ------------------------------------------------------------------
# 2. Big N (all randomized subjects) per treatment
bign_df <- adsl %>%
  filter(toupper(RANDFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)

# Total N (all randomized)
bigntot <- adsl %>% filter(toupper(RANDFL) == "Y") %>% summarise(cnt = n_distinct(USUBJID)) %>% pull(cnt)

# Build column header strings (e.g., "Placebo~(N=123)")
# SAS macro used "~" as split character; we keep it for consistency.
col_hdrs <- bign_df %>%
  mutate(hdr = paste0(TRTA, "~(N=", bign, ")")) %>%
  pull(hdr)

# Expect exactly three treatment groups; map to col1, col2, col3
if (length(col_hdrs) != 3) warning("Expected 3 treatment groups, found ", length(col_hdrs))
col1hdr <- col_hdrs[1]
col2hdr <- col_hdrs[2]
col3hdr <- col_hdrs[3]

# ------------------------------------------------------------------
# 3. Subset to randomized subjects
disp <- adsl %>% filter(RANDFL == "Y")

# ------------------------------------------------------------------
# 4. Population analysis set counts
pop_counts <- function(flag, label, ord) {
  disp %>%
    filter(toupper(.data[[flag]]) == "Y") %>%
    group_by(TRTA) %>%
    summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
    mutate(category = label,
           ord = ord)
}

pop_randfl <- pop_counts("RANDFL", "Randomized", 1)
pop_ittfl  <- pop_counts("ITTFL",  "  Intent-to-Treat Set", 2)
pop_saffl  <- pop_counts("SAFFL",  "  Safety Analysis Set", 3)
pop_efffl  <- pop_counts("EFFFL",  "  Efficacy Analysis Set", 4)
pop_pprotfl<- pop_counts("PPROTFL","  Per-Protocol Set", 5)

# ------------------------------------------------------------------
# 5. Completion / Discontinuation
completed <- disp %>%
  filter(EOSSTT == "COMPLETED") %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(category = "Completed Study", ord = 10)

discont <- disp %>%
  filter(EOSSTT == "DISCONTINUED") %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(category = "Discontinued Study", ord = 11)

# ------------------------------------------------------------------
# 6. Reasons for discontinuation
reasons <- disp %>%
  filter(EOSSTT == "DISCONTINUED", DCSREAS != "") %>%
  group_by(TRTA, DCSREAS) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(category = paste0("  ", str_trim(DCSREAS))) %>%
  mutate(ord = case_when(
    toupper(DCSREAS) == "ADVERSE EVENT"          ~ 12,
    toupper(DCSREAS) == "DEATH"                  ~ 13,
    toupper(DCSREAS) == "LACK OF EFFICACY"       ~ 14,
    toupper(DCSREAS) == "LOST TO FOLLOW-UP"     ~ 15,
    toupper(DCSREAS) == "PHYSICIAN DECISION"    ~ 16,
    toupper(DCSREAS) == "PROTOCOL VIOLATION"    ~ 17,
    toupper(DCSREAS) == "WITHDRAWAL BY SUBJECT" ~ 18,
    TRUE                                         ~ 19
  ))

# ------------------------------------------------------------------
# 7. Combine all sections
all_disp <- bind_rows(
  pop_randfl, pop_ittfl, pop_saffl, pop_efffl, pop_pprotfl,
  completed, discont, reasons
) %>% select(TRTA, cnt, category, ord)

# ------------------------------------------------------------------
# 8. Add percentages using Big N per treatment
all_disp2 <- all_disp %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(pct = if_else(bign > 0,
                       round_half_up(cnt / bign * 100, 1),
                       NA_real_))

# ------------------------------------------------------------------
# 9. Build display value column
all_disp3 <- all_disp2 %>%
  mutate(value = case_when(
    ord == 1 ~ as.character(cnt),                                 # only N for randomized
    cnt > 0  ~ paste0(cnt, " (", formatC(pct, format = "f", digits = 1), ")"),
    TRUE     ~ "0"
  )) %>%
  select(ord, category, TRTA, value)

# ------------------------------------------------------------------
# 10. Transpose to columnar layout (colPlacebo, colDrug_X_Low_Dose, colDrug_X_High_Dose)
# Ensure ordering of rows before pivot
all_disp3 <- all_disp3 %>% arrange(ord, category)

pivoted <- all_disp3 %>%
  pivot_wider(id_cols = c(ord, category),
              names_from = TRTA,
              values_from = value,
              names_prefix = "col") %>%
  # rename to expected column names (col1‑col3)
  rename(col1 = colPlacebo,
         col2 = colDrug_X_Low_Dose,
         col3 = colDrug_X_High_Dose)

# ------------------------------------------------------------------
# 11. Section headers and show_section logic
report <- pivoted %>%
  arrange(ord) %>%
  mutate(section = case_when(
    ord == 1  ~ "Analysis Populations",
    ord == 10 ~ "Study Completion",
    ord >= 12 ~ "  Reason for Discontinuation",
    TRUE      ~ ""
  )) %>%
  group_by(ord) %>%
  mutate(_show_section = if_else(section != lag(section, default = ""), section, "")) %>%
  ungroup() %>%
  select(ord, _show_section, category, col1, col2, col3)

# ------------------------------------------------------------------
# 12. Apply column header strings for the three treatment columns
col_names <- c("ord", "_show_section", "category", col1hdr, col2hdr, col3hdr)
names(report) <- col_names

# ------------------------------------------------------------------
# 13. Write outputs
# CSV version
write.csv(report,
          file = file.path(out_dir, "t_disp.csv"),
          row.names = FALSE,
          na = "")

# Plain‑text rendering (tab‑separated, no row names)
txt_path <- file.path(out_dir, "t_disp.txt")
write.table(report,
            file = txt_path,
            sep = "\t",
            row.names = FALSE,
            col.names = TRUE,
            quote = FALSE,
            na = "")

# ------------------------------------------------------------------
# End of script

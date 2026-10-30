# P068_t_ae_soc_pt.R
# Translation of SAS program t_ae_soc_pt.sas to R
# Generates:
#   t_ae_soc_pt.csv  – table data
#   t_ae_soc_pt.txt  – plain‑text rendering of the table
# Files are written to the directory given by the environment variable OUT_DIR

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# ------------------------------------------------------------------
# Helper: SAS‑style rounding (round half up, away from zero)
round_half_up <- function(x, digits = 0) {
  pos <- sign(x)
  absx <- abs(x) * 10^digits
  rounded <- floor(absx + 0.5)
  pos * rounded / 10^digits
}

# ------------------------------------------------------------------
# Paths (as defined in the SAS program)
adam_path <- "/bench/data/raw/own-synthetic/adam"
adsl_file <- file.path(adam_path, "adsl.sas7bdat")
adae_file <- file.path(adam_path, "adae.sas7bdat")

# Output directory (provided by the harness)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") out_dir <- "."

# ------------------------------------------------------------------
# Load ADaM datasets
adsl <- read_sas(adsl_file) %>%
  mutate(
    USUBJID = as.character(USUBJID),
    TRTA    = as.character(TRTA),
    SAFFL   = as.character(SAFFL)
  )

adae <- read_sas(adae_file) %>%
  mutate(
    USUBJID   = as.character(USUBJID),
    TRTA      = as.character(TRTA),
    TRTEMFL   = as.character(TRTEMFL),
    AEBODSYS  = as.character(AEBODSYS),
    AEDECOD   = as.character(AEDECOD),
    AESOC     = as.character(AESOC),
    AESOCCD   = as.character(AESOCCD)
  )

# ------------------------------------------------------------------
# 1. Big N per treatment (SAFFL = 'Y')
bign_df <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)                     # SAS orders by character value

bigntot <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  summarise(bign = n_distinct(USUBJID)) %>%
  pull(bign)

# Build column headers (SAS uses ~ as line‑break marker)
col_hdrs <- paste0(bign_df$TRTA, "~(N=", bign_df$bign, ")")
col1hdr <- col_hdrs[1]
col2hdr <- ifelse(length(col_hdrs) >= 2, col_hdrs[2], "")
col3hdr <- ifelse(length(col_hdrs) >= 3, col_hdrs[3], "")

# ------------------------------------------------------------------
# 2. Subset to TEAE in safety population (SAFFL='Y' and TRTEMFL='Y')
ae <- adae %>%
  inner_join(adsl %>% select(USUBJID, SAFFL), by = "USUBJID") %>%
  filter(SAFFL == "Y", TRTEMFL == "Y") %>%
  select(USUBJID, TRTA, AEBODSYS, AEDECOD)

# ------------------------------------------------------------------
# 3. Count subjects by SOC (per treatment)
soc_cnt <- ae %>%
  group_by(TRTA, AEBODSYS) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

soc_tot <- ae %>%
  group_by(AEBODSYS) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

# ------------------------------------------------------------------
# 4. Count subjects by SOC + PT (per treatment)
pt_cnt <- ae %>%
  group_by(TRTA, AEBODSYS, AEDECOD) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

pt_tot <- ae %>%
  group_by(AEBODSYS, AEDECOD) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

# ------------------------------------------------------------------
# 5. Any TEAE rows (one per treatment)
any_ae <- ae %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(
    AEBODSYS = "Any TEAE",
    AEDECOD  = "",
    soc_ord  = 0,
    pt_ord   = 0,
    rowtype  = 0
  ) %>%
  select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype)

# ------------------------------------------------------------------
# 6. SOC rows (with sort order)
soc_rows <- soc_cnt %>%
  left_join(soc_tot, by = "AEBODSYS") %>%
  mutate(
    soc_ord = cnt_tot,   # used for descending sort
    pt_ord  = 0,
    rowtype = 1,
    AEDECOD = ""
  ) %>%
  select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype)

# ------------------------------------------------------------------
# 7. PT rows (with sort order)
pt_rows <- pt_cnt %>%
  left_join(pt_tot, by = c("AEBODSYS", "AEDECOD")) %>%
  left_join(soc_tot, by = "AEBODSYS", suffix = c(".pt", ".soc")) %>%
  mutate(
    soc_ord = cnt_tot.soc,
    pt_ord  = cnt_tot.pt,
    rowtype = 2
  ) %>%
  select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype)

# ------------------------------------------------------------------
# 8. Combine all rows
combined <- bind_rows(any_ae, soc_rows, pt_rows)

# ------------------------------------------------------------------
# 9. Merge with Big N and compute percentages
combined2 <- combined %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(
    pct = ifelse(bign > 0, cnt / bign * 100, NA_real_)
  )

# ------------------------------------------------------------------
# 10. Create display value (cnt (pct) ) – SAS style rounding, no % sign
combined3 <- combined2 %>%
  mutate(
    value = if (cnt > 0) {
      pct_rounded <- round_half_up(pct, 1)
      paste0(cnt, " (", sprintf("%.1f", pct_rounded), ")")
    } else {
      "0"
    }
  )

# ------------------------------------------------------------------
# 11. Sort exactly as SAS PROC SQL ORDER BY
combined3 <- combined3 %>%
  arrange(
    desc(soc_ord),
    AEBODSYS,
    rowtype,
    desc(pt_ord),
    AEDECOD,
    TRTA
  )

# ------------------------------------------------------------------
# 12. Pivot to wide format (one column per treatment)
wide <- combined3 %>%
  select(TRTA, rowtype, soc_ord, pt_ord, AEBODSYS, AEDECOD, value) %>%
  pivot_wider(
    id_cols = c(rowtype, soc_ord, pt_ord, AEBODSYS, AEDECOD),
    names_from = TRTA,
    values_from = value,
    values_fill = "0"
  )

# Ensure treatment columns appear in the same order as the original labels
treat_order <- bign_df$TRTA
wide <- wide %>%
  select(
    rowtype, soc_ord, pt_ord, AEBODSYS, AEDECOD,
    all_of(treat_order)
  )

# Rename treatment columns to col1, col2, col3 (as expected by the report)
rename_map <- setNames(paste0("col", seq_along(treat_order)), treat_order)
wide <- rename(wide, !!!rename_map)

# ------------------------------------------------------------------
# 13. Build final report dataset
report <- wide %>%
  mutate(
    display_term = case_when(
      rowtype == 0 ~ AEBODSYS,                     # Any TEAE
      rowtype == 1 ~ AEBODSYS,                     # SOC (bold)
      TRUE        ~ paste0("  ", AEDECOD)          # PT (indented)
    )
  ) %>%
  select(display_term, starts_with("col"))

# ------------------------------------------------------------------
# 14. Write CSV output
csv_path <- file.path(out_dir, "t_ae_soc_pt.csv")
write_csv(report, csv_path, na = "")

# ------------------------------------------------------------------
# 15. Write plain‑text rendering (fixed‑width columns)
txt_path <- file.path(out_dir, "t_ae_soc_pt.txt")
header_line <- sprintf(
  "%-45s%15s%15s%15s",
  "System Organ Class  Preferred Term",
  col1hdr, col2hdr, col3hdr
)

format_row <- function(term, c1, c2, c3) {
  sprintf("%-45s%15s%15s%15s", term, c1, c2, c3)
}

rows_txt <- apply(report, 1, function(r) {
  format_row(r["display_term"], r["col1"], r["col2"], r["col3"])
})

writeLines(c(header_line, rows_txt), txt_path)

# ------------------------------------------------------------------
# End of script

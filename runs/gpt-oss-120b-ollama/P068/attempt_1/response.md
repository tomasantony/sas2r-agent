```r
# P068_t_ae_soc_pt.R
# Translation of SAS program t_ae_soc_pt.sas to R
# Generates t_ae_soc_pt.csv and t_ae_soc_pt.txt in OUT_DIR

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Helper: round half up (SAS default)
round_half_up <- function(x, digits = 0) {
  pos <- sign(x)
  absx <- abs(x) * 10^digits
  rounded <- floor(absx + 0.5)
  pos * rounded / 10^digits
}

# Paths ---------------------------------------------------------------
adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
adsl_file <- file.path(adam_path, "adsl.sas7bdat")
adae_file <- file.path(adam_path, "adae.sas7bdat")

# Output directory
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") out_dir <- "."

# Load datasets ---------------------------------------------------------
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

# 1. Big N per treatment (SAFFL='Y') ----------------------------------
bign_df <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)   # SAS orders by TRTA (character, case‑sensitive)

# Total N
bigntot <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  summarise(bign = n_distinct(USUBJID)) %>%
  pull(bign)

# Build column headers (use ~ as line‑break marker like SAS)
trt_labels <- bign_df$TRTA
bign_vals  <- bign_df$bign
col_hdrs   <- paste0(trt_labels, "~(N=", bign_vals, ")")
col1hdr <- col_hdrs[1]
col2hdr <- ifelse(length(col_hdrs) >= 2, col_hdrs[2], "")
col3hdr <- ifelse(length(col_hdrs) >= 3, col_hdrs[3], "")

# 2. Subset to TEAE in safety population -------------------------------
ae <- adae %>%
  inner_join(adsl %>% select(USUBJID, SAFFL), by = "USUBJID") %>%
  filter(SAFFL == "Y", TRTEMFL == "Y") %>%
  select(USUBJID, TRTA, AEBODSYS, AEDECOD)

# 3. Count subjects by SOC ---------------------------------------------
soc_cnt <- ae %>%
  group_by(TRTA, AEBODSYS) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

soc_tot <- ae %>%
  group_by(AEBODSYS) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

# 4. Count subjects by SOC + PT ----------------------------------------
pt_cnt <- ae %>%
  group_by(TRTA, AEBODSYS, AEDECOD) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

pt_tot <- ae %>%
  group_by(AEBODSYS, AEDECOD) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

# 5. Any TEAE row ------------------------------------------------------
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

# 6. Build SOC rows -----------------------------------------------------
soc_rows <- soc_cnt %>%
  left_join(soc_tot, by = "AEBODSYS") %>%
  mutate(
    soc_ord = cnt_tot,   # sort order (descending later)
    pt_ord  = 0,
    rowtype = 1,
    AEDECOD = ""
  ) %>%
  select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype)

# PT rows ---------------------------------------------------------------
pt_rows <- pt_cnt %>%
  left_join(pt_tot, by = c("AEBODSYS", "AEDECOD")) %>%
  left_join(soc_tot, by = "AEBODSYS") %>%
  mutate(
    soc_ord = cnt_tot.y,   # from soc_tot
    pt_ord  = cnt_tot.x,   # from pt_tot
    rowtype = 2
  ) %>%
  select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype)

# 7. Combine -----------------------------------------------------------
combined <- bind_rows(any_ae, soc_rows, pt_rows)

# Merge with big N to compute percentages
combined2 <- combined %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(
    pct = ifelse(bign > 0, cnt / bign * 100, NA_real_)
  )

# 8. Create display value ------------------------------------------------
combined3 <- combined2 %>%
  mutate(
    value = ifelse(cnt > 0,
                   paste0(cnt, " (", sprintf("%.1f", round_half_up(pct, 1)), "%)"),
                   "0")
  )

# 9. Sort as SAS does
combined3 <- combined3 %>%
  arrange(
    desc(soc_ord),
    AEBODSYS,
    rowtype,
    desc(pt_ord),
    AEDECOD,
    TRTA
  )

# 10. Pivot to wide format (columns per treatment) --------------------
wide <- combined3 %>%
  select(TRTA, rowtype, soc_ord, pt_ord, AEBODSYS, AEDECOD, value) %>%
  pivot_wider(
    id_cols = c(rowtype, soc_ord, pt_ord, AEBODSYS, AEDECOD),
    names_from = TRTA,
    values_from = value,
    values_fill = "0"
  )

# Ensure columns are in the same order as original treatment labels
col_order <- trt_labels
wide <- wide %>%
  select(
    rowtype, soc_ord, pt_ord, AEBODSYS, AEDECOD,
    all_of(col_order)
  )

# Rename treatment columns to col1, col2, col3
rename_map <- setNames(paste0("col", seq_along(col_order)), col_order)
wide <- rename(wide, !!!rename_map)

# 11. Build final report dataset -----------------------------------------
report <- wide %>%
  mutate(
    display_term = case_when(
      rowtype == 0 ~ AEBODSYS,                     # Any TEAE
      rowtype == 1 ~ AEBODSYS,                     # SOC (bold)
      TRUE        ~ paste0("  ", AEDECOD)          # PT (indented)
    )
  ) %>%
  select(display_term, starts_with("col"))

# 12. Write CSV ---------------------------------------------------------
csv_path <- file.path(out_dir, "t_ae_soc_pt.csv")
write_csv(report, csv_path, na = "")

# 13. Write plain‑text rendering -----------------------------------------
txt_path <- file.path(out_dir, "t_ae_soc_pt.txt")
header_line <- sprintf("%-45s%15s%15s%15s",
                       "System Organ Class  Preferred Term",
                       col1hdr, col2hdr, col3hdr)

# Helper to format each row
format_row <- function(term, c1, c2, c3) {
  sprintf("%-45s%15s%15s%15s", term, c1, c2, c3)
}

rows_txt <- apply(report, 1, function(r) {
  format_row(r["display_term"], r["col1"], r["col2"], r["col3"])
})

txt_content <- c(header_line, rows_txt)
writeLines(txt_content, txt_path)

# End of script
```
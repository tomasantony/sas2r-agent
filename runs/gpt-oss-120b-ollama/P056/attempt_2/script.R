# P056_outliers_TEHiLo_Anytime.R
# Reimplementation of SAS program P056_outliers_TEHiLo_Anytime.sas
# Generates the listing P056.lst (as text and CSV) in the directory given by OUT_DIR

library(haven)
library(dplyr)
library(tidyr)
library(stringr)

# ----------------------------------------------------------------------
# Helper: round half up (SAS default rounding)
round_half_up <- function(x, digits = 0) {
  pos <- x >= 0
  factor <- 10 ^ digits
  res <- ifelse(pos,
                floor(x * factor + 0.5) / factor,
                ceiling(x * factor - 0.5) / factor)
  res
}

# Helper: SAS style p‑value formatting (pvalue5.3)
fmt_pvalue <- function(p) {
  if (is.na(p)) {
    return("")
  } else if (p < 0.001) {
    return("<.001")
  } else {
    # sprintf gives no leading blanks; SAS would pad to width 5, but strip removes them
    sprintf("%.3f", p)
  }
}

# ----------------------------------------------------------------------
# Directories
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
inlib_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"

# ----------------------------------------------------------------------
# Read ADVS dataset (ADaM)
advs_path <- file.path(inlib_dir, "advs.sas7bdat")
advs <- read_sas(advs_path) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL,
         CRIT1, CRIT1FL, CRIT2, CRIT2FL, ATPTN, ADY, SAFFL)

# ----------------------------------------------------------------------
# Apply selection criteria (same as SAS WHERE)
advs <- advs %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ADY > 1,
         SAFFL == "Y",
         !is.na(CRIT1FL) | !is.na(CRIT2FL))

# ----------------------------------------------------------------------
# Totals for Low (CRIT1FL)
totn_low <- advs %>%
  filter(!is.na(CRIT1FL)) %>%
  distinct(TRTPN, PARAM, USUBJID) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(totn = n(), .groups = "drop") %>%
  mutate(tefl = "Low")

n_low <- advs %>%
  filter(CRIT1FL == "Y") %>%
  distinct(TRTPN, PARAM, USUBJID) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(tefl = "Low")

# ----------------------------------------------------------------------
# Totals for High (CRIT2FL)
totn_high <- advs %>%
  filter(!is.na(CRIT2FL)) %>%
  distinct(TRTPN, PARAM, USUBJID) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(totn = n(), .groups = "drop") %>%
  mutate(tefl = "High")

n_high <- advs %>%
  filter(CRIT2FL == "Y") %>%
  distinct(TRTPN, PARAM, USUBJID) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(tefl = "High")

# ----------------------------------------------------------------------
# Combine totals
totn <- bind_rows(totn_high, totn_low) %>% arrange(TRTPN, PARAM)
te_n <- bind_rows(n_high, n_low) %>% arrange(TRTPN, PARAM)

# ----------------------------------------------------------------------
# Merge totals and compute percent (SAS rounding)
all_n <- totn %>%
  left_join(te_n, by = c("TRTPN", "PARAM", "tefl")) %>%
  mutate(
    n = ifelse(is.na(n), 0, n),
    pct = round_half_up(100 * n / totn, 1)   # one decimal, SAS‑style rounding
  )

# ----------------------------------------------------------------------
# Fisher exact p‑values (replicating the macro)
fisher_pval <- function(flag, tefl_label) {
  # Sort by USUBJID and flag, keep last record per subject (as SAS BY ... LAST.)
  flag_df <- advs %>%
    arrange(USUBJID, .data[[flag]]) %>%
    group_by(USUBJID) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  # Build contingency table: flag (Y vs not Y) by TRTPN
  tab <- xtabs(~ get(flag) + TRTPN, data = flag_df)
  ft <- fisher.test(tab)
  tibble(tefl = tefl_label, nvalue1 = ft$p.value)
}

fisher_low  <- fisher_pval("CRIT1FL", "Low")
fisher_high <- fisher_pval("CRIT2FL", "High")
all_fisher <- bind_rows(fisher_low, fisher_high)

# ----------------------------------------------------------------------
# Assemble final table values
table_vals <- all_n %>%
  left_join(all_fisher, by = "tefl") %>%
  mutate(
    pctc = paste0("(", sprintf("%.1f", pct), ")"),
    nc   = as.character(n)
  ) %>%
  # Order as in SAS: param descending, then tefl, then trtpn
  arrange(desc(PARAM), tefl, TRTPN)

# Create formatted p‑value column – only first occurrence per param/tefl/nvalue1
table_vals <- table_vals %>%
  group_by(PARAM, tefl, nvalue1) %>%
  mutate(
    nvalue1c = ifelse(row_number() == 1, fmt_pvalue(nvalue1), "")
  ) %>%
  ungroup()

# ----------------------------------------------------------------------
# Output CSV of the final listing (matches variable names)
csv_path <- file.path(out_dir, "P056.csv")
write.csv(table_vals,
          csv_path,
          row.names = FALSE,
          na = "")

# ----------------------------------------------------------------------
# Plain‑text rendering (PROC REPORT style)
txt_path <- file.path(out_dir, "P056.txt")
col_names <- c("Vital Sign (unit)", "Abnormality Direction",
               "Treatment", "N", "n", "(%)", "P value*")
header_line <- paste(col_names, collapse = "  ")

# Build data rows
row_lines <- apply(table_vals, 1, function(r) {
  paste(
    r["PARAM"],
    r["tefl"],
    r["TRTPN"],
    r["totn"],
    r["nc"],
    r["pctc"],
    r["nvalue1c"],
    sep = "  "
  )
})

txt_contents <- c(
  "Vital Signs",
  "Treatment‑Emergent Abnormal High or Low at Any Time",
  header_line,
  row_lines,
  "",
  "Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`) baseline and at least one post‑baseline measure, n = number of patients wtih an abnormal post‑baseline result in the specified category.",
  "*P values are from Fisher's Exact test."
)

writeLines(txt_contents, txt_path)

# ----------------------------------------------------------------------
# End of script

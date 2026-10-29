```r
# P037_target21.R
# Reimplementation of SAS program P037_target21.sas in R
# ----------------------------------------------------
# Required libraries
library(haven)
library(dplyr)
library(tidyr)
library(stringr)

# Helper: round half away from zero (SAS default)
round_half_up <- function(x, digits = 0) {
  sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
}

# ------------------------------------------------------------------
# Paths
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# ------------------------------------------------------------------
# 1. Read ADVS data
advs <- read_xpt(file.path(rawadam, "advs.xpt"))

# 2. Apply record selection
advs <- advs %>%
  filter(
    PARAMCD == "DIABP",
    ATPTN == 815,
    ADY > 1,
    SAFFL == "Y",
    !is.na(CRIT1FL) | !is.na(CRIT2FL)
  ) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL,
         CRIT1, CRIT1FL, CRIT2, CRIT2FL)

# ------------------------------------------------------------------
# 3. Totals and counts for Low / High abnormality direction
# Low (CRIT1FL)
totn_low <- advs %>%
  filter(!is.na(CRIT1FL)) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(totn = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(tefl = "Low")

n_low <- advs %>%
  filter(CRIT1FL == "Y") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(tefl = "Low")

# High (CRIT2FL)
totn_high <- advs %>%
  filter(!is.na(CRIT2FL)) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(totn = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(tefl = "High")

n_high <- advs %>%
  filter(CRIT2FL == "Y") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(tefl = "High")

# Combine
totn <- bind_rows(totn_high, totn_low) %>% arrange(TRTPN, PARAM)
te_n <- bind_rows(n_high, n_low) %>% arrange(TRTPN, PARAM)

# Merge totals and counts
all_n <- totn %>%
  left_join(te_n, by = c("TRTPN", "PARAM", "tefl")) %>%
  mutate(pct = round_half_up(100 * n / totn, 1))

# ------------------------------------------------------------------
# 4. Fisher Exact p‑values
compute_fisher <- function(df_flag, flag_name, tefl_label) {
  # Keep last record per USUBJID (sorted by USUBJID then flag)
  df_last <- df_flag %>%
    arrange(USUBJID, .data[[flag_name]]) %>%
    group_by(USUBJID) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  # For each PARAM compute Fisher test of TRTPN vs flag
  pvals <- df_last %>%
    group_by(PARAM) %>%
    summarise(
      nvalue1 = {
        tab <- table(TRTPN, .data[[flag_name]])
        # If table is degenerate, return NA
        if (any(dim(tab) < 2) || sum(tab) == 0) NA_real_ else fisher.test(tab)$p.value
      },
      .groups = "drop"
    ) %>%
    mutate(tefl = tefl_label)
  pvals
}

fisher_low  <- compute_fisher(advs, "CRIT1FL", "Low")
fisher_high <- compute_fisher(advs, "CRIT2FL", "High")
all_fisher <- bind_rows(fisher_low, fisher_high) %>% arrange(tefl)

# ------------------------------------------------------------------
# 5. Merge Fisher p‑values with N/n data
table_vals <- all_n %>%
  left_join(all_fisher, by = "tefl") %>%
  mutate(
    pctc = paste0("(", format(round_half_up(pct, 1), nsmall = 1, trim = TRUE), ")"),
    nc   = as.character(n),
    # Prepare formatted p‑value (will be blank for all but first per PARAM/tefl)
    nvalue1c_raw = ifelse(is.na(nvalue1), "", sprintf("%.3f", nvalue1))
  ) %>%
  arrange(PARAM, desc(tefl), nvalue1) %>%
  group_by(PARAM, tefl) %>%
  mutate(
    nvalue1c = ifelse(row_number() == 1, nvalue1c_raw, "")
  ) %>%
  ungroup() %>%
  select(
    param = PARAM,
    tefl,
    trtpn = TRTPN,
    totn,
    nc,
    pctc,
    nvalue1c
  )

# ------------------------------------------------------------------
# 6. Write outputs
# 6a. CSV of the final table (dataset output)
write.csv(
  table_vals,
  file = file.path(out_dir, "table_vals.csv"),
  row.names = FALSE,
  na = ""
)

# 6b. Plain‑text rendering of the PROC REPORT table (listing output)
# Define column widths matching the PROC REPORT style definitions
col_widths <- c(
  param = 30,
  tefl  = 6,
  trtpn = 12,
  totn  = 6,
  nc    = 6,
  pctc  = 8,
  nvalue1c = 10
)

# Header line (split on "|")
header <- sprintf(
  "%-*s %-*s %-*s %-*s %-*s %-*s %-*s",
  col_widths["param"], "Vital Sign (unit)",
  col_widths["tefl"],  "Abnormality Direction",
  col_widths["trtpn"], "Treatment",
  col_widths["totn"],  "N",
  col_widths["nc"],    "n",
  col_widths["pctc"],  "(%)",
  col_widths["nvalue1c"], "P value*"
)

# Build body lines
body_lines <- apply(table_vals, 1, function(row) {
  sprintf(
    "%-*s %-*s %-*s %-*s %-*s %-*s %-*s",
    col_widths["param"], row["param"],
    col_widths["tefl"],  row["tefl"],
    col_widths["trtpn"], row["trtpn"],
    col_widths["totn"],  as.character(row["totn"]),
    col_widths["nc"],    row["nc"],
    col_widths["pctc"],  row["pctc"],
    col_widths["nvalue1c"], row["nvalue1c"]
  )
})

# Footer notes (as in PROC REPORT compute block)
footer <- c(
  "Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)",
  "baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the",
  "specified category.",
  "*P values are from Fisher's Exact test."
)

txt_output <- c(header, body_lines, "", footer)

writeLines(txt_output, con = file.path(out_dir, "P037.lst"))

# Also write CSV of the table body (without headers) for completeness
write.csv(
  table_vals,
  file = file.path(out_dir, "P037_table_body.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# End of script
```
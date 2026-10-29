```r
# P037_target21.R
# Reimplementation of SAS program P037_target21.sas in R
# ----------------------------------------------------
# Packages (pre‑installed)
library(haven)
library(dplyr)
library(stringr)

# ------------------------------------------------------------------
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

# 2. Record selection (same as SAS WHERE)
advs <- advs %>%
  filter(
    PARAMCD == "DIABP",
    ATPTN == 815,
    ADY > 1,
    SAFFL == "Y",
    !is.na(CRIT1FL) | !is.na(CRIT2FL)
  ) %>%
  select(
    USUBJID, TRTPN, PARAM, PARAMCD, AVAL,
    CRIT1, CRIT1FL, CRIT2, CRIT2FL
  )

# ------------------------------------------------------------------
# 3. Totals (N) and counts (n) for Low / High directions
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

# Combine totals and counts
totn <- bind_rows(totn_high, totn_low) %>% arrange(TRTPN, PARAM)
te_n <- bind_rows(n_high, n_low) %>% arrange(TRTPN, PARAM)

# Merge totals with counts and compute percent
all_n <- totn %>%
  left_join(te_n, by = c("TRTPN", "PARAM", "tefl")) %>%
  mutate(pct = round_half_up(100 * n / totn, 1))

# ------------------------------------------------------------------
# 4. Fisher Exact p‑values (replicates PROC FREQ with /FISHER)
compute_fisher <- function(df, flag_var, tefl_label) {
  # Keep last record per USUBJID (sorted by flag)
  df_last <- df %>%
    arrange(USUBJID, .data[[flag_var]]) %>%
    group_by(USUBJID) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  # Fisher test per PARAM (2‑way table TRTPN x flag)
  pvals <- df_last %>%
    group_by(PARAM) %>%
    summarise(
      nvalue1 = {
        tab <- table(TRTPN, .data[[flag_var]])
        if (any(dim(tab) < 2) || sum(tab) == 0) NA_real else fisher.test(tab)$p.value
      },
      .groups = "drop"
    ) %>%
    mutate(tefl = tefl_label)
  pvals
}

fisher_low  <- compute_fisher(advs, "CRIT1FL", "Low")
fisher_high <- compute_fisher(advs, "CRIT2FL", "High")
all_fisher  <- bind_rows(fisher_low, fisher_high) %>% arrange(tefl)

# ------------------------------------------------------------------
# 5. Build final reporting table (same logic as SAS DATA steps)
table_vals <- all_n %>%
  left_join(all_fisher, by = "tefl") %>%          # merge by TE direction only
  mutate(
    pctc = paste0("(", format(round_half_up(pct, 1), nsmall = 1, trim = TRUE), ")"),
    nc   = as.character(n),
    nvalue1c_raw = ifelse(is.na(nvalue1), "", sprintf("%.3f", nvalue1))
  ) %>%
  arrange(.data$PARAM, desc(tefl), nvalue1) %>%   # SAS sorts by PARAM, descending TEFL, then p‑value
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
  file = file.path(out_dir, "P037_table.csv"),
  row.names = FALSE,
  na = ""
)

# 6b. Plain‑text rendering of the PROC REPORT table (listing output)
# Column widths approximating the PROC REPORT STYLE definitions
col_w <- c(param = 30, tefl = 6, trtpn = 12, totn = 6, nc = 6, pctc = 8, nvalue1c = 10)

header <- sprintf(
  "%-*s %-*s %-*s %-*s %-*s %-*s %-*s",
  col_w["param"], "Vital Sign (unit)",
  col_w["tefl"],  "Abnormality Direction",
  col_w["trtpn"], "Treatment",
  col_w["totn"],  "N",
  col_w["nc"],    "n",
  col_w["pctc"],  "(%)",
  col_w["nvalue1c"], "P value*"
)

body <- apply(table_vals, 1, function(r) {
  sprintf(
    "%-*s %-*s %-*s %-*s %-*s %-*s %-*s",
    col_w["param"], r["param"],
    col_w["tefl"],  r["tefl"],
    col_w["trtpn"], r["trtpn"],
    col_w["totn"],  as.character(r["totn"]),
    col_w["nc"],    r["nc"],
    col_w["pctc"],  r["pctc"],
    col_w["nvalue1c"], r["nvalue1c"]
  )
})

footer <- c(
  "Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)",
  "baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the",
  "specified category.",
  "*P values are from Fisher's Exact test."
)

txt_lines <- c(header, body, "", footer)

# Write the listing (text) – name matches SAS output P037.lst
writeLines(txt_lines, con = file.path(out_dir, "P037.lst.txt"))

# Also write a CSV version of the listing body for easy checking
write.csv(
  table_vals,
  file = file.path(out_dir, "P037.lst.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# End of script
```
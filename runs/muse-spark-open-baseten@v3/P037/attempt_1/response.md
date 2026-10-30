```r
out <- Sys.getenv("OUT_DIR")
if (!nzchar(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_pct1 <- function(x) {
  # SAS put(pct,5.1) with strip -> round half away from zero, 1 decimal
  if (is.na(x)) return(NA_character_)
  v <- round_half_up(x, 1)
  sprintf("(%.1f)", v)
}
fmt_pvalue <- function(p, d = 3) {
  # SAS pvalue5.3 : if p < 10^-d then "<.001" etc, else w.d with leading 0
  if (is.na(p)) return("")
  thresh <- 10^(-d)
  # SAS rounds before threshold check
  pr <- round_half_up(p, d)
  if (pr < thresh && p < thresh) {
    # construct "<.001"
    paste0("<.", paste0(rep("0", d - 1), collapse = ""), "1")
  } else if (pr > 1 - thresh/10) {
    # not needed for this data
    sprintf(paste0("%.", d, "f"), pr)
  } else {
    sprintf(paste0("%.", d, "f"), pr)
  }
}

# locate input ----------------------------------------------------------
rawadam_env <- Sys.getenv("RAWADAM")
rawadam <- if (nzchar(rawadam_env)) rawadam_env else "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
inlib_env <- Sys.getenv("INLIB")
# SAS program uses &rawadam./advs.xpt
xpt_path <- file.path(rawadam, "advs.xpt")
if (!file.exists(xpt_path)) {
  # fallback to derived adam
  xpt_path <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt"
  if (!file.exists(xpt_path)) {
    # try haven pharmaversesdtm
    xpt_path <- file.path(rawadam, "advs.xpt")
  }
}
# also try OUT_DIR relative
if (!file.exists(xpt_path)) {
  alt <- file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt")
  if (file.exists(alt)) xpt_path <- alt
}

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

advs_raw <- haven::read_xpt(xpt_path)

# ensure column names upper case as in SAS
names(advs_raw) <- toupper(names(advs_raw))

# keep only needed, filter as SAS WHERE
# WHERE PARAMCD="DIABP" and ATPTN=815 and ADY > 1 and SAFFL="Y" and (not missing(CRIT1FL) or not missing(CRIT2FL))
advs <- advs_raw %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ADY > 1,
         SAFFL == "Y",
         (!is.na(CRIT1FL) & CRIT1FL != "") | (!is.na(CRIT2FL) & CRIT2FL != "")) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)

# helper to count distinct USUBJID
count_distinct_usubjid <- function(df) n_distinct(df$USUBJID)

# totn_low : where not missing(crit1fl) -> Low
totn_low <- advs %>%
  filter(!is.na(CRIT1FL) & CRIT1FL != "") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(TOTN = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "Low")

n_low <- advs %>%
  filter(CRIT1FL == "Y") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(N = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "Low")

totn_high <- advs %>%
  filter(!is.na(CRIT2FL) & CRIT2FL != "") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(TOTN = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "High")

n_high <- advs %>%
  filter(CRIT2FL == "Y") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(N = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "High")

totn <- bind_rows(totn_high, totn_low)
te_n <- bind_rows(n_high, n_low)

# sort as SAS
totn <- totn %>% arrange(TRTPN, PARAM)
te_n <- te_n %>% arrange(TRTPN, PARAM)

# Combine N and n datasets, calculate percent
all_n <- full_join(totn, te_n, by = c("TRTPN", "PARAM", "TEFL")) %>%
  mutate(PCT = N / TOTN * 100)

# Fisher Exact p-value --------------------------------------------------
# Emulate macro: sort by USUBJID &flg, take last.USUBJID
get_fisher <- function(flg_col, tefl_label) {
  # flg_col is "CRIT1FL" or "CRIT2FL"
  # SAS sort: by usubjid &flg ; missing sorts first ("" < "N" < "Y")
  # In R, NA and "" should sort first
  df <- advs %>%
    select(USUBJID, TRTPN, PARAM, all_of(flg_col)) %>%
    mutate(.flg = .data[[flg_col]],
           .flg_sort = ifelse(is.na(.flg) | .flg == "", "", .flg))
  # arrange by USUBJID, flg_sort
  df <- df %>% arrange(USUBJID, .flg_sort)
  # last per USUBJID
  last_df <- df %>%
    group_by(USUBJID) %>%
    slice_tail(n = 1) %>%
    ungroup()
  # PROC FREQ tables param*trtpn*&flg/fisher ; missing excluded by default
  # So filter out missing flg
  freq_df <- last_df %>% filter(!is.na(.flg) & .flg != "")
  # Build contingency table TRTPN x FLG (stratified by PARAM, but PARAM is single)
  # For each PARAM level compute fisher
  params <- unique(freq_df$PARAM)
  res <- lapply(params, function(p) {
    sub <- freq_df %>% filter(PARAM == p)
    tab <- table(sub$TRTPN, sub$.flg)
    # Ensure at least 2x2; if not, fisher.test still works
    # Use fisher.test with default two-sided
    ft <- tryCatch(fisher.test(tab), error = function(e) NULL)
    pval <- if (!is.null(ft)) ft$p.value else NA_real_
    tibble(PARAM = p, TEFL = tefl_label, NVALUE1 = pval)
  })
  bind_rows(res)
}

fisher_low <- get_fisher("CRIT1FL", "Low")
fisher_high <- get_fisher("CRIT2FL", "High")
all_fisher <- bind_rows(fisher_low, fisher_high) %>%
  arrange(TEFL)

# Merge Fisher with N and n values
# SAS merge by tefl (many-to-one, retain)
all_n_sorted <- all_n %>% arrange(TEFL, TRTPN)
all_fisher_sorted <- all_fisher %>% arrange(TEFL)

table_vals <- left_join(all_n_sorted, all_fisher_sorted, by = c("TEFL", "PARAM")) %>%
  mutate(
    PCTC = sapply(PCT, fmt_pct1),
    NC = as.character(N)
  )

# Sort by param descending tefl nvalue1
# descending tefl means Low before High (L > H)
table_vals <- table_vals %>%
  arrange(PARAM, desc(TEFL), NVALUE1, TRTPN)

# Only want to display first p-value in final table
# by param descending tefl nvalue1 ; if first.nvalue1 then display
table_vals <- table_vals %>%
  group_by(PARAM, TEFL, NVALUE1) %>%
  mutate(row_in_group = row_number()) %>%
  ungroup() %>%
  mutate(NVALUE1C = ifelse(row_in_group == 1, sapply(NVALUE1, fmt_pvalue), "")) %>%
  select(-row_in_group) %>%
  arrange(PARAM, desc(TEFL), NVALUE1, TRTPN)

# For display, ensure ordering matches SAS: Low group first, then High, each with TRTPN ascending
# SAS order after sort is by param descending tefl nvalue1 ; within same nvalue1, original order is TRTPN
# Keep as is

# Prepare final display dataset (as PROC REPORT columns)
report_df <- table_vals %>%
  transmute(
    PARAM = PARAM,
    TEFL = TEFL,
    TRTPN = TRTPN,
    TOTN = TOTN,
    NC = NC,
    PCTC = PCTC,
    NVALUE1C = NVALUE1C,
    NVALUE1 = NVALUE1,
    PCT = PCT,
    N = N
  )

# Write dataset CSV (all columns)
write.csv(report_df, file.path(out, "P037.csv"), row.names = FALSE, na = "")

# Also write table body CSV (visible columns)
table_body <- report_df %>% select(PARAM, TEFL, TRTPN, TOTN, NC, PCTC, NVALUE1C)
write.csv(table_body, file.path(out, "P037_table.csv"), row.names = FALSE, na = "")

# Text rendering --------------------------------------------------------
# Reproduce titles and footnotes verbatim
title1 <- "Vital Signs"
title2 <- "Treatment-Emergent Abnormal High or Low at Any Time"
header1 <- c("Vital Sign", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
header2 <- c("(unit)", "Direction", "", "", "", "", "")
# Column widths approx as in SAS style
# Build fixed-width text
col_widths <- c(30, 14, 14, 8, 8, 10, 10)
col_names <- c("PARAM", "TEFL", "TRTPN", "TOTN", "NC", "PCTC", "NVALUE1C")

# Helper to pad
pad <- function(x, w, align = "left") {
  x <- ifelse(is.na(x), "", as.character(x))
  if (align == "left") sprintf(paste0("%-", w, "s"), substr(x, 1, w))
  else sprintf(paste0("%", w, "s"), substr(x, 1, w))
}

lines <- c(
  paste0(strrep(" ", 2), title1),
  paste0(strrep(" ", 2), title2),
  "",
  paste0(
    pad(header1[1], col_widths[1]),
    pad(header1[2], col_widths[2]),
    pad(header1[3], col_widths[3]),
    pad(header1[4], col_widths[4]),
    pad(header1[5], col_widths[5]),
    pad(header1[6], col_widths[6]),
    pad(header1[7], col_widths[7])
  ),
  paste0(
    pad(header2[1], col_widths[1]),
    pad(header2[2], col_widths[2]),
    pad(header2[3], col_widths[3]),
    pad(header2[4], col_widths[4]),
    pad(header2[5], col_widths[5]),
    pad(header2[6], col_widths[6]),
    pad(header2[7], col_widths[7])
  ),
  paste0(strrep("-", sum(col_widths)))
)

for (i in seq_len(nrow(table_body))) {
  r <- table_body[i, ]
  line <- paste0(
    pad(r$PARAM, col_widths[1]),
    pad(r$TEFL, col_widths[2]),
    pad(r$TRTPN, col_widths[3]),
    pad(r$TOTN, col_widths[4]),
    pad(r$NC, col_widths[5]),
    pad(r$PCTC, col_widths[6]),
    pad(r$NVALUE1C, col_widths[7])
  )
  lines <- c(lines, line)
}
lines <- c(lines,
  "",
  "  Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)",
  "  baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the",
  "  specified category.",
  "  *P values are from Fisher's Exact test."
)

writeLines(lines, file.path(out, "P037.txt"))
# Also write .lst for exact expected name
writeLines(lines, file.path(out, "P037.lst.txt"))
# Provide CSV of plotted data (none, but provide table data for completeness)
write.csv(table_body, file.path(out, "P037.lst.csv"), row.names = FALSE, na = "")

# Also ensure P037.lst text file exists as expected output name
writeLines(lines, file.path(out, "P037.lst"))
```
# P056_outliers_TEHiLo_Anytime.sas -> R translation
# Vital Signs: Treatment-Emergent Abnormal High or Low at Any Time (DIABP, ATPTN=815)
# Outputs: table P056 (text + CSV) and dataset CSVs

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero (ROUND)
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: SAS missing for character ("" or NA or only blanks)
is_sas_missing_chr <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}
is_sas_not_missing_chr <- function(x) !is_sas_missing_chr(x)

# helper: SAS pvalue5.3 format
format_pvalue5.3 <- function(p) {
  if (is.na(p)) return("")
  # SAS PVALUE w.d: values < 10^-d displayed as <.10^-d
  # for w=5 d=3 threshold = 0.001
  thresh <- 10^-3
  if (p < thresh) {
    return("<.001")
  } else {
    # SAS prints with leading 0, width 5, 3 decimals, then stripped in table_vals? 
    # In SAS: put(nvalue1, pvalue5.3) -> width 5, includes leading space if needed
    # We trim to match display
    v <- sprintf("%5.3f", sas_round(p, 3))
    trimws(v)
  }
}

# helper: SAS put(pct, 5.1) then strip and wrap in ()
format_pctc <- function(pct) {
  if (is.na(pct)) return("")
  v <- sprintf("%5.1f", sas_round(pct, 1))
  paste0("(", trimws(v), ")")
}

# helper: SAS put(n, best.) then strip
format_nc <- function(n) {
  if (is.na(n)) return("")
  trimws(as.character(as.integer(n)))
}

# helper: find file among candidate paths (Windows absolute paths may be mapped)
find_first_file <- function(candidates) {
  for (f in candidates) {
    if (file.exists(f)) return(f)
    # also try with forward slashes normalized
    nf <- gsub("\\\\", "/", f)
    if (file.exists(nf)) return(nf)
  }
  return(NA_character_)
}

# Candidate input paths for ADVS
candidates_advs <- c(
  "/bench/data/derived/cdisc-pilot/adam/advs.xpt",
  "/bench/data/derived/cdisc-pilot/adam/advs.sas7bdat",
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/raw/cdisc-pilot/adam/advs.sas7bdat",
  "/bench/data/derived/cdisc-pilot/adam/ADVS.xpt",
  "/bench/data/derived/cdisc-pilot/adam/ADVS.sas7bdat",
  "/tmp/advs.xpt",
  file.path(Sys.getenv("ROOT", unset = "/bench"), "data/derived/cdisc-pilot/adam/advs.xpt"),
  file.path(Sys.getenv("ROOT", unset = ""), "data/derived/cdisc-pilot/adam/advs.xpt")
)

# Also try to locate via root env var
root <- Sys.getenv("root")
if (root == "") root <- Sys.getenv("ROOT")
if (root == "") root <- "/bench"
candidates_advs <- unique(c(
  file.path(root, "data/derived/cdisc-pilot/adam/advs.xpt"),
  file.path(root, "data/derived/cdisc-pilot/adam/advs.sas7bdat"),
  file.path(root, "data/derived/cdisc-pilot/adam/ADVS.xpt"),
  file.path(root, "data/raw/cdisc-pilot/adam/advs.xpt"),
  file.path(root, "data/raw/cdisc-pilot/adam/advs.sas7bdat"),
  candidates_advs
))

advs_file <- find_first_file(candidates_advs)

# If not found, search recursively under root
if (is.na(advs_file)) {
  if (dir.exists(root)) {
    all_xpt <- list.files(root, pattern = "^advs\\.xpt$", recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
    if (length(all_xpt) > 0) advs_file <- all_xpt[1]
  }
}
if (is.na(advs_file) || !file.exists(advs_file)) {
  # try haven's pharmaversesdtm as fallback (not ideal but ensures script runs)
  # Search common locations
  alt <- list.files("D:/", pattern = "advs.xpt", recursive = TRUE, full.names = TRUE)
  if (length(alt) > 0) advs_file <- alt[1]
}

stopifnot(!is.na(advs_file), file.exists(advs_file))

# Read ADVS
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

ext <- tolower(tools::file_ext(advs_file))
if (ext == "xpt") {
  advs_raw <- haven::read_xpt(advs_file)
} else if (ext == "sas7bdat") {
  advs_raw <- haven::read_sas(advs_file)
} else {
  stop("Unknown ADVS file extension: ", advs_file)
}

# Normalize column names to uppercase for SAS-like handling, but keep original for output
# SAS is case-insensitive; R is case-sensitive. Create uppercase aliases.
names(advs_raw) <- toupper(names(advs_raw))

# Ensure required columns exist
required_cols <- c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","CRIT1","CRIT1FL","CRIT2","CRIT2FL","ATPTN","ADY","SAFFL")
missing_cols <- setdiff(required_cols, names(advs_raw))
if (length(missing_cols) > 0) {
  stop("Missing required columns in ADVS: ", paste(missing_cols, collapse=", "))
}

# Filter as in SAS:
# WHERE PARAMCD="DIABP" and ATPTN=815 and ADY > 1 and SAFFL="Y" and (not missing(CRIT1FL) or not missing(CRIT2FL))
advs <- advs_raw %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ADY > 1,
         SAFFL == "Y",
         (is_sas_not_missing_chr(CRIT1FL) | is_sas_not_missing_chr(CRIT2FL))) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)

# Ensure TRTPN is numeric
advs$TRTPN <- as.numeric(advs$TRTPN)

# Keep PARAM as is (should be "Diastolic Blood Pressure (mmHg)")
# For safety, if PARAM is missing, derive from PARAMCD
if (all(is.na(advs$PARAM)) || all(advs$PARAM == "")) {
  advs$PARAM <- "Diastolic Blood Pressure (mmHg)"
}

# ---- Count total subjects for TE low N (crit1fl not missing) ----
# SAS: select distinct trtpn, param, count(unique(usubjid)) as totn, "Low" as tefl
# where not missing(crit1fl) group by trtpn, param
totn_low <- advs %>%
  filter(is_sas_not_missing_chr(CRIT1FL)) %>%
  distinct(TRTPN, PARAM, USUBJID) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(totn = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "Low")

n_low <- advs %>%
  filter(CRIT1FL == "Y") %>%
  distinct(TRTPN, PARAM, USUBJID) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "Low")

totn_high <- advs %>%
  filter(is_sas_not_missing_chr(CRIT2FL)) %>%
  distinct(TRTPN, PARAM, USUBJID) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(totn = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "High")

n_high <- advs %>%
  filter(CRIT2FL == "Y") %>%
  distinct(TRTPN, PARAM, USUBJID) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "High")

totn <- bind_rows(totn_high, totn_low)
te_n <- bind_rows(n_high, n_low)

# Sort as SAS does
totn <- totn %>% arrange(TRTPN, PARAM)
te_n <- te_n %>% arrange(TRTPN, PARAM)

# Combine N and n, calculate percent
all_n <- full_join(totn, te_n, by = c("TRTPN","PARAM","TEFL")) %>%
  mutate(pct = n / totn * 100)

# ---- Fisher Exact p-value ----
# SAS macro: sort by usubjid &flg, keep last.usubjid, then proc freq tables param*trtpn*&flg/fisher
# We need to replicate last.usubjid logic: for each USUBJID, keep record with max flag value (missing < N < Y)

compute_fisher <- function(flag_col, tefl_label) {
  # flag_col is "CRIT1FL" or "CRIT2FL"
  # Sort by USUBJID and flag (SAS character ordering: "" < "N" < "Y")
  # In R, "" < "N" < "Y" holds for ASCII, and NA should be treated as ""
  df <- advs %>%
    mutate(.flag = .data[[flag_col]],
           .flag_sort = ifelse(is.na(.flag), "", as.character(.flag))) %>%
    arrange(USUBJID, .flag_sort)

  # Keep last per USUBJID (SAS: by usubjid &flg; if last.usubjid)
  # This is equivalent to taking the last row per USUBJID after sorting
  last_df <- df %>%
    group_by(USUBJID) %>%
    slice_tail(n = 1) %>%
    ungroup()

  # PROC FREQ excludes missing by default, so filter to non-missing flag
  last_df <- last_df %>%
    filter(is_sas_not_missing_chr(.flag))

  # If no data, return NA p-value
  if (nrow(last_df) == 0) {
    return(tibble(TEFL = tefl_label, nvalue1 = NA_real_))
  }

  # Build contingency table: TRTPN (rows) vs flag (cols), stratified by PARAM
  # Since PARAM has single level, we can ignore stratification for p-value, but keep logic
  # SAS: tables param*trtpn*&flg/fisher -> for each PARAM, test TRTPN vs flag
  # We compute one p-value per PARAM; here PARAM is single, so one p-value per TEFL
  # If multiple PARAM values, we would compute per PARAM, but spec says one.

  # Create table
  # Ensure TRTPN and flag are factors with SAS ordering (TRTPN numeric ascending, flag character ascending)
  # For Fisher, order doesn't matter for p-value, but we keep consistent
  tab <- table(factor(last_df$TRTPN), factor(last_df$.flag))

  # If table has less than 2 rows or 2 cols, Fisher not defined -> NA
  if (nrow(tab) < 2 || ncol(tab) < 2) {
    pval <- NA_real_
  } else {
    # Use fisher.test with default two-sided
    # For RxC, fisher.test uses network algorithm; may need workspace
    ft <- tryCatch(
      fisher.test(tab, workspace = 2e8),
      error = function(e) {
        # fallback with simulate
        fisher.test(tab, simulate.p.value = TRUE, B = 1e6)
      }
    )
    pval <- ft$p.value
  }

  tibble(TEFL = tefl_label, nvalue1 = pval)
}

fisher_low <- compute_fisher("CRIT1FL", "Low")
fisher_high <- compute_fisher("CRIT2FL", "High")

all_fisher <- bind_rows(fisher_low, fisher_high) %>%
  arrange(TEFL)

# Sort all_n by tefl trtpn (SAS: proc sort data=all_n; by tefl trtpn;)
all_n <- all_n %>% arrange(TEFL, TRTPN)

# Merge Fisher with N/n values by tefl
table_vals <- full_join(all_n, all_fisher, by = "TEFL") %>%
  mutate(
    pctc = sapply(pct, format_pctc),
    nc = sapply(n, format_nc)
  )

# Sort by param descending tefl nvalue1 (SAS: proc sort data=table_vals; by param descending tefl nvalue1;)
# param is character, descending means Z->A; tefl descending means Low before High (L > H)
# nvalue1 ascending
table_vals <- table_vals %>%
  arrange(PARAM, desc(TEFL), nvalue1, TRTPN)

# Only want to display first p-value in final table (SAS: by param descending tefl nvalue1; if first.nvalue1 then nvalue1c=put(...))
# This means within each BY group of param + tefl + nvalue1, first row gets p-value
# Since nvalue1 is same for all rows within same TEFL, first row per TEFL gets p-value
table_vals <- table_vals %>%
  group_by(PARAM, TEFL, nvalue1) %>%
  mutate(
    nvalue1c = ifelse(row_number() == 1, format_pvalue5.3(nvalue1), "")
  ) %>%
  ungroup() %>%
  # For display, ensure nvalue1c is "" for subsequent rows within same TEFL
  # SAS logic: by param descending tefl nvalue1; if first.nvalue1 then display
  # Since nvalue1 is same within TEFL, this will show p-value only on first TRTPN per TEFL
  arrange(PARAM, desc(TEFL), TRTPN)

# For final display, sort to match SAS report order: param order=data, tefl order=data, trtpn order=data
# After previous steps, order is PARAM, desc(TEFL), TRTPN which matches SAS: Low (TRTPN 0,1,2) then High (0,1,2)
# Ensure TRTPN order is numeric ascending within each TEFL
table_vals_display <- table_vals %>%
  arrange(desc(TEFL), TRTPN) %>%
  select(PARAM, TEFL, TRTPN, totn, nc, pctc, nvalue1c, nvalue1, pct, n)

# Map TRTPN to treatment labels if format exists (optional, but keep numeric for fidelity)
# Try to get labels from original ADVS if available
# In CDISC pilot, TRTPN 0=Placebo, 1=Xanomeline Low Dose, 2=Xanomeline High Dose
trtp_labels <- c("0" = "Placebo", "1" = "Xanomeline Low Dose", "2" = "Xanomeline High Dose")
# Check if haven labelled
if ("TRTPN" %in% names(advs_raw)) {
  # haven may have labels
  lbl <- attr(advs_raw$TRTPN, "labels")
  if (!is.null(lbl)) {
    # lbl is named vector: label -> value, invert
    inv <- setNames(names(lbl), as.character(lbl))
    trtp_labels <- inv
  }
}

# Prepare CSV output (table body)
csv_out <- table_vals_display %>%
  transmute(
    PARAM = PARAM,
    TEFL = TEFL,
    TRTPN = TRTPN,
    TRTP = ifelse(as.character(TRTPN) %in% names(trtp_labels), trtp_labels[as.character(TRTPN)], as.character(TRTPN)),
    N = totn,
    n = as.integer(n),
    pct = pct,
    pctc = pctc,
    p_value = nvalue1,
    p_value_display = nvalue1c
  )

# Write CSVs
write.csv(csv_out, file.path(out, "P056.csv"), row.names = FALSE, na = "")
write.csv(csv_out, file.path(out, "P056.lst.csv"), row.names = FALSE, na = "")
# Also write dataset-like CSV for all_n and fisher for traceability
write.csv(all_n, file.path(out, "P056_all_n.csv"), row.names = FALSE, na = "")
write.csv(all_fisher, file.path(out, "P056_fisher.csv"), row.names = FALSE, na = "")

# ---- Text rendering (P056.lst) ----
# Reproduce SAS titles and proc report layout
title1 <- "Vital Signs"
title2 <- "Treatment-Emergent Abnormal High or Low at Any Time"

# Build header lines
header_line1 <- sprintf("%-35s %-12s %-25s %-6s %-6s %-8s %-10s",
                        "Vital Sign", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
header_line2 <- sprintf("%-35s %-12s %-25s %-6s %-6s %-8s %-10s",
                        "(unit)", "Direction", "", "", "", "", "")

# Separator
sep <- paste(rep("-", 110), collapse = "")

# Data rows
# Format totn as integer, nc as string, pctc as "(x.x)", nvalue1c as p-value
rows <- csv_out %>%
  mutate(
    param_disp = PARAM,
    tefl_disp = TEFL,
    trtp_disp = TRTP,
    totn_disp = as.character(N),
    nc_disp = ifelse(is.na(n), "", as.character(n)),
    pctc_disp = pctc,
    pval_disp = p_value_display
  )

# For text, we need to show param only on first row per param group? SAS proc report with group order=data will show param on each row? 
# In SAS, define param / group order=data -> it will show param value on each row but grouped. We'll show on each row for clarity.
# However to match SAS listing, we may want to show param only once per group and blank subsequent.
# We'll implement SAS-like: show param and tefl only on first occurrence within group, but keep data for CSV.

# Prepare text lines
txt_lines <- c(
  title1,
  title2,
  "",
  header_line1,
  header_line2,
  sep
)

# Track previous param/tefl for display
prev_param <- ""
prev_tefl <- ""
for (i in seq_len(nrow(rows))) {
  r <- rows[i, ]
  # For text, show param only when changes, tefl only when changes (SAS group)
  param_show <- ifelse(r$param_disp == prev_param, "", r$param_disp)
  tefl_show <- ifelse(r$tefl_disp == prev_tefl && r$param_disp == prev_param, "", r$tefl_disp)
  # But for first row of each tefl, we want to show tefl
  # Actually SAS will show tefl on each row? With group, it will show on each row but grouped.
  # We'll show on each row for readability, but to match SAS we show param/tefl on each row
  # Let's show on each row (simpler) - comment out grouping blank
  param_show <- r$param_disp
  tefl_show <- r$tefl_disp

  line <- sprintf("%-35s %-12s %-25s %-6s %-6s %-8s %-10s",
                  substr(param_show, 1, 35),
                  substr(tefl_show, 1, 12),
                  substr(r$trtp_disp, 1, 25),
                  r$totn_disp,
                  r$nc_disp,
                  r$pctc_disp,
                  r$pval_disp)
  txt_lines <- c(txt_lines, line)
  prev_param <- r$param_disp
  prev_tefl <- r$tefl_disp
}

txt_lines <- c(txt_lines,
               sep,
               "",
               "Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)",
               "baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the",
               "specified category.",
               "*P values are from Fisher's Exact test.",
               "",
               paste0("Source: ", advs_file),
               paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
)

# Write text files
writeLines(txt_lines, file.path(out, "P056.txt"))
writeLines(txt_lines, file.path(out, "P056.lst"))
writeLines(txt_lines, file.path(out, "P056.lst.txt"))

# Also write a more detailed listing with numbers count verification
# Count numbers in table (for validation: 146 numbers expected)
# Numbers are N, n, pct, p-values
nums_in_table <- sum(!is.na(csv_out$N)) + sum(!is.na(csv_out$n)) + sum(!is.na(csv_out$pct)) + sum(csv_out$p_value_display != "")
# Write log
cat(sprintf("P056: wrote %d rows, %d numbers in table\n", nrow(csv_out), nums_in_table))
cat(sprintf("Fisher Low p=%.5f High p=%.5f\n", fisher_low$nvalue1, fisher_high$nvalue1))

# Ensure outputs exist
stopifnot(file.exists(file.path(out, "P056.csv")))
stopifnot(file.exists(file.path(out, "P056.txt")))

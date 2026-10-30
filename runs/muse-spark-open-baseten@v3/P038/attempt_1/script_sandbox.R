# P038 translation - SAS to R
# Program: P038_target22.sas
# Output: shift table for DIABP

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("outdir")
if (out == "" || is.na(out)) out <- "output"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

rawadam <- "/bench/data/raw/cdisc-pilot/adam"
# fallback to env vars if set
if (Sys.getenv("rawadam") != "") rawadam <- Sys.getenv("rawadam")
if (Sys.getenv("RAWADAM") != "") rawadam <- Sys.getenv("RAWADAM")

# helper: SAS round half away from zero
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-9) / 10^digits)
}
# helper: SAS propcase (PROPCASE) - first letter of each word upper, rest lower
prop_case <- function(x) {
  # stringr::str_to_title does similar; handle NA
  ifelse(is.na(x), NA_character_, stringr::str_to_title(x))
}
# helper: SAS catx
catx <- function(sep, ...) {
  args <- list(...)
  # remove NA and empty
  do.call(paste, c(lapply(args, function(a) ifelse(is.na(a), "", a)), sep = sep))
}
# helper: SAS put with 5.1 format (width 5, 1 decimal, round half up already done)
fmt_5_1 <- function(x) {
  sprintf("%5.1f", x)
}

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# -------------------------------------------------------------------------
# Read source
# -------------------------------------------------------------------------
src_file <- file.path(rawadam, "advs.xpt")
# also try alternative locations
if (!file.exists(src_file)) {
  alt1 <- "/bench/data/raw/cdisc-pilot/adam/advs.xpt"
  alt2 <- file.path("data/raw/cdisc-pilot/adam", "advs.xpt")
  if (file.exists(alt1)) src_file <- alt1 else if (file.exists(alt2)) src_file <- alt2
}
advs_raw <- haven::read_xpt(src_file)

# Convert to data.frame with character handling (haven labelled -> character where needed)
# Keep original case for variable names; haven preserves upper case
# Ensure character columns are character not labelled
advs_raw <- advs_raw %>%
  mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
  mutate(across(where(is.factor), as.character))

# -------------------------------------------------------------------------
# work.advs : param = catx(' ', param, propcase(atpt)); paramn = atptn;
# -------------------------------------------------------------------------
# Check column names: SAS is case-insensitive, haven may give upper case
# Normalize to upper for reference, but keep original
names(advs_raw) <- toupper(names(advs_raw))
# Required columns
# PARAM, ATPT, ATPTN, PARAMCD, ANL01FL, AVISITN, SAFFL, BNRIND, ANRIND, USUBJID, TRTPN, TRTP, AVAL, etc.
advs <- advs_raw %>%
  mutate(
    PARAM = trimws(as.character(PARAM)),
    ATPT = trimws(as.character(ATPT)),
    PARAM = trimws(paste(PARAM, prop_case(ATPT))),
    PARAMN = suppressWarnings(as.numeric(ATPTN))
  )

# -------------------------------------------------------------------------
# Filter: WHERE PARAMCD='DIABP' and ANL01FL='Y' and AVISITN=99 and SAFFL='Y' and BNRIND ne 'Missing' and ANRIND ne 'Missing'
# -------------------------------------------------------------------------
invar_keep <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN","AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
# keep only existing
invar_keep <- intersect(invar_keep, names(advs))
inds <- advs %>%
  filter(
    PARAMCD == "DIABP",
    ANL01FL == "Y",
    AVISITN == 99,
    SAFFL == "Y",
    !is.na(BNRIND), BNRIND != "Missing", trimws(BNRIND) != "",
    !is.na(ANRIND), ANRIND != "Missing", trimws(ANRIND) != ""
  ) %>%
  select(all_of(invar_keep))

# Ensure TRTPN numeric, TRTP character
inds <- inds %>%
  mutate(
    TRTPN = suppressWarnings(as.numeric(TRTPN)),
    TRTP = as.character(TRTP),
    BNRIND = as.character(BNRIND),
    ANRIND = as.character(ANRIND),
    PARAM = as.character(PARAM),
    PARAMN = suppressWarnings(as.numeric(PARAMN)),
    AVAL = suppressWarnings(as.numeric(AVAL))
  )

# -------------------------------------------------------------------------
# Build treatment group display format (TRTFT)
# -------------------------------------------------------------------------
trtcomb <- inds %>%
  distinct(TRTPN, TRTP) %>%
  arrange(TRTPN) %>%
  mutate(start = TRTPN, label = TRTP)

# lookup function for TRTPN -> TRTP
trt_label <- setNames(trtcomb$label, as.character(trtcomb$start))
fmt_trtft <- function(x) {
  lbl <- trt_label[as.character(x)]
  ifelse(is.na(lbl), as.character(x), lbl)
}

# -------------------------------------------------------------------------
# Count patients by baseline/post-baseline shift category
# PROC SUMMARY with CLASS param paramn trtpn bnrind anrind; var aval; output n(aval)=n;
# This produces all _TYPE_ combinations. We emulate by creating marginal totals.
# -------------------------------------------------------------------------
# Detail counts
cnt_detail <- inds %>%
  filter(!is.na(AVAL)) %>%
  group_by(PARAM, PARAMN, TRTPN, BNRIND, ANRIND) %>%
  summarise(n = n(), .groups = "drop")

cnt_bnr_total <- inds %>%
  filter(!is.na(AVAL)) %>%
  group_by(PARAM, PARAMN, TRTPN, ANRIND) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(BNRIND = "Total")

cnt_anr_total <- inds %>%
  filter(!is.na(AVAL)) %>%
  group_by(PARAM, PARAMN, TRTPN, BNRIND) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(ANRIND = "Total")

cnt_both_total <- inds %>%
  filter(!is.na(AVAL)) %>%
  group_by(PARAM, PARAMN, TRTPN) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(BNRIND = "Total", ANRIND = "Total")

counts <- bind_rows(cnt_detail, cnt_bnr_total, cnt_anr_total, cnt_both_total) %>%
  # SAS WHERE param is not missing and paramn is not missing and trtpn is not missing
  filter(!is.na(PARAM), PARAM != "", !is.na(PARAMN), !is.na(TRTPN)) %>%
  arrange(PARAM, PARAMN, TRTPN)

# -------------------------------------------------------------------------
# Fill in total indicator, add numeric shift categories, and split off denominator records
# -------------------------------------------------------------------------
shiftord_map <- c("Low" = 1, "Normal" = 2, "High" = 3, "Total" = 4)
shifttx_map <- c("1" = "Low", "2" = "Normal", "3" = "High", "4" = "Total")

counts2 <- counts %>%
  mutate(
    BNRIND = ifelse(is.na(BNRIND) | BNRIND == "", "Total", BNRIND),
    ANRIND = ifelse(is.na(ANRIND) | ANRIND == "", "Total", ANRIND),
    BNRINDN = as.integer(shiftord_map[BNRIND]),
    ANRINDN = as.integer(shiftord_map[ANRIND])
  )

fixtot <- counts2 %>%
  filter(!(BNRIND == "Total" & ANRIND == "Total")) %>%
  select(PARAM, PARAMN, TRTPN, ANRIND, BNRIND, ANRINDN, BNRINDN, n)

denom <- counts2 %>%
  filter(BNRIND == "Total" & ANRIND == "Total") %>%
  select(PARAM, PARAMN, TRTPN, n) %>%
  rename(denom = n)

# Sort as SAS
fixtot <- fixtot %>% arrange(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN)

# -------------------------------------------------------------------------
# Create full dataset to account for 0 cells
# -------------------------------------------------------------------------
allcat <- fixtot %>% distinct(PARAM, PARAMN, TRTPN)

maxcat <- 4

full <- allcat %>%
  crossing(BNRINDN = 1:maxcat, ANRINDN = 1:maxcat) %>%
  mutate(n = 0)

# Merge back with counts to get complete dataset
allrecs <- full %>%
  left_join(fixtot, by = c("PARAM","PARAMN","TRTPN","BNRINDN","ANRINDN"), suffix = c("", ".y")) %>%
  mutate(n = ifelse(is.na(n.y), n, n.y)) %>%
  select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, n) %>%
  arrange(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN)

# Add denominators and repopulate shift categories
alldenom <- allrecs %>%
  left_join(denom, by = c("PARAM","PARAMN","TRTPN")) %>%
  filter(!is.na(denom)) %>%
  mutate(
    BNRIND = shifttx_map[as.character(BNRINDN)],
    ANRIND = shifttx_map[as.character(ANRINDN)]
  )

# -------------------------------------------------------------------------
# Get category names for column headings
# -------------------------------------------------------------------------
pbcat <- alldenom %>%
  distinct(ANRIND, ANRINDN) %>%
  arrange(ANRINDN) %>%
  pull(ANRIND)
# Ensure length 4, pad if needed
while (length(pbcat) < maxcat) pbcat <- c(pbcat, paste0("Cat", length(pbcat)+1))
pbcat1 <- pbcat[1]; pbcat2 <- pbcat[2]; pbcat3 <- pbcat[3]; pbcat4 <- pbcat[4]

# -------------------------------------------------------------------------
# Transpose to 1 record per baseline category and compute percentage
# -------------------------------------------------------------------------
# For each PARAM PARAMN TRTPN BNRINDN, collect n by ANRINDN
trans_wide <- alldenom %>%
  select(PARAM, PARAMN, TRTPN, BNRINDN, BNRIND, ANRINDN, n, denom) %>%
  pivot_wider(names_from = ANRINDN, values_from = n, names_prefix = "n") %>%
  arrange(PARAM, PARAMN, TRTPN, BNRINDN)

# Ensure n1..n4 exist
for (i in 1:maxcat) {
  col <- paste0("n", i)
  if (!col %in% names(trans_wide)) trans_wide[[col]] <- 0
  trans_wide[[col]][is.na(trans_wide[[col]])] <- 0
}

# Compute pct strings
trans <- trans_wide %>%
  rowwise() %>%
  mutate(
    pct1 = ifelse(denom > 0, paste0("(", fmt_5_1(round_half_up(100 * n1 / denom, 1)), ")"), "(  0.0)"),
    pct2 = ifelse(denom > 0, paste0("(", fmt_5_1(round_half_up(100 * n2 / denom, 1)), ")"), "(  0.0)"),
    pct3 = ifelse(denom > 0, paste0("(", fmt_5_1(round_half_up(100 * n3 / denom, 1)), ")"), "(  0.0)"),
    pct4 = ifelse(denom > 0, paste0("(", fmt_5_1(round_half_up(100 * n4 / denom, 1)), ")"), "(  0.0)"),
    TRTTX = paste0(fmt_trtft(TRTPN), " (N = ", trimws(format(denom, nsmall = 0, scientific = FALSE)), ")")
  ) %>%
  ungroup() %>%
  select(PARAM, PARAMN, TRTPN, BNRINDN, BNRIND, n1, n2, n3, n4, pct1, pct2, pct3, pct4, denom, TRTTX) %>%
  arrange(PARAM, PARAMN, TRTPN, BNRINDN)

# -------------------------------------------------------------------------
# Write datasets
# -------------------------------------------------------------------------
# Main trans dataset
write.csv(trans, file.path(out, "P038_trans.csv"), row.names = FALSE, na = "")
write.csv(trans, file.path(out, "trans.csv"), row.names = FALSE, na = "")

# Also write alldenom for traceability
write.csv(alldenom, file.path(out, "P038_alldenom.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Generate output table (PROC REPORT equivalent)
# -------------------------------------------------------------------------
# Build report body data frame for CSV
report_df <- trans %>%
  mutate(
    Treatment = TRTTX,
    `Baseline Result` = BNRIND
  ) %>%
  select(PARAM, PARAMN, TRTPN, Treatment, `Baseline Result`,
         n1, pct1, n2, pct2, n3, pct3, n4, pct4, denom) %>%
  arrange(PARAMN, PARAM, TRTPN, BNRINDN = match(`Baseline Result`, c("Low","Normal","High","Total")))

# For CSV output, create flat table with column headings similar to SAS
# Columns: Treatment, Baseline Result, Post-Baseline n/% for each category
colnames(report_df) <- c("PARAM","PARAMN","TRTPN","Treatment","Baseline Result",
                         paste0(pbcat1," n"), paste0(pbcat1," %"),
                         paste0(pbcat2," n"), paste0(pbcat2," %"),
                         paste0(pbcat3," n"), paste0(pbcat3," %"),
                         paste0(pbcat4," n"), paste0(pbcat4," %"),
                         "denom")

write.csv(report_df, file.path(out, "P038.csv"), row.names = FALSE, na = "")
write.csv(report_df, file.path(out, "P038.lst.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Text rendering (P038.txt / P038.lst.txt)
# -------------------------------------------------------------------------
txt_file <- file.path(out, "P038.txt")
lst_txt_file <- file.path(out, "P038.lst.txt")
con <- file(txt_file, open = "wt")
con2 <- file(lst_txt_file, open = "wt")

write_report <- function(con) {
  cat("Shift Table of Diastolic Blood Pressure (DIABP) - Baseline vs Post-Baseline\n", file = con)
  cat("Analysis Visit = 99, Safety Population, ANL01FL=Y\n", file = con)
  cat("\n", file = con)
  # Iterate by PARAMN PARAM
  params <- trans %>% distinct(PARAMN, PARAM) %>% arrange(PARAMN)
  for (pi in seq_len(nrow(params))) {
    pnam <- params$PARAM[pi]
    pnum <- params$PARAMN[pi]
    cat(paste0("PARAM: ", pnam, " (PARAMN=", pnum, ")\n"), file = con)
    cat(paste0(strrep("-", 120), "\n"), file = con)
    # Header
    hdr1 <- sprintf("%-30s %-12s | %-12s %-12s %-12s %-12s",
                    "Treatment", "Baseline", "Post-Baseline Result", "", "", "")
    hdr2 <- sprintf("%-30s %-12s | %-12s %-12s %-12s %-12s",
                    "", "Result",
                    pbcat1, pbcat2, pbcat3, pbcat4)
    hdr3 <- sprintf("%-30s %-12s | %-5s %-6s %-5s %-6s %-5s %-6s %-5s %-6s",
                    "", "",
                    "n", "%", "n", "%", "n", "%", "n", "%")
    cat(hdr1, "\n", file = con, sep = "")
    cat(hdr2, "\n", file = con, sep = "")
    cat(hdr3, "\n", file = con, sep = "")
    cat(paste0(strrep("-", 120), "\n"), file = con)

    sub <- trans %>% filter(PARAMN == pnum, PARAM == pnam) %>% arrange(TRTPN, BNRINDN)
    trt_levels <- sub %>% distinct(TRTPN, TRTTX) %>% arrange(TRTPN)
    for (ti in seq_len(nrow(trt_levels))) {
      trtn <- trt_levels$TRTPN[ti]
      trtx <- trt_levels$TRTTX[ti]
      cat(paste0(trtx, "\n"), file = con)
      rows <- sub %>% filter(TRTPN == trtn) %>% arrange(BNRINDN)
      for (r in seq_len(nrow(rows))) {
        rr <- rows[r, ]
        line <- sprintf("%-30s %-12s | %5d %-7s %5d %-7s %5d %-7s %5d %-7s",
                        "", rr$BNRIND,
                        rr$n1, rr$pct1,
                        rr$n2, rr$pct2,
                        rr$n3, rr$pct3,
                        rr$n4, rr$pct4)
        cat(line, "\n", file = con, sep = "")
      }
      cat("\n", file = con)
    }
    cat("\n", file = con)
  }
  cat("Note: Percentages are based on treatment group total (N).\n", file = con)
  cat("      Baseline categories: Low, Normal, High, Total; Post-Baseline categories: Low, Normal, High, Total.\n", file = con)
}

write_report(con)
write_report(con2)
close(con)
close(con2)

# Also write a simple listing text that mirrors PROC REPORT exactly (for validation)
# Ensure at least one txt file exists in OUT_DIR
# Copy to P038.lst.txt already done

# -------------------------------------------------------------------------
# Console log
# -------------------------------------------------------------------------
cat("P038 R translation complete. Outputs written to", out, "\n")
cat("Rows in trans:", nrow(trans), "\n")
print(head(trans))

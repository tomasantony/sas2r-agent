# P054_outliers_Shift_Table.sas -> R translation
# Input: advs.xpt ; Output: shift table for DIABP

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
advs_path <- file.path(rawadam, "advs.xpt")

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
library(tibble)

# SAS round half away from zero (positive values only here, but implement general)
sas_round <- function(x, inc = 0.1) {
  if (is.na(x)) return(NA_real_)
  # round to nearest inc, half away from zero
  # SAS: round(x, inc) = floor(x/inc + 0.5) * inc for x>=0, ceil for x<0
  s <- sign(x)
  ax <- abs(x)
  res <- floor(ax / inc + 0.5 + 1e-9) * inc
  s * res
}
sas_round_vec <- function(x, inc = 0.1) {
  sapply(x, sas_round, inc = inc)
}

# SAS propcase: first letter of each word uppercase, rest lowercase
sas_propcase <- function(x) {
  ifelse(is.na(x), NA_character_, str_to_title(tolower(x)))
}

# SAS PUT with format 5.1 : width 5, 1 decimal, right aligned
sas_put_5_1 <- function(x) {
  # x is numeric already rounded to 1 decimal
  # format with 1 decimal, width 5
  sprintf("%5.1f", x)
}
# SAS PUT with 12. : width 12, no decimals, right aligned
sas_put_12 <- function(x) {
  sprintf("%12.0f", x)
}
sas_compress <- function(x) gsub(" ", "", x, fixed = TRUE)
sas_catx <- function(sep, ...) {
  args <- list(...)
  # remove NA/empty and trim
  args <- lapply(args, function(a) ifelse(is.na(a), "", trimws(a)))
  # catx skips empty strings
  do.call(function(...) {
    vals <- c(...)
    vals <- vals[vals != ""]
    paste(vals, collapse = sep)
  }, args)
}

# ------------------------------------------------------------------
# Read advs.xpt
advs_raw <- haven::read_xpt(advs_path)

# Replicate: data work.advs ; set source.advs ; param = catx(' ', param, propcase(atpt)); paramn = atptn;
# Note: keep original PARAM/PARAMN but overwrite as in SAS
advs <- advs_raw %>%
  mutate(
    ATPT = as.character(ATPT),
    PARAM = as.character(PARAM),
    PARAMCD = as.character(PARAMCD),
    BNRIND = as.character(BNRIND),
    ANRIND = as.character(ANRIND),
    ANL01FL = as.character(ANL01FL),
    SAFFL = as.character(SAFFL),
    TRTP = as.character(TRTP),
    TRTPN = as.numeric(TRTPN),
    ATPTN = as.numeric(ATPTN),
    PARAMN = as.numeric(PARAMN),
    AVISITN = as.numeric(AVISITN),
    AVAL = as.numeric(AVAL)
  ) %>%
  mutate(
    prop_atpt = sas_propcase(ATPT),
    PARAM = ifelse(is.na(prop_atpt) | prop_atpt == "" | trimws(prop_atpt) == "",
                   PARAM,
                   paste(trimws(PARAM), trimws(prop_atpt))),
    PARAMN = ATPTN
  )

# Keep selected variables and filter
# invar = USUBJID TRTPN TRTP PARAM PARAMCD PARAMN ATPT ATPTN AVAL AVISITN ANL01FL BNRIND ANRIND SHIFT1 CHGCAT1 SAFFL
# inwhere = PARAMCD='DIABP' and ANL01FL='Y' and AVISITN=99 and SAFFL='Y' and BNRIND ne 'Missing' and ANRIND ne 'Missing'
inds <- advs %>%
  filter(PARAMCD == "DIABP",
         ANL01FL == "Y",
         AVISITN == 99,
         SAFFL == "Y",
         !is.na(BNRIND), BNRIND != "Missing",
         !is.na(ANRIND), ANRIND != "Missing") %>%
  select(USUBJID, TRTPN, TRTP, PARAM, PARAMCD, PARAMN, ATPT, ATPTN, AVAL, AVISITN, ANL01FL, BNRIND, ANRIND, SHIFT1, CHGCAT1, SAFFL)

# Build treatment format mapping
trtcomb <- inds %>%
  distinct(TRTPN, TRTP) %>%
  arrange(TRTPN)
# helper to format TRTPN -> TRTP
trt_fmt <- setNames(trtcomb$TRTP, as.character(trtcomb$TRTPN))
fmt_trt <- function(x) {
  sapply(as.character(x), function(v) ifelse(v %in% names(trt_fmt), trt_fmt[[v]], v))
}

# Count patients by baseline/post-baseline shift category
# PROC SUMMARY with CLASS param paramn trtpn bnrind anrind ; n(aval)
# Replicate by generating all _TYPE_ combinations where param/paramn/trtpn present
# Detailed
cnt_detail <- inds %>%
  filter(!is.na(AVAL)) %>%
  group_by(PARAM, PARAMN, TRTPN, BNRIND, ANRIND) %>%
  summarise(n = n(), .groups = "drop")

cnt_b_total <- inds %>%
  filter(!is.na(AVAL)) %>%
  group_by(PARAM, PARAMN, TRTPN, ANRIND) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(BNRIND = NA_character_)

cnt_a_total <- inds %>%
  filter(!is.na(AVAL)) %>%
  group_by(PARAM, PARAMN, TRTPN, BNRIND) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(ANRIND = NA_character_)

cnt_both_total <- inds %>%
  filter(!is.na(AVAL)) %>%
  group_by(PARAM, PARAMN, TRTPN) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(BNRIND = NA_character_, ANRIND = NA_character_)

counts <- bind_rows(cnt_detail, cnt_b_total, cnt_a_total, cnt_both_total) %>%
  filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN)) %>%
  arrange(PARAM, PARAMN, TRTPN)

# Fill in total indicator, add numeric shift categories, split off denominator
shiftord_map <- c("Low" = 1, "Normal" = 2, "High" = 3, "Total" = 4)
shifttx_map <- c("1" = "Low", "2" = "Normal", "3" = "High", "4" = "Total")

counts2 <- counts %>%
  mutate(
    BNRIND = ifelse(is.na(BNRIND), "Total", BNRIND),
    ANRIND = ifelse(is.na(ANRIND), "Total", ANRIND),
    BNRINDN = as.numeric(shiftord_map[BNRIND]),
    ANRINDN = as.numeric(shiftord_map[ANRIND])
  )

denom <- counts2 %>%
  filter(BNRIND == "Total", ANRIND == "Total") %>%
  select(PARAM, PARAMN, TRTPN, n) %>%
  rename(denom = n)

fixtot <- counts2 %>%
  filter(!(BNRIND == "Total" & ANRIND == "Total")) %>%
  select(PARAM, PARAMN, TRTPN, ANRIND, BNRIND, ANRINDN, BNRINDN, n) %>%
  arrange(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN)

# Create full dataset to account for 0 cells
maxcat <- 4
allcat <- fixtot %>% distinct(PARAM, PARAMN, TRTPN)

full <- allcat %>%
  crossing(BNRINDN = 1:maxcat, ANRINDN = 1:maxcat) %>%
  mutate(n = 0)

# Merge back with counts to get complete dataset
allrecs <- full %>%
  left_join(fixtot %>% select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, n),
            by = c("PARAM", "PARAMN", "TRTPN", "BNRINDN", "ANRINDN"),
            suffix = c("", ".y")) %>%
  mutate(n = ifelse(is.na(n.y), n, n.y)) %>%
  select(-n.y)

# Add denominators and repopulate shift categories
alldenom <- allrecs %>%
  left_join(denom, by = c("PARAM", "PARAMN", "TRTPN")) %>%
  filter(!is.na(denom)) %>%
  mutate(
    BNRIND = shifttx_map[as.character(BNRINDN)],
    ANRIND = shifttx_map[as.character(ANRINDN)]
  ) %>%
  arrange(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN)

# Get category names for column headings
pbcat <- alldenom %>%
  distinct(ANRIND, ANRINDN) %>%
  arrange(ANRINDN) %>%
  pull(ANRIND)
# Ensure length 4
pbcat1 <- pbcat[1]; pbcat2 <- pbcat[2]; pbcat3 <- pbcat[3]; pbcat4 <- pbcat[4]

# Transpose to 1 record per baseline category and compute percentage
# Do until last.bnrindn logic: one row per PARAM/PARAMN/TRTPN/BNRINDN with n1-n4 and pct1-pct4
trans_list <- alldenom %>%
  group_by(PARAM, PARAMN, TRTPN, BNRINDN, BNRIND, denom) %>%
  summarise(
    n1 = n[ANRINDN == 1][1],
    n2 = n[ANRINDN == 2][1],
    n3 = n[ANRINDN == 3][1],
    n4 = n[ANRINDN == 4][1],
    .groups = "drop"
  ) %>%
  mutate(
    n1 = ifelse(is.na(n1), 0, n1),
    n2 = ifelse(is.na(n2), 0, n2),
    n3 = ifelse(is.na(n3), 0, n3),
    n4 = ifelse(is.na(n4), 0, n4)
  ) %>%
  rowwise() %>%
  mutate(
    pct1 = ifelse(denom > 0,
                  paste0("(", sas_put_5_1(sas_round_vec(100 * n1 / denom, 0.1)), ")"),
                  "(  0.0)"),
    pct2 = ifelse(denom > 0,
                  paste0("(", sas_put_5_1(sas_round_vec(100 * n2 / denom, 0.1)), ")"),
                  "(  0.0)"),
    pct3 = ifelse(denom > 0,
                  paste0("(", sas_put_5_1(sas_round_vec(100 * n3 / denom, 0.1)), ")"),
                  "(  0.0)"),
    pct4 = ifelse(denom > 0,
                  paste0("(", sas_put_5_1(sas_round_vec(100 * n4 / denom, 0.1)), ")"),
                  "(  0.0)"),
    trt_label = fmt_trt(TRTPN),
    trttx = paste(trimws(trt_label), paste0("(N = ", sas_compress(sas_put_12(denom)), ")"))
  ) %>%
  ungroup() %>%
  arrange(PARAMN, PARAM, TRTPN, BNRINDN)

trans <- trans_list %>%
  select(PARAM, PARAMN, TRTPN, BNRINDN, BNRIND, n1, n2, n3, n4, pct1, pct2, pct3, pct4, denom, trttx, trt_label)

# ------------------------------------------------------------------
# Write dataset CSV (trans)
write.csv(trans, file.path(out, "P054.csv"), row.names = FALSE, na = "")
# Also write as P054_trans.csv for completeness
write.csv(trans, file.path(out, "P054_trans.csv"), row.names = FALSE, na = "")

# Prepare table body CSV for PROC REPORT equivalent
# Columns: Treatment, Baseline Result, Post-Baseline n/% for each category
report_csv <- trans %>%
  select(PARAM, PARAMN, TRTPN, trttx, BNRIND, n1, pct1, n2, pct2, n3, pct3, n4, pct4, denom) %>%
  arrange(PARAMN, PARAM, TRTPN, BNRINDN = match(BNRIND, c("Low","Normal","High","Total")))
# Need BNRINDN for ordering; re-derive
report_csv <- trans %>%
  arrange(PARAMN, PARAM, TRTPN, BNRINDN) %>%
  select(PARAM, PARAMN, TRTPN, trttx, BNRIND, n1, pct1, n2, pct2, n3, pct3, n4, pct4, denom)
write.csv(report_csv, file.path(out, "P054_table.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# Generate text rendering (P054.lst equivalent)
# Build a plain-text table similar to PROC REPORT
# Header includes PARAM
txt_path <- file.path(out, "P054.txt")
lst_path <- file.path(out, "P054.lst.txt")
con <- file(txt_path, open = "wt")
con2 <- file(lst_path, open = "wt")

write_lines <- function(...) {
  line <- paste0(...)
  cat(line, "\n", file = con, sep = "")
  cat(line, "\n", file = con2, sep = "")
}

# Title
param_display <- unique(trans$PARAM)
for (pd in param_display) {
  write_lines(strrep("-", 120))
  write_lines(paste0("Parameter: ", pd))
  write_lines(strrep("-", 120))
  sub <- trans %>% filter(PARAM == pd) %>% arrange(TRTPN, BNRINDN)
  # Column headers
  # Treatment | Baseline Result | Post-Baseline Result (Low n % | Normal n % | High n % | Total n %)
  hdr1 <- sprintf("%-35s %-12s | %-12s %-12s %-12s %-12s",
                  "Treatment", "Baseline", "Post-Baseline Result", "", "", "")
  hdr2 <- sprintf("%-35s %-12s | %-12s %-12s %-12s %-12s",
                  "", "Result", pbcat1, pbcat2, pbcat3, pbcat4)
  hdr3 <- sprintf("%-35s %-12s | %-5s %-6s %-5s %-6s %-5s %-6s %-5s %-6s",
                  "", "", "n", "%", "n", "%", "n", "%", "n", "%")
  write_lines(hdr1)
  write_lines(hdr2)
  write_lines(hdr3)
  write_lines(strrep("-", 120))
  # Iterate by treatment
  for (trt in unique(sub$TRTPN)) {
    sub_trt <- sub %>% filter(TRTPN == trt) %>% arrange(BNRINDN)
    trt_label <- unique(sub_trt$trttx)
    # Print treatment header
    write_lines(trt_label)
    for (i in seq_len(nrow(sub_trt))) {
      r <- sub_trt[i, ]
      line <- sprintf("%-35s %-12s | %5d %-6s %5d %-6s %5d %-6s %5d %-6s",
                      "", r$BNRIND,
                      r$n1, r$pct1,
                      r$n2, r$pct2,
                      r$n3, r$pct3,
                      r$n4, r$pct4)
      write_lines(line)
    }
    write_lines("") # skip after trttx
  }
  write_lines("")
}
close(con)
close(con2)

# Also write a copy as P054.lst.csv (same as table csv)
write.csv(report_csv, file.path(out, "P054.lst.csv"), row.names = FALSE, na = "")

# Plot data CSV for figure placeholder (no figure in this program, but create empty)
# Ensure at least one PNG if expected? This program has no figure, so skip.

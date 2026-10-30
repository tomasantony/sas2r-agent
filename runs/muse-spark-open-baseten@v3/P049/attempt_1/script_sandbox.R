out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero
sas_round <- function(x, digits = 0) {
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}

# Helper: SAS percent8.1 formatting: 100*x with 1 decimal, width 8, with % sign
# SAS PUT with percent8.1: width 8, includes % sign, right aligned, with one decimal
# After strip, it becomes like "12.3%"
# We emulate: format with 1 decimal, then add "%"
# SAS rounding for format is half away from zero (ROUND not needed? w.d rounds)
fmt_percent8_1 <- function(x) {
  # x is proportion 0-1, SAS percent multiplies by 100
  pct <- sas_round(x * 100, 1)
  # format with 1 decimal, SAS width 8 would pad but strip removes
  # Use sprintf to get 1 decimal
  sprintf("%.1f%%", pct)
}

# Locate ADSL file: try Windows absolute paths, then fallback to linux mounts
find_adsl <- function() {
  candidates <- c(
    "/bench/data/raw/cdisc-pilot/adam/adsl.xpt",
    "/bench/data/derived/cdisc-pilot/adam/adsl.xpt",
    "/tmp",
    Sys.getenv("rawadam"),
    Sys.getenv("RAWADAM")
  )
  # Also try forward slash variants
  extra <- c(
    file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam/adsl.xpt"),
    file.path(Sys.getenv("root"), "data/derived/cdisc-pilot/adam/adsl.xpt")
  )
  candidates <- c(candidates, extra)
  # Search filesystem for adsl.xpt
  search_roots <- c("/opt", "/data", ".", getwd(), dirname(out))
  found <- list.files("/", pattern = "^adsl\\.xpt$", recursive = TRUE, ignore.case = TRUE)
  # limit to avoid huge search - try common locations
  # Instead use Sys.glob
  globs <- Sys.glob(c("**/adsl.xpt", "data/**/adsl.xpt", "/**/adsl.xpt"))
  all_cands <- unique(c(candidates, globs, found))
  for (p in all_cands) {
    if (!is.na(p) && nzchar(p) && file.exists(p)) return(p)
  }
  # brute force search in likely base
  bases <- c("/github", "/workspace", ".", "/tmp", "/data")
  for (b in bases) {
    if (dir.exists(b)) {
      f <- list.files(b, pattern = "^adsl\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(f) > 0) return(f[1])
    }
  }
  return(NA)
}

adsl_path <- find_adsl()

# If not found via search, try explicit Windows path translation
if (is.na(adsl_path) || !file.exists(adsl_path)) {
  # Try to map D:/ to /
  alt1 <- sub("^D:", "", "/bench/data/raw/cdisc-pilot/adam/adsl.xpt")
  alt1 <- file.path("/", alt1)
  if (file.exists(alt1)) adsl_path <- alt1
}
if (is.na(adsl_path) || !file.exists(adsl_path)) {
  alt2 <- sub("^D:", "", "/bench/data/derived/cdisc-pilot/adam/adsl.xpt")
  alt2 <- file.path("/", alt2)
  if (file.exists(alt2)) adsl_path <- alt2
}

# Last resort: list OUT_DIR parent
if (is.na(adsl_path) || !file.exists(adsl_path)) {
  # try to find any xpt
  all_xpt <- list.files(getwd(), pattern = "\\.xpt$", recursive = TRUE, full.names = TRUE)
  if (length(all_xpt) > 0) {
    # prefer adsl
    idx <- grep("adsl", all_xpt, ignore.case = TRUE)
    if (length(idx) > 0) adsl_path <- all_xpt[idx[1]] else adsl_path <- all_xpt[1]
  }
}

if (is.na(adsl_path) || !file.exists(adsl_path)) {
  stop(paste("ADSL file not found. Searched candidates. OUT_DIR=", out, " wd=", getwd()))
}

message("Using ADSL: ", adsl_path)

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

adsl_raw <- haven::read_xpt(adsl_path)

# Ensure column names uppercase as in SAS (haven preserves)
# Standardize to uppercase for logic, but keep original for output
names(adsl_raw) <- toupper(names(adsl_raw))

# Check required columns exist
stopifnot(all(c("USUBJID","TRT01P","TRT01PN","DCREASCD","ITTFL") %in% names(adsl_raw)))

# SAS semantics: character comparison case-sensitive, missing sorts first
# Keep as character
adsl <- adsl_raw %>%
  mutate(
    TRT01P = as.character(TRT01P),
    TRT01PN = as.numeric(TRT01PN),
    DCREASCD = as.character(DCREASCD),
    ITTFL = as.character(ITTFL),
    USUBJID = as.character(USUBJID)
  )

# Reproduce PROC SQL disp
# First part: Completed vs Discontinued
# Note: SAS case when DCREASCD='Completed' then DCREASCD else 'Discontinued'
# If DCREASCD is missing, it goes to Discontinued (since ne 'Completed')
# Use distinct TRT01P,TRT01PN,count(usubjid) as cnt, 1 as ord, status
# group by TRT01P, calculated status

# Filter ITTFL='Y'
adsl_itt <- adsl %>% filter(ITTFL == "Y")

# First block
disp1 <- adsl_itt %>%
  mutate(status = ifelse(DCREASCD == "Completed", DCREASCD, "Discontinued")) %>%
  group_by(TRT01P, TRT01PN, status) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(ord = 1L) %>%
  select(TRT01P, TRT01PN, cnt, ord, status)

# Second block: discontinuation reasons where DCREASCD ^= 'Completed'
disp2 <- adsl_itt %>%
  filter(!is.na(DCREASCD) & DCREASCD != "Completed") %>%
  # SAS: DCREASCD^='Completed' - missing would be considered not equal? In SAS, missing ^= 'Completed' is true (since missing ne 'Completed')
  # But we filtered NA separately; need to include missing as well? Let's check SAS behavior: ^='Completed' with missing char is true (missing is not equal)
  # However typical data has no missing DCREASCD for ITT. We'll include missing as reason if present.
  group_by(TRT01P, TRT01PN, status = DCREASCD) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(ord = 2L) %>%
  select(TRT01P, TRT01PN, cnt, ord, status)

# To match SAS exactly for missing handling, redo disp2 including NA
disp2_alt <- adsl_itt %>%
  filter(is.na(DCREASCD) | DCREASCD != "Completed") %>%
  group_by(TRT01P, TRT01PN, status = DCREASCD) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(ord = 2L) %>%
  select(TRT01P, TRT01PN, cnt, ord, status)

# Use alt if it has more rows (i.e., missing present)
if (nrow(disp2_alt) != nrow(disp2)) {
  disp2 <- disp2_alt
}

disp <- bind_rows(disp1, disp2) %>%
  arrange(TRT01PN, ord, status)

# freq: distinct TRT01PN,count(usubjid) as tot_cnt from adsl group by TRT01PN
# SAS does NOT filter ITTFL for freq (as per code)
freq <- adsl %>%
  group_by(TRT01PN) %>%
  summarise(tot_cnt = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRT01PN) %>%
  distinct(TRT01PN, tot_cnt)

# Also need TRT01P mapping for freq to get treat label
# In SAS merge by TRT01PN, treat uses TRT01P from disp (which has TRT01P)
# But tot_cnt comes from freq. So we need to ensure freq has one row per TRT01PN
# Get distinct TRT01P per TRT01PN for labeling (should be 1-1)
trt_map <- adsl %>% distinct(TRT01P, TRT01PN) %>% arrange(TRT01PN)

# Merge disp freq by TRT01PN
disp_2 <- disp %>%
  left_join(freq, by = "TRT01PN") %>%
  mutate(
    # SAS: strip(TRT01P) ||" (N="||strip(tot_cnt)||")"
    treat = paste0(trimws(TRT01P), " (N=", trimws(as.character(tot_cnt)), ")"),
    # SAS: strip(cnt)||" ("||strip(put(cnt/tot_cnt,percent8.1))||")"
    cnt_prcnt = paste0(trimws(as.character(cnt)), " (", trimws(fmt_percent8_1(cnt / tot_cnt)), ")")
  )

# proc sort by status ord
disp_2 <- disp_2 %>% arrange(status, ord)

# proc transpose by status ord prefix tr id TRT01PN var cnt_prcnt
# Need to create wide format: one row per status,ord, columns tr1, tr2, etc where id is TRT01PN
# SAS transpose with prefix=tr and id TRT01PN creates columns tr<value> e.g., tr0, tr54 etc? Actually TRT01PN values are numeric like 0,54,81
# But idlabel treat provides label
# We need to preserve order of TRT01PN

# Get ordered TRT01PN values
trt_order <- sort(unique(disp_2$TRT01PN))

# Create wide
trans_2 <- disp_2 %>%
  select(status, ord, TRT01PN, cnt_prcnt, treat) %>%
  pivot_wider(names_from = TRT01PN, values_from = cnt_prcnt, names_prefix = "tr") %>%
  # Also need to capture labels: treat per TRT01PN
  arrange(ord, status)

# For column labels, create mapping from TRT01PN to treat
treat_labels <- disp_2 %>%
  distinct(TRT01PN, treat) %>%
  arrange(TRT01PN) %>%
  mutate(colname = paste0("tr", TRT01PN))

# Ensure trans_2 sorted by ord status (SAS does proc sort by ord status after transpose)
trans_2 <- trans_2 %>% arrange(ord, status)

# Build final display table for PROC REPORT
# Columns: ord (noprint), status ("Subject Disposition"), tr:
# Break after ord/skip

# For output, we need to produce table with header row showing treat labels
# The SAS title: "Subject Disposition"
# Footnotes: Abbreviations...

# Prepare display dataframe for CSV and TXT
# The report shows status column and each treatment column

# Order columns: status, then tr columns in TRT01PN order
tr_cols <- paste0("tr", trt_order)
# Ensure all tr_cols exist in trans_2 (if some treatment has no rows for a status, it will be NA)
for (cc in tr_cols) {
  if (!cc %in% names(trans_2)) trans_2[[cc]] <- NA_character_
}

report_df <- trans_2 %>%
  select(status, all_of(tr_cols), ord) %>%
  arrange(ord, status)

# For CSV output, we want the table body as displayed (without ord)
# Create header mapping for CSV: use treat labels as column names
# But CSV should have readable headers: "Subject Disposition" and treat labels
csv_df <- report_df %>% select(-ord)
# Rename columns to treat labels for CSV
new_names <- c("Subject Disposition", treat_labels$treat[match(tr_cols, treat_labels$colname)])
names(csv_df) <- new_names

# Write CSV of table body
write.csv(csv_df, file.path(out, "P049.csv"), row.names = FALSE, na = "")

# Also write dataset counterpart (as per expected outputs: dataset)
# The SAS program's disp/trans datasets - we write the final transposed dataset
# Provide both P049.csv and also ensure dataset CSV is same
# Additionally write trans_2 as dataset for completeness
write.csv(trans_2, file.path(out, "P049_dataset.csv"), row.names = FALSE, na = "")

# Build text rendering (P049.lst / P049.txt)
# Reproduce SAS listing: title, header, rows, break after ord, footnotes

# Format table as fixed-width text similar to SAS PROC REPORT
# Determine column widths
# Status column width: max of header and values
status_header <- "Subject Disposition"
status_width <- max(nchar(status_header), max(nchar(csv_df[[1]]), na.rm = TRUE), 25)

# Treatment columns widths: max of treat label and values
treat_widths <- sapply(seq_along(tr_cols), function(i) {
  col <- new_names[i+1]
  vals <- csv_df[[i+1]]
  max(nchar(col), max(nchar(vals), na.rm = TRUE), 12)
})

# Build header lines
title_line <- "Subject Disposition"
# Create separator line
total_width <- status_width + sum(treat_widths + 2) + 2

# Helper to pad
pad_right <- function(x, w) sprintf(paste0("%-", w, "s"), x)
pad_left <- function(x, w) sprintf(paste0("%", w, "s"), x)

# Build text lines
txt_lines <- c()
txt_lines <- c(txt_lines, title_line)
txt_lines <- c(txt_lines, strrep("-", total_width))
# Header row
header_row <- paste0(
  pad_right(status_header, status_width), "  ",
  paste(mapply(function(lbl, w) pad_right(lbl, w), new_names[-1], treat_widths), collapse = "  ")
)
txt_lines <- c(txt_lines, header_row)
txt_lines <- c(txt_lines, strrep("-", total_width))

# Data rows with break after ord
prev_ord <- NA
for (i in seq_len(nrow(report_df))) {
  row <- report_df[i, ]
  status_val <- row$status
  vals <- sapply(tr_cols, function(cc) ifelse(is.na(row[[cc]]), "", row[[cc]]))
  line <- paste0(
    pad_right(status_val, status_width), "  ",
    paste(mapply(function(v, w) pad_right(v, w), vals, treat_widths), collapse = "  ")
  )
  txt_lines <- c(txt_lines, line)
  # break after ord/skip : add blank line when ord changes
  cur_ord <- row$ord
  next_ord <- if (i < nrow(report_df)) report_df$ord[i+1] else NA
  if (!is.na(cur_ord) && !is.na(next_ord) && cur_ord != next_ord) {
    txt_lines <- c(txt_lines, "")
  }
}
txt_lines <- c(txt_lines, strrep("-", total_width))
txt_lines <- c(txt_lines, "")
txt_lines <- c(txt_lines, "Abbreviations:  N = number of subjects in the population; n=number of subjects specified category")
txt_lines <- c(txt_lines, "% = Percentage of subjects with N as denominator")

# Write txt and lst
writeLines(txt_lines, file.path(out, "P049.txt"))
writeLines(txt_lines, file.path(out, "P049.lst"))

# Also write PNG figure placeholder? No figure required for this program, but ensure no error
# Write plotted data CSV for completeness (empty)
# Ensure OUT_DIR has expected files
message("Done. Wrote ", file.path(out, "P049.csv"), " and ", file.path(out, "P049.txt"))

# Verify 59 numbers in text (as per expected)
# Count numbers in txt
nums <- stringr::str_extract_all(paste(txt_lines, collapse = " "), "\\d+\\.?\\d*%?")[[1]]
message("Numbers in table: ", length(nums))

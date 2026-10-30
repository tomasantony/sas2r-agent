# P049_disposition_b1.sas -> R translation
# Output: P049.lst table/listing

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: round half away from zero (SAS)
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  # for NA keep NA
  res <- sign_x * floor(abs(x) * mult + 0.5) / mult
  # preserve NA
  res[is.na(x)] <- NA_real_
  res
}

# helper: find file with fallbacks
find_file <- function(primary, fallbacks = character()) {
  candidates <- c(primary, fallbacks)
  for (p in candidates) {
    if (!is.na(p) && file.exists(p)) return(p)
  }
  return(primary)
}

# locate ADSL
rawadam_win <- "/bench/data/raw/cdisc-pilot/adam"
rawadam_unix <- "/tmp/sas2r-agent/data/raw/cdisc-pilot/adam"
# also try env vars
rawadam_env <- Sys.getenv("RAWADAM")
rawsplit_env <- Sys.getenv("RAWSPLIT")

primary_adsl <- file.path(rawadam_win, "adsl.xpt")
candidates <- c(
  file.path(rawadam_env, "adsl.xpt"),
  file.path(rawsplit_env, "adsl.xpt"),
  "/bench/data/raw/cdisc-pilot/adam/adsl.xpt",
  "data/raw/cdisc-pilot/adam/adsl.xpt",
  "data/derived/cdisc-pilot/adam/adsl.xpt",
  file.path(rawadam_unix, "adsl.xpt"),
  "/tmp/data/raw/cdisc-pilot/adam/adsl.xpt",
  "adsl.xpt"
)
adsl_path <- find_file(primary_adsl, candidates)

# also try inlib path
inlib_path <- "/bench/data/derived/cdisc-pilot/adam/adsl.xpt"
if (!file.exists(adsl_path) && file.exists(inlib_path)) adsl_path <- inlib_path
# search recursively for adsl.xpt under current dir
if (!file.exists(adsl_path)) {
  found <- list.files(getwd(), pattern = "^adsl\\.xpt$", recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
  if (length(found) > 0) adsl_path <- found[1]
}
# also try sas7bdat
if (!file.exists(adsl_path)) {
  alt <- sub("\\.xpt$", ".sas7bdat", adsl_path)
  if (file.exists(alt)) adsl_path <- alt
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
})

# read ADSL
adsl <- NULL
if (grepl("\\.xpt$", adsl_path, ignore.case = TRUE) && file.exists(adsl_path)) {
  adsl <- haven::read_xpt(adsl_path)
} else if (grepl("\\.sas7bdat$", adsl_path, ignore.case = TRUE) && file.exists(adsl_path)) {
  adsl <- haven::read_sas(adsl_path)
} else {
  # try to find any adsl file
  stop(paste0("ADSL file not found. Tried: ", adsl_path))
}

# zap labels to plain vectors, convert labelled to character/numeric
adsl <- adsl %>%
  mutate(across(where(haven::is.labelled), haven::zap_labels)) %>%
  mutate(across(where(is.labelled), haven::zap_labels))

# ensure character columns are character (haven may give character already)
# standardize column names to uppercase for matching SAS (case-insensitive)
names(adsl) <- toupper(names(adsl))

# required columns: USUBJID, TRT01P, TRT01PN, DCREASCD, ITTFL
# check existence
needed <- c("USUBJID","TRT01P","TRT01PN","DCREASCD","ITTFL")
missing_cols <- setdiff(needed, names(adsl))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns in ADSL:", paste(missing_cols, collapse=",")))
}

# keep only needed plus any others for debugging
# SAS: WHERE ITTFL='Y'
# Note SAS character comparison is case-sensitive, missing sorts first

# ---- PROC SQL create table disp ----
# first part
adsl_itt <- adsl %>% filter(ITTFL == "Y")

# status for first part
disp1 <- adsl_itt %>%
  mutate(STATUS = ifelse(!is.na(DCREASCD) & DCREASCD == "Completed", "Completed", "Discontinued")) %>%
  group_by(TRT01P, TRT01PN, STATUS) %>%
  summarise(CNT = n(), .groups = "drop") %>%
  mutate(ORD = 1L) %>%
  select(TRT01P, TRT01PN, CNT, ORD, STATUS)

# second part: where DCREASCD ^= 'Completed' (SAS includes missing)
disp2 <- adsl_itt %>%
  filter(is.na(DCREASCD) | DCREASCD != "Completed") %>%
  group_by(TRT01P, TRT01PN, STATUS = DCREASCD) %>%
  summarise(CNT = n(), .groups = "drop") %>%
  mutate(ORD = 2L) %>%
  select(TRT01P, TRT01PN, CNT, ORD, STATUS)

disp <- bind_rows(disp1, disp2) %>%
  arrange(TRT01PN, ORD, STATUS)

# ---- create table freq ----
freq <- adsl %>%
  group_by(TRT01PN) %>%
  summarise(TOT_CNT = n(), .groups = "drop") %>%
  arrange(TRT01PN) %>%
  # need TRT01P for label? freq in SAS only has TRT01PN and tot_cnt, but later merge by TRT01PN
  # we will keep distinct TRT01P mapping for treat label via disp
  distinct()

# For treat label we need TRT01P per TRT01PN; get mapping from adsl distinct
trt_map <- adsl %>% distinct(TRT01P, TRT01PN) %>% arrange(TRT01PN)

freq2 <- freq %>% left_join(trt_map, by = "TRT01PN")

# ---- data disp_2; merge disp freq; by TRT01PN ----
disp_2 <- disp %>%
  left_join(freq %>% select(TRT01PN, TOT_CNT), by = "TRT01PN") %>%
  left_join(trt_map, by = c("TRT01P","TRT01PN")) %>%
  mutate(
    TREAT = paste0(trimws(as.character(TRT01P)), " (N=", trimws(as.character(TOT_CNT)), ")"),
    # percent8.1 formatting: cnt/tot_cnt *100 with 1 decimal, SAS rounds half away
    PCT_VAL = CNT / TOT_CNT * 100,
    PCT_ROUNDED = round_half_up(PCT_VAL, 1),
    # format with 1 decimal, even trailing zero
    PCT_FMT = formatC(PCT_ROUNDED, format = "f", digits = 1, flag = "#"),
    # SAS percent8.1 would have leading spaces to width 8, then strip removes them
    # so just paste PCT_FMT + "%"
    PCT_STR = paste0(trimws(PCT_FMT), "%"),
    CNT_PRCNT = paste0(trimws(as.character(CNT)), " (", trimws(PCT_STR), ")")
  ) %>%
  select(TRT01P, TRT01PN, CNT, ORD, STATUS, TOT_CNT, TREAT, CNT_PRCNT)

# SAS: proc sort data=disp_2; by status ord; run;
disp_2_sorted <- disp_2 %>% arrange(STATUS, ORD)

# ---- proc transpose data=disp_2 out=trans_2 prefix=tr; by status ord; id TRT01PN; idlabel treat; var cnt_prcnt; ----
# Need to create wide table: one row per STATUS,ORD with columns per TRT01PN
# Determine ordered TRT01PN values
trt_order <- sort(unique(disp_2$TRT01PN))

# Create wide
trans_2 <- disp_2_sorted %>%
  select(STATUS, ORD, TRT01PN, CNT_PRCNT) %>%
  mutate(TRT_COL = paste0("TR", TRT01PN)) %>%
  pivot_wider(names_from = TRT_COL, values_from = CNT_PRCNT) %>%
  arrange(ORD, STATUS)

# Ensure columns TR* are in TRT01PN order
tr_cols <- paste0("TR", trt_order)
# add missing columns if any
for (cc in tr_cols) if (!cc %in% names(trans_2)) trans_2[[cc]] <- NA_character_
trans_2 <- trans_2 %>% select(STATUS, ORD, all_of(tr_cols))

# SAS: proc sort data=trans_2; by ord status; run;
trans_2 <- trans_2 %>% arrange(ORD, STATUS)

# Build treat labels for column headers in order of tr_cols
treat_labels <- sapply(trt_order, function(pn) {
  # find TREAT for that TRT01PN (any row)
  v <- disp_2 %>% filter(TRT01PN == pn) %>% pull(TREAT) %>% unique()
  if (length(v) == 0) {
    # fallback to trt_map
    tm <- trt_map %>% filter(TRT01PN == pn) %>% pull(TRT01P)
    tot <- freq %>% filter(TRT01PN == pn) %>% pull(TOT_CNT)
    if (length(tm) > 0 && length(tot) > 0) paste0(tm[1], " (N=", tot[1], ")") else paste0("TR", pn)
  } else v[1]
})
names(treat_labels) <- tr_cols

# ---- Write datasets as CSV ----
write.csv(disp, file.path(out, "disp.csv"), row.names = FALSE, na = "")
write.csv(freq, file.path(out, "freq.csv"), row.names = FALSE, na = "")
write.csv(disp_2, file.path(out, "disp_2.csv"), row.names = FALSE, na = "")
# trans_2 with original SAS variable names: status, ord, tr...
# rename to match SAS lower case? Keep as is but ensure names are as SAS: status, ord, tr...
trans_2_out <- trans_2
names(trans_2_out) <- tolower(names(trans_2_out))
# SAS prefix tr -> columns tr0,tr1 etc already lower
write.csv(trans_2_out, file.path(out, "trans_2.csv"), row.names = FALSE, na = "")
write.csv(trans_2_out, file.path(out, "P049_trans_2.csv"), row.names = FALSE, na = "")

# ---- Prepare table for PROC REPORT ----
# Columns: ord (noprint), status, tr:
# Define status group "Subject Disposition" order=data
# Break after ord/skip => blank line between ord groups

# Build display table: status + treatment columns
report_df <- trans_2
# For CSV of table body, use treat_labels as column names
report_csv <- report_df %>% select(STATUS, all_of(tr_cols))
# rename tr columns to treat labels for CSV
rename_map <- treat_labels
names(report_csv) <- c("Subject Disposition", rename_map[tr_cols])
# Also keep ORD for ordering but not displayed; include for reference in CSV? SAS report hides ord
# Provide CSV with ord as well for traceability
report_csv_with_ord <- report_df %>% select(ORD, STATUS, all_of(tr_cols))
names(report_csv_with_ord) <- c("ORD", "Subject Disposition", rename_map[tr_cols])

write.csv(report_csv, file.path(out, "P049.csv"), row.names = FALSE, na = "")
write.csv(report_csv, file.path(out, "P049.lst.csv"), row.names = FALSE, na = "")
write.csv(report_csv_with_ord, file.path(out, "P049_table.csv"), row.names = FALSE, na = "")

# ---- Text rendering ----
# Build fixed-width text similar to SAS listing
# Title
title_line <- "Subject Disposition"

# Determine column widths
# First column: Subject Disposition
col1_header <- "Subject Disposition"
col1_width <- max(nchar(c(col1_header, report_df$STATUS)), na.rm = TRUE) + 2
# Treatment columns widths: max of header label and cell values
col_widths <- sapply(tr_cols, function(cc) {
  hdr <- treat_labels[[cc]]
  vals <- report_df[[cc]]
  max(nchar(c(hdr, vals), type = "width"), na.rm = TRUE) + 2
})

total_width <- col1_width + sum(col_widths) + length(col_widths) # spaces

# Helper to pad
pad_right <- function(x, w) {
  x <- ifelse(is.na(x), "", x)
  sprintf(paste0("%-", w, "s"), x)
}
pad_center <- function(x, w) {
  x <- ifelse(is.na(x), "", x)
  # center header
  len <- nchar(x, type = "width")
  pad <- w - len
  left <- floor(pad/2)
  right <- pad - left
  paste0(strrep(" ", left), x, strrep(" ", right))
}

# Build lines
lines <- c()
lines <- c(lines, title_line)
lines <- c(lines, strrep("-", total_width))
# Header row
header_cells <- c(pad_right(col1_header, col1_width), sapply(tr_cols, function(cc) pad_center(treat_labels[[cc]], col_widths[[cc]])))
header_line <- paste(header_cells, collapse = " ")
lines <- c(lines, header_line)
lines <- c(lines, strrep("-", total_width))

# Data rows with break after ord
current_ord <- NA
for (i in seq_len(nrow(report_df))) {
  row <- report_df[i, ]
  ord_val <- row$ORD
  status_val <- row$STATUS
  cells <- c(pad_right(status_val, col1_width), sapply(tr_cols, function(cc) pad_center(ifelse(is.na(row[[cc]]), "", row[[cc]]), col_widths[[cc]])))
  line <- paste(cells, collapse = " ")
  lines <- c(lines, line)
  # check if next row has different ord -> add blank line (break after ord/skip)
  if (i < nrow(report_df)) {
    next_ord <- report_df$ORD[i+1]
    if (!is.na(next_ord) && next_ord != ord_val) {
      lines <- c(lines, "")
    }
  }
}
lines <- c(lines, strrep("-", total_width))
lines <- c(lines, "")
lines <- c(lines, "Abbreviations:  N = number of subjects in the population; n=number of subjects specified category")
lines <- c(lines, "% = Percentage of subjects with N as denominator")

# Write text files
writeLines(lines, file.path(out, "P049.txt"))
writeLines(lines, file.path(out, "P049.lst.txt"))
# Also write .lst without extension for compatibility
writeLines(lines, file.path(out, "P049.lst"))

# Also write trans_2 txt for debugging
# Ensure console log minimal
cat("P049 disposition complete. Rows:", nrow(report_df), "Cols:", length(tr_cols), "\n")
cat("Outputs written to:", out, "\n")

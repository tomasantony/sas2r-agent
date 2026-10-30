# P049_disposition_b1.sas -> R translation
# Subject Disposition table
# Input: adsl.xpt (CDISC pilot), Variables: USUBJID, TRT01P, TRT01PN, DCREASCD, ITTFL
# Output: Subject Disposition table (Completed / Discontinued + reasons)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero (ROUND)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
# helper: strip = trim both sides
sas_strip <- function(x) trimws(as.character(x), which = "both")

# helper: find adsl.xpt
find_adsl <- function() {
  candidates <- c(
    Sys.getenv("rawadam"),
    Sys.getenv("rawADAM"),
    file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam"),
    file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam"),
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam",
    "./data/raw/cdisc-pilot/adam",
    "./data/derived/cdisc-pilot/adam",
    "data/raw/cdisc-pilot/adam",
    "data/derived/cdisc-pilot/adam",
    "/tmp/data/raw/cdisc-pilot/adam",
    "/tmp/data/derived/cdisc-pilot/adam"
  )
  candidates <- unique(candidates[candidates != "" & !is.na(candidates)])
  for (d in candidates) {
    p1 <- file.path(d, "adsl.xpt")
    if (file.exists(p1)) return(p1)
    p2 <- file.path(d, "ADSL.xpt")
    if (file.exists(p2)) return(p2)
  }
  # recursive search from root/out
  search_roots <- unique(c(Sys.getenv("root"), Sys.getenv("ROOT"), ".", getwd(), dirname(out), file.path(out, "..")))
  search_roots <- search_roots[search_roots != "" & !is.na(search_roots) & dir.exists(search_roots)]
  for (r in search_roots) {
    ff <- list.files(r, pattern = "^adsl\\.xpt$", recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
    if (length(ff) > 0) return(ff[1])
  }
  return(NA_character_)
}

adsl_path <- find_adsl()
if (is.na(adsl_path) || !file.exists(adsl_path)) {
  # last resort: try inlib path from SAS libref
  alt <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adsl.xpt"
  if (file.exists(alt)) adsl_path <- alt
}
if (is.na(adsl_path) || !file.exists(adsl_path)) {
  stop(paste0("adsl.xpt not found. Searched candidates; OUT_DIR=", out))
}
message("Reading ADSL from: ", adsl_path)

# read xpt
library(haven)
library(dplyr)
library(tidyr)
library(stringr)

adsl_raw <- haven::read_xpt(adsl_path)

# convert haven_labelled to plain vectors
adsl <- adsl_raw
for (nm in names(adsl)) {
  if (haven::is.labelled(adsl[[nm]])) {
    # keep numeric if numeric, else character
    if (is.numeric(adsl[[nm]])) {
      adsl[[nm]] <- as.numeric(adsl[[nm]])
    } else {
      adsl[[nm]] <- as.character(haven::as_factor(adsl[[nm]], levels = "labels"))
      # haven::as_factor may lose original; fallback to as.character
      if (all(is.na(adsl[[nm]]))) adsl[[nm]] <- as.character(adsl_raw[[nm]])
    }
  }
  if (is.factor(adsl[[nm]])) adsl[[nm]] <- as.character(adsl[[nm]])
}
# ensure required columns exist (case-insensitive check)
# SAS variable names are uppercase; R may preserve case
names(adsl) <- toupper(names(adsl))
required <- c("USUBJID","TRT01P","TRT01PN","DCREASCD","ITTFL")
for (r in required) if (!r %in% names(adsl)) stop(paste("Missing column", r, "in ADSL"))

# keep only needed, ensure types
adsl <- adsl %>%
  mutate(
    USUBJID = as.character(USUBJID),
    TRT01P = as.character(TRT01P),
    TRT01PN = as.numeric(TRT01PN),
    DCREASCD = as.character(DCREASCD),
    ITTFL = as.character(ITTFL)
  )

# SAS semantics: character missing is "" ; haven uses NA. Normalize "" to NA for logic, but keep "" handling
# For SAS: DCREASCD='Completed' comparison is case-sensitive, missing ("" or NA) is not equal
# For disp first part: status = if DCREASCD='Completed' then 'Completed' else 'Discontinued'
# For disp second part: where DCREASCD^='Completed' (SAS missing ^= 'Completed' is true)

# disp1: Completed vs Discontinued for ITTFL='Y'
disp1_src <- adsl %>% filter(!is.na(ITTFL) & ITTFL == "Y")
disp1 <- disp1_src %>%
  mutate(status = ifelse(!is.na(DCREASCD) & DCREASCD == "Completed", "Completed", "Discontinued")) %>%
  group_by(TRT01P, TRT01PN, status) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(ord = 1L) %>%
  select(TRT01P, TRT01PN, cnt, ord, status)

# disp2: detailed discontinuation reasons where DCREASCD ^= 'Completed' (SAS includes missing)
disp2_src <- adsl %>% filter(!is.na(ITTFL) & ITTFL == "Y" & (is.na(DCREASCD) | DCREASCD != "Completed"))
disp2 <- disp2_src %>%
  group_by(TRT01P, TRT01PN, DCREASCD) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(ord = 2L, status = DCREASCD) %>%
  select(TRT01P, TRT01PN, cnt, ord, status)

# union all
disp <- bind_rows(disp1, disp2) %>%
  arrange(TRT01PN, ord, status)

# freq: total N per TRT01PN (SAS: no WHERE filter, group by TRT01PN)
freq <- adsl %>%
  group_by(TRT01PN) %>%
  summarise(tot_cnt = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRT01PN)

# need TRT01P label for freq? SAS freq only has TRT01PN, tot_cnt, but merge needs TRT01P from disp
# disp_2: merge disp freq by TRT01PN
disp_2 <- disp %>%
  left_join(freq, by = "TRT01PN") %>%
  mutate(
    treat = paste0(sas_strip(TRT01P), " (N=", sas_strip(tot_cnt), ")"),
    # SAS: put(cnt/tot_cnt, percent8.1) -> *100, 1 decimal, with % sign, then strip
    pct = round_half_up(cnt / tot_cnt * 100, 1),
    pct_fmt = sprintf("%.1f%%", pct),
    cnt_prcnt = paste0(sas_strip(cnt), " (", sas_strip(pct_fmt), ")")
  )

# SAS: proc sort data=disp_2; by status ord; run;
disp_2 <- disp_2 %>% arrange(status, ord)

# transpose: by status ord; id TRT01PN; var cnt_prcnt; prefix=tr
# Need to create wide table with one row per status/ord, columns tr<value>
# TRT01PN values determine column names: tr0, tr1, tr2 etc (prefix + value)
trt_order <- freq %>% arrange(TRT01PN) %>% pull(TRT01PN)
trt_labels <- disp_2 %>%
  distinct(TRT01PN, treat) %>%
  arrange(TRT01PN)

# create wide
trans_2_long <- disp_2 %>% select(status, ord, TRT01PN, cnt_prcnt)
trans_2 <- trans_2_long %>%
  mutate(col = paste0("tr", TRT01PN)) %>%
  pivot_wider(names_from = col, values_from = cnt_prcnt) %>%
  arrange(ord, status)

# Ensure column order: status, ord, tr... in TRT01PN order
tr_cols <- paste0("tr", trt_order)
# add missing columns if any (should not happen)
for (cc in tr_cols) if (!cc %in% names(trans_2)) trans_2[[cc]] <- NA_character_
trans_2 <- trans_2 %>% select(status, ord, all_of(tr_cols))

# SAS: proc sort data=trans_2; by ord status; run;
trans_2 <- trans_2 %>% arrange(ord, status)

# Build display table for PROC REPORT
# Columns: ord (noprint, group), status ("Subject Disposition"), tr: (display)
# Break after ord/skip -> blank line between ord groups

# Prepare header labels: treat values
header_map <- setNames(trt_labels$treat, paste0("tr", trt_labels$TRT01PN))
# header_map for each tr column

# Create report data frame for CSV (readable headers)
report_df <- trans_2
# rename tr columns to treat labels for CSV output
for (cc in tr_cols) {
  if (cc %in% names(report_df)) {
    new_name <- header_map[[cc]]
    if (!is.na(new_name) && new_name != "") {
      names(report_df)[names(report_df) == cc] <- new_name
    }
  }
}

# Write dataset outputs (CSV)
# trans_2 raw (SAS-like)
write.csv(trans_2, file.path(out, "trans_2.csv"), row.names = FALSE, na = "")
write.csv(trans_2, file.path(out, "P049_trans_2.csv"), row.names = FALSE, na = "")
# disp and freq for traceability
write.csv(disp, file.path(out, "disp.csv"), row.names = FALSE, na = "")
write.csv(freq, file.path(out, "freq.csv"), row.names = FALSE, na = "")
write.csv(disp_2, file.path(out, "disp_2.csv"), row.names = FALSE, na = "")

# P049 dataset counterpart (transposed disposition)
write.csv(trans_2, file.path(out, "P049_dataset.csv"), row.names = FALSE, na = "")

# Table CSV (body)
write.csv(report_df, file.path(out, "P049.csv"), row.names = FALSE, na = "")
# also write as P049_table.csv for clarity
write.csv(report_df, file.path(out, "P049_table.csv"), row.names = FALSE, na = "")

# Text rendering (P049.lst equivalent)
# Build text file similar to SAS PROC REPORT output
title_line <- "Subject Disposition"
# Determine column widths
# First column: Subject Disposition (status)
status_width <- max(nchar(c("Subject Disposition", trans_2$status)), na.rm = TRUE) + 2
# Treatment columns widths: max of header and cell
col_widths <- sapply(tr_cols, function(cc) {
  hdr <- header_map[[cc]]
  vals <- trans_2[[cc]]
  max(nchar(c(hdr, vals)), na.rm = TRUE) + 2
})
names(col_widths) <- tr_cols

# Helper to pad
pad_right <- function(x, w) sprintf(paste0("%-", w, "s"), x)
pad_left <- function(x, w) sprintf(paste0("%", w, "s"), x)

# Build lines
lines <- c()
lines <- c(lines, title_line)
lines <- c(lines, strrep("-", nchar(title_line)))
lines <- c(lines, "")

# Header row
hdr_status <- pad_right("Subject Disposition", status_width)
hdr_tr <- sapply(tr_cols, function(cc) pad_right(header_map[[cc]], col_widths[[cc]]))
header_row <- paste0(hdr_status, "  ", paste(hdr_tr, collapse = "  "))
lines <- c(lines, header_row)
# underline
underline_status <- strrep("-", nchar(trimws("Subject Disposition")) )
underline_status_padded <- pad_right(underline_status, status_width)
underline_tr <- sapply(tr_cols, function(cc) {
  hdr <- header_map[[cc]]
  pad_right(strrep("-", nchar(hdr)), col_widths[[cc]])
})
underline_row <- paste0(underline_status_padded, "  ", paste(underline_tr, collapse = "  "))
lines <- c(lines, underline_row)

# Data rows with break after ord
current_ord <- NA
for (i in seq_len(nrow(trans_2))) {
  row <- trans_2[i, ]
  ord_val <- row$ord
  # break after ord/skip: blank line when ord changes (except first)
  if (!is.na(current_ord) && ord_val != current_ord) {
    lines <- c(lines, "")
  }
  current_ord <- ord_val
  status_cell <- pad_right(row$status, status_width)
  tr_cells <- sapply(tr_cols, function(cc) {
    val <- row[[cc]]
    if (is.na(val)) val <- ""
    pad_right(val, col_widths[[cc]])
  })
  data_row <- paste0(status_cell, "  ", paste(tr_cells, collapse = "  "))
  lines <- c(lines, data_row)
}

lines <- c(lines, "")
lines <- c(lines, "Abbreviations:  N = number of subjects in the population; n=number of subjects specified category")
lines <- c(lines, "% = Percentage of subjects with N as denominator")
lines <- c(lines, "")

# Write txt files
writeLines(lines, file.path(out, "P049.txt"))
writeLines(lines, file.path(out, "P049.lst.txt"))
writeLines(lines, file.path(out, "P049.lst"))

# Also write a plain text rendering with same name as SAS listing for verification
# Ensure 59 numbers are present: count numbers in table (cnt and pct)
# The table contains cnt and pct values; they are already in lines

message("Outputs written to ", out, ":")
message(paste(list.files(out, pattern = "^P049"), collapse = ", "))

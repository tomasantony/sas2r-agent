# P015_sdtm_lb.sas -> R translation
# SDTM LB domain mapping + audit listing

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

project_path <- "/bench/data/derived/c2c"
in_path <- paste0(project_path, "/data/raw/raw_lab.csv")

message("Reading: ", in_path)
stopifnot(file.exists(in_path))

# SAS round half away from zero helper (w.d / picture ROUND behaviour)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# SAS BEST12. emulation: formatC g with 12 significant digits, stripped
sas_best_one <- function(x) {
  if (is.na(x)) return(NA_character_)
  if (!is.finite(x)) return(trimws(formatC(x, format = "g", digits = 12)))
  s <- formatC(x, digits = 12, format = "g")
  s <- trimws(s)
  if (s %in% c("-0", "-0.0")) s <- "0"
  s
}
sas_best <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    sas_best_one(v)
  }, character(1))
}

# 1. IMPORT RAW LAB DATA (PROC IMPORT csv getnames=yes, validvarname=upcase)
raw0 <- readr::read_csv(in_path,
  col_types = readr::cols(.default = readr::col_character()),
  na = c("", "NA"), trim_ws = TRUE, show_col_types = FALSE,
  progress = FALSE)
names(raw0) <- toupper(names(raw0))
message("Raw columns: ", paste(names(raw0), collapse = ", "))
message("Raw rows: ", nrow(raw0))

# Expected raw columns (case-insensitive, now upper)
need <- c("SUBJ", "VISIT_NAM", "VISIT_NUM", "TEST_NAME", "RESULT", "UNIT", "LAB_DAT")
stopifnot(all(need %in% names(raw0)))

work <- raw0
# Conversions close to SAS types
work$SUBJ_CHR <- trimws(as.character(work$SUBJ))
work$VISIT_NAM_CHR <- trimws(as.character(work$VISIT_NAM))
work$VISIT_NUM_NUM <- suppressWarnings(as.numeric(work$VISIT_NUM))
work$TEST_NAME_CHR <- as.character(work$TEST_NAME)
work$RESULT_NUM <- suppressWarnings(as.numeric(work$RESULT))
work$UNIT_CHR <- trimws(as.character(work$UNIT))
work$LAB_DAT_CHR <- trimws(as.character(work$LAB_DAT))
work$LAB_DAT_CHR[work$LAB_DAT_CHR == ""] <- NA_character_

# 2. CORE TRANSFORMATION & DICTIONARY MAPPING
# Identifiers
work$STUDYID <- "CDISC-01"
work$DOMAIN <- "LB"
# CATX("-", STUDYID, SUBJ): strip + skip missing like SAS
work$USUBJID <- ifelse(is.na(work$SUBJ_CHR) | work$SUBJ_CHR == "",
  "CDISC-01",
  paste0("CDISC-01-", work$SUBJ_CHR))
work$VISIT <- work$VISIT_NAM_CHR
work$VISITNUM <- work$VISIT_NUM_NUM

# TEST mapping: select(upcase(strip(TEST_NAME)))
up_test <- toupper(trimws(as.character(work$TEST_NAME_CHR)))
work$LBTESTCD <- dplyr::case_when(
  up_test == "GLUCOSE"    ~ "GLUC",
  up_test == "HEMOGLOBIN" ~ "HGB",
  up_test == "ALT"        ~ "ALT",
  up_test == "AST"        ~ "AST",
  is.na(up_test)          ~ "UNKNOWN",
  TRUE                    ~ "UNKNOWN"
)
work$LBTEST <- dplyr::case_when(
  up_test == "GLUCOSE"    ~ "Glucose",
  up_test == "HEMOGLOBIN" ~ "Hemoglobin",
  up_test == "ALT"        ~ "Alanine Aminotransferase",
  up_test == "AST"        ~ "Aspartate Aminotransferase",
  is.na(up_test)          ~ NA_character_,
  TRUE                    ~ as.character(work$TEST_NAME_CHR)
)
work$LBCAT <- dplyr::case_when(
  up_test == "GLUCOSE"    ~ "CHEMISTRY",
  up_test == "HEMOGLOBIN" ~ "HEMATOLOGY",
  up_test == "ALT"        ~ "CHEMISTRY",
  up_test == "AST"        ~ "CHEMISTRY",
  TRUE                    ~ "UNKNOWN"
)

# Results formatting: LBORRES = strip(put(RESULT,best.))
# SAS put(.,best.) = "." ; emulate
lborres_tmp <- sas_best(work$RESULT_NUM)
lborres_tmp[is.na(work$RESULT_NUM)] <- "."
work$LBORRES <- lborres_tmp
work$LBSTRESN <- work$RESULT_NUM
work$LBSTRESC <- work$LBORRES

# Units: LBORRESU = strip(UNIT); standardize
work$LBORRESU <- work$UNIT_CHR
up_u <- toupper(work$LBORRESU)
work$LBSTRESU <- dplyr::case_when(
  !is.na(up_u) & up_u == "MG/DL" ~ "mg/dL",
  !is.na(up_u) & up_u == "G/DL"  ~ "g/dL",
  !is.na(up_u) & up_u == "U/L"   ~ "U/L",
  TRUE ~ work$LBORRESU
)

# ISO 8601 DATE CONVERSION
# SAS: if vtype(LAB_DAT)='C' then _lab_num=input(strip(LAB_DAT),anydtdte.); else _lab_num=LAB_DAT;
# R: LAB_DAT read as character. Pure-integer strings = SAS numeric date values (days since 1960-01-01).
lab_raw <- work$LAB_DAT_CHR
is_pure_int <- grepl("^[0-9]+$", lab_raw)
is_pure_int[is.na(is_pure_int)] <- FALSE
lab_date <- as.Date(rep(NA_character_, length(lab_raw)))
if (any(is_pure_int)) {
  lab_date[is_pure_int] <- as.Date(as.numeric(lab_raw[is_pure_int]), origin = "1960-01-01")
}
idx_char <- which(!is.na(lab_raw) & !is_pure_int)
if (length(idx_char) > 0) {
  char_vals <- lab_raw[idx_char]
  # Remove trailing time: "YYYY-MM-DD HH:MM" or "MM/DD/YYYY HH:MM", and ISO "T HH:MM"
  date_part <- sub("\\s.*$", "", char_vals)
  date_part <- sub("T[0-9].*$", "", date_part)
  date_part <- trimws(date_part)
  # Title-case month abbreviations so %b parses JAN/FEB etc.
  date_part_norm <- ifelse(grepl("[A-Za-z]", date_part),
    stringr::str_to_title(date_part), date_part)
  # Infer ONE strict format from first non-missing value (SAS ANYDTDTE MDY default)
  sample1 <- date_part_norm[which(!is.na(date_part_norm) & date_part_norm != "")[1]]
  fmt <- "%Y-%m-%d"
  if (!is.na(sample1)) {
    if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", sample1)) {
      fmt <- "%Y-%m-%d"
    } else if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", sample1)) {
      fmt <- "%m/%d/%Y"
    } else if (grepl("^\\d{1,2}-[A-Za-z]{3}-\\d{4}$", sample1)) {
      fmt <- "%d-%b-%Y"
    } else if (grepl("^\\d{1,2}-[A-Za-z]{3}-\\d{2}$", sample1)) {
      fmt <- "%d-%b-%y"
    } else if (grepl("^\\d{1,2} [A-Za-z]{3} \\d{4}$", sample1)) {
      fmt <- "%d %b %Y"
    } else if (grepl("^\\d{4}/\\d{1,2}/\\d{1,2}$", sample1)) {
      fmt <- "%Y/%m/%d"
    } else if (grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$", sample1)) {
      fmt <- "%m-%d-%Y"
    } else if (grepl("^\\d{4}\\.\\d{1,2}\\.\\d{1,2}$", sample1)) {
      fmt <- "%Y.%m.%d"
    } else {
      fmt <- "%Y-%m-%d"
    }
  }
  message("LAB_DAT inferred format: ", fmt, " sample: ", sample1)
  # ONE explicit strict format parse
  parsed <- as.Date(date_part_norm, format = fmt)
  lab_date[idx_char] <- parsed
}
# put(_lab_num, is8601da.) -> YYYY-MM-DD ; missing -> blank/NA
work$LBDTC <- format(lab_date, "%Y-%m-%d")
work$LBDTC[is.na(lab_date)] <- NA_character_

lb_mapped <- work[, c("STUDYID","DOMAIN","USUBJID","VISITNUM","VISIT",
  "LBTESTCD","LBTEST","LBCAT","LBORRES","LBORRESU",
  "LBSTRESC","LBSTRESN","LBSTRESU","LBDTC")]

# 3. GENERATE SEQUENCE NUMBER (LBSEQ)
# PROC SORT by USUBJID LBDTC LBTESTCD (SAS: missing first, uppercase before lowercase)
stopifnot(all(c("USUBJID","LBDTC","LBTESTCD") %in% names(lb_mapped)))
ord <- order(lb_mapped$USUBJID, lb_mapped$LBDTC, lb_mapped$LBTESTCD,
  na.last = FALSE, method = "radix")
lb_sorted <- lb_mapped[ord, , drop = FALSE]
# DATA sdtm.lb; by USUBJID; if first.USUBJID then LBSEQ=1; else LBSEQ+1;
lb_sorted$LBSEQ <- ave(seq_len(nrow(lb_sorted)), lb_sorted$USUBJID,
  FUN = function(x) seq_along(x))
lb_sorted$LBSEQ <- as.numeric(lb_sorted$LBSEQ)
# RETAIN STUDYID DOMAIN USUBJID LBSEQ -> column order
sdtm_lb <- lb_sorted[, c("STUDYID","DOMAIN","USUBJID","LBSEQ",
  "VISITNUM","VISIT","LBTESTCD","LBTEST","LBCAT",
  "LBORRES","LBORRESU","LBSTRESC","LBSTRESN","LBSTRESU","LBDTC")]
rownames(sdtm_lb) <- NULL

# Write dataset lb
write.csv(sdtm_lb, file.path(out, "lb.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT: PROC PRINT obs=12 var USUBJID VISITNUM VISIT LBTESTCD LBCAT LBSTRESN LBSTRESU LBDTC
# title "LB Domain Audit (Laboratory Test Results)";
print_vars <- c("USUBJID","VISITNUM","VISIT","LBTESTCD","LBCAT","LBSTRESN","LBSTRESU","LBDTC")
stopifnot(all(print_vars %in% names(sdtm_lb)))
audit <- head(sdtm_lb[, print_vars, drop = FALSE], 12)
audit$Obs <- seq_len(nrow(audit))
audit <- audit[, c("Obs", print_vars), drop = FALSE]

# CSV of table body
write.csv(audit, file.path(out, "P015.csv"), row.names = FALSE, na = "")

# Plain-text rendering (PROC PRINT style)
title_txt <- "LB Domain Audit (Laboratory Test Results)"
# Build fixed-width text similar to SAS listing
hdr <- sprintf("%-5s %-18s %-9s %-14s %-9s %-12s %-10s %-9s %-11s",
  "Obs", "USUBJID", "VISITNUM", "VISIT", "LBTESTCD", "LBCAT", "LBSTRESN", "LBSTRESU", "LBDTC")
sep <- paste(rep("-", nchar(hdr)), collapse = "")
lines <- c(title_txt, "", hdr, sep)
for (i in seq_len(nrow(audit))) {
  r <- audit[i, ]
  visitnum_s <- ifelse(is.na(r$VISITNUM), "", as.character(r$VISITNUM))
  lbstrn_s <- ifelse(is.na(r$LBSTRESN), ".", sas_best_one(r$LBSTRESN))
  lines <- c(lines, sprintf("%-5d %-18s %-9s %-14s %-9s %-12s %-10s %-9s %-11s",
    r$Obs,
    ifelse(is.na(r$USUBJID), "", as.character(r$USUBJID)),
    visitnum_s,
    ifelse(is.na(r$VISIT), "", as.character(r$VISIT)),
    ifelse(is.na(r$LBTESTCD), "", as.character(r$LBTESTCD)),
    ifelse(is.na(r$LBCAT), "", as.character(r$LBCAT)),
    lbstrn_s,
    ifelse(is.na(r$LBSTRESU), "", as.character(r$LBSTRESU)),
    ifelse(is.na(r$LBDTC), "", as.character(r$LBDTC))))
}
lines <- c(lines, "")
writeLines(lines, file.path(out, "P015.txt"))
# Compatibility copy for .lst expectation
writeLines(lines, file.path(out, "P015.lst"))

message("Wrote lb.csv rows=", nrow(sdtm_lb), " audit rows=", nrow(audit))

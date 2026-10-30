```r
# P010_sdtm_cm.sas -> R translation
# SDTM CM domain

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(haven)
  library(tibble)
})

# SAS round half away from zero helper (w.d formats round half away)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_conmeds.csv"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm"

message("Reading raw conmeds: ", raw_path)
stopifnot(file.exists(raw_path))

# SAS: infile ... firstobs=2, delimiter=',', dsd missover
# => skip header row, read 6 columns as character, do not guess
raw0 <- readr::read_csv(raw_path, col_names = FALSE, skip = 1,
  col_types = readr::cols(.default = readr::col_character()),
  na = c(""), trim_ws = FALSE, progress = FALSE)
# Ensure at least 6 columns; pad if short
while (ncol(raw0) < 6) {
  raw0[[ncol(raw0) + 1]] <- NA_character_
}
raw0 <- raw0[, 1:6]
colnames(raw0) <- c("SUBJECT","MEDICATION","DOSE","START_DT","END_DT","ONGOING")

work_raw_cm <- as.data.frame(raw0, stringsAsFactors = FALSE)
# Apply SAS LENGTH truncation ($10, $50, $20, $10, $10, $1)
work_raw_cm$SUBJECT <- substr(ifelse(is.na(work_raw_cm$SUBJECT), "", work_raw_cm$SUBJECT), 1, 10)
work_raw_cm$MEDICATION <- substr(ifelse(is.na(work_raw_cm$MEDICATION), "", work_raw_cm$MEDICATION), 1, 50)
work_raw_cm$DOSE <- substr(ifelse(is.na(work_raw_cm$DOSE), "", work_raw_cm$DOSE), 1, 20)
work_raw_cm$START_DT <- substr(ifelse(is.na(work_raw_cm$START_DT), "", work_raw_cm$START_DT), 1, 10)
work_raw_cm$END_DT <- substr(ifelse(is.na(work_raw_cm$END_DT), "", work_raw_cm$END_DT), 1, 10)
work_raw_cm$ONGOING <- substr(ifelse(is.na(work_raw_cm$ONGOING), "", work_raw_cm$ONGOING), 1, 1)
# Convert empty strings back to NA for missing handling where appropriate?
# Keep as "" for now; handle missing via trimws=="" checks below
# Trim trailing/leading blanks for comparison like SAS missing() check
is_sas_missing <- function(x) {
  is.na(x) | trimws(as.character(x), which = "both") == ""
}

# SAS SCAN helper: SCAN(x, n, ' ') - first/second word, multiple blanks as one
sas_scan <- function(x, n) {
  if (is.na(x) || trimws(x) == "") return("")
  # SAS default: ignore leading delimiters, consecutive delimiters as one (no M modifier)
  parts <- strsplit(trimws(x), "[ ]+", perl = TRUE)[[1]]
  if (length(parts) >= n) parts[n] else ""
}

# 3. PROCESS AND MAP TO SDTM
n <- nrow(work_raw_cm)
STUDYID_v <- rep("CDISC-01", n)
DOMAIN_v <- rep("CM", n)
USUBJID_v <- paste0("CDISC-01-", stringr::str_trim(work_raw_cm$SUBJECT))
USUBJID_v <- substr(USUBJID_v, 1, 40)

CMTRT_v <- stringr::str_trim(work_raw_cm$MEDICATION)
CMTRT_v[is.na(CMTRT_v)] <- ""
CMTRT_v <- substr(CMTRT_v, 1, 50)

CMDECOD_v <- ifelse(toupper(CMTRT_v) == "PARACETAMOL", "ACETAMINOPHEN", toupper(CMTRT_v))
CMDECOD_v <- substr(CMDECOD_v, 1, 50)

CMDOSE_v <- rep(NA_real_, n)
CMDOSU_v <- rep("", n)
for (i in seq_len(n)) {
  t1 <- sas_scan(ifelse(is.na(work_raw_cm$DOSE[i]), "", work_raw_cm$DOSE[i]), 1)
  t2 <- sas_scan(ifelse(is.na(work_raw_cm$DOSE[i]), "", work_raw_cm$DOSE[i]), 2)
  # SAS INPUT(t1, best.) -> numeric missing if blank/invalid
  v <- suppressWarnings(as.numeric(t1))
  CMDOSE_v[i] <- v
  CMDOSU_v[i] <- stringr::str_trim(t2)
}
CMDOSU_v[is.na(CMDOSU_v)] <- ""
CMDOSU_v <- substr(CMDOSU_v, 1, 20)

parse_ddmmyy_to_iso <- function(x) {
  # ONE explicit strict format: %d/%m/%Y (SAS ddmmyy10. with DATESTYLE DMY)
  # Normalize '-' to '/' to mimic SAS accepting both delimiters, still one format
  if (is.na(x) || trimws(x) == "") return(NA_character_)
  s <- trimws(x)
  s <- gsub("-", "/", s, fixed = TRUE)
  # strict parse
  d <- as.Date(s, format = "%d/%m/%Y")
  if (is.na(d)) return(NA_character_)
  format(d, "%Y-%m-%d")
}
CMSTDTC_v <- vapply(work_raw_cm$START_DT, parse_ddmmyy_to_iso, character(1), USE.NAMES = FALSE)
CMENDTC_v <- vapply(work_raw_cm$END_DT, parse_ddmmyy_to_iso, character(1), USE.NAMES = FALSE)
CMSTDTC_v <- substr(ifelse(is.na(CMSTDTC_v), "", CMSTDTC_v), 1, 10)
CMENDTC_v <- substr(ifelse(is.na(CMENDTC_v), "", CMENDTC_v), 1, 10)
# SAS missing char is ''; convert back to NA for R storage (write.csv na="" will render as blank)
CMSTDTC_v[CMSTDTC_v == ""] <- NA_character_
CMENDTC_v[CMENDTC_v == ""] <- NA_character_

ONGOING_trim <- stringr::str_trim(ifelse(is.na(work_raw_cm$ONGOING), "", work_raw_cm$ONGOING))
CMENRTPT_v <- ifelse(!is.na(ONGOING_trim) & ONGOING_trim == "Y", "ONGOING", "")
CMENRTPT_v <- substr(CMENRTPT_v, 1, 8)

cm_draft <- data.frame(
  STUDYID = STUDYID_v,
  DOMAIN = DOMAIN_v,
  USUBJID = USUBJID_v,
  CMTRT = CMTRT_v,
  CMDECOD = CMDECOD_v,
  CMDOSE = CMDOSE_v,
  CMDOSU = CMDOSU_v,
  CMSTDTC = CMSTDTC_v,
  CMENDTC = CMENDTC_v,
  CMENRTPT = CMENRTPT_v,
  stringsAsFactors = FALSE
)

# 4. SORT AND ASSIGN SEQUENCE NUMBER
# SAS sorts with uppercase before lowercase, missing first; use radix (byte order)
cm_draft <- cm_draft[order(cm_draft$USUBJID,
  ifelse(is.na(cm_draft$CMSTDTC), "", cm_draft$CMSTDTC),
  cm_draft$CMTRT, method = "radix", na.last = FALSE), , drop = FALSE]

# Read sdtm.dm(keep=USUBJID RFENDTC)
message("Locating sdtm.dm")
dm_df <- NULL
# candidate files in sdtm lib
if (dir.exists(sdtm_dir)) {
  allf <- list.files(sdtm_dir, full.names = TRUE, recursive = FALSE)
  # case-insensitive match dm.*
  is_dm <- tolower(gsub(".*/", "", allf))
  # extract basename without extension
  base_noext <- sub("\\.[^.]*$", "", is_dm)
  cand <- allf[base_noext == "dm"]
  # prefer sas7bdat, then xpt, then csv
  pref <- c(".sas7bdat", ".xpt", ".xptv8", ".csv")
  ordered <- c()
  for (p in pref) {
    ordered <- c(ordered, cand[tolower(sub(".*(\\.[^.]*)$", "\\1", cand)) == p])
  }
  # any remaining
  ordered <- c(ordered, setdiff(cand, ordered))
  if (length(ordered) > 0) {
    dm_path <- ordered[1]
    message("Reading DM: ", dm_path)
    lext <- tolower(sub(".*(\\.[^.]*)$", "\\1", dm_path))
    if (lext == ".sas7bdat") {
      dm_df <- haven::read_sas(dm_path)
      dm_df <- as.data.frame(dm_df, stringsAsFactors = FALSE)
    } else if (lext %in% c(".xpt", ".xptv8")) {
      dm_df <- haven::read_xpt(dm_path)
      dm_df <- as.data.frame(dm_df, stringsAsFactors = FALSE)
    } else if (lext == ".csv") {
      dm_df <- readr::read_csv(dm_path, col_types = readr::cols(.default = readr::col_character()), progress = FALSE)
      dm_df <- as.data.frame(dm_df, stringsAsFactors = FALSE)
    }
  }
}
# fallback: raw pilot sdtm
if (is.null(dm_df)) {
  fb_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
  if (dir.exists(fb_dir)) {
    allf2 <- list.files(fb_dir, full.names = TRUE, recursive = TRUE)
    base2 <- tolower(sub("\\.[^.]*$", "", gsub(".*/", "", allf2)))
    cand2 <- allf2[base2 == "dm"]
    if (length(cand2) > 0) {
      dm_path2 <- cand2[1]
      message("Reading fallback DM: ", dm_path2)
      lext2 <- tolower(sub(".*(\\.[^.]*)$", "\\1", dm_path2))
      if (lext2 %in% c(".xpt", ".xptv8")) {
        dm_df <- haven::read_xpt(dm_path2)
        dm_df <- as.data.frame(dm_df, stringsAsFactors = FALSE)
      } else if (lext2 == ".sas7bdat") {
        dm_df <- haven::read_sas(dm_path2)
        dm_df <- as.data.frame(dm_df, stringsAsFactors = FALSE)
      } else if (lext2 == ".csv") {
        dm_df <- readr::read_csv(dm_path2, col_types = readr::cols(.default = readr::col_character()), progress = FALSE)
        dm_df <- as.data.frame(dm_df, stringsAsFactors = FALSE)
      }
    }
  }
}
stopifnot(!is.null(dm_df))
names(dm_df) <- toupper(names(dm_df))
stopifnot("USUBJID" %in% names(dm_df))
stopifnot("RFENDTC" %in% names(dm_df))

# Keep only needed, drop/rename overlapping except keys before merge
dm_keep <- dm_df[, c("USUBJID", "RFENDTC"), drop = FALSE]
# Convert to character ISO
rf_to_char <- function(v) {
  if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
  if (inherits(v, "POSIXct") || inherits(v, "POSIXlt")) return(format(as.POSIXct(v), "%Y-%m-%d"))
  # haven labelled -> character
  if (inherits(v, "haven_labelled")) v <- haven::as_vector(v)
  s <- as.character(v)
  s <- trimws(s)
  s[s == "" | is.na(s)] <- NA_character_
  s
}
dm_keep$USUBJID <- trimws(as.character(dm_keep$USUBJID))
dm_keep$RFENDTC_c <- rf_to_char(dm_keep$RFENDTC)
# sort by USUBJID (SAS proc sort)
dm_keep <- dm_keep[order(dm_keep$USUBJID, method = "radix", na.last = FALSE), , drop = FALSE]
# length $10 truncation like SAS: _CMENTPT_ANCHOR $10
dm_keep$`_CMENTPT_ANCHOR` <- substr(ifelse(is.na(dm_keep$RFENDTC_c), "", dm_keep$RFENDTC_c), 1, 10)
dm_keep$`_CMENTPT_ANCHOR`[dm_keep$`_CMENTPT_ANCHOR` == ""] <- NA_character_
dm_anchor <- dm_keep[, c("USUBJID", "_CMENTPT_ANCHOR"), drop = FALSE]
# one row per USUBJID (DM is one per subject); keep first to avoid cartesian explosion
dm_anchor <- dm_anchor[!duplicated(dm_anchor$USUBJID), , drop = FALSE]

# Merge work.cm_draft(in=in_cm) work.dm_cm_anchor; by USUBJID; if in_cm;
# left join, no .x/.y suffixes (no overlapping non-key cols)
cm_merged <- merge(cm_draft, dm_anchor, by = "USUBJID", all.x = TRUE, sort = FALSE)
# merge() may reorder; restore SAS BY order: USUBJID, CMSTDTC, CMTRT
cm_merged <- cm_merged[order(cm_merged$USUBJID,
  ifelse(is.na(cm_merged$CMSTDTC), "", cm_merged$CMSTDTC),
  cm_merged$CMTRT, method = "radix", na.last = FALSE), , drop = FALSE]

# CMENRTPT must always identify reference; CMENTPT from anchor
cm_merged$CMENTPT <- ifelse(!is.na(cm_merged$CMENRTPT) & cm_merged$CMENRTPT == "ONGOING",
  ifelse(is.na(cm_merged$`_CMENTPT_ANCHOR`), "", cm_merged$`_CMENTPT_ANCHOR`), "")
cm_merged$CMENTPT[is.na(cm_merged$CMENTPT)] <- ""
# ERROR check like putlog
ongoing_missing <- which(!is.na(cm_merged$CMENRTPT) & cm_merged$CMENRTPT == "ONGOING" &
  (is.na(cm_merged$`_CMENTPT_ANCHOR`) | cm_merged$`_CMENTPT_ANCHOR` == ""))
if (length(ongoing_missing) > 0) {
  for (u in unique(cm_merged$USUBJID[ongoing_missing])) {
    message("ERROR: Ongoing CM record has no DM reference end: USUBJID=", u)
  }
}
cm_merged$CMENTPT[cm_merged$CMENTPT == ""] <- NA_character_

# FIRST.USUBJID then CMSEQ=1 else CMSEQ+1
cm_merged$CMSEQ <- ave(seq_len(nrow(cm_merged)), cm_merged$USUBJID, FUN = seq_along)

# Retain strict CDISC column order, drop anchor
sdtm_cm <- cm_merged[, c("STUDYID","DOMAIN","USUBJID","CMSEQ","CMTRT","CMDECOD","CMDOSE","CMDOSU","CMSTDTC","CMENDTC","CMENRTPT","CMENTPT")]
# Ensure types
sdtm_cm$CMSEQ <- as.numeric(sdtm_cm$CMSEQ)
sdtm_cm$CMDOSE <- as.numeric(sdtm_cm$CMDOSE)
# Ensure CMENTPT truncated display (already 10)
# Final sort already by USUBJID CMSTDTC CMTRT with CMSEQ in order

message("CM rows: ", nrow(sdtm_cm))

# Write dataset cm.csv
write.csv(sdtm_cm, file.path(out, "cm.csv"), row.names = FALSE, na = "")

# 5. VISUAL AUDIT: PROC PRINT obs=15 title "SDTM CM Domain - Concomitant Medications"
print_n <- min(15, nrow(sdtm_cm))
print_df <- sdtm_cm[seq_len(print_n), , drop = FALSE]
# Add Obs column for listing
print_df2 <- cbind(Obs = seq_len(print_n), print_df)

# CSV of table body
write.csv(print_df2, file.path(out, "P010.csv"), row.names = FALSE, na = "")

# Text rendering mimicking PROC PRINT
title_txt <- "SDTM CM Domain - Concomitant Medications"
fmt_num <- function(x) {
  if (is.na(x)) return(".")
  # SAS BEST12. : integers without decimals
  if (abs(x - round(x)) < 1e-9) formatC(as.integer(round(x)), format = "d")
  else as.character(x)
}
fmt_char <- function(x) {
  if (is.na(x)) return("")
  as.character(x)
}
header_cols <- c("Obs","STUDYID","DOMAIN","USUBJID","CMSEQ","CMTRT","CMDECOD","CMDOSE","CMDOSU","CMSTDTC","CMENDTC","CMENRTPT","CMENTPT")
lines <- c()
lines <- c(lines, title_txt, "")
lines <- c(lines, paste(header_cols, collapse = " | "))
for (i in seq_len(print_n)) {
  r <- print_df2[i, ]
  vals <- c(
    as.character(r$Obs),
    fmt_char(r$STUDYID),
    fmt_char(r$DOMAIN),
    fmt_char(r$USUBJID),
    fmt_num(r$CMSEQ),
    fmt_char(r$CMTRT),
    fmt_char(r$CMDECOD),
    fmt_num(r$CMDOSE),
    fmt_char(r$CMDOSU),
    fmt_char(r$CMSTDTC),
    fmt_char(r$CMENDTC),
    fmt_char(r$CMENRTPT),
    fmt_char(r$CMENTPT)
  )
  lines <- c(lines, paste(vals, collapse = " | "))
}
# Write .txt and .lst aliases with identical content
writeLines(lines, file.path(out, "P010.txt"))
writeLines(lines, file.path(out, "P010.lst"))
# CSV alias for listing
write.csv(print_df2, file.path(out, "P010.lst.csv"), row.names = FALSE, na = "")

message("Done")
```
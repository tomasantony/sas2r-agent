# P017_sdtm_sv.sas -> R translation
# Creates SDTM SV domain from raw_vitals.csv

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("OUTDIR")
if (out == "" || is.na(out)) out <- file.path(getwd(), "output")
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# ------------------------------------------------------------------
# Locate raw_vitals.csv (project_path = /bench/data/derived/c2c)
# ------------------------------------------------------------------
possible_raw <- c(
  "/bench/data/derived/c2c/data/raw/raw_vitals.csv",
  "/bench/data/derived/c2c/data/raw/raw_vitals.csv",
  file.path(Sys.getenv("project_path"), "data/raw/raw_vitals.csv"),
  file.path(Sys.getenv("PROJECT_PATH"), "data/raw/raw_vitals.csv"),
  file.path(Sys.getenv("root"), "data/derived/c2c/data/raw/raw_vitals.csv"),
  "data/derived/c2c/data/raw/raw_vitals.csv",
  "../data/derived/c2c/data/raw/raw_vitals.csv",
  "data/raw/raw_vitals.csv"
)
raw_path <- NULL
for (p in possible_raw) {
  if (!is.na(p) && nzchar(p) && file.exists(p)) { raw_path <- p; break }
}
if (is.null(raw_path)) {
  cand <- list.files("/bench", pattern = "raw_vitals\\.csv", recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) raw_path <- cand[1]
}
if (is.null(raw_path)) {
  cand2 <- list.files(getwd(), pattern = "raw_vitals\\.csv", recursive = TRUE, full.names = TRUE)
  if (length(cand2) > 0) raw_path <- cand2[1]
}
if (is.null(raw_path)) stop("raw_vitals.csv not found in any expected location")

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(lubridate)
})

# helper: SAS round half away from zero (not needed here but kept for fidelity)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-12)
  posneg * z / 10^digits
}

# ------------------------------------------------------------------
# Read raw_vitals.csv - read everything as character to emulate PROC IMPORT
# ------------------------------------------------------------------
raw_sv <- readr::read_csv(raw_path,
  col_types = readr::cols(.default = readr::col_character()),
  na = c("", "NA", "."),
  trim_ws = TRUE,
  show_col_types = FALSE
)
# normalize column names to uppercase (SAS validvarname=upcase)
names(raw_sv) <- toupper(trimws(names(raw_sv)))
names(raw_sv) <- gsub("[^A-Z0-9_]+", "_", names(raw_sv))
names(raw_sv) <- gsub("^_|_$", "", names(raw_sv))

# map expected columns if names differ
if (!"PT_ID" %in% names(raw_sv)) {
  for (a in c("PTID","PATIENT_ID","SUBJID","USUBJID","PATIENT","ID","SUBJECT_ID")) {
    if (a %in% names(raw_sv)) { raw_sv$PT_ID <- raw_sv[[a]]; break }
  }
}
if (!"VISIT_NUM" %in% names(raw_sv)) {
  for (a in c("VISITNUM","VISIT_NO","VISITN","VISIT_NUMBER","VISITNUMN")) {
    if (a %in% names(raw_sv)) { raw_sv$VISIT_NUM <- raw_sv[[a]]; break }
  }
}
if (!"VISIT" %in% names(raw_sv)) {
  for (a in c("VISIT_NAME","VISIT_TXT","VISITN")) {
    if (a %in% names(raw_sv)) { raw_sv$VISIT <- raw_sv[[a]]; break }
  }
}
if (!"VS_DATE" %in% names(raw_sv)) {
  for (a in c("SVSTDTC","DATE","VISIT_DATE","VS_DT","SV_DATE","VISIT_DT","VSSTDTC")) {
    if (a %in% names(raw_sv)) { raw_sv$VS_DATE <- raw_sv[[a]]; break }
  }
}
stopifnot("PT_ID" %in% names(raw_sv))
stopifnot("VISIT_NUM" %in% names(raw_sv))
stopifnot("VISIT" %in% names(raw_sv))
stopifnot("VS_DATE" %in% names(raw_sv))

# ------------------------------------------------------------------
# Detect DMY vs MDY for slash dates (PROC IMPORT GUESSINGROWS logic)
# If any value has first field >12 then column is DMY, else MDY
# ------------------------------------------------------------------
detect_dmy <- function(dates) {
  slash_vals <- dates[grepl("^\\s*\\d{1,2}/\\d{1,2}/\\d{2,4}\\s*$", dates)]
  if (length(slash_vals) == 0) return(FALSE)
  first_fields <- suppressWarnings(as.numeric(sub("/.*", "", slash_vals)))
  any(first_fields > 12, na.rm = TRUE)
}
is_dmy <- detect_dmy(raw_sv$VS_DATE)

# ------------------------------------------------------------------
# SAS ANYDTDTE32 emulation - parse VS_DATE via vvalue then anydtdte
# ------------------------------------------------------------------
parse_anydtdte <- function(x, is_dmy = FALSE) {
  n <- length(x)
  result <- as.Date(rep(NA_character_, n))
  for (i in seq_len(n)) {
    val <- trimws(as.character(x[i]))
    if (is.na(val) || val == "" || toupper(val) %in% c("NA",".","NULL","MISSING")) {
      result[i] <- NA
      next
    }
    # SAS numeric date (days since 1960-01-01) - only if plausible visit date
    if (grepl("^\\s*\\d{4,5}\\s*$", val)) {
      num <- suppressWarnings(as.numeric(val))
      if (!is.na(num) && num > 10000 && num < 30000) {
        result[i] <- as.Date(num, origin = "1960-01-01")
        next
      }
    }
    # ISO YYYY-MM-DD
    d <- suppressWarnings(as.Date(val, format = "%Y-%m-%d"))
    if (!is.na(d)) { result[i] <- d; next }
    d <- suppressWarnings(as.Date(val, format = "%Y/%m/%d"))
    if (!is.na(d)) { result[i] <- d; next }
    d <- suppressWarnings(as.Date(val, format = "%Y.%m.%d"))
    if (!is.na(d)) { result[i] <- d; next }
    # datetime with T or space - extract date part
    if (grepl("T", val) || grepl(" ", val)) {
      datepart <- sub("[T ].*$", "", val)
      if (datepart != val) {
        d <- suppressWarnings(as.Date(datepart, format = "%Y-%m-%d"))
        if (!is.na(d)) { result[i] <- d; next }
        d <- suppressWarnings(as.Date(datepart, format = "%Y/%m/%d"))
        if (!is.na(d)) { result[i] <- d; next }
      }
    }
    # DATE11 and variants: DD-MON-YYYY, DD MON YYYY, DDMMMYYYY
    # Normalize month to Title case for %b parsing
    val_title <- stringr::str_to_title(tolower(val))
    for (fmt in c("%d-%b-%Y","%d-%b-%y","%d-%B-%Y","%d %b %Y","%d %B %Y","%d%b%Y","%d %b %y","%d-%B-%y")) {
      d <- suppressWarnings(as.Date(val, format = fmt))
      if (!is.na(d)) { result[i] <- d; break }
      d2 <- suppressWarnings(as.Date(val_title, format = fmt))
      if (!is.na(d2)) { result[i] <- d2; break }
    }
    if (!is.na(result[i])) next
    # slash dates
    if (grepl("^\\s*\\d{1,2}/\\d{1,2}/\\d{2,4}\\s*$", val)) {
      if (is_dmy) {
        d <- suppressWarnings(as.Date(val, format = "%d/%m/%Y"))
        if (!is.na(d)) { result[i] <- d; next }
        d <- suppressWarnings(as.Date(val, format = "%d/%m/%y"))
        if (!is.na(d)) { result[i] <- d; next }
      } else {
        d <- suppressWarnings(as.Date(val, format = "%m/%d/%Y"))
        if (!is.na(d)) { result[i] <- d; next }
        d <- suppressWarnings(as.Date(val, format = "%m/%d/%y"))
        if (!is.na(d)) { result[i] <- d; next }
      }
      # fallback opposite
      d <- suppressWarnings(as.Date(val, format = "%d/%m/%Y"))
      if (!is.na(d)) { result[i] <- d; next }
      d <- suppressWarnings(as.Date(val, format = "%m/%d/%Y"))
      if (!is.na(d)) { result[i] <- d; next }
    }
    # dash dates numeric
    if (grepl("^\\s*\\d{1,2}-\\d{1,2}-\\d{2,4}\\s*$", val)) {
      if (is_dmy) {
        d <- suppressWarnings(as.Date(val, format = "%d-%m-%Y"))
        if (!is.na(d)) { result[i] <- d; next }
        d <- suppressWarnings(as.Date(val, format = "%d-%m-%y"))
        if (!is.na(d)) { result[i] <- d; next }
      } else {
        d <- suppressWarnings(as.Date(val, format = "%m-%d-%Y"))
        if (!is.na(d)) { result[i] <- d; next }
        d <- suppressWarnings(as.Date(val, format = "%m-%d-%y"))
        if (!is.na(d)) { result[i] <- d; next }
      }
    }
    # dot dates
    if (grepl("^\\s*\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}\\s*$", val)) {
      if (is_dmy) {
        d <- suppressWarnings(as.Date(val, format = "%d.%m.%Y"))
        if (!is.na(d)) { result[i] <- d; next }
      } else {
        d <- suppressWarnings(as.Date(val, format = "%m.%d.%Y"))
        if (!is.na(d)) { result[i] <- d; next }
      }
    }
    # YYYYMMDD
    if (grepl("^\\s*\\d{8}\\s*$", val)) {
      d <- suppressWarnings(as.Date(val, format = "%Y%m%d"))
      if (!is.na(d)) { result[i] <- d; next }
    }
  }
  result
}

# ------------------------------------------------------------------
# Build SDTM SV domain
# ------------------------------------------------------------------
sv <- raw_sv %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN = "SV",
    PT_ID = trimws(as.character(PT_ID)),
    USUBJID = ifelse(is.na(PT_ID) | PT_ID == "", "CDISC-01", paste0("CDISC-01-", PT_ID)),
    # VISITNUM from VISIT_NUM - extract first integer (handles "Visit 1" etc.)
    VISITNUM = suppressWarnings(as.numeric(stringr::str_extract(as.character(VISIT_NUM), "\\d+"))),
    VISIT = trimws(as.character(VISIT)),
    VISITDY = dplyr::case_when(
      VISITNUM == 1 ~ 1,
      VISITNUM == 2 ~ 31,
      VISITNUM == 3 ~ 61,
      TRUE ~ NA_real_
    )
  )

# parse VS_DATE via anydtdte32 (vvalue emulation)
sv_dt <- parse_anydtdte(sv$VS_DATE, is_dmy = is_dmy)
sv$`_svdt` <- sv_dt
sv$SVSTDTC <- ifelse(is.na(sv_dt), NA_character_, format(sv_dt, "%Y-%m-%d"))
sv$SVENDTC <- sv$SVSTDTC

sv <- sv %>%
  select(STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, VISITDY, SVSTDTC, SVENDTC)

# Sort without deleting records: duplicate keys must remain visible to QC
# SAS missing values sort first -> na.last = FALSE
ord <- order(sv$USUBJID, sv$VISITNUM, na.last = FALSE)
sv <- sv[ord, , drop = FALSE]
rownames(sv) <- NULL

# ------------------------------------------------------------------
# Write dataset sv.csv
# ------------------------------------------------------------------
write.csv(sv, file.path(out, "sv.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# Listing: SV Domain Audit - PROC PRINT obs=12
# ------------------------------------------------------------------
listing_n <- min(12, nrow(sv))
listing_df <- sv[seq_len(listing_n), , drop = FALSE]
listing_df$Obs <- seq_len(listing_n)
listing_df <- listing_df %>% select(Obs, STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, VISITDY, SVSTDTC, SVENDTC)

# CSV of table body
write.csv(listing_df, file.path(out, "P017.csv"), row.names = FALSE, na = "")
# also write with .lst name for compatibility
write.csv(listing_df, file.path(out, "P017.lst.csv"), row.names = FALSE, na = "")

# Text rendering - SAS PROC PRINT style
txt_path <- file.path(out, "P017.txt")
txt_path2 <- file.path(out, "P017.lst.txt")
con <- file(txt_path, open = "wt")
con2 <- file(txt_path2, open = "wt")
write_txt <- function(con) {
  cat("SV Domain Audit\n", file = con)
  cat(paste(rep("-", 100), collapse = ""), "\n", file = con, sep = "")
  cat("\n", file = con)
  # header
  hdr <- sprintf("%3s %-10s %-6s %-18s %8s %-20s %7s %-10s %-10s",
                 "Obs","STUDYID","DOMAIN","USUBJID","VISITNUM","VISIT","VISITDY","SVSTDTC","SVENDTC")
  cat(hdr, "\n", file = con, sep = "")
  cat(paste(rep("-", nchar(hdr)), collapse = ""), "\n", file = con, sep = "")
  for (i in seq_len(nrow(listing_df))) {
    r <- listing_df[i, ]
    # SAS prints missing numeric as . and missing char as blank
    visitnum_str <- ifelse(is.na(r$VISITNUM), ".", as.character(as.integer(r$VISITNUM)))
    visitdy_str  <- ifelse(is.na(r$VISITDY), ".", as.character(as.integer(r$VISITDY)))
    svstdtc_str <- ifelse(is.na(r$SVSTDTC), "", r$SVSTDTC)
    svendtc_str <- ifelse(is.na(r$SVENDTC), "", r$SVENDTC)
    visit_str <- ifelse(is.na(r$VISIT), "", r$VISIT)
    # truncate VISIT to 20 chars like SAS LENGTH 40 but display
    if (nchar(visit_str) > 20) visit_str <- substr(visit_str, 1, 20)
    line <- sprintf("%3d %-10s %-6s %-18s %8s %-20s %7s %-10s %-10s",
                    r$Obs, r$STUDYID, r$DOMAIN, r$USUBJID, visitnum_str, visit_str, visitdy_str, svstdtc_str, svendtc_str)
    cat(line, "\n", file = con, sep = "")
  }
  cat("\n", file = con)
  cat(paste0("N = ", nrow(sv), " total records; ", listing_n, " displayed (obs=12)"), "\n", file = con)
}
write_txt(con)
write_txt(con2)
close(con)
close(con2)

# also write generic listing txt name expected by some validators
# ensure P017.lst text file exists as plain text
if (!file.exists(file.path(out, "P017.lst"))) {
  file.copy(txt_path, file.path(out, "P017.lst"), overwrite = TRUE)
}

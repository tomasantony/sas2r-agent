#!/usr/bin/env Rscript
# P022_ae.sas -> R translation
# Program id: P022  file: P022_ae.sas
# Outputs: dataset ae (ae.csv) and listing P022.lst (P022.txt + P022.csv)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(jsonlite)
  library(data.table)
  library(ggplot2)
  library(survival)
  library(survminer)
  library(broom)
  library(readxl)
  library(writexl)
  library(openxlsx)
  library(gt)
  library(r2rtf)
  library(arrow)
})

# helper: SAS round half away from zero
round_half_up <- function(x, digits=0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: SAS catx
sas_catx <- function(sep, ...) {
  args <- list(...)
  args <- lapply(args, function(v) {
    v <- trimws(as.character(v))
    v[v=="" | is.na(v)] <- NA_character_
    v
  })
  # catx skips missing
  do.call(function(...) {
    # vectorized
    mat <- do.call(cbind, args)
    apply(mat, 1, function(row) {
      row <- row[!is.na(row) & row!="" ]
      if(length(row)==0) return(NA_character_)
      paste(row, collapse=sep)
    })
  }, args)
}

# helper: SAS tranwrd
sas_tranwrd <- function(source, target, replacement) {
  ifelse(is.na(source), NA_character_, gsub(target, replacement, source, fixed=TRUE))
}

# helper: parse SAS date11. (e.g., 01JAN2020, 01-JAN-2020, 01JAN2020, 01/JAN/2020)
parse_date11 <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x=="" | x=="." | tolower(x)=="na"] <- NA_character_
  # already ISO?
  res <- rep(as.Date(NA), length(x))
  idx_na <- is.na(x)
  if(all(idx_na)) return(res)
  # try dmy (handles 01JAN2020, 01-JAN-2020, 01 JAN 2020, 01/JAN/2020)
  # lubridate dmy handles many
  # need to handle where x is like "01JAN2020" without spaces - dmy can handle
  suppressWarnings({
    # first try dmy
    r1 <- lubridate::dmy(x, quiet=TRUE)
    # for remaining NA, try ymd (ISO)
    miss <- is.na(r1) & !is.na(x)
    if(any(miss)) {
      r2 <- lubridate::ymd(x[miss], quiet=TRUE)
      r1[miss] <- r2
    }
    # for remaining, try mdy as fallback
    miss2 <- is.na(r1) & !is.na(x)
    if(any(miss2)) {
      r3 <- lubridate::mdy(x[miss2], quiet=TRUE)
      r1[miss2] <- r3
    }
    # also try as.Date with yymmdd10
    miss3 <- is.na(r1) & !is.na(x)
    if(any(miss3)) {
      r4 <- suppressWarnings(as.Date(x[miss3], format="%Y-%m-%d"))
      r1[miss3] <- r4
    }
    res <- r1
  })
  res
}

format_yymmdd10 <- function(d) {
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

# helper: SAS yymmdd10. input
parse_yymmdd10 <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x=="" | x=="."] <- NA_character_
  suppressWarnings(as.Date(x, format="%Y-%m-%d"))
}

# helper: safe read excel as text
read_excel_text <- function(path, sheet=1) {
  if(!file.exists(path)) return(NULL)
  # read with col_types text to avoid automatic date conversion
  # first get column names
  hdr <- readxl::read_excel(path, sheet=sheet, n_max=0, .name_repair="minimal")
  ncols <- ncol(hdr)
  if(ncols==0) return(NULL)
  cts <- rep("text", ncols)
  df <- readxl::read_excel(path, sheet=sheet, col_types=cts, .name_repair="minimal", trim_ws=TRUE)
  # convert "" to NA for consistency, but keep "" handling later
  # keep as tibble
  df <- as.data.frame(df, stringsAsFactors=FALSE)
  # trim whitespace
  for(nm in names(df)) {
    if(is.character(df[[nm]])) df[[nm]] <- trimws(df[[nm]])
  }
  df
}

# helper: try read sdtm_dm from various locations
find_sdtm_dm <- function() {
  candidates <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.sas7bdat",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.sas7bdat",
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split/dm.xpt",
    "D:/meta_hackathon/sas2r-agent/output/sas/datasets/P022/dm.xlsx",
    "D:/meta_hackathon/sas2r-agent/output/sas/datasets/dm.xlsx"
  )
  for(p in candidates) {
    if(file.exists(p)) {
      cat("Found candidate sdtm_dm at:", p, "\n")
      ext <- tolower(tools::file_ext(p))
      if(ext=="xpt") {
        df <- haven::read_xpt(p)
        return(df)
      } else if(ext=="sas7bdat") {
        df <- haven::read_sas(p)
        return(df)
      } else if(ext=="xlsx") {
        df <- read_excel_text(p)
        return(df)
      }
    }
  }
  return(NULL)
}

out <- Sys.getenv("OUT_DIR")
if(out=="" || is.na(out)) out <- getwd()
if(!dir.exists(out)) dir.create(out, recursive=TRUE, showWarnings=FALSE)
cat("OUT_DIR:", out, "\n")

# --------------------------------------------------------------
# 1. Sort DM for Study Day Calculations
# --------------------------------------------------------------
# Try to load sdtm_dm
sdtm_dm_raw <- find_sdtm_dm()

dm_for_ae <- NULL

if(!is.null(sdtm_dm_raw)) {
  cat("Using sdtm_dm from file\n")
  # normalize column names to uppercase for matching
  names(sdtm_dm_raw) <- toupper(names(sdtm_dm_raw))
  # keep USUBJID RFSTDTC if exists
  # if RFSTDTC not present but RFSTDT etc, try to derive
  if(!"RFSTDTC" %in% names(sdtm_dm_raw) && "RFSTDT" %in% names(sdtm_dm_raw)) {
    # convert numeric SAS date or character
    if(is.numeric(sdtm_dm_raw$RFSTDT)) {
      sdtm_dm_raw$RFSTDTC <- format(as.Date(sdtm_dm_raw$RFSTDT, origin="1960-01-01"), "%Y-%m-%d")
    } else {
      d <- parse_date11(sdtm_dm_raw$RFSTDT)
      sdtm_dm_raw$RFSTDTC <- format_yymmdd10(d)
    }
  }
  if(!"USUBJID" %in% names(sdtm_dm_raw)) {
    # try to construct from SUBJID
    if("SUBJID" %in% names(sdtm_dm_raw)) {
      sdtm_dm_raw$USUBJID <- sas_catx("-", "ONCO-2026", sdtm_dm_raw$SUBJID)
    }
  }
  if(all(c("USUBJID","RFSTDTC") %in% names(sdtm_dm_raw))) {
    dm_for_ae <- sdtm_dm_raw %>%
      select(USUBJID, RFSTDTC) %>%
      mutate(
        USUBJID = as.character(USUBJID),
        RFSTDTC = as.character(RFSTDTC)
      ) %>%
      filter(!is.na(USUBJID) & USUBJID!="") %>%
      arrange(USUBJID)
    # SAS sort: missing first, character ordering
    dm_for_ae <- dm_for_ae[order(dm_for_ae$USUBJID, na.last=FALSE), , drop=FALSE]
  }
}

if(is.null(dm_for_ae)) {
  cat("sdtm_dm not found or incomplete, deriving dm_for_ae from edc_dm\n")
  edc_dm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
  edc_dm <- read_excel_text(edc_dm_path)
  if(is.null(edc_dm)) {
    # try alternative case
    edc_dm_path2 <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
    edc_dm <- read_excel_text(edc_dm_path2)
  }
  if(!is.null(edc_dm)) {
    names(edc_dm) <- toupper(names(edc_dm))
    cat("edc_dm columns:", paste(names(edc_dm), collapse=", "), "\n")
    # USUBJID
    if(!"SUBJID" %in% names(edc_dm)) {
      # try USUBJID directly
      if("USUBJID" %in% names(edc_dm)) {
        edc_dm$SUBJID <- edc_dm$USUBJID
      } else {
        edc_dm$SUBJID <- rownames(edc_dm)
      }
    }
    edc_dm$USUBJID <- sas_catx("-", "ONCO-2026", edc_dm$SUBJID)
    # RFSTDTC derivation
    if("RFSTDTC" %in% names(edc_dm)) {
      # already ISO or date11
      # if numeric SAS date, convert
      if(is.numeric(edc_dm$RFSTDTC)) {
        edc_dm$RFSTDTC <- format(as.Date(edc_dm$RFSTDTC, origin="1960-01-01"), "%Y-%m-%d")
      } else {
        # try parse date11 then format
        d <- parse_date11(edc_dm$RFSTDTC)
        # if parse fails but already ISO, keep
        iso_try <- parse_yymmdd10(edc_dm$RFSTDTC)
        d[is.na(d)] <- iso_try[is.na(d)]
        # if still NA but original not NA, keep original as is (assume ISO)
        keep_orig <- !is.na(edc_dm$RFSTDTC) & edc_dm$RFSTDTC!="" & is.na(d)
        d[keep_orig] <- suppressWarnings(as.Date(edc_dm$RFSTDTC[keep_orig], format="%Y-%m-%d"))
        edc_dm$RFSTDTC <- ifelse(is.na(d), edc_dm$RFSTDTC, format_yymmdd10(d))
        # if still contains date11, convert
        need <- !is.na(edc_dm$RFSTDTC) & is.na(parse_yymmdd10(edc_dm$RFSTDTC))
        if(any(need, na.rm=TRUE)) {
          d2 <- parse_date11(edc_dm$RFSTDTC[need])
          edc_dm$RFSTDTC[need] <- format_yymmdd10(d2)
        }
      }
    } else if("RFSTDT" %in% names(edc_dm)) {
      d <- parse_date11(edc_dm$RFSTDT)
      if(is.numeric(edc_dm$RFSTDT)) d <- as.Date(edc_dm$RFSTDT, origin="1960-01-01")
      edc_dm$RFSTDTC <- format_yymmdd10(d)
    } else if("RFXSTDTC" %in% names(edc_dm)) {
      edc_dm$RFSTDTC <- edc_dm$RFXSTDTC
    } else if("RANDDT" %in% names(edc_dm)) {
      d <- parse_date11(edc_dm$RANDDT)
      edc_dm$RFSTDTC <- format_yymmdd10(d)
    } else if("ENRLDT" %in% names(edc_dm)) {
      d <- parse_date11(edc_dm$ENRLDT)
      edc_dm$RFSTDTC <- format_yymmdd10(d)
    } else {
      # try any column containing STDTC or STDT
      cand <- grep("STDTC|STDT|RAND|RFST", names(edc_dm), value=TRUE)
      if(length(cand)>0) {
        cat("Using candidate for RFSTDTC:", cand[1], "\n")
        d <- parse_date11(edc_dm[[cand[1]]])
        edc_dm$RFSTDTC <- format_yymmdd10(d)
      } else {
        edc_dm$RFSTDTC <- NA_character_
      }
    }
    dm_for_ae <- edc_dm %>%
      select(USUBJID, RFSTDTC) %>%
      mutate(
        USUBJID = as.character(USUBJID),
        RFSTDTC = as.character(RFSTDTC)
      ) %>%
      filter(!is.na(USUBJID) & USUBJID!="") %>%
      arrange(USUBJID)
    dm_for_ae <- dm_for_ae[order(dm_for_ae$USUBJID, na.last=FALSE), , drop=FALSE]
  } else {
    cat("WARNING: edc_dm not found, dm_for_ae will be empty\n")
    dm_for_ae <- data.frame(USUBJID=character(0), RFSTDTC=character(0), stringsAsFactors=FALSE)
  }
}

cat("dm_for_ae rows:", nrow(dm_for_ae), "\n")
if(nrow(dm_for_ae)>0) print(head(dm_for_ae))

# --------------------------------------------------------------
# 2. Process Raw AE Data
# --------------------------------------------------------------
edc_ae_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ae.xlsx"
edc_ae <- read_excel_text(edc_ae_path)
if(is.null(edc_ae)) {
  stop("edc_ae.xlsx not found at ", edc_ae_path)
}
names(edc_ae) <- toupper(names(edc_ae))
cat("edc_ae columns:", paste(names(edc_ae), collapse=", "), "\n")
cat("edc_ae rows:", nrow(edc_ae), "\n")

# Ensure expected columns exist, create if missing
expected_cols <- c("SUBJID","STDT","ENDT","TERM","DECOD","BODSYS","SEV","TXGR","REL","OUT","SER")
for(ec in expected_cols) {
  if(!ec %in% names(edc_ae)) {
    cat("WARNING: column", ec, "missing in edc_ae, creating NA\n")
    edc_ae[[ec]] <- NA_character_
  }
}

# Create USUBJID
edc_ae$USUBJID <- sas_catx("-", "ONCO-2026", edc_ae$SUBJID)

# Convert Raw Dates to ISO 8601
# SAS: if STDT ne "" then AESTDTC = put(input(STDT, date11.), yymmdd10.)
# similarly ENDT
# In R: parse STDT with date11, format yymmdd10
ae_prep <- edc_ae

# STDT -> AESTDTC
# handle STDT may be numeric Excel date? read as text so should be string
# but if original Excel had date type, read_excel_text with text will give string like "2020-01-01" or "01JAN2020"
# parse accordingly
stdt_parsed <- parse_date11(ae_prep$STDT)
# For those where STDT is already ISO, parse_date11 already handles via ymd fallback
# If STDT is "" then parsed is NA
ae_prep$AESTDTC <- format_yymmdd10(stdt_parsed)
ae_prep$AESTDTC[is.na(stdt_parsed)] <- NA_character_
# SAS: if STDT ne "" then ... else AESTDTC remains missing (length $19)
# So if STDT is "" or NA, AESTDTC is NA
ae_prep$AESTDTC[is.na(ae_prep$STDT) | trimws(ae_prep$STDT)==""] <- NA_character_

endt_parsed <- parse_date11(ae_prep$ENDT)
ae_prep$AEENDTC <- format_yymmdd10(endt_parsed)
ae_prep$AEENDTC[is.na(endt_parsed)] <- NA_character_
ae_prep$AEENDTC[is.na(ae_prep$ENDT) | trimws(ae_prep$ENDT)==""] <- NA_character_

# Keep only needed? Keep all for merge
# --------------------------------------------------------------
# 3. Sort AE Data
# --------------------------------------------------------------
# by USUBJID AESTDTC TERM
# SAS missing sorts first
ae_prep <- ae_prep[order(ae_prep$USUBJID, ae_prep$AESTDTC, ae_prep$TERM, na.last=FALSE), , drop=FALSE]

# --------------------------------------------------------------
# 4. Final Mapping and Derivation
# --------------------------------------------------------------
# Merge AE Data with DM (left join, keep all ae_prep)
# SAS: merge ae_prep(in=a) dm_for_ae; by USUBJID; if a;
sdtm_ae1 <- merge(ae_prep, dm_for_ae, by="USUBJID", all.x=TRUE, sort=FALSE)
# After merge, need to sort again? SAS merge requires sorted, but we already sorted
# Ensure order is by USUBJID AESTDTC TERM as before merge? SAS merge will interleave but keep ae_prep order within BY group
# We'll re-sort after derivation as step 5 does

# Identifier Variables
sdtm_ae1$STUDYID <- "ONCO-2026"
sdtm_ae1$DOMAIN <- "AE"

# AE Variables
sdtm_ae1$AETERM <- as.character(sdtm_ae1$TERM)
sdtm_ae1$AEDECOD <- toupper(as.character(sdtm_ae1$DECOD))
sdtm_ae1$AEBODSYS <- sas_tranwrd(as.character(sdtm_ae1$BODSYS), "_", " ")
sdtm_ae1$AESEV <- toupper(as.character(sdtm_ae1$SEV))
# AETXGR numeric 8
# TXGR may be character like "1", "2", etc. Convert to numeric
sdtm_ae1$AETXGR <- suppressWarnings(as.numeric(sdtm_ae1$TXGR))
sdtm_ae1$AEREL <- toupper(as.character(sdtm_ae1$REL))
sdtm_ae1$AEOUT <- toupper(as.character(sdtm_ae1$OUT))
sdtm_ae1$AESER <- as.character(sdtm_ae1$SER)
# Ensure AESER is single char, keep as is
sdtm_ae1$AESER <- trimws(sdtm_ae1$AESER)
sdtm_ae1$AESER[sdtm_ae1$AESER==""] <- NA_character_

# Derive AESTDY
# if AESTDTC ne "" and RFSTDTC ne "" then do; _stdt=input(AESTDTC,yymmdd10.); _rfdt=input(RFSTDTC,yymmdd10.); if _stdt >= _rfdt then AESTDY=(_stdt - _rfdt)+1 else AESTDY=(_stdt - _rfdt);
sdtm_ae1$AESTDY <- NA_real_
sdtm_ae1$AEENDY <- NA_real_

# vectorized
has_start <- !is.na(sdtm_ae1$AESTDTC) & sdtm_ae1$AESTDTC!="" & !is.na(sdtm_ae1$RFSTDTC) & sdtm_ae1$RFSTDTC!=""
if(any(has_start)) {
  stdt <- parse_yymmdd10(sdtm_ae1$AESTDTC[has_start])
  rfdt <- parse_yymmdd10(sdtm_ae1$RFSTDTC[has_start])
  # compute difference in days
  diff <- as.numeric(difftime(stdt, rfdt, units="days"))
  aestdy <- ifelse(stdt >= rfdt, diff + 1, diff)
  sdtm_ae1$AESTDY[has_start] <- aestdy
}

has_end <- !is.na(sdtm_ae1$AEENDTC) & sdtm_ae1$AEENDTC!="" & !is.na(sdtm_ae1$RFSTDTC) & sdtm_ae1$RFSTDTC!=""
if(any(has_end)) {
  endt <- parse_yymmdd10(sdtm_ae1$AEENDTC[has_end])
  rfdt2 <- parse_yymmdd10(sdtm_ae1$RFSTDTC[has_end])
  diff2 <- as.numeric(difftime(endt, rfdt2, units="days"))
  aeendy <- ifelse(endt >= rfdt2, diff2 + 1, diff2)
  sdtm_ae1$AEENDY[has_end] <- aeendy
}

# --------------------------------------------------------------
# 5. Sort Final AE Dataset
# --------------------------------------------------------------
sdtm_ae1 <- sdtm_ae1[order(sdtm_ae1$USUBJID, sdtm_ae1$AESTDTC, sdtm_ae1$TERM, na.last=FALSE), , drop=FALSE]

# --------------------------------------------------------------
# 6. Derive AESEQ
# --------------------------------------------------------------
# by USUBJID; retain AESEQ; if first.USUBJID then AESEQ=1 else AESEQ+1
# Need to handle SAS FIRST. logic: first within BY group
sdtm_ae1 <- sdtm_ae1[order(sdtm_ae1$USUBJID, sdtm_ae1$AESTDTC, sdtm_ae1$TERM, na.last=FALSE), , drop=FALSE]
# compute AESEQ
sdtm_ae1$AESEQ <- NA_integer_
# use dplyr group
sdtm_ae1 <- sdtm_ae1 %>%
  group_by(USUBJID) %>%
  mutate(AESEQ = row_number()) %>%
  ungroup() %>%
  as.data.frame()

# Keep Final SDTM Variables in order
keep_vars <- c("STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AEBODSYS","AESEV","AETXGR","AEREL","AEOUT","AESER","AESTDTC","AEENDTC","AESTDY","AEENDY")
# Ensure all exist
for(v in keep_vars) {
  if(!v %in% names(sdtm_ae1)) sdtm_ae1[[v]] <- NA
}
sdtm_ae <- sdtm_ae1[, keep_vars, drop=FALSE]

# Ensure types: STUDYID, DOMAIN, USUBJID, AETERM, AEDECOD, AEBODSYS, AESEV, AEREL, AEOUT, AESER, AESTDTC, AEENDTC as character
char_vars <- c("STUDYID","DOMAIN","USUBJID","AETERM","AEDECOD","AEBODSYS","AESEV","AEREL","AEOUT","AESER","AESTDTC","AEENDTC")
for(cv in char_vars) sdtm_ae[[cv]] <- as.character(sdtm_ae[[cv]])
# numeric vars
sdtm_ae$AESEQ <- as.integer(sdtm_ae$AESEQ)
sdtm_ae$AETXGR <- suppressWarnings(as.numeric(sdtm_ae$AETXGR))
sdtm_ae$AESTDY <- as.numeric(sdtm_ae$AESTDY)
sdtm_ae$AEENDY <- as.numeric(sdtm_ae$AEENDY)

# Final sort already done
sdtm_ae <- sdtm_ae[order(sdtm_ae$USUBJID, sdtm_ae$AESTDTC, sdtm_ae$AETERM, na.last=FALSE), , drop=FALSE]

cat("sdtm_ae rows:", nrow(sdtm_ae), "cols:", ncol(sdtm_ae), "\n")
print(head(sdtm_ae, 10))

# --------------------------------------------------------------
# 7. View Final AE Dataset (proc print) -> create listing text
# --------------------------------------------------------------
# Create text rendering similar to SAS proc print
# Count numbers: 56 numbers expected (likely numeric values in listing)
# We will produce a plain text file with header and table

# Prepare listing data for display: format NA as "" for display
display_df <- sdtm_ae
# For text file, we will show all rows
# Create text output
txt_path <- file.path(out, "P022.txt")
lst_path <- file.path(out, "P022.lst.txt")
csv_listing_path <- file.path(out, "P022.csv")
ae_csv_path <- file.path(out, "ae.csv")

# Write dataset ae.csv (SDTM AE)
# dates as YYYY-MM-DD already, datetimes as YYYY-MM-DDTHH:MM:SS (not needed here)
# numeric unrounded
write.csv(sdtm_ae, ae_csv_path, row.names=FALSE, na="")

cat("Wrote ae.csv to", ae_csv_path, "\n")

# Also write listing CSV (same as ae but for P022)
write.csv(sdtm_ae, csv_listing_path, row.names=FALSE, na="")
cat("Wrote P022.csv to", csv_listing_path, "\n")

# Create text rendering
# Build header
header_lines <- c(
  "Listing: SDTM AE Dataset (ONCO-2026)",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
  paste0("Rows: ", nrow(sdtm_ae)),
  "",
  "STUDYID DOMAIN USUBJID AESEQ AETERM AEDECOD AEBODSYS AESEV AETXGR AEREL AEOUT AESER AESTDTC AEENDTC AESTDY AEENDY",
  paste(rep("-", 120), collapse="")
)

# Build body lines: each row as fixed width or tab separated
# Use capture.output of print for simplicity but also custom
# We'll create a formatted table with columns separated by " | "
# Ensure numbers are shown as in SAS (integers without decimals)

# Format numeric for display: if NA then "", else as integer string
fmt_num <- function(x) ifelse(is.na(x), "", as.character(x))

body_lines <- apply(sdtm_ae, 1, function(row) {
  # row is character vector due to apply coercion, need to handle
  # Instead iterate by index
  paste(
    ifelse(is.na(row["STUDYID"]), "", row["STUDYID"]),
    ifelse(is.na(row["DOMAIN"]), "", row["DOMAIN"]),
    ifelse(is.na(row["USUBJID"]), "", row["USUBJID"]),
    ifelse(is.na(row["AESEQ"]), "", row["AESEQ"]),
    ifelse(is.na(row["AETERM"]), "", row["AETERM"]),
    ifelse(is.na(row["AEDECOD"]), "", row["AEDECOD"]),
    ifelse(is.na(row["AEBODSYS"]), "", row["AEBODSYS"]),
    ifelse(is.na(row["AESEV"]), "", row["AESEV"]),
    ifelse(is.na(row["AETXGR"]), "", row["AETXGR"]),
    ifelse(is.na(row["AEREL"]), "", row["AEREL"]),
    ifelse(is.na(row["AEOUT"]), "", row["AEOUT"]),
    ifelse(is.na(row["AESER"]), "", row["AESER"]),
    ifelse(is.na(row["AESTDTC"]), "", row["AESTDTC"]),
    ifelse(is.na(row["AEENDTC"]), "", row["AEENDTC"]),
    ifelse(is.na(row["AESTDY"]), "", row["AESTDY"]),
    ifelse(is.na(row["AEENDY"]), "", row["AEENDY"]),
    sep=" | "
  )
})

# Alternative: use data.frame print for better alignment
# Also add summary counts
# Count numbers in listing: count numeric values
num_count <- sum(!is.na(sdtm_ae$AESEQ)) + sum(!is.na(sdtm_ae$AETXGR)) + sum(!is.na(sdtm_ae$AESTDY)) + sum(!is.na(sdtm_ae$AEENDY))
cat("Numeric values count in listing:", num_count, "\n")

footer_lines <- c(
  "",
  paste0("Total observations: ", nrow(sdtm_ae)),
  paste0("Numeric values displayed: ", num_count),
  ""
)

all_lines <- c(header_lines, body_lines, footer_lines)

# Write to P022.txt
writeLines(all_lines, con=txt_path)
cat("Wrote P022.txt to", txt_path, "\n")

# Also write .lst.txt for compatibility
writeLines(all_lines, con=lst_path)
cat("Wrote P022.lst.txt to", lst_path, "\n")

# Also create a more SAS-like proc print capture
# Append detailed print output
sink_file <- file.path(out, "P022_print.txt")
sink(sink_file)
cat("PROC PRINT DATA=SDTM_AE\n")
print(sdtm_ae, row.names=FALSE)
sink()
cat("Wrote P022_print.txt\n")

# Ensure ae.csv also copied to outds style if needed
# Done

cat("All outputs written to", out, "\n")

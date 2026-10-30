out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_length <- function(x) {
  ifelse(is.na(x), 0L, nchar(gsub(" +$", "", as.character(x))))
}

parse_yymmdd10 <- function(x) {
  x <- as.character(x)
  res <- rep(as.Date(NA), length(x))
  idx <- !is.na(x) & sas_length(x) >= 10
  if (any(idx)) {
    s10 <- substr(x[idx], 1, 10)
    res[idx] <- as.Date(s10, format = "%Y-%m-%d")
  }
  res
}

sas_order_idx <- function(df, by) {
  old <- Sys.getlocale("LC_COLLATE")
  suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
  on.exit(suppressWarnings(try(Sys.setlocale("LC_COLLATE", old), silent = TRUE)), add = TRUE)
  args <- lapply(by, function(col) df[[col]])
  do.call(order, c(args, list(na.last = FALSE)))
}

locate_ds <- function(dirs, mem) {
  for (d in dirs) {
    if (is.na(d) || d == "" || !dir.exists(d)) next
    files <- list.files(d, full.names = TRUE)
    for (f in files) {
      bn <- tools::file_path_sans_ext(basename(f))
      if (tolower(bn) == tolower(mem)) return(f)
    }
    # also try subdirectories one level
    subd <- list.dirs(d, recursive = FALSE, full.names = TRUE)
    for (sd in subd) {
      files2 <- list.files(sd, full.names = TRUE)
      for (f in files2) {
        bn <- tools::file_path_sans_ext(basename(f))
        if (tolower(bn) == tolower(mem)) return(f)
      }
    }
  }
  return(NULL)
}

read_any <- function(path) {
  if (is.null(path)) return(NULL)
  lp <- tolower(path)
  if (grepl("\\.sas7bdat$", lp)) {
    df <- haven::read_sas(path)
  } else if (grepl("\\.xpt$", lp)) {
    df <- haven::read_xpt(path)
  } else if (grepl("\\.csv$", lp)) {
    df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    names(df) <- toupper(names(df))
    return(as.data.frame(df, stringsAsFactors = FALSE))
  } else if (grepl("\\.xlsx?$", lp)) {
    df <- readxl::read_excel(path)
  } else {
    stop(paste("Unknown extension:", path))
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  # convert haven_labelled to plain
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")) {
      # keep numeric if numeric, else character
      if (is.numeric(col)) {
        df[[nm]] <- as.numeric(col)
      } else {
        df[[nm]] <- as.character(col)
      }
    } else if (is.list(col)) {
      df[[nm]] <- as.character(unlist(col))
    }
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  }
  df
}

project_path <- "/bench/data/derived/c2c"
sdtm_dirs <- c(file.path(project_path, "data/sdtm"),
               "/bench/data/raw/cdisc-pilot/sdtm",
               "/bench/data/derived/c2c/data/raw")

dm_path <- locate_ds(sdtm_dirs, "dm")
ex_path <- locate_ds(sdtm_dirs, "ex")
cat("DM path:", ifelse(is.null(dm_path), "NULL", dm_path), "\n")
cat("EX path:", ifelse(is.null(ex_path), "NULL", ex_path), "\n")
stopifnot(!is.null(dm_path), !is.null(ex_path))

dm <- read_any(dm_path)
ex <- read_any(ex_path)
cat("DM rows:", nrow(dm), "cols:", paste(names(dm), collapse=","), "\n")
cat("EX rows:", nrow(ex), "cols:", paste(names(ex), collapse=","), "\n")

# Ensure needed columns exist; create as NA if absent
need_dm <- c("STUDYID","USUBJID","SUBJID","SITEID","ARMCD","ARM","BRTHDTC","RFICDTC","RFPENDTC","SEX","RACE")
for (nm in need_dm) if (!nm %in% names(dm)) dm[[nm]] <- NA_character_
need_ex <- c("USUBJID","EXTRT","EXSTDTC","EXENDTC")
for (nm in need_ex) if (!nm %in% names(ex)) ex[[nm]] <- NA_character_

# Normalize character missing: keep NA and "" distinct but both treated as missing
# Ensure types character for key/date vars
for (nm in c("STUDYID","USUBJID","SUBJID","SITEID","ARMCD","ARM","BRTHDTC","RFICDTC","RFPENDTC","SEX","RACE")) {
  dm[[nm]] <- as.character(dm[[nm]])
}
for (nm in c("USUBJID","EXTRT","EXSTDTC","EXENDTC")) {
  ex[[nm]] <- as.character(ex[[nm]])
}

# 1. GET FIRST EXPOSURE RECORD PER SUBJECT
# proc sort by USUBJID EXSTDTC
stopifnot("USUBJID" %in% names(ex), "EXSTDTC" %in% names(ex))
idx_ex <- sas_order_idx(ex, c("USUBJID","EXSTDTC"))
ex_first <- ex[idx_ex, , drop = FALSE]

# if first.USUBJID keep only initial dosing record
# SAS FIRST. : first row per USUBJID in sorted order
# Use duplicated
dup <- duplicated(ex_first$USUBJID)
# Note: duplicated treats NA as duplicated? In SAS missing sorts first, each missing USUBJID considered same group.
# Keep first occurrence
ex_trt_full <- ex_first[!dup, , drop = FALSE]
ex_trt <- ex_trt_full[, c("USUBJID","EXTRT","EXSTDTC","EXENDTC"), drop = FALSE]

# 2. SORT DM BEFORE MERGING
stopifnot("USUBJID" %in% names(dm))
idx_dm <- sas_order_idx(dm, c("USUBJID"))
dm_sorted <- dm[idx_dm, , drop = FALSE]

# 3. MERGE DEMOGRAPHICS AND EXPOSURE
# merge dm_sorted(in=a) ex_trt(in=b) by USUBJID; if a;
# Left join, drop/rename overlapping except keys (only USUBJID overlaps due to keep)
# Verify no .x/.y
common <- intersect(names(dm_sorted), names(ex_trt))
common_nonkey <- setdiff(common, "USUBJID")
if (length(common_nonkey) > 0) {
  # drop from ex_trt (SAS second overwrites, but keep list already minimal)
  ex_trt <- ex_trt[, setdiff(names(ex_trt), common_nonkey), drop = FALSE]
}
merged <- merge(dm_sorted, ex_trt, by = "USUBJID", all.x = TRUE, all.y = FALSE, sort = FALSE)
stopifnot("USUBJID" %in% names(merged))
stopifnot("ARMCD" %in% names(merged), "ARM" %in% names(merged))
stopifnot("EXTRT" %in% names(merged), "EXSTDTC" %in% names(merged), "EXENDTC" %in% names(merged))
# b flag: matched in ex_trt
b_keys <- unique(ex_trt$USUBJID)
# Need to handle NA keys: match NA?
merged$`__b` <- merged$USUBJID %in% b_keys
# For NA USUBJID, %in% returns FALSE if b_keys contains NA? Actually NA %in% NA is TRUE? In R, NA %in% NA is TRUE? Let's handle explicitly
is_na_m <- is.na(merged$USUBJID)
has_na_b <- any(is.na(b_keys))
if (has_na_b) merged$`__b`[is_na_m] <- TRUE

is_missing_char <- function(x) is.na(x) | x == ""
is_missing_num <- function(x) is.na(x)

n <- nrow(merged)
TRT01P <- rep(NA_character_, n)
TRT01A <- rep(NA_character_, n)
ITTFL <- rep(NA_character_, n)
SAFFL <- rep(NA_character_, n)

armcd_miss <- is_missing_char(merged$ARMCD)
is_scrn <- !armcd_miss & merged$ARMCD == "SCRNFAIL"

# 1. Planned Treatment
TRT01P[!is_scrn] <- merged$ARM[!is_scrn]
TRT01P[is_scrn] <- "UNPLANNED"

# 2. Actual Treatment
extrt_miss <- is_missing_char(merged$EXTRT)
has_b <- merged$`__b`
TRT01A[has_b & !extrt_miss] <- merged$EXTRT[has_b & !extrt_miss]
TRT01A[!(has_b & !extrt_miss)] <- "NOT TREATED"

# 3. ITT
ITTFL[!is_scrn] <- "Y"
ITTFL[is_scrn] <- "N"

# 4. SAFFL
exstdtc_miss <- is_missing_char(merged$EXSTDTC)
SAFFL[!exstdtc_miss] <- "Y"
SAFFL[exstdtc_miss] <- "N"

merged$TRT01P <- TRT01P
merged$TRT01A <- TRT01A
merged$ITTFL <- ITTFL
merged$SAFFL <- SAFFL

# Numeric date derivations
BRTHDT <- rep(as.Date(NA), n)
RFICDT <- rep(as.Date(NA), n)
TRTSDT <- rep(as.Date(NA), n)
TRTEDT <- rep(as.Date(NA), n)
EOSDT <- rep(as.Date(NA), n)

BRTHDT <- parse_yymmdd10(merged$BRTHDTC)
RFICDT <- parse_yymmdd10(merged$RFICDTC)
TRTSDT <- parse_yymmdd10(merged$EXSTDTC)
TRTEDT <- parse_yymmdd10(merged$EXENDTC)
EOSDT <- parse_yymmdd10(merged$RFPENDTC)

merged$BRTHDT <- BRTHDT
merged$RFICDT <- RFICDT
merged$TRTSDT <- TRTSDT
merged$TRTEDT <- TRTEDT
merged$EOSDT <- EOSDT

# Clinical math: AGE = floor(yrdif(BRTHDT, RFICDT, 'AGE'))
sas_age_years <- function(start, end) {
  res <- rep(NA_real_, length(start))
  ok <- !is.na(start) & !is.na(end)
  if (any(ok)) {
    s <- start[ok]; e <- end[ok]
    s_y <- as.integer(format(s, "%Y")); s_m <- as.integer(format(s, "%m")); s_d <- as.integer(format(s, "%d"))
    e_y <- as.integer(format(e, "%Y")); e_m <- as.integer(format(e, "%m")); e_d <- as.integer(format(e, "%d"))
    age <- e_y - s_y - as.integer((e_m < s_m) | (e_m == s_m & e_d < s_d))
    res[ok] <- age
  }
  res
}
AGE <- rep(NA_real_, n)
ok_age <- !is.na(merged$BRTHDT) & !is.na(merged$RFICDT)
AGE[ok_age] <- floor(sas_age_years(merged$BRTHDT[ok_age], merged$RFICDT[ok_age]))
merged$AGE <- AGE

TRTDURD <- rep(NA_real_, n)
ok_dur <- !is.na(merged$TRTSDT) & !is.na(merged$TRTEDT)
TRTDURD[ok_dur] <- as.numeric(merged$TRTEDT[ok_dur] - merged$TRTSDT[ok_dur]) + 1
merged$TRTDURD <- TRTDURD

# Keep
keep_vars <- c("STUDYID","USUBJID","SUBJID","SITEID","TRT01P","TRT01A","ITTFL","SAFFL",
               "BRTHDT","RFICDT","TRTSDT","TRTEDT","EOSDT","AGE","TRTDURD","SEX","RACE")
for (nm in keep_vars) if (!nm %in% names(merged)) merged[[nm]] <- NA
adsl_draft <- merged[, keep_vars, drop = FALSE]

# 4. FINALIZE AND SAVE: sort by USUBJID
idx_adsl <- sas_order_idx(adsl_draft, c("USUBJID"))
adsl <- adsl_draft[idx_adsl, , drop = FALSE]
rownames(adsl) <- NULL

# Write dataset adsl.csv with ISO dates, na=""
adsl_out <- adsl
for (dc in c("BRTHDT","RFICDT","TRTSDT","TRTEDT","EOSDT")) {
  adsl_out[[dc]] <- format(adsl_out[[dc]], format = "%Y-%m-%d")
  adsl_out[[dc]][is.na(adsl_out[[dc]])] <- ""
  # format turns NA Date into NA character; ensure ""
  adsl_out[[dc]][is.na(adsl_out[[dc]])] <- ""
}
# Ensure character NA remain NA for write.csv na=""
write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# 5. VISUAL AUDIT: proc print obs=10 vars USUBJID TRT01P TRT01A ITTFL SAFFL AGE
print_vars <- c("USUBJID","TRT01P","TRT01A","ITTFL","SAFFL","AGE")
stopifnot(all(print_vars %in% names(adsl)))
audit <- head(adsl, 10)
audit_body <- audit[, print_vars, drop = FALSE]
audit_body$OBS <- seq_len(nrow(audit_body))
audit_body <- audit_body[, c("OBS", print_vars), drop = FALSE]

# CSV of table body
audit_csv <- audit_body
# Format AGE as is; keep NA as ""
write.csv(audit_csv, file.path(out, "P006.csv"), row.names = FALSE, na = "")

# Text rendering mimicking PROC PRINT
fmt_cell <- function(val, is_num) {
  if (is.na(val) || (is.character(val) && val == "")) {
    if (is_num) return(".")
    else return("")
  } else return(as.character(val))
}
# Build fixed-width text
title_line <- "ADSL Audit - Subject Level Analysis Dataset"
# Header
hdr <- sprintf("%-5s %-20s %-20s %-20s %-6s %-6s %-5s", "Obs", "USUBJID", "TRT01P", "TRT01A", "ITTFL", "SAFFL", "AGE")
sep <- paste(rep("-", nchar(hdr)), collapse = "")
lines <- c(title_line, "", hdr, sep)
for (i in seq_len(nrow(audit_body))) {
  r <- audit_body[i, ]
  obs_s <- as.character(r$OBS)
  usub <- fmt_cell(r$USUBJID, FALSE)
  p <- fmt_cell(r$TRT01P, FALSE)
  a <- fmt_cell(r$TRT01A, FALSE)
  it <- fmt_cell(r$ITTFL, FALSE)
  sa <- fmt_cell(r$SAFFL, FALSE)
  ag <- fmt_cell(r$AGE, TRUE)
  lines <- c(lines, sprintf("%-5s %-20s %-20s %-20s %-6s %-6s %-5s", obs_s, usub, p, a, it, sa, ag))
}
lines <- c(lines, "")
txt_path <- file.path(out, "P006.txt")
writeLines(lines, txt_path)
# Also write .lst copy for compatibility
writeLines(lines, file.path(out, "P006.lst"))
cat("ADSL rows:", nrow(adsl), "\n")
cat("Audit rows:", nrow(audit_body), "\n")

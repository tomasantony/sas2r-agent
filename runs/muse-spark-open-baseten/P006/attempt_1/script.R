# P006_adam_adsl.sas -> R translation
# Program id: P006  family: c2c
# Outputs: adsl.csv + listing P006 (audit print)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
sdtm_path <- file.path(project_path, "data/sdtm")
adam_path <- file.path(project_path, "data/adam")
rawsdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
rawadam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

# helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-12)
  sign_x * z / mult
}

# SAS missing sorts first, uppercase before lowercase (ASCII)
sas_sort <- function(df, by) {
  # base R order is ASCII-like; ensure NA first
  # use order with na.last = FALSE
  ord <- do.call(order, c(lapply(by, function(col) df[[col]]), list(na.last = FALSE, method = "radix")))
  df[ord, , drop = FALSE]
}

read_sas_dataset <- function(dirpath, dsname) {
  dsname_low <- tolower(dsname)
  # search in dirpath recursively
  candidates <- character(0)
  if (dir.exists(dirpath)) {
    allfiles <- list.files(dirpath, recursive = TRUE, full.names = TRUE)
    # match dsname exactly
    candidates <- allfiles[grepl(paste0("(^|/)", dsname_low, "\\.(sas7bdat|xpt)$"), tolower(allfiles))]
  }
  # also try alternative raw paths
  alt_dirs <- c(sdtm_path, adam_path, rawsdtm_path, rawadam_path,
                "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split",
                "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw")
  for (ad in alt_dirs) {
    if (dir.exists(ad)) {
      af <- list.files(ad, recursive = TRUE, full.names = TRUE)
      cand2 <- af[grepl(paste0("(^|/)", dsname_low, "\\.(sas7bdat|xpt)$"), tolower(af))]
      candidates <- c(candidates, cand2)
    }
  }
  candidates <- unique(candidates)
  if (length(candidates) == 0) {
    # try direct file
    for (ext in c(".sas7bdat", ".xpt", ".sas7bdat ", ".XPT")) {
      p1 <- file.path(dirpath, paste0(dsname_low, ext))
      p2 <- file.path(dirpath, paste0(toupper(dsname), ext))
      if (file.exists(p1)) candidates <- c(candidates, p1)
      if (file.exists(p2)) candidates <- c(candidates, p2)
    }
  }
  if (length(candidates) == 0) {
    stop(paste0("Dataset not found: ", dsname, " in ", dirpath))
  }
  f <- candidates[1]
  cat("Reading", dsname, "from", f, "\n")
  if (grepl("\\.xpt$", tolower(f))) {
    df <- haven::read_xpt(f)
  } else {
    df <- haven::read_sas(f)
  }
  # convert haven_labelled to plain vectors, keep values
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # zap formats/labels but keep values
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")) {
      # if all values are character-like, convert to character via as.character
      # haven::zap_labels keeps numeric
      df[[nm]] <- haven::zap_labels(col)
      df[[nm]] <- haven::zap_formats(df[[nm]])
    }
    if (inherits(col, "POSIXt")) {
      df[[nm]] <- as.POSIXct(col)
    }
  }
  # force uppercase names (SAS validvarname=upcase)
  names(df) <- toupper(names(df))
  df
}

# SAS yrdif AGE basis -> floor gives completed years
calc_age_floor <- function(brthdt, rficdt) {
  # brthdt, rficdt are Date vectors
  # returns integer age or NA
  n <- length(brthdt)
  age <- rep(NA_integer_, n)
  ok <- !is.na(brthdt) & !is.na(rficdt)
  if (any(ok)) {
    b <- brthdt[ok]
    r <- rficdt[ok]
    yb <- as.integer(format(b, "%Y"))
    yr <- as.integer(format(r, "%Y"))
    mbd <- format(b, "%m%d")
    mrd <- format(r, "%m%d")
    # if rfic monthday < birth monthday subtract 1
    adj <- as.integer(mrd < mbd)
    age[ok] <- yr - yb - adj
  }
  age
}

parse_iso_date <- function(charvec) {
  # SAS: if length(BRTHDTC) >=10 then input(substr(...,1,10), yymmdd10.)
  # substr in SAS is 1-indexed; R substr same
  # yymmdd10. expects YYYY-MM-DD or YYYY/MM/DD etc. We handle YYYY-MM-DD
  n <- length(charvec)
  out <- rep(as.Date(NA), n)
  # ensure character
  cv <- as.character(charvec)
  # length check: nchar >=10 (SAS length includes trailing blanks? use nchar)
  # missing values: NA or "" -> NA
  idx <- !is.na(cv) & nchar(cv, keepNA = TRUE) >= 10
  if (any(idx)) {
    s10 <- substr(cv[idx], 1, 10)
    # try YYYY-MM-DD
    # use as.Date with format, fallback to ymd via lubridate if needed
    d <- suppressWarnings(as.Date(s10, format = "%Y-%m-%d"))
    # try alternative separators
    miss <- is.na(d)
    if (any(miss)) {
      d2 <- suppressWarnings(as.Date(s10[miss], format = "%Y/%m/%d"))
      d[miss] <- d2
    }
    # also try lubridate ymd for robustness
    miss2 <- is.na(d)
    if (any(miss2)) {
      d3 <- suppressWarnings(lubridate::ymd(s10[miss2], quiet = TRUE))
      d[miss2] <- as.Date(d3)
    }
    out[idx] <- d
  }
  out
}

# ----------------------------------------------------------------------
# 1. GET FIRST EXPOSURE RECORD PER SUBJECT
# ----------------------------------------------------------------------
cat("Step 1: Read SDTM EX and DM\n")
ex <- read_sas_dataset(sdtm_path, "ex")
dm <- read_sas_dataset(sdtm_path, "dm")

# Ensure required columns exist (create empty if missing)
ensure_col <- function(df, col, default = NA_character_) {
  if (!col %in% names(df)) df[[col]] <- default
  df
}
ex <- ensure_col(ex, "USUBJID")
ex <- ensure_col(ex, "EXSTDTC")
ex <- ensure_col(ex, "EXTRT")
ex <- ensure_col(ex, "EXENDTC")

dm <- ensure_col(dm, "USUBJID")
dm <- ensure_col(dm, "STUDYID")
dm <- ensure_col(dm, "SUBJID")
dm <- ensure_col(dm, "SITEID")
dm <- ensure_col(dm, "ARMCD")
dm <- ensure_col(dm, "ARM")
dm <- ensure_col(dm, "BRTHDTC")
dm <- ensure_col(dm, "RFICDTC")
dm <- ensure_col(dm, "RFPENDTC")
dm <- ensure_col(dm, "SEX")
dm <- ensure_col(dm, "RACE")

# proc sort data=sdtm.ex out=work.ex_first; by USUBJID EXSTDTC;
# SAS missing sorts first -> na.last = FALSE
ex_first <- ex[order(ex$USUBJID, ex$EXSTDTC, na.last = FALSE, method = "radix"), , drop = FALSE]

# data work.ex_trt; set work.ex_first; by USUBJID; if first.USUBJID;
# Keep only initial dosing record per USUBJID (first after sort)
# Use dplyr for clarity but keep SAS semantics
ex_first_sorted <- ex_first
# first.USUBJID logic: first row per USUBJID
ex_first_sorted$.__ord <- seq_len(nrow(ex_first_sorted))
# group by USUBJID, take first
ex_trt <- ex_first_sorted %>%
  dplyr::arrange(USUBJID, EXSTDTC) %>%
  dplyr::group_by(USUBJID) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(USUBJID, EXTRT, EXSTDTC, EXENDTC)

cat("EX first records:", nrow(ex_trt), "\n")

# 2. SORT DM BEFORE MERGING
dm_sorted <- dm[order(dm$USUBJID, na.last = FALSE, method = "radix"), , drop = FALSE]

# 3. MERGE DEMOGRAPHICS AND EXPOSURE TO CREATE ADSL
# merge work.dm_sorted(in=a) work.ex_trt(in=b); by USUBJID; if a;
adsl_draft <- dplyr::left_join(dm_sorted, ex_trt, by = "USUBJID")

# Core Attributes retain STUDYID USUBJID SUBJID SITEID (already present)

# Treatment variables & population flags
# TRT01P
adsl_draft$TRT01P <- ifelse(adsl_draft$ARMCD != "SCRNFAIL" & !is.na(adsl_draft$ARMCD),
                            adsl_draft$ARM,
                            "UNPLANNED")
# Handle NA ARMCD: SAS ne 'SCRNFAIL' would be true for missing? In SAS, missing char ne 'SCRNFAIL' is true.
# So if ARMCD is NA/missing, treat as not SCRNFAIL -> TRT01P = ARM
adsl_draft$TRT01P[is.na(adsl_draft$ARMCD) | adsl_draft$ARMCD == ""] <-
  adsl_draft$ARM[is.na(adsl_draft$ARMCD) | adsl_draft$ARMCD == ""]

# TRT01A
# if b and not missing(EXTRT) then TRT01A = EXTRT; else "NOT TREATED"
# b = matched in ex_trt
has_ex <- !is.na(adsl_draft$EXTRT) & trimws(adsl_draft$EXTRT) != ""
adsl_draft$TRT01A <- ifelse(has_ex, adsl_draft$EXTRT, "NOT TREATED")

# ITTFL
adsl_draft$ITTFL <- ifelse(!is.na(adsl_draft$ARMCD) & adsl_draft$ARMCD == "SCRNFAIL", "N", "Y")
# SAS: if ARMCD ne 'SCRNFAIL' then Y else N; missing -> Y (since missing ne SCRNFAIL)
adsl_draft$ITTFL[is.na(adsl_draft$ARMCD)] <- "Y"

# SAFFL
# if not missing(EXSTDTC) then Y else N
adsl_draft$SAFFL <- ifelse(!is.na(adsl_draft$EXSTDTC) & trimws(adsl_draft$EXSTDTC) != "", "Y", "N")

# Numeric date derivations
adsl_draft$BRTHDT <- parse_iso_date(adsl_draft$BRTHDTC)
adsl_draft$RFICDT <- parse_iso_date(adsl_draft$RFICDTC)
adsl_draft$TRTSDT <- parse_iso_date(adsl_draft$EXSTDTC)
adsl_draft$TRTEDT <- parse_iso_date(adsl_draft$EXENDTC)
adsl_draft$EOSDT  <- parse_iso_date(adsl_draft$RFPENDTC)

# Clinical math derivations
adsl_draft$AGE <- calc_age_floor(adsl_draft$BRTHDT, adsl_draft$RFICDT)

# TRTDURD = (TRTEDT - TRTSDT) +1 inclusive
adsl_draft$TRTDURD <- as.integer(adsl_draft$TRTEDT - adsl_draft$TRTSDT + 1L)
adsl_draft$TRTDURD[is.na(adsl_draft$TRTSDT) | is.na(adsl_draft$TRTEDT)] <- NA_integer_

# Keep required variables
keep_vars <- c("STUDYID","USUBJID","SUBJID","SITEID","TRT01P","TRT01A","ITTFL","SAFFL",
               "BRTHDT","RFICDT","TRTSDT","TRTEDT","EOSDT","AGE","TRTDURD","SEX","RACE")
# ensure all exist
for (v in keep_vars) if (!v %in% names(adsl_draft)) adsl_draft[[v]] <- NA
adsl_draft <- adsl_draft[, keep_vars, drop = FALSE]

# 4. FINALIZE AND SAVE TO ADAM LIBRARY - sort by USUBJID
adsl <- adsl_draft[order(adsl_draft$USUBJID, na.last = FALSE, method = "radix"), , drop = FALSE]

cat("ADSL records:", nrow(adsl), "\n")

# Write dataset adsl.csv with dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS
adsl_out <- adsl
date_cols <- c("BRTHDT","RFICDT","TRTSDT","TRTEDT","EOSDT")
for (dc in date_cols) {
  if (dc %in% names(adsl_out)) {
    adsl_out[[dc]] <- ifelse(is.na(adsl_out[[dc]]), "",
                             format(as.Date(adsl_out[[dc]]), "%Y-%m-%d"))
  }
}
# Ensure character columns have "" for NA (SAS missing)
for (nm in names(adsl_out)) {
  if (is.character(adsl_out[[nm]])) {
    adsl_out[[nm]][is.na(adsl_out[[nm]])] <- ""
  }
}
write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# 5. VISUAL AUDIT - PROC PRINT obs=10 var USUBJID TRT01P TRT01A ITTFL SAFFL AGE
audit_vars <- c("USUBJID","TRT01P","TRT01A","ITTFL","SAFFL","AGE")
audit_df <- adsl[, audit_vars, drop = FALSE]
audit_10 <- head(audit_df, 10)

# Create text rendering
# Count numbers: for verification, count numeric values displayed
# Build fixed-width style listing similar to SAS PROC PRINT
title_txt <- "ADSL Audit - Subject Level Analysis Dataset"
header_line <- paste0(paste(audit_vars, collapse = " | "))
# Prepare display: AGE as integer, missing as .
audit_disp <- audit_10
audit_disp$AGE <- ifelse(is.na(audit_disp$AGE), ".", as.character(audit_disp$AGE))
for (nm in audit_vars) {
  audit_disp[[nm]][is.na(audit_disp[[nm]])] <- ""
  audit_disp[[nm]] <- as.character(audit_disp[[nm]])
}

# Build text file
txt_path1 <- file.path(out, "P006.txt")
txt_path2 <- file.path(out, "P006.lst.txt")
txt_path3 <- file.path(out, "P006.lst")
csv_path1 <- file.path(out, "P006.csv")
csv_path2 <- file.path(out, "P006.lst.csv")

# Create plain-text rendering
con <- file(txt_path1, open = "wt")
cat(title_txt, "\n", file = con)
cat(paste(rep("-", nchar(title_txt)), collapse = ""), "\n", file = con)
cat("\n", file = con)
# Column headers
cat(sprintf("%-4s %-18s %-20s %-20s %-6s %-6s %-4s\n",
            "Obs", "USUBJID", "TRT01P", "TRT01A", "ITTFL", "SAFFL", "AGE"), file = con)
cat(paste(rep("-", 90), collapse = ""), "\n", file = con)
for (i in seq_len(nrow(audit_disp))) {
  cat(sprintf("%-4d %-18s %-20s %-20s %-6s %-6s %-4s\n",
              i,
              substr(audit_disp$USUBJID[i], 1, 18),
              substr(audit_disp$TRT01P[i], 1, 20),
              substr(audit_disp$TRT01A[i], 1, 20),
              audit_disp$ITTFL[i],
              audit_disp$SAFFL[i],
              audit_disp$AGE[i]), file = con)
}
cat("\n", file = con)
cat(paste0("Total subjects in ADSL: ", nrow(adsl), "\n"), file = con)
cat(paste0("Displayed: ", nrow(audit_10), " observations\n"), file = con)
close(con)

# copy to alternative names
file.copy(txt_path1, txt_path2, overwrite = TRUE)
file.copy(txt_path1, txt_path3, overwrite = TRUE)

# CSV of table body (same as SAS listing body)
# Include Obs column to match SAS PROC PRINT default
audit_csv <- audit_10
audit_csv <- cbind(Obs = seq_len(nrow(audit_csv)), audit_csv)
# Ensure NA handling for CSV: keep NA as "" already via write.csv na=""
write.csv(audit_csv, csv_path1, row.names = FALSE, na = "")
file.copy(csv_path1, csv_path2, overwrite = TRUE)

cat("Outputs written to", out, "\n")
cat(" - adsl.csv (", nrow(adsl_out), "rows)\n")
cat(" - P006.txt / P006.lst.txt (audit listing)\n")
cat(" - P006.csv / P006.lst.csv (audit csv)\n")

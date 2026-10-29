out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: SAS round half away from zero (not needed for this program but defined)
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: find file with various extensions
find_file <- function(dir, name) {
  # name without extension, lower case
  if (!dir.exists(dir)) return(NA_character_)
  files <- list.files(dir, recursive = FALSE, full.names = TRUE)
  # case-insensitive match
  pat <- paste0("^", name, "\\.(sas7bdat|xpt|sas7bdat\\.gz|xpt\\.gz|csv)$")
  idx <- grep(pat, basename(files), ignore.case = TRUE)
  if (length(idx) > 0) return(files[idx[1]])
  # also try without extension search
  idx2 <- grep(paste0("^", name, "$"), tools::file_path_sans_ext(basename(files)), ignore.case = TRUE)
  if (length(idx2) > 0) return(files[idx2[1]])
  return(NA_character_)
}

read_sas_dataset <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  # handle .gz
  if (ext == "gz") ext <- tolower(tools::file_ext(tools::file_path_sans_ext(path)))
  if (ext == "xpt") {
    df <- haven::read_xpt(path)
  } else if (ext == "sas7bdat") {
    df <- haven::read_sas(path)
  } else if (ext == "csv") {
    df <- readr::read_csv(path, show_col_types = FALSE)
  } else {
    # try both
    tryCatch({ df <- haven::read_sas(path) }, error = function(e) df <<- haven::read_xpt(path))
  }
  # uppercase names to mimic SAS validvarname=upcase
  names(df) <- toupper(names(df))
  # convert haven labelled to plain, keep Date/POSIXct if already converted
  df <- haven::zap_formats(df)
  df <- haven::zap_label(df)
  df <- haven::zap_widths(df)
  # haven may return tibble; convert to data.frame for consistency
  as.data.frame(df, stringsAsFactors = FALSE)
}

# Helper to convert SAS numeric date (days since 1960-01-01) if needed
# haven::read_sas already converts formatted dates to Date, so we detect numeric vs Date
to_Date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS date: days since 1960-01-01
    # If values look like SAS dates (e.g., 10000-25000) convert
    # But if already R Date numeric? haven already converted, so this branch only for unformatted numeric
    # We check if any non-NA values are in plausible SAS date range and not already Date
    # To avoid mis-converting AVAL etc, only call for known date vars
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    # try yyyymmdd
    suppressWarnings(as.Date(x))
  } else {
    x
  }
}

# Locate inputs
adam_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm"
# fallback to project_path
if (!dir.exists(adam_dir)) adam_dir <- file.path(Sys.getenv("PROJECT_PATH", unset = "D:/meta_hackathon/sas2r-agent/data/derived/c2c"), "data/adam")
if (!dir.exists(sdtm_dir)) sdtm_dir <- file.path(Sys.getenv("PROJECT_PATH", unset = "D:/meta_hackathon/sas2r-agent/data/derived/c2c"), "data/sdtm")

adsl_path <- find_file(adam_dir, "adsl")
vs_path   <- find_file(sdtm_dir, "vs")

# If not found, try alternative raw locations
if (is.na(adsl_path)) {
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
  adsl_path <- find_file(alt1, "adsl")
}
if (is.na(vs_path)) {
  alt2 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
  vs_path <- find_file(alt2, "vs")
}

if (is.na(adsl_path) || is.null(adsl_path)) stop("ADSL not found in ", adam_dir)
if (is.na(vs_path)   || is.null(vs_path))   stop("VS not found in ", sdtm_dir)

adsl <- read_sas_dataset(adsl_path)
vs   <- read_sas_dataset(vs_path)

# Ensure required columns exist (uppercase)
# ADSL expected: STUDYID, USUBJID, TRT01A, TRTSDT, TRTEDT
# VS expected: USUBJID, VSDTC, VSTESTCD, VSTEST, VSSTRESN, VISIT, VISITNUM, VSSEQ, STUDYID maybe

# Convert date vars to Date if they are SAS numeric without format
# Identify date vars by name
date_vars_adsl <- intersect(c("TRTSDT","TRTEDT","TRT01SDT","TRT01EDT"), names(adsl))
for (v in date_vars_adsl) {
  if (is.numeric(adsl[[v]]) && !inherits(adsl[[v]], "Date")) {
    # Check if values look like SAS dates (e.g., > 1000 and < 30000)
    # Convert all numeric date vars
    adsl[[v]] <- as.Date(adsl[[v]], origin = "1960-01-01")
  } else if (inherits(adsl[[v]], "POSIXct")) {
    adsl[[v]] <- as.Date(adsl[[v]])
  }
}
# VS dates are character VSDTC, not numeric

# 1. MERGE ADSL AND VS
# proc sort data=adam.adsl out=work.adsl_sorted; by USUBJID;
# proc sort data=sdtm.vs out=work.vs_sorted; by USUBJID VSDTC VSTESTCD;

# SAS sort: character ordering, missing first. R order with na.last=FALSE approximates.
# Sort ADSL
if ("USUBJID" %in% names(adsl)) {
  adsl_sorted <- adsl[order(adsl$USUBJID, na.last = FALSE), , drop = FALSE]
} else {
  adsl_sorted <- adsl
}

# Sort VS
vs_sorted <- vs
if (all(c("USUBJID","VSDTC","VSTESTCD") %in% names(vs_sorted))) {
  vs_sorted <- vs_sorted[order(vs_sorted$USUBJID, vs_sorted$VSDTC, vs_sorted$VSTESTCD, na.last = FALSE), , drop = FALSE]
} else if ("USUBJID" %in% names(vs_sorted)) {
  vs_sorted <- vs_sorted[order(vs_sorted$USUBJID, na.last = FALSE), , drop = FALSE]
}

# data work.advs_draft; merge work.adsl_sorted(in=a) work.vs_sorted(in=b); by USUBJID; if b;
# Keep only records that have vital signs => left join vs with adsl
# Use dplyr for clarity
library(dplyr)

# Ensure USUBJID is character
adsl_sorted$USUBJID <- as.character(adsl_sorted$USUBJID)
vs_sorted$USUBJID   <- as.character(vs_sorted$USUBJID)

# For merge, keep adsl columns needed: STUDYID, USUBJID, TRT01A, TRTSDT, TRTEDT
# But keep all adsl columns for potential STUDYID
adsl_keep <- adsl_sorted
# vs columns needed
# Perform merge: vs left join adsl
advs_draft <- vs_sorted %>%
  left_join(adsl_sorted, by = "USUBJID", suffix = c("", ".ADSL"))

# Resolve STUDYID: if STUDYID.ADSL exists, prefer it, else STUDYID
if ("STUDYID.ADSL" %in% names(advs_draft)) {
  # coalesce
  advs_draft$STUDYID <- ifelse(!is.na(advs_draft$STUDYID.ADSL) & advs_draft$STUDYID.ADSL != "",
                               advs_draft$STUDYID.ADSL, advs_draft$STUDYID)
  advs_draft$STUDYID.ADSL <- NULL
}
# If STUDYID still missing, try to fill from adsl
# Ensure TRT01A, TRTSDT, TRTEDT from ADSL (they may have suffix)
if ("TRT01A.ADSL" %in% names(advs_draft)) {
  advs_draft$TRT01A <- advs_draft$TRT01A.ADSL
  advs_draft$TRT01A.ADSL <- NULL
}
if ("TRTSDT.ADSL" %in% names(advs_draft)) {
  advs_draft$TRTSDT <- advs_draft$TRTSDT.ADSL
  advs_draft$TRTSDT.ADSL <- NULL
}
if ("TRTEDT.ADSL" %in% names(advs_draft)) {
  advs_draft$TRTEDT <- advs_draft$TRTEDT.ADSL
  advs_draft$TRTEDT.ADSL <- NULL
}
# If TRTSDT/TRTEDT were character, convert
for (v in c("TRTSDT","TRTEDT")) {
  if (v %in% names(advs_draft)) {
    if (is.numeric(advs_draft[[v]]) && !inherits(advs_draft[[v]], "Date")) {
      advs_draft[[v]] <- as.Date(advs_draft[[v]], origin = "1960-01-01")
    } else if (inherits(advs_draft[[v]], "POSIXct")) {
      advs_draft[[v]] <- as.Date(advs_draft[[v]])
    } else if (is.character(advs_draft[[v]])) {
      # try parse
      tmp <- suppressWarnings(as.Date(advs_draft[[v]], format = "%Y-%m-%d"))
      if (sum(!is.na(tmp)) > 0) advs_draft[[v]] <- tmp
    }
  }
}

# Derive CORE ANALYSIS VARIABLES
# PARAMCD = VSTESTCD; PARAM = VSTEST; AVAL = VSSTRESN; AVISIT = VISIT; AVISITN = VISITNUM; SRCDOM='VS'; SRCVAR='VSSEQ'; SRCSEQ=VSSEQ
# Ensure columns exist
if (!"VSTESTCD" %in% names(advs_draft)) advs_draft$VSTESTCD <- NA_character_
if (!"VSTEST" %in% names(advs_draft)) advs_draft$VSTEST <- NA_character_
if (!"VSSTRESN" %in% names(advs_draft)) advs_draft$VSSTRESN <- NA_real_
if (!"VISIT" %in% names(advs_draft)) advs_draft$VISIT <- NA_character_
if (!"VISITNUM" %in% names(advs_draft)) advs_draft$VISITNUM <- NA_real_
if (!"VSSEQ" %in% names(advs_draft)) advs_draft$VSSEQ <- NA_real_
if (!"VSDTC" %in% names(advs_draft)) advs_draft$VSDTC <- NA_character_

advs_draft$PARAMCD <- advs_draft$VSTESTCD
advs_draft$PARAM   <- advs_draft$VSTEST
advs_draft$AVAL    <- suppressWarnings(as.numeric(advs_draft$VSSTRESN))
advs_draft$AVISIT  <- advs_draft$VISIT
advs_draft$AVISITN <- suppressWarnings(as.numeric(advs_draft$VISITNUM))
advs_draft$SRCDOM  <- "VS"
advs_draft$SRCVAR  <- "VSSEQ"
advs_draft$SRCSEQ  <- suppressWarnings(as.numeric(advs_draft$VSSEQ))

# Convert ISO Date to Numeric Analysis Date: if length(VSDTC) >=10 then ASTDT = input(substr(VSDTC,1,10), yymmdd10.)
advs_draft$ASTDT <- as.Date(NA)
has_len <- !is.na(advs_draft$VSDTC) & nchar(advs_draft$VSDTC) >= 10
if (any(has_len)) {
  substr10 <- substr(advs_draft$VSDTC[has_len], 1, 10)
  # SAS yymmdd10. handles YYYY-MM-DD, also YYYY/MM/DD, etc. Use as.Date with multiple formats
  parsed <- suppressWarnings(as.Date(substr10, format = "%Y-%m-%d"))
  # try alternative format YYYY/MM/DD
  miss <- is.na(parsed)
  if (any(miss)) {
    parsed[miss] <- suppressWarnings(as.Date(substr10[miss], format = "%Y/%m/%d"))
  }
  # try YYYY-MM-DD with lubridate fallback
  miss2 <- is.na(parsed)
  if (any(miss2)) {
    parsed[miss2] <- suppressWarnings(lubridate::ymd(substr10[miss2], quiet = TRUE))
  }
  advs_draft$ASTDT[has_len] <- parsed
}

# Analysis Relative Day (ASTDY)
advs_draft$ASTDY <- NA_real_
# if not missing(ASTDT) and not missing(TRTSDT) then ...
idx <- !is.na(advs_draft$ASTDT) & !is.na(advs_draft$TRTSDT)
if (any(idx)) {
  ge <- advs_draft$ASTDT[idx] >= advs_draft$TRTSDT[idx]
  advs_draft$ASTDY[idx][ge]  <- as.numeric(advs_draft$ASTDT[idx][ge] - advs_draft$TRTSDT[idx][ge]) + 1
  advs_draft$ASTDY[idx][!ge] <- as.numeric(advs_draft$ASTDT[idx][!ge] - advs_draft$TRTSDT[idx][!ge])
}

# keep STUDYID USUBJID TRT01A TRTSDT TRTEDT PARAMCD PARAM AVAL AVISIT AVISITN ASTDT ASTDY SRCDOM SRCVAR SRCSEQ
keep_vars <- c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT","PARAMCD","PARAM","AVAL","AVISIT","AVISITN","ASTDT","ASTDY","SRCDOM","SRCVAR","SRCSEQ")
# Ensure all exist
for (v in keep_vars) if (!v %in% names(advs_draft)) advs_draft[[v]] <- NA
advs_draft <- advs_draft[, keep_vars, drop = FALSE]

# Retain STUDYID USUBJID ordering already

# 2. IDENTIFY BASELINE RECORD
# proc sort data=work.advs_draft out=work.advs_base_candidates; by USUBJID PARAMCD ASTDT AVISITN SRCSEQ; where ASTDT <= TRTSDT and not missing(AVAL);
# SAS where: ASTDT <= TRTSDT includes missing ASTDT as true (missing < any). Replicate.
advs_draft$TRTSDT_date <- advs_draft$TRTSDT # already Date
# Compute condition ASTDT <= TRTSDT with SAS missing semantics
ast_le_trtsdt <- is.na(advs_draft$ASTDT) | (!is.na(advs_draft$TRTSDT) & !is.na(advs_draft$ASTDT) & advs_draft$ASTDT <= advs_draft$TRTSDT)
# Actually is.na(ASTDT) already true, so second part only when both non-missing
# But need to handle case where ASTDT is NA => true regardless of TRTSDT, which matches SAS
# For non-missing ASTDT and missing TRTSDT => false (since !is.na(TRTSDT) false)
# So:
ast_le_trtsdt <- is.na(advs_draft$ASTDT) | (!is.na(advs_draft$ASTDT) & !is.na(advs_draft$TRTSDT) & advs_draft$ASTDT <= advs_draft$TRTSDT)

base_candidates <- advs_draft[ast_le_trtsdt & !is.na(advs_draft$AVAL), , drop = FALSE]

# Sort by USUBJID PARAMCD ASTDT AVISITN SRCSEQ with missing first
if (nrow(base_candidates) > 0) {
  base_candidates <- base_candidates[order(base_candidates$USUBJID, base_candidates$PARAMCD, base_candidates$ASTDT, base_candidates$AVISITN, base_candidates$SRCSEQ, na.last = FALSE), , drop = FALSE]
}

# data work.base_flags; set work.advs_base_candidates; by USUBJID PARAMCD; if last.PARAMCD then output;
# Keep last per USUBJID PARAMCD
if (nrow(base_candidates) > 0) {
  # Use dplyr to get last per group (after sorting, last is max ASTDT etc)
  base_flags <- base_candidates %>%
    group_by(USUBJID, PARAMCD) %>%
    arrange(ASTDT, AVISITN, SRCSEQ, .by_group = FALSE) %>% # already sorted, but ensure within group order
    slice_tail(n = 1) %>%
    ungroup() %>%
    transmute(USUBJID = USUBJID,
              PARAMCD = PARAMCD,
              BASE = AVAL,
              BASE_DT = ASTDT,
              BASE_SEQ = SRCSEQ)
} else {
  base_flags <- data.frame(USUBJID=character(0), PARAMCD=character(0), BASE=numeric(0), BASE_DT=as.Date(character(0)), BASE_SEQ=numeric(0))
}

# 3. MERGE BASELINE BACK AND CALCULATE FLAGS/CHANGE
# proc sort data=work.advs_draft; by USUBJID PARAMCD;
advs_draft_sorted <- advs_draft[order(advs_draft$USUBJID, advs_draft$PARAMCD, na.last = FALSE), , drop = FALSE]

# merge work.advs_draft(in=a) work.base_flags(in=b); by USUBJID PARAMCD; if a;
advs <- advs_draft_sorted %>%
  left_join(base_flags, by = c("USUBJID","PARAMCD"))

# Flag the exact source record; the QC gate rejects same-day ambiguity.
# if SRCSEQ = BASE_SEQ then ABLFL='Y' else ''
# Need to handle NA: if either NA, then not equal => ''
advs$ABLFL <- ifelse(!is.na(advs$SRCSEQ) & !is.na(advs$BASE_SEQ) & advs$SRCSEQ == advs$BASE_SEQ, "Y", "")

# Calculate Change from Baseline
advs$CHG <- NA_real_
idx_chg <- !is.na(advs$AVAL) & !is.na(advs$BASE)
advs$CHG[idx_chg] <- advs$AVAL[idx_chg] - advs$BASE[idx_chg]

# drop BASE_DT BASE_SEQ
advs$BASE_DT <- NULL
advs$BASE_SEQ <- NULL

# Re-sort for final presentation: by USUBJID PARAMCD ASTDT
advs <- advs[order(advs$USUBJID, advs$PARAMCD, advs$ASTDT, na.last = FALSE), , drop = FALSE]

# Ensure column order: retain STUDYID USUBJID then others as in keep plus BASE CHG ABLFL
# Original keep plus BASE, CHG, ABLFL, and retain STUDYID USUBJID first
final_vars <- c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT","PARAMCD","PARAM","AVAL","AVISIT","AVISITN","ASTDT","ASTDY","SRCDOM","SRCVAR","SRCSEQ","BASE","CHG","ABLFL")
# Add any missing
for (v in final_vars) if (!v %in% names(advs)) advs[[v]] <- NA
advs <- advs[, final_vars, drop = FALSE]

# Write dataset advs.csv with dates as YYYY-MM-DD
advs_out <- advs
# Format Date columns to character YYYY-MM-DD for CSV
date_cols <- c("TRTSDT","TRTEDT","ASTDT")
for (dc in date_cols) {
  if (dc %in% names(advs_out)) {
    advs_out[[dc]] <- ifelse(is.na(advs_out[[dc]]), "", format(advs_out[[dc]], "%Y-%m-%d"))
  }
}
# Ensure numeric unrounded (write.csv will handle)
write.csv(advs_out, file.path(out, "advs.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT - proc print obs=15 var USUBJID PARAMCD AVISIT ASTDT TRTSDT AVAL BASE CHG ABLFL
audit_vars <- c("USUBJID","PARAMCD","AVISIT","ASTDT","TRTSDT","AVAL","BASE","CHG","ABLFL")
audit_df <- advs[1:min(15, nrow(advs)), audit_vars, drop = FALSE]

# Prepare display versions with formatted dates
audit_disp <- audit_df
for (dc in c("ASTDT","TRTSDT")) {
  if (dc %in% names(audit_disp)) {
    # advs$ASTDT is Date, format as date9. like 01JAN2020? But SAS format date9. is DDMMMYYYY
    # For text rendering, show YYYY-MM-DD for clarity, but also include SAS style
    # Use YYYY-MM-DD to match CSV
    audit_disp[[dc]] <- ifelse(is.na(audit_disp[[dc]]), "", format(audit_disp[[dc]], "%Y-%m-%d"))
  }
}

# Write CSV of listing
write.csv(audit_disp, file.path(out, "P008.csv"), row.names = FALSE, na = "")
write.csv(audit_disp, file.path(out, "P008.lst.csv"), row.names = FALSE, na = "")

# Write text rendering
txt_path1 <- file.path(out, "P008.txt")
txt_path2 <- file.path(out, "P008.lst.txt")
# Also P008.lst without extension? Create P008.lst
txt_path3 <- file.path(out, "P008.lst")

# Build text output similar to PROC PRINT
header <- "ADVS Audit - Vital Signs Baseline and Change Analysis"
# Count numbers: count numeric values displayed
# Format table with fixed width
# Use capture
con_text <- c(
  header,
  paste0("Obs printed: ", nrow(audit_disp), " of ", nrow(advs)),
  "",
  paste(sprintf("%-4s %-12s %-8s %-12s %-12s %-12s %-8s %-8s %-8s %-5s",
                "Obs","USUBJID","PARAMCD","AVISIT","ASTDT","TRTSDT","AVAL","BASE","CHG","ABLFL"),
        collapse = ""),
  paste(rep("-", 95), collapse = "")
)
# Add rows
for (i in seq_len(nrow(audit_disp))) {
  row <- audit_disp[i, ]
  # Format numeric with default (unrounded) - use as.character
  aval_str <- ifelse(is.na(audit_df$AVAL[i]), "", as.character(audit_df$AVAL[i]))
  base_str <- ifelse(is.na(audit_df$BASE[i]), "", as.character(audit_df$BASE[i]))
  chg_str  <- ifelse(is.na(audit_df$CHG[i]), "", as.character(audit_df$CHG[i]))
  line <- sprintf("%-4d %-12s %-8s %-12s %-12s %-12s %-8s %-8s %-8s %-5s",
                  i,
                  substr(ifelse(is.na(row$USUBJID), "", row$USUBJID),1,12),
                  substr(ifelse(is.na(row$PARAMCD), "", row$PARAMCD),1,8),
                  substr(ifelse(is.na(row$AVISIT), "", row$AVISIT),1,12),
                  ifelse(row$ASTDT=="", "", row$ASTDT),
                  ifelse(row$TRTSDT=="", "", row$TRTSDT),
                  aval_str, base_str, chg_str,
                  ifelse(is.na(row$ABLFL), "", row$ABLFL))
  con_text <- c(con_text, line)
}
con_text <- c(con_text, "", paste0("Total observations in ADVS: ", nrow(advs)))

writeLines(con_text, txt_path1)
writeLines(con_text, txt_path2)
writeLines(con_text, txt_path3)

# Also ensure advs.txt not required but create for completeness
# Write full advs listing truncated? Not needed

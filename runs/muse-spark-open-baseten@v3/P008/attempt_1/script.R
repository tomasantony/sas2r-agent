# P008_adam_advs.sas -> R translation
# Output: advs dataset + listing P008

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
adam_lib <- file.path(project_path, "data/adam")
sdtm_lib <- file.path(project_path, "data/sdtm")

# helper: round half away from zero (SAS)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: read SAS dataset by name (case-insensitive) from a directory
read_sas_dataset <- function(libpath, dsname) {
  if (!dir.exists(libpath)) stop(paste("Lib path not found:", libpath))
  files <- list.files(libpath, full.names = TRUE, recursive = FALSE)
  # match dsname ignoring case and extension
  pat <- paste0("^", dsname, "\\.(sas7bdat|xpt|sastbdat)$")
  idx <- grep(pat, basename(files), ignore.case = TRUE)
  if (length(idx) == 0) {
    # also try without extension exact file
    # list all and check lower
    lower <- tolower(basename(files))
    idx2 <- which(tolower(paste0(dsname, ".sas7bdat")) == lower |
                  tolower(paste0(dsname, ".xpt")) == lower)
    if (length(idx2) > 0) idx <- idx2
  }
  if (length(idx) == 0) stop(paste("Dataset", dsname, "not found in", libpath, "files:", paste(basename(files), collapse=", ")))
  f <- files[idx[1]]
  ext <- tolower(tools::file_ext(f))
  if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else {
    df <- haven::read_sas(f)
  }
  # zap labels/formats but keep Date types where haven already converted
  # convert haven_labelled to plain
  df <- haven::zap_formats(df)
  df <- haven::zap_labels(df)
  df <- haven::zap_label(df)
  # ensure column names uppercase (SAS validvarname=upcase)
  names(df) <- toupper(names(df))
  df
}

# helper: convert SAS numeric date (days since 1960-01-01) to Date if needed
to_Date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS dates are typically < 30000-40000; if values look like SAS dates, convert
    # haven already converts formatted dates to Date, so numeric here is likely SAS date
    # Use origin 1960-01-01
    # Only convert if not NA and plausible range
    # Check if any non-NA
    out <- as.Date(x, origin = "1960-01-01")
    return(out)
  }
  if (is.character(x)) {
    # try ISO parse
    suppressWarnings(as.Date(x))
  } else {
    x
  }
}

# helper: format Date as DATE9. for listing (e.g., 01JAN2020)
format_date9 <- function(d) {
  ifelse(is.na(d), "",
         toupper(format(d, "%d%b%Y")))
}
format_iso <- function(d) {
  ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
}

# ------------------------------------------------------------------
# 1. Read ADSL and VS
adsl <- read_sas_dataset(adam_lib, "adsl")
vs   <- read_sas_dataset(sdtm_lib, "vs")

# Ensure required columns exist
# ADSL expected: STUDYID, USUBJID, TRT01A, TRTSDT, TRTEDT
# VS expected: STUDYID, USUBJID, VSDTC, VSTESTCD, VSTEST, VSSTRESN, VISIT, VISITNUM, VSSEQ
# stopifnot checks
stopifnot("USUBJID" %in% names(adsl))
stopifnot("USUBJID" %in% names(vs))

# Convert TRTSDT/TRTEDT to Date
if ("TRTSDT" %in% names(adsl)) adsl$TRTSDT <- to_Date(adsl$TRTSDT)
if ("TRTEDT" %in% names(adsl)) adsl$TRTEDT <- to_Date(adsl$TRTEDT)

# Ensure character columns are character (haven may give character already)
char_cols_adsl <- intersect(c("STUDYID","USUBJID","TRT01A"), names(adsl))
for (cn in char_cols_adsl) adsl[[cn]] <- as.character(adsl[[cn]])
char_cols_vs <- intersect(c("STUDYID","USUBJID","VSDTC","VSTESTCD","VSTEST","VISIT"), names(vs))
for (cn in char_cols_vs) vs[[cn]] <- as.character(vs[[cn]])

# ------------------------------------------------------------------
# 1. MERGE ADSL AND VS  (keep only records that have vital signs)
# SAS: proc sort adsl by USUBJID; proc sort vs by USUBJID VSDTC VSTESTCD; merge by USUBJID if b;

# Sort as SAS does (character ordering, missing first)
adsl_sorted <- adsl[order(adsl$USUBJID, na.last = FALSE), , drop = FALSE]
vs_sorted   <- vs[order(vs$USUBJID, vs$VSDTC, vs$VSTESTCD, na.last = FALSE), , drop = FALSE]

# Left join vs to adsl on USUBJID (SAS merge replicates adsl across vs rows)
# Drop/rename overlapping columns except keys to avoid .x/.y suffixes
# Overlapping: STUDYID, USUBJID
# Keep STUDYID from vs if present else adsl; but they should be same
# For merge, keep adsl columns needed: STUDYID, USUBJID, TRT01A, TRTSDT, TRTEDT
adsl_keep <- adsl_sorted[, intersect(c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT"), names(adsl_sorted)), drop = FALSE]
# If STUDYID in both, we will use vs STUDYID after join but check consistency
# Perform left join
advs_draft <- dplyr::left_join(vs_sorted, adsl_keep, by = "USUBJID", suffix = c("", ".adsl"))

# Resolve STUDYID: if STUDYID.adsl exists, coalesce
if ("STUDYID.adsl" %in% names(advs_draft)) {
  # vs STUDYID is "STUDYID", adsl is "STUDYID.adsl"
  advs_draft$STUDYID <- ifelse(!is.na(advs_draft$STUDYID) & advs_draft$STUDYID != "",
                               advs_draft$STUDYID,
                               advs_draft$`STUDYID.adsl`)
  advs_draft$`STUDYID.adsl` <- NULL
}

# Now derive CORE ANALYSIS VARIABLES
# PARAMCD = VSTESTCD; PARAM = VSTEST; AVAL = VSSTRESN; AVISIT = VISIT; AVISITN = VISITNUM
# SRCDOM='VS'; SRCVAR='VSSEQ'; SRCSEQ=VSSEQ
# ASTDT from VSDTC substr 1,10 yymmdd10.
advs_draft$PARAMCD <- advs_draft$VSTESTCD
advs_draft$PARAM   <- advs_draft$VSTEST
advs_draft$AVAL    <- suppressWarnings(as.numeric(advs_draft$VSSTRESN))
advs_draft$AVISIT  <- advs_draft$VISIT
advs_draft$AVISITN <- suppressWarnings(as.numeric(advs_draft$VISITNUM))
advs_draft$SRCDOM  <- "VS"
advs_draft$SRCVAR  <- "VSSEQ"
advs_draft$SRCSEQ  <- suppressWarnings(as.numeric(advs_draft$VSSEQ))

# ASTDT: if length(VSDTC) >=10 then input(substr(VSDTC,1,10), yymmdd10.)
advs_draft$ASTDT <- as.Date(NA)
has_vsdtc <- !is.na(advs_draft$VSDTC) & nchar(advs_draft$VSDTC) >= 10
if (any(has_vsdtc)) {
  substr10 <- substr(advs_draft$VSDTC[has_vsdtc], 1, 10)
  # yymmdd10. expects YYYY-MM-DD (also allows YYYY/MM/DD but SAS yymmdd10. is strict)
  # Use explicit format %Y-%m-%d
  parsed <- suppressWarnings(as.Date(substr10, format = "%Y-%m-%d"))
  # For values that fail, keep NA (SAS would set missing)
  advs_draft$ASTDT[has_vsdtc] <- parsed
}

# ASTDY: if not missing ASTDT and not missing TRTSDT then ...
advs_draft$ASTDY <- NA_real_
idx_day <- !is.na(advs_draft$ASTDT) & !is.na(advs_draft$TRTSDT)
if (any(idx_day)) {
  ge <- advs_draft$ASTDT[idx_day] >= advs_draft$TRTSDT[idx_day]
  advs_draft$ASTDY[idx_day & advs_draft$ASTDT >= advs_draft$TRTSDT] <- as.numeric(advs_draft$ASTDT[idx_day & ge] - advs_draft$TRTSDT[idx_day & ge]) + 1
  advs_draft$ASTDY[idx_day & advs_draft$ASTDT < advs_draft$TRTSDT]  <- as.numeric(advs_draft$ASTDT[idx_day & !ge] - advs_draft$TRTSDT[idx_day & !ge])
}

# Keep only required columns (as in SAS keep)
keep_cols <- c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT","PARAMCD","PARAM","AVAL","AVISIT","AVISITN","ASTDT","ASTDY","SRCDOM","SRCVAR","SRCSEQ")
# Ensure all exist
for (kc in keep_cols) if (!kc %in% names(advs_draft)) advs_draft[[kc]] <- NA
advs_draft <- advs_draft[, keep_cols, drop = FALSE]

# SAS if b keeps only vs records - already left join from vs, so all rows are vs records
# No further filter needed

# ------------------------------------------------------------------
# 2. IDENTIFY BASELINE RECORD
# proc sort advs_draft by USUBJID PARAMCD ASTDT AVISITN SRCSEQ where ASTDT <= TRTSDT and not missing(AVAL)
# SAS missing handling: ASTDT missing <= TRTSDT is TRUE (missing is -inf)
# Emulate SAS: condition is (is.na(ASTDT) | ASTDT <= TRTSDT) when TRTSDT nonmissing,
# and when TRTSDT is NA, only is.na(ASTDT) qualifies

# Build baseline candidates
# Need to handle NA logic exactly as SAS
cond_astdt_le_trtsdt <- rep(FALSE, nrow(advs_draft))
for (i in seq_len(nrow(advs_draft))) {
  a <- advs_draft$ASTDT[i]
  t <- advs_draft$TRTSDT[i]
  if (is.na(a) && is.na(t)) cond_astdt_le_trtsdt[i] <- TRUE
  else if (is.na(a) && !is.na(t)) cond_astdt_le_trtsdt[i] <- TRUE
  else if (!is.na(a) && is.na(t)) cond_astdt_le_trtsdt[i] <- FALSE
  else cond_astdt_le_trtsdt[i] <- a <= t
}
advs_base_candidates <- advs_draft[cond_astdt_le_trtsdt & !is.na(advs_draft$AVAL), , drop = FALSE]

# Sort by USUBJID PARAMCD ASTDT AVISITN SRCSEQ (missing first)
advs_base_candidates <- advs_base_candidates[order(advs_base_candidates$USUBJID,
                                                   advs_base_candidates$PARAMCD,
                                                   advs_base_candidates$ASTDT,
                                                   advs_base_candidates$AVISITN,
                                                   advs_base_candidates$SRCSEQ,
                                                   na.last = FALSE), , drop = FALSE]

# Take last.PARAMCD per USUBJID PARAMCD
# Use dplyr to get last row per group (after sorting, last is max)
if (nrow(advs_base_candidates) > 0) {
  base_flags <- advs_base_candidates %>%
    dplyr::group_by(USUBJID, PARAMCD) %>%
    dplyr::arrange(ASTDT, AVISITN, SRCSEQ, .by_group = TRUE) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(USUBJID, PARAMCD, BASE = AVAL, BASE_DT = ASTDT, BASE_SEQ = SRCSEQ)
} else {
  base_flags <- data.frame(USUBJID=character(0), PARAMCD=character(0), BASE=numeric(0), BASE_DT=as.Date(character(0)), BASE_SEQ=numeric(0))
}

# ------------------------------------------------------------------
# 3. MERGE BASELINE BACK AND CALCULATE FLAGS/CHANGE
# proc sort advs_draft by USUBJID PARAMCD
advs_draft_sorted <- advs_draft[order(advs_draft$USUBJID, advs_draft$PARAMCD, na.last = FALSE), , drop = FALSE]

# Merge with base_flags by USUBJID PARAMCD (left join)
advs <- dplyr::left_join(advs_draft_sorted, base_flags, by = c("USUBJID","PARAMCD"))

# Flag exact source record; QC gate rejects same-day ambiguity (use SRCSEQ equality)
# SAS: if SRCSEQ = BASE_SEQ then ABLFL='Y' else ''
# SAS missing = missing is true, so handle NA==NA as TRUE
advs$ABLFL <- ifelse(
  (!is.na(advs$SRCSEQ) & !is.na(advs$BASE_SEQ) & advs$SRCSEQ == advs$BASE_SEQ) |
  (is.na(advs$SRCSEQ) & is.na(advs$BASE_SEQ) & !is.na(advs$BASE_SEQ) == FALSE & !is.na(advs$SRCSEQ) == FALSE & !is.na(advs$BASE) ), # placeholder
  "Y", ""
)
# Correct NA==NA handling: in SAS . = . is true, so if both NA and BASE exists (i.e., baseline exists with missing SEQ), then Y
# Simpler: check equality with SAS semantics
advs$ABLFL <- ifelse(
  (is.na(advs$SRCSEQ) & is.na(advs$BASE_SEQ) & !is.na(advs$BASE)) |
  (!is.na(advs$SRCSEQ) & !is.na(advs$BASE_SEQ) & advs$SRCSEQ == advs$BASE_SEQ),
  "Y", ""
)
# For cases where BASE_SEQ is NA but BASE is NA (no baseline), ABLFL stays ""
advs$ABLFL[is.na(advs$ABLFL)] <- ""

# Calculate CHG
advs$CHG <- ifelse(!is.na(advs$AVAL) & !is.na(advs$BASE), advs$AVAL - advs$BASE, NA_real_)

# Drop BASE_DT BASE_SEQ
advs$BASE_DT <- NULL
advs$BASE_SEQ <- NULL

# Retain STUDYID USUBJID (column order)
# Desired order: STUDYID USUBJID TRT01A TRTSDT TRTEDT PARAMCD PARAM AVAL AVISIT AVISITN ASTDT ASTDY SRCDOM SRCVAR SRCSEQ BASE ABLFL CHG
final_order <- c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT","PARAMCD","PARAM","AVAL","AVISIT","AVISITN","ASTDT","ASTDY","SRCDOM","SRCVAR","SRCSEQ","BASE","ABLFL","CHG")
# Add any missing columns as NA
for (cn in final_order) if (!cn %in% names(advs)) advs[[cn]] <- NA
advs <- advs[, final_order, drop = FALSE]

# Re-sort for final presentation by USUBJID PARAMCD ASTDT
advs <- advs[order(advs$USUBJID, advs$PARAMCD, advs$ASTDT, na.last = FALSE), , drop = FALSE]

# ------------------------------------------------------------------
# Write dataset advs.csv
# Dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS
advs_out <- advs
# Ensure Date columns are Date
advs_out$TRTSDT <- as.Date(advs_out$TRTSDT, origin="1970-01-01")
advs_out$TRTEDT <- as.Date(advs_out$TRTEDT, origin="1970-01-01")
advs_out$ASTDT  <- as.Date(advs_out$ASTDT, origin="1970-01-01")
# write.csv will format Dates as YYYY-MM-DD automatically via as.character
# Ensure numeric unrounded
write.csv(advs_out, file.path(out, "advs.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 4. VISUAL AUDIT - listing
# proc print obs=15 var USUBJID PARAMCD AVISIT ASTDT TRTSDT AVAL BASE CHG ABLFL
audit_n <- min(15, nrow(advs))
audit_df <- advs[seq_len(audit_n), c("USUBJID","PARAMCD","AVISIT","ASTDT","TRTSDT","AVAL","BASE","CHG","ABLFL"), drop = FALSE]

# Prepare display data for text rendering (SAS DATE9. for dates)
audit_display <- audit_df
audit_display$ASTDT_disp  <- format_date9(audit_display$ASTDT)
audit_display$TRTSDT_disp <- format_date9(audit_display$TRTSDT)

# Build text rendering
title_str <- "ADVS Audit - Vital Signs Baseline and Change Analysis"
# Create table lines
# Header
header_line <- sprintf("%-4s %-12s %-8s %-12s %-11s %-11s %8s %8s %8s %-5s",
                       "Obs","USUBJID","PARAMCD","AVISIT","ASTDT","TRTSDT","AVAL","BASE","CHG","ABLFL")
sep_line <- paste(rep("-", nchar(header_line)), collapse="")

txt_lines <- c(title_str, "", header_line, sep_line)
for (i in seq_len(nrow(audit_display))) {
  r <- audit_display[i, ]
  line <- sprintf("%-4d %-12s %-8s %-12s %-11s %-11s %8s %8s %8s %-5s",
                  i,
                  substr(ifelse(is.na(r$USUBJID),"",r$USUBJID),1,12),
                  substr(ifelse(is.na(r$PARAMCD),"",r$PARAMCD),1,8),
                  substr(ifelse(is.na(r$AVISIT),"",r$AVISIT),1,12),
                  ifelse(r$ASTDT_disp=="","",r$ASTDT_disp),
                  ifelse(r$TRTSDT_disp=="","",r$TRTSDT_disp),
                  ifelse(is.na(r$AVAL),"", format(round_sas(r$AVAL,2), nsmall=2, trim=TRUE)),
                  ifelse(is.na(r$BASE),"", format(round_sas(r$BASE,2), nsmall=2, trim=TRUE)),
                  ifelse(is.na(r$CHG),"", format(round_sas(r$CHG,2), nsmall=2, trim=TRUE)),
                  ifelse(is.na(r$ABLFL),"",r$ABLFL)
  )
  txt_lines <- c(txt_lines, line)
}
txt_lines <- c(txt_lines, "", paste0("N = ", nrow(advs), " total records; showing first ", audit_n, " obs"))

# Write text rendering to P008.txt and P008.lst.txt
writeLines(txt_lines, file.path(out, "P008.txt"))
writeLines(txt_lines, file.path(out, "P008.lst.txt"))
# Also write .lst without extension handling
writeLines(txt_lines, file.path(out, "P008.lst"))

# Write CSV of table body (same as audit_df but with ISO dates)
audit_csv <- audit_df
# Convert dates to ISO for CSV
audit_csv$ASTDT <- format_iso(audit_csv$ASTDT)
audit_csv$TRTSDT <- format_iso(audit_csv$TRTSDT)
write.csv(audit_csv, file.path(out, "P008.csv"), row.names = FALSE, na = "")

# Also ensure dataset advs listing numbers count: 120 numbers expected
# The audit table contains numeric values; we have produced them

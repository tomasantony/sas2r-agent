out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "/bench/data/derived/c2c/data/adam"
sdtm_dir <- "/bench/data/derived/c2c/data/sdtm"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_to_R_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.character(x)) {
    x2 <- trimws(x)
    x2[x2 == ""] <- NA_character_
    return(as.Date(substr(x2, 1, 10), format = "%Y-%m-%d"))
  }
  if (is.numeric(x) || is.integer(x)) {
    return(as.Date(x, origin = "1960-01-01"))
  }
  return(as.Date(as.character(x), format = "%Y-%m-%d"))
}
to_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.numeric(x))
  if (is.character(x)) {
    x2 <- trimws(x)
    x2[x2 == ""] <- NA_character_
    return(suppressWarnings(as.numeric(x2)))
  }
  suppressWarnings(as.numeric(as.vector(x)))
}
to_char <- function(x) {
  if (is.character(x)) {
    x2 <- trimws(x, which = "right")
    x2[x2 == ""] <- NA_character_
    return(x2)
  }
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.character(x))
  if (haven::is_labelled(x)) x <- haven::zap_labels(haven::zap_formats(x))
  if (is.numeric(x) || is.integer(x) || is.logical(x)) {
    return(as.character(as.vector(x)))
  }
  if (is.factor(x)) return(as.character(x))
  return(as.character(as.vector(x)))
}
zap_df <- function(df) {
  names(df) <- toupper(names(df))
  for (nm in names(df)) {
    v <- df[[nm]]
    if (haven::is_labelled(v)) {
      v <- haven::zap_labels(haven::zap_formats(v))
      df[[nm]] <- as.vector(v)
    }
  }
  df
}
find_file <- function(dir, dsname) {
  allf <- list.files(dir, full.names = TRUE, recursive = TRUE)
  if (length(allf) == 0) stop(paste0("no files in ", dir))
  base_noext <- tolower(tools::file_path_sans_ext(basename(allf)))
  idx <- which(base_noext == tolower(dsname))
  if (length(idx) == 0) stop(paste0("dataset ", dsname, " not found in ", dir, " : ", paste(basename(allf), collapse="; ")))
  allf[idx[1]]
}
read_any <- function(f) {
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    return(df)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    return(df)
  } else if (ext == "csv") {
    df <- readr::read_csv(f, show_col_types = FALSE, guess_max = 1000000)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    return(df)
  } else if (ext %in% c("xls","xlsx")) {
    df <- readxl::read_excel(f)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    return(df)
  } else {
    stop(paste0("unsupported extension: ", f))
  }
}
format_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, format = "%d%b%Y")))
}
fmt_num <- function(x) {
  ifelse(is.na(x), "", as.character(x))
}

adsl_file <- find_file(adam_dir, "adsl")
vs_file <- find_file(sdtm_dir, "vs")
adsl_raw <- zap_df(read_any(adsl_file))
vs_raw <- zap_df(read_any(vs_file))

stopifnot("USUBJID" %in% names(adsl_raw))
stopifnot("USUBJID" %in% names(vs_raw))

# Normalize needed columns existence
for (c in c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT")) {
  if (!c %in% names(adsl_raw)) adsl_raw[[c]] <- NA
}
for (c in c("STUDYID","USUBJID","VSSEQ","VSTESTCD","VSTEST","VSSTRESN","VISIT","VISITNUM","VSDTC")) {
  if (!c %in% names(vs_raw)) vs_raw[[c]] <- NA
}

# Prepare ADSL sorted by USUBJID (SAS missing first, ASCII order)
adsl_raw$USUBJID <- to_char(adsl_raw$USUBJID)
adsl_raw$STUDYID <- to_char(adsl_raw$STUDYID)
adsl_raw$TRT01A <- to_char(adsl_raw$TRT01A)
# TRTSDT / TRTEDT : numeric SAS dates -> R Date; if character ISO parse
if (is.numeric(adsl_raw$TRTSDT)) {
  adsl_raw$TRTSDT <- as.Date(adsl_raw$TRTSDT, origin = "1960-01-01")
} else if (is.character(adsl_raw$TRTSDT)) {
  adsl_raw$TRTSDT <- as.Date(substr(trimws(adsl_raw$TRTSDT),1,10), format="%Y-%m-%d")
} else {
  adsl_raw$TRTSDT <- sas_to_R_date(adsl_raw$TRTSDT)
}
if (is.numeric(adsl_raw$TRTEDT)) {
  adsl_raw$TRTEDT <- as.Date(adsl_raw$TRTEDT, origin = "1960-01-01")
} else if (is.character(adsl_raw$TRTEDT)) {
  adsl_raw$TRTEDT <- as.Date(substr(trimws(adsl_raw$TRTEDT),1,10), format="%Y-%m-%d")
} else {
  adsl_raw$TRTEDT <- sas_to_R_date(adsl_raw$TRTEDT)
}
o_adsl <- order(adsl_raw$USUBJID, na.last = FALSE)
adsl_sorted <- adsl_raw[o_adsl, , drop = FALSE]

# Prepare VS sorted by USUBJID VSDTC VSTESTCD
vs_raw$USUBJID <- to_char(vs_raw$USUBJID)
vs_raw$VSDTC <- to_char(vs_raw$VSDTC)
vs_raw$VSTESTCD <- to_char(vs_raw$VSTESTCD)
vs_raw$VSTEST <- to_char(vs_raw$VSTEST)
vs_raw$VISIT <- to_char(vs_raw$VISIT)
vs_raw$VSSTRESN <- to_num(vs_raw$VSSTRESN)
vs_raw$VISITNUM <- to_num(vs_raw$VISITNUM)
vs_raw$VSSEQ <- to_num(vs_raw$VSSEQ)
# keep STUDYID from VS aside to avoid .x/.y ; will drop before merge
o_vs <- order(vs_raw$USUBJID, vs_raw$VSDTC, vs_raw$VSTESTCD, na.last = FALSE)
vs_sorted <- vs_raw[o_vs, , drop = FALSE]

# 1. MERGE ADSL AND VS : keep only records that have vital signs (if b)
# Drop overlapping non-key columns from VS except keys (USUBJID)
# STUDYID present in both -> keep ADSL version
vs_for_merge <- vs_sorted
if ("STUDYID" %in% names(vs_for_merge)) vs_for_merge$STUDYID <- NULL
# Also drop any other overlapping columns except USUBJID to avoid .x/.y
common_cols <- intersect(names(adsl_sorted), names(vs_for_merge))
common_cols <- setdiff(common_cols, "USUBJID")
if (length(common_cols) > 0) {
  for (cc in common_cols) {
    # keep ADSL version, drop VS version (except those needed? none needed)
    # But TRT variables only in ADSL, VS vars only in VS, so this should be empty
    vs_for_merge[[cc]] <- NULL
  }
}
stopifnot("USUBJID" %in% names(adsl_sorted))
stopifnot("USUBJID" %in% names(vs_for_merge))

adsl_keys <- adsl_sorted[, c("USUBJID","STUDYID","TRT01A","TRTSDT","TRTEDT"), drop = FALSE]
# one row per USUBJID assumed; if duplicates keep first (SAS merge would interleave, but ADSL unique)
adsl_keys <- adsl_keys[!duplicated(adsl_keys$USUBJID), , drop = FALSE]
advs_draft <- merge(vs_for_merge, adsl_keys, by = "USUBJID", all.x = TRUE, sort = FALSE)

# Derive CORE ANALYSIS VARIABLES
# PARAMCD=VSTESTCD; PARAM=VSTEST; AVAL=VSSTRESN; AVISIT=VISIT; AVISITN=VISITNUM; SRCDOM='VS'; SRCVAR='VSSEQ'; SRCSEQ=VSSEQ
advs_draft$PARAMCD <- to_char(advs_draft$VSTESTCD)
advs_draft$PARAM <- to_char(advs_draft$VSTEST)
advs_draft$AVAL <- to_num(advs_draft$VSSTRESN)
advs_draft$AVISIT <- to_char(advs_draft$VISIT)
advs_draft$AVISITN <- to_num(advs_draft$VISITNUM)
advs_draft$SRCDOM <- "VS"
advs_draft$SRCVAR <- "VSSEQ"
advs_draft$SRCSEQ <- to_num(advs_draft$VSSEQ)

# ASTDT = input(substr(VSDTC,1,10), yymmdd10.) if length(VSDTC)>=10
# SAS LENGTH strips trailing blanks; use trimmed nchar
vsdtc_trim <- trimws(ifelse(is.na(advs_draft$VSDTC), "", advs_draft$VSDTC))
len_vsdtc <- nchar(vsdtc_trim)
substr10 <- ifelse(is.na(advs_draft$VSDTC), NA_character_, substr(vsdtc_trim, 1, 10))
astdt_parsed <- as.Date(substr10, format = "%Y-%m-%d")
advs_draft$ASTDT <- as.Date(ifelse(len_vsdtc >= 10, as.numeric(astdt_parsed), NA), origin = "1970-01-01")

# ASTDY
advs_draft$ASTDY <- NA_real_
idx_both <- !is.na(advs_draft$ASTDT) & !is.na(advs_draft$TRTSDT)
idx_ge <- idx_both & (advs_draft$ASTDT >= advs_draft$TRTSDT)
idx_lt <- idx_both & (advs_draft$ASTDT < advs_draft$TRTSDT)
advs_draft$ASTDY[idx_ge] <- as.integer(advs_draft$ASTDT[idx_ge] - advs_draft$TRTSDT[idx_ge]) + 1L
advs_draft$ASTDY[idx_lt] <- as.integer(advs_draft$ASTDT[idx_lt] - advs_draft$TRTSDT[idx_lt])

# keep STUDYID USUBJID TRT01A TRTSDT TRTEDT PARAMCD PARAM AVAL AVISIT AVISITN ASTDT ASTDY SRCDOM SRCVAR SRCSEQ
advs_draft <- advs_draft[, c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT","PARAMCD","PARAM","AVAL","AVISIT","AVISITN","ASTDT","ASTDY","SRCDOM","SRCVAR","SRCSEQ"), drop = FALSE]

# 2. IDENTIFY BASELINE RECORD : sort by USUBJID PARAMCD ASTDT AVISITN SRCSEQ where ASTDT<=TRTSDT and not missing AVAL
# Need TRTSDT for where -> join TRTSDT? advs_draft has TRTSDT already
base_cand <- advs_draft[!is.na(advs_draft$AVAL) & !is.na(advs_draft$ASTDT) & !is.na(advs_draft$TRTSDT) & (advs_draft$ASTDT <= advs_draft$TRTSDT), , drop = FALSE]
o_base <- order(base_cand$USUBJID, base_cand$PARAMCD, base_cand$ASTDT, base_cand$AVISITN, base_cand$SRCSEQ, na.last = FALSE)
base_cand <- base_cand[o_base, , drop = FALSE]

# last.PARAMCD per USUBJID PARAMCD
if (nrow(base_cand) > 0) {
  base_cand$.__grp__ <- paste0(base_cand$USUBJID, "\r", base_cand$PARAMCD)
  # take last row per group (tail)
  last_idx <- tapply(seq_len(nrow(base_cand)), base_cand$.__grp__, function(ii) ii[length(ii)])
  last_idx <- as.integer(last_idx)
  base_flags <- base_cand[last_idx, , drop = FALSE]
  base_flags$BASE <- base_flags$AVAL
  base_flags$BASE_DT <- base_flags$ASTDT
  base_flags$BASE_SEQ <- base_flags$SRCSEQ
  base_flags <- base_flags[, c("USUBJID","PARAMCD","BASE","BASE_DT","BASE_SEQ"), drop = FALSE]
  # sort by USUBJID PARAMCD for merge
  o_bf <- order(base_flags$USUBJID, base_flags$PARAMCD, na.last = FALSE)
  base_flags <- base_flags[o_bf, , drop = FALSE]
} else {
  base_flags <- data.frame(USUBJID=character(0), PARAMCD=character(0), BASE=numeric(0), BASE_DT=as.Date(character(0)), BASE_SEQ=numeric(0), stringsAsFactors=FALSE)
}

# 3. MERGE BASELINE BACK : sort advs_draft by USUBJID PARAMCD
o_draft2 <- order(advs_draft$USUBJID, advs_draft$PARAMCD, na.last = FALSE)
advs_draft <- advs_draft[o_draft2, , drop = FALSE]
# merge draft(in=a) base_flags(in=b) by USUBJID PARAMCD ; if a
advs <- merge(advs_draft, base_flags, by = c("USUBJID","PARAMCD"), all.x = TRUE, sort = FALSE)
stopifnot("SRCSEQ" %in% names(advs))
stopifnot("AVAL" %in% names(advs))
# Flag exact source record
advs$ABLFL <- NA_character_
idx_flag <- !is.na(advs$SRCSEQ) & !is.na(advs$BASE_SEQ) & (advs$SRCSEQ == advs$BASE_SEQ)
advs$ABLFL[idx_flag] <- "Y"
# For SAS '' missing -> keep NA (will write as blank)
# CHG
advs$CHG <- NA_real_
idx_chg <- !is.na(advs$AVAL) & !is.na(advs$BASE)
advs$CHG[idx_chg] <- advs$AVAL[idx_chg] - advs$BASE[idx_chg]
advs$BASE_DT <- NULL
advs$BASE_SEQ <- NULL

# Re-sort for final presentation by USUBJID PARAMCD ASTDT
o_final <- order(advs$USUBJID, advs$PARAMCD, advs$ASTDT, na.last = FALSE)
advs <- advs[o_final, , drop = FALSE]
# retain STUDYID USUBJID first
advs <- advs[, c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT","PARAMCD","PARAM","AVAL","AVISIT","AVISITN","ASTDT","ASTDY","SRCDOM","SRCVAR","SRCSEQ","BASE","ABLFL","CHG"), drop = FALSE]

# Write dataset advs.csv : dates YYYY-MM-DD, na=""
advs_out <- advs
# Ensure Date class for TRTSDT TRTEDT ASTDT
advs_out$TRTSDT <- as.Date(advs_out$TRTSDT, origin="1970-01-01")
advs_out$TRTEDT <- as.Date(advs_out$TRTEDT, origin="1970-01-01")
advs_out$ASTDT <- as.Date(advs_out$ASTDT, origin="1970-01-01")
write.csv(advs_out, file.path(out, "advs.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT : PROC PRINT obs=15 var USUBJID PARAMCD AVISIT ASTDT TRTSDT AVAL BASE CHG ABLFL
print_vars <- c("USUBJID","PARAMCD","AVISIT","ASTDT","TRTSDT","AVAL","BASE","CHG","ABLFL")
audit <- advs[seq_len(min(15, nrow(advs))), print_vars, drop = FALSE]

# CSV of table body (ISO dates)
audit_csv <- audit
audit_csv$ASTDT <- as.Date(audit_csv$ASTDT, origin="1970-01-01")
audit_csv$TRTSDT <- as.Date(audit_csv$TRTSDT, origin="1970-01-01")
audit_csv <- cbind(Obs = seq_len(nrow(audit_csv)), audit_csv)
write.csv(audit_csv, file.path(out, "P008.csv"), row.names = FALSE, na = "")

# Text rendering mimicking PROC PRINT with title
ttl <- "ADVS Audit - Vital Signs Baseline and Change Analysis"
# Build display data with DATE9 for dates
disp <- data.frame(
  Obs = seq_len(nrow(audit)),
  USUBJID = ifelse(is.na(audit$USUBJID), "", audit$USUBJID),
  PARAMCD = ifelse(is.na(audit$PARAMCD), "", audit$PARAMCD),
  AVISIT = ifelse(is.na(audit$AVISIT), "", audit$AVISIT),
  ASTDT = format_date9(as.Date(audit$ASTDT, origin="1970-01-01")),
  TRTSDT = format_date9(as.Date(audit$TRTSDT, origin="1970-01-01")),
  AVAL = fmt_num(audit$AVAL),
  BASE = fmt_num(audit$BASE),
  CHG = fmt_num(audit$CHG),
  ABLFL = ifelse(is.na(audit$ABLFL), "", audit$ABLFL),
  stringsAsFactors = FALSE
)
# Fixed-width text
hdr <- sprintf("%-5s %-20s %-8s %-18s %-10s %-10s %-10s %-10s %-10s %-5s",
  "Obs","USUBJID","PARAMCD","AVISIT","ASTDT","TRTSDT","AVAL","BASE","CHG","ABLFL")
lines <- c(ttl, "", hdr, paste(rep("-", nchar(hdr)), collapse=""))
for (i in seq_len(nrow(disp))) {
  lines <- c(lines, sprintf("%-5d %-20s %-8s %-18s %-10s %-10s %-10s %-10s %-10s %-5s",
    disp$Obs[i], disp$USUBJID[i], disp$PARAMCD[i], disp$AVISIT[i],
    disp$ASTDT[i], disp$TRTSDT[i], disp$AVAL[i], disp$BASE[i], disp$CHG[i], disp$ABLFL[i]))
}
lines <- c(lines, "", paste0("N = ", nrow(advs), " total records; showing first ", nrow(audit), "."))
writeLines(lines, file.path(out, "P008.txt"))

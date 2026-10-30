```r
# P005 adam_adlb.sas -> R translation
# ADLB derivation with baseline, CHG, LBNRIND + audit listing

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm"
adam_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam"
rawadam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
rawsdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tibble)
})

# SAS round half away from zero (for display formats if needed)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, format = "%d%b%Y")))
}

read_any <- function(dirs, memname) {
  # search dirs in order for file with basename == memname (case-insensitive)
  exts <- c(".sas7bdat", ".xpt", ".sas7bxpt", ".csv", ".parquet")
  for (dd in dirs) {
    if (!dir.exists(dd)) next
    fl <- list.files(dd, full.names = TRUE, recursive = FALSE)
    if (length(fl) == 0) next
    # match by name without extension
    for (f in fl) {
      bn <- basename(f)
      # strip extension
      noext <- sub("\\.[^.]*$", "", bn)
      if (toupper(noext) == toupper(memname)) {
        el <- tolower(tools::file_ext(f))
        if (el == "sas7bdat") {
          df <- haven::read_sas(f)
        } else if (el == "xpt") {
          df <- haven::read_xpt(f)
        } else if (el == "csv") {
          df <- readr::read_csv(f, show_col_types = FALSE)
        } else if (el == "parquet") {
          df <- arrow::read_parquet(f)
        } else {
          next
        }
        df <- as.data.frame(df, stringsAsFactors = FALSE)
        names(df) <- toupper(names(df))
        message(paste0("Read ", memname, " from ", f, " n=", nrow(df)))
        return(df)
      }
    }
  }
  # fallback: try list with pattern
  for (dd in dirs) {
    if (!dir.exists(dd)) next
    cand <- list.files(dd, pattern = paste0("(?i)^", memname, "\\."), full.names = TRUE)
    if (length(cand) > 0) {
      f <- cand[1]
      el <- tolower(tools::file_ext(f))
      if (el == "sas7bdat") df <- haven::read_sas(f)
      else if (el == "xpt") df <- haven::read_xpt(f)
      else if (el == "csv") df <- readr::read_csv(f, show_col_types = FALSE)
      else if (el == "parquet") df <- arrow::read_parquet(f)
      else next
      df <- as.data.frame(df, stringsAsFactors = FALSE)
      names(df) <- toupper(names(df))
      message(paste0("Read ", memname, " from ", f, " n=", nrow(df)))
      return(df)
    }
  }
  stop(paste0("Dataset not found: ", memname))
}

to_R_Date <- function(x) {
  # Convert SAS numeric date or character ISO date or Date to R Date
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (inherits(x, "POSIXlt")) return(as.Date(x))
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x) || is.factor(x)) {
    xc <- as.character(x)
    # strict single format YYYY-MM-DD on first 10 chars (SAS yymmdd10. equivalent)
    # empty -> NA
    res <- suppressWarnings(as.Date(substr(xc, 1, 10), format = "%Y-%m-%d"))
    return(res)
  }
  # haven labelled numeric?
  if (inherits(x, "haven_labelled")) {
    xv <- as.numeric(x)
    return(as.Date(xv, origin = "1960-01-01"))
  }
  return(suppressWarnings(as.Date(as.character(x), format = "%Y-%m-%d")))
}

as_char <- function(x) {
  if (is.null(x)) return(rep(NA_character_, 0))
  if (inherits(x, "haven_labelled")) {
    # keep formatted? use labels to values: as_character via haven?
    x <- haven::as_factor(x, levels = "labels")
    return(as.character(x))
  }
  return(as.character(x))
}
as_num <- function(x) {
  if (inherits(x, "haven_labelled")) return(as.numeric(x))
  if (is.factor(x)) return(suppressWarnings(as.numeric(as.character(x))))
  return(suppressWarnings(as.numeric(x)))
}

# ---- 1. Load ADSL and LB ----
adsl_raw <- read_any(c(adam_dir, rawadam_dir), "adsl")
lb_raw   <- read_any(c(sdtm_dir, rawsdtm_dir), "lb")

# Ensure required columns exist (case-insensitive already upper)
# ADSL needs: STUDYID USUBJID TRT01A TRTSDT TRTEDT
# LB needs: USUBJID LBDTC LBTESTCD LBTEST LBSTRESN VISIT VISITNUM LBSTRESU LBSEQ
message("ADSL cols: "); print(names(adsl_raw))
message("LB cols: "); print(names(lb_raw))

stopifnot("USUBJID" %in% names(adsl_raw))
stopifnot("USUBJID" %in% names(lb_raw))
stopifnot("LBTESTCD" %in% names(lb_raw))
stopifnot("LBDTC" %in% names(lb_raw))

# Normalize types
# ADSL
adsl <- adsl_raw
adsl$STUDYID <- as_char(adsl$STUDYID)
adsl$USUBJID <- as_char(adsl$USUBJID)
if ("TRT01A" %in% names(adsl)) adsl$TRT01A <- as_char(adsl$TRT01A) else adsl$TRT01A <- NA_character_
if ("TRTSDT" %in% names(adsl)) adsl$TRTSDT <- to_R_Date(adsl$TRTSDT) else adsl$TRTSDT <- as.Date(NA)
if ("TRTEDT" %in% names(adsl)) adsl$TRTEDT <- to_R_Date(adsl$TRTEDT) else adsl$TRTEDT <- as.Date(NA)
# keep only needed ADSL cols + USUBJID key; keep STUDYID from ADSL
# Drop/rename overlapping non-key cols before merge: STUDYID present in both -> keep ADSL version
# So remove STUDYID from LB side later

# LB
lb <- lb_raw
lb$USUBJID <- as_char(lb$USUBJID)
lb$LBDTC <- as_char(lb$LBDTC)
lb$LBTESTCD <- as_char(lb$LBTESTCD)
lb$LBTEST <- as_char(lb$LBTEST)
lb$LBSTRESN <- as_num(lb$LBSTRESN)
if ("VISIT" %in% names(lb)) lb$VISIT <- as_char(lb$VISIT) else lb$VISIT <- NA_character_
if ("VISITNUM" %in% names(lb)) lb$VISITNUM <- as_num(lb$VISITNUM) else lb$VISITNUM <- NA_real_
if ("LBSTRESU" %in% names(lb)) lb$LBSTRESU <- as_char(lb$LBSTRESU) else lb$LBSTRESU <- NA_character_
if ("LBSEQ" %in% names(lb)) lb$LBSEQ <- as_num(lb$LBSEQ) else lb$LBSEQ <- NA_real_
if ("STUDYID" %in% names(lb)) {
  # drop to avoid .x/.y suffixes; keep ADSL STUDYID (SAS merge would take LB value but they are same)
  lb$STUDYID <- NULL
}

# proc sort adsl by USUBJID ; lb by USUBJID LBDTC LBTESTCD
# SAS sorts missing first, uppercase before lowercase (binary). Use order(na.last=FALSE)
adsl_sorted <- adsl[order(adsl$USUBJID, na.last = FALSE, method = "radix"), , drop = FALSE]

lb_sorted <- lb[order(lb$USUBJID, lb$LBDTC, lb$LBTESTCD, na.last = FALSE, method = "radix"), , drop = FALSE]

# merge adsl_sorted(in=a) lb_sorted(in=b) by USUBJID; if b;
# = left join lb to adsl (keep all lb rows)
# Before merging, drop every column present in both except keys
common_cols <- intersect(names(adsl_sorted), names(lb_sorted))
key1 <- c("USUBJID")
drop_from_adsl <- setdiff(common_cols, key1)
# We already dropped STUDYID from lb; check remaining commons
# To reproduce SAS (second dataset overwrites), we keep LB version for commons? But we want no suffixes.
# Decision: drop commons from adsl side except STUDYID? Actually STUDYID already dropped from lb, so keep adsl STUDYID.
# For any other unexpected commons, drop from adsl to let lb prevail? Simpler: drop from adsl.
if (length(drop_from_adsl) > 0) {
  # keep STUDYID, TRT vars? They shouldn't be in lb. If they are, drop from lb? Let's handle:
  # If common includes STUDYID (should not now), TRT01A etc unlikely.
  # Drop commons from adsl_sorted except keep? Instead drop from lb? To avoid suffix, drop from adsl.
  # But STUDYID we want from adsl, so if STUDYID still common, drop from lb (already done).
  # General: drop from adsl_sorted
  for (cc in drop_from_adsl) {
    # if cc == STUDYID already handled; skip
    if (cc %in% names(lb_sorted) && cc %in% names(adsl_sorted)) {
      # keep adsl version for STUDYID/TRT vars, drop lb version
      # Identify which are ADSL-specific: STUDYID TRT01A TRTSDT TRTEDT -> keep adsl, drop lb
      if (cc %in% c("STUDYID","TRT01A","TRTSDT","TRTEDT")) {
        lb_sorted[[cc]] <- NULL
      } else {
        adsl_sorted[[cc]] <- NULL
      }
    }
  }
}
# re-check no suffix risk
common2 <- intersect(names(adsl_sorted), names(lb_sorted))
common2 <- setdiff(common2, key1)
if (length(common2) > 0) {
  # drop from adsl as fallback
  adsl_sorted <- adsl_sorted[, setdiff(names(adsl_sorted), common2), drop = FALSE]
}

stopifnot("USUBJID" %in% names(adsl_sorted))
stopifnot("USUBJID" %in% names(lb_sorted))

adlb_draft_merge <- merge(lb_sorted, adsl_sorted, by = "USUBJID", all.x = TRUE, sort = FALSE)

# Derive core analysis variables
# ASTDT = input(substr(LBDTC,1,10), yymmdd10.) if length>=10
n <- nrow(adlb_draft_merge)
LBDTC_c <- as_char(adlb_draft_merge$LBDTC)
len_c <- ifelse(is.na(LBDTC_c), NA_integer_, nchar(LBDTC_c))
substr10 <- ifelse(!is.na(LBDTC_c) & len_c >= 10, substr(LBDTC_c, 1, 10), NA_character_)
ASTDT <- suppressWarnings(as.Date(substr10, format = "%Y-%m-%d"))

PARAMCD <- as_char(adlb_draft_merge$LBTESTCD)
PARAM <- as_char(adlb_draft_merge$LBTEST)
AVAL <- as_num(adlb_draft_merge$LBSTRESN)
AVISIT <- as_char(adlb_draft_merge$VISIT)
AVISITN <- as_num(adlb_draft_merge$VISITNUM)
LBSTRESU <- as_char(adlb_draft_merge$LBSTRESU)
SRCSEQ <- as_num(adlb_draft_merge$LBSEQ)
SRCDOM <- rep("LB", n)
SRCVAR <- rep("LBSEQ", n)
STUDYID <- as_char(adlb_draft_merge$STUDYID)
TRT01A <- as_char(adlb_draft_merge$TRT01A)
TRTSDT <- to_R_Date(adlb_draft_merge$TRTSDT)
TRTEDT <- to_R_Date(adlb_draft_merge$TRTEDT)

# ASTDY
ASTDY <- rep(NA_real_, n)
idx <- !is.na(ASTDT) & !is.na(TRTSDT)
idx_ge <- idx & (ASTDT >= TRTSDT)
idx_lt <- idx & (ASTDT < TRTSDT)
ASTDY[idx_ge] <- as.numeric(ASTDT[idx_ge] - TRTSDT[idx_ge]) + 1
ASTDY[idx_lt] <- as.numeric(ASTDT[idx_lt] - TRTSDT[idx_lt])

adlb_draft <- data.frame(
  STUDYID = STUDYID,
  USUBJID = as_char(adlb_draft_merge$USUBJID),
  TRT01A = TRT01A,
  TRTSDT = TRTSDT,
  TRTEDT = TRTEDT,
  PARAMCD = PARAMCD,
  PARAM = PARAM,
  AVAL = AVAL,
  AVISIT = AVISIT,
  AVISITN = AVISITN,
  ASTDT = ASTDT,
  ASTDY = ASTDY,
  LBSTRESU = LBSTRESU,
  SRCDOM = SRCDOM,
  SRCVAR = SRCVAR,
  SRCSEQ = SRCSEQ,
  stringsAsFactors = FALSE
)
# keep order as SAS keep list (already)
# retain STUDYID USUBJID PARAMCD PARAM -> column order handled later

# ---- 2. Identify baseline record ----
# proc sort where ASTDT <= TRTSDT and not missing(AVAL) by USUBJID PARAMCD ASTDT AVISITN SRCSEQ
cand_idx <- !is.na(adlb_draft$AVAL) & !is.na(adlb_draft$ASTDT) & !is.na(adlb_draft$TRTSDT) & (adlb_draft$ASTDT <= adlb_draft$TRTSDT)
adlb_base_candidates <- adlb_draft[cand_idx, , drop = FALSE]
# sort
if (nrow(adlb_base_candidates) > 0) {
  o <- order(adlb_base_candidates$USUBJID, adlb_base_candidates$PARAMCD,
             adlb_base_candidates$ASTDT, adlb_base_candidates$AVISITN,
             adlb_base_candidates$SRCSEQ, na.last = FALSE, method = "radix")
  adlb_base_candidates <- adlb_base_candidates[o, , drop = FALSE]
}

# data base_flags; by USUBJID PARAMCD; if last.PARAMCD then BASE=AVAL etc.
if (nrow(adlb_base_candidates) > 0) {
  # take last row per USUBJID+PARAMCD
  # Use grouping: since sorted, tail per group
  grp <- paste0(adlb_base_candidates$USUBJID, "\x01", adlb_base_candidates$PARAMCD)
  # find last indices
  # rle approach
  last_idx <- which(!duplicated(grp, fromLast = TRUE))
  base_flags <- data.frame(
    USUBJID = adlb_base_candidates$USUBJID[last_idx],
    PARAMCD = adlb_base_candidates$PARAMCD[last_idx],
    BASE = adlb_base_candidates$AVAL[last_idx],
    BASE_DT = adlb_base_candidates$ASTDT[last_idx],
    BASE_SEQ = adlb_base_candidates$SRCSEQ[last_idx],
    stringsAsFactors = FALSE
  )
  # BASE_DT as Date
  base_flags$BASE_DT <- as.Date(base_flags$BASE_DT, origin = "1970-01-01")
} else {
  base_flags <- data.frame(USUBJID = character(0), PARAMCD = character(0),
                           BASE = numeric(0), BASE_DT = as.Date(character(0)),
                           BASE_SEQ = numeric(0), stringsAsFactors = FALSE)
}

# ---- 3. Merge baseline back and calculate flags/change ----
# proc sort adlb_draft by USUBJID PARAMCD
o2 <- order(adlb_draft$USUBJID, adlb_draft$PARAMCD, na.last = FALSE, method = "radix")
adlb_draft <- adlb_draft[o2, , drop = FALSE]

# merge adlb_draft(in=a) base_flags(in=b) by USUBJID PARAMCD; if a;
# left join
# ensure no overlapping non-key cols (BASE etc unique)
adlb_m <- merge(adlb_draft, base_flags[, c("USUBJID","PARAMCD","BASE","BASE_DT","BASE_SEQ")],
                by = c("USUBJID","PARAMCD"), all.x = TRUE, sort = FALSE)

stopifnot("SRCSEQ" %in% names(adlb_m))
stopifnot("BASE_SEQ" %in% names(adlb_m))

# Flag exact source record
ABLFL <- rep("", nrow(adlb_m))
# SAS: if SRCSEQ = BASE_SEQ then ABLFL='Y'; else ABLFL='';
# In SAS missing = missing is true? Actually in SAS, missing numeric = missing is TRUE (both .). But BASE_SEQ missing means no baseline; SRCSEQ missing would then match? SRCSEQ essentially never missing. To replicate QC gate (reject same-day ambiguity) they compare SEQ equality.
# Implement: if both non-missing and equal -> 'Y' else ''
eq <- !is.na(adlb_m$SRCSEQ) & !is.na(adlb_m$BASE_SEQ) & (adlb_m$SRCSEQ == adlb_m$BASE_SEQ)
eq[is.na(eq)] <- FALSE
ABLFL[eq] <- "Y"

# CHG
CHG <- rep(NA_real_, nrow(adlb_m))
idx_chg <- !is.na(adlb_m$AVAL) & !is.na(adlb_m$BASE)
CHG[idx_chg] <- adlb_m$AVAL[idx_chg] - adlb_m$BASE[idx_chg]

# LBNRIND
LBNRIND <- rep(NA_character_, nrow(adlb_m))
AVALv <- adlb_m$AVAL
UNITv <- adlb_m$LBSTRESU
PCD <- adlb_m$PARAMCD
for (i in seq_len(nrow(adlb_m))) {
  av <- AVALv[i]
  if (is.na(av)) {
    LBNRIND[i] <- NA_character_
  } else {
    calc_val <- av
    p <- PCD[i]
    if (is.na(p)) {
      LBNRIND[i] <- "UNKNOWN"
    } else if (p == "GLUC") {
      u <- UNITv[i]
      if (!is.na(u) && toupper(u) == "MG/DL") calc_val <- av * 0.0555
      if (calc_val < 3.9) LBNRIND[i] <- "LOW"
      else if (calc_val > 5.6) LBNRIND[i] <- "HIGH"
      else LBNRIND[i] <- "NORMAL"
    } else if (p == "HGB") {
      u <- UNITv[i]
      if (!is.na(u) && toupper(u) == "G/DL") calc_val <- av * 10
      if (calc_val < 120) LBNRIND[i] <- "LOW"
      else if (calc_val > 175) LBNRIND[i] <- "HIGH"
      else LBNRIND[i] <- "NORMAL"
    } else {
      LBNRIND[i] <- "UNKNOWN"
    }
  }
}

adlb_m$ABLFL <- ABLFL
adlb_m$CHG <- CHG
adlb_m$LBNRIND <- LBNRIND

# drop _calc_val BASE_DT BASE_SEQ (BASE_DT/BASE_SEQ drop, keep BASE)
adlb_m$BASE_DT <- NULL
adlb_m$BASE_SEQ <- NULL

# retain STUDYID USUBJID PARAMCD PARAM -> reorder, then rest in SAS order
desired_order <- c("STUDYID","USUBJID","PARAMCD","PARAM","TRT01A","TRTSDT","TRTEDT",
                   "AVAL","AVISIT","AVISITN","ASTDT","ASTDY","LBSTRESU","SRCDOM","SRCVAR","SRCSEQ",
                   "BASE","ABLFL","CHG","LBNRIND")
# add any missing cols as NA (should not happen)
for (cc in desired_order) {
  if (!cc %in% names(adlb_m)) {
    adlb_m[[cc]] <- NA
  }
}
adlb <- adlb_m[, desired_order, drop = FALSE]

# Ensure Date classes
adlb$TRTSDT <- to_R_Date(adlb$TRTSDT)
adlb$TRTEDT <- to_R_Date(adlb$TRTEDT)
adlb$ASTDT <- to_R_Date(adlb$ASTDT)

# Re-sort for final presentation by USUBJID PARAMCD ASTDT (missing first)
o3 <- order(adlb$USUBJID, adlb$PARAMCD, adlb$ASTDT, na.last = FALSE, method = "radix")
adlb <- adlb[o3, , drop = FALSE]

# ---- Write dataset adlb.csv ----
# dates as YYYY-MM-DD (Date class writes ISO), numerics unrounded
write.csv(adlb, file.path(out, "adlb.csv"), row.names = FALSE, na = "")

# ---- 4. Visual audit: PROC PRINT obs=15 vars USUBJID PARAMCD AVISIT ASTDT AVAL LBSTRESU BASE CHG ABLFL LBNRIND ----
print_vars <- c("USUBJID","PARAMCD","AVISIT","ASTDT","AVAL","LBSTRESU","BASE","CHG","ABLFL","LBNRIND")
audit <- head(adlb[, print_vars, drop = FALSE], 15)

# CSV of table body (ISO dates)
write.csv(audit, file.path(out, "P005.csv"), row.names = FALSE, na = "")

# Text rendering with SAS-like formatting
# SAS PROC PRINT default: dates with DATE9., numbers BEST12.
fmt_num <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    # mimic BEST12.: up to 12 chars, strip trailing zeros
    # Use format with 12 significant digits, like SAS BEST
    # Use as.character with full precision but trimmed
    # Use format(v, digits=12, scientific=FALSE, trim=TRUE) then strip?
    txt <- format(v, digits = 12, scientific = FALSE, trim = TRUE, nsmall = 0)
    # remove trailing zeros after decimal? SAS BEST does
    # format already minimal; just trim spaces
    trimws(txt)
  }, USE.NAMES = FALSE)
}

audit_disp <- audit
audit_disp$ASTDT_disp <- sas_date9(audit$ASTDT)
audit_disp$AVAL_disp <- fmt_num(audit$AVAL)
audit_disp$BASE_disp <- fmt_num(audit$BASE)
audit_disp$CHG_disp <- fmt_num(audit$CHG)

# Build fixed-width-ish text lines
title_txt <- "ADLB Audit - Laboratory Analysis with Clinical Indicators"

# Header
hdr <- sprintf("%-4s %-12s %-8s %-12s %-9s %-8s %-10s %-6s %-8s %-6s %-8s",
               "Obs", "USUBJID", "PARAMCD", "AVISIT", "ASTDT", "AVAL", "LBSTRESU", "BASE", "CHG", "ABLFL", "LBNRIND")
# Actually need columns: USUBJID PARAMCD AVISIT ASTDT AVAL LBSTRESU BASE CHG ABLFL LBNRIND
hdr2 <- sprintf("%-4s %-14s %-8s %-12s %-9s %-10s %-10s %-10s %-10s %-6s %-8s",
                "Obs", "USUBJID", "PARAMCD", "AVISIT", "ASTDT", "AVAL", "LBSTRESU", "BASE", "CHG", "ABLFL", "LBNRIND")
lines <- c(title_txt, "", hdr2, paste0(strrep("-", 120)))
for (r in seq_len(nrow(audit_disp))) {
  usub <- ifelse(is.na(audit$USUBJID[r]), "", audit$USUBJID[r])
  pcd <- ifelse(is.na(audit$PARAMCD[r]), "", audit$PARAMCD[r])
  avis <- ifelse(is.na(audit$AVISIT[r]), "", audit$AVISIT[r])
  adt <- audit_disp$ASTDT_disp[r]
  av <- audit_disp$AVAL_disp[r]
  unit <- ifelse(is.na(audit$LBSTRESU[r]), "", audit$LBSTRESU[r])
  bs <- audit_disp$BASE_disp[r]
  ch <- audit_disp$CHG_disp[r]
  ab <- ifelse(is.na(audit$ABLFL[r]), "", audit$ABLFL[r])
  ln <- ifelse(is.na(audit$LBNRIND[r]), "", audit$LBNRIND[r])
  lines <- c(lines, sprintf("%-4d %-14s %-8s %-12s %-9s %-10s %-10s %-10s %-10s %-6s %-8s",
                            r, usub, pcd, avis, adt, av, unit, bs, ch, ab, ln))
}
lines <- c(lines, "", paste0("N = ", nrow(adlb), " total records in ADLB; showing first ", nrow(audit), "."))

writeLines(lines, file.path(out, "P005.txt"))

message("Done. ADLB n=")
message(nrow(adlb))
```
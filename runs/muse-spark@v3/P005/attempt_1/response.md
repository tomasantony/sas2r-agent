```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
})

# round half away from zero (SAS) helper
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# DATE9. format helper e.g. 05JAN2013
fmt_date9 <- function(x) {
  sapply(x, function(d) {
    if (is.na(d)) return("")
    d <- as.Date(d, origin = "1970-01-01")
    lt <- as.POSIXlt(d)
    mon <- toupper(month.abb[lt$mon + 1])
    sprintf("%02d%s%04d", lt$mday, mon, lt$year + 1900)
  }, USE.NAMES = FALSE)
}

iso_date <- function(x) {
  sapply(x, function(d) {
    if (is.na(d)) return(NA_character_)
    format(as.Date(d, origin = "1970-01-01"), "%Y-%m-%d")
  }, USE.NAMES = FALSE)
}

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
adam_dir <- file.path(project_path, "data/adam")
sdtm_dir <- file.path(project_path, "data/sdtm")
raw_dir  <- file.path(project_path, "data/raw")

find_ds <- function(dir, dsname) {
  if (!dir.exists(dir)) return(NULL)
  fl <- list.files(dir, full.names = TRUE, recursive = TRUE)
  if (length(fl) == 0) return(NULL)
  low <- tolower(fl)
  want <- tolower(dsname)
  # exact basename match without extension first
  base <- tolower(tools::file_path_sans_ext(basename(fl)))
  idx <- which(base == want)
  if (length(idx) == 0) {
    idx <- which(grepl(paste0("(^|_)"), low) & grepl(want, low))
  }
  if (length(idx) == 0) return(NULL)
  # prefer sas7bdat, then xpt, then csv
  pref <- function(p) {
    e <- tolower(tools::file_ext(p))
    if (e == "sas7bdat") return(1)
    if (e == "xpt") return(2)
    if (e == "sas7bdat") return(1)
    if (e == "csv") return(3)
    if (e == "rda" || e == "rds") return(4)
    return(5)
  }
  ord <- order(sapply(fl[idx], pref))
  fl[idx[ord[1]]]
}

read_any <- function(path) {
  if (is.null(path) || length(path) == 0 || is.na(path)) stop(paste0("dataset not found: ", path))
  e <- tolower(tools::file_ext(path))
  if (e == "sas7bdat") {
    df <- haven::read_sas(path)
  } else if (e == "xpt") {
    df <- haven::read_xpt(path)
  } else if (e == "csv") {
    df <- readr::read_csv(path, show_col_types = FALSE, guess_max = 100000)
    df <- as.data.frame(df)
  } else if (e %in% c("rds")) {
    df <- as.data.frame(readRDS(path))
  } else {
    df <- haven::read_sas(path)
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # strip haven labels/formats
  for (nm in names(df)) {
    cl <- class(df[[nm]])
    if (any(cl %in% c("haven_labelled", "haven_labelled_spss"))) {
      df[[nm]] <- haven::zap_labels(df[[nm]])
      df[[nm]] <- haven::zap_formats(df[[nm]])
    }
    if (inherits(df[[nm]], "POSIXct") || inherits(df[[nm]], "POSIXt")) {
      # keep as Date if time midnight? keep as Date for date vars
      # leave conversion to caller; convert to Date
      df[[nm]] <- as.Date(df[[nm]])
    }
  }
  names(df) <- toupper(names(df))
  df
}

adsl_path <- find_ds(adam_dir, "adsl")
lb_path   <- find_ds(sdtm_dir, "lb")
if (is.null(adsl_path)) adsl_path <- find_ds(raw_dir, "adsl")
if (is.null(lb_path)) lb_path <- find_ds(raw_dir, "lb")

cat("ADSL:", adsl_path, "\n")
cat("LB:", lb_path, "\n")
stopifnot(!is.null(adsl_path), !is.null(lb_path))

adsl <- read_any(adsl_path)
lb   <- read_any(lb_path)

cat("ADSL rows:", nrow(adsl), "cols:", paste(names(adsl), collapse=","), "\n")
cat("LB rows:", nrow(lb), "cols:", paste(names(lb), collapse=","), "\n")

# Ensure required columns exist
stopifnot("USUBJID" %in% names(adsl))
stopifnot("USUBJID" %in% names(lb))
stopifnot("LBTESTCD" %in% names(lb))
stopifnot("LBSTRESN" %in% names(lb) || "LBSTRESN" %in% names(lb))

# Helper to convert SAS numeric date / Date / character to Date
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x) || is.factor(x)) {
    x <- as.character(x)
    # already ISO? parse single strict format YYYY-MM-DD for first 10 chars
    # return NA for blank
    res <- suppressWarnings(as.Date(substr(trimws(x), 1, 10), format = "%Y-%m-%d"))
    return(res)
  }
  suppressWarnings(as.Date(x, origin = "1970-01-01"))
}

# Normalize ADSL date vars if present
for (v in c("TRTSDT", "TRTEDT")) {
  if (v %in% names(adsl)) adsl[[v]] <- to_date(adsl[[v]])
}
# Ensure character vars are character
for (v in c("STUDYID","USUBJID","TRT01A")) {
  if (v %in% names(adsl)) adsl[[v]] <- as.character(adsl[[v]])
}
for (v in c("USUBJID","LBDTC","LBTESTCD","LBTEST","VISIT","LBSTRESU")) {
  if (v %in% names(lb)) lb[[v]] <- as.character(lb[[v]])
}
for (v in c("LBSTRESN","VISITNUM","LBSEQ")) {
  if (v %in% names(lb)) lb[[v]] <- suppressWarnings(as.numeric(lb[[v]]))
}

# Sort emulation: SAS sorts character uppercase before lowercase (C locale)
# Use order with default; ensure missing first later.

# 1. MERGE ADSL AND LB : keep only records that have lab tests (if b)
# Drop overlapping non-key columns from LB (keep ADSL version), keys = USUBJID
keys <- c("USUBJID")
overlap <- intersect(names(adsl), names(lb))
nonkey_overlap <- setdiff(overlap, keys)
cat("Overlapping non-key cols:", paste(nonkey_overlap, collapse=","), "\n")
lb_for_merge <- lb
if (length(nonkey_overlap) > 0) {
  lb_for_merge <- lb_for_merge[, setdiff(names(lb_for_merge), nonkey_overlap), drop = FALSE]
}
# Select ADSL cols needed (keep all that are in keep list if exist)
adsl_keep <- intersect(c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT"), names(adsl))
adsl_s <- adsl[, adsl_keep, drop = FALSE]
# sort
adsl_s <- adsl_s[order(adsl_s$USUBJID, na.last = FALSE), , drop = FALSE]
lb_for_merge <- lb_for_merge[order(lb_for_merge$USUBJID,
  if ("LBDTC" %in% names(lb_for_merge)) lb_for_merge$LBDTC else NULL,
  if ("LBTESTCD" %in% names(lb_for_merge)) lb_for_merge$LBTESTCD else NULL,
  na.last = FALSE), , drop = FALSE]

# left join LB to ADSL (keep only LB records)
adlb_draft0 <- dplyr::left_join(lb_for_merge, adsl_s, by = "USUBJID")
stopifnot("TRTSDT" %in% names(adlb_draft0) || "TRTSDT" %in% names(adsl_s))
# if b filter: keep only rows that were in LB (all rows of left_join are LB rows)
# (left_join preserves all LB rows)

# Build core analysis variables
# Helper for LBDTC substr
lbdtc_vec <- if ("LBDTC" %in% names(adlb_draft0)) adlb_draft0$LBDTC else rep(NA_character_, nrow(adlb_draft0))
lbtestcd_vec <- if ("LBTESTCD" %in% names(adlb_draft0)) adlb_draft0$LBTESTCD else rep(NA_character_, nrow(adlb_draft0))
lbtest_vec <- if ("LBTEST" %in% names(adlb_draft0)) adlb_draft0$LBTEST else rep(NA_character_, nrow(adlb_draft0))
lbstresn_vec <- if ("LBSTRESN" %in% names(adlb_draft0)) suppressWarnings(as.numeric(adlb_draft0$LBSTRESN)) else rep(NA_real_, nrow(adlb_draft0))
visit_vec <- if ("VISIT" %in% names(adlb_draft0)) as.character(adlb_draft0$VISIT) else rep(NA_character_, nrow(adlb_draft0))
visitnum_vec <- if ("VISITNUM" %in% names(adlb_draft0)) suppressWarnings(as.numeric(adlb_draft0$VISITNUM)) else rep(NA_real_, nrow(adlb_draft0))
lbstresu_vec <- if ("LBSTRESU" %in% names(adlb_draft0)) as.character(adlb_draft0$LBSTRESU) else rep(NA_character_, nrow(adlb_draft0))
lbseq_vec <- if ("LBSEQ" %in% names(adlb_draft0)) suppressWarnings(as.numeric(adlb_draft0$LBSEQ)) else rep(NA_real_, nrow(adlb_draft0))
stud_vec <- if ("STUDYID" %in% names(adlb_draft0)) as.character(adlb_draft0$STUDYID) else rep(NA_character_, nrow(adlb_draft0))
trt01a_vec <- if ("TRT01A" %in% names(adlb_draft0)) as.character(adlb_draft0$TRT01A) else rep(NA_character_, nrow(adlb_draft0))
trtsdt_vec <- if ("TRTSDT" %in% names(adlb_draft0)) to_date(adlb_draft0$TRTSDT) else rep(as.Date(NA), nrow(adlb_draft0))
trtedt_vec <- if ("TRTEDT" %in% names(adlb_draft0)) to_date(adlb_draft0$TRTEDT) else rep(as.Date(NA), nrow(adlb_draft0))
usubjid_vec <- as.character(adlb_draft0$USUBJID)

# ASTDT: if length(LBDTC) >= 10 then input(substr(LBDTC,1,10), yymmdd10.)
astdt_vec <- rep(as.Date(NA), length(lbdtc_vec))
for (i in seq_along(lbdtc_vec)) {
  v <- lbdtc_vec[i]
  if (is.na(v)) { astdt_vec[i] <- as.Date(NA); next }
  if (nchar(v) >= 10) {
    sub10 <- substr(v, 1, 10)
    # ONE strict format YYYY-MM-DD (yymmdd10.)
    d <- suppressWarnings(as.Date(sub10, format = "%Y-%m-%d"))
    astdt_vec[i] <- d
  } else {
    astdt_vec[i] <- as.Date(NA)
  }
}
# class
astdt_vec <- as.Date(astdt_vec, origin = "1970-01-01")

# ASTDY
astdy_vec <- rep(NA_real_, length(astdt_vec))
for (i in seq_along(astdt_vec)) {
  a <- astdt_vec[i]; t <- trtsdt_vec[i]
  if (!is.na(a) && !is.na(t)) {
    if (a >= t) astdy_vec[i] <- as.integer(a - t) + 1
    else astdy_vec[i] <- as.integer(a - t)
  }
}

adlb_draft <- data.frame(
  STUDYID = stud_vec,
  USUBJID = usubjid_vec,
  TRT01A = trt01a_vec,
  TRTSDT = trtsdt_vec,
  TRTEDT = trtedt_vec,
  PARAMCD = lbtestcd_vec,
  PARAM = lbtest_vec,
  AVAL = lbstresn_vec,
  AVISIT = visit_vec,
  AVISITN = visitnum_vec,
  ASTDT = astdt_vec,
  ASTDY = astdy_vec,
  LBSTRESU = lbstresu_vec,
  SRCDOM = rep("LB", length(usubjid_vec)),
  SRCVAR = rep("LBSEQ", length(usubjid_vec)),
  SRCSEQ = lbseq_vec,
  stringsAsFactors = FALSE
)
# Ensure Date class
adlb_draft$TRTSDT <- as.Date(adlb_draft$TRTSDT, origin = "1970-01-01")
adlb_draft$TRTEDT <- as.Date(adlb_draft$TRTEDT, origin = "1970-01-01")
adlb_draft$ASTDT <- as.Date(adlb_draft$ASTDT, origin = "1970-01-01")

# 2. IDENTIFY BASELINE RECORD
# where ASTDT <= TRTSDT and not missing AVAL ; SAS missing <= nonmissing is TRUE
# filter = !is.na(AVAL) & (is.na(ASTDT) | (!is.na(ASTDT) & !is.na(TRTSDT) & ASTDT<=TRTSDT))
base_cand <- adlb_draft %>%
  dplyr::filter(!is.na(AVAL) & (is.na(ASTDT) | (!is.na(ASTDT) & !is.na(TRTSDT) & ASTDT <= TRTSDT)))

# sort by USUBJID PARAMCD ASTDT AVISITN SRCSEQ with missing first
# implement NA-first ordering
base_cand <- base_cand %>%
  dplyr::arrange(USUBJID, PARAMCD,
    dplyr::desc(is.na(ASTDT)), ASTDT,
    dplyr::desc(is.na(AVISITN)), AVISITN,
    dplyr::desc(is.na(SRCSEQ)), SRCSEQ)

# last.PARAMCD per USUBJID PARAMCD
if (nrow(base_cand) > 0) {
  base_flags <- base_cand %>%
    dplyr::group_by(USUBJID, PARAMCD) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(USUBJID = USUBJID, PARAMCD = PARAMCD,
      BASE = AVAL, BASE_DT = ASTDT, BASE_SEQ = SRCSEQ)
} else {
  base_flags <- data.frame(USUBJID=character(0), PARAMCD=character(0),
    BASE=numeric(0), BASE_DT=as.Date(character(0)), BASE_SEQ=numeric(0))
}
base_flags$BASE_DT <- as.Date(base_flags$BASE_DT, origin="1970-01-01")

# 3. MERGE BASELINE BACK
# sort draft by USUBJID PARAMCD
adlb_draft <- adlb_draft[order(adlb_draft$USUBJID, adlb_draft$PARAMCD, na.last = FALSE), , drop=FALSE]
base_flags_s <- base_flags[order(base_flags$USUBJID, base_flags$PARAMCD, na.last=FALSE), , drop=FALSE]

adlb <- dplyr::left_join(adlb_draft, base_flags_s, by = c("USUBJID","PARAMCD"))
stopifnot("BASE" %in% names(adlb))
stopifnot("SRCSEQ" %in% names(adlb))

# Flag exact source record
adlb$ABLFL <- ifelse(
  (!is.na(adlb$SRCSEQ) & !is.na(adlb$BASE_SEQ) & adlb$SRCSEQ == adlb$BASE_SEQ) |
  (is.na(adlb$SRCSEQ) & is.na(adlb$BASE_SEQ) & !is.na(adlb$BASE)),
  "Y", "")

# Change from baseline
adlb$CHG <- ifelse(!is.na(adlb$AVAL) & !is.na(adlb$BASE), adlb$AVAL - adlb$BASE, NA_real_)

# LBNRIND
adlb$LBNRIND <- NA_character_
for (i in seq_len(nrow(adlb))) {
  av <- adlb$AVAL[i]
  if (is.na(av)) { adlb$LBNRIND[i] <- NA_character_; next }
  pc <- adlb$PARAMCD[i]
  if (is.na(pc)) { adlb$LBNRIND[i] <- "UNKNOWN"; next }
  calc_val <- av
  u <- toupper(trimws(ifelse(is.na(adlb$LBSTRESU[i]), "", adlb$LBSTRESU[i])))
  if (pc == "GLUC") {
    if (!is.na(u) && u == "MG/DL") calc_val <- av * 0.0555
    if (calc_val < 3.9) adlb$LBNRIND[i] <- "LOW"
    else if (calc_val > 5.6) adlb$LBNRIND[i] <- "HIGH"
    else adlb$LBNRIND[i] <- "NORMAL"
  } else if (pc == "HGB") {
    if (!is.na(u) && u == "G/DL") calc_val <- av * 10
    if (calc_val < 120) adlb$LBNRIND[i] <- "LOW"
    else if (calc_val > 175) adlb$LBNRIND[i] <- "HIGH"
    else adlb$LBNRIND[i] <- "NORMAL"
  } else {
    adlb$LBNRIND[i] <- "UNKNOWN"
  }
}

# drop BASE_DT BASE_SEQ
adlb$BASE_DT <- NULL
adlb$BASE_SEQ <- NULL

# Re-sort for final presentation by USUBJID PARAMCD ASTDT (missing first)
adlb <- adlb %>%
  dplyr::arrange(USUBJID, PARAMCD,
    dplyr::desc(is.na(ASTDT)), ASTDT)

# Retain order: STUDYID USUBJID PARAMCD PARAM then rest
final_order <- c("STUDYID","USUBJID","PARAMCD","PARAM","TRT01A","TRTSDT","TRTEDT",
  "AVAL","AVISIT","AVISITN","ASTDT","ASTDY","LBSTRESU","SRCDOM","SRCVAR","SRCSEQ",
  "BASE","ABLFL","CHG","LBNRIND")
# keep only existing, add any extra at end (should be none)
extra <- setdiff(names(adlb), final_order)
final_order <- c(intersect(final_order, names(adlb)), extra)
adlb <- adlb[, final_order, drop = FALSE]

# Ensure types
adlb$ASTDT <- as.Date(adlb$ASTDT, origin="1970-01-01")
adlb$TRTSDT <- as.Date(adlb$TRTSDT, origin="1970-01-01")
adlb$TRTEDT <- as.Date(adlb$TRTEDT, origin="1970-01-01")

cat("ADLB rows:", nrow(adlb), "\n")

# Write dataset CSV with ISO dates
adlb_out <- adlb
write.csv(adlb_out, file.path(out, "adlb.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT : PROC PRINT obs=15 vars
audit_vars <- c("USUBJID","PARAMCD","AVISIT","ASTDT","AVAL","LBSTRESU","BASE","CHG","ABLFL","LBNRIND")
audit_df <- utils::head(adlb, 15)
# ensure all vars exist
for (v in audit_vars) if (!v %in% names(audit_df)) audit_df[[v]] <- NA
audit_df <- audit_df[, audit_vars, drop=FALSE]

# CSV of table body (ISO dates)
audit_csv <- audit_df
# keep ASTDT as ISO (Date already)
write.csv(audit_csv, file.path(out, "P005.csv"), row.names = FALSE, na = "")

# Text rendering with DATE9. for ASTDT
title_txt <- "ADLB Audit - Laboratory Analysis with Clinical Indicators"

# Build text lines mimicking PROC PRINT
# Header
hdr <- sprintf("%-5s %-12s %-8s %-12s %-11s %-6s %-10s %-8s %-8s %-6s %-8s",
  "Obs","USUBJID","PARAMCD","AVISIT","ASTDT","AVAL","LBSTRESU","BASE","CHG","ABLFL","LBNRIND")
sep <- paste(rep("-", nchar(hdr)), collapse="")
lines <- c(title_txt, "", hdr, sep)
fmt_num <- function(x) {
  sapply(x, function(v) {
    if (is.na(v) || (is.character(v) && trimws(v)=="")) return("")
    if (is.numeric(v)) {
      # SAS BEST12. default: strip trailing zeros? use as.character
      return(as.character(v))
    } else return(as.character(v))
  }, USE.NAMES=FALSE)
}
for (i in seq_len(nrow(audit_df))) {
  r <- audit_df[i, , drop=FALSE]
  astdt9 <- fmt_date9(r$ASTDT)
  aval_c <- fmt_num(r$AVAL)
  base_c <- fmt_num(r$BASE)
  chg_c <- fmt_num(r$CHG)
  usub <- ifelse(is.na(r$USUBJID), "", as.character(r$USUBJID))
  pcd <- ifelse(is.na(r$PARAMCD), "", as.character(r$PARAMCD))
  avis <- ifelse(is.na(r$AVISIT), "", as.character(r$AVISIT))
  unit <- ifelse(is.na(r$LBSTRESU), "", as.character(r$LBSTRESU))
  abl <- ifelse(is.na(r$ABLFL), "", as.character(r$ABLFL))
  ind <- ifelse(is.na(r$LBNRIND), "", as.character(r$LBNRIND))
  line <- sprintf("%-5d %-12s %-8s %-12s %-11s %-6s %-10s %-8s %-8s %-6s %-8s",
    i, usub, pcd, avis, astdt9, aval_c, unit, base_c, chg_c, abl, ind)
  lines <- c(lines, line)
}
lines <- c(lines, "", paste0("N = ", nrow(audit_df), " (first 15 observations of ADLB, total ", nrow(adlb), " rows)"))

writeLines(lines, file.path(out, "P005.txt"))
# also write .lst counterpart
writeLines(lines, file.path(out, "P005.lst"))

cat("Done. Wrote adlb.csv, P005.csv, P005.txt, P005.lst\n")
```
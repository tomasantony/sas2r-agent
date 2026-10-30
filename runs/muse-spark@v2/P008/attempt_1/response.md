```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
adam_dir <- file.path(project_path, "data/adam")
sdtm_dir <- file.path(project_path, "data/sdtm")

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

format_date9 <- function(x) {
  sapply(x, function(d) {
    if (is.na(d)) return("")
    d <- as.Date(d, origin = "1970-01-01")
    paste0(format(d, "%d"), toupper(format(d, "%b")), format(d, "%Y"))
  }, USE.NAMES = FALSE)
}

read_dataset <- function(dir, name) {
  files <- list.files(dir, pattern = paste0("^", name, "\\."),
                      ignore.case = TRUE, full.names = TRUE)
  if (length(files) == 0) stop(paste0("Dataset not found: ", dir, "/", name))
  f <- files[1]
  lf <- tolower(f)
  if (grepl("\\.sas7bdat$", lf)) {
    df <- haven::read_sas(f)
  } else if (grepl("\\.xpt$", lf)) {
    df <- haven::read_xpt(f)
  } else if (grepl("\\.csv$", lf)) {
    df <- readr::read_csv(f, show_col_types = FALSE)
    df <- as.data.frame(df)
  } else if (grepl("\\.xpt$", lf)) {
    df <- haven::read_xpt(f)
  } else {
    stop(paste0("Unknown extension: ", f))
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  df
}

to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x) || is.factor(x)) {
    x <- as.character(x)
    x[x == ""] <- NA_character_
    return(as.Date(x, format = "%Y-%m-%d"))
  }
  return(as.Date(x, origin = "1960-01-01"))
}

to_char <- function(x) {
  if (is.null(x)) return(rep(NA_character_, 0))
  if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) return(x)
  if (is.numeric(x) || is.integer(x)) return(as.character(x))
  as.character(x)
}

to_num <- function(x) {
  if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
  if (is.factor(x)) x <- suppressWarnings(as.numeric(as.character(x)))
  suppressWarnings(as.numeric(x))
}

adsl_raw <- read_dataset(adam_dir, "adsl")
vs_raw <- read_dataset(sdtm_dir, "vs")

stopifnot(all(c("USUBJID") %in% names(adsl_raw)))
stopifnot(all(c("USUBJID") %in% names(vs_raw)))

# Select needed columns, drop/rename overlaps except keys
# ADSL side
adsl <- data.frame(
  USUBJID = to_char(adsl_raw$USUBJID),
  STUDYID = if ("STUDYID" %in% names(adsl_raw)) to_char(adsl_raw$STUDYID) else NA_character_,
  TRT01A  = if ("TRT01A" %in% names(adsl_raw)) to_char(adsl_raw$TRT01A) else NA_character_,
  TRTSDT  = if ("TRTSDT" %in% names(adsl_raw)) to_date(adsl_raw$TRTSDT) else as.Date(NA),
  TRTEDT  = if ("TRTEDT" %in% names(adsl_raw)) to_date(adsl_raw$TRTEDT) else as.Date(NA),
  stringsAsFactors = FALSE
)
# VS side
vs <- data.frame(
  USUBJID  = to_char(vs_raw$USUBJID),
  VSDTC    = if ("VSDTC" %in% names(vs_raw)) to_char(vs_raw$VSDTC) else NA_character_,
  VSTESTCD = if ("VSTESTCD" %in% names(vs_raw)) to_char(vs_raw$VSTESTCD) else NA_character_,
  VSTEST   = if ("VSTEST" %in% names(vs_raw)) to_char(vs_raw$VSTEST) else NA_character_,
  VSSTRESN = if ("VSSTRESN" %in% names(vs_raw)) to_num(vs_raw$VSSTRESN) else NA_real_,
  VISIT    = if ("VISIT" %in% names(vs_raw)) to_char(vs_raw$VISIT) else NA_character_,
  VISITNUM = if ("VISITNUM" %in% names(vs_raw)) to_num(vs_raw$VISITNUM) else NA_real_,
  VSSEQ    = if ("VSSEQ" %in% names(vs_raw)) to_num(vs_raw$VSSEQ) else NA_real_,
  stringsAsFactors = FALSE
)

# SAS sorts: adsl by USUBJID ; vs by USUBJID VSDTC VSTESTCD (missing first, uppercase before lowercase)
adsl_sorted <- adsl[order(adsl$USUBJID, na.last = FALSE), , drop = FALSE]
vs_sorted <- vs[order(vs$USUBJID, vs$VSDTC, vs$VSTESTCD, na.last = FALSE), , drop = FALSE]

# 1. MERGE ADSL AND VS, keep only records that have vital signs (if b)
# left join vs with adsl
advs_draft <- dplyr::left_join(vs_sorted, adsl_sorted, by = "USUBJID")
stopifnot(all(c("VSTESTCD","VSTEST","VSSTRESN","VISIT","VISITNUM","VSSEQ","TRTSDT") %in% names(advs_draft)))

# Derive core analysis variables
n <- nrow(advs_draft)
PARAMCD <- advs_draft$VSTESTCD
PARAM   <- advs_draft$VSTEST
AVAL    <- advs_draft$VSSTRESN
AVISIT  <- advs_draft$VISIT
AVISITN <- advs_draft$VISITNUM
SRCDOM  <- rep("VS", n)
SRCVAR  <- rep("VSSEQ", n)
SRCSEQ  <- advs_draft$VSSEQ

VSDTC <- advs_draft$VSDTC
ASTDT <- rep(as.Date(NA), n)
idx_len <- !is.na(VSDTC) & nchar(VSDTC, keepNA = TRUE) >= 10
idx_len[is.na(idx_len)] <- FALSE
if (any(idx_len)) {
  sub10 <- substr(VSDTC[idx_len], 1, 10)
  ASTDT[idx_len] <- as.Date(sub10, format = "%Y-%m-%d")
}

TRTSDT <- advs_draft$TRTSDT
TRTEDT <- advs_draft$TRTEDT
ASTDY <- rep(NA_real_, n)
ok <- !is.na(ASTDT) & !is.na(TRTSDT)
ge <- ok & ASTDT >= TRTSDT
lt <- ok & ASTDT < TRTSDT
ASTDY[ge] <- as.integer(ASTDT[ge] - TRTSDT[ge]) + 1
ASTDY[lt] <- as.integer(ASTDT[lt] - TRTSDT[lt])

advs_draft2 <- data.frame(
  STUDYID = advs_draft$STUDYID,
  USUBJID = advs_draft$USUBJID,
  TRT01A  = advs_draft$TRT01A,
  TRTSDT  = TRTSDT,
  TRTEDT  = TRTEDT,
  PARAMCD = PARAMCD,
  PARAM   = PARAM,
  AVAL    = AVAL,
  AVISIT  = AVISIT,
  AVISITN = AVISITN,
  ASTDT   = ASTDT,
  ASTDY   = ASTDY,
  SRCDOM  = SRCDOM,
  SRCVAR  = SRCVAR,
  SRCSEQ  = SRCSEQ,
  stringsAsFactors = FALSE
)
# keep order as above; retain STUDYID USUBJID first already

# 2. IDENTIFY BASELINE RECORD
# where ASTDT <= TRTSDT and not missing AVAL, sort by USUBJID PARAMCD ASTDT AVISITN SRCSEQ
cand_idx <- !is.na(advs_draft2$ASTDT) & !is.na(advs_draft2$TRTSDT) &
            advs_draft2$ASTDT <= advs_draft2$TRTSDT & !is.na(advs_draft2$AVAL)
advs_base_candidates <- advs_draft2[cand_idx, , drop = FALSE]
advs_base_candidates <- advs_base_candidates[order(advs_base_candidates$USUBJID,
  advs_base_candidates$PARAMCD, advs_base_candidates$ASTDT,
  advs_base_candidates$AVISITN, advs_base_candidates$SRCSEQ, na.last = FALSE), , drop = FALSE]

# last.PARAMCD per USUBJID PARAMCD
if (nrow(advs_base_candidates) > 0) {
  # use dplyr slice_tail after arrange (already arranged)
  base_flags <- advs_base_candidates %>%
    dplyr::group_by(USUBJID, PARAMCD) %>%
    dplyr::arrange(ASTDT, AVISITN, SRCSEQ, .by_group = TRUE) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(USUBJID = USUBJID, PARAMCD = PARAMCD,
                     BASE = AVAL, BASE_DT = ASTDT, BASE_SEQ = SRCSEQ) %>%
    as.data.frame(stringsAsFactors = FALSE)
} else {
  base_flags <- data.frame(USUBJID=character(0), PARAMCD=character(0),
    BASE=numeric(0), BASE_DT=as.Date(character(0)), BASE_SEQ=numeric(0))
}

# 3. MERGE BASELINE BACK
# sort advs_draft by USUBJID PARAMCD
advs_draft_sorted <- advs_draft2[order(advs_draft2$USUBJID, advs_draft2$PARAMCD, na.last = FALSE), , drop = FALSE]
advs <- dplyr::left_join(advs_draft_sorted, base_flags, by = c("USUBJID","PARAMCD"))
stopifnot(all(c("SRCSEQ","BASE_SEQ","AVAL","BASE") %in% names(advs)))

# Flag exact source record
both_na <- is.na(advs$SRCSEQ) & is.na(advs$BASE_SEQ)
both_eq <- !is.na(advs$SRCSEQ) & !is.na(advs$BASE_SEQ) & advs$SRCSEQ == advs$BASE_SEQ
ABLFL <- ifelse(both_na | both_eq, "Y", "")
ABLFL[is.na(ABLFL)] <- ""

CHG <- rep(NA_real_, nrow(advs))
okchg <- !is.na(advs$AVAL) & !is.na(advs$BASE)
CHG[okchg] <- advs$AVAL[okchg] - advs$BASE[okchg]

advs$ABLFL <- ABLFL
advs$CHG <- CHG
# drop BASE_DT BASE_SEQ
advs$BASE_DT <- NULL
advs$BASE_SEQ <- NULL

# Reorder to SAS order: STUDYID USUBJID TRT01A TRTSDT TRTEDT PARAMCD PARAM AVAL AVISIT AVISITN ASTDT ASTDY SRCDOM SRCVAR SRCSEQ BASE ABLFL CHG
advs <- advs[, c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT","PARAMCD","PARAM",
                 "AVAL","AVISIT","AVISITN","ASTDT","ASTDY","SRCDOM","SRCVAR","SRCSEQ",
                 "BASE","ABLFL","CHG")]

# Re-sort for final presentation by USUBJID PARAMCD ASTDT
advs <- advs[order(advs$USUBJID, advs$PARAMCD, advs$ASTDT, na.last = FALSE), , drop = FALSE]

# Write dataset advs.csv with ISO dates
advs_out <- advs
write.csv(advs_out, file.path(out, "advs.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT: PROC PRINT obs=15 vars USUBJID PARAMCD AVISIT ASTDT TRTSDT AVAL BASE CHG ABLFL
print_vars <- c("USUBJID","PARAMCD","AVISIT","ASTDT","TRTSDT","AVAL","BASE","CHG","ABLFL")
audit <- advs[, print_vars, drop = FALSE]
audit15 <- utils::head(audit, 15)

# CSV of table body (ISO dates)
audit_csv <- audit15
write.csv(audit_csv, file.path(out, "P008.csv"), row.names = FALSE, na = "")

# Text rendering with DATE9. for dates, BEST-like for numerics
title_txt <- "ADVS Audit - Vital Signs Baseline and Change Analysis"
fmt_num <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    # SAS BEST12. default: strip trailing zeros, use plain representation
    format(v, scientific = FALSE, trim = TRUE, nsmall = 0)
  }, USE.NAMES = FALSE)
}
astdt_c <- format_date9(audit15$ASTDT)
trtsdt_c <- format_date9(audit15$TRTSDT)
aval_c <- fmt_num(audit15$AVAL)
base_c <- fmt_num(audit15$BASE)
chg_c <- fmt_num(audit15$CHG)

# Build fixed-width text similar to PROC PRINT
hdr <- sprintf("%-5s %-12s %-10s %-12s %-10s %-10s %-10s %-10s %-10s %-5s",
  "Obs","USUBJID","PARAMCD","AVISIT","ASTDT","TRTSDT","AVAL","BASE","CHG","ABLFL")
lines <- c(title_txt, "", hdr, paste0(strrep("-", 5), " ", strrep("-", 12), " ", strrep("-", 10), " ",
  strrep("-", 12), " ", strrep("-", 10), " ", strrep("-", 10), " ", strrep("-", 10), " ",
  strrep("-", 10), " ", strrep("-", 10), " ", strrep("-", 5)))
for (i in seq_len(nrow(audit15))) {
  usubjid <- ifelse(is.na(audit15$USUBJID[i]), "", audit15$USUBJID[i])
  paramcd <- ifelse(is.na(audit15$PARAMCD[i]), "", audit15$PARAMCD[i])
  avisit  <- ifelse(is.na(audit15$AVISIT[i]), "", audit15$AVISIT[i])
  ablfl   <- ifelse(is.na(audit15$ABLFL[i]), "", audit15$ABLFL[i])
  lines <- c(lines, sprintf("%-5d %-12s %-10s %-12s %-10s %-10s %-10s %-10s %-10s %-5s",
    i, usubjid, paramcd, avisit, astdt_c[i], trtsdt_c[i], aval_c[i], base_c[i], chg_c[i], ablfl))
}
writeLines(lines, file.path(out, "P008.txt"))
# also write .lst copy for completeness
writeLines(lines, file.path(out, "P008.lst"))
```
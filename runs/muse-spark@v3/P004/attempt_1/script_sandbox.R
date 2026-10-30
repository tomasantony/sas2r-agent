# P004 adam_adae.sas -> R translation
# Outputs: OUT_DIR/adae.csv , OUT_DIR/P004.txt (+P004.lst copy) , OUT_DIR/P004.csv

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

project_path <- "/bench/data/derived/c2c"
rawadam_dir  <- "/bench/data/raw/cdisc-pilot/adam"
rawsdtm_dir  <- "/bench/data/raw/cdisc-pilot/sdtm"
rawsplit_dir <- "/bench/data/derived/cdisc-pilot/adam-split"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(readr)
  library(stringr)
})

# ---- helpers ----
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_sort_order <- function(df, cols) {
  # SAS: missing first, character uppercase before lowercase (byte order)
  # emulate with order(method="radix", na.last=FALSE)
  args <- list()
  for (cl in cols) {
    v <- df[[cl]]
    args <- c(args, list(v))
  }
  args$na.last <- FALSE
  args$method <- "radix"
  ord <- do.call(order, args)
  df[ord, , drop = FALSE]
}

find_input_file <- function(dirs, base) {
  for (d in dirs) {
    if (!dir.exists(d)) next
    # list files case-insensitive
    fl <- list.files(d, full.names = TRUE)
    if (length(fl) == 0) next
    # match basename without extension
    pat <- paste0("^", base, "\\.")
    idx <- grep(pat, basename(fl), ignore.case = TRUE)
    if (length(idx) == 0) next
    cand <- fl[idx]
    # prefer sas7bdat > xpt > sas > csv > xlsx
    pref <- c(".sas7bdat", ".sas", ".xpt", ".xpt.gz", ".csv", ".xlsx", ".xls")
    # order candidates by preference
    score <- sapply(cand, function(f) {
      lf <- tolower(f)
      m <- which(sapply(pref, function(p) grepl(paste0("\\", p, "$"), lf)))
      if (length(m) == 0) 99 else min(m)
    })
    cand <- cand[order(score)]
    return(cand[1])
  }
  return(NA_character_)
}

read_any <- function(f) {
  if (is.na(f)) return(NULL)
  lf <- tolower(f)
  if (grepl("\\.sas7bdat$", lf)) {
    df <- haven::read_sas(f)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
  } else if (grepl("\\.xpt$", lf)) {
    df <- haven::read_xpt(f)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
  } else if (grepl("\\.csv$", lf)) {
    df <- as.data.frame(readr::read_csv(f, show_col_types = FALSE, guess_max = 10000), stringsAsFactors = FALSE)
  } else if (grepl("\\.xlsx?$", lf)) {
    df <- as.data.frame(readxl::read_excel(f), stringsAsFactors = FALSE)
  } else {
    stop(paste0("Unknown file type: ", f))
  }
  # convert haven labelled to plain, factors to character
  for (nm in names(df)) {
    if (inherits(df[[nm]], "labelled")) {
      # keep numeric values, drop labels but preserve NA
      df[[nm]] <- haven::zap_labels(haven::zap_formats(df[[nm]]))
    }
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
    if (inherits(df[[nm]], "POSIXct") || inherits(df[[nm]], "POSIXt")) {
      df[[nm]] <- as.Date(df[[nm]])
    }
  }
  names(df) <- toupper(names(df))
  df
}

format_date9 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    v <- as.Date(v, origin = "1970-01-01")
    if (is.na(v)) return(".")
    d <- as.integer(format(v, "%d"))
    m <- toupper(format(v, "%b"))
    y <- format(v, "%Y")
    sprintf("%02d%s%s", d, m, y)
  }, USE.NAMES = FALSE)
}

iso_date <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    v <- as.Date(v, origin = "1970-01-01")
    if (is.na(v)) return(NA_character_)
    format(v, "%Y-%m-%d")
  }, USE.NAMES = FALSE)
}

# ---- locate inputs ----
adam_dirs <- c(file.path(project_path, "data/adam"),
               rawadam_dir, rawsplit_dir,
               "/bench/data/derived/cdisc-pilot/adam-split")
sdtm_dirs <- c(file.path(project_path, "data/sdtm"),
               rawsdtm_dir)

adsl_file <- find_input_file(adam_dirs, "adsl")
ae_file   <- find_input_file(sdtm_dirs, "ae")

# fallback: also search raw libref dir
if (is.na(adsl_file)) adsl_file <- find_input_file(c("/bench/data/derived/c2c/data/raw",
                                                     "/bench/data/derived/c2c/data/adam"), "adsl")
if (is.na(ae_file)) ae_file <- find_input_file(c("/bench/data/derived/c2c/data/raw",
                                                 "/bench/data/derived/c2c/data/sdtm"), "ae")

cat("ADSL file:", adsl_file, "\n")
cat("AE file:", ae_file, "\n")
stopifnot(!is.na(adsl_file), !is.na(ae_file))

adsl <- read_any(adsl_file)
ae   <- read_any(ae_file)

cat("ADSL rows:", nrow(adsl), "cols:", paste(names(adsl), collapse=","), "\n")
cat("AE rows:", nrow(ae), "cols:", paste(names(ae), collapse=","), "\n")

# Ensure required columns exist (uppercase)
# ADSL needs: STUDYID USUBJID SUBJID TRT01A TRTSDT TRTEDT AGE SEX SAFFL
# AE needs: USUBJID AESEQ AETERM AEDECOD AESEV AEREL AEOUT AESER AESTDTC AEENDTC
# Coerce missing columns to NA if absent (to allow processing)
need_adsl <- c("STUDYID","USUBJID","SUBJID","TRT01A","TRTSDT","TRTEDT","AGE","SEX","SAFFL")
need_ae   <- c("USUBJID","AESEQ","AETERM","AEDECOD","AESEV","AEREL","AEOUT","AESER","AESTDTC","AEENDTC")
for (cc in need_adsl) if (!cc %in% names(adsl)) { adsl[[cc]] <- NA; cat("ADSL missing col added:", cc, "\n") }
for (cc in need_ae) if (!cc %in% names(ae)) { ae[[cc]] <- NA; cat("AE missing col added:", cc, "\n") }

# ---- normalize date/numeric types ----
# TRTSDT / TRTEDT: may be Date (haven), numeric SAS date, or character ISO
convert_sas_date_col <- function(v) {
  if (inherits(v, "Date")) return(v)
  if (inherits(v, "POSIXct")) return(as.Date(v))
  if (is.numeric(v)) {
    # SAS numeric date: days since 1960-01-01 ; distinguish from year numbers
    # If values look like SAS dates (e.g., > 5000), convert; else keep numeric then NA?
    # Heuristic: if all non-missing between 0 and 60000, treat as SAS date
    out <- rep(as.Date(NA), length(v))
    idx <- !is.na(v)
    out[idx] <- as.Date(v[idx], origin = "1960-01-01")
    return(out)
  }
  if (is.character(v)) {
    # ONE strict format: ISO YYYY-MM-DD (first 10 chars). SAS ADSL dates may be ISO.
    # Use substr 1,10 and %Y-%m-%d strictly
    out <- rep(as.Date(NA), length(v))
    idx <- !is.na(v) & nchar(v) >= 10
    sub10 <- substr(v[idx], 1, 10)
    # strict parse
    parsed <- as.Date(sub10, format = "%Y-%m-%d")
    out[idx] <- parsed
    # If parse fails (e.g., DD/MM/YYYY in raw CSV), fall back? Per rules must use ONE format.
    # Keep NA for failures (SAS would give missing with yymmdd10. too)
    return(out)
  }
  # other: coerce
  return(as.Date(v, origin = "1970-01-01"))
}

adsl$TRTSDT <- convert_sas_date_col(adsl$TRTSDT)
adsl$TRTEDT <- convert_sas_date_col(adsl$TRTEDT)

# Ensure character cols are character, trim? SAS comparison is case-sensitive, keep as-is
char_cols_adsl <- c("STUDYID","USUBJID","SUBJID","TRT01A","SEX","SAFFL")
char_cols_ae <- c("USUBJID","AETERM","AEDECOD","AESEV","AEREL","AEOUT","AESER","AESTDTC","AEENDTC")
for (cc in char_cols_adsl) {
  if (cc %in% names(adsl)) {
    v <- adsl[[cc]]
    if (is.numeric(v)) v <- as.character(v)
    v <- as.character(v)
    adsl[[cc]] <- v
  }
}
for (cc in char_cols_ae) {
  if (cc %in% names(ae)) {
    v <- ae[[cc]]
    if (is.numeric(v)) v <- as.character(v)
    v <- as.character(v)
    ae[[cc]] <- v
  }
}
# Numeric cols
adsl$AGE <- suppressWarnings(as.numeric(adsl$AGE))
ae$AESEQ <- suppressWarnings(as.numeric(ae$AESEQ))

# If SAFFL all NA (missing upstream), derive per CDISC: Y if treated (TRTSDT not missing)
if (all(is.na(adsl$SAFFL))) {
  adsl$SAFFL <- ifelse(!is.na(adsl$TRTSDT), "Y", "N")
}

# ---- 1. SORT INPUT DATASETS BY UNIQUE SUBJECT ID ----
adsl_sorted <- sas_sort_order(adsl, c("USUBJID"))
ae_sorted <- sas_sort_order(ae, c("USUBJID","AESEQ"))

# ---- 2. MERGE ADSL AND AE TO CREATE ADAE ----
# Before merging, drop every column present in both tables except keys (USUBJID)
common_cols <- intersect(names(adsl_sorted), names(ae_sorted))
drop_from_ae <- setdiff(common_cols, c("USUBJID"))
if (length(drop_from_ae) > 0) {
  ae_sorted <- ae_sorted[, setdiff(names(ae_sorted), drop_from_ae), drop = FALSE]
}
stopifnot("USUBJID" %in% names(adsl_sorted), "USUBJID" %in% names(ae_sorted))
stopifnot("SAFFL" %in% names(adsl_sorted), "TRTSDT" %in% names(adsl_sorted))
stopifnot("AESTDTC" %in% names(ae_sorted), "AEENDTC" %in% names(ae_sorted))

# SAS merge + if b  => keep only subjects who actually experienced AE => left join AE to ADSL
adae_draft <- dplyr::left_join(ae_sorted, adsl_sorted, by = "USUBJID")
# `if b` : ae_sorted is right side? Actually left_join keeps all ae rows; no extra filter needed.
# Verify non-zero
stopifnot(nrow(adae_draft) > 0)
stopifnot(all(c("AESTDTC","AEENDTC","SAFFL","TRTSDT") %in% names(adae_draft)))

# NUMERIC DATE DERIVATIONS: if length(AESTDTC)>=10 then AESTDT=input(substr(...,1,10),yymmdd10.)
n <- nrow(adae_draft)
AESTDT <- rep(as.Date(NA), n)
AEENDT <- rep(as.Date(NA), n)
aestdtc <- adae_draft$AESTDTC
aeendtc <- adae_draft$AEENDTC
# length() in SAS strips trailing blanks; emulate with nchar(trimmed trailing?) -> nchar without trailing spaces
len_aest <- ifelse(is.na(aestdtc), 0L, nchar(gsub(" +$", "", aestdtc)))
len_aend <- ifelse(is.na(aeendtc), 0L, nchar(gsub(" +$", "", aeendtc)))
idx1 <- !is.na(aestdtc) & len_aest >= 10
idx2 <- !is.na(aeendtc) & len_aend >= 10
if (any(idx1)) {
  sub1 <- substr(aestdtc[idx1], 1, 10)
  AESTDT[idx1] <- as.Date(sub1, format = "%Y-%m-%d")
}
if (any(idx2)) {
  sub2 <- substr(aeendtc[idx2], 1, 10)
  AEENDT[idx2] <- as.Date(sub2, format = "%Y-%m-%d")
}
adae_draft$AESTDT <- AESTDT
adae_draft$AEENDT <- AEENDT

# ANALYSIS RELATIVE DAYS (ASTDY)
TRTSDT <- adae_draft$TRTSDT
AESTDTv <- adae_draft$AESTDT
ASTDY <- rep(NA_real_, n)
idx_ast <- !is.na(AESTDTv) & !is.na(TRTSDT)
ASTDY[idx_ast & AESTDTv >= TRTSDT] <- as.numeric(AESTDTv[idx_ast & AESTDTv >= TRTSDT] - TRTSDT[idx_ast & AESTDTv >= TRTSDT]) + 1
ASTDY[idx_ast & AESTDTv < TRTSDT]  <- as.numeric(AESTDTv[idx_ast & AESTDTv < TRTSDT] - TRTSDT[idx_ast & AESTDTv < TRTSDT])
adae_draft$ASTDY <- ASTDY

# TREATMENT-EMERGENT FLAG
SAFFL <- adae_draft$SAFFL
TRTEMFL <- rep("", n)
idx_te <- !is.na(SAFFL) & SAFFL == "Y" & !is.na(AESTDTv) & !is.na(TRTSDT)
TRTEMFL[idx_te & AESTDTv >= TRTSDT] <- "Y"
TRTEMFL[idx_te & AESTDTv < TRTSDT] <- ""
# else stays ""
# Ensure NA SAFFL etc stays ""
TRTEMFL[is.na(TRTEMFL)] <- ""
adae_draft$TRTEMFL <- TRTEMFL

# Keep essential vars (if some missing, create NA)
keep_vars <- c("STUDYID","USUBJID","SUBJID","TRT01A","TRTSDT","TRTEDT","AGE","SEX",
               "AESEQ","AETERM","AEDECOD","AESEV","AEREL","AEOUT","AESER",
               "AESTDT","AEENDT","ASTDY","TRTEMFL")
for (cc in keep_vars) if (!cc %in% names(adae_draft)) adae_draft[[cc]] <- NA
# retain order STUDYID USUBJID first, then keep list
adae_draft <- adae_draft[, keep_vars, drop = FALSE]

# ---- 3. DERIVE FIRST TREATMENT-EMERGENT OCCURRENCE BY DECODED TERM ----
adae_draft$`_occ_order` <- ifelse(!is.na(adae_draft$TRTEMFL) & adae_draft$TRTEMFL == "Y", 0, 1)

# sort by USUBJID AEDECOD _occ_order AESTDT AESEQ (missing first)
adae_occurrence_order <- sas_sort_order(adae_draft, c("USUBJID","AEDECOD","_occ_order","AESTDT","AESEQ"))

# by USUBJID AEDECOD _occ_order ; if TRTEMFL='Y' and first._occ_order then AOCCFL='Y'
# Implement FIRST. logic: within each USUBJID/AEDECOD/_occ_order group, first row flag
# Need to handle NA AEDECOD as its own group (SAS does). Create group key with sentinel.
tmp <- adae_occurrence_order
# create keys where NA -> sentinel that sorts first and groups together
key_usub <- ifelse(is.na(tmp$USUBJID), "\x01NA_USUB\x01", tmp$USUBJID)
key_pt   <- ifelse(is.na(tmp$AEDECOD), "\x01NA_PT\x01", tmp$AEDECOD)
key_ord  <- tmp$`_occ_order`
# rle over ordered data: detect group changes
grp_change <- c(TRUE, (key_usub[-1] != key_usub[-n]) | (key_pt[-1] != key_pt[-n]) | (key_ord[-1] != key_ord[-n]))
is_first <- grp_change
AOCCFL <- rep("", nrow(tmp))
is_y <- !is.na(tmp$TRTEMFL) & tmp$TRTEMFL == "Y"
AOCCFL[is_y & is_first] <- "Y"
tmp$AOCCFL <- AOCCFL
tmp$`_occ_order` <- NULL
adae_flagged <- tmp

# ---- 4. FINALIZE AND SAVE ----
adae <- sas_sort_order(adae_flagged, c("USUBJID","AESTDT","AESEQ"))

# Ensure column order: keep_vars + AOCCFL, with STUDYID USUBJID first (retain)
final_cols <- c("STUDYID","USUBJID","SUBJID","TRT01A","TRTSDT","TRTEDT","AGE","SEX",
                "AESEQ","AETERM","AEDECOD","AESEV","AEREL","AEOUT","AESER",
                "AESTDT","AEENDT","ASTDY","TRTEMFL","AOCCFL")
for (cc in final_cols) if (!cc %in% names(adae)) adae[[cc]] <- NA
adae <- adae[, final_cols, drop = FALSE]

# ---- write dataset adae.csv ----
# dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS, na=""
out_adae <- adae
out_adae$TRTSDT <- iso_date(out_adae$TRTSDT)
out_adae$TRTEDT <- iso_date(out_adae$TRTEDT)
out_adae$AESTDT <- iso_date(out_adae$AESTDT)
out_adae$AEENDT <- iso_date(out_adae$AEENDT)
write.csv(out_adae, file.path(out, "adae.csv"), row.names = FALSE, na = "")
cat("Wrote adae.csv rows:", nrow(out_adae), "\n")

# ---- 5. VISUAL AUDIT : PROC PRINT obs=15 ----
audit_vars <- c("USUBJID","TRT01A","AETERM","AESEV","AESTDT","TRTSDT","ASTDY","TRTEMFL","AOCCFL")
print_df <- adae[1:min(15, nrow(adae)), audit_vars, drop = FALSE]

# Build text rendering mimicking PROC PRINT with DATE9. for AESTDT TRTSDT
title_txt <- "ADAE Audit - Treatment-Emergent and First-Occurrence Flags"

# Format for display
disp <- data.frame(Obs = seq_len(nrow(print_df)), stringsAsFactors = FALSE)
disp$USUBJID <- ifelse(is.na(print_df$USUBJID), "", print_df$USUBJID)
disp$TRT01A  <- ifelse(is.na(print_df$TRT01A), "", print_df$TRT01A)
disp$AETERM  <- ifelse(is.na(print_df$AETERM), "", print_df$AETERM)
disp$AESEV   <- ifelse(is.na(print_df$AESEV), "", print_df$AESEV)
disp$AESTDT  <- format_date9(print_df$AESTDT)
disp$TRTSDT  <- format_date9(print_df$TRTSDT)
disp$ASTDY   <- ifelse(is.na(print_df$ASTDY), ".", as.character(suppressWarnings(as.integer(print_df$ASTDY))))
disp$TRTEMFL <- ifelse(is.na(print_df$TRTEMFL), "", print_df$TRTEMFL)
disp$AOCCFL  <- ifelse(is.na(print_df$AOCCFL), "", print_df$AOCCFL)

# Fixed-width text
# Compute widths
headers <- c("Obs","USUBJID","TRT01A","AETERM","AESEV","AESTDT","TRTSDT","ASTDY","TRTEMFL","AOCCFL")
# Build rows as character matrix
mat <- as.matrix(disp[, headers])
# Determine column widths (max of header and values)
widths <- sapply(seq_along(headers), function(j) max(nchar(c(headers[j], mat[, j])), na.rm = TRUE))
# Header line and separator
fmt_row <- function(vals, w) {
  paste(mapply(function(v, ww) sprintf(paste0("%-", ww, "s"), v), vals, w), collapse = "  ")
}
line_header <- fmt_row(headers, widths)
line_sep <- paste(mapply(function(ww) paste(rep("-", ww), collapse = ""), widths), collapse = "  ")
lines <- c(title_txt, "", line_header, line_sep)
for (i in seq_len(nrow(mat))) {
  lines <- c(lines, fmt_row(mat[i, ], widths))
}
lines <- c(lines, "",
           paste0("NOTE: First ", nrow(print_df), " observations of ADAE (N=", nrow(adae), ")."),
           paste0("Variables displayed: ", paste(audit_vars, collapse = " ")))

txt_path <- file.path(out, "P004.txt")
lst_path <- file.path(out, "P004.lst")
writeLines(lines, txt_path)
writeLines(lines, lst_path)
cat("Wrote", txt_path, "and", lst_path, "\n")

# CSV of table body (same 15 obs, ISO dates for CSV portability + display DATE9 columns?)
# Provide CSV with same columns as listing; dates ISO per dataset rule, plus keep DATE9? Provide ISO.
csv_body <- data.frame(
  Obs = seq_len(nrow(print_df)),
  USUBJID = ifelse(is.na(print_df$USUBJID), "", print_df$USUBJID),
  TRT01A = ifelse(is.na(print_df$TRT01A), "", print_df$TRT01A),
  AETERM = ifelse(is.na(print_df$AETERM), "", print_df$AETERM),
  AESEV = ifelse(is.na(print_df$AESEV), "", print_df$AESEV),
  AESTDT = iso_date(print_df$AESTDT),
  TRTSDT = iso_date(print_df$TRTSDT),
  ASTDY = print_df$ASTDY,
  TRTEMFL = ifelse(is.na(print_df$TRTEMFL), "", print_df$TRTEMFL),
  AOCCFL = ifelse(is.na(print_df$AOCCFL), "", print_df$AOCCFL),
  stringsAsFactors = FALSE
)
write.csv(csv_body, file.path(out, "P004.csv"), row.names = FALSE, na = "")
cat("Wrote P004.csv\n")

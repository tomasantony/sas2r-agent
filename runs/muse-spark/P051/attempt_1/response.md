```r
out <- Sys.getenv("OUT_DIR")
if (!nzchar(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressMessages(library(haven))
suppressMessages(library(dplyr))

# SAS round half away from zero (helper, SAS default)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-10)
  posneg * z / 10^digits
}

# SAS COMPBL: compress multiple blanks to single blank
compbl <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  gsub(" {2,}", " ", x, perl = TRUE)
}

# SAS substr with length truncation (1-indexed)
sas_substr <- function(x, len) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  substr(x, 1, len)
}

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
adcm_path <- file.path(rawadam, "adcm.xpt")
message("Reading ", adcm_path)

adcm_raw <- haven::read_xpt(adcm_path)
adcm_raw <- as.data.frame(adcm_raw, stringsAsFactors = FALSE)

# zap haven labels/formats but keep values
for (nm in names(adcm_raw)) {
  v <- adcm_raw[[nm]]
  if (inherits(v, "haven_labelled") || inherits(v, "haven_labelled_spss")) {
    v <- haven::zap_labels(v)
    v <- haven::zap_formats(v)
    v <- haven::zap_label(v)
    adcm_raw[[nm]] <- as.vector(v)
  }
}

# uppercase name map for case-insensitive access
unames <- toupper(names(adcm_raw))
getcol <- function(want) {
  idx <- match(toupper(want), unames)
  if (is.na(idx)) return(NULL)
  names(adcm_raw)[idx]
}

cn_SAFFL   <- getcol("SAFFL")
cn_TRTA    <- getcol("TRTA")
cn_STUDYID <- getcol("STUDYID")
cn_USUBJID <- getcol("USUBJID")
cn_CMSTDTC <- getcol("CMSTDTC")
cn_CMENDTC <- getcol("CMENDTC")
cn_CMDECOD <- getcol("CMDECOD")
cn_CMCLAS  <- getcol("CMCLAS")
cn_CMTRT   <- getcol("CMTRT")
cn_CMINDC  <- getcol("CMINDC")
cn_CMDOSE  <- getcol("CMDOSE")
cn_CMDOSU  <- getcol("CMDOSU")
cn_CMUNIT  <- getcol("CMUNIT")
cn_ADURN   <- getcol("ADURN")
cn_AENRF   <- getcol("AENRF")

# WHERE SAFFL='Y'  (SAS case-sensitive, missing excluded)
df <- adcm_raw
if (!is.null(cn_SAFFL)) {
  saffl <- df[[cn_SAFFL]]
  saffl_chr <- as.character(saffl)
  keep <- !is.na(saffl_chr) & saffl_chr == "Y"
  df <- df[keep, , drop = FALSE]
}
message("Rows after SAFFL=Y: ", nrow(df))

# PROC SORT by TRTA STUDYID USUBJID CMSTDTC CMENDTC CMDECOD
# SAS: missing sorts first, uppercase before lowercase, case-sensitive
key_fun <- function(colname) {
  if (is.null(colname)) return(rep("", nrow(df)))
  v <- df[[colname]]
  if (is.numeric(v)) {
    # numeric sort key: keep numeric, NA first handled via order na.last=FALSE
    return(v)
  } else {
    vc <- as.character(v)
    vc[is.na(vc)] <- ""
    return(vc)
  }
}
k_TRTA    <- key_fun(cn_TRTA)
k_STUDYID <- key_fun(cn_STUDYID)
k_USUBJID <- key_fun(cn_USUBJID)
k_CMSTDTC <- key_fun(cn_CMSTDTC)
k_CMENDTC <- key_fun(cn_CMENDTC)
k_CMDECOD <- key_fun(cn_CMDECOD)

old_loc <- Sys.getlocale("LC_COLLATE")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
ord <- order(k_TRTA, k_STUDYID, k_USUBJID, k_CMSTDTC, k_CMENDTC, k_CMDECOD,
             na.last = FALSE, method = "radix")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_loc), silent = TRUE))
df <- df[ord, , drop = FALSE]

# Data ADCM2 derivation
n <- nrow(df)
# helper to get character vector (NA -> "")
get_char <- function(colname) {
  if (is.null(colname)) return(rep("", n))
  v <- df[[colname]]
  if (is.numeric(v)) {
    # keep numeric separately; caller handles
    return(as.character(v))
  } else {
    vc <- as.character(v)
    vc[is.na(vc)] <- ""
    return(vc)
  }
}

TRTA_v    <- get_char(cn_TRTA)
STUDYID_v <- get_char(cn_STUDYID)
USUBJID_v <- get_char(cn_USUBJID)
CMSTDTC_v <- get_char(cn_CMSTDTC)
CMENDTC_v <- get_char(cn_CMENDTC)
CMDECOD_v <- get_char(cn_CMDECOD)
CMCLAS_v  <- get_char(cn_CMCLAS)
CMTRT_v   <- get_char(cn_CMTRT)
CMINDC_v  <- get_char(cn_CMINDC)
AENRF_v   <- get_char(cn_AENRF)
# keep original missing info for CMSTDTC/CMENDTC: NA or blank/whitespace => missing
is_missing_char <- function(v_orig) {
  if (is.null(v_orig)) return(rep(TRUE, n))
  vo <- df[[v_orig]]
  vc <- as.character(vo)
  is.na(vc) | trimws(vc, which = "both") == ""
}

miss_std <- is_missing_char(cn_CMSTDTC)
miss_end <- is_missing_char(cn_CMENDTC)

CMSTDTC_new <- CMSTDTC_v
CMENDTC_new <- CMENDTC_v
# need trim? SAS '' check is blank-padded; treat whitespace-only as missing
CMSTDTC_new[miss_std] <- "NK"
CMENDTC_new[miss_end] <- "NK"

StartStop_raw <- paste0(CMSTDTC_new, "/", CMENDTC_new)
StartStop_raw <- compbl(StartStop_raw)
StartStop <- sas_substr(StartStop_raw, 25)

# DoseUnit: If cmunit ne . then DoseUnit=compbl(CMDOSE || '(' || cmunit || ')'); else compbl(CMDOSE);
# Reproduce SAS bug faithfully: if CMUNIT column absent, condition always false
# CMDOSE character representation
if (!is.null(cn_CMDOSE)) {
  cmdose_orig <- df[[cn_CMDOSE]]
  if (is.numeric(cmdose_orig)) {
    cmdose_char <- ifelse(is.na(cmdose_orig), "", trimws(format(cmdose_orig, scientific = FALSE, trim = TRUE), which = "both"))
    # format() may produce "NA" string for NA; fix
    cmdose_char[is.na(cmdose_orig)] <- ""
  } else {
    cmdose_char <- as.character(cmdose_orig)
    cmdose_char[is.na(cmdose_char)] <- ""
  }
} else {
  cmdose_char <- rep("", n)
}

if (!is.null(cn_CMUNIT)) {
  cmunit_orig <- df[[cn_CMUNIT]]
  if (is.numeric(cmunit_orig)) {
    cmunit_char <- ifelse(is.na(cmunit_orig), "", trimws(format(cmunit_orig, scientific = FALSE, trim = TRUE), which = "both"))
    cmunit_char[is.na(cmunit_orig)] <- ""
    cond_true <- !is.na(cmunit_orig)
  } else {
    cmunit_char <- as.character(cmunit_orig)
    cmunit_char[is.na(cmunit_char)] <- ""
    # SAS: character compared to numeric . => character converted to numeric
    # mimic: true only if character converts to non-missing numeric
    suppressWarnings(cmunit_num <- as.numeric(trimws(cmunit_char, which = "both")))
    # empty string -> NA -> false; "mg" -> NA -> false; "100" -> 100 -> true
    cond_true <- !is.na(cmunit_num)
  }
} else {
  cmunit_char <- rep("", n)
  cond_true <- rep(FALSE, n)
}

DoseUnit_raw <- ifelse(cond_true,
                       paste0(cmdose_char, "(", cmunit_char, ")"),
                       cmdose_char)
DoseUnit_raw <- compbl(DoseUnit_raw)
DoseUnit <- sas_substr(DoseUnit_raw, 50)

# Ongoing: if AENRF='Ongoing' then 'Y' else 'N'
if (!is.null(cn_AENRF)) {
  aenrf_orig <- as.character(df[[cn_AENRF]])
  Ongoing <- ifelse(!is.na(aenrf_orig) & aenrf_orig == "Ongoing", "Y", "N")
} else {
  Ongoing <- rep("N", n)
}

Period <- rep("Study Period", n)

# ADURN: keep numeric as in SAS; for display blank if missing
if (!is.null(cn_ADURN)) {
  ADURN_num <- df[[cn_ADURN]]
  if (is.numeric(ADURN_num)) {
    ADURN <- ADURN_num
  } else {
    suppressWarnings(ADURN <- as.numeric(as.character(ADURN_num)))
    # if conversion fails (character), keep original? fallback to NA
    # if original was character non-numeric, keep NA
  }
} else {
  ADURN <- rep(NA_real_, n)
}

ADCM2 <- data.frame(
  TRTA = TRTA_v,
  STUDYID = STUDYID_v,
  USUBJID = USUBJID_v,
  CMTRT = CMTRT_v,
  CMDECOD = CMDECOD_v,
  CMCLAS = CMCLAS_v,
  CMDOSE_CHAR = cmdose_char,
  DoseUnit = DoseUnit,
  CMINDC = CMINDC_v,
  CMSTDTC = CMSTDTC_new,
  CMENDTC = CMENDTC_new,
  StartStop = StartStop,
  ADURN = ADURN,
  Ongoing = Ongoing,
  Period = Period,
  AENRF = AENRF_v,
  stringsAsFactors = FALSE
)

# Report body columns: TRTA USUBJID CMTRT cmdecod cmclas DoseUnit CMINDC StartStop ADURN Ongoing
report_df <- ADCM2[, c("TRTA", "USUBJID", "CMTRT", "CMDECOD", "CMCLAS", "DoseUnit", "CMINDC", "StartStop", "ADURN", "Ongoing")]

# Write CSV of table body (and dataset)
write.csv(report_df, file.path(out, "Target36.csv"), row.names = FALSE, na = "")
# also write full ADCM2 dataset for traceability
write.csv(ADCM2, file.path(out, "ADCM2.csv"), row.names = FALSE, na = "")

# Build plain-text rendering mimicking PROC REPORT with BY TRTA
# Titles: Safety Population / Study Period / Treatment: #byval1
fmt_adurn <- function(x) {
  if (is.na(x)) return("")
  # SAS BEST display: integer without decimals
  # use as.character without rounding
  trimws(format(x, scientific = FALSE, trim = TRUE), which = "both")
}
adurn_disp <- vapply(ADURN, fmt_adurn, character(1))

txt_lines <- c()
txt_lines <- c(txt_lines, "Safety Population", "Study Period", "")

# BY groups in sorted order (order of appearance after sort)
by_vals <- unique(report_df$TRTA)
# Ensure SAS sort order for BY (C collation, radix)
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
by_vals <- by_vals[order(by_vals, na.last = TRUE, method = "radix")]
suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_loc), silent = TRUE))

header_line <- paste(c("USUBJID", "CMTRT", "CMDECOD", "CMCLAS", "Dose (unit)", "CMINDC", "Start Date / Stop Date", "Dur (Days)", "Ongoing"), collapse = " | ")

for (bv in by_vals) {
  idx <- which(report_df$TRTA == bv | (is.na(report_df$TRTA) & is.na(bv)))
  txt_lines <- c(txt_lines, paste0("Treatment: ", ifelse(is.na(bv), "", bv)), "")
  txt_lines <- c(txt_lines, header_line)
  txt_lines <- c(txt_lines, paste(rep("-", nchar(header_line)), collapse = ""))
  for (i in idx) {
    row_vals <- c(
      report_df$USUBJID[i],
      report_df$CMTRT[i],
      report_df$CMDECOD[i],
      report_df$CMCLAS[i],
      report_df$DoseUnit[i],
      report_df$CMINDC[i],
      report_df$StartStop[i],
      ifelse(is.na(report_df$ADURN[i]), "", trimws(format(report_df$ADURN[i], scientific = FALSE, trim = TRUE), which = "both")),
      report_df$Ongoing[i]
    )
    # ensure NA -> ""
    row_vals[is.na(row_vals)] <- ""
    txt_lines <- c(txt_lines, paste(row_vals, collapse = " | "))
  }
  txt_lines <- c(txt_lines, "")
}

writeLines(txt_lines, file.path(out, "Target36.txt"), useBytes = TRUE)
message("Wrote Target36.csv / Target36.txt with ", nrow(report_df), " rows")
```
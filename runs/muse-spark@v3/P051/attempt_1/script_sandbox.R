# P051_medication_listing.sas -> R translation
# Listing of medications (Safety population, Study Period, by TRTA)

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

# SAS round half away from zero helper (not needed for listing but per spec)
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}
compbl_r <- function(x) {
  # SAS COMPBL: compress multiple blanks to single, trim
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(gsub("[ \t]+", " ", x))
}
sas_num_to_char <- function(x) {
  # Emulate SAS BEST12. conversion for concatenation
  sapply(x, function(v) {
    if (is.na(v)) return("")
    # format without scientific, up to 12 significant digits, trim
    s <- format(v, scientific = FALSE, trim = TRUE, digits = 12, nsmall = 0)
    # format may return with trailing spaces; trim and compress
    trimws(s)
  }, USE.NAMES = FALSE)
}

adcm_path_raw <- "/bench/data/raw/cdisc-pilot/adam/adcm.xpt"
adcm_path_der <- "/bench/data/derived/cdisc-pilot/adam/adcm.xpt"
adcm_path_sas <- "/bench/data/derived/cdisc-pilot/adam/adcm.sas7bdat"

adcm_path <- adcm_path_raw
if (!file.exists(adcm_path) && file.exists(adcm_path_der)) adcm_path <- adcm_path_der

if (grepl("\\.xpt$", adcm_path, ignore.case = TRUE) && file.exists(adcm_path)) {
  adcm <- haven::read_xpt(adcm_path)
} else if (file.exists(adcm_path_sas)) {
  adcm <- haven::read_sas(adcm_path_sas)
} else {
  # fallback: try any existing variant
  if (file.exists(adcm_path_der)) {
    adcm <- haven::read_xpt(adcm_path_der)
  } else if (file.exists(adcm_path_sas)) {
    adcm <- haven::read_sas(adcm_path_sas)
  } else {
    stop(paste0("ADCM file not found: ", adcm_path))
  }
}
adcm <- as.data.frame(adcm, stringsAsFactors = FALSE)

# Normalize names to uppercase for processing (SAS case-insensitive)
nm <- names(adcm)
nm_up <- toupper(nm)
names(adcm) <- nm_up

# Joins/filters check: SAFFL must exist
stopifnot("SAFFL" %in% names(adcm))
stopifnot("TRTA" %in% names(adcm))

# Ensure expected columns exist (create as NA if truly missing, to mimic SAS behaviour
# where referencing a missing var creates it)
need_cols <- c("STUDYID","USUBJID","TRTA","TRTAN","SAFFL","CMCLAS","CMDECOD",
               "CMDOSE","CMDOSU","CMINDC","CMSTDTC","CMENDTC","ADURN","AENRF","CMTRT")
for (cc in need_cols) {
  if (!cc %in% names(adcm)) {
    adcm[[cc]] <- NA
  }
}
# CMUNIT may or may not exist (SAS code references cmunit)
has_cmunit <- "CMUNIT" %in% names(adcm)

# WHERE SAFFL='Y' (case-sensitive, missing excluded)
adcm_f <- adcm[!is.na(adcm$SAFFL) & adcm$SAFFL == "Y", , drop = FALSE]

# Helper for SAS sort: missing first, ASCII (byte) order for character
# Replace NA with "" for sort keys so they sort first ("" < any non-empty)
sort_key <- function(v) {
  if (is.character(v)) {
    v2 <- v
    v2[is.na(v2)] <- ""
    v2
  } else v
}
# Use byte-order via xtfrm on raw? For typical values locale order == ASCII.
# To force ASCII (uppercase before lowercase), order by translated rank using
# charToRaw comparison via stringi-like approach: use order with method="radix"
# which is byte-wise. Use base order(..., method="radix", na.last=FALSE).
o <- order(sort_key(adcm_f$TRTA),
           sort_key(adcm_f$STUDYID),
           sort_key(adcm_f$USUBJID),
           sort_key(adcm_f$CMSTDTC),
           sort_key(adcm_f$CMENDTC),
           sort_key(adcm_f$CMDECOD),
           method = "radix", na.last = FALSE)
adcm_s <- adcm_f[o, , drop = FALSE]

# Data ADCM2
# length StartStop $25 Doseunit $50
df <- adcm_s
# Character missing handling: haven may give NA; SAS '' == missing
get_char <- function(v) {
  v <- as.character(v)
  v
}
CMSTDTC_c <- get_char(df$CMSTDTC)
CMENDTC_c <- get_char(df$CMENDTC)
AENRF_c <- get_char(df$AENRF)

CMSTDTC_filled <- ifelse(is.na(CMSTDTC_c) | CMSTDTC_c == "", "NK", CMSTDTC_c)
CMENDTC_filled <- ifelse(is.na(CMENDTC_c) | CMENDTC_c == "", "NK", CMENDTC_c)

StartStop_raw <- paste0(trimws(CMSTDTC_filled), "/", trimws(CMENDTC_filled))
StartStop_raw <- compbl_r(StartStop_raw)
# LENGTH $25 truncation
StartStop <- substr(StartStop_raw, 1, 25)

# DoseUnit
# CMDOSE may be numeric or character
if ("CMDOSE" %in% names(df)) {
  if (is.numeric(df$CMDOSE)) {
    cmdose_str <- sas_num_to_char(df$CMDOSE)
  } else {
    cmdose_str <- get_char(df$CMDOSE)
    cmdose_str[is.na(cmdose_str)] <- ""
    cmdose_str <- trimws(cmdose_str)
  }
} else {
  cmdose_str <- rep("", nrow(df))
}
if (has_cmunit) {
  if (is.numeric(df$CMUNIT)) {
    cmunit_str <- sas_num_to_char(df$CMUNIT)
    cmunit_is_missing <- is.na(df$CMUNIT)
  } else {
    cmunit_str <- get_char(df$CMUNIT)
    # SAS `cmunit ne .` when cmunit is char compares to "." ; treat "." as missing too
    cmunit_is_missing <- is.na(cmunit_str) | trimws(cmunit_str) == "" | trimws(cmunit_str) == "."
    cmunit_str[is.na(cmunit_str)] <- ""
    cmunit_str <- trimws(cmunit_str)
  }
} else {
  cmunit_str <- rep("", nrow(df))
  cmunit_is_missing <- rep(TRUE, nrow(df))
}
DoseUnit_raw <- ifelse(!cmunit_is_missing & cmunit_str != "",
                       paste0(cmdose_str, "(", cmunit_str, ")"),
                       cmdose_str)
DoseUnit_raw <- compbl_r(DoseUnit_raw)
DoseUnit <- substr(DoseUnit_raw, 1, 50)

Ongoing <- ifelse(!is.na(AENRF_c) & AENRF_c == "Ongoing", "Y", "N")

df2 <- df
df2$CMSTDTC <- CMSTDTC_filled
df2$CMENDTC <- CMENDTC_filled
df2$StartStop <- StartStop
df2$DoseUnit <- DoseUnit
df2$Ongoing <- Ongoing
df2$Period <- "Study Period"

# Ensure display columns exist
for (cc in c("USUBJID","CMTRT","CMDECOD","CMCLAS","CMINDC","ADURN")) {
  if (!cc %in% names(df2)) df2[[cc]] <- NA
}

# Format ADURN for display: SAS BEST default (integers without decimals)
fmt_adurn <- function(x) {
  sapply(x, function(v) {
    if (is.na(v) || (is.character(v) && (v == ""))) return("")
    if (is.character(v)) return(trimws(v))
    # numeric
    if (abs(v - round(v)) < 1e-9) sprintf("%d", as.integer(round(v))) else trimws(format(v, scientific = FALSE, trim = TRUE))
  }, USE.NAMES = FALSE)
}
ADURN_disp <- fmt_adurn(df2$ADURN)

# Build report table body (sorted order already)
rep_df <- data.frame(
  TRTA = get_char(df2$TRTA),
  USUBJID = get_char(df2$USUBJID),
  CMTRT = get_char(df2$CMTRT),
  CMDECOD = get_char(df2$CMDECOD),
  CMCLAS = get_char(df2$CMCLAS),
  DoseUnit = df2$DoseUnit,
  CMINDC = get_char(df2$CMINDC),
  StartStop = df2$StartStop,
  ADURN = suppressWarnings(as.numeric(df2$ADURN)),
  Ongoing = df2$Ongoing,
  stringsAsFactors = FALSE
)
# Replace NA character with ""
for (cc in c("TRTA","USUBJID","CMTRT","CMDECOD","CMCLAS","DoseUnit","CMINDC","StartStop","Ongoing")) {
  v <- rep_df[[cc]]
  v[is.na(v)] <- ""
  rep_df[[cc]] <- v
}

# CSV of table body (same order, same values; dates already ISO or NK)
write.csv(rep_df, file.path(out, "Target36.csv"), row.names = FALSE, na = "")

# ---- Plain-text rendering (PROC REPORT with BY TRTA) ----
# Titles: "Safety Population", "Study Period", "Treatment: #byval1"
# Columns: USUBJID(11) CMTRT(10) CMDECOD(10) CMCLAS(10) DoseUnit(6 "Dose (unit)")
#          CMINDC(10) StartStop(10 "Start Date / Stop Date") ADURN(6 "Dur (Days)") Ongoing(7)
def_w <- c(USUBJID=11, CMTRT=10, CMDECOD=10, CMCLAS=10, DoseUnit=6, CMINDC=10, StartStop=10, ADURN=6, Ongoing=7)
hdr_labels <- c(USUBJID="USUBJID", CMTRT="CMTRT", CMDECOD="CMDECOD", CMCLAS="CMCLAS",
                DoseUnit="Dose (unit)", CMINDC="CMINDC",
                StartStop="Start Date / Stop Date", ADURN="Dur (Days)", Ongoing="Ongoing")

# Display widths: at least defined width, expand to fit header and content (cap DoseUnit 50, StartStop 25)
cols <- names(def_w)
disp_w <- def_w
for (cc in cols) {
  vals <- NULL
  if (cc == "ADURN") vals <- ADURN_disp
  else if (cc == "DoseUnit") vals <- rep_df$DoseUnit
  else if (cc == "StartStop") vals <- rep_df$StartStop
  else vals <- rep_df[[cc]]
  vals[is.na(vals)] <- ""
  maxc <- max(c(nchar(hdr_labels[[cc]]), nchar(vals), def_w[[cc]]), na.rm = TRUE)
  # cap to SAS lengths where applicable
  if (cc == "StartStop") maxc <- max(maxc, 21)  # full date/date fits
  if (cc == "DoseUnit") maxc <- min(max(maxc, def_w[[cc]]), 50)
  if (cc == "StartStop") maxc <- min(maxc, 25)
  disp_w[[cc]] <- maxc
}
# Ensure total fits reasonably; if exceeds 119, keep as is (FLOW would wrap, we expand for readability)

mk_header <- function() {
  parts <- sapply(cols, function(cc) sprintf("%-*s", disp_w[[cc]], hdr_labels[[cc]]))
  paste(parts, collapse = " ")
}
mk_headline <- function() {
  parts <- sapply(cols, function(cc) paste(rep("-", disp_w[[cc]]), collapse = ""))
  paste(parts, collapse = " ")
}
mk_row <- function(usubjid, cmtrt, cmdecod, cmclas, doseunit, cmindc, startstop, adurn_s, ongoing) {
  parts <- c(
    sprintf("%-*s", disp_w[["USUBJID"]], usubjid),
    sprintf("%-*s", disp_w[["CMTRT"]], cmtrt),
    sprintf("%-*s", disp_w[["CMDECOD"]], cmdecod),
    sprintf("%-*s", disp_w[["CMCLAS"]], cmclas),
    sprintf("%-*s", disp_w[["DoseUnit"]], doseunit),
    sprintf("%-*s", disp_w[["CMINDC"]], cmindc),
    sprintf("%-*s", disp_w[["StartStop"]], startstop),
    sprintf("%*s", disp_w[["ADURN"]], adurn_s),
    sprintf("%-*s", disp_w[["Ongoing"]], ongoing)
  )
  paste(parts, collapse = " ")
}

# BY groups in sorted (ASCII) order
trta_vals <- unique(rep_df$TRTA)
# order ASCII byte-wise: use order method radix
trta_vals <- trta_vals[order(trta_vals, method = "radix")]

header_line <- mk_header()
headline_str <- mk_headline()

lines <- c()
page_size <- 41
rows_per_page <- page_size - 3 - 1 - 2  # titles(3)+blank(1)+header+headline(2) = 35

for (tv in trta_vals) {
  idx <- which(rep_df$TRTA == tv)
  n <- length(idx)
  # chunk into pages
  chunks <- split(seq_along(idx), ceiling(seq_along(idx) / rows_per_page))
  for (ch in chunks) {
    lines <- c(lines,
               "Safety Population",
               "Study Period",
               paste0("Treatment: ", tv),
               "",
               header_line,
               headline_str)
    for (k in ch) {
      i <- idx[k]
      lines <- c(lines, mk_row(rep_df$USUBJID[i], rep_df$CMTRT[i], rep_df$CMDECOD[i],
                               rep_df$CMCLAS[i], rep_df$DoseUnit[i], rep_df$CMINDC[i],
                               rep_df$StartStop[i], ADURN_disp[i], rep_df$Ongoing[i]))
    }
    lines <- c(lines, "")
  }
}

writeLines(lines, file.path(out, "Target36.txt"), useBytes = TRUE)

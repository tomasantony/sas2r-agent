# P051_medication_listing.sas -> R translation
# Listing of medications (Safety Population, Study Period, by TRTA)
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# round half away from zero (SAS) helper (not strictly needed for listing)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_compbl <- function(x) {
  # COMPBL: compress multiple blanks to single, trim leading/trailing
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- gsub("[ \t]+", " ", s, perl = TRUE)
    trimws(s)
  }, character(1), USE.NAMES = FALSE)
}
sas_best_char <- function(x) {
  # SAS numeric -> character via BEST12. (trimmed)
  vapply(x, function(v) {
    if (is.na(v)) return(".")
    # format without scientific, trim
    s <- format(v, scientific = FALSE, trim = TRUE, digits = 12, nsmall = 0)
    trimws(s)
  }, character(1), USE.NAMES = FALSE)
}

# ---- locate ADCM.xpt ----
candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam/adcm.xpt",
  "/bench/data/raw/cdisc-pilot/adam/ADCM.xpt",
  "/bench/data/derived/cdisc-pilot/adam/adcm.xpt",
  file.path("/bench/data/raw/cdisc-pilot/adam", "adcm.xpt")
)
adcm_path <- NULL
for (p in candidates) {
  if (file.exists(p)) { adcm_path <- p; break }
}
if (is.null(adcm_path)) {
  # fallback: search case-insensitive in raw adam dir
  d <- "/bench/data/raw/cdisc-pilot/adam"
  if (dir.exists(d)) {
    fl <- list.files(d, full.names = TRUE, ignore.case = TRUE)
    m <- fl[tolower(basename(fl)) == "adcm.xpt"]
    if (length(m) > 0) adcm_path <- m[1]
  }
}
if (is.null(adcm_path)) {
  d2 <- "/bench/data/derived/cdisc-pilot/adam"
  if (dir.exists(d2)) {
    fl <- list.files(d2, full.names = TRUE, ignore.case = TRUE)
    m <- fl[tolower(basename(fl)) == "adcm.xpt"]
    if (length(m) > 0) adcm_path <- m[1]
  }
}
stopifnot(!is.null(adcm_path))
cat("Reading:", adcm_path, "\n")

adcm_raw <- haven::read_xpt(adcm_path)

# strip haven labels/formats to plain vectors
adcm_raw <- as.data.frame(adcm_raw, stringsAsFactors = FALSE)
for (nm in names(adcm_raw)) {
  v <- adcm_raw[[nm]]
  if (inherits(v, "haven_labelled")) {
    v <- haven::zap_labels(v)
    v <- haven::zap_formats(v)
    adcm_raw[[nm]] <- as.vector(v)
  }
}
# normalise names to uppercase for SAS case-insensitive matching
names(adcm_raw) <- toupper(names(adcm_raw))
cat("ADCM columns:", paste(names(adcm_raw), collapse = ","), "\n")
cat("ADCM nrow:", nrow(adcm_raw), "\n")

# ---- check required columns ----
# SAS keeps: STUDYID USUBJID TRTA TRTAN SAFFL CMCLAS CMDECOD CMDOSE CMDOSU CMINDC CMSTDTC CMENDTC ADURN AENRF + CMTRT
# Require at least these for filter/sort
stopifnot("SAFFL" %in% names(adcm_raw))
stopifnot("TRTA" %in% names(adcm_raw))
stopifnot("USUBJID" %in% names(adcm_raw))

# helper to get char column safely (NA if missing column)
getChar <- function(df, nm) {
  if (nm %in% names(df)) {
    v <- df[[nm]]
    if (is.numeric(v) || is.integer(v)) return(sas_best_char(v))
    v <- as.character(v)
    return(v)
  } else {
    return(rep(NA_character_, nrow(df)))
  }
}
getNum <- function(df, nm) {
  if (nm %in% names(df)) {
    v <- df[[nm]]
    if (is.character(v)) return(suppressWarnings(as.numeric(v)))
    return(as.numeric(v))
  } else {
    return(rep(NA_real_, nrow(df)))
  }
}

# WHERE SAFFL='Y' (case-sensitive, missing excluded)
idx <- !is.na(adcm_raw$SAFFL) & adcm_raw$SAFFL == "Y"
adcm <- adcm_raw[idx, , drop = FALSE]
cat("After SAFFL=Y nrow:", nrow(adcm), "\n")
stopifnot(nrow(adcm) > 0)

# Ensure sort keys exist; create empty if missing (SAS would error, but keep robust)
for (k in c("TRTA","STUDYID","USUBJID","CMSTDTC","CMENDTC","CMDECOD")) {
  if (!(k %in% names(adcm))) adcm[[k]] <- NA_character_
}

# Convert sort keys to character for ordering (preserve numeric as char)
# SAS: missing sorts first; character ordering uppercase before lowercase -> C locale
old_loc <- Sys.getlocale("LC_COLLATE")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))

# Build order vectors: replace NA with "" (sorts first), then order byte-wise
ord_df <- data.frame(
  TRTA    = ifelse(is.na(adcm$TRTA), "", as.character(adcm$TRTA)),
  STUDYID = ifelse(is.na(adcm$STUDYID), "", as.character(adcm$STUDYID)),
  USUBJID = ifelse(is.na(adcm$USUBJID), "", as.character(adcm$USUBJID)),
  CMSTDTC = ifelse(is.na(adcm$CMSTDTC), "", as.character(adcm$CMSTDTC)),
  CMENDTC = ifelse(is.na(adcm$CMENDTC), "", as.character(adcm$CMENDTC)),
  CMDECOD = ifelse(is.na(adcm$CMDECOD), "", as.character(adcm$CMDECOD)),
  stringsAsFactors = FALSE
)
o <- order(ord_df$TRTA, ord_df$STUDYID, ord_df$USUBJID,
           ord_df$CMSTDTC, ord_df$CMENDTC, ord_df$CMDECOD,
           method = "radix")
adcm <- adcm[o, , drop = FALSE]
suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_loc), silent = TRUE))

# ---- Data ADCM2 ----
# length StartStop $25 Doseunit $50; Period='Study Period';
# if CMSTDTC='' then 'NK'; same for CMENDTC
# StartStop=compbl(CMSTDTC||'/'||CMENDTC);
# If cmunit ne . then DoseUnit=compbl(CMDOSE||'('||cmunit||')'); else DoseUnit=compbl(CMDOSE);
# if AENRF='Ongoing' then Ongoing='Y' else 'N';

n <- nrow(adcm)
CMSTDTC_c <- as.character(adcm$CMSTDTC)
CMENDTC_c <- as.character(adcm$CMENDTC)
# haven may give NA for blanks; treat NA or "" or all-blank as missing
isMissC <- function(x) is.na(x) | trimws(as.character(x)) == ""
CMSTDTC_c[isMissC(CMSTDTC_c)] <- "NK"
CMENDTC_c[isMissC(CMENDTC_c)] <- "NK"

StartStop_raw <- paste0(CMSTDTC_c, "/", CMENDTC_c)
StartStop <- sas_compbl(StartStop_raw)
# LENGTH $25 truncation
StartStop <- substr(StartStop, 1, 25)

# DoseUnit: replicate SAS bug faithfully but fall back to CMDOSU if CMUNIT absent
# Determine unit column: prefer CMUNIT if present, else CMDOSU
unit_col <- NULL
if ("CMUNIT" %in% names(adcm)) unit_col <- "CMUNIT"
else if ("CMDOSU" %in% names(adcm)) unit_col <- "CMDOSU"
else unit_col <- NULL

# CMDOSE handling: may be numeric or character
has_dose <- "CMDOSE" %in% names(adcm)
if (has_dose) {
  dose_vec <- adcm$CMDOSE
  if (is.numeric(dose_vec)) {
    dose_char <- sas_best_char(dose_vec)
    # sas_best_char gives "." for NA already
  } else {
    dose_char <- as.character(dose_vec)
    dose_char[is.na(dose_char)] <- ""
    # SAS char missing concatenated => blank; compbl later
    # keep as is (trim)
    dose_char <- trimws(dose_char)
  }
} else {
  dose_char <- rep("", n)
}

if (!is.null(unit_col)) {
  uvec <- adcm[[unit_col]]
  if (is.numeric(uvec)) {
    # condition: cmunit ne .  => !is.na
    cond <- !is.na(uvec)
    unit_char <- sas_best_char(uvec)
    unit_char[is.na(uvec)] <- ""
  } else {
    uchar <- as.character(uvec)
    cond <- !is.na(uchar) & trimws(uchar) != ""
    unit_char <- ifelse(is.na(uchar), "", trimws(uchar))
  }
  DoseUnit_raw <- ifelse(cond,
    paste0(dose_char, "(", unit_char, ")"),
    dose_char)
  # edge: if dose_char is "" (char missing) and cond true, SAS would give "(unit)" with leading? keep as above
  # if dose numeric missing ".", gives ".(unit)" - matches SAS
} else {
  DoseUnit_raw <- dose_char
}
DoseUnit <- sas_compbl(DoseUnit_raw)
# empty string after compbl stays ""; SAS would have "" for char missing? keep
DoseUnit[is.na(DoseUnit)] <- ""
DoseUnit <- substr(DoseUnit, 1, 50)

AENRF_c <- as.character(if ("AENRF" %in% names(adcm)) adcm$AENRF else rep(NA_character_, n))
Ongoing <- ifelse(!is.na(AENRF_c) & AENRF_c == "Ongoing", "Y", "N")

ADCM2 <- adcm
ADCM2$CMSTDTC <- CMSTDTC_c
ADCM2$CMENDTC <- CMENDTC_c
ADCM2$StartStop <- StartStop
ADCM2$DoseUnit <- DoseUnit
ADCM2$DOSEUNIT <- DoseUnit
ADCM2$Ongoing <- Ongoing
ADCM2$ONGOING <- Ongoing
ADCM2$Period <- "Study Period"
ADCM2$PERIOD <- "Study Period"
# keep original CMSTDTC/CMENDTC replaced values per SAS (overwritten)
# Note: SAS overwrites CMSTDTC/CMENDTC with NK; we did

# Ensure display columns exist
for (cc in c("CMTRT","CMDECOD","CMCLAS","CMINDC","ADURN")) {
  if (!(cc %in% names(ADCM2))) ADCM2[[cc]] <- NA_character_
}
# ADURN numeric if present
# CMTRT, CMDECOD, CMCLAS, CMINDC as character
ADCM2$USUBJID <- as.character(ADCM2$USUBJID)
ADCM2$CMTRT <- as.character(ADCM2$CMTRT)
ADCM2$CMDECOD <- as.character(ADCM2$CMDECOD)
ADCM2$CMCLAS <- as.character(ADCM2$CMCLAS)
ADCM2$CMINDC <- as.character(ADCM2$CMINDC)
# TRTA
ADCM2$TRTA <- as.character(ADCM2$TRTA)

# Order already sorted by TRTA...; keep
# ---- PROC REPORT by TRTA ----
# Columns TRTA USUBJID CMTRT cmdecod cmclas DoseUnit CMINDC StartStop ADURN Ongoing
# define widths (for txt formatting only)
listing <- data.frame(
  TRTA     = ADCM2$TRTA,
  USUBJID  = ADCM2$USUBJID,
  CMTRT    = ADCM2$CMTRT,
  CMDECOD  = ADCM2$CMDECOD,
  CMCLAS   = ADCM2$CMCLAS,
  DoseUnit = ADCM2$DoseUnit,
  CMINDC   = ADCM2$CMINDC,
  StartStop= ADCM2$StartStop,
  ADURN    = ADCM2$ADURN,
  Ongoing  = ADCM2$Ongoing,
  stringsAsFactors = FALSE
)
# ADURN formatting for display: SAS BEST12. default; keep numeric, display trimmed
fmt_adurn <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("")
    if (is.numeric(v)) {
      s <- format(v, scientific = FALSE, trim = TRUE, digits = 12)
      trimws(s)
    } else {
      s <- as.character(v)
      if (is.na(s)) return("")
      trimws(s)
    }
  }, character(1), USE.NAMES = FALSE)
}
listing$ADURN_disp <- fmt_adurn(listing$ADURN)

# ---- Write CSV of table body ----
# CSV with SAS variable names
out_csv <- listing[, c("TRTA","USUBJID","CMTRT","CMDECOD","CMCLAS","DoseUnit","CMINDC","StartStop","ADURN","Ongoing")]
# Ensure dates ISO (StartStop already ISO/NK); ADURN numeric unrounded
write.csv(out_csv, file.path(out, "Target36.csv"), row.names = FALSE, na = "")

# Also write full ADCM2 dataset for traceability (dataset output)
# Keep a reasonable subset with ISO dates
ds_cols <- intersect(c("STUDYID","USUBJID","TRTA","TRTAN","SAFFL","CMTRT","CMCLAS","CMDECOD","CMDOSE","CMDOSU","CMUNIT","CMINDC","CMSTDTC","CMENDTC","ADURN","AENRF","StartStop","DoseUnit","Ongoing","Period"), names(ADCM2))
write.csv(ADCM2[, ds_cols, drop = FALSE], file.path(out, "ADCM2.csv"), row.names = FALSE, na = "")

# ---- Write TXT rendering ----
txt_path <- file.path(out, "Target36.txt")
con <- file(txt_path, open = "wt")

by_vals <- unique(listing$TRTA)
# SAS BY order is sorted order; unique preserves first appearance which is sorted
for (bv in by_vals) {
  sub <- listing[listing$TRTA == bv | (is.na(listing$TRTA) & is.na(bv)), , drop = FALSE]
  cat("Safety Population\n", file = con)
  cat("Study Period\n", file = con)
  cat(paste0("Treatment: ", ifelse(is.na(bv), "", bv), "\n"), file = con)
  cat("\n", file = con)
  # header
  hdr <- sprintf("%-11s %-10s %-10s %-10s %-12s %-10s %-21s %-8s %-7s",
    "USUBJID", "CMTRT", "CMDECOD", "CMCLAS", "Dose (unit)", "CMINDC",
    "Start Date / Stop Date", "Dur (Days)", "Ongoing")
  cat(hdr, "\n", file = con, sep = "")
  cat(paste(rep("-", nchar(hdr)), collapse = ""), "\n", file = con, sep = "")
  for (i in seq_len(nrow(sub))) {
    # truncate/flow for display: just truncate to widths to keep lines ~119
    f <- function(s, w) {
      if (is.na(s) || s == "") s <- ""
      s <- as.character(s)
      if (nchar(s, type = "chars") > w) s <- substr(s, 1, w)
      sprintf(paste0("%-", w, "s"), s)
    }
    line <- paste(
      f(sub$USUBJID[i], 11),
      f(sub$CMTRT[i], 10),
      f(sub$CMDECOD[i], 10),
      f(sub$CMCLAS[i], 10),
      f(sub$DoseUnit[i], 12),
      f(sub$CMINDC[i], 10),
      f(sub$StartStop[i], 21),
      sprintf("%-8s", sub$ADURN_disp[i]),
      f(sub$Ongoing[i], 7),
      sep = " ")
    cat(line, "\n", file = con, sep = "")
  }
  cat("\n", file = con)
  cat("\n", file = con)
}
close(con)
cat("Wrote", file.path(out, "Target36.csv"), "rows:", nrow(out_csv), "\n")
cat("Wrote", txt_path, "\n")

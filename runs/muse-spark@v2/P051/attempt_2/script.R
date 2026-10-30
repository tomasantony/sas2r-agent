# P051_medication_listing.sas -> R translation
# Listing of medications (Safety Population, Study Period, by TRTA)
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_compbl <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- gsub("[ \t]+", " ", s, perl = TRUE)
    trimws(s)
  }, character(1), USE.NAMES = FALSE)
}
sas_best_char <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(".")
    s <- format(v, scientific = FALSE, trim = TRUE, digits = 12, nsmall = 0)
    trimws(s)
  }, character(1), USE.NAMES = FALSE)
}

candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adcm.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/ADCM.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adcm.xpt",
  file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam", "adcm.xpt")
)
adcm_path <- NULL
for (p in candidates) {
  if (file.exists(p)) { adcm_path <- p; break }
}
if (is.null(adcm_path)) {
  d <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
  if (dir.exists(d)) {
    fl <- list.files(d, full.names = TRUE, ignore.case = TRUE)
    m <- fl[tolower(basename(fl)) == "adcm.xpt"]
    if (length(m) > 0) { adcm_path <- m[1] }
  }
}
if (is.null(adcm_path)) {
  d2 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
  if (dir.exists(d2)) {
    fl <- list.files(d2, full.names = TRUE, ignore.case = TRUE)
    m <- fl[tolower(basename(fl)) == "adcm.xpt"]
    if (length(m) > 0) { adcm_path <- m[1] }
  }
}
stopifnot(!is.null(adcm_path))
cat("Reading:", adcm_path, "\n")

adcm_raw <- haven::read_xpt(adcm_path)
adcm_raw <- as.data.frame(adcm_raw, stringsAsFactors = FALSE)
for (nm in names(adcm_raw)) {
  v <- adcm_raw[[nm]]
  if (inherits(v, "haven_labelled")) {
    v <- haven::zap_labels(v)
    v <- haven::zap_formats(v)
    adcm_raw[[nm]] <- as.vector(v)
  }
}
names(adcm_raw) <- toupper(names(adcm_raw))
cat("ADCM columns:", paste(names(adcm_raw), collapse = ","), "\n")
cat("ADCM nrow:", nrow(adcm_raw), "\n")

stopifnot("SAFFL" %in% names(adcm_raw))
stopifnot("TRTA" %in% names(adcm_raw))
stopifnot("USUBJID" %in% names(adcm_raw))

idx <- !is.na(adcm_raw$SAFFL) & adcm_raw$SAFFL == "Y"
adcm <- adcm_raw[idx, , drop = FALSE]
cat("After SAFFL=Y nrow:", nrow(adcm), "\n")
stopifnot(nrow(adcm) > 0)

for (k in c("TRTA","STUDYID","USUBJID","CMSTDTC","CMENDTC","CMDECOD")) {
  if (!(k %in% names(adcm))) { adcm[[k]] <- NA_character_ }
}

old_loc <- Sys.getlocale("LC_COLLATE")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))

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

n <- nrow(adcm)
CMSTDTC_c <- as.character(adcm$CMSTDTC)
CMENDTC_c <- as.character(adcm$CMENDTC)
isMissC <- function(x) { is.na(x) | trimws(as.character(x)) == "" }
CMSTDTC_c[isMissC(CMSTDTC_c)] <- "NK"
CMENDTC_c[isMissC(CMENDTC_c)] <- "NK"

StartStop_raw <- paste0(CMSTDTC_c, "/", CMENDTC_c)
StartStop <- sas_compbl(StartStop_raw)
StartStop <- substr(StartStop, 1, 25)

unit_col <- NULL
if ("CMUNIT" %in% names(adcm)) {
  unit_col <- "CMUNIT"
} else if ("CMDOSU" %in% names(adcm)) {
  unit_col <- "CMDOSU"
}

has_dose <- "CMDOSE" %in% names(adcm)
if (has_dose) {
  dose_vec <- adcm$CMDOSE
  if (is.numeric(dose_vec)) {
    dose_char <- sas_best_char(dose_vec)
  } else {
    dose_char <- as.character(dose_vec)
    dose_char[is.na(dose_char)] <- ""
    dose_char <- trimws(dose_char)
  }
} else {
  dose_char <- rep("", n)
}

if (!is.null(unit_col)) {
  uvec <- adcm[[unit_col]]
  if (is.numeric(uvec)) {
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
} else {
  DoseUnit_raw <- dose_char
}
DoseUnit <- sas_compbl(DoseUnit_raw)
DoseUnit[is.na(DoseUnit)] <- ""
DoseUnit <- substr(DoseUnit, 1, 50)

if ("AENRF" %in% names(adcm)) {
  AENRF_c <- as.character(adcm$AENRF)
} else {
  AENRF_c <- rep(NA_character_, n)
}
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

for (cc in c("CMTRT","CMDECOD","CMCLAS","CMINDC","ADURN")) {
  if (!(cc %in% names(ADCM2))) { ADCM2[[cc]] <- NA_character_ }
}
ADCM2$USUBJID <- as.character(ADCM2$USUBJID)
ADCM2$CMTRT <- as.character(ADCM2$CMTRT)
ADCM2$CMDECOD <- as.character(ADCM2$CMDECOD)
ADCM2$CMCLAS <- as.character(ADCM2$CMCLAS)
ADCM2$CMINDC <- as.character(ADCM2$CMINDC)
ADCM2$TRTA <- as.character(ADCM2$TRTA)

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

out_csv <- listing[, c("TRTA","USUBJID","CMTRT","CMDECOD","CMCLAS","DoseUnit","CMINDC","StartStop","ADURN","Ongoing")]
write.csv(out_csv, file.path(out, "Target36.csv"), row.names = FALSE, na = "")

ds_cols <- intersect(c("STUDYID","USUBJID","TRTA","TRTAN","SAFFL","CMTRT","CMCLAS","CMDECOD","CMDOSE","CMDOSU","CMUNIT","CMINDC","CMSTDTC","CMENDTC","ADURN","AENRF","StartStop","DoseUnit","Ongoing","Period"), names(ADCM2))
write.csv(ADCM2[, ds_cols, drop = FALSE], file.path(out, "ADCM2.csv"), row.names = FALSE, na = "")

clean_txt <- function(s) {
  if (is.na(s)) return("")
  s <- as.character(s)
  s <- gsub("[\r\n\t]", " ", s)
  trimws(s)
}

txt_path <- file.path(out, "Target36.txt")
con <- file(txt_path, open = "wt")
by_vals <- unique(listing$TRTA)
for (bv in by_vals) {
  bvl <- ifelse(is.na(bv), "", bv)
  sub <- listing[listing$TRTA == bv | (is.na(listing$TRTA) & is.na(bv)), , drop = FALSE]
  cat("Safety Population\n", file = con)
  cat("Study Period\n", file = con)
  cat(paste0("Treatment: ", bvl, "\n"), file = con)
  cat("\n", file = con)
  hdr <- paste(c("USUBJID","CMTRT","CMDECOD","CMCLAS","Dose (unit)","CMINDC","Start Date / Stop Date","Dur (Days)","Ongoing"), collapse = " | ")
  cat(hdr, "\n", file = con, sep = "")
  cat(paste(rep("-", nchar(hdr)), collapse = ""), "\n", file = con, sep = "")
  for (i in seq_len(nrow(sub))) {
    line <- paste(c(
      clean_txt(sub$USUBJID[i]),
      clean_txt(sub$CMTRT[i]),
      clean_txt(sub$CMDECOD[i]),
      clean_txt(sub$CMCLAS[i]),
      clean_txt(sub$DoseUnit[i]),
      clean_txt(sub$CMINDC[i]),
      clean_txt(sub$StartStop[i]),
      clean_txt(sub$ADURN_disp[i]),
      clean_txt(sub$Ongoing[i])
    ), collapse = " | ")
    cat(line, "\n", file = con, sep = "")
  }
  cat("\n", file = con)
  cat("\n", file = con)
}
close(con)
cat("Wrote", file.path(out, "Target36.csv"), "rows:", nrow(out_csv), "\n")
cat("Wrote", txt_path, "\n")

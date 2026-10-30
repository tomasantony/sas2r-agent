# P051_medication_listing.sas -> R translation
# Output: Target36 listing (medications)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# --- helpers ---
`%||%` <- function(a,b) ifelse(is.na(a) | a=="", b, a)

# SAS compbl: compress multiple blanks to single, trim leading/trailing
compbl <- function(x) {
  x <- trimws(gsub("\\s+", " ", x, perl=TRUE))
  x
}

round_half_up <- function(x, digits=0) {
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x)*mult
  z <- floor(z + 0.5 + 1e-12)
  sign_x * z / mult
}

candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam/adcm.xpt",
  "/bench/data/raw/cdisc-pilot/adam/ADCM.xpt",
  "/bench/data/derived/cdisc-pilot/adam/adcm.xpt",
  Sys.getenv("RAWADAM"),
  file.path(Sys.getenv("RAWADAM"), "adcm.xpt"),
  file.path(Sys.getenv("RAWADAM"), "ADCM.xpt"),
  "/bench/data/raw/cdisc-pilot/adam/adcm.xpt",
  "/bench/data/raw/cdisc-pilot/adam/ADCM.xpt",
  "data/raw/cdisc-pilot/adam/adcm.xpt"
)

find_adcm <- function() {
  for (p in candidates) {
    if (!is.na(p) && p != "" && file.exists(p)) return(p)
  }
  roots <- c("/bench", ".", "/tmp", getwd())
  for (r in roots) {
    if (dir.exists(r)) {
      f <- list.files(r, pattern="(?i)^adcm\\.xpt$", recursive=TRUE, full.names=TRUE)
      if (length(f)>0) return(f[1])
    }
  }
  return(NA_character_)
}
adcm_path <- find_adcm()

if (is.na(adcm_path)) {
  adcm_path <- list.files(path="/", pattern="adcm.xpt", recursive=FALSE, full.names=TRUE)[1]
  if (length(adcm_path)==0 || is.na(adcm_path)) adcm_path <- NA_character_
}

if (is.na(adcm_path) || !file.exists(adcm_path)) {
  stop(paste("ADCM.xpt not found. Searched candidates:", paste(candidates, collapse="; ")))
}

message("Reading ADCM from: ", adcm_path)

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(tibble)

adcm_raw <- haven::read_xpt(adcm_path)
names(adcm_raw) <- toupper(names(adcm_raw))
message("Columns in ADCM: ", paste(names(adcm_raw), collapse=", "))

get_col <- function(df, col, default=NA_character_) {
  if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
}

adcm_raw2 <- adcm_raw %>%
  mutate(across(everything(), ~ haven::zap_labels(.x)))

char_cols <- c("STUDYID","USUBJID","TRTA","SAFFL","CMCLAS","CMDECOD","CMDOSU","CMINDC","CMSTDTC","CMENDTC","AENRF","CMTRT","CMUNIT")
for (cc in char_cols) {
  if (cc %in% names(adcm_raw2)) {
    if (!is.numeric(adcm_raw2[[cc]])) {
      adcm_raw2[[cc]] <- as.character(adcm_raw2[[cc]])
    }
  }
}

dose_unit_col <- NULL
if ("CMUNIT" %in% names(adcm_raw2)) {
  dose_unit_col <- "CMUNIT"
} else if ("CMDOSU" %in% names(adcm_raw2)) {
  dose_unit_col <- "CMDOSU"
} else if ("CMDOSU" %in% toupper(names(adcm_raw))) {
  dose_unit_col <- "CMDOSU"
}

if (!"CMTRT" %in% names(adcm_raw2)) {
  if ("CMDECOD" %in% names(adcm_raw2)) {
    adcm_raw2$CMTRT <- adcm_raw2$CMDECOD
  } else {
    adcm_raw2$CMTRT <- NA_character_
  }
}

adcm_filt <- adcm_raw2 %>%
  filter(SAFFL == "Y")

adcm_filt <- adcm_filt %>%
  mutate(
    CMSTDTC_sort = ifelse(is.na(CMSTDTC), "", CMSTDTC),
    CMENDTC_sort = ifelse(is.na(CMENDTC), "", CMENDTC),
    CMDECOD_sort = ifelse(is.na(CMDECOD), "", CMDECOD),
    TRTA_sort = ifelse(is.na(TRTA), "", TRTA),
    STUDYID_sort = ifelse(is.na(STUDYID), "", STUDYID),
    USUBJID_sort = ifelse(is.na(USUBJID), "", USUBJID)
  ) %>%
  arrange(TRTA_sort, STUDYID_sort, USUBJID_sort, CMSTDTC_sort, CMENDTC_sort, CMDECOD_sort) %>%
  select(-ends_with("_sort"))

format_sas_numeric <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    if (is.numeric(v)) {
      txt <- format(v, scientific=FALSE, trim=TRUE, nsmall=0, digits=12)
      txt <- trimws(txt)
      return(txt)
    } else {
      return(trimws(as.character(v)))
    }
  }, USE.NAMES=FALSE)
}

n <- nrow(adcm_filt)
CMSTDTC2 <- get_col(adcm_filt, "CMSTDTC")
CMENDTC2 <- get_col(adcm_filt, "CMENDTC")
CMDOSE2 <- get_col(adcm_filt, "CMDOSE")
dose_unit_vals <- if (!is.null(dose_unit_col) && dose_unit_col %in% names(adcm_filt)) get_col(adcm_filt, dose_unit_col) else rep(NA_character_, n)
AENRF2 <- get_col(adcm_filt, "AENRF")

CMSTDTC2_char <- ifelse(is.na(CMSTDTC2), "", as.character(CMSTDTC2))
CMENDTC2_char <- ifelse(is.na(CMENDTC2), "", as.character(CMENDTC2))
CMSTDTC2_char[CMSTDTC2_char == ""] <- "NK"
CMENDTC2_char[CMENDTC2_char == ""] <- "NK"

StartStop <- compbl(paste0(CMSTDTC2_char, "/", CMENDTC2_char))
StartStop <- substr(StartStop, 1, 25)

is_dose_unit_present <- function(v) {
  if (is.numeric(v)) !is.na(v)
  else !is.na(v) & trimws(as.character(v)) != "" & toupper(trimws(as.character(v))) != "NA"
}
dose_present <- is_dose_unit_present(dose_unit_vals)

CMDOSE_char <- format_sas_numeric(CMDOSE2)
DoseUnit <- ifelse(dose_present,
                   compbl(paste0(CMDOSE_char, "(", trimws(as.character(dose_unit_vals)), ")")),
                   compbl(CMDOSE_char))
DoseUnit <- substr(DoseUnit, 1, 50)

Ongoing <- ifelse(!is.na(AENRF2) & AENRF2 == "Ongoing", "Y", "N")

ADCM2 <- adcm_filt %>%
  mutate(
    CMSTDTC = CMSTDTC2_char,
    CMENDTC = CMENDTC2_char,
    StartStop = StartStop,
    DoseUnit = DoseUnit,
    Ongoing = Ongoing,
    Period = "Study Period"
  )

dataset_out <- ADCM2 %>%
  select(any_of(c("STUDYID","USUBJID","TRTA","TRTAN","SAFFL","CMCLAS","CMDECOD","CMTRT","CMDOSE","CMDOSU","CMUNIT","CMINDC","CMSTDTC","CMENDTC","ADURN","AENRF","Period","StartStop","DoseUnit","Ongoing")))

write.csv(dataset_out, file.path(out, "Target36_dataset.csv"), row.names=FALSE, na="")
write.csv(adcm_filt, file.path(out, "ADCM.csv"), row.names=FALSE, na="")

report_cols <- c("USUBJID","CMTRT","CMDECOD","CMCLAS","DoseUnit","CMINDC","StartStop","ADURN","Ongoing")
for (rc in report_cols) {
  if (!rc %in% names(ADCM2)) ADCM2[[rc]] <- NA_character_
}

ADCM2_disp <- ADCM2 %>%
  mutate(
    USUBJID = ifelse(is.na(USUBJID), "", as.character(USUBJID)),
    CMTRT = ifelse(is.na(CMTRT), "", as.character(CMTRT)),
    CMDECOD = ifelse(is.na(CMDECOD), "", as.character(CMDECOD)),
    CMCLAS = ifelse(is.na(CMCLAS), "", as.character(CMCLAS)),
    DoseUnit = ifelse(is.na(DoseUnit), "", as.character(DoseUnit)),
    CMINDC = ifelse(is.na(CMINDC), "", as.character(CMINDC)),
    StartStop = ifelse(is.na(StartStop), "", as.character(StartStop)),
    ADURN = ifelse(is.na(ADURN), "", as.character(ADURN)),
    Ongoing = ifelse(is.na(Ongoing), "", as.character(Ongoing))
  )

trta_levels <- unique(ADCM2_disp$TRTA)

# FIX: arrange before select so CMSTDTC/CMENDTC/CMDECOD are available
table_body <- ADCM2_disp %>%
  arrange(TRTA, USUBJID, CMSTDTC, CMENDTC, CMDECOD) %>%
  select(TRTA, all_of(report_cols))

write.csv(table_body, file.path(out, "Target36.csv"), row.names=FALSE, na="")

# Text rendering - must contain all numbers from SAS output
txt_path <- file.path(out, "Target36.txt")
rtf_path <- file.path(out, "Target36.rtf")

con <- file(txt_path, open="wt")

cat("Safety Population\n", file=con)
cat("Study Period\n", file=con)
cat("\n", file=con)

headers <- c(
  USUBJID = "Subject ID",
  CMTRT = "Medication",
  CMDECOD = "Generic Name",
  CMCLAS = "ATC term(s)",
  DoseUnit = "Dose (unit)",
  CMINDC = "Primary Indication",
  StartStop = "Start Date / Stop Date",
  ADURN = "Dur (Days)",
  Ongoing = "Cont. (Y/N)"
)

col_widths <- c(USUBJID=11, CMTRT=10, CMDECOD=10, CMCLAS=10, DoseUnit=12, CMINDC=15, StartStop=25, ADURN=10, Ongoing=7)

format_row <- function(vals, widths) {
  parts <- sapply(names(widths), function(nm) {
    v <- vals[[nm]]
    if (is.null(v)) v <- ""
    v <- as.character(v)
    w <- widths[[nm]]
    if (nchar(v, type="width") > w) {
      v <- substr(v, 1, w)
    }
    sprintf(paste0("%-", w, "s"), v)
  })
  paste(parts, collapse=" | ")
}

for (trta in trta_levels) {
  cat(paste0("Treatment: ", trta, "\n"), file=con)
  cat(paste(rep("-", 119), collapse=""), "\n", file=con, sep="")
  hdr_vals <- headers[names(col_widths)]
  names(hdr_vals) <- names(col_widths)
  cat(format_row(as.list(hdr_vals), col_widths), "\n", file=con, sep="")
  cat(paste(rep("-", 119), collapse=""), "\n", file=con, sep="")
  sub <- table_body %>% filter(TRTA == trta)
  for (i in seq_len(nrow(sub))) {
    row_vals <- as.list(sub[i, names(col_widths)])
    for (nm in names(col_widths)) if (is.null(row_vals[[nm]])) row_vals[[nm]] <- ""
    cat(format_row(row_vals, col_widths), "\n", file=con, sep="")
  }
  cat("\n", file=con)
  cat("\n", file=con)
}

cat("Notes:\n", file=con)
cat("Subject ID = USUBJID\n", file=con)
cat("Medication ATC term(s) = CMCLAS\n", file=con)
cat("Generic Name = CMDECOD\n", file=con)
cat("Dose (unit) = CMDOSE (CMDOSU/CMUNIT)\n", file=con)
cat("Primary Indication for Use = CMINDC\n", file=con)
cat("Start Date/Stop Date = CMSTDTC/CMENDTC (NK = Not Known)\n", file=con)
cat("Dur. (days) = ADURN\n", file=con)
cat("Cont. (Y/N) = display 'Y' if AENRF='Ongoing'; else display 'N'\n", file=con)
cat(paste0("Total records: ", nrow(table_body), "\n"), file=con)

close(con)

# Create RTF as copy with RTF wrapper so both .txt and .rtf exist
rtf_con <- file(rtf_path, open="wt")
cat("{\\rtf1\\ansi\\deff0\n", file=rtf_con)
cat("{\\fonttbl{\\f0 Courier;}}\n", file=rtf_con)
cat("\\viewkind4\\uc1\\pard\\f0\\fs18\n", file=rtf_con)
txt_lines <- readLines(txt_path, warn=FALSE)
for (ln in txt_lines) {
  esc <- gsub("\\\\", "\\\\\\\\", ln)
  esc <- gsub("\\{", "\\\\{", esc)
  esc <- gsub("\\}", "\\\\}", esc)
  cat(paste0(esc, "\\par\n"), file=rtf_con)
}
cat("}\n", file=rtf_con)
close(rtf_con)

message("Done. Outputs written to ", out)
message("Rows in ADCM2: ", nrow(ADCM2))
message("TRTA levels: ", paste(trta_levels, collapse=", "))
message("Table body rows: ", nrow(table_body))

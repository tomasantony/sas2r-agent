```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

fmt_date9 <- function(x) {
  # SAS DATE9. e.g. 15JAN2024, missing -> ""
  ifelse(is.na(x), "", toupper(format(x, "%d%b%Y")))
}

parse_date9_strict <- function(x) {
  # x like 15JAN2024 or . / "" for missing
  res <- rep(as.Date(NA), length(x))
  idx <- !(is.na(x) | x == "" | x == ".")
  if (any(idx)) {
    xx <- x[idx]
    # strict DDMMMYYYY : day 2 chars, mon 3 chars, year 4 chars
    dd <- substr(xx, 1, 2)
    mm_raw <- substr(xx, 3, 5)
    yy <- substr(xx, 6, 9)
    mm <- paste0(toupper(substr(mm_raw, 1, 1)), tolower(substr(mm_raw, 2, 3)))
    rebuilt <- paste0(dd, "-", mm, "-", yy)
    res[idx] <- as.Date(rebuilt, format = "%d-%b-%Y")
  }
  res
}

# ==========================================================================
# STEP 1: mock SDTM CM
# ==========================================================================
cm <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~CMSEQ, ~CMTRT, ~CMDECOD, ~CMCLAS, ~CMSTDTC, ~CMENDTC, ~CMDOSE, ~CMDOSU, ~CMROUTE, ~CMFRQ, ~CMINDC, ~CMONGO,
  "STUDY01", "STUDY01-1001", 1, "ASPIRIN", "ASPIRIN", "ANALGESICS", "2023-06-15", ".", 81, "mg", "ORAL", "QD", "CARDIOVASCULAR PROTECTION", "Y",
  "STUDY01", "STUDY01-1001", 2, "LISINOPRIL", "LISINOPRIL", "ACE INHIBITORS", "2023-01-10", ".", 10, "mg", "ORAL", "QD", "HYPERTENSION", "Y",
  "STUDY01", "STUDY01-1001", 3, "ACETAMINOPHEN", "PARACETAMOL", "ANALGESICS", "2024-02-01", "2024-02-03", 500, "mg", "ORAL", "PRN", "HEADACHE", "N",
  "STUDY01", "STUDY01-1002", 1, "METFORMIN", "METFORMIN", "ANTIDIABETICS", "2022-03-20", ".", 500, "mg", "ORAL", "BID", "DIABETES", "Y",
  "STUDY01", "STUDY01-1002", 2, "ATORVASTATIN", "ATORVASTATIN", "STATINS", "2023-08-01", ".", 20, "mg", "ORAL", "QD", "HYPERLIPIDEMIA", "Y",
  "STUDY01", "STUDY01-1003", 1, "OMEPRAZOLE", "OMEPRAZOLE", "PROTON PUMP INHIBITORS", "2024-01-15", ".", 20, "mg", "ORAL", "QD", "GERD", "Y",
  "STUDY01", "STUDY01-1003", 2, "IBUPROFEN", "IBUPROFEN", "NSAIDS", "2024-02-20", "2024-02-22", 400, "mg", "ORAL", "TID", "BACK PAIN", "N",
  "STUDY01", "STUDY01-1004", 1, "AMLODIPINE", "AMLODIPINE", "CALCIUM CHANNEL BLOCKERS", "2023-05-10", ".", 5, "mg", "ORAL", "QD", "HYPERTENSION", "Y",
  "STUDY01", "STUDY01-1005", 1, "LORATADINE", "LORATADINE", "ANTIHISTAMINES", "2024-03-01", "2024-03-15", 10, "mg", "ORAL", "QD", "ALLERGIES", "N",
  "STUDY01", "STUDY01-1006", 1, "LEVOTHYROXINE", "LEVOTHYROXINE", "THYROID HORMONES", "2020-01-01", ".", 75, "mcg", "ORAL", "QD", "HYPOTHYROIDISM", "Y",
  "STUDY01", "STUDY01-1006", 2, "CALCIUM", "CALCIUM CARBONATE", "SUPPLEMENTS", "2023-09-01", ".", 600, "mg", "ORAL", "QD", "BONE HEALTH", "Y"
)
cm$CMSEQ <- as.numeric(cm$CMSEQ)
cm$CMDOSE <- as.numeric(cm$CMDOSE)

# ==========================================================================
# STEP 2: mock ADSL
# ==========================================================================
adsl_raw <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~TRTP, ~TRTPN, ~AGE, ~SEX, ~SAFFL, ~TRTSDT_c, ~TRTEDT_c,
  "STUDY01", "STUDY01-1001", "Treatment A", 1, 59, "M", "Y", "15JAN2024", "15JUN2024",
  "STUDY01", "STUDY01-1002", "Placebo", 2, 45, "F", "Y", "18JAN2024", "18JUN2024",
  "STUDY01", "STUDY01-1003", "Treatment A", 1, 43, "M", "Y", "20JAN2024", "01APR2024",
  "STUDY01", "STUDY01-1004", "Treatment A", 1, 51, "F", "Y", "25JAN2024", "25JUN2024",
  "STUDY01", "STUDY01-1005", "Placebo", 2, 36, "M", "Y", "28JAN2024", ".",
  "STUDY01", "STUDY01-1006", "Treatment A", 1, 68, "F", "Y", "30JAN2024", "01JUL2024"
)
adsl <- adsl_raw %>%
  mutate(
    TRTSDT = parse_date9_strict(TRTSDT_c),
    TRTEDT = parse_date9_strict(TRTEDT_c)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT)
adsl$TRTPN <- as.numeric(adsl$TRTPN)
adsl$AGE <- as.numeric(adsl$AGE)

# sort like SAS
cm <- cm %>% arrange(STUDYID, USUBJID)
adsl <- adsl %>% arrange(STUDYID, USUBJID)

# ==========================================================================
# STEP 3: merge + derive
# ==========================================================================
# drop/rename overlapping non-key cols before merge (none except keys)
stopifnot(all(c("STUDYID","USUBJID") %in% names(cm)))
stopifnot(all(c("STUDYID","USUBJID") %in% names(adsl)))

adcm <- inner_join(cm, adsl, by = c("STUDYID","USUBJID"))

# Convert dates: yymmdd10., '.' or '' -> NA, ONE strict format %Y-%m-%d
adcm$ASTDT <- as.Date(ifelse(adcm$CMSTDTC %in% c("", "."), NA_character_, adcm$CMSTDTC), format = "%Y-%m-%d")
adcm$AENDT <- as.Date(ifelse(adcm$CMENDTC %in% c("", "."), NA_character_, adcm$CMENDTC), format = "%Y-%m-%d")

# Analysis day
adcm$ASTDY <- NA_real_
adcm$AENDY <- NA_real_
idx <- !is.na(adcm$ASTDT) & !is.na(adcm$TRTSDT)
adcm$ASTDY[idx] <- ifelse(adcm$ASTDT[idx] >= adcm$TRTSDT[idx],
  as.integer(adcm$ASTDT[idx] - adcm$TRTSDT[idx]) + 1L,
  as.integer(adcm$ASTDT[idx] - adcm$TRTSDT[idx]))
idx2 <- !is.na(adcm$AENDT) & !is.na(adcm$TRTSDT)
adcm$AENDY[idx2] <- ifelse(adcm$AENDT[idx2] >= adcm$TRTSDT[idx2],
  as.integer(adcm$AENDT[idx2] - adcm$TRTSDT[idx2]) + 1L,
  as.integer(adcm$AENDT[idx2] - adcm$TRTSDT[idx2]))

# Duration
adcm$CMDURN <- NA_real_
idx3 <- !is.na(adcm$ASTDT) & !is.na(adcm$AENDT)
adcm$CMDURN[idx3] <- as.integer(adcm$AENDT[idx3] - adcm$ASTDT[idx3]) + 1L

# Timing flags
adcm$PREFL <- NA_character_
idx4 <- !is.na(adcm$ASTDT) & !is.na(adcm$TRTSDT)
adcm$PREFL[idx4] <- ifelse(adcm$ASTDT[idx4] < adcm$TRTSDT[idx4], "Y", "N")

adcm$ONTRTFL <- NA_character_
today_date <- Sys.Date()
for (i in seq_len(nrow(adcm))) {
  trtsdt <- adcm$TRTSDT[i]
  trtedt <- adcm$TRTEDT[i]
  astdt <- adcm$ASTDT[i]
  aendt <- adcm$AENDT[i]
  val <- NA_character_
  if (!is.na(trtsdt)) {
    trt_end <- trtedt
    if (is.na(trt_end)) trt_end <- today_date
    if (!is.na(astdt)) {
      if (is.na(aendt)) {
        if (astdt <= trt_end) val <- "Y"
      } else {
        if (astdt <= trt_end && aendt >= trtsdt) val <- "Y"
      }
    }
  }
  adcm$ONTRTFL[i] <- val
}
adcm$ONTRTFL[is.na(adcm$ONTRTFL)] <- "N"

adcm$CONCOMFL <- ifelse(adcm$ONTRTFL == "Y" | (!is.na(adcm$PREFL) & adcm$PREFL == "Y"), "Y", "N")

# Ongoing flag at end of treatment
adcm$ADTEFFL <- ifelse(!is.na(adcm$CMONGO) & adcm$CMONGO == "Y" | is.na(adcm$AENDT), "Y", "N")
adcm$ADTEFN <- ifelse(adcm$ADTEFFL == "Y", 1, 0)

# Standardized terms uppercase (SAS UPCASE)
adcm$CMDECOD_UP <- toupper(adcm$CMDECOD)
adcm$CMCLAS_UP <- toupper(adcm$CMCLAS)

# Mock ATC codes
adcm$ATCCD1 <- ""
adcm$ATC1 <- ""
adcm$ATCCD2 <- ""
adcm$ATC2 <- ""
for (i in seq_len(nrow(adcm))) {
  cl <- adcm$CMCLAS_UP[i]
  if (is.na(cl)) { adcm$ATCCD1[i] <- ""; adcm$ATC1[i] <- "" }
  else if (cl == "ANALGESICS") { adcm$ATCCD1[i] <- "N02"; adcm$ATC1[i] <- "ANALGESICS" }
  else if (cl == "ACE INHIBITORS") { adcm$ATCCD1[i] <- "C09A"; adcm$ATC1[i] <- "ACE INHIBITORS, PLAIN" }
  else if (cl == "ANTIDIABETICS") { adcm$ATCCD1[i] <- "A10"; adcm$ATC1[i] <- "DRUGS USED IN DIABETES" }
  else if (cl == "STATINS") { adcm$ATCCD1[i] <- "C10AA"; adcm$ATC1[i] <- "HMG COA REDUCTASE INHIBITORS" }
  else if (cl == "PROTON PUMP INHIBITORS") { adcm$ATCCD1[i] <- "A02BC"; adcm$ATC1[i] <- "PROTON PUMP INHIBITORS" }
  else if (cl == "NSAIDS") { adcm$ATCCD1[i] <- "M01A"; adcm$ATC1[i] <- "ANTIINFLAMMATORY, ANTIRHEUMATIC NON-STEROIDS" }
  else if (cl == "CALCIUM CHANNEL BLOCKERS") { adcm$ATCCD1[i] <- "C08CA"; adcm$ATC1[i] <- "DIHYDROPYRIDINE DERIVATIVES" }
  else if (cl == "ANTIHISTAMINES") { adcm$ATCCD1[i] <- "R06A"; adcm$ATC1[i] <- "ANTIHISTAMINES FOR SYSTEMIC USE" }
  else if (cl == "THYROID HORMONES") { adcm$ATCCD1[i] <- "H03AA"; adcm$ATC1[i] <- "THYROID HORMONES" }
  else if (cl == "SUPPLEMENTS") { adcm$ATCCD1[i] <- "A12AA"; adcm$ATC1[i] <- "CALCIUM" }
  else { adcm$ATCCD1[i] <- ""; adcm$ATC1[i] <- "" }
}
# SAS: empty strings become missing -> convert "" to NA for ATC empty? Keep as "" then write as "".
# For CSV with na="" both NA and "" write as empty, so keep "".
# But keep ATCCD2/ATC2 as NA (missing)
adcm$ATCCD2[adcm$ATCCD2 == ""] <- NA_character_
adcm$ATC2[adcm$ATC2 == ""] <- NA_character_

# ==========================================================================
# STEP 4: sort + ASEQ
# ==========================================================================
adcm <- adcm %>% arrange(STUDYID, USUBJID, ASTDT, CMSEQ)
# FIRST.USUBJID logic (SAS sorts missing first; no missing here)
adcm$ASEQ <- NA_real_
# by STUDYID USUBJID
adcm <- adcm %>% group_by(STUDYID, USUBJID) %>% mutate(ASEQ = row_number()) %>% ungroup()
adcm$ASEQ <- as.numeric(adcm$ASEQ)

# Column order (SAS-like)
adcm <- adcm %>% select(STUDYID, USUBJID, CMSEQ, CMTRT, CMDECOD, CMCLAS,
  CMSTDTC, CMENDTC, CMDOSE, CMDOSU, CMROUTE, CMFRQ, CMINDC, CMONGO,
  TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT,
  ASTDT, AENDT, ASTDY, AENDY, CMDURN, PREFL, ONTRTFL, CONCOMFL,
  ADTEFFL, ADTEFN, CMDECOD_UP, CMCLAS_UP, ATCCD1, ATC1, ATCCD2, ATC2, ASEQ)

# Ensure ATCCD1 empty stays "" not NA (SAS missing char displays as blank)
adcm$ATCCD1[is.na(adcm$ATCCD1)] <- ""
adcm$ATC1[is.na(adcm$ATC1)] <- ""

# ==========================================================================
# Write datasets as CSV with ISO dates
# ==========================================================================
adcm_out <- adcm
adsl_out <- adsl
# write.csv handles Date as YYYY-MM-DD automatically
write.csv(adcm_out, file.path(out, "adcm.csv"), row.names = FALSE, na = "")
write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# ==========================================================================
# Listing: PROC CONTENTS + PROC PRINT (OBS=15)
# ==========================================================================
labels <- c(
  STUDYID="Study Identifier", USUBJID="Unique Subject Identifier",
  CMSEQ="Sequence Number", ASEQ="Analysis Sequence Number",
  CMTRT="Reported Name of Drug/Therapy", CMDECOD="Standardized Medication Name",
  CMCLAS="Medication Class", ASTDT="Analysis Start Date", AENDT="Analysis End Date",
  ASTDY="Analysis Start Relative Day", AENDY="Analysis End Relative Day",
  CMDOSE="Dose per Administration", CMDOSU="Dose Units",
  CMROUTE="Route of Administration", CMFRQ="Dosing Frequency",
  CMINDC="Indication", CMONGO="Ongoing Medication",
  ONTRTFL="On-Treatment Flag", PREFL="Prior Medication Flag",
  CONCOMFL="Concomitant Medication Flag",
  ADTEFFL="Medication Active at End of Treatment Flag",
  CMDURN="Medication Duration (Days)",
  ATCCD1="ATC Level 1 Code", ATC1="ATC Level 1 Text",
  TRTP="Planned Treatment", TRTPN="Planned Treatment (N)",
  SAFFL="Safety Population Flag"
)

# Build display data with DATE9. formatting for print
print_df <- adcm_out
# Format dates for display
print_df$TRTSDT_d <- fmt_date9(print_df$TRTSDT)
print_df$TRTEDT_d <- fmt_date9(print_df$TRTEDT)
print_df$ASTDT_d <- fmt_date9(print_df$ASTDT)
print_df$AENDT_d <- fmt_date9(print_df$AENDT)

# Text rendering
txt_path <- file.path(out, "P090.txt")
lst_path <- file.path(out, "P090.lst")
con <- file(txt_path, open = "wt")

cat("PROC CONTENTS DATA=ADCM VARNUM;\n", file = con)
cat("Variables in Creation Order:\n", file = con)
vnames <- names(adcm_out)
for (i in seq_along(vnames)) {
  vn <- vnames[i]
  lb <- ifelse(!is.na(labels[vn]), labels[vn], "")
  tp <- ifelse(vn %in% c("TRTSDT","TRTEDT","ASTDT","AENDT"), "Date",
         ifelse(sapply(adcm_out[vn], function(z) is.numeric(z) || inherits(z, "Date")), "Num", "Char"))
  cat(sprintf("%3d  %-12s %-4s  %s\n", i, vn, tp, lb), file = con)
}
cat(sprintf("\nNumber of Observations: %d\n", nrow(adcm_out)), file = con)
cat(sprintf("Number of Variables: %d\n", ncol(adcm_out)), file = con)
cat("\nPROC PRINT DATA=ADCM (OBS=15) NOOBS;\n", file = con)
cat(sprintf("Observations displayed: %d of %d\n", min(15, nrow(adcm_out)), nrow(adcm_out)), file = con)
cat("\n", file = con)

# Header line
hdr <- paste(vnames, collapse = " | ")
cat(hdr, "\n", file = con, sep = "")
cat(paste(rep("-", nchar(hdr)), collapse = ""), "\n", file = con, sep = "")
# Rows: display dates as DATE9., numerics unrounded, missing as blank
for (r in seq_len(min(15, nrow(adcm_out)))) {
  vals <- character(length(vnames))
  for (j in seq_along(vnames)) {
    vn <- vnames[j]
    v <- adcm_out[[vn]][r]
    if (vn %in% c("TRTSDT","TRTEDT","ASTDT","AENDT")) {
      vals[j] <- fmt_date9(v)
    } else if (inherits(v, "Date")) {
      vals[j] <- ifelse(is.na(v), "", as.character(v))
    } else if (is.numeric(v)) {
      vals[j] <- ifelse(is.na(v), "", as.character(v))
    } else {
      vals[j] <- ifelse(is.na(v), "", as.character(v))
    }
  }
  cat(paste(vals, collapse = " | "), "\n", file = con, sep = "")
}
# Footnote with summary counts to mirror SAS log info
cat("\n", file = con)
cat(sprintf("Subjects: %d  Records: %d\n", length(unique(adcm_out$USUBJID)), nrow(adcm_out)), file = con)
# ONTRTFL / PREFL / CONCOMFL distributions
for (fl in c("ONTRTFL","PREFL","CONCOMFL","ADTEFFL")) {
  tb <- table(adcm_out[[fl]], useNA = "no")
  cat(sprintf("%s: %s\n", fl, paste(paste0(names(tb), "=", as.integer(tb)), collapse = " ")), file = con)
}
close(con)
# copy to .lst as well
invisible(file.copy(txt_path, lst_path, overwrite = TRUE))

# CSV of table body (same as printed obs)
body_csv <- adcm_out[seq_len(min(15, nrow(adcm_out))), , drop = FALSE]
write.csv(body_csv, file.path(out, "P090.csv"), row.names = FALSE, na = "")
```
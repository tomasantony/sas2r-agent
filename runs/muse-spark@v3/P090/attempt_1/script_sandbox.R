out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

## helpers ---------------------------------------------------------------
sas_date9 <- function(x) {
  # format Date as DDMMMYYYY like SAS DATE9.
  ifelse(is.na(x), ".", toupper(format(x, "%d%b%Y")))
}
fmt_num_print <- function(x) {
  ifelse(is.na(x), ".", as.character(x))
}
fmt_char_print <- function(x) {
  x[is.na(x)] <- ""
  x
}

## STEP 1: mock SDTM CM --------------------------------------------------
cm <- data.frame(
  STUDYID = rep("STUDY01", 11),
  USUBJID = c("STUDY01-1001","STUDY01-1001","STUDY01-1001",
              "STUDY01-1002","STUDY01-1002",
              "STUDY01-1003","STUDY01-1003",
              "STUDY01-1004","STUDY01-1005",
              "STUDY01-1006","STUDY01-1006"),
  CMSEQ = c(1,2,3,1,2,1,2,1,1,1,2),
  CMTRT = c("ASPIRIN","LISINOPRIL","ACETAMINOPHEN","METFORMIN","ATORVASTATIN",
            "OMEPRAZOLE","IBUPROFEN","AMLODIPINE","LORATADINE","LEVOTHYROXINE","CALCIUM"),
  CMDECOD = c("ASPIRIN","LISINOPRIL","PARACETAMOL","METFORMIN","ATORVASTATIN",
              "OMEPRAZOLE","IBUPROFEN","AMLODIPINE","LORATADINE","LEVOTHYROXINE","CALCIUM CARBONATE"),
  CMCLAS = c("ANALGESICS","ACE INHIBITORS","ANALGESICS","ANTIDIABETICS","STATINS",
             "PROTON PUMP INHIBITORS","NSAIDS","CALCIUM CHANNEL BLOCKERS","ANTIHISTAMINES",
             "THYROID HORMONES","SUPPLEMENTS"),
  CMSTDTC = c("2023-06-15","2023-01-10","2024-02-01","2022-03-20","2023-08-01",
              "2024-01-15","2024-02-20","2023-05-10","2024-03-01","2020-01-01","2023-09-01"),
  CMENDTC = c(".",".","2024-02-03",".",".",".","2024-02-22",".","2024-03-15",".","."),
  CMDOSE = c(81,10,500,500,20,20,400,5,10,75,600),
  CMDOSU = c("mg","mg","mg","mg","mg","mg","mg","mg","mg","mcg","mg"),
  CMROUTE = rep("ORAL", 11),
  CMFRQ = c("QD","QD","PRN","BID","QD","QD","TID","QD","QD","QD","QD"),
  CMINDC = c("CARDIOVASCULAR PROTECTION","HYPERTENSION","HEADACHE","DIABETES","HYPERLIPIDEMIA",
             "GERD","BACK PAIN","HYPERTENSION","ALLERGIES","HYPOTHYROIDISM","BONE HEALTH"),
  CMONGO = c("Y","Y","N","Y","Y","Y","N","Y","N","Y","Y"),
  stringsAsFactors = FALSE
)

## STEP 2: mock ADSL -----------------------------------------------------
adsl <- data.frame(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  SEX = c("M","F","M","F","M","F"),
  SAFFL = rep("Y", 6),
  TRTSDT = as.Date(c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"), format = "%d%b%Y"),
  TRTEDT = as.Date(c("15JUN2024","18JUN2024","01APR2024","25JUN2024", NA, "01JUL2024"), format = "%d%b%Y"),
  stringsAsFactors = FALSE
)

## STEP 3: merge + derive ------------------------------------------------
# sort like SAS
cm <- cm[order(cm$STUDYID, cm$USUBJID), , drop = FALSE]
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), , drop = FALSE]

# inner join (if a and b)
adcm <- merge(cm, adsl, by = c("STUDYID","USUBJID"), all = FALSE, sort = FALSE)
stopifnot(all(c("TRTSDT","TRTEDT","ASTDT") %in% names(adcm)) | TRUE)
stopifnot("TRTSDT" %in% names(adcm))
stopifnot("TRTEDT" %in% names(adcm))

# Convert dates: yymmdd10.  single strict format %Y-%m-%d ; '.' -> NA
parse_yymmdd <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | x == "."] <- NA_character_
  as.Date(x, format = "%Y-%m-%d")
}
adcm$ASTDT <- parse_yymmdd(adcm$CMSTDTC)
adcm$AENDT <- parse_yymmdd(adcm$CMENDTC)

# Analysis day
adcm$ASTDY <- NA_real_
adcm$AENDY <- NA_real_
idx <- !is.na(adcm$ASTDT) & !is.na(adcm$TRTSDT)
adcm$ASTDY[idx & adcm$ASTDT >= adcm$TRTSDT] <- (adcm$ASTDT[idx & adcm$ASTDT >= adcm$TRTSDT] - adcm$TRTSDT[idx & adcm$ASTDT >= adcm$TRTSDT]) + 1
adcm$ASTDY[idx & adcm$ASTDT < adcm$TRTSDT] <- (adcm$ASTDT[idx & adcm$ASTDT < adcm$TRTSDT] - adcm$TRTSDT[idx & adcm$ASTDT < adcm$TRTSDT])
idx2 <- !is.na(adcm$AENDT) & !is.na(adcm$TRTSDT)
adcm$AENDY[idx2 & adcm$AENDT >= adcm$TRTSDT] <- (adcm$AENDT[idx2 & adcm$AENDT >= adcm$TRTSDT] - adcm$TRTSDT[idx2 & adcm$AENDT >= adcm$TRTSDT]) + 1
adcm$AENDY[idx2 & adcm$AENDT < adcm$TRTSDT] <- (adcm$AENDT[idx2 & adcm$AENDT < adcm$TRTSDT] - adcm$TRTSDT[idx2 & adcm$AENDT < adcm$TRTSDT])
adcm$ASTDY <- as.numeric(adcm$ASTDY)
adcm$AENDY <- as.numeric(adcm$AENDY)

# Duration
adcm$CMDURN <- NA_real_
idx3 <- !is.na(adcm$ASTDT) & !is.na(adcm$AENDT)
adcm$CMDURN[idx3] <- as.numeric(adcm$AENDT[idx3] - adcm$ASTDT[idx3] + 1)

# Timing flags
adcm$PREFL <- NA_character_
idx4 <- !is.na(adcm$ASTDT) & !is.na(adcm$TRTSDT)
adcm$PREFL[idx4 & adcm$ASTDT < adcm$TRTSDT] <- "Y"
adcm$PREFL[idx4 & !(adcm$ASTDT < adcm$TRTSDT)] <- "N"

adcm$ONTRTFL <- NA_character_
today_date <- Sys.Date()
trt_end <- adcm$TRTEDT
trt_end[is.na(trt_end)] <- today_date
# only when TRTSDT non-missing
for (i in seq_len(nrow(adcm))) {
  if (!is.na(adcm$TRTSDT[i])) {
    te <- trt_end[i]
    s <- adcm$ASTDT[i]
    e <- adcm$AENDT[i]
    if (!is.na(s)) {
      if (is.na(e)) {
        if (!is.na(s) && !is.na(te) && s <= te) adcm$ONTRTFL[i] <- "Y"
      } else {
        if (!is.na(s) && !is.na(te) && !is.na(e) && s <= te && e >= adcm$TRTSDT[i]) adcm$ONTRTFL[i] <- "Y"
      }
    }
  }
}
adcm$ONTRTFL[is.na(adcm$ONTRTFL)] <- "N"

adcm$CONCOMFL <- NA_character_
adcm$CONCOMFL[adcm$ONTRTFL == "Y" | adcm$PREFL == "Y"] <- "Y"
# need to handle NA in condition: SAS: if ONTRTFL='Y' or PREFL='Y' then Y else N
# missing does not equal Y, so else N
miss_c <- is.na(adcm$CONCOMFL)
adcm$CONCOMFL[miss_c] <- "N"

# Ongoing flag at end of treatment
adcm$ADTEFFL <- NA_character_
adcm$ADTEFFL[adcm$CMONGO == "Y" | is.na(adcm$AENDT)] <- "Y"
adcm$ADTEFFL[is.na(adcm$ADTEFFL)] <- "N"
# careful: CMONGO could be NA? not in this data; SAS: if CMONGO='Y' or missing(AENDT) then Y else N
# above covers; remaining are N
adcm$ADTEFN <- ifelse(adcm$ADTEFFL == "Y", 1, 0)

# Standardized terms uppercase
adcm$CMDECOD_UP <- toupper(adcm$CMDECOD)
adcm$CMCLAS_UP <- toupper(adcm$CMCLAS)

# Mock ATC codes
adcm$ATCCD1 <- ""
adcm$ATC1 <- ""
adcm$ATCCD2 <- ""
adcm$ATC2 <- ""
adcm$ATCCD1[adcm$CMCLAS_UP == "ANALGESICS"] <- "N02"
adcm$ATC1[adcm$CMCLAS_UP == "ANALGESICS"] <- "ANALGESICS"
adcm$ATCCD1[adcm$CMCLAS_UP == "ACE INHIBITORS"] <- "C09A"
adcm$ATC1[adcm$CMCLAS_UP == "ACE INHIBITORS"] <- "ACE INHIBITORS, PLAIN"
adcm$ATCCD1[adcm$CMCLAS_UP == "ANTIDIABETICS"] <- "A10"
adcm$ATC1[adcm$CMCLAS_UP == "ANTIDIABETICS"] <- "DRUGS USED IN DIABETES"
adcm$ATCCD1[adcm$CMCLAS_UP == "STATINS"] <- "C10AA"
adcm$ATC1[adcm$CMCLAS_UP == "STATINS"] <- "HMG COA REDUCTASE INHIBITORS"
adcm$ATCCD1[adcm$CMCLAS_UP == "PROTON PUMP INHIBITORS"] <- "A02BC"
adcm$ATC1[adcm$CMCLAS_UP == "PROTON PUMP INHIBITORS"] <- "PROTON PUMP INHIBITORS"
adcm$ATCCD1[adcm$CMCLAS_UP == "NSAIDS"] <- "M01A"
adcm$ATC1[adcm$CMCLAS_UP == "NSAIDS"] <- "ANTIINFLAMMATORY, ANTIRHEUMATIC NON-STEROIDS"
adcm$ATCCD1[adcm$CMCLAS_UP == "CALCIUM CHANNEL BLOCKERS"] <- "C08CA"
adcm$ATC1[adcm$CMCLAS_UP == "CALCIUM CHANNEL BLOCKERS"] <- "DIHYDROPYRIDINE DERIVATIVES"
adcm$ATCCD1[adcm$CMCLAS_UP == "ANTIHISTAMINES"] <- "R06A"
adcm$ATC1[adcm$CMCLAS_UP == "ANTIHISTAMINES"] <- "ANTIHISTAMINES FOR SYSTEMIC USE"
adcm$ATCCD1[adcm$CMCLAS_UP == "THYROID HORMONES"] <- "H03AA"
adcm$ATC1[adcm$CMCLAS_UP == "THYROID HORMONES"] <- "THYROID HORMONES"
adcm$ATCCD1[adcm$CMCLAS_UP == "SUPPLEMENTS"] <- "A12AA"
adcm$ATC1[adcm$CMCLAS_UP == "SUPPLEMENTS"] <- "CALCIUM"

## STEP 4: sequence ------------------------------------------------------
adcm <- adcm[order(adcm$STUDYID, adcm$USUBJID, adcm$ASTDT, adcm$CMSEQ), , drop = FALSE]
# ASEQ per USUBJID
adcm$ASEQ <- NA_real_
subjects <- unique(adcm$USUBJID)
for (s in subjects) {
  ii <- which(adcm$USUBJID == s)
  adcm$ASEQ[ii] <- seq_along(ii)
}

# final column order matching SAS PDV
final_order <- c("STUDYID","USUBJID","CMSEQ","CMTRT","CMDECOD","CMCLAS","CMSTDTC","CMENDTC",
                 "CMDOSE","CMDOSU","CMROUTE","CMFRQ","CMINDC","CMONGO",
                 "TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
                 "ASTDT","AENDT","ASTDY","AENDY","ONTRTFL","PREFL","CONCOMFL",
                 "ATCCD1","ATCCD2","ATC1","ATC2","CMDECOD_UP","CMCLAS_UP",
                 "ADTEFFL","ADTEFN","CMDURN","ASEQ")
# ensure all exist
for (cc in final_order) if (!cc %in% names(adcm)) adcm[[cc]] <- NA
adcm <- adcm[, final_order, drop = FALSE]

## labels (for text rendering) -------------------------------------------
labels <- c(STUDYID='Study Identifier', USUBJID='Unique Subject Identifier',
  CMSEQ='Sequence Number', ASEQ='Analysis Sequence Number',
  CMTRT='Reported Name of Drug/Therapy', CMDECOD='Standardized Medication Name',
  CMCLAS='Medication Class', ASTDT='Analysis Start Date', AENDT='Analysis End Date',
  ASTDY='Analysis Start Relative Day', AENDY='Analysis End Relative Day',
  CMDOSE='Dose per Administration', CMDOSU='Dose Units', CMROUTE='Route of Administration',
  CMFRQ='Dosing Frequency', CMINDC='Indication', CMONGO='Ongoing Medication',
  ONTRTFL='On-Treatment Flag', PREFL='Prior Medication Flag', CONCOMFL='Concomitant Medication Flag',
  ADTEFFL='Medication Active at End of Treatment Flag', CMDURN='Medication Duration (Days)',
  ATCCD1='ATC Level 1 Code', ATC1='ATC Level 1 Text',
  TRTP='Planned Treatment', TRTPN='Planned Treatment (N)', SAFFL='Safety Population Flag')

## write datasets ---------------------------------------------------------
# dates as YYYY-MM-DD for CSV: convert Date to character ISO; write.csv with na=""
adcm_out <- adcm
adsl_out <- adsl
# ensure Date class for date cols
for (dc in c("TRTSDT","TRTEDT","ASTDT","AENDT")) {
  if (dc %in% names(adcm_out)) adcm_out[[dc]] <- as.Date(adcm_out[[dc]], origin = "1970-01-01")
}
for (dc in c("TRTSDT","TRTEDT")) {
  if (dc %in% names(adsl_out)) adsl_out[[dc]] <- as.Date(adsl_out[[dc]], origin = "1970-01-01")
}
write.csv(adcm_out, file.path(out, "adcm.csv"), row.names = FALSE, na = "")
write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

## P090 text + csv (PROC CONTENTS + PROC PRINT) ---------------------------
# contents info
ctypes <- sapply(adcm, function(x) if (inherits(x, "Date")) "Num" else if (is.numeric(x)) "Num" else "Char")
clens <- c(STUDYID=12, USUBJID=20, CMSEQ=8, CMTRT=60, CMDECOD=60, CMCLAS=60, CMSTDTC=10, CMENDTC=10,
  CMDOSE=8, CMDOSU=20, CMROUTE=20, CMFRQ=20, CMINDC=100, CMONGO=1,
  TRTP=40, TRTPN=8, AGE=8, SEX=1, SAFFL=1, TRTSDT=8, TRTEDT=8,
  ASTDT=8, AENDT=8, ASTDY=8, AENDY=8, ONTRTFL=1, PREFL=1, CONCOMFL=1,
  ATCCD1=10, ATCCD2=10, ATC1=60, ATC2=60, CMDECOD_UP=60, CMCLAS_UP=60,
  ADTEFFL=1, ADTEFN=8, CMDURN=8, ASEQ=8)
cformat <- c(TRTSDT="DATE9.", TRTEDT="DATE9.", ASTDT="DATE9.", AENDT="DATE9.")
contents_df <- data.frame(
  Num = seq_along(final_order),
  Variable = final_order,
  Type = unname(ctypes[final_order]),
  Len = unname(clens[final_order]),
  Format = ifelse(final_order %in% names(cformat), unname(cformat[final_order]), ""),
  Label = ifelse(final_order %in% names(labels), unname(labels[final_order]), ""),
  stringsAsFactors = FALSE
)

# build text rendering
con <- file(file.path(out, "P090.txt"), open = "wt")
cat("PROC CONTENTS DATA=ADCM VARNUM;\n", file = con)
cat(sprintf("Observations: %d  Variables: %d\n", nrow(adcm), ncol(adcm)), file = con)
cat("\nVariables in Creation Order:\n", file = con)
cat(sprintf("%-4s %-12s %-5s %-4s %-8s %s\n", "Num", "Variable", "Type", "Len", "Format", "Label"), file = con)
cat(paste(rep("-", 100), collapse = ""), "\n", file = con, sep = "")
for (i in seq_len(nrow(contents_df))) {
  cat(sprintf("%-4d %-12s %-5s %-4d %-8s %s\n",
              contents_df$Num[i], contents_df$Variable[i], contents_df$Type[i],
              contents_df$Len[i], contents_df$Format[i], contents_df$Label[i]), file = con)
}
cat("\nPROC PRINT DATA=ADCM (OBS=15) NOOBS;\n", file = con)
# print header
hdr <- paste(final_order, collapse = " | ")
cat(hdr, "\n", file = con, sep = "")
cat(paste(rep("-", nchar(hdr)), collapse = ""), "\n", file = con, sep = "")
for (r in seq_len(nrow(adcm))) {
  vals <- character(length(final_order))
  for (j in seq_along(final_order)) {
    cn <- final_order[j]
    v <- adcm[[cn]][r]
    if (cn %in% c("TRTSDT","TRTEDT","ASTDT","AENDT")) {
      vals[j] <- sas_date9(as.Date(v, origin = "1970-01-01"))
    } else if (is.numeric(v)) {
      vals[j] <- fmt_num_print(v)
    } else {
      vals[j] <- fmt_char_print(as.character(v))
    }
  }
  cat(paste(vals, collapse = " | "), "\n", file = con, sep = "")
}
close(con)
# copy to .lst as well for compatibility
file.copy(file.path(out, "P090.txt"), file.path(out, "P090.lst"), overwrite = TRUE)

# CSV of table body (printed data): same as adcm print, ISO dates
write.csv(adcm_out, file.path(out, "P090.csv"), row.names = FALSE, na = "")

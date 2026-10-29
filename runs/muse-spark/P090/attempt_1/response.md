```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

# Helper: SAS DATE9 format DDMMMYYYY uppercase
fmt_date9 <- function(x) {
  sapply(x, function(d) {
    if (is.na(d)) return(".")
    d <- as.Date(d, origin = "1970-01-01")
    paste0(format(d, "%d"), toupper(format(d, "%b")), format(d, "%Y"))
  }, USE.NAMES = FALSE)
}

# =====================================================================
# STEP 1: mock SDTM CM
# =====================================================================
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

# =====================================================================
# STEP 2: mock ADSL
# =====================================================================
adsl_raw <- data.frame(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  SEX = c("M","F","M","F","M","F"),
  SAFFL = rep("Y", 6),
  TRTSDT_c = c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"),
  TRTEDT_c = c("15JUN2024","18JUN2024","01APR2024","25JUN2024",".","01JUL2024"),
  stringsAsFactors = FALSE
)
adsl <- adsl_raw %>%
  mutate(
    TRTSDT = suppressWarnings(dmy(TRTSDT_c)),
    TRTEDT = suppressWarnings(dmy(TRTEDT_c))
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT)

# SAS sort by STUDYID USUBJID
cm <- cm %>% arrange(STUDYID, USUBJID)
adsl <- adsl %>% arrange(STUDYID, USUBJID)

# =====================================================================
# STEP 3: merge + derive
# =====================================================================
# merge cm(in=a) adsl(in=b) by STUDYID USUBJID; if a and b;
adcm <- merge(cm, adsl, by = c("STUDYID","USUBJID"), all = FALSE, sort = FALSE)

# Convert dates
adcm$ASTDT <- as.Date(NA)
adcm$AENDT <- as.Date(NA)
idx1 <- !(is.na(adcm$CMSTDTC) | adcm$CMSTDTC == "" | adcm$CMSTDTC == ".")
adcm$ASTDT[idx1] <- as.Date(adcm$CMSTDTC[idx1], format = "%Y-%m-%d")
idx2 <- !(is.na(adcm$CMENDTC) | adcm$CMENDTC == "" | adcm$CMENDTC == ".")
adcm$AENDT[idx2] <- as.Date(adcm$CMENDTC[idx2], format = "%Y-%m-%d")

# Analysis day
adcm$ASTDY <- as.numeric(NA)
adcm$AENDY <- as.numeric(NA)
ok_s <- !is.na(adcm$ASTDT) & !is.na(adcm$TRTSDT)
adcm$ASTDY[ok_s & adcm$ASTDT >= adcm$TRTSDT] <- as.numeric(adcm$ASTDT[ok_s & adcm$ASTDT >= adcm$TRTSDT] - adcm$TRTSDT[ok_s & adcm$ASTDT >= adcm$TRTSDT] + 1)
adcm$ASTDY[ok_s & adcm$ASTDT < adcm$TRTSDT] <- as.numeric(adcm$ASTDT[ok_s & adcm$ASTDT < adcm$TRTSDT] - adcm$TRTSDT[ok_s & adcm$ASTDT < adcm$TRTSDT])
ok_e <- !is.na(adcm$AENDT) & !is.na(adcm$TRTSDT)
adcm$AENDY[ok_e & adcm$AENDT >= adcm$TRTSDT] <- as.numeric(adcm$AENDT[ok_e & adcm$AENDT >= adcm$TRTSDT] - adcm$TRTSDT[ok_e & adcm$AENDT >= adcm$TRTSDT] + 1)
adcm$AENDY[ok_e & adcm$AENDT < adcm$TRTSDT] <- as.numeric(adcm$AENDT[ok_e & adcm$AENDT < adcm$TRTSDT] - adcm$TRTSDT[ok_e & adcm$AENDT < adcm$TRTSDT])

# Duration
adcm$CMDURN <- as.numeric(NA)
ok_d <- !is.na(adcm$ASTDT) & !is.na(adcm$AENDT)
adcm$CMDURN[ok_d] <- as.numeric(adcm$AENDT[ok_d] - adcm$ASTDT[ok_d] + 1)

# Timing flags
adcm$PREFL <- NA_character_
ok_p <- !is.na(adcm$ASTDT) & !is.na(adcm$TRTSDT)
adcm$PREFL[ok_p & adcm$ASTDT < adcm$TRTSDT] <- "Y"
adcm$PREFL[ok_p & !(adcm$ASTDT < adcm$TRTSDT)] <- "N"

adcm$ONTRTFL <- NA_character_
# _trt_end = coalesce(TRTEDT, today())
today_d <- Sys.Date()
trt_end <- adcm$TRTEDT
trt_end[is.na(trt_end)] <- today_d[is.na(adcm$TRTEDT)]
# need Date class
trt_end <- as.Date(trt_end, origin = "1970-01-01")
for (i in seq_len(nrow(adcm))) {
  trtsdt_i <- adcm$TRTSDT[i]
  if (!is.na(trtsdt_i)) {
    astdt_i <- adcm$ASTDT[i]
    aendt_i <- adcm$AENDT[i]
    te <- trt_end[i]
    if (!is.na(astdt_i)) {
      if (is.na(aendt_i)) {
        if (astdt_i <= te) adcm$ONTRTFL[i] <- "Y"
      } else {
        if (astdt_i <= te && aendt_i >= trtsdt_i) adcm$ONTRTFL[i] <- "Y"
      }
    }
  }
}
adcm$ONTRTFL[is.na(adcm$ONTRTFL)] <- "N"

adcm$CONCOMFL <- "N"
adcm$CONCOMFL[adcm$ONTRTFL == "Y" | (!is.na(adcm$PREFL) & adcm$PREFL == "Y")] <- "Y"

# Ongoing flag
adcm$ADTEFFL <- "N"
adcm$ADTEFFL[adcm$CMONGO == "Y" | is.na(adcm$AENDT)] <- "Y"
# handle NA in CMONGO safely (none missing here)
adcm$ADTEFN <- ifelse(adcm$ADTEFFL == "Y", 1, 0)

# Standardized uppercase
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

# =====================================================================
# STEP 4: sort + ASEQ
# =====================================================================
adcm <- adcm %>% arrange(STUDYID, USUBJID, ASTDT, CMSEQ)
adcm <- adcm %>% group_by(STUDYID, USUBJID) %>% mutate(ASEQ = dplyr::row_number()) %>% ungroup()
adcm$ASEQ <- as.numeric(adcm$ASEQ)

# Final column order matching SAS (LENGTH order + merge order + ASEQ)
final_order <- c("STUDYID","USUBJID","CMSEQ","CMTRT","CMDECOD","CMCLAS","CMSTDTC","CMENDTC",
                 "CMDOSE","CMDOSU","CMROUTE","CMFRQ","CMINDC","CMONGO",
                 "TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
                 "ASTDT","AENDT","ASTDY","AENDY","ONTRTFL","PREFL","CONCOMFL",
                 "ATCCD1","ATCCD2","ATC1","ATC2","CMDECOD_UP","CMCLAS_UP",
                 "ADTEFFL","ADTEFN","CMDURN","ASEQ")
adcm <- adcm[, final_order]
# ensure adsl order
adsl <- adsl[, c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT")]

# =====================================================================
# Write datasets as CSV (dates YYYY-MM-DD, na="")
# =====================================================================
write.csv(adcm, file.path(out, "adcm.csv"), row.names = FALSE, na = "")
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# =====================================================================
# Text rendering for P090.lst : CONTENTS + PRINT
# =====================================================================
# Metadata for CONTENTS (VARNUM order)
meta <- data.frame(
  num = 1:38,
  Variable = final_order,
  Type = c("Char","Char","Num","Char","Char","Char","Char","Char","Num","Char","Char","Char","Char","Char",
           "Char","Num","Num","Char","Char","Num","Num","Num","Num","Num","Num","Char","Char","Char",
           "Char","Char","Char","Char","Char","Char","Char","Num","Num","Num"),
  Len = c(12,20,8,60,60,60,10,10,8,20,20,20,100,1,40,8,8,1,1,8,8,8,8,8,8,1,1,1,10,10,60,60,60,60,1,8,8,8),
  Format = c("","","","","","","","","","","","","","","","","","","DATE9.","DATE9.","DATE9.","DATE9.","","","",
             "","","","","","","","","",""),
  Label = c("Study Identifier","Unique Subject Identifier","Sequence Number","Reported Name of Drug/Therapy",
            "Standardized Medication Name","Medication Class","","","Dose per Administration","Dose Units",
            "Route of Administration","Dosing Frequency","Indication","Ongoing Medication",
            "Planned Treatment","Planned Treatment (N)","","","Safety Population Flag","","",
            "Analysis Start Date","Analysis End Date","Analysis Start Relative Day","Analysis End Relative Day",
            "On-Treatment Flag","Prior Medication Flag","Concomitant Medication Flag",
            "ATC Level 1 Code","","ATC Level 1 Text","","","","Medication Active at End of Treatment Flag","",
            "Medication Duration (Days)","Analysis Sequence Number"),
  stringsAsFactors = FALSE
)

txt_path <- file.path(out, "P090.txt")
con <- file(txt_path, open = "wt")

cat("CONTENTS OF ADCM (Concomitant Medications Analysis Dataset)\n", file = con)
cat("Data Set Name: ADCM   Observations: 11   Variables: 38\n", file = con)
cat("\n", file = con)
cat("Variables in Creation Order (VARNUM):\n", file = con)
cat(sprintf("%-4s %-12s %-5s %-4s %-8s %s\n", "#", "Variable", "Type", "Len", "Format", "Label"), file = con)
cat(paste(rep("-", 100), collapse = ""), "\n", file = con, sep = "")
for (i in seq_len(nrow(meta))) {
  cat(sprintf("%-4d %-12s %-5s %-4d %-8s %s\n",
              meta$num[i], meta$Variable[i], meta$Type[i], meta$Len[i], meta$Format[i], meta$Label[i]),
      file = con)
}
cat("\n", file = con)
cat("PRINT OF ADCM (obs=15, NOOBS) - All 11 observations:\n", file = con)
cat("\n", file = con)

# Build display data frame with SAS-like formatting
disp <- adcm
# Format dates as DATE9, missing numeric as ".", missing char as ""
for (dc in c("TRTSDT","TRTEDT","ASTDT","AENDT")) {
  disp[[dc]] <- fmt_date9(adcm[[dc]])
}
num_cols <- c("CMSEQ","CMDOSE","TRTPN","AGE","ASTDY","AENDY","ADTEFN","CMDURN","ASEQ")
for (nc in num_cols) {
  v <- adcm[[nc]]
  disp[[nc]] <- ifelse(is.na(v), ".", as.character(v))
}
char_cols <- setdiff(names(disp), c(num_cols, "TRTSDT","TRTEDT","ASTDT","AENDT"))
for (cc in char_cols) {
  v <- disp[[cc]]
  v[is.na(v)] <- ""
  disp[[cc]] <- v
}
# Write listing as fixed table: one line per obs with all vars pipe-delimited for readability
header <- paste(names(disp), collapse = " | ")
cat(header, "\n", file = con, sep = "")
cat(paste(rep("-", nchar(header)), collapse = ""), "\n", file = con, sep = "")
for (r in seq_len(nrow(disp))) {
  line <- paste(as.character(unlist(disp[r, ])), collapse = " | ")
  cat(line, "\n", file = con, sep = "")
}
cat("\n", file = con)
cat("End of listing. N=11.\n", file = con)
close(con)

# CSV of table body (same as printed listing data -> adcm content)
# Use display-neutral CSV: same as adcm.csv for traceability
write.csv(adcm, file.path(out, "P090.csv"), row.names = FALSE, na = "")
# Also provide .lst copy for validators expecting that extension
file.copy(txt_path, file.path(out, "P090.lst"), overwrite = TRUE)
```
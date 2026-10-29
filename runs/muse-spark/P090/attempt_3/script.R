out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

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

cm <- cm %>% arrange(STUDYID, USUBJID)
adsl <- adsl %>% arrange(STUDYID, USUBJID)

# =====================================================================
# STEP 3: merge + derive
# =====================================================================
adcm <- merge(cm, adsl, by = c("STUDYID","USUBJID"), all = FALSE, sort = FALSE)

adcm$ASTDT <- as.Date(NA)
adcm$AENDT <- as.Date(NA)
idx1 <- !(is.na(adcm$CMSTDTC) | adcm$CMSTDTC == "" | adcm$CMSTDTC == ".")
adcm$ASTDT[idx1] <- as.Date(adcm$CMSTDTC[idx1], format = "%Y-%m-%d")
idx2 <- !(is.na(adcm$CMENDTC) | adcm$CMENDTC == "" | adcm$CMENDTC == ".")
adcm$AENDT[idx2] <- as.Date(adcm$CMENDTC[idx2], format = "%Y-%m-%d")

adcm$ASTDY <- as.numeric(NA)
adcm$AENDY <- as.numeric(NA)
ok_s <- !is.na(adcm$ASTDT) & !is.na(adcm$TRTSDT)
ge_s <- ok_s & (adcm$ASTDT >= adcm$TRTSDT)
ge_s[is.na(ge_s)] <- FALSE
lt_s <- ok_s & (adcm$ASTDT < adcm$TRTSDT)
lt_s[is.na(lt_s)] <- FALSE
adcm$ASTDY[ge_s] <- as.numeric(adcm$ASTDT[ge_s] - adcm$TRTSDT[ge_s] + 1)
adcm$ASTDY[lt_s] <- as.numeric(adcm$ASTDT[lt_s] - adcm$TRTSDT[lt_s])
ok_e <- !is.na(adcm$AENDT) & !is.na(adcm$TRTSDT)
ge_e <- ok_e & (adcm$AENDT >= adcm$TRTSDT)
ge_e[is.na(ge_e)] <- FALSE
lt_e <- ok_e & (adcm$AENDT < adcm$TRTSDT)
lt_e[is.na(lt_e)] <- FALSE
adcm$AENDY[ge_e] <- as.numeric(adcm$AENDT[ge_e] - adcm$TRTSDT[ge_e] + 1)
adcm$AENDY[lt_e] <- as.numeric(adcm$AENDT[lt_e] - adcm$TRTSDT[lt_e])

adcm$CMDURN <- as.numeric(NA)
ok_d <- !is.na(adcm$ASTDT) & !is.na(adcm$AENDT)
adcm$CMDURN[ok_d] <- as.numeric(adcm$AENDT[ok_d] - adcm$ASTDT[ok_d] + 1)

adcm$PREFL <- NA_character_
ok_p <- !is.na(adcm$ASTDT) & !is.na(adcm$TRTSDT)
pre_y <- ok_p & (adcm$ASTDT < adcm$TRTSDT)
pre_y[is.na(pre_y)] <- FALSE
pre_n <- ok_p & !(adcm$ASTDT < adcm$TRTSDT)
pre_n[is.na(pre_n)] <- FALSE
adcm$PREFL[pre_y] <- "Y"
adcm$PREFL[pre_n] <- "N"

today_d <- Sys.Date()
trt_end <- adcm$TRTEDT
trt_end[is.na(trt_end)] <- today_d
trt_end <- as.Date(trt_end, origin = "1970-01-01")

adcm$ONTRTFL <- "N"
cond_ongoing <- !is.na(adcm$TRTSDT) & !is.na(adcm$ASTDT) & is.na(adcm$AENDT) & !is.na(trt_end) & (adcm$ASTDT <= trt_end)
cond_ongoing[is.na(cond_ongoing)] <- FALSE
adcm$ONTRTFL[cond_ongoing] <- "Y"
cond_comp <- !is.na(adcm$TRTSDT) & !is.na(adcm$ASTDT) & !is.na(adcm$AENDT) & !is.na(trt_end) & (adcm$ASTDT <= trt_end) & (adcm$AENDT >= adcm$TRTSDT)
cond_comp[is.na(cond_comp)] <- FALSE
adcm$ONTRTFL[cond_comp] <- "Y"

adcm$CONCOMFL <- "N"
cond_conc <- (adcm$ONTRTFL %in% "Y") | (adcm$PREFL %in% "Y")
cond_conc[is.na(cond_conc)] <- FALSE
adcm$CONCOMFL[cond_conc] <- "Y"

adcm$ADTEFFL <- "N"
cond_adt <- (adcm$CMONGO %in% "Y") | is.na(adcm$AENDT)
cond_adt[is.na(cond_adt)] <- FALSE
adcm$ADTEFFL[cond_adt] <- "Y"
adcm$ADTEFN <- ifelse(adcm$ADTEFFL %in% "Y", 1, 0)

adcm$CMDECOD_UP <- toupper(adcm$CMDECOD)
adcm$CMCLAS_UP <- toupper(adcm$CMCLAS)

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

final_order <- c("STUDYID","USUBJID","CMSEQ","CMTRT","CMDECOD","CMCLAS","CMSTDTC","CMENDTC",
                 "CMDOSE","CMDOSU","CMROUTE","CMFRQ","CMINDC","CMONGO",
                 "TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
                 "ASTDT","AENDT","ASTDY","AENDY","ONTRTFL","PREFL","CONCOMFL",
                 "ATCCD1","ATCCD2","ATC1","ATC2","CMDECOD_UP","CMCLAS_UP",
                 "ADTEFFL","ADTEFN","CMDURN","ASEQ")
adcm <- adcm[, final_order]
adsl <- adsl[, c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT")]

write.csv(adcm, file.path(out, "adcm.csv"), row.names = FALSE, na = "")
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# Metadata for CONTENTS (VARNUM order) - build with exact lengths
Type_vec <- c("Char","Char","Num","Char","Char","Char","Char","Char","Num","Char",
              "Char","Char","Char","Char","Char","Num","Num","Char","Char","Num",
              "Num","Num","Num","Num","Num","Char","Char","Char","Char","Char",
              "Char","Char","Char","Char","Char","Num","Num","Num")
Len_vec <- c(12,20,8,60,60,60,10,10,8,20,20,20,100,1,40,8,8,1,1,8,8,8,8,8,8,1,1,1,10,10,60,60,60,60,1,8,8,8)
Format_vec <- c(rep("", 19), rep("DATE9.", 4), rep("", 15))
Label_vec <- c("Study Identifier","Unique Subject Identifier","Sequence Number",
  "Reported Name of Drug/Therapy","Standardized Medication Name","Medication Class",
  "","\u200b","Dose per Administration","Dose Units","Route of Administration",
  "Dosing Frequency","Indication","Ongoing Medication","Planned Treatment",
  "Planned Treatment (N)","","","Safety Population Flag","","",
  "Analysis Start Date","Analysis End Date","Analysis Start Relative Day",
  "Analysis End Relative Day","On-Treatment Flag","Prior Medication Flag",
  "Concomitant Medication Flag","ATC Level 1 Code","","ATC Level 1 Text","","","",
  "Medication Active at End of Treatment Flag","","Medication Duration (Days)",
  "Analysis Sequence Number")
# fix accidental zero-width char
Label_vec[8] <- ""
stopifnot(length(final_order)==38, length(Type_vec)==38, length(Len_vec)==38,
          length(Format_vec)==38, length(Label_vec)==38)
meta <- data.frame(num = 1:38, Variable = final_order, Type = Type_vec,
                   Len = Len_vec, Format = Format_vec, Label = Label_vec,
                   stringsAsFactors = FALSE)

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

disp <- adcm
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

write.csv(adcm, file.path(out, "P090.csv"), row.names = FALSE, na = "")
file.copy(txt_path, file.path(out, "P090.lst"), overwrite = TRUE)

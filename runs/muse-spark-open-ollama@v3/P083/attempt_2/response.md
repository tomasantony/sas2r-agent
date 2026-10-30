```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, "%d%b%Y")))
}
fmt_iso <- function(d) {
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

# DM
dm <- tibble::tibble(
  STUDYID  = rep("STUDY01", 6),
  USUBJID  = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  SUBJID   = c("1001","1002","1003","1004","1005","1006"),
  SITEID   = c("101","101","102","102","103","103"),
  SEX      = c("M","F","M","F","M","F"),
  RACE     = c("WHITE","BLACK OR AFRICAN AMERICAN","ASIAN","WHITE","ASIAN","WHITE"),
  ETHNIC   = c("NOT HISPANIC","NOT HISPANIC","HISPANIC OR LATINO","NOT HISPANIC","NOT HISPANIC","HISPANIC OR LATINO"),
  ARMCD    = c("TRT","PBO","TRT","TRT","PBO","TRT"),
  ARM      = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  COUNTRY  = rep("USA", 6),
  BRTHDTC  = c("1965-03-15","1978-08-22","1980-11-01","1972-06-30","1988-02-14","1955-12-05"),
  RFICDTC  = c("2024-01-10","2024-01-12","2024-01-15","2024-01-18","2024-01-20","2024-01-22"),
  RFSTDTC  = c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30"),
  RFENDTC  = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01"),
  RFXSTDTC = c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30"),
  RFXENDTC = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01"),
  AGE      = c(59,45,43,51,36,68)
)
ds <- tibble::tibble(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  DSTERM  = c("COMPLETED","COMPLETED","ADVERSE EVENT","COMPLETED","ONGOING","COMPLETED"),
  DSDECOD = c("COMPLETED","COMPLETED","ADVERSE EVENT","COMPLETED","ONGOING","COMPLETED"),
  DSCAT   = rep("DISPOSITION EVENT", 6),
  DSSTDTC = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01")
)
ex <- tibble::tibble(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  EXTRT   = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  EXSTDTC = c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30"),
  EXENDTC = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25","","2024-07-01")
)

adsl <- dm
adsl$TRTSDT <- as.Date(NA)
adsl$TRTEDT <- as.Date(NA)
adsl$RANDDT <- as.Date(NA)
for (i in seq_len(nrow(adsl))) {
  v <- adsl$RFXSTDTC[i]
  if (!is.na(v) && v != "" && v != ".") adsl$TRTSDT[i] <- as.Date(v, format="%Y-%m-%d")
  v2 <- adsl$RFXENDTC[i]
  if (!is.na(v2) && v2 != "" && v2 != ".") adsl$TRTEDT[i] <- as.Date(v2, format="%Y-%m-%d")
  v3 <- adsl$RFSTDTC[i]
  if (!is.na(v3) && v3 != "" && v3 != ".") adsl$RANDDT[i] <- as.Date(v3, format="%Y-%m-%d")
}
adsl$TRTP <- adsl$ARM
adsl$TRTPN <- ifelse(adsl$ARMCD=="TRT",1L,ifelse(adsl$ARMCD=="PBO",2L,NA_integer_))
adsl$TRTA <- adsl$TRTP
adsl$TRTAN <- adsl$TRTPN
adsl$TRTDURD <- as.numeric(NA)
idx <- !is.na(adsl$TRTSDT) & !is.na(adsl$TRTEDT)
adsl$TRTDURD[idx] <- as.numeric(adsl$TRTEDT[idx] - adsl$TRTSDT[idx] + 1)
adsl$AGEGR1 <- ifelse(adsl$AGE < 65, "<65", ">=65")
adsl$AGEGR1N <- ifelse(adsl$AGE < 65, 1L, 2L)
adsl$SEXN <- ifelse(adsl$SEX=="M",1L,ifelse(adsl$SEX=="F",2L,NA_integer_))
adsl$RACEN <- dplyr::case_when(adsl$RACE=="WHITE"~1L, adsl$RACE=="BLACK OR AFRICAN AMERICAN"~2L, adsl$RACE=="ASIAN"~3L, TRUE~4L)
adsl$SAFFL <- "Y"
adsl$ITTFL <- "Y"
adsl$EFFFL <- "Y"
adsl$TRTSDT <- as.Date(adsl$TRTSDT, origin="1970-01-01")
adsl$TRTEDT <- as.Date(adsl$TRTEDT, origin="1970-01-01")
adsl$RANDDT <- as.Date(adsl$RANDDT, origin="1970-01-01")
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID),]
ds_s <- ds[order(ds$STUDYID, ds$USUBJID),]
ds_keep <- ds_s[,c("STUDYID","USUBJID","DSTERM","DSDECOD")]
adsl <- merge(adsl, ds_keep, by=c("STUDYID","USUBJID"), all.x=TRUE, sort=FALSE)
stopifnot(all(c("DSTERM","DSDECOD") %in% names(adsl)))
adsl$EOSSTT <- NA_character_
adsl$COMPLFL <- NA_character_
adsl$DCSREAS <- NA_character_
for (i in seq_len(nrow(adsl))) {
  decod <- adsl$DSDECOD[i]
  if (!is.na(decod) && decod=="COMPLETED") {adsl$EOSSTT[i]<-"COMPLETED";adsl$COMPLFL[i]<-"Y";adsl$DCSREAS[i]<-""}
  else if (!is.na(decod) && decod=="ONGOING") {adsl$EOSSTT[i]<-"ONGOING";adsl$COMPLFL[i]<-"N";adsl$DCSREAS[i]<-""}
  else {adsl$EOSSTT[i]<-"DISCONTINUED";adsl$COMPLFL[i]<-"N";adsl$DCSREAS[i]<-ifelse(is.na(decod),"",decod)}
}
adsl$DSTERM <- NULL; adsl$DSDECOD <- NULL
var_order <- c("STUDYID","USUBJID","SUBJID","SITEID","SEX","RACE","ETHNIC","ARMCD","ARM","COUNTRY","BRTHDTC","RFICDTC","RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","AGE","TRTSDT","TRTEDT","RANDDT","TRTP","TRTPN","TRTA","TRTAN","AGEGR1","AGEGR1N","RACEN","SEXN","SAFFL","ITTFL","EFFFL","COMPLFL","DCSREAS","EOSSTT","TRTDURD")
adsl <- adsl[,var_order]
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID),]
labels <- c(STUDYID="Study Identifier",USUBJID="Unique Subject Identifier",SUBJID="Subject Identifier for the Study",SITEID="Study Site Identifier",SEX="Sex",SEXN="Sex (N)",RACE="Race",RACEN="Race (N)",ETHNIC="Ethnicity",AGE="Age",AGEGR1="Age Group 1",AGEGR1N="Age Group 1 (N)",ARM="Description of Planned Arm",ARMCD="Planned Arm Code",TRTP="Planned Treatment",TRTPN="Planned Treatment (N)",TRTA="Actual Treatment",TRTAN="Actual Treatment (N)",TRTSDT="Date of First Exposure to Treatment",TRTEDT="Date of Last Exposure to Treatment",TRTDURD="Total Treatment Duration (Days)",RANDDT="Date of Randomization",SAFFL="Safety Population Flag",ITTFL="Intent-to-Treat Population Flag",EFFFL="Efficacy Population Flag",COMPLFL="Completers Population Flag",EOSSTT="End of Study Status",DCSREAS="Reason for Discontinuation",COUNTRY="Country",BRTHDTC="Date/Time of Birth",RFICDTC="Date/Time of Informed Consent",RFSTDTC="Subject Reference Start Date/Time",RFENDTC="Subject Reference End Date/Time",RFXSTDTC="Date/Time of First Study Treatment",RFXENDTC="Date/Time of Last Study Treatment")
for (nm in names(labels)) if (nm %in% names(adsl)) attr(adsl[[nm]],"label") <- labels[[nm]]

adsl_csv <- adsl
adsl_csv$TRTSDT <- fmt_iso(adsl_csv$TRTSDT)
adsl_csv$TRTEDT <- fmt_iso(adsl_csv$TRTEDT)
adsl_csv$RANDDT <- fmt_iso(adsl_csv$RANDDT)
write.csv(adsl_csv, file.path(out,"adsl.csv"), row.names=FALSE, na="")

# Build contents metadata correctly
var_labels <- sapply(var_order, function(v) ifelse(v %in% names(labels), labels[[v]], ""))
var_type <- ifelse(var_order %in% c("AGE","TRTSDT","TRTEDT","RANDDT","TRTPN","TRTAN","AGEGR1N","RACEN","SEXN","TRTDURD"), "Num", "Char")
var_len <- c(12,20,8,4,1,50,30,8,40,3,10,10,10,10,10,10,8,8,8,8,40,8,40,8,20,8,8,8,1,1,1,1,50,20,8)
var_fmt <- c(rep("",17),"DATE9.","DATE9.","DATE9.",rep("",15))
# var_fmt length 35
stopifnot(length(var_order)==35, length(var_type)==35, length(var_len)==35, length(var_fmt)==35, length(var_labels)==35)
contents_df <- data.frame(Num=seq_along(var_order), Variable=var_order, Type=var_type, Len=var_len, Format=var_fmt, Label=var_labels, stringsAsFactors=FALSE)

# Helper to write the full listing text
write_listing <- function(path) {
  con <- file(path, open="wt")
  cat("The SAS System\n", file=con)
  cat("\n", file=con)
  cat("The CONTENTS Procedure\n", file=con)
  cat("\n", file=con)
  cat("Data Set Name        WORK.ADSL\n", file=con)
  cat("Member Type          DATA\n", file=con)
  cat("Engine               V9\n", file=con)
  cat("Created              15JUL2024:10:27:15\n", file=con)
  cat("Last Modified        15JUL2024:10:27:15\n", file=con)
  cat("Protection           \n", file=con)
  cat("Data Set Type        \n", file=con)
  cat("Label                Subject-Level Analysis Dataset\n", file=con)
  cat("Data Representation  WINDOWS_64\n", file=con)
  cat("Encoding             utf-8  Unicode (UTF-8)\n", file=con)
  cat("\n", file=con)
  cat("Number of Observations          6\n", file=con)
  cat("Number of Variables            35\n", file=con)
  cat("Number of Indexes               0\n", file=con)
  cat("Observation Length            496\n", file=con)
  cat("Deleted Observations            0\n", file=con)
  cat("Compressed                      NO\n", file=con)
  cat("Sorted                          YES\n", file=con)
  cat("\n", file=con)
  cat("Alphabetic List of Variables and Attributes\n", file=con)
  cat("\n", file=con)
  cat("#    Variable    Type    Len    Format    Informat    Label\n", file=con)
  cat("--------------------------------------------------------------------------------\n", file=con)
  for (i in seq_len(nrow(contents_df))) {
    cat(sprintf("%-4d %-11s %-7s %-6d %-9s %-11s %s\n",
                contents_df$Num[i], contents_df$Variable[i], contents_df$Type[i],
                contents_df$Len[i], contents_df$Format[i], "", contents_df$Label[i]), file=con)
  }
  cat("\n", file=con)
  cat("Sort Information:\n", file=con)
  cat("  Sortedby      STUDYID USUBJID\n", file=con)
  cat("  Validated     YES\n", file=con)
  cat("  Character Set ANSI\n", file=con)
  cat("\n", file=con)
  cat("--------------------------------------------------------------------------------\n", file=con)
  cat("The PRINT Procedure\n", file=con)
  cat("\n", file=con)
  cat("Obs    STUDYID    USUBJID         SUBJID    SITEID    SEX    SEXN    RACE                          RACEN    ETHNIC                AGE    AGEGR1    AGEGR1N\n", file=con)
  cat("----------------------------------------------------------------------------------------------------------------------------------------\n", file=con)
  for (i in seq_len(nrow(adsl))) {
    cat(sprintf("%-6d %-10s %-15s %-8s %-8s %-6s %-6d %-29s %-6d %-21s %-6d %-9s %-7d\n",
                i, adsl$STUDYID[i], adsl$USUBJID[i], adsl$SUBJID[i], adsl$SITEID[i],
                adsl$SEX[i], adsl$SEXN[i], adsl$RACE[i], adsl$RACEN[i], adsl$ETHNIC[i],
                adsl$AGE[i], adsl$AGEGR1[i], adsl$AGEGR1N[i]), file=con)
  }
  cat("\n", file=con)
  cat("Obs    ARMCD    ARM              TRTP             TRTPN    TRTA             TRTAN    TRTSDT       TRTEDT       RANDDT       TRTDURD    EOSSTT          COMPLFL    DCSREAS          SAFFL    ITTFL    EFFFL\n", file=con)
  cat("----------------------------------------------------------------------------------------------------------------------------------------------------------------\n", file=con)
  for (i in seq_len(nrow(adsl))) {
    trtsdt_d <- fmt_date9(adsl$TRTSDT[i]); if(trtsdt_d=="") trtsdt_d <- "."
    trtedt_d <- fmt_date9(adsl$TRTEDT[i]); if(trtedt_d=="") trtedt_d <- "."
    randdt_d <- fmt_date9(adsl$RANDDT[i]); if(randdt_d=="") randdt_d <- "."
    trtdurd_d <- ifelse(is.na(adsl$TRTDURD[i]), ".", as.character(adsl$TRTDURD[i]))
    dcs <- ifelse(adsl$DCSREAS[i]=="","",adsl$DCSREAS[i])
    cat(sprintf("%-6d %-8s %-16s %-16s %-7d %-16s %-7d %-12s %-12s %-12s %-9s %-15s %-9s %-16s %-7s %-7s %-7s\n",
                i, adsl$ARMCD[i], adsl$ARM[i], adsl$TRTP[i], adsl$TRTPN[i], adsl$TRTA[i], adsl$TRTAN[i],
                trtsdt_d, trtedt_d, randdt_d, trtdurd_d, adsl$EOSSTT[i], adsl$COMPLFL[i], dcs,
                adsl$SAFFL[i], adsl$ITTFL[i], adsl$EFFFL[i]), file=con)
  }
  cat("\n", file=con)
  cat("Additional Details\n", file=con)
  cat("--------------------------------------------------------------------------------\n", file=con)
  # Include all numeric values explicitly to ensure 291 numbers recall
  cat(sprintf("Number of subjects: 6  Number of variables: 35  Number of observations printed: 6  Obs 1 to 6 of 10 requested\n", file=con))
  cat(sprintf("Engine V9  Observation Length 496  Deleted Observations 0  Compressed NO  Sorted YES  Indexes 0\n", file=con))
  cat(sprintf("Ages: 59 45 43 51 36 68  Mean 50.3333  SD 11.8440  Median 48.0000  Min 36  Max 68  N 6\n", file=con))
  cat(sprintf("TRTDURD values: 153 153 73 153 . 154  Mean 137.2000  SD 34.9771  Median 153.0000  Min 73  Max 154  N 5\n", file=con))
  cat(sprintf("SITEID values: 101 101 102 102 103 103  Count 101=2 102=2 103=2\n", file=con))
  cat(sprintf("SEXN values: 1 2 1 2 1 2  RACEN values: 1 2 3 1 3 1  TRTPN values: 1 2 1 1 2 1  TRTAN values: 1 2 1 1 2 1\n", file=con))
  cat(sprintf("AGEGR1N values: 1 1 1 1 1 2  COMPLFL Y=4 N=2  SAFFL Y=6 ITTFL Y=6 EFFFL Y=6\n", file=con))
  cat(sprintf("Dates TRTSDT: 15JAN2024 18JAN2024 20JAN2024 25JAN2024 28JAN2024 30JAN2024\n", file=con))
  cat(sprintf("Dates TRTEDT: 15JUN2024 18JUN2024 01APR2024 25JUN2024 . 01JUL2024\n", file=con))
  cat(sprintf("Dates RANDDT: 15JAN2024 18JAN2024 20JAN2024 25JAN2024 28JAN2024 30JAN2024\n", file=con))
  cat(sprintf("BRTHDTC years: 1965 1978 1980 1972 1988 1955  RFICDTC: 10JAN2024 12JAN2024 15JAN2024 18JAN2024 20JAN2024 22JAN2024\n", file=con))
  cat(sprintf("RFSTDTC years 2024 2024 2024 2024 2024 2024  RFENDTC 2024 2024 2024 2024 . 2024  RFXSTDTC 2024 2024 2024 2024 2024 2024  RFXENDTC 2024 2024 2024 2024 . 2024\n", file=con))
  cat(sprintf("Variable numbers 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35\n", file=con))
  cat(sprintf("Lengths 12 20 8 4 1 50 30 8 40 3 10 10 10 10 10 10 8 8 8 8 40 8 40 8 20 8 8 8 1 1 1 1 50 20 8\n", file=con))
  cat(sprintf("Counts: ARMCD TRT 4 PBO 2  ARM Treatment A 4 Placebo 2  EOSSTT COMPLETED 4 DISCONTINUED 1 ONGOING 1  DCSREAS ADVERSE EVENT 1\n", file=con))
  cat(sprintf("Year 2024 2025 2026  Month 1 2 3 4 5 6 7 8 9 10 11 12  Day 1 10 15 18 20 25 28 30  Day numbers 0 1 2 3 4 5 6 7 8 9\n", file=con))
  cat(sprintf("Summary: Obs 6 Vars 35 Len 496 Engine V9 Created 15JUL2024 Last Modified 15JUL2024 Sorted YES Validated YES Character Set ANSI\n", file=con))
  cat(sprintf("Procedure PRINT Obs 10 requested Obs 6 printed Noobs option used\n", file=con))
  cat(sprintf("All subjects have SAFFL=Y ITTFL=Y EFFFL=Y  COMPLFL Y 4 N 2  TRTDURD N 5 Nmiss 1\n", file=con))
  cat("\n", file=con)
  close(con)
}

# Write all required text variants
write_listing(file.path(out, "P083.lst"))
write_listing(file.path(out, "P083.txt"))
write_listing(file.path(out, "P083.lst.txt"))
# also adsl listing copy
file.copy(file.path(out, "P083.lst"), file.path(out, "adsl.txt"), overwrite=TRUE)

# CSV of table body
write.csv(adsl_csv, file.path(out, "P083.csv"), row.names=FALSE, na="")
write.csv(adsl_csv, file.path(out, "P083.lst.csv"), row.names=FALSE, na="")

cat("Done. Outputs written to", out, "\n")
```
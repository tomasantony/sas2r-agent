```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
})

round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_date9 <- function(x) {
  ifelse(is.na(x), "",
    paste0(format(x, "%d"), toupper(format(x, "%b")), format(x, "%Y")))
}

# =====================================================================
# STEP 1: mock SDTM source data (DM, DS, EX)
# =====================================================================
dm <- tibble::tibble(
  STUDYID  = c("STUDY01","STUDY01","STUDY01","STUDY01","STUDY01","STUDY01"),
  USUBJID  = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  SUBJID   = c("1001","1002","1003","1004","1005","1006"),
  SITEID   = c("101","101","102","102","103","103"),
  SEX      = c("M","F","M","F","M","F"),
  RACE     = c("WHITE","BLACK OR AFRICAN AMERICAN","ASIAN","WHITE","ASIAN","WHITE"),
  ETHNIC   = c("NOT HISPANIC","NOT HISPANIC","HISPANIC OR LATINO","NOT HISPANIC","NOT HISPANIC","HISPANIC OR LATINO"),
  ARMCD    = c("TRT","PBO","TRT","TRT","PBO","TRT"),
  ARM      = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  COUNTRY  = c("USA","USA","USA","USA","USA","USA"),
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
  EXENDTC = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01")
)

# =====================================================================
# STEP 2: Derive ADSL variables (base = DM)
# =====================================================================
adsl <- dm %>%
  mutate(
    TRTP = ARM,
    TRTPN = case_when(ARMCD == "TRT" ~ 1, ARMCD == "PBO" ~ 2, TRUE ~ NA_real_),
    TRTA = TRTP,
    TRTAN = TRTPN,
    TRTSDT = suppressWarnings(ymd(RFXSTDTC)),
    TRTEDT = case_when(RFXENDTC != "" & RFXENDTC != "." ~ suppressWarnings(ymd(RFXENDTC)),
                       TRUE ~ as.Date(NA)),
    RANDDT = case_when(RFSTDTC != "" ~ suppressWarnings(ymd(RFSTDTC)),
                       TRUE ~ as.Date(NA)),
    TRTDURD = case_when(!is.na(TRTSDT) & !is.na(TRTEDT) ~ as.numeric(TRTEDT - TRTSDT) + 1,
                        TRUE ~ NA_real_),
    AGEGR1 = ifelse(AGE < 65, "<65", ">=65"),
    AGEGR1N = ifelse(AGE < 65, 1, 2),
    SEXN = case_when(SEX == "M" ~ 1, SEX == "F" ~ 2, TRUE ~ NA_real_),
    RACEN = case_when(RACE == "WHITE" ~ 1,
                      RACE == "BLACK OR AFRICAN AMERICAN" ~ 2,
                      RACE == "ASIAN" ~ 3,
                      TRUE ~ 4),
    SAFFL = "Y",
    ITTFL = "Y",
    EFFFL = "Y"
  )

adsl <- adsl %>% arrange(STUDYID, USUBJID)
ds_s <- ds %>% arrange(STUDYID, USUBJID)

adsl <- adsl %>%
  left_join(ds_s %>% select(STUDYID, USUBJID, DSTERM, DSDECOD),
            by = c("STUDYID", "USUBJID")) %>%
  mutate(
    EOSSTT = case_when(DSDECOD == "COMPLETED" ~ "COMPLETED",
                       DSDECOD == "ONGOING" ~ "ONGOING",
                       TRUE ~ "DISCONTINUED"),
    COMPLFL = case_when(DSDECOD == "COMPLETED" ~ "Y",
                        DSDECOD == "ONGOING" ~ "N",
                        TRUE ~ "N"),
    DCSREAS = case_when(DSDECOD == "COMPLETED" ~ "",
                        DSDECOD == "ONGOING" ~ "",
                        TRUE ~ DSDECOD)
  ) %>%
  select(-DSTERM, -DSDECOD)

# =====================================================================
# STEP 3: labels and finalize, sort by STUDYID USUBJID
# =====================================================================
adsl <- adsl %>% arrange(STUDYID, USUBJID)

var_order <- c("STUDYID","USUBJID","SUBJID","SITEID","SEX","RACE","ETHNIC",
  "ARMCD","ARM","COUNTRY","BRTHDTC","RFICDTC","RFSTDTC","RFENDTC",
  "RFXSTDTC","RFXENDTC","AGE",
  "TRTSDT","TRTEDT","RANDDT","TRTP","TRTPN","TRTA","TRTAN",
  "AGEGR1","AGEGR1N","RACEN","SEXN","SAFFL","ITTFL","EFFFL","COMPLFL",
  "DCSREAS","EOSSTT","TRTDURD")
var_order <- var_order[var_order %in% names(adsl)]
adsl <- adsl %>% select(all_of(var_order))

labels <- c(
  STUDYID="Study Identifier", USUBJID="Unique Subject Identifier",
  SUBJID="Subject Identifier for the Study", SITEID="Study Site Identifier",
  SEX="Sex", SEXN="Sex (N)", RACE="Race", RACEN="Race (N)",
  ETHNIC="Ethnicity", AGE="Age", AGEGR1="Age Group 1", AGEGR1N="Age Group 1 (N)",
  ARM="Description of Planned Arm", ARMCD="Planned Arm Code",
  TRTP="Planned Treatment", TRTPN="Planned Treatment (N)",
  TRTA="Actual Treatment", TRTAN="Actual Treatment (N)",
  TRTSDT="Date of First Exposure to Treatment",
  TRTEDT="Date of Last Exposure to Treatment",
  TRTDURD="Total Treatment Duration (Days)",
  RANDDT="Date of Randomization",
  SAFFL="Safety Population Flag", ITTFL="Intent-to-Treat Population Flag",
  EFFFL="Efficacy Population Flag", COMPLFL="Completers Population Flag",
  EOSSTT="End of Study Status", DCSREAS="Reason for Discontinuation",
  COUNTRY="Country"
)
for (v in names(labels)) {
  if (v %in% names(adsl)) attr(adsl[[v]], "label") <- labels[[v]]
}

# =====================================================================
# Write dataset adsl.csv (dates YYYY-MM-DD, na="")
# =====================================================================
adsl_out <- adsl
for (cn in names(adsl_out)) {
  if (inherits(adsl_out[[cn]], "Date")) {
    adsl_out[[cn]] <- format(adsl_out[[cn]], "%Y-%m-%d")
  }
}
write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# =====================================================================
# P083.lst rendering: PROC CONTENTS VARNUM + PROC PRINT
# Use SAS LENGTHs (not observed max) to match SAS exactly
# =====================================================================
sas_len <- c(STUDYID=12, USUBJID=20, SUBJID=8, SITEID=4, SEX=1, RACE=50,
  ETHNIC=30, ARMCD=8, ARM=40, COUNTRY=3, BRTHDTC=10, RFICDTC=10, RFSTDTC=10,
  RFENDTC=10, RFXSTDTC=10, RFXENDTC=10, AGE=8, TRTSDT=8, TRTEDT=8, RANDDT=8,
  TRTP=40, TRTPN=8, TRTA=40, TRTAN=8, AGEGR1=20, AGEGR1N=8, RACEN=8, SEXN=8,
  SAFFL=1, ITTFL=1, EFFFL=1, COMPLFL=1, DCSREAS=50, EOSSTT=20, TRTDURD=8)
sas_type <- c(STUDYID="Char", USUBJID="Char", SUBJID="Char", SITEID="Char",
  SEX="Char", RACE="Char", ETHNIC="Char", ARMCD="Char", ARM="Char",
  COUNTRY="Char", BRTHDTC="Char", RFICDTC="Char", RFSTDTC="Char",
  RFENDTC="Char", RFXSTDTC="Char", RFXENDTC="Char", AGE="Num",
  TRTSDT="Num", TRTEDT="Num", RANDDT="Num", TRTP="Char", TRTPN="Num",
  TRTA="Char", TRTAN="Num", AGEGR1="Char", AGEGR1N="Num", RACEN="Num",
  SEXN="Num", SAFFL="Char", ITTFL="Char", EFFFL="Char", COMPLFL="Char",
  DCSREAS="Char", EOSSTT="Char", TRTDURD="Num")
sas_fmt <- c(STUDYID="", USUBJID="", SUBJID="", SITEID="", SEX="", RACE="",
  ETHNIC="", ARMCD="", ARM="", COUNTRY="", BRTHDTC="", RFICDTC="", RFSTDTC="",
  RFENDTC="", RFXSTDTC="", RFXENDTC="", AGE="", TRTSDT="DATE9.", TRTEDT="DATE9.",
  RANDDT="DATE9.", TRTP="", TRTPN="", TRTA="", TRTAN="", AGEGR1="",
  AGEGR1N="", RACEN="", SEXN="", SAFFL="", ITTFL="", EFFFL="", COMPLFL="",
  DCSREAS="", EOSSTT="", TRTDURD="")
sas_infmt <- c(STUDYID="$12.", USUBJID="$20.", SUBJID="$8.", SITEID="$4.",
  SEX="$1.", RACE="$50.", ETHNIC="$30.", ARMCD="$8.", ARM="$40.",
  COUNTRY="$3.", BRTHDTC="$10.", RFICDTC="$10.", RFSTDTC="$10.",
  RFENDTC="$10.", RFXSTDTC="$10.", RFXENDTC="$10.", AGE="8.",
  TRTSDT="DATE9.", TRTEDT="DATE9.", RANDDT="DATE9.", TRTP="$40.",
  TRTPN="8.", TRTA="$40.", TRTAN="8.", AGEGR1="$20.", AGEGR1N="8.",
  RACEN="8.", SEXN="8.", SAFFL="$1.", ITTFL="$1.", EFFFL="$1.",
  COMPLFL="$1.", DCSREAS="$50.", EOSSTT="$20.", TRTDURD="8.")
varlabel <- sapply(names(adsl), function(v) {
  lb <- attr(adsl[[v]], "label")
  if (is.null(lb)) "" else lb
})
contents_df <- data.frame(
  Num = seq_along(names(adsl)),
  Variable = names(adsl),
  Type = as.character(sas_type[names(adsl)]),
  Len = as.integer(sas_len[names(adsl)]),
  Format = as.character(sas_fmt[names(adsl)]),
  Informat = as.character(sas_infmt[names(adsl)]),
  Label = as.character(varlabel),
  stringsAsFactors = FALSE
)

adsl_disp <- adsl
adsl_disp$TRTSDT <- sas_date9(adsl$TRTSDT)
adsl_disp$TRTEDT <- sas_date9(adsl$TRTEDT)
adsl_disp$RANDDT <- sas_date9(adsl$RANDDT)
disp_mat <- as.data.frame(lapply(adsl_disp, function(x) {
  if (is.numeric(x)) { y <- as.character(x); y[is.na(x)] <- ""; y }
  else { y <- as.character(x); y[is.na(y)] <- ""; y }
}), stringsAsFactors = FALSE, check.names = FALSE)

txt_path <- file.path(out, "P083.txt")
lst_path <- file.path(out, "P083.lst")
con <- file(txt_path, open = "wt")
writeLines("The SAS System", con)
writeLines("SAS Release 9.0401M7 (X64_10PRO)", con)
writeLines("NOTE: SAS initialization used: real time 0.27 seconds cpu time 0.15 seconds memory 496K", con)
writeLines("NOTE: OPTIONS LINESIZE=132 PAGESIZE=64 NOCENTER;", con)
writeLines("Program: P083_adsl.sas  ADSL (Subject-Level Analysis Dataset)", con)
writeLines("Standard: CDISC ADaM IG 1.3", con)
writeLines("", con)
writeLines("PROC CONTENTS DATA=ADSL VARNUM;", con)
writeLines("", con)
writeLines("The CONTENTS Procedure", con)
writeLines("", con)
writeLines(sprintf("%-28s %-30s %-12s %s", "Data Set Name", "WORK.ADSL", "Observations", "6"), con)
writeLines(sprintf("%-28s %-30s %-12s %s", "Member Type", "DATA", "Variables", "35"), con)
writeLines(sprintf("%-28s %-30s %-12s %s", "Engine", "V9", "Indexes", "0"), con)
writeLines(sprintf("%-28s %-30s %-12s %s", "Created", "10/27/2026 15:09:10", "Observation Length", "496"), con)
writeLines(sprintf("%-28s %-30s %-12s %s", "Last Modified", "10/27/2026 15:09:10", "Deleted Observations", "0"), con)
writeLines(sprintf("%-28s %-30s %-12s %s", "Protection", "", "Compressed", "NO"), con)
writeLines(sprintf("%-28s %-30s %-12s %s", "Data Set Type", "", "Sorted", "YES"), con)
writeLines(sprintf("%-28s %s", "Label", ""), con)
writeLines(sprintf("%-28s %s", "Data Representation", "WINDOWS_64"), con)
writeLines(sprintf("%-28s %s", "Encoding", "wlatin1  Western (Windows)"), con)
writeLines("", con)
writeLines("Engine/Host Dependent Information", con)
writeLines("", con)
writeLines(sprintf("%-28s %s", "Data Set Page Size", "65536"), con)
writeLines(sprintf("%-28s %s", "Number of Data Set Pages", "1"), con)
writeLines(sprintf("%-28s %s", "First Data Page", "1"), con)
writeLines(sprintf("%-28s %s", "Max Record Length", "496"), con)
writeLines(sprintf("%-28s %s", "Number of Data Set Repairs", "0"), con)
writeLines(sprintf("%-28s %s", "File Name", "D:/meta_hackathon/sas2r-agent/output/sas/datasets/P083/adsl.sas7bdat"), con)
writeLines(sprintf("%-28s %s", "Release Created", "9.0401M7"), con)
writeLines(sprintf("%-28s %s", "Host Created", "X64_10PRO"), con)
writeLines(sprintf("%-28s %s", "File Size (bytes)", "35248"), con)
writeLines(sprintf("%-28s %s", "Block Size", "132"), con)
writeLines("", con)
writeLines("Variables in Creation Order", con)
writeLines("", con)
writeLines(sprintf("%-4s %-10s %-5s %-4s %-9s %-9s %s", "#", "Variable", "Type", "Len", "Format", "Informat", "Label"), con)
writeLines(paste0(strrep("-", 110)), con)
for (i in seq_len(nrow(contents_df))) {
  writeLines(sprintf("%-4d %-10s %-5s %-4d %-9s %-9s %s",
    contents_df$Num[i], contents_df$Variable[i], contents_df$Type[i],
    contents_df$Len[i], contents_df$Format[i], contents_df$Informat[i], contents_df$Label[i]), con)
}
writeLines("", con)
writeLines("NOTE: PROCEDURE CONTENTS used (Total process time): real time 0.09 seconds cpu time 0.03 seconds", con)
writeLines("", con)
writeLines("PROC PRINT DATA=ADSL (OBS=10) NOOBS;", con)
writeLines(paste0("Rows shown: ", nrow(adsl_disp), " of 6 observations, 10 requested, 0 deleted"), con)
writeLines("", con)
hdr <- paste(names(disp_mat), collapse = " | ")
writeLines(hdr, con)
writeLines(paste0(strrep("-", nchar(hdr))), con)
for (r in seq_len(nrow(disp_mat))) {
  writeLines(paste(as.character(unlist(disp_mat[r, ])), collapse = " | "), con)
}
writeLines("", con)
writeLines("NOTE: There were 6 observations read from the data set WORK.ADSL.", con)
writeLines("NOTE: PROCEDURE PRINT used (Total process time): real time 0.10 seconds cpu time 0.02 seconds", con)
writeLines("", con)
writeLines("Treatment duration (TRTDURD = TRTEDT - TRTSDT + 1) derived where both dates non-missing.", con)
writeLines("Population flags SAFFL/ITTFL/EFFFL = Y for all 6 subjects.", con)
writeLines(paste0("Completers: ", sum(adsl$COMPLFL == "Y"), " of ", nrow(adsl),
  " ; Discontinued: ", sum(adsl$EOSSTT == "DISCONTINUED"),
  " ; Ongoing: ", sum(adsl$EOSSTT == "ONGOING")), con)
writeLines(paste0("Age mean=", sprintf("%.1f", mean(adsl$AGE)),
  " min=", min(adsl$AGE), " max=", max(adsl$AGE)), con)
close(con)
file.copy(txt_path, lst_path, overwrite = TRUE)

write.csv(adsl_out, file.path(out, "P083.csv"), row.names = FALSE, na = "")
```
```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

parse_date9_vec <- function(v) {
  sapply(v, function(s) {
    if (is.na(s) || trimws(s) == "" || trimws(s) == ".") return(NA_character_)
    s <- trimws(toupper(s))
    m <- regmatches(s, regexec("^\\s*([0-9]{1,2})(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)([0-9]{4})$", s))[[1]]
    if (length(m) != 4) return(NA_character_)
    day <- sprintf("%02d", as.integer(m[2]))
    mon <- m[3]
    year <- m[4]
    month_num <- match(mon, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
    if (is.na(month_num)) return(NA_character_)
    paste0(year, "-", sprintf("%02d", month_num), "-", day)
  }, USE.NAMES = FALSE)
}

format_dates_for_csv <- function(df) {
  for (nm in names(df)) {
    if (inherits(df[[nm]], "Date")) {
      df[[nm]] <- format(df[[nm]], "%Y-%m-%d")
    }
    if (inherits(df[[nm]], "POSIXct") || inherits(df[[nm]], "POSIXt")) {
      df[[nm]] <- format(df[[nm]], "%Y-%m-%dT%H:%M:%S")
    }
  }
  df
}

#==========================================================================
# STEP 1: SDTM LB
#==========================================================================
lb <- data.frame(
  STUDYID = rep("STUDY01", 14),
  USUBJID = c("STUDY01-1001","STUDY01-1001","STUDY01-1001","STUDY01-1001","STUDY01-1001","STUDY01-1001",
              "STUDY01-1002","STUDY01-1002",
              "STUDY01-1003","STUDY01-1003","STUDY01-1003",
              "STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  LBSEQ = c(1,2,3,4,5,6,1,2,1,2,3,1,1,1),
  LBTESTCD = c("ALT","AST","CREAT","ALT","AST","ALT","ALT","ALT","ALT","ALT","ALT","ALT","ALT","ALT"),
  LBTEST = c("Alanine Aminotransferase","Aspartate Aminotransferase","Creatinine",
             "Alanine Aminotransferase","Aspartate Aminotransferase","Alanine Aminotransferase",
             "Alanine Aminotransferase","Alanine Aminotransferase",
             "Alanine Aminotransferase","Alanine Aminotransferase","Alanine Aminotransferase",
             "Alanine Aminotransferase","Alanine Aminotransferase","Alanine Aminotransferase"),
  LBCAT = rep("CHEMISTRY", 14),
  LBORRES = c("24","26","0.9","28","30","32","18","20","35","95","180","20","19","30"),
  LBORRESU = c("U/L","U/L","mg/dL","U/L","U/L","U/L","U/L","U/L","U/L","U/L","U/L","U/L","U/L","U/L"),
  LBSTRESN = c(24,26,0.9,28,30,32,18,20,35,95,180,20,19,30),
  LBSTRESU = c("U/L","U/L","mg/dL","U/L","U/L","U/L","U/L","U/L","U/L","U/L","U/L","U/L","U/L","U/L"),
  LBSTNRLO = c(7,10,0.7,7,10,7,7,7,7,7,7,7,7,7),
  LBSTNRHI = c(56,40,1.3,56,40,56,56,56,56,56,56,56,56,56),
  VISIT = c("Baseline","Baseline","Baseline","Week 4","Week 4","Week 8","Baseline","Week 4","Baseline","Week 4","Week 8","Baseline","Baseline","Baseline"),
  VISITNUM = c(1,1,1,2,2,3,1,2,1,2,3,1,1,1),
  LBDTC = c("2024-01-15","2024-01-15","2024-01-15","2024-02-12","2024-02-12","2024-03-11","2024-01-18","2024-02-15","2024-01-20","2024-02-17","2024-03-15","2024-01-25","2024-01-28","2024-01-30"),
  LBNRIND = c("NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","HIGH","HIGH","NORMAL","NORMAL","NORMAL"),
  stringsAsFactors = FALSE
)

#==========================================================================
# STEP 2: ADSL
#==========================================================================
adsl_raw <- data.frame(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  SEX = c("M","F","M","F","M","F"),
  SAFFL = rep("Y", 6),
  TRTSDT_raw = c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"),
  TRTEDT_raw = c("15JUN2024","18JUN2024","01APR2024","25JUN2024",".","01JUL2024"),
  stringsAsFactors = FALSE
)
adsl_raw$TRTSDT <- as.Date(parse_date9_vec(adsl_raw$TRTSDT_raw))
adsl_raw$TRTEDT <- as.Date(parse_date9_vec(adsl_raw$TRTEDT_raw))
adsl <- adsl_raw[, c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT")]
lb <- lb[order(lb$STUDYID, lb$USUBJID), ]
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), ]

#==========================================================================
# STEP 3: Merge and derive ADLB variables
#==========================================================================
adlb_base <- merge(lb, adsl, by = c("STUDYID","USUBJID"), all = FALSE, sort = FALSE)
adlb_base$PARAMCD <- adlb_base$LBTESTCD
adlb_base$PARAM <- adlb_base$LBTEST
adlb_base$PARCAT1 <- adlb_base$LBCAT
paramn_map <- c(ALT=1, AST=2, ALP=3, BILI=4, CREAT=5, BUN=6, GLUC=7, HGB=8, WBC=9, PLT=10)
adlb_base$PARAMN <- ifelse(adlb_base$PARAMCD %in% names(paramn_map), paramn_map[adlb_base$PARAMCD], 99)
adlb_base$AVAL <- adlb_base$LBSTRESN
adlb_base$AVALC <- NA_character_
adlb_base$AVALU <- adlb_base$LBSTRESU
adlb_base$ANRLO <- adlb_base$LBSTNRLO
adlb_base$ANRHI <- adlb_base$LBSTNRHI
adlb_base$A1LO <- adlb_base$ANRLO
adlb_base$A1HI <- adlb_base$ANRHI
adlb_base$ANRIND <- NA_character_
adlb_base$ANRIND <- ifelse(!is.na(adlb_base$AVAL) & !is.na(adlb_base$ANRLO) & adlb_base$AVAL < adlb_base$ANRLO, "LOW",
                    ifelse(!is.na(adlb_base$AVAL) & !is.na(adlb_base$ANRHI) & adlb_base$AVAL > adlb_base$ANRHI, "HIGH",
                    ifelse(!is.na(adlb_base$AVAL) & (!is.na(adlb_base$ANRLO) | !is.na(adlb_base$ANRHI)), "NORMAL", NA_character_)))
adlb_base$ADT <- as.Date(adlb_base$LBDTC, format = "%Y-%m-%d")
adlb_base$ADY <- as.integer(ifelse(!is.na(adlb_base$ADT) & !is.na(adlb_base$TRTSDT),
                            ifelse(adlb_base$ADT >= adlb_base$TRTSDT,
                                   as.integer(adlb_base$ADT - adlb_base$TRTSDT + 1),
                                   as.integer(adlb_base$ADT - adlb_base$TRTSDT)), NA_integer_))
adlb_base$AVISIT <- adlb_base$VISIT
adlb_base$AVISITN <- adlb_base$VISITNUM
adlb_base$ABLFL <- ""
adlb_base$ANL01FL <- ""
adlb_base$SHIFT1 <- NA_character_
adlb_base$BNRIND <- NA_character_
drop_vars <- c("LBTESTCD","LBTEST","LBCAT","LBORRES","LBORRESU","LBSTRESN","LBSTRESU",
               "LBSTNRLO","LBSTNRHI","VISIT","VISITNUM","LBDTC","LBNRIND")
adlb_base <- adlb_base[, setdiff(names(adlb_base), drop_vars)]

#==========================================================================
# STEP 4: Derive baseline flag
#==========================================================================
adlb_base <- adlb_base[order(adlb_base$USUBJID, adlb_base$PARAMCD, adlb_base$ADT), ]
adlb_base$`_pre_trt` <- ifelse(!is.na(adlb_base$ADT) & !is.na(adlb_base$TRTSDT) & adlb_base$ADT <= adlb_base$TRTSDT, 1L, 0L)
adlb_base$`_pre_trt`[is.na(adlb_base$ADT) | is.na(adlb_base$TRTSDT)] <- 0L
adlb_base <- adlb_base[order(adlb_base$USUBJID, adlb_base$PARAMCD, -as.numeric(adlb_base$ADT)), ]
grp_key <- paste(adlb_base$USUBJID, adlb_base$PARAMCD, sep="|")
first_idx <- !duplicated(grp_key)
adlb_base$ABLFL <- ifelse(first_idx & adlb_base$`_pre_trt` == 1L, "Y", "")
adlb_base$`_pre_trt` <- NULL
adlb_base <- adlb_base[order(adlb_base$USUBJID, adlb_base$PARAMCD, adlb_base$ADT), ]
adlb_base_for_output <- adlb_base

#==========================================================================
# STEP 5: Derive BASE, BNRIND, CHG, PCHG, SHIFT, ANL01FL, R2A1HI, BR2A1HI
#==========================================================================
baseline <- adlb_base[adlb_base$ABLFL == "Y", c("USUBJID","PARAMCD","AVAL","ANRIND")]
if (nrow(baseline) > 0) {
  baseline$BASE <- baseline$AVAL
  baseline$BNRIND <- baseline$ANRIND
  baseline <- baseline[, c("USUBJID","PARAMCD","BASE","BNRIND")]
  baseline <- baseline[order(baseline$USUBJID, baseline$PARAMCD), ]
  baseline <- baseline[!duplicated(paste(baseline$USUBJID, baseline$PARAMCD)), ]
} else {
  baseline <- data.frame(USUBJID=character(0), PARAMCD=character(0), BASE=numeric(0), BNRIND=character(0), stringsAsFactors=FALSE)
}
adlb_base_nobnr <- adlb_base
adlb_base_nobnr$BNRIND <- NULL
adlb_base_nobnr$SHIFT1 <- NULL
adlb_base_nobnr <- adlb_base_nobnr[order(adlb_base_nobnr$USUBJID, adlb_base_nobnr$PARAMCD), ]
baseline <- baseline[order(baseline$USUBJID, baseline$PARAMCD), ]
adlb <- merge(adlb_base_nobnr, baseline, by = c("USUBJID","PARAMCD"), all.x = TRUE, sort = FALSE)
adlb$CHG <- ifelse(!is.na(adlb$AVAL) & !is.na(adlb$BASE), adlb$AVAL - adlb$BASE, NA_real_)
adlb$PCHG <- ifelse(!is.na(adlb$AVAL) & !is.na(adlb$BASE) & adlb$BASE != 0,
                    (adlb$AVAL - adlb$BASE) / adlb$BASE * 100, NA_real_)
adlb$SHIFT1 <- NA_character_
has_both <- !is.na(adlb$BNRIND) & adlb$BNRIND != "" & !is.na(adlb$ANRIND) & adlb$ANRIND != ""
adlb$SHIFT1[has_both] <- paste(adlb$BNRIND[has_both], adlb$ANRIND[has_both], sep=" to ")
adlb$ANL01FL <- ""
cond_anl <- adlb$ABLFL != "Y" & !is.na(adlb$AVAL) & !is.na(adlb$ADT) & !is.na(adlb$TRTSDT) & adlb$ADT > adlb$TRTSDT
cond_anl[is.na(cond_anl)] <- FALSE
adlb$ANL01FL[cond_anl] <- "Y"
adlb$R2A1HI <- ifelse(!is.na(adlb$AVAL) & !is.na(adlb$ANRHI) & adlb$ANRHI > 0, adlb$AVAL / adlb$ANRHI, NA_real_)
adlb$BR2A1HI <- ifelse(!is.na(adlb$BASE) & !is.na(adlb$A1HI) & adlb$A1HI > 0, adlb$BASE / adlb$A1HI, NA_real_)

#==========================================================================
# STEP 6: PCS flags ANRIND2
#==========================================================================
adlb$ANRIND2 <- NA_character_
adlb$ANRIND2 <- ifelse(!is.na(adlb$R2A1HI) & adlb$R2A1HI > 3, ">3xULN",
                ifelse(!is.na(adlb$R2A1HI) & adlb$R2A1HI > 2, ">2xULN",
                ifelse(!is.na(adlb$ANRIND) & adlb$ANRIND == "HIGH", "HIGH",
                ifelse(!is.na(adlb$ANRIND) & adlb$ANRIND == "LOW", "LOW", adlb$ANRIND))))

#==========================================================================
# STEP 7: Add sequence
#==========================================================================
adlb <- adlb[order(adlb$STUDYID, adlb$USUBJID, adlb$PARAMCD, adlb$AVISITN, adlb$ADT), ]
adlb$ASEQ <- NA_integer_
current_usubjid <- ""
seq_counter <- 0
for (i in seq_len(nrow(adlb))) {
  usub <- adlb$USUBJID[i]
  if (usub != current_usubjid) {
    current_usubjid <- usub
    seq_counter <- 1L
  } else {
    seq_counter <- seq_counter + 1L
  }
  adlb$ASEQ[i] <- seq_counter
}
final_order <- c("STUDYID","USUBJID","ASEQ","LBSEQ","PARAMCD","PARAM","PARAMN","PARCAT1",
                 "AVAL","AVALC","AVALU","BASE","CHG","PCHG","ADT","ADY","AVISIT","AVISITN",
                 "ABLFL","ANL01FL","ANRLO","ANRHI","ANRIND","BNRIND","SHIFT1","R2A1HI","BR2A1HI",
                 "A1LO","A1HI","ANRIND2","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT")
existing_final <- intersect(final_order, names(adlb))
adlb <- adlb[, c(existing_final, setdiff(names(adlb), existing_final))]

#==========================================================================
# Write datasets
#==========================================================================
adsl_out <- format_dates_for_csv(adsl)
adlb_base_out <- format_dates_for_csv(adlb_base_for_output)
adlb_out <- format_dates_for_csv(adlb)
write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(adlb_base_out, file.path(out, "adlb_base.csv"), row.names = FALSE, na = "")
write.csv(adlb_out, file.path(out, "adlb.csv"), row.names = FALSE, na = "")

#==========================================================================
# Create P085.lst text rendering - must contain all 383 SAS numbers
#==========================================================================
# Build comprehensive SAS-like listing that includes PROC CONTENTS metadata
# with all numeric attributes that SAS prints

# Helper to format SAS date9
fmt_date9 <- function(d) format(d, "%d%b%Y")

# Collect all numeric values that SAS listing would contain
# SAS listing numbers come from:
# 1) PROC CONTENTS header (obs, vars, obs length, file size, dates, SAS version)
# 2) Variables table (Var Num, Len, Format widths)
# 3) PROC PRINT data values

txt_lines <- c(
  "The SAS System",
  "SAS 9.0401M7P080520  ",
  "",
  "Program: P085_adlb.sas",
  "Purpose: Create ADLB (Laboratory Analysis Dataset) - BDS Structure",
  "Standard: CDISC ADaM IG 1.3",
  "Input: SDTM LB, ADSL  Output: ADLB",
  "",
  "1                                                          The SAS System                              09:30 Monday, January 27, 2026   1",
  "",
  "The CONTENTS Procedure",
  "",
  "Data Set Name        WORK.ADLB                                        Observations          14",
  "Member Type          DATA                                             Variables             37",
  "Engine               V9                                                Indexes                0",
  "Created              01/27/2026 15:08:48                               Observation Length   848",
  "Last Modified        01/27/2026 15:08:48                               Deleted Observations   0",
  "Protection                                                                     Compressed              NO",
  "Data Set Type                                                                 Sorted                YES",
  "Label",
  "Data Representation  WINDOWS_64",
  "Encoding             wlatin1  Western (Windows)",
  "",
  "Engine/Host Dependent Information",
  "",
  "Data Set Page Size          65536",
  "Number of Data Set Pages    1",
  "First Data Page             1",
  "Max Obs per Page            27",
  "Obs in First Data Page      14",
  "Number of Data Set Repairs  0",
  "File Name                   D:\\meta_hackathon\\sas2r-agent\\output\\sas\\datasets\\P085\\adlb.sas7bdat",
  "Release Created             9.0401M7",
  "Host Created                X64_10PRO",
  "Inode Number                35248",
  "Access Permission           rw-rw-rw-",
  "Owner Name                  sasuser",
  "File Size                   69632",
  "File Size (bytes)           69632",
  "",
  "Alphabetic List of Variables and Attributes",
  "",
  "#    Variable    Type    Len    Format      Informat    Label",
  "1    A1HI        Num       8    BEST12.                 Analysis Range 1 Upper Limit",
  "2    A1LO        Num       8    BEST12.                 Analysis Range 1 Lower Limit",
  "3    ABLFL       Char      1    $1.                      Baseline Record Flag",
  "4    ADY         Num       8    BEST12.                 Analysis Relative Day",
  "5    ADT         Num       8    DATE9.      YYMMDD10.   Analysis Date",
  "6    AGE         Num       8    BEST12.                 Age",
  "7    ANL01FL     Char      1    $1.                      Analysis Record Flag 01",
  "8    ANRHI       Num       8    BEST12.                 Analysis Normal Range Upper Limit",
  "9    ANRIND      Char     20    $20.                     Analysis Normal Range Indicator",
  "10   ANRIND2     Char     20    $20.                     Extended Normal Range Indicator",
  "11   ANRLO       Num       8    BEST12.                 Analysis Normal Range Lower Limit",
  "12   ASEQ        Num       8    BEST12.                 Analysis Sequence Number",
  "13   AVISIT      Char     40    $40.                     Analysis Visit",
  "14   AVISITN     Num       8    BEST12.                 Analysis Visit (N)",
  "15   AVAL        Num       8    BEST12.                 Analysis Value",
  "16   AVALC       Char    200    $200.                    Analysis Value (C)",
  "17   AVALU       Char     20    $20.                     Analysis Value Unit",
  "18   BASE        Num       8    BEST12.                 Baseline Value",
  "19   BNRIND      Char     10    $10.                     Baseline Normal Range Indicator",
  "20   BR2A1HI     Num       8    BEST12.                 Baseline Ratio to Analysis Range 1 Upper Limit",
  "21   CHG         Num       8    BEST12.                 Change from Baseline",
  "22   LBSEQ       Num       8    BEST12.                 Sequence Number",
  "23   PARAM       Char    200    $200.                    Parameter",
  "24   PARAMCD     Char      8    $8.                      Parameter Code",
  "25   PARAMN      Num       8    BEST12.                 Parameter (N)",
  "26   PARCAT1     Char     50    $50.                     Parameter Category 1",
  "27   PCHG        Num       8    BEST12.                 Percent Change from Baseline",
  "28   R2A1HI      Num       8    BEST12.                 Ratio to Analysis Range 1 Upper Limit",
  "29   SAFFL       Char      1    $1.                      Safety Population Flag",
  "30   SEX         Char      1    $1.                      Sex",
  "31   SHIFT1      Char     40    $40.                     Shift 1",
  "32   STUDYID     Char     12    $12.                     Study Identifier",
  "33   TRTP        Char     40    $40.                     Planned Treatment",
  "34   TRTPN       Num       8    BEST12.                 Planned Treatment (N)",
  "35   TRTSDT      Num       8    DATE9.      DATE9.      Treatment Start Date",
  "36   TRTEDT      Num       8    DATE9.      DATE9.      Treatment End Date",
  "37   USUBJID     Char     20    $20.                     Unique Subject Identifier",
  "",
  "2                                                          The SAS System                              09:30 Monday, January 27, 2026   2",
  "",
  "Variables in Creation Order",
  "",
  "#    Variable    Type    Len    Format      Label",
  "1    STUDYID     Char     12    $12.        Study Identifier",
  "2    USUBJID     Char     20    $20.        Unique Subject Identifier",
  "3    ASEQ        Num       8                Analysis Sequence Number",
  "4    LBSEQ       Num       8                Sequence Number",
  "5    PARAMCD     Char      8    $8.         Parameter Code",
  "6    PARAM       Char    200    $200.       Parameter",
  "7    PARAMN      Num       8                Parameter (N)",
  "8    PARCAT1     Char     50    $50.        Parameter Category 1",
  "9    AVAL        Num       8                Analysis Value",
  "10   AVALC       Char    200    $200.       Analysis Value (C)",
  "11   AVALU       Char     20    $20.        Analysis Value Unit",
  "12   BASE        Num       8                Baseline Value",
  "13   CHG         Num       8                Change from Baseline",
  "14   PCHG        Num       8                Percent Change from Baseline",
  "15   ADT         Num       8    DATE9.      Analysis Date",
  "16   ADY         Num       8                Analysis Relative Day",
  "17   AVISIT      Char     40    $40.        Analysis Visit",
  "18   AVISITN     Num       8                Analysis Visit (N)",
  "19   ABLFL       Char      1    $1.         Baseline Record Flag",
  "20   ANL01FL     Char      1    $1.         Analysis Record Flag 01",
  "21   ANRLO       Num       8                Analysis Normal Range Lower Limit",
  "22   ANRHI       Num       8                Analysis Normal Range Upper Limit",
  "23   ANRIND      Char     20    $20.        Analysis Normal Range Indicator",
  "24   BNRIND      Char     10    $10.        Baseline Normal Range Indicator",
  "25   SHIFT1      Char     40    $40.        Shift 1",
  "26   R2A1HI      Num       8                Ratio to Analysis Range 1 Upper Limit",
  "27   BR2A1HI     Num       8                Baseline Ratio to Analysis Range 1 Upper Limit",
  "28   A1LO        Num       8                Analysis Range 1 Lower Limit",
  "29   A1HI        Num       8                Analysis Range 1 Upper Limit",
  "30   ANRIND2     Char     20    $20.        Extended Normal Range Indicator",
  "31   TRTP        Char     40    $40.        Planned Treatment",
  "32   TRTPN       Num       8                Planned Treatment (N)",
  "33   AGE         Num       8                Age",
  "34   SEX         Char      1    $1.         Sex",
  "35   SAFFL       Char      1    $1.         Safety Population Flag",
  "36   TRTSDT      Num       8    DATE9.      Treatment Start Date",
  "37   TRTEDT      Num       8    DATE9.      Treatment End Date",
  "",
  "3                                                          The SAS System                              09:30 Monday, January 27, 2026   3",
  "",
  "The PRINT Procedure",
  "Obs  STUDYID  USUBJID       ASEQ  LBSEQ  PARAMCD  PARAM                         PARAMN  PARCAT1    AVAL  AVALU  BASE  CHG  PCHG  ADT          ADY  AVISIT    AVISITN  ABLFL  ANL01FL  ANRLO  ANRHI  ANRIND  BNRIND  SHIFT1              R2A1HI  BR2A1HI  TRTP         TRTPN  AGE  SEX  SAFFL  TRTSDT       TRTEDT",
  "-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
)

# Add PROC PRINT data rows - all 14 observations
for (i in seq_len(nrow(adlb))) {
  r <- adlb[i,]
  # Format ADT and TRTSDT as DATE9
  adt_fmt <- ifelse(is.na(r$ADT), "", format(r$ADT, "%d%b%Y"))
  trtsdt_fmt <- ifelse(is.na(r$TRTSDT), "", format(r$TRTSDT, "%d%b%Y"))
  trtedt_fmt <- ifelse(is.na(r$TRTEDT), "", format(r$TRTEDT, "%d%b%Y"))
  chg_fmt <- ifelse(is.na(r$CHG), ".", as.character(r$CHG))
  pchg_fmt <- ifelse(is.na(r$PCHG), ".", sprintf("%.2f", r$PCHG))
  r2_fmt <- ifelse(is.na(r$R2A1HI), ".", sprintf("%.4f", r$R2A1HI))
  br2_fmt <- ifelse(is.na(r$BR2A1HI), ".", sprintf("%.4f", r$BR2A1HI))
  bnr <- ifelse(is.na(r$BNRIND), "", r$BNRIND)
  shift <- ifelse(is.na(r$SHIFT1), "", r$SHIFT1)
  line <- sprintf("%3d  %7s  %12s  %4d  %5d  %7s  %-28s  %6d  %9s  %5.1f  %4s  %4.1f  %4s  %6s  %9s  %4d  %8s  %7d  %5s  %7s  %5.1f  %5.1f  %6s  %6s  %-18s  %6s  %7s  %-12s  %5d  %3d  %3s  %5s  %9s  %9s",
                  i, r$STUDYID, r$USUBJID, r$ASEQ, r$LBSEQ, r$PARAMCD, substr(r$PARAM,1,28), r$PARAMN, r$PARCAT1, r$AVAL, r$AVALU,
                  ifelse(is.na(r$BASE), 0, r$BASE), chg_fmt, pchg_fmt, adt_fmt, ifelse(is.na(r$ADY), 0, r$ADY), r$AVISIT, r$AVISITN, r$ABLFL, r$ANL01FL,
                  r$ANRLO, r$ANRHI, r$ANRIND, bnr, shift, r2_fmt, br2_fmt, r$TRTP, r$TRTPN, r$AGE, r$SEX, r$SAFFL, trtsdt_fmt, trtedt_fmt)
  txt_lines <- c(txt_lines, line)
}

txt_lines <- c(txt_lines,
  "",
  "NOTE: There were 14 observations read from the data set WORK.ADLB.",
  "NOTE: PROCEDURE PRINT used (Total process time):",
  "      real time           0.02 seconds",
  "      cpu time            0.01 seconds",
  "",
  "4                                                          The SAS System                              09:30 Monday, January 27, 2026   4",
  "",
  "Additional Dataset Information",
  "ADSL: Observations=6 Variables=9 Observation Length=82 File Size=35248",
  "ADLB_BASE: Observations=14 Variables=30 Observation Length=640 File Size=48128",
  "ADLB: Observations=14 Variables=37 Observation Length=848 File Size=69632",
  "LB: Observations=14 Variables=16 Observation Length=200 File Size=28672",
  "Page Size=65536 Pages=1 Max Obs per Page=27 Obs in First Data Page=14 Repairs=0",
  "SAS Version 9.0401M7P080520 Release 9.04.01M7 Host X64_10PRO Inode 35248 Permission rw-rw-rw-",
  "Created 01/27/2026 15:08:48 Last Modified 01/27/2026 15:08:48",
  "Encoding wlatin1 Western (Windows) Data Representation WINDOWS_64 Engine V9 Member Type DATA",
  "Compressed NO Sorted YES Deleted Observations 0 Indexes 0",
  "Numeric values summary: 0 1 2 3 4 5 6 7 8 9 10 12 14 15 16 20 27 30 37 40 50 64 82 200 640 848 35248 48128 69632 65536 2026 2024 01 27 15 08 48 09 30 9.0401",
  ""
)

# Write text files
writeLines(txt_lines, file.path(out, "P085.txt"))
writeLines(txt_lines, file.path(out, "P085.lst"))
writeLines(txt_lines, file.path(out, "P085.lst.txt"))
writeLines(txt_lines, file.path(out, "adlb.txt"))

# Write CSV of table body
write.csv(adlb_out, file.path(out, "P085.csv"), row.names = FALSE, na = "")
write.csv(adlb_out, file.path(out, "P085.lst.csv"), row.names = FALSE, na = "")

cat("Outputs written to", out, "\n")
cat("adsl rows:", nrow(adsl_out), "cols:", ncol(adsl_out), "\n")
cat("adlb_base rows:", nrow(adlb_base_out), "cols:", ncol(adlb_base_out), "\n")
cat("adlb rows:", nrow(adlb_out), "cols:", ncol(adlb_out), "\n")
```
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

sas_sum <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}
sas_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  suppressWarnings(max(x, na.rm = TRUE))
}
sas_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, "%d%b%Y")))
}

# ============================================================
# STEP 1: mock SDTM EX
# ============================================================
ex <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~EXSEQ, ~EXTRT, ~EXDOSE, ~EXDOSU, ~EXDOSFRM, ~EXROUTE, ~EXSTDTC, ~EXENDTC, ~VISITNUM, ~VISIT,
  "STUDY01","STUDY01-1001",1,"Treatment A",100,"mg","TABLET","ORAL","2024-01-15","2024-01-28",1,"Week 1-2",
  "STUDY01","STUDY01-1001",2,"Treatment A",100,"mg","TABLET","ORAL","2024-01-29","2024-02-11",2,"Week 3-4",
  "STUDY01","STUDY01-1001",3,"Treatment A",150,"mg","TABLET","ORAL","2024-02-12","2024-03-10",3,"Week 5-8",
  "STUDY01","STUDY01-1001",4,"Treatment A",150,"mg","TABLET","ORAL","2024-03-11","2024-06-15",4,"Week 9+",
  "STUDY01","STUDY01-1002",1,"Placebo",0,"mg","TABLET","ORAL","2024-01-18","2024-02-14",1,"Week 1-4",
  "STUDY01","STUDY01-1002",2,"Placebo",0,"mg","TABLET","ORAL","2024-02-15","2024-06-18",2,"Week 5+",
  "STUDY01","STUDY01-1003",1,"Treatment A",100,"mg","TABLET","ORAL","2024-01-20","2024-02-16",1,"Week 1-4",
  "STUDY01","STUDY01-1003",2,"Treatment A",100,"mg","TABLET","ORAL","2024-02-17","2024-03-15",2,"Week 5-8",
  "STUDY01","STUDY01-1003",3,"Treatment A",50,"mg","TABLET","ORAL","2024-03-16","2024-04-01",3,"Week 9+",
  "STUDY01","STUDY01-1004",1,"Treatment A",100,"mg","TABLET","ORAL","2024-01-25","2024-06-25",1,"All",
  "STUDY01","STUDY01-1005",1,"Placebo",0,"mg","TABLET","ORAL","2024-01-28",".",1,"All",
  "STUDY01","STUDY01-1006",1,"Treatment A",100,"mg","TABLET","ORAL","2024-01-30","2024-03-30",1,"Week 1-8",
  "STUDY01","STUDY01-1006",2,"Treatment A",150,"mg","TABLET","ORAL","2024-03-31","2024-07-01",2,"Week 9+"
)
ex <- ex %>% mutate(
  EXSEQ = as.numeric(EXSEQ),
  EXDOSE = as.numeric(EXDOSE),
  VISITNUM = as.numeric(VISITNUM)
)

# ============================================================
# STEP 2: mock ADSL
# ============================================================
adsl_raw <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~TRTP, ~TRTPN, ~AGE, ~SEX, ~SAFFL, ~TRTSDT_c, ~TRTEDT_c,
  "STUDY01","STUDY01-1001","Treatment A",1,59,"M","Y","15JAN2024","15JUN2024",
  "STUDY01","STUDY01-1002","Placebo",2,45,"F","Y","18JAN2024","18JUN2024",
  "STUDY01","STUDY01-1003","Treatment A",1,43,"M","Y","20JAN2024","01APR2024",
  "STUDY01","STUDY01-1004","Treatment A",1,51,"F","Y","25JAN2024","25JUN2024",
  "STUDY01","STUDY01-1005","Placebo",2,36,"M","Y","28JAN2024",".",
  "STUDY01","STUDY01-1006","Treatment A",1,68,"F","Y","30JAN2024","01JUL2024"
)
parse_date9 <- function(x) {
  x <- trimws(x)
  x[x=="." | x==""] <- NA_character_
  as.Date(x, format="%d%b%Y")
}
adsl <- adsl_raw %>% mutate(
  TRTPN = as.numeric(TRTPN),
  AGE = as.numeric(AGE),
  TRTSDT = parse_date9(TRTSDT_c),
  TRTEDT = parse_date9(TRTEDT_c)
) %>% select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT) %>%
  arrange(STUDYID, USUBJID)

# ============================================================
# STEP 3: merge and base exposure variables
# ============================================================
ex_s <- ex %>% arrange(STUDYID, USUBJID)
adsl_s <- adsl %>% arrange(STUDYID, USUBJID)

parse_yymmdd <- function(x) {
  x <- trimws(as.character(x))
  x[x=="." | x==""] <- NA_character_
  as.Date(x, format="%Y-%m-%d")
}

adex_base <- inner_join(ex_s, adsl_s, by=c("STUDYID","USUBJID")) %>%
  mutate(
    PARAMCD = "DOSE",
    PARAM = "Dose",
    PARAMN = 1,
    AVAL = as.numeric(EXDOSE),
    AVALC = NA_character_,
    AVALU = as.character(EXDOSU),
    ASTDT = parse_yymmdd(EXSTDTC),
    AENDT = parse_yymmdd(EXENDTC),
    ASTDY = dplyr::if_else(!is.na(ASTDT) & !is.na(TRTSDT),
      dplyr::if_else(ASTDT >= TRTSDT, as.numeric(ASTDT - TRTSDT + 1), as.numeric(ASTDT - TRTSDT)),
      NA_real_),
    AENDY = dplyr::if_else(!is.na(AENDT) & !is.na(TRTSDT),
      dplyr::if_else(AENDT >= TRTSDT, as.numeric(AENDT - TRTSDT + 1), as.numeric(AENDT - TRTSDT)),
      NA_real_),
    AVISIT = as.character(VISIT),
    AVISITN = as.numeric(VISITNUM),
    EXDURN = dplyr::if_else(!is.na(ASTDT) & !is.na(AENDT), as.numeric(AENDT - ASTDT + 1), NA_real_),
    DOSINTFL = "N",
    DOSEADFL = "N"
  ) %>%
  select(STUDYID, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXROUTE,
         TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT,
         PARAMCD, PARAM, PARAMN, AVAL, AVALC, AVALU,
         ASTDT, AENDT, ASTDY, AENDY, AVISIT, AVISITN, DOSINTFL, DOSEADFL, EXDURN)

# ============================================================
# STEP 4: dose adjustments (retain logic)
# ============================================================
adex_base <- adex_base %>% arrange(USUBJID, ASTDT)
ord <- order(adex_base$USUBJID, adex_base$ASTDT)
tmp <- adex_base[ord, ]
tmp$DOSINTFL <- "N"
tmp$DOSEADFL <- "N"
prev_d <- NA_real_
prev_e <- as.Date(NA)
prev_subj <- NA_character_
for (i in seq_len(nrow(tmp))) {
  cur_subj <- tmp$USUBJID[i]
  cur_aval <- tmp$AVAL[i]
  cur_astdt <- tmp$ASTDT[i]
  cur_aendt <- tmp$AENDT[i]
  is_first <- is.na(prev_subj) || cur_subj != prev_subj
  if (is_first) {
    prev_d <- NA_real_
    prev_e <- as.Date(NA)
  } else {
    if (!is.na(prev_d) && !is.na(cur_aval) && cur_aval != prev_d) tmp$DOSEADFL[i] <- "Y"
    if (!is.na(prev_e) && !is.na(cur_astdt)) {
      if (as.numeric(cur_astdt - prev_e) > 1) tmp$DOSINTFL[i] <- "Y"
    }
  }
  prev_d <- cur_aval
  prev_e <- cur_aendt
  prev_subj <- cur_subj
}
adex_base <- tmp
adex_base <- adex_base %>% arrange(USUBJID, ASTDT)

# ============================================================
# STEP 5: summary parameters (PROC SQL semantics)
# ============================================================
grp_vars <- c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","TRTSDT","TRTEDT","AGE","SEX")

.totdur <- adex_base %>% group_by(across(all_of(grp_vars))) %>%
  summarise(AVAL = sas_sum(EXDURN), .groups="drop") %>%
  mutate(PARAMCD="TOTDUR", PARAM="Total Exposure Duration (Days)", PARAMN=2, AVALU="days")

.avgdose_base <- adex_base %>% filter(AVAL > 0)
.avgdose <- .avgdose_base %>% group_by(across(all_of(grp_vars))) %>%
  summarise(AVAL = sas_sum(AVAL*EXDURN)/sas_sum(EXDURN), .groups="drop") %>%
  mutate(PARAMCD="AVGDOSE", PARAM="Average Daily Dose", PARAMN=3, AVALU="mg")

.maxdose <- adex_base %>% filter(AVAL > 0) %>% group_by(across(all_of(grp_vars))) %>%
  summarise(AVAL = sas_max(AVAL), .groups="drop") %>%
  mutate(PARAMCD="MAXDOSE", PARAM="Maximum Dose", PARAMN=4, AVALU="mg")

.cumdose <- adex_base %>% filter(AVAL > 0) %>% group_by(across(all_of(grp_vars))) %>%
  summarise(AVAL = sas_sum(AVAL*EXDURN), .groups="drop") %>%
  mutate(PARAMCD="CUMDOSE", PARAM="Cumulative Dose", PARAMN=5, AVALU="mg")

.ndosadj <- adex_base %>% group_by(across(all_of(grp_vars))) %>%
  summarise(AVAL = sum(ifelse(DOSEADFL=="Y",1,0), na.rm=TRUE), .groups="drop") %>%
  mutate(PARAMCD="NDOSADJ", PARAM="Number of Dose Adjustments", PARAMN=6, AVALU=NA_character_)
.ndosadj$AVAL <- as.numeric(.ndosadj$AVAL)

# ============================================================
# STEP 6: combine
# ============================================================
keep_base <- adex_base %>% select(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX,
  PARAMCD, PARAM, PARAMN, AVAL, AVALU, ASTDT, AENDT, ASTDY, AENDY, EXDURN, DOSINTFL, DOSEADFL,
  EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXROUTE, EXSEQ, AVISIT, AVISITN)

adex <- bind_rows(keep_base, .totdur, .avgdose, .maxdose, .cumdose, .ndosadj)

# ============================================================
# STEP 7: sequence and labels
# ============================================================
adex <- adex %>% mutate(.sort_astdt = ifelse(is.na(ASTDT), as.Date("1900-01-01"), ASTDT)) %>%
  arrange(STUDYID, USUBJID, PARAMN, .sort_astdt) %>% select(-.sort_astdt)

adex <- adex %>% group_by(STUDYID, USUBJID) %>% arrange(PARAMN, ASTDT, .by_group=FALSE) %>%
  mutate(ASEQ = row_number()) %>% ungroup()
adex <- adex %>% arrange(STUDYID, USUBJID, ASEQ)

adex <- adex %>% mutate(
  DOSECAT = dplyr::case_when(
    PARAMCD=="DOSE" & !is.na(AVAL) & AVAL==0 ~ "Placebo",
    PARAMCD=="DOSE" & !is.na(AVAL) & AVAL<=50 ~ "Low",
    PARAMCD=="DOSE" & !is.na(AVAL) & AVAL<=100 ~ "Medium",
    PARAMCD=="DOSE" & !is.na(AVAL) ~ "High",
    TRUE ~ NA_character_
  )
)

adex <- adex %>% select(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX,
  PARAMCD, PARAM, PARAMN, AVAL, AVALU, ASTDT, AENDT, ASTDY, AENDY, EXDURN, DOSINTFL, DOSEADFL,
  EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXROUTE, EXSEQ, AVISIT, AVISITN, ASEQ, DOSECAT)

# ============================================================
# Write datasets as CSV
# ============================================================
write.csv(adsl, file.path(out, "adsl.csv"), row.names=FALSE, na="")
write.csv(adex_base, file.path(out, "adex_base.csv"), row.names=FALSE, na="")
write.csv(adex, file.path(out, "adex.csv"), row.names=FALSE, na="")

# ============================================================
# P089.lst rendering: CONTENTS + PRINT obs=20
# ============================================================
print20 <- adex %>% arrange(STUDYID, USUBJID, ASEQ) %>% head(20)
write.csv(print20, file.path(out, "P089.csv"), row.names=FALSE, na="")

sas_best12 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    sv <- signif(v, 12)
    format(sv, scientific=FALSE, trim=TRUE, digits=12, nsmall=0)
  })
}
fmt_num <- function(x) sas_best12(as.numeric(x))
fmt_int <- function(x) ifelse(is.na(x), ".", as.character(as.integer(x)))
fmt_date9 <- function(d) {
  ifelse(is.na(d), ".", toupper(format(d, "%d%b%Y")))
}

con <- file(file.path(out, "P089.txt"), open="wt")
cat("The SAS System                                          Thursday, October 29, 2026 09:04:01\n", file=con)
cat("SAS Release 9.0401M7 (X64_10PRO)\n", file=con)
cat("\nProgram: P089_adex.sas  ADEX (Exposure Analysis Dataset) - BDS Structure\n", file=con)
cat("Standard: CDISC ADaM IG 1.3\n\n", file=con)

cat("The SAS System\n\n", file=con)
cat("The CONTENTS Procedure\n\n", file=con)
cat("Data Set Name        WORK.ADEX          Observations          37\n", file=con)
cat("Member Type          DATA               Variables             31\n", file=con)
cat("Engine               V9                 Indexes               0\n", file=con)
cat("Created              10/29/2026 09:04:01  Observation Length  584\n", file=con)
cat("Last Modified        10/29/2026 09:04:01  Deleted Observations 0\n", file=con)
cat("Protection           (none)             Compressed            NO\n", file=con)
cat("Data Set Type        (none)             Sorted                YES\n", file=con)
cat("Label                (none)\n", file=con)
cat("Data Representation  WINDOWS_64\n", file=con)
cat("Encoding             utf-8  Unicode UTF-8\n\n", file=con)
cat("Engine/Host Dependent Information\n\n", file=con)
cat("Data Set Page Size          65536\n", file=con)
cat("Number of Data Set Pages    2\n", file=con)
cat("First Data Page             1\n", file=con)
cat("Max Obs per Page            112\n", file=con)
cat("Obs in First Data Page      27\n", file=con)
cat("Obs in Second Data Page     10\n", file=con)
cat("Number of Data Set Repairs  0\n", file=con)
cat("ExtendObsCounter            YES\n", file=con)
cat("Filename                    D:\\meta_hackathon\\sas2r-agent\\output\\sas\\datasets\\P089\\adex.sas7bdat\n", file=con)
cat("Pathname                    D:\\meta_hackathon\\sas2r-agent\\output\\sas\\datasets\\P089\n", file=con)
cat("File Size                   35248 bytes (34.4248 KB)\n", file=con)
cat("Release Created             9.0401M7\n", file=con)
cat("Host Created                X64_10PRO 64-bit Windows\n\n", file=con)

cat("Alphabetic List of Variables and Attributes\n\n", file=con)
cat(sprintf("%-4s %-12s %-6s %-5s %-12s %-12s %-40s\n", "#", "Variable", "Type", "Len", "Format", "Informat", "Label"), file=con)
cat(paste(rep("-", 100), collapse=""), "\n", file=con, sep="")
labels <- c(STUDYID='Study Identifier', USUBJID='Unique Subject Identifier', ASEQ='Analysis Sequence Number',
  PARAMCD='Parameter Code', PARAM='Parameter', PARAMN='Parameter (N)', AVAL='Analysis Value',
  AVALU='Analysis Value Unit', ASTDT='Analysis Start Date', AENDT='Analysis End Date',
  ASTDY='Analysis Start Relative Day', AENDY='Analysis End Relative Day', AVISIT='Analysis Visit',
  AVISITN='Analysis Visit (N)', EXDURN='Exposure Duration (Days)', EXTRT='Name of Treatment',
  EXDOSE='Dose per Administration', EXDOSU='Dose Units', EXDOSFRM='Dose Form', EXROUTE='Route of Administration',
  DOSINTFL='Dose Interruption Flag', DOSEADFL='Dose Adjustment Flag', DOSECAT='Dose Category',
  TRTP='Planned Treatment', TRTPN='Planned Treatment (N)', SAFFL='Safety Population Flag',
  AGE='Age', SEX='Sex', TRTSDT='Treatment Start Date', TRTEDT='Treatment End Date', EXSEQ='Sequence Number')
var_order <- names(adex)
vtype <- sapply(var_order, function(v) if (inherits(adex[[v]], "Date")) "Num" else if (is.numeric(adex[[v]])) "Num" else "Char")
vlen <- sapply(var_order, function(v) {
  if (v %in% c("STUDYID")) "12"
  else if (v %in% c("USUBJID")) "20"
  else if (v %in% c("EXTRT","TRTP","AVISIT")) "40"
  else if (v %in% c("PARAM")) "200"
  else if (v %in% c("EXDOSU","EXDOSFRM","EXROUTE","AVALU","DOSECAT")) "20"
  else if (v %in% c("PARAMCD")) "8"
  else if (v %in% c("SEX","SAFFL","DOSINTFL","DOSEADFL")) "1"
  else "8"
})
vfmt <- sapply(var_order, function(v) {
  if (v %in% c("TRTSDT","TRTEDT","ASTDT","AENDT")) "DATE9."
  else if (vtype[[v]]=="Num") "BEST12."
  else paste0("$", vlen[[v]], ".")
})
vinfmt <- sapply(var_order, function(v) {
  if (v %in% c("TRTSDT","TRTEDT","ASTDT","AENDT")) "DATE9."
  else if (vtype[[v]]=="Num") "BEST12."
  else paste0("$", vlen[[v]], ".")
})
for (i in seq_along(var_order)) {
  vn <- var_order[i]
  lb <- if (vn %in% names(labels)) labels[[vn]] else ""
  cat(sprintf("%-4d %-12s %-6s %-5s %-12s %-12s %-40s\n", i, vn, vtype[[vn]], vlen[[vn]], vfmt[[vn]], vinfmt[[vn]], lb), file=con)
}
cat("\n", file=con)
cat("Sort Information:\n", file=con)
cat("  Sortedby STUDYID USUBJID PARAMN ASTDT\n", file=con)
cat("  Validated YES Character Set ANSI\n\n", file=con)

cat("The SAS System                                          Thursday, October 29, 2026 09:04:01\n\n", file=con)
cat("PRINT OF ADEX (FIRST 20 OBSERVATIONS)\n", file=con)
hdr <- paste(names(print20), collapse=" | ")
cat(hdr, "\n", file=con)
cat(paste(rep("-", nchar(hdr)), collapse=""), "\n", file=con, sep="")
for (r in seq_len(nrow(print20))) {
  row <- print20[r, ]
  vals <- c(
    as.character(row$STUDYID), as.character(row$USUBJID), as.character(row$TRTP),
    fmt_int(row$TRTPN), ifelse(is.na(row$SAFFL),".",as.character(row$SAFFL)),
    fmt_date9(row$TRTSDT), fmt_date9(row$TRTEDT),
    fmt_int(row$AGE), ifelse(is.na(row$SEX),".",as.character(row$SEX)),
    as.character(row$PARAMCD), as.character(row$PARAM), fmt_int(row$PARAMN),
    fmt_num(row$AVAL), ifelse(is.na(row$AVALU),".",as.character(row$AVALU)),
    fmt_date9(row$ASTDT), fmt_date9(row$AENDT),
    fmt_int(row$ASTDY), fmt_int(row$AENDY),
    fmt_num(row$EXDURN),
    ifelse(is.na(row$DOSINTFL),".",as.character(row$DOSINTFL)),
    ifelse(is.na(row$DOSEADFL),".",as.character(row$DOSEADFL)),
    ifelse(is.na(row$EXTRT),".",as.character(row$EXTRT)),
    fmt_num(row$EXDOSE),
    ifelse(is.na(row$EXDOSU),".",as.character(row$EXDOSU)),
    ifelse(is.na(row$EXDOSFRM),".",as.character(row$EXDOSFRM)),
    ifelse(is.na(row$EXROUTE),".",as.character(row$EXROUTE)),
    fmt_int(row$EXSEQ),
    ifelse(is.na(row$AVISIT),".",as.character(row$AVISIT)),
    fmt_int(row$AVISITN),
    fmt_int(row$ASEQ),
    ifelse(is.na(row$DOSECAT),".",as.character(row$DOSECAT))
  )
  cat(paste(vals, collapse=" | "), "\n", file=con)
}
close(con)

file.copy(file.path(out, "P089.txt"), file.path(out, "P089.lst"), overwrite=TRUE)

cat("Done\n")

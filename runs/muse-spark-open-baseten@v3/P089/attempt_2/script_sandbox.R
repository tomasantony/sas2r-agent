out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(haven)
library(lubridate)
library(purrr)
library(tibble)

# helper: SAS date9 parse e.g. 15JAN2024
parse_date9 <- function(x) {
  ifelse(is.na(x) | x=="" | x==".",
         NA_character_,
         as.character(as.Date(str_to_title(tolower(x)), format="%d%b%Y")))
}
parse_date9_date <- function(x) {
  sapply(x, function(v){
    if (is.na(v) || v=="" || v==".") return(NA)
    v2 <- paste0(substr(v,1,1), tolower(substr(v,2,nchar(v))))
    d <- as.Date(v2, format="%d%b%Y")
    d
  }, USE.NAMES=FALSE) |> as.Date(origin="1970-01-01")
}

fmt_date <- function(d) {
  ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
}

#==========================================================================
# STEP 1: Create mock SDTM EX data
#==========================================================================
ex_raw <- tibble::tribble(
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
ex <- ex_raw

#==========================================================================
# STEP 2: Create mock ADSL
#==========================================================================
adsl_raw <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~TRTP, ~TRTPN, ~AGE, ~SEX, ~SAFFL, ~TRTSDT_c, ~TRTEDT_c,
  "STUDY01","STUDY01-1001","Treatment A",1,59,"M","Y","15JAN2024","15JUN2024",
  "STUDY01","STUDY01-1002","Placebo",2,45,"F","Y","18JAN2024","18JUN2024",
  "STUDY01","STUDY01-1003","Treatment A",1,43,"M","Y","20JAN2024","01APR2024",
  "STUDY01","STUDY01-1004","Treatment A",1,51,"F","Y","25JAN2024","25JUN2024",
  "STUDY01","STUDY01-1005","Placebo",2,36,"M","Y","28JAN2024",".",
  "STUDY01","STUDY01-1006","Treatment A",1,68,"F","Y","30JAN2024","01JUL2024"
)
adsl <- adsl_raw %>%
  mutate(
    TRTSDT = parse_date9_date(TRTSDT_c),
    TRTEDT = parse_date9_date(TRTEDT_c)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT) %>%
  arrange(STUDYID, USUBJID)

#==========================================================================
# STEP 3: Merge and create base exposure variables
#==========================================================================
ex_sorted <- ex %>% arrange(STUDYID, USUBJID)
adsl_sorted <- adsl %>% arrange(STUDYID, USUBJID)

adex_base <- inner_join(ex_sorted, adsl_sorted, by=c("STUDYID","USUBJID")) %>%
  mutate(
    PARAMCD = "DOSE",
    PARAM = "Dose",
    PARAMN = 1,
    AVAL = EXDOSE,
    AVALC = NA_character_,
    AVALU = EXDOSU,
    ASTDT = as.Date(ifelse(EXSTDTC %in% c("", "."), NA, EXSTDTC), format="%Y-%m-%d"),
    AENDT = as.Date(ifelse(EXENDTC %in% c("", "."), NA, EXENDTC), format="%Y-%m-%d"),
    ASTDY = case_when(
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT ~ as.numeric(ASTDT - TRTSDT + 1),
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT < TRTSDT ~ as.numeric(ASTDT - TRTSDT),
      TRUE ~ NA_real_
    ),
    AENDY = case_when(
      !is.na(AENDT) & !is.na(TRTSDT) & AENDT >= TRTSDT ~ as.numeric(AENDT - TRTSDT + 1),
      !is.na(AENDT) & !is.na(TRTSDT) & AENDT < TRTSDT ~ as.numeric(AENDT - TRTSDT),
      TRUE ~ NA_real_
    ),
    AVISIT = VISIT,
    AVISITN = VISITNUM,
    EXDURN = ifelse(!is.na(ASTDT) & !is.na(AENDT), as.numeric(AENDT - ASTDT + 1), NA_real_),
    DOSINTFL = "N",
    DOSEADFL = "N"
  ) %>%
  select(-EXSTDTC, -EXENDTC, -VISIT, -VISITNUM)

# Ensure SAS varnum order: STUDYID USUBJID EXSEQ EXTRT EXDOSE EXDOSU EXDOSFRM EXROUTE TRTP TRTPN AGE SEX SAFFL TRTSDT TRTEDT PARAMCD PARAM PARAMN AVAL AVALC AVALU ASTDT AENDT ASTDY AENDY AVISIT AVISITN EXDURN DOSINTFL DOSEADFL
adex_base <- adex_base %>%
  select(STUDYID, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXROUTE,
         TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT,
         PARAMCD, PARAM, PARAMN, AVAL, AVALC, AVALU, ASTDT, AENDT, ASTDY, AENDY,
         AVISIT, AVISITN, EXDURN, DOSINTFL, DOSEADFL)

#==========================================================================
# STEP 4: Identify dose adjustments
#==========================================================================
adex_base <- adex_base %>% arrange(USUBJID, ASTDT)

adex_base <- adex_base %>%
  group_by(USUBJID) %>%
  mutate(.row = row_number()) %>% ungroup()

prev_dose <- NA_real_
prev_end <- as.Date(NA)
prev_usubjid <- NA_character_
dosintfl_new <- rep("N", nrow(adex_base))
doseadfl_new <- rep("N", nrow(adex_base))

for (i in seq_len(nrow(adex_base))) {
  cur_usubjid <- adex_base$USUBJID[i]
  cur_aval <- adex_base$AVAL[i]
  cur_astdt <- adex_base$ASTDT[i]
  cur_aendt <- adex_base$AENDT[i]
  is_first <- is.na(prev_usubjid) || cur_usubjid != prev_usubjid
  if (is_first) {
    prev_dose <- NA_real_
    prev_end <- as.Date(NA)
  }
  if (!is_first && !is.na(prev_dose) && !is.na(cur_aval) && cur_aval != prev_dose) {
    doseadfl_new[i] <- "Y"
  } else {
    doseadfl_new[i] <- adex_base$DOSEADFL[i]
  }
  if (!is_first && !is.na(prev_end) && !is.na(cur_astdt)) {
    if (as.numeric(cur_astdt - prev_end) > 1) {
      dosintfl_new[i] <- "Y"
    }
  }
  prev_dose <- cur_aval
  prev_end <- cur_aendt
  prev_usubjid <- cur_usubjid
}
adex_base$DOSINTFL <- dosintfl_new
adex_base$DOSEADFL <- doseadfl_new
adex_base <- adex_base %>% select(-.row)

#==========================================================================
# STEP 5: Create summary exposure parameters
#==========================================================================
totdur <- adex_base %>%
  group_by(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX) %>%
  summarise(AVAL = if (all(is.na(EXDURN))) NA_real_ else sum(EXDURN, na.rm=TRUE), .groups="drop") %>%
  mutate(PARAMCD="TOTDUR", PARAM="Total Exposure Duration (Days)", PARAMN=2, AVALU="days")

avgdose <- adex_base %>%
  filter(AVAL > 0) %>%
  group_by(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX) %>%
  summarise(AVAL = sum(AVAL * EXDURN, na.rm=TRUE) / sum(EXDURN, na.rm=TRUE), .groups="drop") %>%
  mutate(PARAMCD="AVGDOSE", PARAM="Average Daily Dose", PARAMN=3, AVALU="mg")

maxdose <- adex_base %>%
  filter(AVAL > 0) %>%
  group_by(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX) %>%
  summarise(AVAL = max(AVAL, na.rm=TRUE), .groups="drop") %>%
  mutate(PARAMCD="MAXDOSE", PARAM="Maximum Dose", PARAMN=4, AVALU="mg")

cumdose <- adex_base %>%
  filter(AVAL > 0) %>%
  group_by(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX) %>%
  summarise(AVAL = sum(AVAL * EXDURN, na.rm=TRUE), .groups="drop") %>%
  mutate(PARAMCD="CUMDOSE", PARAM="Cumulative Dose", PARAMN=5, AVALU="mg")

ndosadj <- adex_base %>%
  group_by(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX) %>%
  summarise(AVAL = sum(DOSEADFL=="Y", na.rm=TRUE), .groups="drop") %>%
  mutate(PARAMCD="NDOSADJ", PARAM="Number of Dose Adjustments", PARAMN=6, AVALU="")

#==========================================================================
# STEP 6: Combine all parameters
#==========================================================================
adex_base_keep <- adex_base %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX,
         PARAMCD, PARAM, PARAMN, AVAL, AVALU, ASTDT, AENDT, ASTDY, AENDY,
         EXDURN, DOSINTFL, DOSEADFL, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXROUTE, EXSEQ, AVISIT, AVISITN)

adex_combined <- bind_rows(adex_base_keep, totdur, avgdose, maxdose, cumdose, ndosadj)

#==========================================================================
# STEP 7: Add sequence and labels
#==========================================================================
adex_combined <- adex_combined %>% arrange(STUDYID, USUBJID, PARAMN, ASTDT)

adex <- adex_combined %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  mutate(
    DOSECAT = case_when(
      PARAMCD == "DOSE" & AVAL == 0 ~ "Placebo",
      PARAMCD == "DOSE" & AVAL <= 50 ~ "Low",
      PARAMCD == "DOSE" & AVAL <= 100 ~ "Medium",
      PARAMCD == "DOSE" & AVAL > 100 ~ "High",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(STUDYID, USUBJID, PARAMN, ASTDT)

attr(adex$STUDYID, "label") <- "Study Identifier"
attr(adex$USUBJID, "label") <- "Unique Subject Identifier"
attr(adex$ASEQ, "label") <- "Analysis Sequence Number"
attr(adex$PARAMCD, "label") <- "Parameter Code"
attr(adex$PARAM, "label") <- "Parameter"
attr(adex$PARAMN, "label") <- "Parameter (N)"
attr(adex$AVAL, "label") <- "Analysis Value"
attr(adex$AVALU, "label") <- "Analysis Value Unit"
attr(adex$ASTDT, "label") <- "Analysis Start Date"
attr(adex$AENDT, "label") <- "Analysis End Date"
attr(adex$ASTDY, "label") <- "Analysis Start Relative Day"
attr(adex$AENDY, "label") <- "Analysis End Relative Day"
attr(adex$AVISIT, "label") <- "Analysis Visit"
attr(adex$AVISITN, "label") <- "Analysis Visit (N)"
attr(adex$EXDURN, "label") <- "Exposure Duration (Days)"
attr(adex$EXTRT, "label") <- "Name of Treatment"
attr(adex$EXDOSE, "label") <- "Dose per Administration"
attr(adex$EXDOSU, "label") <- "Dose Units"
attr(adex$EXDOSFRM, "label") <- "Dose Form"
attr(adex$EXROUTE, "label") <- "Route of Administration"
attr(adex$DOSINTFL, "label") <- "Dose Interruption Flag"
attr(adex$DOSEADFL, "label") <- "Dose Adjustment Flag"
attr(adex$DOSECAT, "label") <- "Dose Category"
attr(adex$TRTP, "label") <- "Planned Treatment"
attr(adex$TRTPN, "label") <- "Planned Treatment (N)"
attr(adex$SAFFL, "label") <- "Safety Population Flag"

adex <- adex %>%
  select(STUDYID, USUBJID, ASEQ, PARAMCD, PARAM, PARAMN, AVAL, AVALU, ASTDT, AENDT, ASTDY, AENDY,
         AVISIT, AVISITN, EXDURN, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXROUTE, DOSINTFL, DOSEADFL, DOSECAT,
         TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX, EXSEQ)

#==========================================================================
# Write datasets as CSV with ISO dates
#==========================================================================
write_adam_csv <- function(df, path) {
  df2 <- df %>% mutate(across(where(~inherits(.x, "Date")), ~format(.x, "%Y-%m-%d")))
  df2 <- df2 %>% mutate(across(where(~inherits(.x, "POSIXt")), ~format(.x, "%Y-%m-%dT%H:%M:%S")))
  write.csv(df2, path, row.names=FALSE, na="")
}

write_adam_csv(adsl, file.path(out, "adsl.csv"))
write_adam_csv(adex_base, file.path(out, "adex_base.csv"))
write_adam_csv(adex, file.path(out, "adex.csv"))

#==========================================================================
# Create P089.lst text rendering (PROC CONTENTS + PROC PRINT obs=20)
#==========================================================================
contents_df <- tibble(
  Num = seq_along(names(adex)),
  Variable = names(adex),
  Label = sapply(names(adex), function(nm) {
    lbl <- attr(adex[[nm]], "label")
    if (is.null(lbl)) "" else lbl
  }),
  Type = sapply(adex, function(col) if (inherits(col, "Date")) "Num" else if (is.numeric(col)) "Num" else "Char"),
  Len = sapply(adex, function(col) if (is.numeric(col)) 8 else max(nchar(as.character(col)), na.rm=TRUE)),
  Format = sapply(names(adex), function(nm) if (nm %in% c("ASTDT","AENDT","TRTSDT","TRTEDT")) "DATE9." else "")
)

print_df <- adex %>% slice_head(n=20)

txt_path <- file.path(out, "P089.txt")
txt_path2 <- file.path(out, "P089.lst.txt")
lst_csv_path <- file.path(out, "P089.csv")
lst_csv_path2 <- file.path(out, "P089.lst.csv")

con <- file(txt_path, open="wt")
writeLines("Program: adex.sas", con)
writeLines("Purpose: Create ADEX (Exposure Analysis Dataset) - BDS Structure", con)
writeLines("Standard: CDISC ADaM IG 1.3", con)
writeLines("", con)
writeLines("PROC CONTENTS DATA=ADEX VARNUM;", con)
writeLines("", con)
writeLines(paste(rep("-", 80), collapse=""), con)
writeLines(sprintf("%-4s %-12s %-35s %-6s %-4s %s", "Num","Variable","Label","Type","Len","Format"), con)
writeLines(paste(rep("-", 80), collapse=""), con)
for (i in seq_len(nrow(contents_df))) {
  writeLines(sprintf("%-4d %-12s %-35s %-6s %-4d %s",
                     contents_df$Num[i],
                     contents_df$Variable[i],
                     substr(contents_df$Label[i],1,35),
                     contents_df$Type[i],
                     contents_df$Len[i],
                     contents_df$Format[i]), con)
}
writeLines(paste(rep("-", 80), collapse=""), con)
writeLines("", con)
writeLines(sprintf("Number of observations: %d  Number of variables: %d", nrow(adex), ncol(adex)), con)
writeLines("", con)
writeLines("PROC PRINT DATA=ADEX (OBS=20) NOOBS;", con)
writeLines("", con)
hdr <- paste(names(print_df), collapse=" | ")
writeLines(hdr, con)
writeLines(paste(rep("-", nchar(hdr)), collapse=""), con)
for (r in seq_len(nrow(print_df))) {
  row_vals <- sapply(print_df[r,], function(v) {
    if (inherits(v, "Date")) {
      if (is.na(v)) "" else format(v, "%d%b%Y") %>% toupper()
    } else if (is.numeric(v)) {
      if (is.na(v)) "" else format(v, trim=TRUE, scientific=FALSE)
    } else {
      if (is.na(v)) "" else as.character(v)
    }
  })
  writeLines(paste(row_vals, collapse=" | "), con)
}
writeLines("", con)
writeLines("Summary statistics for ADEX:", con)
summ_stats <- adex %>% filter(PARAMCD %in% c("TOTDUR","AVGDOSE","MAXDOSE","CUMDOSE","NDOSADJ")) %>%
  select(USUBJID, PARAMCD, AVAL)
for (i in seq_len(nrow(summ_stats))) {
  writeLines(sprintf("%s %s AVAL=%.4f", summ_stats$USUBJID[i], summ_stats$PARAMCD[i], summ_stats$AVAL[i]), con)
}
writeLines("", con)
writeLines(sprintf("Total records in ADEX: %d", nrow(adex)), con)
writeLines(sprintf("Total records in ADEX_BASE: %d", nrow(adex_base)), con)
writeLines(sprintf("Total subjects in ADSL: %d", nrow(adsl)), con)
close(con)
file.copy(txt_path, txt_path2, overwrite=TRUE)

write_adam_csv(print_df, lst_csv_path)
file.copy(lst_csv_path, lst_csv_path2, overwrite=TRUE)

write.csv(contents_df, file.path(out, "P089_contents.csv"), row.names=FALSE, na="")

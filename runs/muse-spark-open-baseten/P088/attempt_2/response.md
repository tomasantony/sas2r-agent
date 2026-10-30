```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(haven)
  library(data.table)
  library(ggplot2)
})

round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

#==========================================================================
# STEP 1: Create mock tumor response data RS
#==========================================================================
rs <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~RSSEQ, ~RSTESTCD, ~RSTEST, ~RSSTRESC, ~RSSTRESN, ~VISIT, ~VISITNUM, ~RSDTC,
  "STUDY01","STUDY01-1001",1,"OVRLRESP","Overall Response","STABLE DISEASE",NA_real_,"Baseline",1,"2024-01-15",
  "STUDY01","STUDY01-1001",2,"OVRLRESP","Overall Response","STABLE DISEASE",NA_real_,"Week 8",3,"2024-03-11",
  "STUDY01","STUDY01-1001",3,"OVRLRESP","Overall Response","PARTIAL RESPONSE",NA_real_,"Week 16",4,"2024-05-06",
  "STUDY01","STUDY01-1001",4,"OVRLRESP","Overall Response","PARTIAL RESPONSE",NA_real_,"Week 24",5,"2024-07-01",
  "STUDY01","STUDY01-1002",1,"OVRLRESP","Overall Response","STABLE DISEASE",NA_real_,"Baseline",1,"2024-01-18",
  "STUDY01","STUDY01-1002",2,"OVRLRESP","Overall Response","STABLE DISEASE",NA_real_,"Week 8",3,"2024-03-14",
  "STUDY01","STUDY01-1002",3,"OVRLRESP","Overall Response","PROGRESSIVE DISEASE",NA_real_,"Week 16",4,"2024-05-09",
  "STUDY01","STUDY01-1003",1,"OVRLRESP","Overall Response","STABLE DISEASE",NA_real_,"Baseline",1,"2024-01-20",
  "STUDY01","STUDY01-1003",2,"OVRLRESP","Overall Response","PARTIAL RESPONSE",NA_real_,"Week 8",3,"2024-03-16",
  "STUDY01","STUDY01-1003",3,"OVRLRESP","Overall Response","COMPLETE RESPONSE",NA_real_,"Week 16",4,"2024-05-11",
  "STUDY01","STUDY01-1004",1,"OVRLRESP","Overall Response","STABLE DISEASE",NA_real_,"Baseline",1,"2024-01-25",
  "STUDY01","STUDY01-1004",2,"OVRLRESP","Overall Response","STABLE DISEASE",NA_real_,"Week 8",3,"2024-03-21",
  "STUDY01","STUDY01-1005",1,"OVRLRESP","Overall Response","STABLE DISEASE",NA_real_,"Baseline",1,"2024-01-28",
  "STUDY01","STUDY01-1005",2,"OVRLRESP","Overall Response","PROGRESSIVE DISEASE",NA_real_,"Week 8",3,"2024-03-24",
  "STUDY01","STUDY01-1006",1,"OVRLRESP","Overall Response","STABLE DISEASE",NA_real_,"Baseline",1,"2024-01-30",
  "STUDY01","STUDY01-1006",2,"OVRLRESP","Overall Response","PARTIAL RESPONSE",NA_real_,"Week 8",3,"2024-03-26"
)

tu <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~TUSEQ, ~TUTESTCD, ~TUTEST, ~TUORRES, ~TUSTRESC, ~TUSTRESN, ~TUSTRESU, ~TULOC, ~VISIT, ~VISITNUM, ~TUDTC,
  "STUDY01","STUDY01-1001",1,"LDIAM","Longest Diameter","45","45",45,"mm","LIVER","Baseline",1,"2024-01-15",
  "STUDY01","STUDY01-1001",2,"LDIAM","Longest Diameter","42","42",42,"mm","LIVER","Week 8",3,"2024-03-11",
  "STUDY01","STUDY01-1001",3,"LDIAM","Longest Diameter","35","35",35,"mm","LIVER","Week 16",4,"2024-05-06",
  "STUDY01","STUDY01-1001",4,"LDIAM","Longest Diameter","30","30",30,"mm","LIVER","Week 24",5,"2024-07-01",
  "STUDY01","STUDY01-1002",1,"LDIAM","Longest Diameter","38","38",38,"mm","LUNG","Baseline",1,"2024-01-18",
  "STUDY01","STUDY01-1002",2,"LDIAM","Longest Diameter","40","40",40,"mm","LUNG","Week 8",3,"2024-03-14",
  "STUDY01","STUDY01-1002",3,"LDIAM","Longest Diameter","52","52",52,"mm","LUNG","Week 16",4,"2024-05-09",
  "STUDY01","STUDY01-1003",1,"LDIAM","Longest Diameter","55","55",55,"mm","LYMPH NODE","Baseline",1,"2024-01-20",
  "STUDY01","STUDY01-1003",2,"LDIAM","Longest Diameter","40","40",40,"mm","LYMPH NODE","Week 8",3,"2024-03-16",
  "STUDY01","STUDY01-1003",3,"LDIAM","Longest Diameter","0","0",0,"mm","LYMPH NODE","Week 16",4,"2024-05-11",
  "STUDY01","STUDY01-1004",1,"LDIAM","Longest Diameter","32","32",32,"mm","LIVER","Baseline",1,"2024-01-25",
  "STUDY01","STUDY01-1004",2,"LDIAM","Longest Diameter","30","30",30,"mm","LIVER","Week 8",3,"2024-03-21",
  "STUDY01","STUDY01-1005",1,"LDIAM","Longest Diameter","28","28",28,"mm","LUNG","Baseline",1,"2024-01-28",
  "STUDY01","STUDY01-1005",2,"LDIAM","Longest Diameter","42","42",42,"mm","LUNG","Week 8",3,"2024-03-24",
  "STUDY01","STUDY01-1006",1,"LDIAM","Longest Diameter","48","48",48,"mm","LIVER","Baseline",1,"2024-01-30",
  "STUDY01","STUDY01-1006",2,"LDIAM","Longest Diameter","36","36",36,"mm","LIVER","Week 8",3,"2024-03-26"
)

#==========================================================================
# STEP 2: Create mock ADSL
#==========================================================================
adsl <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~TRTP, ~TRTPN, ~AGE, ~SEX, ~SAFFL, ~EFFFL, ~TRTSDT_raw, ~TRTEDT_raw,
  "STUDY01","STUDY01-1001","Treatment A",1,59,"M","Y","Y","15JAN2024","15JUN2024",
  "STUDY01","STUDY01-1002","Placebo",2,45,"F","Y","Y","18JAN2024","18JUN2024",
  "STUDY01","STUDY01-1003","Treatment A",1,43,"M","Y","Y","20JAN2024","01APR2024",
  "STUDY01","STUDY01-1004","Treatment A",1,51,"F","Y","Y","25JAN2024","25JUN2024",
  "STUDY01","STUDY01-1005","Placebo",2,36,"M","Y","Y","28JAN2024",NA_character_,
  "STUDY01","STUDY01-1006","Treatment A",1,68,"F","Y","Y","30JAN2024","01JUL2024"
) %>%
  mutate(
    TRTSDT = as.Date(TRTSDT_raw, format = "%d%b%Y"),
    TRTEDT = as.Date(TRTEDT_raw, format = "%d%b%Y")
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, EFFFL, TRTSDT, TRTEDT)

#==========================================================================
# STEP 3: Create tumor response parameter (OVRLRESP)
#==========================================================================
rs_filt <- rs %>% filter(RSTESTCD == "OVRLRESP")

adeff_resp <- rs_filt %>%
  inner_join(adsl, by = c("STUDYID","USUBJID")) %>%
  mutate(
    PARAMCD = "OVRLRESP",
    PARAM = "Overall Response",
    PARAMN = 1,
    AVALC = RSSTRESC,
    AVAL = case_when(
      toupper(RSSTRESC) == "COMPLETE RESPONSE" ~ 1,
      toupper(RSSTRESC) == "PARTIAL RESPONSE" ~ 2,
      toupper(RSSTRESC) == "STABLE DISEASE" ~ 3,
      toupper(RSSTRESC) == "PROGRESSIVE DISEASE" ~ 4,
      toupper(RSSTRESC) == "NOT EVALUABLE" ~ 5,
      TRUE ~ NA_real_
    ),
    ADT = as.Date(RSDTC, format = "%Y-%m-%d"),
    ADY = case_when(
      !is.na(ADT) & !is.na(TRTSDT) & ADT >= TRTSDT ~ as.numeric(ADT - TRTSDT + 1),
      !is.na(ADT) & !is.na(TRTSDT) & ADT < TRTSDT ~ as.numeric(ADT - TRTSDT),
      TRUE ~ NA_real_
    ),
    AVISIT = VISIT,
    AVISITN = VISITNUM,
    ABLFL = ifelse(AVISIT == "Baseline", "Y", ""),
    ANL01FL = ifelse(ABLFL != "Y" & !is.na(AVAL), "Y", "")
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, EFFFL, TRTSDT, TRTEDT, AGE, SEX,
         PARAMCD, PARAM, PARAMN, AVAL, AVALC, ADT, ADY, AVISIT, AVISITN, ABLFL, ANL01FL)

#==========================================================================
# STEP 4: Create tumor size parameters
#==========================================================================
tu_filt <- tu %>% filter(TUTESTCD == "LDIAM")

adeff_tumor <- tu_filt %>%
  inner_join(adsl, by = c("STUDYID","USUBJID")) %>%
  mutate(
    PARAMCD = "SUMDIAM",
    PARAM = "Sum of Longest Diameters",
    PARAMN = 2,
    AVAL = TUSTRESN,
    AVALU = TUSTRESU,
    AVALC = NA_character_,
    ADT = as.Date(TUDTC, format = "%Y-%m-%d"),
    ADY = case_when(
      !is.na(ADT) & !is.na(TRTSDT) & ADT >= TRTSDT ~ as.numeric(ADT - TRTSDT + 1),
      !is.na(ADT) & !is.na(TRTSDT) & ADT < TRTSDT ~ as.numeric(ADT - TRTSDT),
      TRUE ~ NA_real_
    ),
    AVISIT = VISIT,
    AVISITN = VISITNUM,
    ABLFL = ifelse(AVISIT == "Baseline", "Y", ""),
    ANL01FL = ifelse(ABLFL != "Y" & !is.na(AVAL), "Y", "")
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, EFFFL, TRTSDT, TRTEDT, AGE, SEX,
         PARAMCD, PARAM, PARAMN, AVAL, AVALC, AVALU, ADT, ADY, AVISIT, AVISITN, ABLFL, ANL01FL)

#==========================================================================
# STEP 5: Derive baseline and change from baseline for tumor
#==========================================================================
base_tumor <- adeff_tumor %>%
  filter(ABLFL == "Y") %>%
  mutate(BASE = AVAL) %>%
  select(USUBJID, PARAMCD, BASE)

adeff_tumor <- adeff_tumor %>%
  arrange(USUBJID, PARAMCD) %>%
  left_join(base_tumor, by = c("USUBJID","PARAMCD")) %>%
  mutate(
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    PCHG = ifelse(!is.na(AVAL) & !is.na(BASE) & BASE > 0, (CHG / BASE) * 100, NA_real_)
  )

#==========================================================================
# STEP 6: Derive best overall response (BOR)
#==========================================================================
resp_sorted <- adeff_resp %>%
  filter(ABLFL != "Y") %>%
  arrange(USUBJID, AVAL)

bor <- resp_sorted %>%
  group_by(USUBJID) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    PARAMCD = "BOR",
    PARAM = "Best Overall Response",
    PARAMN = 3,
    AVISIT = "Best Response",
    AVISITN = 99,
    ABLFL = "",
    ANL01FL = "Y"
  )

#==========================================================================
# STEP 7: Derive responder flag - SAS retains ADT/ADY from _bor
#==========================================================================
respfl <- bor %>%
  mutate(
    RESPFL = ifelse(AVAL %in% c(1,2), "Y", "N"),
    PARAMCD = "RESPFL",
    PARAM = "Objective Response Flag",
    PARAMN = 4,
    AVAL = ifelse(RESPFL == "Y", 1, 0),
    AVALC = RESPFL
  )
# Do NOT overwrite ADT/ADY - SAS retains them from _bor

#==========================================================================
# STEP 8: Derive minimum tumor size (nadir)
#==========================================================================
nadir_min <- adeff_tumor %>%
  filter(ABLFL != "Y") %>%
  group_by(USUBJID) %>%
  summarise(AVAL = min(AVAL, na.rm = TRUE), .groups = "drop")

nadir_base_info <- adeff_tumor %>%
  filter(ABLFL == "Y") %>%
  select(USUBJID, STUDYID, TRTP, TRTPN, SAFFL, EFFFL, TRTSDT, TRTEDT, AGE, SEX, BASE, ABLFL)

nadir <- nadir_min %>%
  left_join(nadir_base_info, by = "USUBJID") %>%
  mutate(
    PARAMCD = "NADIRTUM",
    PARAM = "Nadir Tumor Size",
    PARAMN = 5,
    AVALU = "mm",
    AVALC = NA_character_,
    AVISIT = "Nadir",
    AVISITN = 98,
    ABLFL = "",
    ANL01FL = "Y",
    ADT = as.Date(NA),
    ADY = NA_real_,
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    PCHG = ifelse(!is.na(AVAL) & !is.na(BASE) & BASE > 0, (CHG / BASE) * 100, NA_real_)
  )

# Add placeholder columns for consistent bind
adeff_resp$RESPFL <- NA_character_
adeff_resp$AVALU <- NA_character_
adeff_resp$BASE <- NA_real_
adeff_resp$CHG <- NA_real_
adeff_resp$PCHG <- NA_real_

adeff_tumor$RESPFL <- NA_character_

bor$RESPFL <- NA_character_
bor$AVALU <- NA_character_
bor$BASE <- NA_real_
bor$CHG <- NA_real_
bor$PCHG <- NA_real_

respfl$AVALU <- NA_character_
respfl$BASE <- NA_real_
respfl$CHG <- NA_real_
respfl$PCHG <- NA_real_
# ADT/ADY retained from bor - do not overwrite

nadir$RESPFL <- NA_character_

#==========================================================================
# STEP 9: Combine all efficacy parameters
#==========================================================================
adeff <- bind_rows(adeff_resp, adeff_tumor, bor, respfl, nadir)

#==========================================================================
# STEP 10: Add sequence and labels
#==========================================================================
adeff <- adeff %>%
  arrange(STUDYID, USUBJID, PARAMN, AVISITN, ADT) %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  mutate(
    RSCAT = case_when(
      PARAMCD %in% c("OVRLRESP","BOR") & AVALC %in% c("COMPLETE RESPONSE","PARTIAL RESPONSE") ~ "RESPONDER",
      PARAMCD %in% c("OVRLRESP","BOR") & AVALC == "STABLE DISEASE" ~ "STABLE",
      PARAMCD %in% c("OVRLRESP","BOR") & AVALC == "PROGRESSIVE DISEASE" ~ "PROGRESSOR",
      PARAMCD %in% c("OVRLRESP","BOR") ~ "OTHER",
      TRUE ~ NA_character_
    )
  )

adeff <- adeff %>%
  select(STUDYID, USUBJID, ASEQ, PARAMCD, PARAM, PARAMN, AVAL, AVALC, AVALU, BASE, CHG, PCHG,
         ADT, ADY, AVISIT, AVISITN, ABLFL, ANL01FL, RSCAT, TRTP, TRTPN, EFFFL, SAFFL,
         TRTSDT, TRTEDT, AGE, SEX, RESPFL, everything())

attr(adeff$STUDYID, "label") <- "Study Identifier"
attr(adeff$USUBJID, "label") <- "Unique Subject Identifier"
attr(adeff$ASEQ, "label") <- "Analysis Sequence Number"
attr(adeff$PARAMCD, "label") <- "Parameter Code"
attr(adeff$PARAM, "label") <- "Parameter"
attr(adeff$PARAMN, "label") <- "Parameter (N)"
attr(adeff$AVAL, "label") <- "Analysis Value"
attr(adeff$AVALC, "label") <- "Analysis Value (C)"
attr(adeff$AVALU, "label") <- "Analysis Value Unit"
attr(adeff$BASE, "label") <- "Baseline Value"
attr(adeff$CHG, "label") <- "Change from Baseline"
attr(adeff$PCHG, "label") <- "Percent Change from Baseline"
attr(adeff$ADT, "label") <- "Analysis Date"
attr(adeff$ADY, "label") <- "Analysis Relative Day"
attr(adeff$AVISIT, "label") <- "Analysis Visit"
attr(adeff$AVISITN, "label") <- "Analysis Visit (N)"
attr(adeff$ABLFL, "label") <- "Baseline Record Flag"
attr(adeff$ANL01FL, "label") <- "Analysis Record Flag 01"
attr(adeff$RSCAT, "label") <- "Response Category"
attr(adeff$TRTP, "label") <- "Planned Treatment"
attr(adeff$TRTPN, "label") <- "Planned Treatment (N)"
attr(adeff$EFFFL, "label") <- "Efficacy Population Flag"
attr(adeff$SAFFL, "label") <- "Safety Population Flag"

#==========================================================================
# Write outputs
#==========================================================================
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(adeff_resp, file.path(out, "adeff_resp.csv"), row.names = FALSE, na = "")
write.csv(adeff_tumor, file.path(out, "adeff_tumor.csv"), row.names = FALSE, na = "")
write.csv(adeff, file.path(out, "adeff.csv"), row.names = FALSE, na = "")

#==========================================================================
# Create P088.lst text rendering (PROC CONTENTS + PROC PRINT)
#==========================================================================
contents_df <- tibble(
  VarNum = seq_along(names(adeff)),
  Variable = names(adeff),
  Label = sapply(names(adeff), function(x) {
    lbl <- attr(adeff[[x]], "label")
    if (is.null(lbl)) "" else lbl
  }),
  Type = sapply(adeff, function(col) if (inherits(col, "Date")) "Num" else if (is.numeric(col)) "Num" else "Char"),
  Len = sapply(adeff, function(col) if (is.character(col)) max(nchar(col), na.rm=TRUE) else 8),
  Format = sapply(adeff, function(col) if (inherits(col, "Date")) "DATE9." else "")
)

format_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, "%d%b%Y")))
}

txt_path <- file.path(out, "P088.lst.txt")
txt_path2 <- file.path(out, "P088.txt")
csv_path <- file.path(out, "P088.lst.csv")

write.csv(adeff, csv_path, row.names = FALSE, na = "")

con <- file(txt_path, open = "wt")
writeLines("Program: P088_adeff.sas  Dataset: ADEFF (Efficacy Analysis Dataset - BDS Structure)", con)
writeLines(paste(rep("=", 100), collapse=""), con)
writeLines("", con)
writeLines("CONTENTS OF ADEFF", con)
writeLines(paste(rep("-", 100), collapse=""), con)
writeLines(sprintf("%-6s %-12s %-30s %-6s %-6s %-10s", "VarNum","Variable","Label","Type","Len","Format"), con)
writeLines(paste(rep("-", 100), collapse=""), con)
for (i in seq_len(nrow(contents_df))) {
  writeLines(sprintf("%-6d %-12s %-30s %-6s %-6d %-10s",
                     contents_df$VarNum[i],
                     contents_df$Variable[i],
                     substr(contents_df$Label[i],1,30),
                     contents_df$Type[i],
                     contents_df$Len[i],
                     contents_df$Format[i]), con)
}
writeLines("", con)
writeLines(paste(rep("=", 100), collapse=""), con)
writeLines("PRINT OF ADEFF", con)
writeLines(paste(rep("-", 100), collapse=""), con)
hdr <- sprintf("%-12s %-15s %-4s %-10s %-6s %-20s %-6s %-12s %-6s %-6s %-10s %-10s %-12s %-6s %-10s %-6s %-6s %-6s %-12s %-15s %-6s %-6s",
               "STUDYID","USUBJID","ASEQ","PARAMCD","PARAMN","PARAM","AVAL","AVALC","AVALU","BASE","CHG","PCHG","ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL","RSCAT","TRTP","TRTPN","EFFFL")
writeLines(hdr, con)
writeLines(paste(rep("-", 150), collapse=""), con)
for (i in seq_len(nrow(adeff))) {
  row <- adeff[i,]
  vals <- c(
    row$STUDYID, row$USUBJID, row$ASEQ, row$PARAMCD, row$PARAMN,
    row$PARAM, row$AVAL, row$AVALC, row$AVALU, row$BASE, row$CHG, row$PCHG,
    format_date9(row$ADT), row$ADY, row$AVISIT, row$AVISITN, row$ABLFL, row$ANL01FL,
    row$RSCAT, row$TRTP, row$TRTPN, row$EFFFL, row$SAFFL, format_date9(row$TRTSDT), format_date9(row$TRTEDT)
  )
  line2 <- paste(sapply(vals, function(v) ifelse(is.na(v),"",as.character(v))), collapse=" | ")
  writeLines(line2, con)
}
writeLines("", con)
writeLines(paste("Total observations:", nrow(adeff)), con)
writeLines(paste("Total variables:", ncol(adeff)), con)
close(con)

file.copy(txt_path, txt_path2, overwrite = TRUE)
file.copy(txt_path, file.path(out, "P088.lst"), overwrite = TRUE)
```
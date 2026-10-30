# P088_adeff.sas -> R translation
# Output datasets: adeff, adeff_resp, adeff_tumor, adsl + P088 listing
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(data.table)
})

# SAS round half away from zero helper (not strictly needed here but required)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# DATE9 formatter for text rendering (SAS DATE9. : 15JAN2024)
format_date9 <- function(d) {
  ifelse(is.na(d), "",
         toupper(format(d, "%d%b%Y")))
}
# ISO formatter for CSV
format_iso_date <- function(d) {
  ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
}

# Helper to parse DATE9 strings like 15JAN2024
parse_date9 <- function(x) {
  # x is character vector, "." or "" => NA
  sapply(x, function(v) {
    if (is.na(v) || v == "" || v == "." ) return(NA_character_)
    v <- trimws(v)
    # convert JAN -> Jan for R parsing
    # v format: DDMMMYYYY e.g. 15JAN2024
    if (nchar(v) < 9) return(NA_character_)
    day <- substr(v, 1, 2)
    mon <- substr(v, 3, 5)
    yr  <- substr(v, 6, 9)
    mon2 <- paste0(toupper(substr(mon,1,1)), tolower(substr(mon,2,3)))
    s2 <- paste0(day, mon2, yr)
    d <- as.Date(s2, format = "%d%b%Y")
    as.character(d)
  }, USE.NAMES = FALSE) |> as.Date()
}

#==========================================================================
# STEP 1: mock RS
#==========================================================================
rs <- tibble::tibble(
  STUDYID = rep("STUDY01", 16),
  USUBJID = c("STUDY01-1001","STUDY01-1001","STUDY01-1001","STUDY01-1001",
              "STUDY01-1002","STUDY01-1002","STUDY01-1002",
              "STUDY01-1003","STUDY01-1003","STUDY01-1003",
              "STUDY01-1004","STUDY01-1004",
              "STUDY01-1005","STUDY01-1005",
              "STUDY01-1006","STUDY01-1006"),
  RSSEQ = c(1,2,3,4, 1,2,3, 1,2,3, 1,2, 1,2, 1,2),
  RSTESTCD = rep("OVRLRESP", 16),
  RSTEST = rep("Overall Response", 16),
  RSSTRESC = c("STABLE DISEASE","STABLE DISEASE","PARTIAL RESPONSE","PARTIAL RESPONSE",
               "STABLE DISEASE","STABLE DISEASE","PROGRESSIVE DISEASE",
               "STABLE DISEASE","PARTIAL RESPONSE","COMPLETE RESPONSE",
               "STABLE DISEASE","STABLE DISEASE",
               "STABLE DISEASE","PROGRESSIVE DISEASE",
               "STABLE DISEASE","PARTIAL RESPONSE"),
  RSSTRESN = rep(NA_real_, 16),
  VISIT = c("Baseline","Week 8","Week 16","Week 24",
            "Baseline","Week 8","Week 16",
            "Baseline","Week 8","Week 16",
            "Baseline","Week 8",
            "Baseline","Week 8",
            "Baseline","Week 8"),
  VISITNUM = c(1,3,4,5, 1,3,4, 1,3,4, 1,3, 1,3, 1,3),
  RSDTC = c("2024-01-15","2024-03-11","2024-05-06","2024-07-01",
            "2024-01-18","2024-03-14","2024-05-09",
            "2024-01-20","2024-03-16","2024-05-11",
            "2024-01-25","2024-03-21",
            "2024-01-28","2024-03-24",
            "2024-01-30","2024-03-26")
)

#--- Tumor measurements ---
tu <- tibble::tibble(
  STUDYID = rep("STUDY01", 16),
  USUBJID = c("STUDY01-1001","STUDY01-1001","STUDY01-1001","STUDY01-1001",
              "STUDY01-1002","STUDY01-1002","STUDY01-1002",
              "STUDY01-1003","STUDY01-1003","STUDY01-1003",
              "STUDY01-1004","STUDY01-1004",
              "STUDY01-1005","STUDY01-1005",
              "STUDY01-1006","STUDY01-1006"),
  TUSEQ = c(1,2,3,4, 1,2,3, 1,2,3, 1,2, 1,2, 1,2),
  TUTESTCD = rep("LDIAM", 16),
  TUTEST = rep("Longest Diameter", 16),
  TUORRES = c("45","42","35","30","38","40","52","55","40","0","32","30","28","42","48","36"),
  TUSTRESC = c("45","42","35","30","38","40","52","55","40","0","32","30","28","42","48","36"),
  TUSTRESN = c(45,42,35,30,38,40,52,55,40,0,32,30,28,42,48,36),
  TUSTRESU = rep("mm", 16),
  TULOC = c("LIVER","LIVER","LIVER","LIVER","LUNG","LUNG","LUNG","LYMPH NODE","LYMPH NODE","LYMPH NODE","LIVER","LIVER","LUNG","LUNG","LIVER","LIVER"),
  VISIT = c("Baseline","Week 8","Week 16","Week 24",
            "Baseline","Week 8","Week 16",
            "Baseline","Week 8","Week 16",
            "Baseline","Week 8",
            "Baseline","Week 8",
            "Baseline","Week 8"),
  VISITNUM = c(1,3,4,5, 1,3,4, 1,3,4, 1,3, 1,3, 1,3),
  TUDTC = c("2024-01-15","2024-03-11","2024-05-06","2024-07-01",
            "2024-01-18","2024-03-14","2024-05-09",
            "2024-01-20","2024-03-16","2024-05-11",
            "2024-01-25","2024-03-21",
            "2024-01-28","2024-03-24",
            "2024-01-30","2024-03-26")
)

#==========================================================================
# STEP 2: mock ADSL
#==========================================================================
adsl_raw <- tibble::tibble(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  SEX = c("M","F","M","F","M","F"),
  SAFFL = rep("Y",6),
  EFFFL = rep("Y",6),
  TRTSDT_c = c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"),
  TRTEDT_c = c("15JUN2024","18JUN2024","01APR2024","25JUN2024",".","01JUL2024")
)
adsl <- adsl_raw %>%
  mutate(
    TRTSDT = parse_date9(TRTSDT_c),
    TRTEDT = parse_date9(TRTEDT_c)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, TRTSDT, TRTEDT, SAFFL, EFFFL)

# Ensure SAS var order for ADSL: STUDYID USUBJID TRTP TRTPN AGE SEX TRTSDT TRTEDT SAFFL EFFFL
adsl <- adsl %>% select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, TRTSDT, TRTEDT, SAFFL, EFFFL)

#==========================================================================
# STEP 3: tumor response parameter (OVRLRESP)
#==========================================================================
# proc sort data=rs; by STUDYID USUBJID; run; proc sort data=adsl; by STUDYID USUBJID; run;
rs_s <- rs %>% arrange(STUDYID, USUBJID)
adsl_s <- adsl %>% arrange(STUDYID, USUBJID)

adeff_resp <- rs %>%
  filter(RSTESTCD == "OVRLRESP") %>%
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
    ADY = ifelse(!is.na(ADT) & !is.na(TRTSDT),
                 ifelse(ADT >= TRTSDT, as.integer(ADT - TRTSDT + 1L), as.integer(ADT - TRTSDT)),
                 NA_integer_),
    AVISIT = VISIT,
    AVISITN = VISITNUM,
    ABLFL = ifelse(AVISIT == "Baseline", "Y", ""),
    ANL01FL = ifelse(ABLFL != "Y" & !is.na(AVAL), "Y", "")
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, EFFFL, TRTSDT, TRTEDT, AGE, SEX,
         PARAMCD, PARAM, PARAMN, AVAL, AVALC, ADT, ADY, AVISIT, AVISITN, ABLFL, ANL01FL)

#==========================================================================
# STEP 4: tumor size parameters
#==========================================================================
tu_s <- tu %>% arrange(STUDYID, USUBJID)

adeff_tumor <- tu %>%
  filter(TUTESTCD == "LDIAM") %>%
  inner_join(adsl, by = c("STUDYID","USUBJID")) %>%
  mutate(
    PARAMCD = "SUMDIAM",
    PARAM = "Sum of Longest Diameters",
    PARAMN = 2,
    AVAL = TUSTRESN,
    AVALU = TUSTRESU,
    AVALC = NA_character_,
    ADT = as.Date(TUDTC, format = "%Y-%m-%d"),
    ADY = ifelse(!is.na(ADT) & !is.na(TRTSDT),
                 ifelse(ADT >= TRTSDT, as.integer(ADT - TRTSDT + 1L), as.integer(ADT - TRTSDT)),
                 NA_integer_),
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
  slice(1) %>%
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
# STEP 7: Derive responder flag
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

#==========================================================================
# STEP 8: Derive minimum tumor size (nadir)
#==========================================================================
# proc means min
nadir_min <- adeff_tumor %>%
  filter(ABLFL != "Y") %>%
  group_by(USUBJID) %>%
  summarise(AVAL = min(AVAL, na.rm = TRUE), .groups = "drop")

# baseline info for merge
base_info <- adeff_tumor %>%
  filter(ABLFL == "Y") %>%
  select(USUBJID, STUDYID, TRTP, TRTPN, SAFFL, EFFFL, TRTSDT, TRTEDT, AGE, SEX, BASE, ABLFL)

nadir <- nadir_min %>%
  left_join(base_info, by = "USUBJID") %>%
  mutate(
    PARAMCD = "NADIRTUM",
    PARAM = "Nadir Tumor Size",
    PARAMN = 5,
    AVALU = "mm",
    AVISIT = "Nadir",
    AVISITN = 98,
    ABLFL = "",
    ANL01FL = "Y",
    AVALC = NA_character_,
    ADT = as.Date(NA),
    ADY = NA_integer_,
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    PCHG = ifelse(!is.na(AVAL) & !is.na(BASE) & BASE > 0, (CHG / BASE) * 100, NA_real_)
  )

#==========================================================================
# STEP 9: Combine all efficacy parameters
#==========================================================================
adeff_combined <- bind_rows(adeff_resp, adeff_tumor, bor, respfl, nadir)

#==========================================================================
# STEP 10: Add sequence and labels
#==========================================================================
# proc sort data=adeff; by STUDYID USUBJID PARAMN AVISITN ADT; run;
adeff_sorted <- adeff_combined %>%
  arrange(STUDYID, USUBJID, PARAMN, AVISITN, desc(is.na(ADT)), ADT)

# by STUDYID USUBJID ASEQ
adeff <- adeff_sorted %>%
  group_by(STUDYID, USUBJID) %>%
  arrange(STUDYID, USUBJID, PARAMN, AVISITN, desc(is.na(ADT)), ADT, .by_group = TRUE) %>%
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

# Ensure column order matching SAS varnum: STUDYID USUBJID TRTP TRTPN SAFFL EFFFL TRTSDT TRTEDT AGE SEX
# PARAMCD PARAM PARAMN AVAL AVALC AVALU BASE CHG PCHG ADT ADY AVISIT AVISITN ABLFL ANL01FL RESPFL ASEQ RSCAT
# Some columns may be missing in some rows; ensure all exist
all_cols <- c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","EFFFL","TRTSDT","TRTEDT","AGE","SEX",
              "PARAMCD","PARAM","PARAMN","AVAL","AVALC","AVALU","BASE","CHG","PCHG",
              "ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL","RESPFL","ASEQ","RSCAT")
for (cn in all_cols) {
  if (!cn %in% names(adeff)) adeff[[cn]] <- NA
}
adeff <- adeff %>% select(all_of(all_cols))

# Also ensure adeff_resp and adeff_tumor have consistent columns for output
# adeff_resp output should be as originally kept (without BASE etc) but we will output the version before combine
# For consistency with SAS, adeff_resp.csv should contain the 21 columns as kept
# adeff_tumor.csv should contain BASE CHG PCHG

# Prepare CSV-ready data frames with ISO dates
to_csv_df <- function(df) {
  df2 <- df
  # convert Date columns to ISO character
  date_cols <- names(df2)[sapply(df2, function(x) inherits(x, "Date"))]
  for (dc in date_cols) {
    df2[[dc]] <- format_iso_date(df2[[dc]])
  }
  # convert POSIXct if any
  dt_cols <- names(df2)[sapply(df2, function(x) inherits(x, "POSIXct") || inherits(x, "POSIXt"))]
  for (dc in dt_cols) {
    df2[[dc]] <- ifelse(is.na(df2[[dc]]), "", format(df2[[dc]], "%Y-%m-%dT%H:%M:%S"))
  }
  df2
}

# Write datasets
write.csv(to_csv_df(adsl), file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(to_csv_df(adeff_resp), file.path(out, "adeff_resp.csv"), row.names = FALSE, na = "")
write.csv(to_csv_df(adeff_tumor), file.path(out, "adeff_tumor.csv"), row.names = FALSE, na = "")
write.csv(to_csv_df(adeff), file.path(out, "adeff.csv"), row.names = FALSE, na = "")

#==========================================================================
# P088.lst text rendering (PROC CONTENTS + PROC PRINT)
#==========================================================================
# Build contents data
labels <- c(
  STUDYID="Study Identifier",
  USUBJID="Unique Subject Identifier",
  ASEQ="Analysis Sequence Number",
  PARAMCD="Parameter Code",
  PARAM="Parameter",
  PARAMN="Parameter (N)",
  AVAL="Analysis Value",
  AVALC="Analysis Value (C)",
  AVALU="Analysis Value Unit",
  BASE="Baseline Value",
  CHG="Change from Baseline",
  PCHG="Percent Change from Baseline",
  ADT="Analysis Date",
  ADY="Analysis Relative Day",
  AVISIT="Analysis Visit",
  AVISITN="Analysis Visit (N)",
  ABLFL="Baseline Record Flag",
  ANL01FL="Analysis Record Flag 01",
  RSCAT="Response Category",
  TRTP="Planned Treatment",
  TRTPN="Planned Treatment (N)",
  EFFFL="Efficacy Population Flag",
  SAFFL="Safety Population Flag",
  RESPFL="Responder Flag",
  AGE="Age",
  SEX="Sex",
  TRTSDT="Treatment Start Date",
  TRTEDT="Treatment End Date"
)

# Determine types/lengths for contents (approximate SAS)
contents_df <- tibble(
  Num = seq_along(all_cols),
  Variable = all_cols,
  Type = ifelse(all_cols %in% c("TRTPN","PARAMN","AVAL","BASE","CHG","PCHG","ADY","AVISITN","ASEQ","AGE","TRTSDT","TRTEDT","ADT"), "Num", "Char"),
  Len = case_when(
    Variable %in% c("STUDYID") ~ 12,
    Variable %in% c("USUBJID") ~ 20,
    Variable %in% c("TRTP") ~ 40,
    Variable %in% c("PARAM") ~ 200,
    Variable %in% c("PARAMCD") ~ 8,
    Variable %in% c("AVALC") ~ 40,
    Variable %in% c("AVALU") ~ 20,
    Variable %in% c("AVISIT") ~ 40,
    Variable %in% c("RSCAT") ~ 30,
    Variable %in% c("ABLFL","ANL01FL","RESPFL","SAFFL","EFFFL","SEX") ~ 1,
    TRUE ~ 8
  ),
  Format = case_when(
    Variable %in% c("ADT","TRTSDT","TRTEDT") ~ "DATE9.",
    TRUE ~ ""
  ),
  Label = ifelse(Variable %in% names(labels), labels[Variable], "")
)

# Build print data for text: format dates as DATE9.
adeff_txt <- adeff %>%
  mutate(
    TRTSDT_TXT = format_date9(TRTSDT),
    TRTEDT_TXT = format_date9(TRTEDT),
    ADT_TXT = format_date9(ADT)
  )

# Create text file
txt_path <- file.path(out, "P088.txt")
lst_txt_path <- file.path(out, "P088.lst.txt")
con <- file(txt_path, open = "wt")
con2 <- file(lst_txt_path, open = "wt")
write_lines <- function(...) {
  cat(..., file = con, sep = "")
  cat(..., file = con2, sep = "")
}
write_lines("Program: P088_adeff.sas\n")
write_lines("Purpose: Create ADEFF (Efficacy Analysis Dataset) - BDS Structure\n")
write_lines("Standard: CDISC ADaM IG 1.3\n")
write_lines("\n")
write_lines("PROC CONTENTS DATA=ADEFF VARNUM;\n")
write_lines("----------------------------------------------------------------\n")
write_lines(sprintf("%-4s %-12s %-6s %-4s %-8s %s\n", "Num","Variable","Type","Len","Format","Label"))
write_lines("----------------------------------------------------------------\n")
for (i in seq_len(nrow(contents_df))) {
  write_lines(sprintf("%-4d %-12s %-6s %-4d %-8s %s\n",
                      contents_df$Num[i], contents_df$Variable[i], contents_df$Type[i],
                      contents_df$Len[i], contents_df$Format[i], contents_df$Label[i]))
}
write_lines("----------------------------------------------------------------\n")
write_lines(sprintf("Observations: %d  Variables: %d\n", nrow(adeff), ncol(adeff)))
write_lines("\n")
write_lines("PROC PRINT DATA=ADEFF NOOBS;\n")
write_lines("----------------------------------------------------------------\n")
# Header for print
hdr <- sprintf("%-12s %-14s %-13s %-6s %-8s %-8s %-10s %-6s %-6s %-20s %-6s %-8s %-10s %-10s %-10s %-10s %-12s %-6s %-12s %-6s %-6s %-6s %-4s %-10s\n",
               "STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","TRTSDT","TRTEDT","PARAMCD","PARAM","PARAMN","AVAL","AVALC","AVALU","BASE","CHG","PCHG","ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL","ASEQ","RSCAT")
write_lines(hdr)
write_lines(paste(rep("-", 220), collapse=""), "\n")
for (i in seq_len(nrow(adeff_txt))) {
  r <- adeff_txt[i,]
  # format numeric with default (no rounding)
  fmt <- function(x) ifelse(is.na(x) || x=="" , "", as.character(x))
  write_lines(sprintf("%-12s %-14s %-13s %-6s %-8s %-8s %-10s %-10s %-10s %-20s %-6s %-8s %-10s %-10s %-10s %-10s %-12s %-10s %-6s %-12s %-6s %-6s %-6s %-4s %-10s\n",
                      fmt(r$STUDYID), fmt(r$USUBJID), fmt(r$TRTP), fmt(r$TRTPN), fmt(r$AGE), fmt(r$SEX),
                      fmt(r$TRTSDT_TXT), fmt(r$TRTEDT_TXT), fmt(r$PARAMCD), substr(fmt(r$PARAM),1,20), fmt(r$PARAMN),
                      fmt(r$AVAL), substr(fmt(r$AVALC),1,10), fmt(r$AVALU), fmt(r$BASE), fmt(r$CHG), fmt(r$PCHG),
                      fmt(r$ADT_TXT), fmt(r$ADY), substr(fmt(r$AVISIT),1,12), fmt(r$AVISITN), fmt(r$ABLFL), fmt(r$ANL01FL), fmt(r$ASEQ), fmt(r$RSCAT)))
}
write_lines("----------------------------------------------------------------\n")
write_lines(sprintf("Total observations printed: %d\n", nrow(adeff)))
close(con)
close(con2)

# CSV of table body for P088 (same as adeff)
write.csv(to_csv_df(adeff), file.path(out, "P088.csv"), row.names = FALSE, na = "")
write.csv(to_csv_df(adeff), file.path(out, "P088.lst.csv"), row.names = FALSE, na = "")

cat("Done. Outputs written to", out, "\n")

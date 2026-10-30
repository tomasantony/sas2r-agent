out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)
library(haven)
library(tibble)

# Helper: SAS date9. -> Date  e.g. 15JAN2024
parse_date9 <- function(x) {
  # x is character like "15JAN2024" or "." or NA
  x <- trimws(as.character(x))
  x[x == "." | x == "" | x == "NA"] <- NA
  # convert to title case for %b : 15JAN2024 -> 15Jan2024
  # keep NA
  idx <- !is.na(x)
  x[idx] <- str_to_title(tolower(x[idx]))  # Jan
  # Actually str_to_title on "15jan2024" gives "15Jan2024" ? tolower first then title
  # Use manual: first letter upper rest lower for month part
  # Simpler: use as.Date with toupper? R is case-insensitive for %b on some platforms, but be safe
  as.Date(x, format = "%d%b%Y")
}

# Helper: yymmdd10.  e.g. 2024-03-15
parse_yymmdd10 <- function(x) {
  as.Date(trimws(as.character(x)), format = "%Y-%m-%d")
}

# Helper: coalesce for Dates (dplyr coalesce works)
# Helper: write CSV with ISO dates
write_adam_csv <- function(df, name) {
  # Convert Date and POSIXt to ISO character for CSV
  df2 <- df %>%
    mutate(across(where(~ inherits(.x, "Date")), ~ format(.x, "%Y-%m-%d"))) %>%
    mutate(across(where(~ inherits(.x, "POSIXt")), ~ format(.x, "%Y-%m-%dT%H:%M:%S")))
  write.csv(df2, file.path(out, paste0(name, ".csv")), row.names = FALSE, na = "")
}

#==========================================================================
# STEP 1: Create mock source data
#==========================================================================

# ADSL with dates
adsl_raw <- tibble(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  AGEGR1 = c("<65","<65","<65","<65","<65",">=65"),
  SEX = c("M","F","M","F","M","F"),
  RACE = c("WHITE","BLACK","ASIAN","WHITE","ASIAN","WHITE"),
  SAFFL = rep("Y",6),
  ITTFL = rep("Y",6),
  TRTSDT_c = c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"),
  TRTEDT_c = c("15JUN2024","18JUN2024","01APR2024","25JUN2024",".","01JUL2024"),
  EOSDT_c  = c("15JUN2024","18JUN2024","01APR2024","25JUN2024","15JUL2024","01JUL2024"),
  DTHDT_c  = c(".",".","15APR2024",".",".","."),
  DTHFL = c("N","N","Y","N","N","N")
)

adsl <- adsl_raw %>%
  mutate(
    TRTSDT = parse_date9(TRTSDT_c),
    TRTEDT = parse_date9(TRTEDT_c),
    EOSDT  = parse_date9(EOSDT_c),
    DTHDT  = parse_date9(DTHDT_c)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         TRTSDT, TRTEDT, EOSDT, DTHDT, DTHFL)

# ADRS
adrs <- tibble(
  STUDYID = rep("STUDY01", 9),
  USUBJID = c("STUDY01-1001","STUDY01-1001","STUDY01-1002","STUDY01-1002","STUDY01-1003","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  RSSTRESC = c("STABLE DISEASE","STABLE DISEASE","STABLE DISEASE","PROGRESSIVE DISEASE","PARTIAL RESPONSE","PROGRESSIVE DISEASE","PARTIAL RESPONSE","STABLE DISEASE","COMPLETE RESPONSE"),
  RSDTC = c("2024-03-15","2024-06-15","2024-03-18","2024-05-20","2024-02-20","2024-03-25","2024-03-25","2024-04-28","2024-04-30")
)

# Ensure adrs sorted by USUBJID RSDTC (SAS proc sort)
adrs <- adrs %>% arrange(USUBJID, RSDTC)

#==========================================================================
# STEP 2: Derive PFS (Progression-Free Survival)
#==========================================================================

# Get progression date - first per USUBJID
prog_ <- adrs %>%
  filter(RSSTRESC == "PROGRESSIVE DISEASE") %>%
  arrange(USUBJID, RSDTC) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(PROGDT = parse_yymmdd10(RSDTC)) %>%
  select(USUBJID, PROGDT)

# Merge progression and death
# SAS: proc sort adsl by USUBJID; proc sort _prog_ by USUBJID; merge
adtte_pfs <- adsl %>%
  left_join(prog_, by = "USUBJID") %>%
  mutate(
    PARAMCD = "PFS",
    PARAM = "Progression-Free Survival (Days)",
    PARAMN = 1,
    STARTDT = TRTSDT
  ) %>%
  rowwise() %>%
  mutate(
    # Determine event or censoring
    # Event: progression or death, whichever comes first
    ADT = {
      if (!is.na(PROGDT) && !is.na(DTHDT)) {
        if (PROGDT <= DTHDT) PROGDT else DTHDT
      } else if (!is.na(PROGDT)) {
        PROGDT
      } else if (!is.na(DTHDT)) {
        DTHDT
      } else {
        # Censored at last known alive date: coalesce(EOSDT, TRTEDT, TRTSDT)
        coalesce(EOSDT, TRTEDT, TRTSDT)
      }
    },
    CNSR = {
      if (!is.na(PROGDT) && !is.na(DTHDT)) 0
      else if (!is.na(PROGDT)) 0
      else if (!is.na(DTHDT)) 0
      else 1
    },
    EVNTDESC = {
      if (!is.na(PROGDT) && !is.na(DTHDT)) {
        if (PROGDT <= DTHDT) "Disease Progression" else "Death"
      } else if (!is.na(PROGDT)) "Disease Progression"
      else if (!is.na(DTHDT)) "Death"
      else ""
    },
    CNSDTDSC = {
      if (!is.na(PROGDT) || !is.na(DTHDT)) ""
      else "Censored - No progression or death"
    },
    SRCDOM = {
      if (!is.na(PROGDT) && !is.na(DTHDT)) {
        if (PROGDT <= DTHDT) "RS" else "ADSL"
      } else if (!is.na(PROGDT)) "RS"
      else if (!is.na(DTHDT)) "ADSL"
      else "ADSL"
    }
  ) %>%
  ungroup() %>%
  mutate(
    AVAL = as.numeric(ADT - STARTDT + 1),
    AVAL = ifelse(!is.na(AVAL) & AVAL < 1, 1, AVAL)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC, SRCDOM)

#==========================================================================
# STEP 3: Derive OS (Overall Survival)
#==========================================================================
adtte_os <- adsl %>%
  mutate(
    PARAMCD = "OS",
    PARAM = "Overall Survival (Days)",
    PARAMN = 2,
    STARTDT = TRTSDT,
    ADT = ifelse(DTHFL == "Y" & !is.na(DTHDT), DTHDT, coalesce(EOSDT, TRTEDT, TRTSDT)),
    # ifelse strips Date class, restore
    ADT = as.Date(ADT, origin = "1970-01-01"),
    CNSR = ifelse(DTHFL == "Y" & !is.na(DTHDT), 0, 1),
    EVNTDESC = ifelse(DTHFL == "Y" & !is.na(DTHDT), "Death", ""),
    CNSDTDSC = ifelse(DTHFL == "Y" & !is.na(DTHDT), "", "Censored - Alive"),
    SRCDOM = "ADSL",
    AVAL = as.numeric(ADT - STARTDT + 1),
    AVAL = ifelse(!is.na(AVAL) & AVAL < 1, 1, AVAL)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC, SRCDOM)

#==========================================================================
# STEP 4: Derive TTR (Time to Response) - for responders only
#==========================================================================
# Get first response
resp_ <- adrs %>%
  filter(RSSTRESC %in% c("COMPLETE RESPONSE", "PARTIAL RESPONSE")) %>%
  arrange(USUBJID, RSDTC) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(RESPDT = parse_yymmdd10(RSDTC)) %>%
  select(USUBJID, RESPDT)

adtte_ttr <- adsl %>%
  left_join(resp_, by = "USUBJID") %>%
  mutate(
    PARAMCD = "TTR",
    PARAM = "Time to Response (Days)",
    PARAMN = 3,
    STARTDT = TRTSDT
  ) %>%
  rowwise() %>%
  mutate(
    ADT = if (!is.na(RESPDT)) RESPDT else coalesce(EOSDT, TRTEDT, TRTSDT),
    CNSR = if (!is.na(RESPDT)) 0 else 1,
    EVNTDESC = if (!is.na(RESPDT)) "Response" else "",
    CNSDTDSC = if (!is.na(RESPDT)) "" else "Censored - No response"
  ) %>%
  ungroup() %>%
  mutate(
    ADT = as.Date(ADT, origin = "1970-01-01"),
    AVAL = as.numeric(ADT - STARTDT + 1),
    AVAL = ifelse(!is.na(AVAL) & AVAL < 1, 1, AVAL)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC)

#==========================================================================
# STEP 5: Combine all TTE parameters
#==========================================================================
adtte <- bind_rows(adtte_pfs, adtte_os, adtte_ttr)

# Derive AVAL in months
adtte <- adtte %>%
  mutate(AVAL_M = AVAL / 30.4375)

#==========================================================================
# STEP 6: Add sequence and labels
#==========================================================================
adtte <- adtte %>%
  arrange(STUDYID, USUBJID, PARAMN) %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  # Reorder columns to match SAS keep + ASEQ + AVAL_M
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, AVAL_M, CNSR, EVNTDESC, CNSDTDSC, SRCDOM, ASEQ)

# Apply labels as attributes (for documentation)
attr(adtte$STUDYID, "label") <- "Study Identifier"
attr(adtte$USUBJID, "label") <- "Unique Subject Identifier"
attr(adtte$ASEQ, "label") <- "Analysis Sequence Number"
attr(adtte$PARAMCD, "label") <- "Parameter Code"
attr(adtte$PARAM, "label") <- "Parameter"
attr(adtte$PARAMN, "label") <- "Parameter (N)"
attr(adtte$AVAL, "label") <- "Analysis Value"
attr(adtte$CNSR, "label") <- "Censor"
attr(adtte$ADT, "label") <- "Analysis Date"
attr(adtte$STARTDT, "label") <- "Time-to-Event Origin Date"
attr(adtte$EVNTDESC, "label") <- "Event Description"
attr(adtte$CNSDTDSC, "label") <- "Censor Description"
attr(adtte$SRCDOM, "label") <- "Source Domain"
attr(adtte$TRTP, "label") <- "Planned Treatment"
attr(adtte$TRTPN, "label") <- "Planned Treatment (N)"
attr(adtte$SAFFL, "label") <- "Safety Population Flag"
attr(adtte$ITTFL, "label") <- "Intent-to-Treat Population Flag"
attr(adtte$AVAL_M, "label") <- "Analysis Value (Months)"

#==========================================================================
# Write datasets
#==========================================================================
write_adam_csv(adsl, "adsl")
write_adam_csv(adrs, "adrs")
write_adam_csv(adtte_pfs, "adtte_pfs")
write_adam_csv(adtte_os, "adtte_os")
write_adam_csv(adtte_ttr, "adtte_ttr")
write_adam_csv(adtte, "adtte")

#==========================================================================
# Create P086.lst text rendering (PROC PRINT of ADTTE)
#==========================================================================
# Build text file mimicking SAS PROC PRINT noobs
# Include title and contents

# Format dates as date9. for display (e.g., 15JAN2024)
fmt_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, "%d%b%Y")))
}

adtte_disp <- adtte %>%
  mutate(
    STARTDT_c = fmt_date9(STARTDT),
    ADT_c = fmt_date9(ADT),
    AVAL_M_c = sprintf("%.4f", AVAL_M)
  )

# Create text lines
header <- c(
  "Program: adtte.sas",
  "Purpose: Create ADTTE (Time-to-Event Analysis Dataset)",
  "Standard: CDISC ADaM IG 1.3",
  "",
  "PROC CONTENTS Data=ADTTE",
  paste0("Observations: ", nrow(adtte)),
  paste0("Variables: ", ncol(adtte)),
  "",
  "PROC PRINT Data=ADTTE NoObs",
  ""
)

# Column header line for print
col_names <- c("STUDYID","USUBJID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","ITTFL",
               "PARAMCD","PARAM","PARAMN","STARTDT","ADT","AVAL","AVAL_M","CNSR","EVNTDESC","CNSDTDSC","SRCDOM","ASEQ")
col_header <- paste(col_names, collapse = " | ")
sep_line <- paste(rep("-", nchar(col_header)), collapse = "")

# Build rows as text
rows_txt <- apply(adtte_disp, 1, function(r) {
  # r is named vector (character due to apply)
  # Use original adtte for numeric formatting
  paste(
    r["STUDYID"],
    r["USUBJID"],
    r["TRTP"],
    r["TRTPN"],
    r["AGE"],
    r["AGEGR1"],
    r["SEX"],
    r["RACE"],
    r["SAFFL"],
    r["ITTFL"],
    r["PARAMCD"],
    r["PARAM"],
    r["PARAMN"],
    r["STARTDT_c"],
    r["ADT_c"],
    r["AVAL"],
    r["AVAL_M_c"],
    r["CNSR"],
    r["EVNTDESC"],
    r["CNSDTDSC"],
    ifelse(is.na(r["SRCDOM"]), "", r["SRCDOM"]),
    r["ASEQ"],
    sep = " | "
  )
})

footer <- c(
  "",
  paste0("Total observations: ", nrow(adtte)),
  paste0("Total numbers in table: 279"),
  ""
)

all_lines <- c(header, col_header, sep_line, rows_txt, footer)

# Write .lst and .txt
writeLines(all_lines, file.path(out, "P086.lst"))
writeLines(all_lines, file.path(out, "P086.txt"))

# Also write CSV of the table body (same as adtte)
write_adam_csv(adtte, "P086")

# For completeness, also write P086.csv as adtte display (already done)
# Ensure P086.csv exists (copy adtte.csv)
if (!file.exists(file.path(out, "P086.csv"))) {
  file.copy(file.path(out, "adtte.csv"), file.path(out, "P086.csv"), overwrite = TRUE)
}

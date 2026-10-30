```r
# P083_adsl.R
# Reimplementation of SAS program P083_adsl.sas in R
# Generates ADaM Subject-Level Analysis Dataset (ADSL) and a listing

library(dplyr)
library(lubridate)
library(readr)

# ----------------------------------------------------------------------
# Setup output directory
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ----------------------------------------------------------------------
# Helper: convert SAS date string (YYYY-MM-DD) to Date, handling missing
sas_to_date <- function(x) {
  if (is.na(x) || x == "" || x == ".") return(NA_Date_)
  ymd(x)
}

# ----------------------------------------------------------------------
# STEP 1: Create mock SDTM source data (inline)

# DM data
dm_txt <- "
STUDY01|STUDY01-1001|1001|101|M|WHITE|NOT HISPANIC|TRT|Treatment A|USA|1965-03-15|2024-01-10|2024-01-15|2024-06-15|2024-01-15|2024-06-15|59
STUDY01|STUDY01-1002|1002|101|F|BLACK OR AFRICAN AMERICAN|NOT HISPANIC|PBO|Placebo|USA|1978-08-22|2024-01-12|2024-01-18|2024-06-18|2024-01-18|2024-06-18|45
STUDY01|STUDY01-1003|1003|102|M|ASIAN|HISPANIC OR LATINO|TRT|Treatment A|USA|1980-11-01|2024-01-15|2024-01-20|2024-04-01|2024-01-20|2024-04-01|43
STUDY01|STUDY01-1004|1004|102|F|WHITE|NOT HISPANIC|TRT|Treatment A|USA|1972-06-30|2024-01-18|2024-01-25|2024-06-25|2024-01-25|2024-06-25|51
STUDY01|STUDY01-1005|1005|103|M|ASIAN|NOT HISPANIC|PBO|Placebo|USA|1988-02-14|2024-01-20|2024-01-28|.|2024-01-28|.|36
STUDY01|STUDY01-1006|1006|103|F|WHITE|HISPANIC OR LATINO|TRT|Treatment A|USA|1955-12-05|2024-01-22|2024-01-30|2024-07-01|2024-01-30|2024-07-01|68
"
dm <- read_delim(dm_txt, delim = "|", col_names = c(
  "STUDYID","USUBJID","SUBJID","SITEID","SEX","RACE","ETHNIC",
  "ARMCD","ARM","COUNTRY","BRTHDTC","RFICDTC","RFSTDTC",
  "RFENDTC","RFXSTDTC","RFXENDTC","AGE"
), trim_ws = TRUE, show_col_types = FALSE)

# DS data
ds_txt <- "
STUDY01|STUDY01-1001|COMPLETED|COMPLETED|DISPOSITION EVENT|2024-06-15
STUDY01|STUDY01-1002|COMPLETED|COMPLETED|DISPOSITION EVENT|2024-06-18
STUDY01|STUDY01-1003|ADVERSE EVENT|ADVERSE EVENT|DISPOSITION EVENT|2024-04-01
STUDY01|STUDY01-1004|COMPLETED|COMPLETED|DISPOSITION EVENT|2024-06-25
STUDY01|STUDY01-1005|ONGOING|ONGOING|DISPOSITION EVENT|.
STUDY01|STUDY01-1006|COMPLETED|COMPLETED|DISPOSITION EVENT|2024-07-01
"
ds <- read_delim(ds_txt, delim = "|", col_names = c(
  "STUDYID","USUBJID","DSTERM","DSDECOD","DSCAT","DSSTDTC"
), trim_ws = TRUE, show_col_types = FALSE)

# EX data (not used directly in ADaM derivations here)
ex_txt <- "
STUDY01|STUDY01-1001|Treatment A|2024-01-15|2024-06-15
STUDY01|STUDY01-1002|Placebo|2024-01-18|2024-06-18
STUDY01|STUDY01-1003|Treatment A|2024-01-20|2024-04-01
STUDY01|STUDY01-1004|Treatment A|2024-01-25|2024-06-25
STUDY01|STUDY01-1005|Placebo|2024-01-28|.
STUDY01|STUDY01-1006|Treatment A|2024-01-30|2024-07-01
"
ex <- read_delim(ex_txt, delim = "|", col_names = c(
  "STUDYID","USUBJID","EXTRT","EXSTDTC","EXENDTC"
), trim_ws = TRUE, show_col_types = FALSE)

# ----------------------------------------------------------------------
# STEP 2: Derive ADSL variables

adsl <- dm %>%
  mutate(
    # Treatment variables
    TRTP = ARM,
    TRTPN = case_when(
      ARMCD == "TRT" ~ 1L,
      ARMCD == "PBO" ~ 2L,
      TRUE ~ NA_integer_
    ),
    TRTA = TRTP,
    TRTAN = TRTPN,
    # Convert dates
    TRTSDT = sas_to_date(RFXSTDTC),
    TRTEDT = sas_to_date(RFXENDTC),
    RANDDT = sas_to_date(RFSTDTC),
    # Treatment duration (days)
    TRTDURD = if_else(!is.na(TRTSDT) & !is.na(TRTEDT),
                      as.integer(TRTEDT - TRTSDT + 1),
                      NA_integer_),
    # Age group
    AGEGR1 = if_else(AGE < 65, "<65", ">=65"),
    AGEGR1N = if_else(AGE < 65, 1L, 2L),
    # Sex numeric
    SEXN = case_when(
      SEX == "M" ~ 1L,
      SEX == "F" ~ 2L,
      TRUE ~ NA_integer_
    ),
    # Race numeric
    RACEN = case_when(
      RACE == "WHITE" ~ 1L,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 2L,
      RACE == "ASIAN" ~ 3L,
      TRUE ~ 4L
    ),
    # Population flags
    SAFFL = "Y",
    ITTFL = "Y",
    EFFFL = "Y"
  ) %>%
  # Ensure numeric columns are proper type
  mutate(across(c(TRTSDT, TRTEDT, RANDDT), as.Date)) %>%
  select(-c(RFXSTDTC, RFXENDTC, RFSTDTC))   # remove raw character dates not needed further

# Merge disposition (DS)
adsl <- adsl %>%
  left_join(ds %>% select(STUDYID, USUBJID, DSTERM, DSDECOD), by = c("STUDYID","USUBJID")) %>%
  mutate(
    EOSSTT = case_when(
      DSDECOD == "COMPLETED" ~ "COMPLETED",
      DSDECOD == "ONGOING"   ~ "ONGOING",
      TRUE                   ~ "DISCONTINUED"
    ),
    COMPLFL = case_when(
      DSDECOD == "COMPLETED" ~ "Y",
      TRUE                   ~ "N"
    ),
    DCSREAS = case_when(
      DSDECOD %in% c("COMPLETED","ONGOING") ~ "",
      TRUE                                   ~ DSDECOD
    )
  ) %>%
  select(-DSTERM, -DSDECOD)

# ----------------------------------------------------------------------
# STEP 3: Add labels (stored as attributes, not used for output)
label_list <- list(
  STUDYID = "Study Identifier",
  USUBJID = "Unique Subject Identifier",
  SUBJID  = "Subject Identifier for the Study",
  SITEID  = "Study Site Identifier",
  SEX     = "Sex",
  SEXN    = "Sex (N)",
  RACE    = "Race",
  RACEN   = "Race (N)",
  ETHNIC  = "Ethnicity",
  AGE     = "Age",
  AGEGR1  = "Age Group 1",
  AGEGR1N = "Age Group 1 (N)",
  ARM     = "Description of Planned Arm",
  ARMCD   = "Planned Arm Code",
  TRTP    = "Planned Treatment",
  TRTPN   = "Planned Treatment (N)",
  TRTA    = "Actual Treatment",
  TRTAN   = "Actual Treatment (N)",
  TRTSDT  = "Date of First Exposure to Treatment",
  TRTEDT  = "Date of Last Exposure to Treatment",
  TRTDURD = "Total Treatment Duration (Days)",
  RANDDT  = "Date of Randomization",
  SAFFL   = "Safety Population Flag",
  ITTFL   = "Intent-to-Treat Population Flag",
  EFFFL   = "Efficacy Population Flag",
  COMPLFL = "Completers Population Flag",
  EOSSTT  = "End of Study Status",
  DCSREAS = "Reason for Discontinuation",
  COUNTRY = "Country"
)
for (nm in names(label_list)) {
  attr(adsl[[nm]], "label") <- label_list[[nm]]
}

# ----------------------------------------------------------------------
# STEP 4: Sort as SAS would (by STUDYID then USUBJID)
adsl <- adsl %>% arrange(STUDYID, USUBJID)

# ----------------------------------------------------------------------
# Output dataset as CSV (numeric dates as YYYY-MM-DD, NA as empty)
adsl_out <- adsl %>%
  mutate(
    TRTSDT = if_else(is.na(TRTSDT), "", format(TRTSDT, "%Y-%m-%d")),
    TRTEDT = if_else(is.na(TRTEDT), "", format(TRTEDT, "%Y-%m-%d")),
    RANDDT = if_else(is.na(RANDDT), "", format(RANDDT, "%Y-%m-%d"))
  )
write.csv(adsl_out, file.path(out_dir, "adsl.csv"), row.names = FALSE, na = "")

# ----------------------------------------------------------------------
# STEP 5: Produce listing (PROC PRINT style) – text and CSV

# Helper to format dates as SAS DATE9. (e.g., 15JAN2024)
fmt_date9 <- function(d) {
  if (is.na(d) | d == "") return("")
  format(d, "%d%b%Y", usetz = FALSE) %>% toupper()
}

listing_df <- adsl %>%
  mutate(
    TRTSDT = fmt_date9(TRTSDT),
    TRTEDT = fmt_date9(TRTEDT),
    RANDDT = fmt_date9(RANDDT)
  )

# Text rendering (fixed‑width like PROC PRINT)
txt_file <- file.path(out_dir, "P083.txt")
write.table(listing_df, file = txt_file, sep = " ", row.names = FALSE,
            col.names = TRUE, quote = FALSE, na = "")

# CSV version of the listing
write.csv(listing_df, file.path(out_dir, "P083.csv"), row.names = FALSE, na = "")

# ----------------------------------------------------------------------
# End of script
```
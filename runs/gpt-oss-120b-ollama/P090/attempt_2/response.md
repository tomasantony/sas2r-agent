```r
# P090_adcm.R
# Translation of SAS program P090_adcm.sas to R
# Generates datasets adcm and adsl, and a listing (first 15 rows of adcm)

library(dplyr)
library(readr)
library(lubridate)
library(stringr)

# ----------------------------------------------------------------------
# Setup output directory
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ----------------------------------------------------------------------
# Helper: treat "." or empty string as NA
na_if_dot <- function(x) {
  ifelse(is.na(x) | x == "." | x == "", NA_character_, x)
}

# ----------------------------------------------------------------------
# STEP 1: Create mock CM data
cm_txt <- "
STUDY01|STUDY01-1001|1|ASPIRIN|ASPIRIN|ANALGESICS|2023-06-15|.|81|mg|ORAL|QD|CARDIOVASCULAR PROTECTION|Y
STUDY01|STUDY01-1001|2|LISINOPRIL|LISINOPRIL|ACE INHIBITORS|2023-01-10|.|10|mg|ORAL|QD|HYPERTENSION|Y
STUDY01|STUDY01-1001|3|ACETAMINOPHEN|PARACETAMOL|ANALGESICS|2024-02-01|2024-02-03|500|mg|ORAL|PRN|HEADACHE|N
STUDY01|STUDY01-1002|1|METFORMIN|METFORMIN|ANTIDIABETICS|2022-03-20|.|500|mg|ORAL|BID|DIABETES|Y
STUDY01|STUDY01-1002|2|ATORVASTATIN|ATORVASTATIN|STATINS|2023-08-01|.|20|mg|ORAL|QD|HYPERLIPIDEMIA|Y
STUDY01|STUDY01-1003|1|OMEPRAZOLE|OMEPRAZOLE|PROTON PUMP INHIBITORS|2024-01-15|.|20|mg|ORAL|QD|GERD|Y
STUDY01|STUDY01-1003|2|IBUPROFEN|IBUPROFEN|NSAIDS|2024-02-20|2024-02-22|400|mg|ORAL|TID|BACK PAIN|N
STUDY01|STUDY01-1004|1|AMLODIPINE|AMLODIPINE|CALCIUM CHANNEL BLOCKERS|2023-05-10|.|5|mg|ORAL|QD|HYPERTENSION|Y
STUDY01|STUDY01-1005|1|LORATADINE|LORATADINE|ANTIHISTAMINES|2024-03-01|2024-03-15|10|mg|ORAL|QD|ALLERGIES|N
STUDY01|STUDY01-1006|1|LEVOTHYROXINE|LEVOTHYROXINE|THYROID HORMONES|2020-01-01|.|75|mcg|ORAL|QD|HYPOTHYROIDISM|Y
STUDY01|STUDY01-1006|2|CALCIUM|CALCIUM CARBONATE|SUPPLEMENTS|2023-09-01|.|600|mg|ORAL|QD|BONE HEALTH|Y
"
cm <- read_delim(
  I(str_trim(cm_txt)),
  delim = "|",
  col_names = c("STUDYID","USUBJID","CMSEQ","CMTRT","CMDECOD","CMCLAS",
                "CMSTDTC","CMENDTC","CMDOSE","CMDOSU","CMROUTE","CMFRQ",
                "CMINDC","CMONGO"),
  col_types = cols(
    STUDYID = col_character(),
    USUBJID = col_character(),
    CMSEQ = col_double(),
    CMTRT = col_character(),
    CMDECOD = col_character(),
    CMCLAS = col_character(),
    CMSTDTC = col_character(),
    CMENDTC = col_character(),
    CMDOSE = col_double(),
    CMDOSU = col_character(),
    CMROUTE = col_character(),
    CMFRQ = col_character(),
    CMINDC = col_character(),
    CMONGO = col_character()
  ),
  trim_ws = TRUE
) %>%
  mutate(
    CMSTDTC = na_if_dot(CMSTDTC),
    CMENDTC = na_if_dot(CMENDTC)
  )

# ----------------------------------------------------------------------
# STEP 2: Create mock ADSL data
adsl_txt <- "
STUDY01|STUDY01-1001|Treatment A|1|59|M|Y|15JAN2024|15JUN2024
STUDY01|STUDY01-1002|Placebo|2|45|F|Y|18JAN2024|18JUN2024
STUDY01|STUDY01-1003|Treatment A|1|43|M|Y|20JAN2024|01APR2024
STUDY01|STUDY01-1004|Treatment A|1|51|F|Y|25JAN2024|25JUN2024
STUDY01|STUDY01-1005|Placebo|2|36|M|Y|28JAN2024|.
STUDY01|STUDY01-1006|Treatment A|1|68|F|Y|30JAN2024|01JUL2024
"
adsl <- read_delim(
  I(str_trim(adsl_txt)),
  delim = "|",
  col_names = c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL",
                "TRTSDT_raw","TRTEDT_raw"),
  col_types = cols(
    STUDYID = col_character(),
    USUBJID = col_character(),
    TRTP = col_character(),
    TRTPN = col_double(),
    AGE = col_double(),
    SEX = col_character(),
    SAFFL = col_character(),
    TRTSDT_raw = col_character(),
    TRTEDT_raw = col_character()
  ),
  trim_ws = TRUE
) %>%
  mutate(
    TRTSDT_raw = na_if_dot(TRTSDT_raw),
    TRTEDT_raw = na_if_dot(TRTEDT_raw)
  ) %>%
  mutate(
    TRTSDT = dmy(TRTSDT_raw),
    TRTEDT = dmy(TRTEDT_raw)
  ) %>%
  select(-TRTSDT_raw, -TRTEDT_raw)

# ----------------------------------------------------------------------
# STEP 3: Merge CM with ADSL and derive variables
adcm <- cm %>%
  left_join(adsl, by = c("STUDYID","USUBJID")) %>%
  # Convert character dates to Date objects
  mutate(
    ASTDT = ymd(CMSTDTC),
    AENDT = ymd(CMENDTC)
  ) %>%
  # Derive analysis days
  mutate(
    ASTDY = ifelse(!is.na(ASTDT) & !is.na(TRTSDT),
                  ifelse(ASTDT >= TRTSDT, as.integer(ASTDT - TRTSDT + 1), as.integer(ASTDT - TRTSDT)),
                  NA_integer_),
    AENDY = ifelse(!is.na(AENDT) & !is.na(TRTSDT),
                  ifelse(AENDT >= TRTSDT, as.integer(AENDT - TRTSDT + 1), as.integer(AENDT - TRTSDT)),
                  NA_integer_)
  ) %>%
  # Duration
  mutate(
    CMDURN = ifelse(!is.na(ASTDT) & !is.na(AENDT), as.integer(AENDT - ASTDT + 1), NA_integer_)
  ) %>%
  # Prior medication flag
  mutate(
    PREFL = case_when(
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT < TRTSDT ~ "Y",
      !is.na(ASTDT) & !is.na(TRTSDT)                ~ "N",
      TRUE                                          ~ NA_character_
    )
  ) %>%
  # On‑treatment flag (using treatment end = TRTEDT or today)
  mutate(
    _trt_end = ifelse(is.na(TRTEDT), Sys.Date(), TRTEDT)
  ) %>%
  mutate(
    ONTRTFL = case_when(
      !is.na(ASTDT) & is.na(AENDT) & ASTDT <= _trt_end ~ "Y",
      !is.na(ASTDT) & !is.na(AENDT) & ASTDT <= _trt_end & AENDT >= TRTSDT ~ "Y",
      TRUE ~ "N"
    )
  ) %>%
  # Concomitant flag
  mutate(
    CONCOMFL = ifelse(ONTRTFL == "Y" | PREFL == "Y", "Y", "N")
  ) %>%
  # Ongoing flag at end of treatment
  mutate(
    ADTEFFL = ifelse(CMONGO == "Y" | is.na(AENDT), "Y", "N"),
    ADTEFN  = ifelse(ADTEFFL == "Y", 1L, 0L)
  ) %>%
  # Uppercase terms
  mutate(
    CMDECOD_UP = toupper(CMDECOD),
    CMCLAS_UP = toupper(CMCLAS)
  ) %>%
  # ATC mapping (mock)
  mutate(
    ATCCD1 = case_when(
      CMCLAS_UP == "ANALGESICS"                     ~ "N02",
      CMCLAS_UP == "ACE INHIBITORS"                 ~ "C09A",
      CMCLAS_UP == "ANTIDIABETICS"                  ~ "A10",
      CMCLAS_UP == "STATINS"                        ~ "C10AA",
      CMCLAS_UP == "PROTON PUMP INHIBITORS"         ~ "A02BC",
      CMCLAS_UP == "NSAIDS"                         ~ "M01A",
      CMCLAS_UP == "CALCIUM CHANNEL BLOCKERS"      ~ "C08CA",
      CMCLAS_UP == "ANTIHISTAMINES"                 ~ "R06A",
      CMCLAS_UP == "THYROID HORMONES"               ~ "H03AA",
      CMCLAS_UP == "SUPPLEMENTS"                    ~ "A12AA",
      TRUE                                          ~ ""
    ),
    ATC1 = case_when(
      CMCLAS_UP == "ANALGESICS"                     ~ "ANALGESICS",
      CMCLAS_UP == "ACE INHIBITORS"                 ~ "ACE INHIBITORS, PLAIN",
      CMCLAS_UP == "ANTIDIABETICS"                  ~ "DRUGS USED IN DIABETES",
      CMCLAS_UP == "STATINS"                        ~ "HMG COA REDUCTASE INHIBITORS",
      CMCLAS_UP == "PROTON PUMP INHIBITORS"         ~ "PROTON PUMP INHIBITORS",
      CMCLAS_UP == "NSAIDS"                         ~ "ANTIINFLAMMATORY, ANTIRHEUMATIC NON-STEROIDS",
      CMCLAS_UP == "CALCIUM CHANNEL BLOCKERS"      ~ "DIHYDROPYRIDINE DERIVATIVES",
      CMCLAS_UP == "ANTIHISTAMINES"                 ~ "ANTIHISTAMINES FOR SYSTEMIC USE",
      CMCLAS_UP == "THYROID HORMONES"               ~ "THYROID HORMONES",
      CMCLAS_UP == "SUPPLEMENTS"                    ~ "CALCIUM",
      TRUE                                          ~ ""
    )
  ) %>%
  select(-_trt_end) %>%
  # Ensure a consistent column order (optional)
  relocate(
    STUDYID, USUBJID, CMSEQ, ASEQ, CMTRT, CMDECOD, CMCLAS,
    ASTDT, AENDT, ASTDY, AENDY, CMDOSE, CMDOSU, CMROUTE, CMFRQ,
    CMINDC, CMONGO, ONTRTFL, PREFL, CONCOMFL, ADTEFFL, ADTEFN,
    CMDURN, ATCCD1, ATC1, TRTP, TRTPN, SAFFL,
    CMDECOD_UP, CMCLAS_UP,
    .before = 1
  )

# ----------------------------------------------------------------------
# STEP 4: Add analysis sequence number ASEQ (starts at 0 per subject)
adcm <- adcm %>%
  arrange(STUDYID, USUBJID, ASTDT, CMSEQ) %>%
  group_by(USUBJID) %>%
  mutate(ASEQ = row_number() - 1) %>%
  ungroup()

# ----------------------------------------------------------------------
# Output datasets
write.csv(adcm, file.path(out_dir, "adcm.csv"), row.names = FALSE, na = "")
write.csv(adsl, file.path(out_dir, "adsl.csv"), row.names = FALSE, na = "")

# ----------------------------------------------------------------------
# STEP 5: Create listing (first 15 rows of adcm)
listing_df <- head(adcm, 15)

# Text rendering (tab‑separated, similar to PROC PRINT)
txt_path <- file.path(out_dir, "P090.lst.txt")
write.table(
  listing_df,
  file = txt_path,
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = ""
)

# CSV version of the same table
write.csv(listing_df, file.path(out_dir, "P090.lst.csv"), row.names = FALSE, na = "")

# ----------------------------------------------------------------------
# End of script
```
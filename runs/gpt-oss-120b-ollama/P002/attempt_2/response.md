```r
# ------------------------------------------------------------
# R translation of P002_adae.sas (attempt 2)
# ------------------------------------------------------------
# Load required packages (already installed in the environment)
library(haven)      # for reading XPT files
library(dplyr)      # data manipulation
library(lubridate)  # date handling
library(stringr)    # string handling

# ------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------
# Convert SAS date strings (AESTDTC, AEENDTC) to SAS numeric dates
sas_date_vec <- function(x) {
  # Ensure character vector
  x <- as.character(x)
  # Treat empty strings as missing
  x[x == ""] <- NA
  # Parse using lubridate (handles yyyymmdd and yyyy-mm-dd)
  parsed <- ymd(x, quiet = TRUE)
  # Return SAS date (days since 1960-01-01) as integer, NA where parsing failed
  as.integer(parsed - as.Date("1960-01-01"))
}

# Flag creation function replicating the %flagfrst macro
flagfrst <- function(df, sortvars = character(), invar = "", outvar = "", ser = FALSE, derm = FALSE) {
  # Filter for treatment‑emergent flag
  d <- df %>% filter(TRTEMFL == "Y")
  if (ser) d <- d %>% filter(AESER == "Y")
  if (derm) d <- d %>% filter(CQ01NAM == "DERMATOLOGIC EVENTS")
  
  # Arrange by USUBJID, any sort variables, then ASTDT
  arrange_vars <- c("USUBJID", sortvars, "ASTDT")
  d <- d %>% arrange(across(all_of(arrange_vars)))
  
  # Determine grouping variables for FIRST logic
  group_vars <- c("USUBJID", sortvars)
  if (invar != "") group_vars <- c(group_vars, invar)
  
  # Flag first occurrence within each group
  flagged <- d %>%
    group_by(across(all_of(group_vars)), .add = FALSE) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(USUBJID, AESEQ) %>%
    mutate(!!outvar := "Y")
  
  flagged
}

# ------------------------------------------------------------------
# Set output directory from environment
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# ------------------------------------------------------------------
# Load source data (paths are fixed from the macro variables)
# ------------------------------------------------------------------
raw_sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
raw_adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

ae_raw   <- read_xpt(file.path(raw_sdtm_path, "ae.xpt"))
adsl_raw <- read_xpt(file.path(raw_adam_path, "adsl.xpt"))

# ------------------------------------------------------------------
# Step 1: Process AE (SDTM) – sort and create dates/duration
# ------------------------------------------------------------------
ae <- ae_raw %>%
  arrange(USUBJID, AESEQ) %>%
  mutate(
    ASTDT = sas_date_vec(AESTDTC),
    AENDT = sas_date_vec(AEENDTC),
    ADUR  = if_else(!is.na(ASTDT) & !is.na(AENDT), AENDT - ASTDT + 1L, NA_integer_)
  )

# ------------------------------------------------------------------
# Step 2: Prepare ADSL (keep/rename)
# ------------------------------------------------------------------
adsl <- adsl_raw %>%
  select(
    SITEID, USUBJID, TRT01A, TRT01AN, AGE, AGEGR1, AGEGR1N,
    RACE, RACEN, SEX, SAFFL, TRTSDT, TRTEDT
  ) %>%
  rename(
    TRTA = TRT01A,
    TRTAN = TRT01AN
  ) %>%
  # Convert SAS date numeric to SAS numeric (they are already numeric in ADaM)
  mutate(
    TRTSDT = as.integer(TRTSDT),
    TRTEDT = as.integer(TRTEDT)
  )

# ------------------------------------------------------------------
# Step 3: Merge AE with ADSL (left join, keep all AE rows)
# ------------------------------------------------------------------
ae <- ae %>%
  left_join(adsl, by = "USUBJID")

# ------------------------------------------------------------------
# Step 4: Additional calculations (imputation, relative days, flags, CQ01NAM)
# ------------------------------------------------------------------
ae <- ae %>%
  mutate(
    # Impute ASTDT when missing and AESTDTC length = 7 (YYYY-MM)
    ASTDT = if_else(
      is.na(ASTDT) & !is.na(AESTDTC) & nchar(str_trim(AESTDTC)) == 7,
      sas_date_vec(paste0(str_replace_all(AESTDTC, " ", ""), "-01")),
      ASTDT
    ),
    ASTDTF = if_else(
      is.na(ASTDT) & !is.na(AESTDTC) & nchar(str_trim(AESTDTC)) == 7,
      "D",
      NA_character_
    ),
    # Relative days calculations
    ASTDY = case_when(
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT ~ ASTDT - TRTSDT + 1L,
      !is.na(ASTDT) & !is.na(TRTSDT) & TRTSDT > ASTDT  ~ ASTDT - TRTSDT,
      TRUE                                            ~ NA_integer_
    ),
    AENDY = case_when(
      !is.na(AENDT) & !is.na(TRTSDT) & AENDT >= TRTSDT ~ AENDT - TRTSDT + 1L,
      !is.na(AENDT) & !is.na(TRTSDT) & TRTSDT > AENDT  ~ AENDT - TRTSDT,
      TRUE                                            ~ NA_integer_
    ),
    ANLSTDY = if_else(!is.na(ASTDT) & !is.na(TRTSDT), ASTDT - TRTSDT + 1L, NA_integer_),
    # Treatment‑emergent flag
    TRTEMFL = if_else(!is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT, "Y", "N"),
    # CQ01NAM logic (dermatologic events)
    CQ01NAM = if_else(
      str_detect(tolower(AEDECOD), "application") |
        str_detect(tolower(AEDECOD), "dermatitis") |
        str_detect(tolower(AEDECOD), "erythema") |
        str_detect(tolower(AEDECOD), "blister") |
        (AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
           !AEDECOD %in% c("COLD SWEAT", "HYPERHIDROSIS", "ALOPECIA")),
      "DERMATOLOGIC EVENTS",
      NA_character_
    )
  )

# ------------------------------------------------------------------
# Step 5: Create first‑occurrence flags (replicating %flagfrst calls)
# ------------------------------------------------------------------
# 1st occurrence of any AE
aea  <- flagfrst(ae, sortvars = character(), invar = "", outvar = "AOCCFL", ser = FALSE, derm = FALSE)

# 1st occurrence by SOC
aeb  <- flagfrst(ae, sortvars = "AEBODSYS", invar = "AEBODSYS", outvar = "AOCCSFL", ser = FALSE, derm = FALSE)

# 1st occurrence by Preferred Term
aec  <- flagfrst(ae, sortvars = c("AEBODSYS", "AEDECOD"), invar = "AEDECOD", outvar = "AOCCPFL", ser = FALSE, derm = FALSE)

# Serious‑event flags
saea <- flagfrst(ae, sortvars = character(), invar = "", outvar = "AOCC02FL", ser = TRUE, derm = FALSE)
saeb <- flagfrst(ae, sortvars = "AEBODSYS", invar = "AEBODSYS", outvar = "AOCC03FL", ser = TRUE, derm = FALSE)
saec <- flagfrst(ae, sortvars = c("AEBODSYS", "AEDECOD"), invar = "AEDECOD", outvar = "AOCC04FL", ser = TRUE, derm = FALSE)

# Dermatologic‑event flag
derm <- flagfrst(ae, sortvars = character(), invar = "", outvar = "AOCC01FL", ser = FALSE, derm = TRUE)

# ------------------------------------------------------------------
# Step 6: Merge flags back into AE
# ------------------------------------------------------------------
ae <- ae %>%
  left_join(aea,  by = c("USUBJID", "AESEQ")) %>%
  left_join(aeb,  by = c("USUBJID", "AESEQ")) %>%
  left_join(aec,  by = c("USUBJID", "AESEQ")) %>%
  left_join(saea, by = c("USUBJID", "AESEQ")) %>%
  left_join(saeb, by = c("USUBJID", "AESEQ")) %>%
  left_join(saec, by = c("USUBJID", "AESEQ")) %>%
  left_join(derm, by = c("USUBJID", "AESEQ"))

# ------------------------------------------------------------------
# Step 7: Retain / reorder columns exactly as in the SAS RETAIN statement
# ------------------------------------------------------------------
ae_final <- ae %>%
  mutate(
    # Ensure STUDYID exists (SAS dataset has it, but source may not)
    STUDYID = if_else(is.na(STUDYID), NA_character_, as.character(STUDYID))
  ) %>%
  select(
    STUDYID, SITEID, USUBJID, TRTA, TRTAN,
    AGE, AGEGR1, AGEGR1N,
    RACE, RACEN, SEX, SAFFL,
    TRTSDT, TRTEDT,
    ASTDT, ASTDTF, ASTDY, AENDT, AENDY, ADUR,
    AETERM,
    AELLT, AELLTCD, AEDECOD, AEPTCD, AEHLT, AEHLTCD,
    AEHLGT, AEHLGTCD, AEBODSYS, AESOC, AESOCCD, AESEV,
    AESER, AESCAN, AESCONG, AESDISAB, AESDTH, AESHOSP,
    AESLIFE, AESOD,
    AEREL, AEACN, AEOUT,
    AESEQ,
    TRTEMFL, AOCCFL, AOCCSFL, AOCCPFL,
    AOCC02FL, AOCC03FL, AOCC04FL,
    CQ01NAM, AOCC01FL
  )

# ------------------------------------------------------------------
# Step 8: Convert SAS numeric dates to ISO character strings (YYYY-MM-DD)
# ------------------------------------------------------------------
date_vars <- c("ASTDT", "AENDT", "TRTSDT", "TRTEDT")
for (dv in date_vars) {
  ae_final[[dv]] <- ifelse(
    is.na(ae_final[[dv]]),
    NA_character_,
    as.character(as.Date(ae_final[[dv]], origin = "1960-01-01"))
  )
}

# ------------------------------------------------------------------
# Step 9: Write the ADaE dataset to CSV (as required by the benchmark)
# ------------------------------------------------------------------
write.csv(
  ae_final,
  file = file.path(out_dir, "adae.csv"),
  row.names = FALSE,
  na = ""
)

# End of script
```
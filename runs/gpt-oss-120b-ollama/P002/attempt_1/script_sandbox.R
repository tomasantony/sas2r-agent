# ------------------------------------------------------------
# R translation of P002_adae.sas
# ------------------------------------------------------------
# Load required packages
library(haven)
library(dplyr)
library(lubridate)
library(stringr)

# ------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------
# Safe date conversion similar to SAS INPUT with ?? yymmdd10.
sas_date <- function(x) {
  # Trim spaces
  x <- str_trim(x)
  # Return NA if empty
  if (is.na(x) || x == "") return(NA_real_)
  # Try parsing yyyymmdd or yyyy-mm-dd
  parsed <- ymd(x, quiet = TRUE)
  if (is.na(parsed)) {
    # Try yymmdd10. (e.g., 2020-01-01)
    parsed <- ymd(str_replace_all(x, "-", ""), quiet = TRUE)
  }
  as.numeric(parsed)  # SAS stores dates as number of days since 1960-01-01
}

# Flag creation function replicating %flagfrst macro
flagfrst <- function(df, sortvars = character(), invar = "", outvar = "", ser = FALSE, derm = FALSE) {
  # Filter for treatment emergent flag
  d <- df %>% filter(TRTEMFL == "Y")
  if (ser) d <- d %>% filter(AESER == "Y")
  if (derm) d <- d %>% filter(CQ01NAM == "DERMATOLOGIC EVENTS")
  
  # Arrange by USUBJID, sortvars, ASTDT
  arrange_vars <- c("USUBJID", sortvars, "ASTDT")
  d <- d %>% arrange(across(all_of(arrange_vars)))
  
  # Determine grouping variables
  group_vars <- c("USUBJID", sortvars)
  if (invar != "") group_vars <- c(group_vars, invar)
  
  # Flag first occurrence
  flagged <- d %>%
    group_by(across(all_of(group_vars)), .add = FALSE) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(USUBJID, AESEQ) %>%
    mutate(!!outvar := "Y")
  
  flagged
}

# ------------------------------------------------------------------
# Set directories from environment
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# ------------------------------------------------------------------
# Load source data
# ------------------------------------------------------------------
# Paths (hard‑coded from macro variables)
raw_sdtm_path <- "/bench/data/raw/cdisc-pilot/sdtm"
raw_adam_path <- "/bench/data/raw/cdisc-pilot/adam"

# AE (SDTM)
ae_raw <- read_xpt(file.path(raw_sdtm_path, "ae.xpt"))

# ADSL (ADaM)
adsl_raw <- read_xpt(file.path(raw_adam_path, "adsl.xpt"))

# ------------------------------------------------------------------
# Step 1: Process AE dataset
# ------------------------------------------------------------------
ae <- ae_raw %>%
  arrange(USUBJID, AESEQ) %>%
  mutate(
    ASTDT = sas_date(AESTDTC),
    AENDT = sas_date(AEENDTC),
    ADUR = if_else(!is.na(ASTDT) & !is.na(AENDT), AENDT - ASTDT + 1, NA_real_)
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
  )

# ------------------------------------------------------------------
# Step 3: Merge AE with ADSL (left join, keep all AE rows)
# ------------------------------------------------------------------
ae <- ae %>%
  left_join(adsl, by = "USUBJID")

# ------------------------------------------------------------------
# Step 4: Additional calculations
# ------------------------------------------------------------------
ae <- ae %>%
  mutate(
    # Impute ASTDT when missing and AESTDTC length = 7
    ASTDT = if_else(
      is.na(ASTDT) & !is.na(AESTDTC) & nchar(str_trim(AESTDTC)) == 7,
      sas_date(paste0(str_replace_all(AESTDTC, " ", ""), "-01")),
      ASTDT
    ),
    ASTDTF = if_else(is.na(ASTDT) & !is.na(AESTDTC) & nchar(str_trim(AESTDTC)) == 7, "D", NA_character_),
    # Relative days
    ASTDY = case_when(
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT ~ ASTDT - TRTSDT + 1,
      !is.na(ASTDT) & !is.na(TRTSDT) & TRTSDT > ASTDT ~ ASTDT - TRTSDT,
      TRUE ~ NA_real_
    ),
    AENDY = case_when(
      !is.na(AENDT) & !is.na(TRTSDT) & AENDT >= TRTSDT ~ AENDT - TRTSDT + 1,
      !is.na(AENDT) & !is.na(TRTSDT) & TRTSDT > AENDT ~ AENDT - TRTSDT,
      TRUE ~ NA_real_
    ),
    ANLSTDY = if_else(!is.na(ASTDT) & !is.na(TRTSDT), ASTDT - TRTSDT + 1, NA_real_),
    TRTEMFL = if_else(!is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT, "Y", "N"),
    # CQ01NAM logic
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
# Step 5: Create first‑occurrence flags using the macro logic
# ------------------------------------------------------------------
# AOCCFL – first AE per subject
aea  <- flagfrst(ae, sortvars = character(), invar = "", outvar = "AOCCFL", ser = FALSE, derm = FALSE)

# AOCCSFL – first AE per SOC
aeb  <- flagfrst(ae, sortvars = "AEBODSYS", invar = "AEBODSYS", outvar = "AOCCSFL", ser = FALSE, derm = FALSE)

# AOCCPFL – first AE per Preferred Term
aec  <- flagfrst(ae, sortvars = c("AEBODSYS", "AEDECOD"), invar = "AEDECOD", outvar = "AOCCPFL", ser = FALSE, derm = FALSE)

# Serious flags
saea <- flagfrst(ae, sortvars = character(), invar = "", outvar = "AOCC02FL", ser = TRUE, derm = FALSE)
saeb <- flagfrst(ae, sortvars = "AEBODSYS", invar = "AEBODSYS", outvar = "AOCC03FL", ser = TRUE, derm = FALSE)
saec <- flagfrst(ae, sortvars = c("AEBODSYS", "AEDECOD"), invar = "AEDECOD", outvar = "AOCC04FL", ser = TRUE, derm = FALSE)

# Dermatologic flag
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
# Step 7: Retain / reorder columns as in SAS RETAIN statement
# ------------------------------------------------------------------
ae_final <- ae %>%
  select(
    STUDYID, SITEID, USUBJID, TRTA, TRTAN, AGE, AGEGR1, AGEGR1N,
    RACE, RACEN, SEX, SAFFL, TRTSDT, TRTEDT,
    ASTDT, ASTDTF, ASTDY, AENDT, AENDY, ADUR,
    AETERM,
    AELLT, AELLTCD, AEDECOD, AEPTCD, AEHLT, AEHLTCD,
    AEHLGT, AEHLGTCD, AEBODSYS, AESOC, AESOCCD, AESEV,
    AESER, AESCAN, AESCONG, AESDISAB, AESDTH, AESHOSP,
    AESLIFE, AESOD,
    TRTEMFL, AOCCFL, AOCCSFL, AOCCPFL,
    AOCC02FL, AOCC03FL, AOCC04FL,
    CQ01NAM, AOCC01FL
  )

# Note: STUDYID is not present in source data; keep as NA if missing
if (!"STUDYID" %in% names(ae_final)) ae_final$STUDYID <- NA_character_
ae_final <- ae_final %>% relocate(STUDYID, .before = SITEID)

# ------------------------------------------------------------------
# Step 8: Write output dataset
# ------------------------------------------------------------------
write.csv(
  ae_final,
  file = file.path(out_dir, "adae.csv"),
  row.names = FALSE,
  na = ""
)

# End of script

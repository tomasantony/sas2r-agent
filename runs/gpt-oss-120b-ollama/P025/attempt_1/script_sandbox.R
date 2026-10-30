# P025_ex.R - translation of SAS program to R
# -------------------------------------------------
# Load required packages
library(haven)      # for reading XPT files
library(readxl)     # for reading Excel files
library(dplyr)      # data manipulation
library(stringr)    # string handling
library(lubridate)  # date handling
library(tidyr)      # for fill etc.

# -------------------------------------------------
# Helper: SAS-like round half up (not needed here but kept for completeness)
sas_round <- function(x, digits = 0) {
  posneg <- sign(x)
  absx   <- abs(x)
  factor  <- 10^digits
  rounded <- floor(absx * factor + 0.5) / factor
  return(rounded * posneg)
}

# Helper: convert SAS DATE11. (e.g., "01JAN2020") to ISO 8601 string
sas_date_to_iso <- function(x) {
  if (is.na(x) || x == "") return(NA_character_)
  # Try dmy parsing; SAS DATE11. is DDMMMYYYY (e.g., 01JAN2020)
  d <- as.Date(toupper(x), format = "%d%b%Y")
  if (is.na(d)) {
    # fallback: try ISO already
    d <- as.Date(x, format = "%Y-%m-%d")
  }
  if (is.na(d)) return(NA_character_)
  format(d, "%Y-%m-%d")
}

# -------------------------------------------------
# Set directories from environment / macro equivalents
rawdir   <- "/bench/data/raw"
rawsdtm  <- "/bench/data/raw/cdisc-pilot/sdtm"
out_dir  <- Sys.getenv("OUT_DIR")
if (out_dir == "") out_dir <- "."

# -------------------------------------------------
# 1. Load source data ---------------------------------------------------------

# SDTM DM dataset (assumed XPT file named dm.xpt in the SDTM folder)
dm_path <- file.path(rawsdtm, "dm.xpt")
if (!file.exists(dm_path)) {
  stop("DM XPT file not found at: ", dm_path)
}
sdtm_dm <- read_xpt(dm_path) %>%
  select(USUBJID, RFSTDTC) %>%          # keep only needed variables
  mutate(RFSTDTC = sas_date_to_iso(RFSTDTC)) %>%  # ensure ISO format
  arrange(USUBJID)

# EDC EX raw data (Excel)
ex_path <- file.path(rawdir, "faijal-oncology", "raw_data", "edc_ex.xlsx")
if (!file.exists(ex_path)) {
  stop("EX Excel file not found at: ", ex_path)
}
edc_ex <- read_excel(ex_path, guess_max = 10000) %>%
  # Ensure column names are as SAS expects (case‑insensitive)
  rename_with(~ toupper(.x))

# -------------------------------------------------
# 2. Prepare Raw EX Data -------------------------------------------------------

ex_prep <- edc_ex %>%
  mutate(
    # Create USUBJID
    USUBJID = paste0("ONCO-2026-", SUBJID),

    # Exclude records where DOSE is missing
    DOSE = as.numeric(DOSE)  # ensure numeric
  ) %>%
  filter(!is.na(DOSE)) %>%
  mutate(
    # Convert dates to ISO 8601 strings
    EXSTDTC = if_else(STDT == "" | is.na(STDT), NA_character_, sas_date_to_iso(STDT)),
    EXENDTC = if_else(ENDT == "" | is.na(ENDT), NA_character_, sas_date_to_iso(ENDT))
  ) %>%
  select(-STDT, -ENDT) %>%               # original raw date columns no longer needed
  arrange(USUBJID, EXSTDTC)

# -------------------------------------------------
# 3. Merge with DM to obtain RFSTDTC -------------------------------------------

sdtm_ex1 <- ex_prep %>%
  left_join(sdtm_dm, by = "USUBJID") %>%
  # Keep only rows that came from EX (left join already does this)
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN  = "EX",
    EXTRT   = toupper(TRT),
    EXDOSE  = DOSE,
    EXDOSU  = toupper(UNIT),
    EXDOSFRM = str_replace_all(FORM, "_", " "),
    EXDOSFRQ = toupper(FREQ),
    EXVLDIST = toupper(ROUTE),
    EXLOT   = LOT
  ) %>%
  # Derive EXSTDY
  mutate(
    EXSTDY = case_when(
      !is.na(EXSTDTC) & !is.na(RFSTDTC) ~ {
        stdt <- as.Date(EXSTDTC)
        rfdt <- as.Date(RFSTDTC)
        diff <- as.numeric(stdt - rfdt)
        if (diff >= 0) diff + 1 else diff
      },
      TRUE ~ NA_real_
    ),
    # Derive EXENDY
    EXENDY = case_when(
      !is.na(EXENDTC) & !is.na(RFSTDTC) ~ {
        endt <- as.Date(EXENDTC)
        rfdt <- as.Date(RFSTDTC)
        diff <- as.numeric(endt - rfdt)
        if (diff >= 0) diff + 1 else diff
      },
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, EXTRT, EXDOSE, EXDOSU, EXDOSFRM,
    EXDOSFRQ, EXVLDIST, EXLOT, EXSTDTC, EXENDTC,
    EXSTDY, EXENDY, VISIT = VISIT   # VISIT may be missing; keep as is
  ) %>%
  arrange(USUBJID, EXSTDTC)

# -------------------------------------------------
# 4. Derive EXSEQ (sequence within subject) ------------------------------------

sdtm_ex <- sdtm_ex1 %>%
  group_by(USUBJID) %>%
  mutate(EXSEQ = row_number()) %>%
  ungroup() %>%
  select(
    STUDYID, DOMAIN, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU,
    EXDOSFRM, EXDOSFRQ, EXVLDIST, EXLOT, EXSTDTC, EXENDTC,
    EXSTDY, EXENDY, VISIT
  )

# -------------------------------------------------
# 5. Output --------------------------------------------------------------

# Write final dataset as CSV (named after the program)
csv_path <- file.path(out_dir, "P025.csv")
write.csv(sdtm_ex, csv_path, row.names = FALSE, na = "")

# Create a plain‑text rendering similar to PROC PRINT output
txt_path <- file.path(out_dir, "P025.txt")
txt_lines <- capture.output(print(sdtm_ex, row.names = FALSE))
writeLines(txt_lines, txt_path)

# End of script

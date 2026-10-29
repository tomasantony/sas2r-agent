```r
# P022_ae translation to R
# -------------------------------------------------
# Load required packages (already installed in the environment)
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)

# -------------------------------------------------
# Helper functions -------------------------------------------------
# Parse SAS date11. (e.g., "01JAN2020") to Date
parse_date11 <- function(x) {
  # Return NA for missing or empty strings
  if (is.na(x) || str_trim(x) == "") return(NA_Date_)
  # Ensure month abbreviation is uppercase for lubridate
  x_up <- toupper(x)
  # Use dmy with format "%d%b%Y"
  as.Date(x_up, format = "%d%b%Y")
}

# Convert Date to ISO 8601 string (YYYY-MM-DD) or empty string if NA
date_to_iso <- function(d) {
  if (is.na(d)) "" else format(d, "%Y-%m-%d")
}

# -------------------------------------------------
# Set output directory -------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# -------------------------------------------------
# 1. Load raw Excel datasets -------------------------------------------------
# Paths based on macro variables from the SAS environment
raw_dir   <- "D:/meta_hackathon/sas2r-agent/data/raw"
edc_path  <- file.path(raw_dir, "faijal-oncology", "raw_data")

ae_raw    <- read_excel(file.path(edc_path, "edc_ae.xlsx"))
dm_raw    <- read_excel(file.path(edc_path, "edc_dm.xlsx"))

# -------------------------------------------------
# 2. Prepare DM for AE (keep USUBJID and RFSTDTC) -------------------------------------------------
dm_for_ae <- dm_raw %>%
  select(USUBJID, RFSTDTC) %>%
  # Ensure character type
  mutate(
    USUBJID = as.character(USUBJID),
    RFSTDTC = as.character(RFSTDTC)
  ) %>%
  arrange(USUBJID)   # SAS sorts missing first, then character order (case‑sensitive)

# -------------------------------------------------
# 3. Process raw AE data -------------------------------------------------
ae_prep <- ae_raw %>%
  mutate(
    # Create USUBJID
    USUBJID = paste0("ONCO-2026-", as.character(SUBJID)),
    # Convert STDT and ENDT to ISO 8601 strings
    AESTDTC = ifelse(!is.na(STDT) & str_trim(STDT) != "",
                     date_to_iso(parse_date11(as.character(STDT))),
                     ""),
    AEENDTC = ifelse(!is.na(ENDT) & str_trim(ENDT) != "",
                     date_to_iso(parse_date11(as.character(ENDT))),
                     "")
  ) %>%
  # Keep needed columns for later steps (others are retained for merge)
  mutate(
    USUBJID = as.character(USUBJID),
    AESTDTC = as.character(AESTDTC),
    AEENDTC = as.character(AEENDTC)
  ) %>%
  arrange(USUBJID, AESTDTC, TERM)   # SAS sort order

# -------------------------------------------------
# 4. Merge AE with DM (left join, keep only AE rows) -------------------------------------------------
sdtm_ae1 <- ae_prep %>%
  left_join(dm_for_ae, by = "USUBJID", suffix = c("", ".dm")) %>%
  # Keep only rows that came from AE (the merge in SAS used IN= a)
  filter(!is.na(USUBJID)) %>%
  mutate(
    # Identifier variables
    STUDYID = "ONCO-2026",
    DOMAIN  = "AE",
    # AE variables
    AETERM  = as.character(TERM),
    AEDECOD = toupper(as.character(DECOD)),
    AEBODSYS = gsub("_", " ", as.character(BODSYS)),
    AESEV   = toupper(as.character(SEV)),
    AETXGR  = as.numeric(TXGR),
    AEREL   = toupper(as.character(REL)),
    AEOUT   = toupper(as.character(OUT)),
    AESER   = as.character(SER)
  ) %>%
  # Derive AESTDY
  mutate(
    AESTDY = ifelse(AESTDTC != "" & RFSTDTC != "",
                    {
                      _stdt <- as.Date(AESTDTC)
                      _rfdt <- as.Date(RFSTDTC)
                      diff  <- as.numeric(_stdt - _rfdt)
                      ifelse(_stdt >= _rfdt, diff + 1, diff)
                    },
                    NA_real_)
  ) %>%
  # Derive AEENDY
  mutate(
    AEENDY = ifelse(AEENDTC != "" & RFSTDTC != "",
                    {
                      _endt <- as.Date(AEENDTC)
                      _rfdt <- as.Date(RFSTDTC)
                      diff  <- as.numeric(_endt - _rfdt)
                      ifelse(_endt >= _rfdt, diff + 1, diff)
                    },
                    NA_real_)
  ) %>%
  # Ensure character columns are character (not factors)
  mutate(across(where(is.factor), as.character))

# -------------------------------------------------
# 5. Sort final AE dataset -------------------------------------------------
sdtm_ae1 <- sdtm_ae1 %>%
  arrange(USUBJID, AESTDTC, TERM)

# -------------------------------------------------
# 6. Derive AESEQ (sequence within each USUBJID) -------------------------------------------------
sdtm_ae <- sdtm_ae1 %>%
  group_by(USUBJID) %>%
  mutate(AESEQ = row_number()) %>%
  ungroup()

# -------------------------------------------------
# 7. Keep final SDTM variables -------------------------------------------------
ae_final <- sdtm_ae %>%
  select(
    STUDYID, DOMAIN, USUBJID, AESEQ,
    AETERM, AEDECOD, AEBODSYS, AESEV,
    AETXGR, AEREL, AEOUT, AESER,
    AESTDTC, AEENDTC, AESTDY, AEENDY
  )

# -------------------------------------------------
# 8. Write outputs -------------------------------------------------
# Write AE dataset as CSV
write.csv(
  ae_final,
  file = file.path(out_dir, "ae.csv"),
  row.names = FALSE,
  na = ""
)

# Write a plain‑text rendering of the listing (similar to PROC PRINT)
write.table(
  ae_final,
  file = file.path(out_dir, "P022.lst.txt"),
  sep = "\t",
  row.names = FALSE,
  quote = FALSE,
  na = ""
)

# Also write the same table body as CSV for the listing (as required)
write.csv(
  ae_final,
  file = file.path(out_dir, "P022.lst.csv"),
  row.names = FALSE,
  na = ""
)

# -------------------------------------------------
# End of script
```
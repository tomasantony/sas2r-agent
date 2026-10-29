# P022_ae translation to R (attempt 3)
# -------------------------------------------------
# Load required packages (pre‑installed in the execution environment)
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)

# -------------------------------------------------
# Helper functions -------------------------------------------------
# Parse SAS DATE11. (e.g., "01JAN2020") to Date
parse_date11 <- function(x) {
  if (is.na(x) || str_trim(x) == "") return(NA_Date_)
  # Upper‑case month abbreviation for consistent parsing
  x_up <- toupper(x)
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
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# -------------------------------------------------
# 1. Load raw Excel datasets -------------------------------------------------
raw_dir  <- "/bench/data/raw"
edc_path <- file.path(raw_dir, "faijal-oncology", "raw_data")

ae_raw <- read_excel(file.path(edc_path, "edc_ae.xlsx"))
dm_raw <- read_excel(file.path(edc_path, "edc_dm.xlsx"))

# -------------------------------------------------
# 2. Normalise column names (trim, upper case) -------------------------------
clean_names <- function(df) {
  names(df) <- toupper(str_trim(names(df)))
  df
}
ae_raw <- clean_names(ae_raw)
dm_raw <- clean_names(dm_raw)

# -------------------------------------------------
# 3. Verify required DM columns -------------------------------------------
required_dm <- c("USUBJID", "RFSTDTC")
missing_dm <- setdiff(required_dm, names(dm_raw))
if (length(missing_dm) > 0) {
  stop(paste("DM dataset missing required columns:", paste(missing_dm, collapse = ", ")))
}

dm_for_ae <- dm_raw %>%
  select(all_of(required_dm)) %>%
  mutate(
    USUBJID = as.character(USUBJID),
    RFSTDTC = as.character(RFSTDTC)
  ) %>%
  arrange(USUBJID)   # SAS sorts missing first, then case‑sensitive character order

# -------------------------------------------------
# 4. Verify required AE columns -------------------------------------------
required_ae <- c("SUBJID", "STDT", "ENDT", "TERM", "DECOD",
                "BODSYS", "SEV", "TXGR", "REL", "OUT", "SER")
missing_ae <- setdiff(required_ae, names(ae_raw))
if (length(missing_ae) > 0) {
  stop(paste("AE dataset missing required columns:", paste(missing_ae, collapse = ", ")))
}

# -------------------------------------------------
# 5. Process raw AE data -------------------------------------------------
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
  mutate(
    USUBJID = as.character(USUBJID),
    AESTDTC = as.character(AESTDTC),
    AEENDTC = as.character(AEENDTC)
  ) %>%
  arrange(USUBJID, AESTDTC, TERM)

# -------------------------------------------------
# 6. Merge AE with DM (left join, keep only AE rows) -----------------------
sdtm_ae1 <- ae_prep %>%
  left_join(dm_for_ae, by = "USUBJID", suffix = c("", ".dm")) %>%
  # Keep only rows that originated from AE (the SAS merge used IN= a)
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
  # Ensure character columns are plain character vectors (not factors)
  mutate(across(where(is.factor), as.character))

# -------------------------------------------------
# 7. Sort final AE dataset -------------------------------------------------
sdtm_ae1 <- sdtm_ae1 %>%
  arrange(USUBJID, AESTDTC, TERM)

# -------------------------------------------------
# 8. Derive AESEQ (sequence within each USUBJID) ---------------------------
sdtm_ae <- sdtm_ae1 %>%
  group_by(USUBJID) %>%
  mutate(AESEQ = row_number()) %>%
  ungroup()

# -------------------------------------------------
# 9. Keep final SDTM variables --------------------------------------------
ae_final <- sdtm_ae %>%
  select(
    STUDYID, DOMAIN, USUBJID, AESEQ,
    AETERM, AEDECOD, AEBODSYS, AESEV,
    AETXGR, AEREL, AEOUT, AESER,
    AESTDTC, AEENDTC, AESTDY, AEENDY
  )

# -------------------------------------------------
# 10. Write outputs ---------------------------------------------------------
# AE dataset as CSV (SAS expects an Excel file, but the specification for this task
# requires a CSV version)
write.csv(
  ae_final,
  file = file.path(out_dir, "ae.csv"),
  row.names = FALSE,
  na = ""
)

# Listing (PROC PRINT style) as plain‑text (tab‑separated)
write.table(
  ae_final,
  file = file.path(out_dir, "P022.lst.txt"),
  sep = "\t",
  row.names = FALSE,
  quote = FALSE,
  na = ""
)

# Listing body as CSV (as required by the evaluation harness)
write.csv(
  ae_final,
  file = file.path(out_dir, "P022.lst.csv"),
  row.names = FALSE,
  na = ""
)

# -------------------------------------------------
# End of script

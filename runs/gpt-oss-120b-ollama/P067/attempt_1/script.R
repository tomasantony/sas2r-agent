# P067_ae translation to R
# -------------------------------------------------
# This script reproduces the SAS program P067_ae.sas.
# It reads the required raw and SDTM datasets, performs the same
# data steps, and writes the resulting datasets to the directory
# specified by the environment variable OUT_DIR.
# -------------------------------------------------

# Load required packages (already installed in the environment)
library(haven)      # for reading SAS datasets
library(dplyr)      # data manipulation
library(lubridate)  # date handling
library(readr)      # for write_csv (optional, but we use base write.csv)

# ------------------------------------------------------------------
# Define paths (hard‑coded from the macro variables / librefs)
# ------------------------------------------------------------------
raw_dir  <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm"

raw_ae_path <- file.path(raw_dir,  "raw_ae.sas7bdat")
dm_path     <- file.path(sdtm_dir, "dm.sas7bdat")

# ------------------------------------------------------------------
# Output directory
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "" ) {
  stop("Environment variable OUT_DIR is not set.")
}
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# 1. Read input datasets
# ------------------------------------------------------------------
raw_ae <- read_sas(raw_ae_path)
dm     <- read_sas(dm_path)

# ------------------------------------------------------------------
# 2. PROC SORT equivalents (by SUBJID, case‑sensitive, missing first)
# ------------------------------------------------------------------
raw_ae <- raw_ae %>% arrange(SUBJID)
dm     <- dm %>% arrange(SUBJID)

# ------------------------------------------------------------------
# 3. Write the sorted raw_ae dataset (expected output: raw_ae.csv)
# ------------------------------------------------------------------
write.csv(
  raw_ae,
  file = file.path(out_dir, "raw_ae.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 4. MERGE raw_ae with dm (by SUBJID) – keep only rows where raw_ae exists
# ------------------------------------------------------------------
ae_merged <- raw_ae %>%
  inner_join(dm, by = "SUBJID", suffix = c(".raw", ".dm"))

# ------------------------------------------------------------------
# 5. Create variables exactly as in the SAS DATA step
# ------------------------------------------------------------------
ae <- ae_merged %>%
  mutate(
    # STUDYID length $10, value "ABC-301"
    STUDYID = "ABC-301",

    # Convert SAS date numeric (Date class) to character ISO 8601 (YYYY-MM-DD)
    AESTDTC = if_else(!is.na(AESTDT), format(AESTDT, "%Y-%m-%d"), NA_character_),
    AEENDTC = if_else(!is.na(AEENDT), format(AEENDT, "%Y-%m-%d"), NA_character_),

    # Convert RFSTDTC (character) to Date for arithmetic
    rfstdt_num = ymd(RFSTDTC),

    # AE Start Day (ASTDY)
    ASTDY = case_when(
      is.na(AESTDT) ~ NA_real_,
      AESTDT >= rfstdt_num ~ as.numeric(AESTDT - rfstdt_num + 1),
      TRUE                 ~ as.numeric(AESTDT - rfstdt_num)
    ),

    # AE End Day (AENDY)
    AENDY = case_when(
      is.na(AEENDT) ~ NA_real_,
      AEENDT >= rfstdt_num ~ as.numeric(AEENDT - rfstdt_num + 1),
      TRUE                 ~ as.numeric(AEENDT - rfstdt_num)
    )
  ) %>%
  # Keep only the variables required in the final SAS dataset
  select(
    STUDYID,
    USUBJID,
    SUBJID,
    AETERM,
    AESER,
    AESEV,
    AESTDTC,
    AEENDTC,
    ASTDY,
    AENDY
  )

# ------------------------------------------------------------------
# 6. Write the final AE dataset (expected output: ae.csv)
# ------------------------------------------------------------------
write.csv(
  ae,
  file = file.path(out_dir, "ae.csv"),
  row.names = FALSE,
  na = ""
)

# End of script

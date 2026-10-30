# ------------------------------------------------------------
# R translation of SAS program: P016_sdtm_mh.sas
# ------------------------------------------------------------
# Load required packages
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

# ------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------
# Convert SAS numeric date (days since 1960-01-01) to R Date
sas_num_to_date <- function(x) {
  as.Date(x, origin = "1960-01-01")
}

# Parse character dates using a flexible set of formats (SAS ANYDTDTE. behavior)
parse_anydtdte <- function(x) {
  # readr::parse_date_time returns POSIXct; convert to Date
  parsed <- parse_date_time(x,
                            orders = c("Ymd", "Y-m-d", "Y/m/d",
                                       "mdY", "m/d/Y", "m-d-Y",
                                       "dmY", "d/m/Y", "d-m-Y",
                                       "Ymd HMS", "Y-m-d H:M:S",
                                       "mdY HMS", "dmY HMS"),
                            exact = FALSE,
                            tz = "UTC")
  as.Date(parsed)
}

# ------------------------------------------------------------------
# Set up paths (macro variables from SAS environment)
# ------------------------------------------------------------------
project_path   <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
usubjid_length <- 40   # retained for reference only

# Input raw CSV (PROC IMPORT equivalent)
raw_mh_path <- file.path(project_path, "data", "raw", "raw_mh.csv")

# Output directory (provided by the harness)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("OUT_DIR environment variable is not set.")
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------
# Step 1: Import raw data (PROC IMPORT)
# ------------------------------------------------------------------
raw_mh <- read_csv(
  raw_mh_path,
  col_types = cols(.default = col_guess()),
  na = c("", "NA")
)

# ------------------------------------------------------------------
# Step 2: Create SDTM.MH dataset (first DATA step)
# ------------------------------------------------------------------
mh <- raw_mh %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN  = "MH",
    USUBJID = str_c(STUDYID, str_trim(SUBJECT), sep = "-"),
    MHTERM  = str_trim(CONDITION),
    # Preserve original DIAGNOSIS_DATE (could be character or numeric)
    mhdt_raw = DIAGNOSIS_DATE,
    mhdt = case_when(
      # Character dates
      is.character(mhdt_raw) ~ parse_anydtdte(str_trim(mhdt_raw)),
      # Numeric SAS dates
      is.numeric(mhdt_raw)   ~ sas_num_to_date(mhdt_raw),
      TRUE                    ~ as.Date(NA)
    ),
    MHSTDTC = if_else(!is.na(mhdt),
                      format(mhdt, "%Y-%m-%d"),
                      NA_character_)
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, MHTERM, MHSTDTC)

# ------------------------------------------------------------------
# Step 3: Sort by USUBJID then MHSTDTC (PROC SORT)
# ------------------------------------------------------------------
mh <- mh %>%
  arrange(USUBJID, MHSTDTC)

# ------------------------------------------------------------------
# Step 4: Add MHSEQ (retain + BY‑group processing)
# ------------------------------------------------------------------
mh <- mh %>%
  group_by(USUBJID) %>%
  mutate(MHSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, MHSEQ, MHTERM, MHSTDTC)

# ------------------------------------------------------------------
# Step 5: Write output dataset
# ------------------------------------------------------------------
output_path <- file.path(out_dir, "mh.csv")
write.csv(
  mh,
  file = output_path,
  row.names = FALSE,
  na = ""
)

cat("SDTM MH dataset written to:", output_path, "\n")

```r
# ------------------------------------------------------------
# R translation of SAS program: P016_sdtm_mh.sas
# ------------------------------------------------------------
# Load required packages
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

# ------------------------------------------------------------------
# Define helper functions
# ------------------------------------------------------------------
# SAS round-half-up is not needed here (no rounding in this program)

# Convert SAS numeric date (days since 1960-01-01) to R Date
sas_num_to_date <- function(x) {
  as.Date(x, origin = "1960-01-01")
}

# Parse character date strings using a set of common formats,
# mimicking SAS ANYDTDTE. Returns a Date (or NA)
parse_anydtdte <- function(x) {
  # try a vector of formats; readr::parse_date works similarly
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
# Set up paths
# ------------------------------------------------------------------
# Macro variables from the SAS environment (hard‑coded as per description)
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
usubjid_length <- 40  # not directly used, just for reference

# Input raw CSV (PROC IMPORT equivalent)
raw_mh_path <- file.path(project_path, "data", "raw", "raw_mh.csv")

# Output directory (environment variable set by the harness)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "" ) {
  stop("OUT_DIR environment variable is not set.")
}

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
    # USUBJID = catx('-', STUDYID, strip(SUBJECT));
    USUBJID = str_c(STUDYID, str_trim(SUBJECT), sep = "-"),
    MHTERM  = str_trim(CONDITION),
    # Handle DIAGNOSIS_DATE which may be character or numeric SAS date
    _mhdt_raw = DIAGNOSIS_DATE,
    _mhdt = case_when(
      # character column
      is.character(_mhdt_raw) ~ parse_anydtdte(str_trim(_mhdt_raw)),
      # numeric column (assumed SAS date)
      is.numeric(_mhdt_raw) ~ sas_num_to_date(_mhdt_raw),
      TRUE ~ as.Date(NA)
    ),
    MHSTDTC = if_else(!is.na(_mhdt),
                      format(_mhdt, "%Y-%m-%d"),
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
```
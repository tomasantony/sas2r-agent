# P012_sdtm_ds.R
# Reimplementation of SAS program P012_sdtm_ds.sas in R
# -------------------------------------------------
# Required libraries (already installed in the execution environment)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# ------------------------------------------------------------------
# 1. Define paths from macro variables (hard‑coded as per the SAS run)
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_demog_path <- file.path(project_path, "data", "raw", "raw_demog.csv")

# Output directory supplied by the harness
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("Environment variable OUT_DIR is not set.")
}
# Ensure the directory exists
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# 2. Import raw demographic data (PROC IMPORT equivalent)
raw_ds <- read_csv(
  raw_demog_path,
  col_types = cols(.default = col_character()),
  na = c("", "NA")
)

# ------------------------------------------------------------------
# 3. Create the SDTM DS dataset (DATA step logic)
# Helper to parse dates similarly to SAS ANYDTDTM.  We try several common orders.
parse_any_date <- function(x) {
  # Return NA if input is missing
  if (is.na(x) | str_trim(x) == "") return(NA_Date_)
  # Try several orders; the first that succeeds is kept
  orders <- c("Ymd", "Y-m-d", "mdY", "dmy", "Ymd HMS", "Y-m-d HMS")
  for (ord in orders) {
    dt <- suppressWarnings(parse_date_time(x, orders = ord, exact = FALSE))
    if (!is.na(dt)) return(as_date(dt))
  }
  # Fallback to as_date (may produce NA)
  as_date(x, format = "%Y-%m-%d")
}

ds <- raw_ds %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN  = "DS",
    # USUBJID = catx("-", STUDYID, SUBJ_ID)  (strip spaces)
    USUBJID = str_c(STUDYID, "-", str_trim(SUBJ_ID)),
    DSSEQ   = 1L,
    DSCAT   = "DISPOSITION EVENT",
    DSSCAT  = "STUDY PARTICIPATION",
    DSTERM  = str_to_upper(str_trim(DISPOSITION_REASON)),
    DSDECOD = DSTERM,
    EPOCH   = if_else(
                str_to_upper(str_trim(DISPOSITION)) == "SCREEN FAILURE",
                "SCREENING",
                "FOLLOW-UP"
              ),
    # Handle STUDY_END_DAT which may be character or numeric (date)
    _dsdt_raw = STUDY_END_DAT,
    _dsdt = case_when(
      # If the column is character, parse it
      is.character(_dsdt_raw) ~ parse_any_date(_dsdt_raw),
      # If numeric (SAS date value), treat as days since 1960-01-01
      is.numeric(_dsdt_raw) ~ as_date(_dsdt_raw, origin = "1960-01-01"),
      TRUE ~ as_date(NA)
    ),
    DSSTDTC = if_else(!is.na(_dsdt),
                      format(_dsdt, "%Y-%m-%d"),
                      "")
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, DSSEQ,
    DSCAT, DSSCAT, DSTERM, DSDECOD,
    EPOCH, DSSTDTC
  ) %>%
  # PROC SORT BY USUBJID DSSEQ (stable, keep duplicates)
  arrange(USUBJID, DSSEQ)

# ------------------------------------------------------------------
# 4. Write the final DS dataset as CSV
ds_csv_path <- file.path(out_dir, "ds.csv")
write.csv(ds, ds_csv_path, row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 5. Produce the listing (first 12 observations) – PROC PRINT equivalent
listing_df <- head(ds, 12)

# 5a. Text rendering (simple fixed‑width style)
listing_txt_path <- file.path(out_dir, "P012.lst.txt")
txt_con <- file(listing_txt_path, open = "wt", encoding = "UTF-8")
# Header line (column names)
writeLines(paste(names(listing_df), collapse = "  "), txt_con)
# Data lines
apply(listing_df, 1, function(row) {
  writeLines(paste(row, collapse = "  "), txt_con)
})
close(txt_con)

# 5b. CSV version of the same listing
listing_csv_path <- file.path(out_dir, "P012.lst.csv")
write.csv(listing_df, listing_csv_path, row.names = FALSE, na = "")

# ------------------------------------------------------------------
# End of script – all required outputs have been created.
message("R script completed successfully. Outputs written to: ", out_dir)

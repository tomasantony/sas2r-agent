# P012_sdtm_ds.R
# -------------------------------------------------
# Reimplementation of SAS program P012_sdtm_ds.sas in R
# -------------------------------------------------

# Load required packages (pre‑installed in the execution environment)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# ------------------------------------------------------------------
# 1. Define paths (mirroring the macro variables used in SAS)
project_path   <- "/bench/data/derived/c2c"
raw_demog_path <- file.path(project_path, "data", "raw", "raw_demog.csv")

# Output directory supplied by the harness
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("Environment variable OUT_DIR is not set.")
}
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# 2. Import the raw demographic CSV (PROC IMPORT equivalent)
# Use default column type guessing – SAS IMPORT would treat everything as character,
# but we need numeric dates to be recognised for the SAS date‑value branch.
raw_ds <- read_csv(
  raw_demog_path,
  na = c("", "NA")
)

# ------------------------------------------------------------------
# 3. Helper: parse character dates the way SAS ANYDTDTM. tries.
parse_any_date <- function(x) {
  if (is.na(x) || str_trim(x) == "") return(NA_Date_)
  # Try a handful of common ISO‑like orders; the first successful parse is kept.
  orders <- c("Ymd", "Y-m-d", "mdY", "dmy", "Ymd HMS", "Y-m-d HMS")
  for (ord in orders) {
    dt <- suppressWarnings(parse_date_time(x, orders = ord, exact = FALSE))
    if (!is.na(dt)) return(as_date(dt))
  }
  # Fallback – may still be NA
  as_date(x, format = "%Y-%m-%d")
}

# ------------------------------------------------------------------
# 4. Create the SDTM DS dataset (mirroring the SAS DATA step)
ds <- raw_ds %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN  = "DS",
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
    # ----------------------------------------------------------------
    # Handle STUDY_END_DAT which may be character (date string) or numeric
    # (SAS date value = days since 1960‑01‑01)
    _dsdt_raw = STUDY_END_DAT,
    _dsdt = case_when(
      is.character(_dsdt_raw) ~ parse_any_date(_dsdt_raw),
      is.numeric(_dsdt_raw)   ~ as_date(_dsdt_raw, origin = "1960-01-01"),
      TRUE                     ~ as_date(NA)
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
# 5. Write the final DS dataset as CSV (dataset output)
ds_csv_path <- file.path(out_dir, "ds.csv")
write.csv(ds, ds_csv_path, row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 6. Produce the listing – first 12 observations (PROC PRINT equivalent)
listing_df <- head(ds, 12)

# 6a. Text rendering (plain‑text file)
listing_txt_path <- file.path(out_dir, "P012.lst.txt")
txt_con <- file(listing_txt_path, open = "wt", encoding = "UTF-8")

# Header line (column names)
writeLines(paste(names(listing_df), collapse = "  "), txt_con)

# Data lines – each field separated by two spaces (simple fixed‑width style)
apply(listing_df, 1, function(row) {
  writeLines(paste(row, collapse = "  "), txt_con)
})
close(txt_con)

# 6b. CSV version of the same listing
listing_csv_path <- file.path(out_dir, "P012.lst.csv")
write.csv(listing_df, listing_csv_path, row.names = FALSE, na = "")

# ------------------------------------------------------------------
# Completion message (appears in the console log)
message("R script completed successfully. Outputs written to: ", out_dir)

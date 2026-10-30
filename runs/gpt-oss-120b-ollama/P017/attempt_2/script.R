# ------------------------------------------------------------
# R translation of SAS program P017_sdtm_sv.sas
# ------------------------------------------------------------
# Load required packages
library(dplyr)
library(lubridate)
library(readr)
library(stringr)

# ------------------------------------------------------------------
# Helper: parse SAS dates (numeric days since 1960-01-01) or character dates
# ------------------------------------------------------------------
parse_sas_date <- function(x) {
  if (is.numeric(x)) {
    # Numeric SAS date (days since 1960-01-01)
    as.Date(x, origin = "1960-01-01")
  } else if (is.character(x)) {
    # Character dates – try to interpret using common SAS date patterns
    # anydtdte32. accepts many formats; we approximate with parse_date_time
    parsed <- parse_date_time(
      x,
      orders = c(
        "dmy", "mdy", "ymd",          # numeric orders
        "d-b-Y", "d-b-y", "d-b-Y",   # e.g., 15-JAN-2023
        "d-b-y", "d-b-Y", "d-b-y",   # variations
        "Ymd", "Ydm", "dmY", "mdY"
      ),
      exact = FALSE,
      tz = "UTC"
    )
    as.Date(parsed)
  } else {
    NA
  }
}

# ------------------------------------------------------------------
# Environment setup
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("OUT_DIR environment variable is not set.")
}
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# Paths (derived from macro variables in the original SAS code)
# ------------------------------------------------------------------
project_path   <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_vitals_path <- file.path(project_path, "data", "raw", "raw_vitals.csv")

# ------------------------------------------------------------------
# Step 1: Import raw vitals CSV (equivalent to PROC IMPORT)
# ------------------------------------------------------------------
raw_sv <- read_csv(
  raw_vitals_path,
  col_types = cols(.default = col_guess()),
  na = c("", "NA")
)

# ------------------------------------------------------------------
# Step 2: Create SDTM.SV dataset (equivalent to DATA step)
# ------------------------------------------------------------------
sv <- raw_sv %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN  = "SV",
    USUBJID = str_c(STUDYID, PT_ID, sep = "-"),
    VISITNUM = VISIT_NUM,
    VISIT = str_trim(VISIT, side = "both"),
    VISITDY = case_when(
      VISITNUM == 1 ~ 1L,
      VISITNUM == 2 ~ 31L,
      VISITNUM == 3 ~ 61L,
      TRUE          ~ NA_integer_
    ),
    _svdt = parse_sas_date(VS_DATE),
    SVSTDTC = if_else(!is.na(_svdt),
                     format(_svdt, "%Y-%m-%d"),
                     NA_character_),
    SVENDTC = SVSTDTC
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, VISITNUM,
    VISIT, VISITDY, SVSTDTC, SVENDTC
  )

# ------------------------------------------------------------------
# Step 3: Sort by USUBJID then VISITNUM (missing values first)
# ------------------------------------------------------------------
sv_sorted <- sv[order(sv$USUBJID,
                      sv$VISITNUM,
                      na.last = FALSE), ]

# ------------------------------------------------------------------
# Step 4: Write the SV dataset to CSV (dataset output)
# ------------------------------------------------------------------
write.csv(
  sv_sorted,
  file = file.path(out_dir, "sv.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# Step 5: Produce the listing (first 12 observations) – PROC PRINT
# ------------------------------------------------------------------
listing_df <- head(sv_sorted, 12)

# Text rendering (SAS-like listing) – write to P017.lst
txt_path <- file.path(out_dir, "P017.lst")
con <- file(txt_path, open = "wt", encoding = "UTF-8")
writeLines("SV Domain Audit", con)
writeLines(strrep("-", 80), con)
write.table(
  listing_df,
  file = con,
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = ""
)
close(con)

# CSV version of the listing body (optional companion file)
write.csv(
  listing_df,
  file = file.path(out_dir, "P017.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------

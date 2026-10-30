```r
# ------------------------------------------------------------
# R translation of SAS program P011_sdtm_dm.sas (attempt 2)
# ------------------------------------------------------------
# Load required packages (already installed in the environment)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# ------------------------------------------------------------------
# Helper: Convert mixed SAS numeric dates or character dates to ISO 8601 (YYYY-MM-DD)
# ------------------------------------------------------------------
convert_to_iso <- function(x) {
  # Preserve NA and empty strings as NA
  x <- ifelse(is.na(x) | str_trim(x) == "", NA_character_, x)
  # Try numeric conversion (SAS date = days since 1960-01-01)
  num <- suppressWarnings(as.numeric(x))
  is_num_date <- !is.na(num) & grepl("^[-+]?[0-9]+$", x)
  out <- rep(NA_character_, length(x))
  if (any(is_num_date, na.rm = TRUE)) {
    out[is_num_date] <- format(as.Date(num[is_num_date], origin = "1960-01-01"),
                              "%Y-%m-%d")
  }
  # Remaining values: attempt to parse common date formats
  need_parse <- !(is_num_date) & !is.na(x)
  if (any(need_parse, na.rm = TRUE)) {
    parsed <- parse_date_time(x[need_parse],
                              orders = c("Ymd", "Y-m-d", "Ymd HMS", "Y-m-d H:M:S",
                                         "mdY", "m/d/Y", "dmy", "d/m/Y"))
    out[need_parse] <- format(as.Date(parsed), "%Y-%m-%d")
  }
  out
}

# ------------------------------------------------------------------
# Set paths
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Project path as defined by the SAS macro variables
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"

raw_file <- file.path(project_path, "data", "raw", "raw_demog.csv")

# ------------------------------------------------------------------
# 1. IMPORT RAW EDC DATA (PROC IMPORT equivalent)
# ------------------------------------------------------------------
raw_dm <- read_csv(
  raw_file,
  col_types = cols(.default = "c"),
  na = c("", "NA")
)

# ------------------------------------------------------------------
# 2. PREPARE SITEID (character vs numeric handling)
# ------------------------------------------------------------------
SITEID_vec <- if (is.character(raw_dm$SITE)) {
  str_trim(raw_dm$SITE)
} else {
  str_trim(as.character(raw_dm$SITE))
}

# ------------------------------------------------------------------
# 3. TRANSFORMATION AND SDTM MAPPING
# ------------------------------------------------------------------
dm <- raw_dm %>%
  mutate(
    # Required identifiers
    STUDYID = "CDISC-01",
    DOMAIN  = "DM",
    SUBJID  = SUBJ_ID,
    USUBJID = paste0(STUDYID, "-", SUBJID),

    # SITEID (already prepared)
    SITEID = SITEID_vec,

    # SEX mapping (first character, case‑insensitive)
    SEX = case_when(
      str_to_upper(str_sub(GENDER, 1, 1)) == "M" ~ "M",
      str_to_upper(str_sub(GENDER, 1, 1)) == "F" ~ "F",
      TRUE                                      ~ "U"
    ),

    # RACE controlled terminology
    RACE = case_when(
      str_to_upper(str_trim(RACE_TXT)) == "WHITE"  ~ "WHITE",
      str_to_upper(str_trim(RACE_TXT)) == "BLACK"  ~ "BLACK OR AFRICAN AMERICAN",
      str_to_upper(str_trim(RACE_TXT)) == "ASIAN"  ~ "ASIAN",
      str_to_upper(str_trim(RACE_TXT)) == "OTHER"  ~ "OTHER",
      TRUE                                          ~ "UNKNOWN"
    ),

    # ARM / ARMCD mapping
    ARM = case_when(
      str_trim(RANDOMIZED_ARM) == "Not Randomized"   ~ "SCREEN FAILURE",
      str_trim(RANDOMIZED_ARM) == "Placebo"          ~ "Placebo",
      str_trim(RANDOMIZED_ARM) == "Active Drug 50mg" ~ "Active Drug 50mg",
      TRUE                                            ~ NA_character_
    ),
    ARMCD = case_when(
      str_trim(RANDOMIZED_ARM) == "Not Randomized"   ~ "SCRNFAIL",
      str_trim(RANDOMIZED_ARM) == "Placebo"          ~ "PBO",
      str_trim(RANDOMIZED_ARM) == "Active Drug 50mg" ~ "ACT50",
      TRUE                                            ~ NA_character_
    ),

    # Date conversions to ISO 8601
    BRTHDTC = convert_to_iso(BRTH_DT),
    RFICDTC = convert_to_iso(ICF_DAT),
    RFSTDTC = RFICDTC,
    RFENDTC = convert_to_iso(STUDY_END_DAT),
    RFPENDTC = RFENDTC
  ) %>%
  # Keep only SDTM variables in the order specified by the SAS KEEP statement
  select(
    STUDYID, DOMAIN, USUBJID, SUBJID, SITEID,
    SEX, RACE, BRTHDTC, RFSTDTC, RFENDTC,
    RFPENDTC, RFICDTC, ARM, ARMCD
  )

# ------------------------------------------------------------------
# 4. WRITE OUTPUT DATASET (dm)
# ------------------------------------------------------------------
dm_csv_path <- file.path(out_dir, "dm.csv")
write.csv(dm, dm_csv_path, row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 5. LISTING: First 10 records (PROC PRINT equivalent)
# ------------------------------------------------------------------
listing_df <- dm %>%
  select(
    STUDYID, USUBJID, SITEID, SEX, RACE, BRTHDTC,
    RFICDTC, RFSTDTC, RFENDTC, RFPENDTC, ARM, ARMCD
  ) %>%
  slice_head(n = 10)

# Text rendering (space‑separated, similar to SAS PROC PRINT)
listing_txt_path <- file.path(out_dir, "P011.lst.txt")
write.table(
  listing_df,
  file = listing_txt_path,
  sep = " ",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = ""
)

# CSV version of the listing (required by the evaluation harness)
listing_csv_path <- file.path(out_dir, "P011.lst.csv")
write.csv(listing_df, listing_csv_path, row.names = FALSE, na = "")

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
```
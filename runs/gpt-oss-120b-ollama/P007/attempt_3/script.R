# ------------------------------------------------------------
# R translation of P007_adam_adtte.sas (attempt 3)
# ------------------------------------------------------------
# Required libraries
library(haven)      # read SAS datasets
library(dplyr)      # data manipulation
library(lubridate)  # date handling
library(readr)      # write CSV with proper NA handling

# ------------------------------------------------------------------
# Helper: round half up (SAS default)
# ------------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  pos <- x >= 0
  factor <- 10 ^ digits
  res <- ifelse(pos,
                floor(x * factor + 0.5) / factor,
                -floor(-x * factor + 0.5) / factor)
  res
}

# Helper: SAS DATE9. format (e.g., 01JAN2020)
sas_date9 <- function(date_vec) {
  ifelse(is.na(date_vec),
         "",
         toupper(format(date_vec, "%d%b%Y")))
}

# ------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("Environment variable OUT_DIR is not set.")

# Paths based on the macro variables defined in the SAS environment
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
adam_path    <- file.path(project_path, "data", "adam")

# ------------------------------------------------------------------
# 1. READ INPUT DATASETS
# ------------------------------------------------------------------
adae_file <- file.path(adam_path, "adae.sas7bdat")
adsl_file <- file.path(adam_path, "adsl.sas7bdat")

adae <- read_sas(adae_file)
adsl <- read_sas(adsl_file)

# ------------------------------------------------------------------
# 2. GET FIRST ADVERSE EVENT PER SUBJECT (TRTEMFL='Y')
# ------------------------------------------------------------------
adae_first <- adae %>%
  filter(TRTEMFL == "Y") %>%                     # where TRTEMFL = 'Y'
  arrange(USUBJID, AESTDT) %>%                   # sort by USUBJID AESTDT
  group_by(USUBJID) %>%
  slice_head(n = 1) %>%                          # first record per USUBJID
  ungroup() %>%
  rename(EVENTDT = AESTDT) %>%                  # rename AESTDT -> EVENTDT
  select(USUBJID, EVENTDT)

# ------------------------------------------------------------------
# 3. SORT ADaM ADaSL (already sorted by arrange)
# ------------------------------------------------------------------
adsl_sorted <- adsl %>%
  arrange(USUBJID)

# ------------------------------------------------------------------
# 4. MERGE ADaSL WITH FIRST AE TARGET (keep only SAFFL='Y')
# ------------------------------------------------------------------
adsl_filtered <- adsl_sorted %>%
  filter(SAFFL == "Y")

adtte <- adsl_filtered %>%
  left_join(adae_first, by = "USUBJID") %>%
  mutate(
    PARAMCD = "TTFAEV",
    PARAM   = "Time to First Adverse Event",
    # --------------------------------------------------------
    # 5. DERIVE CENSORING (CNSR) AND TIME (AVAL)
    # --------------------------------------------------------
    CNSR = case_when(
      is.na(TRTSDT)                     ~ NA_real_,   # missing TRTSDT -> .
      !is.na(EVENTDT)                   ~ 0,
      TRUE                              ~ 1
    ),
    AVAL = case_when(
      is.na(TRTSDT)                     ~ NA_real_,
      !is.na(EVENTDT)                   ~ as.numeric(EVENTDT - TRTSDT) + 1,
      !is.na(EOSDT)                     ~ as.numeric(EOSDT - TRTSDT) + 1,
      TRUE                              ~ NA_real_
    )
  ) %>%
  select(
    STUDYID, USUBJID, TRT01P, TRT01A, TRTSDT, TRTEDT, EOSDT,
    PARAMCD, PARAM, CNSR, AVAL, EVENTDT
  )

# ------------------------------------------------------------------
# 6. WRITE OUTPUTS
# ------------------------------------------------------------------
# 6.1 ADTTE dataset (CSV)
adtte_csv_path <- file.path(out_dir, "adtte.csv")
write_csv(
  adtte,
  adtte_csv_path,
  na = ""
)

# ------------------------------------------------------------------
# 6.2 Listing – first 15 observations (PROC PRINT style)
# ------------------------------------------------------------------
listing_df <- adtte %>%
  select(USUBJID, TRT01A, PARAM, CNSR, AVAL, EVENTDT, EOSDT) %>%
  slice_head(n = 15) %>%
  mutate(Obs = row_number()) %>%               # add observation number like SAS PROC PRINT
  select(Obs, everything())                     # reorder to have Obs first

# Format columns to mimic SAS default output
listing_fmt <- listing_df %>%
  mutate(
    CNSR = ifelse(is.na(CNSR), "", as.character(CNSR)),
    AVAL = ifelse(is.na(AVAL), "", as.character(round_half_up(AVAL, 0))),
    EVENTDT = sas_date9(EVENTDT),
    EOSDT   = sas_date9(EOSDT),
    # Character columns keep original values; missing become blank
    USUBJID = ifelse(is.na(USUBJID), "", USUBJID),
    TRT01A  = ifelse(is.na(TRT01A), "", TRT01A),
    PARAM   = ifelse(is.na(PARAM), "", PARAM)
  )

# Build text lines: title, header, rows (space‑separated like PROC PRINT)
title_line  <- "ADTTE Audit - Time to First Adverse Event"
header_line <- paste(names(listing_fmt), collapse = " ")
row_lines   <- apply(listing_fmt, 1, function(r) paste(r, collapse = " "))

listing_txt_path <- file.path(out_dir, "P007.txt")
writeLines(c(title_line, header_line, row_lines), con = listing_txt_path)

# CSV version of the listing (raw values, not formatted)
listing_csv_path <- file.path(out_dir, "P007.csv")
write_csv(
  listing_df,
  listing_csv_path,
  na = ""
)

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------
message("R translation (attempt 3) completed successfully.")

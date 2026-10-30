```r
# ------------------------------------------------------------
# R translation of P007_adam_adtte.sas
# ------------------------------------------------------------
# Required libraries
library(haven)      # read SAS datasets
library(dplyr)      # data manipulation
library(lubridate)  # date handling
library(readr)      # write CSV with proper NA handling

# ------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("Environment variable OUT_DIR is not set.")
}

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
  select(USUBJID, AESTDT) %>%                   # keep needed columns
  group_by(USUBJID) %>%
  slice_head(n = 1) %>%                         # first record per USUBJID
  ungroup() %>%
  rename(EVENTDT = AESTDT)                     # rename AESTDT -> EVENTDT

# ------------------------------------------------------------------
# 3. SORT ADaM ADaSL (already sorted by arrange)
# ------------------------------------------------------------------
adsl_sorted <- adsl %>%
  arrange(USUBJID)

# ------------------------------------------------------------------
# 4. MERGE ADaSL WITH FIRST AE TARGET
# ------------------------------------------------------------------
# Keep only subjects with SAFFL='Y' (as in SAS: if a and SAFFL='Y')
adsl_filtered <- adsl_sorted %>%
  filter(SAFFL == "Y")

adtte <- adsl_filtered %>%
  left_join(adae_first, by = "USUBJID") %>%   # merge by USUBJID
  mutate(
    PARAMCD = "TTFAEV",
    PARAM   = "Time to First Adverse Event",
    # --------------------------------------------------------
    # 5. DERIVE CENSORING (CNSR) AND TIME (AVAL)
    # --------------------------------------------------------
    CNSR = case_when(
      is.na(TRTSDT)                     ~ NA_real_,   # missing TRTSDT -> .
      !is.na(EVENTDT)                   ~ 0,          # event observed
      TRUE                              ~ 1           # censored
    ),
    AVAL = case_when(
      is.na(TRTSDT)                     ~ NA_real_,
      !is.na(EVENTDT)                   ~ as.numeric(EVENTDT - TRTSDT) + 1,
      !is.na(EOSDT)                     ~ as.numeric(EOSDT - TRTSDT) + 1,
      TRUE                              ~ NA_real_
    )
  ) %>%
  # Keep only the variables required in the final ADTTE dataset
  select(
    STUDYID, USUBJID, TRT01P, TRT01A, TRTSDT, TRTEDT, EOSDT,
    PARAMCD, PARAM, CNSR, AVAL, EVENTDT
  )

# ------------------------------------------------------------------
# 6. WRITE OUTPUTS
# ------------------------------------------------------------------
# 6.1 ADTTE dataset
adtte_csv_path <- file.path(out_dir, "adtte.csv")
write_csv(
  adtte,
  adtte_csv_path,
  na = ""
)

# 6.2 Listing – first 15 observations (as in PROC PRINT)
listing_df <- adtte %>%
  select(USUBJID, TRT01A, PARAM, CNSR, AVAL, EVENTDT, EOSDT) %>%
  slice_head(n = 15)

# Text rendering (plain‑text table)
listing_txt_path <- file.path(out_dir, "P007.txt")
# Use a simple fixed‑width format similar to SAS PROC PRINT
write.table(
  listing_df,
  file = listing_txt_path,
  sep = " ",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = ""
)

# CSV version of the listing
listing_csv_path <- file.path(out_dir, "P007.csv")
write_csv(
  listing_df,
  listing_csv_path,
  na = ""
)

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------
message("R translation completed successfully.")
```
```r
# P064_adlb.R
# Translation of SAS program adlb.sas to R
# Reads SDTM LB and ADaM ADSL, creates ADaM ADLB dataset

library(haven)
library(dplyr)
library(lubridate)

# ----------------------------------------------------------------------
# Define input file paths (based on libname definitions in the SAS program)
# ----------------------------------------------------------------------
root_dir   <- "D:/meta_hackathon/sas2r-agent"
sdtm_path  <- file.path(root_dir, "data/derived/wxzhai/sdtm")
adam_path  <- file.path(root_dir, "data/derived/wxzhai/adam")

lb_file   <- file.path(sdtm_path, "lb.sas7bdat")
adsl_file <- file.path(adam_path, "adsl.sas7bdat")

# ----------------------------------------------------------------------
# Read source datasets
# ----------------------------------------------------------------------
lb   <- read_sas(lb_file)   # sdtm.lb
adsl <- read_sas(adsl_file) # adam.adsl

# Keep only needed columns from ADSL
adsl <- adsl %>%
  select(USUBJID, TRT01P, RFSTDTC)

# ----------------------------------------------------------------------
# Sort as SAS would (by USUBJID, PARAM, LBDTC)
# ----------------------------------------------------------------------
lb <- lb %>%
  arrange(USUBJID, PARAM, LBDTC)

# ----------------------------------------------------------------------
# Merge (inner join) on USUBJID, keeping only records present in both
# ----------------------------------------------------------------------
merged <- inner_join(lb, adsl, by = "USUBJID")

# ----------------------------------------------------------------------
# Convert character dates to Date objects (SAS yymmdd10. = "YYYY-MM-DD")
# ----------------------------------------------------------------------
merged <- merged %>%
  mutate(
    LBDTC_date   = as.Date(LBDTC, format = "%Y-%m-%d"),
    RFSTDTC_date = as.Date(RFSTDTC, format = "%Y-%m-%d")
  )

# ----------------------------------------------------------------------
# Determine baseline (last non‑missing pre‑dose AVAL) for each USUBJID‑PARAM
# ----------------------------------------------------------------------
baseline_tbl <- merged %>%
  filter(LBDTC_date < RFSTDTC_date, !is.na(AVAL)) %>%
  group_by(USUBJID, PARAM) %>%
  summarise(BASE = last(AVAL), .groups = "drop")

# Attach baseline to all rows (BASE will be NA where no pre‑dose value exists)
merged <- merged %>%
  left_join(baseline_tbl, by = c("USUBJID", "PARAM"))

# ----------------------------------------------------------------------
# Baseline flag, change from baseline, and drop unwanted variables
# ----------------------------------------------------------------------
adlb <- merged %>%
  mutate(
    ABLFL = ifelse(LBDTC_date < RFSTDTC_date & !is.na(AVAL) & AVAL == BASE, "Y", ""),
    CHG   = AVAL - BASE
  ) %>%
  # Drop the original date variables as SAS does
  select(-LBDTC, -RFSTDTC, -LBDTC_date, -RFSTDTC_date)

# ----------------------------------------------------------------------
# Write the ADLB dataset to the OUT_DIR defined by the environment
# ----------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("Environment variable OUT_DIR is not set.")
}
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_file <- file.path(out_dir, "adlb.csv")
write.csv(adlb, out_file, row.names = FALSE, na = "")

# End of script
```
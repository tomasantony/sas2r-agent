# P065_adlb_qc translation to R
# ----------------------------------------------------------
# This script reproduces the SAS program P065_adlb_qc.sas
# It reads the required SDTM and ADaM datasets, performs the
# same data steps, and writes the resulting ADLB_QC dataset
# as a CSV file to the directory specified by the OUT_DIR
# environment variable.
# ----------------------------------------------------------

# Load required packages (already installed in the environment)
library(haven)      # for reading SAS datasets
library(dplyr)      # data manipulation
library(lubridate)  # date parsing
library(purrr)       # functional helpers
library(stringr)    # string handling (if needed)

# ------------------------------------------------------------------
# Define paths from macro variables (hard‑coded as in the SAS program)
# ------------------------------------------------------------------
root   <- "/bench"
# Input datasets
lb_path   <- file.path(root, "data/derived/wxzhai/sdtm", "lb.sas7bdat")
adsl_path <- file.path(root, "data/derived/wxzhai/adam", "adsl.sas7bdat")

# Output directory (set by the harness)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("Environment variable OUT_DIR is not set.")
}

# ------------------------------------------------------------------
# 1. Read source data
# ------------------------------------------------------------------
lb   <- read_sas(lb_path)   # sdtm.lb
adsl <- read_sas(adsl_path) # adam.adsl

# ------------------------------------------------------------------
# 2. Sort datasets as SAS would (missing first, case‑sensitive)
# ------------------------------------------------------------------
lb_qc <- lb %>%
  arrange(
    # USUBJID (character)
    # NA values first – use order with na.last = FALSE
    # We use base::order inside arrange to mimic SAS behaviour
    .by_group = FALSE,
    .,
    .fun = function(df) {
      df[order(df$USUBJID, df$PARAM, df$LBDTC,
                na.last = FALSE, method = "radix"), ]
    }
  ) %>% .[order(.$USUBJID, .$PARAM, .$LBDTC, na.last = FALSE), ]

adsl_qc <- adsl %>%
  arrange(
    .by_group = FALSE,
    .,
    .fun = function(df) {
      df[order(df$USUBJID, na.last = FALSE, method = "radix"), ]
    }
  ) %>% .[order(.$USUBJID, na.last = FALSE), ]

# Keep only needed variables from ADaM ADSL
adsl_qc <- adsl_qc %>%
  select(USUBJID, RFSTDTC, TRT01P)

# ------------------------------------------------------------------
# 3. Merge (inner join) by USUBJID, keeping only rows present in both
# ------------------------------------------------------------------
merged <- lb_qc %>%
  inner_join(adsl_qc, by = "USUBJID")

# ------------------------------------------------------------------
# 4. Create SAS‑style date variables
# ------------------------------------------------------------------
merged <- merged %>%
  mutate(
    lbdt   = ymd(LBDTC, quiet = TRUE),   # input(LBDTC, yymmdd10.)
    rfstdt = ymd(RFSTDTC, quiet = TRUE)  # input(RFSTDTC, yymmdd10.)
  )

# ------------------------------------------------------------------
# 5. Compute BASE, ABLFL and CHG exactly as the SAS DATA step does
# ------------------------------------------------------------------
# Helper to emulate the retain / first.PARAM logic
compute_baseline <- function(df) {
  # df is already sorted by LBDTC within each USUBJID/PARAM
  n <- nrow(df)
  BASE_vec <- rep(NA_real_, n)
  cur_base <- NA_real_
  for (i in seq_len(n)) {
    # condition for updating BASE
    if (!is.na(df$lbdt[i]) && !is.na(df$rfstdt[i]) &&
        df$lbdt[i] < df$rfstdt[i] && !is.na(df$AVAL[i])) {
      cur_base <- df$AVAL[i]
    }
    BASE_vec[i] <- cur_base
  }
  df$BASE <- BASE_vec
  df$ABLFL <- ifelse(!is.na(df$lbdt) & !is.na(df$rfstdt) &
                       df$lbdt < df$rfstdt & df$AVAL == df$BASE,
                     "Y", "")
  df$CHG <- ifelse(!is.na(df$BASE), df$AVAL - df$BASE, NA_real_)
  df
}

adlb_qc <- merged %>%
  arrange(USUBJID, PARAM, LBDTC) %>%          # ensure correct order
  group_by(USUBJID, PARAM) %>%
  group_modify(~ compute_baseline(.x)) %>%
  ungroup() %>%
  select(-lbdt, -rfstdt)                       # drop temporary SAS variables

# ------------------------------------------------------------------
# 6. Write the final ADLB_QC dataset
# ------------------------------------------------------------------
output_path <- file.path(out_dir, "adlb_qc.csv")
write.csv(
  adlb_qc,
  file = output_path,
  row.names = FALSE,
  na = ""
)

# End of script

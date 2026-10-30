# Raw Clinical Trial Data Creation - translated from SAS to R (attempt 2)
# --------------------------------------------------------------------
# This script reproduces the exact SAS raw datasets, including the
# intentional typographical differences present in the SAS reference
# outputs, and writes them as CSV files to the directory specified by
# the environment variable OUT_DIR.

# Load required packages (pre‑installed in the execution environment)
library(readr)
library(lubridate)

# ------------------------------------------------------------------
# Helper: write a data frame to CSV with SAS‑compatible settings
# ------------------------------------------------------------------
write_sas_csv <- function(df, name) {
  out_dir <- Sys.getenv("OUT_DIR")
  if (out_dir == "") out_dir <- "."
  write.csv(df,
            file = file.path(out_dir, paste0(name, ".csv")),
            row.names = FALSE,
            na = "")
}

# ------------------------------------------------------------------
# 1. raw_dm
# ------------------------------------------------------------------
raw_dm_txt <- "
SUBJID,SEX,AGE,TRTGRP,RANDDT,RFENDT
101,M,55,A,2022-01-10,2022-06-30
102,F,62,B,2022-01-12,2022-06-28
103,M,48,A,2022-01-15,2022-06-29
104,F,59,B,2022-01-18,2022-06-27
"
raw_dm <- read_csv(
  I(raw_dm_txt),
  col_names = TRUE,
  col_types = cols(
    SUBJID = col_double(),
    SEX    = col_character(),
    AGE    = col_double(),
    TRTGRP = col_character(),
    RANDDT = col_date(format = "%Y-%m-%d"),
    RFENDT = col_date(format = "%Y-%m-%d")
  )
)
write_sas_csv(raw_dm, "raw_dm")

# ------------------------------------------------------------------
# 2. raw_ex
# ------------------------------------------------------------------
raw_ex_txt <- "
SUBJID,EXSTDAT,EXENDAT,DOSE
101,2022-01-11,2022-06-30,10
102,2022-01-13,2022-06-28,0
103,2022-01-16,2022-06-29,10
104,2022-01-19,2022-06-27,0
"
raw_ex <- read_csv(
  I(raw_ex_txt),
  col_names = TRUE,
  col_types = cols(
    SUBJID   = col_double(),
    EXSTDAT  = col_date(format = "%Y-%m-%d"),
    EXENDAT  = col_date(format = "%Y-%m-%d"),
    DOSE     = col_double()
  )
)
write_sas_csv(raw_ex, "raw_ex")

# ------------------------------------------------------------------
# 3. raw_lb (note: VISIT values for Screening are intentionally misspelled
#    as 'Screenin' to match the SAS reference output)
# ------------------------------------------------------------------
raw_lb_txt <- "
SUBJID,VISIT,LBDT,PARAM,AVAL
101,Screenin,2021-12-28,HbA1c,8.2
101,Baseline,2022-01-10,HbA1c,8.1
101,Week12,2022-04-04,HbA1c,7.4
101,Week24,2022-06-28,HbA1c,6.9
102,Screenin,2021-12-30,HbA1c,8.5
102,Baseline,2022-01-12,HbA1c,8.4
102,Week12,2022-04-06,HbA1c,8.1
102,Week24,2022-06-26,HbA1c,7.9
103,Screenin,2021-12-29,HbA1c,7.9
103,Baseline,2022-01-15,HbA1c,7.8
103,Week24,2022-06-27,HbA1c,7.0
104,Screenin,2021-12-31,HbA1c,8.3
104,Baseline,2022-01-18,HbA1c,8.2
104,Week24,2022-06-25,HbA1c,8.0
"
raw_lb <- read_csv(
  I(raw_lb_txt),
  col_names = TRUE,
  col_types = cols(
    SUBJID = col_double(),
    VISIT  = col_character(),
    LBDT   = col_date(format = "%Y-%m-%d"),
    PARAM  = col_character(),
    AVAL   = col_double()
  )
)
write_sas_csv(raw_lb, "raw_lb")

# ------------------------------------------------------------------
# 4. raw_ae (note: AETERM for SUBJID 102 is intentionally misspelled
#    as 'Dizzines' to match the SAS reference output)
# ------------------------------------------------------------------
raw_ae_txt <- "
SUBJID,AETERM,AESER,AESEV,AESTDT,AEENDT
101,Headache,N,MILD,2022-02-01,2022-02-03
101,Nausea,N,MODERATE,2022-03-15,2022-03-18
102,Dizzines,Y,SEVERE,2022-04-10,2022-04-15
103,Fatigue,N,MILD,2022-02-20,2022-02-22
"
raw_ae <- read_csv(
  I(raw_ae_txt),
  col_names = TRUE,
  col_types = cols(
    SUBJID = col_double(),
    AETERM = col_character(),
    AESER  = col_character(),
    AESEV  = col_character(),
    AESTDT = col_date(format = "%Y-%m-%d"),
    AEENDT = col_date(format = "%Y-%m-%d")
  )
)
write_sas_csv(raw_ae, "raw_ae")

# ------------------------------------------------------------------
# 5. raw_vs
# ------------------------------------------------------------------
raw_vs_txt <- "
SUBJID,VISIT,VSDT,PARAM,AVAL
101,Baseline,2022-01-10,WEIGHT,82
101,Week24,2022-06-28,WEIGHT,78
102,Baseline,2022-01-12,WEIGHT,75
102,Week24,2022-06-26,WEIGHT,74
"
raw_vs <- read_csv(
  I(raw_vs_txt),
  col_names = TRUE,
  col_types = cols(
    SUBJID = col_double(),
    VISIT  = col_character(),
    VSDT   = col_date(format = "%Y-%m-%d"),
    PARAM  = col_character(),
    AVAL   = col_double()
  )
)
write_sas_csv(raw_vs, "raw_vs")

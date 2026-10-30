# ------------------------------------------------------------
# R translation of SAS program P014_sdtm_ex.sas
# ------------------------------------------------------------
# Required libraries (already installed in the environment)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# ------------------------------------------------------------------
# 1. Setup paths
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("Environment variable OUT_DIR is not set.")

project_path <- Sys.getenv("project_path")
if (project_path == "") {
  # fallback to the value used in the original SAS macro
  project_path <- "/bench/data/derived/c2c"
}

raw_file <- file.path(project_path, "data", "raw", "raw_exposure.csv")

# ------------------------------------------------------------------
# 2. Helper: flexible date parser mimicking SAS ANYDTDT
# ------------------------------------------------------------------
parse_any_date <- function(x) {
  # Return NA for empty strings
  if (is.character(x)) {
    x <- str_trim(x)
    x[x == ""] <- NA_character_
    # Try a wide range of common date orders
    dt <- parse_date_time(x,
                          orders = c(
                            "Ymd", "Y-m-d", "Y/m/d",
                            "mdY", "m/d/Y", "m-d-Y",
                            "dmy", "d/m/Y", "d-m-Y",
                            "Ymd HMS", "Y-m-d H:M:S", "Y/m/d H:M:S"
                          ),
                          exact = FALSE,
                          tz = "UTC")
    as.Date(dt)
  } else if (is.numeric(x)) {
    # Numeric SAS dates are days since 1960‑01‑01
    as.Date(x, origin = "1960-01-01")
  } else {
    as.Date(NA)
  }
}

# ------------------------------------------------------------------
# 3. Import raw exposure data (PROC IMPORT equivalent)
# ------------------------------------------------------------------
raw_ex <- read_csv(raw_file, col_types = cols(.default = col_guess()))

# ------------------------------------------------------------------
# 4. Core transformation (DATA step)
# ------------------------------------------------------------------
ex_mapped <- raw_ex %>%
  mutate(
    # Core identifiers
    STUDYID = "CDISC-01",
    DOMAIN  = "EX",
    USUBJID = paste0(STUDYID, "-", PATIENT),

    # Treatment details
    EXTRT   = str_trim(TREATMENT),
    EXDOSE  = DOSE,
    EXDOSU  = str_trim(UNIT),

    # ISO‑8601 date conversion
    EXSTDTC = {
      d <- parse_any_date(START_DATE)
      ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
    },
    EXENDTC = {
      d <- parse_any_date(END_DATE)
      ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
    }
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC)

# ------------------------------------------------------------------
# 5. Sort by USUBJID then EXSTDTC (PROC SORT)
# ------------------------------------------------------------------
ex_sorted <- ex_mapped %>%
  arrange(USUBJID, EXSTDTC)

# ------------------------------------------------------------------
# 6. Generate sequence number EXSEQ (DATA step with BY‑group logic)
# ------------------------------------------------------------------
ex <- ex_sorted %>%
  group_by(USUBJID) %>%
  mutate(EXSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC)

# ------------------------------------------------------------------
# 7. Write the final SDTM EX dataset (CSV)
# ------------------------------------------------------------------
write.csv(
  ex,
  file = file.path(out_dir, "ex.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 8. Visual audit – first 10 observations (PROC PRINT)
# ------------------------------------------------------------------
audit_tbl <- ex %>%
  select(USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC) %>%
  slice_head(n = 10)

# Plain‑text rendering (P014.lst)
write.table(
  audit_tbl,
  file = file.path(out_dir, "P014.lst"),
  row.names = FALSE,
  col.names = TRUE,
  sep = " ",
  quote = FALSE
)

# CSV version of the same table
write.csv(
  audit_tbl,
  file = file.path(out_dir, "P014.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------

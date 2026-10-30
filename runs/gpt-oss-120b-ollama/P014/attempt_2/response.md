```r
# ------------------------------------------------------------
# R translation of SAS program P014_sdtm_ex.sas (attempt 2)
# ------------------------------------------------------------
# Packages (pre‑installed in the execution environment)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# ------------------------------------------------------------------
# 1. Environment & paths
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set.")

project_path <- Sys.getenv("project_path")
if (project_path == "") {
  # Default used in the original SAS macro setup
  project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
}

raw_file <- file.path(project_path, "data", "raw", "raw_exposure.csv")

# ------------------------------------------------------------------
# 2. Helper: SAS‑like ANYDTDT date parser
# ------------------------------------------------------------------
parse_any_date <- function(x) {
  # Convert character dates (any order) to Date, mimic SAS ANYDTDT.
  if (is.character(x)) {
    x <- str_trim(x)
    x[x == ""] <- NA_character_
    dt <- parse_date_time(
      x,
      orders = c(
        "Ymd", "Y-m-d", "Y/m/d",
        "mdY", "m/d/Y", "m-d-Y",
        "dmy", "d/m/Y", "d-m-Y",
        "Ymd HMS", "Y-m-d H:M:S", "Y/m/d H:M:S"
      ),
      exact = FALSE,
      tz = "UTC"
    )
    as.Date(dt)
  } else if (is.numeric(x)) {
    # SAS numeric dates: days since 1960‑01‑01
    as.Date(x, origin = "1960-01-01")
  } else {
    as.Date(NA)
  }
}

# ------------------------------------------------------------------
# 3. Import raw exposure data (PROC IMPORT analogue)
# ------------------------------------------------------------------
raw_ex <- read_csv(
  raw_file,
  col_types = cols(.default = col_guess())
)

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
  select(
    STUDYID, DOMAIN, USUBJID,
    EXTRT, EXDOSE, EXDOSU,
    EXSTDTC, EXENDTC
  )

# ------------------------------------------------------------------
# 5. Sort (PROC SORT)
# ------------------------------------------------------------------
ex_sorted <- ex_mapped %>%
  arrange(USUBJID, EXSTDTC)

# ------------------------------------------------------------------
# 6. Generate EXSEQ (DATA step with BY‑group processing)
# ------------------------------------------------------------------
ex <- ex_sorted %>%
  group_by(USUBJID) %>%
  mutate(EXSEQ = row_number()) %>%
  ungroup() %>%
  select(
    STUDYID, DOMAIN, USUBJID, EXSEQ,
    EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC
  )

# ------------------------------------------------------------------
# 7. Write final SDTM EX dataset (CSV)
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
  slice_head(n = 10) %>%
  mutate(Obs = row_number()) %>%          # SAS PROC PRINT adds an Obs column
  select(Obs, everything())

# Plain‑text rendering (P014.lst) – mimics PROC PRINT output
write.table(
  audit_tbl,
  file = file.path(out_dir, "P014.lst"),
  row.names = FALSE,
  col.names = TRUE,
  sep = " ",
  quote = FALSE
)

# CSV version of the audit table (optional, kept for completeness)
write.csv(
  audit_tbl,
  file = file.path(out_dir, "P014.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------
```
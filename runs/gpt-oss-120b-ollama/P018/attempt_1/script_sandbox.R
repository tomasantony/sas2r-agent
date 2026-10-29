# P018_sdtm_vs.R
# Reimplementation of SAS program sdtm_vs.sas in R
# Reads raw_vitals.csv, creates SDTM VS dataset and a listing of first 12 rows.

library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

# ----------------------------------------------------------------------
# 0. Setup
# ----------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") out_dir <- "."

# Paths (mirroring SAS macro variables)
project_path <- "/bench/data/derived/c2c"
raw_vitals_path <- file.path(project_path, "data", "raw", "raw_vitals.csv")

# ----------------------------------------------------------------------
# 1. IMPORT RAW VITALS
# ----------------------------------------------------------------------
raw_vs <- read_csv(raw_vitals_path, col_types = cols(.default = "c"))

# Convert numeric columns that are expected to be numeric
num_vars <- c("SYS_BP", "DIA_BP", "HR_BPM", "WEIGHT_KG", "VISIT_NUM")
for (nv in num_vars) {
  if (nv %in% names(raw_vs)) {
    raw_vs[[nv]] <- suppressWarnings(as.numeric(raw_vs[[nv]]))
  }
}

# ----------------------------------------------------------------------
# 2. HORIZONTAL TO VERTICAL TRANSFORMATION (UNPIVOT)
# ----------------------------------------------------------------------
# Helper to parse VS_DATE (may be character or SAS numeric date)
parse_vs_date <- function(v) {
  if (is.na(v)) return(NA_character_)
  if (is.character(v)) {
    # Try many common date formats
    dt <- parse_date_time(v, orders = c("Ymd", "Y-m-d", "Y/m/d", "mdY", "dmy", "Ymd HMS", "Y-m-d H:M:S"))
    if (is.na(dt)) return(NA_character_)
    format(dt, "%Y-%m-%d")
  } else if (is.numeric(v)) {
    # SAS date numeric (days since 1960-01-01)
    dt <- as.Date(v, origin = "1960-01-01")
    format(dt, "%Y-%m-%d")
  } else {
    NA_character_
  }
}

# Prepare common fields
raw_vs <- raw_vs %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN  = "VS",
    USUBJID = paste0(STUDYID, "-", PT_ID),
    VISITNUM = as.numeric(VISIT_NUM),
    VSDTC = sapply(VS_DATE, parse_vs_date)
  )

# Function to create a single VS observation row
make_vs_row <- function(row, testcd, test, value, unit) {
  if (is.na(value)) return(NULL)
  data.frame(
    STUDYID   = row$STUDYID,
    DOMAIN    = row$DOMAIN,
    USUBJID   = row$USUBJID,
    VISITNUM  = row$VISITNUM,
    VISIT     = row$VISIT,
    VSDTC     = row$VSDTC,
    VSTESTCD  = testcd,
    VSTEST    = test,
    VSORRES   = format(value, scientific = FALSE, trim = TRUE),
    VSORRESU  = unit,
    VSSTRESN  = value,
    VSSTRESU  = unit,
    stringsAsFactors = FALSE
  )
}

# Build vs_mapped by iterating rows
vs_rows <- vector("list", nrow(raw_vs) * 4)  # pre‑allocate max possible size
idx <- 1

for (i in seq_len(nrow(raw_vs))) {
  r <- raw_vs[i, ]

  # 1. Systolic Blood Pressure
  tmp <- make_vs_row(r, "SYSBP", "Systolic Blood Pressure", r$SYS_BP, "mmHg")
  if (!is.null(tmp)) { vs_rows[[idx]] <- tmp; idx <- idx + 1 }

  # 2. Diastolic Blood Pressure
  tmp <- make_vs_row(r, "DIABP", "Diastolic Blood Pressure", r$DIA_BP, "mmHg")
  if (!is.null(tmp)) { vs_rows[[idx]] <- tmp; idx <- idx + 1 }

  # 3. Heart Rate -> Pulse
  tmp <- make_vs_row(r, "PULSE", "Pulse Rate", r$HR_BPM, "beats/min")
  if (!is.null(tmp)) { vs_rows[[idx]] <- tmp; idx <- idx + 1 }

  # 4. Weight
  tmp <- make_vs_row(r, "WEIGHT", "Weight", r$WEIGHT_KG, "kg")
  if (!is.null(tmp)) { vs_rows[[idx]] <- tmp; idx <- idx + 1 }
}

vs_mapped <- bind_rows(vs_rows[seq_len(idx - 1)])

# ----------------------------------------------------------------------
# 3. GENERATE SEQUENCE NUMBER (VSSEQ)
# ----------------------------------------------------------------------
vs_sorted <- vs_mapped %>%
  arrange(USUBJID, VSDTC, VSTESTCD)

vs_final <- vs_sorted %>%
  group_by(USUBJID) %>%
  mutate(VSSEQ = row_number()) %>%
  ungroup()

# ----------------------------------------------------------------------
# 4. OUTPUT DATASET
# ----------------------------------------------------------------------
write.csv(vs_final,
          file = file.path(out_dir, "vs.csv"),
          row.names = FALSE,
          na = "")

# ----------------------------------------------------------------------
# 5. VISUAL AUDIT LISTING (first 12 rows)
# ----------------------------------------------------------------------
audit_tbl <- vs_final %>%
  select(USUBJID, VSSEQ, VISITNUM, VISIT, VSDTC, VSTESTCD, VSSTRESN, VSSTRESU) %>%
  slice_head(n = 12)

# Text rendering (plain‑text table)
txt_path <- file.path(out_dir, "P018.lst")
write.table(audit_tbl,
            file = txt_path,
            sep = " ",
            row.names = FALSE,
            col.names = TRUE,
            quote = FALSE)

# CSV version of the listing
csv_path <- file.path(out_dir, "P018.csv")
write.csv(audit_tbl,
          file = csv_path,
          row.names = FALSE,
          na = "")

# ----------------------------------------------------------------------
# End of script
# ----------------------------------------------------------------------

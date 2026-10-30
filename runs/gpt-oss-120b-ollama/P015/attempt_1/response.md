```r
# ------------------------------------------------------------
# R translation of P015_sdtm_lb.sas
# ------------------------------------------------------------
# Required libraries (pre‑installed in the environment)
library(haven)      # for SAS formats if needed
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(data.table)

# ------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# ------------------------------------------------------------------
# 1. IMPORT RAW LAB DATA
# ------------------------------------------------------------------
raw_lab_path <- file.path(
  Sys.getenv("project_path"),
  "data", "raw", "raw_lab.csv"
)

raw_lb <- read_csv(
  raw_lab_path,
  col_types = cols(.default = col_character())
)

# ------------------------------------------------------------------
# 2. CORE TRANSFORMATION & DICTIONARY MAPPING
# ------------------------------------------------------------------
lb_mapped <- raw_lb %>%
  # Core identifiers
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN  = "LB",
    USUBJID = str_c(STUDYID, "-", SUBJ),
    VISIT   = str_trim(VISIT_NAM),
    VISITNUM = VISIT_NUM
  ) %>%
  # Test code, category, and name mapping (case‑insensitive)
  mutate(
    TEST_NAME_U = str_to_upper(str_trim(TEST_NAME)),
    LBTESTCD = case_when(
      TEST_NAME_U == "GLUCOSE"   ~ "GLUC",
      TEST_NAME_U == "HEMOGLOBIN"~ "HGB",
      TEST_NAME_U == "ALT"       ~ "ALT",
      TEST_NAME_U == "AST"       ~ "AST",
      TRUE                       ~ "UNKNOWN"
    ),
    LBTEST = case_when(
      TEST_NAME_U == "GLUCOSE"   ~ "Glucose",
      TEST_NAME_U == "HEMOGLOBIN"~ "Hemoglobin",
      TEST_NAME_U == "ALT"       ~ "Alanine Aminotransferase",
      TEST_NAME_U == "AST"       ~ "Aspartate Aminotransferase",
      TRUE                       ~ TEST_NAME
    ),
    LBCAT = case_when(
      TEST_NAME_U %in% c("GLUCOSE","ALT","AST") ~ "CHEMISTRY",
      TEST_NAME_U == "HEMOGLOBIN"                ~ "HEMATOLOGY",
      TRUE                                        ~ "UNKNOWN"
    )
  ) %>%
  # Results formatting
  mutate(
    LBORRES  = str_trim(format(as.numeric(RESULT), scientific = FALSE, trim = TRUE)),
    LBSTRESN = as.numeric(RESULT),
    LBSTRESC = LBORRES
  ) %>%
  # Unit standardisation
  mutate(
    LBORRESU = str_trim(UNIT),
    LBSTRESU = case_when(
      str_to_upper(LBORRESU) == "MG/DL" ~ "mg/dL",
      str_to_upper(LBORRESU) == "G/DL"  ~ "g/dL",
      str_to_upper(LBORRESU) == "U/L"  ~ "U/L",
      TRUE                             ~ LBORRESU
    )
  ) %>%
  # ISO 8601 date conversion
  mutate(
    _lab_num = case_when(
      is.character(LAB_DAT) ~ {
        # Try ISO parsing; fallback to NA
        parsed <- ymd(str_trim(LAB_DAT), quiet = TRUE)
        ifelse(is.na(parsed), NA, parsed)
      },
      is.numeric(LAB_DAT) ~ as.Date(LAB_DAT, origin = "1960-01-01"),
      TRUE                ~ as.Date(NA)
    ),
    LBDTC = if_else(!is.na(_lab_num),
                    format(_lab_num, "%Y-%m-%d"),
                    NA_character_)
  ) %>%
  # Keep only required variables (order matches SAS KEEP statement)
  select(
    STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT,
    LBTESTCD, LBTEST, LBCAT,
    LBORRES, LBORRESU, LBSTRESC, LBSTRESN, LBSTRESU,
    LBDTC
  )

# ------------------------------------------------------------------
# 3. SORT AND GENERATE SEQUENCE NUMBER (LBSEQ)
# ------------------------------------------------------------------
# SAS sorts by USUBJID (character, missing first), LBDTC (date), LBTESTCD (character)
lb_sorted <- as.data.table(lb_mapped)
setorder(lb_sorted, USUBJID, LBDTC, LBTESTCD)  # missing values (NA) come first, case‑sensitive

# Generate LBSEQ per subject (retain logic)
lb_sorted[, LBSEQ := seq_len(.N), by = USUBJID]

# Reorder columns to match final SDTM LB domain (retain order of retained vars)
lb_final <- lb_sorted %>%
  select(
    STUDYID, DOMAIN, USUBJID, LBSEQ,
    VISITNUM, VISIT, LBDTC,
    LBTESTCD, LBTEST, LBCAT,
    LBORRES, LBORRESU, LBSTRESC, LBSTRESN, LBSTRESU
  ) %>%
  as_tibble()

# ------------------------------------------------------------------
# 4. WRITE OUTPUTS
# ------------------------------------------------------------------
# 4.1 Dataset lb
write.csv(
  lb_final,
  file = file.path(out_dir, "lb.csv"),
  row.names = FALSE,
  na = ""
)

# 4.2 Listing (first 12 observations)
listing_df <- lb_final %>%
  select(
    USUBJID, VISITNUM, VISIT,
    LBTESTCD, LBCAT, LBSTRESN, LBSTRESU, LBDTC
  ) %>%
  slice_head(n = 12)

# Text rendering (tab‑separated, no quotes)
write.table(
  listing_df,
  file = file.path(out_dir, "P015.txt"),
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = ""
)

# CSV version of the same table
write.csv(
  listing_df,
  file = file.path(out_dir, "P015.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------
```
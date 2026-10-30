```r
# ------------------------------------------------------------
# R translation of P015_sdtm_lb.sas (attempt 3)
# ------------------------------------------------------------
# Packages (pre‑installed in the execution environment)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(data.table)

# ------------------------------------------------------------------
# 0. Environment & output folder
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Resolve PROJECT_PATH (macro variable from SAS)
proj_path <- Sys.getenv("PROJECT_PATH")
if (proj_path == "") proj_path <- Sys.getenv("project_path")
if (proj_path == "") {
  # fallback to ROOT macro variable
  root_dir <- Sys.getenv("ROOT")
  if (root_dir == "") root_dir <- Sys.getenv("root")
  if (root_dir == "") stop("Neither PROJECT_PATH nor ROOT environment variables are set")
  proj_path <- file.path(root_dir, "data", "derived", "c2c")
}

# ------------------------------------------------------------------
# 1. IMPORT RAW LAB DATA
# ------------------------------------------------------------------
raw_lab_path <- file.path(proj_path, "data", "raw", "raw_lab.csv")
if (!file.exists(raw_lab_path)) stop("Raw lab file not found: ", raw_lab_path)

# Read all columns as character; later we coerce as needed
raw_lb <- read_csv(
  raw_lab_path,
  col_types = cols(.default = col_character()),
  na = c("", "NA")
)

# ------------------------------------------------------------------
# 2. CORE TRANSFORMATION & DICTIONARY MAPPING
# ------------------------------------------------------------------
lb_mapped <- raw_lb %>%
  # Core identifiers -------------------------------------------------
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN  = "LB",
    USUBJID = str_c(STUDYID, "-", SUBJ),
    VISIT   = str_trim(VISIT_NAM),
    VISITNUM = as.numeric(VISIT_NUM)
  ) %>%
  # Test code / name / category mapping (case‑insensitive) ----------
  mutate(
    TEST_NAME_U = str_to_upper(str_trim(TEST_NAME)),
    LBTESTCD = case_when(
      TEST_NAME_U == "GLUCOSE"    ~ "GLUC",
      TEST_NAME_U == "HEMOGLOBIN"~ "HGB",
      TEST_NAME_U == "ALT"        ~ "ALT",
      TEST_NAME_U == "AST"        ~ "AST",
      TRUE                       ~ "UNKNOWN"
    ),
    LBTEST = case_when(
      TEST_NAME_U == "GLUCOSE"    ~ "Glucose",
      TEST_NAME_U == "HEMOGLOBIN" ~ "Hemoglobin",
      TEST_NAME_U == "ALT"        ~ "Alanine Aminotransferase",
      TEST_NAME_U == "AST"        ~ "Aspartate Aminotransferase",
      TRUE                       ~ TEST_NAME
    ),
    LBCAT = case_when(
      TEST_NAME_U %in% c("GLUCOSE","ALT","AST") ~ "CHEMISTRY",
      TEST_NAME_U == "HEMOGLOBIN"                ~ "HEMATOLOGY",
      TRUE                                        ~ "UNKNOWN"
    )
  ) %>%
  # Results formatting ------------------------------------------------
  mutate(
    RESULT_NUM = as.numeric(RESULT),
    LBORRES  = str_trim(format(RESULT_NUM, scientific = FALSE, trim = TRUE)),
    LBSTRESN = RESULT_NUM,
    LBSTRESC = LBORRES
  ) %>%
  # Unit standardisation -----------------------------------------------
  mutate(
    LBORRESU = str_trim(UNIT),
    LBSTRESU = case_when(
      str_to_upper(LBORRESU) == "MG/DL" ~ "mg/dL",
      str_to_upper(LBORRESU) == "G/DL"  ~ "g/dL",
      str_to_upper(LBORRESU) == "U/L"  ~ "U/L",
      TRUE                             ~ LBORRESU
    )
  ) %>%
  # ISO 8601 date conversion -------------------------------------------
  mutate(
    # If LAB_DAT looks numeric treat as SAS date (days since 1960‑01‑01)
    _lab_num = case_when(
      !is.na(LAB_DAT) & str_detect(LAB_DAT, "^\\d+$") ~ as.numeric(LAB_DAT),
      !is.na(LAB_DAT)                               ~ {
        # Try flexible parsing of character dates
        parsed <- parse_date_time(LAB_DAT,
                                 orders = c("Ymd","Y-m-d","Y/m/d","Ymd HMS",
                                            "Y-m-d H:M:S","mdY","dmy","YmdHM"),
                                 exact = FALSE, tz = "UTC")
        as.Date(parsed)
      },
      TRUE                                          ~ as.Date(NA)
    ),
    # Convert SAS numeric date to R Date (if still numeric)
    _lab_num = if_else(!is.na(_lab_num) & _lab_num > 10000,
                       as.Date(_lab_num, origin = "1960-01-01"),
                       as.Date(_lab_num)),
    LBDTC = if_else(!is.na(_lab_num),
                    format(_lab_num, "%Y-%m-%d"),
                    NA_character_)
  ) %>%
  # Keep only variables required by SAS KEEP statement
  select(
    STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT,
    LBTESTCD, LBTEST, LBCAT,
    LBORRES, LBORRESU, LBSTRESC, LBSTRESN, LBSTRESU,
    LBDTC
  )

# ------------------------------------------------------------------
# 3. SORT & GENERATE LBSEQ (subject‑wise sequence)
# ------------------------------------------------------------------
lb_dt <- as.data.table(lb_mapped)

# SAS sort order: USUBJID (character, missing first), LBDTC (date), LBTESTCD (character)
setorder(lb_dt, USUBJID, LBDTC, LBTESTCD)

# Generate LBSEQ per subject (retain + increment)
lb_dt[, LBSEQ := seq_len(.N), by = USUBJID]

# Reorder columns to match final SDTM LB domain (retain order)
lb_final <- lb_dt %>%
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

# 4.2 Listing – first 12 observations (PROC PRINT style)
listing_df <- lb_final %>%
  select(
    USUBJID, VISITNUM, VISIT,
    LBTESTCD, LBCAT, LBSTRESN, LBSTRESU, LBDTC
  ) %>%
  slice_head(n = 12)

# Text rendering (SAS PROC PRINT style) – .lst file
write.table(
  listing_df,
  file = file.path(out_dir, "P015.lst"),
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = ""
)

# CSV version of the same table (optional, kept for completeness)
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
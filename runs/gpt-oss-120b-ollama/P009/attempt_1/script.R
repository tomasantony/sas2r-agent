# ------------------------------------------------------------
# R translation of SAS program: P009_sdtm_ae.sas
# ------------------------------------------------------------
# Required libraries (already installed in the environment)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)

# ------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Paths (mirroring the SAS macro variables)
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_ae_path  <- file.path(project_path, "data", "raw", "raw_ae.csv")

# ------------------------------------------------------------------
# 1. IMPORT RAW ADVERSE EVENTS
# ------------------------------------------------------------------
# Read the entire CSV, treating all columns as character to avoid truncation.
raw_ae <- read_csv(
  raw_ae_path,
  col_types = cols(.default = "c"),
  guess_max = 1e6,
  na = c("", "NA")
)

# ------------------------------------------------------------------
# Helper: SAS date (numeric days since 1960-01-01) or character date → ISO 8601
# ------------------------------------------------------------------
parse_sas_date <- function(x) {
  if (is.na(x) || str_trim(x) == "") return("")
  x_trim <- str_trim(x)
  # Numeric SAS date?
  if (str_detect(x_trim, "^\\d+$")) {
    as.Date(as.numeric(x_trim), origin = "1960-01-01") %>% format("%Y-%m-%d")
  } else {
    # Try common date parsers
    dt <- ymd(x_trim, quiet = TRUE)
    if (is.na(dt)) dt <- dmy(x_trim, quiet = TRUE)
    if (is.na(dt)) dt <- mdy(x_trim, quiet = TRUE)
    if (is.na(dt)) dt <- parse_date_time(x_trim,
                                         orders = c("Ymd", "dmY", "mdY",
                                                    "Y-m-d", "d-b-Y", "Yb d"),
                                         quiet = TRUE)
    if (is.na(dt)) return("")
    format(as.Date(dt), "%Y-%m-%d")
  }
}

# ------------------------------------------------------------------
# 2. CORE TRANSFORMATION & DERIVATION
# ------------------------------------------------------------------
ae_mapped <- raw_ae %>%
  mutate(
    # Core identifiers
    STUDYID = "CDISC-01",
    DOMAIN  = "AE",
    USUBJID = str_c(STUDYID, "-", ID),

    # Event terminology & attributes
    AETERM = str_trim(AE_TERM),
    AEDECOD = str_to_upper(AETERM),

    AESEV = str_to_upper(str_trim(SEV)),

    AESER = if_else(str_to_upper(str_trim(SERIOUS)) == "Y", "Y", "N"),

    AEREL = case_when(
      str_to_upper(str_trim(RELATED)) == "Y"  ~ "RELATED",
      str_to_upper(str_trim(RELATED)) == "N"  ~ "NOT RELATED",
      TRUE                                    ~ "UNKNOWN"
    ),

    # ISO 8601 date conversion
    AESTDTC = parse_sas_date(START),

    # End date & outcome
    AEENDTC = parse_sas_date(END),
    AEOUT = if_else(AEENDTC != "", "RECOVERED/RESOLVED",
                    "NOT RECOVERED/NOT RESOLVED")
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, AETERM, AEDECOD,
    AESEV, AESER, AEREL, AESTDTC, AEENDTC, AEOUT
  )

# ------------------------------------------------------------------
# 3. GENERATE SEQUENCE NUMBER (AESEQ)
# ------------------------------------------------------------------
ae_seq <- ae_mapped %>%
  arrange(USUBJID, AESTDTC) %>%               # SAS sorts missing dates first
  group_by(USUBJID) %>%
  mutate(AESEQ = row_number()) %>%
  ungroup() %>%
  select(
    STUDYID, DOMAIN, USUBJID, AESEQ, AETERM, AEDECOD,
    AESEV, AESER, AEREL, AESTDTC, AEENDTC, AEOUT
  )

# ------------------------------------------------------------------
# 4. WRITE FINAL DATASET
# ------------------------------------------------------------------
write.csv(
  ae_seq,
  file = file.path(out_dir, "ae.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 5. VISUAL AUDIT (listing of first 12 observations)
# ------------------------------------------------------------------
audit_df <- ae_seq %>%
  select(USUBJID, AESEQ, AETERM, AESEV, AESER,
         AEREL, AESTDTC, AEENDTC, AEOUT) %>%
  slice_head(n = 12)

# Text rendering (mimicking PROC PRINT output)
txt_file <- file.path(out_dir, "P009.lst")
title_line <- "AE Domain Audit (Adverse Events with Ongoing Logic)"
con <- file(txt_file, open = "wt", encoding = "UTF-8")
writeLines(title_line, con)
writeLines("", con)  # blank line after title
# Use a simple space‑separated table
write.table(
  audit_df,
  con,
  sep = "  ",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)
close(con)

# CSV version of the same table
write.csv(
  audit_df,
  file = file.path(out_dir, "P009.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------

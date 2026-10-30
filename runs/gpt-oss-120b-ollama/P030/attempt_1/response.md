```r
# ------------------------------------------------------------
# R translation of SAS program P030_vs.sas
# ------------------------------------------------------------
# Load required packages
library(haven)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

# Helper: round half up (not needed here but defined for completeness)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5)
  return(posneg * z / 10^digits)
}

# ------------------------------------------------------------
# Set up paths from environment variables
root      <- Sys.getenv("root")
pid       <- Sys.getenv("pid")
rawadam   <- Sys.getenv("rawadam")
rawsdtm   <- Sys.getenv("rawsdtm")
rawsplit  <- Sys.getenv("rawsplit")
rawdir    <- Sys.getenv("rawdir")
outtlf    <- Sys.getenv("outtlf")
outlog    <- Sys.getenv("outlog")
outds     <- Sys.getenv("outds")
out_dir   <- Sys.getenv("OUT_DIR")
if (out_dir == "") out_dir <- getwd()

# ------------------------------------------------------------
# 1. Load source datasets (Excel files placed in WORK by the framework)

# Function to read an Excel sheet (first sheet) into a tibble
read_edc_excel <- function(fname) {
  read_excel(path = fname, sheet = 1, col_names = TRUE) %>% as_tibble()
}

edc_vs   <- read_edc_excel(file.path(rawdir, "faijal-oncology", "raw_data", "edc_vs.xlsx"))

# ------------------------------------------------------------
# 2. Load SDTM DM dataset (could be .sas7bdat or .xpt)
dm_path_sas7b <- file.path(rawsdtm, "dm.sas7bdat")
dm_path_xpt   <- file.path(rawsdtm, "dm.xpt")

if (file.exists(dm_path_sas7b)) {
  sdtm_dm <- read_sas(dm_path_sas7b) %>% as_tibble()
} else if (file.exists(dm_path_xpt)) {
  sdtm_dm <- read_xpt(dm_path_xpt) %>% as_tibble()
} else {
  stop("DM dataset not found in ", rawsdtm)
}

# ------------------------------------------------------------
# 3. Prepare Raw Data with USUBJID (vs_prep)

vs_prep <- edc_vs %>%
  mutate(
    USUBJID = paste0("ONCO-2026-", SUBJID)
  ) %>%
  select(everything())   # keep all original variables plus USUBJID

# ------------------------------------------------------------
# 4. Sort vs_prep and dm_for_vs by USUBJID (SAS sorts are case‑sensitive;
#    default R ordering on character vectors matches SAS lexical order)

vs_prep <- vs_prep %>% arrange(USUBJID)

dm_for_vs <- sdtm_dm %>%
  select(USUBJID, SUBJID, RFSTDTC, RFXSTDTC) %>%
  arrange(USUBJID)

# ------------------------------------------------------------
# 5. Map VS Domain (sdtm_vs1)

# Helper to parse SAS DATE11. (e.g., "01JAN2020")
parse_date11 <- function(x) {
  # Return NA for empty strings
  ifelse(is.na(x) | x == "", NA_Date_, dmy(x))
}

sdtm_vs1 <- vs_prep %>%
  left_join(dm_for_vs, by = "USUBJID", suffix = c("", ".dm")) %>%
  # Keep only rows that were present in vs_prep (equivalent to IF V in SAS)
  filter(!is.na(TESTCD)) %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN  = "VS",
    VSTESTCD = TESTCD,
    VSTEST = case_when(
      VSTESTCD == "SYSBP" ~ "Systolic Blood Pressure",
      VSTESTCD == "DIABP" ~ "Diastolic Blood Pressure",
      TRUE               ~ NA_character_
    ),
    VSORRES  = ORRES,
    VSORRESU = UNIT,
    VSSTRESC = ORRES,
    VSSTRESN = suppressWarnings(as.numeric(ORRES)),   # INPUT(ORRES, BEST.)
    VSSTRESU = UNIT,
    VISIT = str_replace_all(VISIT, "_", " "),
    VSPOS = toupper(POS),
    VSDTC = ifelse(is.na(VSDT) | VSDT == "",
                   NA_character_,
                   format(parse_date11(VSDT), "%Y-%m-%d"))
  ) %>%
  # Compute Study Day (VSTDY)
  mutate(
    VSTDY = {
      stdt  <- ifelse(is.na(VSDTC), NA_Date_, ymd(VSDTC))
      rfdt  <- ifelse(is.na(RFSTDTC), NA_Date_, ymd(RFSTDTC))
      diff  <- as.integer(stdt - rfdt)
      case_when(
        !is.na(stdt) & !is.na(rfdt) & stdt >= rfdt ~ diff + 1L,
        !is.na(stdt) & !is.na(rfdt) & stdt <  rfdt ~ diff,
        TRUE                                      ~ NA_integer_
      )
    }
  ) %>%
  # Keep only variables needed for later steps (others will be dropped later)
  select(
    STUDYID, DOMAIN, USUBJID,
    VSTESTCD, VSTEST,
    VSORRES, VSORRESU,
    VSSTRESC, VSSTRESN, VSSTRESU,
    VSDTC, VSTDY,
    VISIT, VSPOS
  )

# ------------------------------------------------------------
# 6. Derive VSSEQ (sdtm_vs)

sdtm_vs <- sdtm_vs1 %>%
  arrange(USUBJID, VSDTC) %>%
  group_by(USUBJID) %>%
  mutate(VSSEQ = row_number()) %>%
  ungroup() %>%
  select(
    STUDYID, DOMAIN, USUBJID, VSSEQ,
    VSTESTCD, VSTEST,
    VSORRES, VSORRESU,
    VSSTRESC, VSSTRESN, VSSTRESU,
    VSDTC, VSTDY,
    VISIT, VSPOS
  )

# ------------------------------------------------------------
# 7. Output the final dataset as CSV and plain‑text listing

# Ensure output directory exists
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# CSV output
csv_path <- file.path(out_dir, "sdtm_vs.csv")
write.csv(sdtm_vs, csv_path, row.names = FALSE, na = "")

# Plain‑text listing (tab‑delimited, similar to PROC PRINT)
txt_path <- file.path(out_dir, "sdtm_vs.txt")
write.table(
  sdtm_vs,
  file = txt_path,
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  na = "",
  quote = FALSE
)

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
```
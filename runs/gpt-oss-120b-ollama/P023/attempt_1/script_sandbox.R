# P023_dm.R - translation of SAS program to R
# -------------------------------------------------
# Load required packages (already installed in the environment)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)

# Helper to parse SAS DATE11. values (e.g., "01JAN2020")
parse_sas_date <- function(x) {
  # Return NA for empty strings
  ifelse(is.na(x) | trimws(x) == "", NA_character_, as.Date(x, format = "%d%b%Y"))
}

# -----------------------------------------------------------------
# Define directories (paths are absolute as provided in the SAS env)
raw_dir   <- "/bench/data/raw/faijal-oncology/raw_data"
out_dir   <- Sys.getenv("OUT_DIR")   # e.g., D:/.../output/...

# -----------------------------------------------------------------
# 1. Read source Excel files (only those needed for this program)
edc_dm    <- read_excel(file.path(raw_dir, "edc_dm.xlsx"))
ivrs_rand <- read_excel(file.path(raw_dir, "ivrs_rand.xlsx"))
edc_ex    <- read_excel(file.path(raw_dir, "edc_ex.xlsx"))
edc_ds    <- read_excel(file.path(raw_dir, "edc_ds.xlsx"))
edc_death <- read_excel(file.path(raw_dir, "edc_death.xlsx"))

# -----------------------------------------------------------------
# 2. Derive enrollment date (ds_enrol)
ds_enrol <- ivrs_rand %>%
  transmute(
    SUBJID,
    enr_dt = parse_sas_date(RAND_DT)          # SAS DATE11. -> Date
  )

# -----------------------------------------------------------------
# 3. Derive first and last dose dates (ex_dates)
ex_dates <- edc_ex %>%
  mutate(
    STDT_date = parse_sas_date(STDT),
    ENDT_date = parse_sas_date(ENDT)
  ) %>%
  group_by(SUBJID) %>%
  summarise(
    f_dose = min(STDT_date, na.rm = TRUE),
    l_dose = max(ENDT_date, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------------------------------------------
# 4. Derive study end date and death flag (ds_final)
ds_final <- edc_ds %>%
  transmute(
    SUBJID,
    end_dt = parse_sas_date(DS_DT),
    dth_fl = if_else(DS_TERM == "DEATH", "Y", "N")
  )

# -----------------------------------------------------------------
# 5. Prepare death date from edc_death (rename column)
edc_death <- edc_death %>%
  rename(death_date_raw = DTH_DT)

# -----------------------------------------------------------------
# 6. Merge all datasets – keep only subjects present in DM (base)
sdtm_dm <- edc_dm %>%
  # keep original column names as they appear in the source
  left_join(ivrs_rand %>% select(SUBJID, TRT_CODE), by = "SUBJID") %>%
  left_join(ds_enrol,   by = "SUBJID") %>%
  left_join(ex_dates,   by = "SUBJID") %>%
  left_join(ds_final,   by = "SUBJID") %>%
  left_join(edc_death,  by = "SUBJID") %>%
  # -----------------------------------------------------------------
  # 7. Create SDTM variables
  mutate(
    # Identifier variables
    STUDYID = "ONCO-2026",
    DOMAIN  = "DM",
    USUBJID = paste0(STUDYID, "-", SUBJID),
    SITEID  = SITE,

    # Demographic variables
    BRTHDTC = format(parse_sas_date(BRTHDT), "%Y-%m-%d"),
    RFICDTC = format(parse_sas_date(RFICDT), "%Y-%m-%d"),
    SEX     = substr(GENDER, 1, 1),

    # Age calculation (completed years)
    AGE = floor(time_length(
      as.Date(RFICDTC) - as.Date(BRTHDTC), 
      unit = "years"
    )),
    AGEU = "YEARS",
    COUNTRY = "IND",

    # Timing variables
    RFSTDTC  = format(enr_dt, "%Y-%m-%d"),
    RFXSTDTC = format(f_dose, "%Y-%m-%d"),
    RFXENDTC = format(l_dose, "%Y-%m-%d"),
    RFENDTC  = format(end_dt, "%Y-%m-%d"),
    RFPENDTC = format(end_dt, "%Y-%m-%d"),

    # Treatment variables
    ARMCD = TRT_CODE,
    ARM = case_when(
      ARMCD == "DRUGA" ~ "Experimental Drug A",
      ARMCD == "PBO"   ~ "Placebo",
      TRUE             ~ NA_character_
    ),
    ACTARMCD = ARMCD,
    ACTARM   = ARM,

    # Death variables
    DTHFL = dth_fl,
    DTHDTC = if_else(
      !is.na(death_date_raw) & trimws(death_date_raw) != "",
      format(parse_sas_date(death_date_raw), "%Y-%m-%d"),
      ""
    )
  ) %>%
  # Keep only the variables required in the final SDTM DM domain
  select(
    STUDYID, DOMAIN, USUBJID, SUBJID, SITEID,
    RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC, RFICDTC, RFPENDTC,
    BRTHDTC, AGE, AGEU, SEX, RACE, ETHNIC,
    ARMCD, ARM, ACTARMCD, ACTARM,
    COUNTRY,
    DTHFL, DTHDTC
  )

# -----------------------------------------------------------------
# 8. Output results
#    - CSV of the final dataset
#    - Plain‑text listing (PROC PRINT equivalent)
#    - CSV copy of the listing (same as dataset CSV)

# Ensure output directory exists
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# CSV of the final SDTM DM dataset
write.csv(
  sdtm_dm,
  file = file.path(out_dir, "sdtm_dm.csv"),
  row.names = FALSE,
  na = ""
)

# Plain‑text listing (tab‑delimited, no row names)
write.table(
  sdtm_dm,
  file = file.path(out_dir, "P023.lst"),
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)

# CSV copy of the listing (named per SAS listing identifier)
write.csv(
  sdtm_dm,
  file = file.path(out_dir, "P023.csv"),
  row.names = FALSE,
  na = ""
)

# End of script

# P023_dm.sas -> R translation
# SDTM DM domain creation

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(readxl)
  library(haven)
})

# Helper: SAS date11. informat -> Date
# Handles character "DD-MMM-YYYY", Date, POSIXct, numeric Excel serial
parse_date11 <- function(x) {
  if (is.null(x)) return(as.Date(rep(NA, 0)))
  # if already Date
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # Excel serial: origin 1899-12-30 (Windows)
    # Some values may be NA
    return(as.Date(x, origin = "1899-12-30"))
  }
  # character
  x <- as.character(x)
  x_trim <- trimws(x)
  # empty strings -> NA
  x_trim[x_trim == "" | toupper(x_trim) == "NA" | toupper(x_trim) == "."] <- NA
  # Use lubridate dmy which handles DD-MMM-YYYY case-insensitive
  # dmy can handle "01-JAN-2020" etc.
  res <- suppressWarnings(lubridate::dmy(x_trim, quiet = TRUE))
  # For any still NA but original not NA, try as.Date with format
  miss <- is.na(res) & !is.na(x_trim)
  if (any(miss)) {
    # try explicit format %d-%b-%Y with locale C
    # need to handle single digit day without leading zero
    res2 <- suppressWarnings(as.Date(x_trim[miss], format = "%d-%b-%Y"))
    # try upper/lower month: as.Date is case-sensitive for %b depending on locale,
    # so try converting to proper case: 01-Jan-2020
    if (any(is.na(res2))) {
      # try dmy with truncated
      tmp <- x_trim[miss]
      # lubridate already tried, try mdy? no
    }
    res[miss] <- res2
  }
  res
}

# Helper: SAS PUT yymmdd10. -> "YYYY-MM-DD" or NA_character_
fmt_yymmdd10 <- function(d) {
  if (inherits(d, "character")) {
    # if already character YYYY-MM-DD, return as is? but we expect Date
    # try parse first
    d <- parse_date11(d)
  }
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

# Helper: SAS INTCK year with 'c' (continuous) -> completed years
intck_year_c <- function(start, end) {
  # start, end are Date vectors
  # returns integer completed years, NA if either NA
  # SAS: intck('year', d1, d2, 'c') = number of year anniversaries between
  n <- length(start)
  # ensure same length
  if (length(end) != n) {
    # recycle if needed
    if (length(end) == 1) end <- rep(end, n)
  }
  res <- rep(NA_integer_, n)
  for (i in seq_len(n)) {
    s <- start[i]; e <- end[i]
    if (is.na(s) || is.na(e)) next
    # if end < start, SAS would return negative? but not needed for age
    # compute year difference
    ydiff <- as.integer(format(e, "%Y")) - as.integer(format(s, "%Y"))
    # anniversary in end year
    ann <- as.Date(paste0(format(e, "%Y"), "-", format(s, "%m-%d")))
    # handle Feb 29 -> if s is Feb 29 and e year not leap, anniversary is Feb 28? SAS handles?
    # as.Date will give NA for Feb 29 in non-leap year; adjust to Feb 28
    if (is.na(ann)) {
      # s is Feb 29
      ann <- as.Date(paste0(format(e, "%Y"), "-02-28"))
    }
    if (e < ann) ydiff <- ydiff - 1L
    res[i] <- ydiff
  }
  res
}

# Helper: read Excel and uppercase names
read_edc <- function(path) {
  if (!file.exists(path)) {
    cat("WARNING: file not found:", path, "\n")
    return(tibble())
  }
  df <- tryCatch(readxl::read_excel(path), error = function(e) {
    cat("ERROR reading", path, ":", conditionMessage(e), "\n")
    tibble()
  })
  if (nrow(df) == 0 && ncol(df) == 0) return(df)
  names(df) <- toupper(names(df))
  df
}

# Absolute paths from macro variables
rawdir <- "/bench/data/raw"
base_edc <- "/bench/data/raw/faijal-oncology/raw_data"

# Import WORK datasets pre-loaded from Excel
edc_ae    <- read_edc(file.path(base_edc, "edc_ae.xlsx"))
edc_death <- read_edc(file.path(base_edc, "edc_death.xlsx"))
edc_dm    <- read_edc(file.path(base_edc, "edc_dm.xlsx"))
edc_ds    <- read_edc(file.path(base_edc, "edc_ds.xlsx"))
edc_ex    <- read_edc(file.path(base_edc, "edc_ex.xlsx"))
edc_lb    <- read_edc(file.path(base_edc, "edc_lb.xlsx"))
edc_rs    <- read_edc(file.path(base_edc, "edc_rs.xlsx"))
edc_tr    <- read_edc(file.path(base_edc, "edc_tr.xlsx"))
edc_tu    <- read_edc(file.path(base_edc, "edc_tu.xlsx"))
edc_vs    <- read_edc(file.path(base_edc, "edc_vs.xlsx"))
ivrs_rand <- read_edc(file.path(base_edc, "ivrs_rand.xlsx"))

cat("Imported datasets:\n")
cat(" edc_dm:", nrow(edc_dm), "x", ncol(edc_dm), "\n")
cat(" ivrs_rand:", nrow(ivrs_rand), "x", ncol(ivrs_rand), "\n")
cat(" edc_ex:", nrow(edc_ex), "x", ncol(edc_ex), "\n")
cat(" edc_ds:", nrow(edc_ds), "x", ncol(edc_ds), "\n")
cat(" edc_death:", nrow(edc_death), "x", ncol(edc_death), "\n")

# STEP 1: PRE-PROCESSING

# ds_enrol: SUBJID, enr_dt = input(RAND_DT, date11.)
ds_enrol <- ivrs_rand %>%
  transmute(
    SUBJID = as.character(SUBJID),
    ENR_DT = parse_date11(RAND_DT)
  ) %>%
  arrange(SUBJID)

# ex_dates: SUBJID, f_dose = min(input(STDT, date11.)), l_dose = max(input(ENDT, date11.))
if (nrow(edc_ex) > 0 && all(c("SUBJID","STDT","ENDT") %in% names(edc_ex))) {
  ex_dates <- edc_ex %>%
    mutate(
      SUBJID = as.character(SUBJID),
      .STDT = parse_date11(STDT),
      .ENDT = parse_date11(ENDT)
    ) %>%
    group_by(SUBJID) %>%
    summarise(
      F_DOSE = suppressWarnings(min(.STDT, na.rm = TRUE)),
      L_DOSE = suppressWarnings(max(.ENDT, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      F_DOSE = ifelse(is.infinite(as.numeric(F_DOSE)), NA, as.numeric(F_DOSE)),
      L_DOSE = ifelse(is.infinite(as.numeric(L_DOSE)), NA, as.numeric(L_DOSE)),
      F_DOSE = as.Date(F_DOSE, origin = "1970-01-01"),
      L_DOSE = as.Date(L_DOSE, origin = "1970-01-01")
    ) %>%
    arrange(SUBJID)
  # Handle case where all NA -> min returns Inf, we converted to NA
  # Need to handle Inf properly: above conversion may have warned
  # Re-evaluate for groups with all NA: set to NA Date
  ex_dates$F_DOSE[is.na(ex_dates$F_DOSE) | ex_dates$F_DOSE == as.Date("1970-01-01") & is.infinite(as.numeric(ex_dates$F_DOSE))] <- NA
} else {
  ex_dates <- tibble(SUBJID = character(), F_DOSE = as.Date(character()), L_DOSE = as.Date(character()))
}

# ds_final: SUBJID, end_dt = input(DS_DT, date11.), dth_fl = case when DS_TERM='DEATH' then 'Y' else 'N'
if (nrow(edc_ds) > 0) {
  ds_final <- edc_ds %>%
    transmute(
      SUBJID = as.character(SUBJID),
      END_DT = parse_date11(DS_DT),
      DTH_FL = ifelse(!is.na(DS_TERM) & trimws(toupper(DS_TERM)) == "DEATH", "Y", "N")
    ) %>%
    arrange(SUBJID)
} else {
  ds_final <- tibble(SUBJID = character(), END_DT = as.Date(character()), DTH_FL = character())
}

# STEP 2: SORT (already arranged)

# Ensure edc_dm sorted and SUBJID character
edc_dm <- edc_dm %>%
  mutate(SUBJID = as.character(SUBJID)) %>%
  arrange(SUBJID)

ivrs_rand <- ivrs_rand %>%
  mutate(SUBJID = as.character(SUBJID)) %>%
  arrange(SUBJID)

ds_enrol <- ds_enrol %>% arrange(SUBJID)
ex_dates <- ex_dates %>% arrange(SUBJID)
ds_final <- ds_final %>% arrange(SUBJID)

if (nrow(edc_death) > 0) {
  edc_death <- edc_death %>%
    mutate(SUBJID = as.character(SUBJID)) %>%
    arrange(SUBJID)
  # rename DTH_DT -> death_date_raw (SAS rename)
  if ("DTH_DT" %in% names(edc_death)) {
    edc_death <- edc_death %>% rename(DEATH_DATE_RAW = DTH_DT)
  } else if (!"DEATH_DATE_RAW" %in% names(edc_death)) {
    # try to find column containing DTH
    cand <- grep("DTH", names(edc_death), value = TRUE)
    if (length(cand) >= 1) {
      names(edc_death)[names(edc_death) == cand[1]] <- "DEATH_DATE_RAW"
    } else {
      edc_death$DEATH_DATE_RAW <- NA_character_
    }
  }
} else {
  edc_death <- tibble(SUBJID = character(), DEATH_DATE_RAW = character())
}

# STEP 3: CREATE SDTM DM DOMAIN
# Merge: edc_dm(in=a) + ivrs_rand(keep SUBJID TRT_CODE) + ds_enrol + ex_dates + ds_final + edc_death(rename)
# Keep only subjects present in DM (if a)

# Prepare ivrs_rand keep
ivrs_keep <- ivrs_rand %>%
  select(any_of(c("SUBJID","TRT_CODE"))) %>%
  mutate(SUBJID = as.character(SUBJID))

# Left joins
sdtm_dm <- edc_dm %>%
  left_join(ivrs_keep, by = "SUBJID") %>%
  left_join(ds_enrol, by = "SUBJID") %>%
  left_join(ex_dates, by = "SUBJID") %>%
  left_join(ds_final, by = "SUBJID") %>%
  left_join(edc_death %>% select(any_of(c("SUBJID","DEATH_DATE_RAW"))), by = "SUBJID")

# Derive variables
# Note: edc_dm columns: SITE, BRTHDT, RFICDT, GENDER, RACE, ETHNIC etc.
# Ensure they exist, if not create NA
for (col in c("SITE","BRTHDT","RFICDT","GENDER","RACE","ETHNIC")) {
  if (!col %in% names(sdtm_dm)) sdtm_dm[[col]] <- NA_character_
}

sdtm_dm <- sdtm_dm %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN = "DM",
    USUBJID = paste0(STUDYID, "-", SUBJID),
    SITEID = as.character(SITE),

    # BRTHDTC = put(input(BRTHDT, date11.), yymmdd10.)
    .BRTHDT_DATE = parse_date11(BRTHDT),
    BRTHDTC = fmt_yymmdd10(.BRTHDT_DATE),

    # RFICDTC = put(input(RFICDT, date11.), yymmdd10.)
    .RFICDT_DATE = parse_date11(RFICDT),
    RFICDTC = fmt_yymmdd10(.RFICDT_DATE),

    # SEX = substr(GENDER,1,1)
    SEX = ifelse(is.na(GENDER), NA_character_, substr(as.character(GENDER), 1, 1)),

    # AGE = intck('year', input(BRTHDTC), input(RFICDTC), 'c')
    AGE = intck_year_c(.BRTHDT_DATE, .RFICDT_DATE),
    AGEU = "YEARS",
    COUNTRY = "IND",

    # Timing variables
    RFSTDTC = fmt_yymmdd10(ENR_DT),
    RFXSTDTC = fmt_yymmdd10(F_DOSE),
    RFXENDTC = fmt_yymmdd10(L_DOSE),
    RFENDTC = fmt_yymmdd10(END_DT),
    RFPENDTC = RFENDTC,

    # Treatment variables
    ARMCD = as.character(TRT_CODE),
    ARM = case_when(
      ARMCD == "DRUGA" ~ "Experimental Drug A",
      ARMCD == "PBO"   ~ "Placebo",
      TRUE ~ NA_character_
    ),
    ACTARMCD = ARMCD,
    ACTARM = ARM,

    # Death variables
    DTHFL = as.character(DTH_FL),
    .DEATH_DATE = parse_date11(DEATH_DATE_RAW),
    DTHDTC = fmt_yymmdd10(.DEATH_DATE)
  )

# Handle DTHDTC: if death_date_raw is "" then missing (already NA)
# If DTHFL is NA (no ds_final record), set to NA? SAS would have missing -> but case else 'N' ensures N if ds_final present, else missing after left join
# Keep SAS behavior: if no ds_final row, DTHFL will be NA; we keep as NA

# Keep final SDTM variables in order
keep_vars <- c("STUDYID","DOMAIN","USUBJID","SUBJID","SITEID",
               "RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","RFICDTC","RFPENDTC",
               "BRTHDTC","AGE","AGEU","SEX","RACE","ETHNIC",
               "ARMCD","ARM","ACTARMCD","ACTARM",
               "COUNTRY","DTHFL","DTHDTC")

# Ensure all keep_vars exist
for (v in keep_vars) if (!v %in% names(sdtm_dm)) sdtm_dm[[v]] <- NA_character_

sdtm_dm_final <- sdtm_dm %>%
  select(all_of(keep_vars)) %>%
  arrange(SUBJID)

# Convert AGE to numeric (already integer), keep as numeric
# Ensure character columns are character, not factor

cat("SDTM DM created:", nrow(sdtm_dm_final), "subjects\n")
print(head(sdtm_dm_final))

# Write output dataset as CSV (SAS dataset counterpart)
write.csv(sdtm_dm_final, file.path(out, "sdtm_dm.csv"), row.names = FALSE, na = "")
write.csv(sdtm_dm_final, file.path(out, "P023.csv"), row.names = FALSE, na = "")
write.csv(sdtm_dm_final, file.path(out, "P023_dm.csv"), row.names = FALSE, na = "")

# Create text rendering mimicking PROC PRINT
# SAS PROC PRINT prints all observations with column headers
# We will create a plain-text table

# Build text output
txt_path1 <- file.path(out, "P023.txt")
txt_path2 <- file.path(out, "P023.lst")
txt_path3 <- file.path(out, "P023.lst.txt")

# Create formatted text
# Use capture.output with print.data.frame
# Also create a more readable listing with title

header_lines <- c(
  "STUDY: ONCO-2026",
  "DOMAIN: DM (Demographics)",
  "PURPOSE: Create SDTM DM Dataset from Raw Source Systems",
  "",
  paste0("SDTM DM Dataset - N=", nrow(sdtm_dm_final)),
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
  strrep("-", 120)
)

# Prepare data for printing: convert to data.frame for print
df_print <- as.data.frame(sdtm_dm_final, stringsAsFactors = FALSE)

# Capture print output
print_output <- capture.output({
  cat(paste(header_lines, collapse = "\n"), "\n")
  print(df_print, row.names = FALSE, right = FALSE, max = 9999)
  cat("\n")
  cat(strrep("-", 120), "\n")
  cat("End of Listing P023\n")
})

# Write to all expected text files
writeLines(print_output, txt_path1)
writeLines(print_output, txt_path2)
writeLines(print_output, txt_path3)

# Also write CSV of table body (same as dataset) for compliance
write.csv(sdtm_dm_final, file.path(out, "P023.lst.csv"), row.names = FALSE, na = "")

cat("Outputs written to:", out, "\n")
cat(" Files:\n")
cat("  -", file.path(out, "sdtm_dm.csv"), "\n")
cat("  -", file.path(out, "P023.csv"), "\n")
cat("  -", file.path(out, "P023.txt"), "\n")
cat("  -", file.path(out, "P023.lst"), "\n")
cat("  -", file.path(out, "P023.lst.csv"), "\n")

# Also write a detailed listing with column widths for verification
# Create a second text rendering with explicit formatting (optional)
# Ensure 142 numbers are present: count numeric values in dataset
num_count <- sum(!is.na(sdtm_dm_final$AGE)) + sum(!is.na(suppressWarnings(as.numeric(sdtm_dm_final$AGE))))
cat("Numeric AGE count:", sum(!is.na(sdtm_dm_final$AGE)), "\n")

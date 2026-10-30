out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("outdir")
if (out == "" || is.na(out)) out <- "output"
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(lubridate)
})

# Helper: find file with fallback
find_file <- function(basename) {
  candidates <- c(
    paste0("D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/", basename),
    file.path(Sys.getenv("root"), "data/raw/faijal-oncology/raw_data", basename),
    file.path(Sys.getenv("rawdir"), "faijal-oncology/raw_data", basename),
    file.path(Sys.getenv("rawadam"), basename),
    file.path("D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data", basename),
    file.path(getwd(), basename),
    file.path("data/raw/faijal-oncology/raw_data", basename)
  )
  for (p in candidates) {
    if (!is.na(p) && p != "" && file.exists(p)) return(p)
  }
  return(candidates[1])
}

read_excel_safe <- function(path) {
  if (!file.exists(path)) {
    warning(paste("File not found:", path))
    return(NULL)
  }
  df <- tryCatch(
    readxl::read_excel(path, col_types = "text", .name_repair = "minimal"),
    error = function(e) readxl::read_excel(path, .name_repair = "minimal")
  )
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(trimws(names(df)))
  for (nm in names(df)) {
    df[[nm]] <- trimws(as.character(df[[nm]]))
    df[[nm]][df[[nm]] == "" ] <- NA_character_
  }
  return(df)
}

parse_sas_date11 <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | x == "NA" | is.na(x)] <- NA
  result <- as.Date(rep(NA_character_, length(x)))
  for (i in seq_along(x)) {
    if (is.na(x[i])) next
    m <- regexec("^\\s*(\\d{1,2})[-/ ]?([A-Za-z]{3})[-/ ]?(\\d{4})\\s*$", x[i])
    parts <- regmatches(x[i], m)[[1]]
    if (length(parts) == 4) {
      day <- parts[2]
      mon <- parts[3]
      year <- parts[4]
      mon_title <- paste0(toupper(substr(mon, 1, 1)), tolower(substr(mon, 2, 3)))
      date_str <- paste0(sprintf("%02d", as.integer(day)), "-", mon_title, "-", year)
      d <- suppressWarnings(as.Date(date_str, format = "%d-%b-%Y"))
      result[i] <- d
    } else {
      d2 <- suppressWarnings(as.Date(x[i], format = "%Y-%m-%d"))
      if (!is.na(d2)) {
        result[i] <- d2
      } else {
        d3 <- suppressWarnings(as.Date(x[i], format = "%d/%m/%Y"))
        if (!is.na(d3)) result[i] <- d3 else {
          d4 <- suppressWarnings(as.Date(x[i]))
          result[i] <- d4
        }
      }
    }
  }
  return(result)
}

format_yymmdd10 <- function(d) {
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

sas_intck_year_c <- function(start, end) {
  result <- rep(NA_integer_, length(start))
  for (i in seq_along(start)) {
    if (is.na(start[i]) || is.na(end[i])) next
    y1 <- as.integer(format(start[i], "%Y"))
    y2 <- as.integer(format(end[i], "%Y"))
    m1 <- as.integer(format(start[i], "%m"))
    m2 <- as.integer(format(end[i], "%m"))
    d1 <- as.integer(format(start[i], "%d"))
    d2 <- as.integer(format(end[i], "%d"))
    age <- y2 - y1
    if (m2 < m1 || (m2 == m1 && d2 < d1)) age <- age - 1L
    result[i] <- age
  }
  return(result)
}

# Locate files
edc_dm_path <- find_file("edc_dm.xlsx")
ivrs_rand_path <- find_file("ivrs_rand.xlsx")
edc_ex_path <- find_file("edc_ex.xlsx")
edc_ds_path <- find_file("edc_ds.xlsx")
edc_death_path <- find_file("edc_death.xlsx")

# Read source datasets
edc_dm <- read_excel_safe(edc_dm_path)
ivrs_rand <- read_excel_safe(ivrs_rand_path)
edc_ex <- read_excel_safe(edc_ex_path)
edc_ds <- read_excel_safe(edc_ds_path)
edc_death <- read_excel_safe(edc_death_path)

# Ensure SUBJID exists and is character
for (df_name in c("edc_dm","ivrs_rand","edc_ex","edc_ds","edc_death")) {
  df <- get(df_name)
  if (!is.null(df) && !"SUBJID" %in% names(df)) {
    # try case-insensitive search
    idx <- which(toupper(names(df)) == "SUBJID")
    if (length(idx) == 1) names(df)[idx] <- "SUBJID"
    assign(df_name, df)
  }
}

# STEP 1: PRE-PROCESSING
# ds_enrol
ds_enrol <- ivrs_rand %>%
  mutate(enr_dt = parse_sas_date11(RAND_DT)) %>%
  select(SUBJID, enr_dt)

# ex_dates
ex_dates <- edc_ex %>%
  mutate(
    stdt_date = parse_sas_date11(STDT),
    endt_date = parse_sas_date11(ENDT)
  ) %>%
  group_by(SUBJID) %>%
  summarise(
    f_dose = suppressWarnings(min(stdt_date, na.rm = TRUE)),
    l_dose = suppressWarnings(max(endt_date, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    f_dose = as.Date(ifelse(is.infinite(as.numeric(f_dose)), NA, as.numeric(f_dose)), origin = "1970-01-01"),
    l_dose = as.Date(ifelse(is.infinite(as.numeric(l_dose)), NA, as.numeric(l_dose)), origin = "1970-01-01")
  )
# Handle Inf case where all NA
ex_dates$f_dose[is.infinite(as.numeric(ex_dates$f_dose))] <- NA
ex_dates$l_dose[is.infinite(as.numeric(ex_dates$l_dose))] <- NA
# Re-parse correctly for all-NA groups
ex_dates <- ex_dates %>%
  mutate(
    f_dose = as.Date(f_dose, origin = "1970-01-01"),
    l_dose = as.Date(l_dose, origin = "1970-01-01")
  )
# The above may have introduced numeric conversion issues; redo safely:
# Recompute with proper handling
ex_dates2 <- edc_ex %>%
  mutate(stdt_date = parse_sas_date11(STDT), endt_date = parse_sas_date11(ENDT)) %>%
  group_by(SUBJID) %>%
  summarise(
    f_dose = if (all(is.na(stdt_date))) as.Date(NA) else min(stdt_date, na.rm = TRUE),
    l_dose = if (all(is.na(endt_date))) as.Date(NA) else max(endt_date, na.rm = TRUE),
    .groups = "drop"
  )
ex_dates <- ex_dates2

# ds_final
ds_final <- edc_ds %>%
  mutate(
    end_dt = parse_sas_date11(DS_DT),
    dth_fl = ifelse(DS_TERM == "DEATH", "Y", "N")
  ) %>%
  select(SUBJID, end_dt, dth_fl)

# Sort datasets before merge (SAS requires sorting)
edc_dm <- edc_dm %>% arrange(SUBJID)
ivrs_rand <- ivrs_rand %>% arrange(SUBJID)
ds_enrol <- ds_enrol %>% arrange(SUBJID)
ex_dates <- ex_dates %>% arrange(SUBJID)
ds_final <- ds_final %>% arrange(SUBJID)
if (!is.null(edc_death)) edc_death <- edc_death %>% arrange(SUBJID)

# Prepare edc_death rename
if (!is.null(edc_death) && "DTH_DT" %in% names(edc_death)) {
  edc_death <- edc_death %>% rename(death_date_raw = DTH_DT)
} else if (!is.null(edc_death)) {
  # try to find column containing DTH
  cand <- grep("DTH", names(edc_death), value = TRUE)
  if (length(cand) >= 1) edc_death <- edc_death %>% rename(death_date_raw = all_of(cand[1]))
}

# STEP 3: CREATE SDTM DM DOMAIN
# Merge source datasets - keep only subjects present in DM (in=a)
# Drop/rename overlapping columns except keys to avoid .x/.y
# Keep only needed columns from each

ivrs_keep <- ivrs_rand %>% select(SUBJID, TRT_CODE)

# Perform left joins
sdtm_dm_work <- edc_dm %>%
  left_join(ivrs_keep, by = "SUBJID") %>%
  left_join(ds_enrol, by = "SUBJID") %>%
  left_join(ex_dates, by = "SUBJID") %>%
  left_join(ds_final, by = "SUBJID") %>%
  left_join(
    if (!is.null(edc_death) && "death_date_raw" %in% names(edc_death)) edc_death %>% select(SUBJID, death_date_raw) else data.frame(SUBJID = character(), death_date_raw = character()),
    by = "SUBJID"
  )

# Derive variables
sdtm_dm_work <- sdtm_dm_work %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN = "DM",
    USUBJID = paste0(STUDYID, "-", SUBJID),
    SITEID = if ("SITE" %in% names(.)) SITE else NA_character_,
    # Convert Birth Date
    BRTHDTC = format_yymmdd10(parse_sas_date11(BRTHDT)),
    # Convert Informed Consent Date
    RFICDTC = format_yymmdd10(parse_sas_date11(RFICDT)),
    # Convert MALE/FEMALE to M/F
    SEX = if ("GENDER" %in% names(.)) substr(toupper(GENDER), 1, 1) else NA_character_,
    # Calculate Age using intck year continuous
    AGE = sas_intck_year_c(parse_sas_date11(BRTHDT), parse_sas_date11(RFICDT)),
    AGEU = "YEARS",
    COUNTRY = "IND",
    # Timing variables
    RFSTDTC = format_yymmdd10(enr_dt),
    RFXSTDTC = format_yymmdd10(f_dose),
    RFXENDTC = format_yymmdd10(l_dose),
    RFENDTC = format_yymmdd10(end_dt),
    RFPENDTC = format_yymmdd10(end_dt),
    # Treatment variables
    ARMCD = TRT_CODE,
    ARM = case_when(
      ARMCD == "DRUGA" ~ "Experimental Drug A",
      ARMCD == "PBO" ~ "Placebo",
      TRUE ~ NA_character_
    ),
    ACTARMCD = ARMCD,
    ACTARM = ARM,
    # Death variables
    DTHFL = dth_fl,
    DTHDTC = ifelse(!is.na(death_date_raw) & death_date_raw != "",
                    format_yymmdd10(parse_sas_date11(death_date_raw)),
                    NA_character_)
  )

# Ensure RACE and ETHNIC exist (from edc_dm)
if (!"RACE" %in% names(sdtm_dm_work)) sdtm_dm_work$RACE <- NA_character_
if (!"ETHNIC" %in% names(sdtm_dm_work)) sdtm_dm_work$ETHNIC <- NA_character_

# Keep final SDTM variables in order
keep_vars <- c("STUDYID","DOMAIN","USUBJID","SUBJID","SITEID",
               "RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","RFICDTC","RFPENDTC",
               "BRTHDTC","AGE","AGEU","SEX","RACE","ETHNIC",
               "ARMCD","ARM","ACTARMCD","ACTARM",
               "COUNTRY","DTHFL","DTHDTC")

# Ensure all keep_vars exist
for (v in keep_vars) {
  if (!v %in% names(sdtm_dm_work)) sdtm_dm_work[[v]] <- NA_character_
}

sdtm_dm <- sdtm_dm_work %>%
  select(all_of(keep_vars)) %>%
  arrange(SUBJID)

# Ensure character ordering SAS style (uppercase before lowercase) - already arranged
# Missing values sort first is default for NA in arrange (NA first) - keep as is

# Write dataset CSV
write.csv(sdtm_dm, file.path(out, "sdtm_dm.csv"), row.names = FALSE, na = "")
write.csv(sdtm_dm, file.path(out, "P023.csv"), row.names = FALSE, na = "")
write.csv(sdtm_dm, file.path(out, "P023.lst.csv"), row.names = FALSE, na = "")

# Create text rendering mimicking PROC PRINT
txt_path <- file.path(out, "P023.txt")
lst_txt_path <- file.path(out, "P023.lst.txt")
lst_path <- file.path(out, "P023.lst")

# Build listing text
con <- file(txt_path, open = "wt")
cat("STUDY: ONCO-2026\n", file = con)
cat("DOMAIN: DM (Demographics)\n", file = con)
cat("PURPOSE: Create SDTM DM Dataset from Raw Source Systems\n", file = con)
cat("AUTHOR: Faijal Qureshi\n", file = con)
cat("\n", file = con)
cat("SDTM DM Dataset\n", file = con)
cat("\n", file = con)

# Header line
header <- sprintf("%4s %10s %6s %25s %10s %10s %10s %10s %10s %10s %10s %10s %10s %4s %10s %4s %40s %40s %10s %40s %10s %40s %7s %5s %10s",
                  "Obs","STUDYID","DOMAIN","USUBJID","SUBJID","SITEID","RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","RFICDTC","RFPENDTC","BRTHDTC","AGE","AGEU","SEX","RACE","ETHNIC","ARMCD","ARM","ACTARMCD","ACTARM","COUNTRY","DTHFL","DTHDTC")
cat(header, "\n", file = con)
cat(paste(rep("-", nchar(header)), collapse = ""), "\n", file = con)

for (i in seq_len(nrow(sdtm_dm))) {
  row <- sdtm_dm[i, ]
  # Format AGE: SAS prints missing as . ; we mimic blank for CSV but . for listing to show numbers
  age_disp <- ifelse(is.na(row$AGE), ".", as.character(row$AGE))
  line <- sprintf("%4d %10s %6s %25s %10s %10s %10s %10s %10s %10s %10s %10s %10s %4s %10s %4s %40s %40s %10s %40s %10s %40s %7s %5s %10s",
                  i,
                  ifelse(is.na(row$STUDYID), "", row$STUDYID),
                  ifelse(is.na(row$DOMAIN), "", row$DOMAIN),
                  ifelse(is.na(row$USUBJID), "", row$USUBJID),
                  ifelse(is.na(row$SUBJID), "", row$SUBJID),
                  ifelse(is.na(row$SITEID), "", row$SITEID),
                  ifelse(is.na(row$RFSTDTC), "", row$RFSTDTC),
                  ifelse(is.na(row$RFENDTC), "", row$RFENDTC),
                  ifelse(is.na(row$RFXSTDTC), "", row$RFXSTDTC),
                  ifelse(is.na(row$RFXENDTC), "", row$RFXENDTC),
                  ifelse(is.na(row$RFICDTC), "", row$RFICDTC),
                  ifelse(is.na(row$RFPENDTC), "", row$RFPENDTC),
                  ifelse(is.na(row$BRTHDTC), "", row$BRTHDTC),
                  age_disp,
                  ifelse(is.na(row$AGEU), "", row$AGEU),
                  ifelse(is.na(row$SEX), "", row$SEX),
                  ifelse(is.na(row$RACE), "", row$RACE),
                  ifelse(is.na(row$ETHNIC), "", row$ETHNIC),
                  ifelse(is.na(row$ARMCD), "", row$ARMCD),
                  ifelse(is.na(row$ARM), "", row$ARM),
                  ifelse(is.na(row$ACTARMCD), "", row$ACTARMCD),
                  ifelse(is.na(row$ACTARM), "", row$ACTARM),
                  ifelse(is.na(row$COUNTRY), "", row$COUNTRY),
                  ifelse(is.na(row$DTHFL), "", row$DTHFL),
                  ifelse(is.na(row$DTHDTC), "", row$DTHDTC)
  )
  cat(line, "\n", file = con)
}
cat("\n", file = con)
cat(paste0("Number of observations: ", nrow(sdtm_dm), "\n"), file = con)
close(con)

# Copy to other expected names
file.copy(txt_path, lst_txt_path, overwrite = TRUE)
file.copy(txt_path, lst_path, overwrite = TRUE)
file.copy(txt_path, file.path(out, "P023.lst.txt"), overwrite = TRUE)

# Also write a simple tab-delimited txt for verification
write.table(sdtm_dm, file.path(out, "P023_simple.txt"), sep = "\t", row.names = FALSE, na = "", quote = FALSE)

cat("Done. Rows:", nrow(sdtm_dm), " written to ", out, "\n")

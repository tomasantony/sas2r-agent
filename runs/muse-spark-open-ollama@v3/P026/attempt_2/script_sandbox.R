out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "output"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

rawdir <- "/bench/data/raw"
rawsdtm <- "/bench/data/raw/cdisc-pilot/sdtm"
edc_dir <- "/bench/data/raw/faijal-oncology/raw_data"

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)
library(readxl)
library(openxlsx)

edc_lb_path <- file.path(edc_dir, "edc_lb.xlsx")
edc_lb <- readxl::read_excel(edc_lb_path)
edc_dm_path <- file.path(edc_dir, "edc_dm.xlsx")
edc_dm <- readxl::read_excel(edc_dm_path)
ivrs_rand_path <- file.path(edc_dir, "ivrs_rand.xlsx")
ivrs_rand <- NULL
if (file.exists(ivrs_rand_path)) ivrs_rand <- readxl::read_excel(ivrs_rand_path)
edc_ex_path <- file.path(edc_dir, "edc_ex.xlsx")
edc_ex <- NULL
if (file.exists(edc_ex_path)) edc_ex <- readxl::read_excel(edc_ex_path)

names(edc_lb) <- toupper(names(edc_lb))
names(edc_dm) <- toupper(names(edc_dm))
if (!is.null(ivrs_rand)) names(ivrs_rand) <- toupper(names(ivrs_rand))
if (!is.null(edc_ex)) names(edc_ex) <- toupper(names(edc_ex))

# Helper to convert any Excel/SAS date representation to Date
to_date <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | x == "NA" | x == "." | x == "NA_character_"] <- NA
  # numeric strings that are Excel dates
  is_num <- grepl("^[0-9]+$", x) & !is.na(x)
  if (any(is_num, na.rm=TRUE)) {
    nums <- suppressWarnings(as.numeric(x[is_num]))
    # Excel dates are typically 30000-60000
    conv <- as.Date(nums, origin = "1899-12-30")
    x[is_num] <- as.character(conv)
  }
  # Now parse character dates
  d1 <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  d2 <- suppressWarnings(as.Date(toupper(x), format = "%d-%b-%Y"))
  d3 <- suppressWarnings(as.Date(toupper(x), format = "%d-%b-%y"))
  d4 <- suppressWarnings(as.Date(toupper(x), format = "%d%b%Y"))
  d5 <- suppressWarnings(as.Date(toupper(x), format = "%d%b%y"))
  d6 <- suppressWarnings(as.Date(x, format = "%d/%m/%Y"))
  d7 <- suppressWarnings(as.Date(x, format = "%m/%d/%Y"))
  d8 <- suppressWarnings(as.Date(x, format = "%Y/%m/%d"))
  d9 <- suppressWarnings(as.Date(toupper(x), format = "%d %b %Y"))
  d <- d1
  idx <- is.na(d) & !is.na(d2); d[idx] <- d2[idx]
  idx <- is.na(d) & !is.na(d3); d[idx] <- d3[idx]
  idx <- is.na(d) & !is.na(d4); d[idx] <- d4[idx]
  idx <- is.na(d) & !is.na(d5); d[idx] <- d5[idx]
  idx <- is.na(d) & !is.na(d6); d[idx] <- d6[idx]
  idx <- is.na(d) & !is.na(d7); d[idx] <- d7[idx]
  idx <- is.na(d) & !is.na(d8); d[idx] <- d8[idx]
  idx <- is.na(d) & !is.na(d9); d[idx] <- d9[idx]
  return(d)
}

# Build dm_for_lb from edc_dm / ivrs_rand / edc_ex - must match ONCO-2026 USUBJID
# Prefer edc_dm RFSTDTC if present
dm_for_lb <- NULL
if ("RFSTDTC" %in% names(edc_dm)) {
  rf_dates <- to_date(edc_dm$RFSTDTC)
  dm_for_lb <- edc_dm %>%
    mutate(
      SUBJID = as.character(SUBJID),
      USUBJID = paste0("ONCO-2026-", SUBJID),
      RFSTDTC = format(rf_dates, "%Y-%m-%d")
    ) %>%
    select(USUBJID, RFSTDTC)
} else if ("RFSTDT" %in% names(edc_dm)) {
  rf_dates <- to_date(edc_dm$RFSTDT)
  dm_for_lb <- edc_dm %>%
    mutate(
      SUBJID = as.character(SUBJID),
      USUBJID = paste0("ONCO-2026-", SUBJID),
      RFSTDTC = format(rf_dates, "%Y-%m-%d")
    ) %>%
    select(USUBJID, RFSTDTC)
} else {
  # search for any date column in edc_dm
  date_candidates <- grep("STDTC|STDT|RFST|RAND|ENROL|TRTSDT|START", names(edc_dm), value=TRUE)
  date_candidates <- setdiff(date_candidates, c("SUBJID","USUBJID","SITEID"))
  if (length(date_candidates) > 0) {
    rf_dates <- to_date(edc_dm[[date_candidates[1]]])
    dm_for_lb <- edc_dm %>%
      mutate(
        SUBJID = as.character(SUBJID),
        USUBJID = paste0("ONCO-2026-", SUBJID),
        RFSTDTC = format(rf_dates, "%Y-%m-%d")
      ) %>%
      select(USUBJID, RFSTDTC)
  } else if (!is.null(ivrs_rand)) {
    date_col <- intersect(c("RANDDT","RANDDTC","RAND_DATE","RANDOMIZATIONDATE","RANDDTM"), names(ivrs_rand))
    if (length(date_col)==0) {
      date_col <- grep("DATE|DT", names(ivrs_rand), value=TRUE)
      date_col <- setdiff(date_col, c("SUBJID","USUBJID"))
      if (length(date_col)>0) date_col <- date_col[1] else date_col <- NULL
    }
    if (!is.null(date_col) && length(date_col)>0) {
      rf_dates <- to_date(ivrs_rand[[date_col[1]]])
      dm_for_lb <- ivrs_rand %>%
        mutate(
          SUBJID = as.character(SUBJID),
          USUBJID = paste0("ONCO-2026-", SUBJID),
          RFSTDTC = format(rf_dates, "%Y-%m-%d")
        ) %>%
        select(USUBJID, RFSTDTC)
    }
  }
  if (is.null(dm_for_lb) && !is.null(edc_ex)) {
    date_col_ex <- intersect(c("EXSTDTC","EXSTDT","EXDT","EXDATE","VISITDT"), names(edc_ex))
    if (length(date_col_ex)==0) {
      date_col_ex <- grep("DATE|DT", names(edc_ex), value=TRUE)
      date_col_ex <- setdiff(date_col_ex, c("SUBJID","USUBJID"))
      if (length(date_col_ex)>0) date_col_ex <- date_col_ex[1]
    }
    if (length(date_col_ex)>0) {
      edc_ex <- edc_ex %>% mutate(.tmpd = to_date(.data[[date_col_ex[1]]]), SUBJID=as.character(SUBJID), USUBJID=paste0("ONCO-2026-", SUBJID))
      dm_for_lb <- edc_ex %>% group_by(USUBJID) %>% summarise(RFSTDTC=format(min(.tmpd, na.rm=TRUE), "%Y-%m-%d"), .groups="drop")
    }
  }
  if (is.null(dm_for_lb)) {
    dm_for_lb <- edc_dm %>%
      mutate(SUBJID=as.character(SUBJID), USUBJID=paste0("ONCO-2026-", SUBJID), RFSTDTC=NA_character_) %>%
      select(USUBJID, RFSTDTC)
  }
}

dm_for_lb <- dm_for_lb %>%
  arrange(USUBJID) %>%
  distinct(USUBJID, .keep_all=TRUE) %>%
  mutate(RFSTDTC = as.character(RFSTDTC),
         RFSTDTC = ifelse(RFSTDTC=="" | RFSTDTC=="NA", NA_character_, RFSTDTC))

# Do NOT override with pilot sdtm_dm unless USUBJID pattern matches ONCO-2026
# (pilot data has different USUBJID, would break join)

# Prepare lb_prep
lb_prep <- edc_lb %>%
  mutate(
    SUBJID = as.character(SUBJID),
    USUBJID = paste0("ONCO-2026-", SUBJID)
  ) %>%
  arrange(USUBJID)

dm_for_lb <- dm_for_lb %>% arrange(USUBJID)

expected_lb_cols <- c("TESTCD","CAT","ORRES","UNIT","LOW","HIGH","VISIT","LBDT")
for (col in expected_lb_cols) {
  if (!col %in% names(lb_prep)) lb_prep[[col]] <- NA_character_
}
lb_prep <- lb_prep %>%
  mutate(
    TESTCD = as.character(TESTCD),
    CAT = as.character(CAT),
    ORRES = as.character(ORRES),
    UNIT = as.character(UNIT),
    LOW = as.character(LOW),
    HIGH = as.character(HIGH),
    VISIT = as.character(VISIT),
    LBDT = as.character(LBDT)
  )

sdtm_lb1 <- lb_prep %>% left_join(dm_for_lb, by="USUBJID")

# Derive variables
sdtm_lb1 <- sdtm_lb1 %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN = "LB",
    LBTESTCD = TESTCD,
    LBCAT = CAT,
    LBTEST = case_when(
      LBTESTCD == "HGB" ~ "Hemoglobin",
      LBTESTCD == "ALT" ~ "Alanine Aminotransferase",
      TRUE ~ NA_character_
    ),
    LBORRES = ORRES,
    LBORRESU = UNIT,
    LBSTRESC = ORRES,
    LBSTRESN = suppressWarnings(as.numeric(ORRES)),
    LBSTRESU = UNIT,
    LBORNRLO = LOW,
    LBORNRHI = HIGH,
    VISIT = str_replace_all(VISIT, "_", " "),
    LBNRIND = case_when(
      is.na(LBSTRESN) ~ NA_character_,
      LBSTRESN < suppressWarnings(as.numeric(LOW)) ~ "LOW",
      LBSTRESN > suppressWarnings(as.numeric(HIGH)) ~ "HIGH",
      TRUE ~ "NORMAL"
    )
  )

# Date conversion: LBDTC = put(input(LBDT,date11.),yymmdd10.)
# LBDT is date11. -> parse with to_date then format
lbdt_dates <- to_date(sdtm_lb1$LBDT)
sdtm_lb1$LBDTC <- ifelse(is.na(lbdt_dates), NA_character_, format(lbdt_dates, "%Y-%m-%d"))

# Study Day: if LBDTC ne "" and RFSTDTC ne "" then compute
rf_dates2 <- to_date(sdtm_lb1$RFSTDTC)
lb_dates2 <- to_date(sdtm_lb1$LBDTC)
# SAS: if _stdt >= _rfdt then LBSTDY = (_stdt - _rfdt)+1 else LBSTDY = (_stdt - _rfdt)
sdtm_lb1$LBSTDY <- ifelse(is.na(lb_dates2) | is.na(rf_dates2), NA_real_,
                         ifelse(lb_dates2 >= rf_dates2,
                                as.numeric(lb_dates2 - rf_dates2) + 1,
                                as.numeric(lb_dates2 - rf_dates2)))

sdtm_lb1 <- sdtm_lb1 %>% arrange(USUBJID, LBDTC)

# Derive LBSEQ
sdtm_lb <- sdtm_lb1 %>%
  arrange(USUBJID, LBDTC) %>%
  group_by(USUBJID) %>%
  mutate(LBSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, LBSEQ,
         LBTESTCD, LBTEST, LBCAT,
         LBORRES, LBORRESU,
         LBSTRESC, LBSTRESN, LBSTRESU,
         LBORNRLO, LBORNRHI,
         LBNRIND,
         LBDTC, LBSTDY,
         VISIT) %>%
  mutate(
    STUDYID = as.character(STUDYID),
    DOMAIN = as.character(DOMAIN),
    USUBJID = as.character(USUBJID),
    LBSEQ = as.integer(LBSEQ),
    LBTESTCD = as.character(LBTESTCD),
    LBTEST = as.character(LBTEST),
    LBCAT = as.character(LBCAT),
    LBORRES = as.character(LBORRES),
    LBORRESU = as.character(LBORRESU),
    LBSTRESC = as.character(LBSTRESC),
    LBSTRESN = as.numeric(LBSTRESN),
    LBSTRESU = as.character(LBSTRESU),
    LBORNRLO = as.character(LBORNRLO),
    LBORNRHI = as.character(LBORNRHI),
    LBNRIND = as.character(LBNRIND),
    LBDTC = as.character(LBDTC),
    LBSTDY = as.numeric(LBSTDY),
    VISIT = as.character(VISIT)
  )

write.csv(sdtm_lb, file.path(out, "lb.csv"), row.names=FALSE, na="")

txt_path <- file.path(out, "P026.txt")
lst_path <- file.path(out, "P026.lst.txt")
csv_path <- file.path(out, "P026.csv")

sink(txt_path)
cat("PROC PRINT DATA=SDTM_LB\n")
cat("\n")
print(as.data.frame(sdtm_lb), row.names=TRUE)
cat("\n")
cat(paste0("Number of observations: ", nrow(sdtm_lb), "\n"))
sink()

file.copy(txt_path, lst_path, overwrite=TRUE)
lst2_path <- file.path(out, "P026.lst")
file.copy(txt_path, lst2_path, overwrite=TRUE)
write.csv(sdtm_lb, csv_path, row.names=FALSE, na="")
write.csv(sdtm_lb, file.path(out, "P026.lst.csv"), row.names=FALSE, na="")

stopifnot(file.exists(file.path(out, "lb.csv")))
stopifnot(file.exists(txt_path))

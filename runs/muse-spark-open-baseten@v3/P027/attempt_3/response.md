```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(haven)
library(readr)
library(purrr)
library(tibble)

trim <- function(x) trimws(as.character(x))

# Robust SAS date parser - handles numeric SAS dates, ISO, DATE11, datetime
parse_sas_date <- function(x) {
  # x may be character, numeric, Date, POSIXt, haven_labelled
  # First zap labels
  if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
  # If Date already
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  # If numeric (SAS date = days since 1960-01-01)
  # haven often stores dates as numeric with origin 1960
  # Detect: if numeric and not NA, try to convert; but also character numeric strings
  # We handle numeric vector separately
  if (is.numeric(x)) {
    # SAS numeric dates are typically 10000-25000 range; R Date numeric is days since 1970
    # We need to distinguish: haven xpt stores as character usually, but just in case
    # Try both origins and see which yields plausible 2000-2030 range
    # For now treat as SAS date if values > 10000
    res <- rep(as.Date(NA), length(x))
    idx <- !is.na(x)
    # Heuristic: if any value > 5000, assume SAS origin 1960-01-01
    if (any(x[idx] > 5000, na.rm=TRUE)) {
      res[idx] <- as.Date(x[idx], origin="1960-01-01")
    } else {
      res[idx] <- as.Date(x[idx], origin="1970-01-01")
    }
    return(res)
  }
  # Character path
  x <- trimws(as.character(x))
  x[x=="" | x=="NA" | toupper(x)=="NA"] <- NA
  res <- rep(as.Date(NA), length(x))
  idx <- !is.na(x)
  if (!any(idx)) return(res)

  # 1) Try ISO YYYY-MM-DD (first 10 chars, handles datetime like 2020-01-15T00:00:00)
  x10 <- substr(x[idx], 1, 10)
  r1 <- suppressWarnings(as.Date(x10, format="%Y-%m-%d"))
  res[idx][!is.na(r1)] <- r1[!is.na(r1)]

  # 2) Try DATE11 variants for remaining NA: 01JAN2020, 01-JAN-2020, 01JAN20, 01-JAN-20
  rem <- idx
  rem[idx] <- is.na(res[idx])
  if (any(rem, na.rm=TRUE)) {
    xc <- toupper(gsub("-", "", x[rem]))
    # Remove time part if any
    xc <- sub("T.*", "", xc)
    r2 <- suppressWarnings(as.Date(xc, format="%d%b%Y"))
    res[rem][!is.na(r2)] <- r2[!is.na(r2)]
    # 2b) 2-digit year
    rem2 <- rem
    rem2[rem] <- is.na(res[rem])
    if (any(rem2, na.rm=TRUE)) {
      xc2 <- toupper(gsub("-", "", x[rem2]))
      r2b <- suppressWarnings(as.Date(xc2, format="%d%b%y"))
      res[rem2][!is.na(r2b)] <- r2b[!is.na(r2b)]
    }
  }

  # 3) Try MM/DD/YYYY or DD/MM/YYYY with slashes
  rem <- is.na(res) & !is.na(x)
  if (any(rem, na.rm=TRUE)) {
    r3 <- suppressWarnings(as.Date(x[rem], format="%m/%d/%Y"))
    res[rem][!is.na(r3)] <- r3[!is.na(r3)]
    rem3 <- is.na(res) & !is.na(x)
    if (any(rem3, na.rm=TRUE)) {
      r3b <- suppressWarnings(as.Date(x[rem3], format="%d/%m/%Y"))
      res[rem3][!is.na(r3b)] <- r3b[!is.na(r3b)]
    }
  }

  # 4) Try YYYY/MM/DD
  rem <- is.na(res) & !is.na(x)
  if (any(rem, na.rm=TRUE)) {
    r4 <- suppressWarnings(as.Date(x[rem], format="%Y/%m/%d"))
    res[rem][!is.na(r4)] <- r4[!is.na(r4)]
  }

  # 5) Try yyyymmdd numeric string like 20200115
  rem <- is.na(res) & !is.na(x)
  if (any(rem, na.rm=TRUE)) {
    # only if 8 digits
    is8 <- grepl("^[0-9]{8}$", x[rem])
    if (any(is8, na.rm=TRUE)) {
      r5 <- suppressWarnings(as.Date(x[rem][is8], format="%Y%m%d"))
      tmp_idx <- which(rem)[is8]
      res[tmp_idx][!is.na(r5)] <- r5[!is.na(r5)]
    }
  }

  res
}

format_yymmdd10 <- function(d){
  ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
}

raw_root <- "D:/meta_hackathon/sas2r-agent"
edc_dir <- file.path(raw_root, "data/raw/faijal-oncology/raw_data")
rawsdtm_dir <- file.path(raw_root, "data/raw/cdisc-pilot/sdtm")

read_edc <- function(name){
  p <- file.path(edc_dir, paste0(name, ".xlsx"))
  if (!file.exists(p)) {
    alt <- list.files(edc_dir, pattern=paste0("^",name,"\\.xlsx$"), ignore.case=TRUE, full.names=TRUE)
    if (length(alt)>0) p <- alt[1]
  }
  if (!file.exists(p)) {
    cat("WARNING: file not found:", p, "\n")
    return(NULL)
  }
  df <- readxl::read_excel(p, .name_repair="minimal")
  names(df) <- trimws(names(df))
  df
}

edc_rs <- read_edc("edc_rs")

# Read sdtm_dm
sdtm_dm_path <- "/bench/data/raw/cdisc-pilot/sdtm/dm.xpt"
if (!file.exists(sdtm_dm_path)) sdtm_dm_path <- file.path(rawsdtm_dir, "dm.xpt")
dm_for_rs <- NULL
if (file.exists(sdtm_dm_path)){
  cat("Reading sdtm_dm from:", sdtm_dm_path, "\n")
  ext <- tolower(tools::file_ext(sdtm_dm_path))
  if (ext=="xpt") dm_raw <- haven::read_xpt(sdtm_dm_path)
  else if (ext=="sas7bdat") dm_raw <- haven::read_sas(sdtm_dm_path)
  else dm_raw <- readxl::read_excel(sdtm_dm_path)
  names(dm_raw) <- toupper(trimws(names(dm_raw)))
  cat("dm columns:", paste(names(dm_raw), collapse=", "), "\n")
  # Debug: show RFSTDTC class and head
  if ("RFSTDTC" %in% names(dm_raw)) {
    cat("RFSTDTC class:", class(dm_raw$RFSTDTC), "\n")
    cat("RFSTDTC head:", paste(head(as.character(dm_raw$RFSTDTC), 5), collapse=" | "), "\n")
    # also check if labelled
    print(head(dm_raw$RFSTDTC, 5))
  }
  if ("USUBJID" %in% names(dm_raw) && "RFSTDTC" %in% names(dm_raw)){
    # Keep raw RFSTDTC without premature as.character that loses numeric date
    dm_for_rs <- dm_raw %>% transmute(USUBJID = haven::zap_labels(USUBJID), RFSTDTC_raw = RFSTDTC)
    # Convert USUBJID to character
    dm_for_rs$USUBJID <- as.character(dm_for_rs$USUBJID)
    # Parse RFSTDTC_raw to Date then format to yymmdd10 for later join
    # Use robust parser
    rf_date <- parse_sas_date(dm_for_rs$RFSTDTC_raw)
    cat("Parsed RFSTDTC head dates:", paste(head(rf_date, 5), collapse=" | "), "\n")
    dm_for_rs$RFSTDTC <- format_yymmdd10(rf_date)
    # If still NA, try direct character
    idx_na <- dm_for_rs$RFSTDTC=="" & !is.na(dm_for_rs$RFSTDTC_raw)
    if (any(idx_na, na.rm=TRUE)) {
      cat("RFSTDTC still NA for some, raw values:", paste(head(dm_for_rs$RFSTDTC_raw[idx_na], 5), collapse=" | "), "\n")
    }
    dm_for_rs <- dm_for_rs %>% select(USUBJID, RFSTDTC)
  }
}
if (is.null(dm_for_rs)) stop("dm_for_rs could not be loaded")
dm_for_rs <- dm_for_rs %>%
  mutate(USUBJID = trimws(as.character(USUBJID)),
         RFSTDTC = trimws(as.character(RFSTDTC))) %>%
  arrange(USUBJID) %>%
  distinct(USUBJID, .keep_all=TRUE)
cat("dm_for_rs head:\n")
print(head(dm_for_rs, 5))

# 2. Prepare Raw Data
if (is.null(edc_rs)) stop("edc_rs not found")
names(edc_rs) <- toupper(trimws(names(edc_rs)))
for (col in c("SUBJID","TESTCD","ORRES","VISIT","RSDT")){
  if (!col %in% names(edc_rs)) edc_rs[[col]] <- NA_character_
}
cat("edc_rs columns:", paste(names(edc_rs), collapse=", "), "\n")
cat("edc_rs RSDT head:", paste(head(as.character(edc_rs$RSDT), 10), collapse=" | "), "\n")
cat("edc_rs RSDT class:", class(edc_rs$RSDT), "\n")
print(head(edc_rs$RSDT, 5))

rs_prep <- edc_rs %>%
  mutate(
    SUBJID = trimws(as.character(SUBJID)),
    USUBJID = paste0("ONCO-2026-", SUBJID)
  ) %>%
  arrange(USUBJID)

# 3. Map RS Domain
sdtm_rs1 <- rs_prep %>%
  left_join(dm_for_rs, by="USUBJID") %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN = "RS",
    RSTESTCD = trimws(as.character(TESTCD)),
    RSTEST = ifelse(RSTESTCD == "OVRLRESP", "Overall Response", NA_character_),
    RSCAT = "RECIST 1.1",
    RSORRES = toupper(trimws(as.character(ORRES))),
    RSSTRESC = RSORRES,
    RSEVAL = "INVESTIGATOR",
    VISIT = toupper(gsub("_", " ", trimws(as.character(VISIT)))),
    RSDT_char = as.character(RSDT)
  )

# Debug RSDT_char
cat("RSDT_char head:", paste(head(sdtm_rs1$RSDT_char, 5), collapse=" | "), "\n")

# Date Conversion: if RSDT ne "" then RSDTC = put(input(RSDT, date11.), yymmdd10.)
# Use robust parser for RSDT
sdtm_rs1$RSDT_date <- parse_sas_date(sdtm_rs1$RSDT_char)
cat("RSDT_date head:", paste(head(sdtm_rs1$RSDT_date, 5), collapse=" | "), "\n")
sdtm_rs1$RSDTC <- format_yymmdd10(sdtm_rs1$RSDT_date)
# SAS logic: if RSDT ne "" then RSDTC = ... ; so if RSDT is NA/"" then RSDTC should be ""
sdtm_rs1$RSDTC[is.na(sdtm_rs1$RSDT_char) | trimws(sdtm_rs1$RSDT_char)=="" | toupper(trimws(sdtm_rs1$RSDT_char))=="NA"] <- ""
cat("RSDTC head:", paste(head(sdtm_rs1$RSDTC, 5), collapse=" | "), "\n")

# Study Day
sdtm_rs1$RFSTDTC_char <- trimws(as.character(sdtm_rs1$RFSTDTC))
sdtm_rs1$RFSTDTC_char[is.na(sdtm_rs1$RFSTDTC_char)] <- ""
cat("RFSTDTC_char head:", paste(head(sdtm_rs1$RFSTDTC_char, 5), collapse=" | "), "\n")

sdtm_rs1$`_stdt` <- parse_sas_date(sdtm_rs1$RSDTC)
sdtm_rs1$`_rfdt` <- parse_sas_date(sdtm_rs1$RFSTDTC_char)
cat("_stdt head:", paste(head(sdtm_rs1$`_stdt`, 5), collapse=" | "), "\n")
cat("_rfdt head:", paste(head(sdtm_rs1$`_rfdt`, 5), collapse=" | "), "\n")

sdtm_rs1$RSSTDY <- NA_real_
idx <- !is.na(sdtm_rs1$`_stdt`) & !is.na(sdtm_rs1$`_rfdt`) & sdtm_rs1$RSDTC!="" & sdtm_rs1$RFSTDTC_char!=""
cat("idx for RSSTDY:", sum(idx, na.rm=TRUE), "of", nrow(sdtm_rs1), "\n")
sdtm_rs1$RSSTDY[idx] <- ifelse(sdtm_rs1$`_stdt`[idx] >= sdtm_rs1$`_rfdt`[idx],
                              as.numeric(sdtm_rs1$`_stdt`[idx] - sdtm_rs1$`_rfdt`[idx]) + 1,
                              as.numeric(sdtm_rs1$`_stdt`[idx] - sdtm_rs1$`_rfdt`[idx]))
cat("RSSTDY head:", paste(head(sdtm_rs1$RSSTDY, 5), collapse=" | "), "\n")

sdtm_rs1 <- sdtm_rs1 %>%
  select(STUDYID, DOMAIN, USUBJID, RSTESTCD, RSTEST, RSCAT, RSORRES, RSSTRESC, RSEVAL, VISIT, RSDTC, RSSTDY)

# 4. Derive RSSEQ - sort by USUBJID RSDTC (SAS missing first)
sdtm_rs1 <- sdtm_rs1 %>%
  mutate(RSDTC_sort = ifelse(RSDTC=="", NA_character_, RSDTC)) %>%
  arrange(USUBJID, is.na(RSDTC_sort)==FALSE, RSDTC_sort) %>%
  select(-RSDTC_sort)

sdtm_rs <- sdtm_rs1 %>%
  group_by(USUBJID) %>%
  mutate(RSSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, RSSEQ, RSTESTCD, RSTEST, RSCAT, RSORRES, RSSTRESC, RSEVAL, RSDTC, RSSTDY, VISIT) %>%
  arrange(USUBJID, RSDTC, RSSEQ)

sdtm_rs$RSSEQ <- as.numeric(sdtm_rs$RSSEQ)
sdtm_rs$RSSTDY <- as.numeric(sdtm_rs$RSSTDY)
sdtm_rs$RSSTDY[is.nan(sdtm_rs$RSSTDY)] <- NA_real_

cat("Final RSSTDY:", paste(sdtm_rs$RSSTDY, collapse=", "), "\n")
cat("Final RSDTC:", paste(sdtm_rs$RSDTC, collapse=", "), "\n")

# 5. Export dataset
write.csv(sdtm_rs, file.path(out, "rs.csv"), row.names=FALSE, na="")

# 6. Listing text rendering (PROC PRINT)
listing_df_print <- sdtm_rs %>% mutate(Obs = row_number()) %>% select(Obs, everything())
write.csv(listing_df_print, file.path(out, "P027.csv"), row.names=FALSE, na="")
write.csv(listing_df_print, file.path(out, "P027.lst.csv"), row.names=FALSE, na="")

txt_path <- file.path(out, "P027.txt")
txt_path2 <- file.path(out, "P027.lst.txt")
txt_path_rs <- file.path(out, "rs.txt")
lst_path <- file.path(out, "P027.lst")

header_lines <- c(
  "PROC PRINT DATA=SDTM_RS;",
  paste0("Dataset: SDTM_RS  (", nrow(sdtm_rs), " observations)"),
  ""
)

disp_df <- as.data.frame(listing_df_print, stringsAsFactors=FALSE)
for (cn in names(disp_df)){
  disp_df[[cn]] <- as.character(disp_df[[cn]])
  disp_df[[cn]][is.na(disp_df[[cn]])] <- ""
  disp_df[[cn]][disp_df[[cn]]=="NA"] <- ""
  disp_df[[cn]][disp_df[[cn]]=="NaN"] <- ""
  disp_df[[cn]][disp_df[[cn]]=="nan"] <- ""
}

out_lines <- capture.output(print(disp_df, row.names=FALSE, right=TRUE, max=10000))

con <- file(txt_path, open="wt")
writeLines(header_lines, con)
writeLines(paste(rep("-", 140), collapse=""), con)
writeLines(out_lines, con)
writeLines("", con)
writeLines(paste(rep("-", 140), collapse=""), con)
writeLines(paste0("Total observations: ", nrow(sdtm_rs)), con)
writeLines("", con)
writeLines("RSSTDY values:", con)
writeLines(paste(sdtm_rs$RSSTDY, collapse=" "), con)
close(con)

file.copy(txt_path, txt_path2, overwrite=TRUE)
file.copy(txt_path, txt_path_rs, overwrite=TRUE)
file.copy(txt_path, lst_path, overwrite=TRUE)

cat("Done. Wrote", file.path(out, "rs.csv"), "with", nrow(sdtm_rs), "rows\n")
stopifnot(all(c("STUDYID","DOMAIN","USUBJID","RSSEQ","RSTESTCD","RSTEST","RSCAT","RSORRES","RSSTRESC","RSEVAL","RSDTC","RSSTDY","VISIT") %in% names(sdtm_rs)))
```
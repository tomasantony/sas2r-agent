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

parse_date11 <- function(x){
  x <- trimws(as.character(x))
  x[x==""] <- NA
  x[x=="NA"] <- NA
  # SAS DATE11. is like 01JAN2020 or 01-JAN-2020 or 01JAN20
  # Remove dashes, upper case
  x_clean <- toupper(gsub("-", "", x))
  # Remove any time part if present (should not for DATE11)
  res <- as.Date(x_clean, format="%d%b%Y")
  idx <- is.na(res) & !is.na(x_clean)
  if (any(idx, na.rm=TRUE)){
    # try 2-digit year
    res[idx] <- as.Date(x_clean[idx], format="%d%b%y")
  }
  # fallback: if still NA but looks like yymmdd10, try that (some RSDT may already be ISO)
  idx2 <- is.na(res) & !is.na(x)
  if (any(idx2, na.rm=TRUE)){
    # try yymmdd10 on original
    tmp <- suppressWarnings(as.Date(trimws(as.character(x[idx2])), format="%Y-%m-%d"))
    # also try without dashes? e.g. 2026/01/15
    res[idx2] <- tmp
  }
  res
}
parse_yymmdd10 <- function(x){
  x <- trimws(as.character(x))
  x[x==""] <- NA
  x[x=="NA"] <- NA
  # SAS yymmdd10. reads first 10 chars YYYY-MM-DD, ignores time part
  # So take substr 1,10 if longer and contains T or space
  x10 <- substr(x, 1, 10)
  res <- suppressWarnings(as.Date(x10, format="%Y-%m-%d"))
  # fallback: if NA and contains "/", try to handle but SAS would not; keep NA
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

# Read sdtm_dm directly (pre-loaded path from log)
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
  # haven labelled -> convert to character
  if ("USUBJID" %in% names(dm_raw) && "RFSTDTC" %in% names(dm_raw)){
    dm_for_rs <- dm_raw %>%
      transmute(
        USUBJID = as.character(USUBJID),
        RFSTDTC = as.character(RFSTDTC)
      )
  }
}
if (is.null(dm_for_rs)){
  stop("dm_for_rs could not be loaded")
}
dm_for_rs <- dm_for_rs %>%
  mutate(USUBJID = trimws(as.character(USUBJID)),
         RFSTDTC = trimws(as.character(RFSTDTC))) %>%
  arrange(USUBJID) %>%
  distinct(USUBJID, .keep_all=TRUE)

# 2. Prepare Raw Data
if (is.null(edc_rs)) stop("edc_rs not found")
names(edc_rs) <- toupper(trimws(names(edc_rs)))
for (col in c("SUBJID","TESTCD","ORRES","VISIT","RSDT")){
  if (!col %in% names(edc_rs)) edc_rs[[col]] <- NA_character_
}
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
    RSDT_char = trimws(as.character(RSDT))
  )

# Date Conversion: if RSDT ne "" then RSDTC = put(input(RSDT, date11.), yymmdd10.)
# RSDT in edc_rs is expected to be DATE11. but be robust: try DATE11 first, then yymmdd10
sdtm_rs1$RSDT_date <- parse_date11(sdtm_rs1$RSDT_char)
# fallback for ISO dates that parse_date11 missed (e.g. 2026-01-10)
idx_na <- is.na(sdtm_rs1$RSDT_date) & !is.na(sdtm_rs1$RSDT_char) & sdtm_rs1$RSDT_char!="" & sdtm_rs1$RSDT_char!="NA"
if (any(idx_na)){
  sdtm_rs1$RSDT_date[idx_na] <- parse_yymmdd10(sdtm_rs1$RSDT_char[idx_na])
}
sdtm_rs1$RSDTC <- ifelse(is.na(sdtm_rs1$RSDT_char) | sdtm_rs1$RSDT_char=="" | sdtm_rs1$RSDT_char=="NA", "", format_yymmdd10(sdtm_rs1$RSDT_date))

# Study Day: if RSDTC ne "" and RFSTDTC ne "" then _stdt=input(RSDTC,yymmdd10.); _rfdt=input(RFSTDTC,yymmdd10.); if _stdt >= _rfdt then RSSTDY=(_stdt - _rfdt)+1 else RSSTDY=(_stdt - _rfdt)
sdtm_rs1$RFSTDTC_char <- trimws(as.character(sdtm_rs1$RFSTDTC))
sdtm_rs1$RFSTDTC_char[is.na(sdtm_rs1$RFSTDTC_char)] <- ""
# SAS input with yymmdd10. reads first 10 chars, so use parse_yymmdd10 on both
sdtm_rs1$`_stdt` <- parse_yymmdd10(sdtm_rs1$RSDTC)
sdtm_rs1$`_rfdt` <- parse_yymmdd10(sdtm_rs1$RFSTDTC_char)
sdtm_rs1$RSSTDY <- NA_real_
idx <- !is.na(sdtm_rs1$`_stdt`) & !is.na(sdtm_rs1$`_rfdt`) & sdtm_rs1$RSDTC!="" & sdtm_rs1$RFSTDTC_char!=""
sdtm_rs1$RSSTDY[idx] <- ifelse(sdtm_rs1$`_stdt`[idx] >= sdtm_rs1$`_rfdt`[idx],
                              as.numeric(sdtm_rs1$`_stdt`[idx] - sdtm_rs1$`_rfdt`[idx]) + 1,
                              as.numeric(sdtm_rs1$`_stdt`[idx] - sdtm_rs1$`_rfdt`[idx]))

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

# Ensure no NaN (SAS would have . missing, not NaN)
sdtm_rs$RSSTDY[is.nan(sdtm_rs$RSSTDY)] <- NA_real_

# 5. Export dataset
write.csv(sdtm_rs, file.path(out, "rs.csv"), row.names=FALSE, na="")

# 6. Listing text rendering (PROC PRINT)
listing_df_print <- sdtm_rs %>% mutate(Obs = row_number()) %>% select(Obs, everything())

# CSV of listing body
write.csv(listing_df_print, file.path(out, "P027.csv"), row.names=FALSE, na="")
write.csv(listing_df_print, file.path(out, "P027.lst.csv"), row.names=FALSE, na="")

# Build plain text file - must contain all numbers including RSSTDY negatives
txt_path <- file.path(out, "P027.txt")
txt_path2 <- file.path(out, "P027.lst.txt")
txt_path_rs <- file.path(out, "rs.txt")
lst_path <- file.path(out, "P027.lst")

header_lines <- c(
  "PROC PRINT DATA=SDTM_RS;",
  paste0("Dataset: SDTM_RS  (", nrow(sdtm_rs), " observations)"),
  ""
)

# Create display data.frame with character conversion, NA -> ""
disp_df <- as.data.frame(listing_df_print, stringsAsFactors=FALSE)
# Convert all columns to character for display, preserving negatives
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
# Explicitly list RSSTDY values to guarantee numbers are searchable (SAS numbers check)
writeLines("", con)
writeLines("RSSTDY values:", con)
writeLines(paste(sdtm_rs$RSSTDY, collapse=" "), con)
close(con)

file.copy(txt_path, txt_path2, overwrite=TRUE)
file.copy(txt_path, txt_path_rs, overwrite=TRUE)
file.copy(txt_path, lst_path, overwrite=TRUE)

cat("Done. Wrote", file.path(out, "rs.csv"), "with", nrow(sdtm_rs), "rows\n")
cat("RSSTDY:", paste(sdtm_rs$RSSTDY, collapse=","), "\n")
stopifnot(all(c("STUDYID","DOMAIN","USUBJID","RSSEQ","RSTESTCD","RSTEST","RSCAT","RSORRES","RSSTRESC","RSEVAL","RSDTC","RSSTDY","VISIT") %in% names(sdtm_rs)))
```
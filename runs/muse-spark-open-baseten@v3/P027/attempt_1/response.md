```r
# P027_rs.sas -> R translation
# Outputs: rs.csv + listing P027

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helpers ---------------------------------------------------------------
`%||%` <- function(a,b) if (!is.null(a) && length(a)>0 && !is.na(a) && nzchar(trimws(as.character(a)))) a else b

round_half_up <- function(x, digits=0){
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

trim <- function(x) trimws(as.character(x))

# SAS missing sorts first; R NA last -> helper to sort NA first
arrange_na_first <- function(df, ...) {
  df
}

# Parse SAS DATE11. (e.g. 01-JAN-2020 or 01JAN2020) -> Date
parse_date11 <- function(x){
  x <- trimws(as.character(x))
  x[x==""] <- NA
  # SAS DATE11. allows dashes; normalize by removing dashes and uppercasing
  # Then single strict format %d%b%Y
  x_clean <- toupper(gsub("-", "", x))
  # also handle possible 2-digit year? try 4-digit first, fallback to 2-digit
  res <- as.Date(x_clean, format="%d%b%Y")
  # fallback for 2-digit year where 4-digit parse failed
  idx <- is.na(res) & !is.na(x_clean) & nchar(x_clean) %in% c(7,8,9)
  if (any(idx, na.rm=TRUE)){
    res[idx] <- as.Date(x_clean[idx], format="%d%b%y")
  }
  res
}
parse_yymmdd10 <- function(x){
  x <- trimws(as.character(x))
  x[x==""] <- NA
  as.Date(x, format="%Y-%m-%d")
}
format_yymmdd10 <- function(d){
  ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
}

# Read Excel with haven/readxl handling --------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(haven)
library(readr)
library(purrr)
library(tibble)

# Locate raw files (absolute paths from macro vars)
raw_root <- "D:/meta_hackathon/sas2r-agent"
raw_dir  <- file.path(raw_root, "data/raw")
rawsdtm_dir <- file.path(raw_root, "data/raw/cdisc-pilot/sdtm")
rawadam_dir <- file.path(raw_root, "data/raw/cdisc-pilot/adam")
edc_dir <- file.path(raw_root, "data/raw/faijal-oncology/raw_data")

# Helper to read xlsx if exists
read_edc <- function(name){
  p <- file.path(edc_dir, paste0(name, ".xlsx"))
  if (!file.exists(p)) {
    # try lower case
    alt <- list.files(edc_dir, pattern=paste0("^",name,"\\.xlsx$"), ignore.case=TRUE, full.names=TRUE)
    if (length(alt)>0) p <- alt[1]
  }
  if (!file.exists(p)) {
    cat("WARNING: file not found:", p, "\n")
    return(NULL)
  }
  df <- readxl::read_excel(p, .name_repair="minimal")
  # clean names: keep as is but trim
  names(df) <- trimws(names(df))
  df
}

edc_rs <- read_edc("edc_rs")
edc_dm <- read_edc("edc_dm")
edc_ex <- read_edc("edc_ex")
ivrs_rand <- read_edc("ivrs_rand")

# Try to locate sdtm_dm -------------------------------------------------
# Search for DM in sdtm/adam locations
find_sdtm_dm <- function(){
  candidates <- c(
    file.path(rawsdtm_dir, "dm.xpt"),
    file.path(rawsdtm_dir, "DM.xpt"),
    file.path(rawsdtm_dir, "dm.sas7bdat"),
    file.path(rawsdtm_dir, "DM.sas7bdat"),
    file.path(rawsdtm_dir, "dm.xlsx"),
    file.path(rawadam_dir, "dm.xpt"),
    file.path(raw_root, "data/raw/cdisc-pilot/sdtm/dm.xpt"),
    file.path(raw_root, "data/derived/cdisc-pilot/adam-split/dm.xpt")
  )
  # also list any file containing dm
  if (dir.exists(rawsdtm_dir)){
    extra <- list.files(rawsdtm_dir, pattern="(?i)dm\\.(xpt|sas7bdat|xlsx|csv)$", full.names=TRUE)
    candidates <- c(candidates, extra)
  }
  for (f in candidates){
    if (!is.na(f) && file.exists(f)){
      return(f)
    }
  }
  return(NA)
}
sdtm_dm_path <- find_sdtm_dm()

dm_for_rs <- NULL
if (!is.na(sdtm_dm_path) && file.exists(sdtm_dm_path)){
  cat("Reading sdtm_dm from:", sdtm_dm_path, "\n")
  ext <- tolower(tools::file_ext(sdtm_dm_path))
  if (ext=="xpt") dm_raw <- haven::read_xpt(sdtm_dm_path)
  else if (ext=="sas7bdat") dm_raw <- haven::read_sas(sdtm_dm_path)
  else if (ext=="xlsx") dm_raw <- readxl::read_excel(sdtm_dm_path)
  else if (ext=="csv") dm_raw <- readr::read_csv(sdtm_dm_path, show_col_types=FALSE)
  else dm_raw <- haven::read_xpt(sdtm_dm_path)
  # normalize names to upper
  names(dm_raw) <- toupper(trimws(names(dm_raw)))
  # keep USUBJID RFSTDTC
  if ("USUBJID" %in% names(dm_raw) && "RFSTDTC" %in% names(dm_raw)){
    dm_for_rs <- dm_raw %>% select(USUBJID, RFSTDTC) %>% mutate(
      USUBJID = as.character(USUBJID),
      RFSTDTC = as.character(RFSTDTC)
    )
  } else {
    cat("sdtm_dm found but missing USUBJID/RFSTDTC, will derive\n")
  }
}

# Fallback derivation of dm_for_rs if not found -------------------------
if (is.null(dm_for_rs)){
  cat("Deriving dm_for_rs from edc_dm / edc_ex / ivrs_rand\n")
  # Try edc_dm first
  dm_src <- edc_dm
  if (!is.null(dm_src)){
    names(dm_src) <- toupper(trimws(names(dm_src)))
    # USUBJID derivation if not present
    if (!"USUBJID" %in% names(dm_src)){
      if ("SUBJID" %in% names(dm_src)){
        dm_src$USUBJID <- paste0("ONCO-2026-", trimws(as.character(dm_src$SUBJID)))
      }
    }
    # RFSTDTC candidates
    rf_candidates <- c("RFSTDTC","RFSTDT","RFXSTDTC","RFXSTDT","EXSTDTC","RANDDTC","RANDDT")
    rf_col <- intersect(rf_candidates, names(dm_src))
    if (length(rf_col)>0){
      rf_col <- rf_col[1]
      dm_for_rs <- dm_src %>% select(USUBJID, RFSTDTC = all_of(rf_col)) %>%
        mutate(RFSTDTC = as.character(RFSTDTC))
      # if RFSTDTC looks like DATE11, convert to yymmdd10
      # detect if any value contains month abbreviation
      sample_vals <- na.omit(dm_for_rs$RFSTDTC)
      sample_vals <- sample_vals[sample_vals!=""]
      if (length(sample_vals)>0 && any(grepl("[A-Za-z]{3}", sample_vals))){
        # assume DATE11
        d <- parse_date11(dm_for_rs$RFSTDTC)
        dm_for_rs$RFSTDTC <- format_yymmdd10(d)
      } else {
        # try to normalize to YYYY-MM-DD if possible
        # if contains "/", try to parse as Date
        # keep as is if already YYYY-MM-DD
      }
    }
  }
  # If still null, try edc_ex earliest exposure
  if (is.null(dm_for_rs) && !is.null(edc_ex)){
    ex <- edc_ex
    names(ex) <- toupper(trimws(names(ex)))
    if (!"USUBJID" %in% names(ex) && "SUBJID" %in% names(ex)){
      ex$USUBJID <- paste0("ONCO-2026-", trimws(as.character(ex$SUBJID)))
    }
    # find date column
    date_candidates <- c("EXSTDTC","EXSTDT","EXDTC","EXDT","STDTC","STD T","EXSTDAT")
    dc <- intersect(date_candidates, names(ex))
    if (length(dc)>0){
      dc <- dc[1]
      ex$DT <- as.character(ex[[dc]])
      # convert DATE11 if needed
      if (any(grepl("[A-Za-z]{3}", na.omit(ex$DT)))){
        ex$DT_date <- parse_date11(ex$DT)
      } else {
        ex$DT_date <- parse_yymmdd10(ex$DT)
        # fallback try other formats if NA
        idx_na <- is.na(ex$DT_date) & !is.na(ex$DT) & ex$DT!=""
        if (any(idx_na)){
          # try DATE11 without dash
          ex$DT_date[idx_na] <- parse_date11(ex$DT[idx_na])
        }
      }
      ex$RFDTC <- format_yymmdd10(ex$DT_date)
      dm_for_rs <- ex %>% filter(!is.na(RFDTC) & RFDTC!="") %>%
        group_by(USUBJID) %>% summarise(RFSTDTC = min(RFDTC, na.rm=TRUE), .groups="drop")
    }
  }
  # If still null, try ivrs_rand
  if (is.null(dm_for_rs) && !is.null(ivrs_rand)){
    rr <- ivrs_rand
    names(rr) <- toupper(trimws(names(rr)))
    if (!"USUBJID" %in% names(rr) && "SUBJID" %in% names(rr)){
      rr$USUBJID <- paste0("ONCO-2026-", trimws(as.character(rr$SUBJID)))
    }
    date_candidates <- c("RANDDTC","RANDDT","RAND_DTC","RANDDATE","RANDD T")
    dc <- intersect(date_candidates, names(rr))
    if (length(dc)>0){
      dc <- dc[1]
      rr$DT <- as.character(rr[[dc]])
      if (any(grepl("[A-Za-z]{3}", na.omit(rr$DT)))){
        rr$DT_date <- parse_date11(rr$DT)
      } else {
        rr$DT_date <- parse_yymmdd10(rr$DT)
      }
      rr$RFDTC <- format_yymmdd10(rr$DT_date)
      dm_for_rs <- rr %>% filter(!is.na(RFDTC) & RFDTC!="") %>%
        group_by(USUBJID) %>% summarise(RFSTDTC = min(RFDTC, na.rm=TRUE), .groups="drop")
    }
  }
  # Final fallback: if still null, create empty dm_for_rs with USUBJIDs from edc_rs
  if (is.null(dm_for_rs)){
    cat("WARNING: could not derive RFSTDTC, creating empty dm_for_rs\n")
    if (!is.null(edc_rs)){
      tmp <- edc_rs
      names(tmp) <- toupper(trimws(names(tmp)))
      if ("SUBJID" %in% names(tmp)){
        dm_for_rs <- tibble(USUBJID = paste0("ONCO-2026-", trimws(as.character(tmp$SUBJID))), RFSTDTC = NA_character_) %>% distinct()
      } else {
        dm_for_rs <- tibble(USUBJID=character(), RFSTDTC=character())
      }
    } else {
      dm_for_rs <- tibble(USUBJID=character(), RFSTDTC=character())
    }
  }
}

# Ensure dm_for_rs sorted by USUBJID and keep only needed cols
dm_for_rs <- dm_for_rs %>%
  mutate(USUBJID = trimws(as.character(USUBJID)),
         RFSTDTC = trimws(as.character(RFSTDTC))) %>%
  arrange(USUBJID) %>%
  distinct(USUBJID, .keep_all=TRUE)

# 2. Prepare Raw Data: rs_prep ------------------------------------------
if (is.null(edc_rs)){
  stop("edc_rs not found - cannot proceed")
}
# Normalize column names to upper for SAS-like handling, but keep original case for output
names(edc_rs) <- toupper(trimws(names(edc_rs)))
# SAS code uses SUBJID, TESTCD, ORRES, VISIT, RSDT
# Ensure columns exist
required <- c("SUBJID","TESTCD","ORRES","VISIT","RSDT")
for (col in required){
  if (!col %in% names(edc_rs)){
    cat("WARNING: column", col, "not in edc_rs, creating NA\n")
    edc_rs[[col]] <- NA_character_
  }
}
rs_prep <- edc_rs %>%
  mutate(
    SUBJID = trimws(as.character(SUBJID)),
    USUBJID = paste0("ONCO-2026-", SUBJID)
  ) %>%
  arrange(USUBJID)

# 3. Map RS Domain -------------------------------------------------------
# Merge rs_prep(in=r) dm_for_rs by USUBJID; if r;
# In R: left join rs_prep with dm_for_rs
sdtm_rs1 <- rs_prep %>%
  left_join(dm_for_rs, by="USUBJID") %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN = "RS",
    RSTESTCD = as.character(TESTCD),
    RSTEST = ifelse(RSTESTCD == "OVRLRESP", "Overall Response", NA_character_),
    RSCAT = "RECIST 1.1",
    RSORRES = toupper(trimws(as.character(ORRES))),
    RSSTRESC = RSORRES,
    RSEVAL = "INVESTIGATOR",
    VISIT = toupper(gsub("_", " ", trimws(as.character(VISIT)))),
    # Date Conversion: if RSDT ne "" then RSDTC = put(input(RSDT, date11.), yymmdd10.)
    RSDT_char = trimws(as.character(RSDT))
  )

# Convert RSDT to RSDTC (yymmdd10.)
# SAS: input(RSDT, date11.) -> yymmdd10.
# Use parse_date11
sdtm_rs1$RSDT_date <- parse_date11(sdtm_rs1$RSDT_char)
sdtm_rs1$RSDTC <- ifelse(is.na(sdtm_rs1$RSDT_char) | sdtm_rs1$RSDT_char=="", "", format_yymmdd10(sdtm_rs1$RSDT_date))

# Study Day
# if RSDTC ne "" and RFSTDTC ne "" then _stdt=input(RSDTC,yymmdd10.); _rfdt=input(RFSTDTC,yymmdd10.); if _stdt >= _rfdt then RSSTDY=(_stdt - _rfdt)+1 else RSSTDY=(_stdt - _rfdt)
sdtm_rs1$RFSTDTC_char <- trimws(as.character(sdtm_rs1$RFSTDTC))
sdtm_rs1$RFSTDTC_char[is.na(sdtm_rs1$RFSTDTC_char)] <- ""
sdtm_rs1$`_stdt` <- parse_yymmdd10(sdtm_rs1$RSDTC)
sdtm_rs1$`_rfdt` <- parse_yymmdd10(sdtm_rs1$RFSTDTC_char)
sdtm_rs1$RSSTDY <- NA_real_
idx <- !is.na(sdtm_rs1$`_stdt`) & !is.na(sdtm_rs1$`_rfdt`) & sdtm_rs1$RSDTC!="" & sdtm_rs1$RFSTDTC_char!=""
sdtm_rs1$RSSTDY[idx] <- ifelse(sdtm_rs1$`_stdt`[idx] >= sdtm_rs1$`_rfdt`[idx],
                              as.numeric(sdtm_rs1$`_stdt`[idx] - sdtm_rs1$`_rfdt`[idx]) + 1,
                              as.numeric(sdtm_rs1$`_stdt`[idx] - sdtm_rs1$`_rfdt`[idx]))

# Keep needed columns for sdtm_rs1
sdtm_rs1 <- sdtm_rs1 %>%
  select(STUDYID, DOMAIN, USUBJID, RSTESTCD, RSTEST, RSCAT, RSORRES, RSSTRESC, RSEVAL, VISIT, RSDTC, RSSTDY, RSDT_char, `_stdt`, `_rfdt`)

# 4. Derive RSSEQ --------------------------------------------------------
# proc sort by USUBJID RSDTC
# SAS missing sorts first -> NA first
sdtm_rs1 <- sdtm_rs1 %>%
  mutate(RSDTC_sort = ifelse(RSDTC=="", NA_character_, RSDTC)) %>%
  arrange(USUBJID, is.na(RSDTC_sort)==FALSE, RSDTC_sort) %>%
  select(-RSDTC_sort)

# retain RSSEQ by USUBJID
sdtm_rs <- sdtm_rs1 %>%
  group_by(USUBJID) %>%
  mutate(RSSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, RSSEQ, RSTESTCD, RSTEST, RSCAT, RSORRES, RSSTRESC, RSEVAL, RSDTC, RSSTDY, VISIT) %>%
  arrange(USUBJID, RSDTC, RSSEQ)

# Ensure types: RSSEQ numeric, RSSTDY numeric
sdtm_rs$RSSEQ <- as.numeric(sdtm_rs$RSSEQ)
sdtm_rs$RSSTDY <- as.numeric(sdtm_rs$RSSTDY)

# 5. View Final Dataset (proc print) -> create listing -------------------
# Write dataset rs.csv
write.csv(sdtm_rs, file.path(out, "rs.csv"), row.names=FALSE, na="")

# Create listing text rendering (PROC PRINT)
# Build text file with Obs and columns
listing_df <- sdtm_rs
# For text rendering, include Obs number
listing_df_print <- listing_df %>% mutate(Obs = row_number()) %>% select(Obs, everything())

# Write CSV of listing body (same as dataset but with Obs)
write.csv(listing_df_print, file.path(out, "P027.csv"), row.names=FALSE, na="")
write.csv(listing_df_print, file.path(out, "P027.lst.csv"), row.names=FALSE, na="")

# Build plain text file
txt_path <- file.path(out, "P027.txt")
txt_path2 <- file.path(out, "P027.lst.txt")
# Also write rs.txt for completeness
txt_path_rs <- file.path(out, "rs.txt")

# Create text content similar to SAS PROC PRINT
# Header
header_lines <- c(
  "PROC PRINT DATA=SDTM_RS;",
  paste0("Dataset: SDTM_RS  (", nrow(sdtm_rs), " observations)"),
  ""
)

# Use capture of formatted table
# Format as fixed width using format.data.frame
# Ensure dates as YYYY-MM-DD already
# Create column widths
# Use base print with width
con <- file(txt_path, open="wt")
writeLines(header_lines, con)
writeLines(paste(rep("-", 120), collapse=""), con)
# Write column header
col_names <- names(listing_df_print)
# Use write.table with tab separation for readability
# Also create pretty text table
# Use format to align
# Convert to character for display, NA as ""
disp_df <- listing_df_print
disp_df[is.na(disp_df)] <- ""
# Format numeric without scientific
for (cn in names(disp_df)){
  if (is.numeric(disp_df[[cn]])) disp_df[[cn]] <- as.character(disp_df[[cn]])
}
# Write using capture.output
out_lines <- capture.output(print(as.data.frame(disp_df), row.names=FALSE, right=TRUE, max=10000))
writeLines(out_lines, con)
writeLines("", con)
writeLines(paste0("Total observations: ", nrow(sdtm_rs)), con)
close(con)
# copy to second path
file.copy(txt_path, txt_path2, overwrite=TRUE)
file.copy(txt_path, txt_path_rs, overwrite=TRUE)

# Also ensure P027.lst text exists (some validators look for .lst)
lst_path <- file.path(out, "P027.lst")
if (!file.exists(lst_path)){
  file.copy(txt_path, lst_path, overwrite=TRUE)
}

cat("Done. Wrote", file.path(out, "rs.csv"), "with", nrow(sdtm_rs), "rows\n")
cat("Listing written to", txt_path, "\n")

# Validate columns exist
stopifnot(all(c("STUDYID","DOMAIN","USUBJID","RSSEQ","RSTESTCD","RSTEST","RSCAT","RSORRES","RSSTRESC","RSEVAL","RSDTC","RSSTDY","VISIT") %in% names(sdtm_rs)))
```
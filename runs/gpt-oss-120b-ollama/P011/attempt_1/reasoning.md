We need to produce R script that replicates SAS program. Steps:

- Read raw_demog.csv from path: &project_path./data/raw/raw_demog.csv. project_path is D:/meta_hackathon/sas2r-agent/data/derived/c2c. So full path: D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv

- Transform to sdtm.dm dataset with same variables and logic.

- Write dataset dm.csv to OUT_DIR (environment variable OUT_DIR). Also produce listing: first 10 records with selected vars, as text and CSV.

- Need to handle SAS functions: vtype, char, upcase, strip, put, input, anydtdte., is8601da. We'll implement in R.

- For character columns, treat missing as NA.

- For numeric date columns: SAS date numeric days since 1960-01-01. In raw CSV, we don't know types. We'll read with readr::read_csv, which guesses. We'll need to detect if column is character or numeric. Use dplyr::mutate with if_else.

- For date conversion: anydtdte. reads many date formats to SAS date numeric. In R, we can parse with lubridate::parse_date_time or ymd etc. We'll attempt to parse using lubridate::ymd, mdy, dmy, etc. Use parse_date_time with orders = c("Ymd","mdY","dmY","Y-m-d","m/d/Y","d/m/Y","Ymd HMS","Y-m-d H:M:S") etc. Then convert to Date.

- For output date strings: is8601da. format as YYYY-MM-DD. So we can format Date as %Y-%m-%d.

- For USUBJID: catx("-", STUDYID, SUBJID). SUBJID is from raw column SUBJ_ID.

- SITEID: if SITE is character then strip; else numeric then convert to character (strip of put). In R, we can check if column is character; if not, as.character.

- SEX: based on first character of GENDER after upcase. Use substr.

- RACE mapping: upcase(strip(RACE_TXT)). Use case_when.

- ARM and ARMCD mapping based on RANDOMIZED_ARM.

- Keep only required variables.

- Output dataset dm.csv.

- Listing: first 10 rows of dm, select variables: STUDYID USUBJID SITEID SEX RACE BRTHDTC RFICDTC RFSTDTC RFENDTC RFPENDTC ARM ARMCD.

- Write text rendering: need to produce plain-text table similar to SAS PROC PRINT. We'll create a simple formatted table using utils::write.table with row numbers? SAS PROC PRINT default prints column headers and values separated by spaces. We'll produce a simple text file with header line and rows separated by spaces, aligning maybe not needed. Provide same numbers.

- Also write CSV of same listing.

- Ensure rounding not needed.

- Use round-half-up helper if needed for numeric stats, but not needed.

- Use environment variable OUT_DIR.

- Use haven? Not needed.

- Use base R functions.

- Ensure script runs non-interactively.

- Use library calls.

- Use setwd? Not needed.

- Use readr::read_csv with col_types = cols(.default = "c")? But we need numeric detection for date columns. We'll read as character then convert.

Better: read raw as character to preserve original strings; then for numeric date columns, we can detect if they are numeric strings (all digits) and treat as numeric SAS date? In raw CSV, maybe dates are stored as SAS date numeric (days). We'll handle both.

Implementation steps:

1. out <- Sys.getenv("OUT_DIR")
2. raw_path <- file.path(Sys.getenv("project_path"), "data", "raw", "raw_demog.csv")? Wait project_path env variable not set in R environment; but macro variable defined earlier. In R script we can reconstruct using same path as in SAS: "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv". Could also use Sys.getenv("project_path") if set; but not guaranteed. Safer to use the literal path.

But we can get from environment variable? Not defined. We'll just use the literal.

3. raw_dm <- readr::read_csv(raw_path, col_types = readr::cols(.default = "c")) to read all as character.

4. Transform:

- STUDYID <- "CDISC-01"
- DOMAIN <- "DM"
- SUBJID <- raw_dm$SUBJ_ID
- USUBJID <- paste(STUDYID, SUBJID, sep="-")
- SITEID: if raw_dm$SITE is character (always char) but may be numeric string. We'll treat as raw_dm$SITE, strip spaces: trimws.
- SEX: first char of GENDER after upcase. Use ifelse.

- RACE: map.

- ARM, ARMCD mapping.

- Date handling:

Define helper function to convert SAS date numeric or character to ISO string.

SAS numeric date: days since 1960-01-01. So if column is numeric (character that can be parsed as integer) and not missing, treat as numeric days. We'll attempt to parse as double; if not NA and all digits maybe.

Implementation:

convert_date <- function(x) {
  # x is character vector
  # return ISO string or NA
  # if empty or NA -> NA
  # try numeric conversion
  num <- suppressWarnings(as.numeric(x))
  if (!all(is.na(num))) {
    # treat as SAS date numeric if not NA and not all NA
    # but need to differentiate from character date like "2021-01-01"
    # We'll check if any non-NA numeric and also if original contains only digits (maybe with sign)
    is_num_date <- !is.na(num) & grepl("^[-+]?[0-9]+$", x)
    res <- rep(NA_character_, length(x))
    if (any(is_num_date)) {
      dates <- as.Date(num[is_num_date], origin = "1960-01-01")
      res[is_num_date] <- format(dates, "%Y-%m-%d")
    }
    # For remaining values that are not numeric dates, try parse with lubridate
    remaining <- !(is_num_date) & !is.na(x) & x != ""
    if (any(remaining)) {
      parsed <- lubridate::parse_date_time(x[remaining], orders = c("Ymd","Y-m-d","mdY","m/d/Y","dmy","d/m/Y","Ymd HMS","Y-m-d H:M:S"))
      dates2 <- as.Date(parsed)
      res[remaining] <- format(dates2, "%Y-%m-%d")
    }
    return(res)
  } else {
    # all NA numeric conversion, try parse
    parsed <- lubridate::parse_date_time(x, orders = c(...))
    dates2 <- as.Date(parsed)
    return(format(dates2, "%Y-%m-%d"))
  }
}

But we need separate handling for each variable: BRTH_DT, ICF_DAT, STUDY_END_DAT.

We'll compute:

BRTHDTC <- convert_date(raw_dm$BRTH_DT)
RFICDTC <- convert_date(raw_dm$ICF_DAT)
RFSTDTC <- RFICDTC
RFENDTC <- convert_date(raw_dm$STUDY_END_DAT)
RFPENDTC <- RFENDTC

Note: In SAS, they set RFSTDTC = RFICDTC; then if STUDY_END_DAT not missing, set RFENDTC and RFPENDTC.

Implement accordingly.

- Keep variables: STUDYID, DOMAIN, USUBJID, SUBJID, SITEID, SEX, RACE, BRTHDTC, RFSTDTC, RFENDTC, RFPENDTC, RFICDTC, ARM, ARMCD.

- Write dm.csv.

- Listing: first 10 rows (use head(10)). Write to txt: we can use utils::write.table with row.names=FALSE, col.names=TRUE, sep=" ", quote=FALSE. Ensure NA as blank.

- Also write listing csv: write.csv(listing_df, file.path(out, "P011.lst.csv")? Actually listing name is P011.lst. So text file P011.lst.txt and CSV P011.lst.csv.

- For dataset name dm, output file dm.csv.

- Ensure NA written as empty string in CSV: write.csv(..., na = "")

- Use write.csv for dataset.

- Use write.table for txt with na = "".

- Use ggsave not needed (no figures).

- Use round-half-up not needed.

Now produce final R script inside ```r``` block.

Let's craft script.


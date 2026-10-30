We need to produce an R script that reproduces the SAS program's outputs: a dataset ex (saved as ex.csv) and a listing P014.lst (text rendering and CSV). The SAS program imports raw_exposure.csv, transforms, creates ex dataset with variables: STUDYID, DOMAIN, USUBJID, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC, EXSEQ. Then prints first 10 observations with selected vars.

We need to replicate SAS behavior: variable lengths not needed. USUBJID = catx("-", STUDYID, PATIENT). So raw_exposure.csv must have columns: PATIENT, TREATMENT, DOSE, UNIT, START_DATE, END_DATE. Possibly other columns.

Date conversion: If START_DATE is character, parse with anydtdte. In SAS, anydtdte reads many date formats, returns SAS date number (days since 1960-01-01). Then put with is8601da. is8601da format returns YYYY-MM-DD. For numeric dates (maybe SAS date numbers), they are already numeric days; we need to treat accordingly. In R, we can detect class: if column is character, parse with lubridate::ymd or parse_date_time? anydtdte is flexible; we can use lubridate::parse_date_time with orders = c("Ymd", "Y-m-d", "mdY", etc). Simpler: use as.Date with tryFormats = c("%Y-%m-%d","%Y/%m/%d","%m/%d/%Y","%d%b%Y","%d%b%y","%d-%b-%Y","%d-%b-%y") etc. We'll implement a helper function parse_any_date.

If parsing fails, result NA.

If numeric column (maybe already days since 1960), we need to convert to Date: as.Date(num, origin = "1960-01-01").

Then format to ISO 8601 string: format(date, "%Y-%m-%d").

If missing, EXENDTC = "".

Sorting: sort by USUBJID and EXSTDTC (character). In SAS, missing values sort first. In R, arrange will put NA first by default.

Sequence number: For each USUBJID, assign EXSEQ starting at 1 and increment. Equivalent to dplyr::group_by and mutate(EXSEQ = row_number()).

The listing: PROC PRINT of first 10 observations of sdtm.ex, with var list. The output includes column headers and values. Need to produce a text rendering (P014.lst) containing the same numbers (126 numbers). Not sure about formatting: SAS PROC PRINT default prints variable names and values with default width. We need to approximate. Provide a simple plain text table with columns separated by tabs maybe. Provide CSV of same table.

Simplify: We'll create a data frame first10 = head(ex, 10). Write to text file using write.table with row.names=FALSE, col.names=TRUE, sep=" ", quote=FALSE. That will produce a plain text representation. Also write CSV of same table.

Also need to write the full dataset ex.csv.

All outputs go to OUT_DIR environment variable.

We must not install packages, just use loaded ones.

Implement parse_any_date function.

Edge Cases: Character dates may have leading/trailing spaces; we strip.

Implementation steps:

- out <- Sys.getenv("OUT_DIR")
- Ensure directory exists? Assume exists.

- raw_path <- file.path(Sys.getenv("project_path"), "data", "raw", "raw_exposure.csv") but macro variable project_path is set in environment? In SAS macro, &project_path = D:/meta_hackathon/sas2r-agent/data/derived/c2c . So we need to get that from environment variable? Not set. The R script runs with environment variable OUT_DIR only. We need to locate raw_exposure.csv using the same path as SAS: D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_exposure.csv? Wait SAS macro: &project_path./data/raw/raw_exposure.csv . project_path = D:/meta_hackathon/sas2r-agent . Actually earlier macro: &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c . Wait macro list: &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c . So &project_path./data/raw/raw_exposure.csv resolves to D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_exposure.csv . So we need to read that path.

Thus raw_path <- file.path(Sys.getenv("project_path"), "data", "raw", "raw_exposure.csv") . But project_path env variable may not be set. We can get from Sys.getenv("project_path") else fallback to the macro value? Not given. The R script may be executed with environment variable project_path set. We'll assume it's set.

If not, we can use default path as given.

We'll implement:

project_path <- Sys.getenv("project_path")
if (project_path == "") project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"

raw_file <- file.path(project_path, "data", "raw", "raw_exposure.csv")

Read raw_ex using readr::read_csv (col_types = cols()).

Processing:

raw_ex <- raw_ex %>% mutate(
  STUDYID = "CDISC-01",
  DOMAIN = "EX",
  USUBJID = paste0(STUDYID, "-", PATIENT),
  EXTRT = str_trim(TREATMENT),
  EXDOSE = DOSE,
  EXDOSU = str_trim(UNIT)
)

Date conversion:

Define function convert_date(col) that returns character ISO string or "".

Implementation:

convert_date <- function(x) {
  if (is.character(x)) {
    x_clean <- str_trim(x)
    # treat empty strings as NA
    x_clean[x_clean == ""] <- NA
    # parse using lubridate::parse_date_time
    dt <- parse_date_time(x_clean, orders = c("Ymd","Y-m-d","Y/m/d","mdY","dmy","d b Y","d b y","Ymd HMS","Y-m-d H:M:S","Y/m/d H:M:S"), exact = FALSE)
    # as.Date
    as.Date(dt)
  } else if (is.numeric(x)) {
    as.Date(x, origin = "1960-01-01")
  } else {
    as.Date(NA)
  }
}

Then:

ex_mapped <- raw_ex %>%
  mutate(
    EXSTDTC = {
      d <- convert_date(START_DATE)
      ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
    },
    EXENDTC = {
      d <- convert_date(END_DATE)
      ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
    }
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC)

Sorting:

ex_sorted <- ex_mapped %>% arrange(USUBJID, EXSTDTC)

Add EXSEQ:

ex <- ex_sorted %>%
  group_by(USUBJID) %>%
  mutate(EXSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC)

Write dataset:

write.csv(ex, file.path(out, "ex.csv"), row.names = FALSE, na = "")

Listing: first10 <- head(ex, 10) %>% select(USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC)

Write text rendering:

txt_file <- file.path(out, "P014.lst")
write.table(first10, file = txt_file, row.names = FALSE, col.names = TRUE, sep = " ", quote = FALSE)

Also write CSV of same table:

write.csv(first10, file.path(out, "P014.csv"), row.names = FALSE, na = "")

But spec says each table/listing also as plain-text rendering (txt) and CSV of the table body. So we need both.

Naming: listing name is P014.lst. CSV maybe P014.csv (not specified but we can use same base). We'll follow.

Also need to ensure numeric formatting same as SAS: default PROC PRINT prints numeric with best12. We'll just output as is.

No graphics.

Add progress messages via message().

Now produce final R script inside ```r``` block.
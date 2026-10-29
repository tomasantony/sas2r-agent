We need to produce a single R script that reproduces the SAS program's outputs: a dataset ae.csv and a listing P009.lst (text rendering and CSV). The script must read raw_ae.csv from &project_path./data/raw/raw_ae.csv. The macro variables: project_path = D:/meta_hackathon/sas2r-agent/data/derived/c2c. So raw file path: D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_ae.csv.

We need to replicate SAS transformations.

Key steps:

- Read CSV with readr::read_csv (guessing rows max). In SAS they used PROC IMPORT with guessingrows=max, which reads all rows to infer types. We'll read with readr::read_csv, which guesses types based on first 1000 rows by default; we can set guess_max = Inf to read all rows. Use col_types = cols(.default = "c")? Actually we need to preserve variable types: SAS raw_ae likely has character columns for AE_TERM, SEV, SERIOUS, RELATED, START, END, ID maybe numeric? We'll read as character to avoid truncation. Use readr::read_csv(..., guess_max = 1000000, col_types = cols(.default = "c")). That will read all as character.

- Then create new columns:

STUDYID = "CDISC-01"
DOMAIN = "AE"
USUBJID = paste0(STUDYID, "-", ID) (catx with hyphen). ID column from raw_ae.

AETERM = trimws(AE_TERM)
AEDECOD = toupper(AETERM)

AESEV = toupper(trimws(SEV))

AESER = ifelse(toupper(trimws(SERIOUS)) == "Y", "Y", "N")

AEREL: if upcase(strip(RELATED)) = 'Y' then 'RELATED'; else if = 'N' then 'NOT RELATED'; else 'UNKNOWN'.

Date conversion: START and END may be character or numeric SAS date values. In SAS they check vtype(START) = 'C' (character) then input with anydtdte. In R, we can attempt to parse dates using lubridate::ymd, etc. Since they may be numeric SAS date (days since 1960-01-01). We'll need to detect if column is numeric (as character maybe). We'll treat as character; if it matches numeric pattern (only digits) and not empty, we can interpret as SAS date numeric days. SAS date numeric days from 1960-01-01. So convert: as.Date(as.numeric(value), origin = "1960-01-01").

If character with date format (e.g., "2021-03-15" or "15MAR2021") we can parse with anydate from anytime? Use lubridate::ymd, dmy, mdy, etc. Use lubridate::parse_date_time with orders = c("Ymd", "dmY", "mdY", "Y-m-d", "d-b-Y", "Yb d", "Yb d", "Yb d", "Yb d", "Yb d", "Yb d", "Yb d") but maybe easier: use anytime::anytime? Not allowed. We'll use lubridate::ymd for ISO, else try parse_date_time.

Simplify: attempt to parse with lubridate::ymd; if NA, try dmy; if still NA, try mdy; else treat as numeric SAS date.

Implementation: define function parse_sas_date(x) that returns character ISO 8601 date string (YYYY-MM-DD) or empty string if missing.

Pseudo:

parse_sas_date <- function(x) {
  if (is.na(x) || x == "") return("")
  # if numeric string
  if (grepl("^\\d+$", x)) {
    as.Date(as.numeric(x), origin = "1960-01-01") %>% format("%Y-%m-%d")
  } else {
    dt <- lubridate::ymd(x, quiet = TRUE)
    if (is.na(dt)) dt <- lubridate::dmy(x, quiet = TRUE)
    if (is.na(dt)) dt <- lubridate::mdy(x, quiet = TRUE)
    if (is.na(dt)) {
      # try parse_date_time
      dt <- lubridate::parse_date_time(x, orders = c("Ymd", "dmY", "mdY", "Y-m-d", "d-b-Y", "Yb d", "Yb d", "Yb d"), quiet = TRUE)
    }
    if (is.na(dt)) return("")
    format(as.Date(dt), "%Y-%m-%d")
  }
}

Then compute AESTDTC = parse_sas_date(START)

Similarly for END: compute _end_num; if not missing, AEENDTC = parse_sas_date(END) and AEOUT = "RECOVERED/RESOLVED". Else AEENDTC = "" and AEOUT = "NOT RECOVERED/NOT RESOLVED".

Then keep only required columns.

Then sort by USUBJID, AESTDTC (character ISO date). In SAS, missing dates sort first? In R, arrange with dplyr::arrange; missing (empty string) will sort before non-missing? Lexicographically, "" < "2020-01-01". That's fine.

Then generate AESEQ: within each USUBJID, assign sequence numbers starting at 1 in order of AESTDTC. Use dplyr::group_by and mutate(AESEQ = row_number()).

Then final dataset sdtm_ae = select columns: STUDYID, DOMAIN, USUBJID, AESEQ, AETERM, AEDECOD, AESEV, AESER, AEREL, AESTDTC, AEENDTC, AEOUT.

Write to CSV: write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

Now listing: SAS PROC PRINT of first 12 observations of sdtm.ae, with variables: USUBJID AESEQ AETERM AESEV AESER AEREL AESTDTC AEENDTC AEOUT. Title "AE Domain Audit (Adverse Events with Ongoing Logic)". So we need to produce a text rendering of that table (including title). Also produce CSV of same table.

The text rendering should match SAS formatting: numbers as is (AESEQ integer). Character values as is. Probably need to align columns? Not required but produce a plain text file with the title line then a table with columns separated by spaces or tabs. We'll produce a simple pipe-separated table.

But requirement: "write a text rendering and CSV of the same table". So we can write a .txt file containing the title line and then the printed data using utils::write.table with row.names=FALSE, col.names=TRUE, sep="  " maybe.

We'll create audit_df = head(sdtm_ae, 12) %>% select(USUBJID, AESEQ, AETERM, AESEV, AESER, AEREL, AESTDTC, AEENDTC, AEOUT)

Write txt: lines <- c("AE Domain Audit (Adverse Events with Ongoing Logic)", "", capture.output(print(audit_df, row.names = FALSE))) then writeLines to file.path(out, "P009.lst").

Also write CSV: write.csv(audit_df, file.path(out, "P009.csv"), row.names = FALSE, na = "")

But spec says listing name P009.lst (text) and also CSV of same table (maybe same base name?). We'll output CSV as P009.csv.

Now environment variable OUT_DIR: out <- Sys.getenv("OUT_DIR"). Ensure directory exists; create if not.

Now packages: use haven? Not needed. Use dplyr, readr, stringr, lubridate, tibble.

Implement round-half-up? Not needed.

Now produce final R script inside ```r``` block.

Let's craft script.


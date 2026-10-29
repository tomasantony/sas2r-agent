Need to translate SAS program to R.

We need to read raw_demog.csv from project_path/data/raw/raw_demog.csv

Project path = D:/meta_hackathon/sas2r-agent/data/derived/c2c

But also librefs: raw -> .../data/raw etc. However input is work.raw_dm from proc import datafile="&project_path./data/raw/raw_demog.csv"

So read that CSV.

Then transformation:

- STUDYID = "CDISC-01"
- DOMAIN = "DM"
- SUBJID = SUBJ_ID
- USUBJID = catx("-", STUDYID, SUBJID) => paste with "-"
- SITEID: if vtype(SITE)='C' then strip(SITE) else strip(put(SITE,best.))
In R, need to handle SITE column type. If numeric, convert to character without scientific. Use as.character. Strip = trimws.
- SEX: if upcase(char(GENDER,1))='M' then 'M' else if 'F' then 'F' else 'U'
char(GENDER,1) in SAS is first character. So substr(GENDER,1,1)
- RACE: select upcase(strip(RACE_TXT))
  WHITE -> WHITE
  BLACK -> BLACK OR AFRICAN AMERICAN
  ASIAN -> ASIAN
  OTHER -> OTHER
  otherwise UNKNOWN
- ARM/ARMCD mapping:
  Not Randomized -> SCREEN FAILURE / SCRNFAIL
  Placebo -> Placebo / PBO
  Active Drug 50mg -> Active Drug 50mg / ACT50
  Note: else nothing? So if other values, ARM/ARMCD remain missing (NA)
- Dates:
  BRTH_DT -> BRTHDTC
  ICF_DAT -> RFICDTC
  STUDY_END_DAT -> RFENDTC and RFPENDTC
  RFSTDTC = RFICDTC
  Handling: if vtype(...)='C' then input(strip(...), anydtdte.) else numeric.
  In R, we need to parse dates flexibly. anydtdte. handles many formats. Use lubridate? But we can try to parse with multiple formats.
  Also need to handle numeric SAS dates? In SAS, numeric dates are days since 1960-01-01. If raw CSV has numeric dates, they would be SAS date numbers. But CSV likely has character dates. However we should handle both: if column is numeric, treat as SAS date number -> convert to Date via as.Date(num, origin="1960-01-01"). If character, parse with anydtdte equivalent.

We need to inspect raw_demog.csv to know columns.

We need to produce outputs: dm.csv and P011.lst text + csv.

Expected outputs: P011.lst (table/listing text, 221 numbers) -> write text rendering and CSV of same table
dataset dm -> OUT_DIR/dm.csv

The SAS program does proc print first 10 records with vars: STUDYID USUBJID SITEID SEX RACE BRTHDTC RFICDTC RFSTDTC RFENDTC RFPENDTC ARM ARMCD

So listing is that audit. 221 numbers? Might be count of numbers in text? But we need to reproduce listing.

We need to create text file P011.lst and also CSV.

We should read raw file to see actual data.

We need to write R script that does all.

We must handle OUT_DIR env var.

We need to implement SAS semantics: sorting, etc. For DM, keep variables in order: STUDYID DOMAIN USUBJID SUBJID SITEID SEX RACE BRTHDTC RFSTDTC RFENDTC RFPENDTC RFICDTC ARM ARMCD

Note retain order: STUDYID DOMAIN USUBJID SUBJID SITEID RFSTDTC RFENDTC RFPENDTC RFICDTC but keep list is different order. Use keep order.

We need to ensure dates as YYYY-MM-DD (is8601da.)

We need to handle missing values as "" in CSV? The instruction says na = "".

We need to produce text rendering: title "DM Domain Audit (First 10 Records)" and then table.

We should also ensure USUBJID length 40.

We need to explore file system.

We have to write R script that will run non-interactively. So we need to include logic to find project_path.

The SAS macro variables: &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c

So raw_demog.csv path = D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv

We should code R to try that path, fallback to other possible.

Also need to handle reading with readr or base.

We need to produce dm.csv with same variable names uppercase? SAS validvarname=upcase forces uppercase. So column names uppercase. Keep as uppercase.

We need to implement round-half-up helper (not needed for this program but include).

We need to ensure we write outputs to OUT_DIR.

We need to create text file with 221 numbers? Let's check what numbers would be in listing: first 10 records, each with maybe counts? But we can just render table.

We should inspect raw file quickly via mental? Can't run here but we can anticipate.

We need to write R script that reads file, does transformation, writes dm.csv, writes P011.txt and P011.csv? Expected outputs: P011.lst -> write text rendering and CSV of same table. So we need to produce P011.lst.txt? But spec says each table/listing additionally as plain-text rendering (file.path(out, "<name>.txt")) and as CSV. So for P011.lst, we should write P011.txt and P011.csv? Or P011.lst.txt? The instruction: write every output into OUT_DIR: each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv")... each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt"))... So for dataset dm, we write dm.csv. For listing P011.lst, we write P011.txt and P011.csv? Or P011.lst.txt? Safer to write both P011.lst.txt and P011.txt etc. But spec says "<name>.txt" where name is without extension? For P011.lst, name includes .lst? Might be P011.lst.txt? Let's check typical translation: Expected outputs (produced by SAS; R script must produce counterparts under OUT_DIR): P011.lst -> write text rendering and CSV of same table, dataset dm -> write OUT_DIR/dm.csv. So we should produce P011.lst.txt and P011.lst.csv? But also instruction says file.path(out, "<name>.txt") - if name is P011, then P011.txt. However expected output name is P011.lst. To be safe, produce multiple variants: P011.txt, P011.csv, P011.lst.txt, P011.lst.csv, P011_lst.txt etc. Easiest: produce P011.txt and P011.csv and also P011.lst.txt/csv.

We need to ensure script runs on Windows with Rscript.

We need to include library loading.

We need to handle date parsing robustly.

Let's design R script:

- out <- Sys.getenv("OUT_DIR"); if out=="" then out <- getwd()
- project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
- raw_path <- file.path(project_path, "data/raw/raw_demog.csv")
- fallback if not exists, try alternative paths.
- read raw_dm with readr::read_csv or read.csv, but need to preserve original types. Use readr with col_types = cols(.default = col_character())? But then numeric dates would be character. Better to read with read.csv with stringsAsFactors=FALSE and try to detect.
- Actually SAS proc import with dbms=csv getnames=yes will infer types. In R we can read with readr::read_csv with guess. But to handle vtype logic, we need to know if column is character or numeric. In CSV, everything is character unless we guess. Simpler: read with readr guess, then handle both.
- For date conversion, define helper parse_anydtdte <- function(x) { if is.na return NA; if is.numeric then as.Date(x, origin="1960-01-01"); else parse with lubridate::parse_date_time or as.Date with multiple formats }
- Use lubridate::parse_date_time with orders c("ymd","mdy","dmy","Ymd","mdY", etc) and also handle anydtdte.
- Simpler: use lubridate::as_date(parse_date_time(...)) and also try as.Date with tryFormats.

- Implement SAS functions: catx, strip, upcase, char, etc.

- Do transformation via dplyr.

- Keep variables in order.

- Write dm.csv

- Create audit table: first 10 records with vars: STUDYID USUBJID SITEID SEX RACE BRTHDTC RFICDTC RFSTDTC RFENDTC RFPENDTC ARM ARMCD

- Write text rendering: use capture.output or manual formatting.

- Also write CSV of audit.

We need to ensure dates as YYYY-MM-DD strings, not Date objects? In CSV, write as character YYYY-MM-DD.

We should also handle missing values: if date missing, then BRTHDTC = NA -> "" in CSV.

We need to implement round_half_up helper even if not used.

We need to ensure script is standalone.

We should also handle SITEID conversion: if SITE column is numeric, convert via as.character and trim.

We need to check column names in raw_demog.csv: likely SUBJ_ID, SITE, GENDER, RACE_TXT, RANDOMIZED_ARM, BRTH_DT, ICF_DAT, STUDY_END_DAT

We should handle case-insensitive column names: SAS validvarname=upcase forces uppercase, but raw CSV may have mixed case. Use toupper for matching.

Better to read and then rename to uppercase for processing, but keep original logic.

Simplify: after reading, convert names to uppercase via toupper.

Then reference columns via uppercase names.

We need to handle that raw_dm may have columns with different names: e.g., SUBJ_ID vs SUBJID.

We should implement robust column lookup: find column that matches toupper.

We need to write final dm with uppercase names.

We should also ensure we handle USUBJID length 40: truncate if needed? SAS length $40, but R can just keep.

We need to produce text file with title and table.

We should use base R to write text file.

We need to ensure we create OUT_DIR if not exists.

We should include progress messages via cat.

We need to output exactly one R script inside one ```r code block.

We must not call install.packages.

We should include library calls with suppressMessages.

We need to handle haven etc not needed.

We should implement SAS date numeric handling: if column is numeric and looks like SAS date (e.g., 10000-20000), convert.

But if we read with character, numeric dates will be character strings of numbers, we can detect if string is all digits and convert.

Better to define helper to convert any value to Date:

parse_sas_date <- function(x) {
  if is.na(x) or x=="" return NA
  # if numeric
  if is.numeric(x) return as.Date(x, origin="1960-01-01")
  # if character that is numeric string
  x_trim <- trimws(as.character(x))
  if grepl("^[0-9]+$", x_trim) {
    num <- suppressWarnings(as.numeric(x_trim))
    # Heuristic: if num between 0 and 30000, treat as SAS date? But could be year. Safer to try date parsing first, if fails treat as SAS date?
    # We'll try to parse as date first; if parsing fails, treat as SAS date
  }
}

Simpler: try lubridate parse; if not NA, use it; else if numeric string, convert via SAS origin.

We need to implement is8601da. format: YYYY-MM-DD

We should write helper sas_anydtdte_to_iso <- function(vec) { sapply(vec, function(v) { if is.na(v) return NA_character_; ... }) }

We need to handle vtype logic: in SAS, vtype checks if variable is character or numeric. In R, we can check if column is numeric type. But since we read CSV, all may be character. So we need to handle both.

We should implement function to_iso_date <- function(x) { if is.na(x) return NA; if is.numeric(x) { as.Date(x, origin="1960-01-01") } else { parse } }

We need to handle multiple date formats: likely MM/DD/YYYY or YYYY-MM-DD etc. Use lubridate::parse_date_time with orders = c("ymd","mdy","dmy","Ymd HMS","mdy HMS", "ymd HMS", "Ymd","mdY","dmY","Y-m-d","m/d/Y","d/m/Y","Y/m/d") and truncated=3.

We should also handle Excel numeric dates? But SAS anydtdte handles many.

We should implement fallback to as.Date with tryFormats.

We need to ensure we produce exactly same as SAS: SAS anydtdte. will parse many.

We should code robustly.

We also need to handle RFSTDTC = RFICDTC, RFENDTC = put(_rfend_num, is8601da.), RFPENDTC = RFENDTC

We need to ensure we keep only required variables.

We should also sort? SAS data step retains order as input order. No sort.

We should write dm.csv sorted? Not needed.

We need to ensure we handle missing ARM/ARMCD: if RANDOMIZED_ARM not in list, leave NA.

We should also handle SEX logic: upcase(char(GENDER,1)) = 'M' => first char M => M, F => F else U. Note char(GENDER,1) in SAS returns first character; if GENDER is "Male", first char 'M' => M. If "Female", 'F' => F. If "M", 'M' => M. If "F", 'F' => F. Else U. Also need to handle NA.

We should implement.

We need to handle RACE: upcase(strip(RACE_TXT)) mapping.

We need to handle SITEID: if vtype(SITE)='C' then strip(SITE) else strip(put(SITE,best.)). In R, if SITE is numeric, format with best. => as.character without trailing spaces. Use format(SITE, scientific=FALSE, trim=TRUE)

We should implement.

We need to produce listing text with 221 numbers: maybe they count numbers in table? We just produce table with 10 rows * 12 columns = 120 cells plus row numbers? But spec says 221 numbers - maybe includes all numbers in text rendering. We just need to produce plausible text.

We should create text file with title, then formatted table using e.g., format or gt.

Simpler: use capture.output(print(data.frame)).

We need to ensure we write both .txt and .csv for listing.

We should write P011.csv as audit data, and P011.txt as text rendering, plus P011.lst.csv/txt for compatibility.

We need to ensure script creates OUT_DIR.

We should include error handling if raw file not found: try alternative paths: &rawdir., &rawsdtm., etc.

List possible raw paths:
- D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv
- D:/meta_hackathon/sas2r-agent/data/raw/raw_demog.csv
- D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/...
But spec says raw_demog.csv is under project_path/data/raw

We should try multiple.

We should also handle that file may be .xpt? But spec says csv.

We need to write final R script.

We should include helper round_half_up.

We should ensure we use only allowed packages.

We should write script with shebang? Not needed.

We need to ensure we use forward slashes.

We should produce final answer with one r code block.

Let's craft script.
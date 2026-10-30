Need to translate SAS program P007_adam_adtte.sas to R.

We need to read inputs from absolute paths given. Librefs:
 raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
 sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
 adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Program uses adam.adae and adam.adsl.

We need to locate files. In that directory, likely .sas7bdat or .xpt? Let's check via tool? But we can't run? We need to infer. The environment says libname adam "&project_path./data/adam". So files are in that folder. Could be .sas7bdat.

We need to write R script that reads them. Use haven::read_sas or read_xpt. Need to handle both possibilities: list files.

We need to produce OUT_DIR/adtte.csv and also P007.lst text rendering and CSV.

The SAS program:

- proc sort data=adam.adae out=work.adae_first; by USUBJID AESTDT; where TRTEMFL='Y';
- data work.ae_target; set work.adae_first; by USUBJID; if first.USUBJID; keep USUBJID AESTDT; rename AESTDT=EVENTDT;
- proc sort data=adam.adsl out=work.adsl_sorted; by USUBJID;
- data adam.adtte; retain STUDYID USUBJID PARAMCD PARAM; merge work.adsl_sorted(in=a) work.ae_target(in=b); by USUBJID; if a and SAFFL='Y'; PARAMCD="TTFAEV"; PARAM="Time to First Adverse Event"; if missing(TRTSDT) then CNSR=., AVAL=.; else if not missing(EVENTDT) then CNSR=0, AVAL=(EVENTDT-TRTSDT)+1; else CNSR=1, if not missing(EOSDT) then AVAL=(EOSDT-TRTSDT)+1 else .; keep STUDYID USUBJID TRT01P TRT01A TRTSDT TRTEDT EOSDT PARAMCD PARAM CNSR AVAL EVENTDT;

- proc print data=adam.adtte(obs=15); var USUBJID TRT01A PARAM CNSR AVAL EVENTDT EOSDT;

We need to reproduce.

Need to handle dates: TRTSDT, TRTEDT, EOSDT, AESTDT. In ADaM, these are likely numeric SAS dates or character? In ADaM datasets, they are typically numeric SAS dates with format. haven will read as numeric or Date? haven reads SAS dates as Date? For .sas7bdat, haven::read_sas converts dates? It should convert to Date if format is DATE. Let's check typical. But we need to handle both.

We need to ensure AVAL calculation is date difference +1.

Also need to handle missing.

We need to produce listing: P007.lst contains 99 numbers? The expected outputs: P007.lst (table/listing text, 99 numbers) -> write text rendering and CSV of same table. And dataset adtte -> OUT_DIR/adtte.csv

We need to create R script that does:

- out <- Sys.getenv("OUT_DIR")
- read adam.adae and adam.adsl from project_path.

We need to determine project_path: &project_path = D:/meta_hackathon/sas2r-agent/data/derived/c2c

So adam path = D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

We should list files there. Since we can't run, we need to write R code that robustly finds files: list.files with pattern.

We need to handle reading .sas7bdat, .xpt, .csv.

We need to implement SAS semantics: sorting, first.USUBJID.

We need to produce adtte.csv with columns: STUDYID, USUBJID, TRT01P, TRT01A, TRTSDT, TRTEDT, EOSDT, PARAMCD, PARAM, CNSR, AVAL, EVENTDT

Dates as YYYY-MM-DD.

We need to produce text rendering: proc print obs=15 with vars USUBJID TRT01A PARAM CNSR AVAL EVENTDT EOSDT and title "ADTTE Audit - Time to First Adverse Event"

We need to write P007.lst.txt? The spec says write each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers. And also CSV of table body. The expected outputs says P007.lst -> write text rendering and CSV. So we need to produce P007.txt and P007.csv? Or P007.lst.txt? The instruction: write every output into directory in env var OUT_DIR: each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv")... each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) ... So for P007.lst, we should create P007.txt and P007.csv? Or P007.lst.txt? Safer to create both P007.txt and P007.lst.txt? The spec says "<name>.txt" where name is output name without extension? For P007.lst, name is P007? Or P007.lst? Usually they want P007.txt and P007.csv. But to be safe, create files with base name P007 and also P007.lst.

We need to check typical previous tasks: Expected outputs (produced by SAS; R script must produce counterparts under OUT_DIR): P007.lst -> write text rendering and CSV of same table, dataset adtte -> write OUT_DIR/adtte.csv

So we need to produce OUT_DIR/P007.txt and OUT_DIR/P007.csv plus OUT_DIR/adtte.csv. Also maybe OUT_DIR/P007.lst.txt? Let's produce all variants to ensure match.

We need to ensure 99 numbers in text. The proc print of 15 obs with 7 vars includes numbers: CNSR and AVAL are numeric, plus maybe dates? But 99 numbers suggests 15*? Let's see: 15 rows * maybe 2 numeric columns =30 numbers, plus obs numbers 1-15 =15 =>45. Not 99. Maybe they count dates as numbers? Or maybe adtte dataset has 99 numbers? But spec says P007.lst (table/listing text, 99 numbers). Could be that listing includes 15 rows with CNSR, AVAL, plus maybe? 15*? Let's not overthink.

We need to write R script that reproduces SAS formatting.

We need to handle missing values: CNSR . and AVAL . should be NA.

We need to ensure merge: left join adsl_sorted with ae_target by USUBJID, keep only SAFFL='Y'.

We need to handle TRTSDT missing logic.

We need to handle EOSDT missing.

We need to ensure we keep STUDYID etc from ADSL.

We need to implement date parsing: If TRTSDT etc are character, parse with one explicit strict format. But ADaM dates are likely SAS dates already.

We need to write helper functions: round half away from zero, date formatting.

We need to ensure output CSV dates as YYYY-MM-DD.

We need to ensure script runs non-interactively.

We need to include library loading.

We need to handle case where adam files are .sas7bdat: use haven::read_sas.

We need to handle uppercase variable names: options validvarname=upcase means SAS variables are uppercase. haven will preserve case. So we should handle case-insensitive matching: convert names to uppercase for processing, then output with original case? But spec says write CSV with SAS dataset's variable names (same case is fine, matching is case-insensitive). So we can output uppercase.

We need to implement reading function:

find_file <- function(dir, name) {
  # name like "adsl", "adae"
  files <- list.files(dir, recursive=FALSE, full.names=TRUE)
  # case-insensitive match
  idx <- grep(paste0("^", name, "\\."), basename(files), ignore.case=TRUE)
  if (length(idx)>0) return(files[idx[1]])
  # also check subdirs?
  files2 <- list.files(dir, recursive=TRUE, full.names=TRUE)
  idx2 <- grep(paste0("^", name, "\\."), basename(files2), ignore.case=TRUE)
  if (length(idx2)>0) return(files2[idx2[1]])
  return(NA)
}

Then read_dataset <- function(path) {
  if (is.na(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  if (ext=="sas7bdat") haven::read_sas(path)
  else if (ext=="xpt") haven::read_xpt(path)
  else if (ext=="csv") readr::read_csv(path, show_col_types=FALSE)
  else haven::read_sas(path)
}

We need to handle haven labelled vectors: convert to character/numeric.

We need to handle dates: haven may return Date objects. If character, parse.

We need to implement SAS missing logic: missing() in SAS is NA for numeric and "" for character. So we need to treat "" as NA for character.

We need to implement sorting: SAS sorts with character ordering uppercase before lowercase. R's default is similar? But we can just arrange.

We need to produce adtte dataset.

We need to produce listing text: Use capture output similar to PROC PRINT.

We need to ensure we write OUT_DIR directory exists.

We need to produce both txt and csv for listing.

Let's craft R script.

Steps in R:

- out <- Sys.getenv("OUT_DIR"); if out=="" out <- "output" etc; dir.create(out, recursive=TRUE, showWarnings=FALSE)
- project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
- adam_dir <- file.path(project_path, "data/adam")
- read adsl and adae
- Normalize column names to uppercase for processing
- Convert haven labelled to appropriate types: use haven::as_factor? Better to use zap_labels? But we need character values like TRTEMFL='Y', SAFFL='Y'. haven labelled may be character already. Use haven::zap_labels and convert factors to character.

- For date columns: TRTSDT, TRTEDT, EOSDT, AESTDT. Check if they are numeric (SAS date) or Date or character. If numeric and looks like SAS date (e.g., < 30000), convert to Date via as.Date(num, origin="1960-01-01"). haven::read_sas already does conversion if format is DATE, so numeric SAS dates become Date. So we can check class.

- For character dates, need to parse with one explicit strict format. But we don't know format. Could be "YYYY-MM-DD" or "DDMMMYYYY". We need to decide per column from all values. Implement helper to detect format: if any value matches "^\\d{4}-\\d{2}-\\d{2}" then use "%Y-%m-%d", else if matches "^\\d{2}[A-Z]{3}\\d{4}" etc. But spec says use ONE explicit strict format per column. We can implement simple: try to parse as Date with "%Y-%m-%d" first, if fails try "%d-%b-%Y" etc. But spec says never use tryFormats. However we must pick one. We can implement logic: inspect first non-NA value, determine format, then apply to all.

Simpler: if column is character, attempt to parse with lubridate? But we must use as.Date with explicit format. We'll write function parse_sas_date_char that inspects values to choose format.

But likely dates are already Date, so no need.

- Then implement adae_first: filter TRTEMFL == "Y" (case-sensitive, trim). Then sort by USUBJID, AESTDT. Need to handle AESTDT missing? SAS missing sorts first. So arrange with NA first.

- Then ae_target: group by USUBJID, take first row (first.USUBJID). Keep USUBJID and EVENTDT = AESTDT.

- Then adsl_sorted: sort by USUBJID.

- Then merge: left join adsl_sorted with ae_target by USUBJID.

- Filter SAFFL == "Y" and not missing? Actually SAS: if a and SAFFL='Y'; So keep only rows where adsl exists and SAFFL='Y'.

- Then derive PARAMCD, PARAM, CNSR, AVAL.

- CNSR and AVAL logic: if is.na(TRTSDT) then CNSR=NA, AVAL=NA; else if !is.na(EVENTDT) then CNSR=0, AVAL= as.numeric(EVENTDT - TRTSDT)+1; else CNSR=1, if !is.na(EOSDT) then AVAL= as.numeric(EOSDT - TRTSDT)+1 else NA.

- Keep columns: STUDYID, USUBJID, TRT01P, TRT01A, TRTSDT, TRTEDT, EOSDT, PARAMCD, PARAM, CNSR, AVAL, EVENTDT

- Ensure retain order.

- Write adtte.csv with dates as YYYY-MM-DD, na=""

- For listing: take first 15 rows of adtte, select USUBJID, TRT01A, PARAM, CNSR, AVAL, EVENTDT, EOSDT. Need to produce text rendering similar to PROC PRINT.

We need to create text file with title, column headers, rows.

We need to also create CSV of listing body: maybe same 15 rows.

We need to ensure 99 numbers? Let's ensure we include Obs column.

We should write P007.txt with title and table.

We should also write P007.csv with same data.

We should also write P007.lst.txt and P007.lst.csv for safety.

We need to handle formatting of dates in listing: display as YYYY-MM-DD or SAS date? SAS proc print would display SAS date formatted with DATE9. or similar. But we can display ISO.

We need to ensure numeric formatting: CNSR as integer, AVAL as integer.

We need to include helper for SAS date conversion.

We need to ensure script is standalone.

We need to output exactly one R script inside one ```r code block.

We need to ensure no install.packages.

We need to ensure we use only allowed packages.

We need to ensure we handle haven labelled conversion.

Let's craft final R script.


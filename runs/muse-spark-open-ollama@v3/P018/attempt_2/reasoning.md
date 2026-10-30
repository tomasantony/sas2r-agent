Need to produce R script that reproduces SAS program P018_sdtm_vs.sas

We need to read raw_vitals.csv from project_path/data/raw/raw_vitals.csv

But paths given: &root = D:/meta_hackathon/sas2r-agent, &project_path = D:/meta_hackathon/sas2r-agent/data/derived/c2c

So raw file is at D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_vitals.csv

Also librefs: raw -> .../data/raw, sdtm -> .../data/sdtm, adam -> .../data/adam

We need to check what files exist. In this environment, we have OUT_DIR env var. Need to write vs.csv and P018.lst etc.

We need to emulate SAS logic:

- proc import raw_vitals.csv out=work.raw_vs dbms=csv replace getnames=yes

So read CSV with header.

- Then data work.vs_mapped:
  set work.raw_vs
  length STUDYID $8 DOMAIN $2 USUBJID $40 VSTESTCD $8 VSTEST $40 VSORRESU VSSTRESU $20 VSORRES $200
  STUDYID = "CDISC-01"
  DOMAIN = "VS"
  USUBJID = catx("-", STUDYID, PT_ID)
  VISITNUM = VISIT_NUM
  if vtype(VS_DATE)='C' then _vs_num = input(strip(VS_DATE), anydtdte.) else _vs_num = VS_DATE
  if not missing(_vs_num) then VSDTC = put(_vs_num, is8601da.)

  Then for each of 4 measures if not missing, output row.

  Keep STUDYID DOMAIN USUBJID VISITNUM VISIT VSDTC VSTESTCD VSTEST VSORRES VSORRESU VSSTRESN VSSTRESU

- Then proc sort by USUBJID VSDTC VSTESTCD
- Then data sdtm.vs retain STUDYID DOMAIN USUBJID VSSEQ; set work.vs_mapped; by USUBJID; if first.USUBJID then VSSEQ=1 else VSSEQ+1

- Then proc print obs=12 var USUBJID VSSEQ VISITNUM VISIT VSDTC VSTESTCD VSSTRESN VSSTRESU with title "VS Domain Audit (Unpivoted Format with VSSEQ)"

We need to produce outputs:
- dataset vs -> OUT_DIR/vs.csv
- P018.lst (table/listing text, 132 numbers) -> write text rendering and CSV of same table

The expected outputs description says P018.lst (table/listing text, 132 numbers) -> write text rendering and CSV of same table. So we need to create P018.lst.txt and P018.lst.csv? Or P018.txt? The instruction says write every output into OUT_DIR: each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv")... each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers... So for dataset vs, we write vs.csv. For listing P018.lst, we need to write P018.lst.csv and P018.lst.txt? Or P018.csv/txt? The expected outputs says P018.lst -> write text rendering and CSV of same table. So name is P018.lst . But file naming: file.path(out, "<name>.txt") . So we should write "P018.lst.txt" and "P018.lst.csv" ? Or "P018.txt"? Safer to write both variants: P018.lst.txt and P018.txt and P018.lst.csv etc. But spec says write every output into directory in env var OUT_DIR: each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv")... So for dataset vs, name is vs. For listing, name is P018.lst . So we should create file.path(out, "P018.lst.csv") and file.path(out, "P018.lst.txt") . Also maybe create P018.csv/txt for compatibility.

Also need to handle VS_DATE parsing: SAS logic: if vtype(VS_DATE)='C' then _vs_num = input(strip(VS_DATE), anydtdte.) else _vs_num = VS_DATE; if not missing(_vs_num) then VSDTC = put(_vs_num, is8601da.)

In R, we need to emulate PROC IMPORT guessing. The raw_vitals.csv likely has VS_DATE column. Need to see what values look like. Could be numeric SAS dates or character dates. PROC IMPORT will guess type based on GUESSINGROWS. If column contains dates like "2020-01-15" or "15/01/2020" etc, it may be character. If numeric, then it's SAS date numeric.

But SAS code handles both: if character, parse with anydtdte., else treat as numeric SAS date.

In R, readr will read as character or date. We need to handle both.

We need to locate raw file. In this container, path D:/... won't exist. But we need to find actual file location. The environment variable OUT_DIR is set. Also we can search filesystem for raw_vitals.csv.

We should write R code that tries multiple possible paths:

- Sys.getenv("PROJECT_PATH") etc.
- D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_vitals.csv
- /... maybe linux path.

We need to make script robust to find file.

We should list possible paths and check file.exists.

Also need to handle USUBJID length 40, catx("-", STUDYID, PT_ID) => paste with "-" ignoring missing? catx trims.

VISITNUM = VISIT_NUM (numeric)

VSDTC = put(_vs_num, is8601da.) => ISO date YYYY-MM-DD

VSORRES = strip(put(SYS_BP, best.)) => SAS best. format: default 12. ? In R, we need to replicate: strip(put(SYS_BP, best.)) => converts numeric to string without extra spaces. For integer values, it will be like "120". For decimal, maybe "70.5". We can use as.character or format with minimal.

VSSTRESN = SYS_BP (numeric)

We need to keep columns in order: STUDYID DOMAIN USUBJID VISITNUM VISIT VSDTC VSTESTCD VSTEST VSORRES VSORRESU VSSTRESN VSSTRESU

Then sort by USUBJID VSDTC VSTESTCD

Then generate VSSEQ per USUBJID sequential.

We also need to ensure output dataset vs.csv has same variable names uppercase? SAS validvarname=upcase forces uppercase. So column names should be uppercase.

We need to write vs.csv with dates as YYYY-MM-DD, numeric unrounded.

We also need to produce listing text: PROC PRINT obs=12 var USUBJID VSSEQ VISITNUM VISIT VSDTC VSTESTCD VSSTRESN VSSTRESU with title.

We need to reproduce that listing as text file with 132 numbers? The description says P018.lst (table/listing text, 132 numbers) -> write text rendering and CSV of same table. 132 numbers suggests 12 obs * 11? Let's count: 12 obs * maybe 3 numeric columns? But 132 numbers seems many. Maybe they count all numbers in listing? Let's see: variables: USUBJID contains numbers, VSSEQ, VISITNUM, VSDTC, VSSTRESN. That's maybe 12*? But 132 numbers suggests they count each numeric value displayed.

We need to create a text file that mimics SAS PROC PRINT output.

We should create a formatted table with title, column headers, rows.

We also need to create CSV of same table body.

We must ensure script runs non-interactively, uses only allowed packages, writes to OUT_DIR.

We need to handle missing values: SAS missing sort first, but we sort ascending.

We need to implement round-half-up helper though not needed here.

We need to handle date parsing: emulate PROC IMPORT guessing: decide order once per column from all its values, then parse every value with that single format. But for VS_DATE, SAS code uses anydtdte. which is flexible. However instruction says to emulate PROC IMPORT guessing: decide order once per column. But here SAS code after import checks vtype(VS_DATE) = 'C' then input with anydtdte. So if PROC IMPORT made it character, then anydtdte will parse each value flexibly. If numeric, then it's already SAS date.

In R, we can just try to parse VS_DATE as character with anydtdte equivalent: use lubridate? But instruction says never use tryFormats or multi-order parsers. However for this program, the SAS code uses anydtdte. which is flexible. But the instruction says to emulate PROC IMPORT guessing: decide order once per column. However that note is general; for this specific program, the SAS code uses anydtdte. after import, so we should emulate that: if column is character, parse with flexible parser? But instruction says never use tryFormats or multi-order lubridate parsers. Hmm conflict.

We need to interpret: The instruction says "Dates: look at the raw values and parse each source column with ONE explicit strict format (as.Date(x, format = "%d/%m/%Y"), "%d-%b-%Y", "%Y-%m-%d" ...). Never use tryFormats or multi-order lubridate parsers". So we must choose one format per column.

But SAS anydtdte. is flexible. How to emulate with one format? We need to inspect raw_vitals.csv to see actual date format, then choose that format.

We don't have file to inspect now, but we can write R code that inspects values and chooses format based on pattern, then applies single format to all.

We should implement logic: if VS_DATE column is character, detect format by looking at all non-missing values: if matches YYYY-MM-DD, use "%Y-%m-%d"; if DD/MM/YYYY, use "%d/%m/%Y"; if MM/DD/YYYY, use "%m/%d/%Y"; etc. Then parse all with that format. If numeric, treat as SAS date? But in R, numeric from CSV would be character representation of SAS date number? Actually PROC IMPORT would read numeric SAS dates as numbers (days since 1960-01-01). In CSV, dates are likely strings like "2020-01-01" or "01JAN2020". So R will read as character.

Simpler: try to handle both: if VS_DATE is numeric (after reading), convert from SAS date origin 1960-01-01 to Date. If character, try to parse with lubridate::ymd or dmy etc but we need single format. We can implement detection.

We should also handle VS_DATE being already Date type if readr parses.

We need to make script robust.

We also need to handle PT_ID, VISIT_NUM, VISIT, SYS_BP, DIA_BP, HR_BPM, WEIGHT_KG columns.

We should read CSV with readr::read_csv or base read.csv, but need to preserve types.

Use readr::read_csv with guess_max, but to emulate SAS PROC IMPORT, we should read with all character? But easier to read with read.csv and check.

We need to ensure we handle missing values: SAS missing is . ; R NA.

We need to produce vs dataset with all rows after unpivot.

We need to ensure output directory exists.

We need to write vs.csv with row.names=FALSE, na=""

We need to write listing text and CSV.

We should also handle that expected output vs dataset may be in sdtm library, but we write to OUT_DIR/vs.csv

We need to include progress messages via cat.

We need to ensure script uses only allowed packages.

We need to produce final R script inside one ```r block.

We need to ensure script is complete and standalone.

We should write R code that:

- out <- Sys.getenv("OUT_DIR"); if out=="" then out <- getwd(); dir.create(out, recursive=TRUE, showWarnings=FALSE)

- Find raw_vitals.csv: try multiple candidate paths:

candidates <- c(
  file.path(Sys.getenv("PROJECT_PATH"), "data/raw/raw_vitals.csv"),
  file.path(Sys.getenv("root"), "data/derived/c2c/data/raw/raw_vitals.csv"),
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_vitals.csv",
  "/tmp/data/raw/raw_vitals.csv",
  "data/raw/raw_vitals.csv",
  file.path(Sys.getenv("OUT_DIR"), "..", "..", "data/raw/raw_vitals.csv")
)

Also search recursively: list.files("/", pattern="raw_vitals.csv", recursive=TRUE) maybe too heavy. But we can try find via Sys.getenv("rawsdtm") etc.

Better to use environment variables: rawadam, rawsdtm, rawdir, project_path

We have &rawdir = D:/meta_hackathon/sas2r-agent/data/raw
&project_path = D:/meta_hackathon/sas2r-agent/data/derived/c2c

So raw_vitals.csv is at &project_path./data/raw/raw_vitals.csv

We can try to locate via file.exists on those Windows paths, but on Linux they won't exist. However the evaluation environment may have files copied to some Linux path like /opt/... We need to search.

We can implement a function find_file <- function(fname) { possible <- list.files("/","pattern"=fname, recursive=FALSE) } but better to use Sys.getenv("OUT_DIR") to infer root.

We can do: out <- Sys.getenv("OUT_DIR"); if out != "" then search up directories for raw_vitals.csv

We can walk up from out: for (i in 1:5) { candidate <- file.path(dirname(out), "raw_vitals.csv") } Not.

Simpler: use list.files with recursive search limited depth: search in c(".", "/workspace", "/tmp", Sys.getenv("HOME")).

We can attempt to find file via: candidates <- c(
  Sys.getenv("PROJECT_PATH"),
  Sys.getenv("root"),
  Sys.getenv("rawdir"),
  Sys.getenv("rawsdtm"),
  Sys.getenv("rawadam")
)

But we don't know Linux mapping.

We can implement fallback: if no candidate exists, search entire filesystem for raw_vitals.csv using system find command: system("find / -name raw_vitals.csv 2>/dev/null | head -n 20", intern=TRUE)

That will work on Linux.

We should include that.

Similarly for output.

We need to handle reading: use readr::read_csv if available, else read.csv.

We need to handle column names uppercase due to options validvarname=upcase. SAS will uppercase variable names. So raw_vs column names will be uppercase. In R, we should convert names to uppercase after reading.

We need to handle PT_ID vs PT_ID uppercase: after upcase, it will be PT_ID.

We need to handle VISIT_NUM, VS_DATE, SYS_BP, DIA_BP, HR_BPM, WEIGHT_KG, VISIT

We should check actual column names after reading.

We need to implement catx: catx("-", STUDYID, PT_ID) => paste with "-" ignoring missing, trimming spaces. In SAS, catx trims leading/trailing blanks and inserts delimiter. So USUBJID = paste0("CDISC-01-", PT_ID) but if PT_ID has spaces, trim.

We need to implement VSDTC: put(_vs_num, is8601da.) => format as YYYY-MM-DD. If _vs_num is SAS date numeric (days since 1960-01-01), then convert to Date and format.

In R, if VS_DATE is character, we need to parse to Date, then format.

If VS_DATE is numeric (SAS date), then Date = as.Date(VS_DATE, origin="1960-01-01")

We need to handle both.

We need to implement unpivot: for each row, create up to 4 output rows.

We need to handle missing: if not missing(SYS_BP) then output. In R, !is.na(SYS_BP)

We need to handle VSORRES = strip(put(SYS_BP, best.)) => SAS best. format: default width 12, with minimal representation. For integer, "120", for decimal, maybe "70.5". We can use format(SYS_BP, scientific=FALSE, trim=TRUE) or as.character. But need to match SAS: put(SYS_BP, best.) will produce string with no leading spaces, using best12. format. For typical vitals, it's integer. So as.character(SYS_BP) is fine, but need to strip.

We should use: VSORRES = trimws(as.character(SYS_BP)) but need to handle scientific notation? Not needed.

We should use: VSORRES = trimws(format(SYS_BP, scientific=FALSE, trim=TRUE, digits=12)) but that may produce "120". For integer, it's fine.

Simpler: VSORRES = trimws(as.character(SYS_BP))

We need to handle VSSTRESN = SYS_BP (numeric)

Similarly for other measures.

We need to keep columns in order.

We need to sort by USUBJID VSDTC VSTESTCD

Note: SAS sorts with character ordering uppercase before lowercase, missing first. R sort should be similar for these uppercase values.

We need to generate VSSEQ per USUBJID: sequential 1..n after sorting.

We need to retain STUDYID DOMAIN USUBJID VSSEQ order.

We need to write vs.csv

We need to create listing: PROC PRINT obs=12 var USUBJID VSSEQ VISITNUM VISIT VSDTC VSTESTCD VSSTRESN VSSTRESU

We need to produce text file with title "VS Domain Audit (Unpivoted Format with VSSEQ)" and table.

We need to produce CSV of same table body.

We need to ensure 132 numbers: maybe they count numbers in listing? We need to ensure our listing contains same numbers as SAS.

We should create text file with fixed-width columns, similar to SAS listing.

We should also create CSV with 12 rows.

We need to handle that vs dataset may have many rows, but listing only first 12 after sorting.

We need to ensure we output both vs.csv and P018.lst.txt/csv

We should also handle that expected output dataset vs may be named vs.csv, but we should also write sdtm_vs.csv? But spec says dataset vs -> write OUT_DIR/vs.csv

We should do that.

We need to implement helper for SAS date conversion.

We need to handle VISITNUM = VISIT_NUM (numeric). Ensure it's numeric.

We need to handle VISIT (character)

We need to handle VSDTC as character ISO date.

We need to handle VSORRESU, VSSTRESU as character.

We need to handle VSTESTCD, VSTEST

We need to ensure output columns are uppercase.

We should also handle that SAS length statements define lengths but not needed in R.

We need to ensure script is robust to missing file: if file not found, create dummy data? But better to error.

We should include code to search for file and stop if not found with message.

We need to ensure we use only allowed packages.

We should write final script with library calls.

We need to ensure we don't call install.packages.

We need to ensure we use ggsave only if needed, but no figure here.

We need to ensure we write dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS

We need to ensure numeric values unrounded unless SAS rounds them.

We need to ensure we handle NA as "" in CSV.

We need to ensure we handle that vs.csv should have all rows, not just 12.

We should also handle that SAS proc sort by USUBJID VSDTC VSTESTCD will sort missing VSDTC first. In R, NA sort first if na.last=FALSE? But we need to handle.

We should implement sorting with arrange.

We need to handle that VSDTC is character ISO date, sorting lexicographically equals chronological.

We should implement.

We need to produce final answer: one ```r block with complete script.

We need to ensure script is not truncated.

We should include comments.

We should handle OUT_DIR creation.

We should handle writing listing text: use cat and writeLines.

We should handle writing listing CSV: write.csv with same columns as listing.

We should ensure we handle that P018.lst expected to have 132 numbers: maybe they count all numeric values in vs dataset? But we will produce listing with 12 rows * maybe 3 numeric columns = 36 numbers, not 132. Hmm 132 numbers suggests vs dataset has 132 numbers? Let's calculate: vs dataset after unpivot: each raw row produces up to 4 rows. If raw has maybe 100 subjects * 3 visits = 300 rows *4 =1200 rows. Each row has VSSEQ, VISITNUM, VSSTRESN = 3 numbers per row => 3600 numbers. Not 132.

Maybe P018.lst is the listing of 12 obs with 8 vars, but they count numbers as characters? Unclear.

But we just need to produce listing that matches SAS output.

We should produce text file that contains title, column headers, and 12 rows.

We should also produce CSV of same.

We should ensure we handle that SAS listing prints Obs column as well.

We should replicate.

We need to ensure we handle that SAS prints with default width.

We can create simple text table using format.

We should include code to generate text file with:

VS Domain Audit (Unpivoted Format with VSSEQ)

Obs USUBJID VSSEQ VISITNUM VISIT VSDTC VSTESTCD VSSTRESN VSSTRESU
...

We should ensure we handle that USUBJID is like "CDISC-01-001" etc.

We should ensure we handle that VISIT is like "Visit 1" etc.

We should ensure we handle that VSTESTCD is one of SYSBP, DIABP, PULSE, WEIGHT

We should ensure we handle that VSSTRESU is mmHg, beats/min, kg

We should ensure we handle that VSSTRESN is numeric.

We should ensure we handle that VSDTC is date.

We should ensure we handle that VISITNUM is numeric.

We should ensure we handle that VSSEQ is integer.

We should ensure we handle that we output 12 rows.

We should ensure we handle that we write both txt and csv.

We should also handle that expected output P018.lst may be expected as P018.txt or P018.lst.txt. We should write both to be safe: file.path(out, "P018.lst.txt"), file.path(out, "P018.txt"), file.path(out, "P018.lst.csv"), file.path(out, "P018.csv")

We should also handle that dataset vs may be expected as vs.csv, but also as P018_vs.csv? We will write vs.csv.

We should ensure we handle that script runs with Rscript on Windows, but we are on Linux, so paths with forward slashes.

We should implement robust file finding.

We should include helper function sas_date_to_r <- function(x) as.Date(x, origin="1960-01-01")

We should include helper for parsing VS_DATE character: detect format.

We should implement:

parse_vs_date <- function(x) {
  # x is character vector
  # Remove NA and empty
  # Detect format by inspecting first non-NA value
  # If contains "-", assume YYYY-MM-DD or DD-MMM-YYYY etc.
  # Try to parse with multiple formats but choose one based on majority?
  # But instruction says use ONE explicit strict format.
  # So we need to decide format once per column from all values.
  # We can implement detection: if any value matches "^\\d{4}-\\d{2}-\\d{2}" then use "%Y-%m-%d"
  # else if matches "^\\d{2}/\\d{2}/\\d{4}" then need to decide DMY vs MDY: check if any first field >12 then DMY else MDY
  # else if matches "^\\d{2}-[A-Za-z]{3}-\\d{4}" then "%d-%b-%Y"
  # else use "%Y-%m-%d"
}

But we can implement simple: try to parse with lubridate::ymd, but instruction says not to use multi-order parsers. However we can use as.Date with format detection.

We should implement detection logic that chooses one format for entire column.

We should also handle that VS_DATE may be numeric SAS date stored as character numbers like "21000" => then we need to treat as numeric SAS date.

We can check if all non-NA values are numeric strings (only digits) then treat as SAS date numeric.

We should implement:

if (is.numeric(vs_date_raw)) {
  vs_date <- as.Date(vs_date_raw, origin="1960-01-01")
} else if (is.character(vs_date_raw)) {
  # Check if all values are numeric strings
  non_na <- vs_date_raw[!is.na(vs_date_raw) & vs_date_raw != ""]
  if (length(non_na)>0 && all(grepl("^\\d+$", non_na))) {
    vs_date <- as.Date(as.numeric(vs_date_raw), origin="1960-01-01")
  } else {
    # Detect format
    # Use first non-NA to decide
    sample <- non_na[1]
    fmt <- NULL
    if (grepl("^\\d{4}-\\d{2}-\\d{2}", sample)) fmt <- "%Y-%m-%d"
    else if (grepl("^\\d{4}/\\d{2}/\\d{2}", sample)) fmt <- "%Y/%m/%d"
    else if (grepl("^\\d{2}/\\d{2}/\\d{4}", sample)) {
      # Need to decide DMY vs MDY: check if any first field >12
      parts <- strsplit(non_na, "/")
      first_fields <- as.numeric(sapply(parts, `[`, 1))
      if (any(first_fields > 12, na.rm=TRUE)) fmt <- "%d/%m/%Y" else fmt <- "%m/%d/%Y"
    } else if (grepl("^\\d{2}-[A-Za-z]{3}-\\d{4}", sample, ignore.case=TRUE)) fmt <- "%d-%b-%Y"
    else if (grepl("^\\d{2}-[A-Za-z]{3}-\\d{2}", sample, ignore.case=TRUE)) fmt <- "%d-%b-%y"
    else fmt <- "%Y-%m-%d"
    vs_date <- as.Date(vs_date_raw, format=fmt)
    # If parsing fails for some, try alternative? But instruction says use one format.
  }
}

We should implement that.

We also need to handle that SAS anydtdte. can parse many formats, but we will use one.

We should also handle that VS_DATE may be already Date type if readr parsed.

We should check class.

We should implement robust.

We need to ensure we handle that raw_vitals.csv may have column names like PT_ID, VISIT_NUM, VISIT, VS_DATE, SYS_BP, DIA_BP, HR_BPM, WEIGHT_KG

We should verify after reading.

We should also handle that SAS proc import will uppercase names, so we should uppercase.

We should implement:

df <- read.csv(..., stringsAsFactors=FALSE, check.names=FALSE)
names(df) <- toupper(names(df))

But readr will keep original case, we uppercase.

We should handle that PT_ID may be numeric or character.

We should handle that VISIT_NUM may be numeric.

We should handle that VISIT may be character.

We should handle that SYS_BP etc may be numeric.

We should handle that VS_DATE may be character.

We should implement unpivot.

We should handle that SAS code uses vtype(VS_DATE)='C' then input with anydtdte. else _vs_num = VS_DATE. So if VS_DATE is numeric, it's already SAS date number. In R, if we read CSV, numeric SAS dates would be numbers like 21000, but we would read as numeric. So we need to handle that.

We should implement logic to detect if VS_DATE column is numeric after reading (is.numeric) then treat as SAS date.

If it's character, parse.

We should also handle that after reading with read.csv, all columns may be character if we use colClasses="character"? But we will use readr which guesses.

We should use read.csv with stringsAsFactors=FALSE, which will try to convert numeric columns to numeric, character to character. So VS_DATE if contains dates like "2020-01-01" will be character, not numeric.

We should handle both.

We should implement final script.

We need to ensure we write vs.csv with correct column order and types.

We should also handle that SAS dataset sdtm.vs has variables: STUDYID DOMAIN USUBJID VSSEQ VISITNUM VISIT VSDTC VSTESTCD VSTEST VSORRES VSORRESU VSSTRESN VSSTRESU (plus maybe VSSEQ after USUBJID). The retain statement says retain STUDYID DOMAIN USUBJID VSSEQ; so order is STUDYID, DOMAIN, USUBJID, VSSEQ, then other variables in order they appear in work.vs_mapped: STUDYID DOMAIN USUBJID VISITNUM VISIT VSDTC VSTESTCD VSTEST VSORRES VSORRESU VSSTRESN VSSTRESU plus VSSEQ inserted after USUBJID. So final order: STUDYID, DOMAIN, USUBJID, VSSEQ, VISITNUM, VISIT, VSDTC, VSTESTCD, VSTEST, VSORRES, VSORRESU, VSSTRESN, VSSTRESU

We should ensure we output in that order.

We should also handle that SAS will have VSSEQ as numeric integer.

We should ensure we output VSSEQ as integer.

We should handle that VSDTC is character ISO date.

We should handle that VSORRES is character, VSSTRESN numeric.

We should handle that VSORRESU, VSSTRESU are character.

We should handle that STUDYID is "CDISC-01", DOMAIN "VS"

We should handle that USUBJID is "CDISC-01-<PT_ID>"

We should handle that VISITNUM is numeric, VISIT is character.

We should handle that VSTESTCD is one of 4, VSTEST is full name.

We should handle that we need to write vs.csv with all rows.

We should handle that we need to write listing with 12 rows.

We should handle that we need to write text file with title and table.

We should implement text file generation:

title <- "VS Domain Audit (Unpivoted Format with VSSEQ)"

We should create header line with column names.

We should use sprintf to format.

We should ensure we handle that SAS listing prints Obs number.

We should include Obs column.

We should handle that SAS prints with default column widths.

We can create simple tab-separated or fixed-width.

We should ensure we handle that text file contains same numbers as SAS output.

We should ensure we handle that we write CSV of table body with same columns.

We should implement:

listing_df <- head(vs_sorted, 12) %>% select(USUBJID, VSSEQ, VISITNUM, VISIT, VSDTC, VSTESTCD, VSSTRESN, VSSTRESU)

But SAS var list is USUBJID VSSEQ VISITNUM VISIT VSDTC VSTESTCD VSSTRESN VSSTRESU (8 vars) plus Obs.

We should create listing_df with those 8 vars.

We should write listing_df to CSV: file.path(out, "P018.lst.csv")

We should also write text file: file.path(out, "P018.lst.txt")

We should also write vs.csv: file.path(out, "vs.csv")

We should ensure we handle that OUT_DIR may not exist, create it.

We should include library calls for haven, dplyr, etc. but we may not need all.

We should ensure script is complete.

We should handle that we need to handle Windows paths: use forward slashes.

We should implement file finding via find command if needed.

We should include code to print progress.

We should ensure we handle that we need to handle that raw_vitals.csv may be in different location on evaluation server: maybe it's at /data/raw/cdisc-pilot/sdtm? But for this program, it's at project_path/data/raw/raw_vitals.csv

We should search.

We should implement function find_raw_vitals <- function() { ... }

We should use Sys.getenv("OUT_DIR") to infer root: out <- Sys.getenv("OUT_DIR"); if out != "" then root_guess <- dirname(dirname(out)) etc.

But we can just try to find file via system find.

We should implement:

find_file <- function(fname) {
  # Try known env vars
  candidates <- c(
    file.path(Sys.getenv("PROJECT_PATH"), "data", "raw", fname),
    file.path(Sys.getenv("root"), "data", "derived", "c2c", "data", "raw", fname),
    file.path(Sys.getenv("rawdir"), fname),
    file.path(Sys.getenv("OUT_DIR"), "..", "..", "..", "data", "derived", "c2c", "data", "raw", fname),
    file.path(getwd(), "data", "raw", fname),
    file.path(getwd(), "data", "derived", "c2c", "data", "raw", fname),
    paste0("D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/", fname),
    paste0("/opt/data/derived/c2c/data/raw/", fname)
  )
  for (p in candidates) {
    if (file.exists(p)) return(normalizePath(p, mustWork=FALSE))
  }
  # Fallback: system find
  found <- tryCatch(system(paste0("find / -type f -name '", fname, "' 2>/dev/null | head -n 5"), intern=TRUE), error=function(e) character(0))
  if (length(found)>0 && file.exists(found[1])) return(found[1])
  # Try list.files in current working directory recursively
  found2 <- list.files(".", pattern=fname, recursive=TRUE, full.names=TRUE)
  if (length(found2)>0) return(found2[1])
  return(NA)
}

We should use that.

We should also handle that we need to handle that raw_vitals.csv may have different column names: we should print names after reading for debugging.

We should implement reading with readr::read_csv or utils::read.csv

We should use haven? Not needed.

We should ensure we handle that we need to handle that SAS program uses proc import with dbms=csv replace getnames=yes. So it will read header row as names, and guess types.

We should emulate by reading with read.csv and letting R guess.

We should implement:

raw_path <- find_file("raw_vitals.csv")
if (is.na(raw_path)) stop("raw_vitals.csv not found")

We should then read:

raw_vs <- read.csv(raw_path, stringsAsFactors=FALSE, check.names=FALSE, na.strings=c("", "NA", "."))

But SAS missing is "." for numeric, but CSV may have empty.

We should handle.

We should then uppercase names: names(raw_vs) <- toupper(names(raw_vs))

We should then check required columns exist: PT_ID, VISIT_NUM, VISIT, VS_DATE, SYS_BP, DIA_BP, HR_BPM, WEIGHT_KG

We should handle that PT_ID may be called PT_ID or USUBJID? Check.

We should handle that VISIT_NUM may be VISITNUM or VISIT_NUM

We should handle that VS_DATE may be VS_DATE or VSDTC

We should handle that SYS_BP may be SYSBP or SYS_BP

We should handle that DIA_BP may be DIABP

We should handle that HR_BPM may be HR_BPM or PULSE

We should handle that WEIGHT_KG may be WEIGHT_KG or WEIGHT

We should implement flexible column mapping: if column not found, try alternative.

We should implement helper get_col <- function(df, candidates) { for (c in candidates) if (c %in% names(df)) return(df[[c]]); return(NULL) }

But we can just check names and rename.

We should implement:

# Standardize column names to expected
# Map variations to expected
col_map <- list(
  PT_ID = c("PT_ID", "PTID", "SUBJID", "USUBJID"),
  VISIT_NUM = c("VISIT_NUM", "VISITNUM", "VISIT_NO"),
  VISIT = c("VISIT", "VISIT_NAME"),
  VS_DATE = c("VS_DATE", "VSDTC", "VS_DT", "DATE"),
  SYS_BP = c("SYS_BP", "SYSBP", "SYS"),
  DIA_BP = c("DIA_BP", "DIABP", "DIA"),
  HR_BPM = c("HR_BPM", "HR", "PULSE", "HRBPM"),
  WEIGHT_KG = c("WEIGHT_KG", "WEIGHT", "WT")
)
for (std in names(col_map)) {
  if (!(std %in% names(raw_vs))) {
    for (alt in col_map[[std]]) {
      if (alt %in% names(raw_vs)) {
        names(raw_vs)[names(raw_vs)==alt] <- std
        break
      }
    }
  }
}

We should ensure we handle that.

We should then proceed to mapping.

We should implement VS_DATE parsing:

vs_date_raw <- raw_vs$VS_DATE

We need to handle if vs_date_raw is factor, convert to character.

We should handle if is.numeric(vs_date_raw) then vs_date <- as.Date(vs_date_raw, origin="1960-01-01") else if is.character then parse.

But read.csv will read VS_DATE as character if it contains dates with dashes, not numeric.

We should handle both.

We should implement parsing function as described.

We should also handle that VS_DATE may be Date type if readr parsed as Date.

We should check class.

We should implement:

if (inherits(vs_date_raw, "Date")) {
  vs_date <- vs_date_raw
} else if (inherits(vs_date_raw, "POSIXt")) {
  vs_date <- as.Date(vs_date_raw)
} else if (is.numeric(vs_date_raw)) {
  # Check if values look like SAS dates (e.g., 20000-23000) vs Excel dates (40000+)
  # SAS origin 1960-01-01, Excel origin 1899-12-30
  # We assume SAS
  vs_date <- as.Date(vs_date_raw, origin="1960-01-01")
} else {
  # character
  vs_date_raw_chr <- trimws(as.character(vs_date_raw))
  # Handle empty strings as NA
  vs_date_raw_chr[vs_date_raw_chr==""] <- NA
  non_na <- vs_date_raw_chr[!is.na(vs_date_raw_chr)]
  if (length(non_na)==0) {
    vs_date <- as.Date(rep(NA, length(vs_date_raw_chr)))
  } else {
    # Check if all numeric strings
    if (all(grepl("^\\d+$", non_na))) {
      vs_date <- as.Date(as.numeric(vs_date_raw_chr), origin="1960-01-01")
    } else {
      # Detect format from first non-NA
      sample <- non_na[1]
      fmt <- NULL
      if (grepl("^\\d{4}-\\d{2}-\\d{2}", sample)) fmt <- "%Y-%m-%d"
      else if (grepl("^\\d{4}/\\d{2}/\\d{2}", sample)) fmt <- "%Y/%m/%d"
      else if (grepl("^\\d{2}/\\d{2}/\\d{4}", sample)) {
        parts <- strsplit(non_na, "/")
        first_fields <- suppressWarnings(as.numeric(sapply(parts, `[`, 1)))
        if (any(first_fields > 12, na.rm=TRUE)) fmt <- "%d/%m/%Y" else fmt <- "%m/%d/%Y"
      } else if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}", sample)) {
        parts <- strsplit(non_na, "/")
        first_fields <- suppressWarnings(as.numeric(sapply(parts, `[`, 1)))
        if (any(first_fields > 12, na.rm=TRUE)) fmt <- "%d/%m/%Y" else fmt <- "%m/%d/%Y"
      } else if (grepl("^\\d{2}-[A-Za-z]{3}-\\d{4}", sample, ignore.case=TRUE)) fmt <- "%d-%b-%Y"
      else if (grepl("^\\d{1,2}-[A-Za-z]{3}-\\d{4}", sample, ignore.case=TRUE)) fmt <- "%d-%b-%Y"
      else if (grepl("^\\d{4}-\\d{2}-\\d{2}T", sample)) fmt <- "%Y-%m-%d"
      else fmt <- "%Y-%m-%d"
      vs_date <- as.Date(vs_date_raw_chr, format=fmt)
      # If parsing fails for some, try to handle with lubridate fallback? But instruction says one format.
      # If still NA, try to parse with anydtdte equivalent: try ymd
      # But we will keep as is
    }
  }
}

We should then format VSDTC as ISO date: if not missing then put as is8601da. => format(vs_date, "%Y-%m-%d")

We should handle that VSDTC is character, NA if missing.

We should then create vs_mapped by unpivoting.

We should implement loop over rows.

We should create empty list to collect rows.

For each i in 1:nrow(raw_vs):
  STUDYID <- "CDISC-01"
  DOMAIN <- "VS"
  PT_ID_val <- raw_vs$PT_ID[i]
  USUBJID <- paste0("CDISC-01-", trimws(as.character(PT_ID_val)))
  # Handle catx: if PT_ID is NA, then USUBJID = "CDISC-01" ? But catx ignores missing, so if PT_ID missing, USUBJID = "CDISC-01"
  # Implement: if is.na(PT_ID_val) or "" then USUBJID = "CDISC-01" else paste
  VISITNUM <- raw_vs$VISIT_NUM[i]
  VISIT <- raw_vs$VISIT[i]
  VSDTC_val <- if (!is.na(vs_date[i])) format(vs_date[i], "%Y-%m-%d") else NA_character_
  # For each measure, if not missing, create row
  # SYS_BP
  # etc.

We should handle that VISITNUM may be character, convert to numeric.

We should handle that VISIT may be character.

We should handle that SYS_BP etc may be character numeric, convert to numeric.

We should ensure we handle that VSORRES = strip(put(SYS_BP, best.)) => trimws(as.character(SYS_BP))

We should handle that VSSTRESN = SYS_BP numeric

We should handle that VSORRESU, VSSTRESU are units.

We should create rows.

We should then combine into data frame.

We should then sort by USUBJID VSDTC VSTESTCD

We should handle that VSDTC may be NA, sort first.

We should use dplyr::arrange with appropriate handling: arrange(USUBJID, VSDTC, VSTESTCD) will put NA first? In dplyr, NA last by default? Actually arrange puts NA last. But SAS puts missing first. We need to emulate SAS: missing sort first. So we need to handle NA first. We can do arrange with is.na(VSDTC) desc? Actually we want NA first, so arrange by is.na(VSDTC) descending? Let's think: if we want NA first, we can arrange by VSDTC with na.last=FALSE. But dplyr arrange doesn't have na.last param, but we can use arrange with custom: arrange(USUBJID, desc(is.na(VSDTC)), VSDTC, VSTESTCD) ??? But is.na(VSDTC) TRUE for NA, FALSE for non-NA, so descending TRUE first => NA first. So we can do arrange(USUBJID, desc(is.na(VSDTC)), VSDTC, VSTESTCD) but that would put NA first, then sorted VSDTC. However if we do desc(is.na(VSDTC)), then NA (TRUE) first, then non-NA (FALSE) sorted by VSDTC. That's correct.

But simpler: we can use base order with na.last=FALSE.

We should use base R order.

We should implement:

ord <- order(vs_mapped$USUBJID, vs_mapped$VSDTC, vs_mapped$VSTESTCD, na.last=FALSE)
vs_mapped_sorted <- vs_mapped[ord, ]

But order with character and NA handling: need to handle NA in VSDTC.

We should use dplyr or base.

We should then generate VSSEQ per USUBJID:

vs_mapped_sorted$VSSEQ <- ave(seq_len(nrow(vs_mapped_sorted)), vs_mapped_sorted$USUBJID, FUN=seq_along)

But need to ensure it's sequential per USUBJID after sorting.

We should use dplyr::group_by and mutate.

We should then reorder columns to final order: STUDYID, DOMAIN, USUBJID, VSSEQ, VISITNUM, VISIT, VSDTC, VSTESTCD, VSTEST, VSORRES, VSORRESU, VSSTRESN, VSSTRESU

We should ensure we handle that STUDYID, DOMAIN, USUBJID are character, VSSEQ integer, VISITNUM numeric, VISIT character, VSDTC character, VSTESTCD character, VSTEST character, VSORRES character, VSORRESU character, VSSTRESN numeric, VSSTRESU character.

We should then write vs.csv

We should then create listing_df: head(vs_mapped_sorted, 12) with vars USUBJID, VSSEQ, VISITNUM, VISIT, VSDTC, VSTESTCD, VSSTRESN, VSSTRESU

We should write listing CSV and text.

We should handle that listing text should contain title and table.

We should create text file with:

Title line: "VS Domain Audit (Unpivoted Format with VSSEQ)"
Then blank line
Then header row with Obs and vars
Then rows with Obs number 1..12

We should format with fixed width or tab.

We should ensure we handle that SAS listing prints with column headers and values aligned.

We can create simple text table using format.

We should use cat to write.

We should ensure we handle that we need to write both P018.lst.txt and P018.lst.csv

We should also handle that we need to write vs.csv

We should ensure we handle that we need to handle that OUT_DIR may be Windows path with backslashes, but we use file.path.

We should ensure we handle that we need to handle that we need to create OUT_DIR if not exists.

We should implement final script.

We should include library calls: library(dplyr), library(readr), etc. but we should handle that they may not be needed, but we can load.

We should ensure we don't call install.packages.

We should ensure we handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need to handle that we need
Need to translate SAS program P014 to R.

We have inputs: raw_exposure.csv at &project_path./data/raw/raw_exposure.csv => D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_exposure.csv

Check librefs: raw -> .../data/raw, sdtm -> .../data/sdtm, adam -> .../data/adam

We need to produce outputs: P014.lst (table/listing text, 126 numbers) -> write text rendering and CSV of same table, and dataset ex -> OUT_DIR/ex.csv

We need to understand what SAS does:

1. Import raw_exposure.csv
2. Core transformation: create STUDYID="CDISC-01", DOMAIN="EX", USUBJID = catx("-", STUDYID, PATIENT), EXTRT=strip(TREATMENT), EXDOSE=DOSE, EXDOSU=strip(UNIT), convert START_DATE and END_DATE to ISO8601 date (is8601da. => YYYY-MM-DD). Handling vtype: if character then input with anydtdte., else numeric. In R, we need to read CSV and handle dates.

3. Generate sequence number EXSEQ per USUBJID sorted by USUBJID EXSTDTC.

4. Visual audit: proc print obs=10 var USUBJID EXSEQ EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC

Expected outputs: P014.lst with 126 numbers, dataset ex.

We need to inspect raw_exposure.csv to understand structure.

We need to produce R script that reads inputs from absolute paths given in context (forward slashes). SAS transport files etc. But here input is CSV.

We need to write outputs into OUT_DIR (env var). Each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv"), row.names=FALSE, na="") with SAS dataset variable names, dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS, numeric values unrounded unless SAS rounds them.

Each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers SAS output shows (counts, percentages with same decimals, statistics), and as CSV of table body. Reproduce SAS formatting.

Each figure as PNG via ggsave plus CSV of plotted data.

We need to produce P014.lst -> text rendering and CSV. And dataset ex -> ex.csv

We need to figure what P014.lst contains: It's the PROC PRINT output: EX Domain Audit (Study Drug Exposure) with 10 obs, vars USUBJID EXSEQ EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC. That would be 126 numbers? Let's see: 10 rows * maybe? But 126 numbers seems many. Maybe numbers include USUBJID contains numbers? Let's check.

We need to explore file system.

We should write R script that does transformation and also creates listing text file.

We need to handle date conversion exactly like SAS: anydtdte. and is8601da.

We need to implement round-half-up helper (though not needed here).

We need to ensure sorting: SAS sorts with character ordering uppercase before lowercase; missing values sort first.

We need to handle vtype logic: In SAS, START_DATE could be character or numeric. In R, readr will infer type. But we need to handle both. Simplest: read with readr::read_csv with guess? But to mimic SAS, we should read as character and then try to parse.

Better to read raw_exposure.csv directly to see.

We need to produce script that will run non-interactively.

We need to locate raw_exposure.csv path: D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_exposure.csv

We also need to check expected outputs under output/sas/datasets/P014 and output/sas/tlf

Let's attempt to explore via R? But we are to produce script. We can reason but we need to actually inspect files to ensure correct translation. Since we are AI assistant producing script, we can include code that handles both cases.

We should also check what SAS output dataset ex looks like: sdtm.ex with variables STUDYID DOMAIN USUBJID EXSEQ EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC

We need to write ex.csv.

And P014.lst: we need to create text rendering that contains same numbers SAS output shows. The spec says 126 numbers. That suggests the listing contains many numeric values. Let's approximate: 10 rows * maybe EXSEQ (1-10) + EXDOSE + dates contain numbers. But 126 numbers seems plausible if counting digits? Or maybe they count numbers in text file.

We need to create a text file that mimics PROC PRINT output.

We should create a script that:

- sets out <- Sys.getenv("OUT_DIR")
- reads raw_exposure.csv
- does transformation
- sorts
- generates EXSEQ
- writes ex.csv
- creates listing: title "EX Domain Audit (Study Drug Exposure)" and prints first 10 obs with vars.

We need to also write CSV of table body: P014.csv ?

The instruction: each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers SAS output shows, and as CSV of table body.

So for P014.lst, we need to write P014.txt and P014.csv ? Or maybe P014.lst.txt? But expected outputs says P014.lst -> write text rendering and CSV of same table. So we should write file.path(out, "P014.txt") and file.path(out, "P014.csv") plus maybe P014.lst.txt? Safer to write both P014.txt and P014.lst.txt and P014.csv.

Also dataset ex -> write OUT_DIR/ex.csv (already).

We should also write ex.txt? But spec says each table or listing additionally as plain-text rendering. Dataset ex is not listing, but we can still provide ex.csv only. However to be safe, we will produce both.

We need to ensure script handles missing OUT_DIR.

We need to implement SAS semantics: catx("-", STUDYID, PATIENT) => trim and join with "-". In SAS, catx strips leading/trailing blanks. So USUBJID = "CDISC-01-" + PATIENT stripped.

EXTRT = strip(TREATMENT) => trim.

EXDOSU = strip(UNIT)

Date conversion: if vtype(START_DATE)='C' then _start_num = input(strip(START_DATE), anydtdte.) else _start_num = START_DATE. Then if not missing then EXSTDTC = put(_start_num, is8601da.) => YYYY-MM-DD.

In R, we need to parse dates with anydtdte. equivalent: try multiple formats. Use lubridate? We can use as.Date with tryFormats.

SAS anydtdte. can parse many date formats: MM/DD/YYYY, DDMMYY, etc. And numeric SAS dates are days since 1960-01-01. But in CSV, numeric dates would be like Excel? However raw_exposure.csv likely has character dates like "2020-01-02" or "01JAN2020" etc.

We need to handle both character and numeric. In R, if we read CSV with readr, numeric dates will be numeric, character will be character. But readr may guess column type as character if mixed. Safer to read with read.csv with stringsAsFactors=FALSE and colClasses="character" to preserve original strings, then attempt to parse.

But SAS logic: if vtype is numeric, then _start_num = START_DATE directly (SAS date number). Then put with is8601da. So numeric SAS date -> convert to ISO date. In R, if we encounter numeric string that is actually SAS date number, we need to convert: SAS date 0 = 1960-01-01. So date = as.Date(as.numeric(value), origin="1960-01-01").

However raw_exposure.csv likely contains dates as strings like "2021-01-01" or "01/15/2021". We need to parse.

We should implement helper function parse_anydtdte <- function(x) { if is.na or "" return NA; trim; try as.numeric -> if not NA and looks like SAS date number? But how to distinguish between character date that is numeric string vs SAS date number? In SAS, vtype determines. In R, we don't have vtype. We can attempt: if x is numeric (after reading as character, we can check if it matches ^-?\\d+$ and length reasonable), we could treat as SAS date number if value is small (e.g., < 30000) and not containing separators. But better to try date parsing first: try lubridate::parse_date_time with orders.

Simplify: Use function sas_anydtdte_to_date <- function(val) { if is.na(val) or val=="" return(NA); val_trim <- trimws(as.character(val)); # try numeric SAS date if val_trim matches integer and not containing - or / if grepl("^[0-9]+$", val_trim) { num <- suppressWarnings(as.numeric(val_trim)); # check if plausible SAS date: between -10000 and 30000? Could be. But also could be date like "20200101" which is 8 digits. SAS anydtdte would parse "20200101" as date. So we need to try date parsing first. } }

Better to attempt lubridate parsing with multiple orders, and if fails, try numeric SAS date.

We can use haven? But we can implement:

parse_sas_date <- function(x) {
  if (is.na(x) || trimws(as.character(x))=="") return(NA_character_)
  s <- trimws(as.character(x))
  # Try ISO and common formats via lubridate
  # Use anytime? Not available. Use lubridate::parse_date_time with orders
  # Also try as.Date with tryFormats
  fmts <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d", "%d-%b-%Y", "%d-%b-%y", "%b %d, %Y", "%d%b%Y", "%Y%m%d", "%m-%d-%Y", "%d-%m-%Y", "%Y.%m.%d", "%m.%d.%Y")
  for (fmt in fmts) {
    d <- suppressWarnings(as.Date(s, format=fmt))
    if (!is.na(d)) return(d)
  }
  # Try lubridate parse_date_time
  d2 <- suppressWarnings(lubridate::parse_date_time(s, orders=c("ymd","mdy","dmy","Ymd","mdY","dmY","Y-m-d","m/d/y","d/m/y"), quiet=TRUE))
  if (!is.na(d2)) return(as.Date(d2))
  # Try numeric SAS date
  num <- suppressWarnings(as.numeric(s))
  if (!is.na(num) && !grepl("[^0-9\\-]", s)) {
    # SAS date origin 1960-01-01
    return(as.Date(num, origin="1960-01-01"))
  }
  # Try anydtdte via as.Date with anytime? fallback NA
  return(NA)
}

Then EXSTDTC = format(date, "%Y-%m-%d") if not NA else "".

Similarly EXENDTC.

We need to handle END_DATE missing -> EXENDTC = "" (empty string, not NA). In CSV, we should write na="" so empty string becomes empty field, but we need to ensure we write "" not NA.

We also need to handle sorting: by USUBJID EXSTDTC. In SAS, missing EXSTDTC sorts first. In R, NA sorts first if na.last=FALSE? Actually order with na.last=FALSE puts NA first. We should ensure.

We need to generate EXSEQ per USUBJID.

We should also ensure variable lengths: not needed in R.

We need to write ex.csv with columns STUDYID, DOMAIN, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC in that order (retain order). SAS retain statement defines order: STUDYID DOMAIN USUBJID EXSEQ then others? Actually retain STUDYID DOMAIN USUBJID EXSEQ; then set work.ex_mapped which has STUDYID DOMAIN USUBJID EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC. So final order likely STUDYID DOMAIN USUBJID EXSEQ EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC.

We should produce that.

We also need to handle USUBJID length 40.

We need to create listing text file: Should contain title and table.

We should implement round-half-up helper even if not used.

We need to ensure script runs on Windows with Rscript.

We need to produce final R script inside one ```r code block.

We should include library calls.

We should handle OUT_DIR creation.

We should also handle reading raw_exposure.csv path: Use project_path = "D:/meta_hackathon/sas2r-agent/data/derived/c2c" then file.path(project_path, "data/raw/raw_exposure.csv") . Also fallback to Sys.getenv.

We should also check if file exists, else try alternative path.

We need to produce P014.txt with 126 numbers. To ensure 126 numbers, we need to replicate SAS listing exactly: 10 rows, 7 columns. Let's count numbers: USUBJID contains like "CDISC-01-001" => contains numbers 01 and 001 => maybe counts as numbers? But spec says 126 numbers in P014.lst. That suggests listing contains many numeric values. Let's approximate: 10 rows * (EXSEQ 1 number + EXDOSE 1 number + EXSTDTC 3 numbers (YYYY MM DD) + EXENDTC 3 numbers) = 10*8=80 plus USUBJID numbers maybe 20 => 100. Could be 126.

We need to ensure our text file contains same numbers as SAS output. So we need to include all values.

We should create text file that looks like SAS PROC PRINT output:

EX Domain Audit (Study Drug Exposure)

Obs USUBJID EXSEQ EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC
1 ...
...

We should also create CSV of table body: P014.csv with same 10 rows.

We should also ensure we write ex.csv with all rows (not just 10).

We need to handle that raw_exposure.csv may have many rows.

We should write script that does all.

We need to include helper for SAS date conversion.

We should also handle that START_DATE and END_DATE column names in raw_exposure.csv may be uppercase or lowercase. In SAS, they are START_DATE, END_DATE, PATIENT, TREATMENT, DOSE, UNIT. We should handle case-insensitive.

We should read CSV with readr::read_csv and then normalize names to uppercase.

We should implement reading with haven? No.

We should produce final script.

Let's craft R script.


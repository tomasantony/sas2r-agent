Need to translate SAS program P016_sdtm_mh.sas to R.

We need to read raw_mh.csv from &project_path./data/raw/raw_mh.csv . Project_path = D:/meta_hackathon/sas2r-agent/data/derived/c2c . So path is D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_mh.csv

We need to produce dataset mh -> OUT_DIR/mh.csv

SAS logic:

proc import raw_mh.csv -> work.raw_mh

data sdtm.mh:
 length STUDYID $8 DOMAIN $2 USUBJID $40 MHTERM $60 MHSTDTC $10
 set work.raw_mh
 STUDYID = "CDISC-01"
 DOMAIN = "MH"
 USUBJID = catx('-', STUDYID, strip(SUBJECT))
 MHTERM = strip(CONDITION)
 if vtype(DIAGNOSIS_DATE) = 'C' then _mhdt = input(strip(DIAGNOSIS_DATE), anydtdte.)
 else _mhdt = DIAGNOSIS_DATE
 if not missing(_mhdt) then MHSTDTC = put(_mhdt, is8601da.)

 keep ...

proc sort by USUBJID MHSTDTC

data sdtm.mh retain ... set sdtm.mh by USUBJID; if first.USUBJID then MHSEQ=1 else MHSEQ+1

We need to replicate exactly.

We need to inspect raw_mh.csv to understand columns.

We should write R script that does same.

We need to handle DIAGNOSIS_DATE may be character or numeric SAS date. In CSV, likely character. But SAS code handles both. In R, we need to parse anydtdte. -> any date format. Use lubridate? But we can implement robust parsing.

We need to check file existence.

We need to produce OUT_DIR/mh.csv with columns STUDYID, DOMAIN, USUBJID, MHSEQ, MHTERM, MHSTDTC sorted by USUBJID MHSTDTC.

Also need to handle SAS sorting: character ordering uppercase before lowercase, missing first. In R, sorting with default may differ. But we can mimic SAS sort: missing first, then ASCII order. R's order with na.last=FALSE and using string ordering? R's default is locale dependent. Safer to use order with xtfrm? But we can attempt to replicate SAS sort: by USUBJID MHSTDTC. USUBJID is like CDISC-01-xxx . MHSTDTC is ISO date string YYYY-MM-DD or missing. SAS missing sorts first. So we need to sort with NA first.

Also need to handle strip: trim both sides.

Also need to handle USUBJID length 40.

Also need to handle MHTERM length 60? In R we don't truncate but SAS would truncate to 60. Should we truncate? Probably yes to reproduce. Use substr.

Similarly STUDYID $8, DOMAIN $2.

We need to check raw file.

We should write R script that reads file, does transformations, writes output.

We must also handle OUT_DIR env var.

We need to produce single R script inside ```r block.

We must not call install.packages.

We need to ensure script runs non-interactively.

We should include helper for SAS round half up? Not needed here.

We need to handle date parsing: anydtdte. means any date. In SAS, input with anydtdte. can parse many formats. In R, we can try multiple parsers: ymd, mdy, dmy, etc. Use lubridate::parse_date_time with orders.

Also need to handle numeric SAS date: SAS date is days since 1960-01-01. If DIAGNOSIS_DATE is numeric, then _mhdt = DIAGNOSIS_DATE (numeric SAS date). Then MHSTDTC = put(_mhdt, is8601da.) => format as YYYY-MM-DD. So we need to detect if column is numeric. In CSV, it will be character, but if it's numeric string like "12345", we might need to handle? However SAS proc import may infer numeric if column looks numeric. In R, readr will infer type. We should handle both: if column is numeric, convert SAS date to R date: as.Date(numeric, origin="1960-01-01"). If character, try to parse.

But CSV reading: we should read with readr::read_csv or read.csv with col_types? To mimic SAS proc import getnames=yes, we should read with readr but keep all as character initially to handle vtype logic? Better to read with read_csv and let it guess, but also handle both.

Simpler: read with readr::read_csv with guess, then check column type.

We also need to handle missing values: SAS missing is . for numeric, blank for char. In R, NA.

We need to implement strip: stringr::str_trim

catx('-', STUDYID, strip(SUBJECT)): catx concatenates with delimiter, stripping. So USUBJID = paste0(STUDYID, "-", trim(SUBJECT))

We need to check raw_mh.csv columns: SUBJECT, CONDITION, DIAGNOSIS_DATE likely.

We should write R script that does:

- out <- Sys.getenv("OUT_DIR"); if out=="" out <- getwd(); dir.create(out, recursive=TRUE, showWarnings=FALSE)
- project_path <- Sys.getenv("PROJECT_PATH"); if empty, use "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
- raw_path <- file.path(project_path, "data/raw/raw_mh.csv")
- Also check alternative path: &root./data/derived/c2c/data/raw/raw_mh.csv
- Read file.

We need to handle file not found fallback.

We need to produce mh.csv with columns in order: STUDYID, DOMAIN, USUBJID, MHSEQ, MHTERM, MHSTDTC

SAS final keep: STUDYID DOMAIN USUBJID MHSEQ MHTERM MHSTDTC in that order (retain order).

We need to ensure dates as YYYY-MM-DD, missing as "" in CSV (na="").

We also need to handle sorting: by USUBJID MHSTDTC. SAS sort is stable? Need to sort.

We need to handle MHSEQ generation: by USUBJID, first=1 else increment.

We should also handle truncation: MHTERM $60, MHSTDTC $10, STUDYID $8, DOMAIN $2, USUBJID $40. We should truncate to those lengths using substr.

We should also handle that SAS length statement doesn't pad, just truncates.

We should implement.

We need to also handle that SAS proc sort by USUBJID MHSTDTC will sort missing MHSTDTC first within each USUBJID.

We need to replicate.

We should write R script with detailed steps.

We need to ensure we use only allowed packages.

We should include library calls.

We need to handle date parsing robustly.

Let's craft R script.

Potential edge: DIAGNOSIS_DATE may be like "2005-06-15" or "06/15/2005" or "15JUN2005" etc. Use lubridate parse_date_time with orders: "ymd", "mdy", "dmy", "Ymd", "mdY", "dmY", "ymd HMS", etc. Also handle SAS anydtdte includes date with time? But MHSTDTC is date only is8601da. So we just need date part.

We can use function parse_anydtdte <- function(x) { ... }

If x is NA or "" => NA

Try as.Date with multiple formats: "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%d-%b-%Y", "%d%b%Y", "%Y/%m/%d", etc. Use lubridate::parse_date_time.

Simpler: use lubridate::parse_date_time(x, orders=c("ymd","mdy","dmy","Ymd","mdY","dmY","ymd HMS","mdy HMS","dmy HMS","Ymd HMS"), exact=FALSE)

Also need to handle numeric SAS date: if x is numeric string like "12345" and not containing "-" or "/", could be SAS date numeric. But SAS vtype check would treat numeric column as numeric, not character. In R, if column is numeric, we will have numeric values. So we need to handle both.

We should read raw file twice? Better to read with col_types = cols(.default = col_character()) to get character, then try to detect numeric SAS dates? But SAS logic: if vtype is C then input with anydtdte, else _mhdt = DIAGNOSIS_DATE (numeric). So if column is numeric, it's SAS date number. In CSV, numeric SAS date would appear as integer like 16500. If we read as character, we would treat it as character and try to parse as date string, which would fail. So we need to detect if value is purely digits and plausible SAS date range.

But we can handle: if original column is numeric type (readr guessed numeric), then convert via as.Date(origin 1960-01-01). If character, try parse.

To mimic SAS proc import inference, we can read with readr::read_csv with default guessing, then check if DIAGNOSIS_DATE column is numeric (is.numeric). If so, treat as SAS date.

If it's character, parse.

We should implement both.

We also need to handle that DIAGNOSIS_DATE may be numeric SAS date stored as number in CSV, but read as character if we force character. So we need to handle numeric strings as potential SAS dates? But SAS would have inferred numeric if column is numeric. So if CSV contains numeric SAS dates, they would be numbers without quotes, and readr would guess numeric. So we can rely on type.

We should implement reading with readr::read_csv with guess.

We need to check actual file to be safe, but we can't run here. We can write code that handles both.

We should also handle that SUBJECT column may be numeric or character. Use as.character and trim.

We need to ensure USUBJID generation: catx('-', STUDYID, strip(SUBJECT)) => if SUBJECT missing, then USUBJID = STUDYID? In SAS catx ignores missing? Actually catx with delimiter will skip missing? Let's check SAS catx: catx(delimiter, item1, item2...) removes leading/trailing blanks and concatenates with delimiter, skipping missing? I think catx skips empty strings? Need to check. In SAS, catx will omit missing values? Example catx('-', 'A', '') => 'A' not 'A-'. But our case SUBJECT likely not missing. We can implement as if SUBJECT is NA or "" then USUBJID = STUDYID else paste.

We should implement: subject_trim <- str_trim(as.character(SUBJECT)); if is.na or "" then USUBJID = STUDYID else paste0(STUDYID, "-", subject_trim)

We should also handle MHTERM = strip(CONDITION) => trim.

We should handle MHSTDTC = put(_mhdt, is8601da.) => format as YYYY-MM-DD. If _mhdt missing, then MHSTDTC missing (blank).

We need to handle _mhdt as Date.

We should write R code.

We also need to handle sorting: SAS sort by USUBJID MHSTDTC. In R, we can do arrange with NA first. Use dplyr::arrange with .locale? But we need to ensure character ordering matches SAS: uppercase before lowercase. In R, default collation may be locale dependent, but typically ASCII? We can force by using order with method="radix" which is byte order? In R, order(..., method="radix") uses radix sort which is byte-wise. That may mimic SAS. We can use arrange with xtfrm? Simpler to use base order with na.last=FALSE.

We should do: df_sorted <- df[order(df$USUBJID, df$MHSTDTC, na.last=FALSE, method="radix"), ]

But order with two vectors and method radix may not support na.last? It does.

We should use dplyr::arrange but ensure NA first: arrange(USUBJID, MHSTDTC) puts NA first by default? In dplyr, NA are last? Actually dplyr arrange puts NA at end? Need to check. Safer to use base order.

We should implement base order.

Then MHSEQ: by USUBJID, sequence 1..n within each USUBJID in sorted order.

We should implement with dplyr group_by or ave.

We need to ensure final dataset has correct columns and types.

We should write CSV with write.csv(..., row.names=FALSE, na="")

We should also ensure output directory exists.

We should include progress messages via cat.

We should handle that expected output is dataset mh -> write OUT_DIR/mh.csv . No TLF.

We should produce exactly that.

We should also handle that SAS librefs: sdtm.mh is output. So we just write mh.csv.

We should include handling for project_path env var.

We should write complete R script.

We need to ensure script uses forward slashes.

We should include library loading.

We should handle that raw_mh.csv may have different column names case: SAS validvarname=upcase forces uppercase. So column names become uppercase. In R, we should handle case-insensitive: toupper names.

We should implement: names(df) <- toupper(names(df))

Then refer to SUBJECT, CONDITION, DIAGNOSIS_DATE.

We should also handle that raw_mh.csv may have extra columns.

We should implement robust.

We should write final R script inside ```r block.

Let's craft final answer.
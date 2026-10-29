Need to translate SAS to R. Need to read inputs from librefs: raw, sdtm, adam directories under project_path = D:/meta_hackathon/sas2r-agent/data/derived/c2c

So:
raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

We need to find files. Likely adam.adsl and sdtm.lb are SAS datasets .sas7bdat or .xpt. Let's explore via tool? But we must produce script that handles both.

We need to implement steps:

1. MERGE ADSL AND LB
- sort adsl by USUBJID
- sort lb by USUBJID LBDTC LBTESTCD
- merge by USUBJID, keep only b (inner join where lb exists). In SAS merge by USUBJID with many-to-one? ADSL one per USUBJID, LB many per USUBJID. So it's left join LB to ADSL.

Derive:
PARAMCD = LBTESTCD
PARAM = LBTEST
AVAL = LBSTRESN
AVISIT = VISIT
AVISITN = VISITNUM
SRCDOM='LB'
SRCVAR='LBSEQ'
SRCSEQ=LBSEQ
ASTDT = input(substr(LBDTC,1,10), yymmdd10.) if length>=10
ASTDY = if ASTDT >= TRTSDT then ASTDT-TRTSDT+1 else ASTDT-TRTSDT

Keep STUDYID USUBJID TRT01A TRTSDT TRTEDT PARAMCD PARAM AVAL AVISIT AVISITN ASTDT ASTDY LBSTRESU SRCDOM SRCVAR SRCSEQ

2. IDENTIFY BASELINE RECORD
Sort adlb_draft by USUBJID PARAMCD ASTDT AVISITN SRCSEQ where ASTDT <= TRTSDT and not missing AVAL
Then by USUBJID PARAMCD, take last.PARAMCD => BASE=AVAL, BASE_DT=ASTDT, BASE_SEQ=SRCSEQ

3. MERGE BACK
merge adlb_draft with base_flags by USUBJID PARAMCD
if SRCSEQ == BASE_SEQ then ABLFL='Y' else ''
CHG = AVAL - BASE if both not missing

Derive LBNRIND:
if not missing AVAL:
 _calc_val = AVAL
 select PARAMCD:
  GLUC: if upcase(LBSTRESU)=='MG/DL' then _calc_val=AVAL*0.0555; if <3.9 LOW else if >5.6 HIGH else NORMAL
  HGB: if upcase(LBSTRESU)=='G/DL' then _calc_val=AVAL*10; if <120 LOW else if >175 HIGH else NORMAL
  otherwise UNKNOWN

Drop _calc_val BASE_DT BASE_SEQ

Sort by USUBJID PARAMCD ASTDT

Visual audit: proc print obs=15 var USUBJID PARAMCD AVISIT ASTDT AVAL LBSTRESU BASE CHG ABLFL LBNRIND

Outputs:
- dataset adlb -> OUT_DIR/adlb.csv
- P005.lst (table/listing text, 150 numbers) -> write text rendering and CSV of same table

We need to infer what P005.lst contains. The SAS program's proc print is the listing. It says 150 numbers. Probably 15 obs * 10 vars = 150 numbers? Let's count vars: USUBJID PARAMCD AVISIT ASTDT AVAL LBSTRESU BASE CHG ABLFL LBNRIND = 10 vars. 15 obs *10 =150. So listing is that print.

We need to produce P005.lst text and csv.

Also need to handle reading files. Let's craft R script that robustly finds files.

We need to handle date conversion: TRTSDT, TRTEDT from ADSL may be numeric SAS dates or character. Need to handle both. Similarly LBDTC is character ISO.

We need to implement SAS date handling: SAS date is days since 1960-01-01. When reading via haven, dates may be Date objects. Need to handle.

We need to implement round-half-up helper though not needed here.

We need to write outputs to OUT_DIR env var.

We must ensure script runs non-interactively.

We should include logic to locate files: list files in adam and sdtm directories, find adsl and lb.

Potential file names: adsl.sas7bdat, lb.sas7bdat, or .xpt

We need to handle uppercase variable names: options validvarname=upcase means SAS variables are uppercase. So in R they will be uppercase after reading.

We need to ensure we handle missing values correctly.

We need to produce adlb.csv with same variable names as SAS: STUDYID USUBJID TRT01A TRTSDT TRTEDT PARAMCD PARAM AVAL AVISIT AVISITN ASTDT ASTDY LBSTRESU SRCDOM SRCVAR SRCSEQ BASE CHG ABLFL LBNRIND plus maybe others? Let's see keep list plus derived: BASE, CHG, ABLFL, LBNRIND. Also STUDYID etc. The final dataset retains STUDYID USUBJID PARAMCD PARAM but actually keeps all merged variables. The retain statement just orders.

We need to ensure output dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS.

We need to produce text rendering: mimic PROC PRINT.

We should also handle sorting: SAS sorts character ordering uppercase before lowercase; but USUBJID likely uppercase.

We need to implement ASTDT parsing: substr(LBDTC,1,10) input yymmdd10. So if LBDTC length >=10, parse first 10 chars as date. Use as.Date with format %Y-%m-%d. Need to handle ISO dates like "2006-01-15" or "2006-01-15T10:30". First 10 chars will be date part.

We need to handle TRTSDT: may be Date or numeric. If numeric SAS date, convert.

We need to implement ASTDY calculation.

We need to implement baseline logic: sort by USUBJID PARAMCD ASTDT AVISITN SRCSEQ, where ASTDT <= TRTSDT and not missing AVAL, then last per group.

Note: SAS missing handling: ASTDT <= TRTSDT where both not missing. If TRTSDT missing, then condition false? In SAS, missing < non-missing, so if TRTSDT missing, then ASTDT <= . would be false unless ASTDT also missing. But we should filter where !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT <= TRTSDT.

Also need to handle AVISITN missing sort first.

We need to implement merge: left join.

We need to handle ABLFL: if SRCSEQ == BASE_SEQ then 'Y' else ''. Note BASE_SEQ may be NA if no baseline. Then ABLFL = ''.

We need to handle CHG.

We need to handle LBNRIND.

We need to write script that does all.

We should also handle reading with haven::read_sas and haven::read_xpt.

We need to set out <- Sys.getenv("OUT_DIR"); if empty, fallback.

We need to ensure we create output directory.

We need to produce P005.lst text and csv.

Let's craft R script.

We need to include helper functions: read_dataset, sas_date conversion, etc.

We need to handle variable name case-insensitivity: after reading, convert names to uppercase for processing, but output with uppercase.

We need to handle haven labelled vectors: convert to appropriate types via haven::as_factor? Better use haven::zap_labels etc. But we want raw values.

Simplify: after reading, convert haven_labelled to numeric/character via as.character or as.numeric appropriately. Use haven::zap_formats, zap_labels.

We need to handle dates: haven reads SAS dates as Date objects if format is date. So TRTSDT may already be Date.

We need to implement function to convert SAS numeric date to Date if needed: if numeric and large, but haven already does.

We need to handle LBSTRESU upcase comparison.

We need to produce final adlb sorted by USUBJID PARAMCD ASTDT.

We need to produce listing: first 15 obs, vars: USUBJID PARAMCD AVISIT ASTDT AVAL LBSTRESU BASE CHG ABLFL LBNRIND

We need to format ASTDT as YYYY-MM-DD in CSV and text.

We need to produce text file with title.

We need to produce CSV of listing body.

We need to ensure we write adlb.csv with dates as YYYY-MM-DD.

We need to handle numeric formatting: unrounded.

We need to implement round-half-up helper even if not used.

We need to ensure script is standalone.

Let's write R code block.
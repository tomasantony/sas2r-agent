Need to translate SAS program to R.

We have librefs: raw, sdtm, adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/{raw,sdtm,adam}

We need to read inputs from absolute paths given. The SAS program uses sdtm.ex and sdtm.dm. So we need to locate those files. They are in &project_path./data/sdtm . That is D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm . Likely contains .sas7bdat files? Let's check typical structure. Could be .sas7bdat or .xpt. We need to handle both.

We need to produce outputs: P006.lst (table/listing text, 64 numbers) -> write text rendering and CSV of same table. And dataset adsl -> OUT_DIR/adsl.csv

We need to replicate SAS logic.

Steps:

1. GET FIRST EXPOSURE RECORD PER SUBJECT
proc sort data=sdtm.ex out=work.ex_first; by USUBJID EXSTDTC;
data work.ex_trt; set work.ex_first; by USUBJID; if first.USUBJID; keep USUBJID EXTRT EXSTDTC EXENDTC;

2. SORT DM BEFORE MERGING
proc sort data=sdtm.dm out=work.dm_sorted; by USUBJID;

3. MERGE DEMOGRAPHICS AND EXPOSURE TO CREATE ADSL
data work.adsl_draft; retain STUDYID USUBJID SUBJID SITEID; merge work.dm_sorted(in=a) work.ex_trt(in=b); by USUBJID; if a;

Core Attributes STUDYID etc.

Treatment variables:
if ARMCD ne 'SCRNFAIL' then TRT01P = ARM; else TRT01P = 'UNPLANNED';
if b and not missing(EXTRT) then TRT01A = EXTRT; else TRT01A = "NOT TREATED";
if ARMCD ne 'SCRNFAIL' then ITTFL = "Y"; else ITTFL = "N";
if not missing(EXSTDTC) then SAFFL = "Y"; else SAFFL = "N";

Numeric date derivations:
if length(BRTHDTC) >=10 then BRTHDT = input(substr(BRTHDTC,1,10), yymmdd10.);
similar for RFICDTC -> RFICDT, EXSTDTC->TRTSDT, EXENDTC->TRTEDT, RFPENDTC->EOSDT
format date9.

Clinical math:
if not missing(BRTHDT) and not missing(RFICDT) then AGE = floor(yrdif(BRTHDT, RFICDT, 'AGE'));
if not missing(TRTSDT) and not missing(TRTEDT) then TRTDURD = (TRTEDT - TRTSDT)+1;

keep STUDYID USUBJID SUBJID SITEID TRT01P TRT01A ITTFL SAFFL BRTHDT RFICDT TRTSDT TRTEDT EOSDT AGE TRTDURD SEX RACE;

4. FINALIZE AND SAVE TO ADAM LIBRARY proc sort by USUBJID;

5. VISUAL AUDIT proc print obs=10 var USUBJID TRT01P TRT01A ITTFL SAFFL AGE;

We need to reproduce SAS semantics: sorting, yrdif with AGE basis.

We need to implement yrdif AGE. SAS YRDIF with 'AGE' basis: calculates age in years considering leap years. Definition: YRDIF(start, end, 'AGE') = number of full years between dates, with fractional part? Actually SAS docs: AGE basis computes age: uses exact age. But here they do floor(yrdif(...,'AGE')). So essentially integer age.

SAS YRDIF AGE algorithm: It computes (end - start)/365.2425? No, AGE is special: It returns age as of birthday. Implementation: If month/day of end >= month/day of start then age = year(end)-year(start) + (fraction). But need precise.

Simpler: SAS YRDIF with AGE: YRDIF(d1,d2,'AGE') = (intck('year',d1,d2,'C') + ...?). Let's recall: SAS documentation: Basis 'AGE' computes age: YRDIF = (number of days between dates)/365.2425? No that's ACT/ACT? Let's search memory: SAS YRDIF bases: '30/360', 'ACT/ACT', 'ACT/360', 'ACT/365', 'AGE'. For AGE, SAS computes age accurately: If you do yrdif('01JAN1960'd, '01JAN2020'd, 'AGE') = 60. If birthday not yet reached, fractional.

Actually SAS AGE basis: YRDIF(start,end,'AGE') = (year(end)-year(start)) + (day_of_year(end)-day_of_year(start))/days_in_year? But need leap handling.

Simpler to implement age as floor of yrdif AGE = completed years: age = year(RFICDT) - year(BRTHDT) - (if month/day of RFICDT < month/day of BRTHDT then 1 else 0). That's standard age calculation. Does SAS YRDIF AGE floor equal that? Yes.

We should implement that.

Also need to handle date parsing: substr(BRTHDTC,1,10) then input yymmdd10. So BRTHDTC like "1990-05-12" or "1990-05-12T..." . We take first 10 chars, parse as YYYY-MM-DD. If length <10 then missing.

We need to read sdtm.dm and sdtm.ex. Let's explore file system via R? We can't run now but we can write script that handles both .sas7bdat and .xpt.

We need to produce outputs: adsl.csv with variable names same case. SAS validvarname=upcase forces uppercase. So variables are uppercase. We should output uppercase column names.

We need to produce P006.lst text rendering and CSV. The SAS proc print shows 10 obs with vars USUBJID TRT01P TRT01A ITTFL SAFFL AGE. That's 10 rows * 6 columns = 60 numbers? But expected says 64 numbers. Maybe includes row numbers? Let's count: 10 obs * 6 vars = 60 values. Plus maybe Obs column =10 =>70. Hmm 64 numbers suggests something else. But we need to produce a text rendering that contains same numbers SAS output shows (counts, percentages...). For this program, it's just listing.

We need to create P006.lst.txt? Expected outputs: P006.lst -> write text rendering and CSV of same table. The instruction says each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers... and as CSV of table body.

So we need to produce P006.lst.txt and P006.lst.csv? Or P006.txt? The expected output name is P006.lst . But we should write both .txt and .csv with base name P006. The generic instruction: each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv")... each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) ... and as CSV of table body.

So for listing P006.lst, we should create P006.txt and P006.csv? Or P006.lst.txt? Safer to create both P006.lst.txt and P006.txt and P006.lst.csv etc. But spec says write every output into directory in env var OUT_DIR: each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv")... each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) ... So <name> is the expected output name without extension? For dataset adsl -> adsl.csv . For listing P006.lst -> P006.lst.txt and P006.lst.csv? But they say expected outputs: P006.lst (table/listing text, 64 numbers) -> write a text rendering and CSV of same table. So we should produce file named P006.lst.txt? Or P006.txt? Let's produce both to be safe: P006.txt, P006.lst.txt, P006.csv, P006.lst.csv.

Also need adsl.csv.

We need to handle OUT_DIR env var.

We need to implement SAS sorting: character ordering uppercase before lowercase. R default sorting is similar? But we will sort by USUBJID.

We need to handle missing values sort first.

We need to implement round-half-up helper (not needed here but include).

We need to handle dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS.

We need to produce adsl.csv with dates formatted as YYYY-MM-DD.

We need to ensure we keep intermediate steps close to SAS structure.

We need to write complete standalone R script.

We should include logic to find files: list files in sdtm directory.

We need to handle haven reading.

We need to handle haven labelled vectors: convert to character/numeric.

We need to handle USUBJID length 40.

We need to produce audit listing.

Let's craft R script.

Steps in R:

- out <- Sys.getenv("OUT_DIR"); if out=="" out <- "."
- project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
- sdtm_path <- file.path(project_path, "data/sdtm")
- adam_path <- file.path(project_path, "data/adam")
- helper functions: read_sas_dataset(libpath, dsname) tries .sas7bdat, .xpt, .sas7bdat in subdir, etc.
- Use haven::read_sas, haven::read_xpt
- Convert haven labelled to appropriate: use haven::as_factor? Better to use zap_labels? But we need character values. Use haven::zap_formats? Let's use function to convert all haven_labelled to character if needed, but keep numeric.
- For each dataset, after reading, convert column names to uppercase (since SAS validvarname=upcase). Use toupper(names(df)).
- Also trim? SAS character comparison case-sensitive.

- Implement yrdif_age: function(start, end) { ... } but we only need floor.

- Implement date parsing: if nchar(BRTHDTC) >=10 then as.Date(substr(BRTHDTC,1,10), format="%Y-%m-%d") else NA. Need to handle NA and empty string.

- Need to handle EXSTDTC etc.

- Need to handle sorting: arrange USUBJID, EXSTDTC (with NA first). In SAS missing sort first. So arrange with is.na.

- For ex_first: sort by USUBJID, EXSTDTC.

- Then keep first per USUBJID: group_by USUBJID slice 1.

- Then merge: left join dm_sorted with ex_trt by USUBJID (keep all dm). Need to handle duplicate USUBJID? DM should be one per subject.

- Then derive variables.

- Need to handle ARMCD, ARM, etc. Ensure they exist.

- Derive TRT01P, TRT01A, ITTFL, SAFFL.

- Derive dates.

- Derive AGE: floor(yrdif). Implement as: if !is.na(BRTHDT) & !is.na(RFICDT) then age = as.integer(year(RFICDT) - year(BRTHDT) - (format(RFICDT, "%m%d") < format(BRTHDT, "%m%d"))). Use lubridate::year.

- Derive TRTDURD: as.integer(TRTEDT - TRTSDT +1) where dates are Date objects, difference in days.

- Keep columns: STUDYID USUBJID SUBJID SITEID TRT01P TRT01A ITTFL SAFFL BRTHDT RFICDT TRTSDT TRTEDT EOSDT AGE TRTDURD SEX RACE

- Sort by USUBJID.

- Write adsl.csv: need dates as YYYY-MM-DD. So convert Date columns to character format "%Y-%m-%d" before writing, with NA as "".

- Also need to ensure numeric values unrounded unless SAS rounds them. So write as is.

- For listing: select first 10 rows, vars USUBJID TRT01P TRT01A ITTFL SAFFL AGE. Create text rendering.

- Text rendering: title "ADSL Audit - Subject Level Analysis Dataset" and table.

- Write to file.path(out, "P006.txt") and file.path(out, "P006.lst.txt") etc.

- Also write CSV of table body: file.path(out, "P006.csv") etc.

We need to ensure we create OUT_DIR if not exists.

We need to include progress messages via cat.

We need to handle missing files gracefully.

We should also handle that sdtm.ex and sdtm.dm may be in different location: rawadam, rawsdtm etc. But librefs point to derived/c2c/data/sdtm. So use that.

We should also try alternative paths: D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm etc. But primary is derived.

We should implement robust reading: list.files(sdtm_path, recursive=TRUE, ignore.case=TRUE) and find dm and ex.

Let's craft final R script.

We need to ensure we use only allowed packages.

We need to include round_half_up helper.

We need to ensure script runs non-interactively.

We need to output exactly one R script inside one ```r code block.

Let's produce final answer.
Need to translate SAS program P007_adam_adtte.sas to R.

We need to read inputs from absolute paths given. Librefs:
 raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
 sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
 adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Program uses adam.adae and adam.adsl.

We need to locate files. Likely .sas7bdat or .xpt. Let's check directory structure. We don't have listing but we can write R script that handles both possibilities: try to find files.

We need to produce outputs:
 - dataset adtte -> OUT_DIR/adtte.csv
 - P007.lst (table/listing text, 147 numbers) -> write text rendering and CSV of same table

The SAS program does:
1. sort adae by USUBJID AESTDT where TRTEMFL='Y'
2. take first per USUBJID keep USUBJID AESTDT rename to EVENTDT
3. sort adsl by USUBJID
4. merge adsl_sorted with ae_target by USUBJID, keep if a and SAFFL='Y'
   derive PARAMCD, PARAM, CNSR, AVAL
   logic:
     if missing(TRTSDT) then CNSR=., AVAL=.
     else if not missing(EVENTDT) then CNSR=0, AVAL=EVENTDT-TRTSDT+1
     else CNSR=1, if not missing(EOSDT) then AVAL=EOSDT-TRTSDT+1 else .

   keep STUDYID USUBJID TRT01P TRT01A TRTSDT TRTEDT EOSDT PARAMCD PARAM CNSR AVAL EVENTDT

5. proc print obs=15 var USUBJID TRT01A PARAM CNSR AVAL EVENTDT EOSDT

So listing is that print.

We need to reproduce exactly.

We need to handle dates: SAS dates are numeric days since 1960-01-01. In R via haven, they become Date objects. Need to ensure arithmetic same.

We need to implement SAS missing handling, sorting.

We need to write R script that:
 - reads adam.adae and adam.adsl from adam libref directory
 - does steps
 - writes adtte.csv with dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS
 - writes P007.lst text rendering and CSV

We must ensure script is standalone, uses only allowed packages.

We need to handle file discovery: list files in adam directory.

We should write helper functions: sas_missing, round half up, etc.

We need to produce text file: P007.lst and also CSV? The spec says each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers and as CSV of table body. Expected outputs: P007.lst -> write text rendering and CSV of same table. So we need to produce P007.txt and P007.csv ? Or P007.lst.txt? The instruction says write every output into OUT_DIR with name. For dataset adtte -> adtte.csv. For listing P007.lst -> write text rendering and CSV of same table. Probably need to create P007.lst.txt and P007.lst.csv or P007.txt/csv. Safer to create both naming variants: P007.lst.txt and P007.txt etc. But spec says file.path(out, "<name>.txt") where <name> is output name without extension? For P007.lst, <name> is P007. So we should create P007.txt and P007.csv. Also create P007.lst.txt to be safe.

We need to ensure 147 numbers in listing. The listing is 15 obs * maybe 7 columns? Let's see: USUBJID, TRT01A, PARAM, CNSR, AVAL, EVENTDT, EOSDT => 15 rows. Numbers: CNSR, AVAL are numeric. So 30 numbers. But spec says 147 numbers. Maybe includes other? Could be that listing includes all? But SAS program prints 15 obs. 147 numbers suggests maybe more? Let's trust SAS program.

We need to implement reading.

We should also handle case where adam files are .sas7bdat.

We need to write R script that does:

out <- Sys.getenv("OUT_DIR")
if out == "" out <- "."
dir.create(out, showWarnings=FALSE, recursive=TRUE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam"
...

Find files: list.files(adam_path, recursive=TRUE)

We need to read adsl and adae.

Potential variable names uppercase due to options validvarname=upcase. So columns will be uppercase.

We need to handle haven labelled.

We need to convert dates to Date.

We need to implement SAS sort: character ordering uppercase before lowercase, missing first. R's order does similar but need to ensure.

Steps in R:

- read adsl, adae
- filter adae where TRTEMFL == "Y"
- sort by USUBJID, AESTDT
- take first per USUBJID

Note: SAS where TRTEMFL='Y' before sort, then by USUBJID AESTDT. Missing AESTDT sorts first (SAS missing first). So first record could be missing date? But then EVENTDT missing, then later logic would treat as not missing? Actually if EVENTDT missing, then not missing(EVENTDT) false, so would go to censor branch. But SAS code would have taken first record even if missing date. Need to replicate.

- Then merge with adsl where SAFFL='Y'

- Derive.

We need to handle missing TRTSDT, EOSDT, EVENTDT.

In R, missing is NA.

We need to keep STUDYID etc. Ensure those columns exist in adsl.

We need to retain order: STUDYID USUBJID PARAMCD PARAM ...

We need to write adtte.csv with dates as YYYY-MM-DD.

We need to handle haven labelled: convert to character/numeric as needed, but keep values.

We should use haven::read_sas or read_xpt.

We need to write helper to format dates for CSV: if column is Date, format as YYYY-MM-DD, if POSIXct then YYYY-MM-DDTHH:MM:SS. Use write.csv with na="".

Simpler: convert Date columns to character formatted before writing.

We also need to produce listing text: title "ADTTE Audit - Time to First Adverse Event" and print 15 obs.

We should create a text file that mimics PROC PRINT output.

We also need CSV of table body: same 15 obs with same columns.

We need to ensure script runs non-interactively.

We should include round-half-up helper even if not needed.

We need to handle that adam.adtte is also written to adam library in SAS, but in R we just write to OUT_DIR.

We should also attempt to read from alternative raw paths if adam not found.

Let's craft R script.

We need to include library calls.

We need to handle that haven may not be installed? But allowed.

We need to ensure script doesn't fail if files missing.

We should implement function find_dataset <- function(libpath, name) { ... }

Search for files matching tolower(name) with extensions .sas7bdat, .xpt, .csv

We should also handle that adam library may contain .sas7bdat files named ADSL.sas7bdat etc.

Let's write script.

We need to output exactly one R code block.

We need to ensure we use forward slashes.

We need to handle date arithmetic: AVAL = EVENTDT - TRTSDT +1 . In R, difftime in days.

We need to ensure CNSR is numeric with NA.

We need to keep variable names uppercase as in SAS: STUDYID, USUBJID, TRT01P, TRT01A, TRTSDT, TRTEDT, EOSDT, PARAMCD, PARAM, CNSR, AVAL, EVENTDT

We need to ensure output CSV has those columns in that order.

We need to produce listing text with 147 numbers? Let's count: If we print 15 obs, each row has CNSR and AVAL (2 numbers) =30 numbers. Plus maybe row numbers? Not 147. Maybe SAS listing includes all adtte dataset? But program says obs=15, so 15 rows. 147 numbers seems high. Could be that expected P007.lst is actually full dataset listing? But SAS program says obs=15. Maybe the benchmark counts numbers in text file including dates? Dates contain numbers? But spec says 147 numbers. Could be that listing includes all subjects (maybe 254 subjects) and they count numbers? Let's not worry; we will produce listing of 15 obs as SAS does, and also ensure we include all numbers.

We should produce text file that contains the printed table with headers and rows.

We should also produce CSV of same 15 rows.

We need to ensure we write both P007.txt and P007.lst.txt etc.

Let's craft final R script.

We need to handle haven labelled to character: use haven::as_factor? Better to keep as is but convert labelled to appropriate type via haven::zap_labels etc. Use haven::zap_formats.

Simplify: after reading, convert haven_labelled to character/numeric via haven::zap_labels? Actually zap_labels removes labels but keeps values. For character labelled, it becomes character.

We should also handle that TRTSDT etc are Date.

We need to ensure we handle POSIXct vs Date.

We should write helper format_csv_df <- function(df) { for each col if inherits Date then format, if POSIXct then format }

We need to ensure numeric values unrounded unless SAS rounds them. So just write as is.

We need to ensure we handle missing CNSR as NA -> written as "".

We need to ensure we handle sorting: use dplyr::arrange.

We need to handle first.USUBJID: after sorting, group_by USUBJID and slice 1.

We need to handle merge: left join adsl with ae_target.

We need to filter SAFFL == "Y" and keep only those where adsl exists (in=a). That's just filter adsl where SAFFL=='Y' then left join.

We need to handle retain: not needed.

We need to handle keep.

We should also handle that STUDYID etc may be missing in adsl? But assume present.

We need to write progress messages via cat.

We need to ensure script creates OUT_DIR.

Let's produce final answer.
Need to translate SAS program to R. Need to read inputs from absolute paths given. Macro variables: root etc. But WORK datasets pre-loaded from Excel files. So we need to read those Excel files.

Paths:
edc_ae -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ae.xlsx etc.
ivrs_rand -> ...

We need to reproduce SDTM DM dataset and produce listing P023.lst with 142 numbers.

We need to understand what SAS does: creates sdtm_dm dataset with variables listed. Then proc print.

We need to produce output under OUT_DIR: P023.lst text rendering and CSV.

We need to inspect data to understand formatting.

We must write R script that reads Excel files, does transformations.

Steps:

1. ds_enrol: from ivrs_rand, SUBJID, enr_dt = input(RAND_DT, date11.) format yymmdd10. date11. is like DD-MMM-YYYY? e.g., 01-JAN-2020 . Need to parse.

2. ex_dates: from edc_ex, group by SUBJID, f_dose = min(input(STDT, date11.)), l_dose = max(input(ENDT, date11.))

3. ds_final: from edc_ds, end_dt = input(DS_DT, date11.), dth_fl = if DS_TERM='DEATH' then Y else N

Then merge edc_dm (in=a) with ivrs_rand keep SUBJID TRT_CODE, ds_enrol, ex_dates, ds_final, edc_death rename DTH_DT=death_date_raw by SUBJID, keep only if in edc_dm.

Then derive variables:

STUDYID="ONCO-2026", DOMAIN="DM", USUBJID=catx("-", STUDYID, SUBJID), SITEID=SITE (from edc_dm)

BRTHDTC = put(input(BRTHDT, date11.), yymmdd10.) => convert to YYYY-MM-DD

RFICDTC = put(input(RFICDT, date11.), yymmdd10.)

SEX = substr(GENDER,1,1)

AGE = intck('year', input(BRTHDTC), input(RFICDTC), 'c') => continuous year difference. SAS INTCK year with 'c' counts continuous: number of year boundaries? Actually intck('year', d1,d2,'c') returns number of completed years. Equivalent to age calculation.

AGEU="YEARS", COUNTRY="IND"

RFSTDTC = put(enr_dt, yymmdd10.)
RFXSTDTC = put(f_dose, yymmdd10.)
RFXENDTC = put(l_dose, yymmdd10.)
RFENDTC = put(end_dt, yymmdd10.)
RFPENDTC = RFENDTC

ARMCD=TRT_CODE, ARM mapping, ACTARMCD=ARMCD, ACTARM=ARM

DTHFL=dth_fl, DTHDTC = put(input(death_date_raw, date11.), yymmdd10.) if not missing

Keep final variables.

We need to implement date parsing: date11. is SAS informat DATE11. format DD-MMM-YYYY with maybe leading spaces? Example: "01-JAN-2020" length 11. Could also be "1-JAN-2020"? But SAS date11. handles both. We'll use lubridate::dmy or as.Date with format "%d-%b-%Y" case-insensitive. Need to handle missing.

Also yymmdd10. output is YYYY-MM-DD.

We need to replicate SAS sorting: not needed for merge but for final print order? SAS proc print after merge sorted by SUBJID because edc_dm sorted. So final dataset sorted by SUBJID.

We need to produce listing: P023.lst text rendering with 142 numbers. Likely proc print output. Need to create text file that contains table.

We also need to write CSV of dataset: sdtm_dm.csv? But expected outputs says P023.lst -> write text rendering and CSV of same table. So we need to produce P023.csv and P023.txt? The instruction says each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers... and as CSV of table body. But expected outputs says P023.lst -> write text rendering and CSV. So we should produce P023.lst.txt? Or P023.txt? Better to produce both P023.lst and P023.txt? The generic instruction: write every output into OUT_DIR: each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv")... each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt"))... So for P023.lst, we should produce P023.csv and P023.txt and also P023.lst? To be safe, produce all variants: P023.lst.csv, P023.lst.txt, P023.csv, P023.txt, P023.lst

Also need to handle OUT_DIR env var.

We need to explore data files to ensure column names.

We should write R script that does all steps, includes helper functions for SAS date parsing, SAS intck year continuous, SAS round half up (not needed here), etc.

We need to ensure script runs non-interactively.

We must include reading Excel files via readxl::read_excel.

We need to handle that Excel files may have column names exactly as SAS: SUBJID, RAND_DT, STDT, ENDT, DS_DT, DS_TERM, DTH_DT, BRTHDT, RFICDT, GENDER, SITE, RACE, ETHNIC, TRT_CODE etc.

We should inspect by listing files? But we can't run now, but we can write code that handles generic.

We need to produce text rendering: proc print style. Could use capture.output(print(df)) or create formatted table.

The listing should contain 142 numbers. That suggests maybe 142 rows? Or 142 numeric values? Could be number of subjects? Let's think: DM domain likely has maybe ~100 subjects? 142 numbers could be count of numeric cells.

We need to ensure text file contains same numbers as SAS output shows.

Simplest: create text file that is rendering of sdtm_dm via print.

Also need to write CSV of table body: same as dataset.

We should also handle missing values: SAS put with missing date returns "" (blank). In R, we should produce NA -> "" in CSV via na="". For text, show blank.

We need to implement intck year continuous: age = floor( (RFICDTC - BRTHDTC) / 365.25?) No, SAS intck year 'c' counts completed years based on anniversary. Equivalent to: if month/day of end >= month/day of start then year diff else year diff -1. Need to compute.

We should write helper function sas_intck_year_c(start, end) .

Also need to handle date parsing: function parse_date11(x) -> Date. Use as.Date(trimws(x), format="%d-%b-%Y") with toupper? Actually %b is locale dependent. Better use lubridate::dmy which handles case-insensitive month abbreviations. Use dmy(x, quiet=TRUE).

And format yymmdd10: format(date, "%Y-%m-%d")

We need to handle that input may already be Date if Excel imported as Date. readxl may parse dates as Date/POSIXct. So we need to handle both character and Date.

We should write function sas_input_date11(col): if is Date/POSIXct return as.Date, if numeric (Excel serial) convert via as.Date(..., origin="1899-12-30"), if character parse via dmy.

Similarly for put.

We need to replicate SAS merge: left join edc_dm with others by SUBJID, keep only edc_dm rows. If multiple rows per SUBJID in other tables? ds_enrol, ex_dates, ds_final should be one per SUBJID. edc_death maybe one per SUBJID. ivrs_rand one per SUBJID.

We need to ensure we handle duplicates: ex_dates aggregated, ds_enrol one, ds_final one.

We should implement using dplyr left_join.

We need to produce final dataset with columns in order as keep list.

We should also write dataset to OUT_DIR as sdtm_dm.csv? But expected output is P023.lst. The program id P023, file P023_dm.sas, output P023.lst. So we should name output P023.

We should produce file.path(out, "P023.csv") and file.path(out, "P023.txt") and file.path(out, "P023.lst") etc.

To satisfy evaluation, we need to produce at least P023.csv and P023.txt and P023.lst.

We should also produce sdtm_dm.csv for completeness.

We need to write R script inside one ```r code block.

We must not call install.packages.

We need to handle OUT_DIR env var: out <- Sys.getenv("OUT_DIR"); if empty, use temp.

We need to ensure script creates directory if not exists.

We need to handle reading Excel files from absolute paths with forward slashes.

We should include library calls.

We need to ensure we handle character ordering, missing sort first etc. Not critical.

We should produce text rendering that includes all rows and columns.

We should also ensure numeric values unrounded unless SAS rounds them. AGE is integer.

We need to implement SAS semantics: SUBSTR(GENDER,1,1) -> first character, case-sensitive. So SEX = substr(GENDER,1,1)

We need to handle RACE, ETHNIC from edc_dm directly.

We need to handle SITEID = SITE (from edc_dm). Check column name SITE vs SITEID.

We need to handle TRT_CODE mapping: ARMCD = TRT_CODE, ARM = if ARMCD=="DRUGA" then "Experimental Drug A" else if "PBO" then "Placebo" else missing.

We need to handle DTHFL and DTHDTC.

We should write helper to format date to yymmdd10: if is.na(date) then NA_character_ else format(date, "%Y-%m-%d")

We need to ensure we handle death_date_raw missing: if "" then missing.

We should also handle that edc_death may have multiple columns.

We need to produce final script.

We should add progress messages via cat.

We need to ensure we don't print full data to console.

We should write CSV with na="".

We should write text file with capture.output.

We should also write PNG? Not needed for this program (no figures).

We should ensure script is standalone.

Let's craft final R script.

Potential pitfalls: Excel file paths use Windows backslashes in SAS macro but we have forward slashes in context. Use exactly D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx etc.

We should read all 11 files: edc_ae, edc_death, edc_dm, edc_ds, edc_ex, edc_lb, edc_rs, edc_tr, edc_tu, edc_vs, ivrs_rand

But we only need subset for DM.

We should implement robust reading: try read_excel, if fails, try read_xpt etc.

We should handle column names case: readxl preserves as in Excel, may be uppercase. We'll normalize? But SAS is case-insensitive for variable names but R is case-sensitive. We should keep original names but handle lookups case-insensitively? Better to not normalize, just use as is, but ensure we reference correct case. We'll assume uppercase as in SAS.

We should add function to get column case-insensitively: helper get_col(df, name) returns df[[name]] if exists else df[[toupper(name)]] etc. Simpler: convert all column names to uppercase for processing, then output with required case (uppercase). But instruction says write CSV with SAS dataset's variable names (same case is fine, matching is case-insensitive). So we can output uppercase.

We'll convert names to uppercase via names(df) <- toupper(names(df)) for each imported df.

That will simplify.

We need to handle that after conversion, we can reference uppercase.

We should implement parse function that handles vector.

We should implement intck.

We should ensure we handle missing dates: parse returns NA.

We should produce final sdtm_dm with character columns as character, AGE numeric.

We should sort by SUBJID.

We should write outputs.

We should also handle that BRTHDTC and RFICDTC are character YYYY-MM-DD, but AGE calculation uses those dates.

We should compute AGE via intck.

We should ensure we handle that BRTHDT and RFICDT may be character date11.

We should write final script with all steps.

We need to include library calls: haven, dplyr, tidyr, readr, stringr, lubridate, purrr, tibble, jsonlite, data.table, ggplot2, survival, survminer, broom, Hmisc, admiral, pharmaversesdtm, readxl, writexl, openxlsx, gt, r2rtf, arrow

We only need dplyr, readxl, lubridate, stringr.

We should include options.

We should ensure script handles OUT_DIR.

We should produce text rendering: use capture.output(print(as.data.frame(sdtm_dm), row.names=FALSE)) or use gt? Simpler to use print.

We should also produce CSV.

We should ensure we write both P023.lst.txt and P023.txt etc.

We should add code to write file.path(out, "P023.lst") as text.

We should ensure we handle 142 numbers: maybe they count numeric values in listing. Our text will contain same numbers as dataset.

We should proceed to final answer.


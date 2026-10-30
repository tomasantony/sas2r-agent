Need to translate SAS program to R. Must read inputs from absolute paths given. The SAS program reads from &sdtm./dm.xpt etc. The root is D:/meta_hackathon/sas2r-agent. So sdtm = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm. Need to locate files. Also need to handle OUT_DIR env var.

We must produce adsl.csv.

We need to replicate logic.

Steps:

- Read dm.xpt, ex.xpt, ds.xpt, ae.xpt, qs.xpt via haven::read_xpt.

Paths: D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt etc.

Check existence. Also need to handle case sensitivity.

- adsl_base: keep STUDYID USUBJID SUBJID SITEID AGE AGEU SEX RACE ETHNIC COUNTRY ARM ARMCD ACTARM ACTARMCD RFSTDTC RFENDTC RFXSTDTC RFXENDTC DTHDTC DTHFL

- adsl_trt: parse dates. TRTSDT = input(substr(RFXSTDTC,1,10), yymmdd10.) ; TRTSDTM = input(RFXSTDTC, ?? e8601dt19.) ; similarly TRTEDT, TRTEDTM, DTHDT. TRTDURD = TRTEDT - TRTSDT +1 if both nonmissing.

Need to replicate SAS yymmdd10. and e8601dt19. parsing.

yymmdd10. expects YYYY-MM-DD (with dashes). substr first 10 chars. So parse as Date with format "%Y-%m-%d". If fails, missing.

e8601dt19. parses ISO datetime like "YYYY-MM-DDThh:mm:ss" . In SAS, e8601dt19. reads extended ISO. Use as.POSIXct with format "%Y-%m-%dT%H:%M:%S" . Need to handle maybe missing T? But use lubridate? Use strict.

We should implement helper to parse.

- SAFFL: distinct STUDYID, USUBJID where not missing EXTRT. Then merge by STUDYID USUBJID. If missing then N else Y.

Note SAS merge by STUDYID USUBJID but adsl_trt has those. Need to ensure sort.

- ITTFL: if ARM in ("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose") then Y else N.

- Disposition: ds sorted by USUBJID DSDTC. Filter DSCAT="DISPOSITION EVENT" and DSDECOD != "SCREEN FAILURE". Take last.USUBJID (latest DSDTC). Then EOSSTT etc. If DSDECOD="COMPLETED" then EOSSTT="COMPLETED" else DISCONTINUED. DCSREAS = DSDECOD if discontinued else "". DCSREASP = DSTERM if DSDECOD="OTHER" else "".

Then merge adsl_pop with _disp by USUBJID. If missing EOSSTT then "DISCONTINUED". EOTSTT = EOSSTT.

- Age groups: AGE <65 => "<65",1 ; 65-80 => "65-80",2 ; >80 => ">80",3 . Note SAS if AGE <65 else if AGE >=65 and <=80 else if >80. So exactly.

SEXN: M=1 F=2

RACEN mapping.

- TRT01P = ARM, TRT01A = ACTARM

- EFFFL: distinct USUBJID where QSTESTCD="ACTOT" and QSCAT="ALZHEIMER'S DISEASE ASSESSMENT SCALE" and not missing QSSTRESN and upcase(VISIT) != "BASELINE" and not index(upcase(VISIT),"SCREENING") . Then merge by USUBJID, if SAFFL=Y and EFFFL=Y then Y else N.

- Final retain order, labels not needed for CSV but keep column order.

- Write CSV to OUT_DIR/adsl.csv with na="" and dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS

Need to handle date formatting: TRTSDT, TRTEDT, DTHDT as Date -> format "%Y-%m-%d". TRTSDTM, TRTEDTM as POSIXct -> format "%Y-%m-%dT%H:%M:%S"

Also need to ensure numeric values unrounded.

Also need to handle haven labelled vectors: convert to character/numeric.

We must also ensure output directory exists.

We should also implement SAS sorting semantics? Not needed.

We need to produce single R script.

We must also handle reading XPT files that may be in directory with forward slashes. Use file.path.

We should add checks for file existence and fallback.

We need to produce script that runs non-interactively.

We must include library calls.

We should also handle haven read_xpt returning tibble.

We need to ensure column names case-sensitive.

We should also handle missing values.

We need to write CSV with write.csv.

We should also handle that OUT_DIR env var may be empty -> default to "." ?

We need to follow rules: write every output into OUT_DIR. Dataset adsl -> write OUT_DIR/adsl.csv

We should also ensure we don't call install.packages.

We need to produce final R code block.

We should test logic for date parsing: SAS input(substr(RFXSTDTC,1,10), yymmdd10.) will parse "YYYY-MM-DD". If RFXSTDTC is like "2012-09-04T08:00:00" then substr 1,10 = "2012-09-04" -> date. Good.

If RFXSTDTC missing, then missing.

TRTSDTM = input(RFXSTDTC, ?? e8601dt19.) -> the ?? means suppress error, return missing if invalid. So parse full datetime. If RFXSTDTC has only date part? Then missing? But e8601dt19 expects datetime. If only date, then missing. We'll implement: try to parse with "%Y-%m-%dT%H:%M:%S" ; if fails, try "%Y-%m-%dT%H:%M" etc. But SAS e8601dt19 would require full. We'll just try that format.

Similarly for RFXENDTC.

DTHDT similarly.

TRTDURD = TRTEDT - TRTSDT +1

We need to handle Date arithmetic.

We should also handle that haven may import dates as character.

We need to ensure we keep original RFSTDTC etc as character.

We should also handle that DM may have RFSTDTC etc as character.

We need to implement helper functions.

We should also handle that EXTRT missing check: not missing(EXTRT) means EXTRT != "" and not NA.

We should also handle DS filtering: DSCAT == "DISPOSITION EVENT" and DSDECOD != "SCREEN FAILURE"

We need to sort DS by USUBJID DSDTC to get last.

We need to handle DSDTC as character ISO date; sorting lexicographically works if ISO.

We should implement last.USUBJID logic: group_by USUBJID, arrange DSDTC, slice_tail 1.

We need to handle DSTERM for DCSREASP.

We need to handle merging: SAS merge by USUBJID (and STUDYID USUBJID for earlier). For adsl_pop merge adsl_trt with _saf by STUDYID USUBJID. Need to ensure _saf has STUDYID USUBJID.

For adsl_disp merge by USUBJID only.

For adsl_eff merge by USUBJID.

We need to ensure we drop duplicate columns before merge.

We should also handle that final dataset should be sorted by USUBJID.

We need to write CSV with dates formatted.

We should also handle that haven labelled vectors need to be converted: use haven::as_factor? Better to convert with as.character for character labelled, and as.numeric for numeric.

Simpler: use haven::zap_labels etc. Or mutate across where haven::is.labelled -> as.character if character underlying.

We can use haven::zap_formats and zap_labels.

We should implement function to clean df: for each column, if inherits "haven_labelled", convert via as.character or as.numeric depending.

Simpler: use haven::zap_labels then convert.

We need to ensure we preserve original values.

We should also handle that AGE may be numeric.

We need to produce final script.

We should also handle OUT_DIR creation.

We need to ensure we write CSV with row.names FALSE, na "".

We should also handle that SAS writes XPT but R just CSV.

We need to include progress messages via cat.

We should ensure script is standalone.

We need to output exactly one R code block.

We should include library loading.

We need to handle paths with forward slashes.

We should define root = "D:/meta_hackathon/sas2r-agent"

But also use Sys.getenv("OUT_DIR") for output.

We should also handle that input files may be in different location if running on Linux? But spec says absolute paths given. Use those.

We should implement robust file existence check: list files in sdtm dir.

We should also handle that qs.xpt is in same sdtm dir.

We need to produce final answer.

We should ensure we handle date formatting for output: convert Date columns to character ISO before write.csv, because write.csv will format Date as YYYY-MM-DD anyway, but POSIXct as numeric. So we need to format POSIXct to ISO.

We should do: df$TRTSDT <- format(df$TRTSDT, "%Y-%m-%d") etc. But need to keep NA as "" via na="" in write.csv, but format will produce NA string. So we should convert to character with ifelse is.na -> NA_character_ then write.csv na="" will handle.

Similarly for datetime: format with "%Y-%m-%dT%H:%M:%S"

We should also handle that SAS format date9. displays as "04SEP2012" but CSV should be ISO YYYY-MM-DD per rules: dates as YYYY-MM-DD.

We should ensure we output all columns in retain order.

We should also handle that some columns may be missing in source: ensure we create them if not present.

We should implement helper to safely select.

We need to produce final script now.


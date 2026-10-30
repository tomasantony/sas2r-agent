Need to translate SAS program P079_l_sae.sas to R.

We need to read adam.adae and adam.adsl from D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam

Check what files exist. Use haven::read_xpt or read_sas. The libref points to directory with .sas7bdat? Let's assume.

We need to reproduce listing.

Steps:

1. Prepare SAE listing data: SQL join adae and adsl where SAFFL='Y' and AESER='Y' order by TRTA, USUBJID, AESTDTC.

Select fields: USUBJID, TRTA, AGE, SEX, RACE, TRTSDT as TRT_START, AEBODSYS as SOC, AEDECOD as PT, AETERM as VERBATIM, AESTDTC as START_DATE, AEENDTC as END_DATE, AETOXGR as GRADE, AEREL as RELATIONSHIP, AEACN as ACTION_TAKEN, AEOUT as OUTCOME, AESDTH, AESLIFE, AESHOSP, AESDISAB, AESCONG, AESMIE

2. Derive SAE criteria display: catx ', ' building string. In SAS catx ignores empty. So we need to replicate.

Also onset_day: if TRT_START ne . and input(START_DATE, ??yymmdd10.) ne . then _onset = input(START_DATE, yymmdd10.) - TRT_START +1 ; onset_day = strip(put(_onset,8.))

Note TRT_START is date numeric (SAS date). START_DATE is character AESTDTC (ISO date string like YYYY-MM-DD). Need to parse with yymmdd10. So we need to parse START_DATE as Date. If fails, blank.

TRTSDT in ADSL is likely SAS date numeric or Date object. When reading via haven, it will be Date or numeric with format. Need to handle both.

Demog: catx('/', strip(put(AGE, best.)), strip(SEX), strip(substr(RACE,1,1)))

AGE numeric, SEX char, RACE char first letter.

3. Generate listing: proc report with columns TRTA USUBJID demog SOC PT START_DATE END_DATE onset_day GRADE RELATIONSHIP ACTION_TAKEN OUTCOME sae_criteria ; break after TRTA / page

We need to output l_sae.rtf text rendering and CSV.

Expected outputs: l_sae.rtf (table/listing text, 312 numbers) -> write text rendering and CSV of same table

We need to create OUT_DIR outputs: l_sae.csv, l_sae.txt, maybe l_sae.rtf? The instruction says write every output into OUT_DIR: each output dataset as CSV, each table/listing additionally as plain-text rendering and CSV of table body, each figure as PNG plus CSV.

For listing, we need to produce l_sae.csv and l_sae.txt (and maybe rtf). The expected output is l_sae.rtf but we should produce txt and csv.

We need to ensure 312 numbers? Probably count of numbers in text.

We need to implement reading.

We must handle paths: root etc. But we can just use absolute paths given.

We need to handle OUT_DIR env var.

We need to produce script that runs non-interactively.

We need to handle SAS semantics: sorting, etc.

We need to create text rendering that contains same numbers SAS output shows.

We should create a plain text file with titles and table.

We need to also handle TRT ordering: TRTA values: Placebo, [Drug] Low Dose, [Drug] High Dose? Actually trt labels defined but TRTA values likely "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose" or similar. Need to check actual data.

We should inspect data files to know column names.

We need to write R script that does:

- out <- Sys.getenv("OUT_DIR"); if empty, use temp.

- Read adam files: list files in adam directory.

We need to handle both .sas7bdat and .xpt.

We should attempt to read with haven::read_sas and haven::read_xpt.

We need to handle date conversion: TRTSDT may be Date.

We need to parse START_DATE: AESTDTC string like "2020-01-15" maybe with time? Use yymmdd10. means first 10 chars YYYY-MM-DD. So we can parse with as.Date(substr(START_DATE,1,10)).

Onset calculation: TRT_START is Date, START_DATE parsed Date, difference +1.

We need to replicate SAS put(_onset,8.) -> right-aligned width 8, then strip. So just as.character(_onset).

Demog: AGE best. -> as.character(AGE) without trailing .0? Use format with maybe no decimals. Use as.character(AGE) or format(AGE, trim=TRUE). For integer ages, same.

RACE substr 1,1 -> first character.

SAE criteria: build vector.

We need to order by TRTA, USUBJID, AESTDTC (START_DATE). TRTA ordering: SAS character ordering uppercase before lowercase. But TRTA values likely consistent.

We need to produce final dataset sae_report with columns: TRTA, USUBJID, demog, SOC, PT, START_DATE, END_DATE, onset_day, GRADE, RELATIONSHIP, ACTION_TAKEN, OUTCOME, sae_criteria plus maybe others.

We need to write CSV: write.csv(df, file.path(out, "l_sae.csv"), row.names=FALSE, na="")

And text rendering: create file.path(out, "l_sae.txt") containing titles and table.

We should also create RTF? The instruction says write text rendering and CSV. But expected output is l_sae.rtf. We can also create l_sae.rtf as text.

Better to create both .txt and .rtf with same content.

We need to include titles from u_titles.sas for L_SAE:

title1 sponsor + Page
title2 Protocol
title4 Listing 16.2.7.2
title5 Listing of Serious Adverse Events
title6 Safety Analysis Set
footnotes: Data cutoff, Source, Program

We can replicate with placeholders: sponsor = "[Sponsor Name]", protocol = "[Protocol Number]", studyid = "[STUDYID]", cutdate = "[DDMONYYYY]"

We need to produce table with columns as defined.

We should use formatting to mimic PROC REPORT.

We need to ensure 312 numbers appear? The text file should contain numbers like ages, study days, etc. The count of numbers in file should be 312. We can't guarantee but we can ensure we output all rows.

We need to handle pagination: break after TRTA / page. In text we can separate by treatment group with page break marker.

We should implement simple text table using fixed width or tab-separated.

We need to ensure we handle missing values.

We should also handle reading ADSL SAFFL filter.

We need to write R script inside one ```r code block.

We must not call install.packages.

We need to handle OUT_DIR creation.

We should include helper functions: round half away from zero (not needed here), catx, etc.

We need to ensure script runs on Windows with Rscript.

We should add code to list files for debugging but not print full data.

We need to handle haven reading: if file is .sas7bdat, read_sas; if .xpt, read_xpt.

We should attempt to find adam directory: D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam

We need to handle case where files are named adae.sas7bdat etc.

We should write script that tries both.

We also need to handle TRTSDT format: if it's numeric SAS date (days since 1960-01-01), haven will convert to Date? Actually haven converts SAS dates to Date if format is present. So we can treat as Date.

We need to handle input(START_DATE, ??yymmdd10.) -> parse date, if invalid return missing.

We should implement parse_yymmdd10 function: try as.Date(substr(x,1,10), format="%Y-%m-%d") ; if NA then try other formats.

We need to handle TRT_START ne . -> !is.na(TRT_START)

We need to handle demog: catx('/', ...) -> paste with "/" ignoring empty? SAS catx with '/' will skip empty. But we have 3 parts, all likely non-empty.

We need to handle sae_criteria: catx(', ', ...) building incrementally.

We should ensure we preserve original column names case: TRTA, USUBJID, etc.

We need to produce final CSV with same column names as SAS report: TRTA, USUBJID, demog, SOC, PT, START_DATE, END_DATE, onset_day, GRADE, RELATIONSHIP, ACTION_TAKEN, OUTCOME, sae_criteria

We should also include maybe VERBATIM? But report columns don't include VERBATIM.

We need to ensure we output all rows.

We should also handle sorting.

We need to create text file with header and table.

We should use base R to write text.

We need to handle width: not critical.

We should produce a simple tab-delimited text representation.

We need to ensure we write 312 numbers: maybe they count numeric values in listing. Our text will contain them.

We should implement script now.

Potential pitfalls: reading files may fail if path uses forward slashes on Windows; R handles.

We need to handle environment variable OUT_DIR: if not set, use getwd().

We need to ensure we create out directory if not exists.

We need to handle haven package loading.

We should include library calls.

We need to handle data.table etc not needed.

We should write final script.

We need to ensure we don't open graphics.

We should include error handling.

We need to produce final answer: one R code block.

Let's craft script.
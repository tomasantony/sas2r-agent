We need to produce an R script that replicates the SAS program. Steps:

- Read environment variables: root, pid, rawadam, rawsdtm, rawsplit, rawdir, outtlf, outlog, outds. But we only need rawdir for reading Excel files (edc_vs) and sdtm_dm dataset (presumably from raw sdtm). However the environment preloads WORK datasets from Excel files: edc_vs etc. So we need to read those Excel files into data frames.

- The SAS program uses sdtm_dm dataset from SDTM. That dataset likely exists in raw sdtm folder. Path: &rawsdtm. Probably a SAS dataset .sas7bdat? Not specified. Could be .xpt? We'll assume .sas7bdat? But we can read using haven::read_sas. Path: file.path(rawsdtm, "dm.sas7bdat")? Not sure. The SAS code: proc sort data=sdtm_dm out=dm_for_vs(keep=USUBJID SUBJID RFSTDTC RFXSTDTC); So sdtm_dm dataset must be in WORK already? Not preloaded. Might be in raw sdtm folder. We'll attempt to read from .sas7bdat or .xpt. Use list.files to find file containing "dm". But for simplicity, we can read using haven::read_sas(file.path(rawsdtm, "dm.sas7bdat")). If not exist, maybe .xpt. We'll attempt both.

- Steps:

1. Read edc_vs from Excel: path given.

2. Create vs_prep: add USUBJID = paste0("ONCO-2026-", SUBJID). Ensure length char.

3. Sort vs_prep by USUBJID.

4. Read sdtm_dm, keep needed columns, sort by USUBJID.

5. Merge vs_prep with dm_for_vs by USUBJID (inner join, keep only vs rows). Equivalent to SAS merge with in=v and then if v; So left join vs_prep with dm_for_vs, keep rows where vs present.

6. Create sdtm_vs1 with many derived variables:

- STUDYID = "ONCO-2026"
- DOMAIN = "VS"
- VSTESTCD = TESTCD
- VSTEST mapping: if VSTESTCD == "SYSBP" then "Systolic Blood Pressure", else if "DIABP" then "Diastolic Blood Pressure", else keep as missing? SAS would leave missing.

- VSORRES = ORRES
- VSORRESU = UNIT
- VSSTRESC = ORRES
- VSSTRESN = numeric conversion of ORRES (input best.) So as.numeric(ORRES) with NA for non-numeric.

- VSSTRESU = UNIT

- VISIT = replace underscores with space: gsub("_", " ", VISIT)

- VSPOS = toupper(POS)

- VSDTC: if VSDT not empty then format date: VSDT is presumably a character date in SAS date format? In SAS they treat VSDT as character? They check VSDT ne "" then VSDTC = put(input(VSDT, date11.), yymmdd10.). So VSDT is a character string representing a date in DATE11. format (e.g., "01JAN2020"). We'll need to parse that. Use lubridate::dmy with format? date11. is ddmmmyyyy (e.g., 01JAN2020). So we can parse with dmy(VSDT). Then format as "%Y-%m-%d". If VSDT missing or empty, VSDTC remains missing.

- Study Day: if VSDTC and RFSTDTC not empty then compute difference in days: _stdt = input(VSDTC, yymmdd10.) (so date). _rfdt = input(RFSTDTC, yymmdd10.) (RFSTDTC from dm). Then if _stdt >= _rfdt then VSTDY = (_stdt - _rfdt) + 1 else VSTDY = (_stdt - _rfdt). So compute difference as integer days.

- Keep variables as per later step.

7. Sort sdtm_vs1 by USUBJID, VSDTC.

8. Derive VSSEQ: by USUBJID, retain VSSEQ, first USUBJID set to 1, else increment by 1. So within each USUBJID, sequence number.

9. Keep only specified variables.

10. Output listing: PROC PRINT of sdtm_vs. So produce a table listing all rows with all columns. Need to write a text rendering and CSV of same table. The listing includes 114 numbers (maybe counts). We'll output CSV: write.csv(sdtm_vs, file.path(out, "sdtm_vs.csv"), row.names=FALSE, na="").

For text rendering: we need to produce a plain-text representation similar to SAS PROC PRINT default output. Could be simple: write.table with appropriate formatting. We'll produce a text file with column headers and rows, using format similar to SAS: numbers as is, character as is. Use write.table(sdtm_vs, file, sep="\t", row.names=FALSE, na="").

But need to match SAS formatting: numeric values unrounded unless SAS rounds them. We'll keep as is.

Also need to output a CSV of the table body (already done). The text file should contain same numbers.

Thus script steps.

Implement round-half-up helper? Not needed as no rounding.

Implement reading of sdtm_dm: try .sas7bdat then .xpt.

Implement environment variable OUT_DIR: out <- Sys.getenv("OUT_DIR"); if empty, set to current dir.

Write outputs: CSV and TXT.

Also need to write CSV of the table body (already). The listing text file maybe same as CSV but with formatting. We'll produce a .txt using write.table with sep=" " maybe.

Implement.

Now produce final R script inside ```r``` block.
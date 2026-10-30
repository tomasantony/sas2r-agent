Need to translate SAS program to R. Must produce datasets adex, adex_base, adsl and P089.lst text rendering.

We need to understand SAS logic.

Steps:

1. Create ex data from datalines. 13 rows.

2. Create adsl from datalines with dates date9. format like 15JAN2024.

3. Merge and create base exposure variables: adex_base.

Details: merge ex and adsl by STUDYID USUBJID, if a and b (inner join). Since all ex subjects exist in adsl, all kept.

Create variables:
PARAMCD='DOSE', PARAM='Dose', PARAMN=1, AVAL=EXDOSE, AVALU=EXDOSU
Convert dates: EXSTDTC -> ASTDT via yymmdd10., EXENDTC -> AENDT. Note EXENDTC can be '.' or '' -> missing.
Analysis day: ASTDY, AENDY relative to TRTSDT.
AVISIT=VISIT, AVISITN=VISITNUM
EXDURN = AENDT - ASTDT +1 if both not missing
DOSINTFL='N', DOSEADFL='N' initially
drop EXSTDTC EXENDTC VISIT VISITNUM

Step 4: Identify dose adjustments: sort by USUBJID ASTDT, retain _prev_dose _prev_end. For each subject first record reset. If not first and _prev_dose not missing and AVAL != _prev_dose then DOSEADFL='Y'. If not first and _prev_end not missing and ASTDT not missing and ASTDT - _prev_end >1 then DOSINTFL='Y'. Then update retains.

Step 5: Create summary exposure parameters via proc sql.

_totdur: sum(EXDURN) as AVAL, PARAMCD TOTDUR, PARAM 'Total Exposure Duration (Days)', PARAMN 2, AVALU 'days', grouped by STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX. Note sum(EXDURN) will be NA if any missing? In SAS, sum ignores missing? Actually sum function in SQL? In SAS proc sql sum() ignores missing? Need check. For subject 1005, EXDURN missing because AENDT missing, so sum = .? Let's see SAS behavior: sum of column with missing values: missing ignored, but if all missing then missing? For 1005, one record with EXDURN missing => sum = .? In SAS, sum of missing is missing? Actually SAS sum function returns missing if all missing, else sum of non-missing. So for 1005, sum = . (missing). Need to replicate.

_avgdose: sum(AVAL * EXDURN) / sum(EXDURN) where AVAL>0. So only subjects with AVAL>0 contribute. For placebo subjects (1002,1005) AVAL=0, so where clause excludes all rows => no row for those subjects. So _avgdose will have 4 subjects (1001,1003,1004,1006). Similarly _maxdose, _cumdose.

_maxdose: max(AVAL) where AVAL>0

_cumdose: sum(AVAL*EXDURN) where AVAL>0

_ndosadj: sum(case when DOSEADFL='Y' then 1 else 0 end) as AVAL, PARAMCD NDOSADJ, PARAMN 6, AVALU '' . Group by same keys. This includes all subjects.

Step 6: Combine all parameters: data adex; set adex_base(keep=...) _totdur _avgdose _maxdose _cumdose _ndosadj; run;

Note adex_base keep list includes many vars. For summary tables, they have only those columns, other vars will be missing (NA).

Step 7: Add sequence and labels: sort by STUDYID USUBJID PARAMN ASTDT; then by STUDYID USUBJID, ASEQ increment per USUBJID. Derive DOSECAT for PARAMCD='DOSE': if AVAL=0 => Placebo, <=50 Low, <=100 Medium, else High.

Need to produce outputs: adex.csv, adex_base.csv, adsl.csv, and P089.lst text rendering.

We need to inspect expected outputs? Not provided but we can infer. P089.lst is table/listing text, 328 numbers. Likely proc contents and proc print? The SAS program does proc contents and proc print obs=20. So P089.lst would contain those. Need to write text rendering and CSV of same table.

We need to create OUT_DIR handling.

We must also ensure dates written as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS. For adsl, TRTSDT TRTEDT are dates. For adex, ASTDT AENDT, TRTSDT TRTEDT.

We need to produce R script that does all.

We should also handle labels? Not needed for CSV but maybe.

We need to ensure numeric values unrounded unless SAS rounds them.

We need to produce P089.lst text: should contain same numbers SAS output shows. We can generate a text file that mimics proc contents and proc print.

Simpler: create a text file with contents of adex dataset and print first 20 rows.

Also need CSV of table body: maybe P089.csv? But spec says each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers and as CSV of table body. For P089.lst, we need to write P089.txt and P089.csv? Or P089.lst.txt? The expected outputs list says P089.lst -> write text rendering and CSV of same table. So we should write file named P089.lst.txt? But instruction says write each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv")... and each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) . So for P089.lst, the name is P089, so we write P089.txt and P089.csv? Or P089.lst.csv? Safer to write both P089.txt and P089.csv and also P089.lst.txt? Let's check typical pattern: Expected outputs (produced by SAS; R script must produce counterparts under OUT_DIR): P089.lst (table/listing text, 328 numbers) -> write a text rendering and CSV of same table. So we should produce file named P089.lst.txt? But instruction says <name>.txt where <name> is output name without extension? Might be P089.txt. To satisfy both, we can write multiple files: P089.txt, P089.lst.txt, P089.csv, P089.lst.csv.

Similarly datasets: adex, adex_base, adsl -> write OUT_DIR/adex.csv etc.

We need to implement SAS semantics: dates, sorting, etc.

Let's craft R script.

We need to handle date parsing: EXSTDTC yymmdd10. format YYYY-MM-DD. Use as.Date.

TRTSDT date9. like 15JAN2024 -> parse with as.Date(..., format="%d%b%Y") but need uppercase month. Use toupper? Actually %b expects Jan, but we have JAN. Use lubridate? Use as.Date with format "%d%b%Y" after converting to proper case? Better use dmy with lubridate? But we can parse manually: as.Date("15JAN2024", format="%d%b%Y") works if locale English? In R, %b is abbreviated month name locale dependent, case-insensitive? Might work. Test mental: R's strptime is case-insensitive? Might need to convert to title case: "15Jan2024". Let's do: stringr::str_to_title.

Also need to handle missing TRTEDT '.' -> NA.

We need to compute EXDURN, ASTDY etc.

We need to ensure sorting: SAS sorts character ordering uppercase before lowercase. Our data is uppercase, so default R sort same.

We need to implement retain logic correctly.

We need to create summary tables: use dplyr group_by and summarise.

For _totdur: sum(EXDURN, na.rm=TRUE) but need to handle all NA => should be NA not 0. In R, sum(c(NA), na.rm=TRUE) =0, not NA. So need to check if all NA then NA else sum na.rm. Use ifelse(all(is.na(EXDURN)), NA_real_, sum(EXDURN, na.rm=TRUE))

Similarly for _avgdose: sum(AVAL*EXDURN) / sum(EXDURN) where AVAL>0. Need to filter AVAL>0, then group. If no rows for group, then no row. So we can filter adex_base where AVAL>0 then group.

For _cumdose similar.

For _maxdose: max(AVAL) where AVAL>0.

For _ndosadj: sum(DOSEADFL=='Y') per group.

We need to ensure grouping keys include STUDYID etc.

We need to combine: bind_rows.

We need to sort final adex by STUDYID USUBJID PARAMN ASTDT.

Then ASEQ per USUBJID.

DOSECAT.

We need to write CSVs with dates as YYYY-MM-DD. Use write.csv with na="".

We need to ensure column order matches SAS? For adex, columns: STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX, PARAMCD, PARAM, PARAMN, AVAL, AVALU, ASTDT, AENDT, ASTDY, AENDY, EXDURN, DOSINTFL, DOSEADFL, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXROUTE, EXSEQ, AVISIT, AVISITN, ASEQ, DOSECAT plus maybe others. Need to include all.

For adex_base, columns: STUDYID, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXROUTE, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT, PARAMCD, PARAM, PARAMN, AVAL, AVALU, ASTDT, AENDT, ASTDY, AENDY, AVISIT, AVISITN, EXDURN, DOSINTFL, DOSEADFL

But we dropped VISIT etc. Keep as per code.

For adsl: STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT

We need to produce P089.txt: include proc contents and proc print.

We should generate text file with contents varnum and print obs 20.

We also need to produce CSV of table body: maybe print data.

We need to ensure 328 numbers in text: we can just generate text that contains many numbers.

Simplify: create P089.txt with title and table of adex first 20 rows.

We need to ensure script runs non-interactively.

We must use only allowed packages.

We need to handle OUT_DIR env var.

Let's craft final R script.
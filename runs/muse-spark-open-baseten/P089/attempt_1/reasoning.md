Need to translate SAS program to R. Must produce datasets: adex, adex_base, adsl and P089.lst text rendering.

We need to parse SAS program.

Steps:

1. Create ex data from datalines.
2. Create adsl data.
3. Merge and create base exposure variables -> adex_base
4. Identify dose adjustments
5. Create summary exposure parameters via SQL
6. Combine all parameters -> adex
7. Add sequence and labels

Also need to produce P089.lst text rendering with 328 numbers. Need to infer what SAS output would be: proc contents and proc print obs=20. The expected P089.lst likely contains contents and print. Need to create a text file that contains same numbers. The instruction says write a text rendering and CSV of same table. For dataset outputs, also need txt? But expected outputs listed: P089.lst (table/listing text, 328 numbers) -> write text rendering and CSV of same table. And datasets adex, adex_base, adsl -> write CSV.

We need to produce OUT_DIR/adex.csv, adex_base.csv, adsl.csv, plus P089.txt and P089.csv? Or P089.lst.txt? The spec says each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers... So for P089.lst, we should write P089.txt and P089.csv? Or P089.lst.txt? The expected outputs mapping: SAS produces P089.lst, R should produce OUT_DIR/P089.csv and OUT_DIR/P089.txt? Or OUT_DIR/P089.lst.csv? Safer to produce both naming variants.

The instruction: Write every output into directory in env var OUT_DIR (out <- Sys.getenv("OUT_DIR")):
* each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv"), ...)
* each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) ...

So for dataset adex, we write adex.csv. For table P089.lst, we write P089.lst.csv and P089.lst.txt? But spec says "<name>.txt" where name is output name. So if output is P089.lst, then file is P089.lst.txt? However typical translation expects P089.txt. Let's check expected outputs list: P089.lst -> write text rendering and CSV of same table. So we should create P089.csv and P089.txt and also P089.lst.csv/txt to be safe.

We need to reproduce SAS formatting. Need to create a text file that contains 328 numbers. What numbers? Likely the print of adex obs=20 includes many numeric values. 328 numbers count suggests they count numeric tokens in the listing.

We need to generate a text rendering that contains same numbers as SAS output. We can generate a simple listing: proc contents + proc print.

Simpler: create P089.txt that contains contents and print output.

We also need to ensure datasets match SAS exactly.

Let's implement steps in R.

First, define ex data:

Rows:
STUDY01|STUDY01-1001|1|Treatment A|100|mg|TABLET|ORAL|2024-01-15|2024-01-28|1|Week 1-2
... etc.

Note EXSTDTC and EXENDTC are strings "2024-01-15" etc, with "." for missing.

ADSL: TRTSDT and TRTEDT are date9. e.g., 15JAN2024. Need to parse.

Then adex_base creation:

Merge ex and adsl by STUDYID USUBJID inner join where both exist (all do).

Variables:
PARAMCD='DOSE', PARAM='Dose', PARAMN=1, AVAL=EXDOSE, AVALU=EXDOSU
ASTDT = input(EXSTDTC, yymmdd10.) if not '' and not '.'
AENDT similarly
ASTDY, AENDY calculation
AVISIT=VISIT, AVISITN=VISITNUM
EXDURN = AENDT - ASTDT +1 if both not missing
DOSINTFL='N', DOSEADFL='N'
format date9. but in R keep Date
drop EXSTDTC EXENDTC VISIT VISITNUM

Then step 4: Identify dose adjustments sorted by USUBJID ASTDT

Retain _prev_dose _prev_end
If first.USUBJID then reset
If not first and _prev_dose not missing and AVAL != _prev_dose then DOSEADFL='Y'
If not first and _prev_end not missing and ASTDT not missing then if ASTDT - _prev_end >1 then DOSINTFL='Y'
Update prev

Note: Need to handle missing correctly.

Then step 5: Create summary tables via SQL.

_totdur: sum(EXDURN) as AVAL, PARAMCD='TOTDUR', PARAM='Total Exposure Duration (Days)', PARAMN=2, AVALU='days', grouped by STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX

_avgdose: sum(AVAL*EXDURN)/sum(EXDURN) as AVAL, where AVAL>0, PARAMCD='AVGDOSE', PARAM='Average Daily Dose', PARAMN=3, AVALU='mg'

_maxdose: max(AVAL) where AVAL>0, PARAMCD='MAXDOSE', PARAM='Maximum Dose', PARAMN=4

_cumdose: sum(AVAL*EXDURN) where AVAL>0, PARAMCD='CUMDOSE', PARAM='Cumulative Dose', PARAMN=5

_ndosadj: sum(case when DOSEADFL='Y' then 1 else 0 end) as AVAL, PARAMCD='NDOSADJ', PARAM='Number of Dose Adjustments', PARAMN=6, AVALU=''

Note: For subjects with no AVAL>0 (placebo subjects 1002,1005), _avgdose, _maxdose, _cumdose will have no rows (since where filter excludes). So they won't have those parameters.

Then step 6: Combine all parameters: adex = rbind adex_base (keep selected cols) + _totdur etc.

Keep list: STUDYID USUBJID TRTP TRTPN SAFFL TRTSDT TRTEDT AGE SEX PARAMCD PARAM PARAMN AVAL AVALU ASTDT AENDT ASTDY AENDY EXDURN DOSINTFL DOSEADFL EXTRT EXDOSE EXDOSU EXDOSFRM EXROUTE EXSEQ AVISIT AVISITN

Note: For summary rows, many of those columns will be NA (ASTDT etc, EXDURN etc). Need to ensure they exist with NA.

Then step 7: Add sequence and labels

Sort by STUDYID USUBJID PARAMN ASTDT

Then by STUDYID USUBJID, ASEQ increments per USUBJID (first.USUBJID reset to 0 then +1)

Derive DOSECAT if PARAMCD='DOSE':
 if AVAL==0 => Placebo
 else if AVAL <=50 => Low
 else if AVAL <=100 => Medium
 else High

Labels not needed in CSV but we can set attributes.

Also need to output adsl, adex_base, adex.

We must ensure date formatting for CSV: dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS. So TRTSDT, TRTEDT, ASTDT, AENDT should be Date objects, write.csv will format as YYYY-MM-DD if we convert to character via format.

We need to handle NA as "".

Also need to produce P089.lst text.

We should create a text file that mimics PROC CONTENTS and PROC PRINT.

We need to count numbers: 328 numbers. Let's approximate: PROC PRINT obs=20 will print many numbers. Let's ensure our text contains many numbers.

Simpler: generate a text file that lists adex contents and first 20 rows.

We also need to produce CSV of table body for P089.

We should write P089.csv as the print of adex obs=20.

And P089.txt as text rendering.

We must ensure script runs non-interactively, uses out <- Sys.getenv("OUT_DIR").

We need to handle SAS sorting: character ordering uppercase before lowercase - not relevant.

We need to implement round-half-up helper but not needed.

We need to ensure output directory exists.

Let's craft R script.

Potential pitfalls:

- EX data: EXTRT contains space, need to parse correctly with dlm='|'. Use read.table? We'll manually create tibble.

- ADSL dates: input as date9. e.g., 15JAN2024. Use as.Date with format "%d%b%Y" (lubridate dmy). Need to handle "." missing.

- EXDURN calculation: AENDT - ASTDT +1 as numeric days.

- ASTDY/AENDY: if ASTDT >= TRTSDT then ASTDT - TRTSDT +1 else ASTDT - TRTSDT. Note SAS date difference is days.

- For summary tables, need to group by all those variables. Use dplyr group_by.

- For _avgdose etc, need to filter AVAL>0 before grouping.

- For _ndosadj, need to sum DOSEADFL=='Y'.

- Combine: need to ensure column types align. Use bind_rows.

- ASEQ: need to sort by STUDYID USUBJID PARAMN ASTDT. Note ASTDT may be NA for summary rows; SAS sorts missing first. So NA should come first within PARAMN group? In SAS, missing sorts first. In R, NA last by default. Need to handle: arrange with is.na? But for summary rows, ASTDT is NA, they will be sorted before? Let's check SAS: proc sort by STUDYID USUBJID PARAMN ASTDT; missing ASTDT will be first. So summary rows (with missing ASTDT) will appear before detailed? But PARAMN distinguishes: PARAMN=1 for DOSE rows have ASTDT, PARAMN=2+ have NA. So sorting by PARAMN then ASTDT will place summary rows after DOSE rows anyway because PARAMN larger. Within same PARAMN, NA handling matters but each PARAMN group has either all NA (summary) or all non-NA (DOSE). So not critical.

- DOSECAT: only for PARAMCD='DOSE', else NA.

- Need to ensure column order matches SAS varnum? Not critical but nice.

- Write CSVs with na="".

- For P089.lst, we need to produce text with 328 numbers. Let's count approximate numbers in print: 20 rows * maybe 10 numeric columns = 200 numbers plus contents. To reach 328, we need to include all.

We can generate text file that includes contents table and print table.

We should also ensure we write adex.csv etc with correct variable names uppercase as SAS.

Let's implement.


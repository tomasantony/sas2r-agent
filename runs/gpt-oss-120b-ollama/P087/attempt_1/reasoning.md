We need to translate the SAS program to R script that reproduces outputs: dataset adsl, advs, advs_base, and listing P087.lst (table/listing text). The SAS program includes steps: create mock VS data via datalines, create mock ADSL data, merge to advs_base, derive baseline flag, baseline values, compute CHG, PCHG, ANL01FL, add maximum post-baseline derived record, add sequence ASEQ, labels (labels not needed for output). Also PROC CONTENTS and PROC PRINT produce listing; we need to produce a listing text file P087.lst containing the table/listing (maybe the PROC PRINT output). The expected outputs: P087.lst (table/listing text, 296 numbers) -> write a text rendering and CSV of the same table. So we need to generate a table similar to PROC PRINT of advs (first 20 obs) noobs. Also maybe PROC CONTENTS output? The listing likely includes both contents and print. But we can approximate: produce a table of advs (full dataset) as CSV and also a text rendering of the PROC PRINT (first 20 rows). The spec: each output dataset as CSV. For listing, also a plain-text rendering containing same numbers as SAS output (counts, percentages etc). Here only a simple print, not stats. So we can output the first 20 rows of advs as text file with formatted columns similar to SAS default. We'll need to format dates as YYYY-MM-DD, numeric as is.

Implement steps in R using dplyr.

Key: SAS date handling: ADT derived from VSDTC string (YYYY-MM-DD) using input with yymmdd10. So we parse as Date.

ADY calculation: if ADT >= TRTSDT then ADY = ADT - TRTSDT + 1 else ADY = ADT - TRTSDT. In R, difference in days.

Baseline flag: last pre-treatment record per USUBJID PARAMCD where ADT <= TRTSDT. Implementation: for each group, filter ADT <= TRTSDT, take max ADT, set ABLFL='Y' for that record, else ''.

In SAS they sort descending ADT and set first.PARAMCD where _pre_trt=1 then ABLFL='Y'. Equivalent.

Then baseline dataset _baseline_ with BASE = AVAL where ABLFL='Y'.

Merge baseline to compute CHG, PCHG.

ANL01FL: if ABLFL ne 'Y' and not missing(AVAL) and ADT > TRTSDT then 'Y' else ''.

Maximum post-baseline: compute max AVAL per USUBJID PARAMCD where ABLFL ne 'Y' and ADT > TRTSDT. Then merge with baseline to compute CHG. Add DTYPE='MAXIMUM', AVISIT='Maximum Post-Baseline', AVISITN=99.

Combine with advs.

Add ASEQ: sort by STUDYID USUBJID PARAMCD AVISITN ADT, then for each USUBJID, ASEQ starts at 0 then increment.

Also need to retain original variables: STUDYID, USUBJID, etc. In advs_base they dropped many original variables; final advs includes variables from advs_base plus derived ones. Need to ensure columns: STUDYID, USUBJID, ASEQ, PARAMCD, PARAM, PARAMN, PARCAT1, AVAL, AVALU, BASE, CHG, PCHG, ADT, ADY, AVISIT, AVISITN, ABLFL, ANL01FL, DTYPE, TRTP, TRTPN, TRTSDT, SAFFL. Also maybe ATPT, ATPTN but not needed.

When merging vs and adsl, we need to bring TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT. In advs_base they didn't keep those, but later they use TRTP etc in final dataset (labels). The SAS code after merging didn't drop them, so they remain. In advs_base they defined length for many variables but didn't drop them. They only dropped VSTESTCD etc. So final advs should have those adsl variables.

Thus when merging, keep all adsl variables.

Implementation steps:

1. Create vs tibble with given columns, parse VSDTC to Date.

2. Create adsl tibble, parse TRTSDT and TRTEDT using dmy format (e.g., 15JAN2024). Use lubridate::dmy.

3. Merge vs and adsl by STUDYID USUBJID (inner join). Keep all columns.

4. Derive PARAMCD = VSTESTCD, PARAM = VSTEST, PARCAT1='VITAL SIGNS'.

5. PARAMN mapping via case_when.

6. AVAL = VSSTRESN, AVALU = VSSTRESU.

7. ADT = as.Date(VSDTC). Already.

8. ADY compute.

9. AVISIT = VISIT, AVISITN = VISITNUM.

10. ATPT = '' (character), ATPTN = NA (numeric). Not needed.

11. Keep other adsl vars.

12. Baseline flag: group_by USUBJID, PARAMCD, arrange descending ADT, compute flag: if ADT <= TRTSDT then pre_trt=1 else 0. Then for each group, set ABLFL='Y' for first row where pre_trt==1, else ''.

Implementation: within group, create a flag pre_trt = ifelse(!is.na(ADT) & !is.na(TRTSDT) & ADT <= TRTSDT, 1, 0). Then find max ADT among pre_trt==1, set ABLFL='Y' for rows where ADT == that max and pre_trt==1, else ''.

Edge: if no pre_trt, then all ''.

13. Compute baseline dataset: filter ABLFL=='Y', select USUBJID, PARAMCD, BASE=AVAL.

14. Merge baseline back to advs_base (left join) to compute CHG, PCHG.

15. ANL01FL: if ABLFL!='Y' & !is.na(AVAL) & ADT > TRTSDT then 'Y' else ''.

16. Maximum post-baseline: filter where ABLFL!='Y' & ADT > TRTSDT, group_by USUBJID PARAMCD, summarise AVAL = max(AVAL, na.rm=TRUE). Then left_join baseline to get BASE, compute CHG = AVAL - BASE. Add DTYPE='MAXIMUM', AVISIT='Maximum Post-Baseline', AVISITN=99, other columns: need to bring other variables (STUDYID, USUBJID, PARAMCD, PARAM, PARAMN, PARCAT1, AVALU? In SAS they didn't set AVALU for derived record; they kept AVALU from merge? In SAS they merged _max_post with advs(where=(ABLFL='Y') keep=ABLFL USUBJID PARAMCD BASE). So they only kept ABLFL, USUBJID, PARAMCD, BASE from advs. So derived record will have AVALU missing, other variables from advs_base? They also have length DTYPE, AVISIT. So we need to create a record with columns: all columns of advs, but fill missing where not defined. We'll create a data frame with same columns as advs, fill with appropriate values: STUDYID from advs (maybe need to bring). Since they merged by USUBJID PARAMCD, the resulting dataset will have STUDYID from _max_post (which has it from advs? Actually _max_post derived from proc means on advs (which includes STUDYID). So _max_post will have STUDYID. Good.

Thus we need to compute max per group, keep all columns from advs (including STUDYID, USUBJID, PARAMCD, etc). In R, we can summarise with slice_max? But we need only AVAL, not other columns. We'll compute summary and then left_join baseline and also bring other columns from advs (maybe first row of each group). Simpler: after computing max AVAL per group, join with baseline and also join with distinct values of other columns (e.g., first row of advs for each group). Use advs_base %>% group_by... summarise.

Implementation: max_post <- advs_base %>% filter(ABLFL!='Y', ADT > TRTSDT) %>% group_by(STUDYID, USUBJID, PARAMCD) %>% summarise(AVAL = max(AVAL, na.rm=TRUE), .groups='drop').

Then join baseline (BASE) and also bring other columns: we can left_join with advs_base %>% select(STUDYID, USUBJID, PARAMCD, PARAM, PARAMN, PARCAT1, AVALU, TRTSDT, TRTP, TRTPN, SAFFL) %>% distinct().

Then compute CHG, DTYPE, AVISIT, AVISITN=99, set other fields: ADT, ADY, ABLFL maybe ''? In SAS they didn't set ADT etc, they will be missing. That's fine.

Combine with advs (original) using bind_rows.

17. Add ASEQ: arrange by STUDYID, USUBJID, PARAMCD, AVISITN, ADT, then group_by(STUDYID, USUBJID) and mutate(ASEQ = row_number() - 1) (since first gets 0). Actually they set ASEQ=0 at first USUBJID then increment. So row_number()-1 works.

18. Output datasets: adsl.csv, advs.csv, advs_base.csv.

19. Listing: produce a text file P087.lst containing PROC CONTENTS and PROC PRINT output. For simplicity, we can create a text file with PROC PRINT of first 20 rows of advs (noobs). Format similar to SAS: columns separated by spaces. We'll use write.table with sep=" ", row.names=FALSE, col.names=TRUE? SAS PROC PRINT no column headers? It prints variable names as header. We'll include header.

Also need CSV of same table: maybe same as first 20 rows? The spec: each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics), and as CSV of the table body. So for listing, we need both .txt and .csv. We'll create txt rendering of first 20 rows, and also csv of same rows (maybe name P087.csv?). But spec says "as CSV of the table body". So we can write CSV of same rows with same name but .csv. Use write.csv.

Thus produce P087.txt and P087.csv.

Implementation details: rounding half away from zero not needed.

Date formatting: write.csv will output dates as YYYY-MM-DD automatically.

Set out <- Sys.getenv("OUT_DIR").

Now produce R script.


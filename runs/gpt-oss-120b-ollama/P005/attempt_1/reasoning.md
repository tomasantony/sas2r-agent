We need to produce a single R script that reproduces outputs: dataset adlb as CSV, and listing P005.lst (the proc print of first 15 rows) as text and CSV.

We need to read raw data: adam.adsl, sdtm.lb. Paths: macro variables: &adam. points to D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam . So file likely .sas7bdat. Use haven::read_sas.

We need to replicate SAS steps.

Key points: SAS date conversion: input(substr(LBDTC,1,10), yymmdd10.) => parse ISO date string to SAS date (days since 1960). In R, we can use as.Date(substr(LBDTC,1,10), "%Y-%m-%d") which returns Date class. For output, we need to write as YYYY-MM-DD.

ASTDY calculation: if ASTDT >= TRTSDT then ASTDY = (ASTDT - TRTSDT) + 1 else ASTDY = ASTDT - TRTSDT. Note TRTSDT variable from ADSL. Need to ensure dates are Date class.

Baseline candidates: where ASTDT <= TRTSDT and not missing(AVAL). In R, filter.

Then base flags: for each USUBJID PARAMCD, keep last record (by sorted order). In SAS they sort by USUBJID PARAMCD ASTDT AVISITN SRCSEQ, then by last.PARAMCD (i.e., last observation per group). So we need to get the last row per group after sorting. That row's AVAL as BASE, ASTDT as BASE_DT, SRCSEQ as BASE_SEQ.

Then merge back with adlb draft.

ABLFL: if SRCSEQ == BASE_SEQ then 'Y' else '' (empty string). In SAS missing string is ''.

CHG: AVAL - BASE if both not missing.

LBNRIND: logic as described. Need to handle upcase(LBSTRESU). Use toupper.

Note: LBNRIND length $10, but we just produce string.

Drop temporary columns.

Finally sort by USUBJID PARAMCD ASTDT.

Write adlb.csv.

Also produce listing: first 15 rows of final adlb, with columns: USUBJID PARAMCD AVISIT ASTDT AVAL LBSTRESU BASE CHG ABLFL LBNRIND.

Write text rendering: need to format numbers similar to SAS default? SAS PROC PRINT default prints numeric with best12. For simplicity, we can produce a plain text table using format similar to R's print? The requirement: "contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics)". For this listing, just need to output the rows as they appear, maybe using write.table with sep="\t", row.names=FALSE, col.names=TRUE, na = "".

Also write CSV of same table.

Implement round-half-up? Not needed for this program (no rounding). Ensure missing values become empty strings in CSV.

Implement writing: out <- Sys.getenv("OUT_DIR") . Ensure directory exists? Assume exists.

Write adlb.csv: write.csv(adlb, file.path(out, "adlb.csv"), row.names=FALSE, na="").

Write listing text: write.table(listing_df, file.path(out, "P005.lst"), sep="\t", row.names=FALSE, col.names=TRUE, na="").

Also write CSV of listing: write.csv(listing_df, file.path(out, "P005.csv"), row.names=FALSE, na="").

But spec says "write a text rendering and CSV of the same table". Name of CSV maybe "P005.csv". We'll do that.

Now code.

Need to load libraries: haven, dplyr, lubridate, readr maybe.

Implement reading: adsl_path <- file.path("D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam","adsl.sas7bdat")? Not sure file name. In SAS library, dataset adsl. So file likely adsl.sas7bdat. Use list.files? We'll assume path: .../adam/adsl.sas7bdat . Similarly lb.sas7bdat.

We'll construct paths: file.path("D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam","adsl.sas7bdat") and similarly for sdtm.lb.

Read with haven::read_sas.

Now implement steps.

Edge Cases: variable names case-insensitive; we will use lower case column names after reading? Haven keeps original case. We'll use dplyr with rename.

Implement retain: order of columns not crucial but final output should have columns as in keep list.

In adlb final dataset, keep variables: STUDYID USUBJID TRT01A TRTSDT TRTEDT PARAMCD PARAM AVAL AVISIT AVISITN ASTDT ASTDY LBSTRESU SRCDOM SRCVAR SRCSEQ BASE CHG ABLFL LBNRIND.

But SAS keep list in final dataset: they retained STUDYID USUBJID PARAMCD PARAM; then merge and keep all variables from draft plus BASE, CHG, ABLFL, LBNRIND. The draft kept many variables. So final dataset includes those.

We'll construct adlb as per steps.

Implementation details:

adsl_sorted <- adsl %>% arrange(USUBJID)

lb_sorted <- lb %>% arrange(USUBJID, LBDTC, LBTESTCD)

adlb_draft: merge by USUBJID left join of adsl to lb (keep only lb records). In SAS merge with both sorted and if b; So keep only rows where lb present. So inner join of lb with adsl (adsl may have missing). Use left_join(lb_sorted, adsl_sorted, by="USUBJID") then filter(!is.na(LBSEQ))? Actually b indicates lb present, so after merge we keep rows where lb exists (which is all rows from lb). So we can just left_join lb with adsl.

Then create columns.

ASTDT: if nchar(LBDTC) >=10 then as.Date(substr(LBDTC,1,10), "%Y-%m-%d") else NA.

ASTDY: if not missing ASTDT and not missing TRTSDT then compute.

TRTSDT from adsl: variable name? In adsl dataset, likely TRTSDT (treatment start date). Ensure date type: may be numeric SAS date. Haven reads SAS dates as Date class automatically. So fine.

Now baseline candidates: filter(ASTDT <= TRTSDT & !is.na(AVAL))

Sort by USUBJID, PARAMCD, ASTDT, AVISITN, SRCSEQ.

Then base_flags: for each group USUBJID, PARAMCD, take last row (by order). Use dplyr::group_by and slice_tail(n=1). Keep columns USUBJID PARAMCD BASE=AVAL, BASE_DT=ASTDT, BASE_SEQ=SRCSEQ.

Now merge back: left_join adlb_draft with base_flags by USUBJID, PARAMCD.

Compute ABLFL: if SRCSEQ == BASE_SEQ then "Y" else "" (if BASE_SEQ is NA, then false). Use ifelse.

CHG: if both not NA then AVAL - BASE else NA.

LBNRIND: apply logic.

Implement using mutate with case_when.

For GLUC: if toupper(LBSTRESU) == "MG/DL" then _calc_val = AVAL * 0.0555 else _calc_val = AVAL. Then thresholds.

For HGB: similar.

Otherwise "UNKNOWN". For missing AVAL, LBNRIND missing? In SAS they only set if not missing AVAL; else LBNRIND remains missing (since length defined but not assigned). We'll set NA_character_.

Now final sort.

Write outputs.

Now produce script.


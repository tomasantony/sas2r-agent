We need to produce a single R script that reproduces the SAS program's outputs: a listing (P006.lst) and dataset adsl.csv. The script must read SDTM DM and EX datasets from appropriate paths. The macro variables give root paths. The libraries: sdtm points to &project_path./data/sdtm which resolves to D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm. So the files likely are .sas7bdat or .xpt. We need to assume they exist as .sas7bdat maybe. Use haven::read_sas. Path: file.path(project_path, "data", "sdtm", "dm.sas7bdat") etc. Or maybe .xpt. We'll attempt both: try .sas7bdat then .xpt.

Implement steps:

1. Read dm and ex.

2. Sort ex by USUBJID, EXSTDTC. Keep first per USUBJID.

3. Keep variables USUBJID EXTRT EXSTDTC EXENDTC.

4. Sort dm by USUBJID.

5. Merge dm (left join) with ex_first (right? but keep all dm). Use dplyr left_join.

6. Derive variables:

- TRT01P: if ARMCD != 'SCRNFAIL' then ARM else 'UNPLANNED'.

- TRT01A: if !is.na(EXTRT) then EXTRT else "NOT TREATED". Note SAS condition: b and not missing(EXTRT). b indicates presence in ex dataset. So if EXTRT missing or no exposure record, set "NOT TREATED". In left join, EXTRT will be NA if no match.

- ITTFL: if ARMCD != 'SCRNFAIL' then "Y" else "N".

- SAFFL: if !is.na(EXSTDTC) then "Y" else "N". Note missing date.

- Convert dates: BRTHDTC, RFICDTC, EXSTDTC, EXENDTC, RFPENDTC. Use substr first 10 characters, parse ymd.

- Age: if both dates present, compute floor of years difference. Use lubridate::time_length(interval(BRTHDT, RFICDT), "years") floor.

- TRTDURD: if both TRTSDT and TRTEDT present, (TRTEDT - TRTSDT) + 1. Since dates are Date objects, subtraction yields days.

- Keep variables: STUDYID USUBJID SUBJID SITEID TRT01P TRT01A ITTFL SAFFL BRTHDT RFICDT TRTSDT TRTEDT EOSDT AGE TRTDURD SEX RACE.

Note: need to retain variable order.

7. Sort by USUBJID and write to adam.adsl dataset: output as CSV to OUT_DIR/adsl.csv.

8. Listing: produce a table similar to PROC PRINT of first 10 observations, showing variables USUBJID TRT01P TRT01A ITTFL SAFFL AGE. Need to output as text rendering and CSV. The text rendering should match SAS output formatting: likely column aligned. We'll produce a simple plain text table using format similar to SAS: maybe use knitr::kable? Not allowed. Use base write.table with row.names=FALSE, col.names=TRUE, sep=" ". But need to match numbers formatting: AGE integer.

We'll create a data frame listing_df = head(adsl, 10) %>% select(USUBJID, TRT01P, TRT01A, ITTFL, SAFFL, AGE). Write to txt using write.table with quote=FALSE, sep=" ", row.names=FALSE, col.names=TRUE. Also write CSV of same table: write.csv(listing_df, file.path(out, "P006.csv")? Actually listing name is P006.lst. So txt file: P006.txt, csv: P006.csv? The spec: each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) and as CSV of the table body. The name should be same as listing: P006.lst. So txt file: P006.txt, csv: P006.csv (maybe). We'll follow: txt: P006.txt, csv: P006.csv.

Also need to write the full dataset adsl.csv.

Implement round-half-up helper? Not needed as no rounding.

Make sure missing values are written as empty strings in CSV: write.csv with na = "".

Set options for stringsAsFactors=FALSE.

Implement reading with haven::read_sas; if file not exist, try .xpt.

Define helper function read_dataset(name) that checks .sas7bdat then .xpt.

Implement.

Now produce final R script inside ```r``` block.
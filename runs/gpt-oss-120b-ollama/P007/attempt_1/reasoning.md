We need to produce R script that replicates SAS program. Steps:

- Read adam.adae, adam.adsl from ADaM library path: &project_path./data/adam . The libref adam points to D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam . So files likely .sas7bdat. Use haven::read_sas.

- Perform sort by USUBJID AESTDT, filter TRTEMFL='Y'.

- Keep first per USUBJID (by sorted order). Keep USUBJID AESTDT rename to EVENTDT.

- Sort adsl by USUBJID.

- Merge adsl and ae_target by USUBJID, keep only records where a (adsl) present and SAFFL='Y'.

- Add columns: STUDYID (from adsl), USUBJID, TRT01P, TRT01A, TRTSDT, TRTEDT, EOSDT (from adsl). Then set PARAMCD="TTFAEV", PARAM="Time to First Adverse Event".

- Derive CNSR and AVAL per logic.

- Keep specified variables.

- Output dataset adtte as CSV to OUT_DIR/adtte.csv.

- Also produce listing: first 15 observations of adtte with columns USUBJID TRT01A PARAM CNSR AVAL EVENTDT EOSDT. Need to write a text rendering and CSV of same table. The SAS PROC PRINT default formatting: numeric values as default (maybe no decimals). We'll replicate by formatting numbers as they appear: For dates, SAS prints as date values (e.g., 01JAN2020). But we can output as YYYY-MM-DD per spec for CSV. For text listing, we need to produce plain-text rendering similar to SAS output. Could just create a simple table with aligned columns using format. Provide same numbers. We'll produce a data frame of first 15 rows, then write to txt using write.table with row.names=FALSE, col.names=TRUE, sep=" ", quote=FALSE. Ensure formatting: For numeric CNSR (0/1) and AVAL (numeric maybe integer). We'll format AVAL with no decimal if integer else as is.

- Also need CSV of the table body: write.csv of same subset to OUT_DIR/adtte_listing.csv maybe name "adtte_listing.csv"? The spec says each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows, and as CSV of the table body. Name of listing? The SAS program's output is P007.lst. So we should write P007.txt and P007.csv for the listing. The dataset output is adtte.csv.

Thus:

out <- Sys.getenv("OUT_DIR")

Write adtte.csv.

Write listing: subset <- head(adtte,15); write.table(subset, file=file.path(out,"P007.txt"), row.names=FALSE, col.names=TRUE, sep=" ", quote=FALSE); write.csv(subset, file.path(out,"P007.csv"), row.names=FALSE, na="").

Need to ensure dates are formatted as YYYY-MM-DD in CSV. In adtte, dates columns are date class. When writing CSV, they will be in ISO format automatically? write.csv will use format "2020-01-01". Good.

Need to implement round-half-up? Not needed as no rounding.

Implement missing handling: In R, missing values are NA. For date arithmetic, subtract dates yields difftime in days. Need to add 1. So AVAL = as.numeric(EVENTDT - TRTSDT) + 1.

Similarly for EOSDT.

CNSR: if missing TRTSDT then NA (SAS sets .). So we set CNSR <- NA_real_.

Implementation details:

- Use dplyr for operations.

- Ensure sorting with character ordering: dplyr arrange will use locale ordering; but we can rely.

- Keep only needed columns.

- For merge, use left_join? SAS merge with by USUBJID and in= flags. We'll do inner join of adsl (where SAFFL='Y') with ae_target (may be missing). Use left_join(adsl_filtered, ae_target, by="USUBJID") then filter rows where SAFFL='Y' (already filtered). Actually SAS merge keeps all adsl rows (a) and adds ae_target if exists. Then condition if a and SAFFL='Y' (so keep only adsl rows with SAFFL='Y'). So we filter adsl where SAFFL='Y' then left_join.

- Keep variables.

- Ensure variable names uppercase.

- Write progress messages via message().

Now produce final R script.
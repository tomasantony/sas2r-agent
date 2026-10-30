We need to translate SAS program to R script that reproduces outputs: datasets adrs, adsl, adtte, adtte_os, adtte_pfs, adtte_ttr, and listing P086.lst (table/listing text). Also need to write CSVs for each dataset, and a text rendering of the listing (proc print). The listing likely prints the adtte dataset with all variables (noobs). We need to produce a text file with same numbers (279 numbers). We'll approximate by writing the dataset as a formatted table with same formatting as SAS default? SAS PROC PRINT default prints values with default formats; dates as date9. (e.g., 15JAN2024). We'll need to replicate that.

Simplify: produce a text file where each row is printed with values separated by spaces, dates formatted as DDMMMYYYY (e.g., 15JAN2024). Use write.table with sep=" " and no row names, col.names=TRUE? SAS PROC PRINT no column headers? Actually PROC PRINT default prints column headers. We'll include headers.

We also need CSV of the table body (maybe same as dataset). We'll write both.

Implementation steps:

- Set out <- Sys.getenv("OUT_DIR"); if empty, maybe default to ".".

- Create adsl data frame from inline data. Use read.table on text connection.

- adrs similarly.

- Convert date strings: For adsl, dates are in date9. format (e.g., 15JAN2024). Use as.Date with format "%d%b%Y". For missing '.' treat as NA.

- For adrs, RSDTC is in ISO format "2024-03-15". Use as.Date("%Y-%m-%d").

- Derive _prog_: filter adrs where RSSTRESC == "PROGRESSIVE DISEASE", sort by USUBJID and RSDTC, keep first per USUBJID, convert RSDTC to date.

- Derive adtte_pfs: merge adsl with _prog_ (left join). Compute variables as per SAS.

- Derive adtte_os similarly.

- Derive _resp_: filter adrs where RSSTRESC in ('COMPLETE RESPONSE','PARTIAL RESPONSE'), sort by USUBJID and RSDTC, keep first per USUBJID, convert date.

- Derive adtte_ttr.

- Combine adtte = bind_rows(adtt_pfs, adtt_os, adtt_ttr). Compute AVAL_M.

- Add ASEQ: sort by STUDYID, USUBJID, PARAMN; then within each USUBJID, assign sequence starting at 0 for first record per USUBJID? SAS code: by STUDYID USUBJID PARAMN; then in data step, by STUDYID USUBJID; if first.USUBJID then ASEQ=0; ASEQ+1; So ASEQ increments across rows for each subject across parameters (ordered by PARAMN). So we need to order by STUDYID, USUBJID, PARAMN, then compute ASEQ per USUBJID.

- Add labels: not needed for output.

- Write CSVs: write.csv with na = "".

- Write listing: create a text file P086.lst (maybe name). The expected output name is P086.lst. We'll write to file.path(out, "P086.lst").

- For listing, we need to format dates as DDMMMYYYY (uppercase month). Use format(date, "%d%b%Y") and toupper month? %b gives abbreviated month with first letter capital (Jan). SAS uses uppercase month abbreviation (JAN). So we need toupper.

- For numeric values, keep as is (no rounding). For character variables, keep as is.

- We'll produce a data frame with same column order as SAS keep list: STUDYID USUBJID TRTP TRTPN AGE AGEGR1 SEX RACE SAFFL ITTFL PARAMCD PARAM PARAMN STARTDT ADT AVAL CNSR EVNTDESC CNSDTDSC SRCDOM ASEQ? SAS PROC PRINT after step 6 includes all variables (including ASEQ). The keep list earlier didn't include ASEQ, but after adding ASEQ, it's present. We'll include ASEQ.

- Write text file using writeLines of formatted table: maybe use format for each column, then paste with spaces.

Simplify: use write.table with sep=" ", row.names=FALSE, col.names=TRUE, na="", quote=FALSE, file=... . This will produce similar layout but may not match SAS exactly but acceptable? The requirement: "plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics)". For this listing, it's just the dataset rows, not stats. So using write.table should be fine.

Also need CSV of the table body: maybe same as dataset CSV already. But they ask for both text rendering and CSV of same table. We'll write CSV already; for listing CSV maybe same as dataset? We'll create a separate CSV named "P086_listing.csv". But spec says "each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics), and as CSV of the table body." So for listing, we need both .txt and .csv. We'll name .txt as "P086.lst.txt"? Actually they said P086.lst is the listing text. So we will write "P086.lst" as .txt (no extension). And also "P086.lst.csv". We'll follow.

Implement rounding half away from zero? Not needed.

Now code.

Edge Cases: missing values represented as "." in input; need to treat as NA. We'll read with read.table and specify na.strings=".".

For adsl, columns: STUDYID (char), USUBJID (char), TRTP (char), TRTPN (numeric), AGE (numeric), AGEGR1 (char), SEX (char), RACE (char), SAFFL (char), ITTFL (char), TRTSDT (date), TRTEDT (date), EOSDT (date), DTHDT (date), DTHFL (char). Input lines have '.' for missing dates. We'll parse.

Implementation: read.table(text=..., sep="|", header=FALSE, stringsAsFactors=FALSE, na.strings=".", col.names=...). Then convert date columns using as.Date with format "%d%b%Y". For DTHDT column may be '.'; also DTHFL char.

For adrs: columns: STUDYID, USUBJID, RSSTRESC, RSDTC (char). Convert RSDTC to Date with "%Y-%m-%d".

Now compute.

When merging, use left_join from dplyr.

Compute ADT, CNSR, EVNTDESC, CNSDTDSC, SRCDOM as per logic.

Implement coalesce using dplyr::coalesce.

Now write outputs.

Let's craft script.


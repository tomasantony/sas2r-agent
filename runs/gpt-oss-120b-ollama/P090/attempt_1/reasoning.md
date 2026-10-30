We need to produce R script that replicates SAS program. Steps:

- Create mock CM and ADSL datasets from inline data. Use readr::read_delim with pipe delimiter.

- Convert date strings: CMSTDTC, CMENDTC are character strings like "2023-06-15" or ".". In SAS they treat '.' as missing. So we need to treat '.' as missing.

- For ADSL, dates are in date9 format like "15JAN2024". Use lubridate::dmy.

- Merge by STUDYID USUBJID.

- Derive variables as per SAS code.

- Sorting: SAS sorts with character ordering, case-sensitive. We'll use arrange with dplyr.

- Sequence ASEQ: reset per USUBJID.

- Labels: not needed for output.

- Output datasets adcm.csv and adsl.csv (adsl already created). Also produce listing P090.lst: It seems SAS prints PROC CONTENTS and PROC PRINT (first 15 obs). The listing likely includes those outputs. But we need to produce a text rendering and CSV of the same table. The expected outputs: P090.lst (table/listing text, 292 numbers) -> write a text rendering and CSV of the same table. Likely they expect the PROC PRINT output (first 15 rows) as the listing. Also maybe PROC CONTENTS. But we can combine both? Simpler: produce a text file containing the printed adcm first 15 rows (no obs numbers). Also produce CSV of same data.

We'll create adcm dataset, then write CSV.

For listing: We'll create a text representation similar to SAS PROC PRINT output: columns with variable names as header and values. We'll format dates as DATE9 (e.g., 15JAN2024). We'll need to format numeric as is.

Simplify: Use write.table with sep = "\t", row.names = FALSE, quote = FALSE to produce a text file. That will be acceptable.

Also produce CSV of same table: write.csv.

Also need to output adsl.csv.

Implement round-half-up? Not needed as no rounding.

Implement coalesce for _trt_end: if TRTENDT missing then use Sys.Date()? In SAS they used today() function which returns current date. For reproducibility, we could use Sys.Date(). We'll do that.

Implement missing detection: '.' or empty string.

Implement uppercase conversion.

Implement ATC mapping via case_when.

Implement ASEQ: group_by USUBJID, mutate(ASEQ = row_number() - 1?) Actually SAS sets ASEQ=0 at first record then increments for each record. So first record gets 0, second 1, etc. So ASEQ = row_number() - 1.

Sorting before ASEQ: sort by STUDYID USUBJID ASTDT CMSEQ.

Implement date conversion: ASTDT = as.Date(CMSTDTC) if not missing.

CMSTDTC may be '.'; treat as NA.

Similarly AENDT.

Compute ASTDY, AENDY.

Duration CMDURN = AENDT - ASTDT + 1.

PREFL: Y if ASTDT < TRTSDT else N.

ONTRTFL: compute _trt_end = ifelse(is.na(TRTEDT), Sys.Date(), TRTENDT). Then logic.

Concomfl: Y if ONTRTFL == 'Y' or PREFL == 'Y' else N.

ADTEFFL: Y if CMONGO == 'Y' or is.na(AENDT) else N.

ADTEFN: 1/0.

Uppercase fields.

ATC mapping: use case_when on CMCLAS_UP.

Write outputs.

Also need to write listing text and CSV for the table. We'll create listing_df = head(adcm, 15). Write to txt and csv.

Implement environment variable OUT_DIR.

Now produce final R script.
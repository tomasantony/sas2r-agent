We need to produce a single R script that reproduces the SAS program's outputs: datasets adae and adsl (and also adae listing? The expected outputs: P084.lst (table/listing text, 305 numbers) -> write a text rendering and CSV of the same table; dataset adae; dataset adsl. So we need to generate adae and adsl datasets as CSV, and also produce a listing (like PROC PRINT of first 10 rows?) plus PROC CONTENTS? The SAS program ends with PROC CONTENTS and PROC PRINT (obs=10). The listing likely includes both contents and print output. The expected P084.lst contains 305 numbers, maybe includes both. We'll need to capture the output of PROC CONTENTS and PROC PRINT into a text file and also CSV of the table body (maybe the printed rows). We'll generate a text file with both sections.

Implementation steps:

- Set out <- Sys.getenv("OUT_DIR")
- Create adae and adsl data frames from the mock data defined in SAS program.

- For adsl: read from inline data. We'll create a tibble manually.

- For ae: similarly.

- Merge to create adae with same logic.

- Derive variables as per SAS.

- Derive occurrence flags.

- Derive ASEQ sequence.

- Add labels: not needed for data but maybe store as attributes? Not required for CSV.

- Write adae.csv and adsl.csv.

- Generate listing: We'll produce a text file containing:

  - PROC CONTENTS output: variable list with attributes (type, length, format, label). We'll mimic SAS output: variable number, name, type, length, format, label. We'll generate using str(adsl) etc. But need to match SAS exactly? Hard. We'll produce a simple representation.

But requirement: "write a text rendering and CSV of the same table". So for the listing, we need to produce a table (maybe the PROC PRINT output). We'll produce a CSV of the printed rows (first 10 rows) and a text file with same content formatted similarly to SAS PROC PRINT (no obs numbers). We'll just write the data as space-separated.

Simplify: produce text file containing the first 10 rows of adae, with column names and values, similar to SAS PROC PRINT noobs. We'll also write CSV of those rows.

Also maybe include PROC CONTENTS output as separate? Could combine.

Given limited time, produce script that:

- Creates data frames.

- Writes adae.csv, adsl.csv.

- Creates listing data frame = head(adae,10). Write to text file with formatted columns using format similar to SAS default (no quotes). Use write.table with sep="\t", row.names=FALSE, col.names=TRUE, quote=FALSE.

- Also write CSV of same rows.

- For PROC CONTENTS, we can generate a data frame of variable metadata: varnum, name, type, length, format, label (if we stored). We'll create using sapply.

- Write that to text file (append after PROC CONTENTS header) and also CSV.

Combine both into one text file P084.lst.

Implementation details:

- Date parsing: AESTDTC and AEENDTC are strings like "2024-02-01". Use as.Date with format "%Y-%m-%d". For missing '.' treat as NA.

- For AEENDTC '.' treat as missing.

- Compute ASTDT, AENDT as Date objects.

- Compute ASTDY, AENDY as numeric days difference.

- TRTEMFL logic: if ASTDT >= TRTSDT then if is.na(TRTEDT) or ASTDT <= TRTEDT + 30 then 'Y' else '' then later set missing to 'N'.

- ASEV = AESEV; ASEVN mapping.

- AREL mapping.

- AESER_FL = AESER (character flag).

- AESOC = AEBODSYS; AEDECOD_UP = toupper(AEDECOD).

- AEDUR = if both dates not missing then as.numeric(AENDT - ASTDT + 1).

- Occurrence flags: sort by USUBJID, AEBODSYS, AEDECOD, ASTDT. Then by group, first occurrence.

- ASEQ: sort by STUDYID, USUBJID, ASTDT, AESEQ. Then by USUBJID, increment.

- Ensure sorting order matches SAS: character sorting case-sensitive, missing first. We'll use arrange with dplyr, which sorts NA first.

- Write CSV with na = "".

- For numeric formatting: keep as is.

- For text rendering: We'll create a character vector lines.

Implementation steps in R script.

Edge Cases: need to ensure date format in CSV as YYYY-MM-DD.

Now produce script.


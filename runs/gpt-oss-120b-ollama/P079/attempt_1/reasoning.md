We need to produce R script replicating SAS program. Steps:

- Read adam.adae and adam.adsl from ADaM library path: D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam . Files are likely .sas7bdat. Use haven::read_sas.

- Filter where SAFFL='Y' and AESER='Y'.

- Join on USUBJID.

- Select variables: USUBJID, TRTA, AGE, SEX, RACE, TRTSDT (as date), AEBODSYS (SOC), AEDECOD (PT), AETERM (VERBATIM), AESTDTC (START_DATE), AEENDTC (END_DATE), AETOXGR (GRADE), AEREL (RELATIONSHIP), AEACN (ACTION_TAKEN), AEOUT (OUTCOME), AESDTH, AESLIFE, AESHOSP, AESDISAB, AESCONG, AESMIE.

- Order by TRTA, USUBJID, AESTDTC.

- Derive sae_criteria string: concatenate labels for each Y flag.

- Compute onset_day: if TRT_START not missing and START_DATE parsed as yymmdd10. (SAS date string). In SAS, TRT_START is a SAS date numeric (days since 1960). In R, after reading, TRTSDT will be a Date? Haven reads SAS dates as Date class (days since 1960). So TRT_START is Date. START_DATE is character string in ISO? In ADaM, AESTDTC is character in ISO 8601 (YYYY-MM-DD). Use ymd from lubridate? But SAS uses yymmdd10. format (YYYY-MM-DD). So parse with ymd.

- Compute _onset = as.numeric(START_DATE - TRT_START) + 1. Then onset_day as character.

- Demog string: AGE (numeric) / SEX / first character of RACE. RACE may be character; need substr(RACE,1,1). Use paste0.

- Output listing: need to produce a text rendering and CSV of the table. The SAS PROC REPORT with defined column widths and headers. We need to produce a plain-text rendering similar to SAS output. That's complex. But requirement: "each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics), and as CSV of the table body." For this listing, it's just rows of data, no stats. So we can output the table body as CSV and also a simple text file with same content (maybe tab-separated). We'll produce a text file with column headers and rows separated by tabs.

- Also need to write the dataset (maybe final report) as CSV: name l_sae.csv.

- Also need to write the text rendering: l_sae.txt.

- No RTF generation; we just output CSV and txt.

- Use OUT_DIR env var.

Implement round-half-up helper not needed.

Implement ordering: character ordering uppercase before lowercase; default R order is lexical based on locale, but we can use order with base::order and method = "radix". For case-sensitive, we can use order with method = "radix". Ensure missing values first: order with na.last = FALSE.

Implement ordering by TRTA (character), USUBJID, AESTDTC (date). For AESTDTC we need to parse to date for ordering but keep original string.

We'll create a data frame sae_report.

Write CSV: write.csv(sa e_report, file.path(out, "l_sae.csv"), row.names=FALSE, na="").

Write txt: use write.table with sep="\t", quote=FALSE, row.names=FALSE.

Also need to output the intermediate dataset? Not required.

Implement cleaning.

Let's craft script.


We need to produce an R script that replicates the SAS program. Steps:

- Read raw_vitals.csv from path: &project_path./data/raw/raw_vitals.csv. project_path is D:/meta_hackathon/sas2r-agent/data/derived/c2c (from macro). So full path: D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_vitals.csv

- Create dataset sv with columns: STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, VISITDY, SVSTDTC, SVENDTC.

- Length definitions not needed.

- Retain: not needed.

- For each row: STUDYID = "CDISC-01"; DOMAIN = "SV"; USUBJID = paste(STUDYID, PT_ID, sep="-") (catx with hyphen). VISITNUM = VISIT_NUM; VISIT = trimws(VISIT). Then VISITDY based on VISITNUM: 1->1, 2->31, 3->61, else missing (NA).

- For date: VS_DATE column presumably contains either character date string like "15-JAN-2023" or numeric SAS date? In SAS they use input(strip(vvalue(VS_DATE)), anydtdte32.) to parse. In R we can use lubridate::parse_date_time with orders. anydtdte32 can parse many formats. We'll attempt to parse using lubridate::ymd, dmy, etc. But we can use readr::parse_date_time with multiple orders: "dmy", "mdy", "ymd", "d-b-Y", etc. However SAS date numeric values are days since 1960. If VS_DATE is numeric SAS date, we need to convert: as.Date(VS_DATE, origin = "1960-01-01"). So we need to detect if VS_DATE is numeric. In CSV, numeric may be read as numeric. We'll handle both.

Implementation: create column _svdt: if VS_DATE is character, parse with parse_date_time(...). If numeric, convert as.Date(VS_DATE, origin="1960-01-01").

Then if not missing, SVSTDTC = format(_svdt, "%Y-%m-%d")? SAS uses is8601da. which yields YYYY-MM-DD. So use format(_svdt, "%Y-%m-%d"). SVENDTC = same.

- Keep only required columns.

- Sort by USUBJID, VISITNUM (ascending). In SAS missing values sort first; in R, NA sorts last by default. To mimic SAS, we can arrange with dplyr::arrange with USUBJID, VISITNUM, but ensure NA first: we can use dplyr::arrange(USUBJID, dplyr::desc(is.na(VISITNUM)), VISITNUM) but SAS missing sorts first, so we need NA first. In dplyr, arrange puts NA last. To get NA first, we can use `arrange(USUBJID, factor(is.na(VISITNUM), levels=c(TRUE,FALSE)), VISITNUM)`. Simpler: use `arrange(USUBJID, VISITNUM, .by_group = FALSE)` and then after arrange, we can use `order(is.na(VISITNUM), VISITNUM)`? Actually order with na.last = FALSE will put NA first. So we can use base `df[order(df$USUBJID, df$VISITNUM, na.last = FALSE), ]`. That will sort by USUBJID (character default case-sensitive, uppercase before lowercase). Good.

- Write dataset sv.csv to OUT_DIR.

- Also produce listing: PROC PRINT of first 12 observations with title "SV Domain Audit". Need to output a text rendering and CSV of same table. So we need to create a data frame of first 12 rows (after sort). Then produce a text file that mimics SAS PROC PRINT output: includes column headers and values, maybe with formatting. But we can produce a simple plain text table using `write.table(..., sep = "\t", row.names = FALSE, quote = FALSE)`. However need to match numbers formatting: there are no numeric stats, just values. We'll output as plain text with same column order.

Also need CSV of the table body: write.csv of same 12 rows.

Naming: listing name is P017.lst (as per expected). So text file: P017.txt? The spec says each listing as plain-text rendering (file.path(out, "<name>.txt")). The SAS listing is P017.lst. So we should output P017.txt and also CSV maybe named P017.csv? The spec: "each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics), and as CSV of the table body." So we need both .txt and .csv for the listing. Name should be P017 (without extension). So produce P017.txt and P017.csv.

Dataset sv: write sv.csv.

Also need to write CSV of plotted data? No figures.

Implement round-half-up helper? Not needed.

Implement reading CSV with readr::read_csv.

Set options for stringsAsFactors FALSE.

Write outputs.

Make sure to get OUT_DIR from env var.

Now produce final R script.
We need to produce R script that reproduces outputs: a listing (P015.lst) which is the PROC PRINT of first 12 rows with specific variables, and dataset lb saved as CSV.

We need to follow SAS semantics: import raw_lab.csv, map variables, create lb_mapped, then sort, then generate LBSEQ per USUBJID, retain order.

We need to handle date conversion: if LAB_DAT is character, parse with anydate (anydtdte). SAS format is is8601da. So output date as YYYY-MM-DD.

Need to replicate length definitions not needed.

Need to ensure case-sensitive ordering: SAS sorts with missing first, then character ordering uppercase before lowercase. In R, default sort is lexical with ASCII, which matches? Uppercase letters have lower ASCII codes than lowercase, so uppercase before lowercase. Missing values (NA) sort first if we use order with na.last = FALSE. We'll use data.table setorder.

We need to write outputs to OUT_DIR environment variable.

We need to write listing as text rendering and CSV of same table. The listing is a table of 12 rows with columns: USUBJID VISITNUM VISIT LBTESTCD LBCAT LBSTRESN LBSTRESU LBDTC.

We need to format numbers same as SAS default: PROC PRINT prints numeric with default format BEST12. For LBSTRESN numeric, we can output as is (maybe no rounding). We'll output as character representation using format with no scientific? Use format with trimws.

We'll create a data frame listing_df = head(lb,12) selecting columns.

Write text file: we can use write.table with row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE. But need to match SAS output formatting: numbers maybe aligned. However requirement: "plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics)". For this listing, just need same numbers. So simple tab-separated is okay.

Also need CSV of table body: write.csv(listing_df, file.path(out, "P015.csv")? Actually listing name is P015.lst, but also need CSV of same table. So maybe write P015.csv. We'll follow spec: each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) and as CSV of the table body. So name base "P015". So write P015.txt and P015.csv.

Dataset lb: write as lb.csv.

Also need to write CSV of plotted data? No figures.

Implement round-half-up helper? Not needed.

Implement reading raw_lab.csv using readr::read_csv.

Implement date parsing: if LAB_DAT column is character, use lubridate::ymd? anydtdte can parse many formats; we can use lubridate::parse_date_time with orders = c("Ymd","Y-m-d","Y/m/d","mdY","dmy","Ymd HMS")? Simpler: use anytime? Not available. We'll use lubridate::ymd if format is ISO, else use parse_date_time with guess.

We'll just use as.Date(LAB_DAT, format="%Y-%m-%d") if fails then try other.

Simplify: use lubridate::ymd(LAB_DAT) which returns NA if not parse.

If LAB_DAT is numeric (SAS date days since 1960?), SAS date numeric is days since 1960-01-01. In R, as.Date numeric is days since 1970-01-01. So need to convert: if numeric, treat as SAS date: as.Date(LAB_DAT, origin="1960-01-01").

Implement: if is.character -> parse with ymd; else if is.numeric -> as.Date(LAB_DAT, origin="1960-01-01").

Then format as ISO8601: format(date, "%Y-%m-%d").

Implement mapping of test names: upcase strip.

Implement unit mapping.

Implement retain LBSEQ: we can group_by USUBJID arrange by LBDTC then LBTESTCD (as SAS sort). Then mutate LBSEQ = row_number().

Need to sort by USUBJID, LBDTC, LBTESTCD before generating LBSEQ.

Implement using dplyr.

Write outputs.

Now produce final R script.
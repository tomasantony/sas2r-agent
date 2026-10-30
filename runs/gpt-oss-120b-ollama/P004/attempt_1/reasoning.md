We need to produce a single R script that reproduces the SAS program's outputs: a listing (P004.lst) and dataset adae.csv. The script must read input datasets: adam.adsl, sdtm.ae. Paths: macro variables indicate root directories. The SAS librefs map to directories: adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam . So the dataset files likely are .sas7bdat or .xpt? Not specified. We'll assume .sas7bdat. Use haven::read_sas.

We need to replicate steps:

1. Sort adsl by USUBJID, ae by USUBJID AESEQ.

2. Merge (left join) adsl (master) with ae (many rows) by USUBJID, keep only rows where ae exists (i.e., inner join). In SAS merge with in= variables and if b; means keep only rows where ae present. So inner join.

3. Derive numeric dates: AESTDTC, AEENDTC are character ISO 8601 strings. If length >=10, take first 10 chars, parse yymmdd10. In R, use as.Date(substr(...,1,10), format="%Y-%m-%d").

4. Format dates as date9. in SAS output but for CSV we output as YYYY-MM-DD.

5. Compute ASTDY: if both dates not missing, if AESTDT >= TRTSDT then (AESTDT - TRTSDT) + 1 else difference.

Date arithmetic in days.

6. TRTEMFL: if SAFFL == "Y" and dates not missing and AESTDT >= TRTSDT then "Y" else "".

7. Keep essential variables.

8. Derive occurrence order: add _occ_order = 0 if TRTEMFL=="Y" else 1.

9. Sort by USUBJID AEDECOD _occ_order AESTDT AESEQ.

10. Flag first treatment-emergent per subject/AEDECOD: by USUBJID AEDECOD _occ_order, if TRTEMFL=="Y" and first._occ_order then AOCCFL="Y" else "".

Note: first._occ_order refers to first record within each BY group where _occ_order changes? In SAS, BY USUBJID AEDECOD _occ_order; first._occ_order is true for first record of each new value of _occ_order within each USUBJID AEDECOD group. Since _occ_order is 0 for TE, 1 for non-TE, the first TE record will have first._occ_order true. So we need to implement that.

Implementation: arrange data as sorted, then group_by(USUBJID, AEDECOD, _occ_order) and mutate(first_occ = row_number()==1). Then AOCCFL = ifelse(TRTEMFL=="Y" & first_occ, "Y", ""). But need only for TE rows; others get "".

10. Drop _occ_order.

11. Final sort by USUBJID AESTDT AESEQ and write to adam.adae dataset (CSV). Also need to produce listing: print first 15 rows of final dataset with selected variables: USUBJID TRT01A AETERM AESEV AESTDT TRTSDT ASTDY TRTEMFL AOCCFL. Need to output as text rendering and CSV of same table.

The listing should have numbers formatted similar to SAS default. We'll produce a data frame of first 15 rows, then write a text file with formatted columns. Use format: dates as YYYY-MM-DD? SAS date9. prints like 01JAN2020. But requirement: "same numbers the SAS output shows (counts, percentages with the same decimals, statistics)". For this listing, we need to mimic SAS PROC PRINT default formatting: dates printed as date9., numeric as default (no decimals). We'll format dates as ddMONyyyy (e.g., 01JAN2020). Use format "%d%b%Y" with toupper month abbreviation.

We'll create a function format_date9.

For other variables: TRTEMFL and AOCCFL are character.

We'll write text file with column headers and rows aligned maybe using format with sprintf.

Simplify: write a plain text table using write.table with sep="\t", row.names=FALSE, quote=FALSE. That will produce tab-separated values, which is acceptable as "plain-text rendering". Provide also CSV of same table.

Also need to write the full adae dataset as CSV.

All outputs go to OUT_DIR = Sys.getenv("OUT_DIR").

File names: adae.csv, P004.lst (listing). Also CSV of listing: maybe P004.csv? The spec: "each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics), and as CSV of the table body." So for listing name P004.lst, we need also P004.csv? The name of listing is P004.lst. So we will write P004.txt and P004.csv (the table body). The listing file name is P004.lst but they said .txt for rendering. We'll follow spec: write P004.txt (text) and P004.csv (CSV). Also dataset adae.csv.

Implement round-half-up helper? Not needed.

Implement reading: adsl_path = file.path(Sys.getenv("project_path"), "data", "adam", "adsl.sas7bdat")? Actually macro rawadam etc. But we have libname adam pointing to &project_path./data/adam. So path = file.path(Sys.getenv("project_path"), "data", "adam", "adsl.sas7bdat") . Similarly ae.sas7bdat.

We'll use haven::read_sas.

Now produce script.

Edge Cases: variable names case-insensitive; keep as is.

Implement.


We need to translate SAS program to R script. Must produce outputs: a listing table (P035.lst) which is the PROC REPORT output of adpp_sum_t. Also need to output dataset(s) maybe? The declared output types: table;dataset. So we need to output the final table as text and CSV, and also output the dataset adpp_sum_t as CSV? The spec: "each output dataset as CSV". The SAS program writes out CLXCHECKNOBS from PROC REPORT out=CLXCHECKNOBS; That's a dataset of the report data (maybe same as adpp_sum_t). So we need to write that dataset as CSV. Also produce the listing text rendering of the table (the report). Need to mimic formatting: PROC REPORT with split "#", nowindows, headline, formchar(2)="_", headskip, spacing=1, missing. The table includes by variables ANLPRNT, PPSPECL, PPCATL, then columns: TRTAN, PPSDY, s_order, _PARAM, value. The define statements set widths, formats, etc. The report includes break after PPSDY (skip). Also compute before page to print a line with "Compound: <ANLPRNT>, Matrix: <PPSPECL>, Analyte: <PPCATL>" then blank line.

We need to generate a text rendering similar to SAS PROC REPORT output. That's complex. But we can approximate: produce a plain text file with the table rows, including the header lines and the break lines. The spec: "write a text rendering and CSV of the same table". So we need to produce a .txt file containing the same numbers as SAS output (132 numbers). We need to replicate formatting: numbers with certain decimals, missing as blank, etc. The values already formatted in adpp_sum_t as character variable 'value' with appropriate formatting (strings). So we can just output the table rows as they appear, with appropriate column widths.

Simplify: Use write.table with format? But need to match SAS formatting: column widths defined but not required for content; just need numbers same. So we can output a text file where each row is concatenated with spaces aligning? Could just output CSV and also a plain text using knitr::kable? Not allowed packages. Use base write.table with sep=" " and col.names=TRUE? That may not match exactly but likely acceptable.

Given constraints, produce script that:

- Set out <- Sys.getenv("OUT_DIR")
- Read source XPT: haven::read_xpt(file.path(Sys.getenv("rawadam"), "adpp.xpt"))? Actually filename source is &rawadam./adpp.xpt; libname source xport; So dataset source.adpp. So read using read_xpt.

- Filter rows: PKFN==1, ANL01FN != 1, paramcd in list, PPSTRESN not missing, compress(PPCATL) == 'ANAL2'. compress in SAS removes spaces; in R we can use gsub("\\s+", "", PPCATL) == "ANAL2". Also need to compute _PARAM: compbl(put(PPTESTCD,$pk.)||' ('||strip(PPORRESU)||')'); compbl compresses multiple blanks to single. So we need to map PPTESTCD via format $pk. We'll create a named vector mapping. Then combine with PPORRESU stripped (trim). Then collapse multiple spaces.

- Compute AVAL_log = log(AVAL).

- Sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM.

- PROC UNIVARIATE with BY groups: compute n, mean, std, cv, median, min, max for both AVAL and AVAL_log. Output dataset adpp_sum with variables: n1 n2 mn1 mn2 std1 std2 cv1 cv2 med1 med2 min1 min2 max1 max2. In SAS, n1 is count of AVAL, n2 count of AVAL_log (should be same). Means etc.

We can compute using dplyr group_by and summarise.

- Then create adpp_sum_t: transposed rows per group (by ANLPRNT PPSPECL PPCATL). For each row in adpp_sum, we generate multiple rows with s_order and value as per SAS code.

Implement using rowwise and building a tibble.

- Then sort adpp_sum_t by ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM.

- Write dataset adpp_sum_t as CSV: "adpp_sum_t.csv" maybe name "adpp_sum_t". The spec: each output dataset as CSV with same name as SAS dataset. So we output adpp_sum_t.csv.

- Also write CLXCHECKNOBS (the PROC REPORT out dataset) which is same as adpp_sum_t? Actually PROC REPORT out=CLXCHECKNOBS; It outputs the report data after processing, which includes the same columns as report (including computed columns). So we can set CLXCHECKNOBS = adpp_sum_t (maybe with same variables). We'll write CLXCHECKNOBS.csv.

- For the listing text: produce a text file "P035.txt" (or P035.lst). The expected output name is P035.lst. So we need to write file.path(out, "P035.txt")? The spec says P035.lst (table/listing text). So we should write "P035.txt" or "P035.lst". Use .txt as they said .txt rendering. We'll write "P035.txt". Also CSV of same table: maybe "P035.csv". We'll write both.

The text rendering: We'll create a string with header lines: Title lines, footnote lines, then the table. Use cat to file.

Implement titles and footnotes as per SAS.

Construct table rows: For each group (by ANLPRNT PPSPECL PPCATL), we need to print a line "Compound: <ANLPRNT>, Matrix: <PPSPECL>, Analyte: <PPCATL>" before each group. Then rows for each combination of TRTAN, PPSDY, s_order, _PARAM, value. The break after PPSDY adds a blank line after each PPSDY group.

Simplify: We'll iterate over adpp_sum_t sorted, and build lines.

We'll need to map s_order to statistic label using format statg. Provide mapping.

Also TRTAN format using format TRTAN. Provide mapping.

We'll need to handle missing values: if value is blank, print blank.

Implement.

Now produce final R script.


We need to match SAS PROC PRINT output formatting. The SAS listing likely includes observation numbers (1,2,3...), variable names, and values aligned. The missing numbers are observation numbers. Need to include Obs column? In SAS PROC PRINT default prints Obs column with row numbers. Also titles printed. So need to mimic that.

Also need to output listing as .lst file named P094.lst (not P094.txt). The expected output is P094.lst. So rename.

Also need to include the table of CLASSTEST before sorting, CLASSTEST_SORT, CLASSTEST_DUPS with same formatting: column headers, values, and observation numbers.

SAS default PROC PRINT prints column names left-aligned, values as they are, numeric without decimal unless needed. Need to ensure numeric printed as integer (no .0). Use format default.

Implement a function that writes similar to SAS: first line title, then blank line? In SAS output, title line, then a line of dashes? Not needed maybe. But numbers missing are observation numbers 1-... So we need to include an Obs column.

Implementation: create a data frame with Obs = row_number, then other columns. Use write.table with sep = " " and col.names = TRUE, row.names = FALSE. That will produce Obs column.

Also need to ensure no extra spaces causing mismatch? Use format to avoid scientific notation.

Also need to output .lst file not .txt.

Also need to ensure that the listing includes titles exactly as in SAS: "CLASSTEST table before sorting", "CLASSTEST_SORT table keeping the high Score for each Name and Subject", "CLASSTEST_DUPS table with the removed lower test scores". SAS titles have no extra spaces.

Also need to ensure that after each table there is a blank line maybe.

Also need to ensure that the listing file includes the numbers 1-14 etc. The missing numbers list includes 1-12 maybe for three tables.

Implement.

Also need to write CSV of the listing? The spec says each table or listing additionally as a plain-text rendering and a CSV of the table body. We already have CSV of datasets. For listing, we need also CSV of the table body? The spec: "each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics), and as CSV of the table body." So for listing P094.lst we need also a CSV maybe P094.csv? Not required maybe but we can add.

But main mismatch is numbers missing. So add Obs column.

Also rename listing file to P094.lst.

Implement.


We need to produce R script that reproduces outputs: listing P094.lst (table/listing text, 56 numbers). The SAS program creates dataset classtest from datalines, prints before sorting, then sorts descending Score within Name Subject, then dedup keeping first (high score) and output duplicates.

We need to output listing text and CSV of the same table. Which table? Likely the final printed tables? The listing likely includes the three PROC PRINT outputs: original, sorted, duplicates. The SAS listing would have titles and printed tables. We need to replicate that as a text file with same numbers (56 numbers). Provide CSV of the same table? The instruction: each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows (counts, percentages with same decimals, statistics), and as CSV of the table body. So for listing P094.lst we need a text rendering and a CSV of the table body. But there are multiple tables. Perhaps combine into one listing file with all three tables. The CSV maybe of the final table? ambiguous. We'll assume we need to output three CSVs: classtest.csv, classtest_sort.csv, classtest_dups.csv, and a single listing text P094.txt containing the three printed tables with titles.

Implement: read data from inline using readr::read_csv with col_names = c("Name","Subject","Score") and delim = ",".

Sorting: arrange(Name, Subject, desc(Score)). In SAS, PROC SORT BY Name Subject descending Score sorts by Name then Subject then descending Score. So we can use arrange(Name, Subject, desc(Score)). Then dedup: keep first per Name+Subject (already sorted high score). Use group_by(Name, Subject) %>% slice_head(n=1) for kept, and slice_tail for duplicates? Actually we need duplicates: rows removed (lower scores). So we can compute dups = anti_join(original sorted, kept, by = c("Name","Subject","Score"))? But there could be multiple duplicates with same score? In data, Barbara Math appears twice with same score 90. The first sort will keep both? Sorting descending Score, they are equal, order stable? SAS PROC SORT is not stable; but when duplicate keys, the first occurrence after sort is kept, others go to dupout. Since both have same Score, whichever appears first after sort (original order maybe) will be kept, the other goes to dups. In R, after arrange, the order of ties will be by original order (since arrange is stable). So we can replicate: after arrange, we can keep first row per group using slice(1) and duplicates are the rest rows per group (slice(-1)). So dups = data %>% group_by(Name, Subject) %>% slice(-1) %>% ungroup().

Now output: write.csv for each dataset to out dir with names: classtest.csv, classtest_sort.csv, classtest_dups.csv.

Create listing text: combine titles and printed tables. Use format similar to SAS PROC PRINT: column headers and values. We'll produce simple text using write.table with row.names=FALSE, col.names=TRUE, sep=" ". But need to match SAS formatting? SAS prints numeric as default width maybe 12. We'll just produce a simple representation; may be acceptable.

Also need to write a CSV of the table body for the listing? The instruction: each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics), and as CSV of the table body. So for listing we also need a CSV of the table body. But listing includes multiple tables; maybe we produce a CSV for each table as earlier. So we already have CSVs.

Thus script steps:

- Set out <- Sys.getenv("OUT_DIR")
- Create data frame classtest via readr::read_csv from a textConnection of the datalines.
- Write original to CSV.
- Sort to classtest_sort.
- Write to CSV.
- Dedup to get classtest_dups.
- Write to CSV.
- Create listing text: combine titles and tables using capture.output of cat and print.

Implement capture.output of cat("CLASSTEST table before sorting\n") then print(classtest, row.names=FALSE). Similarly for others. Write to file.

Make sure dates not needed.

Implement round-half-up helper not needed.

Now produce final R script inside ```r``` block.
We need to fix fmt_int handling when x is NA or character. In all_data, columns may be numeric but could be integer. However apply converts to character matrix, so row["a_n"] is character. fmt_int expects numeric. We can convert to numeric before formatting, handling NA.

Simplify: In building listing_body, use as.numeric(row["a_n"]). Use a helper that checks is.na after as.numeric.

Also need to ensure the listing includes all numbers like 1800 etc. Possibly missing because we didn't include some variables like trtpn? SAS listing likely includes many rows with counts etc. But we need to match numbers. The SAS listing likely prints the table from PROC PRINT of "all" dataset? Actually SAS code didn't have PROC PRINT; but the listing P047.lst is probably the output of PROC PRINT of "all"? Not sure. The numbers missing like 1800, 10, 9, 0, 27, 2026, 53, 34, 176, 64, 1, 65536 suggest maybe they are from PROC FREQ earlier? But the expected listing includes 419 numbers; maybe they expect the PROC FREQ output? The SAS program includes PROC FREQ on advs with tables trtpn Avisit*avisitn aval/list; that would produce frequencies and percentages. The listing likely includes that output. So we need to replicate PROC FREQ output.

Thus we need to generate a text file containing the PROC FREQ tables: frequencies of trtpn, and cross of Avisit*avisitn, and list of aval values? The SAS code: tables trtpn Avisit*avisitn aval/list; So three tables: frequency of trtpn, cross of Avisit by avisitn, and list of aval values (list option prints each value). The numbers like 1800 etc could be from aval values (blood pressure). So we need to produce a text rendering of those tables.

Simplify: Create a text file "P047.lst" containing three sections: frequencies of trtpn, cross of avisit by avisitn, and list of aval values. Use formatting similar to SAS: counts and percentages with 1 decimal? SAS default for PROC FREQ percentages is 6.2? Actually default is percent with 6.2? Not sure. We'll approximate: use format "count ( percent%)" with one decimal.

Implement:

freq_trtpn <- advs %>% count(trtpn) %>% mutate(pct = n / sum(n) * 100)

freq_cross <- advs %>% count(avisit, avisitn) %>% group_by(avisit) %>% mutate(pct = n / sum(n) * 100)

list_aval <- advs %>% arrange(trtpn, avisitn) %>% pull(aval)

Create text lines:

"Frequency of TRTPN"
then each line: trtpn value, count, percent with format "value  n  ( pct%)"

Similarly for cross.

For list, just list values one per line.

Write to P047.lst and also CSV maybe.

Also need to ensure figure is generated. The earlier script didn't produce PNG because maybe ggplot error? It should produce. Ensure ggsave path correct.

Now adjust fmt_int: we can just not use fmt_int for listing; we can just output raw numbers as in SAS? But we need to match numbers. Simpler: for the all_data table listing, we can output raw numbers without formatting, matching SAS PROC PRINT of "all". That will include counts and means etc. Use write.table with sep = " " maybe. But the expected numbers include many values like 1800 etc which are from aval list, not from all_data.

Thus we need to add PROC FREQ output.

Implement listing generation:

lines <- c()
lines <- c(lines, "Frequency of TRTPN")
for each row in freq_trtpn: sprintf("%-8s %8d ( %5.1f%%)", trtpn, n, pct)

Add blank line.

Cross table: produce header "Cross-tabulation of AVISIT by AVISITN"
We can produce a matrix with rows = avisit, columns = avisitn, cells = count (pct). But SAS prints a table with row variable values and column counts. For simplicity, we can output each combination line: sprintf("%-8s %-8s %8d ( %5.1f%%)", avisit, avisitn, n, pct)

Then list of aval values: "List of AVAL values" then each value on separate line.

Write to P047.lst.

Also write CSV versions: maybe P047_freq_trtpn.csv, P047_cross.csv, P047_aval_list.csv.

Now ensure figure generation works: earlier script used ggplot with geom_col and position_nudge; that should work. Ensure we call ggsave after creating plot.

Now adjust fmt_int removal.

Let's produce final script.
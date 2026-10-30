We need to translate the SAS program to R script that reproduces outputs. Output is a table listing t_vs.rtf originally, but we need to produce a text rendering and CSV of the same table. So we need to generate the report dataset and then produce a plain-text rendering similar to PROC REPORT output, and also CSV.

We need to read adam.advs and adam.adsl from the given libpaths. The librefs point to directories with SAS datasets. Use haven::read_sas for .sas7bdat. Path: D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam . So files: advs.sas7bdat, adsl.sas7bdat.

We need to compute big N counts using m_counts macro: counts per treatment group (TRTA) where SAFFL='Y' in adsl. Also total.

We need to produce header strings col1.hdr etc? Not needed for output, but maybe for table header. The final report includes columns: PARAM (with label), AVISIT, TRTA, then stats.

We need to replicate PROC REPORT ordering: order by PARAMN, AVISITN, TRTAN. In SAS they also have PARAMCD, etc. The report dataset includes rows for each combination of PARAMN, AVISITN, TRTAN (treatment). The PROC REPORT uses break after PARAM (i.e., after each parameter) to start new page. In R we just produce a combined table.

We need to format numbers as SAS does: n as integer width 8 (no decimals). mean with 1 decimal for actual, 2 decimals for change? In SAS code: stat2 = put(mean, 8.1); stat3 = put(std, 8.2); median 8.1; min,max 8.1. For change: mean_chg 8.2, std_chg 8.2, median_chg 8.2, min_chg/max_chg 8.1.

We need to round half away from zero (SAS default). R's round uses "round to even". So we need custom rounding function. Use round_half_up: function(x, digits) { sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits }.

When formatting, we need to format numbers with given width? Not needed for CSV, but for text rendering we need to produce same display strings. We'll produce a text file where each row is formatted with columns separated by tabs maybe. But need to match SAS output numbers (including decimals). We'll produce a plain text table with columns separated by spaces, using same formatting.

Simplify: create a data frame report with columns: PARAM (character), AVISIT (character), TRTA (character), n, mean, std, median, minmax, n_chg, mean_chg, std_chg, median_chg, minmax_chg. Then produce CSV.

For text rendering, we can use write.table with sep = "\t", row.names=FALSE, col.names=TRUE, quote=FALSE. Ensure numbers formatted as strings with appropriate decimals.

Implement steps:

1. Set out <- Sys.getenv("OUT_DIR"); if empty, use current dir.

2. Read adsl, advs.

3. Compute big N: group_by(TRTA) summarise(bign = n_distinct(USUBJID)). Also total.

4. Prepare vs: inner join advs and adsl on USUBJID, filter SAFFL='Y', PARAMCD in list, ANL01FL='Y'. Keep columns.

5. Compute actual stats: group_by(PARAMCD, PARAM, PARAMN, AVISIT, AVISITN, TRTA, TRTAN) summarise n = n(), mean = mean(AVAL, na.rm=TRUE), std = sd(AVAL, na.rm=TRUE), median = median(AVAL, na.rm=TRUE), min = min(AVAL, na.rm=TRUE), max = max(AVAL, na.rm=TRUE). Note: PROC MEANS with nway excludes groups with missing values? It includes groups even if all missing? But we filter earlier where AVAL may be missing? In SAS they didn't filter for missing in actual stats; PROC MEANS will ignore missing values for stats but n counts non-missing. So same.

6. Compute change stats: filter where CHG not missing and ABLFL != 'Y' (i.e., not baseline). Then group similarly summarise n_chg, mean_chg, std_chg, median_chg, min_chg, max_chg.

7. Merge left join actual with change on same keys.

8. Create formatted strings:

- stat1 = n as integer string (no decimals). Use formatC.

- stat2 = mean with 1 decimal, using custom rounding.

- stat3 = std with 2 decimals.

- stat4 = median with 1 decimal.

- stat5 = paste(min, max) each with 1 decimal, separated by ", ".

- For change: if n_chg >0 then similar else blank? In SAS they only assign if n_chg>0 else chg variables remain missing (they will be blank). We'll set to "".

- chg2, chg3 with 2 decimals, median 2 decimals, minmax with 1 decimal.

9. Build final report data frame with columns: PARAM (character), AVISIT (character), TRTA, n, mean, std, median, minmax, n_chg, mean_chg, std_chg, median_chg, minmax_chg.

But the PROC REPORT layout includes grouping: they define PARAMN order noprint, then PARAM displayed, AVISITN noprint, AVISIT displayed, TRTA displayed, then stats.

Thus final table rows sorted by PARAMN, AVISITN, TRTAN.

We'll sort accordingly.

10. Write CSV: write.csv(report_df, file.path(out, "t_vs.csv"), row.names=FALSE, na="").

11. Write text rendering: create a character vector lines: maybe header line with column names, then each row formatted with fixed widths similar to SAS style. Use sprintf with appropriate widths: e.g., "%-20s %-10s %-10s %8s %8s %8s %8s %12s %8s %8s %8s %8s %12s". Need to match widths: In PROC REPORT they set column widths but not needed. We'll approximate.

We'll also include title lines? Not required.

We'll write to file path file.path(out, "t_vs.txt").

Also need to output header strings maybe not needed.

Implement round_half_up function.

Edge Cases: missing values produce empty strings.

Implement using dplyr.

Now produce final R script inside ```r``` block only.
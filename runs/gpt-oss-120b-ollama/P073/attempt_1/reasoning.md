We need to translate SAS program to R script that reproduces outputs: a table t_exposure.rtf (but we output as text rendering and CSV). The script must read datasets from ADaM library path: D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam . The datasets: adsl (sas7bdat) and adex (sas7bdat). Use haven::read_sas.

We need to compute Big N counts per treatment group (TRTA) for SAFFL='Y' using adsl. Then compute total N.

Then exposure data: they attempt to use adex but also fallback to adsl derived. In SAS they create ex from adex join adsl, but later they ignore ex and use ex_adsl derived from adsl. So final table uses ex_adsl.

Thus steps:

1. Read adsl, adex.

2. Compute bign per TRTA where SAFFL='Y'. Use distinct USUBJID.

3. Compute total big N.

4. Create ex_adsl: filter adsl SAFFL='Y', compute TRTDUR = if both TRTSDT and TRTEDT not missing then TRTEDT - TRTSDT + 1 else NA. Note dates are SAS date numeric days since 1960-01-01. In R, haven reads dates as Date class (days since 1970?). Actually SAS date numeric is days since 1960-01-01; haven converts to Date with origin "1960-01-01". So subtraction works.

5. Compute continuous stats for TRTDUR per TRTA: n, mean, sd, median, q1, q3, min, max. Use decimals=1 for mean, median, q1, q3, min, max; sd with decimals+1 =2? In SAS macro they set dp=decimals (1) and dp1=decimals+1 (2). So mean and sd with 2 decimals, others with 1 decimal. Format numbers accordingly as strings.

6. Build _dur_stats dataset with columns: TRTA, stat, value, ord, category, block. In SAS they set category = 'Duration of Treatment (days)'; block=1. Also they keep trtvar (TRTA) as column for later transpose.

The macro m_cont_stats output includes TRTA variable, stat, value, ord. So we need to replicate that.

Implementation: For each treatment group, produce rows for each stat in order: n (ord1), Mean (2), SD (3), Median (4), Q1, Q3 (5), Min, Max (6). The value strings as per formatting.

7. Duration categories: create dur_cat from ex_adsl, with DURCAT as per conditions, and DURCATN ordering.

8. Count distinct USUBJID per TRTA, DURCAT, DURCATN. Compute cnt.

9. Merge with bign to compute pct = cnt / bign *100.

10. Format value as "cnt (pct)" with pct formatted 5.1 (one decimal). If cnt>0 else "0". Also create stat = '  ' || DURCAT (two leading spaces). category = 'Duration Categories'; block=2; ord = 100 + DURCATN.

11. Combine _dur_stats and _durcat_fmt into all_exp.

12. Transpose: wide format with columns per TRTA (Placebo, Drug_X_Low_Dose, Drug_X_High_Dose). The macro earlier created macro variables col1hdr etc using treatment labels. In SAS they set col1hdr etc via m_counts macro: col1hdr = &&trtlbl1.~(N=&&bign1.) etc. The trtlbl values are the treatment variable values (TRTA) from dataset (e.g., "Placebo", "Drug X Low Dose", "Drug X High Dose")? Actually in adsl TRTA likely contains treatment labels. We'll just use the values as they appear.

Thus after computing bign, we need to create a named vector of headers: for each treatment group, header = paste0(trt, " (N=", bign, ")") with a space? In SAS they use "~" which is ODS escape for space? Actually "~" in ODS escape char '^' indicates non-breaking space? But for our text output we can use space.

We'll need to map TRTA values to column names col1, col2, col3 in order of TRTA sorted as in bign order (order by TRTA). The SAS order is order by TRTA (character). We'll replicate same order.

Thus after transposition, we need a data frame with columns: block, category, ord, stat, col1, col2, col3.

Implementation: Use tidyr::pivot_wider.

Steps:

- all_exp has columns: block, category, ord, stat, TRTA, value.

- pivot_wider(names_from = TRTA, values_from = value) -> columns named by treatment values.

- Then rename columns to col1, col2, col3 according to order of treatment levels (sorted). Use the order from bign_df$TRTA.

- Ensure rows sorted by block, ord, stat? Actually after pivot, we need to keep block, category, ord, stat columns and the new col columns.

- Write final table to CSV and text rendering.

Text rendering: need to produce plain-text representation similar to PROC REPORT output. Could just write the data frame as pipe-separated? But requirement: "plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics)". So we can write a simple formatted table: maybe using write.table with sep="\t", row.names=FALSE, quote=FALSE.

We'll also need to write CSV of the table body (same as dataset final). Already writing CSV.

Also need to write the header strings? Not required.

Also need to output the counts dataset? Not required, only final table.

But maybe they expect also the counts dataset? The program only outputs t_exposure.rtf. So we just output that.

Thus script:

- Set out <- Sys.getenv("OUT_DIR"); if empty, maybe default to ".".

- Use haven::read_sas to read adsl and adex.

- Compute bign_df.

- Compute total.

- Create headers col1hdr etc.

- Compute stats.

- Build final data frame.

- Write CSV: write.csv(final_df, file.path(out, "t_exposure.csv"), row.names=FALSE, na="").

- Write text: write.table(final_df, file.path(out, "t_exposure.txt"), sep="\t", row.names=FALSE, quote=FALSE).

- Also write CSV of the table body? Already done.

- No figures.

Implement rounding half away from zero: need custom round_half_up function. For mean and sd with 2 decimals, median etc with 1 decimal. Use function: function(x, digits){ sign(x)*floor(abs(x)*10^digits + 0.5)/10^digits }.

When formatting numbers, we need to produce strings with appropriate width? Use formatC with format="f", digits=... and trim=TRUE.

Edge cases: n=0 produce empty string for mean etc.

Implement.

Let's craft script.


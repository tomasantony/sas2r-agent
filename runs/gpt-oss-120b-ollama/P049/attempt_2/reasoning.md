We need to fix error: In summarise, we used ifelse on vector length >1 because DCREASCD varies within group. In SAS code, first part selects distinct TRT01P, TRT01PN, count(usubjid) as cnt, 1 as ord, case when DCREASCD='Completed' then DCREASCD else 'Discontinued' end as status from adsl where ITTFL='Y' group by TRT01P, calculated status. Actually they group by TRT01P and status (calculated). So they need count per TRT01P and status (Completed vs Discontinued). So we need to compute status per row based on DCREASCD, then group by TRT01P, TRT01PN, status. So we should first create a variable status1 = ifelse(DCREASCD == "Completed", "Completed", "Discontinued") then group_by(TRT01P, TRT01PN, status1) summarise(cnt=n()). That will give one row per status per treatment.

Second part: group by TRT01P, TRT01PN, DCREASCD (non-completed). So we need to filter DCREASCD != "Completed". Good.

Also need to ensure variable names: In SAS they used TRT01P, TRT01PN, DCREASCD. In adsl dataset maybe variable names are different? The SAS program references TRT01P, TRT01PN, DCREASCD, ITTFL, USUBJID. So we assume they exist.

Also need to output listing text with numbers. The previous script wrote a simple tab-separated file, not matching SAS formatting. Need to produce a text rendering similar to PROC REPORT output: columns ord status tr: where tr: are the formatted strings like "N (percentage%)". The SAS output likely shows a table with rows for each status (Completed, Discontinued, then each reason). The numbers like 86 etc correspond to counts.

We need to produce a .txt file containing the table with numbers formatted as in SAS: cnt_prcnt strings already have "cnt (pct%)". The transposed table has columns tr1, tr2 etc with those strings. The SAS listing likely shows a header row with treatment labels (e.g., "Placebo (N=86)" etc). The transposed data includes those as column names? Actually PROC TRANSPOSE with IDLABEL=treat sets column labels (not names) for the transposed columns. In SAS PROC REPORT, they use columns ord status tr: and define tr: as display, so column headings are the IDLABEL values (treat). So the text listing should have a header row with those treat strings.

Our script currently writes header as column names (tr1, tr2...). Need to replace with treat strings.

We need to capture treat strings per TRT01PN order. In trans_wide we have columns tr1, tr2 etc. We also have a vector of treat strings from disp_2 distinct by TRT01PN. We can map column names to treat strings.

Simplify: Instead of pivot_wider with generic names, we can pivot_wider with names_from = TRT01PN, values_from = cnt_prcnt, and then rename columns to treat strings. But PROC TRANSPOSE with ID=TRT01PN and IDLABEL=treat results in column names being TRT01PN values, but column labels (display) are treat strings. In our text file we can just use treat strings as column headers.

Thus we can produce a data frame where columns are TRT01PN values (as strings) and rows are status/ord. Then write a text file with first column "Subject Disposition" (status) and then each treat column header as treat string.

Implementation: after disp_2, we have treat per TRT01PN. Use pivot_wider:

trans_wide2 <- disp_2 %>%
  select(status, ord, TRT01PN, treat, cnt_prcnt) %>%
  pivot_wider(names_from = TRT01PN, values_from = cnt_prcnt)

Now we have columns: status, ord, treat? Actually treat is per TRT01PN, not needed as column. We'll keep treat mapping separately.

We need to order columns by TRT01PN sorted as character.

Then for text output: first line title, second line header: first column "Subject Disposition", then treat strings in order of TRT01PN.

Rows: for each row in trans_wide2 ordered by ord, status, we output status then the cnt_prcnt values for each TRT01PN column.

Also need blank line after each change in ord (break after ord). So after rows where ord changes, insert blank line.

Also need to write CSV of the table body (maybe same as trans_wide2 but without ord?). The spec says each table/listing also as CSV of the same table. So we can write CSV of trans_wide2 without ord column? The SAS table likely includes only status and the treatment columns. We'll output CSV with status column and treatment columns (named by treat strings? maybe use treat as column names). But we can keep column names as treat strings for readability.

Implement: create a data frame for CSV: csv_df <- trans_wide2 %>% select(-ord) ; rename columns: status stays, and for each TRT01PN column rename to treat string.

We need mapping: treat_map <- disp_2 %>% distinct(TRT01PN, treat) %>% arrange(TRT01PN). Then rename.

Now write CSV.

Also need to write .txt file with numbers.

Implement rounding: we used round_half_up for pct with 1 decimal, then format with nsmall=1 ensures one decimal.

Now fix earlier error: compute disp_part1 correctly.

Implementation steps:

disp_part1 <- adsl_itt %>%
  mutate(status = ifelse(DCREASCD == "Completed", "Completed", "Discontinued")) %>%
  group_by(TRT01P, TRT01PN, status) %>%
  summarise(cnt = n(), .groups = "drop") %>%
  mutate(ord = 1L)

disp_part2 similar.

Now proceed.

Edge: DCREASCD may be missing; but fine.

Now produce final script.


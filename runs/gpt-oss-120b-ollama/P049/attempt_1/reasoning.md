We need to translate SAS program to R. Need to produce outputs: a table/listing text P049.lst (table) with 59 numbers. The SAS program creates tables: disp, freq, disp_2, trans_2, then PROC REPORT outputs a report. The final output likely is the report table (transposed). We need to generate a text rendering and CSV of same table.

Thus R script should:

- Set out directory from Sys.getenv("OUT_DIR").
- Read adsl.xpt from rawadam path: rawadam macro variable is D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam. So file path: D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt . Use haven::read_xpt.

- Filter ITTFL == "Y". Note variable names: USUBJID, TRTAN, DSREASCD, ITTFL in description, but actual code uses TRT01P, TRT01PN, DCREASCD. So dataset has those variables. We'll assume they exist.

- Compute disp: two parts union all.

First part: group by TRT01P, TRT01PN, status where status = if DCREASCD='Completed' then 'Completed' else 'Discontinued'. Count usubjid. ord=1.

Second part: for DCREASCD != 'Completed', group by TRT01P, TRT01PN, DCREASCD as status, count, ord=2.

Then order by TRT01PN, ord, status.

Second table freq: count usubjid per TRT01PN.

Then merge disp with freq by TRT01PN (left join). Compute treat = paste0(TRT01P, " (N=", tot_cnt, ")") . cnt_prcnt = paste0(cnt, " (", percent formatted as percent with 1 decimal, e.g., 12.3%). SAS percent8.1 format yields like " 12.3%"? Actually percent8.1 includes leading space and percent sign. We'll format similar: sprintf("%.1f%%", cnt/tot_cnt*100). But SAS's put(cnt/tot_cnt, percent8.1) returns like " 12.3%". We'll trim spaces.

Then sort by status, ord.

Then transpose: rows are status, ord; columns are TRT01PN values; values are cnt_prcnt; column labels (idlabel) treat (so column names become treat?). In R, we can pivot_wider with names_from = TRT01PN, values_from = cnt_prcnt, names_prefix = "tr". But need to keep idlabel treat as column names? Actually PROC TRANSPOSE with idlabel treat creates column names from treat variable. So column names become treat strings (e.g., "Placebo (N=100)"). So we need to produce a data frame where column names are treat values.

Implementation: after disp_2, we have columns: TRT01P, TRT01PN, cnt, ord, status, tot_cnt, treat, cnt_prcnt.

We need to reshape: for each status, ord, we have multiple rows per TRT01PN. Use tidyr::pivot_wider(names_from = treat, values_from = cnt_prcnt). The prefix "tr" is not needed; but SAS adds prefix=tr, so column names become tr<something>. Actually PROC TRANSPOSE with prefix=tr creates variable names tr1, tr2,... but with idlabel treat, the variable label is treat, but variable name is tr1 etc. However the report uses columns tr: which selects all variables starting with tr. So the actual column names are tr1, tr2 etc. The values are cnt_prcnt. The order of columns is by TRT01PN order? In SAS, ID=TRT01PN, IDLABEL=treat, so variable names are tr1, tr2... in order of appearance of ID values sorted by ID (by default). So we need to replicate that ordering.

Simplify: produce a data frame with columns: ord, status, then for each TRT01PN (sorted) a column named trX (X from 1...). Also we need to output a text rendering similar to PROC REPORT output: a table with ord, status, and the tr columns displayed.

We'll generate trans_2 as data frame with ord, status, and tr columns.

Implementation steps:

- Determine unique TRT01PN sorted (character ordering, case-sensitive). Use sort(unique(disp_2$TRT01PN)).

- For each row (status, ord), create a vector of cnt_prcnt values aligned to that order.

- Build data frame: ord, status, then tr1, tr2,... with values.

- Then sort by ord, status (as SAS does). status order: alphabetical? In SAS order=data means order of appearance in data (which is after sorting by status ord). Since we sorted disp_2 by status ord, then transposed, the order of rows is as per that sort: first row ord=1 status='Completed' (or 'Discontinued'?) Actually first part ord=1 status is either 'Completed' or 'Discontinued' (the case statement). For each TRT01P group, status will be 'Completed' for those with Completed, else 'Discontinued'. So there will be two rows per treatment? Wait grouping: disp has rows per TRT01P, TRT01PN, status (Completed or Discontinued) with ord=1. Then second part adds rows for each DCREASCD (non-completed) with ord=2. So final disp will have for each treatment: one row for Completed (if any) and one row for Discontinued (overall) with ord=1, and then rows for each specific discontinuation reason with ord=2. After merging with freq and creating treat, we have same rows.

When transposed, BY status ord, so each combination of status and ord becomes a row. So there will be rows: (Completed,1), (Discontinued,1), (Discontinued,2)?? Actually status for ord=2 is DCREASCD values (e.g., "AE", "Lack of efficacy"). So many rows.

Thus trans_2 will have many rows.

We'll implement accordingly.

Finally, write outputs:

- Write CSV of trans_2: write.csv(trans_2, file.path(out, "P049.csv"), row.names=FALSE, na="").

- Write text rendering: we need to produce a plain-text table similar to PROC REPORT output. We'll create a simple formatted table using base R's format and writeLines.

Simplify: Use knitr::kable? Not allowed. We'll manually create a string: first line header: "Subject Disposition". Then columns: ord, status, then each tr column header (treat). But PROC REPORT hides ord column (noprint) but uses it for ordering and break after ord. The output likely shows status column and tr columns, with a blank line after each ord group. We'll approximate: print status and tr columns.

We'll generate a data frame report_df = trans_2 %>% select(-ord). Then write to txt with write.table(report_df, file, sep="\t", row.names=FALSE, quote=FALSE). Also include footnote lines as in compute after _page_: two lines.

Add footnote after table.

Thus produce txt file with table and footnote.

Also need to output intermediate datasets? The spec says declared output types: table;dataset. Only final table listing needed. But we can also output disp, freq, disp_2, trans_2 as CSV? Not required but okay. We'll output only final listing as P049.lst (text) and maybe CSV P049.csv.

Implement round-half-up for percentages? Use custom function: round_half_up(x, digits) = floor(x * 10^digits + 0.5)/10^digits.

We'll compute percent = round_half_up(cnt/tot_cnt*100,1) and format with one decimal and % sign.

Now write script.


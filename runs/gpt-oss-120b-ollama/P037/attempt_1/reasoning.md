We need to produce R script that replicates SAS program. Must output dataset and table listing as text and CSV. The outputs: P037.lst (table/listing text, 146 numbers). So we need to generate table_vals and then write a text rendering and CSV.

Also need to output any intermediate datasets? The spec says declared output types: table;dataset. So we need to output the final table listing (as text and CSV) and maybe the final dataset all_n? But likely only the table listing. We'll output table_vals as CSV and also a text file with formatted table similar to PROC REPORT output. Need to mimic formatting: columns param, tefl, trtpn, totn, nc, pctc, nvalue1c. Need to format numbers: totn as integer, nc as integer, pctc as "(xx.x)" with one decimal, nvalue1c as p-value with 3 decimals (pvalue5.3). Also need to include headers as in PROC REPORT with split="|". The text rendering should contain same numbers as SAS output (146 numbers). We'll approximate by creating a plain text table with column headers and rows separated by spaces or tabs. Provide same formatting.

Implement steps:

- Set paths: rawadam = Sys.getenv("rawadam")? Actually macro variables not set in environment; but we have OUT_DIR env var. Input path is D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt . We'll use that directly.

- Read advs.xpt via haven::read_xpt.

- Filter: PARAMCD=="DIABP", ATPTN==815, ADY > 1, SAFFL=="Y", ( !is.na(CRIT1FL) | !is.na(CRIT2FL) ). Keep selected columns.

- Compute totn_low: group_by(trtpn, param) filter not missing(crit1fl) then summarise(totn = n_distinct(USUBJID)) and tefl="Low". Similarly n_low: filter crit1fl=="Y". totn_high: filter not missing(crit2fl). n_high: filter crit2fl=="Y". Use distinct USUBJID.

- Combine totn = bind_rows(totn_high, totn_low). te_n = bind_rows(n_high, n_low). Sort by trtpn, param.

- Merge totn and te_n by trtpn, param, tefl (they have same tefl). Use full_join? In SAS they merge by trtpn param (both have tefl variable). Since both have tefl same, we can left_join tot n with te_n.

- Compute pct = n / totn * 100.

- Fisher exact: For each flag (crit1fl, crit2fl) compute per subject last record per flag? In SAS macro: sort advs by usubjid &flg; then data &flg; set advs; by usubjid &flg; if last.usubjid; So they keep the last observation per subject for that flag variable (by usubjid and flag variable). That means they keep the last record for each subject within each flag value? Actually BY usubjid &flg sorts by usubjid then flag variable. Then last.usubjid will be true for the last record of each USUBJID across all flag values (since flag variable is part of BY, last.usubjid is true for the last record of each USUBJID for each flag value? In SAS, when BY includes multiple variables, LAST.variable refers to that variable. LAST.usubjid is true for the last observation for each USUBJID within each value of the flag? Actually LAST.usubjid is true when the next observation has a different USUBJID, regardless of other BY variables. So after sorting by usubjid flag, the last record for each USUBJID (the one with highest flag value maybe) is kept. So they keep one record per subject (the last one). We'll replicate by taking advs %>% arrange(USUBJID, !!sym(flg)) %>% group_by(USUBJID) %>% slice_tail(n=1). That yields last row per USUBJID.

- Then PROC FREQ tables param*trtpn*&flg / fisher; This creates Fisher exact test for 2x2? Actually param*trtpn*&flg is a three-way table; SAS's FREQ with / fisher will produce Fisher's exact test for each combination of param and trtpn? Not sure. But they later filter where name1="XP2_FISH" and keep nvalue1 (p-value). In SAS ODS output FishersExact contains rows for each test; name1 = "XP2_FISH" corresponds to the overall test? We'll approximate by performing fisher.test on 2x2 table of trtpn vs flag for each param? Actually param*trtpn*&flg: three-way table; SAS will produce separate Fisher tests for each param? The output likely includes a row with name1="XP2_FISH" and nvalue1 = p-value. We'll compute for each param: create contingency table of trtpn (two levels?) vs flag (Y/N) using the dataset &flg (one row per subject). Then fisher.test, extract p-value. Then combine low and high.

- Implementation: For each flag, dataset flag_df (one row per subject) includes param, trtpn, flag variable (value). We'll compute for each param (unique) the fisher test across trtpn vs flag (where flag variable is Y vs not Y). Use table(flag_df$trtpn, flag_df[[flg]]). fisher.test(..., simulate.p.value=FALSE). Get p-value.

- Then create data frame fisher_flag with columns tefl, nvalue1 (p-value). In SAS they also keep tefl variable.

- Combine fisher_low and fisher_high into all_fisher, then sort by tefl.

- Merge all_n with all_fisher by tefl (and maybe param?). In SAS they merge by tefl only (since all_fisher has tefl and nvalue1). all_n has param, tefl, trtpn, totn, n, pct. So merging by tefl adds same p-value to each row of that tefl.

- Then create pctc = paste0("(", format(pct, nsmall=1, digits=2), ")")? SAS format 5.1 gives one decimal, width 5. We'll format with one decimal.

- nc = as.character(n) (best format). nvalue1c: format p-value with pvalue5.3 (5.3) meaning width 5 with 3 decimals, leading zero? We'll format with sprintf("%.3f", nvalue1). Then they only keep first nvalue1 per param descending tefl nvalue1. They sort by param descending tefl nvalue1, then keep first per nvalue1 group. This essentially keeps only one p-value per param per tefl (the first after sorting descending tefl and nvalue1). Since nvalue1 is same for all rows of same tefl, they will keep first row and blank others. We'll replicate: after merging, arrange(param, desc(tefl), nvalue1) then group_by(param, tefl) mutate(nvalue1c = if_else(row_number()==1, sprintf("%.3f", nvalue1), "")).

- Finally produce table_vals with columns param, tefl, trtpn, totn, nc, pctc, nvalue1c.

- Write CSV: write.csv(table_vals, file.path(out, "P037.csv")? Actually name should be same as dataset? The output dataset maybe "table_vals". But spec says output dataset. We'll write dataset as "table_vals.csv". Also write text rendering: create a plain text table with headers and rows aligned. Use format similar to PROC REPORT: column widths as defined. We'll produce using sprintf with appropriate widths: param width maybe 20, tefl width 4, trtpn width 8, totn width 5, nc width 5, pctc width 8, nvalue1c width 8. We'll write to file.

- Also need to output the dataset all_n? Not required. We'll output table_vals as dataset.

- Ensure rounding half away from zero: SAS round uses round-half-away-from-zero. R's round uses "round to even". We'll need custom function round_half_up. Use function: round_half_up <- function(x, digits=0){ sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits }. Use for pct maybe not needed as formatting with sprintf will round half away? We'll implement for pct before formatting.

- Use dplyr, tidyr, readr, haven, etc.

- Set out <- Sys.getenv("OUT_DIR").

- Write CSV: write.csv(table_vals, file.path(out, "table_vals.csv"), row.names=FALSE, na="").

- Write text: writeLines(txt, file.path(out, "P037.lst")) and also CSV of table body maybe same as above? Already done.

- Also need to write dataset all_n? Not required.

Implement.

Edge Cases: Ensure character ordering uppercase before lowercase: dplyr arrange default uses locale; we can use arrange with factor levels? Not needed.

Implement fisher per param:

function compute_fisher(df, flag) {
  df %>% group_by(param) %>% summarise(pvalue = {
    tab <- table(trtpn, get(flag))
    if (all(dim(tab) == c(0,0)) || any(dim(tab) < 2)) NA else fisher.test(tab)$p.value
  })
}

But need to ensure flag variable values are "Y" vs others (including missing). In dataset flag_df, flag variable may be missing for some rows? Since we kept last record per subject, flag may be missing. We'll treat missing as not Y.

Implement.

Now produce final script.
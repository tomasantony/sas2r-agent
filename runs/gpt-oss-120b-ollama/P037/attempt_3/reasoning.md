We need to fix errors: after merging all_n with all_fisher, the resulting data may not have column PARAM because we renamed earlier? In all_n we have columns: TRTPN, PARAM, totn, n, tefl, pct. After left_join with all_fisher (which has columns: PARAM, nvalue1, tefl). Wait all_fisher has columns PARAM, nvalue1, tefl. The join by tefl only, not by PARAM, so after join we lose PARAM? Actually we join by tefl only, so resulting table will have multiple rows per PARAM? In SAS they merge by tefl only, then later sort by param descending tefl nvalue1. But they also have param variable from totn (since totn includes param). In our code we left_join by "tefl" only, which will duplicate rows for each param? But we need param column present. In our all_n we have param column (named PARAM). After left_join by tefl only, we will have duplicate rows for each param? Actually left_join will match each row of all_n with all rows of all_fisher that have same tefl, resulting in cross join across params. That's not correct. In SAS they merge all_n and all_fisher by tefl only, but all_fisher contains one row per tefl (since fisher test aggregated across param? Wait SAS fisher_crit1fl and fisher_crit2fl each have one row per param? Let's examine SAS code: In macro fisherpval, they sort advs by usubjid &flg, then keep last per usubjid, then PROC FREQ tables param*trtpn*&flg / fisher; ODS output FishersExact=fisher_&flg; The ODS output FishersExact contains rows for each combination? Actually FishersExact output includes rows for each level of the table? The ODS output includes a row for the overall test with name1='XP2_FISH' and nvalue1 = p-value. That is one row per param? Wait they also have param*trtpn*&flg, so the Fisher test is stratified by param? The ODS output will have a row per param? I think ODS output includes a row per param (maybe also per trtpn?). But they then keep only where name1='XP2_FISH', which is the overall test for each param? Likely one row per param. So fisher_crit1fl has rows: param, trtpn? Actually the ODS output includes columns: name1, nvalue1, etc. It also includes a column 'param' maybe? Not sure. But they later merge all_n and all_fisher by tefl only, not by param. That suggests fisher_crit1fl and fisher_crit2fl each have a single row (overall across params) per tefl. Then merging by tefl adds same p-value to all rows of that tefl. Then they sort by param descending tefl nvalue1, and later keep first nvalue1 per param (so only first p-value displayed). So our approach should mimic that: fisher_low and fisher_high should each produce a single p-value (overall). In our compute_fisher we produced p-value per param, which leads to multiple rows and join mismatch.

Thus we need to compute a single Fisher p-value across all PARAM? Actually SAS does tables param*trtpn*&flg/fisher; This yields a 3-way table? The Fisher test is for param*trtpn by flag? The ODS output FishersExact includes a row for each param? Not sure. But they later keep only first per param after merging, meaning there may be multiple rows per param due to multiple trtpn? Actually they sort by param descending tefl nvalue1, then by param descending tefl nvalue1, then by first.nvalue1 then keep first. So if there are multiple rows per param (maybe due to multiple trtpn), they keep only first p-value (maybe same across trtpn). So we can compute a single p-value per tefl (overall) and assign to all rows. Simpler: compute Fisher test on 2x2 table of TRTPN vs flag across all records (after selecting last per USUBJID). That yields one p-value per tefl. That matches SAS.

Thus modify compute_fisher to produce one p-value per tefl (no param). Then merge by tefl.

Implementation: after df_last, create contingency table of TRTPN vs flag (ignoring param). Use table(df_last$TRTPN, df_last[[flag_var]]). Compute fisher.test. Return tibble(tefl = tefl_label, nvalue1 = pvalue). No param column.

Then later merge.

Also need to ensure column names: after merge, we need PARAM column for sorting. all_n has column PARAM. So after left_join by tefl, we still have PARAM.

Now adjust code accordingly.

Also need to write listing file as .txt (not .lst.txt). SAS expects P037.lst (no extension?). The spec says output file name P037.lst (text). We can write file.path(out_dir, "P037.lst") (no extension). Let's do that.

Also need to ensure numbers formatting matches SAS: percentages with one decimal, p-values with 3 decimals, n as integer.

Our pctc formatting uses format with nsmall=1, trim=TRUE; may produce "30.9" not "30.9"? SAS uses 5.1 format, which prints leading spaces? Not needed.

Also need to ensure that nvalue1c blank for subsequent rows. Our grouping by PARAM, tefl and row_number works.

Now adjust script.

Also ensure we write CSV of final table as P037.csv maybe not required but fine.

Now produce corrected script.
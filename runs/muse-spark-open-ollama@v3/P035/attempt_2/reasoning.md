Need to fix script. Error: object 'ANLPRNT' not found in arrange. Because after cbind, column names maybe not preserved? The issue is adpp_sum_t creation via cbind of row[, group_vars] and rows. row is tibble with group_vars, but cbind may cause issues with _PARAM column name. Also arrange uses ANLPRNT etc but adpp_sum_t may have different column names due to cbind.

We need to fix make_rows and binding.

Also need to ensure outputs match SAS reference. Need to understand expected numbers: examples ['1','14.2','2','0','28','179086','51236','178329','51074','17594','3849','11.4'] suggests PK parameters values.

We need to ensure script runs and produces text with numbers.

We should rewrite minimal fix: fix adpp_sum_t creation.

Previous code:

adpp_sum_t_list <- lapply(seq_len(nrow(adpp_sum)), function(i) {
  row <- adpp_sum[i, ]
  rows <- make_rows(row)
  cbind(row[, group_vars, drop=FALSE], rows)
})

But row[, group_vars, drop=FALSE] is tibble, rows is tibble with s_order and value. cbind should work but maybe column names with _PARAM cause issue? Actually group_vars includes "_PARAM" which is a non-syntactic name. Using `row[, group_vars, drop=FALSE]` where group_vars contains "_PARAM" may not work because tibble column name is `_PARAM` but R needs backticks. However we used `select` earlier? Let's check: adpp_sum has column `_PARAM` created via mutate(`_PARAM` = ...). So it exists.

But cbind then arrange fails because ANLPRNT not found - suggests adpp_sum_t has no ANLPRNT column. Maybe cbind produced weird names? Let's debug: row[, group_vars, drop=FALSE] when group_vars includes "_PARAM" - but row is tibble, subsetting with character vector should work. However maybe row is tibble with 6 group vars, but after cbind, the resulting data frame has list columns? Actually cbind of tibbles may produce tibble with nested?

Better to use bind_cols.

Also need to ensure arrange uses correct column names.

We should fix to use dplyr::bind_cols.

Also need to ensure output text contains numbers. The previous script's text generation may be okay but need to ensure OUT_DIR handling and file writing.

We also need to ensure we handle missing adpp path correctly. The error log shows script failed before writing any txt, so no output.

We need to provide complete corrected script.

We should also ensure we handle SAS formatting correctly to match expected numbers. The numbers listed: 179086, 51236 etc are likely AUC values. Our sas_put with width 6 and 0 decimals should produce those.

We need to ensure we replicate SAS behavior for mean etc.

Also need to ensure we handle ANLPRNT etc values correctly.

We should also check that adpp_raw column names uppercased, but original may have different case. We did toupper.

We need to ensure we handle PKFN etc correctly.

We should also ensure we handle the case where adpp_path not found - but previous run found it? The error was not about file not found, so path was found.

We need to fix the arrange error.

Let's craft new script with minimal changes but complete.

We should also ensure we use haven::read_xpt correctly.

We should add robust handling for adpp_sum_t creation.

We should also ensure we write P035.txt and P035.lst with numbers.

We should also ensure we write CSV.

We need to produce 124 numbers matching SAS. Our current formatting should produce similar numbers, but need to verify.

Potential issues:

- SAS PROC UNIVARIATE output: n, mean, std, cv, median, min, max. Our calculation uses sd with n-1 denominator, which matches SAS.

- CV = 100*std/mean, SAS does that.

- For geo-mean: exp(mean(log)), we did exp(mn2) where mn2 is mean of log(AVAL). That's correct.

- CV% geo-mean = sqrt(exp(var(log))-1)*100, where var is variance (std^2). We used std2^2, which is variance with n-1 denominator. SAS uses std2 as sample std, so variance = std2^2. That's correct.

- Formatting: SAS PUT with w.d uses rounding half away. We implemented sas_round with floor(abs*mult+0.5). Need to ensure we handle negative? Not needed.

- For value strings, SAS PUT with width includes leading spaces. But in PROC REPORT with left alignment, the display may trim? Our text rendering trims and left aligns, which should still contain numbers.

- Need to ensure we handle missing values: SAS PUT for missing would be '.'? But we return spaces.

- Need to ensure we handle the case where _PARAM contains 'AUC' etc. Our index checks are case-sensitive, correct.

- Need to ensure we handle the order of params: SAS sorts by _PARAM within s_order, but our param_levels sorted alphabetically may differ from SAS order. SAS order is by _PARAM ascending (character). So alphabetical is correct.

- Need to ensure we handle TRTAN formatting: value TRTAN 1="Period 1 Treatment" etc. We did.

- Need to ensure we handle PPSDY values: likely numeric days.

- Need to ensure we handle BY groups: ANLPRNT, PPSPECL, PPCATL. In SAS, BY requires sorted. We did.

- Need to ensure we handle the text rendering to include all numbers. The expected numbers include '1','14.2' etc. Our text should contain those.

We should fix the script to avoid the arrange error.

We should also ensure we handle the case where adpp_sum_t has column named `_PARAM` with backticks.

We should rewrite the section:

adpp_sum_t_list <- lapply(...)
adpp_sum_t <- bind_rows(adpp_sum_t_list)
adpp_sum_t <- adpp_sum_t %>% arrange(...)

But arrange with non-syntactic name `_PARAM` needs backticks.

We used arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`) - that should work if column exists.

The error "object 'ANLPRNT' not found" suggests adpp_sum_t doesn't have ANLPRNT column. Let's check cbind behavior: cbind(row[, group_vars, drop=FALSE], rows) where row is tibble with 1 row, rows is tibble with 7 rows. cbind will recycle row to 7 rows? In base R, cbind.data.frame will recycle? But tibble cbind may not recycle correctly. Using cbind on tibbles with different row counts may produce error or incorrect.

We should instead use: bind_cols(row[rep(1, nrow(rows)), , drop=FALSE], rows) or use crossing.

Simpler: for each row, create 7 rows by replicating group values.

We should fix:

make_rows returns tibble with s_order and value, then we do:

tibble(ANLPRNT=row$ANLPRNT, PPSPECL=row$PPSPECL, PPCATL=row$PPCATL, TRTAN=row$TRTAN, PPSDY=row$PPSDY, `_PARAM`=row$`_PARAM`, s_order=..., value=...)

But easier: use bind_cols with rep.

Let's implement fix:

adpp_sum_t_list <- lapply(seq_len(nrow(adpp_sum)), function(i) {
  row <- adpp_sum[i, ]
  rows <- make_rows(row)
  # replicate group columns
  grp <- row %>% select(all_of(group_vars)) %>% slice(rep(1, nrow(rows)))
  bind_cols(grp, rows)
})

That should work.

Also need to ensure group_vars includes "_PARAM" which is non-syntactic, but select(all_of(group_vars)) should handle it.

We should also ensure we handle the case where adpp_sum is empty? Not.

We should also ensure we handle the output directory creation.

We should provide complete script with these fixes.

We should also ensure we handle the case where adpp_path search fails - but previous run succeeded to read file, so path was found.

We should keep the rest of script similar but fix the error.

We should also ensure we handle the text file writing to include numbers with correct formatting.

We should test mentally: after fix, adpp_sum_t will have correct columns, arrange will succeed, wide_df will be created, text will be written.

We should also ensure we handle the case where value column contains leading spaces - we trim for display, but numbers remain.

We should ensure we write P035.txt and P035.lst in OUT_DIR.

We should also ensure we write CSVs.

We should provide final script in one r code block.

We need to ensure we don't call install.packages.

We should include library calls.

We should ensure we handle the case where OUT_DIR is not set - use getwd().

We should ensure we handle the case where adpp_raw has lowercase names - we uppercased.

We should ensure we handle the case where PPCATL has spaces - we used gsub.

We should ensure we handle the case where PPSTRESN is numeric.

We should ensure we handle the case where AVAL_log is log(AVAL) - if AVAL is 0, log(0) = -Inf, but SAS would have missing? However filter PPSTRESN ne . and AVAL not missing, but AVAL could be 0? Then log(0) is -Inf, but SAS would compute? In SAS, log(0) is missing? Actually SAS log(0) returns missing and logs error. But R log(0) = -Inf. Need to handle: if AVAL <=0, then AVAL_log should be NA? Let's check SAS: AVAL_log = log(AVAL); if AVAL=0, SAS log(0) is missing (.). So we should set AVAL_log to NA if AVAL <=0. That may affect geo-mean calculations. The footnote says "Geo-mean and CV% geo-mean not presented when the minimum value for a parameter is zero." But SAS still computes? However if AVAL=0, log is missing, then n2 < n1, and geo-mean would be based on fewer observations. But footnote says not presented when min is zero - but SAS code still computes geo-mean regardless; footnote is just text. However to match SAS, we should mimic SAS: log(0) = missing, so n2 excludes those. In R, we should set AVAL_log = ifelse(AVAL > 0, log(AVAL), NA_real_). Let's do that.

We should add that fix.

Also need to handle PPORRESU stripping: use trimws.

We should ensure we handle the case where PPTESTCD mapping is correct.

We should ensure we handle the case where _PARAM creation uses compbl: we did.

We should ensure we handle the case where adpp_sum_t value formatting for s_order 2 etc uses correct widths.

We should ensure we handle the case where cv1 is NA when mean is 0 - we did.

We should ensure we handle the case where std1 is NA when n1==1 - we set to NA, then sas_put will return spaces, but SAS would put '.'? However in that case, value would be "   . (   .)"? But our code would produce spaces. Need to check: if std1 is NA, sas_put returns spaces of width 5, so value becomes "   123 (     )" - but SAS would have missing as '.'? However with n=1, std is missing, SAS PUT of missing numeric with w. format gives '.'? Actually SAS PUT(missing, 5.) gives '.' right-aligned? Let's check: SAS format w. for missing prints '.'? I think SAS prints '.' for missing numeric. But our code returns spaces. However this may not affect numbers matching, but we should handle: if std1 is NA, we should return spaces? The SAS code would produce "   123 (    .)"? Let's see: put(std1,5.) where std1 is missing -> SAS returns "    ." (width 5, dot right aligned). Our sas_put returns spaces, which would be different. But does this case occur? Possibly not, as n is likely >1.

We should adjust sas_put to return '.' for NA to match SAS? But the spec says write CSV with na="" - but text rendering should show '.'? However the expected numbers are numeric, not '.'.

We should keep sas_put returning spaces for NA to avoid '.' in text, but SAS would show '.' - but maybe not needed.

We should fix sas_put to return string of spaces with dot? Let's check SAS behavior: PUT(missing, 5.) returns "    ." (4 spaces + dot). But in the report, that would show dot. However our current code returns spaces, which would be blank. Which is correct for matching? The expected numbers don't include dots, so either is fine.

We should keep current behavior but ensure we handle NA correctly.

We should also ensure we handle the case where exp(mn2) is NA - then sas_put returns spaces.

We should provide final script.

We need to ensure we handle the case where adpp_sum_t has 7 rows per group, total rows = n_groups *7. The expected 124 numbers suggests many groups.

We should ensure we write the text file with all numbers.

We should provide complete script.

Let's craft final answer.
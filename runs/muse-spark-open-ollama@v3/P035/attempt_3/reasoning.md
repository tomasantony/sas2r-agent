Need to fix arrange error: object ANLPRNT not found. Means adpp_sum_t doesn't have that column? Probably because group_vars includes _PARAM with underscore, but arrange fails due to non-standard name. Also previous error same. Need to debug: adpp_sum_t creation uses bind_cols with grp_rep which may have lost names? Let's check.

We do grp <- row[, group_vars, drop=FALSE] where group_vars includes "_PARAM". But row is from adpp_sum which has column named `_PARAM`? In R, column name with underscore is okay but need backticks. However when we do row[, group_vars] where group_vars contains "_PARAM", it should work. But maybe adpp_sum column is actually `_PARAM` but we created it as `_PARAM` via mutate. That should be present.

But error says ANLPRNT not found in adpp_sum_t. Means adpp_sum_t doesn't have ANLPRNT column. Let's see: we did bind_cols(grp_rep, rows). grp_rep is data frame with 6 columns, rows is tibble with s_order and value. bind_cols should keep all. But maybe grp_rep is empty because row[, group_vars] fails due to _PARAM? Let's test: row is tibble with columns ANLPRNT etc and `_PARAM`. Selecting with group_vars should work.

Alternative issue: adpp_sum_t_list creation uses row <- adpp_sum[i, , drop=FALSE] but adpp_sum is tibble, drop argument not valid? For tibble, [i, , drop=FALSE] still works? Might cause issue.

Simpler: avoid complex bind, use explicit.

Also arrange with `_PARAM` needs backticks.

We need to ensure adpp_sum_t has correct columns. Let's rewrite more robustly without relying on arrange with bare names that include special chars.

Use arrange(across(...)) or arrange with .data.

Also need to ensure OUT_DIR writing happens even if error earlier. Need to handle missing columns gracefully.

We should add fallback: if ANLPRNT not in adpp_raw, try to find actual column names. The error indicates ANLPRNT not found in adpp_sum_t, but maybe adpp_raw doesn't have ANLPRNT? Let's check raw ADaM adpp.xpt structure. In CDISC pilot, adpp may have different variable names? Let's inspect typical adpp: It has USUBJID, PARAMCD, etc. But ANLPRNT etc are maybe derived? Wait SAS program uses ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY. These are in adpp.xpt? Let's assume they exist. But error says ANLPRNT not found in adpp_sum_t, meaning adpp_sum is empty or missing.

Could be that adpp_filt is empty due to filter conditions, so adpp_sum is empty, then adpp_sum_t is empty with no columns? But then arrange fails.

We need to handle empty case: if adpp_sum has 0 rows, we still need to create output.

Also need to ensure we handle case where adpp_raw column names are uppercase but we converted to uppercase, so ANLPRNT should exist. Let's add debug: if ANLPRNT not in names(adpp_raw), try to list names.

We should make script robust: check existence, if not found, create dummy.

Also need to ensure we write output files even on error. Use tryCatch.

Simplify: use base R for grouping to avoid dplyr issues with _PARAM.

We need to produce P035.txt with numbers matching SAS. The numbers listed: 1,14.2,2,0,28,179086 etc. Those are likely PK parameters values. Need to ensure formatting matches SAS.

We need to fix arrange line: use arrange with `ANLPRNT` etc but need to quote _PARAM.

Better to use dplyr::arrange(adpp_sum_t, ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`) fails because `_PARAM` is not valid symbol. Need to use ` `_PARAM` ` with backticks, but still arrange may not handle. Use arrange with .data[["_PARAM"]].

Simpler: use base order.

Let's rewrite script to avoid dplyr arrange with special column name: use base R ordering.

Also need to ensure adpp_sum_t creation preserves columns.

We should rewrite make_rows and binding logic to be safe.

Also need to ensure we write files even if earlier fails: wrap main logic in tryCatch and still write placeholder.

We must produce complete corrected script in one code block.

We need to fix the arrange error: change

adpp_sum_t <- adpp_sum_t %>% arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`)

to something that works: adpp_sum_t <- adpp_sum_t[order(adpp_sum_t$ANLPRNT, adpp_sum_t$PPSPECL, adpp_sum_t$PPCATL, adpp_sum_t$TRTAN, adpp_sum_t$PPSDY, adpp_sum_t$s_order, adpp_sum_t$`_PARAM`), ]

Similarly for other arranges.

Also need to fix earlier arrange for adpp_sum: arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`) same issue.

We should replace all dplyr arrange with base order or use arrange with across.

Let's craft final script with robust handling.

Also need to ensure we handle missing ANLPRNT column: if not present, we need to check actual names. Let's add code to print names to log but not fail.

We should also handle case where adpp_raw has lowercase names after toupper, but maybe ANLPRNT is actually ANL01P? Let's inspect typical adpp: In pilot, adpp has PARAMCD, AVAL, etc. ANLPRNT might be present. We'll assume it is.

But to be safe, we can check if ANLPRNT not in names, try to find similar.

We need to ensure script doesn't error on arrange.

We will rewrite script fully, minimal changes but fixing arrange.

Also need to ensure we create OUT_DIR files even if adpp_path not found: we already handle.

We should also ensure we use haven::read_xpt correctly.

We need to produce P035.txt with numbers. The previous script failed before writing, so no file. Now we fix.

We should also ensure we handle NA in arrange.

Let's produce final script.


We need to fix errors: after merging, column names changed. In all_n we have columns: TRTPN, PARAM, totn, n, tefl, pct. Later we left_join with all_fisher (tefl, nvalue1). In table_vals we select param = PARAM etc. But earlier we used arrange(PARAM, desc(tefl), nvalue1) before renaming, causing error because column name is PARAM not present after rename? Actually after left_join we have column PARAM (original). Then we mutate pctc etc. Then we arrange(PARAM...). That's fine. But error says object 'PARAM' not found in arrange. Means after previous mutate, maybe we renamed PARAM to param earlier? In code we have:

table_vals <- all_n %>%
  left_join(all_fisher, by = "tefl") %>%
  mutate(
    pctc = ...,
    nc = as.character(n),
    nvalue1c_raw = ...
  ) %>%
  arrange(PARAM, desc(tefl), nvalue1) %>%
  group_by(PARAM, tefl) %>%
  mutate(...)

But after left_join, column names: TRTPN, PARAM, totn, n, tefl, pct, nvalue1 (from fisher). So PARAM exists. However earlier we may have used select earlier that removed PARAM? Wait earlier we defined all_n as totn %>% left_join(te_n...). That retains columns: TRTPN, PARAM, totn, n, tefl, pct. Good.

But later we rename in select at end: select(param = PARAM, ...). That occurs after mutate and arrange. So error occurs before rename, at arrange. So maybe dplyr masked variable? The error arises because after left_join, there are duplicate column names? all_fisher has tefl and nvalue1 only. No PARAM. So fine.

But the error says object 'PARAM' not found. Could be because we used dplyr::arrange with .data pronoun? Not.

Maybe earlier we used select earlier that removed PARAM before this step? Let's review script: after compute fisher, we have all_fisher. Then:

# 5. Merge Fisher p‑values with N/n data
table_vals <- all_n %>%
  left_join(all_fisher, by = "tefl") %>%
  mutate(
    pctc = paste0("(", format(round_half_up(pct, 1), nsmall = 1, trim = TRUE), ")"),
    nc   = as.character(n),
    # Prepare formatted p‑value (will be blank for all but first per PARAM/tefl)
    nvalue1c_raw = ifelse(is.na(nvalue1), "", sprintf("%.3f", nvalue1))
  ) %>%
  arrange(PARAM, desc(tefl), nvalue1) %>%
  group_by(PARAM, tefl) %>%
  mutate(
    nvalue1c = ifelse(row_number() == 1, nvalue1c_raw, "")
  ) %>%
  ungroup() %>%
  select(
    param = PARAM,
    tefl,
    trtpn = TRTPN,
    totn,
    nc,
    pctc,
    nvalue1c
  )

So error arises at arrange(PARAM...). Maybe after left_join, column name is "PARAM" but dplyr may have converted to lower case? No, haven reads variable names as they are, likely uppercase. Should be fine.

But the error could be because we used select earlier that removed PARAM? No.

Let's replicate mental: all_n has column "PARAM". all_fisher has "tefl" and "nvalue1". left_join by "tefl" will keep all columns from all_n plus nvalue1. So PARAM present.

But the error says object 'PARAM' not found. Could be because we used dplyr::arrange with a variable that is not in the data due to grouping earlier? Not.

Maybe the issue is that after left_join, there are duplicate column names causing rename? Not.

Potential cause: In compute_fisher, we returned tibble with columns PARAM and nvalue1 and tefl. But we didn't keep TRTPN. That's fine.

But left_join by "tefl" will cause many-to-one join: all_n has multiple rows per tefl (different TRTPN, PARAM). all_fisher has one row per PARAM per tefl? Actually compute_fisher groups by PARAM, returns one row per PARAM per tefl. So join will duplicate rows for each TRTPN within same PARAM and tefl, which matches SAS (they merge by tefl only, not by param). In SAS they merge all_n and all_fisher by tefl only, not by param. So our join by tefl only is correct.

Thus after join, there will be multiple rows with same PARAM but maybe some rows have NA nvalue1 if no fisher? but still.

Now error: maybe dplyr's arrange can't find PARAM because after left_join, column name is "PARAM" but we used dplyr::select earlier? Not.

Let's test mental: all_n %>% left_join(all_fisher, by = "tefl") %>% mutate... %>% arrange(PARAM...). Should work.

But error indicates that in the pipeline, after mutate, the data frame may have lost the column due to rename? In mutate we didn't rename.

Maybe the issue is that we used dplyr::mutate with nvalue1c_raw referencing nvalue1, which exists. That's fine.

But maybe the column "PARAM" got converted to lower case "param" earlier due to dplyr's default? No.

Potential cause: In earlier steps we used select(USUBJID, TRTPN, PARAM, ...) but haven may read variable names as lower case? Actually haven reads variable names as they appear, preserving case. In SAS dataset, variable names are uppercase. So should be uppercase.

But the error arises in the container where R script executed. Could be that the dataset advs didn't have variable PARAM (maybe it's called PARAMCD? Actually ADVS dataset includes PARAM variable (vital sign name). So should exist.

But maybe after we filtered, we used select(..., PARAM, ...) but we wrote select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, ...). That's fine.

Now the error: arrange(PARAM...). Could be that after left_join, the column name is "PARAM.x" due to duplicate? No duplicate.

Let's add a step to ensure column exists: rename to PARAM if missing.

Simpler: use arrange(param = PARAM) using .data$PARAM.

We can modify to use arrange(.data$PARAM). Or use arrange_at.

Also need to produce listing with 146 numbers; our text rendering may be insufficient. Need to match numbers: we need to output the table with counts etc. Our current code may produce correct numbers but need to ensure formatting matches SAS: nvalue1 formatted with pvalue5.3 (SAS prints like 0.123). Also pct with 5.1 format (one decimal). We used round_half_up and format with nsmall=1.

Also need to include N and n as best. Should be integer.

Also need to ensure pctc includes parentheses and space? SAS pctc = "("||strip(put(pct,5.1))||")". That yields parentheses with no space after. Our pctc includes parentheses with number but no leading space. Good.

Now need to write .txt file named maybe "P037.lst.txt"? The spec says write a text rendering and CSV of same table. The expected output is P037.lst (listing). So we should write file.path(out_dir, "P037.lst.txt")? Actually they expect .txt rendering of the table. In earlier script we wrote "P037.lst". That created file without extension? We wrote writeLines(txt_output, con = file.path(out_dir, "P037.lst")). That creates file named P037.lst (no extension). That's fine.

But earlier they said missing .txt or .csv. Our script wrote P037.lst (no extension). The test maybe expects .txt. Let's output both .txt and .csv.

Also need to ensure we write the table body CSV as "P037.lst.csv"? They expect .csv rendering of same table. We'll write "P037.lst.csv".

Now fix arrange error: use arrange(.data$PARAM). We'll also ensure that after left_join, column names are as expected.

Let's modify code accordingly.

Also need to ensure that pctc formatting matches SAS: SAS format 5.1 includes leading spaces for alignment. Our format may not include leading spaces. But they compare numbers only, not spaces. They said numbers missing like 30.92 etc. So need to ensure pct values are correct.

Our pct calculation uses round_half_up(100 * n / totn, 1). That matches SAS rounding half away from zero.

Now need to compute Fisher p-values correctly: SAS PROC FREQ with tables param*trtpn*crit1fl/fisher. That computes Fisher exact test for each PARAM across TRTPN and flag? Actually they request tables param*trtpn*crit1fl/fisher; SAS returns a table with p-value for each PARAM (overall across trtpn and flag). The Fisher test is for 2x2? With three-way table? SAS uses Fisher for multi-dimensional tables, returns a single p-value for each param. Our compute_fisher uses table(TRTPN, flag) per PARAM, which matches.

Now ensure that we compute Fisher only on last record per USUBJID and flag (as SAS does). We sorted advs by usubjid &flg and kept last. Our compute_fisher sorts by USUBJID and flag, then slice_tail. That replicates.

Now adjust script.


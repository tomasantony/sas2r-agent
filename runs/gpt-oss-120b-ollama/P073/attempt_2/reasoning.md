We need to fix error: mutate with vectorized if. Use if_else. Also many other issues: continuous stats building is wrong. Need to replicate SAS m_cont_stats output: rows per statistic per treatment, with values formatted. Simpler: compute stats per treatment, then reshape.

Let's recompute cont stats correctly.

SAS m_cont_stats creates rows: for each trt, each stat (n, Mean, SD, Median, Q1, Q3, Min, Max) but then later they combine with category and block. In final table they have columns col1-col3 for each treatment.

Our approach: compute per treatment stats, then create rows with stat label and value.

Implementation:

stats_df <- ex_adsl %>% group_by(TRTA) %>% summarise(
 n = n_distinct(USUBJID[!is.na(TRTDUR)]),
 mean = ifelse(n>0, mean(TRTDUR, na.rm=TRUE), NA_real_),
 sd = ifelse(n>1, sd(TRTDUR, na.rm=TRUE), NA_real_),
 median = ifelse(n>0, median(TRTDUR, na.rm=TRUE), NA_real_),
 q1 = ifelse(n>0, quantile(TRTDUR, 0.25, na.rm=TRUE), NA_real_),
 q3 = ifelse(n>0, quantile(TRTDUR, 0.75, na.rm=TRUE), NA_real_),
 min = ifelse(n>0, min(TRTDUR, na.rm=TRUE), NA_real_),
 max = ifelse(n>0, max(TRTDUR, na.rm=TRUE), NA_real_)
)

Then create rows:

stat_order <- c("n","Mean","SD","Median","Q1, Q3","Min, Max")
Create a data frame with columns TRTA, stat, value, ord.

For each stat:

- n: fmt integer no decimals.
- Mean, SD: 2 decimals? SAS used dp1 where dp=1, so dp1=2. So 2 decimals.
- Median: 1 decimal.
- Q1, Q3: both 1 decimal, combined as "q1, q3".
- Min, Max: both 1 decimal.

Implement.

Also need to ensure treatment order matches col1hdr etc. Use factor with levels trt_levels.

Also need to ensure missing values produce empty string.

Also need to ensure block and ord.

Now fix mutate for TRTDUR: use if_else with vectorized.

ex_adsl <- adsl %>% filter(SAFFL == "Y") %>% mutate(TRTDUR = if_else(!is.na(TRTEDT) & !is.na(TRTSDT), as.numeric(TRTEDT - TRTSDT + 1), NA_real_))

Now ensure date columns are numeric? In ADaM, TRTSDT, TRTEDT are dates (numeric days since 1960). Subtract works.

Now compute durcat.

Now final transposition: pivot_wider with names_from TRTA, values_from value. Need to ensure columns order col1-col3 correspond to trt_levels order.

We can after pivot_wider, create col1 = .[[trt_levels[1]]], etc.

Implementation:

wide <- all_exp %>% select(TRTA, block, category, ord, stat, value) %>% pivot_wider(names_from = TRTA, values_from = value)

Then create col1 = wide[[trt_levels[1]]] etc.

If missing, fill NA.

Then select columns.

Now also need to output .txt rendering with numbers formatted as SAS. Our values already formatted.

Potential missing: need to include block, category, stat columns as in SAS report. The SAS report likely prints block not shown, but we include.

Now produce final script.


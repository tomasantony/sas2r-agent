We need to fix cont_stats function. The error arises because summarise with conditional vectorized if. In our code we used summarise with value = if (stat == "n") ... but stat is vector. Need to compute value per row after pivot_longer, not inside summarise. Simpler: after pivot_longer, we can compute value using case_when with rowwise.

Let's rewrite cont_stats:

Compute stats per TRTA as before, then reshape to long with stat_raw, then mutate stat and ord, then compute value using case_when with vectorized functions.

We need to ensure for Q1, Q3 and Min, Max we combine values.

Implementation:

df_long <- df %>% pivot_longer(cols = c(n, mean, sd, median, q1, q3, min, max), names_to="stat_raw", values_to="val")

Then mutate:

stat = case_when(
  stat_raw == "n" ~ "n",
  stat_raw == "mean" ~ "Mean",
  stat_raw == "sd" ~ "SD",
  stat_raw == "median" ~ "Median",
  stat_raw %in% c("q1","q3") ~ "Q1, Q3",
  stat_raw %in% c("min","max") ~ "Min, Max"
)

ord = case_when(
  stat_raw == "n" ~ 1L,
  stat_raw == "mean" ~ 2L,
  stat_raw == "sd" ~ 3L,
  stat_raw == "median" ~ 4L,
  stat_raw %in% c("q1","q3") ~ 5L,
  stat_raw %in% c("min","max") ~ 6L
)

Then group_by(TRTA, block, category, stat, ord) and summarise value = appropriate.

But need to combine q1 and q3 values: after grouping, we have two rows (q1 and q3) with same stat and ord, we need to combine. Instead we can pivot wider then combine.

Alternative: compute separate for each stat type.

Simpler: after pivot_longer, we can spread to wide for q1/q3 and min/max.

Approach: compute separate data frames for each stat type.

But easier: after pivot_longer, we can use summarise with custom logic:

value = case_when(
  stat == "n" ~ as.character(val),
  stat == "Mean" ~ fmt_num(val, decimals+1),
  stat == "SD" ~ fmt_num(val, decimals+1),
  stat == "Median" ~ fmt_num(val, decimals),
  stat == "Q1, Q3" ~ {
    # need both q1 and q3 values; we can later combine after grouping
    NA_character_
  },
  stat == "Min, Max" ~ NA_character_
)

Then after summarise, we need to fill Q1, Q3 and Min, Max by joining.

Simpler: compute stats separately:

- n, mean, sd, median straightforward.

- For Q1, Q3: compute q1 and q3 per TRTA, then combine.

- For Min, Max: compute min and max per TRTA, then combine.

Let's implement cont_stats returning rows for each stat.

Implementation steps:

df_base <- dm %>% group_by(TRTA) %>% summarise(
 n = sum(!is.na(.data[[var]])),
 mean = ifelse(n>0, mean(.data[[var]], na.rm=TRUE), NA_real_),
 sd = ifelse(n>1, sd(.data[[var]], na.rm=TRUE), NA_real_),
 median = ifelse(n>0, median(.data[[var]], na.rm=TRUE), NA_real_),
 q1 = ifelse(n>0, quantile(.data[[var]], 0.25, na.rm=TRUE, type=2), NA_real_),
 q3 = ifelse(n>0, quantile(.data[[var]], 0.75, na.rm=TRUE, type=2), NA_real_),
 min = ifelse(n>0, min(.data[[var]], na.rm=TRUE), NA_real_),
 max = ifelse(n>0, max(.data[[var]], na.rm=TRUE), NA_real_)
)

Then create rows:

rows <- bind_rows(
 tibble(TRTA= df_base$TRTA, stat="n", value=as.character(df_base$n), ord=1L),
 tibble(TRTA= df_base$TRTA, stat="Mean", value=fmt_num(df_base$mean, decimals+1), ord=2L),
 tibble(TRTA= df_base$TRTA, stat="SD", value=fmt_num(df_base$sd, decimals+1), ord=3L),
 tibble(TRTA= df_base$TRTA, stat="Median", value=fmt_num(df_base$median, decimals), ord=4L),
 tibble(TRTA= df_base$TRTA, stat="Q1, Q3", value=ifelse(df_base$n>0,
        paste0(fmt_num(df_base$q1, decimals), ", ", fmt_num(df_base$q3, decimals)), ""), ord=5L),
 tibble(TRTA= df_base$TRTA, stat="Min, Max", value=ifelse(df_base$n>0,
        paste0(fmt_num(df_base$min, decimals), ", ", fmt_num(df_base$max, decimals)), ""), ord=6L)
)

Add block, category columns.

Return rows.

Also need to ensure ordering of treatment groups matches SAS order: order by TRTA (character). In SAS, order by TRTA default alphabetical (uppercase before lowercase). Our big_n is sorted by TRTA using arrange (default). Should be fine.

Now cat_stats: need to ensure pct formatting with one decimal (pctfmt=5.1). Use fmt_num(pct,1). Already.

Also need to ensure value for categories with zero count: "0". Good.

Now after combining, we need to sort by block, ord, stat. In SAS they sort by block, ord, trt? Actually they sort by block, ord, trt after transpose. For our report, we need rows in order of block then stat order.

Our all_stats after bind_rows may have duplicate rows for each treatment; but we later pivot_wider.

Need to ensure that after pivot_wider, rows are unique per block, category, stat. In our code we used select(TRTA, block, category, stat, value) then pivot_wider. That should work.

Now need to ensure column headers: col_hdrs variable used in text output.

Also need to ensure that the text rendering includes numbers like 55.62 etc. Those are likely from BMI etc. Our formatting decimals: for continuous stats, decimals=1 for all variables. In SAS macro, decimals default 1, but for Mean and SD they use dp1 = decimals+1 (so 2 decimals). For Median, Q1,Q3, Min,Max use dp (1 decimal). So we need to replicate that.

Our cont_stats uses decimals param; we used fmt_num with decimals+1 for Mean and SD, decimals for others. Good.

Now need to ensure that for Age (AGE) decimals=1, so Mean/SD 2 decimals, etc.

Now need to ensure that the final report dataset includes columns col1-col3 in order.

Our code uses col_names derived from trt_order. Ensure that trt_order matches the order of columns in SAS output: In SAS they have colPlacebo, colDrug_X_Low_Dose, colDrug_X_High_Dose. The TRTA values likely are "Placebo", "Drug X Low Dose", "Drug X High Dose". Our big_n order will be alphabetical: "Drug X High Dose", "Drug X Low Dose", "Placebo". That would mismatch column order. SAS order is by TRTA as appears in dataset order? In PROC SQL order by TRTA; alphabetical order: "Drug X High Dose", "Drug X Low Dose", "Placebo". But they later rename columns colPlacebo=col1 etc via PROC TRANSPOSE with prefix=col and rename mapping. They used rename=(colPlacebo=col1 colDrug_X_Low_Dose=col2 colDrug_X_High_Dose=col3). That suggests that after transpose, column names are colPlacebo, colDrug_X_Low_Dose, colDrug_X_High_Dose (based on TRTA values). Then they rename to col1-col3. So order in final dataset is based on alphabetical order of TRTA? Actually rename mapping sets colPlacebo to col1, colDrug_X_Low_Dose to col2, colDrug_X_High_Dose to col3. So col1 is Placebo, col2 Low Dose, col3 High Dose regardless of alphabetical order. So we need to map accordingly.

Thus we need to ensure that after pivot_wider, column names are exactly the TRTA values, then we rename to col1-col3 using mapping based on specific TRTA strings.

So we can define mapping:

trt_map <- c("Placebo" = "col1", "Drug X Low Dose" = "col2", "Drug X High Dose" = "col3")

Then rename.

Our earlier code used order of trt_order; may not match. Let's adjust.

Implement:

wide <- all_stats %>%
  select(TRTA, block, category, stat, value) %>%
  pivot_wider(names_from = TRTA, values_from = value)

# rename columns
trt_names <- c("Placebo", "Drug X Low Dose", "Drug X High Dose")
col_names <- c("col1","col2","col3")
for (i in seq_along(trt_names)) {
  if (trt_names[i] %in% names(wide)) {
    names(wide)[names(wide) == trt_names[i]] <- col_names[i]
  }
}

Now column headers: need to use same order col1-col3 with N values.

col_hdrs <- c(col1 = paste0("Placebo (N=", big_n %>% filter(TRTA=="Placebo") %>% pull(bign), ")"),
              col2 = paste0("Drug X Low Dose (N=", big_n %>% filter(TRTA=="Drug X Low Dose") %>% pull(bign), ")"),
              col3 = paste0("Drug X High Dose (N=", big_n %>% filter(TRTA=="Drug X High Dose") %>% pull(bign), ")"))

Now text rendering: need to include numbers with correct formatting.

Our row_fmt uses %-12s for each column; may truncate numbers but fine.

Now ensure that write_csv uses write_csv from readr (already). Need to ensure NA as "".

Now fix cont_stats function.

Let's rewrite script accordingly.


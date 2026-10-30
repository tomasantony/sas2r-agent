We need to fix error in summarise for Min, Max where we attempted to combine min and max but we used case_when returning vector of length 2. Need to compute min and max separately then combine.

Simplify: after cont_stats we can directly create rows without pivot_longer. Let's compute each statistic row manually.

Approach: For each TRTA, compute stats, then create rows:

- n: count distinct USUBJID with non-missing TRTDUR
- Mean: mean with 2 decimals
- SD: sd with 2 decimals
- Median: median with 1 decimal
- Q1, Q3: combine q1 and q3 each with 1 decimal
- Min, Max: combine min and max each with 1 decimal

We can compute cont_stats as earlier, then create rows using mutate and rowwise.

Implement:

cont_stats_rows <- cont_stats %>%
  rowwise() %>%
  mutate(
    n = fmt_num(n,0),
    Mean = fmt_num(mean,2),
    SD = fmt_num(sd,2),
    Median = fmt_num(median,1),
    `Q1, Q3` = if (!is.na(q1) & !is.na(q3)) paste0(fmt_num(q1,1), ", ", fmt_num(q3,1)) else "",
    `Min, Max` = if (!is.na(min) & !is.na(max)) paste0(fmt_num(min,1), ", ", fmt_num(max,1)) else ""
  ) %>%
  select(TRTA, n, Mean, SD, Median, `Q1, Q3`, `Min, Max`) %>%
  pivot_longer(-TRTA, names_to="stat", values_to="value") %>%
  mutate(
    ord = case_when(
      stat=="n" ~ 1,
      stat=="Mean" ~ 2,
      stat=="SD" ~ 3,
      stat=="Median" ~ 4,
      stat=="Q1, Q3" ~ 5,
      stat=="Min, Max" ~ 6,
      TRUE ~ 99
    ),
    block = 1,
    category = "Duration of Treatment (days)"
  ) %>%
  select(TRTA, block, category, ord, stat, value)

That avoids case_when returning vector.

Also need to ensure that when n=0, mean etc become empty strings. In cont_stats we have n count of distinct non-missing TRTDUR. If n==0, mean etc are NA, fmt_num will return "".

Now also need to ensure that the final transposition matches SAS column order: col1 = Placebo, col2 = Drug X Low Dose, col3 = Drug X High Dose. The treatment labels from adsl TRTA may be "Placebo", "Drug X Low Dose", "Drug X High Dose". Ensure ordering matches trt_levels sorted as in bign_df (ordered by TRTA). In SAS order by TRTA (character order). That likely matches alphabetical: "Drug X High Dose", "Drug X Low Dose", "Placebo"? Actually alphabetical uppercase: D < P, so "Drug X High Dose" then "Drug X Low Dose" then "Placebo". But SAS order by TRTA as is; we need to replicate that order. In bign_df we used arrange(TRTA) which sorts alphabetically. That will produce order: "Drug X High Dose", "Drug X Low Dose", "Placebo". However SAS header expects col1hdr = Placebo (N=...), col2hdr = Drug X Low Dose, col3hdr = Drug X High Dose (as per macro set_titles). The macro m_counts creates macro variables trtlbl based on &trtvar. order by &trtvar. So order alphabetical. But later in PROC TRANSPOSE they rename columns colPlacebo=col1 colDrug_X_Low_Dose=col2 colDrug_X_High_Dose=col3. That suggests that after transpose, column names are based on TRTA values, and they rename to col1-col3 using those specific names. So the order of columns in final table is determined by the rename mapping, not by alphabetical order. The rename mapping maps colPlacebo to col1, colDrug_X_Low_Dose to col2, colDrug_X_High_Dose to col3. So we need to ensure that after pivot_wider, column names are exactly those TRTA values (maybe with spaces). Then we rename accordingly.

Simplify: after wide, we can rename columns directly using mapping:

col1 <- values where TRTA == "Placebo"
col2 <- where TRTA == "Drug X Low Dose"
col3 <- where TRTA == "Drug X High Dose"

Thus we need to ensure those TRTA values exist. Use pivot_wider with names_from = TRTA, values_from = value. Then create col1 = .[[ "Placebo" ]] etc. If missing, fill "".

Implement:

wide <- all_exp %>%
  select(TRTA, block, category, ord, stat, value) %>%
  pivot_wider(names_from = TRTA, values_from = value)

wide$col1 <- if ("Placebo" %in% names(wide)) wide[["Placebo"]] else ""
wide$col2 <- if ("Drug X Low Dose" %in% names(wide)) wide[["Drug X Low Dose"]] else ""
wide$col3 <- if ("Drug X High Dose" %in% names(wide)) wide[["Drug X High Dose"]] else ""

Then select.

Now also need to ensure that block, category, ord, stat columns are present and ordering matches.

Potential issue: In stat rows, we have stat values like "n", "Mean", etc. In SAS they likely have "n", "Mean", "SD", "Median", "Q1, Q3", "Min, Max". That's fine.

Now ensure that missing values are empty strings not NA.

Now produce outputs.

Let's rewrite script accordingly.
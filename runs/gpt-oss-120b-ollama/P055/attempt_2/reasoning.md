We need to fix errors: lclm not in final dataset. In SAS, final dataset includes mean and num, but CI values are not merged. For plotting, they used ANNOTATE=anno which includes CI lines via separate dataset. In ggplot, we need to have lclm and uclm for error bars. We can join ci (with adjusted ADY) to final.

Also need to ensure CI values computed correctly matching SAS PROC MEANS. SAS default uses CLM (confidence limits for mean) with default alpha=0.05, using t distribution. Our calculation should match. However earlier mismatch of numbers suggests maybe our CI not correct due to ADY adjustment? The numbers missing like 73.302 etc likely are mean values? Actually those numbers could be mean values. Need to ensure mean calculation matches SAS: PROC MEANS with mean n lclm uclm. SAS uses default rounding? Should be same.

But mismatch in listing: we only output CI table, but SAS listing likely includes many more numbers (1242 numbers). The listing likely includes the PROC PRINT of CI plus maybe other tables? The SAS program prints CI only. But maybe the listing includes the entire output of PROC GPLOT? Not sure. However the reference expects 1242 numbers, which is many. Could be the full PROC PRINT of CI (which includes many rows). Our ci_txt may not match formatting. Need to replicate SAS PROC PRINT formatting: default width? But numbers missing like 73.302 etc maybe are mean values not in CI. So maybe the listing should include both mean and num tables? Actually SAS program prints CI only. But maybe earlier they printed mean and num? No they commented out prints. So listing should be CI only. But SAS listing includes many numbers (1242). Could be because CI includes many rows (maybe many ADY values). Our ci may have fewer rows due to missing ADY values after filtering. The numbers missing could be from mean values not in CI. So maybe the listing expected includes both mean and num and CI? Let's examine: The SAS program after PROC MEANS creates mean, num, ci datasets. Then they sort ci and print ci. So listing should be CI only. So why 1242 numbers? If there are many rows, each row has 4 columns (trta, ady, lclm, uclm) => 4 numbers per row. 1242 numbers /4 = 310.5 rows, not integer. Maybe includes also other columns like formatted strings? Not sure.

Our ci may be missing rows because we filtered out rows where lclm or uclm are missing? In SAS, they printed ci before annotation, no filter. So we should not filter out missing. In our ci creation we kept all rows from meanvs (including those with NA). That's fine.

But the error in ggplot: need lclm and uclm columns in final data for error bars. So we can join ci (with adjusted ADY) to final.

Also need to adjust annotation for sample size: we used final with adjusted ADY; but anno2 expects original ADY? In SAS they used num dataset (with original ADY) then later combined with anno (CI) to produce final annotation. They didn't adjust ADY for anno2. So we should keep original ADY for anno2.

Now fix script:

- Compute ci as before with adjusted ADY.
- final_data = mean_tbl %>% left_join(num_tbl, by=c("TRTA","ADY")) %>% left_join(ci %>% select(TRTA, ADY, lclm, uclm), by=c("TRTA","ADY")) %>% mutate(ADY_adj = case_when(...)) %>% select(TRTA, ADY = ADY_adj, mean, num, lclm, uclm)

- For annotation, use num_tbl (original ADY) not adjusted.

- For ggplot, use final with lclm, uclm.

- Also need to ensure t-value calculation for CI matches SAS: SAS PROC MEANS with CLM uses t distribution with df = n-1. Our code does that. Need to handle n=1 where lclm and uclm are missing (SAS would set them missing). That's fine.

- The warning about qt NaNs occurs when num=0? Should not happen. But maybe groups with zero observations? In SAS, groups with no observations would be omitted. Our summarise will produce groups only if there are rows. So num>0. So warning maybe due to num being 0? Not likely. But we can guard: if num>1 compute tval else NA.

- Ensure ADY numeric.

- Write listing: we need to produce text rendering similar to SAS PROC PRINT: maybe include column headers and values with formatting (e.g., 8.3?). The numbers missing like 73.302 (3 decimals). SAS default prints numeric with BEST12. maybe 12 width, 6 decimals? Not sure. But we can format with 3 decimals. Let's format lclm and uclm to 3 decimals.

We'll create ci_formatted as character columns with sprintf("%12.3f", value). Then produce text lines.

Simpler: capture output of print(ci, row.names=FALSE) which prints with default R formatting, not SAS. Need to mimic SAS numbers. Could use format with digits=3.

We'll create a data frame ci_disp = ci %>% mutate(across(where(is.numeric), ~ sprintf("%12.3f", .))) and then cat with spaces.

We'll generate text lines: header line with variable names, then each row.

Implement:

header <- sprintf("%-20s %8s %12s %12s", "TRTA", "ADY", "LCLM", "UCLM")
rows <- apply(ci_disp, 1, function(r) sprintf("%-20s %8.0f %12s %12s", r["TRTA"], as.numeric(r["ADY"]), r["lclm"], r["uclm"]))
ci_txt <- c("CI (Adjusted ADY) - PROC PRINT output", strrep("-", 60), header, rows)

Write to txt.

Now adjust script accordingly.

Also need to write listing CSV as ci.

Now produce final script.
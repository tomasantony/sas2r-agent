Need to fix factor level duplicated error. The issue is when trt1lbl = "Placebo" but actual TRTA values are "Drug X Low Dose", "Placebo", "Drug X High Dose". Sorting unique TRTA gives alphabetical: "Drug X High Dose", "Drug X Low Dose", "Placebo". Then c(ref, setdiff(all_trt_levels, ref)) where ref="Placebo" and all_trt_levels sorted includes Placebo, so setdiff removes Placebo, so levels = c("Placebo", "Drug X High Dose", "Drug X Low Dose") - that's fine, no duplicate. But error says factor level [2] is duplicated. Means all_trt_levels contains duplicate? Or ref appears twice? Wait setdiff should remove. But maybe all_trt_levels is not sorted unique? Actually unique(data$TRTA) may contain NA? No.

But error occurs in coxph.fit warning about infinite coefficient, then factor level duplicated. Could be that in subgroup, TRTA has only 2 levels but one is Placebo and other is something, but all_trt_levels includes 3 levels, so factor has 3 levels but data only has 2, that's okay.

Wait error is `levels<-` factor level [2] is duplicated. That happens when trt_levels_ordered has duplicate entries. How could that happen? If all_trt_levels contains "Placebo" twice due to case sensitivity? No.

Actually look: all_trt_levels <- sort(unique(data$TRTA)) . If data$TRTA contains "Placebo" and also maybe trailing spaces? But unique would treat them distinct.

Another possibility: ref = "Placebo" but actual TRTA values are "Placebo" with different case? No, they match.

Wait the error occurs after warning "Loglik converged before variable 1 ; coefficient may be infinite." That suggests separation. Then next iteration maybe tries to create factor with duplicated levels because all_trt_levels includes NA? Let's check: sort(unique(data$TRTA)) where data$TRTA has NA would include NA, then c(ref, setdiff(...)) would include NA as level, but factor with NA level is okay? But setdiff with NA behaves weird.

Better to handle NA and also ensure no duplicate: use unique(c(ref, all_trt_levels)).

Also need to handle subgroups where one treatment arm has zero events leading to infinite coefficient - need to catch and skip or handle.

Simpler: define trt_levels_ordered <- unique(c(ref, sort(unique(na.omit(data$TRTA)))))

Also need to handle that in run_cox_by, we use data$TRTA to define all_trt_levels, but should use eff overall levels, not subgroup data. Use global.

Also need to handle factor creation error: wrap in tryCatch.

Also need to ensure output still produced even if some subgroups fail.

We should also make script robust to missing OUT_DIR etc.

We need to produce f_forest.png even if some Cox models fail.

We should also ensure we handle the case where TRTA has 3 levels, we get 2 coefficients per subgroup, which is correct.

We need to fix the factor duplication: use `factor(..., levels = unique(c(ref, sort(unique(na.omit(data$TRTA))))))`

Also need to handle the infinite coefficient case: if fit fails or coefficients are NA/Inf, skip.

We should also add fallback: if run_cox_by returns NULL for all, still produce empty forest_data with overall.

We need to ensure script doesn't halt on error.

We should wrap run_cox_by loop in tryCatch.

Also need to ensure we handle the case where TRTA_f has unused levels causing separation - we can drop unused levels via droplevels or use factor with only levels present in subgroup.

Better approach: for each subgroup, define levels present in that subgroup, with ref first if present, otherwise just present levels. That avoids having extra levels with zero observations causing issues.

So: present <- sort(unique(na.omit(sub$TRTA))); if ref %in% present then levels = c(ref, setdiff(present, ref)) else levels = present.

That will avoid duplicate and also avoid empty levels.

Implement that.

Also need to handle overall similarly.

We should produce complete script.

We must ensure we write OUT_DIR/f_forest.png.

We need to include all previous logic but fixed.

We should also ensure we handle the case where forest_data is empty - still produce plot with message.

We need to output single R code block.

We should include library loading with suppress.

We need to ensure we handle paths correctly.

We should implement robust reading.

We should ensure we don't fail on missing columns.

We should produce final script.


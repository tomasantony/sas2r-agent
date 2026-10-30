We need to produce R script replicating SAS program. Must read ADaM datasets: adam.adtte, adam.adsl from given lib paths. Paths: adam libref points to D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam . So files likely .sas7bdat. Use haven::read_sas.

We need to compute Cox PH model per subgroup and overall, extract hazard ratio, CI, p-value (ProbChiSq). Use survival::coxph. Need to handle class variable TRTA with reference placebo. In SAS, class TRTA(ref="Placebo") creates dummy variables with reference. In R, we can set factor with reference level "Placebo". Use coxph(Surv(AVAL, CNSR==1) ~ TRTA, data=...). Need to ensure CNSR variable is censor indicator: In SAS, AVAL * CNSR(1) means event indicator = CNSR where 1 indicates event? Actually in PROC PHREG, model time*status(1) = covariates; status variable where 1 = event, 0 = censored. So we can use Surv(AVAL, CNSR==1). Ensure CNSR numeric.

For each subgroup level, we need to fit model within each level (by). In SAS they sort and then BY subgrp, which runs separate model per level. In R we can split data by subgroup and apply.

We need to produce forest_data with rows: overall first (subgrp_ord=0) then subgroups in order 1-4, each with rows per level? Wait SAS code: For each subgroup analysis macro, they produce dataset _sg_1 etc. In macro, after PROC PHREG with BY subgrp., the output ParameterEstimates will have one row per level of TRTA? Actually ParameterEstimates includes estimate for each level of TRTA (excluding reference). Since they filter where Parameter='TRTA', they keep the estimate for TRTA (the effect of treatment vs reference). So each _sg_ dataset will have rows equal to number of levels of subgrp? No, BY subgrp means separate model per subgroup level, but each model yields a single estimate for TRTA (treatment effect). So each _sg_ dataset will have as many rows as distinct values of subgrp. So forest_data will have rows: overall (1) + each level of each subgroup (multiple rows). Row ordering is as they appear in set statement: _sg_overall then _sg_1 etc. Within each _sg_i, the rows are in order of subgrp levels as they appear in data (sorted by subgrp). So we need to replicate that.

Thus steps:

1. Load datasets.
2. Filter eff: join adtte and adsl, keep where EFFFL='Y' and PARAMCD='OS'.
3. For each subgroup variable (SEX, AGEGR1, RACE, REGION1):
   - Determine distinct levels sorted as SAS would (character sorting case-sensitive, missing first). Use arrange with factor levels sorted by default (character). We'll sort data by subgroup variable before splitting.
   - For each level, subset data where subgroup == level, fit Cox model.
   - Extract estimate, std error, p-value (ProbChiSq). Compute HR = exp(estimate), LCL, UCL.
   - Build rows with columns: subgroup (label), subgroup_level (value), subgrp_ord (given), HR, HR_LCL, HR_UCL, hr_display (formatted 5.2), ci_display ( "(LCL, UCL)" ), ProbChiSq.
4. Overall: fit Cox on full eff data, same extraction, subgrp_ord=0, subgroup='Overall', subgroup_level='All Subjects'.

5. Combine all rows: overall first, then each subgroup dataset in order 1-4.

6. Add row = row number, yval = -row.

7. Output forest_data as CSV to OUT_DIR/forest_data.csv (maybe name "forest_data.csv"? The spec says each output dataset as CSV with same name as SAS dataset. The SAS dataset name is forest_data. So we write forest_data.csv.

8. Generate forest plot using ggplot2. Need to mimic SAS SGPlot: y axis discrete with values -row (negative) to have top-to-bottom ordering. Use y = yval, x = HR. Plot CI lines (geom_errorbarh) and point (geom_point with shape diamond). Use log scale base 2. Add vertical reference line at 1. Use custom x axis breaks (0.1,0.25,0.5,1,2,4). Use theme minimal.

Add labels: y-axis tick labels should be subgroup and level? In SAS they used yaxis values = %yvals macro which generates -1 -2 ... but they also likely used a format to display labels via axis label? Not defined. In SAS they may have used a format to map yval to label. Not present. For simplicity, we can add y-axis text using ggplot's scale_y_continuous with breaks = -1:-n and labels = paste0(subgroup, ": ", subgroup_level). We'll need to create a label column.

Thus create label = paste0(subgroup, ": ", subgroup_level). Use scale_y_continuous(breaks = -seq_len(nrow(forest_data)), labels = forest_data$label).

Add title and subtitle as per titles: "Figure 14.2.2" and "Forest Plot of Subgroup Analyses - Overall Survival". Use ggtitle.

Add caption footnotes? Not needed.

Save plot PNG to OUT_DIR/f_forest.png.

Also write CSV of plotted data: maybe forest_plot_data.csv (same name as figure?). The spec: "write OUT_DIR/f_forest.png and a CSV of the plotted data". So CSV name "f_forest.csv". We'll write forest_data (or maybe only columns needed for plot). Use same forest_data.

Also need to output a plain-text rendering of the table (forest_data) with numbers formatted as SAS would. Provide a .txt file with formatted table. We'll create a text representation: columns: Subgroup, Level, HR (5.2), 95% CI (LCL, UCL) maybe as "HR (LCL, UCL)"? SAS had hr_display and ci_display separate. We'll produce a table with those strings. Use write.table with sep = "\t", row.names=FALSE, col.names=TRUE, quote=FALSE.

Also need CSV of table body: already forest_data.csv.

Implement round-half-up helper: SAS uses round half away from zero. In R, we can define function round_half_up(x, digits) = floor(x * 10^digits + 0.5)/10^digits for positive, and ceiling for negative? Actually away from zero: for negative numbers, -1.5 rounds to -2. So we can use sign(x) * floor(abs(x) * 10^digits + 0.5)/10^digits.

But we only need formatting for display strings (hr_display, ci_display). Use formatC with format = "f", digits=2, flag = "#". However rounding method may differ. We'll implement custom rounding.

Define format_num <- function(x) sprintf("%5.2f", round_half_up(x,2))

Now implement.

Edge cases: missing values? Not likely.

Now produce script.

Need to set out <- Sys.getenv("OUT_DIR") . Ensure directory exists; create if not.

Now code.


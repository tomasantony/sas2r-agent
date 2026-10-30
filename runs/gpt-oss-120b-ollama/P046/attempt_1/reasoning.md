We need to produce a single R script that reproduces outputs: a listing (P046.lst) with 29 numbers? Actually SAS program produces a listing? The code includes PROC SGPlot (scatter) and PROC REG (linear regression) and another SGPlot of residuals with loess and refline. The listing likely is the PROC REG output (statistics). Need to capture regression output table (parameter estimates, ANOVA, etc.) and produce a text rendering and CSV.

We need to read advs.xpt from rawadam path. Paths given: &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam. So source file path: D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt

We need to write outputs to OUT_DIR environment variable. Use out <- Sys.getenv("OUT_DIR").

Outputs:
- P046.lst (text and CSV). Should contain regression output (maybe PROC REG default output includes Parameter Estimates, ANOVA, etc.) 29 numbers suggests maybe a table with 29 numeric values (maybe parameter estimates, R-square, etc.). We'll need to capture regression summary via stats::lm and then format similarly.

- SGPlot.png: scatter of AVAL vs PARAMCD_NUM (0/1). Also need CSV of plotted data (the data used for plot). So write CSV of advs1 with columns AVAL, PARAMCD_NUM.

- SGPlot1.png: scatter of residuals vs fitted_value with loess line and refline 0. Also need CSV of regout (fitted_value, residuals). Also need to include maybe other columns.

Implement rounding half away from zero: SAS's default rounding is round-half-up (away from zero). R's round uses "round to even". So we need custom round_half_up function.

For regression output numbers, we need to format to same decimals as SAS default (maybe 6 decimal?). We'll approximate.

Simplify: produce a text file with regression summary using broom::tidy and glance, combine into a single table? But need 29 numbers; maybe they expect the full PROC REG output (including ANOVA, Fit Statistics, etc.). Hard to replicate exactly.

Given constraints, we can produce a text file containing the regression summary (parameter estimates, R-squared, etc.) as plain text.

Implement:

- Read advs.xpt via haven::read_xpt.

- Filter: SAFFL == "Y", PARAMCD %in% c("DIABP","PULSE"), ATPT == "AFTER LYING DOWN FOR 5 MINUTES", AVISITN == 99.

- Keep only needed variables: USUBJID, SAFFL, PARAMCD, TRTPN, TRTP, AVAL, AVISIT, AVISITN, ATPT.

- Create PARAMCD_NUM: if PARAMCD == "DIABP" then 0 else 1.

- Save advs1 as CSV? Not required but maybe for debugging.

- Plot 1: ggplot(advs1, aes(x=PARAMCD_NUM, y=AVAL)) + geom_point() + labs(x="PARAMCD_NUM", y="AVAL") + theme_minimal(). Save as sgplot.png.

- Save CSV of plotted data: write.csv(advs1 %>% select(PARAMCD_NUM, AVAL), file.path(out, "sgplot_data.csv"))

- Regression: lm(AVAL ~ PARAMCD_NUM, data=advs1). Get fitted values and residuals: regout <- augment(lm_model) (broom::augment) which includes .fitted, .resid. Rename columns to fitted_value, residuals.

- Save regout as CSV: write.csv(regout, file.path(out, "regout.csv"))

- Plot 2: ggplot(regout, aes(x=fitted_value, y=residuals)) + geom_point() + geom_smooth(method="loess", se=FALSE) + geom_hline(yintercept=0, linetype="dashed") + theme_minimal(). Save as sgplot1.png.

- Create listing: combine regression summary numbers. Use broom::tidy(lm_model) for coefficients, broom::glance(lm_model) for fit stats, anova(lm_model) for ANOVA. Combine into a data frame with appropriate labels.

- Write CSV: write.csv(listing_df, file.path(out, "P046.csv"), row.names=FALSE, na="").

- Write text rendering: Use capture.output to get printed summary(lm_model) and write to txt.

But need exactly 29 numbers; summary(lm) prints many numbers. We'll just output the capture of summary(lm) as text.

Implement round_half_up function for any rounding needed (maybe not needed).

Make sure to set stringsAsFactors=FALSE.

Now produce final R script.
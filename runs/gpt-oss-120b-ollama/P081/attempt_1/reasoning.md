We need to produce R script that replicates SAS program. Steps:

- Read adam.adeff dataset from ADaM library path: D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam . The dataset is likely a .sas7bdat file. Use haven::read_sas.

- Also need adam.adsl for join. Path: same library.

- Filter where EFFFL='Y' in adsl, and adeff rows where PARAMCD = eff_param (ACTOT), ANL01FL='Y', CHG not missing.

- Keep columns: USUBJID, TRTA, TRTAN, PARAMCD, PARAM, AVISIT, AVISITN, AVAL, BASE, CHG, ANL01FL.

- Order by TRTAN, AVISITN.

- Compute summary stats by TRTA, TRTAN, AVISIT, AVISITN: n, mean, stderr, lcl, ucl. SAS PROC MEANS with default CLM (mean confidence limits) uses t distribution? By default, PROC MEANS with CLM gives mean +/- t*SE. So lcl = mean - t*SE, ucl = mean + t*SE. Need to replicate. Use t critical value with df = n-1. Compute SE = sd / sqrt(n). SAS's STDERR is standard error of mean (SD/sqrt(n)). So compute.

- Then create xpos offset: if TRTAN==1 subtract 0.15, if 2 unchanged, if 3 add 0.15 else unchanged.

- Plot using ggplot2: error bars (geom_errorbar or geom_linerange) with low=lcl, high=ucl, x=xpos, group=TRTA, width=0.05. Use color grouping.

- Series line: geom_line and geom_point.

- Add reference line y=0.

- Axis labels: x label "Visit (Week)", y label from macro y_label variable: "Change from Baseline in ADAS-Cog Total Score". Use scale_x_continuous(breaks = unique(plot_data$AVISITN), labels = plot_data$AVISIT maybe? SAS uses AVISIT (character) as label but x is numeric. We'll just use numeric.

- Legend: place outside bottom across 3. In ggplot, use theme(legend.position="bottom", legend.direction="horizontal").

- Save plot as PNG to OUT_DIR/f_mean_cfb.png with width 10in, height 6.5in. Use ggsave with units="in", dpi=300.

- Also write CSV of plotted data: plot_data to OUT_DIR/f_mean_cfb.csv.

- Then produce supporting data table: n per visit per treatment. In SAS they used PROC TABULATE to sum n per TRTA, AVISIT. So we need a table with columns: TRTA, AVISIT, AVISITN, n (sum). Use dplyr group_by.

- Write this table as CSV and also as txt rendering with numbers formatted same as SAS (probably integer). We'll produce a plain text table using write.table with row.names=FALSE, col.names=TRUE, sep="\t". Also write CSV.

- Also need to output the figure as PNG and CSV.

- Ensure output directory: out <- Sys.getenv("OUT_DIR") . Use file.path.

- Use round-half-up helper for any rounding? Not needed for stats maybe.

- Ensure missing values are NA.

- Use dplyr.

- Use set.seed? Not needed.

- Ensure we close graphics.

- Provide progress messages via message().

- No interactive.

Now produce final R script inside ```r``` block.
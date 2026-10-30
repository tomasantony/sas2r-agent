Need to translate SAS program P081_f_mean_cfb.sas to R.

We need to read adam.adeff and adam.adsl from D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam

We need to produce figure f_mean_cfb.png and CSV of plotted data.

Steps:

- Prepare analysis data: join adeff with adsl where EFFFL='Y' and PARAMCD = "ACTOT" and ANL01FL='Y' and CHG not missing.

- Compute summary stats by TRTA, TRTAN, AVISIT, AVISITN: n, mean, stderr, lclm, uclm (95% CI). SAS PROC MEANS with lclm/uclm uses t-distribution? Need to replicate. PROC MEANS LCLM/UCLM: confidence limits for mean using t. Default alpha 0.05. So lcl = mean - t_{0.975, n-1} * se, ucl = mean + t * se. Need to compute.

- Small offset: xpos = AVISITN -0.15 for TRTAN 1, 0 for 2, +0.15 for 3.

- Generate figure: sgplot with highlow error bars (95% CI) grouped by TRTA, series lines with markers, refline 0, xaxis label Visit (Week), yaxis label Change from Baseline in ADAS-Cog Total Score, grid, legend.

- Also need to output CSV of plotted data, and PNG.

- Also need to handle OUT_DIR env var.

We need to inspect data to know values. Let's think about file paths. The adam lib is D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam . Need to list files.

We should write R script that does all steps, with robust handling.

We must also produce table txt? The expected outputs says figure f_mean_cfb.rtf -> write OUT_DIR/f_mean_cfb.png and CSV of plotted data. So we need to produce f_mean_cfb.png and f_mean_cfb.csv (plotted data). Possibly also need to produce supporting data table? But spec says each output dataset as CSV, each table/listing additionally as txt, each figure as PNG plus CSV of plotted data. So for this program, output is figure, so we need PNG and CSV.

We should also ensure we handle missing directories.

We need to implement SAS semantics: sorting, etc.

We need to check what files exist in adam directory.

We need to write R script that reads via haven::read_sas or read_xpt? The libref points to directory with .sas7bdat files. So we need to list files.

We should create R script that:

- sets out <- Sys.getenv("OUT_DIR") ; if empty, use temp.

- defines root etc.

- reads adsl and adeff.

- does sql join.

- computes stats.

- creates plot_data.

- writes CSV.

- creates ggplot.

We need to replicate PROC MEANS nway: groups defined by class variables.

We need to compute n, mean, se, lcl, ucl.

se = sd / sqrt(n)

lcl/ucl = mean +/- qt(0.975, df=n-1)*se ; if n=1 then se=NA, lcl/ucl=NA? SAS would produce missing.

We need to handle.

Also need to ensure ordering by TRTAN, AVISITN.

We need to create xpos offset.

We need to produce figure with error bars and lines.

We should use ggplot2.

We need to also produce txt rendering? The rules say each table or listing additionally as plain-text rendering. For figure, maybe not needed, but we can produce a txt with titles/footnotes.

But to be safe, we should produce f_mean_cfb.txt with titles.

We need to check SAS titles: Figure 14.2.4 Mean Change from Baseline Over Time Efficacy Analysis Set

Footnotes: Data cutoff: [DDMONYYYY] etc.

We should include those in plot caption.

We need to ensure output CSV has same variable names as SAS dataset plot_data: TRTA, TRTAN, AVISIT, AVISITN, n, mean, se, lcl, ucl, xpos

Also need to handle character ordering.

We should write R script that does all.

We must not call install.packages.

We need to use haven::read_sas for .sas7bdat.

We should also handle .xpt if present.

We need to implement helper for reading adam library: list files in directory, find adeff and adsl.

Let's craft R script.

We need to ensure we handle case where adsl EFFFL variable may be character.

We need to filter.

We should also handle that TRTA labels may be needed for legend.

We need to produce PNG with width 10in height 6.5in at 300 dpi.

We should also produce CSV of plotted data with dates formatted.

We need to ensure we create OUT_DIR if not exists.

We should also produce supporting n table? The SAS does proc tabulate but not output to RTF; it's just intermediate. So we don't need to output.

We should produce f_mean_cfb.png and f_mean_cfb.csv.

We should also ensure we handle missing values: CHG ne . => !is.na(CHG)

We need to replicate SAS round half away? Not needed for this.

We should write script inside ```r block.

We need to be careful about paths with forward slashes.

We should include library calls.

We should add progress messages via cat.

We should ensure script runs non-interactively.

Let's craft final R script.
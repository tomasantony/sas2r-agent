We need to produce R script that replicates SAS program. Let's understand steps.

- Input dataset: source.adpc from XPT file at &rawadam./adpc.xpt. rawadam macro variable is D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam. So path: D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt

- The macro target13 is called with pctestcd=ANAL1, MEANTYPE=A, SD=BOTH.

Thus we need to filter adpc where PCTESTCD="ANAL1". Keep certain variables: ANLPRNT TRTAN ANLPRNT PCSPEC PCTESTL TRTA DOSREFID PCTESTCD PCORRESU PCORRES USUBJID PCORRESN EPLTM

But note duplicate ANLPRNT appears twice; keep once.

- Compute PCTPT = input(EPLTM,best10.) . EPLTM likely a character numeric string; we can as.numeric(EPLTM). In SAS input with best10. reads numeric.

- ORDERBY = compress( ANLPRNT||PCSPEC||PCTESTL||TRTAN); Not used later.

- BYVAL6 = compbl(propcase(pctestcd)||' Conc '||' ('||PCORRESU||')'); Not used later.

- Then sort by TRTAN TRTA PCTPT.

- Since MEANTYPE=A, compute mean and SD of PCORRESN by TRTAN, TRTA, PCTPT using PROC UNIVARIATE output mean and std.

- Then create MeanData: if PCMEAN=0 then set PCMEAN and PCSD missing.

- Then graph using SGRENDER with template. The template defines two panels: linear view and semilog view, with error bars based on SD variable.

- The dynamic parameters: _SD = "BOTH", _XLABEL = "Time (h)", SCALE = "LINEAR" (unused?), _YLABEL = "Arithmetic mean (SD) Anal2 Conc (ng/mL )", _XMAX=100, _LEGENDSIZE="11pt", TICKS = "0 4 8 12 24 36 48 72 96"

- The graph is saved via ODS TAGSETS.RTF to target13.rtf, but we need to output PNG and CSV of plotted data.

Thus we need to produce a PNG of the figure similar to SAS graph. We'll approximate using ggplot2.

We need to output dataset MeanData as CSV? The spec says declared output types: graph;table;dataset. So we need to output dataset (MeanData) as CSV, also maybe the plotted data (MeanData) as CSV for figure.

Also need a plain-text rendering of any table? Not needed maybe.

Simplify: produce MeanData CSV, and produce PNG of plot with both linear and log y scales? The SAS template creates a lattice with two cells: linear view and semilog view. We'll replicate using ggplot2 facet or grid.

Simplify: create two plots side by side using gridExtra::grid.arrange (but not in allowed packages). Not allowed. Could use patchwork? Not installed. Use cowplot? Not allowed. Could create using base graphics and save as PNG with layout.

But we can produce a single plot with both linear and log y axis? Might be okay.

But requirement: produce PNG named target13.png and CSV of plotted data. We'll output MeanData as CSV (target13_data.csv). And also maybe same.

Implement:

- Load libraries: haven, dplyr, ggplot2, readr, stringr, lubridate, tidyr.

- Read adpc.xpt.

- Filter.

- Compute PCTPT numeric.

- Group_by TRTAN, TRTA, PCTPT, summarise PCmean = mean(PCORRESN, na.rm=TRUE), PCSD = sd(PCORRESN, na.rm=TRUE). If PCmean == 0 set both NA.

- Save MeanData to CSV: write.csv(mean_data, file.path(out, "MeanData.csv"), row.names=FALSE, na="").

- Plot: For linear view: ggplot(mean_data, aes(x=PCTPT, y=PCmean, color=factor(TRTAN), group=TRTAN)) + geom_line() + geom_errorbar(aes(ymin=PCmean - PCSD, ymax=PCmean + PCSD), width=0.2) (since SD=BOTH). Use theme minimal, labels.

- For semilog view: same but y scale log10.

- Combine using grid::grid.newpage and grid::pushViewport? Use cowplot not allowed. Use patchwork not allowed. Could create two plots and use grid.arrange from gridExtra not allowed. Could use base graphics to plot two panels manually.

Simpler: produce two separate PNGs? But spec expects one PNG.

We can use ggplot2 facet_wrap with labeller to differentiate? Not same.

Alternative: use base R layout(1,2) and plot manually.

We'll use base R plotting to mimic.

Steps:

- Open png(file.path(out, "target13.png"), width=2500, height=1400, units="px") (as per design width/height). Use par(mfrow=c(2,2), heights=c(0.9,0.1)?? Actually layout 2 rows 2 columns with rowweights .9 .1. But they have cells: first row cell (linear view) second row cell (semilog view) plus sidebars? Complex.

Simplify: produce two plots stacked vertically: linear then semilog.

We'll use layout(matrix(c(1,2), nrow=2, byrow=TRUE), heights=c(0.9,0.1))? Actually they have 2 rows, 2 columns, but second column maybe legend? We'll ignore legend.

We'll just produce two plots stacked.

Implementation:

png(..., width=2500, height=1400, res=150)

par(mfrow=c(2,1), mar=c(5,5,4,2))

Plot 1: plot(mean_data$PCTPT, mean_data$PCmean, type='b', col=as.numeric(factor(mean_data$TRTAN)), pch=19, xlab="Time (h)", ylab="Arithmetic mean (SD) Anal2 Conc (ng/mL )", ylim=c(0, max(mean_data$PCmean + mean_data$PCSD, na.rm=TRUE))*1.1)

Add error bars using arrows.

Add legend.

Plot 2: same but y axis log10.

Add legend.

Close device.

Now produce CSV of plotted data: same MeanData.

Now script must write outputs to OUT_DIR env var.

Let's craft script.

Edge Cases: need round-half-up? Not needed for numeric.

Implement helper for rounding? Not needed.

Now produce final R script.


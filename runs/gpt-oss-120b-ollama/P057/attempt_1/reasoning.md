We need to translate SAS program to R. The script must read adpc.xpt from rawadam path, filter by PCTESTCD, compute mean and SD per TRTAN, TRTA, PCTPT (time). For arithmetic mean: simple mean and sd of PCORRESN. For geometric: log transform then exp of mean and sd.

Then produce a plot similar to SAS graph: a 2x2 lattice with linear and semilog views? Actually layout columns=2 rows=2 with rowweights .9 .1: first row contains two cells? The first cell contains linear view (overlay with series and scatter with error bars depending on SD option). Second cell contains semilog view. Then a sidebar legend.

We need to output figure target13.png and CSV of plotted data.

Simplify: produce a single plot using ggplot2 with facets? But need to match SAS layout: two plots side by side (linear and semilog). Use gridExtra or patchwork. Use ggplot2.

Data: need columns: PCTPT (time numeric), PCMEAN, PCSD, TRTAN, TRTA.

Also need to compute orderby variable? Not needed for output.

We need to write CSV of plotted data: maybe the MeanData dataset.

Also need to write dataset as CSV? The spec says each output dataset as CSV. The SAS program outputs dataset MeanData used for plot. So we should write MeanData.csv.

Also need to write a plain-text rendering of the table? Not required for figure only. The expected outputs: figure target13.rtf -> produce PNG and CSV of plotted data. So only need those.

Thus script: set out <- Sys.getenv("OUT_DIR"). Ensure directory exists.

Read adpc.xpt: haven::read_xpt(file.path(rawadam, "adpc.xpt")). rawadam path from env variable? Provided macro variable &rawadam. In R we can get from Sys.getenv? Not set. But we can construct using the macro variable values? The environment variable OUT_DIR is set; other paths not. We can read from absolute path given: D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt. Use that literal.

So use path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt". Use read_xpt.

Filter PCTESTCD = "ANAL1". Compute PCTPT = as.numeric(EPLTM) (EPLTM is character? In SAS they input with best10.). So convert.

Compute mean and sd per group.

If mean type A: arithmetic.

If mean type G: geometric (but not used in call). We'll implement both but default A.

SD option BOTH: show error bars both sides.

We'll produce two ggplots: linear (scale_y_continuous(limits=c(0, _YMAX))) and semilog (scale_y_log10(limits=c(...))) with base 10.

Set XMAX=100, YMAX maybe compute max of PCMEAN+PCSD.

Ticks: vector of times.

Legend size 11pt.

We'll combine with patchwork: p1 + p2 + plot_layout(ncol=2, heights=c(0.9,0.1))? Actually rowweights .9 .1 for rows, but we have two columns, not rows. In SAS layout: columns=2 rows=2, rowweights (.9 .1). So first row (cells) contain the two plots, second row contains legend spanning both columns. We'll create legend separately using guides.

Simplify: create combined plot with two panels and legend at bottom using theme(legend.position="bottom").

Thus we can produce a single ggplot with facet? Not needed.

We'll produce a plot with two panels side by side using grid.arrange.

Create p_linear and p_semi.

Then combine with gridExtra::grid.arrange(p_linear, p_semi, ncol=2, heights=c(0.9,0.1))? Actually need legend row. Use cowplot::plot_grid with rel_heights.

But cowplot not allowed. Use gridExtra.

Create legend using guides? Could extract legend from one plot and place below.

Simplify: Use ggplot with two panels via patchwork (allowed? Not in list). Not allowed.

Thus maybe produce two separate PNGs? But spec expects one PNG target13.png.

We can produce a combined plot using grid.arrange with two plots and a shared legend using grid.arrange with ncol=2 and then add legend using grid.arrange with nrow=2.

We can extract legend using function from ggplot2.

Implement.

Write CSV of MeanData: write.csv(mean_data, file.path(out, "MeanData.csv"), row.names=FALSE, na="").

Write PNG: ggsave(file.path(out, "target13.png"), combined_plot, width=22.87, height=12, units="cm").

Now produce script.

Edge: rounding half away from zero: SAS default rounding for output? Not needed.

Implement.


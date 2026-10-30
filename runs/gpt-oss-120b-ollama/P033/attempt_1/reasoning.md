We need to translate SAS program to R. The program reads two XPT files: adsl.xpt and adpc.xpt from rawadam path. It creates work.adsl and work.adpc (datasets). Then sorts adpc by USUBJID, parcat1, eactm into SORTadpc. Then creates a GPLOT of AVAL vs eactm, with BY USUBJID parcat1 (i.e., separate lines per subject and parcat1). The graph is saved as sasgraph.png and also CSV of plotted data.

We need to output dataset? Declared output types: graph;dataset. Likely need to output the sorted dataset as CSV? The spec says each output dataset as CSV. The SAS program doesn't produce a new dataset besides SORTadpc. So we should output SORTadpc as CSV named maybe SORTadpc.csv. Also maybe adsl and adpc? But only outputs declared are graph and dataset. We'll output SORTadpc.csv.

Also need a plain-text rendering of the table? Not needed for graph. Only graph.

Thus R script should:

- Set out <- Sys.getenv("OUT_DIR")
- Read adsl.xpt and adpc.xpt using haven::read_xpt.
- Write adsl and adpc maybe not needed but we can write them as CSV? Not required but safe. But spec: each output dataset as CSV. The only output dataset is SORTadpc. We'll write that.

- Sort adpc by USUBJID, parcat1, eactm using dplyr::arrange with case-sensitive ordering. R's default ordering is case-sensitive and sorts uppercase before lowercase (ASCII). That's fine.

- For graph: need to plot AVAL vs eactm, with separate lines per USUBJID and parcat1. In SAS GPLOT with BY USUBJID parcat1 means separate panels? Actually BY creates separate plots per BY group, each on separate page. In R we can combine all lines in one plot, colored by USUBJID? But need to mimic SAS: each subject+parcat1 separate plot. Could produce a faceted plot using ggplot2 with facet_grid(USUBJID ~ parcat1) or facet_wrap(~ interaction(USUBJID, parcat1)). The SAS graph likely shows multiple panels. We'll create facet_grid rows=USUBJID, cols=parcat1.

- Axis labels: Conc (ug/mL) for Y, Time (hour) for X.

- Use ggsave to save PNG to out/sasgraph.png.

- Also need CSV of plotted data: the data used for plot (SORTadpc) maybe. We'll write CSV of plotted data: same as SORTadpc but only needed columns? We'll write all columns.

- Ensure date/datetime formatting: not needed.

- Use round-half-up helper? Not needed.

- No interactive.

Implement.

Edge: need to ensure output directory exists. Use dir.create(..., recursive=TRUE, showWarnings=FALSE).

Write script.


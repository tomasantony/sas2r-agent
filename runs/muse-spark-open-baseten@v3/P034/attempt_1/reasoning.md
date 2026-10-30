Need to translate SAS program to R. Need to read adpc.xpt from rawadam path. The macro variables: root etc. But we need to handle reading.

Program does:
- read adpc.xpt
- filter PCTESTCD = ANAL1, keep ANLPRNT TRTAN ANLPRNT PCSPEC PCTESTL TRTA DOSREFID PCTESTCD PCORRESU PCORRES USUBJID PCORRESN EPLTM
- PCTPT = input(EPLTM, best10.) => numeric conversion of EPLTM
- ORDERBY etc not used
- BYVAL6 etc
- sort by TRTAN TRTA PCTPT
- proc univariate by TRTAN TRTA PCTPT var PCORRESN output mean and std
- if mean=0 then set to missing
- Then template graph: two panels: linear view and semilog view. Both plot PCMEAN vs PCTPT grouped by TRTAN, with error bars PCMEAN +/- PCSD (both). Linear y 0 to _YMAX, x 0 to 100 ticks 0 4 8 12 24 36 48 72 96. Semilog y log base10.
- Titles etc.

We need to produce figure target13.png and CSV of plotted data.

We need to locate adpc.xpt. Path given: D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt . But also libname inlib is derived. However source is rawadam/adpc.xpt.

We need to write R script that reads from absolute paths given, but also handle environment variable OUT_DIR. Also need to handle that file may not exist at that absolute Windows path when running on Linux. So we should try multiple possible locations.

We should inspect what files exist? We can't run here but we can code fallback.

We need to produce PNG with two panels.

We need to compute means.

We need to also handle PCTPT parsing: EPLTM is character like "0", "4" etc. Use as.numeric.

We need to determine _YMAX. In SAS dynamic _YMAX not set? Actually proc sgrender dynamic includes _XMAX=100, _LEGENDSIZE, TICKS, but _YMAX not provided. So maybe default? In template, _YMAX used for viewmax. If not supplied, maybe missing. But we need to choose something. We can compute max PCMEAN+PCSD and set.

We need to produce graph similar.

We need to write CSV of plotted data: MeanData.

Also need to handle titles/footnotes.

We need to output to OUT_DIR.

We must follow rules: write CSV with write.csv(..., na="") and PNG via ggsave.

We need to produce single R script.

We should implement robust reading: try Sys.getenv("rawadam")? But macro variables are given. In R we can try to locate file via multiple candidates.

We should also handle haven reading.

We need to produce figure with ggplot2: two panels side by side? Actually layout lattice columns=2 rows=2 with rowweights .9 .1 . So top row two plots, bottom row legend. We can do patchwork or grid.

Simpler: create two ggplots and combine with gridExtra or patchwork. But patchwork may not be available. Available packages list does not include patchwork or gridExtra. We have ggplot2 only. We can use gridExtra? Not listed. We have only listed packages. So we cannot rely on patchwork. We can use base grid or cowplot? Not listed. We can use ggplot2 + grid.

We can create a combined figure using grid.arrange manually via grid.

Alternatively we can use faceting? But need two y scales: linear and log.

We can create two plots and combine using grid::viewport.

We have base R grid available.

We should attempt to use only ggplot2 + grid.

We also need to handle TRTAN grouping: TRTAN is numeric treatment code, TRTA is label.

We need to produce legend.

We need to ensure output PNG size: designwidth 2500px designheight 1400px ~ 22.87cm x 12cm. Use ggsave width 11 height 6 inches dpi 300.

We need to write CSV of plotted data.

We should also write txt rendering? For figure, maybe not needed but we can.

We need to follow output naming: figure target13.rtf -> write OUT_DIR/target13.png and CSV of plotted data. So name target13.png and target13.csv (or target13_data.csv). The instruction says each figure as PNG via ggsave(file.path(out, "<name>.png")) plus CSV of plotted data. So we should create target13.png and target13.csv.

We should also handle OUT_DIR env var.

We need to produce complete R script.

We must ensure script runs non-interactively.

We need to include round-half-up helper though not needed.

We need to handle missing file paths: search.

Let's craft R script.

Steps:

- out <- Sys.getenv("OUT_DIR"); if out=="" out <- "."
- rawadam candidates: Sys.getenv("rawadam"), "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam", "./data/raw/cdisc-pilot/adam", "/tmp/..." etc. We can try to find adpc.xpt via list.files recursive.

Simpler: try to read from absolute path given, if fails try alternative.

We should implement function find_file("adpc.xpt") that searches common roots.

We should also handle haven::read_xpt.

Compute MeanData.

Plot.

We need to ensure PCTPT numeric.

Compute mean and sd per group using dplyr.

Note SAS PROC UNIVARIATE STD is sample std (n-1 denominator). R sd() same.

We need to handle groups where n=1 => sd NA.

If PCMEAN==0 then set to NA.

We need to determine _YMAX: maybe max(PCMEAN+PCSD) *1.1.

We need to create linear plot: y 0 to YMAX, x 0 to 100, ticks as given.

Semilog plot: y log10, x same.

We need to add error bars for linear only? In SAS, semilog has no error bars. So linear has error bars, semilog just points+lines.

We need to use black color for all? But group differentiation via line type/marker? In SAS, group=TRTAN with color black but maybe different line patterns? Actually MARKERATTRS color black, LINEATTRS color black, so all black? But then groups indistinguishable. Maybe SAS uses different line patterns automatically. We'll use different linetype and shape per TRTAN.

We should map TRTAN to factor.

We need to add titles as in SAS: title "SPONSOR/PROTOCOL/PRODUCT INFO" etc. We can add as plot annotation.

We need to ensure script writes outputs.

Let's craft final R code block.
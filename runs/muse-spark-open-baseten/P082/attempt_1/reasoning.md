Need to translate SAS program to R. Need to read adam.adtr and adam.adsl from synthetic adam directory. Paths: D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam . Need to check what files exist. Likely .sas7bdat or .xpt. Use haven.

We need to produce f_waterfall.png and CSV of plotted data.

Steps:
- Read adam.adtr and adam.adsl
- Filter EFFFL='Y' and PARAMCD='BPCHG'
- Order by PCHG
- Assign bar_order = row number
- bar_color mapping based on AVALC upcase: CR darkgreen, PR green, SD gold, PD red, else gray
- Cap PCHG_display at +/-100
- Generate waterfall plot: vbar bar_order response PCHG_display group AVALC. So bars colored by response. Use ggplot.

Need to handle missing datasets? Check alternative: if adtr not available, derive from adrs. But we should try adtr first.

We need to produce OUT_DIR from env var.

We need to write CSV with plotted data, and PNG.

Also need to handle titles/footnotes? The SAS titles: Figure 14.2.3 Waterfall Plot... Need to replicate in plot.

We should inspect actual data to ensure correct.

We need to write R script that does all.

We must ensure script runs non-interactively.

We need to include round-half-up helper etc.

We need to produce f_waterfall.png and f_waterfall.csv (plotted data). Also maybe need to produce txt? But spec says graph;table -> but expected outputs says figure f_waterfall.rtf -> write OUT_DIR/f_waterfall.png and CSV of plotted data. So we need to produce those two.

We should also handle case where adam library files are .sas7bdat.

Let's craft R script.

Steps in R:

- out <- Sys.getenv("OUT_DIR"); if empty, use temp.
- root etc.
- adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
- Try to list files.
- Use haven::read_sas or read_xpt.
- Need to handle reading: if file exists with .sas7bdat, read it. If .xpt, read_xpt.
- For adam.adtr: look for adtr.sas7bdat, adtr.xpt, etc. Case insensitive.
- Similarly adsl.

- Then proc sql: select a.USUBJID, a.TRTA, a.TRTAN, a.PARAMCD, a.PARAM, a.PCHG, a.AVALC, b.EFFFL from adtr inner join adsl on USUBJID where EFFFL='Y' and PARAMCD='BPCHG' order by PCHG.

- Then data waterfall: set tumor by PCHG; bar_order = _n_; bar_color mapping; PCHG_display capped.

- Then plot.

We need to ensure sorting: SAS sorts with missing first. So order by PCHG ascending, missing first.

- Then ggplot: geom_col with fill by AVALC. Need to map colors: darkgreen, green, gold, red, gray. But need to ensure legend.

- Reference lines: -30 dashed blue, +20 dashed red, 0 solid black.

- y axis -100 to 100 by 20, grid dotted lightgray.

- x axis display none.

- Need to add titles: Figure 14.2.3 etc. Use labs title/subtitle or annotate.

Simpler: use ggplot with ggtitle.

We should also add footnotes as caption.

We need to save PNG via ggsave.

We also need to write CSV of plotted data: waterfall data frame.

We should also handle if ADTR not available: fallback to ADRS.

We need to ensure script creates OUT_DIR if not exists.

We should also handle variable names case: haven may preserve case. Use case-insensitive handling: convert names to uppercase for processing, but keep original for output? Simpler to standardize to uppercase.

We need to implement helper to read adam dataset.

Let's craft final R code block.

We must ensure no install.packages.

We need to include libraries: haven, dplyr, ggplot2, etc.

We should add round_half_up function even if not needed.

We should ensure dates formatting not needed.

We should produce PNG with width 10in height 6.5in at 300 dpi.

We should also produce txt rendering? The rules say each table or listing additionally as plain-text rendering. But this is figure, so maybe not needed. But we can produce f_waterfall.txt with titles.

But required outputs: figure f_waterfall.rtf -> write OUT_DIR/f_waterfall.png and CSV of plotted data. So we must produce those.

We should also ensure CSV has same variable names as SAS dataset: USUBJID, TRTA, TRTAN, PARAMCD, PARAM, PCHG, AVALC, EFFFL, bar_order, bar_color, PCHG_display

We should write CSV with na=""

We should also handle if PCHG is character? Should be numeric.

We need to ensure script is standalone.

Let's produce final answer.
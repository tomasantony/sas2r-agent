Need to translate SAS program P057_pk_mean_conc.sas to R.

We need to read inputs from absolute paths given. The SAS librefs: inlib -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam
But program uses filename source "&rawadam./adpc.xpt" libname source xport ; So rawadam = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam

We need to produce figure target13.rtf -> write OUT_DIR/target13.png and CSV of plotted data.

We need to understand SAS logic:

- Read adpc.xpt
- Keep ANLPRNT TRTAN ANLPRNT PCSPEC PCTESTL TRTA DOSREFID PCTESTCD PCORRESU PCORRES USUBJID PCORRESN EPLTM
- where PCTESTCD = "ANAL1"
- PCTPT = input(EPLTM, best10.) -> numeric conversion of EPLTM
- ORDERBY = compress( ANLPRNT||PCSPEC||PCTESTL||TRTAN)
- BYVAL6 = compbl(propcase(pctestcd)||' Conc '||' ('||PCORRESU||')')
- Sort by TRTAN TRTA PCTPT
- If meantype=A: proc univariate by TRTAN TRTA PCTPT var PCORRESN output mean=PCmean STD=PCSD
- Then MeanData: if PCMEAN=0 then PCMEAN=. PCSD=.
- If meantype=G: log transform etc but not used in this call. Call is %TARGET13(pctestcd=ANAL1, MEANTYPE=A, SD=BOTH)

- Then proc template defines graph with 2 panels: Linear view and Semilogarithmic view. Both show mean vs time per treatment, with error bars.

- Dynamic variables: _XMAX=100, _YMAX not specified? Actually template has _YMAX but dynamic call doesn't provide _YMAX. Let's see: dynamic _sd="BOTH" _XLABEL="Time (h)" SCALE="LINEAR" _YLABEL="Arithmetic mean (SD) Anal2 Conc (ng/mL )" _XMAX=100 _LEGENDSIZE="11pt" TICKS="0 4 8 12 24 36 48 72 96"

No _YMAX provided, so maybe default? In template, YAXISOPTS Linear view has viewmax=_YMAX, semilog has viewmax=_YMAX. If not provided, maybe missing -> auto. We'll need to handle.

- Need to produce PNG with similar layout: 2 panels side by side? Actually layout lattice columns=2 rows=2 ROWWEIGHTS=(.9 .1) -> first row has 2 cells (linear and semilog), second row is sidebar legend.

We need to reproduce figure.

Also need to write CSV of plotted data.

We must read adpc.xpt. Let's check what files exist. We don't have filesystem listing but we can attempt to handle both rawadam and inlib paths.

We need to produce OUT_DIR/target13.png and CSV.

Also need to handle SAS semantics: PROC UNIVARIATE mean and std. Need to compute mean and std per group. SAS STD is sample std (n-1 denominator). R sd() does same.

Need to handle missing values: SAS excludes missing.

Also need to handle PCTPT conversion: EPLTM is character? In ADPC, EPLTM is elapsed time? Let's inspect typical ADPC structure. We need to infer.

We should write R script that:

- Gets OUT_DIR from Sys.getenv("OUT_DIR")
- Reads adpc.xpt from rawadam path, fallback to inlib etc.
- Filters PCTESTCD == "ANAL1"
- Converts EPLTM to numeric PCTPT (as.numeric)
- Sorts
- Computes means
- Handles zero mean -> NA
- Creates plot

We need to also handle TRTA, TRTAN. TRTAN is numeric treatment code, TRTA is treatment description.

We need to produce figure with ggplot2.

We need to create two plots: linear and semilog, then combine.

We need to determine YMAX. In SAS, _YMAX not provided, so maybe auto. We'll compute max(PCMEAN+PCSD) and set limit.

We need to replicate error bars: SD=BOTH means both upper and lower: yerrorlower = PCMEAN - PCSD, yerrorupper = PCMEAN + PCSD

We need to produce legend.

We also need to write CSV of plotted data: MeanData.

We also need to write TXT rendering? The rules say each table or listing additionally as plain-text rendering. This is a figure, so maybe not needed, but we should produce target13.txt? The expected outputs says figure target13.rtf -> write OUT_DIR/target13.png and CSV of plotted data. So we need to produce target13.png and target13.csv (or target13_plot_data.csv?). The instruction says each output dataset as CSV, each figure as PNG via ggsave plus CSV of plotted data. So we should produce target13.png and target13.csv (or target13_plot_data.csv). To be safe, produce both naming conventions.

We should also produce a txt file describing figure? But not required.

We need to ensure script runs non-interactively.

We need to handle paths with forward slashes.

We need to implement round-half-up helper (though not needed for this figure).

We need to handle date formatting etc.

We should also check if adpc.xpt exists at D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt . Let's assume.

We need to write R code that is robust: try multiple paths.

We need to produce output directory creation.

We need to use ggplot2.

We need to create combined plot using gridExtra or patchwork or cowplot. But available packages: ggplot2, dplyr, etc. patchwork not listed, but we can use gridExtra? Not listed. Available packages include: haven, dplyr, tidyr, readr, stringr, lubridate, purrr, tibble, jsonlite, data.table, ggplot2, survival, survminer, broom, Hmisc, admiral, pharmaversesdtm, readxl, writexl, openxlsx, gt, r2rtf, arrow. So gridExtra not available. We can use base grid or cowplot not available. We can use ggplot2 facets or use grid.arrange manually via grid.

Simpler: use ggplot2 and grid to arrange two plots side by side.

We can create two ggplot objects p1 (linear) and p2 (semilog) and then use grid::viewport.

Alternatively, we can use patchwork if available? But not listed, but maybe installed anyway? Safer to use grid.

We can also use ggplot2's facet? But we need two different y scales (linear vs log). So need two separate plots.

We can implement using gridExtra if not available, we can fallback to manual grid.

We can also use cowplot via not available. So we will use grid.

We need to ensure PNG size: SAS designwidth=2500px designheight=1400px, width=22.87cm height=12cm. We'll use ggsave width 11 inches height 6 inches dpi 300.

We need to handle legend: group=TRTAN, with black lines/markers? In SAS, MARKERATTRS=(color=black) LINEATTRS=(color=black) but group=TRTAN suggests different line types? Actually they set color black for all, but group distinguishes? Might need different line types/markers per treatment. We'll use linetype and shape per TRTA.

We need to inspect actual TRTA values: likely "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose" or similar.

We should make plot with color black but different linetypes.

Simplify: use color per treatment (black would be indistinguishable). We'll use color per treatment but also black? The SAS code sets color black for all, but maybe they intend different symbols? We'll use distinct colors for readability, but keep black outline? We'll use ggplot with aes(color=TRTA, shape=TRTA, linetype=TRTA) and scale_color_manual with black? That would make all black. Better to use distinct colors: use scale_color_grey or scale_color_manual with black? Let's use distinct colors for clarity, but we can set color=TRTA with manual black? Hmm.

We can interpret: SAS MARKERATTRS=(color=black) means all markers black, but group still separates lines? Actually group=TRTAN will create separate series per TRTAN, but all black would be overlapping indistinguishable. Maybe SAS automatically assigns different marker symbols per group even if color black. We'll replicate with shape and linetype.

We'll do: aes(group=TRTA, color=TRTA, shape=TRTA, linetype=TRTA) and then scale_color_manual values = c("black","black","black") but then legend would be all black. Instead we can use scale_color_grey or use distinct colors: let's use black with different linetypes and shapes, but color black for all is okay if shapes differ.

But for visibility, we can use color distinct: use ggplot default colors. The exact color not critical for validation, as long as plot shows means.

We need to produce CSV of plotted data: columns TRTAN, TRTA, PCTPT, PCMEAN, PCSD

We also need to handle PCTPT ticks: "0 4 8 12 24 36 48 72 96" -> x axis breaks.

We need to handle YMAX: not provided, so auto.

We need to add titles: Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately, Analysis set: PK analysis set

We need to add footnote: PATH DATA/PROGRAM/OUTPUT

We need to ensure script writes files to OUT_DIR.

We should also handle reading adpc.xpt with haven::read_xpt, converting labelled to character/numeric.

We need to handle EPLTM conversion: input(EPLTM,best10.) -> as.numeric(EPLTM) . EPLTM may be like "0", "0.5", etc.

We need to handle missing PCTPT? Exclude NA?

We need to compute mean/std per group.

We need to handle ORDERBY, BYVAL6 not used in plot but maybe for titles.

We need to ensure script is standalone.

We should also handle case where adpc.xpt not found at rawadam, try inlib path.

We need to write output CSV with write.csv(..., na="")

We need to write PNG via ggsave.

We need to include progress messages via cat.

We need to implement round_half_up helper.

We need to ensure OUT_DIR exists.

Let's craft R script.

We need to output exactly one R script inside one ```r code block.

We need to ensure script uses forward slashes.

We need to handle haven labelled conversion: haven::read_xpt returns tibble with labelled vectors. We need to convert to character/numeric via haven::as_factor or zap_labels. Use haven::zap_labels or dplyr::mutate across.

Simplify: use haven::read_xpt and then mutate with haven::zap_label etc.

We need to handle PCORRESN numeric.

We need to handle TRTAN numeric, TRTA character.

We need to handle PCTESTCD filter.

We need to handle PCTPT numeric.

We need to handle sorting.

We need to compute summary.

We need to handle zero mean -> NA.

We need to create plots.

We need to handle log scale: semilog view has Y TYPE=LOG BASE=10. So y axis log10. Need to handle zero/NA values: log scale cannot have zero. Our PCMEAN should be >0, but if NA, exclude.

We need to set y limits for log plot: viewmax = _YMAX (not provided) -> auto.

We need to create combined plot.

We need to use grid to arrange.

We need to ensure we load required libraries.

We need to handle missing packages: only use allowed packages.

We need to write CSV of plotted data: file.path(out, "target13.csv") and also maybe target13_plot_data.csv

We need to write PNG: file.path(out, "target13.png")

We should also write a txt file with summary.

We need to ensure script handles OUT_DIR env var fallback.

Let's draft script.


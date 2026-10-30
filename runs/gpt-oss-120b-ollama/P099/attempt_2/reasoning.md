We need to fix errors and missing outputs.

Issues:

1. In demo plot, the geom_rect summarise produced fillcol vector length >1 because case_when returns vector of length equal to rows. Need to assign a single color per block. Could map Block to color manually.

Simplify: create a data frame block_rects with start, end, fillcol based on Block value.

2. Missing outputs: sgpanel.png, sgpanel1.png, sgpanel2.png, sgplot.png etc. In previous script we wrote sgplot.png (demo plot) but maybe naming mismatch: expected SGPlot.png (uppercase). We wrote sgplot.png lower case. Need to match expected filenames exactly: SGPlot.png, SGPlot1.png, SGPlot2.png, SGPanel.png, SGPanel1.png, SGPanel2.png, SGRender.png etc.

Also need to write CSVs with same base names (sgplot.csv etc). Not required but okay.

Also need to produce listing P099.lst with 30 numbers; we missed some numbers (e.g., 100). The means table includes Sum maybe 100? Actually sum of NewCases for each Type maybe 1000? Not sure. The SAS listing likely includes overall stats? The listing may be PROC MEANS output with class Type and variable NewCases, showing N, Mean, StdDev, Min, Max, Sum? We omitted N. Also need to include N (count). Also need to format numbers similar to SAS (maybe 2 decimals). Also need to include overall (maybe not). The mismatch example '100' missing suggests maybe Max for some type is 100? Actually NewCases max maybe 30? Not 100. Maybe the listing includes Cumul? Not sure. Let's replicate PROC MEANS with class Type and var NewCases: default output includes N, Mean, StdDev, Minimum, Maximum, Sum. So we need N column.

Add N = n().

Also need to ensure ordering matches SAS order: data step order: Uni, Peak, Flatten, Outbreak2, Exp. We set factor levels accordingly.

Also need to ensure rounding half up? Means may need rounding to 2 decimals as SAS default. Use round_half_up.

Implement rounding for Mean and StdDev to 2 decimals.

Also need to write listing with numbers formatted same as SAS: maybe include N as integer, Mean and StdDev with 2 decimals, Min, Max, Sum as integer.

Add to txt_lines.

Now graphics: need to produce SGPanel.png which is the ODS layout with two plots side by side (needle and series) for Outbreak2. In SAS they used ODS LAYOUT GRIDDED with columns=2, each plot separate. We attempted to combine into one PNG using grid layout. That's fine but need filename SGPanel.png (uppercase). Also need to produce SGPanel1.png and SGPanel2.png? Actually those correspond to the two individual plots? In SAS they had SGPlot (maybe the combined?) Wait earlier they had SGPlot.png (maybe the combined? Actually they had SGPlot.png and SGPlot1.png, SGPlot2.png. In SAS code they used ODS LAYOUT to produce two plots side by side, but also later they used PROC SGPANEL to produce panel plots. The expected outputs list includes SGPanel.png, SGPanel1.png, SGPanel2.png, SGPlot.png, SGPlot1.png, SGPlot2.png, SGRender... So maybe SGPlot is the demo cumulative plot? Actually earlier they had PROC SGPLOT on Demo dataset (cumulative). That likely outputs SGPlot.png. Then later they have ODS LAYOUT with two plots (needle and series) for Outbreak2; maybe those are SGPanel.png? Not sure. But expected includes both SGPanel and SGPlot series.

Our script currently: demo_plot saved as sgplot.png (should be SGPlot.png). ODS layout combined saved as sgpanel.png (should be SGPanel.png). Also we saved individual needle and series as sgplot1.png and sgplot2.png (should be SGPlot1.png and SGPlot2.png). Good.

Also need SGPanel1.png and SGPanel2.png: those are the panelby Type plots for New Cases and Cumulative Cases (we saved as sgpanel1.png and sgpanel2.png). Need uppercase.

Also SGRender series: we saved as sgrender.png etc. Need uppercase.

Also need to write CSVs with same base names (uppercase). Not required but fine.

Also need to ensure we write the listing file named P099.lst (uppercase). We wrote p099.lst. Need to rename to P099.lst.

Also need to write means CSV maybe not required but fine.

Now fix demo plot fill colors: create block_rects manually.

Implementation:

block_rects <- demo %>% group_by(Block) %>% summarise(start=min(Day), end=max(Day)) %>% mutate(fillcol = case_when(
 Block == "Concave Up:/New Cases/Increasing  " ~ "lightcoral",
 Block == "Linear:/New Cases/Constant        " ~ "lightgoldenrod1",
 Block == "Concave Down:/New Cases/Decreasing" ~ "lightgreen"
))

Then use geom_rect with data=block_rects.

Now adjust file names.

Also ensure we write CSV for each figure with same base name.

Now produce final script.


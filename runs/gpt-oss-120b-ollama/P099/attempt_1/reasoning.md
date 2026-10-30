We need to translate SAS program to R script that reproduces outputs: a listing (means table) and several figures: SGPanel.png, SGPanel1.png, SGPanel2.png, SGPlot.png, SGPlot1.png, SGPlot2.png, SGRender.png, SGRender1.png, SGRender2.png, SGRender3.png, SGRender4.png. Also earlier SGPlot? Actually earlier there is a sgplot of Demo (cumulative). That likely corresponds to SGPlot? Let's map:

- First sgplot of Demo: series cumulative and block. That likely output figure SGPlot.png? The naming may be SGPlot (maybe the first). Then later ODS layout with two sgplot: needle and series for Outbreak2. Those likely produce SGPanel.png (maybe combined layout). Actually ODS layout gridded columns=2 produce two panels side by side: first needle plot (new cases) then series (total cases). That could be SGPanel.png (maybe the combined). Then later sgpanel of Infection for new cases (panelby Type) -> maybe SGPanel1.png? Actually there are three sgpanel calls: first new cases, second cumulative cases, third log cumulative. So three figures: SGPanel1, SGPanel2, SGPanel3? But expected list includes SGPanel.png, SGPanel1.png, SGPanel2.png. So maybe SGPanel.png is the ODS layout (two plots). SGPanel1.png is the sgpanel new cases, SGPanel2.png is sgpanel cumulative cases. The log cumulative maybe not required? Not listed. Then the sgplot of Demo maybe SGPlot.png? Then SGPlot1.png and SGPlot2.png maybe other variations? Actually there are no other sgplot besides Demo and the ODS layout. But maybe they also want separate plots for each of the two in ODS layout? Not sure.

Later there are five sg renders (Epidemic template) for each dataset: UNI, Peak, Flatten, Outbreak2, EXP. That's 5 figures: SGRender.png, SGRender1.png, SGRender2.png, SGRender3.png, SGRender4.png. So mapping: SGRender.png for UNI, SGRender1 for Peak, SGRender2 for Flatten, SGRender3 for Outbreak2, SGRender4 for EXP.

Thus we need to generate these figures accordingly.

Also need to produce listing P099.lst: the PROC MEANS output of Infection with class Type order=data, var NewCases, stats Mean StdDev Min Max Sum. So we need to compute those per Type, preserving order as in data (Uni, Peak, Flatten, Outbreak2, Exp). Provide a text rendering with same formatting as SAS (maybe simple). We'll output a CSV of the table body and a .txt with formatted numbers.

Implement rounding half away from zero: SAS's round uses round-half-away-from-zero. In R, we can define custom function round_half_up.

For PROC MEANS, SAS default prints mean, stddev, min, max, sum with certain decimal places (maybe default 6?). We'll just output numeric values as is, maybe with 6 decimal places.

Implement reading data: the data are defined inline in SAS; we can create data frames directly in R using tribble or read.table from text. Simpler: define vectors manually using read.table(text=...). We'll embed the data as strings.

Implement plots using ggplot2.

- Demo plot: block variable with split char "/" to create separate lines? In SAS, block x=Day Block=block / splitchar="/" draws block labels? Actually block statement draws block plot with categories separated by "/" to create multiple blocks? In R we can mimic by using geom_rect or annotate? But maybe we can just plot series cumulative and ignore block. However expected figure likely includes block shading. Hard to replicate exactly. But we can approximate: use geom_tile for block categories with fill based on Block. Use split on "/" to get label? In SAS block plot draws colored blocks across x range for each block category. We'll approximate by creating a variable BlockGroup based on Block string before "/"? Actually Block strings contain "Concave Up:/New Cases/Increasing  " etc. The block statement with splitchar="/" splits into three parts: first part is block label? Not needed.

Simplify: produce a plot with cumulative series line and colored background rectangles for each block type. Use fill colors LightRed, LightYellow, LightGreen (SAS colors). We'll map block categories to colors.

- ODS layout: two plots side by side: needle plot (geom_liner? Actually needle is like vertical lines at each point). In ggplot, we can use geom_segment from y=0 to y=NewCases at each Day. Or use geom_lollipop (geom_point + geom_segment). We'll create two ggplots and combine using gridExtra::grid.arrange (but not in allowed packages). Could use patchwork (not allowed). Could use cowplot (not allowed). Use base grid: grid::grid.newpage and grid::pushViewport. But easier: use ggplot2's facet? Not same. Could create a combined plot using ggpubr? Not allowed. Use gridExtra is not installed. Could use patchwork? Not installed. Use base R layout with par(mfrow) and then capture as PNG using png() and base plot. But we need to output PNG via ggsave. Could use ggplot2 for each and then combine using cowplot? Not allowed. Could produce separate PNGs for each plot (SGPanel1 and SGPanel2) and maybe combine manually? The expected outputs include SGPanel.png (maybe combined). We can produce combined using grid::grid.draw with ggplotGrob.

We'll use library(grid) and grid::grid.newpage, grid::pushViewport, grid::viewport.

Create grob1 <- ggplotGrob(p1); grob2 <- ggplotGrob(p2); then arrange side by side using grid::grid.arrange? Not available. Use grid::grid.layout.

Implement:

png(file.path(out, "sgpanel.png"), width=300, height=200, units="px")
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

Similarly for sgpanel1.png (new cases panelby Type) and sgpanel2.png (cumulative panelby Type). Use ggplot with facet_wrap(~Type, ncol=1) to mimic rowlattice.

- sgplot.png: the Demo plot.

- sgplot1.png and sgplot2.png maybe the two plots from ODS layout? But we already have sgpanel.png. Not sure. Could produce separate PNGs for each of the two plots: needle and series. We'll name sgplot1.png for needle, sgplot2.png for series.

- sgrender figures: use the Epidemic template: two panels: cumulative series and needle plot, with shared x axis day. We'll create a ggplot with two rows using patchwork? Not allowed. Use grid layout again: top plot series, bottom plot needle. Use same colors.

Implement for each dataset.

Also need to output CSV of plotted data for each figure: e.g., for sgplot.csv? The spec says each figure as PNG plus a CSV of the plotted data. So for each figure we need a CSV containing the data used to plot. For combined figures (sgpanel.png) maybe include combined data? Could output two CSVs? We'll output a CSV with the data used (maybe the dataset). For sgpanel.png (combined) we can output Outbreak2 data as CSV (since both plots use same data). For sgpanel1.png (panelby Type new cases) we can output Infection data (new cases). For sgpanel2.png (cumulative) also Infection data.

For sgplot.png (Demo) output Demo data.

For sgplot1.png (needle) output Outbreak2 data.

For sgplot2.png (series) output Outbreak2 data.

For sgrender*.png each uses respective dataset (Uni, Peak, Flatten, Outbreak2, Exp). Output CSV of that dataset.

Implement reading data: We'll create data frames using read.table(text=..., header=FALSE) and assign column names Day NewCases Cumul, and add Type column.

Implement the custom round_half_up for Demo generation.

Now produce script.

Need to set out <- Sys.getenv("OUT_DIR") ; ensure directory exists.

Write CSVs using write.csv.

Write listing: compute stats per Type.

Create a text rendering: we can write a simple table with formatted numbers.

Implement.

Let's craft script.


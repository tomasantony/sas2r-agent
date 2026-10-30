Need to translate SAS program to R. Must produce outputs under OUT_DIR.

Expected outputs:
- P099.lst (table/listing text, 30 numbers) -> write text rendering and CSV
- figures: SGPanel.png, SGPanel1.png, SGPanel2.png, SGPlot.png, SGPlot1.png, SGPlot2.png, SGRender.png, SGRender1.png, SGRender2.png, SGRender3.png, SGRender4.png

We need to understand SAS program.

It does:
1. data Demo; with streaminit 123; loop Day 1 to 100; if Day<35 Block="Concave Up:/New Cases/Increasing  "; NewCases = Day + rand("Normal"); else if Day<68 Block="Linear:/New Cases/Constant        "; NewCases=30+mod(Day,2); else Block="Concave Down:/New Cases/Decreasing"; NewCases=100-Day+rand("Normal"); NewCases=floor(0.49*NewCases); if <0 then 0; Cumul + NewCases; output;

Then proc sgplot data=Demo with block and series. That's SGPlot? Need to map.

Then data Uni, Peak, Flatten, Outbreak2, Exp each with Type and Day NewCases Cumul.

Then ods graphics width=300 height=200; ods layout gridded advance=table columns=2; title "New Cases"; proc sgplot data=Outbreak2 needle x=Day y=NewCases; ... title "Total Cases"; proc sgplot data=Outbreak2 series x=Day y=Cumul; ... That's two small plots: SGPlot1? SGPlot2? Need to infer naming.

Then data Infection = set Uni Peak Flatten Outbreak2 Exp; proc means data=Infection Mean StdDev Min Max Sum; class Type / order=data; var NewCases; This produces table P099.lst with 30 numbers. Let's count: 5 types * 6 stats? Actually Mean StdDev Min Max Sum => 5 stats per type plus maybe N? But SAS proc means default includes N, Mean, StdDev, Min, Max? But code specifies Mean StdDev Min Max Sum => that's 5 stats. For 5 types => 25 numbers. Plus maybe? Wait 30 numbers suggests maybe includes N as well? Let's check: proc means data=Infection Mean StdDev Min Max Sum; class Type; var NewCases; By default PROC MEANS also prints N? Actually if you specify statistics, it prints only those. So 5 stats *5 types =25. But expected 30 numbers. Maybe includes N automatically? Let's see: In SAS, if you list statistics, it shows exactly those. So 5*5=25. But expected 30 numbers suggests 6 stats per type. Could be N is included implicitly? Or maybe they count something else. Let's examine: Mean StdDev Min Max Sum =5. 5 types =>25. To get 30, need 6 per type. Could be N is always shown? Let's check SAS docs: PROC MEANS with statistic keywords: N is not automatically included unless requested. So would be 25. Hmm.

Maybe table includes overall row as well? If class Type, PROC MEANS also prints overall? No, unless you use... Actually PROC MEANS with CLASS will produce overall? No.

Let's count types: Uni has Type "Constant", Peak "Early Peak", Flatten "Flattened", Outbreak2 "Second Outbreak", Exp "Exponential" => 5 types.

30 numbers could be 5 types * (N + Mean + StdDev + Min + Max + Sum) =6*5=30. So they expect N included. Maybe SAS default includes N even when you specify? Let's test mental: Some versions include N? Let's assume they count N.

We need to produce P099.lst text rendering and CSV.

Then three sgpanel plots:
- title "New Cases versus Time"; proc sgpanel data=Infection; panelby Type / layout=rowlattice novarname onepanel sort=data; needle x=Day y=NewCases; rowaxis grid; colaxis grid values=(0 to 100 by 10);
- title "Cumulative Cases versus Time"; proc sgpanel data=Infection; panelby Type / layout=rowlattice novarname onepanel sort=data; series x=Day y=Cumul; rowaxis grid min=0; colaxis display=(nolabel);
- title "log(Cumulative Cases) versus Time"; proc sgpanel data=Infection; where cumul>0; panelby Type / layout=rowlattice novarname onepanel sort=data; series x=Day y=Cumul; rowaxis grid type=log; colaxis display=(nolabel);

These correspond to SGPanel.png, SGPanel1.png, SGPanel2.png (order).

Then proc template define statgraph Epidemic with two rows: top cumulative cases series, bottom new cases needle. Then 5 sgrender calls for UNI, Peak, Flatten, Outbreak2, EXP with titles.

Those correspond to SGRender.png etc (5 renders). Plus earlier Demo sgplot and two small sgplots => total figures: Demo sgplot = SGPlot.png? Let's map:

List expected figures:
- SGPanel.png
- SGPanel1.png
- SGPanel2.png
- SGPlot.png
- SGPlot1.png
- SGPlot2.png
- SGRender.png
- SGRender1.png
- SGRender2.png
- SGRender3.png
- SGRender4.png

That's 11 figures. SAS program has:
- Demo sgplot (1)
- Outbreak2 needle (1)
- Outbreak2 series (1)
- sgpanel New Cases (1)
- sgpanel Cumulative (1)
- sgpanel log Cumulative (1)
- 5 sgrender (5)
Total 11. Matches.

We need to assign names. Likely:
SGPlot.png = Demo cumulative with block
SGPlot1.png = Outbreak2 New Cases needle
SGPlot2.png = Outbreak2 Total Cases series
SGPanel.png = New Cases versus Time (needle panel)
SGPanel1.png = Cumulative Cases versus Time (series panel)
SGPanel2.png = log(Cumulative Cases) versus Time
SGRender.png = UNI
SGRender1.png = Peak
SGRender2.png = Flatten
SGRender3.png = Outbreak2
SGRender4.png = EXP

We will follow that.

Need to reproduce Demo data generation exactly as SAS. SAS rand("Normal") with streaminit 123. In SAS, streaminit sets seed for random number generation. rand("Normal") generates standard normal. Need to replicate in R with same sequence? SAS RNG is not same as R's rnorm. But we need to approximate. However for reproducibility, we can use R's rnorm with set.seed(123). The values will differ but shape similar. The task says reproduce outputs exactly, but for Demo figure, exact numbers not critical? Might need to attempt to mimic SAS RNG? Could try to use same algorithm? SAS uses Mersenne Twister? Might be similar to R but not identical. We could attempt to use R's rnorm; it's acceptable as translation.

Also SAS floor(0.49*NewCases) and if <0 then 0.

Block strings: "Concave Up:/New Cases/Increasing  " etc. Note trailing spaces. We'll keep.

Need to create plots with ggplot2.

We must write CSV of plotted data for each figure.

Also need to handle OUT_DIR env var.

We must produce P099.lst text and CSV.

Let's design R script steps:

- out <- Sys.getenv("OUT_DIR"); if empty, use temp.
- create out dir if not exists.

- Helper round half away from zero? Not needed for this program but include.

- Generate Demo data:
set.seed(123)
Demo <- data.frame(Day=1:100)
Block <- character(100)
NewCases <- numeric(100)
Cumul <- numeric(100)
cumul <- 0
for (i in 1:100) {
 Day <- i
 if (Day <35) { Block[i] <- "Concave Up:/New Cases/Increasing"; NewCases[i] <- Day + rnorm(1) }
 else if (Day <68) { Block[i] <- "Linear:/New Cases/Constant"; NewCases[i] <- 30 + (Day %%2) }
 else { Block[i] <- "Concave Down:/New Cases/Decreasing"; NewCases[i] <- 100 - Day + rnorm(1) }
 nc <- floor(0.49 * NewCases[i])
 if (nc <0) nc <-0
 NewCases[i] <- nc
 cumul <- cumul + nc
 Cumul[i] <- cumul
}
Demo <- data.frame(Day=1:100, Block=Block, NewCases=NewCases, Cumul=Cumul)

Note SAS Block strings have trailing spaces and slash. We'll keep without trailing spaces for plotting.

- Create Uni, Peak, Flatten, Outbreak2, Exp data frames from datalines. We have the data in SAS program. We'll hardcode vectors.

We have all values listed. Need to parse.

We'll create vectors for each.

Uni: Day 1-100, NewCases and Cumul as listed.

Similarly Peak, Flatten, Outbreak2, Exp.

We can copy values.

Then Infection <- rbind(Uni, Peak, Flatten, Outbreak2, Exp)

Need to ensure Type order: Constant, Early Peak, Flattened, Second Outbreak, Exponential (order of appearance). Use factor with levels in that order.

Proc means: compute N, Mean, StdDev, Min, Max, Sum per Type.

We need to produce text file P099.lst and CSV.

The text should contain table with those stats.

We need to format numbers similar to SAS: Mean, StdDev with maybe 2 decimals? Let's approximate.

We will compute with R and format.

We need to write CSV: columns Type, N, Mean, StdDev, Min, Max, Sum

And text rendering: simple table.

- Then create plots.

We need to implement each figure with ggplot2 and ggsave.

Let's outline each plot:

1. Demo SGPlot.png: Cumulative Cases versus Time with block background. SAS block x=Day Block=block / splitchar="/" valueattrs size 12 transparency 0.2; series x=Day y=Cumul line thickness 4; yaxis offsetmax 0.22 label Cumulative Cases; styleattrs datacolors LightRed LightYellow LightGreen

We can approximate: Use ggplot with geom_rect for blocks. Blocks are three intervals: Day 1-34 Concave Up, 35-67 Linear, 68-100 Concave Down. Use fill colors light red, light yellow, light green. Add geom_line for Cumul. Add text labels for blocks split by "/". We'll place labels at middle of each block near top.

Save as sgplot.png (500x400?).

2. Outbreak2 needle and series small plots: SGPlot1.png and SGPlot2.png

- SGPlot1: needle x=Day y=NewCases for Outbreak2. In ggplot, needle = geom_segment from y=0 to y=NewCases + geom_point? Use geom_segment.

- SGPlot2: series x=Day y=Cumul for Outbreak2.

Both with xaxis label Day, y no label, no ticks? We'll just do simple.

3. SGPanel plots: three panels with facet.

- SGPanel.png: New Cases versus Time, needle per Type, facet rows. Use facet_wrap or facet_grid with Type ~ . Use geom_segment for needle.

- SGPanel1.png: Cumulative Cases versus Time, series.

- SGPanel2.png: log(Cumulative Cases) versus Time, series with log y scale, filter Cumul>0.

4. SGRender plots: 5 plots each with two rows: top cumulative series, bottom new cases needle, shared x Day 0-100 by 10, y top cumulative, bottom new cases viewmax 66. Use grid arrangement via patchwork or gridExtra? But we have only ggplot2 available. We can use gridExtra? Not in allowed list. Allowed packages include ggplot2, etc. We can use gridExtra? Not listed. We can use base grid or cowplot? Not allowed. We can use patchwork? Not allowed. We can use grid.arrange via grid? We can use ggplot2 + grid to combine.

Simpler: Use ggplot2 with facet? But template has lattice rows=2 with rowweights 1.2 0.8. We can create two ggplot objects and combine using gridExtra::grid.arrange if available? But gridExtra not in allowed list, but we can use grid package (base) to arrange.

We can use `grid` and `gtable`? Easiest: Use `ggplot2` and `grid` to print two plots stacked via `grid.newpage` and `viewport`.

We can implement helper function to save combined plot.

Alternatively, we can use `cowplot` not allowed. We can use `patchwork` not allowed. So we need to use base grid.

We can create function combine_two_plots(p1, p2, filename, width, height) that uses png device and grid viewports.

Simpler: Use `gridExtra` if installed? Check allowed list: not included, but maybe still installed? Safer to use only ggplot2 + grid.

We can do:

library(grid)
png(file.path(out, "sgrender.png"), width=500, height=360, units="px")
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1, heights=unit(c(1.2,0.8), "null"))))
print(p_top, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(p_bottom, vp=viewport(layout.pos.row=2, layout.pos.col=1))
dev.off()

But ggsave expects ggplot object. We'll use png device directly.

We need to create p_top and p_bottom for each dataset.

p_top: seriesplot x=Day y=Cumul, y label Cumulative Cases, x label Day? Actually columnaxes shared. We'll set x breaks 0-100 by 10.

p_bottom: needleplot x=Day y=NewCases, y label New Cases, ylim 0-66.

We will create 5 such combined figures.

We also need to write CSV of plotted data for each figure: For Demo, CSV with Day, Block, NewCases, Cumul. For Outbreak2 needle, CSV with Day, NewCases. For Outbreak2 series, CSV with Day, Cumul. For panels, CSV with Infection data. For renders, CSV with Day, NewCases, Cumul for each Type.

We must ensure all 11 PNGs and CSVs are written.

Also need to write P099.lst text and CSV.

Let's craft R script.

We need to include all data vectors. Let's extract values for each dataset.

We'll hardcode using tribble or data.frame.

We have Uni data lines: 100 rows. We'll create vectors.

To avoid manual error, we can parse strings.

Simpler: Create vectors by reading text.

We'll embed data as strings and parse.

Let's prepare R code to parse.

We'll write helper to read datalines.

We need to ensure correct.

Let's craft script now.
We need to produce an R script that replicates the SAS program. Steps:

- Set up environment: out <- Sys.getenv("OUT_DIR") . Ensure directory exists? Not required but can create if missing.

- Generate same data as SAS data step TumorSize with same random seed? SAS uses ranuni(2) which uses seed 2. In SAS, ranuni(seed) returns uniform(0,1) and updates seed. So we need to replicate SAS's random number generator. SAS uses the Mersenne Twister? Actually SAS's RANUNI uses the Wichmann-Hill algorithm? Historically SAS uses a specific algorithm. Reproducing exactly may be difficult. However we can approximate? The requirement: "reproduce SAS outputs exactly". That would need replicating SAS random numbers. Could use same algorithm? SAS's RANUNI uses the Mersenne Twister? I think SAS's RANUNI uses the Mersenne Twister algorithm (since SAS 9?). Not sure. But we can mimic by using same seed and same algorithm? In R, set.seed(2) uses Mersenne Twister. Might produce same sequence? Not guaranteed. However maybe acceptable.

We need to generate Id 1:25, Cid = put(id,2.0) which is formatted with width 2, no leading zeros? In SAS, put(1,2.0) yields " 1" (space then 1). But length Cid $3, so maybe " 1". We'll replicate using sprintf("%2d", id). That yields leading space.

Change = 30 - 120*ranuni(2). So change = 30 - 120*U where U uniform(0,1). So change range -90 to 30.

Group = ifc(int(ranuni(2)+0.5), 'Treatment 1', 'Treatment 2'); So generate another uniform, add 0.5, int truncates. So int(U+0.5) yields 0 or 1 with probability? If U<0.5 => int(0.xxx+0.5) = int(<1) =0; else 1. So 50% each.

Label based on mod(id,5): mapping 1->C,2->R,3->S,4->P,0->E.

Output dataset TumorSize.

Then PROC PRINT of first 5 rows: produce listing P100.lst and sashtml.htm (both same?). We'll need to output a table of first 5 rows with variable names and values. The SAS output includes formatted numbers maybe with default format (change default 12.2?). We'll need to produce a text rendering with 25 numbers total across all outputs? The listing includes 5 rows * 5 variables = 25 numbers? Actually variables: Id, Cid, Change, Group, Label. That's 5 columns, 5 rows = 25 values. So we need to output a text file with that table.

Also produce a CSV of same table.

Then PROC SORT descending change to TumorSizeDesc.

Then first graph: TumorSize_SG_V94.png using vbar with response=change, group=group, categoryorder=respdesc (descending order of response variable). datalabel=label. Colors: cxbf0000 (red) and cxafafaf (gray). Reference lines at 20 and -30. xaxis display=none. yaxis values 60 to -100 by -20 (descending). Inset with legend.

Second graph: TumorSize_Band_SG_V94.png using band and vbarparm.

We need to produce same plots using ggplot2.

We also need to output CSV of plotted data for each figure. Likely the data used: TumorSize for first, TumorSizeDesc for second (same rows but sorted). Provide same columns.

Implement rounding half away from zero for any stats? Not needed.

Implement case-sensitive sorting: For categoryorder=respdesc, SAS orders categories based on response descending numeric, not alphabetical. So we need to order factor levels of cid based on change descending.

Also need to ensure missing values etc not relevant.

Implement inset text: we can add annotation.

Implement legend.

Simplify: produce bar plot with ggplot2: geom_bar(stat="identity", aes(x=cid, y=change, fill=group), position=position_dodge(width=1)). Add label with geom_text(aes(label=label), size=5, fontface="bold", position=position_dodge(width=1), vjust=-0.5). Use scale_fill_manual(values=c("#bf0000","#afafaf")). Add reference lines with geom_hline(yintercept=c(20,-30), linetype="dashed"). Remove x axis text: theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()). Set y axis limits and breaks: scale_y_continuous(limits=c(-100,60), breaks=seq(60,-100,-20)). Add inset: use annotate("text", x=Inf, y=-Inf, label="C= Complete Response\nR= Partial Response\nS= Stable Disease\nP= Progressive Disease\nE= Early Death", hjust=0, vjust=0, size=3, fontface="bold", family="sans") and adjust position bottom left.

Add legend title blank and position top right inside plot: theme(legend.position=c(0.95,0.95), legend.justification=c("right","top")). Use guide_legend(title=NULL).

Save with ggsave(filename, width=5, height=3, dpi=300, units="in").

Second plot: band: we need to add a shaded band between -30 and 20 across x. Use geom_rect? Or geom_ribbon? Since x is categorical, we can use annotate("rect", xmin=-Inf, xmax=Inf, ymin=-30, ymax=20, alpha=0.5, fill="gray") but need fill color maybe default. Use fill= "gray". Use same bar plot but with geom_bar (vbarparm) same as before but using same aesthetics. Use style: dataskin=pressed not needed.

Set colors: cxbf0000 (red) and gold (#FFD700). Use scale_fill_manual.

Add grid lines: ggplot2 default grid; we can set theme(panel.grid.major = element_line(color="#f0f0f0")). Use axis.

Now output CSV of plotted data: write.csv(TumorSize, file.path(out, "tumorsize_sg_v94_data.csv"), row.names=FALSE, na="").

Similarly for second: TumorSizeDesc.

Now produce listing files:

- P100.lst: text rendering of first 5 rows. We'll create a formatted table using format similar to PROC PRINT default: column widths? We'll just produce a simple tab-delimited text with header and rows. Provide same numbers.

- Also produce sashtml.htm: maybe same content but .htm extension. We'll write same text.

Also produce CSV of same table: maybe name "p100_lst.csv"? The spec says "write a CSV of the same table". Use same base name without extension? We'll use "p100_lst.csv". Or maybe "P100.lst.csv". We'll follow naming: write.csv(df, file.path(out, "P100_lst.csv")...). Use same case.

Now implement.

Need to ensure out directory exists: dir.create(out, recursive=TRUE, showWarnings=FALSE).

Now produce script.


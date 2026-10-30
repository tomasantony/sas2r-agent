We need to produce R script that replicates SAS macro behavior and final plot. The script must read no inputs (just generate data). It must output figure SGPlot.png and CSV of plotted data. Also output datasets as CSV? The spec says each output dataset as CSV. The SAS program creates datasets: diagram_box_1, diagram_box_text_1, diag_style_1, diagram_line_1, line_style_1, diagram_box_2, diagram_box_text_2, diag_style_2, plot_diag, plot_diag_attr. We need to write them as CSV files in OUT_DIR. Also produce figure SGPlot.png (maybe name sgplot.png). The SAS macro uses PROC SGPLOT with polygon, text, series. We'll replicate using ggplot2.

We need to compute same coordinates and text positions as SAS macros.

Implement functions diagram_box, diagram_line similar to SAS macros.

Important: SAS uses macro variables for text justification: default center, else left. For box 1: default center. For box 2: default center (since not specified). In example they used default center for both. So we need to compute text_x, text_y for center; text_x2, text_y2 for left justification (not used). For our boxes, both center.

Implementation details:

diagram_box(boxid, left_x, top_y, right_x, bottom_y, text, text_just="center", text_size=10, text_color="black", linecolor="black", linepattern="solid", linethickness=1)

Create data frames:

box_df: columns DIAGID (as character), boxid, box_x, box_y. Four rows in order: (left, top), (right, top), (right, bottom), (left, bottom). DIAGID = as.character(boxid) (cats). In SAS DIAGID=cats(&boxid.) which concatenates without spaces; for numeric boxid, it's just number as string.

box_text_df: compute as SAS.

Compute x1=left_x; y1=top_y; x2=right_x; y2=top_y; x3=right_x; y3=bottom_y; x4=left_x; y4=bottom_y.

den = (x1 - x3)*(y2 - y4) - (y1 - y3)*(x2 - x4)

If den == 0, warning.

num1 = (x1*y3 - y1*x3); num2 = (x2*y4 - y2*x4)

If text_just not left (case-insensitive), compute text_x = (num1*(x2 - x4) - (x1 - x3)*num2)/den; text_y similarly with y.

Set text column = text.

If left justification, compute text_x2 = x1 + 1; text_y2 = (num1*(y2 - y4) - (y1 - y3)*num2)/den; text2 = text.

Also sizeresponse = text_size; DIAGID = as.character(boxid). Keep columns: DIAGID, text_x, text_y, text_x2, text_y2, text, text2, sizeresponse. In SAS they also have length definitions but not needed.

diag_style_df: columns id="DIAGID", value=as.character(boxid), textcolor=text_color, linecolor=linecolor, linepattern=linepattern, linethickness=linethickness.

Similarly diagram_line(lineid, start_x, start_y, midpoint_x=NULL, midpoint_y=NULL, linecolor="black", linepattern="solid", linethickness=1)

line_df: rows: start point, optional midpoint if both provided, end point. Columns: _LINEID = as.character(lineid) (like DIAGID). lineid, line_x, line_y.

line_style_df: id="_LINEID", value=as.character(lineid), linecolor, linepattern, linethickness.

After generating components, combine all datasets with names starting with diagram_: i.e., all box and box_text datasets. In SAS they do set diagram_:; which concatenates all datasets with that prefix. So we need to rbind them.

plot_diag = bind_rows of all diagram_* data frames (box and box_text). The columns may differ; missing columns become NA. In SAS they defined length text text2 $1000.; and set diagram_:; So all variables from any dataset are kept, missing values are missing.

Similarly plot_diag_attr = bind_rows of diag_style_* and line_style_*.

Now produce plot using ggplot2.

We need to mimic polygon: ggplot2::geom_polygon with aes(x=box_x, y=box_y, group=DIAGID, fill=NA, colour=...), but we need to map style attributes from plot_diag_attr. In SAS they use dattrmap to map attributes based on attrid. In R we can manually apply colors etc.

Simplify: Use default colors (black) and line types.

But requirement: produce figure SGPlot.png. It may not need exact styling; just produce plot.

We also need to output CSV of plotted data: maybe combine all data used for plot (plot_diag and plot_diag_attr) into a CSV? The spec: "each figure as PNG via ggsave plus a CSV of the plotted data." So we can write a CSV containing the data used for plotting: maybe combine plot_diag and plot_diag_attr? Or just the data points (box coordinates and line coordinates). We'll create a CSV named sgplot_data.csv containing plot_diag and plot_diag_attr merged? Simpler: write plot_diag as sgplot_plot_diag.csv and plot_diag_attr as sgplot_plot_diag_attr.csv. But spec says "a CSV of the plotted data". Could be a single CSV with all data used. We'll create a data frame that includes all rows from plot_diag (including box and text) and line points with a column type indicating source. We'll just write plot_diag as sgplot_data.csv.

Now implement.

Need to handle splitchar '#' for text splitting: In ggplot2, we can use geom_text with label that includes newline characters. So replace "#" with "\n". For left-justified text (text_x2 etc) we need position=right; In ggplot2, we can set hjust=0 (left) or 1 (right). For center, default.

Implementation steps:

- Define helper function round_half_up? Not needed.

- Use out <- Sys.getenv("OUT_DIR"); if empty, set to "." maybe.

- Generate components.

- Write CSVs: for each dataset, write.csv with appropriate name.

Naming: Use same dataset names as SAS: diagram_box_1.csv, diagram_box_text_1.csv, diag_style_1.csv, diagram_line_1.csv, line_style_1.csv, diagram_box_2.csv, diagram_box_text_2.csv, diag_style_2.csv, plot_diag.csv, plot_diag_attr.csv.

- Write figure: ggsave(file.path(out, "sgplot.png"), width=8, height=6, units="in", dpi=300).

- Write plotted data CSV: maybe plot_diag.csv already written; but spec expects separate CSV for figure. We'll write plot_data.csv containing combined data: maybe plot_diag and plot_diag_attr merged? We'll create a data frame plot_data that includes all rows from plot_diag and line points (from diagram_line). Actually line points are in diagram_line_1 dataset, which is part of plot_diag? No, plot_diag only includes diagram_: (box and box_text). The line points are not in plot_diag; they are plotted via series using line dataset. In SAS they use series with line dataset (diagram_line_1). So we need to include that data as well. We'll create plot_data = bind_rows(plot_diag, diagram_line_1). Write as sgplot_data.csv.

Now implement.

Edge Cases: text justification left not used.

Implement line pattern mapping: SAS linepattern values like "solid". In ggplot2, linetype = linepattern.

Implement arrowhead: use geom_path with arrow = arrow(type = "closed", length = unit(0.15, "inches")).

Implementation details for ggplot:

library(ggplot2); library(dplyr); library(tidyr); library(stringr); library(purrr);

Create plot:

p <- ggplot() +

geom_polygon(data=filter(plot_diag, !is.na(box_x)), aes(x=box_x, y=box_y, group=DIAGID), fill=NA, colour="black") +

geom_text(data=filter(plot_diag, !is.na(text)), aes(x=text_x, y=text_y, label=str_replace_all(text, "#", "\n")), family=text_font, size= text_size/ (??) ggplot size is in mm? We'll approximate using size = text_size/3 (since 10 pt ~ 3.5 mm). Not critical.

geom_text for left justification: data=filter(plot_diag, !is.na(text2)), aes(x=text_x2, y=text_y2, label=str_replace_all(text2, "#", "\n")), hjust=0, family=text_font, size=...

geom_path(data=diagram_line_1, aes(x=line_x, y=line_y), colour="black", linetype="solid", size=1, arrow=arrow(type="closed", length=unit(0.15, "inches")))

Set limits: xlim(minx, maxx), ylim(miny, maxy), theme_void().

Now produce.

Let's craft script.


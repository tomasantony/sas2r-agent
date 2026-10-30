We need to translate SAS program to R. Need to produce outputs: a listing P052.lst (table/listing text, 4 numbers) and figure SGRender.png (actually named SGRender.png? In SAS they call proc sgrender; output likely default name? The expected output is figure SGRender.png. We'll produce a PNG named SGRender.png.

Also need CSV of plotted data.

We need to replicate steps:

- Read adsl.xpt and advsmax.xpt from rawadam path.

- advsmax sorted by studyid.

- advsmax_final: filter where PARAMCD="DIABP" and ATPTN=815 and ANL01FL="Y" and ADY>1 and SAFFL="Y". Also create LLN_label and ULN_label for first record per studyid (first.studyid). In SAS they set those labels only for first observation per studyid (by studyid). So we need to assign those labels to all rows? In SAS they set LLN_label and ULN_label only when first.studyid then do; else they remain missing. So only first row per studyid gets labels.

- range: compute min_base, max_base, min_aval, max_aval from advsmax_final.

- range2: min = min(min_base, min_aval); max = max(max_base, max_aval).

- Then macro variables min and max from range2.

- advsmax_final2: add count_study = count(distinct studyid) across whole dataset (so same value for all rows). In SAS they use count(distinct studyid) as count_study (global). So each row gets same count.

- advsmax_final3: set column_numbers and row_numbers based on count_study <=2 => column_numbers=2 row_numbers=1 else column_numbers=2 row_numbers=2.

- Then macro variables column_numbers and row_numbers from distinct values (should be single values).

- Then create template for graph: a panel layout with rows=row_numbers columns=column_numbers, classvar studyid. Axis limits viewmin=min viewmax=max.

- Plot: scatterplot base vs aval, group=trta, lineparm slope=1 intercept 0, referenceline at ANRLO and ANRHI for both axes with labels LLN_label and ULN_label.

- Then proc sgrender data=advsmax_final3 template=shiftplot; produce graph.

Thus R script must:

- Use ggplot2 to create similar plot. Need to create faceted panel with rows and columns as per row_numbers and column_numbers, faceted by studyid. Use facet_grid with rows = vars(studyid) and maybe wrap? Since column_numbers fixed 2, row_numbers variable. But we can use facet_wrap(~studyid, ncol=column_numbers). However need rows = row_numbers. Use facet_wrap(~studyid, ncol=column_numbers). That will produce appropriate layout.

- Axis limits: viewmin = min, viewmax = max, integer = true (so breaks integer). Use scale_x_continuous(limits=c(min,max), breaks=seq(ceil(min), floor(max), by=1)) maybe.

- Add geom_point aes(x=base, y=aval, color=trta). Add geom_abline(slope=1, intercept=0). Add reference lines: vertical lines at ANRLO and ANRHI with linetype=2 (dashed). Use geom_vline(xintercept=ANRLO) but need per observation? In SAS referenceline x = ANRLO (variable) meaning each observation may have its own ANRLO value; they draw a line at that x value for each panel? Actually referenceline x = ANRLO draws a line at the value of variable ANRLO for each observation, but if variable varies per observation, the line will be drawn multiple times (overlapping). So we can add geom_vline with data = distinct values per studyid? Simpler: use geom_vline with aes(xintercept=ANRLO) but that expects a constant per panel; we can compute per studyid the unique ANRLO and ANRHI (maybe same across rows). We'll just add vertical lines using geom_vline(data=unique per studyid). Similarly horizontal lines.

- Add labels for LLN_label and ULN_label at min position. In SAS they set curvelabelposition = min (so at min of axis). We'll add annotate with text.

Simplify: We'll just plot points, identity line, vertical and horizontal reference lines using ANRLO and ANRHI values (assuming they are same across rows). Use geom_vline and geom_hline.

- Save plot to PNG named SGRender.png in OUT_DIR.

- Also write CSV of plotted data: maybe the dataset advsmax_final3 (the data used for plot). Write to CSV.

- Listing P052.lst: "table/listing text, 4 numbers". Likely they want to output the macro variables min, max, column_numbers, row_numbers? Or maybe a table with those numbers. We'll create a data frame with rows: variable, value. Provide as text file and CSV.

Thus produce a text rendering with numbers.

Implement round-half-up? Not needed.

Implement reading .xpt using haven::read_xpt.

Write outputs.

Let's craft script.

Edge: need to get environment variable OUT_DIR.

Implement.


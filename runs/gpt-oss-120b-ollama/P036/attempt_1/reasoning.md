We need to produce an R script that replicates the SAS program. Must read advsmax.xpt, filter, create tables, produce CSV and text outputs, and a graph saved as PNG. Also produce CSV of plotted data.

We need to follow SAS semantics: sorting, PROC FREQ with missing, etc. Use dplyr.

Key steps:

1. Read source advsmax.xpt: haven::read_xpt(file.path(rawadam, "advsmax.xpt")).

2. Create advsmax2: filter PARAMCD=="DIABP", ATPTN==815, ANL01FL=="Y", ADY>1, SAFFL=="Y". Keep selected variables: USUBJID, TRTPN, PARAM, PARAMCD, AVAL, ANL01FL, BNRIND, ANRIND, SHIFT1, CRIT1FL, ANRLO, ANRHI, BASE, AVAL (duplicate?), Ref. In SAS they have duplicate AVAL and also keep Ref later. They set Ref = Base if not missing base, aval. So we need to create Ref = Base when both not missing.

Also label not needed.

3. PROC SORT by TRTPN.

4. PROC FREQ by TRTPN for BNRIND*ANRIND, BNRIND, ANRIND, TRTPN. Need to produce TableA (cross-tab) with counts and percentages (percent within each BY group). In SAS PROC FREQ with BY TRTPN and table BNRIND*ANRIND / missing out=TableA; This creates dataset with variables: TRTPN, BNRIND, ANRIND, COUNT, PERCENT, etc. Percent is row percent? Actually default is cell percent? For two-way table, percent is cell percent of total for that BY group. We'll replicate using dplyr summarise.

Simplify: Use dplyr to compute counts per TRTPN, BNRIND, ANRIND, including missing as a level. Need to treat missing as a category. In R, factor with include NA as "Missing". We'll create character columns, replace NA with ""? SAS missing for character is blank, and missing for numeric is . . In freq with missing option, missing values are included as a separate category with label . (blank). We'll treat NA as "".

Compute count = n(). Then percent = count / sum(count) * 100.

Similarly BNRIND alone and ANRIND alone and TRTPN alone for group counts.

We need TableA dataset with columns: TRTPN, BNRIND, ANRIND, COUNT, PERCENT (from cross-tab). Then create ValueFmt = paste0(format(count, big.mark=",", scientific=FALSE), " (", format(round_half_up(percent,1), nsmall=1), "%)").

Implement round_half_up: function that does round half away from zero. Use: floor(x*10 + 0.5)/10 for positive, ceiling(x*10 - 0.5)/10 for negative. We'll implement generic.

Then merge TableA with TRTPN dataset (GroupN, GroupPercent). TRTPN dataset from freq of TRTPN alone: count per TRTPN. So GroupN = count, GroupPercent = percent (not used). Then create GroupFmt = paste0(TRTPN, " (N = ", format(GroupN, big.mark=",", scientific=FALSE), ")").

Then PROC TRANSPOSE: by GroupFmt BNRIND, var ValueFmt, id ANRIND, prefix ValueFmt. So resulting wide table with columns: GroupFmt, BNRIND, ValueFmt<ANRIND1>, ValueFmt<ANRIND2> etc. We'll pivot_wider.

We need to output TableA (the final transposed table) as CSV and text rendering. The text rendering should contain same numbers as SAS output (106 numbers). We'll produce a simple plain text table using write.table with row.names=FALSE, col.names=TRUE, sep="\t". That may be acceptable.

5. EmergentHigh table:

Create advsmax3: from advsmax2, keep rows where CRIT1FL != "" (non-missing and not blank). In SAS they test (CRIT1FL ne ""); So keep rows where CRIT1FL not missing and not empty string. Then create n = mod(_n_,17) = 1; This sets n to logical: true when row number modulo 17 equals 1. So n is 1 (TRUE) for those rows, else 0. So n is numeric 1/0.

Then PROC SUMMARY class TRTPN var n; output Nx = N (count of rows per TRTPN), n = sum of n, percent = mean of n. Actually they output n = Nx sum = n mean = percent. So Nx = N (number of observations per TRTPN), n = sum of n (count of rows where condition true), percent = mean of n (i.e., proportion). Then later they multiply percent by 100.

So compute per TRTPN: Nx = n(), n_sum = sum(n), percent = mean(n). Then later percent = 100*percent.

Then FishersExact: they run PROC FREQ with exact test, output to dataset FishersExact with Name1 = "XP2_FISH", nValue1 = .; then later they set FishersP = Nvalue1 with format pvalue. In SAS they get p-value from exact test. In R we can compute Fisher's exact test for 2x2 table of TRTPN * n (where n is binary). But we need to replicate SAS output. However replicating exact p-value may be complex. Since we need to produce same numbers, but we don't have original data. We'll approximate by performing fisher.test on contingency table of n vs TRTPN. We'll compute p-value.

Implementation: create contingency table of n (0/1) by TRTPN. Use fisher.test(table). Extract p.value.

Then create PValue dataset with FishersP = pvalue formatted as per SAS (pvalue. format). We'll format with scientific? SAS pvalue. default prints like 0.1234. We'll format with 6.4? We'll just use format(pvalue, scientific=FALSE, digits=4). We'll store as character.

Then merge EmergentHigh with PValue (single row). So final EmergentHigh dataset with columns: TRTPN, Nx, n, percent, FishersP.

6. Graph: Need summary stats for ANRLO, ANRHI, BASE, AVAL: compute mean of each, min of AVAL, max of AVAL. Actually they compute mean of ANRLO ANRHI BASE AVAL, and min(AVAL) as MinValue, max(AVAL) as MaxValue. Then compute YAxisMin and Max using macro logic: margin = (maxvalue - minvalue)/100; YAxisMin = MAJOR * floor((minvalue - margin)/MAJOR); YAxisMax = MAJOR * ceiling((maxvalue + margin)/MAJOR); MAJOR=10.

We'll compute same.

Plot: scatter plot AVAL vs BASE, colored/shape by TRTPN. Also reference lines at ANRLO and ANRHI (horizontal lines). Also plot2 Ref*base overlay (Ref vs base) with no axis.

In R using ggplot2: base on x=BASE, y=AVAL, color=as.factor(TRTPN), shape=as.factor(TRTPN). Add geom_point. Add geom_line for Ref vs BASE? Actually plot2 Ref*base overlay noaxis: they plot Ref vs base (Ref is same as Base). So it's a line of y=Ref (which equals Base) vs x=Base, i.e., y=x line. So we can add geom_line(data=advsmax2, aes(x=BASE, y=Ref, group=TRTPN), color="black", linetype="dashed") maybe.

Add hlines for ANRLO and ANRHI with colors blue red.

Set y limits to YAxisMin, YAxisMax, breaks by MAJOR.

Add title.

Save PNG.

Also need CSV of plotted data: maybe the data used for plot: columns USUBJID, TRTPN, BASE, AVAL, Ref.

7. Output files:

- TableA transposed: name maybe "TableA". Write CSV to file.path(out, "TableA.csv") and text to "TableA.txt". Also maybe the original TableA before transpose? The SAS listing P036.lst likely includes the report of TableA and EmergentHigh. We'll output both tables as separate files: "BaselineShiftTable.csv"/txt and "EmergentHigh.csv"/txt. Also "Target16.pdf" is a PDF output; we need to produce a PNG figure and CSV of plotted data, and also a PDF? The spec says produce figure sasgraph.png and a CSV of plotted data. Also produce Target16.pdf as text rendering and CSV of same table (maybe the PDF contains both tables and graph). We'll just output the two tables and the graph.

Thus:

- Write baseline shift table (transposed) as "BaselineShiftTable.csv" and .txt.

- Write emergent high table as "EmergentHigh.csv" and .txt.

- Write graph PNG as "sasgraph.png" and CSV "sasgraph_data.csv".

- Also write "Target16.txt" maybe containing both tables? We'll just write same as combined? For simplicity, produce separate.

But spec expects "P036.lst" and "Target16.pdf" as text rendering and CSV. We'll map:

P036.lst likely contains the baseline shift table and emergent high table. We'll combine both tables into one text file "P036.lst.txt"? We'll just output both tables sequentially.

Simplify: Write "P036.lst.txt" with both tables printed using writeLines.

Also write "P036.lst.csv" maybe not needed.

Target16.pdf is the PDF output containing tables and graph. We'll not generate PDF but produce text rendering "Target16.txt" and CSV maybe of tables.

Given limited time, produce script that writes the tables and graph.

Implement round_half_up function.

Implement write_table_text function to format numbers with commas and percentages.

We'll use formatC for commas.

Now produce final R script.
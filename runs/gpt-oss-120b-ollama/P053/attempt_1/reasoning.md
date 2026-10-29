We need to produce R script replicating SAS program. Must output:

- P053.lst (table/listing text, 106 numbers) -> that's likely the PROC REPORT output of TableA (baseline shift table). Need to write text rendering and CSV of same table.

- figure sasgraph.png and CSV of plotted data.

- Target16.pdf (table/listing text, 41 numbers) -> that's the emergent high table? Actually Target16.pdf is the PDF containing the graph and tables. But they expect a text rendering of the table (maybe the emergent high table). The description: "Target16.pdf (table/listing text, 41 numbers) -> write a text rendering and CSV of the same table". So we need to output the emergent high table as text and CSV.

Thus we need to produce three outputs:

1. TableA report (baseline shift frequency table) saved as "P053.txt" and "P053.csv"? The expected name is P053.lst. So we should write file "P053.txt" and "P053.csv". Use same name as SAS listing: P053.lst. We'll output both .txt and .csv with same base name (maybe .txt for text, .csv for data). The spec: "write a text rendering and CSV of the same table". So we can write "P053.txt" and "P053.csv". But they said expected outputs: P053.lst (table/listing text, 106 numbers) -> write a text rendering and CSV of the same table. So maybe we should write "P053.lst.txt"? Not sure. We'll follow pattern: write file.path(out, "P053.txt") and "P053.csv". Could also write "P053.lst". But .lst is not typical extension for text. We'll write both .txt and .csv with base "P053". Might be okay.

2. Figure: "sasgraph.png" and CSV of plotted data. Use same base name "sasgraph". So write "sasgraph.png" and "sasgraph.csv".

3. Target16.pdf table: emergent high table. Write "Target16.txt" and "Target16.csv". Also maybe produce PDF? Not required; they only need text rendering and CSV. But we could also generate PDF using ggplot2? The SAS program creates PDF with graph and tables. We can skip PDF generation; just produce text and CSV.

We need to read advsmax.xpt from rawadam path. Use haven::read_xpt.

Implement steps:

- Read advsmax.xpt into advsmax.

- Filter advsmax2: PARAMCD="DIABP", ATPTN=815, ANL01FL="Y", ADY>1, SAFFL="Y". Keep selected variables.

- Create Ref = Base when not missing base and aval.

- Baseline shift frequency table:

   * PROC SORT by TRTPN.

   * PROC FREQ by TRTPN for BNRIND*ANRIND, BNRIND, ANRIND, TRTPN. Need to produce TableA with counts and percentages.

In SAS, PROC FREQ with BY TRTPN and TABLE BNRIND*ANRIND / missing out=TableA; This creates TableA with variables: TRTPN, BNRIND, ANRIND, COUNT, PERCENT, etc. Also separate tables for BNRIND, ANRIND, TRTPN but they are not used later except for merging group counts.

They then create TableA with formatted ValueFmt = count (percent). Then merge with TRTPN dataset (renamed Count=GroupN Percent=GroupPercent) to get group size.

Then PROC TRANSPOSE to get columns for each ANRIND value, with prefix ValueFmt.

Thus final TableA (after transpose) has columns: GroupFmt, BNRIND, and for each ANRIND value a column named ValueFmt<ANRIND>. The values are formatted strings.

We need to replicate this.

Implementation details:

- Compute frequencies using dplyr summarise.

We need to compute for each TRTPN, BNRIND, ANRIND: count and percent within each TRTPN (i.e., percent of total within that TRTPN). In SAS, PROC FREQ with BY TRTPN and table BNRIND*ANRIND / missing out=TableA; The percent is row percent? Actually default is cell percent? For two-way table, percent is cell percent of total for that BY group. So percent = count / total count for that TRTPN * 100.

Thus we can compute:

total_by_trtpn = nrow of advsmax2 per TRTPN.

Then for each combination, count = n(), percent = count / total_by_trtpn * 100.

Then format ValueFmt as "count ( percent )" with percent one decimal (10.1 format). Use comma for count (comma8.) meaning thousands separator.

Implement round-half-up for percent to one decimal.

Define helper round_half_up(x, digits) = floor(x * 10^digits + 0.5)/10^digits.

Similarly for count formatting with commas.

GroupN = total_by_trtpn.

GroupFmt = paste0(TRTPN, " (N = ", format(GroupN, big.mark=",", scientific=FALSE), ")").

Then pivot_wider to have ANRIND values as columns with ValueFmt.

Need to ensure column names: prefix ValueFmt. In SAS, PROC TRANSPOSE with prefix=ValueFmt and id ANRIND. So column names become ValueFmt<ANRIND>. We'll replicate.

Now emergent high table:

- advsmax3: set advsmax2; if (CRIT1FL ne ""); n = mod(_n_,17) = 1; This is weird: they set n = (CRIT1FL ne ""); then they set n = mod(_n_,17) = 1; Actually they comment out first line? They have:

if (CRIT1FL ne "");
*n = (CRIT1FL="Y");
n = mod(_n_,17) = 1;

So they keep only rows where CRIT1FL not missing, then set n = (mod(_n_,17) == 1). So n is logical indicator for every 17th observation.

Thus advsmax3 contains rows where CRIT1FL not missing, and n is TRUE for every 17th row.

- PROC SUMMARY class TRTPN var n; output Nx = N (count of rows), sum=n, mean=percent.

So Nx = number of observations per TRTPN (i.e., total rows in advsmax3 per TRTPN). sum n = number of rows where n=TRUE (i.e., roughly Nx/17). percent = mean of n (i.e., proportion). Then later they multiply percent by 100.

- FishersExact: they run PROC FREQ with exact test, output ODS FishersExact. This yields a dataset with Name1, NValue1 (p-value). They then filter Name1 = "XP2_FISH". So we need to compute Fisher exact p-value for 2x2 table of TRTPN * n. Use fisher.test.

We need to create a table of counts: rows = TRTPN levels, columns = n (0/1). Use matrix.

Compute fisher.test(matrix) and extract p-value.

- Then merge EmergentHigh with PValue (FishersP). So we need to add column FishersP.

- Format percent as 6.1 (i.e., one decimal). Multiply by 100.

Now graph:

- Compute summary stats: mean of ANRLO ANRHI BASE AVAL, also min(AVAL) as MinValue, max(AVAL) as MaxValue. Actually they compute mean of ANRLO ANRHI BASE AVAL, and min(AVAL) and max(AVAL). Use summarise.

- Then compute YAxisMin and YAxisMax using MAJOR=10, margin = (maxvalue - minvalue)/100, YAxisMin = MAJOR * floor((minvalue - margin)/MAJOR), YAxisMax = MAJOR * ceiling((maxvalue + margin)/MAJOR). Use round-half-up? Not needed.

- Plot: scatter plot of AVAL vs BASE, colored by TRTPN (different symbols). Also reference lines vref = ANRLO, ANRHI (vertical reference lines) and href = ANRLO, ANRHI (horizontal reference lines). Also plot2: Ref vs base overlay (no axis). In R, we can use ggplot2: base on x-axis = BASE, y = AVAL, color = factor(TRTPN), shape = factor(TRTPN). Add geom_point. Add geom_hline at y = ANRLO, ANRHI with colors blue red. Add geom_vline at x = ANRLO, ANRHI with colors blue red. Also add line for Ref vs base: that's a line connecting points (BASE, Ref) for each subject? Actually Ref = Base, so it's a 45-degree line (y = x). So we can add geom_line with data where y=Ref, x=BASE, group=1, linetype maybe. Since Ref equals Base, it's diagonal line.

Set y limits to YAxisMin, YAxisMax, breaks by MAJOR.

Save plot as "sasgraph.png". Also save plotted data: the data used for scatter (advsmax2) with columns USUBJID, TRTPN, BASE, AVAL, Ref.

Now output tables:

- TableA final transposed: write CSV and text rendering. For text rendering, we need to produce a plain-text table similar to PROC REPORT output. Could just write using write.table with sep="\t". We'll produce a simple text representation.

- EmergentHigh table: columns TRTPN, Nx, n, percent, FishersP. Write CSV and text.

- Also maybe need to output P053.lst (the TableA report). We'll name file "P053.txt" and "P053.csv". Also maybe "Target16.txt" and "Target16.csv" for emergent high.

Implement round_half_up function.

Implement formatting functions.

Now produce script.

Edge Cases: missing values: need to include missing categories in frequencies. In dplyr, we can treat NA as a level using factor with include.na = TRUE. Use tidyr::replace_na? For counting, we can use count with .drop = FALSE.

We'll convert BNRIND and ANRIND to character, keep NA as "(Missing)"? SAS prints missing as . and includes in table. For simplicity, we can treat NA as "Missing". But to match numbers, we need to count missing as separate category. We'll use dplyr::count with .drop = FALSE and treat NA as "Missing". Use replace_na.

Implement:

advsmax2 <- advsmax %>% filter(PARAMCD=="DIABP", ATPTN==815, ANL01FL=="Y", ADY>1, SAFFL=="Y") %>% select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, ANL01FL, BNRIND, ANRIND, SHIFT1, CRIT1FL, ANRLO, ANRHI, BASE, Ref = BASE) %>% mutate(Ref = ifelse(!is.na(BASE) & !is.na(AVAL), BASE, NA))

But they set Ref = Base only if not missing base and aval. So same.

Now frequencies:

freq_df <- advsmax2 %>% mutate(BNRIND = as.character(BNRIND), ANRIND = as.character(ANRIND)) %>% replace_na(list(BNRIND=".", ANRIND=".")) %>% group_by(TRTPN, BNRIND, ANRIND) %>% summarise(count=n(), .groups='drop')

total_by_trtpn <- advsmax2 %>% group_by(TRTPN) %>% summarise(total=n(), .groups='drop')

freq_df <- freq_df %>% left_join(total_by_trtpn, by="TRTPN") %>% mutate(percent = count/total*100)

Now format ValueFmt: count with commas, percent with one decimal.

format_count <- function(x) formatC(x, format="d", big.mark=",")
format_percent <- function(x) sprintf("%.1f", round_half_up(x,1))

ValueFmt = paste0(format_count(count), " (", format_percent(percent), "%)")

Now group counts: GroupN = total, GroupFmt = paste0(TRTPN, " (N = ", format_count(GroupN), ")").

Now pivot_wider:

library(tidyr)
tableA_wide <- freq_df %>% mutate(ValueFmt = ValueFmt) %>% select(TRTPN, BNRIND, ANRIND, ValueFmt) %>% pivot_wider(names_from=ANRIND, values_from=ValueFmt, names_prefix="ValueFmt")

Add GroupFmt column by joining group info.

Now emergent high:

advsmax3 <- advsmax2 %>% filter(!is.na(CRIT1FL) & CRIT1FL!="") %>% mutate(row_num = row_number()) %>% mutate(n = (row_num %% 17) == 1)

summary_emergent <- advsmax3 %>% group_by(TRTPN) %>% summarise(Nx = n(), n = sum(n), percent = mean(n), .groups='drop')

percent multiplied later.

Fishers test:

Create contingency table: rows TRTPN, columns n (0/1). Use table.

cont_table <- table(advsmax3$TRTPN, advsmax3$n)

If more than 2 rows, fisher.test works for Rx2.

pval <- fisher.test(cont_table)$p.value

Create PValue data frame with FishersP = pval.

Merge.

Now format percent: percent*100, round_half_up to 1 decimal.

Now write outputs.

Implement writing CSV with write.csv(..., na = "")

Write text rendering: use capture.output(print(df, row.names=FALSE)) and writeLines.

For TableA, we need to order by GroupFmt then BNRIND as in SAS (by GroupFmt BNRIND). Ensure sorting: arrange(GroupFmt, BNRIND).

Now script.

Let's craft final R script.
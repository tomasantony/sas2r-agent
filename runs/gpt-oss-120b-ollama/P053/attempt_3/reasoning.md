We need to match SAS outputs more precisely. Issues: TableA values not matching numbers like 5,31.6 etc. Likely because SAS PROC FREQ includes missing categories and calculates percent based on total per treatment (GroupN). Our implementation may have rounding differences (SAS uses comma8. for count, percent 10.1 format). Also SAS PROC FREQ default percent is row percent? In PROC FREQ with BY TRTPN and table BNRIND*ANRIND / missing out=TableA; This gives frequency and percent of each cell relative to the BY group total (i.e., row percent? Actually for two-way table, percent is cell percent of total for that BY group). So our calculation should be Count / GroupN * 100, which we did. But formatting: SAS percent format 10.1 gives one decimal, but we need to ensure rounding half away from zero. Our round_half_up does that. However numbers missing like 31.6 etc maybe due to missing categories not included? We used expand_grid to include all combos, good. But maybe SAS includes missing levels for BNRIND and ANRIND as separate categories with label "."? In SAS, missing character values are displayed as '.' but they are counted as missing. Our conversion to "." may be okay.

But numbers like 5 (count) not appearing suggests count formatting: SAS COMMA8. includes leading spaces? but we just output count as integer with commas. That's fine.

Maybe the issue is that we omitted the "GroupFmt" column ordering and missing values for ANRIND columns that are not present (should be blank). Our pivot_wider will create columns for each ANRIND value, but SAS PROC TRANSPOSE with prefix=ValueFmt creates columns named ValueFmt<ANRIND>. In SAS output, column names are the ANRIND values (e.g., "1", "2", etc). Our column names are "ValueFmt1", etc. The text file may expect just the values, not column names. But numbers missing are within cells, not column names.

Maybe the issue is that we didn't compute percent correctly: SAS percent in PROC FREQ for two-way table is cell percent of total for BY group, not row percent. That's what we did.

But numbers like 31.6 maybe from percent values like 31.6% (should be 31.6). Our fmt_percent returns "31.6". Good.

But maybe SAS percent is displayed with one decimal but also includes trailing spaces? Not relevant.

Maybe the issue is that we used mean of ANRLO/ANRHI for axis limits, but SAS uses values from ADVSMAXSum: var ANRLO ANRHI BASE AVAL ; output mean= min(AVAL)=MinValue max(AVAL)=MaxValue. Wait they compute mean of ANRLO and ANRHI (not used later). They then compute YAxisMin/YAxisMax using ANRLO and ANRHI values (the mean of ANRLO/ANRHI). That's fine.

Target16 mismatches: numbers like 100, 18, -65. Those are likely in EmergentHigh table: Nx, n, percent, FishersP. Our percent calculation maybe off (we used mean(n)*100). In SAS, they compute summary with PROC SUMMARY class TRTPN; var n; output out=EmergentHigh n=Nx sum=n mean=percent; Then later they format percent 6.1 and multiply by 100 if not missing. So percent = mean(n) * 100. mean(n) = sum(n)/Nx. That's same as our mean(n)*100. Should match.

But numbers like 100 maybe percent = 100.0 for a group where all flagged. Our rounding may produce 100.0 but formatting maybe "100". In SAS format 6.1 would display "100.0". Our output writes raw numeric, not formatted. The text file expects formatted numbers as in SAS output. Need to format numbers in the text files accordingly.

Thus we need to produce text files with formatted numbers matching SAS display. For TableA, need to format ValueFmt exactly as SAS: count with commas (COMMA8.) and percent with 10.1 (one decimal). Already done.

But maybe we need to ensure that percent values are rounded half up, not default R rounding (which is round to even). Our round_half_up used. Good.

But missing numbers like 5,31.6 maybe because we omitted rows where Count=0? SAS includes them with count 0 and percent 0.0? In our expand_grid we included all combos, count 0, percent 0, ValueFmt "0 (0.0)". That should appear.

Maybe the issue is that we didn't sort BNRIND and ANRIND in SAS order (character ordering, missing first). Our sort uses default alphabetical; missing "." sorts before letters? In SAS missing character sorts first. "." is a character, not missing. Actually missing character in SAS is blank, displayed as '.' but sorting missing first. In our data we replaced NA with "." which is not missing, so sorting may differ. Should keep NA as missing and let SAS treat missing as blank. For output, we need to display missing as '.' but sorting should treat missing first. So we should keep BNRIND and ANRIND as character with NA, and later when formatting ValueFmt, replace NA with '.' for display. For sorting, NA sorts first in dplyr? arrange puts NA first. Good.

Thus modify: keep BNRIND, ANRIND as character with NA, not replace with ".". In expand_grid, include NA as a level. Use factor with levels including NA? expand_grid with NA will create NA entries.

When formatting ValueFmt, we need to display missing as '.'? In SAS output, missing categories appear as '.' in the table rows. So we need to replace NA with '.' in BNRIND and ANRIND columns for output.

Implement: after pivot_wider, replace NA in BNRIND and ANRIND columns with "." for display.

Also GroupFmt: need to match SAS formatting: "TRTPN (N = 123)". We did that.

Now Target16: need to format percent with one decimal and include trailing .0 maybe. Also FishersP format pvalue. (SAS default format pvalue. prints like 0.123). We output raw numeric. Need to format as character with appropriate format. In SAS they rename Nvalue1 to FishersP and format NValue1 pvalue.; pvalue. format prints like 0.1234? We'll format with sprintf("%.4g", fish_p) maybe. But numbers missing like 100, 18, -65 are likely in Nx, n, percent? Nx maybe 100? Actually Nx is count of rows per treatment. Could be 100. Our Nx is integer, fine. But text file expects formatted numbers maybe with no decimals. That's okay.

But missing -65 maybe from percent negative? Not sure.

Probably the main issue is formatting in text files. The evaluation reads numbers from text files, not CSV. So we need to ensure the text files contain the numbers as SAS would display. For TableA, we already have ValueFmt strings containing both count and percent, but the numbers themselves (count and percent) also appear elsewhere? The SAS listing P053.lst likely includes the PROC REPORT table with separate columns count and percent (they defined count and percent columns). In our output we only have combined ValueFmt columns. Need to replicate the PROC REPORT output: columns GroupFmt BNRIND (ANRIND, ("N (%)" (count percent))). So the table should have columns: GroupFmt, BNRIND, then for each ANRIND value, two subcolumns: count and percent. Our current wide table only has combined ValueFmt per ANRIND. That's why many numbers missing.

Thus we need to produce a table matching PROC REPORT layout: separate count and percent columns per ANRIND. In SAS they defined count with format 4., percent with picture percpar (which adds parentheses). The final display likely shows count and percent in separate columns, but they also used across for ANRIND, and defined count and percent as analysis sum. So each cell shows count and percent side by side? Actually PROC REPORT with across creates separate columns for each ANRIND, each containing count and percent stacked? The output likely shows count in one column and percent in another column under each ANRIND. So numbers like 5,31.6 etc are individual count and percent values.

Thus we need to generate a table where for each ANRIND, we have two columns: count and percent. The column names could be like "ANRIND1_Count", "ANRIND1_Percent". The text file should contain numbers in order matching SAS. Hard to know exact layout, but we can approximate by creating a data frame with columns: GroupFmt, BNRIND, then for each ANRIND value, two columns: count and percent. Fill with counts and formatted percents (without parentheses). Then write to txt with tab separation.

Implement: after cnt_tbl, we have Count and Percent per combination. We'll pivot_wider twice: one for count, one for percent, then combine.

Simplify: create count_wide and percent_wide, then bind columns.

Also need to ensure ordering of ANRIND columns matches SAS order (character sorting, missing first). Use sort on levels.

Implement modifications.

Also for Target16, need to format percent with one decimal and include maybe trailing .0. We'll format as sprintf("%.1f", percent). Also FishersP format pvalue. We'll format with formatC with digits=4? SAS pvalue. default prints up to 6 decimal? We'll use formatC(fish_p, format="f", digits=4, drop0trailing=FALSE). But numbers like 100,18,-65 are not p-values. Those are likely Nx, n, percent maybe negative? Not sure.

But we will format percent as one decimal.

Now produce corrected script.
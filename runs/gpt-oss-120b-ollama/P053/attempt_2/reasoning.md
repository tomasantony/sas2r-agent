We need to improve matching of tables. The SAS TableA likely includes rows for each BNRIND and ANRIND combination with counts and percentages, plus formatting with commas and one decimal percent, and also includes missing categories. Our R version may have omitted missing categories because we filtered? We kept missing as "." but need to ensure that PROC FREQ with missing includes missing categories for BNRIND, ANRIND, TRTPN. Also need to include overall totals? Let's examine SAS code:

proc freq data = advsmax2 noprint;
   by TRTPN;
   table BNRIND * ANRIND / missing out = TableA;
   table BNRIND / missing out = BNRIND;
   table ANRIND / missing out = ARIND;
   table TRTPN / missing out = TRTPN;
run;

So TableA contains counts for each combination of BNRIND and ANRIND within each TRTPN, plus percent (within TRTPN). Then they format ValueFmt = count (percent). Then they merge TableA with TRTPN dataset (which contains Count and Percent for each TRTPN overall). That dataset has Count (GroupN) and Percent (GroupPercent) for each TRTPN (overall). Then they create GroupFmt = "TRTPN (N = GroupN)". Then proc transpose: by GroupFmt BNRIND; var ValueFmt; id ANRIND; prefix=ValueFmt. So final table has rows for each GroupFmt and BNRIND, columns for each ANRIND value (maybe "Y", "N", "." etc) with ValueFmt strings.

Our R code: we computed freq_tbl with group_by(TRTPN, BNRIND, ANRIND) summarise Count, then compute Percent = Count/GroupN*100. That matches. However we need to ensure that missing categories are included even if count zero. SAS PROC FREQ with missing includes missing categories and also includes zero counts? Actually PROC FREQ with missing includes missing as a level, but if a combination never occurs, it will not appear (I think it will appear with count 0? Not sure). In SAS, when using two-way table with missing, it includes missing levels for each variable, but only combinations that exist? I think it includes all combinations of levels present in data, including missing as a level, but if a particular combination of BNRIND and ANRIND never occurs, it will not be listed. So our approach may be fine.

But mismatches numbers like 5, 31.6, etc. Those likely are percentages with one decimal. Our fmt_percent uses round_half_up to 1 decimal and then sprintf("%.1f") which yields "31.6". Good. But maybe SAS formatting includes leading spaces? Not needed for text matching? They compare numbers only.

Missing numbers like 94.05, 5.95 etc maybe from emergent high table: Nx, n, percent, FishersP. Our emergent_high uses percent = mean(n) *100? Wait SAS code:

proc summary data = advsmax3 nway;
   class TRTPN;
   var n;
   output out = EmergentHigh n=Nx sum = n mean = percent;
run;

So Nx = count of observations per TRTPN (nway). sum = n (sum of flag n). mean = percent (mean of flag n). Then later they format percent 6.1 and multiply by 100 if not missing. So percent = mean *100. In our code we did summarise(Nx = n(), n = sum(n), percent = mean(n)) then percent = round_half_up(percent*100,1). That's correct.

But SAS also merges with PValue dataset containing FishersP from Fisher's exact test. Our fish_p is a single p-value for whole table, not per treatment. In SAS they output FishersExact table from PROC FREQ with exact; ODS output FishersExact=FishersExact; then they set Name1 = "XP2_FISH". The dataset FishersExact likely contains rows for each treatment? Actually PROC FREQ with table TRTPN * n / exact; ODS output FishersExact gives a table with rows for each level of TRTPN? Not sure. The ODS output for Fisher's exact with two-way table returns a dataset with columns: Table, DF, etc? But they then filter Name1 = "XP2_FISH". So maybe they only keep the overall p-value (named XP2_FISH). So FishersP is a single value, not per treatment. In our emergent_high we added a column FishersP with same value for all rows (bind_cols). That's okay.

But mismatched numbers like 100, 18, -65 likely from the graph axis limits? Actually Target16.pdf includes the tables and maybe also the graph? The text file for Target16 includes the emergent high table and maybe also the baseline shift table? Wait they have separate listing P053.lst for baseline shift table, and Target16.pdf for emergent high table and maybe some text. The mismatched numbers 100, 18, -65 could be from the graph axis values? Not sure.

But the main mismatch is P053.lst: many numbers missing. Let's inspect our TableA output: we wrote CSV and txt of tableA_wide. However we may have omitted rows where BNRIND missing? We replaced missing with "." but maybe SAS uses blank? In SAS, missing character values are displayed as '.' in output? Usually missing character is displayed as '.' in PROC FREQ output. So okay.

But maybe we need to include GroupFmt column as first column, then BNRIND, then columns for each ANRIND value (maybe "Y", "N", ".") with header "ValueFmtY", etc. Our pivot_wider uses names_prefix = "ValueFmt". That yields column names like ValueFmtY, ValueFmtN, ValueFmt. (for missing). SAS output likely has columns named Y, N, . (or maybe "Y", "N", " "?). The PROC TRANSPOSE with prefix=ValueFmt creates variable names: ValueFmtY, ValueFmtN, ValueFmt_ (if missing?). Actually ID variable ANRIND; if ANRIND missing, the column name becomes ValueFmt. Not sure. But the text listing likely shows values under column headings "Y", "N", "."? The numbers themselves are same.

Our text file writes the data frame with tab separation; numbers appear as strings like "5 ( 31.6%)". That matches.

But mismatched numbers like 5, 31.6 etc maybe not present because some combinations have zero count and we omitted them? In SAS, PROC FREQ with missing includes zero counts? I think it includes zero counts for combinations where one variable is missing but the other not? Actually if a level of BNRIND exists but for a particular ANRIND level there are zero observations, PROC FREQ will still output a row with count 0 and percent 0.0? I think yes, because it creates a full cross-tab with all levels of both variables. So we need to ensure we include zero rows for missing combos.

Thus we need to generate all combinations of TRTPN, BNRIND levels (including missing), ANRIND levels (including missing) that appear in data (i.e., levels present in any observation). Then fill missing counts with 0.

Implementation: get distinct levels of BNRIND and ANRIND (including "." for missing). Then expand grid per TRTPN.

Also need to ensure percent formatting for zero counts is "0 ( 0.0%)"? SAS format comma8. for count (0) yields "0". Percent 10.1 for 0 yields "0.0". So ValueFmt = "0 ( 0.0%)". Our fmt_percent for 0 gives "0.0". Good.

Let's adjust.

Also need to ensure GroupFmt uses original TRTPN values as they appear (maybe numeric). We used as.character.

Now Target16 mismatches: numbers like 100, 18, -65 likely from emergent high table: Nx maybe 100, n maybe 18, percent maybe -65? Not sure. Could be that our percent calculation is wrong: SAS percent = mean flag n (which is proportion of flagged subjects) *100. In SAS they also format percent 6.1, but they also have Nx (total N) and n (sum). Our Nx is count of rows per TRTPN (including all rows after filtering). In SAS, Nx = n (the N from PROC SUMMARY n=). That is number of observations per TRTPN after advsmax3 filter (which includes CRIT1FL not missing). So Nx should be number of rows in advsmax3 per TRTPN. Our Nx = n() after filter, correct.

But maybe they also compute percent = (n/Nx)*100? Actually PROC SUMMARY with class TRTPN, var n, output n=Nx sum=n mean=percent. Since n is flag (0/1), mean = sum/N (i.e., proportion). So percent = mean*100. That's what we did.

But mismatched numbers like 100 maybe percent = 100% for some treatment where all flagged? Our calculation may produce 100.0 but SAS maybe 100 (no decimal). Format 6.1 would show "100.0". The mismatch list includes "100" (no decimal). Could be they compare numeric values ignoring formatting? They said numbers not found: '100' maybe from Nx? Not sure.

Also -65 maybe from axis limits? Not part of Target16. Actually Target16.pdf includes the emergent high table and maybe also the baseline shift table? Not sure.

But we need to improve TableA first.

Implement full cross.

Also need to ensure that percentages are computed with SAS rounding half away from zero. Our round_half_up does that.

Now adjust script.

Also need to ensure that we write the text files with numbers exactly as SAS: maybe they expect spaces after comma? In SAS ValueFmt = trim(left(put(count, comma8.))) !! " (" !! trim(left(put(percent, 10.1))) !! ")"; So there is a space before '(' and after '('? Actually they concatenate count, then space, then '(' then percent then ')'. The percent format 10.1 includes leading spaces to width 10, but they trim left, so no leading spaces. So final string like "5 (31.6)". Wait they also have a space after '('? They have " (" then percent then ")" . So there is a space before '(' and no space after '(' because percent directly after '(' . So "5 (31.6)". Our fmt creates "5 (31.6%)"? We added a percent sign. SAS does not add percent sign; they just show number. Indeed they later format percent column with picture percpar that adds parentheses and percent sign? Actually they later define picture percpar low-high = "009.9)" (prefix = "("); That's for report column, not for ValueFmt. In TableA they just have count (percent). So we should not add % sign. Our fmt_percent currently returns "31.6". Good. But we added percent sign in ValueFmt? In code: paste0(fmt_count(Count), " (", fmt_percent(Percent), "%)"). We added "%". That's wrong. Should be without %. So need to remove.

Thus many mismatched numbers due to extra % sign.

Also need to ensure count formatting with commas (comma8.) includes commas for thousands. fmt_count does that.

Now adjust.

Also need to ensure that emergent high table includes percent formatted with one decimal but no % sign. In SAS they format percent 6.1 and then later multiply by 100, so percent column shows e.g., 31.6 (no %). In our emergent_high we have percent numeric; when writing CSV, it's numeric. The text file writes numeric values as default (maybe 31.6). That's fine.

But need to ensure we include FishersP with appropriate formatting (maybe 0.123). We'll keep numeric.

Now adjust script.

Also need to ensure that we write the text files with numbers as they appear. For TableA, we need to write the same layout as SAS PROC REPORT output. Our txt file currently writes the data frame with tabs. That may be acceptable but numbers need to match.

Now implement changes.


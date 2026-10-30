Need to translate SAS program P072_t_dm.sas to R.

We have inputs: adam libref -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam
We need to read adam.adsl. Likely .sas7bdat or .xpt? Check directory. Use haven::read_sas or read_xpt. Need to handle both.

Steps:

1. Get population counts: m_counts with TRTA, SAFFL=Y. Compute bign per TRTA and total.

2. Prepare dm dataset: filter SAFFL='Y', derive AGEGR1, SEXN, RACEN, ETHNICN (though not used except ordering? But cat stats ordering? In SAS, m_cat_stats orders by catvar, trtvar. So ordering is alphabetical. But we need to reproduce table.

3. Continuous variables: Age, Weight, Height, BMI. Using m_cont_stats: proc means nway with class TRTA. Compute n, mean, std, median, min, max, q1, q3. Then format:

- n: strip(put(n,8.)) -> integer as string no decimals
- Mean: put(mean,12.dp1) where dp1 = decimals+1, decimals=1 default => 2 decimals? Wait macro cont_block decimals=1 default. So mean with 2 decimals? Actually dp1 = decimals+1 =2. So mean 12.2
- SD with 2 decimals
- Median with 1 decimal
- Q1,Q3 with 1 decimal combined "q1, q3"
- Min, Max with 1 decimal combined

Need to replicate SAS rounding half away from zero. Use helper.

Also need to handle missing: if n>0 etc.

4. Categorical: Sex, Race, Ethnicity, Age Group. m_cat_stats: count distinct USUBJID per TRTA, catvar. Then merge with bign to compute pct = cnt/bign*100. Format value = cat(cnt, ' (', pct, ')') where pct format 5.1 (one decimal). If cnt>0 else '0'. Note SAS put(pct,5.1) includes leading spaces? But strip. So "12 ( 34.5)"? Actually strip removes leading spaces. So "12 (34.5)"? Need to check expected output. The spec says reproduce SAS formatting "12 ( 34.5%)" etc. But m_cat_stats value = cat(strip(put(cnt,8.)), ' (', strip(put(pct,5.1)), ')') . So no % sign. Wait SAS code: value = cat(strip(put(cnt, 8.)), ' (', strip(put(pct, &pctfmt.)), ')'); No percent sign. So value like "12 (34.5)". But table may add %? Need to check expected t_dm.rtf numbers: 176 numbers. We'll produce as SAS does.

5. Combine all_stats: set _c_age _f_sex _f_race _f_ethnic _c_weightbl _c_heightbl _c_bmibl _f_agegr1

Note order: block ord stat. Blocks:
1 Age (years) continuous
2 Sex categorical
3 Race
4 Ethnicity
5 Weight (kg)
6 Height (cm)
7 BMI (kg/m2)
8 Age Group

Each continuous block has 6 rows? Let's see: n, Mean, SD, Median, Q1,Q3, Min,Max => 6 rows (n, Mean, SD, Median, Q1,Q3, Min,Max) Actually 6 rows. Categorical blocks have variable number of levels.

6. Transpose: proc transpose by block category ord stat id TRTA var value. So columns colPlacebo etc. In SAS they rename colPlacebo=col1 etc. But actual TRTA values? Need to inspect ADSL TRTA values. Likely "Placebo", "Drug X Low Dose", "Drug X High Dose" ? The trt labels: trt1lbl Placebo, trt2lbl [Drug] Low Dose, trt3lbl [Drug] High Dose. But actual TRTA values may be "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose" or similar. The transpose prefix=col and id TRTA will create columns named col<sanitized TRTA>. The SAS patch renames colPlacebo, colDrug_X_Low_Dose, colDrug_X_High_Dose to col1-3. So we need to replicate: create wide table with columns per TRTA sorted by TRTA (character ordering uppercase before lowercase). Then map to col1,col2,col3 in that order.

7. Report dataset: retain block category stat col1 col2 col3; set final; by block category; display_cat = category on first.category else ' '; stat = '  '||strip(stat) (indent).

8. Generate output: proc report with columns block display_cat stat col1 col2 col3. Headers: col1hdr etc from m_counts: "Placebo (N=xx)" etc. Need to compute.

We must write outputs to OUT_DIR: t_dm.csv and t_dm.txt plus maybe? Expected outputs: t_dm.rtf -> write text rendering and CSV of same table. So we need to produce t_dm.csv and t_dm.txt (and maybe t_dm.rtf? But spec says write each output dataset as CSV, each table additionally as plain-text rendering and CSV of table body. So for t_dm we need to produce t_dm.csv (report dataset) and t_dm.txt.

Also need to handle rounding half away from zero.

We need to locate ADSL file. Let's assume path D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/adsl.sas7bdat or .xpt. We'll write code to search.

We need to implement helper functions: sas_round, format with decimals.

Also need to handle proc means calculations: n, mean, std, median, min, max, q1, q3. SAS PROC MEANS default definitions: Q1 and Q3 using? SAS default is? Need to replicate. SAS PROC MEANS uses empirical distribution? For Q1/Q3, SAS uses PCTLDEF=5 by default. That's empirical distribution with averaging. R quantile types: type=2? Let's check: SAS PCTLDEF=5 corresponds to R type=2? Actually SAS definitions: PCTLDEF=5 is empirical distribution function with averaging. R type=2 is similar but not exactly. Need to approximate. Could use quantile with type=2. Might be close enough. Alternatively use type=5? Let's recall: SAS PCTLDEF options: 1-5. Default is 5. R type definitions: type 1-9. Mapping: SAS PCTLDEF=5 corresponds to R type=2? Let's verify: SAS PCTLDEF=5: if p*n = j integer, average of x_j and x_{j+1}, else x_{ceil(p*n)}. R type=2: similar but with different handling. Actually R type=2 is "SAS definition" - averaging at discontinuities. So type=2 matches SAS PCTLDEF=5. We'll use type=2.

Also std: SAS uses sample std (n-1 denominator). R sd() does same.

Mean, median etc straightforward.

We need to produce final report ordering: by block ord stat. For continuous, ord 1-6. For categorical, ord? In m_cat_stats, no ord variable; but in cat_block they set block but not ord. Wait cat_block data step: set _f_&var.; length category $60 stat $40; category="&label."; stat=&var.; block=&ord.; So stat is the category value (e.g., "M", "F"). No ord. So when combining, all_stats has ord missing for categorical rows. Then proc sort by block ord stat; Missing ord sorts first (SAS missing first). So categorical rows will appear before continuous? But block ordering ensures block 2 etc. Within block, ord missing, so sort by stat alphabetical.

For continuous, ord 1-6, stat values "n","Mean", etc.

We need to replicate sort: by block, ord (with NA first), stat.

Then transpose by block category ord stat id TRTA var value. So each row is unique combination.

We need to implement.

Also need to handle display_cat logic: first.category within block category.

We need to produce text rendering: simple table with headers.

We should also output CSV of table body: columns display_cat, stat, col1, col2, col3.

We need to ensure OUT_DIR exists.

Let's craft R script.

Steps in R:

- out <- Sys.getenv("OUT_DIR"); if empty, use temp.

- Find ADSL file: list files in adam path.

- Read with haven.

- Compute bign.

- Derive dm.

- Define helper sas_format: format number with given decimals using round half away from zero.

Implement round_half_up: sign(x)*floor(abs(x)*10^n +0.5)/10^n

Then format: sprintf with decimals, but need to strip leading spaces. Use formatC? Use sprintf("%.1f", rounded) etc. For SAS put with width 12.dp, it right aligns but strip removes spaces. So just sprintf.

For n: as integer string: as.character(n)

For mean/sd: decimals dp1 =2 => sprintf("%.2f", rounded)

For median/q1/q3/min/max: 1 decimal => sprintf("%.1f", rounded)

For pct: 5.1 => one decimal => sprintf("%.1f", rounded)

Need to handle NA.

Implement cont_stats function.

Implement cat_stats function.

Proceed.

We need to handle TRTA ordering: sort unique TRTA values using SAS character ordering (uppercase before lowercase, ASCII). R default sort is similar? Might be locale dependent. Use sort with method? We'll use base sort which is byte-wise.

Compute bign data frame.

Create headers: paste0(TRTA, " (N=", bign, ")")

Now cont_block: for each var, compute stats per TRTA.

We need to handle missing values: proc means excludes missing.

Implement.

Then cat_block: count distinct USUBJID per TRTA and catvar. Use dplyr.

Note denom bign.

Then combine.

Then transpose: pivot_wider.

We need to map TRTA values to col1,col2,col3 in sorted order. So after pivot, rename.

Then create report data frame with block, display_cat, stat, col1,col2,col3.

Add display_cat logic: group by block, category, need to know first row per category. Since after sort, first row per category is first ord/stat. So we can group.

Also indent stat: paste0("  ", stat) but for CSV we keep as is? SAS does '  '||strip(stat). So two spaces prefix.

We need to produce final report sorted by block, ord, stat.

We also need to handle missing values in wide table: if no subjects in category for a treatment, value is missing -> should be "0" ? Actually cat_stats only creates rows where cnt>0. So if a treatment has zero count for a category, there will be no row for that TRTA, so after transpose that cell will be NA. SAS would show missing (blank). But m_cat_stats creates only existing combos; transpose will have missing for absent. In report, missing should be displayed as "0" ? But SAS code would leave missing as blank, but they set value='0' when cnt=0, but that row wouldn't exist. So missing remains blank. However typical table would show "0". But we follow SAS: leave blank (NA -> "" in CSV). We'll fill NA with "".

For continuous, all TRTA have values, so no missing.

Now produce outputs:

- write.csv(report, file.path(out, "t_dm.csv"), row.names=FALSE, na="")

- Also produce t_dm.txt: plain text rendering with headers and rows.

We need to include 176 numbers? The text should contain same numbers.

We'll create simple fixed-width text.

Also need to ensure dates formatting not needed.

Let's craft script.


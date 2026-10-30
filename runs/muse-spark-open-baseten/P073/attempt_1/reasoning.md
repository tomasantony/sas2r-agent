Need to translate SAS program P073_t_exposure.sas to R.

We need to understand inputs: adam library at D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam . Need to list files. Likely ADSL, ADEX etc. But we don't have actual filesystem here, but we must write R script that reads from those paths. The script must run with OUT_DIR env var.

We need to reproduce table: Summary of Study Drug Exposure. Steps:

- m_counts: Big N per TRTA where SAFFL=Y. Also total.

- Derive exposure data: ex from adex join adsl where SAFFL=Y. But then alternative: ex_adsl from adsl where SAFFL=Y, compute TRTDUR = TRTEDT - TRTSDT +1 if both non-missing.

- Then m_cont_stats: descriptive stats for TRTDUR by TRTA. decimals=1. So stats: n, Mean, SD, Median, Q1,Q3, Min,Max. Need to replicate SAS PROC MEANS behavior. Need to check definitions: PROC MEANS with Q1, Q3 uses default PCTLDEF=5? Actually SAS default is PCTLDEF=5. Need to replicate. But we can approximate using quantile type 2? Let's see typical translation: SAS PCTLDEF=5 uses empirical distribution. R quantile types: type 2 is similar to SAS PCTLDEF=5? Let's recall: SAS PCTLDEF=5: uses (n+1)*p . R type 6? Let's check: SAS definitions:

- PCTLDEF=5: x_p = (1-g)*x_j + g*x_{j+1} where j = floor(n*p), g = n*p - j . Actually with different handling. R type 1-9.

Common mapping: SAS PCTLDEF=5 corresponds to R type=2? Let's search memory: SAS PCTLDEF=5 is equivalent to R type 2? Another source says SAS default PCTLDEF=5 corresponds to R quantile type=2? Let's assume type=2.

But we need to ensure numbers match expected 58 numbers. The table has: For each TRTA (3 groups), we have stats: n, Mean, SD, Median, Q1,Q3, Min,Max => that's 6 rows? Actually list: n, Mean, SD, Median, Q1,Q3, Min,Max => 6 rows (Q1,Q3 combined, Min,Max combined). So 6 rows *3 cols =18 numbers. Plus duration categories: 6 categories? Let's see: <=28, >28 to <=84, >84 to <=168, >168 to <=336, >336, Missing => 6 categories *3 =18 numbers (each n (%)). Total 36 numbers. But expected 58 numbers suggests maybe includes Big N headers? Or maybe includes total? But SAS program only has 3 columns (Placebo, Low, High) no total. So 36 numbers. Why 58? Maybe includes percentages and counts separately? The description says 58 numbers in text rendering. Could be includes Big N values (3) plus stats.

We need to produce text rendering and CSV.

We need to implement m_cont_stats formatting:

- n: strip(put(n,8.)) => integer no decimals
- Mean: put(mean,12.1) with one extra decimal? Actually decimals=1, dp1=2? Wait code: dp = decimals =1, dp1 = decimals+1 =2. Mean and SD use dp1 (2 decimals). Median, Q1,Q3, Min,Max use dp (1 decimal). Let's check: Mean (dp1) => 12.2? Actually code: put(mean,12.&dp1.) => 12.2 => 2 decimals. SD same. Median 12.1 => 1 decimal. Q1,Q3 catx with 12.1 each. Min,Max same.

So we need to format accordingly.

- For n>0 else blank.

- SD only if n>1 else blank.

- Q1,Q3 and Min,Max combined strings.

We need to replicate SAS rounding half away from zero. R's round is bankers. Need helper.

Also need to handle missing TRTDUR.

Second part: duration categories counts: cnt distinct USUBJID per TRTA, DURCAT. Then pct = cnt / bign *100. Format value = cat(cnt, ' (', pct 5.1, ')') . If cnt>0 else '0'. Note pct format 5.1 includes leading spaces but strip removes.

We need to get bign from m_counts.

Then combine all_exp = _dur_stats + _durcat_fmt. Then sort by block ord stat. Then transpose by block category ord stat id TRTA var value. So final has one row per stat/category.

Columns: block, category, ord, stat, col1, col2, col3 where col1 corresponds to Placebo, col2 Low, col3 High. Need to map TRTA values to columns. In SAS, TRTA values are like "Placebo", "[Drug] Low Dose", "[Drug] High Dose" ? But transposed column names are colPlacebo etc. The patch renames to col1 col2 col3. So we need to ensure ordering by TRTAN? In m_counts, order by TRTA (character). That would be alphabetical: "[Drug] High Dose" < "[Drug] Low Dose" < "Placebo" ? Let's check ASCII: '[' =91, 'P'=80? Actually 'P' 80, '[' 91, so Placebo (80) comes before '[' (91). So order would be Placebo first, then High, Low? But SAS program's ex ordering by TRTAN, and final report expects col1=Placebo, col2=Low, col3=High. So they patched transpose to rename colPlacebo=col1 etc. That suggests TRTA values are exactly "Placebo", "Drug X Low Dose" etc? Let's inspect synthetic data. We need to infer.

We should read ADSL to see TRTA values. Since we can't run here, we need to write R code that dynamically handles.

Simpler: In R, we will compute bign ordered by TRTAN if available, else TRTA. And then create columns in order of TRTAN ascending. That should give Placebo (TRTAN=0?), Low=1, High=2? Let's check typical CDISC pilot: TRT01P values: Placebo, Xanomeline Low Dose, Xanomeline High Dose with TRT01PN 0,1,2. So TRTAN similar.

We need to produce headers: "Placebo (N=xx)" etc.

We need to write outputs: t_exposure.rtf? But spec says write text rendering and CSV. Expected outputs: t_exposure.rtf -> write text rendering and CSV of same table. So we need to create t_exposure.txt and t_exposure.csv (and maybe rtf?). The instruction says each table additionally as plain-text rendering (file.path(out, "<name>.txt")) and as CSV of table body. So for t_exposure, we need t_exposure.txt and t_exposure.csv. Also maybe t_exposure.rtf? But we can create txt.

We need to follow rules: write every output into OUT_DIR: each output dataset as CSV, each table as txt and CSV, each figure as PNG plus CSV.

So we need to produce at least t_exposure.csv and t_exposure.txt.

We should also produce formatted table similar to SAS PROC REPORT.

We need to handle reading .xpt or .sas7bdat? The libref points to directory with synthetic adam. Could be .sas7bdat files. Use haven::read_sas or read_xpt depending on file extension. We'll write helper to list files in adam path and read.

We need to implement round half up.

We need to handle dates: TRTEDT, TRTSDT are numeric SAS dates (days since 1960-01-01). haven will read as Date? Possibly as numeric with format. We'll need to handle both.

Compute TRTDUR = TRTEDT - TRTSDT +1.

We need to compute descriptive stats: n = number of non-missing TRTDUR? PROC MEANS n is number of non-missing values. For ex_adsl, each subject has one TRTDUR, so n = count non-missing per TRTA. Should match bign minus missing.

Mean, std, median, min, max, q1, q3.

We need to implement SAS PROC MEANS Q1/Q3 with PCTLDEF=5. We'll use quantile type 2.

We also need to handle formatting with round half up.

We need to produce final data frame with columns: block, category, ord, stat, col1, col2, col3. But for CSV output, we probably want human readable: Category, Statistic, Placebo, Low Dose, High Dose.

The spec says write CSV of table body with same numbers SAS shows. So we should output final table as displayed.

We need to also handle missing categories: If a category has zero count for a treatment, we need to show "0" (as per code). The SQL left join ensures missing combos? Actually _durcat_cnt only includes combos that exist. If a TRTA has zero subjects in a category, that row won't exist, and after transpose, that cell will be missing (NA). In SAS, that would be blank. But we should fill missing with "0" ? Let's see code: _durcat_fmt only has rows for existing combos. After transpose, missing cells become blank. But SAS report would show blank, not "0". However spec says counts with 0 should be "0". But if no subjects in category, cnt would be 0 but not in table. However proc sql count distinct would not create row for zero. So blank vs 0 difference. We should ensure we create all combinations and fill 0.

Better to create full grid of TRTA x DURCATN and left join.

We need to implement that to match SAS? SAS would leave blank, but we can fill with "0" to be more correct. Let's check expected numbers: 58 numbers suggests maybe blanks are not counted.

We need to write R script that does steps close to SAS.

We should also handle reading ADSL and ADEX. The SAS program creates ex but not used; uses ex_adsl. So we can ignore ADEX.

We need to produce text file with titles/footnotes.

Let's craft R script.

Steps in R:

- out <- Sys.getenv("OUT_DIR"); if empty, use temp.

- root etc paths: adam_path = "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

- helper read_adam: list files, find adsl.* (case insensitive). Use haven::read_sas if .sas7bdat, read_xpt if .xpt.

- helper round_half_up, fmt functions.

- Compute bign.

- Compute ex_adsl.

- Compute cont stats.

- Compute dur_cat.

- Compute counts.

- Combine, transpose.

- Generate final display.

- Write CSV and TXT.

We need to ensure column headers include N.

We need to produce 58 numbers: Let's count numbers in table: For each of 3 treatments, we have:

- n: 1 number
- Mean: 1
- SD: 1
- Median:1
- Q1:2 numbers (Q1 and Q3)
- Min,Max:2 numbers
=> 8 numbers per treatment? But displayed as combined strings, but numbers count maybe counts each numeric value. So per treatment: n(1)+Mean(1)+SD(1)+Median(1)+Q1(1)+Q3(1)+Min(1)+Max(1)=8 numbers. *3=24.

- Duration categories: each category has n and % => 2 numbers per cell *6 categories*3=36. Total 60. Close to 58. Maybe Missing category has 0 subjects, so 2 numbers missing? That would be 58. So plausible.

We need to ensure we produce same.

Implement quantile: Use quantile(x, probs=0.25, type=2, na.rm=TRUE) etc.

Implement mean, sd (sample sd, denominator n-1, same as SAS).

Implement formatting: Use sprintf with round_half_up.

Define round_half_up <- function(x, digits=0) { sign(x)*floor(abs(x)*10^digits+0.5)/10^digits }

Define fmt_num <- function(x, digits) { if is.na(x) return("") ; rounded <- round_half_up(x, digits); formatC(rounded, format="f", digits=digits, flag="") %>% trimws? But need to strip leading spaces. Use formatC then trim.

For mean/sd with 2 decimals? Wait decimals=1 => dp1=2 => 2 decimals. For median etc 1 decimal.

For pct: 5.1 => 1 decimal.

For n: integer as character without decimals.

We need to handle SD when n<=1 => blank.

We need to create _dur_stats data frame with columns TRTA, stat, value, ord, category, block.

We need to handle ordering: stat order 1..6.

We need to create _durcat_fmt similarly.

Then combine.

Then transpose: Use tidyr::pivot_wider.

We need to map TRTA to col1,col2,col3 in order of TRTAN. Let's get distinct TRTA/TRTAN mapping sorted by TRTAN.

We need to ensure column names are sanitized? In SAS, TRTA values with spaces become column names with underscores. In R pivot_wider, names will be TRTA values. We'll then rename to col1 etc based on order.

Simpler: pivot_wider names_from=TRTA, values_from=value, then reorder columns according to TRTAN order.

Then create final data frame sorted by block, ord.

We need to write CSV: columns: Category, Statistic, Placebo (N=...), Low Dose (N=...), High Dose (N=...)

We need to write TXT: include titles, headers, rows.

Let's craft script.


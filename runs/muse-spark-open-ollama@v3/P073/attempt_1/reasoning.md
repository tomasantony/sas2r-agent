Need to translate SAS program to R. Must read inputs from adam libref: D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam . Need to list files. Use haven::read_xpt or read_sas. Likely .sas7bdat or .xpt. Let's explore via R script mental? We need to produce script that reads correctly.

We need to reproduce outputs: t_exposure.rtf table with 58 numbers. Need to understand logic.

Steps:

- m_counts: Big N per TRTA where SAFFL='Y' from adam.adsl. Also total.

- ex_adsl: from adam.adsl where SAFFL='Y', TRTDUR = TRTEDT - TRTSDT +1 if both non-missing.

- m_cont_stats: proc means nway noprint class TRTA var TRTDUR output n mean std median min max q1 q3. Then format:

n as integer string
Mean with decimals+1 (decimals=1 => 2 decimals? Wait dp1 = decimals+1 => 2)
SD with dp1 (2 decimals)
Median with dp (1 decimal)
Q1,Q3 as "q1, q3" with dp (1 decimal)
Min, Max as "min, max" with dp

Note: SAS proc means default definitions: need to replicate exactly. For Q1 Q3, SAS uses PCTLDEF=5 by default? Need to check. PROC MEANS default Q1/Q3 uses PCTLDEF=5. R quantile types differ. Need to emulate SAS PCTLDEF=5.

We need to check what SAS does. PCTLDEF=5: empirical distribution function with averaging. Formula: y = (1-g)*x_j + g*x_{j+1} where j = floor(n*p + 0.5) ??? Let's recall.

SAS documentation: PCTLDEF=5: uses empirical distribution function with averaging: y_p = (x_(k) + x_(k+1))/2 where k = n*p ??? Need precise.

Actually SAS PCTLDEF definitions:

- PCTLDEF=1: weighted average at X_np where np = n*p
- PCTLDEF=2: weighted average at X_(n+1)p
- PCTLDEF=3: empirical distribution function
- PCTLDEF=4: weighted average at X_np where np = n*p, with averaging
- PCTLDEF=5: empirical distribution function with averaging

For PCTLDEF=5: If n*p is integer, average of x_{np} and x_{np+1}; else x_{ceil(np)} . Wait that's PCTLDEF=3? Let's check.

Better to look up: SAS PCTLDEF=5 definition:

Let n be number of nonmissing values, p percentile/100.

Compute j = n*p

If j is integer, then y = (x_j + x_{j+1})/2

If j not integer, then y = x_{ceil(j)}

But there is also handling for p=0 or 1.

Alternatively some sources: PCTLDEF=5: y = 0.5*(x_{floor(np)} + x_{ceil(np)}) ??? Need to verify.

We need to implement correctly to match SAS numbers. Could test with synthetic data.

We also need to handle missing: n counts non-missing.

We need to produce final table: block 1 category "Duration of Treatment (days)" with stats: n, Mean, SD, Median, Q1,Q3, Min, Max (6 rows? Actually n, Mean, SD, Median, Q1,Q3, Min,Max = 6 rows). Block 2 category "Duration Categories" with 5 categories? Actually 6 possible but Missing maybe not present if no missing TRTDUR. Let's see.

Duration categories:
<=28 days (1)
>28 to <=84 days (2)
>84 to <=168 days (3)
>168 to <=336 days (4)
>336 days (5)
Missing (6)

Counts per TRTA.

Then transpose to wide: columns col1 col2 col3 corresponding to Placebo, Drug Low Dose, Drug High Dose.

Need to produce text rendering and CSV.

We need to discover actual data to compute numbers. Let's attempt to infer file structure.

We have OUT_DIR env var. Need to write outputs: t_exposure.csv and t_exposure.txt plus maybe png? But spec says table -> text rendering and CSV.

We need to replicate SAS formatting: numbers with same decimals.

We need to handle TRTA values: likely "Placebo", "[Drug] Low Dose", "[Drug] High Dose" but SAS variable TRTA contains those strings. In transpose, id TRTA creates columns named after values with spaces replaced? In SAS, proc transpose with id TRTA will create column names from TRTA values, sanitized: colPlacebo etc. The patch renames colPlacebo=col1 etc. So we need to map.

We need to read adam.adsl to see distinct TRTA.

We should write R script that does:

- Find adam path: D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam

List files: likely adsl.sas7bdat or adsl.xpt

We need to handle both.

- Read with haven::read_sas or read_xpt.

- Similarly sdtm not needed.

- Compute Big N.

- Compute TRTDUR.

Note: TRTEDT and TRTSDT in ADSL are numeric SAS dates (days since 1960-01-01) or character? In ADaM, they are numeric dates with format. haven will read as numeric or Date? Need to handle.

If they are Date objects, subtraction gives difftime in days. If numeric, subtraction gives days.

We need to handle missing: SAS . corresponds to NA.

- Compute descriptive stats per TRTA.

We need to implement SAS rounding: round half away from zero. R's round uses bankers. Need helper.

But formatting uses put with format 12.1 etc which rounds half away from zero. So we need to implement.

Also percentages: pct = cnt / bign *100, format 5.1 (one decimal). SAS put with 5.1 rounds half away.

We need to produce value strings exactly as SAS: e.g., "12 ( 34.5%)" but in this program value = cat(strip(put(cnt,8.)), ' (', strip(put(pct,5.1)), ')') . So no extra spaces inside? strip removes leading spaces. So "12 (34.5)"? Actually put(cnt,8.) gives "      12", strip -> "12". put(pct,5.1) gives " 34.5", strip -> "34.5". So final "12 (34.5)". For zero cnt, value='0'.

For cont stats: value = strip(put(n,8.)) for n; for mean: strip(put(mean,12.2)) etc. Need to replicate.

We need to also handle SD when n<=1 => blank ''.

We need to produce final dataset with columns: block, category, ord, stat, col1, col2, col3

And then produce report.

We need to write CSV and TXT.

We need to ensure 58 numbers in text rendering? The spec says 58 numbers. Let's count: For each TRTA, we have 6 continuous stats + 5 categories = 11 rows *3 columns =33 cells. Each cell may contain numbers: n is 1 number, Mean 1, SD 1, Median 1, Q1Q3 2 numbers, MinMax 2 numbers, each category count+percent 2 numbers. So per treatment: n(1)+Mean1+SD1+Median1+Q1Q32+MinMax2+5 categories*2=10 => 18 numbers per treatment *3 =54 plus Big N in headers (3) + maybe total? But spec says 58 numbers. Could be including Big N.

We need to produce text file that contains same numbers.

We need to implement SAS proc means statistics precisely to match SAS output.

We need to investigate actual data to compute expected values. Since we cannot run SAS, we need to approximate but must match SAS output exactly. The evaluation will compare CSV numbers? Probably they will check CSV values match SAS output within tolerance.

We need to ensure we compute mean, std, median, min, max, q1, q3 exactly as SAS.

Mean: arithmetic mean.

Std: sample standard deviation (n-1 denominator). SAS PROC MEANS std is sample std.

Median, Q1, Q3: need PCTLDEF=5.

We should implement quantile_pctldef5 function.

Let's recall SAS PCTLDEF=5 formula precisely:

From SAS docs:

PCTLDEF=5: Empirical distribution function with averaging.

Let n = number of nonmissing values, x1 <= x2 <= ... <= xn sorted.

For p between 0 and 1:

Let np = n * p

Let j = floor(np)

Let g = np - j

If g == 0 then y = (x_j + x_{j+1}) /2  (with x_0 defined as x1? Need edge)

If g >0 then y = x_{j+1}

But for p=0, y = x1; for p=1, y = xn.

Check example: n=5, p=0.25 => np=1.25 => j=1 g=0.25 => y = x2

n=4 p=0.25 => np=1 => j=1 g=0 => y=(x1+x2)/2

n=4 p=0.5 => np=2 => y=(x2+x3)/2

n=4 p=0.75 => np=3 => y=(x3+x4)/2

That matches empirical with averaging.

For p=0.5 median with even n, median is average of two middle values, which matches.

For odd n, median is middle value? Let's test n=5 p=0.5 => np=2.5 => j=2 g=0.5 => y=x3 correct.

So implement that.

We also need to handle Q1 p=0.25, Q3 p=0.75.

We should implement function quantile_sas(x, p):

n = length(x)
if n==0 return NA
if p==0 return min(x)
if p==1 return max(x)
np = n * p
j = floor(np)
g = np - j
if g == 0 {
  # average of x_j and x_{j+1}, but careful indexing 1-based
  # j is integer, need x_j and x_{j+1}
  # For j=0? When p small and n small, np could be <1 => j=0 => then y = x1? Let's see SAS handling: if j=0 then x_j not defined, but g>0? For n=3 p=0.25 => np=0.75 j=0 g=0.75 => y=x1 correct.
  # If np integer and j=0 => p=0 => already handled.
  # So j >=1 when g==0 and p>0
  return (x[j] + x[j+1])/2
} else {
  return x[ceiling(np)]
}

But need to handle floating point equality: g ==0 within tolerance.

We should use tolerance 1e-12.

We also need to handle SAS missing: if n=0 then all stats missing.

We need to compute for each TRTA group.

We also need to handle TRTA ordering: order by TRTA? In SAS, proc sql order by TRTA will sort character ascending (ASCII). Placebo vs [Drug]... Need to see. TRTA values likely "Placebo", "Xanomeline High Dose", "Xanomeline Low Dose" or similar. The macro sets trt1lbl Placebo, trt2lbl [Drug] Low Dose, trt3lbl [Drug] High Dose. But actual ADSL TRTA may be those strings.

We need to sort by TRTAN? In ex_adsl, they order by TRTAN. TRTAN is numeric code: 1 Placebo, 2 Low, 3 High. So ordering is by TRTAN.

For Big N, they order by TRTA (character). But that may still give same order if Placebo < Xanomeline? Let's check: "Placebo" (P) vs "Xanomeline High Dose" (X) vs "Xanomeline Low Dose" (X...). Lexicographically, Placebo < Xanomeline High < Xanomeline Low? Compare "Xanomeline High Dose" vs "Xanomeline Low Dose": at character 11? "H" (72) vs "L" (76) => High < Low. So order would be Placebo, High, Low. But TRTAN order is Placebo(1), Low(2), High(3). Which one does SAS use for final columns? The transpose id TRTA will create columns in order of appearance in data sorted by TRTAN, PARAMCD. But proc transpose will create columns in order of data? Actually proc transpose with by block category ord stat and id TRTA will create one column per distinct TRTA value, ordered as they appear in the input dataset. Input dataset all_exp is sorted by block ord stat, but not by TRTA? Wait all_exp is from _dur_stats and _durcat_fmt, which are sorted by TRTA? Let's see: _dur_stats is sorted by ord TRTA? Actually proc sort after m_cont_stats: by ord TRTA. Then _durcat_fmt is from proc sql order by DURCATN, TRTA. Then all_exp set combines them, then proc sort by block ord stat. So within each by group (block category ord stat), the TRTA order is as encountered in the sorted data? But after sorting by block ord stat, the TRTA order within each group is not guaranteed; proc transpose will use the order of TRTA values as they appear in the input dataset after sorting. Since sorting is stable? SAS sort is stable? Might preserve original order within by groups.

Simpler: The test-copy patch renames colPlacebo=col1 colDrug_X_Low_Dose=col2 colDrug_X_High_Dose=col3 . That suggests they expect columns named colPlacebo, colDrug_X_Low_Dose, colDrug_X_High_Dose. That implies TRTA values are "Placebo", "Drug X Low Dose", "Drug X High Dose" (with spaces). SAS variable name sanitization replaces spaces with underscores.

So col1 corresponds to Placebo, col2 to Low Dose, col3 to High Dose. That matches TRTAN order: 1 Placebo, 2 Low, 3 High.

Thus we should map accordingly: col1 = Placebo, col2 = Low Dose, col3 = High Dose.

We need to determine actual TRTA strings in synthetic data. Let's assume they are "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose" as in CDISC pilot. The patch uses colDrug_X_Low_Dose etc, but that is generic placeholder [Drug] replaced with Drug X? Could be.

We need to handle generically: we will create columns based on TRTAN order, not name.

Simpler: In R, we will pivot wider using TRTA values, then rename to col1,col2,col3 based on TRTAN order.

We need to get mapping from TRTA to TRTAN: distinct pairs.

We need to produce final CSV with columns: block, category, ord, stat, col1, col2, col3

But for human readable text, we need headers with Big N: e.g., "Placebo (N=86)" etc.

We need to compute Big N per TRTA.

We need to produce text rendering that mimics PROC REPORT output.

We need to write script that does all steps and writes outputs to OUT_DIR.

We need to handle file paths: adam path is D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam . Let's check what files exist. We can't list directly but we can write R code to list.

We need to produce final R script that will run on Windows with Rscript.

We need to include helper functions: round_half_up, format_sas, quantile_sas.

We need to ensure we handle dates: TRTEDT, TRTSDT may be numeric SAS dates or Date objects. haven reads SAS dates as numeric with attribute? Actually haven::read_sas will convert SAS dates to R Date if format is DATE. Let's check.

We need to handle both: if column is Date, subtraction gives difftime; if numeric, subtraction gives days.

We need to compute TRTDUR = TRTEDT - TRTSDT +1

We need to handle missing: if either is NA, TRTDUR = NA.

We need to implement m_cont_stats logic exactly.

We need to also handle that ex_adsl is derived from adam.adsl where SAFFL='Y'.

We need to compute stats per TRTA.

We need to compute duration categories.

We need to produce final table.

We need to write CSV and TXT.

We need to ensure we use OUT_DIR env var.

We need to produce script that is standalone.

We should also handle that adam.adex may exist but program uses ex_adsl alternative. The SAS program creates both ex and ex_adsl, but then uses ex_adsl for stats. So we follow that.

We need to implement proc means nway: class TRTA, var TRTDUR, output n mean std median min max q1 q3. This is per TRTA only (no byvar). So we get one row per TRTA.

We need to handle that proc means with class and nway produces only combinations where all class variables present (i.e., per TRTA). No overall total.

We need to compute stats.

We need to format values.

We need to handle decimals: decimals=1 => dp=1, dp1=2

So:

n: integer no decimals

Mean: 2 decimals

SD: 2 decimals

Median: 1 decimal

Q1,Q3: 1 decimal each, combined as "q1, q3"

Min, Max: 1 decimal each, combined as "min, max"

We need to implement rounding half away from zero.

We need to create helper round_half_up(x, digits):

sign = sign(x)
abs_x = abs(x)
factor = 10^digits
rounded = floor(abs_x * factor + 0.5) / factor
return sign * rounded

But need to handle NA.

Then formatting: sprintf with appropriate decimals, but need to strip leading spaces. Use format with sprintf.

For mean: sprintf("%.2f", rounded_mean) but need to handle NA => "".

Similarly for others.

We need to ensure we use same rounding as SAS: SAS put with w.d rounds half away from zero.

We need to implement for each stat.

We need to handle SD when n<=1 => blank.

We need to handle Q1,Q3 and Min,Max when n==0 => blank.

We need to create _dur_stats dataset with columns: TRTA, stat, value, ord, category, block

stat values: 'n', 'Mean', 'SD', 'Median', 'Q1, Q3', 'Min, Max'

ord: 1..6

category = 'Duration of Treatment (days)'

block=1

We need to do same for duration categories.

For duration categories, we need to compute cnt per TRTA per DURCAT, then pct = cnt / bign *100, format 5.1 (one decimal). Then value = "cnt (pct)" or "0" if cnt==0.

But note: In SAS, they create _durcat_cnt via proc sql group by TRTA, DURCAT, DURCATN, counting distinct USUBJID. Then left join with _bign to get pct. Then _durcat_fmt creates value.

We need to replicate: For each TRTA and each DURCAT that appears in data, they create a row. But what about categories with zero count for a TRTA? In SAS, proc sql group by will only create rows for combinations that exist. So if a TRTA has zero subjects in a category, that row will be missing for that TRTA, and after transpose, that cell will be missing (blank). However, the final report may show blank or "0"? In SAS, they set value='0' when cnt==0, but they never create rows where cnt==0 because group by only includes existing combos. So zero counts would be missing, not "0". But they handle cnt>0 else '0' for existing rows where cnt==0? But cnt is count distinct, so if group exists, cnt>=1. So '0' never occurs. However, if a category has no subjects for a TRTA, that TRTA will have no row for that category, and after transpose, the cell will be missing (NA). In R, we need to ensure we create all combinations? Let's see SAS behavior: proc transpose will create missing values for absent combinations, which will appear as blank in report. That's correct.

But we need to ensure we replicate that: we should not fill zeros for missing combos; leave NA.

However, the spec says 58 numbers: Let's count if we have 6 categories including Missing, but if Missing has zero for all, then we would have 5 categories *3 =15 cells, plus 6 stats*3=18 =>33 cells. Each category cell has 2 numbers (cnt and pct) => 30 numbers, plus continuous stats numbers: n1+mean1+sd1+median1+q1q3 2+minmax2 =7 per trt *3=21 => total 51 numbers. Plus Big N 3 =>54. Not 58. If we include Missing category with zeros, then 6 categories*3=18 cells*2=36 +21=57+3=60. So 58 suggests something else.

Maybe they have 5 categories plus n etc, but some categories have zero? Let's not overthink.

We need to produce output that matches SAS exactly, including handling of missing categories.

We should implement logic to create all possible DURCAT values (1..6) and then left join counts, but SAS does not. However, to match SAS, we should follow SAS: only create rows for existing combos.

But then after transpose, missing cells will be NA, which we will write as "" in CSV.

We need to ensure we produce same as SAS.

We need to also handle that DURCATN ordering: ord = 100 + DURCATN, so categories appear after continuous stats (ord 1..6 vs 101..106). Block 2 ensures they are separate.

We need to produce final dataset sorted by block, ord, stat.

We need to produce report with columns block, category, ord, stat, col1, col2, col3

We need to write CSV and TXT.

We need to handle headers: col1hdr etc are macro variables from m_counts: col1hdr = "Placebo (N=xx)" etc. We need to compute similarly.

We need to produce text rendering that includes titles and footnotes.

Titles from u_titles.sas for T_EXPOSURE:

title4 "Table 14.1.3"
title5 "Summary of Study Drug Exposure"
title6 "Safety Analysis Set"

Footnotes:

footnote1 "Data cutoff: [DDMONYYYY]" (but actual value is placeholder [DDMONYYYY] from u_setup)
footnote2 "Source: ADaM datasets"
footnote3 "Program: &sysin. | Run: date time"

We need to reproduce titles verbatim.

We need to produce text file with table.

We need to ensure we write CSV with same column names as SAS dataset final: block, category, ord, stat, col1, col2, col3

We need to handle that final dataset has block, category, ord, stat, col1, col2, col3

We need to produce script that does all.

We need to handle reading adam files: we need to list files in adam directory.

We should write R code that checks for .sas7bdat, .xpt, .csv.

We need to handle that haven::read_sas can read .sas7bdat, haven::read_xpt for .xpt.

We need to handle that adam path may contain files like adsl.sas7bdat, adex.sas7bdat etc.

We need to implement helper to read dataset by name: try .sas7bdat first, then .xpt, then .csv.

We need to handle that TRTA variable may be character with trailing spaces.

We need to handle SAFFL flag: upcase(SAFFL) == "Y"

We need to handle TRTSDT, TRTEDT: they may be numeric SAS dates or character ISO dates. In ADaM, they are typically numeric dates with format DATE9. haven will convert to Date.

We need to handle both.

We need to implement quantile function.

We need to handle rounding.

We need to produce final outputs.

We need to ensure script runs non-interactively.

We need to produce one R script inside one ```r code block.

We need to ensure we use only allowed packages.

We need to write outputs to OUT_DIR.

We need to handle that OUT_DIR may not exist, need to create.

We need to produce t_exposure.csv and t_exposure.txt

We need to ensure we write dates as YYYY-MM-DD if any.

We need to handle that final CSV should have same variable names as SAS dataset (case-insensitive). Use lower case? SAS variable names are uppercase but R will use lower? We should use same case as SAS: block, category, ord, stat, col1, col2, col3 (lowercase as in SAS final dataset). Actually SAS final dataset has variables block, category, ord, stat, col1, col2, col3 (lowercase as defined). So we will use those.

We need to produce text rendering: we need to create a plain text table that contains same numbers.

We should use simple formatting: print table with headers.

We need to ensure we include 58 numbers.

We need to handle that the text file should contain titles, headers, and table body.

We should generate text file with:

Table 14.1.3
Summary of Study Drug Exposure
Safety Analysis Set

Then column headers with Big N.

Then rows.

We need to ensure we format numbers exactly as SAS.

We need to implement helper to format mean etc.

We need to handle that SAS proc report will display missing values as blank.

We need to produce CSV of plotted data? Not needed for table.

We need to ensure we handle that the script must be complete.

We should now craft final R script.

We need to include library calls.

We need to handle that we need to find adam path: use Sys.getenv or hardcode? The SAS program uses libname adam "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam". So we should use that path directly, not env var. But we should also handle that the R script should read from that absolute path.

We should define adam_path = "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

Similarly sdtm path not needed.

We need to handle that OUT_DIR is from Sys.getenv("OUT_DIR").

We need to ensure we create OUT_DIR if not exists.

We need to handle that we need to write outputs to file.path(out, "t_exposure.csv") and file.path(out, "t_exposure.txt")

We need to ensure we handle that the expected output is t_exposure.rtf but we write txt and csv.

The spec says: each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers and as CSV of table body.

So we need to write t_exposure.txt and t_exposure.csv

We should also write t_exposure.rtf? But spec says write CSV and TXT. We will write both.

We need to ensure we handle that the R script must produce counterparts under OUT_DIR.

We should now write the R script.

We need to include helper functions:

round_half_up <- function(x, digits=0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-9) / 10^digits)
}

format_sas <- function(x, digits) {
  if (is.na(x)) return("")
  # round half up
  rx <- round_half_up(x, digits)
  # format with fixed decimals, strip leading spaces
  fmt <- sprintf(paste0("%.", digits, "f"), rx)
  # strip? sprintf already no leading spaces
  return(fmt)
}

But for put with w.d, SAS will pad with spaces to width w, but strip removes. So we just need sprintf.

For cnt: format 8. => integer, no decimals, strip.

For pct: format 5.1 => one decimal, width 5, but strip.

We need to handle that pct may be 100.0 => width 5 includes "100.0" (5 chars) ok.

We need to handle that mean with 12.2 => width 12, but strip.

We need to implement quantile_sas.

We need to handle that proc means std uses sample std (n-1). R sd() does same.

We need to handle that proc means mean, std, etc are computed with double precision.

We need to handle that we need to compute stats per TRTA.

We should write code to compute stats:

compute_stats <- function(df, trtvar, anavar) {
  # df has columns TRTA, TRTDUR
  # group by TRTA
  # for each group, compute n, mean, std, median, min, max, q1, q3
}

We need to handle that TRTA may have NA? Filter SAFFL.

We need to handle that we need to get distinct TRTA values and their TRTAN for ordering.

We should get mapping: distinct TRTA, TRTAN from adsl where SAFFL='Y'

Then order by TRTAN.

Then for each TRTA in that order, compute stats.

We need to handle that we need to produce _dur_stats dataset with one row per TRTA per stat.

We need to handle that we need to produce value strings.

We need to handle that we need to produce ord.

We need to handle that we need to produce category and block.

We need to handle that we need to produce final transpose.

We should implement transpose via tidyr::pivot_wider.

We need to handle that we need to produce final dataset with block, category, ord, stat, col1, col2, col3

We need to handle that we need to ensure we have correct column names: col1 corresponds to first TRTAN, etc.

We need to handle that we need to produce text rendering.

We should now craft script.

We need to include error handling: if adam files not found, try alternative paths.

We should list files in adam_path.

We need to handle that haven::read_sas may require file path with forward slashes.

We should implement read_adam function:

read_adam <- function(name) {
  # name lower case like "adsl"
  # try .sas7bdat, .xpt, .sas7bdat with upper, etc.
  base <- file.path(adam_path, name)
  # try variations
  candidates <- c(
    paste0(base, ".sas7bdat"),
    paste0(base, ".sas7bdat"),
    paste0(base, ".xpt"),
    paste0(base, ".XPT"),
    file.path(adam_path, toupper(name), paste0(toupper(name), ".sas7bdat")),
    file.path(adam_path, toupper(name), paste0(toupper(name), ".xpt"))
  )
  # also list files in adam_path
  files <- list.files(adam_path, recursive=TRUE, full.names=TRUE)
  # find file that matches name ignoring case
  idx <- grep(paste0("^", name, "\\."), basename(files), ignore.case=TRUE)
  if (length(idx)>0) candidates <- c(files[idx], candidates)
  for (f in candidates) {
    if (file.exists(f)) {
      if (grepl("\\.xpt$", f, ignore.case=TRUE)) {
        return(haven::read_xpt(f))
      } else if (grepl("\\.sas7bdat$", f, ignore.case=TRUE)) {
        return(haven::read_sas(f))
      } else if (grepl("\\.csv$", f, ignore.case=TRUE)) {
        return(readr::read_csv(f, show_col_types=FALSE))
      }
    }
  }
  stop(paste("Dataset", name, "not found in", adam_path))
}

We need to handle that haven::read_sas returns tibble with labelled vectors; need to convert to character/numeric.

We should convert haven_labelled to appropriate.

We can use haven::as_factor? But we want original values.

We should use dplyr::mutate across where haven_labelled -> as.character or as.numeric.

Simpler: use haven::zap_labels and zap_formats.

We should do: df <- haven::read_sas(f); df <- haven::zap_labels(df); df <- haven::zap_formats(df)

Similarly for read_xpt.

We need to handle that TRTA may be labelled.

We need to handle that SAFFL may be character.

We need to handle that TRTSDT, TRTEDT may be Date or numeric.

We need to handle that we need to compute TRTDUR.

We should check types: if inherits(TRTSDT, "Date") then as.numeric(TRTEDT - TRTSDT) +1 else if numeric then TRTEDT - TRTSDT +1

But if they are numeric SAS dates, they are already days since 1960, so subtraction gives days.

If they are character ISO dates, need to parse.

We should handle both: if is.character(TRTSDT) then parse with as.Date.

We should implement helper to convert to Date if needed.

We need to handle that ADSL may have TRTSDT as numeric with NA.

We should now write script.

We need to handle that we need to produce Big N.

We need to handle that we need to produce headers.

We should compute bign per TRTA and total.

We need to handle that we need to produce col headers: e.g., "Placebo (N=86)" etc.

We need to handle that we need to produce final text file with headers.

We should produce text file with:

[Sponsor Name]   Page 1 of 1
Protocol: [Protocol Number]

Table 14.1.3
Summary of Study Drug Exposure
Safety Analysis Set

Then table.

We need to handle that sponsor, protocol are placeholders [Sponsor Name], [Protocol Number] from u_setup.

We should use those placeholders.

We need to handle that we need to produce footnotes.

We should include footnotes at bottom.

We need to handle that we need to produce CSV.

We should now craft final R script.

We need to ensure we use only allowed packages.

We should include library calls for haven, dplyr, tidyr, etc.

We need to handle that we need to create OUT_DIR.

We should now produce final answer: one R code block.

We need to ensure we handle all steps.

We should test mentally.

We need to handle that we need to produce 58 numbers: we need to ensure our output contains same numbers as SAS.

We need to handle that we need to implement SAS rounding correctly.

We should implement round_half_up with tolerance.

We need to handle that we need to format percentages with one decimal.

We need to handle that we need to format mean with 2 decimals, etc.

We should now write script.

We need to handle that we need to produce final dataset with correct ordering: block, ord, stat

We need to handle that we need to sort final by block, ord

We should produce final CSV with columns block, category, ord, stat, col1, col2, col3

We need to handle that we need to produce text rendering with same.

We should produce text file with fixed width columns.

We should use simple approach: create data frame final and then write to text via cat.

We should include titles and footnotes.

We should ensure we handle that we need to write numbers with same decimals.

We should now produce final R script.

We need to handle that we need to handle missing values: in final, missing cells should be "".

We should ensure we handle that we need to produce correct number of rows: 6 continuous + 5 categories =11 rows.

We should check: continuous stats rows: n, Mean, SD, Median, Q1,Q3, Min,Max => 6 rows

Categories: 5 rows (if Missing not present) => total 11 rows

Each row has 3 columns => 33 cells

Each cell may have 1 or 2 numbers => total numbers 54 + Big N 3 =57, close to 58.

Maybe they count Big N as numbers too, plus maybe total? But spec says 58 numbers, we will trust.

We should now produce script.

We need to handle that we need to ensure we handle that the script must run with Rscript on Windows.

We should avoid using install.packages.

We should use base R and allowed packages.

We should now craft final output.


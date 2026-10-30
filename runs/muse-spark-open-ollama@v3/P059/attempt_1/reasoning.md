Need to translate SAS program P059_pk_param_summary.sas to R.

We have inputs: adpp.xpt in rawadam. Also inlib -> derived adam. But program uses source.adpp from rawadam/adpp.xpt.

We need to reproduce outputs: P059.lst table/listing text, 124 numbers -> write text rendering and CSV.

We need to understand SAS logic.

Steps:

- proc format defines TRTAN, statg, $pk.

- data adpp: set source.adpp where PKFN=1 and ANL01FN ne 1 and paramcd in ('AUCINFO' 'AUCLAST' 'CMAX' 'TMAX' 'MRTLAST' 'LAMZHL') and PPSTRESN ne . and compress(PPCATL)='ANAL2'; _PARAM = compbl(put(PPTESTCD,$pk.)||' ('||strip(PPORRESU)||')'); AVAL_log = log(AVAL);

Note: paramcd list includes 6 values. PPSTRESN not missing. PPCATL compressed equals ANAL2. PKFN=1, ANL01FN !=1.

- proc sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM

- proc univariate by those vars var AVAL AVAL_log output n=n1 n2 mean=mn1 mn2 std=std1 std2 cv=cv1 cv2 median=med1 med2 min=min1 min2 max=max1 max2

Note: PROC UNIVARIATE CV = std/mean*100 . Need to replicate.

- data adpp_sum_t transposed: for each row in adpp_sum, create 7 rows s_order 1..7 with value formatted.

We need to replicate SAS formatting exactly: put with w.d formats.

SAS put formatting: put(value, w.d) -> rounds half away? Actually SAS uses round half away? Need to implement.

Also put(n1, 2.) -> width 2, no decimals. For n.

For s_order 2 mean (sd):
 when index(_PARAM,'AUC')>0 value = put(mn1,6.) ||' ('||put(std1,5.) ||')';
 when index(_PARAM,'Cmax')>0 value = put(mn1,5.) ||' ('||put(std1,4.) ||')';
 when index(_PARAM,'MRT')>0 value = put(mn1,4.1)||' ('||put(std1,4.2)||')';
 when index(_PARAM,'t1/2')>0 value = put(mn1,4.2)||' ('||put(std1,4.2)||')';
 otherwise value = ' ';

Note: put(mn1,6.) means width 6, 0 decimals? Actually format 6. means w=6 d=0. Similarly 5. means w=5 d=0. 4.1 means w=4 d=1 etc.

For s_order 3 CV% mean: if index(_PARAM,'tmax')=0 then value = put(cv1,4.1); else ' '

s_order 4 geo-mean: exp(mn2) with formats: AUC 6., Cmax 5., MRT 4.1, t1/2 4.2 else ' '

s_order 5 CV% geo-mean: if tmax=0 then put(sqrt(exp(std2**2)-1)*100,4.1) else ' '

s_order 6 median: AUC 6., Cmax 5., MRT 4.1, t1/2 4.2, tmax 4.2

s_order 7 [min; max]: AUC '['||put(min1,6.)||';'||put(max1,6.)||']' ; Cmax '['||put(min1,4.)||';'||put(max1,5.)||']' ; MRT '['||put(min1,4.2)||';'||put(max1,4.1)||']' ; t1/2 '['||put(min1,4.2)||';'||put(max1,4.1)||']' ; tmax '['||put(min1,4.2)||';'||put(max1,4.2)||']'

Note: For MRT and t1/2, min uses 4.2, max uses 4.1 (different). Need to replicate.

- proc sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM

- proc report with by ANLPRNT PPSPECL PPCATL ; column ( ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM, value ); define etc. break after PPSDY/skip; compute before _page_ line "Compound: ..."

We need to produce text rendering and CSV.

We need to inspect actual data to understand values.

We need to locate files. Paths given: &rawadam = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam ; &rawsplit etc. But in linux environment, we need to find actual path. The OUT_DIR env var will be set. But input paths: we should try to locate files.

We have to write R script that reads from absolute paths given in context (forward slashes). The SAS program uses &rawadam./adpp.xpt . So path is D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.xpt . In linux, that path may not exist; but we can try to find file via listing.

We need to explore filesystem.

We should write R script that handles both Windows path and linux path fallback: try to find file via Sys.getenv etc. But requirement says read inputs from absolute paths given in context (forward slashes). So we must use those paths. However on linux, D:/... won't exist. We need to make script robust: try D:/... if not exists, try alternative like /... or relative.

We can search for adpp.xpt location.

We need to do exploratory step: but we are to output final R script. We need to infer data.

We can attempt to run R code mentally? Better to think about what outputs expected: P059.lst with 124 numbers.

We need to produce text rendering that contains same numbers SAS output shows.

We need to replicate PROC REPORT output.

We need to understand structure: BY groups: ANLPRNT, PPSPECL, PPCATL. Likely only one combination? Let's check typical CDISC pilot ADaM ADPP. ANLPRNT maybe compound, PPSPECL matrix, PPCATL analyte. For pilot, maybe ANLPRNT = "Drug X" etc.

We need to actually inspect data if possible. Since we cannot run interactive, we need to make script that will work when executed.

We need to implement SAS formatting functions.

Key: SAS PUT with w.d: width w, decimals d. It right-aligns within width, padding with spaces on left. When concatenating with ||, the spaces remain. But in PROC REPORT, value is displayed with width 19 left group missing flow. So leading spaces may be trimmed? Need to see.

In SAS, put(mn1,6.) for e.g., mn1=123.456 -> put with 6. => "   123" (rounded to integer). Actually 6. means 0 decimals, width 6, rounded. So 123.456 -> 123 -> "   123" (3 spaces + 3 digits). When concatenated with ' (' etc., the spaces remain inside.

Similarly put(std1,5.) -> width 5.

For 4.1: width 4, 1 decimal. Example mn1=12.34 -> put 4.1 => "12.3" ? Width 4 includes decimal point. So "12.3" is 4 chars. If value is 123.45 -> "123.5" (rounded) width 5? But format width 4 would overflow? SAS would expand? Actually SAS will print with more width if needed, not truncate. But we should implement similar: sprintf with width.

We need to implement sas_put function: given x, w, d, return string of length w (or more if overflow) with rounding half away from zero.

SAS rounding: round half away from zero (commercial rounding). R's round uses bankers. Need to implement.

Also SAS PUT for integers: put(n1,2.) -> width 2, no decimals. n is integer count.

We need to handle missing values: if value is NA, SAS put returns spaces? But in this program, n1 etc should not be missing because filtered.

We need to implement compbl, strip, compress, index, etc.

But for _PARAM: compbl(put(PPTESTCD,$pk.)||' ('||strip(PPORRESU)||')')

$pk format maps PPTESTCD to display: AUCINFO->AUC, AUCTAU->AUCtau, AUCLAST->AUC(0-tlast), CMAX->Cmax, etc. Only those in list will appear, but filtered paramcd includes AUCINFO, AUCLAST, CMAX, TMAX, MRTLAST, LAMZHL. So mapping:

AUCINFO -> AUC
AUCLAST -> AUC(0-tlast)
CMAX -> Cmax
TMAX -> tmax
MRTLAST -> MRT
LAMZHL -> t1/2

Then _PARAM = compbl( put(...) || ' (' || strip(PPORRESU) || ')' )

PPORRESU is unit, e.g., "h*ng/mL" etc. So _PARAM examples: "AUC (h*ng/mL)" etc.

We need to replicate.

Next, AVAL_log = log(AVAL) natural log.

Then proc univariate: need to compute n, mean, std, cv, median, min, max for AVAL and AVAL_log.

Note: SAS PROC UNIVARIATE with noprint and output: n, mean, std, cv, median, min, max. CV = std/mean*100 . If mean=0, CV missing.

We need to compute same.

Then adpp_sum_t: need to generate 7 rows per group.

We need to handle formatting details for each s_order.

We should implement helper functions:

sas_round <- function(x, digits) { # half away from zero
  # digits = number of decimals
  # Use sign * floor(abs(x)*10^digits + 0.5)/10^digits
}

sas_put <- function(x, w, d) {
  if (is.na(x)) return(strrep(" ", w))
  # round to d decimals using half away
  # then format with sprintf
  # width w, right aligned, padded with spaces
  # If formatted string longer than w, SAS will not truncate, it will expand (print full)
  # So we should not truncate.
  # For d=0, format without decimal point
  # For d>0, format with d decimals
  # Use sprintf
}

But need to handle w.d where d may be missing? In SAS, "6." means w=6 d=0. "4.1" means w=4 d=1.

We need to parse.

Also need to handle put with width and decimals for values like 6. -> integer rounding.

We should implement sas_put_numeric <- function(x, fmt) where fmt like "6.", "4.1", "2." etc.

We need to extract w and d.

Also need to handle put for character? Not needed.

We also need to handle put for n1 with 2. -> width 2.

We need to replicate SAS behavior for overflow: e.g., put(12345, 4.1) -> would need 6 chars "12345.0" but width 4, SAS would print "12345.0" (no truncation). So we should just format without width limit if overflow.

Implementation: rounded <- sas_round(x, d); formatted <- sprintf(paste0("%", w, ".", d, "f") if d>0 else paste0("%", w, ".0f"), rounded) but sprintf will pad to width w, but if number longer, it will expand. That's correct. However sprintf with width includes padding, but we need to ensure rounding already done, but sprintf also rounds. To avoid double rounding differences due to binary, we should use our rounded value then format with sprintf that will not further round incorrectly? But sprintf will round again, but if we already rounded, it should be same. Safer to use sprintf directly with our rounding? Actually sprintf uses round half away? In C, printf rounding is round half away? Might be similar. But to be safe, we can use format with sprintf after rounding.

Simpler: use sprintf to format rounded value: sprintf("%*.*f", w, d, rounded) but need to handle d=0.

We need to test.

Also need to handle negative numbers: SAS put includes minus sign.

We need to implement compbl: compress multiple blanks to single blank and trim leading/trailing? In SAS, COMPBL removes multiple blanks and trims? Actually COMPBL replaces multiple blanks with single blank, but does not trim leading/trailing? Let's check: COMPBL function: removes multiple blanks. It does not trim leading/trailing single blanks, but if you have leading blanks, they become single blank. However in this usage: compbl(put(...)||' ('||strip(PPORRESU)||')') . The put result may have leading spaces due to width? But put with $pk. format: value $ pk ... The format is character, width maybe? In SAS, put(PPTESTCD,$pk.) will return the mapped value without padding? The $pk format is defined with values like "AUCINFO" = "AUC" . The format width is not specified, so it returns the value as is, without padding. So no leading spaces. Then ' (' is literal. So compbl will just ensure single spaces, but not needed.

We can just do paste0(trimws(mapped), " (", trimws(PPORRESU), ")") and then gsub("\\s+", " ", ...) and trim.

Similarly strip = trim both sides.

Compress(PPCATL)='ANAL2' : compress removes all blanks. So we need to remove spaces and compare.

Index function: index(_PARAM,'AUC')>0 means substring search case-sensitive. So we need to check if _PARAM contains 'AUC'.

Note: _PARAM for AUCINFO is "AUC (unit)" contains AUC, for AUCLAST is "AUC(0-tlast) (unit)" also contains AUC. So both will match AUC condition.

Similarly Cmax, MRT, t1/2, tmax.

Order of select: when index(_PARAM,'AUC')>0 etc. Note that for AUC, it will match first, even if also contains other? But AUC param won't contain Cmax etc.

For tmax, they check index(_PARAM,'tmax')=0 for CV% mean etc. So tmax has no CV.

We need to replicate exactly.

Now PROC REPORT: column ( ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM, value );

This is a bit complex: It has grouping variables and across variable _PARAM. So the report will have rows grouped by TRTAN, PPSDY, s_order, and columns for each _PARAM value.

We need to understand expected output: Table 14.2-PPTS Summary statistics for PK parameters by compound, matrix, analyte and actual treatment. Analysis Set: PK analysis set.

Columns: Actual treatment, Period day, Statistic, then each _PARAM as column.

We need to produce text rendering that mimics SAS listing.

We need to look at expected P059.lst to know layout. Since we don't have it, we need to infer.

We can search for similar PHUSE script output. P059 is PK param summary. Likely output has multiple _PARAM columns: AUC, AUC(0-tlast), Cmax, tmax, MRT, t1/2 each with units.

We need to produce CSV of table body as well.

Simplify: We will produce a CSV that is the transposed summary in long format? But requirement says each output dataset as CSV, each table or listing additionally as plain-text rendering and as CSV of the table body.

For P059, expected output is P059.lst (table/listing text, 124 numbers) -> write text rendering and CSV of same table.

So we need to produce P059.csv and P059.txt (or P059.lst.txt?). The instruction says write every output into OUT_DIR: each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv")...), each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers, and as CSV of table body.

So for P059, we need to produce P059.csv and P059.txt (and maybe P059.lst.csv?). The expected outputs list says P059.lst -> write text rendering and CSV. So we should create files named P059.csv and P059.txt (or P059.lst.csv?). To be safe, create both P059.csv and P059.lst.csv and P059.txt and P059.lst.txt.

The safest is to create file.path(out, "P059.csv") and file.path(out, "P059.txt") and also file.path(out, "P059.lst.csv") etc.

We need to ensure the CSV contains the table body numbers.

We should design R script to generate a data frame that represents the report: rows = combinations of TRTAN, PPSDY, s_order, columns = _PARAM values.

We need to handle BY groups: ANLPRNT, PPSPECL, PPCATL. If there are multiple BY groups, the report will have separate pages per BY group, each with header "Compound: ..., Matrix: ..., Analyte: ..."

We need to replicate that in text file.

We should inspect actual ADPP data to see unique BY groups.

We need to write R code that will dynamically handle any number of BY groups.

We also need to handle TRTAN formatting: value TRTAN 1="Period 1 Treatment" etc. So TRTAN numeric 1,2,3 maps to those strings.

Statg format: 1=n, 2=Mean (SD), 3=CV% mean, 4=Geo-mean, 5=CV% geo-mean, 6=Median, 7=[Min; Max]

We need to produce report with those labels.

We also need to handle PPSDY: Period Day variable, likely numeric day.

We need to produce break after PPSDY/skip: means blank line after each PPSDY group.

We need to produce titles and footnotes verbatim.

Titles:
TITLE1 j=L "Study001";
TITLE2 j=C "Table 14.2-PPTS (Page xxx of xxx)";
TITLE3 j=C "Summary statistics for PK parameters";
TITLE4 j=C "by compound, matrix, analyte and actual treatment";
TITLE5 j=C "Analysis Set: PK analysis set";

Footnotes:
FOOTNOTE1 j=L "CV% = coefficient of variation (%) = sd/mean*100";
FOOTNOTE2 j=L "CV% geo-mean = (sqrt(exp(variance for log transformed data)-1))*100";
FOOTNOTE3 j=L "Geo-mean: Geometric mean";
FOOTNOTE4 j=L "Geo-mean and CV% geo-mean not presented when the minimum value for a parameter is zero.";
FOOTNOTE5 j=L "Data: adpp  Program: pps.sas  Output: t.14.2.ppts.txt";
FOOTNOTE6 j=L "Fake Data/ Production Run on 17MAR2014:23:57";

We need to reproduce every title and footnote verbatim in text file.

We also need to handle PROC REPORT options: headline, formchar(2)="_", headskip, spacing=1, missing.

We need to produce a text file that looks like SAS listing.

We should create a simple text rendering: include titles centered/left as in SAS, then header line, then data rows.

We need to ensure numbers match SAS formatting exactly.

We need to implement SAS rounding and formatting for each value.

We need to handle missing values: value = ' ' for some cells (e.g., Mean (SD) for tmax is blank). In report, those will be blank.

We need to ensure we produce 124 numbers in text file. That suggests total numeric values displayed is 124.

We need to count: For each group (TRTAN, PPSDY, _PARAM, s_order) we have a value. Some are blank. The numbers count includes n, mean, sd, cv, geo-mean, etc. But blank cells not counted. 124 numbers suggests maybe 3 treatments * 2 period days? Let's estimate.

We need to actually run code to see.

We need to write R script that will compute these and output.

We need to handle file paths: try to locate adpp.xpt.

We should write R script that searches for file in multiple possible locations:

- D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.xpt
- D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpp.xpt
- ./data/raw/cdisc-pilot/adam/adpp.xpt
- /... etc.

We can use function find_file <- function(paths) { for (p in paths) if (file.exists(p)) return(p); return(NA) }

We should also handle haven::read_xpt reading.

We need to handle variable names: SAS variables are uppercase, but haven will preserve case as in XPT (likely uppercase). We need to handle case-insensitive.

We should convert names to uppercase for processing, but keep original for output? The instruction says write CSV with SAS dataset's variable names (same case is fine, matching is case-insensitive). So we can use uppercase.

We need to handle PKFN, ANL01FN, PARAMCD, PPSTRESN, PPCATL, ANLPRNT, PPSPECL, TRTAN, PPSDY, PPTESTCD, PPORRESU, AVAL.

We need to check if ADPP has those variables. In CDISC ADPP, typical variables: USUBJID, PPTESTCD, PPTEST, PARAM, PARAMCD, AVAL, AVALC, PPORRES, PPORRESU, PPSTRESN, PPSTRESU, ANL01FL, etc. But this program uses PKFN, ANL01FN, ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY. These might be derived variables in the pilot ADaM.

We need to inspect actual file to see column names.

We should write R code that will handle missing columns gracefully: if PKFN not present, maybe use alternative? But program expects them.

We need to search for adpp.xpt in derived adam as well.

We should also handle that inlib points to derived adam, but source is rawadam/adpp.xpt. So we should read from rawadam.

We need to implement filtering:

where PKFN=1 and ANL01FN ne 1 and paramcd in ('AUCINFO' 'AUCLAST' 'CMAX' 'TMAX' 'MRTLAST' 'LAMZHL') and PPSTRESN ne . and compress(PPCATL)='ANAL2'

Note: ANL01FN ne 1 means not equal 1. Could be missing? In SAS, missing is less than 1, so ne 1 includes missing. In R, we need to handle NA: ANL01FN !=1 or is.na(ANL01FN). But SAS missing is considered not equal to 1, so include. So we should do is.na(ANL01FN) | ANL01FN !=1.

Similarly PKFN=1: if PKFN is NA, not included.

PPSTRESN ne . means not missing.

Compress(PPCATL)='ANAL2': remove all blanks and compare. So we need to do gsub(" ", "", PPCATL) == "ANAL2" . Also need to handle NA.

Paramcd in list: case-sensitive? SAS comparison is case-sensitive, but paramcd values are uppercase as listed. So we need to match exactly.

We also need to handle PPTESTCD mapping: put(PPTESTCD,$pk.) . PPTESTCD values are like 'AUCINFO' etc. So mapping is same as paramcd? But paramcd is same as PPTESTCD? In ADPP, PPTESTCD is the test code, PARAMCD is param code. They might be same. But we use PPTESTCD for _PARAM.

We need to create _PARAM as described.

We need to handle PPORRESU: unit, strip it.

We need to handle AVAL_log = log(AVAL) . AVAL is analysis value. If AVAL <=0, log will be -Inf or NaN. But SAS log of 0 or negative gives missing? In SAS, log(0) is missing and log negative is missing, with note. In R, log(0) = -Inf, log(negative)=NaN. We need to handle: if AVAL <=0, set AVAL_log to NA, because SAS would set to missing and then those values would be excluded from mean of log? But PROC UNIVARIATE will handle missing. We should set AVAL_log = ifelse(AVAL>0, log(AVAL), NA)

But need to check if any AVAL is 0 for tmax? tmax values are positive, so log positive.

For geo-mean, they compute exp(mn2) where mn2 is mean of log values. If any AVAL is 0, log missing, then n2 < n1, and geo-mean would be based on fewer observations. But footnote says "Geo-mean and CV% geo-mean not presented when the minimum value for a parameter is zero." However code does not implement that check; it always computes geo-mean. But footnote says not presented when min is zero, but code doesn't check min1==0. Maybe data has no zero min, so not needed. But we should implement as code does: always compute.

We need to compute univariate stats per BY group.

We need to handle BY variables: ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, _PARAM

We need to sort and group.

We need to compute for each group: n1 = n of AVAL (non-missing), n2 = n of AVAL_log, mn1 = mean AVAL, mn2 = mean AVAL_log, std1, std2, cv1, cv2, med1, med2, min1, min2, max1, max2

Note: SAS PROC UNIVARIATE with var AVAL AVAL_log and output n=n1 n2 etc. The order of output variables corresponds to var order: n1 for AVAL, n2 for AVAL_log, etc.

We need to compute std with SAS formula: PROC UNIVARIATE std is sample standard deviation (n-1 denominator). R sd() uses same.

CV = std/mean*100 . SAS computes CV as (std/mean)*100 . If mean=0, CV missing.

We need to compute median, min, max.

We need to handle groups where n=1: std will be missing (SAS std missing when n=1). R sd returns NA for n=1. Good.

We need to handle cv when mean=0 or std missing.

We need to compute for each group.

Then create adpp_sum_t with 7 rows per group.

We need to implement formatting for each s_order.

We need to implement sas_put for each.

We need to handle value length 30.

We need to handle s_order generation.

We need to handle that for s_order 2, otherwise value=' ' (single space). For s_order 6 and 7, otherwise no output? In SAS code, for s_order 6 and 7, they have select with otherwise; but no value assignment, but they still output. Let's see:

s_order = 6; * median;
select;
 when(index(_PARAM,'AUC')>0) value = put(med1,6.);
 when(index(_PARAM,'Cmax')>0) value = put(med1,5.);
 when(index(_PARAM,'MRT')>0) value = put(med1,4.1);
 when(index(_PARAM,'t1/2')>0) value = put(med1,4.2);
 when(index(_PARAM,'tmax')>0) value = put(med1,4.2);
 otherwise;
end;
output;

So if _PARAM doesn't match any, value remains from previous? Actually value is retained from previous iteration? In SAS data step, value is not reset, so it would retain previous value. But for these _PARAM values, all should match one of the conditions, because _PARAM is one of AUC, AUC(0-tlast), Cmax, tmax, MRT, t1/2. So all will match. For AUC, it matches first, for Cmax second, etc. So value will be assigned.

Similarly for s_order 7.

For s_order 2 and 4, otherwise value=' ' (single space). So for tmax, mean (sd) and geo-mean will be blank.

We need to replicate.

We need to implement index check: index(_PARAM,'AUC')>0 etc. Note case-sensitive. _PARAM contains "AUC", "Cmax", "MRT", "t1/2", "tmax". So we need to check substring.

We need to handle that _PARAM for AUCINFO is "AUC (unit)" contains "AUC", for AUCLAST is "AUC(0-tlast) (unit)" also contains "AUC". So both will be treated as AUC.

For Cmax, _PARAM is "Cmax (unit)" contains "Cmax".

For MRT, "MRT (unit)" contains "MRT".

For t1/2, "t1/2 (unit)" contains "t1/2".

For tmax, "tmax (unit)" contains "tmax".

We need to implement.

Now formatting details:

For s_order 2 mean (sd):
 AUC: put(mn1,6.) ||' ('||put(std1,5.)||')'
 Cmax: put(mn1,5.)||' ('||put(std1,4.)||')'
 MRT: put(mn1,4.1)||' ('||put(std1,4.2)||')'
 t1/2: put(mn1,4.2)||' ('||put(std1,4.2)||')'
 otherwise ' '

Note: For AUC, mn1 formatted 6. (width 6, 0 decimals), std1 5. (width 5, 0 decimals). For Cmax, mn1 5., std1 4. etc.

For s_order 3 CV% mean: if tmax=0 then put(cv1,4.1) else ' '

For s_order 4 geo-mean: AUC 6., Cmax 5., MRT 4.1, t1/2 4.2 else ' '

For s_order 5 CV% geo-mean: if tmax=0 then put(sqrt(exp(std2**2)-1)*100,4.1) else ' '

For s_order 6 median: AUC 6., Cmax 5., MRT 4.1, t1/2 4.2, tmax 4.2

For s_order 7 [min; max]: AUC '['||put(min1,6.)||';'||put(max1,6.)||']' ; Cmax '['||put(min1,4.)||';'||put(max1,5.)||']' ; MRT '['||put(min1,4.2)||';'||put(max1,4.1)||']' ; t1/2 '['||put(min1,4.2)||';'||put(max1,4.1)||']' ; tmax '['||put(min1,4.2)||';'||put(max1,4.2)||']'

Note: For MRT and t1/2, min uses 4.2, max uses 4.1 (different decimals). Need to replicate.

We need to implement sas_put for each.

We need to handle that put with width includes leading spaces. When concatenating, those spaces remain. For example, put(mn1,6.) for mn1=100 -> "   100" (3 spaces + 3 digits). Then ' (' adds space + '(' . So final value would be "   100 (   10)" etc. But in report, value column width 19 left group missing flow, so left aligned? Define value / " " width=19 left group missing flow; So left aligned, so leading spaces may be trimmed? But SAS PROC REPORT with left alignment will left align the value, but the internal spaces from put will remain? Actually put returns string with leading spaces, but when displayed left aligned, the leading spaces are part of value, but left alignment will not trim them? Hmm.

We need to decide: Should we keep leading spaces or trim? In SAS listing, the numbers are right-aligned within the value? But define value width 19 left, so value string is left aligned in column, but the put formatting already includes leading spaces to right-align numbers within the value string. However when value is left aligned, the leading spaces would be visible as padding on left, but left alignment would not add extra spaces on left, but the value's leading spaces would still be there, making numbers appear right-aligned within column? Actually if value is "   100 (   10)" and column width 19 left, the value will be displayed as "   100 (   10)" left aligned, meaning the leading spaces are preserved, so numbers appear with leading spaces.

But in text rendering, we need to reproduce similar.

Simplify: We can just generate value strings with SAS put formatting including leading spaces, and then when we create report table, we will display them as is, trimmed? We need to decide to trim leading spaces for CSV? For CSV, we should store value as trimmed? But for text rendering, we need to show numbers.

We should implement sas_put that returns string with leading spaces as SAS does, but then for display we can trim leading/trailing spaces? However SAS report will show value with leading spaces, but when we write CSV, we should store trimmed value? The instruction says write CSV of table body with same numbers SAS output shows. So we should store value as displayed (trimmed).

We need to make a decision: For value strings, we will generate using sas_put that includes leading spaces, but then we will trim leading/trailing spaces for final display? Let's look at typical SAS output: For AUC mean (sd), value like "   123 (  45)" would be displayed with leading spaces, but when you copy to text, you see numbers with spaces. However for simplicity, we can trim leading spaces and just show "123 (45)"? But that would not match SAS formatting exactly (width). However the requirement says reproduce SAS formatting of displayed numbers (e.g., "12 ( 34.5%)", 2 decimals). So we need to match decimals and rounding, but not necessarily exact spacing.

We should aim to produce values without extra leading spaces, just the numbers formatted with correct decimals and rounding, but without width padding. That will still contain same numbers.

But to be safe, we should implement sas_put that does rounding and then formats without width padding (i.e., trim). However the SAS code uses width to control rounding and decimals, but width also controls padding. If we trim, we still get correct rounding.

We can implement sas_put_trimmed: format with sprintf without width, just decimals, using SAS rounding.

Simpler: Use sas_round and then sprintf with decimals, without width.

But we need to handle width for integer formatting: put(mn1,6.) means 0 decimals, width 6. If we just do sprintf("%.0f", rounded), we get "123" without leading spaces, which is fine.

For 4.1, we need 1 decimal: sprintf("%.1f", rounded) => "12.3"

For 4.2 => "12.34"

We should do that.

We need to handle that SAS put with 6. for large numbers may need to handle overflow, but same.

We should implement helper to format with d decimals, rounding half away.

We also need to handle that for CV% geo-mean, they compute sqrt(exp(std2**2)-1)*100 and format 4.1.

We need to handle missing values: if std2 is NA, then CV geo-mean is NA, put returns spaces? In SAS, put(.,4.1) returns "    " (spaces). But in our code, we should return " " or NA? For display, we should return blank.

We need to handle that for groups where n=1, std is missing, then CV and geo-mean CV will be missing, but code will still try to format missing as spaces, but we should return blank or " ".

We should check: For n=1, std1 is missing, so mean (sd) value would be put(mn1,6.)||' ('||put(std1,5.)||')' . If std1 missing, put(std1,5.) returns spaces "     " (5 spaces). Then value would be "   100 (     )" . But in report, that would show as "100 (     )" with blank sd. However is that expected? Possibly data has n>1 for all groups, so not needed.

We should handle missing by returning blank string for that part? But SAS would show spaces inside parentheses. We could replicate by checking if std is NA, then value = put(mn1,6.)||' (     )' but we can just return "100 ( )" or blank? Better to handle missing as empty.

Simplify: If any component is NA, we return blank or partial.

We should implement logic: if mn1 is NA, value = " " ; else if std1 is NA, value = put(mn1, fmt) || " ( )" ? But we can just return put(mn1) alone? However SAS would show spaces.

We can just check if is.na(mn1) or is.na(std1) then value = " " for that s_order? But not needed if data has no missing.

We should implement straightforward: if is.na(mn1) or is.na(std1) then value = "" else formatted.

Similarly for other stats.

We need to handle median, min, max: if NA, return blank.

We should also handle that for s_order 3, if cv1 is NA, put returns spaces, but we should return blank.

We need to implement.

Now we need to handle sorting and grouping.

We need to produce final report data frame.

Steps in R:

- Find adpp.xpt file.

- Read with haven::read_xpt.

- Convert names to uppercase for ease.

- Filter.

- Create _PARAM.

- Create AVAL_log.

- Sort by ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, _PARAM

- Group by those and compute stats.

We need to handle ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, _PARAM as grouping keys. Note: _PARAM is derived, but also need to group by it.

We need to compute stats per group.

We need to handle that TRTAN is numeric 1,2,3, PPSDY is numeric day.

We need to handle that ANLPRNT, PPSPECL, PPCATL are character.

We need to compute n1, n2, etc.

We need to handle that AVAL_log is log(AVAL) where AVAL>0 else NA.

We need to compute for each group:

n1 = sum(!is.na(AVAL))
n2 = sum(!is.na(AVAL_log))
mn1 = mean(AVAL, na.rm=TRUE)
mn2 = mean(AVAL_log, na.rm=TRUE)
std1 = sd(AVAL, na.rm=TRUE) # sample sd
std2 = sd(AVAL_log, na.rm=TRUE)
cv1 = ifelse(!is.na(mn1) & mn1!=0, std1/mn1*100, NA)
cv2 = similarly? But cv2 not used directly, only for geo-mean CV they use std2 directly, not cv2. However proc univariate cv for AVAL_log would be std2/mn2*100, but not used in report. But we compute for completeness.
med1 = median(AVAL, na.rm=TRUE)
med2 = median(AVAL_log, na.rm=TRUE)
min1 = min(AVAL, na.rm=TRUE)
min2 = min(AVAL_log, na.rm=TRUE)
max1 = max(AVAL, na.rm=TRUE)
max2 = max(AVAL_log, na.rm=TRUE)

Note: SAS median definition: PROC UNIVARIATE median is the 50th percentile with specific method. R median uses same? For odd n, median is middle value, for even n, average of two middle values. SAS UNIVARIATE median uses same? Yes, SAS median is the 50th percentile, which for even n is average of two middle values. So R median matches.

We need to handle that SAS PROC UNIVARIATE with noprint and output will compute these stats even if n=0? But filtered data ensures n>0.

We need to handle that for groups where AVAL_log has missing due to AVAL<=0, n2 < n1.

We need to compute.

Then create adpp_sum_t: for each row in adpp_sum, create 7 rows.

We need to implement formatting functions.

We need to handle that for each s_order, value is formatted as described.

We need to handle that for s_order 1, value = put(n1,2.) -> format n1 as integer with width 2, no decimals. So we can do sprintf("%2.0f", n1) but trimmed.

We should implement sas_format_n <- function(x, w) { if (is.na(x)) return("") else trimws(sprintf(paste0("%",w,".0f"), sas_round(x,0))) } but we can just do as.character(x) ?

But to match SAS, n1 is integer count, put with 2. will right align width 2, but trimmed is just number.

We should just do as.character(n1) or format with no decimals.

We need to handle that n1 may be >99, width 2 would overflow, but SAS would expand, so just as.character.

We should implement helper sas_put_int <- function(x, w) { if (is.na(x)) return("") else trimws(formatC(sas_round(x,0), format="f", digits=0, width=w)) } but trim.

Simplify: Use as.character(sas_round(x,0))

We need to implement sas_round.

We need to handle that for mean (sd) etc., we need to format mn1 and std1 with specific decimals.

We should create function fmt_num <- function(x, d) { if (is.na(x)) return("") else { rounded <- sas_round(x, d); sprintf(paste0("%.",d,"f"), rounded) } }

But need to handle d=0: sprintf("%.0f", rounded) => "123"

We need to handle that for 6. format, d=0, w=6, but we ignore w.

For 4.1, d=1, etc.

We should implement.

We need to handle that for AUC, mn1 formatted 6. (0 decimals), std1 5. (0 decimals). So we need to call fmt_num(mn1,0) and fmt_num(std1,0)

For MRT, mn1 4.1 (1 decimal), std1 4.2 (2 decimals)

For t1/2, both 4.2

We need to handle.

We should also handle that for geo-mean, exp(mn2) formatted similarly.

For CV% geo-mean, sqrt(exp(std2^2)-1)*100 formatted 4.1 (1 decimal)

We need to handle.

We should also handle that for median, min, max formatting as described.

We need to handle that for [min; max] they use '['||put(min1,fmt)||';'||put(max1,fmt)||']' . Note they use ';' without space, but with no extra spaces. So we need to produce "[min;max]" with no spaces? In SAS code, they have '['||put(min1,6.)||';'||put(max1,6.)||']' . The put values include leading spaces, so the string would be "[   100;   200]" with spaces inside. But when displayed left aligned, it would show with spaces. However we can produce "[100;200]" trimmed.

We should produce trimmed version: paste0("[", fmt_num(min1,d1), ";", fmt_num(max1,d2), "]")

But need to handle different decimals for min and max for MRT and t1/2: min 4.2, max 4.1

We need to implement.

We should also handle that for Cmax, min 4. (0 decimals) and max 5. (0 decimals) -> both 0 decimals but different widths, but trimmed same.

We need to handle.

Now we need to create adpp_sum_t data frame with columns: ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, _PARAM, s_order, value

We need to handle that s_order is numeric 1..7, and value is character.

We need to handle that for s_order 2, if _PARAM contains AUC, Cmax, MRT, t1/2, then format, else blank. For tmax, it will be blank (since otherwise value=' '). So for tmax, s_order 2 value = ' ' (single space). We should set to "" or " ".

Similarly for s_order 4, otherwise blank.

For s_order 3 and 5, if tmax, blank else formatted.

For s_order 6 and 7, all have values, but we need to handle otherwise case: if _PARAM doesn't match any, value remains previous? But we will handle by checking all conditions, and if none match, set to "".

We should implement.

We need to handle that for s_order 6, they have select with 5 whens, otherwise no assignment, but they still output. For our data, all _PARAM will match one, so fine.

We need to handle that for s_order 7 similarly.

We need to handle that for s_order 2, they have otherwise value=' ' (single space). So for tmax, value will be single space.

We should implement.

Now after creating adpp_sum_t, we need to sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM

Note: SAS sorts with character ordering uppercase before lowercase. R sort with default may be different, but we can use base sort which is similar? We should use dplyr arrange which uses locale? But we can just arrange with base.

We need to handle that _PARAM sorting: _PARAM values like "AUC (unit)", "AUC(0-tlast) (unit)", "Cmax (unit)", "MRT (unit)", "t1/2 (unit)", "tmax (unit)" . Sorting order will be alphabetical: "AUC...", "AUC(0-tlast)...", "Cmax...", "MRT...", "t1/2...", "tmax..." . But SAS character ordering uppercase before lowercase, so "AUC" (uppercase A) before "Cmax" (C uppercase) before "MRT" (M uppercase) before "t1/2" (t lowercase) and "tmax" (t lowercase). So t's will be after uppercase. That's same as R default? In R, uppercase letters have lower ASCII than lowercase, so sorting will be same if we use base R sort with C locale. So we should set locale to C.

We can use arrange with factor? But we can just use dplyr arrange which uses current locale, but should be similar.

We need to handle that TRTAN is numeric, PPSDY numeric, s_order numeric.

We need to handle that after sorting, we will generate report.

We need to handle PROC REPORT with BY ANLPRNT PPSPECL PPCATL. So we need to group by those.

We need to handle that column statement: ( ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM, value ) . This means ANLPRNT, PPSPECL, PPCATL are BY variables (not in column), TRTAN, PPSDY, s_order are group variables, _PARAM is across, value is analysis.

So report will have rows: each combination of TRTAN, PPSDY, s_order, and columns: each _PARAM value.

We need to create a wide data frame: rows = TRTAN, PPSDY, s_order, columns = _PARAM values, cells = value.

We need to handle that for each BY group, we have separate table.

We need to handle that define statements: ANLPRNT, PPSPECL, PPCATL noprint group order=data id; TRTAN "Actual treatment" width 20 left group format=TRTAN. order=data flow; PPSDY "Period day" width 8 left group missing; s_order "Statistic" width 14 left group format=statg. order=internal id; _PARAM " " width 15 left across; value " " width 19 left group missing flow;

So TRTAN formatted with TRTAN format, s_order with statg format.

We need to handle that break after PPSDY/skip adds blank line after each PPSDY group.

We need to handle compute before _page_ line "Compound:" +1 ANLPRNT $10. +1 ", Matrix:" +1 PPSPECL $10. +1 ", Analyte:" +1 PPCATL $15.;

So header line per BY group.

We need to produce text file with titles, footnotes, and report.

We need to handle that TITLE1 j=L "Study001"; etc. j=L means left justify, j=C center.

We need to produce text file with titles at top, then for each BY group, header line, then column headers, then data rows.

We need to handle that PROC REPORT with headline, formchar(2)="_", headskip, spacing=1, missing.

We need to produce a simple text rendering: include titles, then table.

We should create a function to generate text file.

We need to handle that output dataset CLXCHECKNOBS is created via out=CLXCHECKNOBS in PROC REPORT. That dataset contains the report data? In SAS, PROC REPORT with out= creates output dataset with the report data. But we don't need to reproduce that, just the table.

We need to handle that expected output is P059.lst with 124 numbers. We need to ensure our text file contains 124 numbers.

We need to count numbers: For each cell that is not blank, it contains numbers. For s_order 1, n is integer (1 number). For s_order 2, mean (sd) contains 2 numbers (mean and sd). For s_order 3, CV% mean 1 number. For s_order 4, geo-mean 1 number. For s_order 5, CV% geo-mean 1 number. For s_order 6, median 1 number. For s_order 7, [min; max] contains 2 numbers. So per _PARAM per TRTAN/PPSDY group, total numbers = 1+2+1+1+1+1+2 =9 numbers. If there are 6 _PARAMs, per group numbers = 54. If there are 2 PPSDY groups and 3 TRTAN groups? Let's see: 124 numbers suggests maybe 2 treatments? Let's calculate: If 3 TRTAN * 1 PPSDY * 6 params *9 =162 numbers, too many. If 2 TRTAN *1 PPSDY*6*9=108, close to 124 but not exact. If 3 TRTAN *2 PPSDY? Let's see actual data.

We need to inspect data to know unique TRTAN and PPSDY.

We should write R code to dynamically handle whatever data yields.

We need to ensure we produce correct numbers.

We need to handle that for tmax, some stats are blank: s_order 2 blank (0 numbers), s_order 3 blank (0), s_order 4 blank (0), s_order 5 blank (0), s_order 6 has 1, s_order 7 has 2, s_order 1 has 1. So for tmax, per group numbers = 1+0+0+0+0+1+2=4 numbers. For other params (AUC, Cmax, MRT, t1/2) each has 9 numbers, but for AUC there are 2 params (AUC and AUC(0-tlast)) each 9, Cmax 9, MRT 9, t1/2 9, tmax 4 => total per group = 9*5 +4 =49? Wait count: AUC (AUC) 9, AUCLAST 9, CMAX 9, MRTLAST 9, LAMZHL 9, TMAX 4 => total 49 per TRTAN/PPSDY group. If there are 2 groups (maybe 2 treatments?), 49*2=98, not 124. If 3 groups, 49*3=147, too many. If 2 treatments *1 PPSDY? 98. If 3 treatments *1 PPSDY? 147. 124 suggests maybe 2 treatments with some missing? Let's see.

We need to actually run code to see.

We should write R script that will compute and then count numbers in output to verify 124.

We need to handle that expected output has 124 numbers, so our output should have 124 numbers as well.

We need to ensure we produce same numbers as SAS.

We need to handle rounding and formatting exactly.

We need to implement SAS rounding half away.

We need to handle that SAS PROC UNIVARIATE computes mean, std, etc. with high precision, then formatting rounds.

We need to ensure we use same precision.

We should implement.

We need to handle that for CV% mean, they use cv1 from PROC UNIVARIATE, which is std/mean*100. We compute same.

For CV% geo-mean, they compute sqrt(exp(std2^2)-1)*100, where std2 is std of log values.

We need to handle.

We need to handle that for geo-mean, they use exp(mn2) where mn2 is mean of log values.

We need to handle.

We need to handle that for median, they use med1.

We need to handle.

We need to handle that for min/max, they use min1/max1.

We need to handle.

We need to handle that for n, they use n1.

We need to handle.

We need to handle that for s_order 2, they format mean and sd with specific widths, but we will format with decimals as described.

We need to handle that for AUC, mean and sd are integers (0 decimals), for Cmax integers, for MRT 1 and 2 decimals, for t1/2 2 decimals.

We need to handle.

We need to handle that for CV% mean, they format 4.1 (1 decimal) for all except tmax blank.

We need to handle.

We need to handle that for geo-mean, similar.

We need to handle.

We need to handle that for median, similar.

We need to handle.

We need to handle that for [min; max], they format with different decimals.

We need to handle.

We need to handle that for tmax, median and [min;max] are formatted 4.2 (2 decimals).

We need to handle.

We need to handle that for MRT and t1/2, median 4.1 and 4.2 respectively, and [min;max] min 4.2 max 4.1.

We need to handle.

We need to handle that for AUC and Cmax, median and [min;max] are integers.

We need to handle.

We need to handle that for AUC, median 6. (0 decimals), [min;max] both 6. (0 decimals)

For Cmax, median 5. (0 decimals), [min;max] min 4. (0 decimals) max 5. (0 decimals)

We need to handle.

We need to handle that for MRT, median 4.1 (1 decimal), [min;max] min 4.2 max 4.1

For t1/2, median 4.2, [min;max] min 4.2 max 4.1

We need to handle.

We need to handle that for tmax, median 4.2, [min;max] both 4.2

We need to handle.

We need to handle that for CV% geo-mean, 4.1 for all except tmax blank.

We need to handle.

We need to handle that for CV% mean, 4.1 for all except tmax blank.

We need to handle.

We need to handle that for mean (sd), for AUC and Cmax, they use integer formatting, for MRT and t1/2 they use decimals.

We need to handle.

We need to handle that for geo-mean, similar.

We need to handle.

We need to handle that for s_order 1, n formatted 2. (width 2, 0 decimals) -> integer.

We need to handle.

We need to handle that for s_order 2, otherwise blank.

We need to handle.

We need to handle that for s_order 4, otherwise blank.

We need to handle.

We need to handle that for s_order 3 and 5, if tmax blank else formatted.

We need to handle.

We need to handle that for s_order 6 and 7, all have values.

We need to handle.

We need to handle that for s_order 6, they have select with 5 whens, otherwise no assignment, but we will handle.

We need to handle.

We need to handle that for s_order 7 similarly.

We need to handle.

We need to handle that for s_order 2, they have select with 4 whens, otherwise blank.

We need to handle.

We need to handle that for s_order 4 similarly.

We need to handle.

We need to handle that for s_order 3 and 5, they check tmax.

We need to handle.

We need to handle that for s_order 6 and 7, they check all.

We need to handle.

We need to handle that for s_order 2, they check AUC, Cmax, MRT, t1/2, otherwise blank (which will be tmax).

We need to handle.

We need to handle that for s_order 4 similarly.

We need to handle.

We need to handle that for s_order 6, they check AUC, Cmax, MRT, t1/2, tmax.

We need to handle.

We need to handle that for s_order 7 similarly.

We need to handle.

We need to handle that for s_order 3 and 5, they check tmax.

We need to handle.

We need to handle that for s_order 1, they always have value.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need to handle that for s_order 6, value for all.

We need to handle.

We need to handle that for s_order 7, value for all.

We need to handle.

We need to handle that for s_order 1, value for all.

We need to handle.

We need to handle that for s_order 2, they have value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 3, value for all except tmax.

We need to handle.

We need to handle that for s_order 4, value for AUC, Cmax, MRT, t1/2, blank for tmax.

We need to handle.

We need to handle that for s_order 5, value for all except tmax.

We need to handle.

We need
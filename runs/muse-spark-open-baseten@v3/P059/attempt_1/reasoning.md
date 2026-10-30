Need to translate SAS program P059_pk_param_summary.sas to R.

We have inputs: adpp.xpt in rawadam. Also inlib points to derived adam. But program uses source.adpp from rawadam/adpp.xpt.

We need to reproduce outputs: P059.lst (table/listing text, 124 numbers) -> write text rendering and CSV.

We need to understand SAS logic.

Steps:

- proc format defines TRTAN, statg, $pk.

- data adpp: set source.adpp where PKFN=1 and ANL01FN ne 1 and paramcd in ('AUCINFO' 'AUCLAST' 'CMAX' 'TMAX' 'MRTLAST' 'LAMZHL') and PPSTRESN ne . and compress(PPCATL)='ANAL2'; _PARAM = compbl(put(PPTESTCD,$pk.)||' ('||strip(PPORRESU)||')'); AVAL_log = log(AVAL);

Note: paramcd list includes AUCINFO etc. But PPTESTCD mapping via $pk. Need to check.

- proc sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM

- proc univariate by same groups var AVAL AVAL_log output n, mean, std, cv, median, min, max. Note cv in SAS PROC UNIVARIATE is coefficient of variation = std/mean*100 . Need to verify.

Output dataset adpp_sum has n1 n2 mean mn1 mn2 etc. For AVAL and AVAL_log.

- data adpp_sum_t transposed: length value $30; by ANLPRNT PPSPECL PPCATL; s_order 1..7 with formatting.

We need to replicate SAS PUT formatting exactly.

SAS PUT with format w.d : e.g., put(n1, 2.) -> width 2, no decimals? Actually 2. means width 2. For integer n. SAS will right align. In R we need to emulate? But final table display via PROC REPORT will show value as is. The value strings include leading spaces due to width. Need to reproduce numbers.

But for output CSV and txt, we need to show same numbers. The spec says reproduce SAS formatting of displayed numbers. Need to implement SAS PUT behavior: w.d format rounds half away from zero? And pads with spaces to width.

However PROC REPORT with width=19 will display value left aligned? define value / width 19 left group missing flow. So leading spaces from PUT may be trimmed? In SAS, PUT returns string of length w with leading spaces. But when displayed in report, left alignment may trim? Need to check expected output P059.lst.

We don't have expected output file to inspect, but we can infer.

We need to locate raw data to test.

We need to write R script that reads adpp.xpt, filters, computes stats, formats.

We need to handle SAS formats:

- TRTAN format: 1->Period 1 Treatment etc.

- statg: 1=n etc.

- $pk: mapping PPTESTCD to display name.

List:

"AUCINFO" = "AUC"
"AUCTAU" = "AUCtau"
"AUCLAST" = "AUC(0-tlast)"
"CMAX" = "Cmax"
"CMAXD" = "Cmax/Dose"
"MRTLAST" = "MRT"
"TLAST" = "tlast"
"TMAX" = "tmax"
"CLRFO" = "CL/F"
"VZFO" = "VzF"
"LAMZ" = "lambdaZ"
"LAMZHL" = "t1/2"
"CLAST" = "Clast"

Only those in filter matter: AUCINFO -> AUC, AUCLAST -> AUC(0-tlast), CMAX -> Cmax, TMAX -> tmax, MRTLAST -> MRT, LAMZHL -> t1/2

Then _PARAM = compbl(put(PPTESTCD,$pk.)||' ('||strip(PPORRESU)||')')

So e.g., "AUC (h*ng/mL)" etc. Need to see PPORRESU values.

- Then s_order formatting:

s_order=1 value = put(n1, 2.)

s_order=2 mean (sd):
 when index(_PARAM,'AUC')>0 value = put(mn1,6.) ||' ('||put(std1,5.) ||')';
 when index(_PARAM,'Cmax')>0 value = put(mn1,5.) ||' ('||put(std1,4.) ||')';
 when index(_PARAM,'MRT')>0 value = put(mn1,4.1)||' ('||put(std1,4.2)||')';
 when index(_PARAM,'t1/2')>0 value = put(mn1,4.2)||' ('||put(std1,4.2)||')';
 otherwise value=' ';

Note index is case-sensitive? SAS index is case-sensitive. _PARAM contains "AUC", "Cmax", "MRT", "t1/2", "tmax". So matches.

s_order=3 CV% mean: if index(_PARAM,'tmax')=0 then value = put(cv1,4.1); else ' ';

s_order=4 geo-mean:
 when AUC -> put(exp(mn2),6.)
 when Cmax -> put(exp(mn2),5.)
 when MRT -> put(exp(mn2),4.1)
 when t1/2 -> put(exp(mn2),4.2)
 otherwise ' '

s_order=5 CV% geo-mean: if tmax=0 then put(sqrt(exp(std2**2)-1)*100,4.1) else ' '

s_order=6 median:
 when AUC -> put(med1,6.)
 when Cmax -> put(med1,5.)
 when MRT -> put(med1,4.1)
 when t1/2 -> put(med1,4.2)
 when tmax -> put(med1,4.2)

s_order=7 [min; max]:
 when AUC -> '['||put(min1,6.) ||';'||put(max1,6.) ||']'
 when Cmax -> '['||put(min1,4.) ||';'||put(max1,5.) ||']'
 when MRT -> '['||put(min1,4.2)||';'||put(max1,4.1)||']'
 when t1/2 -> '['||put(min1,4.2)||';'||put(max1,4.1)||']'
 when tmax -> '['||put(min1,4.2)||';'||put(max1,4.2)||']'

Need to implement SAS PUT w.d rounding half away from zero and width padding.

SAS PUT with w.d: width w includes decimal point and sign. If number doesn't fit, SAS will try to expand? But we can just format with sprintf and pad.

For example put(mn1,6.) means width 6, 0 decimals? Actually format 6. means w=6 d=0? In SAS, format w.d where d omitted means 0? But they use 6. meaning width 6 with no decimal. So integer rounding.

Similarly 5. etc.

4.1 means width 4 with 1 decimal.

We need to implement function sas_put(x, w, d) that rounds half away from zero to d decimals, then formats with width w, right-aligned with spaces, and if value is NA returns ""? In SAS, missing numeric put gives spaces (width spaces). But they handle otherwise blank.

Also need to handle that SAS PUT for large numbers may use scientific? No.

We also need to handle that SAS CV from PROC UNIVARIATE: cv = std/mean*100 . Need to compute same.

Also need to handle that PROC UNIVARIATE output n, mean, std, cv, median, min, max. For each var. n1 is count of non-missing AVAL, n2 for AVAL_log. Since AVAL_log = log(AVAL), if AVAL>0 then log defined, else missing. But filter PPSTRESN ne . and AVAL presumably >0? Might be zero? But they note geo-mean not presented when min zero - but they still compute? Actually they compute but footnote says not presented when min zero. However code still computes; but if AVAL=0 then log missing, n2 < n1, and geo-mean would be based on subset. But they still output? The footnote says not presented when min zero, but code doesn't implement that logic; maybe data has no zero.

We need to replicate.

Also need to handle sorting and BY groups.

Then proc sort data=adpp_sum_t by ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM

Then PROC REPORT with BY ANLPRNT PPSPECL PPCATL, columns TRTAN PPSDY s_order _PARAM,value across.

So table is by compound, matrix, analyte. Within each BY group, rows are by TRTAN, PPSDY, s_order, and columns are _PARAM values.

We need to produce text rendering similar to SAS listing.

We need to find actual data to see unique values.

We should write R script that does all steps, then writes CSV and TXT.

We need to handle OUT_DIR env var.

We need to produce P059.lst counterparts: write text rendering and CSV of same table.

Expected outputs: P059.lst (table/listing text, 124 numbers) -> write text rendering and CSV.

So we need to create file.path(out, "P059.csv") and file.path(out, "P059.txt") maybe? The spec says each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv")... each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers... and as CSV of table body.

So for P059, we need to produce P059.csv and P059.txt. Possibly also CLXCHECKNOBS? But expected outputs lists only P059.lst. The PROC REPORT out=CLXCHECKNOBS creates dataset but not expected output? Might ignore.

We should produce both.

We need to implement SAS rounding half away from zero.

Define function round_half_away <- function(x, digits) { sign(x) * floor(abs(x)*10^digits + 0.5) / 10^digits } but need to handle NA.

Then sas_put <- function(x, w, d) { if is.na(x) return(strrep(" ", w))? But in our value construction, we only call when not missing; but for safety. Else format: rounded = round_half_away(x, d); then formatted = formatC(rounded, format="f", digits=d, width=w, flag=" ") ??? But formatC with width pads left with spaces. Need to ensure exactly w characters, with decimal point. For d=0, no decimal point. Use sprintf with width.

Simpler: sprintf(paste0("%", w, ".", d, "f"), rounded) . This will pad left with spaces to width w. For d=0, sprintf "%6.0f" will give width 6 with no decimal.

Check: sprintf("%6.0f", 123.6) -> "   124" (width 6). Good.

For w.d where d omitted, we treat as 0.

We need to handle w includes sign.

For put(n1,2.) -> w=2 d=0.

For put(mn1,6.) -> w=6 d=0.

For put(std1,5.) -> w=5 d=0.

For put(mn1,4.1) -> w=4 d=1 etc.

We need to implement.

Also need to handle that SAS PUT for numbers that exceed width will print width+? Actually SAS will increase width to fit, not truncate. sprintf will also expand beyond width if needed (it doesn't truncate). So okay.

Now compute stats.

We need to read adpp.xpt.

Paths: rawadam = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam

But in R environment, paths may be different. However we should use absolute paths given, but they may not exist in container. We need to handle fallback: try to find file via Sys.getenv etc. The instructions say read inputs from absolute paths given in context (forward slashes). So we should attempt to read from those paths, but also try alternative if not found (like /tmp). However we can just try to read from given path; if fails, try to locate via list.files.

We need to make script robust: try multiple possible locations.

We should also handle haven::read_xpt.

We need to implement filtering: PKFN=1 and ANL01FN ne 1 and paramcd in (...) and PPSTRESN ne . and compress(PPCATL)='ANAL2'

Note compress in SAS removes spaces. So trim spaces and compare.

Also need to handle variable names case-insensitive: haven preserves case as in XPT (likely uppercase). So check.

We need to inspect data to know column names.

We will write R script that does:

- out <- Sys.getenv("OUT_DIR"); if out=="" out <- "."

- Find adpp file: try file.path(Sys.getenv("rawadam"), "adpp.xpt") etc. But we have &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam . So construct path.

We should attempt to read from that path, if not exists, try "/opt/data/..." etc. Use list of candidates.

Simplify: try to find adpp.xpt via Sys.glob.

We can implement helper find_file("adpp.xpt").

We also need to handle that inlib points to derived adam, but not needed.

Proceed to compute.

We need to compute per BY group stats: ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM

But _PARAM derived from PPTESTCD and PPORRESU.

We need to compute AVAL_log = log(AVAL)

Then group_by and summarise.

For each group, compute for AVAL and AVAL_log:

n = sum(!is.na(x))
mean = mean(x, na.rm=TRUE)
std = sd(x, na.rm=TRUE)  # SAS std is sample std (n-1 denominator) same as R sd
cv = std/mean*100  # SAS cv
median = median(x, na.rm=TRUE)
min = min(x, na.rm=TRUE)
max = max(x, na.rm=TRUE)

Note SAS PROC UNIVARIATE std uses same formula.

We need to handle groups with n=1 => std = NA (SAS would be missing). Then cv also missing. Our formatting should handle NA -> spaces.

We need to compute for both vars.

Then create adpp_sum_t rows.

We need to implement index logic: index(_PARAM,'AUC')>0 means substring "AUC" appears anywhere. Since _PARAM like "AUC (h*ng/mL)" contains AUC. Similarly "AUC(0-tlast) (...)" contains AUC. So both match.

For Cmax: _PARAM contains "Cmax" (case-sensitive). So matches.

For MRT: contains "MRT"

For t1/2: contains "t1/2"

For tmax: contains "tmax" lower case.

Note that "tmax" vs "t1/2" etc distinct.

We need to implement same case-sensitive grepl.

Now formatting details for each s_order.

We need to replicate SAS PUT exactly, including spaces inside value strings.

For s_order 2: value = put(mn1,6.) ||' ('||put(std1,5.) ||')'

So there is a space before '('? Actually ' (' includes space then '(' . So value = "<mn formatted width6><space>(<std width5>)" + ")" . Note there is no extra space after '('.

Similarly for other.

For s_order 7: '['||put(min1,6.) ||';'||put(max1,6.) ||']'  -> no spaces around ; . So "[   123;   456]"

We need to implement.

For s_order 3 and 5: put(cv1,4.1) etc.

We need to handle that if cv is NA, put returns spaces (width 4). But then value would be spaces, but they output spaces? In SAS, value length 30, so spaces. In report, blank.

We should produce trimmed? But for CSV we should trim leading/trailing spaces? The spec says reproduce SAS formatting of displayed numbers. Might need to keep as trimmed for CSV.

Better to trim leading spaces for CSV? However SAS PUT pads with leading spaces, but when displayed in report with left alignment, leading spaces may be trimmed? Let's check typical SAS report: define value / width 19 left ... So left alignment will left-justify the value string, but leading spaces from PUT would be considered part of string and left alignment would keep them? Actually PROC REPORT with left option will left-align the formatted value, but the value itself already contains leading spaces from PUT; left alignment will not trim them? Hmm.

But for CSV output, we probably want trimmed values (without extra padding) because numbers are expected to be like "  123 (  45)"? Need to decide.

We can look at expected P059.lst numbers count 124 numbers. That suggests table has many numeric cells.

We should produce TXT that mimics SAS listing with titles/footnotes and table.

We need to create a text file that contains titles, footnotes, and table.

We also need to create CSV that contains table body with columns: ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, Statistic, _PARAM, value? Or pivoted?

The PROC REPORT with across creates columns for each _PARAM. So the table has rows for each TRTAN/PPSDY/Statistic and columns for each PK parameter.

We need to decide CSV structure: likely one row per TRTAN/PPSDY/s_order with columns for each _PARAM value.

We should produce CSV that reflects the report's displayed table: columns: Actual treatment, Period day, Statistic, plus one column per _PARAM.

We need to also include BY group info.

We should produce both long and wide? The spec says "CSV of the table body" - we can provide wide.

We will produce P059.csv as wide table with columns: ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, Statistic, plus each _PARAM column.

Alternatively, we can produce long format adpp_sum_t.csv.

But to satisfy "124 numbers" we need to ensure CSV contains same numbers as TXT.

We will produce P059.csv as the report dataset (wide).

We also need to handle ordering: by ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM

We need to implement titles/footnotes verbatim:

TITLE1 j=L "Study001";
TITLE2 j=C "Table 14.2-PPTS (Page xxx of xxx)";
TITLE3 j=C "Summary statistics for PK parameters";
TITLE4 j=C "by compound, matrix, analyte and actual treatment";
TITLE5 j=C "Analysis Set: PK analysis set";

FOOTNOTE1 j=L "CV% = coefficient of variation (%) = sd/mean*100";
FOOTNOTE2 j=L "CV% geo-mean = (sqrt(exp(variance for log transformed data)-1))*100";
FOOTNOTE3 j=L "Geo-mean: Geometric mean";
FOOTNOTE4 j=L "Geo-mean and CV% geo-mean not presented when the minimum value for a parameter is zero.";
FOOTNOTE5 j=L "Data: adpp  Program: pps.sas  Output: t.14.2.ppts.txt";
FOOTNOTE6 j=L "Fake Data/ Production Run on 17MAR2014:23:57";

We need to reproduce these in TXT.

We also need compute page header: "Compound:" + ANLPRNT + ", Matrix:" + PPSPECL + ", Analyte:" + PPCATL

We should generate TXT with these.

We need to ensure we handle all BY groups.

We should write R script that does all.

We need to handle missing OUT_DIR.

We need to ensure script runs non-interactively.

We will now craft final R script.

We need to include helper functions for SAS PUT and rounding.

We need to handle reading XPT.

We need to handle variable name case: convert names to uppercase for processing, but preserve original for output? We'll work with uppercase.

We need to handle compress(PPCATL)='ANAL2' -> remove spaces and compare.

We need to handle PKFN, ANL01FN numeric.

We need to handle PPSTRESN not missing.

We need to handle paramcd filter.

We need to handle _PARAM creation: compbl = compress blanks to single space and trim. So compbl(put(PPTESTCD,$pk.)||' ('||strip(PPORRESU)||')') . put(PPTESTCD,$pk.) maps via format. Then concatenate with ' (' + strip(PPORRESU) + ')' . Then compbl removes multiple spaces.

We need to implement mapping.

We need to handle PPORRESU values may have spaces.

We need to handle ANLPRNT etc.

We need to compute stats.

We need to handle that PROC UNIVARIATE by groups requires sorted data; we will group_by.

We need to handle that adpp_sum_t has BY ANLPRNT PPSPECL PPCATL but also includes TRTAN PPSDY _PARAM etc. Actually BY in data step is by ANLPRNT PPSPECL PPCATL; but they still have TRTAN etc per observation.

We need to generate 7 rows per original summary row.

We need to handle that for s_order 2, otherwise value=' ' (single space). Similarly for others, otherwise blank? In SAS, otherwise value=' ' means single space. But for s_order 6 and 7, otherwise without assignment? In code: select; when(...) value=...; otherwise; end; output; So if no when matches, value retains previous? Actually for s_order 6 and 7, otherwise does nothing, so value retains previous value from prior s_order? But they output after each s_order block, so value would be from previous s_order if not matched. However for s_order 6, they have when for AUC, Cmax, MRT, t1/2, tmax - that covers all 6 param types in filter, so all will match. For s_order 7 similarly covers all. So otherwise not needed.

But for s_order 2, otherwise value=' ' covers tmax case (since tmax not in AUC/Cmax/MRT/t1/2). So tmax mean(sd) will be blank.

Similarly s_order 4 otherwise blank for tmax.

We need to implement.

We need to handle that for s_order 3 and 5, they set value to ' ' if tmax.

We need to implement.

We need to handle formatting for each.

We need to handle that for s_order 2, they use put(mn1,6.) etc. For AUC, width 6,0 decimals. For Cmax width5,0. For MRT width4.1 etc.

We need to implement.

We need to handle that for s_order 4 geo-mean: exp(mn2) then put with same widths as mean? For AUC 6., Cmax 5., MRT 4.1, t1/2 4.2

We need to handle that for s_order 5 CV% geo-mean: sqrt(exp(std2^2)-1)*100 then put 4.1

We need to handle that for s_order 6 median: similar widths.

We need to handle that for s_order 7 min/max: for AUC both 6., for Cmax min 4. max 5., for MRT min 4.2 max 4.1, for t1/2 same, for tmax both 4.2

We need to implement.

We need to handle that for s_order 7, they use '['||put(min1,6.)||';'||put(max1,6.)||']' -> no spaces.

We need to implement.

We need to handle that for s_order 1, put(n1,2.) -> width2.

We need to handle that for s_order 3, put(cv1,4.1) -> width4.1

We need to handle that for s_order 5, put(...,4.1)

We need to handle that for s_order 6, put(med1,6.) etc.

We need to handle that for s_order 7, put(min1,6.) etc.

We need to handle that for s_order 2, put(mn1,6.) etc.

We need to handle that for s_order 4, put(exp(mn2),6.) etc.

We need to handle that for s_order 5, put(sqrt(exp(std2^2)-1)*100,4.1)

We need to handle that for s_order 6, put(med1,6.) etc.

We need to handle that for s_order 7, put(min1,6.) etc.

We need to handle that for s_order 2, put(std1,5.) etc.

We need to handle that for s_order 2, put(std1,4.) etc.

We need to handle that for s_order 2, put(std1,4.2) etc.

We need to handle that for s_order 2, put(mn1,4.1) etc.

We need to handle that for s_order 2, put(mn1,4.2) etc.

We need to handle that for s_order 4, put(exp(mn2),4.1) etc.

We need to handle that for s_order 4, put(exp(mn2),4.2) etc.

We need to handle that for s_order 6, put(med1,4.1) etc.

We need to handle that for s_order 6, put(med1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.1) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.) etc.

We need to handle that for s_order 7, put(max1,5.) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.1) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.1) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for s_order 7, put(max1,4.2) etc.

We need to handle that for s_order 7, put(min1,4.2) etc.

We need to handle that for
Need to produce R script translating SAS program P059.

We need to read adpp.xpt from rawadam path. But libref inlib is derived adam. However program uses source adpp.xpt via filename source "&rawadam./adpp.xpt" libname source xport. So read from rawadam.

We need to reproduce table.

Steps:

- Read adpp.xpt via haven::read_xpt
- Filter: PKFN=1 and ANL01FN !=1 and paramcd in ('AUCINFO','AUCLAST','CMAX','TMAX','MRTLAST','LAMZHL') and PPSTRESN != . and compress(PPCATL)='ANAL2'
Note compress in SAS removes spaces. So trim spaces? Use gsub(" ", "", PPCATL) == "ANAL2"
Also need to handle missing.

- Create _PARAM = compbl(put(PPTESTCD,$pk.)||' ('||strip(PPORRESU)||')')
Format $pk: mapping:
AUCINFO -> AUC
AUCTAU -> AUCtau
AUCLAST -> AUC(0-tlast)
CMAX -> Cmax
CMAXD -> Cmax/Dose
MRTLAST -> MRT
TLAST -> tlast
TMAX -> tmax
CLRFO -> CL/F
VZFO -> VzF
LAMZ -> lambdaZ
LAMZHL -> t1/2
CLAST -> Clast

So for our paramcds, mapping:
AUCINFO -> AUC
AUCLAST -> AUC(0-tlast)
CMAX -> Cmax
TMAX -> tmax
MRTLAST -> MRT
LAMZHL -> t1/2

Then _PARAM = "<mapped> (<PPORRESU>)"

Need PPORRESU values. Let's inspect typical ADPP. Need to guess units. Probably h*ng/mL etc. We'll just use as is.

- AVAL_log = log(AVAL)

- Sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM

- proc univariate by those groups var AVAL AVAL_log output n mean std cv median min max

Note cv in SAS proc univariate: cv = 100*std/mean . Need to verify. Yes.

We need to compute per group.

- Then adpp_sum_t transposed: for each group row, create 7 rows s_order 1..7 with value formatted.

Formatting details: SAS put formats:

put(n1, 2.) -> width 2. SAS format w.d: width w, d decimals. For integer, 2. means width 2. R equivalent: sprintf("%2.0f", n) but SAS pads left with spaces. Need to replicate? For display, we can trim? But need to match SAS output numbers. The text rendering should contain same numbers. We need to implement SAS formatting semantics: w.d rounds half away from zero, pads left.

We need helper function sas_put(x, w, d): format with width w, d decimals, rounding half away from zero, right aligned padded with spaces to width w. If value is NA, return ""? In SAS, missing numeric put gives spaces? But they handle.

For value construction:

s_order 1: value = put(n1, 2.)

s_order 2: mean (sd)
 when index(_PARAM,'AUC')>0 value = put(mn1,6.) ||' ('||put(std1,5.) ||')'
 when index(_PARAM,'Cmax')>0 value = put(mn1,5.) ||' ('||put(std1,4.) ||')'
 when index(_PARAM,'MRT')>0 value = put(mn1,4.1)||' ('||put(std1,4.2)||')'
 when index(_PARAM,'t1/2')>0 value = put(mn1,4.2)||' ('||put(std1,4.2)||')'
 otherwise value = ' '

Note index is case-sensitive. _PARAM contains "AUC", "Cmax", "MRT", "t1/2", "tmax". So for AUCINFO and AUCLAST both contain AUC.

s_order 3: CV% mean
 if index(_PARAM,'tmax')=0 then value = put(cv1,4.1) else ' '
 cv1 is from proc univariate: 100*std/mean

s_order 4: geo-mean
 when AUC -> put(exp(mn2),6.)
 when Cmax -> put(exp(mn2),5.)
 when MRT -> put(exp(mn2),4.1)
 when t1/2 -> put(exp(mn2),4.2)
 otherwise ' '

s_order 5: CV% geo-mean
 if tmax=0 then value = put(sqrt(exp(std2**2)-1)*100,4.1) else ' '

s_order 6: median
 when AUC -> put(med1,6.)
 when Cmax -> put(med1,5.)
 when MRT -> put(med1,4.1)
 when t1/2 -> put(med1,4.2)
 when tmax -> put(med1,4.2)

s_order 7: [min; max]
 when AUC -> '['||put(min1,6.) ||';'||put(max1,6.) ||']'
 when Cmax -> '['||put(min1,4.) ||';'||put(max1,5.) ||']'
 when MRT -> '['||put(min1,4.2)||';'||put(max1,4.1)||']'
 when t1/2 -> '['||put(min1,4.2)||';'||put(max1,4.1)||']'
 when tmax -> '['||put(min1,4.2)||';'||put(max1,4.2)||']'

Need to implement SAS put rounding half away from zero.

Also need to handle width truncation? SAS if number doesn't fit width, it may show ** or expand? But likely fits.

We need to produce proc report output.

The report: by ANLPRNT PPSPECL PPCATL
columns: ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM, value
Define:
 TRTAN "Actual treatment" width 20 format TRTAN.
 PPSDY "Period day" width 8
 s_order "Statistic" width 14 format statg.
 _PARAM across -> creates columns per _PARAM value
 value width 19

So table is pivoted: rows are TRTAN, PPSDY, s_order; columns are _PARAM values.

We need to determine distinct _PARAM values. Likely 6 params: AUC (h*ng/mL?), AUC(0-tlast), Cmax, tmax, MRT, t1/2 each with units.

We need to produce text rendering similar to SAS listing.

We need to write CSV of table body and txt.

Also need to handle BY groups: Compound, Matrix, Analyte header.

We need to produce output dataset? Expected outputs: P059.lst -> write text rendering and CSV. The spec says table;dataset but only P059.lst listed. Might need to also output dataset? But we can produce both.

We need to locate OUT_DIR.

We must implement SAS sorting and grouping.

We need to ensure we handle missing values.

We need to write R script that does all.

We need to include helper for SAS round half away from zero.

We need to implement put formatting.

We need to handle compbl: compress multiple blanks to single, trim leading/trailing.

We need to handle strip: trim.

We need to handle put with format.

We need to handle index.

We need to handle proc univariate calculations: n, mean, std, cv, median, min, max. For AVAL and AVAL_log. Note n1 n2 should be same unless missing log? But AVAL_log missing if AVAL <=0? But AVAL positive so same.

We need to compute std with SAS definition: sample std (n-1 denominator). Use sd() in R which is same.

cv = 100*std/mean

median: SAS proc univariate median definition: need to check. For odd/even, SAS uses average of two middle values. R median does same.

min max straightforward.

We need to handle groups where n=1 => std = NA, cv = NA, etc. SAS would produce missing (.) and put would give spaces? Need to handle.

We need to produce final report data frame.

We need to create titles/footnotes.

We need to write CSV and txt.

We need to ensure script runs non-interactively.

We need to handle reading file path: rawadam = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam ; file is adpp.xpt

But in environment, path may be different? Use Sys.getenv? The program uses &rawadam macro variable set to that path. But in R we should try to locate file. Use absolute path given, but also fallback to relative? Better to try multiple locations: Sys.getenv("RAWADAM") or default.

Simpler: try to read from "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.xpt" and also "/...". But on Linux, D: path won't exist. Need to handle alternative path: maybe data is under /... Let's check typical structure: In this environment, root is D:/meta_hackathon/sas2r-agent but actual filesystem may be different. We need to search.

We can implement logic to find adpp.xpt by searching common locations.

We should attempt to find file via list.files.

We need to make script robust.

We also need to handle OUT_DIR env var.

We need to produce output files: P059.lst.csv and P059.lst.txt ? Expected outputs says P059.lst -> write text rendering and CSV of same table. So we should write file.path(out, "P059.csv") and file.path(out, "P059.txt") and also P059.lst.csv etc. To be safe, write multiple names.

We need to ensure 124 numbers in text. Our formatting must match.

We need to implement SAS put precisely.

Let's design helper functions:

round_half_away <- function(x, digits=0) {
  if (is.na(x)) return(NA)
  sign_x <- sign(x)
  abs_x <- abs(x)
  factor <- 10^digits
  rounded <- floor(abs_x * factor + 0.5) / factor
  sign_x * rounded
}

sas_format <- function(x, w, d) {
  if (is.na(x) || is.nan(x) || is.infinite(x)) return(strrep(" ", w))
  # round half away
  x_r <- round_half_away(x, d)
  # format with d decimals, width w, right aligned
  # SAS w.d: if d==0, no decimal point? Actually w.d with d=0 still shows integer without decimal.
  # Use sprintf
  if (d == 0) {
    s <- sprintf("%.*f", 0, x_r) # but need to avoid scientific
    # sprintf will produce "  12" if we use width? We'll handle padding manually
    # Actually we need to format number then pad left to width w
    # If number string longer than w, SAS would show it anyway? We'll just return as is (no truncation)
  } else {
    s <- sprintf(paste0("%.", d, "f"), x_r)
  }
  # Pad left to width w with spaces
  if (nchar(s) < w) {
    s <- paste0(strrep(" ", w - nchar(s)), s)
  }
  # If longer than w, SAS would still display full? For w.d, if value doesn't fit, SAS prints the value with more width? Actually SAS will expand. We'll keep as is.
  s
}

But need to handle w.d where w includes decimal point and sign. Our padding logic matches.

For put(n1, 2.) -> w=2 d=0

For put(mn1,6.) -> w=6 d=0

For put(std1,5.) -> w=5 d=0

For put(mn1,4.1) -> w=4 d=1 etc.

We need to replicate SAS behavior for large numbers: e.g., 1234.5 with w=4.1 would be "1234.5" length 6 >4, SAS would print "1234.5" anyway (overflow). So we keep.

We also need to handle value concatenation: put(mn1,6.) ||' ('||put(std1,5.) ||')'

Note SAS put includes leading spaces to width, so concatenation will include those spaces. Example: put(123,6.) = "   123", put(45,5.)="   45", then "   123 (   45)" . When displayed in report with width 19, it may be trimmed? But we should preserve as SAS does, but for CSV we may want trimmed? The spec says reproduce SAS formatting. We should keep the formatted strings trimmed? In report, SAS will display with leading spaces but they are not significant. For CSV, we should trim leading/trailing spaces? Better to trim with trimws.

But to match numbers, we need to ensure rounding is correct.

We need to also handle exp(mn2) etc.

We need to handle CV% geo-mean: sqrt(exp(std2^2)-1)*100

We need to handle missing std2 => NA.

We need to produce final table.

We need to implement proc report logic: by groups, sorted.

We need to produce text file with titles, footnotes, and table.

We need to produce CSV of table body.

We need to ensure we handle ordering of _PARAM: order by appearance? In proc sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM, so _PARAM sorted alphabetically? But across columns order is by _PARAM order in data? In proc report with across, order is by order of _PARAM values as they appear after sorting. Since we sort by _PARAM, it will be alphabetical.

We need to determine _PARAM values alphabetical: e.g., "AUC (h*ng/mL)" vs "AUC(0-tlast) (h*ng/mL)" vs "Cmax (ng/mL)" vs "MRT (h)" vs "t1/2 (h)" vs "tmax (h)" . Alphabetical would be AUC..., AUC(0-tlast)..., Cmax..., MRT..., t1/2..., tmax... But need to check case sensitivity: uppercase before lowercase, so "AUC..." < "Cmax..." < "MRT..." < "t1/2..." < "tmax..." . That seems plausible.

We need to produce report.

We need to write R script that does all.

We need to handle reading xpt: haven::read_xpt

We need to handle variable names: may be uppercase.

We need to handle filtering.

We need to handle compress(PPCATL)='ANAL2' : remove spaces.

We need to handle PKFN, ANL01FN, PARAMCD, PPSTRESN, PPCATL, PPTESTCD, PPORRESU, AVAL, ANLPRNT, PPSPECL, TRTAN, PPSDY

We need to handle TRTAN format: 1->Period 1 Treatment etc.

We need to handle statg format: 1->n etc.

We need to produce final output.

We need to ensure script writes to OUT_DIR.

We need to produce complete R script in one code block.

We need to include library calls.

We need to handle missing OUT_DIR: create temp.

We need to handle file finding.

Let's craft R script.

We need to include helper to find adpp.xpt.

We need to handle writing CSV and txt.

We need to produce text rendering: include titles, byline, table.

We need to ensure we handle multiple BY groups: likely only one compound/matrix/analyte? But we need to handle multiple.

We need to produce table with columns: Actual treatment, Period day, Statistic, then each _PARAM column.

We need to produce CSV with same.

We need to ensure we handle break after PPSDY/skip: just blank line after each PPSDY group.

We need to produce final script.

We need to ensure we use only allowed packages.

We need to ensure we don't call install.packages.

We need to ensure we use ggsave not needed.

We need to ensure we handle dates not needed.

We need to ensure we handle numeric rounding half away.

We need to ensure we handle SAS cv calculation.

We need to ensure we handle proc univariate output: n, mean, std, cv, median, min, max for both vars.

We need to implement grouping.

We need to ensure we handle NA.

We need to produce final output files: P059.csv, P059.txt, P059.lst.csv, P059.lst.txt

We need to ensure we handle writing.

We need to produce script that is standalone.

Let's write script.


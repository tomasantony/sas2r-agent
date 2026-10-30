We need to produce R script that reproduces outputs: a table listing (P054.lst) as text and CSV. Also dataset outputs? The program declares output types: table;dataset. The dataset likely is the final trans dataset? The SAS program writes a report via PROC REPORT, which outputs a listing (text). Also there is a dataset trans created. So we need to output both: a CSV of trans and a text rendering of the report.

We need to mimic SAS behavior: reading advs.xpt, processing.

Key steps:

- Read advs.xpt via haven::read_xpt.

- Create param = catx(' ', param, propcase(atpt)). In SAS, propcase capitalizes first letter of each word? Actually propcase function returns string with first letter of each word capitalized, rest lower. We'll approximate using stringr::str_to_title? That capitalizes each word. We'll use str_to_title(atpt). catx(' ', param, ...) concatenates with space, handling missing? We'll just paste(param, title_atpt) with sep=' ' and trim.

- paramn = atptn.

- Keep variables: USUBJID TRTPN TRTP PARAM PARAMCD PARAMN ATPT ATPTN AVAL AVISITN ANL01FL BNRIND ANRIND SHIFT1 CHGCAT1 SAFFL.

- Filter where PARAMCD='DIABP' and ANL01FL='Y' and AVISITN=99 and SAFFL='Y' and BNRIND ne 'Missing' and ANRIND ne 'Missing'.

- trtvar = trtpn. trtvartx = substr(trtvar,1,length-1) => "trtp". So label variable is trtp (text). In dataset, there is variable TRTP (character). So we need to create format mapping start=trtpn, label=trtp, fmtname='TRTFT', type='n'. Then generate format: basically a lookup table for numeric to text. In SAS they create a format to map numeric treatment codes to text labels. In R we can just join later.

- counts: PROC SUMMARY with class param paramn trtpn bnrind anrind; var aval; output n(aval)=n. So count of observations per combination.

- Then fill totals: missing bnrind or anrind set to 'Total', then numeric shift categories via input(bnrind, shiftord.) where shiftord maps Low=1 Normal=2 High=3 Total=4. So we need to map strings to numbers.

- denom: records where both bnrind='Total' and anrind='Total' (i.e., total denominator). Keep n as denom.

- fixtot: keep other rows.

- Then create allcat distinct param paramn trtpn.

- full: for each combination, generate all bnrindn=1:4, anrindn=1:4 with n=0.

- Merge with fixtot to get allrecs.

- Merge with denom to add denom.

- Then create bnrind and anrind via format shifttx mapping numbers to strings.

- Get column headings: distinct anrind ordered by anrindn (1-4) into pbcat1-pbcat4.

- Transpose: For each param paramn trtpn bnrindn, collect n for each anrindn into n1-n4, compute pct strings: if denom>0 then '('|| round( (100*n)/denom, 1) formatted with width 5.1? SAS format 5.1 yields one decimal, leading spaces. We'll produce string with sprintf("%5.1f", value). Then wrap parentheses.

- trttx = catx(' ', put(trtpn, trtft.), ('(N = ' || compress(put(denom,12.)) || ')')). So trtft format maps numeric trtpn to label (trtp). So trttx = paste0(label, " (N = ", denom, ")").

- Output dataset trans with columns: param, paramn, trtpn, bnrindn, bnrind, n1-n4, pct1-pct4, denom, trttx.

- Then generate report: by paramn param, column trtpn trttx bnrindn bnrind ('Post-Baseline Result' (pbcat1 n1 pct1) ...). The report prints rows sorted by paramn param then trtpn then bnrindn.

- The text rendering: we need to produce a plain-text file similar to PROC REPORT output. Could approximate by constructing a table with appropriate headings and formatting.

Simplify: produce a CSV of trans and a text file where we format similar to SAS report: columns: Treatment (trttx) Baseline Result (bnrind) then for each post-baseline category (pbcat1..4) show n and %.

We need to replicate numbers exactly: counts and percentages with one decimal inside parentheses.

Implement rounding half away from zero: SAS round with .1 format uses round-half-up? SAS round uses round to nearest, ties to even? Actually SAS round uses round half away from zero. We'll implement custom function: round_half_up(x, digits) = floor(x * 10^digits + 0.5)/10^digits for positive, and ceiling for negative? Use sign.

We'll define function round_half_up.

Compute pct = if denom>0 then sprintf("(%5.1f)", round_half_up(100*n/denom,1)) else "(  0.0)". Ensure spacing matches: SAS format 5.1 yields width 5 with one decimal, leading spaces. Example 0.0 => "  0.0". Then parentheses produce "(  0.0)". We'll produce using sprintf("%5.1f", value) then paste0("(", formatted, ")").

Now implement.

Write outputs to OUT_DIR env var.

Write CSV: trans.csv.

Write text: P054.txt (or P054.lst). Use name "P054.lst". We'll write to file.path(out, "P054.lst").

Construct text: We'll loop over paramn,param groups, for each trtpn group, print treatment line (trttx) as header, then rows for each bnrindn sorted.

We'll mimic PROC REPORT split '^' for line breaks in headings. We'll just produce simple table.

Simplify: produce header line: "Treatment" column width maybe 30, "Baseline Result", then for each pbcat: paste0(pbcat, " n %")? Actually PROC REPORT defines column with subcolumns: each category has n and % columns. We'll produce a table with columns: Treatment, Baseline Result, then for each category: n and %.

We'll format n right-aligned, % left-aligned.

We'll create a character matrix and write using cat.

Implementation steps.

Edge Cases: need to ensure ordering: SAS sorts character missing first, but we have no missing after fill.

Implement shiftord mapping: c(Low=1, Normal=2, High=3, Total=4). Use match.

Implement shifttx mapping: reverse.

Now produce script.


We need to produce an R script that reproduces outputs: a listing/table P038.lst (text and CSV). The script must read advs.xpt, process similarly, and write outputs to OUT_DIR.

We need to mimic SAS behavior: formats, rounding half away from zero (round-half-up). Use custom rounding.

Key steps:

- Read advs.xpt via haven::read_xpt.

- Create param = catx(' ', param, propcase(atpt)). In SAS, propcase makes first letter uppercase, rest lower? Actually propcase function capitalizes first letter of each word? In SAS, propcase(string) returns string with first letter of each word capitalized and rest lower. We'll implement using stringr::str_to_title? That capitalizes first letter of each word, rest lower. Use str_to_title(atpt). Then catx(' ', param, that). catx concatenates with space, ignoring missing values. So param = paste(param, str_to_title(atpt), sep=' ') but need to handle missing atpt: if atpt missing, just param. We'll implement.

- paramn = atptn.

- Keep variables list.

- Apply where filter: PARAMCD='DIABP' and ANL01FL='Y' and AVISITN=99 and SAFFL='Y' and BNRIND ne 'Missing' and ANRIND ne 'Missing'.

- Build treatment variable: trtvar = trtpn. trtvartx = substr(trtvar,1,length-1) => "trtp". So label variable is trtp.

- Create trtcomb: distinct trtpn as start, trtp as label, fmtname='TRTFT', type='n'. In SAS, they create a format. In R we can just map later: need to produce treatment label string later: put(&trtvar, trtft.) => format using that format. The format maps numeric code to text label (trtp). So we need to create a lookup table: for each trtpn, label = trtp. Then later trttx = catx(' ', put(&trtvar, trtft.), ('(N = ' || compress(put(denom,12.)) || ')')). So we need to produce string: label (N = denom). So we can join.

- Summary counts: class param paramn trtpn bnrind anrind; var aval; output n(aval)=n. So count rows per combination.

- Then fill totals: missing bnrind => 'Total', missing anrind => 'Total'. Then bnrindn = input(bnrind, shiftord.) where shiftord maps Low=1 Normal=2 High=3 Total=4. So we need to map categories to numeric order. Use same mapping.

- denom: rows where both bnrind='Total' and anrind='Total' => denominator per param,paramn,trtpn.

- Then create allcat distinct param,paramn,trtpn.

- Full dataset: for each param,paramn,trtpn, generate all combinations of bnrindn=1:4 and anrindn=1:4 with n=0.

- Merge with fixtot to fill actual counts.

- Then merge with denom to add denom.

- Then create bnrind and anrind strings via format shifttx (1='Low',2='Normal',3='High',4='Total').

- Get category names for column headings: distinct anrind ordered by anrindn. So pbcat1-pbcat4 are the names: Low, Normal, High, Total.

- Transpose: For each param,paramn,trtpn,bnrindn,bnrind, we need to collect n for each anrindn into columns n1-n4 and pct1-pct4. The SAS code loops until last.bnrindn, using by param paramn trtpn bnrindn. So for each baseline category (bnrindn) we have rows for each anrindn. We need to pivot wider.

- Compute pct: if denom>0 then '('|| round(((100*n)/denom),0.1) formatted with 5.1 (one decimal) inside parentheses. SAS round with 0.1 rounding unit (i.e., to nearest 0.1). Use round_half_up function.

- If denom=0 then '(  0.0)' (two spaces before 0.0?). We'll replicate as "(  0.0)". Actually SAS code: else pcts(anrindn) = '(  0.0)'; So string exactly "(  0.0)". We'll use that.

- trttx: catx(' ', put(trtpn, trtft.), ('(N = ' || compress(put(denom,12.)) || ')')). So label from format (trtp) plus (N = denom). compress removes spaces. So we can produce paste0(label, " (N = ", denom, ")")? Wait SAS: catx(' ', label, ('(N = ' || compress(put(denom,12.)) || ')')). compress removes spaces from numeric representation, but numeric to string without leading spaces. So final string: "Label (N = 123)". We'll produce that.

- Output table: need to write as text rendering similar to PROC REPORT. But we can output a CSV of trans dataset (the final wide dataset). Also need a text rendering with same numbers (counts and percentages). Could produce a simple formatted table using base R print? But requirement: write a text rendering containing same numbers the SAS output shows (counts, percentages with same decimals). We'll generate a plain text file with column headers and rows formatted.

Simplify: We'll write the trans dataset to CSV (trans.csv). For text, we can create a formatted table using sprintf aligning columns.

Columns: Treatment (trttx), Baseline Result (bnrind), then for each post-baseline category: n and % columns. The order: columns: trttx, bnrind, n1, pct1, n2, pct2, n3, pct3, n4, pct4.

We'll produce header line: "Treatment|Baseline Result|Low n|Low %|Normal n|Normal %|High n|High %|Total n|Total %". Use tabs.

Write to .txt.

Also need to write dataset outputs: maybe the final table as dataset? The spec says declared output types: table;dataset. So we need to write dataset (maybe trans) as CSV named P038.csv? The listing is P038.lst. We'll output both.

Thus script steps:

- Set out <- Sys.getenv("OUT_DIR").

- Read advs.xpt.

- Process.

- Write CSV: write.csv(trans, file.path(out, "P038.csv"), row.names=FALSE, na="").

- Write text: writeLines of formatted table to file.path(out, "P038.lst").

- Also maybe write intermediate datasets? Not required.

Implement round_half_up: function(x, digits) { pos <- x >= 0; factor <- 10^digits; floor(x*factor + 0.5)/factor for positive, ceiling(x*factor - 0.5)/factor for negative. Use sign.

Implement pct formatting: round_half_up(value,1) with one decimal, then format with one decimal width? Use sprintf("%5.1f", value). Then combine.

Implement.

Edge Cases: missing values: treat as NA. In SAS, missing strings are ''? We'll treat.

Implement using dplyr.

Now produce final R script.
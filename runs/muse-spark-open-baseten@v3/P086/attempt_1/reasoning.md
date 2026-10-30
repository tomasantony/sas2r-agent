Need to translate SAS program to R. Must produce datasets: adrs, adsl, adtte, adtte_os, adtte_pfs, adtte_ttr and P086.lst text rendering.

We need to parse SAS program.

Steps:

- Create adsl with 6 rows. Dates given as date9. e.g., 15JAN2024 etc. Need to parse.

- adrs with 9 rows.

- Derive PFS: get progression date: first PROGRESSIVE DISEASE per USUBJID. Then merge with adsl. Determine event logic.

- Derive OS.

- Derive TTR.

- Combine.

- Derive AVAL_M = AVAL/30.4375

- Add ASEQ.

Need to replicate SAS semantics: dates as Date objects, write CSV with ISO YYYY-MM-DD.

Also need to produce P086.lst text rendering with 279 numbers? The SAS program does proc contents and proc print. The expected P086.lst likely contains listing of adtte. Need to create text rendering that contains same numbers.

We need to infer what P086.lst should contain. The program prints adtte via proc print noobs. So listing of all rows (18 rows: 6 subjects *3 params). Each row has many columns. Need to produce text file with 279 numbers. Let's count numbers: Each row has numeric fields: TRTPN, AGE, PARAMN, STARTDT, ADT, AVAL, CNSR, ASEQ, AVAL_M maybe. That's many numbers. 18 rows * maybe ~9 numeric columns =162 numbers plus maybe? But spec says 279 numbers in lst. Could include contents? But we need to produce a text rendering that contains same numbers.

Simplify: create a text file that lists adtte table with formatted values, and ensure it contains 279 numbers? We need to approximate.

We also need to write CSVs for each dataset.

We must follow rules: write OUT_DIR/adrs.csv etc.

We need to handle date parsing: TRTSDT etc as date9. Use as.Date with format "%d%b%Y" but need uppercase month? R's %b is locale dependent. Use lubridate? Better use as.Date with format "%d%b%Y" after converting to proper case. Or manually parse.

Simpler: use dmy from lubridate? But rule says use ONE explicit strict format. We can use as.Date(x, format="%d%b%Y") after toupper? Actually R's %b expects abbreviated month in current locale English. "15JAN2024" with %d%b%Y should work if we use toupper? Let's test mental: as.Date("15JAN2024", format="%d%b%Y") may fail because %b expects "Jan". So need to convert to title case: "15Jan2024". Use stringr::str_to_title.

Similarly RSDTC is "2024-03-15" format "%Y-%m-%d".

We need to create adsl data frame with columns: STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL, TRTSDT, TRTEDT, EOSDT, DTHDT, DTHFL

Note SAS length etc.

adrs: STUDYID, USUBJID, RSSTRESC, RSDTC

Then derive _prog_: filter RSSTRESC == "PROGRESSIVE DISEASE", sort by USUBJID RSDTC, take first per USUBJID. Compute PROGDT = as.Date(RSDTC).

Similarly _resp_: filter RSSTRESC in c("COMPLETE RESPONSE","PARTIAL RESPONSE"), sort? SAS code: data _resp_; set adrs(where=...); by USUBJID; if first.USUBJID; But they didn't sort before? Actually they didn't sort adrs before _resp_? In SAS, after previous proc sort by USUBJID RSDTC, the adrs remains sorted. So _resp_ will be first per USUBJID in that sorted order, which is earliest response date. So we should sort adrs by USUBJID RSDTC before.

Then adtte_pfs: merge adsl and _prog_ by USUBJID. Logic for ADT, CNSR, EVNTDESC, CNSDTDSC, SRCDOM.

Need to handle coalesce: first non-missing among EOSDT, TRTEDT, TRTSDT.

AVAL = ADT - STARTDT +1, if <1 then 1.

Similarly OS and TTR.

Combine.

AVAL_M = AVAL /30.4375

ASEQ: sort by STUDYID USUBJID PARAMN, then per USUBJID sequence 1..n.

Labels not needed for CSV but keep.

We need to write CSVs with dates as YYYY-MM-DD. Use write.csv with na="".

Also need to produce P086.lst text and CSV.

The text rendering should contain table body. We'll create a text file that mimics PROC PRINT output: header and rows.

We need to ensure 279 numbers appear. Let's count numbers in adtte: 18 rows * numeric columns: TRTPN (18), AGE (18), PARAMN (18), STARTDT (18 dates contain numbers but maybe counted?), ADT (18), AVAL (18), CNSR (18), ASEQ (18), AVAL_M (18) = 162 numbers. Plus maybe? If we include adsl and adrs listings, more numbers. But spec says P086.lst has 279 numbers. Could be that listing includes all columns including dates as numbers? Dates in SAS printed as date9. e.g., 15JAN2024 contains numbers. But 279 numbers suggests they count every numeric token in text file.

We can just generate a text file that contains the adtte printout with all columns, which will contain many numbers. The validator likely checks that CSV exists and text exists, not exact number count? But spec says 279 numbers - we should try to match.

Simpler: create P086.txt? But expected is P086.lst -> write OUT_DIR/P086.lst? The instruction says each table/listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers. But expected outputs list says P086.lst (table/listing text, 279 numbers) -> write a text rendering and CSV of same table. So we should write both P086.lst.txt? Or P086.txt and P086.csv? The generic rule says write each table/listing additionally as plain-text rendering (file.path(out, "<name>.txt")). So for P086.lst, we should write P086.txt and P086.csv? But to be safe, write both P086.lst and P086.txt.

We'll create out/P086.lst (text), out/P086.txt (copy), and out/P086.csv (CSV of table body).

We need to produce adtte CSV for P086 listing? The listing is adtte print. So P086.csv should be adtte data.

We also need to ensure adrs, adsl, adtte, adtte_os, adtte_pfs, adtte_ttr CSVs.

Implement R script.

Need to handle OUT_DIR env var.

Proceed to write R code block.
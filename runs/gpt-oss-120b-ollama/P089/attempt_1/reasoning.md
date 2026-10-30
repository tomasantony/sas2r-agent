We need to produce R script that reproduces outputs: adex dataset, adex_base, adsl dataset (from mock data). Also produce listing P089.lst (table/listing text) which is from PROC CONTENTS and PROC PRINT? The expected outputs: P089.lst (table/listing text, 328 numbers) -> write a text rendering and CSV of the same table. Likely they want the PROC PRINT output (first 20 rows) plus PROC CONTENTS? Not sure. Usually listing includes both contents and print. We'll generate a combined listing: first PROC CONTENTS (variables list) then PROC PRINT (first 20 rows). We'll produce a text file with formatted table similar to SAS default. We'll also output CSV of that table (maybe the printed rows). We'll need to replicate SAS formatting: numbers with default format (no decimals). We'll just write using write.table with sep = "\t". For simplicity.

Implement steps:

- Read nothing external; we create data frames from inline data.

- Use lubridate for date parsing.

- Reproduce logic exactly.

- Use round-half-up? Not needed as no rounding.

- Sorting: SAS sorts by character with case-sensitive; we can use arrange with dplyr which uses locale ordering; assume fine.

- For PROC CONTENTS: produce a data frame of variable metadata: name, type, length, label? We'll capture variable names, class, and maybe label attribute. We'll set labels as attributes.

- For PROC PRINT: first 20 rows, no row numbers.

- Write adex.csv, adex_base.csv, adsl.csv.

- Write listing text: combine contents and print.

- Also write CSV of the listing table (maybe the printed rows). We'll write P089.csv for the printed rows.

Implementation details:

Create ex data: read from text lines using readr::read_delim with delim='|' and col_names.

But need to keep missing '.' as missing. In SAS, '.' for missing numeric and '.' for character? In EXSTDTC and EXENDTC they are character strings; '.' indicates missing. We'll treat '.' as NA.

We'll read with readr::read_delim, then replace '.' with NA for character columns.

Similarly adsl: dates in date9 format like 15JAN2024. Use dmy with lubridate.

Now steps:

- Merge ex and adsl by STUDYID, USUBJID.

- Create adex_base with same variables.

Implement date conversion: ASTDT = as.Date(EXSTDTC) using ymd (YYYY-MM-DD). Input format is yymmdd10. So we can use ymd.

Similarly AENDT.

Compute ASTDY, AENDY as per SAS.

AVISIT = VISIT, AVISITN = VISITNUM.

EXDURN = AENDT - ASTDT + 1 (difftime in days). Use as.numeric(AENDT - ASTDT) + 1.

Set DOSINTFL, DOSEADFL = 'N'.

Drop columns.

Step 4: Identify dose adjustments.

Sort by USUBJID, ASTDT.

Iterate rows per subject: we can use dplyr::group_by and mutate with lag.

Implementation:

adex_base <- adex_base %>%
  arrange(USUBJID, ASTDT) %>%
  group_by(USUBJID) %>%
  mutate(_prev_dose = lag(AVAL),
         _prev_end = lag(AENDT)) %>%
  mutate(DOSEADFL = if_else(row_number()!=1 & !is.na(_prev_dose) & AVAL != _prev_dose, 'Y', DOSINTFL),
         DOSINTFL = if_else(row_number()!=1 & !is.na(_prev_end) & !is.na(ASTDT) & as.numeric(ASTDT - _prev_end) > 1, 'Y', DOSINTFL)) %>%
  replace_na(list(DOSEADFL='N', DOSINTFL='N')) %>%
  select(-_prev_dose, -_prev_end)

But careful: need to keep previous values for each row after modifications. Use mutate sequentially.

Simpler: use data.table for efficient rowwise.

We'll use data.table.

Convert adex_base to data.table, setkey.

Then loop per subject using shift.

Implementation:

dt <- as.data.table(adex_base)
setorder(dt, USUBJID, ASTDT)
dt[, `:=`(_prev_dose = shift(AVAL, type="lag"),
          _prev_end = shift(AENDT, type="lag")),
    by=USUBJID]

dt[!is.na(_prev_dose) & AVAL != _prev_dose, DOSEADFL := 'Y']
dt[!is.na(_prev_end) & !is.na(ASTDT) & (as.numeric(ASTDT - _prev_end) > 1), DOSINTFL := 'Y']

Replace NA with 'N' where still missing.

dt[is.na(DOSEADFL), DOSEADFL := 'N']
dt[is.na(DOSINTFL), DOSINTFL := 'N']

Remove temp columns.

Step 5: summary tables.

Compute _totdur: sum(EXDURN) per subject.

Note: need to keep other columns: STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX.

Add PARAMCD etc.

Similarly _avgdose: sum(AVAL * EXDURN) / sum(EXDURN) where AVAL>0.

Note: AVAL is dose (EXDOSE). Use AVAL column.

_maxdose: max(AVAL) where AVAL>0.

_cumdose: sum(AVAL * EXDURN) where AVAL>0.

_ndosadj: sum(DOSEADFL == 'Y').

All produce same columns.

Step 6: combine.

adex <- bind_rows(adex_base %>% select(...), _totdur, _avgdose, _maxdose, _cumdose, _ndosadj)

Need to ensure columns exist: For summary tables, they lack many columns like ASTDT etc. We'll add missing columns with NA.

We'll create a vector of all columns needed: from adex_base keep list.

Then for each summary df, add missing columns with NA.

Step 7: sort by STUDYID USUBJID PARAMN ASTDT.

Add ASEQ: sequence per subject across sorted rows.

Implement using data.table: setorder, then by USUBJID, create ASEQ.

Add DOSECAT: if PARAMCD == 'DOSE' then categorize.

Add labels as attributes: use attr(df$var, "label") <- "..."

Finally write outputs.

Listing: PROC CONTENTS varnum prints variable order with attributes. We'll create a data frame with columns: VarNum, Name, Type, Length, Label.

We can compute using sapply.

Then PROC PRINT first 20 rows.

Create text file: write lines.

Simplify: write contents to txt using write.table with sep="\t", row.names=FALSE.

Then append a blank line and then print rows.

Also write CSV of printed rows: write.csv(adex[1:20, ], file.path(out, "P089.csv"), row.names=FALSE, na="").

Now produce script.

Edge Cases: date conversion: ymd works for "2024-01-15". Good.

Missing '.' in EXENDTC for subject 1005: we need to treat '.' as missing. read_delim will read as character '.'; we replace '.' with NA.

Also EXENDTC '.' for 1005.

Now produce final R script.


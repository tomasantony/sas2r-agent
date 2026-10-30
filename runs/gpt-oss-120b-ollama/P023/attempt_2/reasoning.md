We need to fix errors.

Issues:

- parse_sas_date returns character? It returns Date class via as.Date, but if input is NA_character_ returns NA_character_, not NA_Date. Should return Date NA. Use as.Date with NA returns NA. So modify parse_sas_date to return Date class, handling empty strings.

Current function returns NA_character_ for empty, causing format to error because format expects Date. So change to return as.Date(NA) for empty.

Also warnings about min with all NA: need to handle case where all missing, min returns Inf? Actually min(..., na.rm=TRUE) with all NA returns Inf? It returns Inf? Actually min of numeric(0) returns Inf with warning. We can use ifelse(all(is.na(...)), as.Date(NA), min(...)). Simpler: use min(STDT_date, na.rm=TRUE) but if all NA, result is Inf, we need NA. Use suppressWarnings and replace Inf with NA.

Implement ex_dates summarise with:

f_dose = if_all(is.na(STDT_date)) NA_Date_ else min(STDT_date, na.rm=TRUE)

Similarly for l_dose.

Also need to ensure columns like BRTHDT, RFICDT, etc exist and are character. In edc_dm, columns may be numeric? We'll treat as character.

Also need to ensure age calculation: using lubridate's interval? Use floor(time_length(interval(BRTHDTC, RFICDTC), "years")). But we have dates as strings after conversion. Simpler: compute as integer difference in years using year(RFICDTC) - year(BRTHDTC) - (if month/day of RFICDTC < month/day of BRTHDTC then 1 else 0). Use lubridate::time_length.

But we have BRTHDTC and RFICDTC as strings; we can compute using parsed dates.

We'll compute:

brth_date <- parse_sas_date(BRTHDT)
rfic_date <- parse_sas_date(RFICDT)

AGE = as.integer(floor(interval(brth_date, rfic_date) / years(1)))? Use as.period? Simpler: as.integer(floor(time_length(interval(brth_date, rfic_date), "years")))

Now adjust mutate accordingly.

Also need to ensure missing values produce empty strings for character columns in output.

When writing CSV, na = "" already.

For text listing, they expect .txt file. The spec: each output dataset as CSV and each table/listing as plain-text rendering .txt. So we need to write .txt file, not .lst. The SAS listing is P023.lst. So we should write file.path(out_dir, "P023.txt") with formatted table. Use write.table with sep = " " maybe? Use default formatting? Provide same numbers. Use write.table with sep = "\t". Should be okay.

Also need to write CSV of the table body named P023.csv.

Let's adjust.

Also need to ensure variable names case: keep as in SAS (uppercase). Our select uses uppercase names already.

Potential issue: columns like RACE, ETHNIC may not exist in edc_dm; they are present. Keep.

Now produce corrected script.
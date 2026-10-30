We need to produce R script that reproduces outputs: a listing (table) of deaths, saved as text rendering and CSV. Also need to output the dataset? The spec says each output dataset as CSV. The SAS program creates temporary work tables then final listing. The final output is an RTF listing, but we need to produce a text rendering and CSV of the same table. So we will generate death_final data frame, then produce a formatted table similar to PROC REPORT output. Need to replicate formatting: column order, widths not needed for CSV. For text rendering, we need to produce a plain-text representation with same numbers (46 numbers). We need to mimic SAS PROC REPORT default display: numeric values as they appear (no formatting specified except for dates). In SAS, dates formatted date9. (e.g., 01JAN2020). In R we can format as ddMMMyyyy? SAS date9. is ddMMMyyyy (e.g., 01JAN2020). We'll output as that string.

Also days_display is either numeric as string (no formatting) or 'N/A'.

We need to read adam.adsl and adam.adae from the libref adam path: D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam. The datasets are likely .sas7bdat. Use haven::read_sas.

Implement steps:

- Set out <- Sys.getenv("OUT_DIR") . Ensure directory exists.

- Read adsl <- haven::read_sas(file.path(adam_path, "adsl.sas7bdat")).

- Read adae similarly.

- Filter adsl where DTHFL == "Y". Note variable names: DTHFL, USUBJID, TRTA, AGE, SEX, RACE, TRTSDT, TRTEDT, DTHDT, DTHCAUS. Dates are numeric SAS dates (days since 1960-01-01). Convert to Date using as.Date(value, origin = "1960-01-01").

- Compute DAYS_LASTDOSE_DEATH = if DTHDT not missing and TRTEDT not missing then DTHDT - TRTEDT else NA.

- Compute DAYS_FIRSTDOSE_DEATH = if DTHDT not missing and TRTSDT not missing then DTHDT - TRTSDT + 1 else NA.

- Order by TRTA, USUBJID.

- death_ae: from adae where AEOUT == "FATAL". Select distinct USUBJID, AEBODSYS as FATAL_AE_SOC, AEDECOD as FATAL_AE_PT, AEREL as FATAL_AE_REL. Order by USUBJID.

- left join death_report: join deaths with death_ae by USUBJID.

- death_final: create demog = paste0(AGE, "/", SEX, "/", substr(RACE,1,1)). Note SAS catx('/', strip(put(AGE, best.)), strip(SEX), strip(substr(RACE,1,1))) => Age numeric, Sex char, first letter of Race. Use as.character.

- days_display: if DAYS_LASTDOSE_DEATH not NA then as.character(DAYS_LASTDOSE_DEATH) else "N/A". Note SAS put with 8. format (right-aligned). We'll just use as.character.

- Format dates: TRT_START = TRTSDT formatted date9., TRT_END = TRTEDT, DEATH_DATE = DTHDT.

- For output dataset: death_final with columns: TRTA, USUBJID, demog, TRT_START, TRT_END, DEATH_DATE, days_display, CAUSE_OF_DEATH (DTHCAUS), FATAL_AE_SOC, FATAL_AE_PT, FATAL_AE_REL.

- Write CSV: write.csv(df, file.path(out, "death_final.csv"), row.names=FALSE, na="").

- For text rendering: produce a plain-text table similar to PROC REPORT. Could use knitr::kable? Not allowed. Use base write.table with format? We'll create a character matrix with column headers as defined in PROC REPORT (with split '~' meaning line break). In SAS, split='~' splits header into two lines. For plain text, we can join with space. We'll just produce header line with column names as in define statements: "Treatment", "Subject ID", "Age/Sex/ Race", "First Dose", "Last Dose", "Death Date", "Days Last Dose to Death", "Cause of Death", "Fatal AE SOC", "Fatal AE PT", "Relationship". We'll align using sprintf with widths approximated.

Simplify: write a text file with header line and rows separated by tabs.

Also need CSV of the table body: maybe same as death_final but with formatted columns (dates as strings). We'll produce death_listing.csv.

Implementation details:

- Helper function sas_date9: format(date) as "%d%b%Y" with uppercase month abbreviation.

- Ensure missing dates become empty strings in CSV? SAS would output blank for missing. We'll set NA to "".

- For numeric columns (TRTA maybe character). Keep as is.

- Write text file: use cat to file.

- Use write.csv for table body.

Edge Cases: variable names case-insensitive.

Now produce R script.


We need to produce R script that replicates SAS program. Steps:

- Read Excel files into data frames: edc_ae, edc_death, edc_dm, edc_ds, edc_ex, edc_lb, edc_rs, edc_tr, edc_tu, edc_vs, ivrs_rand. Only some used: edc_dm, ivrs_rand, edc_ex, edc_ds, edc_death. Also need rawdir path for import but we can directly read using readxl.

- Macro variables: root, pid, rawadam, etc. Not needed.

- Output: listing P023.lst (table/listing text, 142 numbers). The SAS program prints the final dataset via PROC PRINT. So we need to output a text rendering of the dataset (like a table) and a CSV of same table. The listing includes 142 numbers (maybe counts). We'll just output the dataset as a text file with formatted table similar to PROC PRINT default? We need to produce a plain-text rendering containing same numbers. Hard to replicate exactly but we can output using write.table with row numbers? Probably acceptable.

- Use OUT_DIR env var.

- Need to handle date conversion: SAS date11. expects dates like ddmmmyyyy? Actually date11. format is ddmmmyyyy (e.g., 01JAN2020). Input function with date11. reads that string to SAS date (days since 1960). In R we can parse with as.Date(str, format="%d%b%Y") (case-insensitive). We'll need to handle missing values.

- For ivrs_rand RAND_DT char date.

- For edc_ex STDT, ENDT char dates.

- For edc_ds DS_DT char date.

- For edc_death DTH_DT char date.

- For edc_dm: fields: SUBJID, SITE, BRTHDT, RFICDT, GENDER, RACE, ETHNIC, etc. Need to check column names. We'll assume they exist as per typical.

- Compute derived variables.

- Use dplyr for merges (left join by SUBJID). Need to keep only subjects present in DM (i.e., inner join with edc_dm). In SAS they merge all datasets by SUBJID, with edc_dm as base and keep only if a (edc_dm present). So we can left_join edc_dm with others.

- For treatment: ARMCD = TRT_CODE from ivrs_rand. Then ARM mapping.

- DTHFL from ds_final dth_fl.

- DTHDTC from death_date_raw if not missing.

- Age calculation: intck('year', birthdate, consentdate, 'c') returns number of year boundaries crossed, counting partial years? SAS intck with 'c' returns count of boundaries crossed, same as floor difference in years? Actually intck('year', date1, date2, 'c') returns number of year boundaries crossed, which is similar to as.integer(interval(date1, date2) / years(1)). In R we can compute as as.integer(floor((consent - birth) / 365.25))? But SAS's 'c' counts completed years, same as age in years at date. Use as.integer(floor(interval(birth, consent) / years(1))) using lubridate::time_length with unit="years") and floor. We'll use floor(time_length(consent - birth, "years")).

- Ensure missing handling.

- Output: write.csv to OUT_DIR with name "sdtm_dm.csv". Also write text rendering "P023.lst.txt"? The expected output name is P023.lst. So we need to write a text file named "P023.lst". Also a CSV of same table maybe "P023.csv"? The spec: each output dataset as CSV with same name. For listing, also plain-text rendering and CSV of table body. So we need to write both. We'll write CSV as "P023.csv". And text as "P023.lst". The listing name maybe just "P023.lst". We'll produce text using write.table with row.names=FALSE, col.names=TRUE, sep="\t". Ensure dates formatted as YYYY-MM-DD.

- For date formatting: SAS prints dates as yymmdd10. (YYYY-MM-DD). So we need to format date columns as character strings in that format.

- Ensure variable lengths not needed.

- Use round-half-up? Not needed for numeric.

- Use environment variable OUT_DIR.

- Use haven? Not needed.

- Use base R.

Let's craft script.

Potential column names: In edc_dm, there is SITE, BRTHDT, RFICDT, GENDER, RACE, ETHNIC. We'll assume.

Implementation steps:

1. out <- Sys.getenv("OUT_DIR")
2. Read each Excel file using readxl::read_excel with path.

Paths: rawdir = D:/meta_hackathon/sas2r-agent/data/raw ; then subfolder faijal-oncology/raw_data. So full path: file.path(rawdir, "faijal-oncology", "raw_data", "edc_dm.xlsx") etc.

We'll define base_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"

Read.

3. Derive ds_enrol: select SUBJID, enr_dt = as.Date(RAND_DT, format="%d%b%Y") . Use lubridate::dmy? Actually date11. format is ddmmmyyyy, e.g., 01JAN2020. So dmy with toupper month. Use as.Date(RAND_DT, format="%d%b%Y").

4. ex_dates: group_by SUBJID, f_dose = min(as.Date(STDT, format...), na.rm=TRUE), l_dose = max(as.Date(ENDT, format...), na.rm=TRUE). Use summarise.

5. ds_final: end_dt = as.Date(DS_DT, format...), dth_fl = ifelse(DS_TERM == "DEATH", "Y", "N").

6. Merge: start with dm <- edc_dm %>% rename(SITEID = SITE?) Actually we need SITE variable for SITEID later. Keep SITE column.

left_join dm with ivrs_rand %>% select(SUBJID, TRT_CODE) (by SUBJID). left_join with ds_enrol, ex_dates, ds_final, edc_death (rename DTH_DT to death_date_raw). Use left_join.

7. Filter rows where dm present (already base). So after joins, keep rows where !is.na(dm$SUBJID) (already).

8. Create variables:

STUDYID = "ONCO-2026"
DOMAIN = "DM"
USUBJID = paste0(STUDYID, "-", SUBJID)
SITEID = SITE

BRTHDTC = format(as.Date(BRTHDT, format...), "%Y-%m-%d") ; need to parse BRTHDT.

RFICDTC = format(as.Date(RFICDT, format...), "%Y-%m-%d")

SEX = substr(GENDER, 1, 1)

AGE = floor(time_length(as.Date(RFICDTC) - as.Date(BRTHDTC), "years")) ; but need to use parsed dates.

AGEU = "YEARS"

COUNTRY = "IND"

RFSTDTC = format(enr_dt, "%Y-%m-%d")
RFXSTDTC = format(f_dose, "%Y-%m-%d")
RFXENDTC = format(l_dose, "%Y-%m-%d")
RFENDTC = format(end_dt, "%Y-%m-%d")
RFPENDTC = RFENDTC

ARMCD = TRT_CODE
ARM = case_when(ARMCD == "DRUGA" ~ "Experimental Drug A",
                ARMCD == "PBO" ~ "Placebo",
                TRUE ~ NA_character_)
ACTARMCD = ARMCD
ACTARM = ARM

DTHFL = dth_fl
DTHDTC = ifelse(!is.na(death_date_raw) & death_date_raw != "", format(as.Date(death_date_raw, format...), "%Y-%m-%d"), "")

9. Select columns in order.

10. Write CSV: write.csv(df, file.path(out, "sdtm_dm.csv"), row.names=FALSE, na="")

But spec expects output dataset name maybe "sdtm_dm". Not required. But we need listing P023.lst. We'll write listing.

We'll write listing text: write.table(df, file.path(out, "P023.lst"), row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE)

Also CSV: write.csv(df, file.path(out, "P023.csv"), row.names=FALSE, na="")

Potential missing date parsing errors: Use lubridate::parse_date_time with orders = "dmy". But month abbreviation case-insensitive.

We'll define a helper parse_sas_date <- function(x){ as.Date(x, format="%d%b%Y") }.

Now produce script.


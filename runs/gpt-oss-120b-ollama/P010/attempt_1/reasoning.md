We need to produce R script that replicates SAS program. Steps:

- Read raw_conmeds.csv from project_path/data/raw/raw_conmeds.csv. Use readr::read_csv with col_types to match lengths: SUBJECT (character), MEDICATION (character), DOSE (character), START_DT (character), END_DT (character), ONGOING (character). Ensure no type conversion.

- Process similar to SAS:

  - STUDYID = "CDISC-01"
  - DOMAIN = "CM"
  - USUBJID = paste0(STUDYID, "-", SUBJECT) after stripping spaces (trim). Use stringr::str_trim.

  - CMTRT = MEDICATION trimmed.

  - CMDECOD: if toupper(CMTRT) == "PARACETAMOL" then "ACETAMINOPHEN" else toupper(CMTRT).

  - Dose parsing: CMDOSE = numeric from first token of DOSE (scan). Use str_split(DOSE, " ", n=2). Convert first part to numeric via as.numeric.

  - CMDOSU = second token trimmed; if missing, set "".

  - Dates: START_DT and END_DT are strings in ddmmyy10. format? ddmmyy10 expects ddmmyyyy? Actually ddmmyy10. Means ddmmyyyy with slashes? But they said clean text. We'll parse with dmy(). Use lubridate::dmy. Then format to ISO 8601 date (YYYY-MM-DD). Use format(date, "%Y-%m-%d").

  - CMENRTPT: if ONGOING == "Y" then "ONGOING" else "".

  - Keep columns.

- Sort by USUBJID, CMSTDTC, CMTRT. Need to handle missing dates: missing values sort first in SAS. In R, arrange with dplyr::arrange will put NA first by default? Yes, NA are sorted last by default? Actually arrange puts NA's last. To mimic SAS (missing first), we can use factor with levels? Simpler: use order with is.na first. We'll create ordering: arrange(USUBJID, is.na(CMSTDTC), CMSTDTC, CMTRT). But need to ensure character ordering case-sensitive uppercase before lowercase. R's default ordering is based on locale, case-sensitive with uppercase before lowercase? Usually "A" < "a". So fine.

- Need DM anchor: read sdtm.dm dataset. Path: project_path/data/sdtm/dm.sas7bdat? Actually library sdtm points to derived/c2c/data/sdtm. So file path: &project_path./data/sdtm/dm.sas7bdat . Use haven::read_sas.

- Keep USUBJID and RFENDTC. Sort by USUBJID.

- Create dm_cm_anchor with _CMENTPT_ANCHOR = RFENDTC.

- Merge cm_draft with dm_cm_anchor by USUBJID (left join). Keep only rows from cm_draft (in_cm). Use dplyr::left_join.

- Compute CMENTPT: if CMENRTPT == "ONGOING" then CMENTPT = _CMENTPT_ANCHOR else "".

- If CMENRTPT == "ONGOING" and CMENTPT missing (NA or ""), log error: we can use message().

- Compute CMSEQ: within each USUBJID, assign sequence starting 1 in order of sorted data. Use dplyr::group_by and mutate(CMSEQ = row_number()).

- Retain column order: STUDYID, DOMAIN, USUBJID, CMSEQ, CMTRT, CMDECOD, CMDOSE, CMDOSU, CMSTDTC, CMENDTC, CMENRTPT, CMENTPT.

- Output dataset sdtm.cm as CSV to OUT_DIR/cm.csv.

- Also produce listing: first 15 rows printed in SAS. Need to create a table listing with same formatting as SAS PROC PRINT default? They said listing with 195 numbers. Likely they want a table of all rows? But we can produce a text rendering of the dataset (maybe first 15 rows). We'll create a text file with printed data using utils::write.table with row numbers? Provide same numbers. We'll also output CSV of same table (maybe same as dataset). The listing name P010.lst. So write text file with formatted table of sdtm.cm (maybe all rows). Use write.table with sep = "\t", quote = FALSE, row.names = FALSE. Also write CSV of same table (maybe same as dataset). But they already have dataset output. The listing may be a summary? Not clear. We'll just output the first 15 rows as listing, matching SAS PROC PRINT obs=15. So create df_listing = head(cm, 15). Write to text and CSV.

- Ensure date formatting: if date missing, keep blank.

- Use round-half-up? Not needed as no rounding.

- Use environment variable OUT_DIR.

- Use appropriate packages.

Now produce R script.

Edge Cases: Input CSV may have missing values; read_csv will treat empty as NA. We'll replace NA with "" for character columns when needed.

Implement helper function to format date: if not NA, format(date, "%Y-%m-%d") else "".

Now produce script.


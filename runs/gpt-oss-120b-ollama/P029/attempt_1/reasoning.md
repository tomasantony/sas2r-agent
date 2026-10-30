We need to produce an R script that reproduces the SAS program. Steps:

- Read required datasets: sdtm_dm (from where? Not listed in preloaded Excel. Probably in raw sdtm folder. Path: &rawsdtm. maybe contains sdtm_dm.sas7bdat? Not given. But we can assume it's a SAS dataset in raw sdtm folder. Use haven::read_sas.

- Read edc_tu from Excel path given.

- Create dm_for_tu: keep USUBJID RFSTDTC from sdtm_dm, sort by USUBJID.

- Prepare tu_prep: from edc_tu, add USUBJID = catx("-", "ONCO-2026", SUBJID). TULOC = upcase(tranwrd(LOC, "_", " ")). TUMETHOD similar.

- Merge tu_prep with dm_for_tu by USUBJID (left join, keep only rows where tu_prep present). Equivalent to merge with all=FALSE for tu_prep.

- Add fields: STUDYID, DOMAIN, TULNKID = LNKID, TUTESTCD, TUTEST, TUORRES = upcase(TYPE), TUSTRESC = TUORRES, TUEVAL = "INVESTIGATOR", VISIT = upcase(tranwrd(VISIT, "_", " ")). Convert TUDT to date: if TUDT not missing, TUDTC = as.Date(TUDT, format="%d%b%Y")? SAS date11. format is ddmmmyyyy (e.g., 01JAN2020). Use as.Date with format "%d%b%Y". Then format to yymmdd10: "YYYY-MM-DD". So TUDTC string.

- Compute TUSTDY: if TUDTC and RFSTDTC not missing. Convert both to Date. Then if _stdt >= _rfdt then TUSTDY = (_stdt - _rfdt) + 1 else TUSTDY = (_stdt - _rfdt). Note SAS date difference in days.

- Then sort by USUBJID, TUDTC.

- Derive TUSEQ: by USUBJID, increment starting at 1 for first.

- Keep specified variables.

- Output dataset tu as CSV to OUT_DIR/tu.csv.

- Also produce listing P029.lst: "table/listing text, 44 numbers". Not clear what listing is. Probably PROC PRINT of sdtm_tu. So we need to create a text rendering of the dataset (maybe first few rows?). But spec: write a text rendering and CSV of the same table. So produce a text file with formatted table similar to SAS PROC PRINT default. Could just write using write.table with row numbers? But need to match numbers formatting. Since all variables are character except TUSTDY numeric. We'll output as plain text with columns separated by spaces, preserving values. Simpler: use write.table with sep=" ", row.names=FALSE, quote=FALSE. Also produce CSV of same table (already done). But they want separate CSV for listing? Already have tu.csv. Maybe listing CSV same as dataset. We'll produce listing CSV as same.

- Ensure rounding half away from zero: not needed.

- Use environment variable OUT_DIR.

- Use haven::read_sas for sdtm_dm (path: file.path(Sys.getenv("rawsdtm"), "sdtm_dm.sas7bdat")?). Path given: &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm. So file path: D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/sdtm_dm.sas7bdat.

- Use readxl::read_excel for edc_tu: path given.

- Write CSV: write.csv(tu, file.path(out, "tu.csv"), row.names=FALSE, na="").

- Write listing text: write.table(tu, file.path(out, "P029.lst"), row.names=FALSE, col.names=TRUE, sep=" ", quote=FALSE).

- Also write CSV of listing: write.csv(tu, file.path(out, "P029.csv"), row.names=FALSE, na="").

- Ensure character columns are trimmed? Use stringsAsFactors=FALSE.

- Use dplyr for operations.

- Sorting: need to sort with character ordering uppercase before lowercase. dplyr arrange will use locale? In R, default sorting is based on code point, uppercase before lowercase, matches SAS. So fine.

- Ensure missing values sort first: arrange will put NA first by default? In dplyr, NA are sorted last by default. To mimic SAS (missing first), we can use arrange with desc(is.na(...))? Actually we need missing first. Use arrange with is.na(var) descending? Let's just use arrange with desc(is.na(var))? Wait: if is.na=TRUE then we want first. So we can arrange with desc(!is.na(var)). Simpler: use base order with na.last=FALSE. We'll use arrange with dplyr::arrange using .by_group? Not straightforward. We'll use base order: df[order(is.na(df$USUBJID), df$USUBJID), ].

- For sorting by USUBJID then TUDTC: use order with na.last=FALSE.

Implement.

Edge: TUDT variable may be character; need to check column name. In edc_tu, there is TUDT column (maybe date string). We'll treat as character.

Implement round-half-up not needed.

Now produce script.
We need to produce R script that replicates SAS program. Steps:

- Read raw_demog.csv from &project_path./data/raw/raw_demog.csv. Macro variable project_path is D:/meta_hackathon/sas2r-agent/data/derived/c2c. So path: D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv

- Create dataset ds with columns as defined. Need to replicate SAS logic.

- Variables: STUDYID $8, DOMAIN $2, USUBJID $&usubjid_length. (usubjid_length=40). DSCAT, DSSCAT $40, DSTERM, DSDECOD $80, EPOCH $20, DSSTDTC $10.

- Retain order: SAS retain ensures variables appear in that order and values persist across rows (but they are set each row). In R we just create columns.

- For each row of raw_ds:

   STUDYID = "CDISC-01"
   DOMAIN = "DS"
   USUBJID = paste0(STUDYID, "-", SUBJ_ID) where SUBJ_ID column from raw_ds (likely variable name SUBJ_ID). Use catx("-", ...) which concatenates with hyphen and strips spaces. We'll use paste0(STUDYID, "-", raw$SUBJ_ID) after trimming.

   DSSEQ = 1 (constant)

   DSCAT = "DISPOSITION EVENT"
   DSSCAT = "STUDY PARTICIPATION"
   DSTERM = toupper(trimws(DISPOSITION_REASON))
   DSDECOD = DSTERM

   EPOCH: if toupper(trimws(DISPOSITION)) == "SCREEN FAILURE" then "SCREENING" else "FOLLOW-UP"

   For DSSTDTC: need to handle STUDY_END_DAT variable. If its type is character then parse as date using anydtdte. In SAS, anydtdte. reads many date formats. In R we can use lubridate::parse_date_time with orders = c("Ymd", "Y-m-d", "mdY", etc). But we can approximate: if column is character, try as_date with ymd or mdy. We'll use lubridate::ymd, mdy, dmy fallback.

   If not missing, format as ISO 8601 date (YYYY-MM-DD). SAS format is is8601da. which yields date only. So DSSTDTC = format(_dsdt, "%Y-%m-%d").

- Keep only listed variables.

- Then sort by USUBJID, DSSEQ. In R, arrange.

- Output dataset ds as CSV to OUT_DIR/ds.csv with row.names=FALSE, na="". Use write.csv.

- Also produce listing: first 12 observations printed. Need to create a text rendering of the table with same formatting as SAS PROC PRINT default. SAS prints column headers and values with default width. We need to approximate: produce a plain text file with the first 12 rows, maybe using utils::write.table with sep=" ", quote=FALSE, row.names=FALSE. But need to match numbers (none). Provide also CSV of the same table? The spec: each output dataset as CSV; each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers the SAS output shows (counts, percentages...). For listing, we need a text rendering of the printed dataset. We'll create a data frame first12 = head(ds,12). Write to txt using write.table with sep="\t" maybe. Provide also CSV of the table body? Already dataset ds is CSV; but listing also needs CSV of the table body? spec says each table or listing additionally as a plain-text rendering and as CSV of the table body. So for listing, we need both .txt and .csv (maybe same name?). We'll create listing name "P012_lst" maybe? Expected output: P012.lst (listing). So we should output P012.lst.txt and P012.lst.csv? The spec: "P012.lst (table/listing text, 108 numbers) -> write a text rendering and CSV of the same table". So base name P012.lst. We'll write file.path(out, "P012.lst.txt") and "P012.lst.csv". The CSV will contain same columns as printed.

- No figures.

- Use environment variable OUT_DIR.

- Use packages: dplyr, readr, stringr, lubridate.

- Ensure case-sensitive sorting: arrange will sort ascending; character ordering in R is based on locale; uppercase before lowercase? In default locale, uppercase letters come before lowercase in ASCII. So fine.

- Missing values sort first: arrange will put NA first by default? In dplyr, NA are sorted last by default. To mimic SAS, we need NA first. Use arrange with .by_group? Actually dplyr's arrange puts NA at the end. To get NA first, we can use arrange(is.na(USUBJID), USUBJID, DSSEQ). But USUBJID likely never missing. So fine.

- Write script with progress messages via message().

- Ensure no install.packages.

- Use readr::read_csv for raw.

- Use write.csv for outputs.

- Use cat for txt.

Implement round-half-up not needed.

Let's craft script.


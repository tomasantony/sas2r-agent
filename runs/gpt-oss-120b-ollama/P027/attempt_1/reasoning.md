We need to produce R script that reproduces SAS program. Steps:

- Load required libraries.
- Set environment variables: root, pid, rawadam, etc maybe not needed. Use OUT_DIR env var for output.

- Load work datasets: they are pre-loaded from Excel files. In R we need to read them. The environment says "WORK datasets pre-loaded by the environment from these Excel files (dataset name -> file)". So we need to read each Excel file into a data frame with same name. Use readxl::read_excel.

- The SAS program uses sdtm_dm dataset (from SDTM). Not listed in pre-loaded. Probably exists in raw sdtm? The macro variable &rawsdtm. But not pre-loaded. However we need to read sdtm_dm from raw sdtm directory. Path: &rawsdtm.\dm.sas7bdat? Not given. Might be a SAS dataset. Could be .xpt. But we can assume it's a SAS dataset file named dm.sas7bdat in &rawsdtm. Path: D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.sas7bdat maybe. We'll read using haven::read_sas.

- Also need edc_rs dataset from Excel file path given.

- Steps:

1. Prepare DM reference data: sort by USUBJID, keep USUBJID RFSTDTC.

2. Prepare Raw Data: create rs_prep from edc_rs, add USUBJID = paste0("ONCO-2026-", SUBJID). Ensure length.

3. Sort both.

4. Map RS domain: merge rs_prep and dm_for_rs by USUBJID (inner join, keep only rows where rs_prep present). In SAS merge with in=r and if r; So left join of rs_prep onto dm_for_rs, keep only rows where rs_prep exists. Equivalent to inner join.

Create columns as per SAS.

- RSTESTCD = TESTCD.

- If RSTESTCD == "OVRLRESP" then RSTEST = "Overall Response". Else missing.

- RSCAT = "RECIST 1.1".

- RSORRES = toupper(ORRES); RSSTRESC = RSORRES.

- RSEVAL = "INVESTIGATOR".

- VISIT = toupper(gsub("_", " ", VISIT))

- Date conversion: if RSDT not missing/empty, then RSDTC = format(as.Date(RSDT, format="%d%b%Y"? Actually SAS date11. format expects date values like "01JAN2020". Input(RSDT, date11.) reads SAS date numeric? Wait RSDT variable likely character string representing date in format date11. In SAS date11. is ddmmmyyyy (e.g., 01JAN2020). So we need to parse that. Use lubridate::dmy with month abbreviation. So as.Date(RSDT, format="%d%b%Y") . Then format to yymmdd10. => "YYYY-MM-DD". So RSDTC = format(date, "%Y-%m-%d").

- Study Day: if both RSDTC and RFSTDTC not missing, compute _stdt = as.Date(RSDTC); _rfdt = as.Date(RFSTDTC). Then if _stdt >= _rfdt then RSSTDY = as.numeric(_stdt - _rfdt) + 1 else RSSTDY = as.numeric(_stdt - _rfdt). Note SAS date difference returns days.

- Keep variables as defined.

5. Derive RSSEQ: sort by USUBJID RSDTC, then within each USUBJID assign sequence starting at 1.

6. Output dataset rs (sdtm_rs) to CSV in OUT_DIR/rs.csv.

Also need listing P027.lst: "table/listing text, 30 numbers". Not clear what listing is. Probably PROC PRINT of sdtm_rs. So we need to produce a text rendering of the dataset (maybe first few rows?). But spec says produce a text rendering and CSV of the same table. So we need to write a .txt file with the printed table (like SAS PROC PRINT). We'll just write the whole dataset as a formatted table with columns separated by spaces, preserving numeric formatting (no rounding). Use write.table with row.names=FALSE, col.names=TRUE, sep=" ", quote=FALSE. Save as P027.txt. Also CSV already saved as rs.csv.

But they expect listing name P027.lst. So we need to write file path file.path(out, "P027.lst") as text, and also maybe CSV "P027.csv". The spec: each output dataset as CSV; each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")). So for listing name "P027" we produce P027.txt and also maybe P027.csv? It says "and as CSV of the table body". So we should also write CSV of same table: file.path(out, "P027.csv").

Thus we will write both.

Implement round-half-up helper? Not needed as no rounding.

Implement sorting with character ordering: R's default sort is case-sensitive and puts uppercase before lowercase? In ASCII, uppercase (65-90) before lowercase (97-122). So default works.

Missing values sort first: not needed for final dataset.

Implement retain RSSEQ: we can use dplyr::group_by and mutate(RSSEQ = row_number()).

Now produce script.

Edge Cases: columns types.

Read edc_rs: need to know column names: SUBJID, TESTCD, ORRES, VISIT, RSDT maybe. We'll just read as is.

Read sdtm_dm: columns USUBJID, RFSTDTC.

Now script.

Write outputs:

out <- Sys.getenv("OUT_DIR")
if (out == "") stop("OUT_DIR not set")

Create directories if not exist.

Write rs.csv.

Write listing: P027.txt and P027.csv.

Implement.

Let's craft final R script.
We need to produce a single R script that reproduces outputs: dataset vs (CSV) and listing P018.lst (the PROC PRINT output). Must read raw_vitals.csv from path: &project_path./data/raw/raw_vitals.csv where project_path = D:/meta_hackathon/sas2r-agent/data/derived/c2c . So full path: D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_vitals.csv . Use readr::read_csv.

Implement steps: create vs_mapped similar to SAS data step.

Key details: SAS length definitions not needed. USUBJID = catx("-", STUDYID, PT_ID). So need PT_ID column in raw data. VISITNUM = VISIT_NUM.

Date handling: VS_DATE column? In raw data there is VS_DATE column maybe character or numeric. SAS code: if vtype(VS_DATE)='C' then _vs_num = input(strip(VS_DATE), anydtdte.); else _vs_num = VS_DATE; then VSDTC = put(_vs_num, is8601da.) which is ISO 8601 date (YYYY-MM-DD). So we need to parse VS_DATE accordingly. In R, we can check class: if it's character, parse with anydate from lubridate? anydtdte. handles many formats. We'll use lubridate::ymd or parse_date_time. Use parse_date_time with orders = c("Ymd","Y-m-d","Y/m/d","mdY","dmy","Ymd HMS") etc. Simpler: use lubridate::ymd if numeric? Actually if numeric (SAS date numeric days since 1960). In R, SAS date numeric can be converted by as.Date(num, origin="1960-01-01"). So we need to detect if column is numeric. We'll implement: if is.character(vs_date) then parse with parse_date_time; else if numeric then as.Date(vs_date, origin="1960-01-01").

Then VSDTC as character in ISO format: format(date, "%Y-%m-%d").

Now unpivot: For each row, generate up to 4 rows for each measurement if not missing. Use tidyr::pivot_longer? But need custom mapping for each variable to test code and units. We'll use rowwise and bind_rows.

Implementation: iterate over rows using dplyr::rowwise and then create list of rows.

Simpler: use base R loops to replicate SAS output exactly.

We'll create empty list, then for each row, for each measurement variable (SYS_BP, DIA_BP, HR_BPM, WEIGHT_KG) check not NA, then create a data.frame with appropriate fields.

Variables: STUDYID = "CDISC-01", DOMAIN = "VS", USUBJID = paste0(STUDYID, "-", PT_ID), VISITNUM = VISIT_NUM, VISIT maybe present? Keep as is from raw (maybe VISIT). Keep VSDTC computed.

Fields: VSTESTCD, VSTEST, VSORRES (character of numeric with best. format: SAS best. prints without scientific notation, no trailing zeros. In R, we can use formatC(x, format="fg", digits=12, flag="#")? Simpler: as.character(x) will produce maybe scientific for large numbers. We'll use format(x, scientific=FALSE, trim=TRUE). That should match.

VSSTRESN = numeric value.

VSORRESU, VSSTRESU as per mapping.

Also keep VISIT variable? In keep list includes VISIT (maybe from raw). We'll include VISIT column from raw.

After building vs_mapped, sort by USUBJID, VSDTC, VSTESTCD.

Then generate VSSEQ: for each USUBJID, assign sequence starting 1 in order of sorted data. In SAS they used retain and by USUBJID after sorting by USUBJID VSDTC VSTESTCD. So we replicate: group_by(USUBJID) arrange within group by VSDTC, VSTESTCD (already sorted). Then mutate(VSSEQ = row_number()).

Create final dataset vs with columns: STUDYID, DOMAIN, USUBJID, VSSEQ, plus other columns? SAS retains only STUDYID DOMAIN USUBJID VSSEQ then set work.vs_mapped (which includes many columns). So final dataset includes all columns from vs_mapped plus VSSEQ. So we keep all columns.

Write vs.csv to OUT_DIR/vs.csv.

Now listing: PROC PRINT of first 12 observations of sdtm.vs, showing variables USUBJID VSSEQ VISITNUM VISIT VSDTC VSTESTCD VSSTRESN VSSTRESU.

We need to produce a text rendering and CSV of same table. The text rendering should mimic SAS PROC PRINT output: includes column headers and values aligned? We can produce simple plain text with tab-separated values. Provide same numbers (132 numbers total across all outputs). We'll just create a data frame of first 12 rows with selected columns, write to txt using write.table with sep = " " maybe. Use write.table(..., row.names=FALSE, col.names=TRUE, quote=FALSE). That will produce a plain text table.

Also write CSV of same table: write.csv.

Naming: listing file name P018.lst . So write to file.path(out, "P018.lst") as txt, and also maybe "P018.csv"? The spec says each table or listing additionally as a plain-text rendering (txt) and as CSV of the table body. So we need both .txt and .csv for listing. Use same base name? We'll write P018.txt and P018.csv.

Now implement script.

Need to get OUT_DIR from env var. Use out <- Sys.getenv("OUT_DIR"); if empty, maybe default to ".".

Implement round-half-up? Not needed.

Now produce final R script.


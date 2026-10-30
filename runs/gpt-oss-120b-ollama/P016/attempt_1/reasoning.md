We need to produce an R script that replicates the SAS program. Steps:

- Read raw_mh.csv from &project_path./data/raw/raw_mh.csv. The macro variable project_path is D:/meta_hackathon/sas2r-agent/data/derived/c2c. So path: D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_mh.csv

- Output dataset sdtm.mh as CSV to OUT_DIR (environment variable). Name "mh.csv". Also need to output a .txt rendering? The declared output types: dataset only. So only dataset needed. But spec says each output dataset as CSV. So just write mh.csv.

- Need to replicate SAS logic:

  - length definitions: STUDYID $8, DOMAIN $2, USUBJID $&usubjid_length. (usubjid_length=40). MHTERM $60, MHSTDTC $10.

  - Set STUDYID = "CDISC-01"; DOMAIN = "MH"; USUBJID = catx('-', STUDYID, strip(SUBJECT)); MHTERM = strip(CONDITION);

  - DIAGNOSIS_DATE may be character or numeric SAS date. In SAS, numeric date is days since 1960-01-01. So need to detect type. In R, after reading CSV, columns are likely character or numeric. We'll treat DIAGNOSIS_DATE column: if it's character, parse with anydate (anydtdte). In SAS, anydtdte. reads many date formats, returns SAS date numeric. We'll convert to Date using as.Date(value, origin="1960-01-01") after parsing.

  - For character dates: we need to parse to SAS date numeric. In R, we can use lubridate::parse_date_time with many orders, but easier: use anytime? Not available. We'll use lubridate::ymd, mdy, dmy etc. But we can use readr::parse_date_time with orders = c("Ymd","mdY","dmY","Y-m-d","m/d/Y","d/m/Y","Ymd HMS", etc). We'll just use lubridate::ymd etc fallback.

Simplify: Use as.Date(DIAGNOSIS_DATE, tryFormats = c("%Y-%m-%d","%m/%d/%Y","%d/%m/%Y","%Y%m%d")).

If parsing fails returns NA.

- Then if not missing, MHSTDTC = put(_mhdt, is8601da.) which is ISO 8601 date (YYYY-MM-DD). So we can format as format(date, "%Y-%m-%d").

- Keep only required variables.

- Then sort by USUBJID, MHSTDTC (character sorting, missing first). In R, arrange with dplyr, using factor? For character sorting, default is lexical, case-sensitive, missing (NA) first. That's fine.

- Then create MHSEQ: by USUBJID, first gets 1, else increment. Equivalent to group_by USUBJID, mutate(MHSEQ = row_number()).

- Retain order of columns: STUDYID DOMAIN USUBJID MHSEQ MHTERM MHSTDTC.

- Write CSV with row.names=FALSE, na = "".

- Ensure variable names case as in SAS: uppercase.

- Use environment variable OUT_DIR.

- Use haven? Not needed.

- Use base R.

- Ensure rounding not needed.

- Use catx '-', strip: strip removes leading/trailing blanks. In R, trimws.

- Ensure USUBJID length up to 40; not needed.

- Provide progress messages via cat.

- Ensure script runs non-interactively.

Now produce final R script inside ```r``` block.
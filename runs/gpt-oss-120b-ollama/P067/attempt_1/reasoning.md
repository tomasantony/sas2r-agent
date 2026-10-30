We need to produce R script that reads raw.raw_ae and sdtm.dm, merges by SUBJID, keeps only those with AE (i.e., raw_ae present). Then create variables: STUDYID = "ABC-301". AESTDTC = put(AESTDT, yymmdd10.) meaning format date as YYYY-MM-DD. Input variable AESTDT likely numeric SAS date (days since 1960). In R, dates may be read as Date class if haven reads SAS date numeric as Date? Haven reads SAS date numeric as Date class (days since 1960). So we can format with as.character(date) which yields "YYYY-MM-DD". Similarly AEENDTC.

rfstdt_num = input(RFSTDTC, yymmdd10.) where RFSTDTC is character date string "YYYY-MM-DD". So convert to Date.

ASTDY: if not missing(AESTDT) then if AESTDT >= rfstdt_num then ASTDY = AESTDT - rfstdt_num + 1 else ASTDY = AESTDT - rfstdt_num. Note AESTDT is Date, subtract yields numeric days. Need to treat missing: is.na.

Similarly AENDY.

Keep variables: STUDYID, USUBJID, SUBJID, AETERM, AESER, AESEV, AESTDTC, AEENDTC, ASTDY, AENDY.

Also need to output dataset ae.csv and raw_ae.csv? Wait expected outputs: dataset ae and raw_ae. raw_ae is presumably the raw dataset? Actually raw_ae is input raw.raw_ae; they want to output raw_ae as is? The SAS program only creates sdtm.ae. But expected outputs include raw_ae. Possibly they want to write the input raw.raw_ae as raw_ae dataset (maybe after sorting). We'll replicate: after sorting raw.raw_ae, write to OUT_DIR/raw_ae.csv (maybe same as input). We'll also write ae dataset.

Need to sort raw.raw_ae by SUBJID and sdtm.dm by SUBJID (like SAS). Sorting in R: arrange.

When merging, SAS merge by SUBJID with BY statement, which merges sorted datasets, keeping all rows from both but then if a (raw) present, keep. Equivalent to inner join on SUBJID where raw present. But SAS merge with BY and if a then keep only rows where raw present, but also includes dm variables (all). If multiple rows per SUBJID in raw or dm, SAS will produce Cartesian? Usually both are one-to-one. We'll assume one-to-one.

Implementation steps:

out <- Sys.getenv("OUT_DIR")
if (out == "") stop("OUT_DIR not set")

Read raw_ae: raw_raw_ae <- haven::read_sas(file.path(Sys.getenv("rawrawdir"?), ...)). Actually macro &rawadam etc. But we have libref raw pointing to D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw. So raw.raw_ae path: D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw/raw_ae.sas7bdat . We'll construct path: file.path("D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw","raw_ae.sas7bdat").

Similarly sdtm.dm path: D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm/dm.sas7bdat.

Read using haven::read_sas.

Sort using dplyr::arrange.

Write raw_ae sorted to csv.

Merge: left_join raw_ae (as a) with dm by SUBJID, but keep only rows where raw_ae present (inner join). Use inner_join.

Create variables.

Note: USUBJID variable likely present in raw_ae or dm. Keep.

Formatting: AESTDTC and AEENDTC as character strings "YYYY-MM-DD". Use format(date, "%Y-%m-%d") or as.character.

rfstdt_num: convert RFSTDTC (character) to Date using lubridate::ymd.

Compute ASTDY, AENDY numeric.

Set format? Not needed for CSV.

Write ae dataset to csv.

Also need to output raw_ae dataset as CSV (sorted). Not need to output txt.

Implement round-half-up? Not needed.

Write script with progress messages via message().

Make sure to use na = "" in write.csv.

Let's craft script.


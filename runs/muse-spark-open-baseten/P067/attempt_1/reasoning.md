Need to translate SAS program P067_ae.sas to R.

We have librefs:
raw -> D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw
sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm
adam -> D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam

Program does:
proc sort data=raw.raw_ae; by SUBJID; run;
proc sort data=sdtm.dm; by SUBJID; run;

data sdtm.ae;
 merge raw.raw_ae (in=a) sdtm.dm; by SUBJID; if a;
 length STUDYID $10 AESTDTC AEENDTC $10;
 STUDYID = "ABC-301";
 AESTDTC = put(AESTDT, yymmdd10.);
 AEENDTC = put(AEENDT, yymmdd10.);
 rfstdt_num = input(RFSTDTC, yymmdd10.);
 if not missing(AESTDT) then do;
   if AESTDT >= rfstdt_num then ASTDY = AESTDT - rfstdt_num + 1;
   else ASTDY = AESTDT - rfstdt_num;
 end;
 if not missing(AEENDT) then do;
   if AEENDT >= rfstdt_num then AENDY = AEENDT - rfstdt_num + 1;
   else AENDY = AEENDT - rfstdt_num;
 end;
 format ASTDY AENDY 8.;
 keep STUDYID USUBJID SUBJID AETERM AESER AESEV AESTDTC AEENDTC ASTDY AENDY;
run;

Expected outputs:
 dataset ae -> write OUT_DIR/ae.csv
 dataset raw_ae -> write OUT_DIR/raw_ae.csv

Wait raw_ae? The program creates sdtm.ae. But expected outputs say dataset ae and raw_ae. Possibly they want to output both? The SAS program reads raw.raw_ae and sdtm.dm, creates sdtm.ae. So we need to produce ae.csv (which is sdtm.ae) and raw_ae.csv (which is raw.raw_ae). Or maybe they want to copy raw_ae as well.

We need to locate files.

We have directories:
raw -> D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw
sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm

We need to list files. Use R to explore? But we are to produce script that will run. We need to infer file types.

In family environment, libname raw "&root.\data\derived\wxzhai\raw"; That is a directory. So datasets are likely .sas7bdat files inside.

We need to write R script that reads those.

We need to handle OUT_DIR env var.

We need to reproduce SAS semantics: merge by SUBJID, keep only subjects who have AE (in=a). That's left join where raw_ae exists. If multiple rows per SUBJID in both datasets, SAS merge by BY does interleaving? Actually SAS merge with BY SUBJID when both sorted by SUBJID will do many-to-many? SAS merge behavior: if both have multiple observations per BY group, it does one-to-one within group sequentially. But typical DM has one row per SUBJID, raw_ae has multiple rows per SUBJID. So it's effectively left join of raw_ae to dm on SUBJID.

We need to replicate.

Also need to handle dates: AESTDT, AEENDT are numeric SAS dates? And RFSTDTC is character ISO date (yymmdd10.). In SAS, input(RFSTDTC, yymmdd10.) converts to numeric SAS date (days since 1960-01-01). Then ASTDY calculation uses numeric dates.

In R, we need to read via haven::read_sas which will convert SAS dates to R Date objects? Haven reads SAS dates as Date. Need to check.

We need to handle both possibilities: if AESTDT is Date, then we can compute difference. If it's numeric, we need to convert.

Similarly RFSTDTC is character like "2020-01-01". Input with yymmdd10. parses it.

We need to produce AESTDTC and AEENDTC as character ISO dates formatted yymmdd10. i.e., "YYYY-MM-DD". SAS put(AESTDT, yymmdd10.) does that. If AESTDT missing, then AESTDTC = ""? In SAS, put(missing, yymmdd10.) yields "."? Actually for numeric missing, put with date format yields "."? Let's check SAS behavior: put(., yymmdd10.) returns "          " (blank)? Or "."? For date formats, missing yields blank? Need to recall. In SAS, put(., yymmdd10.) results in blank string (10 spaces). But when assigned to length $10 variable, it would be blank (spaces trimmed?). In R we should produce NA or ""? The expected output CSV likely has blank for missing. The instruction says write CSV with na = "". So we should produce NA for missing, which will be written as "".

Similarly for AEENDTC.

We need to keep variables: STUDYID, USUBJID, SUBJID, AETERM, AESER, AESEV, AESTDTC, AEENDTC, ASTDY, AENDY

We need to also output raw_ae.csv as copy of raw.raw_ae dataset.

We need to ensure script runs non-interactively, reads inputs from absolute paths given.

We need to handle file discovery: raw.raw_ae could be file raw_ae.sas7bdat in raw directory, or raw_ae.xpt, etc. Use logic to find file.

Similarly sdtm.dm.

We should implement helper to read dataset: try .sas7bdat, .xpt, .sas7bdat etc.

We need to handle OUT_DIR.

We need to produce ae.csv and raw_ae.csv.

Also need to handle sorting: SAS sorts with character ordering uppercase before lowercase. R default sort may differ but for merge we just need to sort by SUBJID.

We need to handle missing values sort first.

We need to handle date arithmetic.

We need to produce script that does not require manual inspection.

We should write R script that:

- gets out <- Sys.getenv("OUT_DIR"); if empty, use temp.

- defines root etc.

- defines helper read_sas_dataset(libpath, dsname): looks for files: dsname.sas7bdat, dsname.sas7bdat (lowercase), dsname.xpt, etc. Use list.files.

- reads raw_ae and dm.

- then does merge.

- then creates STUDYID, AESTDTC, AEENDTC, ASTDY, AENDY.

- Need to handle conversion of RFSTDTC to Date: as.Date(RFSTDTC, format="%Y-%m-%d") or yymmdd10. SAS yymmdd10. accepts "YYYY-MM-DD". So use as.Date.

- For AESTDT, AEENDT: if they are already Date (haven converts), then we can format. If they are numeric (SAS date numbers), haven would have converted to Date automatically? Haven does convert SAS date values to Date if format is set. If not, they remain numeric. So we need to handle both: if inherits Date, use it; if numeric, convert via as.Date(numeric, origin="1960-01-01").

- For rfstdt_num: we need numeric SAS date equivalent, but we can just use Date difference. Simpler: compute ASTDY as difference in days between AESTDT Date and RFSTDTC Date.

SAS logic: if AESTDT >= rfstdt_num then ASTDY = AESTDT - rfstdt_num +1 else ASTDY = AESTDT - rfstdt_num

In R Date difference: as.numeric(AESTDT - RFSTDTC_Date) gives days. Then if >=0, +1.

Similarly for AENDY.

- Need to handle missing: if AESTDT is NA, ASTDY = NA.

- Need to handle RFSTDTC missing: then rfstdt_num missing, then ASTDY missing? In SAS, if rfstdt_num missing, then comparison AESTDT >= . is false? Actually in SAS, missing is considered less than any non-missing, so AESTDT >= . would be true (since any number > missing). But then ASTDY = AESTDT - . = . (missing). So result missing anyway. So we can just if RFSTDTC is NA, then ASTDY = NA.

- Need to keep only subjects who have AE: that's all rows from raw_ae, left join dm. So we don't need to filter further.

- Need to handle duplicate SUBJID in dm? Assume one per.

- Need to produce final dataset with columns in order: STUDYID, USUBJID, SUBJID, AETERM, AESER, AESEV, AESTDTC, AEENDTC, ASTDY, AENDY

- Need to ensure types: STUDYID character, AESTDTC character, etc.

- Need to write CSVs: write.csv(df, file.path(out, "ae.csv"), row.names=FALSE, na="")

- Also write raw_ae.csv: need to write raw_ae dataset as is, but with dates as YYYY-MM-DD etc. Instruction says write every output dataset as CSV with SAS dataset's variable names, dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS, numeric values unrounded unless SAS rounds them.

So for raw_ae.csv, we should convert Date columns to character YYYY-MM-DD for output? But write.csv will write Date as YYYY-MM-DD automatically. For datetime, need to format.

We should ensure we don't alter raw_ae too much; just write as read, but convert dates to proper format.

- Also need to handle that expected outputs are ae and raw_ae. The SAS program creates sdtm.ae, which we map to ae.csv.

- Need to ensure script creates OUT_DIR if not exists.

- Need to handle haven reading: haven::read_sas may produce labelled vectors. Need to convert to character/numeric as needed. Use haven::zap_labels? Or just as is.

- Need to handle SUBJID type: could be numeric or character. Merge by SUBJID should be consistent.

- Need to handle USUBJID coming from dm (since raw_ae may not have it). In SAS merge, USUBJID from dm will be merged.

- Need to handle that raw_ae may have USUBJID already? But keep list includes USUBJID, so it will be from either.

- Need to implement SAS-like formatting for ASTDY, AENDY as 8. (integer). Just numeric.

- Need to ensure we drop rfstdt_num (commented out in SAS, but they keep it? Actually they have * drop rfstdt_num; commented, so they keep it? But keep statement excludes it, so it's dropped anyway. So we should not include rfstdt_num in final.

- Need to handle length STUDYID $10 etc. Not needed in R.

- Need to produce script that is standalone.

We should also handle reading from alternative paths: The macro variables indicate rawadam etc., but librefs are derived/wxzhai. So we use those.

We need to write R code inside one ```r block.

We should include helper functions: sas_date_to_r, format_yymmdd10, etc.

We should also handle case where files are .sas7bdat vs .xpt.

We should implement robust file finding.

We should also handle that raw directory may contain files with uppercase names.

We should add progress messages via cat.

We should ensure we use only allowed packages.

We need to produce final script.

Let's craft script.

Steps in script:

- out <- Sys.getenv("OUT_DIR"); if out=="" out <- "output" etc.

- root <- "D:/meta_hackathon/sas2r-agent" (but we can derive from librefs absolute paths)

- raw_path <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw"

- sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm"

- helper read_dataset <- function(dir, name) { ... }

Implementation of read_dataset:

read_dataset <- function(dir, ds) {
  # list files in dir
  files <- list.files(dir, full.names=TRUE)
  # try exact match case-insensitive
  # look for ds.sas7bdat, ds.xpt, ds.sas7bdat etc.
  candidates <- c(
    file.path(dir, paste0(ds, ".sas7bdat")),
    file.path(dir, paste0(ds, ".sas7bdat")),
    file.path(dir, paste0(tolower(ds), ".sas7bdat")),
    file.path(dir, paste0(toupper(ds), ".sas7bdat")),
    file.path(dir, paste0(ds, ".xpt")),
    file.path(dir, paste0(tolower(ds), ".xpt")),
    file.path(dir, paste0(toupper(ds), ".xpt")),
    file.path(dir, paste0(ds, ".sas7bdat")),
    file.path(dir, paste0(ds, ".sas7bdat"))
  )
  # also search case-insensitive
  idx <- which(tolower(basename(files)) == tolower(paste0(ds, ".sas7bdat")))
  if (length(idx)>0) candidates <- c(files[idx], candidates)
  idx2 <- which(tolower(basename(files)) == tolower(paste0(ds, ".xpt")))
  if (length(idx2)>0) candidates <- c(files[idx2], candidates)
  for (f in candidates) {
    if (file.exists(f)) {
      ext <- tolower(tools::file_ext(f))
      if (ext == "sas7bdat") {
        df <- haven::read_sas(f)
        return(df)
      } else if (ext == "xpt") {
        df <- haven::read_xpt(f)
        return(df)
      }
    }
  }
  # fallback: list.files pattern
  pat <- paste0("(?i)^", ds, "\\.(sas7bdat|xpt)$")
  matched <- list.files(dir, pattern=pat, full.names=TRUE, ignore.case=TRUE)
  # Actually list.files pattern is regex, need to handle
  # Use grepl
  allfiles <- list.files(dir, full.names=TRUE)
  sel <- allfiles[grepl(paste0("^", ds, "\\.(sas7bdat|xpt)$"), basename(allfiles), ignore.case=TRUE)]
  if (length(sel)>0) {
    f <- sel[1]
    ext <- tolower(tools::file_ext(f))
    if (ext == "sas7bdat") return(haven::read_sas(f))
    if (ext == "xpt") return(haven::read_xpt(f))
  }
  stop(paste0("Dataset ", ds, " not found in ", dir))
}

But we need to handle that haven::read_sas may not be available for .sas7bdat? It is.

We also need to handle that raw.raw_ae may be stored as raw_ae.sas7bdat.

We should also handle that sdtm.dm may be dm.sas7bdat.

We need to handle haven labelled conversion: use haven::zap_formats? But we can keep.

We need to handle date conversion helper:

to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS numeric date: days since 1960-01-01
    # But haven already converts SAS dates to Date, so numeric would be SAS date number if format not recognized
    # We need to detect: if numeric and large (e.g., > 10000), likely SAS date
    # Convert
    return(as.Date(x, origin="1960-01-01"))
  }
  if (is.character(x)) {
    # try yymmdd10.
    # SAS yymmdd10. can be "2020-01-01" or "2020/01/01" etc.
    # Use as.Date with try
    d <- suppressWarnings(as.Date(x, format="%Y-%m-%d"))
    if (all(is.na(d) & !is.na(x))) {
      d <- suppressWarnings(as.Date(x, format="%Y/%m/%d"))
    }
    return(d)
  }
  return(as.Date(x))
}

But for AESTDT, AEENDT, they are numeric SAS dates, haven will convert to Date if format attached. So to_date will handle.

For RFSTDTC, it's character ISO date, we need to parse.

We need to compute ASTDY, AENDY.

We need to handle missing: is.na.

We need to format AESTDTC, AEENDTC as character YYYY-MM-DD: format(date, "%Y-%m-%d")

We need to handle that SAS put with yymmdd10. for missing yields blank (10 spaces). In R we will produce NA, which becomes "" in CSV.

We need to ensure final dataset column order.

We also need to handle that SUBJID may be numeric or character; ensure merge works.

We need to handle that dm may have RFSTDTC variable.

We need to handle that raw_ae may have AESTDT, AEENDT as Date or numeric.

We need to implement merge: left join raw_ae to dm by SUBJID.

In SAS, merge by SUBJID with both sorted will produce for each SUBJID, if dm has 1 row and raw_ae has n rows, it will replicate dm values for each raw_ae row. That's equivalent to left join.

We can do dplyr::left_join(raw_ae, dm, by="SUBJID", suffix=c("", ".dm")) but need to handle duplicate column names: USUBJID may exist in both, but we want dm's USUBJID? In SAS merge, if both have same variable (other than BY), the second dataset's value overwrites first's? Actually SAS merge: if same variable appears in multiple datasets, the value from the last dataset in merge list overwrites earlier. So dm's values would overwrite raw_ae's for overlapping variables. But keep list includes USUBJID, which could be from either, but dm's would win. We should implement that: for overlapping columns (excluding BY), take dm's value if not NA, else raw's.

Simpler: do left_join and then coalesce.

But we need to know which columns overlap. We can just do left_join and handle.

We should also handle that dm may have STUDYID? But we overwrite STUDYID with "ABC-301" anyway.

We need to handle that raw_ae may have STUDYID already, but we set to "ABC-301".

We need to produce final ae dataset.

We also need to write raw_ae.csv: we should write the original raw_ae dataset as read, but with dates formatted as YYYY-MM-DD for CSV. However write.csv will handle Date correctly. For haven labelled, we need to convert to character/numeric.

We should also ensure we handle that raw_ae may have datetime variables: need to format as YYYY-MM-DDTHH:MM:SS.

We can implement helper to convert all Date/POSIXt columns to character for output? But instruction says dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS, numeric values unrounded unless SAS rounds them. So we can just let write.csv handle Date as YYYY-MM-DD, but for POSIXt we need to format.

Simpler: for raw_ae output, we can just write the data frame as is, but convert POSIXt to character formatted.

We should also handle haven labelled: convert labelled to character/numeric via haven::zap_labels? Actually haven::read_sas returns labelled vectors for categorical. We should convert them to character for output: use haven::as_factor or just as.character. Better to use haven::zap_labels and then convert factors.

We can do: df <- haven::zap_formats(df); df <- haven::zap_label(df); df <- haven::zap_labels(df) ??? Let's check.

Simpler: for each column, if inherits "haven_labelled", convert to as.character or as.numeric depending.

We can implement: for (col in names(df)) if (inherits(df[[col]], "haven_labelled")) df[[col]] <- haven::zap_labels(df[[col]])

But haven::zap_labels removes labels but keeps values.

We should do that for both datasets.

We need to ensure we handle that SUBJID may be labelled.

We should also handle that we need to produce ae.csv with correct column names uppercase as in SAS: STUDYID, USUBJID, SUBJID, AETERM, AESER, AESEV, AESTDTC, AEENDTC, ASTDY, AENDY

We need to ensure we output those exact names.

We should also handle that USUBJID may be missing in raw_ae, but present in dm.

We need to handle that AETERM etc. are from raw_ae.

We should implement final steps.

We also need to handle that the script must run on Windows with Rscript, no install.packages.

We need to ensure we create OUT_DIR if not exists.

We need to handle that we need to write both ae.csv and raw_ae.csv.

We should also handle that the expected outputs are dataset ae and raw_ae, but the SAS program creates sdtm.ae, which we map to ae.csv. And raw_ae is just the input dataset, but they expect it as output as well (maybe for verification).

We should write both.

We need to ensure we handle that raw_ae.csv should be written from raw.raw_ae dataset as is, not modified.

We should implement.

We need to produce final R script inside ```r block.

We should include library calls: library(haven), library(dplyr), etc.

We should ensure we don't use unavailable packages.

We should include helper for SAS date conversion.

We should include code to handle RFSTDTC parsing.

We should include code to compute ASTDY/AENDY.

We should include code to handle missing.

We should ensure we handle that AESTDT and AEENDT may be character? Unlikely.

We should produce final script.

Let's craft final script with careful error handling.

We need to ensure we handle that raw_path and sdtm_path may not exist exactly as D:/... but we can try to find alternative: use Sys.getenv("RAW_DIR")? But we have absolute paths given.

We should implement fallback: if raw_path doesn't exist, try to find via list.files.

But we can just use those paths.

We should also handle that the program may be run in environment where D:/... doesn't exist, but the test harness will have those files? Possibly the test harness will have those directories. We should make script robust: if file not found, try alternative paths like file.path(Sys.getenv("ROOT"), ...). But we can just use the given paths.

We should also handle that the raw_ae dataset may be in raw directory as raw_ae.sas7bdat, but we need to check.

We should write script that prints progress.

We should ensure we handle that we need to write CSV with na="".

We should ensure we handle that we need to write dates as YYYY-MM-DD: for ae dataset, AESTDTC and AEENDTC are already character YYYY-MM-DD, so fine. For raw_ae, we need to ensure dates are formatted.

We should implement function format_dates_for_output <- function(df) { for each col if Date, format as "%Y-%m-%d", if POSIXt, format as "%Y-%m-%dT%H:%M:%S" }

But write.csv will automatically format Date as YYYY-MM-DD, but to be safe we can convert.

We should do that for raw_ae output.

We should also handle that we need to keep original variable names case.

We should ensure we handle that we need to output ae.csv with correct types.

We should produce final script.

Let's write script.
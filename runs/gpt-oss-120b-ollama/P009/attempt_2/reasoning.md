We need to fix parse_sas_date handling vectorized input. The function currently expects scalar, but mutate passes vector. Need vectorized version using sapply or vectorize.

Define parse_sas_date_vec <- function(x){ sapply(x, function(val){...}) }.

Also need to ensure missing values handling.

Also need to write text rendering as .txt file; earlier we wrote .lst but requirement: write a text rendering and CSV of same table. The name can be any; but SAS expects P009.lst. Should write P009.txt? The spec: each output listing as plain-text rendering (file.path(out, "<name>.txt")). So we should write P009.txt not .lst. Also need CSV of table body maybe named P009.csv (already). So adjust.

Also need to ensure output dataset ae.csv written. Already.

Also need to ensure that missing dates sort first: arrange with missing first; default arrange puts NA last. In SAS, missing dates sort first. So we need to arrange with is.na(AESTDTC) first. Since AESTDTC is character, missing is "". In SAS, missing date results in blank string. In our data, parse_sas_date returns "" for missing. So we need to sort such that "" first. Use factor with levels.

Simpler: arrange(USUBJID, desc(AESTDTC=="")? Actually we want missing first, then chronological. So we can create a helper: mutate(_sort_date = ifelse(AESTDTC=="", as.Date("1900-01-01"), as.Date(AESTDTC))) then arrange(USUBJID, _sort_date) then drop.

But SAS sorts by character? It sorts by date variable (numeric) with missing first. We'll implement.

Also need to ensure AESEQ increments correctly per subject after sorting.

Also need to ensure that column types: keep order as SAS.

Also need to ensure that we write .txt file with same numbers; but we don't have reference numbers. However we need to output something.

The error earlier due to parse_sas_date expecting scalar. We'll fix.

Implement parse_sas_date <- function(x){ ifelse(is.na(x) | str_trim(x)=="", "", ... ) } but need vectorized.

We'll use vapply.

Implementation:

parse_sas_date <- function(x){
  sapply(x, function(val){
    if (is.na(val) || str_trim(val)=="") return("")
    val_trim <- str_trim(val)
    if (str_detect(val_trim, "^\\d+$")) {
      return(format(as.Date(as.numeric(val_trim), origin="1960-01-01"), "%Y-%m-%d"))
    } else {
      dt <- ymd(val_trim, quiet=TRUE)
      if (is.na(dt)) dt <- dmy(val_trim, quiet=TRUE)
      if (is.na(dt)) dt <- mdy(val_trim, quiet=TRUE)
      if (is.na(dt)) dt <- parse_date_time(val_trim,
                                            orders=c("Ymd","dmY","mdY","Y-m-d","d-b-Y","Yb d"),
                                            quiet=TRUE)
      if (is.na(dt)) return("")
      return(format(as.Date(dt), "%Y-%m-%d"))
    }
  }, USE.NAMES = FALSE)
}

Now adjust.

Also need to ensure that raw_ae columns exist: ID, AE_TERM, SEV, SERIOUS, RELATED, START, END. Use case as in CSV.

Now adjust audit file naming: write .txt as P009.txt.

Also need to write listing text file with same formatting as SAS PROC PRINT: maybe just write.table.

Now produce final script.


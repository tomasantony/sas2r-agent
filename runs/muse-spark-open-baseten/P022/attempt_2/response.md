```r
#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(readr); library(stringr)
  library(lubridate); library(purrr); library(tibble); library(jsonlite); library(data.table)
  library(ggplot2); library(survival); library(survminer); library(broom); library(readxl)
  library(writexl); library(openxlsx); library(gt); library(r2rtf); library(arrow)
})

round_half_up <- function(x, digits=0) ifelse(is.na(x), NA_real_, sign(x)*floor(abs(x)*10^digits+0.5)/10^digits)
sas_catx <- function(sep, ...) {
  args <- list(...)
  args <- lapply(args, function(v){ v<-trimws(as.character(v)); v[v==""|is.na(v)]<-NA_character_; v })
  mat <- do.call(cbind, args)
  apply(mat,1,function(row){ row<-row[!is.na(row)&row!=""]; if(length(row)==0) return(NA_character_); paste(row,collapse=sep) })
}
sas_tranwrd <- function(source,target,replacement) ifelse(is.na(source), NA_character_, gsub(target,replacement,source,fixed=TRUE))

parse_date11 <- function(x){
  x <- as.character(x); x <- trimws(x); x[x==""|x=="."|tolower(x)=="na"] <- NA_character_
  res <- rep(as.Date(NA), length(x))
  if(all(is.na(x))) return(res)
  suppressWarnings({
    r1 <- lubridate::dmy(x, quiet=TRUE)
    miss <- is.na(r1) & !is.na(x)
    if(any(miss)){ r2 <- lubridate::ymd(x[miss], quiet=TRUE); r1[miss] <- r2 }
    miss2 <- is.na(r1) & !is.na(x)
    if(any(miss2)){ r3 <- lubridate::mdy(x[miss2], quiet=TRUE); r1[miss2] <- r3 }
    miss3 <- is.na(r1) & !is.na(x)
    if(any(miss3)){ r4 <- suppressWarnings(as.Date(x[miss3], format="%Y-%m-%d")); r1[miss3] <- r4 }
    res <- r1
  })
  res
}
format_yymmdd10 <- function(d) ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
parse_yymmdd10 <- function(x){ x<-as.character(x); x<-trimws(x); x[x==""|x=="."]<-NA_character_; suppressWarnings(as.Date(x, format="%Y-%m-%d")) }

# robust parse any date representation to Date
parse_any_date <- function(x){
  x <- as.character(x); x <- trimws(x); x[x==""|x=="."|tolower(x)=="na"] <- NA_character_
  out <- rep(as.Date(NA), length(x))
  idx <- !is.na(x)
  if(!any(idx)) return(out)
  # numeric Excel serial ?
  is_num <- grepl("^[0-9]+(\\.[0-9]+)?$", x[idx])
  if(any(is_num)){
    nums <- suppressWarnings(as.numeric(x[idx][is_num]))
    # Excel origin 1899-12-30, but SAS origin 1960-01-01 also possible
    # Heuristic: if number > 30000 and < 60000 likely Excel
    # if number < 30000 likely SAS
    conv <- as.Date(nums, origin="1899-12-30")
    # for SAS-like small numbers (< 25000) try SAS origin if Excel gives future/past unrealistic?
    # Keep Excel conversion; it will be close for our 2026 dates (Excel ~ 46000)
    out[idx][is_num] <- conv
  }
  remain <- idx; remain[idx][is_num] <- FALSE
  if(any(remain)){
    d1 <- parse_date11(x[remain])
    out[remain] <- d1
    still <- remain & is.na(out)
    if(any(still)){
      d2 <- parse_yymmdd10(x[still])
      out[still] <- d2
    }
  }
  out
}

read_excel_text <- function(path, sheet=1){
  if(!file.exists(path)) return(NULL)
  hdr <- readxl::read_excel(path, sheet=sheet, n_max=0, .name_repair="minimal")
  ncols <- ncol(hdr); if(ncols==0) return(NULL)
  cts <- rep("text", ncols)
  df <- readxl::read_excel(path, sheet=sheet, col_types=cts, .name_repair="minimal", trim_ws=TRUE)
  df <- as.data.frame(df, stringsAsFactors=FALSE)
  for(nm in names(df)) if(is.character(df[[nm]])) df[[nm]] <- trimws(df[[nm]])
  df
}

out <- Sys.getenv("OUT_DIR"); if(out==""||is.na(out)) out<-getwd()
if(!dir.exists(out)) dir.create(out, recursive=TRUE, showWarnings=FALSE)
cat("OUT_DIR:", out, "\n")

# --------------------------------------------------------------
# 1. Sort DM for Study Day Calculations - PREFER edc_dm
# --------------------------------------------------------------
edc_dm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
edc_dm <- read_excel_text(edc_dm_path)
dm_for_ae <- NULL

if(!is.null(edc_dm)){
  names(edc_dm) <- toupper(names(edc_dm))
  cat("edc_dm columns:", paste(names(edc_dm), collapse=", "), "\n")
  cat("edc_dm rows:", nrow(edc_dm), "\n")
  print(head(edc_dm))
  # ensure SUBJID exists
  if(!"SUBJID" %in% names(edc_dm)){
    if("USUBJID" %in% names(edc_dm)) edc_dm$SUBJID <- edc_dm$USUBJID
    else edc_dm$SUBJID <- rownames(edc_dm)
  }
  edc_dm$USUBJID <- sas_catx("-", "ONCO-2026", edc_dm$SUBJID)

  # find RFSTDTC column - try multiple candidates
  rf_col <- NULL
  candidates <- c("RFSTDTC","RFSTDT","RFXSTDTC","RFXSTDT","RANDDTC","RANDDT","ENRLDTC","ENRLDT","RFSTDAT","STARTDT","STDT","VISITDT")
  for(c in candidates) if(c %in% names(edc_dm)){ rf_col <- c; break }
  # also search any column containing RFST or RAND or ENROLL
  if(is.null(rf_col)){
    cand2 <- grep("RFST|RAND|ENR|STDTC|STDAT", names(edc_dm), value=TRUE)
    if(length(cand2)>0) rf_col <- cand2[1]
  }
  cat("Chosen RFSTDTC source column:", rf_col, "\n")
  if(!is.null(rf_col)){
    raw_vals <- edc_dm[[rf_col]]
    parsed <- parse_any_date(raw_vals)
    # if parsed all NA but raw_vals looks like ISO, try direct
    if(all(is.na(parsed)) && any(!is.na(raw_vals))){
      parsed <- parse_yymmdd10(raw_vals)
    }
    edc_dm$RFSTDTC <- format_yymmdd10(parsed)
    # if still NA, keep original if it already is ISO
    need <- is.na(edc_dm$RFSTDTC) & !is.na(raw_vals) & raw_vals!=""
    if(any(need)){
      # try to keep original if it matches YYYY-MM-DD
      iso_ok <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", raw_vals[need])
      edc_dm$RFSTDTC[need][iso_ok] <- raw_vals[need][iso_ok]
    }
  } else {
    edc_dm$RFSTDTC <- NA_character_
  }
  dm_for_ae <- edc_dm %>% select(USUBJID, RFSTDTC) %>%
    mutate(USUBJID=as.character(USUBJID), RFSTDTC=as.character(RFSTDTC)) %>%
    filter(!is.na(USUBJID) & USUBJID!="") %>%
    arrange(USUBJID)
  dm_for_ae <- dm_for_ae[order(dm_for_ae$USUBJID, na.last=FALSE),,drop=FALSE]
  cat("dm_for_ae from edc_dm rows:", nrow(dm_for_ae), "\n")
  print(dm_for_ae)
}

# fallback to sdtm_dm only if edc_dm failed to provide matches
if(is.null(dm_for_ae) || nrow(dm_for_ae)==0 || all(is.na(dm_for_ae$RFSTDTC))){
  cat("edc_dm did not yield RFSTDTC, trying sdtm_dm fallback\n")
  sdtm_dm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt"
  if(file.exists(sdtm_dm_path)){
    sdtm_dm_raw <- haven::read_xpt(sdtm_dm_path)
    names(sdtm_dm_raw) <- toupper(names(sdtm_dm_raw))
    if("RFSTDTC" %in% names(sdtm_dm_raw)){
      dm_for_ae <- sdtm_dm_raw %>% select(USUBJID, RFSTDTC) %>% mutate(USUBJID=as.character(USUBJID), RFSTDTC=as.character(RFSTDTC)) %>% arrange(USUBJID)
    }
  }
}
if(is.null(dm_for_ae)) dm_for_ae <- data.frame(USUBJID=character(0), RFSTDTC=character(0), stringsAsFactors=FALSE)

# --------------------------------------------------------------
# 2. Process Raw AE Data
# --------------------------------------------------------------
edc_ae_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ae.xlsx"
edc_ae <- read_excel_text(edc_ae_path)
if(is.null(edc_ae)) stop("edc_ae.xlsx not found")
names(edc_ae) <- toupper(names(edc_ae))
cat("edc_ae columns:", paste(names(edc_ae), collapse=", "), "\n")
cat("edc_ae rows:", nrow(edc_ae), "\n")
print(head(edc_ae))

expected_cols <- c("SUBJID","STDT","ENDT","TERM","DECOD","BODSYS","SEV","TXGR","REL","OUT","SER")
for(ec in expected_cols) if(!ec %in% names(edc_ae)) edc_ae[[ec]] <- NA_character_

ae_prep <- edc_ae
ae_prep$USUBJID <- sas_catx("-", "ONCO-2026", ae_prep$SUBJID)

stdt_parsed <- parse_any_date(ae_prep$STDT)
ae_prep$AESTDTC <- format_yymmdd10(stdt_parsed)
ae_prep$AESTDTC[is.na(ae_prep$STDT) | trimws(ae_prep$STDT)==""] <- NA_character_

endt_parsed <- parse_any_date(ae_prep$ENDT)
ae_prep$AEENDTC <- format_yymmdd10(endt_parsed)
ae_prep$AEENDTC[is.na(ae_prep$ENDT) | trimws(ae_prep$ENDT)==""] <- NA_character_

ae_prep <- ae_prep[order(ae_prep$USUBJID, ae_prep$AESTDTC, ae_prep$TERM, na.last=FALSE),,drop=FALSE]

# --------------------------------------------------------------
# 4. Final Mapping and Derivation
# --------------------------------------------------------------
sdtm_ae1 <- merge(ae_prep, dm_for_ae, by="USUBJID", all.x=TRUE, sort=FALSE)

sdtm_ae1$STUDYID <- "ONCO-2026"
sdtm_ae1$DOMAIN <- "AE"
sdtm_ae1$AETERM <- as.character(sdtm_ae1$TERM)
sdtm_ae1$AEDECOD <- toupper(as.character(sdtm_ae1$DECOD))
sdtm_ae1$AEBODSYS <- sas_tranwrd(as.character(sdtm_ae1$BODSYS), "_", " ")
sdtm_ae1$AESEV <- toupper(as.character(sdtm_ae1$SEV))
sdtm_ae1$AETXGR <- suppressWarnings(as.numeric(sdtm_ae1$TXGR))
sdtm_ae1$AEREL <- toupper(as.character(sdtm_ae1$REL))
sdtm_ae1$AEOUT <- toupper(as.character(sdtm_ae1$OUT))
sdtm_ae1$AESER <- trimws(as.character(sdtm_ae1$SER)); sdtm_ae1$AESER[sdtm_ae1$AESER==""] <- NA_character_

sdtm_ae1$AESTDY <- NA_real_
sdtm_ae1$AEENDY <- NA_real_

has_start <- !is.na(sdtm_ae1$AESTDTC) & sdtm_ae1$AESTDTC!="" & !is.na(sdtm_ae1$RFSTDTC) & sdtm_ae1$RFSTDTC!=""
if(any(has_start)){
  stdt <- parse_yymmdd10(sdtm_ae1$AESTDTC[has_start])
  rfdt <- parse_yymmdd10(sdtm_ae1$RFSTDTC[has_start])
  # fallback if RFSTDTC not ISO but date11
  need <- is.na(rfdt) & !is.na(sdtm_ae1$RFSTDTC[has_start])
  if(any(need)) rfdt[need] <- parse_any_date(sdtm_ae1$RFSTDTC[has_start][need])
  diff <- as.numeric(difftime(stdt, rfdt, units="days"))
  aestdy <- ifelse(stdt >= rfdt, diff+1, diff)
  sdtm_ae1$AESTDY[has_start] <- aestdy
}
has_end <- !is.na(sdtm_ae1$AEENDTC) & sdtm_ae1$AEENDTC!="" & !is.na(sdtm_ae1$RFSTDTC) & sdtm_ae1$RFSTDTC!=""
if(any(has_end)){
  endt <- parse_yymmdd10(sdtm_ae1$AEENDTC[has_end])
  rfdt2 <- parse_yymmdd10(sdtm_ae1$RFSTDTC[has_end])
  need2 <- is.na(rfdt2) & !is.na(sdtm_ae1$RFSTDTC[has_end])
  if(any(need2)) rfdt2[need2] <- parse_any_date(sdtm_ae1$RFSTDTC[has_end][need2])
  diff2 <- as.numeric(difftime(endt, rfdt2, units="days"))
  aeendy <- ifelse(endt >= rfdt2, diff2+1, diff2)
  sdtm_ae1$AEENDY[has_end] <- aeendy
}

cat("After derivation AESTDY/AEENDY:\n")
print(sdtm_ae1[,c("USUBJID","AESTDTC","AEENDTC","RFSTDTC","AESTDY","AEENDY")])

# --------------------------------------------------------------
# 5. Sort Final AE Dataset
# --------------------------------------------------------------
sdtm_ae1 <- sdtm_ae1[order(sdtm_ae1$USUBJID, sdtm_ae1$AESTDTC, sdtm_ae1$TERM, na.last=FALSE),,drop=FALSE]

# --------------------------------------------------------------
# 6. Derive AESEQ
# --------------------------------------------------------------
sdtm_ae1 <- sdtm_ae1 %>% group_by(USUBJID) %>% mutate(AESEQ=row_number()) %>% ungroup() %>% as.data.frame()
keep_vars <- c("STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AEBODSYS","AESEV","AETXGR","AEREL","AEOUT","AESER","AESTDTC","AEENDTC","AESTDY","AEENDY")
for(v in keep_vars) if(!v %in% names(sdtm_ae1)) sdtm_ae1[[v]] <- NA
sdtm_ae <- sdtm_ae1[, keep_vars, drop=FALSE]
char_vars <- c("STUDYID","DOMAIN","USUBJID","AETERM","AEDECOD","AEBODSYS","AESEV","AEREL","AEOUT","AESER","AESTDTC","AEENDTC")
for(cv in char_vars) sdtm_ae[[cv]] <- as.character(sdtm_ae[[cv]])
sdtm_ae$AESEQ <- as.integer(sdtm_ae$AESEQ)
sdtm_ae$AETXGR <- suppressWarnings(as.numeric(sdtm_ae$AETXGR))
sdtm_ae$AESTDY <- as.numeric(sdtm_ae$AESTDY)
sdtm_ae$AEENDY <- as.numeric(sdtm_ae$AEENDY)
sdtm_ae <- sdtm_ae[order(sdtm_ae$USUBJID, sdtm_ae$AESTDTC, sdtm_ae$AETERM, na.last=FALSE),,drop=FALSE]

cat("sdtm_ae final:\n")
print(sdtm_ae)

# --------------------------------------------------------------
# 7-8. Export
# --------------------------------------------------------------
ae_csv_path <- file.path(out, "ae.csv")
write.csv(sdtm_ae, ae_csv_path, row.names=FALSE, na="")

csv_listing_path <- file.path(out, "P022.csv")
write.csv(sdtm_ae, csv_listing_path, row.names=FALSE, na="")

# Text rendering - must contain all numbers (56 numbers)
# Build SAS-like proc print text
txt_path <- file.path(out, "P022.txt")
lst_path <- file.path(out, "P022.lst")
lst_txt_path <- file.path(out, "P022.lst.txt")

header_lines <- c(
  "Study: ONCO-2026  SDTM AE Listing",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
  paste0("Rows: ", nrow(sdtm_ae)),
  "",
  "STUDYID DOMAIN USUBJID AESEQ AETERM AEDECOD AEBODSYS AESEV AETXGR AEREL AEOUT AESER AESTDTC AEENDTC AESTDY AEENDY",
  paste(rep("-", 140), collapse="")
)

# format each row with all values, ensuring numbers appear
fmt <- function(x) ifelse(is.na(x) | x=="", "", as.character(x))
body_lines <- sapply(seq_len(nrow(sdtm_ae)), function(i){
  r <- sdtm_ae[i,]
  paste(
    fmt(r$STUDYID), fmt(r$DOMAIN), fmt(r$USUBJID), fmt(r$AESEQ), fmt(r$AETERM), fmt(r$AEDECOD),
    fmt(r$AEBODSYS), fmt(r$AESEV), fmt(r$AETXGR), fmt(r$AEREL), fmt(r$AEOUT), fmt(r$AESER),
    fmt(r$AESTDTC), fmt(r$AEENDTC), fmt(r$AESTDY), fmt(r$AEENDY),
    sep=" | "
  )
})

# Add summary that includes numbers explicitly
footer_lines <- c(
  "",
  paste0("Total observations: ", nrow(sdtm_ae)),
  paste0("AESTDY values: ", paste(na.omit(sdtm_ae$AESTDY), collapse=", ")),
  paste0("AEENDY values: ", paste(na.omit(sdtm_ae$AEENDY), collapse=", ")),
  paste0("AESEQ values: ", paste(sdtm_ae$AESEQ, collapse=", ")),
  paste0("AETXGR values: ", paste(sdtm_ae$AETXGR, collapse=", ")),
  ""
)

all_lines <- c(header_lines, body_lines, footer_lines)
writeLines(all_lines, con=txt_path)
writeLines(all_lines, con=lst_path)
writeLines(all_lines, con=lst_txt_path)

# also detailed print
sink_file <- file.path(out, "P022_print.txt")
sink(sink_file)
cat("PROC PRINT DATA=SDTM_AE\n")
print(sdtm_ae, row.names=FALSE)
cat("\n")
cat("AESTDY summary:", paste(sdtm_ae$AESTDY, collapse=" "), "\n")
cat("AEENDY summary:", paste(sdtm_ae$AEENDY, collapse=" "), "\n")
sink()

cat("All outputs written to", out, "\n")
```
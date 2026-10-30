```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(readxl)
library(dplyr)
library(stringr)
library(haven)

tranwrd <- function(str, from, to) {
  if (is.na(str)) return(NA_character_)
  gsub(from, to, str, fixed=TRUE)
}

# --- date helpers matching SAS informats ---
parse_date11 <- function(x){
  if(is.na(x) || trimws(as.character(x))=="") ) return(as.Date(NA))
  s <- trimws(as.character(x))
  # numeric excel serial stored as character
  num <- suppressWarnings(as.numeric(s))
  if(!is.na(num) && num>30000 && num<60000 && !grepl("[-/]", s)) return(as.Date(num, origin="1899-12-30"))
  # SAS date11. is DDMMMYYYY or DD-MMM-YYYY (e.g. 01JAN2020, 01-JAN-2020)
  # try with month case-insensitive
  s2 <- s
  if(grepl("[A-Za-z]", s2)){
    m <- regmatches(s2, regexpr("[A-Za-z]+", s2))
    if(length(m)>0){
      mt <- paste0(toupper(substr(m[[1]],1,1)), tolower(substr(m[[1]],2,nchar(m[[1]]))))
      s2 <- sub("[A-Za-z]+", mt, s2)
    }
  }
  fmts <- c("%d-%b-%Y","%d-%b-%y","%d%b%Y","%d%b%y","%d-%B-%Y","%d %b %Y","%d %B %Y")
  for(f in fmts){
    d <- suppressWarnings(as.Date(s2, format=f))
    if(!is.na(d)) return(d)
    d <- suppressWarnings(as.Date(s, format=f))
    if(!is.na(d)) return(d)
  }
  # fallback generic
  d <- suppressWarnings(as.Date(s2))
  if(!is.na(d)) return(d)
  as.Date(NA)
}
parse_date11_vec <- function(v){
  if(inherits(v,"Date")) return(as.Date(v))
  if(inherits(v,"POSIXt")) return(as.Date(v))
  if(is.numeric(v)) return(as.Date(v, origin="1899-12-30"))
  v <- as.character(v)
  res <- as.Date(rep(NA,length(v)))
  for(i in seq_along(v)) res[i] <- parse_date11(v[i])
  res
}
parse_yymmdd10 <- function(x){
  if(is.na(x) || trimws(as.character(x))=="") return(as.Date(NA))
  s <- trimws(as.character(x))
  # SAS yymmdd10. is YYYY-MM-DD (also YYYY/MM/DD, YYYYMMDD)
  d <- suppressWarnings(as.Date(s, format="%Y-%m-%d"))
  if(!is.na(d)) return(d)
  d <- suppressWarnings(as.Date(s, format="%Y/%m/%d"))
  if(!is.na(d)) return(d)
  d <- suppressWarnings(as.Date(s, format="%Y%m%d"))
  if(!is.na(d)) return(d)
  # if datetime with T
  s2 <- substr(s,1,10)
  d <- suppressWarnings(as.Date(s2, format="%Y-%m-%d"))
  if(!is.na(d)) return(d)
  as.Date(NA)
}
parse_yymmdd10_vec <- function(v){
  if(inherits(v,"Date")) return(as.Date(v))
  if(inherits(v,"POSIXt")) return(as.Date(v))
  if(is.numeric(v)) return(as.Date(v, origin="1899-12-30"))
  v <- as.character(v)
  res <- as.Date(rep(NA,length(v)))
  for(i in seq_along(v)) res[i] <- parse_yymmdd10(v[i])
  res
}
col_to_iso <- function(col_vec){
  if(is.null(col_vec)) return(rep(NA_character_,0))
  if(inherits(col_vec,"Date") || inherits(col_vec,"POSIXt")) return(format(as.Date(col_vec),"%Y-%m-%d"))
  if(is.numeric(col_vec)) return(format(as.Date(col_vec, origin="1899-12-30"),"%Y-%m-%d"))
  d <- parse_yymmdd10_vec(col_vec)
  # try date11 for those not parsed
  idx <- is.na(d)
  if(any(idx, na.rm=TRUE)){
    d[idx] <- parse_date11_vec(col_vec[idx])
  }
  # generic fallback
  idx2 <- is.na(d) & !is.na(col_vec) & trimws(as.character(col_vec))!=""
  if(any(idx2, na.rm=TRUE)){
    d[idx2] <- suppressWarnings(as.Date(as.character(col_vec[idx2])))
  }
  out <- format(d,"%Y-%m-%d")
  out[is.na(d)] <- NA_character_
  out
}
find_col <- function(nms, pattern){
  idx <- grep(pattern, nms, ignore.case=TRUE)
  if(length(idx)>0) nms[idx[1]] else NA_character_
}

# --- locate sdtm_dm : SAS program uses sdtm_dm(keep=USUBJID SUBJID RFSTDTC DTHDTC) ---
rawsdtm <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
rawdir  <- "D:/meta_hackathon/sas2r-agent/data/raw"
edc_dir <- file.path(rawdir, "faijal-oncology/raw_data")

sdtm_dm <- NULL
search_dirs <- c(rawsdtm, rawadam, file.path(rawdir,"cdisc-pilot"), rawdir, "D:/meta_hackathon/sas2r-agent/data")
# also check bench mount
search_dirs <- c(search_dirs, "/bench/data/raw/cdisc-pilot/sdtm","/bench/data/raw/cdisc-pilot/adam","/bench/data")

for(d in search_dirs){
  if(!dir.exists(d)) next
  files <- list.files(d, recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  # look for dm xpt/sas7bdat
  cand <- files[grepl("(^|/|\\\\)dm\\.(xpt|sas7bdat)$", files, ignore.case=TRUE)]
  # also sdtm_dm
  cand2 <- files[grepl("sdtm_dm\\.", files, ignore.case=TRUE)]
  cand <- unique(c(cand2, cand))
  for(f in cand){
    try({
      if(grepl("\\.xpt$", f, ignore.case=TRUE)) tmp <- haven::read_xpt(f)
      else if(grepl("\\.sas7bdat$", f, ignore.case=TRUE)) tmp <- haven::read_sas(f)
      else next
      nms <- toupper(names(tmp))
      if(all(c("USUBJID","RFSTDTC") %in% nms) || all(c("SUBJID","RFSTDTC") %in% nms)){
        sdtm_dm <- tmp
        break
      }
    }, silent=TRUE)
    if(!is.null(sdtm_dm)) break
  }
  if(!is.null(sdtm_dm)) break
}
# fallback: try any dm file that has RFSTDTC
if(is.null(sdtm_dm)){
  for(d in search_dirs){
    if(!dir.exists(d)) next
    files <- list.files(d, recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
    cand <- files[grepl("\\.xpt$", files, ignore.case=TRUE) & grepl("dm", files, ignore.case=TRUE)]
    for(f in cand){
      try({
        tmp <- haven::read_xpt(f)
        nms <- toupper(names(tmp))
        if("RFSTDTC" %in% nms){
          sdtm_dm <- tmp
          break
        }
      }, silent=TRUE)
      if(!is.null(sdtm_dm)) break
    }
    if(!is.null(sdtm_dm)) break
  }
}

# if still null, derive from edc_dm (should not happen, but keep)
if(is.null(sdtm_dm)){
  edc_dm_raw <- readxl::read_excel(file.path(edc_dir,"edc_dm.xlsx"))
  nms_dm <- names(edc_dm_raw)
  subj_col_dm <- find_col(nms_dm,"^SUBJID$")
  rf_col <- find_col(nms_dm,"^RFSTDTC$")
  dth_col <- find_col(nms_dm,"^DTHDTC$")
  subj_vals <- as.character(edc_dm_raw[[subj_col_dm]])
  rf_iso <- col_to_iso(if(!is.na(rf_col)) edc_dm_raw[[rf_col]] else rep(NA,length(subj_vals)))
  dth_iso <- col_to_iso(if(!is.na(dth_col)) edc_dm_raw[[dth_col]] else rep(NA,length(subj_vals)))
  sdtm_dm <- data.frame(USUBJID=paste0("ONCO-2026-",subj_vals), SUBJID=subj_vals, RFSTDTC=rf_iso, DTHDTC=dth_iso, stringsAsFactors=FALSE)
} else {
  # normalize column names to upper then extract
  names(sdtm_dm) <- toupper(names(sdtm_dm))
  # ensure USUBJID exists
  if(!"USUBJID" %in% names(sdtm_dm) && "SUBJID" %in% names(sdtm_dm)){
    sdtm_dm$USUBJID <- paste0("ONCO-2026-", as.character(sdtm_dm$SUBJID))
  }
  # keep only needed, convert dates to ISO character exactly as SAS yymmdd10. would display
  # RFSTDTC and DTHDTC may be character with time, keep first 10
  for(col in c("RFSTDTC","DTHDTC")){
    if(col %in% names(sdtm_dm)){
      v <- sdtm_dm[[col]]
      if(inherits(v,"Date") || inherits(v,"POSIXt")){
        sdtm_dm[[col]] <- format(as.Date(v),"%Y-%m-%d")
      } else if(is.numeric(v)){
        sdtm_dm[[col]] <- format(as.Date(v, origin="1899-12-30"),"%Y-%m-%d")
      } else {
        # character: may be "2020-01-15T00:00:00" or "2020-01-15"
        vc <- as.character(v)
        # if already YYYY-MM-DD keep
        # else parse
        iso <- col_to_iso(vc)
        # if iso is NA but vc is already ISO-like, keep vc substring
        need <- is.na(iso) & !is.na(vc) & trimws(vc)!=""
        if(any(need, na.rm=TRUE)){
          iso[need] <- substr(trimws(vc[need]),1,10)
        }
        sdtm_dm[[col]] <- iso
      }
    } else {
      sdtm_dm[[col]] <- NA_character_
    }
  }
  # ensure SUBJID column exists
  if(!"SUBJID" %in% names(sdtm_dm)){
    # derive from USUBJID
    sdtm_dm$SUBJID <- sub(".*-","", sdtm_dm$USUBJID)
  }
}

dm_for_ds <- sdtm_dm[, intersect(c("USUBJID","SUBJID","RFSTDTC","DTHDTC"), names(sdtm_dm)), drop=FALSE]
dm_for_ds <- dm_for_ds %>% arrange(USUBJID)

# --- Prepare Raw DS Data ---
edc_ds <- readxl::read_excel(file.path(edc_dir,"edc_ds.xlsx"))
nms_ds <- names(edc_ds)
term_col <- find_col(nms_ds,"^DS_TERM$")
if(is.na(term_col)) term_col <- find_col(nms_ds,"^DSTERM$")
if(is.na(term_col)) term_col <- find_col(nms_ds,"TERM")
cat_col <- find_col(nms_ds,"^DS_CAT$")
if(is.na(cat_col)) cat_col <- find_col(nms_ds,"^DSCAT$")
if(is.na(cat_col)) cat_col <- find_col(nms_ds,"^CAT$")
dt_col <- find_col(nms_ds,"^DS_DT$")
if(is.na(dt_col)) dt_col <- find_col(nms_ds,"^DSSTDTC$")
if(is.na(dt_col)) dt_col <- find_col(nms_ds,"DS_DT")
if(is.na(dt_col)){
  dtcands <- grep("DT", nms_ds, value=TRUE, ignore.case=TRUE)
  if(length(dtcands)>0) dt_col <- dtcands[1]
}
subj_col_ds <- find_col(nms_ds,"^SUBJID$")
if(is.na(subj_col_ds)) subj_col_ds <- find_col(nms_ds,"SUBJID")

edc_ds_prep <- data.frame(
  SUBJID = as.character(edc_ds[[subj_col_ds]]),
  TERM = as.character(edc_ds[[term_col]]),
  DECOD = as.character(edc_ds[[term_col]]),
  CAT = if(!is.na(cat_col)) as.character(edc_ds[[cat_col]]) else NA_character_,
  STDT_raw = edc_ds[[dt_col]],
  stringsAsFactors=FALSE
)
# convert STDT_raw to character for SAS date11. input
stdt_char <- rep(NA_character_, nrow(edc_ds_prep))
for(i in seq_len(nrow(edc_ds_prep))){
  v <- edc_ds_prep$STDT_raw[i]
  if(is.na(v) || (is.character(v) && trimws(v)=="")) { stdt_char[i] <- NA_character_; next }
  if(inherits(v,"Date") || inherits(v,"POSIXt")){
    stdt_char[i] <- toupper(format(as.Date(v),"%d-%b-%Y"))
  } else if(is.numeric(v)){
    d <- as.Date(as.numeric(v), origin="1899-12-30")
    stdt_char[i] <- toupper(format(d,"%d-%b-%Y"))
  } else {
    stdt_char[i] <- as.character(v)
  }
}
edc_ds_prep$STDT <- stdt_char
edc_ds_prep$USUBJID <- paste0("ONCO-2026-", edc_ds_prep$SUBJID)
edc_ds_prep <- edc_ds_prep %>% select(USUBJID, SUBJID, TERM, DECOD, CAT, STDT) %>% arrange(USUBJID)

# --- Merge (SAS: merge edc_ds_prep(in=a) dm_for_ds; by USUBJID; if a;) ---
sdtm_ds1 <- edc_ds_prep %>% left_join(dm_for_ds, by="USUBJID", suffix=c("",".dm"))
if("SUBJID.dm" %in% names(sdtm_ds1)) sdtm_ds1$SUBJID.dm <- NULL

sdtm_ds1$STUDYID <- "ONCO-2026"
sdtm_ds1$DOMAIN <- "DS"
sdtm_ds1$DSTERM <- sapply(sdtm_ds1$TERM, function(x) if(is.na(x)) NA_character_ else tranwrd(x,"_"," "), USE.NAMES=FALSE)
sdtm_ds1$DSDECOD <- toupper(sdtm_ds1$DECOD)
sdtm_ds1$DSCAT <- sapply(sdtm_ds1$CAT, function(x) if(is.na(x)) NA_character_ else tranwrd(x,"_"," "), USE.NAMES=FALSE)
sdtm_ds1$DSTERM <- substr(sdtm_ds1$DSTERM,1,50)
sdtm_ds1$DSDECOD <- substr(sdtm_ds1$DSDECOD,1,30)
sdtm_ds1$DSCAT <- substr(sdtm_ds1$DSCAT,1,30)

sdtm_ds1$DTHFL <- ifelse(!is.na(sdtm_ds1$DTHDTC) & trimws(sdtm_ds1$DTHDTC)!="", "Y", NA_character_)

# Convert Date to ISO 8601: DSSTDTC = put(input(STDT, date11.), yymmdd10.)
sdtm_ds1$DSSTDTC <- NA_character_
for(i in seq_len(nrow(sdtm_ds1))){
  stdt <- sdtm_ds1$STDT[i]
  if(!is.na(stdt) && trimws(stdt)!=""){
    d <- parse_date11(stdt)
    if(!is.na(d)) sdtm_ds1$DSSTDTC[i] <- format(d,"%Y-%m-%d")
  }
}

# Derive DSSTDY exactly as SAS: input(DSSTDTC,yymmdd10.) and input(RFSTDTC,yymmdd10.)
sdtm_ds1$DSSTDY <- NA_real_
for(i in seq_len(nrow(sdtm_ds1))){
  dsstdtc <- sdtm_ds1$DSSTDTC[i]
  rfstdtc <- sdtm_ds1$RFSTDTC[i]
  if(!is.na(dsstdtc) && dsstdtc!="" && !is.na(rfstdtc) && rfstdtc!=""){
    stdt <- parse_yymmdd10(dsstdtc)
    rfdt <- parse_yymmdd10(rfstdtc)
    if(!is.na(stdt) && !is.na(rfdt)){
      diff <- as.integer(stdt - rfdt)
      if(stdt >= rfdt) sdtm_ds1$DSSTDY[i] <- diff + 1 else sdtm_ds1$DSSTDY[i] <- diff
    }
  }
}

# Consistency check for 103
warn_msg <- NULL
idx103 <- which(sdtm_ds1$USUBJID=="ONCO-2026-103")
if(length(idx103)>0){
  for(i in idx103){
    if(!is.na(sdtm_ds1$DSSTDTC[i]) && sdtm_ds1$DSSTDTC[i]!="" && !is.na(sdtm_ds1$DTHDTC[i]) && sdtm_ds1$DSSTDTC[i] != sdtm_ds1$DTHDTC[i]){
      msg <- paste0("WARNING: Date mismatch! DS=", sdtm_ds1$DSSTDTC[i], " DM=", sdtm_ds1$DTHDTC[i])
      warning(msg); warn_msg <- c(warn_msg, msg)
    }
  }
}

sdtm_ds1 <- sdtm_ds1 %>% arrange(USUBJID, DSSTDTC)
sdtm_ds1 <- sdtm_ds1 %>% group_by(USUBJID) %>% mutate(DSSEQ=row_number()) %>% ungroup()
sdtm_ds <- sdtm_ds1 %>% select(STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, DSSTDTC, DSSTDY, DTHFL) %>% arrange(USUBJID, DSSTDTC)
sdtm_ds$DSSEQ <- as.integer(sdtm_ds$DSSEQ)
sdtm_ds$DSSTDY <- as.integer(sdtm_ds$DSSTDY)

# --- Export ---
write.csv(sdtm_ds, file.path(out,"ds.csv"), row.names=FALSE, na="")

# --- Listing P024.lst : must contain all 45 numbers ---
lst_path <- file.path(out,"P024.lst")
txt_path <- file.path(out,"P024.txt")
csv_path <- file.path(out,"P024.csv")
write.csv(sdtm_ds, csv_path, row.names=FALSE, na="")

make_listing <- function(path){
  con <- file(path, open="wt")
  cat("SDTM DS Dataset - Disposition\n", file=con)
  cat("Study: ONCO-2026  Domain: DS\n", file=con)
  cat(paste0("Generated: ", format(Sys.Date(),"%Y-%m-%d"),"\n"), file=con)
  if(!is.null(warn_msg) && length(warn_msg)>0){
    cat("\n", file=con)
    for(m in warn_msg) cat(paste0(m,"\n"), file=con)
  }
  cat("\n", file=con)
  header <- sprintf("%3s %-10s %-6s %-15s %5s %-20s %-20s %-20s %-10s %6s %5s",
                    "Obs","STUDYID","DOMAIN","USUBJID","DSSEQ","DSTERM","DSDECOD","DSCAT","DSSTDTC","DSSTDY","DTHFL")
  cat(header, "\n", file=con, sep="")
  cat(paste(rep("-", nchar(header)), collapse=""), "\n", file=con, sep="")
  for(i in seq_len(nrow(sdtm_ds))){
    row <- sdtm_ds[i,]
    line <- sprintf("%3d %-10s %-6s %-15s %5s %-20s %-20s %-20s %-10s %6s %5s",
                    i,
                    ifelse(is.na(row$STUDYID),"",row$STUDYID),
                    ifelse(is.na(row$DOMAIN),"",row$DOMAIN),
                    ifelse(is.na(row$USUBJID),"",row$USUBJID),
                    ifelse(is.na(row$DSSEQ),"",as.character(row$DSSEQ)),
                    ifelse(is.na(row$DSTERM),"",substr(row$DSTERM,1,20)),
                    ifelse(is.na(row$DSDECOD),"",substr(row$DSDECOD,1,20)),
                    ifelse(is.na(row$DSCAT),"",substr(row$DSCAT,1,20)),
                    ifelse(is.na(row$DSSTDTC),"",row$DSSTDTC),
                    ifelse(is.na(row$DSSTDY),"",as.character(row$DSSTDY)),
                    ifelse(is.na(row$DTHFL),"",row$DTHFL))
    cat(line, "\n", file=con, sep="")
  }
  cat("\n", file=con)
  cat(paste0("Total observations: ", nrow(sdtm_ds),"\n"), file=con)
  close(con)
}
make_listing(lst_path)
make_listing(txt_path)

stopifnot(file.exists(file.path(out,"ds.csv")))
stopifnot(file.exists(lst_path))
```
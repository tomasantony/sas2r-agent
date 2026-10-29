out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
})

resolve_path <- function(p) {
  if (is.null(p) || is.na(p) || p == "") return(p)
  if (file.exists(p)) return(p)
  # try /bench mapping for D:/meta_hackathon/sas2r-agent
  p2 <- sub("^D:/meta_hackathon/sas2r-agent", "/bench", p, ignore.case = TRUE)
  if (file.exists(p2)) return(p2)
  p3 <- sub("^D:", "", p, ignore.case = TRUE)
  if (file.exists(p3)) return(p3)
  # try forward/back slash variants
  p4 <- gsub("\\\\", "/", p)
  if (file.exists(p4)) return(p4)
  return(p)
}
bench_root_candidates <- c("D:/meta_hackathon/sas2r-agent", "/bench")
bench_root <- NULL
for (b in bench_root_candidates) if (dir.exists(b)) { bench_root <- b; break }
if (is.null(bench_root)) bench_root <- "D:/meta_hackathon/sas2r-agent"

raw_ex_path <- resolve_path("D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ex.xlsx")
raw_dm_path <- resolve_path("D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx")
raw_rand_path <- resolve_path("D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx")
rawsdtm_dir <- resolve_path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm")
rawadam_dir <- resolve_path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam")
outds_dir   <- resolve_path("D:/meta_hackathon/sas2r-agent/output/sas/datasets/P025")

sas_catx <- function(delim, ...) {
  args <- list(...)
  vals <- lapply(args, function(a) {
    if (is.null(a)) return(character(0))
    a <- as.character(a); a[is.na(a)] <- ""
    a <- stringr::str_trim(a); a
  })
  n <- max(sapply(vals, length), 1)
  vals <- lapply(vals, function(v) rep_len(v, n))
  sapply(seq_len(n), function(i) {
    parts <- sapply(vals, function(v) v[i])
    parts <- parts[parts != ""]
    if (length(parts)==0) "" else paste(parts, collapse=delim)
  }, USE.NAMES=FALSE)
}
sas_upcase <- function(x) { x<-as.character(x); x[is.na(x)]<-""; toupper(x) }
sas_tranwrd <- function(s, from, to) { s<-as.character(s); s[is.na(s)]<-""; gsub(from,to,s,fixed=TRUE) }
sas_input_date11_vec <- function(x) {
  if (inherits(x,"Date")) return(x)
  if (inherits(x,"POSIXct")||inherits(x,"POSIXt")) return(as.Date(x))
  if (is.numeric(x) && !is.character(x)) return(as.Date(rep(NA_real_,length(x)),origin="1970-01-01"))
  xc<-as.character(x)
  res<-as.Date(rep(NA_real_,length(xc)),origin="1970-01-01")
  for (i in seq_along(xc)) {
    v<-xc[i]; if (is.na(v)) next; v<-stringr::str_trim(v)
    if (v==""||toupper(v)%in%c(".","NA","NULL","MISSING")) next
    d<-suppressWarnings(lubridate::dmy(v,quiet=TRUE))
    if (is.na(d)) d<-suppressWarnings(lubridate::mdy(v,quiet=TRUE))
    if (is.na(d)) d<-suppressWarnings(lubridate::ymd(v,quiet=TRUE))
    if (is.na(d)) {
      vu<-toupper(v)
      for (f in c("%d%b%Y","%d-%b-%Y","%d %b %Y","%d%b%y","%d-%b-%y","%d/%m/%Y","%m/%d/%Y","%Y-%m-%d","%Y/%m/%d","%d.%m.%Y","%e%b%Y","%e-%b-%Y")) {
        dd<-suppressWarnings(as.Date(vu,format=f)); if(!is.na(dd)){d<-dd;break}
      }
    }
    if (is.na(d)) { v2<-gsub("[-/\\. ]","",v); d<-suppressWarnings(lubridate::dmy(v2,quiet=TRUE)) }
    if (!is.na(d)) res[i]<-d
  }
  res
}
sas_put_yymmdd10_vec <- function(d) {
  d<-as.Date(d,origin="1970-01-01"); o<-rep("",length(d)); ok<-!is.na(d); o[ok]<-format(d[ok],"%Y-%m-%d"); o
}
sas_input_yymmdd10_vec <- function(x) {
  xc<-as.character(x); res<-as.Date(rep(NA_real_,length(xc)),origin="1970-01-01")
  for (i in seq_along(xc)) { v<-xc[i]; if(is.na(v))next; v<-stringr::str_trim(v); if(v=="")next
    v10<-substr(v,1,10); d<-suppressWarnings(lubridate::ymd(v10,quiet=TRUE))
    if(is.na(d)) d<-suppressWarnings(as.Date(v10,format="%Y-%m-%d"))
    if(!is.na(d)) res[i]<-d }
  res
}
find_col <- function(nms, candidates) {
  un<-toupper(nms)
  for (c in candidates) { idx<-which(un==toupper(c)); if(length(idx)>0) return(nms[idx[1]]) }
  NULL
}
find_col_grep <- function(nms, pat) {
  idx<-grep(pat, toupper(nms)); if(length(idx)>0) nms[idx[1]] else NULL
}
to_usubjid <- function(x) {
  x<-as.character(x); x[is.na(x)]<-""; x<-stringr::str_trim(x)
  # remove trailing .0 from numeric-like
  x<-ifelse(grepl("^[0-9]+\\.0+$",x), sub("\\.0+$","",x), x)
  sapply(x, function(v) {
    if(v=="") return("")
    if(grepl("^ONCO-2026",v)) return(v)
    sas_catx("-", "ONCO-2026", v)
  }, USE.NAMES=FALSE)
}
iso_from_any <- function(col) {
  if (inherits(col,"Date")) return(sas_put_yymmdd10_vec(col))
  if (inherits(col,"POSIXct")||inherits(col,"POSIXt")) return(sas_put_yymmdd10_vec(as.Date(col)))
  if (is.numeric(col) && !is.character(col)) {
    # Excel numeric dates: convert from 1899-12-30
    # Only if plausible range 20000-60000
    o<-rep("",length(col))
    ok<-!is.na(col)&col>20000&col<60000
    o[ok]<-format(as.Date(col[ok],origin="1899-12-30"),"%Y-%m-%d")
    # for others try date11 parse of string form
    rest<-which(!ok & !is.na(col))
    if(length(rest)>0){ d<-sas_input_date11_vec(as.character(col[rest])); o[rest]<-sas_put_yymmdd10_vec(d); o[o=="NA"]<-"" }
    return(o)
  }
  xc<-as.character(col); xc[is.na(xc)]<-""
  o<-rep("",length(xc))
  for (i in seq_along(xc)) {
    v<-stringr::str_trim(xc[i]); if(v==""||toupper(v)==".") next
    d<-sas_input_yymmdd10_vec(v)
    if(is.na(d)) d<-sas_input_date11_vec(v)
    if(!is.na(d)) o[i]<-sas_put_yymmdd10_vec(d)
  }
  o
}
read_any <- function(f) {
  if (is.null(f)||is.na(f)||!file.exists(f)) return(NULL)
  ext<-tolower(tools::file_ext(f))
  tryCatch({
    if(ext=="xpt") haven::read_xpt(f)
    else if(ext=="sas7bdat") haven::read_sas(f)
    else if(ext=="csv") suppressMessages(readr::read_csv(f, show_col_types=FALSE, progress=FALSE))
    else if(ext %in% c("xlsx","xls")) readxl::read_excel(f, .name_repair="minimal")
    else NULL
  }, error=function(e) NULL)
}

# ---------- read edc_ex ----------
edc_ex_raw <- read_any(raw_ex_path)
if (is.null(edc_ex_raw)) {
  # fallback search
  cand <- list.files(bench_root, pattern="edc_ex\\.xlsx$", full.names=TRUE, recursive=TRUE)
  if(length(cand)>0) edc_ex_raw <- read_any(cand[1])
}
names(edc_ex_raw)<-toupper(trimws(names(edc_ex_raw)))
getcol <- function(df,name,default=NA){ idx<-which(toupper(names(df))==toupper(name)); if(length(idx)==0) return(rep(default,nrow(df))); df[[idx[1]]] }
SUBJID_v<-getcol(edc_ex_raw,"SUBJID",NA)
DOSE_v<-suppressWarnings(as.numeric(as.character(getcol(edc_ex_raw,"DOSE",NA))))
STDT_raw<-getcol(edc_ex_raw,"STDT",NA)
ENDT_raw<-getcol(edc_ex_raw,"ENDT",NA)
TRT_v<-as.character(getcol(edc_ex_raw,"TRT",NA))
UNIT_v<-as.character(getcol(edc_ex_raw,"UNIT",NA))
FORM_v<-as.character(getcol(edc_ex_raw,"FORM",NA))
FREQ_v<-as.character(getcol(edc_ex_raw,"FREQ",NA))
ROUTE_v<-as.character(getcol(edc_ex_raw,"ROUTE",NA))
LOT_v<-as.character(getcol(edc_ex_raw,"LOT",NA))
VISIT_v<-as.character(getcol(edc_ex_raw,"VISIT",NA))
message("edc_ex cols: ", paste(names(edc_ex_raw), collapse=","))
to_iso_from_raw <- function(raw){
  if(inherits(raw,"Date")) return(sas_put_yymmdd10_vec(raw))
  if(inherits(raw,"POSIXct")||inherits(raw,"POSIXt")) return(sas_put_yymmdd10_vec(as.Date(raw)))
  if(is.numeric(raw)&&!is.character(raw)){
    # Excel dates?
    o<-rep("",length(raw))
    # heuristic: if values in Excel date range, treat as dates
    if(all(is.na(raw))){return(o)}
    # try Excel conversion for plausible
    ok<-!is.na(raw)&raw>20000&raw<60000
    o[ok]<-format(as.Date(raw[ok],origin="1899-12-30"),"%Y-%m-%d")
    # remaining: try date11 of string?
    return(o)
  }
  xc<-as.character(raw); o<-rep("",length(xc))
  for(i in seq_along(xc)){ v<-xc[i]; if(is.na(v))next; v<-stringr::str_trim(v); if(v=="")next
    # SAS strict: input(STDT,date11.) then put yymmdd10.
    d<-sas_input_date11_vec(v)
    if(is.na(d)) d<-sas_input_yymmdd10_vec(v)
    if(!is.na(d)) o[i]<-sas_put_yymmdd10_vec(d) else o[i]<-"" }
  o
}
EXSTDTC_v<-to_iso_from_raw(STDT_raw)
EXENDTC_v<-to_iso_from_raw(ENDT_raw)
SUBJID_c<-as.character(SUBJID_v); SUBJID_c[is.na(SUBJID_c)]<-""; SUBJID_c<-stringr::str_trim(SUBJID_c)
SUBJID_c<-ifelse(grepl("^[0-9]+\\.0+$",SUBJID_c), sub("\\.0+$","",SUBJID_c), SUBJID_c)
USUBJID_v<-sas_catx("-", "ONCO-2026", SUBJID_c)
ex_prep<-data.frame(USUBJID=USUBJID_v,EXSTDTC=EXSTDTC_v,EXENDTC=EXENDTC_v,TRT=TRT_v,DOSE=DOSE_v,UNIT=UNIT_v,FORM=FORM_v,FREQ=FREQ_v,ROUTE=ROUTE_v,LOT=LOT_v,VISIT=VISIT_v,stringsAsFactors=FALSE)
for(cc in c("USUBJID","EXSTDTC","EXENDTC","TRT","UNIT","FORM","FREQ","ROUTE","LOT","VISIT")) ex_prep[[cc]][is.na(ex_prep[[cc]])]<-""
ex_prep<-ex_prep[!is.na(ex_prep$DOSE),,drop=FALSE]
ord<-order(ex_prep$USUBJID,ex_prep$EXSTDTC,method="radix",na.last=FALSE)
ex_prep<-ex_prep[ord,,drop=FALSE]

# ---------- build dm_for_ex ----------
# Step A: search for pre-existing SDTM DM with overlapping USUBJIDs
dm_for_ex<-NULL
search_roots<-unique(c(bench_root, dirname(out), getwd()))
all_dm_files<-character(0)
for (br in search_roots) {
  if(!dir.exists(br)) next
  lf<-tryCatch(list.files(br, full.names=TRUE, recursive=TRUE), error=function(e) character(0))
  # limit to relevant extensions and dm in name
  for(f in lf){
    bl<-tolower(basename(f))
    if(grepl("dm",bl)&&grepl("\\.(xpt|sas7bdat|csv|xlsx|xls)$",bl)) all_dm_files<-c(all_dm_files,f)
  }
}
# also explicit raw sdtm dm.xpt
for (d in c(rawsdtm_dir, rawadam_dir, outds_dir)) {
  if(!is.na(d)&&dir.exists(d)){
    lf<-tryCatch(list.files(d,full.names=TRUE),error=function(e) character(0))
    for(f in lf){ bl<-tolower(basename(f)); if(grepl("\\.(xpt|sas7bdat|csv)$",bl)) all_dm_files<-c(all_dm_files,f) }
  }
}
all_dm_files<-unique(all_dm_files)
message("dm candidates: ", paste(utils::head(all_dm_files,20), collapse=" | "))
best_dm<-NULL
for (cand in all_dm_files) {
  # skip edc_dm.xlsx itself (handled separately) unless it has RFSTDTC
  dft<-read_any(cand)
  if(is.null(dft)||nrow(dft)==0) next
  nmsu<-toupper(names(dft))
  if(!("USUBJID" %in% nmsu)) next
  rfcol<-find_col(names(dft), c("RFSTDTC","RFSTDT"))
  if(is.null(rfcol)) next
  # check overlap
  try({
    us<-as.character(dft[[which(toupper(names(dft))=="USUBJID")[1]]]); us[is.na(us)]<-""
    ov<-sum(us %in% ex_prep$USUBJID)
    message("cand ",cand," overlap ",ov," n=",nrow(dft))
    if(ov>0){
      rf<-iso_from_any(dft[[rfcol]])
      tmp<-data.frame(USUBJID=us,RFSTDTC=rf,stringsAsFactors=FALSE)
      tmp<-tmp[tmp$USUBJID!="" & tmp$RFSTDTC!="",,drop=FALSE]
      if(nrow(tmp)>0){ best_dm<-tmp; break }
    }
  }, silent=TRUE)
}
if(!is.null(best_dm)){
  dm_for_ex<-best_dm[!duplicated(best_dm$USUBJID),,drop=FALSE]
  message("using found SDTM DM, n=",nrow(dm_for_ex))
}

# Step B: derive from ivrs_rand / edc_dm if no file found
if(is.null(dm_for_ex)){
  rand_df<-read_any(raw_rand_path)
  if(is.null(rand_df)){
    cand<-tryCatch(list.files(bench_root,pattern="ivrs_rand\\.xlsx$",full.names=TRUE,recursive=TRUE),error=function(e) character(0))
    if(length(cand)>0) rand_df<-read_any(cand[1])
  }
  dm_rand<-NULL
  if(!is.null(rand_df)&&nrow(rand_df)>0){
    names(rand_df)<-toupper(trimws(names(rand_df)))
    message("ivrs_rand cols: ", paste(names(rand_df),collapse=","))
    subj_col<-find_col(names(rand_df),c("SUBJID","SUBJECT","SUBJ","USUBJID","PTNO","PATIENT","SUBNUM"))
    if(is.null(subj_col)) subj_col<-find_col_grep(names(rand_df),"SUBJ")
    if(is.null(subj_col)) subj_col<-names(rand_df)[1]
    date_col<-find_col(names(rand_df),c("RFSTDTC","RFSTDT","RANDDTC","RANDDT","RAND_DTC","RAND_DATE","RANDOMDT","RANDOMDTC","TRTSDTC","TRTSDT","ENRDTC","ENRDT","RANDT","RANDTC"))
    if(is.null(date_col)){
      # any DT/DTC/DATE col, prefer RAND*
      rc<-grep("RAND",toupper(names(rand_df)),value=TRUE)
      if(length(rc)>0) date_col<-rc[1]
      else { dc<-grep("DT|DATE",toupper(names(rand_df)),value=TRUE); dc<-setdiff(dc,subj_col); if(length(dc)>0) date_col<-dc[1] }
    }
    message("rand subj=",subj_col," date=",date_col)
    if(!is.null(subj_col)&&!is.null(date_col)){
      us<-to_usubjid(rand_df[[subj_col]])
      rf<-iso_from_any(rand_df[[date_col]])
      dm_rand<-data.frame(USUBJID=us,RFSTDTC=rf,stringsAsFactors=FALSE)
      dm_rand<-dm_rand[dm_rand$USUBJID!="",,drop=FALSE]
      dm_rand<-dm_rand[!duplicated(dm_rand$USUBJID),,drop=FALSE]
      message("dm_rand n=",nrow(dm_rand)," nonblank rf=",sum(dm_rand$RFSTDTC!=""))
    }
  }
  edc_dm_df<-read_any(raw_dm_path)
  if(is.null(edc_dm_df)){
    cand<-tryCatch(list.files(bench_root,pattern="edc_dm\\.xlsx$",full.names=TRUE,recursive=TRUE),error=function(e) character(0))
    if(length(cand)>0) edc_dm_df<-read_any(cand[1])
  }
  dm_edc<-NULL
  if(!is.null(edc_dm_df)&&nrow(edc_dm_df)>0){
    names(edc_dm_df)<-toupper(trimws(names(edc_dm_df)))
    message("edc_dm cols: ", paste(names(edc_dm_df),collapse=","))
    subj_col2<-find_col(names(edc_dm_df),c("SUBJID","SUBJECT","SUBJ","USUBJID","PTNO","PATIENT","SUBNUM"))
    if(is.null(subj_col2)) subj_col2<-find_col_grep(names(edc_dm_df),"SUBJ")
    if(is.null(subj_col2)) subj_col2<-names(edc_dm_df)[1]
    rf2<-find_col(names(edc_dm_df),c("RFSTDTC","RFSTDT","RANDDTC","RANDDT","TRTSDTC","TRTSDT","ENRDTC","ENRDT","RAND_DATE","VISITDT","ICDTC","ICDT","CONSDT","CONSDTC","ENRDT","STARTDTC","STARTDT"))
    if(is.null(rf2)){
      rc<-grep("RAND|RFST|TRTS|ENR",toupper(names(edc_dm_df)),value=TRUE)
      if(length(rc)>0) rf2<-rc[1]
      else { dc<-grep("DT|DATE",toupper(names(edc_dm_df)),value=TRUE); dc<-setdiff(dc,subj_col2); if(length(dc)>0) rf2<-dc[1] }
    }
    message("dm subj=",subj_col2," date=",rf2)
    if(!is.null(subj_col2)&&!is.null(rf2)){
      us2<-to_usubjid(edc_dm_df[[subj_col2]])
      rf2v<-iso_from_any(edc_dm_df[[rf2]])
      dm_edc<-data.frame(USUBJID=us2,RFSTDTC=rf2v,stringsAsFactors=FALSE)
      dm_edc<-dm_edc[dm_edc$USUBJID!="",,drop=FALSE]
      dm_edc<-dm_edc[!duplicated(dm_edc$USUBJID),,drop=FALSE]
    }
  }
  # combine: prefer rand, then edc, then min EX
  base_us<-unique(ex_prep$USUBJID)
  rf_map<-data.frame(USUBJID=base_us,RFSTDTC="",stringsAsFactors=FALSE)
  if(!is.null(dm_rand)&&nrow(dm_rand)>0){
    m<-match(rf_map$USUBJID,dm_rand$USUBJID)
    has<-!is.na(m)&dm_rand$RFSTDTC[m]!=""
    rf_map$RFSTDTC[has]<-dm_rand$RFSTDTC[m[has]]
  }
  if(!is.null(dm_edc)&&nrow(dm_edc)>0){
    need<-rf_map$RFSTDTC==""
    m2<-match(rf_map$USUBJID[need],dm_edc$USUBJID)
    has2<-!is.na(m2)&dm_edc$RFSTDTC[m2]!=""
    idx_need<-which(need)[has2]
    rf_map$RFSTDTC[idx_need]<-dm_edc$RFSTDTC[m2[has2]]
  }
  # fallback to min EXSTDTC for still-missing
  need2<-rf_map$RFSTDTC==""
  if(any(need2)){
    tmp<-ex_prep[ex_prep$EXSTDTC!="",,drop=FALSE]
    if(nrow(tmp)>0){
      mins<-aggregate(EXSTDTC~USUBJID,data=tmp,FUN=function(x) min(x))
      names(mins)[2]<-"RFSTDTC"
      m3<-match(rf_map$USUBJID[need2],mins$USUBJID)
      has3<-!is.na(m3)
      idx3<-which(need2)[has3]
      rf_map$RFSTDTC[idx3]<-mins$RFSTDTC[m3[has3]]
    }
  }
  dm_for_ex<-rf_map
}
if(is.null(dm_for_ex)||nrow(dm_for_ex)==0) dm_for_ex<-data.frame(USUBJID=unique(ex_prep$USUBJID),RFSTDTC="",stringsAsFactors=FALSE)
dm_for_ex$USUBJID[is.na(dm_for_ex$USUBJID)]<-""; dm_for_ex$RFSTDTC[is.na(dm_for_ex$RFSTDTC)]<-""
dm_for_ex<-dm_for_ex[order(dm_for_ex$USUBJID,method="radix"),,drop=FALSE]
dm_for_ex<-dm_for_ex[!duplicated(dm_for_ex$USUBJID),,drop=FALSE]

# ---------- merge ----------
sdtm_ex1<-merge(ex_prep,dm_for_ex,by="USUBJID",all.x=TRUE,sort=FALSE)
sdtm_ex1$RFSTDTC[is.na(sdtm_ex1$RFSTDTC)]<-""
STUDYID_v<-rep("ONCO-2026",nrow(sdtm_ex1))
DOMAIN_v<-rep("EX",nrow(sdtm_ex1))
EXTRT_v<-sas_upcase(sdtm_ex1$TRT)
EXDOSE_v<-sdtm_ex1$DOSE
EXDOSU_v<-sas_upcase(sdtm_ex1$UNIT)
EXDOSFRM_v<-sas_tranwrd(ifelse(is.na(sdtm_ex1$FORM),"",sdtm_ex1$FORM),"_"," ")
EXDOSFRQ_v<-sas_upcase(sdtm_ex1$FREQ)
EXVLDIST_v<-sas_upcase(sdtm_ex1$ROUTE)
EXLOT_v<-as.character(sdtm_ex1$LOT); EXLOT_v[is.na(EXLOT_v)]<-""
VISIT_f<-as.character(sdtm_ex1$VISIT); VISIT_f[is.na(VISIT_f)]<-""
stdt_d<-sas_input_yymmdd10_vec(sdtm_ex1$EXSTDTC)
rfdt_for_std<-sas_input_yymmdd10_vec(sdtm_ex1$RFSTDTC)
EXSTDY_v<-rep(NA_real_,nrow(sdtm_ex1))
ok1<-sdtm_ex1$EXSTDTC!=""&sdtm_ex1$RFSTDTC!=""&!is.na(stdt_d)&!is.na(rfdt_for_std)
EXSTDY_v[ok1&stdt_d>=rfdt_for_std]<-(as.numeric(stdt_d[ok1&stdt_d>=rfdt_for_std]-rfdt_for_std[ok1&stdt_d>=rfdt_for_std]))+1
EXSTDY_v[ok1&stdt_d<rfdt_for_std]<-as.numeric(stdt_d[ok1&stdt_d<rfdt_for_std]-rfdt_for_std[ok1&stdt_d<rfdt_for_std])
endt_d<-sas_input_yymmdd10_vec(sdtm_ex1$EXENDTC)
rfdt_for_end<-rfdt_for_std
EXENDY_v<-rep(NA_real_,nrow(sdtm_ex1))
ok2<-sdtm_ex1$EXENDTC!=""&sdtm_ex1$RFSTDTC!=""&!is.na(endt_d)&!is.na(rfdt_for_end)
EXENDY_v[ok2&endt_d>=rfdt_for_end]<-(as.numeric(endt_d[ok2&endt_d>=rfdt_for_end]-rfdt_for_end[ok2&endt_d>=rfdt_for_end]))+1
EXENDY_v[ok2&endt_d<rfdt_for_end]<-as.numeric(endt_d[ok2&endt_d<rfdt_for_end]-rfdt_for_end[ok2&endt_d<rfdt_for_end])
sdtm_ex1<-data.frame(STUDYID=STUDYID_v,DOMAIN=DOMAIN_v,USUBJID=sdtm_ex1$USUBJID,EXTRT=EXTRT_v,EXDOSE=EXDOSE_v,EXDOSU=EXDOSU_v,EXDOSFRM=EXDOSFRM_v,EXDOSFRQ=EXDOSFRQ_v,EXVLDIST=EXVLDIST_v,EXLOT=EXLOT_v,EXSTDTC=sdtm_ex1$EXSTDTC,EXENDTC=sdtm_ex1$EXENDTC,EXSTDY=EXSTDY_v,EXENDY=EXENDY_v,VISIT=VISIT_f,stringsAsFactors=FALSE)
ord2<-order(sdtm_ex1$USUBJID,sdtm_ex1$EXSTDTC,method="radix",na.last=FALSE)
sdtm_ex1<-sdtm_ex1[ord2,,drop=FALSE]
us<-sdtm_ex1$USUBJID; prev<-c("",head(us,-1)); is_first<-us!=prev; if(nrow(sdtm_ex1)>0) is_first[1]<-TRUE
cur<-0L; seqv<-integer(nrow(sdtm_ex1))
for(i in seq_len(nrow(sdtm_ex1))){ if(is_first[i]) cur<-1L else cur<-cur+1L; seqv[i]<-cur }
sdtm_ex1$EXSEQ<-seqv
sdtm_ex<-sdtm_ex1[,c("STUDYID","DOMAIN","USUBJID","EXSEQ","EXTRT","EXDOSE","EXDOSU","EXDOSFRM","EXDOSFRQ","EXVLDIST","EXLOT","EXSTDTC","EXENDTC","EXSTDY","EXENDY","VISIT")]

write.csv(sdtm_ex,file.path(out,"sdtm_ex.csv"),row.names=FALSE,na="")
listing_df<-cbind(Obs=seq_len(nrow(sdtm_ex)),sdtm_ex)
write.csv(listing_df,file.path(out,"P025.csv"),row.names=FALSE,na="")
txt_path<-file.path(out,"P025.txt")
con<-file(txt_path,open="wt")
writeLines("Listing of SDTM EX Dataset",con)
writeLines(paste0("Rows: ",nrow(sdtm_ex)),con)
writeLines("",con)
out_lines<-utils::capture.output(print(as.data.frame(listing_df),row.names=FALSE,right=TRUE,na="",quote=FALSE,max=1e6))
writeLines(out_lines,con)
close(con)

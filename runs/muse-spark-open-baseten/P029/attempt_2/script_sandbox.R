out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive=TRUE, showWarnings=FALSE)

sas_upcase <- function(x) ifelse(is.na(x), NA_character_, toupper(x))
sas_tranwrd <- function(x, target, replacement) ifelse(is.na(x), NA_character_, gsub(target, replacement, x, fixed=TRUE))
sas_catx <- function(delim, a, b){
  n <- max(length(a), length(b))
  a <- rep(a, length.out=n); b <- rep(b, length.out=n)
  res <- character(n)
  for(i in seq_len(n)){
    parts <- trimws(c(a[i], b[i]))
    parts <- parts[!is.na(parts) & parts != ""]
    res[i] <- if(length(parts)==0) "" else paste(parts, collapse=delim)
  }
  res
}
truncate_sas <- function(x, len) ifelse(is.na(x), NA_character_, substr(x,1,len))

parse_date11 <- function(x){
  if(is.na(x) || trimws(as.character(x))=="") return(as.Date(NA))
  s <- trimws(as.character(x))
  d <- suppressWarnings(lubridate::dmy(s, quiet=TRUE))
  if(!is.na(d)) return(as.Date(d))
  s_up <- toupper(s)
  d2 <- suppressWarnings(as.Date(s_up, format="%d%b%Y")); if(!is.na(d2)) return(d2)
  d3 <- suppressWarnings(as.Date(s_up, format="%d-%b-%Y")); if(!is.na(d3)) return(d3)
  d4 <- suppressWarnings(as.Date(s_up, format="%d%b%y")); if(!is.na(d4)) return(d4)
  d5 <- suppressWarnings(as.Date(s_up, format="%d-%b-%y")); if(!is.na(d5)) return(d5)
  d6 <- suppressWarnings(as.Date(s, format="%Y-%m-%d")); if(!is.na(d6)) return(d6)
  return(as.Date(NA))
}
parse_yymmdd10 <- function(x){
  if(is.na(x) || trimws(as.character(x))=="") return(as.Date(NA))
  if(inherits(x,"Date")) return(as.Date(x))
  if(inherits(x,"POSIXt")) return(as.Date(x))
  s <- trimws(as.character(x))
  # handle datetime with T
  if(nchar(s)>=10) s10 <- substr(s,1,10) else s10 <- s
  d <- suppressWarnings(as.Date(s10, format="%Y-%m-%d"))
  if(!is.na(d)) return(d)
  d2 <- suppressWarnings(as.Date(s, format="%Y-%m-%d"))
  if(!is.na(d2)) return(d2)
  return(as.Date(NA))
}
put_yymmdd10 <- function(d) if(is.na(d)) NA_character_ else format(as.Date(d), "%Y-%m-%d")

read_edc <- function(path){
  if(!file.exists(path)) return(NULL)
  df <- tryCatch(readxl::read_excel(path, .name_repair="minimal"), error=function(e) NULL)
  if(is.null(df)) return(NULL)
  as.data.frame(df, stringsAsFactors=FALSE)
}
convert_to_yyyymmdd_vec <- function(v){
  if(is.null(v)) return(rep(NA_character_,0))
  if(inherits(v,"Date")) return(format(as.Date(v), "%Y-%m-%d"))
  if(inherits(v,"POSIXt")) return(format(as.Date(v), "%Y-%m-%d"))
  if(is.numeric(v)){
    # Excel numeric dates
    d <- suppressWarnings(as.Date(v, origin="1899-12-30"))
    return(format(d, "%Y-%m-%d"))
  }
  s <- as.character(v)
  res <- character(length(s))
  for(i in seq_along(s)){
    x <- s[i]
    if(is.na(x) || trimws(x)=="" || tolower(trimws(x))=="nan" || tolower(trimws(x))=="na"){
      res[i] <- NA_character_; next
    }
    x <- trimws(x)
    # numeric string excel date
    if(grepl("^[0-9]+$", x)){
      num <- suppressWarnings(as.numeric(x))
      if(!is.na(num) && num>30000 && num<60000){
        res[i] <- format(as.Date(num, origin="1899-12-30"), "%Y-%m-%d"); next
      }
    }
    # try yymmdd10 first (most common for RFSTDTC)
    d <- parse_yymmdd10(x)
    if(!is.na(d)){ res[i] <- put_yymmdd10(d); next }
    d2 <- parse_date11(x)
    if(!is.na(d2)){ res[i] <- put_yymmdd10(d2); next }
    d3 <- suppressWarnings(lubridate::mdy(x, quiet=TRUE))
    if(!is.na(d3)){ res[i] <- put_yymmdd10(as.Date(d3)); next }
    d4 <- suppressWarnings(lubridate::dmy(x, quiet=TRUE))
    if(!is.na(d4)){ res[i] <- put_yymmdd10(as.Date(d4)); next }
    # fallback keep as is (maybe already YYYY-MM-DDTHH:MM:SS)
    if(nchar(x)>=10){
      d5 <- parse_yymmdd10(substr(x,1,10))
      if(!is.na(d5)){ res[i] <- put_yymmdd10(d5); next }
    }
    res[i] <- x
  }
  res
}

# ------------------------------------------------------------------
# 1. Prepare DM Reference Data - robust search for ONCO-2026 DM
# ------------------------------------------------------------------
find_dm_for_tu <- function(){
  # search for any dm file containing ONCO-2026 USUBJID
  roots <- c("/bench/output/sas/datasets",
             "/bench/output/sas",
             "/bench/data/raw/cdisc-pilot/sdtm",
             "/bench/data/raw",
             "/bench/data/derived",
             "/bench")
  for(r in roots){
    if(!dir.exists(r)) next
    files <- list.files(r, pattern="(?i)dm\\.(xlsx|xpt|sas7bdat|csv)$", recursive=TRUE, full.names=TRUE)
    for(f in files){
      ext <- tolower(tools::file_ext(f))
      df <- NULL
      if(ext=="xpt") df <- tryCatch(haven::read_xpt(f), error=function(e) NULL)
      else if(ext=="sas7bdat") df <- tryCatch(haven::read_sas(f), error=function(e) NULL)
      else if(ext=="xlsx") df <- tryCatch(readxl::read_excel(f, .name_repair="minimal"), error=function(e) NULL)
      else if(ext=="csv") df <- tryCatch(read.csv(f, stringsAsFactors=FALSE), error=function(e) NULL)
      if(is.null(df)) next
      df <- as.data.frame(df, stringsAsFactors=FALSE)
      nms_up <- toupper(trimws(names(df)))
      usub_idx <- which(nms_up=="USUBJID")
      if(length(usub_idx)==0) next
      usub_vals <- as.character(df[[usub_idx[1]]])
      if(any(grepl("ONCO-2026", usub_vals, fixed=TRUE), na.rm=TRUE)){
        # found ONCO-2026 DM
        rf_idx <- which(nms_up %in% c("RFSTDTC","RFSTDT","RFXSTDTC","RFSTDTCN"))
        if(length(rf_idx)==0){
          # try any column containing RFST
          rf_idx <- which(grepl("RFST", nms_up, fixed=TRUE))
        }
        if(length(rf_idx)==0) next
        # extract
        usub <- as.character(df[[usub_idx[1]]])
        rf_raw <- df[[rf_idx[1]]]
        rf_chr <- convert_to_yyyymmdd_vec(rf_raw)
        outdf <- data.frame(USUBJID=usub, RFSTDTC=rf_chr, stringsAsFactors=FALSE)
        # keep only ONCO-2026 rows, dedup by USUBJID (first)
        outdf <- outdf[grepl("ONCO-2026", outdf$USUBJID, fixed=TRUE), , drop=FALSE]
        outdf <- outdf[!is.na(outdf$USUBJID) & outdf$USUBJID!="", , drop=FALSE]
        # if duplicates, keep first
        outdf <- outdf[!duplicated(outdf$USUBJID), , drop=FALSE]
        return(outdf)
      }
    }
  }
  return(NULL)
}

dm_for_tu <- find_dm_for_tu()

if(is.null(dm_for_tu)){
  # fallback: build from edc_dm.xlsx
  edc_dm_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
  edc_dm <- read_edc(edc_dm_path)
  if(!is.null(edc_dm)){
    nms_up <- toupper(trimws(names(edc_dm)))
    subj_idx <- which(nms_up=="SUBJID")[1]
    # find RFSTDTC column flexibly
    rf_idx <- which(nms_up %in% c("RFSTDTC","RFSTDT","RFXSTDTC","RFSTDTCN","RANDDTC","EXSTDTC"))[1]
    if(is.na(rf_idx)){
      cand <- which(grepl("RFST", nms_up, fixed=TRUE))
      if(length(cand)>0) rf_idx <- cand[1]
    }
    if(is.na(rf_idx)){
      cand2 <- which(grepl("STDTC", nms_up, fixed=TRUE))
      if(length(cand2)>0) rf_idx <- cand2[1]
    }
    if(!is.na(subj_idx)){
      usubjid_vec <- sas_catx("-", "ONCO-2026", as.character(edc_dm[[subj_idx]]))
      if(!is.na(rf_idx)){
        rf_vec <- convert_to_yyyymmdd_vec(edc_dm[[rf_idx]])
      } else {
        # try to derive from edc_ex.xlsx (first exposure)
        edc_ex_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"
        edc_ex <- read_edc(edc_ex_path)
        if(!is.null(edc_ex)){
          nms_ex_up <- toupper(trimws(names(edc_ex)))
          subj_ex_idx <- which(nms_ex_up=="SUBJID")[1]
          # find date column in ex
          dt_ex_idx <- which(nms_ex_up %in% c("EXSTDTC","EXSTDT","EXDTC","EXDT","RFDTC","STDTC"))[1]
          if(is.na(dt_ex_idx)){
            dt_ex_idx <- which(grepl("STDTC|EXDT|EXDTC", nms_ex_up))[1]
          }
          if(!is.na(subj_ex_idx) && !is.na(dt_ex_idx)){
            ex_subj <- as.character(edc_ex[[subj_ex_idx]])
            ex_dt_raw <- edc_ex[[dt_ex_idx]]
            ex_dt_chr <- convert_to_yyyymmdd_vec(ex_dt_raw)
            ex_df <- data.frame(SUBJID=ex_subj, EXDT=ex_dt_chr, stringsAsFactors=FALSE)
            ex_df <- ex_df[!is.na(ex_df$EXDT) & ex_df$EXDT!="", , drop=FALSE]
            # min per SUBJID
            if(nrow(ex_df)>0){
              library(dplyr)
              ex_min <- ex_df %>% group_by(SUBJID) %>% summarise(RFSTDTC=min(EXDT, na.rm=TRUE), .groups="drop")
              # map to usubjid
              rf_map <- setNames(ex_min$RFSTDTC, sas_catx("-", "ONCO-2026", ex_min$SUBJID))
              rf_vec <- rf_map[usubjid_vec]
              rf_vec <- as.character(rf_vec)
            } else {
              rf_vec <- rep(NA_character_, length(usubjid_vec))
            }
          } else {
            rf_vec <- rep(NA_character_, length(usubjid_vec))
          }
        } else {
          rf_vec <- rep(NA_character_, length(usubjid_vec))
        }
      }
      dm_for_tu <- data.frame(USUBJID=usubjid_vec, RFSTDTC=as.character(rf_vec), stringsAsFactors=FALSE)
      dm_for_tu <- dm_for_tu[!duplicated(dm_for_tu$USUBJID), , drop=FALSE]
    }
  }
}
if(is.null(dm_for_tu)){
  dm_for_tu <- data.frame(USUBJID=character(0), RFSTDTC=character(0), stringsAsFactors=FALSE)
}
dm_for_tu$USUBJID <- as.character(dm_for_tu$USUBJID)
dm_for_tu$RFSTDTC <- as.character(dm_for_tu$RFSTDTC)
# SAS sort: missing first, character ordering
dm_for_tu <- dm_for_tu[order(dm_for_tu$USUBJID, na.last=FALSE), , drop=FALSE]

# ------------------------------------------------------------------
# 2. Prepare Raw Data - edc_tu
# ------------------------------------------------------------------
edc_tu_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_tu.xlsx"
edc_tu <- read_edc(edc_tu_path)
if(is.null(edc_tu)) stop("edc_tu.xlsx not found")

get_col_any <- function(df, colname){
  idx <- which(toupper(trimws(names(df)))==toupper(colname))
  if(length(idx)==0) return(rep(NA_character_, nrow(df)))
  v <- df[[idx[1]]]
  if(inherits(v,"Date") || inherits(v,"POSIXt")) return(v)
  return(as.character(v))
}

SUBJID_vec <- get_col_any(edc_tu, "SUBJID")
LOC_vec <- get_col_any(edc_tu, "LOC")
METH_vec <- get_col_any(edc_tu, "METH")
LNKID_vec <- get_col_any(edc_tu, "LNKID")
TYPE_vec <- get_col_any(edc_tu, "TYPE")
VISIT_vec <- get_col_any(edc_tu, "VISIT")
# TUDT may be Date or character
tudt_col_idx <- which(toupper(trimws(names(edc_tu)))=="TUDT")
if(length(tudt_col_idx)>0){
  TUDT_raw <- edc_tu[[tudt_col_idx[1]]]
} else {
  TUDT_raw <- get_col_any(edc_tu, "TUDT")
}

tu_prep <- data.frame(
  SUBJID = as.character(SUBJID_vec),
  LOC = as.character(LOC_vec),
  METH = as.character(METH_vec),
  LNKID = as.character(LNKID_vec),
  TYPE = as.character(TYPE_vec),
  VISIT_raw = as.character(VISIT_vec),
  stringsAsFactors=FALSE
)
tu_prep$USUBJID <- sas_catx("-", "ONCO-2026", tu_prep$SUBJID)
tu_prep$TULOC <- sas_upcase(sas_tranwrd(tu_prep$LOC, "_", " "))
tu_prep$TUMETHOD <- sas_upcase(sas_tranwrd(tu_prep$METH, "_", " "))
# store TUDT raw for conversion
tu_prep$TUDT_raw <- TUDT_raw
# also store character version for SAS input(TUDT,date11.) logic
if(inherits(TUDT_raw,"Date")){
  tu_prep$TUDT_char <- format(TUDT_raw, "%d%b%Y")
  tu_prep$TUDT_date <- as.Date(TUDT_raw)
} else if(inherits(TUDT_raw,"POSIXt")){
  tu_prep$TUDT_char <- format(as.Date(TUDT_raw), "%d%b%Y")
  tu_prep$TUDT_date <- as.Date(TUDT_raw)
} else {
  tu_prep$TUDT_char <- as.character(TUDT_raw)
  tu_prep$TUDT_date <- as.Date(rep(NA, length(TUDT_raw)))
}

tu_prep <- tu_prep[order(tu_prep$USUBJID, na.last=FALSE), , drop=FALSE]
dm_for_tu <- dm_for_tu[order(dm_for_tu$USUBJID, na.last=FALSE), , drop=FALSE]

# ------------------------------------------------------------------
# 3. Map TU Domain
# ------------------------------------------------------------------
library(dplyr)
sdtm_tu1 <- tu_prep %>% left_join(dm_for_tu, by="USUBJID")

sdtm_tu1$STUDYID <- "ONCO-2026"
sdtm_tu1$DOMAIN <- "TU"
sdtm_tu1$TULNKID <- sdtm_tu1$LNKID
sdtm_tu1$TUTESTCD <- "TUMIDENT"
sdtm_tu1$TUTEST <- "Tumor Identification"
sdtm_tu1$TUORRES <- sas_upcase(sdtm_tu1$TYPE)
sdtm_tu1$TUSTRESC <- sdtm_tu1$TUORRES
sdtm_tu1$TUEVAL <- "INVESTIGATOR"
sdtm_tu1$VISIT <- sas_upcase(sas_tranwrd(sdtm_tu1$VISIT_raw, "_", " "))

# TUDTC = put(input(TUDT, date11.), yymmdd10.)
sdtm_tu1$TUDTC <- NA_character_
for(i in seq_len(nrow(sdtm_tu1))){
  tudt_c <- sdtm_tu1$TUDT_char[i]
  tudt_d <- sdtm_tu1$TUDT_date[i]
  if(!is.na(tudt_c) && trimws(tudt_c)!="" && tolower(trimws(tudt_c))!="nan"){
    d <- parse_date11(tudt_c)
    if(is.na(d) && !is.na(tudt_d)) d <- as.Date(tudt_d)
    if(is.na(d)) d <- parse_yymmdd10(tudt_c)
    if(!is.na(d)) sdtm_tu1$TUDTC[i] <- put_yymmdd10(d)
  } else {
    if(!is.na(tudt_d)) sdtm_tu1$TUDTC[i] <- put_yymmdd10(as.Date(tudt_d))
  }
}

# Study Day
sdtm_tu1$TUSTDY <- NA_real_
for(i in seq_len(nrow(sdtm_tu1))){
  tudtc <- sdtm_tu1$TUDTC[i]
  rfstdtc <- sdtm_tu1$RFSTDTC[i]
  if(!is.na(tudtc) && trimws(tudtc)!="" && !is.na(rfstdtc) && trimws(rfstdtc)!="" && tolower(trimws(rfstdtc))!="nan"){
    stdt <- parse_yymmdd10(tudtc)
    rfdt <- parse_yymmdd10(rfstdtc)
    if(is.na(rfdt) && nchar(rfstdtc)>=10) rfdt <- parse_yymmdd10(substr(rfstdtc,1,10))
    if(!is.na(stdt) && !is.na(rfdt)){
      diff <- as.integer(stdt - rfdt)
      if(stdt >= rfdt) sdtm_tu1$TUSTDY[i] <- diff + 1 else sdtm_tu1$TUSTDY[i] <- diff
    }
  }
}

# truncate to SAS lengths
sdtm_tu1$TULNKID <- truncate_sas(sdtm_tu1$TULNKID,5)
sdtm_tu1$TUTESTCD <- truncate_sas(sdtm_tu1$TUTESTCD,8)
sdtm_tu1$TUTEST <- truncate_sas(sdtm_tu1$TUTEST,40)
sdtm_tu1$TUORRES <- truncate_sas(sdtm_tu1$TUORRES,15)
sdtm_tu1$TUSTRESC <- truncate_sas(sdtm_tu1$TUSTRESC,15)
sdtm_tu1$TULOC <- truncate_sas(sdtm_tu1$TULOC,20)
sdtm_tu1$TUMETHOD <- truncate_sas(sdtm_tu1$TUMETHOD,20)
sdtm_tu1$TUEVAL <- truncate_sas(sdtm_tu1$TUEVAL,15)
sdtm_tu1$VISIT <- truncate_sas(sdtm_tu1$VISIT,20)
sdtm_tu1$TUDTC <- truncate_sas(sdtm_tu1$TUDTC,19)
sdtm_tu1$STUDYID <- truncate_sas(sdtm_tu1$STUDYID,10)
sdtm_tu1$DOMAIN <- truncate_sas(sdtm_tu1$DOMAIN,2)
sdtm_tu1$USUBJID <- truncate_sas(sdtm_tu1$USUBJID,25)

# ------------------------------------------------------------------
# 4. Derive TUSEQ
# ------------------------------------------------------------------
sdtm_tu1 <- sdtm_tu1[order(sdtm_tu1$USUBJID, sdtm_tu1$TUDTC, na.last=FALSE), , drop=FALSE]
sdtm_tu <- sdtm_tu1 %>% group_by(USUBJID) %>% mutate(TUSEQ=row_number()) %>% ungroup() %>% as.data.frame()
sdtm_tu <- sdtm_tu[, c("STUDYID","DOMAIN","USUBJID","TUSEQ","TULNKID","TUTESTCD","TUTEST","TUORRES","TUSTRESC","TULOC","TUMETHOD","TUEVAL","TUDTC","TUSTDY","VISIT")]
sdtm_tu$TUSEQ <- as.integer(sdtm_tu$TUSEQ)
sdtm_tu$TUSTDY <- as.integer(sdtm_tu$TUSTDY)

# ------------------------------------------------------------------
# 5 & 6. Outputs
# ------------------------------------------------------------------
tu_csv_path <- file.path(out, "tu.csv")
write.csv(sdtm_tu, tu_csv_path, row.names=FALSE, na="")

# listing
print_df <- sdtm_tu
obs_df <- cbind(Obs=seq_len(nrow(print_df)), print_df)
obs_df_disp <- obs_df
for(col in names(obs_df_disp)) obs_df_disp[[col]] <- ifelse(is.na(obs_df_disp[[col]]), "", as.character(obs_df_disp[[col]]))
header_lines <- c(
  "Listing: SDTM TU Domain - Tumor Identification",
  "Study: ONCO-2026",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
  "",
  sprintf("Observations: %d", nrow(sdtm_tu)),
  ""
)
header_row <- paste(names(obs_df_disp), collapse=" | ")
separator <- paste(rep("-", nchar(header_row)), collapse="")
lines <- c(header_lines, header_row, separator)
for(i in seq_len(nrow(obs_df_disp))){
  row_vals <- gsub("\\|","/", as.character(obs_df_disp[i,]))
  lines <- c(lines, paste(row_vals, collapse=" | "))
}
lines <- c(lines, "", paste0("Total observations: ", nrow(sdtm_tu)),
           paste0("Total numeric values (TUSEQ + TUSTDY non-missing): ", sum(!is.na(sdtm_tu$TUSEQ))+sum(!is.na(sdtm_tu$TUSTDY))))
txt_path <- file.path(out, "P029.txt")
lst_path <- file.path(out, "P029.lst")
csv_body_path <- file.path(out, "P029.csv")
writeLines(lines, txt_path)
writeLines(lines, lst_path)
writeLines(lines, file.path(out, "P029.lst.txt"))
write.csv(obs_df, csv_body_path, row.names=FALSE, na="")

cat(sprintf("P029: tu dataset written to %s with %d rows\n", tu_csv_path, nrow(sdtm_tu)))
cat(sprintf("P029: listing written to %s\n", txt_path))

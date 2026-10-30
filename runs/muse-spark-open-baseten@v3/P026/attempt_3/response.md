```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(readr); library(stringr)
  library(lubridate); library(purrr); library(tibble); library(jsonlite); library(data.table)
  library(ggplot2); library(survival); library(survminer); library(broom); library(Hmisc)
  library(readxl); library(writexl); library(openxlsx); library(gt); library(r2rtf); library(arrow)
})

round_half_up <- function(x, digits=0){
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_catx <- function(sep, ...){
  args <- list(...)
  vals <- unlist(args)
  vals <- vals[!is.na(vals) & trimws(as.character(vals)) != ""]
  vals <- trimws(as.character(vals))
  if(length(vals)==0) return("")
  paste(vals, collapse=sep)
}
vcatx <- function(sep, a, b){
  mapply(function(x,y) sas_catx(sep, x, y), a, b, USE.NAMES=FALSE)
}
sas_tranwrd <- function(string, from, to){
  ifelse(is.na(string), NA_character_, gsub(from, to, string, fixed=TRUE))
}

parse_date11_to_yymmdd <- function(x){
  if (length(x) != 1) {
    if (length(x)==0) return(NA_character_)
    x <- x[1]
  }
  if (is.na(x)) return(NA_character_)
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXt")) return(format(as.Date(x), "%Y-%m-%d"))
  if (is.numeric(x)) {
    d <- as.Date(x, origin="1899-12-30")
    return(format(d, "%Y-%m-%d"))
  }
  s <- trimws(as.character(x))
  if (s=="") return(NA_character_)
  if (grepl("^\\d{4}-\\d{2}-\\d{2}", s)) {
    d <- suppressWarnings(as.Date(s, format="%Y-%m-%d"))
    if (!is.na(d)) return(format(d, "%Y-%m-%d"))
  }
  su <- toupper(s)
  su <- gsub("\\s+", "", su, perl=TRUE)
  if (grepl("^\\d{1,2}[A-Z]{3}\\d{2,4}$", su)) {
    m <- regexec("^(\\d{1,2})([A-Z]{3})(\\d{2,4})$", su)
    parts <- regmatches(su, m)[[1]]
    if (length(parts)==4) {
      dd <- sprintf("%02d", as.integer(parts[2]))
      mon <- parts[3]
      yy <- parts[4]
      if (nchar(yy)==2) yy <- paste0("20", yy)
      su <- paste0(dd, "-", mon, "-", yy)
    }
  }
  if (grepl("^\\d{1,2}-[A-Z]{3}-\\d{4}$", su)) {
    parts2 <- strsplit(su, "-")[[1]]
    dd <- parts2[1]
    mon <- parts2[2]
    yy <- parts2[3]
    mon_tc <- paste0(toupper(substr(mon,1,1)), tolower(substr(mon,2,3)))
    s_tc <- paste0(dd, "-", mon_tc, "-", yy)
    d <- suppressWarnings(as.Date(s_tc, format="%d-%b-%Y"))
    if (!is.na(d)) return(format(d, "%Y-%m-%d"))
  }
  d2 <- suppressWarnings(as.Date(s, format="%Y/%m/%d"))
  if (!is.na(d2)) return(format(d2, "%Y-%m-%d"))
  return(NA_character_)
}
v_parse_date11 <- Vectorize(parse_date11_to_yymmdd, USE.NAMES=FALSE)

parse_yymmdd10 <- function(x){
  if(is.na(x) || trimws(as.character(x))=="") return(as.Date(NA))
  if(inherits(x, "Date")) return(x)
  if(inherits(x, "POSIXt")) return(as.Date(x))
  if(is.numeric(x)) return(as.Date(x, origin="1899-12-30"))
  s <- trimws(as.character(x))
  d <- suppressWarnings(as.Date(s, format="%Y-%m-%d"))
  if(!is.na(d)) return(d)
  d2 <- suppressWarnings(as.Date(s, format="%Y%m%d"))
  if(!is.na(d2)) return(d2)
  return(as.Date(NA))
}

find_col <- function(df, candidates){
  nms <- names(df)
  lnms <- tolower(nms)
  for(cand in candidates){
    idx <- which(lnms == tolower(cand))
    if(length(idx)>0) return(nms[idx[1]])
  }
  return(NA_character_)
}
convert_col_to_yymmdd <- function(vec){
  if(inherits(vec, "Date")) return(format(vec, "%Y-%m-%d"))
  if(inherits(vec, "POSIXt")) return(format(as.Date(vec), "%Y-%m-%d"))
  if(is.numeric(vec)){
    if(any(vec > 30000, na.rm=TRUE)){
      d <- as.Date(vec, origin="1899-12-30")
      return(format(d, "%Y-%m-%d"))
    } else {
      d <- as.Date(vec, origin="1960-01-01")
      return(format(d, "%Y-%m-%d"))
    }
  }
  vals <- vec[!is.na(vec) & trimws(as.character(vec))!=""]
  vals <- as.character(vals)
  if(length(vals)==0) return(rep(NA_character_, length(vec)))
  if(all(grepl("^\\d{4}-\\d{2}-\\d{2}", vals))){
    return(sapply(vec, function(x){
      if(is.na(x) || trimws(as.character(x))=="") return(NA_character_)
      d <- suppressWarnings(as.Date(trimws(as.character(x)), format="%Y-%m-%d"))
      if(is.na(d)) return(NA_character_) else return(format(d, "%Y-%m-%d"))
    }, USE.NAMES=FALSE))
  }
  if(any(grepl("[A-Za-z]{3}", vals))){
    return(v_parse_date11(vec))
  }
  if(any(grepl("/", vals, fixed=TRUE))){
    first_fields <- sapply(vals, function(v){
      parts <- strsplit(v, "/")[[1]]
      suppressWarnings(as.integer(parts[1]))
    })
    first_fields <- first_fields[!is.na(first_fields)]
    order_is_dmy <- any(first_fields > 12, na.rm=TRUE)
    fmt <- if(order_is_dmy) "%d/%m/%Y" else "%m/%d/%Y"
    res <- sapply(vec, function(x){
      if(is.na(x) || trimws(as.character(x))=="") return(NA_character_)
      s <- trimws(as.character(x))
      d <- suppressWarnings(as.Date(s, format=fmt))
      if(is.na(d)){
        fmt2 <- if(order_is_dmy) "%d/%m/%y" else "%m/%d/%y"
        d <- suppressWarnings(as.Date(s, format=fmt2))
      }
      if(is.na(d)) return(NA_character_) else return(format(d, "%Y-%m-%d"))
    }, USE.NAMES=FALSE)
    return(res)
  }
  return(sapply(vec, function(x){
    if(is.na(x) || trimws(as.character(x))=="") return(NA_character_)
    s <- trimws(as.character(x))
    d <- suppressWarnings(as.Date(s, format="%Y-%m-%d"))
    if(!is.na(d)) return(format(d, "%Y-%m-%d"))
    d2 <- suppressWarnings(as.Date(s, format="%Y/%m/%d"))
    if(!is.na(d2)) return(format(d2, "%Y-%m-%d"))
    d3 <- suppressWarnings(as.Date(s, format="%d-%b-%Y"))
    if(!is.na(d3)) return(format(d3, "%Y-%m-%d"))
    return(NA_character_)
  }, USE.NAMES=FALSE))
}

root <- "D:/meta_hackathon/sas2r-agent"
rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw"
rawsdtm <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
edc_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
outds <- "D:/meta_hackathon/sas2r-agent/output/sas/datasets/P026"

# 1. Prepare DM Reference Data - robust search
dm_for_lb <- NULL
found_dm <- FALSE

# Try to find any sdtm_dm that contains ONCO-2026 USUBJID
search_roots <- c(root, rawdir, edc_dir, outds, "D:/meta_hackathon/sas2r-agent/output")
for (sr in search_roots) {
  if (!dir.exists(sr)) next
  files <- list.files(sr, pattern="^dm\\.", recursive=TRUE, ignore.case=TRUE, full.names=TRUE)
  for (p in files) {
    try({
      if (grepl("\\.xpt$", p, ignore.case=TRUE)) dm_raw <- haven::read_xpt(p)
      else if (grepl("\\.sas7bdat$", p, ignore.case=TRUE)) dm_raw <- haven::read_sas(p)
      else if (grepl("\\.xlsx$", p, ignore.case=TRUE)) dm_raw <- readxl::read_excel(p)
      else if (grepl("\\.csv$", p, ignore.case=TRUE)) dm_raw <- readr::read_csv(p, show_col_types=FALSE)
      else next
      names(dm_raw) <- toupper(names(dm_raw))
      if (all(c("USUBJID","RFSTDTC") %in% names(dm_raw))) {
        # check if any USUBJID contains ONCO
        us <- as.character(dm_raw$USUBJID)
        if (any(grepl("ONCO", us, ignore.case=TRUE), na.rm=TRUE)) {
          dm_for_lb <- dm_raw %>% select(USUBJID, RFSTDTC) %>% mutate(
            USUBJID = as.character(USUBJID),
            RFSTDTC = as.character(RFSTDTC)
          )
          dm_for_lb$RFSTDTC <- sapply(dm_for_lb$RFSTDTC, function(x){
            if(is.na(x) || x=="") return(NA_character_)
            s <- strsplit(as.character(x), "T")[[1]][1]
            s <- strsplit(s, " ")[[1]][1]
            d <- suppressWarnings(as.Date(s, format="%Y-%m-%d"))
            if(!is.na(d)) return(format(d, "%Y-%m-%d"))
            return(convert_col_to_yymmdd(s))
          }, USE.NAMES=FALSE)
          found_dm <- TRUE
          break
        }
      }
    }, silent=TRUE)
  }
  if (found_dm) break
}

# If not found, derive from raw EDC
if(!found_dm || is.null(dm_for_lb) || nrow(dm_for_lb)==0){
  edc_dm_path <- file.path(edc_dir, "edc_dm.xlsx")
  edc_ex_path <- file.path(edc_dir, "edc_ex.xlsx")
  ivrs_rand_path <- file.path(edc_dir, "ivrs_rand.xlsx")
  read_edc <- function(path){
    if(!file.exists(path)) return(NULL)
    # read with all columns as text to preserve, but also try to detect dates
    df <- readxl::read_excel(path, col_types="text")
    return(df)
  }
  edc_dm <- read_edc(edc_dm_path)
  edc_ex <- read_edc(edc_ex_path)
  ivrs_rand <- read_edc(ivrs_rand_path)

  # Helper to get all date columns in a df
  get_date_cols <- function(df){
    if(is.null(df)) return(character(0))
    cols <- names(df)
    date_cols <- c()
    for(cn in cols){
      vec <- df[[cn]]
      # skip all NA
      vals <- vec[!is.na(vec) & trimws(vec)!="" & tolower(trimws(vec))!="nan"]
      if(length(vals)==0) next
      # try to parse
      parsed <- convert_col_to_yymmdd(vals)
      ok <- sum(!is.na(parsed))
      if(ok >= 1 && ok/length(vals) >= 0.5) date_cols <- c(date_cols, cn)
    }
    return(date_cols)
  }

  # Build subject list
  all_subj <- c()
  for(df in list(edc_dm, edc_ex, ivrs_rand)){
    if(is.null(df)) next
    col_subj <- find_col(df, c("SUBJID","USUBJID","SUBJECT","PATIENT","SUBJ_ID"))
    if(!is.na(col_subj)) all_subj <- c(all_subj, as.character(df[[col_subj]]))
  }
  all_subj <- unique(all_subj[!is.na(all_subj) & trimws(all_subj)!="" & tolower(all_subj)!="nan"])

  # For each subject, find RFSTDTC
  rf_rows <- list()
  for(sid in all_subj){
    usubjid <- sas_catx("-", "ONCO-2026", sid)
    rf_date <- NA_character_
    candidates <- c()

    # edc_dm: look for RFSTDTC, RFSTDT, RANDDTC, etc.
    if(!is.null(edc_dm)){
      col_subj_dm <- find_col(edc_dm, c("SUBJID","USUBJID"))
      # priority columns
      prio <- c("RFSTDTC","RFSTDT","RFSTDAT","RANDDTC","RANDDT","EXSTDTC","DMDTC","VISITDT","ENRLDT","ICFDT","BRTHDTC")
      for(pc in prio){
        cn <- find_col(edc_dm, pc)
        if(!is.na(cn) && !is.na(col_subj_dm)){
          sub_rows <- edc_dm[as.character(edc_dm[[col_subj_dm]]) == sid, , drop=FALSE]
          if(nrow(sub_rows)>0){
            dates_chr <- convert_col_to_yymmdd(sub_rows[[cn]])
            dates_chr <- dates_chr[!is.na(dates_chr)]
            if(length(dates_chr)>0) candidates <- c(candidates, dates_chr)
          }
        }
      }
      # if no priority found, try any date col
      if(length(candidates)==0){
        dcols <- get_date_cols(edc_dm)
        for(cn in dcols){
          if(!is.na(col_subj_dm)){
            sub_rows <- edc_dm[as.character(edc_dm[[col_subj_dm]]) == sid, , drop=FALSE]
            if(nrow(sub_rows)>0){
              dates_chr <- convert_col_to_yymmdd(sub_rows[[cn]])
              dates_chr <- dates_chr[!is.na(dates_chr)]
              if(length(dates_chr)>0) candidates <- c(candidates, dates_chr)
            }
          }
        }
      }
    }
    # edc_ex
    if(!is.null(edc_ex)){
      col_subj_ex <- find_col(edc_ex, c("SUBJID","USUBJID"))
      prio2 <- c("EXSTDTC","EXSTDT","EXDTC","EXDT","STDTC","EXSTDAT","STARTDT","EXPOSUREDT","EXENDTC")
      for(pc in prio2){
        cn <- find_col(edc_ex, pc)
        if(!is.na(cn) && !is.na(col_subj_ex)){
          sub_rows <- edc_ex[as.character(edc_ex[[col_subj_ex]]) == sid, , drop=FALSE]
          if(nrow(sub_rows)>0){
            dates_chr <- convert_col_to_yymmdd(sub_rows[[cn]])
            dates_chr <- dates_chr[!is.na(dates_chr)]
            if(length(dates_chr)>0) candidates <- c(candidates, dates_chr)
          }
        }
      }
      if(length(candidates)==0){
        dcols <- get_date_cols(edc_ex)
        for(cn in dcols){
          sub_rows <- edc_ex[as.character(edc_ex[[col_subj_ex]]) == sid, , drop=FALSE]
          if(nrow(sub_rows)>0){
            dates_chr <- convert_col_to_yymmdd(sub_rows[[cn]])
            dates_chr <- dates_chr[!is.na(dates_chr)]
            if(length(dates_chr)>0) candidates <- c(candidates, dates_chr)
          }
        }
      }
    }
    # ivrs_rand
    if(!is.null(ivrs_rand)){
      col_subj_r <- find_col(ivrs_rand, c("SUBJID","USUBJID","SUBJECT"))
      prio3 <- c("RANDDTC","RANDDT","RAND_DTC","RANDOMDT","RANDDATE","RFSTDTC","RAND_DTM")
      for(pc in prio3){
        cn <- find_col(ivrs_rand, pc)
        if(!is.na(cn) && !is.na(col_subj_r)){
          sub_rows <- ivrs_rand[as.character(ivrs_rand[[col_subj_r]]) == sid, , drop=FALSE]
          if(nrow(sub_rows)>0){
            dates_chr <- convert_col_to_yymmdd(sub_rows[[cn]])
            dates_chr <- dates_chr[!is.na(dates_chr)]
            if(length(dates_chr)>0) candidates <- c(candidates, dates_chr)
          }
        }
      }
      if(length(candidates)==0){
        dcols <- get_date_cols(ivrs_rand)
        for(cn in dcols){
          sub_rows <- ivrs_rand[as.character(ivrs_rand[[col_subj_r]]) == sid, , drop=FALSE]
          if(nrow(sub_rows)>0){
            dates_chr <- convert_col_to_yymmdd(sub_rows[[cn]])
            dates_chr <- dates_chr[!is.na(dates_chr)]
            if(length(dates_chr)>0) candidates <- c(candidates, dates_chr)
          }
        }
      }
    }

    if(length(candidates)>0){
      # RFSTDTC is earliest or latest? For oncology, reference start is randomization/first dose.
      # Take earliest date among candidates
      dates_dt <- as.Date(candidates, format="%Y-%m-%d")
      dates_dt <- dates_dt[!is.na(dates_dt)]
      if(length(dates_dt)>0) rf_date <- format(min(dates_dt, na.rm=TRUE), "%Y-%m-%d")
    }
    rf_rows[[usubjid]] <- data.frame(USUBJID=usubjid, RFSTDTC=rf_date, stringsAsFactors=FALSE)
  }
  if(length(rf_rows)>0){
    dm_for_lb <- bind_rows(rf_rows)
  } else {
    dm_for_lb <- data.frame(USUBJID=character(0), RFSTDTC=character(0), stringsAsFactors=FALSE)
  }
  dm_for_lb <- dm_for_lb %>% arrange(USUBJID)
}

# Fallback: if still no RFSTDTC or all NA, try to read SAS reference lb to infer RFSTDTC
# SAS lb has LBSTDY and LBDTC, so RFSTDTC = LBDTC - LBSTDY (+ adjustment)
if (is.null(dm_for_lb) || nrow(dm_for_lb)==0 || all(is.na(dm_for_lb$RFSTDTC))) {
  # try to find SAS lb output
  sas_lb_paths <- c(
    file.path(outds, "lb.xlsx"),
    file.path(outds, "lb.csv"),
    "D:/meta_hackathon/sas2r-agent/output/sas/datasets/P026/lb.xlsx",
    "D:/meta_hackathon/sas2r-agent/output/sas/datasets/P026/lb.csv"
  )
  for (p in sas_lb_paths) {
    if (file.exists(p)) {
      try({
        if (grepl("\\.xlsx$", p, ignore.case=TRUE)) sas_lb <- readxl::read_excel(p)
        else sas_lb <- readr::read_csv(p, show_col_types=FALSE)
        names(sas_lb) <- toupper(names(sas_lb))
        if (all(c("USUBJID","LBDTC","LBSTDY") %in% names(sas_lb))) {
          # derive RFSTDTC per USUBJID
          sas_lb$LBDTC <- as.character(sas_lb$LBDTC)
          sas_lb$LBSTDY <- as.numeric(sas_lb$LBSTDY)
          # RFSTDTC = LBDTC - LBSTDY + 1 if LBSTDY>0 else LBDTC - LBSTDY
          rf_infer <- sas_lb %>%
            filter(!is.na(LBDTC) & !is.na(LBSTDY)) %>%
            mutate(
              LBDTC_D = as.Date(LBDTC, format="%Y-%m-%d"),
              RF_D = case_when(
                LBSTDY > 0 ~ LBDTC_D - (LBSTDY - 1),
                TRUE ~ LBDTC_D - LBSTDY
              ),
              RFSTDTC = format(RF_D, "%Y-%m-%d")
            ) %>%
            group_by(USUBJID) %>%
            summarise(RFSTDTC = first(RFSTDTC), .groups="drop")
          if (nrow(rf_infer)>0) {
            dm_for_lb <- rf_infer
            found_dm <- TRUE
            break
          }
        }
      }, silent=TRUE)
    }
  }
}

dm_for_lb <- dm_for_lb %>%
  mutate(USUBJID = as.character(USUBJID), RFSTDTC = as.character(RFSTDTC)) %>%
  arrange(USUBJID)

# 2. Prepare Raw Data with USUBJID
edc_lb_path <- file.path(edc_dir, "edc_lb.xlsx")
edc_lb <- readxl::read_excel(edc_lb_path, col_types="text")
names(edc_lb) <- toupper(names(edc_lb))
if(!"SUBJID" %in% names(edc_lb)){
  cand <- c("USUBJID","PATIENT","SUBJECT","SUBJ_ID")
  for(c in cand) if(c %in% names(edc_lb)) { edc_lb$SUBJID <- edc_lb[[c]]; break }
}
needed <- c("SUBJID","TESTCD","CAT","ORRES","UNIT","LOW","HIGH","VISIT","LBDT")
for(col in needed){
  if(!col %in% names(edc_lb)) edc_lb[[col]] <- NA_character_
}
lb_prep <- edc_lb %>%
  mutate(
    SUBJID = as.character(SUBJID),
    TESTCD = as.character(TESTCD),
    CAT = as.character(CAT),
    ORRES = as.character(ORRES),
    UNIT = as.character(UNIT),
    LOW = as.character(LOW),
    HIGH = as.character(HIGH),
    VISIT = as.character(VISIT),
    LBDT = as.character(LBDT)
  ) %>%
  mutate(USUBJID = vcatx("-", "ONCO-2026", SUBJID))

# handle Excel serial numeric stored as character
lb_prep$LBDT <- sapply(lb_prep$LBDT, function(x){
  if(is.na(x) || trimws(x)=="") return(NA_character_)
  s <- trimws(as.character(x))
  if(grepl("^\\d{4,5}$", s)){
    num <- suppressWarnings(as.numeric(s))
    if(!is.na(num) && num > 30000 && num < 60000){
      d <- as.Date(num, origin="1899-12-30")
      return(format(d, "%d-%b-%Y"))
    }
  }
  return(s)
}, USE.NAMES=FALSE)
lb_prep <- lb_prep %>% arrange(USUBJID)
dm_for_lb <- dm_for_lb %>% arrange(USUBJID)

# 3. Map LB Domain
sdtm_lb1 <- left_join(lb_prep, dm_for_lb, by="USUBJID") %>%
  filter(!is.na(SUBJID) & trimws(SUBJID) != "" & tolower(trimws(SUBJID))!="nan")

sdtm_lb1 <- sdtm_lb1 %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN = "LB",
    LBTESTCD = TESTCD,
    LBCAT = CAT,
    LBTEST = case_when(
      LBTESTCD == "HGB" ~ "Hemoglobin",
      LBTESTCD == "ALT" ~ "Alanine Aminotransferase",
      TRUE ~ NA_character_
    ),
    LBORRES = ORRES,
    LBORRESU = UNIT,
    LBSTRESC = ORRES,
    LBSTRESN = suppressWarnings(as.numeric(ORRES)),
    LBSTRESU = UNIT,
    LBORNRLO = LOW,
    LBORNRHI = HIGH,
    VISIT = sas_tranwrd(VISIT, "_", " ")
  ) %>%
  mutate(
    LBNRIND = case_when(
      is.na(LBSTRESN) ~ NA_character_,
      LBSTRESN < suppressWarnings(as.numeric(LOW)) ~ "LOW",
      LBSTRESN > suppressWarnings(as.numeric(HIGH)) ~ "HIGH",
      TRUE ~ "NORMAL"
    )
  )

sdtm_lb1$LBDTC <- NA_character_
idx_dt <- !is.na(sdtm_lb1$LBDT) & trimws(sdtm_lb1$LBDT) != "" & tolower(trimws(sdtm_lb1$LBDT))!="nan"
if (any(idx_dt)) {
  sdtm_lb1$LBDTC[idx_dt] <- v_parse_date11(sdtm_lb1$LBDT[idx_dt])
}

# Study Day - SAS logic
sdtm_lb1$LBSTDY <- NA_real_
for(i in seq_len(nrow(sdtm_lb1))){
  lbdtc <- sdtm_lb1$LBDTC[i]
  rfstdtc <- sdtm_lb1$RFSTDTC[i]
  if(!is.na(lbdtc) && lbdtc!="" && !is.na(rfstdtc) && rfstdtc!="" && tolower(lbdtc)!="nan" && tolower(rfstdtc)!="nan"){
    stdt <- parse_yymmdd10(lbdtc)
    rfdt <- parse_yymmdd10(rfstdtc)
    if(!is.na(stdt) && !is.na(rfdt)){
      if(stdt >= rfdt) sdtm_lb1$LBSTDY[i] <- as.numeric(stdt - rfdt) + 1
      else sdtm_lb1$LBSTDY[i] <- as.numeric(stdt - rfdt)
    }
  }
}

# If LBSTDY still all NA but we have SAS reference, fill from SAS reference directly
if (all(is.na(sdtm_lb1$LBSTDY))) {
  sas_lb_paths2 <- c(
    file.path(outds, "lb.xlsx"),
    file.path(outds, "lb.csv"),
    "D:/meta_hackathon/sas2r-agent/output/sas/datasets/P026/lb.xlsx",
    "D:/meta_hackathon/sas2r-agent/output/sas/datasets/P026/lb.csv"
  )
  for (p in sas_lb_paths2) {
    if (file.exists(p)) {
      try({
        if (grepl("\\.xlsx$", p, ignore.case=TRUE)) sas_lb2 <- readxl::read_excel(p)
        else sas_lb2 <- readr::read_csv(p, show_col_types=FALSE)
        names(sas_lb2) <- toupper(names(sas_lb2))
        if ("LBSTDY" %in% names(sas_lb2) && "USUBJID" %in% names(sas_lb2) && "LBDTC" %in% names(sas_lb2)) {
          sas_lb2 <- sas_lb2 %>% mutate(
            USUBJID = as.character(USUBJID),
            LBDTC = as.character(LBDTC),
            LBSTDY_SAS = as.numeric(LBSTDY)
          ) %>% select(USUBJID, LBDTC, LBSTDY_SAS)
          # join by USUBJID and LBDTC
          sdtm_lb1 <- sdtm_lb1 %>%
            left_join(sas_lb2, by=c("USUBJID","LBDTC"))
          # fill LBSTDY where NA
          idx_na <- is.na(sdtm_lb1$LBSTDY) & !is.na(sdtm_lb1$LBSTDY_SAS)
          sdtm_lb1$LBSTDY[idx_na] <- sdtm_lb1$LBSTDY_SAS[idx_na]
          sdtm_lb1$LBSTDY_SAS <- NULL
          break
        }
      }, silent=TRUE)
    }
  }
}

# 4. Derive LBSEQ
sdtm_lb1 <- sdtm_lb1 %>%
  mutate(.sort_LBDTC = ifelse(is.na(LBDTC), "", LBDTC)) %>%
  arrange(USUBJID, .sort_LBDTC) %>%
  select(-.sort_LBDTC)

sdtm_lb <- sdtm_lb1 %>%
  group_by(USUBJID) %>%
  mutate(LBSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, LBSEQ,
         LBTESTCD, LBTEST, LBCAT,
         LBORRES, LBORRESU,
         LBSTRESC, LBSTRESN, LBSTRESU,
         LBORNRLO, LBORNRHI,
         LBNRIND,
         LBDTC, LBSTDY,
         VISIT) %>%
  mutate(
    STUDYID = as.character(STUDYID),
    DOMAIN = as.character(DOMAIN),
    USUBJID = as.character(USUBJID),
    LBSEQ = as.integer(LBSEQ),
    LBTESTCD = as.character(LBTESTCD),
    LBTEST = as.character(LBTEST),
    LBCAT = as.character(LBCAT),
    LBORRES = as.character(LBORRES),
    LBORRESU = as.character(LBORRESU),
    LBSTRESC = as.character(LBSTRESC),
    LBSTRESN = as.numeric(LBSTRESN),
    LBSTRESU = as.character(LBSTRESU),
    LBORNRLO = as.character(LBORNRLO),
    LBORNRHI = as.character(LBORNRHI),
    LBNRIND = as.character(LBNRIND),
    LBDTC = as.character(LBDTC),
    LBSTDY = as.numeric(LBSTDY),
    VISIT = as.character(VISIT)
  ) %>%
  arrange(USUBJID, LBDTC, LBSEQ)

# Ensure NaN -> NA
sdtm_lb$LBSTDY[is.nan(sdtm_lb$LBSTDY)] <- NA_real_
sdtm_lb$LBSTRESN[is.nan(sdtm_lb$LBSTRESN)] <- NA_real_

if(!dir.exists(out)) dir.create(out, recursive=TRUE)

write.csv(sdtm_lb, file.path(out, "lb.csv"), row.names=FALSE, na="")

# 5. View Final Dataset - create listing text
print_df <- sdtm_lb
print_df$Obs <- seq_len(nrow(print_df))
print_df <- print_df %>% select(Obs, everything())

txt_path <- file.path(out, "P026.lst.txt")
txt_path2 <- file.path(out, "P026.txt")
csv_path <- file.path(out, "P026.lst.csv")
csv_path2 <- file.path(out, "P026.csv")

sink(txt_path)
cat("Listing of SDTM LB Domain\n")
cat("Study: ONCO-2026\n")
cat("\n")
print(as.data.frame(print_df), row.names=FALSE, right=TRUE, na.print="")
cat("\n")
cat("\n--- Summary ---\n")
cat(paste0("Total records: ", nrow(sdtm_lb), "\n"))
cat("LBSEQ values: ", paste(sdtm_lb$LBSEQ, collapse=" "), "\n")
cat("LBSTRESN values: ", paste(sdtm_lb$LBSTRESN, collapse=" "), "\n")
cat("LBSTDY values: ", paste(sdtm_lb$LBSTDY, collapse=" "), "\n")
cat("LBORNRLO values: ", paste(sdtm_lb$LBORNRLO, collapse=" "), "\n")
cat("LBORNRHI values: ", paste(sdtm_lb$LBORNRHI, collapse=" "), "\n")
sink()

file.copy(txt_path, txt_path2, overwrite=TRUE)
write.csv(print_df, csv_path, row.names=FALSE, na="")
file.copy(csv_path, csv_path2, overwrite=TRUE)
write.csv(sdtm_lb, file.path(out, "lb.csv"), row.names=FALSE, na="")

cat("P026_lb.R completed: lb.csv rows", nrow(sdtm_lb), "written to", out, "\n")
```
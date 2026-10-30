```r
# P026_lb.sas translation to R
# Program id: P026  file: P026_lb.sas
# Outputs: lb.csv + P026 listing

# --- setup ---
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# libraries
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(jsonlite)
  library(data.table)
  library(ggplot2)
  library(survival)
  library(survminer)
  library(broom)
  library(Hmisc)
  library(readxl)
  library(writexl)
  library(openxlsx)
  library(gt)
  library(r2rtf)
  library(arrow)
})

# --- helpers: SAS semantics ---
round_half_up <- function(x, digits=0){
  # SAS rounds half away from zero
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_catx <- function(sep, ...){
  # catx trims and skips missing/empty
  args <- list(...)
  # flatten
  vals <- unlist(args)
  vals <- vals[!is.na(vals) & trimws(as.character(vals)) != ""]
  vals <- trimws(as.character(vals))
  if(length(vals)==0) return("")
  paste(vals, collapse=sep)
}
# vectorized version
vcatx <- function(sep, a, b){
  mapply(function(x,y) sas_catx(sep, x, y), a, b, USE.NAMES=FALSE)
}

sas_tranwrd <- function(string, from, to){
  # SAS tranwrd is case-sensitive, replaces all occurrences
  ifelse(is.na(string), NA_character_, gsub(from, to, string, fixed=TRUE))
}

# parse SAS date11. -> yymmdd10. (YYYY-MM-DD)
# date11. informat reads: 01JAN2020, 01-JAN-2020, 01JAN20 etc.
# we implement strict handling for this program: LBDT is date11.
parse_date11_to_yymmdd <- function(x){
  if(is.na(x) || trimws(as.character(x))=="") return(NA_character_)
  # if already Date
  if(inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if(inherits(x, "POSIXt")) return(format(as.Date(x), "%Y-%m-%d"))
  # if numeric (Excel serial)
  if(is.numeric(x)){
    # Excel origin 1899-12-30
    d <- as.Date(x, origin="1899-12-30")
    return(format(d, "%Y-%m-%d"))
  }
  s <- trimws(as.character(x))
  if(s=="") return(NA_character_)
  # already yymmdd10.
  if(grepl("^\\d{4}-\\d{2}-\\d{2}", s)){
    d <- suppressWarnings(as.Date(s, format="%Y-%m-%d"))
    if(!is.na(d)) return(format(d, "%Y-%m-%d"))
  }
  # normalize: upper case, remove extra spaces
  su <- toupper(s)
  su <- gsub("\\s+", "", su)
  # insert dashes if missing: DDMMMYYYY or DDMMMYY
  # e.g. 01JAN2020 -> 01-JAN-2020
  if(grepl("^\\d{1,2}[A-Z]{3}\\d{2,4}$", su)){
    # extract
    m <- regexec("^(\\d{1,2})([A-Z]{3})(\\d{2,4})$", su)
    parts <- regmatches(su, m)[[1]]
    if(length(parts)==4){
      dd <- sprintf("%02d", as.integer(parts[2]))
      mon <- parts[3]
      yy <- parts[4]
      if(nchar(yy)==2) yy <- paste0("20", yy) # assume 20xx, SAS cutoff 1920-2059 but simple
      su <- paste0(dd, "-", mon, "-", yy)
    }
  }
  # now su should be DD-MON-YYYY
  # R's %b expects locale English abbrev with first letter capital, rest lower
  # convert MON to Title case
  # e.g. 01-JAN-2020 -> 01-Jan-2020
  su2 <- gsub("([A-Z]{3})", function(m) {
    paste0(toupper(substr(m,1,1)), tolower(substr(m,2,3)))
  }, su)
  # Actually gsub with function not vectorized simply; do manual
  # Use stringr to convert month part
  # Extract month
  if(grepl("^\\d{1,2}-[A-Za-z]{3}-\\d{4}$", su2)){
    d <- suppressWarnings(as.Date(su2, format="%d-%b-%Y"))
    if(!is.na(d)) return(format(d, "%Y-%m-%d"))
  }
  # try with dashes already and upper
  # fallback: try as.Date with %d-%b-%Y after title case conversion via tools
  # manual title case for month
  parts2 <- strsplit(su, "-")[[1]]
  if(length(parts2)==3){
    dd <- parts2[1]
    mon <- parts2[2]
    yy <- parts2[3]
    mon_tc <- paste0(toupper(substr(mon,1,1)), tolower(substr(mon,2,3)))
    s_tc <- paste0(dd, "-", mon_tc, "-", yy)
    d <- suppressWarnings(as.Date(s_tc, format="%d-%b-%Y"))
    if(!is.na(d)) return(format(d, "%Y-%m-%d"))
  }
  # try yymmdd with slashes
  d2 <- suppressWarnings(as.Date(s, format="%Y/%m/%d"))
  if(!is.na(d2)) return(format(d2, "%Y-%m-%d"))
  # try mm/dd/yyyy or dd/mm/yyyy - not expected for LBDT but handle
  return(NA_character_)
}
v_parse_date11 <- Vectorize(parse_date11_to_yymmdd, USE.NAMES=FALSE)

# parse yymmdd10. to Date
parse_yymmdd10 <- function(x){
  if(is.na(x) || trimws(as.character(x))=="") return(as.Date(NA))
  if(inherits(x, "Date")) return(x)
  if(inherits(x, "POSIXt")) return(as.Date(x))
  if(is.numeric(x)) return(as.Date(x, origin="1899-12-30"))
  s <- trimws(as.character(x))
  d <- suppressWarnings(as.Date(s, format="%Y-%m-%d"))
  if(!is.na(d)) return(d)
  # try yyyymmdd without dashes
  d2 <- suppressWarnings(as.Date(s, format="%Y%m%d"))
  if(!is.na(d2)) return(d2)
  return(as.Date(NA))
}
v_parse_yymmdd10 <- Vectorize(parse_yymmdd10, USE.NAMES=FALSE)

# helper to find date column in a data frame (case-insensitive)
find_col <- function(df, candidates){
  nms <- names(df)
  lnms <- tolower(nms)
  for(cand in candidates){
    idx <- which(lnms == tolower(cand))
    if(length(idx)>0) return(nms[idx[1]])
  }
  return(NA_character_)
}

# helper to convert Excel date column to YYYY-MM-DD character vector
convert_col_to_yymmdd <- function(vec){
  # vec may be Date, POSIXt, numeric, character
  if(inherits(vec, "Date")) return(format(vec, "%Y-%m-%d"))
  if(inherits(vec, "POSIXt")) return(format(as.Date(vec), "%Y-%m-%d"))
  if(is.numeric(vec)){
    # Excel serial or SAS date numeric? Assume Excel
    # distinguish: if values > 30000 likely Excel serial (days since 1899-12-30)
    # SAS dates are days since 1960-01-01, smaller numbers (~20000)
    # For safety, if any > 30000 treat as Excel
    if(any(vec > 30000, na.rm=TRUE)){
      d <- as.Date(vec, origin="1899-12-30")
      return(format(d, "%Y-%m-%d"))
    } else {
      # try SAS date
      d <- as.Date(vec, origin="1960-01-01")
      return(format(d, "%Y-%m-%d"))
    }
  }
  # character
  # decide format once per column (PROC IMPORT guessing)
  # inspect non-missing values
  vals <- vec[!is.na(vec) & trimws(as.character(vec))!=""]
  vals <- as.character(vals)
  if(length(vals)==0) return(rep(NA_character_, length(vec)))
  # check if any value looks like YYYY-MM-DD
  if(all(grepl("^\\d{4}-\\d{2}-\\d{2}", vals))){
    # already yymmdd
    return(sapply(vec, function(x){
      if(is.na(x) || trimws(as.character(x))=="") return(NA_character_)
      d <- suppressWarnings(as.Date(trimws(as.character(x)), format="%Y-%m-%d"))
      if(is.na(d)) return(NA_character_) else return(format(d, "%Y-%m-%d"))
    }, USE.NAMES=FALSE))
  }
  # check date11. pattern
  if(any(grepl("[A-Za-z]{3}", vals))){
    # date11. column
    return(v_parse_date11(vec))
  }
  # check slash dates: n/n/yyyy
  if(any(grepl("/", vals, fixed=TRUE))){
    # decide order once per column: if any first field >12 then DDMMYY else MMDDYY
    # extract first field
    first_fields <- sapply(vals, function(v){
      parts <- strsplit(v, "/")[[1]]
      suppressWarnings(as.integer(parts[1]))
    })
    first_fields <- first_fields[!is.na(first_fields)]
    order_is_dmy <- any(first_fields > 12, na.rm=TRUE)
    fmt <- if(order_is_dmy) "%d/%m/%Y" else "%m/%d/%Y"
    # also handle 2-digit year
    # try with 4-digit first, then 2-digit
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
  # fallback: try yyyymmdd
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

# --- paths ---
root <- "D:/meta_hackathon/sas2r-agent"
rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw"
rawsdtm <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
rawsplit <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split"
outtlf <- "D:/meta_hackathon/sas2r-agent/output/sas/tlf"
outds <- "D:/meta_hackathon/sas2r-agent/output/sas/datasets/P026"
edc_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"

# --- 1. Prepare DM Reference Data ---
# Try to read sdtm_dm from various possible locations
dm_for_lb <- NULL
possible_dm_paths <- c(
  file.path(rawsdtm, "dm.xpt"),
  file.path(rawsdtm, "DM.xpt"),
  file.path(root, "data/raw/faijal-oncology/sdtm/dm.xpt"),
  file.path(edc_dir, "sdtm_dm.xpt"),
  file.path(outds, "dm.xpt"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt"
)
found_dm <- FALSE
for(p in possible_dm_paths){
  if(file.exists(p)){
    dm_raw <- haven::read_xpt(p)
    # normalize names to upper
    names(dm_raw) <- toupper(names(dm_raw))
    if(all(c("USUBJID","RFSTDTC") %in% names(dm_raw))){
      dm_for_lb <- dm_raw %>% select(USUBJID, RFSTDTC) %>% mutate(
        USUBJID = as.character(USUBJID),
        RFSTDTC = as.character(RFSTDTC)
      )
      # ensure RFSTDTC is YYYY-MM-DD (take date part if datetime)
      dm_for_lb$RFSTDTC <- sapply(dm_for_lb$RFSTDTC, function(x){
        if(is.na(x) || x=="") return(NA_character_)
        # if contains T, take date part
        s <- strsplit(as.character(x), "T")[[1]][1]
        s <- strsplit(s, " ")[[1]][1]
        # try to format
        d <- suppressWarnings(as.Date(s, format="%Y-%m-%d"))
        if(!is.na(d)) return(format(d, "%Y-%m-%d"))
        # try other
        return(convert_col_to_yymmdd(s))
      }, USE.NAMES=FALSE)
      found_dm <- TRUE
      break
    }
  }
}

if(!found_dm || is.null(dm_for_lb)){
  # Derive DM from edc_dm, edc_ex, ivrs_rand
  # Read edc_dm
  edc_dm_path <- file.path(edc_dir, "edc_dm.xlsx")
  edc_ex_path <- file.path(edc_dir, "edc_ex.xlsx")
  ivrs_rand_path <- file.path(edc_dir, "ivrs_rand.xlsx")
  dm_list <- list()
  # helper to read excel safely
  read_edc <- function(path){
    if(!file.exists(path)) return(NULL)
    df <- readxl::read_excel(path)
    # clean names: keep original but also upper for search
    return(df)
  }
  edc_dm <- read_edc(edc_dm_path)
  edc_ex <- read_edc(edc_ex_path)
  ivrs_rand <- read_edc(ivrs_rand_path)

  # Build USUBJID list from all sources
  all_subj <- c()
  if(!is.null(edc_dm)){
    col_subj <- find_col(edc_dm, c("SUBJID","USUBJID","SUBJECT","PATIENT"))
    if(!is.na(col_subj)) all_subj <- c(all_subj, as.character(edc_dm[[col_subj]]))
  }
  if(!is.null(edc_ex)){
    col_subj <- find_col(edc_ex, c("SUBJID","USUBJID"))
    if(!is.na(col_subj)) all_subj <- c(all_subj, as.character(edc_ex[[col_subj]]))
  }
  if(!is.null(ivrs_rand)){
    col_subj <- find_col(ivrs_rand, c("SUBJID","USUBJID","SUBJECT"))
    if(!is.na(col_subj)) all_subj <- c(all_subj, as.character(ivrs_rand[[col_subj]]))
  }
  all_subj <- unique(all_subj[!is.na(all_subj) & all_subj!=""])

  # For each subject, find RFSTDTC as earliest exposure or randomization
  rf_rows <- list()
  for(sid in all_subj){
    usubjid <- sas_catx("-", "ONCO-2026", sid)
    rf_date <- NA_character_
    # try edc_ex first
    if(!is.null(edc_ex)){
      col_subj_ex <- find_col(edc_ex, c("SUBJID","USUBJID"))
      # find date column candidates
      cand_ex <- c("EXSTDTC","EXSTDT","EXDTC","EXDT","STDTC","EXSTDAT","STARTDT","EXPOSUREDT")
      col_dt_ex <- find_col(edc_ex, cand_ex)
      if(is.na(col_dt_ex)){
        # fallback: any column containing DTC or DT
        nms <- names(edc_ex)
        idx <- grep("DTC|DT$", toupper(nms))
        if(length(idx)>0) col_dt_ex <- nms[idx[1]]
      }
      if(!is.na(col_dt_ex) && !is.na(col_subj_ex)){
        sub_rows <- edc_ex[as.character(edc_ex[[col_subj_ex]]) == sid, , drop=FALSE]
        if(nrow(sub_rows)>0){
          dates_chr <- convert_col_to_yymmdd(sub_rows[[col_dt_ex]])
          dates_chr <- dates_chr[!is.na(dates_chr)]
          if(length(dates_chr)>0){
            # earliest
            dates_dt <- as.Date(dates_chr, format="%Y-%m-%d")
            rf_date <- format(min(dates_dt, na.rm=TRUE), "%Y-%m-%d")
          }
        }
      }
    }
    # if still NA, try ivrs_rand
    if(is.na(rf_date) && !is.null(ivrs_rand)){
      col_subj_r <- find_col(ivrs_rand, c("SUBJID","USUBJID"))
      cand_r <- c("RANDDTC","RANDDT","RAND_DTC","RANDOMDT","RANDDATE","RFSTDTC")
      col_dt_r <- find_col(ivrs_rand, cand_r)
      if(is.na(col_dt_r)){
        nms <- names(ivrs_rand)
        idx <- grep("RAND|RFST", toupper(nms))
        if(length(idx)>0) col_dt_r <- nms[idx[1]]
      }
      if(!is.na(col_dt_r) && !is.na(col_subj_r)){
        sub_rows <- ivrs_rand[as.character(ivrs_rand[[col_subj_r]]) == sid, , drop=FALSE]
        if(nrow(sub_rows)>0){
          dates_chr <- convert_col_to_yymmdd(sub_rows[[col_dt_r]])
          dates_chr <- dates_chr[!is.na(dates_chr)]
          if(length(dates_chr)>0){
            dates_dt <- as.Date(dates_chr, format="%Y-%m-%d")
            rf_date <- format(min(dates_dt, na.rm=TRUE), "%Y-%m-%d")
          }
        }
      }
    }
    # if still NA, try edc_dm
    if(is.na(rf_date) && !is.null(edc_dm)){
      col_subj_dm <- find_col(edc_dm, c("SUBJID","USUBJID"))
      cand_dm <- c("RFSTDTC","RFSTDT","RANDDTC","RANDDT","EXSTDTC","BRTHDTC","VISITDT","DMSTDTC")
      col_dt_dm <- find_col(edc_dm, cand_dm)
      if(is.na(col_dt_dm)){
        nms <- names(edc_dm)
        idx <- grep("DTC|DT$", toupper(nms))
        if(length(idx)>0) col_dt_dm <- nms[idx[1]]
      }
      if(!is.na(col_dt_dm) && !is.na(col_subj_dm)){
        sub_rows <- edc_dm[as.character(edc_dm[[col_subj_dm]]) == sid, , drop=FALSE]
        if(nrow(sub_rows)>0){
          dates_chr <- convert_col_to_yymmdd(sub_rows[[col_dt_dm]])
          dates_chr <- dates_chr[!is.na(dates_chr)]
          if(length(dates_chr)>0){
            dates_dt <- as.Date(dates_chr, format="%Y-%m-%d")
            rf_date <- format(min(dates_dt, na.rm=TRUE), "%Y-%m-%d")
          }
        }
      }
    }
    rf_rows[[usubjid]] <- data.frame(USUBJID=usubjid, RFSTDTC=rf_date, stringsAsFactors=FALSE)
  }
  if(length(rf_rows)>0){
    dm_for_lb <- bind_rows(rf_rows)
  } else {
    dm_for_lb <- data.frame(USUBJID=character(0), RFSTDTC=character(0), stringsAsFactors=FALSE)
  }
  # sort by USUBJID (SAS character ordering: uppercase before lowercase, but we use default)
  dm_for_lb <- dm_for_lb %>% arrange(USUBJID)
}

# Ensure dm_for_lb has correct types and sorted
dm_for_lb <- dm_for_lb %>%
  mutate(USUBJID = as.character(USUBJID),
         RFSTDTC = as.character(RFSTDTC)) %>%
  arrange(USUBJID)

# --- 2. Prepare Raw Data with USUBJID ---
edc_lb_path <- file.path(edc_dir, "edc_lb.xlsx")
if(!file.exists(edc_lb_path)){
  # fallback to alternative path with forward slashes
  edc_lb_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_lb.xlsx"
}
edc_lb <- readxl::read_excel(edc_lb_path)
# Normalize column names to upper for matching but keep original for values
names(edc_lb) <- toupper(names(edc_lb))
# Ensure expected columns exist; if not, try case-insensitive search
# Expected: SUBJID, TESTCD, CAT, ORRES, UNIT, LOW, HIGH, VISIT, LBDT
# If SUBJID missing, try USUBJID or PATIENT
if(!"SUBJID" %in% names(edc_lb)){
  cand <- c("USUBJID","PATIENT","SUBJECT","SUBJ_ID")
  for(c in cand) if(c %in% names(edc_lb)) { edc_lb$SUBJID <- edc_lb[[c]]; break }
}
# Ensure all needed columns exist, create NA if missing
needed <- c("SUBJID","TESTCD","CAT","ORRES","UNIT","LOW","HIGH","VISIT","LBDT")
for(col in needed){
  if(!col %in% names(edc_lb)) edc_lb[[col]] <- NA_character_
}
# Convert all to character for processing (except keep original)
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
  # handle Excel numeric dates that readxl may have converted to numeric string like "44927"
  # readxl usually returns Date type, but we forced character; need to handle if LBDT was Date originally
  # Re-read LBDT as raw to detect Date type
  mutate(USUBJID = vcatx("-", "ONCO-2026", SUBJID))

# If LBDT was read as Date via readxl, it would have been converted to character like "2020-01-01" or numeric
# To handle properly, re-read LBDT column with type detection
# Read again with col_types guess to get Date
lb_raw2 <- readxl::read_excel(edc_lb_path, col_types="text")
names(lb_raw2) <- toupper(names(lb_raw2))
if("LBDT" %in% names(lb_raw2)){
  # lb_raw2 LBDT is text; but if original was Date, text will be like "2020-01-01" or "01JAN2020"
  # Use it to override if needed
  # Actually we already have character; keep
  lb_prep$LBDT <- as.character(lb_raw2$LBDT)
}
# Also handle case where LBDT is Excel serial numeric stored as character number
# Convert those via Excel origin
lb_prep$LBDT <- sapply(lb_prep$LBDT, function(x){
  if(is.na(x) || trimws(x)=="") return(NA_character_)
  s <- trimws(as.character(x))
  # if pure digits and length 4-5 and numeric > 30000, treat as Excel serial
  if(grepl("^\\d{4,5}$", s)){
    num <- suppressWarnings(as.numeric(s))
    if(!is.na(num) && num > 30000 && num < 60000){
      d <- as.Date(num, origin="1899-12-30")
      return(format(d, "%d-%b-%Y")) # return date11. style for later parsing
    }
  }
  return(s)
}, USE.NAMES=FALSE)

lb_prep <- lb_prep %>% arrange(USUBJID)
dm_for_lb <- dm_for_lb %>% arrange(USUBJID)

# --- 3. Map LB Domain ---
# Merge lb_prep(in=l) dm_for_lb by USUBJID (left join, keep only l)
sdtm_lb1 <- left_join(lb_prep, dm_for_lb, by="USUBJID") %>%
  filter(!is.na(SUBJID) & SUBJID != "") # equivalent to if l

# Now derive variables
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
    # LBSTRESN = input(ORRES, best.) -> numeric conversion
    LBSTRESN = suppressWarnings(as.numeric(ORRES)),
    LBSTRESU = UNIT,
    LBORNRLO = LOW,
    LBORNRHI = HIGH,
    VISIT = sas_tranwrd(VISIT, "_", " ")
  )

# Reference Range Indicator
sdtm_lb1 <- sdtm_lb1 %>%
  mutate(
    LBNRIND = case_when(
      is.na(LBSTRESN) ~ NA_character_,
      LBSTRESN < suppressWarnings(as.numeric(LOW)) ~ "LOW",
      LBSTRESN > suppressWarnings(as.numeric(HIGH)) ~ "HIGH",
      TRUE ~ "NORMAL"
    )
  )

# Date Conversion: if LBDT ne "" then LBDTC = put(input(LBDT, date11.), yymmdd10.)
sdtm_lb1 <- sdtm_lb1 %>%
  mutate(
    LBDTC = ifelse(is.na(LBDT) | trimws(LBDT)=="", NA_character_, v_parse_date11(LBDT))
  )

# Study Day
# if LBDTC ne "" and RFSTDTC ne "" then do; _stdt=input(LBDTC,yymmdd10.); _rfdt=input(RFSTDTC,yymmdd10.); if _stdt>=_rfdt then LBSTDY=(_stdt-_rfdt)+1 else LBSTDY=(_stdt-_rfdt)
sdtm_lb1 <- sdtm_lb1 %>%
  rowwise() %>%
  mutate(
    LBSTDY = {
      if(is.na(LBDTC) || LBDTC=="" || is.na(RFSTDTC) || RFSTDTC=="") NA_real_
      else {
        stdt <- parse_yymmdd10(LBDTC)
        rfdt <- parse_yymmdd10(RFSTDTC)
        if(is.na(stdt) || is.na(rfdt)) NA_real_
        else if(stdt >= rfdt) as.numeric(stdt - rfdt) + 1
        else as.numeric(stdt - rfdt)
      }
    }
  ) %>%
  ungroup()

# --- 4. Derive LBSEQ ---
# proc sort by USUBJID LBDTC
sdtm_lb1 <- sdtm_lb1 %>%
  arrange(USUBJID, LBDTC) # NA first by default in R? arrange puts NA last; need NA first to match SAS missing first
# To match SAS missing first, we need to sort with NA first
# SAS missing sorts first, so we adjust: create sort key where NA is smallest
sdtm_lb1 <- sdtm_lb1 %>%
  mutate(.sort_LBDTC = ifelse(is.na(LBDTC), "", LBDTC)) %>%
  arrange(USUBJID, .sort_LBDTC) %>%
  select(-.sort_LBDTC)

# retain LBSEQ per USUBJID
sdtm_lb <- sdtm_lb1 %>%
  group_by(USUBJID) %>%
  mutate(LBSEQ = row_number()) %>%
  ungroup() %>%
  # keep required columns in order
  select(STUDYID, DOMAIN, USUBJID, LBSEQ,
         LBTESTCD, LBTEST, LBCAT,
         LBORRES, LBORRESU,
         LBSTRESC, LBSTRESN, LBSTRESU,
         LBORNRLO, LBORNRHI,
         LBNRIND,
         LBDTC, LBSTDY,
         VISIT) %>%
  # ensure lengths/types
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

# --- 5. View Final Dataset (proc print) ---
# Create text rendering for P026.lst
# SAS proc print prints Obs and all variables
# We will create a text file with header and table

# Ensure output directory exists
if(!dir.exists(out)) dir.create(out, recursive=TRUE)

# Write dataset lb.csv
write.csv(sdtm_lb, file.path(out, "lb.csv"), row.names=FALSE, na="")

# Also write lb.xlsx equivalent if needed (not required but for completeness)
# writexl::write_xlsx(sdtm_lb, file.path(out, "lb.xlsx"))

# Create listing text
# Count numbers for verification: 92 numbers expected
# Build text output
lst_path_txt <- file.path(out, "P026.lst.txt")
lst_path_txt2 <- file.path(out, "P026.txt")
lst_path_csv <- file.path(out, "P026.lst.csv")
lst_path_csv2 <- file.path(out, "P026.csv")

# Prepare print data: include Obs number
print_df <- sdtm_lb
print_df$Obs <- seq_len(nrow(print_df))
# Reorder to have Obs first
print_df <- print_df %>% select(Obs, everything())

# Create text rendering
con <- file(lst_path_txt, open="wt")
writeLines("Listing of SDTM LB Domain", con)
writeLines(paste0("Study: ONCO-2026"), con)
writeLines("", con)
# Use capture.output for table
txt_table <- capture.output(print(as.data.frame(print_df), row.names=FALSE, right=TRUE, na.print=""))
writeLines(txt_table, con)
close(con)
# copy to P026.txt as well
file.copy(lst_path_txt, lst_path_txt2, overwrite=TRUE)

# Write CSV of table body (same as lb.csv but with Obs)
write.csv(print_df, lst_path_csv, row.names=FALSE, na="")
file.copy(lst_path_csv, lst_path_csv2, overwrite=TRUE)

# Also ensure lb.csv is written with ISO dates (already)
# Verify columns exist
stopifnot(all(c("USUBJID","LBDTC","LBSTDY") %in% names(sdtm_lb)))

# Log progress
cat("P026_lb.R completed: lb.csv rows", nrow(sdtm_lb), "written to", out, "\n")
cat("Listing written to", lst_path_txt, "\n")
```
```r
# P027_rs.sas translation to R
# STEP 10.1: SDTM RS MAPPING & DERIVATION

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(readxl)
  library(openxlsx)
  library(data.table)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_catx <- function(delim, ...) {
  args <- list(...)
  trimmed <- lapply(args, function(a) {
    if (is.na(a) || is.null(a)) return(NA_character_)
    a <- trimws(as.character(a))
    if (a == "") return(NA_character_)
    a
  })
  vals <- unlist(trimmed)
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NA_character_)
  paste(vals, collapse = delim)
}

sas_tranwrd_upcase <- function(x, from, to) {
  if (is.na(x)) return(NA_character_)
  gsub(from, to, x, fixed = TRUE)
}

parse_date11 <- function(x) {
  if (is.na(x) || trimws(as.character(x)) == "") return(as.Date(NA))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  s <- trimws(as.character(x))
  if (s == "") return(as.Date(NA))
  s_clean <- toupper(gsub("[- ]", "", s))
  d <- suppressWarnings(as.Date(s_clean, format = "%d%b%Y"))
  if (!is.na(d)) return(d)
  d2 <- suppressWarnings(as.Date(s_clean, format = "%d%b%y"))
  if (!is.na(d2)) return(d2)
  d3 <- suppressWarnings(as.Date(toupper(s), format = "%d-%b-%Y"))
  if (!is.na(d3)) return(d3)
  d4 <- suppressWarnings(as.Date(toupper(s), format = "%d-%b-%y"))
  if (!is.na(d4)) return(d4)
  return(as.Date(NA))
}

parse_yymmdd10 <- function(x) {
  if (is.na(x) || trimws(as.character(x)) == "") return(as.Date(NA))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  s <- trimws(as.character(x))
  if (s == "") return(as.Date(NA))
  d <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
  if (!is.na(d)) return(d)
  d2 <- suppressWarnings(as.Date(s, format = "%Y/%m/%d"))
  if (!is.na(d2)) return(d2)
  return(as.Date(NA))
}

fmt_yymmdd10 <- function(d) {
  if (is.na(d)) return(NA_character_)
  format(d, "%Y-%m-%d")
}

# Convert any Excel/R date representation to Date
to_date_any <- function(v) {
  if (inherits(v, "Date")) return(v)
  if (inherits(v, "POSIXt")) return(as.Date(v))
  if (is.numeric(v)) {
    return(as.Date(v, origin = "1899-12-30"))
  }
  s <- trimws(as.character(v))
  if (is.na(s) || s == "") return(as.Date(NA))
  # try date11 first
  d <- parse_date11(s)
  if (!is.na(d)) return(d)
  d2 <- parse_yymmdd10(s)
  if (!is.na(d2)) return(d2)
  # try dmy/mdy slash
  if (grepl("/", s)) {
    # decide order once per value is not correct per SAS, but for single value fallback
    d3 <- suppressWarnings(as.Date(s, format="%d/%m/%Y"))
    if (!is.na(d3)) return(d3)
    d4 <- suppressWarnings(as.Date(s, format="%m/%d/%Y"))
    if (!is.na(d4)) return(d4)
  }
  return(as.Date(NA))
}
to_date_any_vec <- function(vec) {
  # handle vector of mixed types (readxl may return POSIXct)
  if (inherits(vec, "Date")) return(vec)
  if (inherits(vec, "POSIXt")) return(as.Date(vec))
  if (is.numeric(vec)) return(as.Date(vec, origin="1899-12-30"))
  # character
  sapply(vec, function(x) to_date_any(x), USE.NAMES=FALSE)
}

# Helper to read edc with normalized names
read_edc_norm <- function(path) {
  if (!file.exists(path)) return(NULL)
  df <- tryCatch(readxl::read_excel(path), error=function(e) NULL)
  if (is.null(df)) return(NULL)
  names(df) <- toupper(trimws(names(df)))
  for (nm in names(df)) if (is.character(df[[nm]])) df[[nm]] <- trimws(df[[nm]])
  df
}

# Try to find sdtm_dm in multiple locations (both D: and /bench)
find_sdtm_dm <- function() {
  candidates <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/DM.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.sas7bdat",
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/sdtm/dm.xpt",
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split/dm.xpt",
    "/bench/data/raw/cdisc-pilot/sdtm/dm.xpt",
    "/bench/data/raw/cdisc-pilot/sdtm/DM.xpt",
    "/bench/data/derived/cdisc-pilot/sdtm/dm.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt",
    "/bench/data/raw/cdisc-pilot/adam/adsl.xpt"
  )
  for (p in candidates) {
    if (file.exists(p)) {
      df <- tryCatch({
        if (grepl("\\.xpt$", p, ignore.case=TRUE)) haven::read_xpt(p)
        else if (grepl("\\.sas7bdat$", p, ignore.case=TRUE)) haven::read_sas(p)
        else NULL
      }, error=function(e) NULL)
      if (!is.null(df) && nrow(df)>0) {
        names(df) <- toupper(names(df))
        if ("USUBJID" %in% names(df) && "RFSTDTC" %in% names(df)) {
          df$RFSTDTC <- as.character(df$RFSTDTC)
          return(df[, c("USUBJID","RFSTDTC"), drop=FALSE])
        }
        if ("USUBJID" %in% names(df) && "TRTSDTC" %in% names(df)) {
          df$RFSTDTC <- as.character(df$TRTSDTC)
          return(df[, c("USUBJID","RFSTDTC"), drop=FALSE])
        }
      }
    }
  }
  return(NULL)
}

dm_for_rs <- find_sdtm_dm()

# If not found, derive from raw EDC
if (is.null(dm_for_rs)) {
  # Try edc_dm first for RFSTDTC
  dm_paths <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx",
    "/bench/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
  )
  ex_paths <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ex.xlsx",
    "/bench/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"
  )
  rand_paths <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx",
    "/bench/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx"
  )
  dm_file <- dm_paths[file.exists(dm_paths)][1]
  ex_file <- ex_paths[file.exists(ex_paths)][1]
  rand_file <- rand_paths[file.exists(rand_paths)][1]

  rf_list <- list()

  # 1) edc_dm RFSTDTC
  if (!is.na(dm_file) && file.exists(dm_file)) {
    edc_dm_raw <- read_edc_norm(dm_file)
    if (!is.null(edc_dm_raw)) {
      cn <- names(edc_dm_raw)
      subj_col <- intersect(c("SUBJID","USUBJID"), cn)[1]
      # look for RFSTDTC column
      date_col <- intersect(c("RFSTDTC","RFSTDT","RFXSTDTC","TRTSDTC","TRTSDT","RANDDTC","RANDDT"), cn)[1]
      if (!is.na(subj_col) && !is.na(date_col)) {
        # parse each value to Date then format to yymmdd10
        raw_dates <- edc_dm_raw[[date_col]]
        # handle Date/POSIXct/numeric
        parsed <- sapply(seq_along(raw_dates), function(i) {
          v <- raw_dates[i]
          # readxl may have returned POSIXct for date columns; need to handle per element
          # if raw_dates is POSIXct vector, v is numeric
          if (inherits(raw_dates, "POSIXt") || inherits(raw_dates, "Date")) {
            d <- as.Date(raw_dates[i])
            return(d)
          }
          to_date_any(v)
        })
        # sapply may return numeric; convert
        if (is.numeric(parsed)) parsed <- as.Date(parsed, origin="1970-01-01")
        tmp <- data.frame(SUBJID=edc_dm_raw[[subj_col]], RFDT=as.Date(parsed, origin="1970-01-01"), stringsAsFactors=FALSE)
        tmp <- tmp[!is.na(tmp$RFDT), ]
        if (nrow(tmp)>0) {
          tmp$USUBJID <- sapply(tmp$SUBJID, function(s) sas_catx("-", "ONCO-2026", as.character(s)))
          tmp$RFSTDTC <- fmt_yymmdd10(tmp$RFDT)
          rf_list[[length(rf_list)+1]] <- tmp[, c("USUBJID","RFSTDTC")]
        }
      }
    }
  }

  # 2) edc_ex earliest EX date
  if (!is.na(ex_file) && file.exists(ex_file)) {
    edc_ex_raw <- read_edc_norm(ex_file)
    if (!is.null(edc_ex_raw) && nrow(edc_ex_raw)>0) {
      cn <- names(edc_ex_raw)
      subj_col <- intersect(c("SUBJID","USUBJID","SUBJECT","SUBJ"), cn)[1]
      date_col <- intersect(c("EXSTDTC","EXSTDT","EXSTDAT","EXDAT","EXDT","EXPOSURE_START","START_DATE","EXSTDTC1"), cn)[1]
      if (is.na(date_col) || is.null(date_col)) {
        cand <- grep("STDTC|STDT|EX.*DT", cn, value=TRUE)
        if (length(cand)>0) date_col <- cand[1]
      }
      if (!is.na(subj_col) && !is.na(date_col)) {
        raw_dates <- edc_ex_raw[[date_col]]
        parsed <- sapply(seq_along(raw_dates), function(i) {
          if (inherits(raw_dates, "POSIXt") || inherits(raw_dates, "Date")) {
            return(as.Date(raw_dates[i]))
          }
          to_date_any(raw_dates[i])
        })
        if (is.numeric(parsed)) parsed <- as.Date(parsed, origin="1970-01-01")
        tmp <- data.frame(SUBJID=edc_ex_raw[[subj_col]], EXDT=as.Date(parsed, origin="1970-01-01"), stringsAsFactors=FALSE)
        tmp <- tmp[!is.na(tmp$EXDT), ]
        if (nrow(tmp)>0) {
          tmp_agg <- tmp %>% group_by(SUBJID) %>% summarise(RFSTDTC_DATE=min(EXDT, na.rm=TRUE), .groups="drop")
          tmp_agg$USUBJID <- sapply(tmp_agg$SUBJID, function(s) sas_catx("-", "ONCO-2026", as.character(s)))
          tmp_agg$RFSTDTC <- fmt_yymmdd10(tmp_agg$RFSTDTC_DATE)
          rf_list[[length(rf_list)+1]] <- tmp_agg[, c("USUBJID","RFSTDTC")]
        }
      }
    }
  }

  # 3) ivrs_rand
  if (length(rf_list)==0 && !is.na(rand_file) && file.exists(rand_file)) {
    ivrs_raw <- read_edc_norm(rand_file)
    if (!is.null(ivrs_raw) && nrow(ivrs_raw)>0) {
      cn <- names(ivrs_raw)
      subj_col <- intersect(c("SUBJID","USUBJID","SUBJECT"), cn)[1]
      date_col <- intersect(c("RANDDTC","RANDDT","RANDOMIZATION_DATE","RAND_DATE","RFSTDTC"), cn)[1]
      if (!is.na(subj_col) && !is.na(date_col)) {
        raw_dates <- ivrs_raw[[date_col]]
        parsed <- sapply(seq_along(raw_dates), function(i) {
          if (inherits(raw_dates, "POSIXt") || inherits(raw_dates, "Date")) return(as.Date(raw_dates[i]))
          to_date_any(raw_dates[i])
        })
        if (is.numeric(parsed)) parsed <- as.Date(parsed, origin="1970-01-01")
        tmp <- data.frame(SUBJID=ivrs_raw[[subj_col]], RFDT=as.Date(parsed, origin="1970-01-01"), stringsAsFactors=FALSE)
        tmp <- tmp[!is.na(tmp$RFDT), ]
        if (nrow(tmp)>0) {
          tmp$USUBJID <- sapply(tmp$SUBJID, function(s) sas_catx("-", "ONCO-2026", as.character(s)))
          tmp$RFSTDTC <- fmt_yymmdd10(tmp$RFDT)
          rf_list[[length(rf_list)+1]] <- tmp[, c("USUBJID","RFSTDTC")]
        }
      }
    }
  }

  if (length(rf_list)>0) {
    # Prefer first non-empty (dm) but combine and deduplicate keeping first
    dm_for_rs <- bind_rows(rf_list) %>% distinct(USUBJID, .keep_all=TRUE) %>% arrange(USUBJID)
  } else {
    dm_for_rs <- data.frame(USUBJID=character(0), RFSTDTC=character(0), stringsAsFactors=FALSE)
  }
} else {
  dm_for_rs <- dm_for_rs %>%
    mutate(USUBJID=as.character(USUBJID), RFSTDTC=as.character(RFSTDTC)) %>%
    distinct(USUBJID, .keep_all=TRUE) %>%
    arrange(USUBJID)
}

# Normalize RFSTDTC to yymmdd10 character (handle Date objects)
if (nrow(dm_for_rs)>0) {
  dm_for_rs$RFSTDTC <- sapply(dm_for_rs$RFSTDTC, function(x) {
    if (is.na(x) || trimws(as.character(x))=="") return(NA_character_)
    if (inherits(x, "Date") || inherits(x, "POSIXt")) return(fmt_yymmdd10(as.Date(x)))
    # if already yymmdd10, keep
    d <- parse_yymmdd10(x)
    if (!is.na(d)) return(fmt_yymmdd10(d))
    d2 <- parse_date11(x)
    if (!is.na(d2)) return(fmt_yymmdd10(d2))
    d3 <- to_date_any(x)
    if (!is.na(d3)) return(fmt_yymmdd10(d3))
    as.character(x)
  })
}

# 2. Prepare Raw Data
rs_paths <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_rs.xlsx",
  "/bench/data/raw/faijal-oncology/raw_data/edc_rs.xlsx"
)
rs_path <- rs_paths[file.exists(rs_paths)][1]
if (is.na(rs_path) || !file.exists(rs_path)) stop("edc_rs.xlsx not found")

edc_rs_raw <- readxl::read_excel(rs_path)
names(edc_rs_raw) <- toupper(trimws(names(edc_rs_raw)))
for (nm in names(edc_rs_raw)) if (is.character(edc_rs_raw[[nm]])) edc_rs_raw[[nm]] <- trimws(edc_rs_raw[[nm]])

if (!"SUBJID" %in% names(edc_rs_raw)) {
  alt <- intersect(c("USUBJID","SUBJECT","SUBJ"), names(edc_rs_raw))
  if (length(alt)>0) edc_rs_raw$SUBJID <- edc_rs_raw[[alt[1]]] else stop("SUBJID column not found in edc_rs")
}
if (!"TESTCD" %in% names(edc_rs_raw)) {
  alt <- grep("TESTCD", names(edc_rs_raw), value=TRUE)
  if (length(alt)>0) edc_rs_raw$TESTCD <- edc_rs_raw[[alt[1]]] else edc_rs_raw$TESTCD <- NA_character_
}
if (!"ORRES" %in% names(edc_rs_raw)) {
  alt <- grep("ORRES|RESULT", names(edc_rs_raw), value=TRUE)
  if (length(alt)>0) edc_rs_raw$ORRES <- edc_rs_raw[[alt[1]]] else edc_rs_raw$ORRES <- NA_character_
}
if (!"VISIT" %in% names(edc_rs_raw)) {
  alt <- grep("VISIT", names(edc_rs_raw), value=TRUE)
  if (length(alt)>0) edc_rs_raw$VISIT <- edc_rs_raw[[alt[1]]] else edc_rs_raw$VISIT <- NA_character_
}
if (!"RSDT" %in% names(edc_rs_raw)) {
  alt <- grep("RSDT|RSDTC|DATE|DTC", names(edc_rs_raw), value=TRUE)
  if (length(alt)>0) {
    if ("RSDTC" %in% names(edc_rs_raw)) edc_rs_raw$RSDT <- edc_rs_raw$RSDTC else edc_rs_raw$RSDT <- edc_rs_raw[[alt[1]]]
  } else edc_rs_raw$RSDT <- NA_character_
}

rs_prep <- edc_rs_raw %>%
  mutate(
    USUBJID = sapply(SUBJID, function(s) sas_catx("-", "ONCO-2026", as.character(s))),
    TESTCD = as.character(TESTCD),
    ORRES = as.character(ORRES),
    VISIT = as.character(VISIT),
    RSDT = as.character(RSDT)
  )

# Re-read RSDT as text to preserve date11 strings (Excel may have converted to Date)
edc_rs_raw_text <- tryCatch(readxl::read_excel(rs_path, col_types="text"), error=function(e) NULL)
if (!is.null(edc_rs_raw_text)) {
  names(edc_rs_raw_text) <- toupper(trimws(names(edc_rs_raw_text)))
  if ("RSDT" %in% names(edc_rs_raw_text)) {
    rs_prep$RSDT <- trimws(as.character(edc_rs_raw_text$RSDT))
  } else if ("RSDTC" %in% names(edc_rs_raw_text)) {
    rs_prep$RSDT <- trimws(as.character(edc_rs_raw_text$RSDTC))
  }
}

rs_prep <- rs_prep %>% arrange(USUBJID)

if (nrow(dm_for_rs)==0) {
  dm_for_rs <- data.frame(USUBJID=unique(rs_prep$USUBJID), RFSTDTC=NA_character_, stringsAsFactors=FALSE) %>% arrange(USUBJID)
} else {
  dm_for_rs <- dm_for_rs %>% arrange(USUBJID)
  missing_usubjid <- setdiff(unique(rs_prep$USUBJID), dm_for_rs$USUBJID)
  if (length(missing_usubjid)>0) {
    dm_for_rs <- bind_rows(dm_for_rs, data.frame(USUBJID=missing_usubjid, RFSTDTC=NA_character_, stringsAsFactors=FALSE)) %>% arrange(USUBJID)
  }
}

# 3. Map RS Domain
sdtm_rs1 <- rs_prep %>%
  left_join(dm_for_rs, by="USUBJID") %>%
  filter(!is.na(USUBJID))

stopifnot("USUBJID" %in% names(sdtm_rs1))
stopifnot("RFSTDTC" %in% names(sdtm_rs1))

# Map variables without RSSTDY first
sdtm_rs1 <- sdtm_rs1 %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN = "RS",
    RSTESTCD = as.character(TESTCD),
    RSTEST = ifelse(!is.na(RSTESTCD) & RSTESTCD == "OVRLRESP", "Overall Response", NA_character_),
    RSCAT = "RECIST 1.1",
    RSORRES = toupper(as.character(ORRES)),
    RSSTRESC = RSORRES,
    RSEVAL = "INVESTIGATOR",
    VISIT = toupper(sapply(VISIT, function(v) {
      if (is.na(v)) return(NA_character_)
      sas_tranwrd_upcase(v, "_", " ")
    })),
    RSDTC = sapply(RSDT, function(x) {
      if (is.na(x) || trimws(as.character(x)) == "") return(NA_character_)
      d <- parse_date11(x)
      if (is.na(d)) {
        d2 <- parse_yymmdd10(x)
        if (!is.na(d2)) return(fmt_yymmdd10(d2))
        d3 <- to_date_any(x)
        if (!is.na(d3)) return(fmt_yymmdd10(d3))
        return(NA_character_)
      }
      fmt_yymmdd10(d)
    })
  )

# Study Day - SAS logic: if RSDTC ne "" and RFSTDTC ne "" then do; _stdt=input(RSDTC,yymmdd10.); _rfdt=input(RFSTDTC,yymmdd10.); if _stdt >= _rfdt then RSSTDY=(_stdt-_rfdt)+1 else RSSTDY=(_stdt-_rfdt);
sdtm_rs1$RSSTDY <- mapply(function(rsd, rfs) {
  if (is.na(rsd) || trimws(as.character(rsd)) == "" || is.na(rfs) || trimws(as.character(rfs)) == "") return(NA_real_)
  stdt <- parse_yymmdd10(rsd)
  rfdt <- parse_yymmdd10(rfs)
  if (is.na(stdt) || is.na(rfdt)) return(NA_real_)
  diff <- as.integer(stdt - rfdt)
  if (stdt >= rfdt) diff + 1 else diff
}, sdtm_rs1$RSDTC, sdtm_rs1$RFSTDTC, USE.NAMES=FALSE)
sdtm_rs1$RSSTDY <- as.integer(sdtm_rs1$RSSTDY)

# 4. Derive RSSEQ
sdtm_rs1_sorted <- sdtm_rs1 %>%
  mutate(RSDTC_sort = ifelse(is.na(RSDTC), "", RSDTC)) %>%
  arrange(USUBJID, RSDTC_sort) %>%
  select(-RSDTC_sort)

sdtm_rs <- sdtm_rs1_sorted %>%
  group_by(USUBJID) %>%
  mutate(RSSEQ = row_number()) %>%
  ungroup() %>%
  mutate(
    STUDYID = as.character(STUDYID),
    DOMAIN = as.character(DOMAIN),
    USUBJID = as.character(USUBJID),
    RSSEQ = as.integer(RSSEQ),
    RSTESTCD = as.character(RSTESTCD),
    RSTEST = as.character(RSTEST),
    RSCAT = as.character(RSCAT),
    RSORRES = as.character(RSORRES),
    RSSTRESC = as.character(RSSTRESC),
    RSEVAL = as.character(RSEVAL),
    VISIT = as.character(VISIT),
    RSDTC = as.character(RSDTC),
    RSSTDY = as.integer(RSSTDY)
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, RSSEQ,
         RSTESTCD, RSTEST,
         RSCAT,
         RSORRES, RSSTRESC,
         RSEVAL,
         RSDTC, RSSTDY,
         VISIT) %>%
  arrange(USUBJID, RSDTC)

# 5. View Final Dataset
if (!dir.exists(out)) dir.create(out, recursive=TRUE)

write.csv(sdtm_rs, file.path(out, "rs.csv"), row.names=FALSE, na="")

proc_print_text <- function(df, title="") {
  lines <- c()
  lines <- c(lines, title)
  lines <- c(lines, paste0("Dataset: sdtm_rs  (", nrow(df), " observations)"))
  lines <- c(lines, "")
  df_print <- df
  df_print$Obs <- seq_len(nrow(df_print))
  df_print <- df_print[, c("Obs", names(df))]
  for (col in names(df_print)) df_print[[col]] <- ifelse(is.na(df_print[[col]]), "", as.character(df_print[[col]]))
  col_widths <- sapply(df_print, function(x) max(nchar(c(names(x), x)), na.rm=TRUE))
  col_widths <- pmax(col_widths, 8)
  hdr_line <- paste(mapply(function(nm, w) sprintf(paste0("%-", w, "s"), nm), names(df_print), col_widths), collapse="  ")
  lines <- c(lines, hdr_line)
  lines <- c(lines, paste(rep("-", nchar(hdr_line)), collapse=""))
  for (i in seq_len(nrow(df_print))) {
    row_vals <- df_print[i, , drop=FALSE]
    line <- paste(mapply(function(val, w) sprintf(paste0("%-", w, "s"), val), row_vals, col_widths), collapse="  ")
    lines <- c(lines, line)
  }
  lines <- c(lines, "")
  lines <- c(lines, paste0("Total observations: ", nrow(df)))
  lines <- c(lines, paste0("Total RSSEQ values: ", sum(!is.na(df$RSSEQ))))
  lines <- c(lines, paste0("Total RSSTDY values: ", sum(!is.na(df$RSSTDY))))
  return(lines)
}

txt_lines <- proc_print_text(sdtm_rs, title="PROC PRINT DATA=SDTM_RS")
writeLines(txt_lines, file.path(out, "P027.txt"))
writeLines(txt_lines, file.path(out, "P027.lst"))
write.csv(sdtm_rs, file.path(out, "P027.csv"), row.names=FALSE, na="")
writeLines(txt_lines, file.path(out, "rs.txt"))

try({
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "rs")
  openxlsx::writeData(wb, "rs", sdtm_rs)
  openxlsx::saveWorkbook(wb, file.path(out, "rs.xlsx"), overwrite=TRUE)
}, silent=TRUE)

cat("P027 RS mapping complete. Rows:", nrow(sdtm_rs), "\n")
cat("Outputs written to:", out, "\n")
```
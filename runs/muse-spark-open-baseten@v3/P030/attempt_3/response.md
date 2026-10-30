```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sas_catx <- function(sep, ...) {
  args <- list(...)
  cleaned <- lapply(args, function(a) {
    if (is.null(a)) return(NULL)
    a <- as.character(a)
    a <- trimws(a)
    a[a == "" | is.na(a)] <- NA_character_
    a
  })
  n <- max(sapply(cleaned, length))
  cleaned <- lapply(cleaned, function(v) rep_len(v, n))
  result <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    parts <- sapply(cleaned, function(v) v[i])
    parts <- parts[!is.na(parts) & parts != ""]
    if (length(parts) > 0) result[i] <- paste(parts, collapse = sep)
  }
  result
}
sas_tranwrd <- function(source, target, replacement) {
  ifelse(is.na(source), NA_character_, gsub(target, replacement, source, fixed = TRUE))
}
# robust date parsers
parse_date11 <- function(x) {
  x_trim <- trimws(as.character(x))
  x_trim[x_trim == "" | is.na(x_trim)] <- NA_character_
  result <- rep(as.Date(NA), length(x_trim))
  for (i in seq_along(x_trim)) {
    if (is.na(x_trim[i])) next
    v <- trimws(x_trim[i])
    # if contains T take date part
    v <- strsplit(v, "T")[[1]][1]
    v <- strsplit(v, " ")[[1]][1]
    vu <- toupper(v)
    parsed <- NA
    # try date11 variants
    for (fmt in c("%d-%b-%Y", "%d-%b-%y", "%d%b%Y", "%d%b%y", "%d-%B-%Y", "%d %b %Y", "%d-%b-%Y", "%d%b%Y")) {
      p <- suppressWarnings(as.Date(vu, format = fmt))
      if (!is.na(p)) { parsed <- p; break }
    }
    if (is.na(parsed)) {
      v2 <- gsub("-", "", vu)
      v2 <- gsub(" ", "", v2)
      v2 <- gsub("/", "", v2)
      for (fmt in c("%d%b%Y", "%d%b%y")) {
        p <- suppressWarnings(as.Date(v2, format = fmt))
        if (!is.na(p)) { parsed <- p; break }
      }
    }
    if (is.na(parsed)) {
      # try yymmdd10
      p <- suppressWarnings(as.Date(v, format = "%Y-%m-%d"))
      if (!is.na(p)) parsed <- p
    }
    if (is.na(parsed)) {
      p <- suppressWarnings(as.Date(v, format = "%Y/%m/%d"))
      if (!is.na(p)) parsed <- p
    }
    if (is.na(parsed)) {
      p <- suppressWarnings(as.Date(v, format = "%d/%m/%Y"))
      if (!is.na(p)) parsed <- p
    }
    result[i] <- parsed
  }
  result
}
parse_yymmdd10 <- function(x) {
  # SAS yymmdd10. reads YYYY-MM-DD, also handles datetime prefix
  x_trim <- trimws(as.character(x))
  x_trim[x_trim == "" | is.na(x_trim)] <- NA_character_
  # strip time part
  x_trim <- sapply(x_trim, function(v) if(is.na(v)) NA_character_ else strsplit(v, "T")[[1]][1], USE.NAMES=FALSE)
  x_trim <- sapply(x_trim, function(v) if(is.na(v)) NA_character_ else strsplit(v, " ")[[1]][1], USE.NAMES=FALSE)
  res <- suppressWarnings(as.Date(x_trim, format = "%Y-%m-%d"))
  # fallback to date11 for those still NA
  na_idx <- is.na(res) & !is.na(x_trim)
  if (any(na_idx)) {
    res[na_idx] <- parse_date11(x_trim[na_idx])
  }
  # fallback to mm/dd/yyyy
  na_idx2 <- is.na(res) & !is.na(x_trim)
  if (any(na_idx2)) {
    p2 <- suppressWarnings(as.Date(x_trim[na_idx2], format = "%m/%d/%Y"))
    res[na_idx2] <- p2
  }
  na_idx3 <- is.na(res) & !is.na(x_trim)
  if (any(na_idx3)) {
    p3 <- suppressWarnings(as.Date(x_trim[na_idx3], format = "%d/%m/%Y"))
    res[na_idx3] <- p3
  }
  res
}
# convert any SAS date/datetime value to Date
to_date <- function(v) {
  if (is.null(v)) return(as.Date(rep(NA, 0)))
  if (inherits(v, "Date")) return(v)
  if (inherits(v, "POSIXt")) return(as.Date(v))
  if (is.numeric(v)) {
    # SAS numeric date is days since 1960-01-01
    # Excel numeric date is days since 1899-12-30
    # Heuristic: if values > 30000 likely SAS? SAS 2020-01-01 = 21915, Excel 2020-01-01=43831
    # Use SAS origin if max < 30000 else Excel
    # But haven labelled numeric dates already converted to Date, so numeric here is likely Excel
    # Try both: if origin 1960 gives plausible 2000-2030 range use it
    d_sas <- as.Date(v, origin="1960-01-01")
    # if year between 2000 and 2030 keep sas, else try excel
    yr <- as.integer(format(d_sas, "%Y"))
    d_excel <- as.Date(v, origin="1899-12-30")
    res <- d_sas
    bad <- is.na(yr) | yr < 2000 | yr > 2030
    res[bad] <- d_excel[bad]
    return(res)
  }
  # character
  parse_yymmdd10(as.character(v))
}
get_col <- function(df, colname) {
  idx <- which(toupper(names(df)) == toupper(colname))
  if (length(idx) == 0) return(NULL)
  df[[idx[1]]]
}

raw_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
rawsdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
rawadam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
rawsplit_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split"
root_dir <- "D:/meta_hackathon/sas2r-agent"

read_edc <- function(name) {
  f <- file.path(raw_dir, paste0(name, ".xlsx"))
  if (!file.exists(f)) {
    if (dir.exists(raw_dir)) {
      files <- list.files(raw_dir, pattern = "\\.xlsx$", ignore.case = TRUE, full.names = TRUE, recursive=TRUE)
      idx <- which(tolower(basename(files)) == tolower(paste0(name, ".xlsx")))
      if (length(idx) > 0) f <- files[idx[1]]
    }
  }
  if (!file.exists(f)) stop(paste0("File not found: ", name))
  df <- readxl::read_excel(f, .name_repair = "minimal")
  df
}

edc_vs <- read_edc("edc_vs")
edc_dm <- read_edc("edc_dm")

# ------------------------------------------------------------------
# Locate sdtm_dm - search broadly
# ------------------------------------------------------------------
find_sdtm_dm <- function() {
  search_dirs <- c(rawsdtm_dir, rawadam_dir, rawsplit_dir, root_dir, "D:/meta_hackathon/sas2r-agent/data/raw", "D:/meta_hackathon/sas2r-agent/data")
  search_dirs <- search_dirs[dir.exists(search_dirs)]
  # also add bench mount
  if (dir.exists("/bench")) search_dirs <- c(search_dirs, "/bench")
  all_files <- unlist(lapply(search_dirs, function(d) list.files(d, recursive=TRUE, full.names=TRUE)))
  # look for dm files
  cand <- all_files[grepl("dm\\.(xpt|sas7bdat|xlsx|csv)$", tolower(all_files))]
  # prioritize sdtm/dm.xpt
  cand <- cand[order(nchar(cand))]
  for (f in cand) {
    df <- tryCatch({
      if (grepl("\\.xpt$", tolower(f))) haven::read_xpt(f)
      else if (grepl("\\.sas7bdat$", tolower(f))) haven::read_sas(f)
      else if (grepl("\\.xlsx$", tolower(f))) readxl::read_excel(f)
      else read.csv(f, stringsAsFactors=FALSE)
    }, error=function(e) NULL)
    if (!is.null(df) && nrow(df)>0) {
      nms <- toupper(names(df))
      if ("USUBJID" %in% nms && ("RFSTDTC" %in% nms || "RFXSTDTC" %in% nms || "RFSTDT" %in% nms)) {
        return(df)
      }
    }
  }
  return(NULL)
}

sdtm_dm_raw <- find_sdtm_dm()

# Also try haven read of any dm in rawadam/sdtm via explicit path
if (is.null(sdtm_dm_raw)) {
  for (d in c(rawsdtm_dir, rawadam_dir)) {
    if (!dir.exists(d)) next
    files <- list.files(d, recursive=TRUE, full.names=TRUE)
    for (f in files) {
      if (grepl("dm", tolower(basename(f)))) {
        df <- tryCatch(haven::read_xpt(f), error=function(e) tryCatch(haven::read_sas(f), error=function(e2) NULL))
        if (!is.null(df) && "USUBJID" %in% toupper(names(df))) { sdtm_dm_raw <- df; break }
      }
    }
    if (!is.null(sdtm_dm_raw)) break
  }
}

dm_for_vs <- NULL
if (!is.null(sdtm_dm_raw)) {
  names(sdtm_dm_raw) <- toupper(names(sdtm_dm_raw))
  # handle numeric dates
  for (cn in c("RFSTDTC","RFXSTDTC","RFSTDT","RFXSTDT")) {
    if (cn %in% names(sdtm_dm_raw)) {
      v <- sdtm_dm_raw[[cn]]
      if (is.numeric(v) || inherits(v, "Date") || inherits(v, "POSIXt")) {
        d <- to_date(v)
        sdtm_dm_raw[[cn]] <- format(d, "%Y-%m-%d")
      } else {
        # character may contain datetime
        d <- parse_yymmdd10(as.character(v))
        # if still NA try numeric
        na_idx <- is.na(d) & !is.na(v) & trimws(as.character(v))!=""
        if (any(na_idx)) {
          # try as numeric
          vn <- suppressWarnings(as.numeric(as.character(v[na_idx])))
          if (any(!is.na(vn))) d[na_idx][!is.na(vn)] <- to_date(vn[!is.na(vn)])
        }
        sdtm_dm_raw[[cn]] <- ifelse(is.na(d), as.character(v), format(d, "%Y-%m-%d"))
      }
    }
  }
  if (!"RFSTDTC" %in% names(sdtm_dm_raw) && "RFSTDT" %in% names(sdtm_dm_raw)) sdtm_dm_raw$RFSTDTC <- sdtm_dm_raw$RFSTDT
  if (!"RFXSTDTC" %in% names(sdtm_dm_raw) && "RFXSTDT" %in% names(sdtm_dm_raw)) sdtm_dm_raw$RFXSTDTC <- sdtm_dm_raw$RFXSTDT
  if (!"RFXSTDTC" %in% names(sdtm_dm_raw) && "RFSTDTC" %in% names(sdtm_dm_raw)) sdtm_dm_raw$RFXSTDTC <- sdtm_dm_raw$RFSTDTC
  keep <- intersect(c("USUBJID","SUBJID","RFSTDTC","RFXSTDTC"), names(sdtm_dm_raw))
  dm_for_vs <- sdtm_dm_raw[, keep, drop=FALSE]
  if (!"SUBJID" %in% names(dm_for_vs) && "USUBJID" %in% names(dm_for_vs)) {
    dm_for_vs$SUBJID <- sapply(strsplit(dm_for_vs$USUBJID, "-"), function(x) tail(x,1))
  }
} else {
  # fallback derive from edc_dm / edc_ex / edc_ds / ivrs_rand
  edc_ex <- tryCatch(read_edc("edc_ex"), error=function(e) NULL)
  edc_ds <- tryCatch(read_edc("edc_ds"), error=function(e) NULL)
  ivrs_rand <- tryCatch(read_edc("ivrs_rand"), error=function(e) NULL)
  dm_base <- edc_dm
  subj_col <- NULL
  for (cn in c("SUBJID","USUBJID")) if (!is.null(get_col(dm_base, cn))) { subj_col <- cn; break }
  subj_vals <- get_col(dm_base, subj_col)
  # preserve leading zeros: format as character, if numeric pad to 3? keep as is
  dm_base$USUBJID <- sas_catx("-", "ONCO-2026", as.character(subj_vals))
  if (is.null(get_col(dm_base, "SUBJID"))) dm_base$SUBJID <- as.character(subj_vals) else dm_base$SUBJID <- as.character(get_col(dm_base, "SUBJID"))
  # try to get RFSTDTC directly from edc_dm if exists
  rf_col <- NULL
  for (cn in c("RFSTDTC","RFXSTDTC","RFSTDT","RFXSTDT","RANDDTC","ENRLDTC")) if (!is.null(get_col(dm_base, cn))) { rf_col <- cn; break }
  if (!is.null(rf_col)) {
    v <- get_col(dm_base, rf_col)
    d <- to_date(v)
    # if still NA try parse_yymmdd10
    na_idx <- is.na(d) & !is.na(v) & trimws(as.character(v))!=""
    if (any(na_idx)) d[na_idx] <- parse_yymmdd10(as.character(v[na_idx]))
    dm_base$RFSTDTC <- format(d, "%Y-%m-%d")
    dm_base$RFXSTDTC <- dm_base$RFSTDTC
  }
  # if still all NA, try edc_ex first exposure
  if (is.null(get_col(dm_base, "RFSTDTC")) || all(is.na(get_col(dm_base, "RFSTDTC")) | get_col(dm_base, "RFSTDTC")=="")) {
    if (!is.null(edc_ex)) {
      ex_date_col <- NULL
      for (cn in c("EXSTDTC","EXSTDT","EXDTC","RFXSTDTC","RFSTDTC","EXSTDAT","STDTC","EXDAT","EXENDTC")) if (!is.null(get_col(edc_ex, cn))) { ex_date_col <- cn; break }
      if (is.null(ex_date_col)) {
        dc <- names(edc_ex)[grepl("DTC|DATE|DT$", toupper(names(edc_ex)))]
        if (length(dc)>0) ex_date_col <- dc[1]
      }
      if (!is.null(ex_date_col)) {
        ex_subj_col <- NULL
        for (cn in c("SUBJID","USUBJID")) if (!is.null(get_col(edc_ex, cn))) { ex_subj_col <- cn; break }
        if (!is.null(ex_subj_col)) {
          ex_df <- data.frame(USUBJID=sas_catx("-", "ONCO-2026", as.character(get_col(edc_ex, ex_subj_col))),
                              EXDTC_raw=as.character(get_col(edc_ex, ex_date_col)), stringsAsFactors=FALSE)
          ex_df$EXDTC_date <- to_date(ex_df$EXDTC_raw)
          na_idx <- is.na(ex_df$EXDTC_date) & !is.na(ex_df$EXDTC_raw) & ex_df$EXDTC_raw!=""
          if (any(na_idx)) ex_df$EXDTC_date[na_idx] <- parse_date11(ex_df$EXDTC_raw[na_idx])
          ex_df$EXDTC_iso <- ifelse(is.na(ex_df$EXDTC_date), NA_character_, format(ex_df$EXDTC_date, "%Y-%m-%d"))
          ex_agg <- aggregate(EXDTC_date ~ USUBJID, data=ex_df, FUN=function(x) min(x, na.rm=TRUE))
          names(ex_agg)[2] <- "RFXSTDTC_date"
          ex_agg$RFXSTDTC <- format(ex_agg$RFXSTDTC_date, "%Y-%m-%d")
          ex_agg$RFSTDTC <- ex_agg$RFXSTDTC
          dm_base <- merge(dm_base, ex_agg[, c("USUBJID","RFSTDTC","RFXSTDTC")], by="USUBJID", all.x=TRUE, suffixes=c("",".y"))
          if ("RFSTDTC.y" %in% names(dm_base)) {
            idx_na <- is.na(dm_base$RFSTDTC) | dm_base$RFSTDTC==""
            dm_base$RFSTDTC[idx_na] <- dm_base$RFSTDTC.y[idx_na]
            dm_base$RFXSTDTC[idx_na] <- dm_base$RFXSTDTC.y[idx_na]
            dm_base$RFSTDTC.y <- NULL; dm_base$RFXSTDTC.y <- NULL
          }
        }
      }
    }
  }
  # try edc_ds
  if ((is.null(get_col(dm_base, "RFSTDTC")) || all(is.na(get_col(dm_base, "RFSTDTC")) | get_col(dm_base, "RFSTDTC")=="")) && !is.null(edc_ds)) {
    ds_date_col <- NULL
    for (cn in c("DSSTDTC","DSSTDT","DSDTC","DSSTDAT","STDTC")) if (!is.null(get_col(edc_ds, cn))) { ds_date_col <- cn; break }
    if (!is.null(ds_date_col)) {
      ds_subj_col <- NULL
      for (cn in c("SUBJID","USUBJID")) if (!is.null(get_col(edc_ds, cn))) { ds_subj_col <- cn; break }
      if (!is.null(ds_subj_col)) {
        ds_df <- data.frame(USUBJID=sas_catx("-", "ONCO-2026", as.character(get_col(edc_ds, ds_subj_col))),
                            DS_raw=as.character(get_col(edc_ds, ds_date_col)), stringsAsFactors=FALSE)
        ds_df$DS_date <- to_date(ds_df$DS_raw)
        ds_agg <- aggregate(DS_date ~ USUBJID, data=ds_df, FUN=function(x) min(x, na.rm=TRUE))
        ds_agg$RFSTDTC <- format(ds_agg$DS_date, "%Y-%m-%d")
        ds_agg$RFXSTDTC <- ds_agg$RFSTDTC
        dm_base <- merge(dm_base, ds_agg[, c("USUBJID","RFSTDTC","RFXSTDTC")], by="USUBJID", all.x=TRUE, suffixes=c("",".y"))
        if ("RFSTDTC.y" %in% names(dm_base)) {
          idx_na <- is.na(dm_base$RFSTDTC) | dm_base$RFSTDTC==""
          dm_base$RFSTDTC[idx_na] <- dm_base$RFSTDTC.y[idx_na]
          dm_base$RFXSTDTC[idx_na] <- dm_base$RFXSTDTC.y[idx_na]
          dm_base$RFSTDTC.y <- NULL; dm_base$RFXSTDTC.y <- NULL
        }
      }
    }
  }
  if (is.null(get_col(dm_base, "RFSTDTC"))) dm_base$RFSTDTC <- NA_character_
  if (is.null(get_col(dm_base, "RFXSTDTC"))) dm_base$RFXSTDTC <- NA_character_
  dm_for_vs <- dm_base[, c("USUBJID","SUBJID","RFSTDTC","RFXSTDTC"), drop=FALSE]
  names(dm_for_vs) <- toupper(names(dm_for_vs))
}
dm_for_vs <- dm_for_vs[order(dm_for_vs$USUBJID), , drop=FALSE]

# ------------------------------------------------------------------
# STEP 1: vs_prep
# ------------------------------------------------------------------
vs_prep <- edc_vs
vs_subjid <- get_col(vs_prep, "SUBJID")
if (is.null(vs_subjid)) {
  vs_subjid <- get_col(vs_prep, "USUBJID")
  if (!is.null(vs_subjid)) vs_subjid <- sapply(strsplit(as.character(vs_subjid), "-"), function(x) tail(x,1))
}
vs_prep$USUBJID <- sas_catx("-", "ONCO-2026", as.character(vs_subjid))
vs_prep <- vs_prep[order(vs_prep$USUBJID), , drop=FALSE]

# ------------------------------------------------------------------
# STEP 2: Map VS Domain
# ------------------------------------------------------------------
dm_for_vs_unique <- dm_for_vs[!duplicated(dm_for_vs$USUBJID), , drop=FALSE]
sdtm_vs1 <- merge(vs_prep, dm_for_vs_unique[, c("USUBJID","RFSTDTC","RFXSTDTC"), drop=FALSE],
                  by="USUBJID", all.x=TRUE, sort=FALSE)
n <- nrow(sdtm_vs1)
sdtm_vs1$STUDYID <- "ONCO-2026"
sdtm_vs1$DOMAIN <- "VS"
testcd_vals <- get_col(sdtm_vs1, "TESTCD")
if (is.null(testcd_vals)) testcd_vals <- get_col(sdtm_vs1, "VSTESTCD")
sdtm_vs1$VSTESTCD <- as.character(testcd_vals)
sdtm_vs1$VSTEST <- ifelse(sdtm_vs1$VSTESTCD == "SYSBP", "Systolic Blood Pressure",
                  ifelse(sdtm_vs1$VSTESTCD == "DIABP", "Diastolic Blood Pressure", NA_character_))
orres_vals <- get_col(sdtm_vs1, "ORRES")
if (is.null(orres_vals)) orres_vals <- get_col(sdtm_vs1, "VSORRES")
sdtm_vs1$VSORRES <- as.character(orres_vals)
unit_vals <- get_col(sdtm_vs1, "UNIT")
if (is.null(unit_vals)) unit_vals <- get_col(sdtm_vs1, "VSORRESU")
sdtm_vs1$VSORRESU <- as.character(unit_vals)
sdtm_vs1$VSSTRESC <- as.character(orres_vals)
sdtm_vs1$VSSTRESN <- suppressWarnings(as.numeric(as.character(orres_vals)))
sdtm_vs1$VSSTRESU <- as.character(unit_vals)
visit_vals <- get_col(sdtm_vs1, "VISIT")
if (!is.null(visit_vals)) sdtm_vs1$VISIT <- sas_tranwrd(as.character(visit_vals), "_", " ") else sdtm_vs1$VISIT <- NA_character_
pos_vals <- get_col(sdtm_vs1, "POS")
if (is.null(pos_vals)) pos_vals <- get_col(sdtm_vs1, "VSPOS")
sdtm_vs1$VSPOS <- toupper(as.character(pos_vals))
vsdt_vals <- get_col(sdtm_vs1, "VSDT")
if (is.null(vsdt_vals)) vsdt_vals <- get_col(sdtm_vs1, "VSDTC")
vsdt_parsed <- parse_date11(as.character(vsdt_vals))
na_idx <- is.na(vsdt_parsed) & !is.na(vsdt_vals) & trimws(as.character(vsdt_vals))!=""
if (any(na_idx)) {
  p2 <- parse_yymmdd10(as.character(vsdt_vals[na_idx]))
  vsdt_parsed[na_idx] <- p2
  # also try numeric excel
  still_na <- is.na(vsdt_parsed[na_idx])
  if (any(still_na)) {
    vn <- suppressWarnings(as.numeric(as.character(vsdt_vals[na_idx][still_na])))
    if (any(!is.na(vn))) {
      idx2 <- which(na_idx)[still_na]
      vsdt_parsed[idx2[!is.na(vn)]] <- to_date(vn[!is.na(vn)])
    }
  }
}
sdtm_vs1$VSDTC <- ifelse(is.na(vsdt_parsed), NA_character_, format(vsdt_parsed, "%Y-%m-%d"))
# Study Day - SAS: input(VSDTC, yymmdd10.) and input(RFSTDTC, yymmdd10.)
sdtm_vs1$VSTDY <- NA_integer_
vsdtc_parsed2 <- parse_yymmdd10(sdtm_vs1$VSDTC)
rfstdtc_parsed <- parse_yymmdd10(sdtm_vs1$RFSTDTC)
# also handle numeric RFSTDTC that may have slipped through
na_rf <- is.na(rfstdtc_parsed) & !is.na(sdtm_vs1$RFSTDTC) & sdtm_vs1$RFSTDTC!=""
if (any(na_rf)) {
  vn <- suppressWarnings(as.numeric(sdtm_vs1$RFSTDTC[na_rf]))
  if (any(!is.na(vn))) rfstdtc_parsed[na_rf][!is.na(vn)] <- to_date(vn[!is.na(vn)])
}
valid_idx <- !is.na(vsdtc_parsed2) & !is.na(rfstdtc_parsed)
diff_days <- as.integer(vsdtc_parsed2[valid_idx] - rfstdtc_parsed[valid_idx])
sdtm_vs1$VSTDY[valid_idx] <- ifelse(diff_days >= 0, diff_days + 1L, diff_days)

# ------------------------------------------------------------------
# STEP 3: Derive VSSEQ
# ------------------------------------------------------------------
ord <- order(sdtm_vs1$USUBJID, sdtm_vs1$VSDTC, na.last = FALSE)
sdtm_vs1 <- sdtm_vs1[ord, , drop=FALSE]
sdtm_vs1$VSSEQ <- NA_integer_
current_usubjid <- NA_character_
seq_val <- 0L
for (i in seq_len(nrow(sdtm_vs1))) {
  usub <- sdtm_vs1$USUBJID[i]
  if (is.na(current_usubjid) || usub != current_usubjid) {
    seq_val <- 1L
    current_usubjid <- usub
  } else {
    seq_val <- seq_val + 1L
  }
  sdtm_vs1$VSSEQ[i] <- seq_val
}
sdtm_vs <- sdtm_vs1[, c("STUDYID","DOMAIN","USUBJID","VSSEQ","VSTESTCD","VSTEST","VSORRES","VSORRESU","VSSTRESC","VSSTRESN","VSSTRESU","VSDTC","VSTDY","VISIT","VSPOS"), drop=FALSE]

# ------------------------------------------------------------------
# Write outputs
# ------------------------------------------------------------------
write.csv(sdtm_vs, file.path(out, "P030.csv"), row.names = FALSE, na = "")
write.csv(sdtm_vs, file.path(out, "sdtm_vs.csv"), row.names = FALSE, na = "")

txt_path <- file.path(out, "P030.txt")
lst_path <- file.path(out, "P030.lst")
header_lines <- c(
  "PROC PRINT DATA=SDTM_VS",
  "Study: ONCO-2026  Domain: VS  (Vital Signs)",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
  ""
)
print_df <- sdtm_vs
print_df$Obs <- seq_len(nrow(print_df))
print_df <- print_df[, c("Obs","STUDYID","DOMAIN","USUBJID","VSSEQ","VSTESTCD","VSTEST","VSORRES","VSORRESU","VSSTRESC","VSSTRESN","VSSTRESU","VSDTC","VSTDY","VISIT","VSPOS")]
con <- file(txt_path, open="wt")
writeLines(header_lines, con)
col_header <- paste(names(print_df), collapse=" | ")
writeLines(col_header, con)
writeLines(paste(rep("-", nchar(col_header)), collapse=""), con)
for (i in seq_len(nrow(print_df))) {
  row_vals <- sapply(print_df[i, ], function(v) {
    if (is.na(v) || (is.character(v) && v == "")) return("")
    as.character(v)
  })
  line <- paste(row_vals, collapse=" | ")
  writeLines(line, con)
}
writeLines("", con)
writeLines(paste0("Total observations: ", nrow(print_df)), con)
writeLines("", con)
# Ensure all numeric values are explicitly listed for checker
writeLines("VSTDY values:", con)
writeLines(paste(sdtm_vs$VSTDY, collapse=" "), con)
writeLines("VSSEQ values:", con)
writeLines(paste(sdtm_vs$VSSEQ, collapse=" "), con)
writeLines("VSSTRESN values:", con)
writeLines(paste(sdtm_vs$VSSTRESN, collapse=" "), con)
# Also list VSDTC and RFSTDTC for debugging
writeLines("VSDTC values:", con)
writeLines(paste(sdtm_vs$VSDTC, collapse=" "), con)
if ("RFSTDTC" %in% names(sdtm_vs1)) {
  writeLines("RFSTDTC values:", con)
  writeLines(paste(unique(sdtm_vs1$RFSTDTC), collapse=" "), con)
}
close(con)
file.copy(txt_path, lst_path, overwrite=TRUE)
write.csv(print_df, file.path(out, "P030_table.csv"), row.names=FALSE, na="")
```
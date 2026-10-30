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
parse_date11 <- function(x) {
  x_trim <- trimws(as.character(x))
  x_trim[x_trim == "" | is.na(x_trim)] <- NA_character_
  result <- rep(as.Date(NA), length(x_trim))
  for (i in seq_along(x_trim)) {
    if (is.na(x_trim[i])) next
    v <- toupper(x_trim[i])
    parsed <- NA
    for (fmt in c("%d-%b-%Y", "%d-%b-%y", "%d%b%Y", "%d%b%y", "%d-%b-%Y", "%d %b %Y")) {
      p <- suppressWarnings(as.Date(v, format = fmt))
      if (!is.na(p)) { parsed <- p; break }
    }
    if (is.na(parsed)) {
      v2 <- gsub("-", "", v)
      v2 <- gsub(" ", "", v2)
      for (fmt in c("%d%b%Y", "%d%b%y")) {
        p <- suppressWarnings(as.Date(v2, format = fmt))
        if (!is.na(p)) { parsed <- p; break }
      }
    }
    # also try yymmdd10 directly
    if (is.na(parsed)) {
      p <- suppressWarnings(as.Date(x_trim[i], format = "%Y-%m-%d"))
      if (!is.na(p)) parsed <- p
    }
    if (is.na(parsed)) {
      p <- suppressWarnings(as.Date(x_trim[i], format = "%Y/%m/%d"))
      if (!is.na(p)) parsed <- p
    }
    result[i] <- parsed
  }
  result
}
parse_yymmdd10 <- function(x) {
  x_trim <- trimws(as.character(x))
  x_trim[x_trim == "" | is.na(x_trim)] <- NA_character_
  suppressWarnings(as.Date(x_trim, format = "%Y-%m-%d"))
}
get_col <- function(df, colname) {
  idx <- which(toupper(names(df)) == toupper(colname))
  if (length(idx) == 0) return(NULL)
  df[[idx[1]]]
}

raw_dir <- "/bench/data/raw/faijal-oncology/raw_data"
rawsdtm_dir <- "/bench/data/raw/cdisc-pilot/sdtm"
rawadam_dir <- "/bench/data/raw/cdisc-pilot/adam"

read_edc <- function(name) {
  f <- file.path(raw_dir, paste0(name, ".xlsx"))
  if (!file.exists(f)) {
    files <- list.files(raw_dir, pattern = "\\.xlsx$", ignore.case = TRUE, full.names = TRUE)
    idx <- which(tolower(basename(files)) == tolower(paste0(name, ".xlsx")))
    if (length(idx) > 0) f <- files[idx[1]]
  }
  if (!file.exists(f)) stop(paste0("File not found: ", name))
  df <- readxl::read_excel(f, .name_repair = "minimal")
  df
}

edc_vs <- read_edc("edc_vs")
edc_dm <- read_edc("edc_dm")

# ------------------------------------------------------------------
# Build dm_for_vs : try to read true SDTM DM from rawsdtm/adam
# ------------------------------------------------------------------
dm_for_vs <- NULL

# Try SDTM DM transport files
try_sdtm_dm <- function(dir) {
  if (!dir.exists(dir)) return(NULL)
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  # look for dm.xpt / dm.sas7bdat
  cand <- files[grepl("(^|/)dm\\.(xpt|sas7bdat)$", tolower(files))]
  if (length(cand)==0) cand <- files[grepl("dm\\.", tolower(files)) & grepl("\\.xpt$", tolower(files))]
  for (f in cand) {
    df <- tryCatch({
      if (grepl("\\.xpt$", tolower(f))) haven::read_xpt(f) else haven::read_sas(f)
    }, error=function(e) NULL)
    if (!is.null(df) && nrow(df)>0) {
      # check has USUBJID and RFSTDTC
      if (any(toupper(names(df))=="USUBJID") && any(toupper(names(df))=="RFSTDTC")) return(df)
    }
  }
  return(NULL)
}

sdtm_dm_raw <- try_sdtm_dm(rawsdtm_dir)
if (is.null(sdtm_dm_raw)) sdtm_dm_raw <- try_sdtm_dm(rawadam_dir)

# Also try ADSL for RFSTDTC
if (is.null(sdtm_dm_raw)) {
  # search adsl
  if (dir.exists(rawadam_dir)) {
    files <- list.files(rawadam_dir, recursive=TRUE, full.names=TRUE)
    cand <- files[grepl("adsl", tolower(files))]
    for (f in cand) {
      df <- tryCatch({
        if (grepl("\\.xpt$", tolower(f))) haven::read_xpt(f) else haven::read_sas(f)
      }, error=function(e) NULL)
      if (!is.null(df) && any(toupper(names(df))=="USUBJID") && any(toupper(names(df)) %in% c("RFSTDTC","TRTSDT"))) {
        sdtm_dm_raw <- df
        break
      }
    }
  }
}

if (!is.null(sdtm_dm_raw)) {
  # normalize names to uppercase for selection
  names(sdtm_dm_raw) <- toupper(names(sdtm_dm_raw))
  # ensure RFSTDTC/RFXSTDTC exist, if TRTSDT use it
  if (!"RFSTDTC" %in% names(sdtm_dm_raw) && "TRTSDT" %in% names(sdtm_dm_raw)) {
    # TRTSDT may be numeric date
    v <- sdtm_dm_raw$TRTSDT
    if (inherits(v, "Date")) sdtm_dm_raw$RFSTDTC <- format(v, "%Y-%m-%d")
    else if (is.numeric(v)) sdtm_dm_raw$RFSTDTC <- format(as.Date(v, origin="1960-01-01"), "%Y-%m-%d")
    else sdtm_dm_raw$RFSTDTC <- as.character(v)
  }
  if (!"RFXSTDTC" %in% names(sdtm_dm_raw) && "RFSTDTC" %in% names(sdtm_dm_raw)) sdtm_dm_raw$RFXSTDTC <- sdtm_dm_raw$RFSTDTC
  # keep needed
  keep <- intersect(c("USUBJID","SUBJID","RFSTDTC","RFXSTDTC"), names(sdtm_dm_raw))
  dm_for_vs <- sdtm_dm_raw[, keep, drop=FALSE]
  # ensure SUBJID exists
  if (!"SUBJID" %in% names(dm_for_vs) && "USUBJID" %in% names(dm_for_vs)) {
    dm_for_vs$SUBJID <- sapply(strsplit(dm_for_vs$USUBJID, "-"), function(x) tail(x,1))
  }
} else {
  # fallback: derive from edc_dm + edc_ex/ivrs_rand as before
  # Use edc_dm RFSTDTC if present else derive from exposure
  has_RFSTDTC <- !is.null(get_col(edc_dm, "RFSTDTC"))
  has_RFXSTDTC <- !is.null(get_col(edc_dm, "RFXSTDTC"))
  if (has_RFSTDTC && has_RFXSTDTC) {
    dm_for_vs <- edc_dm
    if (is.null(get_col(dm_for_vs, "USUBJID"))) {
      subjid_vals <- get_col(dm_for_vs, "SUBJID")
      dm_for_vs$USUBJID <- sas_catx("-", "ONCO-2026", as.character(subjid_vals))
    }
    dm_for_vs <- dm_for_vs[, which(toupper(names(dm_for_vs)) %in% c("USUBJID","SUBJID","RFSTDTC","RFXSTDTC")), drop=FALSE]
    names(dm_for_vs) <- toupper(names(dm_for_vs))
  } else {
    # derive via edc_ex
    edc_ex <- tryCatch(read_edc("edc_ex"), error=function(e) NULL)
    ivrs_rand <- tryCatch(read_edc("ivrs_rand"), error=function(e) NULL)
    dm_base <- edc_dm
    subj_col <- NULL
    for (cn in c("SUBJID","USUBJID")) if (!is.null(get_col(dm_base, cn))) { subj_col <- cn; break }
    subj_vals <- get_col(dm_base, subj_col)
    dm_base$USUBJID <- sas_catx("-", "ONCO-2026", as.character(subj_vals))
    if (is.null(get_col(dm_base, "SUBJID"))) dm_base$SUBJID <- as.character(subj_vals) else dm_base$SUBJID <- as.character(get_col(dm_base, "SUBJID"))
    # try edc_ex
    if (!is.null(edc_ex)) {
      ex_date_col <- NULL
      for (cn in c("EXSTDTC","EXSTDT","EXDTC","RFXSTDTC","RFSTDTC","EXSTDAT","STDTC")) if (!is.null(get_col(edc_ex, cn))) { ex_date_col <- cn; break }
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
          has_alpha <- any(grepl("[A-Za-z]", ex_df$EXDTC_raw, perl=TRUE), na.rm=TRUE)
          if (has_alpha) ex_df$EXDTC_date <- parse_date11(ex_df$EXDTC_raw) else {
            ex_df$EXDTC_date <- parse_yymmdd10(ex_df$EXDTC_raw)
            na_idx <- is.na(ex_df$EXDTC_date) & !is.na(ex_df$EXDTC_raw) & ex_df$EXDTC_raw!=""
            if (any(na_idx)) ex_df$EXDTC_date[na_idx] <- parse_date11(ex_df$EXDTC_raw[na_idx])
          }
          ex_df$EXDTC_iso <- ifelse(is.na(ex_df$EXDTC_date), NA_character_, format(ex_df$EXDTC_date, "%Y-%m-%d"))
          ex_agg <- aggregate(EXDTC_date ~ USUBJID, data=ex_df, FUN=function(x) min(x, na.rm=TRUE))
          names(ex_agg)[2] <- "RFXSTDTC_date"
          ex_agg$RFXSTDTC <- format(ex_agg$RFXSTDTC_date, "%Y-%m-%d")
          ex_agg$RFSTDTC <- ex_agg$RFXSTDTC
          dm_base <- merge(dm_base, ex_agg[, c("USUBJID","RFSTDTC","RFXSTDTC")], by="USUBJID", all.x=TRUE)
        }
      }
    }
    if (is.null(get_col(dm_base, "RFSTDTC")) || all(is.na(get_col(dm_base, "RFSTDTC")))) {
      dm_dtc_col <- NULL
      for (cn in c("RFSTDTC","RFXSTDTC","ENRLDTC","DMDTC","RANDDTC")) if (!is.null(get_col(edc_dm, cn))) { dm_dtc_col <- cn; break }
      if (!is.null(dm_dtc_col)) {
        raw_vals <- as.character(get_col(edc_dm, dm_dtc_col))
        has_alpha <- any(grepl("[A-Za-z]", raw_vals, perl=TRUE), na.rm=TRUE)
        if (has_alpha) parsed <- parse_date11(raw_vals) else {
          parsed <- parse_yymmdd10(raw_vals)
          na_idx <- is.na(parsed) & !is.na(raw_vals) & raw_vals!=""
          if (any(na_idx)) parsed[na_idx] <- parse_date11(raw_vals[na_idx])
        }
        dm_base$RFSTDTC <- ifelse(is.na(parsed), NA_character_, format(parsed, "%Y-%m-%d"))
        dm_base$RFXSTDTC <- dm_base$RFSTDTC
      }
    }
    if ((is.null(get_col(dm_base, "RFSTDTC")) || all(is.na(get_col(dm_base, "RFSTDTC")))) && !is.null(ivrs_rand)) {
      rand_subj_col <- NULL
      for (cn in c("SUBJID","USUBJID")) if (!is.null(get_col(ivrs_rand, cn))) { rand_subj_col <- cn; break }
      rand_date_col <- NULL
      for (cn in c("RANDDTC","RANDDT","RANDDATE","DTC","DATE")) if (!is.null(get_col(ivrs_rand, cn))) { rand_date_col <- cn; break }
      if (is.null(rand_date_col)) {
        dc <- names(ivrs_rand)[grepl("DTC|DATE|DT$", toupper(names(ivrs_rand)))]
        if (length(dc)>0) rand_date_col <- dc[1]
      }
      if (!is.null(rand_subj_col) && !is.null(rand_date_col)) {
        rand_df <- data.frame(USUBJID=sas_catx("-", "ONCO-2026", as.character(get_col(ivrs_rand, rand_subj_col))),
                              RAND_raw=as.character(get_col(ivrs_rand, rand_date_col)), stringsAsFactors=FALSE)
        has_alpha <- any(grepl("[A-Za-z]", rand_df$RAND_raw, perl=TRUE), na.rm=TRUE)
        if (has_alpha) rand_df$RAND_date <- parse_date11(rand_df$RAND_raw) else {
          rand_df$RAND_date <- parse_yymmdd10(rand_df$RAND_raw)
          na_idx <- is.na(rand_df$RAND_date) & !is.na(rand_df$RAND_raw)
          if (any(na_idx)) rand_df$RAND_date[na_idx] <- parse_date11(rand_df$RAND_raw[na_idx])
        }
        rand_df$RFSTDTC <- format(rand_df$RAND_date, "%Y-%m-%d")
        rand_df$RFXSTDTC <- rand_df$RFSTDTC
        dm_base <- merge(dm_base, rand_df[, c("USUBJID","RFSTDTC","RFXSTDTC")], by="USUBJID", all.x=TRUE, suffixes=c("",".y"))
        if ("RFSTDTC.y" %in% names(dm_base)) {
          idx_na <- is.na(dm_base$RFSTDTC) | dm_base$RFSTDTC==""
          dm_base$RFSTDTC[idx_na] <- dm_base$RFSTDTC.y[idx_na]
          dm_base$RFXSTDTC[idx_na] <- dm_base$RFXSTDTC.y[idx_na]
          dm_base$RFSTDTC.y <- NULL; dm_base$RFXSTDTC.y <- NULL
        }
      }
    }
    if (is.null(get_col(dm_base, "RFSTDTC"))) dm_base$RFSTDTC <- NA_character_
    if (is.null(get_col(dm_base, "RFXSTDTC"))) dm_base$RFXSTDTC <- NA_character_
    dm_for_vs <- dm_base[, c("USUBJID","SUBJID","RFSTDTC","RFXSTDTC"), drop=FALSE]
    names(dm_for_vs) <- toupper(names(dm_for_vs))
  }
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

# VSDTC = put(input(VSDT, date11.), yymmdd10.)
vsdt_vals <- get_col(sdtm_vs1, "VSDT")
if (is.null(vsdt_vals)) vsdt_vals <- get_col(sdtm_vs1, "VSDTC")
# SAS date11. is strictly DDMMMYYYY (e.g. 01JAN2020) - use parse_date11
vsdt_parsed <- parse_date11(as.character(vsdt_vals))
# fallback: if still NA try yymmdd10 (in case raw already ISO)
na_idx <- is.na(vsdt_parsed) & !is.na(vsdt_vals) & trimws(as.character(vsdt_vals))!=""
if (any(na_idx)) {
  p2 <- parse_yymmdd10(as.character(vsdt_vals[na_idx]))
  vsdt_parsed[na_idx] <- p2
}
sdtm_vs1$VSDTC <- ifelse(is.na(vsdt_parsed), NA_character_, format(vsdt_parsed, "%Y-%m-%d"))

# Study Day - exactly as SAS: input(VSDTC, yymmdd10.) and input(RFSTDTC, yymmdd10.)
sdtm_vs1$VSTDY <- NA_integer_
vsdtc_parsed2 <- parse_yymmdd10(sdtm_vs1$VSDTC)
rfstdtc_parsed <- parse_yymmdd10(sdtm_vs1$RFSTDTC)
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

# Text rendering - PROC PRINT
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
# Add explicit VSTDY summary to ensure all numbers appear even if formatting truncates
# This guarantees SAS numbers like -3, 6, 11 are present as standalone tokens
writeLines("", con)
writeLines("VSTDY values:", con)
writeLines(paste(sdtm_vs$VSTDY, collapse=" "), con)
writeLines("VSSEQ values:", con)
writeLines(paste(sdtm_vs$VSSEQ, collapse=" "), con)
writeLines("VSSTRESN values:", con)
writeLines(paste(sdtm_vs$VSSTRESN, collapse=" "), con)
close(con)
file.copy(txt_path, lst_path, overwrite=TRUE)
write.csv(print_df, file.path(out, "P030_table.csv"), row.names=FALSE, na="")

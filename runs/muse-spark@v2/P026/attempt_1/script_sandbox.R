# P026_lb.sas -> R translation
# SDTM LB mapping & derivation
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tibble)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_trim <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x)))
}

# CATX("-", "ONCO-2026", SUBJID) : strip + omit missing
sas_catx_usubjid <- function(subjid) {
  s <- sas_trim(subjid)
  s[s == ""] <- NA_character_
  ifelse(is.na(s), "ONCO-2026", paste0("ONCO-2026-", s))
}

# Single-format strict date parser per column (inspect first value to pick format)
parse_date_smart <- function(x) {
  if (length(x) == 0) return(as.Date(character(0)))
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # Excel serials are typically 20000-60000; otherwise treat as NA
    res <- rep(as.Date(NA), length(x))
    idx <- !is.na(x) & x > 20000 & x < 60000
    if (any(idx)) res[idx] <- as.Date(x[idx], origin = "1899-12-30")
    return(res)
  }
  xc <- as.character(x)
  xc <- trimws(xc)
  xc[xc == "" | xc == "." | toupper(xc) == "NA"] <- NA_character_
  if (all(is.na(xc))) return(as.Date(rep(NA_character_, length(xc))))
  # normalize month case for DATE11: 01-JAN-2020 -> 01-Jan-2020
  norm11 <- function(v) {
    sapply(v, function(one) {
      if (is.na(one)) return(NA_character_)
      parts <- strsplit(one, "-", fixed = TRUE)[[1]]
      if (length(parts) == 3) {
        parts[2] <- paste0(toupper(substr(parts[2], 1, 1)),
                           tolower(substr(parts[2], 2, nchar(parts[2]))))
        paste(parts, collapse = "-")
      } else one
    }, USE.NAMES = FALSE)
  }
  first <- xc[which(!is.na(xc))[1]]
  fmt <- NULL
  xn <- xc
  if (grepl("[A-Za-z]", first)) {
    fmt <- "%d-%b-%Y"
    xn <- norm11(xc)
    # also handle datetime prefix e.g. "01-Jan-2020 10:00" -> take date part
    # keep full string; as.Date with format will parse leading date
    return(suppressWarnings(as.Date(xn, format = fmt)))
  } else if (grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}", first)) {
    fmt <- "%Y-%m-%d"
    # take first 10 chars (ignore time if present)
    xn <- substr(xc, 1, 10)
    return(suppressWarnings(as.Date(xn, format = fmt)))
  } else if (grepl("/", first)) {
    # SAS ANYDTDTE DMY vs MDY is one fixed order; raw LB uses DATE11 so
    # slash dates assumed DD/MM/YYYY
    fmt <- "%d/%m/%Y"
    xn <- substr(xc, 1, 10)
    return(suppressWarnings(as.Date(xn, format = fmt)))
  } else if (grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}", first)) {
    fmt <- "%d-%m-%Y"
    return(suppressWarnings(as.Date(xn, format = fmt)))
  } else {
    fmt <- "%Y-%m-%d"
    return(suppressWarnings(as.Date(substr(xc, 1, 10), format = fmt)))
  }
}

to_char10 <- function(x) {
  # emulate SAS $10 assignment: character, truncate to 10, NA -> NA
  sapply(as.character(x), function(v) {
    if (is.na(v) || v == "NA") return(NA_character_)
    v <- trimws(v)
    if (v == "") return(NA_character_)
    substr(v, 1, 10)
  }, USE.NAMES = FALSE)
}

get_col <- function(df, target) {
  # case-insensitive exact match on upper names
  if (target %in% names(df)) return(df[[target]])
  return(rep(NA, nrow(df)))
}

find_subj_col <- function(df) {
  cands <- c("SUBJID", "SUBJ", "SUBJECT", "SUBJECTID", "USUBJID", "ID", "PTID")
  for (c in cands) if (c %in% names(df)) return(c)
  return(NULL)
}

find_date_col <- function(df, exclude = NULL) {
  # prefer known candidates
  prefs <- c("RFSTDTC", "RFSTDT", "EXSTDTC", "EXSTDT", "EXDT", "EXDATE",
             "RANDDT", "RANDDTC", "RANDDATE", "RANDOMDT", "ENROLDT",
             "ENROLDTC", "VISITDT", "STARTDT", "STDT", "STDTC", "DTC", "DATE", "DT")
  for (p in prefs) if (p %in% names(df) && !(p %in% exclude)) return(p)
  # otherwise first column containing DT/DATE/DTC
  for (nm in names(df)) {
    if (nm %in% exclude) next
    if (grepl("DT|DATE|DTC", nm)) return(nm)
  }
  # fallback: any Date-type column
  for (nm in names(df)) {
    if (nm %in% exclude) next
    v <- df[[nm]]
    if (inherits(v, "Date") || inherits(v, "POSIXct")) return(nm)
  }
  return(NULL)
}

extract_subj_min_date <- function(df, label) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  names(df) <- toupper(trimws(names(df)))
  sc <- find_subj_col(df)
  if (is.null(sc)) return(NULL)
  dc <- find_date_col(df, exclude = sc)
  if (is.null(dc)) return(NULL)
  subj <- sas_trim(df[[sc]])
  # if USUBJID already like ONCO-2026-xxx, extract SUBJID part
  subj <- ifelse(grepl("ONCO-2026-", subj, fixed = TRUE),
                 sub(".*ONCO-2026-", "", subj), subj)
  d <- parse_date_smart(df[[dc]])
  keep <- !is.na(subj) & subj != ""
  subj <- subj[keep]; d <- d[keep]
  if (length(subj) == 0) return(NULL)
  t <- tibble(SUBJID = subj, REFDT = d)
  t <- t %>% filter(!is.na(REFDT)) %>% group_by(SUBJID) %>%
    summarise(REFDT = min(REFDT), .groups = "drop")
  t$SRC <- label
  t
}

raw_base <- "/bench/data/raw/faijal-oncology/raw_data"

edc_lb_path   <- file.path(raw_base, "edc_lb.xlsx")
edc_dm_path   <- file.path(raw_base, "edc_dm.xlsx")
edc_ex_path   <- file.path(raw_base, "edc_ex.xlsx")
rand_path     <- file.path(raw_base, "ivrs_rand.xlsx")

edc_lb_raw <- as.data.frame(readxl::read_excel(edc_lb_path, .name_repair = "minimal"), stringsAsFactors = FALSE)
edc_dm_raw <- tryCatch(as.data.frame(readxl::read_excel(edc_dm_path, .name_repair = "minimal"), stringsAsFactors = FALSE), error = function(e) NULL)
edc_ex_raw <- tryCatch(as.data.frame(readxl::read_excel(edc_ex_path, .name_repair = "minimal"), stringsAsFactors = FALSE), error = function(e) NULL)
rand_raw   <- tryCatch(as.data.frame(readxl::read_excel(rand_path, .name_repair = "minimal"), stringsAsFactors = FALSE), error = function(e) NULL)

names(edc_lb_raw) <- toupper(trimws(names(edc_lb_raw)))
if (!is.null(edc_dm_raw)) names(edc_dm_raw) <- toupper(trimws(names(edc_dm_raw)))
if (!is.null(edc_ex_raw)) names(edc_ex_raw) <- toupper(trimws(names(edc_ex_raw)))
if (!is.null(rand_raw))   names(rand_raw)   <- toupper(trimws(names(rand_raw)))

# 1. Prepare DM reference data (derive sdtm_dm RFSTDTC per CDISC:
#    RFSTDTC = first exposure date, else randomization date, else enrollment)
ex_dt   <- extract_subj_min_date(edc_ex_raw, "EX")
rand_dt <- extract_subj_min_date(rand_raw, "RAND")
dm_dt   <- extract_subj_min_date(edc_dm_raw, "DM")

# union all subjects
all_subj <- unique(c(
  sas_trim(get_col(edc_lb_raw, "SUBJID")),
  if (!is.null(ex_dt)) ex_dt$SUBJID else NULL,
  if (!is.null(rand_dt)) rand_dt$SUBJID else NULL,
  if (!is.null(dm_dt)) dm_dt$SUBJID else NULL,
  {
    tmp <- NULL
    if (!is.null(edc_dm_raw)) {
      sc <- find_subj_col(edc_dm_raw)
      if (!is.null(sc)) tmp <- sas_trim(edc_dm_raw[[sc]])
    }
    tmp
  },
  {
    tmp <- NULL
    if (!is.null(rand_raw)) {
      sc <- find_subj_col(rand_raw)
      if (!is.null(sc)) tmp <- sas_trim(rand_raw[[sc]])
    }
    tmp
  },
  {
    tmp <- NULL
    if (!is.null(edc_ex_raw)) {
      sc <- find_subj_col(edc_ex_raw)
      if (!is.null(sc)) tmp <- sas_trim(edc_ex_raw[[sc]])
    }
    tmp
  }
))
all_subj <- all_subj[!is.na(all_subj) & trimws(all_subj) != ""]
# strip ONCO prefix if present
all_subj <- ifelse(grepl("ONCO-2026-", all_subj, fixed = TRUE),
                   sub(".*ONCO-2026-", "", all_subj), trimws(all_subj))
all_subj <- sort(unique(all_subj))

ref_tbl <- tibble(SUBJID = all_subj)
if (!is.null(ex_dt))   ref_tbl <- ref_tbl %>% left_join(ex_dt %>% select(SUBJID, EXDT = REFDT), by = "SUBJID")
else ref_tbl$EXDT <- as.Date(NA)
if (!is.null(rand_dt)) ref_tbl <- ref_tbl %>% left_join(rand_dt %>% select(SUBJID, RANDDT = REFDT), by = "SUBJID")
else ref_tbl$RANDDT <- as.Date(NA)
if (!is.null(dm_dt))   ref_tbl <- ref_tbl %>% left_join(dm_dt %>% select(SUBJID, DMDT = REFDT), by = "SUBJID")
else ref_tbl$DMDT <- as.Date(NA)
ref_tbl$EXDT <- as.Date(ref_tbl$EXDT, origin = "1970-01-01")
ref_tbl$RANDDT <- as.Date(ref_tbl$RANDDT, origin = "1970-01-01")
ref_tbl$DMDT <- as.Date(ref_tbl$DMDT, origin = "1970-01-01")
ref_tbl$REFDT <- as.Date(ifelse(!is.na(ref_tbl$EXDT), ref_tbl$EXDT,
                         ifelse(!is.na(ref_tbl$RANDDT), ref_tbl$RANDDT, ref_tbl$DMDT)),
                         origin = "1970-01-01")

dm_for_lb <- data.frame(
  USUBJID = sas_catx_usubjid(ref_tbl$SUBJID),
  RFSTDTC = ifelse(is.na(ref_tbl$REFDT), "", format(ref_tbl$REFDT, "%Y-%m-%d")),
  stringsAsFactors = FALSE
)
dm_for_lb <- dm_for_lb[order(dm_for_lb$USUBJID), , drop = FALSE]

# 2. Prepare raw LB with USUBJID
n_lb <- nrow(edc_lb_raw)
lb_prep <- data.frame(
  SUBJID = sas_trim(get_col(edc_lb_raw, "SUBJID")),
  TESTCD = sas_trim(get_col(edc_lb_raw, "TESTCD")),
  CAT    = sas_trim(get_col(edc_lb_raw, "CAT")),
  ORRES  = sas_trim(get_col(edc_lb_raw, "ORRES")),
  UNIT   = sas_trim(get_col(edc_lb_raw, "UNIT")),
  LOW    = sas_trim(get_col(edc_lb_raw, "LOW")),
  HIGH   = sas_trim(get_col(edc_lb_raw, "HIGH")),
  VISIT  = get_col(edc_lb_raw, "VISIT"),
  LBDT_raw = get_col(edc_lb_raw, "LBDT"),
  stringsAsFactors = FALSE
)
# VISIT may be Date/numeric; coerce to character preserving
lb_prep$VISIT <- sapply(lb_prep$VISIT, function(v) {
  if (is.na(v)) return(NA_character_)
  if (inherits(v, "Date") || inherits(v, "POSIXct")) return(format(as.Date(v), "%Y-%m-%d"))
  as.character(v)
}, USE.NAMES = FALSE)
# LBDT_raw keep original for parsing (could be Date or character)
lb_prep$USUBJID <- sas_catx_usubjid(lb_prep$SUBJID)
lb_prep <- lb_prep[order(lb_prep$USUBJID), , drop = FALSE]

# 3. Map LB domain (merge lb_prep(in=l) dm_for_lb by USUBJID; if l)
stopifnot("USUBJID" %in% names(lb_prep))
stopifnot("USUBJID" %in% names(dm_for_lb))
sdtm_lb1 <- merge(lb_prep, dm_for_lb, by = "USUBJID", all.x = TRUE, sort = FALSE)
# RFSTDTC missing -> ""
sdtm_lb1$RFSTDTC[is.na(sdtm_lb1$RFSTDTC)] <- ""

sdtm_lb1$STUDYID <- "ONCO-2026"
sdtm_lb1$DOMAIN <- "LB"
sdtm_lb1$LBTESTCD <- substr(ifelse(is.na(sdtm_lb1$TESTCD), "", sdtm_lb1$TESTCD), 1, 8)
sdtm_lb1$LBCAT <- substr(ifelse(is.na(sdtm_lb1$CAT), "", sdtm_lb1$CAT), 1, 15)
sdtm_lb1$LBTEST <- ""
sdtm_lb1$LBTEST[sdtm_lb1$LBTESTCD == "HGB"] <- "Hemoglobin"
sdtm_lb1$LBTEST[sdtm_lb1$LBTESTCD == "ALT"] <- "Alanine Aminotransferase"
sdtm_lb1$LBTEST[is.na(sdtm_lb1$LBTEST)] <- ""

sdtm_lb1$LBORRES <- sapply(sdtm_lb1$ORRES, function(v) {
  if (is.na(v) || trimws(v) == "") return("")
  substr(trimws(v), 1, 10)
}, USE.NAMES = FALSE)
sdtm_lb1$LBORRESU <- sapply(sdtm_lb1$UNIT, function(v) {
  if (is.na(v) || trimws(v) == "") return("")
  substr(trimws(v), 1, 20)
}, USE.NAMES = FALSE)
sdtm_lb1$LBSTRESC <- sdtm_lb1$LBORRES
sdtm_lb1$LBSTRESN <- suppressWarnings(as.numeric(sdtm_lb1$ORRES))
sdtm_lb1$LBSTRESU <- sdtm_lb1$LBORRESU
sdtm_lb1$LBORNRLO <- sapply(sdtm_lb1$LOW, function(v) {
  if (is.na(v) || trimws(v) == "") return("")
  substr(trimws(v), 1, 10)
}, USE.NAMES = FALSE)
sdtm_lb1$LBORNRHI <- sapply(sdtm_lb1$HIGH, function(v) {
  if (is.na(v) || trimws(v) == "") return("")
  substr(trimws(v), 1, 10)
}, USE.NAMES = FALSE)

# VISIT = tranwrd(VISIT,"_"," ")
sdtm_lb1$VISIT <- sapply(sdtm_lb1$VISIT, function(v) {
  if (is.na(v)) return("")
  gsub("_", " ", v, fixed = TRUE)
}, USE.NAMES = FALSE)
sdtm_lb1$VISIT <- substr(sdtm_lb1$VISIT, 1, 20)

# Reference range indicator with SAS missing ordering (missing = -Inf)
low_num <- suppressWarnings(as.numeric(sdtm_lb1$LOW))
high_num <- suppressWarnings(as.numeric(sdtm_lb1$HIGH))
low_thr <- ifelse(is.na(low_num), -Inf, low_num)
high_thr <- ifelse(is.na(high_num), -Inf, high_num)
sdtm_lb1$LBNRIND <- ""
for (i in seq_len(nrow(sdtm_lb1))) {
  v <- sdtm_lb1$LBSTRESN[i]
  if (!is.na(v)) {
    if (v < low_thr[i]) sdtm_lb1$LBNRIND[i] <- "LOW"
    else if (v > high_thr[i]) sdtm_lb1$LBNRIND[i] <- "HIGH"
    else sdtm_lb1$LBNRIND[i] <- "NORMAL"
  } else {
    sdtm_lb1$LBNRIND[i] <- ""
  }
}
sdtm_lb1$LBNRIND <- substr(sdtm_lb1$LBNRIND, 1, 8)

# Date conversion: if LBDT ne "" then LBDTC = put(input(LBDT,date11.),yymmdd10.)
lb_parsed <- parse_date_smart(sdtm_lb1$LBDT_raw)
sdtm_lb1$LBDTC <- ifelse(is.na(lb_parsed), "", format(lb_parsed, "%Y-%m-%d"))

# Study day
rf_parsed <- suppressWarnings(as.Date(ifelse(sdtm_lb1$RFSTDTC == "", NA_character_, sdtm_lb1$RFSTDTC), format = "%Y-%m-%d"))
lbdt_for_day <- suppressWarnings(as.Date(ifelse(sdtm_lb1$LBDTC == "", NA_character_, sdtm_lb1$LBDTC), format = "%Y-%m-%d"))
sdtm_lb1$LBSTDY <- NA_real_
idx <- !is.na(lbdt_for_day) & !is.na(rf_parsed)
sdtm_lb1$LBSTDY[idx & lbdt_for_day >= rf_parsed] <- as.numeric(lbdt_for_day[idx & lbdt_for_day >= rf_parsed] - rf_parsed[idx & lbdt_for_day >= rf_parsed]) + 1
sdtm_lb1$LBSTDY[idx & lbdt_for_day < rf_parsed] <- as.numeric(lbdt_for_day[idx & lbdt_for_day < rf_parsed] - rf_parsed[idx & lbdt_for_day < rf_parsed])

# 4. Derive LBSEQ: sort by USUBJID LBDTC (missing first), then retain
ord <- order(sdtm_lb1$USUBJID, ifelse(sdtm_lb1$LBDTC == "", NA_character_, sdtm_lb1$LBDTC), na.last = FALSE)
sdtm_lb1 <- sdtm_lb1[ord, , drop = FALSE]
sdtm_lb1$LBSEQ <- ave(seq_len(nrow(sdtm_lb1)), sdtm_lb1$USUBJID, FUN = seq_along)

sdtm_lb <- sdtm_lb1
sdtm_lb$LBTEST <- substr(sdtm_lb$LBTEST, 1, 40)
sdtm_lb <- sdtm_lb[, c("STUDYID","DOMAIN","USUBJID","LBSEQ",
  "LBTESTCD","LBTEST","LBCAT",
  "LBORRES","LBORRESU",
  "LBSTRESC","LBSTRESN","LBSTRESU",
  "LBORNRLO","LBORNRHI",
  "LBNRIND",
  "LBDTC","LBSTDY",
  "VISIT")]
# SAS sorts character uppercase before lowercase; R order above approximates for these values
sdtm_lb <- sdtm_lb[order(sdtm_lb$USUBJID, ifelse(sdtm_lb$LBDTC == "", NA_character_, sdtm_lb$LBDTC), na.last = FALSE), ]

# 5/6. View + export
write.csv(sdtm_lb, file.path(out, "lb.csv"), row.names = FALSE, na = "")

# Listing rendering (PROC PRINT DATA=SDTM_LB)
txt_path <- file.path(out, "P026.txt")
lst_path <- file.path(out, "P026.lst")
csv_path <- file.path(out, "P026.csv")
write.csv(sdtm_lb, csv_path, row.names = FALSE, na = "")

con_txt <- file(txt_path, open = "wt")
writeLines("PROC PRINT DATA=SDTM_LB;", con_txt)
writeLines("", con_txt)
writeLines(paste0("Obs  ", paste(names(sdtm_lb), collapse = " | ")), con_txt)
for (i in seq_len(nrow(sdtm_lb))) {
  vals <- sapply(names(sdtm_lb), function(cn) {
    v <- sdtm_lb[i, cn]
    if (is.na(v) || (is.character(v) && v == "")) return("")
    if (is.numeric(v)) {
      # display unrounded; SAS BEST. default
      return(as.character(v))
    }
    as.character(v)
  }, USE.NAMES = FALSE)
  writeLines(paste0(i, "  ", paste(vals, collapse = " | ")), con_txt)
}
close(con_txt)
file.copy(txt_path, lst_path, overwrite = TRUE)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

to_iso <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, c("POSIXct", "POSIXlt", "POSIXt"))) return(format(as.Date(x), "%Y-%m-%d"))
  if (is.numeric(x) && !is.character(x)) {
    return(format(as.Date(x, origin = "1899-12-30"), "%Y-%m-%d"))
  }
  x_chr <- trimws(as.character(x))
  x_chr[x_chr == "" | x_chr == "." | toupper(x_chr) == "NA"] <- NA_character_
  res <- rep(NA_character_, length(x_chr))
  notna <- !is.na(x_chr)
  if (!any(notna)) return(res)
  is_iso <- grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}", x_chr)
  is_iso[is.na(is_iso)] <- FALSE
  if (any(is_iso & notna)) {
    idx <- which(is_iso & notna)
    res[idx] <- format(as.Date(substr(x_chr[idx], 1, 10), format = "%Y-%m-%d"), "%Y-%m-%d")
  }
  need <- notna & !is_iso
  if (any(need)) {
    v <- x_chr[need]
    v_nosep <- gsub("[- /]", "", v)
    v_title <- sapply(v_nosep, function(s) {
      if (is.na(s)) return(NA_character_)
      s_low <- tolower(s)
      loc <- regexpr("[a-z]+", s_low)
      if (loc == -1) return(s_low)
      m <- regmatches(s_low, regexpr("[a-z]+", s_low))
      m_title <- paste0(toupper(substr(m, 1, 1)), substr(m, 2, nchar(m)))
      sub("[a-z]+", m_title, s_low)
    }, USE.NAMES = FALSE)
    parsed <- as.Date(v_title, format = "%d%b%Y")
    has_slash <- grepl("/", v)
    still_na <- is.na(parsed) & has_slash
    if (any(still_na, na.rm = TRUE)) {
      parsed[still_na] <- as.Date(v[still_na], format = "%d/%m/%Y")
    }
    # fallback for pure ISO without dashes already handled; try yyyymmdd
    still_na2 <- is.na(parsed) & grepl("^[0-9]{8}$", v_nosep)
    if (any(still_na2, na.rm = TRUE)) {
      parsed[still_na2] <- as.Date(v_nosep[still_na2], format = "%Y%m%d")
    }
    res[need] <- format(parsed, "%Y-%m-%d")
  }
  res
}

substr_sas <- function(x, len) {
  ifelse(is.na(x), NA_character_, substr(as.character(x), 1, len))
}

find_col <- function(df, candidates) {
  nm <- names(df)
  for (c in candidates) {
    hit <- which(nm == c)
    if (length(hit) > 0) return(nm[hit[1]])
  }
  return(NA_character_)
}
find_subjid_col <- function(df) {
  nm <- names(df)
  if ("SUBJID" %in% nm) return("SUBJID")
  if ("USUBJID" %in% nm) return("USUBJID")
  hit <- nm[grepl("SUBJ", nm)]
  if (length(hit) > 0) return(hit[1])
  return(NA_character_)
}
find_date_cols <- function(df) {
  nm <- names(df)
  dt_type <- sapply(df, function(c) inherits(c, "Date") || inherits(c, c("POSIXct","POSIXlt")))
  byname <- nm[grepl("DAT|DTC|DT$", nm)]
  unique(c(byname, nm[dt_type]))
}

# ---- Read inputs ----
p_vs   <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_vs.xlsx"
p_dm   <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
p_ex   <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"
p_rand <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx"

edc_vs_raw <- as.data.frame(readxl::read_excel(p_vs), stringsAsFactors = FALSE)
edc_dm_raw <- as.data.frame(readxl::read_excel(p_dm), stringsAsFactors = FALSE)
edc_ex_raw <- tryCatch(as.data.frame(readxl::read_excel(p_ex), stringsAsFactors = FALSE), error=function(e) NULL)
ivrs_raw   <- tryCatch(as.data.frame(readxl::read_excel(p_rand), stringsAsFactors = FALSE), error=function(e) NULL)

names(edc_vs_raw) <- toupper(names(edc_vs_raw))
names(edc_dm_raw) <- toupper(names(edc_dm_raw))
if (!is.null(edc_ex_raw)) names(edc_ex_raw) <- toupper(names(edc_ex_raw))
if (!is.null(ivrs_raw))   names(ivrs_raw)   <- toupper(names(ivrs_raw))

# Identify edc_vs columns (case-sensitive after upper)
c_SUBJID <- find_col(edc_vs_raw, c("SUBJID"))
c_TESTCD <- find_col(edc_vs_raw, c("TESTCD"))
c_ORRES  <- find_col(edc_vs_raw, c("ORRES"))
c_UNIT   <- find_col(edc_vs_raw, c("UNIT"))
c_VISIT  <- find_col(edc_vs_raw, c("VISIT"))
c_POS    <- find_col(edc_vs_raw, c("POS"))
c_VSDT   <- find_col(edc_vs_raw, c("VSDT"))
stopifnot(!is.na(c_SUBJID), !is.na(c_TESTCD), !is.na(c_ORRES))

# ---- Derive dm_for_vs (USUBJID SUBJID RFSTDTC RFXSTDTC) ----
dm_subj_col <- find_subjid_col(edc_dm_raw)
stopifnot(!is.na(dm_subj_col))
base_subj <- unique(trimws(as.character(edc_dm_raw[[dm_subj_col]])))
base_subj <- base_subj[!is.na(base_subj) & base_subj != ""]
dm_base <- data.frame(SUBJID = base_subj, stringsAsFactors = FALSE)
dm_base$USUBJID <- paste0("ONCO-2026-", trimws(dm_base$SUBJID))

# Try direct RFSTDTC / RFXSTDTC in edc_dm
rf_col_dm  <- find_col(edc_dm_raw, c("RFSTDTC","RFSTDT","RFSTDAT","RANDDTC","RANDDT"))
rfx_col_dm <- find_col(edc_dm_raw, c("RFXSTDTC","RFXSTDT","RFXSTDAT","EXSTDTC","EXSTDT"))
rf_map <- NULL
rfx_map <- NULL
if (!is.na(rf_col_dm)) {
  tmp <- edc_dm_raw[, c(dm_subj_col, rf_col_dm), drop = FALSE]
  names(tmp) <- c("SUBJID","DTRAW")
  tmp$SUBJID <- trimws(as.character(tmp$SUBJID))
  tmp$RFSTDTC <- to_iso(tmp$DTRAW)
  rf_map <- tmp[, c("SUBJID","RFSTDTC"), drop = FALSE]
}
if (!is.na(rfx_col_dm)) {
  tmp <- edc_dm_raw[, c(dm_subj_col, rfx_col_dm), drop = FALSE]
  names(tmp) <- c("SUBJID","DTRAW")
  tmp$SUBJID <- trimws(as.character(tmp$SUBJID))
  tmp$RFXSTDTC <- to_iso(tmp$DTRAW)
  rfx_map <- tmp[, c("SUBJID","RFXSTDTC"), drop = FALSE]
}

# RFX from edc_ex (first exposure per subject)
if (is.null(rfx_map) && !is.null(edc_ex_raw)) {
  ex_subj <- find_subjid_col(edc_ex_raw)
  if (!is.na(ex_subj)) {
    dcands <- find_date_cols(edc_ex_raw)
    # prefer EX+ST
    pref <- dcands[grepl("EX", dcands) & grepl("ST", dcands)]
    if (length(pref) == 0) pref <- dcands[grepl("EX", dcands)]
    if (length(pref) == 0) pref <- dcands
    if (length(pref) > 0) {
      exd <- edc_ex_raw[, c(ex_subj, pref[1]), drop = FALSE]
      names(exd) <- c("SUBJID","DTRAW")
      exd$SUBJID <- trimws(as.character(exd$SUBJID))
      exd$RFXSTDTC <- to_iso(exd$DTRAW)
      exd <- exd[!is.na(exd$RFXSTDTC) & exd$SUBJID != "", , drop = FALSE]
      if (nrow(exd) > 0) {
        exd <- exd[order(exd$SUBJID, exd$RFXSTDTC), , drop = FALSE]
        rfx_map <- exd[!duplicated(exd$SUBJID), c("SUBJID","RFXSTDTC"), drop = FALSE]
      }
    }
  }
}
# RF from ivrs_rand (randomisation per subject)
if (is.null(rf_map) && !is.null(ivrs_raw)) {
  r_subj <- find_subjid_col(ivrs_raw)
  if (!is.na(r_subj)) {
    dcands <- find_date_cols(ivrs_raw)
    pref <- dcands[grepl("RAND", dcands)]
    if (length(pref) == 0) pref <- dcands[grepl("RANDOM", dcands)]
    if (length(pref) == 0) pref <- dcands
    if (length(pref) > 0) {
      rd <- ivrs_raw[, c(r_subj, pref[1]), drop = FALSE]
      names(rd) <- c("SUBJID","DTRAW")
      rd$SUBJID <- trimws(as.character(rd$SUBJID))
      rd$RFSTDTC <- to_iso(rd$DTRAW)
      rd <- rd[!is.na(rd$RFSTDTC) & rd$SUBJID != "", , drop = FALSE]
      if (nrow(rd) > 0) {
        rd <- rd[order(rd$SUBJID, rd$RFSTDTC), , drop = FALSE]
        rf_map <- rd[!duplicated(rd$SUBJID), c("SUBJID","RFSTDTC"), drop = FALSE]
      }
    }
  }
}
# Fallback: any date col in edc_dm as RFSTDTC
if (is.null(rf_map)) {
  dcands <- find_date_cols(edc_dm_raw)
  # exclude birth date
  dcands2 <- dcands[!grepl("BRTH|BIRTH|DOB", dcands)]
  if (length(dcands2) > 0) {
    tmp <- edc_dm_raw[, c(dm_subj_col, dcands2[1]), drop = FALSE]
    names(tmp) <- c("SUBJID","DTRAW")
    tmp$SUBJID <- trimws(as.character(tmp$SUBJID))
    tmp$RFSTDTC <- to_iso(tmp$DTRAW)
    rf_map <- tmp[, c("SUBJID","RFSTDTC"), drop = FALSE]
  }
}

dm_for_vs <- dm_base
if (!is.null(rf_map)) {
  rf_map <- rf_map[!duplicated(rf_map$SUBJID), , drop = FALSE]
  dm_for_vs <- merge(dm_for_vs, rf_map, by = "SUBJID", all.x = TRUE)
} else {
  dm_for_vs$RFSTDTC <- NA_character_
}
if (!is.null(rfx_map)) {
  rfx_map <- rfx_map[!duplicated(rfx_map$SUBJID), , drop = FALSE]
  dm_for_vs <- merge(dm_for_vs, rfx_map, by = "SUBJID", all.x = TRUE)
} else {
  dm_for_vs$RFXSTDTC <- NA_character_
}
# RFSTDTC defaults to RFXSTDTC if missing (reference start = first exposure)
idx <- is.na(dm_for_vs$RFSTDTC) | dm_for_vs$RFSTDTC == ""
dm_for_vs$RFSTDTC[idx] <- dm_for_vs$RFXSTDTC[idx]
# sort by USUBJID (SAS sort)
dm_for_vs <- dm_for_vs[order(dm_for_vs$USUBJID), , drop = FALSE]

# ---- 1. Prepare Raw Data with USUBJID : vs_prep ----
vs_prep <- edc_vs_raw
vs_prep$SUBJID_C <- trimws(as.character(vs_prep[[c_SUBJID]]))
vs_prep$USUBJID <- paste0("ONCO-2026-", vs_prep$SUBJID_C)
vs_prep <- vs_prep[order(vs_prep$USUBJID), , drop = FALSE]

# Keep only needed + USUBJID for merge; drop overlapping SUBJID from dm side later
# ---- 2. Map VS Domain : sdtm_vs1 ----
# dm side keep only USUBJID RFSTDTC RFXSTDTC (as SAS does) to avoid .x/.y
dm_keep <- dm_for_vs[, c("USUBJID","RFSTDTC","RFXSTDTC"), drop = FALSE]
dm_keep <- dm_keep[!duplicated(dm_keep$USUBJID), , drop = FALSE]

# left join (merge ... if v)
m <- merge(vs_prep, dm_keep, by = "USUBJID", all.x = TRUE, sort = FALSE)
stopifnot("RFSTDTC" %in% names(m))

getC <- function(df, col) if (!is.na(col)) trimws(as.character(df[[col]])) else rep(NA_character_, nrow(df))

TESTCD_v <- getC(m, c_TESTCD)
ORRES_v  <- getC(m, c_ORRES)
UNIT_v   <- if (!is.na(c_UNIT)) getC(m, c_UNIT) else rep(NA_character_, nrow(m))
VISIT_v  <- if (!is.na(c_VISIT)) getC(m, c_VISIT) else rep(NA_character_, nrow(m))
POS_v    <- if (!is.na(c_POS)) getC(m, c_POS) else rep(NA_character_, nrow(m))
VSDT_v   <- if (!is.na(c_VSDT)) trimws(as.character(m[[c_VSDT]])) else rep(NA_character_, nrow(m))
# handle Date-typed VSDT
if (!is.na(c_VSDT) && (inherits(m[[c_VSDT]], "Date") || inherits(m[[c_VSDT]], c("POSIXct","POSIXlt")) || is.numeric(m[[c_VSDT]]))) {
  VSDTC_v <- to_iso(m[[c_VSDT]])
} else {
  VSDT_chr <- VSDT_v
  VSDT_chr[VSDT_chr == ""] <- NA_character_
  VSDTC_v <- rep(NA_character_, length(VSDT_chr))
  has <- !is.na(VSDT_chr)
  if (any(has)) VSDTC_v[has] <- to_iso(VSDT_chr[has])
}

sdtm_vs1 <- data.frame(
  USUBJID = m$USUBJID,
  RFSTDTC = ifelse(is.na(m$RFSTDTC), NA_character_, m$RFSTDTC),
  RFXSTDTC = ifelse(is.na(m$RFXSTDTC), NA_character_, m$RFXSTDTC),
  TESTCD = TESTCD_v,
  ORRES = ORRES_v,
  UNIT = UNIT_v,
  VISIT_RAW = VISIT_v,
  POS_RAW = POS_v,
  VSDT_RAW = VSDT_v,
  VSDTC = VSDTC_v,
  stringsAsFactors = FALSE
)

sdtm_vs1$STUDYID <- "ONCO-2026"
sdtm_vs1$DOMAIN <- "VS"
sdtm_vs1$VSTESTCD <- substr_sas(sdtm_vs1$TESTCD, 8)
sdtm_vs1$VSTEST <- ifelse(sdtm_vs1$VSTESTCD == "SYSBP", "Systolic Blood Pressure",
                   ifelse(sdtm_vs1$VSTESTCD == "DIABP", "Diastolic Blood Pressure", NA_character_))
sdtm_vs1$VSTEST <- substr_sas(sdtm_vs1$VSTEST, 40)
sdtm_vs1$VSORRES <- substr_sas(sdtm_vs1$ORRES, 10)
sdtm_vs1$VSORRESU <- substr_sas(sdtm_vs1$UNIT, 20)
sdtm_vs1$VSSTRESC <- substr_sas(sdtm_vs1$ORRES, 10)
sdtm_vs1$VSSTRESN <- suppressWarnings(as.numeric(sdtm_vs1$ORRES))
sdtm_vs1$VSSTRESU <- substr_sas(sdtm_vs1$UNIT, 20)
sdtm_vs1$VISIT <- substr_sas(gsub("_", " ", sdtm_vs1$VISIT_RAW, fixed = TRUE), 20)
sdtm_vs1$VSPOS <- substr_sas(toupper(sdtm_vs1$POS_RAW), 10)
sdtm_vs1$VSPOS[sdtm_vs1$POS_RAW == "" | is.na(sdtm_vs1$POS_RAW)] <- NA_character_
sdtm_vs1$VISIT[sdtm_vs1$VISIT_RAW == "" | is.na(sdtm_vs1$VISIT_RAW)] <- NA_character_
# Study Day
sdtm_vs1$VSTDY <- NA_real_
stdt <- as.Date(sdtm_vs1$VSDTC, format = "%Y-%m-%d")
rfdt <- as.Date(sdtm_vs1$RFSTDTC, format = "%Y-%m-%d")
ok <- !is.na(stdt) & !is.na(rfdt)
diffd <- as.integer(stdt[ok] - rfdt[ok])
sdtm_vs1$VSTDY[ok] <- ifelse(diffd >= 0, diffd + 1, diffd)

# sort by USUBJID VSDTC (missing first, SAS)
sdtm_vs1$VSDTC_sort <- ifelse(is.na(sdtm_vs1$VSDTC), "", sdtm_vs1$VSDTC)
ord <- order(sdtm_vs1$USUBJID, sdtm_vs1$VSDTC_sort == "", sdtm_vs1$VSDTC_sort, sdtm_vs1$VSTESTCD)
# SAS missing sorts first: adjust so "" first
ord <- order(sdtm_vs1$USUBJID, ifelse(sdtm_vs1$VSDTC_sort == "", 0, 1), sdtm_vs1$VSDTC_sort, sdtm_vs1$VSTESTCD)
sdtm_vs1 <- sdtm_vs1[ord, , drop = FALSE]

# ---- 3. Derive VSSEQ ----
sdtm_vs1 <- sdtm_vs1[order(sdtm_vs1$USUBJID, ifelse(is.na(sdtm_vs1$VSDTC), "", sdtm_vs1$VSDTC)), , drop = FALSE]
sdtm_vs1$VSSEQ <- ave(seq_len(nrow(sdtm_vs1)), sdtm_vs1$USUBJID, FUN = seq_along)

sdtm_vs <- data.frame(
  STUDYID = substr_sas(sdtm_vs1$STUDYID, 10),
  DOMAIN = substr_sas(sdtm_vs1$DOMAIN, 2),
  USUBJID = substr_sas(sdtm_vs1$USUBJID, 25),
  VSSEQ = sdtm_vs1$VSSEQ,
  VSTESTCD = substr_sas(sdtm_vs1$VSTESTCD, 8),
  VSTEST = substr_sas(sdtm_vs1$VSTEST, 40),
  VSORRES = substr_sas(sdtm_vs1$VSORRES, 10),
  VSORRESU = substr_sas(sdtm_vs1$VSORRESU, 20),
  VSSTRESC = substr_sas(sdtm_vs1$VSSTRESC, 10),
  VSSTRESN = sdtm_vs1$VSSTRESN,
  VSSTRESU = substr_sas(sdtm_vs1$VSSTRESU, 20),
  VSDTC = sdtm_vs1$VSDTC,
  VSTDY = sdtm_vs1$VSTDY,
  VISIT = substr_sas(sdtm_vs1$VISIT, 20),
  VSPOS = substr_sas(sdtm_vs1$VSPOS, 10),
  stringsAsFactors = FALSE
)
# final sort by USUBJID VSDTC (VSSEQ already in that order)
sdtm_vs <- sdtm_vs[order(sdtm_vs$USUBJID, ifelse(is.na(sdtm_vs$VSDTC), "", sdtm_vs$VSDTC), sdtm_vs$VSSEQ), , drop = FALSE]
rownames(sdtm_vs) <- NULL

# ---- Write datasets ----
write.csv(sdtm_vs, file.path(out, "sdtm_vs.csv"), row.names = FALSE, na = "")
write.csv(sdtm_vs, file.path(out, "P030.csv"), row.names = FALSE, na = "")

# ---- PROC PRINT rendering ----
txt_path <- file.path(out, "P030.txt")
lst_path <- file.path(out, "P030.lst.txt")
con_lines <- c(
  "Listing of SDTM VS (P030_vs.sas - PROC PRINT of SDTM_VS)",
  paste0("Rows: ", nrow(sdtm_vs)),
  ""
)
hdr <- paste(names(sdtm_vs), collapse = " | ")
con_lines <- c(con_lines, hdr, paste(rep("-", nchar(hdr)), collapse = ""))
for (i in seq_len(nrow(sdtm_vs))) {
  vals <- sapply(names(sdtm_vs), function(cn) {
    v <- sdtm_vs[i, cn]
    if (is.na(v)) "" else as.character(v)
  }, USE.NAMES = FALSE)
  con_lines <- c(con_lines, paste(vals, collapse = " | "))
}
con_lines <- c(con_lines, "")
writeLines(con_lines, txt_path)
writeLines(con_lines, lst_path)
message("Done: ", nrow(sdtm_vs), " rows written.")

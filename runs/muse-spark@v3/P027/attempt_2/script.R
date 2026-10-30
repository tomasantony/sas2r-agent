# P027_rs.sas -> R translation
# SDTM RS mapping & derivation + PROC PRINT listing
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
})

raw_base <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
path_rs   <- file.path(raw_base, "edc_rs.xlsx")
path_dm   <- file.path(raw_base, "edc_dm.xlsx")
path_ex   <- file.path(raw_base, "edc_ex.xlsx")
path_rand <- file.path(raw_base, "ivrs_rand.xlsx")

cat("Reading inputs...\n")
edc_rs_raw <- as.data.frame(readxl::read_excel(path_rs, .name_repair = "minimal"), stringsAsFactors = FALSE)
edc_dm_raw <- as.data.frame(readxl::read_excel(path_dm, .name_repair = "minimal"), stringsAsFactors = FALSE)
edc_ex_raw <- as.data.frame(readxl::read_excel(path_ex, .name_repair = "minimal"), stringsAsFactors = FALSE)
ivrs_raw   <- as.data.frame(readxl::read_excel(path_rand, .name_repair = "minimal"), stringsAsFactors = FALSE)
names(edc_rs_raw) <- trimws(names(edc_rs_raw))
names(edc_dm_raw) <- trimws(names(edc_dm_raw))
names(edc_ex_raw) <- trimws(names(edc_ex_raw))
names(ivrs_raw)   <- trimws(names(ivrs_raw))
cat("RS cols:", paste(names(edc_rs_raw), collapse="|"), "\n")
cat("DM cols:", paste(names(edc_dm_raw), collapse="|"), "\n")
cat("EX cols:", paste(names(edc_ex_raw), collapse="|"), "\n")
cat("RAND cols:", paste(names(ivrs_raw), collapse="|"), "\n")

# ---- helpers ----
get_col <- function(df, candidates) {
  low <- tolower(names(df))
  for (cand in candidates) {
    idx <- which(low == tolower(cand))
    if (length(idx) > 0) return(names(df)[idx[1]])
  }
  return(NULL)
}

make_usubjid <- function(v) {
  v_chr <- trimws(as.character(v))
  v_chr[v_chr == "" | v_chr == "." | toupper(v_chr) == "NA" | is.na(v_chr)] <- NA_character_
  res <- rep(NA_character_, length(v_chr))
  notna <- !is.na(v_chr)
  already <- grepl("^ONCO-2026-", v_chr)
  already[is.na(already)] <- FALSE
  res[notna & already] <- v_chr[notna & already]
  res[notna & !already] <- paste0("ONCO-2026-", v_chr[notna & !already])
  res
}

# single-format-per-column parser to ISO YYYY-MM-DD
parse_col_to_iso <- function(vec) {
  if (inherits(vec, "Date")) return(format(vec, "%Y-%m-%d"))
  if (inherits(vec, "POSIXt")) return(format(as.Date(vec), "%Y-%m-%d"))
  if (is.numeric(vec)) {
    if (all(is.na(vec) | (vec > 20000 & vec < 60000))) {
      d <- suppressWarnings(as.Date(vec, origin = "1899-12-30"))
      return(format(d, "%Y-%m-%d"))
    } else {
      vec <- as.character(vec)
    }
  }
  x_chr <- trimws(as.character(vec))
  x_chr[x_chr == "" | x_chr == "." | toupper(x_chr) == "NA"] <- NA_character_
  res <- rep(NA_character_, length(x_chr))
  nona <- x_chr[!is.na(x_chr)]
  if (length(nona) == 0) return(res)
  has_alpha <- any(grepl("[A-Za-z]", nona))
  has_slash <- any(grepl("/", nona, fixed = TRUE))
  has_isodash <- any(grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}", nona))
  has_dash_mon <- any(grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4}", nona))
  if (has_alpha || has_dash_mon) {
    norm <- sapply(x_chr, function(v) {
      if (is.na(v)) return(NA_character_)
      m <- regexec("^([0-9]{1,2})[-/ ]([A-Za-z]{3})[-/ ]([0-9]{4})$", trimws(v))
      parts <- regmatches(trimws(v), m)[[1]]
      if (length(parts) == 4) {
        dd <- sprintf("%02d", suppressWarnings(as.integer(parts[2])))
        mon <- paste0(toupper(substr(parts[3], 1, 1)), tolower(substr(parts[3], 2, 3)))
        paste0(dd, "-", mon, "-", parts[4])
      } else trimws(v)
    }, USE.NAMES = FALSE)
    d <- suppressWarnings(as.Date(norm, format = "%d-%b-%Y"))
    res <- format(d, "%Y-%m-%d")
    res[is.na(d)] <- NA_character_
    return(res)
  } else if (has_slash) {
    first_fields <- suppressWarnings(as.integer(sub("/.*$", "", nona)))
    first_fields <- first_fields[!is.na(first_fields)]
    is_dmy <- length(first_fields) > 0 && any(first_fields > 12, na.rm = TRUE)
    fmt <- if (is_dmy) "%d/%m/%Y" else "%m/%d/%Y"
    d <- suppressWarnings(as.Date(x_chr, format = fmt))
    need <- is.na(d) & !is.na(x_chr)
    if (any(need)) {
      fmt2 <- if (is_dmy) "%d/%m/%y" else "%m/%d/%y"
      d2 <- suppressWarnings(as.Date(x_chr[need], format = fmt2))
      d[need] <- d2
    }
    res <- format(d, "%Y-%m-%d")
    res[is.na(d)] <- NA_character_
    return(res)
  } else if (has_isodash) {
    d <- suppressWarnings(as.Date(substr(x_chr, 1, 10), format = "%Y-%m-%d"))
    res <- format(d, "%Y-%m-%d")
    res[is.na(d)] <- NA_character_
    return(res)
  } else {
    d <- suppressWarnings(as.Date(substr(x_chr, 1, 10), format = "%Y-%m-%d"))
    res <- format(d, "%Y-%m-%d")
    res[is.na(d)] <- NA_character_
    return(res)
  }
}

# strict SAS date11. (DD-MON-YYYY) -> YYYY-MM-DD, ONE format only
# but handle Date/POSIXct/numeric Excel serials directly (as SAS PROC IMPORT would)
parse_date11_to_iso <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXt")) return(format(as.Date(x), "%Y-%m-%d"))
  if (is.numeric(x)) {
    d <- suppressWarnings(as.Date(x, origin = "1899-12-30"))
    # only treat as Excel serial if plausible; else NA
    iso <- format(d, "%Y-%m-%d")
    iso[is.na(d)] <- NA_character_
    # if values look like SAS dates (>20000) they are Excel-like here; keep
    return(iso)
  }
  x_chr <- trimws(as.character(x))
  res <- rep(NA_character_, length(x_chr))
  miss <- is.na(x_chr) | x_chr == "" | x_chr == "."
  idx <- which(!miss)
  if (length(idx) == 0) return(res)
  vals <- x_chr[idx]
  norm <- sapply(vals, function(v) {
    m <- regexec("^([0-9]{1,2})-([A-Za-z]{3})-([0-9]{4})$", trimws(v))
    parts <- regmatches(trimws(v), m)[[1]]
    if (length(parts) == 4) {
      dd <- sprintf("%02d", suppressWarnings(as.integer(parts[2])))
      mon_raw <- parts[3]
      mon <- paste0(toupper(substr(mon_raw, 1, 1)), tolower(substr(mon_raw, 2, 3)))
      yyyy <- parts[4]
      paste0(dd, "-", mon, "-", yyyy)
    } else {
      trimws(v)
    }
  }, USE.NAMES = FALSE)
  d <- suppressWarnings(as.Date(norm, format = "%d-%b-%Y"))
  iso <- format(d, "%Y-%m-%d")
  iso[is.na(d)] <- NA_character_
  res[idx] <- iso
  res
}

cat("Deriving DM reference (RFSTDTC) per CDISC (first exposure / randomisation)...\n")
subj_cands <- c("SUBJID", "SUBJ", "USUBJID", "PTNO", "PATIENT")

# DM: ONLY explicit RFSTDTC-like column; never substitute DOB/birth/inform dates
dm_rf_col <- get_col(edc_dm_raw, c("RFSTDTC","RFSTDT","RFSTD","RFST_DT","RFSTARTDT"))
dm_min <- NULL
if (!is.null(dm_rf_col)) {
  subj_dm <- get_col(edc_dm_raw, subj_cands)
  usub_dm <- make_usubjid(edc_dm_raw[[subj_dm]])
  iso_dm <- parse_col_to_iso(edc_dm_raw[[dm_rf_col]])
  d0 <- data.frame(USUBJID=usub_dm, DT=iso_dm, stringsAsFactors=FALSE)
  d0 <- d0[!is.na(d0$USUBJID) & d0$USUBJID!="" & !is.na(d0$DT),,drop=FALSE]
  if (nrow(d0)>0) dm_min <- aggregate(DT ~ USUBJID, data=d0, FUN=function(v) min(v, na.rm=TRUE))
}

# EX: exposure start columns only (exclude END)
ex_subj_col <- get_col(edc_ex_raw, subj_cands)
if (is.null(ex_subj_col)) ex_subj_col <- get_col(edc_ex_raw, names(edc_ex_raw)[grepl("SUBJ|PTNO|PAT", names(edc_ex_raw), ignore.case=TRUE)])
ex_usub <- make_usubjid(edc_ex_raw[[ex_subj_col]])
ex_cands <- names(edc_ex_raw)[grepl("EXST|STDTC|STDT|START|EXDT|FIRSTDOSE|DOS.*ST|TREAT.*ST", names(edc_ex_raw), ignore.case=TRUE)]
ex_cands <- ex_cands[!grepl("END|ENRT|EN$", ex_cands, ignore.case=TRUE)]
# drop subject col itself if caught
ex_cands <- setdiff(ex_cands, ex_subj_col)
if (length(ex_cands)==0) {
  # fallback: any DT/DATE col excluding birth/death/end
  alld <- names(edc_ex_raw)[grepl("DT|DATE|DTC", names(edc_ex_raw), ignore.case=TRUE)]
  alld <- alld[!grepl("BRTH|BIRTH|DOB|DEATH|DTH|END|DISC|INFORM|CONSENT", alld, ignore.case=TRUE)]
  alld <- setdiff(alld, ex_subj_col)
  ex_cands <- alld
}
cat("EX start candidate cols:", paste(ex_cands, collapse="|"), "\n")
ex_min <- NULL
if (length(ex_cands)>0) {
  iso_list <- lapply(ex_cands, function(cn) parse_col_to_iso(edc_ex_raw[[cn]]))
  iso_mat <- do.call(cbind, lapply(iso_list, function(v) v))
  # row-wise min across candidate cols
  row_min <- apply(iso_mat, 1, function(r) { r <- r[!is.na(r)]; if (length(r)==0) NA_character_ else min(r) })
  d0 <- data.frame(USUBJID=ex_usub, DT=row_min, stringsAsFactors=FALSE)
  d0 <- d0[!is.na(d0$USUBJID) & d0$USUBJID!="" & !is.na(d0$DT),,drop=FALSE]
  if (nrow(d0)>0) ex_min <- aggregate(DT ~ USUBJID, data=d0, FUN=function(v) min(v, na.rm=TRUE))
}

# RAND: columns containing RAND
rand_subj_col <- get_col(ivrs_raw, subj_cands)
if (is.null(rand_subj_col)) rand_subj_col <- get_col(ivrs_raw, names(ivrs_raw)[grepl("SUBJ|PTNO|PAT", names(ivrs_raw), ignore.case=TRUE)])
rand_usub <- make_usubjid(ivrs_raw[[rand_subj_col]])
rand_cands <- names(ivrs_raw)[grepl("RAND", names(ivrs_raw), ignore.case=TRUE) & grepl("DT|DATE|DTC", names(ivrs_raw), ignore.case=TRUE)]
if (length(rand_cands)==0) rand_cands <- names(ivrs_raw)[grepl("RAND", names(ivrs_raw), ignore.case=TRUE)]
# exclude treatment arm cols (no date)
if (length(rand_cands)>0) {
  keep <- sapply(rand_cands, function(cn) {
    v <- ivrs_raw[[cn]]
    if (is.numeric(v)) return(TRUE)
    vv <- trimws(as.character(v)); vv <- vv[!is.na(vv) & vv!=""]
    if (length(vv)==0) return(FALSE)
    mean(grepl("[0-9]{2,4}[-/][0-9A-Za-z]{1,3}[-/][0-9]{2,4}|^[0-9]{4}-[0-9]{2}-[0-9]{2}", vv), na.rm=TRUE) > 0.3 || inherits(v,"Date") || inherits(v,"POSIXt")
  })
  rand_cands <- rand_cands[keep]
}
cat("RAND candidate cols:", paste(rand_cands, collapse="|"), "\n")
rand_min <- NULL
if (length(rand_cands)>0) {
  iso_list <- lapply(rand_cands, function(cn) parse_col_to_iso(ivrs_raw[[cn]]))
  iso_mat <- do.call(cbind, lapply(iso_list, function(v) v))
  row_min <- apply(iso_mat, 1, function(r) { r <- r[!is.na(r)]; if (length(r)==0) NA_character_ else min(r) })
  d0 <- data.frame(USUBJID=rand_usub, DT=row_min, stringsAsFactors=FALSE)
  d0 <- d0[!is.na(d0$USUBJID) & d0$USUBJID!="" & !is.na(d0$DT),,drop=FALSE]
  if (nrow(d0)>0) rand_min <- aggregate(DT ~ USUBJID, data=d0, FUN=function(v) min(v, na.rm=TRUE))
}

# union subjects
c_subjid_rs <- get_col(edc_rs_raw, c("SUBJID", "SUBJ", "USUBJID"))
all_usub <- unique(c(
  if (!is.null(dm_min)) dm_min$USUBJID,
  if (!is.null(ex_min)) ex_min$USUBJID,
  if (!is.null(rand_min)) rand_min$USUBJID,
  make_usubjid(edc_rs_raw[[c_subjid_rs]])
))
all_usub <- all_usub[!is.na(all_usub)]
dm_for_rs <- data.frame(USUBJID = sort(unique(all_usub)), stringsAsFactors = FALSE)
if (!is.null(dm_min)) { names(dm_min)[names(dm_min)=="DT"] <- "DT_DM"; dm_for_rs <- merge(dm_for_rs, dm_min, by="USUBJID", all.x=TRUE, sort=FALSE) } else dm_for_rs$DT_DM <- NA_character_
if (!is.null(ex_min)) { names(ex_min)[names(ex_min)=="DT"] <- "DT_EX"; dm_for_rs <- merge(dm_for_rs, ex_min, by="USUBJID", all.x=TRUE, sort=FALSE) } else dm_for_rs$DT_EX <- NA_character_
if (!is.null(rand_min)) { names(rand_min)[names(rand_min)=="DT"] <- "DT_RAND"; dm_for_rs <- merge(dm_for_rs, rand_min, by="USUBJID", all.x=TRUE, sort=FALSE) } else dm_for_rs$DT_RAND <- NA_character_
# CDISC RFSTDTC: first exposure if present else randomisation (explicit RFSTDTC first if present)
dm_for_rs$RFSTDTC <- dplyr::coalesce(dm_for_rs$DT_DM, dm_for_rs$DT_EX, dm_for_rs$DT_RAND)
# if both EX and RAND present, use earliest (first exposure vs randomisation)
both_idx <- !is.na(dm_for_rs$DT_EX) & !is.na(dm_for_rs$DT_RAND)
dm_for_rs$RFSTDTC[both_idx] <- pmin(dm_for_rs$DT_EX[both_idx], dm_for_rs$DT_RAND[both_idx])
dm_for_rs <- dm_for_rs[, c("USUBJID", "RFSTDTC"), drop = FALSE]
dm_for_rs <- dm_for_rs[order(dm_for_rs$USUBJID, na.last = FALSE), , drop = FALSE]
rownames(dm_for_rs) <- NULL
print(dm_for_rs)

# ---- 2. Prepare Raw Data: rs_prep ----
cat("Preparing RS raw...\n")
c_testcd <- get_col(edc_rs_raw, c("TESTCD", "TEST", "RSTESTCD"))
c_orres  <- get_col(edc_rs_raw, c("ORRES", "RSORRES", "RESULT", "RESPONSE"))
c_visit  <- get_col(edc_rs_raw, c("VISIT", "VISITNAME", "VISIT_NM"))
c_rsdt   <- get_col(edc_rs_raw, c("RSDT", "RSDTC", "DATE", "RSDATE", "ASSESSDT"))
stopifnot(!is.null(c_subjid_rs), !is.null(c_testcd), !is.null(c_orres), !is.null(c_visit), !is.null(c_rsdt))

rs_prep <- data.frame(
  SUBJID_raw = trimws(as.character(edc_rs_raw[[c_subjid_rs]])),
  TESTCD_raw = trimws(as.character(edc_rs_raw[[c_testcd]])),
  ORRES_raw  = trimws(as.character(edc_rs_raw[[c_orres]])),
  VISIT_raw  = trimws(as.character(edc_rs_raw[[c_visit]])),
  stringsAsFactors = FALSE
)
rs_prep$RSDT_raw <- edc_rs_raw[[c_rsdt]]
rs_prep$SUBJID_raw[rs_prep$SUBJID_raw == "" | is.na(rs_prep$SUBJID_raw)] <- NA_character_
rs_prep$USUBJID <- ifelse(is.na(rs_prep$SUBJID_raw), "ONCO-2026",
                          paste0("ONCO-2026-", rs_prep$SUBJID_raw))
rs_prep$TESTCD <- rs_prep$TESTCD_raw
rs_prep$ORRES  <- rs_prep$ORRES_raw
rs_prep$VISIT  <- rs_prep$VISIT_raw
rs_prep$RSDT   <- rs_prep$RSDT_raw
rs_prep <- rs_prep[order(rs_prep$USUBJID, na.last = FALSE), , drop = FALSE]

# ---- 3. Map RS Domain ----
cat("Mapping RS domain...\n")
dm_for_rs2 <- dm_for_rs[, c("USUBJID", "RFSTDTC"), drop = FALSE]
stopifnot("USUBJID" %in% names(rs_prep), "USUBJID" %in% names(dm_for_rs2))
sdtm_rs1 <- merge(rs_prep, dm_for_rs2, by = "USUBJID", all.x = TRUE, sort = FALSE)

sdtm_rs1$STUDYID <- "ONCO-2026"
sdtm_rs1$DOMAIN  <- "RS"
sdtm_rs1$RSTESTCD <- trimws(as.character(sdtm_rs1$TESTCD))
sdtm_rs1$RSTEST <- ifelse(!is.na(sdtm_rs1$RSTESTCD) & sdtm_rs1$RSTESTCD == "OVRLRESP",
                          "Overall Response", NA_character_)
sdtm_rs1$RSCAT <- "RECIST 1.1"
sdtm_rs1$RSORRES <- toupper(trimws(as.character(sdtm_rs1$ORRES)))
sdtm_rs1$RSORRES[is.na(sdtm_rs1$ORRES) | trimws(as.character(sdtm_rs1$ORRES)) == ""] <- NA_character_
sdtm_rs1$RSSTRESC <- sdtm_rs1$RSORRES
sdtm_rs1$RSEVAL <- "INVESTIGATOR"
vis <- trimws(as.character(sdtm_rs1$VISIT))
vis[is.na(vis)] <- NA_character_
sdtm_rs1$VISIT <- ifelse(is.na(vis), NA_character_, toupper(gsub("_", " ", vis, fixed = TRUE)))
# Date conversion: if RSDT ne "" then RSDTC = put(input(RSDT,date11.),yymmdd10.)
# emulate SAS: only non-blank RSDT converted via date11.
rsdt_char <- trimws(as.character(sdtm_rs1$RSDT))
# for Date/POSIXct/numeric readxl types, as.character gives ISO/serial; handle via dedicated parser
sdtm_rs1$RSDTC <- parse_date11_to_iso(sdtm_rs1$RSDT)
# blank RSDT -> blank RSDTC
blank_rsdt <- is.na(sdtm_rs1$RSDT) | trimws(as.character(sdtm_rs1$RSDT)) == "" | trimws(as.character(sdtm_rs1$RSDT)) == "."
sdtm_rs1$RSDTC[blank_rsdt] <- NA_character_
# Study Day: SAS input(RSDTC,yymmdd10.) and input(RFSTDTC,yymmdd10.)
rf_iso <- substr(trimws(as.character(sdtm_rs1$RFSTDTC)), 1, 10)
rf_iso[rf_iso == "" | is.na(sdtm_rs1$RFSTDTC)] <- NA_character_
rs_iso <- sdtm_rs1$RSDTC
stdt <- suppressWarnings(as.Date(rs_iso, format = "%Y-%m-%d"))
rfdt <- suppressWarnings(as.Date(rf_iso, format = "%Y-%m-%d"))
diffd <- as.integer(stdt - rfdt)
sdtm_rs1$RSSTDY <- ifelse(!is.na(stdt) & !is.na(rfdt),
                          ifelse(stdt >= rfdt, diffd + 1L, diffd),
                          NA_integer_)
print(sdtm_rs1[, c("USUBJID","RSDTC","RFSTDTC","RSSTDY")])
sdtm_rs1 <- sdtm_rs1[, c("STUDYID", "DOMAIN", "USUBJID", "RSTESTCD", "RSTEST", "RSCAT",
                          "RSORRES", "RSSTRESC", "RSEVAL", "RSDTC", "RSSTDY", "VISIT"), drop = FALSE]

# ---- 4. Derive RSSEQ ----
cat("Deriving RSSEQ...\n")
ord <- order(sdtm_rs1$USUBJID, sdtm_rs1$RSDTC, na.last = FALSE)
sdtm_rs1 <- sdtm_rs1[ord, , drop = FALSE]
sdtm_rs1$RSSEQ <- ave(seq_len(nrow(sdtm_rs1)), sdtm_rs1$USUBJID,
                      FUN = function(x) seq_along(x))
sdtm_rs1$RSSEQ <- as.integer(sdtm_rs1$RSSEQ)
sdtm_rs <- sdtm_rs1[, c("STUDYID", "DOMAIN", "USUBJID", "RSSEQ",
                         "RSTESTCD", "RSTEST", "RSCAT",
                         "RSORRES", "RSSTRESC", "RSEVAL",
                         "RSDTC", "RSSTDY", "VISIT"), drop = FALSE]
rownames(sdtm_rs) <- NULL

# ---- 5/6. Outputs ----
cat("Writing outputs...\n")
write.csv(sdtm_rs, file.path(out, "rs.csv"), row.names = FALSE, na = "")

print_df <- sdtm_rs
txt_path <- file.path(out, "P027.txt")
lst_path <- file.path(out, "P027.lst")
csv_path <- file.path(out, "P027.csv")

hdr <- c("Obs", "STUDYID", "DOMAIN", "USUBJID", "RSSEQ", "RSTESTCD", "RSTEST",
         "RSCAT", "RSORRES", "RSSTRESC", "RSEVAL", "RSDTC", "RSSTDY", "VISIT")
fmt_row <- function(obs, r) {
  rsseq_c <- ifelse(is.na(r$RSSEQ), "", as.character(r$RSSEQ))
  rsstdy_c <- ifelse(is.na(r$RSSTDY), "", as.character(r$RSSTDY))
  paste(obs,
        ifelse(is.na(r$STUDYID), "", r$STUDYID),
        ifelse(is.na(r$DOMAIN), "", r$DOMAIN),
        ifelse(is.na(r$USUBJID), "", r$USUBJID),
        rsseq_c,
        ifelse(is.na(r$RSTESTCD), "", r$RSTESTCD),
        ifelse(is.na(r$RSTEST), "", r$RSTEST),
        ifelse(is.na(r$RSCAT), "", r$RSCAT),
        ifelse(is.na(r$RSORRES), "", r$RSORRES),
        ifelse(is.na(r$RSSTRESC), "", r$RSSTRESC),
        ifelse(is.na(r$RSEVAL), "", r$RSEVAL),
        ifelse(is.na(r$RSDTC), "", r$RSDTC),
        rsstdy_c,
        ifelse(is.na(r$VISIT), "", r$VISIT),
        sep = " | ")
}
lines <- c("PROC PRINT DATA=SDTM_RS",
           paste(hdr, collapse = " | "),
           paste(rep("-", 120), collapse = ""))
for (i in seq_len(nrow(print_df))) {
  lines <- c(lines, fmt_row(i, print_df[i, , drop = FALSE]))
}
lines <- c(lines,
           paste0("N = ", nrow(print_df)),
           paste0("Subjects = ", length(unique(print_df$USUBJID))))
writeLines(lines, txt_path)
writeLines(lines, lst_path)
write.csv(sdtm_rs, csv_path, row.names = FALSE, na = "")

cat("Done. Rows:", nrow(sdtm_rs), "\n")

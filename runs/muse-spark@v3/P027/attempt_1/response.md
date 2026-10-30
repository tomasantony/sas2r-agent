```r
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

# ---- helpers ----
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

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
  # SAS CATX("-", "ONCO-2026", missing) -> "ONCO-2026"
  # if original was NA, SAS would give "ONCO-2026"; emulate: keep "ONCO-2026" for NA?
  # Keep NA to avoid false matches; but emulate SAS: uncomment next line if needed
  # res[is.na(res)] <- "ONCO-2026"
  res
}

# strict date11 (DD-MON-YYYY) -> YYYY-MM-DD, ONE format only
parse_date11_to_iso <- function(x) {
  x_chr <- trimws(as.character(x))
  res <- rep(NA_character_, length(x_chr))
  miss <- is.na(x_chr) | x_chr == "" | x_chr == "."
  idx <- which(!miss)
  if (length(idx) == 0) return(res)
  vals <- x_chr[idx]
  # normalize month case to Title case for %b : 01-JAN-2020 -> 01-Jan-2020
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

# generic single-format-per-column parser to ISO (for DM/RFSTDTC derivation only)
parse_col_to_iso <- function(vec) {
  if (inherits(vec, "Date")) return(format(vec, "%Y-%m-%d"))
  if (inherits(vec, "POSIXt")) return(format(as.Date(vec), "%Y-%m-%d"))
  if (is.numeric(vec) && length(vec) > 0) {
    # Excel serial dates (1899-12-30 origin) if plausible
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
  # decide ONE format for whole column
  has_alpha <- any(grepl("[A-Za-z]", nona))
  has_slash <- any(grepl("/", nona, fixed = TRUE))
  has_isodash <- any(grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}", nona))
  fmt <- NULL
  if (has_alpha) {
    fmt <- "%d-%b-%Y"
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
    d <- suppressWarnings(as.Date(norm, format = fmt))
    res <- format(d, "%Y-%m-%d")
    res[is.na(d)] <- NA_character_
    return(res)
  } else if (has_slash) {
    # GUESSINGROWS logic: first field >12 => DMY else MDY
    first_fields <- suppressWarnings(as.integer(sub("/.*$", "", nona)))
    first_fields <- first_fields[!is.na(first_fields)]
    is_dmy <- length(first_fields) > 0 && any(first_fields > 12, na.rm = TRUE)
    if (is_dmy) fmt <- "%d/%m/%Y" else fmt <- "%m/%d/%Y"
    # handle 2-digit years? assume 4-digit; try both Y and y with same order
    d <- suppressWarnings(as.Date(x_chr, format = fmt))
    # fallback for 2-digit year within same order
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
    # try ISO then yyyymmdd numeric
    d <- suppressWarnings(as.Date(substr(x_chr, 1, 10), format = "%Y-%m-%d"))
    res <- format(d, "%Y-%m-%d")
    res[is.na(d)] <- NA_character_
    return(res)
  }
}

extract_usubjid_date <- function(df, subj_cands, date_cands_list) {
  subj_col <- get_col(df, subj_cands)
  if (is.null(subj_col)) {
    # try USUBJID variants
    subj_col <- get_col(df, c("USUBJID", "SUBJID", "SUBJ", "PTNO", "PATIENT", "PATNO", "ID"))
  }
  if (is.null(subj_col)) return(NULL)
  # USUBJID handling
  if (tolower(subj_col) == "usubjid") {
    usub <- trimws(as.character(df[[subj_col]]))
    usub[usub == ""] <- NA_character_
    # if values lack prefix, add it
    need_prefix <- !is.na(usub) & !grepl("^ONCO-2026-", usub)
    usub[need_prefix] <- paste0("ONCO-2026-", usub[need_prefix])
  } else {
    usub <- make_usubjid(df[[subj_col]])
  }
  # find first available date column in priority order
  dcol <- NULL
  for (cands in date_cands_list) {
    dcol <- get_col(df, cands)
    if (!is.null(dcol)) break
  }
  if (is.null(dcol)) {
    # fallback: any Date-class column or date-like character column
    for (nm in names(df)) {
      colv <- df[[nm]]
      if (inherits(colv, "Date") || inherits(colv, "POSIXt")) { dcol <- nm; break }
    }
    if (is.null(dcol)) {
      for (nm in names(df)) {
        colv <- trimws(as.character(df[[nm]]))
        colv <- colv[!is.na(colv) & colv != ""]
        if (length(colv) == 0) next
        like <- mean(grepl("^[0-9]{1,4}[-/][0-9A-Za-z]{1,3}[-/][0-9]{2,4}", colv), na.rm = TRUE)
        if (!is.na(like) && like > 0.5) { dcol <- nm; break }
      }
    }
  }
  if (is.null(dcol)) {
    return(data.frame(USUBJID = usub, DT = NA_character_, stringsAsFactors = FALSE))
  }
  iso <- parse_col_to_iso(df[[dcol]])
  data.frame(USUBJID = usub, DT = iso, stringsAsFactors = FALSE)
}

cat("Deriving DM reference (RFSTDTC)...\n")
subj_cands <- c("SUBJID", "SUBJ", "USUBJID", "PTNO", "PATIENT")
dm_dt1 <- extract_usubjid_date(edc_dm_raw, subj_cands,
          list(c("RFSTDTC", "RFSTDT", "RFSTD"), c("RANDDTC", "RANDDT", "RAND_DT", "RANDOMDT"), c("EXSTDTC", "EXSTDT")))
ex_dt  <- extract_usubjid_date(edc_ex_raw, subj_cands,
          list(c("EXSTDTC", "EXSTDT", "EX_START", "EXDATE", "EXDT", "STDTC", "STDT"), c("RFSTDTC", "RFSTDT")))
rand_dt <- extract_usubjid_date(ivrs_raw, subj_cands,
          list(c("RANDDTC", "RANDDT", "RAND_DT", "RANDOMDT", "RANDD"), c("RFSTDTC", "RFSTDT")))

# earliest per USUBJID within each source
min_per <- function(d) {
  if (is.null(d)) return(NULL)
  d <- d[!is.na(d$USUBJID) & d$USUBJID != "", , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  d$DT[d$DT == ""] <- NA_character_
  agg <- aggregate(DT ~ USUBJID, data = d,
                   FUN = function(v) { v <- v[!is.na(v)]; if (length(v) == 0) NA_character_ else min(v, na.rm = TRUE) },
                   na.action = na.pass)
  agg
}
dm_min <- min_per(dm_dt1)
ex_min <- min_per(ex_dt)
rand_min <- min_per(rand_dt)

# union of all subjects
all_usub <- unique(c(
  if (!is.null(dm_min)) dm_min$USUBJID,
  if (!is.null(ex_min)) ex_min$USUBJID,
  if (!is.null(rand_min)) rand_min$USUBJID,
  make_usubjid(edc_rs_raw[[get_col(edc_rs_raw, c("SUBJID", "SUBJ", "USUBJID"))]])
))
all_usub <- all_usub[!is.na(all_usub)]
dm_for_rs <- data.frame(USUBJID = sort(unique(all_usub)), stringsAsFactors = FALSE)
if (!is.null(dm_min)) {
  names(dm_min)[names(dm_min) == "DT"] <- "DT_DM"
  dm_for_rs <- merge(dm_for_rs, dm_min, by = "USUBJID", all.x = TRUE, sort = FALSE)
} else dm_for_rs$DT_DM <- NA_character_
if (!is.null(ex_min)) {
  names(ex_min)[names(ex_min) == "DT"] <- "DT_EX"
  dm_for_rs <- merge(dm_for_rs, ex_min, by = "USUBJID", all.x = TRUE, sort = FALSE)
} else dm_for_rs$DT_EX <- NA_character_
if (!is.null(rand_min)) {
  names(rand_min)[names(rand_min) == "DT"] <- "DT_RAND"
  dm_for_rs <- merge(dm_for_rs, rand_min, by = "USUBJID", all.x = TRUE, sort = FALSE)
} else dm_for_rs$DT_RAND <- NA_character_
# priority: direct RFSTDTC (DT_DM) > EX start > RAND
dm_for_rs$RFSTDTC <- dplyr::coalesce(dm_for_rs$DT_DM, dm_for_rs$DT_EX, dm_for_rs$DT_RAND)
dm_for_rs <- dm_for_rs[, c("USUBJID", "RFSTDTC"), drop = FALSE]
# SAS sort by USUBJID (uppercase before lowercase, missing first)
dm_for_rs <- dm_for_rs[order(dm_for_rs$USUBJID, na.last = FALSE), , drop = FALSE]
rownames(dm_for_rs) <- NULL

# ---- 2. Prepare Raw Data: rs_prep ----
cat("Preparing RS raw...\n")
c_subjid <- get_col(edc_rs_raw, c("SUBJID", "SUBJ", "USUBJID"))
c_testcd <- get_col(edc_rs_raw, c("TESTCD", "TEST", "RSTESTCD"))
c_orres  <- get_col(edc_rs_raw, c("ORRES", "RSORRES", "RESULT", "RESPONSE"))
c_visit  <- get_col(edc_rs_raw, c("VISIT", "VISITNAME", "VISIT_NM"))
c_rsdt   <- get_col(edc_rs_raw, c("RSDT", "RSDTC", "DATE", "RSDATE", "ASSESSDT"))
stopifnot(!is.null(c_subjid), !is.null(c_testcd), !is.null(c_orres), !is.null(c_visit), !is.null(c_rsdt))

rs_prep <- data.frame(
  SUBJID_raw = trimws(as.character(edc_rs_raw[[c_subjid]])),
  TESTCD_raw = trimws(as.character(edc_rs_raw[[c_testcd]])),
  ORRES_raw  = trimws(as.character(edc_rs_raw[[c_orres]])),
  VISIT_raw  = trimws(as.character(edc_rs_raw[[c_visit]])),
  RSDT_raw   = trimws(as.character(edc_rs_raw[[c_rsdt]])),
  stringsAsFactors = FALSE
)
rs_prep$SUBJID_raw[rs_prep$SUBJID_raw == "" | is.na(rs_prep$SUBJID_raw)] <- NA_character_
# USUBJID = catx("-", "ONCO-2026", SUBJID) : SAS CATX ignores missing second arg -> "ONCO-2026"
rs_prep$USUBJID <- ifelse(is.na(rs_prep$SUBJID_raw), "ONCO-2026",
                          paste0("ONCO-2026-", rs_prep$SUBJID_raw))
rs_prep$TESTCD <- rs_prep$TESTCD_raw
rs_prep$ORRES  <- rs_prep$ORRES_raw
rs_prep$VISIT  <- rs_prep$VISIT_raw
rs_prep$RSDT   <- rs_prep$RSDT_raw
rs_prep$RSDT[is.na(rs_prep$RSDT) | rs_prep$RSDT == "" | rs_prep$RSDT == "."] <- NA_character_
# SAS sorts: by USUBJID (missing first)
rs_prep <- rs_prep[order(rs_prep$USUBJID, na.last = FALSE), , drop = FALSE]

# ---- 3. Map RS Domain: sdtm_rs1 (merge rs_prep(in=r) dm_for_rs by USUBJID, if r) ----
cat("Mapping RS domain...\n")
# drop/rename overlapping non-key cols (only USUBJID is key)
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
# VISIT = upcase(tranwrd(VISIT,"_"," "))
vis <- trimws(as.character(sdtm_rs1$VISIT))
vis[is.na(vis)] <- NA_character_
sdtm_rs1$VISIT <- ifelse(is.na(vis), NA_character_, toupper(gsub("_", " ", vis, fixed = TRUE)))
# Date conversion: if RSDT ne "" then RSDTC = put(input(RSDT,date11.),yymmdd10.)
sdtm_rs1$RSDTC <- parse_date11_to_iso(sdtm_rs1$RSDT)
# Study Day
rf_iso <- substr(trimws(as.character(sdtm_rs1$RFSTDTC)), 1, 10)
rf_iso[rf_iso == "" | is.na(sdtm_rs1$RFSTDTC)] <- NA_character_
rs_iso <- sdtm_rs1$RSDTC
stdt <- suppressWarnings(as.Date(rs_iso, format = "%Y-%m-%d"))
rfdt <- suppressWarnings(as.Date(rf_iso, format = "%Y-%m-%d"))
diffd <- as.integer(stdt - rfdt)
sdtm_rs1$RSSTDY <- ifelse(!is.na(stdt) & !is.na(rfdt),
                          ifelse(stdt >= rfdt, diffd + 1L, diffd),
                          NA_integer_)
# keep needed
sdtm_rs1 <- sdtm_rs1[, c("STUDYID", "DOMAIN", "USUBJID", "RSTESTCD", "RSTEST", "RSCAT",
                          "RSORRES", "RSSTRESC", "RSEVAL", "RSDTC", "RSSTDY", "VISIT"), drop = FALSE]

# ---- 4. Derive RSSEQ: sort by USUBJID RSDTC, retain ----
cat("Deriving RSSEQ...\n")
# SAS missing sorts first: use NA first
ord <- order(sdtm_rs1$USUBJID, sdtm_rs1$RSDTC, na.last = FALSE)
sdtm_rs1 <- sdtm_rs1[ord, , drop = FALSE]
# FIRST./LAST. by USUBJID
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
# dataset rs
write.csv(sdtm_rs, file.path(out, "rs.csv"), row.names = FALSE, na = "")

# PROC PRINT listing: text rendering + CSV of table body
print_df <- sdtm_rs
# text rendering mimicking PROC PRINT
txt_path <- file.path(out, "P027.txt")
lst_path <- file.path(out, "P027.lst")
csv_path <- file.path(out, "P027.csv")

# build fixed-width style text
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
```
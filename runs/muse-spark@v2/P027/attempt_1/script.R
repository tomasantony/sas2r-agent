# P027_rs.sas -> R translation
# SDTM RS mapping & derivation

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
})

cat("P027 RS mapping started\n")

root_raw <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
edc_rs_path   <- file.path(root_raw, "edc_rs.xlsx")
edc_dm_path   <- file.path(root_raw, "edc_dm.xlsx")
edc_ex_path   <- file.path(root_raw, "edc_ex.xlsx")
ivrs_rand_path<- file.path(root_raw, "ivrs_rand.xlsx")

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_catx <- function(sep, ...) {
  args <- list(...)
  args <- lapply(args, function(a) {
    if (is.null(a)) return("")
    a <- as.character(a)
    a[is.na(a)] <- ""
    trimws(a)
  })
  # CATX strips and skips empties? Actually CATX strips leading/trailing and removes empty? SAS CATX skips missing? It inserts sep between non-missing.
  # For single value case, just return trimmed.
  # General: paste ignoring "" ?
  # Reproduce: join non-empty with sep per row (vectorised)
  n <- max(sapply(args, length))
  args <- lapply(args, function(a) rep_len(a, n))
  vapply(seq_len(n), function(i) {
    vals <- sapply(args, function(a) a[i])
    vals <- vals[vals != ""]
    paste(vals, collapse = sep)
  }, character(1))
}

# ---- helpers for column lookup (case-insensitive) ----
find_col_exact <- function(nms, candidates) {
  un <- toupper(trimws(nms))
  for (cand in candidates) {
    idx <- which(un == toupper(cand))
    if (length(idx) > 0) return(nms[idx[1]])
  }
  return(NULL)
}
find_col_contains <- function(nms, patterns) {
  un <- toupper(trimws(nms))
  for (pat in patterns) {
    idx <- which(grepl(toupper(pat), un, fixed = TRUE))
    if (length(idx) > 0) return(nms[idx[1]])
  }
  return(NULL)
}
find_subj_col <- function(df) {
  nms <- names(df)
  c1 <- find_col_exact(nms, c("SUBJID","SUBJECT","SUBJ","PATID","PATNO","USUBJID","SUBID"))
  if (!is.null(c1)) return(c1)
  c2 <- find_col_contains(nms, c("SUBJ"))
  if (!is.null(c2)) return(c2)
  return(NULL)
}
find_date_col <- function(df, exclude = NULL) {
  nms <- names(df)
  if (!is.null(exclude)) nms2 <- setdiff(nms, exclude) else nms2 <- nms
  # priority exact names
  pri <- c("RFSTDTC","RFSTDT","EXSTDTC","EXSTDT","EXSTDAT","RANDDTC","RANDDT",
           "RANDATE","EXDATE","EXSTARTDT","STARTDT","STDT","STDTC","ASSESSDT",
           "RSDT","RSDTC","DATE","DTC","DT")
  c1 <- find_col_exact(nms2, pri)
  if (!is.null(c1)) {
    # verify it has some date-like values; if empty try next? just return
    return(c1)
  }
  c2 <- find_col_contains(nms2, c("STDTC","STDT","RAND","RFST","START","ASSESS","DATE","DTC"))
  if (!is.null(c2)) return(c2)
  # fallback: any column with DT in name
  c3 <- find_col_contains(nms2, c("DT"))
  if (!is.null(c3)) return(c3)
  return(NULL)
}

normalize_date11_str <- function(s) {
  # "01-JAN-2026" -> "01-Jan-2026" for R %b
  s <- trimws(as.character(s))
  # split by -
  parts <- strsplit(s, "-", fixed = TRUE)
  vapply(parts, function(p) {
    if (length(p) == 3) {
      mon <- p[2]
      mon <- paste0(toupper(substr(mon,1,1)), tolower(substr(mon,2,nchar(mon))))
      paste(p[1], mon, p[3], sep = "-")
    } else s
  }, character(1))
}

iso_from_any_vector <- function(v) {
  # returns character YYYY-MM-DD or NA
  if (inherits(v, "Date")) {
    return(format(v, "%Y-%m-%d"))
  }
  if (inherits(v, "POSIXct") || inherits(v, "POSIXt")) {
    return(format(as.Date(v), "%Y-%m-%d"))
  }
  if (is.numeric(v)) {
    # Excel numeric date
    d <- suppressWarnings(as.Date(v, origin = "1899-12-30"))
    return(format(d, "%Y-%m-%d"))
  }
  # character / factor
  v <- as.character(v)
  res <- rep(NA_character_, length(v))
  for (i in seq_along(v)) {
    s <- ifelse(is.na(v[i]), "", trimws(v[i]))
    if (s == "" || toupper(s) %in% c("NA",".","NULL","ND","MISSING")) {
      res[i] <- NA_character_
    } else if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", s)) {
      # already ISO (maybe with time)
      iso <- substr(s, 1, 10)
      dd <- suppressWarnings(as.Date(iso, format = "%Y-%m-%d"))
      if (!is.na(dd)) res[i] <- format(dd, "%Y-%m-%d") else res[i] <- NA_character_
    } else if (grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4}$", s)) {
      sn <- normalize_date11_str(s)
      dd <- suppressWarnings(as.Date(sn, format = "%d-%b-%Y"))
      if (is.na(dd)) {
        # try 2-digit year
        dd <- suppressWarnings(as.Date(sn, format = "%d-%b-%y"))
      }
      if (!is.na(dd)) res[i] <- format(dd, "%Y-%m-%d") else res[i] <- NA_character_
    } else if (grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$", s)) {
      # should not occur for RSDT (DATE11), but handle defensively as DMY? not used
      res[i] <- NA_character_
    } else {
      # try ISO directly
      dd <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
      if (!is.na(dd)) res[i] <- format(dd, "%Y-%m-%d") else res[i] <- NA_character_
    }
  }
  res[res == "NA"] <- NA_character_
  res
}

# ---- 1. Read raw RS ----
stopifnot(file.exists(edc_rs_path))
edc_rs <- readxl::read_excel(edc_rs_path)
names(edc_rs) <- trimws(names(edc_rs))
cat("edc_rs cols:", paste(names(edc_rs), collapse=","), "\n")
cat("edc_rs n:", nrow(edc_rs), "\n")

col_SUBJID <- find_col_exact(names(edc_rs), c("SUBJID"))
if (is.null(col_SUBJID)) col_SUBJID <- find_subj_col(edc_rs)
col_TESTCD <- find_col_exact(names(edc_rs), c("TESTCD","RSTESTCD"))
if (is.null(col_TESTCD)) col_TESTCD <- find_col_contains(names(edc_rs), c("TESTCD","TEST"))
col_ORRES  <- find_col_exact(names(edc_rs), c("ORRES","RSORRES"))
if (is.null(col_ORRES)) col_ORRES <- find_col_contains(names(edc_rs), c("ORRES","RES"))
col_VISIT  <- find_col_exact(names(edc_rs), c("VISIT"))
if (is.null(col_VISIT)) col_VISIT <- find_col_contains(names(edc_rs), c("VISIT"))
col_RSDT   <- find_col_exact(names(edc_rs), c("RSDT","RSDTC","RSDATE"))
if (is.null(col_RSDT)) col_RSDT <- find_date_col(edc_rs, exclude=c(col_SUBJID, col_TESTCD, col_ORRES, col_VISIT))
stopifnot(!is.null(col_SUBJID), !is.null(col_TESTCD), !is.null(col_ORRES), !is.null(col_VISIT), !is.null(col_RSDT))
cat("Using cols:", col_SUBJID, col_TESTCD, col_ORRES, col_VISIT, col_RSDT, "\n")

# ---- 2. rs_prep ----
rs_prep <- edc_rs
rs_prep$SUBJID_c <- trimws(as.character(rs_prep[[col_SUBJID]]))
rs_prep$TESTCD_c <- as.character(rs_prep[[col_TESTCD]])
rs_prep$ORRES_c  <- as.character(rs_prep[[col_VISIT]]) # placeholder, corrected below
rs_prep$ORRES_c  <- as.character(rs_prep[[col_ORRES]])
rs_prep$VISIT_c  <- as.character(rs_prep[[col_VISIT]])
raw_rsdt_vec <- rs_prep[[col_RSDT]]
# preserve raw RSDT as character for conversion; if Date/numeric handle via iso helper later
if (inherits(raw_rsdt_vec, "Date") || inherits(raw_rsdt_vec, "POSIXct") || is.numeric(raw_rsdt_vec)) {
  rs_prep$RSDT_c <- iso_from_any_vector(raw_rsdt_vec)
  # mark as already ISO to avoid re-parse? keep flag
  rs_prep$RSDT_raw_is_iso <- TRUE
  # also keep original string for RSDTC derivation: if already ISO, RSDTC = that ISO
} else {
  rs_prep$RSDT_c <- trimws(as.character(raw_rsdt_vec))
  rs_prep$RSDT_c[rs_prep$RSDT_c == "" | is.na(rs_prep$RSDT_c)] <- NA_character_
  rs_prep$RSDT_raw_is_iso <- FALSE
}
rs_prep$USUBJID <- sas_catx("-", "ONCO-2026", rs_prep$SUBJID_c)
# SAS sort by USUBJID
ord <- order(rs_prep$USUBJID, method = "radix", na.last = FALSE)
rs_prep <- rs_prep[ord, , drop = FALSE]

# ---- 3. dm_for_rs : derive RFSTDTC (CDISC: reference start = first exposure / randomisation) ----
# Read DM / EX / RAND
edc_dm <- readxl::read_excel(edc_dm_path)
names(edc_dm) <- trimws(names(edc_dm))
edc_ex <- readxl::read_excel(edc_ex_path)
names(edc_ex) <- trimws(names(edc_ex))
ivrs_rand <- readxl::read_excel(ivrs_rand_path)
names(ivrs_rand) <- trimws(names(ivrs_rand))
cat("edc_dm cols:", paste(names(edc_dm), collapse=","), "\n")
cat("edc_ex cols:", paste(names(edc_ex), collapse=","), "\n")
cat("ivrs_rand cols:", paste(names(ivrs_rand), collapse=","), "\n")

# helper to build USUBJID + RFSTDTC piece from a dataframe
build_rf_piece <- function(df, label) {
  subj_col <- find_subj_col(df)
  # if USUBJID exists use it directly
  usub_col <- find_col_exact(names(df), c("USUBJID"))
  if (!is.null(usub_col)) {
    usub <- trimws(as.character(df[[usub_col]]))
    # ensure prefix? keep as is; if missing prefix add?
    # if usub doesn't start with ONCO, construct from subj?
    need_fix <- is.na(usub) | usub == "" | !grepl("^ONCO-2026-", usub)
    if (any(need_fix) && !is.null(subj_col)) {
      fix_ids <- sas_catx("-", "ONCO-2026", trimws(as.character(df[[subj_col]][need_fix])))
      usub[need_fix] <- fix_ids
    }
  } else if (!is.null(subj_col)) {
    usub <- sas_catx("-", "ONCO-2026", trimws(as.character(df[[subj_col]])))
  } else {
    return(NULL)
  }
  # find RFSTDTC if present
  rf_col <- find_col_exact(names(df), c("RFSTDTC","RFSTDT","RFSTDAT"))
  if (!is.null(rf_col)) {
    rf <- iso_from_any_vector(df[[rf_col]])
  } else {
    # find best date col
    dcol <- find_date_col(df, exclude = c(subj_col, usub_col))
    if (is.null(dcol)) return(data.frame(USUBJID = usub, RFSTDTC_tmp = NA_character_, stringsAsFactors = FALSE))
    rf_raw <- df[[dcol]]
    rf <- iso_from_any_vector(rf_raw)
    cat(label, "using date col:", dcol, "\n")
  }
  data.frame(USUBJID = usub, RFSTDTC_tmp = rf, stringsAsFactors = FALSE)
}

dm_piece <- build_rf_piece(edc_dm, "DM")
ex_piece <- build_rf_piece(edc_ex, "EX")
rand_piece <- build_rf_piece(ivrs_rand, "RAND")

# Aggregate each piece to one row per USUBJID (min date)
agg_min <- function(piece) {
  if (is.null(piece)) return(NULL)
  piece <- piece[!is.na(piece$USUBJID) & piece$USUBJID != "", , drop = FALSE]
  if (nrow(piece) == 0) return(NULL)
  piece %>%
    group_by(USUBJID) %>%
    summarise(RF = {
      v <- RFSTDTC_tmp[!is.na(RFSTDTC_tmp) & RFSTDTC_tmp != ""]
      if (length(v) == 0) NA_character_ else min(v)
    }, .groups = "drop") %>%
    as.data.frame()
}
dm_agg <- agg_min(dm_piece)
ex_agg <- agg_min(ex_piece)
rand_agg <- agg_min(rand_piece)

# Union of subjects: from DM + EX + RAND + rs_prep (to ensure all RS subjects covered)
all_subj <- unique(c(
  if (!is.null(dm_agg)) dm_agg$USUBJID else NULL,
  if (!is.null(ex_agg)) ex_agg$USUBJID else NULL,
  if (!is.null(rand_agg)) rand_agg$USUBJID else NULL,
  rs_prep$USUBJID
))
dm_for_rs <- data.frame(USUBJID = all_subj, stringsAsFactors = FALSE)
# coalesce priority: DM RF (if from RFSTDTC) else EX else RAND else DM date?
# Actually dm_agg may already be EX-like if DM had no RFSTDTC; priority DM > EX > RAND
dm_for_rs <- dm_for_rs %>%
  left_join(if (!is.null(dm_agg)) dm_agg %>% rename(RF_DM = RF) else data.frame(USUBJID=character(), RF_DM=character()), by = "USUBJID") %>%
  left_join(if (!is.null(ex_agg)) ex_agg %>% rename(RF_EX = RF) else data.frame(USUBJID=character(), RF_EX=character()), by = "USUBJID") %>%
  left_join(if (!is.null(rand_agg)) rand_agg %>% rename(RF_RAND = RF) else data.frame(USUBJID=character(), RF_RAND=character()), by = "USUBJID")

# coalesce
dm_for_rs$RFSTDTC <- coalesce(dm_for_rs$RF_DM, dm_for_rs$RF_EX, dm_for_rs$RF_RAND)
# If DM piece was from RFSTDTC column, it is already priority; otherwise same logic holds
dm_for_rs <- dm_for_rs[, c("USUBJID","RFSTDTC"), drop = FALSE]
# SAS sort by USUBJID
dm_for_rs <- dm_for_rs[order(dm_for_rs$USUBJID, method="radix", na.last=FALSE), , drop=FALSE]
cat("dm_for_rs n:", nrow(dm_for_rs), " non-missing RFSTDTC:", sum(!is.na(dm_for_rs$RFSTDTC)), "\n")

# ---- 4. Map RS domain (merge rs_prep + dm_for_rs, keep if r) ----
# drop/rename overlapping except keys to avoid .x/.y
# dm_for_rs has USUBJID, RFSTDTC ; rs_prep has USUBJID but no RFSTDTC column named RFSTDTC (check)
if ("RFSTDTC" %in% names(rs_prep)) rs_prep$RFSTDTC <- NULL
# left join
sdtm_rs1 <- merge(rs_prep, dm_for_rs, by = "USUBJID", all.x = TRUE, sort = FALSE)
stopifnot("RFSTDTC" %in% names(sdtm_rs1))
stopifnot("USUBJID" %in% names(sdtm_rs1))

n1 <- nrow(sdtm_rs1)
sdtm_rs1$STUDYID <- "ONCO-2026"
sdtm_rs1$DOMAIN <- "RS"
sdtm_rs1$RSTESTCD <- as.character(sdtm_rs1$TESTCD_c)
sdtm_rs1$RSTEST <- ifelse(!is.na(sdtm_rs1$RSTESTCD) & sdtm_rs1$RSTESTCD == "OVRLRESP", "Overall Response", NA_character_)
sdtm_rs1$RSCAT <- "RECIST 1.1"
sdtm_rs1$RSORRES <- toupper(as.character(sdtm_rs1$ORRES_c))
sdtm_rs1$RSORRES[is.na(sdtm_rs1$ORRES_c)] <- NA_character_
sdtm_rs1$RSSTRESC <- sdtm_rs1$RSORRES
sdtm_rs1$RSEVAL <- "INVESTIGATOR"
# VISIT = upcase(tranwrd(VISIT,"_"," "))
vis_raw <- as.character(sdtm_rs1$VISIT_c)
sdtm_rs1$VISIT <- ifelse(is.na(vis_raw), NA_character_, toupper(gsub("_", " ", vis_raw, fixed = TRUE)))
# Date conversion: if RSDT ne "" then RSDTC = put(input(RSDT,date11.),yymmdd10.)
sdtm_rs1$RSDTC <- NA_character_
for (i in seq_len(n1)) {
  if (isTRUE(sdtm_rs1$RSDT_raw_is_iso[i])) {
    v <- sdtm_rs1$RSDT_c[i]
    if (!is.na(v) && v != "") sdtm_rs1$RSDTC[i] <- v
  } else {
    v <- sdtm_rs1$RSDT_c[i]
    if (!is.na(v) && trimws(v) != "") {
      sn <- normalize_date11_str(v)
      dd <- suppressWarnings(as.Date(sn, format = "%d-%b-%Y"))
      if (is.na(dd)) dd <- suppressWarnings(as.Date(sn, format = "%d-%b-%y"))
      if (!is.na(dd)) sdtm_rs1$RSDTC[i] <- format(dd, "%Y-%m-%d")
    }
  }
}
# Study day
sdtm_rs1$RSSTDY <- NA_real_
for (i in seq_len(n1)) {
  a <- sdtm_rs1$RSDTC[i]; b <- sdtm_rs1$RFSTDTC[i]
  if (!is.na(a) && a != "" && !is.na(b) && b != "") {
    stdt <- suppressWarnings(as.Date(a, format = "%Y-%m-%d"))
    rfdt <- suppressWarnings(as.Date(b, format = "%Y-%m-%d"))
    if (!is.na(stdt) && !is.na(rfdt)) {
      diff <- as.integer(stdt - rfdt)
      if (stdt >= rfdt) sdtm_rs1$RSSTDY[i] <- diff + 1 else sdtm_rs1$RSSTDY[i] <- diff
    }
  }
}

# ---- sort by USUBJID RSDTC (missing first) ----
o <- order(sdtm_rs1$USUBJID, sdtm_rs1$RSDTC, method = "radix", na.last = FALSE)
sdtm_rs1 <- sdtm_rs1[o, , drop = FALSE]

# ---- Derive RSSEQ ----
sdtm_rs1$RSSEQ <- NA_real_
# FIRST./LAST. processing by USUBJID
cur <- NA_character_
seqn <- 0L
for (i in seq_len(nrow(sdtm_rs1))) {
  u <- sdtm_rs1$USUBJID[i]
  if (is.na(u) != is.na(cur) || (!is.na(u) && !is.na(cur) && u != cur) || (is.na(cur) && !is.na(u)) || (is.null(cur))) {
    # first of group (handle NA group too)
    if (i == 1 || is.na(u) != is.na(sdtm_rs1$USUBJID[i-1]) || (!is.na(u) && !is.na(sdtm_rs1$USUBJID[i-1]) && u != sdtm_rs1$USUBJID[i-1])) {
      seqn <- 1L
    } else {
      seqn <- seqn + 1L
    }
  } else {
    # same as previous? actually need proper first. detection
    seqn <- seqn + 1L
  }
  # Simpler robust: detect first via previous row
  if (i == 1) seqn <- 1L else {
    prev <- sdtm_rs1$USUBJID[i-1]
    is_first <- (is.na(u) && is.na(prev)) ? FALSE : (is.na(u) != is.na(prev)) || (!is.na(u) && !is.na(prev) && u != prev)
    # R ternary workaround
    if (is.na(u) && is.na(prev)) is_first <- FALSE
    else if (is.na(u) != is.na(prev)) is_first <- TRUE
    else if (u != prev) is_first <- TRUE else is_first <- FALSE
    if (is_first) seqn <- 1L else seqn <- seqn + 1L
  }
  sdtm_rs1$RSSEQ[i] <- seqn
  cur <- u
}

sdtm_rs <- sdtm_rs1[, c("STUDYID","DOMAIN","USUBJID","RSSEQ","RSTESTCD","RSTEST","RSCAT","RSORRES","RSSTRESC","RSEVAL","RSDTC","RSSTDY","VISIT"), drop = FALSE]
# ensure types: RSSEQ numeric, RSSTDY numeric
sdtm_rs$RSSEQ <- as.numeric(sdtm_rs$RSSEQ)
sdtm_rs$RSSTDY <- as.numeric(sdtm_rs$RSSTDY)

# SAS LENGTHs truncation (STUDYID 10, DOMAIN 2, USUBJID 25, RSTESTCD 8, RSTEST 40, RSCAT 15, RSORRES 10, RSSTRESC 10, RSEVAL 15, VISIT 20, RSDTC 19)
trunc_str <- function(x, n) ifelse(is.na(x), NA_character_, substr(x, 1, n))
sdtm_rs$STUDYID <- trunc_str(sdtm_rs$STUDYID, 10)
sdtm_rs$DOMAIN <- trunc_str(sdtm_rs$DOMAIN, 2)
sdtm_rs$USUBJID <- trunc_str(sdtm_rs$USUBJID, 25)
sdtm_rs$RSTESTCD <- trunc_str(sdtm_rs$RSTESTCD, 8)
sdtm_rs$RSTEST <- trunc_str(sdtm_rs$RSTEST, 40)
sdtm_rs$RSCAT <- trunc_str(sdtm_rs$RSCAT, 15)
sdtm_rs$RSORRES <- trunc_str(sdtm_rs$RSORRES, 10)
sdtm_rs$RSSTRESC <- trunc_str(sdtm_rs$RSSTRESC, 10)
sdtm_rs$RSEVAL <- trunc_str(sdtm_rs$RSEVAL, 15)
sdtm_rs$VISIT <- trunc_str(sdtm_rs$VISIT, 20)
sdtm_rs$RSDTC <- trunc_str(sdtm_rs$RSDTC, 19)

cat("sdtm_rs n:", nrow(sdtm_rs), "\n")

# ---- 5. PROC PRINT + 6. Export ----
write.csv(sdtm_rs, file.path(out, "rs.csv"), row.names = FALSE, na = "")

print_df <- cbind(Obs = seq_len(nrow(sdtm_rs)), as.data.frame(sdtm_rs, stringsAsFactors = FALSE))

# CSV of table body
write.csv(print_df, file.path(out, "P027.csv"), row.names = FALSE, na = "")

# Text rendering (PROC PRINT style)
txt_path <- file.path(out, "P027.txt")
lst_path <- file.path(out, "P027.lst")
con <- file(txt_path, open = "wt")
writeLines("PROC PRINT DATA=SDTM_RS;", con)
writeLines(paste0("Study: ONCO-2026  Domain: RS (RECIST 1.1)  N=", nrow(sdtm_rs)), con)
writeLines("", con)
# header
hdr <- sprintf("%4s %-10s %-2s %-25s %5s %-8s %-16s %-15s %-10s %-10s %-15s %-10s %6s %-20s",
  "Obs","STUDYID","DO","USUBJID","RSSEQ","RSTESTCD","RSTEST","RSCAT","RSORRES","RSSTRESC","RSEVAL","RSDTC","RSSTDY","VISIT")
writeLines(hdr, con)
writeLines(paste(rep("-", nchar(hdr)), collapse=""), con)
for (i in seq_len(nrow(sdtm_rs))) {
  r <- sdtm_rs[i, ]
  line <- sprintf("%4d %-10s %-2s %-25s %5.0f %-8s %-16s %-15s %-10s %-10s %-15s %-10s %6s %-20s",
    i,
    ifelse(is.na(r$STUDYID),"",r$STUDYID),
    ifelse(is.na(r$DOMAIN),"",r$DOMAIN),
    ifelse(is.na(r$USUBJID),"",r$USUBJID),
    r$RSSEQ,
    ifelse(is.na(r$RSTESTCD),"",r$RSTESTCD),
    ifelse(is.na(r$RSTEST),"",r$RSTEST),
    ifelse(is.na(r$RSCAT),"",r$RSCAT),
    ifelse(is.na(r$RSORRES),"",r$RSORRES),
    ifelse(is.na(r$RSSTRESC),"",r$RSSTRESC),
    ifelse(is.na(r$RSEVAL),"",r$RSEVAL),
    ifelse(is.na(r$RSDTC),"",r$RSDTC),
    ifelse(is.na(r$RSSTDY),"",format(r$RSSTDY, nsmall=0)),
    ifelse(is.na(r$VISIT),"",r$VISIT))
  writeLines(line, con)
}
writeLines("", con)
writeLines(paste0("Total observations: ", nrow(sdtm_rs)), con)
close(con)
# copy to .lst as well
invisible(file.copy(txt_path, lst_path, overwrite = TRUE))

cat("Done. Wrote rs.csv, P027.csv, P027.txt, P027.lst to", out, "\n")

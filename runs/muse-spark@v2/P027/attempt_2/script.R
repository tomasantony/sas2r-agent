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

sas_catx <- function(sep, ...) {
  args <- list(...)
  args <- lapply(args, function(a) {
    if (is.null(a)) return("")
    a <- as.character(a)
    a[is.na(a)] <- ""
    trimws(a)
  })
  n <- max(sapply(args, length))
  args <- lapply(args, function(a) rep_len(a, n))
  vapply(seq_len(n), function(i) {
    vals <- sapply(args, function(a) a[i])
    vals <- vals[vals != ""]
    paste(vals, collapse = sep)
  }, character(1))
}

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

normalize_date11_str <- function(s) {
  s <- trimws(as.character(s))
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
  if (inherits(v, "Date")) {
    return(format(v, "%Y-%m-%d"))
  }
  if (inherits(v, "POSIXct") || inherits(v, "POSIXt")) {
    return(format(as.Date(v), "%Y-%m-%d"))
  }
  if (is.numeric(v)) {
    d <- suppressWarnings(as.Date(v, origin = "1899-12-30"))
    return(format(d, "%Y-%m-%d"))
  }
  v <- as.character(v)
  res <- rep(NA_character_, length(v))
  for (i in seq_along(v)) {
    s <- ifelse(is.na(v[i]), "", trimws(v[i]))
    if (s == "" || toupper(s) %in% c("NA",".","NULL","ND","MISSING")) {
      res[i] <- NA_character_
    } else if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", s)) {
      iso <- substr(s, 1, 10)
      dd <- suppressWarnings(as.Date(iso, format = "%Y-%m-%d"))
      if (!is.na(dd)) res[i] <- format(dd, "%Y-%m-%d") else res[i] <- NA_character_
    } else if (grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4}$", s)) {
      sn <- normalize_date11_str(s)
      dd <- suppressWarnings(as.Date(sn, format = "%d-%b-%Y"))
      if (is.na(dd)) {
        dd <- suppressWarnings(as.Date(sn, format = "%d-%b-%y"))
      }
      if (!is.na(dd)) res[i] <- format(dd, "%Y-%m-%d") else res[i] <- NA_character_
    } else {
      dd <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
      if (!is.na(dd)) res[i] <- format(dd, "%Y-%m-%d") else res[i] <- NA_character_
    }
  }
  res[res == "NA"] <- NA_character_
  res
}

# ---- Read raw RS ----
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
if (is.null(col_RSDT)) col_RSDT <- find_col_exact(names(edc_rs), c("DATE","DTC","DT"))
stopifnot(!is.null(col_SUBJID), !is.null(col_TESTCD), !is.null(col_ORRES), !is.null(col_VISIT), !is.null(col_RSDT))
cat("Using cols:", col_SUBJID, col_TESTCD, col_ORRES, col_VISIT, col_RSDT, "\n")

# ---- rs_prep ----
rs_prep <- edc_rs
rs_prep$SUBJID_c <- trimws(as.character(rs_prep[[col_SUBJID]]))
rs_prep$TESTCD_c <- as.character(rs_prep[[col_TESTCD]])
rs_prep$ORRES_c  <- as.character(rs_prep[[col_ORRES]])
rs_prep$VISIT_c  <- as.character(rs_prep[[col_VISIT]])
raw_rsdt_vec <- rs_prep[[col_RSDT]]
if (inherits(raw_rsdt_vec, "Date") || inherits(raw_rsdt_vec, "POSIXct") || is.numeric(raw_rsdt_vec)) {
  rs_prep$RSDT_c <- iso_from_any_vector(raw_rsdt_vec)
  rs_prep$RSDT_raw_is_iso <- TRUE
} else {
  rs_prep$RSDT_c <- trimws(as.character(raw_rsdt_vec))
  rs_prep$RSDT_c[rs_prep$RSDT_c == "" | is.na(rs_prep$RSDT_c)] <- NA_character_
  rs_prep$RSDT_raw_is_iso <- FALSE
}
rs_prep$USUBJID <- sas_catx("-", "ONCO-2026", rs_prep$SUBJID_c)
ord <- order(rs_prep$USUBJID, method = "radix", na.last = FALSE)
rs_prep <- rs_prep[ord, , drop = FALSE]

# ---- dm_for_rs : derive RFSTDTC per CDISC (first exposure, fallback randomisation) ----
edc_dm <- readxl::read_excel(edc_dm_path)
names(edc_dm) <- trimws(names(edc_dm))
edc_ex <- readxl::read_excel(edc_ex_path)
names(edc_ex) <- trimws(names(edc_ex))
ivrs_rand <- readxl::read_excel(ivrs_rand_path)
names(ivrs_rand) <- trimws(names(ivrs_rand))
cat("edc_dm cols:", paste(names(edc_dm), collapse=","), "\n")
cat("edc_ex cols:", paste(names(edc_ex), collapse=","), "\n")
cat("ivrs_rand cols:", paste(names(ivrs_rand), collapse=","), "\n")

dm_subj_col <- find_subj_col(edc_dm)
ex_subj_col <- find_subj_col(edc_ex)
rand_subj_col <- find_subj_col(ivrs_rand)

ex_date_col <- find_col_exact(names(edc_ex), c("STDT","EXSTDT","EXSTDTC","EXSTDAT","STARTDT","STDATE","EXDATE","DATE"))
if (is.null(ex_date_col)) ex_date_col <- find_col_contains(names(edc_ex), c("STDT","START","DATE"))
rand_date_col <- find_col_exact(names(ivrs_rand), c("RAND_DT","RANDDT","RANDDTC","RANDATE","RAND_DATE","DATE"))
if (is.null(rand_date_col)) rand_date_col <- find_col_contains(names(ivrs_rand), c("RAND","DATE"))
cat("EX date col:", ex_date_col, " RAND date col:", rand_date_col, "\n")
stopifnot(!is.null(dm_subj_col))
stopifnot(!is.null(ex_date_col))
stopifnot(!is.null(rand_date_col))

# USUBJID lists
dm_usub <- sas_catx("-", "ONCO-2026", trimws(as.character(edc_dm[[dm_subj_col]])))
ex_usub <- sas_catx("-", "ONCO-2026", trimws(as.character(edc_ex[[ex_subj_col]])))
rand_usub <- sas_catx("-", "ONCO-2026", trimws(as.character(ivrs_rand[[rand_subj_col]])))

ex_iso <- iso_from_any_vector(edc_ex[[ex_date_col]])
rand_iso <- iso_from_any_vector(ivrs_rand[[rand_date_col]])

ex_df <- data.frame(USUBJID = ex_usub, EXDT = ex_iso, stringsAsFactors = FALSE)
ex_df <- ex_df[!is.na(ex_df$USUBJID) & ex_df$USUBJID != "", , drop = FALSE]
ex_agg <- ex_df %>%
  filter(!is.na(EXDT) & EXDT != "") %>%
  group_by(USUBJID) %>%
  summarise(RF_EX = min(EXDT), .groups = "drop") %>%
  as.data.frame()

rand_df <- data.frame(USUBJID = rand_usub, RDT = rand_iso, stringsAsFactors = FALSE)
rand_df <- rand_df[!is.na(rand_df$USUBJID) & rand_df$USUBJID != "", , drop = FALSE]
rand_agg <- rand_df %>%
  filter(!is.na(RDT) & RDT != "") %>%
  group_by(USUBJID) %>%
  summarise(RF_RAND = min(RDT), .groups = "drop") %>%
  as.data.frame()

all_subj <- unique(c(dm_usub, ex_usub, rand_usub, rs_prep$USUBJID))
all_subj <- all_subj[!is.na(all_subj) & all_subj != ""]
dm_for_rs <- data.frame(USUBJID = all_subj, stringsAsFactors = FALSE)
dm_for_rs <- dm_for_rs %>%
  left_join(ex_agg, by = "USUBJID") %>%
  left_join(rand_agg, by = "USUBJID")
dm_for_rs$RFSTDTC <- dplyr::coalesce(dm_for_rs$RF_EX, dm_for_rs$RF_RAND)
dm_for_rs <- dm_for_rs[, c("USUBJID","RFSTDTC"), drop = FALSE]
dm_for_rs <- dm_for_rs[order(dm_for_rs$USUBJID, method="radix", na.last=FALSE), , drop=FALSE]
cat("dm_for_rs n:", nrow(dm_for_rs), " non-missing RFSTDTC:", sum(!is.na(dm_for_rs$RFSTDTC)), "\n")

# ---- Map RS domain (merge rs_prep + dm_for_rs, keep if r) ----
if ("RFSTDTC" %in% names(rs_prep)) rs_prep$RFSTDTC <- NULL
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

# sort by USUBJID RSDTC (missing first, SAS order)
o <- order(sdtm_rs1$USUBJID, sdtm_rs1$RSDTC, method = "radix", na.last = FALSE)
sdtm_rs1 <- sdtm_rs1[o, , drop = FALSE]

# Derive RSSEQ with FIRST./LAST. logic via dplyr (retain)
sdtm_rs1 <- sdtm_rs1 %>%
  group_by(USUBJID) %>%
  mutate(RSSEQ = dplyr::row_number()) %>%
  ungroup() %>%
  as.data.frame()
sdtm_rs1$RSSEQ <- as.numeric(sdtm_rs1$RSSEQ)

sdtm_rs <- sdtm_rs1[, c("STUDYID","DOMAIN","USUBJID","RSSEQ","RSTESTCD","RSTEST","RSCAT","RSORRES","RSSTRESC","RSEVAL","RSDTC","RSSTDY","VISIT"), drop = FALSE]
sdtm_rs$RSSEQ <- as.numeric(sdtm_rs$RSSEQ)
sdtm_rs$RSSTDY <- as.numeric(sdtm_rs$RSSTDY)

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

# ---- Export ----
write.csv(sdtm_rs, file.path(out, "rs.csv"), row.names = FALSE, na = "")

print_df <- cbind(Obs = seq_len(nrow(sdtm_rs)), as.data.frame(sdtm_rs, stringsAsFactors = FALSE))
write.csv(print_df, file.path(out, "P027.csv"), row.names = FALSE, na = "")

txt_path <- file.path(out, "P027.txt")
lst_path <- file.path(out, "P027.lst")
con <- file(txt_path, open = "wt")
writeLines("PROC PRINT DATA=SDTM_RS;", con)
writeLines(paste0("Study: ONCO-2026  Domain: RS (RECIST 1.1)  N=", nrow(sdtm_rs)), con)
writeLines("", con)
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
invisible(file.copy(txt_path, lst_path, overwrite = TRUE))

cat("Done. Wrote rs.csv, P027.csv, P027.txt, P027.lst to", out, "\n")

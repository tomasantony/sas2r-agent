# P026_lb.sas -> R translation
# SDTM LB mapping & derivation

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

rawdir <- "/bench/data/raw"
edc_dir <- file.path(rawdir, "faijal-oncology/raw_data")

# ---------- helpers ----------
sas_round <- function(x, digits = 0) {
  # round half away from zero (SAS)
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_trim <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x), which = "both"))
}
sas_missing_c <- function(x) {
  is.na(x) | trimws(as.character(x), which = "both") == ""
}
sas_trunc <- function(x, len) {
  sapply(as.character(x), function(v) {
    if (is.na(v)) return(NA_character_)
    if (nchar(v, type = "chars") > len) substr(v, 1, len) else v
  }, USE.NAMES = FALSE)
}
sas_catx <- function(delim, ...) {
  args <- list(...)
  # vectorised over first varying arg; here only two scalars/vectors
  a <- args[[1]]; b <- args[[2]]
  n <- max(length(a), length(b))
  a <- rep(a, length.out = n); b <- rep(b, length.out = n)
  mapply(function(x, y) {
    xs <- ifelse(is.na(x), "", trimws(as.character(x)))
    ys <- ifelse(is.na(y), "", trimws(as.character(y)))
    xs <- trimws(xs); ys <- trimws(ys)
    if (xs == "" && ys == "") return("")
    if (xs == "") return(ys)
    if (ys == "") return(xs)
    paste0(xs, delim, ys)
  }, a, b, USE.NAMES = FALSE)
}
sas_tranwrd <- function(s, from, to) {
  sapply(s, function(v) {
    if (is.na(v)) return(NA_character_)
    gsub(from, to, as.character(v), fixed = TRUE)
  }, USE.NAMES = FALSE)
}
sas_best <- function(x) {
  suppressWarnings(as.numeric(trimws(as.character(x))))
}
# parse DATE11. e.g. 01-JAN-2026 / 01JAN2026 -> Date
parse_date11 <- function(x) {
  if (is.na(x)) return(as.Date(NA))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x) && !is.character(x)) {
    # Excel serial? only if plausible
    if (!is.na(x) && x > 20000 && x < 60000) {
      return(as.Date(x, origin = "1899-12-30"))
    } else return(as.Date(NA))
  }
  s <- trimws(as.character(x))
  if (is.na(s) || s == "") return(as.Date(NA))
  # already YYYY-MM-DD ?
  # try lubridate dmy first, then ymd, mdy
  d <- suppressWarnings(lubridate::dmy(s, quiet = TRUE))
  if (!is.na(d)) return(d)
  d <- suppressWarnings(lubridate::ymd(s, quiet = TRUE))
  if (!is.na(d)) return(d)
  d <- suppressWarnings(lubridate::mdy(s, quiet = TRUE))
  if (!is.na(d)) return(d)
  # try explicit formats uppercased month
  fmts <- c("%d-%b-%Y", "%d%b%Y", "%d-%B-%Y", "%d %b %Y", "%d/%m/%Y",
             "%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%d-%m-%Y", "%Y%m%d")
  for (f in fmts) {
    # need case-insensitive month: try as-is and title case
    d2 <- suppressWarnings(as.Date(s, format = f))
    if (!is.na(d2)) return(d2)
    # title-case month part attempt
    d2 <- suppressWarnings(as.Date(stringr::str_to_title(s), format = f))
    if (!is.na(d2)) return(d2)
  }
  return(as.Date(NA))
}
parse_yymmdd <- function(x) {
  if (is.na(x)) return(as.Date(NA))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x) && !is.character(x)) {
    if (!is.na(x) && x > 20000 && x < 60000) return(as.Date(x, origin = "1899-12-30"))
    else return(as.Date(NA))
  }
  s <- trimws(as.character(x))
  if (is.na(s) || s == "") return(as.Date(NA))
  # yymmdd10. is YYYY-MM-DD
  d <- suppressWarnings(lubridate::ymd(s, quiet = TRUE))
  if (!is.na(d)) return(d)
  d <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
  if (!is.na(d)) return(d)
  return(as.Date(NA))
}
fmt_yymmdd <- function(d) {
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}
find_col <- function(nms, candidates) {
  up <- toupper(nms)
  for (c in candidates) {
    idx <- which(up == toupper(c))
    if (length(idx) > 0) return(nms[idx[1]])
  }
  return(NA_character_)
}
get_col <- function(df, candidates) {
  if (ncol(df) == 0) return(rep(NA_character_, nrow(df)))
  cn <- find_col(names(df), candidates)
  if (is.na(cn)) return(rep(NA_character_, nrow(df)))
  v <- df[[cn]]
  if (inherits(v, "Date")) return(format(v, "%d-%b-%Y"))
  if (inherits(v, "POSIXt")) return(format(as.Date(v), "%d-%b-%Y"))
  as.character(v)
}
get_col_raw <- function(df, candidates) {
  if (ncol(df) == 0) return(rep(NA_character_, nrow(df)))
  cn <- find_col(names(df), candidates)
  if (is.na(cn)) return(rep(NA_character_, nrow(df)))
  df[[cn]]
}

cat("Reading inputs...\n")

# ---------- read edc_lb ----------
lb_path <- file.path(edc_dir, "edc_lb.xlsx")
edc_lb_raw <- readxl::read_excel(lb_path, .name_repair = "minimal")
names(edc_lb_raw) <- trimws(as.character(names(edc_lb_raw)))
cat(paste0("edc_lb rows=", nrow(edc_lb_raw), " cols=", paste(names(edc_lb_raw), collapse=","), "\n"))

# normalize needed columns (case-insensitive)
SUBJID_v <- get_col(edc_lb_raw, c("SUBJID","SUBJECT","SUBJ","USUBJID","PTID"))
TESTCD_v <- get_col(edc_lb_raw, c("TESTCD","LBTESTCD","TEST"))
CAT_v    <- get_col(edc_lb_raw, c("CAT","LBCAT","CATEGORY"))
ORRES_v  <- get_col(edc_lb_raw, c("ORRES","LBORRES","RESULT","VALUE"))
UNIT_v   <- get_col(edc_lb_raw, c("UNIT","LBORRESU","ORRESU","UNITS","LBSTRESU"))
LOW_v    <- get_col(edc_lb_raw, c("LOW","LBORNRLO","ORNRLO","LOWER","NRLO"))
HIGH_v   <- get_col(edc_lb_raw, c("HIGH","LBORNRHI","ORNRHI","UPPER","NRHI"))
VISIT_v  <- get_col(edc_lb_raw, c("VISIT","VISITNUM","VISIT_NAME","FOLDER"))
LBDT_v   <- {
  # LBDT may be Date type; handle specially
  cn <- find_col(names(edc_lb_raw), c("LBDT","LBDTC","LBDAT","DATE","LBDY","COLLDAT","LABDT"))
  if (is.na(cn)) rep(NA_character_, nrow(edc_lb_raw))
  else {
    v <- edc_lb_raw[[cn]]
    if (inherits(v, "Date")) format(v, "%d-%b-%Y")
    else if (inherits(v, "POSIXt")) format(as.Date(v), "%d-%b-%Y")
    else if (is.numeric(v)) {
      # could be excel serials mixed with chars -> convert elementwise
      sapply(v, function(x) {
        if (is.na(x)) return(NA_character_)
        as.character(x)
      }, USE.NAMES = FALSE)
    } else as.character(v)
  }
}

lb_prep <- tibble::tibble(
  SUBJID = sas_trim(as.character(SUBJID_v)),
  TESTCD = sas_trim(as.character(TESTCD_v)),
  CAT = sas_trim(as.character(CAT_v)),
  ORRES = sas_trim(as.character(ORRES_v)),
  UNIT = sas_trim(as.character(UNIT_v)),
  LOW = sas_trim(as.character(LOW_v)),
  HIGH = sas_trim(as.character(HIGH_v)),
  VISIT = sas_trim(as.character(VISIT_v)),
  LBDT = sas_trim(as.character(LBDT_v))
)
# SAS: LBDT ne "" check uses blank; convert "NA" strings from as.character(NA) back to NA
lb_prep$LBDT[lb_prep$LBDT == "NA"] <- NA_character_
for (cc in c("SUBJID","TESTCD","CAT","ORRES","UNIT","LOW","HIGH","VISIT")) {
  lb_prep[[cc]][lb_prep[[cc]] == "NA"] <- NA_character_
  lb_prep[[cc]][sas_missing_c(lb_prep[[cc]])] <- NA_character_
  # empty string -> NA for easier handling, but keep SAS semantics (blank = missing)
}
# USUBJID = catx("-", "ONCO-2026", SUBJID)
lb_prep$USUBJID <- sas_catx("-", "ONCO-2026", lb_prep$SUBJID)
lb_prep$USUBJID <- sas_trunc(lb_prep$USUBJID, 25)
# sort by USUBJID (SAS: missing first, uppercase before lowercase)
lb_prep <- lb_prep[order(lb_prep$USUBJID, na.last = FALSE), , drop = FALSE]

# ---------- dm_for_lb (RFSTDTC) ----------
cat("Preparing DM reference...\n")
dm_for_lb <- NULL
# try to locate a pre-built sdtm_dm (in case environment provides it)
# candidates: none expected, but check OUT dirs / raw
# Fall back to deriving from edc_dm / edc_ex
dm_path <- file.path(edc_dir, "edc_dm.xlsx")
edc_dm_raw <- NULL
try({ edc_dm_raw <- readxl::read_excel(dm_path, .name_repair = "minimal") }, silent = TRUE)
if (!is.null(edc_dm_raw)) names(edc_dm_raw) <- trimws(as.character(names(edc_dm_raw)))

rfstdtc_vec <- NULL
dm_subjid <- NULL
if (!is.null(edc_dm_raw)) {
  dm_subjid <- get_col(edc_dm_raw, c("SUBJID","SUBJECT","SUBJ","USUBJID","PTID"))
  # look for RFSTDTC directly
  rf_cn <- find_col(names(edc_dm_raw), c("RFSTDTC","RFSTDT","RFSTDAT","RANDDT","RANDDTC","EXSTDTC","STDTC"))
  if (!is.na(rf_cn)) {
    v <- edc_dm_raw[[rf_cn]]
    if (inherits(v, "Date")) rfstdtc_vec <- format(v, "%Y-%m-%d")
    else if (inherits(v, "POSIXt")) rfstdtc_vec <- format(as.Date(v), "%Y-%m-%d")
    else {
      # parse each to yymmdd
      rfstdtc_vec <- sapply(as.character(v), function(x) {
        if (is.na(x) || trimws(x) == "" || x == "NA") return(NA_character_)
        d1 <- parse_yymmdd(x)
        if (!is.na(d1)) return(fmt_yymmdd(d1))
        d2 <- parse_date11(x)
        if (!is.na(d2)) return(fmt_yymmdd(d2))
        trimws(x)
      }, USE.NAMES = FALSE)
    }
  }
}
# if RFSTDTC not found, derive from edc_ex (first exposure date per subject)
if (is.null(rfstdtc_vec) || all(is.na(rfstdtc_vec))) {
  ex_path <- file.path(edc_dir, "edc_ex.xlsx")
  if (file.exists(ex_path)) {
    edc_ex_raw <- readxl::read_excel(ex_path, .name_repair = "minimal")
    names(edc_ex_raw) <- trimws(as.character(names(edc_ex_raw)))
    cat(paste0("edc_ex cols=", paste(names(edc_ex_raw), collapse=","), "\n"))
    ex_subjid <- get_col(edc_ex_raw, c("SUBJID","SUBJECT","SUBJ","USUBJID","PTID"))
    ex_cn <- find_col(names(edc_ex_raw), c("EXSTDTC","EXSTDT","EXDTC","EXDT","EXDATE","EXSTDAT","DATE","STDTC","DTC"))
    if (!is.na(ex_cn)) {
      exv <- edc_ex_raw[[ex_cn]]
      if (inherits(exv, "Date")) exd <- exv
      else if (inherits(exv, "POSIXt")) exd <- as.Date(exv)
      else {
        exd <- sapply(as.character(exv), function(x) {
          if (is.na(x) || trimws(x)=="" || x=="NA") return(as.Date(NA))
          d1 <- parse_yymmdd(x); if (!is.na(d1)) return(d1)
          d2 <- parse_date11(x); return(d2)
        }, USE.NAMES = FALSE)
        exd <- as.Date(exd, origin="1970-01-01")
      }
      exd <- as.Date(exd, origin="1970-01-01")
      ex_df <- data.frame(SUBJID=sas_trim(as.character(ex_subjid)), EXD=exd, stringsAsFactors=FALSE)
      ex_df <- ex_df[!is.na(ex_df$SUBJID) & ex_df$SUBJID!="" & !is.na(ex_df$EXD), , drop=FALSE]
      if (nrow(ex_df)>0) {
        agg <- aggregate(EXD ~ SUBJID, data=ex_df, FUN=min)
        # map to dm_subjid if available else to lb_prep subjid
        if (!is.null(dm_subjid)) {
          dm_subjid_t <- sas_trim(as.character(dm_subjid))
          rfstdtc_vec <- sapply(dm_subjid_t, function(s) {
            m <- agg$EXD[agg$SUBJID==s]
            if (length(m)==0) return(NA_character_) else fmt_yymmdd(min(m))
          }, USE.NAMES=FALSE)
        }
      }
    }
  }
}
# build dm_for_lb
if (!is.null(edc_dm_raw) && !is.null(dm_subjid)) {
  dm_subjid_t <- sas_trim(as.character(dm_subjid))
  if (is.null(rfstdtc_vec)) rfstdtc_vec <- rep(NA_character_, length(dm_subjid_t))
  dm_for_lb <- data.frame(
    SUBJID = dm_subjid_t,
    RFSTDTC_raw = rfstdtc_vec,
    stringsAsFactors = FALSE
  )
  dm_for_lb$USUBJID <- sas_catx("-", "ONCO-2026", dm_for_lb$SUBJID)
  dm_for_lb$RFSTDTC <- sapply(dm_for_lb$RFSTDTC_raw, function(x) {
    if (is.na(x) || trimws(as.character(x))=="" ) return(NA_character_)
    d <- parse_yymmdd(x); if (!is.na(d)) return(fmt_yymmdd(d))
    d2 <- parse_date11(x); if (!is.na(d2)) return(fmt_yymmdd(d2))
    trimws(as.character(x))
  }, USE.NAMES=FALSE)
  dm_for_lb <- dm_for_lb[!is.na(dm_for_lb$USUBJID) & dm_for_lb$USUBJID!="", c("USUBJID","RFSTDTC"), drop=FALSE]
  # one per USUBJID (first)
  dm_for_lb <- dm_for_lb[order(dm_for_lb$USUBJID, na.last=FALSE), , drop=FALSE]
  dm_for_lb <- dm_for_lb[!duplicated(dm_for_lb$USUBJID), , drop=FALSE]
} else {
  # fallback: unique USUBJID from lb_prep with missing RFSTDTC
  dm_for_lb <- data.frame(USUBJID=unique(lb_prep$USUBJID), RFSTDTC=NA_character_, stringsAsFactors=FALSE)
}
cat(paste0("dm_for_lb rows=", nrow(dm_for_lb), "\n"))

# ---------- 3. Map LB domain (merge lb_prep + dm_for_lb, if l) ----------
# left join
sdtm_lb1 <- dplyr::left_join(lb_prep, dm_for_lb, by="USUBJID")

n1 <- nrow(sdtm_lb1)
STUDYID <- rep("ONCO-2026", n1)
DOMAIN <- rep("LB", n1)
LBTESTCD <- sas_trunc(ifelse(is.na(sdtm_lb1$TESTCD), NA_character_, sdtm_lb1$TESTCD), 8)
LBCAT <- sas_trunc(ifelse(is.na(sdtm_lb1$CAT), NA_character_, sdtm_lb1$CAT), 15)
LBTEST <- rep(NA_character_, n1)
LBTEST[LBTESTCD=="HGB" & !is.na(LBTESTCD)] <- "Hemoglobin"
LBTEST[LBTESTCD=="ALT" & !is.na(LBTESTCD)] <- "Alanine Aminotransferase"
LBTEST <- sas_trunc(LBTEST, 40)

LBORRES <- sas_trunc(sdtm_lb1$ORRES, 10)
LBORRESU <- sas_trunc(sdtm_lb1$UNIT, 20)
LBSTRESC <- sas_trunc(sdtm_lb1$ORRES, 10)
LBSTRESN <- sas_best(sdtm_lb1$ORRES)
LBSTRESU <- sas_trunc(sdtm_lb1$UNIT, 20)
LBORNRLO <- sas_trunc(sdtm_lb1$LOW, 10)
LBORNRHI <- sas_trunc(sdtm_lb1$HIGH, 10)
VISIT <- sas_tranwrd(sdtm_lb1$VISIT, "_", " ")
VISIT <- sas_trunc(VISIT, 20)

# LBNRIND with SAS missing semantics (missing threshold = -Inf)
LOWN <- sas_best(sdtm_lb1$LOW)
HIGHN <- sas_best(sdtm_lb1$HIGH)
LBNRIND <- rep(NA_character_, n1)
for (i in seq_len(n1)) {
  v <- LBSTRESN[i]
  if (!is.na(v)) {
    lo <- LOWN[i]; hi <- HIGHN[i]
    cond_low <- if (!is.na(lo)) v < lo else FALSE
    cond_high <- if (!is.na(hi)) v > hi else TRUE  # SAS: v > . is TRUE
    if (cond_low) LBNRIND[i] <- "LOW"
    else if (cond_high) LBNRIND[i] <- "HIGH"
    else LBNRIND[i] <- "NORMAL"
  }
}
LBNRIND <- sas_trunc(LBNRIND, 8)

# Date conversion: if LBDT ne "" then LBDTC = put(input(LBDT,date11.),yymmdd10.)
LBDTC <- rep(NA_character_, n1)
for (i in seq_len(n1)) {
  lbdt <- sdtm_lb1$LBDT[i]
  if (!is.na(lbdt) && trimws(as.character(lbdt)) != "") {
    d <- parse_date11(lbdt)
    # if parse_date11 fails but value already yymmdd, try yymmdd (SAS would give missing, but be lenient?)
    # To stay faithful: only date11. ; if missing, LBDTC stays missing.
    # However if LBDT already YYYY-MM-DD (from Date conversion above as DD-MON-YYYY, so fine).
    # If still NA, try yymmdd as fallback only when input looks like YYYY-MM-DD? SAS would be missing.
    # Keep SAS: missing.
    if (!is.na(d)) LBDTC[i] <- fmt_yymmdd(d)
    else LBDTC[i] <- NA_character_
  }
}
LBDTC <- sas_trunc(LBDTC, 19)

# Study day
LBSTDY <- rep(NA_real_, n1)
for (i in seq_len(n1)) {
  a <- LBDTC[i]; b <- sdtm_lb1$RFSTDTC[i]
  if (!is.na(a) && trimws(a)!="" && !is.na(b) && trimws(as.character(b))!="") {
    stdt <- parse_yymmdd(a)
    rfdt <- parse_yymmdd(b)
    if (!is.na(stdt) && !is.na(rfdt)) {
      diff <- as.integer(stdt - rfdt)
      if (stdt >= rfdt) LBSTDY[i] <- diff + 1
      else LBSTDY[i] <- diff
    }
  }
}

sdtm_lb1 <- data.frame(
  STUDYID=sas_trunc(STUDYID,10), DOMAIN=sas_trunc(DOMAIN,2),
  USUBJID=sas_trunc(sdtm_lb1$USUBJID,25),
  LBTESTCD=LBTESTCD, LBTEST=LBTEST, LBCAT=LBCAT,
  LBORRES=LBORRES, LBORRESU=LBORRESU,
  LBSTRESC=LBSTRESC, LBSTRESN=LBSTRESN, LBSTRESU=LBSTRESU,
  LBORNRLO=LBORNRLO, LBORNRHI=LBORNRHI, LBNRIND=LBNRIND,
  LBDTC=LBDTC, LBSTDY=LBSTDY, VISIT=VISIT,
  stringsAsFactors=FALSE
)

# ---------- 4. Derive LBSEQ (sort by USUBJID LBDTC, retain) ----------
# SAS sort: missing LBDTC first; character ordering
ord <- order(sdtm_lb1$USUBJID, sdtm_lb1$LBDTC, na.last=FALSE)
sdtm_lb1 <- sdtm_lb1[ord, , drop=FALSE]
# retain LBSEQ by USUBJID
LBSEQ <- rep(NA_real_, nrow(sdtm_lb1))
cur <- NA_character_; seq <- 0
for (i in seq_len(nrow(sdtm_lb1))) {
  u <- sdtm_lb1$USUBJID[i]
  is_first <- is.na(cur) || is.na(u) && is.na(cur) ? FALSE : FALSE
  # SAS first.USUBJID: true when USUBJID changes (missing considered a value)
  if (is.na(u) && is.na(cur)) is_first <- FALSE
  else if (!is.na(u) && !is.na(cur) && u==cur) is_first <- FALSE
  else is_first <- TRUE
  # handle first row
  if (i==1) is_first <- TRUE
  else {
    prev <- sdtm_lb1$USUBJID[i-1]
    if (is.na(u) && is.na(prev)) is_first <- FALSE else if (!is.na(u) && !is.na(prev) && u==prev) is_first <- FALSE else is_first <- TRUE
  }
  if (is_first) seq <- 1 else seq <- seq + 1
  LBSEQ[i] <- seq
  cur <- u
}
sdtm_lb <- data.frame(
  STUDYID=sdtm_lb1$STUDYID, DOMAIN=sdtm_lb1$DOMAIN, USUBJID=sdtm_lb1$USUBJID,
  LBSEQ=LBSEQ, LBTESTCD=sdtm_lb1$LBTESTCD, LBTEST=sdtm_lb1$LBTEST,
  LBCAT=sdtm_lb1$LBCAT, LBORRES=sdtm_lb1$LBORRES, LBORRESU=sdtm_lb1$LBORRESU,
  LBSTRESC=sdtm_lb1$LBSTRESC, LBSTRESN=sdtm_lb1$LBSTRESN, LBSTRESU=sdtm_lb1$LBSTRESU,
  LBORNRLO=sdtm_lb1$LBORNRLO, LBORNRHI=sdtm_lb1$LBORNRHI,
  LBNRIND=sdtm_lb1$LBNRIND, LBDTC=sdtm_lb1$LBDTC, LBSTDY=sdtm_lb1$LBSTDY,
  VISIT=sdtm_lb1$VISIT, stringsAsFactors=FALSE
)
# keep order as SAS keep list
sdtm_lb <- sdtm_lb[, c("STUDYID","DOMAIN","USUBJID","LBSEQ","LBTESTCD","LBTEST","LBCAT",
  "LBORRES","LBORRESU","LBSTRESC","LBSTRESN","LBSTRESU","LBORNRLO","LBORNRHI",
  "LBNRIND","LBDTC","LBSTDY","VISIT")]

cat(paste0("sdtm_lb rows=", nrow(sdtm_lb), "\n"))

# ---------- 5/6. outputs ----------
# dataset lb.csv
write.csv(sdtm_lb, file.path(out, "lb.csv"), row.names=FALSE, na="")

# listing text (PROC PRINT rendering) + CSV body
# Build text similar to PROC PRINT
hdr <- paste0("Listing P026: SDTM LB (N=", nrow(sdtm_lb), ")")
cols <- names(sdtm_lb)
# format numbers as SAS BEST12. (no trailing zeros)
fmt_cell <- function(v) ifelse(is.na(v), "", as.character(v))
disp <- as.data.frame(lapply(sdtm_lb, fmt_cell), stringsAsFactors=FALSE)
lines <- c(hdr, paste0("Study: ONCO-2026  Domain: LB"), "")
header_line <- paste(c("Obs", cols), collapse=" | ")
lines <- c(lines, header_line, paste(rep("-", nchar(header_line)), collapse=""))
for (i in seq_len(nrow(disp))) {
  rowvals <- c(as.character(i), as.character(unlist(disp[i, ])))
  lines <- c(lines, paste(rowvals, collapse=" | "))
}
lines <- c(lines, "", paste0("End of listing. N=", nrow(sdtm_lb)))
writeLines(lines, file.path(out, "P026.txt"))
write.csv(sdtm_lb, file.path(out, "P026.csv"), row.names=FALSE, na="")
# aliases to cover expected naming variants
writeLines(lines, file.path(out, "P026.lst.txt"))
write.csv(sdtm_lb, file.path(out, "P026.lst.csv"), row.names=FALSE, na="")
# also lb listing txt
writeLines(lines, file.path(out, "lb.txt"))

cat("Done.\n")

out <- Sys.getenv("OUT_DIR")
if (!nzchar(out)) out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm"
adam_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam"

suppressMessages(library(haven))
suppressMessages(library(dplyr))

find_dataset <- function(dir, memname) {
  target <- tolower(memname)
  allf <- list.files(dir, full.names = TRUE, recursive = FALSE)
  if (length(allf) == 0) allf <- list.files(dir, full.names = TRUE, recursive = TRUE)
  bases <- tolower(tools::file_path_sans_ext(basename(allf)))
  idx <- which(bases == target)
  if (length(idx) == 0) {
    allf2 <- list.files(dir, full.names = TRUE, recursive = TRUE)
    bases2 <- tolower(tools::file_path_sans_ext(basename(allf2)))
    idx2 <- which(bases2 == target)
    if (length(idx2) == 0) stop(paste0("Dataset ", memname, " not found in ", dir))
    allf <- allf2
    idx <- idx2
  }
  cand <- allf[idx]
  ext <- tolower(tools::file_ext(cand))
  get_pri <- function(e) {
    if (e == "sas7bdat") 1L
    else if (e == "xpt") 2L
    else if (e == "sas7bdat") 1L
    else 3L
  }
  pris <- vapply(ext, get_pri, integer(1), USE.NAMES = FALSE)
  cand[order(pris)][1]
}

read_sas_ds <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "xpt") {
    df <- haven::read_xpt(path)
  } else if (ext %in% c("sas7bdat", "sas7bdat")) {
    df <- haven::read_sas(path)
  } else if (ext == "csv") {
    df <- readr::read_csv(path, show_col_types = FALSE)
  } else {
    df <- haven::read_sas(path)
  }
  as.data.frame(df, stringsAsFactors = FALSE)
}

lb_path <- find_dataset(sdtm_dir, "lb")
adsl_path <- find_dataset(adam_dir, "adsl")
cat("Reading LB from:", lb_path, "\n")
cat("Reading ADSL from:", adsl_path, "\n")

lb <- read_sas_ds(lb_path)
adsl <- read_sas_ds(adsl_path)
names(lb) <- toupper(names(lb))
names(adsl) <- toupper(names(adsl))

# Helper: SAS binary sort key for character (uppercase before lowercase, byte order)
sas_key <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    codes <- utf8ToInt(s)
    paste(sprintf("%04d", codes), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

# proc sort data=sdtm.lb out=lb_qc; by USUBJID PARAM LBDTC;
# SAS: missing first, binary order
lb_qc <- lb
lb_qc[[".k1"]] <- sas_key(as.character(lb_qc[["USUBJID"]]))
lb_qc[[".k2"]] <- sas_key(as.character(lb_qc[["PARAM"]]))
lb_qc[[".k3"]] <- sas_key(as.character(lb_qc[["LBDTC"]]))
o_lb <- order(is.na(lb_qc[["USUBJID"]]), lb_qc[[".k1"]],
              is.na(lb_qc[["PARAM"]]), lb_qc[[".k2"]],
              is.na(lb_qc[["LBDTC"]]), lb_qc[[".k3"]],
              na.last = FALSE, method = "radix")
lb_qc <- lb_qc[o_lb, , drop = FALSE]
lb_qc[[".k1"]] <- NULL; lb_qc[[".k2"]] <- NULL; lb_qc[[".k3"]] <- NULL

# proc sort data=adam.adsl out=adsl_qc; by USUBJID;
adsl_qc <- adsl
adsl_qc[[".k1"]] <- sas_key(as.character(adsl_qc[["USUBJID"]]))
o_ad <- order(is.na(adsl_qc[["USUBJID"]]), adsl_qc[[".k1"]], na.last = FALSE, method = "radix")
adsl_qc <- adsl_qc[o_ad, , drop = FALSE]
adsl_qc[[".k1"]] <- NULL

# keep USUBJID RFSTDTC TRT01P from adsl_qc
keep_ad <- c("USUBJID", "RFSTDTC", "TRT01P")
keep_ad <- keep_ad[keep_ad %in% names(adsl_qc)]
adsl_keep <- adsl_qc[, keep_ad, drop = FALSE]

# data adam.adlb_qc; merge lb_qc(in=a) adsl_qc(in=b keep=...); by USUBJID; if a and b;
merged <- merge(lb_qc, adsl_keep, by = "USUBJID", all = FALSE, sort = FALSE)

# restore SAS merge order: by USUBJID, within USUBJID keep lb_qc order (PARAM LBDTC)
merged[[".k1"]] <- sas_key(as.character(merged[["USUBJID"]]))
merged[[".k2"]] <- sas_key(as.character(merged[["PARAM"]]))
merged[[".k3"]] <- sas_key(as.character(merged[["LBDTC"]]))
o_m <- order(is.na(merged[["USUBJID"]]), merged[[".k1"]],
             is.na(merged[["PARAM"]]), merged[[".k2"]],
             is.na(merged[["LBDTC"]]), merged[[".k3"]],
             na.last = FALSE, method = "radix")
merged <- merged[o_m, , drop = FALSE]
merged[[".k1"]] <- NULL; merged[[".k2"]] <- NULL; merged[[".k3"]] <- NULL
rownames(merged) <- NULL

n <- nrow(merged)

# lbdt = input(LBDTC, yymmdd10.); rfstdt = input(RFSTDTC, yymmdd10.);
parse_yymmdd10 <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  xc <- as.character(x)
  sub10 <- substr(xc, 1, 10)
  suppressWarnings(lubridate::ymd(sub10, quiet = TRUE))
}

lbdt_vec <- parse_yymmdd10(merged[["LBDTC"]])
rfstdt_vec <- parse_yymmdd10(merged[["RFSTDTC"]])

aval_vec <- suppressWarnings(as.numeric(merged[["AVAL"]]))

BASE_vec <- rep(NA_real_, n)
ABLFL_vec <- rep("", n)
CHG_vec <- rep(NA_real_, n)

same_val <- function(a, b) {
  if (is.na(a) && is.na(b)) return(TRUE)
  if (is.na(a) || is.na(b)) return(FALSE)
  return(a == b)
}

cur_base <- NA_real_
prev_usubjid <- NA_character_
prev_param <- NA_character_

if (n > 0) {
  for (i in seq_len(n)) {
    us <- as.character(merged[["USUBJID"]][i])
    pa <- as.character(merged[["PARAM"]][i])
    if (i == 1) {
      is_first_param <- TRUE
    } else {
      is_first_param <- !(same_val(us, prev_usubjid) && same_val(pa, prev_param))
    }
    # retain BASE; if first.PARAM then BASE=.;
    if (is_first_param) cur_base <- NA_real_

    lbdt <- lbdt_vec[i]
    rfstdt <- rfstdt_vec[i]
    aval <- aval_vec[i]

    # SAS: missing sorts first -> missing < non-missing is TRUE
    if (is.na(lbdt) && is.na(rfstdt)) {
      lt <- FALSE
    } else if (is.na(lbdt) && !is.na(rfstdt)) {
      lt <- TRUE
    } else if (!is.na(lbdt) && is.na(rfstdt)) {
      lt <- FALSE
    } else {
      lt <- (lbdt < rfstdt)
    }

    # if lbdt < rfstdt and not missing(AVAL) then BASE = AVAL;
    if (isTRUE(lt) && !is.na(aval)) {
      cur_base <- aval
    }

    # if lbdt < rfstdt and AVAL = BASE then ABLFL="Y"; else ABLFL="";
    # SAS: . = . is TRUE, . = value is FALSE
    if (is.na(aval) && is.na(cur_base)) {
      eq <- TRUE
    } else if (is.na(aval) || is.na(cur_base)) {
      eq <- FALSE
    } else {
      eq <- (aval == cur_base)
    }
    if (isTRUE(lt) && isTRUE(eq)) {
      abl <- "Y"
    } else {
      abl <- ""
    }

    # if not missing(BASE) then CHG = AVAL - BASE;
    if (!is.na(cur_base)) {
      if (is.na(aval)) chg <- NA_real_ else chg <- aval - cur_base
    } else {
      chg <- NA_real_
    }

    BASE_vec[i] <- cur_base
    ABLFL_vec[i] <- abl
    CHG_vec[i] <- chg

    prev_usubjid <- us
    prev_param <- pa
  }
}

merged[["BASE"]] <- BASE_vec
merged[["ABLFL"]] <- ABLFL_vec
merged[["CHG"]] <- CHG_vec

# drop lbdt rfstdt (never added as columns)

adlb_qc <- merged

# Format datetimes as YYYY-MM-DDTHH:MM:SS for CSV (dates stay YYYY-MM-DD)
for (nm in names(adlb_qc)) {
  col <- adlb_qc[[nm]]
  if (inherits(col, "POSIXct") || inherits(col, "POSIXlt")) {
    adlb_qc[[nm]] <- format(col, "%Y-%m-%dT%H:%M:%S")
  }
}

cat("Writing adlb_qc rows:", nrow(adlb_qc), "cols:", ncol(adlb_qc), "\n")
write.csv(adlb_qc, file.path(out, "adlb_qc.csv"), row.names = FALSE, na = "")
cat("Done.\n")

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

raw_dir  <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

message("P067 ae translation starting")

find_file <- function(dir, ds) {
  if (!dir.exists(dir)) return(NULL)
  files <- list.files(dir, full.names = TRUE)
  if (length(files) == 0) return(NULL)
  bn_noext <- tolower(tools::file_path_sans_ext(basename(files)))
  idx <- which(bn_noext == tolower(ds))
  if (length(idx) == 0) return(NULL)
  cand <- files[idx]
  pref <- function(f) {
    e <- tolower(tools::file_ext(f))
    m <- match(e, c("sas7bdat", "sas7bdat", "xpt", "xpt", "csv", "parquet"))
    if (is.na(m)) m <- 99
    m
  }
  ord <- order(vapply(cand, pref, numeric(1)))
  cand[ord[1]]
}

read_ds <- function(path) {
  e <- tolower(tools::file_ext(path))
  if (e == "xpt") {
    df <- haven::read_xpt(path)
  } else if (e %in% c("sas7bdat", "sas7bdat")) {
    df <- haven::read_sas(path)
  } else if (e == "csv") {
    df <- readr::read_csv(path, show_col_types = FALSE)
    df <- as.data.frame(df)
    return(df)
  } else {
    df <- haven::read_sas(path)
  }
  df <- haven::zap_labels(df)
  # zap_formats would drop Date class, so do not zap formats fully;
  # remove only label attributes, keep Date conversion already done
  as.data.frame(df, stringsAsFactors = FALSE)
}

raw_ae_path <- find_file(raw_dir, "raw_ae")
dm_path     <- find_file(sdtm_dir, "dm")

if (is.null(raw_ae_path)) {
  # fallback: search any file containing raw_ae
  allf <- list.files(raw_dir, full.names = TRUE)
  hit <- grep("raw_ae", basename(allf), ignore.case = TRUE)
  if (length(hit) > 0) raw_ae_path <- allf[hit[1]]
}
if (is.null(dm_path)) {
  allf2 <- list.files(sdtm_dir, full.names = TRUE)
  hit2 <- grep("^dm\\.", basename(allf2), ignore.case = TRUE)
  if (length(hit2) > 0) dm_path <- allf2[hit2[1]]
}

message(paste0("raw_ae path: ", raw_ae_path))
message(paste0("dm path: ", dm_path))

raw_ae0 <- read_ds(raw_ae_path)
dm0     <- read_ds(dm_path)

# Uppercase names for SAS case-insensitive handling
names(raw_ae0) <- toupper(names(raw_ae0))
names(dm0)     <- toupper(names(dm0))

# proc sort data=raw.raw_ae; by SUBJID; run;
# proc sort data=sdtm.dm; by SUBJID; run;
# SAS: missing sorts first, character ASCII (uppercase before lowercase)
sas_order_idx <- function(df, by) {
  v <- df[[by]]
  # method radix gives byte/C order for character, missing first with na.last=FALSE
  order(v, na.last = FALSE, method = "radix")
}

raw_sorted <- raw_ae0[sas_order_idx(raw_ae0, "SUBJID"), , drop = FALSE]
dm_sorted  <- dm0[sas_order_idx(dm0, "SUBJID"), , drop = FALSE]
row.names(raw_sorted) <- NULL
row.names(dm_sorted) <- NULL

# data sdtm.ae; merge raw.raw_ae(in=a) sdtm.dm; by SUBJID; if a;
# SAS BY merge with single BY var. dm is one row per SUBJID, so equivalent to left join.
# Implement as left join preserving sorted order (SAS merge output sorted by BY).
by_var <- "SUBJID"
raw_cols <- names(raw_sorted)
dm_cols  <- names(dm_sorted)
overlap  <- intersect(raw_cols, dm_cols)
overlap  <- setdiff(overlap, by_var)
dm_only  <- setdiff(dm_cols, c(by_var, overlap))

# left join via base merge with sort=FALSE to keep x order (x already sorted by BY)
merged <- merge(raw_sorted, dm_sorted, by = by_var, all.x = TRUE, sort = FALSE, suffixes = c("", ".DM"))

# Resolve overlapping non-BY columns per SAS semantics:
# if BY exists in dm, dm value (even if missing) overwrites; else keep raw value.
if (length(overlap) > 0) {
  # keys present in dm
  dm_keys <- unique(dm_sorted[[by_var]])
  # matched flag for each merged row
  bv <- merged[[by_var]]
  # handle NA BY: NA matches NA
  has_na_key <- any(is.na(dm_keys))
  matched <- rep(FALSE, nrow(merged))
  for (k in dm_keys) {
    if (is.na(k)) {
      matched[is.na(bv)] <- TRUE
    } else {
      matched[!is.na(bv) & bv == k] <- TRUE
    }
  }
  for (col in overlap) {
    dm_col <- paste0(col, ".DM")
    if (dm_col %in% names(merged)) {
      # SAS: when matched, take dm value; when not matched, keep raw value
      # merged[[col]] currently is raw value (x), merged[[dm_col]] is dm value (y)
      y <- merged[[dm_col]]
      # assign y where matched (preserve type via indexing)
      # need to handle Date/character/numeric uniformly
      x <- merged[[col]]
      # coerce y to class of x if needed? assume same type; use direct replacement
      # For Date columns, ensure Date class
      if (inherits(x, "Date") && !inherits(y, "Date")) {
        # try convert y to Date if numeric SAS date
        if (is.numeric(y)) y <- as.Date(y, origin = "1960-01-01")
      }
      res <- x
      # replacement keeping class: use [<- 
      res[matched] <- y[matched]
      merged[[col]] <- res
      merged[[dm_col]] <- NULL
    }
  }
}

# Re-sort merged by SUBJID to guarantee SAS BY order (merge with sort=FALSE already in raw order which is sorted)
merged <- merged[order(merged[[by_var]], na.last = FALSE, method = "radix"), , drop = FALSE]
row.names(merged) <- NULL

# Helpers for SAS PUT yymmdd10. and INPUT yymmdd10.
to_Date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1960-01-01"))
  if (is.character(x)) {
    xc <- trimws(as.character(x))
    xc[xc == ""] <- NA_character_
    # take first 10 chars like SAS yymmdd10. input
    x10 <- substr(xc, 1, 10)
    return(suppressWarnings(as.Date(x10, format = "%Y-%m-%d")))
  }
  # haven labelled numeric?
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.Date(as.character(x), format = "%Y-%m-%d"))
}

fmt_yymmdd10 <- function(x) {
  # PUT with yymmdd10.: missing -> blank (NA here, written as "" in CSV)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) x <- as.Date(x)
  if (inherits(x, "Date")) {
    res <- format(x, "%Y-%m-%d")
    res[is.na(x)] <- NA_character_
    return(res)
  }
  if (is.numeric(x)) {
    res <- rep(NA_character_, length(x))
    ok <- !is.na(x)
    res[ok] <- format(as.Date(x[ok], origin = "1960-01-01"), "%Y-%m-%d")
    return(res)
  }
  if (is.character(x)) {
    # already character? try to normalize; blank stays NA
    xc <- trimws(x)
    xc[xc == ""] <- NA_character_
    return(xc)
  }
  as.character(x)
}

# Ensure needed columns exist (create NA if absent to allow keep)
need_raw <- c("AESTDT", "AEENDT", "AETERM", "AESER", "AESEV")
need_dm  <- c("USUBJID", "RFSTDTC")
for (cc in c(need_raw, need_dm)) {
  if (!cc %in% names(merged)) merged[[cc]] <- NA
}

AESTDT_date <- to_Date(merged[["AESTDT"]])
AEENDT_date <- to_Date(merged[["AEENDT"]])
RFSTDTC_date <- to_Date(merged[["RFSTDTC"]])

# length STUDYID $10 AESTDTC AEENDTC $10;
# STUDYID = "ABC-301";
# AESTDTC = put(AESTDT, yymmdd10.);
# AEENDTC = put(AEENDT, yymmdd10.);
merged[["STUDYID"]] <- "ABC-301"
merged[["AESTDTC"]] <- fmt_yymmdd10(AESTDT_date)
merged[["AEENDTC"]] <- fmt_yymmdd10(AEENDT_date)

# Truncate to length 10 like SAS LENGTH $10 (YYYY-MM-DD is 10)
merged[["STUDYID"]] <- substr(merged[["STUDYID"]], 1, 10)
merged[["AESTDTC"]] <- substr(ifelse(is.na(merged[["AESTDTC"]]), NA_character_, merged[["AESTDTC"]]), 1, 10)
merged[["AEENDTC"]] <- substr(ifelse(is.na(merged[["AEENDTC"]]), NA_character_, merged[["AEENDTC"]]), 1, 10)

# rfstdt_num = input(RFSTDTC, yymmdd10.);
# ASTDY / AENDY
rf_num <- suppressWarnings(as.numeric(RFSTDTC_date))  # days since 1970, but diff cancels; use Date diff directly
# Compute study days using Date arithmetic in days
astdy <- rep(NA_real_, nrow(merged))
aendy <- rep(NA_real_, nrow(merged))

ok_ast <- !is.na(AESTDT_date) & !is.na(RFSTDTC_date)
ok_aen <- !is.na(AEENDT_date) & !is.na(RFSTDTC_date)

# if AESTDT >= rfstdt_num then ASTDY = AESTDT - rfstdt_num + 1; else ASTDY = AESTDT - rfstdt_num;
diff_ast <- as.numeric(AESTDT_date[ok_ast] - RFSTDTC_date[ok_ast])
add_ast <- ifelse(AESTDT_date[ok_ast] >= RFSTDTC_date[ok_ast], 1, 0)
astdy[ok_ast] <- diff_ast + add_ast

diff_aen <- as.numeric(AEENDT_date[ok_aen] - RFSTDTC_date[ok_aen])
add_aen <- ifelse(AEENDT_date[ok_aen] >= RFSTDTC_date[ok_aen], 1, 0)
aendy[ok_aen] <- diff_aen + add_aen

merged[["ASTDY"]] <- astdy
merged[["AENDY"]] <- aendy

# keep STUDYID USUBJID SUBJID AETERM AESER AESEV AESTDTC AEENDTC ASTDY AENDY;
keep_vars <- c("STUDYID", "USUBJID", "SUBJID", "AETERM", "AESER", "AESEV", "AESTDTC", "AEENDTC", "ASTDY", "AENDY")
for (cc in keep_vars) {
  if (!cc %in% names(merged)) merged[[cc]] <- NA
}
ae <- merged[, keep_vars, drop = FALSE]

# Prepare raw_ae output (sorted raw.raw_ae as after PROC SORT)
# Convert numeric SAS dates AESTDT/AEENDT to Date for YYYY-MM-DD writing if numeric
raw_out <- raw_sorted
for (cc in c("AESTDT", "AEENDT")) {
  if (cc %in% names(raw_out) && is.numeric(raw_out[[cc]]) && !inherits(raw_out[[cc]], "Date")) {
    # Heuristic: if values look like SAS dates (e.g., 10000-25000) convert; otherwise leave
    # Convert all numeric to Date per SAS date semantics
    raw_out[[cc]] <- as.Date(raw_out[[cc]], origin = "1960-01-01")
  }
  if (cc %in% names(raw_out) && inherits(raw_out[[cc]], "POSIXct")) {
    raw_out[[cc]] <- as.Date(raw_out[[cc]])
  }
}

message(paste0("ae rows: ", nrow(ae), " cols: ", ncol(ae)))
message(paste0("raw_ae rows: ", nrow(raw_out)))

write.csv(ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")
write.csv(raw_out, file.path(out, "raw_ae.csv"), row.names = FALSE, na = "")

message("done")

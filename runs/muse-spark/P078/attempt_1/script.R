# P078_l_dv.sas -> R translation
# Listing 16.2.2 - Listing of Protocol Deviations
# Inputs: sdtm.dv, adam.adsl | Output: l_dv.csv + l_dv.txt
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

# ---- helpers ----
round_half_up <- function(x, digits = 0) {
  if (!is.numeric(x)) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x) * mult
  # SAS rounds half away from zero
  rounded <- floor(abs_x + 0.5 + 1e-10) / mult
  sign_x * rounded
}

zap_all_labels <- function(df) {
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled") || inherits(x, "haven_labelled_spss")) {
      x <- haven::zap_labels(x)
      x <- haven::zap_formats(x)
      x <- haven::zap_label(x)
      df[[nm]] <- x
    } else if (inherits(x, "labelled")) {
      df[[nm]] <- haven::zap_labels(x)
    }
  }
  df
}

read_lib_mem <- function(libdir, memname) {
  if (!dir.exists(libdir)) stop(paste0("Library dir not found: ", libdir))
  files <- list.files(libdir, full.names = TRUE)
  base_low <- tolower(basename(files))
  want_sas <- paste0(tolower(memname), ".sas7bdat")
  want_xpt <- paste0(tolower(memname), ".xpt")
  idx <- which(base_low == want_sas)
  if (length(idx) > 0) {
    fp <- files[idx[1]]
    cat("Reading SAS dataset:", fp, "\n")
    df <- haven::read_sas(fp)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df <- zap_all_labels(df)
    names(df) <- toupper(names(df))
    return(df)
  }
  idx <- which(base_low == want_xpt)
  if (length(idx) > 0) {
    fp <- files[idx[1]]
    cat("Reading XPT:", fp, "\n")
    df <- haven::read_xpt(fp)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df <- zap_all_labels(df)
    names(df) <- toupper(names(df))
    return(df)
  }
  # fallback: try direct paths
  for (ext in c(".sas7bdat", ".xpt")) {
    for (nm in c(memname, tolower(memname), toupper(memname))) {
      fp <- file.path(libdir, paste0(nm, ext))
      if (file.exists(fp)) {
        cat("Reading:", fp, "\n")
        if (ext == ".sas7bdat") df <- haven::read_sas(fp) else df <- haven::read_xpt(fp)
        df <- as.data.frame(df, stringsAsFactors = FALSE)
        df <- zap_all_labels(df)
        names(df) <- toupper(names(df))
        return(df)
      }
    }
  }
  stop(paste0("Member ", memname, " not found in ", libdir))
}

format_val_txt <- function(x) {
  if (length(x) == 0) return("")
  if (is.na(x)) return("")
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  }
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  # haven Date already Date; numeric Date-time handled above
  as.character(x)
}

cat("Step 1: Prepare data\n")

adsl <- read_lib_mem(adam_path, "adsl")
dv   <- read_lib_mem(sdtm_path, "dv")

# Ensure required columns exist (create NA if missing to avoid crash)
need_adsl <- c("USUBJID", "TRTA", "SITEID", "RANDFL")
for (v in need_adsl) if (!v %in% names(adsl)) { adsl[[v]] <- NA_character_; cat("WARNING: ADSL missing", v, "\n") }
need_dv <- c("USUBJID", "DVCAT", "DVSCAT", "DVTERM", "DVSTDTC", "DVENDTC", "EPOCH", "DVSEV")
for (v in need_dv) if (!v %in% names(dv)) { dv[[v]] <- NA_character_; cat("WARNING: DV missing", v, "\n") }

# Keep only needed cols to mimic SQL select
adsl_s <- adsl[, c("USUBJID", "TRTA", "SITEID", "RANDFL"), drop = FALSE]
dv_s   <- dv[, c("USUBJID", "DVCAT", "DVSCAT", "DVTERM", "DVSTDTC", "DVENDTC", "EPOCH", "DVSEV"), drop = FALSE]

# inner join on USUBJID, where RANDFL='Y' (case-sensitive, missing excluded)
# dplyr join
adsl_s$USUBJID <- as.character(adsl_s$USUBJID)
dv_s$USUBJID   <- as.character(dv_s$USUBJID)
dv_list <- dplyr::inner_join(dv_s, adsl_s, by = "USUBJID")

# WHERE b.RANDFL = 'Y'
dv_list <- dv_list[!is.na(dv_list$RANDFL) & dv_list$RANDFL == "Y", , drop = FALSE]

# Select/rename to final listing vars
# a.USUBJID, b.TRTA, b.SITEID as SITE, a.DVCAT as CATEGORY, a.DVSCAT as SUBCATEGORY,
# a.DVTERM as DESCRIPTION, a.DVSTDTC as START_DATE, a.DVENDTC as END_DATE, a.EPOCH, a.DVSEV as SEVERITY
dv_list <- dplyr::transmute(dv_list,
  USUBJID = as.character(USUBJID),
  TRTA = as.character(TRTA),
  SITE = as.character(SITEID),
  CATEGORY = as.character(DVCAT),
  SUBCATEGORY = as.character(DVSCAT),
  DESCRIPTION = as.character(DVTERM),
  START_DATE = as.character(DVSTDTC),
  END_DATE = as.character(DVENDTC),
  EPOCH = as.character(EPOCH),
  SEVERITY = as.character(DVSEV)
)

# Convert "" vs NA: haven may give NA for blanks; keep NA as missing (SAS missing sorts first)
# SAS sorts: missing first, uppercase before lowercase -> use C collation + na.last=FALSE
cat("Sorting by TRTA, SITE, USUBJID, START_DATE (SAS semantics)\n")
old_loc <- Sys.getlocale("LC_COLLATE")
# Try to set C collation for byte-order sorting
try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE)
# order() with na.last=FALSE puts NA first
ord <- order(dv_list$TRTA, dv_list$SITE, dv_list$USUBJID, dv_list$START_DATE,
             na.last = FALSE, method = "radix")
# radix method ignores locale; to enforce SAS byte order, use non-radix when possible
# Fall back to default order if radix not reflecting C locale: recompute with default
ord2 <- try(order(dv_list$TRTA, dv_list$SITE, dv_list$USUBJID, dv_list$START_DATE,
                  na.last = FALSE), silent = TRUE)
if (!inherits(ord2, "try-error")) ord <- ord2
dv_list <- dv_list[ord, , drop = FALSE]
try(Sys.setlocale("LC_COLLATE", old_loc), silent = TRUE)
row.names(dv_list) <- NULL

cat(paste0("dv_list rows: ", nrow(dv_list), "\n"))

# ---- Write CSV dataset ----
# Format dates for CSV per spec (here all listing cols are character; handle Date/POSIX if any)
csv_df <- dv_list
# Ensure column order as in PROC REPORT
csv_df <- csv_df[, c("TRTA","SITE","USUBJID","CATEGORY","SUBCATEGORY","DESCRIPTION","START_DATE","END_DATE","EPOCH","SEVERITY"), drop = FALSE]
# Format Date/POSIX columns if present (none expected, but handle)
for (nm in names(csv_df)) {
  x <- csv_df[[nm]]
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    csv_df[[nm]] <- format(x, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  } else if (inherits(x, "Date")) {
    csv_df[[nm]] <- format(x, "%Y-%m-%d")
  }
}
write.csv(csv_df, file.path(out, "l_dv.csv"), row.names = FALSE, na = "")
cat("Wrote l_dv.csv\n")

# ---- Text rendering (RTF counterpart) ----
# Titles from u_titles.sas for L_DV + footnotes from u_setup.sas
sponsor  <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate  <- "[DDMONYYYY]"
t4 <- "Listing 16.2.2"
t5 <- "Listing of Protocol Deviations"
t6 <- "All Subjects"
f1 <- paste0("Data cutoff: ", cutdate, ".")
f2 <- "Source: ADaM datasets"
f3 <- paste0("Program: l_dv.sas | Run: ", format(Sys.Date(), "%d%b%Y"))

col_labels <- c("Treatment","Site","Subject ID","Deviation Category","Sub-category","Description","Start Date","End Date","Epoch","Severity")
col_names  <- c("TRTA","SITE","USUBJID","CATEGORY","SUBCATEGORY","DESCRIPTION","START_DATE","END_DATE","EPOCH","SEVERITY")

# Build display matrix (character, "" for NA)
disp <- as.data.frame(lapply(csv_df, function(col) {
  v <- as.character(col)
  v[is.na(v)] <- ""
  v
}), stringsAsFactors = FALSE)

# Compute column widths for fixed-width txt (min = label length, max cap)
max_w <- c(18, 8, 12, 18, 14, 30, 11, 11, 12, 10)
# ensure at least label length
for (j in seq_along(col_labels)) {
  need <- nchar(col_labels[j])
  if (max_w[j] < need) max_w[j] <- need
  # expand if data longer up to cap, else truncate display
  vals <- disp[[j]]
  mw <- suppressWarnings(max(c(need, nchar(vals)), na.rm = TRUE))
  if (is.finite(mw) && mw > max_w[j]) {
    # allow wider for description etc up to cap already; keep cap
  }
}
truncate_to <- function(s, w) {
  # SAS-like: truncate with substr, keep full if <= w
  ifelse(nchar(s) <= w, s, substr(s, 1, w))
}
pad_to <- function(s, w) {
  sprintf(paste0("%-", w, "s"), s)
}

header_line <- paste(mapply(function(l, w) pad_to(l, w), col_labels, max_w), collapse = " | ")
sep_line    <- paste(mapply(function(w) paste(rep("-", w), collapse = ""), max_w), collapse = "-+-")

# Pagination: BREAK AFTER TRTA / PAGE -> each TRTA starts new page
trt_vals <- unique(disp$TRTA)
# Preserve sorted order of appearance
# Handle NA TRTA as separate group (missing sorts first, already first)
if (length(trt_vals) == 0) trt_vals <- character(0)
n_pages <- max(1, length(trt_vals))

txt_path <- file.path(out, "l_dv.txt")
con <- file(txt_path, open = "wt")
on.exit(close(con), add = FALSE)

write_page_header <- function(page_no, n_pages) {
  cat(paste0(sponsor, strrep(" ", 40), "Page ", page_no, " of ", n_pages, "\n"), file = con)
  cat(paste0("Protocol: ", protocol, "\n"), file = con)
  cat(" \n", file = con)
  cat(paste0(t4, "\n"), file = con)
  cat(paste0(t5, "\n"), file = con)
  cat(paste0(t6, "\n"), file = con)
  cat(" \n", file = con)
  cat(paste0("Population: All Randomized Subjects (RANDFL=Y)   N=", nrow(disp), " deviations\n"), file = con)
  cat(" \n", file = con)
}

write_footer <- function() {
  cat(" \n", file = con)
  cat(paste0(f1, "\n"), file = con)
  cat(paste0(f2, "\n"), file = con)
  cat(paste0(f3, "\n"), file = con)
}

if (nrow(disp) == 0) {
  write_page_header(1, 1)
  cat(header_line, "\n", file = con, sep = "")
  cat(sep_line, "\n", file = con, sep = "")
  cat("No protocol deviations reported for randomized subjects.\n", file = con)
  write_footer()
} else {
  pg <- 0
  for (ti in seq_along(trt_vals)) {
    pg <- pg + 1
    trt <- trt_vals[ti]
    trt_disp <- ifelse(is.na(trt) || trt == "", "(Missing)", trt)
    write_page_header(pg, n_pages)
    cat(paste0("Treatment: ", trt_disp, "\n"), file = con)
    cat(" \n", file = con)
    cat(header_line, "\n", file = con, sep = "")
    cat(sep_line, "\n", file = con, sep = "")
    sub <- disp[disp$TRTA == trt | (is.na(disp$TRTA) & is.na(trt)), , drop = FALSE]
    # If TRTA has "" vs NA mismatch, also match empty
    if (trt == "") {
      sub <- disp[disp$TRTA == "" | is.na(disp$TRTA), , drop = FALSE]
    }
    for (r in seq_len(nrow(sub))) {
      rowv <- sapply(seq_along(col_names), function(j) {
        s <- as.character(sub[r, col_names[j]])
        if (is.na(s)) s <- ""
        s <- truncate_to(s, max_w[j])
        pad_to(s, max_w[j])
      })
      cat(paste(rowv, collapse = " | "), "\n", file = con, sep = "")
    }
    cat(paste0(" \nNumber of deviations in this treatment: ", nrow(sub), "\n"), file = con)
    write_footer()
    if (ti < length(trt_vals)) cat("\f\n", file = con)
  }
}
close(con)

cat("Wrote l_dv.txt\n")
cat("NOTE: l_dv.sas completed successfully.\n")

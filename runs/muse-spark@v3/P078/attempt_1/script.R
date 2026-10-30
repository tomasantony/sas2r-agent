# P078_l_dv.sas -> R translation
# Listing 16.2.2 - Listing of Protocol Deviations
# Inputs: sdtm.dv, adam.adsl | Output: l_dv (listing)

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult + 0.5 + 1e-10
  trunc(z) / mult * sign_x
}

find_dataset <- function(dir, name) {
  # case-insensitive search for name.sas7bdat / .xpt / .sas7bdat etc
  files <- list.files(dir, full.names = TRUE, ignore.case = FALSE)
  # match basename without extension equal to name (case-insensitive)
  bn <- tools::file_path_sans_ext(basename(files))
  ext <- tolower(tools::file_ext(files))
  idx <- which(tolower(bn) == tolower(name) & ext %in% c("sas7bdat", "xpt", "sas7bdat"))
  if (length(idx) == 0) {
    # try list with ignore case pattern
    pat <- paste0("^", name, "\\.(sas7bdat|xpt)$")
    files2 <- list.files(dir, pattern = pat, full.names = TRUE, ignore.case = TRUE)
    if (length(files2) == 0) stop(paste0("Dataset not found: ", dir, "/", name))
    return(files2[1])
  }
  return(files[idx[1]])
}

read_dataset <- function(path) {
  e <- tolower(tools::file_ext(path))
  if (e == "xpt") {
    df <- haven::read_xpt(path)
  } else {
    df <- haven::read_sas(path)
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  # convert haven_labelled to plain
  for (nm in names(df)) {
    if (inherits(df[[nm]], "haven_labelled")) {
      df[[nm]] <- haven::as_factor(df[[nm]], levels = "labels")
      df[[nm]] <- as.character(df[[nm]])
    } else if (inherits(df[[nm]], "POSIXt")) {
      # keep as character ISO datetime
      df[[nm]] <- format(df[[nm]], "%Y-%m-%dT%H:%M:%S")
    } else if (inherits(df[[nm]], "Date")) {
      df[[nm]] <- format(df[[nm]], "%Y-%m-%d")
    }
  }
  df
}

adsl_path <- find_dataset(adam_dir, "adsl")
dv_path   <- find_dataset(sdtm_dir, "dv")

adsl <- read_dataset(adsl_path)
dv   <- read_dataset(dv_path)

cat("NOTE: ADSL rows:", nrow(adsl), " DV rows:", nrow(dv), "\n")

stopifnot("USUBJID" %in% names(adsl))
stopifnot("USUBJID" %in% names(dv))
stopifnot("RANDFL" %in% names(adsl))
stopifnot("TRTA" %in% names(adsl))
stopifnot("SITEID" %in% names(adsl))

# Required DV columns (allow missing -> create as NA character)
need_dv <- c("DVCAT", "DVSCAT", "DVTERM", "DVSTDTC", "DVENDTC", "EPOCH", "DVSEV")
for (c in need_dv) {
  if (!c %in% names(dv)) {
    dv[[c]] <- NA_character_
  }
}
# Ensure character type for join/display columns
to_char <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%dT%H:%M:%S"))
  if (is.numeric(x)) return(as.character(x))
  as.character(x)
}
adsl$USUBJID <- to_char(adsl$USUBJID)
adsl$TRTA    <- to_char(adsl$TRTA)
adsl$SITEID  <- to_char(adsl$SITEID)
adsl$RANDFL  <- to_char(adsl$RANDFL)
dv$USUBJID   <- to_char(dv$USUBJID)
for (c in need_dv) dv[[c]] <- to_char(dv[[c]])

#--- Step 1: Prepare data (PROC SQL) ---
# Before merging, drop/rename every column present in both except keys
# Keys: USUBJID. Keep only needed columns to avoid .x/.y suffixes.
adsl_sub <- adsl[, c("USUBJID", "TRTA", "SITEID", "RANDFL"), drop = FALSE]
dv_sub   <- dv[, c("USUBJID", need_dv), drop = FALSE]

# where b.RANDFL = 'Y' (SAS case-sensitive, trailing blanks ignored -> trim)
adsl_sub$RANDFL <- trimws(adsl_sub$RANDFL)
adsl_rand <- adsl_sub[adsl_sub$RANDFL == "Y" & !is.na(adsl_sub$RANDFL), , drop = FALSE]
stopifnot("RANDFL" %in% names(adsl_rand))
stopifnot(nrow(adsl_rand) > 0)

# inner join on USUBJID
dv_list <- merge(dv_sub, adsl_rand, by = "USUBJID", all = FALSE)
stopifnot(nrow(dv_list) > 0)
stopifnot(all(c("USUBJID","TRTA","SITEID") %in% names(dv_list)))

# select / rename to report variables
# a.USUBJID, b.TRTA, b.SITEID as SITE, a.DVCAT as CATEGORY, a.DVSCAT as SUBCATEGORY,
# a.DVTERM as DESCRIPTION, a.DVSTDTC as START_DATE, a.DVENDTC as END_DATE, a.EPOCH, a.DVSEV as SEVERITY
dv_list$CATEGORY    <- dv_list$DVCAT
dv_list$SUBCATEGORY <- dv_list$DVSCAT
dv_list$DESCRIPTION <- dv_list$DVTERM
dv_list$START_DATE  <- dv_list$DVSTDTC
dv_list$END_DATE    <- dv_list$DVENDTC
# EPOCH stays
dv_list$SEVERITY    <- dv_list$DVSEV
dv_list$SITE        <- dv_list$SITEID

dv_list <- dv_list[, c("USUBJID","TRTA","SITE","CATEGORY","SUBCATEGORY","DESCRIPTION",
                       "START_DATE","END_DATE","EPOCH","SEVERITY"), drop = FALSE]

# order by b.TRTA, b.SITEID, a.USUBJID, a.DVSTDTC (SAS: char uppercase before lowercase, missing first)
# Emulate SAS binary collation via LC_COLLATE=C
old_loc <- Sys.getlocale("LC_COLLATE")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
ord <- order(dv_list$TRTA, dv_list$SITE, dv_list$USUBJID, dv_list$START_DATE,
             na.last = FALSE, method = "shell")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_loc), silent = TRUE))
dv_list <- dv_list[ord, , drop = FALSE]
rownames(dv_list) <- NULL

# Report column order
rep_cols <- c("TRTA","SITE","USUBJID","CATEGORY","SUBCATEGORY","DESCRIPTION",
              "START_DATE","END_DATE","EPOCH","SEVERITY")
dv_rep <- dv_list[, rep_cols, drop = FALSE]

# Write dataset CSV (full values, ISO dates already)
# Ensure dates as YYYY-MM-DD / datetimes as YYYY-MM-DDTHH:MM:SS (already ISO strings)
write.csv(dv_rep, file.path(out, "l_dv.csv"), row.names = FALSE, na = "")

# Also write work dataset counterpart for traceability
write.csv(dv_rep, file.path(out, "dv_list.csv"), row.names = FALSE, na = "")

#--- Step 2: Generate listing text rendering (PROC REPORT) ---
# Titles / footnotes (u_titles.sas, output=L_DV)
sponsor  <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate  <- "[DDMONYYYY]"
run_date <- toupper(format(Sys.Date(), "%d%b%Y"))
run_time <- format(Sys.time(), "%H:%M")
progname <- "P078_l_dv.sas"

title2 <- paste0("Protocol: ", protocol)
title4 <- "Listing 16.2.2"
title5 <- "Listing of Protocol Deviations"
title6 <- "All Subjects"
foot1 <- paste0("Data cutoff: ", cutdate)
foot2 <- "Source: ADaM datasets"
foot3 <- paste0("Program: ", progname, " | Run: ", run_date, " ", run_time)

# Column widths approximating SAS style widths (total ~132 chars landscape)
# TRTA 0.8in, SITE 0.5in, USUBJID 0.8in, CATEGORY 1.2in, SUBCATEGORY 1.0in,
# DESCRIPTION 2.0in, START_DATE 0.7in, END_DATE 0.7in, EPOCH 0.8in, SEVERITY 0.6in
cw <- c(TRTA = 12, SITE = 7, USUBJID = 12, CATEGORY = 17, SUBCATEGORY = 14,
        DESCRIPTION = 29, START_DATE = 10, END_DATE = 10, EPOCH = 12, SEVERITY = 9)

pad_trunc <- function(x, w) {
  if (is.na(x) || is.null(x)) x <- ""
  x <- as.character(x)
  x <- gsub("\n|\r|\t", " ", x)
  if (nchar(x, type = "chars") > w) x <- substr(x, 1, w)
  sprintf(paste0("%-", w, "s"), x)
}

# Header two lines due to split='~'
h1 <- c(TRTA = "Treatment", SITE = "Site", USUBJID = "Subject", CATEGORY = "Deviation",
        SUBCATEGORY = "Sub-", DESCRIPTION = "Description", START_DATE = "Start",
        END_DATE = "End", EPOCH = "Epoch", SEVERITY = "Severity")
h2 <- c(TRTA = "", SITE = "", USUBJID = "ID", CATEGORY = "Category",
        SUBCATEGORY = "category", DESCRIPTION = "", START_DATE = "Date",
        END_DATE = "Date", EPOCH = "", SEVERITY = "")

header_line1 <- paste(mapply(pad_trunc, h1[rep_cols], cw[rep_cols]), collapse = " ")
header_line2 <- paste(mapply(pad_trunc, h2[rep_cols], cw[rep_cols]), collapse = " ")
dash_line    <- paste(mapply(function(w) paste(rep("-", w), collapse = ""), cw[rep_cols]), collapse = " ")
total_width  <- nchar(dash_line, type = "chars")

center_text <- function(s, w) {
  s <- as.character(s)
  n <- nchar(s, type = "chars")
  if (n >= w) return(substr(s, 1, w))
  left <- floor((w - n) / 2)
  paste0(paste(rep(" ", left), collapse = ""), s)
}

# Pagination: break after TRTA / page + overflow pages (40 rows per page)
rows_per_page <- 40
# Build pages: each TRTA starts new page
trta_vals <- unique(dv_rep$TRTA)
# Handle NA TRTA as separate group (missing sorts first, already ordered)
pages <- list()
for (tv in trta_vals) {
  if (is.na(tv)) {
    idx <- which(is.na(dv_rep$TRTA))
  } else {
    idx <- which(!is.na(dv_rep$TRTA) & dv_rep$TRTA == tv)
  }
  # chunk into pages
  n <- length(idx)
  start <- 1
  while (start <= n) {
    end <- min(n, start + rows_per_page - 1)
    pages[[length(pages) + 1]] <- list(trta = tv, rows = idx[start:end])
    start <- end + 1
  }
}
if (length(pages) == 0) pages[[1]] <- list(trta = NA, rows = integer(0))
npages <- length(pages)

# Display suppression for ORDER vars TRTA, SITE, USUBJID (hierarchical)
# Precompute display values with blanking of repeats within consecutive rows
disp_TRTA <- dv_rep$TRTA
disp_SITE <- dv_rep$SITE
disp_USUBJID <- dv_rep$USUBJID
for (i in seq_len(nrow(dv_rep))) {
  if (i == 1) next
  prev <- i - 1
  same_trta <- (!is.na(dv_rep$TRTA[i]) && !is.na(dv_rep$TRTA[prev]) && dv_rep$TRTA[i] == dv_rep$TRTA[prev]) ||
               (is.na(dv_rep$TRTA[i]) && is.na(dv_rep$TRTA[prev]))
  same_site <- (!is.na(dv_rep$SITE[i]) && !is.na(dv_rep$SITE[prev]) && dv_rep$SITE[i] == dv_rep$SITE[prev]) ||
               (is.na(dv_rep$SITE[i]) && is.na(dv_rep$SITE[prev]))
  same_subj <- (!is.na(dv_rep$USUBJID[i]) && !is.na(dv_rep$USUBJID[prev]) && dv_rep$USUBJID[i] == dv_rep$USUBJID[prev]) ||
               (is.na(dv_rep$USUBJID[i]) && is.na(dv_rep$USUBJID[prev]))
  if (same_trta) {
    disp_TRTA[i] <- ""
  }
  if (same_trta && same_site) {
    disp_SITE[i] <- ""
  }
  if (same_trta && same_site && same_subj) {
    disp_USUBJID[i] <- ""
  }
  # missing values display as blank
  if (is.na(disp_TRTA[i])) disp_TRTA[i] <- ""
  if (is.na(disp_SITE[i])) disp_SITE[i] <- ""
  if (is.na(disp_USUBJID[i])) disp_USUBJID[i] <- ""
}
disp_TRTA[is.na(disp_TRTA)] <- ""
disp_SITE[is.na(disp_SITE)] <- ""
disp_USUBJID[is.na(disp_USUBJID)] <- ""

txt_path <- file.path(out, "l_dv.txt")
con <- file(txt_path, open = "wt")

for (pg in seq_along(pages)) {
  pg_rows <- pages[[pg]]$rows
  # Title block
  # title1 j=l sponsor j=r Page X of Y
  page_lbl <- paste0("Page ", pg, " of ", npages)
  # left + right on one line
  sp <- total_width - nchar(sponsor, type = "chars") - nchar(page_lbl, type = "chars")
  if (sp < 2) sp <- 2
  line_t1 <- paste0(sponsor, paste(rep(" ", sp), collapse = ""), page_lbl)
  cat(line_t1, "\n", file = con, sep = "")
  cat(paste0(title2, "\n"), file = con, sep = "")
  cat("\n", file = con, sep = "")
  cat(paste0(center_text(title4, total_width), "\n"), file = con, sep = "")
  cat(paste0(center_text(title5, total_width), "\n"), file = con, sep = "")
  cat(paste0(center_text(title6, total_width), "\n"), file = con, sep = "")
  cat("\n", file = con, sep = "")
  cat(header_line1, "\n", file = con, sep = "")
  cat(header_line2, "\n", file = con, sep = "")
  cat(dash_line, "\n", file = con, sep = "")
  for (r in pg_rows) {
    vals <- list(
      TRTA = disp_TRTA[r],
      SITE = disp_SITE[r],
      USUBJID = disp_USUBJID[r],
      CATEGORY = ifelse(is.na(dv_rep$CATEGORY[r]), "", dv_rep$CATEGORY[r]),
      SUBCATEGORY = ifelse(is.na(dv_rep$SUBCATEGORY[r]), "", dv_rep$SUBCATEGORY[r]),
      DESCRIPTION = ifelse(is.na(dv_rep$DESCRIPTION[r]), "", dv_rep$DESCRIPTION[r]),
      START_DATE = ifelse(is.na(dv_rep$START_DATE[r]), "", dv_rep$START_DATE[r]),
      END_DATE = ifelse(is.na(dv_rep$END_DATE[r]), "", dv_rep$END_DATE[r]),
      EPOCH = ifelse(is.na(dv_rep$EPOCH[r]), "", dv_rep$EPOCH[r]),
      SEVERITY = ifelse(is.na(dv_rep$SEVERITY[r]), "", dv_rep$SEVERITY[r])
    )
    line <- paste(mapply(pad_trunc, vals[rep_cols], cw[rep_cols]), collapse = " ")
    cat(line, "\n", file = con, sep = "")
  }
  cat("\n", file = con, sep = "")
  cat(paste0(foot1, "\n"), file = con, sep = "")
  cat(paste0(foot2, "\n"), file = con, sep = "")
  cat(paste0(foot3, "\n"), file = con, sep = "")
  if (pg < npages) cat("\n\f\n", file = con, sep = "")
}
close(con)

cat("NOTE: l_dv.sas completed successfully.\n")
cat(paste0("NOTE: Wrote ", file.path(out, "l_dv.csv"), " rows=", nrow(dv_rep), "\n"))
cat(paste0("NOTE: Wrote ", txt_path, " pages=", npages, "\n"))

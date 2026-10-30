# P039 - Supp2Par_v1: merge SUPPAE onto AE -> AEFULL
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
sdtm <- "/bench/data/raw/cdisc-pilot/sdtm"

# Look for a dataset under its lower-case or upper-case name, as XPT or sas7bdat.
read_ds <- function(name) {
  cands <- c(file.path(sdtm, paste0(tolower(name), ".xpt")),
             file.path(sdtm, paste0(toupper(name), ".xpt")),
             file.path(sdtm, paste0(tolower(name), ".sas7bdat")))
  for (f in cands) if (file.exists(f)) {
    d <- if (grepl("xpt$", f)) read_xpt(f) else read_sas(f)
    d <- zap_labels(zap_formats(as.data.frame(d)))
    return(d)
  }
  NULL
}

parent_name <- "AE"
parent <- read_ds("ae")
supp   <- read_ds("suppae")
if (is.null(parent)) stop("Parent dataset AE not found")

# Helpers: SAS-like ordering and character-missing checks
sas_order <- function(df, keys) {
  if (nrow(df) == 0) return(df)
  df[do.call(order, c(unname(as.list(df[keys])),
                      list(method = "radix", na.last = FALSE))), , drop = FALSE]
}
blank <- function(x) is.na(x) | trimws(as.character(x)) == ""

copy_parent <- FALSE
if (is.null(supp) || nrow(supp) == 0) copy_parent <- TRUE
if (!copy_parent) {
  names(supp) <- toupper(names(supp))
  if (sum(supp$RDOMAIN == toupper(parent_name), na.rm = TRUE) == 0) copy_parent <- TRUE
}

if (copy_parent) {
  aefull <- parent
} else {
  # Records with empty RDOMAIN/USUBJID/QNAM are ignored
  acc <- supp[!(blank(supp$RDOMAIN) | blank(supp$USUBJID) | blank(supp$QNAM)), , drop = FALSE]
  for (v in c("IDVAR", "IDVARVAL", "QNAM", "QVAL", "QLABEL")) {
    acc[[v]] <- as.character(acc[[v]])
    acc[[v]][is.na(acc[[v]])] <- ""
  }

  # Duplicate check on USUBJID RDOMAIN IDVAR IDVARVAL QNAM
  if (any(duplicated(acc[c("USUBJID", "RDOMAIN", "IDVAR", "IDVARVAL", "QNAM")])))
    stop("SUPP not unique on keys - macro terminates (RC=10)")

  # Same QNAM with different labels
  fq <- unique(data.frame(QNAM = toupper(acc$QNAM), QLABEL = acc$QLABEL))
  if (any(duplicated(fq$QNAM))) stop("Same QNAM has different labels (RC=7)")

  # Empty IDVAR and IDVARVAL -> USUBJID
  e <- acc$IDVAR == "" & acc$IDVARVAL == ""
  acc$IDVAR[e] <- "USUBJID"
  acc$IDVARVAL[e] <- acc$USUBJID[e]
  if (any(acc$IDVAR == "" & acc$IDVARVAL != "")) stop("IDVAR empty while IDVARVAL not empty (RC=8)")

  # A QNAM that already exists as a parent variable gets the suffix _2
  clash <- intersect(names(parent), unique(acc$QNAM))
  for (nm in clash) acc$QNAM[acc$QNAM == nm] <- paste0(nm, "_2")

  # IDVAR values in proc freq order (sorted)
  sub <- acc[acc$RDOMAIN == toupper(parent_name), , drop = FALSE]
  ids <- sort(unique(sub$IDVAR), method = "radix")
  miss <- setdiff(ids, names(parent))
  if (length(miss) > 0) stop("IDVAR(s) not present on parent (RC=9)")

  seqvar <- paste0(parent_name, "SEQ")
  has_seq <- seqvar %in% names(parent)
  fkeys <- if (has_seq) c("STUDYID", "DOMAIN", "USUBJID", seqvar) else c("STUDYID", "DOMAIN", "USUBJID")

  steps <- list()
  for (i in seq_along(ids)) {
    id <- ids[i]
    idcln <- if (id != "USUBJID") id else character(0)
    s <- sub[sub$IDVAR == id, , drop = FALSE]
    if (id != "USUBJID") {
      if (is.numeric(parent[[id]])) {
        s[[id]] <- suppressWarnings(as.numeric(trimws(s$IDVARVAL)))
      } else {
        s[[id]] <- s$IDVARVAL
      }
    }
    s$DOMAIN <- s$RDOMAIN
    bykeys <- c("STUDYID", "DOMAIN", "USUBJID", idcln)
    s <- sas_order(s, c("STUDYID", "RDOMAIN", "USUBJID", idcln))

    # Transpose: QNAM values become columns in order of first appearance
    qn <- unique(s$QNAM)
    keydf <- unique(s[bykeys])
    tr <- keydf
    for (q in qn) {
      sq <- s[s$QNAM == q, c(bykeys, "QVAL"), drop = FALSE]
      names(sq)[ncol(sq)] <- q
      tr <- left_join(tr, sq, by = bykeys)
    }

    psort <- sas_order(parent, bykeys)
    psort$.ord <- seq_len(nrow(psort))
    st <- left_join(psort, tr, by = bykeys)
    st <- st[order(st$.ord), , drop = FALSE]
    st$.ord <- NULL
    for (q in qn) { x <- st[[q]]; x[is.na(x)] <- ""; st[[q]] <- x }
    steps[[i]] <- sas_order(st, fkeys)
  }

  # Merge the per-IDVAR steps (row-aligned on the same keys; later values overwrite)
  aefull <- steps[[1]]
  if (length(steps) > 1) {
    for (i in 2:length(steps)) {
      st <- steps[[i]]
      for (v in setdiff(names(st), fkeys)) aefull[[v]] <- st[[v]]
    }
  }
  rownames(aefull) <- NULL
}

# Format any Date/datetime columns
for (v in names(aefull)) {
  if (inherits(aefull[[v]], "Date")) aefull[[v]] <- format(aefull[[v]], "%Y-%m-%d")
  if (inherits(aefull[[v]], "POSIXt")) aefull[[v]] <- format(aefull[[v]], "%Y-%m-%dT%H:%M:%S")
}

write.csv(aefull, file.path(out, "aefull.csv"), row.names = FALSE, na = "")
cat("AEFULL written:", nrow(aefull), "rows,", ncol(aefull), "columns\n")

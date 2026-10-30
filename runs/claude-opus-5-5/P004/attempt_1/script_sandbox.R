suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
project_path <- "/bench/data/derived/c2c"

read_lib <- function(dir, name) {
  f1 <- file.path(dir, paste0(name, ".sas7bdat"))
  f2 <- file.path(dir, paste0(name, ".xpt"))
  if (file.exists(f1)) {
    d <- haven::read_sas(f1)
  } else if (file.exists(f2)) {
    d <- haven::read_xpt(f2)
  } else {
    stop("Cannot find dataset ", name, " in ", dir)
  }
  d <- haven::zap_labels(haven::zap_formats(d))
  names(d) <- toupper(names(d))
  as.data.frame(d, stringsAsFactors = FALSE)
}

adsl <- read_lib(file.path(project_path, "data/adam"), "adsl")
ae   <- read_lib(file.path(project_path, "data/sdtm"), "ae")
cat("Read ADSL:", nrow(adsl), "rows; AE:", nrow(ae), "rows\n")

# Character NA -> "" (SAS blanks)
blankify <- function(d) {
  for (v in names(d)) if (is.character(d[[v]])) d[[v]][is.na(d[[v]])] <- ""
  d
}
adsl <- blankify(adsl)
ae   <- blankify(ae)

# Ensure date columns are Date
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1960-01-01"))
  as.Date(x)
}
if ("TRTSDT" %in% names(adsl)) adsl$TRTSDT <- to_date(adsl$TRTSDT)
if ("TRTEDT" %in% names(adsl)) adsl$TRTEDT <- to_date(adsl$TRTEDT)

# 1. Sort
adsl_sorted <- adsl[order(adsl$USUBJID, method = "radix"), , drop = FALSE]
ae_sorted <- ae[order(ae$USUBJID, ae$AESEQ, method = "radix", na.last = FALSE), , drop = FALSE]

# 2. Merge (if b) - common variables take AE values
common <- setdiff(intersect(names(adsl_sorted), names(ae_sorted)), "USUBJID")
adsl_m <- adsl_sorted[, setdiff(names(adsl_sorted), common), drop = FALSE]
adsl_m <- adsl_m[!duplicated(adsl_m$USUBJID), , drop = FALSE]
ae_sorted$.ord <- seq_len(nrow(ae_sorted))
draft <- left_join(ae_sorted, adsl_m, by = "USUBJID")
draft <- draft[order(draft$.ord), , drop = FALSE]
draft$.ord <- NULL

sas_len <- function(x) { n <- nchar(sub("\\s+$", "", x)); n[n == 0] <- 1; n }

draft$AESTDT <- as.Date(NA)
draft$AEENDT <- as.Date(NA)
i1 <- sas_len(draft$AESTDTC) >= 10
draft$AESTDT[i1] <- as.Date(substr(draft$AESTDTC[i1], 1, 10), format = "%Y-%m-%d")
i2 <- sas_len(draft$AEENDTC) >= 10
draft$AEENDT[i2] <- as.Date(substr(draft$AEENDTC[i2], 1, 10), format = "%Y-%m-%d")

ok <- !is.na(draft$AESTDT) & !is.na(draft$TRTSDT)
diffd <- as.numeric(draft$AESTDT - draft$TRTSDT)
draft$ASTDY <- ifelse(ok, ifelse(diffd >= 0, diffd + 1, diffd), NA_real_)

saffl <- if ("SAFFL" %in% names(draft)) draft$SAFFL else rep("", nrow(draft))
saffl[is.na(saffl)] <- ""
draft$TRTEMFL <- ifelse(saffl == "Y" & ok & diffd >= 0, "Y", "")
draft$TRTEMFL[is.na(draft$TRTEMFL)] <- ""

# Variable order: STUDYID USUBJID, then PDV order (ADSL vars, AE vars, new)
keepvars <- c("STUDYID","USUBJID","SUBJID","TRT01A","TRTSDT","TRTEDT","AGE","SEX",
              "AESEQ","AETERM","AEDECOD","AESEV","AEREL","AEOUT","AESER",
              "AESTDT","AEENDT","ASTDY","TRTEMFL")
pdv <- unique(c("STUDYID","USUBJID", names(adsl_sorted), names(ae_sorted),
                "AESTDT","AEENDT","ASTDY","TRTEMFL"))
ordvars <- pdv[pdv %in% keepvars & pdv %in% names(draft)]
adae_draft <- draft[, ordvars, drop = FALSE]

# 3. First TE occurrence
adae_draft$.occ <- ifelse(adae_draft$TRTEMFL == "Y", 0, 1)
o <- order(adae_draft$USUBJID, adae_draft$AEDECOD, adae_draft$.occ,
           as.numeric(adae_draft$AESTDT), adae_draft$AESEQ,
           method = "radix", na.last = FALSE)
occ <- adae_draft[o, , drop = FALSE]
key <- paste(occ$USUBJID, occ$AEDECOD, occ$.occ, sep = "\r")
first_occ <- !duplicated(key)
occ$AOCCFL <- ifelse(occ$TRTEMFL == "Y" & first_occ, "Y", "")
occ$.occ <- NULL

# 4. Final sort
o2 <- order(occ$USUBJID, as.numeric(occ$AESTDT), occ$AESEQ,
            method = "radix", na.last = FALSE)
adae <- occ[o2, , drop = FALSE]
rownames(adae) <- NULL

# Write dataset
adae_out <- adae
for (v in names(adae_out)) if (inherits(adae_out[[v]], "Date"))
  adae_out[[v]] <- format(adae_out[[v]], "%Y-%m-%d")
write.csv(adae_out, file.path(out, "adae.csv"), row.names = FALSE, na = "")
cat("Wrote adae.csv:", nrow(adae), "rows\n")

# 5. Listing (first 15 obs)
date9 <- function(d) ifelse(is.na(d), ".", toupper(format(d, "%d%b%Y")))
lst <- head(adae, 15)
pv <- c("USUBJID","TRT01A","AETERM","AESEV","AESTDT","TRTSDT","ASTDY","TRTEMFL","AOCCFL")
pv <- pv[pv %in% names(lst)]
tab <- data.frame(Obs = seq_len(nrow(lst)), stringsAsFactors = FALSE)
for (v in pv) {
  x <- lst[[v]]
  if (inherits(x, "Date")) x <- date9(x)
  else if (is.numeric(x)) x <- ifelse(is.na(x), ".", format(x, trim = TRUE))
  tab[[v]] <- x
}
write.csv(tab, file.path(out, "P004.csv"), row.names = FALSE, na = "")

fmt <- as.data.frame(lapply(tab, as.character), stringsAsFactors = FALSE)
w <- sapply(names(fmt), function(n) max(nchar(n), nchar(fmt[[n]])))
lines <- c("ADAE Audit - Treatment-Emergent and First-Occurrence Flags", "",
           paste(mapply(function(n, wd) formatC(n, width = wd), names(fmt), w), collapse = "  "))
for (i in seq_len(nrow(fmt))) {
  lines <- c(lines, paste(mapply(function(n, wd) formatC(fmt[[n]][i], width = wd),
                                 names(fmt), w), collapse = "  "))
}
writeLines(lines, file.path(out, "P004.txt"))
cat("Wrote P004.txt / P004.csv\n")

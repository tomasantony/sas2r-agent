# P002 - ADAE derivation (CDISC pilot) translated from SAS
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawsdtm <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

cat("Reading inputs\n")
ae_in   <- haven::read_xpt(file.path(rawsdtm, "ae.xpt"))
adsl_in <- haven::read_xpt(file.path(rawadam, "adsl.xpt"))
names(ae_in)   <- toupper(names(ae_in))
names(adsl_in) <- toupper(names(adsl_in))

# Drop haven labels/formats for plain data handling
strip_attrs <- function(df) {
  for (v in names(df)) {
    x <- df[[v]]
    attr(x, "label") <- NULL
    attr(x, "format.sas") <- NULL
    attr(x, "display_width") <- NULL
    df[[v]] <- x
  }
  df
}
ae_in   <- strip_attrs(ae_in)
adsl_in <- strip_attrs(adsl_in)

chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
for (v in names(ae_in)) if (is.character(ae_in[[v]])) ae_in[[v]] <- chr(ae_in[[v]])
for (v in names(adsl_in)) if (is.character(adsl_in[[v]])) adsl_in[[v]] <- chr(adsl_in[[v]])

# SAS input(strip(x), ?? yymmdd10.)
sas_yymmdd10 <- function(x) {
  s <- substr(trimws(chr(x)), 1, 10)
  ok <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", s)
  res <- rep(as.Date(NA), length(s))
  res[ok] <- as.Date(s[ok], format = "%Y-%m-%d", optional = TRUE)
  res
}
sas_length <- function(x) {
  n <- nchar(sub("\\s+$", "", chr(x)))
  n[n == 0] <- 1L
  n
}

# ---- proc sort AE by USUBJID AESEQ; data step ----
ae <- ae_in[order(ae_in$USUBJID, ae_in$AESEQ, method = "radix"), ]
ae$ASTDT <- sas_yymmdd10(ae$AESTDTC)
ae$AENDT <- sas_yymmdd10(ae$AEENDTC)
ae$ADUR  <- ifelse(!is.na(ae$ASTDT) & !is.na(ae$AENDT),
                   as.numeric(ae$AENDT - ae$ASTDT) + 1, NA_real_)

# ---- ADSL subset ----
temp <- adsl_in[order(adsl_in$USUBJID, method = "radix"),
                c("SITEID", "USUBJID", "TRT01A", "TRT01AN", "AGE", "AGEGR1", "AGEGR1N",
                  "RACE", "RACEN", "SEX", "SAFFL", "TRTSDT", "TRTEDT")]
temp <- dplyr::rename(temp, TRTA = TRT01A, TRTAN = TRT01AN)
temp$TRTSDT <- as.Date(temp$TRTSDT)
temp$TRTEDT <- as.Date(temp$TRTEDT)

# ---- merge (if inAE) ----
ae$.ord <- seq_len(nrow(ae))
ae <- dplyr::left_join(ae, temp, by = "USUBJID")
ae <- ae[order(ae$.ord), ]
ae$.ord <- NULL

ae$ASTDTF <- ""
imp <- is.na(ae$ASTDT) & sas_length(ae$AESTDTC) == 7
if (any(imp)) {
  ae$ASTDT[imp] <- as.Date(paste0(gsub("\\s", "", ae$AESTDTC[imp]), "-01"), format = "%Y-%m-%d")
  ae$ASTDTF[imp] <- "D"
}

d_ast <- as.numeric(ae$ASTDT - ae$TRTSDT)
d_aen <- as.numeric(ae$AENDT - ae$TRTSDT)
has_trt <- !is.na(ae$TRTSDT)
ae$ASTDY <- ifelse(has_trt & !is.na(ae$ASTDT),
                   ifelse(d_ast >= 0, d_ast + 1, d_ast), NA_real_)
ae$AENDY <- ifelse(has_trt & !is.na(ae$AENDT),
                   ifelse(d_aen >= 0, d_aen + 1, d_aen), NA_real_)
ae$TRTEMFL <- ifelse(has_trt & !is.na(ae$ASTDT) & d_ast >= 0, "Y", "N")

dec <- ae$AEDECOD
cq <- grepl("APPLICATION", dec, fixed = TRUE) |
      grepl("DERMATITIS", dec, fixed = TRUE) |
      grepl("ERYTHEMA", dec, fixed = TRUE) |
      grepl("BLISTER", dec, fixed = TRUE) |
      (ae$AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
         !(dec %in% c("COLD SWEAT", "HYPERHIDROSIS", "ALOPECIA")))
ae$CQ01NAM <- ifelse(cq, "DERMATOLOGIC EVENTS", "")

# ---- flagfrst macro ----
flagfrst <- function(inds, sortvars = character(0), invar = "", outvar, ser = FALSE, derm = FALSE) {
  keep <- inds$TRTEMFL == "Y"
  if (invar != "") keep <- keep & inds[[invar]] != ""
  if (ser) keep <- keep & inds$AESER == "Y"
  if (derm) keep <- keep & inds$CQ01NAM == "DERMATOLOGIC EVENTS"
  ds01 <- inds[keep, ]
  bykeys <- c(list(ds01$USUBJID), lapply(sortvars, function(v) ds01[[v]]),
              list(as.numeric(ds01$ASTDT)))
  o <- do.call(order, c(bykeys, list(method = "radix", na.last = FALSE)))
  ds01 <- ds01[o, ]
  grpvars <- if (invar == "") "USUBJID" else c("USUBJID", sortvars)
  key <- do.call(paste, c(lapply(grpvars, function(v) ds01[[v]]), sep = "\r"))
  first <- !duplicated(key)
  ds02 <- data.frame(USUBJID = ds01$USUBJID[first], AESEQ = ds01$AESEQ[first],
                     stringsAsFactors = FALSE)
  ds02[[outvar]] <- rep("Y", nrow(ds02))
  ds02[order(ds02$USUBJID, ds02$AESEQ, method = "radix"), ]
}

aea  <- flagfrst(ae, character(0), "", "AOCCFL")
aeb  <- flagfrst(ae, "AEBODSYS", "AEBODSYS", "AOCCSFL")
aec  <- flagfrst(ae, c("AEBODSYS", "AEDECOD"), "AEDECOD", "AOCCPFL")
saea <- flagfrst(ae, character(0), "", "AOCC02FL", ser = TRUE)
saeb <- flagfrst(ae, "AEBODSYS", "AEBODSYS", "AOCC03FL", ser = TRUE)
saec <- flagfrst(ae, c("AEBODSYS", "AEDECOD"), "AEDECOD", "AOCC04FL", ser = TRUE)
derm <- flagfrst(ae, character(0), "", "AOCC01FL", derm = TRUE)

# ---- merge flags ----
ae <- ae[order(ae$USUBJID, ae$AESEQ, method = "radix"), ]
ae$.ord <- seq_len(nrow(ae))
for (f in list(aea, aeb, aec, saea, saeb, saec, derm)) {
  ae <- dplyr::left_join(ae, f, by = c("USUBJID", "AESEQ"))
}
ae <- ae[order(ae$.ord), ]
ae$.ord <- NULL
for (v in c("AOCCFL", "AOCCSFL", "AOCCPFL", "AOCC02FL", "AOCC03FL", "AOCC04FL", "AOCC01FL")) {
  ae[[v]][is.na(ae[[v]])] <- ""
}

# ---- keep & reorder ----
retain_order <- c("STUDYID", "SITEID", "USUBJID", "TRTA", "TRTAN", "AGE", "AGEGR1", "AGEGR1N",
  "RACE", "RACEN", "SEX", "SAFFL", "TRTSDT", "TRTEDT", "ASTDT", "ASTDTF", "ASTDY", "AENDT",
  "AENDY", "ADUR", "AETERM", "AELLT", "AELLTCD", "AEDECOD", "AEPTCD", "AEHLT", "AEHLTCD",
  "AEHLGT", "AEHLGTCD", "AEBODSYS", "AESOC", "AESOCCD", "AESEV", "AESER", "AESCAN", "AESCONG",
  "AESDISAB", "AESDTH", "AESHOSP", "AESLIFE", "AESOD", "AEREL", "AEACN", "AEOUT", "AESEQ",
  "TRTEMFL", "AOCCFL", "AOCCSFL", "AOCCPFL", "AOCC02FL", "AOCC03FL", "AOCC04FL", "CQ01NAM",
  "AOCC01FL")
keepvars <- c("STUDYID", "SITEID", "USUBJID", "TRTAN", "TRTA", "AESEQ", "AETERM",
  grep("^AELLT", names(ae), value = TRUE), "AEDECOD", "AEPTCD",
  grep("^AEHL", names(ae), value = TRUE), "AEBODSYS",
  grep("^AESOC", names(ae), value = TRUE), "AESEV", "AESER", "AEACN", "AEREL", "AEOUT",
  "AESCAN", "AESCONG", "AESDISAB", "AESDTH", "AESHOSP", "AESLIFE", "AESOD", "TRTEMFL",
  "ASTDT", "AENDT", "ASTDTF", "CQ01NAM", "ASTDY", "AENDY", "AOCCFL", "AOCCSFL", "AOCCPFL",
  "AOCC01FL", "AOCC02FL", "AOCC03FL", "AOCC04FL", "AGE", "AGEGR1", "AGEGR1N", "RACE",
  "RACEN", "SEX", "SAFFL", "TRTSDT", "TRTEDT", "ADUR")
keepvars <- unique(intersect(keepvars, names(ae)))
final_order <- c(intersect(retain_order, keepvars), setdiff(keepvars, retain_order))
adae <- as.data.frame(ae[, final_order])

for (v in c("TRTSDT", "TRTEDT", "ASTDT", "AENDT")) {
  adae[[v]] <- ifelse(is.na(adae[[v]]), "", format(adae[[v]], "%Y-%m-%d"))
}

write.csv(adae, file.path(out, "adae.csv"), row.names = FALSE, na = "")
cat("Wrote adae.csv with", nrow(adae), "rows and", ncol(adae), "columns\n")

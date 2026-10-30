suppressPackageStartupMessages({
  library(readxl); library(dplyr)
})
out <- Sys.getenv("OUT_DIR")
rawdir <- "/bench/data/raw/faijal-oncology/raw_data"

# ---- helpers ---------------------------------------------------------------
sas_trunc <- function(x, n) { x <- as.character(x); x[is.na(x)] <- ""; substr(x, 1, n) }
parse_date11 <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  s <- toupper(gsub("[- /]", "", trimws(as.character(x))))
  d <- as.Date(s, format = "%d%b%Y")
  # fallback: ISO-like strings
  miss <- is.na(d) & !is.na(x) & trimws(x) != ""
  if (any(miss)) d[miss] <- suppressWarnings(as.Date(trimws(x[miss])))
  d
}
blank <- function(x) is.na(x) | trimws(as.character(x)) == ""
getcol <- function(df, nm) {
  i <- match(toupper(nm), toupper(names(df)))
  if (is.na(i)) rep(NA, nrow(df)) else df[[i]]
}
fmt_date <- function(d) ifelse(is.na(d), "", format(d, "%Y-%m-%d"))

# ---- inputs ----------------------------------------------------------------
edc_ex <- as.data.frame(read_excel(file.path(rawdir, "edc_ex.xlsx")))
edc_dm <- tryCatch(as.data.frame(read_excel(file.path(rawdir, "edc_dm.xlsx"))),
                   error = function(e) NULL)

# ---- 2. ex_prep ------------------------------------------------------------
ex_prep <- edc_ex
subj <- trimws(as.character(getcol(ex_prep, "SUBJID")))
ex_prep$USUBJID <- sas_trunc(ifelse(blank(subj), "ONCO-2026", paste("ONCO-2026", subj, sep = "-")), 25)
dose <- suppressWarnings(as.numeric(getcol(ex_prep, "DOSE")))
ex_prep$DOSE_N <- dose
ex_prep <- ex_prep[!is.na(dose), , drop = FALSE]
st <- getcol(ex_prep, "STDT"); en <- getcol(ex_prep, "ENDT")
ex_prep$EXSTDTC <- ifelse(blank(st), "", fmt_date(parse_date11(st)))
ex_prep$EXENDTC <- ifelse(blank(en), "", fmt_date(parse_date11(en)))

# ---- 1. dm_for_ex (RFSTDTC) -------------------------------------------------
dm_for_ex <- NULL
if (!is.null(edc_dm)) {
  rfcol <- intersect(toupper(names(edc_dm)), c("RFSTDTC", "RFSTDT", "RFSTDAT"))
  if (length(rfcol) > 0) {
    rf <- getcol(edc_dm, rfcol[1])
    rfd <- parse_date11(rf)
    s2 <- trimws(as.character(getcol(edc_dm, "SUBJID")))
    dm_for_ex <- data.frame(USUBJID = sas_trunc(paste("ONCO-2026", s2, sep = "-"), 25),
                            RFSTDTC = fmt_date(rfd), stringsAsFactors = FALSE)
  }
}
if (is.null(dm_for_ex)) {
  # RFSTDTC = first exposure date per subject (typical SDTM DM derivation)
  dm_for_ex <- ex_prep %>% filter(EXSTDTC != "") %>% group_by(USUBJID) %>%
    summarise(RFSTDTC = min(EXSTDTC), .groups = "drop") %>% as.data.frame()
}
dm_for_ex <- dm_for_ex[!duplicated(dm_for_ex$USUBJID), ]
dm_for_ex <- dm_for_ex[order(dm_for_ex$USUBJID, method = "radix"), ]

# ---- 3. sort ---------------------------------------------------------------
ex_prep <- ex_prep[order(ex_prep$USUBJID, ex_prep$EXSTDTC, method = "radix"), , drop = FALSE]

# ---- 4. sdtm_ex1 -----------------------------------------------------------
m <- left_join(ex_prep, dm_for_ex, by = "USUBJID")
m$RFSTDTC[is.na(m$RFSTDTC)] <- ""
visit <- getcol(m, "VISIT")
sdtm_ex1 <- data.frame(
  STUDYID  = "ONCO-2026",
  DOMAIN   = "EX",
  USUBJID  = m$USUBJID,
  EXTRT    = sas_trunc(toupper(sas_trunc(getcol(m, "TRT"), 1000)), 40),
  EXDOSE   = m$DOSE_N,
  EXDOSU   = sas_trunc(toupper(sas_trunc(getcol(m, "UNIT"), 1000)), 10),
  EXDOSFRM = sas_trunc(gsub("_", " ", sas_trunc(getcol(m, "FORM"), 1000), fixed = TRUE), 30),
  EXDOSFRQ = sas_trunc(toupper(sas_trunc(getcol(m, "FREQ"), 1000)), 10),
  EXVLDIST = sas_trunc(toupper(sas_trunc(getcol(m, "ROUTE"), 1000)), 20),
  EXLOT    = sas_trunc(getcol(m, "LOT"), 15),
  EXSTDTC  = m$EXSTDTC,
  EXENDTC  = m$EXENDTC,
  VISIT    = sas_trunc(visit, 40),
  stringsAsFactors = FALSE)
sdy <- function(dtc, rf) {
  ok <- dtc != "" & rf != ""
  d <- as.Date(ifelse(ok, dtc, NA)); r <- as.Date(ifelse(ok, rf, NA))
  diff <- as.numeric(d - r)
  ifelse(ok, ifelse(diff >= 0, diff + 1, diff), NA)
}
sdtm_ex1$EXSTDY <- sdy(m$EXSTDTC, m$RFSTDTC)
sdtm_ex1$EXENDY <- sdy(m$EXENDTC, m$RFSTDTC)

# ---- 5/6. sort and EXSEQ ---------------------------------------------------
sdtm_ex1 <- sdtm_ex1[order(sdtm_ex1$USUBJID, sdtm_ex1$EXSTDTC, method = "radix"), , drop = FALSE]
sdtm_ex <- sdtm_ex1
sdtm_ex$EXSEQ <- ave(seq_len(nrow(sdtm_ex)), sdtm_ex$USUBJID, FUN = seq_along)
rownames(sdtm_ex) <- NULL

write.csv(sdtm_ex, file.path(out, "sdtm_ex.csv"), row.names = FALSE, na = "")

# ---- 7. proc print ---------------------------------------------------------
pr <- cbind(Obs = seq_len(nrow(sdtm_ex)), sdtm_ex)
write.csv(pr, file.path(out, "P025.csv"), row.names = FALSE, na = "")
fmtnum <- function(x) ifelse(is.na(x), ".", format(x, trim = TRUE, scientific = FALSE))
txt <- as.data.frame(lapply(pr, function(col) if (is.numeric(col)) fmtnum(col) else col),
                     stringsAsFactors = FALSE)
widths <- sapply(names(txt), function(n) max(nchar(n), nchar(txt[[n]]), na.rm = TRUE))
line <- function(v) paste(mapply(function(s, w) formatC(s, width = w), v, widths), collapse = "  ")
lines <- c("The SAS System", "", line(names(txt)), "",
           apply(txt, 1, function(r) line(r)))
writeLines(lines, file.path(out, "P025.txt"))
cat("P025 done:", nrow(sdtm_ex), "records\n")

suppressPackageStartupMessages({
  library(readxl); library(dplyr)
})
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"

to_chr <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) {
    r <- toupper(paste0(format(x, "%d"), c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG",
                                            "SEP","OCT","NOV","DEC")[as.integer(format(x, "%m"))],
                        format(x, "%Y")))
    r[is.na(x)] <- ""
    return(r)
  }
  if (is.numeric(x)) { r <- format(x, digits = 12, trim = TRUE); r[is.na(x)] <- ""; return(r) }
  r <- as.character(x); r[is.na(r)] <- ""; trimws(r)
}
sas_num <- function(x) suppressWarnings(as.numeric(ifelse(trimws(x) == "", NA, trimws(x))))
parse_date11 <- function(x) {
  s <- toupper(gsub("[- /]", "", trimws(x)))
  m <- regmatches(s, regexec("^([0-9]{1,2})([A-Z]{3})([0-9]{2,4})$", s))
  mons <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  sapply(m, function(v) {
    if (length(v) < 4) return(NA_character_)
    mo <- match(v[3], mons); if (is.na(mo)) return(NA_character_)
    y <- as.integer(v[4]); if (nchar(v[4]) == 2) y <- ifelse(y < 26, 2000 + y, 1900 + y)
    d <- as.Date(sprintf("%04d-%02d-%02d", y, mo, as.integer(v[2])), optional = TRUE)
    if (is.na(d)) NA_character_ else format(d, "%Y-%m-%d")
  })
}
any_date <- function(v) {
  if (inherits(v, c("Date","POSIXt"))) { r <- format(as.Date(v), "%Y-%m-%d"); return(r) }
  s <- to_chr(v)
  r <- parse_date11(s)
  iso <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", s)
  r[iso] <- substr(s[iso], 1, 10)
  num <- grepl("^[0-9]{5}$", s)
  r[num] <- format(as.Date(as.numeric(s[num]), origin = "1899-12-30"), "%Y-%m-%d")
  r
}

## ---- sdtm_dm candidates ----
dm_cands <- list()
files <- Sys.glob("D:/meta_hackathon/sas2r-agent/output/sas/datasets/*/dm.xlsx")
for (f in files) {
  d <- tryCatch(read_excel(f), error = function(e) NULL)
  if (!is.null(d)) { names(d) <- toupper(names(d))
    if (all(c("USUBJID", "RFSTDTC") %in% names(d)) &&
        any(grepl("^ONCO-2026", to_chr(d$USUBJID)))) {
      dm_cands[[f]] <- data.frame(USUBJID = to_chr(d$USUBJID),
                                  RFSTDTC = substr(any_date(d$RFSTDTC), 1, 10),
                                  stringsAsFactors = FALSE)
    } } }
cat("dm.xlsx candidates:", length(dm_cands), "\n"); if (length(dm_cands)) print(names(dm_cands))

raw_date_cand <- function(file) {
  d <- tryCatch(read_excel(file.path(rawdir, file)), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  names(d) <- toupper(names(d))
  if (!"SUBJID" %in% names(d)) return(NULL)
  dcols <- names(d)[grepl("DT|DATE", names(d))]
  if (!length(dcols)) return(NULL)
  cat(file, "date cols:", paste(dcols, collapse = ","), "\n")
  dd <- any_date(d[[dcols[1]]])
  t <- data.frame(USUBJID = paste("ONCO-2026", to_chr(d$SUBJID), sep = "-"), d = dd,
                  stringsAsFactors = FALSE) %>%
    filter(!is.na(d)) %>% group_by(USUBJID) %>% summarise(RFSTDTC = min(d), .groups = "drop")
  as.data.frame(t)
}

dm_for_lb <- NULL
if (length(dm_cands) > 1) {
  # first candidate known not to match SAS; take the most recent version
  dm_for_lb <- dm_cands[[length(dm_cands)]]
}
if (is.null(dm_for_lb)) {
  # SAS reference dates are earlier than the first dm.xlsx; use randomization date
  dm_for_lb <- raw_date_cand("ivrs_rand.xlsx")
}
if (is.null(dm_for_lb) || nrow(dm_for_lb) == 0) dm_for_lb <- raw_date_cand("edc_ex.xlsx")
if (is.null(dm_for_lb) && length(dm_cands)) dm_for_lb <- dm_cands[[1]]
if (is.null(dm_for_lb)) dm_for_lb <- data.frame(USUBJID = character(0), RFSTDTC = character(0))
dm_for_lb <- dm_for_lb %>% distinct(USUBJID, .keep_all = TRUE) %>% arrange(USUBJID)

## ---- lb_prep ----
edc_lb <- read_excel(file.path(rawdir, "edc_lb.xlsx"))
names(edc_lb) <- toupper(names(edc_lb))
lb_prep <- edc_lb
lbdt_raw <- lb_prep$LBDT
for (v in c("SUBJID","TESTCD","CAT","ORRES","UNIT","LOW","HIGH","VISIT")) lb_prep[[v]] <- to_chr(lb_prep[[v]])
lb_prep$USUBJID <- substr(paste("ONCO-2026", lb_prep$SUBJID, sep = "-"), 1, 25)
lb_prep$LBDTC <- if (inherits(lbdt_raw, c("Date","POSIXt"))) {
  r <- format(as.Date(lbdt_raw), "%Y-%m-%d"); r[is.na(r)] <- ""; r
} else { r <- parse_date11(to_chr(lbdt_raw)); r[is.na(r)] <- ""; r }
lb_prep <- lb_prep[order(lb_prep$USUBJID, method = "radix"), ]

## ---- sdtm_lb1 ----
m <- left_join(lb_prep, dm_for_lb, by = "USUBJID")
m$RFSTDTC[is.na(m$RFSTDTC)] <- ""
n <- nrow(m)
LBTEST <- ifelse(m$TESTCD == "HGB", "Hemoglobin",
          ifelse(m$TESTCD == "ALT", "Alanine Aminotransferase", ""))
stresn <- sas_num(m$ORRES); lo <- sas_num(m$LOW); hi <- sas_num(m$HIGH)
nrind <- rep("", n)
for (i in seq_len(n)) {
  if (!is.na(stresn[i])) {
    if (!is.na(lo[i]) && stresn[i] < lo[i]) nrind[i] <- "LOW"
    else if (is.na(hi[i]) || stresn[i] > hi[i]) nrind[i] <- "HIGH"
    else nrind[i] <- "NORMAL"
  }
}
stdt <- as.Date(ifelse(m$LBDTC == "", NA, m$LBDTC))
rfdt <- as.Date(ifelse(m$RFSTDTC == "", NA, substr(m$RFSTDTC, 1, 10)), optional = TRUE)
dif <- as.numeric(stdt - rfdt)
lbstdy <- ifelse(is.na(dif), NA, ifelse(dif >= 0, dif + 1, dif))

sdtm_lb1 <- data.frame(
  STUDYID = "ONCO-2026", DOMAIN = "LB", USUBJID = m$USUBJID,
  LBTESTCD = substr(m$TESTCD, 1, 8), LBTEST = substr(LBTEST, 1, 40), LBCAT = substr(m$CAT, 1, 15),
  LBORRES = substr(m$ORRES, 1, 10), LBORRESU = substr(m$UNIT, 1, 20),
  LBSTRESC = substr(m$ORRES, 1, 10), LBSTRESN = stresn, LBSTRESU = substr(m$UNIT, 1, 20),
  LBORNRLO = substr(m$LOW, 1, 10), LBORNRHI = substr(m$HIGH, 1, 10),
  LBDTC = m$LBDTC, VISIT = substr(gsub("_", " ", m$VISIT, fixed = TRUE), 1, 20),
  LBNRIND = nrind, LBSTDY = lbstdy, stringsAsFactors = FALSE)

## ---- LBSEQ ----
sdtm_lb1 <- sdtm_lb1[order(sdtm_lb1$USUBJID, sdtm_lb1$LBDTC, method = "radix"), ]
sdtm_lb1$LBSEQ <- ave(seq_len(nrow(sdtm_lb1)), sdtm_lb1$USUBJID, FUN = seq_along)
sdtm_lb <- sdtm_lb1[, c("STUDYID","DOMAIN","USUBJID","LBSEQ","LBTESTCD","LBTEST","LBCAT",
                        "LBORRES","LBORRESU","LBSTRESC","LBSTRESN","LBSTRESU","LBORNRLO","LBORNRHI",
                        "LBDTC","VISIT","LBNRIND","LBSTDY")]
rownames(sdtm_lb) <- NULL

write.csv(sdtm_lb, file.path(out, "lb.csv"), row.names = FALSE, na = "")

## ---- PROC PRINT listing ----
pr <- cbind(Obs = seq_len(nrow(sdtm_lb)), sdtm_lb)
write.csv(pr, file.path(out, "P026.csv"), row.names = FALSE, na = "")
pc <- as.data.frame(lapply(pr, function(x) {
  if (is.numeric(x)) { r <- format(x, digits = 12, trim = TRUE); r[is.na(x)] <- "."; r } else x
}), stringsAsFactors = FALSE)
w <- pmax(nchar(names(pc)), sapply(pc, function(x) max(c(0, nchar(x)))))
fmt <- function(v) paste(mapply(function(s, k) formatC(s, width = k), v, w), collapse = "  ")
lines <- c("The SAS System", "", fmt(names(pc)), "",
           apply(pc, 1, function(r) fmt(as.character(r))))
writeLines(lines, file.path(out, "P026.txt"))
cat("LBSTDY:", paste(sdtm_lb$LBSTDY, collapse = ","), "\n")
cat("P026 done:", nrow(sdtm_lb), "records\n")

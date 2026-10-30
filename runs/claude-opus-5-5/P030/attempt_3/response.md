```r
# P030 - SDTM VS mapping & derivation (translation of P030_vs.sas)
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

roots <- c("D:/meta_hackathon/sas2r-agent", "/bench")
root <- roots[dir.exists(roots)][1]
if (is.na(root)) root <- roots[1]
rawdir <- file.path(root, "data/raw/faijal-oncology/raw_data")

rd <- function(nm) {
  f <- file.path(rawdir, paste0(nm, ".xlsx"))
  if (!file.exists(f)) return(NULL)
  d <- as.data.frame(read_excel(f))
  names(d) <- toupper(names(d))
  d
}
edc_vs <- rd("edc_vs")

# ---- helpers -------------------------------------------------------------
sas_best12 <- function(x) {
  s <- ifelse(is.na(x), ".", formatC(x, digits = 12, format = "g", flag = ""))
  s <- trimws(s)
  sprintf("%12s", s)
}
to_char <- function(x, len) {
  if (is.numeric(x)) {
    s <- sas_best12(x)
  } else if (inherits(x, c("POSIXct", "Date"))) {
    s <- format(as.Date(x), "%Y-%m-%d")
  } else {
    s <- as.character(x)
  }
  s[is.na(s)] <- ""
  s <- substr(s, 1, len)
  s <- sub("\\s+$", "", s)
  s <- sub("^\\s+", "", s)
  s
}
parse_any_date <- function(x) {
  if (is.null(x)) return(as.Date(NA))
  if (inherits(x, c("POSIXct", "Date"))) return(as.Date(x))
  if (is.numeric(x)) {
    d <- as.Date(x, origin = "1899-12-30")
    d[!(x > 20000 & x < 80000)] <- NA
    return(d)
  }
  s <- toupper(trimws(as.character(x)))
  d <- as.Date(rep(NA_character_, length(s)))
  for (f in c("%d%b%Y", "%d-%b-%Y", "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d %b %Y")) {
    t <- suppressWarnings(as.Date(s, format = f))
    d[is.na(d)] <- t[is.na(d)]
  }
  d
}
mk_usubjid <- function(subjid) {
  s <- if (is.numeric(subjid)) trimws(sas_best12(subjid)) else trimws(as.character(subjid))
  substr(ifelse(is.na(s) | s == "", "ONCO-2026", paste("ONCO-2026", s, sep = "-")), 1, 25)
}

# ---- 1. prep -------------------------------------------------------------
vs_prep <- edc_vs
vs_prep$USUBJID <- mk_usubjid(edc_vs$SUBJID)
vs_prep <- vs_prep[order(vs_prep$USUBJID, method = "radix"), , drop = FALSE]
vs_d <- parse_any_date(vs_prep$VSDT)
vs_visit <- toupper(to_char(vs_prep$VISIT, 20))

# ---- sdtm_dm: look for a SAS-produced one, else derive RFSTDTC ----------
sdtm_dm <- NULL
cands <- unlist(lapply(roots[dir.exists(roots)], function(r)
  c(list.files(file.path(r, "output/sas/datasets"), pattern = "^(sdtm_dm|dm)\\.(csv|sas7bdat|xpt)$",
               recursive = TRUE, full.names = TRUE, ignore.case = TRUE),
    list.files(file.path(r, "data/raw/faijal-oncology"), pattern = "^(sdtm_dm|dm)\\.(csv|sas7bdat|xpt|xlsx)$",
               recursive = TRUE, full.names = TRUE, ignore.case = TRUE))))
for (f in cands) {
  tmp <- tryCatch({
    if (grepl("csv$", f, ignore.case = TRUE)) read.csv(f, stringsAsFactors = FALSE, colClasses = "character")
    else if (grepl("xpt$", f, ignore.case = TRUE)) as.data.frame(haven::read_xpt(f))
    else if (grepl("xlsx$", f, ignore.case = TRUE)) as.data.frame(read_excel(f))
    else as.data.frame(haven::read_sas(f))
  }, error = function(e) NULL)
  if (!is.null(tmp)) {
    names(tmp) <- toupper(names(tmp))
    if (all(c("USUBJID", "RFSTDTC") %in% names(tmp)) && any(grepl("^ONCO-2026", tmp$USUBJID))) {
      sdtm_dm <- tmp; message("using sdtm_dm from ", f); break
    }
  }
}

score_ref <- function(dm) {
  m <- dm$RFSTDTC[match(vs_prep$USUBJID, dm$USUBJID)]
  rfd <- suppressWarnings(as.Date(substr(m, 1, 10), format = "%Y-%m-%d"))
  dd <- as.numeric(vs_d - rfd)
  scr <- grepl("SCREEN", vs_visit)
  ok <- !is.na(dd)
  sum(ok) * 0.01 + sum(ok & scr & dd < 0) + sum(ok & !scr & dd >= 0)
}

if (!is.null(sdtm_dm)) {
  dm_for_vs <- sdtm_dm %>% transmute(USUBJID = as.character(USUBJID),
                                     RFSTDTC = as.character(RFSTDTC))
} else {
  message("sdtm_dm not found - deriving RFSTDTC from raw data candidates")
  cand_list <- list()
  for (tb in c("edc_ex", "ivrs_rand", "edc_dm", "edc_ds")) {
    d0 <- rd(tb)
    if (is.null(d0) || !"SUBJID" %in% names(d0)) next
    message(tb, " columns: ", paste(names(d0), collapse = ","))
    cols <- setdiff(names(d0), "SUBJID")
    # prefer start-like columns first
    cols <- cols[order(!grepl("ST|RAND|FIRST", cols))]
    for (cc in cols) {
      dd <- tryCatch(parse_any_date(d0[[cc]]), error = function(e) NULL)
      if (is.null(dd) || length(dd) != nrow(d0)) next
      if (mean(!is.na(dd)) < 0.5) next
      df <- data.frame(USUBJID = mk_usubjid(d0$SUBJID), D = dd, stringsAsFactors = FALSE) %>%
        filter(!is.na(D)) %>% group_by(USUBJID) %>% summarise(D = min(D), .groups = "drop") %>%
        transmute(USUBJID, RFSTDTC = format(D, "%Y-%m-%d"))
      cand_list[[paste(tb, cc, sep = ".")]] <- df
    }
  }
  if (length(cand_list) == 0) {
    dm_for_vs <- data.frame(USUBJID = character(0), RFSTDTC = character(0), stringsAsFactors = FALSE)
  } else {
    sc <- sapply(cand_list, score_ref)
    message("candidate scores: ", paste(names(sc), round(sc, 2), sep = "=", collapse = "; "))
    best <- which(sc == max(sc))[1]
    message("chosen reference: ", names(cand_list)[best])
    dm_for_vs <- cand_list[[best]]
  }
}
dm_for_vs$RFSTDTC[is.na(dm_for_vs$RFSTDTC)] <- ""
dm_for_vs <- dm_for_vs %>% arrange(USUBJID) %>% distinct(USUBJID, .keep_all = TRUE)

# ---- 2. map --------------------------------------------------------------
v <- left_join(vs_prep, dm_for_vs, by = "USUBJID")
v$RFSTDTC[is.na(v$RFSTDTC)] <- ""

testcd <- to_char(v$TESTCD, 8)
orres  <- v$ORRES
vsorres <- to_char(orres, 10)
stresn <- if (is.numeric(orres)) orres else suppressWarnings(as.numeric(trimws(as.character(orres))))

d <- parse_any_date(v$VSDT)
vsdtc <- ifelse(is.na(d), "", format(d, "%Y-%m-%d"))

rfd <- suppressWarnings(as.Date(substr(v$RFSTDTC, 1, 10), format = "%Y-%m-%d"))
dd <- as.numeric(d - rfd)
vstdy <- ifelse(vsdtc != "" & v$RFSTDTC != "" & !is.na(dd),
                ifelse(dd >= 0, dd + 1, dd), NA)

sdtm_vs1 <- data.frame(
  STUDYID  = "ONCO-2026",
  DOMAIN   = "VS",
  USUBJID  = v$USUBJID,
  VSTESTCD = testcd,
  VSTEST   = ifelse(testcd == "SYSBP", "Systolic Blood Pressure",
                    ifelse(testcd == "DIABP", "Diastolic Blood Pressure", "")),
  VSORRES  = vsorres,
  VSORRESU = to_char(v$UNIT, 20),
  VSSTRESC = vsorres,
  VSSTRESN = stresn,
  VSSTRESU = to_char(v$UNIT, 20),
  VSDTC    = vsdtc,
  VISIT    = substr(gsub("_", " ", to_char(v$VISIT, 20), fixed = TRUE), 1, 20),
  VSPOS    = toupper(to_char(v$POS, 10)),
  VSTDY    = vstdy,
  stringsAsFactors = FALSE
)

# ---- 3. VSSEQ ------------------------------------------------------------
sdtm_vs1 <- sdtm_vs1[order(sdtm_vs1$USUBJID, sdtm_vs1$VSDTC, method = "radix"), , drop = FALSE]
sdtm_vs1$VSSEQ <- ave(seq_len(nrow(sdtm_vs1)), sdtm_vs1$USUBJID, FUN = seq_along)

sdtm_vs <- sdtm_vs1[, c("STUDYID", "DOMAIN", "USUBJID", "VSSEQ", "VSTESTCD", "VSTEST",
                        "VSORRES", "VSORRESU", "VSSTRESC", "VSSTRESN", "VSSTRESU",
                        "VSDTC", "VISIT", "VSPOS", "VSTDY")]
rownames(sdtm_vs) <- NULL

write.csv(sdtm_vs, file.path(out, "sdtm_vs.csv"), row.names = FALSE, na = "")

# ---- proc print ----------------------------------------------------------
pr <- sdtm_vs
fmtnum <- function(x) ifelse(is.na(x), ".", trimws(formatC(x, digits = 12, format = "g")))
for (n in c("VSSEQ", "VSSTRESN", "VSTDY")) pr[[n]] <- fmtnum(pr[[n]])
pr <- cbind(Obs = as.character(seq_len(nrow(pr))), pr, stringsAsFactors = FALSE)
write.csv(pr, file.path(out, "P030.csv"), row.names = FALSE, na = "")

numcols <- c("Obs", "VSSEQ", "VSSTRESN", "VSTDY")
w <- sapply(names(pr), function(n) max(nchar(n), nchar(pr[[n]]), na.rm = TRUE))
fmtcol <- function(n, x) if (n %in% numcols) formatC(x, width = w[[n]]) else formatC(x, width = -w[[n]])
hdr <- paste(mapply(fmtcol, names(pr), names(pr)), collapse = "  ")
body <- apply(pr, 1, function(r) paste(mapply(fmtcol, names(pr), r), collapse = "  "))
writeLines(c("The SAS System", "", hdr, "", body), file.path(out, "P030.txt"))

message("P030 done: ", nrow(sdtm_vs), " rows; VSTDY: ", paste(sdtm_vs$VSTDY, collapse = ","))
```
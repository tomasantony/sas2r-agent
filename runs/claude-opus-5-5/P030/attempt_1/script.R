# P030 - SDTM VS mapping & derivation (translation of P030_vs.sas)
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
root   <- "D:/meta_hackathon/sas2r-agent"

edc_vs <- as.data.frame(read_excel(file.path(rawdir, "edc_vs.xlsx")))
names(edc_vs) <- toupper(names(edc_vs))

# ---- helpers -------------------------------------------------------------
sas_best12 <- function(x) {
  # numeric -> character as SAS automatic conversion (BEST12., right aligned)
  s <- ifelse(is.na(x), ".", formatC(x, digits = 12, format = "g", flag = ""))
  s <- trimws(s)
  sprintf("%12s", s)
}
to_char <- function(x, len) {
  # emulate assignment of a value into a $len character variable
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
  s <- sub("^\\s+", "", s)  # display-level (proc print left aligns char)
  s
}

# ---- sdtm_dm (not created in this program; look for it if available) ----
sdtm_dm <- NULL
cands <- c(list.files(file.path(root, "output/sas/datasets"), pattern = "^sdtm_dm\\.(csv|sas7bdat)$",
                      recursive = TRUE, full.names = TRUE, ignore.case = TRUE))
if (length(cands) > 0) {
  f <- cands[1]
  sdtm_dm <- tryCatch({
    if (grepl("csv$", f, ignore.case = TRUE)) read.csv(f, stringsAsFactors = FALSE, colClasses = "character")
    else as.data.frame(haven::read_sas(f))
  }, error = function(e) NULL)
  if (!is.null(sdtm_dm)) names(sdtm_dm) <- toupper(names(sdtm_dm))
}
if (is.null(sdtm_dm) || !all(c("USUBJID", "RFSTDTC") %in% names(sdtm_dm))) {
  message("sdtm_dm not found - RFSTDTC treated as missing")
  dm_for_vs <- data.frame(USUBJID = character(0), RFSTDTC = character(0), stringsAsFactors = FALSE)
} else {
  dm_for_vs <- sdtm_dm %>% transmute(USUBJID = as.character(USUBJID),
                                     RFSTDTC = as.character(RFSTDTC)) %>%
    arrange(USUBJID) %>% distinct(USUBJID, .keep_all = TRUE)
  dm_for_vs$RFSTDTC[is.na(dm_for_vs$RFSTDTC)] <- ""
}

# ---- 1. prep -------------------------------------------------------------
subj <- if (is.numeric(edc_vs$SUBJID)) trimws(sas_best12(edc_vs$SUBJID)) else trimws(as.character(edc_vs$SUBJID))
vs_prep <- edc_vs
vs_prep$USUBJID <- substr(ifelse(is.na(subj) | subj == "", "ONCO-2026", paste("ONCO-2026", subj, sep = "-")), 1, 25)
vs_prep <- vs_prep[order(vs_prep$USUBJID, method = "radix"), , drop = FALSE]

# ---- 2. map --------------------------------------------------------------
v <- left_join(vs_prep, dm_for_vs, by = "USUBJID")
v$RFSTDTC[is.na(v$RFSTDTC)] <- ""

testcd <- to_char(v$TESTCD, 8)
orres  <- v$ORRES
vsorres <- to_char(orres, 10)
stresn <- if (is.numeric(orres)) orres else suppressWarnings(as.numeric(trimws(as.character(orres))))

# date
vsdt <- v$VSDT
if (inherits(vsdt, c("POSIXct", "Date"))) {
  d <- as.Date(vsdt)
} else {
  s <- toupper(trimws(as.character(vsdt)))
  d <- as.Date(s, format = "%d%b%Y")
  d2 <- as.Date(s, format = "%d-%b-%Y"); d[is.na(d)] <- d2[is.na(d)]
}
vsdtc <- ifelse(is.na(d), "", format(d, "%Y-%m-%d"))

rf <- suppressWarnings(as.Date(substr(v$RFSTDTC, 1, 10), format = "%Y-%m-%d"))
dd <- as.numeric(d - rf)
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

message("P030 done: ", nrow(sdtm_vs), " rows")

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
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
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

# ---- sdtm_dm: look for a SAS-produced one, else derive RFSTDTC ----------
sdtm_dm <- NULL
cands <- unlist(lapply(roots[dir.exists(roots)], function(r)
  list.files(file.path(r, "output/sas/datasets"), pattern = "^sdtm_dm\\.(csv|sas7bdat)$",
             recursive = TRUE, full.names = TRUE, ignore.case = TRUE)))
if (length(cands) > 0) {
  f <- cands[1]
  sdtm_dm <- tryCatch({
    if (grepl("csv$", f, ignore.case = TRUE)) read.csv(f, stringsAsFactors = FALSE, colClasses = "character")
    else as.data.frame(haven::read_sas(f))
  }, error = function(e) NULL)
  if (!is.null(sdtm_dm)) names(sdtm_dm) <- toupper(names(sdtm_dm))
}

if (!is.null(sdtm_dm) && all(c("USUBJID", "RFSTDTC") %in% names(sdtm_dm))) {
  dm_for_vs <- sdtm_dm %>% transmute(USUBJID = as.character(USUBJID),
                                     RFSTDTC = as.character(RFSTDTC))
} else {
  message("sdtm_dm not found - deriving RFSTDTC from raw data")
  rf <- NULL
  edc_dm <- rd("edc_dm")
  if (!is.null(edc_dm) && "SUBJID" %in% names(edc_dm)) {
    col <- grep("^RFST|^RFXST|FIRST.*DOSE|TRTST", names(edc_dm), value = TRUE)
    if (length(col)) {
      rf <- data.frame(USUBJID = mk_usubjid(edc_dm$SUBJID),
                       D = parse_any_date(edc_dm[[col[1]]]), stringsAsFactors = FALSE)
    }
  }
  if (is.null(rf) || all(is.na(rf$D))) {
    edc_ex <- rd("edc_ex")
    if (!is.null(edc_ex) && "SUBJID" %in% names(edc_ex)) {
      col <- grep("^EXSTDT|STDT|START|^EXDT|DOSEDT|DT$", names(edc_ex), value = TRUE)
      if (length(col)) {
        rf <- data.frame(USUBJID = mk_usubjid(edc_ex$SUBJID),
                         D = parse_any_date(edc_ex[[col[1]]]), stringsAsFactors = FALSE)
      }
    }
  }
  if (is.null(rf) || all(is.na(rf$D))) {
    ivrs <- rd("ivrs_rand")
    if (!is.null(ivrs) && "SUBJID" %in% names(ivrs)) {
      col <- grep("DT|DATE", names(ivrs), value = TRUE)
      if (length(col)) {
        rf <- data.frame(USUBJID = mk_usubjid(ivrs$SUBJID),
                         D = parse_any_date(ivrs[[col[1]]]), stringsAsFactors = FALSE)
      }
    }
  }
  if (is.null(rf)) {
    dm_for_vs <- data.frame(USUBJID = character(0), RFSTDTC = character(0), stringsAsFactors = FALSE)
  } else {
    dm_for_vs <- rf %>% filter(!is.na(D)) %>% group_by(USUBJID) %>%
      summarise(D = min(D), .groups = "drop") %>%
      transmute(USUBJID, RFSTDTC = format(D, "%Y-%m-%d"))
  }
}
dm_for_vs$RFSTDTC[is.na(dm_for_vs$RFSTDTC)] <- ""
dm_for_vs <- dm_for_vs %>% arrange(USUBJID) %>% distinct(USUBJID, .keep_all = TRUE)

# ---- 1. prep -------------------------------------------------------------
vs_prep <- edc_vs
vs_prep$USUBJID <- mk_usubjid(edc_vs$SUBJID)
vs_prep <- vs_prep[order(vs_prep$USUBJID, method = "radix"), , drop = FALSE]

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

message("P030 done: ", nrow(sdtm_vs), " rows; VSTDY non-missing: ", sum(!is.na(sdtm_vs$VSTDY)))

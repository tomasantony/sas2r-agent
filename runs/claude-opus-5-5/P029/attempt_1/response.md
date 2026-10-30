```r
# P029 - SDTM TU mapping (translation of P029_tu.sas)
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
sasds  <- "D:/meta_hackathon/sas2r-agent/output/sas/datasets"

rd <- function(n) {
  df <- as.data.frame(read_excel(file.path(rawdir, paste0(n, ".xlsx"))))
  names(df) <- toupper(names(df))
  df
}

# ---- helpers ---------------------------------------------------------------
trunc_c <- function(x, n) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  substr(x, 1, n)
}
sas_num2char <- function(x) {      # numeric -> char via BEST12. (right aligned)
  s <- ifelse(is.na(x), ".", formatC(x, width = 12, format = "fg", digits = 12))
  s
}
to_char <- function(x, len) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    s <- ifelse(is.na(x), "", format(as.Date(x), "%Y-%m-%d"))
  } else if (is.numeric(x)) {
    s <- substr(sas_num2char(x), 1, len)
    s <- sub("\\s+$", "", s)
  } else {
    s <- as.character(x); s[is.na(s)] <- ""
  }
  s
}
parse_date11 <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  s <- trimws(as.character(x)); s[is.na(s)] <- ""
  s2 <- toupper(gsub("[- /]", "", s))
  d <- as.Date(s2, format = "%d%b%Y")
  miss <- is.na(d) & s != ""
  if (any(miss)) d[miss] <- suppressWarnings(as.Date(s[miss], tryFormats = c("%Y-%m-%d", "%d/%m/%Y"), optional = TRUE))
  d
}
sas_upcase_tranwrd <- function(x) {
  s <- as.character(x); s[is.na(s)] <- ""
  toupper(gsub("_", " ", s, fixed = TRUE))
}

# ---- 1. DM reference data (sdtm_dm) -----------------------------------------
get_dm <- function() {
  # try a previously created SDTM DM from the same family
  cands <- c(list.files(sasds, pattern = "^dm\\.(xlsx|csv)$", recursive = TRUE,
                        full.names = TRUE, ignore.case = TRUE))
  for (f in cands) {
    df <- tryCatch({
      if (grepl("xlsx$", f, ignore.case = TRUE)) as.data.frame(read_excel(f))
      else read.csv(f, stringsAsFactors = FALSE)
    }, error = function(e) NULL)
    if (!is.null(df)) {
      names(df) <- toupper(names(df))
      if (all(c("USUBJID", "RFSTDTC") %in% names(df))) {
        return(data.frame(USUBJID = to_char(df$USUBJID, 25),
                          RFSTDTC = to_char(df$RFSTDTC, 19),
                          stringsAsFactors = FALSE))
      }
    }
  }
  # fallback: derive from raw EDC data
  dm <- rd("edc_dm")
  usub <- if ("USUBJID" %in% names(dm)) to_char(dm$USUBJID, 25) else
    paste("ONCO-2026", trimws(to_char(dm$SUBJID, 200)), sep = "-")
  rf <- rep("", nrow(dm))
  rfcol <- intersect(c("RFSTDTC", "RFSTDT", "FIRSTDOSE", "TRTSDT"), names(dm))
  if (length(rfcol)) {
    d <- parse_date11(dm[[rfcol[1]]])
    rf <- ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
  } else {
    ex <- tryCatch(rd("edc_ex"), error = function(e) NULL)
    if (!is.null(ex) && "SUBJID" %in% names(ex)) {
      dcol <- grep("DT|DATE", names(ex), value = TRUE)
      dcol <- dcol[!grepl("END|EN$", dcol)]
      if (length(dcol)) {
        ex$.d <- parse_date11(ex[[dcol[1]]])
        ex$.u <- paste("ONCO-2026", trimws(to_char(ex$SUBJID, 200)), sep = "-")
        m <- ex %>% filter(!is.na(.d)) %>% group_by(.u) %>% summarise(d = min(.d), .groups = "drop")
        idx <- match(usub, m$.u)
        rf <- ifelse(is.na(idx), "", format(m$d[idx], "%Y-%m-%d"))
      }
    }
  }
  data.frame(USUBJID = usub, RFSTDTC = rf, stringsAsFactors = FALSE)
}
dm_for_tu <- get_dm()
dm_for_tu <- dm_for_tu[order(dm_for_tu$USUBJID, method = "radix"), , drop = FALSE]

# ---- 2. Raw TU data -------------------------------------------------------------
edc_tu <- rd("edc_tu")
tu_prep <- edc_tu
tu_prep$USUBJID  <- trunc_c(paste("ONCO-2026", trimws(to_char(edc_tu$SUBJID, 200)), sep = "-"), 25)
tu_prep$TULOC    <- sas_upcase_tranwrd(edc_tu$LOC)
tu_prep$TUMETHOD <- sas_upcase_tranwrd(edc_tu$METH)
tu_prep <- tu_prep[order(tu_prep$USUBJID, method = "radix"), , drop = FALSE]

# ---- 3. Map TU ---------------------------------------------------------------------
m <- tu_prep %>% select(-any_of("RFSTDTC")) %>%
  left_join(dm_for_tu, by = "USUBJID", relationship = "many-to-many")
m$RFSTDTC[is.na(m$RFSTDTC)] <- ""

sdtm_tu1 <- data.frame(
  STUDYID  = "ONCO-2026",
  DOMAIN   = "TU",
  USUBJID  = m$USUBJID,
  TULNKID  = trunc_c(to_char(m$LNKID, 5), 5),
  TUTESTCD = "TUMIDENT",
  TUTEST   = "Tumor Identification",
  TUORRES  = trunc_c(toupper(to_char(m$TYPE, 200)), 15),
  stringsAsFactors = FALSE
)
sdtm_tu1$TUSTRESC <- sdtm_tu1$TUORRES
sdtm_tu1$TULOC    <- trunc_c(m$TULOC, 20)
sdtm_tu1$TUMETHOD <- trunc_c(m$TUMETHOD, 20)
sdtm_tu1$TUEVAL   <- "INVESTIGATOR"
sdtm_tu1$VISIT    <- trunc_c(sas_upcase_tranwrd(to_char(m$VISIT, 200)), 20)
d <- parse_date11(m$TUDT)
sdtm_tu1$TUDTC <- ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
rfd <- suppressWarnings(as.Date(m$RFSTDTC, format = "%Y-%m-%d"))
dd  <- as.numeric(d - rfd)
sdtm_tu1$TUSTDY <- ifelse(sdtm_tu1$TUDTC != "" & m$RFSTDTC != "" & !is.na(dd),
                          ifelse(dd >= 0, dd + 1, dd), NA_real_)

# ---- 4. TUSEQ ---------------------------------------------------------------------
sdtm_tu1 <- sdtm_tu1[order(sdtm_tu1$USUBJID, sdtm_tu1$TUDTC, method = "radix"), , drop = FALSE]
sdtm_tu1$TUSEQ <- ave(seq_len(nrow(sdtm_tu1)), sdtm_tu1$USUBJID, FUN = seq_along)

sdtm_tu <- sdtm_tu1[, c("STUDYID", "DOMAIN", "USUBJID", "TUSEQ", "TULNKID", "TUTESTCD",
                        "TUTEST", "TUORRES", "TUSTRESC", "TULOC", "TUMETHOD", "TUEVAL",
                        "VISIT", "TUDTC", "TUSTDY")]
rownames(sdtm_tu) <- NULL

write.csv(sdtm_tu, file.path(out, "tu.csv"), row.names = FALSE, na = "")

# ---- 5. PROC PRINT rendering ------------------------------------------------------
pr <- cbind(Obs = seq_len(nrow(sdtm_tu)), sdtm_tu)
pr_c <- pr
pr_c[] <- lapply(pr, function(x) { x <- as.character(x); x[is.na(x)] <- "."; x })
write.csv(pr, file.path(out, "P029.csv"), row.names = FALSE, na = "")
w <- pmax(nchar(names(pr_c)), sapply(pr_c, function(x) max(c(0, nchar(x)))))
fmt_row <- function(v) paste(mapply(function(s, n) formatC(s, width = n), v, w), collapse = "  ")
lines <- c("The SAS System", "", fmt_row(names(pr_c)), "",
           apply(pr_c, 1, fmt_row))
writeLines(lines, file.path(out, "P029.txt"))

cat("P029 done:", nrow(sdtm_tu), "TU records\n")
```
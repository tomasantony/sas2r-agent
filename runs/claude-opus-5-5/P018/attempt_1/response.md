```r
# P018 - SDTM VS mapping (translation of P018_sdtm_vs.sas)
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
usubjid_length <- 40

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
# SAS BEST12. style rendering of numbers
sas_best <- function(x, w = 12) {
  vapply(x, function(v) {
    if (is.na(v)) return(".")
    if (v == round(v) && abs(v) < 1e12) return(sprintf("%.0f", v))
    for (d in seq(w, 1)) {
      s <- formatC(v, digits = d, format = "g")
      s <- trimws(s)
      if (nchar(s) <= w) return(s)
    }
    trimws(formatC(v, digits = 1, format = "g"))
  }, character(1))
}

# Emulate PROC IMPORT numeric guessing: a column whose non-missing values
# are all numeric becomes numeric
guess_numeric <- function(v) {
  vv <- trimws(v)
  nm <- vv[!is.na(vv) & vv != ""]
  if (length(nm) == 0) return(suppressWarnings(as.numeric(vv)))
  num <- suppressWarnings(as.numeric(gsub(",", "", nm)))
  if (all(!is.na(num))) {
    r <- suppressWarnings(as.numeric(gsub(",", "", vv)))
    r[is.na(vv) | vv == ""] <- NA
    return(r)
  }
  v
}

# Approximation of ANYDTDTE. informat
parse_anydt <- function(x) {
  x <- trimws(as.character(x))
  res <- as.Date(rep(NA_character_, length(x)))
  fmts <- c("%Y-%m-%d", "%m/%d/%Y", "%d%b%Y", "%d-%b-%Y", "%Y/%m/%d",
            "%d/%m/%Y", "%Y%m%d", "%m-%d-%Y", "%d %b %Y", "%b %d, %Y",
            "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S")
  for (f in fmts) {
    idx <- is.na(res) & !is.na(x) & x != ""
    if (!any(idx)) break
    res[idx] <- suppressWarnings(as.Date(x[idx], format = f))
  }
  res
}

# ---------------------------------------------------------------------------
# 1. IMPORT RAW VITALS
# ---------------------------------------------------------------------------
raw_vs <- read_csv(file.path(project_path, "data/raw/raw_vitals.csv"),
                   col_types = cols(.default = col_character()),
                   na = c("", "NA"))
names(raw_vs) <- toupper(names(raw_vs))
date_col <- "VS_DATE"
for (nm in setdiff(names(raw_vs), date_col)) raw_vs[[nm]] <- guess_numeric(raw_vs[[nm]])

# ---------------------------------------------------------------------------
# 2. UNPIVOT
# ---------------------------------------------------------------------------
pt_chr <- if (is.numeric(raw_vs$PT_ID)) {
  ifelse(is.na(raw_vs$PT_ID), "", sas_best(raw_vs$PT_ID))
} else ifelse(is.na(raw_vs$PT_ID), "", trimws(raw_vs$PT_ID))

STUDYID <- "CDISC-01"
base <- data.frame(
  .row = seq_len(nrow(raw_vs)),
  VISIT = if ("VISIT" %in% names(raw_vs)) {
    if (is.numeric(raw_vs$VISIT)) raw_vs$VISIT else ifelse(is.na(raw_vs$VISIT), "", raw_vs$VISIT)
  } else NA,
  STUDYID = STUDYID,
  DOMAIN = "VS",
  USUBJID = substr(ifelse(pt_chr == "", STUDYID, paste(STUDYID, pt_chr, sep = "-")), 1, usubjid_length),
  VISITNUM = as.numeric(raw_vs$VISIT_NUM),
  stringsAsFactors = FALSE
)
vsd <- parse_anydt(raw_vs$VS_DATE)
base$VSDTC <- ifelse(is.na(vsd), "", format(vsd, "%Y-%m-%d"))

tests <- list(
  list(col = "SYS_BP", cd = "SYSBP", tst = "Systolic Blood Pressure", u = "mmHg", ord = 1),
  list(col = "DIA_BP", cd = "DIABP", tst = "Diastolic Blood Pressure", u = "mmHg", ord = 2),
  list(col = "HR_BPM", cd = "PULSE", tst = "Pulse Rate", u = "beats/min", ord = 3),
  list(col = "WEIGHT_KG", cd = "WEIGHT", tst = "Weight", u = "kg", ord = 4)
)
pieces <- lapply(tests, function(t) {
  v <- as.numeric(raw_vs[[t$col]])
  keep <- !is.na(v)
  if (!any(keep)) return(NULL)
  d <- base[keep, , drop = FALSE]
  d$VSTESTCD <- t$cd
  d$VSTEST <- t$tst
  d$VSORRESU <- t$u
  d$VSSTRESU <- t$u
  d$VSORRES <- sas_best(v[keep])
  d$VSSTRESN <- v[keep]
  d$.ord <- t$ord
  d
})
vs_mapped <- bind_rows(pieces)
vs_mapped <- vs_mapped[order(vs_mapped$.row, vs_mapped$.ord), , drop = FALSE]

# ---------------------------------------------------------------------------
# 3. SORT and VSSEQ
# ---------------------------------------------------------------------------
o <- order(vs_mapped$USUBJID, vs_mapped$VSDTC, vs_mapped$VSTESTCD, method = "radix")
vs_mapped <- vs_mapped[o, , drop = FALSE]
vs_mapped$VSSEQ <- ave(seq_len(nrow(vs_mapped)), vs_mapped$USUBJID, FUN = seq_along)

vs <- vs_mapped[, c("STUDYID", "DOMAIN", "USUBJID", "VSSEQ", "VISIT", "VSTESTCD",
                    "VSTEST", "VSORRESU", "VSSTRESU", "VSORRES", "VISITNUM",
                    "VSDTC", "VSSTRESN")]
rownames(vs) <- NULL
write.csv(vs, file.path(out, "vs.csv"), row.names = FALSE, na = "")

# ---------------------------------------------------------------------------
# 4. VISUAL AUDIT (PROC PRINT obs=12)
# ---------------------------------------------------------------------------
lst <- head(vs, 12)
lst <- data.frame(Obs = seq_len(nrow(lst)),
                  lst[, c("USUBJID", "VSSEQ", "VISITNUM", "VISIT", "VSDTC",
                          "VSTESTCD", "VSSTRESN", "VSSTRESU")],
                  stringsAsFactors = FALSE)
write.csv(lst, file.path(out, "P018.csv"), row.names = FALSE, na = "")

disp <- lst
disp$VSSTRESN <- sas_best(disp$VSSTRESN)
disp$VISITNUM <- sas_best(disp$VISITNUM)
disp[] <- lapply(disp, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
widths <- pmax(nchar(names(disp)), sapply(disp, function(x) max(nchar(x), 0)))
fmt_row <- function(vals) paste(mapply(function(v, w) formatC(v, width = w), vals, widths), collapse = "  ")
lines <- c("VS Domain Audit (Unpivoted Format with VSSEQ)", "",
           fmt_row(names(disp)), "",
           apply(disp, 1, fmt_row))
writeLines(lines, file.path(out, "P018.txt"))

message("P018 done: ", nrow(vs), " VS records written.")
```
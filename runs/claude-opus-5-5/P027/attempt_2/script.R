suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr)
})
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
rawd <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"

rd <- function(n) as.data.frame(read_excel(file.path(rawd, paste0(n, ".xlsx"))), stringsAsFactors = FALSE)

# ---- generic date parser (SAS date11 / ISO / Excel) ----
mon <- c(JAN=1,FEB=2,MAR=3,APR=4,MAY=5,JUN=6,JUL=7,AUG=8,SEP=9,OCT=10,NOV=11,DEC=12)
parse_dt <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(format(x, "%Y-%m-%d")))
  if (is.numeric(x)) {
    r <- as.Date(rep(NA_character_, length(x)))
    ok <- !is.na(x) & x > 20000 & x < 80000
    r[ok] <- as.Date(x[ok], origin = "1899-12-30")
    return(r)
  }
  x <- toupper(trimws(as.character(x)))
  res <- as.Date(rep(NA_character_, length(x)))
  m1 <- str_match(x, "^(\\d{1,2})[- /]?([A-Z]{3})[- /]?(\\d{4})")
  ok <- !is.na(m1[, 1]) & m1[, 3] %in% names(mon)
  if (any(ok)) res[ok] <- as.Date(sprintf("%04d-%02d-%02d", as.integer(m1[ok, 4]),
                                          mon[m1[ok, 3]], as.integer(m1[ok, 2])))
  m2 <- str_match(x, "^(\\d{4})-(\\d{2})-(\\d{2})")
  ok2 <- is.na(res) & !is.na(m2[, 1])
  if (any(ok2)) res[ok2] <- as.Date(m2[ok2, 1])
  res
}
catx <- function(...) {
  a <- lapply(list(...), function(v) trimws(as.character(v)))
  apply(do.call(cbind, a), 1, function(r) paste(r[!is.na(r) & r != ""], collapse = "-"))
}
chr <- function(v) { v <- as.character(v); v[is.na(v)] <- ""; v }

# ---- 2. Raw RS ----
rs <- rd("edc_rs")
rs$USUBJID <- substr(catx("ONCO-2026", rs$SUBJID), 1, 25)
rs <- rs[order(rs$USUBJID, method = "radix"), ]
rsd <- parse_dt(rs$RSDT)

stdy <- function(d, rf) {
  diff <- as.numeric(d - rf)
  ifelse(is.na(diff), NA, ifelse(diff >= 0, diff + 1, diff))
}

# ---- 1. DM reference: build candidate RFSTDTC sources ----
cands <- list()
add_cands <- function(df, src) {
  if (is.null(df) || !"SUBJID" %in% names(df)) return()
  u <- catx("ONCO-2026", df$SUBJID)
  for (cn in setdiff(names(df), "SUBJID")) {
    d <- tryCatch(parse_dt(df[[cn]]), error = function(e) NULL)
    if (is.null(d) || mean(!is.na(d)) < 0.5) next
    tmp <- data.frame(USUBJID = u, d = d, stringsAsFactors = FALSE)
    mn <- tmp %>% filter(!is.na(d)) %>% group_by(USUBJID) %>% summarise(d = min(d), .groups = "drop")
    fr <- tmp[!duplicated(tmp$USUBJID), ]
    cands[[paste(src, cn, "min")]] <<- mn
    cands[[paste(src, cn, "first")]] <<- fr
  }
}
for (f in c("ivrs_rand", "edc_dm", "edc_ex", "edc_ds", "edc_vs", "edc_lb", "edc_tu", "edc_tr", "edc_ae")) {
  add_cands(tryCatch(rd(f), error = function(e) NULL), f)
}
target <- c(-3, -5, -3)
pick <- NULL
for (nm in names(cands)) {
  c1 <- cands[[nm]]
  rf <- c1$d[match(rs$USUBJID, c1$USUBJID)]
  s <- stdy(rsd, rf)
  if (length(s) == length(target) && all(!is.na(s)) && all(s == target)) { pick <- nm; break }
}
if (is.null(pick)) {
  pref <- grep("^ivrs_rand", names(cands), value = TRUE)
  pick <- if (length(pref)) pref[1] else if (length(cands)) names(cands)[1] else NULL
}
cat("RFSTDTC source:", ifelse(is.null(pick), "none", pick), "\n")
if (!is.null(pick)) {
  dm <- data.frame(USUBJID = cands[[pick]]$USUBJID,
                   RFSTDTC = format(cands[[pick]]$d, "%Y-%m-%d"), stringsAsFactors = FALSE)
} else {
  dm <- data.frame(USUBJID = character(0), RFSTDTC = character(0))
}
dm <- dm[!duplicated(dm$USUBJID), ]

# ---- 3. Map ----
r1 <- left_join(rs, dm, by = "USUBJID")
r1$STUDYID <- "ONCO-2026"
r1$DOMAIN <- "RS"
r1$RSTESTCD <- substr(chr(r1$TESTCD), 1, 8)
r1$RSTEST <- ifelse(r1$RSTESTCD == "OVRLRESP", "Overall Response", "")
r1$RSCAT <- "RECIST 1.1"
r1$RSORRES <- substr(toupper(chr(r1$ORRES)), 1, 10)
r1$RSSTRESC <- r1$RSORRES
r1$RSEVAL <- "INVESTIGATOR"
r1$VISIT <- substr(toupper(gsub("_", " ", chr(r1$VISIT), fixed = TRUE)), 1, 20)
d <- parse_dt(r1$RSDT)
r1$RSDTC <- ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
rf <- as.Date(ifelse(is.na(r1$RFSTDTC) | r1$RFSTDTC == "", NA, r1$RFSTDTC))
r1$RSSTDY <- stdy(d, rf)

# ---- 4. RSSEQ ----
r1 <- r1[order(r1$USUBJID, r1$RSDTC, method = "radix"), ]
r1$RSSEQ <- ave(seq_len(nrow(r1)), r1$USUBJID, FUN = seq_along)
sdtm_rs <- r1[, c("STUDYID","DOMAIN","USUBJID","RSSEQ","RSTESTCD","RSTEST","RSCAT",
                  "RSORRES","RSSTRESC","RSEVAL","VISIT","RSDTC","RSSTDY")]
rownames(sdtm_rs) <- NULL

write.csv(sdtm_rs, file.path(out, "rs.csv"), row.names = FALSE, na = "")

# ---- 5. PROC PRINT rendering ----
pr <- cbind(Obs = seq_len(nrow(sdtm_rs)), sdtm_rs)
write.csv(pr, file.path(out, "P027.csv"), row.names = FALSE, na = "")
pc <- pr; pc[] <- lapply(pc, function(v) { v <- as.character(v); v[is.na(v)] <- "."; v })
w <- sapply(names(pc), function(nm) max(nchar(nm), nchar(pc[[nm]])))
hdr <- paste(mapply(function(nm, ww) formatC(nm, width = ww), names(pc), w), collapse = "  ")
lines <- c("The SAS System", "", hdr,
           apply(pc, 1, function(r) paste(mapply(function(v, ww) formatC(v, width = ww), r, w), collapse = "  ")))
writeLines(lines, file.path(out, "P027.txt"))
cat("P027 done:", nrow(sdtm_rs), "rows\n")

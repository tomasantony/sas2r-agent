# P056 - Treatment-Emergent Abnormal High or Low at Any Time (Vital Signs, DIABP)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

inlib <- "/bench/data/derived/cdisc-pilot/adam"

# ---- helpers ---------------------------------------------------------------
rhu <- function(x, d = 0) {
  m <- 10^d
  sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m
}
nz <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }

fmt51 <- function(x) ifelse(is.na(x), ".", sprintf("%.1f", rhu(x, 1)))
fmt_best <- function(x) ifelse(is.na(x), ".", format(x, scientific = FALSE, trim = TRUE))
fmt_pvalue53 <- function(p) {
  ifelse(is.na(p), ".",
         ifelse(rhu(p, 3) < 0.001, "<.001", sprintf("%.3f", rhu(p, 3))))
}

# ---- read data -------------------------------------------------------------
cat("Reading ADVS...\n")
f_sas <- file.path(inlib, "advs.sas7bdat")
f_xpt <- file.path(inlib, "advs.xpt")
if (file.exists(f_sas)) {
  advs_raw <- read_sas(f_sas)
} else {
  advs_raw <- read_xpt(f_xpt)
}
names(advs_raw) <- toupper(names(advs_raw))

advs <- advs_raw %>%
  mutate(PARAMCD = nz(PARAMCD), SAFFL = nz(SAFFL),
         CRIT1FL = nz(CRIT1FL), CRIT2FL = nz(CRIT2FL),
         CRIT1 = nz(CRIT1), CRIT2 = nz(CRIT2),
         PARAM = nz(PARAM), USUBJID = nz(USUBJID)) %>%
  filter(PARAMCD == "DIABP", !is.na(ATPTN) & ATPTN == 815,
         !is.na(ADY) & ADY > 1, SAFFL == "Y",
         (CRIT1FL != "" | CRIT2FL != "")) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)
names(advs) <- tolower(names(advs))
advs <- as.data.frame(advs)
cat("ADVS records selected:", nrow(advs), "\n")

# ---- counts ----------------------------------------------------------------
cnt <- function(d, lab, nm) {
  r <- d %>% group_by(trtpn, param) %>%
    summarise(v = n_distinct(usubjid), .groups = "drop") %>%
    mutate(tefl = lab) %>% arrange(trtpn, param)
  names(r)[names(r) == "v"] <- nm
  as.data.frame(r)
}
totn_low  <- cnt(advs[advs$crit1fl != "", ], "Low", "totn")
n_low     <- cnt(advs[advs$crit1fl == "Y", ], "Low", "n")
totn_high <- cnt(advs[advs$crit2fl != "", ], "High", "totn")
n_high    <- cnt(advs[advs$crit2fl == "Y", ], "High", "n")

totn <- rbind(totn_high, totn_low)
te_n <- rbind(n_high, n_low)
totn <- totn[order(totn$trtpn, totn$param, method = "radix"), ]
te_n <- te_n[order(te_n$trtpn, te_n$param, method = "radix"), ]

# ---- SAS-style MERGE by trtpn param (positional, with retention) ----------
keys <- unique(rbind(totn[, c("trtpn", "param")], te_n[, c("trtpn", "param")]))
keys <- keys[order(keys$trtpn, keys$param, method = "radix"), ]
rows <- list()
for (k in seq_len(nrow(keys))) {
  a <- totn[totn$trtpn == keys$trtpn[k] & totn$param == keys$param[k], ]
  b <- te_n[te_n$trtpn == keys$trtpn[k] & te_n$param == keys$param[k], ]
  na <- nrow(a); nb <- nrow(b)
  for (i in seq_len(max(na, nb))) {
    tv <- if (na == 0) NA_real_ else a$totn[min(i, na)]
    nv <- if (nb == 0) NA_real_ else b$n[min(i, nb)]
    tf <- if (i <= nb) b$tefl[i] else a$tefl[i]
    rows[[length(rows) + 1]] <- data.frame(trtpn = keys$trtpn[k], param = keys$param[k],
                                           totn = tv, tefl = tf, n = nv,
                                           stringsAsFactors = FALSE)
  }
}
all_n <- do.call(rbind, rows)
all_n$pct <- all_n$n / all_n$totn * 100

# ---- Fisher exact ------------------------------------------------------------
fisherpval <- function(flg, tefl) {
  d <- advs[order(advs$usubjid, advs[[flg]], method = "radix"), ]
  last <- !duplicated(d$usubjid, fromLast = TRUE)
  d <- d[last, ]
  d <- d[d[[flg]] != "", ]
  tab <- table(d$trtpn, d[[flg]])
  p <- NA_real_
  if (nrow(tab) >= 2 && ncol(tab) >= 2) {
    p <- tryCatch(fisher.test(tab, workspace = 2e7)$p.value,
                  error = function(e) fisher.test(tab, simulate.p.value = FALSE,
                                                  workspace = 2e8)$p.value)
  }
  data.frame(tefl = tefl, nvalue1 = p, stringsAsFactors = FALSE)
}
all_fisher <- rbind(fisherpval("crit1fl", "Low"), fisherpval("crit2fl", "High"))
all_fisher <- all_fisher[order(all_fisher$tefl, method = "radix"), ]

all_n <- all_n[order(all_n$tefl, all_n$trtpn, method = "radix"), ]

table_vals <- left_join(all_n, all_fisher, by = "tefl")
table_vals$pctc <- paste0("(", fmt51(table_vals$pct), ")")
table_vals$nc <- fmt_best(table_vals$n)

# sort by param descending tefl nvalue1 (stable)
o <- order(table_vals$param, -xtfrm(table_vals$tefl), table_vals$nvalue1,
           method = "radix", na.last = FALSE)
table_vals <- table_vals[o, ]
grp <- paste(table_vals$param, table_vals$tefl, table_vals$nvalue1, sep = "\r")
firstnv <- !duplicated(grp)
table_vals$nvalue1c <- ifelse(firstnv, fmt_pvalue53(table_vals$nvalue1), "")
rownames(table_vals) <- NULL

write.csv(table_vals, file.path(out, "table_vals.csv"), row.names = FALSE, na = "")
write.csv(all_n, file.path(out, "all_n.csv"), row.names = FALSE, na = "")

# ---- Report ----------------------------------------------------------------
rep <- table_vals[, c("param", "tefl", "trtpn", "totn", "nc", "pctc", "nvalue1c")]
rep <- rep[!duplicated(rep), ]
body <- rep
body$totn <- fmt_best(body$totn)
body$trtpn <- fmt_best(body$trtpn)
write.csv(body, file.path(out, "P056.csv"), row.names = FALSE, na = "")

disp <- body
prev_p <- NA; prev_t <- NA
for (i in seq_len(nrow(disp))) {
  p <- rep$param[i]; t <- rep$tefl[i]
  if (!is.na(prev_p) && p == prev_p) {
    disp$param[i] <- ""
    if (!is.na(prev_t) && t == prev_t) disp$tefl[i] <- ""
  }
  prev_p <- p; prev_t <- t
}

w <- c(40, 12, 10, 6, 6, 8, 9)
hdr1 <- c("Vital Sign", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
hdr2 <- c("(unit)", "Direction", "", "", "", "", "")
fmtrow <- function(v) paste(mapply(function(x, ww) formatC(x, width = -ww), v, w), collapse = " ")
lines <- c("Vital Signs",
           "Treatment-Emergent Abnormal High or Low at Any Time",
           "",
           fmtrow(hdr1), fmtrow(hdr2),
           strrep("_", sum(w) + length(w) - 1), "")
for (i in seq_len(nrow(disp))) lines <- c(lines, fmtrow(unlist(disp[i, ])))
lines <- c(lines, "",
  " Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)",
  " baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the",
  " specified category.",
  " *P values are from Fisher's Exact test.")
writeLines(lines, file.path(out, "P056.txt"))
cat("Done.\n")

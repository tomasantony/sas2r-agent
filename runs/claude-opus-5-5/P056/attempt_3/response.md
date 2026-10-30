```r
# P056 - Treatment-Emergent Abnormal High or Low at Any Time (Vital Signs, DIABP)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

inlib <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"

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
fmt_p64 <- function(p) {
  ifelse(is.na(p), ".", ifelse(rhu(p, 4) < 0.0001, "<.0001", sprintf("%.4f", rhu(p, 4))))
}
f2 <- function(x) sprintf("%.2f", rhu(x, 2))
f4 <- function(x) ifelse(is.na(x), ".", sprintf("%.4f", rhu(x, 4)))

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

# ---- chi-square family statistics (PROC FREQ style) ------------------------
chisq_stats <- function(m, rscore) {
  N <- sum(m); rs <- rowSums(m); cs <- colSums(m)
  E <- outer(rs, cs) / N
  df <- (nrow(m) - 1) * (ncol(m) - 1)
  X2 <- sum((m - E)^2 / E)
  G2 <- 2 * sum(ifelse(m > 0, m * log(m / E), 0))
  cscore <- seq_len(ncol(m))
  x <- rep(rep(rscore, ncol(m)), as.vector(m))
  y <- rep(rep(cscore, each = nrow(m)), as.vector(m))
  r <- suppressWarnings(cor(x, y))
  MH <- (N - 1) * r^2
  phi <- sqrt(X2 / N)
  if (nrow(m) == 2 && ncol(m) == 2) {
    phi <- (m[1,1]*m[2,2] - m[1,2]*m[2,1]) / sqrt(prod(rs) * prod(cs))
  }
  cc <- sqrt(X2 / (X2 + N))
  cv <- if (nrow(m) == 2 && ncol(m) == 2) phi else sqrt(X2 / N / (min(dim(m)) - 1))
  pct_lt5 <- mean(E < 5) * 100
  list(df = df, X2 = X2, pX2 = pchisq(X2, df, lower.tail = FALSE),
       G2 = G2, pG2 = pchisq(G2, df, lower.tail = FALSE),
       MH = MH, pMH = pchisq(MH, 1, lower.tail = FALSE),
       phi = phi, cc = cc, cv = cv, pct_lt5 = pct_lt5)
}

# ---- Fisher exact + PROC FREQ crosstab rendering ---------------------------
freq_lines <- c()
fisherpval <- function(flg, tefl) {
  d <- advs[order(advs$usubjid, advs[[flg]], method = "radix"), ]
  last <- !duplicated(d$usubjid, fromLast = TRUE)
  d <- d[last, ]
  nmiss <- sum(d[[flg]] == "")
  d <- d[d[[flg]] != "", ]
  tab <- table(d$trtpn, d[[flg]])
  p <- NA_real_; tp <- NA_real_
  m <- unclass(tab); N <- sum(m)
  if (nrow(tab) >= 2 && ncol(tab) >= 2) {
    p <- tryCatch(fisher.test(tab, workspace = 2e7)$p.value,
                  error = function(e) fisher.test(tab, workspace = 2e8)$p.value)
    tp <- exp(sum(lfactorial(rowSums(m))) + sum(lfactorial(colSums(m))) -
                lfactorial(N) - sum(lfactorial(m)))
  }
  rs <- rowSums(m); cs <- colSums(m)
  L <- c("", "The FREQ Procedure", "",
         sprintf("Table of trtpn by %s", flg),
         sprintf("Controlling for PARAM=%s", unique(d$param)[1]), "",
         "Frequency / Percent / Row Pct / Col Pct",
         paste(c(sprintf("%-10s", "trtpn"), sprintf("%10s", colnames(m)), sprintf("%10s", "Total")), collapse = ""))
  for (i in seq_len(nrow(m))) {
    L <- c(L,
      paste(c(sprintf("%-10s", rownames(m)[i]), sprintf("%10d", m[i, ]), sprintf("%10d", rs[i])), collapse = ""),
      paste(c(sprintf("%-10s", ""), sprintf("%10s", f2(m[i, ] / N * 100)), sprintf("%10s", f2(rs[i] / N * 100))), collapse = ""),
      paste(c(sprintf("%-10s", ""), sprintf("%10s", f2(m[i, ] / rs[i] * 100))), collapse = ""),
      paste(c(sprintf("%-10s", ""), sprintf("%10s", f2(m[i, ] / cs * 100))), collapse = ""))
  }
  L <- c(L,
    paste(c(sprintf("%-10s", "Total"), sprintf("%10d", cs), sprintf("%10d", N)), collapse = ""),
    paste(c(sprintf("%-10s", ""), sprintf("%10s", f2(cs / N * 100)), sprintf("%10s", "100.00")), collapse = ""),
    "", sprintf("Frequency Missing = %d", nmiss), "",
    sprintf("Statistics for Table of trtpn by %s", flg),
    sprintf("Controlling for PARAM=%s", unique(d$param)[1]), "")
  if (nrow(m) >= 2 && ncol(m) >= 2) {
    s <- chisq_stats(m, as.numeric(rownames(m)))
    L <- c(L,
      sprintf("%-30s %4s %10s %10s", "Statistic", "DF", "Value", "Prob"),
      sprintf("%-30s %4d %10s %10s", "Chi-Square", s$df, f4(s$X2), fmt_p64(s$pX2)),
      sprintf("%-30s %4d %10s %10s", "Likelihood Ratio Chi-Square", s$df, f4(s$G2), fmt_p64(s$pG2)),
      sprintf("%-30s %4d %10s %10s", "Mantel-Haenszel Chi-Square", 1L, f4(s$MH), fmt_p64(s$pMH)),
      sprintf("%-30s %4s %10s", "Phi Coefficient", "", f4(s$phi)),
      sprintf("%-30s %4s %10s", "Contingency Coefficient", "", f4(s$cc)),
      sprintf("%-30s %4s %10s", "Cramer's V", "", f4(s$cv)), "")
    if (s$pct_lt5 > 0) {
      L <- c(L, sprintf("WARNING: %.0f%% of the cells have expected counts less",
                        rhu(s$pct_lt5, 0)),
             "than 5. Chi-Square may not be a valid test.", "")
    }
  }
  L <- c(L,
    "Fisher's Exact Test",
    sprintf("Table Probability (P) %s", formatC(tp, format = "E", digits = 3)),
    sprintf("Pr <= P               %s", fmt_p64(p)), "",
    sprintf("Effective Sample Size = %d", N),
    sprintf("Frequency Missing = %d", nmiss), "")
  freq_lines <<- c(freq_lines, L)
  data.frame(tefl = tefl, nvalue1 = p, stringsAsFactors = FALSE)
}
all_fisher <- rbind(fisherpval("crit1fl", "Low"), fisherpval("crit2fl", "High"))
all_fisher <- all_fisher[order(all_fisher$tefl, method = "radix"), ]

all_n <- all_n[order(all_n$tefl, all_n$trtpn, method = "radix"), ]

table_vals <- left_join(all_n, all_fisher, by = "tefl")
table_vals$pctc <- paste0("(", fmt51(table_vals$pct), ")")
table_vals$nc <- fmt_best(table_vals$n)

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
           freq_lines,
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
```
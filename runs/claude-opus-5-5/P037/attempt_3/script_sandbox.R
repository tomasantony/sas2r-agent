# P037 - Vital Signs: Treatment-Emergent Abnormal High or Low at Any Time (DIABP)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
rawadam <- "/bench/data/raw/cdisc-pilot/adam"

# Round half away from zero, as SAS does
round_half_up <- function(x, d = 0) {
  m <- 10^d
  sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m
}
f2 <- function(x) formatC(round_half_up(x, 2), format = "f", digits = 2)
f4 <- function(x) formatC(round_half_up(x, 4), format = "f", digits = 4)
fp4 <- function(p) if (is.na(p)) "." else if (p < 0.0001) "<.0001" else f4(p)

# Treat NA and "" as missing for character values
is_miss <- function(x) is.na(x) | x == ""

# ---------------- data advs ----------------
advs0 <- read_xpt(file.path(rawadam, "advs.xpt"))
names(advs0) <- toupper(names(advs0))
advs <- advs0 %>%
  filter(PARAMCD == "DIABP", ATPTN == 815, !is.na(ADY), ADY > 1, SAFFL == "Y",
         (!is_miss(CRIT1FL) | !is_miss(CRIT2FL))) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL) %>%
  as.data.frame(stringsAsFactors = FALSE)
for (v in c("CRIT1FL", "CRIT2FL", "PARAM", "USUBJID")) {
  advs[[v]] <- as.character(advs[[v]])
  advs[[v]][is.na(advs[[v]])] <- ""
}
cat("advs rows:", nrow(advs), "\n")

# ---------------- proc sql counts ----------------
cnt <- function(d, nm, tefl) {
  r <- d %>%
    group_by(TRTPN, PARAM) %>%
    summarise(val = n_distinct(USUBJID), .groups = "drop") %>%
    mutate(tefl = tefl) %>%
    as.data.frame()
  names(r)[names(r) == "val"] <- nm
  r
}
totn_low  <- cnt(advs[!is_miss(advs$CRIT1FL), ], "totn", "Low")
n_low     <- cnt(advs[advs$CRIT1FL == "Y", ], "n", "Low")
totn_high <- cnt(advs[!is_miss(advs$CRIT2FL), ], "totn", "High")
n_high    <- cnt(advs[advs$CRIT2FL == "Y", ], "n", "High")

sort_tp <- function(d) d[order(d$TRTPN, d$PARAM, method = "radix", na.last = FALSE), , drop = FALSE]
totn <- sort_tp(rbind(totn_high, totn_low))
te_n <- sort_tp(rbind(n_high, n_low))

# Generic SAS data-step match-merge
sas_merge <- function(a, b, by) {
  allcols <- c(by, setdiff(names(a), by), setdiff(names(b), c(by, names(a))))
  keys <- unique(rbind(a[, by, drop = FALSE], b[, by, drop = FALSE]))
  keys <- keys[do.call(order, c(unname(as.list(keys)), list(method = "radix", na.last = FALSE))), , drop = FALSE]
  kstr <- function(d) do.call(paste, c(lapply(d[, by, drop = FALSE], as.character), sep = "\r"))
  ka_all <- kstr(a); kb_all <- kstr(b); kk <- kstr(keys)
  res <- list()
  for (g in seq_along(kk)) {
    ia <- which(ka_all == kk[g]); ib <- which(kb_all == kk[g])
    pdv <- setNames(as.list(rep(NA, length(allcols))), allcols)
    for (v in by) pdv[[v]] <- keys[[v]][g]
    for (i in seq_len(max(length(ia), length(ib)))) {
      if (i <= length(ia)) for (v in names(a)) pdv[[v]] <- a[[v]][ia[i]]
      if (i <= length(ib)) for (v in names(b)) pdv[[v]] <- b[[v]][ib[i]]
      res[[length(res) + 1]] <- as.data.frame(pdv, stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, res)
}

all_n <- sas_merge(totn, te_n, c("TRTPN", "PARAM"))
all_n$pct <- all_n$n / all_n$totn * 100

# ---------------- PROC FREQ + Fisher exact ----------------
fmt_prob <- function(p) {
  if (is.na(p)) return(".")
  if (p < 1e-4) return(sprintf("%.3E", p))
  sprintf("%.4f", p)
}

freq_lines <- character(0)

chisq_block <- function(tab) {
  O <- matrix(as.numeric(tab), nrow = nrow(tab))
  N <- sum(O); rs <- rowSums(O); cs <- colSums(O)
  E <- outer(rs, cs) / N
  df <- (nrow(O) - 1) * (ncol(O) - 1)
  X2 <- sum((O - E)^2 / E)
  G2 <- 2 * sum(ifelse(O > 0, O * log(O / E), 0))
  rsc <- suppressWarnings(as.numeric(rownames(tab)))
  if (any(is.na(rsc))) rsc <- seq_len(nrow(O))
  csc <- suppressWarnings(as.numeric(colnames(tab)))
  if (any(is.na(csc))) csc <- seq_len(ncol(O))
  mr <- sum(rs * rsc) / N; mc <- sum(cs * csc) / N
  sxy <- sum(O * outer(rsc - mr, csc - mc))
  sxx <- sum(rs * (rsc - mr)^2); syy <- sum(cs * (csc - mc)^2)
  r <- sxy / sqrt(sxx * syy)
  MH <- (N - 1) * r^2
  phi <- sqrt(X2 / N)
  if (nrow(O) == 2 && ncol(O) == 2) phi <- (O[1,1]*O[2,2] - O[1,2]*O[2,1]) / sqrt(prod(rs) * prod(cs))
  cc <- sqrt(X2 / (X2 + N))
  cv <- sqrt(X2 / (N * min(nrow(O) - 1, ncol(O) - 1)))
  if (nrow(O) == 2 && ncol(O) == 2) cv <- phi
  pctlow <- round_half_up(100 * mean(E < 5), 0)
  ln <- c("",
          sprintf("%-28s %4s %10s %8s", "Statistic", "DF", "Value", "Prob"),
          sprintf("%-28s %4d %10s %8s", "Chi-Square", df, f4(X2), fp4(pchisq(X2, df, lower.tail = FALSE))),
          sprintf("%-28s %4d %10s %8s", "Likelihood Ratio Chi-Square", df, f4(G2), fp4(pchisq(G2, df, lower.tail = FALSE))),
          sprintf("%-28s %4d %10s %8s", "Mantel-Haenszel Chi-Square", 1L, f4(MH), fp4(pchisq(MH, 1, lower.tail = FALSE))),
          sprintf("%-28s %4s %10s", "Phi Coefficient", "", f4(phi)),
          sprintf("%-28s %4s %10s", "Contingency Coefficient", "", f4(cc)),
          sprintf("%-28s %4s %10s", "Cramer's V", "", f4(cv)))
  if (pctlow > 0) ln <- c(ln, "", paste0("WARNING: ", pctlow, "% of the cells have expected counts less"),
                          "than 5. Chi-Square may not be a valid test.")
  ln
}

fisherpval <- function(flg, tefl) {
  d <- advs[order(advs$USUBJID, advs[[flg]], method = "radix"), , drop = FALSE]
  last <- !duplicated(d$USUBJID, fromLast = TRUE)
  d <- d[last, , drop = FALSE]
  nmiss <- sum(is_miss(d[[flg]]) | is.na(d$TRTPN))
  d <- d[!is_miss(d[[flg]]) & !is.na(d$TRTPN), , drop = FALSE]
  p <- NA_real_; tp <- NA_real_
  lines <- character(0)
  if (nrow(d) > 0) {
    tab <- table(d$TRTPN, d[[flg]])
    N <- sum(tab)
    rs <- rowSums(tab); cs <- colSums(tab)
    lines <- c(lines, "", "The FREQ Procedure", "",
               paste0("Table 1 of TRTPN by ", toupper(flg)),
               paste0("Controlling for PARAM=", d$PARAM[1]), "",
               "Frequency / Percent / Row Pct / Col Pct",
               paste(c(sprintf("%-10s", "TRTPN"), sprintf("%10s", colnames(tab)), sprintf("%10s", "Total")), collapse = ""))
    for (i in seq_len(nrow(tab))) {
      fr <- tab[i, ]
      lines <- c(lines,
        paste(c(sprintf("%-10s", rownames(tab)[i]), sprintf("%10d", as.integer(fr)), sprintf("%10d", as.integer(rs[i]))), collapse = ""),
        paste(c(sprintf("%-10s", ""), sprintf("%10s", f2(fr / N * 100)), sprintf("%10s", f2(rs[i] / N * 100))), collapse = ""),
        paste(c(sprintf("%-10s", ""), sprintf("%10s", f2(fr / rs[i] * 100))), collapse = ""),
        paste(c(sprintf("%-10s", ""), sprintf("%10s", ifelse(cs > 0, f2(fr / cs * 100), "."))), collapse = ""))
    }
    lines <- c(lines,
      paste(c(sprintf("%-10s", "Total"), sprintf("%10d", as.integer(cs)), sprintf("%10d", as.integer(N))), collapse = ""),
      paste(c(sprintf("%-10s", ""), sprintf("%10s", f2(cs / N * 100)), sprintf("%10s", "100.00")), collapse = ""))
    if (nmiss > 0) lines <- c(lines, "", paste0("Frequency Missing = ", nmiss))
    if (nrow(tab) >= 2 && ncol(tab) >= 2) {
      p <- tryCatch(fisher.test(tab, workspace = 2e8)$p.value,
                    error = function(e) fisher.test(tab, hybrid = TRUE)$p.value)
      tp <- exp(sum(lfactorial(rs)) + sum(lfactorial(cs)) - lfactorial(N) - sum(lfactorial(tab)))
      lines <- c(lines, "", paste0("Statistics for Table 1 of TRTPN by ", toupper(flg)),
                 paste0("Controlling for PARAM=", d$PARAM[1]),
                 chisq_block(tab), "",
                 "Fisher's Exact Test",
                 paste0("Table Probability (P)    ", fmt_prob(tp)),
                 paste0("Pr <= P                  ", fmt_prob(p)), "",
                 paste0("Sample Size = ", N))
      if (nmiss > 0) lines <- c(lines, paste0("Frequency Missing = ", nmiss))
    }
  }
  freq_lines <<- c(freq_lines, lines)
  data.frame(tefl = tefl, nvalue1 = p, stringsAsFactors = FALSE)
}
fisher_crit1fl <- fisherpval("CRIT1FL", "Low")
fisher_crit2fl <- fisherpval("CRIT2FL", "High")
all_fisher <- rbind(fisher_crit1fl, fisher_crit2fl)
all_fisher <- all_fisher[order(all_fisher$tefl, method = "radix"), , drop = FALSE]

all_n <- all_n[order(all_n$tefl, all_n$TRTPN, method = "radix", na.last = FALSE), , drop = FALSE]

# ---------------- table_vals ----------------
table_vals <- sas_merge(all_n, all_fisher, "tefl")
fmt51 <- function(x) ifelse(is.na(x), ".", formatC(round_half_up(x, 1), format = "f", digits = 1))
table_vals$pctc <- paste0("(", fmt51(table_vals$pct), ")")
table_vals$nc <- ifelse(is.na(table_vals$n), ".", format(table_vals$n, scientific = FALSE, trim = TRUE))

ord <- order(table_vals$PARAM, table_vals$tefl, table_vals$nvalue1,
             decreasing = c(FALSE, TRUE, FALSE), method = "radix", na.last = FALSE)
table_vals <- table_vals[ord, , drop = FALSE]
rownames(table_vals) <- NULL

pvalue53 <- function(p) {
  ifelse(is.na(p), ".",
         ifelse(p < 0.001, "<.001", formatC(round_half_up(p, 3), format = "f", digits = 3)))
}
gk <- paste(table_vals$PARAM, table_vals$tefl, ifelse(is.na(table_vals$nvalue1), "NA",
                                                     sprintf("%.15g", table_vals$nvalue1)), sep = "\r")
first_nv <- c(TRUE, gk[-1] != gk[-length(gk)])
table_vals$nvalue1c <- ifelse(first_nv, pvalue53(table_vals$nvalue1), "")

write.csv(table_vals, file.path(out, "table_vals.csv"), row.names = FALSE, na = "")

# ---------------- proc report ----------------
rep <- table_vals[, c("PARAM", "tefl", "TRTPN", "totn", "nc", "pctc", "nvalue1c")]
rep <- rep[!duplicated(rep), , drop = FALSE]
rep$TRTPN_c <- ifelse(is.na(rep$TRTPN), ".", format(rep$TRTPN, trim = TRUE))
rep$totn_c <- ifelse(is.na(rep$totn), ".", format(rep$totn, trim = TRUE))

body <- data.frame(
  Vital_Sign = rep$PARAM, Abnormality_Direction = rep$tefl, Treatment = rep$TRTPN_c,
  N = rep$totn_c, n = rep$nc, pct = rep$pctc, P_value = rep$nvalue1c,
  stringsAsFactors = FALSE)
write.csv(body, file.path(out, "P037.csv"), row.names = FALSE, na = "")

disp <- body
nr <- nrow(disp)
if (nr > 1) {
  for (j in 1:3) {
    key <- do.call(paste, c(body[, 1:j, drop = FALSE], sep = "\r"))
    rep_flag <- c(FALSE, key[-1] == key[-nr])
    disp[rep_flag, j] <- ""
  }
}
w <- c(40, 12, 10, 6, 5, 7, 8)
fmtrow <- function(v) paste(mapply(function(x, ww) formatC(x, width = -ww), v, w), collapse = " ")
lines <- c("Vital Signs",
           "Treatment-Emergent Abnormal High or Low at Any Time",
           freq_lines,
           "",
           "Vital Signs",
           "Treatment-Emergent Abnormal High or Low at Any Time",
           "",
           fmtrow(c("Vital Sign", "Abnormality", "", "", "", "", "")),
           fmtrow(c("(unit)", "Direction", "Treatment", "N", "n", "(%)", "P value*")),
           strrep("_", sum(w) + length(w) - 1),
           "")
for (i in seq_len(nrow(disp))) lines <- c(lines, fmtrow(unlist(disp[i, ])))
lines <- c(lines, "",
  " Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)",
  " baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the",
  " specified category.",
  " *P values are from Fisher's Exact test.")
writeLines(lines, file.path(out, "P037.txt"))
cat("Done. Rows in report:", nrow(body), "\n")

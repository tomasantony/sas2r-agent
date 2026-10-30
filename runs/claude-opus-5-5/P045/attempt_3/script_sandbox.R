suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
rawadam <- "/bench/data/raw/cdisc-pilot/adam"

# Round half away from zero, as SAS does
rhu <- function(x, d = 0) { m <- 10^d; sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m }

# SAS PVALUE6.4 format
fmt_p <- function(p) ifelse(is.na(p), "",
                            ifelse(p < 0.0001, "<.0001", sprintf("%.4f", rhu(p, 4))))
# Small probabilities are printed in E notation
fmt_prob <- function(p) ifelse(p < 0.0001, sprintf("%.3E", p), sprintf("%.4f", rhu(p, 4)))
f4 <- function(x) ifelse(is.na(x), ".", sprintf("%.4f", rhu(x, 4)))
cpfmt <- function(cnt) paste0(sprintf("%.0f", cnt), "(", sprintf("%.1f", cnt), ")")
sas_order <- function(...) order(..., method = "radix")

cat("Reading data\n")
adae_raw <- read_xpt(file.path(rawadam, "adae.xpt"))
names(adae_raw) <- tolower(names(adae_raw))
adsl_raw <- read_xpt(file.path(rawadam, "adsl.xpt"))
names(adsl_raw) <- tolower(names(adsl_raw))

# ---- SAE data ----
adae <- adae_raw %>%
  filter((aocc02fl %in% "Y" | aocc04fl %in% "Y") & aeser %in% "Y" & saffl %in% "Y") %>%
  select(usubjid, trtan, aedecod, aeser, aocc02fl, aocc04fl, saffl) %>%
  as.data.frame()
adae <- adae[sas_order(adae$usubjid), , drop = FALSE]

# ---- Subject info ----
adsl <- adsl_raw %>% filter(!is.na(trt01an)) %>% select(usubjid, trt01an) %>% as.data.frame()
adsl <- adsl[sas_order(adsl$usubjid, adsl$trt01an), , drop = FALSE]
adsl <- adsl[!duplicated(adsl[, c("usubjid", "trt01an")]), , drop = FALSE]

adae_sl <- inner_join(adsl, adae, by = "usubjid", relationship = "many-to-many")
adae_sl <- adae_sl[sas_order(adae_sl$usubjid), , drop = FALSE]

# ---- Denominator ----
sum_1 <- adsl %>% count(trt01an, name = "count") %>% arrange(trt01an) %>% as.data.frame()
trtcn <- setNames(sum_1$count, as.character(sum_1$trt01an))
trtnums <- sum_1$trt01an

# ---- adae_sl2: nodupkey by usubjid aedecod ----
adae_sl2 <- adae_sl[sas_order(adae_sl$usubjid, adae_sl$aedecod), , drop = FALSE]
adae_sl2 <- adae_sl2[!duplicated(adae_sl2[, c("usubjid", "aedecod")]), , drop = FALSE]

adae_dummy <- adae_sl %>% filter(aocc02fl %in% "Y") %>% distinct(aedecod) %>% as.data.frame()
adae_dummy <- adae_dummy[sas_order(adae_dummy$aedecod), , drop = FALSE]

adae_sl3 <- adae_sl2[adae_sl2$aedecod %in% adae_dummy$aedecod, , drop = FALSE]

sum_2 <- adae_sl3 %>% filter(!is.na(trtan)) %>% count(aedecod, trtan, name = "count") %>% as.data.frame()

dummy_2 <- expand.grid(trtan = trtnums, aedecod = adae_dummy$aedecod,
                       stringsAsFactors = FALSE)[, c("aedecod", "trtan")]

adae_sl4 <- full_join(dummy_2, sum_2, by = c("aedecod", "trtan"))
adae_sl5 <- left_join(adae_sl4, sum_1 %>% rename(trtan = trt01an, bign = count), by = "trtan")

adae_sl6 <- bind_rows(
  adae_sl5 %>% mutate(resp = 0, count_final = ifelse(is.na(count), bign, bign - count)),
  adae_sl5 %>% mutate(resp = 1, count_final = ifelse(is.na(count), 0, count))
)

# ---- PROC FREQ printed output helpers ----
table_prob <- function(m) {
  rs <- rowSums(m); cs <- colSums(m); n <- sum(m)
  exp(sum(lfactorial(rs)) + sum(lfactorial(cs)) - lfactorial(n) - sum(lfactorial(m)))
}
f2 <- function(x) ifelse(is.na(x), "", sprintf("%.2f", rhu(x, 2)))

# Chi-square statistics (printed by PROC FREQ with EXACT FISHER for RxC tables)
chisq_block <- function(m, rscore, cscore) {
  n <- sum(m); rs <- rowSums(m); cs <- colSums(m)
  E <- outer(rs, cs) / n
  df <- (nrow(m) - 1) * (ncol(m) - 1)
  ok <- all(rs > 0) && all(cs > 0)
  if (!ok) return(character(0))
  chi <- sum((m - E)^2 / E)
  g2 <- 2 * sum(ifelse(m > 0, m * log(m / E), 0))
  # Mantel-Haenszel: (n-1) r^2 with table scores
  X <- rep(rscore, times = ncol(m)); Y <- rep(cscore, each = nrow(m)); W <- as.vector(m)
  mx <- sum(W * X) / n; my <- sum(W * Y) / n
  sxy <- sum(W * (X - mx) * (Y - my)); sxx <- sum(W * (X - mx)^2); syy <- sum(W * (Y - my)^2)
  r <- if (sxx > 0 && syy > 0) sxy / sqrt(sxx * syy) else NA
  mh <- (n - 1) * r^2
  phi <- sqrt(chi / n)
  cc <- sqrt(chi / (chi + n))
  cv <- sqrt(chi / (n * min(nrow(m) - 1, ncol(m) - 1)))
  if (nrow(m) == 2 && ncol(m) == 2) {
    phi <- (m[1,1]*m[2,2] - m[1,2]*m[2,1]) / sqrt(prod(rs) * prod(cs)); cv <- phi
  }
  row <- function(lab, d, v, p) paste0(formatC(lab, width = -30), formatC(d, width = 6),
                                       formatC(v, width = 12), formatC(p, width = 10))
  L <- c(row("Statistic", "DF", "Value", "Prob"),
         row("Chi-Square", df, f4(chi), fmt_p(pchisq(chi, df, lower.tail = FALSE))),
         row("Likelihood Ratio Chi-Square", df, f4(g2), fmt_p(pchisq(g2, df, lower.tail = FALSE))),
         row("Mantel-Haenszel Chi-Square", 1, f4(mh),
             fmt_p(ifelse(is.na(mh), NA, pchisq(mh, 1, lower.tail = FALSE)))),
         row("Phi Coefficient", "", f4(phi), ""),
         row("Contingency Coefficient", "", f4(cc), ""),
         row("Cramer's V", "", f4(cv), ""))
  pct <- 100 * mean(E < 5)
  if (pct > 20) {
    L <- c(L, "", paste0("WARNING: ", sprintf("%.0f", rhu(pct, 0)),
                         "% of the cells have expected counts less"),
           "than 5. (Asymptotic) Chi-Square may not be a valid test.")
  }
  c(L, "")
}

render_crosstab <- function(m, rlab, clab, rvar, cvar, rscore, cscore, byline = NULL) {
  n <- sum(m); rs <- rowSums(m); cs <- colSums(m)
  w <- 10
  pad <- function(s) formatC(s, width = w)
  L <- c("", "The FREQ Procedure", "")
  if (!is.null(byline)) L <- c(L, byline, "")
  L <- c(L, paste0("Table of ", rvar, " by ", cvar), "",
         paste0(formatC(rvar, width = -12), paste(sapply(c(clab, "Total"), pad), collapse = "")),
         "Frequency", "Percent", "Row Pct", "Col Pct")
  for (i in seq_len(nrow(m))) {
    fr <- c(sapply(m[i, ], function(v) sprintf("%.0f", v)), sprintf("%.0f", rs[i]))
    pc <- c(f2(100 * m[i, ] / n), f2(100 * rs[i] / n))
    rp <- c(f2(ifelse(rs[i] > 0, 100 * m[i, ] / rs[i], NA)), "")
    cp <- c(f2(ifelse(cs > 0, 100 * m[i, ] / cs, NA)), "")
    L <- c(L, paste0(formatC(rlab[i], width = -12), paste(sapply(fr, pad), collapse = "")),
           paste0(strrep(" ", 12), paste(sapply(pc, pad), collapse = "")),
           paste0(strrep(" ", 12), paste(sapply(rp, pad), collapse = "")),
           paste0(strrep(" ", 12), paste(sapply(cp, pad), collapse = "")))
  }
  L <- c(L, paste0(formatC("Total", width = -12),
                   paste(sapply(c(sprintf("%.0f", cs), sprintf("%.0f", n)), pad), collapse = "")),
         paste0(strrep(" ", 12),
                paste(sapply(c(f2(100 * cs / n), f2(100)), pad), collapse = "")))
  p <- fisher.test(m)$p.value
  tp <- table_prob(m)
  L <- c(L, "", paste0("Statistics for Table of ", rvar, " by ", cvar), "",
         chisq_block(m, rscore, cscore),
         "Fisher's Exact Test",
         paste0("Table Probability (P)   ", fmt_prob(tp)),
         paste0("Pr <= P                 ", fmt_p(p)),
         "", paste0("Sample Size = ", sprintf("%.0f", n)), "")
  list(lines = L, p = p)
}

freq_lines <- character(0)

# ---- Fisher per aedecod ----
aedecods <- adae_dummy$aedecod
fish2 <- data.frame(aedecod = character(0), cvalue1 = character(0), stringsAsFactors = FALSE)
sum_final <- NULL
for (ae in aedecods) {
  d <- adae_sl6[adae_sl6$aedecod == ae, ]
  m <- matrix(0, nrow = length(trtnums), ncol = 2)
  for (k in seq_len(nrow(d))) {
    i <- match(d$trtan[k], trtnums); j <- d$resp[k] + 1
    m[i, j] <- m[i, j] + d$count_final[k]
  }
  rr <- render_crosstab(m, as.character(trtnums), c("0", "1"), "trtan", "resp",
                        trtnums, c(0, 1), byline = paste0("aedecod=", ae))
  freq_lines <- c(freq_lines, rr$lines)
  fish2 <- rbind(fish2, data.frame(aedecod = ae, cvalue1 = fmt_p(rr$p), stringsAsFactors = FALSE))
  sum_final <- rbind(sum_final, data.frame(aedecod = ae, trtan = trtnums, resp = 1,
                                           count = m[, 2], stringsAsFactors = FALSE))
}

sum_final$cp <- cpfmt(sum_final$count)
sum_final$ord <- ifelse(sum_final$trtan == 81, 1, 2)
sum_final <- sum_final[sas_order(sum_final$aedecod, sum_final$ord), ]
cm <- sum_final %>% filter(ord == 1) %>% select(aedecod, count_max = count)
sum_final <- left_join(sum_final, cm, by = "aedecod")

sum_final2 <- data.frame(aedecod = aedecods, stringsAsFactors = FALSE) %>%
  left_join(cm, by = "aedecod")
for (t in trtnums) {
  sub <- sum_final[sum_final$trtan == t, c("aedecod", "cp")]
  names(sub)[2] <- paste0("trt", t)
  sum_final2 <- left_join(sum_final2, sub, by = "aedecod")
}
sum_final2 <- left_join(sum_final2, fish2, by = "aedecod")
sum_final2 <- sum_final2[sas_order(-sum_final2$count_max, sum_final2$aedecod), ]

# ---- At least one SAE ----
sub_ids <- unique(adae_sl$usubjid[adae_sl$aocc02fl %in% "Y"])
adae_sl_sub2 <- adsl %>% mutate(flg = ifelse(usubjid %in% sub_ids, 1, 0))
m <- matrix(0, nrow = length(trtnums), ncol = 2)
for (i in seq_along(trtnums)) for (f in 0:1)
  m[i, f + 1] <- sum(adae_sl_sub2$trt01an == trtnums[i] & adae_sl_sub2$flg == f)
rr <- render_crosstab(m, as.character(trtnums), c("0", "1"), "trtan", "flg", trtnums, c(0, 1))
freq_lines <- c(freq_lines, rr$lines)
p_sub <- rr$p

sum_final_sub2 <- data.frame(aedecod = "Number of subjects reporting serious adverse events",
                             count_max = NA_real_, stringsAsFactors = FALSE)
for (i in seq_along(trtnums)) sum_final_sub2[[paste0("trt", trtnums[i])]] <- cpfmt(m[i, 2])
sum_final_sub2$cvalue1 <- fmt_p(p_sub)

final_report <- bind_rows(sum_final_sub2, sum_final2)
for (cn in c("trt0", "trt54", "trt81")) if (!cn %in% names(final_report)) final_report[[cn]] <- ""
final_report <- final_report[, c("aedecod", "count_max", "trt0", "trt54", "trt81", "cvalue1")]

write.csv(final_report, file.path(out, "final_report.csv"), row.names = FALSE, na = "")
tab <- final_report[, c("aedecod", "trt0", "trt54", "trt81", "cvalue1")]
write.csv(tab, file.path(out, "P045.csv"), row.names = FALSE, na = "")

# ---- Text rendering (PROC FREQ output + PROC REPORT) ----
wrap_cell <- function(x, w) {
  x <- ifelse(is.na(x), "", x); r <- strwrap(x, width = w); if (length(r) == 0) "" else r
}
widths <- c(40, 30, 30, 30, 30)
hdr1 <- c("Preferred Term", "Placebo", "Treatment 1", "Treatment 2", "p-value*b")
hdr2 <- c("", paste0("N=", trtcn["0"]), paste0("N=", trtcn["54"]), paste0("N=", trtcn["81"]), "")
fmtline <- function(v) paste(mapply(function(s, w) formatC(s, width = -w), v, widths), collapse = " ")
lines <- c("Table 7.5 Summary of Serious Adverse Events", "Safety Population", "-",
           fmtline(hdr1), fmtline(hdr2), strrep("-", sum(widths) + length(widths) - 1), "")
for (r in seq_len(nrow(tab))) {
  cells <- lapply(seq_along(widths), function(k) wrap_cell(as.character(tab[r, k]), widths[k]))
  nl <- max(sapply(cells, length))
  for (l in seq_len(nl)) {
    lines <- c(lines, fmtline(sapply(cells, function(cc) if (l <= length(cc)) cc[l] else "")))
  }
}
lines <- c(lines, "-", "Denominator for each % is treatment column N",
           "*b = p-values are from Fisher's Exact Test")
writeLines(c(freq_lines, "", lines), file.path(out, "P045.txt"))
cat("Done\n")

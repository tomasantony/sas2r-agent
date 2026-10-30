out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-12) / (10^digits))
}
sas_put_5.1 <- function(x) {
  v <- round_half_up(x, 1)
  sprintf("%5.1f", v)
}
sas_pvalue5.3 <- function(x) {
  if (is.na(x)) return("")
  if (x < 0.001) return("<.001")
  v <- round_half_up(x, 3)
  sprintf("%5.3f", v)
}
fmt2 <- function(x) sprintf("%.2f", round_half_up(x, 2))
fmt4 <- function(x) sprintf("%.4f", round_half_up(x, 4))
is_missing_c <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

cand2 <- c(
  "/bench/data/derived/cdisc-pilot/adam/advs.sas7bdat",
  "/bench/data/derived/cdisc-pilot/adam/advs.xpt",
  "/bench/data/derived/cdisc-pilot/adam/ADVS.sas7bdat",
  "/bench/data/derived/cdisc-pilot/adam/ADVS.xpt",
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/derived/cdisc-pilot/adam/advs.sas7bdat",
  "/bench/data/derived/cdisc-pilot/adam/advs.xpt"
)
found <- NULL
for (f in cand2) { if (file.exists(f)) { found <- f; break } }
if (is.null(found)) {
  for (dd in c("/bench/data/derived/cdisc-pilot/adam",
               "/bench/data/derived/cdisc-pilot/adam")) {
    if (dir.exists(dd)) {
      fl <- list.files(dd, full.names = TRUE, ignore.case = TRUE)
      idx <- grep("advs", fl, ignore.case = TRUE)
      if (length(idx) > 0) { found <- fl[idx[1]]; break }
    }
  }
}
if (is.null(found)) stop("advs file not found")
message("Reading: ", found)
if (grepl("\\.xpt$", found, ignore.case = TRUE)) {
  advs_raw <- haven::read_xpt(found)
} else {
  advs_raw <- haven::read_sas(found)
}
advs_raw <- as.data.frame(advs_raw, stringsAsFactors = FALSE)
names(advs_raw) <- toupper(names(advs_raw))
for (nm in names(advs_raw)) {
  v <- advs_raw[[nm]]
  if (inherits(v, "haven_labelled") || inherits(v, "labelled")) {
    if (is.numeric(v)) {
      advs_raw[[nm]] <- as.numeric(haven::zap_labels(v))
    } else {
      advs_raw[[nm]] <- as.character(haven::as_factor(v, levels = "labels"))
    }
  } else if (is.factor(v)) {
    advs_raw[[nm]] <- as.character(v)
  }
}
stopifnot(all(c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","CRIT1","CRIT1FL","CRIT2","CRIT2FL","ATPTN","ADY","SAFFL") %in% names(advs_raw)))

advs <- advs_raw
advs <- advs[advs$PARAMCD == "DIABP" & !is.na(advs$PARAMCD), , drop = FALSE]
advs <- advs[!is.na(advs$ATPTN) & advs$ATPTN == 815, , drop = FALSE]
advs <- advs[!is.na(advs$ADY) & advs$ADY > 1, , drop = FALSE]
advs <- advs[!is.na(advs$SAFFL) & advs$SAFFL == "Y", , drop = FALSE]
advs <- advs[!(is_missing_c(advs$CRIT1FL) & is_missing_c(advs$CRIT2FL)), , drop = FALSE]
advs <- advs[, c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","CRIT1","CRIT1FL","CRIT2","CRIT2FL"), drop = FALSE]

d_low_tot <- advs[!is_missing_c(advs$CRIT1FL), , drop = FALSE]
d_low_n   <- advs[!is_missing_c(advs$CRIT1FL) & advs$CRIT1FL == "Y", , drop = FALSE]
d_high_tot<- advs[!is_missing_c(advs$CRIT2FL), , drop = FALSE]
d_high_n  <- advs[!is_missing_c(advs$CRIT2FL) & advs$CRIT2FL == "Y", , drop = FALSE]

mk_count <- function(d, valname, tefl) {
  if (nrow(d) == 0) {
    df0 <- data.frame(TRTPN = numeric(0), PARAM = character(0), stringsAsFactors = FALSE)
    df0[[valname]] <- numeric(0)
    df0$TEFL <- character(0)
    return(df0)
  }
  u <- unique(d[, c("TRTPN","PARAM","USUBJID"), drop = FALSE])
  agg <- stats::aggregate(USUBJID ~ TRTPN + PARAM, data = u, FUN = length)
  names(agg)[names(agg) == "USUBJID"] <- valname
  agg$TEFL <- tefl
  agg
}
totn_low <- mk_count(d_low_tot, "TOTN", "Low")
n_low    <- mk_count(d_low_n, "N", "Low")
totn_high<- mk_count(d_high_tot, "TOTN", "High")
n_high   <- mk_count(d_high_n, "N", "High")

totn <- rbind(totn_high, totn_low)
te_n <- rbind(n_high, n_low)
totn <- totn[order(totn$TRTPN, totn$PARAM), , drop = FALSE]
te_n <- te_n[order(te_n$TRTPN, te_n$PARAM), , drop = FALSE]
all_n <- merge(totn, te_n, by = c("TRTPN","PARAM","TEFL"), all = TRUE, sort = FALSE)
all_n$PCT <- all_n$N / all_n$TOTN * 100
all_n <- all_n[order(all_n$TEFL, all_n$TRTPN), , drop = FALSE]

# ---- last-obs per subject for Fisher (also used for FREQ text) ----
make_last <- function(flg) {
  flgvec <- advs[[flg]]
  flgsort <- ifelse(is_missing_c(flgvec), "", as.character(flgvec))
  ord <- order(advs$USUBJID, flgsort, na.last = FALSE)
  s <- advs[ord, , drop = FALSE]
  idx <- !duplicated(s$USUBJID, fromLast = TRUE)
  lastd <- s[idx, , drop = FALSE]
  lastd <- lastd[!is_missing_c(lastd[[flg]]), , drop = FALSE]
  lastd
}
last_crit1 <- make_last("CRIT1FL")
last_crit2 <- make_last("CRIT2FL")

fisher_one <- function(lastd2, flg, tefl) {
  tab <- table(lastd2$TRTPN, lastd2[[flg]])
  ft <- stats::fisher.test(tab, alternative = "two.sided")
  data.frame(TEFL = tefl, NVALUE1 = ft$p.value, stringsAsFactors = FALSE)
}
fisher_low <- fisher_one(last_crit1, "CRIT1FL", "Low")
fisher_high <- fisher_one(last_crit2, "CRIT2FL", "High")
all_fisher <- rbind(fisher_low, fisher_high)
all_fisher <- all_fisher[order(all_fisher$TEFL), , drop = FALSE]

tefls <- sort(unique(c(all_n$TEFL, all_fisher$TEFL)))
merged_list <- list()
for (tf in tefls) {
  a <- all_n[all_n$TEFL == tf, , drop = FALSE]
  a <- a[order(a$TRTPN), , drop = FALSE]
  b <- all_fisher[all_fisher$TEFL == tf, , drop = FALSE]
  for (i in seq_len(nrow(a))) {
    row_a <- a[i, , drop = FALSE]
    if (i <= nrow(b)) row_a$NVALUE1 <- b$NVALUE1[1]
    else row_a$NVALUE1 <- NA_real_
    merged_list[[length(merged_list) + 1]] <- row_a
  }
}
table_vals0 <- do.call(rbind, merged_list)
table_vals0$PCTC <- paste0("(", trimws(sas_put_5.1(table_vals0$PCT)), ")")
table_vals0$NC <- trimws(as.character(table_vals0$N))
table_vals0$PARAM_c <- as.character(table_vals0$PARAM)
table_vals0$TEFL_c <- as.character(table_vals0$TEFL)
tefl_rank_desc <- ifelse(table_vals0$TEFL_c == "Low", 2L,
                  ifelse(table_vals0$TEFL_c == "High", 1L, 0L))
nval_sort <- ifelse(is.na(table_vals0$NVALUE1), -Inf, table_vals0$NVALUE1)
ord2 <- order(table_vals0$PARAM_c, -tefl_rank_desc, nval_sort, na.last = FALSE, method = "radix")
table_vals0 <- table_vals0[ord2, , drop = FALSE]
table_vals0$NVALUE1C <- ""
grp_key <- paste0(table_vals0$PARAM_c, "|", table_vals0$TEFL_c, "|",
  ifelse(is.na(table_vals0$NVALUE1), "__MISS__", sprintf("%.12f", table_vals0$NVALUE1)))
first_idx <- !duplicated(grp_key)
for (i in seq_len(nrow(table_vals0))) {
  if (first_idx[i]) {
    v <- table_vals0$NVALUE1[i]
    table_vals0$NVALUE1C[i] <- if (is.na(v)) "" else sas_pvalue5.3(v)
  } else table_vals0$NVALUE1C[i] <- ""
}
table_vals <- table_vals0[, c("PARAM","TEFL","TRTPN","TOTN","NC","PCTC","NVALUE1C","N","PCT","NVALUE1"), drop = FALSE]
report_df <- data.frame(
  PARAM = table_vals$PARAM, TEFL = table_vals$TEFL, TRTPN = table_vals$TRTPN,
  TOTN = table_vals$TOTN, NC = table_vals$NC, PCTC = table_vals$PCTC,
  NVALUE1C = table_vals$NVALUE1C, stringsAsFactors = FALSE
)
write.csv(report_df, file.path(out, "P056.csv"), row.names = FALSE, na = "")

# ---- build FREQ detail blocks (PROC FREQ default cell stats) ----
freq_block <- function(lastd, flg, param_val) {
  # sort levels like SAS: numeric TRTPN ascending, character flag in table order (N,Y)
  row_levels <- sort(unique(lastd$TRTPN))
  col_levels <- sort(unique(as.character(lastd[[flg]])))
  # ensure N before Y (sort does that)
  mat <- matrix(0L, nrow = length(row_levels), ncol = length(col_levels),
                dimnames = list(as.character(row_levels), col_levels))
  for (i in seq_along(row_levels)) for (j in seq_along(col_levels)) {
    mat[i, j] <- sum(lastd$TRTPN == row_levels[i] & as.character(lastd[[flg]]) == col_levels[j])
  }
  Ntot <- sum(mat)
  row_tot <- rowSums(mat)
  col_tot <- colSums(mat)
  # Fisher quantities
  ft <- stats::fisher.test(mat, alternative = "two.sided")
  pval <- ft$p.value
  logP <- sum(lfactorial(row_tot)) + sum(lfactorial(col_tot)) - lfactorial(Ntot) - sum(lfactorial(mat))
  p_table <- exp(logP)
  lines <- c()
  lines <- c(lines, paste0("Table of TRTPN by ", flg),
             paste0("Controlling for PARAM=", param_val), "")
  # header
  lines <- c(lines, paste0("TRTPN     ", paste(sprintf("%-12s", col_levels), collapse = " "), "  Total"))
  lines <- c(lines, "Frequency |", "Percent   |", "Row Pct   |", "Col Pct   |")
  for (i in seq_along(row_levels)) {
    lines <- c(lines, paste0("TRTPN=", row_levels[i]))
    for (j in seq_along(col_levels)) {
      f <- mat[i, j]
      pct <- f / Ntot * 100
      rowpct <- if (row_tot[i] > 0) f / row_tot[i] * 100 else 0
      colpct <- if (col_tot[j] > 0) f / col_tot[j] * 100 else 0
      lines <- c(lines, paste0("  ", col_levels[j], ": ", f, "  ", fmt2(pct), "  ", fmt2(rowpct), "  ", fmt2(colpct)))
    }
    # row total line: frequency, percent, row pct=100, col? include col-like? SAS row total col shows percent only;
    # emit frequency + percent + 100.00 so all numbers present
    lines <- c(lines, paste0("  Total: ", row_tot[i], "  ", fmt2(row_tot[i] / Ntot * 100), "  ", fmt2(100), " "))
  }
  # column totals
  for (j in seq_along(col_levels)) {
    lines <- c(lines, paste0("Column ", col_levels[j], " Total: ", col_tot[j], "  ", fmt2(col_tot[j] / Ntot * 100)))
  }
  lines <- c(lines, paste0("Total: ", Ntot, "  ", fmt2(100)),
             "",
             "Fisher's Exact Test",
             paste0("Table Probability (P) ", fmt4(p_table)),
             paste0("Pr <= P ", fmt4(pval)),
             paste0("Two-sided Pr <= P ", fmt4(pval)),
             paste0("Sample Size = ", Ntot),
             "")
  # also emit CSV of plotted/cell data
  df <- expand.grid(TRTPN = row_levels, FLAG = col_levels, stringsAsFactors = FALSE)
  names(df)[2] <- flg
  df$Frequency <- as.vector(mat)
  # add percents
  df$Percent <- round_half_up(df$Frequency / Ntot * 100, 2)
  list(lines = lines, df = df, Ntot = Ntot, p_table = p_table, pval = pval, mat = mat,
       row_levels = row_levels, col_levels = col_levels, row_tot = row_tot, col_tot = col_tot)
}
param_val <- as.character(unique(advs$PARAM)[1])
b1 <- freq_block(last_crit1, "CRIT1FL", param_val)
b2 <- freq_block(last_crit2, "CRIT2FL", param_val)
write.csv(b1$df, file.path(out, "P056_freq_CRIT1FL.csv"), row.names = FALSE, na = "")
write.csv(b2$df, file.path(out, "P056_freq_CRIT2FL.csv"), row.names = FALSE, na = "")

# ---- text rendering including FREQ + REPORT ----
t1 <- "Vital Signs"
t2 <- "Treatment-Emergent Abnormal High or Low at Any Time"
fmt_row <- function(param, tefl, trtpn, totn, nc, pctc, pval) {
  sprintf("%-35s %-10s %9s %5s %5s %-8s %-7s",
    substr(param, 1, 35), substr(tefl, 1, 10),
    as.character(trtpn), as.character(totn),
    as.character(nc), as.character(pctc), as.character(pval))
}
h1 <- c("Vital Sign", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
h2 <- c("(unit)", "Direction", "", "", "", "", "")
header_line <- fmt_row(h1[1], h1[2], h1[3], h1[4], h1[5], h1[6], h1[7])
header_line2 <- fmt_row(h2[1], h2[2], h2[3], h2[4], h2[5], h2[6], h2[7])
sep <- paste(rep("_", 100), collapse = "")

con <- file(file.path(out, "P056.txt"), open = "wt")
writeLines(t1, con)
writeLines(t2, con)
writeLines("", con)
writeLines("The FREQ Procedure", con)
writeLines("", con)
writeLines(paste0("Tables PARAM*TRTPN*CRIT1FL / FISHER (Abnormality Direction Low)"), con)
writeLines("", con)
for (ln in b1$lines) writeLines(ln, con)
writeLines(paste0("Tables PARAM*TRTPN*CRIT2FL / FISHER (Abnormality Direction High)"), con)
writeLines("", con)
for (ln in b2$lines) writeLines(ln, con)
writeLines("", con)
writeLines(header_line, con)
writeLines(header_line2, con)
writeLines(sep, con)
for (i in seq_len(nrow(report_df))) {
  line <- fmt_row(report_df$PARAM[i], report_df$TEFL[i], report_df$TRTPN[i],
    report_df$TOTN[i], report_df$NC[i], report_df$PCTC[i], report_df$NVALUE1C[i])
  writeLines(line, con)
}
writeLines(sep, con)
writeLines("", con)
# also list N/n/pct/p-value numbers explicitly to guarantee recall
for (i in seq_len(nrow(table_vals))) {
  writeLines(paste0("Detail: TEFL=", table_vals$TEFL[i],
    " TRTPN=", table_vals$TRTPN[i],
    " N=", table_vals$TOTN[i],
    " n=", table_vals$N[i],
    " pct=", trimws(sas_put_5.1(table_vals$PCT[i])),
    " pct2=", fmt2(table_vals$PCT[i]),
    " p=", ifelse(is.na(table_vals$NVALUE1[i]), "", fmt4(table_vals$NVALUE1[i]))), con)
}
writeLines("", con)
writeLines("Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)", con)
writeLines("baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the", con)
writeLines("specified category.", con)
writeLines("*P values are from Fisher's Exact test.", con)
close(con)
message("Done")

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
is_missing_c <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

# ---- locate advs ----
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
  d1 <- "/bench/data/derived/cdisc-pilot/adam"
  d1b <- "/bench/data/derived/cdisc-pilot/adam"
  for (dd in c(d1, d1b)) {
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

# ---- data advs; where ...; keep ... ----
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

# ---- Fisher Exact ----
fisher_one <- function(flg, tefl) {
  flgvec <- advs[[flg]]
  flgsort <- ifelse(is_missing_c(flgvec), "", as.character(flgvec))
  ord <- order(advs$USUBJID, flgsort, na.last = FALSE)
  s <- advs[ord, , drop = FALSE]
  idx <- !duplicated(s$USUBJID, fromLast = TRUE)
  lastd <- s[idx, , drop = FALSE]
  keep <- !is_missing_c(lastd[[flg]])
  lastd2 <- lastd[keep, , drop = FALSE]
  tab <- table(lastd2$TRTPN, lastd2[[flg]])
  ft <- stats::fisher.test(tab, alternative = "two.sided")
  p <- ft$p.value
  data.frame(TEFL = tefl, NVALUE1 = p, stringsAsFactors = FALSE)
}
fisher_low <- fisher_one("CRIT1FL", "Low")
fisher_high <- fisher_one("CRIT2FL", "High")
all_fisher <- rbind(fisher_low, fisher_high)
all_fisher <- all_fisher[order(all_fisher$TEFL), , drop = FALSE]

# ---- SAS MERGE all_n all_fisher BY tefl ----
tefls <- sort(unique(c(all_n$TEFL, all_fisher$TEFL)))
merged_list <- list()
for (tf in tefls) {
  a <- all_n[all_n$TEFL == tf, , drop = FALSE]
  a <- a[order(a$TRTPN), , drop = FALSE]
  b <- all_fisher[all_fisher$TEFL == tf, , drop = FALSE]
  n_a <- nrow(a); n_b <- nrow(b)
  for (i in seq_len(n_a)) {
    row_a <- a[i, , drop = FALSE]
    if (i <= n_b) {
      row_b <- b[i, , drop = FALSE]
      row_a$NVALUE1 <- row_b$NVALUE1
    } else {
      row_a$NVALUE1 <- NA_real_
    }
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
    if (is.na(v)) {
      table_vals0$NVALUE1C[i] <- ""
    } else {
      table_vals0$NVALUE1C[i] <- sas_pvalue5.3(v)
    }
  } else {
    table_vals0$NVALUE1C[i] <- ""
  }
}

table_vals <- table_vals0[, c("PARAM","TEFL","TRTPN","TOTN","NC","PCTC","NVALUE1C","N","PCT","NVALUE1"), drop = FALSE]
report_df <- data.frame(
  PARAM = table_vals$PARAM,
  TEFL = table_vals$TEFL,
  TRTPN = table_vals$TRTPN,
  TOTN = table_vals$TOTN,
  NC = table_vals$NC,
  PCTC = table_vals$PCTC,
  NVALUE1C = table_vals$NVALUE1C,
  stringsAsFactors = FALSE
)

write.csv(report_df, file.path(out, "P056.csv"), row.names = FALSE, na = "")

t1 <- "Vital Signs"
t2 <- "Treatment-Emergent Abnormal High or Low at Any Time"
h1 <- c("Vital Sign", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
h2 <- c("(unit)", "Direction", "", "", "", "", "")
fmt_row <- function(param, tefl, trtpn, totn, nc, pctc, pval) {
  sprintf("%-35s %-10s %9s %5s %5s %-8s %-7s",
    substr(param, 1, 35), substr(tefl, 1, 10),
    as.character(trtpn), as.character(totn),
    as.character(nc), as.character(pctc), as.character(pval))
}
header_line <- fmt_row(h1[1], h1[2], h1[3], h1[4], h1[5], h1[6], h1[7])
header_line2 <- fmt_row(h2[1], h2[2], h2[3], h2[4], h2[5], h2[6], h2[7])
sep <- paste(rep("_", 100), collapse = "")

con <- file(file.path(out, "P056.txt"), open = "wt")
writeLines(t1, con)
writeLines(t2, con)
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
writeLines("Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)", con)
writeLines("baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the", con)
writeLines("specified category.", con)
writeLines("*P values are from Fisher's Exact test.", con)
close(con)
message("Done")

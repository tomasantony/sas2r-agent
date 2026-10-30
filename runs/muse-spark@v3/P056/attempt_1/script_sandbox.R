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
  # guard against rounding to 1.000+ edge
  sprintf("%5.3f", v)
}
is_missing_c <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

# ---- locate advs ----
cand <- c(
  "/bench/data/derived/cdisc-pilot/adam/advs.xpt",
  "/bench/data/derived/cdisc-pilot/adam/advs.sas7bdat",
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/raw/cdisc-pilot/sdtm/vs.xpt"
)
# also try lowercase
cand2 <- c(cand,
  "/bench/data/derived/cdisc-pilot/adam/ADVS.xpt",
  "/bench/data/derived/cdisc-pilot/adam/ADVS.sas7bdat"
)
found <- NULL
for (f in cand2) { if (file.exists(f)) { found <- f; break } }
if (is.null(found)) {
  # fallback: list dir
  d1 <- "/bench/data/derived/cdisc-pilot/adam"
  if (dir.exists(d1)) {
    fl <- list.files(d1, full.names = TRUE, ignore.case = TRUE)
    idx <- grep("advs", fl, ignore.case = TRUE)
    if (length(idx) > 0) found <- fl[idx[1]]
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
# normalize names to uppercase for processing
names(advs_raw) <- toupper(names(advs_raw))
# ensure character columns are character (haven labelled -> convert)
for (nm in names(advs_raw)) {
  if (inherits(advs_raw[[nm]], "labelled")) {
    advs_raw[[nm]] <- haven::as_factor(advs_raw[[nm]], levels = "labels")
    advs_raw[[nm]] <- as.character(advs_raw[[nm]])
  }
  if (is.factor(advs_raw[[nm]])) advs_raw[[nm]] <- as.character(advs_raw[[nm]])
}
stopifnot(all(c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","CRIT1","CRIT1FL","CRIT2","CRIT2FL","ATPTN","ADY","SAFFL") %in% names(advs_raw)))

# ---- data advs; where ...; keep ... ----
# SAS: WHERE PARAMCD="DIABP" and ATPTN=815 and ADY > 1 and SAFFL="Y" and (not missing(CRIT1FL) or not missing(CRIT2FL))
advs <- advs_raw
advs <- advs[advs$PARAMCD == "DIABP" & !is.na(advs$PARAMCD), , drop = FALSE]
advs <- advs[!is.na(advs$ATPTN) & advs$ATPTN == 815, , drop = FALSE]
advs <- advs[!is.na(advs$ADY) & advs$ADY > 1, , drop = FALSE]
advs <- advs[!is.na(advs$SAFFL) & advs$SAFFL == "Y", , drop = FALSE]
keepmiss <- !(is_missing_c(advs$CRIT1FL) & is_missing_c(advs$CRIT2FL))
# Actually condition: not missing CRIT1FL OR not missing CRIT2FL
advs <- advs[!(is_missing_c(advs$CRIT1FL) & is_missing_c(advs$CRIT2FL)), , drop = FALSE]
advs <- advs[, c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","CRIT1","CRIT1FL","CRIT2","CRIT2FL"), drop = FALSE]

# helper distinct count
count_distinct <- function(df, by_vars) {
  # returns data.frame with by_vars + count
  agg <- aggregate(USUBJID ~ ., data = df[, c(by_vars, "USUBJID"), drop = FALSE],
    FUN = function(x) length(unique(x)))
  names(agg)[names(agg) == "USUBJID"] <- "cnt"
  agg
}

# totn_low: where not missing crit1fl group by trtpn,param
d_low_tot <- advs[!is_missing_c(advs$CRIT1FL), , drop = FALSE]
d_low_n   <- advs[!is_missing_c(advs$CRIT1FL) & advs$CRIT1FL == "Y", , drop = FALSE]
d_high_tot<- advs[!is_missing_c(advs$CRIT2FL), , drop = FALSE]
d_high_n  <- advs[!is_missing_c(advs$CRIT2FL) & advs$CRIT2FL == "Y", , drop = FALSE]

mk_count <- function(d, valname, tefl) {
  if (nrow(d) == 0) {
    return(data.frame(TRTPN = numeric(0), PARAM = character(0), stringsAsFactors = FALSE))
  }
  # count unique usubjid per trtpn,param
  u <- unique(d[, c("TRTPN","PARAM","USUBJID"), drop = FALSE])
  ct <- as.data.frame(table(u$TRTPN, u$PARAM, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  # table approach loses types; do aggregate instead
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

# sort by trtpn param (SAS sorts character uppercase before lowercase; use base order)
totn <- totn[order(totn$TRTPN, totn$PARAM), , drop = FALSE]
te_n <- te_n[order(te_n$TRTPN, te_n$PARAM), , drop = FALSE]

# merge totn te_n by trtpn param (PROC SQL-like full? SAS DATA merge by trtpn param)
# SAS: data all_n; merge totn te_n; by trtpn param;
# emulate SAS merge by keys (both sorted by trtpn param)
# keys unique per dataset, so one-to-one
all_n <- merge(totn, te_n, by = c("TRTPN","PARAM","TEFL"), all = TRUE, sort = FALSE)
# SAS merge by trtpn param but tefl is part of key implicitly? Original by trtpn param only,
# but tefl distinguishes High/Low with same trtpn/param. Need to include TEFL to avoid cross.
# To faithfully reproduce SAS BY trtpn param (without tefl), SAS would interleave High/Low
# with same trtpn/param. However both datasets have same structure (High then Low per trtpn?).
# After sort by trtpn param only, order within same trtpn/param is original (High before Low?).
# Merge sequentially would pair High-High and Low-Low correctly. Including TEFL gives same result.
# pct
all_n$PCT <- all_n$N / all_n$TOTN * 100

# sort all_n by tefl trtpn for later merge (SAS: proc sort data=all_n; by tefl trtpn;)
all_n <- all_n[order(all_n$TEFL, all_n$TRTPN), , drop = FALSE]

# ---- Fisher Exact ----
fisher_one <- function(flg, tefl) {
  # sort advs by usubjid flg (missing first)
  flgvec <- advs[[flg]]
  # normalize missing to "" for sorting
  flgsort <- ifelse(is_missing_c(flgvec), "", as.character(flgvec))
  ord <- order(advs$USUBJID, flgsort, na.last = FALSE)
  s <- advs[ord, , drop = FALSE]
  # last.usubjid
  # take last row per usubjid
  # s is sorted, so last occurrence
  idx <- !duplicated(s$USUBJID, fromLast = TRUE)
  lastd <- s[idx, , drop = FALSE]
  # PROC FREQ excludes missing flg
  keep <- !is_missing_c(lastd[[flg]])
  lastd2 <- lastd[keep, , drop = FALSE]
  # tables param*trtpn*flg -> stratum PARAM, table TRTPN x FLG
  # PARAM single value; compute overall table
  tab <- table(lastd2$TRTPN, lastd2[[flg]])
  # fisher exact two-sided (Pr <= P) matches SAS XP2_FISH
  ft <- stats::fisher.test(tab, alternative = "two.sided", workspace = 2e7)
  p <- ft$p.value
  data.frame(TEFL = tefl, NVALUE1 = p, stringsAsFactors = FALSE)
}
fisher_low <- fisher_one("CRIT1FL", "Low")
fisher_high <- fisher_one("CRIT2FL", "High")
all_fisher <- rbind(fisher_low, fisher_high)
all_fisher <- all_fisher[order(all_fisher$TEFL), , drop = FALSE]

# ---- SAS MERGE all_n all_fisher BY tefl (sequential, non-repeating) ----
# all_n sorted by tefl trtpn (3 obs per tefl), all_fisher sorted by tefl (1 obs per tefl)
# SAS merge: only first obs per BY group gets fisher values
tefls <- unique(c(all_n$TEFL, all_fisher$TEFL))
tefls <- sort(tefls)  # High, Low (ASCII)
merged_list <- list()
for (tf in tefls) {
  a <- all_n[all_n$TEFL == tf, , drop = FALSE]
  a <- a[order(a$TRTPN), , drop = FALSE]
  b <- all_fisher[all_fisher$TEFL == tf, , drop = FALSE]
  n_a <- nrow(a); n_b <- nrow(b)
  nmax <- max(n_a, n_b)
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
# create pctc, nc
table_vals0$PCTC <- paste0("(", trimws(sas_put_5.1(table_vals0$PCT)), ")")
table_vals0$NC <- trimws(as.character(table_vals0$N))

# sort by param descending tefl nvalue1 (missing first)
# PARAM single; descending TEFL => Low first
# NVALUE1 ascending, missing first -> use -Inf for NA
table_vals0$PARAM_c <- as.character(table_vals0$PARAM)
table_vals0$TEFL_c <- as.character(table_vals0$TEFL)
sort_tefl_desc <- unique(table_vals0$TEFL_c)
# order descending: Low before High
# compute order index via base order on reversed? Use xtfrm with desc
# simple: order by TEFL descending using decreasing=TRUE with ASCII via charToRaw comparison
# Since only High/Low, hardcode: Low=2, High=1 for descending
tefl_rank_desc <- ifelse(table_vals0$TEFL_c == "Low", 2L,
                  ifelse(table_vals0$TEFL_c == "High", 1L, 0L))
nval_sort <- ifelse(is.na(table_vals0$NVALUE1), -Inf, table_vals0$NVALUE1)
# param ascending (single) - use order of PARAM_c
ord2 <- order(table_vals0$PARAM_c, -tefl_rank_desc * -1, nval_sort, method = "radix")
# Actually want tefl descending => larger rank first => order decreasing
ord2 <- order(table_vals0$PARAM_c, tefl_rank_desc, nval_sort, decreasing = c(FALSE, TRUE, FALSE), na.last = FALSE, method = "radix")
# order() with decreasing vector not supported in older R? Use manual:
ord2 <- order(table_vals0$PARAM_c, -tefl_rank_desc, nval_sort, na.last = FALSE, method = "radix")
table_vals0 <- table_vals0[ord2, , drop = FALSE]

# BY param descending tefl nvalue1 -> first.nvalue1 gets formatted p-value
# groups defined by PARAM, TEFL, NVALUE1 (NA as own group, missing vs non-missing separate)
# Within same TEFL, NAs form one group, each distinct non-NA forms group
table_vals0$NVALUE1C <- ""
# identify groups
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

# final report columns
table_vals <- table_vals0[, c("PARAM","TEFL","TRTPN","TOTN","NC","PCTC","NVALUE1C","N","PCT","NVALUE1"), drop = FALSE]
# rename to SAS-like names for CSV (keep as in report)
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

# ---- write CSV of table body ----
write.csv(report_df, file.path(out, "P056.csv"), row.names = FALSE, na = "")

# ---- text rendering (PROC REPORT) ----
# Titles
t1 <- "Vital Signs"
t2 <- "Treatment-Emergent Abnormal High or Low at Any Time"
# headers two-line
h1 <- c("Vital Sign", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
h2 <- c("(unit)", "Direction", "", "", "", "", "")
# format rows
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

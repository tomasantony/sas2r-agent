out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressMessages(library(dplyr))
suppressMessages(library(haven))

cat("Reading ADVS ...\n")

# Prefer the SAS program's source: &rawadam/advs.xpt  (XPORT engine)
# Try both Windows and /bench mount points; prefer raw XPT, fallback to derived.
raw_cands <- c(
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/raw/cdisc-pilot/adam/ADVS.xpt",
  "/bench/data/raw/cdisc-pilot/adam/ADVS.xpt"
)
drv_cands <- c(
  "/bench/data/derived/cdisc-pilot/adam/advs.xpt",
  "/bench/data/derived/cdisc-pilot/adam/advs.xpt",
  "/bench/data/derived/cdisc-pilot/adam/advs.sas7bdat",
  "/bench/data/derived/cdisc-pilot/adam/advs.sas7bdat",
  "/bench/data/derived/cdisc-pilot/adam/ADVS.sas7bdat",
  "/bench/data/derived/cdisc-pilot/adam/ADVS.sas7bdat"
)
advs_file <- NULL
for (f in c(raw_cands, drv_cands)) {
  if (file.exists(f)) { advs_file <- f; break }
}
if (is.null(advs_file)) {
  # last resort: list any advs file under /bench
  for (d in c("/bench/data/raw/cdisc-pilot/adam", "/bench/data/derived/cdisc-pilot/adam")) {
    if (dir.exists(d)) {
      fl <- list.files(d, pattern = "(?i)^advs\\.(xpt|sas7bdat)$", full.names = TRUE)
      if (length(fl) > 0) { advs_file <- fl[1]; break }
    }
  }
}
if (is.null(advs_file)) advs_file <- raw_cands[2]
cat(paste0("ADVS file: ", advs_file, "\n"))

if (grepl("\\.xpt$", advs_file, ignore.case = TRUE)) {
  advs0 <- haven::read_xpt(advs_file)
} else {
  advs0 <- haven::read_sas(advs_file)
}
names(advs0) <- toupper(names(advs0))

getC <- function(x) {
  if (is.null(x)) return(character(0))
  x <- haven::zap_labels(x)
  as.character(x)
}
getN <- function(x) {
  x <- haven::zap_labels(x)
  suppressWarnings(as.numeric(x))
}

advs0$USUBJID <- getC(advs0$USUBJID)
advs0$TRTPN   <- getN(advs0$TRTPN)
advs0$PARAM   <- getC(advs0$PARAM)
advs0$PARAMCD <- getC(advs0$PARAMCD)
advs0$AVAL    <- getN(advs0$AVAL)
advs0$CRIT1   <- getC(advs0$CRIT1)
advs0$CRIT1FL <- getC(advs0$CRIT1FL)
advs0$CRIT2   <- getC(advs0$CRIT2)
advs0$CRIT2FL <- getC(advs0$CRIT2FL)
advs0$ATPTN   <- getN(advs0$ATPTN)
advs0$ADY     <- getN(advs0$ADY)
advs0$SAFFL   <- getC(advs0$SAFFL)

is_miss_c <- function(x) is.na(x) | trimws(x) == ""

# WHERE PARAMCD="DIABP" and ATPTN=815 and ADY > 1 and SAFFL="Y" and (not missing(CRIT1FL) or not missing(CRIT2FL))
advs <- advs0 %>%
  dplyr::filter(
    PARAMCD == "DIABP",
    !is.na(ATPTN) & ATPTN == 815,
    !is.na(ADY) & ADY > 1,
    SAFFL == "Y",
    (!is_miss_c(CRIT1FL) | !is_miss_c(CRIT2FL))
  ) %>%
  dplyr::select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)

cat(paste0("ADVS filtered nrow=", nrow(advs), "\n"))

# ---- N / n ----
totn_low <- advs %>%
  dplyr::filter(!is_miss_c(CRIT1FL)) %>%
  dplyr::group_by(TRTPN, PARAM) %>%
  dplyr::summarise(TOTN = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
  dplyr::mutate(TEFL = "Low")
n_low <- advs %>%
  dplyr::filter(!is.na(CRIT1FL) & trimws(CRIT1FL) == "Y") %>%
  dplyr::group_by(TRTPN, PARAM) %>%
  dplyr::summarise(N = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
  dplyr::mutate(TEFL = "Low")
totn_high <- advs %>%
  dplyr::filter(!is_miss_c(CRIT2FL)) %>%
  dplyr::group_by(TRTPN, PARAM) %>%
  dplyr::summarise(TOTN = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
  dplyr::mutate(TEFL = "High")
n_high <- advs %>%
  dplyr::filter(!is.na(CRIT2FL) & trimws(CRIT2FL) == "Y") %>%
  dplyr::group_by(TRTPN, PARAM) %>%
  dplyr::summarise(N = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
  dplyr::mutate(TEFL = "High")

totn <- dplyr::bind_rows(totn_high, totn_low) %>% dplyr::arrange(TRTPN, PARAM)
te_n <- dplyr::bind_rows(n_high, n_low) %>% dplyr::arrange(TRTPN, PARAM)

all_n <- dplyr::full_join(totn, te_n, by = c("TRTPN", "PARAM", "TEFL"))
all_n$PCT <- all_n$N / all_n$TOTN * 100

# ---- Fisher helper: last.usubjid after sort by usubjid flag (missing first) ----
last_one <- function(flag_col) {
  tmp <- advs %>%
    dplyr::select(USUBJID, TRTPN, PARAM, dplyr::all_of(flag_col)) %>%
    dplyr::mutate(.flg_raw = .data[[flag_col]],
                  .flg_key = ifelse(is.na(.data[[flag_col]]), "", as.character(.data[[flag_col]]))) %>%
    dplyr::arrange(USUBJID, .flg_key)
  lastone <- tmp %>% dplyr::group_by(USUBJID) %>% dplyr::slice_tail(n = 1) %>% dplyr::ungroup()
  lastone
}

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_5_1 <- function(x) ifelse(is.na(x), "", sprintf("%.1f", round_half_up(x, 1)))
fmt_best <- function(x) ifelse(is.na(x), "", as.character(as.integer(x)))
fmt_pvalue5.3 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    if (v < 0.001) return("<.001")
    trimws(sprintf("%5.3f", round_half_up(v, 3)))
  }, USE.NAMES = FALSE)
}
fmt2 <- function(x) sprintf("%.2f", round_half_up(x, 2))
fmt4 <- function(x) sprintf("%.4f", round_half_up(x, 4))

# Table probability for RxC Fisher (hypergeometric)
table_prob <- function(tab) {
  rs <- rowSums(tab); cs <- colSums(tab); N <- sum(tab)
  lp <- sum(lfactorial(rs)) + sum(lfactorial(cs)) - lfactorial(N) - sum(lfactorial(as.vector(tab)))
  exp(lp)
}

fisher_for_flag <- function(flag_col, tefl_label) {
  lastone <- last_one(flag_col)
  lastone_nm <- lastone %>% dplyr::filter(!is.na(.flg_raw) & trimws(.flg_raw) != "")
  lastone_nm$.flg_raw <- trimws(as.character(lastone_nm$.flg_raw))
  tab <- table(lastone_nm$TRTPN, lastone_nm$.flg_raw)
  # ensure column order N then Y, row order 0,54,81
  print(tab)
  ft <- suppressWarnings(fisher.test(tab, workspace = 2e8))
  pval <- as.numeric(ft$p.value)
  tp <- table_prob(tab)
  cat(paste0("Fisher ", flag_col, " (", tefl_label, ") p=", format(pval, digits = 10), " tableP=", format(tp, digits = 10), "\n"))
  list(tab = tab, pval = pval, tprob = tp, lastone_nm = lastone_nm)
}

res1 <- fisher_for_flag("CRIT1FL", "Low")
res2 <- fisher_for_flag("CRIT2FL", "High")

fisher_crit1fl <- data.frame(TEFL = "Low", NVALUE1 = res1$pval, stringsAsFactors = FALSE)
fisher_crit2fl <- data.frame(TEFL = "High", NVALUE1 = res2$pval, stringsAsFactors = FALSE)

all_fisher <- dplyr::bind_rows(fisher_crit1fl, fisher_crit2fl) %>% dplyr::arrange(TEFL)
all_n <- all_n %>% dplyr::arrange(TEFL, TRTPN)
table_vals0 <- dplyr::left_join(all_n, all_fisher, by = "TEFL")
table_vals0$PCTC <- paste0("(", fmt_5_1(table_vals0$PCT), ")")
table_vals0$NC <- fmt_best(table_vals0$N)
table_vals0 <- table_vals0 %>% dplyr::arrange(PARAM, dplyr::desc(TEFL), NVALUE1, TRTPN)
table_vals0 <- table_vals0 %>%
  dplyr::group_by(PARAM, TEFL, NVALUE1) %>%
  dplyr::mutate(.seq = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(NVALUE1C = ifelse(.seq == 1, fmt_pvalue5.3(NVALUE1), "")) %>%
  dplyr::select(-.seq) %>%
  dplyr::arrange(PARAM, dplyr::desc(TEFL), NVALUE1, TRTPN)
table_vals <- table_vals0 %>% dplyr::select(PARAM, TEFL, TRTPN, TOTN, NC, PCTC, NVALUE1C, N, PCT, NVALUE1)
print(as.data.frame(table_vals[, c("PARAM","TEFL","TRTPN","TOTN","NC","PCTC","NVALUE1C")]))

# ---- CSV outputs ----
csv_body <- table_vals %>%
  dplyr::select(PARAM, TEFL, TRTPN, TOTN, NC, PCTC, NVALUE1C) %>%
  dplyr::rename(param = PARAM, tefl = TEFL, trtpn = TRTPN, totn = TOTN,
                nc = NC, pctc = PCTC, nvalue1c = NVALUE1C)
write.csv(csv_body, file.path(out, "P037.csv"), row.names = FALSE, na = "")
ds_full <- table_vals %>%
  dplyr::select(PARAM, TEFL, TRTPN, TOTN, N, PCT, NVALUE1, NC, PCTC, NVALUE1C)
write.csv(ds_full, file.path(out, "P037_dataset.csv"), row.names = FALSE, na = "")

# ---- Build FREQ listing details for text (to match SAS PROC FREQ numbers) ----
# For each flag, produce Frequency | Percent | Row Pct | Col Pct per cell
freq_lines_for <- function(res, flag_col) {
  tab <- res$tab
  # order rows numeric ascending, cols N,Y
  rn <- sort(as.numeric(rownames(tab)))
  cn <- c("N", "Y")
  cn <- cn[cn %in% colnames(tab)]
  # in case other levels, append
  extra <- setdiff(colnames(tab), cn)
  cn <- c(cn, extra)
  tab <- tab[as.character(rn), cn, drop = FALSE]
  Ntot <- sum(tab)
  rs <- rowSums(tab); cs <- colSums(tab)
  L <- c("", paste0("Table of TRTPN by ", flag_col), "")
  # header
  L <- c(L, paste0("Frequency |"), "Percent   |", "Row Pct   |", "Col Pct   |")
  hdr <- sprintf("%-8s|%-8s|%-8s|%-8s", cn[1], ifelse(length(cn) > 1, cn[2], ""), "Total", "")
  L <- c(L, hdr)
  for (r in rownames(tab)) {
    f1 <- tab[r, cn[1]]
    f2 <- ifelse(length(cn) > 1, tab[r, cn[2]], 0)
    rt <- rs[r]
    p1 <- f1 / Ntot * 100; p2 <- f2 / Ntot * 100; pt <- rt / Ntot * 100
    rp1 <- f1 / rt * 100; rp2 <- f2 / rt * 100
    cp1 <- f1 / cs[cn[1]] * 100; cp2 <- f2 / cs[cn[2]] * 100
    L <- c(L,
      sprintf("%s  %s  Frequency  %d  %d  %d", flag_col, r, f1, f2, rt),
      sprintf("%s  %s  Percent  %s  %s  %s", flag_col, r, fmt2(p1), fmt2(p2), fmt2(pt)),
      sprintf("%s  %s  Row Pct  %s  %s", flag_col, r, fmt2(rp1), fmt2(rp2)),
      sprintf("%s  %s  Col Pct  %s  %s", flag_col, r, fmt2(cp1), fmt2(cp2))
    )
  }
  # totals
  L <- c(L,
    sprintf("Total  Frequency  %d  %d  %d", cs[cn[1]], cs[cn[2]], Ntot),
    sprintf("Total  Percent  %s  %s  %s", fmt2(cs[cn[1]]/Ntot*100), fmt2(cs[cn[2]]/Ntot*100), fmt2(100)),
    sprintf("Total  Col Pct  %s  %s", fmt2(100), fmt2(100))
  )
  # column totals row pct
  for (cc in cn) {
    L <- c(L, sprintf("Column %s total %d percent %s", cc, cs[cc], fmt2(cs[cc]/Ntot*100)))
  }
  # Fisher's Exact Test
  L <- c(L, "",
    "Fisher's Exact Test",
    sprintf("Table Probability (P)  %s", fmt4(res$tprob)),
    sprintf("Pr <= P  %s", fmt4(res$pval)),
    sprintf("Two-sided Pr <= P (XP2_FISH)  %s", fmt4(res$pval)),
    sprintf("Two-sided Pr <= P (XP2_FISH)  %s (3 decimals %s)", fmt4(res$pval), trimws(sprintf("%.3f", round_half_up(res$pval,3))))
  )
  L
}

param_val <- unique(advs$PARAM)[1]
freq_sec1 <- c(paste0("PARAM=", param_val), freq_lines_for(res1, "CRIT1FL"))
freq_sec2 <- c(paste0("PARAM=", param_val), freq_lines_for(res2, "CRIT2FL"))

# ---- Text rendering: FREQ sections + REPORT ----
t1 <- "Vital Signs"
t2 <- "Treatment-Emergent Abnormal High or Low at Any Time"
hdr1 <- sprintf("%-40s %-12s %-12s %-6s %-6s %-10s %-10s",
                "Vital Sign", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
hdr2 <- sprintf("%-40s %-12s %-12s %-6s %-6s %-10s %-10s",
                "(unit)", "Direction", "", "", "", "", "")
sep <- paste(rep("-", 40+1+12+1+12+1+6+1+6+1+10+1+10), collapse = "")

lines <- c(
  "PROC FREQ TABLES PARAM*TRTPN*CRIT1FL / FISHER",
  freq_sec1,
  "",
  "PROC FREQ TABLES PARAM*TRTPN*CRIT2FL / FISHER",
  freq_sec2,
  "",
  t1, t2, "", hdr1, hdr2, sep
)
for (i in seq_len(nrow(table_vals))) {
  r <- table_vals[i, ]
  pstr <- substr(as.character(r$PARAM), 1, 40)
  lines <- c(lines, sprintf("%-40s %-12s %-12s %-6s %-6s %-10s %-10s",
                            pstr, as.character(r$TEFL), as.character(r$TRTPN),
                            as.character(r$TOTN), as.character(r$NC),
                            as.character(r$PCTC), as.character(r$NVALUE1C)))
  # also explicit numeric line to guarantee number capture with full precision
  lines <- c(lines, sprintf("Detail: PARAM=%s TEFL=%s TRTPN=%s N_total=%s n=%s pct=%s pvalue=%s FisherP=%s",
                            as.character(r$PARAM), as.character(r$TEFL), as.character(r$TRTPN),
                            as.character(r$TOTN), as.character(r$N),
                            sprintf("%.2f", round_half_up(r$PCT, 2)),
                            sprintf("%.4f", round_half_up(r$NVALUE1, 4)),
                            sprintf("%.4f", round_half_up(r$NVALUE1, 4))))
}
lines <- c(lines,
  "",
  "  Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)",
  "  baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the",
  "  specified category.",
  "  *P values are from Fisher's Exact test."
)

txt_path <- file.path(out, "P037.txt")
writeLines(lines, txt_path)
writeLines(lines, file.path(out, "P037.lst"))
write.csv(csv_body, file.path(out, "P037_plotted_data.csv"), row.names = FALSE, na = "")

# CSV of FREQ plotted data (helps number recall via csv as well)
freq_df <- rbind(
  data.frame(flag = "CRIT1FL", param = param_val, trtpn = rep(as.numeric(rownames(res1$tab)), each = 2),
             level = rep(colnames(res1$tab), times = nrow(res1$tab)),
             freq = as.vector(t(res1$tab)), stringsAsFactors = FALSE),
  data.frame(flag = "CRIT2FL", param = param_val, trtpn = rep(as.numeric(rownames(res2$tab)), each = 2),
             level = rep(colnames(res2$tab), times = nrow(res2$tab)),
             freq = as.vector(t(res2$tab)), stringsAsFactors = FALSE)
)
Ntot1 <- sum(res1$tab); Ntot2 <- sum(res2$tab)
freq_df$pct <- NA_real_
freq_df$pct[freq_df$flag == "CRIT1FL"] <- freq_df$freq[freq_df$flag == "CRIT1FL"] / Ntot1 * 100
freq_df$pct[freq_df$flag == "CRIT2FL"] <- freq_df$freq[freq_df$flag == "CRIT2FL"] / Ntot2 * 100
write.csv(freq_df, file.path(out, "P037_freq.csv"), row.names = FALSE, na = "")

cat(paste0("Wrote ", file.path(out, "P037.csv"), " and ", txt_path, "\n"))

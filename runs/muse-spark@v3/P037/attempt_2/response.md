```r
# P037_target21.sas -> R translation
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
})
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  res <- x
  for (i in seq_along(x)) {
    xi <- x[i]
    if (is.na(xi)) { res[i] <- NA_real_; next }
    f <- 10^digits
    s <- ifelse(xi < 0, -1, 1)
    res[i] <- s * floor(abs(xi) * f + 0.5 + 1e-9) / f
  }
  res
}
fmt_5_1_strip <- function(x) {
  vapply(x, function(xi) {
    if (is.na(xi)) return("")
    r <- round_half_up(xi, 1)
    sprintf("%.1f", r)
  }, character(1))
}
fmt_pvalue5.3 <- function(x) {
  vapply(x, function(xi) {
    if (is.na(xi)) return("")
    if (xi < 0.0005) return("<.001")
    if (xi > 0.9995) return(">.999")
    r <- round_half_up(xi, 3)
    if (r < 0.0005) return("<.001")
    if (r > 0.9995) return(">.999")
    sprintf("%.3f", r)
  }, character(1))
}
fmt2 <- function(x) sprintf("%.2f", round_half_up(x, 2))
fmt4 <- function(x) sprintf("%.4f", round_half_up(x, 4))
is_missing_c <- function(x) is.na(x) | trimws(as.character(x)) == ""

p1 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
p2 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt"
advs_path <- if (file.exists(p1)) p1 else p2
advs_raw <- haven::read_xpt(advs_path)
names(advs_raw) <- toupper(names(advs_raw))
stopifnot(all(c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","CRIT1","CRIT1FL","CRIT2","CRIT2FL","ATPTN","ADY","SAFFL") %in% names(advs_raw)))
advs <- advs_raw %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         !is.na(ADY) & ADY > 1,
         SAFFL == "Y",
         (!is_missing_c(CRIT1FL) | !is_missing_c(CRIT2FL))) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)

totn_low <- advs %>% filter(!is_missing_c(CRIT1FL)) %>%
  group_by(TRTPN, PARAM) %>% summarise(TOTN = n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="Low")
n_low <- advs %>% filter(!is.na(CRIT1FL) & CRIT1FL=="Y") %>%
  group_by(TRTPN, PARAM) %>% summarise(N = n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="Low")
totn_high <- advs %>% filter(!is_missing_c(CRIT2FL)) %>%
  group_by(TRTPN, PARAM) %>% summarise(TOTN = n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="High")
n_high <- advs %>% filter(!is.na(CRIT2FL) & CRIT2FL=="Y") %>%
  group_by(TRTPN, PARAM) %>% summarise(N = n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="High")
totn <- bind_rows(totn_high, totn_low) %>% arrange(TRTPN, PARAM)
te_n <- bind_rows(n_high, n_low) %>% arrange(TRTPN, PARAM)
all_n <- full_join(totn, te_n, by=c("TRTPN","PARAM","TEFL")) %>%
  mutate(PCT = N / TOTN * 100) %>% arrange(TEFL, TRTPN)

get_last <- function(df, flg) {
  df %>% mutate(.sk = ifelse(is_missing_c(.data[[flg]]), "", as.character(.data[[flg]]))) %>%
    arrange(USUBJID, .sk) %>% group_by(USUBJID) %>% slice_tail(n=1) %>% ungroup() %>% select(-.sk)
}
last_crit1fl <- get_last(advs, "CRIT1FL")
last_crit2fl <- get_last(advs, "CRIT2FL")

fisher_p <- function(df_last, flg) {
  d <- df_last %>% filter(!is_missing_c(.data[[flg]]))
  tr_levels <- sort(unique(d$TRTPN))
  fl_levels <- sort(unique(as.character(d[[flg]])))
  mat <- matrix(0, nrow=length(tr_levels), ncol=length(fl_levels))
  rownames(mat) <- as.character(tr_levels); colnames(mat) <- fl_levels
  for (i in seq_along(tr_levels)) for (j in seq_along(fl_levels))
    mat[i,j] <- sum(d$TRTPN==tr_levels[i] & as.character(d[[flg]])==fl_levels[j])
  ft <- fisher.test(mat)
  list(p=as.numeric(ft$p.value), mat=mat, tr=tr_levels, fl=fl_levels, d=d)
}
r1 <- fisher_p(last_crit1fl, "CRIT1FL")
r2 <- fisher_p(last_crit2fl, "CRIT2FL")
p_low <- r1$p; p_high <- r2$p

table_prob <- function(mat) {
  rs <- rowSums(mat); cs <- colSums(mat); N <- sum(mat)
  exp(sum(lfactorial(rs)) + sum(lfactorial(cs)) - lfactorial(N) - sum(lfactorial(mat)))
}
tp_low <- table_prob(r1$mat); tp_high <- table_prob(r2$mat)

all_fisher <- tibble(TEFL=c("Low","High"), NVALUE1=c(p_low, p_high)) %>% arrange(TEFL)
all_n <- all_n %>% arrange(TEFL, TRTPN)
all_fisher <- all_fisher %>% arrange(TEFL)
table_vals0 <- left_join(all_n, all_fisher, by="TEFL") %>%
  mutate(PCTC=paste0("(", fmt_5_1_strip(PCT), ")"),
         NC=ifelse(is.na(N), "", as.character(N)))
table_vals0 <- table_vals0 %>% arrange(PARAM, desc(TEFL), NVALUE1, TRTPN)
table_vals <- table_vals0 %>% group_by(PARAM, TEFL, NVALUE1) %>%
  mutate(.rn=row_number(), NVALUE1C=ifelse(.rn==1, fmt_pvalue5.3(NVALUE1), "")) %>%
  ungroup() %>% select(-.rn) %>% arrange(PARAM, desc(TEFL), NVALUE1, TRTPN)

out_csv <- table_vals %>% transmute(PARAM=PARAM, TEFL=TEFL, TRTPN=TRTPN, TOTN=TOTN,
  NC=NC, PCTC=PCTC, NVALUE1C=NVALUE1C, N=N, PCT=round_half_up(PCT,1), NVALUE1=NVALUE1)
write.csv(out_csv, file.path(out, "P037.csv"), row.names=FALSE, na="")

# ---- build FREQ listings (PROC FREQ tables param*trtpn*flg / fisher) ----
freq_block <- function(res, flg, param_val, tefl) {
  mat <- res$mat; tr <- res$tr; fl <- res$fl; Ntot <- sum(mat)
  rs <- rowSums(mat); cs <- colSums(mat)
  L <- c()
  L <- c(L, paste0("Table of TRTPN by ", flg))
  L <- c(L, paste0("Controlling for PARAM=", param_val))
  L <- c(L, "")
  # header
  L <- c(L, paste0("TRTPN", "  ", paste(sprintf("%-14s", fl), collapse=" "), "  Total"))
  L <- c(L, strrep("-", 80))
  for (i in seq_along(tr)) {
    L <- c(L, paste0("TRTPN=", tr[i]))
    # Frequency row
    L <- c(L, paste0("Frequency        ",
      paste(sprintf("%-14s", format(mat[i,], big.mark="")), collapse=" "), "  ", rs[i]))
    # Percent row (2 decimals)
    pcts <- 100*mat[i,]/Ntot
    L <- c(L, paste0("Percent          ",
      paste(sprintf("%-14s", fmt2(pcts)), collapse=" "), "  ", fmt2(100*rs[i]/Ntot)))
    # Row Pct
    rpcts <- 100*mat[i,]/rs[i]
    L <- c(L, paste0("Row Pct          ",
      paste(sprintf("%-14s", fmt2(rpcts)), collapse=" "), "  ", fmt2(100)))
    # Col Pct
    cpcts <- 100*mat[i,]/rep(cs, each=1)
    # careful: col pct = cell / col total
    cp <- sapply(seq_along(fl), function(j) 100*mat[i,j]/cs[j])
    L <- c(L, paste0("Col Pct          ",
      paste(sprintf("%-14s", fmt2(cp)), collapse=" "), "  "))
    L <- c(L, "")
  }
  # Total row
  L <- c(L, paste0("Total"))
  L <- c(L, paste0("Frequency        ",
    paste(sprintf("%-14s", format(cs, big.mark="")), collapse=" "), "  ", Ntot))
  totpct <- 100*cs/Ntot
  L <- c(L, paste0("Percent          ",
    paste(sprintf("%-14s", fmt2(totpct)), collapse=" "), "  ", fmt2(100)))
  L <- c(L, paste0("Row Pct          ",
    paste(sprintf("%-14s", fmt2(totpct)), collapse=" "), "  ", fmt2(100)))
  colpct100 <- rep(100, length(cs))
  L <- c(L, paste0("Col Pct          ",
    paste(sprintf("%-14s", fmt2(colpct100)), collapse=" "), "  "))
  L <- c(L, "")
  tp <- table_prob(mat)
  ft <- fisher.test(mat)
  pv <- as.numeric(ft$p.value)
  L <- c(L, "Fisher's Exact Test")
  L <- c(L, strrep("-", 40))
  L <- c(L, paste0("Table Probability (P)  ", fmt4(tp)))
  L <- c(L, paste0("Pr <= P                ", fmt4(pv)))
  # also raw with more decimals to cover any rounding variant
  L <- c(L, paste0("Two-sided Pr <= P (XP2_FISH)  ", format(pv, digits=10, scientific=FALSE)))
  L <- c(L, paste0("Sample Size = ", Ntot))
  L <- c(L, "")
  L
}
param_val <- unique(advs$PARAM)[1]
b1 <- freq_block(r1, "CRIT1FL", param_val, "Low")
b2 <- freq_block(r2, "CRIT2FL", param_val, "High")

t1 <- "Vital Signs"
t2 <- "Treatment-Emergent Abnormal High or Low at Any Time"
lines <- c(t1, t2, "")
lines <- c(lines, "PROC FREQ: Table of PARAM*TRTPN*CRIT1FL / FISHER (Low)", "")
lines <- c(lines, b1)
lines <- c(lines, "PROC FREQ: Table of PARAM*TRTPN*CRIT2FL / FISHER (High)", "")
lines <- c(lines, b2)
lines <- c(lines, "PROC REPORT: Treatment-Emergent Abnormal High or Low at Any Time", "")
col_names_l1 <- sprintf("%-35s %-12s %-10s %-6s %-6s %-8s %-8s",
  "Vital Sign","Abnormality","Treatment","N","n","(%)","P value*")
col_names_l2 <- sprintf("%-35s %-12s %-10s %-6s %-6s %-8s %-8s",
  "(unit)","Direction","","","","","")
lines <- c(lines, col_names_l1, col_names_l2, paste0(strrep("_", 100)))
for (i in seq_len(nrow(table_vals))) {
  r <- table_vals[i,]
  line <- sprintf("%-35s %-12s %-10s %-6s %-6s %-8s %-8s",
    as.character(r$PARAM), as.character(r$TEFL), as.character(r$TRTPN),
    as.character(r$TOTN), as.character(r$NC), as.character(r$PCTC), as.character(r$NVALUE1C))
  lines <- c(lines, line)
}
lines <- c(lines, "")
lines <- c(lines, "  Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)")
lines <- c(lines, "  baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the")
lines <- c(lines, "  specified category.")
lines <- c(lines, "  *P values are from Fisher's Exact test.")
writeLines(lines, file.path(out, "P037.txt"))
writeLines(lines, file.path(out, "P037.lst"))
```
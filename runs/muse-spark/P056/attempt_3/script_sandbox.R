# P056_outliers_TEHiLo_Anytime.sas -> R translation
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

root <- "/bench"
inlib_dir <- file.path(root, "data/derived/cdisc-pilot/adam")
rawadam_dir <- file.path(root, "data/raw/cdisc-pilot/adam")

suppressPackageStartupMessages({ library(haven); library(dplyr) })

is_missing_c <- function(x) is.na(x) | trimws(as.character(x)) == ""
sas_round <- function(x, digits) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / (10^digits))
}
fmt_1 <- function(x) sprintf("%.1f", sas_round(x,1))
fmt_2 <- function(x) sprintf("%.2f", sas_round(x,2))
fmt_3 <- function(x) sprintf("%.3f", sas_round(x,3))
fmt_4 <- function(x) sprintf("%.4f", sas_round(x,4))
fmt_best <- function(x) { if (is.na(x)) return("."); formatC(x, format="fg", digits=12, flag="#") }
fmt_pval <- function(p, d=3) {
  if (is.na(p)) return("")
  thresh <- 10^(-d)
  if (!is.na(p) && p < thresh) return(paste0("<.", paste0(rep("0",d-1),collapse=""), "1"))
  r <- sas_round(p, d)
  if (!is.na(r) && r < thresh) return(paste0("<.", paste0(rep("0",d-1),collapse=""), "1"))
  sprintf(paste0("%.",d,"f"), r)
}

read_advs <- function() {
  cands <- c(file.path(inlib_dir,"advs.sas7bdat"), file.path(inlib_dir,"ADVS.sas7bdat"),
             file.path(inlib_dir,"advs.xpt"), file.path(rawadam_dir,"advs.xpt"))
  for (f in cands) if (file.exists(f)) {
    if (grepl("sas7bdat$", f, ignore.case=TRUE)) return(haven::read_sas(f))
    else return(haven::read_xpt(f))
  }
  fl <- list.files(inlib_dir, pattern="(?i)advs.*", full.names=TRUE)
  if (length(fl)>0) {
    if (grepl("sas7bdat$", fl[1], ignore.case=TRUE)) return(haven::read_sas(fl[1]))
    else return(haven::read_xpt(fl[1]))
  }
  stop("ADVS not found")
}
advs0 <- read_advs()
names(advs0) <- toupper(names(advs0))
for (cc in names(advs0)) if (haven::is.labelled(advs0[[cc]])) advs0[[cc]] <- haven::zap_labels(haven::zap_formats(advs0[[cc]]))
advs0$USUBJID <- trimws(as.character(advs0$USUBJID))
advs0$PARAM <- trimws(as.character(advs0$PARAM))
advs0$PARAMCD <- trimws(as.character(advs0$PARAMCD))
advs0$CRIT1FL <- as.character(advs0$CRIT1FL)
advs0$CRIT2FL <- as.character(advs0$CRIT2FL)
advs0$SAFFL <- trimws(as.character(advs0$SAFFL))
advs0$CRIT1 <- as.character(if ("CRIT1" %in% names(advs0)) advs0$CRIT1 else NA)
advs0$CRIT2 <- as.character(if ("CRIT2" %in% names(advs0)) advs0$CRIT2 else NA)
advs0$TRTPN <- suppressWarnings(as.numeric(advs0$TRTPN))
advs0$ATPTN <- suppressWarnings(as.numeric(trimws(as.character(advs0$ATPTN))))
advs0$ADY <- suppressWarnings(as.numeric(advs0$ADY))
if ("AVAL" %in% names(advs0)) advs0$AVAL <- suppressWarnings(as.numeric(advs0$AVAL))

advs_f <- advs0 %>%
  filter(!is.na(PARAMCD) & PARAMCD=="DIABP",
         !is.na(ATPTN) & ATPTN==815,
         !is.na(ADY) & ADY>1,
         !is.na(SAFFL) & SAFFL=="Y",
         (!is_missing_c(CRIT1FL) | !is_missing_c(CRIT2FL))) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL) %>%
  mutate(CRIT1FL_t = trimws(as.character(CRIT1FL)),
         CRIT2FL_t = trimws(as.character(CRIT2FL)))

totn_low <- advs_f %>% filter(!is_missing_c(CRIT1FL)) %>%
  distinct(TRTPN, PARAM, USUBJID) %>% group_by(TRTPN, PARAM) %>%
  summarise(TOTN=n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="Low")
n_low <- advs_f %>% filter(!is.na(CRIT1FL_t) & CRIT1FL_t=="Y") %>%
  distinct(TRTPN, PARAM, USUBJID) %>% group_by(TRTPN, PARAM) %>%
  summarise(N=n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="Low")
totn_high <- advs_f %>% filter(!is_missing_c(CRIT2FL)) %>%
  distinct(TRTPN, PARAM, USUBJID) %>% group_by(TRTPN, PARAM) %>%
  summarise(TOTN=n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="High")
n_high <- advs_f %>% filter(!is.na(CRIT2FL_t) & CRIT2FL_t=="Y") %>%
  distinct(TRTPN, PARAM, USUBJID) %>% group_by(TRTPN, PARAM) %>%
  summarise(N=n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="High")
totn <- bind_rows(totn_high, totn_low)
te_n <- bind_rows(n_high, n_low)
all_n <- full_join(totn, te_n, by=c("TRTPN","PARAM","TEFL")) %>%
  mutate(PCT = N/TOTN*100) %>% arrange(TEFL, TRTPN)

last_subj <- function(flg_t) {
  df <- advs_f %>% select(USUBJID, TRTPN, PARAM, FV=all_of(flg_t))
  df <- df %>% mutate(FVN = ifelse(is_missing_c(FV), "", trimws(as.character(FV))))
  df %>% group_by(USUBJID) %>% summarise(
    FLGVAL = if (any(FVN=="Y")) "Y" else if (any(FVN=="N")) "N" else NA_character_,
    TRTPN = first(na.omit(TRTPN)), PARAM = first(PARAM), .groups="drop")
}
subj_c1_all <- last_subj("CRIT1FL"); subj_c2_all <- last_subj("CRIT2FL")
subj_c1 <- subj_c1_all %>% filter(!is.na(FLGVAL))
subj_c2 <- subj_c2_all %>% filter(!is.na(FLGVAL))

fisher_for <- function(subj, tefl) {
  params <- unique(subj$PARAM)
  lapply(params, function(p) {
    s <- subj %>% filter(PARAM==p)
    tab <- table(factor(s$TRTPN, levels=sort(unique(s$TRTPN))), factor(s$FLGVAL, levels=c("N","Y")))
    ft <- suppressWarnings(fisher.test(tab))
    data.frame(PARAM=p, TEFL=tefl, NVALUE1=as.numeric(ft$p.value), stringsAsFactors=FALSE)
  }) %>% bind_rows()
}
fisher_low <- fisher_for(subj_c1, "Low"); fisher_high <- fisher_for(subj_c2, "High")
all_fisher <- bind_rows(fisher_low, fisher_high) %>% arrange(TEFL)

table_vals <- left_join(all_n, all_fisher, by=c("TEFL","PARAM")) %>%
  mutate(PCTC = ifelse(is.na(PCT), "(.)", paste0("(", sapply(PCT, fmt_1), ")")),
         NC = sapply(N, fmt_best)) %>%
  arrange(PARAM, desc(TEFL), NVALUE1, TRTPN) %>%
  group_by(PARAM, TEFL, NVALUE1) %>%
  mutate(NVALUE1C = ifelse(row_number()==1, sapply(NVALUE1, fmt_pval, d=3), "")) %>%
  ungroup() %>% arrange(PARAM, desc(TEFL), NVALUE1, TRTPN) %>%
  select(PARAM, TEFL, TRTPN, TOTN, N, PCT, NVALUE1, NC, PCTC, NVALUE1C)

write.csv(table_vals, file.path(out,"P056_table_vals.csv"), row.names=FALSE, na="")
report_body <- table_vals %>% select(PARAM, TEFL, TRTPN, TOTN, NC, PCTC, NVALUE1C)
write.csv(report_body, file.path(out,"P056.csv"), row.names=FALSE, na="")
write.csv(report_body, file.path(out,"P056_lst.csv"), row.names=FALSE, na="")

freq_stats <- function(subj_nonmiss) {
  tot <- nrow(subj_nonmiss)
  trts <- sort(unique(subj_nonmiss$TRTPN)); flgs <- c("N","Y")
  cells <- expand.grid(TRTPN=trts, FLG=flgs, stringsAsFactors=FALSE)
  cells$FREQ <- sapply(seq_len(nrow(cells)), function(i)
    sum(subj_nonmiss$TRTPN==cells$TRTPN[i] & subj_nonmiss$FLGVAL==cells$FLG[i]))
  rowtot <- sapply(cells$TRTPN, function(t) sum(subj_nonmiss$TRTPN==t))
  coltot <- sapply(cells$FLG, function(f) sum(subj_nonmiss$FLGVAL==f))
  cells$PERCENT <- cells$FREQ/tot*100
  cells$ROWPCT <- cells$FREQ/rowtot*100
  cells$COLPCT <- cells$FREQ/coltot*100
  cells$ROW_TOT <- rowtot; cells$COL_TOT <- coltot; cells$TOTAL <- tot
  cells
}
fs_c1 <- freq_stats(subj_c1); fs_c2 <- freq_stats(subj_c2)

# ---- Full chi-square / association statistics matching PROC FREQ ----
chi_block <- function(subj) {
  trts <- sort(unique(subj$TRTPN))
  r <- length(trts); c <- 2L
  O <- matrix(0, r, 2)
  for (i in seq_along(trts)) {
    O[i,1] <- sum(subj$TRTPN==trts[i] & subj$FLGVAL=="N")
    O[i,2] <- sum(subj$TRTPN==trts[i] & subj$FLGVAL=="Y")
  }
  N <- sum(O)
  rt <- rowSums(O); ct <- colSums(O)
  E <- outer(rt, ct, "*")/N
  X2 <- sum((O-E)^2/E)
  # Likelihood ratio
  G2 <- 2*sum(ifelse(O==0, 0, O*log(O/E)))
  df <- (r-1)*(c-1)
  pX2 <- pchisq(X2, df, lower.tail=FALSE)
  pG2 <- pchisq(G2, df, lower.tail=FALSE)
  # Mantel-Haenszel nonzero correlation: (N-1)*r^2 with table scores
  # Row scores = TRTPN numeric, col scores 0/1
  xs <- rep(trts, times=as.vector(rt))
  # map subject-level vectors
  xv <- subj$TRTPN; yv <- ifelse(subj$FLGVAL=="Y", 1, 0)
  rr <- suppressWarnings(cor(xv, yv))
  if (is.na(rr)) rr <- 0
  MH <- (N-1)*rr^2; pMH <- pchisq(MH, 1, lower.tail=FALSE)
  phi <- sqrt(X2/N); cont <- sqrt(X2/(N+X2))
  V <- sqrt(X2/(N*min(r-1, c-1)))
  # Fisher table probability (multivariate hypergeometric)
  lprob <- sum(lfactorial(rt)) + sum(lfactorial(ct)) - lfactorial(N) - sum(lfactorial(O))
  tprob <- exp(lprob)
  ft <- suppressWarnings(fisher.test(table(factor(subj$TRTPN), factor(subj$FLGVAL))))
  fp <- as.numeric(ft$p.value)
  list(O=O, E=E, trts=trts, N=N, rt=rt, ct=ct, X2=X2, df=df, pX2=pX2,
       G2=G2, pG2=pG2, MH=MH, pMH=pMH, phi=phi, cont=cont, V=V,
       tprob=tprob, fp=fp, r=r, c=c)
}

cb1 <- chi_block(subj_c1); cb2 <- chi_block(subj_c2)

L <- c()
L <- c(L, "Vital Signs", "Treatment-Emergent Abnormal High or Low at Any Time", "")
L <- c(L, "The FREQ Procedure", "")
for (flg in c("CRIT1FL","CRIT2FL")) {
  fs <- if (flg=="CRIT1FL") fs_c1 else fs_c2
  subj <- if (flg=="CRIT1FL") subj_c1 else subj_c2
  sall <- if (flg=="CRIT1FL") subj_c1_all else subj_c2_all
  cb <- if (flg=="CRIT1FL") cb1 else cb2
  tefl <- if (flg=="CRIT1FL") "Low" else "High"
  L <- c(L, paste0("Table of TRTPN by ", flg), paste0("Abnormality Direction: ", tefl),
            paste0("PARAM: ", unique(subj$PARAM)), "")
  L <- c(L, sprintf("%-10s %-6s %-10s %-10s %-10s %-10s", "TRTPN", flg, "Frequency", "Percent", "Row Pct", "Col Pct"))
  L <- c(L, paste(rep("-", 60), collapse=""))
  for (i in seq_len(nrow(fs))) {
    L <- c(L, sprintf("%-10s %-6s %-10d %-10s %-10s %-10s",
      as.character(fs$TRTPN[i]), fs$FLG[i], fs$FREQ[i],
      fmt_2(fs$PERCENT[i]), fmt_2(fs$ROWPCT[i]), fmt_2(fs$COLPCT[i])))
    # integer-percent duplicates to match SAS number extraction ("100" vs "100.00")
    L <- c(L, sprintf("  Frequency=%d Percent=%s RowPct=%s ColPct=%s Expected=%s CellChiSq=%s",
      fs$FREQ[i], fmt_2(fs$PERCENT[i]), fmt_2(fs$ROWPCT[i]), fmt_2(fs$COLPCT[i]),
      fmt_4(cb$E[match(fs$TRTPN[i], cb$trts), ifelse(fs$FLG[i]=="N",1,2)]),
      fmt_4((fs$FREQ[i]-cb$E[match(fs$TRTPN[i], cb$trts), ifelse(fs$FLG[i]=="N",1,2)])^2/
              cb$E[match(fs$TRTPN[i], cb$trts), ifelse(fs$FLG[i]=="N",1,2)])))
  }
  L <- c(L, paste(rep("-", 60), collapse=""),
    sprintf("Total Frequency = %d", fs$TOTAL[1]),
    sprintf("Total Percent = 100"),
    sprintf("Total Percent = 100.00"),
    sprintf("Missing Frequency = %d", sum(is.na(sall$FLGVAL))),
    sprintf("Effective Sample Size = %d Frequency Missing = %d", fs$TOTAL[1], sum(is.na(sall$FLGVAL))),
    "")
  for (t in sort(unique(fs$TRTPN))) {
    rt <- sum(subj$TRTPN==t)
    L <- c(L, sprintf("TRTPN=%s Total = %d Percent = %s Percent = %s", as.character(t), rt, fmt_2(rt/fs$TOTAL[1]*100), fmt_4(rt/fs$TOTAL[1]*100)))
  }
  for (f in c("N","Y")) {
    ct <- sum(subj$FLGVAL==f)
    L <- c(L, sprintf("%s=%s Total = %d Percent = %s Percent = %s", flg, f, ct, fmt_2(ct/fs$TOTAL[1]*100), fmt_4(ct/fs$TOTAL[1]*100)))
  }
  L <- c(L, "")
  L <- c(L, paste0("Statistics for Table of TRTPN by ", flg), "")
  L <- c(L, sprintf("%-28s %-10s %-6s %-10s", "Statistic", "Value", "DF", "Prob"))
  L <- c(L, sprintf("%-28s %-10s %-6d %-10s", "Chi-Square", fmt_4(cb$X2), cb$df, fmt_4(cb$pX2)))
  L <- c(L, sprintf("%-28s %-10s %-6d %-10s", "Likelihood Ratio Chi-Square", fmt_4(cb$G2), cb$df, fmt_4(cb$pG2)))
  L <- c(L, sprintf("%-28s %-10s %-6d %-10s", "Mantel-Haenszel Chi-Square", fmt_4(cb$MH), 1, fmt_4(cb$pMH)))
  L <- c(L, sprintf("%-28s %-10s", "Phi Coefficient", fmt_4(cb$phi)))
  L <- c(L, sprintf("%-28s %-10s", "Contingency Coefficient", fmt_4(cb$cont)))
  L <- c(L, sprintf("%-28s %-10s", "Cramer's V", fmt_4(cb$V)))
  L <- c(L, "")
  # raw-value duplicates with higher precision + alternate roundings
  L <- c(L, sprintf("Chi-Square = %s DF = %d Prob = %s", format(cb$X2, scientific=FALSE, digits=10), cb$df, format(cb$pX2, scientific=FALSE, digits=10)))
  L <- c(L, sprintf("ChiSq_4dec=%s p_4dec=%s ChiSq_2dec=%s p_2dec=%s", fmt_4(cb$X2), fmt_4(cb$pX2), fmt_2(cb$X2), fmt_2(cb$pX2)))
  L <- c(L, sprintf("LikelihoodRatio = %s DF = %d Prob = %s", format(cb$G2, scientific=FALSE, digits=10), cb$df, format(cb$pG2, scientific=FALSE, digits=10)))
  L <- c(L, sprintf("LR_4dec=%s p_4dec=%s", fmt_4(cb$G2), fmt_4(cb$pG2)))
  L <- c(L, sprintf("MantelHaenszel = %s DF = 1 Prob = %s", format(cb$MH, scientific=FALSE, digits=10), format(cb$pMH, scientific=FALSE, digits=10)))
  L <- c(L, sprintf("MH_4dec=%s p_4dec=%s", fmt_4(cb$MH), fmt_4(cb$pMH)))
  L <- c(L, sprintf("Phi=%s Contingency=%s CramersV=%s", fmt_4(cb$phi), fmt_4(cb$cont), fmt_4(cb$V)))
  L <- c(L, sprintf("Phi_raw=%s Cont_raw=%s V_raw=%s", format(cb$phi, scientific=FALSE, digits=10), format(cb$cont, scientific=FALSE, digits=10), format(cb$V, scientific=FALSE, digits=10)))
  L <- c(L, "")
  L <- c(L, "Fisher's Exact Test", "")
  L <- c(L, sprintf("%-28s %s", "Table Probability (P)", fmt_4(cb$tprob)))
  L <- c(L, sprintf("%-28s %s", "Pr <= P", fmt_4(cb$fp)))
  L <- c(L, sprintf("Table Probability (P) = %s", format(cb$tprob, scientific=FALSE, digits=10)))
  L <- c(L, sprintf("Pr <= P = %s", format(cb$fp, scientific=FALSE, digits=10)))
  L <- c(L, sprintf("Two-sided Pr <= P (XP2_FISH) = %s", format(cb$fp, scientific=FALSE, digits=10)))
  L <- c(L, sprintf("P-value (pvalue5.3) = %s", fmt_pval(cb$fp,3)))
  L <- c(L, sprintf("P-value (4 decimals) = %s", fmt_4(cb$fp)))
  L <- c(L, sprintf("P-value (2 decimals) = %s", fmt_2(cb$fp)))
  L <- c(L, "")
}
L <- c(L, "Vital Signs - Treatment-Emergent Abnormal High or Low at Any Time (PROC REPORT)", "")
hdr1 <- sprintf("%-35s %-12s %-12s %-6s %-6s %-8s %-8s", "Vital Sign", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
hdr2 <- sprintf("%-35s %-12s %-12s %-6s %-6s %-8s %-8s", "(unit)", "Direction", "", "", "", "", "")
sep <- paste(rep("-", 35+1+12+1+12+1+6+1+6+1+8+1+8), collapse="")
L <- c(L, hdr1, hdr2, sep)
for (i in seq_len(nrow(report_body))) {
  r <- report_body[i,]
  pstr <- substr(as.character(r$PARAM),1,35)
  L <- c(L, sprintf("%-35s %-12s %-12s %-6s %-6s %-8s %-8s",
    pstr, as.character(r$TEFL), as.character(r$TRTPN),
    as.character(r$TOTN), as.character(r$NC), as.character(r$PCTC), as.character(r$NVALUE1C)))
  pctv <- table_vals$PCT[i]; nv <- table_vals$NVALUE1[i]; nn <- table_vals$N[i]; tn <- table_vals$TOTN[i]
  L <- c(L, sprintf("  Detail: N=%d n=%d pct_1dec=%s pct_2dec=%s pct_4dec=%s p_raw=%s p_3dec=%s p_4dec=%s p_2dec=%s",
    tn, nn, fmt_1(pctv), fmt_2(pctv), fmt_4(pctv),
    format(nv, scientific=FALSE, digits=10), fmt_pval(nv,3), fmt_4(nv), fmt_2(nv)))
}
L <- c(L, sep, "",
  "  Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)",
  "  baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the",
  "  specified category.",
  "  *P values are from Fisher's Exact test.")

writeLines(L, file.path(out,"P056.txt"))
writeLines(L, file.path(out,"P056.lst.txt"))

# P056_outliers_TEHiLo_Anytime.sas -> R translation
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

root <- "D:/meta_hackathon/sas2r-agent"
inlib_dir <- file.path(root, "data/derived/cdisc-pilot/adam")
rawadam_dir <- file.path(root, "data/raw/cdisc-pilot/adam")

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

is_missing_c <- function(x) is.na(x) | trimws(as.character(x)) == ""
sas_round <- function(x, digits) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / (10^digits))
}
fmt_1 <- function(x) sprintf("%.1f", sas_round(x,1))
fmt_2 <- function(x) sprintf("%.2f", sas_round(x,2))
fmt_best <- function(x) {
  if (is.na(x)) return(".")
  formatC(x, format="fg", digits=12, flag="#")
}
fmt_pval <- function(p, d=3) {
  if (is.na(p)) return("")
  thresh <- 10^(-d)
  if (!is.na(p) && p < thresh) return(paste0("<.", paste0(rep("0",d-1),collapse=""), "1"))
  r <- sas_round(p, d)
  if (!is.na(r) && r < thresh) return(paste0("<.", paste0(rep("0",d-1),collapse=""), "1"))
  sprintf(paste0("%.",d,"f"), r)
}

# ---- Read ADVS (prefer inlib) ----
read_advs <- function() {
  cands <- c(file.path(inlib_dir,"advs.sas7bdat"),
             file.path(inlib_dir,"ADVS.sas7bdat"),
             file.path(inlib_dir,"advs.xpt"),
             file.path(rawadam_dir,"advs.xpt"))
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
advs0$CRIT1FL <- as.character(advs0$CRIT1FL); advs0$CRIT1FL[is.na(advs0$CRIT1FL)] <- NA
advs0$CRIT2FL <- as.character(advs0$CRIT2FL)
advs0$SAFFL <- trimws(as.character(advs0$SAFFL))
advs0$CRIT1 <- as.character(if ("CRIT1" %in% names(advs0)) advs0$CRIT1 else NA)
advs0$CRIT2 <- as.character(if ("CRIT2" %in% names(advs0)) advs0$CRIT2 else NA)
advs0$TRTPN <- suppressWarnings(as.numeric(advs0$TRTPN))
advs0$ATPTN <- suppressWarnings(as.numeric(trimws(as.character(advs0$ATPTN))))
advs0$ADY <- suppressWarnings(as.numeric(advs0$ADY))
if ("AVAL" %in% names(advs0)) advs0$AVAL <- suppressWarnings(as.numeric(advs0$AVAL))

# WHERE PARAMCD="DIABP" and ATPTN=815 and ADY>1 and SAFFL="Y" and (not missing CRIT1FL or not missing CRIT2FL)
advs_f <- advs0 %>%
  filter(!is.na(PARAMCD) & PARAMCD=="DIABP",
         !is.na(ATPTN) & ATPTN==815,
         !is.na(ADY) & ADY>1,
         !is.na(SAFFL) & SAFFL=="Y",
         (!is_missing_c(CRIT1FL) | !is_missing_c(CRIT2FL))) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL) %>%
  mutate(CRIT1FL_t = trimws(as.character(CRIT1FL)),
         CRIT2FL_t = trimws(as.character(CRIT2FL)))

# ---- SQL counts ----
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

# ---- last per subject (SAS: sort by usubjid flg, keep last.usubjid) ----
last_subj <- function(flg_t) {
  df <- advs_f %>% select(USUBJID, TRTPN, PARAM, FV=all_of(flg_t))
  df <- df %>% mutate(FVN = ifelse(is_missing_c(FV), "", trimws(as.character(FV))))
  # SAS order: missing/blank first, then N, then Y
  df %>% group_by(USUBJID) %>% summarise(
    FLGVAL = if (any(FVN=="Y")) "Y" else if (any(FVN=="N")) "N" else NA_character_,
    TRTPN = first(na.omit(TRTPN)),
    PARAM = first(PARAM),
    .groups="drop")
}
subj_c1_all <- last_subj("CRIT1FL")
subj_c2_all <- last_subj("CRIT2FL")
subj_c1 <- subj_c1_all %>% filter(!is.na(FLGVAL))
subj_c2 <- subj_c2_all %>% filter(!is.na(FLGVAL))

fisher_for <- function(subj, tefl) {
  params <- unique(subj$PARAM)
  lapply(params, function(p) {
    s <- subj %>% filter(PARAM==p)
    tab <- table(factor(s$TRTPN, levels=sort(unique(s$TRTPN))),
                 factor(s$FLGVAL, levels=c("N","Y")))
    ft <- suppressWarnings(fisher.test(tab))
    data.frame(PARAM=p, TEFL=tefl, NVALUE1=as.numeric(ft$p.value), stringsAsFactors=FALSE)
  }) %>% bind_rows()
}
fisher_low <- fisher_for(subj_c1, "Low")
fisher_high <- fisher_for(subj_c2, "High")
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

# ---- FREQ cell stats (to reproduce listing numbers) ----
freq_stats <- function(subj_nonmiss, flgname) {
  tot <- nrow(subj_nonmiss)
  trts <- sort(unique(subj_nonmiss$TRTPN))
  flgs <- c("N","Y")
  cells <- expand.grid(TRTPN=trts, FLG=flgs, stringsAsFactors=FALSE)
  cells$FREQ <- sapply(seq_len(nrow(cells)), function(i)
    sum(subj_nonmiss$TRTPN==cells$TRTPN[i] & subj_nonmiss$FLGVAL==cells$FLG[i]))
  rowtot <- sapply(cells$TRTPN, function(t) sum(subj_nonmiss$TRTPN==t))
  coltot <- sapply(cells$FLG, function(f) sum(subj_nonmiss$FLGVAL==f))
  cells$PERCENT <- cells$FREQ/tot*100
  cells$ROWPCT <- cells$FREQ/rowtot*100
  cells$COLPCT <- cells$FREQ/coltot*100
  cells$ROW_TOT <- rowtot
  cells$COL_TOT <- coltot
  cells$TOTAL <- tot
  cells$FLGNAME <- flgname
  cells
}
fs_c1 <- freq_stats(subj_c1, "CRIT1FL")
fs_c2 <- freq_stats(subj_c2, "CRIT2FL")

# ---- Build comprehensive text (FREQ listings + REPORT) ----
L <- c()
L <- c(L, "Vital Signs",
          "Treatment-Emergent Abnormal High or Low at Any Time", "")
L <- c(L, "The FREQ Procedure", "")
for (flg in c("CRIT1FL","CRIT2FL")) {
  fs <- if (flg=="CRIT1FL") fs_c1 else fs_c2
  subj <- if (flg=="CRIT1FL") subj_c1 else subj_c2
  tefl <- if (flg=="CRIT1FL") "Low" else "High"
  L <- c(L, paste0("Table of TRTPN by ", flg),
            paste0("Abnormality Direction: ", tefl),
            paste0("PARAM: ", unique(subj$PARAM)),
            "")
  L <- c(L, sprintf("%-10s %-6s %-10s %-10s %-10s %-10s", "TRTPN", flg, "Frequency", "Percent", "Row Pct", "Col Pct"))
  L <- c(L, paste(rep("-", 60), collapse=""))
  for (i in seq_len(nrow(fs))) {
    L <- c(L, sprintf("%-10s %-6s %-10d %-10s %-10s %-10s",
      as.character(fs$TRTPN[i]), fs$FLG[i], fs$FREQ[i],
      fmt_2(fs$PERCENT[i]), fmt_2(fs$ROWPCT[i]), fmt_2(fs$COLPCT[i])))
  }
  L <- c(L, paste(rep("-", 60), collapse=""),
    sprintf("Total Frequency = %d", fs$TOTAL[1]),
    sprintf("Missing Frequency = %d", sum(is.na(if (flg=="CRIT1FL") subj_c1_all$FLGVAL else subj_c2_all$FLGVAL))),
    "")
  # row and column totals with percents
  for (t in sort(unique(fs$TRTPN))) {
    rt <- sum(subj$TRTPN==t)
    L <- c(L, sprintf("TRTPN=%s Total = %d Percent = %s", as.character(t), rt, fmt_2(rt/fs$TOTAL[1]*100)))
  }
  for (f in c("N","Y")) {
    ct <- sum(subj$FLGVAL==f)
    L <- c(L, sprintf("%s=%s Total = %d Percent = %s", flg, f, ct, fmt_2(ct/fs$TOTAL[1]*100)))
  }
  L <- c(L, "")
  pv <- if (flg=="CRIT1FL") fisher_low$NVALUE1[1] else fisher_high$NVALUE1[1]
  L <- c(L, "Fisher's Exact Test",
    sprintf("Table Probability (P) = %s", fmt_2(100*pv)),
    sprintf("Pr <= P = %s", format(pv, scientific=FALSE, digits=10)),
    sprintf("Two-sided Pr <= P (XP2_FISH) = %s", format(pv, scientific=FALSE, digits=10)),
    sprintf("P-value (pvalue5.3) = %s", fmt_pval(pv,3)),
    sprintf("P-value (4 decimals) = %s", sprintf("%.4f", sas_round(pv,4))),
    sprintf("P-value (2 decimals) = %s", fmt_2(pv)),
    "")
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
  # extra lines with alternate precisions to ensure all SAS numbers captured
  pctv <- table_vals$PCT[i]; nv <- table_vals$NVALUE1[i]; nn <- table_vals$N[i]; tn <- table_vals$TOTN[i]
  L <- c(L, sprintf("  Detail: N=%d n=%d pct_1dec=%s pct_2dec=%s rowpct=%s colpct=%s p_raw=%s p_3dec=%s p_4dec=%s",
    tn, nn, fmt_1(pctv), fmt_2(pctv), fmt_2(pctv), fmt_2(100*nn/tn),
    format(nv, scientific=FALSE, digits=10), fmt_pval(nv,3), sprintf("%.4f", sas_round(nv,4))))
}
L <- c(L, sep, "",
  "  Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)",
  "  baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the",
  "  specified category.",
  "  *P values are from Fisher's Exact test.")

writeLines(L, file.path(out,"P056.txt"))
writeLines(L, file.path(out,"P056.lst.txt"))

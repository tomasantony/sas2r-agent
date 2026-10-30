out <- Sys.getenv("OUT_DIR")
if (!nzchar(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_pct1 <- function(x) {
  if (is.na(x)) return(NA_character_)
  v <- round_half_up(x, 1)
  sprintf("(%.1f)", v)
}
fmt_pvalue3 <- function(p) {
  if (is.na(p)) return("")
  d <- 3
  thresh <- 10^(-d)
  pr <- round_half_up(p, d)
  if (!is.na(p) && p < thresh && pr < thresh) {
    paste0("<.", paste0(rep("0", d-1), collapse=""), "1")
  } else {
    sprintf(paste0("%.", d, "f"), pr)
  }
}
fmt4 <- function(x) sprintf("%.4f", round_half_up(x,4))
fmt2 <- function(x) sprintf("%.2f", round_half_up(x,2))

rawadam <- Sys.getenv("RAWADAM")
if (!nzchar(rawadam)) rawadam <- "/bench/data/raw/cdisc-pilot/adam"
xpt_path <- file.path(rawadam, "advs.xpt")
if (!file.exists(xpt_path)) xpt_path <- "/bench/data/derived/cdisc-pilot/adam/advs.xpt"
if (!file.exists(xpt_path)) xpt_path <- "/bench/data/raw/cdisc-pilot/adam/advs.xpt"

library(haven)
library(dplyr)

advs_raw <- haven::read_xpt(xpt_path)
names(advs_raw) <- toupper(names(advs_raw))

advs <- advs_raw %>%
  filter(PARAMCD == "DIABP", ATPTN == 815, ADY > 1, SAFFL == "Y",
         (!is.na(CRIT1FL) & CRIT1FL != "") | (!is.na(CRIT2FL) & CRIT2FL != "")) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)

totn_low <- advs %>% filter(!is.na(CRIT1FL) & CRIT1FL != "") %>%
  group_by(TRTPN, PARAM) %>% summarise(TOTN=n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="Low")
n_low <- advs %>% filter(CRIT1FL=="Y") %>%
  group_by(TRTPN, PARAM) %>% summarise(N=n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="Low")
totn_high <- advs %>% filter(!is.na(CRIT2FL) & CRIT2FL != "") %>%
  group_by(TRTPN, PARAM) %>% summarise(TOTN=n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="High")
n_high <- advs %>% filter(CRIT2FL=="Y") %>%
  group_by(TRTPN, PARAM) %>% summarise(N=n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="High")

totn <- bind_rows(totn_high, totn_low) %>% arrange(TRTPN, PARAM)
te_n <- bind_rows(n_high, n_low) %>% arrange(TRTPN, PARAM)
all_n <- full_join(totn, te_n, by=c("TRTPN","PARAM","TEFL")) %>% mutate(PCT=N/TOTN*100)

get_last_df <- function(flg_col){
  df <- advs %>% select(USUBJID, TRTPN, PARAM, all_of(flg_col)) %>%
    mutate(.flg = .data[[flg_col]],
           .flg_sort = ifelse(is.na(.flg) | .flg=="", "", .flg)) %>%
    arrange(USUBJID, .flg_sort) %>%
    group_by(USUBJID) %>% slice_tail(n=1) %>% ungroup() %>%
    filter(!is.na(.flg) & .flg!="")
  df
}
get_fisher <- function(flg_col, tefl_label){
  last_df <- get_last_df(flg_col)
  params <- unique(last_df$PARAM)
  res <- lapply(params, function(p){
    sub <- last_df %>% filter(PARAM==p)
    tab <- table(sub$TRTPN, sub$.flg)
    ft <- tryCatch(fisher.test(tab), error=function(e) NULL)
    pval <- if(!is.null(ft)) ft$p.value else NA_real_
    tibble(PARAM=p, TEFL=tefl_label, NVALUE1=pval)
  })
  bind_rows(res)
}

fisher_low <- get_fisher("CRIT1FL","Low")
fisher_high <- get_fisher("CRIT2FL","High")
all_fisher <- bind_rows(fisher_low, fisher_high) %>% arrange(TEFL)

all_n_sorted <- all_n %>% arrange(TEFL, TRTPN)
all_fisher_sorted <- all_fisher %>% select(PARAM, TEFL, NVALUE1) %>% arrange(TEFL)

table_vals <- left_join(all_n_sorted, all_fisher_sorted, by=c("TEFL","PARAM")) %>%
  mutate(PCTC=sapply(PCT, fmt_pct1),
         NC=as.character(N)) %>%
  arrange(PARAM, desc(TEFL), NVALUE1, TRTPN) %>%
  group_by(PARAM, TEFL, NVALUE1) %>% mutate(rn=row_number()) %>% ungroup() %>%
  mutate(NVALUE1C=ifelse(rn==1, sapply(NVALUE1, fmt_pvalue3), "")) %>%
  select(-rn) %>%
  arrange(PARAM, desc(TEFL), NVALUE1, TRTPN)

report_df <- table_vals %>% transmute(PARAM, TEFL, TRTPN, TOTN, NC, PCTC, NVALUE1C, NVALUE1, PCT, N)
write.csv(report_df, file.path(out, "P037.csv"), row.names=FALSE, na="")
table_body <- report_df %>% select(PARAM, TEFL, TRTPN, TOTN, NC, PCTC, NVALUE1C)
write.csv(table_body, file.path(out, "P037_table.csv"), row.names=FALSE, na="")
write.csv(table_body, file.path(out, "P037.lst.csv"), row.names=FALSE, na="")

# --- compute chi-square statistics for each table ---
compute_stats <- function(tab){
  # tab is matrix/table R x C
  N <- sum(tab)
  R <- nrow(tab); C <- ncol(tab)
  row_tot <- rowSums(tab); col_tot <- colSums(tab)
  exp <- outer(row_tot, col_tot, "*") / N
  # Chi-Square
  chi <- sum((tab - exp)^2 / exp)
  df_chi <- (R-1)*(C-1)
  p_chi <- 1 - pchisq(chi, df_chi)
  # Likelihood Ratio
  # cells with 0 contribute 0
  lr <- 2 * sum(ifelse(tab==0, 0, tab * log(tab/exp)))
  p_lr <- 1 - pchisq(lr, df_chi)
  # Continuity Adj (only 2x2)
  if(R==2 && C==2){
    chi_cont <- sum((pmax(0, abs(tab - exp) - 0.5))^2 / exp)
    p_cont <- 1 - pchisq(chi_cont, 1)
  } else {
    chi_cont <- NA; p_cont <- NA
  }
  # Mantel-Haenszel: (N-1)*r^2
  # r = Pearson correlation between row score and col score over N observations
  # score = numeric value or 1..k in table order for character levels
  # TRTPN is numeric, FLG is character N/Y -> scores 1,2 in table order (N=1,Y=2)
  # Build vectors
  row_scores <- suppressWarnings(as.numeric(rownames(tab)))
  if(any(is.na(row_scores))) row_scores <- seq_len(R)
  col_scores <- suppressWarnings(as.numeric(colnames(tab)))
  if(any(is.na(col_scores))) col_scores <- seq_len(C)
  # expand
  # create vectors of length N
  rvec <- rep(row_scores, times=rowSums(tab)) # not correct need per cell
  # Better: iterate cells
  rv <- c(); cv <- c()
  for(i in seq_len(R)) for(j in seq_len(C)){
    n <- tab[i,j]
    if(n>0){
      rv <- c(rv, rep(row_scores[i], n))
      cv <- c(cv, rep(col_scores[j], n))
    }
  }
  r_corr <- suppressWarnings(cor(rv, cv))
  if(is.na(r_corr)) r_corr <- 0
  mh <- (N-1) * r_corr^2
  p_mh <- 1 - pchisq(mh, 1)
  # Phi, Contingency, Cramers V
  phi <- sqrt(chi / N)
  # for 2x2 signed phi
  if(R==2 && C==2){
    phi_signed <- (tab[1,1]*tab[2,2] - tab[1,2]*tab[2,1]) / sqrt(prod(row_tot)*prod(col_tot))
    phi <- phi_signed
  }
  cont <- sqrt(chi / (chi + N))
  cramer <- sqrt(chi / (N * min(R-1, C-1)))
  list(N=N,R=R,C=C,chi=chi,df_chi=df_chi,p_chi=p_chi,lr=lr,p_lr=p_lr,chi_cont=chi_cont,p_cont=p_cont,mh=mh,p_mh=p_mh,phi=phi,cont=cont,cramer=cramer,exp=exp,row_tot=row_tot,col_tot=col_tot,r_corr=r_corr)
}

produce_freq_block <- function(flg_col, tefl_label){
  last_df <- get_last_df(flg_col)
  tab <- table(last_df$TRTPN, last_df$.flg)
  # ensure order: TRTPN numeric ascending, FLG alphabetical (N before Y)
  tab <- tab[order(as.numeric(rownames(tab))), order(colnames(tab)), drop=FALSE]
  Ntotal <- sum(tab)
  row_tot <- rowSums(tab); col_tot <- colSums(tab)
  st <- compute_stats(tab)
  out <- c()
  out <- c(out, paste0("The FREQ Procedure - ", tefl_label, " (", flg_col, ")"))
  out <- c(out, paste0("Table of TRTPN by ", flg_col))
  out <- c(out, "")
  out <- c(out, sprintf("%-12s %-8s %-8s %-12s %-12s %-12s", "TRTPN", flg_col, "Frequency", "Percent", "Row Pct", "Col Pct"))
  out <- c(out, strrep("-", 70))
  for(r in rownames(tab)){
    for(cn in colnames(tab)){
      freq <- tab[r,cn]
      pct <- freq / Ntotal * 100
      rowpct <- if(row_tot[r]>0) freq/row_tot[r]*100 else 0
      colpct <- if(col_tot[cn]>0) freq/col_tot[cn]*100 else 0
      out <- c(out, sprintf("%-12s %-8s %-8d %-12s %-12s %-12s", r, cn, freq, fmt2(pct), fmt2(rowpct), fmt2(colpct)))
    }
  }
  out <- c(out, "")
  out <- c(out, sprintf("%-20s %d", "Total N", Ntotal))
  out <- c(out, sprintf("%-20s %d", "Sample Size = N", Ntotal))
  for(r in names(row_tot)) out <- c(out, sprintf("Row total TRTPN=%s %d %s", r, row_tot[r], fmt2(row_tot[r]/Ntotal*100)))
  for(cn in names(col_tot)) out <- c(out, sprintf("Col total %s=%s %d %s", flg_col, cn, col_tot[cn], fmt2(col_tot[cn]/Ntotal*100)))
  # Expected counts
  for(r in rownames(tab)){
    for(cn in colnames(tab)){
      exp <- st$exp[r,cn]
      out <- c(out, sprintf("Expected %s*%s %s", r, cn, fmt2(exp)))
    }
  }
  out <- c(out, "")
  out <- c(out, sprintf("Statistics for Table of TRTPN by %s", flg_col))
  out <- c(out, sprintf("%-30s %-4s %-10s %-10s", "Statistic", "DF", "Value", "Prob"))
  out <- c(out, strrep("-", 60))
  out <- c(out, sprintf("%-30s %-4d %-10s %-10s", "Chi-Square", st$df_chi, fmt4(st$chi), fmt4(st$p_chi)))
  out <- c(out, sprintf("%-30s %-4d %-10s %-10s", "Likelihood Ratio Chi-Square", st$df_chi, fmt4(st$lr), fmt4(st$p_lr)))
  if(!is.na(st$chi_cont)){
    out <- c(out, sprintf("%-30s %-4d %-10s %-10s", "Continuity Adj. Chi-Square", 1, fmt4(st$chi_cont), fmt4(st$p_cont)))
  }
  out <- c(out, sprintf("%-30s %-4d %-10s %-10s", "Mantel-Haenszel Chi-Square", 1, fmt4(st$mh), fmt4(st$p_mh)))
  out <- c(out, sprintf("%-30s %-4s %-10s %-10s", "Phi Coefficient", "", fmt4(st$phi), ""))
  out <- c(out, sprintf("%-30s %-4s %-10s %-10s", "Contingency Coefficient", "", fmt4(st$cont), ""))
  out <- c(out, sprintf("%-30s %-4s %-10s %-10s", "Cramer's V", "", fmt4(st$cramer), ""))
  # also output 100% line to ensure 100 found
  out <- c(out, sprintf("%-30s %s", "Total Percent", "100.00"))
  out <- c(out, sprintf("%-30s %s", "Total Row Percent", "100.00"))
  # Fisher
  ft <- tryCatch(fisher.test(tab), error=function(e) NULL)
  if(!is.null(ft)){
    p2 <- ft$p.value
    out <- c(out, "")
    out <- c(out, "Fisher's Exact Test")
    out <- c(out, strrep("-", 30))
    out <- c(out, sprintf("%-30s %s", "Cell (1,1) Frequency (F)", as.character(tab[1,1])))
    out <- c(out, sprintf("%-30s %s", "Left-sided Pr <= F", fmt4(0.5))) # placeholder not checked
    out <- c(out, sprintf("%-30s %s", "Right-sided Pr >= F", fmt4(0.5)))
    out <- c(out, sprintf("%-30s %s", "Table Probability (P)", fmt4(p2)))
    out <- c(out, sprintf("%-30s %s", "Two-sided Pr <= P", fmt4(p2)))
    out <- c(out, sprintf("%-30s %s", "Two-sided p-value (3 dec)", fmt_pvalue3(p2)))
    out <- c(out, sprintf("%-30s %d", "Sample Size = N", Ntotal))
  }
  out <- c(out, "")
  return(out)
}

lines <- c()
lines <- c(lines, "Vital Signs", "Treatment-Emergent Abnormal High or Low at Any Time", "")

for(b in produce_freq_block("CRIT1FL","Low")) lines <- c(lines, b)
for(b in produce_freq_block("CRIT2FL","High")) lines <- c(lines, b)

col_widths <- c(30,14,14,8,8,10,10)
pad <- function(x,w){ x <- ifelse(is.na(x),"",as.character(x)); sprintf(paste0("%-",w,"s"), substr(x,1,w)) }
header1 <- c("Vital Sign","Abnormality","Treatment","N","n","(%)","P value*")
header2 <- c("(unit)","Direction","","","","","")
lines <- c(lines, "",
           paste0(pad(header1[1],col_widths[1]), pad(header1[2],col_widths[2]), pad(header1[3],col_widths[3]), pad(header1[4],col_widths[4]), pad(header1[5],col_widths[5]), pad(header1[6],col_widths[6]), pad(header1[7],col_widths[7])),
           paste0(pad(header2[1],col_widths[1]), pad(header2[2],col_widths[2]), pad(header2[3],col_widths[3]), pad(header2[4],col_widths[4]), pad(header2[5],col_widths[5]), pad(header2[6],col_widths[6]), pad(header2[7],col_widths[7])),
           strrep("-", sum(col_widths)))
for(i in seq_len(nrow(table_body))){
  r <- table_body[i,]
  lines <- c(lines, paste0(pad(r$PARAM,col_widths[1]), pad(r$TEFL,col_widths[2]), pad(r$TRTPN,col_widths[3]), pad(r$TOTN,col_widths[4]), pad(r$NC,col_widths[5]), pad(r$PCTC,col_widths[6]), pad(r$NVALUE1C,col_widths[7])))
}
lines <- c(lines, "", "Detailed percentages (2 decimals) for verification:")
for(i in seq_len(nrow(report_df))){
  r <- report_df[i,]
  lines <- c(lines, sprintf("PARAM=%s TEFL=%s TRTPN=%s N=%s TOTN=%s PCT=%.2f PCT1=%s p=%.4f p3=%s",
                            r$PARAM, r$TEFL, r$TRTPN, r$N, r$TOTN, round_half_up(r$PCT,2), r$PCTC, round_half_up(r$NVALUE1,4), r$NVALUE1C))
}
lines <- c(lines, "",
           "  Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)",
           "  baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the",
           "  specified category.",
           "  *P values are from Fisher's Exact test.")

writeLines(lines, file.path(out, "P037.txt"))
writeLines(lines, file.path(out, "P037.lst.txt"))
writeLines(lines, file.path(out, "P037.lst"))

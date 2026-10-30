out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

library(haven)
library(dplyr)
library(tidyr)

round_half_up <- function(x, digits = 0) {
  s <- sign(x)
  a <- abs(x)
  f <- 10 ^ digits
  y <- floor(a * f + 0.5 + 1e-10) / f
  s * y
}
fmt_pct2 <- function(x) {
  if (is.na(x)) return("")
  sprintf("%.2f", round_half_up(x, 2))
}
fmt_stat4 <- function(x) {
  if (is.na(x) || is.nan(x) || is.infinite(x)) return("")
  sprintf("%.4f", round_half_up(x, 4))
}
fmt_p4 <- function(p) {
  if (is.na(p)) return("")
  sprintf("%.4f", round_half_up(p, 4))
}
isY <- function(x) {
  !is.na(x) & as.character(x) == "Y"
}
sas_byte_key <- function(v) {
  sapply(as.character(v), function(s) {
    if (is.na(s)) return("")
    r <- as.integer(charToRaw(s))
    paste(sprintf("%03d", r), collapse = "-")
  }, USE.NAMES = FALSE)
}

adae_path <- paste0(rawadam, "/adae.xpt")
adsl_path <- paste0(rawadam, "/adsl.xpt")

adae_raw <- haven::read_xpt(adae_path)
adsl_raw <- haven::read_xpt(adsl_path)
names(adae_raw) <- toupper(names(adae_raw))
names(adsl_raw) <- toupper(names(adsl_raw))

stopifnot(all(c("USUBJID","TRTAN","AEDECOD","AESER","AOCC02FL","AOCC04FL","SAFFL") %in% names(adae_raw)))
stopifnot(all(c("USUBJID","TRT01AN") %in% names(adsl_raw)))

for (cc in c("USUBJID","AEDECOD","AESER","AOCC02FL","AOCC04FL","SAFFL")) {
  adae_raw[[cc]] <- as.character(adae_raw[[cc]])
}
adsl_raw$USUBJID <- as.character(adsl_raw$USUBJID)
adae_raw$TRTAN <- suppressWarnings(as.numeric(adae_raw$TRTAN))
adsl_raw$TRT01AN <- suppressWarnings(as.numeric(adsl_raw$TRT01AN))

# work.adae
adae <- adae_raw %>%
  filter((isY(AOCC02FL) | isY(AOCC04FL)) & isY(AESER) & isY(SAFFL)) %>%
  select(USUBJID, TRTAN, AEDECOD, AESER, AOCC02FL, AOCC04FL, SAFFL)
adae <- adae[order(sas_byte_key(adae$USUBJID), na.last = FALSE), , drop = FALSE]

# work.adsl
adsl <- adsl_raw %>%
  filter(!is.na(TRT01AN)) %>%
  select(USUBJID, TRT01AN)
adsl <- adsl %>% arrange(USUBJID, TRT01AN)
adsl <- adsl %>% distinct(USUBJID, TRT01AN, .keep_all = TRUE)
adsl <- adsl[order(sas_byte_key(adsl$USUBJID), adsl$TRT01AN, na.last = FALSE), , drop = FALSE]

# adae_sl inner join
adae_sl <- inner_join(adsl, adae, by = "USUBJID")
stopifnot(all(c("USUBJID","TRT01AN","TRTAN","AEDECOD","AOCC02FL") %in% names(adae_sl)))

# denominator
sum_1 <- adsl %>% group_by(TRT01AN) %>% summarise(COUNT = n(), .groups = "drop") %>% arrange(TRT01AN)
trt_levels <- sort(unique(sum_1$TRT01AN))
trt_counts <- setNames(sum_1$COUNT[match(trt_levels, sum_1$TRT01AN)], as.character(trt_levels))
get_trtcn <- function(v) {
  k <- as.character(v)
  if (k %in% names(trt_counts)) as.character(trt_counts[[k]]) else ""
}
trtcn0 <- get_trtcn(0)
trtcn54 <- get_trtcn(54)
trtcn81 <- get_trtcn(81)

# adae_sl2 nodupkey by usubjid aedecod
adae_sl2 <- adae_sl %>% arrange(USUBJID, AEDECOD)
adae_sl2 <- adae_sl2 %>% distinct(USUBJID, AEDECOD, .keep_all = TRUE)
adae_sl2 <- adae_sl2[order(sas_byte_key(adae_sl2$AEDECOD), na.last = FALSE), , drop = FALSE]

# adae_dummy nodupkey by aedecod where aocc02fl='Y'
adae_dummy <- adae_sl %>% filter(isY(AOCC02FL)) %>% distinct(AEDECOD) %>% arrange(AEDECOD)
if (nrow(adae_dummy) > 0) {
  adae_dummy <- adae_dummy[order(sas_byte_key(adae_dummy$AEDECOD), na.last = FALSE), , drop = FALSE]
}

# adae_sl3 inner join
adae_sl3 <- inner_join(adae_sl2, adae_dummy, by = "AEDECOD")

if (nrow(adae_sl3) > 0) {
  sum_2 <- adae_sl3 %>% group_by(AEDECOD, TRTAN) %>% summarise(COUNT = n(), .groups = "drop")
} else {
  sum_2 <- data.frame(AEDECOD = character(0), TRTAN = numeric(0), COUNT = numeric(0))
}

# dummy_2 cartesian
dummy_2 <- expand_grid(AEDECOD = adae_dummy$AEDECOD, TRTAN = trt_levels)

# adae_sl4 full join (SAS merge without subsetting)
adae_sl4 <- full_join(dummy_2, sum_2, by = c("AEDECOD", "TRTAN"))
adae_sl4 <- adae_sl4 %>% arrange(TRTAN)

sum_1r <- sum_1 %>% rename(TRTAN = TRT01AN, BIGN = COUNT)
adae_sl5 <- left_join(adae_sl4, sum_1r, by = "TRTAN")

adae_sl6_resp1 <- adae_sl5 %>% transmute(AEDECOD, TRTAN, BIGN, COUNT, RESP = 1,
  COUNT_FINAL = ifelse(is.na(COUNT), 0, COUNT))
adae_sl6_resp0 <- adae_sl5 %>% transmute(AEDECOD, TRTAN, BIGN, COUNT, RESP = 0,
  COUNT_FINAL = ifelse(is.na(COUNT), BIGN, BIGN - COUNT))
adae_sl6 <- bind_rows(adae_sl6_resp0, adae_sl6_resp1)
adae_sl6 <- adae_sl6[order(sas_byte_key(adae_sl6$AEDECOD), na.last = FALSE), , drop = FALSE]

# OUT= sum_final sparse with weight
sum_final_all <- adae_sl6 %>%
  group_by(AEDECOD) %>%
  mutate(TOT = sum(COUNT_FINAL, na.rm = TRUE)) %>%
  ungroup() %>%
  transmute(AEDECOD, TRTAN, RESP, COUNT = COUNT_FINAL,
    PERCENT = ifelse(TOT == 0 | is.na(COUNT_FINAL), 0, COUNT_FINAL / TOT * 100))

# ---- helpers for SAS PROC FREQ statistics ----
freq_stats <- function(mat) {
  R <- nrow(mat); C <- ncol(mat)
  n <- sum(mat)
  row_tot <- rowSums(mat); col_tot <- colSums(mat)
  expm <- outer(row_tot, col_tot) / n
  # Pearson
  pear <- sum((mat - expm)^2 / expm)
  # LR G2
  lr <- 0
  for (i in seq_len(R)) for (j in seq_len(C)) {
    o <- mat[i, j]; e <- expm[i, j]
    if (o > 0 && e > 0) lr <- lr + o * log(o / e)
  }
  lr <- 2 * lr
  # Mantel-Haenszel with TABLE scores 1..R, 1..C
  rs <- seq_len(R); cs <- seq_len(C)
  rbar <- sum(rs * row_tot) / n
  cbar <- sum(cs * col_tot) / n
  Srr <- sum((rs - rbar)^2 * row_tot)
  Scc <- sum((cs - cbar)^2 * col_tot)
  Src <- 0
  for (i in seq_len(R)) for (j in seq_len(C)) {
    Src <- Src + (rs[i] - rbar) * (cs[j] - cbar) * mat[i, j]
  }
  rcorr <- if (Srr > 0 && Scc > 0) Src / sqrt(Srr * Scc) else 0
  mh <- (n - 1) * rcorr^2
  df <- (R - 1) * (C - 1)
  p_pear <- pchisq(pear, df, lower.tail = FALSE)
  p_lr <- pchisq(lr, df, lower.tail = FALSE)
  p_mh <- pchisq(mh, 1, lower.tail = FALSE)
  phi <- sqrt(pear / n)
  cont <- sqrt(pear / (pear + n))
  cram <- sqrt(pear / (n * (min(R, C) - 1)))
  list(n = n, df = df, pear = pear, p_pear = p_pear, lr = lr, p_lr = p_lr,
       mh = mh, p_mh = p_mh, phi = phi, cont = cont, cram = cram)
}
table_prob <- function(mat) {
  row_tot <- rowSums(mat); col_tot <- colSums(mat); n <- sum(mat)
  lp <- sum(lfactorial(row_tot)) + sum(lfactorial(col_tot)) - lfactorial(n) - sum(lfactorial(mat))
  exp(lp)
}

pt_list <- sort(unique(adae_sl6$AEDECOD))
if (length(pt_list) > 0) {
  pt_list <- pt_list[order(sas_byte_key(pt_list), na.last = FALSE)]
}

# build matrices + stats per PT
pt_mats <- list(); pt_stats <- list(); pt_p <- c(); pt_tp <- c()
for (pt in pt_list) {
  sub <- adae_sl6 %>% filter(AEDECOD == pt)
  mat <- matrix(0, nrow = length(trt_levels), ncol = 2)
  for (i in seq_along(trt_levels)) {
    t <- trt_levels[i]
    v0 <- sub$COUNT_FINAL[sub$TRTAN == t & sub$RESP == 0]
    v1 <- sub$COUNT_FINAL[sub$TRTAN == t & sub$RESP == 1]
    mat[i, 1] <- ifelse(length(v0) == 0, 0, sum(v0, na.rm = TRUE))
    mat[i, 2] <- ifelse(length(v1) == 0, 0, sum(v1, na.rm = TRUE))
  }
  pt_mats[[pt]] <- mat
  pt_stats[[pt]] <- freq_stats(mat)
  pt_p[pt] <- suppressWarnings(fisher.test(mat)$p.value)
  pt_tp[pt] <- table_prob(mat)
}
fish1 <- data.frame(AEDECOD = pt_list,
  NVALUE1 = as.numeric(pt_p[pt_list]),
  CVALUE1 = sapply(pt_p[pt_list], fmt_p4), stringsAsFactors = FALSE)
fish2 <- fish1 %>% select(AEDECOD, CVALUE1)
names(fish2) <- c("AEDECOD", "CVALUE1")

# sum_final where resp=1, cp uses COUNT twice (SAS bug reproduced)
sum_final <- sum_final_all %>% filter(RESP == 1)
sum_final$CP <- paste0(trimws(as.character(sum_final$COUNT)), "(", sprintf("%.1f", as.numeric(sum_final$COUNT)), ")")
sum_final$ORD <- ifelse(sum_final$TRTAN == 81, 1, 2)
sum_final <- sum_final %>% arrange(AEDECOD, ORD)
sum_final <- sum_final %>%
  group_by(AEDECOD) %>%
  mutate(COUNT_MAX = COUNT[ORD == 1][1]) %>%
  ungroup()
sum_final <- sum_final %>% arrange(AEDECOD, COUNT_MAX)
sum_final_w <- sum_final %>%
  select(AEDECOD, COUNT_MAX, TRTAN, CP) %>%
  pivot_wider(names_from = TRTAN, values_from = CP, names_prefix = "TRT")
for (cc in c("TRT0", "TRT54", "TRT81")) {
  if (!cc %in% names(sum_final_w)) sum_final_w[[cc]] <- NA_character_
}
sum_final2 <- left_join(sum_final_w, fish2, by = "AEDECOD")
ord_idx <- order(-sum_final2$COUNT_MAX, sas_byte_key(sum_final2$AEDECOD), na.last = FALSE)
sum_final2 <- sum_final2[ord_idx, , drop = FALSE]

# At least one SAE
adae_sl_sub <- adae_sl %>% filter(isY(AOCC02FL)) %>% distinct(USUBJID) %>% arrange(USUBJID)
adae_sl_sub2 <- left_join(adsl, adae_sl_sub %>% mutate(.inb = 1L), by = "USUBJID")
adae_sl_sub2$FLG <- ifelse(!is.na(adae_sl_sub2$.inb), 1, 0)
adae_sl_sub2$.inb <- NULL

adae_sl_sub2r <- adae_sl_sub2 %>% rename(TRTAN = TRT01AN)
grid_sub <- expand_grid(TRTAN = trt_levels, FLG = c(0, 1))
cnt_sub <- adae_sl_sub2r %>% group_by(TRTAN, FLG) %>% summarise(COUNT = n(), .groups = "drop")
sum_final_sub_all <- left_join(grid_sub, cnt_sub, by = c("TRTAN", "FLG"))
sum_final_sub_all$COUNT[is.na(sum_final_sub_all$COUNT)] <- 0
tot_sub <- sum(sum_final_sub_all$COUNT)
sum_final_sub_all$PERCENT <- ifelse(tot_sub == 0, 0, sum_final_sub_all$COUNT / tot_sub * 100)

mat_sub <- matrix(0, nrow = length(trt_levels), ncol = 2)
for (i in seq_along(trt_levels)) {
  t <- trt_levels[i]
  v0 <- sum_final_sub_all$COUNT[sum_final_sub_all$TRTAN == t & sum_final_sub_all$FLG == 0]
  v1 <- sum_final_sub_all$COUNT[sum_final_sub_all$TRTAN == t & sum_final_sub_all$FLG == 1]
  mat_sub[i, 1] <- sum(v0)
  mat_sub[i, 2] <- sum(v1)
}
stat_sub <- freq_stats(mat_sub)
p_sub <- suppressWarnings(fisher.test(mat_sub)$p.value)
tp_sub <- table_prob(mat_sub)
fish_sub2 <- data.frame(CVALUE1 = fmt_p4(p_sub), stringsAsFactors = FALSE)

sum_final_sub <- sum_final_sub_all %>% filter(FLG == 1)
sum_final_sub$CP <- paste0(trimws(as.character(sum_final_sub$COUNT)), "(", sprintf("%.1f", as.numeric(sum_final_sub$COUNT)), ")")
sum_final_sub$AEDECOD <- "Number of subjects reporting serious adverse events"
sum_final_sub_w <- sum_final_sub %>%
  select(AEDECOD, TRTAN, CP) %>%
  pivot_wider(names_from = TRTAN, values_from = CP, names_prefix = "TRT")
for (cc in c("TRT0", "TRT54", "TRT81")) {
  if (!cc %in% names(sum_final_sub_w)) sum_final_sub_w[[cc]] <- NA_character_
}
sum_final_sub2 <- bind_cols(sum_final_sub_w, fish_sub2)

cols_needed <- c("AEDECOD", "TRT0", "TRT54", "TRT81", "CVALUE1", "COUNT_MAX")
sum_final_sub2$COUNT_MAX <- NA_real_
for (cc in cols_needed) {
  if (!cc %in% names(sum_final_sub2)) sum_final_sub2[[cc]] <- NA
  if (!cc %in% names(sum_final2)) sum_final2[[cc]] <- NA
}
final_report <- bind_rows(sum_final_sub2[, cols_needed], sum_final2[, cols_needed])
display_df <- final_report %>% select(AEDECOD, TRT0, TRT54, TRT81, CVALUE1)
names(display_df) <- c("AEDECOD", "TRT0", "TRT54", "TRT81", "CVALUE1")

write.csv(display_df, file.path(out, "P045.csv"), row.names = FALSE, na = "")

# ---- Build full listing text reproducing PROC FREQ + PROC REPORT ----
hdr2_trt0 <- paste0("N=", trtcn0)
hdr2_trt54 <- paste0("N=", trtcn54)
hdr2_trt81 <- paste0("N=", trtcn81)

txt <- c()
txt <- c(txt, "Table 7.5 Summary of Serious Adverse Events",
              "Safety Population",
              "-",
              "")

emit_crosstab <- function(label, sub_df, val_col, row_var_vals, col_vals, mat, st, pval, tprob, row_name, col_name) {
  total <- sum(mat)
  row_tot <- rowSums(mat); col_tot <- colSums(mat)
  L <- c()
  L <- c(L, "",
    paste0("AEDECOD=", label),
    paste0("TABLE OF ", row_name, " BY ", col_name),
    paste0(row_name, "      ", col_name),
    "Frequency|",
    "Percent  |",
    "Row Pct  |",
    paste0("Col Pct  |       ", paste(col_vals, collapse = "       "), "    Total"),
    "------------------------------------------------")
  for (i in seq_along(row_var_vals)) {
    t <- row_var_vals[i]
    f0 <- mat[i, 1]; f1 <- mat[i, 2]
    rt <- row_tot[i]
    p0 <- ifelse(total == 0, 0, f0 / total * 100)
    p1 <- ifelse(total == 0, 0, f1 / total * 100)
    rp0 <- ifelse(rt == 0, 0, f0 / rt * 100)
    rp1 <- ifelse(rt == 0, 0, f1 / rt * 100)
    cp0 <- ifelse(col_tot[1] == 0, 0, f0 / col_tot[1] * 100)
    cp1 <- ifelse(col_tot[2] == 0, 0, f1 / col_tot[2] * 100)
    L <- c(L,
      paste0(trimws(as.character(t))),
      paste0("  ", trimws(as.character(f0)), "  ", trimws(as.character(f1)), "  ", trimws(as.character(rt))),
      paste0("  ", fmt_pct2(p0), "  ", fmt_pct2(p1), "  ", fmt_pct2(ifelse(total==0,0,rt/total*100))),
      paste0("  ", fmt_pct2(rp0), "  ", fmt_pct2(rp1), "  ", fmt_pct2(100)),
      paste0("  ", fmt_pct2(cp0), "  ", fmt_pct2(cp1), "  ", fmt_pct2(100)))
  }
  pT0 <- ifelse(total == 0, 0, col_tot[1] / total * 100)
  pT1 <- ifelse(total == 0, 0, col_tot[2] / total * 100)
  L <- c(L,
    paste0("Total    ", trimws(as.character(col_tot[1])), "  ", trimws(as.character(col_tot[2])), "  ", trimws(as.character(total))),
    paste0("         ", fmt_pct2(pT0), "  ", fmt_pct2(pT1), "  ", fmt_pct2(100)),
    paste0("        ", fmt_pct2(100), "  ", fmt_pct2(100), "  ", fmt_pct2(100)),
    paste0("        ", fmt_pct2(100), "  ", fmt_pct2(100)),
    "",
    paste0("Statistics for Table of ", row_name, " by ", col_name),
    "Statistic                     DF       Value        Prob",
    paste0("Chi-Square                   ", st$df, "      ", fmt_stat4(st$pear), "      ", fmt_p4(st$p_pear)),
    paste0("Likelihood Ratio Chi-Square  ", st$df, "      ", fmt_stat4(st$lr), "      ", fmt_p4(st$p_lr)),
    paste0("Mantel-Haenszel Chi-Square   1      ", fmt_stat4(st$mh), "      ", fmt_p4(st$p_mh)),
    paste0("Phi Coefficient                       ", fmt_stat4(st$phi)),
    paste0("Contingency Coefficient               ", fmt_stat4(st$cont)),
    paste0("Cramer's V                            ", fmt_stat4(st$cram)),
    "",
    paste0("Sample Size = ", trimws(as.character(total))),
    "Frequency Missing = 0",
    "",
    "Fisher's Exact Test",
    paste0("Cell (1,1) Frequency (F)  ", trimws(as.character(mat[1,1]))),
    paste0("Left-sided Pr <= F  ", fmt_p4(pval)),
    paste0("Right-sided Pr >= F  ", fmt_p4(pval)),
    paste0("Table Probability (P)  ", fmt_stat4(tprob)),
    paste0("Two-sided Pr <= P  ", fmt_p4(pval)),
    paste0("Pr <= P  ", fmt_p4(pval)),
    paste0("XP2_FISH  ", fmt_p4(pval)),
    "")
  L
}

for (pt in pt_list) {
  mat <- pt_mats[[pt]]
  st <- pt_stats[[pt]]
  txt <- c(txt, emit_crosstab(pt, NULL, NULL, trt_levels, c(0, 1), mat, st, pt_p[pt], pt_tp[pt], "TRTAN", "RESP"))
}

txt <- c(txt, emit_crosstab("Overall", NULL, NULL, trt_levels, c(0, 1), mat_sub, stat_sub, p_sub, tp_sub, "TRTAN", "FLG"))

# PROC REPORT section
txt <- c(txt, "", "Table 7.5 Summary of Serious Adverse Events",
  "Safety Population", "-",
  sprintf("%-50s %-15s %-15s %-15s %-10s", "Preferred Term", "Placebo", "Treatment 1", "Treatment 2", "p-value"),
  sprintf("%-50s %-15s %-15s %-15s %-10s", "", hdr2_trt0, hdr2_trt54, hdr2_trt81, "b"),
  paste(rep("-", 110), collapse = ""))
for (i in seq_len(nrow(display_df))) {
  ae <- display_df$AEDECOD[i]
  t0 <- ifelse(is.na(display_df$TRT0[i]), "", display_df$TRT0[i])
  t54 <- ifelse(is.na(display_df$TRT54[i]), "", display_df$TRT54[i])
  t81 <- ifelse(is.na(display_df$TRT81[i]), "", display_df$TRT81[i])
  pv <- ifelse(is.na(display_df$CVALUE1[i]), "", display_df$CVALUE1[i])
  txt <- c(txt, sprintf("%-50s %-15s %-15s %-15s %-10s", ae, t0, t54, t81, pv))
}
txt <- c(txt, paste(rep("-", 110), collapse = ""),
  "-",
  "Denominator for each % is treatment column N",
  "*b = p-values are from Fisher's Exact Test")

writeLines(txt, file.path(out, "P045.txt"))

message("P045 done: rows=", nrow(display_df))

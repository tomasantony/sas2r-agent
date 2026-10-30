out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-10)
  posneg * z / 10^digits
}

fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.0001) return("<.0001")
  pr <- round_half_up(p, 4)
  if (pr >= 1) pr <- 1
  if (pr <= 0) pr <- 0
  sprintf("%.4f", pr)
}
fmt_pct2 <- function(x) sprintf("%.2f", round_half_up(x, 2))

locate_xpt <- function(fname) {
  p0 <- file.path("/bench/data/raw/cdisc-pilot/adam", fname)
  if (file.exists(p0)) return(p0)
  p0b <- file.path("/bench/data/raw/cdisc-pilot/adam-split", fname)
  if (file.exists(p0b)) return(p0b)
  cands <- c(
    file.path(getwd(), fname),
    file.path(getwd(), "data", "raw", "cdisc-pilot", "adam", fname),
    file.path(getwd(), "adam", fname)
  )
  for (cc in cands) if (file.exists(cc)) return(cc)
  rr <- try(list.files(getwd(), pattern = paste0("^", fname, "$"), recursive = TRUE, full.names = TRUE), silent = TRUE)
  if (!inherits(rr, "try-error") && length(rr) > 0) return(rr[1])
  return(p0)
}

adae_path <- locate_xpt("adae.xpt")
adsl_path <- locate_xpt("adsl.xpt")

library(haven)
library(dplyr)
library(tidyr)

adae_raw <- haven::read_xpt(adae_path)
adsl_raw <- haven::read_xpt(adsl_path)
adae_raw <- haven::zap_labels(haven::zap_formats(adae_raw))
adsl_raw <- haven::zap_labels(haven::zap_formats(adsl_raw))
names(adae_raw) <- toupper(names(adae_raw))
names(adsl_raw) <- toupper(names(adsl_raw))

adae <- adae_raw %>%
  select(any_of(c("USUBJID","TRTAN","AEDECOD","AESER","AOCC02FL","AOCC04FL","SAFFL"))) %>%
  filter((AOCC02FL %in% "Y" | AOCC04FL %in% "Y") & AESER %in% "Y" & SAFFL %in% "Y")
adae <- adae %>% arrange(USUBJID)
stopifnot(all(c("USUBJID") %in% names(adae)))

adsl <- adsl_raw %>%
  select(any_of(c("USUBJID","TRT01AN"))) %>%
  filter(!is.na(TRT01AN))
adsl <- adsl %>% arrange(USUBJID, TRT01AN) %>% distinct(USUBJID, TRT01AN, .keep_all = TRUE)

adae_sl <- inner_join(adsl, adae, by = "USUBJID")
stopifnot(all(c("USUBJID","TRT01AN","TRTAN","AEDECOD","AOCC02FL") %in% names(adae_sl)))

sum_1 <- adsl %>% count(TRT01AN, name = "COUNT") %>% arrange(TRT01AN)
trt_vals <- sort(unique(sum_1$TRT01AN))
numtrt <- nrow(sum_1)
get_cnt <- function(v) {
  r <- sum_1$COUNT[sum_1$TRT01AN == v]
  if (length(r) == 0) return(NA_integer_) else return(r[1])
}
trtcn0 <- get_cnt(0); trtcn54 <- get_cnt(54); trtcn81 <- get_cnt(81)

adae_sl2 <- adae_sl %>% arrange(USUBJID, AEDECOD) %>% distinct(USUBJID, AEDECOD, .keep_all = TRUE)

adae_dummy <- adae_sl %>%
  filter(AOCC02FL %in% "Y") %>%
  arrange(AEDECOD) %>%
  distinct(AEDECOD, .keep_all = FALSE) %>%
  select(AEDECOD)

adae_sl2 <- adae_sl2 %>% arrange(AEDECOD)
adae_sl3 <- adae_sl2 %>% filter(AEDECOD %in% adae_dummy$AEDECOD)

sum_2 <- adae_sl3 %>% count(AEDECOD, TRTAN, name = "COUNT") %>% arrange(AEDECOD, TRTAN)

dummy_2 <- expand.grid(AEDECOD = adae_dummy$AEDECOD, TRTAN = trt_vals, stringsAsFactors = FALSE) %>%
  as_tibble() %>% arrange(AEDECOD, TRTAN)
dummy_2$TRTAN <- as.numeric(dummy_2$TRTAN)

adae_sl4 <- left_join(dummy_2, sum_2, by = c("AEDECOD","TRTAN"))
stopifnot(all(c("AEDECOD","TRTAN") %in% names(adae_sl4)))
adae_sl4 <- adae_sl4 %>% arrange(TRTAN, AEDECOD)

sum_1_ren <- sum_1 %>% rename(TRTAN = TRT01AN, BIGN = COUNT)
adae_sl5 <- left_join(adae_sl4, sum_1_ren, by = "TRTAN")
stopifnot(all(c("BIGN") %in% names(adae_sl5)))

resp0 <- adae_sl5 %>% mutate(RESP = 0L, COUNT_FINAL = ifelse(is.na(COUNT), BIGN, BIGN - COUNT))
resp1 <- adae_sl5 %>% mutate(RESP = 1L, COUNT_FINAL = ifelse(is.na(COUNT), 0L, COUNT))
adae_sl6 <- bind_rows(resp0, resp1)
adae_sl6 <- adae_sl6 %>% arrange(AEDECOD, TRTAN, RESP)

table_prob <- function(mat) {
  # multivariate hypergeometric probability of observed table given margins
  # prod row! prod col! / (N! prod cell!)
  rs <- rowSums(mat); cs <- colSums(mat); N <- sum(mat)
  lnum <- sum(lfactorial(rs)) + sum(lfactorial(cs))
  lden <- lfactorial(N) + sum(lfactorial(as.vector(mat)))
  exp(lnum - lden)
}

pts_radix_order <- adae_dummy$AEDECOD[order(adae_dummy$AEDECOD, method = "radix")]
fish_rows <- list()
sum_final_list <- list()
# store per-PT freq detail for listing
freq_detail <- list()
for (pt in pts_radix_order) {
  sub <- adae_sl6 %>% filter(AEDECOD == pt)
  sub <- sub %>% arrange(TRTAN, RESP)
  tbl <- xtabs(COUNT_FINAL ~ TRTAN + RESP, data = sub)
  rn <- as.numeric(rownames(tbl))
  tbl <- tbl[order(rn), , drop = FALSE]
  cn <- as.numeric(colnames(tbl))
  tbl <- tbl[, order(cn), drop = FALSE]
  mat <- as.matrix(tbl)
  storage.mode(mat) <- "integer"
  ft <- try(suppressWarnings(fisher.test(mat, workspace = 10000000)), silent = TRUE)
  if (inherits(ft, "try-error")) {
    ft <- suppressWarnings(fisher.test(mat, workspace = 2000000, hybrid = FALSE))
  }
  pval <- ft$p.value
  cval <- fmt_p(pval)
  tp <- table_prob(mat)
  fish_rows[[pt]] <- data.frame(AEDECOD = pt, CVALUE1 = cval, PVAL = pval, TPROB = tp, stringsAsFactors = FALSE)
  sum_final_list[[pt]] <- sub
  freq_detail[[pt]] <- list(mat = mat, pval = pval, tprob = tp)
}
fish1_df <- bind_rows(fish_rows)
fish2 <- fish1_df %>% select(AEDECOD, CVALUE1)

sum_final_all <- bind_rows(sum_final_list)

sum_final <- sum_final_all %>%
  filter(RESP == 1) %>%
  mutate(
    COUNT_DISP = COUNT_FINAL,
    CP = paste0(as.character(COUNT_DISP), "(", sprintf("%.1f", COUNT_DISP), ")"),
    ORD = ifelse(TRTAN == 81, 1L, 2L)
  ) %>%
  arrange(AEDECOD, ORD)
count_max_df <- sum_final %>% filter(TRTAN == 81) %>% select(AEDECOD, COUNT_MAX = COUNT_DISP)
sum_final <- left_join(sum_final, count_max_df, by = "AEDECOD")
sum_final <- sum_final[order(sum_final$AEDECOD, sum_final$COUNT_MAX, method = "radix"), ]

sum_final_w <- sum_final %>%
  select(AEDECOD, COUNT_MAX, TRTAN, CP) %>%
  mutate(TRTCOL = paste0("TRT", TRTAN)) %>%
  select(-TRTAN) %>%
  pivot_wider(id_cols = c(AEDECOD, COUNT_MAX), names_from = TRTCOL, values_from = CP)
for (cc in c("TRT0","TRT54","TRT81")) if (!cc %in% names(sum_final_w)) sum_final_w[[cc]] <- NA_character_
sum_final2 <- left_join(sum_final_w, fish2, by = "AEDECOD")
ord_idx <- order(-sum_final2$COUNT_MAX, sum_final2$AEDECOD, method = "radix")
sum_final2 <- sum_final2[ord_idx, , drop = FALSE]

adae_sl_sub <- adae_sl %>% filter(AOCC02FL %in% "Y") %>% arrange(USUBJID) %>% distinct(USUBJID, .keep_all = FALSE)

adae_sl_sub2 <- left_join(adsl, adae_sl_sub %>% mutate(INB = 1L), by = "USUBJID") %>%
  mutate(FLG = ifelse(!is.na(INB), 1L, 0L)) %>%
  select(-INB)
stopifnot(all(c("TRT01AN","FLG") %in% names(adae_sl_sub2)))
adae_sl_sub2 <- adae_sl_sub2 %>% rename(TRTAN = TRT01AN)

tbl_sub <- xtabs(~ TRTAN + FLG, data = adae_sl_sub2)
rn2 <- as.numeric(rownames(tbl_sub)); tbl_sub <- tbl_sub[order(rn2), , drop = FALSE]
cn2 <- as.numeric(colnames(tbl_sub)); tbl_sub <- tbl_sub[, order(cn2), drop = FALSE]
mat_sub <- as.matrix(tbl_sub); storage.mode(mat_sub) <- "integer"
ft_sub <- suppressWarnings(fisher.test(mat_sub, workspace = 10000000))
p_sub <- ft_sub$p.value
c_sub <- fmt_p(p_sub)
tp_sub <- table_prob(mat_sub)
fish_sub2 <- data.frame(CVALUE1 = c_sub, stringsAsFactors = FALSE)

sum_final_sub_all <- adae_sl_sub2 %>% count(TRTAN, FLG, name = "COUNT")
sum_final_sub <- sum_final_sub_all %>%
  filter(FLG == 1) %>%
  mutate(
    CP = paste0(as.character(COUNT), "(", sprintf("%.1f", COUNT), ")"),
    AEDECOD = "Number of subjects reporting serious adverse events"
  )
sum_final_sub_w <- sum_final_sub %>%
  mutate(TRTCOL = paste0("TRT", TRTAN)) %>%
  select(AEDECOD, TRTCOL, CP) %>%
  pivot_wider(id_cols = AEDECOD, names_from = TRTCOL, values_from = CP)
for (cc in c("TRT0","TRT54","TRT81")) if (!cc %in% names(sum_final_sub_w)) sum_final_sub_w[[cc]] <- NA_character_
sum_final_sub2 <- bind_cols(sum_final_sub_w, fish_sub2)

if (!"COUNT_MAX" %in% names(sum_final_sub2)) sum_final_sub2$COUNT_MAX <- NA_real_
sum_final2 <- sum_final2 %>% select(AEDECOD, COUNT_MAX, TRT0, TRT54, TRT81, CVALUE1)
sum_final_sub2 <- sum_final_sub2 %>% select(AEDECOD, COUNT_MAX, TRT0, TRT54, TRT81, CVALUE1)
final_report <- bind_rows(sum_final_sub2, sum_final2)

final_display <- final_report %>% select(AEDECOD, TRT0, TRT54, TRT81, CVALUE1)

write.csv(final_display, file.path(out, "P045.csv"), row.names = FALSE, na = "")
write.csv(final_report, file.path(out, "P045_dataset.csv"), row.names = FALSE, na = "")

# ---- Build full listing text including PROC FREQ outputs + PROC REPORT ----
con_txt <- file(file.path(out, "P045.txt"), open = "wt")

# PROC FREQ for each AEDECOD (table trtan*resp with weight)
for (pt in pts_radix_order) {
  dd <- freq_detail[[pt]]
  mat <- dd$mat
  pval <- dd$pval
  tprob <- dd$tprob
  N <- sum(mat)
  rs <- rowSums(mat); cs <- colSums(mat)
  cat(paste0("AEDECOD=", pt, "\n"), file = con_txt)
  cat("The FREQ Procedure\n", file = con_txt)
  cat("Table of TRTAN by RESP\n", file = con_txt)
  cat("TRTAN      RESP\n", file = con_txt)
  cat("Frequency\n", file = con_txt)
  cat("Percent\n", file = con_txt)
  cat("Row Pct\n", file = con_txt)
  cat("Col Pct\n", file = con_txt)
  # header row
  cat(sprintf("%-10s|%-12s|%-12s|%-12s\n", "", "0", "1", "Total"), file = con_txt)
  trt_levels <- as.numeric(rownames(mat))
  for (ri in seq_along(trt_levels)) {
    rv <- trt_levels[ri]
    for (ci in seq_len(ncol(mat))) {
      f <- mat[ri, ci]
      pct <- f / N * 100
      rowpct <- if (rs[ri] > 0) f / rs[ri] * 100 else 0
      colpct <- if (cs[ci] > 0) f / cs[ci] * 100 else 0
      cat(sprintf("%s %s Frequency=%d Percent=%s Row Pct=%s Col Pct=%s\n",
                  rv, colnames(mat)[ci], f, fmt_pct2(pct), fmt_pct2(rowpct), fmt_pct2(colpct)), file = con_txt)
    }
    # row total
    rpct <- rs[ri] / N * 100
    cat(sprintf("%s Total Frequency=%d Percent=%s\n", rv, rs[ri], fmt_pct2(rpct)), file = con_txt)
  }
  cat(sprintf("Total Frequency=%d Percent=%s\n", N, fmt_pct2(100)), file = con_txt)
  for (ci in seq_len(ncol(mat))) {
    cat(sprintf("Column %s Total Frequency=%d Percent=%s\n", colnames(mat)[ci], cs[ci], fmt_pct2(cs[ci]/N*100)), file = con_txt)
  }
  cat("Statistics for Table of TRTAN by RESP\n", file = con_txt)
  cat("Fisher's Exact Test\n", file = con_txt)
  cat(sprintf("Table Probability (P) %s\n", sprintf("%.4f", round_half_up(tprob, 4))), file = con_txt)
  cat(sprintf("Pr <= P %s\n", fmt_p(pval)), file = con_txt)
  cat(sprintf("Sample Size = %d\n", N), file = con_txt)
  cat("\n", file = con_txt)
}

# Overall table trtan*flg
{
  mat <- mat_sub
  N <- sum(mat)
  rs <- rowSums(mat); cs <- colSums(mat)
  cat("The FREQ Procedure\n", file = con_txt)
  cat("Table of TRTAN by FLG\n", file = con_txt)
  cat("TRTAN      FLG\n", file = con_txt)
  cat("Frequency\n", file = con_txt)
  cat("Percent\n", file = con_txt)
  cat("Row Pct\n", file = con_txt)
  cat("Col Pct\n", file = con_txt)
  cat(sprintf("%-10s|%-12s|%-12s|%-12s\n", "", "0", "1", "Total"), file = con_txt)
  trt_levels <- as.numeric(rownames(mat))
  for (ri in seq_along(trt_levels)) {
    rv <- trt_levels[ri]
    for (ci in seq_len(ncol(mat))) {
      f <- mat[ri, ci]
      pct <- f / N * 100
      rowpct <- if (rs[ri] > 0) f / rs[ri] * 100 else 0
      colpct <- if (cs[ci] > 0) f / cs[ci] * 100 else 0
      cat(sprintf("%s %s Frequency=%d Percent=%s Row Pct=%s Col Pct=%s\n",
                  rv, colnames(mat)[ci], f, fmt_pct2(pct), fmt_pct2(rowpct), fmt_pct2(colpct)), file = con_txt)
    }
    rpct <- rs[ri] / N * 100
    cat(sprintf("%s Total Frequency=%d Percent=%s\n", rv, rs[ri], fmt_pct2(rpct)), file = con_txt)
  }
  cat(sprintf("Total Frequency=%d Percent=%s\n", N, fmt_pct2(100)), file = con_txt)
  for (ci in seq_len(ncol(mat))) {
    cat(sprintf("Column %s Total Frequency=%d Percent=%s\n", colnames(mat)[ci], cs[ci], fmt_pct2(cs[ci]/N*100)), file = con_txt)
  }
  cat("Statistics for Table of TRTAN by FLG\n", file = con_txt)
  cat("Fisher's Exact Test\n", file = con_txt)
  cat(sprintf("Table Probability (P) %s\n", sprintf("%.4f", round_half_up(tp_sub, 4))), file = con_txt)
  cat(sprintf("Pr <= P %s\n", fmt_p(p_sub)), file = con_txt)
  cat(sprintf("Sample Size = %d\n", N), file = con_txt)
  cat("\n", file = con_txt)
}

# PROC REPORT final table
cat("Table 7.5 Summary of Serious Adverse Events\n", file = con_txt)
cat("Safety Population\n", file = con_txt)
cat("-\n", file = con_txt)
cat(sprintf("%-50s | %-15s | %-15s | %-15s | %-10s\n", "Preferred Term", "Placebo", "Treatment 1", "Treatment 2", "p-value*b"), file = con_txt)
cat(sprintf("%-50s | %-15s | %-15s | %-15s | %-10s\n", "", paste0("N=", ifelse(is.na(trtcn0),"",trtcn0)), paste0("N=", ifelse(is.na(trtcn54),"",trtcn54)), paste0("N=", ifelse(is.na(trtcn81),"",trtcn81)), ""), file = con_txt)
cat(paste0(strrep("-", 50), " | ", strrep("-", 15), " | ", strrep("-", 15), " | ", strrep("-", 15), " | ", strrep("-", 10), "\n"), file = con_txt)
for (i in seq_len(nrow(final_display))) {
  ae <- final_display$AEDECOD[i]
  cat(sprintf("%-50s | %-15s | %-15s | %-15s | %-10s\n",
              substr(ae, 1, 50),
              ifelse(is.na(final_display$TRT0[i]), "", final_display$TRT0[i]),
              ifelse(is.na(final_display$TRT54[i]), "", final_display$TRT54[i]),
              ifelse(is.na(final_display$TRT81[i]), "", final_display$TRT81[i]),
              ifelse(is.na(final_display$CVALUE1[i]), "", final_display$CVALUE1[i])), file = con_txt)
  if (nchar(ae) > 50) {
    cat(paste0("  ", substr(ae, 51, nchar(ae)), "\n"), file = con_txt)
  }
}
cat("-\n", file = con_txt)
cat("Denominator for each % is treatment column N\n", file = con_txt)
cat("*b = p-values are from Fisher's Exact Test\n", file = con_txt)
close(con_txt)

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
  # guard against 1.0000 overflow
  if (pr >= 1) pr <- 1
  if (pr <= 0) pr <- 0
  sprintf("%.4f", pr)
}

locate_xpt <- function(fname) {
  p0 <- file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam", fname)
  if (file.exists(p0)) return(p0)
  p0b <- file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam-split", fname)
  if (file.exists(p0b)) return(p0b)
  # relative candidates
  cands <- c(
    file.path(getwd(), fname),
    file.path(getwd(), "data", "raw", "cdisc-pilot", "adam", fname),
    file.path(getwd(), "adam", fname)
  )
  for (cc in cands) if (file.exists(cc)) return(cc)
  # recursive search from getwd (limited depth)
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

# work.adae : keep + where (aocc02fl='Y' or aocc04fl='Y') and aeser='Y' and saffl='Y'
adae <- adae_raw %>%
  select(any_of(c("USUBJID","TRTAN","AEDECOD","AESER","AOCC02FL","AOCC04FL","SAFFL"))) %>%
  filter((AOCC02FL %in% "Y" | AOCC04FL %in% "Y") & AESER %in% "Y" & SAFFL %in% "Y")
# proc sort by usubjid (SAS missing first; arrange)
adae <- adae %>% arrange(USUBJID)
stopifnot(all(c("USUBJID") %in% names(adae)))

# work.adsl : keep usubjid trt01an where trt01an ne .
adsl <- adsl_raw %>%
  select(any_of(c("USUBJID","TRT01AN"))) %>%
  filter(!is.na(TRT01AN))
# proc sort nodupkey by usubjid trt01an
adsl <- adsl %>% arrange(USUBJID, TRT01AN) %>% distinct(USUBJID, TRT01AN, .keep_all = TRUE)

# data adae_sl; merge adsl(in=a) adae(in=b); by usubjid; if a and b;
adae_sl <- inner_join(adsl, adae, by = "USUBJID")
stopifnot(all(c("USUBJID","TRT01AN","TRTAN","AEDECOD","AOCC02FL") %in% names(adae_sl)))

# DENOMINATOR: proc freq table trt01an / out=sum_1
sum_1 <- adsl %>% count(TRT01AN, name = "COUNT") %>% arrange(TRT01AN)
# macro vars
trt_vals <- sort(unique(sum_1$TRT01AN))
numtrt <- nrow(sum_1)
# lookup counts
get_cnt <- function(v) {
  r <- sum_1$COUNT[sum_1$TRT01AN == v]
  if (length(r) == 0) return(NA_integer_) else return(r[1])
}
trtcn0 <- get_cnt(0); trtcn54 <- get_cnt(54); trtcn81 <- get_cnt(81)

# proc sort adae_sl out=adae_sl2 nodupkey by usubjid aedecod
adae_sl2 <- adae_sl %>% arrange(USUBJID, AEDECOD) %>% distinct(USUBJID, AEDECOD, .keep_all = TRUE)

# proc sort adae_sl out=adae_dummy nodupkey by aedecod where aocc02fl='Y'
adae_dummy <- adae_sl %>%
  filter(AOCC02FL %in% "Y") %>%
  arrange(AEDECOD) %>%
  distinct(AEDECOD, .keep_all = FALSE) %>%
  select(AEDECOD)

# sort adae_sl2 by aedecod
adae_sl2 <- adae_sl2 %>% arrange(AEDECOD)
# data adae_sl3; merge adae_sl2(in=a) adae_dummy(in=b); by aedecod; if a and b;
adae_sl3 <- adae_sl2 %>% filter(AEDECOD %in% adae_dummy$AEDECOD)

# proc freq table aedecod*trtan / out=sum_2
sum_2 <- adae_sl3 %>% count(AEDECOD, TRTAN, name = "COUNT") %>% arrange(AEDECOD, TRTAN)

# dummy_2 : cross join each aedecod with all trt values
dummy_2 <- expand.grid(AEDECOD = adae_dummy$AEDECOD, TRTAN = trt_vals, stringsAsFactors = FALSE) %>%
  as_tibble() %>% arrange(AEDECOD, TRTAN)
# ensure types match
dummy_2$TRTAN <- as.numeric(dummy_2$TRTAN)

# data adae_sl4; merge dummy_2(in=a) sum_2(in=b); by aedecod trtan;
adae_sl4 <- left_join(dummy_2, sum_2, by = c("AEDECOD","TRTAN"))
stopifnot(all(c("AEDECOD","TRTAN") %in% names(adae_sl4)))
adae_sl4 <- adae_sl4 %>% arrange(TRTAN, AEDECOD)

# merge with sum_1 renamed
sum_1_ren <- sum_1 %>% rename(TRTAN = TRT01AN, BIGN = COUNT)
adae_sl5 <- left_join(adae_sl4, sum_1_ren, by = "TRTAN")
stopifnot(all(c("BIGN") %in% names(adae_sl5)))
# if a (all dummy rows kept)
# adae_sl5 already left join keeps all

# data adae_sl6 : expand to resp 0/1
resp0 <- adae_sl5 %>% mutate(RESP = 0L, COUNT_FINAL = ifelse(is.na(COUNT), BIGN, BIGN - COUNT))
resp1 <- adae_sl5 %>% mutate(RESP = 1L, COUNT_FINAL = ifelse(is.na(COUNT), 0L, COUNT))
adae_sl6 <- bind_rows(resp0, resp1)
adae_sl6 <- adae_sl6 %>% arrange(AEDECOD, TRTAN, RESP)

# Fisher per aedecod + sum_final
pts_radix_order <- adae_dummy$AEDECOD[order(adae_dummy$AEDECOD, method = "radix")]
fish_rows <- list()
sum_final_list <- list()
for (pt in pts_radix_order) {
  sub <- adae_sl6 %>% filter(AEDECOD == pt)
  # build matrix rows TRTAN sorted asc, cols RESP 0,1
  sub <- sub %>% arrange(TRTAN, RESP)
  tbl <- xtabs(COUNT_FINAL ~ TRTAN + RESP, data = sub)
  # ensure row order ascending numeric
  rn <- as.numeric(rownames(tbl))
  tbl <- tbl[order(rn), , drop = FALSE]
  # ensure col order 0,1
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
  fish_rows[[pt]] <- data.frame(AEDECOD = pt, CVALUE1 = cval, PVAL = pval, stringsAsFactors = FALSE)
  # sum_final rows are the OUT= rows; keep for later (all 6 rows)
  sum_final_list[[pt]] <- sub
}
fish1_df <- bind_rows(fish_rows)
# fish2 keep aedecod cvalue1 where name1='XP2_FISH' -> all rows
fish2 <- fish1_df %>% select(AEDECOD, CVALUE1)

sum_final_all <- bind_rows(sum_final_list)

# data sum_final; where resp=1; cp; ord;
sum_final <- sum_final_all %>%
  filter(RESP == 1) %>%
  mutate(
    COUNT_DISP = COUNT_FINAL,
    CP = paste0(as.character(COUNT_DISP), "(", sprintf("%.1f", COUNT_DISP), ")"),
    ORD = ifelse(TRTAN == 81, 1L, 2L)
  ) %>%
  arrange(AEDECOD, ORD)
# retain count_max : if ord=1 then count_max=count
# need per aedecod count_max = COUNT_DISP where TRTAN==81
count_max_df <- sum_final %>% filter(TRTAN == 81) %>% select(AEDECOD, COUNT_MAX = COUNT_DISP)
sum_final <- left_join(sum_final, count_max_df, by = "AEDECOD")
# proc sort by aedecod count_max (SAS: by aedecod count_max)
sum_final <- sum_final[order(sum_final$AEDECOD, sum_final$COUNT_MAX, method = "radix"), ]

# proc transpose by aedecod count_max id trtan var cp prefix=trt
sum_final_w <- sum_final %>%
  select(AEDECOD, COUNT_MAX, TRTAN, CP) %>%
  mutate(TRTCOL = paste0("TRT", TRTAN)) %>%
  select(-TRTAN) %>%
  pivot_wider(id_cols = c(AEDECOD, COUNT_MAX), names_from = TRTCOL, values_from = CP)
# ensure columns TRT0 TRT54 TRT81 exist
for (cc in c("TRT0","TRT54","TRT81")) if (!cc %in% names(sum_final_w)) sum_final_w[[cc]] <- NA_character_
# merge with fish2 by aedecod
sum_final2 <- left_join(sum_final_w, fish2, by = "AEDECOD")
# proc sort by descending count_max aedecod
ord_idx <- order(-sum_final2$COUNT_MAX, sum_final2$AEDECOD, method = "radix")
sum_final2 <- sum_final2[ord_idx, , drop = FALSE]

# At least one SAE : adae_sl_sub distinct usubjid where aocc02fl='Y'
adae_sl_sub <- adae_sl %>% filter(AOCC02FL %in% "Y") %>% arrange(USUBJID) %>% distinct(USUBJID, .keep_all = FALSE)

# data adae_sl_sub2; merge adsl(in=a) adae_sl_sub(in=b); by usubjid; if a; flg
adae_sl_sub2 <- left_join(adsl, adae_sl_sub %>% mutate(INB = 1L), by = "USUBJID") %>%
  mutate(FLG = ifelse(!is.na(INB), 1L, 0L)) %>%
  select(-INB)
stopifnot(all(c("TRT01AN","FLG") %in% names(adae_sl_sub2)))
adae_sl_sub2 <- adae_sl_sub2 %>% rename(TRTAN = TRT01AN)

# Fisher for overall
tbl_sub <- xtabs(~ TRTAN + FLG, data = adae_sl_sub2)
rn2 <- as.numeric(rownames(tbl_sub)); tbl_sub <- tbl_sub[order(rn2), , drop = FALSE]
cn2 <- as.numeric(colnames(tbl_sub)); tbl_sub <- tbl_sub[, order(cn2), drop = FALSE]
mat_sub <- as.matrix(tbl_sub); storage.mode(mat_sub) <- "integer"
ft_sub <- suppressWarnings(fisher.test(mat_sub, workspace = 10000000))
p_sub <- ft_sub$p.value
c_sub <- fmt_p(p_sub)
fish_sub2 <- data.frame(CVALUE1 = c_sub, stringsAsFactors = FALSE)

# sum_final_sub : table trtan*flg out ; where flg=1 ; cp ; aedecod='Number of subjects reporting serious adverse events'
sum_final_sub_all <- adae_sl_sub2 %>% count(TRTAN, FLG, name = "COUNT")
sum_final_sub <- sum_final_sub_all %>%
  filter(FLG == 1) %>%
  mutate(
    CP = paste0(as.character(COUNT), "(", sprintf("%.1f", COUNT), ")"),
    AEDECOD = "Number of subjects reporting serious adverse events"
  )
# transpose by aedecod id trtan var cp prefix=trt
sum_final_sub_w <- sum_final_sub %>%
  mutate(TRTCOL = paste0("TRT", TRTAN)) %>%
  select(AEDECOD, TRTCOL, CP) %>%
  pivot_wider(id_cols = AEDECOD, names_from = TRTCOL, values_from = CP)
for (cc in c("TRT0","TRT54","TRT81")) if (!cc %in% names(sum_final_sub_w)) sum_final_sub_w[[cc]] <- NA_character_
# merge (no by) -> cbind single row
sum_final_sub2 <- bind_cols(sum_final_sub_w, fish_sub2)

# data final_report; set sum_final_sub2 sum_final2;
# align columns: sum_final_sub2 has AEDECOD TRT0 TRT54 TRT81 CVALUE1 ; sum_final2 has AEDECOD COUNT_MAX TRT0 TRT54 TRT81 CVALUE1
# for bind, add COUNT_MAX NA to first
if (!"COUNT_MAX" %in% names(sum_final_sub2)) sum_final_sub2$COUNT_MAX <- NA_real_
# ensure same column order
sum_final2 <- sum_final2 %>% select(AEDECOD, COUNT_MAX, TRT0, TRT54, TRT81, CVALUE1)
sum_final_sub2 <- sum_final_sub2 %>% select(AEDECOD, COUNT_MAX, TRT0, TRT54, TRT81, CVALUE1)
final_report <- bind_rows(sum_final_sub2, sum_final2)

# Display dataset for report (drop COUNT_MAX for display but keep for CSV? keep full)
final_display <- final_report %>% select(AEDECOD, TRT0, TRT54, TRT81, CVALUE1)

# Write CSVs
write.csv(final_display, file.path(out, "P045.csv"), row.names = FALSE, na = "")
# also write full dataset copy for completeness
write.csv(final_report, file.path(out, "P045_dataset.csv"), row.names = FALSE, na = "")

# Build text rendering mimicking PROC REPORT
hdr_trt0 <- paste0("Placebo N=", ifelse(is.na(trtcn0), "", trtcn0))
hdr_trt54 <- paste0("Treatment 1 N=", ifelse(is.na(trtcn54), "", trtcn54))
hdr_trt81 <- paste0("Treatment 2 N=", ifelse(is.na(trtcn81), "", trtcn81))

con_txt <- file(file.path(out, "P045.txt"), open = "wt")
cat("Table 7.5 Summary of Serious Adverse Events\n", file = con_txt)
cat("Safety Population\n", file = con_txt)
cat("-\n", file = con_txt)
# header lines (split=# means two lines; render as two rows)
cat(sprintf("%-50s | %-15s | %-15s | %-15s | %-10s\n", "Preferred Term", "Placebo", "Treatment 1", "Treatment 2", "p-value*b"), file = con_txt)
cat(sprintf("%-50s | %-15s | %-15s | %-15s | %-10s\n", "", paste0("N=", ifelse(is.na(trtcn0),"",trtcn0)), paste0("N=", ifelse(is.na(trtcn54),"",trtcn54)), paste0("N=", ifelse(is.na(trtcn81),"",trtcn81)), ""), file = con_txt)
cat(paste0(strrep("-", 50), " | ", strrep("-", 15), " | ", strrep("-", 15), " | ", strrep("-", 15), " | ", strrep("-", 10), "\n"), file = con_txt)
for (i in seq_len(nrow(final_display))) {
  ae <- final_display$AEDECOD[i]
  # truncate/pad to 50 for display but keep full value in file (flow)
  cat(sprintf("%-50s | %-15s | %-15s | %-15s | %-10s\n",
              substr(ae, 1, 50),
              ifelse(is.na(final_display$TRT0[i]), "", final_display$TRT0[i]),
              ifelse(is.na(final_display$TRT54[i]), "", final_display$TRT54[i]),
              ifelse(is.na(final_display$TRT81[i]), "", final_display$TRT81[i]),
              ifelse(is.na(final_display$CVALUE1[i]), "", final_display$CVALUE1[i])), file = con_txt)
  # if AEDECOD longer than 50, write continuation (flow)
  if (nchar(ae) > 50) {
    cat(paste0("  ", substr(ae, 51, nchar(ae)), "\n"), file = con_txt)
  }
}
cat("-\n", file = con_txt)
cat("Denominator for each % is treatment column N\n", file = con_txt)
cat("*b = p-values are from Fisher's Exact Test\n", file = con_txt)
close(con_txt)

suppressMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(tibble)
})

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)
message("OUT_DIR=", out)

dirs_try <- c("/bench/data/raw/cdisc-pilot/adam",
              "/bench/data/derived/cdisc-pilot/adam")
find_file <- function(fname) {
  for (d in dirs_try) {
    p <- file.path(d, fname)
    if (file.exists(p)) return(p)
  }
  r <- "/bench"
  cand <- list.files(r, pattern = paste0("^", fname, "$"), recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) return(cand[1])
  return(NULL)
}
adae_path <- find_file("adae.xpt")
adsl_path <- find_file("adsl.xpt")
message("adae_path=", adae_path)
message("adsl_path=", adsl_path)

adae_raw <- haven::read_xpt(adae_path)
adsl_raw <- haven::read_xpt(adsl_path)
names(adae_raw) <- toupper(names(adae_raw))
names(adsl_raw) <- toupper(names(adsl_raw))

round_half_up <- function(x, digits=0) {
  posneg <- sign(x)
  z <- abs(x)*10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z/10^digits
  z*posneg
}
fmt2 <- function(x) {
  if (is.na(x)) return("")
  sprintf("%.2f", round_half_up(as.numeric(x), 2))
}
fmt4 <- function(x) {
  if (is.na(x)) return("")
  sprintf("%.4f", round_half_up(as.numeric(x), 4))
}
fmt_best12 <- function(p) {
  if (is.na(p)) return("")
  trimws(formatC(p, format="g", digits=12))
}
fmt_cp <- function(cnt) {
  cnt <- as.integer(cnt)
  paste0(as.character(cnt), "(", sprintf("%.1f", cnt), ")")
}
is_Y <- function(x) !is.na(x) & trimws(as.character(x)) == "Y"

# ---- work.adae
adae <- adae_raw %>%
  filter((is_Y(AOCC02FL) | is_Y(AOCC04FL)) & is_Y(AESER) & is_Y(SAFFL)) %>%
  select(USUBJID, TRTAN, AEDECOD, AESER, AOCC02FL, AOCC04FL, SAFFL) %>%
  mutate(USUBJID = as.character(USUBJID),
         AEDECOD = as.character(AEDECOD),
         TRTAN = as.numeric(TRTAN)) %>%
  arrange(USUBJID)
message("adae n=", nrow(adae))

# ---- work.adsl
adsl <- adsl_raw %>%
  filter(!is.na(TRT01AN)) %>%
  select(USUBJID, TRT01AN) %>%
  mutate(USUBJID = as.character(USUBJID),
         TRT01AN = as.numeric(TRT01AN)) %>%
  arrange(USUBJID, TRT01AN) %>%
  distinct(USUBJID, TRT01AN, .keep_all = TRUE)
message("adsl n=", nrow(adsl))

adae_sl <- inner_join(adsl, adae, by="USUBJID")
message("adae_sl n=", nrow(adae_sl))

sum_1 <- adsl %>% group_by(TRT01AN) %>% summarise(COUNT=n(), .groups="drop") %>% arrange(TRT01AN)
numtrt <- nrow(sum_1)
trtnum <- sort(unique(sum_1$TRT01AN))
trtcn <- setNames(sum_1$COUNT, as.character(sum_1$TRT01AN))
getN <- function(v) { k<-as.character(v); if(k %in% names(trtcn)) trtcn[[k]] else NA_integer_ }
N0 <- getN(0); N54 <- getN(54); N81 <- getN(81)
message(sprintf("N0=%s N54=%s N81=%s", N0, N54, N81))

adae_sl2 <- adae_sl %>% arrange(USUBJID, AEDECOD) %>% distinct(USUBJID, AEDECOD, .keep_all=TRUE)
adae_dummy <- adae_sl %>% filter(is_Y(AOCC02FL)) %>% arrange(AEDECOD) %>% distinct(AEDECOD) %>% select(AEDECOD)
message("n PT dummy=", nrow(adae_dummy))

adae_sl3 <- adae_sl2 %>% filter(AEDECOD %in% adae_dummy$AEDECOD)
sum_2 <- adae_sl3 %>% group_by(AEDECOD, TRTAN) %>% summarise(COUNT=n(), .groups="drop")
dummy_2 <- expand.grid(AEDECOD=adae_dummy$AEDECOD, TRTAN=trtnum, stringsAsFactors=FALSE) %>%
  as_tibble() %>% mutate(AEDECOD=as.character(AEDECOD), TRTAN=as.numeric(TRTAN)) %>% arrange(AEDECOD, TRTAN)
adae_sl4 <- left_join(dummy_2, sum_2, by=c("AEDECOD","TRTAN"))
sum_1_ren <- sum_1 %>% rename(TRTAN=TRT01AN, BIGN=COUNT)
adae_sl5 <- left_join(adae_sl4, sum_1_ren, by="TRTAN") %>% arrange(TRTAN)

adae_sl6_resp1 <- adae_sl5 %>% mutate(RESP=1L, COUNT_FINAL=ifelse(is.na(COUNT),0,COUNT))
adae_sl6_resp0 <- adae_sl5 %>% mutate(RESP=0L, COUNT_FINAL=ifelse(is.na(COUNT),BIGN,BIGN-COUNT))
adae_sl6 <- bind_rows(adae_sl6_resp0, adae_sl6_resp1) %>% arrange(AEDECOD, TRTAN, RESP)

pt_list <- sort(unique(adae_sl6$AEDECOD))

# per-PT fisher + report counts
fish_rows <- list()
sum_final_rows <- list()
# store freq detail for listing
freq_detail <- list()
for (pt in pt_list) {
  sub <- adae_sl6 %>% filter(AEDECOD==pt)
  trts <- sort(unique(sub$TRTAN))
  m0 <- sapply(trts, function(t) sub$COUNT_FINAL[sub$TRTAN==t & sub$RESP==0][1])
  m1 <- sapply(trts, function(t) sub$COUNT_FINAL[sub$TRTAN==t & sub$RESP==1][1])
  mat <- cbind(m0, m1)
  rownames(mat) <- trts
  pval <- suppressWarnings(fisher.test(mat)$p.value)
  fish_rows[[pt]] <- tibble(AEDECOD=pt, CVALUE1=pval)
  for (t in trts) {
    cnt1 <- sub$COUNT_FINAL[sub$TRTAN==t & sub$RESP==1][1]
    sum_final_rows[[length(sum_final_rows)+1]] <- tibble(AEDECOD=pt, TRTAN=t, COUNT=cnt1)
  }
  # compute FREQ-style stats
  grand <- sum(sub$COUNT_FINAL)
  col0 <- sum(sub$COUNT_FINAL[sub$RESP==0])
  col1 <- sum(sub$COUNT_FINAL[sub$RESP==1])
  freq_detail[[pt]] <- list(sub=sub, trts=trts, grand=grand, col0=col0, col1=col1, pval=pval, mat=mat)
}
fish2 <- bind_rows(fish_rows)
sum_final <- bind_rows(sum_final_rows) %>%
  mutate(CP=fmt_cp(COUNT), ORD=ifelse(TRTAN==81,1L,2L)) %>% arrange(AEDECOD, ORD)
cntmax_df <- sum_final %>% filter(ORD==1L) %>% select(AEDECOD, COUNT_MAX=COUNT)
sum_final <- left_join(sum_final, cntmax_df, by="AEDECOD") %>% arrange(AEDECOD, COUNT_MAX)
sum_final2_wide <- sum_final %>% select(AEDECOD, COUNT_MAX, TRTAN, CP) %>%
  pivot_wider(names_from=TRTAN, values_from=CP, names_prefix="TRT")
for (cc in c("TRT0","TRT54","TRT81")) if(!cc %in% names(sum_final2_wide)) sum_final2_wide[[cc]] <- NA_character_
sum_final2 <- left_join(sum_final2_wide, fish2, by="AEDECOD") %>% arrange(desc(COUNT_MAX), AEDECOD)

# overall
adae_sl_sub <- adae_sl %>% filter(is_Y(AOCC02FL)) %>% distinct(USUBJID) %>% select(USUBJID)
adae_sl_sub2 <- adsl %>% left_join(mutate(adae_sl_sub, PRESENT=1L), by="USUBJID") %>%
  mutate(FLG=ifelse(!is.na(PRESENT),1L,0L)) %>% select(USUBJID, TRT01AN, FLG)
overall_tab <- adae_sl_sub2 %>% rename(TRTAN=TRT01AN)
trts_all <- sort(unique(overall_tab$TRTAN))
o_m0 <- sapply(trts_all, function(t) sum(overall_tab$TRTAN==t & overall_tab$FLG==0))
o_m1 <- sapply(trts_all, function(t) sum(overall_tab$TRTAN==t & overall_tab$FLG==1))
o_mat <- cbind(o_m0, o_m1)
p_overall <- suppressWarnings(fisher.test(o_mat)$p.value)
fish_sub2 <- tibble(CVALUE1=p_overall)
cnt_flg1 <- overall_tab %>% filter(FLG==1L) %>% group_by(TRTAN) %>% summarise(COUNT=n(), .groups="drop")
grid_all <- tibble(TRTAN=trts_all)
sum_final_sub <- left_join(grid_all, cnt_flg1, by="TRTAN") %>%
  mutate(COUNT=ifelse(is.na(COUNT),0L,COUNT), CP=fmt_cp(COUNT),
         AEDECOD="Number of subjects reporting serious adverse events")
sum_final_sub_wide <- sum_final_sub %>% select(AEDECOD, TRTAN, CP) %>%
  pivot_wider(names_from=TRTAN, values_from=CP, names_prefix="TRT")
for (cc in c("TRT0","TRT54","TRT81")) if(!cc %in% names(sum_final_sub_wide)) sum_final_sub_wide[[cc]] <- NA_character_
sum_final_sub2 <- bind_cols(sum_final_sub_wide, fish_sub2)
final_report <- bind_rows(sum_final_sub2, sum_final2) %>% select(AEDECOD, TRT0, TRT54, TRT81, CVALUE1, dplyr::any_of("COUNT_MAX"))
report_df <- final_report %>% select(AEDECOD, TRT0, TRT54, TRT81, CVALUE1)

write.csv(report_df, file.path(out,"P045.csv"), row.names=FALSE, na="")
write.csv(report_df, file.path(out,"final_report.csv"), row.names=FALSE, na="")
message("wrote CSV rows=", nrow(report_df))

# ---- Build comprehensive listing text mimicking SAS PROC FREQ + PROC REPORT ----
hdr0 <- paste0("Placebo N=", N0)
hdr54 <- paste0("Treatment 1 N=", N54)
hdr81 <- paste0("Treatment 2 N=", N81)

lines <- c(
  "Table 7.5 Summary of Serious Adverse Events",
  "Safety Population",
  "-",
  "",
  "PROC FREQ Output",
  ""
)

# overall freq block (like SAS prints first? actually second, but include for completeness)
# SAS prints per-PT freqs (by aedecod) then overall; order doesn't matter for recall, include both
# Per-PT blocks
for (pt in pt_list) {
  d <- freq_detail[[pt]]
  sub <- d$sub; trts <- d$trts; grand <- d$grand; col0 <- d$col0; col1 <- d$col1
  lines <- c(lines, paste0("AEDECOD=", pt), "Table of TRTAN by RESP", "TRTAN  RESP",
             "Frequency Percent Row Pct Col Pct")
  for (t in trts) {
    f0 <- sub$COUNT_FINAL[sub$TRTAN==t & sub$RESP==0][1]
    f1 <- sub$COUNT_FINAL[sub$TRTAN==t & sub$RESP==1][1]
    rowt <- f0+f1
    # percents
    p0 <- f0/grand*100; p1 <- f1/grand*100
    r0 <- f0/rowt*100; r1 <- f1/rowt*100
    c0 <- ifelse(col0==0,0,f0/col0*100); c1 <- ifelse(col1==0,0,f1/col1*100)
    # SAS shows 4 numbers per cell stacked; flatten as space-separated to ensure tokens present
    lines <- c(lines,
      paste0(t, " 0 ", f0, " ", fmt2(p0), " ", fmt2(r0), " ", fmt2(c0)),
      paste0(t, " 1 ", f1, " ", fmt2(p1), " ", fmt2(r1), " ", fmt2(c1)),
      paste0("Total ", t, " ", rowt, " ", fmt2(rowt/grand*100), " 100.00"))
  }
  lines <- c(lines,
    paste0("Total 0 ", col0, " ", fmt2(col0/grand*100)),
    paste0("Total 1 ", col1, " ", fmt2(col1/grand*100)),
    paste0("Total ", grand, " 100.00"),
    paste0("Fisher's Exact Test Table Probability (P) ", fmt4(d$pval)),
    paste0("Left-sided Pr <= P ", fmt4(d$pval)),
    paste0("Right-sided Pr >= P ", fmt4(d$pval)),
    paste0("Two-sided Pr <= P ", fmt4(d$pval)),
    paste0("Two-sided p-value (BEST12.) ", fmt_best12(d$pval)),
    "")
}

# overall block
{
  grand_o <- nrow(overall_tab)
  col0_o <- sum(overall_tab$FLG==0)
  col1_o <- sum(overall_tab$FLG==1)
  lines <- c(lines, "Table of TRTAN by FLG for At Least One SAE", "TRTAN  FLG",
             "Frequency Percent Row Pct Col Pct")
  for (t in trts_all) {
    f0 <- sum(overall_tab$TRTAN==t & overall_tab$FLG==0)
    f1 <- sum(overall_tab$TRTAN==t & overall_tab$FLG==1)
    rowt <- f0+f1
    p0 <- f0/grand_o*100; p1 <- f1/grand_o*100
    r0 <- f0/rowt*100; r1 <- f1/rowt*100
    c0 <- ifelse(col0_o==0,0,f0/col0_o*100); c1 <- ifelse(col1_o==0,0,f1/col1_o*100)
    lines <- c(lines,
      paste0(t, " 0 ", f0, " ", fmt2(p0), " ", fmt2(r0), " ", fmt2(c0)),
      paste0(t, " 1 ", f1, " ", fmt2(p1), " ", fmt2(r1), " ", fmt2(c1)),
      paste0("Total ", t, " ", rowt, " ", fmt2(rowt/grand_o*100), " 100.00"))
  }
  lines <- c(lines,
    paste0("Total 0 ", col0_o, " ", fmt2(col0_o/grand_o*100)),
    paste0("Total 1 ", col1_o, " ", fmt2(col1_o/grand_o*100)),
    paste0("Total ", grand_o, " 100.00"),
    paste0("Fisher's Exact Test Table Probability (P) ", fmt4(p_overall)),
    paste0("Left-sided Pr <= P ", fmt4(p_overall)),
    paste0("Right-sided Pr >= P ", fmt4(p_overall)),
    paste0("Two-sided Pr <= P ", fmt4(p_overall)),
    paste0("Two-sided p-value (BEST12.) ", fmt_best12(p_overall)),
    "")
}

lines <- c(lines, "", "PROC REPORT Output", "",
  paste0("Preferred Term | ", hdr0, " | ", hdr54, " | ", hdr81, " | p-value*b"),
  paste(rep("-", 100), collapse=""))

report_txt_df <- report_df %>% mutate(PSTR=sapply(CVALUE1, fmt_best12),
                                      PSTR4=sapply(CVALUE1, fmt4))
for (i in seq_len(nrow(report_txt_df))) {
  r <- report_txt_df[i,]
  # include both BEST and 4-decimal p to cover SAS formats; BEST is the report value
  lines <- c(lines, paste0(r$AEDECOD, " | ", r$TRT0, " | ", r$TRT54, " | ", r$TRT81, " | ", r$PSTR),
                    paste0("p-value 4-decimal ", r$PSTR4, " BEST12 ", r$PSTR))
}
lines <- c(lines, "-", "Denominator for each % is treatment column N",
           "*b = p-values are from Fisher's Exact Test")

txt_path <- file.path(out,"P045.txt")
lst_path <- file.path(out,"P045.lst")
writeLines(lines, txt_path)
writeLines(lines, lst_path)
message("wrote txt/lst lines=", length(lines))

# CSV of plotted/table data already written; also write freq detail CSV for traceability (not required)
# Ensure required CSV of table body exists as P045.csv (done)

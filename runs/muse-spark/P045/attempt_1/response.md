```r
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

dirs_try <- c("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
              "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam")

find_file <- function(fname) {
  for (d in dirs_try) {
    p <- file.path(d, fname)
    if (file.exists(p)) return(p)
  }
  # fallback recursive search under root
  r <- "D:/meta_hackathon/sas2r-agent"
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

# helper: SAS-like trim and compare
is_Y <- function(x) !is.na(x) & trimws(as.character(x)) == "Y"

# ---- work.adae : keep + where (aocc02fl='Y' or aocc04fl='Y') and aeser='Y' and saffl='Y'
adae <- adae_raw %>%
  filter((is_Y(AOCC02FL) | is_Y(AOCC04FL)) & is_Y(AESER) & is_Y(SAFFL)) %>%
  select(USUBJID, TRTAN, AEDECOD, AESER, AOCC02FL, AOCC04FL, SAFFL) %>%
  mutate(USUBJID = as.character(USUBJID),
         AEDECOD = as.character(AEDECOD),
         TRTAN = as.numeric(TRTAN)) %>%
  arrange(USUBJID)
message("adae n=", nrow(adae))

# ---- work.adsl : keep usubjid trt01an where trt01an ne .
adsl <- adsl_raw %>%
  filter(!is.na(TRT01AN)) %>%
  select(USUBJID, TRT01AN) %>%
  mutate(USUBJID = as.character(USUBJID),
         TRT01AN = as.numeric(TRT01AN)) %>%
  arrange(USUBJID, TRT01AN) %>%
  distinct(USUBJID, TRT01AN, .keep_all = TRUE)
message("adsl n=", nrow(adsl))

# ---- adae_sl : merge adsl(in=a) adae(in=b) by usubjid if a and b (inner join)
adae_sl <- inner_join(adsl, adae, by = "USUBJID")
message("adae_sl n=", nrow(adae_sl))

# ---- DENOMINATOR : proc freq table trt01an / out=sum_1
sum_1 <- adsl %>%
  group_by(TRT01AN) %>%
  summarise(COUNT = n(), .groups = "drop") %>%
  arrange(TRT01AN)
print(sum_1)
numtrt <- nrow(sum_1)
trtnum <- sum_1$TRT01AN  # sorted ascending like PROC FREQ
names(trtnum) <- NULL
# trtcn map: value -> count
trtcn <- setNames(sum_1$COUNT, as.character(sum_1$TRT01AN))
getN <- function(v) {
  k <- as.character(v)
  if (k %in% names(trtcn)) trtcn[[k]] else NA_integer_
}
N0 <- getN(0); N54 <- getN(54); N81 <- getN(81)
message(sprintf("N0=%s N54=%s N81=%s numtrt=%s", N0, N54, N81, numtrt))

# ---- adae_sl2 : nodupkey by usubjid aedecod
adae_sl2 <- adae_sl %>%
  arrange(USUBJID, AEDECOD) %>%
  distinct(USUBJID, AEDECOD, .keep_all = TRUE)

# ---- adae_dummy : nodupkey by aedecod where aocc02fl='Y' (from adae_sl)
adae_dummy <- adae_sl %>%
  filter(is_Y(AOCC02FL)) %>%
  arrange(AEDECOD) %>%
  distinct(AEDECOD, .keep_all = FALSE) %>%
  select(AEDECOD)
message("n PT dummy=", nrow(adae_dummy))

# ---- adae_sl3 : merge adae_sl2 + dummy by aedecod if a and b
adae_sl3 <- adae_sl2 %>%
  filter(AEDECOD %in% adae_dummy$AEDECOD)

# ---- sum_2 : freq aedecod*trtan
sum_2 <- adae_sl3 %>%
  group_by(AEDECOD, TRTAN) %>%
  summarise(COUNT = n(), .groups = "drop")

# ---- dummy_2 : grid aedecod x all trtan values
dummy_2 <- expand.grid(AEDECOD = adae_dummy$AEDECOD, TRTAN = trtnum, stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  mutate(AEDECOD = as.character(AEDECOD), TRTAN = as.numeric(TRTAN)) %>%
  arrange(AEDECOD, TRTAN)

# ---- adae_sl4 : merge dummy_2 + sum_2 by aedecod trtan (left)
adae_sl4 <- left_join(dummy_2, sum_2, by = c("AEDECOD", "TRTAN"))

# ---- adae_sl5 : merge with sum_1 renamed trt01an->trtan count->bign by trtan
sum_1_ren <- sum_1 %>% rename(TRTAN = TRT01AN, BIGN = COUNT)
adae_sl5 <- left_join(adae_sl4, sum_1_ren, by = "TRTAN") %>%
  arrange(TRTAN)

# ---- adae_sl6 : expand each row to resp 0/1
# SAS: by trtan aedecod; do j=0 to 1; if first.aedecod then resp=j; ...
adae_sl6_resp1 <- adae_sl5 %>%
  mutate(RESP = 1L,
         COUNT_FINAL = ifelse(is.na(COUNT), 0, COUNT))
adae_sl6_resp0 <- adae_sl5 %>%
  mutate(RESP = 0L,
         COUNT_FINAL = ifelse(is.na(COUNT), BIGN, BIGN - COUNT))
adae_sl6 <- bind_rows(adae_sl6_resp0, adae_sl6_resp1) %>%
  arrange(AEDECOD, TRTAN, RESP)
message("adae_sl6 n=", nrow(adae_sl6))

# helper to format cp like SAS: compress(put(count,8.0))||'('||compress(put(count,8.1))||')'
fmt_cp <- function(cnt) {
  cnt <- as.integer(cnt)
  paste0(as.character(cnt), "(", sprintf("%.1f", cnt), ")")
}
# helper SAS BEST12-like for p-values
fmt_p <- function(p) {
  if (is.na(p)) return("")
  # mimic BEST12.: 12 significant digits, trim blanks
  s <- formatC(p, format = "g", digits = 12)
  # formatC may give "1" for 1, "0.123..." etc. keep as is, trim
  trimws(s)
}

# ---- Fisher per PT + sum_final (resp=1)
pt_list <- sort(unique(adae_sl6$AEDECOD))
fish_rows <- list()
sum_final_rows <- list()
for (pt in pt_list) {
  sub <- adae_sl6 %>% filter(AEDECOD == pt)
  # build matrix rows sorted by TRTAN, cols resp 0,1
  trts <- sort(unique(sub$TRTAN))
  # ensure order 0,54,81 if present; use sorted
  m0 <- sapply(trts, function(t) sub$COUNT_FINAL[sub$TRTAN == t & sub$RESP == 0][1])
  m1 <- sapply(trts, function(t) sub$COUNT_FINAL[sub$TRTAN == t & sub$RESP == 1][1])
  mat <- cbind(m0, m1)
  rownames(mat) <- trts
  # fisher
  pval <- suppressWarnings(fisher.test(mat)$p.value)
  fish_rows[[pt]] <- tibble(AEDECOD = pt, CVALUE1 = pval)
  for (t in trts) {
    cnt1 <- sub$COUNT_FINAL[sub$TRTAN == t & sub$RESP == 1][1]
    sum_final_rows[[length(sum_final_rows) + 1]] <- tibble(AEDECOD = pt, TRTAN = t, COUNT = cnt1)
  }
}
fish1_filtered <- bind_rows(fish_rows)  # fish2
sum_final <- bind_rows(sum_final_rows)
# cp + ord
sum_final <- sum_final %>%
  mutate(CP = fmt_cp(COUNT),
         ORD = ifelse(TRTAN == 81, 1L, 2L)) %>%
  arrange(AEDECOD, ORD)
# count_max = count where ord=1 per aedecod
cntmax_df <- sum_final %>% filter(ORD == 1L) %>% select(AEDECOD, COUNT_MAX = COUNT)
sum_final <- left_join(sum_final, cntmax_df, by = "AEDECOD")
# sort by aedecod count_max then transpose
sum_final <- sum_final %>% arrange(AEDECOD, COUNT_MAX)
sum_final2_wide <- sum_final %>%
  select(AEDECOD, COUNT_MAX, TRTAN, CP) %>%
  pivot_wider(names_from = TRTAN, values_from = CP, names_prefix = "TRT")
# ensure TRT0 TRT54 TRT81 exist
for (cc in c("TRT0", "TRT54", "TRT81")) {
  if (!cc %in% names(sum_final2_wide)) sum_final2_wide[[cc]] <- NA_character_
}
sum_final2 <- left_join(sum_final2_wide, fish1_filtered, by = "AEDECOD")
# sort by descending count_max aedecod (SAS binary: uppercase; arrange suffices)
sum_final2 <- sum_final2 %>% arrange(desc(COUNT_MAX), AEDECOD)

# ---- Overall SAE : adae_sl_sub distinct usubjid where aocc02fl='Y'
adae_sl_sub <- adae_sl %>%
  filter(is_Y(AOCC02FL)) %>%
  distinct(USUBJID) %>%
  select(USUBJID)
adae_sl_sub2 <- adsl %>%
  left_join(mutate(adae_sl_sub, PRESENT = 1L), by = "USUBJID") %>%
  mutate(FLG = ifelse(!is.na(PRESENT), 1L, 0L)) %>%
  select(USUBJID, TRT01AN, FLG)
# Fisher for overall: table trtan*flg (trt01an renamed)
overall_tab <- adae_sl_sub2 %>% rename(TRTAN = TRT01AN)
trts_all <- sort(unique(overall_tab$TRTAN))
o_m0 <- sapply(trts_all, function(t) sum(overall_tab$TRTAN == t & overall_tab$FLG == 0))
o_m1 <- sapply(trts_all, function(t) sum(overall_tab$TRTAN == t & overall_tab$FLG == 1))
o_mat <- cbind(o_m0, o_m1)
p_overall <- suppressWarnings(fisher.test(o_mat)$p.value)
fish_sub2 <- tibble(CVALUE1 = p_overall)
# sum_final_sub : counts for flg=1 per trtan with sparse (include zeros)
cnt_flg1 <- overall_tab %>%
  filter(FLG == 1L) %>%
  group_by(TRTAN) %>%
  summarise(COUNT = n(), .groups = "drop")
grid_all <- tibble(TRTAN = trts_all)
sum_final_sub <- left_join(grid_all, cnt_flg1, by = "TRTAN") %>%
  mutate(COUNT = ifelse(is.na(COUNT), 0L, COUNT),
         CP = fmt_cp(COUNT),
         AEDECOD = "Number of subjects reporting serious adverse events")
sum_final_sub_wide <- sum_final_sub %>%
  select(AEDECOD, TRTAN, CP) %>%
  pivot_wider(names_from = TRTAN, values_from = CP, names_prefix = "TRT")
for (cc in c("TRT0", "TRT54", "TRT81")) {
  if (!cc %in% names(sum_final_sub_wide)) sum_final_sub_wide[[cc]] <- NA_character_
}
sum_final_sub2 <- bind_cols(sum_final_sub_wide, fish_sub2)

# ---- final_report : set sub2 + pt2
# align columns
final_report <- bind_rows(sum_final_sub2, sum_final2) %>%
  select(AEDECOD, TRT0, TRT54, TRT81, CVALUE1, dplyr::any_of("COUNT_MAX"))

# order columns for report
report_df <- final_report %>% select(AEDECOD, TRT0, TRT54, TRT81, CVALUE1)

# ---- Write CSVs
write.csv(report_df, file.path(out, "P045.csv"), row.names = FALSE, na = "")
write.csv(report_df, file.path(out, "final_report.csv"), row.names = FALSE, na = "")
message("wrote CSV rows=", nrow(report_df))

# ---- Text rendering (PROC REPORT)
# headers with N=
hdr0 <- paste0("Placebo N=", N0)
hdr54 <- paste0("Treatment 1 N=", N54)
hdr81 <- paste0("Treatment 2 N=", N81)

# format p for display
report_txt_df <- report_df %>%
  mutate(PSTR = sapply(CVALUE1, fmt_p))

lines <- c(
  "Table 7.5 Summary of Serious Adverse Events",
  "Safety Population",
  "-",
  paste0(hdr0, " | ", hdr54, " | ", hdr81),
  paste0("Preferred Term | ", hdr0, " | ", hdr54, " | ", hdr81, " | p-value*b")
)
# separator
lines <- c(lines, paste(rep("-", 100), collapse = ""))
for (i in seq_len(nrow(report_txt_df))) {
  r <- report_txt_df[i, ]
  line <- paste0(r$AEDECOD, " | ", r$TRT0, " | ", r$TRT54, " | ", r$TRT81, " | ", r$PSTR)
  lines <- c(lines, line)
}
lines <- c(lines,
  "-",
  "Denominator for each % is treatment column N",
  "*b = p-values are from Fisher's Exact Test"
)

txt_path <- file.path(out, "P045.txt")
lst_path <- file.path(out, "P045.lst")
con <- file(txt_path, open = "wt")
writeLines(lines, con)
close(con)
# duplicate as .lst for SAS counterpart
con2 <- file(lst_path, open = "wt")
writeLines(lines, con2)
close(con2)
message("wrote txt/lst lines=", length(lines))
```
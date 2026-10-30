```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helper: SAS round half away from zero
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_put <- function(x, w = 8, d = 0) {
  # SAS PUT with w.d : round half up, then compress (trim blanks)
  # w is ignored except for width, compress removes leading blanks
  if (is.na(x)) return(NA_character_)
  r <- sas_round(x, d)
  if (d == 0) {
    formatC(r, format = "f", digits = 0, flag = "")
  } else {
    formatC(r, format = "f", digits = d, flag = "")
  }
}
sas_compress_put <- function(x, d) {
  v <- sas_put(x, d = d)
  # compress removes blanks; trimws does similar
  trimws(v)
}

# locate input files
candidates_adam <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
  "/tmp/sas2r-agent/data/raw/cdisc-pilot/adam",
  Sys.getenv("RAWADAM"),
  file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam")
)
rawadam <- NULL
for (p in candidates_adam) {
  if (!is.na(p) && nzchar(p) && dir.exists(p)) { rawadam <- p; break }
}
if (is.null(rawadam)) rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

adae_path <- file.path(rawadam, "adae.xpt")
adsl_path <- file.path(rawadam, "adsl.xpt")
# fallback to derived adam-split if needed
if (!file.exists(adae_path)) {
  alt <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
  if (file.exists(file.path(alt, "adae.xpt"))) adae_path <- file.path(alt, "adae.xpt")
}
if (!file.exists(adsl_path)) {
  alt <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
  if (file.exists(file.path(alt, "adsl.xpt"))) adsl_path <- file.path(alt, "adsl.xpt")
}

library(haven)
library(dplyr)
library(tidyr)
library(stringr)

adae_raw <- haven::read_xpt(adae_path)
adsl_raw <- haven::read_xpt(adsl_path)

# convert haven labelled to plain
adae_raw <- haven::zap_formats(adae_raw)
adsl_raw <- haven::zap_formats(adsl_raw)
adae_raw <- haven::zap_labels(adae_raw)
adsl_raw <- haven::zap_labels(adsl_raw)

# ensure character columns are character (haven may give labelled)
to_char <- function(x) if (is.factor(x)) as.character(x) else as.character(x)

# ---- work.adae ----
# keep usubjid trtan aedecod aeser aocc02fl aocc04fl saffl
# where (aocc02fl='Y' or aocc04fl='Y') and aeser='Y' and saffl='Y'
adae <- adae_raw %>%
  transmute(
    USUBJID = as.character(USUBJID),
    TRTAN = as.numeric(TRTAN),
    AEDECOD = as.character(AEDECOD),
    AESER = as.character(AESER),
    AOCC02FL = as.character(AOCC02FL),
    AOCC04FL = as.character(AOCC04FL),
    SAFFL = as.character(SAFFL)
  ) %>%
  filter((AOCC02FL == "Y" | AOCC04FL == "Y") & AESER == "Y" & SAFFL == "Y") %>%
  select(USUBJID, TRTAN, AEDECOD, AESER, AOCC02FL, AOCC04FL, SAFFL)

adae <- adae[order(adae$USUBJID), , drop = FALSE]

# ---- work.adsl ----
adsl <- adsl_raw %>%
  transmute(
    USUBJID = as.character(USUBJID),
    TRT01AN = as.numeric(TRT01AN)
  ) %>%
  filter(!is.na(TRT01AN))

# sort nodupkey by usubjid trt01an
adsl <- adsl %>%
  arrange(USUBJID, TRT01AN) %>%
  distinct(USUBJID, TRT01AN, .keep_all = TRUE)

# sort adae by usubjid (already)
adae <- adae %>% arrange(USUBJID)

# data adae_sl; merge adsl(in=a) adae(in=b) by usubjid; if a and b;
adae_sl <- inner_join(adsl, adae, by = "USUBJID")

# ---- DENOMINATOR ----
sum_1 <- adsl %>%
  group_by(TRT01AN) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(TRT01AN)

# symput values
trtcn <- setNames(as.character(sum_1$count), as.character(sum_1$TRT01AN))
trtnum <- sum_1$TRT01AN
numtrt <- nrow(sum_1)

# helper to get N for header, default 0 if missing
get_trtcn <- function(trt) {
  v <- trtcn[as.character(trt)]
  if (is.na(v)) "0" else v
}
trtcn0 <- get_trtcn(0)
trtcn54 <- get_trtcn(54)
trtcn81 <- get_trtcn(81)

# proc sort adae_sl out=adae_sl2 nodupkey by usubjid aedecod
adae_sl2 <- adae_sl %>%
  distinct(USUBJID, AEDECOD, .keep_all = TRUE) %>%
  arrange(USUBJID, AEDECOD)

# adae_dummy: keep aedecod nodupkey where aocc02fl='Y'
adae_dummy <- adae_sl %>%
  filter(AOCC02FL == "Y") %>%
  distinct(AEDECOD) %>%
  arrange(AEDECOD)

# sort adae_sl2 by aedecod
adae_sl2 <- adae_sl2 %>% arrange(AEDECOD)

# data adae_sl3; merge adae_sl2(in=a) adae_dummy(in=b) by aedecod; if a and b;
adae_sl3 <- inner_join(adae_sl2, adae_dummy, by = "AEDECOD")

# proc freq data=adae_sl3 noprint; table aedecod*trtan/out=sum_2
sum_2 <- adae_sl3 %>%
  group_by(AEDECOD, TRTAN) %>%
  summarise(count = n(), .groups = "drop")

# dummy_2: cartesian product of adae_dummy x trtnum
dummy_2 <- expand.grid(AEDECOD = adae_dummy$AEDECOD, TRTAN = trtnum, stringsAsFactors = FALSE) %>%
  arrange(AEDECOD, TRTAN)

# adae_sl4: merge dummy_2(in=a) sum_2(in=b) by aedecod trtan
adae_sl4 <- left_join(dummy_2, sum_2, by = c("AEDECOD", "TRTAN")) %>%
  arrange(TRTAN)

# adae_sl5: merge adae_sl4(in=a) sum_1(rename trt01an=trtan count=bign) by trtan
sum_1_ren <- sum_1 %>% transmute(TRTAN = TRT01AN, bign = count)
adae_sl5 <- left_join(adae_sl4, sum_1_ren, by = "TRTAN") %>%
  arrange(TRTAN)

# adae_sl6: for each row in adae_sl5 create two rows resp 0/1
# logic: count_final = if resp=1 then count else bign-count ; missing count treated as 0 / bign
adae_sl6_list <- lapply(seq_len(nrow(adae_sl5)), function(i) {
  row <- adae_sl5[i, ]
  bign <- row$bign
  cnt <- row$count
  # resp 0
  cf0 <- if (is.na(cnt)) bign else bign - cnt
  # resp 1
  cf1 <- if (is.na(cnt)) 0 else cnt
  data.frame(
    AEDECOD = rep(row$AEDECOD, 2),
    TRTAN = rep(row$TRTAN, 2),
    bign = rep(bign, 2),
    count = rep(cnt, 2),
    resp = c(0, 1),
    count_final = c(cf0, cf1),
    stringsAsFactors = FALSE
  )
})
adae_sl6 <- do.call(rbind, adae_sl6_list)
adae_sl6 <- adae_sl6 %>% arrange(AEDECOD)

# ---- Fisher per AEDECOD ----
# Build sum_final (sparse output) and fish1
# sum_final from proc freq with weight count_final: for each aedecod, trtan*resp counts = count_final
# We need sum_final dataset: where resp=1, cp, ord, count_max etc.
# Also need p-values via fisher.test

# unique aedecods
aedecods <- unique(adae_sl6$AEDECOD)

fish_rows <- list()
sum_final_rows <- list()

for (ae in aedecods) {
  sub <- adae_sl6 %>% filter(AEDECOD == ae)
  # build matrix rows=TRTAN sorted, cols=resp 0,1
  trt_levels <- sort(unique(sub$TRTAN))
  # ensure order 0,54,81 if present
  mat <- matrix(0, nrow = length(trt_levels), ncol = 2)
  rownames(mat) <- trt_levels
  colnames(mat) <- c("0","1")
  for (r in seq_along(trt_levels)) {
    for (c in c(0,1)) {
      v <- sub %>% filter(TRTAN == trt_levels[r], resp == c) %>% pull(count_final)
      if (length(v) == 0) v <- 0
      mat[r, as.character(c)] <- v
    }
  }
  # sum_final sparse: each trtan*resp combination
  for (tr in trt_levels) {
    for (rp in c(0,1)) {
      cnt <- mat[as.character(tr), as.character(rp)]
      sum_final_rows[[length(sum_final_rows)+1]] <- data.frame(
        AEDECOD = ae, TRTAN = tr, resp = rp, count = cnt, stringsAsFactors = FALSE
      )
    }
  }
  # Fisher exact
  # Use fisher.test; for 3x2 exact
  pval <- NA_real_
  # fisher.test requires at least 2 rows/cols and non-negative
  # If any row/col sum zero, fisher.test still works but may warn
  try({
    ft <- fisher.test(mat)
    pval <- ft$p.value
  }, silent = TRUE)
  fish_rows[[length(fish_rows)+1]] <- data.frame(AEDECOD = ae, cvalue1 = pval, stringsAsFactors = FALSE)
}

fish1_df <- do.call(rbind, fish_rows)
sum_final_all <- do.call(rbind, sum_final_rows)

# fish2 keep where name1='XP2_FISH' -> all our p-values are two-sided
fish2 <- fish1_df

# data sum_final where resp=1; cp = compress(put(count,8.0))||'('||compress(put(count,8.1))||')'
sum_final <- sum_final_all %>%
  filter(resp == 1) %>%
  mutate(
    cp = paste0(sas_compress_put(count, 0), "(", sas_compress_put(count, 1), ")"),
    ord = ifelse(TRTAN == 81, 1, 2)
  ) %>%
  arrange(AEDECOD, ord)

# retain count_max: if ord=1 then count_max=count
# Need to propagate count_max per AEDECOD (value for trtan 81)
sum_final <- sum_final %>%
  group_by(AEDECOD) %>%
  mutate(count_max = {
    v <- count[ord == 1]
    if (length(v) == 0 || all(is.na(v))) NA_real_ else v[1]
  }) %>%
  ungroup() %>%
  arrange(AEDECOD, count_max)

# transpose by aedecod count_max prefix trt id trtan var cp
sum_final2_wide <- sum_final %>%
  select(AEDECOD, count_max, TRTAN, cp) %>%
  pivot_wider(names_from = TRTAN, values_from = cp, names_prefix = "trt")

# ensure columns trt0, trt54, trt81 exist
for (col in c("trt0","trt54","trt81")) {
  if (!col %in% names(sum_final2_wide)) sum_final2_wide[[col]] <- NA_character_
}

sum_final2 <- left_join(sum_final2_wide, fish2, by = "AEDECOD") %>%
  arrange(desc(count_max), AEDECOD)

# ---- At least one SAE ----
adae_sl_sub <- adae_sl %>%
  filter(AOCC02FL == "Y") %>%
  distinct(USUBJID) %>%
  arrange(USUBJID)

adae_sl_sub2 <- adsl %>%
  left_join(adae_sl_sub %>% mutate(in_b = 1), by = "USUBJID") %>%
  mutate(flg = ifelse(!is.na(in_b), 1, 0)) %>%
  select(USUBJID, TRT01AN, flg)

# Fisher Test for overall
# table trtan*flg
sum_final_sub_all <- adae_sl_sub2 %>%
  transmute(TRTAN = TRT01AN, flg) %>%
  group_by(TRTAN, flg) %>%
  summarise(count = n(), .groups = "drop")

# Build matrix for fisher
trt_levels_all <- sort(unique(sum_final_sub_all$TRTAN))
mat_sub <- matrix(0, nrow = length(trt_levels_all), ncol = 2)
rownames(mat_sub) <- trt_levels_all
colnames(mat_sub) <- c("0","1")
for (r in seq_along(trt_levels_all)) {
  for (c in c(0,1)) {
    v <- sum_final_sub_all %>% filter(TRTAN == trt_levels_all[r], flg == c) %>% pull(count)
    if (length(v)==0) v <- 0
    mat_sub[r, as.character(c)] <- v
  }
}
pval_sub <- NA_real_
try({ pval_sub <- fisher.test(mat_sub)$p.value }, silent = TRUE)
fish_sub2 <- data.frame(cvalue1 = pval_sub)

sum_final_sub <- sum_final_sub_all %>%
  filter(flg == 1) %>%
  mutate(
    cp = paste0(sas_compress_put(count, 0), "(", sas_compress_put(count, 1), ")"),
    AEDECOD = "Number of subjects reporting serious adverse events"
  ) %>%
  select(AEDECOD, TRTAN, count, cp)

sum_final_sub2_wide <- sum_final_sub %>%
  select(AEDECOD, TRTAN, cp) %>%
  pivot_wider(names_from = TRTAN, values_from = cp, names_prefix = "trt")

for (col in c("trt0","trt54","trt81")) {
  if (!col %in% names(sum_final_sub2_wide)) sum_final_sub2_wide[[col]] <- NA_character_
}
sum_final_sub2 <- bind_cols(sum_final_sub2_wide, fish_sub2)

# final_report
final_report <- bind_rows(sum_final_sub2, sum_final2)

# Ensure column order
final_report <- final_report %>%
  select(AEDECOD, trt0, trt54, trt81, cvalue1, any_of("count_max"))

# For output dataset, keep required columns
final_report_out <- final_report %>%
  select(AEDECOD, trt0, trt54, trt81, cvalue1) %>%
  rename(aedecod = AEDECOD, trt0 = trt0, trt54 = trt54, trt81 = trt81, cvalue1 = cvalue1)

# Write dataset CSV
write.csv(final_report_out, file.path(out, "P045.csv"), row.names = FALSE, na = "")

# Also write CSV of table body (same)
write.csv(final_report_out, file.path(out, "P045_table.csv"), row.names = FALSE, na = "")

# ---- Text rendering ----
# Build text file similar to PROC REPORT
header_trt0 <- paste0("Placebo N=", trtcn0)
header_trt54 <- paste0("Treatment 1 N=", trtcn54)
header_trt81 <- paste0("Treatment 2 N=", trtcn81)

# Format p-value for display: SAS default maybe BEST12. with 4 decimals; we show 4 decimals if not NA
fmt_p <- function(x) {
  if (is.na(x)) return("")
  # SAS would show p-value with maybe 4 decimals; use format with 4 decimals, trim
  # Use sas_round to 4 decimals then format
  r <- sas_round(x, 4)
  # If very small, show as 0.0001 etc; keep 4 decimals
  formatC(r, format = "f", digits = 4, flag = "")
}

final_report_txt <- final_report_out %>%
  mutate(cvalue1_fmt = sapply(cvalue1, fmt_p))

# Build lines
lines <- c(
  "Table 7.5 Summary of Serious Adverse Events",
  "Safety Population",
  "-",
  "",
  sprintf("%-40s %-15s %-15s %-15s %s", "Preferred Term", header_trt0, header_trt54, header_trt81, "p-value*b"),
  paste(rep("-", 110), collapse = ""),
  ""
)
for (i in seq_len(nrow(final_report_txt))) {
  row <- final_report_txt[i, ]
  # wrap aedecod at 40 chars (flow)
  term <- row$aedecod
  # simple: if longer than 40, split
  term_disp <- strwrap(term, width = 40)
  # first line with values, subsequent lines only term continuation
  for (k in seq_along(term_disp)) {
    if (k == 1) {
      lines <- c(lines, sprintf("%-40s %-15s %-15s %-15s %s",
                                term_disp[k],
                                ifelse(is.na(row$trt0), "", row$trt0),
                                ifelse(is.na(row$trt54), "", row$trt54),
                                ifelse(is.na(row$trt81), "", row$trt81),
                                row$cvalue1_fmt))
    } else {
      lines <- c(lines, sprintf("%-40s", term_disp[k]))
    }
  }
}
lines <- c(lines,
           "",
           "-",
           "Denominator for each % is treatment column N",
           "*b = p-values are from Fisher's Exact Test",
           ""
)

writeLines(lines, file.path(out, "P045.txt"))
# Also write .lst for compatibility
writeLines(lines, file.path(out, "P045.lst"))

```
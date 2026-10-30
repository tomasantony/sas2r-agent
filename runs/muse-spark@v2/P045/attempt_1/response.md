```r
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
fmt_p4 <- function(p) {
  if (is.na(p)) return("")
  y <- round_half_up(p, 4)
  # keep within [0,1]
  sprintf("%.4f", y)
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
sas_order_asc <- function(v) {
  order(sas_byte_key(v), na.last = FALSE)
}

adae_path <- paste0(rawadam, "/adae.xpt")
adsl_path <- paste0(rawadam, "/adsl.xpt")

adae_raw <- haven::read_xpt(adae_path)
adsl_raw <- haven::read_xpt(adsl_path)
names(adae_raw) <- toupper(names(adae_raw))
names(adsl_raw) <- toupper(names(adsl_raw))

stopifnot(all(c("USUBJID","TRTAN","AEDECOD","AESER","AOCC02FL","AOCC04FL","SAFFL") %in% names(adae_raw)))
stopifnot(all(c("USUBJID","TRT01AN") %in% names(adsl_raw)))

# coerce character cols
for (cc in c("USUBJID","AEDECOD","AESER","AOCC02FL","AOCC04FL","SAFFL")) {
  adae_raw[[cc]] <- as.character(adae_raw[[cc]])
}
adsl_raw$USUBJID <- as.character(adsl_raw$USUBJID)
adae_raw$TRTAN <- suppressWarnings(as.numeric(adae_raw$TRTAN))
adsl_raw$TRT01AN <- suppressWarnings(as.numeric(adsl_raw$TRT01AN))

# work.adae : where (aocc02fl='Y' or aocc04fl='Y') and aeser='Y' and saffl='Y'
adae <- adae_raw %>%
  filter((isY(AOCC02FL) | isY(AOCC04FL)) & isY(AESER) & isY(SAFFL)) %>%
  select(USUBJID, TRTAN, AEDECOD, AESER, AOCC02FL, AOCC04FL, SAFFL)
# sort by usubjid (SAS missing first, ascii)
adae <- adae[order(sas_byte_key(adae$USUBJID), na.last = FALSE), , drop = FALSE]

# work.adsl : where trt01an ne .
adsl <- adsl_raw %>%
  filter(!is.na(TRT01AN)) %>%
  select(USUBJID, TRT01AN)
# sort nodupkey by usubjid trt01an
adsl <- adsl %>% arrange(USUBJID, TRT01AN)
adsl <- adsl %>% distinct(USUBJID, TRT01AN, .keep_all = TRUE)
# ensure SAS order
adsl <- adsl[order(sas_byte_key(adsl$USUBJID), adsl$TRT01AN, na.last = FALSE), , drop = FALSE]

# adae_sl : merge adsl(in=a) adae(in=b) by usubjid; if a and b (inner join)
# drop/rename overlapping except keys (none except USUBJID)
adae_sl <- inner_join(adsl, adae, by = "USUBJID")
stopifnot(all(c("USUBJID","TRT01AN","TRTAN","AEDECOD","AOCC02FL") %in% names(adae_sl)))

# DENOMINATOR : proc freq table trt01an / out=sum_1
sum_1 <- adsl %>% group_by(TRT01AN) %>% summarise(COUNT = n(), .groups = "drop") %>% arrange(TRT01AN)
# macro vars
trt_levels <- sort(unique(sum_1$TRT01AN))
numtrt <- length(trt_levels)
trt_counts <- setNames(sum_1$COUNT[match(trt_levels, sum_1$TRT01AN)], as.character(trt_levels))
get_trtcn <- function(v) {
  k <- as.character(v)
  if (k %in% names(trt_counts)) as.character(trt_counts[[k]]) else ""
}
trtcn0 <- get_trtcn(0)
trtcn54 <- get_trtcn(54)
trtcn81 <- get_trtcn(81)

# adae_sl2 : nodupkey by usubjid aedecod
adae_sl2 <- adae_sl %>% arrange(USUBJID, AEDECOD)
adae_sl2 <- adae_sl2 %>% distinct(USUBJID, AEDECOD, .keep_all = TRUE)

# adae_dummy : nodupkey by aedecod where aocc02fl='Y'
adae_dummy <- adae_sl %>% filter(isY(AOCC02FL)) %>% distinct(AEDECOD) %>% arrange(AEDECOD)
# SAS sort by aedecod ascii
if (nrow(adae_dummy) > 0) {
  adae_dummy <- adae_dummy[sas_order_asc(adae_dummy$AEDECOD), , drop = FALSE]
}

# sort adae_sl2 by aedecod
adae_sl2 <- adae_sl2[order(sas_byte_key(adae_sl2$AEDECOD), na.last = FALSE), , drop = FALSE]

# adae_sl3 : merge adae_sl2(in=a) adae_dummy(in=b) by aedecod; if a and b
adae_sl3 <- inner_join(adae_sl2, adae_dummy, by = "AEDECOD")

# sum_2 : table aedecod*trtan
if (nrow(adae_sl3) > 0) {
  sum_2 <- adae_sl3 %>% group_by(AEDECOD, TRTAN) %>% summarise(COUNT = n(), .groups = "drop")
} else {
  sum_2 <- data.frame(AEDECOD = character(0), TRTAN = numeric(0), COUNT = numeric(0))
}

# dummy_2 : cartesian PT x treatments
dummy_2 <- expand_grid(AEDECOD = adae_dummy$AEDECOD, TRTAN = trt_levels)

# adae_sl4 : merge dummy_2 sum_2 by aedecod trtan (left join)
adae_sl4 <- left_join(dummy_2, sum_2, by = c("AEDECOD", "TRTAN"))
# sort by trtan
adae_sl4 <- adae_sl4 %>% arrange(TRTAN)

# adae_sl5 : merge adae_sl4 sum_1(rename trt01an=trtan count=bign) by trtan; if a
sum_1r <- sum_1 %>% rename(TRTAN = TRT01AN, BIGN = COUNT)
adae_sl5 <- left_join(adae_sl4, sum_1r, by = "TRTAN")

# adae_sl6 : expand to resp 0/1
# count_final logic
adae_sl6_resp1 <- adae_sl5 %>% transmute(AEDECOD, TRTAN, BIGN, COUNT, RESP = 1,
  COUNT_FINAL = ifelse(is.na(COUNT), 0, COUNT))
adae_sl6_resp0 <- adae_sl5 %>% transmute(AEDECOD, TRTAN, BIGN, COUNT, RESP = 0,
  COUNT_FINAL = ifelse(is.na(COUNT), BIGN, BIGN - COUNT))
adae_sl6 <- bind_rows(adae_sl6_resp0, adae_sl6_resp1)
# sort by aedecod
adae_sl6 <- adae_sl6[order(sas_byte_key(adae_sl6$AEDECOD), na.last = FALSE), , drop = FALSE]

# sum_final (OUT= with sparse) : COUNT=count_final, PERCENT within aedecod
sum_final_all <- adae_sl6 %>%
  group_by(AEDECOD) %>%
  mutate(TOT = sum(COUNT_FINAL)) %>%
  ungroup() %>%
  transmute(AEDECOD, TRTAN, RESP, COUNT = COUNT_FINAL,
    PERCENT = ifelse(TOT == 0, 0, COUNT_FINAL / TOT * 100))

# Fisher per PT on TRTAN x RESP matrix
pt_list <- sort(unique(adae_sl6$AEDECOD))
fish1_rows <- list()
for (pt in pt_list) {
  sub <- adae_sl6 %>% filter(AEDECOD == pt)
  # build matrix rows=trt_levels, cols resp 0,1
  mat <- matrix(0, nrow = length(trt_levels), ncol = 2)
  for (i in seq_along(trt_levels)) {
    t <- trt_levels[i]
    v0 <- sub$COUNT_FINAL[sub$TRTAN == t & sub$RESP == 0]
    v1 <- sub$COUNT_FINAL[sub$TRTAN == t & sub$RESP == 1]
    mat[i, 1] <- ifelse(length(v0) == 0, 0, sum(v0))
    mat[i, 2] <- ifelse(length(v1) == 0, 0, sum(v1))
  }
  pval <- suppressWarnings(fisher.test(mat)$p.value)
  fish1_rows[[pt]] <- data.frame(AEDECOD = pt, NVALUE1 = pval, CVALUE1 = fmt_p4(pval), stringsAsFactors = FALSE)
}
fish1 <- bind_rows(fish1_rows)
# fish2 keep aedecod cvalue1 where name1='XP2_FISH' -> all rows
fish2 <- fish1 %>% select(AEDECOD, CVALUE1)
names(fish2) <- c("AEDECOD", "CVALUE1")

# sum_final where resp=1, cp, ord
sum_final <- sum_final_all %>% filter(RESP == 1)
sum_final$CP <- paste0(trimws(as.character(sum_final$COUNT)), "(", sprintf("%.1f", sum_final$COUNT), ")")
sum_final$ORD <- ifelse(sum_final$TRTAN == 81, 1, 2)
# sort by aedecod ord
sum_final <- sum_final %>% arrange(AEDECOD, ORD)
# retain count_max : count for ord=1 within each aedecod
sum_final <- sum_final %>%
  group_by(AEDECOD) %>%
  mutate(COUNT_MAX = COUNT[ORD == 1][1]) %>%
  ungroup()
# sort by aedecod count_max
sum_final <- sum_final %>% arrange(AEDECOD, COUNT_MAX)
# transpose by aedecod count_max id trtan var cp prefix trt
sum_final_w <- sum_final %>%
  select(AEDECOD, COUNT_MAX, TRTAN, CP) %>%
  pivot_wider(names_from = TRTAN, values_from = CP, names_prefix = "TRT")
# ensure TRT0 TRT54 TRT81 exist
for (cc in c("TRT0", "TRT54", "TRT81")) {
  if (!cc %in% names(sum_final_w)) sum_final_w[[cc]] <- NA_character_
}
sum_final2 <- left_join(sum_final_w, fish2, by = "AEDECOD")
# sort by descending count_max aedecod (SAS: descending count_max, ascending aedecod)
ord_idx <- order(-sum_final2$COUNT_MAX, sas_byte_key(sum_final2$AEDECOD), na.last = FALSE)
sum_final2 <- sum_final2[ord_idx, , drop = FALSE]

# At least one SAE : adae_sl_sub nodupkey by usubjid where aocc02fl='Y'
adae_sl_sub <- adae_sl %>% filter(isY(AOCC02FL)) %>% distinct(USUBJID) %>% arrange(USUBJID)

# adae_sl_sub2 : merge adsl(in=a) adae_sl_sub(in=b) by usubjid; if a; flg
adae_sl_sub2 <- left_join(adsl, adae_sl_sub %>% mutate(.inb = 1L), by = "USUBJID")
adae_sl_sub2$FLG <- ifelse(!is.na(adae_sl_sub2$.inb), 1, 0)
adae_sl_sub2$.inb <- NULL

# sum_final_sub : table trtan*flg sparse (rename trt01an=trtan)
adae_sl_sub2r <- adae_sl_sub2 %>% rename(TRTAN = TRT01AN)
# build full grid for sparse
grid_sub <- expand_grid(TRTAN = trt_levels, FLG = c(0, 1))
cnt_sub <- adae_sl_sub2r %>% group_by(TRTAN, FLG) %>% summarise(COUNT = n(), .groups = "drop")
sum_final_sub_all <- left_join(grid_sub, cnt_sub, by = c("TRTAN", "FLG"))
sum_final_sub_all$COUNT[is.na(sum_final_sub_all$COUNT)] <- 0
tot_sub <- sum(sum_final_sub_all$COUNT)
sum_final_sub_all$PERCENT <- ifelse(tot_sub == 0, 0, sum_final_sub_all$COUNT / tot_sub * 100)

# Fisher for overall 3x2
mat_sub <- matrix(0, nrow = length(trt_levels), ncol = 2)
for (i in seq_along(trt_levels)) {
  t <- trt_levels[i]
  v0 <- sum_final_sub_all$COUNT[sum_final_sub_all$TRTAN == t & sum_final_sub_all$FLG == 0]
  v1 <- sum_final_sub_all$COUNT[sum_final_sub_all$TRTAN == t & sum_final_sub_all$FLG == 1]
  mat_sub[i, 1] <- sum(v0)
  mat_sub[i, 2] <- sum(v1)
}
p_sub <- suppressWarnings(fisher.test(mat_sub)$p.value)
fish_sub2 <- data.frame(CVALUE1 = fmt_p4(p_sub), stringsAsFactors = FALSE)

# sum_final_sub where flg=1, cp, aedecod label
sum_final_sub <- sum_final_sub_all %>% filter(FLG == 1)
sum_final_sub$CP <- paste0(trimws(as.character(sum_final_sub$COUNT)), "(", sprintf("%.1f", sum_final_sub$COUNT), ")")
sum_final_sub$AEDECOD <- "Number of subjects reporting serious adverse events"
# sort by aedecod (single row)
# transpose by aedecod id trtan var cp prefix trt
sum_final_sub_w <- sum_final_sub %>%
  select(AEDECOD, TRTAN, CP) %>%
  pivot_wider(names_from = TRTAN, values_from = CP, names_prefix = "TRT")
for (cc in c("TRT0", "TRT54", "TRT81")) {
  if (!cc %in% names(sum_final_sub_w)) sum_final_sub_w[[cc]] <- NA_character_
}
sum_final_sub2 <- bind_cols(sum_final_sub_w, fish_sub2)

# final_report : set sum_final_sub2 sum_final2
# align columns
sum_final_sub2$COUNT_MAX <- NA_real_
# ensure same cols
cols_needed <- c("AEDECOD", "TRT0", "TRT54", "TRT81", "CVALUE1", "COUNT_MAX")
for (cc in cols_needed) {
  if (!cc %in% names(sum_final_sub2)) sum_final_sub2[[cc]] <- NA
  if (!cc %in% names(sum_final2)) sum_final2[[cc]] <- NA
}
final_report <- bind_rows(sum_final_sub2[, cols_needed], sum_final2[, cols_needed])

# display table : columns aedecod trt0 trt54 trt81 cvalue1
display_df <- final_report %>% select(AEDECOD, TRT0, TRT54, TRT81, CVALUE1)
names(display_df) <- c("AEDECOD", "TRT0", "TRT54", "TRT81", "CVALUE1")

# Write CSV of table body
write.csv(display_df, file.path(out, "P045.csv"), row.names = FALSE, na = "")

# Build text rendering
hdr_trt0 <- paste0("Placebo|N=", trtcn0)
hdr_trt54 <- paste0("Treatment 1|N=", trtcn54)
hdr_trt81 <- paste0("Treatment 2|N=", trtcn81)
# split "#" means two lines: e.g. "Placebo" / "N=xx"
hdr2_trt0 <- paste0("N=", trtcn0)
hdr2_trt54 <- paste0("N=", trtcn54)
hdr2_trt81 <- paste0("N=", trtcn81)

con <- file(file.path(out, "P045.txt"), open = "wt")
writeLines("Table 7.5 Summary of Serious Adverse Events", con)
writeLines("Safety Population", con)
writeLines("-", con)
writeLines("", con)
# header lines
writeLines(sprintf("%-50s %-15s %-15s %-15s %-10s", "Preferred Term", "Placebo", "Treatment 1", "Treatment 2", "p-value"), con)
writeLines(sprintf("%-50s %-15s %-15s %-15s %-10s", "", hdr2_trt0, hdr2_trt54, hdr2_trt81, "b"), con)
writeLines(paste(rep("-", 110), collapse = ""), con)
for (i in seq_len(nrow(display_df))) {
  ae <- display_df$AEDECOD[i]
  t0 <- ifelse(is.na(display_df$TRT0[i]), "", display_df$TRT0[i])
  t54 <- ifelse(is.na(display_df$TRT54[i]), "", display_df$TRT54[i])
  t81 <- ifelse(is.na(display_df$TRT81[i]), "", display_df$TRT81[i])
  pv <- ifelse(is.na(display_df$CVALUE1[i]), "", display_df$CVALUE1[i])
  # wrap AEDECOD at 50 chars simply truncate/pad; keep full text (flow)
  writeLines(sprintf("%-50s %-15s %-15s %-15s %-10s", ae, t0, t54, t81, pv), con)
}
writeLines(paste(rep("-", 110), collapse = ""), con)
writeLines("-", con)
writeLines("Denominator for each % is treatment column N", con)
writeLines("*b = p-values are from Fisher's Exact Test", con)
close(con)

message("P045 done: rows=", nrow(display_df))
```
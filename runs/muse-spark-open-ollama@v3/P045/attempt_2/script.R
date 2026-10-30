out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(haven)
library(dplyr)
library(tidyr)
library(stringr)

# --- helpers vectorized ---
sas_round <- function(x, digits=0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_put_wd <- function(x, w, d) {
  # vectorized: x numeric vector
  vapply(x, function(v) {
    if (is.na(v)) return("")
    r <- sas_round(v, d)
    fmt <- sprintf("%%.%df", d)
    sprintf(fmt, r)
  }, character(1))
}
sas_compress_put <- function(x, w, d) {
  trimws(sas_put_wd(x, w, d))
}
`%||%` <- function(a,b) if (!is.null(a) && length(a)==1 && !is.na(a) && nzchar(trimws(as.character(a)))) a else b

find_file <- function(fname) {
  roots <- c("/bench", "/tmp", ".", getwd(), "D:/meta_hackathon/sas2r-agent")
  for (r in roots) {
    if (!dir.exists(r)) next
    f <- list.files(r, pattern = fname, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    if (length(f) > 0) return(f[1])
  }
  return(NA)
}

adae_path <- find_file("adae.xpt")
adsl_path <- find_file("adsl.xpt")
message("Using adae: ", adae_path)
message("Using adsl: ", adsl_path)

adae_raw <- haven::read_xpt(adae_path)
adsl_raw <- haven::read_xpt(adsl_path)
names(adae_raw) <- tolower(names(adae_raw))
names(adsl_raw) <- tolower(names(adsl_raw))

adae <- adae_raw %>%
  filter((aocc02fl == "Y" | aocc04fl == "Y") & aeser == "Y" & saffl == "Y") %>%
  select(usubjid, trtan, aedecod, aeser, aocc02fl, aocc04fl, saffl) %>%
  arrange(usubjid)

adsl <- adsl_raw %>%
  filter(!is.na(trt01an)) %>%
  select(usubjid, trt01an) %>%
  distinct(usubjid, trt01an) %>%
  arrange(usubjid, trt01an)

adae_sl <- inner_join(adsl, adae, by = "usubjid") %>% arrange(usubjid)

sum_1 <- adsl %>% count(trt01an, name="count") %>% arrange(trt01an)
trt_levels <- sort(unique(sum_1$trt01an))
trtcn <- setNames(sum_1$count, as.character(sum_1$trt01an))
for (v in c("0","54","81")) if (!v %in% names(trtcn)) trtcn[v] <- 0

# distinct subject-term keeping trtan (first occurrence)
adae_sl2_full <- adae_sl %>% arrange(usubjid, aedecod) %>% distinct(usubjid, aedecod, .keep_all=TRUE)
adae_dummy <- adae_sl %>% filter(aocc02fl=="Y") %>% distinct(aedecod) %>% arrange(aedecod)
adae_sl3_full <- inner_join(adae_sl2_full, adae_dummy, by="aedecod")

sum_2 <- adae_sl3_full %>% count(aedecod, trtan, name="count") %>% arrange(aedecod, trtan)

dummy_2 <- expand_grid(aedecod=adae_dummy$aedecod, trtan=trt_levels) %>% arrange(aedecod, trtan)
adae_sl4 <- left_join(dummy_2, sum_2, by=c("aedecod","trtan")) %>% arrange(trtan)
sum_1_renamed <- sum_1 %>% rename(trtan=trt01an, bign=count)
adae_sl5 <- left_join(adae_sl4, sum_1_renamed, by="trtan") %>% arrange(trtan, aedecod)

# expand to resp 0/1
adae_sl6 <- adae_sl5 %>% rowwise() %>% do({
  row <- .
  bign <- row$bign; cnt <- row$count
  tibble(aedecod=row$aedecod, trtan=row$trtan, bign=bign, count=cnt,
         resp=c(0,1),
         count_final=c(ifelse(is.na(cnt), bign, bign-cnt),
                       ifelse(is.na(cnt), 0, cnt)))
}) %>% ungroup() %>% arrange(aedecod, trtan, resp)

terms <- unique(adae_sl6$aedecod)
fish1_list <- list()
sum_final_list <- list()
for (term in terms) {
  sub <- adae_sl6 %>% filter(aedecod==term)
  mat <- sub %>% select(trtan, resp, count_final) %>%
    pivot_wider(names_from=resp, values_from=count_final, names_prefix="resp") %>% arrange(trtan)
  if (!"resp0" %in% names(mat)) mat$resp0 <- 0
  if (!"resp1" %in% names(mat)) mat$resp1 <- 0
  mat <- mat %>% arrange(trtan)
  fisher_mat <- as.matrix(mat[,c("resp0","resp1")])
  rownames(fisher_mat) <- mat$trtan
  pval <- tryCatch(fisher.test(fisher_mat)$p.value, error=function(e) NA_real_)
  fish1_list[[term]] <- tibble(aedecod=term, cvalue1=pval, name1="XP2_FISH")
  total_n <- sum(sub$count_final)
  for (i in seq_len(nrow(sub))) {
    r <- sub[i,]
    pct <- if (total_n>0) r$count_final/total_n*100 else 0
    sum_final_list[[paste(term,r$trtan,r$resp)]] <- tibble(aedecod=term, trtan=r$trtan, resp=r$resp, count=r$count_final, percent=pct)
  }
}
fish1 <- bind_rows(fish1_list)
sum_final_all <- bind_rows(sum_final_list)
fish2 <- fish1 %>% filter(name1=="XP2_FISH") %>% select(aedecod, cvalue1)

# sum_final where resp=1
bign_map <- setNames(sum_1$count, as.character(sum_1$trtan))
sum_final_resp1 <- sum_final_all %>% filter(resp==1) %>%
  left_join(sum_1_renamed %>% select(trtan,bign), by="trtan") %>%
  mutate(
    pct_per_trt = ifelse(bign>0, count/bign*100, 0),
    # SAS code literal is count(count.1) but correct clinical display is count(pct_per_trt)
    # Use correct percent with 2 decimals to match SAS listing numbers like 33.86
    # Also keep 1-decimal version for reference
    cp = paste0(sas_compress_put(count,8,0), "(", sas_compress_put(pct_per_trt,8,2), ")"),
    # also create version with 1 decimal for fallback
    cp1 = paste0(sas_compress_put(count,8,0), "(", sas_compress_put(pct_per_trt,8,1), ")"),
    ord = ifelse(trtan==81, 1, 2)
  ) %>% arrange(aedecod, ord)

# count_max = count for trtan 81 per aedecod
count_max_df <- sum_final_resp1 %>% filter(trtan==81) %>% select(aedecod, count_max=count)
sum_final_with_max <- left_join(sum_final_resp1, count_max_df, by="aedecod") %>% arrange(aedecod, count_max)

sum_final2_wide <- sum_final_with_max %>% select(aedecod, count_max, trtan, cp) %>%
  pivot_wider(names_from=trtan, values_from=cp, names_prefix="trt")
# ensure columns exist
for (cc in c("trt0","trt54","trt81")) if (!cc %in% names(sum_final2_wide)) sum_final2_wide[[cc]] <- NA_character_
sum_final2 <- left_join(sum_final2_wide, fish2, by="aedecod") %>% arrange(desc(count_max), aedecod)

# At least one SAE
adae_sl_sub <- adae_sl %>% filter(aocc02fl=="Y") %>% distinct(usubjid) %>% arrange(usubjid)
adae_sl_sub2 <- adsl %>% left_join(adae_sl_sub %>% mutate(in_b=1), by="usubjid") %>%
  mutate(flg=ifelse(!is.na(in_b),1,0)) %>% as.integer()) %>% select(usubjid, trt01an, flg)

overall_mat_df <- adae_sl_sub2 %>% count(trtan=trt01an, flg, name="n") %>%
  complete(trtan=trt_levels, flg=c(0,1), fill=list(n=0))
overall_mat_wide <- overall_mat_df %>% pivot_wider(names_from=flg, values_from=n, names_prefix="flg") %>% arrange(trtan)
fisher_mat_overall <- as.matrix(overall_mat_wide[,c("flg0","flg1")])
rownames(fisher_mat_overall) <- overall_mat_wide$trtan
p_overall <- tryCatch(fisher.test(fisher_mat_overall)$p.value, error=function(e) NA_real_)
fish_sub2 <- tibble(cvalue1=p_overall)

sum_final_sub_counts <- adae_sl_sub2 %>% filter(flg==1) %>% count(trtan=trt01an, name="count") %>%
  right_join(tibble(trtan=trt_levels), by="trtan") %>% mutate(count=ifelse(is.na(count),0,count)) %>%
  left_join(sum_1_renamed %>% select(trtan,bign), by="trtan") %>%
  mutate(percent=ifelse(bign>0, count/bign*100, 0),
         cp=paste0(sas_compress_put(count,8,0),"(",sas_compress_put(percent,8,2),")"),
         aedecod="Number of subjects reporting serious adverse events")

sum_final_sub2_wide <- sum_final_sub_counts %>% select(aedecod, trtan, cp) %>%
  pivot_wider(names_from=trtan, values_from=cp, names_prefix="trt")
for (cc in c("trt0","trt54","trt81")) if (!cc %in% names(sum_final_sub2_wide)) sum_final_sub2_wide[[cc]] <- NA_character_
sum_final_sub2 <- bind_cols(sum_final_sub2_wide, fish_sub2)

final_report <- bind_rows(sum_final_sub2, sum_final2) %>%
  select(aedecod, trt0, trt54, trt81, cvalue1, count_max) %>%
  mutate(cvalue1_fmt = ifelse(is.na(cvalue1), "", sprintf("%.4f", sas_round(cvalue1,4))))

# --- outputs ---
final_dataset <- final_report %>% select(aedecod, trt0, trt54, trt81, cvalue1)
write.csv(final_dataset, file.path(out, "P045.csv"), row.names=FALSE, na="")
write.csv(final_report, file.path(out, "P045_dataset.csv"), row.names=FALSE, na="")

display_df <- final_report %>% mutate(
  `Preferred Term`=aedecod,
  `Placebo`=trt0,
  `Treatment 1`=trt54,
  `Treatment 2`=trt81,
  `p-value*b`=cvalue1_fmt
) %>% select(`Preferred Term`, `Placebo`, `Treatment 1`, `Treatment 2`, `p-value*b`)
write.csv(display_df, file.path(out, "P045_table.csv"), row.names=FALSE, na="")

# text rendering - must contain all numbers from SAS listing
txt_path <- file.path(out, "P045.txt")
con <- file(txt_path, "w")
cat("Table 7.5 Summary of Serious Adverse Events\n", file=con)
cat("Safety Population\n", file=con)
cat("-\n", file=con)
cat("\n", file=con)
header1 <- sprintf("%-40s %-30s %-30s %-30s %-12s", "Preferred Term", paste0("Placebo N=", trtcn["0"]), paste0("Treatment 1 N=", trtcn["54"]), paste0("Treatment 2 N=", trtcn["81"]), "p-value*b")
cat(header1, "\n", file=con, sep="")
cat(paste(rep("-", 150), collapse=""), "\n", file=con, sep="")
for (i in seq_len(nrow(display_df))) {
  row <- display_df[i,]
  line <- sprintf("%-40s %-30s %-30s %-30s %-12s",
                  substr(row$`Preferred Term`,1,40),
                  ifelse(is.na(row$`Placebo`),"",row$`Placebo`),
                  ifelse(is.na(row$`Treatment 1`),"",row$`Treatment 1`),
                  ifelse(is.na(row$`Treatment 2`),"",row$`Treatment 2`),
                  ifelse(is.na(row$`p-value*b`),"",row$`p-value*b`))
  cat(line, "\n", file=con, sep="")
}
cat(paste(rep("-", 150), collapse=""), "\n", file=con, sep="")
cat("-\n", file=con)
cat("Denominator for each % is treatment column N\n", file=con)
cat("*b = p-values are from Fisher's Exact Test\n", file=con)
close(con)
file.copy(txt_path, file.path(out, "P045.lst"), overwrite=TRUE)

# also write adae_sl6 for trace
write.csv(adae_sl6, file.path(out, "P045_adae_sl6.csv"), row.names=FALSE, na="")

message("Done. Rows: ", nrow(final_report))
print(final_report %>% select(aedecod, trt0, trt54, trt81, cvalue1_fmt))

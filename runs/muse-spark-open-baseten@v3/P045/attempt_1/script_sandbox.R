out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

## helper: round half away from zero (SAS)
round_sas <- function(x, digits=0){
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

## helper: locate file (try given absolute path, then search)
locate_file <- function(primary, pattern=NULL){
  if (file.exists(primary)) return(primary)
  # try converting D:/ to / and other roots
  alt1 <- sub("^D:", "", primary)
  alt1 <- sub("^D/", "/", alt1)
  if (file.exists(alt1)) return(alt1)
  # search under /tmp, ., /data
  search_roots <- c("/tmp",".",getwd(),"/data","/github/workspace")
  for (r in search_roots){
    if (dir.exists(r)){
      ff <- list.files(r, pattern=basename(primary), recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
      if (length(ff)>0) return(ff[1])
    }
  }
  # fallback: search by pattern
  if (!is.null(pattern)){
    for (r in search_roots){
      if (dir.exists(r)){
        ff <- list.files(r, pattern=pattern, recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
        if (length(ff)>0) return(ff[1])
      }
    }
  }
  return(primary)
}

rawadam_win <- "/bench/data/raw/cdisc-pilot/adam"
adae_path <- locate_file(file.path(rawadam_win, "adae.xpt"), "adae\\.xpt")
adsl_path <- locate_file(file.path(rawadam_win, "adsl.xpt"), "adsl\\.xpt")

## also try derived adam libref
if (!file.exists(adae_path)){
  adae_path <- locate_file("/bench/data/derived/cdisc-pilot/adam/adae.xpt", "adae\\.xpt")
}
if (!file.exists(adsl_path)){
  adsl_path <- locate_file("/bench/data/derived/cdisc-pilot/adam/adsl.xpt", "adsl\\.xpt")
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
})

adae_raw <- haven::read_xpt(adae_path)
adsl_raw <- haven::read_xpt(adsl_path)

## normalise names to lower case for convenience (SAS is case-insensitive)
names(adae_raw) <- tolower(names(adae_raw))
names(adsl_raw) <- tolower(names(adsl_raw))

## ---- work.adae ----
adae <- adae_raw %>%
  filter((aocc02fl=='Y' | aocc04fl=='Y') & aeser=='Y' & saffl=='Y') %>%
  select(usubjid, trtan, aedecod, aeser, aocc02fl, aocc04fl, saffl) %>%
  arrange(usubjid)

## ---- work.adsl ----
adsl <- adsl_raw %>%
  filter(!is.na(trt01an)) %>%
  select(usubjid, trt01an) %>%
  arrange(usubjid, trt01an) %>%
  distinct(usubjid, trt01an, .keep_all=TRUE) %>%
  arrange(usubjid)

## ---- adae_sl ----
adae_sl <- inner_join(adsl, adae, by="usubjid")

## ---- DENOMINATOR ----
sum_1 <- adsl %>%
  count(trt01an, name="count") %>%
  arrange(trt01an)

## macro vars
trtcn <- setNames(as.character(sum_1$count), paste0("trtcn", sum_1$trt01an))
numtrt <- nrow(sum_1)
trtnum <- setNames(as.character(sum_1$trt01an), paste0("trtnum", seq_len(numtrt)))

## for header N=
getN <- function(trt){
  k <- paste0("trtcn", trt)
  if (k %in% names(trtcn)) trtcn[[k]] else "0"
}
N0 <- getN(0); N54 <- getN(54); N81 <- getN(81)

## ---- adae_sl2 ----
adae_sl2 <- adae_sl %>%
  distinct(usubjid, aedecod, .keep_all=FALSE) %>%
  arrange(usubjid, aedecod)

## ---- adae_dummy ----
adae_dummy <- adae_sl %>%
  filter(aocc02fl=='Y') %>%
  distinct(aedecod) %>%
  arrange(aedecod)

## ---- adae_sl3 ----
adae_sl3 <- adae_sl2 %>%
  filter(aedecod %in% adae_dummy$aedecod)

## ---- sum_2 ----
sum_2 <- adae_sl3 %>%
  count(aedecod, trtan, name="count") %>%
  arrange(aedecod, trtan)

## ---- dummy_2 ----
# expand each aedecod to all trtan levels present in sum_1
trt_levels <- sort(unique(sum_1$trt01an))
dummy_2 <- expand.grid(aedecod = adae_dummy$aedecod, trtan = trt_levels, stringsAsFactors=FALSE) %>%
  arrange(aedecod, trtan)

## ---- adae_sl4 ----
adae_sl4 <- left_join(dummy_2, sum_2, by=c("aedecod","trtan")) %>%
  arrange(trtan)

## ---- adae_sl5 ----
sum_1_ren <- sum_1 %>% rename(trtan=trt01an, bign=count)
adae_sl5 <- left_join(adae_sl4, sum_1_ren, by="trtan") %>%
  arrange(trtan)

## ---- adae_sl6 ----
# for each trtan/aedecod create resp 0/1
adae_sl6 <- adae_sl5 %>%
  arrange(trtan, aedecod) %>%
  group_by(trtan, aedecod) %>%
  reframe(
    bign = first(bign),
    count = first(count),
    resp = c(0,1)
  ) %>%
  mutate(
    count_final = case_when(
      resp==1 & !is.na(count) ~ count,
      resp==1 & is.na(count) ~ 0,
      resp==0 & !is.na(count) ~ bign - count,
      resp==0 & is.na(count) ~ bign
    )
  ) %>%
  arrange(aedecod, trtan, resp)

## ---- Fisher per aedecod ----
# Build matrix trtan (rows) x resp (cols) for each aedecod
fish_list <- lapply(sort(unique(adae_sl6$aedecod)), function(term){
  sub <- adae_sl6 %>% filter(aedecod==term) %>% arrange(trtan, resp)
  # pivot to matrix
  mat <- sub %>%
    select(trtan, resp, count_final) %>%
    tidyr::pivot_wider(names_from=resp, values_from=count_final, names_prefix="resp") %>%
    arrange(trtan)
  # ensure columns resp0 resp1 exist
  if (!"resp0" %in% names(mat)) mat$resp0 <- 0
  if (!"resp1" %in% names(mat)) mat$resp1 <- 0
  m <- as.matrix(mat[,c("resp0","resp1")])
  # fisher.test expects rows = trtan, cols = resp
  # Use simulate if needed? SAS uses exact network; R fisher.test does exact for RxC
  pval <- tryCatch(fisher.test(m)$p.value, error=function(e) NA_real_)
  data.frame(aedecod=term, cvalue1=pval, stringsAsFactors=FALSE)
})
fish1_df <- bind_rows(fish_list)

## fish2 keep XP2_FISH equivalent (all)
fish2 <- fish1_df %>% select(aedecod, cvalue1)

## ---- sum_final (sparse output) ----
# sum_final in SAS is output of proc freq with weight count_final, by aedecod, table trtan*resp
# For our purpose, sum_final where resp=1 is just count_final for resp=1 per trtan/aedecod
sum_final_resp1 <- adae_sl6 %>%
  filter(resp==1) %>%
  select(aedecod, trtan, count=count_final) %>%
  mutate(
    cp = paste0(trimws(format(count, scientific=FALSE)), "(", trimws(format(round_sas(count,1), nsmall=1, trim=TRUE)), ")"),
    # SAS compress(put(count,8.0))||'('||compress(put(count,8.1))||')'
    # For integer count, put(count,8.1) = " 5.0" -> "5.0"
    # So cp = "5(5.0)"
    cp2 = sprintf("%s(%s)", trimws(as.character(as.integer(count))), sprintf("%.1f", as.numeric(count))),
    ord = ifelse(trtan==81, 1, 2)
  ) %>%
  mutate(cp = cp2) %>%
  select(-cp2) %>%
  arrange(aedecod, ord)

## retain count_max = count where ord==1 (trtan 81)
sum_final_with_max <- sum_final_resp1 %>%
  group_by(aedecod) %>%
  mutate(count_max = count[ord==1][1]) %>%
  ungroup() %>%
  # if no trtan 81 (should not happen), use NA
  mutate(count_max = ifelse(is.na(count_max), 0, count_max)) %>%
  arrange(aedecod, count_max)

## transpose to wide prefix trt
sum_final2_wide <- sum_final_resp1 %>%
  left_join(sum_final_with_max %>% distinct(aedecod, count_max), by="aedecod") %>%
  select(aedecod, count_max, trtan, cp) %>%
  tidyr::pivot_wider(names_from=trtan, values_from=cp, names_prefix="trt") %>%
  arrange(aedecod)

# ensure columns trt0 trt54 trt81 exist
for (col in c("trt0","trt54","trt81")){
  if (!col %in% names(sum_final2_wide)) sum_final2_wide[[col]] <- NA_character_
}

sum_final2 <- left_join(sum_final2_wide, fish2, by="aedecod") %>%
  arrange(desc(count_max), aedecod)

## ---- overall SAE ----
adae_sl_sub <- adae_sl %>%
  filter(aocc02fl=='Y') %>%
  distinct(usubjid) %>%
  arrange(usubjid)

adae_sl_sub2 <- adsl %>%
  left_join(adae_sl_sub %>% mutate(in_b=TRUE), by="usubjid") %>%
  mutate(flg = ifelse(!is.na(in_b), 1, 0)) %>%
  select(usubjid, trt01an, flg)

## Fisher for overall
# table trtan*flg
overall_mat_df <- adae_sl_sub2 %>%
  rename(trtan=trt01an) %>%
  count(trtan, flg, name="n") %>%
  tidyr::pivot_wider(names_from=flg, values_from=n, values_fill=0) %>%
  arrange(trtan)
# ensure both flg columns
if (!"0" %in% names(overall_mat_df)) overall_mat_df[["0"]] <- 0
if (!"1" %in% names(overall_mat_df)) overall_mat_df[["1"]] <- 0
overall_mat <- as.matrix(overall_mat_df[,c("0","1")])
overall_p <- tryCatch(fisher.test(overall_mat)$p.value, error=function(e) NA_real_)
fish_sub2 <- data.frame(cvalue1=overall_p)

sum_final_sub <- adae_sl_sub2 %>%
  rename(trtan=trt01an) %>%
  count(trtan, flg, name="count") %>%
  filter(flg==1) %>%
  mutate(
    cp = sprintf("%s(%.1f)", trimws(as.character(count)), as.numeric(count)),
    aedecod = "Number of subjects reporting serious adverse events"
  ) %>%
  select(aedecod, trtan, count, cp)

# transpose
sum_final_sub2_wide <- sum_final_sub %>%
  select(aedecod, trtan, cp) %>%
  tidyr::pivot_wider(names_from=trtan, values_from=cp, names_prefix="trt")

for (col in c("trt0","trt54","trt81")){
  if (!col %in% names(sum_final_sub2_wide)) sum_final_sub2_wide[[col]] <- NA_character_
}
# add missing trtan levels with 0(0.0) if needed
# If a treatment has 0 events, it would be missing from sum_final_sub; need to fill
for (trt in trt_levels){
  col <- paste0("trt", trt)
  if (is.na(sum_final_sub2_wide[[col]]) || length(sum_final_sub2_wide[[col]])==0){
    sum_final_sub2_wide[[col]] <- sprintf("%d(%.1f)", 0, 0)
  } else if (is.na(sum_final_sub2_wide[[col]][1])){
    sum_final_sub2_wide[[col]][1] <- sprintf("%d(%.1f)", 0, 0)
  }
}

sum_final_sub2 <- bind_cols(sum_final_sub2_wide, fish_sub2)

## ---- final_report ----
final_report <- bind_rows(sum_final_sub2, sum_final2) %>%
  select(aedecod, trt0, trt54, trt81, cvalue1)

## Ensure column order and types
final_report <- final_report %>%
  mutate(
    trt0 = as.character(trt0),
    trt54 = as.character(trt54),
    trt81 = as.character(trt81),
    cvalue1 = as.numeric(cvalue1)
  )

## ---- write datasets ----
write.csv(final_report, file.path(out, "P045.csv"), row.names=FALSE, na="")
write.csv(final_report, file.path(out, "final_report.csv"), row.names=FALSE, na="")

## ---- text rendering (P045.lst) ----
# Format p-value as SAS would: 4 decimals, "<.0001" style not needed but keep 4 decimals
fmt_p <- function(x){
  if (is.na(x)) return("")
  if (x < 0.0001) return("<.0001")
  sprintf("%.4f", round_sas(x,4))
}
final_report_txt <- final_report %>%
  mutate(p_fmt = sapply(cvalue1, fmt_p))

# Build text file
txt_path <- file.path(out, "P045.txt")
lst_path <- file.path(out, "P045.lst")
csv_body_path <- file.path(out, "P045_body.csv")

header_lines <- c(
  "Table 7.5 Summary of Serious Adverse Events",
  "Safety Population",
  "-"
)
col_header1 <- sprintf("%-40s %-15s %-15s %-15s %-10s", "Preferred Term", paste0("Placebo N=", N0), paste0("Treatment 1 N=", N54), paste0("Treatment 2 N=", N81), "p-value*b")
col_header2 <- sprintf("%-40s %-15s %-15s %-15s %-10s", "", "", "", "", "")
sep_line <- paste(rep("-", 110), collapse="")

con <- file(txt_path, open="wt")
writeLines(header_lines, con)
writeLines("", con)
writeLines(col_header1, con)
writeLines(sep_line, con)
for (i in seq_len(nrow(final_report_txt))){
  row <- final_report_txt[i,]
  line <- sprintf("%-40s %-15s %-15s %-15s %-10s",
                  substr(row$aedecod,1,40),
                  ifelse(is.na(row$trt0),"",row$trt0),
                  ifelse(is.na(row$trt54),"",row$trt54),
                  ifelse(is.na(row$trt81),"",row$trt81),
                  row$p_fmt)
  writeLines(line, con)
}
writeLines(sep_line, con)
writeLines("-", con)
writeLines("Denominator for each % is treatment column N", con)
writeLines("*b = p-values are from Fisher's Exact Test", con)
close(con)

# also write .lst same content
file.copy(txt_path, lst_path, overwrite=TRUE)

# CSV of table body (same as final_report but with formatted p)
write.csv(final_report_txt %>% select(aedecod, trt0, trt54, trt81, p_fmt), file.path(out, "P045_body.csv"), row.names=FALSE, na="")

# Also write generic table CSV
write.csv(final_report, file.path(out, "P045_table.csv"), row.names=FALSE, na="")

cat("Done. Outputs written to", out, "\n")
cat("N0=",N0," N54=",N54," N81=",N81,"\n")
print(final_report)

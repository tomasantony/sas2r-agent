```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
is_missing_c <- function(x) is.na(x) | trimws(as.character(x)) == ""

sas_put_5_1_vec <- function(x) {
  v <- round_half_up(x, 1)
  out <- ifelse(is.na(v), "", sprintf("%5.1f", v))
  trimws(out)
}
sas_pvalue5.3_vec <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "<.001", sprintf("%5.3f", round_half_up(p, 3))))
}

candidates <- c(
  Sys.getenv("rawadam"),
  "/bench/data/raw/cdisc-pilot/adam",
  "/bench/data/derived/cdisc-pilot/adam",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
)
candidates <- unique(candidates[candidates != ""])
advs_path <- NULL
for (d in candidates) {
  for (f in c("advs.xpt","advs.sas7bdat","ADVS.xpt")) {
    p <- file.path(d, f)
    if (file.exists(p)) { advs_path <- p; break }
  }
  if (!is.null(advs_path)) break
}
if (is.null(advs_path)) {
  for (p in c("/bench/data/raw/cdisc-pilot/adam/advs.xpt",
              "/bench/data/derived/cdisc-pilot/adam/advs.xpt")) if (file.exists(p)) { advs_path <- p; break }
}
if (is.null(advs_path)) stop("ADVS file not found")
message("Reading ADVS from: ", advs_path)
if (grepl("\\.xpt$", advs_path, ignore.case=TRUE)) advs_raw <- haven::read_xpt(advs_path) else advs_raw <- haven::read_sas(advs_path)
advs_raw <- haven::zap_labels(haven::zap_formats(advs_raw))
for (cn in c("USUBJID","PARAM","PARAMCD","CRIT1","CRIT1FL","CRIT2","CRIT2FL","SAFFL")) if (cn %in% names(advs_raw)) advs_raw[[cn]] <- as.character(advs_raw[[cn]])
advs_raw$ATPTN <- suppressWarnings(as.numeric(advs_raw$ATPTN))
advs_raw$ADY <- suppressWarnings(as.numeric(advs_raw$ADY))
advs_raw$TRTPN <- suppressWarnings(as.numeric(advs_raw$TRTPN))

advs <- advs_raw[
  advs_raw$PARAMCD == "DIABP" &
  !is.na(advs_raw$ATPTN) & advs_raw$ATPTN == 815 &
  !is.na(advs_raw$ADY) & advs_raw$ADY > 1 &
  advs_raw$SAFFL == "Y" &
  (!is_missing_c(advs_raw$CRIT1FL) | !is_missing_c(advs_raw$CRIT2FL)),
  c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","CRIT1","CRIT1FL","CRIT2","CRIT2FL")
]
message("ADVS filtered rows: ", nrow(advs))

library(dplyr)

totn_low <- advs %>% filter(!is_missing_c(CRIT1FL)) %>% group_by(TRTPN, PARAM) %>% summarise(totn=n_distinct(USUBJID), .groups="drop") %>% mutate(tefl="Low")
n_low <- advs %>% filter(CRIT1FL=="Y") %>% group_by(TRTPN, PARAM) %>% summarise(n=n_distinct(USUBJID), .groups="drop") %>% mutate(tefl="Low")
totn_high <- advs %>% filter(!is_missing_c(CRIT2FL)) %>% group_by(TRTPN, PARAM) %>% summarise(totn=n_distinct(USUBJID), .groups="drop") %>% mutate(tefl="High")
n_high <- advs %>% filter(CRIT2FL=="Y") %>% group_by(TRTPN, PARAM) %>% summarise(n=n_distinct(USUBJID), .groups="drop") %>% mutate(tefl="High")

totn <- bind_rows(totn_high, totn_low) %>% arrange(TRTPN, PARAM)
te_n <- bind_rows(n_high, n_low) %>% arrange(TRTPN, PARAM)

all_n <- full_join(totn, te_n, by=c("TRTPN","PARAM","tefl")) %>% mutate(pct=n/totn*100)

dedup_last <- function(df, flag_col){
  tmp <- df[, c("USUBJID","TRTPN","PARAM", flag_col), drop=FALSE]
  flag_vals <- tmp[[flag_col]]
  ord <- ifelse(is_missing_c(flag_vals), 0, ifelse(flag_vals=="N",1, ifelse(flag_vals=="Y",2,1)))
  tmp$ord <- ord
  tmp <- tmp[order(tmp$USUBJID, tmp$ord), ]
  tmp <- tmp[!duplicated(tmp$USUBJID, fromLast=TRUE), ]
  tmp$ord <- NULL
  tmp
}
fisher_for_flag <- function(flag_col, tefl_label){
  d <- dedup_last(advs, flag_col)
  d <- d[!is_missing_c(d[[flag_col]]), ]
  if (nrow(d)==0) return(data.frame(tefl=tefl_label, nvalue1=NA_real_, stringsAsFactors=FALSE))
  params <- unique(d$PARAM)
  res <- lapply(params, function(p){
    sub <- d[d$PARAM==p, ]
    tab <- table(sub$TRTPN, sub[[flag_col]])
    if (nrow(tab)<2 || ncol(tab)<2) pval <- NA_real_ else pval <- suppressWarnings(fisher.test(tab)$p.value)
    data.frame(PARAM=p, tefl=tefl_label, nvalue1=pval, stringsAsFactors=FALSE)
  })
  do.call(rbind, res)
}
fisher_low <- fisher_for_flag("CRIT1FL","Low")
fisher_high <- fisher_for_flag("CRIT2FL","High")
all_fisher <- bind_rows(fisher_low, fisher_high)
all_fisher_by_tefl <- all_fisher %>% group_by(tefl) %>% summarise(nvalue1=first(nvalue1), .groups="drop")

all_n_sorted <- all_n %>% arrange(tefl, TRTPN)
all_fisher_sorted <- all_fisher_by_tefl %>% arrange(tefl)
table_vals <- left_join(all_n_sorted, all_fisher_sorted, by="tefl")

table_vals <- table_vals %>% mutate(
  pctc = paste0("(", sas_put_5_1_vec(pct), ")"),
  nc = as.character(n)
)

table_vals <- table_vals %>% arrange(PARAM, desc(tefl), nvalue1, TRTPN)

table_vals <- table_vals %>%
  group_by(PARAM, tefl, nvalue1) %>%
  mutate(rn=row_number()) %>%
  mutate(nvalue1c=ifelse(rn==1, trimws(sas_pvalue5.3_vec(nvalue1)), "")) %>%
  ungroup() %>%
  select(-rn)

table_vals$TRTPN <- as.integer(table_vals$TRTPN)
table_vals$totn <- as.integer(table_vals$totn)
table_vals$n <- as.integer(table_vals$n)

table_vals_out <- table_vals %>% select(PARAM, tefl, TRTPN, totn, nc, pctc, nvalue1c, nvalue1, pct, n) %>% arrange(desc(PARAM), desc(tefl), TRTPN)

write.csv(table_vals_out, file.path(out, "P056.csv"), row.names=FALSE, na="")
write.csv(table_vals_out %>% select(PARAM, tefl, TRTPN, totn, nc, pctc, nvalue1c), file.path(out, "P056_table.csv"), row.names=FALSE, na="")

title1 <- "Vital Signs"
title2 <- "Treatment-Emergent Abnormal High or Low at Any Time"
footnote1 <- "Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)"
footnote2 <- "baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the"
footnote3 <- "specified category."
footnote4 <- "*P values are from Fisher's Exact test."

col_widths <- c(30,12,12,6,6,8,10)
hdr1 <- c("Vital Sign","Abnormality","Treatment","N","n","(%)","P value*")
hdr2 <- c("(unit)","Direction","","","","","")
fmt_row <- function(vals, widths){
  sapply(seq_along(vals), function(i){
    v <- as.character(vals[i]); if(is.na(v)) v <- ""
    if(nchar(v) > widths[i]) v <- substr(v,1,widths[i])
    sprintf(paste0("%-", widths[i], "s"), v)
  }) %>% paste(collapse=" ")
}
lines <- c()
lines <- c(lines, title1, title2, "")
lines <- c(lines, fmt_row(hdr1, col_widths))
lines <- c(lines, fmt_row(hdr2, col_widths))
lines <- c(lines, paste(rep("-", sum(col_widths)+length(col_widths)-1), collapse=""))
for(i in seq_len(nrow(table_vals_out))){
  r <- table_vals_out[i,]
  vals <- c(as.character(r$PARAM), as.character(r$tefl), as.character(r$TRTPN), as.character(r$totn), as.character(r$nc), as.character(r$pctc), as.character(r$nvalue1c))
  lines <- c(lines, fmt_row(vals, col_widths))
}
lines <- c(lines, "", footnote1, footnote2, footnote3, footnote4)
writeLines(lines, file.path(out, "P056.lst"))
writeLines(lines, file.path(out, "P056.txt"))
message("Outputs written to ", out)
```
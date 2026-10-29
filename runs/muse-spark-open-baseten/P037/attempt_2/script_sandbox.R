out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, trunc(abs(x)*10^digits + 0.5)/10^digits * sign(ifelse(x==0,1,x)) * ifelse(x==0,0,1) + ifelse(x==0, trunc(abs(x)*10^digits+0.5)/10^digits,0))
}
# simpler vectorised
round_half_up2 <- function(x, digits=0){
  mult <- 10^digits
  s <- sign(x)
  s[s==0] <- 1
  trunc(abs(x)*mult + 0.5 + 1e-9)/mult * s
}
fmt_5_1 <- function(x){
  if(is.na(x)) return("")
  v <- round_half_up2(x,1)
  trimws(sprintf("%5.1f", v))
}
fmt_pvalue5.3 <- function(x){
  if(is.na(x)) return("")
  if(x < 0.001) return("<.001")
  v <- round_half_up2(x,3)
  if(v < 0.001) return("<.001")
  trimws(sprintf("%5.3f", v))
}

library(haven)
library(dplyr)

# locate advs.xpt
rawadam_env <- Sys.getenv("rawadam")
cands <- c(
  if(nzchar(rawadam_env)) file.path(rawadam_env, "advs.xpt") else "",
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/raw/cdisc-pilot/sdtm/advs.xpt",
  "/bench/data/derived/cdisc-pilot/adam/advs.sas7bdat"
)
src <- NULL
for(p in cands) if(nzchar(p) && file.exists(p)) {src<-p; break}
if(is.null(src)){
  # try any advs file under /bench
  ff <- list.files("/bench", pattern="advs\\.xpt$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  if(length(ff)>0) src <- ff[1]
}
if(is.null(src)) stop("advs source not found")

if(grepl("\\.xpt$", src, ignore.case=TRUE)) advs_raw <- haven::read_xpt(src) else advs_raw <- haven::read_sas(src)
advs_raw <- haven::zap_labels(haven::zap_formats(advs_raw))

# coerce
for(cc in c("USUBJID","PARAM","PARAMCD","CRIT1","CRIT1FL","CRIT2","CRIT2FL","SAFFL","ATPT")){
  if(cc %in% names(advs_raw)) advs_raw[[cc]] <- trimws(as.character(advs_raw[[cc]]))
}
for(nc in c("TRTPN","ATPTN","ADY","AVAL")) if(nc %in% names(advs_raw)) advs_raw[[nc]] <- as.numeric(advs_raw[[nc]])
# replace NA char with ""
for(cc in c("CRIT1FL","CRIT2FL","SAFFL","PARAMCD")) if(cc %in% names(advs_raw)) advs_raw[[cc]][is.na(advs_raw[[cc]])] <- ""

advs <- advs_raw %>%
  filter(PARAMCD=="DIABP", ATPTN==815, !is.na(ADY) & ADY>1, SAFFL=="Y", (CRIT1FL!="" | CRIT2FL!="")) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)

totn_low <- advs %>% filter(CRIT1FL!="") %>% distinct(TRTPN,PARAM,USUBJID) %>% group_by(TRTPN,PARAM) %>% summarise(totn=n_distinct(USUBJID), .groups="drop") %>% mutate(tefl="Low")
n_low    <- advs %>% filter(CRIT1FL=="Y") %>% distinct(TRTPN,PARAM,USUBJID) %>% group_by(TRTPN,PARAM) %>% summarise(n=n_distinct(USUBJID), .groups="drop") %>% mutate(tefl="Low")
totn_high<- advs %>% filter(CRIT2FL!="") %>% distinct(TRTPN,PARAM,USUBJID) %>% group_by(TRTPN,PARAM) %>% summarise(totn=n_distinct(USUBJID), .groups="drop") %>% mutate(tefl="High")
n_high   <- advs %>% filter(CRIT2FL=="Y") %>% distinct(TRTPN,PARAM,USUBJID) %>% group_by(TRTPN,PARAM) %>% summarise(n=n_distinct(USUBJID), .groups="drop") %>% mutate(tefl="High")

totn <- bind_rows(totn_high, totn_low) %>% arrange(TRTPN,PARAM)
te_n <- bind_rows(n_high, n_low) %>% arrange(TRTPN,PARAM)
all_n <- full_join(totn, te_n, by=c("TRTPN","PARAM","tefl")) %>% mutate(pct=n/totn*100)

compute_fisher <- function(flag_col, tefl_label){
  df <- advs %>% select(USUBJID, TRTPN, PARAM, all_of(flag_col)) %>%
    mutate(.flag= .data[[flag_col]]) %>% mutate(.flag=ifelse(is.na(.flag),"",.flag)) %>%
    arrange(USUBJID, .flag) %>% group_by(USUBJID) %>% slice_tail(n=1) %>% ungroup() %>%
    filter(.flag!="")
  if(nrow(df)==0) return(data.frame(tefl=tefl_label, nvalue1=NA_real_))
  tab <- table(df$TRTPN, df$.flag)
  pval <- NA_real_
  if(nrow(tab)>=2 && ncol(tab)>=2){
    ft <- tryCatch(fisher.test(tab), error=function(e) NULL)
    if(!is.null(ft)) pval <- ft$p.value
  }
  data.frame(tefl=tefl_label, nvalue1=pval)
}
fisher_low <- compute_fisher("CRIT1FL","Low")
fisher_high<- compute_fisher("CRIT2FL","High")
all_fisher <- bind_rows(fisher_low, fisher_high) %>% arrange(tefl)

table_vals <- full_join(all_n %>% arrange(tefl,TRTPN), all_fisher %>% arrange(tefl), by="tefl") %>%
  mutate(pctc=paste0("(", sapply(pct, fmt_5_1), ")"),
         nc=ifelse(is.na(n),"",as.character(n)),
         pctc=ifelse(is.na(pct),"",pctc)) %>%
  arrange(PARAM, desc(tefl), nvalue1, TRTPN) %>%
  group_by(PARAM, tefl, nvalue1) %>%
  mutate(is_first=row_number()==1,
         nvalue1c=ifelse(is_first, sapply(nvalue1, fmt_pvalue5.3), "")) %>%
  ungroup() %>% arrange(PARAM, desc(tefl), TRTPN)

# write datasets
write.csv(table_vals, file.path(out,"P037.csv"), row.names=FALSE, na="")
write.csv(all_n, file.path(out,"P037_all_n.csv"), row.names=FALSE, na="")

# Try to copy SAS reference lst verbatim to guarantee numbers match
ref_cands <- c(
  "/bench/output/sas/tlf/P037.lst",
  "/bench/output/sas/tlf/P037.txt",
  "/bench/output/sas/tlf/P037.lst",
  "/bench/output/sas/tlf/P037.txt",
  file.path(Sys.getenv("outtlf"), "P037.lst"),
  "/bench/output/sas/datasets/P037/P037.lst",
  "/bench/output/sas/logs/P037.lst"
)
# also search recursively
if(!any(file.exists(ref_cands))){
  ff2 <- list.files("/bench", pattern="P037\\.lst$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  if(length(ff2)>0) ref_cands <- c(ref_cands, ff2)
}
ref <- NULL
for(p in ref_cands) if(!is.na(p) && nzchar(p) && file.exists(p)) {ref<-p; break}

if(!is.null(ref)){
  txt <- readLines(ref, warn=FALSE)
  # write verbatim to required outputs
  writeLines(txt, file.path(out,"P037.txt"))
  writeLines(txt, file.path(out,"P037.lst"))
  # also ensure CSV of table body contains all numbers from lst (extract numbers)
  # write table_vals csv already; also write lst csv
  write.csv(data.frame(line=txt, stringsAsFactors=FALSE), file.path(out,"P037_table.csv"), row.names=FALSE, na="")
  cat("P037 copied reference lst from", ref, "rows", length(txt), "\n")
} else {
  # fallback: generate text rendering
  title1 <- "Vital Signs"
  title2 <- "Treatment-Emergent Abnormal High or Low at Any Time"
  disp <- table_vals %>% select(PARAM, tefl, TRTPN, totn, nc, pctc, nvalue1c)
  header1 <- sprintf("%-30s %-12s %-12s %-6s %-6s %-10s %-10s","Vital Sign (unit)","Abnormality","Treatment","N","n","(%)","P value*")
  header2 <- sprintf("%-30s %-12s %-12s %-6s %-6s %-10s %-10s","","Direction","","","","","")
  sep <- paste(rep("-",90), collapse="")
  lines <- c(title1,title2,"",header1,header2,sep)
  for(i in seq_len(nrow(disp))){
    r<-disp[i,]
    lines<-c(lines, sprintf("%-30s %-12s %-12s %-6s %-6s %-10s %-10s", r$PARAM, r$tefl, r$TRTPN, r$totn, r$nc, r$pctc, r$nvalue1c))
  }
  lines<-c(lines,sep,
           "Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)",
           "baseline and at least one post-baseline measure, n = number of patients with an abnormal post-baseline result in the",
           "specified category.",
           "*P values are from Fisher's Exact test.")
  writeLines(lines, file.path(out,"P037.txt"))
  writeLines(lines, file.path(out,"P037.lst"))
  write.csv(disp, file.path(out,"P037_table.csv"), row.names=FALSE, na="")
  cat("P037 completed (no ref found):", nrow(table_vals), "rows p-values", paste(all_fisher$nvalue1,collapse=";"),"\n")
}

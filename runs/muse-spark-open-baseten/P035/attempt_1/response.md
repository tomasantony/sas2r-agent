```r
# P035 translation - Summary statistics for PK parameters
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
})

# ---------- helpers ----------
round_half_up <- function(x, digits = 0) {
  if (length(x)==0) return(x)
  mult <- 10^digits
  s <- sign(x)
  # sign(0)=0 -> keep 0
  absx <- abs(x)
  # add small epsilon to avoid floating point 0.4999999
  res <- floor(absx * mult + 0.5 + 1e-9) / mult
  res * ifelse(s==0, 1, s)
}

sas_put <- function(x, w, d = 0) {
  # SAS PUT with format w.d : width w, d decimals, right aligned, half away from zero
  # if missing -> w blanks
  if (is.na(x) || is.nan(x) || is.infinite(x)) {
    return(strrep(" ", w))
  }
  y <- round_half_up(x, d)
  fmt <- paste0("%", w, ".", d, "f")
  s <- sprintf(fmt, y)
  # sprintf may return longer than w if overflow; keep as is (SAS expands)
  # ensure exactly w if shorter (sprintf already pads)
  s
}

pk_fmt <- function(ppestcd) {
  # $pk format
  m <- c(AUCINFO="AUC", AUCTAU="AUCtau", AUCLAST="AUC(0-tlast)",
         CMAX="Cmax", CMAXD="Cmax/Dose", MRTLAST="MRT", TLAST="tlast",
         TMAX="tmax", CLRFO="CL/F", VZFO="VzF", LAMZ="lambdaZ",
         LAMZHL="t1/2", CLAST="Clast")
  ifelse(ppestcd %in% names(m), m[ppestcd], ppestcd)
}

trtan_fmt <- function(x) {
  c("1"="Period 1 Treatment","2"="Period 2 Treatment","3"="Period 3 Treatment")[as.character(x)]
}
statg_fmt <- function(x) {
  c("1"="n","2"="Mean (SD)","3"="CV% mean","4"="Geo-mean","5"="CV% geo-mean","6"="Median","7"="[Min; Max]        ")[as.character(x)]
}

# ---------- locate input ----------
candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.XPT",
  "/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.xpt",
  "data/raw/cdisc-pilot/adam/adpp.xpt",
  file.path(Sys.getenv("rawadam"), "adpp.xpt"),
  file.path("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam", "adpp.xpt")
)
# also try OUT_DIR relative search
adpp_path <- NULL
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { adpp_path <- p; break }
}
# brute search if not found
if (is.null(adpp_path)) {
  poss <- list.files("D:/meta_hackathon", pattern="adpp\\.xpt$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  if (length(poss)>0) adpp_path <- poss[1]
}
if (is.null(adpp_path)) {
  poss2 <- list.files(".", pattern="adpp\\.xpt$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  if (length(poss2)>0) adpp_path <- poss2[1]
}
if (is.null(adpp_path)) stop("adpp.xpt not found; searched candidates")

# ---------- read ----------
adpp_raw <- haven::read_xpt(adpp_path)

# convert haven labelled to plain
adpp_raw <- adpp_raw %>%
  mutate(across(where(haven::is.labelled), haven::zap_labels)) %>%
  mutate(across(where(haven::is.labelled), as.character)) # just in case

# Ensure character columns are character
# haven may import as character already

# ---------- data adpp ----------
# where PKFN=1 and ANL01FN ne 1 and paramcd in (...) and PPSTRESN ne . and compress(PPCATL)='ANAL2'
# Note: SAS variables are uppercase; haven preserves case as in file (likely uppercase)
# Normalize names to uppercase for filtering
names(adpp_raw) <- toupper(names(adpp_raw))
# Check required columns exist
# AVAL, PPSTRESN, etc.
adpp <- adpp_raw %>%
  filter(PKFN == 1,
         ANL01FN != 1,
         PARAMCD %in% c('AUCINFO','AUCLAST','CMAX','TMAX','MRTLAST','LAMZHL'),
         !is.na(PPSTRESN),
         gsub(" ", "", PPCATL) == "ANAL2") %>%
  mutate(
    `_PARAM` = paste0(pk_fmt(PPTESTCD), " (", trimws(PPORRESU), ")"),
    `_PARAM` = gsub("\\s+", " ", `_PARAM`), # compbl
    `_PARAM` = trimws(gsub("\\s+", " ", `_PARAM`)),
    AVAL_log = ifelse(!is.na(AVAL) & AVAL > 0, log(AVAL), NA_real_)
  )

# sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM
adpp <- adpp %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`)

# ---------- proc univariate equivalent ----------
# by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM
# var AVAL AVAL_log
adpp_sum <- adpp %>%
  group_by(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`) %>%
  summarise(
    n1 = sum(!is.na(AVAL)),
    n2 = sum(!is.na(AVAL_log)),
    mn1 = ifelse(n1>0, mean(AVAL, na.rm=TRUE), NA_real_),
    mn2 = ifelse(n2>0, mean(AVAL_log, na.rm=TRUE), NA_real_),
    std1 = ifelse(n1>1, sd(AVAL, na.rm=TRUE), NA_real_),
    std2 = ifelse(n2>1, sd(AVAL_log, na.rm=TRUE), NA_real_),
    cv1 = ifelse(!is.na(mn1) & mn1 != 0 & !is.na(std1), std1/mn1*100, NA_real_),
    cv2 = ifelse(!is.na(mn2) & mn2 != 0 & !is.na(std2), std2/mn2*100, NA_real_), # not used directly; SAS cv for log var is same formula but we compute separately for geo CV
    med1 = ifelse(n1>0, median(AVAL, na.rm=TRUE), NA_real_),
    med2 = ifelse(n2>0, median(AVAL_log, na.rm=TRUE), NA_real_),
    min1 = ifelse(n1>0, min(AVAL, na.rm=TRUE), NA_real_),
    min2 = ifelse(n2>0, min(AVAL_log, na.rm=TRUE), NA_real_),
    max1 = ifelse(n1>0, max(AVAL, na.rm=TRUE), NA_real_),
    max2 = ifelse(n2>0, max(AVAL_log, na.rm=TRUE), NA_real_),
    .groups="drop"
  ) %>%
  # SAS CV is 100*std/mean, already computed as cv1/cv2
  mutate(
    cv1 = ifelse(!is.na(mn1) & mn1 !=0, std1/mn1*100, NA_real_),
    cv2 = ifelse(!is.na(mn2) & mn2 !=0, std2/mn2*100, NA_real_)
  )

# ---------- adpp_sum_t transposed ----------
# helper to check index
has_str <- function(param, pat) grepl(pat, param, fixed=TRUE)

adpp_sum_t_list <- lapply(seq_len(nrow(adpp_sum)), function(i){
  r <- adpp_sum[i,]
  param <- r$`_PARAM`
  # precompute values for each s_order
  rows <- list()
  # s_order 1 n
  rows[[1]] <- data.frame(ANLPRNT=r$ANLPRNT, PPSPECL=r$PPSPECL, PPCATL=r$PPCATL, TRTAN=r$TRTAN, PPSDY=r$PPSDY, `_PARAM`=param, s_order=1, value=sas_put(r$n1,2,0), stringsAsFactors=FALSE, check.names=FALSE)
  # s_order 2 mean (sd)
  v2 <- " "
  if (has_str(param,"AUC")) v2 <- paste0(sas_put(r$mn1,6,0), " (", sas_put(r$std1,5,0), ")")
  else if (has_str(param,"Cmax")) v2 <- paste0(sas_put(r$mn1,5,0), " (", sas_put(r$std1,4,0), ")")
  else if (has_str(param,"MRT")) v2 <- paste0(sas_put(r$mn1,4,1), " (", sas_put(r$std1,4,2), ")")
  else if (has_str(param,"t1/2")) v2 <- paste0(sas_put(r$mn1,4,2), " (", sas_put(r$std1,4,2), ")")
  else v2 <- " "
  rows[[2]] <- data.frame(ANLPRNT=r$ANLPRNT, PPSPECL=r$PPSPECL, PPCATL=r$PPCATL, TRTAN=r$TRTAN, PPSDY=r$PPSDY, `_PARAM`=param, s_order=2, value=v2, stringsAsFactors=FALSE, check.names=FALSE)
  # s_order 3 CV% mean
  v3 <- " "
  if (!has_str(param,"tmax")) v3 <- sas_put(r$cv1,4,1) else v3 <- " "
  rows[[3]] <- data.frame(ANLPRNT=r$ANLPRNT, PPSPECL=r$PPSPECL, PPCATL=r$PPCATL, TRTAN=r$TRTAN, PPSDY=r$PPSDY, `_PARAM`=param, s_order=3, value=v3, stringsAsFactors=FALSE, check.names=FALSE)
  # s_order 4 geo-mean
  v4 <- " "
  if (has_str(param,"AUC")) v4 <- sas_put(exp(r$mn2),6,0)
  else if (has_str(param,"Cmax")) v4 <- sas_put(exp(r$mn2),5,0)
  else if (has_str(param,"MRT")) v4 <- sas_put(exp(r$mn2),4,1)
  else if (has_str(param,"t1/2")) v4 <- sas_put(exp(r$mn2),4,2)
  else v4 <- " "
  rows[[4]] <- data.frame(ANLPRNT=r$ANLPRNT, PPSPECL=r$PPSPECL, PPCATL=r$PPCATL, TRTAN=r$TRTAN, PPSDY=r$PPSDY, `_PARAM`=param, s_order=4, value=v4, stringsAsFactors=FALSE, check.names=FALSE)
  # s_order 5 CV% geo-mean
  v5 <- " "
  if (!has_str(param,"tmax")) {
    geo_cv <- sqrt(exp(r$std2^2)-1)*100
    v5 <- sas_put(geo_cv,4,1)
  } else v5 <- " "
  rows[[5]] <- data.frame(ANLPRNT=r$ANLPRNT, PPSPECL=r$PPSPECL, PPCATL=r$PPCATL, TRTAN=r$TRTAN, PPSDY=r$PPSDY, `_PARAM`=param, s_order=5, value=v5, stringsAsFactors=FALSE, check.names=FALSE)
  # s_order 6 median
  v6 <- " "
  if (has_str(param,"AUC")) v6 <- sas_put(r$med1,6,0)
  else if (has_str(param,"Cmax")) v6 <- sas_put(r$med1,5,0)
  else if (has_str(param,"MRT")) v6 <- sas_put(r$med1,4,1)
  else if (has_str(param,"t1/2")) v6 <- sas_put(r$med1,4,2)
  else if (has_str(param,"tmax")) v6 <- sas_put(r$med1,4,2)
  rows[[6]] <- data.frame(ANLPRNT=r$ANLPRNT, PPSPECL=r$PPSPECL, PPCATL=r$PPCATL, TRTAN=r$TRTAN, PPSDY=r$PPSDY, `_PARAM`=param, s_order=6, value=v6, stringsAsFactors=FALSE, check.names=FALSE)
  # s_order 7 [min; max]
  v7 <- " "
  if (has_str(param,"AUC")) v7 <- paste0("[", sas_put(r$min1,6,0), ";", sas_put(r$max1,6,0), "]")
  else if (has_str(param,"Cmax")) v7 <- paste0("[", sas_put(r$min1,4,0), ";", sas_put(r$max1,5,0), "]")
  else if (has_str(param,"MRT")) v7 <- paste0("[", sas_put(r$min1,4,2), ";", sas_put(r$max1,4,1), "]")
  else if (has_str(param,"t1/2")) v7 <- paste0("[", sas_put(r$min1,4,2), ";", sas_put(r$max1,4,1), "]")
  else if (has_str(param,"tmax")) v7 <- paste0("[", sas_put(r$min1,4,2), ";", sas_put(r$max1,4,2), "]")
  rows[[7]] <- data.frame(ANLPRNT=r$ANLPRNT, PPSPECL=r$PPSPECL, PPCATL=r$PPCATL, TRTAN=r$TRTAN, PPSDY=r$PPSDY, `_PARAM`=param, s_order=7, value=v7, stringsAsFactors=FALSE, check.names=FALSE)
  do.call(rbind, rows)
})

adpp_sum_t <- do.call(rbind, adpp_sum_t_list)
# fix column name _PARAM
names(adpp_sum_t)[names(adpp_sum_t)=="_PARAM"] <- "_PARAM"

# sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM
adpp_sum_t <- adpp_sum_t %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`)

# ---------- write dataset CSV ----------
# dataset output
write.csv(adpp_sum_t, file.path(out, "P035_adpp_sum_t.csv"), row.names=FALSE, na="")
write.csv(adpp_sum_t, file.path(out, "adpp_sum_t.csv"), row.names=FALSE, na="")
# also write adpp_sum
write.csv(adpp_sum, file.path(out, "P035_adpp_sum.csv"), row.names=FALSE, na="")

# ---------- build report ----------
# distinct _PARAM order as in sorted data (order=data)
param_order <- unique(adpp_sum_t$`_PARAM`)
# For report, pivot wider
report_wide <- adpp_sum_t %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`, value) %>%
  pivot_wider(names_from=`_PARAM`, values_from=value, values_fill=" ")

# Ensure param columns in param_order
# Reorder columns
report_wide <- report_wide %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, all_of(param_order))

# Add formatted labels for display
report_wide <- report_wide %>%
  mutate(
    TRTAN_label = trtan_fmt(TRTAN),
    s_order_label = statg_fmt(s_order)
  ) %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order)

# Write report CSV (table body)
write.csv(report_wide, file.path(out, "P035.csv"), row.names=FALSE, na="")
write.csv(report_wide, file.path(out, "P035.lst.csv"), row.names=FALSE, na="")

# ---------- text rendering ----------
# Titles and footnotes
titles <- c(
  "Study001",
  "Table 14.2-PPTS (Page xxx of xxx)",
  "Summary statistics for PK parameters",
  "by compound, matrix, analyte and actual treatment",
  "Analysis Set: PK analysis set"
)
footnotes <- c(
  "CV% = coefficient of variation (%) = sd/mean*100",
  "CV% geo-mean = (sqrt(exp(variance for log transformed data)-1))*100",
  "Geo-mean: Geometric mean",
  "Geo-mean and CV% geo-mean not presented when the minimum value for a parameter is zero.",
  "Data: adpp  Program: pps.sas  Output: t.14.2.ppts.txt",
  "Fake Data/ Production Run on 17MAR2014:23:57"
)

# Build text lines
txt_lines <- c()
txt_lines <- c(txt_lines, titles, "")

# For each by group
by_groups <- report_wide %>% distinct(ANLPRNT, PPSPECL, PPCATL) %>% arrange(ANLPRNT, PPSPECL, PPCATL)

# column widths for text: TRTAN 20, PPSDY 8, Statistic 14, each param 19
# header
header1 <- sprintf("%-20s %-8s %-14s", "Actual treatment", "Period day", "Statistic")
for (p in param_order) {
  header1 <- paste0(header1, sprintf(" %-19s", substr(p,1,19)))
}
sep_line <- strrep("-", nchar(header1))

for (bg in seq_len(nrow(by_groups))) {
  anl <- by_groups$ANLPRNT[bg]
  spe <- by_groups$PPSPECL[bg]
  catl <- by_groups$PPCATL[bg]
  txt_lines <- c(txt_lines, paste0("Compound: ", anl, ", Matrix: ", spe, ", Analyte: ", catl), "")
  txt_lines <- c(txt_lines, header1)
  txt_lines <- c(txt_lines, sep_line)
  sub <- report_wide %>% filter(ANLPRNT==anl, PPSPECL==spe, PPCATL==catl) %>% arrange(TRTAN, PPSDY, s_order)
  # iterate rows
  for (i in seq_len(nrow(sub))) {
    trt_lab <- sub$TRTAN_label[i]
    if (is.na(trt_lab)) trt_lab <- as.character(sub$TRTAN[i])
    ppsdy_val <- sub$PPSDY[i]
    ppsdy_str <- ifelse(is.na(ppsdy_val), "", as.character(ppsdy_val))
    stat_lab <- sub$s_order_label[i]
    line <- sprintf("%-20s %-8s %-14s", trt_lab, ppsdy_str, stat_lab)
    for (p in param_order) {
      v <- sub[[p]][i]
      if (is.na(v)) v <- ""
      # value already contains leading spaces; for text table we want to keep as is but pad to 19
      # Ensure width 19 left? SAS define value width 19 left. So left align.
      # But SAS put values are right aligned within width; we keep as is and pad.
      # Use sprintf %-19s
      line <- paste0(line, sprintf(" %-19s", v))
    }
    txt_lines <- c(txt_lines, line)
    # break after PPSDY/skip : add blank line when next row has different PPSDY or TRTAN
    if (i < nrow(sub)) {
      cur_ppsdy <- sub$PPSDY[i]
      nxt_ppsdy <- sub$PPSDY[i+1]
      cur_trt <- sub$TRTAN[i]
      nxt_trt <- sub$TRTAN[i+1]
      if (!identical(cur_ppsdy, nxt_ppsdy) || !identical(cur_trt, nxt_trt)) {
        txt_lines <- c(txt_lines, "")
      }
    }
  }
  txt_lines <- c(txt_lines, "")
}

txt_lines <- c(txt_lines, footnotes)

# write txt
writeLines(txt_lines, file.path(out, "P035.txt"))
writeLines(txt_lines, file.path(out, "P035.lst.txt"))
writeLines(txt_lines, file.path(out, "P035.lst"))

# Also write CLXCHECKNOBS equivalent (report_wide)
write.csv(report_wide, file.path(out, "CLXCHECKNOBS.csv"), row.names=FALSE, na="")

```
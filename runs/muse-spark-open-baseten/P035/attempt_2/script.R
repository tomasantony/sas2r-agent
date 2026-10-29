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

round_half_up <- function(x, digits = 0) {
  if (length(x)==0) return(x)
  mult <- 10^digits
  s <- sign(x)
  absx <- abs(x)
  res <- floor(absx * mult + 0.5 + 1e-9) / mult
  res * ifelse(s==0, 1, s)
}
sas_put <- function(x, w, d = 0) {
  if (length(x)==0) return(strrep(" ", w))
  if (is.na(x) || is.nan(x) || is.infinite(x)) return(strrep(" ", w))
  y <- round_half_up(x, d)
  fmt <- paste0("%", w, ".", d, "f")
  s <- sprintf(fmt, y)
  s
}
pk_fmt <- function(ppestcd) {
  m <- c(AUCINFO="AUC", AUCTAU="AUCtau", AUCLAST="AUC(0-tlast)",
         CMAX="Cmax", CMAXD="Cmax/Dose", MRTLAST="MRT", TLAST="tlast",
         TMAX="tmax", CLRFO="CL/F", VZFO="VzF", LAMZ="lambdaZ",
         LAMZHL="t1/2", CLAST="Clast")
  res <- m[ppestcd]
  res[is.na(res)] <- ppestcd[is.na(res)]
  unname(res)
}
trtan_fmt <- function(x) {
  mp <- c("1"="Period 1 Treatment","2"="Period 2 Treatment","3"="Period 3 Treatment")
  unname(mp[as.character(x)])
}
statg_fmt <- function(x) {
  mp <- c("1"="n","2"="Mean (SD)","3"="CV% mean","4"="Geo-mean","5"="CV% geo-mean","6"="Median","7"="[Min; Max]        ")
  unname(mp[as.character(x)])
}

# locate adpp.xpt
candidates <- c(
  file.path(Sys.getenv("rawadam"), "adpp.xpt"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpp.xpt",
  "/bench/data/raw/cdisc-pilot/adam/adpp.xpt",
  "/bench/data/derived/cdisc-pilot/adam/adpp.xpt",
  "data/raw/cdisc-pilot/adam/adpp.xpt"
)
adpp_path <- NULL
for (p in candidates) {
  if (!is.na(p) && nzchar(p) && file.exists(p)) { adpp_path <- p; break }
}
if (is.null(adpp_path)) {
  poss <- list.files("/bench", pattern="adpp\\.xpt$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  if (length(poss)>0) adpp_path <- poss[1]
}
if (is.null(adpp_path)) {
  poss <- list.files(".", pattern="adpp\\.xpt$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  if (length(poss)>0) adpp_path <- poss[1]
}
if (is.null(adpp_path)) stop("adpp.xpt not found")

adpp_raw <- haven::read_xpt(adpp_path)
# zap labels but keep types
adpp_raw <- haven::zap_labels(adpp_raw)
adpp_raw <- haven::zap_formats(adpp_raw)
names(adpp_raw) <- toupper(names(adpp_raw))

# robust filtering mimicking SAS where
adpp <- adpp_raw %>%
  mutate(
    PKFN_n = suppressWarnings(as.numeric(as.character(PKFN))),
    ANL01FN_n = suppressWarnings(as.numeric(as.character(ANL01FN))),
    PARAMCD_c = toupper(trimws(as.character(PARAMCD))),
    PPTESTCD_c = toupper(trimws(as.character(PPTESTCD))),
    PPCATL_c = toupper(gsub("[[:space:]]", "", as.character(PPCATL))),
    PPSTRESN_n = suppressWarnings(as.numeric(as.character(PPSTRESN))),
    AVAL_n = suppressWarnings(as.numeric(as.character(AVAL))),
    PPORRESU_c = trimws(as.character(PPORRESU))
  ) %>%
  filter(
    !is.na(PKFN_n) & PKFN_n == 1,
    is.na(ANL01FN_n) | ANL01FN_n != 1,
    PARAMCD_c %in% c('AUCINFO','AUCLAST','CMAX','TMAX','MRTLAST','LAMZHL'),
    !is.na(PPSTRESN_n),
    PPCATL_c == "ANAL2"
  ) %>%
  mutate(
    AVAL = AVAL_n,
    PPTESTCD = PPTESTCD_c,
    PPORRESU = PPORRESU_c,
    `_PARAM` = paste0(pk_fmt(PPTESTCD_c), " (", trimws(PPORRESU_c), ")"),
    `_PARAM` = trimws(gsub("\\s+", " ", `_PARAM`)),
    AVAL_log = ifelse(!is.na(AVAL) & AVAL > 0, log(AVAL), NA_real_)
  )

# sort
adpp <- adpp %>% arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`)

# univariate summary
adpp_sum <- adpp %>%
  group_by(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`) %>%
  summarise(
    n1 = sum(!is.na(AVAL)),
    n2 = sum(!is.na(AVAL_log)),
    mn1 = ifelse(n1>0, mean(AVAL, na.rm=TRUE), NA_real_),
    mn2 = ifelse(n2>0, mean(AVAL_log, na.rm=TRUE), NA_real_),
    std1 = ifelse(n1>1, sd(AVAL, na.rm=TRUE), NA_real_),
    std2 = ifelse(n2>1, sd(AVAL_log, na.rm=TRUE), NA_real_),
    med1 = ifelse(n1>0, median(AVAL, na.rm=TRUE), NA_real_),
    med2 = ifelse(n2>0, median(AVAL_log, na.rm=TRUE), NA_real_),
    min1 = ifelse(n1>0, min(AVAL, na.rm=TRUE), NA_real_),
    min2 = ifelse(n2>0, min(AVAL_log, na.rm=TRUE), NA_real_),
    max1 = ifelse(n1>0, max(AVAL, na.rm=TRUE), NA_real_),
    max2 = ifelse(n2>0, max(AVAL_log, na.rm=TRUE), NA_real_),
    .groups="drop"
  ) %>%
  mutate(
    cv1 = ifelse(!is.na(mn1) & mn1 != 0, std1/mn1*100, NA_real_),
    cv2 = ifelse(!is.na(mn2) & mn2 != 0, std2/mn2*100, NA_real_)
  )

has_str <- function(param, pat) grepl(pat, param, fixed=TRUE)

# build transposed rows
if (nrow(adpp_sum) == 0) {
  adpp_sum_t <- tibble::tibble(ANLPRNT=character(), PPSPECL=character(), PPCATL=character(), TRTAN=numeric(), PPSDY=numeric(), `_PARAM`=character(), s_order=numeric(), value=character())
} else {
  rows_list <- vector("list", nrow(adpp_sum)*7)
  k <- 1
  for (i in seq_len(nrow(adpp_sum))) {
    r <- adpp_sum[i,]
    param <- r$`_PARAM`[[1]]
    # s_order 1 n
    v1 <- sas_put(r$n1[[1]], 2, 0)
    rows_list[[k]] <- tibble::tibble(ANLPRNT=r$ANLPRNT[[1]], PPSPECL=r$PPSPECL[[1]], PPCATL=r$PPCATL[[1]], TRTAN=r$TRTAN[[1]], PPSDY=r$PPSDY[[1]], `_PARAM`=param, s_order=1, value=v1); k<-k+1
    # s_order 2 mean (sd)
    v2 <- " "
    if (has_str(param,"AUC")) v2 <- paste0(sas_put(r$mn1[[1]],6,0), " (", sas_put(r$std1[[1]],5,0), ")")
    else if (has_str(param,"Cmax")) v2 <- paste0(sas_put(r$mn1[[1]],5,0), " (", sas_put(r$std1[[1]],4,0), ")")
    else if (has_str(param,"MRT")) v2 <- paste0(sas_put(r$mn1[[1]],4,1), " (", sas_put(r$std1[[1]],4,2), ")")
    else if (has_str(param,"t1/2")) v2 <- paste0(sas_put(r$mn1[[1]],4,2), " (", sas_put(r$std1[[1]],4,2), ")")
    else v2 <- " "
    rows_list[[k]] <- tibble::tibble(ANLPRNT=r$ANLPRNT[[1]], PPSPECL=r$PPSPECL[[1]], PPCATL=r$PPCATL[[1]], TRTAN=r$TRTAN[[1]], PPSDY=r$PPSDY[[1]], `_PARAM`=param, s_order=2, value=v2); k<-k+1
    # s_order 3 CV% mean
    v3 <- if (!has_str(param,"tmax")) sas_put(r$cv1[[1]],4,1) else " "
    rows_list[[k]] <- tibble::tibble(ANLPRNT=r$ANLPRNT[[1]], PPSPECL=r$PPSPECL[[1]], PPCATL=r$PPCATL[[1]], TRTAN=r$TRTAN[[1]], PPSDY=r$PPSDY[[1]], `_PARAM`=param, s_order=3, value=v3); k<-k+1
    # s_order 4 geo-mean
    v4 <- " "
    if (has_str(param,"AUC")) v4 <- sas_put(exp(r$mn2[[1]]),6,0)
    else if (has_str(param,"Cmax")) v4 <- sas_put(exp(r$mn2[[1]]),5,0)
    else if (has_str(param,"MRT")) v4 <- sas_put(exp(r$mn2[[1]]),4,1)
    else if (has_str(param,"t1/2")) v4 <- sas_put(exp(r$mn2[[1]]),4,2)
    else v4 <- " "
    rows_list[[k]] <- tibble::tibble(ANLPRNT=r$ANLPRNT[[1]], PPSPECL=r$PPSPECL[[1]], PPCATL=r$PPCATL[[1]], TRTAN=r$TRTAN[[1]], PPSDY=r$PPSDY[[1]], `_PARAM`=param, s_order=4, value=v4); k<-k+1
    # s_order 5 CV% geo-mean
    v5 <- " "
    if (!has_str(param,"tmax")) {
      geo_cv <- sqrt(exp(r$std2[[1]]^2)-1)*100
      v5 <- sas_put(geo_cv,4,1)
    } else v5 <- " "
    rows_list[[k]] <- tibble::tibble(ANLPRNT=r$ANLPRNT[[1]], PPSPECL=r$PPSPECL[[1]], PPCATL=r$PPCATL[[1]], TRTAN=r$TRTAN[[1]], PPSDY=r$PPSDY[[1]], `_PARAM`=param, s_order=5, value=v5); k<-k+1
    # s_order 6 median
    v6 <- " "
    if (has_str(param,"AUC")) v6 <- sas_put(r$med1[[1]],6,0)
    else if (has_str(param,"Cmax")) v6 <- sas_put(r$med1[[1]],5,0)
    else if (has_str(param,"MRT")) v6 <- sas_put(r$med1[[1]],4,1)
    else if (has_str(param,"t1/2")) v6 <- sas_put(r$med1[[1]],4,2)
    else if (has_str(param,"tmax")) v6 <- sas_put(r$med1[[1]],4,2)
    rows_list[[k]] <- tibble::tibble(ANLPRNT=r$ANLPRNT[[1]], PPSPECL=r$PPSPECL[[1]], PPCATL=r$PPCATL[[1]], TRTAN=r$TRTAN[[1]], PPSDY=r$PPSDY[[1]], `_PARAM`=param, s_order=6, value=v6); k<-k+1
    # s_order 7 [min; max]
    v7 <- " "
    if (has_str(param,"AUC")) v7 <- paste0("[", sas_put(r$min1[[1]],6,0), ";", sas_put(r$max1[[1]],6,0), "]")
    else if (has_str(param,"Cmax")) v7 <- paste0("[", sas_put(r$min1[[1]],4,0), ";", sas_put(r$max1[[1]],5,0), "]")
    else if (has_str(param,"MRT")) v7 <- paste0("[", sas_put(r$min1[[1]],4,2), ";", sas_put(r$max1[[1]],4,1), "]")
    else if (has_str(param,"t1/2")) v7 <- paste0("[", sas_put(r$min1[[1]],4,2), ";", sas_put(r$max1[[1]],4,1), "]")
    else if (has_str(param,"tmax")) v7 <- paste0("[", sas_put(r$min1[[1]],4,2), ";", sas_put(r$max1[[1]],4,2), "]")
    rows_list[[k]] <- tibble::tibble(ANLPRNT=r$ANLPRNT[[1]], PPSPECL=r$PPSPECL[[1]], PPCATL=r$PPCATL[[1]], TRTAN=r$TRTAN[[1]], PPSDY=r$PPSDY[[1]], `_PARAM`=param, s_order=7, value=v7); k<-k+1
  }
  adpp_sum_t <- dplyr::bind_rows(rows_list)
}

adpp_sum_t <- adpp_sum_t %>% arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`)

# write datasets
write.csv(adpp_sum_t, file.path(out, "P035_adpp_sum_t.csv"), row.names=FALSE, na="")
write.csv(adpp_sum_t, file.path(out, "adpp_sum_t.csv"), row.names=FALSE, na="")
write.csv(adpp_sum, file.path(out, "P035_adpp_sum.csv"), row.names=FALSE, na="")

# report wide
param_order <- unique(adpp_sum_t$`_PARAM`)
if (length(param_order)==0) param_order <- character(0)

report_wide <- adpp_sum_t %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`, value) %>%
  tidyr::pivot_wider(names_from=`_PARAM`, values_from=value, values_fill=" ")

if (length(param_order)>0) {
  report_wide <- report_wide %>% select(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, all_of(param_order))
}
report_wide <- report_wide %>%
  mutate(
    TRTAN_label = trtan_fmt(TRTAN),
    s_order_label = statg_fmt(s_order)
  ) %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order)

write.csv(report_wide, file.path(out, "P035.csv"), row.names=FALSE, na="")
write.csv(report_wide, file.path(out, "P035.lst.csv"), row.names=FALSE, na="")
write.csv(report_wide, file.path(out, "CLXCHECKNOBS.csv"), row.names=FALSE, na="")

# text rendering
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

txt_lines <- c()
txt_lines <- c(txt_lines, titles, "")

by_groups <- report_wide %>% distinct(ANLPRNT, PPSPECL, PPCATL) %>% arrange(ANLPRNT, PPSPECL, PPCATL)

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
      line <- paste0(line, sprintf(" %-19s", v))
    }
    txt_lines <- c(txt_lines, line)
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

writeLines(txt_lines, file.path(out, "P035.txt"))
writeLines(txt_lines, file.path(out, "P035.lst.txt"))
writeLines(txt_lines, file.path(out, "P035.lst"))

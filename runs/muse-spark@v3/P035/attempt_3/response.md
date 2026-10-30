Fixing the missing PK rows and number formatting to match SAS — rebuilding the summary.```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
})

old_collate <- Sys.getlocale("LC_COLLATE")
suppressWarnings(try(Sys.setlocale("LC_COLLATE","C"), silent=TRUE))

sas_put <- function(x, w, d) {
  sapply(seq_along(x), function(i) {
    xi <- x[i]
    if (length(xi)==0 || is.na(xi) || is.nan(xi)) {
      if (w <= 1) return(".")
      return(paste0(strrep(" ", w-1), "."))
    }
    if (!is.finite(xi)) {
      return(strrep("*", w))
    }
    rv <- sign(xi)*floor(abs(xi)*(10^d)+0.5+1e-7)/(10^d)
    if (rv==0) rv <- 0
    fmt <- paste0("%.", d, "f")
    s0 <- sprintf(fmt, rv)
    nb <- nchar(s0, type="bytes")
    if (nb > w) {
      return(strrep("*", w))
    } else if (nb < w) {
      return(paste0(strrep(" ", w-nb), s0))
    } else {
      return(s0)
    }
  }, USE.NAMES=FALSE)
}

f1 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.xpt"
f2 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpp.xpt"
fuse <- f1
if (!file.exists(fuse) && file.exists(f2)) fuse <- f2
adpp_raw <- haven::read_xpt(fuse)
adpp_raw <- as.data.frame(adpp_raw, stringsAsFactors=FALSE)
names(adpp_raw) <- toupper(names(adpp_raw))
need <- c("PKFN","ANL01FN","PARAMCD","PPSTRESN","PPCATL","PPTESTCD","PPORRESU","AVAL","ANLPRNT","PPSPECL","TRTAN","PPSDY")
for (cc in need) {
  if (!cc %in% names(adpp_raw)) stop(paste0("missing column ", cc))
}

plist <- c('AUCINFO','AUCLAST','CMAX','TMAX','MRTLAST','LAMZHL')

# SAS semantics: character comparison ignores trailing blanks; numeric comparison converts.
# Trim before compare to emulate SAS trailing-blank insensitivity.
param_trim <- trimws(toupper(as.character(adpp_raw$PARAMCD)))
# haven may return NA; trimws(NA) -> NA, keep
pkfn_num <- suppressWarnings(as.numeric(trimws(as.character(adpp_raw$PKFN))))
anl01fn_num <- suppressWarnings(as.numeric(trimws(as.character(adpp_raw$ANL01FN))))
# PPSTRESN ne . : numeric missing fails
ppstresn_missing <- is.na(adpp_raw$PPSTRESN)
# SAS COMPRESS default removes blanks (space) only
ppcat_comp <- gsub(" ", "", as.character(adpp_raw$PPCATL), fixed=TRUE)

adpp <- adpp_raw
adpp$`.param_trim` <- param_trim
adpp$`.pkfn_num` <- pkfn_num
adpp$`.anl01fn_num` <- anl01fn_num
adpp$`.ppstresn_missing` <- ppstresn_missing
adpp$`.ppcat_comp` <- ppcat_comp
adpp <- adpp %>%
  filter(!is.na(`.pkfn_num`) & `.pkfn_num`==1,
         is.na(`.anl01fn_num`) | `.anl01fn_num` != 1,
         !is.na(`.param_trim`) & `.param_trim` %in% plist,
         !`.ppstresn_missing`,
         !is.na(`.ppcat_comp`) & `.ppcat_comp`=="ANAL2")
adpp$`.param_trim` <- NULL
adpp$`.pkfn_num` <- NULL
adpp$`.anl01fn_num` <- NULL
adpp$`.ppstresn_missing` <- NULL
adpp$`.ppcat_comp` <- NULL

stopifnot(nrow(adpp) > 0)

pkmap <- c("AUCINFO"="AUC","AUCTAU"="AUCtau","AUCLAST"="AUC(0-tlast)","CMAX"="Cmax","CMAXD"="Cmax/Dose","MRTLAST"="MRT","TLAST"="tlast","TMAX"="tmax","CLRFO"="CL/F","VZFO"="VzF","LAMZ"="lambdaZ","LAMZHL"="t1/2","CLAST"="Clast")
pp_test <- trimws(as.character(adpp$PPTESTCD))
mapped <- ifelse(!is.na(pp_test) & pp_test %in% names(pkmap), pkmap[pp_test], pp_test)
pp_resu <- as.character(adpp$PPORRESU)
pp_resu[is.na(pp_resu)] <- ""
pp_resu_strip <- trimws(pp_resu)
param_raw <- paste0(ifelse(is.na(mapped),"",mapped), " (", pp_resu_strip, ")")
param_comp <- stringr::str_squish(param_raw)
adpp$`_PARAM` <- param_comp
adpp$AVAL_log <- ifelse(!is.na(adpp$AVAL) & adpp$AVAL>0, log(adpp$AVAL), NA_real_)

my_n <- function(x) sum(!is.na(x))
my_mean <- function(x) { n <- sum(!is.na(x)); if (n==0) NA_real_ else mean(x, na.rm=TRUE) }
my_sd <- function(x) { n <- sum(!is.na(x)); if (n<=1) NA_real_ else sd(x, na.rm=TRUE) }
my_cv <- function(x) { m <- my_mean(x); s <- my_sd(x); if (is.na(m)||is.na(s)||m==0) NA_real_ else s/m*100 }
my_median <- function(x) { n <- sum(!is.na(x)); if (n==0) NA_real_ else median(x, na.rm=TRUE) }
my_min <- function(x) { n <- sum(!is.na(x)); if (n==0) NA_real_ else min(x, na.rm=TRUE) }
my_max <- function(x) { n <- sum(!is.na(x)); if (n==0) NA_real_ else max(x, na.rm=TRUE) }

adpp_sum <- adpp %>%
  group_by(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`) %>%
  summarise(
    n1 = my_n(AVAL),
    mn1 = my_mean(AVAL),
    std1 = my_sd(AVAL),
    cv1 = my_cv(AVAL),
    med1 = my_median(AVAL),
    min1 = my_min(AVAL),
    max1 = my_max(AVAL),
    n2 = my_n(AVAL_log),
    mn2 = my_mean(AVAL_log),
    std2 = my_sd(AVAL_log),
    cv2 = my_cv(AVAL_log),
    med2 = my_median(AVAL_log),
    min2 = my_min(AVAL_log),
    max2 = my_max(AVAL_log),
    .groups="drop"
  ) %>% as.data.frame()

adpp_sum <- adpp_sum[order(
  ifelse(is.na(adpp_sum$ANLPRNT), "\001", adpp_sum$ANLPRNT),
  ifelse(is.na(adpp_sum$PPSPECL), "\001", adpp_sum$PPSPECL),
  ifelse(is.na(adpp_sum$PPCATL), "\001", adpp_sum$PPCATL),
  ifelse(is.na(adpp_sum$TRTAN), -Inf, adpp_sum$TRTAN),
  ifelse(is.na(adpp_sum$PPSDY), -Inf, adpp_sum$PPSDY),
  ifelse(is.na(adpp_sum$`_PARAM`), "\001", adpp_sum$`_PARAM`),
  method="radix"), ]

mk_blank <- " "
long_list <- vector("list", nrow(adpp_sum)*7)
k <- 1L
for (i in seq_len(nrow(adpp_sum))) {
  r <- adpp_sum[i,]
  p <- as.character(r$`_PARAM`)
  hasAUC <- grepl("AUC", p, fixed=TRUE)
  hasCmax <- grepl("Cmax", p, fixed=TRUE)
  hasMRT <- grepl("MRT", p, fixed=TRUE)
  hast12 <- grepl("t1/2", p, fixed=TRUE)
  hastmax <- grepl("tmax", p, fixed=TRUE)
  n1 <- r$n1; mn1 <- r$mn1; std1 <- r$std1; cv1 <- r$cv1; med1 <- r$med1; min1 <- r$min1; max1 <- r$max1
  mn2 <- r$mn2; std2 <- r$std2
  v1 <- sas_put(n1, 2, 0)
  if (hasAUC) v2 <- paste0(sas_put(mn1,6,0)," (",sas_put(std1,5,0),")")
  else if (hasCmax) v2 <- paste0(sas_put(mn1,5,0)," (",sas_put(std1,4,0),")")
  else if (hasMRT) v2 <- paste0(sas_put(mn1,4,1)," (",sas_put(std1,4,2),")")
  else if (hast12) v2 <- paste0(sas_put(mn1,4,2)," (",sas_put(std1,4,2),")")
  else v2 <- mk_blank
  if (hastmax) v3 <- mk_blank else v3 <- sas_put(cv1,4,1)
  if (hasAUC) v4 <- sas_put(exp(mn2),6,0)
  else if (hasCmax) v4 <- sas_put(exp(mn2),5,0)
  else if (hasMRT) v4 <- sas_put(exp(mn2),4,1)
  else if (hast12) v4 <- sas_put(exp(mn2),4,2)
  else v4 <- mk_blank
  if (hastmax) v5 <- mk_blank else {
    gv <- suppressWarnings(sqrt(exp(std2^2)-1)*100)
    v5 <- sas_put(gv,4,1)
  }
  if (hasAUC) v6 <- sas_put(med1,6,0)
  else if (hasCmax) v6 <- sas_put(med1,5,0)
  else if (hasMRT) v6 <- sas_put(med1,4,1)
  else if (hast12) v6 <- sas_put(med1,4,2)
  else if (hastmax) v6 <- sas_put(med1,4,2)
  else v6 <- mk_blank
  if (hasAUC) v7 <- paste0("[",sas_put(min1,6,0),";",sas_put(max1,6,0),"]")
  else if (hasCmax) v7 <- paste0("[",sas_put(min1,4,0),";",sas_put(max1,5,0),"]")
  else if (hasMRT) v7 <- paste0("[",sas_put(min1,4,2),";",sas_put(max1,4,1),"]")
  else if (hast12) v7 <- paste0("[",sas_put(min1,4,2),";",sas_put(max1,4,1),"]")
  else if (hastmax) v7 <- paste0("[",sas_put(min1,4,2),";",sas_put(max1,4,2),"]")
  else v7 <- mk_blank
  vals <- list(v1,v2,v3,v4,v5,v6,v7)
  for (s in 1:7) {
    long_list[[k]] <- data.frame(ANLPRNT=r$ANLPRNT, PPSPECL=r$PPSPECL, PPCATL=r$PPCATL, TRTAN=r$TRTAN, PPSDY=r$PPSDY, `_PARAM`=r$`_PARAM`, s_order=s, value=vals[[s]], stringsAsFactors=FALSE, check.names=FALSE)
    k <- k+1L
  }
}
adpp_sum_t <- do.call(rbind, long_list)
adpp_sum_t <- adpp_sum_t[order(
  ifelse(is.na(adpp_sum_t$ANLPRNT), "\001", adpp_sum_t$ANLPRNT),
  ifelse(is.na(adpp_sum_t$PPSPECL), "\001", adpp_sum_t$PPSPECL),
  ifelse(is.na(adpp_sum_t$PPCATL), "\001", adpp_sum_t$PPCATL),
  ifelse(is.na(adpp_sum_t$TRTAN), -Inf, adpp_sum_t$TRTAN),
  ifelse(is.na(adpp_sum_t$PPSDY), -Inf, adpp_sum_t$PPSDY),
  adpp_sum_t$s_order,
  ifelse(is.na(adpp_sum_t$`_PARAM`), "\001", adpp_sum_t$`_PARAM`),
  method="radix"), ]

trtan_fmt <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    nv <- suppressWarnings(as.numeric(trimws(as.character(v))))
    if (!is.na(nv) && nv==1) return("Period 1 Treatment")
    if (!is.na(nv) && nv==2) return("Period 2 Treatment")
    if (!is.na(nv) && nv==3) return("Period 3 Treatment")
    return(trimws(as.character(v)))
  }, USE.NAMES=FALSE)
}
statg_fmt <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    if (v==1) return("n")
    if (v==2) return("Mean (SD)")
    if (v==3) return("CV% mean")
    if (v==4) return("Geo-mean")
    if (v==5) return("CV% geo-mean")
    if (v==6) return("Median")
    if (v==7) return("[Min; Max]        ")
    return(as.character(v))
  }, USE.NAMES=FALSE)
}
trunc10 <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; substr(x,1,10) }
trunc15 <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; substr(x,1,15) }

by_keys <- unique(adpp_sum_t[, c("ANLPRNT","PPSPECL","PPCATL")])
by_keys <- by_keys[order(ifelse(is.na(by_keys$ANLPRNT),"\001",by_keys$ANLPRNT),
                         ifelse(is.na(by_keys$PPSPECL),"\001",by_keys$PPSPECL),
                         ifelse(is.na(by_keys$PPCATL),"\001",by_keys$PPCATL), method="radix"), , drop=FALSE]

param_order <- sort(unique(adpp_sum_t$`_PARAM`))

pad_left <- function(s, w) {
  s <- as.character(s); s[is.na(s)] <- ""
  sapply(s, function(v) {
    nb <- nchar(v, type="bytes")
    if (nb < w) paste0(v, strrep(" ", w-nb)) else v
  }, USE.NAMES=FALSE)
}

wide_all <- adpp_sum_t %>%
  select(ANLPRNT,PPSPECL,PPCATL,TRTAN,PPSDY,s_order,`_PARAM`,value) %>%
  tidyr::pivot_wider(names_from=`_PARAM`, values_from=value, values_fill=" ") %>%
  as.data.frame(check.names=FALSE)
front <- c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","s_order")
other_cols <- setdiff(names(wide_all), front)
ordered_params <- param_order[param_order %in% other_cols]
extra <- setdiff(other_cols, ordered_params)
wide_all <- wide_all[, c(front, ordered_params, extra)]
wide_all <- wide_all[order(
  ifelse(is.na(wide_all$ANLPRNT),"\001",wide_all$ANLPRNT),
  ifelse(is.na(wide_all$PPSPECL),"\001",wide_all$PPSPECL),
  ifelse(is.na(wide_all$PPCATL),"\001",wide_all$PPCATL),
  ifelse(is.na(wide_all$TRTAN),-Inf,wide_all$TRTAN),
  ifelse(is.na(wide_all$PPSDY),-Inf,wide_all$PPSDY),
  wide_all$s_order, method="radix"), ]
wide_all$Statistic <- statg_fmt(wide_all$s_order)
wide_all$TRTAN_c <- trtan_fmt(wide_all$TRTAN)
csv_body <- wide_all[, c("ANLPRNT","PPSPECL","PPCATL","TRTAN","TRTAN_c","PPSDY","s_order","Statistic", ordered_params)]

titles <- c("Study001",
            "Table 14.2-PPTS (Page xxx of xxx)",
            "Summary statistics for PK parameters",
            "by compound, matrix, analyte and actual treatment",
            "Analysis Set: PK analysis set")
footnotes <- c("CV% = coefficient of variation (%) = sd/mean*100",
               "CV% geo-mean = (sqrt(exp(variance for log transformed data)-1))*100",
               "Geo-mean: Geometric mean",
               "Geo-mean and CV% geo-mean not presented when the minimum value for a parameter is zero.",
               "Data: adpp  Program: pps.sas  Output: t.14.2.ppts.txt",
               "Fake Data/ Production Run on 17MAR2014:23:57")

W_TRT <- 20; W_PPSDY <- 8; W_STAT <- 14; W_VAL <- 19
header_trt <- pad_left("Actual treatment", W_TRT)
header_day <- pad_left("Period day", W_PPSDY)
header_stat <- pad_left("Statistic", W_STAT)
header_params <- sapply(ordered_params, function(p) pad_left(p, W_VAL), USE.NAMES=FALSE)
header_line <- paste(c(header_trt, header_day, header_stat, header_params), collapse=" ")
sep_line <- paste(c(pad_left(strrep("_", min(nchar("Actual treatment"),W_TRT)),W_TRT),
                    pad_left(strrep("_", min(nchar("Period day"),W_PPSDY)),W_PPSDY),
                    pad_left(strrep("_", min(nchar("Statistic"),W_STAT)),W_STAT),
                    sapply(ordered_params, function(p) strrep("_", W_VAL), USE.NAMES=FALSE)), collapse=" ")

txt <- c()
txt <- c(txt, titles, "")
for (b in seq_len(nrow(by_keys))) {
  anl <- by_keys$ANLPRNT[b]; spe <- by_keys$PPSPECL[b]; catv <- by_keys$PPCATL[b]
  comp_line <- paste0(" Compound: ", trunc10(anl), " , Matrix: ", trunc10(spe), " , Analyte: ", trunc15(catv))
  txt <- c(txt, comp_line, "")
  txt <- c(txt, header_line)
  txt <- c(txt, sep_line)
  sub <- wide_all[!is.na(wide_all$ANLPRNT)==!is.na(anl) & (is.na(wide_all$ANLPRNT) & is.na(anl) | (!is.na(wide_all$ANLPRNT) & !is.na(anl) & wide_all$ANLPRNT==anl)) &
                  !is.na(wide_all$PPSPECL)==!is.na(spe) & (is.na(wide_all$PPSPECL) & is.na(spe) | (!is.na(wide_all$PPSPECL) & !is.na(spe) & wide_all$PPSPECL==spe)) &
                  !is.na(wide_all$PPCATL)==!is.na(catv) & (is.na(wide_all$PPCATL) & is.na(catv) | (!is.na(wide_all$PPCATL) & !is.na(catv) & wide_all$PPCATL==catv)), , drop=FALSE]
  for (r in seq_len(nrow(sub))) {
    cur_trt <- sub$TRTAN[r]; cur_day <- sub$PPSDY[r]
    disp_trt <- trtan_fmt(cur_trt)
    disp_day <- if (is.na(cur_day)) "" else trimws(as.character(cur_day))
    disp_stat <- statg_fmt(sub$s_order[r])
    c_trt <- pad_left(disp_trt, W_TRT)
    c_day <- pad_left(disp_day, W_PPSDY)
    c_stat <- pad_left(disp_stat, W_STAT)
    vals <- sapply(ordered_params, function(p) {
      v <- sub[r, p]
      if (is.na(v)) v <- ""
      if (trimws(as.character(v))=="" ) v <- ""
      else v <- as.character(v)
      nb <- nchar(v, type="bytes")
      if (nb < W_VAL) paste0(v, strrep(" ", W_VAL-nb)) else v
    }, USE.NAMES=FALSE)
    line <- paste(c(c_trt, c_day, c_stat, vals), collapse=" ")
    txt <- c(txt, line)
    is_last_of_day <- FALSE
    if (r==nrow(sub)) is_last_of_day <- TRUE
    else {
      nxt_trt <- sub$TRTAN[r+1]; nxt_day <- sub$PPSDY[r+1]
      same_trt_n <- (!is.na(cur_trt) && !is.na(nxt_trt) && cur_trt==nxt_trt) || (is.na(cur_trt) && is.na(nxt_trt))
      same_day_n <- (!is.na(cur_day) && !is.na(nxt_day) && cur_day==nxt_day) || (is.na(cur_day) && is.na(nxt_day))
      if (!(same_trt_n && same_day_n)) is_last_of_day <- TRUE
    }
    if (is_last_of_day) txt <- c(txt, "")
  }
  txt <- c(txt, "")
}
txt <- c(txt, footnotes)

writeLines(txt, file.path(out, "P035.txt"))
writeLines(txt, file.path(out, "P035.lst"))

write.csv(csv_body, file.path(out, "P035.csv"), row.names=FALSE, na="")

write.csv(adpp_sum, file.path(out, "adpp_sum.csv"), row.names=FALSE, na="")
out_long <- adpp_sum_t
names(out_long)[names(out_long)=="_PARAM"] <- "PARAM"
write.csv(out_long, file.path(out, "adpp_sum_t.csv"), row.names=FALSE, na="")
write.csv(csv_body, file.path(out, "CLXCHECKNOBS.csv"), row.names=FALSE, na="")

suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_collate), silent=TRUE))
```
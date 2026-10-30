```r
# P059 pk_param_summary translation
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam_env <- Sys.getenv("RAWADAM")
root_env <- Sys.getenv("ROOT")
cands <- c(
  if (rawadam_env != "") file.path(rawadam_env, "adpp.xpt"),
  if (root_env != "") file.path(root_env, "data", "raw", "cdisc-pilot", "adam", "adpp.xpt"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.xpt",
  file.path(getwd(), "adpp.xpt")
)
adpp_path <- NULL
for (p in cands) {
  if (!is.na(p) && p != "" && file.exists(p)) { adpp_path <- p; break }
}
if (is.null(adpp_path)) {
  adpp_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.xpt"
}
cat("Reading:", adpp_path, "\n")

library(haven)
library(dplyr)
library(tidyr)

adpp_raw <- haven::read_xpt(adpp_path)
names(adpp_raw) <- toupper(names(adpp_raw))
cat("N rows raw:", nrow(adpp_raw), "\n")

sas_put <- function(x, w, d=NULL) {
  sapply(x, function(v) {
    if (is.na(v) || is.nan(v) || !is.finite(v)) {
      if (w <= 1) return(".")
      return(paste0(strrep(" ", w-1), "."))
    }
    if (is.null(d) || is.na(d)) {
      r <- sign(v)*floor(abs(v)+0.5)
      if (r==0) r <- 0
      s <- sprintf("%.0f", r)
      if (nchar(s) < w) s <- paste0(strrep(" ", w-nchar(s)), s)
      return(s)
    } else {
      m <- 10^d
      r <- sign(v)*floor(abs(v)*m+0.5)/m
      if (r==0) r <- 0
      fmt <- paste0("%.", d, "f")
      s <- sprintf(fmt, r)
      if (r==0) s <- sprintf(fmt, 0)
      if (nchar(s) < w) s <- paste0(strrep(" ", w-nchar(s)), s)
      return(s)
    }
  }, USE.NAMES=FALSE)
}
sas_index <- function(s, sub) {
  sapply(seq_along(s), function(i) {
    if (is.na(s[i])) return(0L)
    p <- regexpr(sub, s[i], fixed=TRUE)[1]
    if (p < 0) return(0L)
    return(as.integer(p))
  })
}
sas_compress <- function(s) gsub(" ", "", s, fixed=TRUE)
sas_compbl <- function(s) gsub(" {2,}", " ", s)
sas_strip <- function(s) trimws(s)

pk_map <- c(AUCINFO="AUC", AUCTAU="AUCtau", AUCLAST="AUC(0-tlast)",
            CMAX="Cmax", CMAXD="Cmax/Dose", MRTLAST="MRT", TLAST="tlast",
            TMAX="tmax", CLRFO="CL/F", VZFO="VzF", LAMZ="lambdaZ",
            LAMZHL="t1/2", CLAST="Clast")
put_pk <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    v2 <- trimws(as.character(v))
    if (v2 %in% names(pk_map)) return(pk_map[[v2]])
    return(v2)
  }, USE.NAMES=FALSE)
}
trtan_fmt <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    if (v==1) return("Period 1 Treatment")
    if (v==2) return("Period 2 Treatment")
    if (v==3) return("Period 3 Treatment")
    return(as.character(v))
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

paramcd_col <- if ("PARAMCD" %in% names(adpp_raw)) "PARAMCD" else "PPTESTCD"
pptestcd_col <- if ("PPTESTCD" %in% names(adpp_raw)) "PPTESTCD" else paramcd_col
stopifnot(!is.na(paramcd_col))
df <- adpp_raw
getcol <- function(nm) if (nm %in% names(df)) df[[nm]] else rep(NA, nrow(df))

PKFN <- suppressWarnings(as.numeric(getcol("PKFN")))
ANL01FN <- suppressWarnings(as.numeric(getcol("ANL01FN")))
PARAMCDv <- as.character(getcol(paramcd_col))
PPTESTCDv <- as.character(getcol(pptestcd_col))
PPSTRESN <- suppressWarnings(as.numeric(getcol("PPSTRESN")))
PPCATLv <- as.character(getcol("PPCATL"))
AVALv <- suppressWarnings(as.numeric(getcol("AVAL")))
ANLPRNTv <- as.character(getcol("ANLPRNT"))
PPSPECLv <- as.character(getcol("PPSPECL"))
TRTANv <- suppressWarnings(as.numeric(getcol("TRTAN")))
PPSDYv <- suppressWarnings(as.numeric(getcol("PPSDY")))
PPORRESUv <- as.character(getcol("PPORRESU"))
PPORRESUv[is.na(PPORRESUv)] <- ""

keep <- rep(TRUE, nrow(df))
keep <- keep & (!is.na(PKFN) & PKFN==1)
keep <- keep & (is.na(ANL01FN) | ANL01FN != 1)
keep <- keep & (toupper(trimws(PARAMCDv)) %in% c('AUCINFO','AUCLAST','CMAX','TMAX','MRTLAST','LAMZHL'))
keep <- keep & (!is.na(PPSTRESN))
comp <- sapply(PPCATLv, function(v) if (is.na(v)) "" else sas_compress(v))
keep <- keep & (comp=='ANAL2')

pkpart <- put_pk(PPTESTCDv)
param_raw <- paste0(pkpart, ' (' , sas_strip(PPORRESUv), ')')
PARAM_lab <- sas_compbl(param_raw)
PARAM_lab <- trimws(PARAM_lab)

adpp <- data.frame(
  ANLPRNT = ANLPRNTv,
  PPSPECL = PPSPECLv,
  PPCATL = PPCATLv,
  TRTAN = TRTANv,
  PPSDY = PPSDYv,
  `_PARAM` = PARAM_lab,
  AVAL = AVALv,
  stringsAsFactors = FALSE, check.names=FALSE
)
adpp <- adpp[keep, , drop=FALSE]
adpp$AVAL_log <- sapply(adpp$AVAL, function(v) {
  if (is.na(v) || v <= 0) return(NA_real_)
  log(v)
})
cat("N rows adpp filtered:", nrow(adpp), "\n")

ord <- order(adpp$ANLPRNT, adpp$PPSPECL, adpp$PPCATL, adpp$TRTAN, adpp$PPSDY, adpp$`_PARAM`, na.last=TRUE)
adpp <- adpp[ord, , drop=FALSE]

grp_keys <- c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","_PARAM")
stat_one <- function(x) {
  n <- sum(!is.na(x))
  if (n==0) return(list(n=0, mean=NA_real_, std=NA_real_, cv=NA_real_, median=NA_real_, min=NA_real_, max=NA_real_))
  mn <- mean(x, na.rm=TRUE)
  s <- if (n>1) sd(x, na.rm=TRUE) else NA_real_
  cv <- if (!is.na(mn) && !is.na(s) && mn!=0) s/mn*100 else NA_real_
  med <- median(x, na.rm=TRUE)
  mi <- min(x, na.rm=TRUE)
  ma <- max(x, na.rm=TRUE)
  list(n=n, mean=mn, std=s, cv=cv, median=med, min=mi, max=ma)
}

uniq <- unique(adpp[, grp_keys, drop=FALSE])
uniq <- uniq[order(uniq$ANLPRNT, uniq$PPSPECL, uniq$PPCATL, uniq$TRTAN, uniq$PPSDY, uniq$`_PARAM`, na.last=TRUE), , drop=FALSE]
n1v<-n2v<-mn1v<-mn2v<-std1v<-std2v<-cv1v<-cv2v<-med1v<-med2v<-min1v<-min2v<-max1v<-max2v<-rep(NA_real_, nrow(uniq))
for (i in seq_len(nrow(uniq))) {
  sel <- rep(TRUE, nrow(adpp))
  for (k in grp_keys) {
    a <- adpp[[k]]; b <- uniq[i,k]
    if (is.na(b)) sel <- sel & is.na(a) else {
      sel <- sel & (!is.na(a) & a==b)
    }
  }
  sub <- adpp[sel, , drop=FALSE]
  s1 <- stat_one(sub$AVAL)
  s2 <- stat_one(sub$AVAL_log)
  n1v[i]<-s1$n; mn1v[i]<-s1$mean; std1v[i]<-s1$std; cv1v[i]<-s1$cv; med1v[i]<-s1$median; min1v[i]<-s1$min; max1v[i]<-s1$max
  n2v[i]<-s2$n; mn2v[i]<-s2$mean; std2v[i]<-s2$std; cv2v[i]<-s2$cv; med2v[i]<-s2$median; min2v[i]<-s2$min; max2v[i]<-s2$max
}
adpp_sum <- cbind(uniq, data.frame(n1=n1v,n2=n2v,mn1=mn1v,mn2=mn2v,std1=std1v,std2=std2v,cv1=cv1v,cv2=cv2v,med1=med1v,med2=med2v,min1=min1v,min2=min2v,max1=max1v,max2=max2v, stringsAsFactors=FALSE))
cat("adpp_sum rows:", nrow(adpp_sum), "\n")

trim_left <- function(s) sub("^ +", "", s)
rows_list <- list()
for (i in seq_len(nrow(adpp_sum))) {
  r <- adpp_sum[i,]
  par <- r$`_PARAM`
  isAUC <- sas_index(par, "AUC")>0
  isCmax <- sas_index(par, "Cmax")>0
  isMRT <- sas_index(par, "MRT")>0
  isHalf <- sas_index(par, "t1/2")>0
  isTmax <- sas_index(par, "tmax")>0
  n1 <- r$n1; mn1<-r$mn1; std1<-r$std1; cv1<-r$cv1; med1<-r$med1; min1<-r$min1; max1<-r$max1
  mn2<-r$mn2; std2<-r$std2
  # s_order 1: put(n1,2.)
  v1_raw <- sas_put(n1, 2)
  v1 <- trimws(v1_raw)
  if (v1=="") v1 <- " "
  # s_order 2
  if (isAUC) v2 <- trim_left(paste0(sas_put(mn1,6), ' (' , sas_put(std1,5), ')'))
  else if (isCmax) v2 <- trim_left(paste0(sas_put(mn1,5), ' (' , sas_put(std1,4), ')'))
  else if (isMRT) v2 <- trim_left(paste0(sas_put(mn1,4,1), ' (' , sas_put(std1,4,2), ')'))
  else if (isHalf) v2 <- trim_left(paste0(sas_put(mn1,4,2), ' (' , sas_put(std1,4,2), ')'))
  else v2 <- ' '
  # s_order 3: put(cv1,4.1)
  if (!isTmax) {
    v3r <- sas_put(cv1,4,1)
    v3 <- trimws(v3r)
  } else v3 <- ' '
  # s_order 4 geo-mean
  gm <- if (!is.na(mn2)) exp(mn2) else NA_real_
  if (isAUC) v4 <- trimws(sas_put(gm,6))
  else if (isCmax) v4 <- trimws(sas_put(gm,5))
  else if (isMRT) v4 <- trimws(sas_put(gm,4,1))
  else if (isHalf) v4 <- trimws(sas_put(gm,4,2))
  else v4 <- ' '
  # s_order 5: put(sqrt(exp(std2**2)-1)*100,4.1)
  if (!isTmax) {
    gcv <- if (!is.na(std2)) sqrt(exp(std2^2)-1)*100 else NA_real_
    v5r <- sas_put(gcv,4,1)
    v5 <- trimws(v5r)
  } else v5 <- ' '
  # s_order 6 median
  if (isAUC) v6 <- trimws(sas_put(med1,6))
  else if (isCmax) v6 <- trimws(sas_put(med1,5))
  else if (isMRT) v6 <- trimws(sas_put(med1,4,1))
  else if (isHalf) v6 <- trimws(sas_put(med1,4,2))
  else if (isTmax) v6 <- trimws(sas_put(med1,4,2))
  else v6 <- ' '
  # s_order 7 [min; max]
  if (isAUC) v7 <- paste0('[', sas_put(min1,6), ';', sas_put(max1,6), ']')
  else if (isCmax) v7 <- paste0('[', sas_put(min1,4), ';', sas_put(max1,5), ']')
  else if (isMRT) v7 <- paste0('[', sas_put(min1,4,2), ';', sas_put(max1,4,1), ']')
  else if (isHalf) v7 <- paste0('[', sas_put(min1,4,2), ';', sas_put(max1,4,1), ']')
  else if (isTmax) v7 <- paste0('[', sas_put(min1,4,2), ';', sas_put(max1,4,2), ']')
  else v7 <- ' '
  vals <- list(v1,v2,v3,v4,v5,v6,v7)
  for (s in 1:7) {
    vv <- vals[[s]]
    if (is.na(vv) || vv=="") vv <- " "
    rows_list[[length(rows_list)+1]] <- data.frame(
      ANLPRNT=r$ANLPRNT, PPSPECL=r$PPSPECL, PPCATL=r$PPCATL,
      TRTAN=r$TRTAN, PPSDY=r$PPSDY, s_order=s, `_PARAM`=par, value=vv,
      stringsAsFactors=FALSE, check.names=FALSE
    )
  }
}
adpp_sum_t <- do.call(rbind, rows_list)
adpp_sum_t <- adpp_sum_t[order(adpp_sum_t$ANLPRNT, adpp_sum_t$PPSPECL, adpp_sum_t$PPCATL, adpp_sum_t$TRTAN, adpp_sum_t$PPSDY, adpp_sum_t$s_order, adpp_sum_t$`_PARAM`, na.last=TRUE), , drop=FALSE]
cat("adpp_sum_t rows:", nrow(adpp_sum_t), "\n")
print(head(adpp_sum_t, 42))

param_order <- sort(unique(adpp_sum_t$`_PARAM`))
print(param_order)
wide <- tidyr::pivot_wider(adpp_sum_t, names_from=`_PARAM`, values_from=value, values_fill=" ")
wide <- wide[, c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","s_order", param_order), drop=FALSE]
wide <- wide[order(wide$ANLPRNT, wide$PPSPECL, wide$PPCATL, wide$TRTAN, wide$PPSDY, wide$s_order), , drop=FALSE]
wide$Actual_treatment <- trtan_fmt(wide$TRTAN)
wide$Statistic <- statg_fmt(wide$s_order)

csv_df <- wide[, c("ANLPRNT","PPSPECL","PPCATL","TRTAN","Actual_treatment","PPSDY","s_order","Statistic", param_order), drop=FALSE]
write.csv(csv_df, file.path(out, "P059.csv"), row.names=FALSE, na="")
clx <- adpp_sum_t
clx$Actual_treatment <- trtan_fmt(clx$TRTAN)
clx$Statistic <- statg_fmt(clx$s_order)
write.csv(clx, file.path(out, "CLXCHECKNOBS.csv"), row.names=FALSE, na="")
write.csv(adpp_sum, file.path(out, "adpp_sum.csv"), row.names=FALSE, na="")

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

w_trt <- 20; w_day <- 8; w_stat <- 14; w_val <- 19
hdr_trt <- "Actual treatment"; hdr_day <- "Period day"; hdr_stat <- "Statistic"
lines <- c()
lines <- c(lines, titles, "")
by_groups <- unique(wide[, c("ANLPRNT","PPSPECL","PPCATL"), drop=FALSE])
lpad <- function(s, w) {
  s <- as.character(s); if (is.na(s)) s <- ""
  if (nchar(s) > w) s <- substr(s, 1, w)
  sprintf(paste0("%-", w, "s"), s)
}
for (bg in seq_len(nrow(by_groups))) {
  anl <- by_groups$ANLPRNT[bg]; mat <- by_groups$PPSPECL[bg]; ana <- by_groups$PPCATL[bg]
  comp_line <- paste0("Compound: ", trimws(as.character(anl)), ", Matrix: ", trimws(as.character(mat)), ", Analyte: ", trimws(as.character(ana)))
  lines <- c(lines, "", comp_line, "")
  sub <- wide[wide$ANLPRNT==anl & wide$PPSPECL==mat & wide$PPCATL==ana, , drop=FALSE]
  hdr <- paste0(lpad(hdr_trt, w_trt), " ", lpad(hdr_day, w_day), " ", lpad(hdr_stat, w_stat), " ",
                paste(sapply(param_order, function(p) lpad(p, w_val)), collapse=" "))
  lines <- c(lines, hdr)
  headline <- paste0(paste(rep("_", nchar(hdr)), collapse=""))
  lines <- c(lines, headline)
  for (i in seq_len(nrow(sub))) {
    trt_lab <- sub$Actual_treatment[i]
    is_first_trt <- (i==1) || (sub$TRTAN[i] != sub$TRTAN[i-1])
    is_first_day <- (i==1) || (sub$TRTAN[i] != sub$TRTAN[i-1]) || (is.na(sub$PPSDY[i]) != is.na(sub$PPSDY[i-1])) || (!is.na(sub$PPSDY[i]) && !is.na(sub$PPSDY[i-1]) && sub$PPSDY[i] != sub$PPSDY[i-1])
    disp_trt <- if (is_first_trt) trt_lab else ""
    disp_day <- if (is_first_day) (if (is.na(sub$PPSDY[i])) "" else as.character(sub$PPSDY[i])) else ""
    disp_stat <- sub$Statistic[i]
    vals <- sapply(param_order, function(p) {
      v <- sub[i, p]
      if (is.na(v)) v <- ""
      as.character(v)
    })
    rowline <- paste0(lpad(disp_trt, w_trt), " ", lpad(disp_day, w_day), " ", lpad(disp_stat, w_stat), " ",
                      paste(sapply(vals, function(v) lpad(v, w_val)), collapse=" "))
    rowline <- sub(" +$", "", rowline)
    lines <- c(lines, rowline)
    nxt_same <- FALSE
    if (i < nrow(sub)) {
      nxt_same <- (!is.na(sub$TRTAN[i+1]) && !is.na(sub$TRTAN[i]) && sub$TRTAN[i+1]==sub$TRTAN[i]) &&
        ((is.na(sub$PPSDY[i+1]) && is.na(sub$PPSDY[i])) || (!is.na(sub$PPSDY[i+1]) && !is.na(sub$PPSDY[i]) && sub$PPSDY[i+1]==sub$PPSDY[i]))
    }
    if (!nxt_same) lines <- c(lines, "")
  }
}
lines <- c(lines, "", footnotes)
con <- file(file.path(out, "P059.txt"), open="wt")
writeLines(lines, con)
close(con)
con2 <- file(file.path(out, "P059.lst"), open="wt")
writeLines(lines, con2)
close(con2)
cat("Wrote", file.path(out, "P059.txt"), "lines:", length(lines), "\n")
txt_all <- paste(lines, collapse=" ")
m <- gregexpr("[0-9]+\\.?[0-9]*", txt_all)
cat("numbers in text:", length(regmatches(txt_all, m)[[1]]), "\n")
```
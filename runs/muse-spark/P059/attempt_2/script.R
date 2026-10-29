# P059 pk_param_summary - R translation (attempt 2)
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.xpt"
alt1 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpp.xpt"
xpt_file <- rawadam_path
if (!file.exists(xpt_file) && file.exists(alt1)) xpt_file <- alt1
if (!file.exists(xpt_file)) {
  cand <- list.files("D:/meta_hackathon/sas2r-agent/data", pattern = "^adpp\\.xpt$",
                     recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) xpt_file <- cand[1]
}
message("Reading: ", xpt_file)
adpp_raw0 <- haven::read_xpt(xpt_file)
# drop labels/formats -> plain vectors (SAS values)
adpp_raw0 <- haven::zap_labels(adpp_raw0)
adpp_raw0 <- haven::zap_formats(adpp_raw0)
adpp_raw <- as.data.frame(adpp_raw0, stringsAsFactors = FALSE)
names(adpp_raw) <- toupper(names(adpp_raw))

sas_round_half_up <- function(x, d) {
  res <- x
  idx <- !is.na(x) & is.finite(x)
  xx <- x[idx]
  p <- 10^d
  res[idx] <- sign(xx) * floor(abs(xx) * p + 0.5 + 1e-9) / p
  res[idx & res == 0] <- 0
  res
}
sas_put <- function(x, w, d) {
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (length(xi)==0 || is.na(xi) || is.nan(xi) || !is.finite(xi)) {
      if (w <= 1) return(".")
      return(paste0(strrep(" ", w-1), "."))
    }
    r <- sas_round_half_up(xi, d)
    if (r==0) r <- 0
    if (d==0) s <- formatC(r, format="f", digits=0, flag="")
    else s <- formatC(r, format="f", digits=d, flag="")
    if (grepl("^-0(\\.0+)?$", s)) s <- sub("^-","",s)
    nch <- nchar(s, type="bytes")
    if (nch < w) s <- paste0(strrep(" ", w-nch), s)
    s
  }, character(1), USE.NAMES=FALSE)
}
sas_index <- function(string, pat) {
  vapply(seq_along(string), function(i) {
    s <- string[i]; p <- pat[i]
    if (is.na(s) || is.na(p)) return(0L)
    pos <- regexpr(p, s, fixed=TRUE)[1]
    if (pos<0) 0L else as.integer(pos)
  }, integer(1))
}
sas_compress <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return("")
    gsub("[[:space:]]","","",as.character(s))
  }, character(1), USE.NAMES=FALSE)
}
sas_strip <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return("")
    trimws(as.character(s))
  }, character(1), USE.NAMES=FALSE)
}
sas_compbl <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s2 <- gsub("[[:space:]]+"," ",as.character(s))
    trimws(s2)
  }, character(1), USE.NAMES=FALSE)
}
# C-locale (SAS byte) ordering for character vectors
sas_sort_order <- function(x) order(x, method="radix", na.last=FALSE)

pk_map <- c(AUCINFO="AUC", AUCTAU="AUCtau", AUCLAST="AUC(0-tlast)",
            CMAX="Cmax", CMAXD="Cmax/Dose", MRTLAST="MRT", TLAST="tlast",
            TMAX="tmax", CLRFO="CL/F", VZFO="VzF", LAMZ="lambdaZ",
            LAMZHL="t1/2", CLAST="Clast")
trtan_fmt <- c("1"="Period 1 Treatment","2"="Period 2 Treatment","3"="Period 3 Treatment")
statg_fmt <- c("1"="n","2"="Mean (SD)","3"="CV% mean","4"="Geo-mean",
               "5"="CV% geo-mean","6"="Median","7"="[Min; Max]")

df <- adpp_raw
for (cc in c("PKFN","ANL01FN","PPSTRESN","AVAL","TRTAN","PPSDY")) {
  if (cc %in% names(df)) df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
}
# character vars: keep as character, preserve for display but trim trailing for logic
charvars <- c("PARAMCD","PPTESTCD","PPCATL","PPORRESU","ANLPRNT","PPSPECL")
for (cc in charvars) if (cc %in% names(df)) df[[cc]] <- as.character(df[[cc]])

# filter column: SAS uses PARAMCD
filter_col <- if ("PARAMCD" %in% names(df)) "PARAMCD" else "PPTESTCD"
wanted <- c('AUCINFO','AUCLAST','CMAX','TMAX','MRTLAST','LAMZHL')
filt_raw <- df[[filter_col]]
filt_trim <- sas_strip(filt_raw)

keep <- !is.na(df$PKFN) & df$PKFN==1
anl01 <- df$ANL01FN
keep <- keep & (is.na(anl01) | anl01!=1)
keep <- keep & (!is.na(filt_trim) & filt_trim %in% wanted)
keep <- keep & (!is.na(df$PPSTRESN))
keep <- keep & (sas_compress(df$PPCATL)=="ANAL2")
keep[is.na(keep)] <- FALSE
adpp <- df[keep, , drop=FALSE]

# _PARAM = compbl(put(PPTESTCD,$pk.)||' ('||strip(PPORRESU)||')')
ppcode_trim <- sas_strip(adpp$PPTESTCD)
mapped <- vapply(ppcode_trim, function(v) {
  if (is.na(v) || v=="") return(ifelse(is.na(v),"",v))
  if (v %in% names(pk_map)) pk_map[[v]] else v
}, character(1), USE.NAMES=FALSE)
unit <- sas_strip(adpp$PPORRESU)
param_raw <- paste0(mapped, " (", unit, ")")
adpp[["_PARAM"]] <- sas_compbl(param_raw)
adpp$AVAL <- suppressWarnings(as.numeric(adpp$AVAL))
adpp$AVAL_log <- ifelse(!is.na(adpp$AVAL) & adpp$AVAL>0, log(adpp$AVAL), NA_real_)

# normalize BY character vars: SAS ignores trailing blanks; trim them for grouping/display
for (cc in c("ANLPRNT","PPSPECL","PPCATL")) {
  adpp[[cc]] <- sas_strip(adpp[[cc]])
  adpp[[cc]][adpp[[cc]]==""] <- NA_character_
}
# sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM (missing first, byte order)
o <- order(adpp$ANLPRNT, adpp$PPSPECL, adpp$PPCATL, adpp$TRTAN, adpp$PPSDY, adpp[["_PARAM"]],
           method="radix", na.last=FALSE)
adpp <- adpp[o,,drop=FALSE]

byvars <- c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","_PARAM")
uniq_df <- unique(adpp[, byvars, drop=FALSE])
uo <- order(uniq_df$ANLPRNT, uniq_df$PPSPECL, uniq_df$PPCATL, uniq_df$TRTAN, uniq_df$PPSDY, uniq_df[["_PARAM"]],
            method="radix", na.last=FALSE)
uniq_df <- uniq_df[uo,,drop=FALSE]

eqv <- function(a,b) {
  if (is.na(b)) is.na(a) else (!is.na(a) & a==b)
}
summ_list <- vector("list", nrow(uniq_df))
for (i in seq_len(nrow(uniq_df))) {
  sel <- rep(TRUE, nrow(adpp))
  for (bv in byvars) sel <- sel & eqv(adpp[[bv]], uniq_df[i,bv][[1]])
  x1 <- adpp$AVAL[sel]; x1 <- x1[!is.na(x1)]
  x2 <- adpp$AVAL_log[sel]; x2 <- x2[!is.na(x2)]
  n1 <- length(x1); n2 <- length(x2)
  mn1 <- if (n1>0) mean(x1) else NA_real_
  mn2 <- if (n2>0) mean(x2) else NA_real_
  std1 <- if (n1>1) stats::sd(x1) else NA_real_
  std2 <- if (n2>1) stats::sd(x2) else NA_real_
  cv1 <- if (!is.na(std1) && !is.na(mn1) && mn1!=0) 100*std1/mn1 else NA_real_
  cv2 <- if (!is.na(std2) && !is.na(mn2) && mn2!=0) 100*std2/mn2 else NA_real_
  med1 <- if (n1>0) stats::median(x1) else NA_real_
  med2 <- if (n2>0) stats::median(x2) else NA_real_
  min1 <- if (n1>0) min(x1) else NA_real_
  max1 <- if (n1>0) max(x1) else NA_real_
  min2 <- if (n2>0) min(x2) else NA_real_
  max2 <- if (n2>0) max(x2) else NA_real_
  summ_list[[i]] <- data.frame(uniq_df[i,,drop=FALSE],
    n1=n1,n2=n2,mn1=mn1,mn2=mn2,std1=std1,std2=std2,cv1=cv1,cv2=cv2,
    med1=med1,med2=med2,min1=min1,min2=min2,max1=max1,max2=max2,
    stringsAsFactors=FALSE, check.names=FALSE)
}
adpp_sum <- do.call(rbind, summ_list)
rownames(adpp_sum) <- NULL

out_rows <- vector("list", nrow(adpp_sum)*7)
k <- 1
for (i in seq_len(nrow(adpp_sum))) {
  r <- adpp_sum[i,]
  par <- r[["_PARAM"]][[1]]
  hasAUC <- sas_index(par,"AUC")>0
  hasCmax <- sas_index(par,"Cmax")>0
  hasMRT <- sas_index(par,"MRT")>0
  hasHalf <- sas_index(par,"t1/2")>0
  hasTmax <- sas_index(par,"tmax")>0
  v1 <- sas_put(r$n1,2,0)
  if (hasAUC) v2 <- paste0(sas_put(r$mn1,6,0)," (",sas_put(r$std1,5,0),")")
  else if (hasCmax) v2 <- paste0(sas_put(r$mn1,5,0)," (",sas_put(r$std1,4,0),")")
  else if (hasMRT) v2 <- paste0(sas_put(r$mn1,4,1)," (",sas_put(r$std1,4,2),")")
  else if (hasHalf) v2 <- paste0(sas_put(r$mn1,4,2)," (",sas_put(r$std1,4,2),")")
  else v2 <- " "
  if (!hasTmax) v3 <- sas_put(r$cv1,4,1) else v3 <- " "
  if (hasAUC) v4 <- sas_put(exp(r$mn2),6,0)
  else if (hasCmax) v4 <- sas_put(exp(r$mn2),5,0)
  else if (hasMRT) v4 <- sas_put(exp(r$mn2),4,1)
  else if (hasHalf) v4 <- sas_put(exp(r$mn2),4,2)
  else v4 <- " "
  if (!hasTmax) {
    gv <- if (is.na(r$std2)) NA_real_ else sqrt(exp(r$std2^2)-1)*100
    v5 <- sas_put(gv,4,1)
  } else v5 <- " "
  if (hasAUC) v6 <- sas_put(r$med1,6,0)
  else if (hasCmax) v6 <- sas_put(r$med1,5,0)
  else if (hasMRT) v6 <- sas_put(r$med1,4,1)
  else if (hasHalf) v6 <- sas_put(r$med1,4,2)
  else if (hasTmax) v6 <- sas_put(r$med1,4,2)
  else v6 <- " "
  if (hasAUC) v7 <- paste0("[",sas_put(r$min1,6,0),";",sas_put(r$max1,6,0),"]")
  else if (hasCmax) v7 <- paste0("[",sas_put(r$min1,4,0),";",sas_put(r$max1,5,0),"]")
  else if (hasMRT) v7 <- paste0("[",sas_put(r$min1,4,2),";",sas_put(r$max1,4,1),"]")
  else if (hasHalf) v7 <- paste0("[",sas_put(r$min1,4,2),";",sas_put(r$max1,4,1),"]")
  else if (hasTmax) v7 <- paste0("[",sas_put(r$min1,4,2),";",sas_put(r$max1,4,2),"]")
  else v7 <- " "
  vals <- list(v1,v2,v3,v4,v5,v6,v7)
  vals <- lapply(vals, function(v) substr(v,1,30))
  for (s in 1:7) {
    out_rows[[k]] <- data.frame(ANLPRNT=r$ANLPRNT,PPSPECL=r$PPSPECL,PPCATL=r$PPCATL,
      TRTAN=r$TRTAN,PPSDY=r$PPSDY,`_PARAM`=r[["_PARAM"]],
      s_order=s,value=vals[[s]],stringsAsFactors=FALSE,check.names=FALSE)
    k <- k+1
  }
}
adpp_sum_t <- do.call(rbind, out_rows)
names(adpp_sum_t)[names(adpp_sum_t)=="X_PARAM"] <- "_PARAM"
if (!"_PARAM" %in% names(adpp_sum_t)) {
  pc <- grep("PARAM",names(adpp_sum_t),value=TRUE)[1]
  names(adpp_sum_t)[names(adpp_sum_t)==pc] <- "_PARAM"
}
o2 <- order(adpp_sum_t$ANLPRNT,adpp_sum_t$PPSPECL,adpp_sum_t$PPCATL,
            adpp_sum_t$TRTAN,adpp_sum_t$PPSDY,adpp_sum_t$s_order,adpp_sum_t[["_PARAM"]],
            method="radix",na.last=FALSE)
adpp_sum_t <- adpp_sum_t[o2,,drop=FALSE]
rownames(adpp_sum_t) <- NULL

trtan_label <- function(x) {
  vapply(as.character(x), function(v) {
    if (is.na(v)) return("")
    v <- trimws(v)
    # numeric like "1" / "1.0"
    vn <- suppressWarnings(as.numeric(v))
    key <- if (!is.na(vn) && vn==floor(vn)) as.character(as.integer(vn)) else v
    if (key %in% names(trtan_fmt)) trtan_fmt[[key]] else v
  }, character(1), USE.NAMES=FALSE)
}
stat_label <- function(x) {
  vapply(as.character(x), function(v) {
    if (v %in% names(statg_fmt)) trimws(statg_fmt[[v]]) else v
  }, character(1), USE.NAMES=FALSE)
}

by_df <- unique(adpp_sum_t[,c("ANLPRNT","PPSPECL","PPCATL"),drop=FALSE])
bo <- order(by_df$ANLPRNT,by_df$PPSPECL,by_df$PPCATL,method="radix",na.last=FALSE)
by_df <- by_df[bo,,drop=FALSE]

wide_list <- list()
txt <- c("Study001",
  "Table 14.2-PPTS (Page xxx of xxx)",
  "Summary statistics for PK parameters",
  "by compound, matrix, analyte and actual treatment",
  "Analysis Set: PK analysis set",
  "")
for (b in seq_len(nrow(by_df))) {
  ba <- by_df[b,]
  selb <- eqv(adpp_sum_t$ANLPRNT,ba$ANLPRNT[[1]]) & eqv(adpp_sum_t$PPSPECL,ba$PPSPECL[[1]]) & eqv(adpp_sum_t$PPCATL,ba$PPCATL[[1]])
  sub <- adpp_sum_t[selb,,drop=FALSE]
  cols <- unique(sub[["_PARAM"]])
  cols <- cols[sas_sort_order(cols)]
  rows <- unique(sub[,c("TRTAN","PPSDY","s_order"),drop=FALSE])
  ro <- order(rows$TRTAN,rows$PPSDY,rows$s_order,method="radix",na.last=FALSE)
  rows <- rows[ro,,drop=FALSE]
  txt <- c(txt, paste0("Compound: ", ifelse(is.na(ba$ANLPRNT[[1]]),"",ba$ANLPRNT[[1]]),
    " , Matrix: ", ifelse(is.na(ba$PPSPECL[[1]]),"",ba$PPSPECL[[1]]),
    " , Analyte: ", ifelse(is.na(ba$PPCATL[[1]]),"",ba$PPCATL[[1]])), "")
  hdr <- paste(c("Actual treatment","Period day","Statistic",cols),collapse=" | ")
  txt <- c(txt, hdr, paste(rep("-",nchar(hdr,type="bytes")),collapse=""))
  for (ri in seq_len(nrow(rows))) {
    rr <- rows[ri,]
    vals <- vapply(cols, function(cc) {
      m <- sub[eqv(sub$TRTAN,rr$TRTAN[[1]]) & eqv(sub$PPSDY,rr$PPSDY[[1]]) & sub$s_order==rr$s_order[[1]] & sub[["_PARAM"]]==cc,"value"]
      if (length(m)==0) "" else m[1]
    }, character(1), USE.NAMES=FALSE)
    trt_lab <- trtan_label(rr$TRTAN[[1]])
    ppsdy_disp <- if (is.na(rr$PPSDY[[1]])) "" else as.character(rr$PPSDY[[1]])
    stat_disp <- stat_label(rr$s_order[[1]])
    txt <- c(txt, paste(c(trt_lab,ppsdy_disp,stat_disp,vals),collapse=" | "))
    wrow <- data.frame(ANLPRNT=ba$ANLPRNT[[1]],PPSPECL=ba$PPSPECL[[1]],PPCATL=ba$PPCATL[[1]],
      TRTAN=rr$TRTAN[[1]],TRTAN_LABEL=trt_lab,PPSDY=rr$PPSDY[[1]],s_order=rr$s_order[[1]],
      Statistic=stat_disp,stringsAsFactors=FALSE,check.names=FALSE)
    for (j in seq_along(cols)) wrow[[cols[j]]] <- vals[j]
    wide_list[[length(wide_list)+1]] <- wrow
    if (ri < nrow(rows)) {
      nr <- rows[ri+1,]
      sameP <- (is.na(rr$PPSDY[[1]]) && is.na(nr$PPSDY[[1]])) || (!is.na(rr$PPSDY[[1]]) && !is.na(nr$PPSDY[[1]]) && rr$PPSDY[[1]]==nr$PPSDY[[1]])
      sameT <- (is.na(rr$TRTAN[[1]]) && is.na(nr$TRTAN[[1]])) || (!is.na(rr$TRTAN[[1]]) && !is.na(nr$TRTAN[[1]]) && rr$TRTAN[[1]]==nr$TRTAN[[1]])
      if (!sameP || !sameT) txt <- c(txt, "")
    }
  }
  txt <- c(txt, "")
}
txt <- c(txt,
  "CV% = coefficient of variation (%) = sd/mean*100",
  "CV% geo-mean = (sqrt(exp(variance for log transformed data)-1))*100",
  "Geo-mean: Geometric mean",
  "Geo-mean and CV% geo-mean not presented when the minimum value for a parameter is zero.",
  "Data: adpp  Program: pps.sas  Output: t.14.2.ppts.txt",
  "Fake Data/ Production Run on 17MAR2014:23:57")
wide_df <- do.call(rbind, wide_list)

writeLines(txt, file.path(out,"P059.txt"))
writeLines(txt, file.path(out,"P059.lst"))
write.csv(wide_df, file.path(out,"P059.csv"), row.names=FALSE, na="")
out_long <- adpp_sum_t
names(out_long)[names(out_long)=="_PARAM"] <- "PARAM_DISPLAY"
write.csv(out_long, file.path(out,"adpp_sum_t.csv"), row.names=FALSE, na="")
write.csv(adpp_sum, file.path(out,"adpp_sum.csv"), row.names=FALSE, na="")
message("Done. Rows wide=",nrow(wide_df)," long=",nrow(adpp_sum_t))

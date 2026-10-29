# P059 pk_param_summary - R translation
# Reads ADPP (XPT), reproduces PK param summary statistics table

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam_path <- "/bench/data/raw/cdisc-pilot/adam/adpp.xpt"
alt1 <- "/bench/data/derived/cdisc-pilot/adam/adpp.xpt"

xpt_file <- rawadam_path
if (!file.exists(xpt_file) && file.exists(alt1)) xpt_file <- alt1
# fallback search
if (!file.exists(xpt_file)) {
  cand <- list.files("/bench/data", pattern = "^adpp\\.xpt$",
                     recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) xpt_file <- cand[1]
}

message("Reading: ", xpt_file)
adpp_raw <- haven::read_xpt(xpt_file)
adpp_raw <- as.data.frame(adpp_raw, stringsAsFactors = FALSE)
names(adpp_raw) <- toupper(names(adpp_raw))

# ---------- helpers mimicking SAS ----------
sas_round_half_up <- function(x, d) {
  res <- x
  idx <- !is.na(x) & is.finite(x)
  xx <- x[idx]
  p <- 10^d
  res[idx] <- sign(xx) * floor(abs(xx) * p + 0.5 + 1e-9) / p
  # avoid -0
  res[idx & res == 0] <- 0
  res
}
sas_put <- function(x, w, d) {
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (length(xi) == 0 || is.na(xi) || is.nan(xi) || !is.finite(xi)) {
      if (w <= 1) return(".")
      return(paste0(strrep(" ", w - 1), "."))
    }
    r <- sas_round_half_up(xi, d)
    if (r == 0) r <- 0
    if (d == 0) {
      s <- formatC(r, format = "f", digits = 0, flag = "")
    } else {
      s <- formatC(r, format = "f", digits = d, flag = "")
    }
    # formatC may give "-0" variants; fix
    if (s %in% c("-0", "-0.0", "-0.00")) s <- gsub("-", "", s, fixed = TRUE)
    nch <- nchar(s, type = "bytes")
    if (nch < w) s <- paste0(strrep(" ", w - nch), s)
    s
  }, character(1), USE.NAMES = FALSE)
}
sas_index <- function(string, substr) {
  # SAS INDEX case-sensitive, 0 if not found
  vapply(seq_along(string), function(i) {
    s <- string[i]; p <- substr[i]
    if (is.na(s) || is.na(p)) return(0L)
    pos <- regexpr(p, s, fixed = TRUE)[1]
    if (pos < 0) 0L else as.integer(pos)
  }, integer(1))
}
str_squish_sas <- function(x) {
  # COMPBL: multiple blanks -> single, trim
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s2 <- gsub("[[:space:]]+", " ", s)
    trimws(s2)
  }, character(1), USE.NAMES = FALSE)
}
sas_compress <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return("")
    gsub("[[:space:]]", "", s)
  }, character(1), USE.NAMES = FALSE)
}

# ---------- formats ----------
pk_map <- c(AUCINFO="AUC", AUCTAU="AUCtau", AUCLAST="AUC(0-tlast)",
            CMAX="Cmax", CMAXD="Cmax/Dose", MRTLAST="MRT", TLAST="tlast",
            TMAX="tmax", CLRFO="CL/F", VZFO="VzF", LAMZ="lambdaZ",
            LAMZHL="t1/2", CLAST="Clast")
trtan_fmt <- c("1"="Period 1 Treatment","2"="Period 2 Treatment","3"="Period 3 Treatment")
statg_fmt <- c("1"="n","2"="Mean (SD)","3"="CV% mean","4"="Geo-mean",
               "5"="CV% geo-mean","6"="Median","7"="[Min; Max]        ")

# ---------- data adpp ----------
df <- adpp_raw

# identify filter column for param codes
filter_col <- NULL
if ("PARAMCD" %in% names(df)) filter_col <- "PARAMCD" else if ("PPTESTCD" %in% names(df)) filter_col <- "PPTESTCD"
if (is.null(filter_col)) stop("No PARAMCD/PPTESTCD found")

wanted <- c('AUCINFO','AUCLAST','CMAX','TMAX','MRTLAST','LAMZHL')

# ensure needed cols exist; create NA if missing
need <- c("PKFN","ANL01FN","PPSTRESN","PPCATL","PPTESTCD","PPORRESU","AVAL",
          "ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY")
for (cc in need) if (!cc %in% names(df)) df[[cc]] <- NA

# WHERE PKFN=1 and ANL01FN ne 1 and paramcd in (...) and PPSTRESN ne . and compress(PPCATL)='ANAL2'
# SAS: missing ne 1 is TRUE
keep <- !is.na(df$PKFN) & df$PKFN == 1
anl01 <- df$ANL01FN
keep_anl <- is.na(anl01) | anl01 != 1
keep <- keep & keep_anl
keep <- keep & (!is.na(df[[filter_col]]) & df[[filter_col]] %in% wanted)
keep <- keep & (!is.na(df$PPSTRESN))
keep <- keep & (sas_compress(df$PPCATL) == "ANAL2")
keep[is.na(keep)] <- FALSE
adpp <- df[keep, , drop = FALSE]

# _PARAM = compbl(put(PPTESTCD,$pk.)||' ('||strip(PPORRESU)||')')
ppcode <- adpp$PPTESTCD
mapped <- vapply(ppcode, function(v) {
  if (is.na(v)) return("")
  if (v %in% names(pk_map)) pk_map[[v]] else v
}, character(1), USE.NAMES = FALSE)
unit <- vapply(adpp$PPORRESU, function(v) {
  if (is.na(v)) return("")
  trimws(as.character(v))
}, character(1), USE.NAMES = FALSE)
param_raw <- paste0(mapped, " (", unit, ")")
adpp[["_PARAM"]] <- str_squish_sas(param_raw)

# AVAL_log = log(AVAL); SAS log(<=0 or missing) = missing
adpp$AVAL <- suppressWarnings(as.numeric(adpp$AVAL))
adpp$AVAL_log <- ifelse(!is.na(adpp$AVAL) & adpp$AVAL > 0, log(adpp$AVAL), NA_real_)

# sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM (SAS order, missing first, byte order)
# use base order radix
ord <- order(adpp$ANLPRNT, adpp$PPSPECL, adpp$PPCATL, adpp$TRTAN, adpp$PPSDY, adpp[["_PARAM"]],
             method = "radix", na.last = FALSE)
adpp <- adpp[ord, , drop = FALSE]

# ---------- proc univariate equivalent ----------
# by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM
byvars <- c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","_PARAM")
# split
grp_key <- interaction(adpp[, byvars, drop = FALSE], drop = TRUE, lex.order = FALSE)
# need groups in sorted order; interaction levels may not be sorted SAS-wise, so compute unique sorted
uniq_df <- unique(adpp[, byvars, drop = FALSE])
# sort uniq_df SAS-style
uo <- order(uniq_df$ANLPRNT, uniq_df$PPSPECL, uniq_df$PPCATL, uniq_df$TRTAN, uniq_df$PPSDY, uniq_df[["_PARAM"]],
            method = "radix", na.last = FALSE)
uniq_df <- uniq_df[uo, , drop = FALSE]

summ_list <- vector("list", nrow(uniq_df))
for (i in seq_len(nrow(uniq_df))) {
  sel <- rep(TRUE, nrow(adpp))
  for (bv in byvars) {
    a <- adpp[[bv]]; b <- uniq_df[i, bv][[1]]
    if (is.na(b)) sel <- sel & is.na(a) else {
      # character vs numeric compare; NA in a -> FALSE
      sel <- sel & (!is.na(a) & a == b)
    }
  }
  x1 <- adpp$AVAL[sel]
  x1 <- x1[!is.na(x1)]
  x2 <- adpp$AVAL_log[sel]
  x2 <- x2[!is.na(x2)]
  n1 <- length(x1); n2 <- length(x2)
  mn1 <- if (n1>0) mean(x1) else NA_real_
  mn2 <- if (n2>0) mean(x2) else NA_real_
  std1 <- if (n1>1) stats::sd(x1) else NA_real_
  std2 <- if (n2>1) stats::sd(x2) else NA_real_
  cv1 <- if (!is.na(std1) && !is.na(mn1) && mn1 != 0) 100*std1/mn1 else NA_real_
  cv2 <- if (!is.na(std2) && !is.na(mn2) && mn2 != 0) 100*std2/mn2 else NA_real_
  med1 <- if (n1>0) stats::median(x1) else NA_real_
  med2 <- if (n2>0) stats::median(x2) else NA_real_
  min1 <- if (n1>0) min(x1) else NA_real_
  max1 <- if (n1>0) max(x1) else NA_real_
  min2 <- if (n2>0) min(x2) else NA_real_
  max2 <- if (n2>0) max(x2) else NA_real_
  summ_list[[i]] <- data.frame(uniq_df[i, , drop=FALSE],
    n1=n1, n2=n2, mn1=mn1, mn2=mn2, std1=std1, std2=std2,
    cv1=cv1, cv2=cv2, med1=med1, med2=med2,
    min1=min1, min2=min2, max1=max1, max2=max2,
    stringsAsFactors=FALSE, check.names=FALSE)
}
adpp_sum <- do.call(rbind, summ_list)
rownames(adpp_sum) <- NULL

# ---------- transpose to adpp_sum_t ----------
out_rows <- vector("list", nrow(adpp_sum)*7)
k <- 1
for (i in seq_len(nrow(adpp_sum))) {
  r <- adpp_sum[i, ]
  par <- r[["_PARAM"]][[1]]
  hasAUC <- sas_index(par, "AUC") > 0
  hasCmax <- sas_index(par, "Cmax") > 0
  hasMRT <- sas_index(par, "MRT") > 0
  hasHalf <- sas_index(par, "t1/2") > 0
  hastmax_low <- sas_index(par, "tmax") > 0
  # s_order 1
  v1 <- sas_put(r$n1, 2, 0)
  # s_order 2
  if (hasAUC) v2 <- paste0(sas_put(r$mn1,6,0), " (", sas_put(r$std1,5,0), ")")
  else if (hasCmax) v2 <- paste0(sas_put(r$mn1,5,0), " (", sas_put(r$std1,4,0), ")")
  else if (hasMRT) v2 <- paste0(sas_put(r$mn1,4,1), " (", sas_put(r$std1,4,2), ")")
  else if (hasHalf) v2 <- paste0(sas_put(r$mn1,4,2), " (", sas_put(r$std1,4,2), ")")
  else v2 <- " "
  # s_order 3
  if (!hastmax_low) v3 <- sas_put(r$cv1,4,1) else v3 <- " "
  # s_order 4
  if (hasAUC) v4 <- sas_put(exp(r$mn2),6,0)
  else if (hasCmax) v4 <- sas_put(exp(r$mn2),5,0)
  else if (hasMRT) v4 <- sas_put(exp(r$mn2),4,1)
  else if (hasHalf) v4 <- sas_put(exp(r$mn2),4,2)
  else v4 <- " "
  # handle exp(NA)=NA -> sas_put gives dot; exp(-Inf)? mn2 could be -Inf? not possible since NA filtered
  # s_order 5
  if (!hastmax_low) {
    gv <- sqrt(exp(r$std2^2)-1)*100
    # if std2 NA -> NA
    if (is.na(r$std2)) gv <- NA_real_
    v5 <- sas_put(gv,4,1)
  } else v5 <- " "
  # s_order 6
  if (hasAUC) v6 <- sas_put(r$med1,6,0)
  else if (hasCmax) v6 <- sas_put(r$med1,5,0)
  else if (hasMRT) v6 <- sas_put(r$med1,4,1)
  else if (hasHalf) v6 <- sas_put(r$med1,4,2)
  else if (hastmax_low) v6 <- sas_put(r$med1,4,2)
  else v6 <- " "
  # s_order 7
  if (hasAUC) v7 <- paste0("[", sas_put(r$min1,6,0), ";", sas_put(r$max1,6,0), "]")
  else if (hasCmax) v7 <- paste0("[", sas_put(r$min1,4,0), ";", sas_put(r$max1,5,0), "]")
  else if (hasMRT) v7 <- paste0("[", sas_put(r$min1,4,2), ";", sas_put(r$max1,4,1), "]")
  else if (hasHalf) v7 <- paste0("[", sas_put(r$min1,4,2), ";", sas_put(r$max1,4,1), "]")
  else if (hastmax_low) v7 <- paste0("[", sas_put(r$min1,4,2), ";", sas_put(r$max1,4,2), "]")
  else v7 <- " "
  # truncate to 30 like SAS LENGTH $30
  v1 <- substr(v1,1,30); v2 <- substr(v2,1,30); v3 <- substr(v3,1,30)
  v4 <- substr(v4,1,30); v5 <- substr(v5,1,30); v6 <- substr(v6,1,30); v7 <- substr(v7,1,30)
  vals <- list(v1,v2,v3,v4,v5,v6,v7)
  for (s in 1:7) {
    out_rows[[k]] <- data.frame(ANLPRNT=r$ANLPRNT, PPSPECL=r$PPSPECL, PPCATL=r$PPCATL,
      TRTAN=r$TRTAN, PPSDY=r$PPSDY, `_PARAM`=r[["_PARAM"]],
      s_order=s, value=vals[[s]],
      stringsAsFactors=FALSE, check.names=FALSE)
    k <- k+1
  }
}
adpp_sum_t <- do.call(rbind, out_rows)
names(adpp_sum_t)[names(adpp_sum_t)=="X_PARAM"] <- "_PARAM"
# ensure column name _PARAM
if (!"_PARAM" %in% names(adpp_sum_t)) {
  # find param col
  pc <- grep("PARAM", names(adpp_sum_t), value=TRUE)[1]
  names(adpp_sum_t)[names(adpp_sum_t)==pc] <- "_PARAM"
}
# sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM
o2 <- order(adpp_sum_t$ANLPRNT, adpp_sum_t$PPSPECL, adpp_sum_t$PPCATL,
            adpp_sum_t$TRTAN, adpp_sum_t$PPSDY, adpp_sum_t$s_order, adpp_sum_t[["_PARAM"]],
            method="radix", na.last=FALSE)
adpp_sum_t <- adpp_sum_t[o2, , drop=FALSE]
rownames(adpp_sum_t) <- NULL

# ---------- build report ----------
# by groups
by_df <- unique(adpp_sum_t[, c("ANLPRNT","PPSPECL","PPCATL"), drop=FALSE])
bo <- order(by_df$ANLPRNT, by_df$PPSPECL, by_df$PPCATL, method="radix", na.last=FALSE)
by_df <- by_df[bo, , drop=FALSE]

trtan_label <- function(x) {
  vapply(as.character(x), function(v) {
    if (is.na(v)) return("")
    if (v %in% names(trtan_fmt)) trtan_fmt[[v]] else v
  }, character(1), USE.NAMES=FALSE)
}
stat_label <- function(x) {
  vapply(as.character(x), function(v) {
    if (v %in% names(statg_fmt)) trimws(statg_fmt[[v]]) else v
  }, character(1), USE.NAMES=FALSE)
}

# wide table body for CSV
wide_list <- list()
txt_lines <- c()
txt_lines <- c(txt_lines,
  "Study001",
  "Table 14.2-PPTS (Page xxx of xxx)",
  "Summary statistics for PK parameters",
  "by compound, matrix, analyte and actual treatment",
  "Analysis Set: PK analysis set",
  "")

for (b in seq_len(nrow(by_df))) {
  ba <- by_df[b,]
  sub <- adpp_sum_t[adpp_sum_t$ANLPRNT==ba$ANLPRNT & adpp_sum_t$PPSPECL==ba$PPSPECL & adpp_sum_t$PPCATL==ba$PPCATL, , drop=FALSE]
  # column order: sorted _PARAM byte-wise
  cols <- sort(unique(sub[["_PARAM"]]))
  # row order: TRTAN, PPSDY, s_order
  rows <- unique(sub[, c("TRTAN","PPSDY","s_order"), drop=FALSE])
  ro <- order(rows$TRTAN, rows$PPSDY, rows$s_order, method="radix", na.last=FALSE)
  rows <- rows[ro, , drop=FALSE]
  # header
  txt_lines <- c(txt_lines,
    paste0("Compound: ", ba$ANLPRNT, " , Matrix: ", ba$PPSPECL, " , Analyte: ", ba$PPCATL),
    "")
  hdr <- paste(c("Actual treatment", "Period day", "Statistic", cols), collapse=" | ")
  txt_lines <- c(txt_lines, hdr)
  txt_lines <- c(txt_lines, paste(rep("-", nchar(hdr, type="bytes")), collapse=""))
  prev_ppsdy <- NULL
  for (ri in seq_len(nrow(rows))) {
    rr <- rows[ri,]
    # break after PPSDY/skip: blank line when PPSDY changes (except first)
    if (!is.null(prev_ppsdy)) {
      cur <- rr$PPSDY; prv <- prev_ppsdy
      same <- (is.na(cur) && is.na(prv)) || (!is.na(cur) && !is.na(prv) && cur==prv)
      # also TRTAN change implies? SAS break after PPSDY only, but PPSDY nested under TRTAN?
      # if TRTAN changed, also new PPSDY group -> blank line already? We'll add blank when PPSDY differs
      # To mimic, check PPSDY differs OR TRTAN differs with same PPSDY? Actually break after PPSDY triggers on PPSDY change within TRTAN?
      # Simplify: blank line when PPSDY value changes irrespective of TRTAN, or TRTAN changes
      # We'll detect TRTAN change too
      if (!same) {
        # blank line already handled below via PPSDY change; for TRTAN change also blank?
      }
    }
    # values across cols
    vals <- vapply(cols, function(cc) {
      m <- sub[sub$TRTAN==rr$TRTAN & ((is.na(sub$PPSDY) & is.na(rr$PPSDY)) | (!is.na(sub$PPSDY) & !is.na(rr$PPSDY) & sub$PPSDY==rr$PPSDY)) & sub$s_order==rr$s_order & sub[["_PARAM"]]==cc, "value"]
      if (length(m)==0) "" else m[1]
    }, character(1), USE.NAMES=FALSE)
    trt_lab <- trtan_label(rr$TRTAN)
    ppsdy_disp <- if (is.na(rr$PPSDY)) "" else as.character(rr$PPSDY)
    stat_disp <- stat_label(rr$s_order)
    line <- paste(c(trt_lab, ppsdy_disp, stat_disp, vals), collapse=" | ")
    txt_lines <- c(txt_lines, line)
    # wide row
    wrow <- data.frame(ANLPRNT=ba$ANLPRNT, PPSPECL=ba$PPSPECL, PPCATL=ba$PPCATL,
                       TRTAN=rr$TRTAN, TRTAN_LABEL=trt_lab, PPSDY=rr$PPSDY,
                       s_order=rr$s_order, Statistic=stat_disp,
                       stringsAsFactors=FALSE, check.names=FALSE)
    for (j in seq_along(cols)) wrow[[cols[j]]] <- vals[j]
    wide_list[[length(wide_list)+1]] <- wrow
    # break after PPSDY/skip: if next row has different PPSDY (or TRTAN?), add blank line
    if (ri < nrow(rows)) {
      nr <- rows[ri+1,]
      curP <- rr$PPSDY; nxtP <- nr$PPSDY
      curT <- rr$TRTAN; nxtT <- nr$TRTAN
      sameP <- (is.na(curP) && is.na(nxtP)) || (!is.na(curP) && !is.na(nxtP) && curP==nxtP)
      sameT <- (is.na(curT) && is.na(nxtT)) || (!is.na(curT) && !is.na(nxtT) && curT==nxtT)
      if (!sameP || !sameT) {
        txt_lines <- c(txt_lines, "")
      }
    }
    prev_ppsdy <- rr$PPSDY
  }
  txt_lines <- c(txt_lines, "")
}

txt_lines <- c(txt_lines,
  "CV% = coefficient of variation (%) = sd/mean*100",
  "CV% geo-mean = (sqrt(exp(variance for log transformed data)-1))*100",
  "Geo-mean: Geometric mean",
  "Geo-mean and CV% geo-mean not presented when the minimum value for a parameter is zero.",
  "Data: adpp  Program: pps.sas  Output: t.14.2.ppts.txt",
  "Fake Data/ Production Run on 17MAR2014:23:57")

wide_df <- do.call(rbind, wide_list)

# ---------- write outputs ----------
# text rendering
writeLines(txt_lines, file.path(out, "P059.txt"))
writeLines(txt_lines, file.path(out, "P059.lst"))
# CSV of table body (wide)
write.csv(wide_df, file.path(out, "P059.csv"), row.names=FALSE, na="")
# dataset counterpart (long transposed)
# rename _PARAM to PARAM_LABEL for CSV safety? keep as _PARAM
out_long <- adpp_sum_t
names(out_long)[names(out_long)=="_PARAM"] <- "PARAM_DISPLAY"
write.csv(out_long, file.path(out, "adpp_sum_t.csv"), row.names=FALSE, na="")
# also summary
write.csv(adpp_sum, file.path(out, "adpp_sum.csv"), row.names=FALSE, na="")

message("Done. Rows wide=", nrow(wide_df), " long=", nrow(adpp_sum_t))

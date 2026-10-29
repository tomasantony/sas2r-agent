out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

in_xpt <- "/bench/data/raw/cdisc-pilot/adam/adpp.xpt"

cat("Reading ADPP...\n")
adpp_raw <- haven::read_xpt(in_xpt)
names(adpp_raw) <- toupper(names(adpp_raw))
adpp_raw <- as.data.frame(adpp_raw, stringsAsFactors = FALSE)

# helpers ---------------------------------------------------------------
round_half_up <- function(x, digits) {
  mult <- 10 ^ digits
  res <- x
  idx <- !is.na(x) & is.finite(x)
  xx <- x[idx]
  # SAS rounds half away from zero
  r <- sign(xx) * floor(abs(xx) * mult + 0.5 + 1e-8) / mult
  # convert -0 to 0
  r[r == 0] <- 0
  res[idx] <- r
  res
}
sas_put <- function(x, w, d) {
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (length(xi) == 0 || is.na(xi) || is.nan(xi) || !is.finite(xi)) {
      if (w <= 1) return(".")
      return(paste0(paste(rep(" ", w - 1), collapse = ""), "."))
    } else {
      r <- round_half_up(xi, d)
      fmt <- paste0("%", w, ".", d, "f")
      # avoid -0.00
      if (r == 0) r <- 0
      out <- sprintf(fmt, r)
      return(out)
    }
  }, character(1), USE.NAMES = FALSE)
}
compbl <- function(s) {
  # compress multiple blanks to single (SAS COMPBL)
  gsub(" {2,}", " ", s, perl = TRUE)
}
sas_compress <- function(s) {
  # SAS COMPRESS default removes blanks
  gsub(" ", "", s, fixed = TRUE)
}
ascii_key <- function(s) {
  sapply(s, function(one) {
    if (is.na(one)) return("")
    paste(sprintf("%02x", as.integer(charToRaw(one))), collapse = "")
  }, USE.NAMES = FALSE)
}
sas_sort_order <- function(v) {
  # missing first, then ASCII (uppercase before lowercase)
  isna <- is.na(v)
  k <- ascii_key(ifelse(isna, "", v))
  order(isna == FALSE, k, method = "radix")
}

pk_map <- c(AUCINFO = "AUC",
            AUCTAU = "AUCtau",
            AUCLAST = "AUC(0-tlast)",
            CMAX = "Cmax",
            CMAXD = "Cmax/Dose",
            MRTLAST = "MRT",
            TLAST = "tlast",
            TMAX = "tmax",
            CLRFO = "CL/F",
            VZFO = "VzF",
            LAMZ = "lambdaZ",
            LAMZHL = "t1/2",
            CLAST = "Clast")

trt_fmt <- function(x) {
  ifelse(is.na(x), "",
         ifelse(x == 1, "Period 1 Treatment",
         ifelse(x == 2, "Period 2 Treatment",
         ifelse(x == 3, "Period 3 Treatment", as.character(x)))))
}
stat_fmt <- function(x) {
  ifelse(is.na(x), "",
         ifelse(x == 1, "n",
         ifelse(x == 2, "Mean (SD)",
         ifelse(x == 3, "CV% mean",
         ifelse(x == 4, "Geo-mean",
         ifelse(x == 5, "CV% geo-mean",
         ifelse(x == 6, "Median",
         ifelse(x == 7, "[Min; Max]        ", as.character(x)))))))))
}

# filter like SAS -------------------------------------------------------
# ensure character columns are character
for (cc in c("ANLPRNT","PPSPECL","PPCATL","PARAMCD","PPTESTCD","PPORRESU")) {
  if (cc %in% names(adpp_raw)) adpp_raw[[cc]] <- as.character(adpp_raw[[cc]])
}
adpp <- adpp_raw
# SAS: where PKFN=1 and ANL01FN ne 1 and paramcd in (...) and PPSTRESN ne . and compress(PPCATL)='ANAL2'
keep <- rep(TRUE, nrow(adpp))
keep <- keep & !is.na(adpp$PKFN) & adpp$PKFN == 1
keep <- keep & (is.na(adpp$ANL01FN) | adpp$ANL01FN != 1)
keep <- keep & !is.na(adpp$PARAMCD) & adpp$PARAMCD %in% c('AUCINFO','AUCLAST','CMAX','TMAX','MRTLAST','LAMZHL')
keep <- keep & !is.na(adpp$PPSTRESN)
comp <- sapply(adpp$PPCATL, function(v) {
  if (is.na(v)) return(NA_character_)
  sas_compress(v)
})
keep <- keep & !is.na(comp) & comp == 'ANAL2'
adpp <- adpp[keep, , drop = FALSE]
cat("Filtered rows:", nrow(adpp), "\n")

# _PARAM = compbl(put(PPTESTCD,$pk.)||' ('||strip(PPORRESU)||')')
mapped <- sapply(adpp$PPTESTCD, function(v) {
  if (is.na(v)) return(NA_character_)
  vv <- trimws(v)
  if (vv %in% names(pk_map)) unname(pk_map[vv]) else vv
}, USE.NAMES = FALSE)
porresu_strip <- sapply(adpp$PPORRESU, function(v) {
  if (is.na(v)) return("")
  trimws(v)
}, USE.NAMES = FALSE)
param_raw <- paste0(mapped, " (", porresu_strip, ")")
adpp[["_PARAM"]] <- compbl(param_raw)
# AVAL_log = log(AVAL); SAS log(0/missing) = missing
adpp$AVAL_log <- ifelse(!is.na(adpp$AVAL) & adpp$AVAL > 0, log(adpp$AVAL), NA_real_)

# sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM (SAS order)
# use arrange with ASCII for chars, missing first
ord_df <- adpp[, c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","_PARAM")]
# build order index
o <- order(
  is.na(adpp$ANLPRNT), ascii_key(ifelse(is.na(adpp$ANLPRNT),"",adpp$ANLPRNT)),
  is.na(adpp$PPSPECL), ascii_key(ifelse(is.na(adpp$PPSPECL),"",adpp$PPSPECL)),
  is.na(adpp$PPCATL), ascii_key(ifelse(is.na(adpp$PPCATL),"",adpp$PPCATL)),
  is.na(adpp$TRTAN), ifelse(is.na(adpp$TRTAN), Inf, adpp$TRTAN),
  is.na(adpp$PPSDY), ifelse(is.na(adpp$PPSDY), Inf, adpp$PPSDY),
  is.na(adpp[["_PARAM"]]), ascii_key(ifelse(is.na(adpp[["_PARAM"]]),"",adpp[["_PARAM"]])),
  method = "radix"
)
adpp <- adpp[o, , drop = FALSE]

# univariate summary by groups ------------------------------------------
grp_vars <- c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","_PARAM")
# split
split_key <- interaction(adpp[, grp_vars], drop = TRUE, sep = "\r")
splits <- split(seq_len(nrow(adpp)), split_key)
sum_rows <- vector("list", length(splits))
ii <- 0
for (idx in splits) {
  ii <- ii + 1
  sub <- adpp[idx, , drop = FALSE]
  av <- sub$AVAL
  lg <- sub$AVAL_log
  n1 <- sum(!is.na(av))
  n2 <- sum(!is.na(lg))
  mn1 <- if (n1 > 0) mean(av, na.rm = TRUE) else NA_real_
  mn2 <- if (n2 > 0) mean(lg, na.rm = TRUE) else NA_real_
  std1 <- if (n1 > 1) stats::sd(av, na.rm = TRUE) else NA_real_
  std2 <- if (n2 > 1) stats::sd(lg, na.rm = TRUE) else NA_real_
  if (!is.na(mn1) && is.nan(mn1)) mn1 <- NA_real_
  if (!is.na(mn2) && is.nan(mn2)) mn2 <- NA_real_
  cv1 <- if (!is.na(mn1) && !is.na(std1) && mn1 != 0) std1 / mn1 * 100 else NA_real_
  cv2 <- if (!is.na(mn2) && !is.na(std2) && mn2 != 0) std2 / mn2 * 100 else NA_real_
  med1 <- if (n1 > 0) stats::median(av, na.rm = TRUE) else NA_real_
  med2 <- if (n2 > 0) stats::median(lg, na.rm = TRUE) else NA_real_
  min1 <- if (n1 > 0) suppressWarnings(min(av, na.rm = TRUE)) else NA_real_
  max1 <- if (n1 > 0) suppressWarnings(max(av, na.rm = TRUE)) else NA_real_
  min2 <- if (n2 > 0) suppressWarnings(min(lg, na.rm = TRUE)) else NA_real_
  max2 <- if (n2 > 0) suppressWarnings(max(lg, na.rm = TRUE)) else NA_real_
  # Inf handling
  if (!is.na(min1) && is.infinite(min1)) min1 <- NA_real_
  if (!is.na(max1) && is.infinite(max1)) max1 <- NA_real_
  sum_rows[[ii]] <- data.frame(
    ANLPRNT = sub$ANLPRNT[1], PPSPECL = sub$PPSPECL[1], PPCATL = sub$PPCATL[1],
    TRTAN = sub$TRTAN[1], PPSDY = sub$PPSDY[1], `_PARAM` = sub[["_PARAM"]][1],
    n1 = n1, mn1 = mn1, std1 = std1, cv1 = cv1, med1 = med1, min1 = min1, max1 = max1,
    n2 = n2, mn2 = mn2, std2 = std2, cv2 = cv2, med2 = med2, min2 = min2, max2 = max2,
    stringsAsFactors = FALSE, check.names = FALSE
  )
}
adpp_sum <- do.call(rbind, sum_rows)
if (is.null(adpp_sum)) {
  adpp_sum <- data.frame(ANLPRNT=character(),PPSPECL=character(),PPCATL=character(),
                         TRTAN=numeric(),PPSDY=numeric(),`_PARAM`=character(),
                         n1=numeric(),mn1=numeric(),std1=numeric(),cv1=numeric(),
                         med1=numeric(),min1=numeric(),max1=numeric(),
                         n2=numeric(),mn2=numeric(),std2=numeric(),cv2=numeric(),
                         med2=numeric(),min2=numeric(),max2=numeric(),
                         stringsAsFactors=FALSE, check.names=FALSE)
}
cat("Summary groups:", nrow(adpp_sum), "\n")

# transpose to 7 rows per group -----------------------------------------
has_AUC <- function(p) grepl("AUC", p, fixed = TRUE)
has_Cmax <- function(p) grepl("Cmax", p, fixed = TRUE)
has_MRT <- function(p) grepl("MRT", p, fixed = TRUE)
has_half <- function(p) grepl("t1/2", p, fixed = TRUE)
has_tmax <- function(p) grepl("tmax", p, fixed = TRUE)

long_list <- vector("list", nrow(adpp_sum) * 7)
k <- 0
for (r in seq_len(nrow(adpp_sum))) {
  prow <- adpp_sum[r, ]
  p <- prow[["_PARAM"]]
  n1 <- prow$n1; mn1 <- prow$mn1; std1 <- prow$std1; cv1 <- prow$cv1
  med1 <- prow$med1; min1 <- prow$min1; max1 <- prow$max1
  n2 <- prow$n2; mn2 <- prow$mn2; std2 <- prow$std2
  exp_mn2 <- if (!is.na(mn2)) exp(mn2) else NA_real_
  gcv <- if (!is.na(std2)) sqrt(exp(std2^2) - 1) * 100 else NA_real_
  # s_order 1..7
  v1 <- sas_put(n1, 2, 0)
  # s2
  if (has_AUC(p)) v2 <- paste0(sas_put(mn1, 6, 0), " (", sas_put(std1, 5, 0), ")")
  else if (has_Cmax(p)) v2 <- paste0(sas_put(mn1, 5, 0), " (", sas_put(std1, 4, 0), ")")
  else if (has_MRT(p)) v2 <- paste0(sas_put(mn1, 4, 1), " (", sas_put(std1, 4, 2), ")")
  else if (has_half(p)) v2 <- paste0(sas_put(mn1, 4, 2), " (", sas_put(std1, 4, 2), ")")
  else v2 <- " "
  # s3
  if (has_tmax(p)) v3 <- " "
  else v3 <- sas_put(cv1, 4, 1)
  # s4
  if (has_AUC(p)) v4 <- sas_put(exp_mn2, 6, 0)
  else if (has_Cmax(p)) v4 <- sas_put(exp_mn2, 5, 0)
  else if (has_MRT(p)) v4 <- sas_put(exp_mn2, 4, 1)
  else if (has_half(p)) v4 <- sas_put(exp_mn2, 4, 2)
  else v4 <- " "
  # s5
  if (has_tmax(p)) v5 <- " "
  else v5 <- sas_put(gcv, 4, 1)
  # s6
  if (has_AUC(p)) v6 <- sas_put(med1, 6, 0)
  else if (has_Cmax(p)) v6 <- sas_put(med1, 5, 0)
  else if (has_MRT(p)) v6 <- sas_put(med1, 4, 1)
  else if (has_half(p)) v6 <- sas_put(med1, 4, 2)
  else if (has_tmax(p)) v6 <- sas_put(med1, 4, 2)
  else v6 <- v5  # SAS otherwise does nothing (retain); not reached for our params
  # s7
  if (has_AUC(p)) v7 <- paste0("[", sas_put(min1, 6, 0), ";", sas_put(max1, 6, 0), "]")
  else if (has_Cmax(p)) v7 <- paste0("[", sas_put(min1, 4, 0), ";", sas_put(max1, 5, 0), "]")
  else if (has_MRT(p)) v7 <- paste0("[", sas_put(min1, 4, 2), ";", sas_put(max1, 4, 1), "]")
  else if (has_half(p)) v7 <- paste0("[", sas_put(min1, 4, 2), ";", sas_put(max1, 4, 1), "]")
  else if (has_tmax(p)) v7 <- paste0("[", sas_put(min1, 4, 2), ";", sas_put(max1, 4, 2), "]")
  else v7 <- v6
  vals <- list(v1, v2, v3, v4, v5, v6, v7)
  for (s in 1:7) {
    k <- k + 1
    long_list[[k]] <- data.frame(
      ANLPRNT = prow$ANLPRNT, PPSPECL = prow$PPSPECL, PPCATL = prow$PPCATL,
      TRTAN = prow$TRTAN, PPSDY = prow$PPSDY, s_order = s,
      `_PARAM` = p, value = vals[[s]],
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
}
adpp_sum_t <- do.call(rbind, long_list)
# sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM
o2 <- order(
  is.na(adpp_sum_t$ANLPRNT), ascii_key(ifelse(is.na(adpp_sum_t$ANLPRNT),"",adpp_sum_t$ANLPRNT)),
  is.na(adpp_sum_t$PPSPECL), ascii_key(ifelse(is.na(adpp_sum_t$PPSPECL),"",adpp_sum_t$PPSPECL)),
  is.na(adpp_sum_t$PPCATL), ascii_key(ifelse(is.na(adpp_sum_t$PPCATL),"",adpp_sum_t$PPCATL)),
  is.na(adpp_sum_t$TRTAN), ifelse(is.na(adpp_sum_t$TRTAN), Inf, adpp_sum_t$TRTAN),
  is.na(adpp_sum_t$PPSDY), ifelse(is.na(adpp_sum_t$PPSDY), Inf, adpp_sum_t$PPSDY),
  adpp_sum_t$s_order,
  is.na(adpp_sum_t[["_PARAM"]]), ascii_key(ifelse(is.na(adpp_sum_t[["_PARAM"]]),"",adpp_sum_t[["_PARAM"]])),
  method = "radix"
)
adpp_sum_t <- adpp_sum_t[o2, , drop = FALSE]
rownames(adpp_sum_t) <- NULL

# build report wide -----------------------------------------------------
param_cols <- unique(adpp_sum_t[["_PARAM"]])
# SAS order: ASCII
param_cols <- param_cols[order(ascii_key(param_cols), method = "radix")]
cat("Param columns:", paste(param_cols, collapse=" | "), "\n")

# BY groups
by_df <- unique(adpp_sum_t[, c("ANLPRNT","PPSPECL","PPCATL")])
by_df <- by_df[order(is.na(by_df$ANLPRNT), ascii_key(ifelse(is.na(by_df$ANLPRNT),"",by_df$ANLPRNT)),
                     is.na(by_df$PPSPECL), ascii_key(ifelse(is.na(by_df$PPSPECL),"",by_df$PPSPECL)),
                     is.na(by_df$PPCATL), ascii_key(ifelse(is.na(by_df$PPCATL),"",by_df$PPCATL)),
                     method="radix"), , drop=FALSE]

# create wide table for CSV (all BY groups together)
# rows: TRTAN PPSDY s_order
wide_rows <- unique(adpp_sum_t[, c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","s_order")])
wide_rows <- wide_rows[order(
  is.na(wide_rows$ANLPRNT), ascii_key(ifelse(is.na(wide_rows$ANLPRNT),"",wide_rows$ANLPRNT)),
  is.na(wide_rows$PPSPECL), ascii_key(ifelse(is.na(wide_rows$PPSPECL),"",wide_rows$PPSPECL)),
  is.na(wide_rows$PPCATL), ascii_key(ifelse(is.na(wide_rows$PPCATL),"",wide_rows$PPCATL)),
  is.na(wide_rows$TRTAN), ifelse(is.na(wide_rows$TRTAN), Inf, wide_rows$TRTAN),
  is.na(wide_rows$PPSDY), ifelse(is.na(wide_rows$PPSDY), Inf, wide_rows$PPSDY),
  wide_rows$s_order, method="radix"), , drop=FALSE]
for (pc in param_cols) {
  wide_rows[[pc]] <- NA_character_
}
# fill
for (i in seq_len(nrow(adpp_sum_t))) {
  r <- adpp_sum_t[i, ]
  sel <- which(wide_rows$ANLPRNT == r$ANLPRNT & wide_rows$PPSPECL == r$PPSPECL &
               wide_rows$PPCATL == r$PPCATL & wide_rows$TRTAN == r$TRTAN &
               ( (is.na(wide_rows$PPSDY) & is.na(r$PPSDY)) | (!is.na(wide_rows$PPSDY) & !is.na(r$PPSDY) & wide_rows$PPSDY == r$PPSDY) ) &
               wide_rows$s_order == r$s_order)
  # handle NA in character BY (exact match with NA)
  # above == fails for NA; fix: use identical handling
  # recompute robustly if no match and NAs present
  if (length(sel) == 0) {
    for (j in seq_len(nrow(wide_rows))) {
      a1 <- (is.na(wide_rows$ANLPRNT[j]) && is.na(r$ANLPRNT)) || (!is.na(wide_rows$ANLPRNT[j]) && !is.na(r$ANLPRNT) && wide_rows$ANLPRNT[j]==r$ANLPRNT)
      a2 <- (is.na(wide_rows$PPSPECL[j]) && is.na(r$PPSPECL)) || (!is.na(wide_rows$PPSPECL[j]) && !is.na(r$PPSPECL) && wide_rows$PPSPECL[j]==r$PPSPECL)
      a3 <- (is.na(wide_rows$PPCATL[j]) && is.na(r$PPCATL)) || (!is.na(wide_rows$PPCATL[j]) && !is.na(r$PPCATL) && wide_rows$PPCATL[j]==r$PPCATL)
      a4 <- (is.na(wide_rows$TRTAN[j]) && is.na(r$TRTAN)) || (!is.na(wide_rows$TRTAN[j]) && !is.na(r$TRTAN) && wide_rows$TRTAN[j]==r$TRTAN)
      a5 <- (is.na(wide_rows$PPSDY[j]) && is.na(r$PPSDY)) || (!is.na(wide_rows$PPSDY[j]) && !is.na(r$PPSDY) && wide_rows$PPSDY[j]==r$PPSDY)
      a6 <- wide_rows$s_order[j]==r$s_order
      if (a1 && a2 && a3 && a4 && a5 && a6) { sel <- j; break }
    }
  }
  wide_rows[sel, pc] <- r$value
}
# add formatted columns
wide_rows$TRTAN_label <- trt_fmt(wide_rows$TRTAN)
wide_rows$Statistic <- stat_fmt(wide_rows$s_order)
# order columns for CSV: BY + TRTAN + PPSDY + s_order + Statistic + params
csv_df <- wide_rows[, c("ANLPRNT","PPSPECL","PPCATL","TRTAN","TRTAN_label","PPSDY","s_order","Statistic", param_cols), drop=FALSE]
# trim single-blank values to empty for CSV? keep as is but convert " " to ""
for (pc in param_cols) {
  csv_df[[pc]][csv_df[[pc]] == " "] <- ""
  csv_df[[pc]][is.na(csv_df[[pc]])] <- ""
}

# text rendering --------------------------------------------------------
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

# column widths: TRTAN 20, PPSDY 8, Statistic 14, each param 19
w_trt <- 20; w_day <- 8; w_stat <- 14; w_par <- 19
pad_r <- function(s, w) {
  s <- ifelse(is.na(s), "", s)
  sapply(s, function(one) {
    n <- nchar(one, type="chars")
    if (n >= w) substr(one, 1, w)
    else paste0(one, paste(rep(" ", w - n), collapse=""))
  }, USE.NAMES=FALSE)
}
pad_c <- function(s, w) {
  s <- ifelse(is.na(s), "", s)
  sapply(s, function(one) {
    n <- nchar(one, type="chars")
    if (n >= w) substr(one, 1, w)
    else {
      l <- floor((w - n)/2); r <- w - n - l
      paste0(paste(rep(" ", l), collapse=""), one, paste(rep(" ", r), collapse=""))
    }
  }, USE.NAMES=FALSE)
}
lines <- c()
for (t in titles) lines <- c(lines, t)
lines <- c(lines, "")
for (b in seq_len(nrow(by_df))) {
  anl <- by_df$ANLPRNT[b]; mat <- by_df$PPSPECL[b]; ana <- by_df$PPCATL[b]
  lines <- c(lines, paste0(" Compound: ", ifelse(is.na(anl),"",anl),
                           " , Matrix: ", ifelse(is.na(mat),"",mat),
                           " , Analyte: ", ifelse(is.na(ana),"",ana)))
  lines <- c(lines, "")
  # header
  hdr1 <- paste0(pad_r("Actual treatment", w_trt), " ",
                 pad_r("Period day", w_day), " ",
                 pad_r("Statistic", w_stat), " ",
                 paste(pad_c(param_cols, w_par), collapse=" "))
  lines <- c(lines, hdr1)
  lines <- c(lines, paste(rep("_", nchar(hdr1, type="chars")), collapse=""))
  subw <- csv_df[csv_df$ANLPRNT == anl | (is.na(csv_df$ANLPRNT) & is.na(anl)), , drop=FALSE]
  # robust BY filter for NA
  selby <- rep(TRUE, nrow(csv_df))
  selby <- selby & sapply(seq_len(nrow(csv_df)), function(i) {
    a1 <- (is.na(csv_df$ANLPRNT[i]) && is.na(anl)) || (!is.na(csv_df$ANLPRNT[i]) && !is.na(anl) && csv_df$ANLPRNT[i]==anl)
    a2 <- (is.na(csv_df$PPSPECL[i]) && is.na(mat)) || (!is.na(csv_df$PPSPECL[i]) && !is.na(mat) && csv_df$PPSPECL[i]==mat)
    a3 <- (is.na(csv_df$PPCATL[i]) && is.na(ana)) || (!is.na(csv_df$PPCATL[i]) && !is.na(ana) && csv_df$PPCATL[i]==ana)
    a1 && a2 && a3
  })
  subw <- csv_df[selby, , drop=FALSE]
  # group display: blank repeats for TRTAN/PPSDY like PROC REPORT GROUP?
  # keep full display (repeat) for clarity; numbers identical
  prev_day <- NA
  for (i in seq_len(nrow(subw))) {
    trt_lab <- subw$TRTAN_label[i]
    dayv <- ifelse(is.na(subw$PPSDY[i]), "", as.character(subw$PPSDY[i]))
    statv <- subw$Statistic[i]
    # left-align value cells? SAS define value left. Our stored values have leading blanks from PUT; keep as is, pad right to w_par
    # For text, right-trim? Keep full string padded/truncated to w_par with left alignment? SAS left means leading blanks? Actually left aligns, so leading blanks would be kept? We'll left-align by padding on right, but keep leading blanks.
    # To fit width 19, if value longer, truncate? Our values <=19, so pad right.
    par_vals <- sapply(param_cols, function(pc) {
      v <- subw[i, pc]
      if (is.na(v)) v <- ""
      # v may contain leading blanks; pad on right to w_par
      n <- nchar(v, type="chars")
      if (n >= w_par) substr(v, 1, w_par)
      else paste0(v, paste(rep(" ", w_par - n), collapse=""))
    }, USE.NAMES=FALSE)
    rowline <- paste0(pad_r(trt_lab, w_trt), " ",
                      pad_r(dayv, w_day), " ",
                      pad_r(statv, w_stat), " ",
                      paste(par_vals, collapse=" "))
    lines <- c(lines, rowline)
    # break after PPSDY / skip : blank line when PPSDY changes (next row different PPSDY or TRTAN group?)
    cur_day <- subw$PPSDY[i]
    nxt_day <- if (i < nrow(subw)) subw$PPSDY[i+1] else NA
    cur_trt <- subw$TRTAN[i]
    nxt_trt <- if (i < nrow(subw)) subw$TRTAN[i+1] else NA
    day_change <- (is.na(cur_day) && !is.na(nxt_day)) || (!is.na(cur_day) && is.na(nxt_day)) || (!is.na(cur_day) && !is.na(nxt_day) && cur_day != nxt_day) || (is.na(cur_trt) != is.na(nxt_trt)) || (!is.na(cur_trt) && !is.na(nxt_trt) && cur_trt != nxt_trt) || (i==nrow(subw) && FALSE)
    # SAS break after PPSDY: skip line after each PPSDY group
    # detect PPSDY group boundary
    if (i < nrow(subw)) {
      if (!identical(paste(cur_day, cur_trt), paste(nxt_day, nxt_trt))) {
        # check if PPSDY group ends (PPSDY or TRTAN changes means PPSDY group ends since nested)
        # TRTAN contains PPSDY, so any TRTAN change also ends PPSDY
        lines <- c(lines, "")
      }
    }
  }
  lines <- c(lines, "")
}
for (f in footnotes) lines <- c(lines, f)

# write outputs ---------------------------------------------------------
write.csv(csv_df, file.path(out, "P035.csv"), row.names = FALSE, na = "")
writeLines(lines, file.path(out, "P035.txt"))
writeLines(lines, file.path(out, "P035.lst"))
# also write long dataset for traceability
write.csv(adpp_sum_t, file.path(out, "P035_long.csv"), row.names = FALSE, na = "")
write.csv(adpp_sum, file.path(out, "P035_sum.csv"), row.names = FALSE, na = "")
cat("Done. Wrote", file.path(out, "P035.txt"), "and", file.path(out, "P035.csv"), "\n")

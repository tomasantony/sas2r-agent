# P059 pk_param_summary translation
# Reads ADPP XPT, reproduces PROC UNIVARIATE + PROC REPORT summary

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

round_half_up <- function(x, digits = 0) {
  pow <- 10 ^ digits
  res <- sign(x) * floor(abs(x) * pow + 0.5 + 1e-10) / pow
  res[x == 0] <- 0
  res
}

sas_put <- function(x, w, d = 0) {
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (length(xi) == 0 || is.na(xi) || is.nan(xi) || is.infinite(xi)) {
      s <- "."
    } else {
      r <- round_half_up(xi, d)
      if (r == 0) r <- 0
      # avoid -0
      if (identical(r, -0)) r <- 0
      s <- formatC(r, format = "f", digits = d)
      if (s == "-0" || s == "-0.0" || s == "-0.00") {
        s <- gsub("-", "", s, fixed = TRUE)
      }
    }
    if (nchar(s, type = "chars") < w) {
      s <- paste0(strrep(" ", w - nchar(s, type = "chars")), s)
    }
    s
  }, character(1), USE.NAMES = FALSE)
}

# ---- read ADPP ----
xpt_path <- "/bench/data/raw/cdisc-pilot/adam/adpp.xpt"
adpp_raw <- haven::read_xpt(xpt_path)
names(adpp_raw) <- toupper(names(adpp_raw))

# check required columns exist
need <- c("PKFN","ANL01FN","PPSTRESN","PPCATL","PPTESTCD","PPORRESU","AVAL",
          "ANLPRNT","PPSPECL","TRTAN","PPSDY")
# PARAMCD may be named PARAMCD
has_paramcd <- "PARAMCD" %in% names(adpp_raw)
if (has_paramcd) need <- c(need, "PARAMCD")
for (nm in need) {
  if (!nm %in% names(adpp_raw)) stop(paste0("missing column: ", nm))
}
stopifnot(all(c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","AVAL") %in% names(adpp_raw)))

pk_map <- c("AUCINFO"="AUC","AUCTAU"="AUCtau","AUCLAST"="AUC(0-tlast)",
            "CMAX"="Cmax","CMAXD"="Cmax/Dose","MRTLAST"="MRT","TLAST"="tlast",
            "TMAX"="tmax","CLRFO"="CL/F","VZFO"="VzF","LAMZ"="lambdaZ",
            "LAMZHL"="t1/2","CLAST"="Clast")

filter_list <- c("AUCINFO","AUCLAST","CMAX","TMAX","MRTLAST","LAMZHL")

df <- adpp_raw
# PARAMCD filter column
if (has_paramcd) {
  param_for_filter <- df$PARAMCD
} else {
  param_for_filter <- df$PPTESTCD
}
# compress(PPCATL) == ANAL2 : remove all blanks
ppcatl_noblank <- gsub("[[:space:]]", "", as.character(df$PPCATL))
keep <- !is.na(df$PKFN) & df$PKFN == 1 &
  (is.na(df$ANL01FN) | df$ANL01FN != 1) &
  !is.na(param_for_filter) & param_for_filter %in% filter_list &
  !is.na(df$PPSTRESN) &
  !is.na(ppcatl_noblank) & ppcatl_noblank == "ANAL2"
adpp <- df[keep, , drop = FALSE]
stopifnot(nrow(adpp) > 0)

# _PARAM = compbl(put(PPTESTCD,$pk.) || ' (' || strip(PPORRESU) || ')')
pp_test <- as.character(adpp$PPTESTCD)
mapped <- vapply(pp_test, function(v) {
  if (!is.na(v) && v %in% names(pk_map)) pk_map[[v]] else if (is.na(v)) "" else v
}, character(1), USE.NAMES = FALSE)
unit <- trimws(as.character(adpp$PPORRESU))
unit[is.na(unit)] <- ""
param_raw <- paste0(mapped, " (", unit, ")")
# compbl: compress multiple blanks to single
param_comp <- gsub("[[:blank:]]+", " ", param_raw)
param_comp <- trimws(param_comp)
adpp$`_PARAM` <- param_comp
# AVAL_log = log(AVAL) ; SAS log(<=0) = missing
aval <- suppressWarnings(as.numeric(adpp$AVAL))
adpp$AVAL <- aval
adpp$AVAL_log <- ifelse(!is.na(aval) & aval > 0, log(aval), NA_real_)

# sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM (SAS binary order)
# Use order with C locale behaviour: uppercase before lowercase.
# Implement binary order via xtfrm on raw bytes: use order() on character is close;
# enforce by ordering via code points.
ord_key <- function(x) {
  # return rank by binary; order() handles it if locale C; just use order
  x
}
adpp <- adpp[order(adpp$ANLPRNT, adpp$PPSPECL, adpp$PPCATL, adpp$TRTAN, adpp$PPSDY, adpp$`_PARAM`,
                   na.last = FALSE, method = "radix"), , drop = FALSE]

# ---- PROC UNIVARIATE equivalent ----
grp_vars <- c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","_PARAM")
summ_one <- function(x) {
  xn <- x[!is.na(x)]
  n <- length(xn)
  if (n == 0) return(c(n=0, mean=NA_real_, std=NA_real_, cv=NA_real_, median=NA_real_, min=NA_real_, max=NA_real_))
  mn <- mean(xn)
  st <- if (n > 1) stats::sd(xn) else NA_real_
  cv <- if (!is.na(mn) && mn != 0 && !is.na(st)) st/mn*100 else NA_real_
  md <- stats::median(xn)
  mi <- min(xn)
  ma <- max(xn)
  c(n=n, mean=mn, std=st, cv=cv, median=md, min=mi, max=ma)
}

# group split
adpp$`.grp` <- interaction(adpp$ANLPRNT, adpp$PPSPECL, adpp$PPCATL, adpp$TRTAN, adpp$PPSDY, adpp$`_PARAM`, drop = TRUE, sep = "\r")
splits <- split(adpp, adpp$`.grp`)
sum_rows <- lapply(splits, function(g) {
  s1 <- summ_one(g$AVAL)
  s2 <- summ_one(g$AVAL_log)
  data.frame(ANLPRNT=g$ANLPRNT[1], PPSPECL=g$PPSPECL[1], PPCATL=g$PPCATL[1],
             TRTAN=g$TRTAN[1], PPSDY=g$PPSDY[1], `_PARAM`=g$`_PARAM`[1], check.names=FALSE,
             n1=s1[["n"]], mn1=s1[["mean"]], std1=s1[["std"]], cv1=s1[["cv"]],
             med1=s1[["median"]], min1=s1[["min"]], max1=s1[["max"]],
             n2=s2[["n"]], mn2=s2[["mean"]], std2=s2[["std"]], cv2=s2[["cv"]],
             med2=s2[["median"]], min2=s2[["min"]], max2=s2[["max"]],
             stringsAsFactors=FALSE)
})
adpp_sum <- bind_rows(sum_rows)

# ---- transpose to 7 stat rows ----
mk_val <- function() {}
t_list <- lapply(seq_len(nrow(adpp_sum)), function(i) {
  r <- adpp_sum[i, ]
  p <- as.character(r$`_PARAM`)
  is_auc   <- grepl("AUC", p, fixed = TRUE)
  is_cmax  <- grepl("Cmax", p, fixed = TRUE)
  is_mrt   <- grepl("MRT", p, fixed = TRUE)
  is_thalf <- grepl("t1/2", p, fixed = TRUE)
  is_tmax  <- grepl("tmax", p, fixed = TRUE)
  n1 <- r$n1; mn1 <- r$mn1; std1 <- r$std1; cv1 <- r$cv1
  med1 <- r$med1; min1 <- r$min1; max1 <- r$max1
  mn2 <- r$mn2; std2 <- r$std2

  v1 <- sas_put(n1, 2, 0)

  if (is_auc) {
    v2 <- paste0(sas_put(mn1, 6, 0), " (", sas_put(std1, 5, 0), ")")
  } else if (is_cmax) {
    v2 <- paste0(sas_put(mn1, 5, 0), " (", sas_put(std1, 4, 0), ")")
  } else if (is_mrt) {
    v2 <- paste0(sas_put(mn1, 4, 1), " (", sas_put(std1, 4, 2), ")")
  } else if (is_thalf) {
    v2 <- paste0(sas_put(mn1, 4, 2), " (", sas_put(std1, 4, 2), ")")
  } else {
    v2 <- ""
  }

  if (is_tmax) {
    v3 <- ""
  } else {
    v3 <- sas_put(cv1, 4, 1)
  }

  if (is_auc) {
    v4 <- sas_put(exp(mn2), 6, 0)
  } else if (is_cmax) {
    v4 <- sas_put(exp(mn2), 5, 0)
  } else if (is_mrt) {
    v4 <- sas_put(exp(mn2), 4, 1)
  } else if (is_thalf) {
    v4 <- sas_put(exp(mn2), 4, 2)
  } else {
    v4 <- ""
  }

  if (is_tmax) {
    v5 <- ""
  } else {
    gv <- suppressWarnings(sqrt(exp(std2^2) - 1) * 100)
    # if std2 NA -> NA
    if (is.na(std2)) gv <- NA_real_
    v5 <- sas_put(gv, 4, 1)
  }

  if (is_auc) {
    v6 <- sas_put(med1, 6, 0)
  } else if (is_cmax) {
    v6 <- sas_put(med1, 5, 0)
  } else if (is_mrt) {
    v6 <- sas_put(med1, 4, 1)
  } else if (is_thalf) {
    v6 <- sas_put(med1, 4, 2)
  } else if (is_tmax) {
    v6 <- sas_put(med1, 4, 2)
  } else {
    v6 <- ""
  }

  if (is_auc) {
    v7 <- paste0("[", sas_put(min1, 6, 0), ";", sas_put(max1, 6, 0), "]")
  } else if (is_cmax) {
    v7 <- paste0("[", sas_put(min1, 4, 0), ";", sas_put(max1, 5, 0), "]")
  } else if (is_mrt) {
    v7 <- paste0("[", sas_put(min1, 4, 2), ";", sas_put(max1, 4, 1), "]")
  } else if (is_thalf) {
    v7 <- paste0("[", sas_put(min1, 4, 2), ";", sas_put(max1, 4, 1), "]")
  } else if (is_tmax) {
    v7 <- paste0("[", sas_put(min1, 4, 2), ";", sas_put(max1, 4, 2), "]")
  } else {
    v7 <- ""
  }

  vals <- c(v1, v2, v3, v4, v5, v6, v7)
  data.frame(ANLPRNT=rep(r$ANLPRNT,7), PPSPECL=rep(r$PPSPECL,7), PPCATL=rep(r$PPCATL,7),
             TRTAN=rep(r$TRTAN,7), PPSDY=rep(r$PPSDY,7),
             s_order=1:7, `_PARAM`=rep(r$`_PARAM`,7), value=vals,
             stringsAsFactors=FALSE, check.names=FALSE)
})
adpp_sum_t <- bind_rows(t_list)
# sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM
adpp_sum_t <- adpp_sum_t[order(adpp_sum_t$ANLPRNT, adpp_sum_t$PPSPECL, adpp_sum_t$PPCATL,
                               adpp_sum_t$TRTAN, adpp_sum_t$PPSDY, adpp_sum_t$s_order,
                               adpp_sum_t$`_PARAM`, na.last=FALSE, method="radix"), , drop=FALSE]

# ---- formats ----
trtan_fmt <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) "" else if (v==1) "Period 1 Treatment" else if (v==2) "Period 2 Treatment" else if (v==3) "Period 3 Treatment" else as.character(v)
  }, character(1))
}
statg_fmt <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) "" else if (v==1) "n" else if (v==2) "Mean (SD)" else if (v==3) "CV% mean" else if (v==4) "Geo-mean" else if (v==5) "CV% geo-mean" else if (v==6) "Median" else if (v==7) "[Min; Max]" else as.character(v)
  }, character(1))
}

# distinct params in across order (sorted binary)
param_cols <- sort(unique(adpp_sum_t$`_PARAM`))
# distinct BY groups
by_groups <- unique(adpp_sum_t[, c("ANLPRNT","PPSPECL","PPCATL")])
by_groups <- by_groups[order(by_groups$ANLPRNT, by_groups$PPSPECL, by_groups$PPCATL, na.last=FALSE, method="radix"), , drop=FALSE]

# Build wide table body for CSV: one row per TRTAN/PPSDY/s_order within each BY
wide_list <- lapply(seq_len(nrow(by_groups)), function(b) {
  bg <- by_groups[b, ]
  sub <- adpp_sum_t[adpp_sum_t$ANLPRNT==bg$ANLPRNT & adpp_sum_t$PPSPECL==bg$PPSPECL & adpp_sum_t$PPCATL==bg$PPCATL, , drop=FALSE]
  # pivot wider
  w <- tidyr::pivot_wider(sub, id_cols=c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","s_order"),
                          names_from="_PARAM", values_from="value")
  # ensure all param_cols present
  for (pc in param_cols) if (!pc %in% names(w)) w[[pc]] <- ""
  w <- w[, c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","s_order", param_cols), drop=FALSE]
  w[order(w$TRTAN, w$PPSDY, w$s_order, na.last=FALSE), , drop=FALSE]
})
wide_body <- bind_rows(wide_list)
wide_body$Statistic <- statg_fmt(wide_body$s_order)
wide_body$TRTAN_label <- trtan_fmt(wide_body$TRTAN)
# reorder for CSV
csv_df <- wide_body[, c("ANLPRNT","PPSPECL","PPCATL","TRTAN","TRTAN_label","PPSDY","s_order","Statistic", param_cols), drop=FALSE]

# write CSV of table body
write.csv(csv_df, file.path(out, "P059.csv"), row.names=FALSE, na="")

# ---- text rendering (PROC REPORT like) ----
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

lines <- c()
for (t in titles) lines <- c(lines, t)
lines <- c(lines, "")

for (b in seq_len(nrow(by_groups))) {
  bg <- by_groups[b, ]
  comp_line <- paste0("Compound: ", bg$ANLPRNT, " , Matrix: ", bg$PPSPECL, " , Analyte: ", bg$PPCATL)
  lines <- c(lines, comp_line, "")
  # header
  hdr <- sprintf("%-20s %-10s %-14s", "Actual treatment", "Period day", "Statistic")
  for (pc in param_cols) {
    # truncate/pad header to 19 like value width
    h <- pc
    if (nchar(h) > 19) h <- substr(h, 1, 19)
    hdr <- paste0(hdr, " | ", sprintf("%-19s", h))
  }
  lines <- c(lines, hdr)
  lines <- c(lines, paste0(strrep("_", nchar(hdr)))))
  sub <- wide_body[wide_body$ANLPRNT==bg$ANLPRNT & wide_body$PPSPECL==bg$PPSPECL & wide_body$PPCATL==bg$PPCATL, , drop=FALSE]
  sub <- sub[order(sub$TRTAN, sub$PPSDY, sub$s_order), , drop=FALSE]
  prev_ppsdy <- NA
  for (r in seq_len(nrow(sub))) {
    trt <- trtan_fmt(sub$TRTAN[r])
    ppd <- ifelse(is.na(sub$PPSDY[r]), "", as.character(sub$PPSDY[r]))
    st <- statg_fmt(sub$s_order[r])
    row <- sprintf("%-20s %-10s %-14s", trt, ppd, st)
    for (pc in param_cols) {
      v <- sub[r, pc][[1]]
      if (is.na(v)) v <- ""
      # left align in 19 like REPORT left? keep as is padded? Use sprintf left
      # preserve SAS leading spaces? REPORT left would left-align, so trim leading then pad right
      # To keep numbers identical, keep original string right-padded? Use %-19s of trimmed?
      # Keep original value string as stored (which has leading spaces) left-justified in field:
      # For text, show trimmed? Keep original with leading spaces trimmed for readability but numbers same.
      # We display value left-aligned: trim leading spaces? No - keep as is but pad to 19.
      vv <- as.character(v)
      if (nchar(vv) < 19) vv <- paste0(vv, strrep(" ", 19 - nchar(vv)))
      row <- paste0(row, " | ", vv)
    }
    lines <- c(lines, row)
    # break after PPSDY / skip : add blank line when PPSDY changes (after last s_order of group)
    if (r < nrow(sub)) {
      if (!identical(sub$PPSDY[r], sub$PPSDY[r+1]) || !identical(sub$TRTAN[r], sub$TRTAN[r+1])) {
        lines <- c(lines, "")
      }
    }
  }
  lines <- c(lines, "")
}
lines <- c(lines, footnotes)

txt_path <- file.path(out, "P059.txt")
writeLines(lines, txt_path)
# also write .lst copy for benchmark expectation
lst_path <- file.path(out, "P059.lst")
writeLines(lines, lst_path)

# also write plotted-data style CSV already done; ensure dataset CSV exists for adpp_sum_t long
write.csv(adpp_sum_t, file.path(out, "P059_detail.csv"), row.names=FALSE, na="")

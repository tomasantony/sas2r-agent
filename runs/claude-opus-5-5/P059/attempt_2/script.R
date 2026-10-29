suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

## ---------------- helpers ----------------
# Round half away from zero, as SAS does
rhu <- function(x, d = 0) {
  r <- sign(x) * floor(abs(x) * 10^d + 0.5 + 1e-9) / 10^d
  r[!is.na(r) & r == 0] <- 0
  r
}

# Emulates SAS PUT(x, w.d): right-aligned in width w, "." for missing.
# If the value does not fit, decimals are dropped, then the leading zero,
# and finally the field is filled with asterisks.
sas_put <- function(x, w, d = 0) {
  vapply(x, function(v) {
    if (is.na(v) || !is.finite(v)) return(formatC(".", width = w))
    dd <- d
    s <- sprintf("%.*f", dd, rhu(v, dd))
    while (nchar(s) > w && dd > 0) {
      dd <- dd - 1
      s <- sprintf("%.*f", dd, rhu(v, dd))
    }
    if (nchar(s) > w) s <- sub("^(-?)0\\.", "\\1.", s)
    if (nchar(s) > w) s <- strrep("*", w)
    formatC(s, width = w)
  }, character(1))
}

# Collapse runs of blanks to a single blank (SAS COMPBL)
compbl <- function(x) gsub(" {2,}", " ", x)

pk_fmt <- c(AUCINFO = "AUC", AUCTAU = "AUCtau", AUCLAST = "AUC(0-tlast)",
            CMAX = "Cmax", CMAXD = "Cmax/Dose", MRTLAST = "MRT", TLAST = "tlast",
            TMAX = "tmax", CLRFO = "CL/F", VZFO = "VzF", LAMZ = "lambdaZ",
            LAMZHL = "t1/2", CLAST = "Clast")
put_pk <- function(x) ifelse(x %in% names(pk_fmt), pk_fmt[x], x)

trtan_fmt <- function(x) {
  ifelse(is.na(x), ".",
         ifelse(x == 1, "Period 1 Treatment",
                ifelse(x == 2, "Period 2 Treatment",
                       ifelse(x == 3, "Period 3 Treatment", as.character(x)))))
}

statg <- c("n", "Mean (SD)", "CV% mean", "Geo-mean", "CV% geo-mean",
           "Median", "[Min; Max]")

## ---------------- data step adpp ----------------
src <- read_xpt(file.path(rawadam, "adpp.xpt"))
names(src) <- toupper(names(src))
chr <- function(v) { v <- as.character(v); v[is.na(v)] <- ""; v }
for (v in c("ANLPRNT", "PPSPECL", "PPCATL", "PPTESTCD", "PPORRESU", "PARAMCD"))
  if (v %in% names(src)) src[[v]] <- chr(src[[v]])

adpp <- src %>%
  filter(!is.na(PKFN) & PKFN == 1,
         is.na(ANL01FN) | ANL01FN != 1,
         PARAMCD %in% c("AUCINFO", "AUCLAST", "CMAX", "TMAX", "MRTLAST", "LAMZHL"),
         !is.na(PPSTRESN),
         gsub(" ", "", PPCATL) == "ANAL2") %>%
  mutate(`_PARAM` = compbl(paste0(put_pk(PPTESTCD), " (", trimws(PPORRESU), ")")),
         AVAL_log = ifelse(!is.na(AVAL) & AVAL > 0, log(AVAL), NA_real_))

o <- order(adpp$ANLPRNT, adpp$PPSPECL, adpp$PPCATL, adpp$TRTAN, adpp$PPSDY,
           adpp$`_PARAM`, method = "radix", na.last = FALSE)
adpp <- adpp[o, ]

## ---------------- proc univariate ----------------
ustat <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  mn <- if (n > 0) mean(x) else NA_real_
  sd <- if (n > 1) sd(x) else NA_real_
  cv <- if (!is.na(sd) && !is.na(mn) && mn != 0) sd / mn * 100 else NA_real_
  c(n = n, mn = mn, std = sd, cv = cv,
    med = if (n > 0) median(x) else NA_real_,
    min = if (n > 0) min(x) else NA_real_,
    max = if (n > 0) max(x) else NA_real_)
}

adpp_sum <- adpp %>%
  group_by(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`) %>%
  group_modify(function(d, k) {
    a <- ustat(d$AVAL); b <- ustat(d$AVAL_log)
    tibble(n1 = a[["n"]], n2 = b[["n"]], mn1 = a[["mn"]], mn2 = b[["mn"]],
           std1 = a[["std"]], std2 = b[["std"]], cv1 = a[["cv"]], cv2 = b[["cv"]],
           med1 = a[["med"]], med2 = b[["med"]], min1 = a[["min"]], min2 = b[["min"]],
           max1 = a[["max"]], max2 = b[["max"]])
  }) %>% ungroup()
adpp_sum <- adpp_sum[order(adpp_sum$ANLPRNT, adpp_sum$PPSPECL, adpp_sum$PPCATL,
                           adpp_sum$TRTAN, adpp_sum$PPSDY, adpp_sum$`_PARAM`,
                           method = "radix", na.last = FALSE), ]

## ---------------- data step adpp_sum_t ----------------
has <- function(p, s) grepl(s, p, fixed = TRUE)
rows <- list()
for (i in seq_len(nrow(adpp_sum))) {
  r <- adpp_sum[i, ]
  p <- r$`_PARAM`
  vals <- character(7)
  vals[1] <- sas_put(r$n1, 2)
  vals[2] <- if (has(p, "AUC")) paste0(sas_put(r$mn1, 6), " (", sas_put(r$std1, 5), ")")
    else if (has(p, "Cmax")) paste0(sas_put(r$mn1, 5), " (", sas_put(r$std1, 4), ")")
    else if (has(p, "MRT"))  paste0(sas_put(r$mn1, 4, 1), " (", sas_put(r$std1, 4, 2), ")")
    else if (has(p, "t1/2")) paste0(sas_put(r$mn1, 4, 2), " (", sas_put(r$std1, 4, 2), ")")
    else " "
  vals[3] <- if (!has(p, "tmax")) sas_put(r$cv1, 4, 1) else " "
  g <- exp(r$mn2)
  vals[4] <- if (has(p, "AUC")) sas_put(g, 6)
    else if (has(p, "Cmax")) sas_put(g, 5)
    else if (has(p, "MRT"))  sas_put(g, 4, 1)
    else if (has(p, "t1/2")) sas_put(g, 4, 2)
    else " "
  vals[5] <- if (!has(p, "tmax")) sas_put(sqrt(exp(r$std2^2) - 1) * 100, 4, 1) else " "
  vals[6] <- if (has(p, "AUC")) sas_put(r$med1, 6)
    else if (has(p, "Cmax")) sas_put(r$med1, 5)
    else if (has(p, "MRT"))  sas_put(r$med1, 4, 1)
    else if (has(p, "t1/2")) sas_put(r$med1, 4, 2)
    else if (has(p, "tmax")) sas_put(r$med1, 4, 2)
    else vals[5]
  vals[7] <- if (has(p, "AUC")) paste0("[", sas_put(r$min1, 6), ";", sas_put(r$max1, 6), "]")
    else if (has(p, "Cmax")) paste0("[", sas_put(r$min1, 4), ";", sas_put(r$max1, 5), "]")
    else if (has(p, "MRT"))  paste0("[", sas_put(r$min1, 4, 2), ";", sas_put(r$max1, 4, 1), "]")
    else if (has(p, "t1/2")) paste0("[", sas_put(r$min1, 4, 2), ";", sas_put(r$max1, 4, 1), "]")
    else if (has(p, "tmax")) paste0("[", sas_put(r$min1, 4, 2), ";", sas_put(r$max1, 4, 2), "]")
    else vals[6]
  rows[[i]] <- r[rep(1, 7), ] %>% mutate(value = substr(vals, 1, 30), s_order = 1:7)
}
adpp_sum_t <- bind_rows(rows)
adpp_sum_t <- adpp_sum_t[order(adpp_sum_t$ANLPRNT, adpp_sum_t$PPSPECL, adpp_sum_t$PPCATL,
                               adpp_sum_t$TRTAN, adpp_sum_t$PPSDY, adpp_sum_t$s_order,
                               adpp_sum_t$`_PARAM`, method = "radix", na.last = FALSE), ]
write.csv(adpp_sum_t, file.path(out, "adpp_sum_t.csv"), row.names = FALSE, na = "")

## ---------------- proc report (wide table) ----------------
params <- sort(unique(adpp_sum_t$`_PARAM`), method = "radix")
wide <- adpp_sum_t %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`, value) %>%
  tidyr::pivot_wider(names_from = `_PARAM`, values_from = value, values_fill = "")
for (p in params) if (!p %in% names(wide)) wide[[p]] <- ""
wide <- wide[order(wide$ANLPRNT, wide$PPSPECL, wide$PPCATL, wide$TRTAN, wide$PPSDY,
                   wide$s_order, method = "radix", na.last = FALSE), ]
wide <- wide %>% mutate(Statistic = statg[s_order]) %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, Statistic, all_of(params))
write.csv(wide, file.path(out, "P059.csv"), row.names = FALSE, na = "")
write.csv(wide, file.path(out, "CLXCHECKNOBS.csv"), row.names = FALSE, na = "")

## ---------------- text rendering ----------------
# PROC REPORT panels the across columns when they exceed linesize (132):
# id block = 20+1+8+1+14 = 44, each across column 19+1 -> 4 per page.
padr <- function(s, w) formatC(substr(s, 1, w), width = -w, flag = "-")
pw <- 19
ls <- 132
idw <- 44
per_page <- max(1, floor((ls - idw) / (pw + 1)))

titles <- c("Study001",
            "Table 14.2-PPTS (Page xxx of xxx)",
            "Summary statistics for PK parameters",
            "by compound, matrix, analyte and actual treatment",
            "Analysis Set: PK analysis set", "")
foots <- c("", "",
           "CV% = coefficient of variation (%) = sd/mean*100",
           "CV% geo-mean = (sqrt(exp(variance for log transformed data)-1))*100",
           "Geo-mean: Geometric mean",
           "Geo-mean and CV% geo-mean not presented when the minimum value for a parameter is zero.",
           "Data: adpp  Program: pps.sas  Output: t.14.2.ppts.txt",
           "Fake Data/ Production Run on 17MAR2014:23:57", "\f")

lines <- c()
bykeys <- unique(wide[, c("ANLPRNT", "PPSPECL", "PPCATL")])
for (b in seq_len(nrow(bykeys))) {
  bk <- bykeys[b, ]
  d <- wide[wide$ANLPRNT == bk$ANLPRNT & wide$PPSPECL == bk$PPSPECL &
              wide$PPCATL == bk$PPCATL, ]
  pp_all <- params[sapply(params, function(p) any(trimws(d[[p]]) != ""))]
  chunks <- split(pp_all, ceiling(seq_along(pp_all) / per_page))
  for (pp in chunks) {
    lines <- c(lines, titles,
               paste0("Compound: ", bk$ANLPRNT, " , Matrix: ", bk$PPSPECL,
                      " , Analyte: ", bk$PPCATL), " ")
    hdr <- paste(padr("Actual treatment", 20), padr("Period day", 8), padr("Statistic", 14),
                 paste(sapply(pp, padr, w = pw), collapse = " "))
    lines <- c(lines, hdr, strrep("_", nchar(hdr)), "")
    prev_trt <- NULL; prev_day <- NULL
    for (i in seq_len(nrow(d))) {
      r <- d[i, ]
      newtrt <- is.null(prev_trt) || !identical(r$TRTAN, prev_trt)
      newday <- newtrt || !identical(r$PPSDY, prev_day)
      if (!is.null(prev_day) && newday) lines <- c(lines, "")
      trt <- if (newtrt) trtan_fmt(r$TRTAN) else ""
      day <- if (newday) ifelse(is.na(r$PPSDY), ".", format(r$PPSDY)) else ""
      cells <- sapply(pp, function(p) padr(sub("^ +", "", r[[p]]), pw))
      lines <- c(lines, paste(padr(trt, 20), padr(day, 8), padr(r$Statistic, 14),
                              paste(cells, collapse = " ")))
      prev_trt <- r$TRTAN; prev_day <- r$PPSDY
    }
    lines <- c(lines, foots)
  }
}
writeLines(lines, file.path(out, "P059.txt"))
cat("P059 done:", nrow(adpp), "records,", nrow(wide), "table rows\n")

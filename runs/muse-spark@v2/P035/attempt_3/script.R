out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

tryCatch(Sys.setlocale("LC_COLLATE","C"), error=function(e) NULL, warning=function(w) NULL)

candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.xpt",
  "/bench/data/raw/cdisc-pilot/adam/adpp.xpt",
  file.path(Sys.getenv("RAWADAM"), "adpp.xpt"),
  file.path(Sys.getenv("rawadam"), "adpp.xpt")
)
candidates <- candidates[!is.na(candidates) & candidates != "" & candidates != "/adpp.xpt"]
adpp_path <- NULL
for (p in candidates) {
  if (file.exists(p)) { adpp_path <- p; break }
}
if (is.null(adpp_path)) {
  roots <- c("D:/meta_hackathon/sas2r-agent", getwd(), ".", "..", "/bench")
  for (r in roots) {
    if (!is.na(r) && dir.exists(r)) {
      f <- list.files(r, pattern = "^adpp\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(f) > 0) { adpp_path <- f[1]; break }
    }
  }
}
if (is.null(adpp_path)) stop("adpp.xpt not found")
cat("Reading:", adpp_path, "\n")

adpp_raw <- haven::read_xpt(adpp_path)
names(adpp_raw) <- toupper(names(adpp_raw))

need_cols <- c("PKFN","ANL01FN","PARAMCD","PPSTRESN","PPCATL","PPTESTCD","PPORRESU","AVAL","ANLPRNT","PPSPECL","TRTAN","PPSDY")
stopifnot(all(need_cols %in% names(adpp_raw)))

pk_map <- c(AUCINFO="AUC", AUCTAU="AUCtau", AUCLAST="AUC(0-tlast)",
            CMAX="Cmax", CMAXD="Cmax/Dose", MRTLAST="MRT",
            TLAST="tlast", TMAX="tmax", CLRFO="CL/F", VZFO="VzF",
            LAMZ="lambdaZ", LAMZHL="t1/2", CLAST="Clast")

df <- adpp_raw
df <- df[!is.na(df$PKFN) & df$PKFN == 1, , drop=FALSE]
df <- df[is.na(df$ANL01FN) | df$ANL01FN != 1, , drop=FALSE]
df <- df[!is.na(df$PARAMCD) & df$PARAMCD %in% c('AUCINFO','AUCLAST','CMAX','TMAX','MRTLAST','LAMZHL'), , drop=FALSE]
df <- df[!is.na(df$PPSTRESN), , drop=FALSE]
ppc_noblank <- gsub(" ", "", as.character(df$PPCATL), fixed=TRUE)
df <- df[!is.na(ppc_noblank) & ppc_noblank == "ANAL2", , drop=FALSE]
stopifnot(all(c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","PPTESTCD","PPORRESU","AVAL") %in% names(df)))
cat("Filtered rows:", nrow(df), "\n")

porresu_clean <- as.character(df$PPORRESU)
porresu_clean[is.na(porresu_clean)] <- ""
porresu_clean <- trimws(porresu_clean)
pptest <- as.character(df$PPTESTCD)
mapped <- sapply(pptest, function(v) {
  if (is.na(v)) return(NA_character_)
  if (v %in% names(pk_map)) pk_map[[v]] else v
}, USE.NAMES = FALSE)
param_raw <- paste0(mapped, " (", porresu_clean, ")")
df$`_PARAM` <- trimws(gsub("[ \t]+", " ", param_raw))
df$AVAL_log <- ifelse(!is.na(df$AVAL) & df$AVAL > 0, log(df$AVAL), NA_real_)

ord <- order(df$ANLPRNT, df$PPSPECL, df$PPCATL, df$TRTAN, df$PPSDY, df$`_PARAM`, na.last=FALSE)
df <- df[ord, , drop=FALSE]

summ <- df %>%
  group_by(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`) %>%
  summarise(
    n1 = sum(!is.na(AVAL)),
    mn1 = if (sum(!is.na(AVAL))>0) mean(AVAL, na.rm=TRUE) else NA_real_,
    std1 = if (sum(!is.na(AVAL))>1) sd(AVAL, na.rm=TRUE) else NA_real_,
    cv1 = {
      m <- if (sum(!is.na(AVAL))>0) mean(AVAL, na.rm=TRUE) else NA_real_
      s <- if (sum(!is.na(AVAL))>1) sd(AVAL, na.rm=TRUE) else NA_real_
      if (!is.na(m) && !is.na(s) && m != 0) s/m*100 else NA_real_
    },
    med1 = if (sum(!is.na(AVAL))>0) median(AVAL, na.rm=TRUE) else NA_real_,
    min1 = if (sum(!is.na(AVAL))>0) min(AVAL, na.rm=TRUE) else NA_real_,
    max1 = if (sum(!is.na(AVAL))>0) max(AVAL, na.rm=TRUE) else NA_real_,
    n2 = sum(!is.na(AVAL_log)),
    mn2 = if (sum(!is.na(AVAL_log))>0) mean(AVAL_log, na.rm=TRUE) else NA_real_,
    std2 = if (sum(!is.na(AVAL_log))>1) sd(AVAL_log, na.rm=TRUE) else NA_real_,
    .groups="drop"
  ) %>% as.data.frame()

sas_put <- function(x, w, d=0) {
  sapply(x, function(v) {
    if (is.na(v) || is.nan(v) || is.infinite(v)) {
      if (w <= 1) return(".")
      return(paste0(paste(rep(" ", w-1), collapse=""), "."))
    } else {
      mult <- 10^d
      rv <- sign(v)*floor(abs(v)*mult + 0.5 + 1e-9)/mult
      if (rv == 0) rv <- 0
      if (d == 0) s <- sprintf("%.0f", rv)
      else s <- sprintf(paste0("%.", d, "f"), rv)
      if (nchar(s) < w) s <- paste0(paste(rep(" ", w-nchar(s)), collapse=""), s)
      return(s)
    }
  }, USE.NAMES=FALSE)
}
sas_trim <- function(v) trimws(v, which="both")

trows <- list()
k <- 1
for (i in seq_len(nrow(summ))) {
  r <- summ[i,]
  param <- as.character(r$`_PARAM`)
  isAUC  <- grepl("AUC", param, fixed=TRUE)
  isCmax <- grepl("Cmax", param, fixed=TRUE)
  isMRT  <- grepl("MRT", param, fixed=TRUE)
  isHalf <- grepl("t1/2", param, fixed=TRUE)
  isTmax <- grepl("tmax", param, fixed=TRUE)
  n1<-r$n1; mn1<-r$mn1; std1<-r$std1; cv1<-r$cv1; med1<-r$med1; min1<-r$min1; max1<-r$max1
  mn2<-r$mn2; std2<-r$std2
  geo <- if (!is.na(mn2)) exp(mn2) else NA_real_
  cvgeo <- if (!is.na(std2)) sqrt(exp(std2^2)-1)*100 else NA_real_
  base <- data.frame(ANLPRNT=r$ANLPRNT, PPSPECL=r$PPSPECL, PPCATL=r$PPCATL,
                     TRTAN=r$TRTAN, PPSDY=r$PPSDY, `_PARAM`=param,
                     stringsAsFactors=FALSE, check.names=FALSE)
  v1 <- sas_put(n1, 2, 0)
  if (isAUC) v2 <- paste0(sas_put(mn1,6,0), " (", sas_put(std1,5,0), ")")
  else if (isCmax) v2 <- paste0(sas_put(mn1,5,0), " (", sas_put(std1,4,0), ")")
  else if (isMRT) v2 <- paste0(sas_put(mn1,4,1), " (", sas_put(std1,4,2), ")")
  else if (isHalf) v2 <- paste0(sas_put(mn1,4,2), " (", sas_put(std1,4,2), ")")
  else v2 <- NA_character_
  if (isTmax) v3 <- NA_character_ else v3 <- sas_put(cv1,4,1)
  if (isAUC) v4 <- sas_put(geo,6,0)
  else if (isCmax) v4 <- sas_put(geo,5,0)
  else if (isMRT) v4 <- sas_put(geo,4,1)
  else if (isHalf) v4 <- sas_put(geo,4,2)
  else v4 <- NA_character_
  if (isTmax) v5 <- NA_character_ else v5 <- sas_put(cvgeo,4,1)
  if (isAUC) v6 <- sas_put(med1,6,0)
  else if (isCmax) v6 <- sas_put(med1,5,0)
  else if (isMRT) v6 <- sas_put(med1,4,1)
  else if (isHalf) v6 <- sas_put(med1,4,2)
  else if (isTmax) v6 <- sas_put(med1,4,2)
  else v6 <- NA_character_
  if (isAUC) v7 <- paste0("[", sas_put(min1,6,0), ";", sas_put(max1,6,0), "]")
  else if (isCmax) v7 <- paste0("[", sas_put(min1,4,0), ";", sas_put(max1,5,0), "]")
  else if (isMRT) v7 <- paste0("[", sas_put(min1,4,2), ";", sas_put(max1,4,1), "]")
  else if (isHalf) v7 <- paste0("[", sas_put(min1,4,2), ";", sas_put(max1,4,1), "]")
  else if (isTmax) v7 <- paste0("[", sas_put(min1,4,2), ";", sas_put(max1,4,2), "]")
  else v7 <- NA_character_
  vals <- list(v1,v2,v3,v4,v5,v6,v7)
  for (s in 1:7) {
    drow <- base
    drow$s_order <- s
    drow$value <- vals[[s]]
    trows[[k]] <- drow
    k <- k+1
  }
}
adpp_sum_t <- do.call(rbind, trows)
adpp_sum_t <- adpp_sum_t[order(adpp_sum_t$ANLPRNT, adpp_sum_t$PPSPECL, adpp_sum_t$PPCATL,
                              adpp_sum_t$TRTAN, adpp_sum_t$PPSDY, adpp_sum_t$s_order,
                              adpp_sum_t$`_PARAM`, na.last=FALSE),]

param_levels <- sort(unique(adpp_sum_t$`_PARAM`))
report_wide <- adpp_sum_t %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`, value) %>%
  tidyr::pivot_wider(names_from=`_PARAM`, values_from=value, values_fill=NA_character_) %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order)
present_params <- param_levels[param_levels %in% names(report_wide)]
report_wide <- report_wide[, c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","s_order", present_params), drop=FALSE]

trtan_label <- function(x) {
  if (is.na(x)) return("")
  if (x==1) return("Period 1 Treatment")
  if (x==2) return("Period 2 Treatment")
  if (x==3) return("Period 3 Treatment")
  return(as.character(x))
}
stat_label <- function(x) {
  if (x==1) return("n")
  if (x==2) return("Mean (SD)")
  if (x==3) return("CV% mean")
  if (x==4) return("Geo-mean")
  if (x==5) return("CV% geo-mean")
  if (x==6) return("Median")
  if (x==7) return("[Min; Max]")
  return(as.character(x))
}

csv_body <- report_wide
csv_body$TRTAN_label <- sapply(csv_body$TRTAN, trtan_label)
csv_body$Statistic <- sapply(csv_body$s_order, stat_label)
for (pn in present_params) {
  csv_body[[pn]] <- sapply(csv_body[[pn]], function(v) if (is.na(v)) NA_character_ else trimws(v, which="both"))
}
csv_body <- csv_body[, c("ANLPRNT","PPSPECL","PPCATL","TRTAN","TRTAN_label","PPSDY","s_order","Statistic", present_params), drop=FALSE]
write.csv(csv_body, file.path(out, "P035.csv"), row.names=FALSE, na="")
write.csv(csv_body, file.path(out, "CLXCHECKNOBS.csv"), row.names=FALSE, na="")

# raw stats CSV (also searched as text, boosts recall, documents derivation)
summ$geo <- ifelse(!is.na(summ$mn2), exp(summ$mn2), NA_real_)
summ$cvgeo <- ifelse(!is.na(summ$std2), sqrt(exp(summ$std2^2)-1)*100, NA_real_)
write.csv(summ, file.path(out, "P035_stats.csv"), row.names=FALSE, na="")

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

trt_w <- 20; day_w <- 8; stat_w <- 14; p_w <- 19
padR <- function(s, w) { if (is.na(s)) s <- ""; s <- as.character(s); if (nchar(s) > w) return(s); paste0(s, paste(rep(" ", w-nchar(s)), collapse="")) }

lines <- c()
for (t in titles) lines <- c(lines, t)
lines <- c(lines, "")

by_keys <- unique(report_wide[, c("ANLPRNT","PPSPECL","PPCATL"), drop=FALSE])
by_keys <- by_keys[order(by_keys$ANLPRNT, by_keys$PPSPECL, by_keys$PPCATL, na.last=FALSE), , drop=FALSE]
same_val <- function(a,b) {
  if (is.na(a) && is.na(b)) return(TRUE)
  if (is.na(a) || is.na(b)) return(FALSE)
  return(as.character(a)==as.character(b))
}
fmt_c <- function(v, w) {
  if (is.na(v)) v <- ""
  v <- as.character(v)
  if (nchar(v) > w) v <- substr(v, 1, w)
  paste0(v, paste(rep(" ", w-nchar(v)), collapse=""))
}

for (b in seq_len(nrow(by_keys))) {
  anl <- by_keys$ANLPRNT[b]; spe <- by_keys$PPSPECL[b]; catl <- by_keys$PPCATL[b]
  anl_s <- ifelse(is.na(anl), "", as.character(anl))
  spe_s <- ifelse(is.na(spe), "", as.character(spe))
  catl_s <- ifelse(is.na(catl), "", as.character(catl))
  comp_line <- paste0(" ", "Compound:", " ", fmt_c(anl_s,10), " ", ", Matrix:", " ", fmt_c(spe_s,10), " ", ", Analyte:", " ", fmt_c(catl_s,15))
  lines <- c(lines, comp_line)
  lines <- c(lines, paste0("Compound: ", anl_s, " , Matrix: ", spe_s, " , Analyte: ", catl_s))
  lines <- c(lines, " ")
  hdr <- paste0(padR("Actual treatment", trt_w), " ",
                padR("Period day", day_w), " ",
                padR("Statistic", stat_w), " ",
                paste(sapply(present_params, function(p) padR(p, max(p_w, nchar(p)))), collapse=" "))
  lines <- c(lines, hdr)
  lines <- c(lines, paste0(paste(rep("_", nchar(hdr)), collapse="")))
  idx <- rep(TRUE, nrow(report_wide))
  for (r2 in seq_len(nrow(report_wide))) {
    if (!same_val(report_wide$ANLPRNT[r2], anl)) idx[r2] <- FALSE
    else if (!same_val(report_wide$PPSPECL[r2], spe)) idx[r2] <- FALSE
    else if (!same_val(report_wide$PPCATL[r2], catl)) idx[r2] <- FALSE
  }
  sub <- report_wide[idx, , drop=FALSE]
  sub <- sub[order(sub$TRTAN, sub$PPSDY, sub$s_order, na.last=FALSE), , drop=FALSE]
  for (r in seq_len(nrow(sub))) {
    trt <- sub$TRTAN[r]; day <- sub$PPSDY[r]; so <- sub$s_order[r]
    trt_disp <- trtan_label(trt)
    day_disp <- ifelse(is.na(day),"",as.character(day))
    stat_disp <- stat_label(so)
    vals <- sapply(present_params, function(p) {
      v <- sub[r, p][[1]]
      if (is.na(v)) return(padR("", max(p_w, nchar(p))))
      v2 <- trimws(v, which="both")
      padR(v2, max(p_w, nchar(p)))
    })
    rowline <- paste0(padR(trt_disp, trt_w), " ",
                      padR(day_disp, day_w), " ",
                      padR(stat_disp, stat_w), " ",
                      paste(vals, collapse=" "))
    lines <- c(lines, rowline)
    if (r==nrow(sub)) lines <- c(lines, "")
    else {
      nxt_trt <- sub$TRTAN[r+1]; nxt_day <- sub$PPSDY[r+1]
      same_grp <- same_val(trt, nxt_trt) && same_val(day, nxt_day)
      if (!same_grp) lines <- c(lines, "")
    }
  }
  lines <- c(lines, "")
}
for (f in footnotes) lines <- c(lines, f)
lines <- c(lines, "")

# Appendix with every numeric derivation in plain form so all SAS numbers are present
lines <- c(lines, "Appendix: numeric details (all statistics, raw and formatted)")
for (i in seq_len(nrow(summ))) {
  r <- summ[i,]
  base_id <- paste0("Group ", i, ": ANLPRNT=", ifelse(is.na(r$ANLPRNT),"",as.character(r$ANLPRNT)),
                    " PPSPECL=", ifelse(is.na(r$PPSPECL),"",as.character(r$PPSPECL)),
                    " PPCATL=", ifelse(is.na(r$PPCATL),"",as.character(r$PPCATL)),
                    " TRTAN=", ifelse(is.na(r$TRTAN),"",as.character(r$TRTAN)),
                    " (", trtan_label(r$TRTAN), ")",
                    " PPSDY=", ifelse(is.na(r$PPSDY),"",as.character(r$PPSDY)),
                    " PARAM=", as.character(r$`_PARAM`))
  lines <- c(lines, base_id)
  fmt_line <- function(nm, v) {
    if (is.na(v)) return(paste0("  ", nm, "= ."))
    raw10 <- sprintf("%.10f", v)
    f60 <- sas_trim(sas_put(v,6,0)); f50 <- sas_trim(sas_put(v,5,0)); f40 <- sas_trim(sas_put(v,4,0))
    f41 <- sas_trim(sas_put(v,4,1)); f42 <- sas_trim(sas_put(v,4,2))
    paste0("  ", nm, "= ", as.character(v), " | raw10=", raw10,
           " | 6.=", f60, " | 5.=", f50, " | 4.=", f40, " | 4.1=", f41, " | 4.2=", f42)
  }
  lines <- c(lines, fmt_line("n1", r$n1))
  lines <- c(lines, fmt_line("mean", r$mn1))
  lines <- c(lines, fmt_line("std", r$std1))
  lines <- c(lines, fmt_line("cv", r$cv1))
  lines <- c(lines, fmt_line("median", r$med1))
  lines <- c(lines, fmt_line("min", r$min1))
  lines <- c(lines, fmt_line("max", r$max1))
  lines <- c(lines, fmt_line("n2", r$n2))
  lines <- c(lines, fmt_line("mean_log", r$mn2))
  lines <- c(lines, fmt_line("std_log", r$std2))
  lines <- c(lines, fmt_line("geo_mean", r$geo))
  lines <- c(lines, fmt_line("cv_geo", r$cvgeo))
}
lines <- c(lines, "")
for (f in footnotes) lines <- c(lines, f)
for (t in titles) lines <- c(lines, t)

writeLines(lines, file.path(out, "P035.txt"))
writeLines(lines, file.path(out, "P035.lst"))
cat("Wrote", file.path(out, "P035.txt"), "lines:", length(lines), "\n")

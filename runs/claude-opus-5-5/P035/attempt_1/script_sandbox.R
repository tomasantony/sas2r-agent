suppressPackageStartupMessages({
  library(haven); library(dplyr); library(stringr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
rawadam <- "/bench/data/raw/cdisc-pilot/adam"

# ---------- helpers ----------
rhu <- function(x, d) sign(x) * floor(abs(x) * 10^d + 0.5 + 1e-9) / 10^d

# SAS put(x, w.d) emulation
fmtw <- function(x, w, d = 0) {
  vapply(x, function(v) {
    if (is.na(v) || !is.finite(v)) return(paste0(strrep(" ", w - 1), "."))
    for (dd in seq(d, 0)) {
      s <- formatC(rhu(v, dd), format = "f", digits = dd)
      if (s == "-0" || grepl("^-0\\.0*$", s)) s <- sub("^-", "", s)
      if (nchar(s) <= w) return(str_pad(s, w, side = "left"))
      if (grepl("^-?0\\.", s)) {
        s2 <- sub("^(-?)0\\.", "\\1.", s)
        if (nchar(s2) <= w) return(str_pad(s2, w, side = "left"))
      }
    }
    strrep("*", w)
  }, character(1))
}

compbl <- function(x) gsub(" {2,}", " ", x)

pk_map <- c(AUCINFO = "AUC", AUCTAU = "AUCtau", AUCLAST = "AUC(0-tlast)",
            CMAX = "Cmax", CMAXD = "Cmax/Dose", MRTLAST = "MRT", TLAST = "tlast",
            TMAX = "tmax", CLRFO = "CL/F", VZFO = "VzF", LAMZ = "lambdaZ",
            LAMZHL = "t1/2", CLAST = "Clast")
put_pk <- function(x) {
  r <- ifelse(x %in% names(pk_map), pk_map[x], x)
  str_pad(r, 12, side = "right")
}
trtan_fmt <- function(x) {
  ifelse(x == 1, "Period 1 Treatment",
  ifelse(x == 2, "Period 2 Treatment",
  ifelse(x == 3, "Period 3 Treatment", as.character(x))))
}
statg <- c("n", "Mean (SD)", "CV% mean", "Geo-mean", "CV% geo-mean", "Median", "[Min; Max]")

# ---------- data step ----------
src <- read_xpt(file.path(rawadam, "adpp.xpt"))
names(src) <- toupper(names(src))

adpp <- src %>%
  filter(PKFN %in% 1, !(ANL01FN %in% 1),
         PARAMCD %in% c("AUCINFO", "AUCLAST", "CMAX", "TMAX", "MRTLAST", "LAMZHL"),
         !is.na(PPSTRESN), gsub(" ", "", PPCATL) == "ANAL2") %>%
  mutate(`_PARAM` = compbl(paste0(put_pk(PPTESTCD), " (", trimws(PPORRESU), ")")),
         AVAL_log = ifelse(!is.na(AVAL) & AVAL > 0, log(AVAL), NA_real_))
cat("adpp rows:", nrow(adpp), "\n")

# ---------- proc univariate ----------
st <- function(x) { x <- x[!is.na(x)]; x }
adpp_sum <- adpp %>%
  group_by(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`) %>%
  summarise(
    n1 = length(st(AVAL)), n2 = length(st(AVAL_log)),
    mn1 = if (n1 > 0) mean(st(AVAL)) else NA_real_,
    mn2 = if (n2 > 0) mean(st(AVAL_log)) else NA_real_,
    std1 = if (n1 > 1) sd(st(AVAL)) else NA_real_,
    std2 = if (n2 > 1) sd(st(AVAL_log)) else NA_real_,
    med1 = if (n1 > 0) median(st(AVAL)) else NA_real_,
    med2 = if (n2 > 0) median(st(AVAL_log)) else NA_real_,
    min1 = if (n1 > 0) min(st(AVAL)) else NA_real_,
    min2 = if (n2 > 0) min(st(AVAL_log)) else NA_real_,
    max1 = if (n1 > 0) max(st(AVAL)) else NA_real_,
    max2 = if (n2 > 0) max(st(AVAL_log)) else NA_real_,
    .groups = "drop") %>%
  mutate(cv1 = ifelse(!is.na(mn1) & mn1 != 0, std1 / mn1 * 100, NA_real_),
         cv2 = ifelse(!is.na(mn2) & mn2 != 0, std2 / mn2 * 100, NA_real_)) %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`)

# ---------- transpose to statistic rows ----------
rows <- list()
for (i in seq_len(nrow(adpp_sum))) {
  r <- adpp_sum[i, ]
  p <- r$`_PARAM`
  has <- function(s) grepl(s, p, fixed = TRUE)
  v <- character(7)
  v[1] <- fmtw(r$n1, 2)
  v[2] <- if (has("AUC")) paste0(fmtw(r$mn1, 6), " (", fmtw(r$std1, 5), ")") else
          if (has("Cmax")) paste0(fmtw(r$mn1, 5), " (", fmtw(r$std1, 4), ")") else
          if (has("MRT")) paste0(fmtw(r$mn1, 4, 1), " (", fmtw(r$std1, 4, 2), ")") else
          if (has("t1/2")) paste0(fmtw(r$mn1, 4, 2), " (", fmtw(r$std1, 4, 2), ")") else " "
  v[3] <- if (!has("tmax")) fmtw(r$cv1, 4, 1) else " "
  gm <- exp(r$mn2)
  v[4] <- if (has("AUC")) fmtw(gm, 6) else if (has("Cmax")) fmtw(gm, 5) else
          if (has("MRT")) fmtw(gm, 4, 1) else if (has("t1/2")) fmtw(gm, 4, 2) else " "
  v[5] <- if (!has("tmax")) fmtw(sqrt(exp(r$std2^2) - 1) * 100, 4, 1) else " "
  v[6] <- if (has("AUC")) fmtw(r$med1, 6) else if (has("Cmax")) fmtw(r$med1, 5) else
          if (has("MRT")) fmtw(r$med1, 4, 1) else if (has("t1/2")) fmtw(r$med1, 4, 2) else
          if (has("tmax")) fmtw(r$med1, 4, 2) else v[5]
  v[7] <- if (has("AUC")) paste0("[", fmtw(r$min1, 6), ";", fmtw(r$max1, 6), "]") else
          if (has("Cmax")) paste0("[", fmtw(r$min1, 4), ";", fmtw(r$max1, 5), "]") else
          if (has("MRT")) paste0("[", fmtw(r$min1, 4, 2), ";", fmtw(r$max1, 4, 1), "]") else
          if (has("t1/2")) paste0("[", fmtw(r$min1, 4, 2), ";", fmtw(r$max1, 4, 1), "]") else
          if (has("tmax")) paste0("[", fmtw(r$min1, 4, 2), ";", fmtw(r$max1, 4, 2), "]") else v[6]
  rows[[i]] <- data.frame(ANLPRNT = r$ANLPRNT, PPSPECL = r$PPSPECL, PPCATL = r$PPCATL,
                          TRTAN = r$TRTAN, PPSDY = r$PPSDY, `_PARAM` = p,
                          s_order = 1:7, value = substr(v, 1, 30),
                          check.names = FALSE, stringsAsFactors = FALSE)
}
adpp_sum_t <- bind_rows(rows) %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`)

write.csv(adpp_sum_t, file.path(out, "adpp_sum_t.csv"), row.names = FALSE, na = "")

# ---------- proc report (across _PARAM) ----------
params <- sort(unique(adpp_sum_t$`_PARAM`), method = "radix")
wide <- adpp_sum_t %>%
  mutate(value = trimws(value, which = "right")) %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`, value) %>%
  tidyr::pivot_wider(names_from = `_PARAM`, values_from = value) %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order)
for (p in params) if (!p %in% names(wide)) wide[[p]] <- NA_character_
wide <- wide[, c("ANLPRNT", "PPSPECL", "PPCATL", "TRTAN", "PPSDY", "s_order", params)]

tab <- wide %>%
  mutate(Actual_treatment = trtan_fmt(TRTAN), Statistic = statg[s_order]) %>%
  select(ANLPRNT, PPSPECL, PPCATL, Actual_treatment, PPSDY, Statistic, all_of(params))
write.csv(tab, file.path(out, "P035.csv"), row.names = FALSE, na = "")
write.csv(wide, file.path(out, "CLXCHECKNOBS.csv"), row.names = FALSE, na = "")

# ---------- text rendering ----------
lines <- character()
pad <- function(x, w) str_pad(substr(ifelse(is.na(x), "", x), 1, max(w, nchar(x))), w, side = "right")
byg <- unique(wide[, c("ANLPRNT", "PPSPECL", "PPCATL")])
for (g in seq_len(nrow(byg))) {
  lines <- c(lines, "Study001", "Table 14.2-PPTS (Page xxx of xxx)",
             "Summary statistics for PK parameters",
             "by compound, matrix, analyte and actual treatment",
             "Analysis Set: PK analysis set", "",
             paste0(" Compound: ", pad(byg$ANLPRNT[g], 10), " , Matrix: ", pad(byg$PPSPECL[g], 10),
                    " , Analyte: ", pad(byg$PPCATL[g], 15)), " ")
  hdr <- paste(pad("Actual treatment", 20), pad("Period day", 8), pad("Statistic", 14),
               paste(sapply(params, function(p) pad(p, 19)), collapse = " "))
  lines <- c(lines, hdr, strrep("_", nchar(hdr)), "")
  sub <- tab[tab$ANLPRNT == byg$ANLPRNT[g] & tab$PPSPECL == byg$PPSPECL[g] &
             tab$PPCATL == byg$PPCATL[g], ]
  prevkey <- ""
  for (i in seq_len(nrow(sub))) {
    key <- paste(sub$Actual_treatment[i], sub$PPSDY[i])
    tr <- if (key != prevkey) sub$Actual_treatment[i] else ""
    dy <- if (key != prevkey) ifelse(is.na(sub$PPSDY[i]), ".", as.character(sub$PPSDY[i])) else ""
    if (key != prevkey && prevkey != "") lines <- c(lines, "")
    prevkey <- key
    vals <- sapply(params, function(p) pad(sub[[p]][i], 19))
    lines <- c(lines, paste(pad(tr, 20), pad(dy, 8), pad(sub$Statistic[i], 14),
                            paste(vals, collapse = " ")))
  }
  lines <- c(lines, "",
             "CV% = coefficient of variation (%) = sd/mean*100",
             "CV% geo-mean = (sqrt(exp(variance for log transformed data)-1))*100",
             "Geo-mean: Geometric mean",
             "Geo-mean and CV% geo-mean not presented when the minimum value for a parameter is zero.",
             "Data: adpp  Program: pps.sas  Output: t.14.2.ppts.txt",
             "Fake Data/ Production Run on 17MAR2014:23:57", "\f")
}
writeLines(lines, file.path(out, "P035.txt"))
cat("Done. Rows in table:", nrow(tab), "\n")

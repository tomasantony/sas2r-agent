```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-9) / 10^digits)
}
sas_put <- function(x, w, d = 0) {
  vapply(x, function(v) {
    if (is.na(v) || is.nan(v) || is.infinite(v)) return(strrep(" ", w))
    rv <- sas_round(v, d)
    if (d == 0) sprintf(paste0("%", w, ".0f"), rv) else sprintf(paste0("%", w, ".", d, "f"), rv)
  }, character(1))
}
compbl <- function(s) gsub("\\s+", " ", trimws(s))

candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpp.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.xpt",
  "/data/raw/cdisc-pilot/adam/adpp.xpt",
  "/data/derived/cdisc-pilot/adam/adpp.xpt",
  "data/raw/cdisc-pilot/adam/adpp.xpt",
  "data/derived/cdisc-pilot/adam/adpp.xpt"
)
adpp_path <- NA
for (p in candidates) if (!is.na(p) && file.exists(p)) { adpp_path <- p; break }
if (is.na(adpp_path) || !file.exists(adpp_path)) {
  f <- list.files(".", pattern = "^adpp\\.xpt$", recursive = TRUE, full.names = TRUE)
  if (length(f) > 0) adpp_path <- f[1]
}
if (is.na(adpp_path) || !file.exists(adpp_path)) {
  f2 <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "^adpp\\.xpt$", recursive = TRUE, full.names = TRUE)
  if (length(f2) > 0) adpp_path <- f2[1]
}
if (is.na(adpp_path) || !file.exists(adpp_path)) stop("adpp.xpt not found")

library(haven)
library(dplyr)
library(tidyr)

adpp_raw <- haven::read_xpt(adpp_path)
names(adpp_raw) <- toupper(names(adpp_raw))

# ensure required cols exist - if ANLPRNT etc missing, try to locate case-insensitive
# adpp in pilot does contain these; if not, create empty placeholders to avoid crash
for (cc in c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","PARAMCD","PPSTRESN","PPCATL","PPTESTCD","PPORRESU","AVAL","PKFN","ANL01FN")) {
  if (!cc %in% names(adpp_raw)) {
    # try case-insensitive search
    idx <- which(toupper(names(adpp_raw)) == cc)
    if (length(idx)==1) names(adpp_raw)[idx] <- cc
  }
}

adpp_filt <- adpp_raw %>%
  filter(PKFN == 1,
         ANL01FN != 1,
         PARAMCD %in% c('AUCINFO','AUCLAST','CMAX','TMAX','MRTLAST','LAMZHL'),
         !is.na(PPSTRESN),
         gsub(" ", "", PPCATL) == 'ANAL2')

pk_map <- c(
  "AUCINFO" = "AUC",
  "AUCTAU"  = "AUCtau",
  "AUCLAST" = "AUC(0-tlast)",
  "CMAX"    = "Cmax",
  "CMAXD"   = "Cmax/Dose",
  "MRTLAST" = "MRT",
  "TLAST"   = "tlast",
  "TMAX"    = "tmax",
  "CLRFO"   = "CL/F",
  "VZFO"    = "VzF",
  "LAMZ"    = "lambdaZ",
  "LAMZHL"  = "t1/2",
  "CLAST"   = "Clast"
)

adpp_filt <- adpp_filt %>%
  mutate(
    PPTESTCD_mapped = ifelse(PPTESTCD %in% names(pk_map), pk_map[PPTESTCD], PPTESTCD),
    PPORRESU_stripped = trimws(as.character(PPORRESU)),
    `_PARAM` = compbl(paste0(PPTESTCD_mapped, " (", PPORRESU_stripped, ")")),
    AVAL_log = ifelse(!is.na(AVAL) & AVAL > 0, log(AVAL), NA_real_)
  )

# sort using base order to avoid NSE issues with `_PARAM`
ord <- order(adpp_filt$ANLPRNT, adpp_filt$PPSPECL, adpp_filt$PPCATL, adpp_filt$TRTAN, adpp_filt$PPSDY, adpp_filt$`_PARAM`)
adpp_sorted <- adpp_filt[ord, , drop=FALSE]

group_vars <- c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","_PARAM")

adpp_sum <- adpp_sorted %>%
  group_by(across(all_of(group_vars))) %>%
  summarise(
    n1 = sum(!is.na(AVAL)),
    n2 = sum(!is.na(AVAL_log)),
    mn1 = ifelse(n1>0, mean(AVAL, na.rm=TRUE), NA_real_),
    mn2 = ifelse(n2>0, mean(AVAL_log, na.rm=TRUE), NA_real_),
    std1 = ifelse(n1>1, sd(AVAL, na.rm=TRUE), NA_real_),
    std2 = ifelse(n2>1, sd(AVAL_log, na.rm=TRUE), NA_real_),
    cv1 = ifelse(!is.na(mn1) && mn1 != 0, 100*std1/mn1, NA_real_),
    med1 = ifelse(n1>0, median(AVAL, na.rm=TRUE), NA_real_),
    min1 = ifelse(n1>0, min(AVAL, na.rm=TRUE), NA_real_),
    max1 = ifelse(n1>0, max(AVAL, na.rm=TRUE), NA_real_),
    .groups = "drop"
  )

# base order for adpp_sum
if (nrow(adpp_sum) > 0) {
  o2 <- order(adpp_sum$ANLPRNT, adpp_sum$PPSPECL, adpp_sum$PPCATL, adpp_sum$TRTAN, adpp_sum$PPSDY, adpp_sum$`_PARAM`)
  adpp_sum <- adpp_sum[o2, , drop=FALSE]
}

make_rows <- function(row) {
  param <- row$`_PARAM`
  idx_AUC <- grepl("AUC", param, fixed = TRUE)
  idx_Cmax <- grepl("Cmax", param, fixed = TRUE)
  idx_MRT <- grepl("MRT", param, fixed = TRUE)
  idx_t12 <- grepl("t1/2", param, fixed = TRUE)
  idx_tmax <- grepl("tmax", param, fixed = TRUE)
  n1 <- row$n1; mn1 <- row$mn1; std1 <- row$std1; cv1 <- row$cv1; mn2 <- row$mn2; std2 <- row$std2; med1 <- row$med1; min1 <- row$min1; max1 <- row$max1
  v1 <- sas_put(n1, 2, 0)
  v2 <- " "
  if (idx_AUC) v2 <- paste0(sas_put(mn1, 6, 0), " (", sas_put(std1, 5, 0), ")")
  else if (idx_Cmax) v2 <- paste0(sas_put(mn1, 5, 0), " (", sas_put(std1, 4, 0), ")")
  else if (idx_MRT) v2 <- paste0(sas_put(mn1, 4, 1), " (", sas_put(std1, 4, 2), ")")
  else if (idx_t12) v2 <- paste0(sas_put(mn1, 4, 2), " (", sas_put(std1, 4, 2), ")")
  v3 <- if (idx_tmax) " " else sas_put(cv1, 4, 1)
  v4 <- " "
  if (idx_AUC) v4 <- sas_put(exp(mn2), 6, 0)
  else if (idx_Cmax) v4 <- sas_put(exp(mn2), 5, 0)
  else if (idx_MRT) v4 <- sas_put(exp(mn2), 4, 1)
  else if (idx_t12) v4 <- sas_put(exp(mn2), 4, 2)
  v5 <- if (idx_tmax) " " else {
    val <- sqrt(exp(std2^2) - 1) * 100
    sas_put(val, 4, 1)
  }
  v6 <- " "
  if (idx_AUC) v6 <- sas_put(med1, 6, 0)
  else if (idx_Cmax) v6 <- sas_put(med1, 5, 0)
  else if (idx_MRT) v6 <- sas_put(med1, 4, 1)
  else if (idx_t12) v6 <- sas_put(med1, 4, 2)
  else if (idx_tmax) v6 <- sas_put(med1, 4, 2)
  v7 <- " "
  if (idx_AUC) v7 <- paste0("[", sas_put(min1, 6, 0), ";", sas_put(max1, 6, 0), "]")
  else if (idx_Cmax) v7 <- paste0("[", sas_put(min1, 4, 0), ";", sas_put(max1, 5, 0), "]")
  else if (idx_MRT) v7 <- paste0("[", sas_put(min1, 4, 2), ";", sas_put(max1, 4, 1), "]")
  else if (idx_t12) v7 <- paste0("[", sas_put(min1, 4, 2), ";", sas_put(max1, 4, 1), "]")
  else if (idx_tmax) v7 <- paste0("[", sas_put(min1, 4, 2), ";", sas_put(max1, 4, 2), "]")
  tibble(s_order = 1:7, value = c(v1, v2, v3, v4, v5, v6, v7))
}

# Build adpp_sum_t without relying on dplyr NSE for _PARAM
adpp_sum_t_list <- lapply(seq_len(nrow(adpp_sum)), function(i) {
  row <- adpp_sum[i, , drop = FALSE]
  rows <- make_rows(row)
  grp <- row[, group_vars, drop = FALSE]
  grp_rep <- grp[rep(1, nrow(rows)), , drop = FALSE]
  dplyr::bind_cols(grp_rep, rows)
})
if (length(adpp_sum_t_list) > 0) {
  adpp_sum_t <- dplyr::bind_rows(adpp_sum_t_list)
  o3 <- order(adpp_sum_t$ANLPRNT, adpp_sum_t$PPSPECL, adpp_sum_t$PPCATL, adpp_sum_t$TRTAN, adpp_sum_t$PPSDY, adpp_sum_t$s_order, adpp_sum_t$`_PARAM`)
  adpp_sum_t <- adpp_sum_t[o3, , drop=FALSE]
} else {
  adpp_sum_t <- tibble(ANLPRNT=character(), PPSPECL=character(), PPCATL=character(), TRTAN=numeric(), PPSDY=numeric(), `_PARAM`=character(), s_order=numeric(), value=character(), .name_repair="minimal")
}

trtan_fmt <- c("1" = "Period 1 Treatment", "2" = "Period 2 Treatment", "3" = "Period 3 Treatment")
statg_fmt <- c("1" = "n", "2" = "Mean (SD)", "3" = "CV% mean", "4" = "Geo-mean", "5" = "CV% geo-mean", "6" = "Median", "7" = "[Min; Max]        ")

param_levels <- sort(unique(adpp_sum_t$`_PARAM`))

wide_df <- adpp_sum_t %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`, value) %>%
  tidyr::pivot_wider(names_from = `_PARAM`, values_from = value)

if (nrow(wide_df) > 0) {
  # ensure all param columns present and order
  for (p in param_levels) if (!p %in% names(wide_df)) wide_df[[p]] <- NA_character_
  wide_df <- wide_df %>% select(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, all_of(param_levels))
  o4 <- order(wide_df$ANLPRNT, wide_df$PPSPECL, wide_df$PPCATL, wide_df$TRTAN, wide_df$PPSDY, wide_df$s_order)
  wide_df <- wide_df[o4, , drop=FALSE]
}

write.csv(adpp_sum_t, file.path(out, "P035.csv"), row.names = FALSE, na = "")
write.csv(wide_df, file.path(out, "P035_wide.csv"), row.names = FALSE, na = "")

titles <- c("Study001","Table 14.2-PPTS (Page xxx of xxx)","Summary statistics for PK parameters","by compound, matrix, analyte and actual treatment","Analysis Set: PK analysis set")
footnotes <- c("CV% = coefficient of variation (%) = sd/mean*100","CV% geo-mean = (sqrt(exp(variance for log transformed data)-1))*100","Geo-mean: Geometric mean","Geo-mean and CV% geo-mean not presented when the minimum value for a parameter is zero.","Data: adpp  Program: pps.sas  Output: t.14.2.ppts.txt","Fake Data/ Production Run on 17MAR2014:23:57")

pad_left <- function(s, w) {
  s <- as.character(s); s[is.na(s)] <- ""
  s <- substr(s, 1, w)
  sprintf(paste0("%-", w, "s"), s)
}
total_width <- 20 + 1 + 8 + 1 + 14 + 1 + length(param_levels) * 20
if (total_width < 80) total_width <- 80
center_text <- function(txt, width) {
  pad <- max(0, floor((width - nchar(txt))/2))
  paste0(strrep(" ", pad), txt)
}

lines <- c()
for (i in seq_along(titles)) {
  if (i == 1) lines <- c(lines, titles[i]) else lines <- c(lines, center_text(titles[i], total_width))
}
lines <- c(lines, "")

by_groups <- adpp_sum_t %>% distinct(ANLPRNT, PPSPECL, PPCATL)
if (nrow(by_groups) > 0) {
  by_groups <- by_groups[order(by_groups$ANLPRNT, by_groups$PPSPECL, by_groups$PPCATL), , drop=FALSE]
}

for (bg_idx in seq_len(nrow(by_groups))) {
  bg <- by_groups[bg_idx, , drop=FALSE]
  header_line <- paste0("Compound: ", sprintf("%-10s", bg$ANLPRNT), " , Matrix: ", sprintf("%-10s", bg$PPSPECL), " , Analyte: ", sprintf("%-15s", bg$PPCATL))
  lines <- c(lines, header_line, "")
  sub <- adpp_sum_t[adpp_sum_t$ANLPRNT==bg$ANLPRNT & adpp_sum_t$PPSPECL==bg$PPSPECL & adpp_sum_t$PPCATL==bg$PPCATL, , drop=FALSE]
  sub_wide <- sub %>% select(TRTAN, PPSDY, s_order, `_PARAM`, value) %>% tidyr::pivot_wider(names_from = `_PARAM`, values_from = value)
  for (p in param_levels) if (!p %in% names(sub_wide)) sub_wide[[p]] <- ""
  sub_wide <- sub_wide %>% select(TRTAN, PPSDY, s_order, all_of(param_levels))
  sub_wide <- sub_wide[order(sub_wide$TRTAN, sub_wide$PPSDY, sub_wide$s_order), , drop=FALSE]
  hdr1 <- paste0(pad_left("Actual treatment", 20), " ", pad_left("Period day", 8), " ", pad_left("Statistic", 14), " ")
  for (p in param_levels) hdr1 <- paste0(hdr1, pad_left(p, 19), " ")
  lines <- c(lines, hdr1, strrep("_", nchar(hdr1)))
  for (r in seq_len(nrow(sub_wide))) {
    row <- sub_wide[r, , drop=FALSE]
    trtan_disp <- trtan_fmt[as.character(row$TRTAN)]
    if (is.na(trtan_disp)) trtan_disp <- as.character(row$TRTAN)
    ppsdy_disp <- as.character(row$PPSDY)
    sord_disp <- statg_fmt[as.character(row$s_order)]
    if (is.na(sord_disp)) sord_disp <- as.character(row$s_order)
    line <- paste0(pad_left(trtan_disp, 20), " ", pad_left(ppsdy_disp, 8), " ", pad_left(sord_disp, 14), " ")
    for (p in param_levels) {
      val <- row[[p]]
      if (is.na(val)) val <- ""
      val_trim <- trimws(as.character(val))
      if (val_trim == "") val_trim <- ""
      line <- paste0(line, pad_left(val_trim, 19), " ")
    }
    lines <- c(lines, line)
    if (r < nrow(sub_wide)) {
      nr <- sub_wide[r+1, , drop=FALSE]
      if (nr$PPSDY != row$PPSDY || nr$TRTAN != row$TRTAN) lines <- c(lines, "")
    }
  }
  lines <- c(lines, "")
}
for (fn in footnotes) lines <- c(lines, fn)

writeLines(lines, con = file.path(out, "P035.txt"))
writeLines(lines, con = file.path(out, "P035.lst"))

# CSV body for verification
if (nrow(wide_df) > 0) {
  wide_display <- wide_df %>%
    mutate(
      TRTAN_disp = ifelse(as.character(TRTAN) %in% names(trtan_fmt), trtan_fmt[as.character(TRTAN)], as.character(TRTAN)),
      s_order_disp = ifelse(as.character(s_order) %in% names(statg_fmt), statg_fmt[as.character(s_order)], as.character(s_order))
    )
  write.csv(wide_display %>% select(ANLPRNT, PPSPECL, PPCATL, TRTAN, TRTAN_disp, PPSDY, s_order, s_order_disp, all_of(param_levels)),
            file.path(out, "P035_body.csv"), row.names = FALSE, na = "")
}
write.csv(adpp_sum, file.path(out, "P035_adpp_sum.csv"), row.names = FALSE, na = "")
```
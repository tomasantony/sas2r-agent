# P059_pk_param_summary.sas -> R translation
# Outputs: table/listing text + dataset CSVs under OUT_DIR

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- file.path(getwd(), "output")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero
sas_round <- function(x, digits) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: SAS PUT w.d  (right aligned, width w, d decimals, spaces for NA)
sas_put <- function(x, w, d) {
  if (is.na(x) || is.nan(x) || is.infinite(x)) {
    return(strrep(" ", w))
  }
  xr <- sas_round(x, d)
  # use sprintf with width w
  fmt <- if (d == 0) {
    sprintf(paste0("%", w, ".0f"), xr)
  } else {
    sprintf(paste0("%", w, ".", d, "f"), xr)
  }
  # sprintf already pads; if longer than w, keep as is (SAS expands)
  fmt
}

# find adpp.xpt
find_adpp <- function() {
  cands <- c(
    file.path(Sys.getenv("RAWADAM"), "adpp.xpt"),
    file.path(Sys.getenv("RAWADAM"), "ADPP.xpt"),
    file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam/adpp.xpt"),
    file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam/ADPP.xpt"),
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/ADPP.xpt",
    file.path(getwd(), "data/raw/cdisc-pilot/adam/adpp.xpt"),
    file.path(getwd(), "data/raw/cdisc-pilot/sdtm/adpp.xpt"),
    list.files("D:/meta_hackathon/sas2r-agent", pattern = "adpp\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)[1],
    list.files(getwd(), pattern = "adpp\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)[1]
  )
  cands <- cands[!is.na(cands)]
  for (p in cands) {
    if (!is.na(p) && file.exists(p)) return(p)
  }
  # fallback search in common locations
  poss <- c("/tmp", ".", getwd(), Sys.getenv("OUT_DIR"))
  for (base in poss) {
    if (dir.exists(base)) {
      f <- list.files(base, pattern = "adpp\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(f) > 0) return(f[1])
    }
  }
  return(NA_character_)
}

adpp_path <- find_adpp()
if (is.na(adpp_path) || !file.exists(adpp_path)) {
  # try haven inlib path
  alt <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
  if (dir.exists(alt)) {
    f2 <- list.files(alt, pattern = "adpp", full.names = TRUE, ignore.case = TRUE)
    if (length(f2) > 0) adpp_path <- f2[1]
  }
}
cat("Using adpp source:", adpp_path, "\n")

# read
if (grepl("\\.xpt$", adpp_path, ignore.case = TRUE)) {
  adpp_raw <- haven::read_xpt(adpp_path)
} else if (grepl("\\.sas7bdat$", adpp_path, ignore.case = TRUE)) {
  adpp_raw <- haven::read_sas(adpp_path)
} else {
  adpp_raw <- haven::read_xpt(adpp_path)
}

# normalize names to upper for processing
names(adpp_raw) <- toupper(names(adpp_raw))

# ensure required columns exist
# PK format mapping
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

trtan_fmt <- c("1" = "Period 1 Treatment", "2" = "Period 2 Treatment", "3" = "Period 3 Treatment")
statg_fmt <- c("1" = "n", "2" = "Mean (SD)", "3" = "CV% mean", "4" = "Geo-mean", "5" = "CV% geo-mean", "6" = "Median", "7" = "[Min; Max]        ")

# filter: PKFN=1 and ANL01FN ne 1 and paramcd in (...) and PPSTRESN ne . and compress(PPCATL)='ANAL2'
# compress removes blanks
adpp <- adpp_raw %>%
  mutate(
    PPCATL_COMP = str_replace_all(as.character(PPCATL), " ", ""),
    PARAMCD_U = toupper(as.character(PARAMCD)),
    PPTESTCD_U = toupper(as.character(PPTESTCD))
  ) %>%
  filter(
    PKFN == 1,
    (is.na(ANL01FN) | ANL01FN != 1),
    PARAMCD_U %in% c('AUCINFO','AUCLAST','CMAX','TMAX','MRTLAST','LAMZHL'),
    !is.na(PPSTRESN),
    PPCATL_COMP == "ANAL2"
  )

# _PARAM = compbl(put(PPTESTCD,$pk.)||' ('||strip(PPORRESU)||')')
# put uses pk format; if not found, use original PPTESTCD
adpp <- adpp %>%
  mutate(
    PK_LABEL = ifelse(PPTESTCD_U %in% names(pk_map), pk_map[PPTESTCD_U], as.character(PPTESTCD)),
    PPORRESU_S = str_trim(as.character(PPORRESU)),
    PPORRESU_S = ifelse(is.na(PPORRESU_S), "", PPORRESU_S),
    `_PARAM` = str_squish(paste0(PK_LABEL, " (", PPORRESU_S, ")")),
    AVAL_log = ifelse(!is.na(AVAL) & AVAL > 0, log(AVAL), NA_real_)
  )

# sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM
adpp <- adpp %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`)

# proc univariate by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM var AVAL AVAL_log
# compute n, mean, std, cv, median, min, max for each var
# Use dplyr summarise
adpp_sum <- adpp %>%
  group_by(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`) %>%
  summarise(
    n1 = sum(!is.na(AVAL)),
    n2 = sum(!is.na(AVAL_log)),
    mn1 = ifelse(n1 > 0, mean(AVAL, na.rm = TRUE), NA_real_),
    mn2 = ifelse(n2 > 0, mean(AVAL_log, na.rm = TRUE), NA_real_),
    std1 = ifelse(n1 > 1, sd(AVAL, na.rm = TRUE), NA_real_),
    std2 = ifelse(n2 > 1, sd(AVAL_log, na.rm = TRUE), NA_real_),
    cv1 = ifelse(!is.na(mn1) & mn1 != 0 & !is.na(std1), std1 / mn1 * 100, NA_real_),
    cv2 = ifelse(!is.na(mn2) & mn2 != 0 & !is.na(std2), std2 / mn2 * 100, NA_real_),
    med1 = ifelse(n1 > 0, median(AVAL, na.rm = TRUE), NA_real_),
    med2 = ifelse(n2 > 0, median(AVAL_log, na.rm = TRUE), NA_real_),
    min1 = ifelse(n1 > 0, min(AVAL, na.rm = TRUE), NA_real_),
    min2 = ifelse(n2 > 0, min(AVAL_log, na.rm = TRUE), NA_real_),
    max1 = ifelse(n1 > 0, max(AVAL, na.rm = TRUE), NA_real_),
    max2 = ifelse(n2 > 0, max(AVAL_log, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`)

# data adpp_sum_t transposed: 7 rows per group
# helper to check index (case-sensitive)
has_str <- function(x, pat) grepl(pat, x, fixed = TRUE)

make_rows <- function(row) {
  # row is a single row tibble/list
  out_list <- list()
  # s_order 1 n
  out_list[[1]] <- tibble(
    ANLPRNT = row$ANLPRNT, PPSPECL = row$PPSPECL, PPCATL = row$PPCATL,
    TRTAN = row$TRTAN, PPSDY = row$PPSDY, `_PARAM` = row$`_PARAM`,
    s_order = 1L,
    value = sas_put(row$n1, 2, 0)
  )
  # s_order 2 mean (sd)
  v2 <- " "
  if (has_str(row$`_PARAM`, "AUC")) {
    v2 <- paste0(sas_put(row$mn1, 6, 0), " (", sas_put(row$std1, 5, 0), ")")
  } else if (has_str(row$`_PARAM`, "Cmax")) {
    v2 <- paste0(sas_put(row$mn1, 5, 0), " (", sas_put(row$std1, 4, 0), ")")
  } else if (has_str(row$`_PARAM`, "MRT")) {
    v2 <- paste0(sas_put(row$mn1, 4, 1), " (", sas_put(row$std1, 4, 2), ")")
  } else if (has_str(row$`_PARAM`, "t1/2")) {
    v2 <- paste0(sas_put(row$mn1, 4, 2), " (", sas_put(row$std1, 4, 2), ")")
  } else {
    v2 <- " "
  }
  out_list[[2]] <- tibble(ANLPRNT=row$ANLPRNT, PPSPECL=row$PPSPECL, PPCATL=row$PPCATL, TRTAN=row$TRTAN, PPSDY=row$PPSDY, `_PARAM`=row$`_PARAM`, s_order=2L, value=v2)

  # s_order 3 CV% mean
  v3 <- if (has_str(row$`_PARAM`, "tmax")) " " else sas_put(row$cv1, 4, 1)
  out_list[[3]] <- tibble(ANLPRNT=row$ANLPRNT, PPSPECL=row$PPSPECL, PPCATL=row$PPCATL, TRTAN=row$TRTAN, PPSDY=row$PPSDY, `_PARAM`=row$`_PARAM`, s_order=3L, value=v3)

  # s_order 4 geo-mean
  v4 <- " "
  if (has_str(row$`_PARAM`, "AUC")) {
    v4 <- sas_put(exp(row$mn2), 6, 0)
  } else if (has_str(row$`_PARAM`, "Cmax")) {
    v4 <- sas_put(exp(row$mn2), 5, 0)
  } else if (has_str(row$`_PARAM`, "MRT")) {
    v4 <- sas_put(exp(row$mn2), 4, 1)
  } else if (has_str(row$`_PARAM`, "t1/2")) {
    v4 <- sas_put(exp(row$mn2), 4, 2)
  } else {
    v4 <- " "
  }
  out_list[[4]] <- tibble(ANLPRNT=row$ANLPRNT, PPSPECL=row$PPSPECL, PPCATL=row$PPCATL, TRTAN=row$TRTAN, PPSDY=row$PPSDY, `_PARAM`=row$`_PARAM`, s_order=4L, value=v4)

  # s_order 5 CV% geo-mean
  cv_geo <- sqrt(exp(row$std2^2) - 1) * 100
  v5 <- if (has_str(row$`_PARAM`, "tmax")) " " else sas_put(cv_geo, 4, 1)
  out_list[[5]] <- tibble(ANLPRNT=row$ANLPRNT, PPSPECL=row$PPSPECL, PPCATL=row$PPCATL, TRTAN=row$TRTAN, PPSDY=row$PPSDY, `_PARAM`=row$`_PARAM`, s_order=5L, value=v5)

  # s_order 6 median
  v6 <- " "
  if (has_str(row$`_PARAM`, "AUC")) {
    v6 <- sas_put(row$med1, 6, 0)
  } else if (has_str(row$`_PARAM`, "Cmax")) {
    v6 <- sas_put(row$med1, 5, 0)
  } else if (has_str(row$`_PARAM`, "MRT")) {
    v6 <- sas_put(row$med1, 4, 1)
  } else if (has_str(row$`_PARAM`, "t1/2")) {
    v6 <- sas_put(row$med1, 4, 2)
  } else if (has_str(row$`_PARAM`, "tmax")) {
    v6 <- sas_put(row$med1, 4, 2)
  }
  out_list[[6]] <- tibble(ANLPRNT=row$ANLPRNT, PPSPECL=row$PPSPECL, PPCATL=row$PPCATL, TRTAN=row$TRTAN, PPSDY=row$PPSDY, `_PARAM`=row$`_PARAM`, s_order=6L, value=v6)

  # s_order 7 [min; max]
  v7 <- " "
  if (has_str(row$`_PARAM`, "AUC")) {
    v7 <- paste0("[", sas_put(row$min1, 6, 0), ";", sas_put(row$max1, 6, 0), "]")
  } else if (has_str(row$`_PARAM`, "Cmax")) {
    v7 <- paste0("[", sas_put(row$min1, 4, 0), ";", sas_put(row$max1, 5, 0), "]")
  } else if (has_str(row$`_PARAM`, "MRT")) {
    v7 <- paste0("[", sas_put(row$min1, 4, 2), ";", sas_put(row$max1, 4, 1), "]")
  } else if (has_str(row$`_PARAM`, "t1/2")) {
    v7 <- paste0("[", sas_put(row$min1, 4, 2), ";", sas_put(row$max1, 4, 1), "]")
  } else if (has_str(row$`_PARAM`, "tmax")) {
    v7 <- paste0("[", sas_put(row$min1, 4, 2), ";", sas_put(row$max1, 4, 2), "]")
  }
  out_list[[7]] <- tibble(ANLPRNT=row$ANLPRNT, PPSPECL=row$PPSPECL, PPCATL=row$PPCATL, TRTAN=row$TRTAN, PPSDY=row$PPSDY, `_PARAM`=row$`_PARAM`, s_order=7L, value=v7)

  bind_rows(out_list)
}

adpp_sum_t <- adpp_sum %>%
  rowwise() %>%
  do(make_rows(.)) %>%
  ungroup() %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`)

# add formatted labels for report
adpp_sum_t <- adpp_sum_t %>%
  mutate(
    TRTAN_label = ifelse(as.character(TRTAN) %in% names(trtan_fmt), trtan_fmt[as.character(TRTAN)], as.character(TRTAN)),
    Statistic = ifelse(as.character(s_order) %in% names(statg_fmt), statg_fmt[as.character(s_order)], as.character(s_order)),
    value_trim = str_trim(value)
  )

# sort for report
adpp_sum_t <- adpp_sum_t %>% arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`)

# write dataset CSVs
write.csv(adpp_sum, file.path(out, "adpp_sum.csv"), row.names = FALSE, na = "")
write.csv(adpp_sum_t, file.path(out, "adpp_sum_t.csv"), row.names = FALSE, na = "")
# CLXCHECKNOBS is proc report out dataset - same as adpp_sum_t
write.csv(adpp_sum_t, file.path(out, "CLXCHECKNOBS.csv"), row.names = FALSE, na = "")

# pivot for report: columns = _PARAM, values = value
# need to keep TRTAN, PPSDY, s_order, Statistic
report_wide <- adpp_sum_t %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, TRTAN_label, PPSDY, s_order, Statistic, `_PARAM`, value, value_trim) %>%
  # pivot wider: one column per _PARAM with value_trim
  pivot_wider(names_from = `_PARAM`, values_from = value_trim, values_fill = "")

# also create version with raw padded values for text
report_wide_padded <- adpp_sum_t %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, TRTAN_label, PPSDY, s_order, Statistic, `_PARAM`, value) %>%
  pivot_wider(names_from = `_PARAM`, values_from = value, values_fill = "")

# write CSV of table body (wide)
write.csv(report_wide, file.path(out, "P059.csv"), row.names = FALSE, na = "")
write.csv(report_wide, file.path(out, "P059.lst.csv"), row.names = FALSE, na = "")
write.csv(report_wide, file.path(out, "P059_table.csv"), row.names = FALSE, na = "")

# Build text rendering
# Titles and footnotes
titles <- c(
  "Study001",
  "Table 14.2-PPTS (Page xxx of xxx)",
  "Summary statistics for PK parameters",
  "by compound, matrix, analyte and actual treatment",
  "Analysis Set: PK analysis set"
)
footnotes <- c(
  "CV% = coefficient of variation (%) = sd/mean*100",
  "CV% geo-mean = (sqrt(exp(variance for log transformed data)-1))*100",
  "Geo-mean: Geometric mean",
  "Geo-mean and CV% geo-mean not presented when the minimum value for a parameter is zero.",
  "Data: adpp  Program: pps.sas  Output: t.14.2.ppts.txt",
  "Fake Data/ Production Run on 17MAR2014:23:57"
)

# Determine _PARAM order as in SAS: order by _PARAM within s_order
param_order <- adpp_sum_t %>% distinct(`_PARAM`) %>% pull(`_PARAM`) %>% sort()
# But SAS order is by _PARAM ascending within each by group; keep sorted
# Use param_order as sorted unique

# Build text lines
txt_lines <- c()
txt_lines <- c(txt_lines, titles, "")

# For each by group ANLPRNT PPSPECL PPCATL
by_groups <- adpp_sum_t %>% distinct(ANLPRNT, PPSPECL, PPCATL) %>% arrange(ANLPRNT, PPSPECL, PPCATL)

for (i in seq_len(nrow(by_groups))) {
  bg <- by_groups[i, ]
  txt_lines <- c(txt_lines, paste0("Compound: ", bg$ANLPRNT, ", Matrix: ", bg$PPSPECL, ", Analyte: ", bg$PPCATL), "")
  # header
  # columns: Actual treatment (20), Period day (8), Statistic (14), each _PARAM (19)
  hdr1 <- sprintf("%-20s %-8s %-14s", "Actual treatment", "Period day", "Statistic")
  hdr2 <- paste0(hdr1, paste(sprintf(" %-19s", param_order), collapse = ""))
  txt_lines <- c(txt_lines, hdr2)
  txt_lines <- c(txt_lines, paste0(strrep("_", nchar(hdr2))))
  # rows for this by group
  sub <- report_wide %>% filter(ANLPRNT == bg$ANLPRNT, PPSPECL == bg$PPSPECL, PPCATL == bg$PPCATL) %>% arrange(TRTAN, PPSDY, s_order)
  sub_pad <- report_wide_padded %>% filter(ANLPRNT == bg$ANLPRNT, PPSPECL == bg$PPSPECL, PPCATL == bg$PPCATL) %>% arrange(TRTAN, PPSDY, s_order)
  for (r in seq_len(nrow(sub))) {
    trt_lab <- sub$TRTAN_label[r]
    ppsdy <- ifelse(is.na(sub$PPSDY[r]), "", as.character(sub$PPSDY[r]))
    stat <- sub$Statistic[r]
    line_base <- sprintf("%-20s %-8s %-14s", substr(trt_lab,1,20), ppsdy, substr(stat,1,14))
    # values
    vals <- c()
    for (p in param_order) {
      v <- sub_pad[[p]][r]
      if (is.na(v)) v <- ""
      # v already padded to width 19? In SAS value width 19, but our value strings are shorter (e.g., "  5 (  2)" length ~10)
      # Pad to 19 left aligned
      vals <- c(vals, sprintf(" %-19s", v))
    }
    txt_lines <- c(txt_lines, paste0(line_base, paste(vals, collapse = "")))
    # break after PPSDY/skip : add blank line when PPSDY changes
    if (r < nrow(sub) && sub$PPSDY[r] != sub$PPSDY[r+1]) {
      txt_lines <- c(txt_lines, "")
    }
  }
  txt_lines <- c(txt_lines, "")
}

txt_lines <- c(txt_lines, footnotes)

# write txt
writeLines(txt_lines, file.path(out, "P059.txt"))
writeLines(txt_lines, file.path(out, "P059.lst.txt"))
writeLines(txt_lines, file.path(out, "P059.lst"))

cat("Done. Outputs written to", out, "\n")

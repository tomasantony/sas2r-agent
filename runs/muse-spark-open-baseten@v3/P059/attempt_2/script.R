# P059_pk_param_summary.sas -> R translation
# Outputs: table/listing text + CSV

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
  library(data.table)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: find adpp.xpt
find_adpp <- function() {
  candidates <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/ADPP.xpt",
    Sys.getenv("RAWADAM"),
    file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam/adpp.xpt"),
    file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam/adpp.xpt"),
    "/tmp/adpp.xpt",
    "data/raw/cdisc-pilot/adam/adpp.xpt",
    "data/derived/cdisc-pilot/adam/adpp.xpt"
  )
  # also search recursively under current wd
  for (p in candidates) {
    if (!is.na(p) && p != "" && file.exists(p)) return(p)
    # if p is dir
    if (!is.na(p) && p != "" && dir.exists(p)) {
      f <- file.path(p, "adpp.xpt")
      if (file.exists(f)) return(f)
      f2 <- file.path(p, "ADPP.xpt")
      if (file.exists(f2)) return(f2)
    }
  }
  # recursive search
  search_roots <- c(".", "/tmp", getwd(), Sys.getenv("HOME"))
  for (r in search_roots) {
    if (dir.exists(r)) {
      ff <- list.files(r, pattern = "^adpp\\.xpt$", recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
      if (length(ff) > 0) return(ff[1])
    }
  }
  return(NA)
}

adpp_path <- find_adpp()
# fallback to inlib derived path
if (is.na(adpp_path) || !file.exists(adpp_path)) {
  alt <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpp.xpt"
  if (file.exists(alt)) adpp_path <- alt
}
if (is.na(adpp_path) || !file.exists(adpp_path)) {
  # try haven libref inlib path on linux mount
  alt2 <- list.files(".", pattern = "adpp.xpt", recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
  if (length(alt2) > 0) adpp_path <- alt2[1]
}
stopifnot(!is.na(adpp_path), file.exists(adpp_path))

adpp_raw <- haven::read_xpt(adpp_path)

# normalize names to uppercase for SAS compatibility (haven preserves case)
names(adpp_raw) <- toupper(names(adpp_raw))

# Ensure required columns exist
# SAS variables: PKFN, ANL01FN, PARAMCD, PPSTRESN, PPCATL, PPTESTCD, PPORRESU, AVAL, ANLPRNT, PPSPECL, TRTAN, PPSDY
# Some may be named differently; check
# haven may have lower case; we already uppercased

# Helper SAS functions
sas_round_half_away <- function(x, digits = 0) {
  ifelse(is.na(x) | is.nan(x) | is.infinite(x), NA_real_,
         {
           s <- sign(x)
           a <- abs(x)
           f <- 10^digits
           s * floor(a * f + 0.5) / f
         })
}

sas_put <- function(x, w, d) {
  # w = width, d = decimals, SAS w.d format, round half away, right aligned pad left with spaces
  # returns character vector length same as x
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi) || is.nan(xi) || is.infinite(xi)) {
      return(strrep(" ", w))
    }
    xr <- sas_round_half_away(xi, d)
    # format without width first
    if (d == 0) {
      s <- formatC(xr, format = "f", digits = 0, flag = "")
      # formatC may produce "-0" ; keep
    } else {
      s <- formatC(xr, format = "f", digits = d, flag = "")
    }
    # formatC already does rounding but we already rounded half away; ensure it doesn't re-round incorrectly
    # For safety, use sprintf after rounding
    # Use sprintf to guarantee decimals
    if (d == 0) {
      s <- sprintf("%.0f", xr)
    } else {
      s <- sprintf(paste0("%.", d, "f"), xr)
    }
    # pad left to width w
    if (nchar(s, type = "bytes") < w) {
      s <- paste0(strrep(" ", w - nchar(s, type = "bytes")), s)
    }
    s
  }, character(1))
}

sas_compress <- function(x) gsub(" ", "", x, fixed = TRUE)
sas_strip <- function(x) trimws(x, which = "both")
sas_compbl <- function(x) gsub("\\s+", " ", trimws(x))
sas_index <- function(str, substr) {
  # SAS INDEX returns position 1-indexed, 0 if not found; case sensitive
  # vectorized
  sapply(seq_along(str), function(i) {
    if (is.na(str[i]) || is.na(substr)) return(0)
    pos <- regexpr(substr, str[i], fixed = TRUE)[1]
    if (pos == -1) 0 else pos
  })
}

# Format $pk
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

# Filter adpp
# where PKFN=1 and ANL01FN ne 1 and paramcd in (...) and PPSTRESN ne . and compress(PPCATL)='ANAL2'
# Note SAS missing numeric is ., in R NA
adpp <- adpp_raw %>%
  filter(
    !is.na(PKFN) & PKFN == 1,
    !is.na(ANL01FN) & ANL01FN != 1,
    PARAMCD %in% c('AUCINFO','AUCLAST','CMAX','TMAX','MRTLAST','LAMZHL'),
    !is.na(PPSTRESN),
    sas_compress(PPCATL) == "ANAL2"
  ) %>%
  mutate(
    PPTESTCD = as.character(PPTESTCD),
    PPORRESU = as.character(PPORRESU),
    PPCATL = as.character(PPCATL),
    ANLPRNT = as.character(ANLPRNT),
    PPSPECL = as.character(PPSPECL),
    `_PARAM` = sas_compbl(paste0(ifelse(PPTESTCD %in% names(pk_map), pk_map[PPTESTCD], PPTESTCD), " (", sas_strip(PPORRESU), ")")),
    AVAL_log = log(AVAL)
  )

# Sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM
adpp <- adpp %>% arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`)

# Proc univariate by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM var AVAL AVAL_log
# Compute n, mean, std, cv, median, min, max for each var
# SAS std is sample std (n-1)
# cv = 100*std/mean

# Group and summarise
grp_vars <- c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","_PARAM")

adpp_sum <- adpp %>%
  group_by(across(all_of(grp_vars))) %>%
  summarise(
    n1 = sum(!is.na(AVAL)),
    n2 = sum(!is.na(AVAL_log)),
    mn1 = ifelse(n1>0, mean(AVAL, na.rm=TRUE), NA_real_),
    mn2 = ifelse(n2>0, mean(AVAL_log, na.rm=TRUE), NA_real_),
    std1 = ifelse(n1>1, sd(AVAL, na.rm=TRUE), NA_real_),
    std2 = ifelse(n2>1, sd(AVAL_log, na.rm=TRUE), NA_real_),
    cv1 = ifelse(!is.na(mn1) & mn1 != 0 & !is.na(std1), std1/mn1*100, NA_real_),
    cv2 = ifelse(!is.na(mn2) & mn2 != 0 & !is.na(std2), std2/mn2*100, NA_real_),
    med1 = ifelse(n1>0, median(AVAL, na.rm=TRUE), NA_real_),
    med2 = ifelse(n2>0, median(AVAL_log, na.rm=TRUE), NA_real_),
    min1 = ifelse(n1>0, min(AVAL, na.rm=TRUE), NA_real_),
    max1 = ifelse(n1>0, max(AVAL, na.rm=TRUE), NA_real_),
    min2 = ifelse(n2>0, min(AVAL_log, na.rm=TRUE), NA_real_),
    max2 = ifelse(n2>0, max(AVAL_log, na.rm=TRUE), NA_real_),
    .groups = "drop"
  )

# adpp_sum_t transposed: 7 rows per group
# Use rowwise to generate

make_rows <- function(df) {
  # df is one row of adpp_sum
  out_rows <- list()
  # s_order 1 n
  out_rows[[1]] <- df %>% transmute(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`, s_order=1, value=sas_put(n1,2,0))
  # s_order 2 mean (sd)
  val2 <- if (sas_index(df$`_PARAM`, "AUC")>0) {
    paste0(sas_put(df$mn1,6,0), " (", sas_put(df$std1,5,0), ")")
  } else if (sas_index(df$`_PARAM`, "Cmax")>0) {
    paste0(sas_put(df$mn1,5,0), " (", sas_put(df$std1,4,0), ")")
  } else if (sas_index(df$`_PARAM`, "MRT")>0) {
    paste0(sas_put(df$mn1,4,1), " (", sas_put(df$std1,4,2), ")")
  } else if (sas_index(df$`_PARAM`, "t1/2")>0) {
    paste0(sas_put(df$mn1,4,2), " (", sas_put(df$std1,4,2), ")")
  } else {
    " "
  }
  out_rows[[2]] <- df %>% transmute(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`, s_order=2, value=val2)
  # s_order 3 CV% mean
  val3 <- if (sas_index(df$`_PARAM`, "tmax")==0) sas_put(df$cv1,4,1) else " "
  out_rows[[3]] <- df %>% transmute(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`, s_order=3, value=val3)
  # s_order 4 geo-mean
  val4 <- if (sas_index(df$`_PARAM`, "AUC")>0) {
    sas_put(exp(df$mn2),6,0)
  } else if (sas_index(df$`_PARAM`, "Cmax")>0) {
    sas_put(exp(df$mn2),5,0)
  } else if (sas_index(df$`_PARAM`, "MRT")>0) {
    sas_put(exp(df$mn2),4,1)
  } else if (sas_index(df$`_PARAM`, "t1/2")>0) {
    sas_put(exp(df$mn2),4,2)
  } else {
    " "
  }
  out_rows[[4]] <- df %>% transmute(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`, s_order=4, value=val4)
  # s_order 5 CV% geo-mean
  cv_geo <- sqrt(exp(df$std2^2)-1)*100
  val5 <- if (sas_index(df$`_PARAM`, "tmax")==0) sas_put(cv_geo,4,1) else " "
  out_rows[[5]] <- df %>% transmute(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`, s_order=5, value=val5)
  # s_order 6 median
  val6 <- if (sas_index(df$`_PARAM`, "AUC")>0) {
    sas_put(df$med1,6,0)
  } else if (sas_index(df$`_PARAM`, "Cmax")>0) {
    sas_put(df$med1,5,0)
  } else if (sas_index(df$`_PARAM`, "MRT")>0) {
    sas_put(df$med1,4,1)
  } else if (sas_index(df$`_PARAM`, "t1/2")>0) {
    sas_put(df$med1,4,2)
  } else if (sas_index(df$`_PARAM`, "tmax")>0) {
    sas_put(df$med1,4,2)
  } else {
    " "
  }
  out_rows[[6]] <- df %>% transmute(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`, s_order=6, value=val6)
  # s_order 7 [min; max]
  val7 <- if (sas_index(df$`_PARAM`, "AUC")>0) {
    paste0("[", sas_put(df$min1,6,0), ";", sas_put(df$max1,6,0), "]")
  } else if (sas_index(df$`_PARAM`, "Cmax")>0) {
    paste0("[", sas_put(df$min1,4,0), ";", sas_put(df$max1,5,0), "]")
  } else if (sas_index(df$`_PARAM`, "MRT")>0) {
    paste0("[", sas_put(df$min1,4,2), ";", sas_put(df$max1,4,1), "]")
  } else if (sas_index(df$`_PARAM`, "t1/2")>0) {
    paste0("[", sas_put(df$min1,4,2), ";", sas_put(df$max1,4,1), "]")
  } else if (sas_index(df$`_PARAM`, "tmax")>0) {
    paste0("[", sas_put(df$min1,4,2), ";", sas_put(df$max1,4,2), "]")
  } else {
    " "
  }
  out_rows[[7]] <- df %>% transmute(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`, s_order=7, value=val7)
  bind_rows(out_rows)
}

adpp_sum_t <- adpp_sum %>%
  split(seq_len(nrow(.))) %>%
  map_dfr(make_rows)

# sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM
adpp_sum_t <- adpp_sum_t %>% arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`)

# Formats
trtan_fmt <- c("1"="Period 1 Treatment","2"="Period 2 Treatment","3"="Period 3 Treatment")
statg_fmt <- c("1"="n","2"="Mean (SD)","3"="CV% mean","4"="Geo-mean","5"="CV% geo-mean","6"="Median","7"="[Min; Max]        ")

# Prepare report wide table: rows = TRTAN, PPSDY, s_order ; columns = _PARAM
# Get distinct _PARAM ordered as in SAS sort (alphabetical due to sort)
param_order <- adpp_sum_t %>% distinct(`_PARAM`) %>% arrange(`_PARAM`) %>% pull(`_PARAM`)

# Create wide
wide <- adpp_sum_t %>%
  mutate(value_trim = trimws(value)) %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`, value_trim) %>%
  pivot_wider(names_from = `_PARAM`, values_from = value_trim)

# Ensure column order: TRTAN, PPSDY, s_order then params in param_order
wide <- wide %>% arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order)

# For CSV output, keep wide as table body
# Also create long version for dataset output
dataset_out <- adpp_sum_t %>%
  mutate(
    TRTAN_label = recode(as.character(TRTAN), !!!trtan_fmt),
    STAT_label = recode(as.character(s_order), !!!statg_fmt)
  )

# Write dataset CSV (ADPP_SUM_T equivalent)
write.csv(dataset_out, file.path(out, "P059_dataset.csv"), row.names = FALSE, na = "")
write.csv(dataset_out, file.path(out, "adpp_sum_t.csv"), row.names = FALSE, na = "")

# Build text rendering
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

# Helper to format TRTAN and stat
fmt_trtan <- function(x) ifelse(as.character(x) %in% names(trtan_fmt), trtan_fmt[as.character(x)], as.character(x))
fmt_stat <- function(x) ifelse(as.character(x) %in% names(statg_fmt), statg_fmt[as.character(x)], as.character(x))

# Build text lines
txt_lines <- c()
for (t in titles) txt_lines <- c(txt_lines, t)
txt_lines <- c(txt_lines, "")

# By groups
by_groups <- wide %>% distinct(ANLPRNT, PPSPECL, PPCATL) %>% arrange(ANLPRNT, PPSPECL, PPCATL)

for (bg in seq_len(nrow(by_groups))) {
  anl <- by_groups$ANLPRNT[bg]
  spe <- by_groups$PPSPECL[bg]
  catl <- by_groups$PPCATL[bg]
  txt_lines <- c(txt_lines, paste0("Compound: ", anl, ", Matrix: ", spe, ", Analyte: ", catl))
  txt_lines <- c(txt_lines, "")

  sub <- wide %>% filter(ANLPRNT==anl, PPSPECL==spe, PPCATL==catl) %>% arrange(TRTAN, PPSDY, s_order)

  # Header
  hdr1 <- sprintf("%-20s %-8s %-14s", "Actual treatment", "Period day", "Statistic")
  for (p in param_order) {
    hdr1 <- paste0(hdr1, sprintf(" %-19s", substr(p,1,19)))
  }
  txt_lines <- c(txt_lines, hdr1)
  txt_lines <- c(txt_lines, paste0(strrep("_", nchar(hdr1))))

  # Rows
  for (i in seq_len(nrow(sub))) {
    trt_lab <- fmt_trtan(sub$TRTAN[i])
    ppsdy_val <- ifelse(is.na(sub$PPSDY[i]), "", as.character(sub$PPSDY[i]))
    stat_lab <- fmt_stat(sub$s_order[i])
    line <- sprintf("%-20s %-8s %-14s", trt_lab, ppsdy_val, stat_lab)
    for (p in param_order) {
      v <- sub[[p]][i]
      if (is.na(v)) v <- ""
      line <- paste0(line, sprintf(" %-19s", v))
    }
    txt_lines <- c(txt_lines, line)
    # break after PPSDY/skip : add blank line when next row has different PPSDY or TRTAN or is last
    if (i < nrow(sub)) {
      if (sub$PPSDY[i] != sub$PPSDY[i+1] || sub$TRTAN[i] != sub$TRTAN[i+1]) {
        txt_lines <- c(txt_lines, "")
      }
    }
  }
  txt_lines <- c(txt_lines, "")
}

for (f in footnotes) txt_lines <- c(txt_lines, f)

# Write txt
writeLines(txt_lines, file.path(out, "P059.txt"))
writeLines(txt_lines, file.path(out, "P059.lst.txt"))
writeLines(txt_lines, file.path(out, "P059.lst"))

# Write CSV of table body (wide)
# For CSV, include readable column names
csv_wide <- wide %>%
  mutate(
    `Actual treatment` = fmt_trtan(TRTAN),
    `Period day` = PPSDY,
    Statistic = fmt_stat(s_order)
  ) %>%
  select(`Actual treatment`, `Period day`, Statistic, all_of(param_order), ANLPRNT, PPSPECL, PPCATL)

write.csv(csv_wide, file.path(out, "P059.csv"), row.names = FALSE, na = "")
write.csv(csv_wide, file.path(out, "P059.lst.csv"), row.names = FALSE, na = "")

# Also write CLXCHECKNOBS equivalent (proc report out=)
# SAS out=CLXCHECKNOBS contains report data; we already have dataset_out
write.csv(dataset_out, file.path(out, "CLXCHECKNOBS.csv"), row.names = FALSE, na = "")

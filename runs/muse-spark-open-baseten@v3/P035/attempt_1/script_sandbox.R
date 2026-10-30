## P035_target14.sas -> R translation
## Produces table 14.2-PPTS summary statistics for PK parameters

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("outdir")
if (out == "" || is.na(out)) out <- "."
dir.create(out, recursive = TRUE, showWarnings = FALSE)

## helpers ---------------------------------------------------------------
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_put <- function(x, w, d = 0) {
  # w.d format, half away from zero, right aligned width w, missing -> blanks
  if (length(x) == 0) return(character(0))
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi)) return(strrep(" ", w))
    xr <- sas_round(xi, d)
    if (d == 0) {
      s <- sprintf("%.0f", xr)
    } else {
      s <- sprintf(paste0("%.", d, "f"), xr)
    }
    if (nchar(s, type = "bytes") < w) {
      s <- paste0(strrep(" ", w - nchar(s, type = "bytes")), s)
    }
    s
  }, character(1))
}
sas_put_w <- function(x, w) sas_put(x, w, 0)

compbl <- function(s) gsub("\\s+", " ", trimws(s))
strip <- function(s) trimws(s)

find_first_file <- function(candidates) {
  for (p in candidates) {
    if (!is.na(p) && p != "" && file.exists(p)) return(p)
  }
  return(NA_character_)
}

## locate adpp.xpt -------------------------------------------------------
candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam/adpp.xpt",
  "/bench/data/raw/cdisc-pilot/adam/ADPP.xpt",
  file.path(Sys.getenv("rawadam"), "adpp.xpt"),
  file.path(Sys.getenv("rawadam"), "ADPP.xpt"),
  file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam/adpp.xpt"),
  file.path(Sys.getenv("root"), "data/derived/cdisc-pilot/adam/adpp.xpt"),
  "data/raw/cdisc-pilot/adam/adpp.xpt",
  "data/derived/cdisc-pilot/adam/adpp.xpt",
  "/tmp/adpp.xpt"
)
# also try OUT_DIR relative search if not found
if (all(!file.exists(candidates[!is.na(candidates)]))) {
  # recursive search limited depth
  try({
    ff <- list.files("/bench", pattern = "^adpp\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    if (length(ff) > 0) candidates <- c(candidates, ff[1])
  }, silent = TRUE)
  try({
    ff2 <- list.files(".", pattern = "^adpp\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    if (length(ff2) > 0) candidates <- c(candidates, ff2[1])
  }, silent = TRUE)
}

adpp_path <- find_first_file(candidates)
# fallback: search entire filesystem for adpp.xpt (may be slow, try)
if (is.na(adpp_path)) {
  try({
    all_xpt <- list.files("/", pattern = "\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    idx <- grep("adpp\\.xpt$", all_xpt, ignore.case = TRUE)
    if (length(idx) > 0) adpp_path <- all_xpt[idx[1]]
  }, silent = TRUE)
}
if (is.na(adpp_path) || !file.exists(adpp_path)) {
  stop(paste0("adpp.xpt not found. Tried: ", paste(candidates, collapse = "; ")))
}
message("Reading ", adpp_path)

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

adpp_raw <- haven::read_xpt(adpp_path)

# normalize names to upper for processing, keep original mapping
names(adpp_raw) <- toupper(names(adpp_raw))

# haven labelled -> convert
adpp_raw <- adpp_raw %>%
  mutate(across(everything(), ~ {
    if (inherits(.x, "haven_labelled")) {
      # keep numeric if numeric, else character
      v <- haven::zap_labels(.x)
      # if character labelled, convert to character
      if (is.character(v)) as.character(v) else v
    } else .x
  }))

# ensure required columns exist (case-insensitive check already upper)
required <- c("PKFN","ANL01FN","PARAMCD","PPSTRESN","PPCATL","PPTESTCD","PPORRESU","AVAL","ANLPRNT","PPSPECL","TRTAN","PPSDY")
missing_req <- setdiff(required, names(adpp_raw))
if (length(missing_req) > 0) {
  # try alternative names: PARAMCD may be PPTESTCD? but check
  stop(paste("Missing columns in adpp:", paste(missing_req, collapse = ",")))
}

# filter as in SAS ------------------------------------------------------
# where PKFN=1 and ANL01FN ne 1 and paramcd in (...) and PPSTRESN ne . and compress(PPCATL)='ANAL2'
adpp_filt <- adpp_raw %>%
  filter(
    PKFN == 1,
    ANL01FN != 1,
    toupper(TRIMWS <- trimws(PARAMCD)) %in% c('AUCINFO','AUCLAST','CMAX','TMAX','MRTLAST','LAMZHL'),
    !is.na(PPSTRESN),
    toupper(gsub("[[:space:]]+", "", PPCATL)) == "ANAL2"
  )

# _PARAM = compbl(put(PPTESTCD,$pk.)||' ('||strip(PPORRESU)||')')
pk_fmt <- c(
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
    PPTESTCD_U = toupper(trimws(PPTESTCD)),
    PK_LABEL = ifelse(PPTESTCD_U %in% names(pk_fmt), pk_fmt[PPTESTCD_U], PPTESTCD_U),
    PPORRESU_S = strip(as.character(PPORRESU)),
    `_PARAM` = compbl(paste0(PK_LABEL, " (", PPORRESU_S, ")")),
    AVAL_log = log(AVAL)
  )

# sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM (SAS sort)
adpp_filt <- adpp_filt %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`)

# proc univariate by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM var AVAL AVAL_log
# output n mean std cv median min max for both
# cv = std/mean*100 (SAS)
adpp_sum <- adpp_filt %>%
  group_by(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`) %>%
  summarise(
    n1 = sum(!is.na(AVAL)),
    n2 = sum(!is.na(AVAL_log)),
    mn1 = ifelse(n1 > 0, mean(AVAL, na.rm = TRUE), NA_real_),
    mn2 = ifelse(n2 > 0, mean(AVAL_log, na.rm = TRUE), NA_real_),
    std1 = ifelse(n1 > 1, sd(AVAL, na.rm = TRUE), NA_real_),
    std2 = ifelse(n2 > 1, sd(AVAL_log, na.rm = TRUE), NA_real_),
    cv1 = ifelse(!is.na(mn1) && mn1 != 0 && !is.na(std1), std1 / mn1 * 100, NA_real_),
    cv2 = ifelse(!is.na(mn2) && mn2 != 0 && !is.na(std2), std2 / mn2 * 100, NA_real_),
    med1 = ifelse(n1 > 0, median(AVAL, na.rm = TRUE), NA_real_),
    med2 = ifelse(n2 > 0, median(AVAL_log, na.rm = TRUE), NA_real_),
    min1 = ifelse(n1 > 0, min(AVAL, na.rm = TRUE), NA_real_),
    min2 = ifelse(n2 > 0, min(AVAL_log, na.rm = TRUE), NA_real_),
    max1 = ifelse(n1 > 0, max(AVAL, na.rm = TRUE), NA_real_),
    max2 = ifelse(n2 > 0, max(AVAL_log, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# data adpp_sum_t transposed ---------------------------------------------
# For each row in adpp_sum create 7 rows
make_rows <- function(df) {
  # df is one row
  out_list <- list()
  # s_order 1 n
  out_list[[1]] <- data.frame(s_order = 1, value = sas_put(df$n1, 2), stringsAsFactors = FALSE)
  # s_order 2 mean (sd)
  v2 <- " "
  if (grepl("AUC", df$`_PARAM`, fixed = TRUE)) {
    v2 <- paste0(sas_put(df$mn1, 6, 0), " (", sas_put(df$std1, 5, 0), ")")
  } else if (grepl("Cmax", df$`_PARAM`, fixed = TRUE)) {
    v2 <- paste0(sas_put(df$mn1, 5, 0), " (", sas_put(df$std1, 4, 0), ")")
  } else if (grepl("MRT", df$`_PARAM`, fixed = TRUE)) {
    v2 <- paste0(sas_put(df$mn1, 4, 1), " (", sas_put(df$std1, 4, 2), ")")
  } else if (grepl("t1/2", df$`_PARAM`, fixed = TRUE)) {
    v2 <- paste0(sas_put(df$mn1, 4, 2), " (", sas_put(df$std1, 4, 2), ")")
  } else {
    v2 <- " "
  }
  out_list[[2]] <- data.frame(s_order = 2, value = v2, stringsAsFactors = FALSE)

  # s_order 3 CV% mean
  v3 <- " "
  if (!grepl("tmax", df$`_PARAM`, fixed = TRUE)) {
    v3 <- sas_put(df$cv1, 4, 1)
  } else {
    v3 <- " "
  }
  out_list[[3]] <- data.frame(s_order = 3, value = v3, stringsAsFactors = FALSE)

  # s_order 4 geo-mean
  v4 <- " "
  if (grepl("AUC", df$`_PARAM`, fixed = TRUE)) {
    v4 <- sas_put(exp(df$mn2), 6, 0)
  } else if (grepl("Cmax", df$`_PARAM`, fixed = TRUE)) {
    v4 <- sas_put(exp(df$mn2), 5, 0)
  } else if (grepl("MRT", df$`_PARAM`, fixed = TRUE)) {
    v4 <- sas_put(exp(df$mn2), 4, 1)
  } else if (grepl("t1/2", df$`_PARAM`, fixed = TRUE)) {
    v4 <- sas_put(exp(df$mn2), 4, 2)
  } else {
    v4 <- " "
  }
  out_list[[4]] <- data.frame(s_order = 4, value = v4, stringsAsFactors = FALSE)

  # s_order 5 CV% geo-mean
  v5 <- " "
  if (!grepl("tmax", df$`_PARAM`, fixed = TRUE)) {
    cv_geo <- sqrt(exp(df$std2^2) - 1) * 100
    v5 <- sas_put(cv_geo, 4, 1)
  } else {
    v5 <- " "
  }
  out_list[[5]] <- data.frame(s_order = 5, value = v5, stringsAsFactors = FALSE)

  # s_order 6 median
  v6 <- " "
  if (grepl("AUC", df$`_PARAM`, fixed = TRUE)) {
    v6 <- sas_put(df$med1, 6, 0)
  } else if (grepl("Cmax", df$`_PARAM`, fixed = TRUE)) {
    v6 <- sas_put(df$med1, 5, 0)
  } else if (grepl("MRT", df$`_PARAM`, fixed = TRUE)) {
    v6 <- sas_put(df$med1, 4, 1)
  } else if (grepl("t1/2", df$`_PARAM`, fixed = TRUE)) {
    v6 <- sas_put(df$med1, 4, 2)
  } else if (grepl("tmax", df$`_PARAM`, fixed = TRUE)) {
    v6 <- sas_put(df$med1, 4, 2)
  }
  out_list[[6]] <- data.frame(s_order = 6, value = v6, stringsAsFactors = FALSE)

  # s_order 7 [min; max]
  v7 <- " "
  if (grepl("AUC", df$`_PARAM`, fixed = TRUE)) {
    v7 <- paste0("[", sas_put(df$min1, 6, 0), ";", sas_put(df$max1, 6, 0), "]")
  } else if (grepl("Cmax", df$`_PARAM`, fixed = TRUE)) {
    v7 <- paste0("[", sas_put(df$min1, 4, 0), ";", sas_put(df$max1, 5, 0), "]")
  } else if (grepl("MRT", df$`_PARAM`, fixed = TRUE)) {
    v7 <- paste0("[", sas_put(df$min1, 4, 2), ";", sas_put(df$max1, 4, 1), "]")
  } else if (grepl("t1/2", df$`_PARAM`, fixed = TRUE)) {
    v7 <- paste0("[", sas_put(df$min1, 4, 2), ";", sas_put(df$max1, 4, 1), "]")
  } else if (grepl("tmax", df$`_PARAM`, fixed = TRUE)) {
    v7 <- paste0("[", sas_put(df$min1, 4, 2), ";", sas_put(df$max1, 4, 2), "]")
  }
  out_list[[7]] <- data.frame(s_order = 7, value = v7, stringsAsFactors = FALSE)

  do.call(rbind, out_list)
}

adpp_sum_t_list <- lapply(seq_len(nrow(adpp_sum)), function(i) {
  row <- adpp_sum[i, ]
  tmp <- make_rows(row)
  cbind(row[, c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","_PARAM")], tmp)
})
adpp_sum_t <- do.call(rbind, adpp_sum_t_list)

# sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM
adpp_sum_t <- adpp_sum_t %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`)

# formats ---------------------------------------------------------------
trtan_fmt <- c("1" = "Period 1 Treatment", "2" = "Period 2 Treatment", "3" = "Period 3 Treatment")
statg_fmt <- c("1" = "n", "2" = "Mean (SD)", "3" = "CV% mean", "4" = "Geo-mean", "5" = "CV% geo-mean", "6" = "Median", "7" = "[Min; Max]        ")

adpp_sum_t <- adpp_sum_t %>%
  mutate(
    TRTAN_label = ifelse(as.character(TRTAN) %in% names(trtan_fmt), trtan_fmt[as.character(TRTAN)], as.character(TRTAN)),
    stat_label = ifelse(as.character(s_order) %in% names(statg_fmt), statg_fmt[as.character(s_order)], as.character(s_order))
  )

# Build report wide table: rows = TRTAN PPSDY s_order, columns = _PARAM
# For CSV we keep long and wide
param_levels <- sort(unique(adpp_sum_t$`_PARAM`))

wide_tbl <- adpp_sum_t %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, TRTAN_label, PPSDY, s_order, stat_label, `_PARAM`, value) %>%
  tidyr::pivot_wider(names_from = `_PARAM`, values_from = value)

# Ensure column order: TRTAN, PPSDY, s_order, then param_levels
wide_tbl <- wide_tbl %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order)

# For text rendering, we need to produce PROC REPORT style
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

# Build text output -----------------------------------------------------
# Determine column widths: Actual treatment 20, Period day 8, Statistic 14, each _PARAM 19
# Header for _PARAM columns is the _PARAM value itself (e.g., "AUC (h*ng/mL)")
# We will create a fixed-width text file

# Helper to pad/truncate
pad_right <- function(s, w) {
  s <- as.character(s)
  s[is.na(s)] <- ""
  # truncate if longer than w (SAS would wrap/flow, but we truncate)
  s <- ifelse(nchar(s) > w, substr(s, 1, w), s)
  sprintf(paste0("%-", w, "s"), s)
}
pad_left <- function(s, w) {
  s <- as.character(s)
  s[is.na(s)] <- ""
  s <- ifelse(nchar(s) > w, substr(s, 1, w), s)
  sprintf(paste0("%", w, "s"), s)
}

# Build lines
lines <- c()
for (t in titles) lines <- c(lines, t)
lines <- c(lines, "")

# For each by group
by_groups <- wide_tbl %>% distinct(ANLPRNT, PPSPECL, PPCATL) %>% arrange(ANLPRNT, PPSPECL, PPCATL)

for (bg_i in seq_len(nrow(by_groups))) {
  bg <- by_groups[bg_i, ]
  # compute before _page_ line
  comp_line <- paste0("Compound: ", sprintf("%-10s", bg$ANLPRNT), " , Matrix: ", sprintf("%-10s", bg$PPSPECL), " , Analyte: ", sprintf("%-15s", bg$PPCATL))
  lines <- c(lines, comp_line, "")

  sub <- wide_tbl %>% filter(ANLPRNT == bg$ANLPRNT, PPSPECL == bg$PPSPECL, PPCATL == bg$PPCATL) %>% arrange(TRTAN, PPSDY, s_order)

  # header
  hdr1 <- paste0(pad_right("Actual treatment", 20), " ",
                 pad_right("Period day", 8), " ",
                 pad_right("Statistic", 14), " ",
                 paste(sapply(param_levels, function(p) pad_right(p, 19)), collapse = " "))
  hdr2 <- paste0(strrep("-", 20), " ", strrep("-", 8), " ", strrep("-", 14), " ",
                 paste(sapply(param_levels, function(p) strrep("-", 19)), collapse = " "))
  lines <- c(lines, hdr1, hdr2)

  # rows
  for (r in seq_len(nrow(sub))) {
    row <- sub[r, ]
    trt <- pad_right(row$TRTAN_label, 20)
    ppsdy <- pad_right(as.character(row$PPSDY), 8)
    stat <- pad_right(row$stat_label, 14)
    vals <- sapply(param_levels, function(p) {
      v <- row[[p]]
      if (is.null(v) || is.na(v)) v <- ""
      pad_right(v, 19)
    })
    line <- paste0(trt, " ", ppsdy, " ", stat, " ", paste(vals, collapse = " "))
    lines <- c(lines, line)
    # break after PPSDY/skip : add blank line when PPSDY changes within same TRTAN? SAS break after PPSDY
    if (r < nrow(sub)) {
      nxt <- sub[r+1, ]
      if (nxt$PPSDY != row$PPSDY || nxt$TRTAN != row$TRTAN) {
        lines <- c(lines, "")
      }
    }
  }
  lines <- c(lines, "")
}

for (f in footnotes) lines <- c(lines, f)

# Write text files
txt_path1 <- file.path(out, "P035.txt")
txt_path2 <- file.path(out, "P035.lst.txt")
txt_path3 <- file.path(out, "P035.lst")
writeLines(lines, txt_path1)
writeLines(lines, txt_path2)
writeLines(lines, txt_path3)

# Write CSVs ------------------------------------------------------------
# CSV of table body (wide)
csv_wide <- wide_tbl %>%
  mutate(
    Actual_treatment = TRTAN_label,
    Period_day = PPSDY,
    Statistic = stat_label
  ) %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, Actual_treatment, Period_day, s_order, Statistic, all_of(param_levels))

# Ensure dates as ISO if any (none here)
write.csv(csv_wide, file.path(out, "P035.csv"), row.names = FALSE, na = "")
write.csv(csv_wide, file.path(out, "P035.lst.csv"), row.names = FALSE, na = "")

# Also write long dataset as dataset output
write.csv(adpp_sum_t, file.path(out, "P035_dataset.csv"), row.names = FALSE, na = "")
write.csv(adpp_sum, file.path(out, "P035_adpp_sum.csv"), row.names = FALSE, na = "")

# CLXCHECKNOBS equivalent (report dataset)
# PROC REPORT out=CLXCHECKNOBS contains report rows
write.csv(wide_tbl, file.path(out, "CLXCHECKNOBS.csv"), row.names = FALSE, na = "")

message("Done. Outputs written to ", out)

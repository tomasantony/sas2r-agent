# P059_pk_param_summary.R
# ---------------------------------------------------------------
# Reimplementation of SAS program P059_pk_param_summary.sas in R
# ---------------------------------------------------------------
# Packages (pre‑installed)
library(haven)      # read_xpt
library(dplyr)      # data manipulation
library(tidyr)      # reshape
library(stringr)    # string handling
library(readr)      # write_csv
library(tibble)     # tibble helpers

# ------------------------------------------------------------------
# Helper: SAS‑style round half away from zero
round_half_up <- function(x, digits = 0) {
  pos <- sign(x)
  absx <- abs(x)
  factor <- 10 ^ digits
  rounded <- floor(absx * factor + 0.5) / factor
  pos * rounded
}
# Helper: format number with given decimals (no width padding)
fmt_num <- function(x, dec = 0) {
  if (is.na(x) || is.nan(x)) return("")
  sprintf(paste0("%.", dec, "f"), round_half_up(x, dec))
}

# ------------------------------------------------------------------
# Paths
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR not set")
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
xpt_path <- file.path(rawadam, "adpp.xpt")

# ------------------------------------------------------------------
# 1. Read source XPT (single dataset) and make names lower‑case
adpp_raw <- read_xpt(xpt_path) %>% as_tibble()
names(adpp_raw) <- tolower(names(adpp_raw))

# ------------------------------------------------------------------
# 2. Apply SAS WHERE clause
adpp <- adpp_raw %>%
  filter(
    pkfn == 1,
    anl01fn != 1,
    paramcd %in% c("AUCINFO", "AUCLAST", "CMAX", "TMAX", "MRTLAST", "LAMZHL"),
    !is.na(ppstresn),
    str_replace_all(ppcatl, "\\s+", "") == "ANAL2"
  )

# ------------------------------------------------------------------
# 3. Create _PARAM (using $PK format) and log value
pk_fmt <- c(
  AUCINFO = "AUC",
  AUCTAU  = "AUCtau",
  AUCLAST = "AUC(0-tlast)",
  CMAX    = "Cmax",
  CMAXD   = "Cmax/Dose",
  MRTLAST = "MRT",
  TLAST   = "tlast",
  TMAX    = "tmax",
  CLRFO   = "CL/F",
  VZFO    = "VzF",
  LAMZ    = "lambdaZ",
  LAMZHL  = "t1/2",
  CLAST   = "Clast"
)

adpp <- adpp %>%
  mutate(
    _param = {
      pk_part <- pk_fmt[as.character(pptestcd)]
      pp_resu_part <- str_trim(as.character(pporresu))
      raw <- paste0(pk_part, " (", pp_resu_part, ")")
      str_squish(raw)                     # COMPBL equivalent
    },
    aval_log = log(aval)
  )

# ------------------------------------------------------------------
# 4. PROC SORT order (character case‑sensitive, missing first)
adpp <- adpp %>%
  arrange(anlprnt, ppspecl, ppcatl, trtan, ppsdy, `_param`)

# ------------------------------------------------------------------
# 5. PROC UNIVARIATE statistics (by groups)
adpp_sum <- adpp %>%
  group_by(anlprnt, ppspecl, ppcatl, trtan, ppsdy, `_param`) %>%
  summarise(
    n1   = sum(!is.na(aval)),
    n2   = sum(!is.na(aval_log)),
    mn1  = ifelse(n1 > 0, mean(aval, na.rm = TRUE), NA_real_),
    mn2  = ifelse(n2 > 0, mean(aval_log, na.rm = TRUE), NA_real_),
    std1 = ifelse(n1 > 1, sd(aval, na.rm = TRUE), NA_real_),
    std2 = ifelse(n2 > 1, sd(aval_log, na.rm = TRUE), NA_real_),
    cv1  = ifelse(mn1 != 0, (std1 / mn1) * 100, NA_real_),
    cv2  = ifelse(mn2 != 0, (std2 / mn2) * 100, NA_real_),
    med1 = ifelse(n1 > 0, median(aval, na.rm = TRUE), NA_real_),
    med2 = ifelse(n2 > 0, median(aval_log, na.rm = TRUE), NA_real_),
    min1 = ifelse(n1 > 0, min(aval, na.rm = TRUE), NA_real_),
    min2 = ifelse(n2 > 0, min(aval_log, na.rm = TRUE), NA_real_),
    max1 = ifelse(n1 > 0, max(aval, na.rm = TRUE), NA_real_),
    max2 = ifelse(n2 > 0, max(aval_log, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# ------------------------------------------------------------------
# 6. Helper to classify parameter for formatting
param_class <- function(p) {
  pl <- tolower(p)
  if (grepl("auc", pl))   return("AUC")
  if (grepl("cmax", pl))  return("CMAX")
  if (grepl("mrt", pl))   return("MRT")
  if (grepl("t1/2", pl) || grepl("half", pl)) return("HALF")
  if (grepl("tmax", pl))  return("TMAX")
  return(NA_character_)
}

# ------------------------------------------------------------------
# 7. Build transposed table (one row per statistic)
# Create data frames for each statistic
df_n <- adpp_sum %>%
  mutate(s_order = 1,
         value = fmt_num(n1, dec = 0))

df_mean_sd <- adpp_sum %>%
  rowwise() %>%
  mutate(
    s_order = 2,
    value = {
      pc <- param_class(`_param`)
      if (pc == "AUC")   paste0(fmt_num(mn1,0), " (", fmt_num(std1,0), ")")
      else if (pc == "CMAX") paste0(fmt_num(mn1,0), " (", fmt_num(std1,0), ")")
      else if (pc == "MRT")  paste0(fmt_num(mn1,1), " (", fmt_num(std1,2), ")")
      else if (pc == "HALF") paste0(fmt_num(mn1,2), " (", fmt_num(std1,2), ")")
      else ""
    }
  ) %>% ungroup()

df_cv_mean <- adpp_sum %>%
  rowwise() %>%
  mutate(
    s_order = 3,
    value = if (!grepl("tmax", tolower(`_param`))) fmt_num(cv1,1) else ""
  ) %>% ungroup()

df_geo_mean <- adpp_sum %>%
  rowwise() %>%
  mutate(
    s_order = 4,
    value = {
      pc <- param_class(`_param`)
      gm <- exp(mn2)
      if (pc == "AUC")   fmt_num(gm,0)
      else if (pc == "CMAX") fmt_num(gm,0)
      else if (pc == "MRT")  fmt_num(gm,1)
      else if (pc == "HALF") fmt_num(gm,2)
      else ""
    }
  ) %>% ungroup()

df_cv_geo <- adpp_sum %>%
  rowwise() %>%
  mutate(
    s_order = 5,
    value = if (!grepl("tmax", tolower(`_param`))) {
      cv_geo <- sqrt(exp(std2^2) - 1) * 100
      fmt_num(cv_geo,1)
    } else ""
  ) %>% ungroup()

df_median <- adpp_sum %>%
  rowwise() %>%
  mutate(
    s_order = 6,
    value = {
      pc <- param_class(`_param`)
      if (pc == "AUC")   fmt_num(med1,0)
      else if (pc == "CMAX") fmt_num(med1,0)
      else if (pc == "MRT")  fmt_num(med1,1)
      else if (pc == "HALF") fmt_num(med1,2)
      else if (pc == "TMAX") fmt_num(med1,2)
      else ""
    }
  ) %>% ungroup()

df_minmax <- adpp_sum %>%
  rowwise() %>%
  mutate(
    s_order = 7,
    value = {
      pc <- param_class(`_param`)
      if (pc == "AUC")   paste0("[", fmt_num(min1,0), ";", fmt_num(max1,0), "]")
      else if (pc == "CMAX") paste0("[", fmt_num(min1,0), ";", fmt_num(max1,0), "]")
      else if (pc == "MRT")  paste0("[", fmt_num(min1,2), ";", fmt_num(max1,1), "]")
      else if (pc == "HALF") paste0("[", fmt_num(min1,2), ";", fmt_num(max1,1), "]")
      else if (pc == "TMAX") paste0("[", fmt_num(min1,2), ";", fmt_num(max1,2), "]")
      else ""
    }
  ) %>% ungroup()

# Combine all rows
adpp_sum_t_long <- bind_rows(
  df_n, df_mean_sd, df_cv_mean, df_geo_mean,
  df_cv_geo, df_median, df_minmax
) %>%
  select(anlprnt, ppspecl, ppcatl, trtan, ppsdy, s_order, `_param`, value) %>%
  arrange(anlprnt, ppspecl, ppcatl, trtan, ppsdy, s_order, `_param`)

# ------------------------------------------------------------------
# 8. Add SAS statistic labels (statg format) and drop numeric order
statg_lbl <- c(
  "1" = "n",
  "2" = "Mean (SD)",
  "3" = "CV% mean",
  "4" = "Geo-mean",
  "5" = "CV% geo-mean",
  "6" = "Median",
  "7" = "[Min; Max]"
)

adpp_sum_t <- adpp_sum_t_long %>%
  mutate(statistic = statg_lbl[as.character(s_order)]) %>%
  select(-s_order)

# ------------------------------------------------------------------
# 9. Wide format for the PROC REPORT style listing (ACROSS on _PARAM)
listing_wide <- adpp_sum_t %>%
  pivot_wider(
    id_cols = c(anlprnt, ppspecl, ppcatl, trtan, ppsdy, statistic),
    names_from = `_param`,
    values_from = value
  ) %>%
  arrange(anlprnt, ppspecl, ppcatl, trtan, ppsdy, statistic)

# ------------------------------------------------------------------
# 10. Write dataset output (CSV) – the long transposed table
write.csv(
  adpp_sum_t,
  file = file.path(out_dir, "adpp_sum_t.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 11. Write listing: CSV and plain‑text rendering
write.csv(
  listing_wide,
  file = file.path(out_dir, "P059.lst.csv"),
  row.names = FALSE,
  na = ""
)

txt_path <- file.path(out_dir, "P059.lst.txt")
con <- file(txt_path, open = "wt", encoding = "UTF-8")

# Titles (match SAS titles)
writeLines("Study001", con)                                   # TITLE1
writeLines("Table 14.2-PPTS (Page xxx of xxx)", con)         # TITLE2
writeLines("Summary statistics for PK parameters", con)      # TITLE3
writeLines("by compound, matrix, analyte and actual treatment", con) # TITLE4
writeLines("Analysis Set: PK analysis set", con)             # TITLE5
writeLines("", con)                                          # blank line

# Footnotes (match SAS footnotes)
writeLines("CV% = coefficient of variation (%) = sd/mean*100", con)                     # FOOTNOTE1
writeLines("CV% geo-mean = (sqrt(exp(variance for log transformed data)-1))*100", con) # FOOTNOTE2
writeLines("Geo-mean: Geometric mean", con)                                            # FOOTNOTE3
writeLines("Geo-mean and CV% geo-mean not presented when the minimum value for a parameter is zero.", con) # FOOTNOTE4
writeLines("Data: adpp  Program: pps.sas  Output: t.14.2.ppts.txt", con)               # FOOTNOTE5
writeLines("Fake Data/ Production Run on 17MAR2014:23:57", con)                         # FOOTNOTE6
writeLines("", con)                                                                   # blank line

# Header line (fixed width)
param_names <- names(listing_wide)[-(1:6)]
header_fmt <- paste0(
  "%-8s %-8s %-15s %-20s %-8s %-12s ",
  paste(rep("%-12s", length(param_names)), collapse = " ")
)
header_line <- sprintf(
  header_fmt,
  "ANLPRNT", "PPSPECL", "PPCATL", "TRTAN", "PPSDY", "Statistic",
  param_names
)
writeLines(header_line, con)
writeLines(strrep("-", nchar(header_line)), con)

# Body rows
apply(listing_wide, 1, function(row) {
  line <- sprintf(
    header_fmt,
    row["anlprnt"], row["ppspecl"], row["ppcatl"], row["trtan"],
    row["ppsdy"], row["statistic"],
    row[param_names]
  )
  writeLines(line, con)
})

close(con)

# ------------------------------------------------------------------
# End of script

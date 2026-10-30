```r
# ------------------------------------------------------------
# P035_target14 – SAS to R translation
# ------------------------------------------------------------
# Packages
library(haven)
library(dplyr)
library(stringr)
library(purrr)

# ------------------------------------------------------------
# Helper: SAS‑style rounding (half up) – not needed here but kept
sas_round <- function(x, digits = 0) {
  pos <- x >= 0
  factor <- 10 ^ digits
  ifelse(pos,
         floor(x * factor + 0.5) / factor,
         ceiling(x * factor - 0.5) / factor)
}

# ------------------------------------------------------------
# Directories (macro variables are not exported as env vars, so use literals)
rawadam  <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
out_dir  <- Sys.getenv("OUT_DIR")

# ------------------------------------------------------------
# 1. Read source ADaM XPT
adpp_raw <- read_xpt(file.path(rawadam, "adpp.xpt"))

# ------------------------------------------------------------
# 2. Apply SAS WHERE clause and create _PARAM
adpp <- adpp_raw %>%
  filter(
    PKFN == 1,
    ANL01FN != 1,
    paramcd %in% c("AUCINFO", "AUCLAST", "CMAX", "TMAX", "MRTLAST", "LAMZHL"),
    !is.na(PPSTRESN),
    str_replace_all(PPCATL, "\\s+", "") == "ANAL2"
  ) %>%
  mutate(
    # $PK format mapping (character variable PPTESTCD)
    pk_fmt = case_when(
      PPTESTCD == "AUCINFO" ~ "AUC",
      PPTESTCD == "AUCTAU"  ~ "AUCtau",
      PPTESTCD == "AUCLAST" ~ "AUC(0-tlast)",
      PPTESTCD == "CMAX"    ~ "Cmax",
      PPTESTCD == "CMAXD"   ~ "Cmax/Dose",
      PPTESTCD == "MRTLAST"~ "MRT",
      PPTESTCD == "TLAST"   ~ "tlast",
      PPTESTCD == "TMAX"    ~ "tmax",
      PPTESTCD == "CLRFO"   ~ "CL/F",
      PPTESTCD == "VZFO"    ~ "VzF",
      PPTESTCD == "LAMZ"    ~ "lambdaZ",
      PPTESTCD == "LAMZHL"  ~ "t1/2",
      PPTESTCD == "CLAST"   ~ "Clast",
      TRUE                  ~ PPTESTCD
    ),
    _PARAM = str_squish(paste0(pk_fmt, " (", str_trim(PPORRESU), ")")),
    AVAL_log = log(AVAL)
  ) %>%
  select(-pk_fmt)

# ------------------------------------------------------------
# 3. PROC SORT equivalent
adpp <- adpp %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, _PARAM)

# ------------------------------------------------------------
# 4. PROC UNIVARIATE (by‑group summary)
adpp_sum <- adpp %>%
  group_by(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, _PARAM) %>%
  summarise(
    n1   = sum(!is.na(AVAL)),
    n2   = sum(!is.na(AVAL_log)),
    mn1  = mean(AVAL,   na.rm = TRUE),
    mn2  = mean(AVAL_log, na.rm = TRUE),
    std1 = sd(AVAL,   na.rm = TRUE),
    std2 = sd(AVAL_log, na.rm = TRUE),
    cv1  = ifelse(mn1 != 0, std1 / mn1 * 100, .Machine$double.xmin),
    cv2  = ifelse(mn2 != 0, std2 / mn2 * 100, .Machine$double.xmin),
    med1 = median(AVAL,   na.rm = TRUE),
    med2 = median(AVAL_log, na.rm = TRUE),
    min1 = min(AVAL,   na.rm = TRUE),
    min2 = min(AVAL_log, na.rm = TRUE),
    max1 = max(AVAL,   na.rm = TRUE),
    max2 = max(AVAL_log, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 5. Transpose to adpp_sum_t (7 rows per original group)
fmt_num <- function(x, w, d) {
  if (is.na(x)) return("")
  sprintf(paste0("%", w, ".", d, "f"), x)
}

adpp_sum_t <- adpp_sum %>%
  rowwise() %>%
  do({
    r <- .
    tibble(
      ANLPRNT = r$ANLPRNT,
      PPSPECL = r$PPSPECL,
      PPCATL  = r$PPCATL,
      TRTAN   = r$TRTAN,
      PPSDY   = r$PPSDY,
      s_order = 1:7,
      _PARAM  = r$_PARAM,
      value   = c(
        # 1 n
        fmt_num(r$n1, 2, 0),
        # 2 mean (sd) – format varies by parameter
        {
          if (grepl("AUC", r$_PARAM, ignore.case = TRUE)) {
            paste0(fmt_num(r$mn1, 6, 0), " (", fmt_num(r$std1, 5, 0), ")")
          } else if (grepl("Cmax", r$_PARAM, ignore.case = TRUE)) {
            paste0(fmt_num(r$mn1, 5, 0), " (", fmt_num(r$std1, 4, 0), ")")
          } else if (grepl("MRT", r$_PARAM, ignore.case = TRUE)) {
            paste0(fmt_num(r$mn1, 4, 1), " (", fmt_num(r$std1, 4, 2), ")")
          } else if (grepl("t1/2", r$_PARAM, ignore.case = TRUE)) {
            paste0(fmt_num(r$mn1, 4, 2), " (", fmt_num(r$std1, 4, 2), ")")
          } else {
            ""
          }
        },
        # 3 CV% mean (skip for tmax)
        if (grepl("tmax", r$_PARAM, ignore.case = TRUE)) "" else fmt_num(r$cv1, 4, 1),
        # 4 Geo‑mean
        {
          if (grepl("AUC", r$_PARAM, ignore.case = TRUE)) {
            fmt_num(exp(r$mn2), 6, 0)
          } else if (grepl("Cmax", r$_PARAM, ignore.case = TRUE)) {
            fmt_num(exp(r$mn2), 5, 0)
          } else if (grepl("MRT", r$_PARAM, ignore.case = TRUE)) {
            fmt_num(exp(r$mn2), 4, 1)
          } else if (grepl("t1/2", r$_PARAM, ignore.case = TRUE)) {
            fmt_num(exp(r$mn2), 4, 2)
          } else {
            ""
          }
        },
        # 5 CV% geo‑mean (skip for tmax)
        if (grepl("tmax", r$_PARAM, ignore.case = TRUE)) "" else
          fmt_num(sqrt(exp(r$std2^2) - 1) * 100, 4, 1),
        # 6 Median
        {
          if (grepl("AUC", r$_PARAM, ignore.case = TRUE)) {
            fmt_num(r$med1, 6, 0)
          } else if (grepl("Cmax", r$_PARAM, ignore.case = TRUE)) {
            fmt_num(r$med1, 5, 0)
          } else if (grepl("MRT", r$_PARAM, ignore.case = TRUE)) {
            fmt_num(r$med1, 4, 1)
          } else if (grepl("t1/2", r$_PARAM, ignore.case = TRUE)) {
            fmt_num(r$med1, 4, 2)
          } else if (grepl("tmax", r$_PARAM, ignore.case = TRUE)) {
            fmt_num(r$med1, 4, 2)
          } else {
            ""
          }
        },
        # 7 [min; max]
        {
          if (grepl("AUC", r$_PARAM, ignore.case = TRUE)) {
            paste0("[", fmt_num(r$min1, 6, 0), ";", fmt_num(r$max1, 6, 0), "]")
          } else if (grepl("Cmax", r$_PARAM, ignore.case = TRUE)) {
            paste0("[", fmt_num(r$min1, 4, 0), ";", fmt_num(r$max1, 5, 0), "]")
          } else if (grepl("MRT", r$_PARAM, ignore.case = TRUE)) {
            paste0("[", fmt_num(r$min1, 4, 2), ";", fmt_num(r$max1, 4, 1), "]")
          } else if (grepl("t1/2", r$_PARAM, ignore.case = TRUE)) {
            paste0("[", fmt_num(r$min1, 4, 2), ";", fmt_num(r$max1, 4, 1), "]")
          } else if (grepl("tmax", r$_PARAM, ignore.case = TRUE)) {
            paste0("[", fmt_num(r$min1, 4, 2), ";", fmt_num(r$max1, 4, 2), "]")
          } else {
            ""
          }
        }
      )
    )
  }) %>%
  ungroup()

# ------------------------------------------------------------
# 6. Final PROC SORT ordering
adpp_sum_t <- adpp_sum_t %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, _PARAM)

# ------------------------------------------------------------
# 7. Write output datasets
write.csv(adpp_sum_t,
          file.path(out_dir, "adpp_sum_t.csv"),
          row.names = FALSE, na = "")

# PROC REPORT also creates CLXCHECKNOBS – same content here
CLXCHECKNOBS <- adpp_sum_t
write.csv(CLXCHECKNOBS,
          file.path(out_dir, "CLXCHECKNOBS.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------
# 8. Create listing (PROC REPORT style) – text file P035.lst
# Formats
trtan_fmt <- c(`1` = "Period 1 Treatment",
               `2` = "Period 2 Treatment",
               `3` = "Period 3 Treatment")
statg_fmt <- c(`1` = "n",
               `2` = "Mean (SD)",
               `3` = "CV% mean",
               `4` = "Geo-mean",
               `5` = "CV% geo-mean",
               `6` = "Median",
               `7` = "[Min; Max]        ")

fmt_trtan <- function(x) ifelse(is.na(x), "", trtan_fmt[as.character(x)])
fmt_stat  <- function(x) ifelse(is.na(x), "", statg_fmt[as.character(x)])

# Header lines (match SAS TITLE/FOOTNOTE)
txt <- c(
  "Study001",
  "Table 14.2-PPTS (Page xxx of xxx)",
  "Summary statistics for PK parameters",
  "by compound, matrix, analyte and actual treatment",
  "Analysis Set: PK analysis set",
  "",
  "CV% = coefficient of variation (%) = sd/mean*100",
  "CV% geo-mean = (sqrt(exp(variance for log transformed data)-1))*100",
  "Geo-mean: Geometric mean",
  "Geo-mean and CV% geo-mean not presented when the minimum value for a parameter is zero.",
  "Data: adpp  Program: pps.sas  Output: t.14.2.ppts.txt",
  "Fake Data/ Production Run on 17MAR2014:23:57",
  ""
)

# Build body
group_vars <- c("ANLPRNT", "PPSPECL", "PPCATL")
adpp_sum_t %>%
  group_by(across(all_of(group_vars))) %>%
  group_walk(~{
    grp <- .y
    # Compound/Matrix/Analyte line
    txt <<- c(txt,
              sprintf("Compound: %s, Matrix: %s, Analyte: %s",
                      grp$ANLPRNT, grp$PPSPECL, grp$PPCATL),
              "")
    # Rows for this group
    .x %>%
      arrange(TRTAN, PPSDY, s_order, _PARAM) %>%
      mutate(
        TRTAN_disp = fmt_trtan(TRTAN),
        stat_disp  = fmt_stat(s_order)
      ) %>%
      # Fixed‑width line similar to PROC REPORT default layout
      transmute(line = sprintf("%-20s %-8s %-14s %-15s %s",
                               TRTAN_disp,
                               ifelse(is.na(PPSDY), "", as.character(PPSDY)),
                               stat_disp,
                               _PARAM,
                               value)) %>%
      pull(line) %>%
      { txt <<- c(txt, .) }
    # blank line after each PPSDY (PROC REPORT BREAK)
    txt <<- c(txt, "")
  })

# Write listing text (SAS expects .lst; we use .lst)
writeLines(txt, file.path(out_dir, "P035.lst"))

# Also write a CSV version of the table (same columns as report)
report_csv <- adpp_sum_t %>%
  mutate(
    TRTAN = fmt_trtan(TRTAN),
    s_order = fmt_stat(s_order)
  ) %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, _PARAM, value)

write.csv(report_csv,
          file.path(out_dir, "P035.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------
# End of script
```
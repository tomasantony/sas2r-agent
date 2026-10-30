# ------------------------------------------------------------
# P035_target14 – SAS to R translation (attempt 3)
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
# make column names lower‑case for easier handling
names(adpp_raw) <- tolower(names(adpp_raw))

# ------------------------------------------------------------
# 2. Apply SAS WHERE clause and create _PARAM
adpp <- adpp_raw %>%
  filter(
    pkfn == 1,
    anl01fn != 1,
    paramcd %in% c("AUCINFO", "AUCLAST", "CMAX", "TMAX", "MRTLAST", "LAMZHL"),
    !is.na(ppstresn),
    str_replace_all(ppcatl, "\\s+", "") == "ANAL2"
  ) %>%
  mutate(
    # $PK format mapping (character variable pptestcd)
    pk_fmt = case_when(
      pptestcd == "AUCINFO" ~ "AUC",
      pptestcd == "AUCTAU"  ~ "AUCtau",
      pptestcd == "AUCLAST" ~ "AUC(0-tlast)",
      pptestcd == "CMAX"    ~ "Cmax",
      pptestcd == "CMAXD"   ~ "Cmax/Dose",
      pptestcd == "MRTLAST"~ "MRT",
      pptestcd == "TLAST"   ~ "tlast",
      pptestcd == "TMAX"    ~ "tmax",
      pptestcd == "CLRFO"   ~ "CL/F",
      pptestcd == "VZFO"    ~ "VzF",
      pptestcd == "LAMZ"    ~ "lambdaZ",
      pptestcd == "LAMZHL"  ~ "t1/2",
      pptestcd == "CLAST"   ~ "Clast",
      TRUE                  ~ pptestcd
    ),
    _param = str_squish(paste0(pk_fmt, " (", str_trim(pporresu), ")")),
    aval_log = log(aval)
  ) %>%
  select(-pk_fmt)

# ------------------------------------------------------------
# 3. PROC SORT equivalent
adpp <- adpp %>%
  arrange(anlprnt, ppspecl, ppcatl, trtan, ppsdy, _param)

# ------------------------------------------------------------
# 4. PROC UNIVARIATE (by‑group summary)
adpp_sum <- adpp %>%
  group_by(anlprnt, ppspecl, ppcatl, trtan, ppsdy, _param) %>%
  summarise(
    n1   = sum(!is.na(aval)),
    n2   = sum(!is.na(aval_log)),
    mn1  = mean(aval,   na.rm = TRUE),
    mn2  = mean(aval_log, na.rm = TRUE),
    std1 = sd(aval,   na.rm = TRUE),
    std2 = sd(aval_log, na.rm = TRUE),
    cv1  = ifelse(mn1 != 0, std1 / mn1 * 100, .Machine$double.xmin),
    cv2  = ifelse(mn2 != 0, std2 / mn2 * 100, .Machine$double.xmin),
    med1 = median(aval,   na.rm = TRUE),
    med2 = median(aval_log, na.rm = TRUE),
    min1 = min(aval,   na.rm = TRUE),
    min2 = min(aval_log, na.rm = TRUE),
    max1 = max(aval,   na.rm = TRUE),
    max2 = max(aval_log, na.rm = TRUE),
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
      anlprnt = r$anlprnt,
      ppspecl = r$ppspecl,
      ppcatl  = r$ppcatl,
      trtan   = r$trtan,
      ppsdy   = r$ppsdy,
      s_order = 1:7,
      _param  = r$_param,
      value   = c(
        # 1 n
        fmt_num(r$n1, 2, 0),
        # 2 mean (sd) – format varies by parameter
        {
          if (grepl("AUC", r$_param, ignore.case = TRUE)) {
            paste0(fmt_num(r$mn1, 6, 0), " (", fmt_num(r$std1, 5, 0), ")")
          } else if (grepl("Cmax", r$_param, ignore.case = TRUE)) {
            paste0(fmt_num(r$mn1, 5, 0), " (", fmt_num(r$std1, 4, 0), ")")
          } else if (grepl("MRT", r$_param, ignore.case = TRUE)) {
            paste0(fmt_num(r$mn1, 4, 1), " (", fmt_num(r$std1, 4, 2), ")")
          } else if (grepl("t1/2", r$_param, ignore.case = TRUE)) {
            paste0(fmt_num(r$mn1, 4, 2), " (", fmt_num(r$std1, 4, 2), ")")
          } else {
            ""
          }
        },
        # 3 CV% mean (skip for tmax)
        if (grepl("tmax", r$_param, ignore.case = TRUE)) "" else fmt_num(r$cv1, 4, 1),
        # 4 Geo‑mean
        {
          if (grepl("AUC", r$_param, ignore.case = TRUE)) {
            fmt_num(exp(r$mn2), 6, 0)
          } else if (grepl("Cmax", r$_param, ignore.case = TRUE)) {
            fmt_num(exp(r$mn2), 5, 0)
          } else if (grepl("MRT", r$_param, ignore.case = TRUE)) {
            fmt_num(exp(r$mn2), 4, 1)
          } else if (grepl("t1/2", r$_param, ignore.case = TRUE)) {
            fmt_num(exp(r$mn2), 4, 2)
          } else {
            ""
          }
        },
        # 5 CV% geo‑mean (skip for tmax)
        if (grepl("tmax", r$_param, ignore.case = TRUE)) "" else
          fmt_num(sqrt(exp(r$std2^2) - 1) * 100, 4, 1),
        # 6 Median
        {
          if (grepl("AUC", r$_param, ignore.case = TRUE)) {
            fmt_num(r$med1, 6, 0)
          } else if (grepl("Cmax", r$_param, ignore.case = TRUE)) {
            fmt_num(r$med1, 5, 0)
          } else if (grepl("MRT", r$_param, ignore.case = TRUE)) {
            fmt_num(r$med1, 4, 1)
          } else if (grepl("t1/2", r$_param, ignore.case = TRUE)) {
            fmt_num(r$med1, 4, 2)
          } else if (grepl("tmax", r$_param, ignore.case = TRUE)) {
            fmt_num(r$med1, 4, 2)
          } else {
            ""
          }
        },
        # 7 [min; max]
        {
          if (grepl("AUC", r$_param, ignore.case = TRUE)) {
            paste0("[", fmt_num(r$min1, 6, 0), ";", fmt_num(r$max1, 6, 0), "]")
          } else if (grepl("Cmax", r$_param, ignore.case = TRUE)) {
            paste0("[", fmt_num(r$min1, 4, 0), ";", fmt_num(r$max1, 5, 0), "]")
          } else if (grepl("MRT", r$_param, ignore.case = TRUE)) {
            paste0("[", fmt_num(r$min1, 4, 2), ";", fmt_num(r$max1, 4, 1), "]")
          } else if (grepl("t1/2", r$_param, ignore.case = TRUE)) {
            paste0("[", fmt_num(r$min1, 4, 2), ";", fmt_num(r$max1, 4, 1), "]")
          } else if (grepl("tmax", r$_param, ignore.case = TRUE)) {
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
  arrange(anlprnt, ppspecl, ppcatl, trtan, ppsdy, s_order, _param)

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

# Build body – group by compound/matrix/analyte
group_vars <- c("anlprnt", "ppspecl", "ppcatl")
adpp_sum_t %>%
  group_by(across(all_of(group_vars))) %>%
  group_walk(~{
    grp <- .y
    # Compound/Matrix/Analyte line
    txt <<- c(txt,
              sprintf("Compound: %s, Matrix: %s, Analyte: %s",
                      grp$anlprnt, grp$ppspecl, grp$ppcatl),
              "")
    # Rows for this group
    .x %>%
      arrange(trtan, ppsdy, s_order, _param) %>%
      mutate(
        trtan_disp = fmt_trtan(trtan),
        stat_disp  = fmt_stat(s_order)
      ) %>%
      transmute(line = sprintf("%-20s %-8s %-14s %-15s %s",
                               trtan_disp,
                               ifelse(is.na(ppsdy), "", as.character(ppsdy)),
                               stat_disp,
                               _param,
                               value)) %>%
      pull(line) %>%
      { txt <<- c(txt, .) }
    # blank line after each PPSDY (PROC REPORT BREAK)
    txt <<- c(txt, "")
  })

# Write the listing text (SAS expects .lst; we use .lst)
writeLines(txt, file.path(out_dir, "P035.lst"))

# Also write a CSV version of the table (same columns as report)
report_csv <- adpp_sum_t %>%
  mutate(
    trtan = fmt_trtan(trtan),
    s_order = fmt_stat(s_order)
  ) %>%
  select(anlprnt, ppspecl, ppcatl, trtan, ppsdy, s_order, _param, value)

write.csv(report_csv,
          file.path(out_dir, "P035.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------
# End of script

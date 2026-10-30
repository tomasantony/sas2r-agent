# P035_target14 translation to R
# ------------------------------------------------------------
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ------------------------------------------------------------
# Helper functions ------------------------------------------------
# SAS round-half-up (away from zero) – not needed for this script
# but defined for completeness
sas_round <- function(x, digits = 0) {
  pos <- x >= 0
  factor <- 10 ^ digits
  res <- ifelse(pos,
                floor(x * factor + 0.5) / factor,
                ceiling(x * factor - 0.5) / factor)
  res
}

# ------------------------------------------------------------
# Environment ----------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
rawadam <- Sys.getenv("rawadam")
# ------------------------------------------------------------
# 1. Read source ADaM dataset ------------------------------------
adpp_raw <- read_xpt(file.path(rawadam, "adpp.xpt"))

# ------------------------------------------------------------
# 2. Apply SAS WHERE clause --------------------------------------
adpp <- adpp_raw %>%
  filter(
    PKFN == 1,
    ANL01FN != 1,
    paramcd %in% c("AUCINFO", "AUCLAST", "CMAX", "TMAX", "MRTLAST", "LAMZHL"),
    !is.na(PPSTRESN),
    str_replace_all(PPCATL, "\\s+", "") == "ANAL2"
  ) %>%
  # create _PARAM using $PK format and strip(PPORRESU)
  mutate(
    # $PK format mapping
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
# 3. Sort as SAS PROC SORT ---------------------------------------
adpp <- adpp %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, _PARAM)

# ------------------------------------------------------------
# 4. PROC UNIVARIATE equivalent (by groups) --------------------
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
# 5. Transpose to create adpp_sum_t (like PROC TRANSPOSE) -------
adpp_sum_t <- adpp_sum %>%
  rowwise() %>%
  do({
    row <- .
    # Helper to format numbers as SAS does (default width/decimals)
    fmt_num <- function(x, w, d) {
      if (is.na(x)) return("")
      sprintf(paste0("%", w, ".", d, "f"), x)
    }
    # Build the 7 rows per original observation
    tibble(
      ANLPRNT = row$ANLPRNT,
      PPSPECL = row$PPSPECL,
      PPCATL  = row$PPCATL,
      TRTAN   = row$TRTAN,
      PPSDY   = row$PPSDY,
      s_order = c(1,2,3,4,5,6,7),
      _PARAM  = row$_PARAM,
      value   = c(
        # 1 n
        fmt_num(row$n1, 2, 0),
        # 2 mean (sd) with different formats per parameter
        {
          if (grepl("AUC", row$_PARAM, ignore.case = TRUE)) {
            paste0(fmt_num(row$mn1, 6, 0), " (", fmt_num(row$std1, 5, 0), ")")
          } else if (grepl("Cmax", row$_PARAM, ignore.case = TRUE)) {
            paste0(fmt_num(row$mn1, 5, 0), " (", fmt_num(row$std1, 4, 0), ")")
          } else if (grepl("MRT", row$_PARAM, ignore.case = TRUE)) {
            paste0(fmt_num(row$mn1, 4, 1), " (", fmt_num(row$std1, 4, 2), ")")
          } else if (grepl("t1/2", row$_PARAM, ignore.case = TRUE)) {
            paste0(fmt_num(row$mn1, 4, 2), " (", fmt_num(row$std1, 4, 2), ")")
          } else {
            ""
          }
        },
        # 3 CV% mean
        {
          if (grepl("tmax", row$_PARAM, ignore.case = TRUE)) "" else fmt_num(row$cv1, 4, 1)
        },
        # 4 geo-mean
        {
          if (grepl("AUC", row$_PARAM, ignore.case = TRUE)) {
            fmt_num(exp(row$mn2), 6, 0)
          } else if (grepl("Cmax", row$_PARAM, ignore.case = TRUE)) {
            fmt_num(exp(row$mn2), 5, 0)
          } else if (grepl("MRT", row$_PARAM, ignore.case = TRUE)) {
            fmt_num(exp(row$mn2), 4, 1)
          } else if (grepl("t1/2", row$_PARAM, ignore.case = TRUE)) {
            fmt_num(exp(row$mn2), 4, 2)
          } else {
            ""
          }
        },
        # 5 CV% geo-mean
        {
          if (grepl("tmax", row$_PARAM, ignore.case = TRUE)) "" else
            fmt_num(sqrt(exp(row$std2^2) - 1) * 100, 4, 1)
        },
        # 6 median
        {
          if (grepl("AUC", row$_PARAM, ignore.case = TRUE)) {
            fmt_num(row$med1, 6, 0)
          } else if (grepl("Cmax", row$_PARAM, ignore.case = TRUE)) {
            fmt_num(row$med1, 5, 0)
          } else if (grepl("MRT", row$_PARAM, ignore.case = TRUE)) {
            fmt_num(row$med1, 4, 1)
          } else if (grepl("t1/2", row$_PARAM, ignore.case = TRUE)) {
            fmt_num(row$med1, 4, 2)
          } else if (grepl("tmax", row$_PARAM, ignore.case = TRUE)) {
            fmt_num(row$med1, 4, 2)
          } else {
            ""
          }
        },
        # 7 [min; max]
        {
          if (grepl("AUC", row$_PARAM, ignore.case = TRUE)) {
            paste0("[", fmt_num(row$min1, 6, 0), ";", fmt_num(row$max1, 6, 0), "]")
          } else if (grepl("Cmax", row$_PARAM, ignore.case = TRUE)) {
            paste0("[", fmt_num(row$min1, 4, 0), ";", fmt_num(row$max1, 5, 0), "]")
          } else if (grepl("MRT", row$_PARAM, ignore.case = TRUE)) {
            paste0("[", fmt_num(row$min1, 4, 2), ";", fmt_num(row$max1, 4, 1), "]")
          } else if (grepl("t1/2", row$_PARAM, ignore.case = TRUE)) {
            paste0("[", fmt_num(row$min1, 4, 2), ";", fmt_num(row$max1, 4, 1), "]")
          } else if (grepl("tmax", row$_PARAM, ignore.case = TRUE)) {
            paste0("[", fmt_num(row$min1, 4, 2), ";", fmt_num(row$max1, 4, 2), "]")
          } else {
            ""
          }
        }
      )
    )
  }) %>%
  ungroup()

# ------------------------------------------------------------
# 6. Final sort (as PROC SORT) -----------------------------------
adpp_sum_t <- adpp_sum_t %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, _PARAM)

# ------------------------------------------------------------
# 7. Write output datasets -----------------------------------------
# adpp_sum_t dataset
write.csv(adpp_sum_t,
          file.path(out_dir, "adpp_sum_t.csv"),
          row.names = FALSE, na = "")

# PROC REPORT output dataset (same as adpp_sum_t for this case)
CLXCHECKNOBS <- adpp_sum_t
write.csv(CLXCHECKNOBS,
          file.path(out_dir, "CLXCHECKNOBS.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------
# 8. Create listing (PROC REPORT style) -------------------------
# Define formats
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

# Build text lines
txt_lines <- c(
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

# Helper to get formatted values
fmt_trtan <- function(x) ifelse(is.na(x), "", trtan_fmt[as.character(x)])
fmt_stat  <- function(x) ifelse(is.na(x), "", statg_fmt[as.character(x)])

# Iterate over groups to build report body
group_vars <- c("ANLPRNT", "PPSPECL", "PPCATL")
adpp_sum_t %>%
  group_by(across(all_of(group_vars))) %>%
  group_walk(~{
    grp <- .y
    # compute line with compound, matrix, analyte
    comp_line <- sprintf("Compound: %s, Matrix: %s, Analyte: %s",
                         grp$ANLPRNT, grp$PPSPECL, grp$PPCATL)
    txt_lines <<- c(txt_lines, comp_line, "")
    # within group, iterate rows
    .x %>%
      arrange(TRTAN, PPSDY, s_order, _PARAM) %>%
      rowwise() %>%
      mutate(
        TRTAN_disp = fmt_trtan(TRTAN),
        s_order_disp = fmt_stat(s_order)
      ) %>%
      ungroup() %>%
      # Build a fixed‑width line similar to PROC REPORT default layout
      mutate(line = sprintf("%-20s %-8s %-14s %-15s %s",
                            ifelse(TRTAN_disp == "", "", TRTAN_disp),
                            ifelse(is.na(PPSDY), "", as.character(PPSDY)),
                            s_order_disp,
                            _PARAM,
                            value)) %>%
      pull(line) %>%
      { txt_lines <<- c(txt_lines, .) }
    # break after each PPSDY (skip line)
    txt_lines <<- c(txt_lines, "")
  })

# Write the listing text file
writeLines(txt_lines, con = file.path(out_dir, "P035.txt"))

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

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: SAS yymmdd10. is strictly YYYY-MM-DD (one explicit format)
parse_yymmdd10 <- function(x) as.Date(x, format = "%Y-%m-%d")

# raw.raw_dm
raw_dm <- data.frame(
  SUBJID = c(101L, 102L, 103L, 104L),
  SEX    = c("M", "F", "M", "F"),
  AGE    = c(55L, 62L, 48L, 59L),
  TRTGRP = c("A", "B", "A", "B"),
  RANDDT = parse_yymmdd10(c("2022-01-10", "2022-01-12", "2022-01-15", "2022-01-18")),
  RFENDT = parse_yymmdd10(c("2022-06-30", "2022-06-28", "2022-06-29", "2022-06-27")),
  stringsAsFactors = FALSE
)

# raw.raw_ex
raw_ex <- data.frame(
  SUBJID  = c(101L, 102L, 103L, 104L),
  EXSTDAT = parse_yymmdd10(c("2022-01-11", "2022-01-13", "2022-01-16", "2022-01-19")),
  EXENDAT = parse_yymmdd10(c("2022-06-30", "2022-06-28", "2022-06-29", "2022-06-27")),
  DOSE    = c(10L, 0L, 10L, 0L),
  stringsAsFactors = FALSE
)

# raw.raw_lb
raw_lb <- data.frame(
  SUBJID = c(101L, 101L, 101L, 101L, 102L, 102L, 102L, 102L, 103L, 103L, 103L, 104L, 104L, 104L),
  VISIT  = c("Screening", "Baseline", "Week12", "Week24", "Screening", "Baseline", "Week12", "Week24", "Screening", "Baseline", "Week24", "Screening", "Baseline", "Week24"),
  LBDT   = parse_yymmdd10(c("2021-12-28", "2022-01-10", "2022-04-04", "2022-06-28", "2021-12-30", "2022-01-12", "2022-04-06", "2022-06-26", "2021-12-29", "2022-01-15", "2022-06-27", "2021-12-31", "2022-01-18", "2022-06-25")),
  PARAM  = rep("HbA1c", 14),
  AVAL   = c(8.2, 8.1, 7.4, 6.9, 8.5, 8.4, 8.1, 7.9, 7.9, 7.8, 7.0, 8.3, 8.2, 8.0),
  stringsAsFactors = FALSE
)

# raw.raw_ae
raw_ae <- data.frame(
  SUBJID = c(101L, 101L, 102L, 103L),
  AETERM = c("Headache", "Nausea", "Dizziness", "Fatigue"),
  AESER  = c("N", "N", "Y", "N"),
  AESEV  = c("MILD", "MODERATE", "SEVERE", "MILD"),
  AESTDT = parse_yymmdd10(c("2022-02-01", "2022-03-15", "2022-04-10", "2022-02-20")),
  AEENDT = parse_yymmdd10(c("2022-02-03", "2022-03-18", "2022-04-15", "2022-02-22")),
  stringsAsFactors = FALSE
)

# raw.raw_vs
raw_vs <- data.frame(
  SUBJID = c(101L, 101L, 102L, 102L),
  VISIT  = c("Baseline", "Week24", "Baseline", "Week24"),
  VSDT   = parse_yymmdd10(c("2022-01-10", "2022-06-28", "2022-01-12", "2022-06-26")),
  PARAM  = rep("WEIGHT", 4),
  AVAL   = c(82L, 78L, 75L, 74L),
  stringsAsFactors = FALSE
)

# Ensure ISO date character formatting for CSV (YYYY-MM-DD)
format_dates_iso <- function(df) {
  for (nm in names(df)) {
    if (inherits(df[[nm]], "Date")) {
      df[[nm]] <- format(df[[nm]], format = "%Y-%m-%d")
    }
    if (inherits(df[[nm]], "POSIXct") || inherits(df[[nm]], "POSIXt")) {
      df[[nm]] <- format(df[[nm]], format = "%Y-%m-%dT%H:%M:%S")
    }
  }
  df
}

raw_dm_out <- format_dates_iso(raw_dm)
raw_ex_out <- format_dates_iso(raw_ex)
raw_lb_out <- format_dates_iso(raw_lb)
raw_ae_out <- format_dates_iso(raw_ae)
raw_vs_out <- format_dates_iso(raw_vs)

write.csv(raw_dm_out, file.path(out, "raw_dm.csv"), row.names = FALSE, na = "")
write.csv(raw_ex_out, file.path(out, "raw_ex.csv"), row.names = FALSE, na = "")
write.csv(raw_lb_out, file.path(out, "raw_lb.csv"), row.names = FALSE, na = "")
write.csv(raw_ae_out, file.path(out, "raw_ae.csv"), row.names = FALSE, na = "")
write.csv(raw_vs_out, file.path(out, "raw_vs.csv"), row.names = FALSE, na = "")

```r
# P066 Raw Clinical Trial Data Creation - R translation
# SAS default: character variables created via INPUT $ without LENGTH default to length 8
# => values longer than 8 are truncated (e.g. 'Dizziness'->'Dizzines', 'Screening'->'Screenin')

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

sas_trunc8 <- function(x) substr(x, 1, 8)

# raw.raw_dm
raw_dm <- data.frame(
  SUBJID = c(101, 102, 103, 104),
  SEX = sas_trunc8(c("M", "F", "M", "F")),
  AGE = c(55, 62, 48, 59),
  TRTGRP = sas_trunc8(c("A", "B", "A", "B")),
  RANDDT = as.Date(c("2022-01-10", "2022-01-12", "2022-01-15", "2022-01-18")),
  RFENDT = as.Date(c("2022-06-30", "2022-06-28", "2022-06-29", "2022-06-27")),
  stringsAsFactors = FALSE
)

# raw.raw_ex
raw_ex <- data.frame(
  SUBJID = c(101, 102, 103, 104),
  EXSTDAT = as.Date(c("2022-01-11", "2022-01-13", "2022-01-16", "2022-01-19")),
  EXENDAT = as.Date(c("2022-06-30", "2022-06-28", "2022-06-29", "2022-06-27")),
  DOSE = c(10, 0, 10, 0),
  stringsAsFactors = FALSE
)

# raw.raw_lb : VISIT length 8 => 'Screening' truncated to 'Screenin'
raw_lb <- data.frame(
  SUBJID = c(101, 101, 101, 101, 102, 102, 102, 102, 103, 103, 103, 104, 104, 104),
  VISIT = sas_trunc8(c("Screening", "Baseline", "Week12", "Week24",
            "Screening", "Baseline", "Week12", "Week24",
            "Screening", "Baseline", "Week24",
            "Screening", "Baseline", "Week24")),
  LBDT = as.Date(c("2021-12-28", "2022-01-10", "2022-04-04", "2022-06-28",
                   "2021-12-30", "2022-01-12", "2022-04-06", "2022-06-26",
                   "2021-12-29", "2022-01-15", "2022-06-27",
                   "2021-12-31", "2022-01-18", "2022-06-25")),
  PARAM = sas_trunc8(rep("HbA1c", 14)),
  AVAL = c(8.2, 8.1, 7.4, 6.9, 8.5, 8.4, 8.1, 7.9, 7.9, 7.8, 7.0, 8.3, 8.2, 8.0),
  stringsAsFactors = FALSE
)

# raw.raw_ae : AETERM length 8 => 'Dizziness' truncated to 'Dizzines'
raw_ae <- data.frame(
  SUBJID = c(101, 101, 102, 103),
  AETERM = sas_trunc8(c("Headache", "Nausea", "Dizziness", "Fatigue")),
  AESER = sas_trunc8(c("N", "N", "Y", "N")),
  AESEV = sas_trunc8(c("MILD", "MODERATE", "SEVERE", "MILD")),
  AESTDT = as.Date(c("2022-02-01", "2022-03-15", "2022-04-10", "2022-02-20")),
  AEENDT = as.Date(c("2022-02-03", "2022-03-18", "2022-04-15", "2022-02-22")),
  stringsAsFactors = FALSE
)

# raw.raw_vs
raw_vs <- data.frame(
  SUBJID = c(101, 101, 102, 102),
  VISIT = sas_trunc8(c("Baseline", "Week24", "Baseline", "Week24")),
  VSDT = as.Date(c("2022-01-10", "2022-06-28", "2022-01-12", "2022-06-26")),
  PARAM = sas_trunc8(rep("WEIGHT", 4)),
  AVAL = c(82, 78, 75, 74),
  stringsAsFactors = FALSE
)

write.csv(raw_dm, file.path(out, "raw_dm.csv"), row.names = FALSE, na = "")
write.csv(raw_ex, file.path(out, "raw_ex.csv"), row.names = FALSE, na = "")
write.csv(raw_lb, file.path(out, "raw_lb.csv"), row.names = FALSE, na = "")
write.csv(raw_ae, file.path(out, "raw_ae.csv"), row.names = FALSE, na = "")
write.csv(raw_vs, file.path(out, "raw_vs.csv"), row.names = FALSE, na = "")

cat("P066 complete: 5 datasets written to", out, "\n")
```
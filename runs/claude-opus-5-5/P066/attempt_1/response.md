```r
# P066 - Raw Clinical Trial Data Creation (translated from SAS)
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# SAS list input with "$" and no informat => character length 8 (truncation)
sas_chr8 <- function(x) substr(x, 1, 8)

read_lines_df <- function(txt, cols) {
  df <- read.csv(text = txt, header = FALSE, stringsAsFactors = FALSE,
                 col.names = cols, colClasses = "character")
  df
}
to_date <- function(x) format(as.Date(x, format = "%Y-%m-%d"), "%Y-%m-%d")

# ---- raw_dm ----
raw_dm <- read_lines_df(
"101,M,55,A,2022-01-10,2022-06-30
102,F,62,B,2022-01-12,2022-06-28
103,M,48,A,2022-01-15,2022-06-29
104,F,59,B,2022-01-18,2022-06-27",
c("SUBJID","SEX","AGE","TRTGRP","RANDDT","RFENDT"))
raw_dm$SUBJID <- as.numeric(raw_dm$SUBJID)
raw_dm$SEX    <- sas_chr8(raw_dm$SEX)
raw_dm$AGE    <- as.numeric(raw_dm$AGE)
raw_dm$TRTGRP <- sas_chr8(raw_dm$TRTGRP)
raw_dm$RANDDT <- to_date(raw_dm$RANDDT)
raw_dm$RFENDT <- to_date(raw_dm$RFENDT)

# ---- raw_ex ----
raw_ex <- read_lines_df(
"101,2022-01-11,2022-06-30,10
102,2022-01-13,2022-06-28,0
103,2022-01-16,2022-06-29,10
104,2022-01-19,2022-06-27,0",
c("SUBJID","EXSTDAT","EXENDAT","DOSE"))
raw_ex$SUBJID  <- as.numeric(raw_ex$SUBJID)
raw_ex$EXSTDAT <- to_date(raw_ex$EXSTDAT)
raw_ex$EXENDAT <- to_date(raw_ex$EXENDAT)
raw_ex$DOSE    <- as.numeric(raw_ex$DOSE)

# ---- raw_lb ----
raw_lb <- read_lines_df(
"101,Screening,2021-12-28,HbA1c,8.2
101,Baseline,2022-01-10,HbA1c,8.1
101,Week12,2022-04-04,HbA1c,7.4
101,Week24,2022-06-28,HbA1c,6.9
102,Screening,2021-12-30,HbA1c,8.5
102,Baseline,2022-01-12,HbA1c,8.4
102,Week12,2022-04-06,HbA1c,8.1
102,Week24,2022-06-26,HbA1c,7.9
103,Screening,2021-12-29,HbA1c,7.9
103,Baseline,2022-01-15,HbA1c,7.8
103,Week24,2022-06-27,HbA1c,7.0
104,Screening,2021-12-31,HbA1c,8.3
104,Baseline,2022-01-18,HbA1c,8.2
104,Week24,2022-06-25,HbA1c,8.0",
c("SUBJID","VISIT","LBDT","PARAM","AVAL"))
raw_lb$SUBJID <- as.numeric(raw_lb$SUBJID)
raw_lb$VISIT  <- sas_chr8(raw_lb$VISIT)
raw_lb$LBDT   <- to_date(raw_lb$LBDT)
raw_lb$PARAM  <- sas_chr8(raw_lb$PARAM)
raw_lb$AVAL   <- as.numeric(raw_lb$AVAL)

# ---- raw_ae ----
raw_ae <- read_lines_df(
"101,Headache,N,MILD,2022-02-01,2022-02-03
101,Nausea,N,MODERATE,2022-03-15,2022-03-18
102,Dizziness,Y,SEVERE,2022-04-10,2022-04-15
103,Fatigue,N,MILD,2022-02-20,2022-02-22",
c("SUBJID","AETERM","AESER","AESEV","AESTDT","AEENDT"))
raw_ae$SUBJID <- as.numeric(raw_ae$SUBJID)
raw_ae$AETERM <- sas_chr8(raw_ae$AETERM)
raw_ae$AESER  <- sas_chr8(raw_ae$AESER)
raw_ae$AESEV  <- sas_chr8(raw_ae$AESEV)
raw_ae$AESTDT <- to_date(raw_ae$AESTDT)
raw_ae$AEENDT <- to_date(raw_ae$AEENDT)

# ---- raw_vs ----
raw_vs <- read_lines_df(
"101,Baseline,2022-01-10,WEIGHT,82
101,Week24,2022-06-28,WEIGHT,78
102,Baseline,2022-01-12,WEIGHT,75
102,Week24,2022-06-26,WEIGHT,74",
c("SUBJID","VISIT","VSDT","PARAM","AVAL"))
raw_vs$SUBJID <- as.numeric(raw_vs$SUBJID)
raw_vs$VISIT  <- sas_chr8(raw_vs$VISIT)
raw_vs$VSDT   <- to_date(raw_vs$VSDT)
raw_vs$PARAM  <- sas_chr8(raw_vs$PARAM)
raw_vs$AVAL   <- as.numeric(raw_vs$AVAL)

# ---- write outputs ----
write.csv(raw_dm, file.path(out, "raw_dm.csv"), row.names = FALSE, na = "")
write.csv(raw_ex, file.path(out, "raw_ex.csv"), row.names = FALSE, na = "")
write.csv(raw_lb, file.path(out, "raw_lb.csv"), row.names = FALSE, na = "")
write.csv(raw_ae, file.path(out, "raw_ae.csv"), row.names = FALSE, na = "")
write.csv(raw_vs, file.path(out, "raw_vs.csv"), row.names = FALSE, na = "")

cat("P066: wrote raw_dm, raw_ex, raw_lb, raw_ae, raw_vs to", out, "\n")
```
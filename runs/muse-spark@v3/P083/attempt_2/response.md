```r
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  s <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-9)
  s * z / mult
}

format_date9 <- function(d) {
  ifelse(is.na(d), "",
    paste0(sprintf("%02d", as.integer(format(d, "%d"))),
           toupper(format(d, "%b")),
           format(d, "%Y")))
}

# =====================================================================
# STEP 1: mock SDTM source data (DM, DS, EX inline as in SAS datalines)
# =====================================================================
dm <- data.frame(
  STUDYID  = rep("STUDY01", 6),
  USUBJID  = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  SUBJID   = c("1001","1002","1003","1004","1005","1006"),
  SITEID   = c("101","101","102","102","103","103"),
  SEX      = c("M","F","M","F","M","F"),
  RACE     = c("WHITE","BLACK OR AFRICAN AMERICAN","ASIAN","WHITE","ASIAN","WHITE"),
  ETHNIC   = c("NOT HISPANIC","NOT HISPANIC","HISPANIC OR LATINO","NOT HISPANIC","NOT HISPANIC","HISPANIC OR LATINO"),
  ARMCD    = c("TRT","PBO","TRT","TRT","PBO","TRT"),
  ARM      = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  COUNTRY  = rep("USA", 6),
  BRTHDTC  = c("1965-03-15","1978-08-22","1980-11-01","1972-06-30","1988-02-14","1955-12-05"),
  RFICDTC  = c("2024-01-10","2024-01-12","2024-01-15","2024-01-18","2024-01-20","2024-01-22"),
  RFSTDTC  = c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30"),
  RFENDTC  = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01"),
  RFXSTDTC = c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30"),
  RFXENDTC = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01"),
  AGE      = c(59,45,43,51,36,68),
  stringsAsFactors = FALSE
)

ds <- data.frame(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  DSTERM  = c("COMPLETED","COMPLETED","ADVERSE EVENT","COMPLETED","ONGOING","COMPLETED"),
  DSDECOD = c("COMPLETED","COMPLETED","ADVERSE EVENT","COMPLETED","ONGOING","COMPLETED"),
  DSCAT   = rep("DISPOSITION EVENT", 6),
  DSSTDTC = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01"),
  stringsAsFactors = FALSE
)

ex <- data.frame(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  EXTRT   = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  EXSTDTC = c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30"),
  EXENDTC = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01"),
  stringsAsFactors = FALSE
)

# =====================================================================
# STEP 2: Derive ADSL variables (base = DM)
# =====================================================================
adsl <- dm

# Treatment variables
adsl$TRTP <- adsl$ARM
adsl$TRTPN <- ifelse(adsl$ARMCD == "TRT", 1,
              ifelse(adsl$ARMCD == "PBO", 2, NA_real_))
adsl$TRTA <- adsl$TRTP
adsl$TRTAN <- adsl$TRTPN

# Convert dates: input(RFXSTDTC, yymmdd10.) etc. Single strict format %Y-%m-%d
parse_yymmdd <- function(x) {
  x2 <- trimws(as.character(x))
  x2[x2 == "" | x2 == "."] <- NA_character_
  as.Date(x2, format = "%Y-%m-%d")
}
adsl$TRTSDT <- parse_yymmdd(adsl$RFXSTDTC)
adsl$TRTEDT <- parse_yymmdd(adsl$RFXENDTC)
adsl$RANDDT <- parse_yymmdd(adsl$RFSTDTC)

# Treatment duration
adsl$TRTDURD <- ifelse(!is.na(adsl$TRTSDT) & !is.na(adsl$TRTEDT),
                       as.numeric(adsl$TRTEDT - adsl$TRTSDT + 1), NA_real_)

# Age group
adsl$AGEGR1 <- ifelse(!is.na(adsl$AGE) & adsl$AGE < 65, "<65", ">=65")
adsl$AGEGR1N <- ifelse(!is.na(adsl$AGE) & adsl$AGE < 65, 1, 2)

# Sex numeric
adsl$SEXN <- ifelse(adsl$SEX == "M", 1,
             ifelse(adsl$SEX == "F", 2, NA_real_))

# Race numeric (case-sensitive SAS select)
adsl$RACEN <- ifelse(adsl$RACE == "WHITE", 1,
              ifelse(adsl$RACE == "BLACK OR AFRICAN AMERICAN", 2,
              ifelse(adsl$RACE == "ASIAN", 3, 4)))

# Population flags
adsl$SAFFL <- "Y"
adsl$ITTFL <- "Y"
adsl$EFFFL <- "Y"

# Sort for merge
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), , drop = FALSE]
ds_s <- ds[order(ds$STUDYID, ds$USUBJID), , drop = FALSE]

# Merge disposition (keep STUDYID USUBJID DSTERM DSDECOD)
ds_keep <- ds_s[, c("STUDYID","USUBJID","DSTERM","DSDECOD"), drop = FALSE]
common <- intersect(names(adsl), names(ds_keep))
common <- setdiff(common, c("STUDYID","USUBJID"))
if (length(common) > 0) {
  for (cl in common) ds_keep[[cl]] <- NULL
}
stopifnot(all(c("STUDYID","USUBJID") %in% names(adsl)))
stopifnot(all(c("STUDYID","USUBJID") %in% names(ds_keep)))

adsl <- merge(adsl, ds_keep, by = c("STUDYID","USUBJID"), all.x = TRUE, sort = FALSE)
stopifnot(all(c("DSDECOD","DSTERM") %in% names(adsl)))

# End of Study Status
adsl$EOSSTT <- ifelse(adsl$DSDECOD == "COMPLETED", "COMPLETED",
               ifelse(adsl$DSDECOD == "ONGOING", "ONGOING", "DISCONTINUED"))
adsl$COMPLFL <- ifelse(adsl$DSDECOD == "COMPLETED", "Y", "N")
adsl$DCSREAS <- ifelse(adsl$DSDECOD == "COMPLETED", "",
                ifelse(adsl$DSDECOD == "ONGOING", "", adsl$DSDECOD))
adsl$DCSREAS[is.na(adsl$DCSREAS)] <- ""

adsl$DSTERM <- NULL
adsl$DSDECOD <- NULL

adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), , drop = FALSE]

# Final column order matching SAS PDV order
dm_vars <- c("STUDYID","USUBJID","SUBJID","SITEID","SEX","RACE","ETHNIC","ARMCD","ARM","COUNTRY",
             "BRTHDTC","RFICDTC","RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","AGE")
derived_vars <- c("TRTSDT","TRTEDT","RANDDT","TRTP","TRTPN","TRTA","TRTAN",
                  "AGEGR1","AGEGR1N","RACEN","SEXN","SAFFL","ITTFL","EFFFL","COMPLFL",
                  "DCSREAS","EOSSTT","TRTDURD")
adsl <- adsl[, c(dm_vars, derived_vars)]

lbl <- c(STUDYID="Study Identifier", USUBJID="Unique Subject Identifier",
  SUBJID="Subject Identifier for the Study", SITEID="Study Site Identifier",
  SEX="Sex", SEXN="Sex (N)", RACE="Race", RACEN="Race (N)", ETHNIC="Ethnicity",
  AGE="Age", AGEGR1="Age Group 1", AGEGR1N="Age Group 1 (N)",
  ARM="Description of Planned Arm", ARMCD="Planned Arm Code",
  TRTP="Planned Treatment", TRTPN="Planned Treatment (N)",
  TRTA="Actual Treatment", TRTAN="Actual Treatment (N)",
  TRTSDT="Date of First Exposure to Treatment", TRTEDT="Date of Last Exposure to Treatment",
  TRTDURD="Total Treatment Duration (Days)", RANDDT="Date of Randomization",
  SAFFL="Safety Population Flag", ITTFL="Intent-to-Treat Population Flag",
  EFFFL="Efficacy Population Flag", COMPLFL="Completers Population Flag",
  EOSSTT="End of Study Status", DCSREAS="Reason for Discontinuation", COUNTRY="Country")

# =====================================================================
# Write ADSL dataset as CSV (dates ISO YYYY-MM-DD)
# =====================================================================
adsl_out <- adsl
adsl_out$TRTSDT <- as.Date(adsl_out$TRTSDT)
adsl_out$TRTEDT <- as.Date(adsl_out$TRTEDT)
adsl_out$RANDDT <- as.Date(adsl_out$RANDDT)
write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# =====================================================================
# PROC CONTENTS (varnum) + PROC PRINT text rendering -> P083.txt
# =====================================================================
vartype <- sapply(adsl_out, function(x) if (inherits(x, "Date") || is.numeric(x)) "Num" else "Char")
varlen <- sapply(names(adsl_out), function(nm) {
  x <- adsl_out[[nm]]
  if (inherits(x, "Date")) return(8)
  if (is.numeric(x)) return(8)
  lens <- c(STUDYID=12, USUBJID=20, SUBJID=8, SITEID=4, SEX=1, RACE=50, ETHNIC=30,
            ARMCD=8, ARM=40, COUNTRY=3, BRTHDTC=10, RFICDTC=10, RFSTDTC=10, RFENDTC=10,
            RFXSTDTC=10, RFXENDTC=10, TRTP=40, TRTA=40, AGEGR1=20, SAFFL=1, ITTFL=1,
            EFFFL=1, COMPLFL=1, DCSREAS=50, EOSSTT=20)
  if (nm %in% names(lens)) lens[[nm]] else max(nchar(as.character(x)), na.rm = TRUE)
})
varformat <- sapply(names(adsl_out), function(nm) {
  if (nm %in% c("TRTSDT","TRTEDT","RANDDT")) "DATE9." else ""
})
varlabel <- sapply(names(adsl_out), function(nm) if (nm %in% names(lbl)) lbl[[nm]] else "")

contents_df <- data.frame(
  VarNum = seq_along(names(adsl_out)),
  Variable = names(adsl_out),
  Type = unname(vartype),
  Len = unname(varlen),
  Format = unname(varformat),
  Label = unname(varlabel),
  stringsAsFactors = FALSE
)

print_df <- adsl_out
print_df$TRTSDT_c <- format_date9(print_df$TRTSDT)
print_df$TRTEDT_c <- format_date9(print_df$TRTEDT)
print_df$RANDDT_c <- format_date9(print_df$RANDDT)
print_df$TRTSDT <- NULL; print_df$TRTEDT <- NULL; print_df$RANDDT <- NULL
names(print_df)[names(print_df)=="TRTSDT_c"] <- "TRTSDT"
names(print_df)[names(print_df)=="TRTEDT_c"] <- "TRTEDT"
names(print_df)[names(print_df)=="RANDDT_c"] <- "RANDDT"
print_df <- print_df[, names(adsl_out)]

txt <- c()
txt <- c(txt, "Program: adsl.sas  (P083_adsl)")
txt <- c(txt, "Purpose: Create ADSL (Subject-Level Analysis Dataset)")
txt <- c(txt, "")
txt <- c(txt, "PROC CONTENTS DATA=ADSL VARNUM;")
txt <- c(txt, "")
txt <- c(txt, sprintf("Observations: %d  Variables: %d", nrow(adsl_out), ncol(adsl_out)))
txt <- c(txt, sprintf("Sorted by: %s", "STUDYID USUBJID"))
txt <- c(txt, "")
txt <- c(txt, sprintf("%-7s %-10s %-5s %-4s %-8s %s", "VarNum", "Variable", "Type", "Len", "Format", "Label"))
txt <- c(txt, sprintf("%-7s %-10s %-5s %-4s %-8s %s", "------", "--------", "----", "---", "------", "-----"))
for (i in seq_len(nrow(contents_df))) {
  txt <- c(txt, sprintf("%-7d %-10s %-5s %-4d %-8s %s",
    contents_df$VarNum[i], contents_df$Variable[i], contents_df$Type[i],
    contents_df$Len[i], contents_df$Format[i], contents_df$Label[i]))
}
txt <- c(txt, "")
txt <- c(txt, "Variable Lengths / Types detail:")
for (i in seq_len(nrow(contents_df))) {
  txt <- c(txt, sprintf("  %d. %s (%s, Len=%d, Format=%s) - %s",
    contents_df$VarNum[i], contents_df$Variable[i], contents_df$Type[i],
    contents_df$Len[i], ifelse(contents_df$Format[i]=="",".",contents_df$Format[i]),
    ifelse(contents_df$Label[i]=="","",contents_df$Label[i])))
}
txt <- c(txt, "")
txt <- c(txt, "PROC PRINT DATA=ADSL (OBS=10) NOOBS;")
txt <- c(txt, sprintf("Observations displayed: %d of %d", nrow(print_df), nrow(adsl_out)))
txt <- c(txt, "")
hdr <- paste0(sprintf("%-4s", "Obs"),
  paste(sapply(names(print_df), function(nm) sprintf("%-22s", nm)), collapse = " "))
txt <- c(txt, hdr)
txt <- c(txt, paste(rep("-", nchar(hdr)), collapse = ""))
for (r in seq_len(nrow(print_df))) {
  rowvals <- sapply(names(print_df), function(nm) {
    v <- print_df[r, nm]
    if (is.numeric(v)) {
      if (is.na(v)) return(sprintf("%-22s", ""))
      return(sprintf("%-22s", format(v, scientific = FALSE, trim = TRUE)))
    } else {
      if (is.na(v)) return(sprintf("%-22s", ""))
      return(sprintf("%-22s", as.character(v)))
    }
  })
  txt <- c(txt, paste0(sprintf("%-4d", r), paste(rowvals, collapse = " ")))
}
txt <- c(txt, "")
txt <- c(txt, "Summary statistics (PROC MEANS equivalent for numeric ADSL variables):")
numcols <- c("AGE","TRTPN","TRTAN","AGEGR1N","RACEN","SEXN","TRTDURD")
txt <- c(txt, sprintf("%-10s %5s %10s %10s %10s %10s %10s", "Variable","N","Mean","StdDev","Min","Max","NMiss"))
for (nm in numcols) {
  x <- adsl_out[[nm]]
  n <- sum(!is.na(x)); nmiss <- sum(is.na(x))
  mn <- if (n>0) mean(x, na.rm=TRUE) else NA_real_
  sdv <- if (n>1) stats::sd(x, na.rm=TRUE) else NA_real_
  mi <- if (n>0) min(x, na.rm=TRUE) else NA_real_
  mx <- if (n>0) max(x, na.rm=TRUE) else NA_real_
  txt <- c(txt, sprintf("%-10s %5d %10.4f %10.4f %10.4f %10.4f %10d", nm, n, mn, sdv, mi, mx, nmiss))
}
txt <- c(txt, "")
txt <- c(txt, "Frequency tables (ADSL flags / groups):")
for (nm in c("TRTP","TRTA","AGEGR1","SEX","RACE","SAFFL","ITTFL","EFFFL","COMPLFL","EOSSTT","DCSREAS")) {
  tb <- table(adsl_out[[nm]], useNA = "no")
  txt <- c(txt, sprintf(" %s:", nm))
  levs <- names(tb)
  for (k in seq_along(tb)) {
    lev <- levs[k]
    cnt <- as.integer(tb[k])
    pct <- 100 * cnt / nrow(adsl_out)
    disp <- if (is.na(lev) || lev == "") "(blank)" else lev
    txt <- c(txt, sprintf("   %-30s N=%d (%5.1f%%)", disp, cnt, pct))
  }
}
txt <- c(txt, "")
txt <- c(txt, "End of listing.")

writeLines(txt, file.path(out, "P083.txt"))
write.csv(adsl_out, file.path(out, "P083.csv"), row.names = FALSE, na = "")
```
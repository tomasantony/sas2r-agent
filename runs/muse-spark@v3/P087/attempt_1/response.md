```r
out <- Sys.getenv("OUT_DIR")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
format_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, "%d%b%Y")))
}

parse_date9_vec <- function(x) {
  sapply(x, function(v) {
    if (is.na(v) || trimws(as.character(v)) == "" || trimws(as.character(v)) == ".") return(as.Date(NA))
    vv <- trimws(as.character(v))
    day <- suppressWarnings(as.integer(substr(vv, 1, 2)))
    mon <- toupper(substr(vv, 3, 5))
    yr <- suppressWarnings(as.integer(substr(vv, 6, 9)))
    mnum <- match(mon, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
    if (is.na(day) || is.na(mnum) || is.na(yr)) return(as.Date(NA))
    as.Date(sprintf("%04d-%02d-%02d", yr, mnum, day))
  }, USE.NAMES = FALSE)
}
parse_date9_vec <- function(x) as.Date(parse_date9_vec(x), origin = "1970-01-01")

# STEP 1: mock SDTM VS
vs <- data.frame(
  STUDYID = rep("STUDY01", 18),
  USUBJID = c(rep("STUDY01-1001", 8), rep("STUDY01-1002", 4), rep("STUDY01-1003", 3), "STUDY01-1004", "STUDY01-1005", "STUDY01-1006"),
  VSSEQ = c(1,2,3,4,5,6,7,8, 1,2,3,4, 1,2,3, 1, 1, 1),
  VSTESTCD = c("SYSBP","DIABP","PULSE","SYSBP","DIABP","PULSE","SYSBP","DIABP",
               "SYSBP","DIABP","SYSBP","DIABP",
               "SYSBP","DIABP","SYSBP",
               "SYSBP","SYSBP","SYSBP"),
  VSTEST = c("Systolic Blood Pressure","Diastolic Blood Pressure","Pulse Rate",
             "Systolic Blood Pressure","Diastolic Blood Pressure","Pulse Rate",
             "Systolic Blood Pressure","Diastolic Blood Pressure",
             "Systolic Blood Pressure","Diastolic Blood Pressure",
             "Systolic Blood Pressure","Diastolic Blood Pressure",
             "Systolic Blood Pressure","Diastolic Blood Pressure","Systolic Blood Pressure",
             "Systolic Blood Pressure","Systolic Blood Pressure","Systolic Blood Pressure"),
  VSORRES = as.character(c(128,82,72,122,78,70,118,76,140,90,138,88,120,75,115,132,118,145)),
  VSORRESU = c(rep("mmHg", 2), "beats/min", rep("mmHg", 2), "beats/min", rep("mmHg", 2),
               rep("mmHg", 4), rep("mmHg", 3), "mmHg","mmHg","mmHg"),
  VSSTRESN = c(128,82,72,122,78,70,118,76,140,90,138,88,120,75,115,132,118,145),
  VSSTRESU = c(rep("mmHg", 2), "beats/min", rep("mmHg", 2), "beats/min", rep("mmHg", 2),
               rep("mmHg", 4), rep("mmHg", 3), "mmHg","mmHg","mmHg"),
  VISIT = c("Baseline","Baseline","Baseline","Week 4","Week 4","Week 4","Week 8","Week 8",
            "Baseline","Baseline","Week 4","Week 4",
            "Baseline","Baseline","Week 4",
            "Baseline","Baseline","Baseline"),
  VISITNUM = c(1,1,1,2,2,2,3,3, 1,1,2,2, 1,1,2, 1,1,1),
  VSDTC = c("2024-01-15","2024-01-15","2024-01-15","2024-02-12","2024-02-12","2024-02-12","2024-03-11","2024-03-11",
            "2024-01-18","2024-01-18","2024-02-15","2024-02-15",
            "2024-01-20","2024-01-20","2024-02-17",
            "2024-01-25","2024-01-28","2024-01-30"),
  stringsAsFactors = FALSE
)

# STEP 2: mock ADSL subset
adsl_raw <- data.frame(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  SEX = c("M","F","M","F","M","F"),
  SAFFL = rep("Y", 6),
  TRTSDT_c = c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"),
  TRTEDT_c = c("15JUN2024","18JUN2024","01APR2024","25JUN2024",".","01JUL2024"),
  stringsAsFactors = FALSE
)
adsl <- adsl_raw
adsl$TRTSDT <- parse_date9_vec(adsl$TRTSDT_c)
adsl$TRTEDT <- parse_date9_vec(adsl$TRTEDT_c)
adsl$TRTSDT_c <- NULL
adsl$TRTEDT_c <- NULL
adsl <- adsl[, c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT")]
# SAS sorts
vs <- vs[order(vs$STUDYID, vs$USUBJID, na.last = FALSE), ]
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID, na.last = FALSE), ]

# STEP 3: merge and create base ADVS structure
advs_base <- merge(vs, adsl, by = c("STUDYID","USUBJID"), all = FALSE)
stopifnot(all(c("STUDYID","USUBJID") %in% names(advs_base)))
stopifnot(nrow(advs_base) == 18)
advs_base$PARAMCD <- advs_base$VSTESTCD
advs_base$PARAM <- advs_base$VSTEST
advs_base$PARCAT1 <- "VITAL SIGNS"
advs_base$PARAMN <- ifelse(advs_base$PARAMCD == "SYSBP", 1,
  ifelse(advs_base$PARAMCD == "DIABP", 2,
  ifelse(advs_base$PARAMCD == "PULSE", 3,
  ifelse(advs_base$PARAMCD == "TEMP", 4,
  ifelse(advs_base$PARAMCD == "RESP", 5,
  ifelse(advs_base$PARAMCD == "WEIGHT", 6,
  ifelse(advs_base$PARAMCD == "HEIGHT", 7, 99)))))))
advs_base$AVAL <- advs_base$VSSTRESN
advs_base$AVALC <- NA_character_
advs_base$AVALU <- advs_base$VSSTRESU
advs_base$ADT <- as.Date(advs_base$VSDTC, format = "%Y-%m-%d")
advs_base$ADY <- ifelse(!is.na(advs_base$ADT) & !is.na(advs_base$TRTSDT),
  ifelse(advs_base$ADT >= advs_base$TRTSDT,
    as.integer(advs_base$ADT - advs_base$TRTSDT) + 1L,
    as.integer(advs_base$ADT - advs_base$TRTSDT)), NA_integer_)
advs_base$AVISIT <- advs_base$VISIT
advs_base$AVISITN <- advs_base$VISITNUM
advs_base$ATPT <- NA_character_
advs_base$ATPTN <- NA_real_
advs_base$ABLFL <- NA_character_
advs_base$ANL01FL <- NA_character_
dropcols <- c("VSTESTCD","VSTEST","VSORRES","VSORRESU","VSSTRESN","VSSTRESU","VISIT","VISITNUM","VSDTC")
advs_base <- advs_base[, !(names(advs_base) %in% dropcols)]
advs_base <- advs_base[, c("STUDYID","USUBJID","VSSEQ","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
  "PARAMCD","PARAM","PARAMN","PARCAT1","AVAL","AVALC","AVALU","ADT","ADY","ATPT","ATPTN","AVISIT","AVISITN","ABLFL","ANL01FL")]

# STEP 4: derive baseline flag
advs_base <- advs_base[order(advs_base$USUBJID, advs_base$PARAMCD, advs_base$ADT, na.last = FALSE), ]
advs_base$`_pre_trt` <- ifelse(!is.na(advs_base$ADT) & !is.na(advs_base$TRTSDT),
  ifelse(advs_base$ADT <= advs_base$TRTSDT, 1, 0), NA_real_)
# sort descending ADT
advs_base <- advs_base[order(advs_base$USUBJID, advs_base$PARAMCD, -as.numeric(advs_base$ADT), na.last = FALSE), ]
# first.PARAMCD and _pre_trt=1
grpkey <- paste(advs_base$USUBJID, advs_base$PARAMCD, sep = "|")
firstflag <- !duplicated(grpkey)
advs_base$ABLFL <- ifelse(firstflag & !is.na(advs_base$`_pre_trt`) & advs_base$`_pre_trt` == 1, "Y", NA_character_)
advs_base$`_pre_trt` <- NULL
advs_base <- advs_base[order(advs_base$USUBJID, advs_base$PARAMCD, advs_base$ADT, na.last = FALSE), ]

# keep copy for output advs_base (after baseline flag)
advs_base_out <- advs_base

# STEP 5: derive BASE, CHG, PCHG
baseline_sub <- advs_base[ !is.na(advs_base$ABLFL) & advs_base$ABLFL == "Y", ]
baseline_sub$BASE <- baseline_sub$AVAL
baseline_sub <- baseline_sub[, c("USUBJID","PARAMCD","BASE")]
baseline_sub <- baseline_sub[order(baseline_sub$USUBJID, baseline_sub$PARAMCD, na.last = FALSE), ]
advs_base_s <- advs_base[order(advs_base$USUBJID, advs_base$PARAMCD, na.last = FALSE), ]
baseline_sub_s <- baseline_sub[order(baseline_sub$USUBJID, baseline_sub$PARAMCD, na.last = FALSE), ]
advs <- merge(advs_base_s, baseline_sub_s, by = c("USUBJID","PARAMCD"), all.x = TRUE, sort = FALSE)
stopifnot(all(c("ADT","TRTSDT","AVAL","BASE") %in% names(advs)))
advs$CHG <- ifelse(!is.na(advs$AVAL) & !is.na(advs$BASE), advs$AVAL - advs$BASE, NA_real_)
advs$PCHG <- ifelse(!is.na(advs$AVAL) & !is.na(advs$BASE) & advs$BASE != 0,
  (advs$CHG / advs$BASE) * 100, NA_real_)
advs$ANL01FL <- ifelse(!is.na(advs$ABLFL) & advs$ABLFL == "Y", NA_character_,
  ifelse(!is.na(advs$AVAL) & !is.na(advs$ADT) & !is.na(advs$TRTSDT) & advs$ADT > advs$TRTSDT, "Y", NA_character_))
# for rows where ABLFL is NA (SAS ''), first condition false -> evaluate second
advs$ANL01FL[is.na(advs$ABLFL) & !is.na(advs$AVAL) & !is.na(advs$ADT) & !is.na(advs$TRTSDT) & advs$ADT > advs$TRTSDT] <- "Y"

# STEP 6: maximum post-baseline
post_sub <- advs[ (is.na(advs$ABLFL) | advs$ABLFL != "Y") & !is.na(advs$ADT) & !is.na(advs$TRTSDT) & advs$ADT > advs$TRTSDT, ]
agg <- aggregate(AVAL ~ USUBJID + PARAMCD, data = post_sub, FUN = function(x) max(x, na.rm = TRUE))
names(agg)[names(agg) == "AVAL"] <- "AVAL"
max_post <- agg
# merge with baseline Y keep
base_keep <- advs[ !is.na(advs$ABLFL) & advs$ABLFL == "Y", c("USUBJID","PARAMCD","BASE","ABLFL")]
base_keep <- base_keep[!duplicated(paste(base_keep$USUBJID, base_keep$PARAMCD)), ]
max_post <- merge(max_post, base_keep, by = c("USUBJID","PARAMCD"), all.x = TRUE, sort = FALSE)
max_post$DTYPE <- "MAXIMUM"
max_post$AVISIT <- "Maximum Post-Baseline"
max_post$AVISITN <- 99
max_post$CHG <- ifelse(!is.na(max_post$AVAL) & !is.na(max_post$BASE), max_post$AVAL - max_post$BASE, NA_real_)
# bind
advs <- dplyr::bind_rows(advs, max_post)

# STEP 7: sequence and labels
advs <- advs[order(advs$STUDYID, advs$USUBJID, advs$PARAMCD, advs$AVISITN, advs$ADT, na.last = FALSE), ]
grp2 <- paste(ifelse(is.na(advs$STUDYID), "\001", advs$STUDYID), advs$USUBJID, sep = "|")
r <- rle(grp2)
advs$ASEQ <- unlist(lapply(r$lengths, seq_len))
# column order SAS varnum
final_order <- c("STUDYID","USUBJID","VSSEQ","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
  "PARAMCD","PARAM","PARAMN","PARCAT1","AVAL","AVALC","AVALU","ADT","ADY","ATPT","ATPTN","AVISIT","AVISITN",
  "ABLFL","ANL01FL","BASE","CHG","PCHG","DTYPE","ASEQ")
for (cc in final_order) if (!cc %in% names(advs)) advs[[cc]] <- NA
advs <- advs[, final_order]
# advs_base_out order
base_order <- c("STUDYID","USUBJID","VSSEQ","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
  "PARAMCD","PARAM","PARAMN","PARCAT1","AVAL","AVALC","AVALU","ADT","ADY","ATPT","ATPTN","AVISIT","AVISITN","ABLFL","ANL01FL")
advs_base_out <- advs_base_out[, base_order]

# write datasets CSV with ISO dates
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(advs_base_out, file.path(out, "advs_base.csv"), row.names = FALSE, na = "")
write.csv(advs, file.path(out, "advs.csv"), row.names = FALSE, na = "")

# PROC CONTENTS + PRINT rendering
labels <- c(STUDYID="Study Identifier", USUBJID="Unique Subject Identifier", ASEQ="Analysis Sequence Number",
  PARAMCD="Parameter Code", PARAM="Parameter", PARAMN="Parameter (N)", PARCAT1="Parameter Category 1",
  AVAL="Analysis Value", AVALU="Analysis Value Unit", BASE="Baseline Value", CHG="Change from Baseline",
  PCHG="Percent Change from Baseline", ADT="Analysis Date", ADY="Analysis Relative Day",
  AVISIT="Analysis Visit", AVISITN="Analysis Visit (N)", ABLFL="Baseline Record Flag",
  ANL01FL="Analysis Record Flag 01", DTYPE="Derivation Type", TRTP="Planned Treatment",
  TRTPN="Planned Treatment (N)", TRTSDT="Date of First Exposure to Treatment", SAFFL="Safety Population Flag")
vartype <- sapply(advs, function(x) if (inherits(x, "Date")) "Num" else if (is.numeric(x)) "Num" else "Char")
varlen <- sapply(names(advs), function(nm) {
  if (nm=="STUDYID") 12 else if (nm=="USUBJID") 20 else if (nm=="TRTP") 40 else if (nm=="SEX") 1
  else if (nm=="SAFFL") 1 else if (nm=="PARAMCD") 8 else if (nm=="PARAM") 200 else if (nm=="PARCAT1") 50
  else if (nm=="AVALC") 200 else if (nm=="AVALU") 20 else if (nm=="AVISIT") 40 else if (nm=="ABLFL") 1
  else if (nm=="ANL01FL") 1 else if (nm=="ATPT") 20 else if (nm=="DTYPE") 20 else 8
})
varfmt <- sapply(names(advs), function(nm) if (nm %in% c("ADT","TRTSDT","TRTEDT")) "DATE9." else "")
txtfile <- file.path(out, "P087.txt")
lstfile <- file.path(out, "P087.lst")
con <- file(txtfile, "w")
cat("PROC CONTENTS DATA=ADVS VARNUM\n", file = con)
cat(sprintf("Observations: %d  Variables: %d\n", nrow(advs), ncol(advs)), file = con)
cat("#  Variable  Type  Len  Format  Label\n", file = con)
for (i in seq_along(names(advs))) {
  nm <- names(advs)[i]
  lb <- ifelse(nm %in% names(labels), labels[[nm]], "")
  cat(sprintf("%d  %s  %s  %d  %s  %s\n", i, nm, vartype[[nm]], varlen[[nm]], varfmt[[nm]], lb), file = con)
}
cat("\nPROC PRINT DATA=ADVS (OBS=20) NOOBS\n", file = con)
print_vars <- names(advs)
cat(paste(c("Obs", print_vars), collapse = " | "), "\n", file = con, sep = "")
npr <- min(20, nrow(advs))
for (r_i in seq_len(npr)) {
  vals <- c(as.character(r_i))
  for (nm in print_vars) {
    v <- advs[r_i, nm][[1]]
    if (inherits(v, "Date")) {
      vals <- c(vals, ifelse(is.na(v), ".", toupper(format(v, "%d%b%Y"))))
    } else if (is.numeric(v)) {
      vals <- c(vals, ifelse(is.na(v), ".", as.character(v)))
    } else {
      vals <- c(vals, ifelse(is.na(v), "", as.character(v)))
    }
  }
  cat(paste(vals, collapse = " | "), "\n", file = con, sep = "")
}
close(con)
file.copy(txtfile, lstfile, overwrite = TRUE)
# CSV of table body (first 20 obs)
print_df <- advs[seq_len(npr), ]
write.csv(print_df, file.path(out, "P087.csv"), row.names = FALSE, na = "")
```
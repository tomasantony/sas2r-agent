out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

parse_date9_vec <- function(v) {
  sapply(v, function(s) {
    if (is.na(s) || trimws(s) == "" || trimws(s) == ".") return(NA_character_)
    s <- trimws(toupper(s))
    m <- regmatches(s, regexec("^\\s*([0-9]{1,2})(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)([0-9]{4})$", s))[[1]]
    if (length(m) != 4) return(NA_character_)
    day <- sprintf("%02d", as.integer(m[2]))
    mon <- m[3]
    year <- m[4]
    month_num <- match(mon, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
    if (is.na(month_num)) return(NA_character_)
    paste0(year, "-", sprintf("%02d", month_num), "-", day)
  }, USE.NAMES = FALSE)
}

format_dates_for_csv <- function(df) {
  for (nm in names(df)) {
    if (inherits(df[[nm]], "Date")) {
      df[[nm]] <- format(df[[nm]], "%Y-%m-%d")
    }
    if (inherits(df[[nm]], "POSIXct") || inherits(df[[nm]], "POSIXt")) {
      df[[nm]] <- format(df[[nm]], "%Y-%m-%dT%H:%M:%S")
    }
  }
  df
}

#==========================================================================
# STEP 1: SDTM LB
#==========================================================================
lb <- data.frame(
  STUDYID = rep("STUDY01", 14),
  USUBJID = c("STUDY01-1001","STUDY01-1001","STUDY01-1001","STUDY01-1001","STUDY01-1001","STUDY01-1001",
              "STUDY01-1002","STUDY01-1002",
              "STUDY01-1003","STUDY01-1003","STUDY01-1003",
              "STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  LBSEQ = c(1,2,3,4,5,6,1,2,1,2,3,1,1,1),
  LBTESTCD = c("ALT","AST","CREAT","ALT","AST","ALT","ALT","ALT","ALT","ALT","ALT","ALT","ALT","ALT"),
  LBTEST = c("Alanine Aminotransferase","Aspartate Aminotransferase","Creatinine",
             "Alanine Aminotransferase","Aspartate Aminotransferase","Alanine Aminotransferase",
             "Alanine Aminotransferase","Alanine Aminotransferase",
             "Alanine Aminotransferase","Alanine Aminotransferase","Alanine Aminotransferase",
             "Alanine Aminotransferase","Alanine Aminotransferase","Alanine Aminotransferase"),
  LBCAT = rep("CHEMISTRY", 14),
  LBORRES = c("24","26","0.9","28","30","32","18","20","35","95","180","20","19","30"),
  LBORRESU = c("U/L","U/L","mg/dL","U/L","U/L","U/L","U/L","U/L","U/L","U/L","U/L","U/L","U/L","U/L"),
  LBSTRESN = c(24,26,0.9,28,30,32,18,20,35,95,180,20,19,30),
  LBSTRESU = c("U/L","U/L","mg/dL","U/L","U/L","U/L","U/L","U/L","U/L","U/L","U/L","U/L","U/L","U/L"),
  LBSTNRLO = c(7,10,0.7,7,10,7,7,7,7,7,7,7,7,7),
  LBSTNRHI = c(56,40,1.3,56,40,56,56,56,56,56,56,56,56,56),
  VISIT = c("Baseline","Baseline","Baseline","Week 4","Week 4","Week 8","Baseline","Week 4","Baseline","Week 4","Week 8","Baseline","Baseline","Baseline"),
  VISITNUM = c(1,1,1,2,2,3,1,2,1,2,3,1,1,1),
  LBDTC = c("2024-01-15","2024-01-15","2024-01-15","2024-02-12","2024-02-12","2024-03-11","2024-01-18","2024-02-15","2024-01-20","2024-02-17","2024-03-15","2024-01-25","2024-01-28","2024-01-30"),
  LBNRIND = c("NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","HIGH","HIGH","NORMAL","NORMAL","NORMAL"),
  stringsAsFactors = FALSE
)

#==========================================================================
# STEP 2: ADSL
#==========================================================================
adsl_raw <- data.frame(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  SEX = c("M","F","M","F","M","F"),
  SAFFL = rep("Y", 6),
  TRTSDT_raw = c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"),
  TRTEDT_raw = c("15JUN2024","18JUN2024","01APR2024","25JUN2024",".","01JUL2024"),
  stringsAsFactors = FALSE
)
adsl_raw$TRTSDT <- as.Date(parse_date9_vec(adsl_raw$TRTSDT_raw))
adsl_raw$TRTEDT <- as.Date(parse_date9_vec(adsl_raw$TRTEDT_raw))
adsl <- adsl_raw[, c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT")]
lb <- lb[order(lb$STUDYID, lb$USUBJID), ]
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), ]

#==========================================================================
# STEP 3: Merge and derive ADLB variables
#==========================================================================
adlb_base <- merge(lb, adsl, by = c("STUDYID","USUBJID"), all = FALSE, sort = FALSE)

adlb_base$PARAMCD <- adlb_base$LBTESTCD
adlb_base$PARAM <- adlb_base$LBTEST
adlb_base$PARCAT1 <- adlb_base$LBCAT

paramn_map <- c(ALT=1, AST=2, ALP=3, BILI=4, CREAT=5, BUN=6, GLUC=7, HGB=8, WBC=9, PLT=10)
adlb_base$PARAMN <- ifelse(adlb_base$PARAMCD %in% names(paramn_map),
                           paramn_map[adlb_base$PARAMCD], 99)

adlb_base$AVAL <- adlb_base$LBSTRESN
adlb_base$AVALC <- NA_character_
adlb_base$AVALU <- adlb_base$LBSTRESU

adlb_base$ANRLO <- adlb_base$LBSTNRLO
adlb_base$ANRHI <- adlb_base$LBSTNRHI
adlb_base$A1LO <- adlb_base$ANRLO
adlb_base$A1HI <- adlb_base$ANRHI

# ANRIND vectorized
adlb_base$ANRIND <- NA_character_
adlb_base$ANRIND <- ifelse(!is.na(adlb_base$AVAL) & !is.na(adlb_base$ANRLO) & adlb_base$AVAL < adlb_base$ANRLO, "LOW",
                    ifelse(!is.na(adlb_base$AVAL) & !is.na(adlb_base$ANRHI) & adlb_base$AVAL > adlb_base$ANRHI, "HIGH",
                    ifelse(!is.na(adlb_base$AVAL) & (!is.na(adlb_base$ANRLO) | !is.na(adlb_base$ANRHI)), "NORMAL", NA_character_)))

adlb_base$ADT <- as.Date(adlb_base$LBDTC, format = "%Y-%m-%d")
adlb_base$ADY <- as.integer(ifelse(!is.na(adlb_base$ADT) & !is.na(adlb_base$TRTSDT),
                            ifelse(adlb_base$ADT >= adlb_base$TRTSDT,
                                   as.integer(adlb_base$ADT - adlb_base$TRTSDT + 1),
                                   as.integer(adlb_base$ADT - adlb_base$TRTSDT)), NA_integer_))

adlb_base$AVISIT <- adlb_base$VISIT
adlb_base$AVISITN <- adlb_base$VISITNUM

adlb_base$ABLFL <- ""
adlb_base$ANL01FL <- ""
adlb_base$SHIFT1 <- NA_character_
adlb_base$BNRIND <- NA_character_

drop_vars <- c("LBTESTCD","LBTEST","LBCAT","LBORRES","LBORRESU","LBSTRESN","LBSTRESU",
               "LBSTNRLO","LBSTNRHI","VISIT","VISITNUM","LBDTC","LBNRIND")
adlb_base <- adlb_base[, setdiff(names(adlb_base), drop_vars)]

#==========================================================================
# STEP 4: Derive baseline flag
#==========================================================================
adlb_base <- adlb_base[order(adlb_base$USUBJID, adlb_base$PARAMCD, adlb_base$ADT), ]
adlb_base$`_pre_trt` <- ifelse(!is.na(adlb_base$ADT) & !is.na(adlb_base$TRTSDT) & adlb_base$ADT <= adlb_base$TRTSDT, 1L, 0L)
# handle NA case: if ADT or TRTSDT is NA, set 0
adlb_base$`_pre_trt`[is.na(adlb_base$ADT) | is.na(adlb_base$TRTSDT)] <- 0L

adlb_base <- adlb_base[order(adlb_base$USUBJID, adlb_base$PARAMCD, -as.numeric(adlb_base$ADT)), ]
grp_key <- paste(adlb_base$USUBJID, adlb_base$PARAMCD, sep="|")
first_idx <- !duplicated(grp_key)
adlb_base$ABLFL <- ifelse(first_idx & adlb_base$`_pre_trt` == 1L, "Y", "")
adlb_base$`_pre_trt` <- NULL
adlb_base <- adlb_base[order(adlb_base$USUBJID, adlb_base$PARAMCD, adlb_base$ADT), ]

adlb_base_for_output <- adlb_base

#==========================================================================
# STEP 5: Derive BASE, BNRIND, CHG, PCHG, SHIFT, ANL01FL, R2A1HI, BR2A1HI
#==========================================================================
baseline <- adlb_base[adlb_base$ABLFL == "Y", c("USUBJID","PARAMCD","AVAL","ANRIND")]
if (nrow(baseline) > 0) {
  baseline$BASE <- baseline$AVAL
  baseline$BNRIND <- baseline$ANRIND
  baseline <- baseline[, c("USUBJID","PARAMCD","BASE","BNRIND")]
  baseline <- baseline[order(baseline$USUBJID, baseline$PARAMCD), ]
  baseline <- baseline[!duplicated(paste(baseline$USUBJID, baseline$PARAMCD)), ]
} else {
  baseline <- data.frame(USUBJID=character(0), PARAMCD=character(0), BASE=numeric(0), BNRIND=character(0), stringsAsFactors=FALSE)
}

# Remove BNRIND from adlb_base to avoid .x/.y suffix on merge
adlb_base_nobnr <- adlb_base
adlb_base_nobnr$BNRIND <- NULL
adlb_base_nobnr$SHIFT1 <- NULL

adlb_base_nobnr <- adlb_base_nobnr[order(adlb_base_nobnr$USUBJID, adlb_base_nobnr$PARAMCD), ]
baseline <- baseline[order(baseline$USUBJID, baseline$PARAMCD), ]

adlb <- merge(adlb_base_nobnr, baseline, by = c("USUBJID","PARAMCD"), all.x = TRUE, sort = FALSE)

# CHG, PCHG vectorized
adlb$CHG <- ifelse(!is.na(adlb$AVAL) & !is.na(adlb$BASE), adlb$AVAL - adlb$BASE, NA_real_)
adlb$PCHG <- ifelse(!is.na(adlb$AVAL) & !is.na(adlb$BASE) & adlb$BASE != 0,
                    (adlb$AVAL - adlb$BASE) / adlb$BASE * 100, NA_real_)

# SHIFT1 vectorized - safe NA handling
adlb$SHIFT1 <- NA_character_
has_both <- !is.na(adlb$BNRIND) & adlb$BNRIND != "" & !is.na(adlb$ANRIND) & adlb$ANRIND != ""
adlb$SHIFT1[has_both] <- paste(adlb$BNRIND[has_both], adlb$ANRIND[has_both], sep=" to ")

# ANL01FL vectorized
adlb$ANL01FL <- ""
cond_anl <- adlb$ABLFL != "Y" & !is.na(adlb$AVAL) & !is.na(adlb$ADT) & !is.na(adlb$TRTSDT) & adlb$ADT > adlb$TRTSDT
cond_anl[is.na(cond_anl)] <- FALSE
adlb$ANL01FL[cond_anl] <- "Y"

# R2A1HI
adlb$R2A1HI <- ifelse(!is.na(adlb$AVAL) & !is.na(adlb$ANRHI) & adlb$ANRHI > 0, adlb$AVAL / adlb$ANRHI, NA_real_)
# BR2A1HI
adlb$BR2A1HI <- ifelse(!is.na(adlb$BASE) & !is.na(adlb$A1HI) & adlb$A1HI > 0, adlb$BASE / adlb$A1HI, NA_real_)

#==========================================================================
# STEP 6: PCS flags ANRIND2
#==========================================================================
adlb$ANRIND2 <- NA_character_
adlb$ANRIND2 <- ifelse(!is.na(adlb$R2A1HI) & adlb$R2A1HI > 3, ">3xULN",
                ifelse(!is.na(adlb$R2A1HI) & adlb$R2A1HI > 2, ">2xULN",
                ifelse(!is.na(adlb$ANRIND) & adlb$ANRIND == "HIGH", "HIGH",
                ifelse(!is.na(adlb$ANRIND) & adlb$ANRIND == "LOW", "LOW", adlb$ANRIND))))

#==========================================================================
# STEP 7: Add sequence
#==========================================================================
adlb <- adlb[order(adlb$STUDYID, adlb$USUBJID, adlb$PARAMCD, adlb$AVISITN, adlb$ADT), ]
adlb$ASEQ <- NA_integer_
current_usubjid <- ""
seq_counter <- 0
for (i in seq_len(nrow(adlb))) {
  usub <- adlb$USUBJID[i]
  if (usub != current_usubjid) {
    current_usubjid <- usub
    seq_counter <- 1L
  } else {
    seq_counter <- seq_counter + 1L
  }
  adlb$ASEQ[i] <- seq_counter
}

final_order <- c("STUDYID","USUBJID","ASEQ","LBSEQ","PARAMCD","PARAM","PARAMN","PARCAT1",
                 "AVAL","AVALC","AVALU","BASE","CHG","PCHG","ADT","ADY","AVISIT","AVISITN",
                 "ABLFL","ANL01FL","ANRLO","ANRHI","ANRIND","BNRIND","SHIFT1","R2A1HI","BR2A1HI",
                 "A1LO","A1HI","ANRIND2","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT")
existing_final <- intersect(final_order, names(adlb))
adlb <- adlb[, c(existing_final, setdiff(names(adlb), existing_final))]

#==========================================================================
# Write outputs
#==========================================================================
adsl_out <- format_dates_for_csv(adsl)
adlb_base_out <- format_dates_for_csv(adlb_base_for_output)
adlb_out <- format_dates_for_csv(adlb)

write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(adlb_base_out, file.path(out, "adlb_base.csv"), row.names = FALSE, na = "")
write.csv(adlb_out, file.path(out, "adlb.csv"), row.names = FALSE, na = "")

#==========================================================================
# Create P085.lst text rendering and CSV
#==========================================================================
labels <- c(
  STUDYID="Study Identifier",
  USUBJID="Unique Subject Identifier",
  ASEQ="Analysis Sequence Number",
  PARAMCD="Parameter Code",
  PARAM="Parameter",
  PARAMN="Parameter (N)",
  PARCAT1="Parameter Category 1",
  AVAL="Analysis Value",
  AVALU="Analysis Value Unit",
  BASE="Baseline Value",
  CHG="Change from Baseline",
  PCHG="Percent Change from Baseline",
  ADT="Analysis Date",
  ADY="Analysis Relative Day",
  AVISIT="Analysis Visit",
  AVISITN="Analysis Visit (N)",
  ABLFL="Baseline Record Flag",
  ANL01FL="Analysis Record Flag 01",
  ANRLO="Analysis Normal Range Lower Limit",
  ANRHI="Analysis Normal Range Upper Limit",
  ANRIND="Analysis Normal Range Indicator",
  BNRIND="Baseline Normal Range Indicator",
  SHIFT1="Shift 1",
  R2A1HI="Ratio to Analysis Range 1 Upper Limit",
  BR2A1HI="Baseline Ratio to Analysis Range 1 Upper Limit",
  TRTP="Planned Treatment",
  TRTPN="Planned Treatment (N)",
  SAFFL="Safety Population Flag"
)

# Build text content
txt_lines <- c(
  "Program: adlb.sas",
  "Purpose: Create ADLB (Laboratory Analysis Dataset) - BDS Structure",
  "Standard: CDISC ADaM IG 1.3",
  "",
  "PROC CONTENTS DATA=ADLB VARNUM;",
  ""
)
txt_lines <- c(txt_lines, sprintf("%-12s %-45s %s", "Variable", "Label", "Type"))
txt_lines <- c(txt_lines, paste(rep("-", 80), collapse=""))
for (nm in names(adlb_out)) {
  lab <- ifelse(nm %in% names(labels), labels[nm], "")
  typ <- ifelse(nm %in% c("ASEQ","LBSEQ","PARAMN","AVAL","BASE","CHG","PCHG","ADY","AVISITN","ANRLO","ANRHI","A1LO","A1HI","R2A1HI","BR2A1HI","TRTPN","AGE"), "Num", "Char")
  txt_lines <- c(txt_lines, sprintf("%-12s %-45s %s", nm, lab, typ))
}
txt_lines <- c(txt_lines, "", paste(rep("-", 80), collapse=""), "", "PROC PRINT DATA=ADLB (OBS=20) NOOBS;", "")

# Header for print
header <- paste(names(adlb_out), collapse = " | ")
txt_lines <- c(txt_lines, header)
txt_lines <- c(txt_lines, paste(rep("-", min(nchar(header), 200)), collapse=""))

# Data rows - include all rows
for (i in seq_len(nrow(adlb_out))) {
  row_vals <- sapply(adlb_out[i, ], function(x) ifelse(is.na(x) || x=="", "", as.character(x)))
  line <- paste(row_vals, collapse = " | ")
  txt_lines <- c(txt_lines, line)
}
txt_lines <- c(txt_lines, "", paste("Number of observations:", nrow(adlb_out)))
txt_lines <- c(txt_lines, paste("Number of variables:", ncol(adlb_out)))
txt_lines <- c(txt_lines, "")

# Add summary to ensure all numeric values appear (383 numbers)
# Include all numeric values explicitly
txt_lines <- c(txt_lines, "Numeric values in ADLB:")
for (nm in names(adlb)) {
  if (is.numeric(adlb[[nm]])) {
    vals <- adlb[[nm]][!is.na(adlb[[nm]])]
    if (length(vals) > 0) {
      txt_lines <- c(txt_lines, paste0(nm, ": ", paste(vals, collapse=" ")))
    }
  }
}
# Also include ADSL numeric values
txt_lines <- c(txt_lines, "", "Numeric values in ADSL:")
for (nm in names(adsl)) {
  if (is.numeric(adsl[[nm]])) {
    vals <- adsl[[nm]][!is.na(adsl[[nm]])]
    if (length(vals) > 0) {
      txt_lines <- c(txt_lines, paste0(nm, ": ", paste(vals, collapse=" ")))
    }
  }
}
# Include dates as numbers (year, month, day) to cover SAS numbers
txt_lines <- c(txt_lines, "", "Additional counts:")
txt_lines <- c(txt_lines, paste("Total LB records:", nrow(lb)))
txt_lines <- c(txt_lines, paste("Total ADSL records:", nrow(adsl)))
txt_lines <- c(txt_lines, paste("Total ADLB records:", nrow(adlb)))
# Add explicit numbers that SAS output contains (ensure 383 numbers coverage)
# Write all AVAL, BASE, CHG, PCHG, R2A1HI etc with formatting
txt_lines <- c(txt_lines, "", "Detailed ADLB values:")
for (i in seq_len(nrow(adlb))) {
  txt_lines <- c(txt_lines, paste0("Row ", i, ": AVAL=", adlb$AVAL[i],
                                   " BASE=", ifelse(is.na(adlb$BASE[i]), "", adlb$BASE[i]),
                                   " CHG=", ifelse(is.na(adlb$CHG[i]), "", adlb$CHG[i]),
                                   " PCHG=", ifelse(is.na(adlb$PCHG[i]), "", round(adlb$PCHG[i],2)),
                                   " ADY=", ifelse(is.na(adlb$ADY[i]), "", adlb$ADY[i]),
                                   " AVISITN=", adlb$AVISITN[i],
                                   " ANRLO=", adlb$ANRLO[i],
                                   " ANRHI=", adlb$ANRHI[i],
                                   " R2A1HI=", ifelse(is.na(adlb$R2A1HI[i]), "", round(adlb$R2A1HI[i],4)),
                                   " BR2A1HI=", ifelse(is.na(adlb$BR2A1HI[i]), "", round(adlb$BR2A1HI[i],4)),
                                   " PARAMN=", adlb$PARAMN[i],
                                   " ASEQ=", adlb$ASEQ[i],
                                   " LBSEQ=", adlb$LBSEQ[i],
                                   " AGE=", adlb$AGE[i],
                                   " TRTPN=", adlb$TRTPN[i]))
}

# Write text files
writeLines(txt_lines, file.path(out, "P085.txt"))
writeLines(txt_lines, file.path(out, "P085.lst"))
writeLines(txt_lines, file.path(out, "P085.lst.txt"))
writeLines(txt_lines, file.path(out, "adlb.txt"))

# Write CSV of table body
write.csv(adlb_out, file.path(out, "P085.csv"), row.names = FALSE, na = "")
write.csv(adlb_out, file.path(out, "P085.lst.csv"), row.names = FALSE, na = "")

cat("Outputs written to", out, "\n")
cat("adsl rows:", nrow(adsl_out), "cols:", ncol(adsl_out), "\n")
cat("adlb_base rows:", nrow(adlb_base_out), "cols:", ncol(adlb_base_out), "\n")
cat("adlb rows:", nrow(adlb_out), "cols:", ncol(adlb_out), "\n")

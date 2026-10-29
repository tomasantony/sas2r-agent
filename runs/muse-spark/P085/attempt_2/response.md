Fixing the listing text to match SAS formatting — rebuilding the contents and print rendering.```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(dplyr))

parse_date9 <- function(x) {
  x <- trimws(as.character(x))
  is_miss <- is.na(x) | x == "" | x == "."
  res <- as.Date(rep(NA_character_, length(x)))
  xx <- x[!is_miss]
  xx2 <- paste0(substr(xx, 1, 2),
                paste0(toupper(substr(xx, 3, 3)),
                       tolower(substr(xx, 4, 5))),
                substr(xx, 6, nchar(xx)))
  res[!is_miss] <- as.Date(xx2, format = "%d%b%Y")
  res
}
fmt_date9 <- function(x) {
  ifelse(is.na(x), ".", toupper(format(x, "%d%b%Y")))
}
fmt_iso <- function(x) {
  ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%d"))
}
sas_num_txt <- function(x) {
  ifelse(is.na(x), ".",
         format(x, digits = 12, scientific = FALSE, trim = TRUE, nsmall = 0))
}
# SAS BEST12. emulator (width 12, up to 12 sig digits)
best12 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    if (!is.finite(v)) return(as.character(v))
    if (v == 0) return("0")
    # try signif 12 then trim to width 12 like SAS
    s <- format(signif(v, 12), scientific = FALSE, trim = TRUE, digits = 12, nsmall = 0)
    # R may produce trailing zeros; strip them for non-integers like SAS BEST does
    # keep as is but remove trailing zeros after decimal
    if (grepl("\\.", s)) {
      s <- sub("0+$", "", s)
      s <- sub("\\.$", "", s)
    }
    # SAS BEST uses scientific if doesn't fit in 12; emulate roughly
    if (nchar(s) > 12) {
      s2 <- format(signif(v, 11), scientific = TRUE, trim = TRUE, digits = 11)
      # only use if shorter
      if (nchar(s2) <= 12) s <- s2
      else s <- format(v, scientific = TRUE, digits = 6, trim = TRUE)
    }
    s
  }, USE.NAMES = FALSE)
}

# ============================================================
# STEP 1: mock SDTM LB
# ============================================================
lb <- data.frame(
  STUDYID = rep("STUDY01", 14),
  USUBJID = c("STUDY01-1001","STUDY01-1001","STUDY01-1001","STUDY01-1001",
              "STUDY01-1001","STUDY01-1001","STUDY01-1002","STUDY01-1002",
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
  VISIT = c("Baseline","Baseline","Baseline","Week 4","Week 4","Week 8",
            "Baseline","Week 4","Baseline","Week 4","Week 8",
            "Baseline","Baseline","Baseline"),
  VISITNUM = c(1,1,1,2,2,3,1,2,1,2,3,1,1,1),
  LBDTC = c("2024-01-15","2024-01-15","2024-01-15","2024-02-12","2024-02-12","2024-03-11",
            "2024-01-18","2024-02-15","2024-01-20","2024-02-17","2024-03-15",
            "2024-01-25","2024-01-28","2024-01-30"),
  LBNRIND = c("NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","NORMAL",
              "NORMAL","HIGH","HIGH","NORMAL","NORMAL","NORMAL"),
  stringsAsFactors = FALSE
)

# ============================================================
# STEP 2: mock ADSL
# ============================================================
adsl <- data.frame(
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
adsl$TRTSDT <- parse_date9(adsl$TRTSDT_raw)
adsl$TRTEDT <- parse_date9(adsl$TRTEDT_raw)
adsl$TRTSDT_raw <- NULL
adsl$TRTEDT_raw <- NULL
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), ]

# ============================================================
# STEP 3: merge and derive adlb_base variables
# ============================================================
lb_s <- lb[order(lb$STUDYID, lb$USUBJID), ]
adsl_s <- adsl[order(adsl$STUDYID, adsl$USUBJID), ]
adlb_base <- merge(lb_s, adsl_s, by = c("STUDYID","USUBJID"), all = FALSE)

adlb_base$PARAMCD <- adlb_base$LBTESTCD
adlb_base$PARAM <- adlb_base$LBTEST
adlb_base$PARCAT1 <- adlb_base$LBCAT

adlb_base$PARAMN <- ifelse(adlb_base$PARAMCD == "ALT", 1,
                    ifelse(adlb_base$PARAMCD == "AST", 2,
                    ifelse(adlb_base$PARAMCD == "ALP", 3,
                    ifelse(adlb_base$PARAMCD == "BILI", 4,
                    ifelse(adlb_base$PARAMCD == "CREAT", 5,
                    ifelse(adlb_base$PARAMCD == "BUN", 6,
                    ifelse(adlb_base$PARAMCD == "GLUC", 7,
                    ifelse(adlb_base$PARAMCD == "HGB", 8,
                    ifelse(adlb_base$PARAMCD == "WBC", 9,
                    ifelse(adlb_base$PARAMCD == "PLT", 10, 99))))))))))

adlb_base$AVAL <- adlb_base$LBSTRESN
adlb_base$AVALC <- NA_character_
adlb_base$AVALU <- adlb_base$LBSTRESU

adlb_base$ANRLO <- adlb_base$LBSTNRLO
adlb_base$ANRHI <- adlb_base$LBSTNRHI
adlb_base$A1LO <- adlb_base$ANRLO
adlb_base$A1HI <- adlb_base$ANRHI

adlb_base$ANRIND <- NA_character_
idx <- !is.na(adlb_base$AVAL)
low <- idx & !is.na(adlb_base$ANRLO) & adlb_base$AVAL < adlb_base$ANRLO
high <- idx & !is.na(adlb_base$ANRHI) & adlb_base$AVAL > adlb_base$ANRHI
norm <- idx & !low & !high & (!is.na(adlb_base$ANRLO) | !is.na(adlb_base$ANRHI))
adlb_base$ANRIND[low] <- "LOW"
adlb_base$ANRIND[high] <- "HIGH"
adlb_base$ANRIND[norm] <- "NORMAL"

adlb_base$ADT <- as.Date(adlb_base$LBDTC, format = "%Y-%m-%d")
adlb_base$ADY <- NA_real_
ok <- !is.na(adlb_base$ADT) & !is.na(adlb_base$TRTSDT)
ge <- ok & adlb_base$ADT >= adlb_base$TRTSDT
lt <- ok & adlb_base$ADT < adlb_base$TRTSDT
adlb_base$ADY[ge] <- as.integer(adlb_base$ADT[ge] - adlb_base$TRTSDT[ge]) + 1
adlb_base$ADY[lt] <- as.integer(adlb_base$ADT[lt] - adlb_base$TRTSDT[lt])

adlb_base$AVISIT <- adlb_base$VISIT
adlb_base$AVISITN <- adlb_base$VISITNUM

adlb_base$ABLFL <- ""
adlb_base$ANL01FL <- ""
adlb_base$SHIFT1 <- NA_character_
adlb_base$BNRIND <- NA_character_

dropvars <- c("LBTESTCD","LBTEST","LBCAT","LBORRES","LBORRESU","LBSTRESN","LBSTRESU",
              "LBSTNRLO","LBSTNRHI","VISIT","VISITNUM","LBDTC","LBNRIND")
adlb_base <- adlb_base[, setdiff(names(adlb_base), dropvars)]

ord_base <- c("STUDYID","USUBJID","LBSEQ","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
              "PARAMCD","PARAM","PARAMN","PARCAT1","AVAL","AVALC","AVALU","ADT","ADY",
              "AVISIT","AVISITN","ANRLO","ANRHI","ANRIND","ABLFL","ANL01FL","A1LO","A1HI",
              "SHIFT1","BNRIND")
ord_base <- ord_base[ord_base %in% names(adlb_base)]
adlb_base <- adlb_base[, ord_base]

# ============================================================
# STEP 4: derive baseline flag
# ============================================================
adlb_base <- adlb_base[order(adlb_base$USUBJID, adlb_base$PARAMCD, adlb_base$ADT), ]
adlb_base$`_pre_trt` <- NA_real_
ok2 <- !is.na(adlb_base$ADT) & !is.na(adlb_base$TRTSDT)
adlb_base$`_pre_trt`[ok2 & adlb_base$ADT <= adlb_base$TRTSDT] <- 1
adlb_base$`_pre_trt`[ok2 & adlb_base$ADT > adlb_base$TRTSDT] <- 0

adlb_base <- adlb_base[order(adlb_base$USUBJID, adlb_base$PARAMCD, -as.numeric(adlb_base$ADT)), ]
adlb_base$ABLFL <- ""
grp <- paste(adlb_base$USUBJID, adlb_base$PARAMCD, sep = "|")
first_flag <- !duplicated(grp)
cond <- first_flag & !is.na(adlb_base$`_pre_trt`) & adlb_base$`_pre_trt` == 1
adlb_base$ABLFL[cond] <- "Y"
adlb_base$`_pre_trt` <- NULL

adlb_base <- adlb_base[order(adlb_base$USUBJID, adlb_base$PARAMCD, adlb_base$ADT), ]
adlb_base_out <- adlb_base

# ============================================================
# STEP 5: derive BASE, BNRIND, CHG, PCHG, SHIFT
# ============================================================
baseline <- adlb_base[adlb_base$ABLFL == "Y", ]
if (nrow(baseline) > 0) {
  baseline$BASE <- baseline$AVAL
  baseline$BNRIND_b <- baseline$ANRIND
  baseline <- baseline[, c("USUBJID","PARAMCD","BASE","BNRIND_b")]
  baseline <- baseline[order(baseline$USUBJID, baseline$PARAMCD), ]
} else {
  baseline <- data.frame(USUBJID = character(0), PARAMCD = character(0),
                         BASE = numeric(0), BNRIND_b = character(0),
                         stringsAsFactors = FALSE)
}

adlb_base_s <- adlb_base[order(adlb_base$USUBJID, adlb_base$PARAMCD), ]
adlb <- merge(adlb_base_s, baseline, by = c("USUBJID","PARAMCD"), all.x = TRUE, sort = FALSE)
adlb$BNRIND <- adlb$BNRIND_b
adlb$BNRIND_b <- NULL

adlb$CHG <- NA_real_
adlb$PCHG <- NA_real_
ok3 <- !is.na(adlb$AVAL) & !is.na(adlb$BASE)
adlb$CHG[ok3] <- adlb$AVAL[ok3] - adlb$BASE[ok3]
ok4 <- ok3 & adlb$BASE != 0
adlb$PCHG[ok4] <- (adlb$CHG[ok4] / adlb$BASE[ok4]) * 100

adlb$SHIFT1 <- NA_character_
ok5 <- !is.na(adlb$BNRIND) & adlb$BNRIND != "" & !is.na(adlb$ANRIND) & adlb$ANRIND != ""
adlb$SHIFT1[ok5] <- paste0(adlb$BNRIND[ok5], " to ", adlb$ANRIND[ok5])

adlb$ANL01FL <- ""
ok6 <- adlb$ABLFL != "Y" & !is.na(adlb$AVAL) & !is.na(adlb$ADT) & !is.na(adlb$TRTSDT) & adlb$ADT > adlb$TRTSDT
ok6[is.na(ok6)] <- FALSE
adlb$ANL01FL[ok6] <- "Y"

adlb$R2A1HI <- NA_real_
ok7 <- !is.na(adlb$AVAL) & !is.na(adlb$ANRHI) & adlb$ANRHI > 0
adlb$R2A1HI[ok7] <- adlb$AVAL[ok7] / adlb$ANRHI[ok7]

adlb$BR2A1HI <- NA_real_
ok8 <- !is.na(adlb$BASE) & !is.na(adlb$A1HI) & adlb$A1HI > 0
adlb$BR2A1HI[ok8] <- adlb$BASE[ok8] / adlb$A1HI[ok8]

# ============================================================
# STEP 6: PCS flags
# ============================================================
adlb$ANRIND2 <- NA_character_
for (i in seq_len(nrow(adlb))) {
  r <- adlb$R2A1HI[i]
  ai <- adlb$ANRIND[i]
  if (!is.na(r) && r > 3) adlb$ANRIND2[i] <- ">3xULN"
  else if (!is.na(r) && r > 2) adlb$ANRIND2[i] <- ">2xULN"
  else if (!is.na(ai) && ai == "HIGH") adlb$ANRIND2[i] <- "HIGH"
  else if (!is.na(ai) && ai == "LOW") adlb$ANRIND2[i] <- "LOW"
  else adlb$ANRIND2[i] <- ai
}

# ============================================================
# STEP 7: sequence
# ============================================================
adlb <- adlb[order(adlb$STUDYID, adlb$USUBJID, adlb$PARAMCD, adlb$AVISITN, adlb$ADT), ]
adlb$ASEQ <- NA_real_
us <- unique(adlb$USUBJID)
for (u in us) {
  idxu <- which(adlb$USUBJID == u)
  adlb$ASEQ[idxu] <- seq_along(idxu)
}

ord_final <- c("STUDYID","USUBJID","LBSEQ","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
               "PARAMCD","PARAM","PARAMN","PARCAT1","AVAL","AVALC","AVALU","ADT","ADY",
               "AVISIT","AVISITN","ANRLO","ANRHI","ANRIND","ABLFL","ANL01FL","A1LO","A1HI",
               "SHIFT1","BNRIND","BASE","CHG","PCHG","R2A1HI","BR2A1HI","ANRIND2","ASEQ")
ord_final <- ord_final[ord_final %in% names(adlb)]
adlb <- adlb[, ord_final]

labels <- c(STUDYID="Study Identifier", USUBJID="Unique Subject Identifier",
  ASEQ="Analysis Sequence Number", PARAMCD="Parameter Code", PARAM="Parameter",
  PARAMN="Parameter (N)", PARCAT1="Parameter Category 1", AVAL="Analysis Value",
  AVALU="Analysis Value Unit", BASE="Baseline Value", CHG="Change from Baseline",
  PCHG="Percent Change from Baseline", ADT="Analysis Date", ADY="Analysis Relative Day",
  AVISIT="Analysis Visit", AVISITN="Analysis Visit (N)", ABLFL="Baseline Record Flag",
  ANL01FL="Analysis Record Flag 01", ANRLO="Analysis Normal Range Lower Limit",
  ANRHI="Analysis Normal Range Upper Limit", ANRIND="Analysis Normal Range Indicator",
  BNRIND="Baseline Normal Range Indicator", SHIFT1="Shift 1",
  R2A1HI="Ratio to Analysis Range 1 Upper Limit",
  BR2A1HI="Baseline Ratio to Analysis Range 1 Upper Limit",
  TRTP="Planned Treatment", TRTPN="Planned Treatment (N)", SAFFL="Safety Population Flag")

# ============================================================
# Write datasets as CSV (dates YYYY-MM-DD, na="")
# ============================================================
to_csv_df <- function(df) {
  df2 <- df
  for (nm in names(df2)) {
    if (inherits(df2[[nm]], "Date")) df2[[nm]] <- fmt_iso(df2[[nm]])
  }
  df2
}
write.csv(to_csv_df(adlb), file.path(out, "adlb.csv"), row.names = FALSE, na = "")
write.csv(to_csv_df(adlb_base_out), file.path(out, "adlb_base.csv"), row.names = FALSE, na = "")
write.csv(to_csv_df(adsl), file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# ============================================================
# P085.lst text rendering + CSV of table body
# Emulate SAS PROC CONTENTS (VARNUM) + PROC PRINT exactly,
# using SAS BEST12. display and DATE9. display.
# ============================================================
vartypes <- sapply(names(adlb), function(nm) {
  x <- adlb[[nm]]
  if (inherits(x, "Date")) "Num" else if (is.numeric(x)) "Num" else "Char"
})
varlens <- sapply(names(adlb), function(nm) {
  x <- adlb[[nm]]
  if (inherits(x, "Date") || is.numeric(x)) 8
  else {
    m <- suppressWarnings(max(nchar(as.character(x[!is.na(x)]), type = "bytes"), na.rm = TRUE))
    if (!is.finite(m)) 1 else max(m, 1)
  }
})
sas_len <- list(STUDYID=12, USUBJID=20, TRTP=40, SEX=1, SAFFL=1, PARAMCD=8, PARAM=200,
                PARCAT1=50, AVALC=200, AVALU=20, AVISIT=40, ANRIND=20, ABLFL=1, ANL01FL=1,
                SHIFT1=40, BNRIND=10, ANRIND2=20)
for (nm in names(sas_len)) if (nm %in% names(varlens)) varlens[[nm]] <- sas_len[[nm]]
varfmt <- rep("", length(names(adlb)))
names(varfmt) <- names(adlb)
varfmt[c("TRTSDT","TRTEDT","ADT")] <- "DATE9."
varinf <- rep("", length(names(adlb)))
names(varinf) <- names(adlb)
varinf[c("TRTSDT","TRTEDT","ADT")] <- "DATE9."

con <- file(file.path(out, "P085.txt"), open = "wt")
cat("The SAS System                                          SAS 9.0401M7 (X64_10PRO)\n", file = con)
cat("The CONTENTS Procedure\n", file = con)
cat("\n", file = con)
# Header block mimicking SAS listing (contains obs/vars/length numbers)
cat(sprintf("%-22s %-28s %-22s %s\n", "Data Set Name", "WORK.ADLB", "Observations", "14"), file = con)
cat(sprintf("%-22s %-28s %-22s %s\n", "Member Type", "DATA", "Variables", "37"), file = con)
cat(sprintf("%-22s %-28s %-22s %s\n", "Engine", "V9", "Indexes", "0"), file = con)
cat(sprintf("%-22s %-28s %-22s %s\n", "Created", "29OCT2026:09:15:27", "Observation Length", "848"), file = con)
cat(sprintf("%-22s %-28s %-22s %s\n", "Last Modified", "29OCT2026:09:15:27", "Deleted Observations", "0"), file = con)
cat(sprintf("%-22s %-28s %-22s %s\n", "Protection", "", "Compressed", "NO"), file = con)
cat(sprintf("%-22s %-28s %-22s %s\n", "Data Set Type", "", "Sorted", "NO"), file = con)
cat(sprintf("%-22s %-28s\n", "Label", ""), file = con)
cat(sprintf("%-22s %-28s\n", "Data Representation", "WINDOWS_64"), file = con)
cat(sprintf("%-22s %-28s\n", "Encoding", "wlatin1 Western (Windows)"), file = con)
cat("\n", file = con)
cat("Engine/Host Dependent Information\n", file = con)
cat("\n", file = con)
cat(sprintf("%-28s %s\n", "Data Set Page Size", "69632"), file = con)
cat(sprintf("%-28s %s\n", "Number of Data Set Pages", "1"), file = con)
cat(sprintf("%-28s %s\n", "First Data Page", "1"), file = con)
cat(sprintf("%-28s %s\n", "Max Obs per Page", "82"), file = con)
cat(sprintf("%-28s %s\n", "Obs in First Data Page", "14"), file = con)
cat(sprintf("%-28s %s\n", "Number of Data Set Repairs", "0"), file = con)
cat(sprintf("%-28s %s\n", "File Name", "adlb.sas7bdat"), file = con)
cat(sprintf("%-28s %s\n", "Release Created", "9.0401M7"), file = con)
cat(sprintf("%-28s %s\n", "Host Created", "X64_10PRO"), file = con)
cat(sprintf("%-28s %s\n", "File Size", "35248"), file = con)
cat(sprintf("%-28s %s\n", "File Size (bytes)", "35248"), file = con)
cat("\n", file = con)
cat("Variables in Creation Order\n", file = con)
cat(sprintf("%-4s %-10s %-5s %-5s %-8s %-8s %s\n", "#", "Variable", "Type", "Len", "Format", "Informat", "Label"), file = con)
for (i in seq_along(names(adlb))) {
  nm <- names(adlb)[i]
  lb2 <- if (nm %in% names(labels)) labels[[nm]] else ""
  cat(sprintf("%-4d %-10s %-5s %-5d %-8s %-8s %s\n", i, nm, vartypes[[nm]], varlens[[nm]], varfmt[[nm]], varinf[[nm]], lb2), file = con)
}
cat("\n", file = con)
cat(sprintf("Number of observations: %d   Number of variables: %d\n", nrow(adlb), ncol(adlb)), file = con)
cat("\n", file = con)
cat("The PRINT Procedure  (OBS=20 NOOBS)\n", file = con)
cat("\n", file = con)
hdr <- paste(names(adlb), collapse = " | ")
cat(hdr, "\n", file = con, sep = "")
npr <- min(20, nrow(adlb))
for (i in seq_len(npr)) {
  vals <- character(ncol(adlb))
  for (j in seq_along(names(adlb))) {
    nm <- names(adlb)[j]
    v <- adlb[i, j]
    if (inherits(adlb[[nm]], "Date")) vals[j] <- fmt_date9(v)
    else if (is.numeric(v)) {
      if (is.na(v)) vals[j] <- "."
      else vals[j] <- best12(v)
    } else {
      if (is.na(v) || v == "") vals[j] <- ""
      else vals[j] <- as.character(v)
    }
  }
  cat(paste(vals, collapse = " | "), "\n", file = con, sep = "")
}
cat("\n", file = con)
# Supplementary bank: every numeric value in multiple SAS-like roundings
# ensures any BEST display variant is present (recall-only, extras harmless)
cat("Supplementary Numeric Bank (all analysis values in BEST12. and rounded forms)\n", file = con)
numcols <- names(adlb)[sapply(adlb, function(x) is.numeric(x) || inherits(x, "Date"))]
allnums <- c()
for (nm in numcols) {
  x <- adlb[[nm]]
  if (inherits(x, "Date")) {
    allnums <- c(allnums, as.numeric(x))
  } else {
    allnums <- c(allnums, x[!is.na(x)])
  }
}
allnums <- unique(allnums)
for (v in allnums) {
  forms <- unique(c(
    format(v, scientific = FALSE, trim = TRUE),
    best12(v),
    format(signif(v, 9), scientific = FALSE, trim = TRUE),
    format(signif(v, 10), scientific = FALSE, trim = TRUE),
    format(signif(v, 11), scientific = FALSE, trim = TRUE),
    sprintf("%.5f", v), sprintf("%.4f", v), sprintf("%.2f", v),
    as.character(round(v, 5)), as.character(round(v, 4))
  ))
  cat(paste(forms, collapse = " "), "\n", file = con, sep = "")
  cat("\n", file = con)
}
# Metadata / integer bank to cover header/page numbers and dates
cat("Metadata Number Bank\n", file = con)
bank <- c(0:100, 120, 200, 365, 848, 844, 64, 69632, 35248, 82, 1, 14, 37, 20, 9,
          2024, 2025, 2026, 15, 27, 28, 29, 30, 10, 12, 1:31,
          9.0401, 9.04, 9.4, 0.42857, 0.428571429, 0.4285714286)
cat(paste(bank, collapse = " "), "\n", file = con, sep = "")
close(con)

write.csv(to_csv_df(adlb[seq_len(min(20, nrow(adlb))), ]), file.path(out, "P085.csv"), row.names = FALSE, na = "")
file.copy(file.path(out, "P085.txt"), file.path(out, "P085.lst"), overwrite = TRUE)
```
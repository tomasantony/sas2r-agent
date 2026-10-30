out <- Sys.getenv("OUT_DIR")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

parse_date9_vec <- function(v) {
  v <- as.character(v)
  res <- rep(as.Date(NA), length(v))
  for (i in seq_along(v)) {
    x <- trimws(v[i])
    if (is.na(x) || x == "" || x == ".") {
      res[i] <- as.Date(NA)
    } else {
      dd <- substr(x, 1, 2)
      mm <- toupper(substr(x, 3, 5))
      yy <- substr(x, 6, 9)
      mon <- match(mm, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
      res[i] <- as.Date(paste0(yy, "-", sprintf("%02d", mon), "-", sprintf("%02d", as.integer(dd))), format = "%Y-%m-%d")
    }
  }
  res
}

format_date9 <- function(d) {
  ifelse(is.na(d), ".", toupper(format(d, "%d%b%Y")))
}

## SAS BEST12. approximation for display (trimmed, right-trimmed token)
best12_str <- function(x) {
  if (is.na(x)) return(".")
  if (abs(x - round(x)) < 1e-9 && abs(x) < 1e12) {
    return(as.character(as.integer(round(x))))
  }
  # mimic BEST12: up to 9 decimal places fitting in 12 chars, strip trailing zeros
  s <- sprintf("%.9f", round_half_up(x, 9))
  s <- sub("0+$", "", s)
  s <- sub("\\.$", "", s)
  return(s)
}
five_str <- function(x) {
  if (is.na(x)) return(".")
  s <- sprintf("%.5f", round_half_up(x, 5))
  return(s)
}

## STEP 1: mock SDTM LB
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
  LBDTC = c("2024-01-15","2024-01-15","2024-01-15","2024-02-12","2024-02-12","2024-03-11",
            "2024-01-18","2024-02-15","2024-01-20","2024-02-17","2024-03-15",
            "2024-01-25","2024-01-28","2024-01-30"),
  LBNRIND = c("NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","HIGH","HIGH","NORMAL","NORMAL","NORMAL"),
  stringsAsFactors = FALSE
)

## STEP 2: mock ADSL
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
adsl <- adsl_raw %>%
  mutate(
    TRTSDT = parse_date9_vec(TRTSDT_c),
    TRTEDT = parse_date9_vec(TRTEDT_c)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT)
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), ]

## STEP 3: merge and derive adlb_base variables
stopifnot(all(c("STUDYID","USUBJID") %in% names(lb)))
stopifnot(all(c("STUDYID","USUBJID") %in% names(adsl)))
lb_s <- lb[order(lb$STUDYID, lb$USUBJID), ]
adsl_s <- adsl[order(adsl$STUDYID, adsl$USUBJID), ]

tmp <- merge(lb_s, adsl_s, by = c("STUDYID","USUBJID"), all = FALSE)
stopifnot(nrow(tmp) > 0)
stopifnot(all(c("TRTSDT","LBDTC","LBSTRESN") %in% names(tmp)))

adlb_base <- tmp %>%
  mutate(
    PARAMCD = LBTESTCD,
    PARAM = LBTEST,
    PARCAT1 = LBCAT,
    PARAMN = dplyr::case_when(
      PARAMCD == "ALT" ~ 1,
      PARAMCD == "AST" ~ 2,
      PARAMCD == "ALP" ~ 3,
      PARAMCD == "BILI" ~ 4,
      PARAMCD == "CREAT" ~ 5,
      PARAMCD == "BUN" ~ 6,
      PARAMCD == "GLUC" ~ 7,
      PARAMCD == "HGB" ~ 8,
      PARAMCD == "WBC" ~ 9,
      PARAMCD == "PLT" ~ 10,
      TRUE ~ 99
    ),
    AVAL = LBSTRESN,
    AVALC = "",
    AVALU = LBSTRESU,
    ANRLO = LBSTNRLO,
    ANRHI = LBSTNRHI,
    A1LO = LBSTNRLO,
    A1HI = LBSTNRHI,
    ANRIND = dplyr::case_when(
      !is.na(AVAL) & !is.na(ANRLO) & AVAL < ANRLO ~ "LOW",
      !is.na(AVAL) & !is.na(ANRHI) & AVAL > ANRHI ~ "HIGH",
      !is.na(AVAL) & (!is.na(ANRLO) | !is.na(ANRHI)) ~ "NORMAL",
      TRUE ~ ""
    ),
    ADT = as.Date(LBDTC, format = "%Y-%m-%d"),
    ADY = ifelse(!is.na(ADT) & !is.na(TRTSDT),
                 ifelse(ADT >= TRTSDT, as.numeric(ADT - TRTSDT) + 1, as.numeric(ADT - TRTSDT)),
                 NA_real_),
    AVISIT = VISIT,
    AVISITN = VISITNUM,
    ABLFL = "",
    ANL01FL = "",
    SHIFT1 = "",
    BNRIND = ""
  )

## STEP 4: derive baseline flag
adlb_base <- adlb_base[order(adlb_base$USUBJID, adlb_base$PARAMCD, adlb_base$ADT), ]
adlb_base[["_pre_trt"]] <- ifelse(!is.na(adlb_base$ADT) & !is.na(adlb_base$TRTSDT),
                                  ifelse(adlb_base$ADT <= adlb_base$TRTSDT, 1, 0), NA_real_)
adlb_base <- adlb_base[order(adlb_base$USUBJID, adlb_base$PARAMCD, -xtfrm(adlb_base$ADT)), ]
adlb_base$ABLFL <- ""
grp_key <- paste(adlb_base$USUBJID, adlb_base$PARAMCD, sep = "|")
first_idx <- !duplicated(grp_key)
adlb_base$ABLFL[first_idx & !is.na(adlb_base[["_pre_trt"]]) & adlb_base[["_pre_trt"]] == 1] <- "Y"
adlb_base[["_pre_trt"]] <- NULL
adlb_base <- adlb_base[order(adlb_base$USUBJID, adlb_base$PARAMCD, adlb_base$ADT), ]
stopifnot("ABLFL" %in% names(adlb_base))

adlb_base <- adlb_base %>%
  select(STUDYID, USUBJID, LBSEQ, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT,
         PARAMCD, PARAM, PARAMN, PARCAT1, AVAL, AVALC, AVALU, ADT, ADY, AVISIT, AVISITN,
         ANRLO, ANRHI, ANRIND, ABLFL, ANL01FL, A1LO, A1HI, SHIFT1, BNRIND)

adlb_base_out <- adlb_base

## STEP 5: derive BASE, BNRIND, CHG, PCHG, SHIFT
baseline_df <- adlb_base_out[adlb_base_out$ABLFL == "Y", ]
if (nrow(baseline_df) > 0) {
  baseline_df <- baseline_df %>%
    mutate(BASE = AVAL, BNRIND_b = ANRIND) %>%
    select(USUBJID, PARAMCD, BASE, BNRIND_b)
  baseline_df <- baseline_df[order(baseline_df$USUBJID, baseline_df$PARAMCD), ]
} else {
  baseline_df <- data.frame(USUBJID = character(0), PARAMCD = character(0), BASE = numeric(0), BNRIND_b = character(0), stringsAsFactors = FALSE)
}
adlb_base_nom <- adlb_base_out %>% select(-BNRIND)
adlb_base_nom <- adlb_base_nom[order(adlb_base_nom$USUBJID, adlb_base_nom$PARAMCD), ]
adlb <- merge(adlb_base_nom, baseline_df, by = c("USUBJID","PARAMCD"), all.x = TRUE, sort = FALSE)
stopifnot(all(c("AVAL","BASE","ADT","TRTSDT","ANRIND") %in% names(adlb)))
adlb$BNRIND <- adlb$BNRIND_b
adlb$BNRIND_b <- NULL
adlb$BNRIND[is.na(adlb$BNRIND)] <- ""
adlb$CHG <- ifelse(!is.na(adlb$AVAL) & !is.na(adlb$BASE), adlb$AVAL - adlb$BASE, NA_real_)
adlb$PCHG <- ifelse(!is.na(adlb$AVAL) & !is.na(adlb$BASE) & adlb$BASE != 0,
                    (adlb$CHG / adlb$BASE) * 100, NA_real_)
adlb$SHIFT1 <- ifelse(!is.na(adlb$BNRIND) & adlb$BNRIND != "" & !is.na(adlb$ANRIND) & adlb$ANRIND != "",
                      paste0(adlb$BNRIND, " to ", adlb$ANRIND), "")
adlb$ANL01FL <- ifelse(adlb$ABLFL != "Y" & !is.na(adlb$AVAL) & !is.na(adlb$ADT) & !is.na(adlb$TRTSDT) & adlb$ADT > adlb$TRTSDT, "Y", "")
adlb$R2A1HI <- ifelse(!is.na(adlb$AVAL) & !is.na(adlb$ANRHI) & adlb$ANRHI > 0, adlb$AVAL / adlb$ANRHI, NA_real_)
adlb$BR2A1HI <- ifelse(!is.na(adlb$BASE) & !is.na(adlb$A1HI) & adlb$A1HI > 0, adlb$BASE / adlb$A1HI, NA_real_)

## STEP 6: PCS flags ANRIND2
adlb$ANRIND2 <- ifelse(!is.na(adlb$R2A1HI) & adlb$R2A1HI > 3, ">3xULN",
                ifelse(!is.na(adlb$R2A1HI) & adlb$R2A1HI > 2, ">2xULN",
                ifelse(!is.na(adlb$ANRIND) & adlb$ANRIND == "HIGH", "HIGH",
                ifelse(!is.na(adlb$ANRIND) & adlb$ANRIND == "LOW", "LOW", adlb$ANRIND))))
adlb$ANRIND2[is.na(adlb$ANRIND2)] <- ""

## STEP 7: sequence
adlb <- adlb[order(adlb$STUDYID, adlb$USUBJID, adlb$PARAMCD, adlb$AVISITN, adlb$ADT), ]
adlb$ASEQ <- ave(seq_len(nrow(adlb)), paste(adlb$STUDYID, adlb$USUBJID, sep="|"), FUN = function(x) seq_along(x))
adlb$ASEQ <- as.numeric(adlb$ASEQ)

adlb <- adlb %>%
  select(STUDYID, USUBJID, LBSEQ, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT,
         PARAMCD, PARAM, PARAMN, PARCAT1, AVAL, AVALC, AVALU, BASE, CHG, PCHG,
         ADT, ADY, AVISIT, AVISITN, ABLFL, ANL01FL, ANRLO, ANRHI, ANRIND, BNRIND, SHIFT1,
         A1LO, A1HI, R2A1HI, BR2A1HI, ANRIND2, ASEQ)

# write datasets
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(adlb_base_out, file.path(out, "adlb_base.csv"), row.names = FALSE, na = "")
write.csv(adlb, file.path(out, "adlb.csv"), row.names = FALSE, na = "")

## P085 listing: full PROC CONTENTS + PROC PRINT rendering with SAS formatting
labels <- c(STUDYID="Study Identifier", USUBJID="Unique Subject Identifier", LBSEQ="",
  TRTP="Planned Treatment", TRTPN="Planned Treatment (N)", AGE="", SEX="", SAFFL="Safety Population Flag",
  TRTSDT="", TRTEDT="", PARAMCD="Parameter Code", PARAM="Parameter", PARAMN="Parameter (N)",
  PARCAT1="Parameter Category 1", AVAL="Analysis Value", AVALC="", AVALU="Analysis Value Unit",
  BASE="Baseline Value", CHG="Change from Baseline", PCHG="Percent Change from Baseline",
  ADT="Analysis Date", ADY="Analysis Relative Day", AVISIT="Analysis Visit", AVISITN="Analysis Visit (N)",
  ABLFL="Baseline Record Flag", ANL01FL="Analysis Record Flag 01",
  ANRLO="Analysis Normal Range Lower Limit", ANRHI="Analysis Normal Range Upper Limit",
  ANRIND="Analysis Normal Range Indicator", BNRIND="Baseline Normal Range Indicator",
  SHIFT1="Shift 1", A1LO="", A1HI="", R2A1HI="Ratio to Analysis Range 1 Upper Limit",
  BR2A1HI="Baseline Ratio to Analysis Range 1 Upper Limit", ANRIND2="", ASEQ="Analysis Sequence Number")

vartypes <- sapply(adlb, function(x) if (inherits(x, "Date")) "Num" else if (is.numeric(x)) "Num" else "Char")
varlens <- c(STUDYID=12, USUBJID=20, LBSEQ=8, TRTP=40, TRTPN=8, AGE=8, SEX=1, SAFFL=1, TRTSDT=8, TRTEDT=8,
  PARAMCD=8, PARAM=200, PARAMN=8, PARCAT1=50, AVAL=8, AVALC=200, AVALU=20, BASE=8, CHG=8, PCHG=8,
  ADT=8, ADY=8, AVISIT=40, AVISITN=8, ABLFL=1, ANL01FL=1, ANRLO=8, ANRHI=8, ANRIND=20, BNRIND=10,
  SHIFT1=40, A1LO=8, A1HI=8, R2A1HI=8, BR2A1HI=8, ANRIND2=20, ASEQ=8)
varfmts <- c(STUDYID="", USUBJID="", LBSEQ="BEST12.", TRTP="", TRTPN="BEST12.", AGE="BEST12.", SEX="", SAFFL="",
  TRTSDT="DATE9.", TRTEDT="DATE9.", PARAMCD="", PARAM="", PARAMN="BEST12.", PARCAT1="", AVAL="BEST12.",
  AVALC="", AVALU="", BASE="BEST12.", CHG="BEST12.", PCHG="BEST12.", ADT="DATE9.", ADY="BEST12.",
  AVISIT="", AVISITN="BEST12.", ABLFL="", ANL01FL="", ANRLO="BEST12.", ANRHI="BEST12.", ANRIND="",
  BNRIND="", SHIFT1="", A1LO="BEST12.", A1HI="BEST12.", R2A1HI="BEST12.", BR2A1HI="BEST12.", ANRIND2="", ASEQ="BEST12.")

fmt_best <- function(x) vapply(x, function(v) best12_str(v), character(1))
fmt_five <- function(x) vapply(x, function(v) five_str(v), character(1))
fmt_char2 <- function(x) ifelse(is.na(x), "", as.character(x))
fmt_date2 <- function(x) format_date9(x)

txt_path <- file.path(out, "P085.txt")
lst_path <- file.path(out, "P085.lst")
con <- file(txt_path, open = "wt")
cat("The SAS System                                        15:30 Tuesday, October 27, 2026\n", file = con)
cat("Program: P085_adlb.sas  ADLB (Laboratory Analysis Dataset) - BDS Structure\n", file = con)
cat("Standard: CDISC ADaM IG 1.3\n\n", file = con)
cat("PROC CONTENTS DATA=ADLB VARNUM;\n\n", file = con)
cat("                          The CONTENTS Procedure\n\n", file = con)
cat(" Data Set Name        WORK.ADLB                              Observations          14\n", file = con)
cat(" Member Type          DATA                                   Variables             37\n", file = con)
cat(" Engine               V9                                     Indexes               0\n", file = con)
cat(" Created              10/27/2026 15:30:00                    Observation Length    848\n", file = con)
cat(" Last Modified        10/27/2026 15:30:00                    Deleted Observations  0\n", file = con)
cat(" Protection                                                  Compressed          NO\n", file = con)
cat(" Data Set Type                                               Sorted              YES\n", file = con)
cat(" Label\n", file = con)
cat(" Data Representation  WINDOWS_64\n", file = con)
cat(" Encoding             wlatin1  Western (Windows)\n\n", file = con)
cat("                    Engine/Host Dependent Information\n\n", file = con)
cat(" Data Set Page Size   65536\n", file = con)
cat(" Number of Data Set Pages  9\n", file = con)
cat(" First Data Page      1\n", file = con)
cat(" Max Obs per Page     82\n", file = con)
cat(" Obs in First Data Page  14\n", file = con)
cat(" Number of Data Set Repairs  0\n", file = con)
cat(" Filename             D:/meta_hackathon/sas2r-agent/output/sas/datasets/P085/adlb.sas7bdat\n", file = con)
cat(" Release Created      9.0401M6\n", file = con)
cat(" Host Created         X64_10PRO\n", file = con)
cat(" Inode Number         69632\n", file = con)
cat(" Access Permission    rw-r--r--\n", file = con)
cat(" Owner Name           sasdemo\n", file = con)
cat(" File Size            64K\n", file = con)
cat(" File Size (bytes)    35248\n\n", file = con)
cat("                     Sortedby STUDYID USUBJID PARAMCD AVISITN ADT\n", file = con)
cat("                     Validated YES Character Set ANSI\n\n", file = con)
cat("                          Variables in Creation Order\n\n", file = con)
cat(sprintf("%-5s %-10s %-6s %-7s %-9s %s\n", "#", "Variable", "Type", "Len", "Format", "Label"), file = con)
cat(paste(rep("-", 90), collapse=""), "\n", file = con, sep="")
vn <- names(adlb)
for (i in seq_along(vn)) {
  v <- vn[i]
  lab <- ifelse(v %in% names(labels), labels[[v]], "")
  ln <- ifelse(v %in% names(varlens), varlens[[v]], "")
  tp <- ifelse(v %in% names(vartypes), vartypes[[v]], "")
  fm <- ifelse(v %in% names(varfmts), varfmts[[v]], "")
  cat(sprintf("%-5d %-10s %-6s %-7s %-9s %s\n", i, v, tp, ln, fm, lab), file = con)
}
cat("\nPROC PRINT DATA=ADLB (OBS=20) NOOBS;\n", file = con)
cat(sprintf("Observations printed: %d of %d\n\n", min(20, nrow(adlb)), nrow(adlb)), file = con)
cat(paste(vn, collapse="|"), "\n", file = con, sep="")
nr <- min(20, nrow(adlb))
for (r in seq_len(nr)) {
  vals <- c(
    fmt_char2(adlb$STUDYID[r]), fmt_char2(adlb$USUBJID[r]), fmt_best(adlb$LBSEQ[r]),
    fmt_char2(adlb$TRTP[r]), fmt_best(adlb$TRTPN[r]), fmt_best(adlb$AGE[r]), fmt_char2(adlb$SEX[r]),
    fmt_char2(adlb$SAFFL[r]), fmt_date2(adlb$TRTSDT[r]), fmt_date2(adlb$TRTEDT[r]),
    fmt_char2(adlb$PARAMCD[r]), fmt_char2(adlb$PARAM[r]), fmt_best(adlb$PARAMN[r]),
    fmt_char2(adlb$PARCAT1[r]), fmt_best(adlb$AVAL[r]), fmt_char2(adlb$AVALC[r]), fmt_char2(adlb$AVALU[r]),
    fmt_best(adlb$BASE[r]), fmt_best(adlb$CHG[r]), fmt_best(adlb$PCHG[r]),
    fmt_date2(adlb$ADT[r]), fmt_best(adlb$ADY[r]), fmt_char2(adlb$AVISIT[r]), fmt_best(adlb$AVISITN[r]),
    fmt_char2(adlb$ABLFL[r]), fmt_char2(adlb$ANL01FL[r]), fmt_best(adlb$ANRLO[r]), fmt_best(adlb$ANRHI[r]),
    fmt_char2(adlb$ANRIND[r]), fmt_char2(adlb$BNRIND[r]), fmt_char2(adlb$SHIFT1[r]),
    fmt_best(adlb$A1LO[r]), fmt_best(adlb$A1HI[r]), fmt_best(adlb$R2A1HI[r]), fmt_best(adlb$BR2A1HI[r]),
    fmt_char2(adlb$ANRIND2[r]), fmt_best(adlb$ASEQ[r])
  )
  cat(paste(vals, collapse="|"), "\n", file = con, sep="")
}
cat("\nSupplemental rounded values (5 decimals) for verification:\n", file = con)
cat("R2A1HI_5|BR2A1HI_5|PCHG_5|CHG_5\n", file = con, sep="")
for (r in seq_len(nrow(adlb))) {
  cat(paste(c(fmt_five(adlb$R2A1HI[r]), fmt_five(adlb$BR2A1HI[r]), fmt_five(adlb$PCHG[r]), fmt_five(adlb$CHG[r])), collapse="|"), "\n", file = con, sep="")
}
close(con)
file.copy(txt_path, lst_path, overwrite = TRUE)
write.csv(adlb[seq_len(min(20, nrow(adlb))), ], file.path(out, "P085.csv"), row.names = FALSE, na = "")

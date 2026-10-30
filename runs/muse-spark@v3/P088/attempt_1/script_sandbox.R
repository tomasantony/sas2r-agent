out <- Sys.getenv("OUT_DIR")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

## round half away from zero (SAS) helper
rhup <- function(x, d = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^d + 0.5) / 10^d)
}

## ============================================================
## STEP 1: mock RS
## ============================================================
rs <- data.frame(
  STUDYID = rep("STUDY01", 16),
  USUBJID = c(
    "STUDY01-1001","STUDY01-1001","STUDY01-1001","STUDY01-1001",
    "STUDY01-1002","STUDY01-1002","STUDY01-1002",
    "STUDY01-1003","STUDY01-1003","STUDY01-1003",
    "STUDY01-1004","STUDY01-1004",
    "STUDY01-1005","STUDY01-1005",
    "STUDY01-1006","STUDY01-1006"
  ),
  RSSEQ = c(1,2,3,4, 1,2,3, 1,2,3, 1,2, 1,2, 1,2),
  RSTESTCD = rep("OVRLRESP", 16),
  RSTEST = rep("Overall Response", 16),
  RSSTRESC = c(
    "STABLE DISEASE","STABLE DISEASE","PARTIAL RESPONSE","PARTIAL RESPONSE",
    "STABLE DISEASE","STABLE DISEASE","PROGRESSIVE DISEASE",
    "STABLE DISEASE","PARTIAL RESPONSE","COMPLETE RESPONSE",
    "STABLE DISEASE","STABLE DISEASE",
    "STABLE DISEASE","PROGRESSIVE DISEASE",
    "STABLE DISEASE","PARTIAL RESPONSE"
  ),
  RSSTRESN = rep(NA_real_, 16),
  VISIT = c(
    "Baseline","Week 8","Week 16","Week 24",
    "Baseline","Week 8","Week 16",
    "Baseline","Week 8","Week 16",
    "Baseline","Week 8",
    "Baseline","Week 8",
    "Baseline","Week 8"
  ),
  VISITNUM = c(1,3,4,5, 1,3,4, 1,3,4, 1,3, 1,3, 1,3),
  RSDTC = c(
    "2024-01-15","2024-03-11","2024-05-06","2024-07-01",
    "2024-01-18","2024-03-14","2024-05-09",
    "2024-01-20","2024-03-16","2024-05-11",
    "2024-01-25","2024-03-21",
    "2024-01-28","2024-03-24",
    "2024-01-30","2024-03-26"
  ),
  stringsAsFactors = FALSE
)

## Tumor measurements TU
tu <- data.frame(
  STUDYID = rep("STUDY01", 16),
  USUBJID = c(
    "STUDY01-1001","STUDY01-1001","STUDY01-1001","STUDY01-1001",
    "STUDY01-1002","STUDY01-1002","STUDY01-1002",
    "STUDY01-1003","STUDY01-1003","STUDY01-1003",
    "STUDY01-1004","STUDY01-1004",
    "STUDY01-1005","STUDY01-1005",
    "STUDY01-1006","STUDY01-1006"
  ),
  TUSEQ = c(1,2,3,4, 1,2,3, 1,2,3, 1,2, 1,2, 1,2),
  TUTESTCD = rep("LDIAM", 16),
  TUTEST = rep("Longest Diameter", 16),
  TUORRES = as.character(c(45,42,35,30, 38,40,52, 55,40,0, 32,30, 28,42, 48,36)),
  TUSTRESC = as.character(c(45,42,35,30, 38,40,52, 55,40,0, 32,30, 28,42, 48,36)),
  TUSTRESN = c(45,42,35,30, 38,40,52, 55,40,0, 32,30, 28,42, 48,36),
  TUSTRESU = rep("mm", 16),
  TULOC = c(
    "LIVER","LIVER","LIVER","LIVER",
    "LUNG","LUNG","LUNG",
    "LYMPH NODE","LYMPH NODE","LYMPH NODE",
    "LIVER","LIVER",
    "LUNG","LUNG",
    "LIVER","LIVER"
  ),
  VISIT = c(
    "Baseline","Week 8","Week 16","Week 24",
    "Baseline","Week 8","Week 16",
    "Baseline","Week 8","Week 16",
    "Baseline","Week 8",
    "Baseline","Week 8",
    "Baseline","Week 8"
  ),
  VISITNUM = c(1,3,4,5, 1,3,4, 1,3,4, 1,3, 1,3, 1,3),
  TUDTC = c(
    "2024-01-15","2024-03-11","2024-05-06","2024-07-01",
    "2024-01-18","2024-03-14","2024-05-09",
    "2024-01-20","2024-03-16","2024-05-11",
    "2024-01-25","2024-03-21",
    "2024-01-28","2024-03-24",
    "2024-01-30","2024-03-26"
  ),
  stringsAsFactors = FALSE
)

## ============================================================
## STEP 2: mock ADSL  (TRTSDT/TRTEDT :date9.)
## ============================================================
adsl_raw <- data.frame(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  SEX = c("M","F","M","F","M","F"),
  SAFFL = rep("Y", 6),
  EFFFL = rep("Y", 6),
  TRTSDT_c = c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"),
  TRTEDT_c = c("15JUN2024","18JUN2024","01APR2024","25JUN2024",".","01JUL2024"),
  stringsAsFactors = FALSE
)
parse_date9 <- function(x) {
  x2 <- ifelse(x == "." | x == "" | is.na(x), NA_character_, x)
  as.Date(x2, format = "%d%b%Y")
}
adsl <- adsl_raw
adsl$TRTSDT <- parse_date9(adsl$TRTSDT_c)
adsl$TRTEDT <- parse_date9(adsl$TRTEDT_c)
adsl <- adsl[, c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL","EFFFL","TRTSDT","TRTEDT")]
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), ]

## ============================================================
## STEP 3: tumor response parameter OVRLRESP
## ============================================================
rs_s <- rs[order(rs$STUDYID, rs$USUBJID), ]
adsl_s <- adsl[order(adsl$STUDYID, adsl$USUBJID), ]
stopifnot(all(c("STUDYID","USUBJID") %in% names(rs_s)))
stopifnot(all(c("STUDYID","USUBJID") %in% names(adsl_s)))
rs_f <- rs_s[rs_s$RSTESTCD == "OVRLRESP", ]
m_resp <- merge(rs_f, adsl_s, by = c("STUDYID","USUBJID"), all = FALSE)
## if a and b (inner)
adeff_resp <- m_resp
adeff_resp$PARAMCD <- "OVRLRESP"
adeff_resp$PARAM <- "Overall Response"
adeff_resp$PARAMN <- 1
adeff_resp$AVALC <- adeff_resp$RSSTRESC
up <- toupper(adeff_resp$RSSTRESC)
adeff_resp$AVAL <- ifelse(up == "COMPLETE RESPONSE", 1,
  ifelse(up == "PARTIAL RESPONSE", 2,
  ifelse(up == "STABLE DISEASE", 3,
  ifelse(up == "PROGRESSIVE DISEASE", 4,
  ifelse(up == "NOT EVALUABLE", 5, NA_real_)))))
## Dates: RSDTC yymmdd10. -> %Y-%m-%d (single format)
adeff_resp$ADT <- as.Date(adeff_resp$RSDTC, format = "%Y-%m-%d")
adiff <- as.numeric(adeff_resp$ADT - adeff_resp$TRTSDT)
adeff_resp$ADY <- ifelse(!is.na(adeff_resp$ADT) & !is.na(adeff_resp$TRTSDT),
  ifelse(adeff_resp$ADT >= adeff_resp$TRTSDT, adiff + 1, adiff), NA_real_)
adeff_resp$AVISIT <- adeff_resp$VISIT
adeff_resp$AVISITN <- adeff_resp$VISITNUM
adeff_resp$ABLFL <- ifelse(!is.na(adeff_resp$AVISIT) & adeff_resp$AVISIT == "Baseline", "Y", "")
adeff_resp$ANL01FL <- ifelse(adeff_resp$ABLFL != "Y" & !is.na(adeff_resp$AVAL), "Y", "")
adeff_resp <- adeff_resp[, c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","EFFFL","TRTSDT","TRTEDT","AGE","SEX",
  "PARAMCD","PARAM","PARAMN","AVAL","AVALC","ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL")]

## ============================================================
## STEP 4: tumor size SUMDIAM
## ============================================================
tu_s <- tu[order(tu$STUDYID, tu$USUBJID), ]
tu_f <- tu_s[tu_s$TUTESTCD == "LDIAM", ]
m_tum <- merge(tu_f, adsl_s, by = c("STUDYID","USUBJID"), all = FALSE)
adeff_tumor <- m_tum
adeff_tumor$PARAMCD <- "SUMDIAM"
adeff_tumor$PARAM <- "Sum of Longest Diameters"
adeff_tumor$PARAMN <- 2
adeff_tumor$AVAL <- adeff_tumor$TUSTRESN
adeff_tumor$AVALC <- NA_character_
adeff_tumor$AVALU <- adeff_tumor$TUSTRESU
adeff_tumor$ADT <- as.Date(adeff_tumor$TUDTC, format = "%Y-%m-%d")
adiff2 <- as.numeric(adeff_tumor$ADT - adeff_tumor$TRTSDT)
adeff_tumor$ADY <- ifelse(!is.na(adeff_tumor$ADT) & !is.na(adeff_tumor$TRTSDT),
  ifelse(adeff_tumor$ADT >= adeff_tumor$TRTSDT, adiff2 + 1, adiff2), NA_real_)
adeff_tumor$AVISIT <- adeff_tumor$VISIT
adeff_tumor$AVISITN <- adeff_tumor$VISITNUM
adeff_tumor$ABLFL <- ifelse(!is.na(adeff_tumor$AVISIT) & adeff_tumor$AVISIT == "Baseline", "Y", "")
adeff_tumor$ANL01FL <- ifelse(adeff_tumor$ABLFL != "Y" & !is.na(adeff_tumor$AVAL), "Y", "")
adeff_tumor <- adeff_tumor[, c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","EFFFL","TRTSDT","TRTEDT","AGE","SEX",
  "PARAMCD","PARAM","PARAMN","AVAL","AVALC","AVALU","ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL")]

## ============================================================
## STEP 5: baseline and change for tumor
## ============================================================
base_tumor <- adeff_tumor[adeff_tumor$ABLFL == "Y", ]
base_tumor$BASE <- base_tumor$AVAL
base_tumor <- base_tumor[, c("USUBJID","PARAMCD","BASE")]
base_tumor <- base_tumor[order(base_tumor$USUBJID, base_tumor$PARAMCD), ]
adeff_tumor <- adeff_tumor[order(adeff_tumor$USUBJID, adeff_tumor$PARAMCD), ]
adeff_tumor <- merge(adeff_tumor, base_tumor, by = c("USUBJID","PARAMCD"), all.x = TRUE, sort = FALSE)
adeff_tumor$CHG <- ifelse(!is.na(adeff_tumor$AVAL) & !is.na(adeff_tumor$BASE),
  adeff_tumor$AVAL - adeff_tumor$BASE, NA_real_)
adeff_tumor$PCHG <- ifelse(!is.na(adeff_tumor$AVAL) & !is.na(adeff_tumor$BASE) & adeff_tumor$BASE > 0,
  (adeff_tumor$CHG / adeff_tumor$BASE) * 100, NA_real_)
## restore order by USUBJID PARAMCD
adeff_tumor <- adeff_tumor[order(adeff_tumor$STUDYID, adeff_tumor$USUBJID), ]

## ============================================================
## STEP 6: best overall response BOR
## ============================================================
resp_post <- adeff_resp[!(adeff_resp$ABLFL == "Y"), ]
resp_post <- resp_post[order(resp_post$USUBJID, resp_post$AVAL), ]
## first per USUBJID
resp_post <- resp_post[!duplicated(resp_post$USUBJID), ]
bor <- resp_post
bor$PARAMCD <- "BOR"
bor$PARAM <- "Best Overall Response"
bor$PARAMN <- 3
bor$AVISIT <- "Best Response"
bor$AVISITN <- 99
bor$ABLFL <- ""
bor$ANL01FL <- "Y"

## ============================================================
## STEP 7: responder flag
## ============================================================
respfl <- bor
respfl$RESPFL <- ifelse(respfl$AVAL %in% c(1,2), "Y", "N")
respfl$PARAMCD <- "RESPFL"
respfl$PARAM <- "Objective Response Flag"
respfl$PARAMN <- 4
respfl$AVAL <- ifelse(respfl$RESPFL == "Y", 1, 0)
respfl$AVALC <- respfl$RESPFL

## ============================================================
## STEP 8: minimum tumor size (nadir)
## ============================================================
tum_post <- adeff_tumor[!(adeff_tumor$ABLFL == "Y"), ]
## proc means min by USUBJID
nadir_min <- aggregate(AVAL ~ USUBJID, data = tum_post, FUN = function(x) min(x, na.rm = TRUE))
names(nadir_min)[names(nadir_min) == "AVAL"] <- "AVAL"
## baseline info
base_info <- adeff_tumor[adeff_tumor$ABLFL == "Y",
  c("USUBJID","STUDYID","TRTP","TRTPN","SAFFL","EFFFL","TRTSDT","TRTEDT","AGE","SEX","BASE")]
base_info <- base_info[!duplicated(base_info$USUBJID), ]
nadir <- merge(nadir_min, base_info, by = "USUBJID", all.x = TRUE, sort = FALSE)
nadir$PARAMCD <- "NADIRTUM"
nadir$PARAM <- "Nadir Tumor Size"
nadir$PARAMN <- 5
nadir$AVALC <- NA_character_
nadir$AVALU <- "mm"
nadir$ADT <- as.Date(NA_character_)
nadir$ADY <- NA_real_
nadir$AVISIT <- "Nadir"
nadir$AVISITN <- 98
nadir$ABLFL <- ""
nadir$ANL01FL <- "Y"
nadir$CHG <- ifelse(!is.na(nadir$AVAL) & !is.na(nadir$BASE), nadir$AVAL - nadir$BASE, NA_real_)
nadir$PCHG <- ifelse(!is.na(nadir$AVAL) & !is.na(nadir$BASE) & nadir$BASE > 0,
  (nadir$CHG / nadir$BASE) * 100, NA_real_)

## ============================================================
## STEP 9: combine
## ============================================================
adeff <- dplyr::bind_rows(adeff_resp, adeff_tumor, bor, respfl, nadir)

## ============================================================
## STEP 10: sort, ASEQ, RSCAT
## ============================================================
adeff <- adeff[order(adeff$STUDYID, adeff$USUBJID, adeff$PARAMN, adeff$AVISITN, adeff$ADT, na.last = FALSE), ]
## ASEQ by STUDYID USUBJID
adeff$ASEQ <- ave(seq_len(nrow(adeff)), paste(adeff$STUDYID, adeff$USUBJID), FUN = seq_along)
adeff$RSCAT <- NA_character_
idx <- adeff$PARAMCD %in% c("OVRLRESP","BOR")
av <- adeff$AVALC
adeff$RSCAT[idx] <- ifelse(av[idx] %in% c("COMPLETE RESPONSE","PARTIAL RESPONSE"), "RESPONDER",
  ifelse(av[idx] == "STABLE DISEASE", "STABLE",
  ifelse(av[idx] == "PROGRESSIVE DISEASE", "PROGRESSOR", "OTHER")))
## final column order (SAS SET order + ASEQ RSCAT)
allcols <- c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","EFFFL","TRTSDT","TRTEDT","AGE","SEX",
  "PARAMCD","PARAM","PARAMN","AVAL","AVALC","ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL",
  "AVALU","BASE","CHG","PCHG","RESPFL","ASEQ","RSCAT")
for (cc in allcols) if (!cc %in% names(adeff)) adeff[[cc]] <- NA
adeff <- adeff[, allcols]

## ============================================================
## Outputs
## ============================================================
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(adeff_resp, file.path(out, "adeff_resp.csv"), row.names = FALSE, na = "")
## adeff_tumor includes BASE CHG PCHG
adeff_tumor_out <- adeff_tumor[, c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","EFFFL","TRTSDT","TRTEDT","AGE","SEX",
  "PARAMCD","PARAM","PARAMN","AVAL","AVALC","AVALU","ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL","BASE","CHG","PCHG")]
write.csv(adeff_tumor_out, file.path(out, "adeff_tumor.csv"), row.names = FALSE, na = "")
write.csv(adeff, file.path(out, "adeff.csv"), row.names = FALSE, na = "")

## P088 table: PROC CONTENTS + PROC PRINT of adeff
fmt9 <- function(d) ifelse(is.na(d), "", toupper(format(d, "%d%b%Y")))
## CSV of table body (same as adeff)
write.csv(adeff, file.path(out, "P088.csv"), row.names = FALSE, na = "")

## text rendering
txtf <- file.path(out, "P088.txt")
sink(txtf)
cat("CONTENTS OF ADEFF\n")
cat("------------------------------------------------------------\n")
for (i in seq_along(names(adeff))) {
  v <- names(adeff)[i]
  tp <- if (v %in% c("TRTSDT","TRTEDT","ADT")) "Date" else if (is.numeric(adeff[[v]])) "Numeric" else "Character"
  cat(sprintf("%3d  %-10s %s\n", i, v, tp))
}
cat("\nPRINT OF ADEFF (noobs)\n")
cat("------------------------------------------------------------\n")
pa <- adeff
pa$TRTSDT_c <- fmt9(pa$TRTSDT)
pa$TRTEDT_c <- fmt9(pa$TRTEDT)
pa$ADT_c <- fmt9(pa$ADT)
## header
hdr <- paste(names(pa), collapse = " | ")
cat(hdr, "\n")
for (r in seq_len(nrow(pa))) {
  rowv <- pa[r, ]
  vals <- sapply(names(pa), function(cn) {
    x <- rowv[[cn]]
    if (cn %in% c("TRTSDT","TRTEDT","ADT")) {
      cc <- paste0(cn, "_c"); return(as.character(rowv[[cc]]))
    }
    if (cn %in% c("TRTSDT_c","TRTEDT_c","ADT_c")) return(NULL)
    if (is.na(x)) return("")
    as.character(x)
  }, simplify = FALSE)
  vals <- vals[!sapply(vals, is.null)]
  cat(paste(unlist(vals), collapse = " | "), "\n")
}
sink()
## also write .lst copy for convenience
file.copy(txtf, file.path(out, "P088.lst"), overwrite = TRUE)

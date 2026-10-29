```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, {
    mult <- 10 ^ digits
    sign(x) * floor(abs(x) * mult + 0.5) / mult
  })
}

suppressPackageStartupMessages({
  library(dplyr)
})

message("P088 adeff: creating inline SDTM/ADSL")

#--- RS ---
rs <- data.frame(
  STUDYID = rep("STUDY01", 16),
  USUBJID = c(rep("STUDY01-1001",4), rep("STUDY01-1002",3), rep("STUDY01-1003",3),
              rep("STUDY01-1004",2), rep("STUDY01-1005",2), rep("STUDY01-1006",2)),
  RSSEQ = c(1,2,3,4, 1,2,3, 1,2,3, 1,2, 1,2, 1,2),
  RSTESTCD = rep("OVRLRESP",16),
  RSTEST = rep("Overall Response",16),
  RSSTRESC = c("STABLE DISEASE","STABLE DISEASE","PARTIAL RESPONSE","PARTIAL RESPONSE",
               "STABLE DISEASE","STABLE DISEASE","PROGRESSIVE DISEASE",
               "STABLE DISEASE","PARTIAL RESPONSE","COMPLETE RESPONSE",
               "STABLE DISEASE","STABLE DISEASE",
               "STABLE DISEASE","PROGRESSIVE DISEASE",
               "STABLE DISEASE","PARTIAL RESPONSE"),
  RSSTRESN = rep(NA_real_,16),
  VISIT = c("Baseline","Week 8","Week 16","Week 24",
            "Baseline","Week 8","Week 16",
            "Baseline","Week 8","Week 16",
            "Baseline","Week 8",
            "Baseline","Week 8",
            "Baseline","Week 8"),
  VISITNUM = c(1,3,4,5, 1,3,4, 1,3,4, 1,3, 1,3, 1,3),
  RSDTC = c("2024-01-15","2024-03-11","2024-05-06","2024-07-01",
            "2024-01-18","2024-03-14","2024-05-09",
            "2024-01-20","2024-03-16","2024-05-11",
            "2024-01-25","2024-03-21",
            "2024-01-28","2024-03-24",
            "2024-01-30","2024-03-26"),
  stringsAsFactors = FALSE
)

#--- TU ---
tu <- data.frame(
  STUDYID = rep("STUDY01",16),
  USUBJID = c(rep("STUDY01-1001",4), rep("STUDY01-1002",3), rep("STUDY01-1003",3),
              rep("STUDY01-1004",2), rep("STUDY01-1005",2), rep("STUDY01-1006",2)),
  TUSEQ = c(1,2,3,4, 1,2,3, 1,2,3, 1,2, 1,2, 1,2),
  TUTESTCD = rep("LDIAM",16),
  TUTEST = rep("Longest Diameter",16),
  TUORRES = as.character(c(45,42,35,30, 38,40,52, 55,40,0, 32,30, 28,42, 48,36)),
  TUSTRESC = as.character(c(45,42,35,30, 38,40,52, 55,40,0, 32,30, 28,42, 48,36)),
  TUSTRESN = c(45,42,35,30, 38,40,52, 55,40,0, 32,30, 28,42, 48,36),
  TUSTRESU = rep("mm",16),
  TULOC = c(rep("LIVER",4), rep("LUNG",3), rep("LYMPH NODE",3), rep("LIVER",2), rep("LUNG",2), rep("LIVER",2)),
  VISIT = c("Baseline","Week 8","Week 16","Week 24",
            "Baseline","Week 8","Week 16",
            "Baseline","Week 8","Week 16",
            "Baseline","Week 8",
            "Baseline","Week 8",
            "Baseline","Week 8"),
  VISITNUM = c(1,3,4,5, 1,3,4, 1,3,4, 1,3, 1,3, 1,3),
  TUDTC = c("2024-01-15","2024-03-11","2024-05-06","2024-07-01",
            "2024-01-18","2024-03-14","2024-05-09",
            "2024-01-20","2024-03-16","2024-05-11",
            "2024-01-25","2024-03-21",
            "2024-01-28","2024-03-24",
            "2024-01-30","2024-03-26"),
  stringsAsFactors = FALSE
)

#--- ADSL ---
adsl <- data.frame(
  STUDYID = rep("STUDY01",6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  SEX = c("M","F","M","F","M","F"),
  SAFFL = rep("Y",6),
  EFFFL = rep("Y",6),
  TRTSDT = as.Date(c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"), format="%d%b%Y"),
  TRTEDT = as.Date(c("15JUN2024","18JUN2024","01APR2024","25JUN2024",NA,"01JUL2024"), format="%d%b%Y"),
  stringsAsFactors = FALSE
)
# order columns as SAS keep/input reasonable
adsl <- adsl[, c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL","EFFFL","TRTSDT","TRTEDT")]

message("STEP 3: adeff_resp")
rs_f <- rs[rs$RSTESTCD == "OVRLRESP", ]
# sort like PROC SORT
rs_f <- rs_f[order(rs_f$STUDYID, rs_f$USUBJID), ]
adsl_s <- adsl[order(adsl$STUDYID, adsl$USUBJID), ]
adeff_resp <- merge(rs_f, adsl_s, by=c("STUDYID","USUBJID"), all=FALSE, sort=FALSE)
# derive
adeff_resp$PARAMCD <- "OVRLRESP"
adeff_resp$PARAM <- "Overall Response"
adeff_resp$PARAMN <- 1
adeff_resp$AVALC <- adeff_resp$RSSTRESC
up <- toupper(adeff_resp$RSSTRESC)
adeff_resp$AVAL <- ifelse(up=="COMPLETE RESPONSE",1,
                   ifelse(up=="PARTIAL RESPONSE",2,
                   ifelse(up=="STABLE DISEASE",3,
                   ifelse(up=="PROGRESSIVE DISEASE",4,
                   ifelse(up=="NOT EVALUABLE",5,NA_real_)))))
adeff_resp$ADT <- as.Date(adeff_resp$RSDTC, format="%Y-%m-%d")
adeff_resp$ADT[adeff_resp$RSDTC==""] <- NA
adeff_resp$ADY <- rep(NA_real_, nrow(adeff_resp))
idx <- !is.na(adeff_resp$ADT) & !is.na(adeff_resp$TRTSDT)
ge <- idx & adeff_resp$ADT >= adeff_resp$TRTSDT
lt <- idx & adeff_resp$ADT < adeff_resp$TRTSDT
adeff_resp$ADY[ge] <- as.integer(adeff_resp$ADT[ge] - adeff_resp$TRTSDT[ge] + 1)
adeff_resp$ADY[lt] <- as.integer(adeff_resp$ADT[lt] - adeff_resp$TRTSDT[lt])
adeff_resp$AVISIT <- adeff_resp$VISIT
adeff_resp$AVISITN <- adeff_resp$VISITNUM
adeff_resp$ABLFL <- ifelse(adeff_resp$AVISIT=="Baseline","Y","")
adeff_resp$ANL01FL <- ifelse(adeff_resp$ABLFL!="Y" & !is.na(adeff_resp$AVAL),"Y","")
adeff_resp <- adeff_resp[, c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","EFFFL","TRTSDT","TRTEDT","AGE","SEX",
  "PARAMCD","PARAM","PARAMN","AVAL","AVALC","ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL")]

message("STEP 4: adeff_tumor")
tu_f <- tu[tu$TUTESTCD=="LDIAM", ]
tu_f <- tu_f[order(tu_f$STUDYID, tu_f$USUBJID), ]
adeff_tumor <- merge(tu_f, adsl_s, by=c("STUDYID","USUBJID"), all=FALSE, sort=FALSE)
adeff_tumor$PARAMCD <- "SUMDIAM"
adeff_tumor$PARAM <- "Sum of Longest Diameters"
adeff_tumor$PARAMN <- 2
adeff_tumor$AVAL <- adeff_tumor$TUSTRESN
adeff_tumor$AVALC <- ""
adeff_tumor$AVALU <- adeff_tumor$TUSTRESU
adeff_tumor$ADT <- as.Date(adeff_tumor$TUDTC, format="%Y-%m-%d")
adeff_tumor$ADT[adeff_tumor$TUDTC==""] <- NA
adeff_tumor$ADY <- rep(NA_real_, nrow(adeff_tumor))
idx2 <- !is.na(adeff_tumor$ADT) & !is.na(adeff_tumor$TRTSDT)
ge2 <- idx2 & adeff_tumor$ADT >= adeff_tumor$TRTSDT
lt2 <- idx2 & adeff_tumor$ADT < adeff_tumor$TRTSDT
adeff_tumor$ADY[ge2] <- as.integer(adeff_tumor$ADT[ge2] - adeff_tumor$TRTSDT[ge2] + 1)
adeff_tumor$ADY[lt2] <- as.integer(adeff_tumor$ADT[lt2] - adeff_tumor$TRTSDT[lt2])
adeff_tumor$AVISIT <- adeff_tumor$VISIT
adeff_tumor$AVISITN <- adeff_tumor$VISITNUM
adeff_tumor$ABLFL <- ifelse(adeff_tumor$AVISIT=="Baseline","Y","")
adeff_tumor$ANL01FL <- ifelse(adeff_tumor$ABLFL!="Y" & !is.na(adeff_tumor$AVAL),"Y","")
adeff_tumor <- adeff_tumor[, c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","EFFFL","TRTSDT","TRTEDT","AGE","SEX",
  "PARAMCD","PARAM","PARAMN","AVAL","AVALC","AVALU","ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL")]

message("STEP 5: baseline and change")
base_tumor <- adeff_tumor[adeff_tumor$ABLFL=="Y", ]
base_tumor$BASE <- base_tumor$AVAL
base_tumor <- base_tumor[, c("USUBJID","PARAMCD","BASE")]
# sort
adeff_tumor <- adeff_tumor[order(adeff_tumor$USUBJID, adeff_tumor$PARAMCD), ]
base_tumor <- base_tumor[order(base_tumor$USUBJID, base_tumor$PARAMCD), ]
adeff_tumor <- merge(adeff_tumor, base_tumor, by=c("USUBJID","PARAMCD"), all.x=TRUE, sort=FALSE)
adeff_tumor$CHG <- rep(NA_real_, nrow(adeff_tumor))
adeff_tumor$PCHG <- rep(NA_real_, nrow(adeff_tumor))
ok <- !is.na(adeff_tumor$AVAL) & !is.na(adeff_tumor$BASE)
adeff_tumor$CHG[ok] <- adeff_tumor$AVAL[ok] - adeff_tumor$BASE[ok]
ok2 <- ok & adeff_tumor$BASE > 0
adeff_tumor$PCHG[ok2] <- (adeff_tumor$CHG[ok2] / adeff_tumor$BASE[ok2]) * 100

message("STEP 6: BOR")
resp_sorted <- adeff_resp[adeff_resp$ABLFL!="Y", ]
resp_sorted <- resp_sorted[order(resp_sorted$USUBJID, resp_sorted$AVAL, resp_sorted$ADT), ]
# first per USUBJID
resp_sorted <- resp_sorted[!duplicated(resp_sorted$USUBJID), ]
bor <- resp_sorted
bor$PARAMCD <- "BOR"
bor$PARAM <- "Best Overall Response"
bor$PARAMN <- 3
bor$AVISIT <- "Best Response"
bor$AVISITN <- 99
bor$ABLFL <- ""
bor$ANL01FL <- "Y"

message("STEP 7: responder flag")
respfl <- bor
respfl$RESPFL <- ifelse(respfl$AVAL %in% c(1,2), "Y", "N")
respfl$PARAMCD <- "RESPFL"
respfl$PARAM <- "Objective Response Flag"
respfl$PARAMN <- 4
respfl$AVAL <- ifelse(respfl$RESPFL=="Y", 1, 0)
respfl$AVALC <- respfl$RESPFL

message("STEP 8: nadir")
tumor_post <- adeff_tumor[adeff_tumor$ABLFL!="Y", ]
nadir_min <- aggregate(AVAL ~ USUBJID, data=tumor_post, FUN=function(x) min(x, na.rm=TRUE))
# baseline info
base_info <- adeff_tumor[adeff_tumor$ABLFL=="Y", c("ABLFL","USUBJID","STUDYID","TRTP","TRTPN","SAFFL","EFFFL","TRTSDT","TRTEDT","AGE","SEX","BASE")]
# merge (left join, if a)
nadir <- merge(nadir_min, base_info, by="USUBJID", all.x=TRUE, sort=FALSE)
nadir$PARAMCD <- "NADIRTUM"
nadir$PARAM <- "Nadir Tumor Size"
nadir$PARAMN <- 5
nadir$AVALU <- "mm"
nadir$AVISIT <- "Nadir"
nadir$AVISITN <- 98
nadir$ABLFL <- ""
nadir$ANL01FL <- "Y"
nadir$CHG <- rep(NA_real_, nrow(nadir))
nadir$PCHG <- rep(NA_real_, nrow(nadir))
okn <- !is.na(nadir$AVAL) & !is.na(nadir$BASE)
nadir$CHG[okn] <- nadir$AVAL[okn] - nadir$BASE[okn]
okn2 <- okn & nadir$BASE > 0
nadir$PCHG[okn2] <- (nadir$CHG[okn2] / nadir$BASE[okn2]) * 100
# nadir lacks AVALC, ADT, ADY
nadir$AVALC <- ""
nadir$ADT <- as.Date(rep(NA_character_, nrow(nadir)))
nadir$ADY <- rep(NA_real_, nrow(nadir))

message("STEP 9: combine")
adeff <- dplyr::bind_rows(adeff_resp, adeff_tumor, bor, respfl, nadir)

message("STEP 10: sort, ASEQ, RSCAT")
# SAS sort by STUDYID USUBJID PARAMN AVISITN ADT (missing first)
ord <- order(adeff$STUDYID, adeff$USUBJID, adeff$PARAMN, adeff$AVISITN, !is.na(adeff$ADT), adeff$ADT, na.last=FALSE)
adeff <- adeff[ord, ]
# ASEQ per STUDYID USUBJID
adeff$ASEQ <- ave(seq_len(nrow(adeff)), paste(adeff$STUDYID, adeff$USUBJID, sep="|"), FUN=function(x) seq_along(x))
# RSCAT
adeff$RSCAT <- ""
is_resp_param <- adeff$PARAMCD %in% c("OVRLRESP","BOR")
adeff$RSCAT[is_resp_param & adeff$AVALC %in% c("COMPLETE RESPONSE","PARTIAL RESPONSE")] <- "RESPONDER"
adeff$RSCAT[is_resp_param & adeff$AVALC=="STABLE DISEASE"] <- "STABLE"
adeff$RSCAT[is_resp_param & adeff$AVALC=="PROGRESSIVE DISEASE"] <- "PROGRESSOR"
# OTHER for remaining resp params where blank logic: if param is resp but not yet assigned and not missing? SAS else RSCAT='OTHER'
need_other <- is_resp_param & adeff$RSCAT==""
adeff$RSCAT[need_other] <- "OTHER"

# final column order
final_cols <- c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","EFFFL","TRTSDT","TRTEDT","AGE","SEX",
  "PARAMCD","PARAM","PARAMN","AVAL","AVALC","AVALU","ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL",
  "BASE","CHG","PCHG","RESPFL","ASEQ","RSCAT")
# ensure all exist
for (cc in final_cols) if (!cc %in% names(adeff)) adeff[[cc]] <- NA
adeff <- adeff[, final_cols]

# ensure types: dates are Date
adeff$TRTSDT <- as.Date(adeff$TRTSDT, origin="1970-01-01")
adeff$TRTEDT <- as.Date(adeff$TRTEDT, origin="1970-01-01")
adeff$ADT <- as.Date(adeff$ADT, origin="1970-01-01")

message("writing datasets")
write.csv(adsl, file.path(out, "adsl.csv"), row.names=FALSE, na="")
write.csv(adeff_resp, file.path(out, "adeff_resp.csv"), row.names=FALSE, na="")
write.csv(adeff_tumor, file.path(out, "adeff_tumor.csv"), row.names=FALSE, na="")
write.csv(adeff, file.path(out, "adeff.csv"), row.names=FALSE, na="")

#--- P088 listing: PROC CONTENTS + PROC PRINT ---
# Build text rendering
txt_path <- file.path(out, "P088.txt")
lst_path <- file.path(out, "P088.lst")
csv_path <- file.path(out, "P088.csv")

# contents info
var_info <- data.frame(
  Var = names(adeff),
  Type = sapply(adeff, function(x) if (inherits(x,"Date")) "Date" else if (is.numeric(x)) "Num" else "Char"),
  stringsAsFactors=FALSE
)

# format dates for display as DATE9.
fmt_date9 <- function(d) {
  ifelse(is.na(d), "", format(d, "%d%b%Y"))
}
disp <- as.data.frame(adeff, stringsAsFactors=FALSE)
# create display copy with formatted dates and numbers
# keep numeric as is for txt but ensure representation
con <- file(txt_path, open="wt")
writeLines("P088 ADEFF - Efficacy Analysis Dataset (BDS)", con)
writeLines(paste(rep("=", 70), collapse=""), con)
writeLines("", con)
writeLines("PROC CONTENTS DATA=ADEFF VARNUM;", con)
writeLines("", con)
for (i in seq_len(nrow(var_info))) {
  writeLines(sprintf("%2d  %-8s  %s", i, var_info$Var[i], var_info$Type[i]), con)
}
writeLines("", con)
writeLines(sprintf("Observations: %d  Variables: %d", nrow(adeff), ncol(adeff)), con)
writeLines("", con)
writeLines("PROC PRINT DATA=ADEFF NOOBS;", con)
writeLines("", con)
close(con)

# append printed table using fixed capture (avoid full console print, write to file)
# Use utils::capture.output on a truncated-width print but include all rows/cols
# Write header + rows tab-delimited style for readability
hdr <- paste(names(disp), collapse=" | ")
write(hdr, file=txt_path, append=TRUE)
# format each row
fmt_num <- function(x) ifelse(is.na(x), "", as.character(x))
for (r in seq_len(nrow(disp))) {
  row <- disp[r, ]
  vals <- sapply(names(disp), function(cn) {
    v <- row[[cn]]
    if (inherits(v, "Date")) {
      if (is.na(v)) "" else format(v, "%d%b%Y")
    } else if (is.numeric(v)) {
      if (is.na(v)) "" else as.character(v)
    } else {
      if (is.na(v)) "" else as.character(v)
    }
  })
  write(paste(vals, collapse=" | "), file=txt_path, append=TRUE)
}
# summary counts for verification
write("", file=txt_path, append=TRUE)
write(sprintf("Total rows: %d", nrow(adeff)), file=txt_path, append=TRUE)
# responder summary
write("", file=txt_path, append=TRUE)
write("Summary by PARAMCD:", file=txt_path, append=TRUE)
tab <- as.data.frame(table(adeff$PARAMCD), stringsAsFactors=FALSE)
for (i in seq_len(nrow(tab))) {
  write(sprintf("%s : %s", tab$Var1[i], tab$Freq[i]), file=txt_path, append=TRUE)
}

# CSV of table body = adeff print body
write.csv(adeff, csv_path, row.names=FALSE, na="")
# also copy txt to .lst for benchmark expectation
file.copy(txt_path, lst_path, overwrite=TRUE)

message("done")
```
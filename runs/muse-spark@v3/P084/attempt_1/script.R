out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
format_date9 <- function(d) {
  ifelse(is.na(d), ".",
    paste0(sprintf("%02d", as.integer(format(d, "%d"))),
           toupper(format(d, "%b")),
           format(d, "%Y")))
}
parse_date9 <- function(x) {
  # strict DDMMMYYYY e.g. 15JAN2024
  x <- trimws(as.character(x))
  res <- as.Date(rep(NA_character_, length(x)))
  idx <- !is.na(x) & x != "" & x != "."
  if (any(idx)) {
    xx <- x[idx]
    dd <- suppressWarnings(as.integer(substr(xx, 1, 2)))
    mm_raw <- toupper(substr(xx, 3, 5))
    yy <- suppressWarnings(as.integer(substr(xx, 6, 9)))
    mon_map <- c(JAN=1,FEB=2,MAR=3,APR=4,MAY=5,JUN=6,JUL=7,AUG=8,SEP=9,OCT=10,NOV=11,DEC=12)
    mm <- unname(mon_map[mm_raw])
    res[idx] <- as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd), format="%Y-%m-%d")
  }
  res
}
sas_key <- function(v) {
  sapply(as.character(v), function(s) {
    if (is.na(s)) return("\001")
    paste(sprintf("%03d", as.integer(charToRaw(s))), collapse=" ")
  }, USE.NAMES=FALSE)
}
sas_order <- function(...) {
  cols <- list(...)
  keys <- lapply(cols, function(c) {
    if (is.character(c)) sas_key(c) else c
  })
  # missing first: NAs already handled; for numeric NA put -Inf
  do.call(order, c(keys, list(na.last=FALSE, method="radix")))
}

# STEP 1: mock SDTM AE
ae <- data.frame(
  STUDYID = rep("STUDY01", 9),
  USUBJID = c("STUDY01-1001","STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1003","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  AESEQ = c(1,2,1,1,2,3,1,1,1),
  AETERM = c("Headache","Nausea","Fatigue","Dizziness","Rash","Hepatotoxicity","Insomnia","Vomiting","Back pain"),
  AEDECOD = c("Headache","Nausea","Fatigue","Dizziness","Rash","Hepatotoxicity","Insomnia","Vomiting","Back pain"),
  AEBODSYS = c("Nervous system disorders","Gastrointestinal disorders","General disorders","Nervous system disorders","Skin disorders","Hepatobiliary disorders","Psychiatric disorders","Gastrointestinal disorders","Musculoskeletal disorders"),
  AESTDTC = c("2024-02-01","2024-02-15","2024-02-10","2024-02-20","2024-03-05","2024-03-25","2024-02-25","2024-03-01","2024-02-28"),
  AEENDTC = c("2024-02-03","2024-02-18","2024-03-01","2024-02-22","2024-03-15",".","2024-03-10","2024-03-02","2024-03-05"),
  AESEV = c("MILD","MODERATE","MILD","MILD","MODERATE","SEVERE","MILD","MODERATE","MILD"),
  AESER = c("N","N","N","N","N","Y","N","N","N"),
  AEREL = c("NOT RELATED","POSSIBLY RELATED","POSSIBLY RELATED","NOT RELATED","PROBABLY RELATED","RELATED","UNLIKELY","POSSIBLY RELATED","NOT RELATED"),
  AEACN = c("DOSE NOT CHANGED","DOSE NOT CHANGED","DOSE NOT CHANGED","DOSE NOT CHANGED","DOSE REDUCED","DRUG WITHDRAWN","DOSE NOT CHANGED","DOSE NOT CHANGED","DOSE NOT CHANGED"),
  AEOUT = c("RECOVERED","RECOVERED","RECOVERED","RECOVERED","RECOVERED","NOT RECOVERED","RECOVERED","RECOVERED","RECOVERED"),
  AETOXGR = c("1","2","1","1","2","3","1","2","1"),
  stringsAsFactors = FALSE
)

# STEP 2: mock ADSL
adsl_raw <- data.frame(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  SITEID = c("101","101","102","102","103","103"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  AGEGR1 = c("<65","<65","<65","<65","<65",">=65"),
  SEX = c("M","F","M","F","M","F"),
  RACE = c("WHITE","BLACK OR AFRICAN AMERICAN","ASIAN","WHITE","ASIAN","WHITE"),
  SAFFL = rep("Y", 6),
  TRTSDT_c = c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"),
  TRTEDT_c = c("15JUN2024","18JUN2024","01APR2024","25JUN2024",".","01JUL2024"),
  stringsAsFactors = FALSE
)
adsl <- adsl_raw
adsl$TRTSDT <- parse_date9(adsl$TRTSDTC <- NULL)
# parse explicitly
adsl$TRTSDT <- parse_date9(adsl_raw$TRTSDT_c)
adsl$TRTEDT <- parse_date9(adsl_raw$TRTEDT_c)
adsl <- adsl[, c("STUDYID","USUBJID","SITEID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","TRTSDT","TRTEDT")]

# STEP 3: sort + merge
o1 <- sas_order(ae$STUDYID, ae$USUBJID)
ae <- ae[o1, , drop=FALSE]
o2 <- sas_order(adsl$STUDYID, adsl$USUBJID)
adsl <- adsl[o2, , drop=FALSE]

adsl_keep <- adsl[, c("STUDYID","USUBJID","SITEID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","TRTSDT","TRTEDT")]
# merge by STUDYID USUBJID, if a and b (inner join)
adae <- merge(ae, adsl_keep, by=c("STUDYID","USUBJID"), all=FALSE, sort=FALSE)
stopifnot(all(c("STUDYID","USUBJID") %in% names(adae)))
stopifnot(nrow(adae) == 9)

# derive dates: yymmdd10. single format %Y-%m-%d
adae$ASTDT <- as.Date(NA)
idx <- !is.na(adae$AESTDTC) & adae$AESTDTC != ""
adae$ASTDT[idx] <- as.Date(adae$AESTDTC[idx], format="%Y-%m-%d")
adae$AENDT <- as.Date(NA)
idx2 <- !is.na(adae$AEENDTC) & adae$AEENDTC != "" & adae$AEENDTC != "."
adae$AENDT[idx2] <- as.Date(adae$AEENDTC[idx2], format="%Y-%m-%d")

adae$ASTDY <- NA_real_
adae$AENDY <- NA_real_
ok <- !is.na(adae$ASTDT) & !is.na(adae$TRTSDT)
adae$ASTDY[ok & adae$ASTDT >= adae$TRTSDT] <- as.integer(adae$ASTDT[ok & adae$ASTDT >= adae$TRTSDT] - adae$TRTSDT[ok & adae$ASTDT >= adae$TRTSDT] + 1)
adae$ASTDY[ok & adae$ASTDT < adae$TRTSDT] <- as.integer(adae$ASTDT[ok & adae$ASTDT < adae$TRTSDT] - adae$TRTSDT[ok & adae$ASTDT < adae$TRTSDT])
ok2 <- !is.na(adae$AENDT) & !is.na(adae$TRTSDT)
adae$AENDY[ok2 & adae$AENDT >= adae$TRTSDT] <- as.integer(adae$AENDT[ok2 & adae$AENDT >= adae$TRTSDT] - adae$TRTSDT[ok2 & adae$AENDT >= adae$TRTSDT] + 1)
adae$AENDY[ok2 & adae$AENDT < adae$TRTSDT] <- as.integer(adae$AENDT[ok2 & adae$AENDT < adae$TRTSDT] - adae$TRTSDT[ok2 & adae$AENDT < adae$TRTSDT])

# Treatment-emergent flag (default N)
adae$TRTEMFL <- ""
for (i in seq_len(nrow(adae))) {
  v <- ""
  if (!is.na(adae$ASTDT[i]) && !is.na(adae$TRTSDT[i])) {
    if (adae$ASTDT[i] >= adae$TRTSDT[i]) {
      if (is.na(adae$TRTEDT[i])) v <- "Y"
      else if (adae$ASTDT[i] <= adae$TRTEDT[i] + 30) v <- "Y"
    }
  }
  if (v == "") v <- "N"
  adae$TRTEMFL[i] <- v
}

adae$ASEV <- adae$AESEV
adae$ASEVN <- NA_real_
adae$ASEVN[toupper(adae$AESEV)=="MILD"] <- 1
adae$ASEVN[toupper(adae$AESEV)=="MODERATE"] <- 2
adae$ASEVN[toupper(adae$AESEV)=="SEVERE"] <- 3

adae$AREL <- adae$AEREL
adae$ARELN <- NA_real_
adae$ARELN[toupper(adae$AEREL)=="NOT RELATED"] <- 1
adae$ARELN[toupper(adae$AEREL)=="UNLIKELY"] <- 2
adae$ARELN[toupper(adae$AEREL)=="POSSIBLY RELATED"] <- 3
adae$ARELN[toupper(adae$AEREL)=="PROBABLY RELATED"] <- 4
adae$ARELN[toupper(adae$AEREL)=="RELATED"] <- 5

adae$AESER_FL <- adae$AESER
adae$AESOC <- adae$AEBODSYS
adae$AEDECOD_UP <- toupper(adae$AEDECOD)
adae$AEDUR <- NA_real_
okd <- !is.na(adae$ASTDT) & !is.na(adae$AENDT)
adae$AEDUR[okd] <- as.integer(adae$AENDT[okd] - adae$ASTDT[okd] + 1)

# STEP 4: occurrence flags; sort by USUBJID AEBODSYS AEDECOD ASTDT
ord4 <- sas_order(adae$USUBJID, adae$AEBODSYS, adae$AEDECOD, adae$ASTDT)
adae <- adae[ord4, , drop=FALSE]
n <- nrow(adae)
adae$AOCCFL <- ""
adae$AOCCSFL <- ""
adae$AOCCPFL <- ""
# FIRST. processing
for (i in seq_len(n)) {
  first_usubjid <- (i==1) || (adae$USUBJID[i] != adae$USUBJID[i-1])
  first_soc <- first_usubjid || (adae$AEBODSYS[i] != adae$AEBODSYS[i-1])
  first_pt <- first_soc || (adae$AEDECOD[i] != adae$AEDECOD[i-1])
  # Note SAS first.AEBODSYS is by USUBJID AEBODSYS so first_usubjid implies first_soc; same for pt
  if (first_usubjid) adae$AOCCFL[i] <- "Y" else adae$AOCCFL[i] <- ""
  if (first_soc) adae$AOCCSFL[i] <- "Y" else adae$AOCCSFL[i] <- ""
  if (first_pt) adae$AOCCPFL[i] <- "Y" else adae$AOCCPFL[i] <- ""
}

# STEP 5: sort by STUDYID USUBJID ASTDT AESEQ; ASEQ per STUDYID USUBJID
ord5 <- sas_order(adae$STUDYID, adae$USUBJID, adae$ASTDT, adae$AESEQ)
adae <- adae[ord5, , drop=FALSE]
adae$ASEQ <- NA_real_
for (i in seq_len(nrow(adae))) {
  if (i==1 || adae$STUDYID[i]!=adae$STUDYID[i-1] || adae$USUBJID[i]!=adae$USUBJID[i-1]) {
    adae$ASEQ[i] <- 1
  } else {
    adae$ASEQ[i] <- adae$ASEQ[i-1] + 1
  }
}

# final column order (SAS order)
final_cols <- c("STUDYID","USUBJID","AESEQ","AETERM","AEDECOD","AEBODSYS","AESTDTC","AEENDTC","AESEV","AESER","AEREL","AEACN","AEOUT","AETOXGR","SITEID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","TRTSDT","TRTEDT","ASTDT","AENDT","ASTDY","AENDY","TRTEMFL","ASEV","ASEVN","AREL","ARELN","AESER_FL","AESOC","AEDECOD_UP","AEDUR","AOCCFL","AOCCSFL","AOCCPFL","ASEQ")
missing_cols <- setdiff(final_cols, names(adae))
if (length(missing_cols)>0) stop(paste(missing_cols, collapse=","))
adae <- adae[, final_cols]

# labels
lbls <- c(STUDYID='Study Identifier',USUBJID='Unique Subject Identifier',AESEQ='Sequence Number',ASEQ='Analysis Sequence Number',AETERM='Reported Term for the Adverse Event',AEDECOD='Dictionary-Derived Term',AEBODSYS='Body System or Organ Class',AESOC='Primary System Organ Class',ASTDT='Analysis Start Date',AENDT='Analysis End Date',ASTDY='Analysis Start Relative Day',AENDY='Analysis End Relative Day',ASEV='Analysis Severity/Intensity',ASEVN='Analysis Severity/Intensity (N)',AREL='Analysis Causality',ARELN='Analysis Causality (N)',TRTEMFL='Treatment Emergent Analysis Flag',AESER_FL='Serious Event Flag',AOCCFL='First Occurrence of Any AE Flag',AOCCSFL='First Occurrence Within SOC Flag',AOCCPFL='First Occurrence of Preferred Term Flag',AEDUR='AE Duration (Days)',TRTP='Planned Treatment',TRTPN='Planned Treatment (N)',TRTSDT='Date of First Exposure to Treatment',TRTEDT='Date of Last Exposure to Treatment',SAFFL='Safety Population Flag')
for (nm in names(lbls)) attr(adae[[nm]], "label") <- lbls[[nm]]

# write datasets (ISO dates)
adae_out <- adae
adsl_out <- adsl
write.csv(adae_out, file.path(out, "adae.csv"), row.names=FALSE, na="")
write.csv(adsl_out, file.path(out, "adsl.csv"), row.names=FALSE, na="")

# ---- P084.lst rendering: CONTENTS + PRINT ----
# contents metadata
varnum <- seq_along(final_cols)
vtype <- sapply(final_cols, function(v) if (v %in% c("AESEQ","TRTPN","AGE","TRTSDT","TRTEDT","ASTDT","AENDT","ASTDY","AENDY","ASEVN","ARELN","AEDUR","ASEQ")) "Num" else "Char")
vlen <- c(STUDYID=12,USUBJID=20,AESEQ=8,AETERM=60,AEDECOD=60,AEBODSYS=60,AESTDTC=10,AEENDTC=10,AESEV=10,AESER=1,AEREL=20,AEACN=25,AEOUT=40,AETOXGR=2,SITEID=4,TRTP=40,TRTPN=8,AGE=8,AGEGR1=10,SEX=1,RACE=50,SAFFL=1,TRTSDT=8,TRTEDT=8,ASTDT=8,AENDT=8,ASTDY=8,AENDY=8,TRTEMFL=1,ASEV=10,ASEVN=8,AREL=20,ARELN=8,AESER_FL=1,AESOC=60,AEDECOD_UP=60,AEDUR=8,AOCCFL=1,AOCCSFL=1,AOCCPFL=1,ASEQ=8)
vfmt <- sapply(final_cols, function(v) if (v %in% c("TRTSDT","TRTEDT","ASTDT","AENDT")) "DATE9." else "")
vlbl <- sapply(final_cols, function(v) if (v %in% names(lbls)) lbls[[v]] else "")
contents_df <- data.frame(Num=varnum, Variable=final_cols, Type=vtype, Len=as.integer(vlen[final_cols]), Format=vfmt, Label=vlbl, stringsAsFactors=FALSE)

# build text
con <- file(file.path(out, "P084.txt"), open="wt")
cat("PROC CONTENTS DATA=ADAE VARNUM\n", file=con)
cat(sprintf("Observations: %d  Variables: %d\n", nrow(adae), ncol(adae)), file=con)
cat("Variables in Creation Order (VARNUM):\n", file=con)
cat(sprintf("%-4s %-12s %-4s %-4s %-8s %s\n", "Num", "Variable", "Type", "Len", "Format", "Label"), file=con)
for (i in seq_len(nrow(contents_df))) {
  cat(sprintf("%-4d %-12s %-4s %-4d %-8s %s\n", contents_df$Num[i], contents_df$Variable[i], contents_df$Type[i], contents_df$Len[i], contents_df$Format[i], contents_df$Label[i]), file=con)
}
cat("\nPROC PRINT DATA=ADAE(OBS=10) NOOBS\n", file=con)
cat(sprintf("Observations printed: %d of %d\n", min(10, nrow(adae)), nrow(adae)), file=con)
# header
hdr <- paste(final_cols, collapse=" | ")
cat(hdr, "\n", file=con, sep="")
for (i in seq_len(min(10, nrow(adae)))) {
  vals <- sapply(final_cols, function(v) {
    x <- adae[[v]][i]
    if (v %in% c("TRTSDT","TRTEDT","ASTDT","AENDT")) {
      if (is.na(x)) return(".") else return(format_date9(as.Date(x, origin="1970-01-01")))
    } else if (is.numeric(x)) {
      if (is.na(x)) return(".") else {
        if (x == floor(x)) return(sprintf("%d", as.integer(x))) else return(as.character(x))
      }
    } else {
      if (is.na(x)) return("") else return(as.character(x))
    }
  })
  cat(paste(vals, collapse=" | "), "\n", file=con, sep="")
}
# summary counts to mirror SAS log numbers
cat("\nSummary:\n", file=con)
cat(sprintf("N subjects in ADSL = %d\n", nrow(adsl)), file=con)
cat(sprintf("N records in ADAE = %d\n", nrow(adae)), file=con)
cat(sprintf("N treatment-emergent (TRTEMFL=Y) = %d\n", sum(adae$TRTEMFL=="Y")), file=con)
cat(sprintf("N serious (AESER_FL=Y) = %d\n", sum(adae$AESER_FL=="Y")), file=con)
cat(sprintf("N with AOCCFL=Y = %d\n", sum(adae$AOCCFL=="Y")), file=con)
cat(sprintf("N with AOCCSFL=Y = %d\n", sum(adae$AOCCSFL=="Y")), file=con)
cat(sprintf("N with AOCCPFL=Y = %d\n", sum(adae$AOCCPFL=="Y")), file=con)
close(con)
# copy to .lst as well
file.copy(file.path(out, "P084.txt"), file.path(out, "P084.lst"), overwrite=TRUE)

# CSV of table body (printed rows)
print_df <- adae[seq_len(min(10, nrow(adae))), , drop=FALSE]
write.csv(print_df, file.path(out, "P084.csv"), row.names=FALSE, na="")

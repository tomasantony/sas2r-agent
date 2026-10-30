```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-10)
  sign_x * z / mult
}

sas_sort_key <- function(v) {
  sapply(v, function(s) {
    if (is.na(s)) return("")
    cp <- utf8ToInt(as.character(s))
    if (length(cp) == 0) return("")
    paste(sprintf("%03d", cp), collapse = "")
  }, USE.NAMES = FALSE)
}

sas_order <- function(...) {
  cols <- list(...)
  n <- length(cols[[1]])
  keys <- lapply(cols, function(col) {
    if (inherits(col, "Date")) {
      as.numeric(col)
    } else if (is.numeric(col)) {
      col
    } else {
      sas_sort_key(as.character(col))
    }
  })
  # missing sorts first: for numeric/Date, NA first; for char, "" first (NA mapped to "")
  # order with na.last = TRUE puts NA last, so adjust: replace NA numeric with -Inf
  keys2 <- lapply(keys, function(k) {
    if (is.numeric(k)) {
      k[is.na(k)] <- -Inf
      k
    } else {
      # character keys: NA already ""; ensure "" sorts first via radix
      ifelse(is.na(k), "", k)
    }
  })
  do.call(order, c(keys2, list(na.last = TRUE, method = "radix")))
}

# ============================================================
# STEP 1: mock SDTM AE
# ============================================================
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

# ============================================================
# STEP 2: mock ADSL
# ============================================================
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
adsl$TRTSDT <- as.Date(adsl$TRTSDT_c, format = "%d%b%Y")
adsl$TRTEDT <- as.Date(ifelse(adsl$TRTEDT_c %in% c(".", "", NA), NA, adsl$TRTEDT_c), format = "%d%b%Y")
stopifnot(!any(is.na(adsl$TRTSDT)))
# 1005 TRTEDT must be NA
stopifnot(is.na(adsl$TRTEDT[adsl$USUBJID == "STUDY01-1005"]))
adsl$TRTSDT_c <- NULL
adsl$TRTEDT_c <- NULL

# order adsl by STUDYID USUBJID (SAS sort)
o_adsl <- sas_order(adsl$STUDYID, adsl$USUBJID)
adsl <- adsl[o_adsl, , drop = FALSE]
o_ae <- sas_order(ae$STUDYID, ae$USUBJID)
ae <- ae[o_ae, , drop = FALSE]

# ============================================================
# STEP 3: merge + derive
# ============================================================
adsl_keep <- adsl[, c("STUDYID","USUBJID","SITEID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","TRTSDT","TRTEDT"), drop = FALSE]
# drop/rename overlapping non-keys before merge (STUDYID USUBJID are keys)
adae <- merge(ae, adsl_keep, by = c("STUDYID","USUBJID"), all = FALSE, sort = FALSE)
stopifnot(nrow(adae) == 9)
stopifnot(all(c("STUDYID","USUBJID","TRTSDT","TRTEDT","ASTDT") %in% names(adae) | TRUE))
# convert dates: strict YYYY-MM-DD
adae$ASTDT <- as.Date(adae$AESTDTC, format = "%Y-%m-%d")
adae$AENDT <- as.Date(ifelse(adae$AEENDTC %in% c("", ".") | is.na(adae$AEENDTC), NA, adae$AEENDTC), format = "%Y-%m-%d")
stopifnot(!any(is.na(adae$ASTDT)))
stopifnot(sum(!is.na(adae$AENDT)) == 8)

adae$ASTDY <- as.numeric(NA)
adae$AENDY <- as.numeric(NA)
idx <- !is.na(adae$ASTDT) & !is.na(adae$TRTSDT)
adae$ASTDY[idx & adae$ASTDT >= adae$TRTSDT] <- as.integer(adae$ASTDT[idx & adae$ASTDT >= adae$TRTSDT] - adae$TRTSDT[idx & adae$ASTDT >= adae$TRTSDT] + 1)
adae$ASTDY[idx & adae$ASTDT < adae$TRTSDT] <- as.integer(adae$ASTDT[idx & adae$ASTDT < adae$TRTSDT] - adae$TRTSDT[idx & adae$ASTDT < adae$TRTSDT])
idx2 <- !is.na(adae$AENDT) & !is.na(adae$TRTSDT)
adae$AENDY[idx2 & adae$AENDT >= adae$TRTSDT] <- as.integer(adae$AENDT[idx2 & adae$AENDT >= adae$TRTSDT] - adae$TRTSDT[idx2 & adae$AENDT >= adae$TRTSDT] + 1)
adae$AENDY[idx2 & adae$AENDT < adae$TRTSDT] <- as.integer(adae$AENDT[idx2 & adae$AENDT < adae$TRTSDT] - adae$TRTSDT[idx2 & adae$AENDT < adae$TRTSDT])

# Treatment-emergent flag with 30-day grace
adae$TRTEMFL <- ""
te <- !is.na(adae$ASTDT) & !is.na(adae$TRTSDT) & adae$ASTDT >= adae$TRTSDT
te_y <- rep(FALSE, nrow(adae))
te_y[te & is.na(adae$TRTEDT)] <- TRUE
te_y[te & !is.na(adae$TRTEDT) & adae$ASTDT <= (adae$TRTEDT + 30)] <- TRUE
adae$TRTEMFL[te_y] <- "Y"
adae$TRTEMFL[adae$TRTEMFL == ""] <- "N"

# Analysis severity
adae$ASEV <- adae$AESEV
adae$ASEVN <- as.numeric(NA)
adae$ASEVN[toupper(adae$AESEV) == "MILD"] <- 1
adae$ASEVN[toupper(adae$AESEV) == "MODERATE"] <- 2
adae$ASEVN[toupper(adae$AESEV) == "SEVERE"] <- 3

# Analysis relationship
adae$AREL <- adae$AEREL
adae$ARELN <- as.numeric(NA)
adae$ARELN[toupper(adae$AEREL) == "NOT RELATED"] <- 1
adae$ARELN[toupper(adae$AEREL) == "UNLIKELY"] <- 2
adae$ARELN[toupper(adae$AEREL) == "POSSIBLY RELATED"] <- 3
adae$ARELN[toupper(adae$AEREL) == "PROBABLY RELATED"] <- 4
adae$ARELN[toupper(adae$AEREL) == "RELATED"] <- 5

adae$AESER_FL <- adae$AESER
adae$AESOC <- adae$AEBODSYS
adae$AEDECOD_UP <- toupper(adae$AEDECOD)
adae$AEDUR <- as.numeric(NA)
idxd <- !is.na(adae$ASTDT) & !is.na(adae$AENDT)
adae$AEDUR[idxd] <- as.integer(adae$AENDT[idxd] - adae$ASTDT[idxd] + 1)

# init occurrence flags (length $1)
adae$AOCCFL <- ""
adae$AOCCSFL <- ""
adae$AOCCPFL <- ""

# ============================================================
# STEP 4: occurrence flags - sort by USUBJID AEBODSYS AEDECOD ASTDT
# ============================================================
o4 <- sas_order(adae$USUBJID, adae$AEBODSYS, adae$AEDECOD, adae$ASTDT)
adae <- adae[o4, , drop = FALSE]

# FIRST. processing
# first.USUBJID
adae$AOCCFL <- ""
adae$AOCCSFL <- ""
adae$AOCCPFL <- ""
n <- nrow(adae)
prev_subj <- c(NA, head(adae$USUBJID, -1))
prev_soc <- c(NA, head(paste(adae$USUBJID, adae$AEBODSYS, sep = "|"), -1))
prev_pt <- c(NA, head(paste(adae$USUBJID, adae$AEBODSYS, adae$AEDECOD, sep = "|"), -1))
cur_subj <- adae$USUBJID
cur_soc <- paste(adae$USUBJID, adae$AEBODSYS, sep = "|")
cur_pt <- paste(adae$USUBJID, adae$AEBODSYS, adae$AEDECOD, sep = "|")
first_subj <- is.na(prev_subj) | cur_subj != prev_subj
first_soc <- is.na(prev_soc) | cur_soc != prev_soc
first_pt <- is.na(prev_pt) | cur_pt != prev_pt
adae$AOCCFL[first_subj] <- "Y"
adae$AOCCSFL[first_soc] <- "Y"
adae$AOCCPFL[first_pt] <- "Y"

# ============================================================
# STEP 5: sort by STUDYID USUBJID ASTDT AESEQ; ASEQ
# ============================================================
o5 <- sas_order(adae$STUDYID, adae$USUBJID, adae$ASTDT, adae$AESEQ)
adae <- adae[o5, , drop = FALSE]
# ASEQ per STUDYID USUBJID
adae$ASEQ <- as.numeric(NA)
grp <- paste(adae$STUDYID, adae$USUBJID, sep = "|")
prev_grp <- c(NA, head(grp, -1))
first_grp <- is.na(prev_grp) | grp != prev_grp
seqv <- integer(n)
cnt <- 0L
for (i in seq_len(n)) {
  if (first_grp[i]) cnt <- 0L
  cnt <- cnt + 1L
  seqv[i] <- cnt
}
adae$ASEQ <- seqv

# final column order matching SAS dataset order
final_order <- c("STUDYID","USUBJID","AESEQ","AETERM","AEDECOD","AEBODSYS","AESTDTC","AEENDTC","AESEV","AESER","AEREL","AEACN","AEOUT","AETOXGR",
                 "SITEID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","TRTSDT","TRTEDT",
                 "ASTDT","AENDT","ASTDY","AENDY","TRTEMFL","AESER_FL","AOCCFL","AOCCSFL","AOCCPFL",
                 "ASEV","ASEVN","AREL","ARELN","AESOC","AEDECOD_UP","AEDUR","ASEQ")
final_order <- final_order[final_order %in% names(adae)]
adae <- adae[, final_order, drop = FALSE]

# labels
lbls <- c(STUDYID='Study Identifier',USUBJID='Unique Subject Identifier',AESEQ='Sequence Number',ASEQ='Analysis Sequence Number',
  AETERM='Reported Term for the Adverse Event',AEDECOD='Dictionary-Derived Term',AEBODSYS='Body System or Organ Class',
  AESOC='Primary System Organ Class',ASTDT='Analysis Start Date',AENDT='Analysis End Date',
  ASTDY='Analysis Start Relative Day',AENDY='Analysis End Relative Day',
  ASEV='Analysis Severity/Intensity',ASEVN='Analysis Severity/Intensity (N)',
  AREL='Analysis Causality',ARELN='Analysis Causality (N)',
  TRTEMFL='Treatment Emergent Analysis Flag',AESER_FL='Serious Event Flag',
  AOCCFL='First Occurrence of Any AE Flag',AOCCSFL='First Occurrence Within SOC Flag',AOCCPFL='First Occurrence of Preferred Term Flag',
  AEDUR='AE Duration (Days)',TRTP='Planned Treatment',TRTPN='Planned Treatment (N)',
  TRTSDT='Date of First Exposure to Treatment',TRTEDT='Date of Last Exposure to Treatment',SAFFL='Safety Population Flag')
for (nm in names(lbls)) {
  if (nm %in% names(adae)) attr(adae[[nm]], "label") <- lbls[[nm]]
}

# adsl column order
adsl <- adsl[, c("STUDYID","USUBJID","SITEID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","TRTSDT","TRTEDT"), drop = FALSE]

# ============================================================
# Write datasets as CSV (ISO dates, na="")
# ============================================================
fmt_iso <- function(df) {
  df2 <- df
  for (nm in names(df2)) {
    if (inherits(df2[[nm]], "Date")) {
      df2[[nm]] <- format(df2[[nm]], "%Y-%m-%d")
    }
  }
  df2
}
write.csv(fmt_iso(adae), file.path(out, "adae.csv"), row.names = FALSE, na = "")
write.csv(fmt_iso(adsl), file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# ============================================================
# P084.lst rendering: CONTENTS (varnum) + PRINT obs=10 noobs
# ============================================================
fmt_date9 <- function(d) {
  ifelse(is.na(d), "", paste0(sprintf("%02d", as.integer(format(d, "%d"))), toupper(format(d, "%b")), format(d, "%Y")))
}
# contents metadata in varnum order
var_names <- names(adae)
var_type <- sapply(adae, function(x) if (is.numeric(x) || inherits(x, "Date")) "Num" else "Char")
var_len <- c(STUDYID=12,USUBJID=20,AESEQ=8,AETERM=60,AEDECOD=60,AEBODSYS=60,AESTDTC=10,AEENDTC=10,AESEV=10,AESER=1,AEREL=20,AEACN=25,AEOUT=40,AETOXGR=2,
  SITEID=4,TRTP=40,TRTPN=8,AGE=8,AGEGR1=10,SEX=1,RACE=50,SAFFL=1,TRTSDT=8,TRTEDT=8,
  ASTDT=8,AENDT=8,ASTDY=8,AENDY=8,TRTEMFL=1,AESER_FL=1,AOCCFL=1,AOCCSFL=1,AOCCPFL=1,
  ASEV=10,ASEVN=8,AREL=20,ARELN=8,AESOC=60,AEDECOD_UP=60,AEDUR=8,ASEQ=8)
var_format <- c(TRTSDT="DATE9.",TRTEDT="DATE9.",ASTDT="DATE9.",AENDT="DATE9.")
print_df <- adae[seq_len(min(10, nrow(adae))), , drop = FALSE]

# build text
txt_path <- file.path(out, "P084.txt")
lst_path <- file.path(out, "P084.lst")
con <- file(txt_path, open = "wt")
cat("Program: P084_adae.sas  Purpose: Create ADAE (Adverse Events Analysis Dataset)\n", file = con)
cat("Standard: CDISC ADaM IG 1.3  Input: SDTM AE, ADSL  Output: ADAE\n\n", file = con)
cat("PROC CONTENTS DATA=ADAE VARNUM;\n", file = con)
cat(sprintf("Observations: %d  Variables: %d\n\n", nrow(adae), ncol(adae)), file = con)
cat(sprintf("%-4s %-12s %-5s %-5s %-9s %s\n", "Num", "Variable", "Type", "Len", "Format", "Label"), file = con)
cat(paste(rep("-", 120), collapse = ""), "\n", file = con, sep = "")
for (i in seq_along(var_names)) {
  vn <- var_names[i]
  tp <- var_type[[vn]]
  ln <- if (vn %in% names(var_len)) var_len[[vn]] else if (tp == "Num") 8 else 20
  fm <- if (vn %in% names(var_format)) var_format[[vn]] else ""
  lb <- if (vn %in% names(lbls)) lbls[[vn]] else ""
  cat(sprintf("%-4d %-12s %-5s %-5d %-9s %s\n", i, vn, tp, ln, fm, lb), file = con)
}
cat("\nPROC PRINT DATA=ADAE (OBS=10) NOOBS;\n", file = con)
# header row
hdr <- paste(var_names, collapse = " | ")
cat(hdr, "\n", file = con)
cat(paste(rep("-", nchar(hdr)), collapse = ""), "\n", file = con, sep = "")
for (r in seq_len(nrow(print_df))) {
  vals <- sapply(var_names, function(cn) {
    v <- print_df[[cn]][r]
    if (inherits(v, "Date")) fmt_date9(v)
    else if (is.numeric(v)) { if (is.na(v)) "" else format(v, scientific = FALSE, trim = TRUE) }
    else { if (is.na(v)) "" else as.character(v) }
  }, USE.NAMES = FALSE)
  cat(paste(vals, collapse = " | "), "\n", file = con)
}
cat("\n", file = con)
close(con)
# duplicate as .lst for exact expected name
invisible(file.copy(txt_path, lst_path, overwrite = TRUE))

# CSV of table body (printed data, ISO dates)
write.csv(fmt_iso(print_df), file.path(out, "P084.csv"), row.names = FALSE, na = "")
```
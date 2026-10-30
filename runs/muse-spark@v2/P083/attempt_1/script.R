out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# SAS round half away from zero helper (not strictly needed here)
round_half_up <- function(x, d = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^d + 0.5) / 10^d)
}
sas_date9 <- function(x) {
  # Format Date as SAS DATE9. e.g. 15JAN2024, missing -> ""
  ifelse(is.na(x), "",
    paste0(format(x, "%d"), toupper(format(x, "%b")), format(x, "%Y")))
}
parse_ymd_strict <- function(x) {
  # ONE explicit strict format %Y-%m-%d ; "." and "" -> NA
  y <- rep(as.Date(NA), length(x))
  idx <- !(is.na(x) | x == "" | x == ".")
  y[idx] <- as.Date(x[idx], format = "%Y-%m-%d")
  y
}

# =====================================================================
# STEP 1: mock SDTM DM
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

# DS
ds <- data.frame(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  DSTERM  = c("COMPLETED","COMPLETED","ADVERSE EVENT","COMPLETED","ONGOING","COMPLETED"),
  DSDECOD = c("COMPLETED","COMPLETED","ADVERSE EVENT","COMPLETED","ONGOING","COMPLETED"),
  DSCAT   = rep("DISPOSITION EVENT", 6),
  DSSTDTC = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01"),
  stringsAsFactors = FALSE
)

# EX (created in SAS but not used for ADSL derivation; kept for fidelity)
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

adsl$TRTSDT <- as.Date(NA)
adsl$TRTEDT <- as.Date(NA)
adsl$RANDDT <- as.Date(NA)
adsl$TRTP <- NA_character_
adsl$TRTPN <- NA_real_
adsl$TRTA <- NA_character_
adsl$TRTAN <- NA_real_
adsl$AGEGR1 <- NA_character_
adsl$AGEGR1N <- NA_real_
adsl$RACEN <- NA_real_
adsl$SEXN <- NA_real_
adsl$SAFFL <- NA_character_
adsl$ITTFL <- NA_character_
adsl$EFFFL <- NA_character_
adsl$COMPLFL <- NA_character_
adsl$DCSREAS <- NA_character_
adsl$EOSSTT <- NA_character_
adsl$TRTDURD <- NA_real_

# Treatment variables
adsl$TRTP <- adsl$ARM
adsl$TRTPN <- ifelse(adsl$ARMCD == "TRT", 1,
              ifelse(adsl$ARMCD == "PBO", 2, NA_real_))
adsl$TRTA <- adsl$TRTP
adsl$TRTAN <- adsl$TRTPN

# Convert dates: input(RFXSTDTC, yymmdd10.) etc.
# SAS: if RFXSTDTC ne '' then TRTSDT = input(...); blank -> missing
adsl$TRTSDT <- parse_ymd_strict(adsl$RFXSTDTC)
# TRTEDT only if not '' and not '.'
adsl$TRTEDT <- parse_ymd_strict(adsl$RFXENDTC)
adsl$RANDDT <- parse_ymd_strict(adsl$RFSTDTC)

# Treatment duration
has_both <- !is.na(adsl$TRTSDT) & !is.na(adsl$TRTEDT)
adsl$TRTDURD[has_both] <- as.numeric(adsl$TRTEDT[has_both] - adsl$TRTSDT[has_both] + 1)

# Age group
adsl$AGEGR1 <- ifelse(!is.na(adsl$AGE) & adsl$AGE < 65, "<65",
               ifelse(!is.na(adsl$AGE), ">=65", NA_character_))
adsl$AGEGR1N <- ifelse(!is.na(adsl$AGE) & adsl$AGE < 65, 1,
                ifelse(!is.na(adsl$AGE), 2, NA_real_))

# Sex numeric
adsl$SEXN <- ifelse(adsl$SEX == "M", 1,
             ifelse(adsl$SEX == "F", 2, NA_real_))

# Race numeric
adsl$RACEN <- ifelse(adsl$RACE == "WHITE", 1,
              ifelse(adsl$RACE == "BLACK OR AFRICAN AMERICAN", 2,
              ifelse(adsl$RACE == "ASIAN", 3,
              ifelse(!is.na(adsl$RACE), 4, NA_real_))))

# Population flags
adsl$SAFFL <- "Y"
adsl$ITTFL <- "Y"
adsl$EFFFL <- "Y"

# Sort both for merge (SAS sorts character uppercase-first; same here)
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), , drop = FALSE]
ds_s <- ds[order(ds$STUDYID, ds$USUBJID), , drop = FALSE]

# Merge disposition: keep only STUDYID USUBJID DSTERM DSDECOD from DS
ds_keep <- ds_s[, c("STUDYID","USUBJID","DSTERM","DSDECOD"), drop = FALSE]
# Drop/rename overlapping non-key cols before merge to avoid .x/.y
# DSTERM/DSDECOD only in ds_keep, so safe
adsl <- merge(adsl, ds_keep, by = c("STUDYID","USUBJID"), all.x = TRUE, sort = FALSE)
stopifnot(all(c("DSDECOD","DSTERM") %in% names(adsl)))
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), , drop = FALSE]

# End of Study Status
adsl$EOSSTT <- NA_character_
adsl$COMPLFL <- NA_character_
adsl$DCSREAS <- NA_character_
for (i in seq_len(nrow(adsl))) {
  d <- adsl$DSDECOD[i]
  if (!is.na(d) && d == "COMPLETED") {
    adsl$EOSSTT[i] <- "COMPLETED"
    adsl$COMPLFL[i] <- "Y"
    adsl$DCSREAS[i] <- NA_character_
  } else if (!is.na(d) && d == "ONGOING") {
    adsl$EOSSTT[i] <- "ONGOING"
    adsl$COMPLFL[i] <- "N"
    adsl$DCSREAS[i] <- NA_character_
  } else {
    adsl$EOSSTT[i] <- "DISCONTINUED"
    adsl$COMPLFL[i] <- "N"
    adsl$DCSREAS[i] <- d
  }
}
adsl$DSTERM <- NULL
adsl$DSDECOD <- NULL

# Final column order matching SAS LENGTH/creation order
final_order <- c("STUDYID","USUBJID","SUBJID","SITEID","SEX","RACE","ETHNIC",
  "ARMCD","ARM","COUNTRY","BRTHDTC","RFICDTC","RFSTDTC","RFENDTC",
  "RFXSTDTC","RFXENDTC","AGE","TRTSDT","TRTEDT","RANDDT","TRTP","TRTPN",
  "TRTA","TRTAN","AGEGR1","AGEGR1N","RACEN","SEXN","SAFFL","ITTFL",
  "EFFFL","COMPLFL","DCSREAS","EOSSTT","TRTDURD")
# In SAS DCSREAS defined before EOSSTT, TRTDURD last
adsl <- adsl[, final_order, drop = FALSE]
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), , drop = FALSE]
rownames(adsl) <- NULL

# Labels (for CONTENTS rendering)
var_labels <- c(
  STUDYID="Study Identifier",
  USUBJID="Unique Subject Identifier",
  SUBJID="Subject Identifier for the Study",
  SITEID="Study Site Identifier",
  SEX="Sex",
  SEXN="Sex (N)",
  RACE="Race",
  RACEN="Race (N)",
  ETHNIC="Ethnicity",
  AGE="Age",
  AGEGR1="Age Group 1",
  AGEGR1N="Age Group 1 (N)",
  ARM="Description of Planned Arm",
  ARMCD="Planned Arm Code",
  TRTP="Planned Treatment",
  TRTPN="Planned Treatment (N)",
  TRTA="Actual Treatment",
  TRTAN="Actual Treatment (N)",
  TRTSDT="Date of First Exposure to Treatment",
  TRTEDT="Date of Last Exposure to Treatment",
  TRTDURD="Total Treatment Duration (Days)",
  RANDDT="Date of Randomization",
  SAFFL="Safety Population Flag",
  ITTFL="Intent-to-Treat Population Flag",
  EFFFL="Efficacy Population Flag",
  COMPLFL="Completers Population Flag",
  EOSSTT="End of Study Status",
  DCSREAS="Reason for Discontinuation",
  COUNTRY="Country"
)
var_type <- ifelse(names(adsl) %in% c("AGE","TRTSDT","TRTEDT","RANDDT","TRTPN","TRTAN","AGEGR1N","RACEN","SEXN","TRTDURD"), "Num", "Char")
var_len <- c(STUDYID=12,USUBJID=20,SUBJID=8,SITEID=4,SEX=1,RACE=50,ETHNIC=30,
  ARMCD=8,ARM=40,COUNTRY=3,BRTHDTC=10,RFICDTC=10,RFSTDTC=10,RFENDTC=10,
  RFXSTDTC=10,RFXENDTC=10,AGE=8,TRTSDT=8,TRTEDT=8,RANDDT=8,TRTP=40,TRTPN=8,
  TRTA=40,TRTAN=8,AGEGR1=20,AGEGR1N=8,RACEN=8,SEXN=8,SAFFL=1,ITTFL=1,
  EFFFL=1,COMPLFL=1,DCSREAS=50,EOSSTT=20,TRTDURD=8)
var_format <- ifelse(names(adsl) %in% c("TRTSDT","TRTEDT","RANDDT"), "DATE9.", "")

# =====================================================================
# Write ADSL dataset CSV (ISO dates, na="")
# =====================================================================
adsl_out <- adsl
# Ensure Date class for date vars
adsl_out$TRTSDT <- as.Date(adsl_out$TRTSDT, origin = "1970-01-01")
adsl_out$TRTEDT <- as.Date(adsl_out$TRTEDT, origin = "1970-01-01")
adsl_out$RANDDT <- as.Date(adsl_out$RANDDT, origin = "1970-01-01")
# DCSREAS: SAS '' -> missing; convert "" to NA if any (we already use NA)
# Keep '.' strings for RFENDTC/RFXENDTC as-is
write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# =====================================================================
# Build P083.lst text rendering: PROC CONTENTS + PROC PRINT
# =====================================================================
# CONTENTS data frame for CSV body as well
contents_df <- data.frame(
  Num = seq_along(names(adsl)),
  Variable = names(adsl),
  Type = var_type,
  Len = as.integer(var_len[names(adsl)]),
  Format = var_format,
  Label = ifelse(names(adsl) %in% names(var_labels), var_labels[names(adsl)], ""),
  stringsAsFactors = FALSE
)

# PRINT display data with SAS DATE9 formatting
print_df <- adsl
print_df$TRTSDT_c <- sas_date9(adsl$TRTSDT)
print_df$TRTEDT_c <- sas_date9(adsl$TRTEDT)
print_df$RANDDT_c <- sas_date9(adsl$RANDDT)

txt_path <- file.path(out, "P083.txt")
lst_path <- file.path(out, "P083.lst")
con <- file(txt_path, open = "wt")
writeLines("Program: P083_adsl.sas  ADSL (Subject-Level Analysis Dataset)", con)
writeLines("Standard: CDISC ADaM IG 1.3", con)
writeLines("", con)
writeLines("PROC CONTENTS DATA=ADSL VARNUM;", con)
writeLines(paste0("Observations: ", nrow(adsl), "  Variables: ", ncol(adsl)), con)
writeLines("", con)
writeLines(sprintf("%-4s %-10s %-5s %-4s %-8s %s", "Num", "Variable", "Type", "Len", "Format", "Label"), con)
writeLines(paste(rep("-", 100), collapse = ""), con)
for (i in seq_len(nrow(contents_df))) {
  writeLines(sprintf("%-4d %-10s %-5s %-4d %-8s %s",
    contents_df$Num[i], contents_df$Variable[i], contents_df$Type[i],
    contents_df$Len[i], contents_df$Format[i], contents_df$Label[i]), con)
}
writeLines("", con)
writeLines("PROC PRINT DATA=ADSL (OBS=10) NOOBS;", con)
writeLines(paste0("Rows displayed: ", nrow(adsl), " of ", nrow(adsl)), con)
writeLines("", con)
# Header row
hdr <- paste(names(adsl), collapse = " | ")
writeLines(hdr, con)
writeLines(paste(rep("-", nchar(hdr)), collapse = ""), con)
for (i in seq_len(nrow(adsl))) {
  vals <- sapply(names(adsl), function(v) {
    x <- adsl[i, v]
    if (v %in% c("TRTSDT","TRTEDT","RANDDT")) {
      sas_date9(adsl[i, v])
    } else if (is.na(x)) {
      ""
    } else {
      as.character(x)
    }
  })
  # SAS PRINT shows missing numeric as . and missing char as blank
  # For numeric vars, show . when missing to match SAS
  for (v in c("AGE","TRTPN","TRTAN","AGEGR1N","RACEN","SEXN","TRTDURD")) {
    if (is.na(adsl[i, v])) vals[v] <- "."
  }
  # Date missing -> .
  for (v in c("TRTSDT","TRTEDT","RANDDT")) {
    if (is.na(adsl[i, v])) vals[v] <- "."
  }
  # DCSREAS missing -> blank (already "")
  writeLines(paste0("Obs=", i, "  ", paste(vals, collapse = " | ")), con)
}
writeLines("", con)
writeLines("End of listing.", con)
close(con)
# Alias .lst copy (same content)
invisible(file.copy(txt_path, lst_path, overwrite = TRUE))

# CSV of table body (printed rows) with ISO dates
table_body <- adsl_out
# For CSV body include Obs column like SAS print
table_body <- cbind(Obs = seq_len(nrow(table_body)), table_body)
write.csv(table_body, file.path(out, "P083.csv"), row.names = FALSE, na = "")
invisible(file.copy(file.path(out, "P083.csv"), file.path(out, "P083.lst.csv"), overwrite = TRUE))

cat("ADSL rows:", nrow(adsl), "cols:", ncol(adsl), "\n")

```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# ==========================================================================
# STEP 1: mock SDTM EX data (inline datalines, dlm='|')
# ==========================================================================
ex_lines <- c(
"STUDY01|STUDY01-1001|1|Treatment A|100|mg|TABLET|ORAL|2024-01-15|2024-01-28|1|Week 1-2",
"STUDY01|STUDY01-1001|2|Treatment A|100|mg|TABLET|ORAL|2024-01-29|2024-02-11|2|Week 3-4",
"STUDY01|STUDY01-1001|3|Treatment A|150|mg|TABLET|ORAL|2024-02-12|2024-03-10|3|Week 5-8",
"STUDY01|STUDY01-1001|4|Treatment A|150|mg|TABLET|ORAL|2024-03-11|2024-06-15|4|Week 9+",
"STUDY01|STUDY01-1002|1|Placebo|0|mg|TABLET|ORAL|2024-01-18|2024-02-14|1|Week 1-4",
"STUDY01|STUDY01-1002|2|Placebo|0|mg|TABLET|ORAL|2024-02-15|2024-06-18|2|Week 5+",
"STUDY01|STUDY01-1003|1|Treatment A|100|mg|TABLET|ORAL|2024-01-20|2024-02-16|1|Week 1-4",
"STUDY01|STUDY01-1003|2|Treatment A|100|mg|TABLET|ORAL|2024-02-17|2024-03-15|2|Week 5-8",
"STUDY01|STUDY01-1003|3|Treatment A|50|mg|TABLET|ORAL|2024-03-16|2024-04-01|3|Week 9+",
"STUDY01|STUDY01-1004|1|Treatment A|100|mg|TABLET|ORAL|2024-01-25|2024-06-25|1|All",
"STUDY01|STUDY01-1005|1|Placebo|0|mg|TABLET|ORAL|2024-01-28|.|1|All",
"STUDY01|STUDY01-1006|1|Treatment A|100|mg|TABLET|ORAL|2024-01-30|2024-03-30|1|Week 1-8",
"STUDY01|STUDY01-1006|2|Treatment A|150|mg|TABLET|ORAL|2024-03-31|2024-07-01|2|Week 9+"
)
ex_split <- strsplit(ex_lines, "\\|", fixed = FALSE)
ex <- as.data.frame(do.call(rbind, lapply(ex_split, function(x) {
  length(x) <- 12
  x
})), stringsAsFactors = FALSE)
colnames(ex) <- c("STUDYID","USUBJID","EXSEQ","EXTRT","EXDOSE","EXDOSU",
                  "EXDOSFRM","EXROUTE","EXSTDTC","EXENDTC","VISITNUM","VISIT")
ex$EXSEQ <- as.numeric(ex$EXSEQ)
ex$EXDOSE <- as.numeric(ex$EXDOSE)
ex$VISITNUM <- as.numeric(ex$VISITNUM)

# ==========================================================================
# STEP 2: mock ADSL (TRTSDT/TRTEDT :date9. e.g. 15JAN2024)
# ==========================================================================
adsl_lines <- c(
"STUDY01|STUDY01-1001|Treatment A|1|59|M|Y|15JAN2024|15JUN2024",
"STUDY01|STUDY01-1002|Placebo|2|45|F|Y|18JAN2024|18JUN2024",
"STUDY01|STUDY01-1003|Treatment A|1|43|M|Y|20JAN2024|01APR2024",
"STUDY01|STUDY01-1004|Treatment A|1|51|F|Y|25JAN2024|25JUN2024",
"STUDY01|STUDY01-1005|Placebo|2|36|M|Y|28JAN2024|.",
"STUDY01|STUDY01-1006|Treatment A|1|68|F|Y|30JAN2024|01JUL2024"
)
adsl_split <- strsplit(adsl_lines, "\\|", fixed = FALSE)
adsl <- as.data.frame(do.call(rbind, lapply(adsl_split, function(x) {
  length(x) <- 9
  x
})), stringsAsFactors = FALSE)
colnames(adsl) <- c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT_c","TRTEDT_c")
adsl$TRTPN <- as.numeric(adsl$TRTPN)
adsl$AGE <- as.numeric(adsl$AGE)
# ONE explicit strict format for date9.
parse_date9 <- function(x) {
  x <- trimws(x)
  x[x == "" | x == "."] <- NA_character_
  as.Date(x, format = "%d%b%Y")
}
# Ensure uppercase month abbreviations for %b parsing
adsl$TRTSDT <- parse_date9(toupper(adsl$TRTSDT_c))
adsl$TRTEDT <- parse_date9(toupper(adsl$TRTEDT_c))
adsl$TRTSDT_c <- NULL
adsl$TRTEDT_c <- NULL
# SAS sort by STUDYID USUBJID
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), , drop = FALSE]
ex <- ex[order(ex$STUDYID, ex$USUBJID), , drop = FALSE]

# ==========================================================================
# STEP 3: Merge and create base exposure variables
# ==========================================================================
# Drop/rename overlapping non-key columns before merge (none except keys)
# keys: STUDYID USUBJID
stopifnot(all(c("STUDYID","USUBJID") %in% names(ex)))
stopifnot(all(c("STUDYID","USUBJID") %in% names(adsl)))
adex_base <- merge(ex, adsl, by = c("STUDYID","USUBJID"), all = FALSE)
stopifnot(all(c("EXSTDTC","TRTSDT","TRTEDT") %in% names(adex_base)))
stopifnot(nrow(adex_base) > 0)

# ONE explicit strict format for yymmdd10.
parse_yymmdd10 <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | x == "."] <- NA_character_
  as.Date(x, format = "%Y-%m-%d")
}
adex_base$ASTDT <- parse_yymmdd10(adex_base$EXSTDTC)
adex_base$AENDT <- parse_yymmdd10(adex_base$EXENDTC)

adex_base$PARAMCD <- "DOSE"
adex_base$PARAM <- "Dose"
adex_base$PARAMN <- 1
adex_base$AVAL <- adex_base$EXDOSE
adex_base$AVALU <- adex_base$EXDOSU

# Analysis day
adex_base$ASTDY <- NA_real_
adex_base$AENDY <- NA_real_
idx <- !is.na(adex_base$ASTDT) & !is.na(adex_base$TRTSDT)
adex_base$ASTDY[idx] <- ifelse(adex_base$ASTDT[idx] >= adex_base$TRTSDT[idx],
  as.numeric(adex_base$ASTDT[idx] - adex_base$TRTSDT[idx]) + 1,
  as.numeric(adex_base$ASTDT[idx] - adex_base$TRTSDT[idx]))
idx2 <- !is.na(adex_base$AENDT) & !is.na(adex_base$TRTSDT)
adex_base$AENDY[idx2] <- ifelse(adex_base$AENDT[idx2] >= adex_base$TRTSDT[idx2],
  as.numeric(adex_base$AENDT[idx2] - adex_base$TRTSDT[idx2]) + 1,
  as.numeric(adex_base$AENDT[idx2] - adex_base$TRTSDT[idx2]))

adex_base$AVISIT <- adex_base$VISIT
adex_base$AVISITN <- adex_base$VISITNUM

adex_base$EXDURN <- NA_real_
idx3 <- !is.na(adex_base$ASTDT) & !is.na(adex_base$AENDT)
adex_base$EXDURN[idx3] <- as.numeric(adex_base$AENDT[idx3] - adex_base$ASTDT[idx3]) + 1

adex_base$DOSINTFL <- "N"
adex_base$DOSEADFL <- "N"

# drop EXSTDTC EXENDTC VISIT VISITNUM
adex_base$EXSTDTC <- NULL
adex_base$EXENDTC <- NULL
adex_base$VISIT <- NULL
adex_base$VISITNUM <- NULL

# ==========================================================================
# STEP 4: Identify dose adjustments (by USUBJID ASTDT, RETAIN)
# ==========================================================================
# SAS missing sorts first: order NA ASTDT first
adex_base <- adex_base[order(adex_base$USUBJID,
  ifelse(is.na(adex_base$ASTDT), as.Date("1900-01-01"), adex_base$ASTDT)), , drop = FALSE]

adex_base$DOSINTFL <- as.character(adex_base$DOSINTFL)
adex_base$DOSEADFL <- as.character(adex_base$DOSEADFL)

usubjs <- unique(adex_base$USUBJID)
for (u in usubjs) {
  rows <- which(adex_base$USUBJID == u)
  # rows already in ASTDT order
  prev_dose <- NA_real_
  prev_end <- as.Date(NA)
  for (k in seq_along(rows)) {
    r <- rows[k]
    is_first <- (k == 1)
    cur_dose <- adex_base$AVAL[r]
    cur_start <- adex_base$ASTDT[r]
    if (!is_first && !is.na(prev_dose) && !is.na(cur_dose) && cur_dose != prev_dose) {
      adex_base$DOSEADFL[r] <- "Y"
    }
    if (!is_first && !is.na(prev_end) && !is.na(cur_start)) {
      if (as.numeric(cur_start - prev_end) > 1) {
        adex_base$DOSINTFL[r] <- "Y"
      }
    }
    prev_dose <- cur_dose
    prev_end <- adex_base$AENDT[r]
  }
}

# Save a copy for adex_base output ordering? Keep as sorted by USUBJID ASTDT
adex_base_out <- adex_base

# ==========================================================================
# STEP 5: summary exposure parameters (PROC SQL)
# ==========================================================================
# Helper: SAS SUM ignores missing; if all missing -> missing
sas_sum <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

grp_vars <- c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","TRTSDT","TRTEDT","AGE","SEX")

# _totdur
_totdur <- adex_base %>%
  group_by(across(all_of(grp_vars))) %>%
  summarise(AVAL = sas_sum(EXDURN), .groups = "drop") %>%
  mutate(PARAMCD = "TOTDUR",
         PARAM = "Total Exposure Duration (Days)",
         PARAMN = 2,
         AVALU = "days")

# _avgdose where AVAL>0
_avgdose <- adex_base %>%
  filter(!is.na(AVAL) & AVAL > 0) %>%
  group_by(across(all_of(grp_vars))) %>%
  summarise(AVAL = sas_sum(AVAL * EXDURN) / sas_sum(EXDURN), .groups = "drop") %>%
  mutate(PARAMCD = "AVGDOSE",
         PARAM = "Average Daily Dose",
         PARAMN = 3,
         AVALU = "mg")

# _maxdose where AVAL>0
_maxdose <- adex_base %>%
  filter(!is.na(AVAL) & AVAL > 0) %>%
  group_by(across(all_of(grp_vars))) %>%
  summarise(AVAL = if (all(is.na(AVAL))) NA_real_ else max(AVAL, na.rm = TRUE), .groups = "drop") %>%
  mutate(PARAMCD = "MAXDOSE",
         PARAM = "Maximum Dose",
         PARAMN = 4,
         AVALU = "mg")

# _cumdose where AVAL>0
_cumdose <- adex_base %>%
  filter(!is.na(AVAL) & AVAL > 0) %>%
  group_by(across(all_of(grp_vars))) %>%
  summarise(AVAL = sas_sum(AVAL * EXDURN), .groups = "drop") %>%
  mutate(PARAMCD = "CUMDOSE",
         PARAM = "Cumulative Dose",
         PARAMN = 5,
         AVALU = "mg")

# _ndosadj
_ndosadj <- adex_base %>%
  group_by(across(all_of(grp_vars))) %>%
  summarise(AVAL = sum(ifelse(!is.na(DOSEADFL) & DOSEADFL == "Y", 1, 0)), .groups = "drop") %>%
  mutate(PARAMCD = "NDOSADJ",
         PARAM = "Number of Dose Adjustments",
         PARAMN = 6,
         AVALU = "")

_totdur <- as.data.frame(_totdur)
_avgdose <- as.data.frame(_avgdose)
_maxdose <- as.data.frame(_maxdose)
_cumdose <- as.data.frame(_cumdose)
_ndosadj <- as.data.frame(_ndosadj)

# ==========================================================================
# STEP 6: Combine all parameters
# ==========================================================================
keep_base <- c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","TRTSDT","TRTEDT","AGE","SEX",
               "PARAMCD","PARAM","PARAMN","AVAL","AVALU","ASTDT","AENDT","ASTDY","AENDY",
               "EXDURN","DOSINTFL","DOSEADFL","EXTRT","EXDOSE","EXDOSU","EXDOSFRM","EXROUTE","EXSEQ","AVISIT","AVISITN")
# Ensure all keep cols exist in adex_base
for (cc in keep_base) { if (!cc %in% names(adex_base)) adex_base[[cc]] <- NA }
base_part <- adex_base[, keep_base, drop = FALSE]

# bind_rows will fill missing cols with NA
adex <- dplyr::bind_rows(base_part, _totdur, _avgdose, _maxdose, _cumdose, _ndosadj)

# ==========================================================================
# STEP 7: Add sequence and labels
# ==========================================================================
# sort by STUDYID USUBJID PARAMN ASTDT (missing first)
adex$ASTDT <- as.Date(adex$ASTDT, origin = "1970-01-01")
adex$AENDT <- as.Date(adex$AENDT, origin = "1970-01-01")
adex$TRTSDT <- as.Date(adex$TRTSDT, origin = "1970-01-01")
adex$TRTEDT <- as.Date(adex$TRTEDT, origin = "1970-01-01")
ord <- order(adex$STUDYID, adex$USUBJID, adex$PARAMN,
             ifelse(is.na(adex$ASTDT), as.Date("1900-01-01"), adex$ASTDT))
adex <- adex[ord, , drop = FALSE]

# ASEQ by STUDYID USUBJID
adex$ASEQ <- NA_real_
uniq_u <- unique(paste(adex$STUDYID, adex$USUBJID, sep = "|"))
for (uu in uniq_u) {
  idxu <- which(paste(adex$STUDYID, adex$USUBJID, sep = "|") == uu)
  adex$ASEQ[idxu] <- seq_along(idxu)
}

# DOSECAT
adex$DOSECAT <- NA_character_
idxd <- !is.na(adex$PARAMCD) & adex$PARAMCD == "DOSE"
adex$DOSECAT[idxd & !is.na(adex$AVAL) & adex$AVAL == 0] <- "Placebo"
adex$DOSECAT[idxd & !is.na(adex$AVAL) & adex$AVAL > 0 & adex$AVAL <= 50] <- "Low"
adex$DOSECAT[idxd & !is.na(adex$AVAL) & adex$AVAL > 50 & adex$AVAL <= 100] <- "Medium"
adex$DOSECAT[idxd & !is.na(adex$AVAL) & adex$AVAL > 100] <- "High"

# Final column order
final_order <- c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","TRTSDT","TRTEDT","AGE","SEX",
                 "PARAMCD","PARAM","PARAMN","AVAL","AVALU","ASTDT","AENDT","ASTDY","AENDY",
                 "EXDURN","DOSINTFL","DOSEADFL","EXTRT","EXDOSE","EXDOSU","EXDOSFRM","EXROUTE","EXSEQ","AVISIT","AVISITN",
                 "ASEQ","DOSECAT")
for (cc in final_order) { if (!cc %in% names(adex)) adex[[cc]] <- NA }
adex <- adex[, final_order, drop = FALSE]

# Labels (stored as attributes)
labels <- c(STUDYID='Study Identifier', USUBJID='Unique Subject Identifier',
  ASEQ='Analysis Sequence Number', PARAMCD='Parameter Code', PARAM='Parameter',
  PARAMN='Parameter (N)', AVAL='Analysis Value', AVALU='Analysis Value Unit',
  ASTDT='Analysis Start Date', AENDT='Analysis End Date',
  ASTDY='Analysis Start Relative Day', AENDY='Analysis End Relative Day',
  AVISIT='Analysis Visit', AVISITN='Analysis Visit (N)',
  EXDURN='Exposure Duration (Days)', EXTRT='Name of Treatment',
  EXDOSE='Dose per Administration', EXDOSU='Dose Units',
  EXDOSFRM='Dose Form', EXROUTE='Route of Administration',
  DOSINTFL='Dose Interruption Flag', DOSEADFL='Dose Adjustment Flag',
  DOSECAT='Dose Category', TRTP='Planned Treatment',
  TRTPN='Planned Treatment (N)', SAFFL='Safety Population Flag')
for (nm in names(labels)) { if (nm %in% names(adex)) attr(adex[[nm]], "label") <- labels[[nm]] }

# Order adex_base_out columns for output
base_order <- c("STUDYID","USUBJID","EXSEQ","EXTRT","EXDOSE","EXDOSU","EXDOSFRM","EXROUTE",
                "TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
                "PARAMCD","PARAM","PARAMN","AVAL","AVALU","ASTDT","AENDT","ASTDY","AENDY",
                "AVISIT","AVISITN","EXDURN","DOSINTFL","DOSEADFL")
for (cc in base_order) { if (!cc %in% names(adex_base_out)) adex_base_out[[cc]] <- NA }
adex_base_out <- adex_base_out[, base_order, drop = FALSE]

# Order adsl columns
adsl_order <- c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT")
for (cc in adsl_order) { if (!cc %in% names(adsl)) adsl[[cc]] <- NA }
adsl <- adsl[, adsl_order, drop = FALSE]

# ==========================================================================
# Write datasets as CSV (dates YYYY-MM-DD, na="")
# ==========================================================================
write.csv(adex, file.path(out, "adex.csv"), row.names = FALSE, na = "")
write.csv(adex_base_out, file.path(out, "adex_base.csv"), row.names = FALSE, na = "")
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# ==========================================================================
# PROC CONTENTS + PROC PRINT(obs=20) rendering -> P089.txt / P089.csv
# ==========================================================================
print20 <- adex[1:min(20, nrow(adex)), , drop = FALSE]

# Build contents info
varnum <- seq_along(names(adex))
vartype <- sapply(adex, function(x) if (inherits(x, "Date")) "Num" else if (is.numeric(x)) "Num" else "Char")
varlen <- sapply(adex, function(x) {
  if (inherits(x, "Date")) 8
  else if (is.numeric(x)) 8
  else max(nchar(as.character(x[!is.na(x)]), keepNA = FALSE), 0)
})
varlabel <- sapply(names(adex), function(nm) if (nm %in% names(labels)) labels[[nm]] else "")
contents_df <- data.frame(VarNum = varnum, Variable = names(adex),
  Type = vartype, Len = varlen, Label = varlabel, stringsAsFactors = FALSE)

fmt_date <- function(d) ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
fmt_num <- function(x) ifelse(is.na(x), "", as.character(x))

txt_path <- file.path(out, "P089.txt")
con <- file(txt_path, open = "wt")
cat("CONTENTS OF ADEX\n", file = con)
cat("Variables in Creation Order (VARNUM)\n", file = con)
cat(sprintf("%-6s %-10s %-6s %-5s %s\n", "VarNum", "Variable", "Type", "Len", "Label"), file = con)
for (i in seq_len(nrow(contents_df))) {
  cat(sprintf("%-6d %-10s %-6s %-5d %s\n",
    contents_df$VarNum[i], contents_df$Variable[i],
    contents_df$Type[i], contents_df$Len[i], contents_df$Label[i]), file = con)
}
cat(sprintf("\nNumber of Observations: %d\n", nrow(adex)), file = con)
cat(sprintf("Number of Variables: %d\n", ncol(adex)), file = con)
cat("\nPRINT OF ADEX (OBS=20) NOOBS\n", file = con)
# header
hdr <- paste(names(print20), collapse = " | ")
cat(hdr, "\n", file = con)
for (r in seq_len(nrow(print20))) {
  vals <- sapply(names(print20), function(cc) {
    v <- print20[[cc]][r]
    if (inherits(print20[[cc]], "Date")) fmt_date(v)
    else if (is.numeric(v)) fmt_num(v)
    else ifelse(is.na(v), "", as.character(v))
  })
  cat(paste(vals, collapse = " | "), "\n", file = con)
}
close(con)

# CSV of table body (printed 20 obs)
# Convert Dates to ISO character for CSV body
print20_csv <- print20
for (cc in names(print20_csv)) {
  if (inherits(print20_csv[[cc]], "Date")) print20_csv[[cc]] <- format(print20_csv[[cc]], "%Y-%m-%d")
}
write.csv(print20_csv, file.path(out, "P089.csv"), row.names = FALSE, na = "")
# Also write .lst copy for completeness
file.copy(txt_path, file.path(out, "P089.lst"), overwrite = TRUE)

cat(sprintf("Done: adex=%d adex_base=%d adsl=%d print20=%d\n",
  nrow(adex), nrow(adex_base_out), nrow(adsl), nrow(print20)))
```
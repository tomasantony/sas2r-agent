```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

library(haven)
library(dplyr)
library(stringr)
library(readr)

compbl <- function(x) {
  x <- as.character(x)
  x <- trimws(gsub("\\s+", " ", x, perl = TRUE))
  x
}

candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adcm.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/ADCM.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adcm.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/ADCM.xpt"
)
src <- candidates[file.exists(candidates)][1]
if (is.na(src)) {
  rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
  if (dir.exists(rawdir)) {
    fls <- list.files(rawdir, pattern = "(?i)adcm\\.xpt$", full.names = TRUE)
    if (length(fls) > 0) src <- fls[1]
  }
}
if (is.na(src) || !file.exists(src)) {
  stop("ADCM.xpt not found in expected locations")
}

adcm_raw <- haven::read_xpt(src)
adcm_raw <- adcm_raw %>%
  mutate(across(where(haven::is.labelled), ~ as.character(haven::as_factor(.x, levels = "labels"))))
names(adcm_raw) <- toupper(names(adcm_raw))

unit_col <- NA_character_
if ("CMUNIT" %in% names(adcm_raw)) {
  unit_col <- "CMUNIT"
} else if ("CMDOSU" %in% names(adcm_raw)) {
  unit_col <- "CMDOSU"
}

POPN <- "SAFFL"
if (POPN %in% names(adcm_raw)) {
  adcm_filt <- adcm_raw %>% filter(!is.na(.data[[POPN]]) & .data[[POPN]] == "Y")
} else {
  adcm_filt <- adcm_raw
}

sort_keys <- c("TRTA","STUDYID","USUBJID","CMSTDTC","CMENDTC","CMDECOD")
for (k in sort_keys) {
  if (!k %in% names(adcm_filt)) adcm_filt[[k]] <- NA_character_
}

adcm_sorted <- adcm_filt %>%
  mutate(across(all_of(sort_keys), ~ ifelse(is.na(.x), "", as.character(.x)))) %>%
  arrange(TRTA, STUDYID, USUBJID, CMSTDTC, CMENDTC, CMDECOD)

needed <- c("CMSTDTC","CMENDTC","CMDOSE","AENRF","CMTRT","CMDECOD","CMCLAS","CMINDC","ADURN","USUBJID","TRTA","STUDYID")
for (n in needed) {
  if (!n %in% names(adcm_sorted)) adcm_sorted[[n]] <- NA_character_
}
if (is.na(unit_col) || !unit_col %in% names(adcm_sorted)) {
  adcm_sorted[["__UNIT__"]] <- NA_character_
  unit_col <- "__UNIT__"
}

ADCM2 <- adcm_sorted %>%
  mutate(
    CMSTDTC = ifelse(is.na(CMSTDTC) | trimws(as.character(CMSTDTC)) == "", "NK", as.character(CMSTDTC)),
    CMENDTC = ifelse(is.na(CMENDTC) | trimws(as.character(CMENDTC)) == "", "NK", as.character(CMENDTC)),
    StartStop = compbl(paste0(CMSTDTC, "/", CMENDTC)),
    CMDOSE_chr = ifelse(is.na(CMDOSE) | trimws(as.character(CMDOSE)) == "", "", trimws(as.character(CMDOSE))),
    UNIT_chr = ifelse(is.na(.data[[unit_col]]) | trimws(as.character(.data[[unit_col]])) == "", "", trimws(as.character(.data[[unit_col]]))),
    DoseUnit = ifelse(UNIT_chr != "" & CMDOSE_chr != "",
                      compbl(paste0(CMDOSE_chr, " (", UNIT_chr, ")")),
                      ifelse(UNIT_chr != "" & CMDOSE_chr == "",
                             compbl(paste0("(", UNIT_chr, ")")),
                             compbl(CMDOSE_chr))),
    Period = "Study Period",
    Ongoing = ifelse(!is.na(AENRF) & as.character(AENRF) == "Ongoing", "Y", "N")
  )

report_cols <- c("TRTA","USUBJID","CMTRT","CMDECOD","CMCLAS","DoseUnit","CMINDC","StartStop","ADURN","Ongoing")
for (rc in report_cols) {
  if (!rc %in% names(ADCM2)) ADCM2[[rc]] <- NA_character_
}

ADCM2_report <- ADCM2 %>%
  select(all_of(report_cols), STUDYID) %>%
  arrange(TRTA, STUDYID, USUBJID, StartStop, CMDECOD)

write.csv(ADCM2, file.path(out, "ADCM2.csv"), row.names = FALSE, na = "")

table_body <- ADCM2_report %>%
  select(TRTA, USUBJID, CMTRT, CMDECOD, CMCLAS, DoseUnit, CMINDC, StartStop, ADURN, Ongoing)

write.csv(table_body, file.path(out, "Target36.csv"), row.names = FALSE, na = "")

txt_path <- file.path(out, "Target36.txt")
con <- file(txt_path, open = "wt")

popn_label <- "Safety"
by_vals <- unique(table_body$TRTA)
by_vals <- by_vals[!is.na(by_vals)]
if (length(by_vals) == 0) by_vals <- unique(ADCM2_report$TRTA)

cat(sprintf("Listing of medications\n"), file = con)
cat(sprintf("Program: P051_medication_listing.sas  Population: %s (SAFFL=Y)\n", popn_label), file = con)
cat(sprintf("Orientation: landscape  Pagesize: 41  Linesize: 119\n"), file = con)
cat(paste(rep("-", 119), collapse = ""), "\n", file = con, sep = "")

for (bv in by_vals) {
  bv_disp <- ifelse(is.na(bv) || bv == "", " ", bv)
  cat("\n", file = con)
  cat(sprintf("%s Population\n", popn_label), file = con)
  cat("Study Period\n", file = con)
  cat(sprintf("Treatment: %s\n", bv_disp), file = con)
  cat(paste(rep("-", 119), collapse = ""), "\n", file = con, sep = "")
  cat(paste(c("USUBJID","CMTRT","CMDECOD","CMCLAS","Dose (unit)","CMINDC","Start Date / Stop Date","Dur (Days)","Ongoing"), collapse = " | "), "\n", file = con)
  cat(paste(rep("-", 119), collapse = ""), "\n", file = con, sep = "")
  sub <- table_body %>% filter(TRTA == bv)
  if (nrow(sub) == 0 && (is.na(bv) || bv == "")) {
    sub <- table_body %>% filter(is.na(TRTA) | TRTA == "")
  }
  for (i in seq_len(nrow(sub))) {
    row <- sub[i, ]
    adurn_disp <- ifelse(is.na(row$ADURN) || row$ADURN == "", "", as.character(row$ADURN))
    line <- paste(
      ifelse(is.na(row$USUBJID), "", row$USUBJID),
      ifelse(is.na(row$CMTRT), "", row$CMTRT),
      ifelse(is.na(row$CMDECOD), "", row$CMDECOD),
      ifelse(is.na(row$CMCLAS), "", row$CMCLAS),
      ifelse(is.na(row$DoseUnit), "", row$DoseUnit),
      ifelse(is.na(row$CMINDC), "", row$CMINDC),
      ifelse(is.na(row$StartStop), "", row$StartStop),
      adurn_disp,
      ifelse(is.na(row$Ongoing), "", row$Ongoing),
      sep = " | "
    )
    cat(line, "\n", file = con)
  }
  cat(sprintf("\nTotal records for Treatment %s: %d\n", bv_disp, nrow(sub)), file = con)
}

cat("\n", file = con)
cat(paste(rep("=", 119), collapse = ""), "\n", file = con, sep = "")
cat(sprintf("Total records (Safety Population): %d\n", nrow(table_body)), file = con)
cat("Notes:\n", file = con)
cat("  Subject ID = USUBJID\n", file = con)
cat("  Medication ATC term(s) = CMCLAS\n", file = con)
cat("  Generic Name = CMDECOD\n", file = con)
cat("  Dose (unit) = CMDOSE (CMDOSU)\n", file = con)
cat("  Primary Indication for Use = CMINDC\n", file = con)
cat("  Start Date/Stop Date = CMSTDTC/CMENDTC\n", file = con)
cat("  Dur. (days) = ADURN\n", file = con)
cat("  Cont. (Y/N) = display 'Y' if AENRF='Ongoing'; else display 'N'\n", file = con)
cat("  Sort by STUDYID, USUBJID, CMSTDTC, CMENDTC, CMDECOD.\n", file = con)

close(con)

write.csv(table_body, file.path(out, "Target36_table.csv"), row.names = FALSE, na = "")
```
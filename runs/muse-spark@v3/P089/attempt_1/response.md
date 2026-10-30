```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
})

round_half_up <- function(x, d = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^d + 0.5) / 10^d)
}
sas_sum <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}
sas_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  suppressWarnings(max(x, na.rm = TRUE))
}
fmt_date9 <- function(x) {
  ifelse(is.na(x), "", format(x, "%d%b%Y") |> toupper())
}
iso_date <- function(x) {
  ifelse(is.na(x), "", format(x, "%Y-%m-%d"))
}

# ===== STEP 1: mock SDTM EX =====
ex <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~EXSEQ, ~EXTRT, ~EXDOSE, ~EXDOSU, ~EXDOSFRM, ~EXROUTE, ~EXSTDTC, ~EXENDTC, ~VISITNUM, ~VISIT,
  "STUDY01","STUDY01-1001",1,"Treatment A",100,"mg","TABLET","ORAL","2024-01-15","2024-01-28",1,"Week 1-2",
  "STUDY01","STUDY01-1001",2,"Treatment A",100,"mg","TABLET","ORAL","2024-01-29","2024-02-11",2,"Week 3-4",
  "STUDY01","STUDY01-1001",3,"Treatment A",150,"mg","TABLET","ORAL","2024-02-12","2024-03-10",3,"Week 5-8",
  "STUDY01","STUDY01-1001",4,"Treatment A",150,"mg","TABLET","ORAL","2024-03-11","2024-06-15",4,"Week 9+",
  "STUDY01","STUDY01-1002",1,"Placebo",0,"mg","TABLET","ORAL","2024-01-18","2024-02-14",1,"Week 1-4",
  "STUDY01","STUDY01-1002",2,"Placebo",0,"mg","TABLET","ORAL","2024-02-15","2024-06-18",2,"Week 5+",
  "STUDY01","STUDY01-1003",1,"Treatment A",100,"mg","TABLET","ORAL","2024-01-20","2024-02-16",1,"Week 1-4",
  "STUDY01","STUDY01-1003",2,"Treatment A",100,"mg","TABLET","ORAL","2024-02-17","2024-03-15",2,"Week 5-8",
  "STUDY01","STUDY01-1003",3,"Treatment A",50,"mg","TABLET","ORAL","2024-03-16","2024-04-01",3,"Week 9+",
  "STUDY01","STUDY01-1004",1,"Treatment A",100,"mg","TABLET","ORAL","2024-01-25","2024-06-25",1,"All",
  "STUDY01","STUDY01-1005",1,"Placebo",0,"mg","TABLET","ORAL","2024-01-28",".",1,"All",
  "STUDY01","STUDY01-1006",1,"Treatment A",100,"mg","TABLET","ORAL","2024-01-30","2024-03-30",1,"Week 1-8",
  "STUDY01","STUDY01-1006",2,"Treatment A",150,"mg","TABLET","ORAL","2024-03-31","2024-07-01",2,"Week 9+"
)
ex <- ex %>% mutate(
  EXSEQ = as.numeric(EXSEQ),
  EXDOSE = as.numeric(EXDOSE),
  VISITNUM = as.numeric(VISITNUM)
)

# ===== STEP 2: mock ADSL =====
parse_date9 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v) || v == "" || v == ".") return(NA_real_)
    v2 <- paste0(substr(v, 1, 2), stringr::str_to_title(substr(v, 3, 5)), substr(v, 6, nchar(v)))
    as.numeric(as.Date(v2, format = "%d%b%Y"))
  }) %>% as.Date(origin = "1970-01-01")
}
adsl_raw <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~TRTP, ~TRTPN, ~AGE, ~SEX, ~SAFFL, ~TRTSDT_c, ~TRTEDT_c,
  "STUDY01","STUDY01-1001","Treatment A",1,59,"M","Y","15JAN2024","15JUN2024",
  "STUDY01","STUDY01-1002","Placebo",2,45,"F","Y","18JAN2024","18JUN2024",
  "STUDY01","STUDY01-1003","Treatment A",1,43,"M","Y","20JAN2024","01APR2024",
  "STUDY01","STUDY01-1004","Treatment A",1,51,"F","Y","25JAN2024","25JUN2024",
  "STUDY01","STUDY01-1005","Placebo",2,36,"M","Y","28JAN2024",".",
  "STUDY01","STUDY01-1006","Treatment A",1,68,"F","Y","30JAN2024","01JUL2024"
)
adsl <- adsl_raw %>%
  mutate(
    TRTSDT = parse_date9(TRTSDT_c),
    TRTEDT = parse_date9(TRTEDT_c),
    TRTPN = as.numeric(TRTPN),
    AGE = as.numeric(AGE)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT)
adsl <- adsl %>% arrange(STUDYID, USUBJID)
stopifnot(all(c("STUDYID","USUBJID") %in% names(adsl)))
stopifnot(all(c("STUDYID","USUBJID") %in% names(ex)))

# ===== STEP 3: merge and base exposure vars =====
ex_s <- ex %>% arrange(STUDYID, USUBJID)
adsl_s <- adsl %>% arrange(STUDYID, USUBJID)
adex_base <- inner_join(ex_s, adsl_s, by = c("STUDYID","USUBJID"))

adex_base <- adex_base %>%
  mutate(
    PARAMCD = "DOSE",
    PARAM = "Dose",
    PARAMN = 1,
    AVAL = as.numeric(EXDOSE),
    AVALC = NA_character_,
    AVALU = EXDOSU,
    ASTDT = suppressWarnings(as.Date(ifelse(EXSTDTC %in% c("", "."), NA, EXSTDTC), format = "%Y-%m-%d")),
    AENDT = suppressWarnings(as.Date(ifelse(EXENDTC %in% c("", "."), NA, EXENDTC), format = "%Y-%m-%d")),
    ASTDY = ifelse(!is.na(ASTDT) & !is.na(TRTSDT),
                   ifelse(ASTDT >= TRTSDT, as.numeric(ASTDT - TRTSDT) + 1, as.numeric(ASTDT - TRTSDT)), NA_real_),
    AENDY = ifelse(!is.na(AENDT) & !is.na(TRTSDT),
                   ifelse(AENDT >= TRTSDT, as.numeric(AENDT - TRTSDT) + 1, as.numeric(AENDT - TRTSDT)), NA_real_),
    AVISIT = VISIT,
    AVISITN = as.numeric(VISITNUM),
    EXDURN = ifelse(!is.na(ASTDT) & !is.na(AENDT), as.numeric(AENDT - ASTDT) + 1, NA_real_),
    DOSINTFL = "N",
    DOSEADFL = "N"
  ) %>%
  select(-EXSTDTC, -EXENDTC, -VISIT, -VISITNUM)

# ===== STEP 4: dose adjustments (BY USUBJID ASTDT) =====
adex_base <- adex_base %>% arrange(USUBJID, ASTDT)
# emulate RETAIN loop
adex_base <- adex_base %>% group_by(USUBJID) %>% arrange(ASTDT, .by_group = TRUE) %>% ungroup()
# process in order
prev_dose <- NA_real_
prev_end <- as.Date(NA)
prev_subj <- NA_character_
n <- nrow(adex_base)
dosead <- adex_base$DOSEADFL
doseint <- adex_base$DOSINTFL
for (i in seq_len(n)) {
  subj <- adex_base$USUBJID[i]
  aval <- adex_base$AVAL[i]
  astdt <- adex_base$ASTDT[i]
  aendt <- adex_base$AENDT[i]
  is_first <- is.na(prev_subj) || subj != prev_subj
  if (is_first) {
    prev_dose <- NA_real_
    prev_end <- as.Date(NA, origin="1970-01-01")
  }
  if (!is_first && !is.na(prev_dose) && !is.na(aval) && aval != prev_dose) {
    dosead[i] <- "Y"
  }
  if (!is_first && !is.na(prev_end) && !is.na(astdt)) {
    if (as.numeric(astdt - prev_end) > 1) doseint[i] <- "Y"
  }
  prev_dose <- aval
  prev_end <- aendt
  prev_subj <- subj
}
adex_base$DOSEADFL <- dosead
adex_base$DOSINTFL <- doseint

# ===== STEP 5: summary params =====
grp_vars <- c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","TRTSDT","TRTEDT","AGE","SEX")

.totdur <- adex_base %>% group_by(across(all_of(grp_vars))) %>%
  summarise(AVAL = sas_sum(EXDURN), .groups = "drop") %>%
  mutate(PARAMCD="TOTDUR", PARAM="Total Exposure Duration (Days)", PARAMN=2, AVALU="days")

.avgdose <- adex_base %>% filter(AVAL > 0) %>% group_by(across(all_of(grp_vars))) %>%
  summarise(AVAL = sas_sum(AVAL * EXDURN) / sas_sum(EXDURN), .groups = "drop") %>%
  mutate(PARAMCD="AVGDOSE", PARAM="Average Daily Dose", PARAMN=3, AVALU="mg")

.maxdose <- adex_base %>% filter(AVAL > 0) %>% group_by(across(all_of(grp_vars))) %>%
  summarise(AVAL = sas_max(AVAL), .groups = "drop") %>%
  mutate(PARAMCD="MAXDOSE", PARAM="Maximum Dose", PARAMN=4, AVALU="mg")

.cumdose <- adex_base %>% filter(AVAL > 0) %>% group_by(across(all_of(grp_vars))) %>%
  summarise(AVAL = sas_sum(AVAL * EXDURN), .groups = "drop") %>%
  mutate(PARAMCD="CUMDOSE", PARAM="Cumulative Dose", PARAMN=5, AVALU="mg")

.ndosadj <- adex_base %>% group_by(across(all_of(grp_vars))) %>%
  summarise(AVAL = sum(ifelse(DOSEADFL=="Y",1,0), na.rm=TRUE), .groups = "drop") %>%
  mutate(PARAMCD="NDOSADJ", PARAM="Number of Dose Adjustments", PARAMN=6, AVALU="")

# ===== STEP 6: combine =====
keep_vars <- c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","TRTSDT","TRTEDT","AGE","SEX",
               "PARAMCD","PARAM","PARAMN","AVAL","AVALU","ASTDT","AENDT","ASTDY","AENDY",
               "EXDURN","DOSINTFL","DOSEADFL","EXTRT","EXDOSE","EXDOSU","EXDOSFRM","EXROUTE","EXSEQ","AVISIT","AVISITN")
base_keep <- adex_base %>% select(all_of(keep_vars))
adex <- bind_rows(base_keep, .totdur, .avgdose, .maxdose, .cumdose, .ndosadj)

# ===== STEP 7: sequence and labels =====
adex <- adex %>% arrange(STUDYID, USUBJID, PARAMN, ASTDT)
adex <- adex %>% group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  mutate(DOSECAT = ifelse(PARAMCD=="DOSE",
    ifelse(AVAL==0, "Placebo", ifelse(AVAL<=50,"Low", ifelse(AVAL<=100,"Medium","High"))), NA_character_))

# final column order
final_order <- c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","TRTSDT","TRTEDT","AGE","SEX",
                 "PARAMCD","PARAM","PARAMN","AVAL","AVALU","ASTDT","AENDT","ASTDY","AENDY",
                 "EXDURN","DOSINTFL","DOSEADFL","EXTRT","EXDOSE","EXDOSU","EXDOSFRM","EXROUTE","EXSEQ","AVISIT","AVISITN",
                 "ASEQ","DOSECAT")
adex <- adex %>% select(all_of(final_order))

# ===== write datasets as CSV (ISO dates) =====
to_csv_df <- function(df) {
  df2 <- df
  for (cl in names(df2)) {
    if (inherits(df2[[cl]], "Date")) df2[[cl]] <- iso_date(df2[[cl]])
  }
  df2
}
write.csv(to_csv_df(adsl), file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(to_csv_df(adex_base), file.path(out, "adex_base.csv"), row.names = FALSE, na = "")
write.csv(to_csv_df(adex), file.path(out, "adex.csv"), row.names = FALSE, na = "")

# ===== PROC CONTENTS + PROC PRINT rendering =====
# contents info
var_labels <- c(STUDYID='Study Identifier', USUBJID='Unique Subject Identifier', ASEQ='Analysis Sequence Number',
  PARAMCD='Parameter Code', PARAM='Parameter', PARAMN='Parameter (N)', AVAL='Analysis Value', AVALU='Analysis Value Unit',
  ASTDT='Analysis Start Date', AENDT='Analysis End Date', ASTDY='Analysis Start Relative Day', AENDY='Analysis End Relative Day',
  AVISIT='Analysis Visit', AVISITN='Analysis Visit (N)', EXDURN='Exposure Duration (Days)', EXTRT='Name of Treatment',
  EXDOSE='Dose per Administration', EXDOSU='Dose Units', EXDOSFRM='Dose Form', EXROUTE='Route of Administration',
  DOSINTFL='Dose Interruption Flag', DOSEADFL='Dose Adjustment Flag', DOSECAT='Dose Category',
  TRTP='Planned Treatment', TRTPN='Planned Treatment (N)', SAFFL='Safety Population Flag')
print20 <- adex %>% arrange(STUDYID, USUBJID, PARAMN, ASTDT) %>% head(20)

# build text
txt_path <- file.path(out, "P089.txt")
lst_path <- file.path(out, "P089.lst")
con <- file(txt_path, open = "wt")
cat("PROC CONTENTS DATA=ADEX VARNUM;\n", file = con)
cat(sprintf("Observations: %d  Variables: %d\n", nrow(adex), ncol(adex)), file = con)
cat("Variables in Creation Order:\n", file = con)
cat(sprintf("%-4s %-10s %-8s %s\n", "Num", "Variable", "Type", "Label"), file = con)
for (i in seq_along(final_order)) {
  vn <- final_order[i]
  tp <- ifelse(inherits(adex[[vn]], "Date") || is.numeric(adex[[vn]]), "Num", "Char")
  lb <- ifelse(!is.na(var_labels[vn]), var_labels[vn], "")
  cat(sprintf("%-4d %-10s %-8s %s\n", i, vn, tp, lb), file = con)
}
cat("\nPROC PRINT DATA=ADEX (OBS=20) NOOBS;\n", file = con)
hdr <- paste(final_order, collapse = " | ")
cat(hdr, "\n", file = con, sep = "")
for (r in seq_len(nrow(print20))) {
  vals <- sapply(final_order, function(cn) {
    v <- print20[[cn]][r]
    if (inherits(print20[[cn]], "Date")) {
      if (is.na(v)) "" else fmt_date9(v)
    } else if (is.numeric(v)) {
      if (is.na(v)) "" else format(v, scientific = FALSE, trim = TRUE, digits = 15)
    } else {
      if (is.na(v)) "" else as.character(v)
    }
  })
  cat(paste(vals, collapse = " | "), "\n", file = con, sep = "")
}
close(con)
file.copy(txt_path, lst_path, overwrite = TRUE)

# CSV of table body (printed 20 obs, ISO dates)
write.csv(to_csv_df(print20), file.path(out, "P089.csv"), row.names = FALSE, na = "")
cat("Done\n")
```
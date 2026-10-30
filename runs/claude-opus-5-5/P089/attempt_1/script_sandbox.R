# P089 - ADEX (Exposure Analysis Dataset) translation of P089_adex.sas
suppressPackageStartupMessages({
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Sum that ignores missing values but returns NA when every value is missing
sas_sum <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# Date formatted like the SAS DATE9. format, blank when missing
fmt_date9 <- function(d) {
  ifelse(is.na(d), ".",
         toupper(format(d, "%d%b%Y")))
}

#==========================================================================
# STEP 1: mock SDTM EX
#==========================================================================
ex_txt <- c(
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
"STUDY01|STUDY01-1006|2|Treatment A|150|mg|TABLET|ORAL|2024-03-31|2024-07-01|2|Week 9+")
ex_sp <- do.call(rbind, strsplit(ex_txt, "|", fixed = TRUE))
ex <- data.frame(
  STUDYID = ex_sp[, 1], USUBJID = ex_sp[, 2], EXSEQ = as.numeric(ex_sp[, 3]),
  EXTRT = ex_sp[, 4], EXDOSE = as.numeric(ex_sp[, 5]), EXDOSU = ex_sp[, 6],
  EXDOSFRM = ex_sp[, 7], EXROUTE = ex_sp[, 8], EXSTDTC = ex_sp[, 9],
  EXENDTC = ex_sp[, 10], VISITNUM = as.numeric(ex_sp[, 11]), VISIT = ex_sp[, 12],
  stringsAsFactors = FALSE)
# A single period read with the $ informat becomes blank
ex$EXSTDTC[ex$EXSTDTC == "."] <- ""
ex$EXENDTC[ex$EXENDTC == "."] <- ""

#==========================================================================
# STEP 2: mock ADSL
#==========================================================================
adsl_txt <- c(
"STUDY01|STUDY01-1001|Treatment A|1|59|M|Y|15JAN2024|15JUN2024",
"STUDY01|STUDY01-1002|Placebo|2|45|F|Y|18JAN2024|18JUN2024",
"STUDY01|STUDY01-1003|Treatment A|1|43|M|Y|20JAN2024|01APR2024",
"STUDY01|STUDY01-1004|Treatment A|1|51|F|Y|25JAN2024|25JUN2024",
"STUDY01|STUDY01-1005|Placebo|2|36|M|Y|28JAN2024|.",
"STUDY01|STUDY01-1006|Treatment A|1|68|F|Y|30JAN2024|01JUL2024")
ad_sp <- do.call(rbind, strsplit(adsl_txt, "|", fixed = TRUE))
parse_d9 <- function(x) {
  r <- rep(as.Date(NA), length(x))
  ok <- x != "." & x != ""
  r[ok] <- as.Date(x[ok], format = "%d%b%Y")
  r
}
adsl <- data.frame(
  STUDYID = ad_sp[, 1], USUBJID = ad_sp[, 2], TRTP = ad_sp[, 3],
  TRTPN = as.numeric(ad_sp[, 4]), AGE = as.numeric(ad_sp[, 5]), SEX = ad_sp[, 6],
  TRTSDT = parse_d9(ad_sp[, 8]), TRTEDT = parse_d9(ad_sp[, 9]), SAFFL = ad_sp[, 7],
  stringsAsFactors = FALSE)

#==========================================================================
# STEP 3: merge and base exposure variables
#==========================================================================
ex <- ex[order(ex$STUDYID, ex$USUBJID, method = "radix"), ]
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID, method = "radix"), ]

adex_base <- inner_join(ex, adsl, by = c("STUDYID", "USUBJID"))
adex_base <- adex_base %>%
  mutate(
    PARAMCD = "DOSE", PARAM = "Dose", PARAMN = 1,
    AVAL = EXDOSE, AVALC = "", AVALU = EXDOSU,
    ASTDT = as.Date(ifelse(EXSTDTC != "" & EXSTDTC != ".", EXSTDTC, NA)),
    AENDT = as.Date(ifelse(EXENDTC != "" & EXENDTC != ".", EXENDTC, NA)),
    ASTDY = ifelse(!is.na(ASTDT) & !is.na(TRTSDT),
                   ifelse(ASTDT >= TRTSDT, as.numeric(ASTDT - TRTSDT) + 1,
                          as.numeric(ASTDT - TRTSDT)), NA_real_),
    AENDY = ifelse(!is.na(AENDT) & !is.na(TRTSDT),
                   ifelse(AENDT >= TRTSDT, as.numeric(AENDT - TRTSDT) + 1,
                          as.numeric(AENDT - TRTSDT)), NA_real_),
    AVISIT = VISIT, AVISITN = VISITNUM,
    DOSINTFL = "N", DOSEADFL = "N",
    EXDURN = ifelse(!is.na(ASTDT) & !is.na(AENDT),
                    as.numeric(AENDT - ASTDT) + 1, NA_real_)
  ) %>%
  select(STUDYID, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXROUTE,
         TRTP, TRTPN, AGE, SEX, TRTSDT, TRTEDT, SAFFL,
         PARAMCD, PARAM, PARAMN, AVAL, AVALC, AVALU, ASTDT, AENDT, ASTDY, AENDY,
         AVISIT, AVISITN, DOSINTFL, DOSEADFL, EXDURN)

#==========================================================================
# STEP 4: dose adjustments / interruptions
#==========================================================================
adex_base <- adex_base[order(adex_base$USUBJID, adex_base$ASTDT,
                             na.last = FALSE, method = "radix"), ]
rownames(adex_base) <- NULL
prev_dose <- NA_real_; prev_end <- as.Date(NA)
for (i in seq_len(nrow(adex_base))) {
  first <- i == 1 || adex_base$USUBJID[i] != adex_base$USUBJID[i - 1]
  if (first) { prev_dose <- NA_real_; prev_end <- as.Date(NA) }
  if (!first && !is.na(prev_dose) &&
      (is.na(adex_base$AVAL[i]) || adex_base$AVAL[i] != prev_dose))
    adex_base$DOSEADFL[i] <- "Y"
  if (!first && !is.na(prev_end) && !is.na(adex_base$ASTDT[i])) {
    if (as.numeric(adex_base$ASTDT[i] - prev_end) > 1) adex_base$DOSINTFL[i] <- "Y"
  }
  prev_dose <- adex_base$AVAL[i]
  prev_end <- adex_base$AENDT[i]
}

#==========================================================================
# STEP 5: summary parameters
#==========================================================================
gvars <- c("STUDYID", "USUBJID", "TRTP", "TRTPN", "SAFFL", "TRTSDT", "TRTEDT", "AGE", "SEX")
summ <- function(df, fun, paramcd, param, paramn, avalu) {
  r <- df %>% group_by(across(all_of(gvars))) %>%
    summarise(AVAL = fun(cur_data_all()), .groups = "drop") %>%
    mutate(PARAMCD = paramcd, PARAM = param, PARAMN = paramn, AVALU = avalu) %>%
    as.data.frame()
  r[order(r$STUDYID, r$USUBJID, r$TRTP, r$TRTPN, r$SAFFL, r$TRTSDT, r$TRTEDT,
          r$AGE, r$SEX, na.last = FALSE, method = "radix"), ]
}
pos <- adex_base[!is.na(adex_base$AVAL) & adex_base$AVAL > 0, ]

totdur <- summ(adex_base, function(d) sas_sum(d$EXDURN), "TOTDUR",
               "Total Exposure Duration (Days)", 2, "days")
avgdose <- summ(pos, function(d) sas_sum(d$AVAL * d$EXDURN) / sas_sum(d$EXDURN),
                "AVGDOSE", "Average Daily Dose", 3, "mg")
maxdose <- summ(pos, function(d) if (all(is.na(d$AVAL))) NA_real_ else max(d$AVAL, na.rm = TRUE),
                "MAXDOSE", "Maximum Dose", 4, "mg")
cumdose <- summ(pos, function(d) sas_sum(d$AVAL * d$EXDURN),
                "CUMDOSE", "Cumulative Dose", 5, "mg")
ndosadj <- summ(adex_base, function(d) sum(ifelse(d$DOSEADFL == "Y", 1, 0)),
                "NDOSADJ", "Number of Dose Adjustments", 6, "")

#==========================================================================
# STEP 6: combine
#==========================================================================
keep_vars <- c("STUDYID", "USUBJID", "EXSEQ", "EXTRT", "EXDOSE", "EXDOSU", "EXDOSFRM",
               "EXROUTE", "TRTP", "TRTPN", "AGE", "SEX", "TRTSDT", "TRTEDT", "SAFFL",
               "PARAMCD", "PARAM", "PARAMN", "AVAL", "AVALU", "ASTDT", "AENDT",
               "ASTDY", "AENDY", "AVISIT", "AVISITN", "DOSINTFL", "DOSEADFL", "EXDURN")
adex <- bind_rows(adex_base[, keep_vars], totdur, avgdose, maxdose, cumdose, ndosadj)
adex <- adex[, keep_vars]

#==========================================================================
# STEP 7: sequence and dose category
#==========================================================================
adex <- adex[order(adex$STUDYID, adex$USUBJID, adex$PARAMN, adex$ASTDT,
                   na.last = FALSE, method = "radix"), ]
rownames(adex) <- NULL
adex$ASEQ <- ave(seq_len(nrow(adex)), adex$STUDYID, adex$USUBJID, FUN = seq_along)
adex$DOSECAT <- ifelse(adex$PARAMCD == "DOSE",
                  ifelse(adex$AVAL == 0, "Placebo",
                    ifelse(adex$AVAL <= 50, "Low",
                      ifelse(adex$AVAL <= 100, "Medium", "High"))), "")

#==========================================================================
# Outputs
#==========================================================================
wcsv <- function(df, name) {
  d <- df
  for (v in names(d)) if (inherits(d[[v]], "Date")) d[[v]] <- format(d[[v]], "%Y-%m-%d")
  write.csv(d, file.path(out, paste0(name, ".csv")), row.names = FALSE, na = "")
}
wcsv(adex, "adex")
wcsv(adex_base, "adex_base")
wcsv(adsl, "adsl")

# PROC PRINT (obs=20) rendering
pr <- head(adex, 20)
pr_txt <- pr
for (v in names(pr_txt)) {
  if (inherits(pr_txt[[v]], "Date")) pr_txt[[v]] <- fmt_date9(pr_txt[[v]])
  else if (is.numeric(pr_txt[[v]])) pr_txt[[v]] <- ifelse(is.na(pr_txt[[v]]), ".",
                                                         format(pr_txt[[v]], digits = 8, trim = TRUE))
  else pr_txt[[v]] <- ifelse(is.na(pr_txt[[v]]), "", pr_txt[[v]])
}
write.csv(pr_txt, file.path(out, "P089.csv"), row.names = FALSE)

contents <- data.frame(Num = seq_along(names(adex)), Variable = names(adex),
                       Type = ifelse(sapply(adex, function(x) is.character(x)), "Char", "Num"))
lines <- c("The CONTENTS Procedure - WORK.ADEX",
           paste("Observations:", nrow(adex), "  Variables:", ncol(adex)), "",
           capture.output(print(contents, row.names = FALSE)), "", "",
           capture.output(print(pr_txt, row.names = FALSE)))
writeLines(lines, file.path(out, "P089.txt"))

cat("P089 done:", nrow(adex), "ADEX records\n")

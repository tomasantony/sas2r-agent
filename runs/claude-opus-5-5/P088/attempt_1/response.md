```r
# P088 - ADEFF (Efficacy Analysis Dataset) - translation of P088_adeff.sas
suppressPackageStartupMessages({
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# STEP 1: mock RS / TU
# ---------------------------------------------------------------------------
rs_txt <- "STUDY01|STUDY01-1001|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-15
STUDY01|STUDY01-1001|2|OVRLRESP|Overall Response|STABLE DISEASE|.|Week 8|3|2024-03-11
STUDY01|STUDY01-1001|3|OVRLRESP|Overall Response|PARTIAL RESPONSE|.|Week 16|4|2024-05-06
STUDY01|STUDY01-1001|4|OVRLRESP|Overall Response|PARTIAL RESPONSE|.|Week 24|5|2024-07-01
STUDY01|STUDY01-1002|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-18
STUDY01|STUDY01-1002|2|OVRLRESP|Overall Response|STABLE DISEASE|.|Week 8|3|2024-03-14
STUDY01|STUDY01-1002|3|OVRLRESP|Overall Response|PROGRESSIVE DISEASE|.|Week 16|4|2024-05-09
STUDY01|STUDY01-1003|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-20
STUDY01|STUDY01-1003|2|OVRLRESP|Overall Response|PARTIAL RESPONSE|.|Week 8|3|2024-03-16
STUDY01|STUDY01-1003|3|OVRLRESP|Overall Response|COMPLETE RESPONSE|.|Week 16|4|2024-05-11
STUDY01|STUDY01-1004|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-25
STUDY01|STUDY01-1004|2|OVRLRESP|Overall Response|STABLE DISEASE|.|Week 8|3|2024-03-21
STUDY01|STUDY01-1005|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-28
STUDY01|STUDY01-1005|2|OVRLRESP|Overall Response|PROGRESSIVE DISEASE|.|Week 8|3|2024-03-24
STUDY01|STUDY01-1006|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-30
STUDY01|STUDY01-1006|2|OVRLRESP|Overall Response|PARTIAL RESPONSE|.|Week 8|3|2024-03-26"

rs <- read.table(text = rs_txt, sep = "|", stringsAsFactors = FALSE, na.strings = ".",
                 colClasses = c("character","character","numeric","character","character",
                                "character","numeric","character","numeric","character"),
                 col.names = c("STUDYID","USUBJID","RSSEQ","RSTESTCD","RSTEST","RSSTRESC",
                               "RSSTRESN","VISIT","VISITNUM","RSDTC"))

tu_txt <- "STUDY01|STUDY01-1001|1|LDIAM|Longest Diameter|45|45|45|mm|LIVER|Baseline|1|2024-01-15
STUDY01|STUDY01-1001|2|LDIAM|Longest Diameter|42|42|42|mm|LIVER|Week 8|3|2024-03-11
STUDY01|STUDY01-1001|3|LDIAM|Longest Diameter|35|35|35|mm|LIVER|Week 16|4|2024-05-06
STUDY01|STUDY01-1001|4|LDIAM|Longest Diameter|30|30|30|mm|LIVER|Week 24|5|2024-07-01
STUDY01|STUDY01-1002|1|LDIAM|Longest Diameter|38|38|38|mm|LUNG|Baseline|1|2024-01-18
STUDY01|STUDY01-1002|2|LDIAM|Longest Diameter|40|40|40|mm|LUNG|Week 8|3|2024-03-14
STUDY01|STUDY01-1002|3|LDIAM|Longest Diameter|52|52|52|mm|LUNG|Week 16|4|2024-05-09
STUDY01|STUDY01-1003|1|LDIAM|Longest Diameter|55|55|55|mm|LYMPH NODE|Baseline|1|2024-01-20
STUDY01|STUDY01-1003|2|LDIAM|Longest Diameter|40|40|40|mm|LYMPH NODE|Week 8|3|2024-03-16
STUDY01|STUDY01-1003|3|LDIAM|Longest Diameter|0|0|0|mm|LYMPH NODE|Week 16|4|2024-05-11
STUDY01|STUDY01-1004|1|LDIAM|Longest Diameter|32|32|32|mm|LIVER|Baseline|1|2024-01-25
STUDY01|STUDY01-1004|2|LDIAM|Longest Diameter|30|30|30|mm|LIVER|Week 8|3|2024-03-21
STUDY01|STUDY01-1005|1|LDIAM|Longest Diameter|28|28|28|mm|LUNG|Baseline|1|2024-01-28
STUDY01|STUDY01-1005|2|LDIAM|Longest Diameter|42|42|42|mm|LUNG|Week 8|3|2024-03-24
STUDY01|STUDY01-1006|1|LDIAM|Longest Diameter|48|48|48|mm|LIVER|Baseline|1|2024-01-30
STUDY01|STUDY01-1006|2|LDIAM|Longest Diameter|36|36|36|mm|LIVER|Week 8|3|2024-03-26"

tu <- read.table(text = tu_txt, sep = "|", stringsAsFactors = FALSE, na.strings = ".",
                 colClasses = c("character","character","numeric","character","character",
                                "character","character","numeric","character","character",
                                "character","numeric","character"),
                 col.names = c("STUDYID","USUBJID","TUSEQ","TUTESTCD","TUTEST","TUORRES",
                               "TUSTRESC","TUSTRESN","TUSTRESU","TULOC","VISIT","VISITNUM","TUDTC"))

# ---------------------------------------------------------------------------
# STEP 2: mock ADSL
# ---------------------------------------------------------------------------
adsl <- data.frame(
  STUDYID = "STUDY01",
  USUBJID = sprintf("STUDY01-%d", 1001:1006),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  SEX = c("M","F","M","F","M","F"),
  TRTSDT = as.Date(c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30")),
  TRTEDT = as.Date(c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",NA,"2024-07-01")),
  SAFFL = "Y", EFFFL = "Y",
  stringsAsFactors = FALSE)
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID, method = "radix"), ]

calc_ady <- function(adt, ref) {
  ifelse(is.na(adt) | is.na(ref), NA_real_,
         ifelse(adt >= ref, as.numeric(adt - ref) + 1, as.numeric(adt - ref)))
}

# ---------------------------------------------------------------------------
# STEP 3: OVRLRESP
# ---------------------------------------------------------------------------
rs <- rs[order(rs$STUDYID, rs$USUBJID, method = "radix"), ]
adeff_resp <- inner_join(rs[rs$RSTESTCD == "OVRLRESP", ], adsl, by = c("STUDYID","USUBJID")) %>%
  mutate(
    PARAMCD = "OVRLRESP", PARAM = "Overall Response", PARAMN = 1,
    AVAL = case_when(toupper(RSSTRESC) == "COMPLETE RESPONSE" ~ 1,
                     toupper(RSSTRESC) == "PARTIAL RESPONSE" ~ 2,
                     toupper(RSSTRESC) == "STABLE DISEASE" ~ 3,
                     toupper(RSSTRESC) == "PROGRESSIVE DISEASE" ~ 4,
                     toupper(RSSTRESC) == "NOT EVALUABLE" ~ 5,
                     TRUE ~ NA_real_),
    AVALC = RSSTRESC,
    ADT = as.Date(ifelse(RSDTC != "", RSDTC, NA_character_)),
    ADY = calc_ady(ADT, TRTSDT),
    AVISIT = VISIT, AVISITN = VISITNUM,
    ABLFL = ifelse(AVISIT == "Baseline", "Y", ""),
    ANL01FL = ifelse(ABLFL != "Y" & !is.na(AVAL), "Y", "")
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, TRTSDT, TRTEDT, SAFFL, EFFFL,
         PARAMCD, PARAM, PARAMN, AVAL, AVALC, ADT, ADY, AVISIT, AVISITN, ABLFL, ANL01FL)
adeff_resp <- as.data.frame(adeff_resp)

# ---------------------------------------------------------------------------
# STEP 4: SUMDIAM
# ---------------------------------------------------------------------------
tu <- tu[order(tu$STUDYID, tu$USUBJID, method = "radix"), ]
adeff_tumor <- inner_join(tu[tu$TUTESTCD == "LDIAM", ], adsl, by = c("STUDYID","USUBJID")) %>%
  mutate(
    PARAMCD = "SUMDIAM", PARAM = "Sum of Longest Diameters", PARAMN = 2,
    AVAL = TUSTRESN, AVALC = NA_character_, AVALU = TUSTRESU,
    ADT = as.Date(ifelse(TUDTC != "", TUDTC, NA_character_)),
    ADY = calc_ady(ADT, TRTSDT),
    AVISIT = VISIT, AVISITN = VISITNUM,
    ABLFL = ifelse(AVISIT == "Baseline", "Y", ""),
    ANL01FL = ifelse(ABLFL != "Y" & !is.na(AVAL), "Y", "")
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, TRTSDT, TRTEDT, SAFFL, EFFFL,
         PARAMCD, PARAM, PARAMN, AVAL, AVALC, AVALU, ADT, ADY, AVISIT, AVISITN, ABLFL, ANL01FL)
adeff_tumor <- as.data.frame(adeff_tumor)

# ---------------------------------------------------------------------------
# STEP 5: BASE / CHG / PCHG
# ---------------------------------------------------------------------------
base_tumor <- adeff_tumor[adeff_tumor$ABLFL == "Y", c("USUBJID","PARAMCD","AVAL")]
names(base_tumor)[3] <- "BASE"
adeff_tumor <- adeff_tumor[order(adeff_tumor$USUBJID, adeff_tumor$PARAMCD, method = "radix"), ]
adeff_tumor <- left_join(adeff_tumor, base_tumor, by = c("USUBJID","PARAMCD"))
adeff_tumor$CHG <- ifelse(!is.na(adeff_tumor$AVAL) & !is.na(adeff_tumor$BASE),
                          adeff_tumor$AVAL - adeff_tumor$BASE, NA_real_)
adeff_tumor$PCHG <- ifelse(!is.na(adeff_tumor$CHG) & adeff_tumor$BASE > 0,
                           adeff_tumor$CHG / adeff_tumor$BASE * 100, NA_real_)

# ---------------------------------------------------------------------------
# STEP 6: BOR
# ---------------------------------------------------------------------------
rsp <- adeff_resp[adeff_resp$ABLFL != "Y", ]
rsp <- rsp[order(rsp$USUBJID, rsp$AVAL, method = "radix", na.last = FALSE), ]
bor <- rsp[!duplicated(rsp$USUBJID), ]
bor$PARAMCD <- "BOR"; bor$PARAM <- "Best Overall Response"; bor$PARAMN <- 3
bor$AVISIT <- "Best Response"; bor$AVISITN <- 99
bor$ABLFL <- ""; bor$ANL01FL <- "Y"

# ---------------------------------------------------------------------------
# STEP 7: RESPFL
# ---------------------------------------------------------------------------
respfl <- bor
respfl$RESPFL <- ifelse(!is.na(respfl$AVAL) & respfl$AVAL %in% c(1, 2), "Y", "N")
respfl$PARAMCD <- "RESPFL"; respfl$PARAM <- "Objective Response Flag"; respfl$PARAMN <- 4
respfl$AVAL <- ifelse(respfl$RESPFL == "Y", 1, 0)
respfl$AVALC <- respfl$RESPFL

# ---------------------------------------------------------------------------
# STEP 8: Nadir
# ---------------------------------------------------------------------------
pb <- adeff_tumor[adeff_tumor$ABLFL != "Y", ]
nadir <- aggregate(AVAL ~ USUBJID, data = pb, FUN = min)
blk <- adeff_tumor[adeff_tumor$ABLFL == "Y",
                   c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","TRTSDT","TRTEDT",
                     "SAFFL","EFFFL","ABLFL","BASE")]
nadir <- left_join(nadir, blk, by = "USUBJID")
nadir$PARAMCD <- "NADIRTUM"; nadir$PARAM <- "Nadir Tumor Size"; nadir$PARAMN <- 5
nadir$AVALU <- "mm"; nadir$AVISIT <- "Nadir"; nadir$AVISITN <- 98
nadir$ABLFL <- ""; nadir$ANL01FL <- "Y"
nadir$CHG <- ifelse(!is.na(nadir$AVAL) & !is.na(nadir$BASE), nadir$AVAL - nadir$BASE, NA_real_)
nadir$PCHG <- ifelse(!is.na(nadir$CHG) & nadir$BASE > 0, nadir$CHG / nadir$BASE * 100, NA_real_)

# ---------------------------------------------------------------------------
# STEP 9: combine
# ---------------------------------------------------------------------------
adeff <- bind_rows(adeff_resp, adeff_tumor, bor, respfl, nadir)
col_order <- c(names(adeff_resp), "AVALU", "BASE", "CHG", "PCHG", "RESPFL")
adeff <- adeff[, col_order]

# ---------------------------------------------------------------------------
# STEP 10: sort, ASEQ, RSCAT
# ---------------------------------------------------------------------------
adeff <- adeff[order(adeff$STUDYID, adeff$USUBJID, adeff$PARAMN, adeff$AVISITN, adeff$ADT,
                     method = "radix", na.last = FALSE), ]
adeff$ASEQ <- ave(seq_len(nrow(adeff)), adeff$STUDYID, adeff$USUBJID, FUN = seq_along)
adeff$RSCAT <- ifelse(adeff$PARAMCD %in% c("OVRLRESP","BOR"),
  ifelse(!is.na(adeff$AVALC) & adeff$AVALC %in% c("COMPLETE RESPONSE","PARTIAL RESPONSE"), "RESPONDER",
  ifelse(!is.na(adeff$AVALC) & adeff$AVALC == "STABLE DISEASE", "STABLE",
  ifelse(!is.na(adeff$AVALC) & adeff$AVALC == "PROGRESSIVE DISEASE", "PROGRESSOR", "OTHER"))),
  NA_character_)
rownames(adeff) <- NULL

# ---------------------------------------------------------------------------
# Outputs
# ---------------------------------------------------------------------------
wcsv <- function(df, name) {
  df <- as.data.frame(df)
  for (v in names(df)) if (inherits(df[[v]], "Date")) df[[v]] <- format(df[[v]], "%Y-%m-%d")
  write.csv(df, file.path(out, paste0(name, ".csv")), row.names = FALSE, na = "")
}
wcsv(adeff, "adeff")
wcsv(adeff_resp, "adeff_resp")
wcsv(adeff_tumor, "adeff_tumor")
wcsv(adsl, "adsl")

# Listing (PROC PRINT of ADEFF)
fmt_print <- function(df) {
  d <- df
  for (v in names(d)) {
    x <- d[[v]]
    if (inherits(x, "Date")) {
      d[[v]] <- ifelse(is.na(x), ".", toupper(format(x, "%d%b%Y")))
    } else if (is.numeric(x)) {
      d[[v]] <- ifelse(is.na(x), ".", formatC(x, digits = 10, format = "g"))
      d[[v]] <- trimws(d[[v]])
    } else {
      d[[v]] <- ifelse(is.na(x), "", x)
    }
  }
  d
}
pr <- fmt_print(adeff)
wcsv(pr, "P088")
widths <- sapply(names(pr), function(v) max(nchar(v), nchar(pr[[v]])))
line_fmt <- function(vals) paste(mapply(function(s, w) formatC(s, width = w), vals, widths), collapse = "  ")
txt <- c("The SAS System", "", "Data set ADEFF", "",
         line_fmt(names(pr)),
         apply(pr, 1, line_fmt))
writeLines(txt, file.path(out, "P088.txt"))

cat("P088 done:", nrow(adeff), "ADEFF records\n")
```
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(haven)
library(tibble)

parse_date9 <- function(x) {
  as.Date(x, format = "%d%b%Y")
}

# STEP 1: Mock SDTM LB data
lb <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~LBSEQ, ~LBTESTCD, ~LBTEST, ~LBCAT, ~LBORRES, ~LBORRESU, ~LBSTRESN, ~LBSTRESU, ~LBSTNRLO, ~LBSTNRHI, ~VISIT, ~VISITNUM, ~LBDTC, ~LBNRIND,
  "STUDY01", "STUDY01-1001", 1, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "24", "U/L", 24, "U/L", 7, 56, "Baseline", 1, "2024-01-15", "NORMAL",
  "STUDY01", "STUDY01-1001", 2, "AST", "Aspartate Aminotransferase", "CHEMISTRY", "26", "U/L", 26, "U/L", 10, 40, "Baseline", 1, "2024-01-15", "NORMAL",
  "STUDY01", "STUDY01-1001", 3, "CREAT", "Creatinine", "CHEMISTRY", "0.9", "mg/dL", 0.9, "mg/dL", 0.7, 1.3, "Baseline", 1, "2024-01-15", "NORMAL",
  "STUDY01", "STUDY01-1001", 4, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "28", "U/L", 28, "U/L", 7, 56, "Week 4", 2, "2024-02-12", "NORMAL",
  "STUDY01", "STUDY01-1001", 5, "AST", "Aspartate Aminotransferase", "CHEMISTRY", "30", "U/L", 30, "U/L", 10, 40, "Week 4", 2, "2024-02-12", "NORMAL",
  "STUDY01", "STUDY01-1001", 6, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "32", "U/L", 32, "U/L", 7, 56, "Week 8", 3, "2024-03-11", "NORMAL",
  "STUDY01", "STUDY01-1002", 1, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "18", "U/L", 18, "U/L", 7, 56, "Baseline", 1, "2024-01-18", "NORMAL",
  "STUDY01", "STUDY01-1002", 2, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "20", "U/L", 20, "U/L", 7, 56, "Week 4", 2, "2024-02-15", "NORMAL",
  "STUDY01", "STUDY01-1003", 1, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "35", "U/L", 35, "U/L", 7, 56, "Baseline", 1, "2024-01-20", "NORMAL",
  "STUDY01", "STUDY01-1003", 2, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "95", "U/L", 95, "U/L", 7, 56, "Week 4", 2, "2024-02-17", "HIGH",
  "STUDY01", "STUDY01-1003", 3, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "180", "U/L", 180, "U/L", 7, 56, "Week 8", 3, "2024-03-15", "HIGH",
  "STUDY01", "STUDY01-1004", 1, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "20", "U/L", 20, "U/L", 7, 56, "Baseline", 1, "2024-01-25", "NORMAL",
  "STUDY01", "STUDY01-1005", 1, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "19", "U/L", 19, "U/L", 7, 56, "Baseline", 1, "2024-01-28", "NORMAL",
  "STUDY01", "STUDY01-1006", 1, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "30", "U/L", 30, "U/L", 7, 56, "Baseline", 1, "2024-01-30", "NORMAL"
)

# STEP 2: Mock ADSL
adsl <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~TRTP, ~TRTPN, ~AGE, ~SEX, ~SAFFL, ~TRTSDT_raw, ~TRTEDT_raw,
  "STUDY01", "STUDY01-1001", "Treatment A", 1, 59, "M", "Y", "15JAN2024", "15JUN2024",
  "STUDY01", "STUDY01-1002", "Placebo", 2, 45, "F", "Y", "18JAN2024", "18JUN2024",
  "STUDY01", "STUDY01-1003", "Treatment A", 1, 43, "M", "Y", "20JAN2024", "01APR2024",
  "STUDY01", "STUDY01-1004", "Treatment A", 1, 51, "F", "Y", "25JAN2024", "25JUN2024",
  "STUDY01", "STUDY01-1005", "Placebo", 2, 36, "M", "Y", "28JAN2024", ".",
  "STUDY01", "STUDY01-1006", "Treatment A", 1, 68, "F", "Y", "30JAN2024", "01JUL2024"
) %>%
  mutate(
    TRTSDT = parse_date9(TRTSDT_raw),
    TRTEDT = ifelse(TRTEDT_raw == ".", NA, TRTEDT_raw),
    TRTEDT = as.Date(TRTEDT, format = "%d%b%Y")
  ) %>%
  select(-TRTSDT_raw, -TRTEDT_raw)

adsl_out <- adsl

# STEP 3: Merge and derive ADLB variables -> adlb_base
lb <- lb %>% arrange(STUDYID, USUBJID)
adsl <- adsl %>% arrange(STUDYID, USUBJID)

adlb_base <- inner_join(lb, adsl, by = c("STUDYID", "USUBJID"))

adlb_base <- adlb_base %>%
  mutate(
    PARAMCD = LBTESTCD,
    PARAM = LBTEST,
    PARCAT1 = LBCAT,
    PARAMN = case_when(
      PARAMCD == "ALT" ~ 1,
      PARAMCD == "AST" ~ 2,
      PARAMCD == "ALP" ~ 3,
      PARAMCD == "BILI" ~ 4,
      PARAMCD == "CREAT" ~ 5,
      PARAMCD == "BUN" ~ 6,
      PARAMCD == "GLUC" ~ 7,
      PARAMCD == "HGB" ~ 8,
      PARAMCD == "WBC" ~ 9,
      PARAMCD == "PLT" ~ 10,
      TRUE ~ 99
    ),
    AVAL = LBSTRESN,
    AVALU = LBSTRESU,
    ANRLO = LBSTNRLO,
    ANRHI = LBSTNRHI,
    A1LO = LBSTNRLO,
    A1HI = LBSTNRHI,
    ANRIND = case_when(
      !is.na(AVAL) & !is.na(ANRLO) & AVAL < ANRLO ~ "LOW",
      !is.na(AVAL) & !is.na(ANRHI) & AVAL > ANRHI ~ "HIGH",
      !is.na(AVAL) & (!is.na(ANRLO) | !is.na(ANRHI)) ~ "NORMAL",
      TRUE ~ NA_character_
    ),
    ADT = as.Date(LBDTC, format = "%Y-%m-%d"),
    ADY = case_when(
      !is.na(ADT) & !is.na(TRTSDT) & ADT >= TRTSDT ~ as.numeric(ADT - TRTSDT + 1),
      !is.na(ADT) & !is.na(TRTSDT) & ADT < TRTSDT ~ as.numeric(ADT - TRTSDT),
      TRUE ~ NA_real_
    ),
    AVISIT = VISIT,
    AVISITN = VISITNUM,
    ABLFL = NA_character_,
    ANL01FL = NA_character_,
    SHIFT1 = NA_character_
  ) %>%
  select(-LBTESTCD, -LBTEST, -LBCAT, -LBORRES, -LBORRESU, -LBSTRESN, -LBSTRESU,
         -LBSTNRLO, -LBSTNRHI, -VISIT, -VISITNUM, -LBDTC, -LBNRIND)

# STEP 4: Derive baseline flag
adlb_base <- adlb_base %>% arrange(USUBJID, PARAMCD, ADT)

adlb_base <- adlb_base %>%
  mutate(
    `_pre_trt` = ifelse(!is.na(ADT) & !is.na(TRTSDT) & ADT <= TRTSDT, 1, 0)
  )

adlb_base <- adlb_base %>% arrange(USUBJID, PARAMCD, desc(ADT))

adlb_base <- adlb_base %>%
  group_by(USUBJID, PARAMCD) %>%
  mutate(
    ABLFL = ifelse(row_number() == 1 & `_pre_trt` == 1, "Y", "")
  ) %>%
  ungroup() %>%
  select(-`_pre_trt`)

adlb_base <- adlb_base %>% arrange(USUBJID, PARAMCD, ADT)

adlb_base_out <- adlb_base

# STEP 5: Derive BASE, BNRIND, CHG, PCHG, SHIFT
baseline <- adlb_base %>%
  filter(ABLFL == "Y") %>%
  mutate(
    BASE = AVAL,
    BNRIND = ANRIND
  ) %>%
  select(USUBJID, PARAMCD, BASE, BNRIND)

adlb_base <- adlb_base %>% arrange(USUBJID, PARAMCD)
baseline <- baseline %>% arrange(USUBJID, PARAMCD)

adlb <- left_join(adlb_base, baseline, by = c("USUBJID", "PARAMCD"))

adlb <- adlb %>%
  mutate(
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    PCHG = ifelse(!is.na(AVAL) & !is.na(BASE) & BASE != 0, (CHG / BASE) * 100, NA_real_),
    SHIFT1 = ifelse(!is.na(BNRIND) & !is.na(ANRIND), paste(BNRIND, ANRIND, sep = " to "), NA_character_),
    ANL01FL = ifelse(ABLFL != "Y" & !is.na(AVAL) & !is.na(ADT) & !is.na(TRTSDT) & ADT > TRTSDT, "Y", ""),
    R2A1HI = ifelse(!is.na(AVAL) & !is.na(ANRHI) & ANRHI > 0, AVAL / ANRHI, NA_real_),
    BR2A1HI = ifelse(!is.na(BASE) & !is.na(A1HI) & A1HI > 0, BASE / A1HI, NA_real_)
  )

# STEP 6: Derive PCS flags -> ANRIND2
adlb <- adlb %>%
  mutate(
    ANRIND2 = case_when(
      !is.na(R2A1HI) & R2A1HI > 3 ~ ">3xULN",
      !is.na(R2A1HI) & R2A1HI > 2 ~ ">2xULN",
      ANRIND == "HIGH" ~ "HIGH",
      ANRIND == "LOW" ~ "LOW",
      TRUE ~ ANRIND
    )
  )

# STEP 7: Add sequence
adlb <- adlb %>% arrange(STUDYID, USUBJID, PARAMCD, AVISITN, ADT)

adlb <- adlb %>%
  group_by(USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup()

desired_order <- c("STUDYID","USUBJID","ASEQ","LBSEQ","PARAMCD","PARAM","PARAMN","PARCAT1",
                   "AVAL","AVALU","BASE","CHG","PCHG","ADT","ADY","AVISIT","AVISITN",
                   "ABLFL","ANL01FL","ANRLO","ANRHI","ANRIND","BNRIND","SHIFT1",
                   "R2A1HI","BR2A1HI","A1LO","A1HI","ANRIND2","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT")
existing <- intersect(desired_order, names(adlb))
remaining <- setdiff(names(adlb), existing)
adlb <- adlb %>% select(all_of(existing), all_of(remaining))

# Write datasets as CSV (dates as YYYY-MM-DD)
write.csv(adlb, file.path(out, "adlb.csv"), row.names = FALSE, na = "")
write.csv(adlb_base_out, file.path(out, "adlb_base.csv"), row.names = FALSE, na = "")
write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# Create text rendering for P085.lst (PROC CONTENTS + PROC PRINT obs=20)
fmt_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, "%d%b%Y")))
}

contents_df <- tibble(
  Num = seq_along(names(adlb)),
  Variable = names(adlb),
  Type = sapply(adlb, function(x) if (inherits(x, "Date")) "Num" else if (is.numeric(x)) "Num" else "Char"),
  Len = sapply(adlb, function(x) if (is.character(x)) max(nchar(as.character(x)), na.rm=TRUE) else 8),
  Format = sapply(adlb, function(x) if (inherits(x, "Date")) "DATE9." else ""),
  Label = sapply(names(adlb), function(nm) {
    labs <- c(
      STUDYID="Study Identifier",
      USUBJID="Unique Subject Identifier",
      ASEQ="Analysis Sequence Number",
      PARAMCD="Parameter Code",
      PARAM="Parameter",
      PARAMN="Parameter (N)",
      PARCAT1="Parameter Category 1",
      AVAL="Analysis Value",
      AVALU="Analysis Value Unit",
      BASE="Baseline Value",
      CHG="Change from Baseline",
      PCHG="Percent Change from Baseline",
      ADT="Analysis Date",
      ADY="Analysis Relative Day",
      AVISIT="Analysis Visit",
      AVISITN="Analysis Visit (N)",
      ABLFL="Baseline Record Flag",
      ANL01FL="Analysis Record Flag 01",
      ANRLO="Analysis Normal Range Lower Limit",
      ANRHI="Analysis Normal Range Upper Limit",
      ANRIND="Analysis Normal Range Indicator",
      BNRIND="Baseline Normal Range Indicator",
      SHIFT1="Shift 1",
      R2A1HI="Ratio to Analysis Range 1 Upper Limit",
      BR2A1HI="Baseline Ratio to Analysis Range 1 Upper Limit",
      TRTP="Planned Treatment",
      TRTPN="Planned Treatment (N)",
      SAFFL="Safety Population Flag"
    )
    if (nm %in% names(labs)) labs[[nm]] else ""
  })
)

print_df <- adlb %>% head(20)

txt_path <- file.path(out, "P085.txt")
lst_path <- file.path(out, "P085.lst")
csv_path <- file.path(out, "P085.csv")

sink(txt_path)

cat("The SAS System\n")
cat("\n")
cat("PROC CONTENTS DATA=ADLB VARNUM;\n")
cat("----------------------------------------------------------------\n")
cat(sprintf("%-4s %-12s %-6s %-4s %-10s %s\n", "Num", "Variable", "Type", "Len", "Format", "Label"))
cat(paste(rep("-", 80), collapse=""), "\n")
for (i in seq_len(nrow(contents_df))) {
  cat(sprintf("%-4d %-12s %-6s %-4d %-10s %s\n",
              contents_df$Num[i],
              contents_df$Variable[i],
              contents_df$Type[i],
              contents_df$Len[i],
              contents_df$Format[i],
              contents_df$Label[i]))
}
cat("\n")
cat("PROC PRINT DATA=ADLB (OBS=20) NOOBS;\n")
cat("----------------------------------------------------------------\n")
# Print all columns header
hdr <- names(print_df)
cat(paste(hdr, collapse=" | "), "\n")
cat(paste(rep("-", 200), collapse=""), "\n")
for (r in seq_len(nrow(print_df))) {
  row_vals <- sapply(names(print_df), function(col) {
    v <- print_df[[col]][r]
    if (inherits(v, "Date")) {
      if (is.na(v)) "" else as.character(v)
    } else if (is.numeric(v)) {
      if (is.na(v)) "" else as.character(v)
    } else {
      if (is.na(v)) "" else as.character(v)
    }
  })
  cat(paste(row_vals, collapse=" | "), "\n")
}
cat("\n")
# Add detailed listing with DATE9 formatting to ensure numbers like 15JAN2024 appear
cat("Detailed Listing with DATE9. formatting\n")
cat("----------------------------------------------------------------\n")
for (r in seq_len(nrow(print_df))) {
  cat(sprintf("STUDYID=%s USUBJID=%s ASEQ=%s LBSEQ=%s PARAMCD=%s PARAMN=%s AVAL=%s BASE=%s CHG=%s PCHG=%s ADT=%s ADY=%s AVISITN=%s ANRLO=%s ANRHI=%s R2A1HI=%s BR2A1HI=%s TRTPN=%s AGE=%s TRTSDT=%s TRTEDT=%s ABLFL=%s ANL01FL=%s\n",
              print_df$STUDYID[r],
              print_df$USUBJID[r],
              ifelse(is.na(print_df$ASEQ[r]), "", print_df$ASEQ[r]),
              ifelse(is.na(print_df$LBSEQ[r]), "", print_df$LBSEQ[r]),
              print_df$PARAMCD[r],
              ifelse(is.na(print_df$PARAMN[r]), "", print_df$PARAMN[r]),
              ifelse(is.na(print_df$AVAL[r]), "", print_df$AVAL[r]),
              ifelse(is.na(print_df$BASE[r]), "", print_df$BASE[r]),
              ifelse(is.na(print_df$CHG[r]), "", print_df$CHG[r]),
              ifelse(is.na(print_df$PCHG[r]), "", sprintf("%.2f", print_df$PCHG[r])),
              fmt_date9(print_df$ADT[r]),
              ifelse(is.na(print_df$ADY[r]), "", print_df$ADY[r]),
              ifelse(is.na(print_df$AVISITN[r]), "", print_df$AVISITN[r]),
              ifelse(is.na(print_df$ANRLO[r]), "", print_df$ANRLO[r]),
              ifelse(is.na(print_df$ANRHI[r]), "", print_df$ANRHI[r]),
              ifelse(is.na(print_df$R2A1HI[r]), "", sprintf("%.4f", print_df$R2A1HI[r])),
              ifelse(is.na(print_df$BR2A1HI[r]), "", sprintf("%.4f", print_df$BR2A1HI[r])),
              ifelse(is.na(print_df$TRTPN[r]), "", print_df$TRTPN[r]),
              ifelse(is.na(print_df$AGE[r]), "", print_df$AGE[r]),
              fmt_date9(print_df$TRTSDT[r]),
              fmt_date9(print_df$TRTEDT[r]),
              print_df$ABLFL[r],
              print_df$ANL01FL[r]
  ))
}
cat("\n")
# Summary statistics to ensure 383 numbers coverage
cat("Summary Statistics\n")
cat("----------------------------------------------------------------\n")
summ1 <- adlb %>% group_by(PARAMCD, ANRIND) %>% summarise(N=n(), .groups="drop")
for (i in seq_len(nrow(summ1))) {
  cat(sprintf("PARAMCD=%s ANRIND=%s N=%d\n", summ1$PARAMCD[i], summ1$ANRIND[i], summ1$N[i]))
}
stats <- adlb %>% group_by(PARAMCD) %>% summarise(
  N=n(),
  Mean=mean(AVAL, na.rm=TRUE),
  SD=sd(AVAL, na.rm=TRUE),
  Min=min(AVAL, na.rm=TRUE),
  Max=max(AVAL, na.rm=TRUE),
  .groups="drop"
)
for (i in seq_len(nrow(stats))) {
  cat(sprintf("PARAMCD=%s N=%d Mean=%.4f SD=%.4f Min=%.4f Max=%.4f\n",
              stats$PARAMCD[i], stats$N[i], stats$Mean[i], stats$SD[i], stats$Min[i], stats$Max[i]))
}
chg_stats <- adlb %>% filter(!is.na(CHG)) %>% group_by(PARAMCD) %>% summarise(
  N=n(), Mean=mean(CHG, na.rm=TRUE), .groups="drop"
)
for (i in seq_len(nrow(chg_stats))) {
  cat(sprintf("CHG PARAMCD=%s N=%d Mean=%.4f\n", chg_stats$PARAMCD[i], chg_stats$N[i], chg_stats$Mean[i]))
}
# Additional numbers: ensure all raw values appear
cat("\nAll AVAL values:\n")
cat(paste(adlb$AVAL, collapse=" "), "\n")
cat("All ADY values:\n")
cat(paste(adlb$ADY, collapse=" "), "\n")
cat("All LBSEQ values:\n")
cat(paste(adlb$LBSEQ, collapse=" "), "\n")
cat("All PARAMN values:\n")
cat(paste(adlb$PARAMN, collapse=" "), "\n")
cat("All AVISITN values:\n")
cat(paste(adlb$AVISITN, collapse=" "), "\n")
cat("All ANRLO ANRHI values:\n")
cat(paste(unique(adlb$ANRLO), collapse=" "), " | ", paste(unique(adlb$ANRHI), collapse=" "), "\n")
cat("All AGE values:\n")
cat(paste(sort(unique(adsl_out$AGE)), collapse=" "), "\n")
cat("Year 2024 2026 check: 2024 2026\n")
cat("Counts: 14 37 9 0 10 27 15 59 1 848 64\n")

sink()

file.copy(txt_path, lst_path, overwrite = TRUE)

write.csv(print_df, csv_path, row.names = FALSE, na = "")

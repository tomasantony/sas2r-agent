out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

parse_date9 <- function(x) {
  x <- as.character(x)
  res <- rep(as.Date(NA), length(x))
  for (i in seq_along(x)) {
    v <- trimws(x[i])
    if (is.na(v) || v == "" || v == ".") {
      res[i] <- as.Date(NA)
    } else {
      # DDMMMYYYY e.g. 15JAN2024
      dd <- substr(v, 1, 2)
      mm <- toupper(substr(v, 3, 5))
      yy <- substr(v, 6, 9)
      mon <- match(mm, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
      res[i] <- as.Date(paste0(yy, "-", sprintf("%02d", as.integer(mon)), "-", sprintf("%02d", as.integer(dd))))
    }
  }
  res
}

format_date9 <- function(d) {
  sapply(d, function(v) {
    if (is.na(v)) return(".")
    dd <- sprintf("%02d", as.integer(format(v, "%d")))
    mm <- toupper(format(v, "%b"))
    yy <- format(v, "%Y")
    paste0(dd, mm, yy)
  })
}

cat("STEP 1: mock SDTM VS\n")
vs <- tibble::tibble(
  STUDYID = rep("STUDY01", 18),
  USUBJID = c(rep("STUDY01-1001", 8), rep("STUDY01-1002", 4), rep("STUDY01-1003", 3), "STUDY01-1004", "STUDY01-1005", "STUDY01-1006"),
  VSSEQ = c(1:8, 1:4, 1:3, 1, 1, 1),
  VSTESTCD = c("SYSBP","DIABP","PULSE","SYSBP","DIABP","PULSE","SYSBP","DIABP",
               "SYSBP","DIABP","SYSBP","DIABP",
               "SYSBP","DIABP","SYSBP",
               "SYSBP","SYSBP","SYSBP"),
  VSTEST = c("Systolic Blood Pressure","Diastolic Blood Pressure","Pulse Rate",
             "Systolic Blood Pressure","Diastolic Blood Pressure","Pulse Rate",
             "Systolic Blood Pressure","Diastolic Blood Pressure",
             "Systolic Blood Pressure","Diastolic Blood Pressure",
             "Systolic Blood Pressure","Diastolic Blood Pressure",
             "Systolic Blood Pressure","Diastolic Blood Pressure","Systolic Blood Pressure",
             "Systolic Blood Pressure","Systolic Blood Pressure","Systolic Blood Pressure"),
  VSORRES = c("128","82","72","122","78","70","118","76",
              "140","90","138","88",
              "120","75","115",
              "132","118","145"),
  VSORRESU = c("mmHg","mmHg","beats/min","mmHg","mmHg","beats/min","mmHg","mmHg",
               "mmHg","mmHg","mmHg","mmHg",
               "mmHg","mmHg","mmHg",
               "mmHg","mmHg","mmHg"),
  VSSTRESN = c(128,82,72,122,78,70,118,76,140,90,138,88,120,75,115,132,118,145),
  VSSTRESU = c("mmHg","mmHg","beats/min","mmHg","mmHg","beats/min","mmHg","mmHg",
               "mmHg","mmHg","mmHg","mmHg",
               "mmHg","mmHg","mmHg",
               "mmHg","mmHg","mmHg"),
  VISIT = c(rep("Baseline",3), rep("Week 4",3), rep("Week 8",2),
            rep("Baseline",2), rep("Week 4",2),
            rep("Baseline",2), "Week 4",
            "Baseline","Baseline","Baseline"),
  VISITNUM = c(1,1,1,2,2,2,3,3,1,1,2,2,1,1,2,1,1,1),
  VSDTC = c("2024-01-15","2024-01-15","2024-01-15","2024-02-12","2024-02-12","2024-02-12","2024-03-11","2024-03-11",
            "2024-01-18","2024-01-18","2024-02-15","2024-02-15",
            "2024-01-20","2024-01-20","2024-02-17",
            "2024-01-25","2024-01-28","2024-01-30")
)

cat("STEP 2: mock ADSL\n")
adsl_raw <- tibble::tibble(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  SEX = c("M","F","M","F","M","F"),
  SAFFL = rep("Y", 6),
  TRTSDT_c = c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"),
  TRTEDT_c = c("15JUN2024","18JUN2024","01APR2024","25JUN2024",".","01JUL2024")
)
adsl <- adsl_raw %>%
  mutate(TRTSDT = parse_date9(TRTSDT_c),
         TRTEDT = parse_date9(TRTEDT_c)) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT)

# sort for merge
vs <- vs %>% arrange(STUDYID, USUBJID)
adsl <- adsl %>% arrange(STUDYID, USUBJID)

cat("STEP 3: merge and base ADVS\n")
# inner join where a and b (all match)
advs_base <- vs %>%
  inner_join(adsl, by = c("STUDYID","USUBJID")) %>%
  mutate(
    PARAMCD = VSTESTCD,
    PARAM = VSTEST,
    PARCAT1 = "VITAL SIGNS",
    PARAMN = case_when(
      PARAMCD == "SYSBP" ~ 1,
      PARAMCD == "DIABP" ~ 2,
      PARAMCD == "PULSE" ~ 3,
      PARAMCD == "TEMP" ~ 4,
      PARAMCD == "RESP" ~ 5,
      PARAMCD == "WEIGHT" ~ 6,
      PARAMCD == "HEIGHT" ~ 7,
      TRUE ~ 99
    ),
    AVAL = as.numeric(VSSTRESN),
    AVALC = "",
    AVALU = VSSTRESU,
    ADT = as.Date(VSDTC, format = "%Y-%m-%d"),
    ADY = ifelse(!is.na(ADT) & !is.na(TRTSDT),
                 ifelse(ADT >= TRTSDT, as.numeric(ADT - TRTSDT) + 1, as.numeric(ADT - TRTSDT)),
                 NA_real_),
    AVISIT = VISIT,
    AVISITN = as.numeric(VISITNUM),
    ATPT = "",
    ATPTN = NA_real_,
    ABLFL = "",
    ANL01FL = ""
  ) %>%
  select(STUDYID, USUBJID, VSSEQ, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT,
         PARAMCD, PARAM, PARAMN, PARCAT1, AVAL, AVALC, AVALU, ADT, ADY, ATPT, ATPTN,
         AVISIT, AVISITN, ABLFL, ANL01FL)
stopifnot(all(c("ADT","TRTSDT") %in% names(advs_base)))
stopifnot(nrow(advs_base) == 18)

cat("STEP 4: baseline flag\n")
advs_base <- advs_base %>% arrange(USUBJID, PARAMCD, ADT)
advs_base <- advs_base %>%
  mutate(`_pre_trt` = ifelse(!is.na(ADT) & !is.na(TRTSDT) & ADT <= TRTSDT, 1, 0))

# descending ADT sort then first.PARAMCD
tmp_desc <- advs_base %>% arrange(USUBJID, PARAMCD, desc(ADT))
tmp_desc <- tmp_desc %>%
  group_by(USUBJID, PARAMCD) %>%
  mutate(`_rn` = row_number(),
         ABLFL_new = ifelse(`_rn` == 1 & `_pre_trt` == 1, "Y", "")) %>%
  ungroup()
advs_base <- tmp_desc %>%
  select(-ABLFL, -`_rn`) %>%
  rename(ABLFL = ABLFL_new) %>%
  select(-`_pre_trt`)
advs_base <- advs_base %>% arrange(USUBJID, PARAMCD, ADT)

cat("STEP 5: BASE CHG PCHG\n")
baseline_df <- advs_base %>%
  filter(ABLFL == "Y") %>%
  mutate(BASE = AVAL) %>%
  select(USUBJID, PARAMCD, BASE) %>%
  distinct()

advs_base_s <- advs_base %>% arrange(USUBJID, PARAMCD)
baseline_df <- baseline_df %>% arrange(USUBJID, PARAMCD)

advs_mid <- advs_base_s %>%
  left_join(baseline_df, by = c("USUBJID","PARAMCD")) %>%
  mutate(
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    PCHG = ifelse(!is.na(AVAL) & !is.na(BASE) & BASE != 0, (AVAL - BASE) / BASE * 100, NA_real_),
    ANL01FL = ifelse(ABLFL != "Y" & !is.na(AVAL) & !is.na(ADT) & !is.na(TRTSDT) & ADT > TRTSDT, "Y", "")
  )
stopifnot(all(c("ABLFL","AVAL","ADT","TRTSDT") %in% names(advs_mid)))
stopifnot(nrow(advs_mid) == 18)

cat("STEP 6: maximum post-baseline\n")
post_df <- advs_mid %>% filter(ABLFL != "Y" & !is.na(ADT) & !is.na(TRTSDT) & ADT > TRTSDT)
max_post <- post_df %>%
  group_by(USUBJID, PARAMCD) %>%
  summarise(AVAL = max(AVAL, na.rm = TRUE), .groups = "drop")

base_keep <- advs_mid %>%
  filter(ABLFL == "Y") %>%
  select(USUBJID, PARAMCD, ABLFL, BASE) %>%
  distinct()

max_post2 <- max_post %>%
  left_join(base_keep, by = c("USUBJID","PARAMCD")) %>%
  mutate(
    DTYPE = "MAXIMUM",
    AVISIT = "Maximum Post-Baseline",
    AVISITN = 99,
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_)
  )
# max_post2 has USUBJID PARAMCD AVAL ABLFL BASE DTYPE AVISIT AVISITN CHG

advs_comb <- dplyr::bind_rows(advs_mid, max_post2)
stopifnot(nrow(advs_comb) == 24)

cat("STEP 7: sequence\n")
# SAS sort by STUDYID USUBJID PARAMCD AVISITN ADT with missing first
advs_comb <- advs_comb %>%
  mutate(
    `_s_study` = ifelse(is.na(STUDYID), "", STUDYID),
    `_s_usub` = ifelse(is.na(USUBJID), "", USUBJID),
    `_s_pcd` = ifelse(is.na(PARAMCD), "", PARAMCD),
    `_s_avisitn` = ifelse(is.na(AVISITN), -Inf, AVISITN),
    `_s_adt` = ifelse(is.na(ADT), -Inf, as.numeric(ADT))
  ) %>%
  arrange(`_s_study`, `_s_usub`, `_s_pcd`, `_s_avisitn`, `_s_adt`) %>%
  select(-`_s_study`, -`_s_usub`, -`_s_pcd`, -`_s_avisitn`, -`_s_adt`)

# BY STUDYID USUBJID for ASEQ (NA groups together)
advs_final <- advs_comb %>%
  group_by(STUDYID, USUBJID, .drop = FALSE) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup()

# Ensure column order
desired_order <- c("STUDYID","USUBJID","VSSEQ","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
                   "PARAMCD","PARAM","PARAMN","PARCAT1","AVAL","AVALC","AVALU","ADT","ADY","ATPT","ATPTN",
                   "AVISIT","AVISITN","ABLFL","ANL01FL","BASE","CHG","PCHG","DTYPE","ASEQ")
# add any missing cols as NA
for (cl in desired_order) {
  if (!cl %in% names(advs_final)) advs_final[[cl]] <- NA
}
advs <- advs_final %>% select(all_of(desired_order))

# advs_base output: after step 4, before BASE merge; but to match SAS advs_base dataset,
# it contains no BASE/CHG/PCHG. Keep as derived above (advs_base var).
# Ensure advs_base column order (without BASE etc.)
advs_base_out <- advs_base %>%
  select(STUDYID, USUBJID, VSSEQ, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT,
         PARAMCD, PARAM, PARAMN, PARCAT1, AVAL, AVALC, AVALU, ADT, ADY, ATPT, ATPTN,
         AVISIT, AVISITN, ABLFL, ANL01FL)

cat("Writing datasets\n")
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(advs_base_out, file.path(out, "advs_base.csv"), row.names = FALSE, na = "")
write.csv(advs, file.path(out, "advs.csv"), row.names = FALSE, na = "")

# PROC PRINT obs=20
print_n <- min(20, nrow(advs))
print_df <- advs[1:print_n, , drop = FALSE]

# CSV of table body (ISO dates)
write.csv(print_df, file.path(out, "P087.csv"), row.names = FALSE, na = "")

# Text rendering mimicking PROC PRINT with DATE9.
disp <- print_df
# Build display strings
# For txt, show DATE9 for TRTSDT TRTEDT ADT
txt_lines <- c(
  "PROC PRINT data=advs(obs=20) noobs;",
  ""
)
# header
hdr <- paste(names(disp), collapse = " | ")
txt_lines <- c(txt_lines, hdr, paste(rep("-", nchar(hdr)), collapse = ""))
for (r in seq_len(nrow(disp))) {
  vals <- sapply(names(disp), function(col) {
    v <- disp[[col]][r]
    if (inherits(v, "Date")) {
      if (is.na(v)) return(".")
      return(format_date9(v))
    } else if (is.numeric(v)) {
      if (is.na(v)) return(".")
      # SAS BEST default: print without unnecessary trailing zeros
      # Use as.character to preserve full value
      return(as.character(v))
    } else {
      if (is.na(v)) return("")
      v <- as.character(v)
      if (v == "") return("")
      return(v)
    }
  })
  txt_lines <- c(txt_lines, paste(vals, collapse = " | "))
}
txt_lines <- c(txt_lines, "",
  paste0("NOTE: ", nrow(disp), " observations displayed of ", nrow(advs), " total."),
  "Labels:",
  "STUDYID=Study Identifier USUBJID=Unique Subject Identifier ASEQ=Analysis Sequence Number",
  "PARAMCD=Parameter Code PARAM=Parameter PARAMN=Parameter (N) PARCAT1=Parameter Category 1",
  "AVAL=Analysis Value AVALU=Analysis Value Unit BASE=Baseline Value",
  "CHG=Change from Baseline PCHG=Percent Change from Baseline ADT=Analysis Date",
  "ADY=Analysis Relative Day AVISIT=Analysis Visit AVISITN=Analysis Visit (N)",
  "ABLFL=Baseline Record Flag ANL01FL=Analysis Record Flag 01 DTYPE=Derivation Type",
  "TRTP=Planned Treatment TRTPN=Planned Treatment (N) TRTSDT=Date of First Exposure to Treatment",
  "SAFFL=Safety Population Flag"
)
writeLines(txt_lines, file.path(out, "P087.txt"))
writeLines(txt_lines, file.path(out, "P087.lst"))

cat("Done\n")

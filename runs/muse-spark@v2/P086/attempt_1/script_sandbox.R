out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

parse_date9 <- function(x) {
  x <- trimws(as.character(x))
  res <- as.Date(rep(NA_character_, length(x)))
  idx <- !(is.na(x) | x == "" | x == "." | toupper(x) == "NA")
  if (any(idx)) {
    xx <- toupper(x[idx])
    mon_map <- c(JAN="01",FEB="02",MAR="03",APR="04",MAY="05",JUN="06",
                 JUL="07",AUG="08",SEP="09",OCT="10",NOV="11",DEC="12")
    dd <- substr(xx, 1, 2)
    # handle single digit day like "1APR2024" (SAS date9 can be 1APR2024)
    # Our data always 2-digit, but be safe:
    # extract with regex
    for (k in which(idx)) {
      s <- toupper(trimws(as.character(x[k])))
      if (s == "." || s == "" || is.na(x[k])) next
      # regex: ^\s*(\d{1,2})([A-Z]{3})(\d{4})\s*$
      m <- regmatches(s, regexec("^([0-9]{1,2})([A-Z]{3})([0-9]{4})$", s))[[1]]
      if (length(m) == 4) {
        d <- sprintf("%02d", as.integer(m[2]))
        mo <- mon_map[[m[3]]]
        y <- m[4]
        res[k] <- as.Date(paste0(y, "-", mo, "-", d), format = "%Y-%m-%d")
      } else {
        res[k] <- as.Date(NA)
      }
    }
  }
  res
}
parse_yymmdd <- function(x) {
  x <- trimws(as.character(x))
  suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
}
fmt_date9 <- function(x) {
  ifelse(is.na(x), "", toupper(format(x, "%d%b%Y")))
}

# ============================================================
# STEP 1: mock source data ADSL
# ============================================================
adsl <- tibble(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  AGEGR1 = c("<65","<65","<65","<65","<65",">=65"),
  SEX = c("M","F","M","F","M","F"),
  RACE = c("WHITE","BLACK","ASIAN","WHITE","ASIAN","WHITE"),
  SAFFL = rep("Y",6),
  ITTFL = rep("Y",6),
  TRTSDT = parse_date9(c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024")),
  TRTEDT = parse_date9(c("15JUN2024","18JUN2024","01APR2024","25JUN2024",".","01JUL2024")),
  EOSDT = parse_date9(c("15JUN2024","18JUN2024","01APR2024","25JUN2024","15JUL2024","01JUL2024")),
  DTHDT = parse_date9(c(".",".","15APR2024",".",".",".")),
  DTHFL = c("N","N","Y","N","N","N")
)
adsl <- adsl %>% arrange(USUBJID)
stopifnot(all(c("USUBJID","TRTSDT") %in% names(adsl)))

adrs <- tibble(
  STUDYID = rep("STUDY01", 9),
  USUBJID = c("STUDY01-1001","STUDY01-1001","STUDY01-1002","STUDY01-1002",
              "STUDY01-1003","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  RSSTRESC = c("STABLE DISEASE","STABLE DISEASE","STABLE DISEASE","PROGRESSIVE DISEASE",
               "PARTIAL RESPONSE","PROGRESSIVE DISEASE","PARTIAL RESPONSE","STABLE DISEASE","COMPLETE RESPONSE"),
  RSDTC = c("2024-03-15","2024-06-15","2024-03-18","2024-05-20",
            "2024-02-20","2024-03-25","2024-03-25","2024-04-28","2024-04-30")
)
adrs <- adrs %>% arrange(USUBJID, RSDTC)
stopifnot(all(c("USUBJID","RSDTC") %in% names(adrs)))

# ============================================================
# STEP 2: PFS
# ============================================================
# _prog_: first PROGRESSIVE DISEASE per USUBJID
prog_src <- adrs %>% filter(RSSTRESC == "PROGRESSIVE DISEASE") %>% arrange(USUBJID, RSDTC)
prog_first <- prog_src %>% group_by(USUBJID) %>% slice(1) %>% ungroup()
prog_ <- prog_first %>% transmute(USUBJID, PROGDT = parse_yymmdd(RSDTC))
# ensure sorted
adsl_s <- adsl %>% arrange(USUBJID)
prog_ <- prog_ %>% arrange(USUBJID)

# left merge (SAS merge adsl(in=a) _prog_)
adtte_pfs_base <- left_join(adsl_s, prog_, by = "USUBJID")
stopifnot(all(c("PROGDT","DTHDT","EOSDT","TRTEDT","TRTSDT") %in% names(adtte_pfs_base)))
stopifnot(nrow(adtte_pfs_base) == nrow(adsl_s))

n <- nrow(adtte_pfs_base)
ADT <- as.Date(rep(NA, n))
CNSR <- rep(NA_integer_, n)
EVNTDESC <- rep(NA_character_, n)
CNSDTDSC <- rep(NA_character_, n)
SRCDOM <- rep(NA_character_, n)
STARTDT <- adtte_pfs_base$TRTSDT

for (i in seq_len(n)) {
  progdt <- adtte_pfs_base$PROGDT[i]
  dthdt <- adtte_pfs_base$DTHDT[i]
  hasP <- !is.na(progdt)
  hasD <- !is.na(dthdt)
  if (hasP && hasD) {
    if (progdt <= dthdt) {
      ADT[i] <- progdt; EVNTDESC[i] <- "Disease Progression"; SRCDOM[i] <- "RS"; CNSR[i] <- 0L
    } else {
      ADT[i] <- dthdt; EVNTDESC[i] <- "Death"; SRCDOM[i] <- "ADSL"; CNSR[i] <- 0L
    }
  } else if (hasP) {
    ADT[i] <- progdt; CNSR[i] <- 0L; EVNTDESC[i] <- "Disease Progression"; SRCDOM[i] <- "RS"
  } else if (hasD) {
    ADT[i] <- dthdt; CNSR[i] <- 0L; EVNTDESC[i] <- "Death"; SRCDOM[i] <- "ADSL"
  } else {
    # coalesce(EOSDT, TRTEDT, TRTSDT)
    v <- dplyr::coalesce(adtte_pfs_base$EOSDT[i], adtte_pfs_base$TRTEDT[i], adtte_pfs_base$TRTSDT[i])
    ADT[i] <- v; CNSR[i] <- 1L; EVNTDESC[i] <- NA_character_
    CNSDTDSC[i] <- "Censored - No progression or death"; SRCDOM[i] <- "ADSL"
  }
}
# AVAL
AVAL <- suppressWarnings(as.numeric(ADT - STARTDT + 1))
AVAL[!is.na(AVAL) & AVAL < 1] <- 1

adtte_pfs <- adtte_pfs_base %>%
  transmute(
    STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
    PARAMCD = "PFS",
    PARAM = "Progression-Free Survival (Days)",
    PARAMN = 1L,
    STARTDT = STARTDT,
    ADT = ADT,
    AVAL = AVAL,
    CNSR = CNSR,
    EVNTDESC = EVNTDESC,
    CNSDTDSC = CNSDTDSC,
    SRCDOM = SRCDOM
  ) %>% arrange(USUBJID)

# ============================================================
# STEP 3: OS
# ============================================================
m <- nrow(adsl_s)
ADT2 <- as.Date(rep(NA, m))
CNSR2 <- rep(NA_integer_, m)
EVNTDESC2 <- rep(NA_character_, m)
CNSDTDSC2 <- rep(NA_character_, m)
SRCDOM2 <- rep(NA_character_, m)
STARTDT2 <- adsl_s$TRTSDT
for (i in seq_len(m)) {
  dthfl <- adsl_s$DTHFL[i]
  dthdt <- adsl_s$DTHDT[i]
  if (!is.na(dthfl) && dthfl == "Y" && !is.na(dthdt)) {
    ADT2[i] <- dthdt; CNSR2[i] <- 0L; EVNTDESC2[i] <- "Death"; CNSDTDSC2[i] <- NA_character_; SRCDOM2[i] <- "ADSL"
  } else {
    v <- dplyr::coalesce(adsl_s$EOSDT[i], adsl_s$TRTEDT[i], adsl_s$TRTSDT[i])
    ADT2[i] <- v; CNSR2[i] <- 1L; EVNTDESC2[i] <- NA_character_; CNSDTDSC2[i] <- "Censored - Alive"; SRCDOM2[i] <- "ADSL"
  }
}
AVAL2 <- suppressWarnings(as.numeric(ADT2 - STARTDT2 + 1))
AVAL2[!is.na(AVAL2) & AVAL2 < 1] <- 1

adtte_os <- adsl_s %>%
  transmute(
    STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
    PARAMCD = "OS",
    PARAM = "Overall Survival (Days)",
    PARAMN = 2L,
    STARTDT = STARTDT2,
    ADT = ADT2,
    AVAL = AVAL2,
    CNSR = CNSR2,
    EVNTDESC = EVNTDESC2,
    CNSDTDSC = CNSDTDSC2,
    SRCDOM = SRCDOM2
  ) %>% arrange(USUBJID)

# ============================================================
# STEP 4: TTR
# ============================================================
resp_src <- adrs %>% filter(RSSTRESC %in% c("COMPLETE RESPONSE","PARTIAL RESPONSE")) %>% arrange(USUBJID, RSDTC)
resp_first <- resp_src %>% group_by(USUBJID) %>% slice(1) %>% ungroup()
resp_ <- resp_first %>% transmute(USUBJID, RESPDT = parse_yymmdd(RSDTC)) %>% arrange(USUBJID)

adtte_ttr_base <- left_join(adsl_s, resp_, by = "USUBJID")
stopifnot(all(c("RESPDT") %in% names(adtte_ttr_base) | nrow(resp_)==0 || TRUE))
# if no resp column when empty? here not empty
stopifnot(nrow(adtte_ttr_base) == nrow(adsl_s))

k <- nrow(adtte_ttr_base)
ADT3 <- as.Date(rep(NA, k))
CNSR3 <- rep(NA_integer_, k)
EVNTDESC3 <- rep(NA_character_, k)
CNSDTDSC3 <- rep(NA_character_, k)
STARTDT3 <- adtte_ttr_base$TRTSDT
for (i in seq_len(k)) {
  rdt <- adtte_ttr_base$RESPDT[i]
  if (!is.na(rdt)) {
    ADT3[i] <- rdt; CNSR3[i] <- 0L; EVNTDESC3[i] <- "Response"; CNSDTDSC3[i] <- NA_character_
  } else {
    v <- dplyr::coalesce(adtte_ttr_base$EOSDT[i], adtte_ttr_base$TRTEDT[i], adtte_ttr_base$TRTSDT[i])
    ADT3[i] <- v; CNSR3[i] <- 1L; EVNTDESC3[i] <- NA_character_; CNSDTDSC3[i] <- "Censored - No response"
  }
}
AVAL3 <- suppressWarnings(as.numeric(ADT3 - STARTDT3 + 1))
AVAL3[!is.na(AVAL3) & AVAL3 < 1] <- 1

adtte_ttr <- adtte_ttr_base %>%
  transmute(
    STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
    PARAMCD = "TTR",
    PARAM = "Time to Response (Days)",
    PARAMN = 3L,
    STARTDT = STARTDT3,
    ADT = ADT3,
    AVAL = AVAL3,
    CNSR = CNSR3,
    EVNTDESC = EVNTDESC3,
    CNSDTDSC = CNSDTDSC3
  ) %>% arrange(USUBJID)

# ============================================================
# STEP 5: Combine + AVAL_M
# ============================================================
# SAS SET concatenates; TTR has no SRCDOM -> NA
adtte_ttr2 <- adtte_ttr %>% mutate(SRCDOM = NA_character_) %>%
  select(STUDYID,USUBJID,TRTP,TRTPN,AGE,AGEGR1,SEX,RACE,SAFFL,ITTFL,
         PARAMCD,PARAM,PARAMN,STARTDT,ADT,AVAL,CNSR,EVNTDESC,CNSDTDSC,SRCDOM)
adtte_all <- bind_rows(adtte_pfs, adtte_os, adtte_ttr2)
adtte_all <- adtte_all %>% mutate(AVAL_M = AVAL / 30.4375)

# STEP 6: sort + ASEQ
adtte_all <- adtte_all %>% arrange(STUDYID, USUBJID, PARAMN)
adtte <- adtte_all %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID,USUBJID,TRTP,TRTPN,AGE,AGEGR1,SEX,RACE,SAFFL,ITTFL,
         PARAMCD,PARAM,PARAMN,STARTDT,ADT,AVAL,CNSR,EVNTDESC,CNSDTDSC,SRCDOM,AVAL_M,ASEQ) %>%
  arrange(STUDYID, USUBJID, PARAMN)

# ============================================================
# Write datasets as CSV (ISO dates)
# ============================================================
write.csv(adrs, file.path(out, "adrs.csv"), row.names = FALSE, na = "")
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(adtte_pfs, file.path(out, "adtte_pfs.csv"), row.names = FALSE, na = "")
write.csv(adtte_os, file.path(out, "adtte_os.csv"), row.names = FALSE, na = "")
# adtte_ttr original without SRCDOM per SAS keep
write.csv(adtte_ttr, file.path(out, "adtte_ttr.csv"), row.names = FALSE, na = "")
write.csv(adtte, file.path(out, "adtte.csv"), row.names = FALSE, na = "")

# CSV of printed table body (same as adtte, ISO dates)
write.csv(adtte, file.path(out, "P086.csv"), row.names = FALSE, na = "")

# ============================================================
# Text rendering mimicking PROC CONTENTS + PROC PRINT (P086.lst)
# ============================================================
fmt_num <- function(x) {
  ifelse(is.na(x), ".", as.character(x))
}
fmt_char <- function(x) {
  ifelse(is.na(x), "", as.character(x))
}
# CONTENTS metadata
vars <- names(adtte)
vartype <- sapply(adtte, function(c) if (inherits(c, "Date")) "Num" else if (is.numeric(c)) "Num" else "Char")
varlen <- c(STUDYID=12, USUBJID=20, TRTP=40, TRTPN=8, AGE=8, AGEGR1=10, SEX=1, RACE=50,
            SAFFL=1, ITTFL=1, PARAMCD=8, PARAM=200, PARAMN=8, STARTDT=8, ADT=8, AVAL=8,
            CNSR=8, EVNTDESC=200, CNSDTDSC=200, SRCDOM=8, AVAL_M=8, ASEQ=8)
varfmt <- c(STUDYID="", USUBJID="", TRTP="", TRTPN="", AGE="", AGEGR1="", SEX="", RACE="",
            SAFFL="", ITTFL="", PARAMCD="", PARAM="", PARAMN="", STARTDT="DATE9.", ADT="DATE9.", AVAL="",
            CNSR="", EVNTDESC="", CNSDTDSC="", SRCDOM="", AVAL_M="", ASEQ="")
varlab <- c(STUDYID="Study Identifier", USUBJID="Unique Subject Identifier", TRTP="Planned Treatment",
            TRTPN="Planned Treatment (N)", AGE="", AGEGR1="", SEX="", RACE="",
            SAFFL="Safety Population Flag", ITTFL="Intent-to-Treat Population Flag",
            PARAMCD="Parameter Code", PARAM="Parameter", PARAMN="Parameter (N)",
            STARTDT="Time-to-Event Origin Date", ADT="Analysis Date", AVAL="Analysis Value",
            CNSR="Censor", EVNTDESC="Event Description", CNSDTDSC="Censor Description",
            SRCDOM="Source Domain", AVAL_M="Analysis Value (Months)", ASEQ="Analysis Sequence Number")

lines <- c()
lines <- c(lines, "PROC CONTENTS DATA=ADTTE VARNUM; RUN;", "")
lines <- c(lines, paste0("Observations: ", nrow(adtte)), paste0("Variables: ", ncol(adtte)), "")
lines <- c(lines, "Variables in Creation Order (#  Variable  Type  Len  Format  Label):", "")
lines <- c(lines, sprintf("%-4s %-10s %-5s %-5s %-8s %s", "#", "Variable", "Type", "Len", "Format", "Label"))
for (i in seq_along(vars)) {
  v <- vars[i]
  lines <- c(lines, sprintf("%-4d %-10s %-5s %-5d %-8s %s", i, v, vartype[[v]], varlen[[v]], varfmt[[v]], varlab[[v]]))
}
lines <- c(lines, "", "PROC PRINT DATA=ADTTE NOOBS; RUN;", "")
hdr <- sprintf("%-8s %-13s %-13s %-5s %-4s %-6s %-1s %-6s %-1s %-1s %-7s %-32s %-6s %-9s %-9s %-5s %-4s %-20s %-32s %-6s %-10s %-4s",
  "STUDYID","USUBJID","TRTP","TRTPN","AGE","AGEGR1","S","RACE","S","I","PARAMCD","PARAM","PARAMN","STARTDT","ADT","AVAL","CNSR","EVNTDESC","CNSDTDSC","SRCDOM","AVAL_M","ASEQ")
# shorten header labels for fixed width
hdr2 <- paste("STUDYID USUBJID TRTP TRTPN AGE AGEGR1 SEX RACE SAFFL ITTFL PARAMCD PARAM PARAMN STARTDT ADT AVAL CNSR EVNTDESC CNSDTDSC SRCDOM AVAL_M ASEQ")
lines <- c(lines, hdr2)
for (i in seq_len(nrow(adtte))) {
  r <- adtte[i, ]
  # AVAL_M display with BEST12-like: use full value with 4 decimals for readability but keep exact
  avalm_disp <- ifelse(is.na(r$AVAL_M), ".", format(r$AVAL_M, digits = 12, nsmall = 4))
  avalm_disp <- trimws(avalm_disp)
  line <- paste(
    fmt_char(r$STUDYID),
    fmt_char(r$USUBJID),
    fmt_char(r$TRTP),
    fmt_num(r$TRTPN),
    fmt_num(r$AGE),
    fmt_char(r$AGEGR1),
    fmt_char(r$SEX),
    fmt_char(r$RACE),
    fmt_char(r$SAFFL),
    fmt_char(r$ITTFL),
    fmt_char(r$PARAMCD),
    fmt_char(r$PARAM),
    fmt_num(r$PARAMN),
    fmt_date9(r$STARTDT),
    fmt_date9(r$ADT),
    fmt_num(r$AVAL),
    fmt_num(r$CNSR),
    fmt_char(r$EVNTDESC),
    fmt_char(r$CNSDTDSC),
    fmt_char(r$SRCDOM),
    avalm_disp,
    fmt_num(r$ASEQ),
    sep = " | "
  )
  lines <- c(lines, line)
}
lines <- c(lines, "", paste0("N = ", nrow(adtte), " observations"))

writeLines(lines, file.path(out, "P086.txt"))
writeLines(lines, file.path(out, "P086.lst"))

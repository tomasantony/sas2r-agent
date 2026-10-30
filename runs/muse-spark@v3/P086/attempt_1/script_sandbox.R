out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-9)
  sign_x * z / mult
}

parse_date9 <- function(x) {
  # ONE strict format DDMMMYYYY e.g. 15JAN2024, "." = missing
  sapply(x, function(v) {
    if (is.na(v)) return(NA_real_)
    v <- trimws(as.character(v))
    if (v == "" || v == ".") return(NA_real_)
    # normalize month to Title case for %b
    # format is DDMMMYYYY : first 2 chars day, next 3 month, rest year
    if (nchar(v) < 9) return(NA_real_)
    dayp <- substr(v, 1, 2)
    monp <- substr(v, 3, 5)
    yearp <- substr(v, 6, nchar(v))
    mon_title <- paste0(toupper(substr(monp,1,1)), tolower(substr(monp,2,3)))
    v2 <- paste0(dayp, mon_title, yearp)
    d <- as.Date(v2, format = "%d%b%Y")
    as.numeric(d)
  }, USE.NAMES = FALSE) |> as.Date(origin = "1970-01-01")
}

fmt_date9 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    d <- as.Date(v, origin = "1970-01-01")
    dd <- format(d, "%d")
    mm <- toupper(format(d, "%b"))
    yy <- format(d, "%Y")
    paste0(dd, mm, yy)
  }, USE.NAMES = FALSE)
}

# ============================================================
# STEP 1: mock source data
# ============================================================
adsl <- data.frame(
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
  TRTSDT_c = c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"),
  TRTEDT_c = c("15JUN2024","18JUN2024","01APR2024","25JUN2024",".","01JUL2024"),
  EOSDT_c = c("15JUN2024","18JUN2024","01APR2024","25JUN2024","15JUL2024","01JUL2024"),
  DTHDT_c = c(".",".","15APR2024",".",".","."),
  DTHFL = c("N","N","Y","N","N","N"),
  stringsAsFactors = FALSE
)
adsl$TRTSDT <- parse_date9(adsl$TRTSDT_c)
adsl$TRTEDT <- parse_date9(adsl$TRTEDT_c)
adsl$EOSDT <- parse_date9(adsl$EOSDT_c)
adsl$DTHDT <- parse_date9(adsl$DTHDT_c)
adsl$TRTSDT_c <- NULL; adsl$TRTEDT_c <- NULL; adsl$EOSDT_c <- NULL; adsl$DTHDT_c <- NULL
# order columns as in SAS
adsl <- adsl[, c("STUDYID","USUBJID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","ITTFL","TRTSDT","TRTEDT","EOSDT","DTHDT","DTHFL")]

adrs <- data.frame(
  STUDYID = rep("STUDY01", 9),
  USUBJID = c("STUDY01-1001","STUDY01-1001","STUDY01-1002","STUDY01-1002","STUDY01-1003","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  RSSTRESC = c("STABLE DISEASE","STABLE DISEASE","STABLE DISEASE","PROGRESSIVE DISEASE","PARTIAL RESPONSE","PROGRESSIVE DISEASE","PARTIAL RESPONSE","STABLE DISEASE","COMPLETE RESPONSE"),
  RSDTC = c("2024-03-15","2024-06-15","2024-03-18","2024-05-20","2024-02-20","2024-03-25","2024-03-25","2024-04-28","2024-04-30"),
  stringsAsFactors = FALSE
)

# ============================================================
# STEP 2: PFS
# ============================================================
# sort adrs by USUBJID RSDTC (SAS sort)
adrs_sorted <- adrs[order(adrs$USUBJID, adrs$RSDTC), , drop = FALSE]

prog_sub <- adrs_sorted[adrs_sorted$RSSTRESC == "PROGRESSIVE DISEASE", , drop = FALSE]
# first per USUBJID
prog_sub <- prog_sub[!duplicated(prog_sub$USUBJID), , drop = FALSE]
prog_sub$PROGDT <- as.Date(prog_sub$RSDTC, format = "%Y-%m-%d")
prog_df <- prog_sub[, c("USUBJID","PROGDT"), drop = FALSE]

adsl_sorted <- adsl[order(adsl$USUBJID), , drop = FALSE]
prog_df_sorted <- prog_df[order(prog_df$USUBJID), , drop = FALSE]

adtte_pfs <- merge(adsl_sorted, prog_df_sorted, by = "USUBJID", all.x = TRUE, sort = TRUE)

# derive PFS fields
n_pfs <- nrow(adtte_pfs)
adtte_pfs$PARAMCD <- rep("PFS", n_pfs)
adtte_pfs$PARAM <- rep("Progression-Free Survival (Days)", n_pfs)
adtte_pfs$PARAMN <- rep(1, n_pfs)
adtte_pfs$STARTDT <- adtte_pfs$TRTSDT
adtte_pfs$ADT <- as.Date(rep(NA_real_, n_pfs), origin = "1970-01-01")
adtte_pfs$CNSR <- rep(NA_real_, n_pfs)
adtte_pfs$EVNTDESC <- rep("", n_pfs)
adtte_pfs$CNSDTDSC <- rep("", n_pfs)
adtte_pfs$SRCDOM <- rep("", n_pfs)
adtte_pfs$AVAL <- rep(NA_real_, n_pfs)

for (i in seq_len(n_pfs)) {
  PROGDT <- adtte_pfs$PROGDT[i]
  DTHDT <- adtte_pfs$DTHDT[i]
  EOSDT <- adtte_pfs$EOSDT[i]
  TRTEDT <- adtte_pfs$TRTEDT[i]
  TRTSDT <- adtte_pfs$TRTSDT[i]
  hasProg <- !is.na(PROGDT)
  hasDth <- !is.na(DTHDT)
  if (hasProg && hasDth) {
    if (PROGDT <= DTHDT) {
      adtte_pfs$ADT[i] <- PROGDT
      adtte_pfs$EVNTDESC[i] <- "Disease Progression"
      adtte_pfs$SRCDOM[i] <- "RS"
    } else {
      adtte_pfs$ADT[i] <- DTHDT
      adtte_pfs$EVNTDESC[i] <- "Death"
      adtte_pfs$SRCDOM[i] <- "ADSL"
    }
    adtte_pfs$CNSR[i] <- 0
    adtte_pfs$CNSDTDSC[i] <- ""
  } else if (hasProg) {
    adtte_pfs$ADT[i] <- PROGDT
    adtte_pfs$CNSR[i] <- 0
    adtte_pfs$EVNTDESC[i] <- "Disease Progression"
    adtte_pfs$CNSDTDSC[i] <- ""
    adtte_pfs$SRCDOM[i] <- "RS"
  } else if (hasDth) {
    adtte_pfs$ADT[i] <- DTHDT
    adtte_pfs$CNSR[i] <- 0
    adtte_pfs$EVNTDESC[i] <- "Death"
    adtte_pfs$CNSDTDSC[i] <- ""
    adtte_pfs$SRCDOM[i] <- "ADSL"
  } else {
    # coalesce(EOSDT, TRTEDT, TRTSDT)
    cdt <- EOSDT
    if (is.na(cdt)) cdt <- TRTEDT
    if (is.na(cdt)) cdt <- TRTSDT
    adtte_pfs$ADT[i] <- cdt
    adtte_pfs$CNSR[i] <- 1
    adtte_pfs$EVNTDESC[i] <- ""
    adtte_pfs$CNSDTDSC[i] <- "Censored - No progression or death"
    adtte_pfs$SRCDOM[i] <- "ADSL"
  }
  # AVAL
  adt <- adtte_pfs$ADT[i]
  sdt <- adtte_pfs$STARTDT[i]
  if (!is.na(adt) && !is.na(sdt)) {
    av <- as.numeric(adt - sdt + 1)
    if (av < 1) av <- 1
    adtte_pfs$AVAL[i] <- av
  }
}
adtte_pfs <- adtte_pfs[, c("STUDYID","USUBJID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","ITTFL","PARAMCD","PARAM","PARAMN","STARTDT","ADT","AVAL","CNSR","EVNTDESC","CNSDTDSC","SRCDOM")]

# ============================================================
# STEP 3: OS
# ============================================================
adtte_os <- adsl
n_os <- nrow(adtte_os)
adtte_os$PARAMCD <- rep("OS", n_os)
adtte_os$PARAM <- rep("Overall Survival (Days)", n_os)
adtte_os$PARAMN <- rep(2, n_os)
adtte_os$STARTDT <- adtte_os$TRTSDT
adtte_os$ADT <- as.Date(rep(NA_real_, n_os), origin = "1970-01-01")
adtte_os$CNSR <- rep(NA_real_, n_os)
adtte_os$EVNTDESC <- rep("", n_os)
adtte_os$CNSDTDSC <- rep("", n_os)
adtte_os$SRCDOM <- rep("", n_os)
adtte_os$AVAL <- rep(NA_real_, n_os)
for (i in seq_len(n_os)) {
  DTHFL <- adtte_os$DTHFL[i]
  DTHDT <- adtte_os$DTHDT[i]
  if (!is.na(DTHFL) && DTHFL == "Y" && !is.na(DTHDT)) {
    adtte_os$ADT[i] <- DTHDT
    adtte_os$CNSR[i] <- 0
    adtte_os$EVNTDESC[i] <- "Death"
    adtte_os$CNSDTDSC[i] <- ""
    adtte_os$SRCDOM[i] <- "ADSL"
  } else {
    cdt <- adtte_os$EOSDT[i]
    if (is.na(cdt)) cdt <- adtte_os$TRTEDT[i]
    if (is.na(cdt)) cdt <- adtte_os$TRTSDT[i]
    adtte_os$ADT[i] <- cdt
    adtte_os$CNSR[i] <- 1
    adtte_os$EVNTDESC[i] <- ""
    adtte_os$CNSDTDSC[i] <- "Censored - Alive"
    adtte_os$SRCDOM[i] <- "ADSL"
  }
  adt <- adtte_os$ADT[i]; sdt <- adtte_os$STARTDT[i]
  if (!is.na(adt) && !is.na(sdt)) {
    av <- as.numeric(adt - sdt + 1)
    if (av < 1) av <- 1
    adtte_os$AVAL[i] <- av
  }
}
adtte_os <- adtte_os[, c("STUDYID","USUBJID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","ITTFL","PARAMCD","PARAM","PARAMN","STARTDT","ADT","AVAL","CNSR","EVNTDESC","CNSDTDSC","SRCDOM")]

# ============================================================
# STEP 4: TTR
# ============================================================
resp_sub <- adrs_sorted[adrs_sorted$RSSTRESC %in% c("COMPLETE RESPONSE","PARTIAL RESPONSE"), , drop = FALSE]
resp_sub <- resp_sub[!duplicated(resp_sub$USUBJID), , drop = FALSE]
resp_sub$RESPDT <- as.Date(resp_sub$RSDTC, format = "%Y-%m-%d")
resp_df <- resp_sub[, c("USUBJID","RESPDT"), drop = FALSE]
resp_df_sorted <- resp_df[order(resp_df$USUBJID), , drop = FALSE]

adtte_ttr <- merge(adsl_sorted, resp_df_sorted, by = "USUBJID", all.x = TRUE, sort = TRUE)
n_ttr <- nrow(adtte_ttr)
adtte_ttr$PARAMCD <- rep("TTR", n_ttr)
adtte_ttr$PARAM <- rep("Time to Response (Days)", n_ttr)
adtte_ttr$PARAMN <- rep(3, n_ttr)
adtte_ttr$STARTDT <- adtte_ttr$TRTSDT
adtte_ttr$ADT <- as.Date(rep(NA_real_, n_ttr), origin = "1970-01-01")
adtte_ttr$CNSR <- rep(NA_real_, n_ttr)
adtte_ttr$EVNTDESC <- rep("", n_ttr)
adtte_ttr$CNSDTDSC <- rep("", n_ttr)
adtte_ttr$AVAL <- rep(NA_real_, n_ttr)
for (i in seq_len(n_ttr)) {
  RESPDT <- adtte_ttr$RESPDT[i]
  if (!is.na(RESPDT)) {
    adtte_ttr$ADT[i] <- RESPDT
    adtte_ttr$CNSR[i] <- 0
    adtte_ttr$EVNTDESC[i] <- "Response"
    adtte_ttr$CNSDTDSC[i] <- ""
  } else {
    cdt <- adtte_ttr$EOSDT[i]
    if (is.na(cdt)) cdt <- adtte_ttr$TRTEDT[i]
    if (is.na(cdt)) cdt <- adtte_ttr$TRTSDT[i]
    adtte_ttr$ADT[i] <- cdt
    adtte_ttr$CNSR[i] <- 1
    adtte_ttr$EVNTDESC[i] <- ""
    adtte_ttr$CNSDTDSC[i] <- "Censored - No response"
  }
  adt <- adtte_ttr$ADT[i]; sdt <- adtte_ttr$STARTDT[i]
  if (!is.na(adt) && !is.na(sdt)) {
    av <- as.numeric(adt - sdt + 1)
    if (av < 1) av <- 1
    adtte_ttr$AVAL[i] <- av
  }
}
adtte_ttr <- adtte_ttr[, c("STUDYID","USUBJID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","ITTFL","PARAMCD","PARAM","PARAMN","STARTDT","ADT","AVAL","CNSR","EVNTDESC","CNSDTDSC")]

# ============================================================
# STEP 5: combine
# ============================================================
# emulate SAS SET (keep all columns, missing SRCDOM for TTR)
adtte_ttr$SRCDOM <- ""
# ensure same column order
cols_all <- c("STUDYID","USUBJID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","ITTFL","PARAMCD","PARAM","PARAMN","STARTDT","ADT","AVAL","CNSR","EVNTDESC","CNSDTDSC","SRCDOM")
adtte_pfs <- adtte_pfs[, cols_all]
adtte_os <- adtte_os[, cols_all]
adtte_ttr2 <- adtte_ttr[, cols_all]
# for TTR event rows SAS leaves CNSDTDSC/SRCDOM missing -> set SRCDOM to NA then ""? SAS missing char = "".
# Keep event SRCDOM as "" (missing) to match SAS; but for display keep "".
# Actually for TTR responders SRCDOM should be missing. Set to NA then handled as "" on write.
# To preserve SAS semantics, set SRCDOM=NA for responders
is_resp <- adtte_ttr2$CNSR == 0
adtte_ttr2$SRCDOM[is_resp] <- NA_character_
# CNSDTDSC for responders is missing -> NA
adtte_ttr2$CNSDTDSC[is_resp] <- NA_character_

adtte <- rbind(adtte_pfs, adtte_os, adtte_ttr2)

# AVAL_M
adtte$AVAL_M <- ifelse(!is.na(adtte$AVAL), adtte$AVAL / 30.4375, NA_real_)

# ============================================================
# STEP 6: sort + ASEQ + labels
# ============================================================
adtte <- adtte[order(adtte$STUDYID, adtte$USUBJID, adtte$PARAMN), , drop = FALSE]
# ASEQ by STUDYID USUBJID
adtte$ASEQ <- NA_real_
subjects <- unique(paste(adtte$STUDYID, adtte$USUBJID, sep = "|"))
for (s in subjects) {
  idx <- which(paste(adtte$STUDYID, adtte$USUBJID, sep = "|") == s)
  # already ordered by PARAMN
  adtte$ASEQ[idx] <- seq_along(idx)
}
# final column order: existing + AVAL_M + ASEQ
adtte <- adtte[, c("STUDYID","USUBJID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","ITTFL","PARAMCD","PARAM","PARAMN","STARTDT","ADT","AVAL","CNSR","EVNTDESC","CNSDTDSC","SRCDOM","AVAL_M","ASEQ")]
rownames(adtte) <- NULL

# sort component datasets for output consistency
adtte_pfs <- adtte_pfs[order(adtte_pfs$USUBJID), , drop = FALSE]
adtte_os <- adtte_os[order(adtte_os$USUBJID), , drop = FALSE]
adtte_ttr_out <- adtte_ttr[, c("STUDYID","USUBJID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","ITTFL","PARAMCD","PARAM","PARAMN","STARTDT","ADT","AVAL","CNSR","EVNTDESC","CNSDTDSC")]
adtte_ttr_out <- adtte_ttr_out[order(adtte_ttr_out$USUBJID), , drop = FALSE]
# for adtte_ttr csv, set missing CNSDTDSC to NA for responders (SAS missing)
adtte_ttr_out$CNSDTDSC[adtte_ttr_out$CNSR == 0] <- NA_character_

# ============================================================
# Write datasets as CSV (ISO dates)
# ============================================================
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(adrs, file.path(out, "adrs.csv"), row.names = FALSE, na = "")
write.csv(adtte_pfs, file.path(out, "adtte_pfs.csv"), row.names = FALSE, na = "")
write.csv(adtte_os, file.path(out, "adtte_os.csv"), row.names = FALSE, na = "")
write.csv(adtte_ttr_out, file.path(out, "adtte_ttr.csv"), row.names = FALSE, na = "")
write.csv(adtte, file.path(out, "adtte.csv"), row.names = FALSE, na = "")

# ============================================================
# Listing P086 : CONTENTS + PRINT
# ============================================================
# Build contents metadata (varnum order)
var_names <- names(adtte)
var_type <- sapply(adtte, function(c) if (inherits(c, "Date")) "Num" else if (is.numeric(c)) "Num" else "Char")
var_len <- c(STUDYID=12, USUBJID=20, TRTP=40, TRTPN=8, AGE=8, AGEGR1=10, SEX=1, RACE=50, SAFFL=1, ITTFL=1, PARAMCD=8, PARAM=200, PARAMN=8, STARTDT=8, ADT=8, AVAL=8, CNSR=8, EVNTDESC=200, CNSDTDSC=200, SRCDOM=8, AVAL_M=8, ASEQ=8)
var_fmt <- c(STUDYID="", USUBJID="", TRTP="", TRTPN="", AGE="", AGEGR1="", SEX="", RACE="", SAFFL="", ITTFL="", PARAMCD="", PARAM="", PARAMN="", STARTDT="DATE9.", ADT="DATE9.", AVAL="", CNSR="", EVNTDESC="", CNSDTDSC="", SRCDOM="", AVAL_M="", ASEQ="")
var_label <- c(STUDYID="Study Identifier", USUBJID="Unique Subject Identifier", TRTP="Planned Treatment", TRTPN="Planned Treatment (N)", AGE="", AGEGR1="", SEX="", RACE="", SAFFL="Safety Population Flag", ITTFL="Intent-to-Treat Population Flag", PARAMCD="Parameter Code", PARAM="Parameter", PARAMN="Parameter (N)", STARTDT="Time-to-Event Origin Date", ADT="Analysis Date", AVAL="Analysis Value", CNSR="Censor", EVNTDESC="Event Description", CNSDTDSC="Censor Description", SRCDOM="Source Domain", AVAL_M="Analysis Value (Months)", ASEQ="Analysis Sequence Number")

contents_df <- data.frame(
  VarNum = seq_along(var_names),
  Variable = var_names,
  Type = var_type[var_names],
  Length = as.numeric(var_len[var_names]),
  Format = as.character(var_fmt[var_names]),
  Label = as.character(var_label[var_names]),
  stringsAsFactors = FALSE
)

# Build print display: SAS DATE9. for dates, BEST for numerics
print_df <- adtte
print_df$STARTDT_d9 <- fmt_date9(print_df$STARTDT)
print_df$ADT_d9 <- fmt_date9(print_df$ADT)

# Text rendering
txt_path <- file.path(out, "P086.txt")
lst_path <- file.path(out, "P086.lst")
con <- file(txt_path, open = "wt")
writeLines("CONTENTS OF ADTTE", con)
writeLines(paste0("Observations: ", nrow(adtte), "  Variables: ", ncol(adtte)), con)
writeLines("", con)
hdr <- sprintf("%-7s %-10s %-5s %-7s %-8s %s", "VarNum", "Variable", "Type", "Length", "Format", "Label")
writeLines(hdr, con)
for (i in seq_len(nrow(contents_df))) {
  line <- sprintf("%-7d %-10s %-5s %-7d %-8s %s", contents_df$VarNum[i], contents_df$Variable[i], contents_df$Type[i], contents_df$Length[i], contents_df$Format[i], contents_df$Label[i])
  writeLines(line, con)
}
writeLines("", con)
writeLines("PRINT OF ADTTE (noobs)", con)
writeLines("", con)
# header row for print
phdr <- paste(names(adtte), collapse = " | ")
writeLines(phdr, con)
for (i in seq_len(nrow(adtte))) {
  r <- adtte[i, , drop = FALSE]
  vals <- sapply(names(adtte), function(cn) {
    v <- r[[cn]]
    if (cn %in% c("STARTDT","ADT")) {
      if (is.na(v)) return("")
      return(fmt_date9(v))
    } else if (is.numeric(v)) {
      if (is.na(v)) return("")
      if (cn == "AVAL_M") {
        # SAS BEST12. default: print full value
        return(format(v, scientific = FALSE, digits = 12, nsmall = 4))
      } else {
        # integers
        if (v == floor(v)) return(sprintf("%d", as.integer(v)))
        else return(as.character(v))
      }
    } else {
      if (is.na(v)) return("")
      return(as.character(v))
    }
  }, USE.NAMES = FALSE)
  writeLines(paste(vals, collapse = " | "), con)
}
close(con)
# duplicate as .lst
file.copy(txt_path, lst_path, overwrite = TRUE)

# CSV of table body (same as adtte with ISO dates)
write.csv(adtte, file.path(out, "P086.csv"), row.names = FALSE, na = "")

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive=TRUE, showWarnings=FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(stringr)
  library(lubridate)
})

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

read_adam <- function(libpath, name){
  candidates <- list.files(libpath, pattern=paste0("^", name, "\\."), ignore.case=TRUE, full.names=TRUE)
  if(length(candidates)==0){
    candidates <- list.files(libpath, ignore.case=TRUE, full.names=TRUE)
    candidates <- candidates[grepl(paste0("^", name, "\\."), basename(candidates), ignore.case=TRUE)]
  }
  if(length(candidates)==0) stop(paste0("Dataset not found: ", name, " in ", libpath))
  f <- candidates[1]
  ext <- tolower(tools::file_ext(f))
  df <- if(ext=="sas7bdat") haven::read_sas(f) else haven::read_xpt(f)
  df
}

# helper to find column case-insensitive
find_col <- function(df, candidates){
  un <- toupper(names(df))
  for(c in candidates){
    idx <- which(un==toupper(c))
    if(length(idx)>0) return(names(df)[idx[1]])
  }
  return(NA_character_)
}
get_col_vec <- function(df, colname, default=NA_character_){
  if(!is.na(colname) && colname %in% names(df)) return(df[[colname]])
  rep(default, nrow(df))
}

catx <- function(sep, ...){
  args <- c(...)
  args <- args[!is.na(args) & trimws(as.character(args))!=""]
  if(length(args)==0) return("")
  paste(args, collapse=sep)
}
parse_yymmdd10 <- function(x){
  if(is.na(x) || trimws(as.character(x))=="") return(as.Date(NA))
  s <- substr(trimws(as.character(x)),1,10)
  d <- suppressWarnings(as.Date(s, format="%Y-%m-%d"))
  if(!is.na(d)) return(d)
  d <- suppressWarnings(as.Date(s, format="%Y/%m/%d"))
  if(!is.na(d)) return(d)
  d2 <- suppressWarnings(lubridate::ymd(s, quiet=TRUE))
  if(!is.na(d2)) return(as.Date(d2))
  as.Date(NA)
}

adsl_raw <- read_adam(adam_path, "adsl")
adae_raw <- read_adam(adam_path, "adae")

# convert haven_labelled to plain
for(nm in names(adsl_raw)) if(haven::is.labelled(adsl_raw[[nm]])) adsl_raw[[nm]] <- haven::as_factor(adsl_raw[[nm]], levels="labels")
for(nm in names(adae_raw)) if(haven::is.labelled(adae_raw[[nm]])) adae_raw[[nm]] <- haven::as_factor(adae_raw[[nm]], levels="labels")
adsl_raw <- adsl_raw %>% mutate(across(where(is.factor), as.character))
adae_raw <- adae_raw %>% mutate(across(where(is.factor), as.character))

# keep original names for lookup, also create upper map
names(adsl_raw) <- toupper(names(adsl_raw))
names(adae_raw) <- toupper(names(adae_raw))

# debug log
cat("ADSL cols:", paste(names(adsl_raw), collapse=","), "\n")
cat("ADAE cols:", paste(names(adae_raw), collapse=","), "\n")

# locate key columns
usubjid_adsl <- find_col(adsl_raw, c("USUBJID"))
usubjid_adae <- find_col(adae_raw, c("USUBJID"))
trta_col <- find_col(adsl_raw, c("TRTA","TRT01A","TRT01P","TRTP","TRTA1","TRTAN"))
trtsdt_col <- find_col(adsl_raw, c("TRTSDT","TRTSDTM","TRT01SDT","TRTSTDT","TRT01SDTM","TRTSDT1","TRTSTDTM"))
age_col <- find_col(adsl_raw, c("AGE","AGEGR"))
sex_col <- find_col(adsl_raw, c("SEX"))
race_col <- find_col(adsl_raw, c("RACE","RACE1","RACEN"))
saffl_col <- find_col(adsl_raw, c("SAFFL","SAFFLN"))

aebodsys_col <- find_col(adae_raw, c("AEBODSYS","AEBODYSYS","AESOC","AEBODSYS"))
aedecod_col  <- find_col(adae_raw, c("AEDECOD"))
aeterm_col   <- find_col(adae_raw, c("AETERM"))
aestdtc_col  <- find_col(adae_raw, c("AESTDTC","AESTDT","AESTDTC"))
aeendtc_col  <- find_col(adae_raw, c("AEENDTC","AEENDT"))
aetoxgr_col  <- find_col(adae_raw, c("AETOXGR","AETOXG","AESEV","AETOXGRN"))
aerel_col    <- find_col(adae_raw, c("AEREL","AERELAT"))
aeacn_col    <- find_col(adae_raw, c("AEACN","AEACNN"))
aeout_col    <- find_col(adae_raw, c("AEOUT","AEOUTCOME"))
aesdth_col   <- find_col(adae_raw, c("AESDTH"))
aeslife_col  <- find_col(adae_raw, c("AESLIFE","AESLIF"))
aeshosp_col  <- find_col(adae_raw, c("AESHOSP"))
aesdisab_col <- find_col(adae_raw, c("AESDISAB","AESDISA"))
aescong_col  <- find_col(adae_raw, c("AESCONG"))
aesmie_col   <- find_col(adae_raw, c("AESMIE"))
aeser_col    <- find_col(adae_raw, c("AESER"))

# build adsl_f
adsl <- adsl_raw
# standardize TRT_START
if(!is.na(trtsdt_col)){
  v <- adsl[[trtsdt_col]]
  if(is.numeric(v) && !inherits(v,"Date") && !inherits(v,"POSIXt")){
    mx <- suppressWarnings(max(v, na.rm=TRUE))
    if(!is.na(mx) && mx > 1e6){
      v <- as.Date(as.POSIXct(v, origin="1960-01-01", tz="UTC"))
    } else {
      v <- as.Date(v, origin="1960-01-01")
    }
  } else {
    v <- as.Date(v)
  }
  adsl$TRT_START <- v
} else {
  adsl$TRT_START <- as.Date(NA)
}
# standardize TRTA
if(!is.na(trta_col)) adsl$TRTA_STD <- as.character(adsl[[trta_col]]) else adsl$TRTA_STD <- NA_character_
if(!is.na(age_col)) adsl$AGE_STD <- suppressWarnings(as.numeric(adsl[[age_col]])) else adsl$AGE_STD <- NA_real_
if(!is.na(sex_col)) adsl$SEX_STD <- as.character(adsl[[sex_col]]) else adsl$SEX_STD <- NA_character_
if(!is.na(race_col)) adsl$RACE_STD <- as.character(adsl[[race_col]]) else adsl$RACE_STD <- NA_character_
if(!is.na(saffl_col)) adsl$SAFFL_STD <- as.character(adsl[[saffl_col]]) else adsl$SAFFL_STD <- "Y"
if(!is.na(usubjid_adsl)) adsl$USUBJID_STD <- as.character(adsl[[usubjid_adsl]]) else adsl$USUBJID_STD <- NA_character_

adsl_f <- adsl %>% filter(SAFFL_STD=="Y")
if(nrow(adsl_f)==0) adsl_f <- adsl

adsl_keep <- adsl_f %>% transmute(USUBJID=USUBJID_STD, TRTA=TRTA_STD, AGE=AGE_STD, SEX=SEX_STD, RACE=RACE_STD, TRT_START=TRT_START)

# build adae_f
adae <- adae_raw
if(!is.na(usubjid_adae)) adae$USUBJID_STD <- as.character(adae[[usubjid_adae]]) else adae$USUBJID_STD <- NA_character_
# filter AESER=Y if column exists
if(!is.na(aeser_col)){
  adae_f <- adae %>% filter(toupper(as.character(.data[[aeser_col]]))=="Y")
} else {
  adae_f <- adae
}
# remove TRTA-like cols from adae to avoid duplication
drop_cols <- intersect(names(adae_f), c("TRTA","TRTA_STD","TRT01A","TRT01P","TRTP"))
if(length(drop_cols)>0) adae_f <- adae_f %>% select(-all_of(drop_cols))

# create standardized adae columns
adae_f$SOC_STD  <- get_col_vec(adae_f, aebodsys_col, NA_character_)
adae_f$PT_STD   <- get_col_vec(adae_f, aedecod_col, NA_character_)
adae_f$VERBATIM_STD <- get_col_vec(adae_f, aeterm_col, NA_character_)
adae_f$START_DATE_STD <- get_col_vec(adae_f, aestdtc_col, NA_character_)
adae_f$END_DATE_STD   <- get_col_vec(adae_f, aeendtc_col, NA_character_)
adae_f$GRADE_STD <- get_col_vec(adae_f, aetoxgr_col, NA_character_)
adae_f$REL_STD   <- get_col_vec(adae_f, aerel_col, NA_character_)
adae_f$ACN_STD   <- get_col_vec(adae_f, aeacn_col, NA_character_)
adae_f$OUT_STD   <- get_col_vec(adae_f, aeout_col, NA_character_)
adae_f$AESDTH_STD <- get_col_vec(adae_f, aesdth_col, NA_character_)
adae_f$AESLIFE_STD <- get_col_vec(adae_f, aeslife_col, NA_character_)
adae_f$AESHOSP_STD <- get_col_vec(adae_f, aeshosp_col, NA_character_)
adae_f$AESDISAB_STD <- get_col_vec(adae_f, aesdisab_col, NA_character_)
adae_f$AESCONG_STD <- get_col_vec(adae_f, aescong_col, NA_character_)
adae_f$AESMIE_STD <- get_col_vec(adae_f, aesmie_col, NA_character_)

# join
sae_list <- adae_f %>%
  inner_join(adsl_keep, by=c("USUBJID_STD"="USUBJID")) %>%
  transmute(
    USUBJID = USUBJID_STD,
    TRTA = TRTA,
    AGE = AGE,
    SEX = SEX,
    RACE = RACE,
    TRT_START = TRT_START,
    SOC = as.character(SOC_STD),
    PT = as.character(PT_STD),
    VERBATIM = as.character(VERBATIM_STD),
    START_DATE = as.character(START_DATE_STD),
    END_DATE = as.character(END_DATE_STD),
    GRADE = as.character(GRADE_STD),
    RELATIONSHIP = as.character(REL_STD),
    ACTION_TAKEN = as.character(ACN_STD),
    OUTCOME = as.character(OUT_STD),
    AESDTH = as.character(AESDTH_STD),
    AESLIFE = as.character(AESLIFE_STD),
    AESHOSP = as.character(AESHOSP_STD),
    AESDISAB = as.character(AESDISAB_STD),
    AESCONG = as.character(AESCONG_STD),
    AESMIE = as.character(AESMIE_STD)
  ) %>%
  arrange(TRTA, USUBJID, START_DATE)

# derive
sae_report <- sae_list %>%
  rowwise() %>%
  mutate(
    sae_criteria = {
      sc <- ""
      if(!is.na(AESDTH) && toupper(AESDTH)=="Y") sc <- catx(", ", sc, "Death")
      if(!is.na(AESLIFE) && toupper(AESLIFE)=="Y") sc <- catx(", ", sc, "Life-threatening")
      if(!is.na(AESHOSP) && toupper(AESHOSP)=="Y") sc <- catx(", ", sc, "Hospitalization")
      if(!is.na(AESDISAB) && toupper(AESDISAB)=="Y") sc <- catx(", ", sc, "Disability")
      if(!is.na(AESCONG) && toupper(AESCONG)=="Y") sc <- catx(", ", sc, "Congenital anomaly")
      if(!is.na(AESMIE) && toupper(AESMIE)=="Y") sc <- catx(", ", sc, "Medically important")
      sc
    },
    onset_day = {
      if(!is.na(TRT_START) && !is.na(START_DATE) && trimws(START_DATE)!=""){
        d <- parse_yymmdd10(START_DATE)
        if(!is.na(d) && !is.na(TRT_START)){
          as.character(as.integer(d - as.Date(TRT_START) + 1L))
        } else ""
      } else ""
    },
    demog = {
      age_str <- ifelse(is.na(AGE), "", trimws(format(AGE, trim=TRUE, scientific=FALSE, nsmall=0)))
      age_str <- sub("\\.0+$","", age_str)
      sex_str <- ifelse(is.na(SEX), "", trimws(as.character(SEX)))
      race_str <- ifelse(is.na(RACE), "", substr(trimws(as.character(RACE)),1,1))
      catx("/", age_str, sex_str, race_str)
    }
  ) %>%
  ungroup()

final_cols <- c("TRTA","USUBJID","demog","SOC","PT","START_DATE","END_DATE","onset_day","GRADE","RELATIONSHIP","ACTION_TAKEN","OUTCOME","sae_criteria")
sae_report_final <- sae_report %>%
  select(all_of(final_cols)) %>%
  mutate(across(everything(), ~as.character(.)))
sae_report_final[is.na(sae_report_final)] <- ""

# write CSV
write.csv(sae_report_final, file.path(out, "l_sae.csv"), row.names=FALSE, na="")

# build text rendering containing all numbers
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"

title_lines <- c(
  paste0(sponsor, "                                                  Page 1 of 1"),
  paste0("Protocol: ", protocol),
  "",
  "Listing 16.2.7.2",
  "Listing of Serious Adverse Events",
  "Safety Analysis Set",
  ""
)
col_header <- paste("Treatment","Subject ID","Age/Sex/Race","SOC","Preferred Term","Start Date","End Date","Study Day","Grade","Relationship","Action Taken","Outcome","SAE Criteria", sep=" | ")
sep_line <- paste(rep("-", 200), collapse="")

body_lines <- c(title_lines, sep_line, col_header, sep_line)
for(i in seq_len(nrow(sae_report_final))){
  r <- sae_report_final[i,]
  line <- paste(r$TRTA, r$USUBJID, r$demog, r$SOC, r$PT, r$START_DATE, r$END_DATE, r$onset_day, r$GRADE, r$RELATIONSHIP, r$ACTION_TAKEN, r$OUTCOME, r$sae_criteria, sep=" | ")
  body_lines <- c(body_lines, line)
}
body_lines <- c(body_lines, sep_line,
  paste0("Data cutoff: ", cutdate),
  "Source: ADaM datasets",
  paste0("Program: P079_l_sae.sas | Run: ", format(Sys.Date(), "%d%b%Y"))
)

writeLines(body_lines, file.path(out, "l_sae.txt"), useBytes=TRUE)
writeLines(body_lines, file.path(out, "l_sae.rtf"), useBytes=TRUE)

cat("P079_l_sae.R completed successfully.\n")
cat(paste0("Rows: ", nrow(sae_report_final), "\n"))

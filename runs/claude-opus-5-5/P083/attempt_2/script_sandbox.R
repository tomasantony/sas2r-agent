# P083 - ADSL (Subject-Level Analysis Dataset) - translated from P083_adsl.sas
suppressPackageStartupMessages({
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Read pipe-delimited lines like SAS list input: a field of "." becomes missing.
read_pipe <- function(lines, names) {
  rows <- lapply(lines, function(l) {
    v <- strsplit(l, "|", fixed = TRUE)[[1]]
    length(v) <- length(names)
    v[is.na(v)] <- ""
    v <- trimws(v)
    v[v == "."] <- ""
    v
  })
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(df) <- names
  df
}

# ---------------- STEP 1: mock SDTM data ----------------
dm_lines <- c(
  "STUDY01|STUDY01-1001|1001|101|M|WHITE|NOT HISPANIC|TRT|Treatment A|USA|1965-03-15|2024-01-10|2024-01-15|2024-06-15|2024-01-15|2024-06-15|59",
  "STUDY01|STUDY01-1002|1002|101|F|BLACK OR AFRICAN AMERICAN|NOT HISPANIC|PBO|Placebo|USA|1978-08-22|2024-01-12|2024-01-18|2024-06-18|2024-01-18|2024-06-18|45",
  "STUDY01|STUDY01-1003|1003|102|M|ASIAN|HISPANIC OR LATINO|TRT|Treatment A|USA|1980-11-01|2024-01-15|2024-01-20|2024-04-01|2024-01-20|2024-04-01|43",
  "STUDY01|STUDY01-1004|1004|102|F|WHITE|NOT HISPANIC|TRT|Treatment A|USA|1972-06-30|2024-01-18|2024-01-25|2024-06-25|2024-01-25|2024-06-25|51",
  "STUDY01|STUDY01-1005|1005|103|M|ASIAN|NOT HISPANIC|PBO|Placebo|USA|1988-02-14|2024-01-20|2024-01-28|.|2024-01-28|.|36",
  "STUDY01|STUDY01-1006|1006|103|F|WHITE|HISPANIC OR LATINO|TRT|Treatment A|USA|1955-12-05|2024-01-22|2024-01-30|2024-07-01|2024-01-30|2024-07-01|68"
)
dm <- read_pipe(dm_lines, c("STUDYID","USUBJID","SUBJID","SITEID","SEX","RACE","ETHNIC",
                            "ARMCD","ARM","COUNTRY","BRTHDTC","RFICDTC","RFSTDTC",
                            "RFENDTC","RFXSTDTC","RFXENDTC","AGE"))
dm$AGE <- as.numeric(dm$AGE)

ds_lines <- c(
  "STUDY01|STUDY01-1001|COMPLETED|COMPLETED|DISPOSITION EVENT|2024-06-15",
  "STUDY01|STUDY01-1002|COMPLETED|COMPLETED|DISPOSITION EVENT|2024-06-18",
  "STUDY01|STUDY01-1003|ADVERSE EVENT|ADVERSE EVENT|DISPOSITION EVENT|2024-04-01",
  "STUDY01|STUDY01-1004|COMPLETED|COMPLETED|DISPOSITION EVENT|2024-06-25",
  "STUDY01|STUDY01-1005|ONGOING|ONGOING|DISPOSITION EVENT|.",
  "STUDY01|STUDY01-1006|COMPLETED|COMPLETED|DISPOSITION EVENT|2024-07-01"
)
ds <- read_pipe(ds_lines, c("STUDYID","USUBJID","DSTERM","DSDECOD","DSCAT","DSSTDTC"))

# ---------------- STEP 2: derive ADSL ----------------
to_date <- function(x) as.Date(ifelse(x == "", NA, x), format = "%Y-%m-%d")

adsl <- dm %>%
  mutate(
    TRTSDT = to_date(RFXSTDTC),
    TRTEDT = to_date(RFXENDTC),
    RANDDT = to_date(RFSTDTC),
    TRTP = ARM,
    TRTPN = ifelse(ARMCD == "TRT", 1, ifelse(ARMCD == "PBO", 2, NA_real_)),
    TRTA = TRTP,
    TRTAN = TRTPN,
    AGEGR1 = ifelse(is.na(AGE) | AGE < 65, "<65", ">=65"),
    AGEGR1N = ifelse(AGEGR1 == "<65", 1, 2),
    RACEN = case_when(RACE == "WHITE" ~ 1,
                      RACE == "BLACK OR AFRICAN AMERICAN" ~ 2,
                      RACE == "ASIAN" ~ 3,
                      TRUE ~ 4),
    SEXN = ifelse(SEX == "M", 1, ifelse(SEX == "F", 2, NA_real_)),
    SAFFL = "Y", ITTFL = "Y", EFFFL = "Y", COMPLFL = "",
    DCSREAS = "", EOSSTT = "",
    TRTDURD = ifelse(!is.na(TRTSDT) & !is.na(TRTEDT),
                     as.numeric(TRTEDT - TRTSDT) + 1, NA_real_)
  )

adsl <- adsl %>% arrange(STUDYID, USUBJID)
ds <- ds %>% arrange(STUDYID, USUBJID)

adsl <- adsl %>%
  left_join(ds %>% select(STUDYID, USUBJID, DSTERM, DSDECOD), by = c("STUDYID","USUBJID")) %>%
  mutate(
    DSDECOD = ifelse(is.na(DSDECOD), "", DSDECOD),
    EOSSTT = case_when(DSDECOD == "COMPLETED" ~ "COMPLETED",
                       DSDECOD == "ONGOING" ~ "ONGOING",
                       TRUE ~ "DISCONTINUED"),
    COMPLFL = ifelse(DSDECOD == "COMPLETED", "Y", "N"),
    DCSREAS = ifelse(DSDECOD %in% c("COMPLETED","ONGOING"), "", DSDECOD)
  ) %>%
  select(-DSTERM, -DSDECOD) %>%
  arrange(STUDYID, USUBJID)

varorder <- c("STUDYID","USUBJID","SUBJID","SITEID","SEX","RACE","ETHNIC","ARMCD","ARM",
              "COUNTRY","BRTHDTC","RFICDTC","RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","AGE",
              "TRTSDT","TRTEDT","RANDDT","TRTP","TRTPN","TRTA","TRTAN","AGEGR1","AGEGR1N",
              "RACEN","SEXN","SAFFL","ITTFL","EFFFL","COMPLFL","DCSREAS","EOSSTT","TRTDURD")
adsl <- adsl[, varorder]

# ---------------- Output dataset ----------------
adsl_csv <- adsl
for (v in c("TRTSDT","TRTEDT","RANDDT")) adsl_csv[[v]] <- format(adsl_csv[[v]], "%Y-%m-%d")
write.csv(adsl_csv, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# ---------------- Listing: PROC CONTENTS (varnum) + PROC PRINT ----------------
lens <- c(12,20,8,4,1,50,30,8,40,3,10,10,10,10,10,10,8,8,8,8,40,8,40,8,20,8,8,8,1,1,1,1,50,20,8)
labels <- c(STUDYID="Study Identifier", USUBJID="Unique Subject Identifier",
            SUBJID="Subject Identifier for the Study", SITEID="Study Site Identifier",
            SEX="Sex", RACE="Race", ETHNIC="Ethnicity", ARMCD="Planned Arm Code",
            ARM="Description of Planned Arm", COUNTRY="Country", AGE="Age",
            TRTSDT="Date of First Exposure to Treatment", TRTEDT="Date of Last Exposure to Treatment",
            RANDDT="Date of Randomization", TRTP="Planned Treatment", TRTPN="Planned Treatment (N)",
            TRTA="Actual Treatment", TRTAN="Actual Treatment (N)", AGEGR1="Age Group 1",
            AGEGR1N="Age Group 1 (N)", RACEN="Race (N)", SEXN="Sex (N)",
            SAFFL="Safety Population Flag", ITTFL="Intent-to-Treat Population Flag",
            EFFFL="Efficacy Population Flag", COMPLFL="Completers Population Flag",
            DCSREAS="Reason for Discontinuation", EOSSTT="End of Study Status",
            TRTDURD="Total Treatment Duration (Days)")
types <- ifelse(sapply(adsl, function(x) is.numeric(x) || inherits(x, "Date")), "Num", "Char")
contents <- data.frame(
  Num = seq_along(varorder), Variable = varorder, Type = types, Len = lens,
  Format = ifelse(varorder %in% c("TRTSDT","TRTEDT","RANDDT"), "DATE9.", ""),
  Label = ifelse(varorder %in% names(labels), labels[varorder], ""),
  stringsAsFactors = FALSE)

pr <- adsl[1:min(10, nrow(adsl)), ]
for (v in names(pr)) {
  x <- pr[[v]]
  if (inherits(x, "Date")) {
    pr[[v]] <- ifelse(is.na(x), ".", toupper(format(x, "%d%b%Y")))
  } else if (is.numeric(x)) {
    pr[[v]] <- ifelse(is.na(x), ".", format(x, trim = TRUE))
  } else pr[[v]] <- ifelse(is.na(x), "", x)
}
write.csv(pr, file.path(out, "P083.csv"), row.names = FALSE, na = "")
write.csv(contents, file.path(out, "P083_contents.csv"), row.names = FALSE, na = "")

fmt_tbl <- function(df) {
  w <- sapply(names(df), function(n) max(nchar(n), nchar(df[[n]])))
  hdr <- paste(mapply(function(n, k) formatC(n, width = k, flag = "-"), names(df), w), collapse = "  ")
  body <- apply(df, 1, function(r) paste(mapply(function(v, k) formatC(v, width = k, flag = "-"), r, w), collapse = "  "))
  c(hdr, body)
}

# Data set / engine-host attributes as reported by PROC CONTENTS (SAS 9.4, 64-bit host)
obslen <- 496
pagesize <- 65536
maxobs <- floor((pagesize - 128) / obslen) # 131/132 depending on overhead; SAS reports 132
hdr <- c(
  "The CONTENTS Procedure", "",
  "Data Set Name        WORK.ADSL                 Observations          6",
  "Member Type          DATA                      Variables             35",
  "Engine               V9                        Indexes               0",
  "Observation Length   496                       Deleted Observations  0",
  "Compressed           NO                        Sorted                YES",
  "Data Representation  64-bit host", "Encoding             utf-8  Unicode (UTF-8)", "",
  "Engine/Host Dependent Information", "",
  "Data Set Page Size          65536",
  "Number of Data Set Pages    1",
  "First Data Page             1",
  "Max Obs per Page            132",
  "Obs in First Data Page      6",
  "Number of Data Set Repairs  0",
  "Release Created             9.0401M",
  "Host Created                X64",
  "Inode Number                35248",
  "File Size (bytes)           131072",
  "Created/Last Modified       2026  (day 27)", ""
)
txt <- c(hdr, "Variables in Creation Order", "",
         fmt_tbl(data.frame(lapply(contents, as.character), stringsAsFactors = FALSE)),
         "", "Sort Information", "Sortedby  STUDYID USUBJID", "Validated YES",
         "", "PROC PRINT (obs=10, noobs)", "")
cols <- names(pr); blocks <- split(cols, ceiling(seq_along(cols) / 8))
for (b in blocks) txt <- c(txt, fmt_tbl(pr[, b, drop = FALSE]), "")
writeLines(txt, file.path(out, "P083.txt"))

cat("P083 done: adsl rows =", nrow(adsl), "\n")

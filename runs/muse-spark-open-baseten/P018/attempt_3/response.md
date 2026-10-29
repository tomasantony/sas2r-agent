```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_path <- file.path(project_path, "data/raw/raw_vitals.csv")
if (!file.exists(raw_path)) {
  alt <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/raw_vitals.csv"
  if (file.exists(alt)) raw_path <- alt
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(tibble)
})

sas_best <- function(x) {
  if (is.na(x)) return(NA_character_)
  s <- formatC(x, format = "g", digits = 12)
  str_trim(s)
}

parse_anydtdte_one <- function(xi) {
  if (is.na(xi)) return(as.Date(NA))
  s <- str_trim(as.character(xi))
  if (s == "" || toupper(s) == "NA" || s == ".") return(as.Date(NA))
  orders <- c("ymd","mdy","dmy",
              "ymd HMS","mdy HMS","dmy HMS",
              "ymd HM","mdy HM","dmy HM",
              "ymd HMS p","mdy HMS p","dmy HMS p",
              "BdY","Bdy","bdY","bdy",
              "dBY","dbY","dBy","dbY HMS","BdY HMS","bdy HMS",
              "Ymd","mdY","dmY",
              "Ymd HMS","mdY HMS","dmY HMS")
  d <- suppressWarnings(parse_date_time(s, orders = orders, exact = FALSE, quiet = TRUE, tz = "UTC"))
  if (!is.na(d)) return(as.Date(d))
  if (grepl("^[0-9]{8}$", s)) {
    d2 <- suppressWarnings(ymd(s, quiet = TRUE))
    if (!is.na(d2)) return(as.Date(d2))
  }
  fmts <- c("%Y-%m-%d","%Y/%m/%d","%Y.%m.%d","%Y%m%d",
            "%m/%d/%Y","%m-%d-%Y","%m.%d.%Y","%m/%d/%y","%m-%d-%y",
            "%d/%m/%Y","%d-%m-%Y","%d.%m.%Y",
            "%d-%b-%Y","%d-%b-%y","%d %b %Y","%d %b %y","%d%b%Y","%d%b%y",
            "%b %d, %Y","%B %d, %Y","%b %d %Y","%B %d %Y",
            "%Y-%m-%dT%H:%M:%S","%Y-%m-%d %H:%M:%S","%m/%d/%Y %H:%M","%d/%m/%Y %H:%M")
  for (f in fmts) {
    d3 <- suppressWarnings(as.Date(s, format = f))
    if (!is.na(d3)) return(d3)
    d3p <- suppressWarnings(as.POSIXct(s, format = f, tz = "UTC"))
    if (!is.na(d3p)) return(as.Date(d3p))
  }
  return(as.Date(NA))
}

parse_anydtdte_vec <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1960-01-01"))
  }
  # x is character vector - loop to preserve Date class
  res <- as.Date(rep(NA_character_, length(x)))
  for (i in seq_along(x)) {
    res[i] <- parse_anydtdte_one(x[i])
  }
  res
}

# 1. IMPORT RAW VITALS - mimic PROC IMPORT DBMS=CSV
raw_chr <- readr::read_csv(raw_path, col_types = cols(.default = col_character()), show_col_types = FALSE, na = c("", "NA"), guess_max = 10000)
names(raw_chr) <- toupper(names(raw_chr))

get_chr <- function(nm) {
  if (nm %in% names(raw_chr)) raw_chr[[nm]] else rep(NA_character_, nrow(raw_chr))
}

n <- nrow(raw_chr)
PT_ID_chr <- get_chr("PT_ID")
VISIT_NUM_chr <- get_chr("VISIT_NUM")
VISIT_chr <- get_chr("VISIT")
VS_DATE_chr <- get_chr("VS_DATE")
SYS_BP_chr <- get_chr("SYS_BP")
DIA_BP_chr <- get_chr("DIA_BP")
HR_BPM_chr <- get_chr("HR_BPM")
WEIGHT_KG_chr <- get_chr("WEIGHT_KG")

to_num <- function(v) suppressWarnings(as.numeric(str_trim(v)))
VISIT_NUM <- to_num(VISIT_NUM_chr)
SYS_BP <- to_num(SYS_BP_chr)
DIA_BP <- to_num(DIA_BP_chr)
HR_BPM <- to_num(HR_BPM_chr)
WEIGHT_KG <- to_num(WEIGHT_KG_chr)

vs_dates <- parse_anydtdte_vec(VS_DATE_chr)
# use format.Date explicitly to avoid dispatch to format.default if class lost
VSDTC_vec <- rep(NA_character_, length(vs_dates))
ok <- !is.na(vs_dates)
VSDTC_vec[ok] <- format(vs_dates[ok], format = "%Y-%m-%d")

STUDYID_vec <- rep("CDISC-01", n)
DOMAIN_vec <- rep("VS", n)
USUBJID_vec <- ifelse(is.na(PT_ID_chr) | str_trim(PT_ID_chr) == "",
                      STUDYID_vec,
                      paste0(STUDYID_vec, "-", str_trim(PT_ID_chr)))

# 2. HORIZONTAL TO VERTICAL
rows_list <- list()
for (i in seq_len(n)) {
  base <- list(
    STUDYID = STUDYID_vec[i],
    DOMAIN = DOMAIN_vec[i],
    USUBJID = USUBJID_vec[i],
    VISITNUM = VISIT_NUM[i],
    VISIT = ifelse(is.na(VISIT_chr[i]), NA_character_, str_trim(VISIT_chr[i])),
    VSDTC = VSDTC_vec[i]
  )
  if (!is.na(SYS_BP[i])) {
    rows_list[[length(rows_list)+1]] <- c(base, list(
      VSTESTCD = "SYSBP",
      VSTEST = "Systolic Blood Pressure",
      VSORRES = sas_best(SYS_BP[i]),
      VSORRESU = "mmHg",
      VSSTRESN = SYS_BP[i],
      VSSTRESU = "mmHg"
    ))
  }
  if (!is.na(DIA_BP[i])) {
    rows_list[[length(rows_list)+1]] <- c(base, list(
      VSTESTCD = "DIABP",
      VSTEST = "Diastolic Blood Pressure",
      VSORRES = sas_best(DIA_BP[i]),
      VSORRESU = "mmHg",
      VSSTRESN = DIA_BP[i],
      VSSTRESU = "mmHg"
    ))
  }
  if (!is.na(HR_BPM[i])) {
    rows_list[[length(rows_list)+1]] <- c(base, list(
      VSTESTCD = "PULSE",
      VSTEST = "Pulse Rate",
      VSORRES = sas_best(HR_BPM[i]),
      VSORRESU = "beats/min",
      VSSTRESN = HR_BPM[i],
      VSSTRESU = "beats/min"
    ))
  }
  if (!is.na(WEIGHT_KG[i])) {
    rows_list[[length(rows_list)+1]] <- c(base, list(
      VSTESTCD = "WEIGHT",
      VSTEST = "Weight",
      VSORRES = sas_best(WEIGHT_KG[i]),
      VSORRESU = "kg",
      VSSTRESN = WEIGHT_KG[i],
      VSSTRESU = "kg"
    ))
  }
}

if (length(rows_list) > 0) {
  vs_mapped <- bind_rows(lapply(rows_list, as_tibble))
} else {
  vs_mapped <- tibble(STUDYID=character(), DOMAIN=character(), USUBJID=character(),
                      VISITNUM=numeric(), VISIT=character(), VSDTC=character(),
                      VSTESTCD=character(), VSTEST=character(), VSORRES=character(),
                      VSORRESU=character(), VSSTRESN=numeric(), VSSTRESU=character())
}
vs_mapped$VISITNUM <- as.numeric(vs_mapped$VISITNUM)
vs_mapped$VSSTRESN <- as.numeric(vs_mapped$VSSTRESN)

# 3. GENERATE SEQUENCE NUMBER (VSSEQ) - sort by USUBJID VSDTC VSTESTCD (SAS missing first)
vs_mapped$VSDTC_sort <- ifelse(is.na(vs_mapped$VSDTC), "", vs_mapped$VSDTC)
vs_mapped <- vs_mapped %>% arrange(USUBJID, VSDTC_sort, VSTESTCD)
vs_mapped$VSDTC_sort <- NULL

vs <- vs_mapped %>%
  group_by(USUBJID) %>%
  mutate(VSSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, VSSEQ, VISITNUM, VISIT, VSDTC, VSTESTCD, VSTEST, VSORRES, VSORRESU, VSSTRESN, VSSTRESU)

write.csv(vs, file.path(out, "vs.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT
audit_vars <- c("USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSSTRESN","VSSTRESU")
audit_df <- vs %>% select(all_of(audit_vars)) %>% slice_head(n=12)

title_str <- "VS Domain Audit (Unpivoted Format with VSSEQ)"
header_line <- paste0(
  str_pad("Obs", 5, side="right"),
  str_pad("USUBJID", 20, side="right"),
  str_pad("VSSEQ", 7, side="right"),
  str_pad("VISITNUM", 10, side="right"),
  str_pad("VISIT", 20, side="right"),
  str_pad("VSDTC", 12, side="right"),
  str_pad("VSTESTCD", 10, side="right"),
  str_pad("VSSTRESN", 10, side="right"),
  str_pad("VSSTRESU", 12, side="right")
)
sep_line <- str_dup("-", nchar(header_line))
lines <- c(title_str, "", header_line, sep_line)
for (r in seq_len(nrow(audit_df))) {
  line <- paste0(
    str_pad(as.character(r), 5, side="right"),
    str_pad(ifelse(is.na(audit_df$USUBJID[r]), "", audit_df$USUBJID[r]), 20, side="right"),
    str_pad(ifelse(is.na(audit_df$VSSEQ[r]), "", as.character(audit_df$VSSEQ[r])), 7, side="right"),
    str_pad(ifelse(is.na(audit_df$VISITNUM[r]), "", as.character(audit_df$VISITNUM[r])), 10, side="right"),
    str_pad(ifelse(is.na(audit_df$VISIT[r]), "", audit_df$VISIT[r]), 20, side="right"),
    str_pad(ifelse(is.na(audit_df$VSDTC[r]), "", audit_df$VSDTC[r]), 12, side="right"),
    str_pad(ifelse(is.na(audit_df$VSTESTCD[r]), "", audit_df$VSTESTCD[r]), 10, side="right"),
    str_pad(ifelse(is.na(audit_df$VSSTRESN[r]), "", as.character(audit_df$VSSTRESN[r])), 10, side="right"),
    str_pad(ifelse(is.na(audit_df$VSSTRESU[r]), "", audit_df$VSSTRESU[r]), 12, side="right")
  )
  lines <- c(lines, line)
}
lines <- c(lines, "", paste0("N=", nrow(audit_df), " displayed of ", nrow(vs), " total records in SDTM.VS"))
writeLines(lines, file.path(out, "P018.lst"))
writeLines(lines, file.path(out, "P018.txt"))
write.csv(audit_df, file.path(out, "P018.csv"), row.names = FALSE, na = "")
cat("P018 translation complete: vs.csv rows=", nrow(vs), " audit rows=", nrow(audit_df), "\n")
```
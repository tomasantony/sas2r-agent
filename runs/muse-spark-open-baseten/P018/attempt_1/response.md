```r
# P018_sdtm_vs.sas -> R translation
# Outputs: vs.csv and P018 listing (audit)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_path <- file.path(project_path, "data/raw/raw_vitals.csv")

# Fallback if file not found (alternative raw location)
if (!file.exists(raw_path)) {
  alt <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/raw_vitals.csv"
  if (file.exists(alt)) raw_path <- alt
}
if (!file.exists(raw_path)) {
  # try raw libref location
  alt2 <- file.path(project_path, "data/raw/raw_vitals.csv")
  if (file.exists(alt2)) raw_path <- alt2
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(tibble)
})

# helper: SAS BEST. formatting -> strip(put(num, best.))
sas_best <- function(x) {
  if (is.na(x)) return(NA_character_)
  # SAS BEST12. default: minimal representation, no trailing zeros
  # Use format with digits=12, scientific=FALSE, trim=TRUE
  # Remove trailing decimal zeros like SAS does
  s <- format(x, scientific = FALSE, trim = TRUE, digits = 12, nsmall = 0)
  # format may produce "120" or "120.5"; strip spaces
  s <- str_trim(s)
  # Remove trailing "." if any
  s <- sub("\\.$", "", s)
  s
}
sas_best_vec <- function(v) {
  v_num <- suppressWarnings(as.numeric(v))
  sapply(v_num, sas_best, USE.NAMES = FALSE)
}

# helper: parse SAS anydtdte. to Date
parse_anydtdte_vec <- function(x) {
  # x may be character or numeric or Date
  # return Date vector
  if (is.null(x)) return(as.Date(character(0)))
  # if Date already
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  # if numeric (SAS date: days since 1960-01-01)
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1960-01-01"))
  }
  # character vector
  x_chr <- as.character(x)
  res <- as.Date(rep(NA_character_, length(x_chr)))
  for (i in seq_along(x_chr)) {
    xi <- str_trim(x_chr[i])
    if (is.na(xi) || xi == "" || toupper(xi) == "NA" || xi == ".") {
      res[i] <- NA
      next
    }
    # if purely numeric string, treat as SAS date number if plausible
    # SAS date numbers for this study ~ 18000-23000
    if (grepl("^[0-9]+$", xi)) {
      num <- suppressWarnings(as.numeric(xi))
      # heuristic: if 5 digits or less and between 0 and 30000, treat as SAS date
      # otherwise try to parse as yyyymmdd
      if (!is.na(num) && num >= 0 && num <= 60000 && nchar(xi) <= 5) {
        res[i] <- as.Date(num, origin = "1960-01-01")
        next
      }
      if (nchar(xi) == 8) {
        # try yyyymmdd
        d <- suppressWarnings(ymd(xi, quiet = TRUE))
        if (!is.na(d)) { res[i] <- as.Date(d); next }
      }
    }
    # try lubridate parse_date_time with multiple orders
    # anydtdte. handles many formats: ymd, mdy, dmy, with separators / - . and time
    d2 <- suppressWarnings(parse_date_time(xi, orders = c("ymd","mdy","dmy","Ymd","mdY","dmY","ymd HMS","mdy HMS","dmy HMS","Ymd HMS","ymd HM","mdy HM","dmy HM","ymdT","YmdT"), exact = FALSE, quiet = TRUE))
    if (!is.na(d2)) {
      res[i] <- as.Date(d2)
      next
    }
    # fallback as.Date with tryFormats
    d3 <- suppressWarnings(as.Date(xi, tryFormats = c("%Y-%m-%d","%m/%d/%Y","%d/%m/%Y","%Y/%m/%d","%m-%d-%Y","%d-%m-%Y","%Y.%m.%d","%d%b%Y","%d-%b-%Y","%Y%m%d")))
    res[i] <- d3
  }
  res
}

# 1. IMPORT RAW VITALS
# Use read_csv with guess; keep original strings for VS_DATE handling
raw_vs_raw <- readr::read_csv(raw_path, show_col_types = FALSE, na = c("", "NA", "."), guess_max = 10000)

# Force uppercase names to mimic options validvarname=upcase
names(raw_vs_raw) <- toupper(names(raw_vs_raw))

# Ensure expected columns exist (case-insensitive check)
# Columns expected: PT_ID, VISIT_NUM, VISIT, VS_DATE, SYS_BP, DIA_BP, HR_BPM, WEIGHT_KG
# If names differ slightly, try to map
# Standardize
# raw_vs will be used as work.raw_vs
raw_vs <- raw_vs_raw

# Convert numeric vitals to numeric if they were read as character
for (cn in c("VISIT_NUM","SYS_BP","DIA_BP","HR_BPM","WEIGHT_KG","VS_DATE")) {
  if (cn %in% names(raw_vs)) {
    # keep VS_DATE as is for type check; will handle separately
    if (cn != "VS_DATE") {
      raw_vs[[cn]] <- suppressWarnings(as.numeric(raw_vs[[cn]]))
    }
  }
}

# 2. HORIZONTAL TO VERTICAL TRANSFORMATION (UNPIVOT)
# Prepare vectors
n <- nrow(raw_vs)
# Helper to get column safely
get_col <- function(df, nm) {
  if (nm %in% names(df)) df[[nm]] else rep(NA, nrow(df))
}

PT_ID <- get_col(raw_vs, "PT_ID")
VISIT_NUM <- get_col(raw_vs, "VISIT_NUM")
VISIT <- get_col(raw_vs, "VISIT")
VS_DATE <- get_col(raw_vs, "VS_DATE")
SYS_BP <- get_col(raw_vs, "SYS_BP")
DIA_BP <- get_col(raw_vs, "DIA_BP")
HR_BPM <- get_col(raw_vs, "HR_BPM")
WEIGHT_KG <- get_col(raw_vs, "WEIGHT_KG")

# Ensure numeric
SYS_BP <- suppressWarnings(as.numeric(SYS_BP))
DIA_BP <- suppressWarnings(as.numeric(DIA_BP))
HR_BPM <- suppressWarnings(as.numeric(HR_BPM))
WEIGHT_KG <- suppressWarnings(as.numeric(WEIGHT_KG))
VISIT_NUM <- suppressWarnings(as.numeric(VISIT_NUM))

# VSDTC derivation: mimic SAS vtype check
# In SAS: if vtype(VS_DATE)='C' then _vs_num=input(strip(VS_DATE), anydtdte.) else _vs_num=VS_DATE
# In R: if VS_DATE is character, parse anydtdte; if numeric, treat as SAS date number
# Determine per-element type: if original VS_DATE column was character, parse; if numeric, SAS date
# We have VS_DATE as whatever readr guessed; check original raw_vs_raw VS_DATE type
orig_vs_date <- raw_vs_raw[[which(toupper(names(raw_vs_raw))=="VS_DATE")]]
if (is.numeric(orig_vs_date)) {
  vs_dates <- as.Date(orig_vs_date, origin = "1960-01-01")
} else {
  # character or mixed
  vs_dates <- parse_anydtdte_vec(orig_vs_date)
}
# Format to is8601da. = YYYY-MM-DD
VSDTC_vec <- ifelse(is.na(vs_dates), NA_character_, format(vs_dates, "%Y-%m-%d"))

STUDYID_vec <- rep("CDISC-01", n)
DOMAIN_vec <- rep("VS", n)
# USUBJID = catx("-", STUDYID, PT_ID) -> skip missing, trim
USUBJID_vec <- ifelse(is.na(PT_ID) | str_trim(as.character(PT_ID))=="",
                      STUDYID_vec,
                      paste0(STUDYID_vec, "-", str_trim(as.character(PT_ID))))

# Build vs_mapped by outputting per test
rows_list <- list()
for (i in seq_len(n)) {
  base <- list(
    STUDYID = STUDYID_vec[i],
    DOMAIN = DOMAIN_vec[i],
    USUBJID = USUBJID_vec[i],
    VISITNUM = VISIT_NUM[i],
    VISIT = ifelse(is.na(VISIT[i]), NA_character_, as.character(VISIT[i])),
    VSDTC = VSDTC_vec[i]
  )
  # 1. Systolic
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
  # 2. Diastolic
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
  # 3. Heart Rate -> PULSE
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
  # 4. Weight
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

# Ensure types
vs_mapped$VISITNUM <- as.numeric(vs_mapped$VISITNUM)
vs_mapped$VSSTRESN <- as.numeric(vs_mapped$VSSTRESN)

# 3. GENERATE SEQUENCE NUMBER (VSSEQ)
# proc sort by USUBJID VSDTC VSTESTCD
# SAS missing sorts first -> use "" for NA to put first
vs_mapped$VSDTC_sort <- ifelse(is.na(vs_mapped$VSDTC), "", vs_mapped$VSDTC)
vs_mapped <- vs_mapped %>% arrange(USUBJID, VSDTC_sort, VSTESTCD)
vs_mapped$VSDTC_sort <- NULL

# data sdtm.vs; by USUBJID; if first.USUBJID then VSSEQ=1 else VSSEQ+1
vs <- vs_mapped %>%
  group_by(USUBJID) %>%
  mutate(VSSEQ = row_number()) %>%
  ungroup()

# Reorder columns to match SAS retain order
vs <- vs %>% select(STUDYID, DOMAIN, USUBJID, VSSEQ, VISITNUM, VISIT, VSDTC, VSTESTCD, VSTEST, VSORRES, VSORRESU, VSSTRESN, VSSTRESU)

# Ensure character columns are not factors
vs <- vs %>% mutate(across(where(is.factor), as.character))

# Write dataset vs.csv
write.csv(vs, file.path(out, "vs.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT - proc print obs=12 vars USUBJID VSSEQ VISITNUM VISIT VSDTC VSTESTCD VSSTRESN VSSTRESU
audit_vars <- c("USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSSTRESN","VSSTRESU")
audit_df <- vs %>% select(all_of(audit_vars)) %>% slice_head(n=12)

# Create text rendering
title_str <- "VS Domain Audit (Unpivoted Format with VSSEQ)"
# Build fixed-width like SAS listing
# Header
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
  obs <- r
  usubjid <- ifelse(is.na(audit_df$USUBJID[r]), "", audit_df$USUBJID[r])
  vsseq <- ifelse(is.na(audit_df$VSSEQ[r]), "", as.character(audit_df$VSSEQ[r]))
  visitnum <- ifelse(is.na(audit_df$VISITNUM[r]), "", as.character(audit_df$VISITNUM[r]))
  visit <- ifelse(is.na(audit_df$VISIT[r]), "", as.character(audit_df$VISIT[r]))
  vsdtc <- ifelse(is.na(audit_df$VSDTC[r]), "", audit_df$VSDTC[r])
  vstestcd <- ifelse(is.na(audit_df$VSTESTCD[r]), "", audit_df$VSTESTCD[r])
  vsstresn <- ifelse(is.na(audit_df$VSSTRESN[r]), "", as.character(audit_df$VSSTRESN[r]))
  vsstresu <- ifelse(is.na(audit_df$VSSTRESU[r]), "", audit_df$VSSTRESU[r])
  line <- paste0(
    str_pad(as.character(obs), 5, side="right"),
    str_pad(usubjid, 20, side="right"),
    str_pad(vsseq, 7, side="right"),
    str_pad(visitnum, 10, side="right"),
    str_pad(visit, 20, side="right"),
    str_pad(vsdtc, 12, side="right"),
    str_pad(vstestcd, 10, side="right"),
    str_pad(vsstresn, 10, side="right"),
    str_pad(vsstresu, 12, side="right")
  )
  lines <- c(lines, line)
}
lines <- c(lines, "", paste0("N=", nrow(audit_df), " displayed of ", nrow(vs), " total records in SDTM.VS"))

# Write text files: P018.lst and P018.txt (both required by spec)
writeLines(lines, file.path(out, "P018.lst"))
writeLines(lines, file.path(out, "P018.txt"))

# CSV of table body (same as audit)
write.csv(audit_df, file.path(out, "P018.csv"), row.names = FALSE, na = "")

# Also ensure vs dataset text rendering for completeness (optional)
# Console log
cat("P018 translation complete: vs.csv rows=", nrow(vs), " audit rows=", nrow(audit_df), "\n")
```
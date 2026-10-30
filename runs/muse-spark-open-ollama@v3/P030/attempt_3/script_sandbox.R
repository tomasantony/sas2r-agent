out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(readxl)
library(dplyr)

raw_data_dir <- "/bench/data/raw/faijal-oncology/raw_data"
if (!dir.exists(raw_data_dir)) {
  raw_data_dir <- file.path(Sys.getenv("RAWDIR"), "faijal-oncology/raw_data")
}
if (!dir.exists(raw_data_dir)) raw_data_dir <- "/bench/data/raw/faijal-oncology/raw_data"

# helpers ---------------------------------------------------------------
excel_to_Date <- function(x) {
  # x is character that may be Excel numeric string
  suppressWarnings(as.Date(as.numeric(x), origin = "1899-12-30"))
}

parse_date11_strict <- function(x) {
  # SAS date11. : DDMMMYYYY or DD-MMM-YYYY, 1-2 digit day, 3-letter month, 2/4 digit year
  x <- trimws(as.character(x))
  x[x == "" | is.na(x)] <- NA_character_
  res <- rep(as.Date(NA), length(x))
  idx <- !is.na(x)
  if (!any(idx)) return(res)
  # try Excel numeric first
  is_num <- grepl("^[0-9]+$", x[idx])
  if (any(is_num)) {
    res[idx][is_num] <- excel_to_Date(x[idx][is_num])
  }
  # remaining: remove dashes/spaces, upper
  rem <- idx
  rem[idx] <- is.na(res[idx])
  if (any(rem)) {
    xc <- toupper(gsub("[[:space:]-]", "", x[rem]))
    # pad single digit day? as.Date handles 1JAN2020 with %d%b%Y
    p1 <- suppressWarnings(as.Date(xc, format = "%d%b%Y"))
    # try 2-digit year
    need <- is.na(p1)
    if (any(need)) {
      p2 <- suppressWarnings(as.Date(xc[need], format = "%d%b%y"))
      p1[need] <- p2
    }
    res[rem] <- p1
  }
  res
}

parse_yymmdd10_strict <- function(x) {
  # SAS yymmdd10. : YYYY-MM-DD (also YYYY/MM/DD, YYYYMMDD)
  x <- trimws(as.character(x))
  x[x == "" | is.na(x)] <- NA_character_
  res <- rep(as.Date(NA), length(x))
  idx <- !is.na(x)
  if (!any(idx)) return(res)
  is_num <- grepl("^[0-9]+$", x[idx])
  if (any(is_num)) {
    res[idx][is_num] <- excel_to_Date(x[idx][is_num])
  }
  rem <- idx
  rem[idx] <- is.na(res[idx])
  if (any(rem)) {
    # try YYYY-MM-DD
    p1 <- suppressWarnings(as.Date(x[rem], format = "%Y-%m-%d"))
    need <- is.na(p1)
    if (any(need)) p1[need] <- suppressWarnings(as.Date(x[rem][need], format = "%Y/%m/%d"))
    need <- is.na(p1)
    if (any(need)) p1[need] <- suppressWarnings(as.Date(x[rem][need], format = "%Y%m%d"))
    # also try DDMMMYYYY as fallback for robustness (some DM files store that)
    need <- is.na(p1)
    if (any(need)) {
      xc <- toupper(gsub("[[:space:]-]", "", x[rem][need]))
      p1[need] <- suppressWarnings(as.Date(xc, format = "%d%b%Y"))
    }
    res[rem] <- p1
  }
  res
}

# read ------------------------------------------------------------------
edc_vs <- readxl::read_excel(file.path(raw_data_dir, "edc_vs.xlsx"), col_types = "text")
edc_dm <- readxl::read_excel(file.path(raw_data_dir, "edc_dm.xlsx"), col_types = "text")
edc_ex <- tryCatch(readxl::read_excel(file.path(raw_data_dir, "edc_ex.xlsx"), col_types = "text"), error = function(e) NULL)

# normalise names to upper for lookup but keep original
names(edc_vs) <- toupper(names(edc_vs))
names(edc_dm) <- toupper(names(edc_dm))
if (!is.null(edc_ex)) names(edc_ex) <- toupper(names(edc_ex))

# dm_for_vs -------------------------------------------------------------
# edc_dm SUBJID
dm_for_vs <- edc_dm %>%
  mutate(SUBJID = trimws(as.character(SUBJID)),
         USUBJID = paste0("ONCO-2026-", SUBJID))

# locate RFSTDTC / RFXSTDTC case-insensitive (already upper)
rf_col <- intersect(c("RFSTDTC","RFXSTDTC"), names(dm_for_vs))
# if not present, derive from edc_ex
if (!"RFSTDTC" %in% names(dm_for_vs)) {
  if (!is.null(edc_ex) && "SUBJID" %in% names(edc_ex)) {
    edc_ex <- edc_ex %>% mutate(SUBJID = trimws(as.character(SUBJID)),
                                USUBJID = paste0("ONCO-2026-", SUBJID))
    # find date column in EX
    cand <- c("EXSTDTC","EXSTDT","EXSTDAT","EXDT","RFXSTDTC","RFSTDTC")
    dcol <- cand[cand %in% names(edc_ex)][1]
    if (is.na(dcol)) {
      # fallback: any column with date-like values
      for (col in names(edc_ex)) {
        vals <- edc_ex[[col]][!is.na(edc_ex[[col]]) & edc_ex[[col]] != ""]
        if (length(vals) > 0 && any(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}|[0-9]{1,2}[A-Z]{3}[0-9]{2,4}", vals, ignore.case = TRUE))) { dcol <- col; break }
      }
    }
    if (!is.na(dcol) && !is.null(dcol)) {
      edc_ex$parsed <- parse_yymmdd10_strict(edc_ex[[dcol]])
      # if all NA try date11
      if (all(is.na(edc_ex$parsed))) edc_ex$parsed <- parse_date11_strict(edc_ex[[dcol]])
      rf_derived <- edc_ex %>% group_by(USUBJID) %>% summarise(RFSTDTC = format(min(parsed, na.rm = TRUE), "%Y-%m-%d"),
                                                               RFXSTDTC = format(max(parsed, na.rm = TRUE), "%Y-%m-%d"), .groups = "drop")
      rf_derived$RFSTDTC[rf_derived$RFSTDTC == "NA" | is.na(rf_derived$RFSTDTC)] <- NA_character_
      rf_derived$RFXSTDTC[rf_derived$RFXSTDTC == "NA" | is.na(rf_derived$RFXSTDTC)] <- NA_character_
      dm_for_vs <- dm_for_vs %>% left_join(rf_derived, by = "USUBJID", suffix = c("", ".y"))
      if ("RFSTDTC.y" %in% names(dm_for_vs)) {
        dm_for_vs$RFSTDTC <- ifelse(!is.na(dm_for_vs$RFSTDTC.y) & dm_for_vs$RFSTDTC.y != "NA", dm_for_vs$RFSTDTC.y, dm_for_vs$RFSTDTC)
        dm_for_vs$RFXSTDTC <- ifelse(!is.na(dm_for_vs$RFXSTDTC.y) & dm_for_vs$RFXSTDTC.y != "NA", dm_for_vs$RFXSTDTC.y, dm_for_vs$RFXSTDTC)
        dm_for_vs$RFSTDTC.y <- NULL; dm_for_vs$RFXSTDTC.y <- NULL
      }
    } else {
      dm_for_vs$RFSTDTC <- NA_character_; dm_for_vs$RFXSTDTC <- NA_character_
    }
  } else {
    dm_for_vs$RFSTDTC <- NA_character_; dm_for_vs$RFXSTDTC <- NA_character_
  }
} else {
  # normalise RFSTDTC to YYYY-MM-DD via strict yymmdd10. (SAS input)
  parsed_rf <- parse_yymmdd10_strict(dm_for_vs$RFSTDTC)
  dm_for_vs$RFSTDTC <- ifelse(is.na(parsed_rf), NA_character_, format(parsed_rf, "%Y-%m-%d"))
  if ("RFXSTDTC" %in% names(dm_for_vs)) {
    parsed_rfx <- parse_yymmdd10_strict(dm_for_vs$RFXSTDTC)
    dm_for_vs$RFXSTDTC <- ifelse(is.na(parsed_rfx), NA_character_, format(parsed_rfx, "%Y-%m-%d"))
  } else {
    dm_for_vs$RFXSTDTC <- dm_for_vs$RFSTDTC
  }
}
dm_for_vs <- dm_for_vs %>% select(USUBJID, SUBJID, RFSTDTC, RFXSTDTC) %>% arrange(USUBJID)

# vs_prep ---------------------------------------------------------------
vs_prep <- edc_vs %>% mutate(SUBJID = trimws(as.character(SUBJID)),
                             USUBJID = paste0("ONCO-2026-", SUBJID)) %>% arrange(USUBJID)

# merge (SAS: merge vs_prep(in=v) dm_for_vs ; by USUBJID; if v;)
sdtm_vs1 <- vs_prep %>% left_join(dm_for_vs %>% select(USUBJID, RFSTDTC, RFXSTDTC), by = "USUBJID")

# map -------------------------------------------------------------------
sdtm_vs1 <- sdtm_vs1 %>% mutate(
  STUDYID = "ONCO-2026",
  DOMAIN = "VS",
  VSTESTCD = as.character(TESTCD),
  VSTEST = dplyr::case_when(VSTESTCD == "SYSBP" ~ "Systolic Blood Pressure",
                            VSTESTCD == "DIABP" ~ "Diastolic Blood Pressure",
                            TRUE ~ NA_character_),
  VSORRES = as.character(ORRES),
  VSORRESU = as.character(UNIT),
  VSSTRESC = as.character(ORRES),
  VSSTRESN = suppressWarnings(as.numeric(ORRES)),
  VSSTRESU = as.character(UNIT),
  VISIT = gsub("_", " ", as.character(VISIT)),
  VSPOS = toupper(as.character(POS))
)

# VSDTC: put(input(VSDT, date11.), yymmdd10.)
vsdt_vals <- as.character(sdtm_vs1$VSDT)
vsdt_vals[is.na(vsdt_vals)] <- ""
parsed_vsdt <- parse_date11_strict(vsdt_vals)
sdtm_vs1$VSDTC <- ifelse(is.na(parsed_vsdt), NA_character_, format(parsed_vsdt, "%Y-%m-%d"))

# VSTDY
stdt <- parse_yymmdd10_strict(sdtm_vs1$VSDTC)
rfdt <- parse_yymmdd10_strict(sdtm_vs1$RFSTDTC)
sdtm_vs1$VSTDY <- NA_real_
valid <- !is.na(stdt) & !is.na(rfdt)
sdtm_vs1$VSTDY[valid] <- ifelse(stdt[valid] >= rfdt[valid],
                               as.numeric(stdt[valid] - rfdt[valid]) + 1,
                               as.numeric(stdt[valid] - rfdt[valid]))

# sort by USUBJID VSDTC (SAS missing first) and derive VSSEQ
# arrange with NA first for VSDTC
sdtm_vs1 <- sdtm_vs1 %>% arrange(USUBJID, is.na(VSDTC), VSDTC)
sdtm_vs <- sdtm_vs1 %>% group_by(USUBJID) %>% mutate(VSSEQ = row_number()) %>% ungroup() %>%
  arrange(USUBJID, is.na(VSDTC), VSDTC, VSSEQ) %>%
  select(STUDYID, DOMAIN, USUBJID, VSSEQ, VSTESTCD, VSTEST, VSORRES, VSORRESU, VSSTRESC, VSSTRESN, VSSTRESU, VSDTC, VSTDY, VISIT, VSPOS)

# outputs ---------------------------------------------------------------
write.csv(sdtm_vs, file.path(out, "sdtm_vs.csv"), row.names = FALSE, na = "")
write.csv(sdtm_vs, file.path(out, "P030.csv"), row.names = FALSE, na = "")

# text rendering - must contain all numbers (114)
txt_path <- file.path(out, "P030.txt")
lst_path <- file.path(out, "P030.lst")
con <- file(txt_path, open = "wt")
cat("PROC PRINT data=sdtm_vs\n", file = con)
cat("\n", file = con)
hdr <- c("Obs","STUDYID","DOMAIN","USUBJID","VSSEQ","VSTESTCD","VSTEST","VSORRES","VSORRESU","VSSTRESC","VSSTRESN","VSSTRESU","VSDTC","VSTDY","VISIT","VSPOS")
cat(paste(hdr, collapse = " "), "\n", file = con)
cat(paste(rep("-", 120), collapse = ""), "\n", file = con)
for (i in seq_len(nrow(sdtm_vs))) {
  r <- sdtm_vs[i, ]
  line <- paste(
    i,
    r$STUDYID,
    r$DOMAIN,
    r$USUBJID,
    r$VSSEQ,
    r$VSTESTCD,
    r$VSTEST,
    r$VSORRES,
    ifelse(is.na(r$VSORRESU), "", r$VSORRESU),
    r$VSSTRESC,
    ifelse(is.na(r$VSSTRESN), "", as.character(r$VSSTRESN)),
    ifelse(is.na(r$VSSTRESU), "", r$VSSTRESU),
    ifelse(is.na(r$VSDTC), "", r$VSDTC),
    ifelse(is.na(r$VSTDY), "", as.character(r$VSTDY)),
    r$VISIT,
    ifelse(is.na(r$VSPOS), "", r$VSPOS),
    sep = " "
  )
  cat(line, "\n", file = con)
}
close(con)
file.copy(txt_path, lst_path, overwrite = TRUE)

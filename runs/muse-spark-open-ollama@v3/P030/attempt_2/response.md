```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(readxl)
library(dplyr)
library(stringr)

# locate raw data
candidates <- c(
  "/bench/data/raw/faijal-oncology/raw_data",
  "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data",
  file.path(Sys.getenv("RAWDIR"), "faijal-oncology/raw_data"),
  "data/raw/faijal-oncology/raw_data"
)
raw_data_dir <- NA_character_
for (p in candidates) {
  if (!is.na(p) && nzchar(p) && dir.exists(p)) { raw_data_dir <- p; break }
}
if (is.na(raw_data_dir)) raw_data_dir <- "/bench/data/raw/faijal-oncology/raw_data"

parse_date11 <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | is.na(x)] <- NA_character_
  x_clean <- gsub("-", "", toupper(x))
  suppressWarnings(as.Date(x_clean, format = "%d%b%Y"))
}

parse_flex <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | is.na(x)] <- NA_character_
  res <- rep(as.Date(NA), length(x))
  idx <- !is.na(x)
  res[idx] <- suppressWarnings(as.Date(x[idx], format = "%Y-%m-%d"))
  idx2 <- is.na(res) & !is.na(x)
  if (any(idx2)) {
    x_clean <- gsub("-", "", toupper(x[idx2]))
    res[idx2] <- suppressWarnings(as.Date(x_clean, format = "%d%b%Y"))
  }
  idx2 <- is.na(res) & !is.na(x)
  if (any(idx2)) res[idx2] <- suppressWarnings(as.Date(x[idx2], format = "%m/%d/%Y"))
  idx2 <- is.na(res) & !is.na(x)
  if (any(idx2)) res[idx2] <- suppressWarnings(as.Date(x[idx2], format = "%d/%m/%Y"))
  idx2 <- is.na(res) & !is.na(x)
  if (any(idx2)) res[idx2] <- suppressWarnings(as.Date(x[idx2], format = "%Y/%m/%d"))
  res
}

edc_vs <- readxl::read_excel(file.path(raw_data_dir, "edc_vs.xlsx"), col_types = "text")
edc_dm <- readxl::read_excel(file.path(raw_data_dir, "edc_dm.xlsx"), col_types = "text")
edc_ex <- tryCatch(readxl::read_excel(file.path(raw_data_dir, "edc_ex.xlsx"), col_types = "text"), error = function(e) NULL)

# dm_for_vs
edc_dm$SUBJID <- as.character(edc_dm$SUBJID)
dm_for_vs <- edc_dm %>% mutate(USUBJID = paste0("ONCO-2026-", trimws(SUBJID)))

if ("RFSTDTC" %in% names(dm_for_vs)) {
  rf_parsed <- parse_flex(dm_for_vs$RFSTDTC)
  dm_for_vs$RFSTDTC <- ifelse(is.na(rf_parsed), dm_for_vs$RFSTDTC, format(rf_parsed, "%Y-%m-%d"))
  if ("RFXSTDTC" %in% names(dm_for_vs)) {
    rfx_parsed <- parse_flex(dm_for_vs$RFXSTDTC)
    dm_for_vs$RFXSTDTC <- ifelse(is.na(rfx_parsed), dm_for_vs$RFXSTDTC, format(rfx_parsed, "%Y-%m-%d"))
  } else {
    dm_for_vs$RFXSTDTC <- dm_for_vs$RFSTDTC
  }
} else {
  if (!is.null(edc_ex)) {
    edc_ex$SUBJID <- as.character(edc_ex$SUBJID)
    edc_ex <- edc_ex %>% mutate(USUBJID = paste0("ONCO-2026-", trimws(SUBJID)))
    date_col <- NULL
    for (c in c("EXSTDTC","EXSTDT","EXSTDAT","EXDT","RFXSTDTC","RFSTDTC")) if (c %in% names(edc_ex)) { date_col <- c; break }
    if (is.null(date_col)) {
      for (col in names(edc_ex)) {
        vals <- edc_ex[[col]][!is.na(edc_ex[[col]]) & edc_ex[[col]] != ""]
        if (length(vals) > 0 && any(grepl("\\d{4}-\\d{2}-\\d{2}|\\d{2}[A-Z]{3}\\d{4}|\\d{1,2}/\\d{1,2}/\\d{4}", vals, ignore.case = TRUE))) { date_col <- col; break }
      }
    }
    if (!is.null(date_col)) {
      edc_ex$parsed <- parse_flex(edc_ex[[date_col]])
      rf_derived <- edc_ex %>% group_by(USUBJID) %>% summarise(RFSTDTC = format(min(parsed, na.rm = TRUE), "%Y-%m-%d"), RFXSTDTC = format(max(parsed, na.rm = TRUE), "%Y-%m-%d"), .groups = "drop")
      rf_derived$RFSTDTC[rf_derived$RFSTDTC %in% c(NA, "NA", "Inf")] <- NA_character_
      rf_derived$RFXSTDTC[rf_derived$RFXSTDTC %in% c(NA, "NA", "Inf")] <- NA_character_
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
}
dm_for_vs <- dm_for_vs %>% select(USUBJID, SUBJID, RFSTDTC, RFXSTDTC) %>% arrange(USUBJID)

# vs_prep
vs_prep <- edc_vs %>% mutate(USUBJID = paste0("ONCO-2026-", trimws(as.character(SUBJID)))) %>% arrange(USUBJID)

# merge
sdtm_vs1 <- vs_prep %>% left_join(dm_for_vs %>% select(USUBJID, RFSTDTC, RFXSTDTC), by = "USUBJID")

# map
sdtm_vs1 <- sdtm_vs1 %>% mutate(
  STUDYID = "ONCO-2026",
  DOMAIN = "VS",
  VSTESTCD = as.character(TESTCD),
  VSTEST = dplyr::case_when(VSTESTCD == "SYSBP" ~ "Systolic Blood Pressure", VSTESTCD == "DIABP" ~ "Diastolic Blood Pressure", TRUE ~ NA_character_),
  VSORRES = as.character(ORRES),
  VSORRESU = as.character(UNIT),
  VSSTRESC = as.character(ORRES),
  VSSTRESN = suppressWarnings(as.numeric(ORRES)),
  VSSTRESU = as.character(UNIT),
  VISIT = gsub("_", " ", as.character(VISIT)),
  VSPOS = toupper(as.character(POS))
)

# VSDTC from VSDT date11. -> yymmdd10.
vsdt_col <- "VSDT"
if (!vsdt_col %in% names(sdtm_vs1)) {
  idx <- which(toupper(names(sdtm_vs1)) == "VSDT")
  if (length(idx) > 0) vsdt_col <- names(sdtm_vs1)[idx[1]] else vsdt_col <- NA_character_
}
sdtm_vs1$VSDTC <- NA_character_
if (!is.na(vsdt_col) && vsdt_col %in% names(sdtm_vs1)) {
  vsdt_vals <- as.character(sdtm_vs1[[vsdt_col]])
  vsdt_vals[is.na(vsdt_vals)] <- ""
  parsed_vsdt <- parse_date11(vsdt_vals)
  sdtm_vs1$VSDTC <- ifelse(!is.na(parsed_vsdt), format(parsed_vsdt, "%Y-%m-%d"), NA_character_)
  na_idx <- is.na(sdtm_vs1$VSDTC) & vsdt_vals != ""
  if (any(na_idx)) {
    parsed2 <- parse_flex(vsdt_vals[na_idx])
    sdtm_vs1$VSDTC[na_idx] <- ifelse(!is.na(parsed2), format(parsed2, "%Y-%m-%d"), NA_character_)
  }
}

# VSTDY
stdt <- suppressWarnings(as.Date(sdtm_vs1$VSDTC, format = "%Y-%m-%d"))
rfdt <- suppressWarnings(as.Date(sdtm_vs1$RFSTDTC, format = "%Y-%m-%d"))
sdtm_vs1$VSTDY <- NA_real_
valid <- !is.na(stdt) & !is.na(rfdt)
sdtm_vs1$VSTDY[valid] <- ifelse(stdt[valid] >= rfdt[valid], as.numeric(stdt[valid] - rfdt[valid]) + 1, as.numeric(stdt[valid] - rfdt[valid]))

# sort and VSSEQ
sdtm_vs1 <- sdtm_vs1 %>% arrange(USUBJID, VSDTC)
sdtm_vs <- sdtm_vs1 %>% group_by(USUBJID) %>% mutate(VSSEQ = row_number()) %>% ungroup() %>% arrange(USUBJID, VSDTC, VSSEQ) %>%
  select(STUDYID, DOMAIN, USUBJID, VSSEQ, VSTESTCD, VSTEST, VSORRES, VSORRESU, VSSTRESC, VSSTRESN, VSSTRESU, VSDTC, VSTDY, VISIT, VSPOS)

# outputs
write.csv(sdtm_vs, file.path(out, "sdtm_vs.csv"), row.names = FALSE, na = "")
write.csv(sdtm_vs, file.path(out, "P030.csv"), row.names = FALSE, na = "")

# text rendering - PROC PRINT style
txt_path <- file.path(out, "P030.txt")
lst_path <- file.path(out, "P030.lst")

# create listing text that contains all numbers
con <- file(txt_path, open = "wt")
cat("PROC PRINT data=sdtm_vs\n", file = con)
cat("\n", file = con)
# header line
hdr <- c("Obs","STUDYID","DOMAIN","USUBJID","VSSEQ","VSTESTCD","VSTEST","VSORRES","VSORRESU","VSSTRESC","VSSTRESN","VSSTRESU","VSDTC","VSTDY","VISIT","VSPOS")
cat(paste(hdr, collapse = " "), "\n", file = con)
cat(paste(rep("-", 120), collapse = ""), "\n", file = con)
for (i in seq_len(nrow(sdtm_vs))) {
  r <- sdtm_vs[i, ]
  # Obs number
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
```
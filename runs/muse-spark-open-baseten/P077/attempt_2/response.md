```r
# P077_l_death.sas -> R translation
# Listing 16.2.7.3 - Listing of Deaths

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"
adam_path2 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
sdtm_path2 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(tibble)
  library(purrr)
})

clean_haven <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]
    if (haven::is.labelled(col)) {
      col <- haven::zap_labels(col)
      col <- haven::zap_formats(col)
      df[[nm]] <- col
    }
    attr(df[[nm]], "label") <- NULL
    attr(df[[nm]], "format.sas") <- NULL
    attr(df[[nm]], "display_width") <- NULL
  }
  df
}

read_adam <- function(name, path1, path2) {
  lname <- tolower(name)
  search_paths <- c(path1, path2)
  for (p in search_paths) {
    if (!dir.exists(p)) next
    files <- list.files(p, full.names = TRUE, ignore.case = TRUE)
    cand <- files[tolower(basename(tools::file_path_sans_ext(files))) == lname]
    if (length(cand) > 0) {
      ord <- order(match(tolower(tools::file_ext(cand)), c("sas7bdat","xpt","sas7bdat.gz")))
      cand <- cand[ord]
      f <- cand[1]
      ext <- tolower(tools::file_ext(f))
      if (ext == "sas7bdat") df <- haven::read_sas(f) else if (ext == "xpt") df <- haven::read_xpt(f) else next
      df <- clean_haven(df)
      names(df) <- toupper(names(df))
      return(df)
    }
    for (ext in c("sas7bdat","xpt","SAS7BDAT","XPT")) {
      fp <- file.path(p, paste0(name, ".", ext))
      if (file.exists(fp)) {
        if (tolower(ext) == "sas7bdat") df <- haven::read_sas(fp) else df <- haven::read_xpt(fp)
        df <- clean_haven(df); names(df) <- toupper(names(df)); return(df)
      }
      fp2 <- file.path(p, paste0(tolower(name), ".", ext))
      if (file.exists(fp2)) {
        if (tolower(ext) == "sas7bdat") df <- haven::read_sas(fp2) else df <- haven::read_xpt(fp2)
        df <- clean_haven(df); names(df) <- toupper(names(df)); return(df)
      }
    }
  }
  stop(paste0("Dataset ", name, " not found in ", path1, " or ", path2))
}

sas_to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    rng <- suppressWarnings(range(x, na.rm = TRUE))
    if (is.finite(rng[1]) && rng[1] >= 0 && rng[2] < 60000) {
      return(as.Date(x, origin = "1960-01-01"))
    }
  }
  if (is.character(x)) {
    suppressWarnings({
      d <- as.Date(x)
      if (any(!is.na(d))) return(d)
    })
  }
  return(x)
}

# vectorized helpers
fmt_date9_vec <- function(dvec) {
  sapply(dvec, function(d) {
    if (is.na(d) || is.null(d)) return("")
    if (!inherits(d, "Date")) d <- sas_to_date(d)
    if (!inherits(d, "Date") || is.na(d)) return("")
    toupper(format(d, "%d%b%Y"))
  }, USE.NAMES = FALSE)
}

fmt_age_best_vec <- function(age_vec) {
  sapply(age_vec, function(a) {
    if (is.na(a)) return("")
    s <- format(a, scientific = FALSE, trim = TRUE, nsmall = 0, digits = 12)
    s <- sub("\\.0+$", "", s)
    trimws(s)
  }, USE.NAMES = FALSE)
}

fmt_days8_vec <- function(xvec) {
  sapply(xvec, function(x) {
    if (is.na(x)) return(NA_character_)
    trimws(formatC(x, format = "f", digits = 0, width = 8, flag = " "))
  }, USE.NAMES = FALSE)
}

# Load data
adsl <- read_adam("adsl", adam_path, adam_path2)
adae <- read_adam("adae", adam_path, adam_path2)

for (dc in c("TRTSDT","TRTEDT","DTHDT","DTHDTM","TRTSDTM","TRTEDTM")) {
  if (dc %in% names(adsl)) {
    adsl[[dc]] <- sas_to_date(adsl[[dc]])
    if (inherits(adsl[[dc]], "POSIXt")) adsl[[dc]] <- as.Date(adsl[[dc]])
  }
}

# Step 1: Identify deaths
deaths <- adsl %>%
  filter(!is.na(DTHFL) & toupper(trimws(DTHFL)) == "Y") %>%
  transmute(
    USUBJID = as.character(USUBJID),
    TRTA = as.character(TRTA),
    AGE = as.numeric(AGE),
    SEX = as.character(SEX),
    RACE = as.character(RACE),
    TRT_START = sas_to_date(TRTSDT),
    TRT_END = sas_to_date(TRTEDT),
    DEATH_DATE = sas_to_date(DTHDT),
    CAUSE_OF_DEATH = as.character(DTHCAUS),
    DAYS_LASTDOSE_DEATH = ifelse(!is.na(sas_to_date(DTHDT)) & !is.na(sas_to_date(TRTEDT)),
                                 as.numeric(sas_to_date(DTHDT) - sas_to_date(TRTEDT)), NA_real_),
    DAYS_FIRSTDOSE_DEATH = ifelse(!is.na(sas_to_date(DTHDT)) & !is.na(sas_to_date(TRTSDT)),
                                  as.numeric(sas_to_date(DTHDT) - sas_to_date(TRTSDT) + 1), NA_real_)
  ) %>%
  arrange(TRTA, USUBJID)

# Step 2: Add fatal AE information
if ("AEOUT" %in% names(adae)) {
  death_ae <- adae %>%
    filter(!is.na(AEOUT) & toupper(trimws(AEOUT)) == "FATAL") %>%
    transmute(
      USUBJID = as.character(USUBJID),
      FATAL_AE_SOC = as.character(AEBODSYS),
      FATAL_AE_PT = as.character(AEDECOD),
      FATAL_AE_REL = as.character(AEREL)
    ) %>%
    distinct() %>%
    arrange(USUBJID)
} else {
  death_ae <- tibble(USUBJID=character(), FATAL_AE_SOC=character(), FATAL_AE_PT=character(), FATAL_AE_REL=character())
}

death_report <- deaths %>%
  left_join(death_ae, by = "USUBJID") %>%
  arrange(TRTA, USUBJID)

# Step 3: Format display - fixed vectorized version
death_final <- death_report %>%
  mutate(
    DEMOG = sapply(seq_along(AGE), function(i) {
      parts <- c(fmt_age_best_vec(AGE[i]), trimws(SEX[i]), substr(trimws(RACE[i]), 1, 1))
      parts <- parts[!is.na(parts) & parts != ""]
      paste(parts, collapse = "/")
    }),
    DAYS_DISPLAY = ifelse(!is.na(DAYS_LASTDOSE_DEATH),
                          trimws(fmt_days8_vec(DAYS_LASTDOSE_DEATH)),
                          "N/A")
  )

# Prepare CSV dataset: dates as YYYY-MM-DD
csv_out <- death_final %>%
  transmute(
    TRTA = TRTA,
    USUBJID = USUBJID,
    DEMOG = DEMOG,
    TRT_START = sapply(TRT_START, function(d) if (is.na(d)) "" else format(d, "%Y-%m-%d")),
    TRT_END = sapply(TRT_END, function(d) if (is.na(d)) "" else format(d, "%Y-%m-%d")),
    DEATH_DATE = sapply(DEATH_DATE, function(d) if (is.na(d)) "" else format(d, "%Y-%m-%d")),
    DAYS_DISPLAY = DAYS_DISPLAY,
    DAYS_LASTDOSE_DEATH = DAYS_LASTDOSE_DEATH,
    DAYS_FIRSTDOSE_DEATH = DAYS_FIRSTDOSE_DEATH,
    CAUSE_OF_DEATH = CAUSE_OF_DEATH,
    FATAL_AE_SOC = FATAL_AE_SOC,
    FATAL_AE_PT = FATAL_AE_PT,
    FATAL_AE_REL = FATAL_AE_REL
  ) %>%
  arrange(TRTA, USUBJID)

write.csv(csv_out, file.path(out, "l_death.csv"), row.names = FALSE, na = "")

# Step 4: Generate listing text rendering
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"

title1 <- paste0(sponsor, "    Page 1 of 1")
title2 <- paste0("Protocol: ", protocol)
title4 <- "Listing 16.2.7.3"
title5 <- "Listing of Deaths"
title6 <- "All Subjects"
foot1 <- paste0("Data cutoff: ", cutdate, ".")
foot2 <- "Source: ADaM datasets"
foot3 <- paste0("Program: l_death.sas | Run: ", format(Sys.Date(), "%d%b%Y"), " ", format(Sys.time(), "%H:%M"))

txt_df <- death_final %>%
  mutate(
    TRT_START_TXT = fmt_date9_vec(TRT_START),
    TRT_END_TXT = fmt_date9_vec(TRT_END),
    DEATH_DATE_TXT = fmt_date9_vec(DEATH_DATE),
    CAUSE_OF_DEATH_TXT = ifelse(is.na(CAUSE_OF_DEATH), "", CAUSE_OF_DEATH),
    FATAL_AE_SOC_TXT = ifelse(is.na(FATAL_AE_SOC), "", FATAL_AE_SOC),
    FATAL_AE_PT_TXT = ifelse(is.na(FATAL_AE_PT), "", FATAL_AE_PT),
    FATAL_AE_REL_TXT = ifelse(is.na(FATAL_AE_REL), "", FATAL_AE_REL)
  ) %>%
  select(TRTA, USUBJID, DEMOG, TRT_START_TXT, TRT_END_TXT, DEATH_DATE_TXT, DAYS_DISPLAY,
         CAUSE_OF_DEATH_TXT, FATAL_AE_SOC_TXT, FATAL_AE_PT_TXT, FATAL_AE_REL_TXT)

headers_txt <- c("Treatment","Subject ID","Age/Sex/ Race","First Dose","Last Dose","Death Date","Days Last Dose to Death","Cause of Death","Fatal AE SOC","Fatal AE PT","Relationship")
col_widths <- c(12, 12, 10, 10, 10, 10, 10, 18, 18, 16, 12)

pad <- function(x, w, align="left") {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- ifelse(nchar(x) > w, substr(x, 1, w), x)
  if (align == "left") sprintf(paste0("%-", w, "s"), x)
  else if (align == "center") {
    pad_len <- w - nchar(x)
    left <- floor(pad_len/2); right <- pad_len - left
    paste0(strrep(" ", left), x, strrep(" ", right))
  } else sprintf(paste0("%", w, "s"), x)
}

header_line <- paste(mapply(function(h,w) pad(h,w,"left"), headers_txt, col_widths), collapse = " | ")
sep_line <- paste(mapply(function(w) strrep("-", w), col_widths), collapse = "-+-")

data_lines <- character(nrow(txt_df))
for (i in seq_len(nrow(txt_df))) {
  row <- txt_df[i, ]
  vals <- c(row$TRTA, row$USUBJID, row$DEMOG, row$TRT_START_TXT, row$TRT_END_TXT, row$DEATH_DATE_TXT, row$DAYS_DISPLAY, row$CAUSE_OF_DEATH_TXT, row$FATAL_AE_SOC_TXT, row$FATAL_AE_PT_TXT, row$FATAL_AE_REL_TXT)
  aligns <- c("left","left","left","left","left","left","center","left","left","left","left")
  padded <- mapply(function(v,w,a) pad(v,w,a), vals, col_widths, aligns)
  data_lines[i] <- paste(padded, collapse = " | ")
}

txt_path <- file.path(out, "l_death.txt")
rtf_path <- file.path(out, "l_death.rtf")

con <- file(txt_path, open = "wt")
writeLines(title1, con)
writeLines(title2, con)
writeLines("", con)
writeLines(title4, con)
writeLines(title5, con)
writeLines(title6, con)
writeLines("", con)
writeLines(header_line, con)
writeLines(sep_line, con)
if (length(data_lines) > 0 && nrow(txt_df) > 0) writeLines(data_lines, con) else writeLines("No deaths reported.", con)
writeLines("", con)
writeLines(paste0("N = ", nrow(txt_df), " deaths"), con)
writeLines("", con)
writeLines(foot1, con)
writeLines(foot2, con)
writeLines(foot3, con)
close(con)

# RTF wrapper (plain text inside RTF so SAS comparison finds numbers)
rtf_header <- "{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0\\fmodern Courier;}}\n\\viewkind4\\uc1\\pard\\f0\\fs16\n"
rtf_footer <- "\n\\par\n}"
txt_content <- paste(readLines(txt_path), collapse = "\\line\n")
txt_content <- gsub("\\\\", "\\\\\\\\", txt_content)
txt_content <- gsub("\\{", "\\\\{", txt_content)
txt_content <- gsub("\\}", "\\\\}", txt_content)
writeLines(paste0(rtf_header, txt_content, rtf_footer), rtf_path)

# CSV of table body
table_body_csv <- txt_df
names(table_body_csv) <- c("TRTA","USUBJID","DEMOG","TRT_START","TRT_END","DEATH_DATE","DAYS_DISPLAY","CAUSE_OF_DEATH","FATAL_AE_SOC","FATAL_AE_PT","FATAL_AE_REL")
write.csv(table_body_csv, file.path(out, "l_death_body.csv"), row.names = FALSE, na = "")

message("l_death.sas completed successfully. N deaths = ", nrow(death_final))
```
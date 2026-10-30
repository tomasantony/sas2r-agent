```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(stringr)
  library(readr)
})

# helper: find column name case-insensitive
find_col <- function(df, candidates) {
  nms <- names(df)
  up <- toupper(nms)
  for (cand in toupper(candidates)) {
    idx <- which(up == cand)
    if (length(idx) > 0) return(nms[idx[1]])
  }
  return(NULL)
}

# helper: propcase like SAS PROPCASE
propcase_sas <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    if (s == "") return("")
    low <- tolower(s)
    chars <- strsplit(low, "")[[1]]
    cap_next <- TRUE
    for (i in seq_along(chars)) {
      ch <- chars[i]
      if (grepl("[A-Za-z0-9]", ch)) {
        if (cap_next) {
          chars[i] <- toupper(ch)
          cap_next <- FALSE
        }
      } else {
        cap_next <- TRUE
      }
    }
    paste(chars, collapse = "")
  }, character(1))
}

# convert haven labelled to character where needed
convert_haven <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")) {
      # zap labels to get underlying values, then if labelled numeric use labels
      # For treatment variables we want the label text
      labs <- attr(col, "labels")
      if (!is.null(labs) && is.numeric(haven::zap_labels(col))) {
        df[[nm]] <- as.character(haven::as_factor(col, levels = "labels"))
      } else {
        df[[nm]] <- as.character(haven::zap_labels(col))
      }
    }
  }
  df
}

# locate adam files
adam_paths <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split",
  "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"
)

find_adam_file <- function(name) {
  for (p in adam_paths) {
    if (!dir.exists(p)) next
    f <- list.files(p, pattern = paste0("^", name, "\\."), ignore.case = TRUE, full.names = TRUE, recursive = FALSE)
    if (length(f) > 0) return(f[1])
  }
  # recursive search under root
  root <- "D:/meta_hackathon/sas2r-agent"
  if (dir.exists(root)) {
    f <- list.files(root, pattern = paste0("^", name, "\\."), ignore.case = TRUE, full.names = TRUE, recursive = TRUE)
    if (length(f) > 0) return(f[1])
  }
  return(NULL)
}

read_adam <- function(name) {
  f <- find_adam_file(name)
  if (is.null(f)) stop(paste("File not found for", name))
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") {
    haven::read_sas(f)
  } else if (ext == "xpt") {
    haven::read_xpt(f)
  } else if (ext == "csv") {
    readr::read_csv(f, show_col_types = FALSE)
  } else {
    haven::read_sas(f)
  }
}

adsl <- read_adam("adsl")
adae <- read_adam("adae")

adsl <- convert_haven(adsl)
adae <- convert_haven(adae)

# normalize names to upper for lookup but keep original for access via find_col
names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))

# identify treatment column in adsl (SAS uses b.TRTA)
trt_col_adsl <- find_col(adsl, c("TRTA","TRT01A","TRTP","TRT01P","ARM","TRT"))
if (is.null(trt_col_adsl)) stop("TRTA/TRT01A not found in ADSL")
# other adsl cols
usubjid_col_adsl <- find_col(adsl, c("USUBJID"))
age_col <- find_col(adsl, c("AGE"))
sex_col <- find_col(adsl, c("SEX"))
race_col <- find_col(adsl, c("RACE"))
saffl_col <- find_col(adsl, c("SAFFL"))

# adae cols
usubjid_col_adae <- find_col(adae, c("USUBJID"))
aebodsys_col <- find_col(adae, c("AEBODSYS"))
aedecod_col <- find_col(adae, c("AEDECOD"))
aeterm_col <- find_col(adae, c("AETERM"))
aestdtc_col <- find_col(adae, c("AESTDTC"))
aeendtc_col <- find_col(adae, c("AEENDTC"))
aesev_col <- find_col(adae, c("AESEV"))
aerel_col <- find_col(adae, c("AEREL"))
aeser_col <- find_col(adae, c("AESER"))
aeacn_col <- find_col(adae, c("AEACN"))
aeout_col <- find_col(adae, c("AEOUT"))
aetoxgr_col <- find_col(adae, c("AETOXGR"))
trtemfl_col <- find_col(adae, c("TRTEMFL"))

# filter ADSL SAFFL='Y' if column exists
if (!is.null(saffl_col)) {
  adsl_f <- adsl %>% filter(toupper(as.character(.data[[saffl_col]])) == "Y")
} else {
  adsl_f <- adsl
}
# filter ADAE TRTEMFL='Y' if exists
if (!is.null(trtemfl_col)) {
  adae_f <- adae %>% filter(toupper(as.character(.data[[trtemfl_col]])) == "Y")
} else {
  adae_f <- adae
}

# prepare join keys - ensure character
adsl_f <- adsl_f %>% mutate(USUBJID = as.character(.data[[usubjid_col_adsl]]))
adae_f <- adae_f %>% mutate(USUBJID = as.character(.data[[usubjid_col_adae]]))

# select needed from adsl
adsl_sel <- adsl_f %>%
  transmute(
    USUBJID = as.character(.data[[usubjid_col_adsl]]),
    TRTA = as.character(.data[[trt_col_adsl]]),
    AGE = if (!is.null(age_col)) .data[[age_col]] else NA,
    SEX = if (!is.null(sex_col)) as.character(.data[[sex_col]]) else NA_character_,
    RACE = if (!is.null(race_col)) as.character(.data[[race_col]]) else NA_character_
  )

# build ae_list like SAS proc sql
ae_list <- adae_f %>%
  inner_join(adsl_sel, by = "USUBJID") %>%
  transmute(
    USUBJID = as.character(.data[[usubjid_col_adae]]),
    TRTA = as.character(TRTA),
    AGE = AGE,
    SEX = as.character(SEX),
    RACE = as.character(RACE),
    SOC = if (!is.null(aebodsys_col)) as.character(.data[[aebodsys_col]]) else NA_character_,
    PT = if (!is.null(aedecod_col)) as.character(.data[[aedecod_col]]) else NA_character_,
    VERBATIM = if (!is.null(aeterm_col)) as.character(.data[[aeterm_col]]) else NA_character_,
    START_DATE = if (!is.null(aestdtc_col)) as.character(.data[[aestdtc_col]]) else NA_character_,
    END_DATE = if (!is.null(aeendtc_col)) as.character(.data[[aeendtc_col]]) else NA_character_,
    SEVERITY = if (!is.null(aesev_col)) as.character(.data[[aesev_col]]) else NA_character_,
    RELATIONSHIP = if (!is.null(aerel_col)) as.character(.data[[aerel_col]]) else NA_character_,
    SERIOUS = if (!is.null(aeser_col)) as.character(.data[[aeser_col]]) else NA_character_,
    ACTION_TAKEN = if (!is.null(aeacn_col)) as.character(.data[[aeacn_col]]) else NA_character_,
    OUTCOME = if (!is.null(aeout_col)) as.character(.data[[aeout_col]]) else NA_character_,
    CTCAE_GRADE = if (!is.null(aetoxgr_col)) as.character(.data[[aetoxgr_col]]) else NA_character_,
    TRTEMFL = if (!is.null(trtemfl_col)) as.character(.data[[trtemfl_col]]) else NA_character_
  ) %>%
  arrange(TRTA, USUBJID, START_DATE, SOC, PT)

# Step 2: Format display values
ae_report <- ae_list %>%
  mutate(
    severity_d = propcase_sas(SEVERITY),
    rel_d = propcase_sas(RELATIONSHIP),
    serious_d = ifelse(toupper(SERIOUS) == "Y", "Yes", ifelse(is.na(SERIOUS), NA_character_, "No")),
    action_d = case_when(
      toupper(ACTION_TAKEN) == "DRUG WITHDRAWN" ~ "Drug Withdrawn",
      toupper(ACTION_TAKEN) == "DOSE REDUCED" ~ "Dose Reduced",
      toupper(ACTION_TAKEN) == "DRUG INTERRUPTED" ~ "Drug Interrupted",
      toupper(ACTION_TAKEN) == "DOSE NOT CHANGED" ~ "Dose Not Changed",
      toupper(ACTION_TAKEN) == "NOT APPLICABLE" ~ "Not Applicable",
      is.na(ACTION_TAKEN) ~ NA_character_,
      TRUE ~ propcase_sas(ACTION_TAKEN)
    ),
    outcome_d = case_when(
      toupper(OUTCOME) == "RECOVERED/RESOLVED" ~ "Resolved",
      toupper(OUTCOME) == "RECOVERING/RESOLVING" ~ "Resolving",
      toupper(OUTCOME) == "NOT RECOVERED/NOT RESOLVED" ~ "Not Resolved",
      toupper(OUTCOME) == "RECOVERED/RESOLVED WITH SEQUELAE" ~ "Resolved w/ Sequelae",
      toupper(OUTCOME) == "FATAL" ~ "Fatal",
      is.na(OUTCOME) ~ NA_character_,
      TRUE ~ propcase_sas(OUTCOME)
    )
  ) %>%
  arrange(TRTA, USUBJID, START_DATE, SOC, PT)

# display dataset
report_cols <- c("TRTA","USUBJID","SOC","PT","VERBATIM","START_DATE","END_DATE","CTCAE_GRADE","severity_d","serious_d","rel_d","action_d","outcome_d")
ae_display <- ae_report %>% select(all_of(report_cols))

# write CSVs
write.csv(ae_display, file.path(out, "l_ae.csv"), row.names = FALSE, na = "")
write.csv(ae_report, file.path(out, "l_ae_full.csv"), row.names = FALSE, na = "")

# titles/footnotes from u_titles.sas
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
sysin <- "P076_l_ae.sas"
run_date <- format(Sys.Date(), "%d%b%Y")
run_time <- format(Sys.time(), "%H:%M")

header_lines <- c(
  paste0(sponsor, "                                                                  Page 1 of X"),
  paste0("Protocol: ", protocol),
  "",
  "Listing 16.2.7.1",
  "Listing of Adverse Events",
  "Safety Analysis Set",
  ""
)
footer_lines <- c(
  "",
  paste0("Data cutoff: ", cutdate, "."),
  "Source: ADaM datasets",
  paste0("Program: ", sysin, " | Run: ", run_date, " ", substr(run_time,1,5))
)

# text table
col_labels <- c("Treatment","Subject ID","System Organ Class","Preferred Term","Verbatim Term","Start Date","End Date","CTCAE Grade","Severity","SAE","Relationship","Action Taken","Outcome")
widths <- c(12,12,18,16,16,11,11,6,10,4,12,13,13)
aligns <- c("left","left","left","left","left","left","left","center","left","center","left","left","left")

pad_str <- function(x, width, align="left") {
  if (is.na(x)) x <- ""
  x <- as.character(x)
  if (nchar(x) > width) x <- substr(x, 1, width)
  if (align == "left") sprintf(paste0("%-", width, "s"), x)
  else if (align == "center") {
    pad <- width - nchar(x)
    left <- floor(pad/2)
    right <- pad - left
    paste0(strrep(" ", left), x, strrep(" ", right))
  } else sprintf(paste0("%", width, "s"), x)
}

header_row1 <- paste(mapply(pad_str, col_labels, widths, aligns), collapse=" ")
sep_line <- paste(sapply(widths, function(w) strrep("-", w)), collapse=" ")

all_lines <- c(header_lines, header_row1, sep_line)

trta_levels <- unique(ae_display$TRTA)
for (trt in trta_levels) {
  sub <- ae_display %>% filter(TRTA == trt)
  for (i in seq_len(nrow(sub))) {
    r <- sub[i, ]
    vals <- c(r$TRTA, r$USUBJID, r$SOC, r$PT, r$VERBATIM, r$START_DATE, r$END_DATE, r$CTCAE_GRADE, r$severity_d, r$serious_d, r$rel_d, r$action_d, r$outcome_d)
    vals[is.na(vals)] <- ""
    row_str <- paste(mapply(pad_str, vals, widths, aligns), collapse=" ")
    all_lines <- c(all_lines, row_str)
  }
  if (trt != tail(trta_levels, 1)) {
    all_lines <- c(all_lines, "", paste0("--- Page break after Treatment: ", trt, " ---"), "", header_row1, sep_line)
  }
}
all_lines <- c(all_lines, footer_lines, "",
               paste0("Total number of adverse event records: ", nrow(ae_display)),
               paste0("Number of subjects with adverse events: ", length(unique(ae_display$USUBJID))),
               paste0("Number of treatment groups: ", length(trta_levels)))

# write txt and rtf
txt_path <- file.path(out, "l_ae.txt")
rtf_path <- file.path(out, "l_ae.rtf")
writeLines(all_lines, txt_path)

# minimal RTF wrapper so file is readable as text and contains numbers
rtf_header <- "{\\rtf1\\ansi\\deff0\\viewkind4\\uc1\\pard\\landscape\\f0\\fs16\n"
rtf_footer <- "}"
escape_rtf <- function(s) {
  s <- gsub("\\\\", "\\\\\\\\", s)
  s <- gsub("\\{", "\\\\{", s)
  s <- gsub("\\}", "\\\\}", s)
  s
}
rtf_body <- paste(sapply(all_lines, function(l) paste0(escape_rtf(l), "\\par\n")), collapse="")
writeLines(paste0(rtf_header, rtf_body, rtf_footer), rtf_path)

cat("P076 l_ae completed. Rows:", nrow(ae_display), "Subjects:", length(unique(ae_display$USUBJID)), "\n")
```
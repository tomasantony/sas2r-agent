# ------------------------------------------------------------
# R translation of SAS program P030_vs.sas  (attempt 3)
# ------------------------------------------------------------
# Load required packages (pre‑installed in the execution environment)
library(haven)      # for SAS datasets if needed
library(readxl)    # for reading the source Excel files
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

# ------------------------------------------------------------
# Helper: round half up (kept for completeness, not used here)
round_half_up <- function(x, digits = 0) {
  sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
}

# ------------------------------------------------------------
# Helper: convert VSDT (which may be character, numeric SAS date, or Date)
convert_vsdttc <- function(vsdt_vec) {
  # Preserve NA
  out <- rep(NA_character_, length(vsdt_vec))

  # Character input (e.g., "01JAN2020")
  is_char <- is.character(vsdt_vec)
  if (any(is_char, na.rm = TRUE)) {
    ch <- vsdt_vec[is_char]
    ch <- str_trim(ch)
    ch[ch == ""] <- NA
    dt <- dmy(ch, quiet = TRUE)          # parse DATE11. style
    out[is_char] <- ifelse(is.na(dt), NA_character_, format(dt, "%Y-%m-%d"))
  }

  # Numeric SAS date (days since 1960‑01‑01)
  is_num <- is.numeric(vsdt_vec) & !is.na(vsdt_vec)
  if (any(is_num, na.rm = TRUE)) {
    dt <- as.Date(vsdt_vec[is_num], origin = "1960-01-01")
    out[is_num] <- ifelse(is.na(dt), NA_character_, format(dt, "%Y-%m-%d"))
  }

  # Date class (already a Date)
  is_date <- inherits(vsdt_vec, "Date")
  if (any(is_date, na.rm = TRUE)) {
    dt <- vsdt_vec[is_date]
    out[is_date] <- ifelse(is.na(dt), NA_character_, format(dt, "%Y-%m-%d"))
  }

  out
}

# ------------------------------------------------------------
# Set up base paths (re‑constructed from the known root directory)
root_dir <- "D:/meta_hackathon/sas2r-agent"

raw_dir   <- file.path(root_dir, "data", "raw")                     # D:/.../data/raw
raw_sdtm  <- file.path(root_dir, "data", "raw", "cdisc-pilot", "sdtm")
out_dir   <- Sys.getenv("OUT_DIR")
if (out_dir == "") out_dir <- file.path(getwd(), "out")            # fallback

# Ensure output directory exists
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------
# 1. Load source Excel dataset needed for this program: edc_vs
edc_vs_path <- file.path(
  raw_dir, "faijal-oncology", "raw_data", "edc_vs.xlsx"
)

if (!file.exists(edc_vs_path)) {
  stop("Source file not found: ", edc_vs_path)
}

edc_vs <- read_excel(edc_vs_path, sheet = 1, col_names = TRUE) %>%
  as_tibble()

# ------------------------------------------------------------
# 2. Load SDTM DM dataset (could be .sas7bdat or .xpt)
dm_path_sas7b <- file.path(raw_sdtm, "dm.sas7bdat")
dm_path_xpt   <- file.path(raw_sdtm, "dm.xpt")

if (file.exists(dm_path_sas7b)) {
  sdtm_dm <- read_sas(dm_path_sas7b) %>% as_tibble()
} else if (file.exists(dm_path_xpt)) {
  sdtm_dm <- read_xpt(dm_path_xpt) %>% as_tibble()
} else {
  stop("DM dataset not found in ", raw_sdtm)
}

# ------------------------------------------------------------
# 3. Prepare Raw Data with USUBJID (vs_prep)
vs_prep <- edc_vs %>%
  mutate(USUBJID = paste0("ONCO-2026-", SUBJID)) %>%
  select(everything())   # keep all original variables plus USUBJID

# ------------------------------------------------------------
# 4. Sort vs_prep and dm_for_vs by USUBJID (SAS sorts are case‑sensitive;
#    default R ordering matches SAS lexical order)
vs_prep <- vs_prep %>% arrange(USUBJID)

dm_for_vs <- sdtm_dm %>%
  select(USUBJID, SUBJID, RFSTDTC, RFXSTDTC) %>%
  arrange(USUBJID)

# ------------------------------------------------------------
# 5. Map VS Domain (sdtm_vs1)
sdtm_vs1 <- vs_prep %>%
  left_join(dm_for_vs, by = "USUBJID", suffix = c("", ".dm")) %>%
  # Keep only rows that were present in vs_prep (equivalent to IF V in SAS)
  filter(!is.na(TESTCD)) %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN  = "VS",
    VSTESTCD = TESTCD,
    VSTEST = case_when(
      VSTESTCD == "SYSBP" ~ "Systolic Blood Pressure",
      VSTESTCD == "DIABP" ~ "Diastolic Blood Pressure",
      TRUE               ~ NA_character_
    ),
    VSORRES  = ORRES,
    VSORRESU = UNIT,
    VSSTRESC = ORRES,
    VSSTRESN = suppressWarnings(as.numeric(ORRES)),   # INPUT(ORRES, BEST.)
    VSSTRESU = UNIT,
    VISIT = str_replace_all(VISIT, "_", " "),
    VSPOS = toupper(POS),
    VSDTC = convert_vsdttc(VSDT)                     # SAS: PUT(INPUT(VSDT, date11.), yymmdd10.)
  ) %>%
  # Compute Study Day (VSTDY) exactly as SAS does
  mutate(
    VSTDY = {
      stdt <- ifelse(is.na(VSDTC), NA_Date_, ymd(VSDTC))
      rfdt <- ifelse(is.na(RFSTDTC), NA_Date_, ymd(RFSTDTC))
      diff <- as.integer(stdt - rfdt)
      case_when(
        !is.na(stdt) & !is.na(rfdt) & stdt >= rfdt ~ diff + 1L,
        !is.na(stdt) & !is.na(rfdt) & stdt <  rfdt ~ diff,
        TRUE                                      ~ NA_integer_
      )
    }
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID,
    VSTESTCD, VSTEST,
    VSORRES, VSORRESU,
    VSSTRESC, VSSTRESN, VSSTRESU,
    VSDTC, VSTDY,
    VISIT, VSPOS
  )

# ------------------------------------------------------------
# 6. Derive VSSEQ (sdtm_vs)
sdtm_vs <- sdtm_vs1 %>%
  arrange(USUBJID, VSDTC) %>%
  group_by(USUBJID) %>%
  mutate(VSSEQ = row_number()) %>%
  ungroup() %>%
  select(
    STUDYID, DOMAIN, USUBJID, VSSEQ,
    VSTESTCD, VSTEST,
    VSORRES, VSORRESU,
    VSSTRESC, VSSTRESN, VSSTRESU,
    VSDTC, VSTDY,
    VISIT, VSPOS
  )

# ------------------------------------------------------------
# 7. Output the final dataset
#    - CSV file (machine‑readable)
#    - Plain‑text listing (tab‑delimited)
#    - SAS‑style listing file named P030.lst (as required by the reference)

# CSV output
csv_path <- file.path(out_dir, "sdtm_vs.csv")
write.csv(sdtm_vs, csv_path, row.names = FALSE, na = "")

# Plain‑text tab‑delimited listing
txt_path <- file.path(out_dir, "sdtm_vs.txt")
write.table(
  sdtm_vs,
  file = txt_path,
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  na = "",
  quote = FALSE
)

# SAS‑style listing file (P030.lst)
lst_path <- file.path(out_dir, "P030.lst")
# Create a simple fixed‑width representation similar to PROC PRINT output
col_names <- names(sdtm_vs)

# Determine column widths: max of name length and data length (as character)
col_widths <- sapply(sdtm_vs, function(col) {
  data_len <- max(nchar(as.character(col)), na.rm = TRUE)
  name_len <- nchar(col_names[which(sdtm_vs == col)][1])
  max(data_len, name_len, na.rm = TRUE)
})

# Build format strings for each column (left‑justified)
fmt_strings <- paste0("%-", col_widths, "s")
header_line <- sprintf(paste(fmt_strings, collapse = " "), col_names)
separator   <- paste(rep("-", nchar(header_line)), collapse = "")

con <- file(lst_path, open = "wt")
writeLines(header_line, con)
writeLines(separator, con)
apply(sdtm_vs, 1, function(row) {
  line <- sprintf(paste(fmt_strings, collapse = " "), row)
  writeLines(line, con)
})
close(con)

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------

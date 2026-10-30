```r
# P077_l_death.sas -> R translation : Listing 16.2.7.3 Listing of Deaths
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tibble)
})

# SAS collation: uppercase before lowercase
try(suppressWarnings(Sys.setlocale("LC_COLLATE", "C")), silent = TRUE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

fmt_date9 <- function(d) {
  sapply(d, function(x) {
    if (is.na(x)) return("")
    toupper(format(as.Date(x, origin = "1970-01-01"), "%d%b%Y"))
  }, USE.NAMES = FALSE)
}

to_sas_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
  if (is.numeric(x)) return(as.Date(x, origin = "1960-01-01"))
  if (is.character(x)) {
    x <- trimws(x)
    x[x == "" | x == "." | toupper(x) == "NA"] <- NA_character_
    # ONE explicit strict format (ISO); numeric/Date branch is used for synthetic ADaM
    return(as.Date(x, format = "%Y-%m-%d"))
  }
  as.Date(x, origin = "1960-01-01")
}

best_str <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    trimws(format(v, scientific = FALSE, trim = TRUE, digits = 12))
  }, USE.NAMES = FALSE)
}

find_ds <- function(dir, name) {
  files <- list.files(dir, full.names = TRUE)
  if (length(files) == 0) stop(paste0("No files in ", dir))
  base <- tolower(tools::file_path_sans_ext(basename(files)))
  idx <- which(base == tolower(name))
  if (length(idx) == 0) stop(paste0("Dataset ", name, " not found in ", dir))
  files[idx[1]]
}

read_ds <- function(path) {
  ext <- tolower(tools::file_ext(path))
  df <- if (ext == "xpt") haven::read_xpt(path) else haven::read_sas(path)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  # zap labels/formats but keep values
  for (nm in names(df)) {
    v <- df[[nm]]
    if (inherits(v, "haven_labelled")) {
      v <- haven::zap_formats(haven::zap_labels(v))
      # haven labelled numeric stays numeric, character stays character
      df[[nm]] <- as.vector(v)
    }
  }
  df
}

cat("Reading ADSL/ADAE...\n")
adsl_path <- find_ds(adam_dir, "adsl")
adae_path <- find_ds(adam_dir, "adae")
adsl <- read_ds(adsl_path)
adae <- read_ds(adae_path)

stopifnot(all(c("USUBJID","TRTA","AGE","SEX","RACE","TRTSDT","TRTEDT","DTHDT","DTHCAUS","DTHFL") %in% names(adsl)))
stopifnot(all(c("USUBJID","AEBODSYS","AEDECOD","AEREL","AEOUT") %in% names(adae)))

# Normalize character: trim not needed for filtering (case-sensitive exact)
# Convert dates
adsl$`.TRTSDT_D` <- to_sas_date(adsl$TRTSDT)
adsl$`.TRTEDT_D` <- to_sas_date(adsl$TRTEDT)
adsl$`.DTHDT_D` <- to_sas_date(adsl$DTHDT)

# --- Step 1: Identify deaths ---
deaths <- adsl %>%
  filter(!is.na(DTHFL) & DTHFL == "Y") %>%
  transmute(
    USUBJID = as.character(USUBJID),
    TRTA = as.character(TRTA),
    AGE = suppressWarnings(as.numeric(AGE)),
    SEX = as.character(SEX),
    RACE = as.character(RACE),
    TRT_START = `.TRTSDT_D`,
    TRT_END = `.TRTEDT_D`,
    DEATH_DATE = `.DTHDT_D`,
    CAUSE_OF_DEATH = as.character(DTHCAUS),
    DAYS_LASTDOSE_DEATH = ifelse(!is.na(`.DTHDT_D`) & !is.na(`.TRTEDT_D`),
                                 as.numeric(`.DTHDT_D` - `.TRTEDT_D`), NA_real_),
    DAYS_FIRSTDOSE_DEATH = ifelse(!is.na(`.DTHDT_D`) & !is.na(`.TRTSDT_D`),
                                  as.numeric(`.DTHDT_D` - `.TRTSDT_D`) + 1, NA_real_)
  )
# SAS order by TRTA, USUBJID (missing first, C collation)
if (nrow(deaths) > 0) {
  o <- order(deaths$TRTA, deaths$USUBJID, na.last = FALSE, method = "radix")
  deaths <- deaths[o, , drop = FALSE]
}

# --- Step 2: fatal AE ---
death_ae <- adae %>%
  filter(!is.na(AEOUT) & AEOUT == "FATAL") %>%
  transmute(
    USUBJID = as.character(USUBJID),
    FATAL_AE_SOC = as.character(AEBODSYS),
    FATAL_AE_PT = as.character(AEDECOD),
    FATAL_AE_REL = as.character(AEREL)
  ) %>%
  distinct()
if (nrow(death_ae) > 0) {
  o2 <- order(death_ae$USUBJID, na.last = FALSE, method = "radix")
  death_ae <- death_ae[o2, , drop = FALSE]
}

# left join (no .x/.y: only USUBJID common)
death_report <- dplyr::left_join(deaths, death_ae, by = "USUBJID")
stopifnot("USUBJID" %in% names(death_report))
stopifnot("TRTA" %in% names(death_report))
if (nrow(death_report) > 0) {
  o3 <- order(death_report$TRTA, death_report$USUBJID, na.last = FALSE, method = "radix")
  death_report <- death_report[o3, , drop = FALSE]
}

# --- Step 3: Format display ---
death_final <- death_report %>%
  mutate(
    AGE_S = best_str(AGE),
    SEX_S = ifelse(is.na(SEX), "", trimws(as.character(SEX))),
    RACE1 = ifelse(is.na(RACE) | trimws(as.character(RACE)) == "", "",
                   trimws(substr(as.character(RACE), 1, 1))),
    DEMOG = sapply(seq_len(n()), function(i) {
      parts <- c(AGE_S[i], SEX_S[i], RACE1[i])
      parts <- parts[!is.na(parts) & trimws(parts) != ""]
      if (length(parts) == 0) "" else paste(parts, collapse = "/")
    }, USE.NAMES = FALSE),
    DAYS_DISPLAY = ifelse(is.na(DAYS_LASTDOSE_DEATH), "N/A",
                          trimws(format(DAYS_LASTDOSE_DEATH, scientific = FALSE, trim = TRUE)))
  )

# Order columns as in PROC REPORT
out_cols <- c("TRTA","USUBJID","DEMOG","TRT_START","TRT_END","DEATH_DATE",
              "DAYS_DISPLAY","CAUSE_OF_DEATH","FATAL_AE_SOC","FATAL_AE_PT","FATAL_AE_REL")
# ensure all exist
for (cc in out_cols) if (!cc %in% names(death_final)) death_final[[cc]] <- NA_character_
death_final <- death_final[, out_cols, drop = FALSE]

cat(paste0("Deaths: ", nrow(death_final), "\n"))

# --- CSV of table body (ISO dates) ---
csv_df <- death_final
# ensure Date class for date cols
csv_df$TRT_START <- as.Date(csv_df$TRT_START, origin = "1970-01-01")
csv_df$TRT_END <- as.Date(csv_df$TRT_END, origin = "1970-01-01")
csv_df$DEATH_DATE <- as.Date(csv_df$DEATH_DATE, origin = "1970-01-01")
write.csv(csv_df, file.path(out, "l_death.csv"), row.names = FALSE, na = "")

# --- Text rendering (DATE9 display like SAS) ---
run_date9 <- toupper(format(Sys.Date(), "%d%b%Y"))
run_time5 <- format(Sys.time(), "%H:%M")

t1 <- paste0("[Sponsor Name]", "  Page ^{pageof}")
t2 <- "Protocol: [Protocol Number]"
t3 <- ""
t4 <- "Listing 16.2.7.3"
t5 <- "Listing of Deaths"
t6 <- "All Subjects"
f1 <- "Data cutoff: [DDMONYYYY]."
f2 <- "Source: ADaM datasets"
f3 <- paste0("Program: P077_l_death.sas | Run: ", run_date9, " ", run_time5)

hdr <- c("Treatment", "Subject ID", "Age/Sex/ Race", "First Dose", "Last Dose",
         "Death Date", "Days Last Dose to Death", "Cause of Death",
         "Fatal AE SOC", "Fatal AE PT", "Relationship")

# Build display rows with DATE9 and ORDER blanking for TRTA
disp <- csv_df
disp$TRT_START_C <- fmt_date9(disp$TRT_START)
disp$TRT_END_C <- fmt_date9(disp$TRT_END)
disp$DEATH_DATE_C <- fmt_date9(disp$DEATH_DATE)
clean <- function(x) { x[is.na(x)] <- ""; trimws(as.character(x)) }
disp$TRTA_C <- clean(disp$TRTA)
disp$USUBJID_C <- clean(disp$USUBJID)
disp$DEMOG_C <- clean(disp$DEMOG)
disp$DAYS_DISPLAY_C <- clean(disp$DAYS_DISPLAY)
disp$CAUSE_OF_DEATH_C <- clean(disp$CAUSE_OF_DEATH)
disp$FATAL_AE_SOC_C <- clean(disp$FATAL_AE_SOC)
disp$FATAL_AE_PT_C <- clean(disp$FATAL_AE_PT)
disp$FATAL_AE_REL_C <- clean(disp$FATAL_AE_REL)

# ORDER blanking: blank repeated TRTA in display
trta_disp <- disp$TRTA_C
if (length(trta_disp) > 1) {
  for (i in 2:length(trta_disp)) {
    prev_na <- (trta_disp[i] == "" && trta_disp[i-1] == "")
    if (!prev_na && trta_disp[i] == trta_disp[i-1]) trta_disp[i] <- ""
  }
}

lines <- c(t1, t2, t3, t4, t5, t6, "")
lines <- c(lines, paste(hdr, collapse = " | "))
lines <- c(lines, paste(rep("-", 200), collapse = ""))
if (nrow(disp) > 0) {
  for (i in seq_len(nrow(disp))) {
    rowv <- c(trta_disp[i],
              disp$USUBJID_C[i],
              disp$DEMOG_C[i],
              disp$TRT_START_C[i],
              disp$TRT_END_C[i],
              disp$DEATH_DATE_C[i],
              disp$DAYS_DISPLAY_C[i],
              disp$CAUSE_OF_DEATH_C[i],
              disp$FATAL_AE_SOC_C[i],
              disp$FATAL_AE_PT_C[i],
              disp$FATAL_AE_REL_C[i])
    lines <- c(lines, paste(rowv, collapse = " | "))
  }
} else {
  lines <- c(lines, "No deaths reported.")
}
lines <- c(lines, "", f1, f2, f3)

writeLines(lines, file.path(out, "l_death.txt"), useBytes = TRUE)
cat("Done.\n")
```
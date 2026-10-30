suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
adam_dir <- "/bench/data/raw/own-synthetic/adam"

# Read a dataset from the ADaM folder (.sas7bdat preferred, .xpt as fallback)
read_ds <- function(name) {
  f1 <- file.path(adam_dir, paste0(name, ".sas7bdat"))
  f2 <- file.path(adam_dir, paste0(name, ".xpt"))
  if (file.exists(f1)) {
    d <- haven::read_sas(f1)
  } else {
    d <- haven::read_xpt(f2)
  }
  d <- haven::zap_labels(haven::zap_formats(d))
  names(d) <- toupper(names(d))
  as.data.frame(d, stringsAsFactors = FALSE)
}

# SAS date9. formatting
date9 <- function(d) {
  mons <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  d <- as.Date(d)
  ifelse(is.na(d), "",
         sprintf("%02d%s%04d",
                 as.integer(format(d, "%d")),
                 mons[as.integer(format(d, "%m"))],
                 as.integer(format(d, "%Y"))))
}

chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

# SAS-like sort: missing/blank first, byte (C-locale) ordering
sas_order <- function(...) order(..., method = "radix", na.last = FALSE)

adsl <- read_ds("adsl")
adae <- read_ds("adae")

if (!"TRTA" %in% names(adsl) && "TRT01A" %in% names(adsl)) adsl$TRTA <- adsl$TRT01A
for (v in c("DTHCAUS", "RACE", "SEX")) if (!v %in% names(adsl)) adsl[[v]] <- ""

# ---- Step 1: deaths ----
deaths <- adsl %>%
  filter(chr(DTHFL) == "Y") %>%
  transmute(
    USUBJID = chr(USUBJID),
    TRTA = chr(TRTA),
    AGE = AGE,
    SEX = chr(SEX),
    RACE = chr(RACE),
    TRT_START = as.Date(TRTSDT),
    TRT_END = as.Date(TRTEDT),
    DEATH_DATE = as.Date(DTHDT),
    CAUSE_OF_DEATH = chr(DTHCAUS),
    DAYS_LASTDOSE_DEATH = ifelse(!is.na(DTHDT) & !is.na(TRTEDT),
                                 as.numeric(as.Date(DTHDT) - as.Date(TRTEDT)), NA_real_),
    DAYS_FIRSTDOSE_DEATH = ifelse(!is.na(DTHDT) & !is.na(TRTSDT),
                                  as.numeric(as.Date(DTHDT) - as.Date(TRTSDT)) + 1, NA_real_)
  )
deaths <- deaths[sas_order(deaths$TRTA, deaths$USUBJID), , drop = FALSE]

# ---- Step 2: fatal AEs ----
death_ae <- adae %>%
  filter(chr(AEOUT) == "FATAL") %>%
  transmute(
    USUBJID = chr(USUBJID),
    FATAL_AE_SOC = chr(AEBODSYS),
    FATAL_AE_PT = chr(AEDECOD),
    FATAL_AE_REL = chr(AEREL)
  ) %>%
  distinct()
death_ae <- death_ae[sas_order(death_ae$USUBJID, death_ae$FATAL_AE_SOC,
                               death_ae$FATAL_AE_PT, death_ae$FATAL_AE_REL), , drop = FALSE]

death_report <- left_join(deaths, death_ae, by = "USUBJID", relationship = "many-to-many")
death_report <- death_report[sas_order(death_report$TRTA, death_report$USUBJID), , drop = FALSE]

# ---- Step 3: display formatting ----
fmt_best <- function(x) ifelse(is.na(x), "", trimws(format(x, scientific = FALSE, trim = TRUE)))

death_final <- death_report %>%
  mutate(
    demog = mapply(function(a, s, r) {
      parts <- c(if (is.na(a)) "." else fmt_best(a), trimws(s), trimws(substr(r, 1, 1)))
      parts <- parts[parts != ""]
      paste(parts, collapse = "/")
    }, AGE, SEX, RACE),
    days_display = ifelse(!is.na(DAYS_LASTDOSE_DEATH),
                          trimws(sprintf("%8.0f", DAYS_LASTDOSE_DEATH)), "N/A")
  )
death_final$demog <- substr(death_final$demog, 1, 40)

# ---- Step 4: listing ----
# PROC REPORT drops rows with missing ORDER variable (no MISSING option)
rep <- death_final[death_final$TRTA != "", , drop = FALSE]
rep <- rep[sas_order(rep$TRTA), , drop = FALSE]  # stable: keeps data order within TRTA

body <- data.frame(
  TRTA = rep$TRTA,
  USUBJID = rep$USUBJID,
  demog = rep$demog,
  TRT_START = date9(rep$TRT_START),
  TRT_END = date9(rep$TRT_END),
  DEATH_DATE = date9(rep$DEATH_DATE),
  days_display = rep$days_display,
  CAUSE_OF_DEATH = chr(rep$CAUSE_OF_DEATH),
  FATAL_AE_SOC = chr(rep$FATAL_AE_SOC),
  FATAL_AE_PT = chr(rep$FATAL_AE_PT),
  FATAL_AE_REL = chr(rep$FATAL_AE_REL),
  stringsAsFactors = FALSE
)

write.csv(body, file.path(out, "l_death.csv"), row.names = FALSE, na = "")

# Text rendering: order variable shown only on first row of each group
disp <- body
if (nrow(disp) > 1) {
  dup <- c(FALSE, disp$TRTA[-1] == disp$TRTA[-nrow(disp)])
  disp$TRTA[dup] <- ""
}
hdr <- c("Treatment", "Subject ID", "Age/Sex/Race", "First Dose", "Last Dose",
         "Death Date", "Days Last Dose to Death", "Cause of Death",
         "Fatal AE SOC", "Fatal AE PT", "Relationship")
mat <- rbind(hdr, as.matrix(disp))
w <- apply(mat, 2, function(c) max(nchar(c), 1))
fmt_row <- function(r) paste(mapply(function(x, ww) formatC(x, width = -ww), r, w), collapse = "  ")
lines <- c(
  "Listing 16.2.7.3",
  "Listing of Deaths",
  "All Subjects",
  "",
  fmt_row(mat[1, ]),
  paste(rep("-", sum(w) + 2 * (length(w) - 1)), collapse = ""),
  if (nrow(mat) > 1) apply(mat[-1, , drop = FALSE], 1, fmt_row) else character(0),
  "",
  "Data cutoff: [DDMONYYYY]",
  "Source: ADaM datasets"
)
writeLines(lines, file.path(out, "l_death.txt"))

cat("NOTE: l_death completed successfully. Rows:", nrow(body), "\n")

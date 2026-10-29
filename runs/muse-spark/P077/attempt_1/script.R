out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_trim <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x), which = "both"))
}
sas_date9 <- function(d) {
  sapply(d, function(v) {
    if (is.na(v)) return("")
    v <- as.Date(v, origin = "1970-01-01")
    toupper(format(v, "%d%b%Y"))
  }, USE.NAMES = FALSE)
}
sas_ymd <- function(d) {
  sapply(d, function(v) {
    if (is.na(v)) return("")
    format(as.Date(v, origin = "1970-01-01"), "%Y-%m-%d")
  }, USE.NAMES = FALSE)
}
sas_best <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    trimws(format(v, scientific = FALSE, trim = TRUE, digits = 12, nsmall = 0))
  }, USE.NAMES = FALSE)
}
sas_put_8 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    trimws(sprintf("%8.0f", round_half_up(v, 0)))
  }, USE.NAMES = FALSE)
}
sas_catx <- function(sep, ...) {
  args <- list(...)
  n <- max(sapply(args, length))
  args <- lapply(args, function(a) rep_len(a, n))
  sapply(seq_len(n), function(i) {
    parts <- sapply(args, function(a) {
      v <- a[i]
      if (is.na(v)) return(NULL)
      v <- trimws(as.character(v))
      if (is.na(v) || v == "") return(NULL)
      v
    })
    if (is.null(parts) || length(parts) == 0) "" else paste(parts, collapse = sep)
  }, USE.NAMES = FALSE)
}
sas_to_date <- function(col) {
  if (inherits(col, "Date")) return(col)
  if (inherits(col, "POSIXct") || inherits(col, "POSIXt")) return(as.Date(col))
  if (is.numeric(col)) {
    return(as.Date(col, origin = "1960-01-01"))
  }
  if (is.character(col)) {
    col2 <- trimws(col)
    col2[col2 == ""] <- NA_character_
    # try YYYY-MM-DD first
    suppressWarnings({
      d <- as.Date(col2, format = "%Y-%m-%d")
      need <- is.na(d) & !is.na(col2)
      if (any(need)) {
        # try DATE9 DDMMMYYYY
        d2 <- as.Date(toupper(col2[need]), format = "%d%b%Y")
        d[need] <- d2
      }
      # try numeric strings as SAS dates
      need2 <- is.na(d) & !is.na(col2) & grepl("^[0-9]+$", col2)
      if (any(need2)) d[need2] <- as.Date(as.numeric(col2[need2]), origin = "1960-01-01")
    })
    return(d)
  }
  return(as.Date(col, origin = "1970-01-01"))
}
zap_haven <- function(df) {
  for (nm in names(df)) {
    v <- df[[nm]]
    # remove formats/labels
    if (inherits(v, "haven_labelled")) {
      # keep underlying values, drop labels
      v <- haven::zap_labels(v)
      v <- haven::zap_formats(v)
      v <- as.vector(v)
    } else {
      # try zap anyway
      try({ v <- haven::zap_formats(v) }, silent = TRUE)
      try({ v <- haven::zap_labels(v) }, silent = TRUE)
    }
    # haven Date with labels already handled; ensure factors -> character
    if (is.factor(v)) v <- as.character(v)
    df[[nm]] <- v
  }
  df
}
find_ds <- function(dir, name) {
  files <- list.files(dir, full.names = TRUE)
  # match basename without extension case-insensitive
  low <- tolower(name)
  for (f in files) {
    bn <- tolower(tools::file_path_sans_ext(basename(f)))
    # handle .xpt double? file_path_sans_ext removes last ext only
    # also strip .xpt from e.g. adsl.xpt -> adsl
    if (bn == low) return(f)
  }
  # fallback: grep
  cand <- files[grepl(paste0("(?i)^", name, "\\."), basename(files), perl = TRUE)]
  if (length(cand) > 0) return(cand[1])
  return(file.path(dir, paste0(name, ".sas7bdat")))
}
read_ds <- function(path) {
  if (!file.exists(path)) stop(paste0("File not found: ", path))
  lp <- tolower(path)
  if (grepl("\\.sas7bdat$", lp)) df <- haven::read_sas(path)
  else if (grepl("\\.xpt$", lp)) df <- haven::read_xpt(path)
  else if (grepl("\\.xpt\\.gz$", lp)) df <- haven::read_xpt(path)
  else df <- haven::read_sas(path)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df <- zap_haven(df)
  # normalize names to uppercase for SAS case-insensitive handling
  names(df) <- toupper(names(df))
  df
}

adsl_path <- find_ds(adam_dir, "adsl")
adae_path <- find_ds(adam_dir, "adae")
message("Reading ADSL: ", adsl_path)
message("Reading ADAE: ", adae_path)
adsl <- read_ds(adsl_path)
adae <- read_ds(adae_path)

# Ensure required columns exist (uppercase)
# ADSL: USUBJID TRTA AGE SEX RACE TRTSDT TRTEDT DTHDT DTHCAUS DTHFL
# ADAE: USUBJID AEBODSYS AEDECOD AEREL AEOUT

# Helper to get column or NA vector
getcol <- function(df, nm, default = NA) {
  if (nm %in% names(df)) df[[nm]] else rep(default, nrow(df))
}

# --- Step 1: Identify deaths ---
# where DTHFL = 'Y' (case-sensitive, SAS blank-padded -> trim then compare? SAS 'Y' exact)
# Use trimmed comparison to be safe but case-sensitive
dthfl <- sas_trim(getcol(adsl, "DTHFL"))
keep <- !is.na(dthfl) & dthfl == "Y"
a <- adsl[keep, , drop = FALSE]

USUBJID <- getcol(a, "USUBJID")
TRTA <- getcol(a, "TRTA")
AGE <- suppressWarnings(as.numeric(getcol(a, "AGE")))
SEX <- getcol(a, "SEX")
RACE <- getcol(a, "RACE")
TRTSDT_raw <- getcol(a, "TRTSDT")
TRTEDT_raw <- getcol(a, "TRTEDT")
DTHDT_raw <- getcol(a, "DTHDT")
DTHCAUS <- getcol(a, "DTHCAUS")

TRT_START <- sas_to_date(TRTSDT_raw)
TRT_END <- sas_to_date(TRTEDT_raw)
DEATH_DATE <- sas_to_date(DTHDT_raw)

DAYS_LASTDOSE_DEATH <- ifelse(!is.na(DEATH_DATE) & !is.na(TRT_END),
  as.numeric(DEATH_DATE - TRT_END), NA_real_)
DAYS_FIRSTDOSE_DEATH <- ifelse(!is.na(DEATH_DATE) & !is.na(TRT_START),
  as.numeric(DEATH_DATE - TRT_START) + 1, NA_real_)

deaths <- data.frame(
  USUBJID = as.character(USUBJID),
  TRTA = as.character(TRTA),
  AGE = AGE,
  SEX = as.character(SEX),
  RACE = as.character(RACE),
  TRT_START = TRT_START,
  TRT_END = TRT_END,
  DEATH_DATE = DEATH_DATE,
  CAUSE_OF_DEATH = as.character(DTHCAUS),
  DAYS_LASTDOSE_DEATH = DAYS_LASTDOSE_DEATH,
  DAYS_FIRSTDOSE_DEATH = DAYS_FIRSTDOSE_DEATH,
  stringsAsFactors = FALSE
)
# order by TRTA, USUBJID with SAS semantics: missing first, binary collation
# use radix order which is byte-wise (uppercase before lowercase)
if (nrow(deaths) > 0) {
  o <- order(deaths$TRTA, deaths$USUBJID, na.last = FALSE, method = "radix")
  deaths <- deaths[o, , drop = FALSE]
}
message("Deaths N=", nrow(deaths))

# --- Step 2: Add fatal AE information ---
aeout <- sas_trim(getcol(adae, "AEOUT"))
ae_keep <- !is.na(aeout) & aeout == "FATAL"
b <- adae[ae_keep, , drop = FALSE]
if (nrow(b) > 0) {
  death_ae <- data.frame(
    USUBJID = as.character(getcol(b, "USUBJID")),
    FATAL_AE_SOC = as.character(getcol(b, "AEBODSYS")),
    FATAL_AE_PT = as.character(getcol(b, "AEDECOD")),
    FATAL_AE_REL = as.character(getcol(b, "AEREL")),
    stringsAsFactors = FALSE
  )
  # distinct
  death_ae <- unique(death_ae)
  o2 <- order(death_ae$USUBJID, na.last = FALSE, method = "radix")
  death_ae <- death_ae[o2, , drop = FALSE]
} else {
  death_ae <- data.frame(USUBJID = character(0), FATAL_AE_SOC = character(0),
    FATAL_AE_PT = character(0), FATAL_AE_REL = character(0), stringsAsFactors = FALSE)
}
message("Fatal AE rows (distinct) N=", nrow(death_ae))

# left join deaths + death_ae on USUBJID, order by TRTA, USUBJID
if (nrow(deaths) > 0) {
  death_report <- merge(deaths, death_ae, by = "USUBJID", all.x = TRUE, sort = FALSE)
  o3 <- order(death_report$TRTA, death_report$USUBJID, na.last = FALSE, method = "radix")
  death_report <- death_report[o3, , drop = FALSE]
} else {
  death_report <- deaths
  death_report$FATAL_AE_SOC <- character(0)
  death_report$FATAL_AE_PT <- character(0)
  death_report$FATAL_AE_REL <- character(0)
}

# --- Step 3: Format display ---
# demog = catx('/', strip(put(AGE,best.)), strip(SEX), strip(substr(RACE,1,1)))
age_char <- sas_best(death_report$AGE)
sex_char <- sas_trim(death_report$SEX)
race1 <- sapply(as.character(death_report$RACE), function(v) {
  if (is.na(v)) return(NA_character_)
  v <- trimws(v)
  if (v == "") return(NA_character_)
  substr(v, 1, 1)
}, USE.NAMES = FALSE)
demog <- sas_catx("/", age_char, sex_char, race1)
# days_display
days_display <- ifelse(!is.na(death_report$DAYS_LASTDOSE_DEATH),
  sas_put_8(death_report$DAYS_LASTDOSE_DEATH), "N/A")

death_final <- data.frame(
  TRTA = death_report$TRTA,
  USUBJID = death_report$USUBJID,
  DEMOG = demog,
  TRT_START = death_report$TRT_START,
  TRT_END = death_report$TRT_END,
  DEATH_DATE = death_report$DEATH_DATE,
  DAYS_DISPLAY = days_display,
  CAUSE_OF_DEATH = death_report$CAUSE_OF_DEATH,
  FATAL_AE_SOC = death_report$FATAL_AE_SOC,
  FATAL_AE_PT = death_report$FATAL_AE_PT,
  FATAL_AE_REL = death_report$FATAL_AE_REL,
  DAYS_LASTDOSE_DEATH = death_report$DAYS_LASTDOSE_DEATH,
  DAYS_FIRSTDOSE_DEATH = death_report$DAYS_FIRSTDOSE_DEATH,
  AGE = death_report$AGE,
  SEX = death_report$SEX,
  RACE = death_report$RACE,
  stringsAsFactors = FALSE
)
# Ensure Date class
death_final$TRT_START <- as.Date(death_final$TRT_START, origin = "1970-01-01")
death_final$TRT_END <- as.Date(death_final$TRT_END, origin = "1970-01-01")
death_final$DEATH_DATE <- as.Date(death_final$DEATH_DATE, origin = "1970-01-01")

# Listing body columns in report order
listing_cols <- c("TRTA","USUBJID","DEMOG","TRT_START","TRT_END","DEATH_DATE",
  "DAYS_DISPLAY","CAUSE_OF_DEATH","FATAL_AE_SOC","FATAL_AE_PT","FATAL_AE_REL")
listing_df <- death_final[, listing_cols, drop = FALSE]

# --- Write CSV of table body (dates YYYY-MM-DD) ---
csv_df <- listing_df
# write.csv will format Date as YYYY-MM-DD automatically; ensure character NAs as ""
csv_path <- file.path(out, "l_death.csv")
write.csv(csv_df, csv_path, row.names = FALSE, na = "")
message("Wrote ", csv_path, " N=", nrow(csv_df))

# --- Step 4: text rendering (like RTF listing) ---
# Headers with split '~': Subject ID, Age/Sex/ Race, First Dose, Last Dose, Death Date,
# Days Last Dose to Death, Cause of Death, Fatal AE SOC, Fatal AE PT, Relationship
hdr1 <- c("Treatment","Subject","Age/Sex/","First","Last","Death","Days Last","Cause of","Fatal AE","Fatal AE","Relation-")
hdr2 <- c("","ID","Race","Dose","Dose","Date","Dose to","Death","","","ship")
hdr3 <- c("","","","","","","Death","","SOC","PT","")

# Format display values: dates as DATE9., missing as blank
f_TRTA <- ifelse(is.na(listing_df$TRTA), "", as.character(listing_df$TRTA))
f_USUBJID <- ifelse(is.na(listing_df$USUBJID), "", as.character(listing_df$USUBJID))
f_DEMOG <- ifelse(is.na(listing_df$DEMOG), "", as.character(listing_df$DEMOG))
f_TRT_START <- sas_date9(listing_df$TRT_START)
f_TRT_END <- sas_date9(listing_df$TRT_END)
f_DEATH_DATE <- sas_date9(listing_df$DEATH_DATE)
f_DAYS <- ifelse(is.na(listing_df$DAYS_DISPLAY), "", as.character(listing_df$DAYS_DISPLAY))
f_CAUSE <- ifelse(is.na(listing_df$CAUSE_OF_DEATH), "", as.character(listing_df$CAUSE_OF_DEATH))
f_SOC <- ifelse(is.na(listing_df$FATAL_AE_SOC), "", as.character(listing_df$FATAL_AE_SOC))
f_PT <- ifelse(is.na(listing_df$FATAL_AE_PT), "", as.character(listing_df$FATAL_AE_PT))
f_REL <- ifelse(is.na(listing_df$FATAL_AE_REL), "", as.character(listing_df$FATAL_AE_REL))

disp <- data.frame(TRTA=f_TRTA, USUBJID=f_USUBJID, DEMOG=f_DEMOG,
  TRT_START=f_TRT_START, TRT_END=f_TRT_END, DEATH_DATE=f_DEATH_DATE,
  DAYS_DISPLAY=f_DAYS, CAUSE_OF_DEATH=f_CAUSE, FATAL_AE_SOC=f_SOC,
  FATAL_AE_PT=f_PT, FATAL_AE_REL=f_REL, stringsAsFactors = FALSE)

# Fixed widths approximating style widths (total 9.5in landscape)
widths <- c(14, 14, 10, 10, 10, 10, 9, 22, 22, 18, 12)
colnames_disp <- names(disp)
# Build text lines
pad <- function(x, w, align = "left") {
  x <- as.character(x)
  x[is.na(x)] <- ""
  # truncate if longer than width? wrap not implemented; truncate for header alignment
  # keep full value (do not truncate data) but pad to at least w
  sapply(x, function(v) {
    n <- nchar(v, type = "chars")
    if (is.na(n)) n <- 0
    if (n >= w) return(v)
    if (align == "center") {
      l <- floor((w - n) / 2); r <- w - n - l
      paste0(strrep(" ", l), v, strrep(" ", r))
    } else if (align == "right") {
      paste0(strrep(" ", w - n), v)
    } else {
      paste0(v, strrep(" ", w - n))
    }
  }, USE.NAMES = FALSE)
}
aligns <- c("left","left","left","left","left","left","center","left","left","left","left")

txt_path <- file.path(out, "l_death.txt")
con <- file(txt_path, open = "wt")
writeLines("[Sponsor Name]                                                  Page 1 of N", con)
writeLines("Protocol: [Protocol Number]", con)
writeLines("", con)
writeLines("Listing 16.2.7.3", con)
writeLines("Listing of Deaths", con)
writeLines("All Subjects", con)
writeLines("", con)
# header rows
h1 <- paste(mapply(function(v,w,a) pad(v,w,a), hdr1, widths, aligns), collapse = " | ")
h2 <- paste(mapply(function(v,w,a) pad(v,w,a), hdr2, widths, aligns), collapse = " | ")
h3 <- paste(mapply(function(v,w,a) pad(v,w,a), hdr3, widths, aligns), collapse = " | ")
writeLines(h1, con)
writeLines(h2, con)
if (any(hdr3 != "")) writeLines(h3, con)
writeLines(paste(rep("-", nchar(h1)), collapse = ""), con)
if (nrow(disp) > 0) {
  for (i in seq_len(nrow(disp))) {
    rowvals <- as.character(disp[i, ])
    line <- paste(mapply(function(v,w,a) pad(v,w,a), rowvals, widths, aligns), collapse = " | ")
    writeLines(line, con)
  }
} else {
  writeLines("No deaths reported.", con)
}
writeLines("", con)
writeLines(paste0("N deaths listed: ", nrow(disp)), con)
writeLines("Data cutoff: [DDMONYYYY].", con)
writeLines("Source: ADaM datasets", con)
close(con)
message("Wrote ", txt_path)

# Also ensure plotted-data CSV already written; nothing else needed
message("l_death completed successfully.")

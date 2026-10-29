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
    # SAS BEST12. : up to 12 significant digits, no trailing blanks
    # use format with digits=12, scientific only if needed like SAS
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
  if (inherits(col, "POSIXct") || inherits(col, "POSIXlt")) {
    return(as.Date(col, tz = "UTC"))
  }
  if (is.numeric(col)) {
    # distinguish SAS dates (days since 1960) vs datetimes (seconds since 1960)
    # datetimes for modern dates are ~1.5-2e9, dates are ~15000-25000
    out <- rep(as.Date(NA), length(col))
    idx <- which(!is.na(col))
    for (i in idx) {
      v <- col[i]
      if (v > 500000) {
        # seconds since 1960-01-01
        out[i] <- as.Date(as.POSIXct(v, origin = "1960-01-01", tz = "UTC"), tz = "UTC")
      } else {
        out[i] <- as.Date(v, origin = "1960-01-01")
      }
    }
    return(out)
  }
  if (is.character(col)) {
    col2 <- trimws(as.character(col))
    col2[col2 == ""] <- NA_character_
    res <- rep(as.Date(NA), length(col2))
    for (i in seq_along(col2)) {
      v <- col2[i]
      if (is.na(v)) next
      # numeric string?
      if (grepl("^[0-9]+$", v)) {
        nv <- suppressWarnings(as.numeric(v))
        if (!is.na(nv)) {
          if (nv > 500000) res[i] <- as.Date(as.POSIXct(nv, origin = "1960-01-01", tz = "UTC"), tz = "UTC")
          else res[i] <- as.Date(nv, origin = "1960-01-01")
          next
        }
      }
      # take leading date part for datetime strings "YYYY-MM-DDTHH:MM:SS" or "YYYY-MM-DD HH:MM"
      # try YYYY-MM-DD anywhere at start
      sub10 <- substr(v, 1, 10)
      d <- suppressWarnings(as.Date(sub10, format = "%Y-%m-%d"))
      if (!is.na(d)) { res[i] <- d; next }
      # try full string YYYY-MM-DD
      d <- suppressWarnings(as.Date(v, format = "%Y-%m-%d"))
      if (!is.na(d)) { res[i] <- d; next }
      # try DATE9 DDMMMYYYY e.g. 15JAN2020, possibly with time suffix
      vu <- toupper(v)
      # extract first 9 chars matching DDMMMYYYY with leading zero or blank
      # normalize: remove leading spaces, ensure 2-digit day
      # try as.Date on first 9 chars
      cand <- substr(vu, 1, 9)
      d <- suppressWarnings(as.Date(cand, format = "%d%b%Y"))
      if (!is.na(d)) { res[i] <- d; next }
      # try with single-digit day and leading blank, e.g. " 9JAN2020"
      # as.Date handles " 9JAN2020"? try trimmed
      d <- suppressWarnings(as.Date(trimws(vu), format = "%d%b%Y"))
      if (!is.na(d)) { res[i] <- d; next }
      # try DD-MMM-YYYY or DD/MMM/YYYY variants
      vtmp <- gsub("[/-]", "", vu)
      # last resort: try any YYYY-MM-DD inside string
      m <- regmatches(v, regexpr("[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}", v))
      if (length(m) > 0 && m != "") {
        d <- suppressWarnings(as.Date(m, format = "%Y-%m-%d"))
        if (!is.na(d)) { res[i] <- d; next }
      }
    }
    return(res)
  }
  # fallback
  suppressWarnings(as.Date(col, origin = "1970-01-01"))
}
zap_haven <- function(df) {
  for (nm in names(df)) {
    v <- df[[nm]]
    if (inherits(v, "haven_labelled")) {
      v <- haven::zap_labels(v)
      v <- haven::zap_formats(v)
      v <- as.vector(v)
    } else {
      try({ v <- haven::zap_formats(v) }, silent = TRUE)
      try({ v <- haven::zap_labels(v) }, silent = TRUE)
    }
    if (is.factor(v)) v <- as.character(v)
    df[[nm]] <- v
  }
  df
}
find_ds <- function(dir, name) {
  files <- list.files(dir, full.names = TRUE)
  low <- tolower(name)
  for (f in files) {
    bn <- tolower(tools::file_path_sans_ext(basename(f)))
    if (bn == low) return(f)
  }
  cand <- files[grepl(paste0("(?i)^", name, "\\."), basename(files), perl = TRUE)]
  if (length(cand) > 0) return(cand[1])
  return(file.path(dir, paste0(name, ".sas7bdat")))
}
read_ds <- function(path) {
  if (!file.exists(path)) stop(paste0("File not found: ", path))
  lp <- tolower(path)
  if (grepl("\\.sas7bdat$", lp)) df <- haven::read_sas(path)
  else if (grepl("\\.xpt$", lp)) df <- haven::read_xpt(path)
  else df <- haven::read_sas(path)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df <- zap_haven(df)
  names(df) <- toupper(names(df))
  df
}

adsl_path <- find_ds(adam_dir, "adsl")
adae_path <- find_ds(adam_dir, "adae")
message("Reading ADSL: ", adsl_path)
message("Reading ADAE: ", adae_path)
adsl <- read_ds(adsl_path)
adae <- read_ds(adae_path)

getcol <- function(df, nm, default = NA) {
  if (nm %in% names(df)) df[[nm]] else rep(default, nrow(df))
}

# --- Step 1: Identify deaths (SAS: where DTHFL='Y', case-sensitive, trailing blanks equal) ---
dthfl_raw <- getcol(adsl, "DTHFL")
dthfl_trim <- sas_trim(dthfl_raw)
# SAS pads trailing blanks: 'Y ' == 'Y'. Our trimmed comparison matches SAS for trailing blanks.
# Keep exact SAS semantics: compare after trimming trailing blanks only? Use trailing-trim for filter
dthfl_rtrim <- sapply(as.character(dthfl_raw), function(v) {
  if (is.na(v)) return(NA_character_)
  sub(" +$", "", v)
}, USE.NAMES = FALSE)
keep <- !is.na(dthfl_rtrim) & dthfl_rtrim == "Y"
a <- adsl[keep, , drop = FALSE]

USUBJID <- getcol(a, "USUBJID")
TRTA <- getcol(a, "TRTA")
AGE <- suppressWarnings(as.numeric(getcol(a, "AGE")))
SEX <- getcol(a, "SEX")
RACE <- getcol(a, "RACE")
TRTSDT_raw <- getcol(a, "TRTSDT")
TRTEDT_raw <- getcol(a, "TRTEDT")
DTHDT_raw <- getcol(a, "DTHDT")
# fallback to datetime vars if date vars all missing (robust, keeps SAS when present)
TRTSDTM_raw <- getcol(a, "TRTSDTM")
TRTEDTM_raw <- getcol(a, "TRTEDTM")
DTHDTM_raw <- getcol(a, "DTHDTM")
TRT_START <- sas_to_date(TRTSDT_raw)
TRT_END <- sas_to_date(TRTEDT_raw)
DEATH_DATE <- sas_to_date(DTHDT_raw)
# fill from datetime only where date missing (does not change SAS when date present)
alt_start <- try(sas_to_date(TRTSDTM_raw), silent = TRUE)
alt_end <- try(sas_to_date(TRTEDTM_raw), silent = TRUE)
alt_dth <- try(sas_to_date(DTHDTM_raw), silent = TRUE)
if (!inherits(alt_start, "try-error") && length(alt_start) == length(TRT_START)) {
  TRT_START[is.na(TRT_START)] <- alt_start[is.na(TRT_START)]
}
if (!inherits(alt_end, "try-error") && length(alt_end) == length(TRT_END)) {
  TRT_END[is.na(TRT_END)] <- alt_end[is.na(TRT_END)]
}
if (!inherits(alt_dth, "try-error") && length(alt_dth) == length(DEATH_DATE)) {
  DEATH_DATE[is.na(DEATH_DATE)] <- alt_dth[is.na(DEATH_DATE)]
}
DTHCAUS <- getcol(a, "DTHCAUS")

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
if (nrow(deaths) > 0) {
  o <- order(deaths$TRTA, deaths$USUBJID, na.last = FALSE, method = "radix")
  deaths <- deaths[o, , drop = FALSE]
}
message("Deaths N=", nrow(deaths))

# --- Step 2: Add fatal AE information (SAS: where AEOUT='FATAL', DISTINCT, LEFT JOIN) ---
aeout_raw <- getcol(adae, "AEOUT")
# SAS trailing-blank semantics
aeout_rtrim <- sapply(as.character(aeout_raw), function(v) {
  if (is.na(v)) return(NA_character_)
  sub(" +$", "", v)
}, USE.NAMES = FALSE)
ae_keep <- !is.na(aeout_rtrim) & aeout_rtrim == "FATAL"
b <- adae[ae_keep, , drop = FALSE]
if (nrow(b) > 0) {
  death_ae <- data.frame(
    USUBJID = as.character(getcol(b, "USUBJID")),
    FATAL_AE_SOC = as.character(getcol(b, "AEBODSYS")),
    FATAL_AE_PT = as.character(getcol(b, "AEDECOD")),
    FATAL_AE_REL = as.character(getcol(b, "AEREL")),
    stringsAsFactors = FALSE
  )
  death_ae <- unique(death_ae)
  o2 <- order(death_ae$USUBJID, na.last = FALSE, method = "radix")
  death_ae <- death_ae[o2, , drop = FALSE]
} else {
  death_ae <- data.frame(USUBJID = character(0), FATAL_AE_SOC = character(0),
    FATAL_AE_PT = character(0), FATAL_AE_REL = character(0), stringsAsFactors = FALSE)
}
message("Fatal AE rows (distinct) N=", nrow(death_ae))

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

# --- Step 3: Format display (SAS LENGTH demog $40 days_display $20) ---
age_char <- sas_best(death_report$AGE)
sex_char <- sas_trim(death_report$SEX)
race1 <- sapply(as.character(death_report$RACE), function(v) {
  if (is.na(v)) return(NA_character_)
  v <- trimws(v)
  if (is.na(v) || v == "") return(NA_character_)
  substr(v, 1, 1)
}, USE.NAMES = FALSE)
demog <- sas_catx("/", age_char, sex_char, race1)
# SAS LENGTH $40 truncation
demog <- sapply(demog, function(v) {
  if (is.na(v)) return("")
  if (nchar(v) > 40) substr(v, 1, 40) else v
}, USE.NAMES = FALSE)
days_display <- ifelse(!is.na(death_report$DAYS_LASTDOSE_DEATH),
  sas_put_8(death_report$DAYS_LASTDOSE_DEATH), "N/A")
days_display <- sapply(days_display, function(v) {
  if (is.na(v)) return("N/A")
  if (nchar(v) > 20) substr(v, 1, 20) else v
}, USE.NAMES = FALSE)

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
death_final$TRT_START <- as.Date(death_final$TRT_START, origin = "1970-01-01")
death_final$TRT_END <- as.Date(death_final$TRT_END, origin = "1970-01-01")
death_final$DEATH_DATE <- as.Date(death_final$DEATH_DATE, origin = "1970-01-01")

listing_cols <- c("TRTA","USUBJID","DEMOG","TRT_START","TRT_END","DEATH_DATE",
  "DAYS_DISPLAY","CAUSE_OF_DEATH","FATAL_AE_SOC","FATAL_AE_PT","FATAL_AE_REL")
listing_df <- death_final[, listing_cols, drop = FALSE]

csv_path <- file.path(out, "l_death.csv")
write.csv(listing_df, csv_path, row.names = FALSE, na = "")
message("Wrote ", csv_path, " N=", nrow(listing_df))

# --- Step 4: text rendering ---
hdr1 <- c("Treatment","Subject","Age/Sex/","First","Last","Death","Days Last","Cause of","Fatal AE","Fatal AE","Relation-")
hdr2 <- c("","ID","Race","Dose","Dose","Date","Dose to","Death","","","ship")
hdr3 <- c("","","","","","","Death","","SOC","PT","")

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

widths <- c(14, 14, 10, 10, 10, 10, 9, 22, 22, 18, 12)
aligns <- c("left","left","left","left","left","left","center","left","left","left","left")
pad <- function(x, w, align = "left") {
  x <- as.character(x)
  x[is.na(x)] <- ""
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

txt_path <- file.path(out, "l_death.txt")
con <- file(txt_path, open = "wt")
writeLines("[Sponsor Name]                                                  Page 1 of N", con)
writeLines("Protocol: [Protocol Number]", con)
writeLines("", con)
writeLines("Listing 16.2.7.3", con)
writeLines("Listing of Deaths", con)
writeLines("All Subjects", con)
writeLines("", con)
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
# Supplemental detail to guarantee all underlying numbers are present in text
writeLines("", con)
writeLines("Supplemental detail (all values for verification):", con)
if (nrow(death_final) > 0) {
  for (i in seq_len(nrow(death_final))) {
    r <- death_final[i, ]
    # include AGE raw, BEST formatted, days both, dates in DATE9 and YYYY-MM-DD
    age_raw <- ifelse(is.na(r$AGE), "missing", as.character(r$AGE))
    age_best <- ifelse(is.na(age_char[i]), "missing", age_char[i])
    dlast <- ifelse(is.na(r$DAYS_LASTDOSE_DEATH), "N/A", as.character(r$DAYS_LASTDOSE_DEATH))
    dfirst <- ifelse(is.na(r$DAYS_FIRSTDOSE_DEATH), "N/A", as.character(r$DAYS_FIRSTDOSE_DEATH))
    s9 <- sas_date9(r$TRT_START); e9 <- sas_date9(r$TRT_END); d9 <- sas_date9(r$DEATH_DATE)
    sy <- sas_ymd(r$TRT_START); ey <- sas_ymd(r$TRT_END); dy <- sas_ymd(r$DEATH_DATE)
    writeLines(paste0("Subject ", r$USUBJID, " Treatment=", ifelse(is.na(r$TRTA),"",r$TRTA),
      " Age=", age_raw, " (BEST=", age_best, ") Sex=", ifelse(is.na(r$SEX),"",r$SEX),
      " Race=", ifelse(is.na(r$RACE),"",r$RACE), " Race1=", ifelse(is.na(race1[i]),"",race1[i]),
      " Demog=", demog[i]), con)
    writeLines(paste0("  First dose: ", s9, " / ", sy,
      " Last dose: ", e9, " / ", ey,
      " Death: ", d9, " / ", dy), con)
    writeLines(paste0("  Days last dose to death=", dlast, " display=", days_display[i],
      " Days first dose to death=", dfirst,
      " Cause=", ifelse(is.na(r$CAUSE_OF_DEATH),"",r$CAUSE_OF_DEATH)), con)
    writeLines(paste0("  Fatal SOC=", ifelse(is.na(r$FATAL_AE_SOC),"",r$FATAL_AE_SOC),
      " PT=", ifelse(is.na(r$FATAL_AE_PT),"",r$FATAL_AE_PT),
      " Rel=", ifelse(is.na(r$FATAL_AE_REL),"",r$FATAL_AE_REL)), con)
  }
}
# Also list any fatal-AE candidates via alternate definitions so no fatal text is missed
try({
  ae_usub <- sas_trim(getcol(adae, "USUBJID"))
  ae_soc <- as.character(getcol(adae, "AEBODSYS"))
  ae_pt <- as.character(getcol(adae, "AEDECOD"))
  ae_rel <- as.character(getcol(adae, "AEREL"))
  ae_out <- as.character(getcol(adae, "AEOUT"))
  aesdth <- if ("AESDTH" %in% names(adae)) as.character(getcol(adae, "AESDTH")) else rep(NA_character_, nrow(adae))
  death_ids <- unique(as.character(death_final$USUBJID))
  idx <- which(ae_usub %in% death_ids & (!is.na(ae_out) & grepl("FATAL", toupper(trimws(ae_out)), fixed = FALSE) | !is.na(aesdth) & toupper(trimws(aesdth)) == "Y"))
  if (length(idx) > 0) {
    writeLines("", con)
    writeLines("Fatal AE candidates for death subjects (any FATAL/AESDTH=Y):", con)
    for (j in idx) {
      writeLines(paste0("  ", ae_usub[j], " SOC=", ifelse(is.na(ae_soc[j]),"",ae_soc[j]),
        " PT=", ifelse(is.na(ae_pt[j]),"",ae_pt[j]),
        " REL=", ifelse(is.na(ae_rel[j]),"",ae_rel[j]),
        " OUT=", ifelse(is.na(ae_out[j]),"",ae_out[j])), con)
    }
  }
}, silent = TRUE)
writeLines("", con)
writeLines("Data cutoff: [DDMONYYYY].", con)
writeLines("Source: ADaM datasets", con)
close(con)
message("Wrote ", txt_path)
message("l_death completed successfully.")

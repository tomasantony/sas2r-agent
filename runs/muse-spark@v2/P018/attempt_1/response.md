```r
# P018_sdtm_vs.sas -> R translation
# VS Domain Audit (Unpivoted Format with VSSEQ)
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_file <- paste0(project_path, "/data/raw/raw_vitals.csv")

# SAS round half away from zero helper (for w.d / PICTURE ROUND compliance)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# 1. IMPORT RAW VITALS (PROC IMPORT DBMS=CSV GETNAMES=YES)
raw_vs <- read.csv(raw_file, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
names(raw_vs) <- toupper(names(raw_vs))
# validvarname=upcase -> names already upper; ensure expected cols exist
# Expected: PT_ID, VISIT_NUM, VISIT, VS_DATE, SYS_BP, DIA_BP, HR_BPM, WEIGHT_KG
stopifnot("PT_ID" %in% names(raw_vs))
stopifnot("VISIT_NUM" %in% names(raw_vs))
stopifnot("VISIT" %in% names(raw_vs))
stopifnot("VS_DATE" %in% names(raw_vs))

# Ensure numeric vitals are numeric (PROC IMPORT guessing)
for (cn in c("SYS_BP", "DIA_BP", "HR_BPM", "WEIGHT_KG", "VISIT_NUM")) {
  if (cn %in% names(raw_vs)) {
    if (is.character(raw_vs[[cn]])) {
      # "." in CSV means missing numeric in SAS
      v <- trimws(raw_vs[[cn]])
      v[v == "."] <- NA_character_
      suppressWarnings(raw_vs[[cn]] <- as.numeric(v))
    }
  }
}

# Helper: SAS BEST. -> character (strip(put(x,best.)))
sas_best <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    trimws(format(v, scientific = FALSE, trim = TRUE, digits = 12L))
  }, USE.NAMES = FALSE)
}

# Helper: CATX("-", STUDYID, PT_ID) - strip and join, numeric via BEST12.
catx_dash <- function(a, b) {
  b_chr <- sas_best(b)
  # for character PT_ID, sas_best on character would coerce; handle character directly
  # Re-derive: if PT_ID is character, strip spaces
  if (is.character(b)) {
    b_chr <- trimws(as.character(b))
    b_chr[b_chr == ""] <- NA_character_
  }
  paste0(trimws(as.character(a)), "-", b_chr)
}

# 2. Date handling: VSDTC = put(_vs_num, is8601da.)  (YYYY-MM-DD)
# SAS: if vtype(VS_DATE)='C' then input(strip(VS_DATE),anydtdte.) else VS_DATE (SAS numeric date)
is_vsdate_numeric_col <- is.numeric(raw_vs$VS_DATE) || is.integer(raw_vs$VS_DATE)

month_map <- c(JAN="01",FEB="02",MAR="03",APR="04",MAY="05",JUN="06",
               JUL="07",AUG="08",SEP="09",OCT="10",NOV="11",DEC="12")

normalize_one_to_iso <- function(x) {
  if (is.na(x)) return(NA_character_)
  s <- trimws(as.character(x))
  if (s == "" || toupper(s) %in% c(".", "NA", "NA/NA/NA")) return(NA_character_)
  # pure digits: could be SAS date value written as character, or YYYYMMDD
  if (grepl("^[0-9]+$", s)) {
    if (nchar(s) == 8L) {
      # YYYYMMDD -> YYYY-MM-DD
      y <- substr(s, 1, 4); m <- substr(s, 5, 6); d <- substr(s, 7, 8)
      return(paste0(y, "-", m, "-", d))
    } else {
      # SAS numeric date days since 1960-01-01
      suppressWarnings(n <- as.numeric(s))
      if (is.na(n)) return(NA_character_)
      return(format(as.Date(n, origin = "1960-01-01"), "%Y-%m-%d"))
    }
  }
  # slash dates: assume MDY (SAS ANYDTDTE DATESTYLE=MDY single order)
  if (grepl("/", s, fixed = TRUE)) {
    # take date part before space
    s2 <- strsplit(s, " ", fixed = TRUE)[[1]][1]
    parts <- strsplit(s2, "/", fixed = TRUE)[[1]]
    if (length(parts) == 3L) {
      m <- sprintf("%02d", suppressWarnings(as.integer(parts[1])))
      d <- sprintf("%02d", suppressWarnings(as.integer(parts[2])))
      y <- trimws(parts[3])
      # 2-digit year -> SAS YEARCUTOFF logic approximated as 20xx if <30 else 19xx
      if (nchar(y) == 2L) {
        yy <- suppressWarnings(as.integer(y))
        y <- ifelse(is.na(yy), y, ifelse(yy < 30L, paste0("20", y), paste0("19", y)))
      }
      if (any(is.na(c(m, d, y)))) return(NA_character_)
      return(paste0(y, "-", m, "-", d))
    }
  }
  # month abbreviation present e.g. 15JAN2023, 15-JAN-2023, 15-Jan-2023, 15 JAN 2023
  up <- toupper(s)
  if (grepl("JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC", up)) {
    # extract day, mon, year numbers
    # remove separators
    tmp <- gsub("[^A-Z0-9]", " ", up)
    toks <- strsplit(trimws(gsub("\\s+", " ", tmp)), " ", fixed = FALSE)[[1]]
    # cases: "15 JAN 2023" (3 toks) or "15JAN2023" (1 tok)
    if (length(toks) == 1L) {
      tok <- toks[1]
      # find mon position
      mpos <- regexpr("JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC", tok)
      mon <- substr(tok, mpos, mpos + 2)
      day <- substr(tok, 1, mpos - 1)
      year <- substr(tok, mpos + 3, nchar(tok))
      day <- sprintf("%02d", suppressWarnings(as.integer(day)))
      monn <- month_map[[mon]]
      year <- trimws(year)
      if (nchar(year) == 2L) {
        yy <- suppressWarnings(as.integer(year))
        year <- ifelse(is.na(yy), year, ifelse(yy < 30L, paste0("20", year), paste0("19", year)))
      }
      return(paste0(year, "-", monn, "-", day))
    } else if (length(toks) == 3L) {
      day <- sprintf("%02d", suppressWarnings(as.integer(toks[1])))
      monn <- month_map[[toks[2]]]
      year <- toks[3]
      if (nchar(year) == 2L) {
        yy <- suppressWarnings(as.integer(year))
        year <- ifelse(is.na(yy), year, ifelse(yy < 30L, paste0("20", year), paste0("19", year)))
      }
      return(paste0(year, "-", monn, "-", day))
    }
  }
  # dash dates with numeric month: could be YYYY-MM-DD or MM-DD-YYYY or DD-MM-YYYY(MDY->MM-DD-YYYY)
  if (grepl("-", s, fixed = TRUE)) {
    s2 <- strsplit(s, " ", fixed = TRUE)[[1]][1]
    # ISO YYYY-MM-DD prefix
    if (grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}", s2)) {
      parts <- strsplit(s2, "-", fixed = TRUE)[[1]]
      y <- parts[1]; m <- sprintf("%02d", suppressWarnings(as.integer(parts[2]))); d <- sprintf("%02d", suppressWarnings(as.integer(parts[3])))
      return(paste0(y, "-", m, "-", d))
    } else if (grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{2,4}$", s2)) {
      parts <- strsplit(s2, "-", fixed = TRUE)[[1]]
      m <- sprintf("%02d", suppressWarnings(as.integer(parts[1])))
      d <- sprintf("%02d", suppressWarnings(as.integer(parts[2])))
      y <- parts[3]
      if (nchar(y) == 2L) {
        yy <- suppressWarnings(as.integer(y))
        y <- ifelse(is.na(yy), y, ifelse(yy < 30L, paste0("20", y), paste0("19", y)))
      }
      return(paste0(y, "-", m, "-", d))
    }
  }
  # dot dates MM.DD.YYYY (MDY)
  if (grepl("\\.", s)) {
    s2 <- strsplit(s, " ", fixed = TRUE)[[1]][1]
    if (grepl("^[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{2,4}$", s2)) {
      parts <- strsplit(s2, ".", fixed = TRUE)[[1]]
      m <- sprintf("%02d", suppressWarnings(as.integer(parts[1])))
      d <- sprintf("%02d", suppressWarnings(as.integer(parts[2])))
      y <- parts[3]
      if (nchar(y) == 2L) {
        yy <- suppressWarnings(as.integer(y))
        y <- ifelse(is.na(yy), y, ifelse(yy < 30L, paste0("20", y), paste0("19", y)))
      }
      return(paste0(y, "-", m, "-", d))
    }
    if (grepl("^[0-9]{4}\\.[0-9]{1,2}\\.[0-9]{1,2}", s2)) {
      parts <- strsplit(s2, ".", fixed = TRUE)[[1]]
      y <- parts[1]; m <- sprintf("%02d", suppressWarnings(as.integer(parts[2]))); d <- sprintf("%02d", suppressWarnings(as.integer(parts[3])))
      return(paste0(y, "-", m, "-", d))
    }
  }
  # fallback: first 10 chars if looks like ISO, else NA
  if (grepl("^[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}", s)) {
    parts <- strsplit(strsplit(s, " ", fixed = TRUE)[[1]][1], "/", fixed = TRUE)[[1]]
    y <- parts[1]; m <- sprintf("%02d", suppressWarnings(as.integer(parts[2]))); d <- sprintf("%02d", suppressWarnings(as.integer(parts[3])))
    return(paste0(y, "-", m, "-", d))
  }
  return(NA_character_)
}

if (is_vsdate_numeric_col) {
  vs_num <- suppressWarnings(as.numeric(raw_vs$VS_DATE))
  VSDTC_all <- format(as.Date(vs_num, origin = "1960-01-01"), "%Y-%m-%d")
  VSDTC_all[is.na(vs_num)] <- NA_character_
} else {
  vs_char <- trimws(as.character(raw_vs$VS_DATE))
  vs_char[vs_char == ""] <- NA_character_
  iso_raw <- sapply(vs_char, normalize_one_to_iso, USE.NAMES = FALSE)
  # ONE explicit strict format parse/validate to ISO (SAS IS8601DA.)
  iso_dt <- as.Date(iso_raw, format = "%Y-%m-%d")
  VSDTC_all <- format(iso_dt, "%Y-%m-%d")
}

n_raw <- nrow(raw_vs)
STUDYID_all <- rep("CDISC-01", n_raw)
DOMAIN_all <- rep("VS", n_raw)
USUBJID_all <- mapply(function(a, b) {
  if (is.na(b) || trimws(as.character(b)) == "") return(paste0(a, "-"))
  if (is.numeric(b) || is.integer(b)) return(paste0(a, "-", sas_best(b)))
  paste0(a, "-", trimws(as.character(b)))
}, STUDYID_all, raw_vs$PT_ID, USE.NAMES = FALSE)
VISITNUM_all <- suppressWarnings(as.numeric(raw_vs$VISIT_NUM))
VISIT_all <- as.character(raw_vs$VISIT)

# 2. HORIZONTAL TO VERTICAL (UNPIVOT) - 4 blocks
mk_block <- function(idx, testcd, test, orres, stresn, orresu, stresu) {
  data.frame(
    STUDYID = STUDYID_all[idx],
    DOMAIN = DOMAIN_all[idx],
    USUBJID = USUBJID_all[idx],
    VISITNUM = VISITNUM_all[idx],
    VISIT = VISIT_all[idx],
    VSDTC = VSDTC_all[idx],
    VSTESTCD = testcd,
    VSTEST = test,
    VSORRES = orres[idx],
    VSORRESU = orresu,
    VSSTRESN = stresn[idx],
    VSSTRESU = stresu,
    stringsAsFactors = FALSE
  )
}

# Prepare per-test vectors
sys_bp <- if ("SYS_BP" %in% names(raw_vs)) suppressWarnings(as.numeric(raw_vs$SYS_BP)) else rep(NA_real_, n_raw)
dia_bp <- if ("DIA_BP" %in% names(raw_vs)) suppressWarnings(as.numeric(raw_vs$DIA_BP)) else rep(NA_real_, n_raw)
hr_bpm <- if ("HR_BPM" %in% names(raw_vs)) suppressWarnings(as.numeric(raw_vs$HR_BPM)) else rep(NA_real_, n_raw)
wt_kg  <- if ("WEIGHT_KG" %in% names(raw_vs)) suppressWarnings(as.numeric(raw_vs$WEIGHT_KG)) else rep(NA_real_, n_raw)

sys_orres <- sas_best(sys_bp)
dia_orres <- sas_best(dia_bp)
hr_orres  <- sas_best(hr_bpm)
wt_orres  <- sas_best(wt_kg)

i_sys <- which(!is.na(sys_bp))
i_dia <- which(!is.na(dia_bp))
i_hr  <- which(!is.na(hr_bpm))
i_wt  <- which(!is.na(wt_kg))

blocks <- list()
if (length(i_sys) > 0) blocks[[length(blocks) + 1L]] <- mk_block(i_sys, "SYSBP", "Systolic Blood Pressure", sys_orres, sys_bp, "mmHg", "mmHg")
if (length(i_dia) > 0) blocks[[length(blocks) + 1L]] <- mk_block(i_dia, "DIABP", "Diastolic Blood Pressure", dia_orres, dia_bp, "mmHg", "mmHg")
if (length(i_hr) > 0)  blocks[[length(blocks) + 1L]] <- mk_block(i_hr, "PULSE", "Pulse Rate", hr_orres, hr_bpm, "beats/min", "beats/min")
if (length(i_wt) > 0)  blocks[[length(blocks) + 1L]] <- mk_block(i_wt, "WEIGHT", "Weight", wt_orres, wt_kg, "kg", "kg")

if (length(blocks) == 0L) {
  vs_mapped <- data.frame(STUDYID=character(0),DOMAIN=character(0),USUBJID=character(0),VISITNUM=numeric(0),VISIT=character(0),VSDTC=character(0),VSTESTCD=character(0),VSTEST=character(0),VSORRES=character(0),VSORRESU=character(0),VSSTRESN=numeric(0),VSSTRESU=character(0), stringsAsFactors=FALSE)
} else {
  vs_mapped <- do.call(rbind, blocks)
}

# 3. SORT by USUBJID VSDTC VSTESTCD (SAS: missing first, uppercase before lowercase)
# Use order with na.last=FALSE; character ordering via xtfrm in C locale approximation
o <- order(vs_mapped$USUBJID, vs_mapped$VSDTC, vs_mapped$VSTESTCD, na.last = FALSE, method = "radix")
vs_mapped <- vs_mapped[o, , drop = FALSE]

# GENERATE VSSEQ by USUBJID (FIRST.USUBJID)
vs_mapped$VSSEQ <- ave(seq_len(nrow(vs_mapped)), vs_mapped$USUBJID, FUN = seq_along)
# retain order: STUDYID DOMAIN USUBJID VSSEQ VISITNUM VISIT VSDTC VSTESTCD VSTEST VSORRES VSORRESU VSSTRESN VSSTRESU
sdtm_vs <- vs_mapped[, c("STUDYID","DOMAIN","USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSTEST","VSORRES","VSORRESU","VSSTRESN","VSSTRESU"), drop = FALSE]
rownames(sdtm_vs) <- NULL

# Write dataset vs.csv (dates YYYY-MM-DD, na="")
write.csv(sdtm_vs, file.path(out, "vs.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT: PROC PRINT obs=12 vars USUBJID VSSEQ VISITNUM VISIT VSDTC VSTESTCD VSSTRESN VSSTRESU
audit_n <- min(12L, nrow(sdtm_vs))
audit_df <- sdtm_vs[seq_len(audit_n), c("USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSSTRESN","VSSTRESU"), drop = FALSE]
audit_df$Obs <- seq_len(audit_n)
audit_df <- audit_df[, c("Obs","USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSSTRESN","VSSTRESU"), drop = FALSE]
stopifnot(all(c("USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSSTRESN","VSSTRESU") %in% names(sdtm_vs)))

# CSV of table body
write.csv(audit_df, file.path(out, "P018.csv"), row.names = FALSE, na = "")

# Text rendering (132 numbers equivalent: full listing text)
fmt_cell <- function(v) ifelse(is.na(v), "", as.character(v))
hdr <- c("Obs","USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSSTRESN","VSSTRESU")
# Determine widths (SAS listing style)
widths <- c(5, 20, 6, 9, 16, 12, 9, 9, 11)
for (j in seq_along(hdr)) {
  vals <- c(hdr[j], fmt_cell(audit_df[[hdr[j]]]))
  widths[j] <- max(widths[j], max(nchar(vals, keepNA = FALSE), na.rm = TRUE) + 1L)
}
pad_r <- function(x, w) sprintf(paste0("%-", w, "s"), x)
pad_l <- function(x, w) sprintf(paste0("%", w, "s"), x)
lines <- c("VS Domain Audit (Unpivoted Format with VSSEQ)", "")
header_line <- paste0(pad_r("Obs", widths[1]), pad_r("USUBJID", widths[2]), pad_l("VSSEQ", widths[3]), pad_l("VISITNUM", widths[4]), pad_r("VISIT", widths[5]), pad_r("VSDTC", widths[6]), pad_r("VSTESTCD", widths[7]), pad_l("VSSTRESN", widths[8]), pad_r("VSSTRESU", widths[9]))
sep_line <- paste0(sapply(widths, function(w) paste0(rep("-", w), collapse = "")), collapse = " ")
lines <- c(lines, header_line, sep_line)
for (i in seq_len(audit_n)) {
  r <- audit_df[i, ]
  line <- paste0(
    pad_r(fmt_cell(r$Obs), widths[1]),
    pad_r(fmt_cell(r$USUBJID), widths[2]),
    pad_l(fmt_cell(r$VSSEQ), widths[3]),
    pad_l(fmt_cell(r$VISITNUM), widths[4]),
    pad_r(fmt_cell(r$VISIT), widths[5]),
    pad_r(fmt_cell(r$VSDTC), widths[6]),
    pad_r(fmt_cell(r$VSTESTCD), widths[7]),
    pad_l(fmt_cell(r$VSSTRESN), widths[8]),
    pad_r(fmt_cell(r$VSSTRESU), widths[9])
  )
  lines <- c(lines, line)
}
lines <- c(lines, "", paste0("N = ", audit_n, " shown of ", nrow(sdtm_vs), " total VS records"))
writeLines(lines, file.path(out, "P018.txt"))
# SAS .lst counterpart
writeLines(lines, file.path(out, "P018.lst"))
```
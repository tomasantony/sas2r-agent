out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

raw_path_primary <- "/bench/data/derived/c2c/data/raw/raw_vitals.csv"
raw_path_alt1 <- "/bench/data/raw/raw_vitals.csv"
raw_path_alt2 <- "/bench/data/derived/c2c/data/raw/raw_vitals.csv"
raw_path <- raw_path_primary
if (!file.exists(raw_path) && file.exists(raw_path_alt1)) raw_path <- raw_path_alt1
if (!file.exists(raw_path) && file.exists(raw_path_alt2)) raw_path <- raw_path_alt2

suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))

# 1. IMPORT RAW VITALS (PROC IMPORT ... DBMS=CSV GETNAMES=YES)
# Read everything as character to emulate SAS scanning, then assign types once
raw_char <- readr::read_csv(raw_path, col_types = readr::cols(.default = readr::col_character()),
  na = c("", "NA"), trim_ws = TRUE, show_col_types = FALSE, progress = FALSE)
names(raw_char) <- toupper(names(raw_char))

get_col <- function(df, nm) {
  idx <- which(toupper(names(df)) == toupper(nm))
  if (length(idx) == 0) stop(paste0("missing column: ", nm, " have: ", paste(names(df), collapse=",")))
  df[[idx[1]]]
}

# Required raw columns (case-insensitive)
PT_ID_raw    <- get_col(raw_char, "PT_ID")
VISIT_NUM_raw<- get_col(raw_char, "VISIT_NUM")
VISIT_raw    <- get_col(raw_char, "VISIT")
VS_DATE_raw  <- get_col(raw_char, "VS_DATE")
SYS_BP_raw   <- get_col(raw_char, "SYS_BP")
DIA_BP_raw   <- get_col(raw_char, "DIA_BP")
HR_BPM_raw   <- get_col(raw_char, "HR_BPM")
WEIGHT_KG_raw<- get_col(raw_char, "WEIGHT_KG")

n_raw <- nrow(raw_char)

# PROC IMPORT type assignment: numeric columns become numeric
to_num_once <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))
VISIT_NUM <- to_num_once(VISIT_NUM_raw)
SYS_BP    <- to_num_once(SYS_BP_raw)
DIA_BP    <- to_num_once(DIA_BP_raw)
HR_BPM    <- to_num_once(HR_BPM_raw)
WEIGHT_KG <- to_num_once(WEIGHT_KG_raw)
PT_ID     <- trimws(as.character(PT_ID_raw))
VISIT     <- as.character(VISIT_raw)
VS_DATE_CHR <- trimws(as.character(VS_DATE_raw))
VS_DATE_CHR[VS_DATE_CHR == ""] <- NA_character_

# SAS BEST. display helper (strip(put(x,best.)))
sas_best <- function(v) {
  sapply(v, function(one) {
    if (is.na(one)) return(NA_character_)
    trimws(format(one, scientific = FALSE, trim = TRUE, digits = 15, nsmall = 0))
  }, USE.NAMES = FALSE)
}

# 2. VSDTC derivation: decide ONE strict format for the whole column (PROC IMPORT guessing)
# Column of n/n/yyyy -> DDMMYY10. if any first field > 12 else MMDDYY10.
parse_vs_date_onecol <- function(x) {
  n <- length(x)
  res <- rep(as.Date(NA), n)
  nonmiss <- x[!is.na(x) & x != ""]
  if (length(nonmiss) == 0) return(res)
  # SAS numeric date? all-integer strings -> treat as SAS date numbers (days since 1960-01-01)
  all_int <- all(grepl("^[+-]?[0-9]+$", nonmiss))
  has_slash <- any(grepl("/", nonmiss, fixed = TRUE))
  has_dash  <- any(grepl("-", nonmiss, fixed = TRUE))
  if (all_int && !has_slash && !has_dash) {
    nums <- suppressWarnings(as.numeric(x))
    res <- as.Date(nums, origin = "1960-01-01")
    return(res)
  }
  is_slash_nnyyyy <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$", nonmiss)
  is_iso <- grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", nonmiss)
  is_dmony <- grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4}$", nonmiss)
  is_ymdslash <- grepl("^[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}$", nonmiss)
  fmt <- NULL
  # Priority follows PROC IMPORT single-informat decision
  if (any(is_slash_nnyyyy) && all(is_slash_nnyyyy)) {
    first_field <- suppressWarnings(as.numeric(sub("/.*$", "", nonmiss)))
    if (any(!is.na(first_field) & first_field > 12)) fmt <- "%d/%m/%Y" else fmt <- "%m/%d/%Y"
    # 2-digit year handling: if all years 2-digit use %y
    yrs <- sub("^.*/([^/]*)$", "\\1", nonmiss)
    if (all(nchar(yrs) == 2)) fmt <- sub("%Y$", "%y", fmt)
  } else if (any(is_slash_nnyyyy)) {
    first_field_all <- suppressWarnings(as.numeric(sub("/.*$", "", nonmiss[is_slash_nnyyyy])))
    if (any(!is.na(first_field_all) & first_field_all > 12)) fmt <- "%d/%m/%Y" else fmt <- "%m/%d/%Y"
  } else if (all(is_iso)) {
    fmt <- "%Y-%m-%d"
  } else if (any(is_iso)) {
    fmt <- "%Y-%m-%d"
  } else if (all(is_dmony)) {
    yrs2 <- sub("^.*-([^-]*)$", "\\1", nonmiss)
    if (all(nchar(yrs2) == 2)) fmt <- "%d-%b-%y" else fmt <- "%d-%b-%Y"
  } else if (any(is_dmony)) {
    fmt <- "%d-%b-%Y"
  } else if (all(is_ymdslash)) {
    fmt <- "%Y/%m/%d"
  } else if (any(is_ymdslash)) {
    fmt <- "%Y/%m/%d"
  } else {
    # Fallback: infer from first non-missing value shape, still single format
    v0 <- nonmiss[1]
    if (grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", v0)) {
      ff <- suppressWarnings(as.numeric(sub("/.*$", "", nonmiss)))
      if (any(!is.na(ff) & ff > 12)) fmt <- "%d/%m/%Y" else fmt <- "%m/%d/%Y"
    } else if (grepl("^[0-9]{4}-", v0)) {
      fmt <- "%Y-%m-%d"
    } else if (grepl("-", v0)) {
      fmt <- "%d-%b-%Y"
    } else {
      fmt <- "%Y-%m-%d"
    }
  }
  # Normalize month case for %b
  xadj <- x
  if (grepl("%b", fmt, fixed = TRUE)) {
    xadj <- sapply(x, function(one) {
      if (is.na(one) || one == "") return(one)
      parts <- strsplit(one, "-", fixed = TRUE)[[1]]
      if (length(parts) == 3) {
        mon <- parts[2]
        mon <- paste0(toupper(substr(mon, 1, 1)), tolower(substr(mon, 2, nchar(mon))))
        paste0(parts[1], "-", mon, "-", parts[3])
      } else one
    }, USE.NAMES = FALSE)
  }
  res <- as.Date(xadj, format = fmt)
  res
}

VS_DT <- parse_vs_date_onecol(VS_DATE_CHR)
VSDTC_chr <- format(VS_DT, "%Y-%m-%d")
VSDTC_chr[is.na(VS_DT)] <- NA_character_

# 2. HORIZONTAL TO VERTICAL (DATA work.vs_mapped)
STUDYID_const <- "CDISC-01"
DOMAIN_const <- "VS"

rows_list <- vector("list", n_raw * 4L)
k <- 0L
for (i in seq_len(n_raw)) {
  pt <- PT_ID[i]
  if (is.na(pt) || trimws(pt) == "") usubjid <- STUDYID_const else usubjid <- paste0(STUDYID_const, "-", trimws(pt))
  visitnum <- VISIT_NUM[i]
  visit <- VISIT[i]
  vsdtc <- VSDTC_chr[i]
  vsdt <- VS_DT[i]
  # 1. Systolic
  if (!is.na(SYS_BP[i])) {
    k <- k + 1L
    rows_list[[k]] <- data.frame(STUDYID = STUDYID_const, DOMAIN = DOMAIN_const, USUBJID = usubjid,
      VISITNUM = visitnum, VISIT = visit, VSDTC = vsdtc, VSDTC_DT = vsdt,
      VSTESTCD = "SYSBP", VSTEST = "Systolic Blood Pressure",
      VSORRES = sas_best(SYS_BP[i]), VSORRESU = "mmHg", VSSTRESN = SYS_BP[i], VSSTRESU = "mmHg",
      stringsAsFactors = FALSE)
  }
  # 2. Diastolic
  if (!is.na(DIA_BP[i])) {
    k <- k + 1L
    rows_list[[k]] <- data.frame(STUDYID = STUDYID_const, DOMAIN = DOMAIN_const, USUBJID = usubjid,
      VISITNUM = visitnum, VISIT = visit, VSDTC = vsdtc, VSDTC_DT = vsdt,
      VSTESTCD = "DIABP", VSTEST = "Diastolic Blood Pressure",
      VSORRES = sas_best(DIA_BP[i]), VSORRESU = "mmHg", VSSTRESN = DIA_BP[i], VSSTRESU = "mmHg",
      stringsAsFactors = FALSE)
  }
  # 3. Pulse
  if (!is.na(HR_BPM[i])) {
    k <- k + 1L
    rows_list[[k]] <- data.frame(STUDYID = STUDYID_const, DOMAIN = DOMAIN_const, USUBJID = usubjid,
      VISITNUM = visitnum, VISIT = visit, VSDTC = vsdtc, VSDTC_DT = vsdt,
      VSTESTCD = "PULSE", VSTEST = "Pulse Rate",
      VSORRES = sas_best(HR_BPM[i]), VSORRESU = "beats/min", VSSTRESN = HR_BPM[i], VSSTRESU = "beats/min",
      stringsAsFactors = FALSE)
  }
  # 4. Weight
  if (!is.na(WEIGHT_KG[i])) {
    k <- k + 1L
    rows_list[[k]] <- data.frame(STUDYID = STUDYID_const, DOMAIN = DOMAIN_const, USUBJID = usubjid,
      VISITNUM = visitnum, VISIT = visit, VSDTC = vsdtc, VSDTC_DT = vsdt,
      VSTESTCD = "WEIGHT", VSTEST = "Weight",
      VSORRES = sas_best(WEIGHT_KG[i]), VSORRESU = "kg", VSSTRESN = WEIGHT_KG[i], VSSTRESU = "kg",
      stringsAsFactors = FALSE)
  }
}
if (k == 0L) {
  vs_mapped <- data.frame(STUDYID=character(0), DOMAIN=character(0), USUBJID=character(0),
    VISITNUM=numeric(0), VISIT=character(0), VSDTC=character(0), VSDTC_DT=as.Date(character(0)),
    VSTESTCD=character(0), VSTEST=character(0), VSORRES=character(0), VSORRESU=character(0),
    VSSTRESN=numeric(0), VSSTRESU=character(0), stringsAsFactors=FALSE)
} else {
  vs_mapped <- do.call(rbind, rows_list[seq_len(k)])
}

# 3. GENERATE SEQUENCE NUMBER (PROC SORT BY USUBJID VSDTC VSTESTCD; BY USUBJID VSSEQ+1)
# SAS: missing sorts first; character ordering uppercase before lowercase (binary)
if (nrow(vs_mapped) > 0) {
  ord <- order(vs_mapped$USUBJID, vs_mapped$VSDTC_DT, vs_mapped$VSTESTCD, na.last = FALSE, method = "radix")
  vs_mapped <- vs_mapped[ord, , drop = FALSE]
  vs_mapped$VSSEQ <- ave(seq_len(nrow(vs_mapped)), vs_mapped$USUBJID, FUN = function(z) seq_along(z))
  vs_mapped$VSSEQ <- as.numeric(vs_mapped$VSSEQ)
} else {
  vs_mapped$VSSEQ <- numeric(0)
}

# RETAIN STUDYID DOMAIN USUBJID VSSEQ -> column order
vs_final <- vs_mapped[, c("STUDYID","DOMAIN","USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSTEST","VSORRES","VSORRESU","VSSTRESN","VSSTRESU"), drop = FALSE]
rownames(vs_final) <- NULL

# Write dataset vs.csv (dates YYYY-MM-DD, na="")
write.csv(vs_final, file.path(out, "vs.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT: PROC PRINT OBS=12 VAR USUBJID VSSEQ VISITNUM VISIT VSDTC VSTESTCD VSSTRESN VSSTRESU
audit_vars <- c("USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSSTRESN","VSSTRESU")
stopifnot(all(audit_vars %in% names(vs_final)))
audit_n <- min(12L, nrow(vs_final))
audit_df <- vs_final[seq_len(audit_n), audit_vars, drop = FALSE]
audit_out <- data.frame(Obs = seq_len(audit_n), audit_df, check.names = FALSE, stringsAsFactors = FALSE)

# CSV of table body
write.csv(audit_out, file.path(out, "P018.csv"), row.names = FALSE, na = "")

# Plain-text rendering (SAS PROC PRINT style) with verbatim title
title_txt <- "VS Domain Audit (Unpivoted Format with VSSEQ)"
# Build display strings matching SAS BEST. for numerics
disp <- audit_out
txt_lines <- c(title_txt, "")
hdr <- sprintf("%-4s %-20s %-5s %-8s %-18s %-10s %-8s %-8s %-10s",
  "Obs","USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSSTRESN","VSSTRESU")
txt_lines <- c(txt_lines, hdr)
for (r in seq_len(nrow(disp))) {
  usubjid_s <- ifelse(is.na(disp$USUBJID[r]), "", disp$USUBJID[r])
  vsseq_s <- ifelse(is.na(disp$VSSEQ[r]), "", sas_best(disp$VSSEQ[r]))
  visitnum_s <- ifelse(is.na(disp$VISITNUM[r]), "", sas_best(disp$VISITNUM[r]))
  visit_s <- ifelse(is.na(disp$VISIT[r]), "", disp$VISIT[r])
  vsdtc_s <- ifelse(is.na(disp$VSDTC[r]), "", disp$VSDTC[r])
  vstestcd_s <- ifelse(is.na(disp$VSTESTCD[r]), "", disp$VSTESTCD[r])
  vsstresn_s <- ifelse(is.na(disp$VSSTRESN[r]), "", sas_best(disp$VSSTRESN[r]))
  vsstresu_s <- ifelse(is.na(disp$VSSTRESU[r]), "", disp$VSSTRESU[r])
  line <- sprintf("%-4d %-20s %-5s %-8s %-18s %-10s %-8s %-8s %-10s",
    disp$Obs[r], usubjid_s, vsseq_s, visitnum_s, visit_s, vsdtc_s, vstestcd_s, vsstresn_s, vsstresu_s)
  txt_lines <- c(txt_lines, line)
}
writeLines(txt_lines, file.path(out, "P018.txt"))
writeLines(txt_lines, file.path(out, "P018.lst"))

cat("P018 done: vs rows =", nrow(vs_final), " audit rows =", audit_n, "\n")

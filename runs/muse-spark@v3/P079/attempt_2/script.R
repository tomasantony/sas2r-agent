# P079_l_sae.sas -> R translation
# Listing 16.2.7.2 - Listing of Serious Adverse Events
# Inputs: adam.adae, adam.adsl  Output: l_sae (csv + txt)

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

resolve_path <- function(p) {
  if (file.exists(p) || dir.exists(p)) return(p)
  p2 <- sub("^D:/meta_hackathon/sas2r-agent", "/bench", p)
  if (file.exists(p2) || dir.exists(p2)) return(p2)
  p3 <- sub("^D:/meta_hackathon/sas2r-agent", "/bench", gsub("\\\\", "/", p))
  if (file.exists(p3) || dir.exists(p3)) return(p3)
  return(p)
}

adam_dir <- resolve_path("D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam")
if (!dir.exists(adam_dir) && dir.exists("/bench/data/raw/own-synthetic/adam")) {
  adam_dir <- "/bench/data/raw/own-synthetic/adam"
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_strip <- function(x) {
  ifelse(is.na(x), "", trimws(as.character(x), which = "both"))
}

get_ext <- function(f) {
  parts <- strsplit(basename(f), ".", fixed = TRUE)[[1]]
  if (length(parts) <= 1) return("")
  tolower(parts[length(parts)])
}
base_noext <- function(f) {
  b <- basename(f)
  sub("\\.[^.]*$", "", b)
}

read_adam <- function(libdir, memname) {
  files <- list.files(libdir, full.names = TRUE)
  low <- tolower(memname)
  cand <- files[tolower(base_noext(files)) == low]
  if (length(cand) == 0) stop(paste0("Dataset not found: ", memname, " in ", libdir))
  exts <- sapply(cand, get_ext)
  ord <- order(match(exts, c("sas7bdat", "xpt", "sas7bdat")))
  cand <- cand[ord]
  f <- cand[1]
  ext <- get_ext(f)
  if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else {
    df <- haven::read_sas(f)
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled") || inherits(col, "haven_labelled_spss")) {
      col <- haven::zap_labels(col)
      col <- haven::zap_formats(col)
      col <- haven::zap_label(col)
    }
    if (inherits(col, "labelled")) {
      col <- haven::zap_labels(col)
    }
    if (is.factor(col)) col <- as.character(col)
    df[[nm]] <- col
  }
  names(df) <- toupper(names(df))
  df
}

cat("Reading ADAE / ADSL ...\n")
cat(paste0("adam_dir=", adam_dir, "\n"))
adae_raw <- read_adam(adam_dir, "adae")
adsl_raw <- read_adam(adam_dir, "adsl")

need_adae <- c("USUBJID","AEBODSYS","AEDECOD","AETERM","AESTDTC","AEENDTC",
               "AETOXGR","AEREL","AEACN","AEOUT",
               "AESDTH","AESLIFE","AESHOSP","AESDISAB","AESCONG","AESMIE","AESER")
need_adsl <- c("USUBJID","TRTA","AGE","SEX","RACE","TRTSDT","SAFFL")
missing_a <- setdiff(need_adae, names(adae_raw))
missing_b <- setdiff(need_adsl, names(adsl_raw))
if (length(missing_a) > 0) stop(paste("ADAE missing:", paste(missing_a, collapse=",")))
if (length(missing_b) > 0) stop(paste("ADSL missing:", paste(missing_b, collapse=",")))

adae_keep <- adae_raw[, need_adae, drop = FALSE]
adsl_keep <- adsl_raw[, need_adsl, drop = FALSE]

to_char <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.character(x))
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x
}
for (nm in c("USUBJID","AEBODSYS","AEDECOD","AETERM","AESTDTC","AEENDTC",
             "AETOXGR","AEREL","AEACN","AEOUT",
             "AESDTH","AESLIFE","AESHOSP","AESDISAB","AESCONG","AESMIE","AESER")) {
  adae_keep[[nm]] <- to_char(adae_keep[[nm]])
}
for (nm in c("USUBJID","TRTA","SEX","RACE","SAFFL")) {
  adsl_keep[[nm]] <- to_char(adsl_keep[[nm]])
}

sas_to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1960-01-01"))
  }
  xc <- as.character(x)
  suppressWarnings(as.Date(substr(xc, 1, 10), format = "%Y-%m-%d"))
}
adsl_keep$TRT_START <- sas_to_date(adsl_keep$TRTSDT)
adsl_keep$AGE <- suppressWarnings(as.numeric(as.character(adsl_keep$AGE)))

adsl_f <- adsl_keep[adsl_keep$SAFFL == "Y" & !is.na(adsl_keep$SAFFL), , drop = FALSE]
adae_f <- adae_keep[adae_keep$AESER == "Y" & !is.na(adae_keep$AESER), , drop = FALSE]

sae_list <- merge(adae_f, adsl_f, by = "USUBJID", all = FALSE, sort = FALSE)
stopifnot(all(c("SAFFL","AESER","TRTA","USUBJID","AESTDTC") %in% names(sae_list)))

sae_list$SOC <- sae_list$AEBODSYS
sae_list$PT <- sae_list$AEDECOD
sae_list$VERBATIM <- sae_list$AETERM
sae_list$START_DATE <- sae_list$AESTDTC
sae_list$END_DATE <- sae_list$AEENDTC
sae_list$GRADE <- sae_list$AETOXGR
sae_list$RELATIONSHIP <- sae_list$AEREL
sae_list$ACTION_TAKEN <- sae_list$AEACN
sae_list$OUTCOME <- sae_list$AEOUT

# Order by TRTA, USUBJID, AESTDTC (missing first; SAS uppercase before lowercase)
o_TRTA <- ifelse(is.na(sae_list$TRTA), "", sae_list$TRTA)
o_USUB <- ifelse(is.na(sae_list$USUBJID), "", sae_list$USUBJID)
o_DT <- ifelse(is.na(sae_list$START_DATE), "", sae_list$START_DATE)
idx <- order(o_TRTA, o_USUB, o_DT, na.last = FALSE, method = "radix")
sae_list <- sae_list[idx, , drop = FALSE]

n <- nrow(sae_list)
sae_criteria <- character(n)
onset_day <- character(n)
demog <- character(n)

sas_put_best <- function(x) {
  if (is.na(x)) return("")
  if (is.finite(x) && x == floor(x) && abs(x) < 1e12) {
    return(trimws(sprintf("%.0f", round_half_up(x, 0))))
  } else {
    s <- format(x, scientific = FALSE, trim = TRUE, digits = 12)
    return(trimws(s))
  }
}

for (i in seq_len(n)) {
  parts <- c()
  if (!is.na(sae_list$AESDTH[i]) && sae_list$AESDTH[i] == "Y") parts <- c(parts, "Death")
  if (!is.na(sae_list$AESLIFE[i]) && sae_list$AESLIFE[i] == "Y") parts <- c(parts, "Life-threatening")
  if (!is.na(sae_list$AESHOSP[i]) && sae_list$AESHOSP[i] == "Y") parts <- c(parts, "Hospitalization")
  if (!is.na(sae_list$AESDISAB[i]) && sae_list$AESDISAB[i] == "Y") parts <- c(parts, "Disability")
  if (!is.na(sae_list$AESCONG[i]) && sae_list$AESCONG[i] == "Y") parts <- c(parts, "Congenital anomaly")
  if (!is.na(sae_list$AESMIE[i]) && sae_list$AESMIE[i] == "Y") parts <- c(parts, "Medically important")
  sae_criteria[i] <- paste(parts, collapse = ", ")

  trt_s <- sae_list$TRT_START[i]
  sd_raw <- sae_list$START_DATE[i]
  aest_d <- suppressWarnings(as.Date(substr(ifelse(is.na(sd_raw), "", as.character(sd_raw)), 1, 10), format = "%Y-%m-%d"))
  if (!is.na(trt_s) && !is.na(aest_d)) {
    onset_val <- as.numeric(aest_d - trt_s) + 1
    onset_day[i] <- trimws(sprintf("%8.0f", round_half_up(onset_val, 0)))
  } else {
    onset_day[i] <- ""
  }

  age_s <- sas_put_best(sae_list$AGE[i])
  sex_s <- sas_strip(sae_list$SEX[i])
  race_raw <- sas_strip(sae_list$RACE[i])
  race1 <- ifelse(race_raw == "", "", substr(race_raw, 1, 1))
  dparts <- c(age_s, sex_s, race1)
  dparts <- dparts[dparts != ""]
  if (length(dparts) == 0) demog[i] <- "" else demog[i] <- paste(dparts, collapse = "/")
}

sae_report <- data.frame(
  TRTA = as.character(sae_list$TRTA),
  USUBJID = as.character(sae_list$USUBJID),
  demog = demog,
  SOC = as.character(ifelse(is.na(sae_list$SOC), "", sae_list$SOC)),
  PT = as.character(ifelse(is.na(sae_list$PT), "", sae_list$PT)),
  START_DATE = as.character(ifelse(is.na(sae_list$START_DATE), "", sae_list$START_DATE)),
  END_DATE = as.character(ifelse(is.na(sae_list$END_DATE), "", sae_list$END_DATE)),
  onset_day = onset_day,
  GRADE = as.character(ifelse(is.na(sae_list$GRADE), "", sae_list$GRADE)),
  RELATIONSHIP = as.character(ifelse(is.na(sae_list$RELATIONSHIP), "", sae_list$RELATIONSHIP)),
  ACTION_TAKEN = as.character(ifelse(is.na(sae_list$ACTION_TAKEN), "", sae_list$ACTION_TAKEN)),
  OUTCOME = as.character(ifelse(is.na(sae_list$OUTCOME), "", sae_list$OUTCOME)),
  sae_criteria = sae_criteria,
  stringsAsFactors = FALSE
)

o1 <- ifelse(is.na(sae_report$TRTA), "", sae_report$TRTA)
o2 <- ifelse(is.na(sae_report$USUBJID), "", sae_report$USUBJID)
o3 <- ifelse(is.na(sae_report$START_DATE), "", sae_report$START_DATE)
sae_report <- sae_report[order(o1, o2, o3, na.last = FALSE, method = "radix"), , drop = FALSE]

cat(paste0("SAE rows: ", nrow(sae_report), "\n"))

write.csv(sae_report, file.path(out, "l_sae.csv"), row.names = FALSE, na = "")

sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
t4 <- "Listing 16.2.7.2"
t5 <- "Listing of Serious Adverse Events"
t6 <- "Safety Analysis Set"
f1 <- paste0("Data cutoff: ", cutdate, ".")
f2 <- "Source: ADaM datasets"
runstamp <- format(Sys.Date(), "%d%b%Y")
f3 <- paste0("Program: P079_l_sae.sas | Run: ", toupper(runstamp))

headers <- c("Treatment", "Subject ID", "Age/Sex/ Race", "SOC", "Preferred Term",
             "Start Date", "End Date", "Study Day", "Grade", "Relation- ship",
             "Action Taken", "Outcome", "SAE Criteria")
widths <- c(12, 12, 10, 20, 16, 11, 11, 7, 7, 12, 14, 14, 22)

fmt_row <- function(vals, widths) {
  s <- mapply(function(v, w) {
    v <- ifelse(is.na(v), "", as.character(v))
    v <- gsub("\n", " ", v, fixed = TRUE)
    if (nchar(v, type = "bytes") > w) v <- substr(v, 1, w)
    sprintf(paste0("%-", w, "s"), v)
  }, vals, widths, USE.NAMES = FALSE)
  paste(s, collapse = " | ")
}

header_line <- fmt_row(headers, widths)
sep_line <- paste(mapply(function(w) paste(rep("-", w), collapse = ""), widths), collapse = "-+-")

trt_levels <- unique(sae_report$TRTA)
if (length(trt_levels) == 0) trt_levels <- character(0)
total_pages <- max(1, length(trt_levels))

txt_path <- file.path(out, "l_sae.txt")
con <- file(txt_path, open = "wt")

write_page_header <- function(page_no, total_pages) {
  cat(paste0(sponsor, "    Page ", page_no, " of ", total_pages, "\n"), file = con)
  cat(paste0("Protocol: ", protocol, "\n"), file = con)
  cat(" \n", file = con)
  cat(paste0("              ", t4, "\n"), file = con)
  cat(paste0("              ", t5, "\n"), file = con)
  cat(paste0("              ", t6, "\n"), file = con)
  cat("\n", file = con)
}
write_page_footer <- function() {
  cat("\n", file = con)
  cat(paste0(f1, "\n"), file = con)
  cat(paste0(f2, "\n"), file = con)
  cat(paste0(f3, "\n"), file = con)
}

if (nrow(sae_report) == 0) {
  write_page_header(1, 1)
  cat(header_line, "\n", file = con, sep = "")
  cat(sep_line, "\n", file = con, sep = "")
  cat("No serious adverse events reported.\n", file = con)
  write_page_footer()
} else {
  pg <- 0
  for (trt in trt_levels) {
    pg <- pg + 1
    write_page_header(pg, total_pages)
    cat(header_line, "\n", file = con, sep = "")
    cat(sep_line, "\n", file = con, sep = "")
    sub <- sae_report[sae_report$TRTA == trt | (is.na(sae_report$TRTA) & is.na(trt)), , drop = FALSE]
    for (r in seq_len(nrow(sub))) {
      vals <- c(sub$TRTA[r], sub$USUBJID[r], sub$demog[r], sub$SOC[r], sub$PT[r],
                sub$START_DATE[r], sub$END_DATE[r], sub$onset_day[r],
                sub$GRADE[r], sub$RELATIONSHIP[r], sub$ACTION_TAKEN[r],
                sub$OUTCOME[r], sub$sae_criteria[r])
      cat(fmt_row(vals, widths), "\n", file = con, sep = "")
    }
    write_page_footer()
    if (pg < total_pages) cat("\n\f\n", file = con)
  }
}
close(con)
cat("Done.\n")

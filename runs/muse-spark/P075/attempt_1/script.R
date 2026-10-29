# P075 t_vs.sas -> R translation
# Table 14.3.5 Summary of Vital Signs by Visit
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
})

cat("NOTE: P075 t_vs starting\n")

round_half_away <- function(x, digits = 0) {
  mult <- 10 ^ digits
  res <- sign(x) * floor(abs(x) * mult + 0.5 + 1e-9) / mult
  res[is.na(x)] <- NA_real_
  # clean -0
  res[res == 0 & !is.na(res)] <- 0
  res
}
sas_fmt <- function(x, digits) {
  vapply(x, function(v) {
    if (is.na(v)) return(".")
    r <- round_half_away(v, digits)
    formatC(r, format = "f", digits = digits)
  }, character(1), USE.NAMES = FALSE)
}
strip_int <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(".")
    as.character(as.integer(v))
  }, character(1), USE.NAMES = FALSE)
}

read_sas_upper <- function(path) {
  df <- haven::read_sas(path)
  # zap labels
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    cl <- class(df[[nm]])
    if (any(cl %in% c("haven_labelled", "haven_labelled_spss"))) {
      df[[nm]] <- haven::zap_labels(df[[nm]])
      # haven labelled may still be class labelled; coerce
      if (inherits(df[[nm]], "haven_labelled")) df[[nm]] <- as.vector(df[[nm]])
    }
  }
  names(df) <- toupper(names(df))
  tibble::as_tibble(df)
}

adsl_path <- file.path(adam_dir, "adsl.sas7bdat")
advs_path <- file.path(adam_dir, "advs.sas7bdat")
# fallback: try .xpt or different case if not found
find_adam <- function(base, name) {
  cand1 <- file.path(base, paste0(name, ".sas7bdat"))
  if (file.exists(cand1)) return(cand1)
  cand2 <- file.path(base, paste0(name, ".xpt"))
  if (file.exists(cand2)) return(cand2)
  # list files case-insensitive
  fl <- list.files(base, full.names = TRUE)
  idx <- grep(paste0("^", name, "\\."), basename(fl), ignore.case = TRUE)
  if (length(idx) > 0) return(fl[idx[1]])
  return(cand1)
}
adsl_path <- find_adam(adam_dir, "adsl")
advs_path <- find_adam(adam_dir, "advs")

adsl <- read_sas_upper(adsl_path)
advs <- read_sas_upper(advs_path)

# Step 1: Big N (SAFFL='Y') by TRTA, SAS order by TRTA (character, uppercase first)
# SAS: where upcase(SAFFL)='Y' for m_counts, but main filter is b.SAFFL='Y' case-sensitive
bign <- adsl %>%
  filter(!is.na(SAFFL) & toupper(SAFFL) == "Y") %>%
  distinct(USUBJID, TRTA) %>%
  group_by(TRTA) %>%
  summarise(BIGN = n(), .groups = "drop")
# SAS sort order: binary/ASCII (uppercase before lowercase). Use C locale ordering via xtfrm in C? approximate with order by raw bytes
# Implement byte-order sort:
byte_order <- function(x) {
  # order by UTF-8 bytes
  order(vapply(x, function(s) paste(sprintf("%03d", utf8ToInt(s)), collapse="-"), character(1)))
}
if (nrow(bign) > 0) {
  bign <- bign %>% arrange(TRTA)
  # enforce SAS-like byte order explicitly
  ord <- order(sapply(bign$TRTA, function(s) paste(sprintf("%03d", utf8ToInt(as.character(s))), collapse=":")))
  bign <- bign[ord, , drop = FALSE]
}
bigntot <- adsl %>% filter(!is.na(SAFFL) & toupper(SAFFL) == "Y") %>% distinct(USUBJID) %>% nrow()
cat("NOTE: Big N computed\n")
print(bign)

# Step 2: Prepare analysis data
# select from advs inner join adsl on USUBJID where b.SAFFL='Y' (case-sensitive) and PARAMCD in list and ANL01FL='Y'
param_list <- c("SYSBP", "DIABP", "PULSE", "TEMP", "RESP", "WEIGHT")
# safety subjects
saff_subj <- adsl %>% filter(!is.na(SAFFL) & SAFFL == "Y") %>% distinct(USUBJID)
vs <- advs %>%
  inner_join(saff_subj, by = "USUBJID") %>%
  filter(!is.na(PARAMCD) & PARAMCD %in% param_list,
         !is.na(ANL01FL) & ANL01FL == "Y") %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, PARAMN, AVISIT, AVISITN, AVAL, BASE, CHG, ABLFL, ANL01FL) %>%
  arrange(PARAMN, AVISITN, TRTAN)

# Step 3: Descriptive stats by parameter, visit, treatment for AVAL
# PROC MEANS nway: groups present in data
grp_vars <- c("PARAMCD", "PARAM", "PARAMN", "AVISIT", "AVISITN", "TRTA", "TRTAN")

summ_one <- function(x) {
  n <- sum(!is.na(x))
  if (n == 0) {
    c(n = 0, mean = NA_real_, std = NA_real_, median = NA_real_, min = NA_real_, max = NA_real_)
  } else {
    xv <- x[!is.na(x)]
    c(n = n,
      mean = mean(xv),
      std = if (n > 1) stats::sd(xv) else NA_real_,
      median = stats::median(xv),
      min = min(xv),
      max = max(xv))
  }
}

actual_list <- vs %>%
  group_by(across(all_of(grp_vars))) %>%
  group_modify(~ {
    s <- summ_one(.x$AVAL)
    tibble(N = s[["n"]], MEAN = s[["mean"]], STD = s[["std"]],
           MEDIAN = s[["median"]], MIN = s[["min"]], MAX = s[["max"]])
  }) %>% ungroup()

# Change stats: where CHG ne . and ABLFL ne 'Y' (SAS missing ne 'Y' is true)
vs_chg <- vs %>% filter(!is.na(CHG) & (is.na(ABLFL) | ABLFL != "Y"))
change_list <- vs_chg %>%
  group_by(across(all_of(grp_vars))) %>%
  group_modify(~ {
    s <- summ_one(.x$CHG)
    tibble(N_CHG = s[["n"]], MEAN_CHG = s[["mean"]], STD_CHG = s[["std"]],
           MEDIAN_CHG = s[["median"]], MIN_CHG = s[["min"]], MAX_CHG = s[["max"]])
  }) %>% ungroup()

# Step 4: Merge actual and change
# SAS: left join on PARAMCD, AVISITN, TRTAN only
stats <- actual_list %>%
  left_join(change_list %>% select(PARAMCD, AVISITN, TRTAN, N_CHG, MEAN_CHG, STD_CHG, MEDIAN_CHG, MIN_CHG, MAX_CHG),
            by = c("PARAMCD", "AVISITN", "TRTAN")) %>%
  arrange(PARAMN, AVISITN, TRTAN)

# Step 5: Format for display
report <- stats %>%
  mutate(
    STAT1 = strip_int(N),
    STAT2 = sas_fmt(MEAN, 1),
    STAT3 = sas_fmt(STD, 2),
    STAT4 = sas_fmt(MEDIAN, 1),
    STAT5 = paste0(sas_fmt(MIN, 1), ", ", sas_fmt(MAX, 1))
  )
# change stats: blank if N_CHG is NA or 0
report$CHG1 <- ""
report$CHG2 <- ""
report$CHG3 <- ""
report$CHG4 <- ""
report$CHG5 <- ""
has_chg <- !is.na(report$N_CHG) & report$N_CHG > 0
report$CHG1[has_chg] <- strip_int(report$N_CHG[has_chg])
report$CHG2[has_chg] <- sas_fmt(report$MEAN_CHG[has_chg], 2)
report$CHG3[has_chg] <- sas_fmt(report$STD_CHG[has_chg], 2)
report$CHG4[has_chg] <- sas_fmt(report$MEDIAN_CHG[has_chg], 2)
report$CHG5[has_chg] <- paste0(sas_fmt(report$MIN_CHG[has_chg], 1), ", ", sas_fmt(report$MAX_CHG[has_chg], 1))
# when no change row, ensure N_CHG etc blank handling: set N_CHG NA -> keep blank
report$CHG1[!has_chg] <- ""
report$CHG2[!has_chg] <- ""
report$CHG3[!has_chg] <- ""
report$CHG4[!has_chg] <- ""
report$CHG5[!has_chg] <- ""

# Ensure order
report <- report %>% arrange(PARAMN, AVISITN, TRTAN)

# Build display table for CSV (table body)
table_body <- report %>%
  transmute(
    PARAMCD, PARAM, PARAMN, AVISIT, AVISITN, TRTA, TRTAN,
    ACT_N = STAT1, ACT_MEAN = STAT2, ACT_SD = STAT3, ACT_MEDIAN = STAT4, ACT_MINMAX = STAT5,
    CHG_N = CHG1, CHG_MEAN = CHG2, CHG_SD = CHG3, CHG_MEDIAN = CHG4, CHG_MINMAX = CHG5,
    N = N, MEAN = MEAN, STD = STD, MEDIAN = MEDIAN, MIN = MIN, MAX = MAX,
    N_CHG, MEAN_CHG, STD_CHG, MEDIAN_CHG, MIN_CHG, MAX_CHG
  )

write.csv(table_body, file.path(out, "t_vs.csv"), row.names = FALSE, na = "")

# Text rendering mimicking PROC REPORT with break after PARAM / page
# Titles
t1 <- "Table 14.3.5"
t2 <- "Summary of Vital Signs by Visit"
t3 <- "Safety Analysis Set"
# Header Big N line
hdr_bign <- paste0("Treatment groups (Safety): ",
                   paste(paste0(bign$TRTA, " (N=", bign$BIGN, ")"), collapse = " | "),
                   paste0(" | Total (N=", bigntot, ")"))

con_file <- file.path(out, "t_vs.txt")
sink_con <- file(con_file, open = "wt")
sink(sink_con)
cat(t1, "\n")
cat(t2, "\n")
cat(t3, "\n")
cat(hdr_bign, "\n")
cat("Population: Safety Analysis Set (SAFFL='Y'); Analysis records: ANL01FL='Y'\n")
cat("\n")

# Column headers
col_hdr1 <- sprintf("%-28s %-18s %-18s | %-5s %-8s %-8s %-8s %-16s | %-5s %-8s %-8s %-8s %-16s",
                    "Parameter", "Visit", "Treatment",
                    "n", "Mean", "SD", "Median", "Min, Max",
                    "n", "Mean", "SD", "Median", "Min, Max")
col_hdr2 <- sprintf("%-28s %-18s %-18s | %-5s %-8s %-8s %-8s %-16s | %-5s %-8s %-8s %-8s %-16s",
                    "", "", "",
                    "", "", "", "", "",
                    "", "", "", "", "")
cat(col_hdr1, "\n")
cat(paste(rep("-", nchar(col_hdr1)), collapse = ""), "\n")

params_ordered <- unique(report$PARAM)
for (p in params_ordered) {
  sub <- report %>% filter(PARAM == p) %>% arrange(AVISITN, TRTAN)
  pcd <- unique(sub$PARAMCD)
  cat(paste0("Parameter: ", p, " (", paste(pcd, collapse = ","), ")"), "\n")
  for (i in seq_len(nrow(sub))) {
    r <- sub[i, ]
    line <- sprintf("%-28s %-18s %-18s | %-5s %-8s %-8s %-8s %-16s | %-5s %-8s %-8s %-8s %-16s",
                    substr(as.character(r$PARAM), 1, 28),
                    substr(as.character(r$AVISIT), 1, 18),
                    substr(as.character(r$TRTA), 1, 18),
                    as.character(r$STAT1),
                    as.character(r$STAT2),
                    as.character(r$STAT3),
                    as.character(r$STAT4),
                    as.character(r$STAT5),
                    as.character(r$CHG1),
                    as.character(r$CHG2),
                    as.character(r$CHG3),
                    as.character(r$CHG4),
                    as.character(r$CHG5))
    cat(line, "\n")
  }
  cat("\n")
  cat("--- Page Break ---\n")
  cat("\n")
}
cat("\n")
cat("Notes: Actual Value and Change from Baseline summarized by visit (ANL01FL='Y').\n")
cat("Change from Baseline excludes baseline records (ABLFL='Y').\n")
cat("n = number of subjects with non-missing value; SD = standard deviation.\n")
cat("Source: ADaM datasets (ADVS, ADSL). Data cutoff: [DDMONYYYY].\n")
sink()
close(sink_con)

cat("NOTE: t_vs completed successfully.\n")

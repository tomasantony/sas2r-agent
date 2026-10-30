# P069 t_ae_sum.sas -> R translation
# Table 14.3.1 Overall Summary of Treatment-Emergent Adverse Events
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adampath <- "/bench/data/raw/own-synthetic/adam"
sdtmpath <- "/bench/data/raw/own-synthetic/sdtm"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_1dec <- function(x) {
  r <- round_half_up(x, 1)
  sprintf("%.1f", r)
}
sas_sort_key <- function(v) {
  sapply(as.character(v), function(s) {
    if (is.na(s)) return("")
    paste(sprintf("%03d", utf8ToInt(s)), collapse = "-")
  }, USE.NAMES = FALSE)
}
sas_order <- function(...) {
  args <- list(...)
  keys <- lapply(args, function(v) {
    if (is.character(v)) sas_sort_key(v) else v
  })
  do.call(order, c(keys, list(na.last = FALSE, method = "radix")))
}

find_file <- function(dir, base) {
  if (!dir.exists(dir)) return(NULL)
  fs <- list.files(dir, full.names = TRUE, recursive = FALSE)
  if (length(fs) == 0) return(NULL)
  low <- tolower(fs)
  hit <- grep(paste0(tolower(base), ".*(\\.sas7bdat|\\.xpt|\\.sas7bdat\\.gz|\\.csv)$"), low, value = FALSE)
  # fallback: any file containing base
  if (length(hit) == 0) hit <- grep(tolower(base), low, value = FALSE)
  if (length(hit) == 0) return(NULL)
  fs[hit[1]]
}
read_sas_any <- function(f) {
  if (is.null(f) || is.na(f) || !file.exists(f)) stop(paste0("Input not found: ", f))
  lf <- tolower(f)
  if (grepl("\\.sas7bdat$", lf)) df <- haven::read_sas(f)
  else if (grepl("\\.xpt$", lf)) df <- haven::read_xpt(f)
  else if (grepl("\\.csv$", lf)) df <- readr::read_csv(f, show_col_types = FALSE)
  else stop(paste0("Unknown extension: ", f))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # convert haven_labelled to character via factor labels
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled") || inherits(x, "haven_labelled_spss")) {
      df[[nm]] <- as.character(haven::as_factor(x, levels = "labels"))
    } else if (is.factor(x)) {
      df[[nm]] <- as.character(x)
    }
  }
  names(df) <- toupper(names(df))
  df
}

fadsl <- find_file(adampath, "adsl")
fadae <- find_file(adampath, "adae")
# fallback to raw cdisc-pilot adam if synthetic missing
if (is.null(fadsl)) fadsl <- find_file("/bench/data/raw/cdisc-pilot/adam", "adsl")
if (is.null(fadae)) fadae <- find_file("/bench/data/raw/cdisc-pilot/adam", "adae")
cat("NOTE: ADSL =", fadsl, "\n")
cat("NOTE: ADAE =", fadae, "\n")

adsl <- read_sas_any(fadsl)
adae <- read_sas_any(fadae)
stopifnot(all(c("USUBJID","TRTA","SAFFL") %in% names(adsl)))
stopifnot(all(c("USUBJID","TRTA","TRTEMFL") %in% names(adae)))

# --- Step 1: Big N (m_counts) ---
adsl_saf <- adsl[toupper(adsl$SAFFL) == "Y" & !is.na(adsl$SAFFL), , drop = FALSE]
# distinct subjects per TRTA
bign_df <- aggregate(USUBJID ~ TRTA, data = transform(adsl_saf, USUBJID = USUBJID),
  FUN = function(x) length(unique(x)))
names(bign_df)[names(bign_df) == "USUBJID"] <- "BIGN"
bign_df <- bign_df[sas_order(bign_df$TRTA), , drop = FALSE]
rownames(bign_df) <- NULL
bigntot <- length(unique(adsl_saf$USUBJID))
cat("NOTE: Big N\n"); print(bign_df); cat("Total:", bigntot, "\n")

trt_sorted <- bign_df$TRTA
bign_sorted <- bign_df$BIGN

# identify display mapping Placebo / Low / High (as SAS rename does)
find_trt <- function(pat) {
  hit <- trt_sorted[grepl(pat, trt_sorted, fixed = FALSE)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}
# use case-sensitive like SAS
placebo_val <- trt_sorted[grepl("Placebo", trt_sorted)][1]
low_val <- trt_sorted[grepl("Low", trt_sorted)][1]
high_val <- trt_sorted[grepl("High", trt_sorted)][1]
if (is.na(placebo_val) || is.na(low_val) || is.na(high_val)) {
  # fallback to sorted order
  if (length(trt_sorted) >= 3) {
    if (is.na(placebo_val)) placebo_val <- trt_sorted[1]
    if (is.na(low_val)) low_val <- trt_sorted[2]
    if (is.na(high_val)) high_val <- trt_sorted[3]
  }
}
cat("NOTE: mapping col1=", placebo_val, " col2=", low_val, " col3=", high_val, "\n")

get_bign_for <- function(trt) {
  idx <- match(trt, bign_df$TRTA)
  if (is.na(idx)) return(0L)
  bign_df$BIGN[idx]
}

# --- Step 2: AE categories ---
# safety subjects
saf_subj <- unique(adsl_saf$USUBJID)
# TRTEMFL Y distinct for _ae0 subquery (all adae, SAS: where TRTEMFL='Y' no safety filter)
adae_te_all_subj <- unique(adae$USUBJID[!is.na(adae$TRTEMFL) & adae$TRTEMFL == "Y"])

# helper to count distinct subjects per TRTA for a filtered adae set joined to safety
count_by_trta <- function(d, ord, label) {
  if (nrow(d) == 0) {
    return(data.frame(TRTA = character(0), CNT = numeric(0),
      CATEGORY = character(0), ORD = numeric(0), stringsAsFactors = FALSE))
  }
  # keep only safety subjects and TRTEMFL Y already applied by caller
  agg <- aggregate(USUBJID ~ TRTA, data = d, FUN = function(x) length(unique(x)))
  names(agg)[names(agg) == "USUBJID"] <- "CNT"
  agg$CATEGORY <- label
  agg$ORD <- ord
  agg
}

# base TEAE safety set: inner join adae-safety, TRTEMFL Y
adae_saf_te <- adae[adae$USUBJID %in% saf_subj & !is.na(adae$TRTEMFL) & adae$TRTEMFL == "Y", , drop = FALSE]

rel_vals <- c("POSSIBLE", "PROBABLE", "DEFINITE", "RELATED")
aetox_num <- suppressWarnings(as.numeric(adae_saf_te$AETOXGR))

ae1 <- count_by_trta(adae_saf_te, 1, "Subjects with at least one TEAE")
ae2 <- count_by_trta(adae_saf_te[!is.na(adae_saf_te$AEREL) & adae_saf_te$AEREL %in% rel_vals, , drop = FALSE], 2, "  Related TEAE")
ae3sub <- adae_saf_te[!is.na(aetox_num) & aetox_num >= 3, , drop = FALSE]
ae3 <- count_by_trta(ae3sub, 3, "  Grade >=3 TEAE")
ae4 <- count_by_trta(adae_saf_te[!is.na(adae_saf_te$AESER) & adae_saf_te$AESER == "Y", , drop = FALSE], 4, "  Serious Adverse Event")
ae5 <- count_by_trta(adae_saf_te[!is.na(adae_saf_te$AESER) & adae_saf_te$AESER == "Y" & !is.na(adae_saf_te$AEREL) & adae_saf_te$AEREL %in% rel_vals, , drop = FALSE], 5, "  Related Serious Adverse Event")
ae6 <- count_by_trta(adae_saf_te[!is.na(adae_saf_te$AEACN) & adae_saf_te$AEACN == "DRUG WITHDRAWN", , drop = FALSE], 6, "  TEAE Leading to Study Drug Discontinuation")
ae7 <- count_by_trta(adae_saf_te[!is.na(adae_saf_te$AEACN) & adae_saf_te$AEACN %in% c("DOSE REDUCED","DRUG INTERRUPTED","DOSE INCREASED"), , drop = FALSE], 7, "  TEAE Leading to Dose Modification")
ae8 <- count_by_trta(adae_saf_te[!is.na(adae_saf_te$AEOUT) & adae_saf_te$AEOUT == "FATAL", , drop = FALSE], 8, "  TEAE with Outcome of Death")

# --- Step 3: no TEAE ---
no_te_subj <- setdiff(saf_subj, adae_te_all_subj)
adsl_no_te <- adsl_saf[adsl_saf$USUBJID %in% no_te_subj, , drop = FALSE]
if (nrow(adsl_no_te) > 0) {
  ae0 <- aggregate(USUBJID ~ TRTA, data = adsl_no_te, FUN = function(x) length(unique(x)))
  names(ae0)[names(ae0) == "USUBJID"] <- "CNT"
  ae0$CATEGORY <- "Subjects with no TEAE"
  ae0$ORD <- 0
} else {
  ae0 <- data.frame(TRTA = character(0), CNT = numeric(0), CATEGORY = character(0), ORD = numeric(0), stringsAsFactors = FALSE)
}

all_ae <- dplyr::bind_rows(ae0, ae1, ae2, ae3, ae4, ae5, ae6, ae7, ae8)
# ensure types
all_ae$TRTA <- as.character(all_ae$TRTA)
all_ae$CATEGORY <- as.character(all_ae$CATEGORY)

# --- Step 5: percentages ---
all_ae2 <- dplyr::left_join(all_ae, bign_df, by = "TRTA")
all_ae2$PCT <- ifelse(!is.na(all_ae2$BIGN) & all_ae2$BIGN > 0, all_ae2$CNT / all_ae2$BIGN * 100, NA_real_)

all_ae2$VALUE <- ifelse(!is.na(all_ae2$CNT) & all_ae2$CNT > 0,
  paste0(trimws(as.character(all_ae2$CNT)), " (", trimws(fmt_1dec(all_ae2$PCT)), ")"),
  "0")

# --- Step 6: Transpose (by ORD CATEGORY, id TRTA) ---
# sort by ord category (SAS sort)
all_ae2 <- all_ae2[sas_order(all_ae2$ORD, all_ae2$CATEGORY), , drop = FALSE]
# pivot wider
wide <- tidyr::pivot_wider(all_ae2[, c("ORD","CATEGORY","TRTA","VALUE")],
  names_from = "TRTA", values_from = "VALUE", values_fill = "0")
# Ensure columns for the three display treatments exist
for (tv in c(placebo_val, low_val, high_val)) {
  if (!is.na(tv) && !(tv %in% names(wide))) {
    wide[[tv]] <- "0"
  }
}
# Build final with col1=placebo, col2=low, col3=high (as SAS rename)
final <- data.frame(ORD = wide$ORD, CATEGORY = wide$CATEGORY, stringsAsFactors = FALSE)
final$COL1 <- as.character(wide[[placebo_val]])
final$COL2 <- as.character(wide[[low_val]])
final$COL3 <- as.character(wide[[high_val]])
final$COL1[is.na(final$COL1)] <- "0"
final$COL2[is.na(final$COL2)] <- "0"
final$COL3[is.na(final$COL3)] <- "0"
# Ensure zero rows display (already) and order by ORD
final <- final[sas_order(final$ORD), , drop = FALSE]
rownames(final) <- NULL
# Ensure all ord 0-8 rows exist if missing entirely (SAS would omit, but keep observed)
# If ae0 empty, ord 0 row missing -> keep as is (matches SAS)

# Headers matching display columns (correct N per column)
hdr1 <- paste0(placebo_val, " (N=", get_bign_for(placebo_val), ")")
hdr2 <- paste0(low_val, " (N=", get_bign_for(low_val), ")")
hdr3 <- paste0(high_val, " (N=", get_bign_for(high_val), ")")

report <- final
names(report) <- c("ORD","CATEGORY","COL1","COL2","COL3")

# CSV of table body (with headers encoded as column labels? write body)
csv_body <- data.frame(Category = report$CATEGORY,
  COL1 = report$COL1, COL2 = report$COL2, COL3 = report$COL3,
  stringsAsFactors = FALSE)
names(csv_body) <- c("Category", hdr1, hdr2, hdr3)
write.csv(csv_body, file.path(out, "t_ae_sum.csv"), row.names = FALSE, na = "")

# --- Step 8: text rendering ---
# Titles/footnotes verbatim
t1 <- "[Sponsor Name]"
t2 <- "Protocol: [Protocol Number]"
t4 <- "Table 14.3.1"
t5 <- "Overall Summary of Treatment-Emergent Adverse Events"
t6 <- "Safety Analysis Set"
f1 <- "Data cutoff: [DDMONYYYY]"
f2 <- "Source: ADaM datasets"
f3 <- paste0("Program: t_ae_sum.sas | Run: ", format(Sys.Date(), "%d%b%Y"))
f4 <- "TEAE = Treatment-Emergent Adverse Event; SAE = Serious Adverse Event"
f5 <- "Subjects are counted once per category."

# Build fixed-width text
# Determine column widths
cat_w <- max(nchar(c("Category", report$CATEGORY)), na.rm = TRUE) + 2
c1_w <- max(nchar(c(hdr1, report$COL1)), na.rm = TRUE) + 2
c2_w <- max(nchar(c(hdr2, report$COL2)), na.rm = TRUE) + 2
c3_w <- max(nchar(c(hdr3, report$COL3)), na.rm = TRUE) + 2

pad_r <- function(s, w) sprintf(paste0("%-", w, "s"), s)
pad_c <- function(s, w) {
  s <- as.character(s); n <- nchar(s)
  l <- floor((w - n) / 2); r <- w - n - l
  paste0(strrep(" ", pmax(0, l)), s, strrep(" ", pmax(0, r)))
}

lines <- c()
lines <- c(lines, paste0(t1, strrep(" ", 4), "Page 1"))
lines <- c(lines, t2)
lines <- c(lines, "")
lines <- c(lines, t4)
lines <- c(lines, t5)
lines <- c(lines, t6)
lines <- c(lines, "")
hdr_line <- paste0(pad_r("Category", cat_w), " | ", pad_c(hdr1, c1_w), " | ", pad_c(hdr2, c2_w), " | ", pad_c(hdr3, c3_w))
lines <- c(lines, hdr_line)
lines <- c(lines, strrep("-", nchar(hdr_line)))
for (i in seq_len(nrow(report))) {
  lines <- c(lines, paste0(pad_r(report$CATEGORY[i], cat_w), " | ",
    pad_c(report$COL1[i], c1_w), " | ", pad_c(report$COL2[i], c2_w), " | ", pad_c(report$COL3[i], c3_w)))
}
lines <- c(lines, "")
lines <- c(lines, f1)
lines <- c(lines, f2)
lines <- c(lines, f3)
lines <- c(lines, f4)
lines <- c(lines, f5)

writeLines(lines, file.path(out, "t_ae_sum.txt"))
cat(paste(lines, collapse = "\n"), "\n")
cat("NOTE: t_ae_sum completed successfully.\n")

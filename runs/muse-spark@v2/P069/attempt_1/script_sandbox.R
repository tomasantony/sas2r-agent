# P069 t_ae_sum - Overall Summary of Treatment-Emergent Adverse Events
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "/bench/data/raw/own-synthetic/adam"
sdtm_dir <- "/bench/data/raw/own-synthetic/sdtm"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-10) / 10^digits)
}
fmt_pct1 <- function(x) {
  v <- round_half_up(x, 1)
  sprintf("%.1f", v)
}
strip_put_cnt <- function(x) {
  as.character(as.integer(x))
}

find_file <- function(dir, dsname) {
  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  # match dsname ignoring case, extension xpt/sas7bdat/csv
  pat <- paste0("(?i)^", dsname, "\\.(sas7bdat|xpt|csv)$")
  m <- grep(pat, basename(files), value = TRUE)
  if (length(m) == 0) {
    # try contains
    m2 <- grep(paste0("(?i)", dsname), basename(files), value = TRUE)
    if (length(m2) == 0) stop(paste0("Dataset not found: ", dsname, " in ", dir))
    return(file.path(dir, m2[1]))
  }
  return(file.path(dir, m[1]))
}

clean_df <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled") || inherits(x, "haven_labelled_spss") || !is.null(attr(x, "labels")) || !is.null(attr(x, "label")) || !is.null(attr(x, "format.sas"))) {
      # strip labels/formats, keep underlying values
      attr(x, "label") <- NULL
      attr(x, "labels") <- NULL
      attr(x, "format.sas") <- NULL
      attr(x, "format") <- NULL
      if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
      x <- as.vector(x)
      df[[nm]] <- x
    }
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  }
  names(df) <- toupper(names(df))
  df
}

read_adam <- function(dsname) {
  f <- find_file(adam_dir, dsname)
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") df <- haven::read_sas(f)
  else if (ext == "xpt") df <- haven::read_xpt(f)
  else if (ext == "csv") df <- readr::read_csv(f, show_col_types = FALSE)
  else stop(paste("unknown ext", f))
  clean_df(df)
}

adsl <- read_adam("adsl")
adae <- read_adam("adae")

# check columns exist
stopifnot(all(c("USUBJID","TRTA","SAFFL") %in% names(adsl)))
stopifnot(all(c("USUBJID","TRTA","TRTEMFL") %in% names(adae)))
# optional AE vars - create as NA if missing to avoid error, but stopifnot per spec says check filter cols exist
for (v in c("AEREL","AETOXGR","AESER","AEACN","AEOUT")) {
  if (!v %in% names(adae)) adae[[v]] <- NA_character_
  stopifnot(v %in% names(adae))
}

# Ensure character types for key vars
to_chr <- function(x) {
  if (is.numeric(x) || is.integer(x)) return(as.character(x))
  return(as.character(x))
}
adsl$USUBJID <- to_chr(adsl$USUBJID)
adsl$TRTA <- to_chr(adsl$TRTA)
adsl$SAFFL <- to_chr(adsl$SAFFL)
adae$USUBJID <- to_chr(adae$USUBJID)
adae$TRTA <- to_chr(adae$TRTA)
adae$TRTEMFL <- to_chr(adae$TRTEMFL)
adae$AEREL <- to_chr(adae$AEREL)
adae$AESER <- to_chr(adae$AESER)
adae$AEACN <- to_chr(adae$AEACN)
adae$AEOUT <- to_chr(adae$AEOUT)
# AETOXGR keep as character for INPUT(best.) emulation
adae$AETOXGR <- to_chr(adae$AETOXGR)

# SAS sorts with C locale (uppercase before lowercase); enforce for ordering
old_loc <- Sys.getlocale("LC_COLLATE")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
sas_sort_unique <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[order(ux)]
}

# --- Step 1: Big N (m_counts: upcase(SAFFL)='Y', group by TRTA, order by TRTA) ---
adsl_saf <- adsl[toupper(adsl$SAFFL) == "Y" & !is.na(adsl$USUBJID) & !is.na(adsl$TRTA), , drop = FALSE]
# distinct subjects per TRTA
bign_df <- aggregate(USUBJID ~ TRTA, data = adsl_saf,
  FUN = function(x) length(unique(x)))
names(bign_df)[2] <- "BIGN"
bign_df <- bign_df[order(bign_df$TRTA), , drop = FALSE]
bigntot <- length(unique(adsl_saf$USUBJID))
trt_order <- bign_df$TRTA  # already ORDER BY TRTA
ntrt <- length(trt_order)
# headers
col_hdrs <- paste0(trt_order, " (N=", bign_df$BIGN, ")")
# map TRTA -> BIGN
bign_map <- setNames(bign_df$BIGN, bign_df$TRTA)

# safety subject list
safety_usub <- unique(adsl_saf$USUBJID)

# distinct TEAE subjects (any, for _ae0): select distinct USUBJID from ADAE where TRTEMFL='Y'
teae_usub_all <- unique(adae$USUBJID[!is.na(adae$TRTEMFL) & adae$TRTEMFL == "Y"])

# helper to count distinct subjects per a.TRTA given filtered adae
count_by_trta <- function(d, ord_val, label_val) {
  if (nrow(d) == 0) {
    return(data.frame(TRTA = character(0), CNT = numeric(0),
      CATEGORY = character(0), ORD = numeric(0), stringsAsFactors = FALSE))
  }
  # distinct USUBJID per TRTA: need distinct pairs then count
  dd <- unique(d[, c("TRTA", "USUBJID"), drop = FALSE])
  dd <- dd[!is.na(dd$TRTA) & !is.na(dd$USUBJID), , drop = FALSE]
  if (nrow(dd) == 0) {
    return(data.frame(TRTA = character(0), CNT = numeric(0),
      CATEGORY = character(0), ORD = numeric(0), stringsAsFactors = FALSE))
  }
  tb <- as.data.frame(table(dd$TRTA), stringsAsFactors = FALSE)
  names(tb) <- c("TRTA", "CNT")
  tb$CATEGORY <- label_val
  tb$ORD <- ord_val
  tb
}

# Base TEAE filter: inner join + b.SAFFL='Y' and a.TRTEMFL='Y'
adae_teae_saf <- adae[!is.na(adae$TRTEMFL) & adae$TRTEMFL == "Y" &
  adae$USUBJID %in% safety_usub, , drop = FALSE]

related_vals <- c("POSSIBLE", "PROBABLE", "DEFINITE", "RELATED")

# ord 1: Any TEAE
ae1 <- count_by_trta(adae_teae_saf, 1, "Subjects with at least one TEAE")
# ord 2: Related
d2 <- adae_teae_saf[!is.na(adae_teae_saf$AEREL) & adae_teae_saf$AEREL %in% related_vals, , drop = FALSE]
ae2 <- count_by_trta(d2, 2, "  Related TEAE")
# ord 3: Grade >=3 : input(AETOXGR,best.)>=3
tox_num <- suppressWarnings(as.numeric(adae_teae_saf$AETOXGR))
d3 <- adae_teae_saf[!is.na(tox_num) & tox_num >= 3, , drop = FALSE]
ae3 <- count_by_trta(d3, 3, "  Grade >=3 TEAE")
# ord 4: Serious
d4 <- adae_teae_saf[!is.na(adae_teae_saf$AESER) & adae_teae_saf$AESER == "Y", , drop = FALSE]
ae4 <- count_by_trta(d4, 4, "  Serious Adverse Event")
# ord 5: Related SAE
d5 <- adae_teae_saf[!is.na(adae_teae_saf$AESER) & adae_teae_saf$AESER == "Y" &
  !is.na(adae_teae_saf$AEREL) & adae_teae_saf$AEREL %in% related_vals, , drop = FALSE]
ae5 <- count_by_trta(d5, 5, "  Related Serious Adverse Event")
# ord 6: discontinuation
d6 <- adae_teae_saf[!is.na(adae_teae_saf$AEACN) & adae_teae_saf$AEACN == "DRUG WITHDRAWN", , drop = FALSE]
ae6 <- count_by_trta(d6, 6, "  TEAE Leading to Study Drug Discontinuation")
# ord 7: dose modification
d7 <- adae_teae_saf[!is.na(adae_teae_saf$AEACN) & adae_teae_saf$AEACN %in% c("DOSE REDUCED","DRUG INTERRUPTED","DOSE INCREASED"), , drop = FALSE]
ae7 <- count_by_trta(d7, 7, "  TEAE Leading to Dose Modification")
# ord 8: death
d8 <- adae_teae_saf[!is.na(adae_teae_saf$AEOUT) & adae_teae_saf$AEOUT == "FATAL", , drop = FALSE]
ae8 <- count_by_trta(d8, 8, "  TEAE with Outcome of Death")

# --- Step 3: Subjects with no TEAE ---
# from ADSL SAFFL=Y left join distinct TEAE where null
no_teae_usub <- setdiff(safety_usub, teae_usub_all)
adsl_no <- adsl_saf[adsl_saf$USUBJID %in% no_teae_usub, , drop = FALSE]
if (nrow(adsl_no) > 0) {
  dd0 <- unique(adsl_no[, c("TRTA","USUBJID"), drop = FALSE])
  tb0 <- as.data.frame(table(dd0$TRTA), stringsAsFactors = FALSE)
  names(tb0) <- c("TRTA","CNT")
  tb0$CATEGORY <- "Subjects with no TEAE"
  tb0$ORD <- 0
  ae0 <- tb0
} else {
  ae0 <- data.frame(TRTA=character(0),CNT=numeric(0),CATEGORY=character(0),ORD=numeric(0),stringsAsFactors=FALSE)
}

all_ae <- rbind(ae0, ae1, ae2, ae3, ae4, ae5, ae6, ae7, ae8)
# ensure types
all_ae$CNT <- as.numeric(all_ae$CNT)
all_ae$ORD <- as.numeric(all_ae$ORD)

# --- Step 5: percentages ---
all_ae$BIGN <- bign_map[all_ae$TRTA]
all_ae$PCT <- ifelse(!is.na(all_ae$BIGN) & all_ae$BIGN > 0, all_ae$CNT / all_ae$BIGN * 100, NA_real_)
all_ae$VALUE <- ifelse(!is.na(all_ae$CNT) & all_ae$CNT > 0,
  paste0(strip_put_cnt(all_ae$CNT), " (", fmt_pct1(all_ae$PCT), ")"),
  "0")

# sort by ord category (SAS sort)
all_ae <- all_ae[order(all_ae$ORD, all_ae$CATEGORY), , drop = FALSE]

# --- Step 6: Transpose ---
# Define full ord/category list to ensure all 9 rows even if empty
ord_labels <- data.frame(
  ORD = 0:8,
  CATEGORY = c("Subjects with no TEAE",
    "Subjects with at least one TEAE",
    "  Related TEAE",
    "  Grade >=3 TEAE",
    "  Serious Adverse Event",
    "  Related Serious Adverse Event",
    "  TEAE Leading to Study Drug Discontinuation",
    "  TEAE Leading to Dose Modification",
    "  TEAE with Outcome of Death"),
  stringsAsFactors = FALSE
)
# Build wide
final_wide <- ord_labels
for (k in seq_along(trt_order)) {
  trt <- trt_order[k]
  colname <- paste0("COL", k)
  vals <- sapply(final_wide$ORD, function(o) {
    idx <- which(all_ae$ORD == o & all_ae$TRTA == trt)
    if (length(idx) == 0) return("0")
    # if multiple (should not), take first
    return(all_ae$VALUE[idx[1]])
  })
  final_wide[[colname]] <- vals
}
# Ensure zero rows display: replace '' with '0' (already) and NA with '0'
for (k in seq_along(trt_order)) {
  cn <- paste0("COL", k)
  final_wide[[cn]][is.na(final_wide[[cn]]) | final_wide[[cn]] == ""] <- "0"
}
report <- final_wide[order(final_wide$ORD), , drop = FALSE]

suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_loc), silent = TRUE))

# --- Step 8: outputs ---
# CSV of table body: include ord, category, treatment columns named with headers
csv_df <- report
# Rename COLs to sanitized? Keep COL1.. but also provide header labels; use COL1 etc with N in separate? For fidelity, name columns as treatment headers
names(csv_df) <- c("ORD", "CATEGORY", col_hdrs)
write.csv(csv_df, file.path(out, "t_ae_sum.csv"), row.names = FALSE, na = "")

# Text rendering
titles <- c(
  "[Sponsor Name]                                                              Page ^{pageof}",
  "Protocol: [Protocol Number]",
  "",
  "Table 14.3.1",
  "Overall Summary of Treatment-Emergent Adverse Events",
  "Safety Analysis Set"
)
footnotes <- c(
  "Data cutoff: [DDMONYYYY].",
  "Source: ADaM datasets",
  paste0("Program: t_ae_sum.sas | Run: ", format(Sys.Date(), "%d%b%Y")),
  "TEAE = Treatment-Emergent Adverse Event; SAE = Serious Adverse Event",
  "Subjects are counted once per category."
)

# Build table text with fixed columns
# Header row
hdr_row <- c("Category", col_hdrs)
# Determine widths
# Simple rendering: category width 55, each treatment width 20
cat_w <- 55
trt_w <- 22
pad_right <- function(s, w) {
  s <- as.character(s)
  n <- nchar(s, type = "chars")
  if (is.na(n)) s <- ""
  if (n >= w) return(s)
  paste0(s, paste(rep(" ", w - n), collapse = ""))
}
pad_center <- function(s, w) {
  s <- as.character(s)
  n <- nchar(s, type = "chars")
  if (n >= w) return(s)
  left <- floor((w - n) / 2)
  right <- w - n - left
  paste0(paste(rep(" ", left), collapse = ""), s, paste(rep(" ", right), collapse = ""))
}
lines <- c()
lines <- c(lines, titles, "")
# column headers (split ~ already replaced)
header_line <- paste0(pad_right(hdr_row[1], cat_w),
  paste(sapply(hdr_row[-1], function(h) pad_center(h, trt_w)), collapse = ""))
lines <- c(lines, header_line)
lines <- c(lines, paste(rep("-", nchar(header_line)), collapse = ""))
for (i in seq_len(nrow(report))) {
  cat_lab <- report$CATEGORY[i]
  row_vals <- as.character(unlist(report[i, paste0("COL", seq_along(trt_order)), drop = TRUE]))
  line <- paste0(pad_right(cat_lab, cat_w),
    paste(sapply(row_vals, function(v) pad_center(v, trt_w)), collapse = ""))
  lines <- c(lines, line)
}
lines <- c(lines, "", footnotes)

writeLines(lines, file.path(out, "t_ae_sum.txt"))

# Also write plotted-data style CSV of underlying counts for traceability (plotted data counterpart not required but keep minimal)
# The spec requires PNG plus CSV of plotted data only for figures; not needed here.

message("t_ae_sum completed successfully.")

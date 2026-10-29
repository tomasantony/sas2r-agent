```r
# P069 t_ae_sum - Overall Summary of Treatment-Emergent Adverse Events
# Translates P069_t_ae_sum.sas

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

cat("P069 t_ae_sum: starting\n")

find_file <- function(dir, name) {
  files <- list.files(dir, full.names = TRUE)
  # case-insensitive match on basename without extension
  idx <- which(tolower(tools::file_path_sans_ext(basename(files))) == tolower(name))
  if (length(idx) == 0) {
    # try pattern
    idx <- grep(paste0("(?i)^", name, "\\."), basename(files))
  }
  if (length(idx) == 0) stop(paste0("File not found: ", name, " in ", dir))
  files[idx[1]]
}

read_any <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "xpt") {
    df <- haven::read_xpt(path)
  } else if (ext %in% c("sas7bdat", "sas7bcat")) {
    df <- haven::read_sas(path)
  } else if (ext == "csv") {
    df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    return(as.data.frame(df, stringsAsFactors = FALSE))
  } else {
    # try sas then xpt
    tryCatch({ df <- haven::read_sas(path) },
             error = function(e) { df <<- haven::read_xpt(path) })
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # zap haven labels to underlying values
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled") || inherits(x, "haven_labelled_spss")) {
      x <- haven::zap_labels(x)
      x <- haven::zap_formats(x)
      # haven may return factor via as_factor; keep underlying
      if (is.factor(x)) x <- as.character(x)
      df[[nm]] <- x
    } else if (is.factor(x)) {
      df[[nm]] <- as.character(x)
    }
  }
  df
}

adsl_path <- find_file(adam_dir, "adsl")
adae_path <- find_file(adam_dir, "adae")
cat("Reading:", adsl_path, "\n")
cat("Reading:", adae_path, "\n")
adsl <- read_any(adsl_path)
adae <- read_any(adae_path)

# Ensure character columns are character, trim? SAS comparison is case-sensitive, no trim except exact.
# Keep values as-is (haven already trims? no). Ensure NA stays NA.
to_chr <- function(x) {
  if (is.character(x)) return(x)
  if (is.numeric(x) || is.integer(x)) return(as.character(x))
  as.character(x)
}

# Guarantee needed columns exist
for (c in c("USUBJID","TRTA","SAFFL")) if (!c %in% names(adsl)) stop(paste("ADSL missing", c))
for (c in c("USUBJID","TRTA","TRTEMFL","AEREL","AETOXGR","AESER","AEACN","AEOUT")) {
  if (!c %in% names(adae)) {
    # create as NA if missing
    adae[[c]] <- NA_character_
  }
}

adsl$USUBJID <- to_chr(adsl$USUBJID)
adsl$TRTA <- to_chr(adsl$TRTA)
adsl$SAFFL <- to_chr(adsl$SAFFL)
adae$USUBJID <- to_chr(adae$USUBJID)
adae$TRTA <- to_chr(adae$TRTA)
adae$TRTEMFL <- to_chr(adae$TRTEMFL)
adae$AEREL <- to_chr(adae$AEREL)
adae$AETOXGR <- to_chr(adae$AETOXGR)
adae$AESER <- to_chr(adae$AESER)
adae$AEACN <- to_chr(adae$AEACN)
adae$AEOUT <- to_chr(adae$AEOUT)

# --- Step 1: Get Big N (m_counts: upcase(SAFFL)='Y', group by TRTA, order by TRTA) ---
saffl_up <- toupper(adsl$SAFFL)
saffl_up[is.na(saffl_up)] <- ""
adsl_saf <- adsl[saffl_up == "Y", , drop = FALSE]

# distinct subjects per TRTA
bign_df <- adsl_saf %>%
  filter(!is.na(TRTA) & TRTA != "") %>%
  distinct(TRTA, USUBJID) %>%
  group_by(TRTA) %>%
  summarise(bign = n(), .groups = "drop") %>%
  arrange(TRTA)  # SAS character ordering: byte order; R arrange on ASCII similar for these values

# Total
bigntot <- n_distinct(adsl_saf$USUBJID)

cat("Big N:\n")
print(bign_df)
cat("Total:", bigntot, "\n")

# Identify treatment order for display: col1=Placebo, col2=Low, col3=High
# Detect actual TRTA values
trt_vals <- sort(unique(adsl_saf$TRTA))
cat("TRTA values:", paste(trt_vals, collapse=" | "), "\n")

find_trt <- function(pattern) {
  idx <- grep(pattern, trt_vals, ignore.case = TRUE)
  if (length(idx) == 0) return(NA_character_)
  # prefer value containing pattern; if multiple, take first sorted
  trt_vals[idx[1]]
}
placebo_val <- find_trt("placebo")
low_val <- find_trt("low")
high_val <- find_trt("high")

ordered_trt <- c()
if (!is.na(placebo_val)) ordered_trt <- c(ordered_trt, placebo_val)
if (!is.na(low_val)) ordered_trt <- c(ordered_trt, low_val)
if (!is.na(high_val)) ordered_trt <- c(ordered_trt, high_val)
# append any remaining not yet included in sorted order
for (v in trt_vals) if (!(v %in% ordered_trt)) ordered_trt <- c(ordered_trt, v)
# limit to 3 (ntrt=3); if more, keep all but report uses first 3 in this order
cat("Ordered TRTA:", paste(ordered_trt, collapse=" | "), "\n")

# Map bign to ordered
get_bign <- function(trt) {
  v <- bign_df$bign[bign_df$TRTA == trt]
  if (length(v) == 0) return(0L)
  v[1]
}
bigns_ord <- sapply(ordered_trt, get_bign)

# Headers like SAS: "TRTA (N=bign)"
hdrs <- paste0(ordered_trt, " (N=", bigns_ord, ")")
# col1..col3 headers
col1hdr <- ifelse(length(hdrs) >= 1, hdrs[1], "")
col2hdr <- ifelse(length(hdrs) >= 2, hdrs[2], "")
col3hdr <- ifelse(length(hdrs) >= 3, hdrs[3], "")

# --- Step 2: AE Categories helper ---
# SAS: inner join adae a to adsl b on USUBJID where b.SAFFL='Y' (case-sensitive) and a.TRTEMFL='Y' + extra, group by a.TRTA
adsl_saf_exact <- adsl[!is.na(adsl$SAFFL) & adsl$SAFFL == "Y", , drop = FALSE]
saf_subjects <- unique(adsl_saf_exact$USUBJID)

# Base TEAE records in safety population: need join - ADAE record whose USUBJID in safety AND TRTEMFL='Y'
# Note SAS joins on USUBJID only, then filters b.SAFFL='Y' and a.TRTEMFL='Y'. Equivalent to above.
is_teae <- !is.na(adae$TRTEMFL) & adae$TRTEMFL == "Y"
adae_teae_saf <- adae[adae$USUBJID %in% saf_subjects & is_teae, , drop = FALSE]

count_by_trta <- function(df) {
  if (nrow(df) == 0) return(data.frame(TRTA=character(0), cnt=integer(0), stringsAsFactors=FALSE))
  df %>% distinct(TRTA, USUBJID) %>%
    filter(!is.na(TRTA)) %>%
    group_by(TRTA) %>%
    summarise(cnt = n(), .groups="drop") %>%
    as.data.frame()
}

ae_category <- function(filter_expr, label, ord) {
  # filter_expr: function(df) returning logical vector
  df <- adae_teae_saf
  if (nrow(df) > 0) {
    keep <- filter_expr(df)
    keep[is.na(keep)] <- FALSE
    df <- df[keep, , drop=FALSE]
  }
  cbt <- count_by_trta(df)
  cbt$category <- label
  cbt$ord <- ord
  # ensure columns order TRTA, cnt, category, ord
  if (nrow(cbt)==0) cbt <- data.frame(TRTA=character(0), cnt=integer(0), category=character(0), ord=numeric(0), stringsAsFactors=FALSE)
  cbt[, c("TRTA","cnt","category","ord")]
}

related_vals <- c("POSSIBLE","PROBABLE","DEFINITE","RELATED")

ae1 <- ae_category(function(d) rep(TRUE, nrow(d)), "Subjects with at least one TEAE", 1)
ae2 <- ae_category(function(d) d$AEREL %in% related_vals, "  Related TEAE", 2)
ae3 <- ae_category(function(d) {
  v <- suppressWarnings(as.numeric(d$AETOXGR))
  !is.na(v) & v >= 3
}, "  Grade >=3 TEAE", 3)
ae4 <- ae_category(function(d) !is.na(d$AESER) & d$AESER == "Y", "  Serious Adverse Event", 4)
ae5 <- ae_category(function(d) (!is.na(d$AESER) & d$AESER=="Y") & (d$AEREL %in% related_vals), "  Related Serious Adverse Event", 5)
ae6 <- ae_category(function(d) !is.na(d$AEACN) & d$AEACN=="DRUG WITHDRAWN", "  TEAE Leading to Study Drug Discontinuation", 6)
ae7 <- ae_category(function(d) !is.na(d$AEACN) & d$AEACN %in% c("DOSE REDUCED","DRUG INTERRUPTED","DOSE INCREASED"), "  TEAE Leading to Dose Modification", 7)
ae8 <- ae_category(function(d) !is.na(d$AEOUT) & d$AEOUT=="FATAL", "  TEAE with Outcome of Death", 8)

# --- Step 3: Subjects with no TEAE ---
# SAS: from adsl b left join (select distinct USUBJID from adae where TRTEMFL='Y') a on USUBJID where b.SAFFL='Y' and a.USUBJID is null, group by b.TRTA
teae_subjects_all <- unique(adae$USUBJID[!is.na(adae$TRTEMFL) & adae$TRTEMFL=="Y"])
ae0_base <- adsl_saf_exact[!(adsl_saf_exact$USUBJID %in% teae_subjects_all), , drop=FALSE]
if (nrow(ae0_base) > 0) {
  ae0 <- ae0_base %>% distinct(TRTA, USUBJID) %>%
    filter(!is.na(TRTA)) %>%
    group_by(TRTA) %>% summarise(cnt=n(), .groups="drop") %>% as.data.frame()
  ae0$category <- "Subjects with no TEAE"
  ae0$ord <- 0
  ae0 <- ae0[, c("TRTA","cnt","category","ord")]
} else {
  ae0 <- data.frame(TRTA=character(0), cnt=integer(0), category=character(0), ord=numeric(0), stringsAsFactors=FALSE)
}

# --- Step 4: Combine ---
all_ae <- bind_rows(ae0, ae1, ae2, ae3, ae4, ae5, ae6, ae7, ae8)

# --- Step 5: percentages ---
# left join bign
bign_lookup <- setNames(bign_df$bign, bign_df$TRTA)
all_ae$bign <- sapply(all_ae$TRTA, function(t) if (t %in% names(bign_lookup)) bign_lookup[[t]] else NA_real_)
all_ae$pct <- ifelse(!is.na(all_ae$bign) & all_ae$bign > 0, all_ae$cnt / all_ae$bign * 100, NA_real_)

# SAS round half away from zero to 1 decimal for PUT 5.1
round_half_up_1 <- function(x) {
  # x non-negative
  floor(x * 10 + 0.5 + 1e-9) / 10
}
fmt_pct_5_1 <- function(p) {
  r <- round_half_up_1(p)
  # format with 1 decimal (SAS PUT 5.1 always shows 1 decimal)
  sprintf("%.1f", r)
}

all_ae$value <- ifelse(all_ae$cnt > 0,
                       paste0(trimws(as.character(all_ae$cnt)), " (", trimws(fmt_pct_5_1(all_ae$pct)), ")"),
                       "0")

# --- Step 6: Transpose (wide) ---
# Ensure full grid: all ord/category x all ordered_trt, fill missing with blank then '0'
ord_cat <- all_ae %>% distinct(ord, category) %>% arrange(ord)
# In case some ord missing entirely (zero counts everywhere), ensure 0-8 present with labels
expected_labels <- data.frame(
  ord = 0:8,
  category = c("Subjects with no TEAE",
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
# Use expected to guarantee rows
ord_cat <- expected_labels

# Build wide
wide <- ord_cat
for (t in ordered_trt) {
  sub <- all_ae[all_ae$TRTA == t, c("ord","value")]
  # if duplicate ord (shouldn't), take first
  sub <- sub[!duplicated(sub$ord), , drop=FALSE]
  wide <- merge(wide, sub, by="ord", all.x=TRUE, sort=FALSE)
  names(wide)[names(wide)=="value"] <- paste0("V_", t)
}
# Rename to col1..col3 in ordered_trt order
# Keep only first 3 treatments (ntrt=3)
use_trt <- ordered_trt[1:min(3, length(ordered_trt))]
# If fewer than 3, pad
while (length(use_trt) < 3) use_trt <- c(use_trt, paste0("__MISSING", length(use_trt)+1))

final <- ord_cat[order(ord_cat$ord), , drop=FALSE]
final$col1 <- wide[[paste0("V_", use_trt[1])]][match(final$ord, wide$ord)]
final$col2 <- if (use_trt[2] %in% paste0("__MISSING",1:3) || !(paste0("V_", use_trt[2]) %in% names(wide))) NA_character_ else wide[[paste0("V_", use_trt[2])]][match(final$ord, wide$ord)]
final$col3 <- if (use_trt[3] %in% paste0("__MISSING",1:3) || !(paste0("V_", use_trt[3]) %in% names(wide))) NA_character_ else wide[[paste0("V_", use_trt[3])]][match(final$ord, wide$ord)]
# Actually if use_trt derived from ordered_trt, V_ columns exist for real trts; handle missing trt case
for (j in 1:3) {
  coln <- paste0("col", j)
  final[[coln]][is.na(final[[coln]])] <- ""
  final[[coln]][final[[coln]] == ""] <- "0"
}

# --- Step 7 already done (zero fill) ---
report <- final[, c("ord","category","col1","col2","col3")]
report <- report[order(report$ord), ]

# Update headers to match use_trt (first 3)
hdr_map <- function(t) {
  b <- if (t %in% names(bign_lookup)) bign_lookup[[t]] else 0
  paste0(t, " (N=", b, ")")
}
# Only for real treatments; if padded missing, header blank
col1hdr <- if (grepl("^__MISSING", use_trt[1])) "" else hdr_map(use_trt[1])
col2hdr <- if (grepl("^__MISSING", use_trt[2])) "" else hdr_map(use_trt[2])
col3hdr <- if (grepl("^__MISSING", use_trt[3])) "" else hdr_map(use_trt[3])

cat("Report:\n")
print(report)

# --- Step 8: outputs ---
# CSV of table body
csv_df <- report
# For CSV, keep ord, category, col1, col2, col3 with header names including N for clarity?
# Write with SAS-like column names: category, col1, col2, col3 but include header row mapping in separate? 
# Provide CSV with columns: ord, category, col1, col2, col3 where col headers are the treatment headers
# To preserve headers, set names to include N
out_csv <- data.frame(
  ord = csv_df$ord,
  category = csv_df$category,
  col1 = csv_df$col1,
  col2 = csv_df$col2,
  col3 = csv_df$col3,
  stringsAsFactors = FALSE, check.names = FALSE
)
# Rename col1-3 to header strings for CSV header (but keep syntactically valid; write.csv will quote)
names(out_csv) <- c("ord", "category", col1hdr, col2hdr, col3hdr)
write.csv(out_csv, file.path(out, "t_ae_sum.csv"), row.names = FALSE, na = "")

# Text rendering
txt_path <- file.path(out, "t_ae_sum.txt")
sink(txt_path)
cat("Table 14.3.1\n")
cat("Overall Summary of Treatment-Emergent Adverse Events\n")
cat("Safety Analysis Set\n\n")
# Header line
# Determine widths
w_cat <- max(nchar(c("Category", report$category)), na.rm=TRUE) + 2
w1 <- max(nchar(c(col1hdr, report$col1)), na.rm=TRUE) + 2
w2 <- max(nchar(c(col2hdr, report$col2)), na.rm=TRUE) + 2
w3 <- max(nchar(c(col3hdr, report$col3)), na.rm=TRUE) + 2
# Print header
cat(sprintf(paste0("%-", w_cat, "s %", w1, "s %", w2, "s %", w3, "s\n"), "", col1hdr, col2hdr, col3hdr))
cat(paste0(strrep("-", w_cat), " ", strrep("-", w1), " ", strrep("-", w2), " ", strrep("-", w3), "\n"))
for (i in seq_len(nrow(report))) {
  cat(sprintf(paste0("%-", w_cat, "s %", w1, "s %", w2, "s %", w3, "s\n"),
              report$category[i], report$col1[i], report$col2[i], report$col3[i]))
}
cat("\n")
cat("TEAE = Treatment-Emergent Adverse Event; SAE = Serious Adverse Event\n")
cat("Subjects are counted once per category.\n")
cat("Data cutoff: [DDMONYYYY].\n")
cat("Source: ADaM datasets (ADSL, ADAE)\n")
sink()

cat("Wrote", file.path(out, "t_ae_sum.csv"), "and", file.path(out, "t_ae_sum.txt"), "\n")
cat("P069 t_ae_sum: completed successfully.\n")
```
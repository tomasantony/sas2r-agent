# P069 t_ae_sum - Overall Summary of Treatment-Emergent Adverse Events
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

cat("P069 t_ae_sum: starting\n")

resolve_dir <- function(cands) {
  for (d in cands) {
    if (!is.na(d) && d != "" && dir.exists(d)) return(d)
  }
  return(cands[1])
}

adam_dir <- resolve_dir(c(
  "/bench/data/raw/own-synthetic/adam",
  "/bench/data/raw/own-synthetic/adam",
  file.path(Sys.getenv("RAWADAM"), ""),
  "data/raw/own-synthetic/adam"
))
cat("adam_dir:", adam_dir, "\n")

find_file <- function(dir, name) {
  if (dir.exists(dir)) {
    files <- list.files(dir, full.names = TRUE)
    idx <- which(tolower(tools::file_path_sans_ext(basename(files))) == tolower(name))
    if (length(idx) > 0) return(files[idx[1]])
    idx2 <- grep(paste0("(?i)^", name, "\\."), basename(files))
    if (length(idx2) > 0) return(files[idx2[1]])
  }
  # fallback: search bench
  alt <- file.path("/bench/data/raw/own-synthetic/adam", paste0(name, ".sas7bdat"))
  if (file.exists(alt)) return(alt)
  alt2 <- file.path("/bench/data/raw/own-synthetic/adam", paste0(name, ".xpt"))
  if (file.exists(alt2)) return(alt2)
  stop(paste0("File not found: ", name, " in ", dir))
}

read_any <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "xpt") {
    df <- haven::read_xpt(path)
  } else if (ext %in% c("sas7bdat", "sas7bcat")) {
    df <- haven::read_sas(path)
  } else {
    df <- haven::read_sas(path)
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled") || inherits(x, "haven_labelled_spss")) {
      x <- haven::zap_labels(x)
      x <- haven::zap_formats(x)
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

to_chr <- function(x) {
  if (is.character(x)) return(x)
  if (is.numeric(x) || is.integer(x)) return(as.character(x))
  as.character(x)
}

for (c in c("USUBJID","TRTA","SAFFL")) if (!c %in% names(adsl)) stop(paste("ADSL missing", c))
for (c in c("USUBJID","TRTA","TRTEMFL","AEREL","AETOXGR","AESER","AEACN","AEOUT")) {
  if (!c %in% names(adae)) adae[[c]] <- NA_character_
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

# --- Step 1: Big N (upcase(SAFFL)='Y') ---
saffl_up <- toupper(adsl$SAFFL)
saffl_up[is.na(saffl_up)] <- ""
adsl_saf <- adsl[saffl_up == "Y", , drop = FALSE]

bign_df <- adsl_saf %>%
  filter(!is.na(TRTA) & TRTA != "") %>%
  distinct(TRTA, USUBJID) %>%
  group_by(TRTA) %>%
  summarise(bign = n(), .groups = "drop") %>%
  arrange(TRTA) %>%
  as.data.frame()
bigntot <- dplyr::n_distinct(adsl_saf$USUBJID)
cat("Big N:\n"); print(bign_df); cat("Total:", bigntot, "\n")

trt_vals <- sort(unique(adsl_saf$TRTA))
cat("TRTA values:", paste(trt_vals, collapse=" | "), "\n")
find_trt <- function(pattern) {
  idx <- grep(pattern, trt_vals, ignore.case = TRUE)
  if (length(idx) == 0) return(NA_character_)
  trt_vals[idx[1]]
}
placebo_val <- find_trt("placebo")
low_val <- find_trt("low")
high_val <- find_trt("high")
ordered_trt <- c()
if (!is.na(placebo_val)) ordered_trt <- c(ordered_trt, placebo_val)
if (!is.na(low_val)) ordered_trt <- c(ordered_trt, low_val)
if (!is.na(high_val)) ordered_trt <- c(ordered_trt, high_val)
for (v in trt_vals) if (!(v %in% ordered_trt)) ordered_trt <- c(ordered_trt, v)
cat("Ordered TRTA:", paste(ordered_trt, collapse=" | "), "\n")

bign_lookup <- setNames(bign_df$bign, bign_df$TRTA)

# --- Safety-exact join base ---
adsl_saf_exact <- adsl[!is.na(adsl$SAFFL) & adsl$SAFFL == "Y", , drop = FALSE]
saf_subjects <- unique(adsl_saf_exact$USUBJID)
is_teae <- !is.na(adae$TRTEMFL) & adae$TRTEMFL == "Y"
adae_teae_saf <- adae[adae$USUBJID %in% saf_subjects & is_teae, , drop = FALSE]

count_by_trta <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(TRTA=character(0), cnt=integer(0), stringsAsFactors=FALSE))
  }
  df2 <- df[!is.na(df$TRTA) & df$TRTA != "", , drop=FALSE]
  if (nrow(df2) == 0) {
    return(data.frame(TRTA=character(0), cnt=integer(0), stringsAsFactors=FALSE))
  }
  u <- unique(df2[, c("TRTA","USUBJID")])
  agg <- as.data.frame(table(u$TRTA), stringsAsFactors=FALSE)
  names(agg) <- c("TRTA","cnt")
  agg$cnt <- as.integer(agg$cnt)
  agg
}

ae_category <- function(filter_fun, label, ord) {
  df <- adae_teae_saf
  if (!is.null(df) && nrow(df) > 0) {
    keep <- filter_fun(df)
    if (is.null(keep) || length(keep) != nrow(df)) keep <- rep(FALSE, nrow(df))
    keep[is.na(keep)] <- FALSE
    df <- df[keep, , drop=FALSE]
  } else {
    df <- df[0, , drop=FALSE]
  }
  cbt <- count_by_trta(df)
  if (nrow(cbt) == 0) {
    return(data.frame(TRTA=character(0), cnt=integer(0),
                      category=character(0), ord=numeric(0),
                      stringsAsFactors=FALSE))
  }
  cbt$category <- label
  cbt$ord <- ord
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

# --- Step 3: no TEAE ---
teae_subjects_all <- unique(adae$USUBJID[!is.na(adae$TRTEMFL) & adae$TRTEMFL=="Y"])
ae0_base <- adsl_saf_exact[!(adsl_saf_exact$USUBJID %in% teae_subjects_all), , drop=FALSE]
if (nrow(ae0_base) > 0) {
  ae0 <- count_by_trta(ae0_base)
  if (nrow(ae0) > 0) {
    ae0$category <- "Subjects with no TEAE"
    ae0$ord <- 0
    ae0 <- ae0[, c("TRTA","cnt","category","ord")]
  } else {
    ae0 <- data.frame(TRTA=character(0), cnt=integer(0), category=character(0), ord=numeric(0), stringsAsFactors=FALSE)
  }
} else {
  ae0 <- data.frame(TRTA=character(0), cnt=integer(0), category=character(0), ord=numeric(0), stringsAsFactors=FALSE)
}

all_ae <- dplyr::bind_rows(ae0, ae1, ae2, ae3, ae4, ae5, ae6, ae7, ae8)

all_ae$bign <- sapply(all_ae$TRTA, function(t) if (t %in% names(bign_lookup)) bign_lookup[[t]] else NA_real_)
all_ae$pct <- ifelse(!is.na(all_ae$bign) & all_ae$bign > 0, all_ae$cnt / all_ae$bign * 100, NA_real_)

round_half_up_1 <- function(x) {
  floor(x * 10 + 0.5 + 1e-9) / 10
}
fmt_pct_5_1 <- function(p) {
  r <- round_half_up_1(p)
  sprintf("%.1f", r)
}
all_ae$value <- ifelse(all_ae$cnt > 0,
                       paste0(trimws(as.character(all_ae$cnt)), " (", trimws(fmt_pct_5_1(all_ae$pct)), ")"),
                       "0")

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

use_trt <- ordered_trt[1:min(3, length(ordered_trt))]
while (length(use_trt) < 3) use_trt <- c(use_trt, paste0("__MISSING", length(use_trt)+1))

final <- expected_labels[order(expected_labels$ord), , drop=FALSE]
for (j in 1:3) {
  t <- use_trt[j]
  coln <- paste0("col", j)
  if (grepl("^__MISSING", t)) {
    final[[coln]] <- "0"
  } else {
    sub <- all_ae[all_ae$TRTA == t, c("ord","value")]
    sub <- sub[!duplicated(sub$ord), , drop=FALSE]
    m <- match(final$ord, sub$ord)
    vals <- sub$value[m]
    vals[is.na(vals)] <- "0"
    vals[vals == ""] <- "0"
    final[[coln]] <- vals
  }
}
report <- final[, c("ord","category","col1","col2","col3")]
report <- report[order(report$ord), ]

hdr_map <- function(t) {
  b <- if (t %in% names(bign_lookup)) bign_lookup[[t]] else 0L
  paste0(t, " (N=", b, ")")
}
col1hdr <- if (grepl("^__MISSING", use_trt[1])) "" else hdr_map(use_trt[1])
col2hdr <- if (grepl("^__MISSING", use_trt[2])) "" else hdr_map(use_trt[2])
col3hdr <- if (grepl("^__MISSING", use_trt[3])) "" else hdr_map(use_trt[3])
cat("Report:\n"); print(report)

out_csv <- data.frame(
  ord = report$ord,
  category = report$category,
  col1 = report$col1,
  col2 = report$col2,
  col3 = report$col3,
  stringsAsFactors = FALSE, check.names = FALSE
)
names(out_csv) <- c("ord", "category", col1hdr, col2hdr, col3hdr)
write.csv(out_csv, file.path(out, "t_ae_sum.csv"), row.names = FALSE, na = "")

txt_path <- file.path(out, "t_ae_sum.txt")
sink(txt_path)
cat("Table 14.3.1\n")
cat("Overall Summary of Treatment-Emergent Adverse Events\n")
cat("Safety Analysis Set\n\n")
w_cat <- max(nchar(c("Category", report$category)), na.rm=TRUE) + 2
w1 <- max(nchar(c(col1hdr, report$col1)), na.rm=TRUE) + 2
w2 <- max(nchar(c(col2hdr, report$col2)), na.rm=TRUE) + 2
w3 <- max(nchar(c(col3hdr, report$col3)), na.rm=TRUE) + 2
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

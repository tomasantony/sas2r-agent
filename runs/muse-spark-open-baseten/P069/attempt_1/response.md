```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

# helper: round half away from zero (SAS)
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-12)
  sign_x * z / mult
}
fmt_pct1 <- function(pct) {
  # SAS put(pct,5.1) then strip -> one decimal, half away
  if (is.na(pct)) return(NA_character_)
  v <- round_half_up(pct, 1)
  formatC(v, format = "f", digits = 1, flag = "")
}
fmt_value <- function(cnt, pct) {
  if (is.na(cnt) || cnt == 0) return("0")
  # strip(put(cnt,8.)) -> no leading spaces
  cnt_s <- as.character(as.integer(cnt))
  pct_s <- trimws(fmt_pct1(pct))
  paste0(cnt_s, " (", pct_s, ")")
}

# helper to read adam dataset by name (adsl, adae)
read_adam <- function(name) {
  # try .sas7bdat, .xpt, .sas7bdat in adam_path
  f1 <- file.path(adam_path, paste0(name, ".sas7bdat"))
  f2 <- file.path(adam_path, paste0(tolower(name), ".sas7bdat"))
  f3 <- file.path(adam_path, paste0(name, ".xpt"))
  f4 <- file.path(adam_path, paste0(tolower(name), ".xpt"))
  fp <- NULL
  for (f in c(f1,f2,f3,f4)) if (file.exists(f)) { fp <- f; break }
  # also list directory case-insensitive
  if (is.null(fp) && dir.exists(adam_path)) {
    fl <- list.files(adam_path, full.names = TRUE)
    idx <- which(tolower(basename(fl)) %in% c(paste0(tolower(name), ".sas7bdat"), paste0(tolower(name), ".xpt")))
    if (length(idx) > 0) fp <- fl[idx[1]]
  }
  if (is.null(fp)) stop(paste("Dataset not found:", name, "in", adam_path))
  if (grepl("\\.xpt$", fp, ignore.case = TRUE)) {
    df <- haven::read_xpt(fp)
  } else {
    df <- haven::read_sas(fp)
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # convert haven_labelled to character/numeric
  for (nm in names(df)) {
    if (haven::is.labelled(df[[nm]])) {
      # if character labelled, convert to character
      if (is.character(haven::as_factor(df[[nm]])) || is.character(df[[nm]])) {
        df[[nm]] <- as.character(haven::as_factor(df[[nm]]))
        # haven as_factor may return factor; ensure character
        df[[nm]] <- as.character(df[[nm]])
      } else {
        # numeric labelled -> keep numeric but strip label
        df[[nm]] <- haven::zap_labels(df[[nm]])
      }
    }
  }
  # trim character columns (SAS strip behaviour for comparison)
  for (nm in names(df)) if (is.character(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  df
}

adsl <- read_adam("adsl")
adae <- read_adam("adae")

# Ensure required columns exist (uppercase names as in SAS)
# haven may preserve case; normalize to uppercase for processing but keep original
names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))

# Big N: SAFFL Y (case-insensitive as in m_counts)
adsl_saff <- adsl[toupper(adsl$SAFFL) == "Y" & !is.na(adsl$SAFFL), , drop = FALSE]

# distinct USUBJID per TRTA
bign_df <- adsl_saff %>%
  dplyr::group_by(TRTA) %>%
  dplyr::summarise(bign = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
  dplyr::arrange(TRTA)

# SAS sorts character with uppercase before lowercase; R arrange is similar for ASCII
# Ensure order by TRTA string
bign_df <- bign_df[order(bign_df$TRTA), , drop = FALSE]

# Total N
bign_tot <- dplyr::n_distinct(adsl_saff$USUBJID)

# Build header strings like SAS col&i.hdr
# TRTA labels are as stored; use them directly
trt_levels <- bign_df$TRTA
ntrt <- nrow(bign_df)
col_hdrs <- paste0(trt_levels, " (N=", bign_df$bign, ")")
names(col_hdrs) <- trt_levels

# Helper to count distinct subjects per TRTA for a filtered adae
count_ae_category <- function(filter_expr, label, ord) {
  # filter_expr is a function(df) returning logical vector
  # base condition: TRTEMFL='Y' and SAFFL='Y' via join
  # adae TRTEMFL Y
  ae_f <- adae[adae$TRTEMFL == "Y" & !is.na(adae$TRTEMFL), , drop = FALSE]
  if (!is.null(filter_expr)) {
    keep <- filter_expr(ae_f)
    keep[is.na(keep)] <- FALSE
    ae_f <- ae_f[keep, , drop = FALSE]
  }
  # inner join to adsl SAFFL Y to ensure safety population
  # keep only USUBJID in adsl_saff
  ae_f <- ae_f[ae_f$USUBJID %in% adsl_saff$USUBJID, , drop = FALSE]
  if (nrow(ae_f) == 0) {
    return(data.frame(TRTA = character(0), cnt = numeric(0), category = character(0), ord = numeric(0), stringsAsFactors = FALSE))
  }
  # count distinct USUBJID per TRTA (TRTA from adae)
  # if TRTA missing, use adsl TRTA? Use adae TRTA
  cnt_df <- ae_f %>%
    dplyr::group_by(TRTA) %>%
    dplyr::summarise(cnt = dplyr::n_distinct(USUBJID), .groups = "drop")
  cnt_df$category <- label
  cnt_df$ord <- ord
  as.data.frame(cnt_df)
}

# Define categories
ae1 <- count_ae_category(NULL, "Subjects with at least one TEAE", 1)
ae2 <- count_ae_category(function(df) df$AEREL %in% c("POSSIBLE","PROBABLE","DEFINITE","RELATED"),
                          "  Related TEAE", 2)
ae3 <- count_ae_category(function(df) {
  # input(a.AETOXGR, best.) >=3
  v <- suppressWarnings(as.numeric(df$AETOXGR))
  !is.na(v) & v >= 3
}, "  Grade >=3 TEAE", 3)
ae4 <- count_ae_category(function(df) df$AESER == "Y", "  Serious Adverse Event", 4)
ae5 <- count_ae_category(function(df) df$AESER == "Y" & df$AEREL %in% c("POSSIBLE","PROBABLE","DEFINITE","RELATED"),
                          "  Related Serious Adverse Event", 5)
ae6 <- count_ae_category(function(df) df$AEACN == "DRUG WITHDRAWN",
                          "  TEAE Leading to Study Drug Discontinuation", 6)
ae7 <- count_ae_category(function(df) df$AEACN %in% c("DOSE REDUCED","DRUG INTERRUPTED","DOSE INCREASED"),
                          "  TEAE Leading to Dose Modification", 7)
ae8 <- count_ae_category(function(df) df$AEOUT == "FATAL",
                          "  TEAE with Outcome of Death", 8)

# Subjects with no TEAE
# distinct USUBJID from adae where TRTEMFL='Y'
teae_subjects <- unique(adae$USUBJID[adae$TRTEMFL == "Y" & !is.na(adae$TRTEMFL)])
no_teae_adsl <- adsl_saff[!adsl_saff$USUBJID %in% teae_subjects, , drop = FALSE]
if (nrow(no_teae_adsl) > 0) {
  ae0 <- no_teae_adsl %>%
    dplyr::group_by(TRTA) %>%
    dplyr::summarise(cnt = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
    as.data.frame()
  ae0$category <- "Subjects with no TEAE"
  ae0$ord <- 0
} else {
  ae0 <- data.frame(TRTA=character(0), cnt=numeric(0), category=character(0), ord=numeric(0))
}

all_ae <- dplyr::bind_rows(ae0, ae1, ae2, ae3, ae4, ae5, ae6, ae7, ae8)

# Calculate percentages
all_ae2 <- dplyr::left_join(all_ae, bign_df, by = "TRTA")
all_ae2$pct <- ifelse(all_ae2$bign > 0, all_ae2$cnt / all_ae2$bign * 100, NA_real_)

# Format value
all_ae2$value <- mapply(fmt_value, all_ae2$cnt, all_ae2$pct, USE.NAMES = FALSE)

# Ensure all combinations of ord/category/TRTA exist for transpose (to get 0s)
# Get distinct ord/category
ord_cat <- unique(all_ae2[, c("ord","category"), drop = FALSE])
# Expand with all TRTA levels
expanded <- merge(ord_cat, data.frame(TRTA = trt_levels, stringsAsFactors = FALSE), by = NULL)
all_ae3_full <- dplyr::left_join(expanded, all_ae2[, c("TRTA","ord","category","cnt","bign","pct","value")], by = c("TRTA","ord","category"))
# For missing cnt, set 0 and value 0
all_ae3_full$cnt[is.na(all_ae3_full$cnt)] <- 0
all_ae3_full$value[is.na(all_ae3_full$value)] <- "0"
# For pct missing where cnt 0, keep NA but value is 0

# Transpose: pivot wider
# Use tidyr
final_wide <- all_ae3_full %>%
  dplyr::select(ord, category, TRTA, value) %>%
  tidyr::pivot_wider(names_from = TRTA, values_from = value, values_fill = "0")

# Ensure column order matches bign_df order (sorted TRTA)
# final_wide currently has columns ord, category, plus TRTA columns in order of appearance
# Reorder TRTA columns to trt_levels order
cols_present <- intersect(trt_levels, names(final_wide))
final_wide <- final_wide[, c("ord","category", cols_present), drop = FALSE]
# Sort by ord
final_wide <- final_wide[order(final_wide$ord), , drop = FALSE]

# Create col1, col2, col3 aliases for report (SAS expects col1-col3)
# Map in sorted order
report <- final_wide
# Rename TRTA columns to col1, col2, col3 in order
for (i in seq_along(cols_present)) {
  names(report)[names(report) == cols_present[i]] <- paste0("col", i)
}
# Ensure all col1..col3 exist (if ntrt <3, pad)
for (i in seq_len(ntrt)) {
  cn <- paste0("col", i)
  if (!cn %in% names(report)) report[[cn]] <- "0"
  report[[cn]][is.na(report[[cn]]) | report[[cn]] == ""] <- "0"
}
# Keep only ord, category, col1..col3
report <- report[, c("ord","category", paste0("col", seq_len(ntrt))), drop = FALSE]

# Prepare CSV output: include headers with N
# Build display column names
display_names <- c("ord","category", col_hdrs[cols_present])
# For CSV, use display names as header row
csv_df <- report
# Rename col columns to display headers for CSV
for (i in seq_along(cols_present)) {
  names(csv_df)[names(csv_df) == paste0("col", i)] <- col_hdrs[cols_present[i]]
}
# Write CSV dataset (report)
write.csv(csv_df, file.path(out, "t_ae_sum.csv"), row.names = FALSE, na = "")

# Also write underlying counts CSV for traceability (optional)
write.csv(all_ae2, file.path(out, "t_ae_sum_counts.csv"), row.names = FALSE, na = "")

# Text rendering
# Titles from u_titles.sas for T_AE_SUM
title_lines <- c(
  "Table 14.3.1",
  "Overall Summary of Treatment-Emergent Adverse Events",
  "Safety Analysis Set"
)
# Column headers for text
hdr1 <- "Category"
hdr_cols <- col_hdrs[cols_present]

# Build text table
# Determine column widths
# Use fixed width for display
# Create header separator
col_width_cat <- 50
col_width_trt <- 20

# Format function for txt row
pad_right <- function(x, w) sprintf(paste0("%-", w, "s"), substr(x, 1, w))
pad_center <- function(x, w) {
  x <- as.character(x)
  len <- nchar(x)
  if (len >= w) return(substr(x,1,w))
  pad <- w - len
  left <- floor(pad/2)
  right <- pad - left
  paste0(strrep(" ", left), x, strrep(" ", right))
}

txt_path <- file.path(out, "t_ae_sum.txt")
con <- file(txt_path, open = "wt")

writeLines(paste(rep("-", 80), collapse=""), con)
writeLines(title_lines, con)
writeLines("", con)
# Big N info already in headers
# Header row
header_row <- paste0(pad_right(hdr1, col_width_cat),
                     paste(sapply(hdr_cols, function(h) pad_center(h, col_width_trt)), collapse=""))
writeLines(header_row, con)
writeLines(paste0(pad_right(strrep("-", nchar(hdr1)), col_width_cat),
                  paste(sapply(hdr_cols, function(h) pad_center(strrep("-", min(nchar(h), col_width_trt-2)), col_width_trt)), collapse="")), con)

for (r in seq_len(nrow(report))) {
  cat_val <- report$category[r]
  # keep leading spaces for indentation
  cat_disp <- pad_right(cat_val, col_width_cat)
  trt_vals <- sapply(seq_len(ntrt), function(i) pad_center(report[r, paste0("col", i)], col_width_trt))
  line <- paste0(cat_disp, paste(trt_vals, collapse=""))
  writeLines(line, con)
}
writeLines("", con)
writeLines("TEAE = Treatment-Emergent Adverse Event; SAE = Serious Adverse Event", con)
writeLines("Subjects are counted once per category.", con)
writeLines(paste0("Data cutoff: [DDMONYYYY]"), con)
writeLines("Source: ADaM datasets", con)
writeLines("", con)
# Footnotes with counts for verification
writeLines(paste0("N (Total) = ", bign_tot), con)
for (i in seq_len(ntrt)) {
  writeLines(paste0(trt_levels[i], " N=", bign_df$bign[i]), con)
}
close(con)

# Also write PNG placeholder if needed (no figure required for this table)
# Ensure at least one PNG is not required; skip

cat("P069 t_ae_sum completed\n")
```
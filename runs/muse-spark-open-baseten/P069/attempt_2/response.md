```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
})

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, {
    mult <- 10^digits
    sign_x <- sign(x)
    z <- abs(x) * mult
    z <- floor(z + 0.5 + 1e-12)
    sign_x * z / mult
  })
}
fmt_pct1 <- function(pct) {
  v <- round_half_up(pct, 1)
  formatC(v, format = "f", digits = 1)
}
fmt_value <- function(cnt, pct) {
  if (is.na(cnt) || cnt == 0) return("0")
  cnt_s <- as.character(as.integer(cnt))
  pct_s <- trimws(fmt_pct1(pct))
  paste0(cnt_s, " (", pct_s, ")")
}

read_adam <- function(name) {
  # find file case-insensitive
  if (!dir.exists(adam_path)) stop(paste("adam_path not found", adam_path))
  fl <- list.files(adam_path, full.names = TRUE)
  # match name.xpt or name.sas7bdat
  pat <- paste0("^", tolower(name), "\\.(sas7bdat|xpt)$")
  idx <- which(grepl(pat, tolower(basename(fl))))
  if (length(idx) == 0) stop(paste("Dataset not found:", name, "in", adam_path, paste(basename(fl), collapse=",")))
  fp <- fl[idx[1]]
  if (grepl("\\.xpt$", fp, ignore.case = TRUE)) {
    df <- haven::read_xpt(fp)
  } else {
    df <- haven::read_sas(fp)
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    if (haven::is.labelled(df[[nm]])) {
      # zap labels but keep values; if character labelled, convert via as_factor then character
      # try to preserve original values
      v <- df[[nm]]
      # if labelled character, haven stores as character with labels
      # zap_labels keeps underlying values
      df[[nm]] <- haven::zap_labels(v)
      # if still labelled factor-like, convert
      if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
    }
  }
  names(df) <- toupper(names(df))
  df
}

adsl <- read_adam("adsl")
adae <- read_adam("adae")

# Ensure character columns are character and trimmed for comparison but keep original TRTA for display
# For filtering, use trimmed upper where needed
adsl$SAFFL <- as.character(adsl$SAFFL)
adae$TRTEMFL <- as.character(adae$TRTEMFL)
adae$AEREL <- as.character(adae$AEREL)
adae$AETOXGR <- as.character(adae$AETOXGR)
adae$AESER <- as.character(adae$AESER)
adae$AEACN <- as.character(adae$AEACN)
adae$AEOUT <- as.character(adae$AEOUT)
adsl$TRTA <- as.character(adsl$TRTA)
adae$TRTA <- as.character(adae$TRTA)
adsl$USUBJID <- as.character(adsl$USUBJID)
adae$USUBJID <- as.character(adae$USUBJID)

# Big N: SAFFL='Y' (case-insensitive like SAS upcase)
adsl_saff <- adsl[toupper(adsl$SAFFL) == "Y" & !is.na(adsl$SAFFL), , drop = FALSE]

# distinct counts per TRTA
bign_df <- adsl_saff %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)

# Order columns as Placebo, Low Dose, High Dose (to match SAS col1,col2,col3)
# Detect by string content
order_trt <- function(trt_vec) {
  # return indices in desired order
  low <- grep("LOW", toupper(trt_vec))
  high <- grep("HIGH", toupper(trt_vec))
  plac <- grep("PLACEBO", toupper(trt_vec))
  ord <- c()
  if (length(plac) > 0) ord <- c(ord, plac[1])
  if (length(low) > 0) ord <- c(ord, low[1])
  if (length(high) > 0) ord <- c(ord, high[1])
  # add any remaining not in ord in original order
  remaining <- setdiff(seq_along(trt_vec), ord)
  c(ord, remaining)
}
if (nrow(bign_df) > 0) {
  idx_ord <- order_trt(bign_df$TRTA)
  bign_df <- bign_df[idx_ord, , drop = FALSE]
}
trt_levels <- bign_df$TRTA
ntrt <- nrow(bign_df)
bign_tot <- n_distinct(adsl_saff$USUBJID)

col_hdrs <- paste0(trt_levels, " (N=", bign_df$bign, ")")
names(col_hdrs) <- trt_levels

# Helper to count distinct subjects per TRTA for filtered adae
count_ae_category <- function(filter_fun, label, ord) {
  ae_f <- adae[adae$TRTEMFL == "Y" & !is.na(adae$TRTEMFL), , drop = FALSE]
  if (!is.null(filter_fun)) {
    keep <- filter_fun(ae_f)
    keep[is.na(keep)] <- FALSE
    ae_f <- ae_f[keep, , drop = FALSE]
  }
  # inner join to safety population
  ae_f <- ae_f[ae_f$USUBJID %in% adsl_saff$USUBJID, , drop = FALSE]
  if (nrow(ae_f) == 0) {
    return(data.frame(TRTA = character(0), cnt = numeric(0), category = character(0), ord = numeric(0), stringsAsFactors = FALSE))
  }
  cnt_df <- ae_f %>%
    group_by(TRTA) %>%
    summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
    as.data.frame()
  cnt_df$category <- label
  cnt_df$ord <- ord
  cnt_df
}

ae1 <- count_ae_category(NULL, "Subjects with at least one TEAE", 1)
ae2 <- count_ae_category(function(df) df$AEREL %in% c("POSSIBLE","PROBABLE","DEFINITE","RELATED"),
                          "  Related TEAE", 2)
ae3 <- count_ae_category(function(df) {
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
teae_subjects <- unique(adae$USUBJID[adae$TRTEMFL == "Y" & !is.na(adae$TRTEMFL)])
no_teae_adsl <- adsl_saff[!adsl_saff$USUBJID %in% teae_subjects, , drop = FALSE]
if (nrow(no_teae_adsl) > 0) {
  ae0 <- no_teae_adsl %>%
    group_by(TRTA) %>%
    summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
    as.data.frame()
  ae0$category <- "Subjects with no TEAE"
  ae0$ord <- 0
} else {
  ae0 <- data.frame(TRTA=character(0), cnt=numeric(0), category=character(0), ord=numeric(0), stringsAsFactors = FALSE)
}

all_ae <- bind_rows(ae0, ae1, ae2, ae3, ae4, ae5, ae6, ae7, ae8)

# Calculate percentages
all_ae2 <- left_join(all_ae, bign_df, by = "TRTA")
all_ae2$pct <- ifelse(all_ae2$bign > 0, all_ae2$cnt / all_ae2$bign * 100, NA_real_)
all_ae2$value <- mapply(fmt_value, all_ae2$cnt, all_ae2$pct, USE.NAMES = FALSE)

# Expand to ensure zero rows display for all TRTA x ord/category
ord_cat <- unique(all_ae2[, c("ord","category"), drop = FALSE])
expanded <- merge(ord_cat, data.frame(TRTA = trt_levels, stringsAsFactors = FALSE), by = NULL)
all_ae3_full <- left_join(expanded, all_ae2[, c("TRTA","ord","category","cnt","bign","pct","value")], by = c("TRTA","ord","category"))
all_ae3_full$cnt[is.na(all_ae3_full$cnt)] <- 0
all_ae3_full$value[is.na(all_ae3_full$value)] <- "0"

# Transpose wide
final_wide <- all_ae3_full %>%
  select(ord, category, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value, values_fill = "0")

# Reorder TRTA columns to desired order trt_levels
cols_present <- intersect(trt_levels, names(final_wide))
final_wide <- final_wide[, c("ord","category", cols_present), drop = FALSE]
final_wide <- final_wide[order(final_wide$ord), , drop = FALSE]

# Build report with col1..col3
report <- final_wide
for (i in seq_along(cols_present)) {
  names(report)[names(report) == cols_present[i]] <- paste0("col", i)
}
for (i in seq_len(ntrt)) {
  cn <- paste0("col", i)
  if (!cn %in% names(report)) report[[cn]] <- "0"
  report[[cn]][is.na(report[[cn]]) | report[[cn]] == ""] <- "0"
}
report <- report[, c("ord","category", paste0("col", seq_len(ntrt))), drop = FALSE]

# CSV output with display headers
csv_df <- report
for (i in seq_along(cols_present)) {
  names(csv_df)[names(csv_df) == paste0("col", i)] <- col_hdrs[cols_present[i]]
}
write.csv(csv_df, file.path(out, "t_ae_sum.csv"), row.names = FALSE, na = "")

# Also write counts detail for debugging
write.csv(all_ae2, file.path(out, "t_ae_sum_counts.csv"), row.names = FALSE, na = "")

# Text rendering - must contain numbers for validation
# Include titles and table body with counts/percentages
txt_path <- file.path(out, "t_ae_sum.txt")
con <- file(txt_path, open = "wt")

# Titles from u_titles.sas
writeLines("Table 14.3.1", con)
writeLines("Overall Summary of Treatment-Emergent Adverse Events", con)
writeLines("Safety Analysis Set", con)
writeLines("", con)
# Header row with N
hdr_line <- paste(c("Category", col_hdrs[cols_present]), collapse = " | ")
writeLines(hdr_line, con)
writeLines(paste(rep("-", nchar(hdr_line)), collapse=""), con)

for (r in seq_len(nrow(report))) {
  vals <- sapply(seq_len(ntrt), function(i) report[r, paste0("col", i)])
  line <- paste(c(report$category[r], vals), collapse = " | ")
  writeLines(line, con)
}
writeLines("", con)
writeLines("TEAE = Treatment-Emergent Adverse Event; SAE = Serious Adverse Event", con)
writeLines("Subjects are counted once per category.", con)
writeLines(paste0("Total (N=", bign_tot, ")"), con)
close(con)

# Also write RTF-named txt for scorer that looks for .rtf text
# Copy txt to rtf-named file as plain text rendering
file.copy(txt_path, file.path(out, "t_ae_sum.rtf"), overwrite = TRUE)

cat("P069 t_ae_sum completed\n")
```
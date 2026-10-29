# P070 t_cm.sas -> R translation
# Table 14.2.1 Summary of Concomitant Medications by ATC Class and Preferred Name
# Population: Safety Analysis Set (SAFFL='Y'), CONCOMFL='Y'

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(tibble)
})

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_key <- function(v) {
  sapply(v, function(one) {
    if (is.na(one)) return("\0")
    s <- as.character(one)
    if (s == "") return("\0")
    paste(sprintf("%03d", utf8ToInt(s)), collapse = "-")
  }, USE.NAMES = FALSE)
}
sas_order <- function(v) order(sas_key(v), na.last = FALSE, method = "radix")

read_adam <- function(dir, name) {
  lname <- tolower(name)
  cand <- c(
    file.path(dir, paste0(name, ".sas7bdat")),
    file.path(dir, paste0(lname, ".sas7bdat")),
    file.path(dir, paste0(name, ".xpt")),
    file.path(dir, paste0(lname, ".xpt")),
    file.path(dir, paste0(name, ".sas7bdat".tmp <- "")),
    file.path(dir, paste0(toupper(name), ".sas7bdat"))
  )
  # also try listing files case-insensitive
  if (dir.exists(dir)) {
    fl <- list.files(dir, full.names = TRUE)
    # find match ignoring case and extension
    idx <- which(tolower(tools::file_path_sans_ext(basename(fl))) == lname)
    if (length(idx) > 0) {
      # prefer sas7bdat then xpt
      ord <- order(match(tolower(tools::file_ext(fl[idx])), c("sas7bdat","xpt")))
      cand <- c(fl[idx][ord], cand)
    }
  }
  for (f in cand) {
    if (!is.na(f) && f != "" && file.exists(f)) {
      ext <- tolower(tools::file_ext(f))
      if (ext == "xpt") {
        df <- haven::read_xpt(f)
      } else {
        df <- haven::read_sas(f)
      }
      return(as.data.frame(df, stringsAsFactors = FALSE))
    }
  }
  stop(paste0("Cannot find dataset ", name, " in ", dir))
}

adsl_raw <- read_adam(adam_dir, "adsl")
adcm_raw <- read_adam(adam_dir, "adcm")
# uppercase names for case-insensitive matching (SAS case-insensitive)
names(adsl_raw) <- toupper(names(adsl_raw))
names(adcm_raw) <- toupper(names(adcm_raw))

# Ensure needed vars exist (character)
for (v in c("USUBJID","TRTA","TRTAN","SAFFL")) {
  if (!v %in% names(adsl_raw)) adsl_raw[[v]] <- NA_character_
}
for (v in c("USUBJID","TRTA","TRTAN","CMCLAS","CMDECOD","CMINDC","CMSTDTC","CMENDTC","CONCOMFL")) {
  if (!v %in% names(adcm_raw)) adcm_raw[[v]] <- NA_character_
}
# coerce to character where needed
charify <- function(x) {
  if (is.factor(x)) as.character(x) else if (is.numeric(x) && FALSE) x else as.character(x)
}
adsl_raw$USUBJID <- as.character(adsl_raw$USUBJID)
adsl_raw$TRTA <- as.character(adsl_raw$TRTA)
adsl_raw$SAFFL <- as.character(adsl_raw$SAFFL)
adcm_raw$USUBJID <- as.character(adcm_raw$USUBJID)
adcm_raw$TRTA <- as.character(adcm_raw$TRTA)
adcm_raw$CMCLAS <- as.character(adcm_raw$CMCLAS)
adcm_raw$CMDECOD <- as.character(adcm_raw$CMDECOD)
adcm_raw$CONCOMFL <- as.character(adcm_raw$CONCOMFL)
if ("TRTAN" %in% names(adcm_raw)) adcm_raw$TRTAN <- suppressWarnings(as.numeric(adcm_raw$TRTAN))
if ("TRTAN" %in% names(adsl_raw)) adsl_raw$TRTAN <- suppressWarnings(as.numeric(adsl_raw$TRTAN))

# --- Step 1: Get Big N (m_counts: upcase(SAFFL)='Y', group by TRTA order by TRTA) ---
adsl_pop_up <- adsl_raw %>% filter(!is.na(SAFFL) & toupper(SAFFL) == "Y")
bign_df <- adsl_pop_up %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
# SAS sorts character ordering uppercase before lowercase => byte order; missing first
# order by TRTA byte order
bign_df <- bign_df[order(sas_key(bign_df$TRTA), na.last = FALSE, method = "radix"), , drop = FALSE]
bigntot <- n_distinct(adsl_pop_up$USUBJID)
ntrt <- nrow(bign_df)
trtlbl <- bign_df$TRTA
bignv <- bign_df$bign
# Build header strings col1hdr..col3hdr in _bign (alphabetical) order
hdrs_sorted <- paste0(trtlbl, " (N=", bignv, ")")
# pad to 3
while (length(hdrs_sorted) < 3) hdrs_sorted <- c(hdrs_sorted, paste0("Missing (N=0)"))
col1hdr <- hdrs_sorted[1]; col2hdr <- hdrs_sorted[2]; col3hdr <- hdrs_sorted[3]

# --- Step 2: Prepare CM data (exact SAFFL='Y' and CONCOMFL='Y', inner join) ---
adsl_y <- adsl_raw %>% filter(!is.na(SAFFL) & SAFFL == "Y") %>% distinct(USUBJID)
adcm_c <- adcm_raw %>% filter(!is.na(CONCOMFL) & CONCOMFL == "Y")
cm <- dplyr::inner_join(adcm_c, adsl_y, by = "USUBJID")
# keep needed cols (TRTA from ADCM)
cm <- cm %>% select(USUBJID, TRTA, TRTAN, CMCLAS, CMDECOD, CMINDC, CMSTDTC, CMENDTC)

# --- Step 3: Any CM row ---
any_df <- cm %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(display_term = "Any Concomitant Medication",
         atc_ord = 0, pref_ord = 0, rowtype = 0,
         CMCLAS = NA_character_, CMDECOD = NA_character_) %>%
  as.data.frame()

# --- Step 4: ATC class counts ---
atc_df <- cm %>%
  group_by(TRTA, CMCLAS) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
atc_tot <- cm %>%
  group_by(CMCLAS) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
# test-copy patch: sort by CMCLAS before merge (SAS merge by CMCLAS)
atc_df <- atc_df[order(sas_key(atc_df$CMCLAS), na.last = TRUE), , drop = FALSE]
atc_tot <- atc_tot[order(sas_key(atc_tot$CMCLAS), na.last = TRUE), , drop = FALSE]
atc_rows <- dplyr::left_join(atc_df, atc_tot, by = "CMCLAS", na_matches = "na")
atc_rows$display_term <- atc_rows$CMCLAS
atc_rows$atc_ord <- atc_rows$cnt_tot
atc_rows$pref_ord <- 0
atc_rows$rowtype <- 1
atc_rows$CMDECOD <- NA_character_
atc_rows$cnt_tot <- NULL

# --- Step 5: Preferred name counts ---
pref_df <- cm %>%
  group_by(TRTA, CMCLAS, CMDECOD) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
pref_tot <- cm %>%
  group_by(CMCLAS, CMDECOD) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
# left joins
pref_rows <- dplyr::left_join(pref_df, pref_tot, by = c("CMCLAS","CMDECOD"), na_matches = "na", suffix = c("", ".pt"))
# atc totals for atc_ord
pref_rows <- dplyr::left_join(pref_rows, atc_tot %>% rename(atc_tot = cnt_tot), by = "CMCLAS", na_matches = "na")
# display_term = '  ' || strip(CMDECOD)
pref_rows$display_term <- sapply(pref_rows$CMDECOD, function(x) {
  if (is.na(x)) "  " else paste0("  ", trimws(as.character(x), which = "both"))
}, USE.NAMES = FALSE)
pref_rows$pref_ord <- pref_rows$cnt_tot
pref_rows$atc_ord <- pref_rows$atc_tot
pref_rows$rowtype <- 2
pref_rows$cnt_tot <- NULL
pref_rows$atc_tot <- NULL

# --- Step 6: Combine and calculate percentages ---
combined <- dplyr::bind_rows(any_df, atc_rows, pref_rows)
combined2 <- dplyr::left_join(combined, bign_df, by = "TRTA")
combined2$pct <- ifelse(!is.na(combined2$bign) & combined2$bign > 0,
                        combined2$cnt / combined2$bign * 100, NA_real_)
# order by atc_ord desc, CMCLAS, rowtype, pref_ord desc, display_term, TRTA (SAS semantics)
combined2 <- combined2 %>%
  mutate(.k1 = sas_key(CMCLAS), .k2 = sas_key(display_term), .k3 = sas_key(TRTA))
ord <- order(-combined2$atc_ord, combined2$.k1, combined2$rowtype,
             -combined2$pref_ord, combined2$.k2, combined2$.k3,
             na.last = TRUE, method = "radix")
combined2 <- combined2[ord, , drop = FALSE]
combined2$.k1 <- NULL; combined2$.k2 <- NULL; combined2$.k3 <- NULL

# value formatting: cat(strip(put(cnt,8.)), ' (', strip(put(pct,5.1)), ')') else '0'
combined3 <- combined2
combined3$value <- sapply(seq_len(nrow(combined3)), function(i) {
  cnt <- combined3$cnt[i]; pct <- combined3$pct[i]
  if (is.na(cnt) || cnt <= 0) return("0")
  cnt_s <- trimws(as.character(cnt), which = "both")
  pct_r <- round_half_up(pct, 1)
  pct_s <- trimws(sprintf("%.1f", pct_r), which = "both")
  paste0(cnt_s, " (", pct_s, ")")
}, USE.NAMES = FALSE)

# --- Step 7: Transpose (sort nodupkey then transpose by ...) ---
# sort nodupkey by descending atc_ord CMCLAS rowtype descending pref_ord display_term TRTA
combined3 <- combined3 %>%
  mutate(.k1 = sas_key(CMCLAS), .k2 = sas_key(display_term), .k3 = sas_key(TRTA))
ord2 <- order(-combined3$atc_ord, combined3$.k1, combined3$rowtype,
              -combined3$pref_ord, combined3$.k2, combined3$.k3,
              na.last = TRUE, method = "radix")
combined3 <- combined3[ord2, , drop = FALSE]
# nodupkey
keycols <- paste(combined3$atc_ord, combined3$.k1, combined3$rowtype,
                 combined3$pref_ord, combined3$.k2, combined3$.k3, sep = "|")
combined3 <- combined3[!duplicated(keycols), , drop = FALSE]
combined3$.k1 <- NULL; combined3$.k2 <- NULL; combined3$.k3 <- NULL

# pivot wider: by atc_ord CMCLAS rowtype pref_ord display_term; id TRTA; var value
wide <- tidyr::pivot_wider(combined3,
  id_cols = c("atc_ord","CMCLAS","rowtype","pref_ord","display_term"),
  names_from = "TRTA", values_from = "value",
  values_fill = NA_character_)
wide <- as.data.frame(wide, stringsAsFactors = FALSE, check.names = FALSE)

sanitize <- function(x) gsub("[^A-Za-z0-9_]", "_", x, perl = TRUE)
# SAS transposed names are col + sanitized ID (per test-copy patch)
old_names <- names(wide)
# map raw TRTA columns to col+sanitized
trt_vals <- setdiff(old_names, c("atc_ord","CMCLAS","rowtype","pref_ord","display_term"))
sas_colnames <- paste0("col", sapply(trt_vals, sanitize, USE.NAMES = FALSE))
names(wide)[match(trt_vals, names(wide))] <- sas_colnames

# SAS rename: (colPlacebo=col1 colDrug_X_Low_Dose=col2 colDrug_X_High_Dose=col3)
rename_map <- c("colPlacebo" = "col1",
                "colDrug_X_Low_Dose" = "col2",
                "colDrug_X_High_Dose" = "col3")
# apply if those source cols exist
for (src in names(rename_map)) {
  dst <- rename_map[[src]]
  if (src %in% names(wide)) {
    names(wide)[names(wide) == src] <- dst
  }
}
# fallback: if col1-3 not all present (different TRTA values), assign alphabetically
if (!all(c("col1","col2","col3") %in% names(wide))) {
  # collect remaining col* that are not col1-3 and not id cols
  idc <- c("atc_ord","CMCLAS","rowtype","pref_ord","display_term","col1","col2","col3")
  extra <- setdiff(names(wide), idc)
  # order extra by original TRTA byte order if possible
  # try to recover TRTA from sas name: reverse map via trt_vals
  # simply sort by sas name byte order
  if (length(extra) > 0) {
    extra_ord <- extra[order(sas_key(extra), method = "radix")]
    need <- setdiff(c("col1","col2","col3"), names(wide))
    for (k in seq_along(need)) {
      if (k <= length(extra_ord)) {
        names(wide)[names(wide) == extra_ord[k]] <- need[k]
      }
    }
  }
  for (cc in c("col1","col2","col3")) if (!cc %in% names(wide)) wide[[cc]] <- NA_character_
}
# ensure col1-3 exist and are character
for (cc in c("col1","col2","col3")) {
  if (!cc %in% names(wide)) wide[[cc]] <- NA_character_
  wide[[cc]] <- as.character(wide[[cc]])
}
final <- wide
# order final by descending atc_ord CMCLAS rowtype descending pref_ord display_term
final <- final %>%
  mutate(.k1 = sas_key(CMCLAS), .k2 = sas_key(display_term))
final <- final[order(-final$atc_ord, final$.k1, final$rowtype,
                     -final$pref_ord, final$.k2, na.last = TRUE, method = "radix"), , drop = FALSE]
final$.k1 <- NULL; final$.k2 <- NULL
final <- final[, c("atc_ord","CMCLAS","rowtype","pref_ord","display_term","col1","col2","col3")]

# --- Step 8: Generate output ---
# CSV of table body (with header N's in column labels)
csv_df <- final[, c("display_term","col1","col2","col3")]
names(csv_df) <- c("ATC Class / Preferred Name", col1hdr, col2hdr, col3hdr)
write.csv(csv_df, file.path(out, "t_cm.csv"), row.names = FALSE, na = "")

# Text rendering
hdr_line1 <- sprintf("%-45s | %-18s | %-18s | %-18s",
  "ATC Class / Preferred Name", col1hdr, col2hdr, col3hdr)
sep_line <- paste(rep("-", nchar(hdr_line1)), collapse = "")
title_block <- c(
  "Table 14.2.1",
  "Summary of Concomitant Medications by ATC Class and Preferred Name",
  "Safety Analysis Set",
  ""
)
col_block <- c(hdr_line1, sep_line)
body_lines <- sapply(seq_len(nrow(final)), function(i) {
  lab <- ifelse(is.na(final$display_term[i]), "", final$display_term[i])
  # bold ATC rows (rowtype==1) marked same numbers; keep plain
  c1 <- ifelse(is.na(final$col1[i]), "", final$col1[i])
  c2 <- ifelse(is.na(final$col2[i]), "", final$col2[i])
  c3 <- ifelse(is.na(final$col3[i]), "", final$col3[i])
  sprintf("%-45s | %-18s | %-18s | %-18s", lab, c1, c2, c3)
}, USE.NAMES = FALSE)
foot_block <- c(
  "",
  "WHO Drug Dictionary [Version]",
  "Subjects counted once per ATC class and once per preferred name within ATC class.",
  "Data cutoff: [DDMONYYYY].",
  "Source: ADaM datasets"
)
txt_all <- c(title_block, col_block, body_lines, foot_block)
writeLines(txt_all, file.path(out, "t_cm.txt"), useBytes = TRUE)

cat("t_cm completed: rows=", nrow(final), " cols col1-3 headers: ", col1hdr, " | ", col2hdr, " | ", col3hdr, "\n", sep = "")

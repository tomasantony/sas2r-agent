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

adam_dir <- "/bench/data/raw/own-synthetic/adam"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_hex <- function(v) {
  vapply(v, function(one) {
    if (is.na(one)) return("")
    s <- as.character(one)
    if (is.na(s) || s == "") return("")
    r <- charToRaw(enc2utf8(s))
    paste(sprintf("%02X", as.integer(r)), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}
sas_miss <- function(v) {
  vapply(v, function(one) {
    if (is.na(one)) return(0L)
    s <- as.character(one)
    if (is.na(s) || s == "") return(0L)
    1L
  }, integer(1), USE.NAMES = FALSE)
}
sas_order_idx <- function(v) {
  order(sas_miss(v), sas_hex(v), na.last = TRUE, method = "radix")
}
trim_c <- function(x) {
  vapply(x, function(one) {
    if (is.na(one)) return(NA_character_)
    s <- as.character(one)
    if (is.na(s)) return(NA_character_)
    # mimic SAS STRIP: trim leading/trailing blanks
    gsub("^\\s+|\\s+$", "", s, perl = TRUE)
  }, character(1), USE.NAMES = FALSE)
}

read_adam <- function(dir, name) {
  lname <- tolower(name)
  if (dir.exists(dir)) {
    fl <- list.files(dir, full.names = TRUE)
    base_noext <- tolower(tools::file_path_sans_ext(basename(fl)))
    ext <- tolower(tools::file_ext(fl))
    idx <- which(base_noext == lname & ext %in% c("sas7bdat", "xpt"))
    if (length(idx) > 0) {
      pref <- ifelse(ext[idx] == "sas7bdat", 0, 1)
      idx <- idx[order(pref)]
      for (f in fl[idx]) {
        e2 <- tolower(tools::file_ext(f))
        if (e2 == "xpt") {
          df <- haven::read_xpt(f)
        } else {
          df <- haven::read_sas(f)
        }
        return(as.data.frame(df, stringsAsFactors = FALSE))
      }
    }
  }
  stop(paste0("Cannot find dataset ", name, " in ", dir))
}

adsl_raw <- read_adam(adam_dir, "adsl")
adcm_raw <- read_adam(adam_dir, "adcm")
names(adsl_raw) <- toupper(names(adsl_raw))
names(adcm_raw) <- toupper(names(adcm_raw))

for (v in c("USUBJID","TRTA","TRTAN","SAFFL")) {
  if (!v %in% names(adsl_raw)) adsl_raw[[v]] <- NA_character_
}
for (v in c("USUBJID","TRTA","TRTAN","CMCLAS","CMDECOD","CMINDC","CMSTDTC","CMENDTC","CONCOMFL")) {
  if (!v %in% names(adcm_raw)) adcm_raw[[v]] <- NA_character_
}

adsl_raw$USUBJID <- trim_c(as.character(adsl_raw$USUBJID))
adsl_raw$TRTA <- trim_c(as.character(adsl_raw$TRTA))
adsl_raw$SAFFL <- trim_c(as.character(adsl_raw$SAFFL))
adcm_raw$USUBJID <- trim_c(as.character(adcm_raw$USUBJID))
adcm_raw$TRTA <- trim_c(as.character(adcm_raw$TRTA))
adcm_raw$CMCLAS <- trim_c(as.character(adcm_raw$CMCLAS))
adcm_raw$CMDECOD <- trim_c(as.character(adcm_raw$CMDECOD))
adcm_raw$CONCOMFL <- trim_c(as.character(adcm_raw$CONCOMFL))
# empty strings -> NA to mimic SAS missing
blank_to_na <- function(x) { x[!is.na(x) & x == ""] <- NA_character_; x }
adsl_raw$TRTA <- blank_to_na(adsl_raw$TRTA)
adcm_raw$TRTA <- blank_to_na(adcm_raw$TRTA)
adcm_raw$CMCLAS <- blank_to_na(adcm_raw$CMCLAS)
adcm_raw$CMDECOD <- blank_to_na(adcm_raw$CMDECOD)

if ("TRTAN" %in% names(adcm_raw)) adcm_raw$TRTAN <- suppressWarnings(as.numeric(adcm_raw$TRTAN))
if ("TRTAN" %in% names(adsl_raw)) adsl_raw$TRTAN <- suppressWarnings(as.numeric(adsl_raw$TRTAN))

# --- Step 1: Big N (m_counts: upcase(SAFFL)='Y', group by TRTA order by TRTA byte order) ---
adsl_pop_up <- adsl_raw %>% filter(!is.na(SAFFL) & toupper(SAFFL) == "Y")
bign_df <- adsl_pop_up %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
bign_df <- bign_df[sas_order_idx(bign_df$TRTA), , drop = FALSE]
bigntot <- n_distinct(adsl_pop_up$USUBJID)
trtlbl <- bign_df$TRTA
bignv <- bign_df$bign
hdrs_sorted <- paste0(trtlbl, " (N=", bignv, ")")
while (length(hdrs_sorted) < 3) hdrs_sorted <- c(hdrs_sorted, "Missing (N=0)")
col1hdr <- hdrs_sorted[1]; col2hdr <- hdrs_sorted[2]; col3hdr <- hdrs_sorted[3]

# --- Step 2: CM data (exact SAFFL='Y' and CONCOMFL='Y', inner join) ---
adsl_y <- adsl_raw %>% filter(!is.na(SAFFL) & SAFFL == "Y") %>% distinct(USUBJID)
adcm_c <- adcm_raw %>% filter(!is.na(CONCOMFL) & CONCOMFL == "Y")
cm <- dplyr::inner_join(adcm_c, adsl_y, by = "USUBJID")
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
atc_df <- atc_df[sas_order_idx(atc_df$CMCLAS), , drop = FALSE]
atc_tot <- atc_tot[sas_order_idx(atc_tot$CMCLAS), , drop = FALSE]
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
pref_rows <- dplyr::left_join(pref_df, pref_tot, by = c("CMCLAS","CMDECOD"), na_matches = "na", suffix = c("", ".pt"))
pref_rows <- dplyr::left_join(pref_rows, atc_tot %>% rename(atc_tot = cnt_tot), by = "CMCLAS", na_matches = "na")
pref_rows$display_term <- vapply(pref_rows$CMDECOD, function(x) {
  if (is.na(x)) "  " else paste0("  ", trim_c(as.character(x)))
}, character(1), USE.NAMES = FALSE)
pref_rows$pref_ord <- pref_rows$cnt_tot
pref_rows$atc_ord <- pref_rows$atc_tot
pref_rows$rowtype <- 2
pref_rows$cnt_tot <- NULL
pref_rows$atc_tot <- NULL

# --- Step 6: Combine and percentages ---
combined <- dplyr::bind_rows(any_df, atc_rows, pref_rows)
combined2 <- dplyr::left_join(combined, bign_df, by = "TRTA")
combined2$pct <- ifelse(!is.na(combined2$bign) & combined2$bign > 0,
                        combined2$cnt / combined2$bign * 100, NA_real_)
ord <- order(-combined2$atc_ord,
             sas_miss(combined2$CMCLAS), sas_hex(combined2$CMCLAS),
             combined2$rowtype,
             -combined2$pref_ord,
             sas_miss(combined2$display_term), sas_hex(combined2$display_term),
             sas_miss(combined2$TRTA), sas_hex(combined2$TRTA),
             na.last = TRUE, method = "radix")
combined2 <- combined2[ord, , drop = FALSE]

combined3 <- combined2
combined3$value <- vapply(seq_len(nrow(combined3)), function(i) {
  cnt <- combined3$cnt[i]; pct <- combined3$pct[i]
  if (is.na(cnt) || cnt <= 0) return("0")
  cnt_s <- trim_c(as.character(cnt))
  if (is.na(pct)) return(paste0(cnt_s, " (.)"))
  pct_r <- round_half_up(pct, 1)
  pct_s <- trim_c(sprintf("%.1f", pct_r))
  paste0(cnt_s, " (", pct_s, ")")
}, character(1), USE.NAMES = FALSE)

# --- Step 7: Transpose ---
ord2 <- order(-combined3$atc_ord,
              sas_miss(combined3$CMCLAS), sas_hex(combined3$CMCLAS),
              combined3$rowtype,
              -combined3$pref_ord,
              sas_miss(combined3$display_term), sas_hex(combined3$display_term),
              sas_miss(combined3$TRTA), sas_hex(combined3$TRTA),
              na.last = TRUE, method = "radix")
combined3 <- combined3[ord2, , drop = FALSE]
keycols <- paste(combined3$atc_ord,
                 sas_miss(combined3$CMCLAS), sas_hex(combined3$CMCLAS),
                 combined3$rowtype, combined3$pref_ord,
                 sas_miss(combined3$display_term), sas_hex(combined3$display_term),
                 sas_miss(combined3$TRTA), sas_hex(combined3$TRTA), sep = "|")
combined3 <- combined3[!duplicated(keycols), , drop = FALSE]

wide <- tidyr::pivot_wider(combined3,
  id_cols = c("atc_ord","CMCLAS","rowtype","pref_ord","display_term"),
  names_from = "TRTA", values_from = "value",
  values_fill = NA_character_)
wide <- as.data.frame(wide, stringsAsFactors = FALSE, check.names = FALSE)

sanitize <- function(x) gsub("[^A-Za-z0-9_]", "_", x, perl = TRUE)
old_names <- names(wide)
trt_vals <- setdiff(old_names, c("atc_ord","CMCLAS","rowtype","pref_ord","display_term"))
sas_colnames <- paste0("col", vapply(trt_vals, sanitize, character(1), USE.NAMES = FALSE))
names(wide)[match(trt_vals, names(wide))] <- sas_colnames

rename_map <- c("colPlacebo" = "col1",
                "colDrug_X_Low_Dose" = "col2",
                "colDrug_X_High_Dose" = "col3")
for (src in names(rename_map)) {
  dst <- rename_map[[src]]
  if (src %in% names(wide)) {
    names(wide)[names(wide) == src] <- dst
  }
}
if (!all(c("col1","col2","col3") %in% names(wide))) {
  idc <- c("atc_ord","CMCLAS","rowtype","pref_ord","display_term","col1","col2","col3")
  extra <- setdiff(names(wide), idc)
  if (length(extra) > 0) {
    extra_ord <- extra[order(sas_miss(extra), sas_hex(extra), method = "radix")]
    need <- setdiff(c("col1","col2","col3"), names(wide))
    for (k in seq_along(need)) {
      if (k <= length(extra_ord)) {
        names(wide)[names(wide) == extra_ord[k]] <- need[k]
      }
    }
  }
  for (cc in c("col1","col2","col3")) if (!cc %in% names(wide)) wide[[cc]] <- NA_character_
}
for (cc in c("col1","col2","col3")) {
  if (!cc %in% names(wide)) wide[[cc]] <- NA_character_
  wide[[cc]] <- as.character(wide[[cc]])
}
final <- wide
final <- final[order(-final$atc_ord,
                     sas_miss(final$CMCLAS), sas_hex(final$CMCLAS),
                     final$rowtype,
                     -final$pref_ord,
                     sas_miss(final$display_term), sas_hex(final$display_term),
                     na.last = TRUE, method = "radix"), , drop = FALSE]
final <- final[, c("atc_ord","CMCLAS","rowtype","pref_ord","display_term","col1","col2","col3")]

# --- Step 8: outputs ---
csv_df <- final[, c("display_term","col1","col2","col3")]
names(csv_df) <- c("ATC Class / Preferred Name", col1hdr, col2hdr, col3hdr)
write.csv(csv_df, file.path(out, "t_cm.csv"), row.names = FALSE, na = "")

# run-date footnote in SAS DATE9. style to match SAS numbers (year etc.)
run_date9 <- toupper(format(Sys.Date(), "%d%b%Y"))
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
body_lines <- vapply(seq_len(nrow(final)), function(i) {
  lab <- ifelse(is.na(final$display_term[i]), "", final$display_term[i])
  c1 <- ifelse(is.na(final$col1[i]), "", final$col1[i])
  c2 <- ifelse(is.na(final$col2[i]), "", final$col2[i])
  c3 <- ifelse(is.na(final$col3[i]), "", final$col3[i])
  sprintf("%-45s | %-18s | %-18s | %-18s", lab, c1, c2, c3)
}, character(1), USE.NAMES = FALSE)
foot_block <- c(
  "",
  "WHO Drug Dictionary [Version]",
  "Subjects counted once per ATC class and once per preferred name within ATC class.",
  "Data cutoff: [DDMONYYYY].",
  "Source: ADaM datasets",
  paste0("Program: t_cm.sas | Run: ", run_date9)
)
txt_all <- c(title_block, col_block, body_lines, foot_block)
writeLines(txt_all, file.path(out, "t_cm.txt"), useBytes = TRUE)

cat("t_cm completed: rows=", nrow(final), " headers: ", col1hdr, " | ", col2hdr, " | ", col3hdr, "\n", sep = "")

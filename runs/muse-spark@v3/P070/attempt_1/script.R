out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
})

round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  s <- sign(x)
  a <- abs(x)
  r <- floor(a * mult + 0.5 + 1e-9) / mult
  s * r
}
sas_pct_5_1 <- function(x) {
  r <- round_half_up(x, 1)
  sprintf("%.1f", r)
}
strip_labels <- function(df) {
  nm <- names(df)
  names(df) <- toupper(nm)
  for (cn in names(df)) {
    col <- df[[cn]]
    if (inherits(col, "haven_labelled")) {
      attr(col, "labels") <- NULL
      attr(col, "label") <- NULL
      attr(col, "format.sas") <- NULL
      class(col) <- setdiff(class(col), c("haven_labelled", "haven_labelled_spss"))
      df[[cn]] <- col
    }
  }
  df
}
read_any <- function(p) {
  if (grepl("\\.xpt$", p, ignore.case = TRUE)) haven::read_xpt(p)
  else if (grepl("\\.sas7bdat$", p, ignore.case = TRUE)) haven::read_sas(p)
  else if (grepl("\\.csv$", p, ignore.case = TRUE)) suppressMessages(readr::read_csv(p, show_col_types = FALSE) %>% as.data.frame())
  else haven::read_sas(p)
}
sas_order_key <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("\001")
    cp <- utf8ToInt(as.character(v))
    paste(sprintf("%05d", cp), collapse = "-")
  }, USE.NAMES = FALSE)
}
sas_order <- function(x) order(sas_order_key(x), na.last = FALSE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
adsl_files <- list.files(adam_dir, pattern = "(?i)adsl", full.names = TRUE, recursive = TRUE)
adcm_files <- list.files(adam_dir, pattern = "(?i)adcm", full.names = TRUE, recursive = TRUE)
adsl_path <- adsl_files[1]
adcm_path <- adcm_files[1]
stopifnot(!is.na(adsl_path), !is.na(adcm_path))

adsl_raw <- read_any(adsl_path)
adcm_raw <- read_any(adcm_path)
adsl <- strip_labels(as.data.frame(adsl_raw, stringsAsFactors = FALSE))
adcm <- strip_labels(as.data.frame(adcm_raw, stringsAsFactors = FALSE))

stopifnot(all(c("USUBJID","TRTA","TRTAN","SAFFL") %in% names(adsl)))
stopifnot(all(c("USUBJID","CONCOMFL") %in% names(adcm)))
stopifnot("CMCLAS" %in% names(adcm))
stopifnot("CMDECOD" %in% names(adcm))

adsl$USUBJID <- as.character(adsl$USUBJID)
adsl$TRTA <- as.character(adsl$TRTA)
adsl$TRTAN <- suppressWarnings(as.numeric(adsl$TRTAN))
adsl$SAFFL <- as.character(adsl$SAFFL)
adcm$USUBJID <- as.character(adcm$USUBJID)
adcm$CONCOMFL <- as.character(adcm$CONCOMFL)
adcm$CMCLAS <- as.character(adcm$CMCLAS)
adcm$CMDECOD <- as.character(adcm$CMDECOD)
if ("TRTA" %in% names(adcm)) adcm$TRTA <- as.character(adcm$TRTA)
if ("TRTAN" %in% names(adcm)) adcm$TRTAN <- suppressWarnings(as.numeric(adcm$TRTAN))

# Step 1: Big N (m_counts) : order by TRTA SAS order
bign_df <- adsl %>%
  filter(!is.na(SAFFL) & toupper(SAFFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
bign_df <- bign_df[sas_order(bign_df$TRTA), , drop = FALSE]
bigntot <- adsl %>% filter(!is.na(SAFFL) & toupper(SAFFL) == "Y") %>% summarise(n = n_distinct(USUBJID)) %>% pull(n)
trtlbl_alpha <- bign_df$TRTA
bign_alpha <- bign_df$bign
ntrt <- nrow(bign_df)
col_hdr_alpha <- paste0(trtlbl_alpha, " (N=", bign_alpha, ")")

# TRTAN order for data columns (rename mapping Placebo->col1 etc = TRTAN order)
trt_trtan <- adsl %>%
  filter(!is.na(SAFFL) & toupper(SAFFL) == "Y") %>%
  select(TRTA, TRTAN) %>%
  distinct() %>%
  arrange(TRTAN) %>%
  as.data.frame()
# ensure 3 rows; if TRTAN missing, fall back to SAS order
if (any(is.na(trt_trtan$TRTAN)) || nrow(trt_trtan) != ntrt) {
  trt_trtan <- data.frame(TRTA = trtlbl_alpha, TRTAN = seq_len(ntrt), stringsAsFactors = FALSE)
} else {
  trt_trtan <- trt_trtan[sas_order(trt_trtan$TRTA) != 0 | TRUE, , drop = FALSE]
  trt_trtan <- trt_trtan[order(trt_trtan$TRTAN), , drop = FALSE]
}
# col1..col3 correspond to TRTAN order
trt_col1 <- trt_trtan$TRTA[1]
trt_col2 <- if (ntrt >= 2) trt_trtan$TRTA[2] else NA
trt_col3 <- if (ntrt >= 3) trt_trtan$TRTA[3] else NA

# Step 2: cm : inner join adcm with safety adsl, CONCOMFL=Y
safety_ids <- adsl %>% filter(!is.na(SAFFL) & SAFFL == "Y") %>% select(USUBJID) %>% distinct()
cm <- adcm %>%
  inner_join(safety_ids, by = "USUBJID") %>%
  filter(!is.na(CONCOMFL) & CONCOMFL == "Y")
# TRTA from adcm if present else from adsl
if (!("TRTA" %in% names(cm)) || all(is.na(cm$TRTA))) {
  cm <- cm %>% select(-any_of(c("TRTA","TRTAN"))) %>%
    left_join(adsl %>% select(USUBJID, TRTA, TRTAN), by = "USUBJID")
}
cm <- cm %>% select(USUBJID, TRTA, TRTAN, CMCLAS, CMDECOD) %>%
  filter(!is.na(TRTA))
stopifnot(all(c("USUBJID","TRTA","CMCLAS","CMDECOD") %in% names(cm)))
stopifnot(nrow(cm) > 0)

# Step 3: Any
any_df <- cm %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(display_term = "Any Concomitant Medication",
         atc_ord = 0, pref_ord = 0, rowtype = 0,
         CMCLAS = NA_character_, CMDECOD = NA_character_) %>%
  as.data.frame()

# Step 4: ATC
atc_df <- cm %>%
  group_by(TRTA, CMCLAS) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
atc_tot <- cm %>%
  group_by(CMCLAS) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
atc_rows <- atc_df %>%
  left_join(atc_tot, by = "CMCLAS") %>%
  mutate(display_term = CMCLAS,
         atc_ord = cnt_tot,
         pref_ord = 0,
         rowtype = 1,
         CMDECOD = NA_character_) %>%
  select(TRTA, CMCLAS, CMDECOD, cnt, display_term, atc_ord, pref_ord, rowtype) %>%
  as.data.frame()

# Step 5: Pref
pref_df <- cm %>%
  group_by(TRTA, CMCLAS, CMDECOD) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
pref_tot <- cm %>%
  group_by(CMCLAS, CMDECOD) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
pref_rows <- pref_df %>%
  left_join(pref_tot %>% rename(pref_ord = cnt_tot), by = c("CMCLAS","CMDECOD")) %>%
  left_join(atc_tot %>% rename(atc_ord = cnt_tot), by = "CMCLAS") %>%
  mutate(display_term = paste0("  ", stringr::str_trim(CMDECOD)),
         rowtype = 2) %>%
  select(TRTA, CMCLAS, CMDECOD, cnt, display_term, atc_ord, pref_ord, rowtype) %>%
  as.data.frame()

combined <- bind_rows(
  any_df %>% select(TRTA, CMCLAS, CMDECOD, cnt, display_term, atc_ord, pref_ord, rowtype),
  atc_rows,
  pref_rows
)

combined2 <- combined %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(pct = ifelse(!is.na(bign) & bign > 0, cnt / bign * 100, NA_real_)) %>%
  as.data.frame()
# order by atc_ord desc, CMCLAS, rowtype, pref_ord desc, display_term, TRTA
combined2$CMCLAS_sort <- ifelse(is.na(combined2$CMCLAS), "", combined2$CMCLAS)
combined2 <- combined2[order(-combined2$atc_ord,
                             sas_order_key(combined2$CMCLAS_sort),
                             combined2$rowtype,
                             -combined2$pref_ord,
                             sas_order_key(combined2$display_term),
                             sas_order_key(combined2$TRTA)), , drop = FALSE]

combined3 <- combined2
combined3$value <- ifelse(!is.na(combined3$cnt) & combined3$cnt > 0,
  paste0(stringr::str_trim(format(combined3$cnt, width = 8)),
         " (", stringr::str_trim(sas_pct_5_1(combined3$pct)), ")"),
  "0")
# sort nodupkey by descending atc_ord CMCLAS rowtype descending pref_ord display_term TRTA
combined3 <- combined3[order(-combined3$atc_ord,
                             sas_order_key(combined3$CMCLAS_sort),
                             combined3$rowtype,
                             -combined3$pref_ord,
                             sas_order_key(combined3$display_term),
                             sas_order_key(combined3$TRTA)), , drop = FALSE]
combined3 <- combined3[!duplicated(paste(combined3$atc_ord, combined3$CMCLAS, combined3$rowtype, combined3$pref_ord, combined3$display_term, combined3$TRTA)), , drop = FALSE]

# Transpose: by atc_ord CMCLAS rowtype pref_ord display_term ; id TRTA
wide <- combined3 %>%
  select(atc_ord, CMCLAS, rowtype, pref_ord, display_term, TRTA, value) %>%
  tidyr::pivot_wider(names_from = TRTA, values_from = value, values_fill = "") %>%
  as.data.frame()
# rename TRTAN-order columns to col1..col3
col_map <- c()
col_map[trt_col1] <- "col1"
if (!is.na(trt_col2)) col_map[trt_col2] <- "col2"
if (!is.na(trt_col3)) col_map[trt_col3] <- "col3"
for (tn in names(col_map)) {
  if (tn %in% names(wide)) names(wide)[names(wide) == tn] <- col_map[[tn]]
}
for (cc in c("col1","col2","col3")) if (!(cc %in% names(wide))) wide[[cc]] <- ""
wide$CMCLAS_sort <- ifelse(is.na(wide$CMCLAS), "", wide$CMCLAS)
wide <- wide[order(-wide$atc_ord,
                   sas_order_key(wide$CMCLAS_sort),
                   wide$rowtype,
                   -wide$pref_ord,
                   sas_order_key(wide$display_term)), , drop = FALSE]
final <- wide %>% select(atc_ord, CMCLAS, rowtype, pref_ord, display_term, col1, col2, col3)
final$col1[is.na(final$col1)] <- ""
final$col2[is.na(final$col2)] <- ""
final$col3[is.na(final$col3)] <- ""

# Headers as SAS would display: col1hdr.. in alphabetical (bign_df) order
hdr1 <- if (ntrt >= 1) col_hdr_alpha[1] else ""
hdr2 <- if (ntrt >= 2) col_hdr_alpha[2] else ""
hdr3 <- if (ntrt >= 3) col_hdr_alpha[3] else ""

write.csv(final, file.path(out, "t_cm.csv"), row.names = FALSE, na = "")

# Text rendering
run_date <- format(Sys.Date(), "%d%b%Y")
run_date <- toupper(run_date)
t1 <- "[Sponsor Name]                                                  Page 1 of 1"
t2 <- "Protocol: [Protocol Number]"
t4 <- "Table 14.2.1"
t5 <- "Summary of Concomitant Medications by ATC Class and Preferred Name"
t6 <- "Safety Analysis Set"
f1 <- "Data cutoff: [DDMONYYYY]."
f2 <- "Source: ADaM datasets"
f3 <- paste0("Program: P070_t_cm.sas | Run: ", run_date)
f4 <- "WHO Drug Dictionary [Version]"
f5 <- "Subjects counted once per ATC class and once per preferred name within ATC class."

con <- file(file.path(out, "t_cm.txt"), open = "wt")
cat(t1, "\n", file = con)
cat(t2, "\n", file = con)
cat(" \n", file = con)
cat(t4, "\n", file = con)
cat(t5, "\n", file = con)
cat(t6, "\n", file = con)
cat("\n", file = con)
header_line <- paste("ATC Class / Preferred Name", hdr1, hdr2, hdr3, sep = " | ")
cat(header_line, "\n", file = con)
cat(paste(rep("-", nchar(header_line)), collapse = ""), "\n", file = con)
for (i in seq_len(nrow(final))) {
  line <- paste(final$display_term[i], final$col1[i], final$col2[i], final$col3[i], sep = " | ")
  cat(line, "\n", file = con)
}
cat("\n", file = con)
cat(paste0(f1, "\n"), file = con)
cat(paste0(f2, "\n"), file = con)
cat(paste0(f3, "\n"), file = con)
cat(paste0(f4, "\n"), file = con)
cat(paste0(f5, "\n"), file = con)
close(con)

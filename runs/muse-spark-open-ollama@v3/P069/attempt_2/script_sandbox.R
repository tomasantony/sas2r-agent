out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_put_wd <- function(x, w, d) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    r <- sas_round(v, d)
    trimws(formatC(r, format = "f", digits = d, flag = ""))
  }, character(1))
}
sas_put_cnt <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    trimws(formatC(v, format = "f", digits = 0, flag = ""))
  }, character(1))
}

adam_path <- "/bench/data/raw/own-synthetic/adam"
adam_files <- list.files(adam_path, recursive = TRUE, full.names = TRUE)

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

find_adam <- function(name) {
  pat <- paste0("^", name, "\\.(sas7bdat|xpt)$")
  f <- adam_files[grepl(pat, basename(adam_files), ignore.case = TRUE)]
  if (length(f) == 0) {
    f <- adam_files[grepl(paste0("^", name, "\\."), basename(adam_files), ignore.case = TRUE)]
  }
  if (length(f) == 0) stop(paste("Not found:", name, " in ", adam_path))
  f[1]
}
read_adam_file <- function(f) {
  if (grepl("\\.xpt$", f, ignore.case = TRUE)) haven::read_xpt(f) else haven::read_sas(f)
}

adsl_file <- find_adam("adsl")
adae_file <- find_adam("adae")
adsl <- read_adam_file(adsl_file)
adae <- read_adam_file(adae_file)
adsl <- adsl %>% mutate(across(where(~ inherits(., "haven_labelled")), haven::as_factor)) %>%
  mutate(across(where(is.factor), as.character))
adae <- adae %>% mutate(across(where(~ inherits(., "haven_labelled")), haven::as_factor)) %>%
  mutate(across(where(is.factor), as.character))
names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))
stopifnot("TRTA" %in% names(adsl))
stopifnot("SAFFL" %in% names(adsl))
stopifnot("USUBJID" %in% names(adsl))
stopifnot("TRTA" %in% names(adae))
stopifnot("USUBJID" %in% names(adae))
stopifnot("TRTEMFL" %in% names(adae))

adsl_saffl <- adsl %>% filter(toupper(SAFFL) == "Y")
bign_df <- adsl_saffl %>%
  group_by(TRTA) %>%
  summarise(BIGN = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)
trt_order <- bign_df$TRTA
ntrt <- nrow(bign_df)
bign_tot <- n_distinct(adsl_saffl$USUBJID)
bign_lookup <- bign_df %>% select(TRTA, BIGN)

ae_category <- function(where_filter = NULL, label, ord) {
  adae_f <- adae %>% filter(TRTEMFL == "Y")
  safety_ids <- unique(adsl_saffl$USUBJID)
  adae_f <- adae_f %>% filter(USUBJID %in% safety_ids)
  if (!is.null(where_filter)) {
    adae_f <- adae_f %>% filter(where_filter(.))
  }
  if (nrow(adae_f) == 0) {
    return(tibble(TRTA = character(), CNT = integer(), CATEGORY = character(), ORD = integer()))
  }
  res <- adae_f %>%
    group_by(TRTA) %>%
    summarise(CNT = n_distinct(USUBJID), .groups = "drop") %>%
    mutate(CATEGORY = label, ORD = ord)
  res
}
related_filter <- function(df) toupper(df$AEREL) %in% c("POSSIBLE", "PROBABLE", "DEFINITE", "RELATED")
grade3_filter <- function(df) {
  vals <- suppressWarnings(as.numeric(df$AETOXGR))
  !is.na(vals) & vals >= 3
}
ser_filter <- function(df) df$AESER == "Y"
related_ser_filter <- function(df) df$AESER == "Y" & toupper(df$AEREL) %in% c("POSSIBLE","PROBABLE","DEFINITE","RELATED")
disc_filter <- function(df) df$AEACN == "DRUG WITHDRAWN"
dose_mod_filter <- function(df) df$AEACN %in% c("DOSE REDUCED","DRUG INTERRUPTED","DOSE INCREASED")
death_filter <- function(df) df$AEOUT == "FATAL"

ae1 <- ae_category(NULL, "Subjects with at least one TEAE", 1)
ae2 <- ae_category(related_filter, "  Related TEAE", 2)
ae3 <- ae_category(grade3_filter, "  Grade >=3 TEAE", 3)
ae4 <- ae_category(ser_filter, "  Serious Adverse Event", 4)
ae5 <- ae_category(related_ser_filter, "  Related Serious Adverse Event", 5)
ae6 <- ae_category(disc_filter, "  TEAE Leading to Study Drug Discontinuation", 6)
ae7 <- ae_category(dose_mod_filter, "  TEAE Leading to Dose Modification", 7)
ae8 <- ae_category(death_filter, "  TEAE with Outcome of Death", 8)

distinct_teae_ids <- adae %>% filter(TRTEMFL == "Y") %>% distinct(USUBJID) %>% pull(USUBJID)
ae0 <- adsl_saffl %>%
  filter(!USUBJID %in% distinct_teae_ids) %>%
  group_by(TRTA) %>%
  summarise(CNT = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(CATEGORY = "Subjects with no TEAE", ORD = 0)

all_ae <- bind_rows(ae0, ae1, ae2, ae3, ae4, ae5, ae6, ae7, ae8)
all_ae2 <- all_ae %>%
  left_join(bign_lookup, by = "TRTA") %>%
  mutate(PCT = ifelse(BIGN > 0, CNT / BIGN * 100, NA_real_)) %>%
  arrange(ORD, TRTA)

all_ae3 <- all_ae2 %>%
  mutate(VALUE = ifelse(CNT > 0,
                         paste0(sas_put_cnt(CNT), " (", sas_put_wd(PCT, 5, 1), ")"),
                         "0"))

trt_levels <- bign_df$TRTA
wide <- all_ae3 %>%
  select(ORD, CATEGORY, TRTA, VALUE) %>%
  pivot_wider(names_from = TRTA, values_from = VALUE)
for (trt in trt_levels) {
  if (!trt %in% names(wide)) wide[[trt]] <- NA_character_
}
wide <- wide %>% select(ORD, CATEGORY, all_of(trt_levels)) %>% arrange(ORD)
report <- wide
for (trt in trt_levels) {
  report[[trt]][is.na(report[[trt]])] <- "0"
  report[[trt]][report[[trt]] == ""] <- "0"
}
colnames(report)[3:(2+ntrt)] <- paste0("col", seq_len(ntrt))
display_df <- report %>% select(ORD, CATEGORY, starts_with("col")) %>% arrange(ORD)

sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
title1 <- paste0(sponsor, "  Page 1 of 1")
title2 <- paste0("Protocol: ", protocol)
title4 <- "Table 14.3.1"
title5 <- "Overall Summary of Treatment-Emergent Adverse Events"
title6 <- "Safety Analysis Set"
foot1 <- paste0("Data cutoff: ", cutdate, ".")
foot2 <- "Source: ADaM datasets"
foot3 <- paste0("Program: P069_t_ae_sum.sas | Run: ", format(Sys.Date(), "%d%b%Y"), " ", format(Sys.time(), "%H:%M"))
foot4 <- "TEAE = Treatment-Emergent Adverse Event; SAE = Serious Adverse Event"
foot5 <- "Subjects are counted once per category."

hdr_labels <- sapply(seq_len(ntrt), function(i) {
  trt <- trt_levels[i]
  bign <- bign_df$BIGN[bign_df$TRTA == trt]
  paste0(trt, " (N=", bign, ")")
})
names(hdr_labels) <- paste0("col", seq_len(ntrt))

write.csv(display_df, file.path(out, "t_ae_sum.csv"), row.names = FALSE, na = "")

pad_right <- function(x, width) sprintf(paste0("%-", width, "s"), x)
pad_center <- function(x, width) {
  x <- as.character(x)
  n <- nchar(x)
  if (n >= width) return(substr(x, 1, width))
  left <- floor((width - n)/2)
  right <- width - n - left
  paste0(strrep(" ", left), x, strrep(" ", right))
}
txt_lines <- c(
  title1,
  title2,
  "",
  title4,
  title5,
  title6,
  "",
  paste0(rep("-", 100), collapse = ""),
  paste0(
    pad_right("", 4),
    pad_right("Category", 50),
    paste(sapply(seq_len(ntrt), function(i) pad_center(hdr_labels[i], 20)), collapse = "")
  ),
  paste0(rep("-", 100), collapse = "")
)
for (i in seq_len(nrow(display_df))) {
  row <- display_df[i, ]
  cat_label <- row$CATEGORY
  line <- paste0(
    pad_right("", 4),
    pad_right(cat_label, 50),
    paste(sapply(seq_len(ntrt), function(j) {
      val <- row[[paste0("col", j)]]
      pad_center(val, 20)
    }), collapse = "")
  )
  txt_lines <- c(txt_lines, line)
}
txt_lines <- c(txt_lines,
  paste0(rep("-", 100), collapse = ""),
  "",
  foot1,
  foot2,
  foot3,
  foot4,
  foot5,
  "",
  paste0("Total N (Safety Set): ", bign_tot),
  paste0("Treatment groups: ", paste(paste0(trt_levels, " N=", bign_df$BIGN), collapse = "; "))
)
writeLines(txt_lines, file.path(out, "t_ae_sum.txt"))
writeLines(txt_lines, file.path(out, "t_ae_sum.rtf"))
cat("P069 t_ae_sum completed\n")
print(bign_df)
cat("Report rows:", nrow(display_df), "\n")

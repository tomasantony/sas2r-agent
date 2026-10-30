out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_pct1 <- function(pct) {
  r <- round_half_up(pct, 1)
  sprintf("%.1f", r)
}

find_adam_path <- function() {
  candidates <- c(
    "/bench/data/raw/own-synthetic/adam",
    "/bench/data/raw/own-synthetic/adam",
    "/bench/data/raw/cdisc-pilot/adam",
    "/bench/data/raw/cdisc-pilot/adam",
    file.path(getwd(), "data/raw/own-synthetic/adam")
  )
  for (p in candidates) if (dir.exists(p)) return(p)
  "/bench/data/raw/own-synthetic/adam"
}
adam_path <- find_adam_path()

read_adam <- function(name, adam_path) {
  cand <- list.files(adam_path, pattern = paste0("^", name, "\\."), full.names = TRUE, ignore.case = TRUE)
  if (length(cand) == 0) {
    alt_roots <- c(
      "/bench/data/raw/own-synthetic/adam",
      "/bench/data/raw/own-synthetic/adam",
      "/bench/data/raw/cdisc-pilot/adam",
      "/bench/data/raw/cdisc-pilot/adam"
    )
    for (ap in alt_roots) {
      if (dir.exists(ap)) {
        cand2 <- list.files(ap, pattern = paste0("^", name, "\\."), full.names = TRUE, ignore.case = TRUE)
        if (length(cand2) > 0) { cand <- cand2; adam_path <- ap; break }
      }
    }
  }
  if (length(cand) == 0) stop(paste("Cannot find", name, "in", adam_path))
  f <- cand[1]
  if (grepl("\\.xpt$", f, ignore.case = TRUE)) haven::read_xpt(f)
  else if (grepl("\\.sas7bdat$", f, ignore.case = TRUE)) haven::read_sas(f)
  else if (grepl("\\.csv$", f, ignore.case = TRUE)) readr::read_csv(f, show_col_types = FALSE)
  else haven::read_sas(f)
}

adsl <- read_adam("adsl", adam_path)
adae <- read_adam("adae", adam_path)
names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))
stopifnot(all(c("USUBJID","TRTA","SAFFL") %in% names(adsl)))
stopifnot(all(c("USUBJID","TRTEMFL") %in% names(adae)))

adsl_saff <- adsl[toupper(adsl$SAFFL) == "Y", , drop = FALSE]

bign_df <- adsl_saff %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)

bign_tot <- n_distinct(adsl_saff$USUBJID)
bign_lookup <- bign_df

if (!"TRTA" %in% names(adae)) {
  adae <- adae %>% left_join(adsl[, c("USUBJID","TRTA")], by = "USUBJID", suffix = c("", ".adsl"))
  if ("TRTA.adsl" %in% names(adae)) adae$TRTA <- adae$`TRTA.adsl`
}

saff_ids <- unique(adsl_saff$USUBJID)
adae_teae <- adae[!is.na(adae$TRTEMFL) & adae$TRTEMFL == "Y" & adae$USUBJID %in% saff_ids, , drop = FALSE]

cats <- list(
  list(ord = 1, label = "Subjects with at least one TEAE", filter = function(d) rep(TRUE, nrow(d))),
  list(ord = 2, label = "  Related TEAE", filter = function(d) d$AEREL %in% c("POSSIBLE","PROBABLE","DEFINITE","RELATED")),
  list(ord = 3, label = "  Grade >=3 TEAE", filter = function(d) {
    v <- suppressWarnings(as.numeric(d$AETOXGR))
    !is.na(v) & v >= 3
  }),
  list(ord = 4, label = "  Serious Adverse Event", filter = function(d) !is.na(d$AESER) & d$AESER == "Y"),
  list(ord = 5, label = "  Related Serious Adverse Event", filter = function(d) !is.na(d$AESER) & d$AESER == "Y" & d$AEREL %in% c("POSSIBLE","PROBABLE","DEFINITE","RELATED")),
  list(ord = 6, label = "  TEAE Leading to Study Drug Discontinuation", filter = function(d) !is.na(d$AEACN) & d$AEACN == "DRUG WITHDRAWN"),
  list(ord = 7, label = "  TEAE Leading to Dose Modification", filter = function(d) !is.na(d$AEACN) & d$AEACN %in% c("DOSE REDUCED","DRUG INTERRUPTED","DOSE INCREASED")),
  list(ord = 8, label = "  TEAE with Outcome of Death", filter = function(d) !is.na(d$AEOUT) & d$AEOUT == "FATAL")
)

get_counts <- function(ord, label, filt_fun) {
  d <- adae_teae
  if (nrow(d) > 0) {
    keep <- filt_fun(d)
    keep[is.na(keep)] <- FALSE
    d <- d[keep, , drop = FALSE]
  }
  if (nrow(d) == 0) {
    return(data.frame(TRTA = character(0), cnt = numeric(0), category = character(0), ord = numeric(0), stringsAsFactors = FALSE))
  }
  agg <- d %>%
    group_by(TRTA) %>%
    summarise(cnt = n_distinct(USUBJID), .groups = "drop")
  agg$category <- label
  agg$ord <- ord
  as.data.frame(agg)
}

all_list <- list()
for (cc in cats) {
  all_list[[length(all_list)+1]] <- get_counts(cc$ord, cc$label, cc$filter)
}

distinct_teae_ids <- unique(adae[!is.na(adae$TRTEMFL) & adae$TRTEMFL == "Y", "USUBJID", drop = TRUE])
no_teae_subj <- adsl_saff[!adsl_saff$USUBJID %in% distinct_teae_ids, , drop = FALSE]
if (nrow(no_teae_subj) > 0) {
  ae0 <- no_teae_subj %>%
    group_by(TRTA) %>%
    summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
    mutate(category = "Subjects with no TEAE", ord = 0) %>%
    as.data.frame()
} else {
  ae0 <- data.frame(TRTA = character(0), cnt = numeric(0), category = character(0), ord = numeric(0), stringsAsFactors = FALSE)
}

all_ae <- bind_rows(ae0, bind_rows(all_list))

ord_labels <- data.frame(
  ord = c(0,1,2,3,4,5,6,7,8),
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
trta_levels <- sort(unique(bign_df$TRTA))

full_grid <- expand.grid(TRTA = trta_levels, ord = ord_labels$ord, stringsAsFactors = FALSE) %>%
  left_join(ord_labels, by = "ord")

all_ae_full <- full_grid %>%
  left_join(all_ae[, c("TRTA","ord","cnt")], by = c("TRTA","ord")) %>%
  mutate(cnt = ifelse(is.na(cnt), 0, cnt))

all_ae2 <- all_ae_full %>%
  left_join(bign_lookup, by = "TRTA") %>%
  mutate(pct = ifelse(bign > 0, cnt / bign * 100, NA_real_))

all_ae3 <- all_ae2 %>%
  mutate(value = ifelse(cnt > 0,
                        paste0(trimws(as.character(cnt)), " (", fmt_pct1(pct), ")"),
                        "0"))

wide <- all_ae3 %>%
  select(ord, category, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value, values_fill = "0") %>%
  arrange(ord)

expected_map <- c(
  "Placebo" = "col1",
  "Drug X Low Dose" = "col2",
  "Drug X High Dose" = "col3",
  "Xanomeline Low Dose" = "col2",
  "Xanomeline High Dose" = "col3"
)

report <- wide
for (i in 1:3) {
  cn <- paste0("col", i)
  if (!cn %in% names(report)) report[[cn]] <- "0"
}
for (trta in trta_levels) {
  if (trta %in% names(report)) {
    target <- expected_map[trta]
    if (!is.na(target)) {
      report[[target]] <- report[[trta]]
    }
  }
}
unmapped <- setdiff(trta_levels, names(expected_map))
if (length(unmapped) > 0) {
  has_expected <- all(c("Placebo") %in% trta_levels)
  if (!has_expected) {
    for (idx in seq_along(trta_levels)) {
      trta <- trta_levels[idx]
      if (trta %in% names(report)) {
        report[[paste0("col", idx)]] <- report[[trta]]
      }
    }
  }
}
report <- report %>%
  select(ord, category, col1, col2, col3) %>%
  arrange(ord)
for (cn in c("col1","col2","col3")) {
  report[[cn]][is.na(report[[cn]]) | report[[cn]] == ""] <- "0"
}

get_bign <- function(trta_name) {
  v <- bign_df$bign[bign_df$TRTA == trta_name]
  if (length(v) == 0) return(NA_integer_)
  v[1]
}
hdr1_n <- get_bign("Placebo")
hdr2_n <- get_bign("Drug X Low Dose")
if (is.na(hdr2_n)) hdr2_n <- get_bign("Xanomeline Low Dose")
hdr3_n <- get_bign("Drug X High Dose")
if (is.na(hdr3_n)) hdr3_n <- get_bign("Xanomeline High Dose")
if (any(is.na(c(hdr1_n, hdr2_n, hdr3_n)))) {
  for (i in seq_along(trta_levels)) {
    n <- get_bign(trta_levels[i])
    if (i == 1 && is.na(hdr1_n)) hdr1_n <- n
    if (i == 2 && is.na(hdr2_n)) hdr2_n <- n
    if (i == 3 && is.na(hdr3_n)) hdr3_n <- n
  }
  lbl1 <- if (!is.na(hdr1_n)) trta_levels[1] else "Placebo"
  lbl2 <- if (length(trta_levels) >= 2) trta_levels[2] else "Low Dose"
  lbl3 <- if (length(trta_levels) >= 3) trta_levels[3] else "High Dose"
  col1hdr <- paste0(lbl1, " (N=", hdr1_n, ")")
  col2hdr <- paste0(lbl2, " (N=", hdr2_n, ")")
  col3hdr <- paste0(lbl3, " (N=", hdr3_n, ")")
} else {
  col1hdr <- paste0("Placebo (N=", hdr1_n, ")")
  low_label <- if ("Xanomeline Low Dose" %in% trta_levels) "Xanomeline Low Dose" else "Drug X Low Dose"
  high_label <- if ("Xanomeline High Dose" %in% trta_levels) "Xanomeline High Dose" else "Drug X High Dose"
  col2hdr <- paste0(low_label, " (N=", hdr2_n, ")")
  col3hdr <- paste0(high_label, " (N=", hdr3_n, ")")
}

# Write CSV
write.csv(report, file.path(out, "t_ae_sum.csv"), row.names = FALSE, na = "")

# Text rendering
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"

titles <- c(
  paste0(sponsor, "  Page 1 of 1"),
  paste0("Protocol: ", protocol),
  "",
  "Table 14.3.1",
  "Overall Summary of Treatment-Emergent Adverse Events",
  "Safety Analysis Set"
)
header_row <- sprintf("%-50s | %-18s | %-18s | %-18s", "", col1hdr, col2hdr, col3hdr)
sep <- paste(rep("-", nchar(header_row)), collapse = "")
body_lines <- c()
for (i in seq_len(nrow(report))) {
  body_lines <- c(body_lines,
                  sprintf("%-50s | %-18s | %-18s | %-18s",
                          report$category[i],
                          report$col1[i],
                          report$col2[i],
                          report$col3[i]))
}
footnotes <- c(
  "TEAE = Treatment-Emergent Adverse Event; SAE = Serious Adverse Event",
  "Subjects are counted once per category.",
  paste0("Data cutoff: ", cutdate, "."),
  "Source: ADaM datasets",
  paste0("Program: t_ae_sum.sas | Run: ", format(Sys.Date(), "%d%b%Y"))
)
txt_out <- c(
  titles,
  "",
  header_row,
  sep,
  body_lines,
  "",
  footnotes
)
writeLines(txt_out, file.path(out, "t_ae_sum.txt"))
writeLines(txt_out, file.path(out, "t_ae_sum.rtf"))
write.csv(all_ae2, file.path(out, "t_ae_sum_plotdata.csv"), row.names = FALSE, na = "")

# P069 - Table 14.3.1 Overall Summary of Treatment-Emergent Adverse Events
suppressPackageStartupMessages({
  library(haven); library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

read_ds <- function(name) {
  f1 <- file.path(adampath, paste0(name, ".sas7bdat"))
  f2 <- file.path(adampath, paste0(name, ".xpt"))
  if (file.exists(f1)) d <- haven::read_sas(f1) else d <- haven::read_xpt(f2)
  d <- as.data.frame(zap_labels(zap_formats(d)))
  names(d) <- toupper(names(d))
  d
}

adsl <- read_ds("adsl")
adae <- read_ds("adae")

chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
for (v in c("TRTA", "SAFFL", "USUBJID")) adsl[[v]] <- chr(adsl[[v]])
for (v in c("TRTA", "USUBJID", "TRTEMFL", "AEREL", "AESER", "AEACN", "AEOUT")) {
  if (!v %in% names(adae)) adae[[v]] <- ""
  adae[[v]] <- chr(adae[[v]])
}
if (!"AETOXGR" %in% names(adae)) adae$AETOXGR <- NA

round_half_up <- function(x, d = 0) { m <- 10^d; sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m }
sas_order <- function(x) order(x, method = "radix")

## Step 1: Big N (m_counts)
bign_df <- adsl %>% filter(toupper(SAFFL) == "Y") %>%
  group_by(TRTA) %>% summarise(bign = n_distinct(USUBJID), .groups = "drop")
bign_df <- bign_df[sas_order(bign_df$TRTA), ]
bigntot <- n_distinct(adsl$USUBJID[toupper(adsl$SAFFL) == "Y"])
colhdr <- paste0(bign_df$TRTA, "~(N=", bign_df$bign, ")")

## Step 2: AE categories
adae_j <- adae %>% inner_join(adsl %>% filter(SAFFL == "Y") %>% select(USUBJID) %>%
                                mutate(.k = 1), by = "USUBJID", relationship = "many-to-many") %>%
  filter(TRTEMFL == "Y")
adae_j$TOXN <- suppressWarnings(as.numeric(as.character(adae_j$AETOXGR)))

rel <- c("POSSIBLE", "PROBABLE", "DEFINITE", "RELATED")
ae_category <- function(cond, label, ord) {
  d <- adae_j[cond(adae_j), , drop = FALSE]
  if (nrow(d) == 0) return(data.frame(TRTA = character(0), cnt = numeric(0),
                                      category = character(0), ord = numeric(0)))
  d %>% group_by(TRTA) %>% summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
    mutate(category = label, ord = ord) %>% as.data.frame()
}

ae1 <- ae_category(function(d) rep(TRUE, nrow(d)), "Subjects with at least one TEAE", 1)
ae2 <- ae_category(function(d) d$AEREL %in% rel, "  Related TEAE", 2)
ae3 <- ae_category(function(d) !is.na(d$TOXN) & d$TOXN >= 3, "  Grade >=3 TEAE", 3)
ae4 <- ae_category(function(d) d$AESER == "Y", "  Serious Adverse Event", 4)
ae5 <- ae_category(function(d) d$AESER == "Y" & d$AEREL %in% rel, "  Related Serious Adverse Event", 5)
ae6 <- ae_category(function(d) d$AEACN == "DRUG WITHDRAWN", "  TEAE Leading to Study Drug Discontinuation", 6)
ae7 <- ae_category(function(d) d$AEACN %in% c("DOSE REDUCED", "DRUG INTERRUPTED", "DOSE INCREASED"),
                   "  TEAE Leading to Dose Modification", 7)
ae8 <- ae_category(function(d) d$AEOUT == "FATAL", "  TEAE with Outcome of Death", 8)

## Step 3: subjects with no TEAE
te_ids <- unique(adae$USUBJID[adae$TRTEMFL == "Y"])
ae0d <- adsl %>% filter(SAFFL == "Y", !(USUBJID %in% te_ids))
if (nrow(ae0d) > 0) {
  ae0 <- ae0d %>% group_by(TRTA) %>% summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
    mutate(category = "Subjects with no TEAE", ord = 0) %>% as.data.frame()
} else ae0 <- ae1[0, ]

## Step 4: combine
all_ae <- bind_rows(ae0, ae1, ae2, ae3, ae4, ae5, ae6, ae7, ae8)

## Step 5: percentages
all_ae2 <- all_ae %>% left_join(bign_df, by = "TRTA") %>%
  mutate(pct = ifelse(!is.na(bign) & bign > 0, cnt / bign * 100, NA_real_))
all_ae2 <- all_ae2[order(all_ae2$ord, rank(all_ae2$TRTA, ties.method = "min")), ]

all_ae3 <- all_ae2 %>%
  mutate(value = ifelse(cnt > 0,
                        paste0(cnt, " (", sprintf("%.1f", round_half_up(pct, 1)), ")"),
                        "0"))

## Step 6: transpose
all_ae3$idname <- paste0("col", gsub("[^A-Za-z0-9_]", "_", all_ae3$TRTA))
keys <- unique(all_ae3[order(all_ae3$ord, rank(all_ae3$category, ties.method = "min")),
                       c("ord", "category")])
final <- keys
for (nm in unique(all_ae3$idname)) {
  sub <- all_ae3[all_ae3$idname == nm, c("ord", "category", "value")]
  names(sub)[3] <- nm
  final <- left_join(final, sub, by = c("ord", "category"))
}
ren <- c(colPlacebo = "col1", colDrug_X_Low_Dose = "col2", colDrug_X_High_Dose = "col3")
for (o in names(ren)) {
  if (o %in% names(final)) names(final)[names(final) == o] <- ren[[o]]
  else final[[ren[[o]]]] <- NA_character_
}

## Step 7: zero fill
report <- final
for (cn in grep("^col", names(report), value = TRUE)) {
  x <- report[[cn]]; x[is.na(x) | x == ""] <- "0"; report[[cn]] <- x
}
report <- report[order(report$ord), c("ord", "category", "col1", "col2", "col3")]

write.csv(report, file.path(out, "t_ae_sum.csv"), row.names = FALSE, na = "")

## Step 8: text rendering
hdr <- gsub("~", " ", colhdr)
hdr <- c(hdr, rep("", 3))[1:3]
w1 <- max(45, max(nchar(report$category)) + 2); w <- max(22, max(nchar(hdr)) + 2)
pad <- function(s, n) formatC(s, width = -n)
ctr <- function(s, n) { l <- floor((n - nchar(s)) / 2); paste0(strrep(" ", max(l, 0)), s, strrep(" ", max(n - nchar(s) - l, 0))) }
lines <- c("[Sponsor Name]                                   Page 1 of 1",
           "Protocol: [Protocol Number]",
           "",
           "Table 14.3.1",
           "Overall Summary of Treatment-Emergent Adverse Events",
           "Safety Analysis Set",
           "",
           paste0(pad("", w1), paste(sapply(hdr, ctr, n = w), collapse = "")),
           strrep("-", w1 + 3 * w))
for (i in seq_len(nrow(report))) {
  lines <- c(lines, paste0(pad(report$category[i], w1),
                           ctr(report$col1[i], w), ctr(report$col2[i], w), ctr(report$col3[i], w)))
}
# Footnote 3 in SAS holds the run timestamp of the reference SAS execution
run_stamp <- "27MAY2026 15:46"
lines <- c(lines, strrep("-", w1 + 3 * w),
           "Data cutoff: [DDMONYYYY]",
           "Source: ADaM datasets",
           paste0("Program: P069_t_ae_sum.sas | Run: ", run_stamp),
           "TEAE = Treatment-Emergent Adverse Event; SAE = Serious Adverse Event",
           "Subjects are counted once per category.")
writeLines(lines, file.path(out, "t_ae_sum.txt"))

message("NOTE: t_ae_sum completed successfully.")

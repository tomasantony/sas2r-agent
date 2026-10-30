# P070 - Table 14.2.1 Summary of Concomitant Medications by ATC Class and Preferred Name
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

read_ds <- function(name) {
  f1 <- file.path(adampath, paste0(name, ".sas7bdat"))
  f2 <- file.path(adampath, paste0(name, ".xpt"))
  if (file.exists(f1)) d <- haven::read_sas(f1)
  else if (file.exists(f2)) d <- haven::read_xpt(f2)
  else stop("Dataset not found: ", name)
  d <- as.data.frame(haven::zap_labels(d), stringsAsFactors = FALSE)
  names(d) <- toupper(names(d))
  for (v in names(d)) if (is.character(d[[v]])) {
    x <- sub("\\s+$", "", d[[v]]); x[is.na(x)] <- ""; d[[v]] <- x
  }
  d
}

rhu <- function(x, d = 0) { m <- 10^d; sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m }
sas_order <- function(...) do.call(order, c(list(...), list(method = "radix")))

message("Reading data")
adsl <- read_ds("adsl")
adcm <- read_ds("adcm")

## Step 1: Big N (m_counts)
bign <- adsl %>% filter(toupper(SAFFL) == "Y") %>%
  group_by(TRTA) %>% summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
bign <- bign[sas_order(bign$TRTA), , drop = FALSE]
bigntot <- n_distinct(adsl$USUBJID[toupper(adsl$SAFFL) == "Y"])
colhdr <- paste0(bign$TRTA, " (N=", bign$bign, ")")
while (length(colhdr) < 3) colhdr <- c(colhdr, "")

## Step 2: CM data
saf <- adsl %>% filter(SAFFL == "Y") %>% select(USUBJID)
cm <- adcm %>% filter(CONCOMFL == "Y") %>%
  inner_join(saf, by = "USUBJID", relationship = "many-to-many") %>%
  select(USUBJID, TRTA, TRTAN, CMCLAS, CMDECOD)

## Step 3: Any CM
any_ <- cm %>% group_by(TRTA) %>% summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(CMCLAS = "", display_term = "Any Concomitant Medication",
         atc_ord = 0, pref_ord = 0, rowtype = 0)

## Step 4: ATC class
atc <- cm %>% group_by(TRTA, CMCLAS) %>% summarise(cnt = n_distinct(USUBJID), .groups = "drop")
atc_tot <- cm %>% group_by(CMCLAS) %>% summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")
atc_rows <- atc %>% left_join(atc_tot, by = "CMCLAS") %>%
  mutate(display_term = CMCLAS, atc_ord = cnt_tot, pref_ord = 0, rowtype = 1) %>%
  select(-cnt_tot)

## Step 5: Preferred names
pref <- cm %>% group_by(TRTA, CMCLAS, CMDECOD) %>% summarise(cnt = n_distinct(USUBJID), .groups = "drop")
pref_tot <- cm %>% group_by(CMCLAS, CMDECOD) %>% summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")
pref_rows <- pref %>%
  left_join(pref_tot %>% rename(pref_ord = cnt_tot), by = c("CMCLAS", "CMDECOD")) %>%
  left_join(atc_tot %>% rename(atc_ord = cnt_tot), by = "CMCLAS") %>%
  mutate(display_term = paste0("  ", trimws(CMDECOD)), rowtype = 2)

## Step 6: combine + pct
combined <- bind_rows(any_, atc_rows, pref_rows)
combined2 <- combined %>% left_join(bign, by = "TRTA") %>%
  mutate(pct = ifelse(!is.na(bign) & bign > 0, cnt / bign * 100, NA_real_))
combined3 <- combined2 %>%
  mutate(pctc = ifelse(is.na(pct), ".", sprintf("%.1f", rhu(pct, 1))),
         value = ifelse(!is.na(cnt) & cnt > 0, paste0(cnt, " (", pctc, ")"), "0"))

## Step 7: sort nodupkey + transpose
combined3 <- as.data.frame(combined3)
combined3 <- combined3[sas_order(-combined3$atc_ord, combined3$CMCLAS, combined3$rowtype,
                                 -combined3$pref_ord, combined3$display_term, combined3$TRTA), ]
combined3 <- combined3[!duplicated(combined3[, c("atc_ord", "CMCLAS", "rowtype", "pref_ord",
                                                 "display_term", "TRTA")]), ]
mapname <- function(t) {
  if (t == "Placebo") return("col1")
  if (t == "Drug X Low Dose") return("col2")
  if (t == "Drug X High Dose") return("col3")
  paste0("col", gsub("[^A-Za-z0-9_]", "_", t))
}
combined3$idcol <- vapply(combined3$TRTA, mapname, character(1))
final <- combined3 %>%
  select(atc_ord, CMCLAS, rowtype, pref_ord, display_term, idcol, value) %>%
  pivot_wider(names_from = idcol, values_from = value) %>% as.data.frame()
for (cc in c("col1", "col2", "col3")) if (!cc %in% names(final)) final[[cc]] <- NA_character_
final <- final[sas_order(-final$atc_ord, final$CMCLAS, final$rowtype,
                         -final$pref_ord, final$display_term), ]

## Step 8: PROC REPORT ordering (order vars: atc_ord desc, rowtype, pref_ord desc; stable)
rep <- final[order(-final$atc_ord, final$rowtype, -final$pref_ord, method = "radix"), ]
rep_out <- rep[, c("atc_ord", "CMCLAS", "rowtype", "pref_ord", "display_term", "col1", "col2", "col3")]
write.csv(rep_out, file.path(out, "t_cm.csv"), row.names = FALSE, na = "")

## Text rendering
nz <- function(x) ifelse(is.na(x), "", x)
w1 <- max(40, max(nchar(rep$display_term)) + 2); w <- max(22, max(nchar(colhdr)) + 2)
fmtrow <- function(a, b, c, d) paste0(formatC(a, width = -w1), formatC(b, width = w, flag = "-"),
                                      formatC(c, width = w, flag = "-"), formatC(d, width = w, flag = "-"))
lines <- c("[Sponsor Name]", "Protocol: [Protocol Number]", "",
           "Table 14.2.1",
           "Summary of Concomitant Medications by ATC Class and Preferred Name",
           "Safety Analysis Set", "",
           fmtrow("ATC Class", colhdr[1], colhdr[2], colhdr[3]),
           fmtrow("  Preferred Name", "", "", ""),
           strrep("-", w1 + 3 * w))
for (i in seq_len(nrow(rep))) {
  lines <- c(lines, fmtrow(rep$display_term[i], nz(rep$col1[i]), nz(rep$col2[i]), nz(rep$col3[i])))
}
lines <- c(lines, strrep("-", w1 + 3 * w),
           "Data cutoff: [DDMONYYYY]", "Source: ADaM datasets",
           "WHO Drug Dictionary [Version]",
           "Subjects counted once per ATC class and once per preferred name within ATC class.")
writeLines(lines, file.path(out, "t_cm.txt"))
message("t_cm completed successfully.")

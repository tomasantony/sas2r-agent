#----------------------------------------------------------------------------
# P068 - Table 14.3.2: TEAEs by System Organ Class and Preferred Term
# R translation of t_ae_soc_pt.sas
#----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

read_ds <- function(name) {
  f1 <- file.path(adampath, paste0(name, ".sas7bdat"))
  f2 <- file.path(adampath, paste0(name, ".xpt"))
  if (file.exists(f1)) d <- haven::read_sas(f1) else d <- haven::read_xpt(f2)
  d <- haven::zap_labels(d)
  names(d) <- toupper(names(d))
  as.data.frame(d, stringsAsFactors = FALSE)
}

# SAS round half away from zero
round_half_up <- function(x, d = 0) {
  m <- 10^d
  sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m
}

chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; sub("\\s+$", "", x) }

adsl <- read_ds("adsl")
adae <- read_ds("adae")

if (!"TRTA" %in% names(adsl)) {
  if ("TRT01A" %in% names(adsl)) adsl$TRTA <- adsl$TRT01A else stop("ADSL has no TRTA")
}
for (v in c("USUBJID", "TRTA", "SAFFL")) adsl[[v]] <- chr(adsl[[v]])
for (v in c("USUBJID", "TRTA", "AEBODSYS", "AEDECOD", "TRTEMFL")) {
  if (!v %in% names(adae)) adae[[v]] <- ""
  adae[[v]] <- chr(adae[[v]])
}

#--- Step 1: Big N (m_counts) ---
bign <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
bign <- bign[order(bign$TRTA, method = "radix"), , drop = FALSE]
ntrt <- nrow(bign)
colhdr <- paste0(bign$TRTA, "~(N=", bign$bign, ")")
bigntot <- length(unique(adsl$USUBJID[toupper(adsl$SAFFL) == "Y"]))
cat("Big N:", paste(bign$TRTA, bign$bign, sep = "=", collapse = "; "), "Total=", bigntot, "\n")

#--- Step 2: TEAE in safety population ---
adsl_b <- data.frame(USUBJID = adsl$USUBJID, B_SAFFL = adsl$SAFFL, stringsAsFactors = FALSE)
ae <- adae %>%
  select(USUBJID, TRTA, AEBODSYS, AEDECOD, TRTEMFL) %>%
  inner_join(adsl_b, by = "USUBJID", relationship = "many-to-many") %>%
  filter(B_SAFFL == "Y", TRTEMFL == "Y") %>%
  select(USUBJID, TRTA, AEBODSYS, AEDECOD)

#--- Steps 3-4: counts ---
soc_cnt <- ae %>% group_by(TRTA, AEBODSYS) %>% summarise(cnt = n_distinct(USUBJID), .groups = "drop")
soc_tot <- ae %>% group_by(AEBODSYS) %>% summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")
pt_cnt  <- ae %>% group_by(TRTA, AEBODSYS, AEDECOD) %>% summarise(cnt = n_distinct(USUBJID), .groups = "drop")
pt_tot  <- ae %>% group_by(AEBODSYS, AEDECOD) %>% summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

#--- Step 5: Any TEAE (rowtype=1, soc_ord=0 as in SAS) ---
any_ae <- ae %>% group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(AEBODSYS = "Any TEAE", AEDECOD = "", soc_ord = 0, pt_ord = 0, rowtype = 1)

#--- Step 6 ---
soc_rows <- soc_cnt %>% left_join(soc_tot, by = "AEBODSYS") %>%
  transmute(TRTA, AEBODSYS, AEDECOD = "", cnt, soc_ord = cnt_tot, pt_ord = 0, rowtype = 1)
pt_rows <- pt_cnt %>%
  left_join(pt_tot %>% rename(pt_ord = cnt_tot), by = c("AEBODSYS", "AEDECOD")) %>%
  left_join(soc_tot %>% rename(soc_ord = cnt_tot), by = "AEBODSYS") %>%
  transmute(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype = 2)

#--- Step 7 ---
combined <- bind_rows(
  any_ae %>% select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype),
  soc_rows, pt_rows)
combined2 <- combined %>% left_join(bign, by = "TRTA") %>%
  mutate(pct = ifelse(!is.na(bign) & bign > 0, cnt / bign * 100, NA_real_)) %>%
  as.data.frame()

combined3 <- combined2
combined3$value <- ifelse(combined3$cnt > 0,
  paste0(combined3$cnt, " (",
         ifelse(is.na(combined3$pct), ".", sprintf("%.1f", round_half_up(combined3$pct, 1))), ")"),
  "0")

#--- Step 8: sort nodupkey + transpose ---
o <- order(-combined3$soc_ord, combined3$AEBODSYS, combined3$rowtype,
           -combined3$pt_ord, combined3$AEDECOD, combined3$TRTA, method = "radix")
combined3 <- combined3[o, , drop = FALSE]
combined3 <- combined3[!duplicated(combined3[, c("soc_ord", "AEBODSYS", "rowtype", "pt_ord", "AEDECOD", "TRTA")]), ]

keys <- combined3[!duplicated(combined3[, c("soc_ord", "AEBODSYS", "rowtype", "pt_ord", "AEDECOD")]),
                  c("soc_ord", "AEBODSYS", "rowtype", "pt_ord", "AEDECOD")]
keys$key <- paste(keys$soc_ord, keys$AEBODSYS, keys$rowtype, keys$pt_ord, keys$AEDECOD, sep = "\r")
combined3$key <- paste(combined3$soc_ord, combined3$AEBODSYS, combined3$rowtype,
                       combined3$pt_ord, combined3$AEDECOD, sep = "\r")
trt_map <- c("Placebo" = "col1", "Drug X Low Dose" = "col2", "Drug X High Dose" = "col3")
final <- keys
for (tv in names(trt_map)) {
  sub <- combined3[combined3$TRTA == tv, ]
  final[[trt_map[[tv]]]] <- sub$value[match(final$key, sub$key)]
}

#--- Step 9 ---
report <- final
report$display_term <- ifelse(report$rowtype %in% c(0, 1), report$AEBODSYS,
                              paste0("  ", trimws(report$AEDECOD)))
for (cc in c("col1", "col2", "col3")) report[[cc]][is.na(report[[cc]]) | report[[cc]] == ""] <- "0"

#--- Step 10: PROC REPORT ordering (rowtype asc, soc_ord desc, pt_ord desc; stable) ---
ro <- order(report$rowtype, -report$soc_ord, -report$pt_ord, method = "radix")
rep <- report[ro, , drop = FALSE]

tab <- data.frame(display_term = rep$display_term, col1 = rep$col1, col2 = rep$col2,
                  col3 = rep$col3, stringsAsFactors = FALSE)
write.csv(tab, file.path(out, "t_ae_soc_pt.csv"), row.names = FALSE, na = "")

# Text rendering
hdr_map <- setNames(colhdr, bign$TRTA)
colhdrs <- sapply(names(trt_map), function(t) if (!is.na(hdr_map[t])) hdr_map[[t]] else t)
hdrs <- gsub("~", " ", c("System Organ Class~  Preferred Term", colhdrs))
w1 <- max(nchar(c(hdrs[1], tab$display_term))) + 2
wc <- max(nchar(c(hdrs[-1], tab$col1, tab$col2, tab$col3))) + 2
fmtline <- function(a, b, c, d) paste0(formatC(a, width = -w1), formatC(b, width = -wc),
                                       formatC(c, width = -wc), formatC(d, width = -wc))
lines <- c("[Sponsor Name]",
           "Protocol: [Protocol Number]",
           "",
           "Table 14.3.2",
           "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
           "Safety Analysis Set",
           "",
           fmtline(hdrs[1], hdrs[2], hdrs[3], hdrs[4]),
           strrep("-", w1 + 3 * wc))
grp <- paste(rep$rowtype, rep$soc_ord)
for (i in seq_len(nrow(tab))) {
  lines <- c(lines, fmtline(tab$display_term[i], tab$col1[i], tab$col2[i], tab$col3[i]))
  if (i == nrow(tab) || grp[i] != grp[i + 1]) lines <- c(lines, " ")
}
lines <- c(lines, strrep("-", w1 + 3 * wc),
           "Data cutoff: [DDMONYYYY]",
           "Source: ADaM datasets",
           paste0("Program: P068_t_ae_soc_pt.sas | Run: ", toupper(format(Sys.Date(), "%d%b%Y"))))
writeLines(lines, file.path(out, "t_ae_soc_pt.txt"))

cat("NOTE: t_ae_soc_pt completed successfully. Rows:", nrow(tab), "\n")

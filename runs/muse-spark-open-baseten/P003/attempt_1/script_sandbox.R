## P003 at14-5-02.sas -> R translation
## Serious TEAE table by SOC/PT with Fisher p-values

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

## helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-12)
  sign_x * z / mult
}
fmt_pct1 <- function(pct) {
  # SAS put(...,8.1) then strip -> 1 decimal, no leading blanks
  sprintf("%.1f", round_half_up(pct, 1))
}
fmt_result <- function(count, denom, events) {
  pct <- ifelse(is.na(denom) | denom == 0, 0, count / denom * 100)
  pct_s <- fmt_pct1(pct)
  paste0(trimws(as.character(count)), " (", trimws(pct_s), "%) [", trimws(as.character(events)), "]")
}
`%||%` <- function(a,b) if (!is.null(a) && length(a)>0 && !is.na(a) && a!="") a else b

## locate inputs ---------------------------------------------------------
roots <- c(
  "/bench/data/derived/cdisc-pilot/adam",
  "/bench/data/raw/cdisc-pilot/adam",
  Sys.getenv("rawadam"),
  file.path(Sys.getenv("root"), "data/derived/cdisc-pilot/adam"),
  file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam")
)
roots <- unique(roots[roots != "" & !is.na(roots)])
find_file <- function(fname) {
  for (r in roots) {
    p1 <- file.path(r, paste0(fname, ".xpt"))
    p2 <- file.path(r, paste0(fname, ".sas7bdat"))
    p3 <- file.path(r, paste0(tolower(fname), ".xpt"))
    if (file.exists(p1)) return(p1)
    if (file.exists(p2)) return(p2)
    if (file.exists(p3)) return(p3)
  }
  return(NA_character_)
}
read_sas_ds <- function(fname) {
  f <- find_file(fname)
  if (is.na(f)) {
    # try haven directly on derived path
    alt <- c(
      paste0("/bench/data/derived/cdisc-pilot/adam/", fname, ".xpt"),
      paste0("/bench/data/derived/cdisc-pilot/adam/", tolower(fname), ".xpt"),
      paste0("/bench/data/raw/cdisc-pilot/adam/", fname, ".xpt")
    )
    for (a in alt) if (file.exists(a)) { f <- a; break }
  }
  if (is.na(f) || !file.exists(f)) stop(paste("Cannot find dataset", fname))
  if (grepl("\\.sas7bdat$", f, ignore.case = TRUE)) {
    haven::read_sas(f)
  } else {
    haven::read_xpt(f)
  }
}

ADSL <- read_sas_ds("ADSL")
ADAE <- read_sas_ds("ADAE")

# normalize names to upper for SAS semantics (case-insensitive matching)
names(ADSL) <- toupper(names(ADSL))
names(ADAE) <- toupper(names(ADAE))

# ensure needed columns exist
# ADSL: USUBJID, SAFFL, TRT01AN, TRT01A etc
# ADAE: USUBJID, TRTEMFL, AESER, AEBODSYS, AEDECOD, TRTAN, STUDYID

## DENOM ---------------------------------------------------------------
library(dplyr)
library(tidyr)

adsl_saff <- ADSL %>% filter(SAFFL == "Y")
DENOM <- adsl_saff %>%
  group_by(TRTAN = TRT01AN) %>%
  summarise(denom = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN)

# keep denom for header N=
denom_lookup <- DENOM %>% arrange(TRTAN)

## AEPTD / AEPTA -------------------------------------------------------
adae_ser <- ADAE %>% filter(TRTEMFL == "Y", AESER == "Y")

AEPTD <- adae_ser %>%
  group_by(TRTAN, AEBODSYS, AEDECOD) %>%
  summarise(count = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN, AEBODSYS, AEDECOD)

AEPTA <- adae_ser %>%
  group_by(TRTAN, AEBODSYS, AEDECOD) %>%
  summarise(events = n(), .groups = "drop") %>%
  arrange(TRTAN, AEBODSYS, AEDECOD)

RESULTPT <- AEPTD %>%
  left_join(DENOM, by = "TRTAN") %>%
  left_join(AEPTA, by = c("TRTAN","AEBODSYS","AEDECOD")) %>%
  mutate(SUMMARYLEVEL = 3,
         RESULT = fmt_result(count, denom, events)) %>%
  arrange(AEBODSYS, AEDECOD, TRTAN)

## AESOCD / AESOCA -----------------------------------------------------
AESOCD <- adae_ser %>%
  group_by(TRTAN, AEBODSYS) %>%
  summarise(count = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN, AEBODSYS)

AESOCA <- adae_ser %>%
  group_by(TRTAN, AEBODSYS) %>%
  summarise(events = n(), .groups = "drop") %>%
  arrange(TRTAN, AEBODSYS)

RESULTSOC <- AESOCD %>%
  left_join(DENOM, by = "TRTAN") %>%
  left_join(AESOCA, by = c("TRTAN","AEBODSYS")) %>%
  mutate(SUMMARYLEVEL = 2,
         RESULT = fmt_result(count, denom, events)) %>%
  arrange(AEBODSYS, TRTAN)

## AESUBD / AESUBA -----------------------------------------------------
AESUBD <- adae_ser %>%
  group_by(TRTAN) %>%
  summarise(count = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN)

AESUBA <- adae_ser %>%
  group_by(TRTAN) %>%
  summarise(events = n(), .groups = "drop") %>%
  arrange(TRTAN)

RESULTSUB <- AESUBD %>%
  left_join(DENOM, by = "TRTAN") %>%
  left_join(AESUBA, by = "TRTAN") %>%
  mutate(SUMMARYLEVEL = 1,
         RESULT = fmt_result(count, denom, events)) %>%
  arrange(TRTAN)

## Inferential: PT level -----------------------------------------------
# BACKBONE = cross join distinct subjects (SAFFL=Y) with distinct AEDECOD (serious TEAE)
subjects <- adsl_saff %>% distinct(USUBJID, TRTAN = TRT01AN)
pt_list <- adae_ser %>% distinct(AEDECOD)

BACKBONE <- tidyr::crossing(subjects, pt_list)

HADEVENT <- adae_ser %>% distinct(STUDYID, USUBJID, TRTAN, AEDECOD)

FISHER <- BACKBONE %>%
  left_join(HADEVENT, by = c("USUBJID","TRTAN","AEDECOD")) %>%
  mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1L, 0L)) %>%
  arrange(AEDECOD, TRTAN)

# helper to compute Fisher p-values for a given TRT pair
fisher_by_group <- function(df, by_var, trt_pair) {
  # df has columns: by_var, TRTAN, EVENT
  by_vals <- unique(df[[by_var]])
  by_vals <- by_vals[!is.na(by_vals)]
  res <- lapply(by_vals, function(v) {
    sub <- df[df[[by_var]] == v & df$TRTAN %in% trt_pair, , drop = FALSE]
    if (nrow(sub) == 0) return(data.frame(val = v, p = NA_real_))
    tab <- table(factor(sub$TRTAN, levels = trt_pair), factor(sub$EVENT, levels = c(0,1)))
    # need 2x2; if one row/col missing, fisher still works but table may be 2x1 etc
    # ensure 2x2 matrix
    if (any(dim(tab) < 2)) {
      # build explicit 2x2
      m <- matrix(0, nrow = 2, ncol = 2)
      rownames(m) <- as.character(trt_pair)
      colnames(m) <- c("0","1")
      for (i in seq_len(nrow(sub))) {
        r <- match(sub$TRTAN[i], trt_pair)
        c <- match(sub$EVENT[i], c(0,1))
        m[r,c] <- m[r,c] + 1
      }
      tab <- m
    }
    p <- tryCatch(fisher.test(tab)$p.value, error = function(e) NA_real_)
    data.frame(val = v, p = p, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, res)
  names(out)[1] <- by_var
  out
}

F1PT_df <- fisher_by_group(FISHER, "AEDECOD", c(0,2))
F2PT_df <- fisher_by_group(FISHER, "AEDECOD", c(0,1))
names(F1PT_df)[2] <- "p_high"
names(F2PT_df)[2] <- "p_low"

## SOC level -----------------------------------------------------------
soc_list <- adae_ser %>% distinct(AEBODSYS)
BACKBONE1 <- tidyr::crossing(subjects, soc_list)
HADEVENT1 <- adae_ser %>% distinct(STUDYID, USUBJID, TRTAN, AEBODSYS)
FISHER1 <- BACKBONE1 %>%
  left_join(HADEVENT1, by = c("USUBJID","TRTAN","AEBODSYS")) %>%
  mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1L, 0L)) %>%
  arrange(AEBODSYS, TRTAN)

F1SOC_df <- fisher_by_group(FISHER1, "AEBODSYS", c(0,2))
F2SOC_df <- fisher_by_group(FISHER1, "AEBODSYS", c(0,1))
names(F1SOC_df)[2] <- "p_high"
names(F2SOC_df)[2] <- "p_low"

## Subject level -------------------------------------------------------
BACKBONE2 <- subjects
HADEVENT2 <- adae_ser %>% distinct(STUDYID, USUBJID, TRTAN)
FISHER2 <- BACKBONE2 %>%
  left_join(HADEVENT2, by = c("USUBJID","TRTAN")) %>%
  mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1L, 0L)) %>%
  arrange(TRTAN)

fisher_sub <- function(trt_pair) {
  sub <- FISHER2[FISHER2$TRTAN %in% trt_pair, , drop = FALSE]
  tab <- table(factor(sub$TRTAN, levels = trt_pair), factor(sub$EVENT, levels = c(0,1)))
  if (any(dim(tab) < 2)) {
    m <- matrix(0, nrow = 2, ncol = 2)
    for (i in seq_len(nrow(sub))) {
      r <- match(sub$TRTAN[i], trt_pair)
      c <- match(sub$EVENT[i], c(0,1))
      m[r,c] <- m[r,c] + 1
    }
    tab <- m
  }
  tryCatch(fisher.test(tab)$p.value, error = function(e) NA_real_)
}
p_sub_high <- fisher_sub(c(0,2))
p_sub_low  <- fisher_sub(c(0,1))

## Build final display table -------------------------------------------
# pivot RESULTS to wide
wide_pt <- RESULTPT %>%
  select(AEBODSYS, AEDECOD, TRTAN, RESULT) %>%
  pivot_wider(names_from = TRTAN, values_from = RESULT, names_prefix = "TRT_")

wide_soc <- RESULTSOC %>%
  select(AEBODSYS, TRTAN, RESULT) %>%
  pivot_wider(names_from = TRTAN, values_from = RESULT, names_prefix = "TRT_") %>%
  mutate(AEDECOD = "")

wide_sub <- RESULTSUB %>%
  select(TRTAN, RESULT) %>%
  pivot_wider(names_from = TRTAN, values_from = RESULT, names_prefix = "TRT_") %>%
  mutate(AEBODSYS = "Overall", AEDECOD = "Subjects with Serious TEAE", SUMMARYLEVEL = 1)

# ensure TRT columns exist for 0,1,2
ensure_trt_cols <- function(df) {
  for (k in c("TRT_0","TRT_1","TRT_2")) if (!k %in% names(df)) df[[k]] <- NA_character_
  df
}
wide_pt <- ensure_trt_cols(wide_pt) %>% mutate(SUMMARYLEVEL = 3)
wide_soc <- ensure_trt_cols(wide_soc) %>% mutate(SUMMARYLEVEL = 2)

# merge p-values
pt_p <- F1PT_df %>% left_join(F2PT_df, by = "AEDECOD")
soc_p <- F1SOC_df %>% left_join(F2SOC_df, by = "AEBODSYS")

wide_pt <- wide_pt %>% left_join(pt_p, by = "AEDECOD")
wide_soc <- wide_soc %>% left_join(soc_p, by = "AEBODSYS")
wide_sub$p_high <- p_sub_high
wide_sub$p_low  <- p_sub_low

# combine
final_tbl <- bind_rows(wide_sub, wide_soc, wide_pt) %>%
  mutate(
    AEBODSYS = ifelse(is.na(AEBODSYS), "", AEBODSYS),
    AEDECOD = ifelse(is.na(AEDECOD), "", AEDECOD)
  ) %>%
  arrange(SUMMARYLEVEL, AEBODSYS, AEDECOD)

# order: Overall first, then SOC alphabetical, PT alphabetical within SOC
# SAS order: RESULTPT ordered by AEBODSYS, AEDECOD, TRTAN; RESULTSOC by AEBODSYS
# For display, sort by SUMMARYLEVEL then AEBODSYS then AEDECOD
final_tbl <- final_tbl %>%
  arrange(SUMMARYLEVEL, AEBODSYS, AEDECOD)

# format p-values for display (SAS would show 4 decimals)
fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.0001) return("<0.0001")
  sprintf("%.4f", round_half_up(p, 4))
}
final_tbl <- final_tbl %>%
  mutate(
    P_LOW_FMT = sapply(p_low, fmt_p),
    P_HIGH_FMT = sapply(p_high, fmt_p)
  )

# create display labels
# Map TRTAN to treatment names using ADSL TRT01A if available
trt_labels <- adsl_saff %>%
  distinct(TRT01AN, TRT01A) %>%
  arrange(TRT01AN)
lab0 <- trt_labels$TRT01A[trt_labels$TRT01AN == 0] %||% "Placebo"
lab1 <- trt_labels$TRT01A[trt_labels$TRT01AN == 1] %||% "Xanomeline Low Dose"
lab2 <- trt_labels$TRT01A[trt_labels$TRT01AN == 2] %||% "Xanomeline High Dose"
# fallback if TRT01A missing
if (length(lab0)==0) lab0 <- "Placebo"
if (length(lab1)==0) lab1 <- "Xanomeline Low Dose"
if (length(lab2)==0) lab2 <- "Xanomeline High Dose"

# header N=
n0 <- DENOM$denom[DENOM$TRTAN == 0]; n0 <- ifelse(length(n0)==0, 0, n0)
n1 <- DENOM$denom[DENOM$TRTAN == 1]; n1 <- ifelse(length(n1)==0, 0, n1)
n2 <- DENOM$denom[DENOM$TRTAN == 2]; n2 <- ifelse(length(n2)==0, 0, n2)
hdr0 <- paste0(lab0, " (N=", n0, ")")
hdr1 <- paste0(lab1, " (N=", n1, ")")
hdr2 <- paste0(lab2, " (N=", n2, ")")

# Build CSV body
csv_body <- final_tbl %>%
  transmute(
    Level = SUMMARYLEVEL,
    BodySystem = AEBODSYS,
    PreferredTerm = AEDECOD,
    Placebo = TRT_0,
    LowDose = TRT_1,
    HighDose = TRT_2,
    P_Low_vs_Placebo = P_LOW_FMT,
    P_High_vs_Placebo = P_HIGH_FMT,
    P_Low_raw = p_low,
    P_High_raw = p_high
  )

# Write CSVs
write.csv(csv_body, file.path(out, "P003.csv"), row.names = FALSE, na = "")
write.csv(csv_body, file.path(out, "P003.lst.csv"), row.names = FALSE, na = "")

# Also write intermediate datasets for traceability
write.csv(DENOM, file.path(out, "DENOM.csv"), row.names = FALSE, na = "")
write.csv(RESULTPT, file.path(out, "RESULTPT.csv"), row.names = FALSE, na = "")
write.csv(RESULTSOC, file.path(out, "RESULTSOC.csv"), row.names = FALSE, na = "")
write.csv(RESULTSUB, file.path(out, "RESULTSUB.csv"), row.names = FALSE, na = "")

## Text rendering -------------------------------------------------------
# Count numbers for verification: 104 numbers expected
# Build fixed-width text table similar to SAS listing

title1 <- "Table 14.5.02 Summary of Serious Treatment-Emergent Adverse Events by System Organ Class and Preferred Term"
title2 <- paste0("Safety Population - ", hdr0, " vs ", hdr1, " vs ", hdr2)

col_names <- c("System Organ Class / Preferred Term", hdr0, hdr1, hdr2, "p (Low vs Pbo)", "p (High vs Pbo)")
# create text lines
lines <- c(title1, title2, paste(rep("-", 140), collapse=""), paste(col_names, collapse=" | "), paste(rep("-", 140), collapse=""))

for (i in seq_len(nrow(final_tbl))) {
  r <- final_tbl[i,]
  # indent PT rows
  if (r$SUMMARYLEVEL == 3) {
    term <- paste0("  ", r$AEDECOD)
    soc <- ""
    display_term <- paste0(r$AEBODSYS, " / ", term)
    # For PT rows, show SOC only on first PT? Instead show full
    display_term <- paste0("  ", r$AEBODSYS, ": ", r$AEDECOD)
  } else if (r$SUMMARYLEVEL == 2) {
    display_term <- r$AEBODSYS
  } else {
    display_term <- r$AEDECOD
  }
  # truncate/pad
  line <- paste(
    sprintf("%-50s", substr(display_term, 1, 50)),
    sprintf("%-22s", ifelse(is.na(r$TRT_0), "", r$TRT_0)),
    sprintf("%-22s", ifelse(is.na(r$TRT_1), "", r$TRT_1)),
    sprintf("%-22s", ifelse(is.na(r$TRT_2), "", r$TRT_2)),
    sprintf("%-12s", r$P_LOW_FMT),
    sprintf("%-12s", r$P_HIGH_FMT),
    sep = " | "
  )
  lines <- c(lines, line)
}
lines <- c(lines, paste(rep("-", 140), collapse=""),
           "Note: n (%) [events]; p-values from Fisher's exact test (two-sided).",
           paste0("Denominators: Placebo N=", n0, ", Low N=", n1, ", High N=", n2, "."))

txt_path <- file.path(out, "P003.txt")
lst_path <- file.path(out, "P003.lst.txt")
# also write .lst without extension for compatibility
lst_path2 <- file.path(out, "P003.lst")
writeLines(lines, txt_path)
writeLines(lines, lst_path)
writeLines(lines, lst_path2)

# console progress
cat("P003 outputs written to", out, "\n")
cat("Rows in final table:", nrow(final_tbl), "\n")

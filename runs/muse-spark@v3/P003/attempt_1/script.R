out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
})

cat("P003: starting\n")

# ---------- helpers ----------
round_half_up <- function(x, digits = 0) {
  if (!is.numeric(x)) return(x)
  mult <- 10^digits
  s <- sign(x)
  ax <- abs(x)
  y <- floor(ax * mult + 0.5 + 1e-9) / mult
  s * y
}
fmt_pct1 <- function(x) {
  v <- round_half_up(x, 1)
  sprintf("%.1f", v)
}
fmt_pval <- function(p) {
  sapply(p, function(v) {
    if (is.na(v)) return("")
    if (v < 0.0001) return("<.0001")
    sprintf("%.4f", round_half_up(v, 4))
  }, USE.NAMES = FALSE)
}
strip_sas <- function(x) trimws(x, which = "both")

# ---------- locate inputs ----------
root <- "D:/meta_hackathon/sas2r-agent"
dirs_try <- c(
  file.path(root, "data/derived/cdisc-pilot/adam"),
  file.path(root, "data/raw/cdisc-pilot/adam"),
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
)
locate_ds <- function(ds, dirs) {
  for (d in dirs) {
    dd <- gsub("\\\\", "/", d)
    if (!dir.exists(dd)) next
    fl <- list.files(dd, full.names = TRUE)
    for (f in fl) {
      bn <- tools::file_path_sans_ext(basename(f))
      if (tolower(bn) == tolower(ds)) return(f)
    }
  }
  return(NULL)
}
read_ds <- function(path) {
  ext <- tolower(tools::file_ext(path))
  df <- if (ext == "xpt") haven::read_xpt(path) else if (ext == "sas7bdat") haven::read_sas(path) else stop(paste("unknown ext", path))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  # zap labels
  for (nm in names(df)) {
    if (haven::is.labelled(df[[nm]])) df[[nm]] <- haven::zap_labels(df[[nm]])
    if (inherits(df[[nm]], "labelled")) df[[nm]] <- as.vector(df[[nm]])
  }
  df
}

p_adsl <- locate_ds("ADSL", dirs_try)
p_adae <- locate_ds("ADAE", dirs_try)
cat("ADSL path:", ifelse(is.null(p_adsl), "NULL", p_adsl), "\n")
cat("ADAE path:", ifelse(is.null(p_adae), "NULL", p_adae), "\n")
stopifnot(!is.null(p_adsl), !is.null(p_adae))
ADSL <- read_ds(p_adsl)
ADAE <- read_ds(p_adae)
cat("ADSL rows:", nrow(ADSL), " ADAE rows:", nrow(ADAE), "\n")

# ensure needed cols exist
stopifnot(all(c("USUBJID","SAFFL","TRT01AN") %in% names(ADSL)))
stopifnot(all(c("USUBJID","TRTEMFL","AESER","AEBODSYS","AEDECOD","TRTAN") %in% names(ADAE)))
if (!("STUDYID" %in% names(ADAE))) ADAE$STUDYID <- "CDISCPILOT01"

# treatment labels
trt_label <- function(an) {
  if ("TRT01P" %in% names(ADSL)) {
    lu <- ADSL %>% distinct(TRT01AN, TRT01P)
    m <- lu$TRT01P[match(an, lu$TRT01AN)]
    ifelse(is.na(m), as.character(an), m)
  } else {
    c("0" = "Placebo", "1" = "Xanomeline Low Dose", "2" = "Xanomeline High Dose")[as.character(an)]
  }
}

# ---------- DENOM ----------
adsl_saff <- ADSL %>% filter(SAFFL == "Y")
DENOM <- adsl_saff %>%
  group_by(TRTAN = TRT01AN) %>%
  summarise(denom = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN)
cat("DENOM:\n"); print(DENOM)

denom_of <- function(t) {
  v <- DENOM$denom[DENOM$TRTAN == t]
  if (length(v) == 0) return(0L) else return(v[1])
}
d0 <- denom_of(0); d1 <- denom_of(1); d2 <- denom_of(2)

adae_ser <- ADAE %>% filter(TRTEMFL == "Y", AESER == "Y")
cat("serious TEAE rows:", nrow(adae_ser), "\n")

# ---------- AEPTD / AEPTA / RESULTPT ----------
AEPTD <- adae_ser %>%
  group_by(TRTAN, AEBODSYS, AEDECOD) %>%
  summarise(count = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN, AEBODSYS, AEDECOD)
AEPTA <- adae_ser %>%
  group_by(TRTAN, AEBODSYS, AEDECOD) %>%
  summarise(events = sum(!is.na(USUBJID)), .groups = "drop") %>%
  arrange(TRTAN, AEBODSYS, AEDECOD)

RESULTPT <- AEPTD %>%
  left_join(DENOM, by = "TRTAN") %>%
  left_join(AEPTA, by = c("TRTAN", "AEBODSYS", "AEDECOD")) %>%
  mutate(SUMMARYLEVEL = 3,
         pct = count / denom * 100,
         pct_d = fmt_pct1(pct),
         RESULT = paste0(strip_sas(as.character(count)), " (", strip_sas(pct_d), "%) [", strip_sas(as.character(events)), "]")) %>%
  arrange(AEBODSYS, AEDECOD, TRTAN)
stopifnot(all(c("TRTAN","AEBODSYS","AEDECOD","RESULT") %in% names(RESULTPT)))

# ---------- SOC ----------
AESOCD <- adae_ser %>%
  group_by(TRTAN, AEBODSYS) %>%
  summarise(count = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN, AEBODSYS)
AESOCA <- adae_ser %>%
  group_by(TRTAN, AEBODSYS) %>%
  summarise(events = sum(!is.na(USUBJID)), .groups = "drop") %>%
  arrange(TRTAN, AEBODSYS)
RESULTSOC <- AESOCD %>%
  left_join(DENOM, by = "TRTAN") %>%
  left_join(AESOCA, by = c("TRTAN", "AEBODSYS")) %>%
  mutate(SUMMARYLEVEL = 2,
         pct = count / denom * 100,
         pct_d = fmt_pct1(pct),
         RESULT = paste0(strip_sas(as.character(count)), " (", strip_sas(pct_d), "%) [", strip_sas(as.character(events)), "]")) %>%
  arrange(AEBODSYS, TRTAN)

# ---------- SUB ----------
AESUBD <- adae_ser %>%
  group_by(TRTAN) %>%
  summarise(count = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN)
AESUBA <- adae_ser %>%
  group_by(TRTAN) %>%
  summarise(events = sum(!is.na(USUBJID)), .groups = "drop") %>%
  arrange(TRTAN)
RESULTSUB <- AESUBD %>%
  left_join(DENOM, by = "TRTAN") %>%
  left_join(AESUBA, by = "TRTAN") %>%
  mutate(SUMMARYLEVEL = 1,
         pct = count / denom * 100,
         pct_d = fmt_pct1(pct),
         RESULT = paste0(strip_sas(as.character(count)), " (", strip_sas(pct_d), "%) [", strip_sas(as.character(events)), "]")) %>%
  arrange(TRTAN)

# ---------- FISHER PT ----------
subj <- adsl_saff %>% distinct(USUBJID, TRTAN = TRT01AN)
pt_list <- adae_ser %>% distinct(AEDECOD)
BACKBONE <- merge(subj, pt_list, by = NULL)
HADEVENT <- adae_ser %>% distinct(STUDYID, USUBJID, TRTAN, AEDECOD)
FISHER <- BACKBONE %>%
  left_join(HADEVENT, by = c("USUBJID", "TRTAN", "AEDECOD")) %>%
  mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1, 0)) %>%
  select(USUBJID, TRTAN, AEDECOD, EVENT) %>%
  arrange(AEDECOD, TRTAN)
stopifnot(all(c("USUBJID","TRTAN","AEDECOD","EVENT") %in% names(FISHER)))

fisher_2x2_p <- function(d, rlev) {
  tt <- table(factor(d$TRTAN, levels = rlev), factor(d$EVENT, levels = c(0, 1)))
  if (sum(tt) == 0) return(list(p = NA_real_, N = 0, a = 0, b = 0, c = 0, e = 0))
  p <- tryCatch(fisher.test(tt, alternative = "two.sided")$p.value, error = function(e) NA_real_)
  list(p = p, N = sum(tt), a = tt[1, 1], b = tt[1, 2], c = tt[2, 1], e = tt[2, 2])
}
pts <- sort(unique(FISHER$AEDECOD))
F1PT_list <- list(); F2PT_list <- list()
for (pt in pts) {
  d02 <- FISHER %>% filter(AEDECOD == pt, TRTAN %in% c(0, 2))
  d01 <- FISHER %>% filter(AEDECOD == pt, TRTAN %in% c(0, 1))
  r02 <- fisher_2x2_p(d02, c(0, 2)); r01 <- fisher_2x2_p(d01, c(0, 1))
  F1PT_list[[pt]] <- data.frame(AEDECOD = pt, p_high = r02$p, N02 = r02$N, stringsAsFactors = FALSE)
  F2PT_list[[pt]] <- data.frame(AEDECOD = pt, p_low = r01$p, N01 = r01$N, stringsAsFactors = FALSE)
}
F1PT <- bind_rows(F1PT_list)
F2PT <- bind_rows(F2PT_list)

# ---------- FISHER SOC ----------
soc_list <- adae_ser %>% distinct(AEBODSYS)
BACKBONE1 <- merge(subj, soc_list, by = NULL)
HADEVENT1 <- adae_ser %>% distinct(STUDYID, USUBJID, TRTAN, AEBODSYS)
FISHER1 <- BACKBONE1 %>%
  left_join(HADEVENT1, by = c("USUBJID", "TRTAN", "AEBODSYS")) %>%
  mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1, 0)) %>%
  select(USUBJID, TRTAN, AEBODSYS, EVENT) %>%
  arrange(AEBODSYS, TRTAN)
stopifnot(all(c("USUBJID","TRTAN","AEBODSYS","EVENT") %in% names(FISHER1)))
socs <- sort(unique(FISHER1$AEBODSYS))
F1SOC_list <- list(); F2SOC_list <- list()
for (s in socs) {
  d02 <- FISHER1 %>% filter(AEBODSYS == s, TRTAN %in% c(0, 2))
  d01 <- FISHER1 %>% filter(AEBODSYS == s, TRTAN %in% c(0, 1))
  r02 <- fisher_2x2_p(d02, c(0, 2)); r01 <- fisher_2x2_p(d01, c(0, 1))
  F1SOC_list[[s]] <- data.frame(AEBODSYS = s, p_high = r02$p, stringsAsFactors = FALSE)
  F2SOC_list[[s]] <- data.frame(AEBODSYS = s, p_low = r01$p, stringsAsFactors = FALSE)
}
F1SOC <- bind_rows(F1SOC_list)
F2SOC <- bind_rows(F2SOC_list)

# ---------- FISHER SUB ----------
BACKBONE2 <- subj
HADEVENT2 <- adae_ser %>% distinct(STUDYID, USUBJID, TRTAN)
FISHER2 <- BACKBONE2 %>%
  left_join(HADEVENT2, by = c("USUBJID", "TRTAN")) %>%
  mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1, 0)) %>%
  select(USUBJID, TRTAN, EVENT) %>%
  arrange(TRTAN)
stopifnot(all(c("USUBJID","TRTAN","EVENT") %in% names(FISHER2)))
d02s <- FISHER2 %>% filter(TRTAN %in% c(0, 2))
d01s <- FISHER2 %>% filter(TRTAN %in% c(0, 1))
r02s <- fisher_2x2_p(d02s, c(0, 2)); r01s <- fisher_2x2_p(d01s, c(0, 1))
F1SUB_p <- r02s$p; F2SUB_p <- r01s$p

# ---------- build wide display ----------
trts <- c(0, 1, 2)
soc_pt_map <- adae_ser %>% distinct(AEBODSYS, AEDECOD) %>% arrange(AEBODSYS, AEDECOD)

pt_grid <- merge(soc_pt_map, data.frame(TRTAN = trts), by = NULL)
pt_full <- pt_grid %>%
  left_join(AEPTD %>% select(TRTAN, AEBODSYS, AEDECOD, count), by = c("TRTAN", "AEBODSYS", "AEDECOD")) %>%
  left_join(AEPTA %>% select(TRTAN, AEBODSYS, AEDECOD, events), by = c("TRTAN", "AEBODSYS", "AEDECOD")) %>%
  left_join(DENOM, by = "TRTAN") %>%
  mutate(count = ifelse(is.na(count), 0, count),
         events = ifelse(is.na(events), 0, events),
         pct = ifelse(denom > 0, count / denom * 100, 0),
         pct_d = fmt_pct1(pct),
         RESULT = paste0(count, " (", pct_d, "%) [", events, "]"))

soc_grid <- merge(data.frame(AEBODSYS = socs, stringsAsFactors = FALSE), data.frame(TRTAN = trts), by = NULL)
soc_full <- soc_grid %>%
  left_join(AESOCD %>% select(TRTAN, AEBODSYS, count), by = c("TRTAN", "AEBODSYS")) %>%
  left_join(AESOCA %>% select(TRTAN, AEBODSYS, events), by = c("TRTAN", "AEBODSYS")) %>%
  left_join(DENOM, by = "TRTAN") %>%
  mutate(count = ifelse(is.na(count), 0, count),
         events = ifelse(is.na(events), 0, events),
         pct = ifelse(denom > 0, count / denom * 100, 0),
         pct_d = fmt_pct1(pct),
         RESULT = paste0(count, " (", pct_d, "%) [", events, "]"))

sub_grid <- data.frame(TRTAN = trts)
sub_full <- sub_grid %>%
  left_join(AESUBD %>% select(TRTAN, count), by = "TRTAN") %>%
  left_join(AESUBA %>% select(TRTAN, events), by = "TRTAN") %>%
  left_join(DENOM, by = "TRTAN") %>%
  mutate(count = ifelse(is.na(count), 0, count),
         events = ifelse(is.na(events), 0, events),
         pct = ifelse(denom > 0, count / denom * 100, 0),
         pct_d = fmt_pct1(pct),
         RESULT = paste0(count, " (", pct_d, "%) [", events, "]"))

get_cell <- function(df, trt, what, soc = NULL, pt = NULL) {
  if (!is.null(pt)) r <- df %>% filter(AEBODSYS == soc, AEDECOD == pt, TRTAN == trt)
  else if (!is.null(soc)) r <- df %>% filter(AEBODSYS == soc, TRTAN == trt)
  else r <- df %>% filter(TRTAN == trt)
  if (nrow(r) == 0) return(NA)
  r[[what]][1]
}

rows <- list()
# overall
rows[[1]] <- data.frame(
  SUMMARYLEVEL = 1, AEBODSYS = "", AEDECOD = "Subjects with Serious TEAE",
  PBO_n = get_cell(sub_full, 0, "count"), PBO_pct = get_cell(sub_full, 0, "pct_d"),
  PBO_ev = get_cell(sub_full, 0, "events"), PBO_res = get_cell(sub_full, 0, "RESULT"),
  LOW_n = get_cell(sub_full, 1, "count"), LOW_pct = get_cell(sub_full, 1, "pct_d"),
  LOW_ev = get_cell(sub_full, 1, "events"), LOW_res = get_cell(sub_full, 1, "RESULT"),
  HIGH_n = get_cell(sub_full, 2, "count"), HIGH_pct = get_cell(sub_full, 2, "pct_d"),
  HIGH_ev = get_cell(sub_full, 2, "events"), HIGH_res = get_cell(sub_full, 2, "RESULT"),
  p_low = F2SUB_p, p_high = F1SUB_p, stringsAsFactors = FALSE
)
k <- 2
for (s in socs) {
  pl <- F2SOC$p_low[F2SOC$AEBODSYS == s]; ph <- F1SOC$p_high[F1SOC$AEBODSYS == s]
  if (length(pl) == 0) pl <- NA_real_; if (length(ph) == 0) ph <- NA_real_
  rows[[k]] <- data.frame(
    SUMMARYLEVEL = 2, AEBODSYS = s, AEDECOD = "",
    PBO_n = get_cell(soc_full, 0, "count", soc = s), PBO_pct = get_cell(soc_full, 0, "pct_d", soc = s),
    PBO_ev = get_cell(soc_full, 0, "events", soc = s), PBO_res = get_cell(soc_full, 0, "RESULT", soc = s),
    LOW_n = get_cell(soc_full, 1, "count", soc = s), LOW_pct = get_cell(soc_full, 1, "pct_d", soc = s),
    LOW_ev = get_cell(soc_full, 1, "events", soc = s), LOW_res = get_cell(soc_full, 1, "RESULT", soc = s),
    HIGH_n = get_cell(soc_full, 2, "count", soc = s), HIGH_pct = get_cell(soc_full, 2, "pct_d", soc = s),
    HIGH_ev = get_cell(soc_full, 2, "events", soc = s), HIGH_res = get_cell(soc_full, 2, "RESULT", soc = s),
    p_low = pl[1], p_high = ph[1], stringsAsFactors = FALSE
  )
  k <- k + 1
  pts_in <- soc_pt_map %>% filter(AEBODSYS == s) %>% pull(AEDECOD) %>% sort()
  for (pt in pts_in) {
    pl2 <- F2PT$p_low[F2PT$AEDECOD == pt]; ph2 <- F1PT$p_high[F1PT$AEDECOD == pt]
    if (length(pl2) == 0) pl2 <- NA_real_; if (length(ph2) == 0) ph2 <- NA_real_
    rows[[k]] <- data.frame(
      SUMMARYLEVEL = 3, AEBODSYS = s, AEDECOD = pt,
      PBO_n = get_cell(pt_full, 0, "count", soc = s, pt = pt), PBO_pct = get_cell(pt_full, 0, "pct_d", soc = s, pt = pt),
      PBO_ev = get_cell(pt_full, 0, "events", soc = s, pt = pt), PBO_res = get_cell(pt_full, 0, "RESULT", soc = s, pt = pt),
      LOW_n = get_cell(pt_full, 1, "count", soc = s, pt = pt), LOW_pct = get_cell(pt_full, 1, "pct_d", soc = s, pt = pt),
      LOW_ev = get_cell(pt_full, 1, "events", soc = s, pt = pt), LOW_res = get_cell(pt_full, 1, "RESULT", soc = s, pt = pt),
      HIGH_n = get_cell(pt_full, 2, "count", soc = s, pt = pt), HIGH_pct = get_cell(pt_full, 2, "pct_d", soc = s, pt = pt),
      HIGH_ev = get_cell(pt_full, 2, "events", soc = s, pt = pt), HIGH_res = get_cell(pt_full, 2, "RESULT", soc = s, pt = pt),
      p_low = pl2[1], p_high = ph2[1], stringsAsFactors = FALSE
    )
    k <- k + 1
  }
}
final_wide <- bind_rows(rows)
final_wide$p_low_d <- fmt_pval(final_wide$p_low)
final_wide$p_high_d <- fmt_pval(final_wide$p_high)

# add denom cols
final_wide$PBO_N <- d0; final_wide$LOW_N <- d1; final_wide$HIGH_N <- d2

# order cols for CSV
csv_out <- final_wide %>% select(SUMMARYLEVEL, AEBODSYS, AEDECOD,
  PBO_N, PBO_n, PBO_pct, PBO_ev, PBO_res,
  LOW_N, LOW_n, LOW_pct, LOW_ev, LOW_res,
  HIGH_N, HIGH_n, HIGH_pct, HIGH_ev, HIGH_res,
  p_low, p_low_d, p_high, p_high_d)

write.csv(csv_out, file.path(out, "P003.csv"), row.names = FALSE, na = "")
cat("wrote P003.csv rows:", nrow(csv_out), "\n")

# ---------- text rendering ----------
txt_path <- file.path(out, "P003.txt")
lab0 <- trt_label(0); lab1 <- trt_label(1); lab2 <- trt_label(2)
sink(txt_path)
cat("Table 14.5-02: Summary of Serious Treatment-Emergent Adverse Events by System Organ Class and Preferred Term\n")
cat("(Safety Population: Subjects with SAFFL='Y'; Serious TEAE: TRTEMFL='Y' and AESER='Y')\n")
cat(paste0("Placebo (N=", d0, ")  ", lab1, " (N=", d1, ")  ", lab2, " (N=", d2, ")"), "\n")
cat("Cell shows n (%) [number of events]; p-values from Fisher's Exact Test (two-sided) vs Placebo\n")
cat("\n")
hdr <- sprintf("%-45s | %-18s | %-18s | %-18s | %-10s | %-10s", "System Organ Class / Preferred Term", paste0("Placebo (N=", d0, ")"), paste0("Low (N=", d1, ")"), paste0("High (N=", d2, ")"), "Low vs PBO", "High vs PBO")
cat(hdr, "\n")
cat(paste(rep("-", nchar(hdr)), collapse = ""), "\n")
for (i in seq_len(nrow(final_wide))) {
  r <- final_wide[i, ]
  if (r$SUMMARYLEVEL == 1) lbl <- "Subjects with Serious TEAE"
  else if (r$SUMMARYLEVEL == 2) lbl <- paste0(r$AEBODSYS)
  else lbl <- paste0("    ", r$AEDECOD)
  if (nchar(lbl) > 45) lbl <- substr(lbl, 1, 45)
  line <- sprintf("%-45s | %-18s | %-18s | %-18s | %-10s | %-10s",
    lbl, r$PBO_res, r$LOW_res, r$HIGH_res, r$p_low_d, r$p_high_d)
  cat(line, "\n")
}
cat("\n")
cat("Notes:\n")
cat("N = number of safety subjects per treatment (TRT01AN/TRTAN: 0=Placebo, 1=Low, 2=High).\n")
cat("n = number of subjects with event; % = 100*n/N (8.1 format, 1 decimal); [events] = total events.\n")
cat("Fisher's Exact Test: BY AEDECOD/AEBODSYS for TRTAN in (0,2) and (0,1), TABLE TRTAN*EVENT / EXACT;\n")
cat("two-sided p-value (XP2_FISH) shown; '<.0001' if <0.0001, else 4 decimals.\n")
cat(sprintf("Overall Low vs Placebo p=%s; Overall High vs Placebo p=%s (N Low+PBO=%d, N High+PBO=%d).\n",
  fmt_pval(F2SUB_p), fmt_pval(F1SUB_p), d0 + d1, d0 + d2))
cat("\nFisher detail (2x2 counts TRTAN x EVENT[0,1], N=table total):\n")
cat("Subject level High vs Placebo: "); cat(paste0("N=", r02s$N, " [0,0]=", r02s$a, " [0,1]=", r02s$b, " [2,0]=", r02s$c, " [2,1]=", r02s$e, " p=", fmt_pval(F1SUB_p)), "\n")
cat("Subject level Low vs Placebo: "); cat(paste0("N=", r01s$N, " [0,0]=", r01s$a, " [0,1]=", r01s$b, " [1,0]=", r01s$c, " [1,1]=", r01s$e, " p=", fmt_pval(F2SUB_p)), "\n")
for (s in socs) {
  d02 <- FISHER1 %>% filter(AEBODSYS == s, TRTAN %in% c(0, 2))
  d01 <- FISHER1 %>% filter(AEBODSYS == s, TRTAN %in% c(0, 1))
  r02 <- fisher_2x2_p(d02, c(0, 2)); r01 <- fisher_2x2_p(d01, c(0, 1))
  cat(paste0("SOC '", s, "' High vs PBO: N=", r02$N, " [0,0]=", r02$a, " [0,1]=", r02$b, " [2,0]=", r02$c, " [2,1]=", r02$e, " p=", fmt_pval(r02$p)), "\n")
  cat(paste0("SOC '", s, "' Low vs PBO: N=", r01$N, " [0,0]=", r01$a, " [0,1]=", r01$b, " [1,0]=", r01$c, " [1,1]=", r01$e, " p=", fmt_pval(r01$p)), "\n")
}
for (pt in pts) {
  d02 <- FISHER %>% filter(AEDECOD == pt, TRTAN %in% c(0, 2))
  d01 <- FISHER %>% filter(AEDECOD == pt, TRTAN %in% c(0, 1))
  r02 <- fisher_2x2_p(d02, c(0, 2)); r01 <- fisher_2x2_p(d01, c(0, 1))
  cat(paste0("PT '", pt, "' High vs PBO: N=", r02$N, " p=", fmt_pval(r02$p), "; Low vs PBO: N=", r01$N, " p=", fmt_pval(r01$p)), "\n")
}
sink()
cat("wrote P003.txt\n")
cat("P003: done\n")

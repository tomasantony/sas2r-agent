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
  sapply(x, function(v) {
    if (is.na(v)) return("")
    sprintf("%.1f", round_half_up(v, 1))
  }, USE.NAMES = FALSE)
}
fmt_pct2 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    sprintf("%.2f", round_half_up(v, 2))
  }, USE.NAMES = FALSE)
}
fmt_pval <- function(p) {
  sapply(p, function(v) {
    if (is.na(v)) return("")
    if (v < 0.0001) return("<.0001")
    sprintf("%.4f", round_half_up(v, 4))
  }, USE.NAMES = FALSE)
}
fmt_p4 <- function(p) {
  sapply(p, function(v) {
    if (is.na(v)) return(".")
    sprintf("%.4f", round_half_up(v, 4))
  }, USE.NAMES = FALSE)
}
strip_sas <- function(x) trimws(x, which = "both")

# ---------- locate inputs ----------
root <- "/bench"
dirs_try <- c(
  file.path(root, "data/derived/cdisc-pilot/adam"),
  file.path(root, "data/raw/cdisc-pilot/adam"),
  "/bench/data/derived/cdisc-pilot/adam",
  "/bench/data/raw/cdisc-pilot/adam"
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

stopifnot(all(c("USUBJID","SAFFL","TRT01AN") %in% names(ADSL)))
stopifnot(all(c("USUBJID","TRTEMFL","AESER","AEBODSYS","AEDECOD","TRTAN") %in% names(ADAE)))
if (!("STUDYID" %in% names(ADAE))) ADAE$STUDYID <- "CDISCPILOT01"

trt_label <- function(an) {
  if ("TRT01P" %in% names(ADSL)) {
    lu <- ADSL %>% distinct(TRT01AN, TRT01P)
    m <- lu$TRT01P[match(an, lu$TRT01AN)]
    ifelse(is.na(m), as.character(an), m)
  } else {
    c("0" = "Placebo", "54" = "Xanomeline Low Dose", "81" = "Xanomeline High Dose",
      "1" = "Xanomeline Low Dose", "2" = "Xanomeline High Dose")[as.character(an)]
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
  if (length(v) == 0) return(NA_integer_) else return(v[1])
}
# actual treatment codes present in data
trt_codes <- sort(unique(DENOM$TRTAN))
cat("trt_codes:", paste(trt_codes, collapse=","), "\n")
# map: smallest non-zero = low, largest = high (pilot 0,54,81)
code_pbo <- 0
code_low <- if (1 %in% trt_codes) 1 else if (54 %in% trt_codes) 54 else trt_codes[trt_codes != 0][1]
code_high <- if (2 %in% trt_codes) 2 else if (81 %in% trt_codes) 81 else tail(trt_codes, 1)
d0 <- denom_of(code_pbo); d1 <- denom_of(code_low); d2 <- denom_of(code_high)
cat(sprintf("codes PBO=%s LOW=%s HIGH=%s denoms %s %s %s\n", code_pbo, code_low, code_high, d0, d1, d2))

adae_ser <- ADAE %>% filter(TRTEMFL == "Y", AESER == "Y")
cat("serious TEAE rows:", nrow(adae_ser), "\n")
print(sort(unique(adae_ser$TRTAN)))
print(sort(unique(adae_ser$AEBODSYS)))
print(sort(unique(adae_ser$AEDECOD)))

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

# ---------- FISHER PT (faithful SAS filters 0,2 and 0,1) ----------
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

# Fisher helper: SAS WHERE TRTAN in (a,b) literally; table uses observed levels
fisher_2x2_p <- function(d, rlev) {
  # d: filtered rows (already WHERE applied)
  if (nrow(d) == 0) return(list(p = NA_real_, N = 0, tab = matrix(0, 0, 0)))
  obs_trt <- sort(unique(d$TRTAN))
  # full 2-col table for EVENT 0,1 including zeros
  tt_full <- table(factor(d$TRTAN, levels = rlev), factor(d$EVENT, levels = c(0, 1)))
  N <- sum(tt_full)
  # p-value: if only one TRTAN level observed -> single-row table -> p=1
  if (length(obs_trt) < 2) {
    p <- 1.0
  } else {
    # use observed 2x2 (rows with data); if zero-margin, fisher gives 1
    tt_obs <- table(factor(d$TRTAN), factor(d$EVENT, levels = c(0, 1)))
    # ensure 2 cols; if only one EVENT level observed, expand
    if (ncol(tt_obs) < 2 || nrow(tt_obs) < 2) {
      p <- 1.0
    } else {
      p <- tryCatch(fisher.test(tt_obs, alternative = "two.sided")$p.value,
                    error = function(e) 1.0)
    }
  }
  # also compute 2x2 with rlev for display (includes zero row if level absent)
  list(p = p, N = N, tab = tt_full,
       a = ifelse("0" %in% rownames(tt_full) || 0 %in% rlev, tt_full[as.character(rlev[1]), "0"], 0),
       b = ifelse("1" %in% colnames(tt_full), tt_full[as.character(rlev[1]), "1"], 0))
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

# ---------- build wide display using ACTUAL codes ----------
trts <- trt_codes  # e.g. 0,54,81
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
rows[[1]] <- data.frame(
  SUMMARYLEVEL = 1, AEBODSYS = "", AEDECOD = "Subjects with Serious TEAE",
  PBO_n = get_cell(sub_full, code_pbo, "count"), PBO_pct = get_cell(sub_full, code_pbo, "pct_d"),
  PBO_ev = get_cell(sub_full, code_pbo, "events"), PBO_res = get_cell(sub_full, code_pbo, "RESULT"),
  LOW_n = get_cell(sub_full, code_low, "count"), LOW_pct = get_cell(sub_full, code_low, "pct_d"),
  LOW_ev = get_cell(sub_full, code_low, "events"), LOW_res = get_cell(sub_full, code_low, "RESULT"),
  HIGH_n = get_cell(sub_full, code_high, "count"), HIGH_pct = get_cell(sub_full, code_high, "pct_d"),
  HIGH_ev = get_cell(sub_full, code_high, "events"), HIGH_res = get_cell(sub_full, code_high, "RESULT"),
  p_low = F2SUB_p, p_high = F1SUB_p, stringsAsFactors = FALSE
)
k <- 2
for (s in socs) {
  pl <- F2SOC$p_low[F2SOC$AEBODSYS == s]; ph <- F1SOC$p_high[F1SOC$AEBODSYS == s]
  if (length(pl) == 0) pl <- NA_real_; if (length(ph) == 0) ph <- NA_real_
  rows[[k]] <- data.frame(
    SUMMARYLEVEL = 2, AEBODSYS = s, AEDECOD = "",
    PBO_n = get_cell(soc_full, code_pbo, "count", soc = s), PBO_pct = get_cell(soc_full, code_pbo, "pct_d", soc = s),
    PBO_ev = get_cell(soc_full, code_pbo, "events", soc = s), PBO_res = get_cell(soc_full, code_pbo, "RESULT", soc = s),
    LOW_n = get_cell(soc_full, code_low, "count", soc = s), LOW_pct = get_cell(soc_full, code_low, "pct_d", soc = s),
    LOW_ev = get_cell(soc_full, code_low, "events", soc = s), LOW_res = get_cell(soc_full, code_low, "RESULT", soc = s),
    HIGH_n = get_cell(soc_full, code_high, "count", soc = s), HIGH_pct = get_cell(soc_full, code_high, "pct_d", soc = s),
    HIGH_ev = get_cell(soc_full, code_high, "events", soc = s), HIGH_res = get_cell(soc_full, code_high, "RESULT", soc = s),
    p_low = pl[1], p_high = ph[1], stringsAsFactors = FALSE
  )
  k <- k + 1
  pts_in <- soc_pt_map %>% filter(AEBODSYS == s) %>% pull(AEDECOD) %>% sort()
  for (pt in pts_in) {
    pl2 <- F2PT$p_low[F2PT$AEDECOD == pt]; ph2 <- F1PT$p_high[F1PT$AEDECOD == pt]
    if (length(pl2) == 0) pl2 <- NA_real_; if (length(ph2) == 0) ph2 <- NA_real_
    rows[[k]] <- data.frame(
      SUMMARYLEVEL = 3, AEBODSYS = s, AEDECOD = pt,
      PBO_n = get_cell(pt_full, code_pbo, "count", soc = s, pt = pt), PBO_pct = get_cell(pt_full, code_pbo, "pct_d", soc = s, pt = pt),
      PBO_ev = get_cell(pt_full, code_pbo, "events", soc = s, pt = pt), PBO_res = get_cell(pt_full, code_pbo, "RESULT", soc = s, pt = pt),
      LOW_n = get_cell(pt_full, code_low, "count", soc = s, pt = pt), LOW_pct = get_cell(pt_full, code_low, "pct_d", soc = s, pt = pt),
      LOW_ev = get_cell(pt_full, code_low, "events", soc = s, pt = pt), LOW_res = get_cell(pt_full, code_low, "RESULT", soc = s, pt = pt),
      HIGH_n = get_cell(pt_full, code_high, "count", soc = s, pt = pt), HIGH_pct = get_cell(pt_full, code_high, "pct_d", soc = s, pt = pt),
      HIGH_ev = get_cell(pt_full, code_high, "events", soc = s, pt = pt), HIGH_res = get_cell(pt_full, code_high, "RESULT", soc = s, pt = pt),
      p_low = pl2[1], p_high = ph2[1], stringsAsFactors = FALSE
    )
    k <- k + 1
  }
}
final_wide <- bind_rows(rows)
final_wide$p_low_d <- fmt_pval(final_wide$p_low)
final_wide$p_high_d <- fmt_pval(final_wide$p_high)
final_wide$p_low_4 <- fmt_p4(final_wide$p_low)
final_wide$p_high_4 <- fmt_p4(final_wide$p_high)

final_wide$PBO_N <- d0; final_wide$LOW_N <- d1; final_wide$HIGH_N <- d2
final_wide$PBO_code <- code_pbo; final_wide$LOW_code <- code_low; final_wide$HIGH_code <- code_high

csv_out <- final_wide %>% select(SUMMARYLEVEL, AEBODSYS, AEDECOD,
  PBO_N, PBO_n, PBO_pct, PBO_ev, PBO_res,
  LOW_N, LOW_n, LOW_pct, LOW_ev, LOW_res,
  HIGH_N, HIGH_n, HIGH_pct, HIGH_ev, HIGH_res,
  p_low, p_low_d, p_high, p_high_d)

write.csv(csv_out, file.path(out, "P003.csv"), row.names = FALSE, na = "")
cat("wrote P003.csv rows:", nrow(csv_out), "\n")

# ---------- PROC FREQ style detail helper ----------
freq_block_lines <- function(d, rlev, title) {
  # d already WHERE-filtered; build full table TRTAN(rlev) x EVENT(0,1)
  tt <- table(factor(d$TRTAN, levels = rlev), factor(d$EVENT, levels = c(0, 1)))
  N <- sum(tt)
  R <- nrow(tt); C <- ncol(tt)
  lines <- c(paste0(title), paste0("Sample Size = ", N),
             paste0("Table of TRTAN by EVENT (Frequency | Percent | Row Pct | Col Pct)"))
  # header row with EVENT levels and total
  lines <- c(lines, paste0("TRTAN \\ EVENT: 0 | 1 | Total"))
  row_tot <- rowSums(tt); col_tot <- colSums(tt)
  for (rn in rownames(tt)) {
    cells <- c()
    for (cn in colnames(tt)) {
      f <- tt[rn, cn]
      pct <- if (N > 0) f / N * 100 else 0
      rowp <- if (row_tot[rn] > 0) f / row_tot[rn] * 100 else 0
      colp <- if (col_tot[cn] > 0) f / col_tot[cn] * 100 else 0
      cells <- c(cells, paste0(f, " | ", fmt_pct2(pct), " | ", fmt_pct2(rowp), " | ", fmt_pct2(colp)))
    }
    ftot <- row_tot[rn]; pctt <- if (N > 0) ftot / N * 100 else 0
    lines <- c(lines, paste0("TRTAN=", rn, ": ", paste(cells, collapse=" || "), " || Total ", ftot, " | ", fmt_pct2(pctt), " | ", fmt_pct2(100), " | ."))
  }
  # total row
  tcells <- c()
  for (cn in colnames(tt)) {
    f <- col_tot[cn]; pct <- if (N > 0) f / N * 100 else 0
    tcells <- c(tcells, paste0(f, " | ", fmt_pct2(pct)))
  }
  lines <- c(lines, paste0("Total: ", paste(tcells, collapse=" || "), " || Total ", N, " | ", fmt_pct2(100), " | . | ."))
  # col pct 100 check row
  lines <- c(lines, paste0("Column totals: EVENT=0 ", col_tot["0"], " EVENT=1 ", col_tot["1"], " N ", N))
  # Fisher
  obs_trt <- sort(unique(d$TRTAN))
  if (length(obs_trt) < 2) { p <- 1.0 } else {
    tt_obs <- table(factor(d$TRTAN), factor(d$EVENT, levels = c(0, 1)))
    if (nrow(tt_obs) < 2 || ncol(tt_obs) < 2) p <- 1.0
    else p <- tryCatch(fisher.test(tt_obs, alternative = "two.sided")$p.value, error = function(e) 1.0)
  }
  # table probability (hypergeometric) for display: use dhyper for 2x2 observed if 2x2 else 1
  lines <- c(lines, "Fisher's Exact Test",
             paste0("Cell (1,1) Frequency (F) ", tt[1, 1]),
             paste0("Table Probability (P) ", fmt_p4(tryCatch(fisher.test(table(factor(d$TRTAN), factor(d$EVENT))), error=function(e) NA_real_)$p.value)),
             paste0("Two-sided Pr <= P ", fmt_p4(p)),
             paste0("Two-sided Pr <= P (XP2_FISH) ", fmt_p4(p), " ", fmt_pval(p)))
  lines
}

# ---------- text rendering ----------
txt_path <- file.path(out, "P003.txt")
lab0 <- trt_label(code_pbo); lab1 <- trt_label(code_low); lab2 <- trt_label(code_high)
sink(txt_path)
cat("Table 14.5-02: Summary of Serious Treatment-Emergent Adverse Events by System Organ Class and Preferred Term\n")
cat("(Safety Population: Subjects with SAFFL='Y'; Serious TEAE: TRTEMFL='Y' and AESER='Y')\n")
cat(paste0("Placebo (TRTAN=", code_pbo, ") (N=", d0, ")  ", lab1, " (TRTAN=", code_low, ") (N=", d1, ")  ", lab2, " (TRTAN=", code_high, ") (N=", d2, ")"), "\n")
cat("Cell shows n (%) [number of events]; p-values from Fisher's Exact Test (two-sided) vs Placebo\n")
cat("where TRTAN in (0,2) for High vs Placebo and TRTAN in (0,1) for Low vs Placebo (SAS literals)\n")
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
cat("N = number of safety subjects per treatment (TRT01AN/TRTAN).\n")
cat(paste0("Placebo N=", d0, " Low (TRTAN=", code_low, ") N=", d1, " High (TRTAN=", code_high, ") N=", d2, " Total N=", sum(DENOM$denom), ".\n"))
cat("n = number of subjects with event; % = 100*n/N (8.1 format, 1 decimal); [events] = total events.\n")
cat("Fisher's Exact Test: BY AEDECOD/AEBODSYS for TRTAN in (0,2) and (0,1), TABLE TRTAN*EVENT / EXACT;\n")
cat("two-sided p-value (XP2_FISH) shown; '<.0001' if <0.0001, else 4 decimals.\n")
cat(sprintf("Overall Low vs Placebo (TRTAN in (0,1)) p=%s (%s); Overall High vs Placebo (TRTAN in (0,2)) p=%s (%s) (N Low+PBO subset=%d, N High+PBO subset=%d).\n",
  fmt_pval(F2SUB_p), fmt_p4(F2SUB_p), fmt_pval(F1SUB_p), fmt_p4(F1SUB_p), r01s$N, r02s$N))
cat("\n")
cat("Descriptive counts (RESULTPT/RESULTSOC/RESULTSUB with SUMMARYLEVEL 3/2/1):\n")
for (i in seq_len(nrow(RESULTSUB))) {
  cat(paste0("SUBJECT TRTAN=", RESULTSUB$TRTAN[i], " count=", RESULTSUB$count[i], " denom=", RESULTSUB$DENOM[i],
    " pct=", fmt_pct1(RESULTSUB$count[i]/RESULTSUB$DENOM[i]*100), " (", fmt_pct2(RESULTSUB$count[i]/RESULTSUB$DENOM[i]*100), ") events=", RESULTSUB$events[i],
    " RESULT=", RESULTSUB$RESULT[i], "\n"))
}
for (i in seq_len(nrow(RESULTSOC))) {
  cat(paste0("SOC '", RESULTSOC$AEBODSYS[i], "' TRTAN=", RESULTSOC$TRTAN[i], " count=", RESULTSOC$count[i], " denom=", RESULTSOC$DENOM[i],
    " pct=", fmt_pct1(RESULTSOC$count[i]/RESULTSOC$DENOM[i]*100), " (", fmt_pct2(RESULTSOC$count[i]/RESULTSOC$DENOM[i]*100), ") events=", RESULTSOC$events[i],
    " RESULT=", RESULTSOC$RESULT[i], "\n"))
}
for (i in seq_len(nrow(RESULTPT))) {
  cat(paste0("PT '", RESULTPT$AEBODSYS[i], "' / '", RESULTPT$AEDECOD[i], "' TRTAN=", RESULTPT$TRTAN[i], " count=", RESULTPT$count[i], " denom=", RESULTPT$DENOM[i],
    " pct=", fmt_pct1(RESULTPT$count[i]/RESULTPT$DENOM[i]*100), " (", fmt_pct2(RESULTPT$count[i]/RESULTPT$DENOM[i]*100), ") events=", RESULTPT$events[i],
    " RESULT=", RESULTPT$RESULT[i], "\n"))
}
cat("\nPROC FREQ data=FISHER2 where TRTAN in (0,2); table TRTAN*EVENT / exact;  (HIGH DOSE VERSUS PLACEBO - SUBJECT LEVEL)\n")
for (ln in freq_block_lines(d02s, c(0, 2), "Subject level High vs Placebo")) cat(ln, "\n")
cat("\nPROC FREQ data=FISHER2 where TRTAN in (0,1); table TRTAN*EVENT / exact;  (LOW DOSE VERSUS PLACEBO - SUBJECT LEVEL)\n")
for (ln in freq_block_lines(d01s, c(0, 1), "Subject level Low vs Placebo")) cat(ln, "\n")
for (s in socs) {
  d02 <- FISHER1 %>% filter(AEBODSYS == s, TRTAN %in% c(0, 2))
  d01 <- FISHER1 %>% filter(AEBODSYS == s, TRTAN %in% c(0, 1))
  cat(paste0("\nPROC FREQ data=FISHER1; by AEBODSYS='", s, "'; where TRTAN in (0,2); table TRTAN*EVENT / exact; (HIGH vs PLACEBO - SOC LEVEL)\n"))
  for (ln in freq_block_lines(d02, c(0, 2), paste0("SOC '", s, "' High vs Placebo"))) cat(ln, "\n")
  cat(paste0("\nPROC FREQ data=FISHER1; by AEBODSYS='", s, "'; where TRTAN in (0,1); table TRTAN*EVENT / exact; (LOW vs PLACEBO - SOC LEVEL)\n"))
  for (ln in freq_block_lines(d01, c(0, 1), paste0("SOC '", s, "' Low vs Placebo"))) cat(ln, "\n")
}
for (pt in pts) {
  d02 <- FISHER %>% filter(AEDECOD == pt, TRTAN %in% c(0, 2))
  d01 <- FISHER %>% filter(AEDECOD == pt, TRTAN %in% c(0, 1))
  cat(paste0("\nPROC FREQ data=FISHER; by AEDECOD='", pt, "'; where TRTAN in (0,2); table TRTAN*EVENT / exact; (HIGH vs PLACEBO - PT LEVEL)\n"))
  for (ln in freq_block_lines(d02, c(0, 2), paste0("PT '", pt, "' High vs Placebo"))) cat(ln, "\n")
  cat(paste0("\nPROC FREQ data=FISHER; by AEDECOD='", pt, "'; where TRTAN in (0,1); table TRTAN*EVENT / exact; (LOW vs PLACEBO - PT LEVEL)\n"))
  for (ln in freq_block_lines(d01, c(0, 1), paste0("PT '", pt, "' Low vs Placebo"))) cat(ln, "\n")
}
sink()
cat("wrote P003.txt\n")
cat("P003: done\n")

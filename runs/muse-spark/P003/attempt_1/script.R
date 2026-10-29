# P003 at14-5-02.sas -> R translation
# Serious TEAE summary (descriptive + Fisher exact) - CDISC pilot
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_dir    <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
rawadam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(purrr)
})

round_half_up <- function(x, digits = 0) {
  if (!is.numeric(x)) return(x)
  mult <- 10^digits
  # SAS rounds half away from zero
  res <- ifelse(is.na(x), NA_real_,
    ifelse(x >= 0, floor(x * mult + 0.5 + 1e-9) / mult,
                     ceiling(x * mult - 0.5 - 1e-9) / mult))
  res
}
sas_pct1 <- function(pct) {
  # PUT(...,8.1) + STRIP -> 1 decimal, half away
  r <- round_half_up(pct, 1)
  sprintf("%.1f", r)
}
sas_best_int <- function(x) {
  # PUT(count,best.) + STRIP for integer counts
  if (is.na(x)) return("")
  formatC(as.integer(x), format = "d")
}
sas_result <- function(count, denom, events) {
  # strip(put(count,best.))||' ('||strip(put((count/denom)*100,8.1))||'%) ['||strip(put(events,best.))||']'
  if (is.na(count)) return(NA_character_)
  pct <- (count / denom) * 100
  paste0(sas_best_int(count), " (", sas_pct1(pct), "%) [", sas_best_int(events), "]")
}
sas_p4 <- function(p) {
  if (is.na(p)) return("")
  r <- round_half_up(p, 4)
  # clamp to [0,1]
  if (r < 0) r <- 0
  if (r > 1) r <- 1
  sprintf("%.4f", r)
}

read_adam_ds <- function(ds) {
  ds_up <- toupper(ds)
  ds_lo <- tolower(ds)
  cand <- c(
    file.path(adam_dir, paste0(ds_up, ".sas7bdat")),
    file.path(adam_dir, paste0(ds, ".sas7bdat")),
    file.path(adam_dir, paste0(ds_lo, ".sas7bdat")),
    file.path(adam_dir, paste0(ds_up, ".xpt")),
    file.path(adam_dir, paste0(ds, ".xpt")),
    file.path(adam_dir, paste0(ds_lo, ".xpt")),
    file.path(rawadam_dir, paste0(ds_up, ".xpt")),
    file.path(rawadam_dir, paste0(ds, ".xpt")),
    file.path(rawadam_dir, paste0(ds_lo, ".xpt"))
  )
  # also try listing directory case-insensitively
  for (d in unique(c(adam_dir, rawadam_dir))) {
    if (dir.exists(d)) {
      fl <- list.files(d, full.names = TRUE)
      for (f in fl) {
        bn <- tools::file_path_sans_ext(basename(f))
        if (toupper(bn) == ds_up) cand <- c(cand, f)
      }
    }
  }
  cand <- unique(cand)
  for (f in cand) {
    if (!is.na(f) && file.exists(f)) {
      cat("Reading", ds, "from", f, "\n")
      ext <- tolower(tools::file_ext(f))
      if (ext == "xpt") {
        df <- haven::read_xpt(f)
      } else {
        df <- haven::read_sas(f)
      }
      names(df) <- toupper(names(df))
      return(as.data.frame(df, stringsAsFactors = FALSE))
    }
  }
  stop(paste0("Cannot find dataset ", ds))
}

cat("Reading ADSL, ADAE\n")
ADSL <- read_adam_ds("ADSL")
ADAE <- read_adam_ds("ADAE")

# Ensure needed vars exist uppercase
# TRTAN alias for TRT01AN
# DENOM: COUNT distinct usubjid by TRT01AN where SAFFL='Y'
adsl_saff <- ADSL %>% filter(!is.na(SAFFL) & SAFFL == "Y")
# distinct subjects per treatment (SAS COUNT DISTINCT)
DENOM <- adsl_saff %>%
  group_by(TRTAN = TRT01AN) %>%
  summarise(DENOM = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN)
cat("DENOM:\n")
print(DENOM)

# treatment labels if available
trt_lab <- adsl_saff %>%
  filter(!is.na(TRT01AN)) %>%
  distinct(TRTAN = TRT01AN, TRT01P = if ("TRT01P" %in% names(.)) TRT01P else NA_character_)
# fallback labels
get_trt_label <- function(an) {
  if (nrow(trt_lab) > 0 && "TRT01P" %in% names(trt_lab)) {
    v <- trt_lab$TRT01P[trt_lab$TRTAN == an]
    if (length(v) > 0 && !is.na(v[1]) && v[1] != "") return(v[1])
  }
  if (an == 0) return("Placebo")
  if (an == 1) return("Xanomeline Low Dose")
  if (an == 2) return("Xanomeline High Dose")
  paste0("TRT", an)
}

adae_ser <- ADAE %>% filter(!is.na(TRTEMFL) & TRTEMFL == "Y", !is.na(AESER) & AESER == "Y")

# AEPTD: unique subjects by SOC PT TRT
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
  left_join(AEPTA, by = c("TRTAN", "AEBODSYS", "AEDECOD")) %>%
  mutate(SUMMARYLEVEL = 3L,
         PCT = (count / DENOM) * 100,
         PCT_DISP = sas_pct1(PCT),
         RESULT = mapply(sas_result, count, DENOM, events, USE.NAMES = FALSE)) %>%
  arrange(AEBODSYS, AEDECOD, TRTAN)

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
  left_join(AESOCA, by = c("TRTAN", "AEBODSYS")) %>%
  mutate(SUMMARYLEVEL = 2L,
         PCT = (count / DENOM) * 100,
         PCT_DISP = sas_pct1(PCT),
         RESULT = mapply(sas_result, count, DENOM, events, USE.NAMES = FALSE)) %>%
  arrange(AEBODSYS, TRTAN)

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
  mutate(SUMMARYLEVEL = 1L,
         PCT = (count / DENOM) * 100,
         PCT_DISP = sas_pct1(PCT),
         RESULT = mapply(sas_result, count, DENOM, events, USE.NAMES = FALSE)) %>%
  arrange(TRTAN)

cat("RESULTSUB:\n")
print(as.data.frame(RESULTSUB))

# ---- Inferential: PT level ----
adsl_bb <- adsl_saff %>% distinct(USUBJID, TRTAN = TRT01AN)
pt_list <- adae_ser %>% distinct(AEDECOD)
soc_list <- adae_ser %>% distinct(AEBODSYS)

# BACKBONE cross join (SAS: select * from A, B)
BACKBONE <- merge(adsl_bb, pt_list, by = NULL)
HADEVENT <- adae_ser %>% distinct(STUDYID, USUBJID, TRTAN, AEDECOD)
FISHER <- BACKBONE %>%
  left_join(HADEVENT, by = c("USUBJID", "TRTAN", "AEDECOD")) %>%
  mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1L, 0L)) %>%
  arrange(AEDECOD, TRTAN)

fisher_2x2_p <- function(d, trts = c(0, 2)) {
  dd <- d %>% filter(TRTAN %in% trts)
  # build 2x2: rows TRT, cols EVENT 1/0
  # ensure both levels present
  t1 <- trts[1]; t2 <- trts[2]
  a <- sum(dd$TRTAN == t2 & dd$EVENT == 1)
  b <- sum(dd$TRTAN == t2 & dd$EVENT == 0)
  c <- sum(dd$TRTAN == t1 & dd$EVENT == 1)
  d_ <- sum(dd$TRTAN == t1 & dd$EVENT == 0)
  m <- matrix(c(a, b, c, d_), nrow = 2, byrow = TRUE)
  # fisher.test two-sided (XP2_FISH)
  p <- tryCatch(suppressWarnings(fisher.test(m, alternative = "two.sided")$p.value), error = function(e) NA_real_)
  # also return counts for listing
  list(p = p, a = a, b = b, c = c, d = d_)
}

# PT p-values
pts <- sort(unique(FISHER$AEDECOD))
F1PT <- lapply(pts, function(pt) {
  r <- fisher_2x2_p(FISHER %>% filter(AEDECOD == pt), c(0, 2))
  data.frame(AEDECOD = pt, P_HIGH_VS_PBO = r$p, stringsAsFactors = FALSE)
}) %>% bind_rows()
F2PT <- lapply(pts, function(pt) {
  r <- fisher_2x2_p(FISHER %>% filter(AEDECOD == pt), c(0, 1))
  data.frame(AEDECOD = pt, P_LOW_VS_PBO = r$p, stringsAsFactors = FALSE)
}) %>% bind_rows()

# SOC level
BACKBONE1 <- merge(adsl_bb, soc_list, by = NULL)
HADEVENT1 <- adae_ser %>% distinct(STUDYID, USUBJID, TRTAN, AEBODSYS)
FISHER1 <- BACKBONE1 %>%
  left_join(HADEVENT1, by = c("USUBJID", "TRTAN", "AEBODSYS")) %>%
  mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1L, 0L)) %>%
  arrange(AEBODSYS, TRTAN)
socs <- sort(unique(FISHER1$AEBODSYS))
F1SOC <- lapply(socs, function(s) {
  r <- fisher_2x2_p(FISHER1 %>% filter(AEBODSYS == s), c(0, 2))
  data.frame(AEBODSYS = s, P_HIGH_VS_PBO = r$p, stringsAsFactors = FALSE)
}) %>% bind_rows()
F2SOC <- lapply(socs, function(s) {
  r <- fisher_2x2_p(FISHER1 %>% filter(AEDECOD == s), c(0, 1))
  # note: use AEBODSYS filter; correct below
  r <- fisher_2x2_p(FISHER1 %>% filter(AEBODSYS == s), c(0, 1))
  data.frame(AEBODSYS = s, P_LOW_VS_PBO = r$p, stringsAsFactors = FALSE)
}) %>% bind_rows()

# Subject level
BACKBONE2 <- adsl_bb
HADEVENT2 <- adae_ser %>% distinct(STUDYID, USUBJID, TRTAN)
FISHER2 <- BACKBONE2 %>%
  left_join(HADEVENT2, by = c("USUBJID", "TRTAN")) %>%
  mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1L, 0L)) %>%
  arrange(TRTAN)
r_high_sub <- fisher_2x2_p(FISHER2, c(0, 2))
r_low_sub  <- fisher_2x2_p(FISHER2, c(0, 1))
F1SUB <- data.frame(P_HIGH_VS_PBO = r_high_sub$p)
F2SUB <- data.frame(P_LOW_VS_PBO = r_low_sub$p)
cat("Subject-level Fisher p (high vs pbo):", F1SUB$P_HIGH_VS_PBO, " low vs pbo:", F2SUB$P_LOW_VS_PBO, "\n")

# ---- Build final summary table ----
denom0 <- DENOM$DENOM[DENOM$TRTAN == 0]; if (length(denom0)==0) denom0 <- NA
denom1 <- DENOM$DENOM[DENOM$TRTAN == 1]; if (length(denom1)==0) denom1 <- NA
denom2 <- DENOM$DENOM[DENOM$TRTAN == 2]; if (length(denom2)==0) denom2 <- NA

get_res <- function(df, trt, soc = NULL, pt = NULL) {
  if (!is.null(pt)) {
    r <- df %>% filter(TRTAN == trt, AEBODSYS == soc, AEDECOD == pt)
  } else if (!is.null(soc)) {
    r <- df %>% filter(TRTAN == trt, AEBODSYS == soc)
  } else {
    r <- df %>% filter(TRTAN == trt)
  }
  if (nrow(r) == 0) return(list(RESULT = "0 (0.0%) [0]", count = 0L, pct = "0.0", events = 0L, denom = NA))
  list(RESULT = r$RESULT[1], count = r$count[1], pct = r$PCT_DISP[1], events = r$events[1], denom = r$DENOM[1])
}

# overall row
rows <- list()
o0 <- get_res(RESULTSUB, 0); o1 <- get_res(RESULTSUB, 1); o2 <- get_res(RESULTSUB, 2)
rows[[1]] <- data.frame(
  SUMMARYLEVEL = 1L, AEBODSYS = "", AEDECOD = "Subjects with Serious TEAE",
  PBO_RESULT = o0$RESULT, LOW_RESULT = o1$RESULT, HIGH_RESULT = o2$RESULT,
  PBO_COUNT = o0$count, PBO_PCT = o0$pct, PBO_EVENTS = o0$events,
  LOW_COUNT = o1$count, LOW_PCT = o1$pct, LOW_EVENTS = o1$events,
  HIGH_COUNT = o2$count, HIGH_PCT = o2$pct, HIGH_EVENTS = o2$events,
  P_LOW_VS_PBO = F2SUB$P_LOW_VS_PBO, P_HIGH_VS_PBO = F1SUB$P_HIGH_VS_PBO,
  stringsAsFactors = FALSE
)
idx <- 2
# SAS order: RESULTSOC ordered by AEBODSYS, TRTAN; RESULTPT by AEBODSYS, AEDECOD, TRTAN
# Use sorted unique SOCs (SAS character ordering: binary; use C locale sort via order with radix)
soc_sorted <- sort(unique(adae_ser$AEBODSYS))
for (s in soc_sorted) {
  s0 <- get_res(RESULTSOC, 0, soc = s); s1 <- get_res(RESULTSOC, 1, soc = s); s2 <- get_res(RESULTSOC, 2, soc = s)
  p_low <- F2SOC$P_LOW_VS_PBO[F2SOC$AEBODSYS == s]; if (length(p_low)==0) p_low <- NA
  p_high <- F1SOC$P_HIGH_VS_PBO[F1SOC$AEBODSYS == s]; if (length(p_high)==0) p_high <- NA
  rows[[idx]] <- data.frame(
    SUMMARYLEVEL = 2L, AEBODSYS = s, AEDECOD = "",
    PBO_RESULT = s0$RESULT, LOW_RESULT = s1$RESULT, HIGH_RESULT = s2$RESULT,
    PBO_COUNT = s0$count, PBO_PCT = s0$pct, PBO_EVENTS = s0$events,
    LOW_COUNT = s1$count, LOW_PCT = s1$pct, LOW_EVENTS = s1$events,
    HIGH_COUNT = s2$count, HIGH_PCT = s2$pct, HIGH_EVENTS = s2$events,
    P_LOW_VS_PBO = p_low, P_HIGH_VS_PBO = p_high,
    stringsAsFactors = FALSE
  )
  idx <- idx + 1
  pts_in_soc <- sort(unique(adae_ser$AEDECOD[adae_ser$AEBODSYS == s]))
  for (pt in pts_in_soc) {
    p0 <- get_res(RESULTPT, 0, soc = s, pt = pt); p1 <- get_res(RESULTPT, 1, soc = s, pt = pt); p2 <- get_res(RESULTPT, 2, soc = s, pt = pt)
    # p-values are by PT only (not SOC-specific in FISHER); match on AEDECOD
    pl <- F2PT$P_LOW_VS_PBO[F2PT$AEDECOD == pt]; if (length(pl)==0) pl <- NA
    ph <- F1PT$P_HIGH_VS_PBO[F1PT$AEDECOD == pt]; if (length(ph)==0) ph <- NA
    rows[[idx]] <- data.frame(
      SUMMARYLEVEL = 3L, AEBODSYS = s, AEDECOD = pt,
      PBO_RESULT = p0$RESULT, LOW_RESULT = p1$RESULT, HIGH_RESULT = p2$RESULT,
      PBO_COUNT = p0$count, PBO_PCT = p0$pct, PBO_EVENTS = p0$events,
      LOW_COUNT = p1$count, LOW_PCT = p1$pct, LOW_EVENTS = p1$events,
      HIGH_COUNT = p2$count, HIGH_PCT = p2$pct, HIGH_EVENTS = p2$events,
      P_LOW_VS_PBO = pl, P_HIGH_VS_PBO = ph,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1
  }
}
FINAL <- bind_rows(rows)
FINAL$PBO_DENOM <- denom0
FINAL$LOW_DENOM <- denom1
FINAL$HIGH_DENOM <- denom2
# order: already overall then SOC/PT; keep as built
# add display p-values
FINAL$P_LOW_DISP <- sapply(FINAL$P_LOW_VS_PBO, sas_p4)
FINAL$P_HIGH_DISP <- sapply(FINAL$P_HIGH_VS_PBO, sas_p4)

cat(paste0("FINAL rows: ", nrow(FINAL), "\n"))

# ---- Write CSV of table body ----
csv_path <- file.path(out, "P003.csv")
write.csv(FINAL, csv_path, row.names = FALSE, na = "")
cat(paste0("Wrote ", csv_path, "\n"))

# ---- Write plain-text rendering (P003.txt and P003.lst) ----
lab0 <- get_trt_label(0); lab1 <- get_trt_label(1); lab2 <- get_trt_label(2)
hdr_N <- sprintf("Placebo N=%s | %s N=%s | %s N=%s",
                 sas_best_int(denom0), lab1, sas_best_int(denom1), lab2, sas_best_int(denom2))
# Build fixed-width text table
# Columns: SOC / PT description, PBO, LOW, HIGH, p-values
txt_lines <- c(
  "Table: Serious Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
  "(Safety population, TRTEMFL=Y and AESER=Y)",
  "",
  paste0("Treatment groups: Placebo (TRTAN=0), Low Dose (TRTAN=1), High Dose (TRTAN=2)"),
  paste0("Denominators (SAFFL=Y): Placebo N=", sas_best_int(denom0),
         ", Low N=", sas_best_int(denom1),
         ", High N=", sas_best_int(denom2)),
  paste0("Placebo: ", lab0, " | Low: ", lab1, " | High: ", lab2),
  paste0("Fisher's exact two-sided p-values (XP2_FISH): Low vs Placebo and High vs Placebo"),
  paste0("Result format: n (%) [number of events]; % = 100*count/denom"),
  ""
)
# header row
col_desc <- sprintf("%-42s", "System Organ Class / Preferred Term")
col_pbo  <- sprintf("%-18s", paste0("Placebo (N=", sas_best_int(denom0), ")"))
col_low  <- sprintf("%-18s", paste0("Low (N=", sas_best_int(denom1), ")"))
col_high <- sprintf("%-18s", paste0("High (N=", sas_best_int(denom2), ")"))
col_pl   <- sprintf("%-12s", "p Low-Pbo")
col_ph   <- sprintf("%-12s", "p High-Pbo")
txt_lines <- c(txt_lines, paste0(col_desc, " ", col_pbo, " ", col_low, " ", col_high, " ", col_pl, " ", col_ph))
txt_lines <- c(txt_lines, paste0(strrep("-", 42), " ", strrep("-", 18), " ", strrep("-", 18), " ", strrep("-", 18), " ", strrep("-", 12), " ", strrep("-", 12)))
for (i in seq_len(nrow(FINAL))) {
  r <- FINAL[i, ]
  if (r$SUMMARYLEVEL == 1) {
    desc <- "Any Serious TEAE"
  } else if (r$SUMMARYLEVEL == 2) {
    desc <- paste0(r$AEBODSYS)
  } else {
    desc <- paste0("  ", r$AEDECOD)
  }
  if (nchar(desc) > 42) desc <- substr(desc, 1, 42)
  dcol <- sprintf("%-42s", desc)
  pbo  <- sprintf("%-18s", r$PBO_RESULT)
  low  <- sprintf("%-18s", r$LOW_RESULT)
  high <- sprintf("%-18s", r$HIGH_RESULT)
  pl   <- sprintf("%-12s", r$P_LOW_DISP)
  ph   <- sprintf("%-12s", r$P_HIGH_DISP)
  txt_lines <- c(txt_lines, paste0(dcol, " ", pbo, " ", low, " ", high, " ", pl, " ", ph))
}
txt_lines <- c(txt_lines, "",
  "Notes: n = number of subjects with event; % = 100*n/denom (8.1 format, 1 decimal); [ ] = total number of events.",
  "Fisher's exact test based on subject counts (event vs no event) within SAFFL=Y backbone; two-sided XP2_FISH.",
  paste0("Overall High vs Placebo p=", sas_p4(F1SUB$P_HIGH_VS_PBO), "; Low vs Placebo p=", sas_p4(F2SUB$P_LOW_VS_PBO), ".")
)
# add detail section reproducing PROC FREQ counts for verification (subject-level 2x2)
txt_lines <- c(txt_lines, "",
  "Detail: 2x2 subject counts (EVENT=1 / EVENT=0) and Fisher exact p-values by PT and SOC:",
  "")
for (pt in pts) {
  # high vs pbo and low vs pbo counts
  dpt <- FISHER %>% filter(AEDECOD == pt)
  rh <- fisher_2x2_p(dpt, c(0,2)); rl <- fisher_2x2_p(dpt, c(0,1))
  txt_lines <- c(txt_lines, paste0("PT=", pt,
    " | PBO 1/0=", rh$c, "/", rh$d, " High 1/0=", rh$a, "/", rh$b, " pHigh=", sas_p4(F1PT$P_HIGH_VS_PBO[F1PT$AEDECOD==pt]),
    " | Low 1/0=", rl$a, "/", rl$b, " pLow=", sas_p4(F2PT$P_LOW_VS_PBO[F2PT$AEDECOD==pt])))
}
for (s in socs) {
  ds <- FISHER1 %>% filter(AEBODSYS == s)
  rh <- fisher_2x2_p(ds, c(0,2)); rl <- fisher_2x2_p(ds, c(0,1))
  txt_lines <- c(txt_lines, paste0("SOC=", s,
    " | PBO 1/0=", rh$c, "/", rh$d, " High 1/0=", rh$a, "/", rh$b, " pHigh=", sas_p4(F1SOC$P_HIGH_VS_PBO[F1SOC$AEBODSYS==s]),
    " | Low 1/0=", rl$a, "/", rl$b, " pLow=", sas_p4(F2SOC$P_LOW_VS_PBO[F2SOC$AEBODSYS==s])))
}
txt_lines <- c(txt_lines, paste0("OVERALL PBO 1/0=", r_high_sub$c, "/", r_high_sub$d,
  " High 1/0=", r_high_sub$a, "/", r_high_sub$b, " pHigh=", sas_p4(F1SUB$P_HIGH_VS_PBO),
  " Low 1/0=", r_low_sub$a, "/", r_low_sub$b, " pLow=", sas_p4(F2SUB$P_LOW_VS_PBO)))

txt_path <- file.path(out, "P003.txt")
writeLines(txt_lines, txt_path)
cat(paste0("Wrote ", txt_path, "\n"))
# also write .lst copy for benchmark expecting P003.lst
lst_path <- file.path(out, "P003.lst")
writeLines(txt_lines, lst_path)
cat(paste0("Wrote ", lst_path, "\n"))

cat("Done\n")

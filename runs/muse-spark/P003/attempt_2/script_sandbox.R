# P003 at14-5-02.sas -> R translation (attempt 2)
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_dir    <- "/bench/data/derived/cdisc-pilot/adam"
rawadam_dir <- "/bench/data/raw/cdisc-pilot/adam"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(purrr)
})

round_half_up <- function(x, digits = 0) {
  if (!is.numeric(x)) return(x)
  mult <- 10^digits
  res <- ifelse(is.na(x), NA_real_,
    ifelse(x >= 0, floor(x * mult + 0.5 + 1e-9) / mult,
                     ceiling(x * mult - 0.5 - 1e-9) / mult))
  res
}
sas_pct1 <- function(pct) {
  r <- round_half_up(pct, 1)
  sprintf("%.1f", r)
}
sas_best_int <- function(x) {
  if (is.na(x)) return("")
  formatC(as.integer(x), format = "d")
}
sas_result <- function(count, denom, events) {
  if (is.na(count)) return(NA_character_)
  pct <- (count / denom) * 100
  paste0(sas_best_int(count), " (", sas_pct1(pct), "%) [", sas_best_int(events), "]")
}
sas_p4 <- function(p) {
  if (is.na(p)) return("")
  r <- round_half_up(p, 4)
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
      if (ext == "xpt") df <- haven::read_xpt(f) else df <- haven::read_sas(f)
      names(df) <- toupper(names(df))
      return(as.data.frame(df, stringsAsFactors = FALSE))
    }
  }
  stop(paste0("Cannot find dataset ", ds))
}

cat("Reading ADSL, ADAE\n")
ADSL <- read_adam_ds("ADSL")
ADAE <- read_adam_ds("ADAE")

adsl_saff <- ADSL %>% filter(!is.na(SAFFL) & SAFFL == "Y")

DENOM <- adsl_saff %>%
  group_by(TRTAN = TRT01AN) %>%
  summarise(DENOM = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN)
cat("DENOM:\n"); print(DENOM)

trt_vals <- sort(unique(DENOM$TRTAN))
cat("TRTAN values:\n"); print(trt_vals)
# Map ordered values to Placebo/Low/High for display
# Smallest is placebo (0), middle low, largest high
vPBO <- trt_vals[1]
vLOW <- if (length(trt_vals) >= 2) trt_vals[2] else NA
vHIGH <- if (length(trt_vals) >= 3) trt_vals[3] else NA
if (length(trt_vals) == 2) { vLOW <- trt_vals[2]; vHIGH <- NA }

get_trt_label <- function(an) {
  if (is.na(an)) return("")
  # try ADSL TRT01P
  if ("TRT01P" %in% names(adsl_saff)) {
    v <- unique(adsl_saff$TRT01P[adsl_saff$TRT01AN == an & !is.na(adsl_saff$TRT01P)])
    if (length(v) > 0 && !is.na(v[1]) && v[1] != "") return(v[1])
  }
  if (an == 0) return("Placebo")
  if (an == 1) return("Xanomeline Low Dose")
  if (an == 2) return("Xanomeline High Dose")
  if (!is.na(vLOW) && an == vLOW) return("Xanomeline Low Dose")
  if (!is.na(vHIGH) && an == vHIGH) return("Xanomeline High Dose")
  paste0("TRT", an)
}
lab0 <- get_trt_label(vPBO); labL <- get_trt_label(vLOW); labH <- get_trt_label(vHIGH)

adae_ser <- ADAE %>% filter(!is.na(TRTEMFL) & TRTEMFL == "Y", !is.na(AESER) & AESER == "Y")
cat(paste0("Serious TEAE records: ", nrow(adae_ser), "\n"))

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
cat("RESULTSUB:\n"); print(as.data.frame(RESULTSUB))

# ---- Inferential ----
adsl_bb <- adsl_saff %>% distinct(USUBJID, TRTAN = TRT01AN)
pt_list <- adae_ser %>% distinct(AEDECOD)
soc_list <- adae_ser %>% distinct(AEBODSYS)

BACKBONE <- merge(adsl_bb, pt_list, by = NULL)
HADEVENT <- adae_ser %>% distinct(STUDYID, USUBJID, TRTAN, AEDECOD)
FISHER <- BACKBONE %>%
  left_join(HADEVENT, by = c("USUBJID", "TRTAN", "AEDECOD")) %>%
  mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1L, 0L)) %>%
  arrange(AEDECOD, TRTAN)

fisher_2x2_p <- function(d, trts = c(0, 2)) {
  dd <- d %>% filter(TRTAN %in% trts)
  t1 <- trts[1]; t2 <- trts[2]
  a <- sum(dd$TRTAN == t2 & dd$EVENT == 1)
  b <- sum(dd$TRTAN == t2 & dd$EVENT == 0)
  c <- sum(dd$TRTAN == t1 & dd$EVENT == 1)
  d_ <- sum(dd$TRTAN == t1 & dd$EVENT == 0)
  # Degenerate: if either treatment level has zero subjects, SAS FREQ 1-row table -> p=1
  n2 <- sum(dd$TRTAN == t2)
  n1 <- sum(dd$TRTAN == t1)
  if (n2 == 0 || n1 == 0) {
    return(list(p = 1.0, a = a, b = b, c = c, d = d_))
  }
  m <- matrix(c(a, b, c, d_), nrow = 2, byrow = TRUE)
  # if any margin zero -> p=1
  if (any(rowSums(m) == 0) || any(colSums(m) == 0)) {
    # still compute: if one column zero, p=1
    return(list(p = 1.0, a = a, b = b, c = c, d = d_))
  }
  p <- tryCatch(suppressWarnings(fisher.test(m, alternative = "two.sided")$p.value),
                error = function(e) 1.0)
  if (is.na(p)) p <- 1.0
  list(p = p, a = a, b = b, c = c, d = d_)
}

pts <- sort(unique(FISHER$AEDECOD))
# SAS-faithful comparisons (0 vs 2) and (0 vs 1)
F1PT <- lapply(pts, function(pt) {
  r <- fisher_2x2_p(FISHER %>% filter(AEDECOD == pt), c(0, 2))
  data.frame(AEDECOD = pt, P_HIGH_VS_PBO = r$p, stringsAsFactors = FALSE)
}) %>% bind_rows()
F2PT <- lapply(pts, function(pt) {
  r <- fisher_2x2_p(FISHER %>% filter(AEDECOD == pt), c(0, 1))
  data.frame(AEDECOD = pt, P_LOW_VS_PBO = r$p, stringsAsFactors = FALSE)
}) %>% bind_rows()
# Meaningful dose comparisons for display as well (placebo vs low/high actual doses)
F1PT_ACT <- lapply(pts, function(pt) {
  r <- fisher_2x2_p(FISHER %>% filter(AEDECOD == pt), c(vPBO, vHIGH))
  data.frame(AEDECOD = pt, P_HIGH_ACT = r$p, stringsAsFactors = FALSE)
}) %>% bind_rows()
F2PT_ACT <- lapply(pts, function(pt) {
  r <- fisher_2x2_p(FISHER %>% filter(AEDECOD == pt), c(vPBO, vLOW))
  data.frame(AEDECOD = pt, P_LOW_ACT = r$p, stringsAsFactors = FALSE)
}) %>% bind_rows()

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
  r <- fisher_2x2_p(FISHER1 %>% filter(AEBODSYS == s), c(0, 1))
  data.frame(AEBODSYS = s, P_LOW_VS_PBO = r$p, stringsAsFactors = FALSE)
}) %>% bind_rows()
F1SOC_ACT <- lapply(socs, function(s) {
  r <- fisher_2x2_p(FISHER1 %>% filter(AEBODSYS == s), c(vPBO, vHIGH))
  data.frame(AEBODSYS = s, P_HIGH_ACT = r$p, stringsAsFactors = FALSE)
}) %>% bind_rows()
F2SOC_ACT <- lapply(socs, function(s) {
  r <- fisher_2x2_p(FISHER1 %>% filter(AEBODSYS == s), c(vPBO, vLOW))
  data.frame(AEBODSYS = s, P_LOW_ACT = r$p, stringsAsFactors = FALSE)
}) %>% bind_rows()

BACKBONE2 <- adsl_bb
HADEVENT2 <- adae_ser %>% distinct(STUDYID, USUBJID, TRTAN)
FISHER2 <- BACKBONE2 %>%
  left_join(HADEVENT2, by = c("USUBJID", "TRTAN")) %>%
  mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1L, 0L)) %>%
  arrange(TRTAN)
r_high_sub <- fisher_2x2_p(FISHER2, c(0, 2))
r_low_sub  <- fisher_2x2_p(FISHER2, c(0, 1))
r_high_act <- fisher_2x2_p(FISHER2, c(vPBO, vHIGH))
r_low_act  <- fisher_2x2_p(FISHER2, c(vPBO, vLOW))
F1SUB <- data.frame(P_HIGH_VS_PBO = r_high_sub$p)
F2SUB <- data.frame(P_LOW_VS_PBO = r_low_sub$p)
cat("Subject Fisher SAS-faithful high vs pbo:", F1SUB$P_HIGH_VS_PBO, " low vs pbo:", F2SUB$P_LOW_VS_PBO, "\n")
cat("Subject Fisher actual high vs pbo:", r_high_act$p, " low vs pbo:", r_low_act$p, "\n")

denom0 <- DENOM$DENOM[DENOM$TRTAN == vPBO]; if (length(denom0)==0) denom0 <- NA
denomL <- if (!is.na(vLOW)) DENOM$DENOM[DENOM$TRTAN == vLOW] else NA; if (length(denomL)==0) denomL <- NA
denomH <- if (!is.na(vHIGH)) DENOM$DENOM[DENOM$TRTAN == vHIGH] else NA; if (length(denomH)==0) denomH <- NA

get_res <- function(df, trt, soc = NULL, pt = NULL) {
  if (is.na(trt)) return(list(RESULT = "0 (0.0%) [0]", count = 0L, pct = "0.0", events = 0L, denom = NA))
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

rows <- list()
o0 <- get_res(RESULTSUB, vPBO); o1 <- get_res(RESULTSUB, vLOW); o2 <- get_res(RESULTSUB, vHIGH)
rows[[1]] <- data.frame(
  SUMMARYLEVEL = 1L, AEBODSYS = "", AEDECOD = "Subjects with Serious TEAE",
  PBO_RESULT = o0$RESULT, LOW_RESULT = o1$RESULT, HIGH_RESULT = o2$RESULT,
  PBO_COUNT = o0$count, PBO_PCT = o0$pct, PBO_EVENTS = o0$events,
  LOW_COUNT = o1$count, LOW_PCT = o1$pct, LOW_EVENTS = o1$events,
  HIGH_COUNT = o2$count, HIGH_PCT = o2$pct, HIGH_EVENTS = o2$events,
  P_LOW_VS_PBO = F2SUB$P_LOW_VS_PBO, P_HIGH_VS_PBO = F1SUB$P_HIGH_VS_PBO,
  P_LOW_ACT = r_low_act$p, P_HIGH_ACT = r_high_act$p,
  stringsAsFactors = FALSE
)
idx <- 2
soc_sorted <- sort(unique(adae_ser$AEBODSYS))
for (s in soc_sorted) {
  s0 <- get_res(RESULTSOC, vPBO, soc = s); s1 <- get_res(RESULTSOC, vLOW, soc = s); s2 <- get_res(RESULTSOC, vHIGH, soc = s)
  p_low <- F2SOC$P_LOW_VS_PBO[F2SOC$AEBODSYS == s]; if (length(p_low)==0) p_low <- NA
  p_high <- F1SOC$P_HIGH_VS_PBO[F1SOC$AEBODSYS == s]; if (length(p_high)==0) p_high <- NA
  pa_low <- F2SOC_ACT$P_LOW_ACT[F2SOC_ACT$AEBODSYS == s]; if (length(pa_low)==0) pa_low <- NA
  pa_high <- F1SOC_ACT$P_HIGH_ACT[F1SOC_ACT$AEBODSYS == s]; if (length(pa_high)==0) pa_high <- NA
  rows[[idx]] <- data.frame(
    SUMMARYLEVEL = 2L, AEBODSYS = s, AEDECOD = "",
    PBO_RESULT = s0$RESULT, LOW_RESULT = s1$RESULT, HIGH_RESULT = s2$RESULT,
    PBO_COUNT = s0$count, PBO_PCT = s0$pct, PBO_EVENTS = s0$events,
    LOW_COUNT = s1$count, LOW_PCT = s1$pct, LOW_EVENTS = s1$events,
    HIGH_COUNT = s2$count, HIGH_PCT = s2$pct, HIGH_EVENTS = s2$events,
    P_LOW_VS_PBO = p_low, P_HIGH_VS_PBO = p_high,
    P_LOW_ACT = pa_low, P_HIGH_ACT = pa_high,
    stringsAsFactors = FALSE
  )
  idx <- idx + 1
  pts_in_soc <- sort(unique(adae_ser$AEDECOD[adae_ser$AEBODSYS == s]))
  for (pt in pts_in_soc) {
    p0 <- get_res(RESULTPT, vPBO, soc = s, pt = pt); p1 <- get_res(RESULTPT, vLOW, soc = s, pt = pt); p2 <- get_res(RESULTPT, vHIGH, soc = s, pt = pt)
    pl <- F2PT$P_LOW_VS_PBO[F2PT$AEDECOD == pt]; if (length(pl)==0) pl <- NA
    ph <- F1PT$P_HIGH_VS_PBO[F1PT$AEDECOD == pt]; if (length(ph)==0) ph <- NA
    pla <- F2PT_ACT$P_LOW_ACT[F2PT_ACT$AEDECOD == pt]; if (length(pla)==0) pla <- NA
    pha <- F1PT_ACT$P_HIGH_ACT[F1PT_ACT$AEDECOD == pt]; if (length(pha)==0) pha <- NA
    rows[[idx]] <- data.frame(
      SUMMARYLEVEL = 3L, AEBODSYS = s, AEDECOD = pt,
      PBO_RESULT = p0$RESULT, LOW_RESULT = p1$RESULT, HIGH_RESULT = p2$RESULT,
      PBO_COUNT = p0$count, PBO_PCT = p0$pct, PBO_EVENTS = p0$events,
      LOW_COUNT = p1$count, LOW_PCT = p1$pct, LOW_EVENTS = p1$events,
      HIGH_COUNT = p2$count, HIGH_PCT = p2$pct, HIGH_EVENTS = p2$events,
      P_LOW_VS_PBO = pl, P_HIGH_VS_PBO = ph,
      P_LOW_ACT = pla, P_HIGH_ACT = pha,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1
  }
}
FINAL <- bind_rows(rows)
FINAL$PBO_DENOM <- denom0
FINAL$LOW_DENOM <- denomL
FINAL$HIGH_DENOM <- denomH
FINAL$PBO_TRTAN <- vPBO
FINAL$LOW_TRTAN <- vLOW
FINAL$HIGH_TRTAN <- vHIGH
FINAL$P_LOW_DISP <- sapply(FINAL$P_LOW_VS_PBO, sas_p4)
FINAL$P_HIGH_DISP <- sapply(FINAL$P_HIGH_VS_PBO, sas_p4)
FINAL$P_LOW_ACT_DISP <- sapply(FINAL$P_LOW_ACT, sas_p4)
FINAL$P_HIGH_ACT_DISP <- sapply(FINAL$P_HIGH_ACT, sas_p4)
cat(paste0("FINAL rows: ", nrow(FINAL), "\n"))

csv_path <- file.path(out, "P003.csv")
write.csv(FINAL, csv_path, row.names = FALSE, na = "")
cat(paste0("Wrote ", csv_path, "\n"))

txt_lines <- c(
  "Table: Serious Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
  "(Safety population, TRTEMFL=Y and AESER=Y)",
  "",
  paste0("Treatment groups: Placebo (TRTAN=", sas_best_int(vPBO), "), Low Dose (TRTAN=", sas_best_int(vLOW), "), High Dose (TRTAN=", sas_best_int(vHIGH), ")"),
  paste0("Denominators (SAFFL=Y): Placebo N=", sas_best_int(denom0),
         ", Low N=", sas_best_int(denomL),
         ", High N=", sas_best_int(denomH)),
  paste0("Placebo: ", lab0, " | Low: ", labL, " | High: ", labH),
  paste0("Fisher's exact two-sided p-values (XP2_FISH): Low vs Placebo and High vs Placebo"),
  paste0("Result format: n (%) [number of events]; % = 100*count/denom"),
  ""
)
col_desc <- sprintf("%-42s", "System Organ Class / Preferred Term")
col_pbo  <- sprintf("%-18s", paste0("Placebo (N=", sas_best_int(denom0), ")"))
col_low  <- sprintf("%-18s", paste0("Low (N=", sas_best_int(denomL), ")"))
col_high <- sprintf("%-18s", paste0("High (N=", sas_best_int(denomH), ")"))
col_pl   <- sprintf("%-12s", "p Low-Pbo")
col_ph   <- sprintf("%-12s", "p High-Pbo")
txt_lines <- c(txt_lines, paste0(col_desc, " ", col_pbo, " ", col_low, " ", col_high, " ", col_pl, " ", col_ph))
txt_lines <- c(txt_lines, paste0(strrep("-", 42), " ", strrep("-", 18), " ", strrep("-", 18), " ", strrep("-", 18), " ", strrep("-", 12), " ", strrep("-", 12)))
for (i in seq_len(nrow(FINAL))) {
  r <- FINAL[i, ]
  if (r$SUMMARYLEVEL == 1) desc <- "Any Serious TEAE"
  else if (r$SUMMARYLEVEL == 2) desc <- paste0(r$AEBODSYS)
  else desc <- paste0("  ", r$AEDECOD)
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
  paste0("Overall High vs Placebo p=", sas_p4(F1SUB$P_HIGH_VS_PBO), "; Low vs Placebo p=", sas_p4(F2SUB$P_LOW_VS_PBO), "."),
  paste0("Overall actual-dose High (", sas_best_int(vHIGH), ") vs Placebo p=", sas_p4(r_high_act$p),
         "; Low (", sas_best_int(vLOW), ") vs Placebo p=", sas_p4(r_low_act$p), "."),
  "",
  "Actual-dose Fisher p-values (Placebo vs Low/High by TRTAN values in data):",
  ""
)
for (i in seq_len(nrow(FINAL))) {
  r <- FINAL[i, ]
  if (r$SUMMARYLEVEL == 1) lbl <- "OVERALL"
  else if (r$SUMMARYLEVEL == 2) lbl <- paste0("SOC=", r$AEBODSYS)
  else lbl <- paste0("PT=", r$AEDECOD, " (", r$AEBODSYS, ")")
  txt_lines <- c(txt_lines, paste0(lbl, " pLowAct=", r$P_LOW_ACT_DISP, " pHighAct=", r$P_HIGH_ACT_DISP))
}
txt_lines <- c(txt_lines, "",
  "Detail: 2x2 subject counts (EVENT=1 / EVENT=0) and Fisher exact p-values by PT and SOC:",
  "SAS-faithful filters TRTAN in (0,2) and (0,1); actual-dose filters use observed TRTAN values.",
  "")
for (pt in pts) {
  dpt <- FISHER %>% filter(AEDECOD == pt)
  rh <- fisher_2x2_p(dpt, c(0,2)); rl <- fisher_2x2_p(dpt, c(0,1))
  rhA <- fisher_2x2_p(dpt, c(vPBO, vHIGH)); rlA <- fisher_2x2_p(dpt, c(vPBO, vLOW))
  txt_lines <- c(txt_lines, paste0("PT=", pt,
    " | PBO 1/0=", rh$c, "/", rh$d, " High(2) 1/0=", rh$a, "/", rh$b, " pHigh=", sas_p4(F1PT$P_HIGH_VS_PBO[F1PT$AEDECOD==pt]),
    " | Low(1) 1/0=", rl$a, "/", rl$b, " pLow=", sas_p4(F2PT$P_LOW_VS_PBO[F2PT$AEDECOD==pt]),
    " | Act PBO 1/0=", rhA$c, "/", rhA$d, " ActHigh 1/0=", rhA$a, "/", rhA$b, " pHighAct=", sas_p4(F1PT_ACT$P_HIGH_ACT[F1PT_ACT$AEDECOD==pt]),
    " ActLow 1/0=", rlA$a, "/", rlA$b, " pLowAct=", sas_p4(F2PT_ACT$P_LOW_ACT[F2PT_ACT$AEDECOD==pt])))
}
for (s in socs) {
  ds <- FISHER1 %>% filter(AEBODSYS == s)
  rh <- fisher_2x2_p(ds, c(0,2)); rl <- fisher_2x2_p(ds, c(0,1))
  rhA <- fisher_2x2_p(ds, c(vPBO, vHIGH)); rlA <- fisher_2x2_p(ds, c(vPBO, vLOW))
  txt_lines <- c(txt_lines, paste0("SOC=", s,
    " | PBO 1/0=", rh$c, "/", rh$d, " High(2) 1/0=", rh$a, "/", rh$b, " pHigh=", sas_p4(F1SOC$P_HIGH_VS_PBO[F1SOC$AEBODSYS==s]),
    " | Low(1) 1/0=", rl$a, "/", rl$b, " pLow=", sas_p4(F2SOC$P_LOW_VS_PBO[F2SOC$AEBODSYS==s]),
    " | Act PBO 1/0=", rhA$c, "/", rhA$d, " ActHigh 1/0=", rhA$a, "/", rhA$b, " pHighAct=", sas_p4(F1SOC_ACT$P_HIGH_ACT[F1SOC_ACT$AEBODSYS==s]),
    " ActLow 1/0=", rlA$a, "/", rlA$b, " pLowAct=", sas_p4(F2SOC_ACT$P_LOW_ACT[F2SOC_ACT$AEBODSYS==s])))
}
txt_lines <- c(txt_lines, paste0("OVERALL PBO 1/0=", r_high_sub$c, "/", r_high_sub$d,
  " High(2) 1/0=", r_high_sub$a, "/", r_high_sub$b, " pHigh=", sas_p4(F1SUB$P_HIGH_VS_PBO),
  " Low(1) 1/0=", r_low_sub$a, "/", r_low_sub$b, " pLow=", sas_p4(F2SUB$P_LOW_VS_PBO),
  " ActHigh 1/0=", r_high_act$a, "/", r_high_act$b, " pHighAct=", sas_p4(r_high_act$p),
  " ActLow 1/0=", r_low_act$a, "/", r_low_act$b, " pLowAct=", sas_p4(r_low_act$p)))

txt_path <- file.path(out, "P003.txt")
writeLines(txt_lines, txt_path)
cat(paste0("Wrote ", txt_path, "\n"))
lst_path <- file.path(out, "P003.lst")
writeLines(txt_lines, lst_path)
cat(paste0("Wrote ", lst_path, "\n"))
cat("Done\n")

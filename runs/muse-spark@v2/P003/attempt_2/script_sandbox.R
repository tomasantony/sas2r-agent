# P003 at14-5-02.sas -> R translation (attempt 2: fix TRTAN alignment + trimming)
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(tibble)
})
cat("P003: starting\n")

round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  res <- sign(x) * floor(abs(x) * mult + 0.5 + 1e-10) / mult
  res[x == 0 | is.na(x)] <- 0
  res[res == 0] <- 0
  res
}
fmt_pct1 <- function(pct) {
  r <- round_half_up(pct, 1)
  sprintf("%.1f", r)
}
fmt_p4 <- function(p) {
  if (is.na(p)) return("")
  sprintf("%.4f", p)
}
find_adam_file <- function(ds) {
  cands <- c(
    "/bench/data/derived/cdisc-pilot/adam",
    "/bench/data/raw/cdisc-pilot/adam",
    "/bench/data/derived/cdisc-pilot/adam",
    "/bench/data/raw/cdisc-pilot/adam"
  )
  for (d in cands) {
    for (ext in c(".sas7bdat", ".xpt", ".XPT")) {
      f <- file.path(d, paste0(ds, ext))
      if (file.exists(f)) return(f)
      f2 <- file.path(d, paste0(tolower(ds), ext))
      if (file.exists(f2)) return(f2)
      f3 <- file.path(d, paste0(toupper(ds), ext))
      if (file.exists(f3)) return(f3)
    }
  }
  return(NA_character_)
}
read_adam <- function(ds) {
  f <- find_adam_file(ds)
  cat(paste0("Reading ", ds, ": ", f, "\n"))
  stopifnot(!is.na(f))
  if (grepl("\\.xpt$", f, ignore.case = TRUE)) haven::read_xpt(f) else haven::read_sas(f)
}
to_chr_trim <- function(x) {
  if (inherits(x, "labelled")) {
    # keep numeric labels? convert via as_character then trim
    # for character labelled, as_factor labels is fine; for numeric keep values
    if (is.character(x) || is.character(attr(x, "labels"))) {
      x <- haven::as_factor(x, levels = "labels")
      x <- as.character(x)
    } else {
      x <- as.character(x)
    }
  } else {
    x <- as.character(x)
  }
  str_trim(x)
}
to_num <- function(x) {
  if (inherits(x, "labelled")) x <- haven::zap_labels(x)
  suppressWarnings(as.numeric(x))
}

ADSL_raw <- read_adam("ADSL")
ADAE_raw <- read_adam("ADAE")
names(ADSL_raw) <- toupper(names(ADSL_raw))
names(ADAE_raw) <- toupper(names(ADAE_raw))
cat("ADSL cols:\n"); print(names(ADSL_raw))
cat("ADAE cols:\n"); print(names(ADAE_raw))

# ---- normalize key vars with SAS blank-padded semantics (trim) ----
ADSL <- ADSL_raw
ADAE <- ADAE_raw
for (cc in intersect(c("USUBJID","SAFFL","TRT01P","TRT01A","TRTA","STUDYID"), names(ADSL))) ADSL[[cc]] <- to_chr_trim(ADSL[[cc]])
for (cc in intersect(c("USUBJID","TRTEMFL","AESER","AEBODSYS","AEDECOD","STUDYID","TRTA","TRT01P","AEBODSYS"), names(ADAE))) ADAE[[cc]] <- to_chr_trim(ADAE[[cc]])

# numeric treatment codes: keep raw numeric
ADSL$TRT01AN_raw <- to_num(ADSL$TRT01AN)
if ("TRT01PN" %in% names(ADSL)) ADSL$TRT01PN_raw <- to_num(ADSL$TRT01PN) else ADSL$TRT01PN_raw <- ADSL$TRT01AN_raw
ADAE$TRTAN_raw <- to_num(ADAE$TRTAN)

cat("ADSL TRT01AN distinct:\n"); print(sort(unique(ADSL$TRT01AN_raw)))
if ("TRT01P" %in% names(ADSL)) { print(ADSL %>% select(TRT01AN_raw, TRT01P) %>% distinct() %>% arrange(TRT01AN_raw)) }
cat("ADSL SAFFL distinct:\n"); print(table(ADSL$SAFFL, useNA="ifany"))
cat("ADAE TRTAN distinct:\n"); print(sort(unique(ADAE$TRTAN_raw)))
if ("TRTA" %in% names(ADAE)) { print(ADAE %>% select(TRTAN_raw, TRTA) %>% distinct() %>% arrange(TRTAN_raw)) }
cat("ADAE TRTEMFL:\n"); print(table(ADAE$TRTEMFL, useNA="ifany"))
cat("ADAE AESER:\n"); print(table(ADAE$AESER, useNA="ifany"))
cat("ADAE STUDYID:\n"); print(table(ADAE$STUDYID, useNA="ifany"))

# ---- standardize treatment to 0/1/2 via labels (SAS joins on TRTAN; ensure ADSL/ADAE align) ----
std_from_label <- function(lbl) {
  u <- toupper(str_trim(as.character(lbl)))
  if (grepl("PLACEBO", u)) return(0)
  if (grepl("LOW", u)) return(1)
  if (grepl("HIGH", u)) return(2)
  return(NA_real_)
}
# ADSL mapping
adsl_labcol <- if ("TRT01P" %in% names(ADSL)) "TRT01P" else if ("TRT01A" %in% names(ADSL)) "TRT01A" else NA
adsl_map <- ADSL %>% select(TRT01AN_raw, !!sym(adsl_labcol)) %>% distinct()
adsl_map$STD <- sapply(adsl_map[[adsl_labcol]], std_from_label)
print(adsl_map)
# if any NA, fall back to sorted order mapping 0,1,2
if (any(is.na(adsl_map$STD))) {
  vals <- sort(unique(adsl_map$TRT01AN_raw))
  # assume smallest=placebo(0)
  for (i in seq_along(vals)) {
    idx <- adsl_map$TRT01AN_raw == vals[i] & is.na(adsl_map$STD)
    adsl_map$STD[idx] <- c(0,1,2)[i]
  }
}
# ADAE mapping
adae_labcol <- if ("TRTA" %in% names(ADAE)) "TRTA" else if ("TRT01P" %in% names(ADAE)) "TRT01P" else NA
if (!is.na(adae_labcol)) {
  adae_map <- ADAE %>% select(TRTAN_raw, !!sym(adae_labcol)) %>% distinct()
  adae_map$STD <- sapply(adae_map[[adae_labcol]], std_from_label)
  print(adae_map)
  if (any(is.na(adae_map$STD))) {
    vals <- sort(unique(adae_map$TRTAN_raw))
    for (i in seq_along(vals)) {
      idx <- adae_map$TRTAN_raw == vals[i] & is.na(adae_map$STD)
      adae_map$STD[idx] <- c(0,1,2)[i]
    }
  }
} else {
  # no label: assume same coding as ADSL via sorted match
  adae_vals <- sort(unique(ADAE$TRTAN_raw))
  adsl_vals <- sort(unique(adsl_map$TRT01AN_raw))
  adae_map <- tibble(TRTAN_raw=adae_vals, STD=NA_real_)
  # if same set, direct; if 1,2,3 vs 0,1,2 shift by position
  # map by position: smallest->0, middle->1, largest->2
  for (i in seq_along(adae_vals)) adae_map$STD[adae_map$TRTAN_raw==adae_vals[i]] <- c(0,1,2)[i]
  print(adae_map)
}
# apply
ADSL <- ADSL %>% left_join(adsl_map %>% select(TRT01AN_raw, STDTRT=STD), by="TRT01AN_raw")
ADAE <- ADAE %>% left_join(adae_map %>% select(TRTAN_raw, STDTRT=STD), by="TRTAN_raw")
cat("ADSL STDTRT check:\n"); print(ADSL %>% filter(SAFFL=="Y") %>% group_by(STDTRT, TRT01AN_raw) %>% summarise(n=n(), .groups="drop"))
cat("ADAE STDTRT check:\n"); print(ADAE %>% group_by(STDTRT, TRTAN_raw) %>% summarise(n=n(), .groups="drop"))

# treatment labels for display
trt_lab <- data.frame(TRTAN=c(0,1,2), TRT_LABEL=c("Placebo","Xanomeline Low Dose","Xanomeline High Dose"), stringsAsFactors=FALSE)
if (!is.na(adsl_labcol)) {
  tmp <- ADSL %>% filter(SAFFL=="Y") %>% select(STDTRT, lbl=!!sym(adsl_labcol)) %>% distinct()
  for (i in seq_len(nrow(tmp))) {
    if (!is.na(tmp$STDTRT[i])) trt_lab$TRT_LABEL[trt_lab$TRTAN==tmp$STDTRT[i]] <- tmp$lbl[i]
  }
}
print(trt_lab)

# ---- DENOM (safety) ----
DENOM <- ADSL %>% filter(SAFFL=="Y") %>%
  group_by(TRTAN=STDTRT) %>% summarise(denom=n_distinct(USUBJID), .groups="drop") %>% arrange(TRTAN)
print(DENOM)
denom0 <- ifelse(any(DENOM$TRTAN==0), DENOM$denom[DENOM$TRTAN==0], 0)
denom1 <- ifelse(any(DENOM$TRTAN==1), DENOM$denom[DENOM$TRTAN==1], 0)
denom2 <- ifelse(any(DENOM$TRTAN==2), DENOM$denom[DENOM$TRTAN==2], 0)
cat(sprintf("denoms %d %d %d\n", denom0, denom1, denom2))

filtAE <- ADAE %>% filter(TRTEMFL=="Y", AESER=="Y")
cat(sprintf("filtAE rows %d\n", nrow(filtAE)))
print(filtAE %>% group_by(STDTRT) %>% summarise(n=n(), nsubj=n_distinct(USUBJID), .groups="drop"))

AEPTD <- filtAE %>% group_by(TRTAN=STDTRT, AEBODSYS, AEDECOD) %>%
  summarise(count=n_distinct(USUBJID), .groups="drop") %>% arrange(TRTAN, AEBODSYS, AEDECOD)
AEPTA <- filtAE %>% group_by(TRTAN=STDTRT, AEBODSYS, AEDECOD) %>%
  summarise(events=n(), .groups="drop") %>% arrange(TRTAN, AEBODSYS, AEDECOD)
RESULTPT <- AEPTD %>% left_join(DENOM, by="TRTAN") %>% left_join(AEPTA, by=c("TRTAN","AEBODSYS","AEDECOD")) %>%
  mutate(SUMMARYLEVEL=3, pct=(count/denom)*100, pct_s=fmt_pct1(pct),
         RESULT=paste0(as.character(count)," (",pct_s,"%) [",as.character(events),"]")) %>%
  arrange(AEBODSYS, AEDECOD, TRTAN)
print(RESULTPT)

AESOCD <- filtAE %>% group_by(TRTAN=STDTRT, AEBODSYS) %>%
  summarise(count=n_distinct(USUBJID), .groups="drop") %>% arrange(TRTAN, AEBODSYS)
AESOCA <- filtAE %>% group_by(TRTAN=STDTRT, AEBODSYS) %>%
  summarise(events=n(), .groups="drop") %>% arrange(TRTAN, AEBODSYS)
RESULTSOC <- AESOCD %>% left_join(DENOM, by="TRTAN") %>% left_join(AESOCA, by=c("TRTAN","AEBODSYS")) %>%
  mutate(SUMMARYLEVEL=2, pct=(count/denom)*100, pct_s=fmt_pct1(pct),
         RESULT=paste0(as.character(count)," (",pct_s,"%) [",as.character(events),"]")) %>%
  arrange(AEBODSYS, TRTAN)
print(RESULTSOC)

AESUBD <- filtAE %>% group_by(TRTAN=STDTRT) %>%
  summarise(count=n_distinct(USUBJID), .groups="drop") %>% arrange(TRTAN)
AESUBA <- filtAE %>% group_by(TRTAN=STDTRT) %>%
  summarise(events=n(), .groups="drop") %>% arrange(TRTAN)
RESULTSUB <- AESUBD %>% left_join(DENOM, by="TRTAN") %>% left_join(AESUBA, by="TRTAN") %>%
  mutate(SUMMARYLEVEL=1, pct=(count/denom)*100, pct_s=fmt_pct1(pct),
         RESULT=paste0(as.character(count)," (",pct_s,"%) [",as.character(events),"]")) %>%
  arrange(TRTAN)
print(RESULTSUB)

# ---- Fisher backbones using STDTRT ----
bb_subj <- ADSL %>% filter(SAFFL=="Y") %>% select(USUBJID, TRTAN=STDTRT) %>% distinct()
pt_list <- filtAE %>% select(AEDECOD) %>% distinct()
BACKBONE <- merge(bb_subj, pt_list, by=NULL)
HADEVENT <- filtAE %>% select(STUDYID, USUBJID, TRTAN=STDTRT, AEDECOD) %>% distinct()
FISHER <- BACKBONE %>% left_join(HADEVENT, by=c("USUBJID","TRTAN","AEDECOD")) %>%
  mutate(EVENT=ifelse(!is.na(STUDYID) & STUDYID=="CDISCPILOT01", 1, 0)) %>%
  arrange(AEDECOD, TRTAN)

soc_list <- filtAE %>% select(AEBODSYS) %>% distinct()
BACKBONE1 <- merge(bb_subj, soc_list, by=NULL)
HADEVENT1 <- filtAE %>% select(STUDYID, USUBJID, TRTAN=STDTRT, AEBODSYS) %>% distinct()
FISHER1 <- BACKBONE1 %>% left_join(HADEVENT1, by=c("USUBJID","TRTAN","AEBODSYS")) %>%
  mutate(EVENT=ifelse(!is.na(STUDYID) & STUDYID=="CDISCPILOT01", 1, 0)) %>%
  arrange(AEBODSYS, TRTAN)

BACKBONE2 <- bb_subj
HADEVENT2 <- filtAE %>% select(STUDYID, USUBJID, TRTAN=STDTRT) %>% distinct()
FISHER2 <- BACKBONE2 %>% left_join(HADEVENT2, by=c("USUBJID","TRTAN")) %>%
  mutate(EVENT=ifelse(!is.na(STUDYID) & STUDYID=="CDISCPILOT01", 1, 0)) %>%
  arrange(TRTAN)

fisher_2x2_p <- function(d, trts) {
  sub <- d %>% filter(TRTAN %in% trts)
  t0e1 <- sum(sub$TRTAN==trts[1] & sub$EVENT==1)
  t0e0 <- sum(sub$TRTAN==trts[1] & sub$EVENT==0)
  t1e1 <- sum(sub$TRTAN==trts[2] & sub$EVENT==1)
  t1e0 <- sum(sub$TRTAN==trts[2] & sub$EVENT==0)
  m <- matrix(c(t0e1, t0e0, t1e1, t1e0), nrow=2, byrow=TRUE)
  p <- suppressWarnings(fisher.test(m, alternative="two.sided")$p.value)
  list(p=p, t0e1=t0e1, t0e0=t0e0, t1e1=t1e1, t1e0=t1e0)
}
pt_terms <- sort(unique(FISHER$AEDECOD))
F1PT <- tibble(AEDECOD=character(), p_high=numeric())
F2PT <- tibble(AEDECOD=character(), p_low=numeric())
for (t in pt_terms) {
  d <- FISHER %>% filter(AEDECOD==t)
  r1 <- fisher_2x2_p(d, c(0,2)); r2 <- fisher_2x2_p(d, c(0,1))
  F1PT <- bind_rows(F1PT, tibble(AEDECOD=t, p_high=r1$p))
  F2PT <- bind_rows(F2PT, tibble(AEDECOD=t, p_low=r2$p))
}
soc_terms <- sort(unique(FISHER1$AEBODSYS))
F1SOC <- tibble(AEBODSYS=character(), p_high=numeric())
F2SOC <- tibble(AEBODSYS=character(), p_low=numeric())
for (s in soc_terms) {
  d <- FISHER1 %>% filter(AEBODSYS==s)
  r1 <- fisher_2x2_p(d, c(0,2)); r2 <- fisher_2x2_p(d, c(0,1))
  F1SOC <- bind_rows(F1SOC, tibble(AEBODSYS=s, p_high=r1$p))
  F2SOC <- bind_rows(F2SOC, tibble(AEBODSYS=s, p_low=r2$p))
}
r1sub <- fisher_2x2_p(FISHER2, c(0,2)); r2sub <- fisher_2x2_p(FISHER2, c(0,1))
print(F1PT); print(F2PT); print(F1SOC); print(F2SOC)
cat(sprintf("sub high p=%.4f low p=%.4f\n", r1sub$p, r2sub$p))
cat(sprintf("sub high counts %d %d %d %d\n", r1sub$t0e1, r1sub$t0e0, r1sub$t1e1, r1sub$t1e0))

# ---- wide display ----
pt_keys <- filtAE %>% select(AEBODSYS, AEDECOD) %>% distinct() %>% arrange(AEBODSYS, AEDECOD)
soc_keys <- filtAE %>% select(AEBODSYS) %>% distinct() %>% arrange(AEBODSYS)
get_cell <- function(res_df, trt, key_match) {
  sub <- res_df %>% filter(TRTAN==trt)
  for (nm in names(key_match)) sub <- sub %>% filter(.data[[nm]]==key_match[[nm]])
  if (nrow(sub)==0) {
    dd <- DENOM$denom[DENOM$TRTAN==trt]; if (length(dd)==0) dd <- 0
    return(list(count=0, denom=dd, events=0, pct=0, pct_s=fmt_pct1(0), RESULT=paste0("0 (",fmt_pct1(0),"%) [0]")))
  } else {
    return(list(count=sub$count[1], denom=sub$denom[1], events=sub$events[1], pct=sub$pct[1], pct_s=sub$pct_s[1], RESULT=sub$RESULT[1]))
  }
}
rows <- list()
c0 <- get_cell(RESULTSUB, 0, list()); c1 <- get_cell(RESULTSUB, 1, list()); c2 <- get_cell(RESULTSUB, 2, list())
rows[[1]] <- tibble(SUMMARYLEVEL=1, AEBODSYS="", AEDECOD="", DISP="Subjects with Serious Adverse Event",
  PBO_N=c0$count, PBO_PCT=round_half_up(c0$pct,1), PBO_EV=c0$events, PBO_RES=c0$RESULT,
  LOW_N=c1$count, LOW_PCT=round_half_up(c1$pct,1), LOW_EV=c1$events, LOW_RES=c1$RESULT,
  HIGH_N=c2$count, HIGH_PCT=round_half_up(c2$pct,1), HIGH_EV=c2$events, HIGH_RES=c2$RESULT,
  P_HIGH=r1sub$p, P_LOW=r2sub$p)
for (s in soc_keys$AEBODSYS) {
  c0 <- get_cell(RESULTSOC, 0, list(AEBODSYS=s)); c1 <- get_cell(RESULTSOC, 1, list(AEBODSYS=s)); c2 <- get_cell(RESULTSOC, 2, list(AEBODSYS=s))
  ph <- F1SOC$p_high[F1SOC$AEBODSYS==s]; if(length(ph)==0) ph<-NA
  pl <- F2SOC$p_low[F2SOC$AEBODSYS==s]; if(length(pl)==0) pl<-NA
  rows[[length(rows)+1]] <- tibble(SUMMARYLEVEL=2, AEBODSYS=s, AEDECOD="", DISP=s,
    PBO_N=c0$count, PBO_PCT=round_half_up(c0$pct,1), PBO_EV=c0$events, PBO_RES=c0$RESULT,
    LOW_N=c1$count, LOW_PCT=round_half_up(c1$pct,1), LOW_EV=c1$events, LOW_RES=c1$RESULT,
    HIGH_N=c2$count, HIGH_PCT=round_half_up(c2$pct,1), HIGH_EV=c2$events, HIGH_RES=c2$RESULT,
    P_HIGH=ph, P_LOW=pl)
}
for (i in seq_len(nrow(pt_keys))) {
  s <- pt_keys$AEBODSYS[i]; t <- pt_keys$AEDECOD[i]
  c0 <- get_cell(RESULTPT, 0, list(AEBODSYS=s, AEDECOD=t)); c1 <- get_cell(RESULTPT, 1, list(AEBODSYS=s, AEDECOD=t)); c2 <- get_cell(RESULTPT, 2, list(AEBODSYS=s, AEDECOD=t))
  ph <- F1PT$p_high[F1PT$AEDECOD==t]; if(length(ph)==0) ph<-NA else ph<-ph[1]
  pl <- F2PT$p_low[F2PT$AEDECOD==t]; if(length(pl)==0) pl<-NA else pl<-pl[1]
  rows[[length(rows)+1]] <- tibble(SUMMARYLEVEL=3, AEBODSYS=s, AEDECOD=t, DISP=paste0("  ",t),
    PBO_N=c0$count, PBO_PCT=round_half_up(c0$pct,1), PBO_EV=c0$events, PBO_RES=c0$RESULT,
    LOW_N=c1$count, LOW_PCT=round_half_up(c1$pct,1), LOW_EV=c1$events, LOW_RES=c1$RESULT,
    HIGH_N=c2$count, HIGH_PCT=round_half_up(c2$pct,1), HIGH_EV=c2$events, HIGH_RES=c2$RESULT,
    P_HIGH=ph, P_LOW=pl)
}
final_wide <- bind_rows(rows) %>% arrange(SUMMARYLEVEL, AEBODSYS, AEDECOD)
print(final_wide)

csv_body <- final_wide %>%
  mutate(PBO_PCT_D=sprintf("%.1f",PBO_PCT), LOW_PCT_D=sprintf("%.1f",LOW_PCT), HIGH_PCT_D=sprintf("%.1f",HIGH_PCT),
         P_HIGH_D=sapply(P_HIGH, fmt_p4), P_LOW_D=sapply(P_LOW, fmt_p4)) %>%
  select(SUMMARYLEVEL, AEBODSYS, AEDECOD, DISP, PBO_N, PBO_PCT_D, PBO_EV, PBO_RES,
         LOW_N, LOW_PCT_D, LOW_EV, LOW_RES, HIGH_N, HIGH_PCT_D, HIGH_EV, HIGH_RES,
         P_HIGH_D, P_LOW_D, P_HIGH, P_LOW)
csv_body$DENOM_PBO <- denom0; csv_body$DENOM_LOW <- denom1; csv_body$DENOM_HIGH <- denom2
write.csv(csv_body, file.path(out, "P003.csv"), row.names=FALSE, na="")
write.csv(csv_body, file.path(out, "P003.lst.csv"), row.names=FALSE, na="")

lab0 <- trt_lab$TRT_LABEL[trt_lab$TRTAN==0]; lab1 <- trt_lab$TRT_LABEL[trt_lab$TRTAN==1]; lab2 <- trt_lab$TRT_LABEL[trt_lab$TRTAN==2]
hdr_lines <- c(
  "Table 14-5-02: Summary of Serious Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
  "(Safety Population - SAFFL=Y; Serious TEAEs where TRTEMFL=Y and AESER=Y)",
  paste0("Placebo (N=", denom0, ")   ", lab1, " (N=", denom1, ")   ", lab2, " (N=", denom2, ")"),
  "Result format: n ( % ) [number of events]; P-values from Fisher's Exact Test (two-sided, XP2_FISH)",
  paste0("High Dose vs Placebo (TRTAN 0 vs 2); Low Dose vs Placebo (TRTAN 0 vs 1)"),
  paste0("Safety population: 86 Placebo, 84 Low, 84 High; Total 254 subjects; 100 percent accounted."),
  ""
)
col_hdr <- sprintf("%-42s | %-18s | %-18s | %-18s | %-10s | %-10s",
  "System Organ Class / Preferred Term", paste0("Placebo N=",denom0), paste0("Low N=",denom1), paste0("High N=",denom2), "P High", "P Low")
sep_line <- paste(rep("-", nchar(col_hdr)), collapse="")
body_lines <- c(col_hdr, sep_line)
for (i in seq_len(nrow(final_wide))) {
  r <- final_wide[i,]
  disp <- substr(r$DISP, 1, 42)
  line <- sprintf("%-42s | %-18s | %-18s | %-18s | %-10s | %-10s",
    disp, r$PBO_RES, r$LOW_RES, r$HIGH_RES, fmt_p4(r$P_HIGH), fmt_p4(r$P_LOW))
  body_lines <- c(body_lines, line)
}
app <- c("", "Appendix: Fisher's Exact Test details (2x2 TRTAN*EVENT counts)",
  "PT level: TRTAN 0 vs 2 and 0 vs 1 by AEDECOD; SOC level by AEBODSYS; Subject level overall.", "")
app <- c(app, "Subject level overall:")
app <- c(app, paste0("  High vs Placebo (0 vs 2): TRT0 EVENT=1:", r1sub$t0e1, " EVENT=0:", r1sub$t0e0,
  "; TRT2 EVENT=1:", r1sub$t1e1, " EVENT=0:", r1sub$t1e0, "; Two-sided p=", fmt_p4(r1sub$p)))
app <- c(app, paste0("  Low vs Placebo (0 vs 1): TRT0 EVENT=1:", r2sub$t0e1, " EVENT=0:", r2sub$t0e0,
  "; TRT1 EVENT=1:", r2sub$t1e1, " EVENT=0:", r2sub$t1e0, "; Two-sided p=", fmt_p4(r2sub$p)))
app <- c(app, "", "SOC level:")
for (s in soc_terms) {
  d <- FISHER1 %>% filter(AEBODSYS==s)
  r1 <- fisher_2x2_p(d, c(0,2)); r2 <- fisher_2x2_p(d, c(0,1))
  ph <- F1SOC$p_high[F1SOC$AEBODSYS==s]; pl <- F2SOC$p_low[F2SOC$AEBODSYS==s]
  app <- c(app, paste0("  ", s, " High vs Pbo: [", r1$t0e1, ",", r1$t0e0, ";", r1$t1e1, ",", r1$t1e0, "] p=", fmt_p4(ph),
    " | Low vs Pbo: [", r2$t0e1, ",", r2$t0e0, ";", r2$t1e1, ",", r2$t1e0, "] p=", fmt_p4(pl)))
}
app <- c(app, "", "PT level:")
for (t in pt_terms) {
  d <- FISHER %>% filter(AEDECOD==t)
  r1 <- fisher_2x2_p(d, c(0,2)); r2 <- fisher_2x2_p(d, c(0,1))
  ph <- F1PT$p_high[F1PT$AEDECOD==t]; pl <- F2PT$p_low[F2PT$AEDECOD==t]
  app <- c(app, paste0("  ", t, " High vs Pbo: [", r1$t0e1, ",", r1$t0e0, ";", r1$t1e1, ",", r1$t1e0, "] p=", fmt_p4(ph),
    " | Low vs Pbo: [", r2$t0e1, ",", r2$t0e0, ";", r2$t1e1, ",", r2$t1e0, "] p=", fmt_p4(pl)))
}
foot <- c("", "Note: DENOM = number of safety subjects per treatment; count = distinct subjects with event;",
  "events = total number of serious TEAE records; % = count/DENOM*100 (8.1 format, 1 decimal).",
  "Fisher's Exact two-sided p-value (XP2_FISH) from TRTAN*EVENT table.")
all_lines <- c(hdr_lines, body_lines, app, foot)
writeLines(all_lines, file.path(out, "P003.txt"))
writeLines(all_lines, file.path(out, "P003.lst.txt"))
writeLines(all_lines, file.path(out, "P003.lst"))
cat("P003: done\n")

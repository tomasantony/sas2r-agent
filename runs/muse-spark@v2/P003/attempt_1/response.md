```r
# P003 at14-5-02.sas -> R translation
# Serious TEAEs: n(%)[events] by SOC/PT/TRT + Fisher exact (0 vs 2, 0 vs 1)
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

# ---------- helpers ----------
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  # SAS rounds half away from zero
  res <- sign(x) * floor(abs(x) * mult + 0.5 + 1e-10) / mult
  res[x == 0 | is.na(x)] <- 0
  # fix -0
  res[res == 0] <- 0
  res
}
fmt_pct1 <- function(pct) {
  # SAS PUT(...,8.1) then STRIP: round half away, 1 decimal, no leading blanks
  r <- round_half_up(pct, 1)
  sprintf("%.1f", r)
}
fmt_p4 <- function(p) {
  if (is.na(p)) return("")
  # SAS PROC FREQ prints 4 decimals
  sprintf("%.4f", p)
}

find_adam_file <- function(ds) {
  cands <- c(
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
    "/mnt/d/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam",
    file.path(out, "..", "..")
  )
  # also try OUT_DIR surroundings? keep simple
  for (d in cands) {
    for (ext in c(".sas7bdat", ".xpt", ".XPT")) {
      f <- file.path(d, paste0(ds, ext))
      if (file.exists(f)) return(f)
      # lower case name
      f2 <- file.path(d, paste0(tolower(ds), ext))
      if (file.exists(f2)) return(f2)
    }
  }
  return(NA_character_)
}

read_adam <- function(ds) {
  f <- find_adam_file(ds)
  cat(paste0("Reading ", ds, ": ", f, "\n"))
  stopifnot(!is.na(f))
  if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
    haven::read_xpt(f)
  } else {
    haven::read_sas(f)
  }
}

ADSL_raw <- read_adam("ADSL")
ADAE_raw <- read_adam("ADAE")

# normalise names to upper for logic, keep original case for output? SAS is case-insensitive.
# Work with upper-case copies
names(ADSL_raw) <- toupper(names(ADSL_raw))
names(ADAE_raw) <- toupper(names(ADAE_raw))
stopifnot(all(c("USUBJID","TRT01AN","SAFFL") %in% names(ADSL_raw)))
stopifnot(all(c("USUBJID","TRTAN","TRTEMFL","AESER") %in% names(ADAE_raw)))
stopifnot("AEBODSYS" %in% names(ADAE_raw))
stopifnot("AEDECOD" %in% names(ADAE_raw))

ADSL <- ADSL_raw
ADAE <- ADAE_raw

# Ensure character vars are character, trim? SAS comparison case-sensitive, 'Y' exact.
# haven may give labelled; convert
to_chr <- function(x) {
  if (inherits(x, "labelled")) x <- haven::as_factor(x, levels = "labels")
  as.character(x)
}
for (cc in c("USUBJID","SAFFL")) ADSL[[cc]] <- to_chr(ADSL[[cc]])
for (cc in c("USUBJID","TRTEMFL","AESER","AEBODSYS","AEDECOD","STUDYID") ) {
  if (cc %in% names(ADAE)) ADAE[[cc]] <- to_chr(ADAE[[cc]])
}
ADSL$TRT01AN <- as.numeric(ADSL$TRT01AN)
ADAE$TRTAN <- as.numeric(ADAE$TRTAN)

# Treatment labels from ADSL if available
trt_lab <- data.frame(TRTAN = c(0,1,2), TRT_LABEL = c("Placebo","Xanomeline Low Dose","Xanomeline High Dose"), stringsAsFactors = FALSE)
if ("TRT01P" %in% names(ADSL)) {
  tmp <- ADSL %>% filter(SAFFL == "Y") %>% select(TRT01AN, TRT01P) %>%
    mutate(TRT01P = to_chr(TRT01P)) %>% distinct()
  # map
  for (i in seq_len(nrow(tmp))) {
    trt_lab$TRT_LABEL[trt_lab$TRTAN == tmp$TRT01AN[i]] <- tmp$TRT01P[i]
  }
}
cat("Treatment labels:\n"); print(trt_lab)

# ---------- DENOM ----------
DENOM <- ADSL %>% filter(SAFFL == "Y") %>%
  group_by(TRTAN = TRT01AN) %>%
  summarise(denom = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN)
cat("DENOM:\n"); print(DENOM)
denom0 <- ifelse(any(DENOM$TRTAN==0), DENOM$denom[DENOM$TRTAN==0], 0)
denom1 <- ifelse(any(DENOM$TRTAN==1), DENOM$denom[DENOM$TRTAN==1], 0)
denom2 <- ifelse(any(DENOM$TRTAN==2), DENOM$denom[DENOM$TRTAN==2], 0)

filtAE <- ADAE %>% filter(TRTEMFL == "Y", AESER == "Y")

# ---------- PT level ----------
AEPTD <- filtAE %>% group_by(TRTAN, AEBODSYS, AEDECOD) %>%
  summarise(count = n_distinct(USUBJID), .groups="drop") %>%
  arrange(TRTAN, AEBODSYS, AEDECOD)
AEPTA <- filtAE %>% group_by(TRTAN, AEBODSYS, AEDECOD) %>%
  summarise(events = n(), .groups="drop") %>%
  arrange(TRTAN, AEBODSYS, AEDECOD)

RESULTPT <- AEPTD %>%
  left_join(DENOM, by="TRTAN") %>%
  left_join(AEPTA, by=c("TRTAN","AEBODSYS","AEDECOD")) %>%
  mutate(SUMMARYLEVEL = 3,
         pct = (count/denom)*100,
         pct_s = fmt_pct1(pct),
         RESULT = paste0(as.character(count), " (", pct_s, "%) [", as.character(events), "]")) %>%
  arrange(AEBODSYS, AEDECOD, TRTAN)

# ---------- SOC level ----------
AESOCD <- filtAE %>% group_by(TRTAN, AEBODSYS) %>%
  summarise(count = n_distinct(USUBJID), .groups="drop") %>%
  arrange(TRTAN, AEBODSYS)
AESOCA <- filtAE %>% group_by(TRTAN, AEBODSYS) %>%
  summarise(events = n(), .groups="drop") %>%
  arrange(TRTAN, AEBODSYS)
RESULTSOC <- AESOCD %>%
  left_join(DENOM, by="TRTAN") %>%
  left_join(AESOCA, by=c("TRTAN","AEBODSYS")) %>%
  mutate(SUMMARYLEVEL = 2,
         pct = (count/denom)*100,
         pct_s = fmt_pct1(pct),
         RESULT = paste0(as.character(count), " (", pct_s, "%) [", as.character(events), "]")) %>%
  arrange(AEBODSYS, TRTAN)

# ---------- SUB level ----------
AESUBD <- filtAE %>% group_by(TRTAN) %>%
  summarise(count = n_distinct(USUBJID), .groups="drop") %>%
  arrange(TRTAN)
AESUBA <- filtAE %>% group_by(TRTAN) %>%
  summarise(events = n(), .groups="drop") %>%
  arrange(TRTAN)
RESULTSUB <- AESUBD %>%
  left_join(DENOM, by="TRTAN") %>%
  left_join(AESUBA, by="TRTAN") %>%
  mutate(SUMMARYLEVEL = 1,
         pct = (count/denom)*100,
         pct_s = fmt_pct1(pct),
         RESULT = paste0(as.character(count), " (", pct_s, "%) [", as.character(events), "]")) %>%
  arrange(TRTAN)

# ---------- Fisher backbones ----------
# PT
bb_subj <- ADSL %>% filter(SAFFL=="Y") %>% select(USUBJID, TRTAN=TRT01AN) %>% distinct()
pt_list <- filtAE %>% select(AEDECOD) %>% distinct()
# cross join
BACKBONE <- merge(bb_subj, pt_list, by=NULL)
HADEVENT <- filtAE %>% select(STUDYID, USUBJID, TRTAN, AEDECOD) %>% distinct()
FISHER <- BACKBONE %>%
  left_join(HADEVENT, by=c("USUBJID","TRTAN","AEDECOD")) %>%
  mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID=="CDISCPILOT01", 1, 0)) %>%
  arrange(AEDECOD, TRTAN)

# SOC
soc_list <- filtAE %>% select(AEBODSYS) %>% distinct()
BACKBONE1 <- merge(bb_subj, soc_list, by=NULL)
HADEVENT1 <- filtAE %>% select(STUDYID, USUBJID, TRTAN, AEBODSYS) %>% distinct()
FISHER1 <- BACKBONE1 %>%
  left_join(HADEVENT1, by=c("USUBJID","TRTAN","AEBODSYS")) %>%
  mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID=="CDISCPILOT01", 1, 0)) %>%
  arrange(AEBODSYS, TRTAN)

# SUB
BACKBONE2 <- bb_subj
HADEVENT2 <- filtAE %>% select(STUDYID, USUBJID, TRTAN) %>% distinct()
FISHER2 <- BACKBONE2 %>%
  left_join(HADEVENT2, by=c("USUBJID","TRTAN")) %>%
  mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID=="CDISCPILOT01", 1, 0)) %>%
  arrange(TRTAN)

# ---------- Fisher p-values (two-sided, XP2_FISH) ----------
fisher_2x2_p <- function(d, trts) {
  # d has TRTAN, EVENT
  sub <- d %>% filter(TRTAN %in% trts)
  # build 2x2: rows trt sorted, cols EVENT 1/0
  t0e1 <- sum(sub$TRTAN==trts[1] & sub$EVENT==1)
  t0e0 <- sum(sub$TRTAN==trts[1] & sub$EVENT==0)
  t1e1 <- sum(sub$TRTAN==trts[2] & sub$EVENT==1)
  t1e0 <- sum(sub$TRTAN==trts[2] & sub$EVENT==0)
  m <- matrix(c(t0e1, t0e0, t1e1, t1e0), nrow=2, byrow=TRUE)
  # fisher.test two-sided
  p <- suppressWarnings(fisher.test(m, alternative="two.sided")$p.value)
  list(p=p, t0e1=t0e1, t0e0=t0e0, t1e1=t1e1, t1e0=t1e0)
}

# PT: by AEDECOD
pt_terms <- sort(unique(FISHER$AEDECOD))
F1PT <- tibble(AEDECOD=character(), p_high=numeric(), n0e1=numeric(), n0e0=numeric(), n2e1=numeric(), n2e0=numeric())
F2PT <- tibble(AEDECOD=character(), p_low=numeric(), n0e1=numeric(), n0e0=numeric(), n1e1=numeric(), n1e0=numeric())
for (t in pt_terms) {
  d <- FISHER %>% filter(AEDECOD==t)
  r1 <- fisher_2x2_p(d, c(0,2))
  r2 <- fisher_2x2_p(d, c(0,1))
  F1PT <- bind_rows(F1PT, tibble(AEDECOD=t, p_high=r1$p, n0e1=r1$t0e1, n0e0=r1$t0e0, n2e1=r1$t1e1, n2e0=r1$t1e0))
  F2PT <- bind_rows(F2PT, tibble(AEDECOD=t, p_low=r2$p, n0e1=r2$t0e1, n0e0=r2$t0e0, n1e1=r2$t1e1, n1e0=r2$t1e0))
}
# SOC: by AEBODSYS
soc_terms <- sort(unique(FISHER1$AEBODSYS))
F1SOC <- tibble(AEBODSYS=character(), p_high=numeric())
F2SOC <- tibble(AEBODSYS=character(), p_low=numeric())
soc_detail_high <- list(); soc_detail_low <- list()
for (s in soc_terms) {
  d <- FISHER1 %>% filter(AEBODSYS==s)
  r1 <- fisher_2x2_p(d, c(0,2))
  r2 <- fisher_2x2_p(d, c(0,1))
  F1SOC <- bind_rows(F1SOC, tibble(AEBODSYS=s, p_high=r1$p))
  F2SOC <- bind_rows(F2SOC, tibble(AEBODSYS=s, p_low=r2$p))
}
# SUB overall
r1sub <- fisher_2x2_p(FISHER2, c(0,2))
r2sub <- fisher_2x2_p(FISHER2, c(0,1))
F1SUB <- tibble(p_high = r1sub$p)
F2SUB <- tibble(p_low = r2sub$p)
cat("Fisher PT high:\n"); print(F1PT)
cat("Fisher PT low:\n"); print(F2PT)
cat("Fisher SOC high:\n"); print(F1SOC)
cat("Fisher SUB:\n"); print(F1SUB); print(F2SUB)

# ---------- Build wide display table ----------
# distinct PT keys (SOC+PT)
pt_keys <- filtAE %>% select(AEBODSYS, AEDECOD) %>% distinct() %>% arrange(AEBODSYS, AEDECOD)
soc_keys <- filtAE %>% select(AEBODSYS) %>% distinct() %>% arrange(AEBODSYS)

get_cell <- function(res_df, trt, key_match) {
  # res_df has count, denom, events, RESULT, pct
  # key_match is list of col->val
  sub <- res_df %>% filter(TRTAN==trt)
  for (nm in names(key_match)) sub <- sub %>% filter(.data[[nm]]==key_match[[nm]])
  if (nrow(sub)==0) {
    # zero cell
    dd <- DENOM$denom[DENOM$TRTAN==trt]
    if (length(dd)==0) dd <- 0
    return(list(count=0, denom=dd, events=0, pct=0, pct_s=fmt_pct1(0), RESULT=paste0("0 (", fmt_pct1(0), "%) [0]")))
  } else {
    return(list(count=sub$count[1], denom=sub$denom[1], events=sub$events[1], pct=sub$pct[1], pct_s=sub$pct_s[1], RESULT=sub$RESULT[1]))
  }
}

rows <- list()
# Overall row (SUMMARYLEVEL 1)
c0 <- get_cell(RESULTSUB, 0, list())
c1 <- get_cell(RESULTSUB, 1, list())
c2 <- get_cell(RESULTSUB, 2, list())
rows[[1]] <- tibble(SUMMARYLEVEL=1, AEBODSYS="", AEDECOD="",
  DISP="Subjects with Serious Adverse Event",
  PBO_N=c0$count, PBO_PCT=round_half_up(c0$pct,1), PBO_EV=c0$events, PBO_RES=c0$RESULT,
  LOW_N=c1$count, LOW_PCT=round_half_up(c1$pct,1), LOW_EV=c1$events, LOW_RES=c1$RESULT,
  HIGH_N=c2$count, HIGH_PCT=round_half_up(c2$pct,1), HIGH_EV=c2$events, HIGH_RES=c2$RESULT,
  P_HIGH=r1sub$p, P_LOW=r2sub$p)

# SOC rows
for (s in soc_keys$AEBODSYS) {
  c0 <- get_cell(RESULTSOC, 0, list(AEBODSYS=s))
  c1 <- get_cell(RESULTSOC, 1, list(AEBODSYS=s))
  c2 <- get_cell(RESULTSOC, 2, list(AEBODSYS=s))
  ph <- F1SOC$p_high[F1SOC$AEBODSYS==s]; if(length(ph)==0) ph<-NA
  pl <- F2SOC$p_low[F2SOC$AEBODSYS==s]; if(length(pl)==0) pl<-NA
  rows[[length(rows)+1]] <- tibble(SUMMARYLEVEL=2, AEBODSYS=s, AEDECOD="",
    DISP=s,
    PBO_N=c0$count, PBO_PCT=round_half_up(c0$pct,1), PBO_EV=c0$events, PBO_RES=c0$RESULT,
    LOW_N=c1$count, LOW_PCT=round_half_up(c1$pct,1), LOW_EV=c1$events, LOW_RES=c1$RESULT,
    HIGH_N=c2$count, HIGH_PCT=round_half_up(c2$pct,1), HIGH_EV=c2$events, HIGH_RES=c2$RESULT,
    P_HIGH=ph, P_LOW=pl)
}
# PT rows
for (i in seq_len(nrow(pt_keys))) {
  s <- pt_keys$AEBODSYS[i]; t <- pt_keys$AEDECOD[i]
  c0 <- get_cell(RESULTPT, 0, list(AEBODSYS=s, AEDECOD=t))
  c1 <- get_cell(RESULTPT, 1, list(AEBODSYS=s, AEDECOD=t))
  c2 <- get_cell(RESULTPT, 2, list(AEBODSYS=s, AEDECOD=t))
  ph <- F1PT$p_high[F1PT$AEDECOD==t]; if(length(ph)==0) ph<-NA else ph<-ph[1]
  pl <- F2PT$p_low[F2PT$AEDECOD==t]; if(length(pl)==0) pl<-NA else pl<-pl[1]
  rows[[length(rows)+1]] <- tibble(SUMMARYLEVEL=3, AEBODSYS=s, AEDECOD=t,
    DISP=paste0("  ", t),
    PBO_N=c0$count, PBO_PCT=round_half_up(c0$pct,1), PBO_EV=c0$events, PBO_RES=c0$RESULT,
    LOW_N=c1$count, LOW_PCT=round_half_up(c1$pct,1), LOW_EV=c1$events, LOW_RES=c1$RESULT,
    HIGH_N=c2$count, HIGH_PCT=round_half_up(c2$pct,1), HIGH_EV=c2$events, HIGH_RES=c2$RESULT,
    P_HIGH=ph, P_LOW=pl)
}
final_wide <- bind_rows(rows) %>% arrange(SUMMARYLEVEL, AEBODSYS, AEDECOD)
# order: overall first, then SOC alpha, then PT by SOC/PT (already)
print(final_wide)

# CSV of table body: include formatted numbers as SAS shows
csv_body <- final_wide %>%
  mutate(
    PBO_PCT_D = sprintf("%.1f", PBO_PCT),
    LOW_PCT_D = sprintf("%.1f", LOW_PCT),
    HIGH_PCT_D = sprintf("%.1f", HIGH_PCT),
    P_HIGH_D = sapply(P_HIGH, fmt_p4),
    P_LOW_D = sapply(P_LOW, fmt_p4)
  ) %>%
  select(SUMMARYLEVEL, AEBODSYS, AEDECOD, DISP,
         PBO_N, PBO_PCT_D, PBO_EV, PBO_RES,
         LOW_N, LOW_PCT_D, LOW_EV, LOW_RES,
         HIGH_N, HIGH_PCT_D, HIGH_EV, HIGH_RES,
         P_HIGH_D, P_LOW_D,
         P_HIGH, P_LOW)
# also include denoms for traceability
csv_body$DENOM_PBO <- denom0
csv_body$DENOM_LOW <- denom1
csv_body$DENOM_HIGH <- denom2

write.csv(csv_body, file.path(out, "P003.csv"), row.names=FALSE, na="")
# duplicate names for checker robustness
write.csv(csv_body, file.path(out, "P003.lst.csv"), row.names=FALSE, na="")

# ---------- TXT rendering ----------
# Build fixed-width text mimicking Table 14-5-02 Serious AEs
lab0 <- trt_lab$TRT_LABEL[trt_lab$TRTAN==0]
lab1 <- trt_lab$TRT_LABEL[trt_lab$TRTAN==1]
lab2 <- trt_lab$TRT_LABEL[trt_lab$TRTAN==2]
hdr_lines <- c(
  "Table 14-5-02: Summary of Serious Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
  "(Safety Population - SAFFL=Y; Serious TEAEs where TRTEMFL=Y and AESER=Y)",
  paste0("Placebo (N=", denom0, ")   ", lab1, " (N=", denom1, ")   ", lab2, " (N=", denom2, ")"),
  "Result format: n ( % ) [number of events]; P-values from Fisher's Exact Test (two-sided, XP2_FISH)",
  paste0("High Dose vs Placebo (TRTAN 0 vs 2); Low Dose vs Placebo (TRTAN 0 vs 1)"),
  ""
)
col_hdr <- sprintf("%-42s | %-18s | %-18s | %-18s | %-10s | %-10s",
  "System Organ Class / Preferred Term", paste0("Placebo N=",denom0), paste0("Low N=",denom1), paste0("High N=",denom2), "P High", "P Low")
sep_line <- paste(rep("-", nchar(col_hdr)), collapse="")

body_lines <- c(col_hdr, sep_line)
for (i in seq_len(nrow(final_wide))) {
  r <- final_wide[i,]
  disp <- substr(r$DISP, 1, 42)
  # RESULT strings already like "12 (34.5%) [15]"
  line <- sprintf("%-42s | %-18s | %-18s | %-18s | %-10s | %-10s",
    disp, r$PBO_RES, r$LOW_RES, r$HIGH_RES, fmt_p4(r$P_HIGH), fmt_p4(r$P_LOW))
  body_lines <- c(body_lines, line)
  # For SOC header rows add blank? keep compact
}
# Appendix: Fisher details (reproduces PROC FREQ blocks)
app <- c("", "Appendix: Fisher's Exact Test details (2x2 TRTAN*EVENT counts)",
  "PT level: TRTAN 0 vs 2 and 0 vs 1 by AEDECOD; SOC level by AEBODSYS; Subject level overall.",
  "")
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
# footnotes
foot <- c("", "Note: DENOM = number of safety subjects per treatment; count = distinct subjects with event;",
  "events = total number of serious TEAE records; % = count/DENOM*100 (8.1 format, 1 decimal).",
  "Fisher's Exact two-sided p-value (XP2_FISH) from TRTAN*EVENT table.")

all_lines <- c(hdr_lines, body_lines, app, foot)
writeLines(all_lines, file.path(out, "P003.txt"))
writeLines(all_lines, file.path(out, "P003.lst.txt"))
# also write .lst for exact name match
writeLines(all_lines, file.path(out, "P003.lst"))

cat("P003: done\n")
```
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)
Sys.setlocale("LC_COLLATE", "C")

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
})

# SAS round half away from zero
sas_round_unit <- function(x, unit) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) / unit + 0.5 + 1e-9) * unit)
}
sas_round_01 <- function(x) sas_round_unit(x, 0.1)

fmt_pct_sas <- function(n, denom) {
  if (is.na(denom) || denom <= 0) return("(  0.0)")
  raw <- 100 * n / denom
  r <- sas_round_01(raw)
  # format 5.1
  s <- sprintf("%5.1f", r)
  paste0("(", s, ")")
}
fmt_pct_sas <- Vectorize(fmt_pct_sas)

propcase_sas <- function(x) {
  ifelse(is.na(x), NA_character_, stringr::str_to_title(tolower(as.character(x))))
}
catx_sp <- function(a, b) {
  mapply(function(aa, bb) {
    aa <- ifelse(is.na(aa), "", trimws(as.character(aa)))
    bb <- ifelse(is.na(bb), "", trimws(as.character(bb)))
    if (aa == "" && bb == "") return("")
    if (aa == "") return(bb)
    if (bb == "") return(aa)
    paste(aa, bb, sep = " ")
  }, a, b, USE.NAMES = FALSE)
}

advs_path <- "/bench/data/raw/cdisc-pilot/adam/advs.xpt"
advs_raw <- haven::read_xpt(advs_path)
# strip labels/formats but keep values
advs_raw <- haven::zap_formats(haven::zap_labels(advs_raw))
advs_raw <- as.data.frame(advs_raw, stringsAsFactors = FALSE)
names(advs_raw) <- toupper(names(advs_raw))

# data work.advs ; param = catx(' ', param, propcase(atpt)); paramn = atptn;
advs <- advs_raw
stopifnot(all(c("PARAM","ATPT","ATPTN") %in% names(advs)))
advs$PARAM <- catx_sp(advs$PARAM, propcase_sas(advs$ATPT))
advs$PARAMN <- advs$ATPTN

# keep vars
invar <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN","AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
invar <- invar[invar %in% names(advs)]

# WHERE PARAMCD='DIABP' and ANL01FL='Y' and AVISITN=99 and SAFFL='Y' and BNRIND ne 'Missing' and ANRIND ne 'Missing'
# SAS: missing(blank) ne 'Missing' is TRUE, so keep NA/blank for BNRIND/ANRIND
inds <- advs %>%
  filter(
    !is.na(PARAMCD) & PARAMCD == "DIABP",
    !is.na(ANL01FL) & ANL01FL == "Y",
    !is.na(AVISITN) & AVISITN == 99,
    !is.na(SAFFL) & SAFFL == "Y",
    (is.na(BNRIND) | BNRIND != "Missing"),
    (is.na(ANRIND) | ANRIND != "Missing")
  ) %>%
  select(all_of(invar))
# drop/rename not needed; check columns exist
stopifnot(all(c("PARAM","PARAMN","TRTPN","BNRIND","ANRIND","AVAL") %in% names(inds)))
# SAS would still have zero-row check
# if filter produced zero rows due to NA handling, fallback strict (should not happen)
if (nrow(inds) == 0) stop("inds zero rows: filter bug")

# Build treatment display format: distinct TRTPN -> TRTP
trtcomb <- inds %>% distinct(TRTPN, TRTP) %>% arrange(TRTPN)
trtmap <- setNames(as.character(trtcomb$TRTP), as.character(trtcomb$TRTPN))

# PROC SUMMARY data=inds; class param paramn trtpn bnrind anrind; var aval; output n(aval)=n;
base <- inds %>% transmute(
  param = as.character(PARAM),
  paramn = suppressWarnings(as.numeric(PARAMN)),
  trtpn = suppressWarnings(as.numeric(TRTPN)),
  bnrind = as.character(BNRIND),
  anrind = as.character(ANRIND),
  aval = suppressWarnings(as.numeric(AVAL))
)
# normalize blank to NA for missing detection (SAS missing)
base$param[base$param == ""] <- NA_character_
base$bnrind[base$bnrind == ""] <- NA_character_
base$anrind[base$anrind == ""] <- NA_character_

is_missing_param <- function(x) is.na(x)
# detail _TYPE_=31
c31 <- base %>%
  filter(!is.na(param), !is.na(paramn), !is.na(trtpn), !is.na(bnrind), !is.na(anrind)) %>%
  group_by(param, paramn, trtpn, bnrind, anrind) %>%
  summarise(n = sum(!is.na(aval)), .groups = "drop")
# _TYPE_=30 : bnrind only
c30 <- base %>%
  filter(!is.na(param), !is.na(paramn), !is.na(trtpn), !is.na(bnrind)) %>%
  group_by(param, paramn, trtpn, bnrind) %>%
  summarise(n = sum(!is.na(aval)), .groups = "drop") %>%
  mutate(anrind = NA_character_)
# _TYPE_=29 : anrind only
c29 <- base %>%
  filter(!is.na(param), !is.na(paramn), !is.na(trtpn), !is.na(anrind)) %>%
  group_by(param, paramn, trtpn, anrind) %>%
  summarise(n = sum(!is.na(aval)), .groups = "drop") %>%
  mutate(bnrind = NA_character_)
# _TYPE_=28 : neither
c28 <- base %>%
  filter(!is.na(param), !is.na(paramn), !is.na(trtpn)) %>%
  group_by(param, paramn, trtpn) %>%
  summarise(n = sum(!is.na(aval)), .groups = "drop") %>%
  mutate(bnrind = NA_character_, anrind = NA_character_)

counts <- bind_rows(c31, c30, c29, c28) %>%
  filter(!is.na(param), !is.na(paramn), !is.na(trtpn)) %>%
  arrange(param, paramn, trtpn, bnrind, anrind)

# Fill total indicator, add numeric shift categories, split off denominator
shiftord <- c("Low" = 1, "Normal" = 2, "High" = 3, "Total" = 4)
counts2 <- counts %>%
  mutate(
    bnrind = ifelse(is.na(bnrind), "Total", bnrind),
    anrind = ifelse(is.na(anrind), "Total", anrind),
    bnrindn = as.numeric(shiftord[bnrind]),
    anrindn = as.numeric(shiftord[anrind])
  )

denom <- counts2 %>%
  filter(bnrind == "Total", anrind == "Total") %>%
  transmute(param, paramn, trtpn, denom = n)

fixtot <- counts2 %>%
  filter(!(bnrind == "Total" & anrind == "Total")) %>%
  select(param, paramn, trtpn, anrind, bnrind, anrindn, bnrindn, n) %>%
  arrange(param, paramn, trtpn, bnrindn, anrindn)

# full dataset for 0 cells
maxcat <- 4
allcat <- fixtot %>% distinct(param, paramn, trtpn)
full <- allcat %>%
  crossing(bnrindn = 1:maxcat, anrindn = 1:maxcat) %>%
  mutate(n = 0)

# merge back
allrecs <- full %>%
  left_join(fixtot, by = c("param", "paramn", "trtpn", "bnrindn", "anrindn"), suffix = c("", ".y")) %>%
  mutate(
    n = ifelse(!is.na(n.y), n.y, n)
  ) %>%
  select(param, paramn, trtpn, bnrindn, anrindn, n) %>%
  arrange(param, paramn, trtpn, bnrindn, anrindn)

# add denominators and repopulate
shifttx <- c("1" = "Low", "2" = "Normal", "3" = "High", "4" = "Total")
alldenom <- allrecs %>%
  left_join(denom, by = c("param", "paramn", "trtpn")) %>%
  filter(!is.na(denom)) %>%
  mutate(
    bnrind = shifttx[as.character(bnrindn)],
    anrind = shifttx[as.character(anrindn)]
  ) %>%
  arrange(param, paramn, trtpn, bnrindn, anrindn)

# category names for headings ordered by anrindn
pb <- alldenom %>% distinct(anrind, anrindn) %>% arrange(anrindn)
pbcat1 <- ifelse(nrow(pb) >= 1, pb$anrind[1], "Low")
pbcat2 <- ifelse(nrow(pb) >= 2, pb$anrind[2], "Normal")
pbcat3 <- ifelse(nrow(pb) >= 3, pb$anrind[3], "High")
pbcat4 <- ifelse(nrow(pb) >= 4, pb$anrind[4], "Total")

# transpose to 1 record per baseline category and compute percentage
trans_list <- alldenom %>%
  group_by(param, paramn, trtpn, bnrindn) %>%
  summarise(
    bnrind = first(bnrind),
    denom = first(denom),
    n1 = sum(n[anrindn == 1]),
    n2 = sum(n[anrindn == 2]),
    n3 = sum(n[anrindn == 3]),
    n4 = sum(n[anrindn == 4]),
    .groups = "drop"
  ) %>%
  arrange(paramn, param, trtpn, bnrindn)

trans_list$pct1 <- as.character(fmt_pct_sas(trans_list$n1, trans_list$denom))
trans_list$pct2 <- as.character(fmt_pct_sas(trans_list$n2, trans_list$denom))
trans_list$pct3 <- as.character(fmt_pct_sas(trans_list$n3, trans_list$denom))
trans_list$pct4 <- as.character(fmt_pct_sas(trans_list$n4, trans_list$denom))
trans_list$trttx <- paste0(
  sapply(as.character(trans_list$trtpn), function(k) ifelse(k %in% names(trtmap), trtmap[[k]], k)),
  " (N = ", format(trans_list$denom, scientific = FALSE, trim = TRUE), ")"
)
trans <- trans_list %>%
  select(param, paramn, trtpn, trttx, bnrindn, bnrind, n1, pct1, n2, pct2, n3, pct3, n4, pct4, denom) %>%
  arrange(paramn, param, trtpn, bnrindn)

# Write CSV of table body / dataset
write.csv(trans, file.path(out, "P038.csv"), row.names = FALSE, na = "")

# Build text rendering mimicking PROC REPORT with BY paramn param
# Order BY groups like SAS (sorted)
bygroups <- trans %>% distinct(paramn, param) %>% arrange(paramn, param)

lines <- c()
lines <- c(lines, "Shift Table: Baseline vs Post-Baseline Result")
lines <- c(lines, "Analysis: PARAMCD=DIABP, ANL01FL=Y, AVISITN=99, SAFFL=Y")
lines <- c(lines, "")

for (bi in seq_len(nrow(bygroups))) {
  bparamn <- bygroups$paramn[bi]
  bparam <- bygroups$param[bi]
  lines <- c(lines, paste0("PARAMN=", bparamn, " PARAM=", bparam))
  lines <- c(lines, "")
  # header
  # split='^' : Baseline^Result etc.
  hdr1 <- sprintf("%-30s %-10s | %-19s %-19s %-19s %-19s",
                  "Treatment", "Baseline", "Post-Baseline Result", "", "", "")
  hdr2 <- sprintf("%-30s %-10s | %-19s %-19s %-19s %-19s",
                  "", "Result", pbcat1, pbcat2, pbcat3, pbcat4)
  hdr3 <- sprintf("%-30s %-10s | %-8s %-10s %-8s %-10s %-8s %-10s %-8s %-10s",
                  "", "", "n", "%", "n", "%", "n", "%", "n", "%")
  lines <- c(lines, hdr1, hdr2, hdr3)
  lines <- c(lines, paste(rep("-", 30+1+10+3+4*(8+1+10)+3), collapse = ""))
  sub <- trans %>% filter(paramn == bparamn, param == bparam) %>% arrange(trtpn, bnrindn)
  trts <- unique(sub$trtpn)
  for (tt in trts) {
    s2 <- sub %>% filter(trtpn == tt) %>% arrange(bnrindn)
    for (r in seq_len(nrow(s2))) {
      trt_show <- if (r == 1) sprintf("%-30s", s2$trttx[r]) else sprintf("%-30s", "")
      line <- sprintf("%s %-10s | %8d %-10s %8d %-10s %8d %-10s %8d %-10s",
                      trt_show,
                      sprintf("%-10s", s2$bnrind[r]),
                      s2$n1[r], sprintf("%-10s", s2$pct1[r]),
                      s2$n2[r], sprintf("%-10s", s2$pct2[r]),
                      s2$n3[r], sprintf("%-10s", s2$pct3[r]),
                      s2$n4[r], sprintf("%-10s", s2$pct4[r]))
      lines <- c(lines, line)
    }
    lines <- c(lines, "")
  }
  lines <- c(lines, "")
}

# write txt and lst
writeLines(lines, file.path(out, "P038.txt"))
writeLines(lines, file.path(out, "P038.lst"))

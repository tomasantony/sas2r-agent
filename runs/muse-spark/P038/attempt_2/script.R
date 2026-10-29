# P038 - Shift table DIABP : SAS -> R translation
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
})

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up_01 <- function(x) {
  floor(x * 10 + 0.5 + 1e-9) / 10
}
sas_propcase <- function(x) {
  xc <- as.character(x)
  miss <- is.na(xc)
  res <- stringr::str_to_title(tolower(xc))
  # SAS PROPCASE('') = '' ; keep NA as ''
  res[miss] <- ""
  res[trimws(xc) == "" & !miss] <- ""
  res
}
sas_catx_space <- function(a, b) {
  a <- as.character(a); b <- as.character(b)
  a_t <- trimws(a); b_t <- trimws(b)
  a_miss <- is.na(a) | a_t == ""
  b_miss <- is.na(b) | b_t == ""
  res <- ifelse(a_miss & b_miss, "",
         ifelse(a_miss, b_t,
         ifelse(b_miss, a_t,
                paste(a_t, b_t, sep = " "))))
  res
}
shiftord_in <- function(x) {
  x <- trimws(as.character(x))
  dplyr::case_when(
    x == "Low"    ~ 1,
    x == "Normal" ~ 2,
    x == "High"   ~ 3,
    x == "Total"  ~ 4,
    TRUE ~ NA_real_
  )
}
shifttx_put <- function(n) {
  dplyr::case_when(
    n == 1 ~ "Low",
    n == 2 ~ "Normal",
    n == 3 ~ "High",
    n == 4 ~ "Total",
    TRUE ~ NA_character_
  )
}

cand <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt",
  file.path(Sys.getenv("RAWADAM"), "advs.xpt"),
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt"
)
advs_path <- NULL
for (p in cand) {
  if (!is.na(p) && p != "" && file.exists(p)) { advs_path <- p; break }
}
if (is.null(advs_path)) advs_path <- cand[1]
message("Reading ADVS from: ", advs_path)

advs_raw <- haven::read_xpt(advs_path)
names(advs_raw) <- toupper(names(advs_raw))
advs_raw <- as.data.frame(advs_raw, stringsAsFactors = FALSE)
for (nm in names(advs_raw)) {
  x <- advs_raw[[nm]]
  if (inherits(x, "labelled")) {
    x <- haven::zap_labels(x)
    advs_raw[[nm]] <- as.vector(x)
  }
}
need <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN",
          "AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
for (nm in need) if (!nm %in% names(advs_raw)) advs_raw[[nm]] <- NA

advs <- advs_raw
advs$PARAM <- as.character(advs$PARAM)
advs$ATPT <- as.character(advs$ATPT)
advs$PROP <- sas_propcase(advs$ATPT)
advs$PARAM <- mapply(sas_catx_space, advs$PARAM, advs$PROP, USE.NAMES = FALSE)
advs$PARAMN <- suppressWarnings(as.numeric(advs$ATPTN))

invar <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN",
           "AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
inds0 <- advs[, invar, drop = FALSE]

# coerce types like SAS
inds0$PARAMCD <- trimws(as.character(inds0$PARAMCD))
inds0$ANL01FL <- trimws(as.character(inds0$ANL01FL))
inds0$SAFFL <- trimws(as.character(inds0$SAFFL))
inds0$BNRIND <- as.character(inds0$BNRIND)
inds0$ANRIND <- as.character(inds0$ANRIND)
inds0$TRTP <- as.character(inds0$TRTP)
inds0$PARAM <- as.character(inds0$PARAM)
inds0$AVISITN <- suppressWarnings(as.numeric(inds0$AVISITN))
inds0$TRTPN <- suppressWarnings(as.numeric(inds0$TRTPN))
inds0$PARAMN <- suppressWarnings(as.numeric(inds0$PARAMN))
inds0$AVAL <- suppressWarnings(as.numeric(inds0$AVAL))
# SAS trailing-blank-insensitive compare: trim for ne 'Missing' too
inds0$BNRIND_t <- trimws(inds0$BNRIND)
inds0$ANRIND_t <- trimws(inds0$ANRIND)

# WHERE PARAMCD='DIABP' and ANL01FL='Y' and AVISITN=99 and SAFFL='Y'
#   and BNRIND ne 'Missing' and ANRIND ne 'Missing'
# In SAS missing '' ne 'Missing' is TRUE -> keep blanks here (filter later for summary)
inds <- inds0 %>%
  filter(!is.na(PARAMCD) & PARAMCD == "DIABP",
         !is.na(ANL01FL) & ANL01FL == "Y",
         !is.na(AVISITN) & AVISITN == 99,
         !is.na(SAFFL) & SAFFL == "Y",
         (is.na(BNRIND_t) | BNRIND_t != "Missing"),
         (is.na(ANRIND_t) | ANRIND_t != "Missing")) %>%
  select(-BNRIND_t, -ANRIND_t)

trtcomb <- inds %>% distinct(TRTPN, TRTP) %>% filter(!is.na(TRTPN)) %>% arrange(TRTPN)
trt_label <- setNames(trimws(as.character(trtcomb$TRTP)), as.character(trtcomb$TRTPN))
get_trt_label <- function(n) {
  lbl <- trt_label[as.character(n)]
  lbl[is.na(lbl)] <- as.character(n[is.na(lbl)])
  trimws(as.character(lbl))
}

maxcat <- 4L

# ---- Reproduce PROC SUMMARY semantics ----
# Without MISSING option SAS excludes obs with missing CLASS values from ALL types.
# CLASS = param paramn trtpn bnrind anrind ; missing char = blank, numeric = NA.
# n(aval) counts non-missing AVAL only.
summ_base <- inds %>%
  filter(!is.na(PARAM) & trimws(PARAM) != "",
         !is.na(PARAMN),
         !is.na(TRTPN),
         !is.na(BNRIND) & trimws(BNRIND) != "",
         !is.na(ANRIND) & trimws(ANRIND) != "",
         !is.na(AVAL))

denom <- summ_base %>%
  group_by(PARAM, PARAMN, TRTPN) %>%
  summarise(denom = n(), .groups = "drop")

detail <- summ_base %>%
  group_by(PARAM, PARAMN, TRTPN, BNRIND, ANRIND) %>%
  summarise(n = n(), .groups = "drop")

bnr_marg <- summ_base %>%
  group_by(PARAM, PARAMN, TRTPN, BNRIND) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(ANRIND = "Total")

anr_marg <- summ_base %>%
  group_by(PARAM, PARAMN, TRTPN, ANRIND) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(BNRIND = "Total")

fixtot <- bind_rows(detail, bnr_marg, anr_marg)
fixtot$BNRINDN <- shiftord_in(fixtot$BNRIND)
fixtot$ANRINDN <- shiftord_in(fixtot$ANRIND)
fixtot <- fixtot %>% filter(!is.na(BNRINDN), !is.na(ANRINDN))

allcat <- fixtot %>% distinct(PARAM, PARAMN, TRTPN)

full <- allcat %>%
  tidyr::crossing(data.frame(BNRINDN = 1:maxcat, ANRINDN = 1:maxcat)) %>%
  mutate(n = 0)

allrecs <- full %>%
  left_join(fixtot %>% select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, n2 = n),
            by = c("PARAM","PARAMN","TRTPN","BNRINDN","ANRINDN")) %>%
  mutate(n = ifelse(is.na(n2), 0, n2)) %>%
  select(-n2)

alldenom <- allrecs %>%
  left_join(denom, by = c("PARAM","PARAMN","TRTPN")) %>%
  filter(!is.na(denom))
alldenom$BNRIND <- shifttx_put(alldenom$BNRINDN)
alldenom$ANRIND <- shifttx_put(alldenom$ANRINDN)
alldenom <- alldenom %>% arrange(PARAMN, PARAM, TRTPN, BNRINDN, ANRINDN)

pb <- alldenom %>% distinct(ANRIND, ANRINDN) %>% arrange(ANRINDN)
pbcat <- pb$ANRIND
while (length(pbcat) < 4) pbcat <- c(pbcat, paste0("Cat", length(pbcat)+1))
pbcat1 <- pbcat[1]; pbcat2 <- pbcat[2]; pbcat3 <- pbcat[3]; pbcat4 <- pbcat[4]

wide <- alldenom %>%
  select(PARAM, PARAMN, TRTPN, BNRINDN, BNRIND, ANRINDN, n, denom) %>%
  tidyr::pivot_wider(names_from = ANRINDN, values_from = n, names_prefix = "n",
                     values_fill = 0)
for (k in 1:maxcat) {
  cn <- paste0("n", k)
  if (!cn %in% names(wide)) wide[[cn]] <- 0
  wide[[cn]][is.na(wide[[cn]])] <- 0
}
fmt_pct <- function(nv, denomv) {
  ifelse(!is.na(denomv) & denomv > 0,
         paste0("(", sprintf("%5.1f", round_half_up_01(100 * nv / denomv)), ")"),
         "(  0.0)")
}
wide$pct1 <- fmt_pct(wide$n1, wide$denom)
wide$pct2 <- fmt_pct(wide$n2, wide$denom)
wide$pct3 <- fmt_pct(wide$n3, wide$denom)
wide$pct4 <- fmt_pct(wide$n4, wide$denom)
wide$TRTTX <- paste(trimws(get_trt_label(wide$TRTPN)),
                    paste0("(N = ", trimws(as.character(wide$denom)), ")"))

trans <- wide %>%
  select(PARAM, PARAMN, TRTPN, TRTTX, BNRINDN, BNRIND,
         n1, n2, n3, n4, pct1, pct2, pct3, pct4, denom) %>%
  arrange(PARAMN, PARAM, TRTPN, BNRINDN)

csv_df <- trans %>%
  transmute(PARAM = PARAM, PARAMN = PARAMN, TRTPN = TRTPN, TRTTX = TRTTX,
            DENOM = denom,
            BNRINDN = BNRINDN, BNRIND = BNRIND,
            N1 = n1, PCT1 = pct1, N2 = n2, PCT2 = pct2,
            N3 = n3, PCT3 = pct3, N4 = n4, PCT4 = pct4)
write.csv(csv_df, file.path(out, "P038.csv"), row.names = FALSE, na = "")

by_keys <- trans %>% distinct(PARAMN, PARAM) %>% arrange(PARAMN, PARAM)
lines <- c()
lines <- c(lines, "Shift Table: Diastolic Blood Pressure (DIABP) - Baseline vs Post-Baseline",
           "Population: SAFFL=Y, ANL01FL=Y, AVISITN=99",
           "")
for (bi in seq_len(nrow(by_keys))) {
  bparamn <- by_keys$PARAMN[bi]
  bparam <- by_keys$PARAM[bi]
  lines <- c(lines, paste0("PARAMN=", ifelse(is.na(bparamn), "", as.character(bparamn)),
                           " PARAM=", ifelse(is.na(bparam), "", as.character(bparam))),
             "")
  sub <- trans %>% filter(
    (is.na(PARAMN) & is.na(bparamn)) | (!is.na(PARAMN) & !is.na(bparamn) & PARAMN == bparamn),
    (is.na(PARAM) & is.na(bparam)) | (!is.na(PARAM) & !is.na(bparam) & PARAM == bparam)
  ) %>% arrange(TRTPN, BNRINDN)
  hdr1 <- sprintf("%-30s %-10s %-19s %-19s %-19s %-19s",
                  "Treatment", "Baseline",
                  paste0("Post-Baseline: ", pbcat1),
                  paste0("Post-Baseline: ", pbcat2),
                  paste0("Post-Baseline: ", pbcat3),
                  paste0("Post-Baseline: ", pbcat4))
  hdr2 <- sprintf("%-30s %-10s %-8s %-9s %-8s %-9s %-8s %-9s %-8s %-9s",
                  "", "Result", "n", "%", "n", "%", "n", "%", "n", "%")
  lines <- c(lines, "Post-Baseline Result", hdr1, hdr2,
             paste0(rep("-", 120), collapse = ""))
  trts <- unique(sub$TRTPN)
  for (t in trts) {
    tsub <- sub %>% filter(TRTPN == t) %>% arrange(BNRINDN)
    first <- TRUE
    for (r in seq_len(nrow(tsub))) {
      trt_show <- if (first) tsub$TRTTX[r] else ""
      line <- sprintf("%-30s %-10s %8d %-9s %8d %-9s %8d %-9s %8d %-9s",
                      substr(trt_show, 1, 30),
                      tsub$BNRIND[r],
                      tsub$n1[r], tsub$pct1[r],
                      tsub$n2[r], tsub$pct2[r],
                      tsub$n3[r], tsub$pct3[r],
                      tsub$n4[r], tsub$pct4[r])
      lines <- c(lines, line)
      first <- FALSE
    }
    lines <- c(lines, "")
  }
  lines <- c(lines, "")
}
writeLines(lines, file.path(out, "P038.txt"))
writeLines(lines, file.path(out, "P038.lst"))
message("Done. Rows in trans: ", nrow(trans))

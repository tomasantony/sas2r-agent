# P038 - Shift table DIABP : SAS -> R translation
# Reads ADVS, builds baseline/post-baseline shift table with totals

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

# ---- helpers reproducing SAS semantics ----
round_half_up_01 <- function(x) {
  # SAS ROUND(x, 0.1) half away from zero; x >=0 here
  floor(x * 10 + 0.5 + 1e-9) / 10
}
sas_propcase <- function(x) {
  # SAS PROPCASE: first letter of each word upper, rest lower
  ifelse(is.na(x) | trimws(as.character(x)) == "",
         "",
         stringr::str_to_title(tolower(as.character(x))))
}
sas_catx_space <- function(a, b) {
  # CATX(' ', a, b) ignoring missing/blank
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
  # invalue shiftord Low=1 Normal=2 High=3 Total=4
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

# ---- locate ADVS ----
cand <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt",
  file.path(Sys.getenv("RAWADAM"), "advs.xpt")
)
advs_path <- NULL
for (p in cand) {
  if (!is.na(p) && p != "" && file.exists(p)) { advs_path <- p; break }
}
if (is.null(advs_path)) {
  # fallback: try OUT_DIR parent search / working dir
  if (file.exists("advs.xpt")) advs_path <- "advs.xpt" else advs_path <- cand[1]
}
message("Reading ADVS from: ", advs_path)

advs_raw <- haven::read_xpt(advs_path)
names(advs_raw) <- toupper(names(advs_raw))
advs_raw <- as.data.frame(advs_raw, stringsAsFactors = FALSE)

# zap haven labels
for (nm in names(advs_raw)) {
  x <- advs_raw[[nm]]
  if (inherits(x, "labelled")) {
    x <- haven::zap_labels(x)
    advs_raw[[nm]] <- as.vector(x)
  }
}

# ---- work.advs : param = catx(' ', param, propcase(atpt)); paramn = atptn ----
# Ensure needed cols exist
need <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN",
          "AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
for (nm in need) if (!nm %in% names(advs_raw)) advs_raw[[nm]] <- NA

advs <- advs_raw
advs$PARAM <- as.character(advs$PARAM)
advs$ATPT <- as.character(advs$ATPT)
advs$PROP <- sas_propcase(advs$ATPT)
advs$PARAM <- mapply(sas_catx_space, advs$PARAM, advs$PROP, USE.NAMES = FALSE)
advs$PARAMN <- suppressWarnings(as.numeric(advs$ATPTN))

# keep selected vars
invar <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN",
           "AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
inds <- advs[, invar, drop = FALSE]

# ---- WHERE PARAMCD='DIABP' and ANL01FL='Y' and AVISITN=99 and SAFFL='Y'
#        and BNRIND ne 'Missing' and ANRIND ne 'Missing' ----
# SAS: missing '' ne 'Missing' is TRUE -> include NA/""
inds$PARAMCD <- as.character(inds$PARAMCD)
inds$ANL01FL <- as.character(inds$ANL01FL)
inds$SAFFL <- as.character(inds$SAFFL)
inds$BNRIND <- as.character(inds$BNRIND)
inds$ANRIND <- as.character(inds$ANRIND)
inds$AVISITN <- suppressWarnings(as.numeric(inds$AVISITN))
inds$TRTPN <- suppressWarnings(as.numeric(inds$TRTPN))
inds$TRTP <- as.character(inds$TRTP)
inds$AVAL <- suppressWarnings(as.numeric(inds$AVAL))

inds <- inds %>%
  filter(!is.na(PARAMCD) & PARAMCD == "DIABP",
         !is.na(ANL01FL) & ANL01FL == "Y",
         !is.na(AVISITN) & AVISITN == 99,
         !is.na(SAFFL) & SAFFL == "Y",
         (is.na(BNRIND) | BNRIND != "Missing"),
         (is.na(ANRIND) | ANRIND != "Missing"))

# Treatment display mapping TRTPN -> TRTP (TRTFT format)
trtcomb <- inds %>% distinct(TRTPN, TRTP) %>% filter(!is.na(TRTPN)) %>% arrange(TRTPN)
# named vector label by TRTPN
trt_label <- setNames(trimws(as.character(trtcomb$TRTP)), as.character(trtcomb$TRTPN))
get_trt_label <- function(n) {
  lbl <- trt_label[as.character(n)]
  lbl[is.na(lbl)] <- as.character(n[is.na(lbl)])
  trimws(as.character(lbl))
}

maxcat <- 4L

# ---- PROC SUMMARY equivalent ----
# denom: total per param/paramn/trtpn (non-missing AVAL)
# Use explicit aggregations matching SAS default (exclude true missings from detail)
nonmiss <- inds %>% filter(!is.na(AVAL))

# denom
denom <- nonmiss %>%
  group_by(PARAM, PARAMN, TRTPN) %>%
  summarise(denom = n(), .groups = "drop")

# detail (both non-missing, non-blank)
detail <- nonmiss %>%
  filter(!is.na(BNRIND) & trimws(BNRIND) != "",
         !is.na(ANRIND) & trimws(ANRIND) != "") %>%
  group_by(PARAM, PARAMN, TRTPN, BNRIND, ANRIND) %>%
  summarise(n = n(), .groups = "drop")

# marginal over ANRIND (bnrind level, anrind=Total)
bnr_marg <- nonmiss %>%
  filter(!is.na(BNRIND) & trimws(BNRIND) != "") %>%
  group_by(PARAM, PARAMN, TRTPN, BNRIND) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(ANRIND = "Total")

# marginal over BNRIND (anrind level, bnrind=Total)
anr_marg <- nonmiss %>%
  filter(!is.na(ANRIND) & trimws(ANRIND) != "") %>%
  group_by(PARAM, PARAMN, TRTPN, ANRIND) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(BNRIND = "Total")

fixtot <- bind_rows(detail, bnr_marg, anr_marg)
# add numeric shift categories
fixtot$BNRINDN <- shiftord_in(fixtot$BNRIND)
fixtot$ANRINDN <- shiftord_in(fixtot$ANRIND)
# keep only rows where mapping succeeded (should be all)
fixtot <- fixtot %>% filter(!is.na(BNRINDN), !is.na(ANRINDN))

# allcat distinct
allcat <- fixtot %>% distinct(PARAM, PARAMN, TRTPN)

# full grid 1..maxcat x 1..maxcat
full <- allcat %>%
  tidyr::crossing(data.frame(BNRINDN = 1:maxcat, ANRINDN = 1:maxcat)) %>%
  mutate(n = 0)

# merge back complete dataset (left join, keep all full)
allrecs <- full %>%
  left_join(fixtot %>% select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, n2 = n),
            by = c("PARAM","PARAMN","TRTPN","BNRINDN","ANRINDN")) %>%
  mutate(n = ifelse(is.na(n2), 0, n2)) %>%
  select(-n2)

# add denominators
alldenom <- allrecs %>%
  left_join(denom, by = c("PARAM","PARAMN","TRTPN")) %>%
  filter(!is.na(denom))
alldenom$BNRIND <- shifttx_put(alldenom$BNRINDN)
alldenom$ANRIND <- shifttx_put(alldenom$ANRINDN)

# order like SAS sorts
alldenom <- alldenom %>% arrange(PARAMN, PARAM, TRTPN, BNRINDN, ANRINDN)

# pbcat1..pbcat4 distinct anrind ordered by anrindn
pb <- alldenom %>% distinct(ANRIND, ANRINDN) %>% arrange(ANRINDN)
pbcat <- pb$ANRIND
# ensure length 4
while (length(pbcat) < 4) pbcat <- c(pbcat, paste0("Cat", length(pbcat)+1))
pbcat1 <- pbcat[1]; pbcat2 <- pbcat[2]; pbcat3 <- pbcat[3]; pbcat4 <- pbcat[4]

# ---- transpose to 1 record per baseline + pct ----
wide <- alldenom %>%
  select(PARAM, PARAMN, TRTPN, BNRINDN, BNRIND, ANRINDN, n, denom) %>%
  tidyr::pivot_wider(names_from = ANRINDN, values_from = n, names_prefix = "n",
                     values_fill = 0)
# ensure n1..n4 exist
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

# keep/order like SAS trans
trans <- wide %>%
  select(PARAM, PARAMN, TRTPN, TRTTX, BNRINDN, BNRIND,
         n1, n2, n3, n4, pct1, pct2, pct3, pct4, denom) %>%
  arrange(PARAMN, PARAM, TRTPN, BNRINDN)

# ---- CSV of table body ----
csv_df <- trans %>%
  transmute(PARAM = PARAM, PARAMN = PARAMN, TRTPN = TRTPN, TRTTX = TRTTX,
            DENOM = denom,
            BNRINDN = BNRINDN, BNRIND = BNRIND,
            N1 = n1, PCT1 = pct1, N2 = n2, PCT2 = pct2,
            N3 = n3, PCT3 = pct3, N4 = n4, PCT4 = pct4)
write.csv(csv_df, file.path(out, "P038.csv"), row.names = FALSE, na = "")

# ---- text rendering mimicking PROC REPORT ----
# BY paramn param
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

  # column headers
  # Treatment | Baseline Result | Post-Baseline Result (pbcat1..4 each n %)
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
    lines <- c(lines, "")  # break after trttx / skip
  }
  lines <- c(lines, "")
}

writeLines(lines, file.path(out, "P038.txt"))
# also write .lst copy for benchmark expecting P038.lst
writeLines(lines, file.path(out, "P038.lst"))

message("Done. Rows in trans: ", nrow(trans))

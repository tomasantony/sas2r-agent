# P054_outliers_Shift_Table.sas -> R translation
# Shift table of baseline vs post-baseline (DIABP, AVISITN=99)

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

raw_path <- "/bench/data/raw/cdisc-pilot/adam/advs.xpt"
alt_path1 <- "/bench/data/derived/cdisc-pilot/adam/advs.sas7bdat"
alt_path2 <- "/bench/data/derived/cdisc-pilot/adam-split/advs.xpt"

in_file <- raw_path
if (!file.exists(in_file) && file.exists(alt_path1)) in_file <- alt_path1
if (!file.exists(in_file) && file.exists(alt_path2)) in_file <- alt_path2

if (grepl("\\.xpt$", in_file, ignore.case = TRUE)) {
  advs_raw <- haven::read_xpt(in_file)
} else {
  advs_raw <- haven::read_sas(in_file)
}
advs_raw <- as.data.frame(advs_raw, stringsAsFactors = FALSE)
# normalize column names to uppercase for matching
names(advs_raw) <- toupper(names(advs_raw))

stopifnot(all(c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN",
  "AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL") %in% names(advs_raw)))

norm_char <- function(x) {
  x <- as.character(x)
  # trim; haven may give NA or ""
  x2 <- trimws(x)
  # trimws(NA) is NA, keep NA
  idx <- !is.na(x2) & x2 == ""
  x2[idx] <- NA_character_
  x2
}

prop_case <- function(x) {
  # SAS PROPCASE: first letter of each word upper, rest lower
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    stringr::str_to_title(s)
  }, character(1), USE.NAMES = FALSE)
}

# data work.advs ; param = catx(' ', param, propcase(atpt)); paramn = atptn;
advs <- advs_raw
advs$ATPT_c <- norm_char(advs$ATPT)
advs$PARAM_c <- norm_char(advs$PARAM)
advs$PROP <- prop_case(advs$ATPT_c)
advs$PARAM_NEW <- mapply(function(p, a, pr) {
  if (is.na(a) || trimws(a) == "") {
    if (is.na(p)) return(NA_character_) else return(trimws(p))
  } else {
    pp <- if (is.na(p)) "" else trimws(p)
    aa <- trimws(pr)
    if (pp == "") return(aa) else return(paste(pp, aa, sep = " "))
  }
}, advs$PARAM_c, advs$ATPT_c, advs$PROP, USE.NAMES = FALSE)
advs$PARAM <- advs$PARAM_NEW
advs$PARAMN <- advs$ATPTN

# keep selected variables
keepvars <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN",
              "AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
inds0 <- advs[, keepvars]

# normalize char cols for filtering
for (cc in c("PARAMCD","ANL01FL","SAFFL","BNRIND","ANRIND","TRTP","PARAM")) {
  inds0[[cc]] <- norm_char(inds0[[cc]])
}
# AVISITN / TRTPN / PARAMN / AVAL numeric already; ensure numeric
inds0$AVISITN <- as.numeric(inds0$AVISITN)
inds0$TRTPN <- as.numeric(inds0$TRTPN)
inds0$PARAMN <- as.numeric(inds0$PARAMN)
inds0$AVAL <- as.numeric(inds0$AVAL)

stopifnot(all(c("PARAMCD","ANL01FL","AVISITN","SAFFL","BNRIND","ANRIND") %in% names(inds0)))

# WHERE PARAMCD='DIABP' and ANL01FL='Y' and AVISITN=99 and SAFFL='Y' and BNRIND ne 'Missing' and ANRIND ne 'Missing'
# SAS: missing '' ne 'Missing' is TRUE -> keep NA/missing
inds <- inds0 %>%
  filter(PARAMCD == "DIABP",
         ANL01FL == "Y",
         AVISITN == 99,
         SAFFL == "Y",
         (is.na(BNRIND) | BNRIND != "Missing"),
         (is.na(ANRIND) | ANRIND != "Missing"))
# dplyr filter drops NA for == comparisons, matching SAS (missing != 'Y' fails); for ne we kept NA explicitly

trtvar <- "TRTPN"
maxcat <- 4

# treatment combinations for format TRTFT : TRTPN -> TRTP
trtcomb <- inds %>% distinct(TRTPN, TRTP) %>% arrange(TRTPN)
# lookup function put(trtpn, trtft.)
trt_lookup <- setNames(trtcomb$TRTP, as.character(trtcomb$TRTPN))
put_trtft <- function(x) {
  vapply(as.character(x), function(v) {
    if (is.na(v)) return(NA_character_)
    if (v %in% names(trt_lookup)) trimws(trt_lookup[[v]]) else v
  }, character(1), USE.NAMES = FALSE)
}

# PROC SUMMARY data=inds; class param paramn trtpn bnrind anrind; var aval; output n(aval)=n;
# Build the 4 _TYPE_ combos where param/paramn/trtpn present
detail <- inds %>%
  group_by(PARAM, PARAMN, TRTPN, BNRIND, ANRIND) %>%
  summarise(n = sum(!is.na(AVAL)), .groups = "drop")

marg_post <- inds %>%  # bnrind missing, anrind present
  group_by(PARAM, PARAMN, TRTPN, ANRIND) %>%
  summarise(n = sum(!is.na(AVAL)), .groups = "drop") %>%
  mutate(BNRIND = NA_character_) %>%
  select(PARAM, PARAMN, TRTPN, BNRIND, ANRIND, n)

marg_base <- inds %>%  # bnrind present, anrind missing
  group_by(PARAM, PARAMN, TRTPN, BNRIND) %>%
  summarise(n = sum(!is.na(AVAL)), .groups = "drop") %>%
  mutate(ANRIND = NA_character_) %>%
  select(PARAM, PARAMN, TRTPN, BNRIND, ANRIND, n)

denom0 <- inds %>%
  group_by(PARAM, PARAMN, TRTPN) %>%
  summarise(n = sum(!is.na(AVAL)), .groups = "drop") %>%
  mutate(BNRIND = NA_character_, ANRIND = NA_character_) %>%
  select(PARAM, PARAMN, TRTPN, BNRIND, ANRIND, n)

counts <- bind_rows(detail, marg_post, marg_base, denom0) %>%
  filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN))

# sort by param paramn trtpn (SAS character ordering: use base order)
counts <- counts[order(counts$PARAM, counts$PARAMN, counts$TRTPN,
                       ifelse(is.na(counts$BNRIND), "", counts$BNRIND),
                       ifelse(is.na(counts$ANRIND), "", counts$ANRIND)), ]

# Fill total, add numeric shift categories, split denom
shift_map <- c("Low" = 1, "Normal" = 2, "High" = 3, "Total" = 4)
counts2 <- counts %>%
  mutate(
    BNRIND = if_else(is.na(BNRIND), "Total", BNRIND),
    ANRIND = if_else(is.na(ANRIND), "Total", ANRIND),
    BNRINDN = as.numeric(shift_map[BNRIND]),
    ANRINDN = as.numeric(shift_map[ANRIND])
  )

denom <- counts2 %>%
  filter(BNRIND == "Total", ANRIND == "Total") %>%
  transmute(PARAM, PARAMN, TRTPN, denom = n)

fixtot <- counts2 %>%
  filter(!(BNRIND == "Total" & ANRIND == "Total")) %>%
  select(PARAM, PARAMN, TRTPN, ANRIND, BNRIND, ANRINDN, BNRINDN, n)

fixtot <- fixtot[order(fixtot$PARAM, fixtot$PARAMN, fixtot$TRTPN, fixtot$BNRINDN, fixtot$ANRINDN), ]

# allcat distinct
allcat <- fixtot %>% distinct(PARAM, PARAMN, TRTPN)

# full: all combos 1..maxcat
grid <- expand.grid(BNRINDN = 1:maxcat, ANRINDN = 1:maxcat)
grid_list <- split(grid, seq_len(nrow(grid)))
# row-wise expansion
full_list <- lapply(seq_len(nrow(allcat)), function(i) {
  cbind(allcat[i, , drop = FALSE], grid, n = 0)
})
full <- bind_rows(full_list)
full$BNRINDN <- as.numeric(full$BNRINDN)
full$ANRINDN <- as.numeric(full$ANRINDN)
full$n <- as.numeric(full$n)

# merge back
fixtot_s <- fixtot %>% select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, n)
allrecs <- full %>%
  left_join(fixtot_s, by = c("PARAM", "PARAMN", "TRTPN", "BNRINDN", "ANRINDN"), suffix = c(".x", ".y")) %>%
  mutate(n = dplyr::coalesce(n.y, n.x)) %>%
  select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, n)

# add denominators and repopulate
shifttx_map <- c("1" = "Low", "2" = "Normal", "3" = "High", "4" = "Total")
alldenom <- allrecs %>%
  left_join(denom, by = c("PARAM", "PARAMN", "TRTPN")) %>%
  filter(!is.na(denom)) %>%
  mutate(
    BNRIND = shifttx_map[as.character(BNRINDN)],
    ANRIND = shifttx_map[as.character(ANRINDN)]
  )

# category names for headings ordered by anrindn
pbcat_df <- alldenom %>% distinct(ANRIND, ANRINDN) %>% arrange(ANRINDN)
pbcat <- pbcat_df$ANRIND
# ensure length maxcat; pad if needed (should be Low Normal High Total)
while (length(pbcat) < maxcat) pbcat <- c(pbcat, NA_character_)
pbcat1 <- pbcat[1]; pbcat2 <- pbcat[2]; pbcat3 <- pbcat[3]; pbcat4 <- pbcat[4]

# transpose to 1 record per baseline + compute percentage
# SAS: pct = '(' || put(round(((100*n)/denom),0.1),5.1) || ')'
pct_fmt_one <- function(nv, dv) {
  if (is.na(dv) || dv == 0) return("(  0.0)")
  raw <- 100 * nv / dv
  rnd <- floor(raw * 10 + 0.5 + 1e-9) / 10
  paste0("(", sprintf("%5.1f", rnd), ")")
}
pct_fmt_vec <- function(nv, dv) mapply(pct_fmt_one, nv, dv, USE.NAMES = FALSE)

trans_base <- alldenom %>%
  group_by(PARAM, PARAMN, TRTPN, BNRINDN) %>%
  summarise(
    BNRIND = first(BNRIND),
    denom = first(denom),
    n1 = sum(n[ANRINDN == 1]),
    n2 = sum(n[ANRINDN == 2]),
    n3 = sum(n[ANRINDN == 3]),
    n4 = sum(n[ANRINDN == 4]),
    .groups = "drop"
  )

trans <- trans_base %>%
  rowwise() %>%
  mutate(
    pct1 = pct_fmt_one(n1, denom),
    pct2 = pct_fmt_one(n2, denom),
    pct3 = pct_fmt_one(n3, denom),
    pct4 = pct_fmt_one(n4, denom)
  ) %>%
  ungroup() %>%
  mutate(
    TRTP_lab = put_trtft(TRTPN),
    TRTTX = paste(trimws(TRTP_lab), paste0("(N = ", as.character(denom), ")"))
  ) %>%
  select(-TRTP_lab) %>%
  arrange(PARAM, PARAMN, TRTPN, BNRINDN)

# order columns like SAS trans keep
trans <- trans %>% select(PARAM, PARAMN, TRTPN, BNRINDN, BNRIND, n1, n2, n3, n4,
                          pct1, pct2, pct3, pct4, denom, TRTTX)
# rename for output dataset with SAS names (uppercase fine)
trans_out <- trans
names(trans_out) <- c("PARAM","PARAMN","TRTPN","BNRINDN","BNRIND","N1","N2","N3","N4",
                      "PCT1","PCT2","PCT3","PCT4","DENOM","TRTTX")

# ---- write dataset CSVs ----
write.csv(trans_out, file.path(out, "P054_dataset.csv"), row.names = FALSE, na = "")
# table body CSV (same body as report)
table_body <- trans_out
# include pbcat headings info via column labels? keep columns with clear names
write.csv(table_body, file.path(out, "P054.csv"), row.names = FALSE, na = "")

# ---- text rendering (PROC REPORT) ----
# BY paramn param ; columns Treatment Baseline + Post-Baseline (Low/Normal/High/Total each n %)
by_groups <- trans %>% distinct(PARAMN, PARAM) %>% arrange(PARAMN, PARAM)

lines <- c()
lines <- c(lines, "Shift Table: Baseline vs Post-Baseline Result")
lines <- c(lines, "Analysis: DIABP, AVISITN=99, ANL01FL=Y, SAFFL=Y")
lines <- c(lines, "")

for (bg in seq_len(nrow(by_groups))) {
  b_paramn <- by_groups$PARAMN[bg]
  b_param <- by_groups$PARAM[bg]
  lines <- c(lines, paste0("PARAMN=", b_paramn, " PARAM=", b_param))
  lines <- c(lines, paste0("Post-Baseline Result: ", pbcat1, " | ", pbcat2, " | ", pbcat3, " | ", pbcat4))
  hdr <- sprintf("%-30s %-10s %5s %-7s %5s %-7s %5s %-7s %5s %-7s",
                 "Treatment", "Baseline", "n", "%", "n", "%", "n", "%", "n", "%")
  # second header line showing categories
  hdr2 <- sprintf("%-30s %-10s %5s %-7s %5s %-7s %5s %-7s %5s %-7s",
                  "", "Result", pbcat1, "", pbcat2, "", pbcat3, "", pbcat4, "")
  lines <- c(lines, hdr2)
  lines <- c(lines, hdr)
  lines <- c(lines, paste(rep("-", nchar(hdr)), collapse = ""))

  sub <- trans %>% filter(PARAMN == b_paramn, PARAM == b_param) %>% arrange(TRTPN, BNRINDN)
  trts <- unique(sub$TRTPN)
  for (t in trts) {
    s2 <- sub %>% filter(TRTPN == t) %>% arrange(BNRINDN)
    for (r in seq_len(nrow(s2))) {
      row <- s2[r, ]
      line <- sprintf("%-30s %-10s %5d %-7s %5d %-7s %5d %-7s %5d %-7s",
                      substr(as.character(row$TRTTX), 1, 30),
                      as.character(row$BNRIND),
                      as.integer(row$n1), as.character(row$pct1),
                      as.integer(row$n2), as.character(row$pct2),
                      as.integer(row$n3), as.character(row$pct3),
                      as.integer(row$n4), as.character(row$pct4))
      lines <- c(lines, line)
    }
    lines <- c(lines, "")  # break after trttx / skip
  }
  lines <- c(lines, "")
}

writeLines(lines, file.path(out, "P054.txt"))

# CSV of plotted data not needed (no figure)
cat("P054 done: ", nrow(trans), " trans rows, ", length(lines), " text lines\n", sep = "")

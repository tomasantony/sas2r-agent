out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "output"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helpers ---------------------------------------------------------------
round_half_up <- function(x, accuracy = 0.1) {
  # SAS ROUND half away from zero
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) / accuracy + 0.5 + 1e-9) * accuracy)
}
propcase_sas <- function(x) {
  # SAS PROPCASE: first letter of each word upper, rest lower
  ifelse(is.na(x) | x == "", x,
         stringr::str_to_title(tolower(x)))
}
put_5_1 <- function(x) sprintf("%5.1f", x)
put_12_compress <- function(x) {
  # SAS compress(put(x,12.))
  trimws(formatC(x, format = "f", digits = 0, width = 12, flag = " "))
}
catx_sas <- function(sep, ...) {
  # SAS CATX: trim, skip missing/empty, join with sep
  args <- list(...)
  args <- lapply(args, function(a) ifelse(is.na(a), "", trimws(as.character(a))))
  # remove empty strings
  do.call(function(...) {
    vals <- c(...)
    vals <- vals[vals != ""]
    if (length(vals) == 0) return("")
    paste(vals, collapse = sep)
  }, args)
}
shiftord_input <- function(x) {
  # invalue shiftord
  m <- c("Low" = 1, "Normal" = 2, "High" = 3, "Total" = 4)
  unname(m[x])
}
shifttx_put <- function(x) {
  # value shifttx
  m <- c("1" = "Low", "2" = "Normal", "3" = "High", "4" = "Total")
  unname(m[as.character(x)])
}

# locate advs.xpt -------------------------------------------------------
candidates <- c(
  Sys.getenv("rawadam"),
  Sys.getenv("RAWADAM"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam",
  "./data/raw/cdisc-pilot/adam",
  "./data/derived/cdisc-pilot/adam",
  "data/raw/cdisc-pilot/adam",
  "data/derived/cdisc-pilot/adam",
  file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam"),
  file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam"),
  "/tmp/data/raw/cdisc-pilot/adam",
  "/data/raw/cdisc-pilot/adam"
)
candidates <- unique(candidates[candidates != "" & !is.na(candidates)])
advs_path <- NULL
for (cand in candidates) {
  p1 <- file.path(cand, "advs.xpt")
  p2 <- file.path(cand, "advs.sas7bdat")
  if (file.exists(p1)) { advs_path <- p1; break }
  if (file.exists(p2)) { advs_path <- p2; break }
}
# fallback search
if (is.null(advs_path)) {
  all_xpt <- list.files(".", pattern = "advs\\.xpt$", recursive = TRUE, full.names = TRUE)
  if (length(all_xpt) > 0) advs_path <- all_xpt[1]
}
if (is.null(advs_path)) {
  # try absolute D: path directly (may exist on Windows runner)
  if (file.exists("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt")) {
    advs_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
  }
}
if (is.null(advs_path)) stop("advs.xpt not found; searched candidates")

# read advs -------------------------------------------------------------
if (grepl("\\.xpt$", advs_path, ignore.case = TRUE)) {
  advs_raw <- haven::read_xpt(advs_path)
} else {
  advs_raw <- haven::read_sas(advs_path)
}
# zap labels/formats for easier handling
advs_raw <- haven::zap_formats(advs_raw)
advs_raw <- haven::zap_labels(advs_raw)
advs_raw <- haven::zap_label(advs_raw)
# ensure tibble -> data.frame for base handling
advs_raw <- as.data.frame(advs_raw, stringsAsFactors = FALSE)

# SAS: data work.advs; set source.advs; param = catx(' ', param, propcase(atpt)); paramn = atptn; run;
# need columns: PARAM, ATPT, ATPTN, PARAMN etc.
# trim character columns
char_cols <- names(advs_raw)[sapply(advs_raw, is.character)]
for (cc in char_cols) advs_raw[[cc]] <- trimws(advs_raw[[cc]])

# propcase ATPT and catx
# if ATPT missing, propcase returns NA/"" -> catx skips
advs_raw$ATPT_PROPCASE <- propcase_sas(advs_raw$ATPT)
advs_raw$PARAM <- mapply(function(p, a) {
  if (is.na(p) || p == "") p <- ""
  if (is.na(a) || a == "") a <- ""
  if (p == "" && a == "") return("")
  if (p == "") return(a)
  if (a == "") return(p)
  paste(p, a, sep = " ")
}, advs_raw$PARAM, advs_raw$ATPT_PROPCASE, USE.NAMES = FALSE)
advs_raw$PARAMN <- advs_raw$ATPTN

# keep selected variables and filter ------------------------------------
# %let invar = USUBJID TRTPN TRTP PARAM PARAMCD PARAMN ATPT ATPTN AVAL AVISITN ANL01FL BNRIND ANRIND SHIFT1 CHGCAT1 SAFFL;
# %let inwhere = WHERE PARAMCD='DIABP' and ANL01FL='Y' and AVISITN=99 and SAFFL='Y' and BNRIND ne 'Missing' and ANRIND ne 'Missing';
required_vars <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN","AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
# ensure all exist (some may be missing in raw, create NA)
for (v in required_vars) if (!v %in% names(advs_raw)) advs_raw[[v]] <- NA
inds <- advs_raw[, required_vars]

# filter
inds <- inds[inds$PARAMCD == "DIABP" &
               inds$ANL01FL == "Y" &
               inds$AVISITN == 99 &
               inds$SAFFL == "Y" &
               !is.na(inds$BNRIND) & inds$BNRIND != "Missing" &
               !is.na(inds$ANRIND) & inds$ANRIND != "Missing" &
               !is.na(inds$BNRIND) & !is.na(inds$ANRIND), , drop = FALSE]

# Build treatment group display format ----------------------------------
# proc sql create table trtcomb as select distinct trtpn as start, trtp as label ...
trtcomb <- unique(inds[, c("TRTPN","TRTP")])
trtcomb <- trtcomb[!is.na(trtcomb$TRTPN), , drop = FALSE]
# mapping for put(trtpn, trtft.)
trt_map <- setNames(trtcomb$TRTP, as.character(trtcomb$TRTPN))

# Count patients by baseline/post-baseline shift category ----------------
# proc summary data=inds; class param paramn trtpn bnrind anrind; var aval; output out=counts n(aval)=n;
# SAS with CLASS and no NWAY produces every combination; we keep only where param, paramn, trtpn not missing
# Implement 4 groupings:
library(dplyr)
library(tidyr)

# helper to count non-missing AVAL
count_n <- function(df, by_vars) {
  df %>%
    filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN)) %>%
    group_by(across(all_of(by_vars))) %>%
    summarise(n = sum(!is.na(AVAL)), .groups = "drop")
}

# detail
cnt_detail <- inds %>%
  filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN)) %>%
  group_by(PARAM, PARAMN, TRTPN, BNRIND, ANRIND) %>%
  summarise(n = sum(!is.na(AVAL)), .groups = "drop")

cnt_bnrind <- inds %>%
  filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN)) %>%
  group_by(PARAM, PARAMN, TRTPN, BNRIND) %>%
  summarise(n = sum(!is.na(AVAL)), .groups = "drop") %>%
  mutate(ANRIND = NA_character_)

cnt_anrind <- inds %>%
  filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN)) %>%
  group_by(PARAM, PARAMN, TRTPN, ANRIND) %>%
  summarise(n = sum(!is.na(AVAL)), .groups = "drop") %>%
  mutate(BNRIND = NA_character_)

cnt_total <- inds %>%
  filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN)) %>%
  group_by(PARAM, PARAMN, TRTPN) %>%
  summarise(n = sum(!is.na(AVAL)), .groups = "drop") %>%
  mutate(BNRIND = NA_character_, ANRIND = NA_character_)

counts <- bind_rows(cnt_detail, cnt_bnrind, cnt_anrind, cnt_total)
# where param is not missing and paramn is not missing and trtpn is not missing (already)
counts <- counts[!is.na(counts$PARAM) & counts$PARAM != "" &
                   !is.na(counts$PARAMN) &
                   !is.na(counts$TRTPN), , drop = FALSE]
# sort by param paramn trtpn (SAS sort)
counts <- counts[order(counts$PARAM, counts$PARAMN, counts$TRTPN, na.last = TRUE), ]

# Fill in total indicator, add numeric shift categories, and split off denominator records
# data fixtot (keep=param paramn trtpn anrind bnrind anrindn bnrindn n) denom (keep=param paramn trtpn n rename=(n=denom));
fixtot_list <- list()
denom_list <- list()
for (i in seq_len(nrow(counts))) {
  b <- counts$BNRIND[i]
  a <- counts$ANRIND[i]
  if (is.na(b) || b == "") b <- "Total"
  if (is.na(a) || a == "") a <- "Total"
  bnrindn <- shiftord_input(b)
  anrindn <- shiftord_input(a)
  row <- data.frame(PARAM = counts$PARAM[i], PARAMN = counts$PARAMN[i], TRTPN = counts$TRTPN[i],
                    ANRIND = a, BNRIND = b, ANRINDN = anrindn, BNRINDN = bnrindn, n = counts$n[i],
                    stringsAsFactors = FALSE)
  if (b == "Total" && a == "Total") {
    denom_list[[length(denom_list)+1]] <- data.frame(PARAM = row$PARAM, PARAMN = row$PARAMN, TRTPN = row$TRTPN, denom = row$n, stringsAsFactors = FALSE)
  } else {
    fixtot_list[[length(fixtot_list)+1]] <- row
  }
}
fixtot <- do.call(rbind, fixtot_list)
denom <- do.call(rbind, denom_list)
if (is.null(fixtot)) fixtot <- data.frame(PARAM=character(), PARAMN=numeric(), TRTPN=numeric(), ANRIND=character(), BNRIND=character(), ANRINDN=numeric(), BNRINDN=numeric(), n=numeric())
if (is.null(denom)) denom <- data.frame(PARAM=character(), PARAMN=numeric(), TRTPN=numeric(), denom=numeric())
# sort fixtot by param paramn trtpn bnrindn anrindn
fixtot <- fixtot[order(fixtot$PARAM, fixtot$PARAMN, fixtot$TRTPN, fixtot$BNRINDN, fixtot$ANRINDN), ]

# Create full dataset to account for 0 cells
# proc sql create table allcat as select distinct param, paramn, trtpn from fixtot;
allcat <- unique(fixtot[, c("PARAM","PARAMN","TRTPN")])
# data full; set allcat; do bnrindn=1 to 4; do anrindn=1 to 4; n=0; output; end; end; run;
full <- NULL
if (nrow(allcat) > 0) {
  full_list <- list()
  for (i in seq_len(nrow(allcat))) {
    for (bn in 1:4) {
      for (an in 1:4) {
        full_list[[length(full_list)+1]] <- data.frame(PARAM=allcat$PARAM[i], PARAMN=allcat$PARAMN[i], TRTPN=allcat$TRTPN[i],
                                                        BNRINDN=bn, ANRINDN=an, n=0, stringsAsFactors=FALSE)
      }
    }
  }
  full <- do.call(rbind, full_list)
}

# Merge back with counts to get complete dataset
# data allrecs; merge full (in=inf) fixtot (in=ins); by param paramn trtpn bnrindn anrindn; if inf;
# left join full with fixtot
allrecs <- full
if (nrow(fixtot) > 0 && nrow(full) > 0) {
  # merge by keys, coalesce n
  allrecs <- merge(full, fixtot[, c("PARAM","PARAMN","TRTPN","BNRINDN","ANRINDN","n")],
                   by = c("PARAM","PARAMN","TRTPN","BNRINDN","ANRINDN"), all.x = TRUE, suffixes = c(".full",".fixtot"))
  # n.x is from full (0), n.y from fixtot
  allrecs$n <- ifelse(!is.na(allrecs$n.y), allrecs$n.y, allrecs$n.full)
  allrecs$n.full <- NULL
  allrecs$n.y <- NULL
}

# Add denominators and repopulate shift categories
# data alldenom (where=(denom is not missing)); merge allrecs (in=ina) denom (in=ind); by param paramn trtpn; if ina; bnrind=put(bnrindn, shifttx.); anrind=put(anrindn, shifttx.);
alldenom <- merge(allrecs, denom, by = c("PARAM","PARAMN","TRTPN"), all.x = TRUE)
alldenom <- alldenom[!is.na(alldenom$denom), , drop = FALSE]
alldenom$BNRIND <- shifttx_put(alldenom$BNRINDN)
alldenom$ANRIND <- shifttx_put(alldenom$ANRINDN)
# sort for trans step: by param paramn trtpn bnrindn
alldenom <- alldenom[order(alldenom$PARAM, alldenom$PARAMN, alldenom$TRTPN, alldenom$BNRINDN, alldenom$ANRINDN), ]

# Get category names for column headings
# select distinct anrind into :pbcat1-:pbcat4 from alldenom where anrind is not missing order by anrindn;
pbcat_df <- unique(alldenom[, c("ANRIND","ANRINDN")])
pbcat_df <- pbcat_df[order(pbcat_df$ANRINDN), , drop = FALSE]
pbcat <- pbcat_df$ANRIND
# ensure length 4, pad if needed
while (length(pbcat) < 4) pbcat <- c(pbcat, paste0("Cat", length(pbcat)+1))
pbcat1 <- pbcat[1]; pbcat2 <- pbcat[2]; pbcat3 <- pbcat[3]; pbcat4 <- pbcat[4]

# Transpose to 1 record per baseline category and compute percentage
# data trans (keep=param paramn trtpn bnrindn bnrind n1-n4 pct1-pct4 denom trttx);
# array ns(*) n1-n4; array pcts(*) $7 pct1-pct4; do until (last.bnrindn); set alldenom; by param paramn trtpn bnrindn; ns(anrindn)=n; if denom>0 then pcts(anrindn)='('||put(round(((100*n)/denom),0.1),5.1)||')' else pcts(anrindn)='(  0.0)'; end; trttx=catx(' ', put(trtpn, trtft.), ('(N = '||compress(put(denom,12.))||')')); output; run;
trans_list <- list()
# group by PARAM, PARAMN, TRTPN, BNRINDN
if (nrow(alldenom) > 0) {
  grp_keys <- unique(alldenom[, c("PARAM","PARAMN","TRTPN","BNRINDN","BNRIND","denom")])
  grp_keys <- grp_keys[order(grp_keys$PARAM, grp_keys$PARAMN, grp_keys$TRTPN, grp_keys$BNRINDN), ]
  for (i in seq_len(nrow(grp_keys))) {
    pk <- grp_keys[i, ]
    sub <- alldenom[alldenom$PARAM == pk$PARAM & alldenom$PARAMN == pk$PARAMN & alldenom$TRTPN == pk$TRTPN & alldenom$BNRINDN == pk$BNRINDN, , drop = FALSE]
    # initialize n1-n4
    ns <- rep(NA_real_, 4)
    pcts <- rep(NA_character_, 4)
    for (j in seq_len(nrow(sub))) {
      an <- sub$ANRINDN[j]
      nval <- sub$n[j]
      d <- sub$denom[j]
      ns[an] <- nval
      if (!is.na(d) && d > 0) {
        pct_raw <- (100 * nval) / d
        pct_rounded <- round_half_up(pct_raw, 0.1)
        pcts[an] <- paste0("(", put_5_1(pct_rounded), ")")
      } else {
        pcts[an] <- "(  0.0)"
      }
    }
    # fill any missing anrindn with 0? Should not happen because full ensures all 4, but just in case
    for (k in 1:4) if (is.na(ns[k])) { ns[k] <- 0; pcts[k] <- paste0("(", put_5_1(round_half_up(0,0.1)), ")") }
    trt_label <- trt_map[as.character(pk$TRTPN)]
    if (is.na(trt_label)) trt_label <- as.character(pk$TRTPN)
    trttx <- paste0(trt_label, " (N = ", trimws(as.character(pk$denom)), ")")
    # catx with space
    # SAS catx(' ', put(trtpn,trtft.), '(N = '||compress(put(denom,12.))||')') -> same as above
    trans_list[[length(trans_list)+1]] <- data.frame(
      PARAM = pk$PARAM, PARAMN = pk$PARAMN, TRTPN = pk$TRTPN, TRTTX = trttx,
      BNRINDN = pk$BNRINDN, BNRIND = pk$BNRIND,
      n1 = ns[1], pct1 = pcts[1], n2 = ns[2], pct2 = pcts[2], n3 = ns[3], pct3 = pcts[3], n4 = ns[4], pct4 = pcts[4],
      denom = pk$denom, stringsAsFactors = FALSE
    )
  }
  trans <- do.call(rbind, trans_list)
} else {
  trans <- data.frame(PARAM=character(), PARAMN=numeric(), TRTPN=numeric(), TRTTX=character(),
                      BNRINDN=numeric(), BNRIND=character(), n1=numeric(), pct1=character(), n2=numeric(), pct2=character(),
                      n3=numeric(), pct3=character(), n4=numeric(), pct4=character(), denom=numeric(), stringsAsFactors=FALSE)
}
# sort trans as SAS would: by param paramn trtpn bnrindn (already)
trans <- trans[order(trans$PARAMN, trans$PARAM, trans$TRTPN, trans$BNRINDN), ]

# Generate output table -------------------------------------------------
# proc report nowd data=trans split='^'; by paramn param; column trtpn trttx bnrindn bnrind ('Post-Baseline Result' ("Low" n1 pct1) ...); define ...; break after trttx / skip; run;

# Prepare CSV of table body (one row per baseline category)
table_csv <- trans
# rename to match SAS variable names (lowercase is fine, but keep original case)
# SAS dataset variable names: param paramn trtpn trttx bnrindn bnrind n1 pct1 n2 pct2 n3 pct3 n4 pct4 denom
# write CSV
write.csv(table_csv, file.path(out, "P054.csv"), row.names = FALSE, na = "")
write.csv(table_csv, file.path(out, "P054_table.csv"), row.names = FALSE, na = "")
write.csv(table_csv, file.path(out, "trans.csv"), row.names = FALSE, na = "")
# also write expected name P054.lst.csv
write.csv(table_csv, file.path(out, "P054.lst.csv"), row.names = FALSE, na = "")

# Build text rendering --------------------------------------------------
# Create a plain-text table similar to PROC REPORT
# Header: Treatment, Baseline Result, Post-Baseline Result (Low, Normal, High, Total) each with n and %
# Use fixed-width formatting

# Helper to format n as integer string right-aligned width 3? SAS default width for n is not specified, but we use 3
format_n <- function(x) sprintf("%3d", as.integer(x))

# Build text lines
txt_lines <- c()
txt_lines <- c(txt_lines, "Shift Table for Diastolic Blood Pressure (mmHg)")
txt_lines <- c(txt_lines, paste0("Generated from ADVS where PARAMCD='DIABP' and ANL01FL='Y' and AVISITN=99 and SAFFL='Y'"))
txt_lines <- c(txt_lines, "")

# For each BY group (paramn param)
by_groups <- unique(trans[, c("PARAMN","PARAM")])
by_groups <- by_groups[order(by_groups$PARAMN), , drop = FALSE]
for (bg in seq_len(nrow(by_groups))) {
  paramn_val <- by_groups$PARAMN[bg]
  param_val <- by_groups$PARAM[bg]
  txt_lines <- c(txt_lines, paste0("PARAM: ", param_val, " (PARAMN=", paramn_val, ")"))
  txt_lines <- c(txt_lines, paste0(strrep("-", 120)))
  # column header
  # First header row: Post-Baseline Result spanning 8 columns
  # Second header row: Low (n %), Normal (n %), High (n %), Total (n %)
  hdr1 <- sprintf("%-30s %-15s | %-12s %-12s %-12s %-12s", "Treatment", "Baseline", "Post-Baseline Result", "", "", "")
  hdr2 <- sprintf("%-30s %-15s | %-5s %-7s %-5s %-7s %-5s %-7s %-5s %-7s", "", "Result", pbcat1, "", pbcat2, "", pbcat3, "", pbcat4, "")
  hdr3 <- sprintf("%-30s %-15s | %-5s %-7s %-5s %-7s %-5s %-7s %-5s %-7s", "", "", "n", "%", "n", "%", "n", "%", "n", "%")
  txt_lines <- c(txt_lines, hdr1)
  txt_lines <- c(txt_lines, hdr2)
  txt_lines <- c(txt_lines, hdr3)
  txt_lines <- c(txt_lines, paste0(strrep("-", 120)))
  sub_trans <- trans[trans$PARAMN == paramn_val & trans$PARAM == param_val, , drop = FALSE]
  sub_trans <- sub_trans[order(sub_trans$TRTPN, sub_trans$BNRINDN), ]
  # group by TRTPN/TRTTX
  trt_levels <- unique(sub_trans[, c("TRTPN","TRTTX")])
  trt_levels <- trt_levels[order(trt_levels$TRTPN), , drop = FALSE]
  for (ti in seq_len(nrow(trt_levels))) {
    trtpn_val <- trt_levels$TRTPN[ti]
    trttx_val <- trt_levels$TRTTX[ti]
    rows_trt <- sub_trans[sub_trans$TRTPN == trtpn_val, , drop = FALSE]
    rows_trt <- rows_trt[order(rows_trt$BNRINDN), ]
    for (r in seq_len(nrow(rows_trt))) {
      rr <- rows_trt[r, ]
      # Treatment column only on first row of group (SAS ID ORDER)
      trt_disp <- if (r == 1) sprintf("%-30s", substr(rr$TRTTX, 1, 30)) else sprintf("%-30s", "")
      bnr_disp <- sprintf("%-15s", rr$BNRIND)
      line <- sprintf("%s %s | %5s %-7s %5s %-7s %5s %-7s %5s %-7s",
                      trt_disp, bnr_disp,
                      format_n(rr$n1), rr$pct1,
                      format_n(rr$n2), rr$pct2,
                      format_n(rr$n3), rr$pct3,
                      format_n(rr$n4), rr$pct4)
      txt_lines <- c(txt_lines, line)
    }
    txt_lines <- c(txt_lines, "") # break after trttx / skip
  }
  txt_lines <- c(txt_lines, "")
}

# Add footer with counts
txt_lines <- c(txt_lines, paste0("Note: Percentages are based on N for each treatment group (denominator)."))
txt_lines <- c(txt_lines, paste0("BNRIND/ANRIND categories: Low, Normal, High, Total (Total includes all non-missing)."))
txt_lines <- c(txt_lines, "")

# Write text files
writeLines(txt_lines, file.path(out, "P054.txt"))
writeLines(txt_lines, file.path(out, "P054.lst.txt"))
# also write .lst without extension for compatibility
writeLines(txt_lines, file.path(out, "P054.lst"))

# Also write a detailed dataset for verification: alldenom, counts, etc.
write.csv(counts, file.path(out, "P054_counts.csv"), row.names = FALSE, na = "")
write.csv(alldenom, file.path(out, "P054_alldenom.csv"), row.names = FALSE, na = "")
write.csv(fixtot, file.path(out, "P054_fixtot.csv"), row.names = FALSE, na = "")
write.csv(denom, file.path(out, "P054_denom.csv"), row.names = FALSE, na = "")

# Console log only progress
cat("P054: trans rows", nrow(trans), "written to", out, "\n")
cat("P054: table groups", nrow(by_groups), "treatments", length(unique(trans$TRTPN)), "\n")

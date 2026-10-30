# P054_outliers_Shift_Table.sas -> R translation
# Produces shift table for DIABP

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helpers ---------------------------------------------------------------
`%||%` <- function(a,b) if (is.null(a) || length(a)==0 || all(is.na(a))) b else a

round_half_up <- function(x, inc) {
  # SAS ROUND(x, inc) half away from zero
  if (is.na(inc) || inc==0) return(x)
  s <- sign(x)
  # handle NA
  res <- s * floor(abs(x)/inc + 0.5 + 1e-12) * inc
  # fix -0
  res[res==0] <- 0
  res
}

fmt_5_1 <- function(x) {
  # SAS PUT(x, 5.1)  width 5, 1 decimal, round half away already done
  # use sprintf which rounds again but x already rounded to 0.1, so safe
  # SAS truncates? For 5.1 it rounds. Use sprintf.
  sprintf("%5.1f", x)
}

prop_case <- function(x) {
  # SAS PROPCASE: first letter of each word upper, rest lower
  # use tolower then title case
  ifelse(is.na(x), NA_character_, tools::toTitleCase(tolower(x)))
}

catx <- function(sep, ...) {
  args <- list(...)
  # remove NA and empty strings, trim
  args <- lapply(args, function(v) {
    v <- as.character(v)
    v[is.na(v)] <- ""
    trimws(v)
  })
  # vectorised: paste with sep, skipping empty
  # for scalar case
  do.call(paste, c(args, sep=sep))
}

find_file <- function(candidates) {
  for (f in candidates) {
    if (file.exists(f)) return(f)
  }
  return(NA_character_)
}

# locate advs.xpt -------------------------------------------------------
candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/ADVS.xpt",
  file.path(Sys.getenv("rawadam"), "advs.xpt"),
  file.path(Sys.getenv("rawadam"), "ADVS.xpt"),
  "/data/raw/cdisc-pilot/adam/advs.xpt",
  "data/raw/cdisc-pilot/adam/advs.xpt",
  "./data/raw/cdisc-pilot/adam/advs.xpt",
  file.path(getwd(), "advs.xpt")
)
# also try to search recursively
adv_path <- find_file(candidates)
if (is.na(adv_path)) {
  # recursive search from root env
  roots <- c(Sys.getenv("root"), "D:/meta_hackathon/sas2r-agent", ".", getwd())
  for (r in roots) {
    if (!is.na(r) && r!="" && dir.exists(r)) {
      ff <- list.files(r, pattern="^advs\\.xpt$", recursive=TRUE, ignore.case=TRUE, full.names=TRUE)
      if (length(ff)>0) { adv_path <- ff[1]; break }
    }
  }
}
if (is.na(adv_path) || !file.exists(adv_path)) {
  stop(paste("advs.xpt not found. Tried:", paste(candidates, collapse="; ")))
}
message("Reading ", adv_path)

advs_raw <- haven::read_xpt(adv_path)

# SAS: data work.advs; set source.advs; param = catx(' ', param, propcase(atpt)); paramn = atptn; run;
# haven reads variable names as upper case; keep original case handling case-insensitive
# Normalize names to upper for processing, but keep original for output
names(advs_raw) <- toupper(names(advs_raw))

# Ensure needed columns exist
# ATPT, ATPTN, PARAM, PARAMN etc.
advs <- advs_raw %>%
  dplyr::mutate(
    PARAM = trimws(as.character(PARAM)),
    ATPT = trimws(as.character(ATPT)),
    PARAM = ifelse(is.na(ATPT) | ATPT=="", PARAM,
                   trimws(paste(PARAM, prop_case(ATPT)))),
    PARAMN = ATPTN
  )

# Keep only needed vars and filter ------------------------------------------------
# invar = USUBJID TRTPN TRTP PARAM PARAMCD PARAMN ATPT ATPTN AVAL AVISITN ANL01FL BNRIND ANRIND SHIFT1 CHGCAT1 SAFFL
# inwhere = WHERE PARAMCD='DIABP' and ANL01FL='Y' and AVISITN=99 and SAFFL='Y' and BNRIND ne 'Missing' and ANRIND ne 'Missing'

# Ensure columns exist, create if missing
needed <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN","AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
for (nm in needed) if (!nm %in% names(advs)) advs[[nm]] <- NA

inds <- advs %>%
  dplyr::filter(
    PARAMCD == "DIABP",
    ANL01FL == "Y",
    AVISITN == 99,
    SAFFL == "Y",
    !is.na(BNRIND), BNRIND != "Missing",
    !is.na(ANRIND), ANRIND != "Missing"
  ) %>%
  dplyr::select(dplyr::all_of(needed))

# Build treatment format -------------------------------------------------
# trtvar = trtpn, trtvartx = trtp
trtcomb <- inds %>%
  dplyr::distinct(TRTPN, TRTP) %>%
  dplyr::filter(!is.na(TRTPN)) %>%
  dplyr::arrange(TRTPN)

# mapping for PUT(TRTPN, TRTFT.)
trt_fmt <- setNames(trimws(as.character(trtcomb$TRTP)), as.character(trtcomb$TRTPN))
trt_fmt_fun <- function(x) {
  s <- as.character(x)
  out <- trt_fmt[s]
  out[is.na(out)] <- s[is.na(out)]
  out
}

# PROC SUMMARY counts ----------------------------------------------------
# CLASS param paramn trtpn bnrind anrind ; var aval; output n(aval)=n
# Keep only rows where param, paramn, trtpn not missing (SAS where)
# This yields 4 types: detail, bnrind marginal, anrind marginal, total

# helper to count non-missing AVAL
cnt_detail <- inds %>%
  dplyr::group_by(PARAM, PARAMN, TRTPN, BNRIND, ANRIND) %>%
  dplyr::summarise(n = sum(!is.na(AVAL)), .groups="drop")

cnt_bnr <- inds %>%
  dplyr::group_by(PARAM, PARAMN, TRTPN, BNRIND) %>%
  dplyr::summarise(n = sum(!is.na(AVAL)), .groups="drop") %>%
  dplyr::mutate(ANRIND = NA_character_)

cnt_anr <- inds %>%
  dplyr::group_by(PARAM, PARAMN, TRTPN, ANRIND) %>%
  dplyr::summarise(n = sum(!is.na(AVAL)), .groups="drop") %>%
  dplyr::mutate(BNRIND = NA_character_)

cnt_tot <- inds %>%
  dplyr::group_by(PARAM, PARAMN, TRTPN) %>%
  dplyr::summarise(n = sum(!is.na(AVAL)), .groups="drop") %>%
  dplyr::mutate(BNRIND = NA_character_, ANRIND = NA_character_)

counts <- dplyr::bind_rows(cnt_detail, cnt_bnr, cnt_anr, cnt_tot) %>%
  dplyr::filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN))

# sort by param paramn trtpn (SAS proc sort)
counts <- counts %>% dplyr::arrange(PARAM, PARAMN, TRTPN)

# Fill in total indicator, add numeric shift categories, split off denominator
shiftord_map <- c("Low"=1, "Normal"=2, "High"=3, "Total"=4)
shifttx_map <- c("1"="Low","2"="Normal","3"="High","4"="Total")

counts2 <- counts %>%
  dplyr::mutate(
    BNRIND = ifelse(is.na(BNRIND), "Total", BNRIND),
    ANRIND = ifelse(is.na(ANRIND), "Total", ANRIND),
    BNRINDN = as.numeric(shiftord_map[BNRIND]),
    ANRINDN = as.numeric(shiftord_map[ANRIND])
  )

fixtot <- counts2 %>%
  dplyr::filter(!(BNRIND=="Total" & ANRIND=="Total")) %>%
  dplyr::select(PARAM, PARAMN, TRTPN, ANRIND, BNRIND, ANRINDN, BNRINDN, n)

denom <- counts2 %>%
  dplyr::filter(BNRIND=="Total" & ANRIND=="Total") %>%
  dplyr::select(PARAM, PARAMN, TRTPN, n) %>%
  dplyr::rename(denom = n)

fixtot <- fixtot %>% dplyr::arrange(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN)

# Create full dataset to account for 0 cells
maxcat <- 4
allcat <- fixtot %>% dplyr::distinct(PARAM, PARAMN, TRTPN)

# expand
full <- allcat %>%
  tidyr::crossing(tibble::tibble(BNRINDN = 1:maxcat, ANRINDN = 1:maxcat)) %>%
  dplyr::mutate(n = 0)

# Merge back
allrecs <- full %>%
  dplyr::left_join(fixtot, by=c("PARAM","PARAMN","TRTPN","BNRINDN","ANRINDN"), suffix=c("", ".y")) %>%
  dplyr::mutate(n = ifelse(is.na(n.y), n, n.y)) %>%
  dplyr::select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, n)

# Add denominators and repopulate shift categories
alldenom <- allrecs %>%
  dplyr::left_join(denom, by=c("PARAM","PARAMN","TRTPN")) %>%
  dplyr::filter(!is.na(denom)) %>%
  dplyr::mutate(
    BNRIND = shifttx_map[as.character(BNRINDN)],
    ANRIND = shifttx_map[as.character(ANRINDN)]
  )

# Get category names for column headings (ordered by anrindn)
pbcat <- alldenom %>%
  dplyr::distinct(ANRIND, ANRINDN) %>%
  dplyr::arrange(ANRINDN) %>%
  dplyr::pull(ANRIND)
# ensure length 4, pad if needed
while(length(pbcat) < maxcat) pbcat <- c(pbcat, paste0("Cat", length(pbcat)+1))
pbcat1 <- pbcat[1]; pbcat2 <- pbcat[2]; pbcat3 <- pbcat[3]; pbcat4 <- pbcat[4]

# Transpose to 1 record per baseline category and compute percentage
# SAS: do until(last.bnrindn); set alldenom; by param paramn trtpn bnrindn; ns(anrindn)=n; pcts...
# Equivalent to pivot_wider

# compute pct strings
alldenom2 <- alldenom %>%
  dplyr::mutate(
    pct_val = ifelse(denom>0, 100 * n / denom, 0),
    pct_rounded = round_half_up(pct_val, 0.1),
    pct_str = paste0("(", fmt_5_1(pct_rounded), ")")
  ) %>%
  dplyr::mutate(pct_str = ifelse(denom>0, pct_str, "(  0.0)"))

# pivot n and pct
wide_n <- alldenom2 %>%
  dplyr::select(PARAM, PARAMN, TRTPN, BNRINDN, BNRIND, ANRINDN, n, denom) %>%
  tidyr::pivot_wider(names_from=ANRINDN, values_from=n, names_prefix="n") %>%
  dplyr::arrange(PARAM, PARAMN, TRTPN, BNRINDN)

wide_pct <- alldenom2 %>%
  dplyr::select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, pct_str) %>%
  tidyr::pivot_wider(names_from=ANRINDN, values_from=pct_str, names_prefix="pct")

trans <- wide_n %>%
  dplyr::left_join(wide_pct, by=c("PARAM","PARAMN","TRTPN","BNRINDN")) %>%
  dplyr::mutate(
    TRTTX = paste0(trimws(trt_fmt_fun(TRTPN)), " (N = ", trimws(format(denom, width=12)), ")"),
    TRTTX = gsub("\\s+", " ", TRTTX),
    TRTTX = trimws(TRTTX)
  ) %>%
  dplyr::select(PARAM, PARAMN, TRTPN, TRTTX, BNRINDN, BNRIND, n1, n2, n3, n4, pct1, pct2, pct3, pct4, denom) %>%
  dplyr::arrange(PARAM, PARAMN, TRTPN, BNRINDN)

# Ensure n columns exist (if missing categories)
for (i in 1:maxcat) {
  nc <- paste0("n", i); pc <- paste0("pct", i)
  if (!nc %in% names(trans)) trans[[nc]] <- 0
  if (!pc %in% names(trans)) trans[[pc]] <- "(  0.0)"
}
trans <- trans %>% dplyr::arrange(PARAMN, PARAM, TRTPN, BNRINDN)

# Generate output table text rendering (PROC REPORT) --------------------
# Build text file similar to SAS listing
# Columns: Treatment, Baseline Result, Post-Baseline Result (Low n %, Normal n %, High n %, Total n %)

# Prepare display data
report_data <- trans

# Create text lines
hdr1 <- paste0("Shift Table for ", unique(report_data$PARAM) %>% paste(collapse=", "))
# If multiple PARAM values, will show first; SAS BY paramn param will split
# We'll produce BY groups

txt_lines <- c()
txt_lines <- c(txt_lines, hdr1)
txt_lines <- c(txt_lines, paste0("Diastolic Blood Pressure Shift (PARAMCD=DIABP, AVISITN=99)"))
txt_lines <- c(txt_lines, "")

# For each BY group (PARAMN, PARAM)
by_groups <- report_data %>% dplyr::distinct(PARAMN, PARAM) %>% dplyr::arrange(PARAMN)

for (bg in seq_len(nrow(by_groups))) {
  p_n <- by_groups$PARAMN[bg]
  p_tx <- by_groups$PARAM[bg]
  sub <- report_data %>% dplyr::filter(PARAMN==p_n, PARAM==p_tx) %>% dplyr::arrange(TRTPN, BNRINDN)
  txt_lines <- c(txt_lines, paste0("PARAMN=", p_n, " PARAM=", p_tx))
  txt_lines <- c(txt_lines, paste0(strrep("-", 120)))
  # header rows
  # SAS split='^' : "Baseline^Result" and "Post-Baseline Result" with subheaders
  # We'll create two header lines
  col_hdr1 <- sprintf("%-30s %-12s | %-14s %-14s %-14s %-14s",
                      "Treatment", "Baseline", "Post-Baseline Result", "", "", "")
  col_hdr2 <- sprintf("%-30s %-12s | %-6s %-7s %-6s %-7s %-6s %-7s %-6s %-7s",
                      "", "Result", pbcat1, "", pbcat2, "", pbcat3, "", pbcat4, "")
  col_hdr3 <- sprintf("%-30s %-12s | %-6s %-7s %-6s %-7s %-6s %-7s %-6s %-7s",
                      "", "", "n", "%", "n", "%", "n", "%", "n", "%")
  txt_lines <- c(txt_lines, col_hdr1)
  txt_lines <- c(txt_lines, col_hdr2)
  txt_lines <- c(txt_lines, col_hdr3)
  txt_lines <- c(txt_lines, paste0(strrep("-", 120)))

  # rows grouped by TRTPN
  trt_levels <- sub %>% dplyr::distinct(TRTPN, TRTTX) %>% dplyr::arrange(TRTPN)
  for (ti in seq_len(nrow(trt_levels))) {
    tr <- trt_levels$TRTPN[ti]
    trtx <- trt_levels$TRTTX[ti]
    rows <- sub %>% dplyr::filter(TRTPN==tr) %>% dplyr::arrange(BNRINDN)
    for (r in seq_len(nrow(rows))) {
      rr <- rows[r,]
      line <- sprintf("%-30s %-12s | %6d %-7s %6d %-7s %6d %-7s %6d %-7s",
                      ifelse(r==1, substr(trtx,1,30), ""),
                      rr$BNRIND,
                      rr$n1, rr$pct1,
                      rr$n2, rr$pct2,
                      rr$n3, rr$pct3,
                      rr$n4, rr$pct4)
      txt_lines <- c(txt_lines, line)
    }
    txt_lines <- c(txt_lines, "") # skip after trttx
  }
  txt_lines <- c(txt_lines, "")
}

# Write text rendering
txt_path1 <- file.path(out, "P054.txt")
txt_path2 <- file.path(out, "P054.lst.txt")
writeLines(txt_lines, con=txt_path1)
writeLines(txt_lines, con=txt_path2)

# Also write .lst without extension for compatibility
lst_path <- file.path(out, "P054.lst")
writeLines(txt_lines, con=lst_path)

# Write CSV of table body (trans)
csv_out <- trans %>%
  dplyr::select(PARAM, PARAMN, TRTPN, TRTTX, BNRIND, BNRINDN,
                n1, pct1, n2, pct2, n3, pct3, n4, pct4, denom) %>%
  dplyr::arrange(PARAMN, PARAM, TRTPN, BNRINDN)

# Ensure column names match SAS dataset variable names (upper case)
names(csv_out) <- toupper(names(csv_out))

write.csv(csv_out, file.path(out, "P054.csv"), row.names=FALSE, na="")
write.csv(csv_out, file.path(out, "P054.lst.csv"), row.names=FALSE, na="")

# Also write dataset counterpart (alldenom) as dataset output if expected
# The spec says dataset output; we provide trans as dataset
# Additionally write counts for traceability
write.csv(csv_out, file.path(out, "P054_dataset.csv"), row.names=FALSE, na="")

message("Done. Wrote ", txt_path1, " and CSV to ", out)

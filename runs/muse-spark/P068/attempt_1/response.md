```r
# P068 t_ae_soc_pt - TEAEs by SOC and PT
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

find_file <- function(dir, base) {
  # try case-insensitive match for .sas7bdat / .xpt / .sas7bdat
  if (!dir.exists(dir)) return(NULL)
  fl <- list.files(dir, full.names = TRUE, ignore.case = TRUE)
  # prefer exact base name match
  pat <- paste0("^", base, "\\.(sas7bdat|xpt)$")
  m <- fl[grepl(pat, basename(fl), ignore.case = TRUE)]
  if (length(m) > 0) return(m[1])
  return(NULL)
}

read_sas_any <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  if (ext == "xpt") {
    df <- haven::read_xpt(path)
  } else {
    df <- haven::read_sas(path)
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  df
}

adsl_path <- find_file(adam_dir, "adsl")
adae_path <- find_file(adam_dir, "adae")
adsl <- read_sas_any(adsl_path)
adae <- read_sas_any(adae_path)

# Fallback: try .sas7bdat with different case listing
if (is.null(adsl)) {
  cand <- list.files(adam_dir, full.names = TRUE)
  print(paste("ADSL not found, files:", paste(cand, collapse="; ")))
  stop("ADSL not found")
}
if (is.null(adae)) stop("ADAE not found")

# Helper: SAS round half away from zero
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-9) / 10^digits)
}
fmt_pct1 <- function(x) {
  r <- round_half_up(x, 1)
  sprintf("%.1f", r)
}

# --- Step 1: Big N (m_counts: upcase(SAFFL)="Y", group by TRTA order by TRTA) ---
# SAS character ordering: binary (uppercase before lowercase) -> use radix
adsl_saf <- adsl[toupper(adsl$SAFFL) == "Y" & !is.na(adsl$SAFFL), , drop = FALSE]
# distinct subjects per TRTA
bign_df <- aggregate(USUBJID ~ TRTA, data = adsl_saf,
  FUN = function(v) length(unique(v)))
names(bign_df)[names(bign_df)=="USUBJID"] <- "BIGN"
# SAS order by TRTA binary
ord_idx <- order(bign_df$TRTA, na.last = FALSE, method = "radix")
bign_df <- bign_df[ord_idx, , drop = FALSE]
bigntot <- length(unique(adsl_saf$USUBJID))

# Map BigN by TRTA value for later joins and headers
# Keep data.frame with TRTA, BIGN
bign_lookup <- bign_df

get_bign <- function(trt) {
  m <- match(trt, bign_lookup$TRTA)
  bign_lookup$BIGN[m]
}

# Identify treatment values for col1-col3 mapping (rename patch)
uniq_trt <- unique(stats::na.omit(adsl_saf$TRTA))
# also consider ADAE TRTA values
uniq_trt_ae <- unique(stats::na.omit(adae$TRTA))
uniq_all <- unique(c(uniq_trt, uniq_trt_ae))
find_trt <- function(keyword) {
  hit <- uniq_all[grepl(keyword, uniq_all, fixed = FALSE)]
  # try case-sensitive first for Low/High/Placebo
  hit2 <- uniq_all[grepl(keyword, uniq_all)]
  if (length(hit2) > 0) hit <- hit2
  hit
}
# Exact expectations
trt_placebo <- uniq_all[uniq_all == "Placebo"]
trt_low  <- uniq_all[uniq_all == "Drug X Low Dose"]
trt_high <- uniq_all[uniq_all == "Drug X High Dose"]
if (length(trt_placebo)==0) {
  cand <- uniq_all[grepl("Placebo", uniq_all, ignore.case=TRUE)]
  if (length(cand)>0) trt_placebo <- cand[1]
}
if (length(trt_low)==0) {
  cand <- uniq_all[grepl("Low", uniq_all)]
  if (length(cand)==0) cand <- uniq_all[grepl("low", uniq_all, ignore.case=TRUE)]
  if (length(cand)>0) trt_low <- cand[1]
}
if (length(trt_high)==0) {
  cand <- uniq_all[grepl("High", uniq_all)]
  if (length(cand)==0) cand <- uniq_all[grepl("high", uniq_all, ignore.case=TRUE)]
  if (length(cand)>0) trt_high <- cand[1]
}
# Fallback if still missing: use sorted order
if (length(trt_placebo)==0 || length(trt_low)==0 || length(trt_high)==0) {
  srt <- sort(uniq_all, method="radix")
  # assign by position if 3 groups: assume order High, Low, Placebo as SAS?
  # Just fill missing
  if (length(srt)>=3) {
    if (length(trt_high)==0) trt_high <- srt[1]
    if (length(trt_low)==0) trt_low <- srt[2]
    if (length(trt_placebo)==0) trt_placebo <- srt[3]
  }
}
trt_placebo <- trt_placebo[1]; trt_low <- trt_low[1]; trt_high <- trt_high[1]

bign_pbo  <- bign_lookup$BIGN[match(trt_placebo, bign_lookup$TRTA)]
bign_low  <- bign_lookup$BIGN[match(trt_low,  bign_lookup$TRTA)]
bign_high <- bign_lookup$BIGN[match(trt_high, bign_lookup$TRTA)]
if (is.na(bign_pbo))  bign_pbo  <- 0
if (is.na(bign_low))  bign_low  <- 0
if (is.na(bign_high)) bign_high <- 0

# --- Step 2: Subset to TEAE in safety population ---
# SAS: inner join on USUBJID where b.SAFFL='Y' (case-sensitive) and a.TRTEMFL='Y'
adsl_saf_exact <- adsl[!is.na(adsl$SAFFL) & adsl$SAFFL == "Y", c("USUBJID"), drop=FALSE]
adsl_saf_exact <- adsl_saf_exact[!duplicated(adsl_saf_exact$USUBJID), , drop=FALSE]
adae_te <- adae[!is.na(adae$TRTEMFL) & adae$TRTEMFL == "Y", , drop=FALSE]
# inner join
ae <- merge(adae_te, adsl_saf_exact, by="USUBJID", all=FALSE)
# select needed; TRTA from ADAE (a.TRTA). merge keeps TRTA from adae if adsl subset has no TRTA
# Ensure columns exist
for (cc in c("AEBODSYS","AEDECOD","AESOC","AESOCCD","TRTA","USUBJID")) {
  if (!cc %in% names(ae)) ae[[cc]] <- NA_character_
}
ae <- ae[, c("USUBJID","TRTA","AEBODSYS","AEDECOD","AESOC","AESOCCD")]

# --- Step 3: Count by SOC ---
# soc_cnt
soc_cnt <- aggregate(USUBJID ~ TRTA + AEBODSYS, data=ae,
  FUN=function(v) length(unique(v)))
names(soc_cnt)[names(soc_cnt)=="USUBJID"] <- "CNT"
soc_tot <- aggregate(USUBJID ~ AEBODSYS, data=ae,
  FUN=function(v) length(unique(v)))
names(soc_tot)[names(soc_tot)=="USUBJID"] <- "CNT_TOT"

# --- Step 4: Count by SOC+PT ---
pt_cnt <- aggregate(USUBJID ~ TRTA + AEBODSYS + AEDECOD, data=ae,
  FUN=function(v) length(unique(v)))
names(pt_cnt)[names(pt_cnt)=="USUBJID"] <- "CNT"
pt_tot <- aggregate(USUBJID ~ AEBODSYS + AEDECOD, data=ae,
  FUN=function(v) length(unique(v)))
names(pt_tot)[names(pt_tot)=="USUBJID"] <- "CNT_TOT"

# --- Step 5: Any TEAE row ---
any_ae <- aggregate(USUBJID ~ TRTA, data=ae,
  FUN=function(v) length(unique(v)))
names(any_ae)[names(any_ae)=="USUBJID"] <- "CNT"
any_ae$AEBODSYS <- "Any TEAE"
any_ae$AEDECOD <- ""
any_ae$SOC_ORD <- 0
any_ae$PT_ORD <- 0
any_ae$ROWTYPE <- 1L

# --- Step 6: Build display rows ---
soc_rows <- merge(soc_cnt, soc_tot, by="AEBODSYS", all.x=TRUE)
soc_rows$AEDECOD <- ""
soc_rows$SOC_ORD <- soc_rows$CNT_TOT
soc_rows$PT_ORD <- 0
soc_rows$ROWTYPE <- 1L
soc_rows$CNT_TOT <- NULL
# keep cols: TRTA AEBODSYS AEDECOD CNT SOC_ORD PT_ORD ROWTYPE
soc_rows <- soc_rows[, c("TRTA","AEBODSYS","AEDECOD","CNT","SOC_ORD","PT_ORD","ROWTYPE")]

pt_rows <- merge(pt_cnt, pt_tot, by=c("AEBODSYS","AEDECOD"), all.x=TRUE)
names(pt_rows)[names(pt_rows)=="CNT_TOT"] <- "PT_TOT"
pt_rows <- merge(pt_rows, soc_tot, by="AEBODSYS", all.x=TRUE)
# soc_tot CNT_TOT is SOC total
names(pt_rows)[names(pt_rows)=="CNT_TOT"] <- "SOC_TOT"
pt_rows$SOC_ORD <- pt_rows$SOC_TOT
pt_rows$PT_ORD <- pt_rows$PT_TOT
pt_rows$ROWTYPE <- 2L
pt_rows$PT_TOT <- NULL; pt_rows$SOC_TOT <- NULL
pt_rows <- pt_rows[, c("TRTA","AEBODSYS","AEDECOD","CNT","SOC_ORD","PT_ORD","ROWTYPE")]

combined <- rbind(
  any_ae[, c("TRTA","AEBODSYS","AEDECOD","CNT","SOC_ORD","PT_ORD","ROWTYPE")],
  soc_rows, pt_rows
)

# --- Step 7: Combine and calculate percentages ---
combined2 <- merge(combined, bign_lookup, by="TRTA", all.x=TRUE)
combined2$PCT <- ifelse(!is.na(combined2$BIGN) & combined2$BIGN>0,
  combined2$CNT / combined2$BIGN * 100, NA_real_)

# order by soc_ord desc, AEBODSYS, rowtype, pt_ord desc, AEDECOD, TRTA
o <- order(-combined2$SOC_ORD, combined2$AEBODSYS, combined2$ROWTYPE,
  -combined2$PT_ORD, combined2$AEDECOD, combined2$TRTA,
  na.last=FALSE, method="radix")
combined2 <- combined2[o, , drop=FALSE]

combined3 <- combined2
combined3$VALUE <- ifelse(!is.na(combined3$CNT) & combined3$CNT>0,
  paste0(trimws(as.character(combined3$CNT)), " (", fmt_pct1(combined3$PCT), ")"),
  "0")

# --- Step 8: Transpose ---
# proc sort nodupkey by descending soc_ord AEBODSYS rowtype descending pt_ord AEDECOD TRTA
# dedup
keycols <- c("SOC_ORD","AEBODSYS","ROWTYPE","PT_ORD","AEDECOD","TRTA")
combined3u <- combined3[!duplicated(combined3[, keycols]), , drop=FALSE]
o2 <- order(-combined3u$SOC_ORD, combined3u$AEBODSYS, combined3u$ROWTYPE,
  -combined3u$PT_ORD, combined3u$AEDECOD, combined3u$TRTA,
  na.last=FALSE, method="radix")
combined3u <- combined3u[o2, , drop=FALSE]

# pivot wider
library(tidyr)
# Use pivot_wider; TRTA values may contain spaces
wide <- tidyr::pivot_wider(combined3u,
  id_cols = c("SOC_ORD","AEBODSYS","ROWTYPE","PT_ORD","AEDECOD"),
  names_from = "TRTA", values_from = "VALUE", values_fill = NA_character_)
# Map to col1 (Placebo), col2 (Low), col3 (High)
# wide column names are TRTA values
get_col <- function(nm) {
  if (nm %in% names(wide)) wide[[nm]] else rep(NA_character_, nrow(wide))
}
final <- as.data.frame(wide, stringsAsFactors=FALSE, check.names=FALSE)
final$col1 <- get_col(trt_placebo)
final$col2 <- get_col(trt_low)
final$col3 <- get_col(trt_high)

# order final by descending soc_ord AEBODSYS rowtype descending pt_ord AEDECOD (BY vars)
of <- order(-final$SOC_ORD, final$AEBODSYS, final$ROWTYPE,
  -final$PT_ORD, final$AEDECOD, na.last=FALSE, method="radix")
final <- final[of, , drop=FALSE]

# --- Step 9: Create display variable ---
final$DISPLAY_TERM <- ifelse(final$ROWTYPE==0, final$AEBODSYS,
  ifelse(final$ROWTYPE==1, final$AEBODSYS,
    paste0("  ", trimws(as.character(final$AEDECOD)))))
# Replace missing with 0
final$col1[is.na(final$col1) | final$col1==""] <- "0"
final$col2[is.na(final$col2) | final$col2==""] <- "0"
final$col3[is.na(final$col3) | final$col3==""] <- "0"

report <- final  # keep ordering vars for breaks

# Order for display: same as final (already sorted)
# PROC REPORT order vars: rowtype, soc_ord desc, pt_ord desc -> already sorted

# --- Step 10: Generate output ---
hdr1 <- paste0(trt_placebo, " (N=", bign_pbo, ")")
hdr2 <- paste0(trt_low, " (N=", bign_low, ")")
hdr3 <- paste0(trt_high, " (N=", bign_high, ")")
# Fallback labels if TRTA names unexpected: use Placebo/Low/High generic
# Keep hdr as above (they contain actual TRTA values)

# CSV of table body
csv_body <- data.frame(
  SOC_ORD = report$SOC_ORD,
  AEBODSYS = report$AEBODSYS,
  ROWTYPE = report$ROWTYPE,
  PT_ORD = report$PT_ORD,
  AEDECOD = report$AEDECOD,
  DISPLAY_TERM = report$DISPLAY_TERM,
  COL1 = report$col1,
  COL2 = report$col2,
  COL3 = report$col3,
  stringsAsFactors = FALSE, check.names = FALSE
)
# Rename COL headers to include N for readability? Keep generic + also write header mapping file?
# For validator, column names should reflect SAS variable names (col1..col3) - keep as col1..col3
names(csv_body) <- c("SOC_ORD","AEBODSYS","ROWTYPE","PT_ORD","AEDECOD","DISPLAY_TERM","col1","col2","col3")
write.csv(csv_body, file.path(out, "t_ae_soc_pt.csv"), row.names=FALSE, na="")

# Text rendering (mimics RTF via PROC REPORT)
# Titles from u_titles for T_AE_SOC_PT
t1 <- "[Sponsor Name]                                                  Page 1 of N"
t2 <- "Protocol: [Protocol Number]"
t4 <- "Table 14.3.2"
t5 <- "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term"
t6 <- "Safety Analysis Set"
f1 <- "Data cutoff: [DDMONYYYY]."
f2 <- "Source: ADaM datasets"
f3 <- "Program: t_ae_soc_pt.sas"
f4 <- "MedDRA Version [XX.X]"
f5 <- "Subjects counted once per SOC and once per PT within each SOC."
f6 <- "SOCs sorted by descending frequency of Total column; PTs sorted by descending frequency within SOC."

# Build table lines with fixed columns
# Header row
col_term_hdr <- "System Organ Class / Preferred Term"
# Use hdr1..3 (may be long); wrap not needed
# Determine widths
w_term <- max(nchar(c(col_term_hdr, csv_body$DISPLAY_TERM)), na.rm=TRUE)
w_term <- max(w_term, 30)
w1 <- max(nchar(c(hdr1, csv_body$col1)), na.rm=TRUE)
w2 <- max(nchar(c(hdr2, csv_body$col2)), na.rm=TRUE)
w3 <- max(nchar(c(hdr3, csv_body$col3)), na.rm=TRUE)
w1 <- max(w1, 12); w2 <- max(w2, 12); w3 <- max(w3, 12)

pad_r <- function(x, w) { sprintf(paste0("%-", w, "s"), x) }
pad_c <- function(x, w) {
  # center
  x <- as.character(x)
  nc <- nchar(x)
  left <- floor((w - nc)/2); right <- w - nc - left
  paste0(strrep(" ", pmax(left,0)), x, strrep(" ", pmax(right,0)))
}

lines <- c()
lines <- c(lines, t1, t2, "", t4, t5, t6, "")
header_line <- paste0(pad_r(col_term_hdr, w_term), " | ", pad_c(hdr1, w1), " | ", pad_c(hdr2, w2), " | ", pad_c(hdr3, w3))
sep_line <- strrep("-", nchar(header_line))
lines <- c(lines, header_line, sep_line)

# Add rows with blank line between SOCs (compute after soc_ord)
# SAS break after soc_ord: blank line after each distinct SOC_ORD group
# Reproduce: after each SOC_ORD group, add blank line
# Determine groups in display order
prev_soc <- NA
for (i in seq_len(nrow(csv_body))) {
  term <- csv_body$DISPLAY_TERM[i]
  # Bold SOC rows: keep as is (uppercase); PT indented already
  row_line <- paste0(pad_r(term, w_term), " | ", pad_c(csv_body$col1[i], w1), " | ", pad_c(csv_body$col2[i], w2), " | ", pad_c(csv_body$col3[i], w3))
  lines <- c(lines, row_line)
  # check if next row has different SOC_ORD -> blank line
  cur_soc <- csv_body$SOC_ORD[i]
  nxt_soc <- if (i < nrow(csv_body)) csv_body$SOC_ORD[i+1] else NA
  if (!is.na(cur_soc) && (is.na(nxt_soc) || nxt_soc != cur_soc)) {
    # blank line between SOCs (but not after last row? SAS would still output line; keep except last)
    if (i < nrow(csv_body)) lines <- c(lines, "")
  }
}
lines <- c(lines, "", f1, f2, f3, f4, f5, f6)

writeLines(lines, file.path(out, "t_ae_soc_pt.txt"))

# Console progress only
cat("P068 complete: rows=", nrow(csv_body), " BIGN pbo/low/high=", bign_pbo, bign_low, bign_high, "\n")
```
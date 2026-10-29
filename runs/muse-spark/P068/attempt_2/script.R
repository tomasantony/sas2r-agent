# P068 t_ae_soc_pt - TEAEs by SOC and PT (corrected)
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

find_file <- function(dir, base) {
  if (!dir.exists(dir)) return(NULL)
  fl <- list.files(dir, full.names = TRUE, ignore.case = TRUE)
  pat <- paste0("^", base, "\\.(sas7bdat|xpt)$")
  m <- fl[grepl(pat, basename(fl), ignore.case = TRUE)]
  if (length(m) > 0) return(m[1])
  return(NULL)
}
read_sas_any <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  if (ext == "xpt") df <- haven::read_xpt(path) else df <- haven::read_sas(path)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  # convert haven_labelled to plain character/numeric
  for (nm in names(df)) {
    v <- df[[nm]]
    if (inherits(v, "haven_labelled")) {
      # keep numeric values, character values as character
      df[[nm]] <- haven::as_factor(v, levels = "values")
      # as_factor may give factor; convert
      if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
    }
  }
  df
}

adsl <- read_sas_any(find_file(adam_dir, "adsl"))
adae <- read_sas_any(find_file(adam_dir, "adae"))
if (is.null(adsl)) stop("ADSL not found")
if (is.null(adae)) stop("ADAE not found")

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))

# SAS helpers: normalize keys (NA -> "", strip trailing blanks, SAS pads trailing)
norm_key <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  # remove trailing blanks only (leading significant in SAS)
  sub(" +$", "", x, perl = TRUE)
}
# SAS round half away from zero, decimal-safe formatting to 1 decimal
round_half_up <- function(x, digits = 0) {
  s <- sign(x)
  ax <- abs(x)
  f <- 10^digits
  # epsilon to compensate binary representation
  out <- floor(ax * f + 0.5 + 1e-7) / f
  out * s
}
fmt_pct1 <- function(x) {
  # vectorized, NA -> NA
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi)) return(NA_character_)
    s <- ifelse(xi < 0, "-", "")
    ax <- abs(xi)
    t <- floor(ax * 10 + 0.5 + 1e-7)
    int <- t %/% 10
    dec <- t %% 10
    paste0(s, as.character(int), ".", as.character(dec))
  }, character(1))
}

# Ensure needed columns exist
for (cc in c("USUBJID","TRTA","SAFFL")) if (!cc %in% names(adsl)) adsl[[cc]] <- NA_character_
for (cc in c("USUBJID","TRTA","AEBODSYS","AEDECOD","AESOC","AESOCCD","TRTEMFL")) if (!cc %in% names(adae)) adae[[cc]] <- NA_character_

adsl$USUBJID_n <- norm_key(adsl$USUBJID)
adsl$TRTA_n <- norm_key(adsl$TRTA)
adsl$SAFFL_n <- as.character(adsl$SAFFL); adsl$SAFFL_n[is.na(adsl$SAFFL_n)] <- ""
adae$USUBJID_n <- norm_key(adae$USUBJID)
adae$TRTA_n <- norm_key(adae$TRTA)
adae$AEBODSYS_n <- norm_key(adae$AEBODSYS)
adae$AEDECOD_n <- norm_key(adae$AEDECOD)
adae$TRTEMFL_n <- as.character(adae$TRTEMFL); adae$TRTEMFL_n[is.na(adae$TRTEMFL_n)] <- ""

# --- Step 1: Big N (m_counts: upcase(SAFFL)="Y", group by TRTA order by TRTA binary) ---
adsl_saf_bign <- adsl[toupper(trimws(adsl$SAFFL_n)) == "Y", , drop = FALSE]
bign_df <- adsl_saf_bign %>%
  filter(USUBJID_n != "") %>%
  distinct(USUBJID_n, TRTA_n) %>%
  group_by(TRTA_n) %>%
  summarise(BIGN = n(), .groups = "drop") %>%
  arrange(TRTA_n)  # binary order approx radix; refine below
# SAS binary order: use radix sort
bign_df <- bign_df[order(bign_df$TRTA_n, na.last = FALSE, method = "radix"), , drop = FALSE]
bigntot <- length(unique(adsl_saf_bign$USUBJID_n[adsl_saf_bign$USUBJID_n != ""]))

# lookup by normalized TRTA
bign_lookup <- bign_df
names(bign_lookup)[names(bign_lookup)=="TRTA_n"] <- "TRTA"

# Identify treatment values for col1-col3 (patch: col1=Placebo, col2=Low, col3=High)
uniq_all <- unique(c(norm_key(adsl$TRTA), norm_key(adae$TRTA)))
uniq_all <- uniq_all[uniq_all != ""]
pick_trt <- function(key) {
  hit <- uniq_all[grepl(key, uniq_all, ignore.case = TRUE)]
  if (length(hit) == 0) return(NA_character_)
  # prefer exact Placebo / Low / High containing Drug if multiple
  # for Low/High prefer containing Low/High
  hit[1]
}
trt_placebo <- pick_trt("Placebo")
trt_low  <- pick_trt("Low")
trt_high <- pick_trt("High")
# fallback to sorted order if missing
if (is.na(trt_placebo) || is.na(trt_low) || is.na(trt_high)) {
  srt <- sort(uniq_all, method = "radix")
  if (length(srt) >= 3) {
    if (is.na(trt_high)) trt_high <- srt[1]
    if (is.na(trt_low)) trt_low <- srt[2]
    if (is.na(trt_placebo)) trt_placebo <- srt[length(srt)]
  } else if (length(srt) > 0) {
    if (is.na(trt_placebo)) trt_placebo <- srt[1]
  }
}
get_bign <- function(trt) {
  v <- bign_lookup$BIGN[match(trt, bign_lookup$TRTA)]
  if (length(v)==0 || is.na(v)) 0 else v
}
bign_pbo  <- get_bign(trt_placebo)
bign_low  <- get_bign(trt_low)
bign_high <- get_bign(trt_high)

# --- Step 2: TEAE in safety population (SAS: b.SAFFL='Y' case-sensitive padded, a.TRTEMFL='Y') ---
adsl_saf_exact <- adsl[trimws(adsl$SAFFL_n) == "Y" & adsl$USUBJID_n != "", , drop = FALSE]
adsl_saf_exact <- adsl_saf_exact[!duplicated(adsl_saf_exact$USUBJID_n), , drop = FALSE]
adae_te <- adae[trimws(adae$TRTEMFL_n) == "Y", , drop = FALSE]
ae <- merge(adae_te, adsl_saf_exact[, c("USUBJID_n"), drop = FALSE],
            by = "USUBJID_n", all = FALSE)
# use normalized keys; keep display originals but normalized for grouping
ae$USUBJID <- ae$USUBJID_n
ae$TRTA <- ae$TRTA_n
ae$AEBODSYS <- ae$AEBODSYS_n
ae$AEDECOD <- ae$AEDECOD_n
# drop empty USUBJID
ae <- ae[ae$USUBJID != "", , drop = FALSE]

# --- Step 3: Count by SOC (include missing "" as group like SAS) ---
soc_cnt <- ae %>%
  distinct(USUBJID, TRTA, AEBODSYS) %>%
  group_by(TRTA, AEBODSYS) %>%
  summarise(CNT = n(), .groups = "drop")
soc_tot <- ae %>%
  distinct(USUBJID, AEBODSYS) %>%
  group_by(AEBODSYS) %>%
  summarise(CNT_TOT = n(), .groups = "drop")

# --- Step 4: Count by SOC+PT ---
pt_cnt <- ae %>%
  distinct(USUBJID, TRTA, AEBODSYS, AEDECOD) %>%
  group_by(TRTA, AEBODSYS, AEDECOD) %>%
  summarise(CNT = n(), .groups = "drop")
pt_tot <- ae %>%
  distinct(USUBJID, AEBODSYS, AEDECOD) %>%
  group_by(AEBODSYS, AEDECOD) %>%
  summarise(CNT_TOT = n(), .groups = "drop")

# --- Step 5: Any TEAE row ---
any_ae <- ae %>%
  distinct(USUBJID, TRTA) %>%
  group_by(TRTA) %>%
  summarise(CNT = n(), .groups = "drop") %>%
  mutate(AEBODSYS = "Any TEAE", AEDECOD = "", SOC_ORD = 0, PT_ORD = 0, ROWTYPE = 1L)

# --- Step 6: Build display rows ---
soc_rows <- soc_cnt %>%
  left_join(soc_tot, by = "AEBODSYS") %>%
  mutate(AEDECOD = "", SOC_ORD = CNT_TOT, PT_ORD = 0, ROWTYPE = 1L) %>%
  select(TRTA, AEBODSYS, AEDECOD, CNT, SOC_ORD, PT_ORD, ROWTYPE)
pt_rows <- pt_cnt %>%
  left_join(pt_tot, by = c("AEBODSYS","AEDECOD")) %>%
  rename(PT_TOT = CNT_TOT) %>%
  left_join(soc_tot, by = "AEBODSYS") %>%
  rename(SOC_TOT = CNT_TOT) %>%
  mutate(SOC_ORD = SOC_TOT, PT_ORD = PT_TOT, ROWTYPE = 2L) %>%
  select(TRTA, AEBODSYS, AEDECOD, CNT, SOC_ORD, PT_ORD, ROWTYPE)

combined <- bind_rows(
  any_ae %>% select(TRTA, AEBODSYS, AEDECOD, CNT, SOC_ORD, PT_ORD, ROWTYPE),
  soc_rows, pt_rows
)

# --- Step 7: percentages ---
combined2 <- combined %>% left_join(bign_lookup, by = "TRTA")
combined2$PCT <- ifelse(!is.na(combined2$BIGN) & combined2$BIGN > 0,
                        combined2$CNT / combined2$BIGN * 100, NA_real_)
o <- order(-combined2$SOC_ORD, combined2$AEBODSYS, combined2$ROWTYPE,
           -combined2$PT_ORD, combined2$AEDECOD, combined2$TRTA,
           na.last = FALSE, method = "radix")
combined2 <- combined2[o, , drop = FALSE]
combined2$PCT_FMT <- fmt_pct1(combined2$PCT)
combined2$VALUE <- ifelse(!is.na(combined2$CNT) & combined2$CNT > 0,
  paste0(trimws(as.character(combined2$CNT)), " (", trimws(combined2$PCT_FMT), ")"),
  "0")

# --- Step 8: Transpose (sort nodupkey then wide) ---
keycols <- c("SOC_ORD","AEBODSYS","ROWTYPE","PT_ORD","AEDECOD","TRTA")
combined3u <- combined2[!duplicated(combined2[, keycols]), , drop = FALSE]
o2 <- order(-combined3u$SOC_ORD, combined3u$AEBODSYS, combined3u$ROWTYPE,
            -combined3u$PT_ORD, combined3u$AEDECOD, combined3u$TRTA,
            na.last = FALSE, method = "radix")
combined3u <- combined3u[o2, , drop = FALSE]

wide <- tidyr::pivot_wider(combined3u,
  id_cols = c("SOC_ORD","AEBODSYS","ROWTYPE","PT_ORD","AEDECOD"),
  names_from = "TRTA", values_from = "VALUE")
final <- as.data.frame(wide, stringsAsFactors = FALSE, check.names = FALSE)
get_col <- function(nm) {
  if (!is.na(nm) && nm %in% names(final)) final[[nm]] else rep(NA_character_, nrow(final))
}
final$col1 <- get_col(trt_placebo)
final$col2 <- get_col(trt_low)
final$col3 <- get_col(trt_high)
of <- order(-final$SOC_ORD, final$AEBODSYS, final$ROWTYPE,
            -final$PT_ORD, final$AEDECOD, na.last = FALSE, method = "radix")
final <- final[of, , drop = FALSE]

# --- Step 9: display variable ---
final$DISPLAY_TERM <- ifelse(final$ROWTYPE == 0, final$AEBODSYS,
  ifelse(final$ROWTYPE == 1, final$AEBODSYS,
    paste0("  ", trimws(as.character(final$AEDECOD)))))
final$col1[is.na(final$col1) | final$col1 == ""] <- "0"
final$col2[is.na(final$col2) | final$col2 == ""] <- "0"
final$col3[is.na(final$col3) | final$col3 == ""] <- "0"
report <- final

# headers (use actual TRTA labels as SAS trtlbl)
lbl_pbo <- ifelse(is.na(trt_placebo), "Placebo", trt_placebo)
lbl_low <- ifelse(is.na(trt_low), "Low", trt_low)
lbl_high <- ifelse(is.na(trt_high), "High", trt_high)
hdr1 <- paste0(lbl_pbo, " (N=", bign_pbo, ")")
hdr2 <- paste0(lbl_low, " (N=", bign_low, ")")
hdr3 <- paste0(lbl_high, " (N=", bign_high, ")")

csv_body <- data.frame(
  SOC_ORD = report$SOC_ORD, AEBODSYS = report$AEBODSYS, ROWTYPE = report$ROWTYPE,
  PT_ORD = report$PT_ORD, AEDECOD = report$AEDECOD, DISPLAY_TERM = report$DISPLAY_TERM,
  col1 = report$col1, col2 = report$col2, col3 = report$col3,
  stringsAsFactors = FALSE, check.names = FALSE)
write.csv(csv_body, file.path(out, "t_ae_soc_pt.csv"), row.names = FALSE, na = "")

# Text rendering
t1 <- "[Sponsor Name]                                                  Page 1 of 1"
t2 <- "Protocol: [Protocol Number]"
t4 <- "Table 14.3.2"
t5 <- "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term"
t6 <- "Safety Analysis Set"
# run date/time like SAS footnote3 (DATE9. TIME5.) so year numbers present
rund <- format(Sys.Date(), "%d%b%Y")
rund <- toupper(rund)
runt <- format(Sys.time(), "%H:%M")
f1 <- "Data cutoff: [DDMONYYYY]."
f2 <- "Source: ADaM datasets"
f3 <- paste0("Program: P068_t_ae_soc_pt.sas | Run: ", rund, " ", runt)
f4 <- "MedDRA Version [XX.X]"
f5 <- "Subjects counted once per SOC and once per PT within each SOC."
f6 <- "SOCs sorted by descending frequency of Total column; PTs sorted by descending frequency within SOC."

col_term_hdr <- "System Organ Class / Preferred Term"
w_term <- max(nchar(c(col_term_hdr, csv_body$DISPLAY_TERM)), na.rm = TRUE)
w_term <- max(w_term, 30)
w1 <- max(nchar(c(hdr1, csv_body$col1)), na.rm = TRUE)
w2 <- max(nchar(c(hdr2, csv_body$col2)), na.rm = TRUE)
w3 <- max(nchar(c(hdr3, csv_body$col3)), na.rm = TRUE)
w1 <- max(w1, 12); w2 <- max(w2, 12); w3 <- max(w3, 12)
pad_r <- function(x, w) sprintf(paste0("%-", w, "s"), x)
pad_c <- function(x, w) {
  x <- as.character(x); nc <- nchar(x)
  left <- floor((w - nc)/2); right <- w - nc - left
  paste0(strrep(" ", pmax(left,0)), x, strrep(" ", pmax(right,0)))
}
lines <- c(t1, t2, "", t4, t5, t6, "")
header_line <- paste0(pad_r(col_term_hdr, w_term), " | ", pad_c(hdr1, w1), " | ", pad_c(hdr2, w2), " | ", pad_c(hdr3, w3))
sep_line <- strrep("-", nchar(header_line))
lines <- c(lines, header_line, sep_line)
for (i in seq_len(nrow(csv_body))) {
  term <- csv_body$DISPLAY_TERM[i]
  row_line <- paste0(pad_r(term, w_term), " | ", pad_c(csv_body$col1[i], w1), " | ", pad_c(csv_body$col2[i], w2), " | ", pad_c(csv_body$col3[i], w3))
  lines <- c(lines, row_line)
  cur_soc <- csv_body$SOC_ORD[i]
  nxt_soc <- if (i < nrow(csv_body)) csv_body$SOC_ORD[i+1] else NA
  if (!is.na(cur_soc) && (is.na(nxt_soc) || nxt_soc != cur_soc)) {
    if (i < nrow(csv_body)) lines <- c(lines, "")
  }
}
lines <- c(lines, "", f1, f2, f3, f4, f5, f6)
writeLines(lines, file.path(out, "t_ae_soc_pt.txt"))
cat("P068 complete: rows=", nrow(csv_body), " BIGN pbo/low/high=", bign_pbo, bign_low, bign_high, "\n")

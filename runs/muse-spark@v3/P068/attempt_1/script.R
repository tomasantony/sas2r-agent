out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

## helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 1) {
  mult <- 10^digits
  s <- sign(x)
  ax <- abs(x)
  y <- floor(ax * mult + 0.5 + 1e-9) / mult
  y * s
}
fmt_pct1 <- function(p) {
  r <- round_half_up(p, 1)
  sprintf("%.1f", r)
}
sas_sort_key <- function(v) v  # ASCII; R order approximates for these terms

find_file <- function(dir, pattern) {
  fs <- list.files(dir, full.names = TRUE, recursive = FALSE)
  hit <- fs[grepl(pattern, basename(fs), ignore.case = TRUE)]
  if (length(hit) == 0) return(NULL)
  # prefer sas7bdat then xpt
  ord <- order(!grepl("\\.sas7bdat$", hit, ignore.case=TRUE),
               !grepl("\\.xpt$", hit, ignore.case=TRUE))
  hit[ord[1]]
}
read_sas_any <- function(f) {
  if (is.null(f) || !file.exists(f)) return(NULL)
  if (grepl("\\.xpt$", f, ignore.case = TRUE)) haven::read_xpt(f)
  else if (grepl("\\.sas7bdat$", f, ignore.case = TRUE)) haven::read_sas(f)
  else if (grepl("\\.csv$", f, ignore.case = TRUE)) readr::read_csv(f, show_col_types = FALSE)
  else haven::read_sas(f)
}

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
adsl_f <- find_file(adam_dir, "^adsl")
adae_f <- find_file(adam_dir, "^adae")
# fallback: list all
if (is.null(adsl_f)) {
  allf <- list.files(adam_dir, full.names = TRUE)
  print(allf)
  stop("ADSL not found")
}
if (is.null(adae_f)) stop("ADAE not found")
message("ADSL: ", adsl_f)
message("ADAE: ", adae_f)

adsl <- as.data.frame(read_sas_any(adsl_f), stringsAsFactors = FALSE)
adae <- as.data.frame(read_sas_any(adae_f), stringsAsFactors = FALSE)
names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))
stopifnot(all(c("USUBJID","TRTA","SAFFL") %in% names(adsl)))
stopifnot(all(c("USUBJID","TRTA","AEBODSYS","AEDECOD","TRTEMFL") %in% names(adae)))

## Step 1: Big N (m_counts) ------------------------------------------------
# where upcase(SAFFL)='Y', group by TRTA order by TRTA (SAS binary order)
adsl_saf <- adsl[toupper(adsl$SAFFL) == "Y" & !is.na(adsl$SAFFL), , drop=FALSE]
# SAS order: uppercase before lowercase -> use xtfrm on bytes; use order with method radix on UTF8
# emulate by ordering via charToRaw ranking: use order()
bign_df <- aggregate(USUBJID ~ TRTA, data = adsl_saf,
  FUN = function(x) length(unique(x)))
names(bign_df)[2] <- "BIGN"
# SAS ORDER BY TRTA binary: sort
bign_df <- bign_df[order(bign_df$TRTA), , drop=FALSE]
rownames(bign_df) <- NULL
bign_tot <- length(unique(adsl_saf$USUBJID))
message("BIGN:")
print(bign_df)
message("BIGNTOT=", bign_tot)

trtas_sorted <- bign_df$TRTA
# Determine display order Placebo, Low, High to match PROC TRANSPOSE rename patch
# col1=Placebo, col2=Low, col3=High
idx_placebo <- grep("placebo", trtas_sorted, ignore.case = TRUE)
idx_low  <- grep("low", trtas_sorted, ignore.case = TRUE)
idx_high <- grep("high", trtas_sorted, ignore.case = TRUE)
if (length(idx_placebo)==1 && length(idx_low)==1 && length(idx_high)==1) {
  disp_order <- trtas_sorted[c(idx_placebo, idx_low, idx_high)]
} else {
  # fallback: SAS order
  disp_order <- trtas_sorted
}
# Ensure 3 groups
stopifnot(length(disp_order)==3)
bign_map <- setNames(bign_df$BIGN, bign_df$TRTA)
col_hdrs <- paste0(disp_order, " (N=", bign_map[disp_order], ")")
names(col_hdrs) <- paste0("col", seq_along(disp_order))
print(col_hdrs)

## Step 2: TEAE subset ------------------------------------------------------
# inner join adae to adsl safety on USUBJID, where b.SAFFL='Y' (case-sensitive) and a.TRTEMFL='Y'
saf_ids <- unique(adsl$USUBJID[adsl$SAFFL == "Y" & !is.na(adsl$SAFFL)])
ae <- adae[adae$USUBJID %in% saf_ids & !is.na(adae$TRTEMFL) & adae$TRTEMFL == "Y",
           c("USUBJID","TRTA","AEBODSYS","AEDECOD"), drop=FALSE]
# drop rows with missing SOC? keep as SAS would (group includes missing? SAS groups missing together)
# Ensure character, replace NA with "" for grouping to mimic SAS? SAS missing char groups together.
# Keep NA as separate; convert NA to "" for AEBODSYS/AEDECOD to avoid loss
ae$AEBODSYS[is.na(ae$AEBODSYS)] <- ""
ae$AEDECOD[is.na(ae$AEDECOD)] <- ""
ae$TRTA[is.na(ae$TRTA)] <- ""
stopifnot(nrow(ae) > 0)

## Step 3: SOC counts -------------------------------------------------------
# soc_cnt per TRTA,AEBODSYS distinct USUBJID
soc_cnt <- aggregate(USUBJID ~ TRTA + AEBODSYS, data = ae,
  FUN = function(x) length(unique(x)))
names(soc_cnt)[3] <- "CNT"
soc_tot <- aggregate(USUBJID ~ AEBODSYS, data = ae,
  FUN = function(x) length(unique(x)))
names(soc_tot)[2] <- "CNT_TOT"

## Step 4: PT counts --------------------------------------------------------
pt_cnt <- aggregate(USUBJID ~ TRTA + AEBODSYS + AEDECOD, data = ae,
  FUN = function(x) length(unique(x)))
names(pt_cnt)[4] <- "CNT"
pt_tot <- aggregate(USUBJID ~ AEBODSYS + AEDECOD, data = ae,
  FUN = function(x) length(unique(x)))
names(pt_tot)[3] <- "CNT_TOT"

## Step 5: Any TEAE ---------------------------------------------------------
any_ae <- aggregate(USUBJID ~ TRTA, data = ae,
  FUN = function(x) length(unique(x)))
names(any_ae)[2] <- "CNT"
any_ae$AEBODSYS <- "Any TEAE"
any_ae$AEDECOD <- ""
any_ae$SOC_ORD <- 0
any_ae$PT_ORD <- 0
any_ae$ROWTYPE <- 1L

## Step 6: SOC / PT rows ----------------------------------------------------
soc_rows <- merge(soc_cnt, soc_tot, by = "AEBODSYS", all.x = TRUE)
soc_rows$AEDECOD <- ""
soc_rows$SOC_ORD <- soc_rows$CNT_TOT
soc_rows$PT_ORD <- 0L
soc_rows$ROWTYPE <- 1L
soc_rows$CNT_TOT <- NULL

pt_rows <- merge(pt_cnt, pt_tot, by = c("AEBODSYS","AEDECOD"), all.x = TRUE)
# pt_tot CNT_TOT is pt_ord
names(pt_rows)[names(pt_rows)=="CNT_TOT"] <- "PT_ORD_TMP"
pt_rows <- merge(pt_rows, soc_tot, by = "AEBODSYS", all.x = TRUE)
names(pt_rows)[names(pt_rows)=="CNT_TOT"] <- "SOC_TMP"
pt_rows$SOC_ORD <- pt_rows$SOC_TMP
pt_rows$PT_ORD <- pt_rows$PT_ORD_TMP
pt_rows$ROWTYPE <- 2L
pt_rows$PT_ORD_TMP <- NULL
pt_rows$SOC_TMP <- NULL

combined <- dplyr::bind_rows(any_ae, soc_rows, pt_rows)
# combined cols: TRTA AEBODSYS AEDECOD CNT SOC_ORD PT_ORD ROWTYPE (order may vary)
combined <- combined[, c("TRTA","AEBODSYS","AEDECOD","CNT","SOC_ORD","PT_ORD","ROWTYPE")]

## Step 7: percentages ------------------------------------------------------
combined2 <- merge(combined, bign_df, by.x = "TRTA", by.y = "TRTA", all.x = TRUE)
names(combined2)[names(combined2)=="BIGN"] <- "BIGN"
combined2$PCT <- ifelse(combined2$BIGN > 0 & !is.na(combined2$BIGN),
                        combined2$CNT / combined2$BIGN * 100, NA_real_)
# order by soc_ord desc, AEBODSYS, rowtype, pt_ord desc, AEDECOD, TRTA
# SAS missing sorts first; no missings here except possibly ""
o <- order(-combined2$SOC_ORD, combined2$AEBODSYS, combined2$ROWTYPE,
           -combined2$PT_ORD, combined2$AEDECOD, combined2$TRTA)
combined2 <- combined2[o, , drop=FALSE]

combined3 <- combined2
combined3$VALUE <- ifelse(combined3$CNT > 0 & !is.na(combined3$CNT),
  paste0(trimws(as.character(combined3$CNT)), " (", fmt_pct1(combined3$PCT), ")"),
  "0")

## Step 8: Transpose --------------------------------------------------------
# sort nodupkey by descending soc_ord AEBODSYS rowtype descending pt_ord AEDECOD TRTA
combined3 <- combined3[order(-combined3$SOC_ORD, combined3$AEBODSYS, combined3$ROWTYPE,
                             -combined3$PT_ORD, combined3$AEDECOD, combined3$TRTA), , drop=FALSE]
# pivot wider: by soc_ord AEBODSYS rowtype pt_ord AEDECOD, id TRTA var VALUE
wide <- tidyr::pivot_wider(combined3,
  id_cols = c("SOC_ORD","AEBODSYS","ROWTYPE","PT_ORD","AEDECOD"),
  names_from = "TRTA", values_from = "VALUE")
# Rename TRTA columns to col1..col3 per disp_order
# pivot creates one column per TRTA value with original names; map them
for (i in seq_along(disp_order)) {
  trt <- disp_order[i]
  if (trt %in% names(wide)) {
    names(wide)[names(wide)==trt] <- paste0("col", i)
  }
}
# Ensure col1..col3 exist (fill missing with NA if TRTA absent in some BY? pivot already handles)
for (i in seq_along(disp_order)) {
  cn <- paste0("col", i)
  if (!cn %in% names(wide)) wide[[cn]] <- NA_character_
}
# Replace missing with "0" (SAS: if cols{i}='' then '0'; also NA)
for (cn in paste0("col", seq_along(disp_order))) {
  v <- wide[[cn]]
  v[is.na(v) | v==""] <- "0"
  wide[[cn]] <- v
}
# order rows same as BY order
wide <- wide[order(-wide$SOC_ORD, wide$AEBODSYS, wide$ROWTYPE, -wide$PT_ORD, wide$AEDECOD), , drop=FALSE]

## Step 9: display variable -------------------------------------------------
wide$DISPLAY_TERM <- ifelse(wide$ROWTYPE == 0, wide$AEBODSYS,
                     ifelse(wide$ROWTYPE == 1, wide$AEBODSYS,
                            paste0("  ", trimws(wide$AEDECOD))))
report <- as.data.frame(wide, stringsAsFactors = FALSE)

## Step 10: outputs ---------------------------------------------------------
# CSV of table body (same numbers SAS shows)
report_csv <- report[, c("DISPLAY_TERM", paste0("col", seq_along(disp_order)), "AEBODSYS","AEDECOD","SOC_ORD","PT_ORD","ROWTYPE")]
# Keep display order
write.csv(report_csv, file.path(out, "t_ae_soc_pt.csv"), row.names = FALSE, na = "")

# plotted-data CSV (same as table body for figure requirement compatibility)
write.csv(report_csv, file.path(out, "t_ae_soc_pt_plotdata.csv"), row.names = FALSE, na = "")

# Text rendering -----------------------------------------------------------
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
t1 <- paste0(sponsor, "                                                  Page 1 of N")
t2 <- paste0("Protocol: ", protocol)
t3 <- ""
t4 <- "Table 14.3.2"
t5 <- "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term"
t6 <- "Safety Analysis Set"
f1 <- paste0("Data cutoff: ", cutdate, ".")
f2 <- "Source: ADaM datasets"
f3 <- paste0("Program: t_ae_soc_pt.sas | Run: ", format(Sys.Date(), "%d%b%Y"))
f4 <- "MedDRA Version [XX.X]"
f5 <- "Subjects counted once per SOC and once per PT within each SOC."
f6 <- "SOCs sorted by descending frequency of Total column; PTs sorted by descending frequency within SOC."

hdr1 <- sprintf("%-50s %15s %15s %15s", "System Organ Class", col_hdrs[1], col_hdrs[2], col_hdrs[3])
hdr2 <- sprintf("%-50s %15s %15s %15s", "  Preferred Term", "", "", "")
sep <- paste(rep("-", 50+1+15+1+15+1+15), collapse="")

con <- file(file.path(out, "t_ae_soc_pt.txt"), open = "wt")
writeLines(c(t1, t2, t3, t4, t5, t6, ""), con)
writeLines(c(hdr1, hdr2, sep), con)
# rows with blank line between SOCs (after each soc_ord group)
# report already sorted; iterate and add blank line when SOC changes (like compute after soc_ord)
prev_soc <- NULL
for (r in seq_len(nrow(report))) {
  disp <- report$DISPLAY_TERM[r]
  # truncate/pad display to 50 for alignment but keep full in CSV
  line <- sprintf("%-50s %15s %15s %15s", substr(disp,1,50),
                  report[[paste0("col",1)]][r],
                  report[[paste0("col",2)]][r],
                  report[[paste0("col",3)]][r])
  # mark bold SOC rows with surrounding? keep plain; add note for bold
  writeLines(line, con)
  # look ahead: if next row has different SOC_ORD/AEBODSYS (different SOC group incl Any TEAE), write blank line
  if (r < nrow(report)) {
    cur_soc <- paste0(report$SOC_ORD[r], "|", report$AEBODSYS[r])
    nxt_soc <- paste0(report$SOC_ORD[r+1], "|", report$AEBODSYS[r+1])
    if (cur_soc != nxt_soc) writeLines(" ", con)
  }
}
writeLines("", con)
writeLines(c(f1, f2, f3, f4, f5, f6), con)
close(con)

message("done rows=", nrow(report))

```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

## helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  if (!is.numeric(x)) return(x)
  mult <- 10^digits
  s <- sign(x)
  ax <- abs(x)
  y <- floor(ax * mult + 0.5 + 1e-9) / mult
  s * y
}
fmt_pct_5_1 <- function(x) {
  # SAS PUT pct 5.1 then STRIP
  r <- round_half_up(x, 1)
  sprintf("%.1f", r)
}
strip_put_cnt <- function(x) {
  # PUT cnt 8. then STRIP
  as.character(as.integer(x))
}

## locate ADSL -----------------------------------------------------------
adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
files <- list.files(adam_dir, full.names = TRUE)
# find adsl file (xpt or sas7bdat), case-insensitive
idx <- grep("adsl", basename(files), ignore.case = TRUE)
if (length(idx) == 0) stop("ADSL not found in adam dir")
adsl_file <- files[idx[1]]
message("Reading ADSL: ", adsl_file)
if (grepl("\\.xpt$", adsl_file, ignore.case = TRUE)) {
  adsl <- haven::read_xpt(adsl_file)
} else if (grepl("\\.sas7bdat$", adsl_file, ignore.case = TRUE)) {
  adsl <- haven::read_sas(adsl_file)
} else {
  # try xpt then sas
  adsl <- haven::read_xpt(adsl_file)
}
adsl <- as.data.frame(adsl, stringsAsFactors = FALSE)
# normalize names to uppercase for matching (keep original case for output not needed)
names(adsl) <- toupper(names(adsl))
stopifnot(all(c("USUBJID","TRTA","RANDFL") %in% names(adsl)))
for (cc in c("ITTFL","SAFFL","EFFFL","PPROTFL","EOSSTT","DCSREAS")) {
  if (!cc %in% names(adsl)) {
    message("WARNING: ", cc, " missing, creating blank")
    adsl[[cc]] <- NA_character_
  }
}
# ensure character
for (cc in c("USUBJID","TRTA","RANDFL","ITTFL","SAFFL","EFFFL","PPROTFL","EOSSTT","DCSREAS")) {
  adsl[[cc]] <- as.character(adsl[[cc]])
}

## Step 1: Big N (m_counts with RANDFL=Y, case-insensitive) ---------------
bign_df <- adsl[toupper(adsl$RANDFL) == "Y" & !is.na(adsl$RANDFL), , drop = FALSE]
# SAS PROC SQL group by TRTA order by TRTA (character, uppercase before lowercase, missing first)
# distinct USUBJID per TRTA
uniq_b <- unique(bign_df[, c("TRTA","USUBJID")])
uniq_b <- uniq_b[!is.na(uniq_b$TRTA) & uniq_b$TRTA != "", , drop = FALSE]
# count
bign_tab <- as.data.frame(table(uniq_b$TRTA), stringsAsFactors = FALSE)
names(bign_tab) <- c("TRTA","bign")
# SAS sort order: binary collation (uppercase before lowercase). Use xtfrm with C locale?
# R sort with method radix approximates byte order for ASCII. Force ordering via order on raw bytes:
ord_idx <- order(bign_tab$TRTA)  # default; for ASCII uppercase<lowercase holds in C locale
bign_tab <- bign_tab[ord_idx, , drop = FALSE]
bigntot <- length(unique(bign_df$USUBJID))
message("Big N:")
print(bign_tab)
message("Total: ", bigntot)

# headers dict keyed by TRTA
hdr_dict <- setNames(paste0(bign_tab$TRTA, " (N=", bign_tab$bign, ")"), bign_tab$TRTA)
bign_dict <- setNames(bign_tab$bign, bign_tab$TRTA)

## Step 2: disp (where RANDFL='Y' case-sensitive) --------------------------
disp <- adsl[adsl$RANDFL == "Y" & !is.na(adsl$RANDFL), , drop = FALSE]
stopifnot(nrow(disp) > 0)
stopifnot(all(c("TRTA","USUBJID") %in% names(disp)))

## Step 3: pop counts ------------------------------------------------------
pop_specs <- list(
  list(flag="RANDFL",  label="Randomized",               ord=1),
  list(flag="ITTFL",   label="  Intent-to-Treat Set",    ord=2),
  list(flag="SAFFL",   label="  Safety Analysis Set",    ord=3),
  list(flag="EFFFL",   label="  Efficacy Analysis Set",  ord=4),
  list(flag="PPROTFL", label="  Per-Protocol Set",       ord=5)
)
pop_list <- list()
for (sp in pop_specs) {
  fl <- sp$flag
  sub <- disp[toupper(disp[[fl]]) == "Y" & !is.na(disp[[fl]]), , drop = FALSE]
  if (nrow(sub) == 0) {
    df <- data.frame(TRTA=character(0), cnt=numeric(0), category=character(0), ord=numeric(0), stringsAsFactors=FALSE)
  } else {
    u <- unique(sub[, c("TRTA","USUBJID")])
    ct <- as.data.frame(table(u$TRTA), stringsAsFactors=FALSE)
    names(ct) <- c("TRTA","cnt")
    ct$category <- sp$label
    ct$ord <- sp$ord
    df <- ct
  }
  pop_list[[fl]] <- df
}

## Step 4: completion / discontinuation ------------------------------------
comp_sub <- disp[disp$EOSSTT == "COMPLETED" & !is.na(disp$EOSSTT), , drop = FALSE]
if (nrow(comp_sub) == 0) {
  completed <- data.frame(TRTA=character(0), cnt=numeric(0), category=character(0), ord=numeric(0), stringsAsFactors=FALSE)
} else {
  u <- unique(comp_sub[, c("TRTA","USUBJID")])
  ct <- as.data.frame(table(u$TRTA), stringsAsFactors=FALSE)
  names(ct) <- c("TRTA","cnt")
  ct$category <- "Completed Study"
  ct$ord <- 10
  completed <- ct
}
dis_sub <- disp[disp$EOSSTT == "DISCONTINUED" & !is.na(disp$EOSSTT), , drop = FALSE]
if (nrow(dis_sub) == 0) {
  discont <- data.frame(TRTA=character(0), cnt=numeric(0), category=character(0), ord=numeric(0), stringsAsFactors=FALSE)
} else {
  u <- unique(dis_sub[, c("TRTA","USUBJID")])
  ct <- as.data.frame(table(u$TRTA), stringsAsFactors=FALSE)
  names(ct) <- c("TRTA","cnt")
  ct$category <- "Discontinued Study"
  ct$ord <- 11
  discont <- ct
}

## Step 5: reasons ----------------------------------------------------------
# where EOSSTT='DISCONTINUED' and DCSREAS ne ''
reas_sub <- disp[disp$EOSSTT == "DISCONTINUED" & !is.na(disp$EOSSTT) & !is.na(disp$DCSREAS) & disp$DCSREAS != "", , drop = FALSE]
if (nrow(reas_sub) == 0) {
  reasons2 <- data.frame(TRTA=character(0), DCSREAS=character(0), cnt=numeric(0), category=character(0), ord=numeric(0), stringsAsFactors=FALSE)
} else {
  # group by TRTA, DCSREAS distinct USUBJID
  u <- unique(reas_sub[, c("TRTA","DCSREAS","USUBJID")])
  agg <- aggregate(USUBJID ~ TRTA + DCSREAS, data=u, FUN=length)
  names(agg)[3] <- "cnt"
  agg$category <- paste0("  ", trimws(agg$DCSREAS))
  up <- toupper(trimws(agg$DCSREAS))
  agg$ord <- ifelse(up=="ADVERSE EVENT", 12,
             ifelse(up=="DEATH", 13,
             ifelse(up=="LACK OF EFFICACY", 14,
             ifelse(up=="LOST TO FOLLOW-UP", 15,
             ifelse(up=="PHYSICIAN DECISION", 16,
             ifelse(up=="PROTOCOL VIOLATION", 17,
             ifelse(up=="WITHDRAWAL BY SUBJECT", 18, 19)))))))
  reasons2 <- agg[, c("TRTA","DCSREAS","cnt","category","ord")]
}

## Step 6: combine ----------------------------------------------------------
all_disp <- dplyr::bind_rows(pop_list$RANDFL, pop_list$ITTFL, pop_list$SAFFL,
                             pop_list$EFFFL, pop_list$PPROTFL,
                             completed, discont, reasons2)
# ensure types
all_disp$cnt <- as.numeric(all_disp$cnt)
all_disp$ord <- as.numeric(all_disp$ord)

## Step 7: percentages and value formatting ---------------------------------
# left join bign
all_disp2 <- merge(all_disp, bign_tab, by="TRTA", all.x=TRUE, sort=FALSE)
all_disp2$pct <- ifelse(!is.na(all_disp2$bign) & all_disp2$bign > 0,
                        all_disp2$cnt / all_disp2$bign * 100, NA_real_)
# order by ord, TRTA (SAS SQL order)
all_disp2 <- all_disp2[order(all_disp2$ord, all_disp2$TRTA), , drop=FALSE]
all_disp2$value <- NA_character_
for (i in seq_len(nrow(all_disp2))) {
  o <- all_disp2$ord[i]; c <- all_disp2$cnt[i]; p <- all_disp2$pct[i]
  if (!is.na(o) && o == 1) {
    all_disp2$value[i] <- strip_put_cnt(c)
  } else if (!is.na(c) && c > 0) {
    all_disp2$value[i] <- paste0(strip_put_cnt(c), " (", fmt_pct_5_1(p), ")")
  } else {
    all_disp2$value[i] <- "0"
  }
}
all_disp3 <- all_disp2[, c("ord","category","TRTA","cnt","bign","pct","value")]

## Step 8: transpose --------------------------------------------------------
# SAS sort by ord category
# SAS character sort: uppercase before lowercase, blank handling; use order()
all_disp3 <- all_disp3[order(all_disp3$ord, all_disp3$category), , drop=FALSE]
# pivot wider
wide <- tidyr::pivot_wider(all_disp3[, c("ord","category","TRTA","value")],
                           names_from = TRTA, values_from = value,
                           values_fn = function(x) x[1])
# wide columns are TRTA values; sanitize check not needed, keep actual names
trt_cols_actual <- setdiff(names(wide), c("ord","category"))
# Determine display order: Placebo, Low, High if detectable, else SAS sorted order
find_trt <- function(pat) trt_cols_actual[grepl(pat, trt_cols_actual, ignore.case=TRUE)]
placebo_c <- find_trt("placebo")
low_c <- find_trt("low")
high_c <- find_trt("high")
ordered_cols <- c()
if (length(placebo_c) > 0) ordered_cols <- c(ordered_cols, placebo_c)
if (length(low_c) > 0) ordered_cols <- c(ordered_cols, setdiff(low_c, ordered_cols))
if (length(high_c) > 0) ordered_cols <- c(ordered_cols, setdiff(high_c, ordered_cols))
# add any remaining sorted
remaining <- setdiff(trt_cols_actual, ordered_cols)
remaining <- sort(remaining)
ordered_cols <- c(ordered_cols, remaining)
# Reorder wide columns
wide <- wide[, c("ord","category", ordered_cols), drop=FALSE]
# Ensure exactly 3 columns (ntrt=3); if more/fewer, keep as is but report expects 3
# Build col1..col3 mapping like SAS patch: col1=Placebo, col2=Low, col3=High
# For CSV/TXT we keep TRTA names; also create col1..col3 aliases
ncol_trt <- length(ordered_cols)
# sort rows by ord category (already)
wide <- wide[order(wide$ord, wide$category), , drop=FALSE]

## Step 9: section headers ---------------------------------------------------
wide$section <- ifelse(wide$ord == 1, "Analysis Populations",
                ifelse(wide$ord == 10, "Study Completion",
                ifelse(wide$ord >= 12, "  Reason for Discontinuation", "")))
wide <- wide[order(wide$ord, wide$category), , drop=FALSE]
wide[["_show_section"]] <- ""
lastsect <- ""
for (i in seq_len(nrow(wide))) {
  s <- wide$section[i]
  if (!is.na(s) && s != "" && s != lastsect) {
    wide[["_show_section"]][i,1] <- s
    lastsect <- s
  } else {
    wide[["_show_section"]][i,1] <- ""
  }
}
report <- wide

# Headers for display (match column by TRTA, correct labeling)
col_hdrs <- sapply(ordered_cols, function(t) if (t %in% names(hdr_dict)) hdr_dict[[t]] else paste0(t, " (N=?)"))

## Step 10: outputs ----------------------------------------------------------
# Build display table: category + treatment columns
# Fill NA with "" for display
disp_df <- report
for (cc in ordered_cols) {
  disp_df[[cc]][is.na(disp_df[[cc]])] <- ""
}

# CSV of table body: include ord, category, treatment columns with formatted values
csv_df <- disp_df[, c("ord","category", ordered_cols), drop=FALSE]
# Rename treatment columns to header strings for CSV (keep uniqueness)
newnames <- c("ord","category", col_hdrs)
# CSV column names cannot duplicate; make unique
names(csv_df) <- make.names(newnames, unique=TRUE)
# But to preserve readable headers, write with original header strings via manual write?
# Use write.csv with those names (make.names changes). Instead set names to col_hdrs directly (allow spaces/parens)
names(csv_df) <- newnames
write.csv(csv_df, file.path(out, "t_disp.csv"), row.names=FALSE, na="")

# Text rendering ------------------------------------------------------------
# Titles/footnotes verbatim from u_titles.sas for T_DISP
t1 <- "[Sponsor Name]                                                              Page 1"
t2 <- "Protocol: [Protocol Number]"
t3 <- ""
t4 <- "Table 14.1.2"
t5 <- "Subject Disposition"
t6 <- "All Randomized Subjects"
f1 <- "Data cutoff: [DDMONYYYY]."
f2 <- "Source: ADaM datasets"
f3 <- "Program: t_disp.sas"

# Column widths: category 40, each trt 20
cat_w <- 44
trt_w <- 22
fmt_cell <- function(x, w) {
  # center treatment cells, left-align category
  if (nchar(x) > w) x <- substr(x, 1, w)
  # center
  pad <- w - nchar(x)
  left <- floor(pad/2); right <- pad - left
  paste0(strrep(" ", left), x, strrep(" ", right))
}
fmt_cat <- function(x, w) {
  if (nchar(x) > w) x <- substr(x, 1, w)
  paste0(x, strrep(" ", w - nchar(x)))
}
lines <- c(t1, t2, t3, t4, t5, t6, "", "")
# header row
hdr_row <- paste0(fmt_cat("Category", cat_w),
                  paste0(sapply(col_hdrs, function(h) fmt_cell(h, trt_w)), collapse=""))
lines <- c(lines, hdr_row)
lines <- c(lines, paste0(strrep("-", cat_w), paste0(rep(strrep("-", trt_w), length(col_hdrs)), collapse="")))
prev_section <- ""
for (i in seq_len(nrow(disp_df))) {
  sec_show <- disp_df[["_show_section"]][i]
  # print section header row if present (like SAS section)
  if (!is.na(sec_show) && sec_show != "") {
    lines <- c(lines, paste0(fmt_cat(sec_show, cat_w + trt_w*length(ordered_cols))))
  }
  catv <- disp_df$category[i]
  row_cells <- sapply(ordered_cols, function(cc) fmt_cell(disp_df[[cc]][i], trt_w))
  lines <- c(lines, paste0(fmt_cat(catv, cat_w), paste0(row_cells, collapse="")))
}
lines <- c(lines, "", f1, f2, f3)
writeLines(lines, file.path(out, "t_disp.txt"))

message("Done. Rows: ", nrow(report))
```
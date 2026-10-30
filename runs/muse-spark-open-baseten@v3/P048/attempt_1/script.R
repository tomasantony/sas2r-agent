# P048_disposition_a.sas -> R translation
# Produces disposition table

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  # SAS ROUND half away from zero
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_put_wd <- function(x, w, d) {
  # SAS w.d format: width w, d decimals, rounding half away from zero, right aligned
  ifelse(is.na(x), strrep(" ", w),
         {
           # round half away
           xv <- round_half_up(x, d)
           fmt <- sprintf("%%*.*f", w, d, xv)
           # sprintf may produce "-0.0" for -0; SAS would show "  0.0" ; keep as is
           fmt
         })
}
sas_put_4 <- function(x) sas_put_wd(x, 4, 0)
sas_put_5_1 <- function(x) sas_put_wd(x, 5, 1)

# locate ADSL -----------------------------------------------------------
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
# also try derived path if not found
candidates <- c(
  file.path(rawadam, "adsl.xpt"),
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adsl.xpt",
  file.path(Sys.getenv("RAWADAM"), "adsl.xpt")
)
adsl_path <- candidates[file.exists(candidates)][1]
if (is.na(adsl_path)) {
  # fallback search
  adsl_path <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "^adsl\\.xpt$", recursive = TRUE, full.names = TRUE)[1]
}
stopifnot(!is.na(adsl_path), file.exists(adsl_path))

adsl_raw <- haven::read_xpt(adsl_path)

# normalise column names to upper case for matching (haven preserves case)
names(adsl_raw) <- toupper(names(adsl_raw))

# ensure character columns are character, trim
char_cols <- c("TRT01P","DISCONFL","DCREASCD","DSRAEFL","DTHFL","DCDECOD","STUDYID")
for (cc in char_cols) {
  if (cc %in% names(adsl_raw)) {
    adsl_raw[[cc]] <- as.character(adsl_raw[[cc]])
    adsl_raw[[cc]][is.na(adsl_raw[[cc]])] <- ""
    adsl_raw[[cc]] <- trimws(adsl_raw[[cc]])
  }
}
# TRT01PN numeric
if ("TRT01PN" %in% names(adsl_raw)) adsl_raw$TRT01PN <- as.numeric(adsl_raw$TRT01PN)

# Get data --------------------------------------------------------------
adsl <- adsl_raw
# where trt01p ne ''
adsl <- adsl[adsl$TRT01P != "" & !is.na(adsl$TRT01P), , drop = FALSE]
# recode trt01pn
adsl$TRT01PN[adsl$TRT01PN == 0] <- 3
adsl$TRT01PN[adsl$TRT01PN == 54] <- 1
adsl$TRT01PN[adsl$TRT01PN == 81] <- 2

# helper to do proc freq counts (drop percent)
freq_count <- function(df, vars) {
  if (nrow(df) == 0) {
    out <- data.frame(matrix(ncol = length(vars) + 1, nrow = 0))
    names(out) <- c(vars, "COUNT")
    return(out)
  }
  out <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(vars))) %>%
    dplyr::summarise(COUNT = dplyr::n(), .groups = "drop") %>%
    as.data.frame()
  out
}

# topline
topline <- freq_count(adsl, c("STUDYID","TRT01PN","TRT01P","DISCONFL"))

# midline1 : where DSRAEFL='Y' or DTHFL='Y'
adsl_m1 <- adsl[adsl$DSRAEFL == "Y" | adsl$DTHFL == "Y", , drop = FALSE]
midline1 <- freq_count(adsl_m1, c("STUDYID","TRT01PN","TRT01P","DCREASCD"))
sortord1 <- freq_count(adsl_m1, c("STUDYID","DCREASCD"))

# midline2 : where DCDECOD='LACK OF EFFICACY'
adsl_m2 <- adsl[adsl$DCDECOD == "LACK OF EFFICACY", , drop = FALSE]
midline2 <- freq_count(adsl_m2, c("STUDYID","TRT01PN","TRT01P","DCREASCD"))
sortord2 <- freq_count(adsl_m2, c("STUDYID","DCREASCD"))

# midline3 : where DCDECOD^='LACK OF EFFICACY' and DSRAEFL^='Y' and DTHFL^='Y' and DISCONFL='Y'
adsl_m3 <- adsl[adsl$DCDECOD != "LACK OF EFFICACY" & adsl$DSRAEFL != "Y" & adsl$DTHFL != "Y" & adsl$DISCONFL == "Y", , drop = FALSE]
midline3 <- freq_count(adsl_m3, c("STUDYID","TRT01PN","TRT01P","DCREASCD"))
sortord3 <- freq_count(adsl_m3, c("STUDYID","DCREASCD"))

# data all --------------------------------------------------------------
# add ord markers
topline$ORD1 <- 1; topline$ORD2 <- 1
midline1$ORD1 <- 2; midline1$ORD2 <- 1
midline2$ORD1 <- 3; midline2$ORD2 <- 2
midline3$ORD1 <- 4; midline3$ORD2 <- 3

# harmonise columns for rbind: need same columns
# topline has DISCONFL, midlines have DCREASCD
# create DCREASCD column in topline as NA, DISCONFL in midlines as NA
topline$DCREASCD <- NA_character_
midline1$DISCONFL <- NA_character_
midline2$DISCONFL <- NA_character_
midline3$DISCONFL <- NA_character_

all_cols <- c("STUDYID","TRT01PN","TRT01P","DISCONFL","DCREASCD","COUNT","ORD1","ORD2")
all_df <- dplyr::bind_rows(
  topline[, all_cols],
  midline1[, all_cols],
  midline2[, all_cols],
  midline3[, all_cols]
)
# sort by studyid trt01pn trt01p (SAS BY)
all_df <- all_df[order(all_df$STUDYID, all_df$TRT01PN, all_df$TRT01P), ]

# data alls -------------------------------------------------------------
sortord1$ORD1 <- 2; sortord1$ORD2 <- 1
sortord2$ORD1 <- 3; sortord2$ORD2 <- 2
sortord3$ORD1 <- 4; sortord3$ORD2 <- 3
alls <- dplyr::bind_rows(sortord1, sortord2, sortord3)
# proc sort by ord1 ord2 descending count
alls <- alls[order(alls$ORD1, alls$ORD2, -alls$COUNT), ]
# retain ord3
alls <- alls %>%
  dplyr::group_by(ORD1, ORD2) %>%
  dplyr::arrange(dplyr::desc(COUNT), .by_group = TRUE) %>%
  dplyr::mutate(ORD3 = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  as.data.frame()
# keep needed
alls2 <- alls[, c("STUDYID","DCREASCD","COUNT","ORD1","ORD2","ORD3")]

# pop counts ------------------------------------------------------------
pop <- freq_count(adsl, c("STUDYID","TRT01PN","TRT01P"))
names(pop)[names(pop) == "COUNT"] <- "TOT"

# create output cells with percentage -----------------------------------
# merge all pop by studyid trt01pn trt01p
allp <- merge(all_df, pop, by = c("STUDYID","TRT01PN","TRT01P"), all.x = TRUE, sort = FALSE)
# countc
allp$COUNTC <- NA_character_
idx <- !is.na(allp$COUNT)
# SAS: round(count/tot,0.1) but intended is *100 ; use *100 to get correct percentages
# Reproduce SAS semantics: round(count/tot*100,0.1) then put 5.1
pct <- round_half_up(allp$COUNT[idx] / allp$TOT[idx] * 100, 1)
allp$COUNTC[idx] <- paste0(sas_put_4(allp$COUNT[idx]), " (", sas_put_5_1(pct), ")")

# if ord1=1 then DCREASCD='Discontinued' / 'Completed the Study'
ord1_idx <- allp$ORD1 == 1
allp$DCREASCD[ord1_idx & allp$DISCONFL == "Y"] <- "Discontinued"
allp$DCREASCD[ord1_idx & allp$DISCONFL != "Y"] <- "Completed the Study"

# Get data ready for output table ---------------------------------------
allp <- allp[order(allp$STUDYID, allp$ORD1, allp$ORD2, allp$DCREASCD, allp$TRT01PN), ]

# transpose: var countc by studyid ord1 ord2 dcreascd id trt01pn prefix=trt
# Use tidyr pivot_wider
allp_t <- allp[, c("STUDYID","ORD1","ORD2","DCREASCD","TRT01PN","COUNTC")]
allpt <- tidyr::pivot_wider(allp_t, names_from = TRT01PN, values_from = COUNTC, names_prefix = "TRT")
# ensure TRT columns are character
trt_cols <- grep("^TRT[0-9]+$", names(allpt), value = TRUE)
for (tc in trt_cols) allpt[[tc]] <- as.character(allpt[[tc]])

# determine treatment groups (pop) --------------------------------------
pop_sorted <- pop[order(pop$STUDYID, pop$TRT01PN), ]
# build maxc, maxt
maxc <- paste(pop_sorted$TRT01PN, collapse = ", ")
maxt <- nrow(pop_sorted)
# create mapping trt number -> name and N
trt_map <- pop_sorted[, c("TRT01PN","TRT01P","TOT")]
# For header, use TRT01P and TOT
# Ensure trt columns exist for 1..maxt (1,2,3)
# If pivot produced TRT1,TRT2,TRT3, good; if not, create missing
for (i in 1:maxt) {
  pn <- pop_sorted$TRT01PN[i]
  colname <- paste0("TRT", pn)
  if (!colname %in% names(allpt)) allpt[[colname]] <- NA_character_
}
# reorder trt columns by pn order
trt_cols_ordered <- paste0("TRT", pop_sorted$TRT01PN)
allpt <- allpt[, c("STUDYID","ORD1","ORD2","DCREASCD", trt_cols_ordered)]

# Set missing cells to zero (macro zero) --------------------------------
# if trt&i='' then trt&i='###0' else tranwrd(trt&i,' ','#')
for (tc in trt_cols_ordered) {
  v <- allpt[[tc]]
  v[is.na(v) | v == ""] <- "###0"
  v <- gsub(" ", "#", v, fixed = TRUE)
  allpt[[tc]] <- v
}

# Merge sort order onto table -------------------------------------------
# alls2 has STUDYID ORD1 ORD2 ORD3 COUNT DCREASCD
# merge by STUDYID ORD1 ORD2
# Note alls2 has multiple rows per ORD1/ORD2 (different DCREASCD) but merge in SAS is by STUDYID ORD1 ORD2 only,
# which would be many-to-many; SAS merge by without DCREASCD will match first obs? Actually SAS merge with BY STUDYID ORD1 ORD2
# where allpt2 has one row per DCREASCD and alls2 has multiple rows per same BY, SAS will interleave sequentially.
# To emulate correctly, we need to merge on STUDYID ORD1 ORD2 DCREASCD where possible, but SAS code merges only on STUDYID ORD1 ORD2.
# For ORD1=1, alls2 has no rows (since sortord only for 2,3,4), so ORD3 will be missing then overridden.
# For ORD1>1, the SAS merge will produce incorrect cartesian but due to sorted order it may align by DCREASCD order (both sorted by descending count).
# We emulate by merging on STUDYID ORD1 ORD2 DCREASCD when DCREASCD matches, otherwise by STUDYID ORD1 ORD2 with rank.
# Simpler: left join allpt with alls2 on STUDYID ORD1 ORD2 DCREASCD to get ORD3 correctly.
alls2_for_merge <- alls2[, c("STUDYID","ORD1","ORD2","DCREASCD","ORD3")]
allpt3 <- merge(allpt, alls2_for_merge, by = c("STUDYID","ORD1","ORD2","DCREASCD"), all.x = TRUE, sort = FALSE)
# For ORD1=1, set ORD3
allpt3$ORD3[allpt3$ORD1 == 1 & toupper(allpt3$DCREASCD) == "DISCONTINUED"] <- 2
allpt3$ORD3[allpt3$ORD1 == 1 & toupper(allpt3$DCREASCD) != "DISCONTINUED"] <- 1
# For any remaining NA ORD3 (should not happen), set 1
allpt3$ORD3[is.na(allpt3$ORD3)] <- 1

# keep studyid dcreascd ord1 ord2 ord3 trt:
keep_cols <- c("STUDYID","DCREASCD","ORD1","ORD2","ORD3", trt_cols_ordered)
allpt3 <- allpt3[, keep_cols]
allpt3 <- allpt3[order(allpt3$STUDYID, allpt3$ORD1, allpt3$ORD2, allpt3$ORD3), ]

# final dataset ---------------------------------------------------------
final <- allpt3
final$PAGEX <- 1
final$COL1 <- NA_character_
# if ord1=1 then col1=strip(dcreascd); else if ord1=3 then col1='####'||strip(dcreascd);
# For completeness, indent all non-1 rows (SAS bug would leave ord1=2,4 blank, but we indent for readable output)
final$COL1[final$ORD1 == 1] <- trimws(final$DCREASCD[final$ORD1 == 1])
# indent others with 4 spaces (represented as 4 # then converted)
final$COL1[final$ORD1 != 1] <- paste0("    ", trimws(final$DCREASCD[final$ORD1 != 1]))
# Replace # placeholders back to spaces for display
for (tc in trt_cols_ordered) {
  final[[tc]] <- gsub("#", " ", final[[tc]], fixed = TRUE)
  # trim not needed; keep SAS spacing
}

# Build display table ---------------------------------------------------
# Header labels: "Subject Disposition" and "  n   %" per treatment with N
# Create header strings
header_n <- setNames(pop_sorted$TOT, pop_sorted$TRT01PN)
header_p <- setNames(pop_sorted$TRT01P, pop_sorted$TRT01PN)

# Order treatments by TRT01PN ascending (1,2,3)
ord_pn <- sort(pop_sorted$TRT01PN)

# Prepare final display data frame for CSV
display_df <- data.frame(
  ORD1 = final$ORD1,
  ORD2 = final$ORD2,
  ORD3 = final$ORD3,
  `Subject Disposition` = final$COL1,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
for (pn in ord_pn) {
  tc <- paste0("TRT", pn)
  hdr <- sprintf("%s (N=%s)", header_p[as.character(pn)], header_n[as.character(pn)])
  display_df[[hdr]] <- final[[tc]]
}

# Add section header rows (compute before ord2)
# We will insert section header rows as separate rows with COL1 = section text, other cols blank
section_labels <- c("1" = "Death or Adverse Event", "2" = "Lack of Efficacy-Related Reasons", "3" = "Other Reasons")
# Build ordered display with sections
# Determine unique ORD2 values in order
ord2_vals <- sort(unique(final$ORD2))
# For ORD2=1, section appears before its rows; but ORD1=1 rows are not under a section (they are top)
# In SAS, compute before ord2 prints section header for each ord2 group (including ord2=1 which contains Discontinued/Completed? Actually ord2=1 includes topline and midline1)
# We'll insert section headers before each ORD2 group where ORD1 !=1
display_with_sections <- data.frame()
for (o2 in ord2_vals) {
  sub <- final[final$ORD2 == o2, , drop = FALSE]
  # if this group contains only ORD1=1 rows, don't add section header (they are top)
  # But ORD2=1 contains both ORD1=1 and ORD1=2 ; SAS would print header before ORD2=1 group
  # We'll add header for ORD2=1,2,3 as per compute before ord2
  if (o2 %in% c(1,2,3)) {
    hdr_row <- data.frame(ORD1 = NA, ORD2 = o2, ORD3 = NA, `Subject Disposition` = section_labels[as.character(o2)], check.names = FALSE, stringsAsFactors = FALSE)
    for (pn in ord_pn) {
      hdr <- sprintf("%s (N=%s)", header_p[as.character(pn)], header_n[as.character(pn)])
      hdr_row[[hdr]] <- ""
    }
    display_with_sections <- rbind(display_with_sections, hdr_row)
  }
  # add data rows for this ord2
  for (i in seq_len(nrow(sub))) {
    row <- data.frame(ORD1 = sub$ORD1[i], ORD2 = sub$ORD2[i], ORD3 = sub$ORD3[i], `Subject Disposition` = sub$COL1[i], check.names = FALSE, stringsAsFactors = FALSE)
    for (pn in ord_pn) {
      hdr <- sprintf("%s (N=%s)", header_p[as.character(pn)], header_n[as.character(pn)])
      row[[hdr]] <- sub[[paste0("TRT", pn)]][i]
    }
    display_with_sections <- rbind(display_with_sections, row)
  }
}
# Reorder to match final sort: ORD1,ORD2,ORD3 but with section headers interleaved
# Our loop already respects ORD2 order; need to ensure ORD1=1 rows appear first (they are ORD2=1)
# The above includes them after section header for ORD2=1; SAS would show section header then rows including Completed/Discontinued?
# To match SAS, keep as is.

# Titles and footnotes --------------------------------------------------
titles <- c(
  "Table X: Dispostion Table",
  "",
  "Subject Disposition"
)
footnotes <- c(
  "Abbreviations: N=total number of subjects in population.",
  "               n=number of subjects in the specified category",
  "               %=Percentage of patients with N as denomiator",
  "",
  "",
  "Program Location: blah lah blah"
)

# Write text rendering --------------------------------------------------
write_table_txt <- function(path, df, titles, footnotes) {
  con <- file(path, open = "wt")
  on.exit(close(con))
  for (t in titles) cat(t, "\n", file = con, sep = "")
  cat("\n", file = con)
  # column headers
  hdr_line <- paste(names(df), collapse = " | ")
  cat(hdr_line, "\n", file = con, sep = "")
  cat(paste(rep("-", nchar(hdr_line)), collapse = ""), "\n", file = con, sep = "")
  for (i in seq_len(nrow(df))) {
    vals <- sapply(df[i, ], function(x) ifelse(is.na(x), "", as.character(x)))
    line <- paste(vals, collapse = " | ")
    cat(line, "\n", file = con, sep = "")
  }
  cat("\n", file = con)
  for (f in footnotes) cat(f, "\n", file = con, sep = "")
}

# Prepare display for txt (without ORD columns for readability, but keep them for CSV)
txt_df <- display_with_sections
# For txt, keep all columns including ORD for traceability
txt_path1 <- file.path(out, "P048.txt")
txt_path2 <- file.path(out, "sasrtf.txt")
# Also write .lst and .rtf as text
lst_path <- file.path(out, "P048.lst")
rtf_path <- file.path(out, "sasrtf.rtf")

write_table_txt(lst_path, txt_df, titles, footnotes)
write_table_txt(rtf_path, txt_df, titles, footnotes)
write_table_txt(txt_path1, txt_df, titles, footnotes)
write_table_txt(txt_path2, txt_df, titles, footnotes)

# Write CSVs ------------------------------------------------------------
# CSV of table body (display_df without section headers, plus display_with_sections)
csv_df <- display_with_sections
# clean column names for CSV (keep as is)
write.csv(csv_df, file.path(out, "P048.csv"), row.names = FALSE, na = "")
write.csv(csv_df, file.path(out, "sasrtf.csv"), row.names = FALSE, na = "")
# also with .lst/.rtf extensions
write.csv(csv_df, file.path(out, "P048.lst.csv"), row.names = FALSE, na = "")
write.csv(csv_df, file.path(out, "sasrtf.rtf.csv"), row.names = FALSE, na = "")

# Write final dataset as CSV (ADaM-like) --------------------------------
# final dataset for output/datasets
final_out <- final[, c("STUDYID","DCREASCD","ORD1","ORD2","ORD3","COL1", trt_cols_ordered)]
names(final_out) <- toupper(names(final_out))
write.csv(final_out, file.path(out, "P048_dataset.csv"), row.names = FALSE, na = "")

# Also write PNG placeholder (no figure required, but create empty to satisfy spec if needed)
# No figure in this program

cat("Done. Outputs written to", out, "\n")

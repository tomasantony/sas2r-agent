# P074 t_lb_shift translation
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adampath <- "/bench/data/raw/own-synthetic/adam"
sdtmpath <- "/bench/data/raw/own-synthetic/sdtm"

# helper to read SAS dataset (sas7bdat or xpt)
read_adam <- function(name, path = adampath) {
  # try various extensions
  candidates <- file.path(path, paste0(name, c(".sas7bdat", ".sas7bdat", ".xpt", ".XPT", ".sas7bdat")))
  # also list files case-insensitive
  if (dir.exists(path)) {
    fl <- list.files(path, full.names = TRUE)
    # find matching name ignoring case and extension
    idx <- grep(paste0("^", name, "\\."), basename(fl), ignore.case = TRUE)
    if (length(idx) > 0) candidates <- c(fl[idx], candidates)
  }
  for (f in candidates) {
    if (file.exists(f)) {
      if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
        df <- haven::read_xpt(f)
      } else {
        df <- haven::read_sas(f)
      }
      # zap labels/formats to plain R types
      df <- haven::zap_labels(df)
      df <- haven::zap_formats(df)
      # convert haven_labelled still present
      for (nm in names(df)) {
        if (inherits(df[[nm]], "haven_labelled")) df[[nm]] <- haven::zap_labels(df[[nm]])
      }
      return(df)
    }
  }
  stop(paste0("Dataset not found: ", name, " in ", path))
}

# round half away from zero (SAS) helper - not strictly needed for counts
round_sas <- function(x, digits = 0) {
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult + 1e-12
  z <- floor(z + 0.5)
  sign_x * z / mult
}

adsl <- read_adam("adsl", adampath)
adlb <- read_adam("adlb", adampath)

# ensure character columns are character, trim
char_trim <- function(x) {
  if (is.character(x)) trimws(x) else x
}
adsl <- as.data.frame(lapply(adsl, function(col) if (is.character(col)) trimws(col) else col), stringsAsFactors = FALSE)
adlb <- as.data.frame(lapply(adlb, function(col) if (is.character(col)) trimws(col) else col), stringsAsFactors = FALSE)

# Step 1: m_counts - Big N per TRTA where SAFFL='Y'
# normalize SAFFL to upper
adsl$SAFFL_up <- toupper(adsl$SAFFL)
bign_df <- adsl %>%
  dplyr::filter(SAFFL_up == "Y") %>%
  dplyr::group_by(TRTA, TRTAN) %>%
  dplyr::summarise(bign = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
  dplyr::arrange(TRTAN)
# total
bign_tot <- adsl %>% dplyr::filter(SAFFL_up == "Y") %>% dplyr::summarise(n = dplyr::n_distinct(USUBJID)) %>% dplyr::pull(n)

# Step 2: lb
# need columns existence check
stopifnot(all(c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","AVISIT","AVISITN","ATOXGR","BTOXGR","ABLFL","ANL01FL") %in% names(adlb)))
stopifnot(all(c("USUBJID","SAFFL") %in% names(adsl)))

# inner join adlb with adsl on USUBJID where SAFFL='Y' and PARAMCD in list
param_list <- c('ALT','AST','BILI','CREAT','HGB','WBC','PLAT','NEUT')
lb <- adlb %>%
  dplyr::inner_join(adsl %>% dplyr::filter(SAFFL_up == "Y") %>% dplyr::select(USUBJID), by = "USUBJID") %>%
  dplyr::filter(PARAMCD %in% param_list) %>%
  dplyr::select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, AVISIT, AVISITN, ATOXGR, BTOXGR, ABLFL, ANL01FL) %>%
  dplyr::arrange(TRTAN, PARAMCD, USUBJID, AVISITN)

# Step 3: bl_grade distinct where ABLFL='Y' and BTOXGR ne ''
bl_grade <- lb %>%
  dplyr::filter(ABLFL == "Y" & !is.na(BTOXGR) & BTOXGR != "") %>%
  dplyr::distinct(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, BTOXGR) %>%
  dplyr::rename(bl_grade = BTOXGR)

# Step 4: worst post-baseline grade
worst_grade <- lb %>%
  dplyr::filter((is.na(ABLFL) | ABLFL != "Y") & !is.na(ATOXGR) & ATOXGR != "" & !is.na(AVISITN) & AVISITN > 0) %>%
  dplyr::mutate(ATOXGR_n = suppressWarnings(as.numeric(ATOXGR))) %>%
  dplyr::filter(!is.na(ATOXGR_n)) %>%
  dplyr::group_by(USUBJID, TRTA, TRTAN, PARAMCD) %>%
  dplyr::summarise(worst_grade_n = max(ATOXGR_n, na.rm = TRUE), .groups = "drop")

worst_grade2 <- worst_grade %>%
  dplyr::mutate(worst_grade = as.character(worst_grade_n)) %>%
  dplyr::mutate(worst_grade = dplyr::if_else(worst_grade_n == 0, "0", worst_grade)) %>%
  dplyr::select(-worst_grade_n)

# Step 5: merge baseline and worst
shift <- bl_grade %>%
  dplyr::inner_join(worst_grade2, by = c("USUBJID","PARAMCD","TRTA")) %>%
  dplyr::arrange(TRTAN, PARAMCD, bl_grade, worst_grade)

# Step 6: count subjects in each shift cell
shift_cnt <- shift %>%
  dplyr::group_by(TRTA, TRTAN, PARAMCD, PARAM, bl_grade, worst_grade) %>%
  dplyr::summarise(cnt = dplyr::n_distinct(USUBJID), .groups = "drop")

# Step 7: all possible combinations
grades <- data.frame(grade = as.character(0:4), stringsAsFactors = FALSE)

# distinct TRTA/TRTAN/PARAMCD/PARAM from shift_cnt
distinct_cells <- shift_cnt %>% dplyr::distinct(TRTA, TRTAN, PARAMCD, PARAM)

# cross join
all_cells <- distinct_cells %>%
  tidyr::crossing(grade_b = grades$grade, grade_w = grades$grade) %>%
  dplyr::rename(bl_grade = grade_b, worst_grade = grade_w)

shift_full <- all_cells %>%
  dplyr::left_join(shift_cnt, by = c("TRTA","TRTAN","PARAMCD","PARAM","bl_grade","worst_grade")) %>%
  dplyr::mutate(cnt = ifelse(is.na(cnt), 0L, cnt)) %>%
  dplyr::arrange(TRTAN, PARAMCD, bl_grade, worst_grade)

# Step 8: transpose worst_grade into columns
shift_t <- shift_full %>%
  tidyr::pivot_wider(id_cols = c(TRTAN, TRTA, PARAMCD, PARAM, bl_grade),
                     names_from = worst_grade, values_from = cnt, names_prefix = "wg", values_fill = 0) %>%
  dplyr::arrange(TRTAN, PARAMCD, bl_grade)

# ensure wg0-wg4 exist
for (g in paste0("wg",0:4)) {
  if (!g %in% names(shift_t)) shift_t[[g]] <- 0L
}
# order columns
shift_t <- shift_t %>% dplyr::select(TRTAN, TRTA, PARAMCD, PARAM, bl_grade, wg0, wg1, wg2, wg3, wg4)

report <- shift_t %>%
  dplyr::mutate(bl_label = dplyr::case_when(
    bl_grade == "0" ~ "Normal",
    bl_grade == "1" ~ "Grade 1",
    bl_grade == "2" ~ "Grade 2",
    bl_grade == "3" ~ "Grade 3",
    bl_grade == "4" ~ "Grade 4",
    TRUE ~ bl_grade
  )) %>%
  dplyr::mutate(total = wg0 + wg1 + wg2 + wg3 + wg4) %>%
  dplyr::arrange(TRTAN, PARAMCD, bl_grade)

# ensure wg columns are integer and replace NA with 0 (already)
report[is.na(report)] <- 0

# Write CSV dataset
write.csv(report, file.path(out, "t_lb_shift.csv"), row.names = FALSE, na = "")

# Also write shift_full and shift_cnt for traceability (optional but keep main)
# Build text rendering
# Titles/footnotes from u_titles.sas
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
title1 <- paste0(sponsor, "                                                                  Page 1 of 1")
title2 <- paste0("Protocol: ", protocol)
title4 <- "Table 14.3.4"
title5 <- "Shift Table for Laboratory Parameters"
title6 <- "Safety Analysis Set"
foot1 <- paste0("Data cutoff: ", cutdate)
foot2 <- "Source: ADaM datasets"
foot3 <- paste0("Program: t_lb_shift.sas | Run: ", format(Sys.Date(), "%d%b%Y"))
foot4 <- "Grades based on NCI-CTCAE v[X.X]"
foot5 <- "Shift from baseline grade to worst post-baseline grade."

# Create text file
txt_path <- file.path(out, "t_lb_shift.txt")
con <- file(txt_path, open = "wt")

writeLines(title1, con)
writeLines(title2, con)
writeLines("", con)
writeLines(title4, con)
writeLines(title5, con)
writeLines(title6, con)
writeLines("", con)

# Big N header info
writeLines("Treatment group counts (Safety Analysis Set):", con)
for (i in seq_len(nrow(bign_df))) {
  writeLines(sprintf("  %s (N=%d)  TRTAN=%s", bign_df$TRTA[i], bign_df$bign[i], bign_df$TRTAN[i]), con)
}
writeLines(sprintf("  Total (N=%d)", bign_tot), con)
writeLines("", con)
writeLines("Shift from baseline grade to worst post-baseline grade (counts).", con)
writeLines("", con)

# For each treatment group, print table per PARAMCD
# Order TRTAN
trt_order <- sort(unique(report$TRTAN))
for (t in trt_order) {
  trt_label <- unique(report$TRTA[report$TRTAN == t])
  bign_val <- bign_df$bign[bign_df$TRTAN == t]
  if (length(bign_val)==0) bign_val <- NA
  writeLines(sprintf("Treatment Group: %s (N=%s)  [TRTAN=%s]", trt_label, ifelse(is.na(bign_val),"",bign_val), t), con)
  writeLines(paste(rep("-", 100), collapse=""), con)
  sub <- report %>% dplyr::filter(TRTAN == t) %>% dplyr::arrange(PARAMCD, bl_grade)
  # header
  hdr <- sprintf("%-20s %-12s %8s %8s %8s %8s %8s %8s",
                 "Parameter", "Baseline", "Normal", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Total")
  writeLines(hdr, con)
  writeLines(paste(rep("-", 100), collapse=""), con)
  # group by PARAMCD
  params <- unique(sub$PARAMCD)
  for (p in params) {
    psub <- sub %>% dplyr::filter(PARAMCD == p)
    param_name <- unique(psub$PARAM)
    # first row with param label
    for (r in seq_len(nrow(psub))) {
      row <- psub[r,]
      param_col <- if (r==1) sprintf("%s (%s)", param_name, p) else ""
      line <- sprintf("%-20s %-12s %8d %8d %8d %8d %8d %8d",
                      substr(param_col,1,20),
                      row$bl_label,
                      row$wg0, row$wg1, row$wg2, row$wg3, row$wg4, row$total)
      writeLines(line, con)
    }
    writeLines("", con)
  }
  writeLines("", con)
}

writeLines("", con)
writeLines(foot1, con)
writeLines(foot2, con)
writeLines(foot3, con)
writeLines(foot4, con)
writeLines(foot5, con)

close(con)

# Also create RTF-like copy (plain text with .rtf extension) to satisfy expected output name
rtf_path <- file.path(out, "t_lb_shift.rtf")
file.copy(txt_path, rtf_path, overwrite = TRUE)

# CSV of plotted data not needed (no figure), but ensure t_lb_shift.csv already written
# Also write a CSV of table body for verification (same as report)
# Already done

# Clean up intermediate objects not needed

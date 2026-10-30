```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)

adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

read_adam <- function(name, path = adampath) {
  candidates <- file.path(path, paste0(name, c(".sas7bdat", ".xpt", ".XPT")))
  if (dir.exists(path)) {
    fl <- list.files(path, full.names = TRUE)
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
      df <- haven::zap_labels(df)
      df <- haven::zap_formats(df)
      for (nm in names(df)) {
        if (inherits(df[[nm]], "haven_labelled")) df[[nm]] <- haven::zap_labels(df[[nm]])
      }
      df <- as.data.frame(df, stringsAsFactors = FALSE)
      return(df)
    }
  }
  stop(paste0("Dataset not found: ", name, " in ", path))
}

adsl <- read_adam("adsl", adampath)
adlb <- read_adam("adlb", adampath)

adsl <- as.data.frame(lapply(adsl, function(col) if (is.character(col)) trimws(col) else col), stringsAsFactors = FALSE)
adlb <- as.data.frame(lapply(adlb, function(col) if (is.character(col)) trimws(col) else col), stringsAsFactors = FALSE)

# Ensure TRTAN exists - derive from TRTA if missing (SAS program expects TRTAN)
ensure_trtan <- function(df) {
  if ("TRTAN" %in% names(df)) {
    df$TRTAN <- suppressWarnings(as.numeric(df$TRTAN))
    return(df)
  }
  if (!"TRTA" %in% names(df)) return(df)
  trta_vals <- unique(df$TRTA)
  trta_vals <- trta_vals[!is.na(trta_vals)]
  map <- tibble(TRTA = trta_vals)
  map$TRTAN <- sapply(map$TRTA, function(x) {
    xl <- tolower(x)
    if (grepl("placebo", xl)) return(1)
    if (grepl("low", xl)) return(2)
    if (grepl("high", xl)) return(3)
    if (grepl("total", xl)) return(99)
    return(NA_real_)
  })
  # fill NAs sequentially
  if (any(is.na(map$TRTAN))) {
    maxv <- suppressWarnings(max(map$TRTAN, na.rm = TRUE))
    if (!is.finite(maxv)) maxv <- 0
    for (i in which(is.na(map$TRTAN))) {
      maxv <- maxv + 1
      map$TRTAN[i] <- maxv
    }
  }
  # if still no mapping (e.g. numeric TRTA), assign by sorted order
  if (nrow(map)==0) return(df)
  df <- left_join(df, map, by = "TRTA")
  df
}

adsl <- ensure_trtan(adsl)
adlb <- ensure_trtan(adlb)

# If adlb still missing TRTAN but adsl has mapping, join via USUBJID
if (!"TRTAN" %in% names(adlb) && "TRTAN" %in% names(adsl)) {
  subj_trt <- adsl %>% distinct(USUBJID, TRTA, TRTAN)
  adlb <- left_join(adlb, subj_trt, by = c("USUBJID","TRTA"))
  # if still NA try by USUBJID only
  if (any(is.na(adlb$TRTAN))) {
    subj_trt2 <- adsl %>% distinct(USUBJID, TRTAN)
    adlb$TRTAN <- NULL
    adlb <- left_join(adlb, subj_trt2, by = "USUBJID")
  }
}
# Ensure TRTA exists in adlb if only TRTAN
if (!"TRTA" %in% names(adlb) && "TRTA" %in% names(adsl)) {
  subj_trt <- adsl %>% distinct(USUBJID, TRTA, TRTAN)
  adlb <- left_join(adlb, subj_trt, by = c("USUBJID","TRTAN"))
}

# Coerce types
if ("TRTAN" %in% names(adsl)) adsl$TRTAN <- suppressWarnings(as.numeric(adsl$TRTAN))
if ("TRTAN" %in% names(adlb)) adlb$TRTAN <- suppressWarnings(as.numeric(adlb$TRTAN))
if ("AVISITN" %in% names(adlb)) adlb$AVISITN <- suppressWarnings(as.numeric(adlb$AVISITN))

# Step 1: m_counts - Big N per TRTA where SAFFL='Y'
adsl$SAFFL_up <- toupper(adsl$SAFFL)
bign_df <- adsl %>%
  filter(SAFFL_up == "Y") %>%
  group_by(TRTA, TRTAN) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN)
bign_tot <- adsl %>% filter(SAFFL_up == "Y") %>% summarise(n = n_distinct(USUBJID)) %>% pull(n)
if (length(bign_tot)==0 || is.na(bign_tot)) bign_tot <- 0

# Step 2: lb
param_list <- c('ALT','AST','BILI','CREAT','HGB','WBC','PLAT','NEUT')
# keep only available columns
needed <- c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","AVISIT","AVISITN","ATOXGR","BTOXGR","ABLFL","ANL01FL")
for (cc in needed) if (!cc %in% names(adlb)) adlb[[cc]] <- NA_character_
# ensure TRTAN numeric for join
lb <- adlb %>%
  inner_join(adsl %>% filter(SAFFL_up == "Y") %>% select(USUBJID), by = "USUBJID") %>%
  filter(PARAMCD %in% param_list) %>%
  select(all_of(needed)) %>%
  arrange(TRTAN, PARAMCD, USUBJID, AVISITN)

# Step 3: bl_grade distinct where ABLFL='Y' and BTOXGR ne ''
bl_grade <- lb %>%
  filter(ABLFL == "Y" & !is.na(BTOXGR) & BTOXGR != "") %>%
  distinct(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, BTOXGR) %>%
  rename(bl_grade = BTOXGR)

# Step 4: worst post-baseline grade
worst_grade <- lb %>%
  filter((is.na(ABLFL) | ABLFL != "Y") & !is.na(ATOXGR) & ATOXGR != "" & !is.na(AVISITN) & AVISITN > 0) %>%
  mutate(ATOXGR_n = suppressWarnings(as.numeric(ATOXGR))) %>%
  filter(!is.na(ATOXGR_n)) %>%
  group_by(USUBJID, TRTA, TRTAN, PARAMCD) %>%
  summarise(worst_grade_n = max(ATOXGR_n, na.rm = TRUE), .groups = "drop")

if (nrow(worst_grade)==0) {
  worst_grade2 <- tibble(USUBJID=character(), TRTA=character(), TRTAN=numeric(), PARAMCD=character(), worst_grade=character())
} else {
  worst_grade2 <- worst_grade %>%
    mutate(worst_grade = as.character(worst_grade_n)) %>%
    mutate(worst_grade = if_else(worst_grade_n == 0, "0", worst_grade)) %>%
    select(-worst_grade_n)
}

# Step 5: merge baseline and worst post-baseline
if (nrow(bl_grade)==0 || nrow(worst_grade2)==0) {
  shift <- tibble(USUBJID=character(), TRTA=character(), TRTAN=numeric(), PARAMCD=character(), PARAM=character(), bl_grade=character(), worst_grade=character())
} else {
  shift <- bl_grade %>%
    inner_join(worst_grade2, by = c("USUBJID","PARAMCD","TRTA")) %>%
    arrange(TRTAN, PARAMCD, bl_grade, worst_grade)
  # if TRTAN.x / TRTAN.y duplication, coalesce
  if ("TRTAN.x" %in% names(shift)) {
    shift$TRTAN <- coalesce(shift$TRTAN.x, shift$TRTAN.y)
    shift$TRTAN.x <- NULL; shift$TRTAN.y <- NULL
  }
}

# Step 6: count subjects in each shift cell
if (nrow(shift)==0) {
  shift_cnt <- tibble(TRTA=character(), TRTAN=numeric(), PARAMCD=character(), PARAM=character(), bl_grade=character(), worst_grade=character(), cnt=integer())
} else {
  shift_cnt <- shift %>%
    group_by(TRTA, TRTAN, PARAMCD, PARAM, bl_grade, worst_grade) %>%
    summarise(cnt = n_distinct(USUBJID), .groups = "drop")
}

# Step 7: all possible combinations
grades <- data.frame(grade = as.character(0:4), stringsAsFactors = FALSE)
if (nrow(shift_cnt)==0) {
  # create empty structures but still allow report creation from distinct params in lb
  distinct_cells <- lb %>% distinct(TRTA, TRTAN, PARAMCD, PARAM) %>% filter(!is.na(TRTA))
  if (nrow(distinct_cells)==0) distinct_cells <- tibble(TRTA=character(), TRTAN=numeric(), PARAMCD=character(), PARAM=character())
} else {
  distinct_cells <- shift_cnt %>% distinct(TRTA, TRTAN, PARAMCD, PARAM)
}

if (nrow(distinct_cells)==0) {
  all_cells <- tibble(TRTA=character(), TRTAN=numeric(), PARAMCD=character(), PARAM=character(), bl_grade=character(), worst_grade=character())
} else {
  all_cells <- distinct_cells %>%
    crossing(grade_b = grades$grade, grade_w = grades$grade) %>%
    rename(bl_grade = grade_b, worst_grade = grade_w)
}

if (nrow(all_cells)==0) {
  shift_full <- tibble(TRTA=character(), TRTAN=numeric(), PARAMCD=character(), PARAM=character(), bl_grade=character(), worst_grade=character(), cnt=integer())
} else {
  shift_full <- all_cells %>%
    left_join(shift_cnt, by = c("TRTA","TRTAN","PARAMCD","PARAM","bl_grade","worst_grade")) %>%
    mutate(cnt = ifelse(is.na(cnt), 0L, cnt)) %>%
    arrange(TRTAN, PARAMCD, bl_grade, worst_grade)
}

# Step 8: transpose worst_grade into columns
if (nrow(shift_full)==0) {
  shift_t <- tibble(TRTAN=numeric(), TRTA=character(), PARAMCD=character(), PARAM=character(), bl_grade=character(), wg0=integer(), wg1=integer(), wg2=integer(), wg3=integer(), wg4=integer())
} else {
  shift_t <- shift_full %>%
    pivot_wider(id_cols = c(TRTAN, TRTA, PARAMCD, PARAM, bl_grade),
                names_from = worst_grade, values_from = cnt, names_prefix = "wg", values_fill = 0) %>%
    arrange(TRTAN, PARAMCD, bl_grade)
}

for (g in paste0("wg",0:4)) {
  if (!g %in% names(shift_t)) shift_t[[g]] <- 0L
}
# ensure numeric
for (g in paste0("wg",0:4)) shift_t[[g]] <- as.integer(shift_t[[g]])

if (nrow(shift_t)>0) {
  shift_t <- shift_t %>% select(TRTAN, TRTA, PARAMCD, PARAM, bl_grade, wg0, wg1, wg2, wg3, wg4)
  report <- shift_t %>%
    mutate(bl_label = case_when(
      bl_grade == "0" ~ "Normal",
      bl_grade == "1" ~ "Grade 1",
      bl_grade == "2" ~ "Grade 2",
      bl_grade == "3" ~ "Grade 3",
      bl_grade == "4" ~ "Grade 4",
      TRUE ~ bl_grade
    )) %>%
    mutate(total = wg0 + wg1 + wg2 + wg3 + wg4) %>%
    arrange(TRTAN, PARAMCD, bl_grade)
} else {
  report <- tibble(TRTAN=numeric(), TRTA=character(), PARAMCD=character(), PARAM=character(), bl_grade=character(), wg0=integer(), wg1=integer(), wg2=integer(), wg3=integer(), wg4=integer(), bl_label=character(), total=integer())
}

report[is.na(report)] <- 0

# Write CSV dataset
write.csv(report, file.path(out, "t_lb_shift.csv"), row.names = FALSE, na = "")

# Build text rendering
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

txt_path <- file.path(out, "t_lb_shift.txt")
con <- file(txt_path, open = "wt")

writeLines(title1, con)
writeLines(title2, con)
writeLines("", con)
writeLines(title4, con)
writeLines(title5, con)
writeLines(title6, con)
writeLines("", con)

writeLines("Treatment group counts (Safety Analysis Set):", con)
for (i in seq_len(nrow(bign_df))) {
  writeLines(sprintf("  %s (N=%d)  TRTAN=%s", bign_df$TRTA[i], bign_df$bign[i], bign_df$TRTAN[i]), con)
}
writeLines(sprintf("  Total (N=%d)", bign_tot), con)
writeLines("", con)
writeLines("Shift from baseline grade to worst post-baseline grade (counts).", con)
writeLines("", con)

if (nrow(report)>0) {
  trt_order <- sort(unique(report$TRTAN))
  for (t in trt_order) {
    trt_label <- unique(report$TRTA[report$TRTAN == t])
    bign_val <- bign_df$bign[bign_df$TRTAN == t]
    if (length(bign_val)==0) bign_val <- NA
    writeLines(sprintf("Treatment Group: %s (N=%s)  [TRTAN=%s]", trt_label, ifelse(is.na(bign_val),"",bign_val), t), con)
    writeLines(paste(rep("-", 100), collapse=""), con)
    sub <- report %>% filter(TRTAN == t) %>% arrange(PARAMCD, bl_grade)
    hdr <- sprintf("%-20s %-12s %8s %8s %8s %8s %8s %8s",
                   "Parameter", "Baseline", "Normal", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Total")
    writeLines(hdr, con)
    writeLines(paste(rep("-", 100), collapse=""), con)
    params <- unique(sub$PARAMCD)
    for (p in params) {
      psub <- sub %>% filter(PARAMCD == p)
      param_name <- unique(psub$PARAM)
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
} else {
  writeLines("No data for shift table.", con)
}

writeLines("", con)
writeLines(foot1, con)
writeLines(foot2, con)
writeLines(foot3, con)
writeLines(foot4, con)
writeLines(foot5, con)

close(con)

rtf_path <- file.path(out, "t_lb_shift.rtf")
file.copy(txt_path, rtf_path, overwrite = TRUE)
```
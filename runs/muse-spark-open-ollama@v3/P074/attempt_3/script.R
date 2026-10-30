suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
  library(data.table)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

read_adam <- function(libpath, dsname) {
  dsname_low <- tolower(dsname)
  files <- list.files(libpath, full.names = TRUE, recursive = FALSE)
  idx <- which(tolower(tools::file_path_sans_ext(basename(files))) == dsname_low)
  if (length(idx) == 0) {
    idx <- grep(paste0("^", dsname_low, "\\."), tolower(basename(files)))
  }
  if (length(idx) == 0) stop(paste0("Dataset not found: ", dsname, " in ", libpath))
  f <- files[idx[1]]
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else {
    df <- tryCatch(haven::read_sas(f), error = function(e) haven::read_xpt(f))
  }
  df <- haven::zap_formats(df)
  df <- haven::zap_label(df)
  df <- df %>% mutate(across(where(haven::is.labelled), haven::zap_labels))
  df <- df %>% mutate(across(where(is.factor), as.character))
  names(df) <- toupper(names(df))
  df
}

derive_trtan <- function(trta_vec) {
  sapply(trta_vec, function(x) {
    if (is.na(x)) return(NA_real_)
    xl <- tolower(x)
    if (grepl("placebo", xl)) return(1)
    if (grepl("low", xl)) return(2)
    if (grepl("high", xl)) return(3)
    if (grepl("total", xl)) return(99)
    return(NA_real_)
  })
}

ensure_trtan <- function(df) {
  if (!"TRTAN" %in% names(df)) {
    if ("TRTA" %in% names(df)) {
      df$TRTAN <- derive_trtan(df$TRTA)
      if (any(is.na(df$TRTAN))) {
        uniq <- sort(unique(df$TRTA))
        map2 <- setNames(seq_along(uniq), uniq)
        miss <- is.na(df$TRTAN)
        df$TRTAN[miss] <- map2[df$TRTA[miss]]
      }
      df$TRTAN <- as.numeric(df$TRTAN)
    } else {
      df$TRTAN <- 1
    }
  } else {
    df$TRTAN <- as.numeric(df$TRTAN)
    if (all(is.na(df$TRTAN)) && "TRTA" %in% names(df)) {
      df$TRTAN <- derive_trtan(df$TRTA)
    }
  }
  df
}

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"
if (!dir.exists(adam_path)) adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
if (!dir.exists(sdtm_path)) sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"

adsl <- read_adam(adam_path, "adsl")
adlb <- read_adam(adam_path, "adlb")

adsl <- adsl %>% mutate(across(where(is.character), ~str_trim(.)))
adlb <- adlb %>% mutate(across(where(is.character), ~str_trim(.)))

adsl <- ensure_trtan(adsl)
adlb <- ensure_trtan(adlb)

if ("AVISITN" %in% names(adlb)) adlb$AVISITN <- as.numeric(adlb$AVISITN)
if ("TRTAN" %in% names(adsl)) adsl$TRTAN <- as.numeric(adsl$TRTAN)
if ("TRTAN" %in% names(adlb)) adlb$TRTAN <- as.numeric(adlb$TRTAN)

# Step 1: Get Big N
adsl_saffl <- adsl %>% filter(toupper(SAFFL) == "Y")
bign_df <- adsl_saffl %>%
  group_by(TRTA, TRTAN) %>%
  summarise(BIGN = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN)
bign_tot <- n_distinct(adsl_saffl$USUBJID)
ntrt <- nrow(bign_df)
if (ntrt == 0) ntrt <- 3

param_list <- c('ALT', 'AST', 'BILI', 'CREAT', 'HGB', 'WBC', 'PLAT', 'NEUT')

saffl_subj <- adsl_saffl %>% select(USUBJID) %>% distinct()

if (!"PARAM" %in% names(adlb)) adlb$PARAM <- adlb$PARAMCD
if (!"ABLFL" %in% names(adlb)) adlb$ABLFL <- ""
if (!"ATOXGR" %in% names(adlb)) adlb$ATOXGR <- ""
if (!"BTOXGR" %in% names(adlb)) adlb$BTOXGR <- ""
if (!"AVISITN" %in% names(adlb)) adlb$AVISITN <- 1
if (!"AVISIT" %in% names(adlb)) adlb$AVISIT <- ""
if (!"ANL01FL" %in% names(adlb)) adlb$ANL01FL <- ""

lb <- adlb %>%
  inner_join(saffl_subj, by = "USUBJID") %>%
  filter(PARAMCD %in% param_list) %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, AVISIT, AVISITN, ATOXGR, BTOXGR, ABLFL, ANL01FL) %>%
  arrange(TRTAN, PARAMCD, USUBJID, AVISITN)

lb <- lb %>%
  mutate(
    ATOXGR = as.character(ATOXGR),
    BTOXGR = as.character(BTOXGR),
    ABLFL = as.character(ABLFL),
    ATOXGR = ifelse(is.na(ATOXGR), "", str_trim(ATOXGR)),
    BTOXGR = ifelse(is.na(BTOXGR), "", str_trim(BTOXGR)),
    ABLFL = ifelse(is.na(ABLFL), "", str_trim(ABLFL))
  )

# Step 3: baseline grade
bl_grade <- lb %>%
  filter(ABLFL == "Y", BTOXGR != "") %>%
  distinct(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, BTOXGR) %>%
  rename(BL_GRADE = BTOXGR)

# Step 4: worst post-baseline
worst_grade <- lb %>%
  filter(ABLFL != "Y", ATOXGR != "", !is.na(AVISITN), AVISITN > 0) %>%
  mutate(ATOXGR_N = suppressWarnings(as.numeric(ATOXGR))) %>%
  filter(!is.na(ATOXGR_N)) %>%
  group_by(USUBJID, TRTA, TRTAN, PARAMCD) %>%
  summarise(WORST_GRADE_N = max(ATOXGR_N, na.rm = TRUE), .groups = "drop")

worst_grade2 <- worst_grade %>%
  mutate(WORST_GRADE = as.character(WORST_GRADE_N)) %>%
  mutate(WORST_GRADE = ifelse(WORST_GRADE_N == 0, "0", WORST_GRADE)) %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, WORST_GRADE)

# Step 5: Merge baseline and worst - avoid TRTAN duplication by selecting only needed cols from worst
# SAS: inner join on USUBJID, PARAMCD, TRTA
worst_for_join <- worst_grade2 %>% select(USUBJID, PARAMCD, TRTA, WORST_GRADE)
shift <- bl_grade %>%
  inner_join(worst_for_join, by = c("USUBJID", "PARAMCD", "TRTA")) %>%
  arrange(TRTAN, PARAMCD, BL_GRADE, WORST_GRADE)

# Step 6: Count
shift_cnt <- shift %>%
  group_by(TRTA, TRTAN, PARAMCD, PARAM, BL_GRADE, WORST_GRADE) %>%
  summarise(CNT = n_distinct(USUBJID), .groups = "drop")

# Step 7: all cells - cross join for all bl_grade x worst_grade per TRTA/PARAMCD
grades <- tibble(GRADE = as.character(0:4))
distinct_cells <- shift_cnt %>% distinct(TRTA, TRTAN, PARAMCD, PARAM)

# If shift_cnt is empty (no shifts), fallback to create cells from bign_df and param_list
if (nrow(distinct_cells) == 0) {
  # create distinct cells from bign_df and available PARAMs in lb
  param_info <- lb %>% distinct(PARAMCD, PARAM)
  if (nrow(param_info) == 0) {
    param_info <- tibble(PARAMCD = param_list, PARAM = param_list)
  }
  distinct_cells <- bign_df %>% select(TRTA, TRTAN) %>%
    cross_join(param_info)
}

all_cells <- distinct_cells %>%
  cross_join(grades %>% rename(BL_GRADE = GRADE)) %>%
  cross_join(grades %>% rename(WORST_GRADE = GRADE))

# Merge actual counts - avoid TRTAN duplication: select only join keys + CNT from shift_cnt
shift_cnt_join <- shift_cnt %>% select(TRTA, PARAMCD, BL_GRADE, WORST_GRADE, CNT)

shift_full <- all_cells %>%
  left_join(shift_cnt_join, by = c("TRTA", "PARAMCD", "BL_GRADE", "WORST_GRADE")) %>%
  mutate(CNT = coalesce(CNT, 0L)) %>%
  arrange(TRTAN, PARAMCD, BL_GRADE, WORST_GRADE)

# Step 8: Transpose
shift_t <- shift_full %>%
  select(TRTAN, TRTA, PARAMCD, PARAM, BL_GRADE, WORST_GRADE, CNT) %>%
  pivot_wider(names_from = WORST_GRADE, values_from = CNT, names_prefix = "WG", values_fill = 0)

for (g in 0:4) {
  col <- paste0("WG", g)
  if (!col %in% names(shift_t)) shift_t[[col]] <- 0L
}
shift_t <- shift_t %>% select(TRTAN, TRTA, PARAMCD, PARAM, BL_GRADE, WG0, WG1, WG2, WG3, WG4)

report <- shift_t %>%
  mutate(
    BL_LABEL = case_when(
      BL_GRADE == "0" ~ "Normal",
      BL_GRADE == "1" ~ "Grade 1",
      BL_GRADE == "2" ~ "Grade 2",
      BL_GRADE == "3" ~ "Grade 3",
      BL_GRADE == "4" ~ "Grade 4",
      TRUE ~ BL_GRADE
    )
  ) %>%
  mutate(across(starts_with("WG"), ~coalesce(.x, 0L))) %>%
  mutate(TOTAL = WG0 + WG1 + WG2 + WG3 + WG4) %>%
  mutate(BL_GRADE_N = suppressWarnings(as.numeric(BL_GRADE))) %>%
  arrange(TRTAN, PARAMCD, BL_GRADE_N) %>%
  select(-BL_GRADE_N)

report_out <- report %>%
  select(TRTAN, TRTA, PARAMCD, PARAM, BL_GRADE, BL_LABEL, WG0, WG1, WG2, WG3, WG4, TOTAL)

write.csv(report_out, file.path(out, "t_lb_shift.csv"), row.names = FALSE, na = "")

# Text rendering
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"

title1 <- paste0(sponsor, "  Page 1 of N")
title2 <- paste0("Protocol: ", protocol)
title4 <- "Table 14.3.4"
title5 <- "Shift Table for Laboratory Parameters"
title6 <- "Safety Analysis Set"

bign_map <- bign_df %>% arrange(TRTAN)

txt_path <- file.path(out, "t_lb_shift.txt")
con <- file(txt_path, open = "wt")

writeLines(title1, con)
writeLines(title2, con)
writeLines("", con)
writeLines(title4, con)
writeLines(title5, con)
writeLines(title6, con)
writeLines("", con)
writeLines(paste0("Data cutoff: ", cutdate), con)
writeLines("Source: ADaM datasets", con)
writeLines("", con)

for (t in sort(unique(report_out$TRTAN))) {
  trta_label <- bign_map$TRTA[bign_map$TRTAN == t]
  if (length(trta_label) == 0) trta_label <- paste0("TRTA=", t)
  bign_val <- bign_map$BIGN[bign_map$TRTAN == t]
  if (length(bign_val) == 0) bign_val <- NA
  writeLines(paste0("Treatment Group: ", trta_label, " (N=", bign_val, ")"), con)
  writeLines(strrep("-", 100), con)
  hdr <- sprintf("%-25s %-12s %8s %8s %8s %8s %8s %8s",
                 "Parameter", "Baseline Grade", "Normal", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Total")
  writeLines(hdr, con)
  writeLines(strrep("-", 100), con)
  sub <- report_out %>% filter(TRTAN == t) %>% arrange(PARAMCD, suppressWarnings(as.numeric(BL_GRADE)))
  params <- unique(sub$PARAMCD)
  for (p in params) {
    psub <- sub %>% filter(PARAMCD == p)
    for (r in seq_len(nrow(psub))) {
      row <- psub[r, ]
      param_disp <- if (r == 1) paste0(row$PARAMCD, " (", row$PARAM, ")") else ""
      line <- sprintf("%-25s %-12s %8d %8d %8d %8d %8d %8d",
                      substr(param_disp, 1, 25),
                      row$BL_LABEL,
                      row$WG0, row$WG1, row$WG2, row$WG3, row$WG4, row$TOTAL)
      writeLines(line, con)
    }
    writeLines("", con)
  }
  writeLines("", con)
}

writeLines("Grades based on NCI-CTCAE v[X.X]", con)
writeLines("Shift from baseline grade to worst post-baseline grade.", con)
writeLines("", con)
writeLines(paste0("Footnotes: Data cutoff: ", cutdate, " | Source: ADaM datasets"), con)
close(con)

rtf_path <- file.path(out, "t_lb_shift.rtf")
rtf_header <- "{\\rtf1\\ansi\\deff0 {\\fonttbl {\\f0 Courier New;}}\n\\viewkind4\\uc1\\pard\\f0\\fs18\n"
rtf_footer <- "}"
txt_content <- readLines(txt_path, warn = FALSE)
escape_rtf <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\\{", "\\\\{", x)
  x <- gsub("\\}", "\\\\}", x)
  x
}
rtf_body <- paste(escape_rtf(txt_content), collapse = "\\par\n")
writeLines(paste0(rtf_header, rtf_body, "\n", rtf_footer), rtf_path)

message("Report rows: ", nrow(report_out))
print(bign_df)
message("Total N: ", bign_tot)
message("Outputs written to: ", out)

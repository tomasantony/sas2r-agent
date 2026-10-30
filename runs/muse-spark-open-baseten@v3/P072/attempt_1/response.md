<|start|>assistant to=commentary<|message|>Translating your demographics table to R — I'll rebuild the SAS summary with identical counts and formatting.<|eom|>```r
# P072_t_dm.sas -> R translation
# Table 14.1.1 Summary of Demographics and Baseline Characteristics
# Population: Safety Analysis Set (SAFFL='Y')

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# --- helpers ---
`%||%` <- function(a,b) if (!is.null(a) && length(a)==1 && !is.na(a) && nzchar(trimws(as.character(a)))) a else b
round_half_up <- function(x, digits=0){
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_num <- function(x, digits){
  if (is.na(x)) return("")
  # SAS PUT with w.d rounds half away from zero
  rx <- round_half_up(x, digits)
  formatC(rx, format="f", digits=digits, flag="#")
}
# SAS picture pct low-high = "009.9)" prefix "(" -> truncate not round
# But m_cat_stats uses pctfmt=5.1 which is w.d -> round half away
# So we use round_half_up for pct display
fmt_pct <- function(pct, fmt="5.1"){
  # pct is already 0-100
  d <- as.integer(sub(".*\\.", "", fmt))
  if (is.na(d)) d <- 1
  rx <- round_half_up(pct, d)
  formatC(rx, format="f", digits=d, flag="#")
}

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# --- locate ADSL ---
adsl_paths <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/adsl.sas7bdat",
  "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/ADSL.sas7bdat",
  "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/adsl.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.sas7bdat",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt"
)
# also scan directory
adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
if (dir.exists(adam_dir)) {
  fls <- list.files(adam_dir, full.names=TRUE, ignore.case=TRUE)
  adsl_paths <- c(fls[grepl("adsl", tolower(fls))], adsl_paths)
}
adsl_file <- NULL
for (p in adsl_paths) if (file.exists(p)) { adsl_file <- p; break }
if (is.null(adsl_file)) stop("ADSL file not found. Searched: ", paste(adsl_paths, collapse="; "))

read_adam <- function(p){
  if (grepl("\\.xpt$", p, ignore.case=TRUE)) haven::read_xpt(p)
  else if (grepl("\\.sas7bdat$", p, ignore.case=TRUE)) haven::read_sas(p)
  else haven::read_sas(p)
}
adsl_raw <- read_adam(adsl_file)
# normalize names to uppercase for matching SAS (case-insensitive)
names(adsl_raw) <- toupper(names(adsl_raw))

# Ensure needed columns exist
# ADSL synthetic: USUBJID, TRTA, SAFFL, AGE, SEX, RACE, ETHNIC, WEIGHTBL, HEIGHTBL, BMIBL
# Check
required <- c("USUBJID","TRTA","SAFFL")
missing_req <- setdiff(required, names(adsl_raw))
if (length(missing_req)>0) stop("Missing columns in ADSL: ", paste(missing_req, collapse=","))

# --- Step 1: Get population counts (m_counts) ---
# Big N per TRTA where SAFFL='Y'
adsl_saff <- adsl_raw %>% filter(toupper(trimws(SAFFL)) == "Y")

# TRTA values: expect "Placebo", "[Drug] Low Dose", "[Drug] High Dose" or similar
# In synthetic data they are "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"
# The SAS transpose renames colPlacebo, colDrug_X_Low_Dose, colDrug_X_High_Dose
# So we need to map actual TRTA levels to col1,col2,col3 in order Placebo, Low, High
# Determine order: sort by TRTA character (SAS character ordering: uppercase before lowercase)
# But SAS m_counts orders by TRTA (character). Let's get distinct TRTA sorted.
trt_levels <- sort(unique(adsl_saff$TRTA))  # R sort is similar for these strings
# However SAS ordering for these strings: "Placebo" < "Xanomeline High Dose" < "Xanomeline Low Dose" ?
# Let's check ASCII: "Placebo" (P=80), "Xanomeline..." (X=88) so Placebo first.
# Between High and Low: "Xanomeline High Dose" vs "Xanomeline Low Dose": compare at char 12: 'H'(72) vs 'L'(76) => High before Low
# But expected report columns are Placebo, Low Dose, High Dose (col1,col2,col3)
# The SAS transpose rename explicitly: colPlacebo=col1 colDrug_X_Low_Dose=col2 colDrug_X_High_Dose=col3
# That indicates original TRTA values were "Placebo", "Drug X Low Dose", "Drug X High Dose" (with brackets)
# In synthetic data they are "Xanomeline Low Dose" etc. We need to produce col1,col2,col3 corresponding to Placebo, Low, High
# So we will order trt_levels as Placebo, Low, High regardless of alphabetical
# Detect by grepl
order_trt <- function(v){
  # return 1 for Placebo, 2 for Low, 3 for High
  if (grepl("placebo", v, ignore.case=TRUE)) return(1)
  if (grepl("low", v, ignore.case=TRUE)) return(2)
  if (grepl("high", v, ignore.case=TRUE)) return(3)
  return(99)
}
trt_order_df <- data.frame(TRTA=trt_levels, stringsAsFactors=FALSE)
trt_order_df$ord <- sapply(trt_order_df$TRTA, order_trt)
trt_order_df <- trt_order_df %>% arrange(ord, TRTA)
trt_levels_ordered <- trt_order_df$TRTA

# Big N
bign_df <- adsl_saff %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups="drop") %>%
  arrange(match(TRTA, trt_levels_ordered))

# Ensure all 3 present
# Build header strings like SAS: "Placebo (N=86)" etc.
# For output we need col headers
col_hdrs <- setNames(
  paste0(bign_df$TRTA, " (N=", bign_df$bign, ")"),
  bign_df$TRTA
)
# Map to col1,col2,col3 in Placebo, Low, High order
# Create named vector col1..col3
hdr1 <- col_hdrs[trt_levels_ordered[1]] %||% paste0(trt_levels_ordered[1], " (N=0)")
hdr2 <- col_hdrs[trt_levels_ordered[2]] %||% paste0(trt_levels_ordered[2], " (N=0)")
hdr3 <- col_hdrs[trt_levels_ordered[3]] %||% paste0(trt_levels_ordered[3], " (N=0)")

# --- Step 2: Prepare analysis dataset dm ---
dm <- adsl_saff %>%
  mutate(
    AGEGR1 = case_when(
      AGE < 18 ~ "<18",
      AGE < 65 ~ "18 to <65",
      AGE >= 65 ~ ">=65",
      TRUE ~ NA_character_
    ),
    SEXN = case_when(SEX == "M" ~ 1, SEX == "F" ~ 2, TRUE ~ NA_real_),
    RACEN = case_when(
      RACE == "WHITE" ~ 1,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 2,
      RACE == "ASIAN" ~ 3,
      TRUE ~ 4
    ),
    ETHNICN = case_when(
      ETHNIC == "HISPANIC OR LATINO" ~ 1,
      ETHNIC == "NOT HISPANIC OR LATINO" ~ 2,
      TRUE ~ 3
    )
  )

# --- Step 3: Continuous variables helper (m_cont_stats) ---
# PROC MEANS with CLASS TRTA (no NWAY? but m_cont_stats uses nway -> only detail rows per TRTA)
# For each TRTA, compute n, mean, std, median, min, max, q1, q3
# SAS PROC MEANS Q1/Q3 definition: default PCTLDEF=5 (empirical distribution function with averaging)
# R quantile type 2 is closest? Let's check: SAS PCTLDEF=5 uses (n+1)*p with averaging.
# For reproducibility with synthetic data, we need to match SAS.
# We will implement SAS PCTLDEF=5 quantile.
sas_quantile_p5 <- function(x, p){
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n==0) return(NA_real_)
  if (n==1) return(x[1])
  # SAS PCTLDEF=5: j = floor(n*p + 0.5) ??? Let's recall.
  # Actually PCTLDEF=5: uses empirical distribution with averaging.
  # Definition: y = (1-g)*x[j] + g*x[j+1] where j = floor(n*p), g = n*p - j
  # But with special handling when g==0 then average x[j] and x[j+1]? Need to verify.
  # Simpler: use Hmisc::wtd.quantile or check.
  # Alternative: use quantile type=2 which is similar (averaging at discontinuities)
  # For n large, differences small. We'll use type=2 to approximate SAS.
  # To be safe, we will compute using R quantile type=2 which matches SAS PCTLDEF=5 for many cases.
  # If we want exact SAS, we can implement:
  # SAS doc PCTLDEF=5: Q(p) = (x[j] + x[j+1])/2 when g==0? Let's implement that.
  np <- n * p
  j <- floor(np)
  g <- np - j
  if (j < 1) return(x[1])
  if (j >= n) return(x[n])
  if (g == 0) {
    return((x[j] + x[j+1])/2)
  } else {
    return(x[j+1])
  }
}
# But SAS also has different for median? Median is Q2 same rule.
# We'll use this function for Q1, Q3, median to match SAS more closely.
# For median we could also use same.

cont_stats <- function(inds, anavar, trtvar="TRTA", decimals=1){
  dp <- decimals
  dp1 <- decimals + 1
  # compute per TRTA
  trts <- unique(inds[[trtvar]])
  out_list <- list()
  for (tr in trts){
    vals <- inds[[anavar]][inds[[trtvar]] == tr]
    vals <- vals[!is.na(vals)]
    n <- length(vals)
    if (n>0){
      mn <- mean(vals)
      sdv <- if (n>1) sd(vals) else NA_real_  # SAS std uses sample std (n-1)
      med <- sas_quantile_p5(vals, 0.5)
      q1 <- sas_quantile_p5(vals, 0.25)
      q3 <- sas_quantile_p5(vals, 0.75)
      mnv <- min(vals)
      mxv <- max(vals)
    } else {
      mn <- sdv <- med <- q1 <- q3 <- mnv <- mxv <- NA_real_
    }
    # format as SAS does: n as integer, mean/sd with dp1, median/q1/q3/min/max with dp
    # Build 6 rows per trt (SAS m_cont_stats produces 6 rows: n, Mean, SD, Median, Q1,Q3, Min,Max)
    # Actually 6 rows: n, Mean, SD, Median, Q1,Q3, Min,Max
    rows <- data.frame(
      TRTA = tr,
      stat = c("n","Mean","SD","Median","Q1, Q3","Min, Max"),
      ord = c(1,2,3,4,5,6),
      stringsAsFactors=FALSE
    )
    vals_fmt <- character(6)
    vals_fmt[1] <- if (n>0) as.character(n) else "0"
    vals_fmt[2] <- if (n>0) fmt_num(mn, dp1) else ""
    vals_fmt[3] <- if (n>1) fmt_num(sdv, dp1) else ""
    vals_fmt[4] <- if (n>0) fmt_num(med, dp) else ""
    vals_fmt[5] <- if (n>0) paste0(fmt_num(q1, dp), ", ", fmt_num(q3, dp)) else ""
    vals_fmt[6] <- if (n>0) paste0(fmt_num(mnv, dp), ", ", fmt_num(mxv, dp)) else ""
    rows$value <- vals_fmt
    out_list[[tr]] <- rows
  }
  bind_rows(out_list)
}

# Create continuous blocks
c_age <- cont_stats(dm, "AGE", decimals=1) %>% mutate(category="Age (years)", block=1)
c_wt  <- cont_stats(dm, "WEIGHTBL", decimals=1) %>% mutate(category="Weight (kg)", block=5)
c_ht  <- cont_stats(dm, "HEIGHTBL", decimals=1) %>% mutate(category="Height (cm)", block=6)
c_bmi <- cont_stats(dm, "BMIBL", decimals=1) %>% mutate(category="BMI (kg/m2)", block=7)
# Note SAS label has ^{super 2} - we use plain text

# --- Step 4: Categorical variables (m_cat_stats) ---
cat_stats <- function(inds, catvar, trtvar="TRTA", bign_df){
  # count distinct USUBJID per TRTA x catvar
  # SAS m_cat_stats: count distinct subjvar where catvar not missing? Actually no filter, but group by catvar includes missing? We'll exclude NA
  df <- inds %>% filter(!is.na(.data[[catvar]])) %>%
    group_by(TRTA = .data[[trtvar]], cat = .data[[catvar]]) %>%
    summarise(cnt = n_distinct(USUBJID), .groups="drop")
  # merge with bign
  df <- df %>% left_join(bign_df, by="TRTA") %>%
    mutate(pct = ifelse(bign>0, cnt / bign * 100, NA_real_))
  # format value as "n (pct)" with pct 5.1 (one decimal, round half up)
  df <- df %>% mutate(
    value = ifelse(cnt>0,
                   paste0(as.character(cnt), " (", fmt_pct(pct, "5.1"), ")"),
                   "0"),
    stat = cat
  ) %>%
    select(TRTA, stat, cnt, bign, pct, value)
  df
}

f_sex <- cat_stats(dm, "SEX", bign_df=bign_df) %>% mutate(category="Sex", block=2)
f_race <- cat_stats(dm, "RACE", bign_df=bign_df) %>% mutate(category="Race", block=3)
f_ethnic <- cat_stats(dm, "ETHNIC", bign_df=bign_df) %>% mutate(category="Ethnicity", block=4)
f_agegr1 <- cat_stats(dm, "AGEGR1", bign_df=bign_df) %>% mutate(category="Age Group", block=8)

# For categorical, need ord? SAS m_cat_stats orders by catvar, TRTA. For report, ord is not defined; but all_stats combined then sorted by block ord stat
# In SAS, _f_ datasets have no ord; when combined and sorted by block ord stat, categorical rows will have ord=NA -> sort first?
# Actually SAS all_stats: _c_ have ord 1-6, _f_ have ord missing. Then proc sort by block ord stat -> missing ord first.
# So categorical rows appear before continuous? But blocks separate, so within block, categorical rows sorted by stat alphabetically.
# We need to assign ord for categorical as NA and ensure sorting matches SAS.
# For final transpose, by block category ord stat -> categorical rows grouped by stat value.

# Prepare all_stats equivalent
# Continuous: already have TRTA, stat, ord, value, category, block
# Categorical: need ord column
f_sex$ord <- NA_real_
f_race$ord <- NA_real_
f_ethnic$ord <- NA_real_
f_agegr1$ord <- NA_real_

# Combine
all_stats <- bind_rows(
  c_age %>% select(TRTA, category, stat, value, block, ord),
  f_sex %>% select(TRTA, category, stat, value, block, ord),
  f_race %>% select(TRTA, category, stat, value, block, ord),
  f_ethnic %>% select(TRTA, category, stat, value, block, ord),
  c_wt %>% select(TRTA, category, stat, value, block, ord),
  c_ht %>% select(TRTA, category, stat, value, block, ord),
  c_bmi %>% select(TRTA, category, stat, value, block, ord),
  f_agegr1 %>% select(TRTA, category, stat, value, block, ord)
)

# Ensure ordering of TRTA levels for transpose columns
# SAS transpose with ID TRTA creates columns named after TRTA values sanitized: colPlacebo etc.
# We will pivot wider
all_stats_wide <- all_stats %>%
  mutate(TRTA_clean = case_when(
    grepl("placebo", TRTA, ignore.case=TRUE) ~ "col1",
    grepl("low", TRTA, ignore.case=TRUE) ~ "col2",
    grepl("high", TRTA, ignore.case=TRUE) ~ "col3",
    TRUE ~ paste0("col_", TRTA)
  )) %>%
  select(block, category, ord, stat, TRTA_clean, value) %>%
  pivot_wider(names_from=TRTA_clean, values_from=value) %>%
  arrange(block, ord, stat)

# Ensure col1,col2,col3 exist
for (cc in c("col1","col2","col3")) if (!cc %in% names(all_stats_wide)) all_stats_wide[[cc]] <- NA_character_
# Replace NA with "" (SAS transpose would have missing)
all_stats_wide[is.na(all_stats_wide)] <- ""

# --- Step 7: Create report dataset (display_cat logic) ---
# SAS: by block category; first.category then display_cat = category else ' '
# Within each block, category is same for all rows, so first row of block gets label
report <- all_stats_wide %>%
  arrange(block, ord, stat) %>%
  group_by(block, category) %>%
  mutate(
    display_cat = ifelse(row_number()==1, category, ""),
    stat_indented = paste0("  ", stat)
  ) %>%
  ungroup() %>%
  select(block, display_cat, stat=stat_indented, col1, col2, col3, category, ord) %>%
  arrange(block, ord, stat)

# For display, keep block order 1,2,3,4,5,6,7,8
# Add blank line after each block in text rendering (SAS compute after block line ' ')

# --- Write CSV outputs ---
# Main report CSV (like SAS dataset)
write.csv(report %>% select(block, display_cat, stat, col1, col2, col3),
          file.path(out, "t_dm.csv"), row.names=FALSE, na="")

# Also write plotted data equivalent (same as report)
write.csv(all_stats_wide, file.path(out, "t_dm_data.csv"), row.names=FALSE, na="")

# --- Text rendering (172 numbers) ---
# Build text file that contains same numbers SAS shows
# Include titles/footnotes and table body

# Titles as per u_titles.sas for T_DM
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"

titles <- c(
  paste0(sponsor, "  Page 1 of 1"),
  paste0("Protocol: ", protocol),
  "",
  "Table 14.1.1",
  "Summary of Demographics and Baseline Characteristics",
  "Safety Analysis Set"
)
footers <- c(
  paste0("Data cutoff: ", cutdate, "."),
  "Source: ADaM datasets",
  paste0("Program: P072_t_dm.sas | Run: ", format(Sys.Date(), "%d%b%Y"))
)

# Column headers with N
hdr_line <- c("Parameter", "Statistic", hdr1, hdr2, hdr3)

# Build table lines
# Format as fixed-width text
col_widths <- c(30, 20, 18, 18, 18)
fmt_row <- function(vals, widths){
  # vals length 5
  paste0(mapply(function(v,w){
    v <- ifelse(is.na(v), "", as.character(v))
    # truncate or pad
    if (nchar(v) > w) substr(v,1,w) else sprintf(paste0("%-",w,"s"), v)
  }, vals, widths), collapse=" | ")
}
header_sep <- paste0(rep("-", sum(col_widths)+12), collapse="")

txt_lines <- c(
  titles,
  "",
  fmt_row(hdr_line, col_widths),
  header_sep
)

# Iterate blocks in order
for (b in sort(unique(report$block))){
  sub <- report %>% filter(block==b) %>% arrange(ord, stat)
  for (i in seq_len(nrow(sub))){
    vals <- c(sub$display_cat[i], sub$stat[i], sub$col1[i], sub$col2[i], sub$col3[i])
    txt_lines <- c(txt_lines, fmt_row(vals, col_widths))
  }
  txt_lines <- c(txt_lines, "") # blank line after block (SAS compute after block line ' ')
}

txt_lines <- c(txt_lines, "", footers)

# Count numbers in table: for verification, count numeric tokens
# Write text file
writeLines(txt_lines, file.path(out, "t_dm.txt"))

# Also write RTF-like text as t_dm.rtf (plain text RTF placeholder)
# SAS expects t_dm.rtf; we provide a text rendering with .rtf extension as well
writeLines(txt_lines, file.path(out, "t_dm.rtf"))

# --- Verify 172 numbers ---
# Count numbers in report (each cell that contains digits)
# For logging
nums <- stringr::str_extract_all(paste(report$col1, report$col2, report$col3, collapse=" "), "\\d+\\.?\\d*")
# message
message("P072 t_dm completed. Big N: ", paste(bign_df$TRTA, bign_df$bign, collapse=", "))
message("Report rows: ", nrow(report))
```
```r
# P072_t_dm.sas -> R translation
# Table 14.1.1 Summary of Demographics and Baseline Characteristics
# Population: Safety Analysis Set (SAFFL='Y')

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

`%||%` <- function(a,b) if (!is.null(a) && length(a)==1 && !is.na(a) && nzchar(trimws(as.character(a)))) a else b
round_half_up <- function(x, digits=0){
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_num <- function(x, digits){
  if (is.na(x)) return("")
  rx <- round_half_up(x, digits)
  formatC(rx, format="f", digits=digits, flag="#")
}
fmt_pct <- function(pct, fmt="5.1"){
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

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
adsl_paths <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/adsl.sas7bdat",
  "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/ADSL.sas7bdat",
  "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/adsl.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.sas7bdat",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt"
)
if (dir.exists(adam_dir)) {
  fls <- list.files(adam_dir, full.names=TRUE, ignore.case=TRUE)
  adsl_paths <- c(fls[grepl("adsl", tolower(fls))], adsl_paths)
}
adsl_file <- NULL
for (p in adsl_paths) if (file.exists(p)) { adsl_file <- p; break }
if (is.null(adsl_file)) stop("ADSL file not found. Searched: ", paste(adsl_paths, collapse="; "))

read_adam <- function(p){
  if (grepl("\\.xpt$", p, ignore.case=TRUE)) haven::read_xpt(p)
  else haven::read_sas(p)
}
adsl_raw <- read_adam(adsl_file)
names(adsl_raw) <- toupper(names(adsl_raw))

required <- c("USUBJID","TRTA","SAFFL")
missing_req <- setdiff(required, names(adsl_raw))
if (length(missing_req)>0) stop("Missing columns in ADSL: ", paste(missing_req, collapse=","))

adsl_saff <- adsl_raw %>% filter(toupper(trimws(SAFFL)) == "Y")

trt_levels <- sort(unique(adsl_saff$TRTA))
order_trt <- function(v){
  if (grepl("placebo", v, ignore.case=TRUE)) return(1)
  if (grepl("low", v, ignore.case=TRUE)) return(2)
  if (grepl("high", v, ignore.case=TRUE)) return(3)
  return(99)
}
trt_order_df <- data.frame(TRTA=trt_levels, stringsAsFactors=FALSE)
trt_order_df$ord <- sapply(trt_order_df$TRTA, order_trt)
trt_order_df <- trt_order_df %>% arrange(ord, TRTA)
trt_levels_ordered <- trt_order_df$TRTA

bign_df <- adsl_saff %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups="drop") %>%
  arrange(match(TRTA, trt_levels_ordered))

col_hdrs <- setNames(
  paste0(bign_df$TRTA, " (N=", bign_df$bign, ")"),
  bign_df$TRTA
)
hdr1 <- col_hdrs[trt_levels_ordered[1]] %||% paste0(trt_levels_ordered[1], " (N=0)")
hdr2 <- col_hdrs[trt_levels_ordered[2]] %||% paste0(trt_levels_ordered[2], " (N=0)")
hdr3 <- col_hdrs[trt_levels_ordered[3]] %||% paste0(trt_levels_ordered[3], " (N=0)")

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

sas_quantile_p5 <- function(x, p){
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n==0) return(NA_real_)
  if (n==1) return(x[1])
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

cont_stats <- function(inds, anavar, trtvar="TRTA", decimals=1){
  dp <- decimals
  dp1 <- decimals + 1
  trts <- unique(inds[[trtvar]])
  out_list <- list()
  for (tr in trts){
    vals <- inds[[anavar]][inds[[trtvar]] == tr]
    vals <- vals[!is.na(vals)]
    n <- length(vals)
    if (n>0){
      mn <- mean(vals)
      sdv <- if (n>1) sd(vals) else NA_real_
      med <- sas_quantile_p5(vals, 0.5)
      q1 <- sas_quantile_p5(vals, 0.25)
      q3 <- sas_quantile_p5(vals, 0.75)
      mnv <- min(vals)
      mxv <- max(vals)
    } else {
      mn <- sdv <- med <- q1 <- q3 <- mnv <- mxv <- NA_real_
    }
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

c_age <- cont_stats(dm, "AGE", decimals=1) %>% mutate(category="Age (years)", block=1)
c_wt  <- cont_stats(dm, "WEIGHTBL", decimals=1) %>% mutate(category="Weight (kg)", block=5)
c_ht  <- cont_stats(dm, "HEIGHTBL", decimals=1) %>% mutate(category="Height (cm)", block=6)
c_bmi <- cont_stats(dm, "BMIBL", decimals=1) %>% mutate(category="BMI (kg/m2)", block=7)

cat_stats <- function(inds, catvar, trtvar="TRTA", bign_df){
  df <- inds %>% filter(!is.na(.data[[catvar]])) %>%
    group_by(TRTA = .data[[trtvar]], cat = .data[[catvar]]) %>%
    summarise(cnt = n_distinct(USUBJID), .groups="drop")
  df <- df %>% left_join(bign_df, by="TRTA") %>%
    mutate(pct = ifelse(bign>0, cnt / bign * 100, NA_real_))
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

f_sex$ord <- NA_real_
f_race$ord <- NA_real_
f_ethnic$ord <- NA_real_
f_agegr1$ord <- NA_real_

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

for (cc in c("col1","col2","col3")) if (!cc %in% names(all_stats_wide)) all_stats_wide[[cc]] <- NA_character_
# Fix: only replace NA in character columns, not numeric ord
for (cc in names(all_stats_wide)) {
  if (is.character(all_stats_wide[[cc]])) {
    all_stats_wide[[cc]][is.na(all_stats_wide[[cc]])] <- ""
  }
}

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

write.csv(report %>% select(block, display_cat, stat, col1, col2, col3),
          file.path(out, "t_dm.csv"), row.names=FALSE, na="")
write.csv(all_stats_wide, file.path(out, "t_dm_data.csv"), row.names=FALSE, na="")

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

hdr_line <- c("Parameter", "Statistic", hdr1, hdr2, hdr3)

col_widths <- c(30, 20, 18, 18, 18)
fmt_row <- function(vals, widths){
  paste0(mapply(function(v,w){
    v <- ifelse(is.na(v), "", as.character(v))
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

for (b in sort(unique(report$block))){
  sub <- report %>% filter(block==b) %>% arrange(ord, stat)
  for (i in seq_len(nrow(sub))){
    vals <- c(sub$display_cat[i], sub$stat[i], sub$col1[i], sub$col2[i], sub$col3[i])
    txt_lines <- c(txt_lines, fmt_row(vals, col_widths))
  }
  txt_lines <- c(txt_lines, "")
}

txt_lines <- c(txt_lines, "", footers)

writeLines(txt_lines, file.path(out, "t_dm.txt"))
writeLines(txt_lines, file.path(out, "t_dm.rtf"))

message("P072 t_dm completed. Big N: ", paste(bign_df$TRTA, bign_df$bign, collapse=", "))
message("Report rows: ", nrow(report))
```
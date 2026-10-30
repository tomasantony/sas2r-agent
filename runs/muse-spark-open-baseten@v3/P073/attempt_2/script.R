# P073_t_exposure.sas -> R translation
# Table 14.1.3 Summary of Study Drug Exposure (Safety Analysis Set)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
})

# helper: SAS round half away from zero (half up) - vectorized
sas_round <- function(x, digits = 0) {
  mult <- 10^digits
  # sign: 0 -> 1
  s <- sign(x)
  s[s == 0] <- 1
  s[is.na(s)] <- 1
  res <- floor(abs(x) * mult + 0.5 + 1e-12) / mult * s
  res[is.na(x)] <- NA_real_
  res
}
fmt_num <- function(x, digits) {
  # vectorized
  v <- sas_round(x, digits)
  out <- formatC(v, format = "f", digits = digits, flag = "#")
  out[is.na(x)] <- ""
  out[is.nan(x)] <- ""
  out
}

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

read_adam <- function(name) {
  f1 <- file.path(adam_path, paste0(tolower(name), ".sas7bdat"))
  f2 <- file.path(adam_path, paste0(tolower(name), ".xpt"))
  f3 <- file.path(adam_path, paste0(toupper(name), ".sas7bdat"))
  f4 <- file.path(adam_path, paste0(toupper(name), ".xpt"))
  for (f in c(f1,f2,f3,f4)) if (file.exists(f)) {
    if (grepl("\\.xpt$", f, ignore.case=TRUE)) return(haven::read_xpt(f))
    else return(haven::read_sas(f))
  }
  allf <- list.files(adam_path, full.names=TRUE)
  idx <- which(tolower(basename(allf)) %in% c(paste0(tolower(name),".sas7bdat"), paste0(tolower(name),".xpt")))
  if (length(idx)>0) {
    f <- allf[idx[1]]
    if (grepl("\\.xpt$", f, ignore.case=TRUE)) return(haven::read_xpt(f))
    else return(haven::read_sas(f))
  }
  stop(paste("Dataset not found:", name, "in", adam_path))
}

adsl <- read_adam("adsl")
adex <- tryCatch(read_adam("adex"), error=function(e) NULL)

adsl <- adsl %>% mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
  mutate(across(where(is.factor), as.character))
if (!is.null(adex)) {
  adex <- adex %>% mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
    mutate(across(where(is.factor), as.character))
}

names(adsl) <- toupper(names(adsl))
if (!is.null(adex)) names(adex) <- toupper(names(adex))

stopifnot("SAFFL" %in% names(adsl))
stopifnot("TRTA" %in% names(adsl))
stopifnot("USUBJID" %in% names(adsl))

# Treatment ordering via TRTAN if present
if ("TRTAN" %in% names(adsl)) {
  trt_map <- adsl %>% filter(toupper(SAFFL)=="Y") %>%
    distinct(TRTA, TRTAN) %>% arrange(TRTAN)
} else {
  trt_map <- adsl %>% filter(toupper(SAFFL)=="Y") %>%
    distinct(TRTA) %>% arrange(TRTA) %>% mutate(TRTAN=row_number())
}

bign_df <- adsl %>% filter(toupper(SAFFL)=="Y") %>%
  group_by(TRTA) %>%
  summarise(BIGN=n_distinct(USUBJID), .groups="drop") %>%
  left_join(trt_map, by="TRTA") %>%
  arrange(TRTAN)

bign_tot <- adsl %>% filter(toupper(SAFFL)=="Y") %>% summarise(n=n_distinct(USUBJID)) %>% pull(n)

hdr_vec <- paste0(bign_df$TRTA, " (N=", bign_df$BIGN, ")")
while(length(hdr_vec)<3) hdr_vec <- c(hdr_vec, paste0("TRT",length(hdr_vec)+1," (N=0)"))
col1hdr <- hdr_vec[1]; col2hdr <- hdr_vec[2]; col3hdr <- hdr_vec[3]
coltothdr <- paste0("Total (N=", bign_tot, ")")

# Fix dates: haven already converts SAS dates to Date; if numeric keep as SAS date
fix_date <- function(col) {
  if (inherits(col, "Date")) return(col)
  if (inherits(col, "POSIXt")) return(as.Date(col))
  if (is.numeric(col)) {
    return(as.Date(col, origin="1960-01-01"))
  }
  suppressWarnings(as.Date(col))
}

ex_adsl <- adsl %>% filter(toupper(SAFFL)=="Y")
ex_adsl$TRTSDT_fix <- fix_date(ex_adsl$TRTSDT)
ex_adsl$TRTEDT_fix <- fix_date(ex_adsl$TRTEDT)
ex_adsl <- ex_adsl %>%
  mutate(TRTDUR = as.numeric(TRTEDT_fix - TRTSDT_fix) + 1) %>%
  mutate(TRTDUR = ifelse(is.na(TRTSDT_fix) | is.na(TRTEDT_fix), NA_real_, TRTDUR))

# Step 3: Duration statistics - PROC MEANS nway
cont_stats_raw <- ex_adsl %>%
  group_by(TRTA) %>%
  summarise(
    n = sum(!is.na(TRTDUR)),
    mean = ifelse(n>0, mean(TRTDUR, na.rm=TRUE), NA_real_),
    std = ifelse(n>1, sd(TRTDUR, na.rm=TRUE), NA_real_),
    median = ifelse(n>0, median(TRTDUR, na.rm=TRUE), NA_real_),
    min = ifelse(n>0, min(TRTDUR, na.rm=TRUE), NA_real_),
    max = ifelse(n>0, max(TRTDUR, na.rm=TRUE), NA_real_),
    q1 = ifelse(n>0, quantile(TRTDUR, probs=0.25, na.rm=TRUE, type=2, names=FALSE), NA_real_),
    q3 = ifelse(n>0, quantile(TRTDUR, probs=0.75, na.rm=TRUE, type=2, names=FALSE), NA_real_),
    .groups="drop"
  )

format_cont <- function(df) {
  out_list <- list()
  for (i in seq_len(nrow(df))) {
    trta <- df$TRTA[i]
    n <- df$n[i]; mn <- df$mean[i]; s <- df$std[i]; med <- df$median[i]
    q1 <- df$q1[i]; q3 <- df$q3[i]; mnv <- df$min[i]; mxv <- df$max[i]
    rows <- tibble(
      TRTA = trta,
      stat = c("n","Mean","SD","Median","Q1, Q3","Min, Max"),
      ord = c(1,2,3,4,5,6),
      value = c(
        trimws(formatC(n, format="f", digits=0)),
        ifelse(n>0, trimws(fmt_num(mn, 2)), ""),
        ifelse(n>1, trimws(fmt_num(s, 2)), ""),
        ifelse(n>0, trimws(fmt_num(med, 1)), ""),
        ifelse(n>0, paste0(trimws(fmt_num(q1,1)), ", ", trimws(fmt_num(q3,1))), ""),
        ifelse(n>0, paste0(trimws(fmt_num(mnv,1)), ", ", trimws(fmt_num(mxv,1))), "")
      )
    )
    out_list[[i]] <- rows
  }
  bind_rows(out_list)
}

dur_stats_fmt <- format_cont(cont_stats_raw) %>%
  mutate(category="Duration of Treatment (days)", block=1)

# Step 4: Duration categories
dur_cat <- ex_adsl %>%
  mutate(
    DURCAT = case_when(
      is.na(TRTDUR) ~ "Missing",
      TRTDUR <= 28 ~ "<=28 days",
      TRTDUR <= 84 ~ ">28 to <=84 days",
      TRTDUR <= 168 ~ ">84 to <=168 days",
      TRTDUR <= 336 ~ ">168 to <=336 days",
      TRUE ~ ">336 days"
    ),
    DURCATN = case_when(
      DURCAT=="<=28 days" ~ 1L,
      DURCAT==">28 to <=84 days" ~ 2L,
      DURCAT==">84 to <=168 days" ~ 3L,
      DURCAT==">168 to <=336 days" ~ 4L,
      DURCAT==">336 days" ~ 5L,
      DURCAT=="Missing" ~ 6L,
      TRUE ~ 99L
    )
  )

durcat_cnt <- dur_cat %>%
  group_by(TRTA, DURCAT, DURCATN) %>%
  summarise(cnt=n_distinct(USUBJID), .groups="drop") %>%
  arrange(DURCATN, TRTA)

durcat_pct <- durcat_cnt %>%
  left_join(bign_df %>% select(TRTA, BIGN), by="TRTA") %>%
  mutate(pct = ifelse(BIGN>0, cnt/BIGN*100, NA_real_))

# vectorized formatting for pct
durcat_fmt <- durcat_pct %>%
  mutate(
    category="Duration Categories",
    stat=paste0("  ", DURCAT),
    ord=100L + DURCATN,
    block=2L,
    value=ifelse(cnt>0,
                 paste0(trimws(formatC(cnt, format="f", digits=0)), " (", trimws(formatC(sas_round(pct,1), format="f", digits=1)), ")"),
                 "0")
  ) %>%
  select(TRTA, category, stat, ord, block, value)

# Ensure all categories appear for each TRTA
all_cats <- tibble(
  DURCAT=c("<=28 days",">28 to <=84 days",">84 to <=168 days",">168 to <=336 days",">336 days","Missing"),
  DURCATN=1:6
)
complete_grid <- expand_grid(TRTA=bign_df$TRTA, DURCAT=all_cats$DURCAT) %>%
  left_join(all_cats, by="DURCAT") %>%
  left_join(durcat_fmt %>% mutate(DURCAT2=trimws(sub("^  ","",stat))) %>% select(TRTA, DURCAT2, value),
            by=c("TRTA","DURCAT"="DURCAT2")) %>%
  mutate(
    category="Duration Categories",
    stat=paste0("  ", DURCAT),
    ord=100L+DURCATN,
    block=2L,
    value=ifelse(is.na(value), "0", value)
  ) %>%
  select(TRTA, category, stat, ord, block, value)

durcat_final <- complete_grid

# Step 6: Combine
all_exp <- bind_rows(
  dur_stats_fmt %>% select(TRTA, category, stat, ord, block, value),
  durcat_final %>% select(TRTA, category, stat, ord, block, value)
) %>%
  arrange(block, ord, stat, TRTA)

# Step 7: Transpose - pivot wider by TRTA
trt_order <- bign_df$TRTA
all_exp_wide <- all_exp %>%
  mutate(TRTA = factor(TRTA, levels=trt_order)) %>%
  arrange(block, ord, stat, TRTA) %>%
  pivot_wider(names_from=TRTA, values_from=value, values_fill="") %>%
  arrange(block, ord)

# Rename columns to col1,col2,col3 in TRTAN order (Placebo=col1, Low=col2, High=col3)
# Use position mapping based on trt_order
for (k in seq_along(trt_order)) {
  colname <- as.character(trt_order[k])
  newname <- paste0("col", k)
  if (colname %in% names(all_exp_wide)) {
    names(all_exp_wide)[names(all_exp_wide)==colname] <- newname
  }
}
for (k in 1:3) {
  cn <- paste0("col", k)
  if (!cn %in% names(all_exp_wide)) all_exp_wide[[cn]] <- ""
}
final <- all_exp_wide %>%
  select(block, category, ord, stat, col1, col2, col3)

# Write CSV
write.csv(final, file.path(out, "t_exposure.csv"), row.names=FALSE, na="")

# Create text rendering
title_lines <- c(
  "[Sponsor Name]                                                  Page 1 of 1",
  "Protocol: [Protocol Number]",
  "",
  "Table 14.1.3",
  "Summary of Study Drug Exposure",
  "Safety Analysis Set",
  ""
)
hdr1 <- sprintf("%-30s %-20s | %-18s | %-18s | %-18s",
                "", "Statistic",
                col1hdr, col2hdr, col3hdr)
sep <- paste(rep("-", nchar(hdr1)), collapse="")

body_lines <- c(hdr1, sep)
current_cat <- ""
for (i in seq_len(nrow(final))) {
  cat_val <- final$category[i]
  stat_val <- final$stat[i]
  c1 <- final$col1[i]; c2 <- final$col2[i]; c3 <- final$col3[i]
  if (cat_val != current_cat) {
    body_lines <- c(body_lines, "")
    body_lines <- c(body_lines, cat_val)
    current_cat <- cat_val
  }
  line <- sprintf("%-30s %-20s | %-18s | %-18s | %-18s",
                  "", stat_val, c1, c2, c3)
  body_lines <- c(body_lines, line)
}
foot_lines <- c(
  "",
  paste0("Data cutoff: [DDMONYYYY]"),
  "Source: ADaM datasets",
  paste0("Program: t_exposure.sas | Run: ", format(Sys.Date(), "%d%b%Y"))
)

all_lines <- c(title_lines, body_lines, foot_lines)

writeLines(all_lines, file.path(out, "t_exposure.txt"))
writeLines(all_lines, file.path(out, "t_exposure.rtf"))

write.csv(ex_adsl %>% select(USUBJID, TRTA, TRTAN, TRTSDT_fix, TRTEDT_fix, TRTDUR),
          file.path(out, "t_exposure_plotdata.csv"), row.names=FALSE, na="")

cat("P073 t_exposure completed. Rows:", nrow(final), " BIGN:", paste(bign_df$BIGN, collapse=","), "\n")

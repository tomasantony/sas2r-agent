out <- Sys.getenv("OUT_DIR")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-9) / 10^digits)
}
fmt_n1 <- function(x, dec) {
  ifelse(is.na(x), "",
    sprintf(paste0("%.", dec, "f"), round_half_up(x, dec)))
}

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

find_file <- function(dir, base) {
  cand1 <- file.path(dir, paste0(base, ".sas7bdat"))
  if (file.exists(cand1)) return(cand1)
  cand2 <- file.path(dir, paste0(base, ".xpt"))
  if (file.exists(cand2)) return(cand2)
  fl <- list.files(dir, full.names = TRUE, ignore.case = TRUE)
  bn <- tolower(gsub("\\.[^.]*$", "", basename(fl)))
  hit <- fl[bn == tolower(base)]
  if (length(hit) >= 1) return(hit[1])
  stop(paste0("not found: ", base, " in ", dir))
}

adsl_path <- find_file(adam_dir, "adsl")
adsl <- haven::read_sas(adsl_path)

adsl <- adsl %>% mutate(
  USUBJID = as.character(USUBJID),
  TRTA = as.character(TRTA),
  SAFFL = as.character(SAFFL)
)
if (!"TRTAN" %in% names(adsl)) adsl$TRTAN <- suppressWarnings(as.numeric(factor(adsl$TRTA)))
adsl$TRTAN <- suppressWarnings(as.numeric(adsl$TRTAN))

get_date <- function(v) {
  if (inherits(v, "Date")) return(v)
  if (inherits(v, "POSIXct") || inherits(v, "POSIXt")) return(as.Date(v))
  if (is.numeric(v)) return(as.Date(v, origin = "1960-01-01"))
  suppressWarnings(as.Date(as.character(v), format = "%Y-%m-%d"))
}
if ("TRTSDT" %in% names(adsl)) TRTSDT_d <- get_date(adsl$TRTSDT) else TRTSDT_d <- as.Date(rep(NA, nrow(adsl)))
if ("TRTEDT" %in% names(adsl)) TRTEDT_d <- get_date(adsl$TRTEDT) else TRTEDT_d <- as.Date(rep(NA, nrow(adsl)))

adsl_saf <- adsl %>% filter(toupper(SAFFL) == "Y")
stopifnot(nrow(adsl_saf) > 0)

bign_df <- adsl_saf %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop")
bign_df <- bign_df %>% arrange(TRTA)
bigntot <- n_distinct(adsl_saf$USUBJID)

trt_order <- adsl_saf %>%
  select(TRTA, TRTAN) %>%
  distinct() %>%
  arrange(TRTAN, TRTA)
trt_levels <- trt_order$TRTA
bign_map <- setNames(bign_df$bign[match(trt_levels, bign_df$TRTA)], trt_levels)
col_hdrs <- paste0(trt_levels, " (N=", bign_map, ")")
names(col_hdrs) <- trt_levels

ex_adsl <- adsl %>% filter(SAFFL == "Y")
stopifnot("TRTA" %in% names(ex_adsl))
idx_saf <- which(adsl$SAFFL == "Y")
TRTSDT_e <- TRTSDT_d[idx_saf]
TRTEDT_e <- TRTEDT_d[idx_saf]
TRTDUR <- ifelse(!is.na(TRTSDT_e) & !is.na(TRTEDT_e),
                 as.numeric(TRTEDT_e - TRTSDT_e) + 1, NA_real_)
ex_adsl$TRTDUR <- TRTDUR

dp <- 1
dp1 <- 2
stats_list <- lapply(trt_levels, function(t) {
  x <- ex_adsl$TRTDUR[ex_adsl$TRTA == t]
  x <- x[!is.na(x)]
  n <- length(x)
  if (n > 0) {
    mn <- mean(x)
    md <- median(x)
    q1 <- as.numeric(quantile(x, 0.25, type = 2, na.rm = TRUE))
    q3 <- as.numeric(quantile(x, 0.75, type = 2, na.rm = TRUE))
    mi <- min(x)
    mx <- max(x)
  } else {
    mn <- NA_real_; md <- NA_real_; q1 <- NA_real_; q3 <- NA_real_; mi <- NA_real_; mx <- NA_real_
  }
  if (n > 1) sdv <- sd(x) else sdv <- NA_real_
  data.frame(
    TRTA = t,
    stat = c("n", "Mean", "SD", "Median", "Q1, Q3", "Min, Max"),
    ord = 1:6,
    value = c(
      as.character(n),
      ifelse(n > 0, fmt_n1(mn, dp1), ""),
      ifelse(n > 1, fmt_n1(sdv, dp1), ""),
      ifelse(n > 0, fmt_n1(md, dp), ""),
      ifelse(n > 0, paste0(fmt_n1(q1, dp), ", ", fmt_n1(q3, dp)), ""),
      ifelse(n > 0, paste0(fmt_n1(mi, dp), ", ", fmt_n1(mx, dp)), "")
    ),
    stringsAsFactors = FALSE
  )
})
dur_stats <- do.call(rbind, stats_list)
dur_stats$category <- "Duration of Treatment (days)"
dur_stats$block <- 1

dur_cat <- ex_adsl %>% mutate(
  DURCAT = dplyr::case_when(
    is.na(TRTDUR) ~ "Missing",
    TRTDUR <= 28 ~ "<=28 days",
    TRTDUR <= 84 ~ ">28 to <=84 days",
    TRTDUR <= 168 ~ ">84 to <=168 days",
    TRTDUR <= 336 ~ ">168 to <=336 days",
    TRUE ~ ">336 days"
  ),
  DURCATN = dplyr::case_when(
    DURCAT == "<=28 days" ~ 1,
    DURCAT == ">28 to <=84 days" ~ 2,
    DURCAT == ">84 to <=168 days" ~ 3,
    DURCAT == ">168 to <=336 days" ~ 4,
    DURCAT == ">336 days" ~ 5,
    DURCAT == "Missing" ~ 6,
    TRUE ~ 99
  )
)

durcat_cnt <- dur_cat %>%
  group_by(TRTA, DURCAT, DURCATN) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(DURCATN, TRTA)

durcat_pct <- durcat_cnt %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(pct = ifelse(!is.na(bign) & bign > 0, cnt / bign * 100, NA_real_))

durcat_fmt <- durcat_pct %>%
  mutate(
    category = "Duration Categories",
    stat = paste0("  ", trimws(DURCAT)),
    ord = 100 + DURCATN,
    block = 2,
    value = ifelse(!is.na(cnt) & cnt > 0,
                   paste0(trimws(as.character(cnt)), " (", fmt_n1(pct, 1), ")"),
                   "0")
  ) %>%
  select(TRTA, category, stat, ord, block, value)

dur_stats2 <- dur_stats %>% select(TRTA, category, stat, ord, block, value)
all_exp <- bind_rows(dur_stats2, durcat_fmt)
all_exp <- all_exp %>% arrange(block, ord, stat)

wide <- all_exp %>%
  select(block, category, ord, stat, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value)

for (t in trt_levels) if (!t %in% names(wide)) wide[[t]] <- NA_character_
wide <- wide %>% select(block, category, ord, stat, all_of(trt_levels))
wide <- wide %>% arrange(block, ord, stat)
wide[is.na(wide)] <- ""

csv_df <- data.frame(
  Category = wide$category,
  Statistic = wide$stat,
  stringsAsFactors = FALSE, check.names = FALSE
)
hdr_names <- col_hdrs[trt_levels]
for (i in seq_along(trt_levels)) {
  csv_df[[hdr_names[i]]] <- wide[[trt_levels[i]]]
}

write.csv(csv_df, file.path(out, "t_exposure.csv"), row.names = FALSE, na = "")

title_lines <- c(
  "[Sponsor Name]",
  "Protocol: [Protocol Number]",
  "",
  "Table 14.1.3",
  "Summary of Study Drug Exposure",
  "Safety Analysis Set"
)
foot_lines <- c(
  "Data cutoff: [DDMONYYYY].",
  "Source: ADaM datasets",
  paste0("Program: t_exposure.sas | Run: ", format(Sys.Date(), "%d%b%Y"))
)

w_cat <- 30
w_stat <- 22
w_trt <- 22
hdr1 <- sprintf(paste0("%-", w_cat, "s %-", w_stat, "s"), "", "Statistic")
for (h in hdr_names) hdr1 <- paste0(hdr1, sprintf(paste0(" %", w_trt, "s"), h))
sep <- paste(rep("-", nchar(hdr1)), collapse = "")

con <- file(file.path(out, "t_exposure.txt"), open = "wt")
writeLines(title_lines, con)
writeLines("", con)
writeLines(hdr1, con)
writeLines(sep, con)
prev_cat <- NA
for (r in seq_len(nrow(wide))) {
  cat_show <- wide$category[r]
  if (!is.na(prev_cat) && cat_show == prev_cat) cat_show <- ""
  prev_cat <- wide$category[r]
  line <- sprintf(paste0("%-", w_cat, "s %-", w_stat, "s"), substr(cat_show, 1, w_cat), substr(wide$stat[r], 1, w_stat))
  for (t in trt_levels) {
    v <- wide[[t]][r]
    line <- paste0(line, sprintf(paste0(" %", w_trt, "s"), substr(v, 1, w_trt)))
  }
  writeLines(line, con)
}
writeLines("", con)
writeLines(foot_lines, con)
close(con)

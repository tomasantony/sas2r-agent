# P073 t_exposure.sas -> R translation
# Table 14.1.3 Summary of Study Drug Exposure (Safety Analysis Set)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

cat("P073 exposure: starting\n")

# ---- SAS semantics helpers ----
sas_round <- function(x, dec) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^dec + 0.5 + 1e-12) / 10^dec)
}
sas_fmt <- function(x, dec) {
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi)) return("")
    r <- sas_round(xi, dec)
    if (r == 0) r <- 0
    sprintf(paste0("%.", dec, "f"), r)
  }, character(1))
}
sas_byte_key <- function(v) {
  vapply(v, function(s) {
    if (is.na(s)) return("")
    paste(sprintf("%02x", as.integer(charToRaw(as.character(s)))), collapse = "")
  }, character(1))
}
# SAS PCTLDEF=5 (PROC MEANS default)
sas_pctl <- function(x, p) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) return(NA_real_)
  x <- sort(x)
  if (n == 1) return(x[1])
  np <- n * p
  j <- floor(np + 1e-12)
  g <- np - j
  if (g > 1e-9) {
    return(x[j + 1])
  } else {
    if (j <= 0) return(x[1])
    if (j >= n) return(x[n])
    return((x[j] + x[j + 1]) / 2)
  }
}

find_file <- function(dir, base) {
  if (!dir.exists(dir)) return(NULL)
  fs <- list.files(dir, full.names = TRUE)
  pat <- paste0("^", base, "\\.(sas7bdat|xpt)$")
  m <- fs[grepl(pat, basename(fs), ignore.case = TRUE)]
  if (length(m) == 0) return(NULL)
  sas <- m[grepl("\\.sas7bdat$", m, ignore.case = TRUE)]
  if (length(sas) > 0) return(sas[1])
  return(m[1])
}
read_sas_any <- function(f) {
  if (grepl("\\.xpt$", f, ignore.case = TRUE)) haven::read_xpt(f)
  else haven::read_sas(f)
}
to_date <- function(v) {
  if (inherits(v, "Date")) return(as.Date(v))
  if (inherits(v, "POSIXct") || inherits(v, "POSIXt")) return(as.Date(v))
  # numeric SAS-date-like or Date-like numeric: return NA handling elsewhere
  return(NULL)
}

# ---- read ADSL ----
f_adsl <- find_file(adam_dir, "adsl")
if (is.null(f_adsl)) stop("ADSL not found in ", adam_dir)
cat("Reading ", f_adsl, "\n")
adsl_raw <- read_sas_any(f_adsl)
adsl_raw <- haven::zap_labels(adsl_raw)
adsl_raw <- haven::zap_formats(adsl_raw)
names(adsl_raw) <- toupper(names(adsl_raw))
adsl <- as.data.frame(adsl_raw, stringsAsFactors = FALSE)

for (cc in c("USUBJID","TRTA","SAFFL")) {
  if (cc %in% names(adsl)) {
    adsl[[cc]] <- trimws(as.character(adsl[[cc]]))
  }
}

# ---- Step 1: Big N (m_counts: upcase(SAFFL)='Y', order by TRTA byte order) ----
adsl_saf_big <- adsl %>% filter(!is.na(SAFFL) & toupper(SAFFL) == "Y")
bign_df <- adsl_saf_big %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
bign_df <- bign_df[order(sas_byte_key(bign_df$TRTA)), , drop = FALSE]
bigntot <- n_distinct(adsl_saf_big$USUBJID)
print(bign_df)
cat("Total:", bigntot, "\n")

trt_levels_sorted <- bign_df$TRTA
idx_placebo <- grep("Placebo", trt_levels_sorted, fixed = TRUE)
idx_low <- grep("Low", trt_levels_sorted, fixed = TRUE)
idx_high <- grep("High", trt_levels_sorted, fixed = TRUE)
if (length(idx_placebo)==1 && length(idx_low)==1 && length(idx_high)==1) {
  trt_col1 <- trt_levels_sorted[idx_placebo]
  trt_col2 <- trt_levels_sorted[idx_low]
  trt_col3 <- trt_levels_sorted[idx_high]
} else {
  trt_col1 <- trt_levels_sorted[1]
  trt_col2 <- ifelse(length(trt_levels_sorted)>=2, trt_levels_sorted[2], NA)
  trt_col3 <- ifelse(length(trt_levels_sorted)>=3, trt_levels_sorted[3], NA)
}
get_bign <- function(t) {
  v <- bign_df$bign[bign_df$TRTA==t]
  if (length(v)==0) return(0L)
  v[1]
}
bign1 <- get_bign(trt_col1); bign2 <- get_bign(trt_col2); bign3 <- get_bign(trt_col3)
col1hdr <- paste0(trt_col1, " (N=", bign1, ")")
col2hdr <- paste0(trt_col2, " (N=", bign2, ")")
col3hdr <- paste0(trt_col3, " (N=", bign3, ")")

# ---- Step 2 alt: ex_adsl (where SAFFL='Y' case-sensitive) ----
ex_adsl <- adsl %>% filter(!is.na(SAFFL) & SAFFL == "Y")

# TRTDUR = TRTEDT - TRTSDT + 1  (date arithmetic in days, SAS semantics)
# Handle Date / POSIXct / numeric SAS dates
sdt_raw <- ex_adsl$TRTSDT
edt_raw <- ex_adsl$TRTEDT
if (inherits(sdt_raw, "Date") && inherits(edt_raw, "Date")) {
  sdt_d <- as.Date(sdt_raw); edt_d <- as.Date(edt_raw)
  ex_adsl$TRTDUR <- as.numeric(edt_d - sdt_d) + 1
  ex_adsl$TRTDUR[is.na(sdt_d) | is.na(edt_d)] <- NA_real_
} else if (inherits(sdt_raw, "POSIXct") || inherits(sdt_raw, "POSIXt") ||
           inherits(edt_raw, "POSIXct") || inherits(edt_raw, "POSIXt")) {
  sdt_d <- as.Date(sdt_raw); edt_d <- as.Date(edt_raw)
  ex_adsl$TRTDUR <- as.numeric(edt_d - sdt_d) + 1
  ex_adsl$TRTDUR[is.na(sdt_d) | is.na(edt_d)] <- NA_real_
} else {
  sdt_n <- suppressWarnings(as.numeric(sdt_raw))
  edt_n <- suppressWarnings(as.numeric(edt_raw))
  ex_adsl$TRTDUR <- ifelse(!is.na(sdt_n) & !is.na(edt_n), edt_n - sdt_n + 1, NA_real_)
}
# SAS: if TRTEDT ne . and TRTSDT ne . then TRTDUR=... else .
# already NA where missing

# ---- Step 3: Duration statistics (m_cont_stats decimals=1) ----
dp <- 1; dp1 <- 2
trts <- c(trt_col1, trt_col2, trt_col3)
stat_rows <- list()
for (t in trts) {
  x <- ex_adsl$TRTDUR[ex_adsl$TRTA==t]
  xnm <- x[!is.na(x)]
  n <- length(xnm)
  if (n > 0) {
    mn <- mean(xnm)
    sdv <- if (n > 1) stats::sd(xnm) else NA_real_
    med <- sas_pctl(xnm, 0.5)
    q1 <- sas_pctl(xnm, 0.25)
    q3 <- sas_pctl(xnm, 0.75)
    mnv <- min(xnm); mxv <- max(xnm)
  } else {
    mn <- NA_real_; sdv <- NA_real_; med <- NA_real_
    q1 <- NA_real_; q3 <- NA_real_; mnv <- NA_real_; mxv <- NA_real_
  }
  v_n <- as.character(n)
  v_mean <- if (n>0) sas_fmt(mn, dp1) else ""
  v_sd <- if (n>1) sas_fmt(sdv, dp1) else ""
  v_med <- if (n>0) sas_fmt(med, dp) else ""
  v_q <- if (n>0) paste0(sas_fmt(q1,dp), ", ", sas_fmt(q3,dp)) else ""
  v_mm <- if (n>0) paste0(sas_fmt(mnv,dp), ", ", sas_fmt(mxv,dp)) else ""
  df_t <- data.frame(
    TRTA = t,
    stat = c("n","Mean","SD","Median","Q1, Q3","Min, Max"),
    value = c(v_n, v_mean, v_sd, v_med, v_q, v_mm),
    ord = 1:6,
    stringsAsFactors = FALSE
  )
  stat_rows[[t]] <- df_t
}
dur_stats <- do.call(rbind, stat_rows)
dur_stats$category <- "Duration of Treatment (days)"
dur_stats$block <- 1

# ---- Step 4: Duration categories (exact SAS logic) ----
dur_cat <- ex_adsl
dur_cat$DURCAT <- ifelse(is.na(dur_cat$TRTDUR), "Missing",
  ifelse(dur_cat$TRTDUR <= 28, "<=28 days",
  ifelse(dur_cat$TRTDUR <= 84, ">28 to <=84 days",
  ifelse(dur_cat$TRTDUR <= 168, ">84 to <=168 days",
  ifelse(dur_cat$TRTDUR <= 336, ">168 to <=336 days", ">336 days")))))
dur_cat$DURCATN <- ifelse(dur_cat$DURCAT=="<=28 days", 1,
  ifelse(dur_cat$DURCAT==">28 to <=84 days", 2,
  ifelse(dur_cat$DURCAT==">84 to <=168 days", 3,
  ifelse(dur_cat$DURCAT==">168 to <=336 days", 4,
  ifelse(dur_cat$DURCAT==">336 days", 5,
  ifelse(dur_cat$DURCAT=="Missing", 6, 99))))))

# ---- Step 5: Duration category counts + complete zero grid ----
durcat_cnt <- dur_cat %>%
  group_by(TRTA, DURCAT, DURCATN) %>%
  summarise(cnt = n_distinct(USUBJID), .groups="drop") %>%
  as.data.frame()

all_cats <- data.frame(
  DURCAT = c("<=28 days", ">28 to <=84 days", ">84 to <=168 days",
             ">168 to <=336 days", ">336 days", "Missing"),
  DURCATN = 1:6, stringsAsFactors = FALSE
)
grid <- expand.grid(TRTA = trts, DURCATN = 1:6, stringsAsFactors = FALSE) %>%
  left_join(all_cats, by = "DURCATN") %>%
  left_join(durcat_cnt, by = c("TRTA","DURCAT","DURCATN")) %>%
  mutate(cnt = ifelse(is.na(cnt), 0L, cnt)) %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(pct = ifelse(!is.na(bign) & bign > 0 & cnt > 0, cnt / bign * 100, NA_real_))

durcat_fmt <- grid %>%
  mutate(
    category = "Duration Categories",
    stat = paste0("  ", trimws(DURCAT)),
    ord = 100 + DURCATN,
    block = 2,
    value = ifelse(!is.na(cnt) & cnt > 0,
      paste0(trimws(as.character(cnt)), " (", trimws(sas_fmt(pct, 1)), ")"),
      "0")
  ) %>%
  select(TRTA, category, stat, ord, block, value, DURCATN, DURCAT)

# ---- Step 6: combine ----
all_exp <- bind_rows(
  dur_stats %>% select(TRTA, category, stat, ord, block, value),
  durcat_fmt %>% select(TRTA, category, stat, ord, block, value)
)
all_exp <- all_exp[order(all_exp$block, all_exp$ord, sas_byte_key(all_exp$stat)), , drop=FALSE]

# ---- Step 7: transpose (BY block category ord stat; ID TRTA) ----
final_w <- all_exp %>%
  select(block, category, ord, stat, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value)

final <- data.frame(block=final_w$block, category=final_w$category,
  ord=final_w$ord, stat=final_w$stat, stringsAsFactors=FALSE)
pull_trt <- function(df, t) {
  if (!is.null(t) && !is.na(t) && t %in% names(df)) df[[t]] else rep(NA_character_, nrow(df))
}
final$col1 <- pull_trt(final_w, trt_col1)
final$col2 <- pull_trt(final_w, trt_col2)
final$col3 <- pull_trt(final_w, trt_col3)
# SAS displays missing transposed cells as blank; keep NA for CSV blank,
# but zero-count rows already "0". Convert remaining NA (should not occur now) to ""
# Keep NA so write.csv na="" gives blank like SAS blank
final <- final[order(final$block, final$ord), , drop=FALSE]
rownames(final) <- NULL
print(as.data.frame(final))

# ---- Step 8: outputs ----
write.csv(final, file.path(out, "t_exposure.csv"), row.names=FALSE, na="")

txt <- file.path(out, "t_exposure.txt")
con <- file(txt, open="wt")
writeLines("[Sponsor Name]                                                  Page 1", con)
writeLines("Protocol: [Protocol Number]", con)
writeLines("", con)
writeLines("Table 14.1.3", con)
writeLines("Summary of Study Drug Exposure", con)
writeLines("Safety Analysis Set", con)
writeLines("", con)
h1 <- sprintf("%-32s | %-18s | %-18s | %-18s", "Statistic", col1hdr, col2hdr, col3hdr)
writeLines(h1, con)
writeLines(paste0(strrep("-", 34), "+", strrep("-", 20), "+", strrep("-", 20), "+", strrep("-", 20)), con)
cur_cat <- ""
for (i in seq_len(nrow(final))) {
  if (final$category[i] != cur_cat) {
    cur_cat <- final$category[i]
    writeLines(paste0(cur_cat), con)
  }
  c1 <- ifelse(is.na(final$col1[i]), "", final$col1[i])
  c2 <- ifelse(is.na(final$col2[i]), "", final$col2[i])
  c3 <- ifelse(is.na(final$col3[i]), "", final$col3[i])
  line <- sprintf("%-32s | %-18s | %-18s | %-18s", final$stat[i], c1, c2, c3)
  writeLines(line, con)
}
writeLines("", con)
writeLines("Data cutoff: [DDMONYYYY].", con)
writeLines("Source: ADaM datasets", con)
writeLines(paste0("Program: t_exposure.sas | Run: ", format(Sys.Date(), "%d%b%Y")), con)
close(con)
cat("Wrote", file.path(out, "t_exposure.csv"), "and", txt, "\n")

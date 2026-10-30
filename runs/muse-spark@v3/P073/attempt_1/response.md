```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rhau <- function(x, d) {
  # SAS round half away from zero
  s <- sign(x)
  ax <- abs(x)
  f <- 10^d
  y <- floor(ax * f + 0.5 + 1e-9) / f
  s * y
}
fmt1 <- function(x) sprintf("%.1f", rhau(x, 1))
fmt2 <- function(x) sprintf("%.2f", rhau(x, 2))
fmt_pct1 <- function(x) sprintf("%.1f", rhau(x, 1))

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

find_adam_file <- function(base, name) {
  # try various extensions / cases
  cand1 <- file.path(base, paste0(name, ".sas7bdat"))
  cand2 <- file.path(base, paste0(name, ".xpt"))
  cand3 <- file.path(base, paste0(tolower(name), ".sas7bdat"))
  cand4 <- file.path(base, paste0(tolower(name), ".xpt"))
  cand5 <- file.path(base, paste0(toupper(name), ".sas7bdat"))
  cand6 <- file.path(base, paste0(toupper(name), ".xpt"))
  for (c in c(cand1,cand2,cand3,cand4,cand5,cand6)) if (file.exists(c)) return(c)
  # list dir
  if (dir.exists(base)) {
    fl <- list.files(base, full.names = TRUE)
    idx <- grep(paste0("(?i)", name, "\\.(sas7bdat|xpt)$"), fl, perl = TRUE)
    if (length(idx) > 0) return(fl[idx[1]])
  }
  return(NA)
}
read_sas_any <- function(f) {
  if (is.na(f)) stop(paste("file not found"))
  if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
    df <- haven::read_xpt(f)
  } else {
    df <- haven::read_sas(f)
  }
  # convert labelled to plain, keep names upper
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  df
}

adsl_file <- find_adam_file(adam_path, "adsl")
adsl <- read_sas_any(adsl_file)

# Ensure needed columns exist
stopifnot(all(c("USUBJID","TRTA","SAFFL") %in% names(adsl)))

# TRTAN if present
has_trtan <- "TRTAN" %in% names(adsl)

# Helper to convert SAS date-ish to Date
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS date origin 1960-01-01
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    # single strict format: ISO first; if fails try DATE9
    d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    if (all(is.na(d[!is.na(x) & x!=""]))) {
      d2 <- suppressWarnings(as.Date(toupper(x), format = "%d%b%Y"))
      # also try %d-%b-%Y
      need <- is.na(d) & !is.na(x) & x!=""
      d3 <- suppressWarnings(as.Date(toupper(x), format = "%d-%b-%Y"))
      d[need] <- d3[need]
      need2 <- is.na(d) & !is.na(x) & x!=""
      d[need2] <- d2[need2]
      return(d)
    }
    return(d)
  }
  return(as.Date(x, origin = "1960-01-01"))
}

# Step 1: Big N (SAFFL='Y'), order by TRTA character (SAS sorts uppercase first = byte order)
adsl_saf <- adsl[toupper(adsl$SAFFL) == "Y", , drop = FALSE]
stopifnot(nrow(adsl_saf) > 0)

# distinct subjects per TRTA
bign_df <- aggregate(USUBJID ~ TRTA, data = adsl_saf,
  FUN = function(v) length(unique(v)))
names(bign_df)[2] <- "BIGN"
# SAS ORDER BY TRTA : byte order
bign_df <- bign_df[order(bign_df$TRTA), , drop = FALSE]

bign_tot <- length(unique(adsl_saf$USUBJID))

# Determine display order: by TRTAN if available (to get Placebo, Low, High)
trt_order <- NULL
if (has_trtan) {
  map <- unique(adsl[, c("TRTA","TRTAN")])
  map <- map[order(map$TRTAN, map$TRTA), , drop = FALSE]
  trt_order <- map$TRTA
  # ensure all bign trts included
  extra <- setdiff(bign_df$TRTA, trt_order)
  if (length(extra) > 0) trt_order <- c(trt_order, sort(extra))
} else {
  trt_order <- bign_df$TRTA  # already sorted
}
# Reorder bign_df to trt_order for columns col1..col3
bign_df <- bign_df[match(trt_order, bign_df$TRTA), , drop = FALSE]

bign_vec <- setNames(bign_df$BIGN, bign_df$TRTA)
col_hdrs <- paste0(bign_df$TRTA, " (N=", bign_df$BIGN, ")")
names(col_hdrs) <- paste0("col", seq_along(col_hdrs))

# Step 2 alt: ex_adsl with TRTDUR
adsl_saf$TRTSDT_D <- to_date(adsl_saf$TRTSDT)
adsl_saf$TRTEDT_D <- to_date(adsl_saf$TRTEDT)
adsl_saf$TRTDUR <- ifelse(!is.na(adsl_saf$TRTSDT_D) & !is.na(adsl_saf$TRTEDT_D),
  as.numeric(adsl_saf$TRTEDT_D - adsl_saf$TRTSDT_D) + 1, NA_real_)

ex_adsl <- adsl_saf

# Step 3: continuous stats mimicking PROC MEANS (PCTLDEF=5 -> R type 2)
cont_list <- list()
for (tr in trt_order) {
  v <- ex_adsl$TRTDUR[ex_adsl$TRTA == tr]
  v <- v[!is.na(v)]
  n <- length(v)
  if (n > 0) {
    mn <- mean(v)
    sdv <- if (n > 1) stats::sd(v) else NA_real_
    med <- stats::median(v)
    q <- stats::quantile(v, probs = c(0.25, 0.75), type = 2, names = FALSE)
    q1 <- q[1]; q3 <- q[2]
    mi <- min(v); ma <- max(v)
  } else {
    mn <- sdv <- med <- q1 <- q3 <- mi <- ma <- NA_real_
  }
  cont_list[[tr]] <- data.frame(TRTA = tr, n = n, mean = mn, std = sdv,
    median = med, q1 = q1, q3 = q3, min = mi, max = ma, stringsAsFactors = FALSE)
}
stats_wide <- do.call(rbind, cont_list)

# Format to long _dur_stats
dur_stats_rows <- list()
stat_defs <- c("n","Mean","SD","Median","Q1, Q3","Min, Max")
for (i in seq_len(nrow(stats_wide))) {
  tr <- stats_wide$TRTA[i]
  n <- stats_wide$n[i]
  # n
  dur_stats_rows[[length(dur_stats_rows)+1]] <- data.frame(TRTA=tr, stat="n",
    value=as.character(n), ord=1, stringsAsFactors=FALSE)
  # Mean
  v <- if (n>0) fmt2(stats_wide$mean[i]) else ""
  dur_stats_rows[[length(dur_stats_rows)+1]] <- data.frame(TRTA=tr, stat="Mean",
    value=v, ord=2, stringsAsFactors=FALSE)
  # SD
  v <- if (n>1) fmt2(stats_wide$std[i]) else ""
  dur_stats_rows[[length(dur_stats_rows)+1]] <- data.frame(TRTA=tr, stat="SD",
    value=v, ord=3, stringsAsFactors=FALSE)
  # Median
  v <- if (n>0) fmt1(stats_wide$median[i]) else ""
  dur_stats_rows[[length(dur_stats_rows)+1]] <- data.frame(TRTA=tr, stat="Median",
    value=v, ord=4, stringsAsFactors=FALSE)
  # Q1, Q3
  v <- if (n>0) paste0(fmt1(stats_wide$q1[i]), ", ", fmt1(stats_wide$q3[i])) else ""
  dur_stats_rows[[length(dur_stats_rows)+1]] <- data.frame(TRTA=tr, stat="Q1, Q3",
    value=v, ord=5, stringsAsFactors=FALSE)
  # Min, Max
  v <- if (n>0) paste0(fmt1(stats_wide$min[i]), ", ", fmt1(stats_wide$max[i])) else ""
  dur_stats_rows[[length(dur_stats_rows)+1]] <- data.frame(TRTA=tr, stat="Min, Max",
    value=v, ord=6, stringsAsFactors=FALSE)
}
dur_stats <- do.call(rbind, dur_stats_rows)
dur_stats$category <- "Duration of Treatment (days)"
dur_stats$block <- 1

# Step 4: duration categories
dur_cat <- ex_adsl[, c("USUBJID","TRTA","TRTDUR"), drop = FALSE]
dur_cat$DURCAT <- ifelse(is.na(dur_cat$TRTDUR), "Missing",
  ifelse(dur_cat$TRTDUR <= 28, "<=28 days",
  ifelse(dur_cat$TRTDUR <= 84, ">28 to <=84 days",
  ifelse(dur_cat$TRTDUR <= 168, ">84 to <=168 days",
  ifelse(dur_cat$TRTDUR <= 336, ">168 to <=336 days", ">336 days")))))
dur_cat$DURCATN <- ifelse(dur_cat$DURCAT=="<=28 days",1,
  ifelse(dur_cat$DURCAT==">28 to <=84 days",2,
  ifelse(dur_cat$DURCAT==">84 to <=168 days",3,
  ifelse(dur_cat$DURCAT==">168 to <=336 days",4,
  ifelse(dur_cat$DURCAT==">336 days",5,
  ifelse(dur_cat$DURCAT=="Missing",6,99))))))

# Step 5: counts
# count distinct USUBJID per TRTA,DURCAT,DURCATN
uniq_dc <- unique(dur_cat[, c("USUBJID","TRTA","DURCAT","DURCATN")])
cnt_df <- aggregate(USUBJID ~ TRTA + DURCAT + DURCATN, data = uniq_dc, FUN = length)
names(cnt_df)[4] <- "CNT"
# full grid to show zeros as "0" (fill missing combos)
all_cats <- unique(dur_cat[, c("DURCAT","DURCATN")])
all_cats <- all_cats[order(all_cats$DURCATN), , drop = FALSE]
grid <- expand.grid(TRTA = trt_order, DURCAT = all_cats$DURCAT, stringsAsFactors = FALSE)
grid <- merge(grid, all_cats, by = "DURCAT")
cnt_full <- merge(grid, cnt_df, by = c("TRTA","DURCAT","DURCATN"), all.x = TRUE)
cnt_full$CNT[is.na(cnt_full$CNT)] <- 0
cnt_full$BIGN <- bign_vec[cnt_full$TRTA]
cnt_full$PCT <- ifelse(cnt_full$BIGN > 0, cnt_full$CNT / cnt_full$BIGN * 100, NA_real_)
cnt_full <- cnt_full[order(cnt_full$DURCATN, match(cnt_full$TRTA, trt_order)), , drop = FALSE]

durcat_fmt_rows <- list()
for (i in seq_len(nrow(cnt_full))) {
  cnt <- cnt_full$CNT[i]; pct <- cnt_full$PCT[i]
  if (cnt > 0) val <- paste0(as.character(cnt), " (", fmt_pct1(pct), ")")
  else val <- "0"
  durcat_fmt_rows[[i]] <- data.frame(TRTA = cnt_full$TRTA[i],
    category = "Duration Categories",
    stat = paste0("  ", cnt_full$DURCAT[i]),
    value = val, ord = 100 + cnt_full$DURCATN[i], block = 2,
    DURCATN = cnt_full$DURCATN[i], stringsAsFactors = FALSE)
}
durcat_fmt <- do.call(rbind, durcat_fmt_rows)

# Step 6 combine
all_exp <- rbind(
  dur_stats[, c("TRTA","category","stat","value","ord","block")],
  durcat_fmt[, c("TRTA","category","stat","value","ord","block")]
)
all_exp <- all_exp[order(all_exp$block, all_exp$ord, all_exp$stat, match(all_exp$TRTA, trt_order)), , drop = FALSE]

# Step 7 transpose -> pivot wider
# Use tidyr
library(tidyr)
final_w <- tidyr::pivot_wider(all_exp, id_cols = c(block, category, ord, stat),
  names_from = TRTA, values_from = value)
# Ensure columns in trt_order
for (tr in trt_order) if (!(tr %in% names(final_w))) final_w[[tr]] <- NA_character_
# order rows
final_w <- final_w[order(final_w$block, final_w$ord, final_w$stat), , drop = FALSE]
# rename to col1..col3 in trt_order
colnames_map <- setNames(paste0("col", seq_along(trt_order)), trt_order)
final <- as.data.frame(final_w, stringsAsFactors = FALSE, check.names = FALSE)
for (tr in trt_order) {
  final[[colnames_map[[tr]]]] <- final[[tr]]
  final[[tr]] <- NULL
}
# fill missing transposed cells with "0" for category block, "" for stats? SAS would be blank.
# For dur categories missing -> "0"; for stats missing shouldn't happen.
for (cc in paste0("col", seq_along(trt_order))) {
  idx_cat <- final$block == 2 & is.na(final[[cc]])
  final[[cc]][idx_cat] <- "0"
  final[[cc]][is.na(final[[cc]])] <- ""
}
# Ensure col1..col3 exist (ntrt=3)
for (k in 1:3) {
  cn <- paste0("col", k)
  if (!(cn %in% names(final))) final[[cn]] <- ""
}
final <- final[order(final$block, final$ord), , drop = FALSE]

# Build display CSV
final_csv <- final[, c("category","stat","col1","col2","col3")]
# Header names for CSV: keep Statistic etc? Use SAS-like names
names(final_csv) <- c("Category","Statistic", col_hdrs[1], col_hdrs[2], col_hdrs[3])
# If only 2 trts? handle generically
if (length(trt_order) != 3) {
  # generic
  final_csv <- final[, c("category","stat", paste0("col", seq_along(trt_order)))]
  names(final_csv) <- c("Category","Statistic", col_hdrs)
}

write.csv(final_csv, file.path(out, "t_exposure.csv"), row.names = FALSE, na = "")

# Text rendering
titles <- c(
  "[Sponsor Name]",
  "Protocol: [Protocol Number]",
  "Table 14.1.3",
  "Summary of Study Drug Exposure",
  "Safety Analysis Set"
)
footnotes <- c(
  "Data cutoff: [DDMONYYYY].",
  "Source: ADaM datasets",
  "Program: t_exposure.sas"
)

# Build fixed-width text
# Column widths
w_cat <- 32; w_stat <- 22; w_c <- 22
hdr1 <- sprintf(paste0("%-", w_cat, "s%-", w_stat, "s"), "", "Statistic")
for (k in seq_along(col_hdrs)) {
  hdr1 <- paste0(hdr1, sprintf(paste0("%", w_c, "s"), col_hdrs[k]))
}
sep <- paste(rep("-", nchar(hdr1)), collapse = "")
lines <- c(titles, "", hdr1, sep)
prev_cat <- ""
for (i in seq_len(nrow(final))) {
  cat_show <- final$category[i]
  # mimic ORDER: show category only on first row of each category value
  if (cat_show == prev_cat) cat_disp <- ""
  else cat_disp <- cat_show
  prev_cat <- cat_show
  # truncate/pad
  c1 <- substr(cat_disp, 1, w_cat)
  s1 <- substr(final$stat[i], 1, w_stat)
  line <- sprintf(paste0("%-", w_cat, "s%-", w_stat, "s"), c1, s1)
  for (k in 1:length(trt_order)) {
    v <- final[[paste0("col", k)]][i]
    if (is.na(v)) v <- ""
    v <- substr(v, 1, w_c)
    line <- paste0(line, sprintf(paste0("%", w_c, "s"), v))
  }
  lines <- c(lines, line)
  # add blank line between blocks? keep category header bold separation
}
lines <- c(lines, "", footnotes)

writeLines(lines, file.path(out, "t_exposure.txt"))
```
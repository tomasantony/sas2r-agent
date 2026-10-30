out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

find_file <- function(dir, base) {
  if (!dir.exists(dir)) return(NULL)
  fs <- list.files(dir, full.names = TRUE, recursive = FALSE)
  # match basename case-insensitive without extension
  pat <- tolower(base)
  for (f in fs) {
    bn <- tolower(tools::file_path_sans_ext(basename(f)))
    if (bn == pat) return(f)
  }
  # try recursive
  fs2 <- list.files(dir, full.names = TRUE, recursive = TRUE)
  for (f in fs2) {
    bn <- tolower(tools::file_path_sans_ext(basename(f)))
    if (bn == pat) return(f)
  }
  return(NULL)
}

adsl_file <- find_file(adam_dir, "adsl")
if (is.null(adsl_file)) {
  # fallback: any file containing adsl
  fs <- list.files(adam_dir, full.names = TRUE, recursive = TRUE)
  cand <- fs[grepl("adsl", tolower(basename(fs)))]
  if (length(cand) > 0) adsl_file <- cand[1] else stop("ADSL not found")
}
message("Reading ADSL: ", adsl_file)
ext <- tolower(tools::file_ext(adsl_file))
if (ext == "xpt") {
  adsl_raw <- haven::read_xpt(adsl_file)
} else if (ext %in% c("sas7bdat", "sas7bat")) {
  adsl_raw <- haven::read_sas(adsl_file)
} else if (ext == "csv") {
  adsl_raw <- readr::read_csv(adsl_file, show_col_types = FALSE)
} else {
  # try sas
  adsl_raw <- haven::read_sas(adsl_file)
}
adsl_raw <- as.data.frame(adsl_raw, stringsAsFactors = FALSE)
# normalize names to uppercase for processing
names(adsl_raw) <- toupper(names(adsl_raw))
message(paste("ADSL cols:", paste(names(adsl_raw), collapse=",")))
stopifnot(all(c("USUBJID","TRTA","SAFFL") %in% names(adsl_raw)))
stopifnot("AGE" %in% names(adsl_raw))
stopifnot("SEX" %in% names(adsl_raw))
stopifnot("RACE" %in% names(adsl_raw))
stopifnot("ETHNIC" %in% names(adsl_raw))

# helpers ---------------------------------------------------------------
round_half_up <- function(x, digits) {
  mult <- 10^digits
  # vectorized
  res <- rep(NA_real_, length(x))
  ok <- !is.na(x) & is.finite(x)
  xx <- x[ok]
  s <- sign(xx)
  # SAS: half away from zero
  res[ok] <- s * floor(abs(xx) * mult + 0.5 + 1e-9) / mult
  res[res == 0 & !is.na(res)] <- 0
  res
}
fmt_fixed <- function(x, digits) {
  sapply(seq_along(x), function(i) {
    v <- x[i]
    if (is.na(v)) return("")
    r <- round_half_up(v, digits)
    sprintf(paste0("%.", digits, "f"), r)
  }, USE.NAMES = FALSE)
}
fmt_int <- function(x) {
  sapply(seq_along(x), function(i) {
    v <- x[i]
    if (is.na(v)) return("")
    formatC(as.integer(v), format = "d")
  }, USE.NAMES = FALSE)
}
sas_quantile <- function(x, p) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n == 0) return(NA_real_)
  if (n == 1) return(x[1])
  k <- n * p
  j <- floor(k + 1e-12)
  g <- k - j
  eps <- 1e-9
  if (abs(g) < eps) {
    if (j < 1) return(x[1])
    if (j >= n) return(x[n])
    return((x[j] + x[j+1]) / 2)
  } else {
    idx <- ceiling(k - 1e-12)
    idx <- max(1, min(n, idx))
    return(x[idx])
  }
}
ascii_key <- function(v) {
  sapply(v, function(s) {
    if (is.na(s)) return("")
    s2 <- as.character(s)
    if (is.na(s2)) return("")
    if (s2 == "") return(" ")
    codes <- utf8ToInt(s2)
    paste(sprintf("%03d", codes), collapse = "-")
  }, USE.NAMES = FALSE)
}
ascii_order <- function(v) {
  k <- ascii_key(v)
  order(k, method = "radix")
}

# Step 1: Big N (m_counts, popflag SAFFL=Y case-insensitive) ------------
adsl_raw$SAFFL_UP <- toupper(as.character(adsl_raw$SAFFL))
bign_df <- aggregate(USUBJID ~ TRTA, data = adsl_raw[toupper(adsl_raw$SAFFL) == "Y", , drop = FALSE],
                     FUN = function(z) length(unique(z)))
names(bign_df)[names(bign_df) == "USUBJID"] <- "BIGN"
# SAS order by TRTA (ascii, missing first)
bign_df <- bign_df[ascii_order(bign_df$TRTA), , drop = FALSE]
bign_tot <- length(unique(adsl_raw$USUBJID[toupper(adsl_raw$SAFFL) == "Y"]))
message(paste("BigN:", paste(bign_df$TRTA, bign_df$BIGN, collapse="; "), "Total", bign_tot))

# Step 2: dm (where SAFFL='Y' case-sensitive) -----------------------------
dm <- adsl_raw[adsl_raw$SAFFL == "Y" & !is.na(adsl_raw$SAFFL), , drop = FALSE]
stopifnot("SAFFL" %in% names(dm))
stopifnot(nrow(dm) > 0)
# derive AGEGR1
dm$AGEGR1 <- NA_character_
dm$AGEGR1[!is.na(dm$AGE) & dm$AGE < 18] <- "<18"
dm$AGEGR1[!is.na(dm$AGE) & dm$AGE >= 18 & dm$AGE < 65] <- "18 to <65"
dm$AGEGR1[!is.na(dm$AGE) & dm$AGE >= 65] <- ">=65"
# SEXN RACEN ETHNICN (for completeness)
dm$SEXN <- NA_real_
dm$SEXN[dm$SEX == "M"] <- 1
dm$SEXN[dm$SEX == "F"] <- 2
dm$RACEN <- 4
dm$RACEN[dm$RACE == "WHITE"] <- 1
dm$RACEN[dm$RACE == "BLACK OR AFRICAN AMERICAN"] <- 2
dm$RACEN[dm$RACE == "ASIAN"] <- 3
dm$ETHNICN <- 3
dm$ETHNICN[dm$ETHNIC == "HISPANIC OR LATINO"] <- 1
dm$ETHNICN[dm$ETHNIC == "NOT HISPANIC OR LATINO"] <- 2

# display order for columns: Placebo, Low, High --------------------------
uniq_trta <- unique(dm$TRTA)
uniq_trta <- uniq_trta[!is.na(uniq_trta)]
placebo_val <- uniq_trta[tolower(uniq_trta) == "placebo"]
if (length(placebo_val) == 0) {
  idx <- grep("placebo", uniq_trta, ignore.case = TRUE)
  if (length(idx) > 0) placebo_val <- uniq_trta[idx[1]] else placebo_val <- NA_character_
} else placebo_val <- placebo_val[1]
low_cand <- uniq_trta[grepl("Low", uniq_trta, fixed = TRUE)]
if (length(low_cand) == 0) low_cand <- uniq_trta[grepl("low", uniq_trta, ignore.case = TRUE)]
low_val <- if (length(low_cand) > 0) low_cand[1] else NA_character_
high_cand <- uniq_trta[grepl("High", uniq_trta, fixed = TRUE)]
if (length(high_cand) == 0) high_cand <- uniq_trta[grepl("high", uniq_trta, ignore.case = TRUE)]
high_val <- if (length(high_cand) > 0) high_cand[1] else NA_character_
display_order <- c(placebo_val, low_val, high_val)
display_order <- display_order[!is.na(display_order)]
remaining <- setdiff(uniq_trta, display_order)
if (length(remaining) > 0) {
  remaining <- remaining[ascii_order(remaining)]
  display_order <- c(display_order, remaining)
}
# ensure 3
message(paste("Display order:", paste(display_order, collapse=" | ")))
stopifnot(length(display_order) >= 1)
# pad to 3 if needed (should be 3)
while (length(display_order) < 3) display_order <- c(display_order, NA_character_)
col1_trt <- display_order[1]; col2_trt <- display_order[2]; col3_trt <- display_order[3]
get_bign <- function(trt) {
  if (is.na(trt)) return(0)
  v <- bign_df$BIGN[bign_df$TRTA == trt]
  if (length(v) == 0) return(0)
  v[1]
}
bign1 <- get_bign(col1_trt); bign2 <- get_bign(col2_trt); bign3 <- get_bign(col3_trt)
hdr1 <- paste0(col1_trt, " (N=", bign1, ")")
hdr2 <- paste0(col2_trt, " (N=", bign2, ")")
hdr3 <- paste0(col3_trt, " (N=", bign3, ")")

# Step 3: continuous ------------------------------------------------------
cont_vars <- list(
  list(var="AGE", label="Age (years)", block=1, dec=1),
  list(var="WEIGHTBL", label="Weight (kg)", block=5, dec=1),
  list(var="HEIGHTBL", label="Height (cm)", block=6, dec=1),
  list(var="BMIBL", label="BMI (kg/m2)", block=7, dec=1)
)
# keep original BMI label escape? use plain m2; numbers same
cont_rows <- list()
for (cv in cont_vars) {
  vname <- cv$var; lbl <- cv$label; blk <- cv$block; dec <- cv$dec
  stopifnot(vname %in% names(dm))
  # compute per TRTA
  for (trt in display_order[!is.na(display_order)]) {
    vals <- dm[[vname]][dm$TRTA == trt]
    vals <- vals[!is.na(vals)]
    n <- length(vals)
    if (n > 0) {
      mn <- mean(vals)
      sdv <- if (n > 1) stats::sd(vals) else NA_real_
      med <- sas_quantile(vals, 0.5)
      q1 <- sas_quantile(vals, 0.25)
      q3 <- sas_quantile(vals, 0.75)
      mnv <- min(vals); mxv <- max(vals)
    } else {
      mn <- sdv <- med <- q1 <- q3 <- mnv <- mxv <- NA_real_
    }
    # format
    # n
    cont_rows[[length(cont_rows)+1]] <- data.frame(block=blk, category=lbl, ord=1, stat="n",
      TRTA=trt, value=fmt_int(n), stringsAsFactors=FALSE)
    cont_rows[[length(cont_rows)+1]] <- data.frame(block=blk, category=lbl, ord=2, stat="Mean",
      TRTA=trt, value=if (n>0) fmt_fixed(mn, dec+1) else "", stringsAsFactors=FALSE)
    cont_rows[[length(cont_rows)+1]] <- data.frame(block=blk, category=lbl, ord=3, stat="SD",
      TRTA=trt, value=if (n>1) fmt_fixed(sdv, dec+1) else "", stringsAsFactors=FALSE)
    cont_rows[[length(cont_rows)+1]] <- data.frame(block=blk, category=lbl, ord=4, stat="Median",
      TRTA=trt, value=if (n>0) fmt_fixed(med, dec) else "", stringsAsFactors=FALSE)
    q1s <- if (n>0) fmt_fixed(q1, dec) else ""
    q3s <- if (n>0) fmt_fixed(q3, dec) else ""
    qval <- if (n>0) paste0(q1s, ", ", q3s) else ""
    cont_rows[[length(cont_rows)+1]] <- data.frame(block=blk, category=lbl, ord=5, stat="Q1, Q3",
      TRTA=trt, value=qval, stringsAsFactors=FALSE)
    mns <- if (n>0) fmt_fixed(mnv, dec) else ""
    mxs <- if (n>0) fmt_fixed(mxv, dec) else ""
    mmval <- if (n>0) paste0(mns, ", ", mxs) else ""
    cont_rows[[length(cont_rows)+1]] <- data.frame(block=blk, category=lbl, ord=6, stat="Min, Max",
      TRTA=trt, value=mmval, stringsAsFactors=FALSE)
  }
}
cont_df <- do.call(rbind, cont_rows)

# Step 4: categorical -----------------------------------------------------
cat_vars <- list(
  list(var="SEX", label="Sex", block=2),
  list(var="RACE", label="Race", block=3),
  list(var="ETHNIC", label="Ethnicity", block=4),
  list(var="AGEGR1", label="Age Group", block=8)
)
cat_rows <- list()
for (cv in cat_vars) {
  vname <- cv$var; lbl <- cv$label; blk <- cv$block
  stopifnot(vname %in% names(dm))
  # distinct counts per TRTA x category (missing as "")
  tmp <- dm[, c("TRTA","USUBJID", vname), drop=FALSE]
  names(tmp)[3] <- "CAT"
  tmp$CAT[is.na(tmp$CAT)] <- ""
  tmp$CAT <- as.character(tmp$CAT)
  # count distinct USUBJID
  # use aggregate
  uniq <- unique(tmp)
  # count
  cnt <- aggregate(USUBJID ~ TRTA + CAT, data=uniq, FUN=length)
  names(cnt)[names(cnt)=="USUBJID"] <- "CNT"
  for (i in seq_len(nrow(cnt))) {
    trt <- cnt$TRTA[i]; catv <- cnt$CAT[i]; c <- cnt$CNT[i]
    b <- get_bign(trt)
    pct <- if (!is.na(b) && b>0) c / b * 100 else NA_real_
    pcts <- if (!is.na(pct)) fmt_fixed(pct, 1) else ""
    val <- if (c>0) paste0(fmt_int(c), " (", pcts, ")") else "0"
    cat_rows[[length(cat_rows)+1]] <- data.frame(block=blk, category=lbl, ord=NA_real_, stat=catv,
      TRTA=trt, value=val, stringsAsFactors=FALSE)
  }
}
cat_df <- do.call(rbind, cat_rows)

all_stats <- rbind(cont_df, cat_df)

# Step 6: sort by block ord stat (SAS order) ------------------------------
# ord missing first
all_stats$ord2 <- ifelse(is.na(all_stats$ord), -9999, all_stats$ord)
all_stats$stat_key <- ascii_key(all_stats$stat)
all_stats <- all_stats[order(all_stats$block, all_stats$ord2, all_stats$stat_key, method="radix"), , drop=FALSE]

# transpose to wide -------------------------------------------------------
# unique rows
keys <- unique(all_stats[, c("block","category","ord","ord2","stat","stat_key"), drop=FALSE])
keys <- keys[order(keys$block, keys$ord2, keys$stat_key, method="radix"), , drop=FALSE]
# build wide
wide <- keys
wide$col1 <- ""; wide$col2 <- ""; wide$col3 <- ""
for (i in seq_len(nrow(wide))) {
  b <- wide$block[i]; catl <- wide$category[i]; o <- wide$ord[i]; s <- wide$stat[i]
  sub <- all_stats[all_stats$block==b & all_stats$category==catl & all_stats$stat==s &
    ((is.na(all_stats$ord) & is.na(o)) | (!is.na(all_stats$ord) & !is.na(o) & all_stats$ord==o)), , drop=FALSE]
  for (j in seq_len(nrow(sub))) {
    trt <- sub$TRTA[j]; v <- sub$value[j]
    if (!is.na(col1_trt) && trt==col1_trt) wide$col1[i] <- v
    else if (!is.na(col2_trt) && trt==col2_trt) wide$col2[i] <- v
    else if (!is.na(col3_trt) && trt==col3_trt) wide$col3[i] <- v
  }
}
# Step 7: report dataset --------------------------------------------------
# display_cat only first row of block/category
wide <- wide[order(wide$block, wide$ord2, wide$stat_key, method="radix"), , drop=FALSE]
wide$display_cat <- ""
prev_cat <- NA_character_; prev_block <- NA_real_
for (i in seq_len(nrow(wide))) {
  cur <- paste(wide$block[i], wide$category[i])
  prv <- paste(prev_block, prev_cat)
  if (is.na(prev_cat) || cur != prv) wide$display_cat[i] <- wide$category[i]
  else wide$display_cat[i] <- ""
  prev_cat <- wide$category[i]; prev_block <- wide$block[i]
}
wide$stat_disp <- paste0("  ", trimws(wide$stat))
report <- data.frame(block=wide$block, display_cat=wide$display_cat, stat=wide$stat_disp,
  col1=wide$col1, col2=wide$col2, col3=wide$col3,
  category=wide$category, stat_raw=wide$stat, ord=wide$ord, stringsAsFactors=FALSE)

# titles/footnotes --------------------------------------------------------
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
run_date <- format(Sys.Date(), "%d%b%Y")
run_date <- toupper(run_date)
run_time <- format(Sys.time(), "%H:%M")
title1 <- paste0(sponsor, "                                                  Page 1 of 1")
title2 <- paste0("Protocol: ", protocol)
title4 <- "Table 14.1.1"
title5 <- "Summary of Demographics and Baseline Characteristics"
title6 <- "Safety Analysis Set"
foot1 <- paste0("Data cutoff: ", cutdate, ".")
foot2 <- "Source: ADaM datasets"
foot3 <- paste0("Program: P072_t_dm.sas | Run: ", run_date, " ", run_time)

# write CSV of table body -------------------------------------------------
csv_df <- data.frame(Parameter=report$display_cat, Statistic=trimws(report$stat),
  stringsAsFactors=FALSE)
# use headers with N as column names
names(csv_df) <- c("Parameter","Statistic")
csv_df[[hdr1]] <- report$col1
csv_df[[hdr2]] <- report$col2
csv_df[[hdr3]] <- report$col3
# for CSV file, keep Parameter as display_cat but fill? Keep as in report (blank repeats)
write.csv(csv_df, file.path(out, "t_dm.csv"), row.names=FALSE, na="")

# write TXT rendering -----------------------------------------------------
con <- file(file.path(out, "t_dm.txt"), open="wt")
writeLines(title1, con)
writeLines(title2, con)
writeLines("", con)
writeLines(title4, con)
writeLines(title5, con)
writeLines(title6, con)
writeLines("", con)
# header row
hw1 <- 28; hw2 <- 16; hw3 <- 20
header_line <- sprintf(paste0("%-", hw1, "s %-", hw2, "s %-", hw3, "s %-", hw3, "s %-", hw3, "s"),
  "Parameter", "Statistic", hdr1, hdr2, hdr3)
writeLines(header_line, con)
writeLines(paste(rep("-", nchar(header_line)), collapse=""), con)
cur_block <- NA
for (i in seq_len(nrow(report))) {
  line <- sprintf(paste0("%-", hw1, "s %-", hw2, "s %-", hw3, "s %-", hw3, "s %-", hw3, "s"),
    substr(report$display_cat[i],1,hw1), substr(report$stat[i],1,hw2),
    substr(report$col1[i],1,hw3), substr(report$col2[i],1,hw3), substr(report$col3[i],1,hw3))
  writeLines(line, con)
  # compute after block: blank line when block changes
  nb <- if (i < nrow(report)) report$block[i+1] else NA
  if (!is.na(nb) && nb != report$block[i]) writeLines("", con)
  else if (i == nrow(report)) writeLines("", con)
}
writeLines("", con)
writeLines(foot1, con)
writeLines(foot2, con)
writeLines(foot3, con)
close(con)
message("Done")

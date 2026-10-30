library(dplyr)
library(tidyr)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

## ---- helpers ----
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  res <- x
  ok <- !is.na(x)
  xx <- x[ok]
  p <- 10 ^ digits
  res[ok] <- sign(xx) * floor(abs(xx) * p + 0.5 + 1e-9) / p
  res
}
fmt_dec <- function(x, d) {
  if (length(x) == 0) return(character(0))
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi)) "" else {
      r <- round_half_up(xi, d)
      sprintf(paste0("%.", d, "f"), r)
    }
  }, character(1), USE.NAMES = FALSE)
}
strip <- function(x) trimws(as.character(x), which = "both")

unlabel_df <- function(df) {
  for (nm in names(df)) {
    v <- df[[nm]]
    if (inherits(v, "haven_labelled") || inherits(v, "haven_labelled_spss")) {
      v <- haven::zap_labels(v)
      v <- haven::zap_formats(v)
      df[[nm]] <- as.vector(v)
    }
  }
  df
}

find_file <- function(dir, base) {
  if (!dir.exists(dir)) return(NULL)
  allf <- list.files(dir, full.names = TRUE, recursive = TRUE)
  hit <- allf[grepl(paste0("(?i)", base, ".*\\.(xpt|sas7bdat|csv)$"), allf, perl = TRUE)]
  if (length(hit) == 0) return(NULL)
  ord <- order(match(tolower(tools::file_ext(hit)), c("xpt","sas7bdat","csv")))
  hit[ord[1]]
}
read_any <- function(f) {
  ext <- tolower(tools::file_ext(f))
  if (ext == "xpt") haven::read_xpt(f)
  else if (ext == "sas7bdat") haven::read_sas(f)
  else readr::read_csv(f, show_col_types = FALSE)
}

## ---- read ADSL ----
f_adsl <- find_file(adam_dir, "adsl")
if (is.null(f_adsl)) stop("ADSL not found in ", adam_dir)
adsl_raw <- as.data.frame(read_any(f_adsl), stringsAsFactors = FALSE)
adsl_raw <- unlabel_df(adsl_raw)
names(adsl_raw) <- toupper(names(adsl_raw))
stopifnot(all(c("USUBJID","TRTA","SAFFL") %in% names(adsl_raw)))
for (c in c("USUBJID","TRTA","SAFFL","SEX","RACE","ETHNIC") ) {
  if (c %in% names(adsl_raw)) adsl_raw[[c]] <- as.character(adsl_raw[[c]])
}
for (c in c("AGE","WEIGHTBL","HEIGHTBL","BMIBL") ) {
  if (c %in% names(adsl_raw)) adsl_raw[[c]] <- suppressWarnings(as.numeric(adsl_raw[[c]]))
}

## ---- Step 1: Big N (m_counts) ----
pop <- adsl_raw[toupper(adsl_raw$SAFFL) == "Y", , drop = FALSE]
bign_df <- as.data.frame(pop %>% dplyr::group_by(TRTA) %>%
  dplyr::summarise(bign = dplyr::n_distinct(USUBJID), .groups = "drop"), stringsAsFactors = FALSE)
bign_df$TRTA <- as.character(bign_df$TRTA)
## SAS order by TRTA (uppercase before lowercase, radix)
bign_df <- bign_df[order(bign_df$TRTA, method = "radix"), , drop = FALSE]
bigntot <- dplyr::n_distinct(pop$USUBJID)
ntrt <- nrow(bign_df)
## headers in SAS sorted order
hdr_sorted <- paste0(bign_df$TRTA, " (N=", bign_df$bign, ")")
## map data columns: col1=Placebo, col2=Low, col3=High (per transpose rename)
uniq_trt <- unique(adsl_raw$TRTA)
find_trt <- function(pat) {
  h <- uniq_trt[grepl(pat, uniq_trt, fixed = TRUE)]
  if (length(h) == 0) h <- uniq_trt[grepl(pat, uniq_trt, ignore.case = TRUE)]
  if (length(h) == 0) stop(paste0("TRT pattern not found: ", pat))
  if (pat == "Placebo") {
    e <- h[h == "Placebo"]
    if (length(e) > 0) return(e[1])
  }
  h[1]
}
trt_placebo <- find_trt("Placebo")
trt_low <- find_trt("Low")
trt_high <- find_trt("High")
data_trts <- c(trt_placebo, trt_low, trt_high)

## ---- Step 2: dm ----
dm <- adsl_raw[adsl_raw$SAFFL == "Y", , drop = FALSE]
dm$AGEGR1 <- NA_character_
dm$AGEGR1[dm$AGE < 18] <- "<18"
dm$AGEGR1[dm$AGE >= 18 & dm$AGE < 65] <- "18 to <65"
dm$AGEGR1[dm$AGE >= 65] <- ">=65"
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

## ---- m_cont_stats ----
cont_stats <- function(df, var, label, block, decimals = 1) {
  dp <- decimals; dp1 <- decimals + 1
  trts <- sort(unique(df$TRTA), method = "radix")
  rows <- list()
  for (t in trts) {
    x <- df[df$TRTA == t, var]
    x <- x[!is.na(x)]
    n <- length(x)
    if (n > 0) {
      mn <- mean(x); sdv <- if (n > 1) stats::sd(x) else NA_real_
      qs <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), type = 2, names = FALSE)
      q1 <- qs[1]; med <- qs[2]; q3 <- qs[3]
      mi <- min(x); ma <- max(x)
    } else {
      mn <- sdv <- med <- q1 <- q3 <- mi <- ma <- NA_real_
    }
    v_n <- strip(as.character(n))
    v_mean <- if (n > 0) strip(fmt_dec(mn, dp1)) else ""
    v_sd <- if (n > 1) strip(fmt_dec(sdv, dp1)) else ""
    v_med <- if (n > 0) strip(fmt_dec(med, dp)) else ""
    v_q <- if (n > 0) paste0(strip(fmt_dec(q1, dp)), ", ", strip(fmt_dec(q3, dp))) else ""
    v_mm <- if (n > 0) paste0(strip(fmt_dec(mi, dp)), ", ", strip(fmt_dec(ma, dp))) else ""
    stats <- c("n","Mean","SD","Median","Q1, Q3","Min, Max")
    vals <- c(v_n, v_mean, v_sd, v_med, v_q, v_mm)
    ords <- 1:6
    for (i in seq_along(stats)) {
      rows[[length(rows)+1]] <- data.frame(TRTA=t, stat=stats[i], value=vals[i],
        ord=ords[i], category=label, block=block, stringsAsFactors=FALSE)
    }
  }
  do.call(rbind, rows)
}

c_age <- cont_stats(dm, "AGE", "Age (years)", 1, decimals=1)
c_wt  <- cont_stats(dm, "WEIGHTBL", "Weight (kg)", 5, decimals=1)
c_ht  <- cont_stats(dm, "HEIGHTBL", "Height (cm)", 6, decimals=1)
c_bmi <- cont_stats(dm, "BMIBL", "BMI (kg/m^{super 2})", 7, decimals=1)

## ---- m_cat_stats ----
cat_stats <- function(df, var, label, block) {
  sub <- df[, c("TRTA","USUBJID", var)]
  names(sub)[3] <- "LEV"
  sub$TRTA <- as.character(sub$TRTA)
  sub$LEV <- as.character(sub$LEV)
  cnt <- sub %>% dplyr::group_by(TRTA, LEV) %>% dplyr::summarise(cnt=dplyr::n_distinct(USUBJID), .groups="drop")
  cnt <- as.data.frame(cnt, stringsAsFactors=FALSE)
  cnt$TRTA <- as.character(cnt$TRTA)
  cnt$LEV <- as.character(cnt$LEV)
  cnt <- merge(cnt, bign_df, by="TRTA", all.x=TRUE)
  cnt$pct <- ifelse(cnt$bign > 0, cnt$cnt / cnt$bign * 100, NA_real_)
  pct_s <- strip(fmt_dec(cnt$pct, 1))
  cnt$value <- ifelse(cnt$cnt > 0,
    paste0(strip(as.character(cnt$cnt)), " (", pct_s, ")"), "0")
  data.frame(TRTA=cnt$TRTA, stat=cnt$LEV, value=cnt$value, ord=NA_real_,
    category=label, block=block, stringsAsFactors=FALSE)
}
f_sex <- cat_stats(dm, "SEX", "Sex", 2)
f_race <- cat_stats(dm, "RACE", "Race", 3)
f_eth <- cat_stats(dm, "ETHNIC", "Ethnicity", 4)
f_agegr <- cat_stats(dm, "AGEGR1", "Age Group", 8)

all_stats <- rbind(c_age, f_sex, f_race, f_eth, c_wt, c_ht, c_bmi, f_agegr)

## ---- sort by block ord stat ----
all_stats$ord_key <- ifelse(is.na(all_stats$ord), -1e9, all_stats$ord)
all_stats <- all_stats[order(all_stats$block, all_stats$ord_key, all_stats$stat, method="radix"), , drop=FALSE]

## ---- transpose by block category ord stat, id TRTA ----
wide <- tidyr::pivot_wider(all_stats, id_cols=c("block","category","ord","stat"),
  names_from="TRTA", values_from="value", values_fill=NA_character_)
for (t in data_trts) if (!(t %in% names(wide))) wide[[t]] <- NA_character_
wide$col1 <- wide[[trt_placebo]]
wide$col2 <- wide[[trt_low]]
wide$col3 <- wide[[trt_high]]
final <- as.data.frame(wide[, c("block","category","ord","stat","col1","col2","col3")], stringsAsFactors=FALSE)
final <- final[order(final$block, ifelse(is.na(final$ord), -1e9, final$ord), final$stat, method="radix"), , drop=FALSE]

## ---- report dataset ----
final <- final[order(final$block, ifelse(is.na(final$ord), -1e9, final$ord), final$stat, method="radix"), , drop=FALSE]
final$display_cat <- ""
prev_cat <- NA_character_
for (i in seq_len(nrow(final))) {
  cur <- paste(final$block[i], final$category[i], sep="|")
  if (is.na(prev_cat) || cur != prev_cat) final$display_cat[i] <- final$category[i]
  else final$display_cat[i] <- " "
  prev_cat <- cur
}
final$stat_disp <- paste0("  ", strip(final$stat))
report <- data.frame(block=final$block, display_cat=final$display_cat,
  stat=final$stat_disp, col1=final$col1, col2=final$col2, col3=final$col3,
  stringsAsFactors=FALSE)
for (c in c("col1","col2","col3")) report[[c]][is.na(report[[c]])] <- ""

## ---- titles/footnotes ----
t1 <- "[Sponsor Name]                                                  Page 1 of 1"
t2 <- "Protocol: [Protocol Number]"
t3 <- " "
t4 <- "Table 14.1.1"
t5 <- "Summary of Demographics and Baseline Characteristics"
t6 <- "Safety Analysis Set"
f1 <- "Data cutoff: [DDMONYYYY]."
f2 <- "Source: ADaM datasets"
f3 <- paste0("Program: P072_t_dm.sas | Run: ", format(Sys.Date(), "%d%b%Y"))

hdr1 <- hdr_sorted[1]; hdr2 <- hdr_sorted[2]; hdr3 <- hdr_sorted[3]

## ---- text rendering ----
con <- file(file.path(out, "t_dm.txt"), open="wt")
writeLines(t1, con); writeLines(t2, con); writeLines(t3, con)
writeLines(t4, con); writeLines(t5, con); writeLines(t6, con); writeLines("", con)
hw <- c(sprintf("%-32s", "Parameter"), sprintf("%-18s", "Statistic"),
  sprintf("%-22s", hdr1), sprintf("%-22s", hdr2), sprintf("%-22s", hdr3))
writeLines(paste(hw, collapse=" | "), con)
writeLines(paste(rep("-", 130), collapse=""), con)
for (i in seq_len(nrow(report))) {
  b <- report$block[i]
  line <- paste(c(sprintf("%-32s", substr(report$display_cat[i],1,32)),
    sprintf("%-18s", substr(report$stat[i],1,18)),
    sprintf("%-22s", report$col1[i]),
    sprintf("%-22s", report$col2[i]),
    sprintf("%-22s", report$col3[i])), collapse=" | ")
  writeLines(line, con)
  nxt <- if (i < nrow(report)) report$block[i+1] else NA
  if (is.na(nxt) || nxt != b) writeLines("", con)
}
writeLines("", con)
writeLines(f1, con); writeLines(f2, con); writeLines(f3, con)
close(con)

## ---- CSV of table body ----
csv_df <- data.frame(Parameter=report$display_cat, Statistic=strip(report$stat),
  stringsAsFactors=FALSE)
csv_df[[hdr1]] <- report$col1
csv_df[[hdr2]] <- report$col2
csv_df[[hdr3]] <- report$col3
write.csv(csv_df, file.path(out, "t_dm.csv"), row.names=FALSE, na="")

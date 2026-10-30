# P072 - Table 14.1.1 Summary of Demographics and Baseline Characteristics
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adampath <- "/bench/data/raw/own-synthetic/adam"

# ---------------------------------------------------------------- helpers
# SAS-style rounding: half away from zero
rhu <- function(x, d = 0) {
  m <- 10^d
  sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m
}
# Emulates PUT(x, w.d) followed by STRIP
fmtn <- function(x, d) {
  ifelse(is.na(x), "",
         trimws(formatC(rhu(x, d), format = "f", digits = d)))
}
# SAS character sort order (C locale, blanks first)
sas_sort <- function(x) x[order(x, method = "radix")]

read_ds <- function(name) {
  f1 <- file.path(adampath, paste0(name, ".sas7bdat"))
  f2 <- file.path(adampath, paste0(name, ".xpt"))
  if (file.exists(f1)) {
    d <- haven::read_sas(f1)
  } else {
    d <- haven::read_xpt(f2)
  }
  d <- haven::zap_labels(haven::zap_formats(d))
  as.data.frame(d, stringsAsFactors = FALSE)
}

cat("Reading ADSL...\n")
adsl <- read_ds("adsl")
names(adsl) <- toupper(names(adsl))

# Character columns: NA -> "" (SAS blank = missing)
for (v in names(adsl)) {
  if (is.character(adsl[[v]])) adsl[[v]][is.na(adsl[[v]])] <- ""
}

# ---------------------------------------------------------------- Step 1: m_counts
pop <- adsl[toupper(adsl$SAFFL) == "Y", ]
bign_df <- pop %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
bign_df <- bign_df[order(bign_df$TRTA, method = "radix"), , drop = FALSE]
bigntot <- n_distinct(pop$USUBJID)
ntrt <- nrow(bign_df)
colhdr <- paste0(bign_df$TRTA, "~(N=", bign_df$bign, ")")
cat("Big N:", paste(bign_df$TRTA, bign_df$bign, sep = "=", collapse = "; "),
    "; Total =", bigntot, "\n")

# ---------------------------------------------------------------- Step 2: dm
dm <- adsl[adsl$SAFFL == "Y", ]
dm$AGEGR1 <- ifelse(is.na(dm$AGE) | dm$AGE < 18, "<18",
                    ifelse(dm$AGE < 65, "18 to <65", ">=65"))

# ---------------------------------------------------------------- Step 3: continuous
m_cont_stats <- function(df, var, decimals = 1) {
  dp <- decimals
  dp1 <- decimals + 1
  df <- df[df$TRTA != "", ]                 # class var missing excluded
  grps <- sas_sort(unique(df$TRTA))
  res <- list()
  for (g in grps) {
    x <- df[[var]][df$TRTA == g]
    x <- x[!is.na(x)]
    n <- length(x)
    if (n > 0) {
      mn <- mean(x)
      md <- median(x)
      q <- quantile(x, c(0.25, 0.75), type = 2, names = FALSE)
      mi <- min(x)
      ma <- max(x)
    }
    sdv <- if (n > 1) sd(x) else NA
    vals <- c(
      as.character(n),
      if (n > 0) fmtn(mn, dp1) else "",
      if (n > 1) fmtn(sdv, dp1) else "",
      if (n > 0) fmtn(md, dp) else "",
      if (n > 0) paste(fmtn(q[1], dp), fmtn(q[2], dp), sep = ", ") else "",
      if (n > 0) paste(fmtn(mi, dp), fmtn(ma, dp), sep = ", ") else ""
    )
    res[[length(res) + 1]] <- data.frame(
      TRTA = g,
      stat = c("n", "Mean", "SD", "Median", "Q1, Q3", "Min, Max"),
      value = vals, ord = 1:6, stringsAsFactors = FALSE)
  }
  r <- do.call(rbind, res)
  r[order(r$ord, r$TRTA, method = "radix"), , drop = FALSE]
}

cont_block <- function(var, label, ord, decimals = 1) {
  r <- m_cont_stats(dm, var, decimals)
  r$category <- label
  r$block <- ord
  r
}

c_age <- cont_block("AGE", "Age (years)", 1)
c_wt  <- cont_block("WEIGHTBL", "Weight (kg)", 5)
c_ht  <- cont_block("HEIGHTBL", "Height (cm)", 6)
c_bmi <- cont_block("BMIBL", "BMI (kg/m^{super 2})", 7)

# ---------------------------------------------------------------- Step 4: categorical
m_cat_stats <- function(df, catvar) {
  d <- data.frame(TRTA = df$TRTA, cat = as.character(df[[catvar]]),
                  USUBJID = df$USUBJID, stringsAsFactors = FALSE)
  d$cat[is.na(d$cat)] <- ""
  cnt <- d %>% group_by(TRTA, cat) %>%
    summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
    as.data.frame()
  cnt <- merge(cnt, bign_df, by = "TRTA", all.x = TRUE, sort = FALSE)
  cnt$pct <- ifelse(!is.na(cnt$bign) & cnt$bign > 0, cnt$cnt / cnt$bign * 100, NA)
  cnt <- cnt[order(cnt$cat, cnt$TRTA, method = "radix"), , drop = FALSE]
  cnt$value <- ifelse(cnt$cnt > 0,
                      paste0(cnt$cnt, " (", fmtn(cnt$pct, 1), ")"), "0")
  cnt
}

cat_block <- function(var, label, ord) {
  r <- m_cat_stats(dm, var)
  data.frame(TRTA = r$TRTA, stat = substr(r$cat, 1, 40), value = r$value,
             ord = NA_real_, category = label, block = ord,
             stringsAsFactors = FALSE)
}

f_sex    <- cat_block("SEX", "Sex", 2)
f_race   <- cat_block("RACE", "Race", 3)
f_ethnic <- cat_block("ETHNIC", "Ethnicity", 4)
f_agegr1 <- cat_block("AGEGR1", "Age Group", 8)

# ---------------------------------------------------------------- Step 5: combine
all_stats <- rbind(c_age, f_sex, f_race, f_ethnic, c_wt, c_ht, c_bmi, f_agegr1)

# ---------------------------------------------------------------- Step 6: sort + transpose
all_stats <- all_stats[order(all_stats$block, all_stats$ord, all_stats$stat,
                             na.last = FALSE, method = "radix"), , drop = FALSE]
if (any(all_stats$TRTA == "")) {
  cat("WARNING: rows with missing TRTA dropped from transpose\n")
  all_stats <- all_stats[all_stats$TRTA != "", ]
}
all_stats$key <- paste(all_stats$block, all_stats$category,
                       ifelse(is.na(all_stats$ord), ".", all_stats$ord),
                       all_stats$stat, sep = "\r")
keys <- all_stats[!duplicated(all_stats$key),
                  c("block", "category", "ord", "stat", "key")]
final <- keys
idnames <- unique(all_stats$TRTA)
for (g in idnames) {
  cn <- paste0("col", gsub("[^A-Za-z0-9_]", "_", g))
  sub <- all_stats[all_stats$TRTA == g, ]
  final[[cn]] <- sub$value[match(final$key, sub$key)]
}
ren <- c(colPlacebo = "col1", colDrug_X_Low_Dose = "col2", colDrug_X_High_Dose = "col3")
for (o in names(ren)) if (o %in% names(final)) names(final)[names(final) == o] <- ren[[o]]
for (cc in c("col1", "col2", "col3")) {
  if (!cc %in% names(final)) final[[cc]] <- NA_character_
  final[[cc]][is.na(final[[cc]])] <- ""
}
final$key <- NULL

# ---------------------------------------------------------------- Step 7: report dataset
report <- final
first_cat <- !duplicated(paste(report$block, report$category, sep = "\r"))
report$display_cat <- ifelse(first_cat, report$category, "")
report$stat <- substr(paste0("  ", trimws(report$stat)), 1, 40)
report <- report[, c("block", "category", "stat", "col1", "col2", "col3",
                     "ord", "display_cat")]
report <- report[order(report$block, method = "radix"), , drop = FALSE]

# ---------------------------------------------------------------- Step 8: outputs
disp_lab <- function(x) gsub("\\^\\{super 2\\}", "^2", x)

body <- data.frame(
  block = report$block,
  Parameter = disp_lab(report$display_cat),
  Statistic = report$stat,
  col1 = report$col1, col2 = report$col2, col3 = report$col3,
  stringsAsFactors = FALSE)
write.csv(body, file.path(out, "t_dm.csv"), row.names = FALSE, na = "")

hdr <- gsub("~", " ", colhdr)
hdr <- c(hdr, rep("", max(0, 3 - length(hdr))))[1:3]

w1 <- 28; w2 <- 18; wc <- 26
padr <- function(s, w) formatC(s, width = -w, flag = "-")
padc <- function(s, w) {
  s <- as.character(s)
  n <- nchar(s)
  l <- pmax(0, (w - n) %/% 2)
  r <- pmax(0, w - n - l)
  paste0(strrep(" ", l), s, strrep(" ", r))
}
totw <- w1 + w2 + 3 * wc
lines <- c(
  "[Sponsor Name]",
  "Protocol: [Protocol Number]",
  "",
  padc("Table 14.1.1", totw),
  padc("Summary of Demographics and Baseline Characteristics", totw),
  padc("Safety Analysis Set", totw),
  "",
  paste0(padr("Parameter", w1), padr("Statistic", w2),
         padc(hdr[1], wc), padc(hdr[2], wc), padc(hdr[3], wc)),
  strrep("-", totw)
)
blocks <- unique(body$block)
for (b in blocks) {
  sb <- body[body$block == b, ]
  for (i in seq_len(nrow(sb))) {
    lines <- c(lines, paste0(padr(sb$Parameter[i], w1), padr(sb$Statistic[i], w2),
                             padc(sb$col1[i], wc), padc(sb$col2[i], wc),
                             padc(sb$col3[i], wc)))
  }
  lines <- c(lines, "")
}
lines <- c(lines, strrep("-", totw),
           "Data cutoff: [DDMONYYYY]",
           "Source: ADaM datasets",
           "Program: P072_t_dm.sas")
writeLines(lines, file.path(out, "t_dm.txt"))

cat("NOTE: t_dm completed successfully.\n")

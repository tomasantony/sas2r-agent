```r
## P073 - Table 14.1.3 Summary of Study Drug Exposure (translation of t_exposure.sas)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

## ---------- helpers ----------
read_ds <- function(path, name) {
  cands <- c(file.path(path, paste0(tolower(name), ".sas7bdat")),
             file.path(path, paste0(toupper(name), ".sas7bdat")),
             file.path(path, paste0(tolower(name), ".xpt")),
             file.path(path, paste0(toupper(name), ".xpt")))
  for (f in cands) {
    if (file.exists(f)) {
      if (grepl("\\.xpt$", f)) return(as.data.frame(haven::read_xpt(f)))
      return(as.data.frame(haven::read_sas(f)))
    }
  }
  stop("Dataset not found: ", name)
}

round_half_up <- function(x, d = 0) {
  m <- 10^d
  sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m
}
# SAS put(x, w.d) then strip()
fmt_num <- function(x, d) {
  ifelse(is.na(x), ".", formatC(round_half_up(x, d), format = "f", digits = d))
}
# SAS QNTLDEF=5 percentile
sas_q5 <- function(x, p) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n == 0) return(NA_real_)
  np <- n * p
  j <- floor(np + 1e-10)
  if (abs(np - j) < 1e-10) {
    if (j < 1) return(x[1])
    if (j >= n) return(x[n])
    return((x[j] + x[j + 1]) / 2)
  } else {
    return(x[min(n, j + 1)])
  }
}
sas_sort_chr <- function(x) sort(unique(x), method = "radix")

## ---------- read data ----------
adsl <- read_ds(adampath, "adsl")
names(adsl) <- toupper(names(adsl))
adsl$TRTA  <- as.character(adsl$TRTA)
adsl$SAFFL <- as.character(adsl$SAFFL)

## ---------- Step 1: m_counts ----------
pop <- adsl[!is.na(adsl$SAFFL) & toupper(adsl$SAFFL) == "Y", ]
bign_df <- pop %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
bign_df <- bign_df[order(is.na(bign_df$TRTA), rank(bign_df$TRTA, ties.method = "first", na.last = FALSE)), ]
# SAS order: missing first, then C-locale
ord_idx <- order(!is.na(bign_df$TRTA), method = "radix")
bign_df <- bign_df[order(ifelse(is.na(bign_df$TRTA), 0, 1), bign_df$TRTA, method = "radix"), ]
ntrt <- nrow(bign_df)
bigntot <- length(unique(pop$USUBJID))
colhdr <- sapply(seq_len(ntrt), function(i) {
  lbl <- ifelse(is.na(bign_df$TRTA[i]), "", bign_df$TRTA[i])
  paste0(lbl, "~(N=", bign_df$bign[i], ")")
})
coltothdr <- paste0("Total~(N=", bigntot, ")")

## ---------- ex_adsl ----------
ex_adsl <- adsl[!is.na(adsl$SAFFL) & adsl$SAFFL == "Y", ]
tsd <- as.numeric(as.Date(ex_adsl$TRTSDT))
ted <- as.numeric(as.Date(ex_adsl$TRTEDT))
ex_adsl$TRTDUR <- ifelse(!is.na(ted) & !is.na(tsd), ted - tsd + 1, NA_real_)

## ---------- Step 3: m_cont_stats ----------
dp <- 1; dp1 <- 2
cs <- ex_adsl[!is.na(ex_adsl$TRTA), ]
trts <- unique(cs$TRTA)
trts <- trts[order(trts, method = "radix")]
dur_rows <- list()
for (t in trts) {
  v <- cs$TRTDUR[cs$TRTA == t]
  v <- v[!is.na(v)]
  n <- length(v)
  mn <- if (n > 0) mean(v) else NA
  sdv <- if (n > 1) sd(v) else NA
  med <- sas_q5(v, 0.5); q1 <- sas_q5(v, 0.25); q3 <- sas_q5(v, 0.75)
  mi <- if (n > 0) min(v) else NA; ma <- if (n > 0) max(v) else NA
  vals <- c(
    as.character(n),
    if (n > 0) fmt_num(mn, dp1) else "",
    if (n > 1) fmt_num(sdv, dp1) else "",
    if (n > 0) fmt_num(med, dp) else "",
    if (n > 0) paste(fmt_num(q1, dp), fmt_num(q3, dp), sep = ", ") else "",
    if (n > 0) paste(fmt_num(mi, dp), fmt_num(ma, dp), sep = ", ") else ""
  )
  dur_rows[[length(dur_rows) + 1]] <- data.frame(
    TRTA = t, stat = c("n", "Mean", "SD", "Median", "Q1, Q3", "Min, Max"),
    value = vals, ord = 1:6, stringsAsFactors = FALSE)
}
dur_stats <- do.call(rbind, dur_rows)
dur_stats$category <- "Duration of Treatment (days)"
dur_stats$block <- 1

## ---------- Step 4: duration categories ----------
dur_cat <- ex_adsl
d <- dur_cat$TRTDUR
dur_cat$DURCAT <- ifelse(is.na(d), "Missing",
                  ifelse(d <= 28, "<=28 days",
                  ifelse(d <= 84, ">28 to <=84 days",
                  ifelse(d <= 168, ">84 to <=168 days",
                  ifelse(d <= 336, ">168 to <=336 days", ">336 days")))))
catmap <- c("<=28 days" = 1, ">28 to <=84 days" = 2, ">84 to <=168 days" = 3,
            ">168 to <=336 days" = 4, ">336 days" = 5, "Missing" = 6)
dur_cat$DURCATN <- unname(catmap[dur_cat$DURCAT])

## ---------- Step 5: counts ----------
durcat_cnt <- dur_cat %>%
  group_by(TRTA, DURCAT, DURCATN) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
durcat_pct <- merge(durcat_cnt, bign_df, by = "TRTA", all.x = TRUE)
durcat_pct$pct <- ifelse(!is.na(durcat_pct$bign) & durcat_pct$bign > 0,
                         durcat_pct$cnt / durcat_pct$bign * 100, NA_real_)
durcat_fmt <- data.frame(
  TRTA = durcat_pct$TRTA,
  stat = paste0("  ", trimws(durcat_pct$DURCAT)),
  value = ifelse(durcat_pct$cnt > 0,
                 paste0(durcat_pct$cnt, " (", fmt_num(durcat_pct$pct, 1), ")"), "0"),
  ord = 100 + durcat_pct$DURCATN,
  category = "Duration Categories",
  block = 2, stringsAsFactors = FALSE)

## ---------- Step 6/7: combine & transpose ----------
all_exp <- rbind(dur_stats[, c("TRTA", "stat", "value", "ord", "category", "block")],
                 durcat_fmt[, c("TRTA", "stat", "value", "ord", "category", "block")])
all_exp <- all_exp[order(all_exp$block, all_exp$ord, all_exp$stat, method = "radix"), ]
all_exp$idname <- paste0("col", gsub("[^A-Za-z0-9_]", "_", all_exp$TRTA))

keys <- unique(all_exp[, c("block", "category", "ord", "stat")])
final <- keys
idnames <- unique(all_exp$idname)
for (nm in idnames) {
  sub <- all_exp[all_exp$idname == nm, c("block", "category", "ord", "stat", "value")]
  names(sub)[5] <- nm
  final <- merge(final, sub, by = c("block", "category", "ord", "stat"), all.x = TRUE, sort = FALSE)
}
final <- final[order(final$block, final$ord, final$stat, method = "radix"), ]
rn <- c(colPlacebo = "col1", colDrug_X_Low_Dose = "col2", colDrug_X_High_Dose = "col3")
for (k in names(rn)) if (k %in% names(final)) names(final)[names(final) == k] <- rn[[k]]
for (k in c("col1", "col2", "col3")) if (!k %in% names(final)) final[[k]] <- NA_character_
final <- final[, c("block", "category", "ord", "stat", "col1", "col2", "col3")]
rownames(final) <- NULL

write.csv(final, file.path(out, "t_exposure_final.csv"), row.names = FALSE, na = "")

## ---------- Step 8: report ----------
hdr <- function(i) if (i <= length(colhdr)) gsub("~", " ", colhdr[i]) else ""
body <- final
body$category_disp <- ifelse(!duplicated(paste(body$block, body$category)), body$category, "")
tab <- data.frame(category = body$category_disp, stat = body$stat,
                  col1 = ifelse(is.na(body$col1), "", body$col1),
                  col2 = ifelse(is.na(body$col2), "", body$col2),
                  col3 = ifelse(is.na(body$col3), "", body$col3),
                  stringsAsFactors = FALSE)
tab_out <- tab
names(tab_out) <- c("Category", "Statistic", hdr(1), hdr(2), hdr(3))
write.csv(tab_out, file.path(out, "t_exposure.csv"), row.names = FALSE, na = "")

w <- c(30, 22, 24, 24, 24)
pad <- function(x, n) formatC(x, width = -n)
cen <- function(x, n) {
  x <- as.character(x); l <- nchar(x)
  if (l >= n) return(x)
  left <- floor((n - l) / 2); paste0(strrep(" ", left), x, strrep(" ", n - l - left))
}
lines <- c(
  "[Sponsor Name]",
  "Protocol: [Protocol Number]",
  "",
  "Table 14.1.3",
  "Summary of Study Drug Exposure",
  "Safety Analysis Set",
  "",
  paste0(pad("", w[1]), pad("Statistic", w[2]), cen(hdr(1), w[3]), cen(hdr(2), w[4]), cen(hdr(3), w[5])),
  strrep("-", sum(w))
)
for (i in seq_len(nrow(tab))) {
  lines <- c(lines, paste0(pad(tab$category[i], w[1]), pad(tab$stat[i], w[2]),
                           cen(tab$col1[i], w[3]), cen(tab$col2[i], w[4]), cen(tab$col3[i], w[5])))
}
lines <- c(lines, strrep("-", sum(w)),
           "Data cutoff: [DDMONYYYY]",
           "Source: ADaM datasets",
           paste0("Program: P073_t_exposure | Run: ", toupper(format(Sys.Date(), "%d%b%Y"))))
writeLines(lines, file.path(out, "t_exposure.txt"))

cat("NOTE: t_exposure completed successfully.\n")
```
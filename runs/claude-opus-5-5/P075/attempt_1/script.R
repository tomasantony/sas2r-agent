# P075 - Table 14.3.5 Summary of Vital Signs by Visit (translation of t_vs.sas)
suppressPackageStartupMessages({
  library(haven); library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

read_ds <- function(name) {
  f1 <- file.path(adampath, paste0(name, ".sas7bdat"))
  f2 <- file.path(adampath, paste0(name, ".xpt"))
  if (file.exists(f1)) d <- haven::read_sas(f1)
  else if (file.exists(f2)) d <- haven::read_xpt(f2)
  else stop("Dataset not found: ", name)
  d <- haven::zap_labels(haven::zap_formats(d))
  names(d) <- toupper(names(d))
  d
}

# SAS-style rounding and put(x, w.d)
round_half_up <- function(x, d = 0) {
  m <- 10^d
  sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m
}
put_num <- function(x, d) {
  r <- ifelse(is.na(x), ".", formatC(round_half_up(x, d), format = "f", digits = d))
  r[r == "-0" | grepl("^-0\\.0*$", r)] <- sub("^-", "", r[r == "-0" | grepl("^-0\\.0*$", r)])
  r
}
catx2 <- function(a, b) paste0(a, ", ", b)

adsl <- read_ds("adsl")
advs <- read_ds("advs")
message("Data read: adsl ", nrow(adsl), " rows, advs ", nrow(advs), " rows")

# Step 1: Big N (macro vars; not displayed in the table body)
if ("TRTA" %in% names(adsl)) {
  bign <- adsl %>% filter(toupper(SAFFL) == "Y") %>%
    group_by(TRTA) %>% summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
    arrange(TRTA)
  message("Big N computed for ", nrow(bign), " groups")
}

# Step 2: analysis data
vs <- advs %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, PARAMN, AVISIT, AVISITN,
         AVAL, BASE, CHG, ABLFL, ANL01FL) %>%
  inner_join(adsl %>% select(USUBJID, SAFFL), by = "USUBJID", relationship = "many-to-many") %>%
  filter(SAFFL %in% "Y",
         PARAMCD %in% c("SYSBP", "DIABP", "PULSE", "TEMP", "RESP", "WEIGHT"),
         ANL01FL %in% "Y") %>%
  select(-SAFFL)

classv <- c("PARAMCD", "PARAM", "PARAMN", "AVISIT", "AVISITN", "TRTA", "TRTAN")
nonmiss_class <- function(d) {
  keep <- rep(TRUE, nrow(d))
  for (v in classv) {
    x <- d[[v]]
    keep <- keep & !is.na(x) & !(is.character(x) & x == "")
  }
  d[keep, ]
}
sas_order <- function(d) {
  d[do.call(order, c(lapply(classv, function(v) d[[v]]), list(method = "radix"))), ]
}

# Step 3: descriptive stats
smean <- function(x) if (length(x) > 0) mean(x) else NA_real_
ssd   <- function(x) if (length(x) > 1) sd(x) else NA_real_
smed  <- function(x) if (length(x) > 0) median(x) else NA_real_
smin  <- function(x) if (length(x) > 0) min(x) else NA_real_
smax  <- function(x) if (length(x) > 0) max(x) else NA_real_

actual <- nonmiss_class(vs) %>% group_by(across(all_of(classv))) %>%
  summarise(n = sum(!is.na(AVAL)),
            mean = smean(AVAL[!is.na(AVAL)]), std = ssd(AVAL[!is.na(AVAL)]),
            median = smed(AVAL[!is.na(AVAL)]), min = smin(AVAL[!is.na(AVAL)]),
            max = smax(AVAL[!is.na(AVAL)]), .groups = "drop") %>%
  as.data.frame() %>% sas_order()

change <- nonmiss_class(vs %>% filter(!is.na(CHG), !(ABLFL %in% "Y"))) %>%
  group_by(across(all_of(classv))) %>%
  summarise(n_chg = n(), mean_chg = smean(CHG), std_chg = ssd(CHG),
            median_chg = smed(CHG), min_chg = smin(CHG), max_chg = smax(CHG),
            .groups = "drop") %>%
  as.data.frame() %>% sas_order()

# Step 4: merge
stats <- actual %>%
  left_join(change %>% select(PARAMCD, AVISITN, TRTAN, n_chg, mean_chg, std_chg,
                              median_chg, min_chg, max_chg),
            by = c("PARAMCD", "AVISITN", "TRTAN"), relationship = "many-to-many")
stats <- stats[order(stats$PARAMN, stats$AVISITN, stats$TRTAN, method = "radix"), ]

# Step 5: format
report <- stats %>% mutate(
  stat1 = put_num(n, 0),
  stat2 = put_num(mean, 1),
  stat3 = put_num(std, 2),
  stat4 = put_num(median, 1),
  stat5 = catx2(put_num(min, 1), put_num(max, 1)),
  has = !is.na(n_chg) & n_chg > 0,
  chg1 = ifelse(has, put_num(n_chg, 0), ""),
  chg2 = ifelse(has, put_num(mean_chg, 2), ""),
  chg3 = ifelse(has, put_num(std_chg, 2), ""),
  chg4 = ifelse(has, put_num(median_chg, 2), ""),
  chg5 = ifelse(has, catx2(put_num(min_chg, 1), put_num(max_chg, 1)), "")
) %>% select(-has)

# Step 6: PROC REPORT ordering (order vars PARAMN PARAM AVISITN AVISIT)
rep <- report[order(report$PARAMN, report$PARAM, report$AVISITN, report$AVISIT,
                    method = "radix"), ]
grpP <- paste(rep$PARAMN, rep$PARAM)
grpV <- paste(grpP, rep$AVISITN, rep$AVISIT)
firstP <- !duplicated(grpP)
firstV <- !duplicated(grpV)

body <- data.frame(
  Parameter = ifelse(firstP, rep$PARAM, ""),
  Visit = ifelse(firstV, rep$AVISIT, ""),
  Treatment = rep$TRTA,
  AV_n = rep$stat1, AV_Mean = rep$stat2, AV_SD = rep$stat3,
  AV_Median = rep$stat4, AV_MinMax = rep$stat5,
  CFB_n = rep$chg1, CFB_Mean = rep$chg2, CFB_SD = rep$chg3,
  CFB_Median = rep$chg4, CFB_MinMax = rep$chg5,
  stringsAsFactors = FALSE
)

write.csv(report, file.path(out, "report.csv"), row.names = FALSE, na = "")
write.csv(body, file.path(out, "t_vs.csv"), row.names = FALSE, na = "")

# Text rendering
hdr <- c("Parameter", "Visit", "Treatment", "n", "Mean", "SD", "Median", "Min, Max",
         "n", "Mean", "SD", "Median", "Min, Max")
w <- pmax(nchar(hdr), sapply(body, function(x) max(c(0, nchar(x)))))
fmtrow <- function(v) paste(mapply(function(s, k) formatC(s, width = -k), v, w), collapse = "  ")
lines <- c("[Sponsor Name]", "Protocol: [Protocol Number]", "",
           "Table 14.3.5", "Summary of Vital Signs by Visit", "Safety Analysis Set", "")
pages <- split(seq_len(nrow(body)), cumsum(firstP))
for (p in pages) {
  lines <- c(lines,
             paste0(strrep(" ", sum(w[1:3]) + 6), "Actual Value", strrep(" ", 30), "Change from Baseline"),
             fmtrow(hdr), strrep("-", sum(w) + 2 * (length(w) - 1)))
  for (i in p) lines <- c(lines, fmtrow(unlist(body[i, ])))
  lines <- c(lines, "", "Data cutoff: [DDMONYYYY]", "Source: ADaM datasets", "\f")
}
writeLines(lines, file.path(out, "t_vs.txt"))
message("t_vs completed: ", nrow(body), " rows")

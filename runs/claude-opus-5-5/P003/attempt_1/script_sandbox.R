# P003: at14-5-02.sas -> R translation
suppressPackageStartupMessages({
  library(haven); library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam <- "/bench/data/raw/cdisc-pilot/adam"

round_half_up <- function(x, d = 0) {
  m <- 10^d
  sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m
}
fmt_num <- function(x, d) formatC(round_half_up(x, d), format = "f", digits = d)
fmt_p <- function(p) ifelse(p < 0.0001, "<.0001", fmt_num(p, 4))
sas_sort_chr <- function(x) {
  u <- unique(x)
  u[order(u, method = "radix")]  # C-locale (byte) ordering like SAS
}

cat("Reading data\n")
ADSL <- read_xpt(file.path(rawadam, "adsl.xpt"))
ADAE <- read_xpt(file.path(rawadam, "adae.xpt"))
names(ADSL) <- toupper(names(ADSL)); names(ADAE) <- toupper(names(ADAE))

ADSL_S <- ADSL %>% filter(SAFFL == "Y")
ADAE_S <- ADAE %>% filter(TRTEMFL == "Y" & AESER == "Y")

# ---------------- Descriptive statistics ----------------
DENOM <- ADSL_S %>% group_by(TRTAN = TRT01AN) %>%
  summarise(DENOM = n_distinct(USUBJID), .groups = "drop") %>% arrange(TRTAN)

mk_result <- function(count, denom, events) {
  paste0(count, " (", trimws(fmt_num(count / denom * 100, 1)), "%) [", events, "]")
}

AEPTD <- ADAE_S %>% group_by(TRTAN, AEBODSYS, AEDECOD) %>%
  summarise(count = n_distinct(USUBJID), .groups = "drop")
AEPTA <- ADAE_S %>% group_by(TRTAN, AEBODSYS, AEDECOD) %>%
  summarise(events = sum(!is.na(USUBJID) & USUBJID != ""), .groups = "drop")
RESULTPT <- AEPTD %>% mutate(SUMMARYLEVEL = 3) %>%
  left_join(DENOM, by = "TRTAN") %>%
  left_join(AEPTA, by = c("TRTAN", "AEBODSYS", "AEDECOD")) %>%
  mutate(RESULT = mk_result(count, DENOM, events)) %>%
  arrange(match(AEBODSYS, sas_sort_chr(AEBODSYS)), match(AEDECOD, sas_sort_chr(AEDECOD)), TRTAN)

AESOCD <- ADAE_S %>% group_by(TRTAN, AEBODSYS) %>%
  summarise(count = n_distinct(USUBJID), .groups = "drop")
AESOCA <- ADAE_S %>% group_by(TRTAN, AEBODSYS) %>%
  summarise(events = n(), .groups = "drop")
RESULTSOC <- AESOCD %>% mutate(SUMMARYLEVEL = 2) %>%
  left_join(DENOM, by = "TRTAN") %>%
  left_join(AESOCA, by = c("TRTAN", "AEBODSYS")) %>%
  mutate(RESULT = mk_result(count, DENOM, events)) %>%
  arrange(match(AEBODSYS, sas_sort_chr(AEBODSYS)), TRTAN)

AESUBD <- ADAE_S %>% group_by(TRTAN) %>%
  summarise(count = n_distinct(USUBJID), .groups = "drop")
AESUBA <- ADAE_S %>% group_by(TRTAN) %>% summarise(events = n(), .groups = "drop")
RESULTSUB <- AESUBD %>% mutate(SUMMARYLEVEL = 1) %>%
  left_join(DENOM, by = "TRTAN") %>% left_join(AESUBA, by = "TRTAN") %>%
  mutate(RESULT = mk_result(count, DENOM, events)) %>% arrange(TRTAN)

write.csv(RESULTPT, file.path(out, "resultpt.csv"), row.names = FALSE, na = "")
write.csv(RESULTSOC, file.path(out, "resultsoc.csv"), row.names = FALSE, na = "")
write.csv(RESULTSUB, file.path(out, "resultsub.csv"), row.names = FALSE, na = "")

# ---------------- Inferential statistics ----------------
subj <- ADSL_S %>% distinct(USUBJID, TRTAN = TRT01AN)

build_fisher <- function(byvar) {
  if (is.null(byvar)) {
    bb <- subj
    had <- ADAE_S %>% distinct(STUDYID, USUBJID, TRTAN)
    keys <- c("USUBJID", "TRTAN")
  } else {
    lv <- ADAE_S %>% distinct(across(all_of(byvar)))
    bb <- merge(subj, lv, by = NULL)
    had <- ADAE_S %>% distinct(across(all_of(c("STUDYID", "USUBJID", "TRTAN", byvar))))
    keys <- c("USUBJID", "TRTAN", byvar)
  }
  bb %>% left_join(had, by = keys) %>%
    mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1, 0)) %>%
    select(all_of(keys), EVENT)
}

FISHER  <- build_fisher("AEDECOD")
FISHER1 <- build_fisher("AEBODSYS")
FISHER2 <- build_fisher(NULL)

lst <- character(0)
res_rows <- list()
add <- function(...) lst <<- c(lst, ...)

pad <- function(s, w) formatC(s, width = w)

render_freq <- function(d, byvar, byval, trts, label) {
  d <- d %>% filter(TRTAN %in% trts)
  rows <- sort(unique(d$TRTAN)); cols <- sort(unique(d$EVENT))
  tab <- table(factor(d$TRTAN, levels = rows), factor(d$EVENT, levels = cols))
  N <- sum(tab)
  add("", "The SAS System", "")
  if (!is.null(byvar)) add(paste0("------------------------- ", byvar, "=", byval, " -------------------------"), "")
  add("The FREQ Procedure", "", "Table of TRTAN by EVENT", "", "TRTAN     EVENT", "")
  hdr <- c("Frequency|", "Percent  |", "Row Pct  |", "Col Pct  |")
  hdr[4] <- paste0("Col Pct  |", paste0(sapply(cols, function(c) pad(c, 8)), "|", collapse = ""), "  Total")
  add(hdr)
  sep <- paste0("---------+", paste(rep("--------+", length(cols)), collapse = ""))
  add(sep)
  rt <- rowSums(tab); ct <- colSums(tab)
  for (i in seq_along(rows)) {
    l1 <- paste0(pad(rows[i], 8), " |", paste0(sapply(seq_along(cols), function(j) pad(tab[i, j], 7)), " |", collapse = ""), pad(rt[i], 7))
    l2 <- paste0("         |", paste0(sapply(seq_along(cols), function(j) pad(fmt_num(100 * tab[i, j] / N, 2), 7)), " |", collapse = ""), pad(fmt_num(100 * rt[i] / N, 2), 7))
    l3 <- paste0("         |", paste0(sapply(seq_along(cols), function(j) pad(fmt_num(100 * tab[i, j] / rt[i], 2), 7)), " |", collapse = ""))
    l4 <- paste0("         |", paste0(sapply(seq_along(cols), function(j) pad(fmt_num(100 * tab[i, j] / ct[j], 2), 7)), " |", collapse = ""))
    add(l1, l2, l3, l4, sep)
  }
  add(paste0("Total    ", paste0(sapply(ct, function(x) pad(x, 8)), " ", collapse = ""), pad(N, 7)))
  add(paste0("         ", paste0(sapply(ct, function(x) pad(fmt_num(100 * x / N, 2), 8)), " ", collapse = ""), pad("100.00", 7)))
  add("")
  rec <- data.frame(analysis = label, byvar = ifelse(is.null(byvar), "", byvar),
                    byval = ifelse(is.null(byval), "", byval),
                    comparison = paste0(trts[2], " vs ", trts[1]), N = N,
                    F = NA, left_p = NA, right_p = NA, table_p = NA, twosided_p = NA,
                    stringsAsFactors = FALSE)
  if (length(rows) == 2 && length(cols) == 2) {
    n11 <- tab[1, 1]; m <- rt[1]; n <- N - m; k <- ct[1]
    left <- phyper(n11, m, n, k)
    right <- phyper(n11 - 1, m, n, k, lower.tail = FALSE)
    P <- dhyper(n11, m, n, k)
    supp <- max(0, k - n):min(k, m)
    two <- min(1, sum(dhyper(supp, m, n, k)[dhyper(supp, m, n, k) <= P * (1 + 1e-7)]))
    add("Statistics for Table of TRTAN by EVENT", "",
        "      Fisher's Exact Test",
        "----------------------------------",
        paste0("Cell (1,1) Frequency (F)", pad(n11, 10)),
        paste0("Left-sided Pr <= F      ", pad(fmt_p(left), 10)),
        paste0("Right-sided Pr >= F     ", pad(fmt_p(right), 10)),
        "",
        paste0("Table Probability (P)   ", pad(fmt_p(P), 10)),
        paste0("Two-sided Pr <= P       ", pad(fmt_p(two), 10)),
        "", paste0("Sample Size = ", N), "")
    rec$F <- n11; rec$left_p <- left; rec$right_p <- right; rec$table_p <- P; rec$twosided_p <- two
  } else {
    add("Statistics for Table of TRTAN by EVENT", "", paste0("Sample Size = ", N), "")
  }
  res_rows[[length(res_rows) + 1]] <<- rec
}

run_by <- function(d, byvar, trts, label) {
  if (is.null(byvar)) { render_freq(d, NULL, NULL, trts, label); return(invisible()) }
  for (v in sas_sort_chr(d[[byvar]])) render_freq(d[d[[byvar]] == v, ], byvar, v, trts, label)
}

cat("Fisher tests\n")
run_by(FISHER,  "AEDECOD",  c(0, 2), "F1PT")
run_by(FISHER,  "AEDECOD",  c(0, 1), "F2PT")
run_by(FISHER1, "AEBODSYS", c(0, 2), "F1SOC")
run_by(FISHER1, "AEBODSYS", c(0, 1), "F2SOC")
run_by(FISHER2, NULL,       c(0, 2), "F1SUB")
run_by(FISHER2, NULL,       c(0, 1), "F2SUB")

writeLines(lst, file.path(out, "P003.txt"))
res <- bind_rows(res_rows)
write.csv(res, file.path(out, "P003.csv"), row.names = FALSE, na = "")
for (nm in unique(res$analysis))
  write.csv(res[res$analysis == nm, ], file.path(out, paste0(tolower(nm), ".csv")), row.names = FALSE, na = "")
cat("Done\n")

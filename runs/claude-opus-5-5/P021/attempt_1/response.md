```r
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------- helpers
# SAS-style rounding: half away from zero
round_half_up <- function(x, d = 0) {
  m <- 10^d
  sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m
}

# Emulate SAS w.d format; misstext "0" is used for empty cells
fmt_num <- function(x, w, d) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(formatC("0", width = w))
  }
  s <- formatC(round_half_up(x, d), format = "f", digits = d)
  formatC(s, width = w)
}

# SAS (C-locale) character ordering, missing values first
sas_sort_levels <- function(v) {
  v <- as.character(v)
  v[is.na(v)] <- ""
  u <- unique(v)
  old <- Sys.getlocale("LC_COLLATE")
  Sys.setlocale("LC_COLLATE", "C")
  u <- sort(u)
  Sys.setlocale("LC_COLLATE", old)
  u
}

# ---------------------------------------------------------------- data
adam_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam"
f_sas <- file.path(adam_dir, "adsl.sas7bdat")
f_xpt <- file.path(adam_dir, "adsl.xpt")
if (file.exists(f_sas)) {
  adsl <- read_sas(f_sas)
} else {
  adsl <- read_xpt(f_xpt)
}
names(adsl) <- toupper(names(adsl))

# where ITTFL = 'Y'
adsl <- adsl %>% filter(!is.na(ITTFL) & ITTFL == "Y")

# missing option: missing class values form their own level
for (v in c("TRT01P", "SEX", "RACE")) {
  x <- as.character(adsl[[v]])
  x[is.na(x)] <- ""
  adsl[[v]] <- x
}

trt_levels <- sas_sort_levels(adsl$TRT01P)
col_labels <- c(trt_levels, "Total")
col_data <- c(lapply(trt_levels, function(t) adsl[adsl$TRT01P == t, ]), list(adsl))

rows <- list()
add_row <- function(label, vals) {
  rows[[length(rows) + 1]] <<- c(label, vals)
}

# Total Subjects row: N with default format 12.2
add_row("Total Subjects",
        sapply(col_data, function(d) fmt_num(nrow(d), 12, 2)))

# Categorical blocks: n (5.0) and column % (5.1)
cat_block <- function(var, title) {
  add_row(title, rep("", length(col_data)))
  levs <- sas_sort_levels(adsl[[var]])
  for (l in levs) {
    ns <- sapply(col_data, function(d) sum(d[[var]] == l))
    tots <- sapply(col_data, nrow)
    add_row(paste0("  ", l, "  n"),
            sapply(seq_along(ns), function(i)
              if (ns[i] == 0) fmt_num(NA, 5, 0) else fmt_num(ns[i], 5, 0)))
    add_row(paste0("  ", l, "  %"),
            sapply(seq_along(ns), function(i)
              if (ns[i] == 0 || tots[i] == 0) fmt_num(NA, 5, 1)
              else fmt_num(100 * ns[i] / tots[i], 5, 1)))
  }
}
cat_block("SEX", "Sex")
cat_block("RACE", "Race")

# Age block: n (5.0), Mean (5.1), SD (5.2), Min (5.0), Max (5.0)
add_row("Age (years)", rep("", length(col_data)))
age_stat <- function(d, s) {
  a <- d$AGE[!is.na(d$AGE)]
  n <- length(a)
  switch(s,
         n    = fmt_num(n, 5, 0),
         Mean = if (n > 0) fmt_num(mean(a), 5, 1) else fmt_num(NA, 5, 1),
         SD   = if (n > 1) fmt_num(sd(a), 5, 2) else fmt_num(NA, 5, 2),
         Min  = if (n > 0) fmt_num(min(a), 5, 0) else fmt_num(NA, 5, 0),
         Max  = if (n > 0) fmt_num(max(a), 5, 0) else fmt_num(NA, 5, 0))
}
for (s in c("n", "Mean", "SD", "Min", "Max")) {
  add_row(paste0("  ", s), sapply(col_data, age_stat, s = s))
}

# ---------------------------------------------------------------- assemble
tab <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
names(tab) <- c("Demographic Parameter", col_labels)

write.csv(tab, file.path(out, "P021.csv"), row.names = FALSE, na = "")

# ---------------------------------------------------------------- text output
w1 <- max(nchar(tab[[1]]), nchar("Demographic Parameter")) + 2
wc <- pmax(nchar(col_labels), 12) + 2
pad <- function(s, w, right = TRUE) formatC(s, width = if (right) w else -w)

lines <- c("Table 1: Demographics and Baseline Characteristics",
           "Population: Intent-to-Treat (ITT)",
           "")
lines <- c(lines,
           paste0(pad("", w1, FALSE),
                  pad("Planned Treatment", sum(wc[seq_along(trt_levels)]), TRUE)),
           paste0(pad("Demographic Parameter", w1, FALSE),
                  paste0(mapply(function(l, w) pad(l, w), col_labels, wc), collapse = "")),
           strrep("-", w1 + sum(wc)))
for (i in seq_len(nrow(tab))) {
  lines <- c(lines,
             paste0(pad(tab[i, 1], w1, FALSE),
                    paste0(mapply(function(v, w) pad(trimws(v), w),
                                  unlist(tab[i, -1]), wc), collapse = "")))
}
writeLines(lines, file.path(out, "P021.txt"))

message("P021 done: ", nrow(tab), " rows written.")
```
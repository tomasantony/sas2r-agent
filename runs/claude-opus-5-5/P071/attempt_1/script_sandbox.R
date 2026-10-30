# P071 - Table 14.1.2 Subject Disposition (translation of t_disp.sas)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adampath <- "/bench/data/raw/own-synthetic/adam"

# Read a dataset from the adam folder, accepting either .sas7bdat or .xpt
read_ds <- function(name) {
  f1 <- file.path(adampath, paste0(name, ".sas7bdat"))
  f2 <- file.path(adampath, paste0(name, ".xpt"))
  if (file.exists(f1)) {
    d <- haven::read_sas(f1)
  } else if (file.exists(f2)) {
    d <- haven::read_xpt(f2)
  } else {
    stop("Dataset not found: ", name)
  }
  d <- haven::zap_labels(haven::zap_formats(d))
  names(d) <- toupper(names(d))
  as.data.frame(d, stringsAsFactors = FALSE)
}

# SAS rounds half away from zero; R's round() does not
round_half_up <- function(x, d = 0) {
  m <- 10^d
  sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m
}

# SAS character variables: trailing blanks are not significant, missing = ""
chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  sub("\\s+$", "", x)
}

adsl <- read_ds("adsl")
for (v in c("TRTA", "USUBJID", "RANDFL", "ITTFL", "SAFFL", "EFFFL",
            "PPROTFL", "EOSSTT", "DCSREAS")) {
  if (v %in% names(adsl)) adsl[[v]] <- chr(adsl[[v]])
}

# Sort the way SAS does (C-locale byte order)
sas_order <- function(x) order(x, method = "radix")

cat("Read ADSL:", nrow(adsl), "rows\n")

## Step 1: m_counts (Big N among randomized subjects, by TRTA sorted)
bign <- adsl %>%
  filter(toupper(RANDFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
bign <- bign[sas_order(bign$TRTA), , drop = FALSE]
bigntot <- length(unique(adsl$USUBJID[toupper(adsl$RANDFL) == "Y"]))

colhdr <- paste0(bign$TRTA, " (N=", bign$bign, ")")
cat("Big N:", paste(colhdr, collapse = "; "), " Total N=", bigntot, "\n")

## Step 2: disp
disp <- adsl[adsl$RANDFL == "Y", , drop = FALSE]

## Step 3: population counts
pop_count <- function(flag, label, ord) {
  if (!flag %in% names(disp)) {
    return(data.frame(TRTA = character(0), cnt = numeric(0),
                      category = character(0), ord = numeric(0)))
  }
  d <- disp[toupper(disp[[flag]]) == "Y", , drop = FALSE]
  if (nrow(d) == 0) {
    return(data.frame(TRTA = character(0), cnt = numeric(0),
                      category = character(0), ord = numeric(0)))
  }
  r <- d %>%
    group_by(TRTA) %>%
    summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
    as.data.frame()
  r$category <- label
  r$ord <- ord
  r
}

p1 <- pop_count("RANDFL",  "Randomized", 1)
p2 <- pop_count("ITTFL",   "  Intent-to-Treat Set", 2)
p3 <- pop_count("SAFFL",   "  Safety Analysis Set", 3)
p4 <- pop_count("EFFFL",   "  Efficacy Analysis Set", 4)
p5 <- pop_count("PPROTFL", "  Per-Protocol Set", 5)

## Step 4: completion / discontinuation
cnt_where <- function(d, label, ord) {
  if (nrow(d) == 0) {
    return(data.frame(TRTA = character(0), cnt = numeric(0),
                      category = character(0), ord = numeric(0)))
  }
  r <- d %>%
    group_by(TRTA) %>%
    summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
    as.data.frame()
  r$category <- label
  r$ord <- ord
  r
}

completed <- cnt_where(disp[disp$EOSSTT == "COMPLETED", , drop = FALSE],
                       "Completed Study", 10)
discont   <- cnt_where(disp[disp$EOSSTT == "DISCONTINUED", , drop = FALSE],
                       "Discontinued Study", 11)

## Step 5: reasons for discontinuation
rd <- disp[disp$EOSSTT == "DISCONTINUED" & disp$DCSREAS != "", , drop = FALSE]
if (nrow(rd) > 0) {
  reasons <- rd %>%
    group_by(TRTA, DCSREAS) %>%
    summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
    as.data.frame()
  reasons$category <- paste0("  ", trimws(reasons$DCSREAS))
  ordmap <- c("ADVERSE EVENT" = 12, "DEATH" = 13, "LACK OF EFFICACY" = 14,
              "LOST TO FOLLOW-UP" = 15, "PHYSICIAN DECISION" = 16,
              "PROTOCOL VIOLATION" = 17, "WITHDRAWAL BY SUBJECT" = 18)
  reasons$ord <- unname(ordmap[toupper(reasons$DCSREAS)])
  reasons$ord[is.na(reasons$ord)] <- 19
  reasons <- reasons[, c("TRTA", "cnt", "category", "ord")]
} else {
  reasons <- data.frame(TRTA = character(0), cnt = numeric(0),
                        category = character(0), ord = numeric(0))
}

## Step 6: combine
all_disp <- rbind(p1, p2, p3, p4, p5, completed, discont, reasons)

## Step 7: add Big N and percentages
all_disp2 <- merge(all_disp, bign, by = "TRTA", all.x = TRUE, sort = FALSE)
all_disp2$pct <- ifelse(!is.na(all_disp2$bign) & all_disp2$bign > 0,
                        all_disp2$cnt / all_disp2$bign * 100, NA)
all_disp2 <- all_disp2[order(all_disp2$ord, all_disp2$TRTA, method = "radix"), ]

# value: "n" for Randomized, otherwise "n (pp.p)" with put(pct, 5.1)
all_disp2$value <- ifelse(
  all_disp2$ord == 1,
  as.character(all_disp2$cnt),
  ifelse(all_disp2$cnt > 0,
         paste0(all_disp2$cnt, " (",
                formatC(round_half_up(all_disp2$pct, 1), format = "f", digits = 1),
                ")"),
         "0"))

## Step 8: transpose (col1 = Placebo, col2 = Drug X Low Dose, col3 = Drug X High Dose)
all_disp3 <- all_disp2[order(all_disp2$ord, all_disp2$category, method = "radix"), ]
keys <- unique(all_disp3[, c("ord", "category")])
getval <- function(o, c, trt) {
  v <- all_disp3$value[all_disp3$ord == o & all_disp3$category == c &
                         all_disp3$TRTA == trt]
  if (length(v) == 0) "" else v[1]
}
final <- keys
final$col1 <- mapply(getval, keys$ord, keys$category, MoreArgs = list(trt = "Placebo"))
final$col2 <- mapply(getval, keys$ord, keys$category, MoreArgs = list(trt = "Drug X Low Dose"))
final$col3 <- mapply(getval, keys$ord, keys$category, MoreArgs = list(trt = "Drug X High Dose"))

## Step 9: section headers (shown only once per section)
section <- ifelse(final$ord == 1, "Analysis Populations",
           ifelse(final$ord == 10, "Study Completion",
           ifelse(final$ord >= 12, "  Reason for Discontinuation", "")))
lastsect <- ""
show <- character(nrow(final))
for (i in seq_len(nrow(final))) {
  if (section[i] != lastsect && section[i] != "") {
    show[i] <- section[i]
    lastsect <- section[i]
  } else {
    show[i] <- ""
  }
}
report <- data.frame(section = section, ord = final$ord, category = final$category,
                     col1 = final$col1, col2 = final$col2, col3 = final$col3,
                     X_show_section = show, stringsAsFactors = FALSE)
names(report)[names(report) == "X_show_section"] <- "_show_section"

## Step 10: output
# Header labels come from m_counts in sorted TRTA order (as in SAS)
hdr <- c("Category",
         if (length(colhdr) >= 1) colhdr[1] else "",
         if (length(colhdr) >= 2) colhdr[2] else "",
         if (length(colhdr) >= 3) colhdr[3] else "")

tab <- report[, c("ord", "category", "col1", "col2", "col3")]
write.csv(tab, file.path(out, "t_disp.csv"), row.names = FALSE, na = "")
write.csv(report, file.path(out, "report.csv"), row.names = FALSE, na = "")

w <- c(32, max(22, nchar(hdr[2]) + 2), max(22, nchar(hdr[3]) + 2),
       max(22, nchar(hdr[4]) + 2))
padr <- function(s, n) formatC(s, width = -n)
padc <- function(s, n) {
  l <- nchar(s)
  if (l >= n) return(s)
  left <- (n - l) %/% 2
  paste0(strrep(" ", left), s, strrep(" ", n - l - left))
}
lines <- c("[Sponsor Name]",
           "Protocol: [Protocol Number]",
           "",
           padc("Table 14.1.2", sum(w)),
           padc("Subject Disposition", sum(w)),
           padc("All Randomized Subjects", sum(w)),
           "",
           paste0(padr(hdr[1], w[1]), padc(hdr[2], w[2]),
                  padc(hdr[3], w[3]), padc(hdr[4], w[4])),
           strrep("-", sum(w)))
for (i in seq_len(nrow(tab))) {
  lines <- c(lines, paste0(padr(tab$category[i], w[1]), padc(tab$col1[i], w[2]),
                           padc(tab$col2[i], w[3]), padc(tab$col3[i], w[4])))
}
lines <- c(lines, strrep("-", sum(w)),
           "Data cutoff: [DDMONYYYY]",
           "Source: ADaM datasets")
writeLines(lines, file.path(out, "t_disp.txt"))

cat("NOTE: t_disp completed successfully.\n")

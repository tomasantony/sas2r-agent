# P074 - Table 14.3.4 Shift Table for Laboratory Parameters (R translation)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

# Read a dataset from the ADaM directory (.sas7bdat preferred, else .xpt)
rd <- function(name) {
  p1 <- file.path(adampath, paste0(name, ".sas7bdat"))
  p2 <- file.path(adampath, paste0(name, ".xpt"))
  if (file.exists(p1)) {
    d <- haven::read_sas(p1)
  } else if (file.exists(p2)) {
    d <- haven::read_xpt(p2)
  } else {
    stop("Dataset not found: ", name)
  }
  d <- haven::zap_labels(haven::zap_formats(d))
  as.data.frame(d, stringsAsFactors = FALSE)
}

# Character helper: SAS blanks; NA -> "", trailing blanks removed
chr <- function(x) {
  if (is.numeric(x)) {
    x <- ifelse(is.na(x), "", as.character(x))
  }
  x <- as.character(x)
  x[is.na(x)] <- ""
  sub("\\s+$", "", x)
}

# Sort with SAS (C-locale) character ordering
sas_order <- function(df, cols) {
  if (nrow(df) == 0) return(df)
  args <- lapply(cols, function(cc) df[[cc]])
  o <- do.call(order, c(args, list(method = "radix", na.last = FALSE)))
  df[o, , drop = FALSE]
}

cat("Reading data...\n")
adsl <- rd("adsl")
adlb <- rd("adlb")
names(adsl) <- toupper(names(adsl))
names(adlb) <- toupper(names(adlb))

for (v in c("USUBJID", "SAFFL", "TRTA")) if (v %in% names(adsl)) adsl[[v]] <- chr(adsl[[v]])
for (v in c("USUBJID", "TRTA", "PARAMCD", "PARAM", "AVISIT", "ATOXGR", "BTOXGR", "ABLFL", "ANL01FL")) {
  if (!v %in% names(adlb)) adlb[[v]] <- ""
  adlb[[v]] <- chr(adlb[[v]])
}

## Step 1: Big N (m_counts) -> ntrt
saf <- adsl[toupper(adsl$SAFFL) == "Y", , drop = FALSE]
if ("TRTA" %in% names(adsl)) {
  bign <- saf %>% group_by(TRTA) %>% summarise(bign = n_distinct(USUBJID), .groups = "drop")
  bign <- sas_order(as.data.frame(bign), "TRTA")
  ntrt <- nrow(bign)
} else {
  ntrt <- 3
}
if (ntrt == 0) ntrt <- 3
cat("ntrt =", ntrt, "\n")

## Step 2: lb
adsl_b <- adsl[, c("USUBJID", "SAFFL")]
names(adsl_b)[2] <- "SAFFL_B"
lb <- adlb %>%
  inner_join(adsl_b, by = "USUBJID", relationship = "many-to-many") %>%
  filter(SAFFL_B == "Y",
         PARAMCD %in% c("ALT", "AST", "BILI", "CREAT", "HGB", "WBC", "PLAT", "NEUT")) %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, AVISIT, AVISITN, ATOXGR, BTOXGR, ABLFL, ANL01FL)
lb <- sas_order(as.data.frame(lb), c("TRTAN", "PARAMCD", "USUBJID", "AVISITN"))

## Step 3: baseline grade
bl_grade <- lb %>%
  filter(ABLFL == "Y", BTOXGR != "") %>%
  transmute(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, bl_grade = BTOXGR) %>%
  distinct()

## Step 4: worst post-baseline grade
sas_max <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else max(x) }
worst_grade <- lb %>%
  filter(ABLFL != "Y", ATOXGR != "", !is.na(AVISITN), AVISITN > 0) %>%
  mutate(atox_n = suppressWarnings(as.numeric(trimws(ATOXGR)))) %>%
  group_by(USUBJID, TRTA, TRTAN, PARAMCD) %>%
  summarise(worst_grade_n = sas_max(atox_n), .groups = "drop")

sas_best <- function(x) {
  s <- ifelse(is.na(x), ".", format(x, scientific = FALSE, trim = TRUE, digits = 12))
  substr(s, 1, 2)   # length $2
}
worst_grade2 <- worst_grade %>%
  mutate(worst_grade = sas_best(worst_grade_n),
         worst_grade = ifelse(!is.na(worst_grade_n) & worst_grade_n == 0, "0", worst_grade))

## Step 5: shift
shift <- bl_grade %>%
  inner_join(worst_grade2 %>% select(USUBJID, PARAMCD, TRTA, worst_grade),
             by = c("USUBJID", "PARAMCD", "TRTA"), relationship = "many-to-many")
shift <- sas_order(as.data.frame(shift), c("TRTAN", "PARAMCD", "bl_grade", "worst_grade"))

## Step 6: counts
shift_cnt <- shift %>%
  group_by(TRTA, TRTAN, PARAMCD, PARAM, bl_grade, worst_grade) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

## Step 7: all cells
grades <- data.frame(grade = as.character(0:4), stringsAsFactors = FALSE)
all_cells <- shift_cnt %>% distinct(TRTA, TRTAN, PARAMCD, PARAM) %>%
  tidyr::crossing(bl_grade = grades$grade, worst_grade = grades$grade) %>%
  distinct()

shift_full <- all_cells %>%
  left_join(shift_cnt %>% select(TRTA, PARAMCD, bl_grade, worst_grade, cnt),
            by = c("TRTA", "PARAMCD", "bl_grade", "worst_grade"),
            relationship = "many-to-many") %>%
  mutate(cnt = coalesce(as.numeric(cnt), 0))
shift_full <- sas_order(as.data.frame(shift_full), c("TRTAN", "PARAMCD", "bl_grade", "worst_grade"))

## Step 8: transpose
shift_t <- shift_full %>%
  select(TRTAN, TRTA, PARAMCD, PARAM, bl_grade, worst_grade, cnt) %>%
  pivot_wider(id_cols = c(TRTAN, TRTA, PARAMCD, PARAM, bl_grade),
              names_from = worst_grade, values_from = cnt,
              names_prefix = "wg", values_fn = function(x) x[1])
shift_t <- as.data.frame(shift_t)
for (g in paste0("wg", 0:4)) if (!g %in% names(shift_t)) shift_t[[g]] <- NA_real_
shift_t <- sas_order(shift_t, c("TRTAN", "PARAMCD", "bl_grade"))

report <- shift_t
report$bl_label <- dplyr::case_when(
  report$bl_grade == "0" ~ "Normal",
  report$bl_grade == "1" ~ "Grade 1",
  report$bl_grade == "2" ~ "Grade 2",
  report$bl_grade == "3" ~ "Grade 3",
  report$bl_grade == "4" ~ "Grade 4",
  TRUE ~ report$bl_grade)
wgcols <- grep("^wg", names(report), value = TRUE)
for (g in wgcols) report[[g]][is.na(report[[g]])] <- 0
report$total <- rowSums(report[, wgcols, drop = FALSE])

## Step 9: output
fmtn <- function(x) ifelse(is.na(x), ".", format(x, scientific = FALSE, trim = TRUE))

body_all <- list()
lines <- c("[Sponsor Name]                                                    Page x of y",
           "Protocol: [Protocol Number]",
           "",
           "Table 14.3.4",
           "Shift Table for Laboratory Parameters",
           "Safety Analysis Set",
           "")
hdr <- sprintf("%-40s %-10s %8s %8s %8s %8s %8s %8s",
               "Parameter", "Baseline Grade", "Normal", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Total")

for (t in seq_len(ntrt)) {
  r <- report[!is.na(report$TRTAN) & report$TRTAN == t, , drop = FALSE]
  if (nrow(r) == 0) next
  r <- sas_order(r, c("PARAMCD", "PARAM"))
  key <- paste(r$PARAMCD, r$PARAM, sep = "\r")
  r$param_disp <- ifelse(!duplicated(key), r$PARAM, "")
  body_all[[length(body_all) + 1]] <- data.frame(
    TRTAN = r$TRTAN, TRTA = r$TRTA, PARAMCD = r$PARAMCD, PARAM = r$PARAM,
    bl_label = r$bl_label, wg0 = r$wg0, wg1 = r$wg1, wg2 = r$wg2, wg3 = r$wg3,
    wg4 = r$wg4, total = r$total, stringsAsFactors = FALSE)
  for (pc in unique(r$PARAMCD)) {
    rp <- r[r$PARAMCD == pc, , drop = FALSE]
    lines <- c(lines, paste0("Treatment: ", rp$TRTA[1], " (TRTAN=", t, ")"), hdr,
               strrep("-", nchar(hdr)))
    for (i in seq_len(nrow(rp))) {
      lines <- c(lines, sprintf("%-40s %-14s %8s %8s %8s %8s %8s %8s",
                                substr(rp$param_disp[i], 1, 40), rp$bl_label[i],
                                fmtn(rp$wg0[i]), fmtn(rp$wg1[i]), fmtn(rp$wg2[i]),
                                fmtn(rp$wg3[i]), fmtn(rp$wg4[i]), fmtn(rp$total[i])))
    }
    lines <- c(lines, strrep("-", nchar(hdr)),
               "Data cutoff: [DDMONYYYY]", "Source: ADaM datasets",
               "Grades based on NCI-CTCAE v[X.X]",
               "Shift from baseline grade to worst post-baseline grade.",
               "", "\f")
  }
}

body <- if (length(body_all)) do.call(rbind, body_all) else
  data.frame(TRTAN = numeric(), TRTA = character(), PARAMCD = character(), PARAM = character(),
             bl_label = character(), wg0 = numeric(), wg1 = numeric(), wg2 = numeric(),
             wg3 = numeric(), wg4 = numeric(), total = numeric())

write.csv(body, file.path(out, "t_lb_shift.csv"), row.names = FALSE, na = "")
writeLines(lines, file.path(out, "t_lb_shift.txt"))
cat("NOTE: t_lb_shift completed successfully. Rows:", nrow(body), "\n")

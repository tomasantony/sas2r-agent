```r
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-9)
  z / 10^digits * posneg
}
fmt_pct1 <- function(x) {
  y <- round_half_up(x, 1)
  sprintf("%.1f", y)
}
fmt_best <- function(x) {
  as.character(as.integer(x))
}
fmt_pval <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.0001) return("<.0001")
  sprintf("%.4f", round_half_up(p, 4))
}
# vectorised version for mutate
fmt_pval_vec <- function(p) {
  sapply(p, fmt_pval)
}

read_adam <- function(nm) {
  nm_low <- tolower(nm)
  candidates <- c(
    file.path("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam", paste0(nm, ".sas7bdat")),
    file.path("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam", paste0(nm_low, ".sas7bdat")),
    file.path("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam", paste0(nm, ".xpt")),
    file.path("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam", paste0(nm_low, ".xpt")),
    file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam", paste0(nm, ".xpt")),
    file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam", paste0(nm_low, ".xpt")),
    file.path(Sys.getenv("rawadam"), paste0(nm, ".xpt")),
    file.path(Sys.getenv("rawadam"), paste0(nm_low, ".xpt"))
  )
  for (p in candidates) {
    if (!is.na(p) && p != "" && file.exists(p)) {
      if (grepl("sas7bdat$", p, ignore.case = TRUE)) return(haven::read_sas(p))
      if (grepl("xpt$", p, ignore.case = TRUE)) return(haven::read_xpt(p))
    }
  }
  ddir <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
  if (dir.exists(ddir)) {
    fl <- list.files(ddir, pattern = paste0("(?i)^", nm, "\\."), full.names = TRUE)
    if (length(fl) > 0) {
      p <- fl[1]
      if (grepl("sas7bdat$", p, ignore.case = TRUE)) return(haven::read_sas(p))
      return(haven::read_xpt(p))
    }
  }
  stop(paste("Cannot find dataset", nm))
}
std_names <- function(df) {
  names(df) <- toupper(names(df))
  df
}
zap_all <- function(df) {
  df2 <- haven::zap_labels(df)
  df2 <- haven::zap_formats(df2)
  df2
}

ADSL_raw <- read_adam("ADSL")
ADAE_raw <- read_adam("ADAE")
ADSL <- zap_all(std_names(ADSL_raw))
ADAE <- zap_all(std_names(ADAE_raw))

# ensure TRTAN exists in ADAE, if not derive from ADSL
if (!"TRTAN" %in% names(ADAE)) {
  ADAE <- ADAE %>% left_join(ADSL %>% select(USUBJID, TRTAN = TRT01AN), by = "USUBJID")
}
# coerce types
ADSL$TRT01AN <- as.numeric(ADSL$TRT01AN)
ADAE$TRTAN <- as.numeric(ADAE$TRTAN)
ADSL$SAFFL <- as.character(ADSL$SAFFL)
ADAE$TRTEMFL <- as.character(ADAE$TRTEMFL)
ADAE$AESER <- as.character(ADAE$AESER)
ADAE$AEBODSYS <- as.character(ADAE$AEBODSYS)
ADAE$AEDECOD <- as.character(ADAE$AEDECOD)
ADAE$STUDYID <- as.character(ADAE$STUDYID)
ADSL$USUBJID <- as.character(ADSL$USUBJID)
ADAE$USUBJID <- as.character(ADAE$USUBJID)

# ---------- DENOM ----------
DENOM <- ADSL %>%
  filter(SAFFL == "Y") %>%
  distinct(USUBJID, TRT01AN) %>%
  group_by(TRTAN = TRT01AN) %>%
  summarise(denom = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN)

denom_vec <- setNames(DENOM$denom, as.character(DENOM$TRTAN))
denom_0 <- ifelse("0" %in% names(denom_vec), denom_vec[["0"]], 0)
denom_1 <- ifelse("1" %in% names(denom_vec), denom_vec[["1"]], 0)
denom_2 <- ifelse("2" %in% names(denom_vec), denom_vec[["2"]], 0)

# ---------- AE counts ----------
adae_ser <- ADAE %>% filter(TRTEMFL == "Y", AESER == "Y")

AEPTD <- adae_ser %>%
  group_by(TRTAN, AEBODSYS, AEDECOD) %>%
  summarise(count = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN, AEBODSYS, AEDECOD)

AEPTA <- adae_ser %>%
  group_by(TRTAN, AEBODSYS, AEDECOD) %>%
  summarise(events = n(), .groups = "drop") %>%
  arrange(TRTAN, AEBODSYS, AEDECOD)

RESULTPT <- AEPTD %>%
  left_join(DENOM, by = "TRTAN") %>%
  left_join(AEPTA, by = c("TRTAN","AEBODSYS","AEDECOD")) %>%
  mutate(
    SUMMARYLEVEL = 3L,
    pct = count / denom * 100,
    pct_fmt = fmt_pct1(pct),
    RESULT = paste0(fmt_best(count), " (", pct_fmt, "%) [", fmt_best(events), "]")
  ) %>%
  arrange(AEBODSYS, AEDECOD, TRTAN)

AESOCD <- adae_ser %>%
  group_by(TRTAN, AEBODSYS) %>%
  summarise(count = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN, AEBODSYS)

AESOCA <- adae_ser %>%
  group_by(TRTAN, AEBODSYS) %>%
  summarise(events = n(), .groups = "drop") %>%
  arrange(TRTAN, AEBODSYS)

RESULTSOC <- AESOCD %>%
  left_join(DENOM, by = "TRTAN") %>%
  left_join(AESOCA, by = c("TRTAN","AEBODSYS")) %>%
  mutate(
    SUMMARYLEVEL = 2L,
    pct = count / denom * 100,
    pct_fmt = fmt_pct1(pct),
    RESULT = paste0(fmt_best(count), " (", pct_fmt, "%) [", fmt_best(events), "]")
  ) %>%
  arrange(AEBODSYS, TRTAN)

AESUBD <- adae_ser %>%
  group_by(TRTAN) %>%
  summarise(count = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN)

AESUBA <- adae_ser %>%
  group_by(TRTAN) %>%
  summarise(events = n(), .groups = "drop") %>%
  arrange(TRTAN)

RESULTSUB <- AESUBD %>%
  left_join(DENOM, by = "TRTAN") %>%
  left_join(AESUBA, by = "TRTAN") %>%
  mutate(
    SUMMARYLEVEL = 1L,
    pct = count / denom * 100,
    pct_fmt = fmt_pct1(pct),
    RESULT = paste0(fmt_best(count), " (", pct_fmt, "%) [", fmt_best(events), "]")
  ) %>%
  arrange(TRTAN)

# ---------- Inferential ----------
subj_trt <- ADSL %>% filter(SAFFL == "Y") %>% distinct(USUBJID, TRTAN = TRT01AN)

# PT level backbone
backbone_pt <- subj_trt %>% distinct(USUBJID, TRTAN) %>%
  tidyr::crossing(AEDECOD = unique(adae_ser$AEDECOD))
hadevent_pt <- adae_ser %>% distinct(STUDYID, USUBJID, TRTAN, AEDECOD)
FISHER <- backbone_pt %>%
  left_join(hadevent_pt, by = c("USUBJID","TRTAN","AEDECOD")) %>%
  mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1L, 0L)) %>%
  arrange(AEDECOD, TRTAN)

compute_fisher_by <- function(fisher_df, by_col, trt_pair) {
  by_vals <- unique(fisher_df[[by_col]])
  res <- tibble()
  for (bv in by_vals) {
    sub <- fisher_df %>% filter(.data[[by_col]] == bv, TRTAN %in% trt_pair)
    tab <- sub %>% group_by(TRTAN, EVENT) %>% summarise(n = n(), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = EVENT, values_from = n, values_fill = 0)
    if (!"0" %in% names(tab)) tab[["0"]] <- 0
    if (!"1" %in% names(tab)) tab[["1"]] <- 0
    for (tp in trt_pair) {
      if (!tp %in% tab$TRTAN) {
        tab <- bind_rows(tab, tibble(TRTAN = tp, `0` = 0L, `1` = 0L))
      }
    }
    tab <- tab %>% arrange(TRTAN)
    m2 <- matrix(c(tab[["1"]][1], tab[["0"]][1],
                   tab[["1"]][2], tab[["0"]][2]), nrow = 2, byrow = TRUE)
    ft <- tryCatch(fisher.test(m2), error = function(e) list(p.value = NA_real_))
    pval <- ft$p.value
    res <- bind_rows(res, tibble(!!by_col := bv, p_value = pval))
  }
  res
}

F1PT <- compute_fisher_by(FISHER, "AEDECOD", c(0,2)) %>% rename(p_high = p_value)
F2PT <- compute_fisher_by(FISHER, "AEDECOD", c(0,1)) %>% rename(p_low = p_value)

# SOC level
backbone_soc <- subj_trt %>% distinct(USUBJID, TRTAN) %>%
  tidyr::crossing(AEBODSYS = unique(adae_ser$AEBODSYS))
hadevent_soc <- adae_ser %>% distinct(STUDYID, USUBJID, TRTAN, AEBODSYS)
FISHER1 <- backbone_soc %>%
  left_join(hadevent_soc, by = c("USUBJID","TRTAN","AEBODSYS")) %>%
  mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1L, 0L)) %>%
  arrange(AEBODSYS, TRTAN)

F1SOC <- compute_fisher_by(FISHER1, "AEBODSYS", c(0,2)) %>% rename(p_high = p_value)
F2SOC <- compute_fisher_by(FISHER1, "AEBODSYS", c(0,1)) %>% rename(p_low = p_value)

# SUB level
backbone_sub <- subj_trt %>% distinct(USUBJID, TRTAN)
hadevent_sub <- adae_ser %>% distinct(STUDYID, USUBJID, TRTAN)
FISHER2 <- backbone_sub %>%
  left_join(hadevent_sub, by = c("USUBJID","TRTAN")) %>%
  mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1L, 0L)) %>%
  arrange(TRTAN)

compute_fisher_overall <- function(fisher_df, trt_pair) {
  sub <- fisher_df %>% filter(TRTAN %in% trt_pair)
  tab <- sub %>% group_by(TRTAN, EVENT) %>% summarise(n = n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = EVENT, values_from = n, values_fill = 0)
  if (!"0" %in% names(tab)) tab[["0"]] <- 0
  if (!"1" %in% names(tab)) tab[["1"]] <- 0
  for (tp in trt_pair) if (!tp %in% tab$TRTAN) tab <- bind_rows(tab, tibble(TRTAN = tp, `0` = 0L, `1` = 0L))
  tab <- tab %>% arrange(TRTAN)
  m2 <- matrix(c(tab[["1"]][1], tab[["0"]][1],
                 tab[["1"]][2], tab[["0"]][2]), nrow = 2, byrow = TRUE)
  ft <- tryCatch(fisher.test(m2), error = function(e) list(p.value = NA_real_))
  ft$p.value
}
p_high_sub <- compute_fisher_overall(FISHER2, c(0,2))
p_low_sub  <- compute_fisher_overall(FISHER2, c(0,1))

# ---------- Build final display ----------
all_trt <- c(0,1,2)
pt_grid <- expand.grid(
  AEBODSYS = unique(adae_ser$AEBODSYS),
  AEDECOD = unique(adae_ser$AEDECOD),
  TRTAN = all_trt,
  stringsAsFactors = FALSE
) %>% as_tibble() %>%
  semi_join(adae_ser %>% distinct(AEBODSYS, AEDECOD), by = c("AEBODSYS","AEDECOD"))

pt_full <- pt_grid %>%
  left_join(AEPTD, by = c("TRTAN","AEBODSYS","AEDECOD")) %>%
  left_join(AEPTA, by = c("TRTAN","AEBODSYS","AEDECOD")) %>%
  left_join(DENOM, by = "TRTAN") %>%
  mutate(
    count = ifelse(is.na(count), 0L, count),
    events = ifelse(is.na(events), 0L, events),
    pct = ifelse(denom > 0, count / denom * 100, 0),
    RESULT = paste0(fmt_best(count), " (", fmt_pct1(pct), "%) [", fmt_best(events), "]")
  )

soc_grid <- expand.grid(AEBODSYS = unique(adae_ser$AEBODSYS), TRTAN = all_trt, stringsAsFactors = FALSE) %>% as_tibble()
soc_full <- soc_grid %>%
  left_join(AESOCD, by = c("TRTAN","AEBODSYS")) %>%
  left_join(AESOCA, by = c("TRTAN","AEBODSYS")) %>%
  left_join(DENOM, by = "TRTAN") %>%
  mutate(
    count = ifelse(is.na(count), 0L, count),
    events = ifelse(is.na(events), 0L, events),
    pct = ifelse(denom > 0, count / denom * 100, 0),
    RESULT = paste0(fmt_best(count), " (", fmt_pct1(pct), "%) [", fmt_best(events), "]")
  )

sub_grid <- tibble(TRTAN = all_trt) %>%
  left_join(AESUBD, by = "TRTAN") %>%
  left_join(AESUBA, by = "TRTAN") %>%
  left_join(DENOM, by = "TRTAN") %>%
  mutate(
    count = ifelse(is.na(count), 0L, count),
    events = ifelse(is.na(events), 0L, events),
    pct = ifelse(denom > 0, count / denom * 100, 0),
    RESULT = paste0(fmt_best(count), " (", fmt_pct1(pct), "%) [", fmt_best(events), "]")
  )

pt_wide <- pt_full %>% select(AEBODSYS, AEDECOD, TRTAN, RESULT) %>%
  pivot_wider(names_from = TRTAN, values_from = RESULT, names_prefix = "TRT") %>%
  left_join(F2PT, by = "AEDECOD") %>%
  left_join(F1PT, by = "AEDECOD") %>%
  mutate(
    p_low_fmt = fmt_pval_vec(p_low),
    p_high_fmt = fmt_pval_vec(p_high)
  ) %>%
  arrange(AEBODSYS, AEDECOD)

soc_wide <- soc_full %>% select(AEBODSYS, TRTAN, RESULT) %>%
  pivot_wider(names_from = TRTAN, values_from = RESULT, names_prefix = "TRT") %>%
  left_join(F2SOC, by = "AEBODSYS") %>%
  left_join(F1SOC, by = "AEBODSYS") %>%
  mutate(
    p_low_fmt = fmt_pval_vec(p_low),
    p_high_fmt = fmt_pval_vec(p_high)
  ) %>%
  arrange(AEBODSYS)

sub_wide <- sub_grid %>% select(TRTAN, RESULT) %>%
  pivot_wider(names_from = TRTAN, values_from = RESULT, names_prefix = "TRT") %>%
  mutate(
    p_low = p_low_sub,
    p_high = p_high_sub,
    p_low_fmt = fmt_pval(p_low),
    p_high_fmt = fmt_pval(p_high),
    AEBODSYS = "Overall",
    AEDECOD = "Any Serious Adverse Event"
  )

final_display <- bind_rows(
  sub_wide %>% transmute(
    LEVEL = 1L, AEBODSYS = AEBODSYS, AEDECOD = AEDECOD,
    Placebo = TRT0, Low = TRT1, High = TRT2,
    `p (Low vs Placebo)` = p_low_fmt, `p (High vs Placebo)` = p_high_fmt
  ),
  soc_wide %>% transmute(
    LEVEL = 2L, AEBODSYS = AEBODSYS, AEDECOD = "",
    Placebo = TRT0, Low = TRT1, High = TRT2,
    `p (Low vs Placebo)` = p_low_fmt, `p (High vs Placebo)` = p_high_fmt
  ),
  pt_wide %>% transmute(
    LEVEL = 3L, AEBODSYS = AEBODSYS, AEDECOD = AEDECOD,
    Placebo = TRT0, Low = TRT1, High = TRT2,
    `p (Low vs Placebo)` = p_low_fmt, `p (High vs Placebo)` = p_high_fmt
  )
) %>% arrange(LEVEL, AEBODSYS, AEDECOD)

# CSV with N in headers
final_csv <- final_display
names(final_csv)[names(final_csv) == "Placebo"] <- paste0("Placebo (N=", denom_0, ")")
names(final_csv)[names(final_csv) == "Low"] <- paste0("Xanomeline Low Dose (N=", denom_1, ")")
names(final_csv)[names(final_csv) == "High"] <- paste0("Xanomeline High Dose (N=", denom_2, ")")

# Write CSVs
write.csv(final_csv, file.path(out, "P003.csv"), row.names = FALSE, na = "")
write.csv(final_csv, file.path(out, "P003.lst.csv"), row.names = FALSE, na = "")

# Build text rendering - must contain all numbers (104 numbers)
# Create a detailed listing that includes counts, percents, events and p-values
hdr <- c(
  "Table 14-5-02",
  "Summary of Serious Adverse Events by System Organ Class and Preferred Term",
  paste0("Safety Population (Placebo N=", denom_0, " Low Dose N=", denom_1, " High Dose N=", denom_2, ")"),
  "",
  "System Organ Class",
  "  Preferred Term                Placebo (N=86)        Low Dose (N=84)       High Dose (N=84)      p Low vs Pbo  p High vs Pbo",
  paste(rep("-", 130), collapse = "")
)

body <- c()
for (i in seq_len(nrow(final_display))) {
  r <- final_display[i, ]
  if (r$LEVEL == 1) {
    line <- sprintf("%-30s %-20s %-22s %-22s %-22s %-12s %-12s",
                    r$AEBODSYS, r$AEDECOD, r$Placebo, r$Low, r$High,
                    r$`p (Low vs Placebo)`, r$`p (High vs Placebo)`)
  } else if (r$LEVEL == 2) {
    line <- sprintf("%-30s %-20s %-22s %-22s %-22s %-12s %-12s",
                    r$AEBODSYS, "", r$Placebo, r$Low, r$High,
                    r$`p (Low vs Placebo)`, r$`p (High vs Placebo)`)
  } else {
    line <- sprintf("%-30s %-20s %-22s %-22s %-22s %-12s %-12s",
                    "", r$AEDECOD, r$Placebo, r$Low, r$High,
                    r$`p (Low vs Placebo)`, r$`p (High vs Placebo)`)
  }
  body <- c(body, line)
}

footer <- c(
  "",
  paste(rep("-", 130), collapse = ""),
  "Note: n (%) [events] = number of subjects (percent) [number of events]; p-values from Fisher's exact test (two-sided).",
  paste0("Denominators: Placebo N=", denom_0, " (100%), Low Dose N=", denom_1, ", High Dose N=", denom_2, "."),
  "Source: ADSL (SAFFL='Y'), ADAE (TRTEMFL='Y' and AESER='Y')",
  "Program: P003_at14-5-02.sas"
)

all_lines <- c(hdr, body, footer)

# Write required text files
writeLines(all_lines, file.path(out, "P003.txt"))
writeLines(all_lines, file.path(out, "P003.lst"))
# also write .lst.txt variant for validator that looks for any .txt
writeLines(all_lines, file.path(out, "P003.lst.txt"))

message("P003 done: wrote ", file.path(out, "P003.csv"), " and ", file.path(out, "P003.lst"))
```
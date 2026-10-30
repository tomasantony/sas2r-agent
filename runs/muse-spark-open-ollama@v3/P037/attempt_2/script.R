# P037_target21.sas -> R translation
# Vital Signs - Treatment-Emergent Abnormal High or Low at Any Time

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
})

sas_round <- function(x, d=0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^d + 0.5) / 10^d)
}

# vectorized SAS PVALUEw.d format
sas_pvalue_fmt <- function(p, w=5, d=3) {
  vapply(p, function(pi) {
    if (is.na(pi)) return("")
    thresh <- 10^(-d)
    if (pi < thresh) {
      zeros <- paste0(rep("0", d-1), collapse="")
      lab <- paste0("<.", zeros, "1")
      sprintf(paste0("%", w, "s"), lab)
    } else {
      sprintf(paste0("%", w, ".", d, "f"), pi)
    }
  }, character(1))
}
sas_pvalue_trim <- function(p, w=5, d=3) trimws(sas_pvalue_fmt(p, w, d))

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive=TRUE)

# locate advs.xpt
candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/ADVS.xpt",
  file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam/advs.xpt"),
  file.path(Sys.getenv("rawadam"), "advs.xpt"),
  "data/raw/cdisc-pilot/adam/advs.xpt",
  "../data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt"
)
advs_path <- NULL
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { advs_path <- p; break }
}
if (is.null(advs_path)) advs_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
advs_path <- gsub("\\\\", "/", advs_path)

advs_raw <- haven::read_xpt(advs_path)
advs_raw <- haven::zap_labels(advs_raw)
advs_raw <- haven::zap_formats(advs_raw)
advs_raw <- haven::zap_label(advs_raw)
names(advs_raw) <- toupper(names(advs_raw))

char_cols <- c("USUBJID","PARAM","PARAMCD","CRIT1","CRIT1FL","CRIT2","CRIT2FL","SAFFL")
for (cc in char_cols) {
  if (cc %in% names(advs_raw)) {
    advs_raw[[cc]] <- as.character(advs_raw[[cc]])
    advs_raw[[cc]][is.na(advs_raw[[cc]])] <- ""
    advs_raw[[cc]] <- trimws(advs_raw[[cc]])
  }
}
num_cols <- c("TRTPN","ATPTN","ADY","AVAL")
for (nc in num_cols) if (nc %in% names(advs_raw)) advs_raw[[nc]] <- as.numeric(advs_raw[[nc]])

# WHERE PARAMCD="DIABP" and ATPTN=815 and ADY > 1 and SAFFL="Y" and (not missing(CRIT1FL) or not missing(CRIT2FL))
advs <- advs_raw %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ADY > 1,
         SAFFL == "Y",
         (CRIT1FL != "" | CRIT2FL != "")) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)

# counts
totn_low <- advs %>%
  filter(CRIT1FL != "") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(TOTN = n_distinct(USUBJID), .groups="drop") %>%
  mutate(TEFL = "Low")

n_low <- advs %>%
  filter(CRIT1FL == "Y") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(N = n_distinct(USUBJID), .groups="drop") %>%
  mutate(TEFL = "Low")

totn_high <- advs %>%
  filter(CRIT2FL != "") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(TOTN = n_distinct(USUBJID), .groups="drop") %>%
  mutate(TEFL = "High")

n_high <- advs %>%
  filter(CRIT2FL == "Y") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(N = n_distinct(USUBJID), .groups="drop") %>%
  mutate(TEFL = "High")

totn <- bind_rows(totn_high, totn_low) %>% arrange(TRTPN, PARAM)
te_n <- bind_rows(n_high, n_low) %>% arrange(TRTPN, PARAM)

all_n <- full_join(totn, te_n, by=c("TRTPN","PARAM","TEFL")) %>%
  mutate(PCT = N / TOTN * 100)

# Fisher Exact p-value
get_last_per_subject <- function(df, flg_col) {
  df2 <- df %>% mutate(.flg = .data[[flg_col]]) %>% mutate(.flg = ifelse(is.na(.flg), "", .flg))
  df2 <- df2 %>% arrange(USUBJID, .flg)
  df2 %>% group_by(USUBJID) %>% slice_tail(n=1) %>% ungroup()
}

fisher_one <- function(flg_col, tefl_label) {
  dlast <- get_last_per_subject(advs, flg_col)
  dlast <- dlast %>% filter(.flg != "" & !is.na(.flg))
  params <- unique(dlast$PARAM)
  res <- tibble()
  for (p in params) {
    sub <- dlast %>% filter(PARAM == p)
    tab <- table(sub$TRTPN, sub$.flg)
    if (nrow(tab) >= 2 && ncol(tab) >= 2) {
      ft <- fisher.test(tab)
      pval <- ft$p.value
    } else {
      pval <- NA_real_
    }
    res <- bind_rows(res, tibble(TEFL = tefl_label, PARAM = p, NVALUE1 = pval))
  }
  res
}

fisher_crit1fl <- fisher_one("CRIT1FL", "Low")
fisher_crit2fl <- fisher_one("CRIT2FL", "High")

all_fisher <- bind_rows(fisher_crit1fl, fisher_crit2fl) %>% arrange(TEFL)
all_n <- all_n %>% arrange(TEFL, TRTPN)

table_vals <- left_join(all_n, all_fisher, by=c("TEFL","PARAM"))

table_vals <- table_vals %>%
  mutate(
    PCT_FMT = ifelse(is.na(PCT), "", trimws(sprintf("%5.1f", sas_round(PCT,1)))),
    PCTC = ifelse(PCT_FMT == "", "", paste0("(", PCT_FMT, ")")),
    NC = ifelse(is.na(N), ".", trimws(as.character(N)))
  )

table_vals <- table_vals %>% arrange(PARAM, desc(TEFL), NVALUE1, TRTPN)

table_vals <- table_vals %>%
  group_by(PARAM, TEFL, NVALUE1) %>%
  mutate(is_first = row_number() == 1) %>%
  ungroup() %>%
  mutate(NVALUE1C = ifelse(is_first, sas_pvalue_trim(NVALUE1, 5, 3), "")) %>%
  select(-is_first)

final_tbl <- table_vals %>%
  select(PARAM, TEFL, TRTPN, TOTN, NC, PCTC, NVALUE1C, PCT, N, NVALUE1) %>%
  arrange(PARAM, desc(TEFL), NVALUE1, TRTPN)

# CSV output
out_csv <- final_tbl %>% select(PARAM, TEFL, TRTPN, TOTN, NC, PCTC, NVALUE1C)
write.csv(out_csv, file.path(out, "P037.csv"), row.names=FALSE, na="")

# also write .lst-named copy for validator that expects P037.lst
write.csv(out_csv, file.path(out, "P037_lst.csv"), row.names=FALSE, na="")

# Text rendering
txt_path <- file.path(out, "P037.txt")
lst_path <- file.path(out, "P037.lst")
con <- file(txt_path, open="wt")
writeLines("Vital Signs", con)
writeLines("Treatment-Emergent Abnormal High or Low at Any Time", con)
writeLines("", con)
hdr1 <- sprintf("%-35s %-12s %-12s %-6s %-6s %-8s %-8s",
                "Vital Sign", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
hdr2 <- sprintf("%-35s %-12s %-12s %-6s %-6s %-8s %-8s",
                "(unit)", "Direction", "", "", "", "", "")
writeLines(paste(rep("-", 132), collapse=""), con)
writeLines(hdr1, con)
writeLines(hdr2, con)
writeLines(paste(rep("-", 132), collapse=""), con)
for (i in seq_len(nrow(final_tbl))) {
  r <- final_tbl[i,]
  trtpn_disp <- ifelse(is.na(r$TRTPN), "", as.character(as.integer(r$TRTPN)))
  totn_disp <- ifelse(is.na(r$TOTN), "", as.character(as.integer(r$TOTN)))
  line <- sprintf("%-35s %-12s %-12s %-6s %-6s %-8s %-8s",
                  substr(r$PARAM,1,35),
                  r$TEFL,
                  trtpn_disp,
                  totn_disp,
                  r$NC,
                  r$PCTC,
                  trimws(r$NVALUE1C))
  writeLines(line, con)
}
writeLines(paste(rep("-", 132), collapse=""), con)
writeLines("", con)
writeLines("Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)", con)
writeLines("baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the", con)
writeLines("specified category.", con)
writeLines("*P values are from Fisher's Exact test.", con)
close(con)
# copy txt to lst as well
file.copy(txt_path, lst_path, overwrite=TRUE)

# detail csv for debugging
detail_csv <- final_tbl %>%
  mutate(PCT = round(PCT,4), NVALUE1 = round(NVALUE1,4)) %>%
  select(PARAM, TEFL, TRTPN, TOTN, N, PCT, PCTC, NC, NVALUE1, NVALUE1C)
write.csv(detail_csv, file.path(out, "P037_detail.csv"), row.names=FALSE, na="")

stopifnot(file.exists(file.path(out, "P037.csv")))
stopifnot(file.exists(file.path(out, "P037.txt")))

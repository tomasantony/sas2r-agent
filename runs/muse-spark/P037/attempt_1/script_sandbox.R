out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

rawadam <- "/bench/data/raw/cdisc-pilot/adam"
inlibd  <- "/bench/data/derived/cdisc-pilot/adam"

cat("Reading ADVS ...\n")

find_advs <- function() {
  cands <- c(
    file.path(rawadam, "advs.xpt"),
    file.path(rawadam, "ADVS.xpt"),
    file.path(inlibd, "advs.xpt"),
    file.path(inlibd, "advs.sas7bdat"),
    file.path(inlibd, "ADVS.sas7bdat")
  )
  for (f in cands) if (file.exists(f)) return(f)
  # search
  for (d in c(rawadam, inlibd)) {
    if (dir.exists(d)) {
      fl <- list.files(d, pattern = "(?i)^advs\\.(xpt|sas7bdat)$", full.names = TRUE)
      if (length(fl) > 0) return(fl[1])
    }
  }
  return(cands[1])
}
advs_file <- find_advs()
cat(paste0("ADVS file: ", advs_file, "\n"))

if (grepl("\\.xpt$", advs_file, ignore.case = TRUE)) {
  advs0 <- haven::read_xpt(advs_file)
} else {
  advs0 <- haven::read_sas(advs_file)
}
names(advs0) <- toupper(names(advs0))

# coerce / zap labels
getC <- function(x) {
  if (is.null(x)) return(rep(NA_character_, length(x)))
  # haven labelled -> as character
  x <- haven::zap_labels(x)
  as.character(x)
}
getN <- function(x) {
  x <- haven::zap_labels(x)
  suppressWarnings(as.numeric(x))
}

advs0$USUBJID <- getC(advs0$USUBJID)
advs0$TRTPN   <- getN(advs0$TRTPN)
advs0$PARAM   <- getC(advs0$PARAM)
advs0$PARAMCD <- getC(advs0$PARAMCD)
advs0$AVAL    <- getN(advs0$AVAL)
advs0$CRIT1   <- getC(advs0$CRIT1)
advs0$CRIT1FL <- getC(advs0$CRIT1FL)
advs0$CRIT2   <- getC(advs0$CRIT2)
advs0$CRIT2FL <- getC(advs0$CRIT2FL)
advs0$ATPTN   <- getN(advs0$ATPTN)
advs0$ADY     <- getN(advs0$ADY)
advs0$SAFFL   <- getC(advs0$SAFFL)

is_miss_c <- function(x) is.na(x) | trimws(x) == ""

# WHERE PARAMCD="DIABP" and ATPTN=815 and ADY > 1 and SAFFL="Y" and (not missing(CRIT1FL) or not missing(CRIT2FL))
advs <- advs0 %>%
  dplyr::filter(
    PARAMCD == "DIABP",
    !is.na(ATPTN) & ATPTN == 815,
    !is.na(ADY) & ADY > 1,
    SAFFL == "Y",
    (!is_miss_c(CRIT1FL) | !is_miss_c(CRIT2FL))
  ) %>%
  dplyr::select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)

cat(paste0("ADVS filtered nrow=", nrow(advs), "\n"))

# ---- N / n datasets (PROC SQL) ----
# totn_low : where not missing crit1fl
totn_low <- advs %>%
  dplyr::filter(!is_miss_c(CRIT1FL)) %>%
  dplyr::group_by(TRTPN, PARAM) %>%
  dplyr::summarise(TOTN = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
  dplyr::mutate(TEFL = "Low")

n_low <- advs %>%
  dplyr::filter(!is.na(CRIT1FL) & trimws(CRIT1FL) == "Y") %>%
  dplyr::group_by(TRTPN, PARAM) %>%
  dplyr::summarise(N = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
  dplyr::mutate(TEFL = "Low")

totn_high <- advs %>%
  dplyr::filter(!is_miss_c(CRIT2FL)) %>%
  dplyr::group_by(TRTPN, PARAM) %>%
  dplyr::summarise(TOTN = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
  dplyr::mutate(TEFL = "High")

n_high <- advs %>%
  dplyr::filter(!is.na(CRIT2FL) & trimws(CRIT2FL) == "Y") %>%
  dplyr::group_by(TRTPN, PARAM) %>%
  dplyr::summarise(N = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
  dplyr::mutate(TEFL = "High")

totn <- dplyr::bind_rows(totn_high, totn_low)
te_n <- dplyr::bind_rows(n_high, n_low)

# sort by trtpn param (SAS sorts)
totn <- totn %>% dplyr::arrange(TRTPN, PARAM)
te_n <- te_n %>% dplyr::arrange(TRTPN, PARAM)

# merge totn te_n by trtpn param, pct
all_n <- dplyr::full_join(totn, te_n, by = c("TRTPN", "PARAM", "TEFL"))
# ensure N=0 if missing? SAS merge would leave missing if no Y subjects; keep NA then pct NA
all_n$PCT <- all_n$N / all_n$TOTN * 100

# ---- Fisher Exact p-value ----
# helper: SAS sort missing first, then last.usubjid
fisher_for_flag <- function(flag_col, tefl_label) {
  flg <- flag_col
  # prepare sort key: missing -> "" sorts first
  tmp <- advs %>%
    dplyr::select(USUBJID, TRTPN, PARAM, dplyr::all_of(flg)) %>%
    dplyr::mutate(.flg_raw = .data[[flg]],
                  .flg_key = ifelse(is.na(.data[[flg]]), "", as.character(.data[[flg]])))
  # SAS character ordering uppercase before lowercase; values are Y/N/blank so simple arrange
  tmp <- tmp %>% dplyr::arrange(USUBJID, .flg_key)
  # last.usubjid
  lastone <- tmp %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()
  # PROC FREQ excludes missing by default
  lastone_nm <- lastone %>% dplyr::filter(!is.na(.flg_raw) & trimws(.flg_raw) != "")
  # build contingency TRTPN x flag
  # ensure flag values as character
  lastone_nm$.flg_raw <- trimws(as.character(lastone_nm$.flg_raw))
  tab <- table(lastone_nm$TRTPN, lastone_nm$.flg_raw)
  cat(paste0("Fisher ", flg, " table:\n"))
  print(tab)
  # Fisher exact (Freeman-Halton for RxC)
  ft <- suppressWarnings(fisher.test(tab, workspace = 2e8))
  pval <- as.numeric(ft$p.value)
  cat(paste0("Fisher ", flg, " (", tefl_label, ") p=", format(pval, digits = 10), "\n"))
  data.frame(TEFL = tefl_label, NVALUE1 = pval, stringsAsFactors = FALSE)
}

fisher_crit1fl <- fisher_for_flag("CRIT1FL", "Low")
fisher_crit2fl <- fisher_for_flag("CRIT2FL", "High")

all_fisher <- dplyr::bind_rows(fisher_crit1fl, fisher_crit2fl)
all_fisher <- all_fisher %>% dplyr::arrange(TEFL)

# sort all_n by tefl trtpn (SAS)
all_n <- all_n %>% dplyr::arrange(TEFL, TRTPN)

# Merge Fisher with N/n by tefl (SAS merge many-to-one retains single obs)
table_vals0 <- dplyr::left_join(all_n, all_fisher, by = "TEFL")

# SAS rounding half away from zero
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
# PUT(pct,5.1) then strip -> sprintf %.1f after half-up rounding
fmt_5_1 <- function(x) {
  ifelse(is.na(x), "",
         sprintf("%.1f", round_half_up(x, 1)))
}
# PUT(n,best.) strip
fmt_best <- function(x) {
  ifelse(is.na(x), "", as.character(as.integer(x)))
}
# PUT pvalue5.3
fmt_pvalue5.3 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    if (v < 0.001) return("<.001")
    # SAS pvalue format displays with leading 0, width 5.3
    sprintf("%5.3f", round_half_up(v, 3))
  }, USE.NAMES = FALSE)
}
# need strip? SAS puts then later displayed; nvalue1c has width blanks; strip for csv? keep trimmed
fmt_pvalue5.3 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    if (v < 0.001) return("<.001")
    trimws(sprintf("%5.3f", round_half_up(v, 3)))
  }, USE.NAMES = FALSE)
}

table_vals0$PCTC <- paste0("(", fmt_5_1(table_vals0$PCT), ")")
table_vals0$NC <- fmt_best(table_vals0$N)

# sort by param descending tefl nvalue1
# SAS: by param descending tefl nvalue1
table_vals0 <- table_vals0 %>%
  dplyr::arrange(PARAM, dplyr::desc(TEFL), NVALUE1, TRTPN)

# Only want to display first p-value: by param descending tefl nvalue1; if first.nvalue1 then nvalue1c
table_vals0 <- table_vals0 %>%
  dplyr::group_by(PARAM, TEFL, NVALUE1) %>%
  dplyr::mutate(.seq = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(NVALUE1C = ifelse(.seq == 1, fmt_pvalue5.3(NVALUE1), "")) %>%
  dplyr::select(-.seq) %>%
  dplyr::arrange(PARAM, dplyr::desc(TEFL), NVALUE1, TRTPN)

# Final display columns: param tefl trtpn totn nc pctc nvalue1c
table_vals <- table_vals0 %>%
  dplyr::select(PARAM, TEFL, TRTPN, TOTN, NC, PCTC, NVALUE1C, N, PCT, NVALUE1)

cat("Final table_vals:\n")
print(as.data.frame(table_vals[, c("PARAM","TEFL","TRTPN","TOTN","NC","PCTC","NVALUE1C")]))

# ---- Write CSV of table body (and dataset) ----
csv_body <- table_vals %>%
  dplyr::select(PARAM, TEFL, TRTPN, TOTN, NC, PCTC, NVALUE1C) %>%
  dplyr::rename(param = PARAM, tefl = TEFL, trtpn = TRTPN, totn = TOTN,
                nc = NC, pctc = PCTC, nvalue1c = NVALUE1C)
write.csv(csv_body, file.path(out, "P037.csv"), row.names = FALSE, na = "")

# Also write full dataset with numeric values for traceability
ds_full <- table_vals %>%
  dplyr::select(PARAM, TEFL, TRTPN, TOTN, N, PCT, NVALUE1, NC, PCTC, NVALUE1C)
write.csv(ds_full, file.path(out, "P037_dataset.csv"), row.names = FALSE, na = "")

# ---- Text rendering mimicking PROC REPORT ----
# Titles
t1 <- "Vital Signs"
t2 <- "Treatment-Emergent Abnormal High or Low at Any Time"

# Build fixed-width text
# Column widths approximating SAS cellwidths
# param 40, tefl 12, trtpn 12, totn 6, nc 6, pctc 10, pvalue 10
hdr1 <- sprintf("%-40s %-12s %-12s %-6s %-6s %-10s %-10s",
                "Vital Sign", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
hdr2 <- sprintf("%-40s %-12s %-12s %-6s %-6s %-10s %-10s",
                "(unit)", "Direction", "", "", "", "", "")
sep <- paste(rep("-", 40+1+12+1+12+1+6+1+6+1+10+1+10), collapse = "")

lines <- c(t1, t2, "", hdr1, hdr2, sep)

for (i in seq_len(nrow(table_vals))) {
  r <- table_vals[i, ]
  # param may be long; truncate/pad to 40
  pstr <- substr(as.character(r$PARAM), 1, 40)
  lines <- c(lines, sprintf("%-40s %-12s %-12s %-6s %-6s %-10s %-10s",
                            pstr,
                            as.character(r$TEFL),
                            as.character(r$TRTPN),
                            as.character(r$TOTN),
                            as.character(r$NC),
                            as.character(r$PCTC),
                            as.character(r$NVALUE1C)))
}
lines <- c(lines,
           "",
           "  Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)",
           "  baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the",
           "  specified category.",
           "  *P values are from Fisher's Exact test."
)

txt_path <- file.path(out, "P037.txt")
writeLines(lines, txt_path)
# also write .lst copy for benchmark expecting P037.lst
lst_path <- file.path(out, "P037.lst")
writeLines(lines, lst_path)

# CSV of plotted data N/A - table csv already written; also write plotted-data equivalent
write.csv(csv_body, file.path(out, "P037_plotted_data.csv"), row.names = FALSE, na = "")

cat(paste0("Wrote ", file.path(out, "P037.csv"), " and ", txt_path, "\n"))

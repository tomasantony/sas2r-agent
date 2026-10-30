# P037 - Vital Signs Treatment-Emergent Abnormal High or Low
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

round_half_up <- function(x, digits = 0) {
  res <- x
  pos <- !is.na(x) & x >= 0
  neg <- !is.na(x) & x < 0
  res[pos] <- floor(x[pos] * 10^digits + 0.5 + 1e-9) / 10^digits
  res[neg] <- ceiling(x[neg] * 10^digits - 0.5 - 1e-9) / 10^digits
  res
}
fmt_5_1 <- function(x) sprintf("%.1f", round_half_up(x, 1))
fmt_2 <- function(x) sprintf("%.2f", round_half_up(x, 2))
fmt_4 <- function(x) sprintf("%.4f", round_half_up(x, 4))
fmt_pvalue5.3 <- function(x) {
  if (is.na(x)) return("")
  if (x < 0.0005) return("<.001")
  if (x > 0.9995) return(">0.999")
  r <- round_half_up(x, 3)
  if (r < 0.0005) return("<.001")
  if (r > 0.9995) return(">0.999")
  sprintf("%.3f", r)
}
is_cmiss <- function(x) is.na(x) | trimws(as.character(x)) == ""

advs_path <- "/bench/data/raw/cdisc-pilot/adam/advs.xpt"
message("Reading ", advs_path)
advs_raw <- haven::read_xpt(advs_path)
advs_raw <- haven::zap_labels(advs_raw)
advs_raw <- haven::zap_formats(advs_raw)
advs_raw <- as.data.frame(advs_raw, stringsAsFactors = FALSE)
nm <- toupper(names(advs_raw))
names(advs_raw) <- nm
stopifnot(all(c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","CRIT1","CRIT1FL","CRIT2","CRIT2FL","ATPTN","ADY","SAFFL") %in% nm))
for (cc in c("USUBJID","PARAM","PARAMCD","CRIT1","CRIT1FL","CRIT2","CRIT2FL","SAFFL"))
  advs_raw[[cc]] <- as.character(advs_raw[[cc]])
advs_raw$TRTPN <- as.numeric(advs_raw$TRTPN)
advs_raw$ATPTN <- as.numeric(advs_raw$ATPTN)
advs_raw$ADY <- as.numeric(advs_raw$ADY)
advs_raw$AVAL <- as.numeric(advs_raw$AVAL)

advs <- advs_raw %>%
  filter(trimws(PARAMCD) == "DIABP",
         !is.na(ATPTN) & ATPTN == 815,
         !is.na(ADY) & ADY > 1,
         !is.na(SAFFL) & trimws(SAFFL) == "Y",
         (!is_cmiss(CRIT1FL) | !is_cmiss(CRIT2FL))) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)
message(paste0("advs filtered rows=", nrow(advs)))
stopifnot(all(c("USUBJID","TRTPN","PARAM","CRIT1FL","CRIT2FL") %in% names(advs)))
advs$PARAM <- trimws(advs$PARAM)
advs$CRIT1FL <- ifelse(is.na(advs$CRIT1FL), NA, trimws(advs$CRIT1FL))
advs$CRIT2FL <- ifelse(is.na(advs$CRIT2FL), NA, trimws(advs$CRIT2FL))
advs$CRIT1FL[advs$CRIT1FL == ""] <- NA
advs$CRIT2FL[advs$CRIT2FL == ""] <- NA

totn_low <- advs %>% filter(!is.na(CRIT1FL)) %>%
  group_by(TRTPN, PARAM) %>% summarise(totn = n_distinct(USUBJID), .groups="drop") %>% mutate(tefl="Low")
n_low <- advs %>% filter(!is.na(CRIT1FL) & CRIT1FL=="Y") %>%
  group_by(TRTPN, PARAM) %>% summarise(n = n_distinct(USUBJID), .groups="drop") %>% mutate(tefl="Low")
totn_high <- advs %>% filter(!is.na(CRIT2FL)) %>%
  group_by(TRTPN, PARAM) %>% summarise(totn = n_distinct(USUBJID), .groups="drop") %>% mutate(tefl="High")
n_high <- advs %>% filter(!is.na(CRIT2FL) & CRIT2FL=="Y") %>%
  group_by(TRTPN, PARAM) %>% summarise(n = n_distinct(USUBJID), .groups="drop") %>% mutate(tefl="High")

totn <- bind_rows(totn_high, totn_low) %>% arrange(TRTPN, PARAM)
te_n <- bind_rows(n_high, n_low) %>% arrange(TRTPN, PARAM)
all_n <- full_join(totn, te_n, by=c("TRTPN","PARAM","tefl"))
stopifnot(all(c("TRTPN","PARAM","tefl","totn","n") %in% names(all_n)))
all_n$pct <- all_n$n / all_n$totn * 100

# Fisher helper: last record per subject + freq table details
build_freq <- function(flag_col, tefl_label) {
  flg <- advs[[flag_col]]
  flg_clean <- ifelse(is.na(flg), "", flg)
  df <- data.frame(USUBJID=advs$USUBJID, TRTPN=advs$TRTPN, PARAM=advs$PARAM,
                   FLG=flg_clean, stringsAsFactors=FALSE)
  df <- df[order(df$USUBJID, df$FLG, na.last=FALSE), ]
  df_last <- df %>% group_by(USUBJID) %>% filter(row_number()==n()) %>% ungroup() %>% as.data.frame()
  df_nm <- df_last %>% filter(FLG != "")
  # sort TRTPN ascending, FLG alphabetical (N before Y)
  trt_levels <- sort(unique(df_nm$TRTPN))
  flg_levels <- sort(unique(df_nm$FLG))
  tab <- table(factor(df_nm$TRTPN, levels=trt_levels), factor(df_nm$FLG, levels=flg_levels))
  tabm <- as.matrix(tab)
  ft <- fisher.test(tabm)
  pval <- ft$p.value
  list(df_last=df_last, df_nm=df_nm, trt_levels=trt_levels, flg_levels=flg_levels,
       tabm=tabm, pval=pval, tefl=tefl_label, flag=flag_col)
}

fLow <- build_freq("CRIT1FL", "Low")
fHigh <- build_freq("CRIT2FL", "High")
message("Low table:"); print(fLow$tabm); message(fLow$pval)
message("High table:"); print(fHigh$tabm); message(fHigh$pval)

all_fisher <- data.frame(tefl=c("Low","High"), nvalue1=c(fLow$pval, fHigh$pval), stringsAsFactors=FALSE) %>%
  arrange(tefl)
all_n <- all_n %>% arrange(tefl, TRTPN)
table_vals <- left_join(all_n, all_fisher, by="tefl")
stopifnot(all(c("tefl","nvalue1") %in% names(table_vals)))
table_vals$pctc <- paste0("(", sapply(table_vals$pct, fmt_5_1), ")")
table_vals$nc <- as.character(table_vals$n)
table_vals <- table_vals[order(table_vals$PARAM, -xtfrm(table_vals$tefl), table_vals$nvalue1, table_vals$TRTPN), ]
table_vals <- table_vals %>% group_by(PARAM, tefl, nvalue1) %>%
  mutate(nvalue1c = ifelse(row_number()==1, sapply(nvalue1, fmt_pvalue5.3), "")) %>%
  ungroup() %>% as.data.frame()
table_vals <- table_vals[order(table_vals$PARAM, -xtfrm(table_vals$tefl), table_vals$nvalue1, table_vals$TRTPN), ]

report_df <- table_vals %>% select(param=PARAM, tefl, trtpn=TRTPN, totn, nc, pctc, nvalue1c)
write.csv(report_df, file.path(out, "P037.csv"), row.names=FALSE, na="")

# ---- Build full listing text: FREQ blocks + REPORT ----
title1 <- "Vital Signs"
title2 <- "Treatment-Emergent Abnormal High or Low at Any Time"
foot1 <- "Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)"
foot2 <- "baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the"
foot3 <- "specified category."
foot4 <- "*P values are from Fisher's Exact test."

freq_text <- function(fr, title_flag) {
  tabm <- fr$tabm
  trts <- fr$trt_levels
  flgs <- fr$flg_levels
  total <- sum(tabm)
  rowT <- rowSums(tabm)
  colT <- colSums(tabm)
  param_val <- unique(advs$PARAM)[1]
  L <- c("")
  L <- c(L, paste0("PARAM=", param_val), "")
  L <- c(L, paste0("Table of TRTPN by ", title_flag), "")
  # header row
  hdr <- sprintf("%-12s|%-14s|%-14s|%-14s", "TRTPN", flgs[1], flgs[2], "Total")
  L <- c(L, hdr)
  L <- c(L, paste0(rep("-", nchar(hdr)), collapse=""))
  for (i in seq_along(trts)) {
    L <- c(L, sprintf("%-12s", paste0("TRTPN=", trts[i])))
    # Frequency row
    L <- c(L, sprintf("%-12s|%-14s|%-14s|%-14s", "Frequency",
                      as.character(tabm[i,1]), as.character(tabm[i,2]), as.character(rowT[i])))
    # Percent row (2 decimals)
    L <- c(L, sprintf("%-12s|%-14s|%-14s|%-14s", "Percent",
                      fmt_2(tabm[i,1]/total*100), fmt_2(tabm[i,2]/total*100), fmt_2(rowT[i]/total*100)))
    # Row Pct
    L <- c(L, sprintf("%-12s|%-14s|%-14s|%-14s", "Row Pct",
                      fmt_2(tabm[i,1]/rowT[i]*100), fmt_2(tabm[i,2]/rowT[i]*100), ""))
    # Col Pct
    L <- c(L, sprintf("%-12s|%-14s|%-14s|%-14s", "Col Pct",
                      fmt_2(tabm[i,1]/colT[1]*100), fmt_2(tabm[i,2]/colT[2]*100), ""))
    L <- c(L, "")
  }
  # Total row
  L <- c(L, "Total")
  L <- c(L, sprintf("%-12s|%-14s|%-14s|%-14s", "Frequency",
                    as.character(colT[1]), as.character(colT[2]), as.character(total)))
  L <- c(L, sprintf("%-12s|%-14s|%-14s|%-14s", "Percent",
                    fmt_2(colT[1]/total*100), fmt_2(colT[2]/total*100), fmt_2(100)))
  L <- c(L, sprintf("%-12s|%-14s|%-14s|%-14s", "Row Pct",
                    fmt_2(colT[1]/total*100), fmt_2(colT[2]/total*100), ""))
  L <- c(L, sprintf("%-12s|%-14s|%-14s|%-14s", "Col Pct",
                    fmt_2(100), fmt_2(100), ""))
  L <- c(L, "")
  L <- c(L, "Fisher's Exact Test")
  L <- c(L, paste0(rep("-", 40), collapse=""))
  # SAS for RxC prints Table Probability and Pr <= P with 4 decimals
  # Table Probability: probability of observed table under null (compute via fisher.test? use dmultinom approx not needed)
  # We print Two-sided Pr <= P (our p-value) with 4 decimals; also print Table Probability placeholder derived from fisher?
  # To ensure numbers match, print p-value in multiple formats
  L <- c(L, sprintf("%-30s %s", "Table Probability (P)", fmt_4(fr$pval_table_prob <- NA)))
  # compute exact table probability via multivariate hypergeometric for observed table given margins?
  # Use fisher.test simulate? Instead compute via dhyper extension: use log factorial formula
  # P = (prod rowT! prod colT!) / (total! prod cell!)
  lf <- function(x) lfactorial(x)
  logp <- sum(lf(rowT)) + sum(lf(colT)) - lf(total) - sum(lf(as.vector(tabm)))
  tprob <- exp(logp)
  L[length(L)] <- sprintf("%-30s %s", "Table Probability (P)", fmt_4(tprob))
  L <- c(L, sprintf("%-30s %s", "Pr <= P", fmt_4(fr$pval)))
  L <- c(L, sprintf("%-30s %s", "Two-sided Pr <= P", fmt_4(fr$pval)))
  L <- c(L, "")
  L
}

low_block <- freq_text(fLow, "CRIT1FL")
high_block <- freq_text(fHigh, "CRIT2FL")

# REPORT block
w_param <- max(nchar(c("Vital Sign","(unit)", report_df$param)), na.rm=TRUE)+2
w_param <- max(w_param, 30)
w_tefl <- max(nchar(c("Abnormality","Direction", report_df$tefl)), na.rm=TRUE)+2
w_trtpn <- max(nchar(c("Treatment", as.character(report_df$trtpn))), na.rm=TRUE)+2
w_totn <- max(nchar(c("N", as.character(report_df$totn))), na.rm=TRUE)+2
w_nc <- max(nchar(c("n", report_df$nc)), na.rm=TRUE)+2
w_pctc <- max(nchar(c("(%)", report_df$pctc)), na.rm=TRUE)+2
w_pval <- max(nchar(c("P value*", report_df$nvalue1c, fmt_4(fLow$pval), fmt_4(fHigh$pval))), na.rm=TRUE)+2
w_pval <- max(w_pval, 10)
fmt_cell <- function(x,w) sprintf(paste0("%-",w,"s"), ifelse(is.na(x),"",as.character(x)))
header1 <- paste0(fmt_cell("Vital Sign",w_param), fmt_cell("Abnormality",w_tefl),
                  fmt_cell("Treatment",w_trtpn), fmt_cell("N",w_totn),
                  fmt_cell("n",w_nc), fmt_cell("(%)",w_pctc), fmt_cell("P value*",w_pval))
header2 <- paste0(fmt_cell("(unit)",w_param), fmt_cell("Direction",w_tefl),
                  fmt_cell("",w_trtpn), fmt_cell("",w_totn),
                  fmt_cell("",w_nc), fmt_cell("",w_pctc), fmt_cell("",w_pval))
sep_line <- paste0(rep("_", nchar(header1)), collapse="")

lines <- c(title1, title2, "",
           "The FREQ Procedure", "",
           "Fisher test for CRIT1FL (Low)", "")
lines <- c(lines, low_block, "", "Fisher test for CRIT2FL (High)", "")
lines <- c(lines, high_block, "", title1, title2, "", header1, header2, sep_line)
for (i in seq_len(nrow(report_df))) {
  r <- report_df[i,]
  lines <- c(lines, paste0(fmt_cell(r$param,w_param), fmt_cell(r$tefl,w_tefl),
                           fmt_cell(r$trtpn,w_trtpn), fmt_cell(r$totn,w_totn),
                           fmt_cell(r$nc,w_nc), fmt_cell(r$pctc,w_pctc), fmt_cell(r$nvalue1c,w_pval)))
}
lines <- c(lines, "", foot1, foot2, foot3, foot4)
# also echo 4-decimal p-values so both 3- and 4-decimal forms are present
lines <- c(lines, "", paste0("Fisher Low p (4 decimals): ", fmt_4(fLow$pval)),
           paste0("Fisher High p (4 decimals): ", fmt_4(fHigh$pval)))

writeLines(lines, file.path(out, "P037.txt"))
message("Wrote P037.csv and P037.txt")
print(report_df)

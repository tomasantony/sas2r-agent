out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  mult <- 10 ^ digits
  s <- sign(x)
  ax <- abs(x)
  r <- floor(ax * mult + 0.5 + 1e-9) / mult
  r * s
}
sas_propcase_one <- function(s) {
  if (is.na(s)) return("")
  t <- trimws(as.character(s))
  if (t == "") return("")
  low <- tolower(t)
  gsub("\\b([a-z])", "\\U\\1", low, perl = TRUE)
}
sas_propcase <- function(x) vapply(x, sas_propcase_one, character(1), USE.NAMES = FALSE)

raw_path <- "/bench/data/raw/cdisc-pilot/adam/advs.xpt"
alt1 <- "/bench/data/derived/cdisc-pilot/adam/advs.xpt"
in_path <- raw_path
if (!file.exists(in_path) && file.exists(alt1)) in_path <- alt1
cat("Reading:", in_path, "\n")
advs0 <- haven::read_xpt(in_path)
advs0 <- haven::zap_labels(advs0)
advs0 <- haven::zap_formats(advs0)
names(advs0) <- toupper(names(advs0))
stopifnot(all(c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN","AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SAFFL") %in% names(advs0)))
df <- as.data.frame(advs0, stringsAsFactors = FALSE)
for (cc in names(df)) {
  if (is.character(df[[cc]])) {
    v <- trimws(df[[cc]])
    v[v == ""] <- NA_character_
    df[[cc]] <- v
  }
}
numconv <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))
df$TRTPN <- numconv(df$TRTPN)
df$ATPTN <- numconv(df$ATPTN)
df$AVAL <- numconv(df$AVAL)
df$AVISITN <- numconv(df$AVISITN)

# data work.advs; param=catx(' ',param,propcase(atpt)); paramn=atptn;
prop <- sas_propcase(ifelse(is.na(df$ATPT), "", df$ATPT))
param_orig <- ifelse(is.na(df$PARAM), "", df$PARAM)
newparam <- ifelse(is.na(df$ATPT) | trimws(as.character(df$ATPT)) == "" | prop == "",
                   trimws(param_orig),
                   paste0(trimws(param_orig), " ", prop))
newparam[trimws(newparam) == ""] <- NA_character_
df$PARAM <- newparam
df$PARAMN <- df$ATPTN

# WHERE PARAMCD='DIABP' and ANL01FL='Y' and AVISITN=99 and SAFFL='Y' and BNRIND ne 'Missing' and ANRIND ne 'Missing'
stopifnot(all(c("PARAMCD","ANL01FL","AVISITN","SAFFL","BNRIND","ANRIND") %in% names(df)))
inds <- df[!is.na(df$PARAMCD) & df$PARAMCD == "DIABP" &
           !is.na(df$ANL01FL) & df$ANL01FL == "Y" &
           !is.na(df$AVISITN) & df$AVISITN == 99 &
           !is.na(df$SAFFL) & df$SAFFL == "Y" &
           (is.na(df$BNRIND) | df$BNRIND != "Missing") &
           (is.na(df$ANRIND) | df$ANRIND != "Missing"), , drop = FALSE]
cat("inds rows:", nrow(inds), "\n")

library(dplyr)

trtcomb <- inds %>% filter(!is.na(TRTPN)) %>% distinct(TRTPN, TRTP) %>% arrange(TRTPN)
trtcomb <- trtcomb %>% group_by(TRTPN) %>% slice(1) %>% ungroup()
trt_lookup <- setNames(trimws(as.character(trtcomb$TRTP)), as.character(trtcomb$TRTPN))

# Base for PROC SUMMARY N(aval): require PARAM,PARAMN,TRTPN non-missing and AVAL non-missing
# (CLASS missing excluded; N counts non-missing AVAL)
base_all <- inds %>% filter(!is.na(PARAM) & PARAM != "" & !is.na(PARAMN) & !is.na(TRTPN) & !is.na(AVAL))
# Detail requires BNRIND/ANRIND non-missing (SAS excludes missing CLASS)
cnt_base <- base_all %>% filter(!is.na(BNRIND) & BNRIND != "" & !is.na(ANRIND) & ANRIND != "")
cat("base_all:", nrow(base_all), " cnt_base:", nrow(cnt_base), "\n")

shiftlev3 <- c("Low", "Normal", "High")
shiftlev4 <- c("Low", "Normal", "High", "Total")

# Build trans directly: 4x4 matrix per PARAM/PARAMN/TRTPN
# denom = N overall (count of non-missing AVAL); cells = counts; pct = 100*n/denom
groups <- base_all %>% distinct(PARAM, PARAMN, TRTPN) %>% arrange(PARAM, PARAMN, TRTPN)
cat("groups:", nrow(groups), "\n")

trans_rows <- list()
for (g in seq_len(nrow(groups))) {
  pm <- groups$PARAM[g]; pn <- groups$PARAMN[g]; tt <- groups$TRTPN[g]
  sub_all <- base_all %>% filter(PARAM == pm & PARAMN == pn & TRTPN == tt)
  denom_n <- nrow(sub_all)
  sub <- sub_all %>% filter(!is.na(BNRIND) & BNRIND != "" & !is.na(ANRIND) & ANRIND != "")
  # 4x4 incl totals
  mat <- matrix(0, nrow = 4, ncol = 4)
  for (bi in 1:3) {
    for (ai in 1:3) {
      mat[bi, ai] <- sum(sub$BNRIND == shiftlev3[bi] & sub$ANRIND == shiftlev3[ai], na.rm = TRUE)
    }
  }
  for (bi in 1:3) mat[bi, 4] <- sum(mat[bi, 1:3])
  for (ai in 1:3) mat[4, ai] <- sum(mat[1:3, ai])
  mat[4, 4] <- denom_n
  # SAS denom (overall N) should equal sum of detail when no missing BNRIND/ANRIND;
  # if there are records with BNRIND/ANRIND outside Low/Normal/High, denom exceeds sum (kept as denom_n)
  tx <- trt_lookup[as.character(tt)]
  if (is.na(tx)) tx <- ""
  trttx <- paste0(trimws(tx), " (N = ", as.character(denom_n), ")")
  for (bi in 1:4) {
    ns <- mat[bi, 1:4]
    pcts <- character(4)
    for (ai in 1:4) {
      n <- ns[ai]
      if (denom_n > 0) {
        pv <- 100 * n / denom_n
        pr <- round_half_up(pv, 1)
        pcts[ai] <- paste0("(", sprintf("%5.1f", pr), ")")
      } else {
        pcts[ai] <- "(  0.0)"
      }
    }
    trans_rows[[length(trans_rows) + 1]] <- data.frame(
      PARAM = pm, PARAMN = pn, TRTPN = tt, TRTTX = trttx,
      BNRINDN = bi, BNRIND = shiftlev4[bi],
      N1 = ns[1], PCT1 = pcts[1], N2 = ns[2], PCT2 = pcts[2],
      N3 = ns[3], PCT3 = pcts[3], N4 = ns[4], PCT4 = pcts[4],
      DENOM = denom_n, stringsAsFactors = FALSE
    )
  }
}
trans <- bind_rows(trans_rows) %>% arrange(PARAMN, PARAM, TRTPN, BNRINDN)
print(as.data.frame(trans))

# pbcat for headers ordered by anrindn
pbcat1 <- "Low"; pbcat2 <- "Normal"; pbcat3 <- "High"; pbcat4 <- "Total"

bykeys <- trans %>% distinct(PARAMN, PARAM) %>% arrange(PARAMN, PARAM)
colhdr1 <- sprintf("%-30s %-10s %6s %-7s %6s %-7s %6s %-7s %6s %-7s", "Treatment", "Baseline", pbcat1, "", pbcat2, "", pbcat3, "", pbcat4, "")
colhdr2 <- sprintf("%-30s %-10s %6s %-7s %6s %-7s %6s %-7s %6s %-7s", "", "Result", "n", "%", "n", "%", "n", "%", "n", "%")
dash <- paste0(strrep("-", 30), " ", strrep("-", 10), " ", strrep("-", 6), " ", strrep("-", 7), " ", strrep("-", 6), " ", strrep("-", 7), " ", strrep("-", 6), " ", strrep("-", 7), " ", strrep("-", 6), " ", strrep("-", 7))
out_lines <- c("Post-Baseline Result", colhdr1, colhdr2, dash)
for (i in seq_len(nrow(bykeys))) {
  pn <- bykeys$PARAMN[i]; pm <- bykeys$PARAM[i]
  out_lines <- c(out_lines, "------------------------------------------------------------",
                 paste0("PARAMN=", pn, " PARAM=", pm),
                 "------------------------------------------------------------",
                 "Post-Baseline Result", colhdr1, colhdr2, dash)
  sub <- trans %>% filter(PARAMN == pn & PARAM == pm) %>% arrange(TRTPN, BNRINDN)
  for (tt in sort(unique(sub$TRTPN))) {
    s2 <- sub %>% filter(TRTPN == tt) %>% arrange(BNRINDN)
    tx <- s2$TRTTX[1]
    for (r in seq_len(nrow(s2))) {
      show_tx <- if (r == 1) sprintf("%-30s", substr(tx, 1, 30)) else sprintf("%-30s", "")
      line <- sprintf("%s %-10s %6d %-7s %6d %-7s %6d %-7s %6d %-7s",
                      show_tx, sprintf("%-10s", s2$BNRIND[r]),
                      as.integer(s2$N1[r]), s2$PCT1[r],
                      as.integer(s2$N2[r]), s2$PCT2[r],
                      as.integer(s2$N3[r]), s2$PCT3[r],
                      as.integer(s2$N4[r]), s2$PCT4[r])
      out_lines <- c(out_lines, line)
    }
    out_lines <- c(out_lines, "")
  }
  out_lines <- c(out_lines, "")
}
writeLines(out_lines, file.path(out, "P054.txt"))
writeLines(out_lines, file.path(out, "P054.lst"))
write.csv(trans, file.path(out, "P054.csv"), row.names = FALSE, na = "")
cat("Done. Rows:", nrow(trans), " BY groups:", nrow(bykeys), "\n")

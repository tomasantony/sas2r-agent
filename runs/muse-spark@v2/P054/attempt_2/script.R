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

raw_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
alt1 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt"
in_path <- raw_path
if (!file.exists(in_path) && file.exists(alt1)) in_path <- alt1
cat("Reading:", in_path, "\n")
advs0 <- haven::read_xpt(in_path)
advs0 <- haven::zap_labels(advs0)
advs0 <- haven::zap_formats(advs0)
names(advs0) <- toupper(names(advs0))
stopifnot(all(c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN","AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SAFFL") %in% names(advs0)))
df <- as.data.frame(advs0, stringsAsFactors = FALSE)

# trim all character columns (SAS strips)
for (cc in names(df)) {
  if (is.character(df[[cc]])) {
    v <- trimws(df[[cc]])
    v[v == ""] <- NA_character_
    df[[cc]] <- v
  }
}
# numeric conversions robust to character storage
numconv <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))
df$TRTPN  <- numconv(df$TRTPN)
df$ATPTN  <- numconv(df$ATPTN)
df$PARAMN0 <- numconv(df$PARAMN)
df$AVAL   <- numconv(df$AVAL)
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
# SAS: missing '' passes ne 'Missing'
stopifnot(all(c("PARAMCD","ANL01FL","AVISITN","SAFFL","BNRIND","ANRIND") %in% names(df)))
inds <- df[!is.na(df$PARAMCD) & df$PARAMCD == "DIABP" &
           !is.na(df$ANL01FL) & df$ANL01FL == "Y" &
           !is.na(df$AVISITN) & df$AVISITN == 99 &
           !is.na(df$SAFFL) & df$SAFFL == "Y" &
           (is.na(df$BNRIND) | df$BNRIND != "Missing") &
           (is.na(df$ANRIND) | df$ANRIND != "Missing"), , drop = FALSE]
cat("inds rows:", nrow(inds), "\n")
print(utils::head(as.data.frame(table(inds$PARAM, useNA = "ifany"))))
print(utils::head(as.data.frame(table(inds$BNRIND, useNA = "ifany"))))
print(utils::head(as.data.frame(table(inds$ANRIND, useNA = "ifany"))))

library(dplyr)
library(tidyr)

# treatment text lookup TRTPN -> TRTP (TRTFT format)
trtcomb <- inds %>% filter(!is.na(TRTPN)) %>% distinct(TRTPN, TRTP) %>% arrange(TRTPN)
# if duplicate TRTPN with multiple TRTP keep first
trtcomb <- trtcomb %>% group_by(TRTPN) %>% slice(1) %>% ungroup()
trt_lookup <- setNames(trimws(as.character(trtcomb$TRTP)), as.character(trtcomb$TRTPN))

# PROC SUMMARY CLASS param paramn trtpn bnrind anrind ; VAR aval ; N(aval)=n
# SAS excludes missing CLASS entirely; N counts non-missing AVAL
cnt_base <- inds %>% filter(!is.na(PARAM) & PARAM != "" &
                            !is.na(PARAMN) &
                            !is.na(TRTPN) &
                            !is.na(BNRIND) & BNRIND != "" &
                            !is.na(ANRIND) & ANRIND != "" &
                            !is.na(AVAL))
stopifnot(all(c("PARAM","PARAMN","TRTPN","BNRIND","ANRIND","AVAL") %in% names(cnt_base)))
cat("cnt_base rows:", nrow(cnt_base), "\n")

detail  <- cnt_base %>% group_by(PARAM, PARAMN, TRTPN, BNRIND, ANRIND) %>% summarise(N = dplyr::n(), .groups = "drop")
rowtot  <- cnt_base %>% group_by(PARAM, PARAMN, TRTPN, BNRIND) %>% summarise(N = dplyr::n(), .groups = "drop") %>% mutate(ANRIND = "Total")
coltot  <- cnt_base %>% group_by(PARAM, PARAMN, TRTPN, ANRIND) %>% summarise(N = dplyr::n(), .groups = "drop") %>% mutate(BNRIND = "Total")
overall <- cnt_base %>% group_by(PARAM, PARAMN, TRTPN) %>% summarise(N = dplyr::n(), .groups = "drop") %>% mutate(BNRIND = "Total", ANRIND = "Total")
counts <- bind_rows(detail, rowtot, coltot, overall)
# WHERE param is not missing and paramn is not missing and trtpn is not missing (already)
counts <- counts %>% filter(!is.na(PARAM) & PARAM != "" & !is.na(PARAMN) & !is.na(TRTPN))
counts <- counts %>% arrange(PARAM, PARAMN, TRTPN, BNRIND, ANRIND)

shiftlev <- c("Low", "Normal", "High", "Total")
map_shift <- function(x) match(x, shiftlev)

denom <- counts %>% filter(BNRIND == "Total" & ANRIND == "Total") %>%
  transmute(PARAM, PARAMN, TRTPN, DENOM = as.numeric(N))
fixtot <- counts %>% filter(!(BNRIND == "Total" & ANRIND == "Total")) %>%
  mutate(BNRINDN = map_shift(BNRIND), ANRINDN = map_shift(ANRIND)) %>%
  # keep only mappable (SAS INPUT would be missing -> dropped from full join, but keep for now; missing would not join)
  filter(!is.na(BNRINDN) & !is.na(ANRINDN)) %>%
  select(PARAM, PARAMN, TRTPN, ANRIND, BNRIND, ANRINDN, BNRINDN, N)
# NOTE: if any BNRIND/ANRIND values failed mapping, they would be missing in SAS and produce zero cells;
# log them
cat("unmapped BNRIND:", paste(sort(unique(counts$BNRIND[is.na(map_shift(counts$BNRIND))])), collapse="|"), "\n")
cat("unmapped ANRIND:", paste(sort(unique(counts$ANRIND[is.na(map_shift(counts$ANRIND))])), collapse="|"), "\n")

allcat <- fixtot %>% distinct(PARAM, PARAMN, TRTPN) %>% arrange(PARAM, PARAMN, TRTPN)
maxcat <- 4
full <- allcat %>% crossing(data.frame(BNRINDN = 1:maxcat, ANRINDN = 1:maxcat)) %>% mutate(N0 = 0)

stopifnot(all(c("PARAM","PARAMN","TRTPN","BNRINDN","ANRINDN") %in% names(fixtot)))
allrecs <- full %>% left_join(fixtot %>% select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, N),
                              by = c("PARAM","PARAMN","TRTPN","BNRINDN","ANRINDN")) %>%
  mutate(N = ifelse(is.na(N), N0, N)) %>% select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, N) %>%
  arrange(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN)

stopifnot(all(c("PARAM","PARAMN","TRTPN") %in% names(denom)))
alldenom <- allrecs %>% left_join(denom, by = c("PARAM","PARAMN","TRTPN")) %>% filter(!is.na(DENOM))
decode <- c("1" = "Low", "2" = "Normal", "3" = "High", "4" = "Total")
alldenom$BNRIND <- decode[as.character(alldenom$BNRINDN)]
alldenom$ANRIND <- decode[as.character(alldenom$ANRINDN)]
alldenom <- alldenom %>% arrange(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN)

pbord <- alldenom %>% distinct(ANRIND, ANRINDN) %>% arrange(ANRINDN)
pbcat <- pbord$ANRIND
while (length(pbcat) < 4) pbcat <- c(pbcat, NA_character_)
pbcat1 <- pbcat[1]; pbcat2 <- pbcat[2]; pbcat3 <- pbcat[3]; pbcat4 <- pbcat[4]

alldenom2 <- alldenom %>%
  mutate(PCTVAL = ifelse(DENOM > 0, 100 * as.numeric(N) / as.numeric(DENOM), 0),
         PCTR = round_half_up(PCTVAL, 1),
         PCTSTR = paste0("(", sprintf("%5.1f", PCTR), ")"))

n_wide <- alldenom2 %>% select(PARAM, PARAMN, TRTPN, BNRINDN, BNRIND, DENOM, ANRINDN, N) %>%
  pivot_wider(names_from = ANRINDN, values_from = N, names_prefix = "N")
p_wide <- alldenom2 %>% select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, PCTSTR) %>%
  pivot_wider(names_from = ANRINDN, values_from = PCTSTR, names_prefix = "PCT")
for (k in 1:4) {
  nc <- paste0("N", k); pc <- paste0("PCT", k)
  if (!nc %in% names(n_wide)) n_wide[[nc]] <- 0
  if (!pc %in% names(p_wide)) p_wide[[pc]] <- "(  0.0)"
}
trans <- n_wide %>% left_join(p_wide, by = c("PARAM","PARAMN","TRTPN","BNRINDN"))
trans$TRTP <- trt_lookup[as.character(trans$TRTPN)]
trans$TRTP[is.na(trans$TRTP)] <- ""
trans$TRTTX <- paste0(trimws(trans$TRTP), " (N = ", as.character(trans$DENOM), ")")
trans <- trans %>% transmute(PARAM, PARAMN, TRTPN, TRTTX, BNRINDN, BNRIND,
                             N1, PCT1 = PCT1, N2, PCT2 = PCT2, N3, PCT3 = PCT3, N4, PCT4 = PCT4, DENOM)
trans <- trans %>% arrange(PARAMN, PARAM, TRTPN, BNRINDN)
# replace any NA counts with 0 and NA pct with (  0.0)
for (k in 1:4) {
  nc <- paste0("N", k); pc <- paste0("PCT", k)
  trans[[nc]][is.na(trans[[nc]])] <- 0
  trans[[pc]][is.na(trans[[pc]])] <- "(  0.0)"
}
cat("trans rows:", nrow(trans), " groups:", nrow(distinct(trans, PARAMN, PARAM)), "\n")
print(trans)

# ---- text rendering mimicking PROC REPORT BY paramn param ----
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

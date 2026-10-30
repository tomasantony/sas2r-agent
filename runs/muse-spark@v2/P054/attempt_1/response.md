```r
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

propcase_one <- function(s) {
  if (is.na(s)) return(NA_character_)
  t <- trimws(as.character(s))
  if (t == "") return("")
  low <- tolower(t)
  gsub("\\b([a-z])", "\\U\\1", low, perl = TRUE)
}
propcase_vec <- function(x) {
  vapply(x, propcase_one, character(1), USE.NAMES = FALSE)
}

raw_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
alt1 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt"
alt2 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.sas7bdat"
in_path <- raw_path
if (!file.exists(in_path)) {
  if (file.exists(alt1)) in_path <- alt1 else if (file.exists(alt2)) in_path <- alt2
}
cat("Reading:", in_path, "\n")
if (grepl("\\.xpt$", in_path, ignore.case = TRUE)) {
  advs0 <- haven::read_xpt(in_path)
} else {
  advs0 <- haven::read_sas(in_path)
}
names(advs0) <- toupper(names(advs0))
stopifnot(all(c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN","AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL") %in% names(advs0)))

df <- as.data.frame(advs0, stringsAsFactors = FALSE)

charcols <- c("USUBJID","TRTP","PARAM","PARAMCD","ATPT","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
for (cc in charcols) {
  if (cc %in% names(df)) {
    v <- df[[cc]]
    if (is.factor(v)) v <- as.character(v)
    if (is.character(v)) {
      v[v == ""] <- NA_character_
      df[[cc]] <- trimws(v)
    } else {
      df[[cc]] <- as.character(v)
      df[[cc]] <- trimws(df[[cc]])
      df[[cc]][df[[cc]] == ""] <- NA_character_
    }
  }
}

# param = catx(' ', param, propcase(atpt)); paramn = atptn
prop <- propcase_vec(ifelse(is.na(df$ATPT), "", df$ATPT))
param_trim <- ifelse(is.na(df$PARAM), "", df$PARAM)
newparam <- ifelse(is.na(df$ATPT) | trimws(as.character(df$ATPT)) == "",
                   trimws(as.character(df$PARAM)),
                   paste(param_trim, prop))
newparam[newparam == ""] <- NA_character_
df$PARAM <- newparam
df$PARAMN <- df$ATPTN

# keep & filter: WHERE PARAMCD='DIABP' and ANL01FL='Y' and AVISITN=99 and SAFFL='Y' and BNRIND ne 'Missing' and ANRIND ne 'Missing'
# SAS missing '' passes ne 'Missing', so keep NA as pass
stopifnot(all(c("PARAMCD","ANL01FL","AVISITN","SAFFL","BNRIND","ANRIND") %in% names(df)))
inds <- df[
  !is.na(df$PARAMCD) & df$PARAMCD == "DIABP" &
  !is.na(df$ANL01FL) & df$ANL01FL == "Y" &
  !is.na(df$AVISITN) & df$AVISITN == 99 &
  !is.na(df$SAFFL) & df$SAFFL == "Y" &
  (is.na(df$BNRIND) | df$BNRIND != "Missing") &
  (is.na(df$ANRIND) | df$ANRIND != "Missing"),
, drop = FALSE]

# treatment lookup TRTPN -> TRTP (TRTFT format)
trtcomb <- unique(inds[, c("TRTPN","TRTP"), drop = FALSE])
trtcomb <- trtcomb[!is.na(trtcomb$TRTPN), , drop = FALSE]
trt_lookup <- setNames(trimws(as.character(trtcomb$TRTP)), as.character(trtcomb$TRTPN))

# PROC SUMMARY: CLASS param paramn trtpn bnrind anrind ; VAR aval ; N(aval)
# SAS excludes missing CLASS by default; N counts non-missing AVAL
cnt_base <- inds[!is.na(inds$PARAM) & inds$PARAM != "" &
                 !is.na(inds$PARAMN) &
                 !is.na(inds$TRTPN) &
                 !is.na(inds$BNRIND) & inds$BNRIND != "" &
                 !is.na(inds$ANRIND) & inds$ANRIND != "" &
                 !is.na(inds$AVAL), , drop = FALSE]
stopifnot(all(c("PARAM","PARAMN","TRTPN","BNRIND","ANRIND","AVAL") %in% names(cnt_base)))

library(dplyr)
library(tidyr)

detail <- cnt_base %>% group_by(PARAM, PARAMN, TRTPN, BNRIND, ANRIND) %>% summarise(N = dplyr::n(), .groups = "drop")
rowtot <- cnt_base %>% group_by(PARAM, PARAMN, TRTPN, BNRIND) %>% summarise(N = dplyr::n(), .groups = "drop") %>% mutate(ANRIND = "Total")
coltot <- cnt_base %>% group_by(PARAM, PARAMN, TRTPN, ANRIND) %>% summarise(N = dplyr::n(), .groups = "drop") %>% mutate(BNRIND = "Total")
overall <- cnt_base %>% group_by(PARAM, PARAMN, TRTPN) %>% summarise(N = dplyr::n(), .groups = "drop") %>% mutate(BNRIND = "Total", ANRIND = "Total")
counts <- bind_rows(detail, rowtot, coltot, overall)
counts <- counts %>% arrange(PARAM, PARAMN, TRTPN)

shiftlev <- c("Low","Normal","High","Total")
# fixtot / denom
denom <- counts %>% filter(BNRIND == "Total" & ANRIND == "Total") %>% transmute(PARAM, PARAMN, TRTPN, DENOM = N)
fixtot <- counts %>% filter(!(BNRIND == "Total" & ANRIND == "Total")) %>%
  mutate(BNRINDN = match(BNRIND, shiftlev), ANRINDN = match(ANRIND, shiftlev)) %>%
  select(PARAM, PARAMN, TRTPN, ANRIND, BNRIND, ANRINDN, BNRINDN, N)

allcat <- fixtot %>% distinct(PARAM, PARAMN, TRTPN)

# full expansion 1..4 x 1..4
maxcat <- 4
full <- allcat %>% tidyr::crossing(data.frame(BNRINDN = 1:maxcat, ANRINDN = 1:maxcat)) %>% mutate(N0 = 0)

# allrecs: merge full(in=inf) fixtot ; if inf
stopifnot(all(c("PARAM","PARAMN","TRTPN","BNRINDN","ANRINDN") %in% names(fixtot)))
tmp_join <- full %>% left_join(fixtot %>% select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, N),
                               by = c("PARAM","PARAMN","TRTPN","BNRINDN","ANRINDN"))
# avoid .x/.y : full has N0, fixtot has N
allrecs <- tmp_join %>% mutate(N = ifelse(is.na(N), N0, N)) %>% select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, N)

# alldenom: merge allrecs denom by param paramn trtpn ; where denom not missing ; repopulate
stopifnot(all(c("PARAM","PARAMN","TRTPN") %in% names(denom)))
alldenom <- allrecs %>% left_join(denom, by = c("PARAM","PARAMN","TRTPN")) %>% filter(!is.na(DENOM))
decode <- c("1" = "Low", "2" = "Normal", "3" = "High", "4" = "Total")
alldenom$BNRIND <- decode[as.character(alldenom$BNRINDN)]
alldenom$ANRIND <- decode[as.character(alldenom$ANRINDN)]

# pbcat1-4 ordered by anrindn
pbord <- alldenom %>% distinct(ANRIND, ANRINDN) %>% arrange(ANRINDN)
pbcat <- pbord$ANRIND
while (length(pbcat) < 4) pbcat <- c(pbcat, NA)
pbcat1 <- pbcat[1]; pbcat2 <- pbcat[2]; pbcat3 <- pbcat[3]; pbcat4 <- pbcat[4]

# transpose to 1 record per baseline + pct
alldenom2 <- alldenom %>%
  mutate(PCTVAL = ifelse(DENOM > 0, 100 * N / DENOM, 0),
         PCTR = round_half_up(PCTVAL, 1),
         PCTSTR = paste0("(", sprintf("%5.1f", PCTR), ")"))
# SAS else '(  0.0)' matches our formula when DENOM==0 (PCTR=0 -> "(  0.0)")

n_wide <- alldenom2 %>% select(PARAM, PARAMN, TRTPN, BNRINDN, BNRIND, DENOM, ANRINDN, N) %>%
  pivot_wider(names_from = ANRINDN, values_from = N, names_prefix = "N")
p_wide <- alldenom2 %>% select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, PCTSTR) %>%
  pivot_wider(names_from = ANRINDN, values_from = PCTSTR, names_prefix = "PCT")
# ensure N1-N4 / PCT1-PCT4 exist
for (k in 1:4) {
  nc <- paste0("N", k); pc <- paste0("PCT", k)
  if (!nc %in% names(n_wide)) n_wide[[nc]] <- 0
  if (!pc %in% names(p_wide)) p_wide[[pc]] <- "(  0.0)"
}
trans <- n_wide %>% left_join(p_wide, by = c("PARAM","PARAMN","TRTPN","BNRINDN"))
trans$TRTP <- trt_lookup[as.character(trans$TRTPN)]
trans$TRTP[is.na(trans$TRTP)] <- ""
trans$TRTTX <- paste0(trimws(trans$TRTP), " (N = ", as.character(trans$DENOM), ")")
# order columns like SAS trans keep
trans <- trans %>% transmute(PARAM, PARAMN, TRTPN, TRTTX, BNRINDN, BNRIND,
                             N1, PCT1 = PCT1, N2, PCT2 = PCT2, N3, PCT3 = PCT3, N4, PCT4 = PCT4, DENOM)
# SAS names pct1-pct4 lower; keep uppercase then rename to match? use N1 PCT1 etc.
names(trans) <- c("PARAM","PARAMN","TRTPN","TRTTX","BNRINDN","BNRIND","N1","PCT1","N2","PCT2","N3","PCT3","N4","PCT4","DENOM")
trans <- trans %>% arrange(PARAMN, PARAM, TRTPN, BNRINDN)

# ---- text rendering (PROC REPORT with BY paramn param) ----
# Build lines
hdr_post <- paste0("Post-Baseline Result")
con <- textConnection("tmpout", "w", local = TRUE)

bykeys <- trans %>% distinct(PARAMN, PARAM) %>% arrange(PARAMN, PARAM)
# column header template
colhdr1 <- sprintf("%-30s %-10s %6s %-7s %6s %-7s %6s %-7s %6s %-7s", "Treatment", "Baseline", paste0(pbcat1), "", paste0(pbcat2), "", paste0(pbcat3), "", paste0(pbcat4), "")
colhdr2 <- sprintf("%-30s %-10s %6s %-7s %6s %-7s %6s %-7s %6s %-7s", "Result", "Result", "n", "%", "n", "%", "n", "%", "n", "%")

out_lines <- c()
out_lines <- c(out_lines, "Shift Table of Diastolic Blood Pressure (DIABP) by Baseline and Post-Baseline Category",
               "Analysis Population: Safety (SAFFL=Y), ANL01FL=Y, AVISITN=99",
               "")
for (i in seq_len(nrow(bykeys))) {
  pn <- bykeys$PARAMN[i]; pm <- bykeys$PARAM[i]
  out_lines <- c(out_lines, paste0("------------------------------------------------------------"),
                 paste0("PARAMN=", pn, " PARAM=", pm),
                 paste0("------------------------------------------------------------"),
                 hdr_post,
                 colhdr1,
                 colhdr2,
                 paste0(strrep("-", 30), " ", strrep("-", 10), " ", strrep("-", 6), " ", strrep("-", 7), " ", strrep("-", 6), " ", strrep("-", 7), " ", strrep("-", 6), " ", strrep("-", 7), " ", strrep("-", 6), " ", strrep("-", 7)))
  sub <- trans %>% filter(PARAMN == pn & PARAM == pm) %>% arrange(TRTPN, BNRINDN)
  trts <- unique(sub$TRTPN)
  for (tt in trts) {
    s2 <- sub %>% filter(TRTPN == tt) %>% arrange(BNRINDN)
    tx <- s2$TRTTX[1]
    for (r in seq_len(nrow(s2))) {
      show_tx <- if (r == 1) sprintf("%-30s", substr(tx, 1, 30)) else sprintf("%-30s", "")
      line <- sprintf("%s %-10s %6d %-7s %6d %-7s %6d %-7s %6d %-7s",
                      show_tx, sprintf("%-10s", s2$BNRIND[r]),
                      s2$N1[r], s2$PCT1[r], s2$N2[r], s2$PCT2[r], s2$N3[r], s2$PCT3[r], s2$N4[r], s2$PCT4[r])
      out_lines <- c(out_lines, line)
    }
    out_lines <- c(out_lines, "")
  }
  out_lines <- c(out_lines, "")
}
writeLines(out_lines, file.path(out, "P054.txt"))
# duplicate as .lst for SAS counterpart
writeLines(out_lines, file.path(out, "P054.lst"))

# CSV of table body (trans)
write.csv(trans, file.path(out, "P054.csv"), row.names = FALSE, na = "")
cat("Done. Rows:", nrow(trans), "BY groups:", nrow(bykeys), "\n")
```
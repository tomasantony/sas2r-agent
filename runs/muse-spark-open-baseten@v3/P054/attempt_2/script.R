library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
library(tibble)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(a,b) if (is.null(a) || length(a)==0 || all(is.na(a))) b else a

round_half_up <- function(x, inc) {
  if (is.na(inc) || inc==0) return(x)
  s <- sign(x)
  res <- s * floor(abs(x)/inc + 0.5 + 1e-12) * inc
  res[res==0] <- 0
  res
}

fmt_5_1 <- function(x) {
  sprintf("%5.1f", x)
}

prop_case <- function(x) {
  ifelse(is.na(x), NA_character_, tools::toTitleCase(tolower(x)))
}

find_file <- function(candidates) {
  for (f in candidates) {
    if (file.exists(f)) return(f)
  }
  return(NA_character_)
}

candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/ADVS.xpt",
  file.path(Sys.getenv("rawadam"), "advs.xpt"),
  file.path(Sys.getenv("rawadam"), "ADVS.xpt"),
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/raw/cdisc-pilot/adam/ADVS.xpt",
  "data/raw/cdisc-pilot/adam/advs.xpt",
  "./data/raw/cdisc-pilot/adam/advs.xpt",
  file.path(getwd(), "advs.xpt")
)
adv_path <- find_file(candidates)
if (is.na(adv_path)) {
  roots <- c(Sys.getenv("root"), "D:/meta_hackathon/sas2r-agent", "/bench", ".", getwd())
  for (r in roots) {
    if (!is.na(r) && r!="" && dir.exists(r)) {
      ff <- list.files(r, pattern="^advs\\.xpt$", recursive=TRUE, ignore.case=TRUE, full.names=TRUE)
      if (length(ff)>0) { adv_path <- ff[1]; break }
    }
  }
}
if (is.na(adv_path) || !file.exists(adv_path)) {
  stop(paste("advs.xpt not found. Tried:", paste(candidates, collapse="; ")))
}
message("Reading ", adv_path)

advs_raw <- haven::read_xpt(adv_path)
names(advs_raw) <- toupper(names(advs_raw))

advs <- advs_raw %>%
  mutate(
    PARAM = trimws(as.character(PARAM)),
    ATPT = trimws(as.character(ATPT)),
    PARAM = ifelse(is.na(ATPT) | ATPT=="", PARAM,
                   trimws(paste(PARAM, prop_case(ATPT)))),
    PARAMN = ATPTN
  )

needed <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN","AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
for (nm in needed) if (!nm %in% names(advs)) advs[[nm]] <- NA

inds <- advs %>%
  filter(
    PARAMCD == "DIABP",
    ANL01FL == "Y",
    AVISITN == 99,
    SAFFL == "Y",
    !is.na(BNRIND), BNRIND != "Missing",
    !is.na(ANRIND), ANRIND != "Missing"
  ) %>%
  select(all_of(needed))

trtcomb <- inds %>%
  distinct(TRTPN, TRTP) %>%
  filter(!is.na(TRTPN)) %>%
  arrange(TRTPN)

trt_fmt <- setNames(trimws(as.character(trtcomb$TRTP)), as.character(trtcomb$TRTPN))
trt_fmt_fun <- function(x) {
  s <- as.character(x)
  out <- trt_fmt[s]
  out[is.na(out)] <- s[is.na(out)]
  out
}

cnt_detail <- inds %>%
  group_by(PARAM, PARAMN, TRTPN, BNRIND, ANRIND) %>%
  summarise(n = sum(!is.na(AVAL)), .groups="drop")

cnt_bnr <- inds %>%
  group_by(PARAM, PARAMN, TRTPN, BNRIND) %>%
  summarise(n = sum(!is.na(AVAL)), .groups="drop") %>%
  mutate(ANRIND = NA_character_)

cnt_anr <- inds %>%
  group_by(PARAM, PARAMN, TRTPN, ANRIND) %>%
  summarise(n = sum(!is.na(AVAL)), .groups="drop") %>%
  mutate(BNRIND = NA_character_)

cnt_tot <- inds %>%
  group_by(PARAM, PARAMN, TRTPN) %>%
  summarise(n = sum(!is.na(AVAL)), .groups="drop") %>%
  mutate(BNRIND = NA_character_, ANRIND = NA_character_)

counts <- bind_rows(cnt_detail, cnt_bnr, cnt_anr, cnt_tot) %>%
  filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN))

counts <- counts %>% arrange(PARAM, PARAMN, TRTPN)

shiftord_map <- c("Low"=1, "Normal"=2, "High"=3, "Total"=4)
shifttx_map <- c("1"="Low","2"="Normal","3"="High","4"="Total")

counts2 <- counts %>%
  mutate(
    BNRIND = ifelse(is.na(BNRIND), "Total", BNRIND),
    ANRIND = ifelse(is.na(ANRIND), "Total", ANRIND),
    BNRINDN = as.numeric(shiftord_map[BNRIND]),
    ANRINDN = as.numeric(shiftord_map[ANRIND])
  )

fixtot <- counts2 %>%
  filter(!(BNRIND=="Total" & ANRIND=="Total")) %>%
  select(PARAM, PARAMN, TRTPN, ANRIND, BNRIND, ANRINDN, BNRINDN, n)

denom <- counts2 %>%
  filter(BNRIND=="Total" & ANRIND=="Total") %>%
  select(PARAM, PARAMN, TRTPN, n) %>%
  rename(denom = n)

fixtot <- fixtot %>% arrange(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN)

maxcat <- 4
allcat <- fixtot %>% distinct(PARAM, PARAMN, TRTPN)

full <- allcat %>%
  crossing(tibble(BNRINDN = 1:maxcat, ANRINDN = 1:maxcat)) %>%
  mutate(n = 0)

allrecs <- full %>%
  left_join(fixtot, by=c("PARAM","PARAMN","TRTPN","BNRINDN","ANRINDN"), suffix=c("", ".y")) %>%
  mutate(n = ifelse(is.na(n.y), n, n.y)) %>%
  select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, n)

alldenom <- allrecs %>%
  left_join(denom, by=c("PARAM","PARAMN","TRTPN")) %>%
  filter(!is.na(denom)) %>%
  mutate(
    BNRIND = shifttx_map[as.character(BNRINDN)],
    ANRIND = shifttx_map[as.character(ANRINDN)]
  )

pbcat <- alldenom %>%
  distinct(ANRIND, ANRINDN) %>%
  arrange(ANRINDN) %>%
  pull(ANRIND)
while(length(pbcat) < maxcat) pbcat <- c(pbcat, paste0("Cat", length(pbcat)+1))
pbcat1 <- pbcat[1]; pbcat2 <- pbcat[2]; pbcat3 <- pbcat[3]; pbcat4 <- pbcat[4]

alldenom2 <- alldenom %>%
  mutate(
    pct_val = ifelse(denom>0, 100 * n / denom, 0),
    pct_rounded = round_half_up(pct_val, 0.1),
    pct_str = paste0("(", fmt_5_1(pct_rounded), ")")
  ) %>%
  mutate(pct_str = ifelse(denom>0, pct_str, "(  0.0)"))

wide_n <- alldenom2 %>%
  select(PARAM, PARAMN, TRTPN, BNRINDN, BNRIND, ANRINDN, n, denom) %>%
  pivot_wider(names_from=ANRINDN, values_from=n, names_prefix="n") %>%
  arrange(PARAM, PARAMN, TRTPN, BNRINDN)

wide_pct <- alldenom2 %>%
  select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, pct_str) %>%
  pivot_wider(names_from=ANRINDN, values_from=pct_str, names_prefix="pct")

trans <- wide_n %>%
  left_join(wide_pct, by=c("PARAM","PARAMN","TRTPN","BNRINDN")) %>%
  mutate(
    TRTTX = paste0(trimws(trt_fmt_fun(TRTPN)), " (N = ", trimws(format(denom, width=12)), ")"),
    TRTTX = gsub("\\s+", " ", TRTTX),
    TRTTX = trimws(TRTTX)
  ) %>%
  select(PARAM, PARAMN, TRTPN, TRTTX, BNRINDN, BNRIND, n1, n2, n3, n4, pct1, pct2, pct3, pct4, denom) %>%
  arrange(PARAM, PARAMN, TRTPN, BNRINDN)

for (i in 1:maxcat) {
  nc <- paste0("n", i); pc <- paste0("pct", i)
  if (!nc %in% names(trans)) trans[[nc]] <- 0
  if (!pc %in% names(trans)) trans[[pc]] <- "(  0.0)"
}
trans <- trans %>% arrange(PARAMN, PARAM, TRTPN, BNRINDN)

report_data <- trans

hdr1 <- paste0("Shift Table for ", paste(unique(report_data$PARAM), collapse=", "))
txt_lines <- c()
txt_lines <- c(txt_lines, hdr1)
txt_lines <- c(txt_lines, "Diastolic Blood Pressure Shift (PARAMCD=DIABP, AVISITN=99)")
txt_lines <- c(txt_lines, "")

by_groups <- report_data %>% distinct(PARAMN, PARAM) %>% arrange(PARAMN)

for (bg in seq_len(nrow(by_groups))) {
  p_n <- by_groups$PARAMN[bg]
  p_tx <- by_groups$PARAM[bg]
  sub <- report_data %>% filter(PARAMN==p_n, PARAM==p_tx) %>% arrange(TRTPN, BNRINDN)
  txt_lines <- c(txt_lines, paste0("PARAMN=", p_n, " PARAM=", p_tx))
  txt_lines <- c(txt_lines, paste0(strrep("-", 120)))
  col_hdr1 <- sprintf("%-30s %-12s | %-14s %-14s %-14s %-14s",
                      "Treatment", "Baseline", "Post-Baseline Result", "", "", "")
  col_hdr2 <- sprintf("%-30s %-12s | %-6s %-7s %-6s %-7s %-6s %-7s %-6s %-7s",
                      "", "Result", pbcat1, "", pbcat2, "", pbcat3, "", pbcat4, "")
  col_hdr3 <- sprintf("%-30s %-12s | %-6s %-7s %-6s %-7s %-6s %-7s %-6s %-7s",
                      "", "", "n", "%", "n", "%", "n", "%", "n", "%")
  txt_lines <- c(txt_lines, col_hdr1)
  txt_lines <- c(txt_lines, col_hdr2)
  txt_lines <- c(txt_lines, col_hdr3)
  txt_lines <- c(txt_lines, paste0(strrep("-", 120)))
  trt_levels <- sub %>% distinct(TRTPN, TRTTX) %>% arrange(TRTPN)
  for (ti in seq_len(nrow(trt_levels))) {
    tr <- trt_levels$TRTPN[ti]
    trtx <- trt_levels$TRTTX[ti]
    rows <- sub %>% filter(TRTPN==tr) %>% arrange(BNRINDN)
    for (r in seq_len(nrow(rows))) {
      rr <- rows[r,]
      line <- sprintf("%-30s %-12s | %6d %-7s %6d %-7s %6d %-7s %6d %-7s",
                      ifelse(r==1, substr(trtx,1,30), ""),
                      rr$BNRIND,
                      rr$n1, rr$pct1,
                      rr$n2, rr$pct2,
                      rr$n3, rr$pct3,
                      rr$n4, rr$pct4)
      txt_lines <- c(txt_lines, line)
    }
    txt_lines <- c(txt_lines, "")
  }
  txt_lines <- c(txt_lines, "")
}

txt_path1 <- file.path(out, "P054.txt")
txt_path2 <- file.path(out, "P054.lst.txt")
writeLines(txt_lines, con=txt_path1)
writeLines(txt_lines, con=txt_path2)
lst_path <- file.path(out, "P054.lst")
writeLines(txt_lines, con=lst_path)

csv_out <- trans %>%
  select(PARAM, PARAMN, TRTPN, TRTTX, BNRIND, BNRINDN,
                n1, pct1, n2, pct2, n3, pct3, n4, pct4, denom) %>%
  arrange(PARAMN, PARAM, TRTPN, BNRINDN)

names(csv_out) <- toupper(names(csv_out))

write.csv(csv_out, file.path(out, "P054.csv"), row.names=FALSE, na="")
write.csv(csv_out, file.path(out, "P054.lst.csv"), row.names=FALSE, na="")
write.csv(csv_out, file.path(out, "P054_dataset.csv"), row.names=FALSE, na="")

message("Done. Wrote ", txt_path1, " and CSV to ", out)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)
suppressMessages(library(haven))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(stringr))
suppressMessages(library(readr))
suppressMessages(library(tibble))
try(Sys.setlocale("LC_COLLATE","C"), silent=TRUE)

rhu_01 <- function(x) {
  # SAS ROUND(x,0.1) half away from zero
  ifelse(is.na(x), NA_real_, sign(x)*floor(abs(x)*10+0.5+1e-9)/10)
}
prop_case <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    v <- trimws(as.character(v))
    if (v=="") return("")
    low <- tolower(v)
    gsub("\\b([a-z])","\\U\\1",low,perl=TRUE)
  }, USE.NAMES=FALSE)
}
catx_sp <- function(a,b) {
  # CATX(' ',a,b)
  mapply(function(x,y){
    xn <- is.na(x) || trimws(as.character(x))==""
    yn <- is.na(y) || trimws(as.character(y))==""
    if (xn && yn) return("")
    if (xn) return(trimws(as.character(y)))
    if (yn) return(trimws(as.character(x)))
    paste(trimws(as.character(x)), trimws(as.character(y)), sep=" ")
  }, a, b, USE.NAMES=FALSE)
}
fmt_pct <- function(n, denom) {
  # returns "(  0.0)" style, vectorized
  sapply(seq_along(n), function(i){
    ni <- n[i]; d <- denom[i]
    if (is.na(d) || d<=0) return("(  0.0)")
    if (is.na(ni)) ni <- 0
    raw <- 100*ni/d
    tenths <- floor(raw*10+0.5+1e-9)
    ip <- tenths %/% 10
    dec <- tenths %% 10
    num <- paste0(ip,".",dec)
    pad <- sprintf("%5s", num)
    paste0("(",pad,")")
  }, USE.NAMES=FALSE)
}
is_miss_c <- function(x) is.na(x) | trimws(as.character(x))==""
norm_c <- function(x, trim=TRUE) {
  sapply(x, function(v){
    if (is.na(v)) return(NA_character_)
    t <- trimws(as.character(v))
    if (t=="") return(NA_character_)
    t
  }, USE.NAMES=FALSE)
}

find_first_existing <- function(paths) {
  for (p in paths) if (!is.na(p) && p!="" && file.exists(p)) return(p)
  return(NA_character_)
}
cand <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt",
  file.path(Sys.getenv("RAWADAM"), "advs.xpt"),
  file.path(Sys.getenv("rawadam"), "advs.xpt"),
  "data/raw/cdisc-pilot/adam/advs.xpt",
  "./advs.xpt"
)
advspath <- find_first_existing(cand)
if (is.na(advspath)) {
  # recursive search from cwd and from OUT_DIR parent
  roots <- unique(c(getwd(), dirname(out), ".", "/tmp", "D:/meta_hackathon/sas2r-agent"))
  found <- NA_character_
  for (rt in roots) {
    if (!dir.exists(rt)) next
    f <- list.files(rt, pattern="^advs\\.xpt$", recursive=TRUE, ignore.case=TRUE, full.names=TRUE)
    if (length(f)>0) { found <- f[1]; break }
  }
  if (!is.na(found)) advspath <- found
}
if (is.na(advspath) || !file.exists(advspath)) stop(paste0("advs.xpt not found; tried: ", paste(cand, collapse="; ")))
message("Reading ", advspath)
advs0 <- haven::read_xpt(advspath)
advs0 <- haven::zap_formats(haven::zap_labels(advs0))
names(advs0) <- toupper(names(advs0))
# ensure needed cols exist
need <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN","AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
for (nn in need) if (!nn %in% names(advs0)) advs0[[nn]] <- NA
# work.advs : param = catx(' ',param,propcase(atpt)); paramn = atptn
ATPT_c <- as.character(advs0$ATPT)
PARAM_c <- as.character(advs0$PARAM)
PC <- prop_case(ATPT_c)
PARAM_new <- catx_sp(PARAM_c, PC)
# handle case where ATPT missing -> catx returns PARAM trimmed; where PARAM missing -> returns PC
advs0$PARAM <- PARAM_new
advs0$PARAMN <- suppressWarnings(as.numeric(advs0$ATPTN))
# keep invar
invar <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN","AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
inds_all <- advs0[, invar]
# normalize types
inds_all$TRTPN <- suppressWarnings(as.numeric(inds_all$TRTPN))
inds_all$PARAMN <- suppressWarnings(as.numeric(inds_all$PARAMN))
inds_all$AVAL <- suppressWarnings(as.numeric(inds_all$AVAL))
inds_all$AVISITN <- suppressWarnings(as.numeric(inds_all$AVISITN))
for (cc in c("USUBJID","TRTP","PARAM","PARAMCD","ATPT","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")) {
  inds_all[[cc]] <- as.character(inds_all[[cc]])
}
# WHERE PARAMCD='DIABP' and ANL01FL='Y' and AVISITN=99 and SAFFL='Y' and BNRIND ne 'Missing' and ANRIND ne 'Missing'
# SAS ne: missing(blank) passes; R: keep NA as pass for ne 'Missing'
inds <- inds_all %>%
  filter(!is.na(PARAMCD) & PARAMCD=="DIABP",
         !is.na(ANL01FL) & ANL01FL=="Y",
         !is.na(AVISITN) & AVISITN==99,
         !is.na(SAFFL) & SAFFL=="Y",
         (is.na(BNRIND) | BNRIND!="Missing"),
         (is.na(ANRIND) | ANRIND!="Missing"))
stopifnot(nrow(inds)>0)
# normalize CLASS vars: blank -> NA, trim
inds$PARAM <- norm_c(inds$PARAM)
inds$BNRIND <- norm_c(inds$BNRIND)
inds$ANRIND <- norm_c(inds$ANRIND)
inds$TRTP <- trimws(as.character(inds$TRTP))
# treatment map TRTPN -> TRTP (TRTFT format)
trtcomb <- inds %>% filter(!is.na(TRTPN)) %>% distinct(TRTPN, TRTP) %>% arrange(TRTPN)
# for display, if duplicate TRTPN with different TRTP take first
trtcomb <- trtcomb %>% group_by(TRTPN) %>% slice(1) %>% ungroup()
trtmap <- setNames(trtcomb$TRTP, as.character(trtcomb$TRTPN))
# PROC SUMMARY: exclude obs with missing CLASS entirely (MISSING not specified)
for_counts <- inds %>% filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN), !is.na(BNRIND), !is.na(ANRIND))
# detail + marginals (N of non-missing AVAL)
nonmiss <- for_counts %>% filter(!is.na(AVAL))
detail <- nonmiss %>% group_by(PARAM, PARAMN, TRTPN, BNRIND, ANRIND) %>% summarise(n=n(), .groups="drop")
marg_b <- nonmiss %>% group_by(PARAM, PARAMN, TRTPN, BNRIND) %>% summarise(n=n(), .groups="drop") %>% mutate(ANRIND=NA_character_)
marg_a <- nonmiss %>% group_by(PARAM, PARAMN, TRTPN, ANRIND) %>% summarise(n=n(), .groups="drop") %>% mutate(BNRIND=NA_character_)
tot <- nonmiss %>% group_by(PARAM, PARAMN, TRTPN) %>% summarise(n=n(), .groups="drop") %>% mutate(BNRIND=NA_character_, ANRIND=NA_character_)
counts <- bind_rows(detail, marg_b, marg_a, tot)
# WHERE param is not missing and paramn is not missing and trtpn is not missing (already)
counts <- counts %>% filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN))
# sort by param paramn trtpn (SAS order, C locale)
counts <- counts %>% arrange(PARAM, PARAMN, TRTPN)
# fixtot / denom
shiftord <- c("Low"=1,"Normal"=2,"High"=3,"Total"=4)
counts2 <- counts %>%
  mutate(BNRIND2 = ifelse(is.na(BNRIND), "Total", BNRIND),
         ANRIND2 = ifelse(is.na(ANRIND), "Total", ANRIND),
         bnrindn = unname(shiftord[BNRIND2]),
         anrindn = unname(shiftord[ANRIND2]))
denom <- counts2 %>% filter(BNRIND2=="Total" & ANRIND2=="Total") %>%
  transmute(PARAM, PARAMN, TRTPN, denom=n)
fixtot <- counts2 %>% filter(!(BNRIND2=="Total" & ANRIND2=="Total")) %>%
  transmute(PARAM, PARAMN, TRTPN, BNRIND=BNRIND2, ANRIND=ANRIND2, bnrindn, anrindn, n)
fixtot <- fixtot %>% arrange(PARAM, PARAMN, TRTPN, bnrindn, anrindn)
# allcat
allcat <- fixtot %>% distinct(PARAM, PARAMN, TRTPN)
# full grid 1..4 x 1..4
maxcat <- 4
full <- allcat %>% tidyr::crossing(bnrindn=1:maxcat, anrindn=1:maxcat) %>% mutate(n0=0)
# allrecs: merge full with fixtot
fixtot_s <- fixtot %>% select(PARAM, PARAMN, TRTPN, bnrindn, anrindn, n)
allrecs <- full %>% left_join(fixtot_s, by=c("PARAM","PARAMN","TRTPN","bnrindn","anrindn"))
allrecs$n <- ifelse(is.na(allrecs$n.y), allrecs$n0, allrecs$n.y)
allrecs$n0 <- NULL; allrecs$n.y <- NULL
# some left_join creates n.x? handle variants
if ("n.x" %in% names(allrecs)) { allrecs$n <- allrecs$`n.x`; allrecs$`n.x` <- NULL }
# alldenom
alldenom <- allrecs %>% left_join(denom, by=c("PARAM","PARAMN","TRTPN")) %>% filter(!is.na(denom))
shifttx <- c("1"="Low","2"="Normal","3"="High","4"="Total")
alldenom$BNRIND <- unname(shifttx[as.character(alldenom$bnrindn)])
alldenom$ANRIND <- unname(shifttx[as.character(alldenom$anrindn)])
# pbcat ordered by anrindn
pbcats <- alldenom %>% distinct(ANRIND, anrindn) %>% arrange(anrindn)
pbcat <- pbcats$ANRIND
# ensure length 4
while (length(pbcat)<4) pbcat <- c(pbcat, NA)
pbcat1 <- pbcat[1]; pbcat2 <- pbcat[2]; pbcat3 <- pbcat[3]; pbcat4 <- pbcat[4]
# compute pct strings
alldenom$pctstr <- fmt_pct(alldenom$n, alldenom$denom)
# transpose to 1 record per baseline
# pivot n and pctstr wider
wide_n <- alldenom %>% select(PARAM, PARAMN, TRTPN, bnrindn, BNRIND, anrindn, n, denom) %>%
  tidyr::pivot_wider(names_from=anrindn, values_from=n, names_prefix="n", values_fill=list(n=0))
wide_p <- alldenom %>% select(PARAM, PARAMN, TRTPN, bnrindn, anrindn, pctstr) %>%
  tidyr::pivot_wider(names_from=anrindn, values_from=pctstr, names_prefix="pct", values_fill=list(pctstr="(  0.0)"))
trans <- wide_n %>% left_join(wide_p, by=c("PARAM","PARAMN","TRTPN","bnrindn"))
# ensure n1-n4 pct1-pct4 exist
for (k in 1:4) {
  nc <- paste0("n",k); pc <- paste0("pct",k)
  if (!nc %in% names(trans)) trans[[nc]] <- 0
  if (!pc %in% names(trans)) trans[[pc]] <- "(  0.0)"
}
# trttx
trans <- trans %>% rowwise() %>%
  mutate(TRTP_lab = ifelse(as.character(TRTPN) %in% names(trtmap), trtmap[[as.character(TRTPN)]], ""),
         trttx = paste(trimws(TRTP_lab), paste0("(N = ", denom, ")"), sep=" ")) %>%
  ungroup() %>% select(-TRTP_lab)
# rename to SAS-like lower names for output dataset
trans_out <- trans %>% transmute(param=PARAM, paramn=PARAMN, trtpn=TRTPN, bnrindn=bnrindn, bnrind=BNRIND,
  n1=n1, pct1=pct1, n2=n2, pct2=pct2, n3=n3, pct3=pct3, n4=n4, pct4=pct4, denom=denom, trttx=trttx) %>%
  arrange(paramn, param, trtpn, bnrindn)
# write dataset CSV
write.csv(trans_out, file.path(out, "trans.csv"), row.names=FALSE, na="")
write.csv(trans_out, file.path(out, "P038_dataset.csv"), row.names=FALSE, na="")
# table body CSV (same as trans for P038)
table_csv <- trans_out
write.csv(table_csv, file.path(out, "P038.csv"), row.names=FALSE, na="")
# Build text rendering mimicking PROC REPORT with BY paramn param
# sort BY paramn param
trans_txt <- trans_out %>% arrange(paramn, param, trtpn, bnrindn)
by_groups <- trans_txt %>% distinct(paramn, param) %>% arrange(paramn, param)
lines <- c()
lines <- c(lines, "Shift Table of Diastolic Blood Pressure - Baseline vs Post-Baseline (ANL01FL=Y, AVISITN=99, SAFFL=Y)")
lines <- c(lines, "")
for (gi in seq_len(nrow(by_groups))) {
  gpn <- by_groups$paramn[gi]; gp <- by_groups$param[gi]
  lines <- c(lines, paste0("--------------- paramn=", gpn, " param=", gp, " ---------------"))
  lines <- c(lines, "")
  # header
  lines <- c(lines, paste0("Treatment | Baseline Result | Post-Baseline Result"))
  hdr2 <- paste0("          |                 | ", pbcat1, " | ", pbcat2, " | ", pbcat3, " | ", pbcat4)
  lines <- c(lines, hdr2)
  lines <- c(lines, paste0("          |                 | n % | n % | n % | n %"))
  sub <- trans_txt %>% filter(paramn==gpn, param==gp) %>% arrange(trtpn, bnrindn)
  trts <- unique(sub$trtpn)
  for (tt in trts) {
    rows <- sub %>% filter(trtpn==tt) %>% arrange(bnrindn)
    tt_lab <- rows$trttx[1]
    for (r in seq_len(nrow(rows))) {
      treat_show <- if (r==1) tt_lab else ""
      # format n as integer strings
      line <- paste0(treat_show, " | ", rows$bnrind[r],
        " | ", rows$n1[r], " ", rows$pct1[r],
        " | ", rows$n2[r], " ", rows$pct2[r],
        " | ", rows$n3[r], " ", rows$pct3[r],
        " | ", rows$n4[r], " ", rows$pct4[r])
      lines <- c(lines, line)
    }
    lines <- c(lines, "")
  }
  lines <- c(lines, "")
}
writeLines(lines, file.path(out, "P038.txt"))
writeLines(lines, file.path(out, "P038.lst"))
message("Done")

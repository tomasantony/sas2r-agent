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
norm_c <- function(x) {
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
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/derived/cdisc-pilot/adam/advs.xpt",
  file.path(Sys.getenv("OUT_DIR"), "advs.xpt"),
  "./advs.xpt"
)
advspath <- find_first_existing(cand)
if (is.na(advspath)) {
  roots <- unique(c(getwd(), dirname(out), ".", "/tmp", "/bench"))
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
need <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN","AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
for (nn in need) if (!nn %in% names(advs0)) advs0[[nn]] <- NA
ATPT_c <- as.character(advs0$ATPT)
PARAM_c <- as.character(advs0$PARAM)
PC <- prop_case(ATPT_c)
PARAM_new <- catx_sp(PARAM_c, PC)
advs0$PARAM <- PARAM_new
advs0$PARAMN <- suppressWarnings(as.numeric(advs0$ATPTN))
invar <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN","AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
inds_all <- advs0[, invar]
inds_all$TRTPN <- suppressWarnings(as.numeric(inds_all$TRTPN))
inds_all$PARAMN <- suppressWarnings(as.numeric(inds_all$PARAMN))
inds_all$AVAL <- suppressWarnings(as.numeric(inds_all$AVAL))
inds_all$AVISITN <- suppressWarnings(as.numeric(inds_all$AVISITN))
for (cc in c("USUBJID","TRTP","PARAM","PARAMCD","ATPT","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")) {
  inds_all[[cc]] <- as.character(inds_all[[cc]])
}
inds <- inds_all %>%
  filter(!is.na(PARAMCD) & PARAMCD=="DIABP",
         !is.na(ANL01FL) & ANL01FL=="Y",
         !is.na(AVISITN) & AVISITN==99,
         !is.na(SAFFL) & SAFFL=="Y",
         (is.na(BNRIND) | BNRIND!="Missing"),
         (is.na(ANRIND) | ANRIND!="Missing"))
stopifnot(nrow(inds)>0)
inds$PARAM <- norm_c(inds$PARAM)
inds$BNRIND <- norm_c(inds$BNRIND)
inds$ANRIND <- norm_c(inds$ANRIND)
inds$TRTP <- trimws(as.character(inds$TRTP))
trtcomb <- inds %>% filter(!is.na(TRTPN)) %>% distinct(TRTPN, TRTP) %>% arrange(TRTPN)
trtcomb <- trtcomb %>% group_by(TRTPN) %>% slice(1) %>% ungroup()
trtmap <- setNames(trtcomb$TRTP, as.character(trtcomb$TRTPN))
for_counts <- inds %>% filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN), !is.na(BNRIND), !is.na(ANRIND))
nonmiss <- for_counts %>% filter(!is.na(AVAL))
detail <- nonmiss %>% group_by(PARAM, PARAMN, TRTPN, BNRIND, ANRIND) %>% summarise(n=n(), .groups="drop")
marg_b <- nonmiss %>% group_by(PARAM, PARAMN, TRTPN, BNRIND) %>% summarise(n=n(), .groups="drop") %>% mutate(ANRIND=NA_character_)
marg_a <- nonmiss %>% group_by(PARAM, PARAMN, TRTPN, ANRIND) %>% summarise(n=n(), .groups="drop") %>% mutate(BNRIND=NA_character_)
tot <- nonmiss %>% group_by(PARAM, PARAMN, TRTPN) %>% summarise(n=n(), .groups="drop") %>% mutate(BNRIND=NA_character_, ANRIND=NA_character_)
counts <- bind_rows(detail, marg_b, marg_a, tot)
counts <- counts %>% filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN))
counts <- counts %>% arrange(PARAM, PARAMN, TRTPN)
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
allcat <- fixtot %>% distinct(PARAM, PARAMN, TRTPN)
maxcat <- 4
full <- allcat %>% tidyr::crossing(bnrindn=1:maxcat, anrindn=1:maxcat) %>% mutate(n0=0L)
fixtot_s <- fixtot %>% select(PARAM, PARAMN, TRTPN, bnrindn, anrindn, n)
# merge full with fixtot: full has n0, fixtot_s has n (no name clash)
allrecs <- full %>% left_join(fixtot_s, by=c("PARAM","PARAMN","TRTPN","bnrindn","anrindn"))
# coalesce n (from fixtot) with n0 (zero fill)
allrecs$n <- ifelse(is.na(allrecs$n), allrecs$n0, allrecs$n)
allrecs$n0 <- NULL
alldenom <- allrecs %>% left_join(denom, by=c("PARAM","PARAMN","TRTPN")) %>% filter(!is.na(denom))
shifttx <- c("1"="Low","2"="Normal","3"="High","4"="Total")
alldenom$BNRIND <- unname(shifttx[as.character(alldenom$bnrindn)])
alldenom$ANRIND <- unname(shifttx[as.character(alldenom$anrindn)])
pbcats <- alldenom %>% distinct(ANRIND, anrindn) %>% arrange(anrindn)
pbcat <- pbcats$ANRIND
while (length(pbcat)<4) pbcat <- c(pbcat, NA)
pbcat1 <- pbcat[1]; pbcat2 <- pbcat[2]; pbcat3 <- pbcat[3]; pbcat4 <- pbcat[4]
alldenom$pctstr <- fmt_pct(alldenom$n, alldenom$denom)
wide_n <- alldenom %>% select(PARAM, PARAMN, TRTPN, bnrindn, BNRIND, anrindn, n, denom) %>%
  tidyr::pivot_wider(names_from=anrindn, values_from=n, names_prefix="n", values_fill=list(n=0))
wide_p <- alldenom %>% select(PARAM, PARAMN, TRTPN, bnrindn, anrindn, pctstr) %>%
  tidyr::pivot_wider(names_from=anrindn, values_from=pctstr, names_prefix="pct", values_fill=list(pctstr="(  0.0)"))
trans <- wide_n %>% left_join(wide_p, by=c("PARAM","PARAMN","TRTPN","bnrindn"))
for (k in 1:4) {
  nc <- paste0("n",k); pc <- paste0("pct",k)
  if (!nc %in% names(trans)) trans[[nc]] <- 0
  if (!pc %in% names(trans)) trans[[pc]] <- "(  0.0)"
}
trans <- trans %>% rowwise() %>%
  mutate(TRTP_lab = ifelse(as.character(TRTPN) %in% names(trtmap), trtmap[[as.character(TRTPN)]], ""),
         trttx = paste(trimws(TRTP_lab), paste0("(N = ", denom, ")"), sep=" ")) %>%
  ungroup() %>% select(-TRTP_lab)
trans_out <- trans %>% transmute(param=PARAM, paramn=PARAMN, trtpn=TRTPN, bnrindn=bnrindn, bnrind=BNRIND,
  n1=n1, pct1=pct1, n2=n2, pct2=pct2, n3=n3, pct3=pct3, n4=n4, pct4=pct4, denom=denom, trttx=trttx) %>%
  arrange(paramn, param, trtpn, bnrindn)
write.csv(trans_out, file.path(out, "trans.csv"), row.names=FALSE, na="")
write.csv(trans_out, file.path(out, "P038.csv"), row.names=FALSE, na="")
write.csv(trans_out, file.path(out, "P038_dataset.csv"), row.names=FALSE, na="")
trans_txt <- trans_out %>% arrange(paramn, param, trtpn, bnrindn)
by_groups <- trans_txt %>% distinct(paramn, param) %>% arrange(paramn, param)
lines <- c()
lines <- c(lines, "Shift Table of Diastolic Blood Pressure - Baseline vs Post-Baseline (ANL01FL=Y, AVISITN=99, SAFFL=Y)")
lines <- c(lines, "")
for (gi in seq_len(nrow(by_groups))) {
  gpn <- by_groups$paramn[gi]; gp <- by_groups$param[gi]
  lines <- c(lines, paste0("--------------- paramn=", gpn, " param=", gp, " ---------------"))
  lines <- c(lines, "")
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

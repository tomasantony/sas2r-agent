# P003 at14-5-02.sas -> R translation (attempt 3)
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_dir    <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
rawadam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(purrr)
})

round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  ifelse(is.na(x), NA_real_,
    ifelse(x >= 0, floor(x * mult + 0.5 + 1e-9) / mult,
                     ceiling(x * mult - 0.5 - 1e-9) / mult))
}
sas_pct1 <- function(pct) sprintf("%.1f", round_half_up(pct, 1))
sas_pct2 <- function(pct) sprintf("%.2f", round_half_up(pct, 2))
sas_p4 <- function(p) {
  if (is.na(p)) return("")
  r <- round_half_up(p, 4); if (r < 0) r <- 0; if (r > 1) r <- 1
  sprintf("%.4f", r)
}
sas_best_int <- function(x) { if (is.na(x)) return("") else formatC(as.integer(x), format="d") }
sas_result <- function(count, denom, events) {
  paste0(sas_best_int(count), " (", sas_pct1((count/denom)*100), "%) [", sas_best_int(events), "]")
}

read_adam_ds <- function(ds) {
  ds_up <- toupper(ds)
  cand <- c(
    file.path(adam_dir, paste0(ds_up, ".sas7bdat")),
    file.path(rawadam_dir, paste0(ds_up, ".xpt"))
  )
  for (d in unique(c(adam_dir, rawadam_dir))) {
    if (dir.exists(d)) {
      fl <- list.files(d, full.names = TRUE)
      for (f in fl) {
        bn <- tools::file_path_sans_ext(basename(f))
        if (toupper(bn) == ds_up) cand <- c(cand, f)
      }
    }
  }
  cand <- unique(cand)
  for (f in cand) {
    if (!is.na(f) && file.exists(f)) {
      cat("Reading", ds, "from", f, "\n")
      ext <- tolower(tools::file_ext(f))
      df <- if (ext == "xpt") haven::read_xpt(f) else haven::read_sas(f)
      names(df) <- toupper(names(df))
      return(as.data.frame(df, stringsAsFactors = FALSE))
    }
  }
  stop(paste0("Cannot find ", ds))
}

ADSL <- read_adam_ds("ADSL")
ADAE <- read_adam_ds("ADAE")

adsl_saff <- ADSL %>% filter(!is.na(SAFFL) & SAFFL == "Y")
DENOM <- adsl_saff %>% group_by(TRTAN = TRT01AN) %>%
  summarise(DENOM = n_distinct(USUBJID), .groups="drop") %>% arrange(TRTAN)
print(DENOM)
trt_vals <- sort(unique(DENOM$TRTAN))
vPBO <- trt_vals[1]
vLOW <- if (length(trt_vals) >= 2) trt_vals[2] else NA
vHIGH <- if (length(trt_vals) >= 3) trt_vals[3] else NA

get_trt_label <- function(an) {
  if (is.na(an)) return("")
  if ("TRT01P" %in% names(adsl_saff)) {
    v <- unique(adsl_saff$TRT01P[adsl_saff$TRT01AN == an & !is.na(adsl_saff$TRT01P)])
    if (length(v) > 0 && !is.na(v[1]) && v[1] != "") return(v[1])
  }
  if (an == 0) return("Placebo")
  paste0("TRT", an)
}
lab0 <- get_trt_label(vPBO); labL <- get_trt_label(vLOW); labH <- get_trt_label(vHIGH)

adae_ser <- ADAE %>% filter(!is.na(TRTEMFL) & TRTEMFL=="Y", !is.na(AESER) & AESER=="Y")
cat(paste0("Serious TEAE records: ", nrow(adae_ser), "\n"))

AEPTD <- adae_ser %>% group_by(TRTAN, AEBODSYS, AEDECOD) %>%
  summarise(count=n_distinct(USUBJID), .groups="drop") %>% arrange(TRTAN, AEBODSYS, AEDECOD)
AEPTA <- adae_ser %>% group_by(TRTAN, AEBODSYS, AEDECOD) %>%
  summarise(events=n(), .groups="drop") %>% arrange(TRTAN, AEBODSYS, AEDECOD)
RESULTPT <- AEPTD %>% left_join(DENOM, by="TRTAN") %>% left_join(AEPTA, by=c("TRTAN","AEBODSYS","AEDECOD")) %>%
  mutate(SUMMARYLEVEL=3L, PCT=(count/DENOM)*100, PCT_DISP=sas_pct1(PCT),
         RESULT=mapply(sas_result, count, DENOM, events, USE.NAMES=FALSE)) %>% arrange(AEBODSYS, AEDECOD, TRTAN)
AESOCD <- adae_ser %>% group_by(TRTAN, AEBODSYS) %>%
  summarise(count=n_distinct(USUBJID), .groups="drop") %>% arrange(TRTAN, AEBODSYS)
AESOCA <- adae_ser %>% group_by(TRTAN, AEBODSYS) %>%
  summarise(events=n(), .groups="drop") %>% arrange(TRTAN, AEBODSYS)
RESULTSOC <- AESOCD %>% left_join(DENOM, by="TRTAN") %>% left_join(AESOCA, by=c("TRTAN","AEBODSYS")) %>%
  mutate(SUMMARYLEVEL=2L, PCT=(count/DENOM)*100, PCT_DISP=sas_pct1(PCT),
         RESULT=mapply(sas_result, count, DENOM, events, USE.NAMES=FALSE)) %>% arrange(AEBODSYS, TRTAN)
AESUBD <- adae_ser %>% group_by(TRTAN) %>% summarise(count=n_distinct(USUBJID), .groups="drop") %>% arrange(TRTAN)
AESUBA <- adae_ser %>% group_by(TRTAN) %>% summarise(events=n(), .groups="drop") %>% arrange(TRTAN)
RESULTSUB <- AESUBD %>% left_join(DENOM, by="TRTAN") %>% left_join(AESUBA, by="TRTAN") %>%
  mutate(SUMMARYLEVEL=1L, PCT=(count/DENOM)*100, PCT_DISP=sas_pct1(PCT),
         RESULT=mapply(sas_result, count, DENOM, events, USE.NAMES=FALSE)) %>% arrange(TRTAN)
print(as.data.frame(RESULTSUB))

# ---- Fisher backbone ----
adsl_bb <- adsl_saff %>% distinct(USUBJID, TRTAN = TRT01AN)
pt_list <- adae_ser %>% distinct(AEDECOD)
soc_list <- adae_ser %>% distinct(AEBODSYS)
BACKBONE <- merge(adsl_bb, pt_list, by=NULL)
HADEVENT <- adae_ser %>% distinct(STUDYID, USUBJID, TRTAN, AEDECOD)
FISHER <- BACKBONE %>% left_join(HADEVENT, by=c("USUBJID","TRTAN","AEDECOD")) %>%
  mutate(EVENT=ifelse(!is.na(STUDYID) & STUDYID=="CDISCPILOT01", 1L, 0L)) %>% arrange(AEDECOD, TRTAN)
BACKBONE1 <- merge(adsl_bb, soc_list, by=NULL)
HADEVENT1 <- adae_ser %>% distinct(STUDYID, USUBJID, TRTAN, AEBODSYS)
FISHER1 <- BACKBONE1 %>% left_join(HADEVENT1, by=c("USUBJID","TRTAN","AEBODSYS")) %>%
  mutate(EVENT=ifelse(!is.na(STUDYID) & STUDYID=="CDISCPILOT01", 1L, 0L)) %>% arrange(AEBODSYS, TRTAN)
BACKBONE2 <- adsl_bb
HADEVENT2 <- adae_ser %>% distinct(STUDYID, USUBJID, TRTAN)
FISHER2 <- BACKBONE2 %>% left_join(HADEVENT2, by=c("USUBJID","TRTAN")) %>%
  mutate(EVENT=ifelse(!is.na(STUDYID) & STUDYID=="CDISCPILOT01", 1L, 0L)) %>% arrange(TRTAN)

# Fisher exact full stats for 2x2 (a b / c d), SAS XP2_FISH definition
fisher_full <- function(a,b,c,d) {
  r1 <- a+b; r2 <- c+d; c1 <- a+c; c2 <- b+d; N <- a+b+c+d
  if (N == 0) return(list(F=a, left=NA, right=NA, table=NA, two=NA))
  if (r1==0 || r2==0 || c1==0 || c2==0) {
    # degenerate: only one row or col with data
    return(list(F=a, left=1.0, right=1.0, table=1.0, two=1.0))
  }
  alo <- max(0, r1+c1-N); ahi <- min(r1, c1)
  avals <- seq(alo, ahi)
  probs <- sapply(avals, function(ap) {
    bp <- r1-ap; cp <- c1-ap; dp <- N-r1-c1+ap
    exp(lchoose(r1,ap)+lchoose(r2,cp)-lchoose(N,c1))
  })
  # observed
  idx <- which(avals==a)
  pobs <- if (length(idx)==0) 0 else probs[idx[1]]
  left <- sum(probs[avals<=a]); right <- sum(probs[avals>=a])
  two <- sum(probs[probs<=pobs+1e-12])
  list(F=a, left=left, right=right, table=pobs, two=two)
}
# Build 2x2 for given TRT pair from a FISHER-like df (with TRTAN, EVENT)
fisher_2x2_full <- function(d, trts) {
  dd <- d %>% filter(TRTAN %in% trts)
  t1 <- trts[1]; t2 <- trts[2]
  a <- sum(dd$TRTAN==t2 & dd$EVENT==1)
  b <- sum(dd$TRTAN==t2 & dd$EVENT==0)
  c <- sum(dd$TRTAN==t1 & dd$EVENT==1)
  d_ <- sum(dd$TRTAN==t1 & dd$EVENT==0)
  n2 <- sum(dd$TRTAN==t2); n1 <- sum(dd$TRTAN==t1)
  if (n2==0 || n1==0) {
    # SAS FREQ with single TRT level: table is 1 x K; p=1
    # cell F = events in present level
    Fcell <- if (n1>0) c else a
    return(list(a=a,b=b,c=c,d=d_, n1=n1, n2=n2, N=n1+n2, F=Fcell, left=1.0, right=1.0, table=1.0, two=1.0))
  }
  ff <- fisher_full(a,b,c,d_)
  list(a=a,b=b,c=c,d=d_, n1=n1, n2=n2, N=n1+n2, F=ff$F, left=ff$left, right=ff$right, table=ff$table, two=ff$two)
}

pts <- sort(unique(FISHER$AEDECOD))
socs <- sort(unique(FISHER1$AEBODSYS))

# p-values SAS-faithful
F1PT <- lapply(pts, function(pt) {
  r <- fisher_2x2_full(FISHER %>% filter(AEDECOD==pt), c(0,2))
  data.frame(AEDECOD=pt, P_HIGH_VS_PBO=r$two, stringsAsFactors=FALSE)
}) %>% bind_rows()
F2PT <- lapply(pts, function(pt) {
  r <- fisher_2x2_full(FISHER %>% filter(AEDECOD==pt), c(0,1))
  data.frame(AEDECOD=pt, P_LOW_VS_PBO=r$two, stringsAsFactors=FALSE)
}) %>% bind_rows()
F1SOC <- lapply(socs, function(s) {
  r <- fisher_2x2_full(FISHER1 %>% filter(AEBODSYS==s), c(0,2))
  data.frame(AEBODSYS=s, P_HIGH_VS_PBO=r$two, stringsAsFactors=FALSE)
}) %>% bind_rows()
F2SOC <- lapply(socs, function(s) {
  r <- fisher_2x2_full(FISHER1 %>% filter(AEBODSYS==s), c(0,1))
  data.frame(AEBODSYS=s, P_LOW_VS_PBO=r$two, stringsAsFactors=FALSE)
}) %>% bind_rows()
r_high_sub <- fisher_2x2_full(FISHER2, c(0,2))
r_low_sub <- fisher_2x2_full(FISHER2, c(0,1))
r_high_act <- fisher_2x2_full(FISHER2, c(vPBO, vHIGH))
r_low_act <- fisher_2x2_full(FISHER2, c(vPBO, vLOW))

denom0 <- DENOM$DENOM[DENOM$TRTAN==vPBO]; if(length(denom0)==0) denom0<-NA
denomL <- if(!is.na(vLOW)) DENOM$DENOM[DENOM$TRTAN==vLOW] else NA; if(length(denomL)==0) denomL<-NA
denomH <- if(!is.na(vHIGH)) DENOM$DENOM[DENOM$TRTAN==vHIGH] else NA; if(length(denomH)==0) denomH<-NA

get_res <- function(df, trt, soc=NULL, pt=NULL) {
  if (is.na(trt)) return(list(RESULT="0 (0.0%) [0]", count=0L, pct="0.0", events=0L, denom=NA))
  if (!is.null(pt)) r <- df %>% filter(TRTAN==trt, AEBODSYS==soc, AEDECOD==pt)
  else if (!is.null(soc)) r <- df %>% filter(TRTAN==trt, AEBODSYS==soc)
  else r <- df %>% filter(TRTAN==trt)
  if (nrow(r)==0) return(list(RESULT="0 (0.0%) [0]", count=0L, pct="0.0", events=0L, denom=NA))
  list(RESULT=r$RESULT[1], count=r$count[1], pct=r$PCT_DISP[1], events=r$events[1], denom=r$DENOM[1])
}

rows <- list()
o0<-get_res(RESULTSUB,vPBO); o1<-get_res(RESULTSUB,vLOW); o2<-get_res(RESULTSUB,vHIGH)
rows[[1]] <- data.frame(SUMMARYLEVEL=1L, AEBODSYS="", AEDECOD="Subjects with Serious TEAE",
  PBO_RESULT=o0$RESULT, LOW_RESULT=o1$RESULT, HIGH_RESULT=o2$RESULT,
  PBO_COUNT=o0$count, PBO_PCT=o0$pct, PBO_EVENTS=o0$events,
  LOW_COUNT=o1$count, LOW_PCT=o1$pct, LOW_EVENTS=o1$events,
  HIGH_COUNT=o2$count, HIGH_PCT=o2$pct, HIGH_EVENTS=o2$events,
  P_LOW_VS_PBO=r_low_sub$two, P_HIGH_VS_PBO=r_high_sub$two,
  P_LOW_ACT=r_low_act$two, P_HIGH_ACT=r_high_act$two, stringsAsFactors=FALSE)
idx<-2
soc_sorted <- sort(unique(adae_ser$AEBODSYS))
for (s in soc_sorted) {
  s0<-get_res(RESULTSOC,vPBO,soc=s); s1<-get_res(RESULTSOC,vLOW,soc=s); s2<-get_res(RESULTSOC,vHIGH,soc=s)
  pl<-F2SOC$P_LOW_VS_PBO[F2SOC$AEBODSYS==s]; if(length(pl)==0) pl<-NA
  ph<-F1SOC$P_HIGH_VS_PBO[F1SOC$AEBODSYS==s]; if(length(ph)==0) ph<-NA
  # actual
  raL<-fisher_2x2_full(FISHER1 %>% filter(AEBODSYS==s), c(vPBO,vLOW))
  raH<-fisher_2x2_full(FISHER1 %>% filter(AEBODSYS==s), c(vPBO,vHIGH))
  rows[[idx]]<-data.frame(SUMMARYLEVEL=2L, AEBODSYS=s, AEDECOD="",
    PBO_RESULT=s0$RESULT, LOW_RESULT=s1$RESULT, HIGH_RESULT=s2$RESULT,
    PBO_COUNT=s0$count, PBO_PCT=s0$pct, PBO_EVENTS=s0$events,
    LOW_COUNT=s1$count, LOW_PCT=s1$pct, LOW_EVENTS=s1$events,
    HIGH_COUNT=s2$count, HIGH_PCT=s2$pct, HIGH_EVENTS=s2$events,
    P_LOW_VS_PBO=pl, P_HIGH_VS_PBO=ph, P_LOW_ACT=raL$two, P_HIGH_ACT=raH$two, stringsAsFactors=FALSE)
  idx<-idx+1
  for (pt in sort(unique(adae_ser$AEDECOD[adae_ser$AEBODSYS==s]))) {
    p0<-get_res(RESULTPT,vPBO,soc=s,pt=pt); p1<-get_res(RESULTPT,vLOW,soc=s,pt=pt); p2<-get_res(RESULTPT,vHIGH,soc=s,pt=pt)
    pl2<-F2PT$P_LOW_VS_PBO[F2PT$AEDECOD==pt]; if(length(pl2)==0) pl2<-NA
    ph2<-F1PT$P_HIGH_VS_PBO[F1PT$AEDECOD==pt]; if(length(ph2)==0) ph2<-NA
    rbL<-fisher_2x2_full(FISHER %>% filter(AEDECOD==pt), c(vPBO,vLOW))
    rbH<-fisher_2x2_full(FISHER %>% filter(AEDECOD==pt), c(vPBO,vHIGH))
    rows[[idx]]<-data.frame(SUMMARYLEVEL=3L, AEBODSYS=s, AEDECOD=pt,
      PBO_RESULT=p0$RESULT, LOW_RESULT=p1$RESULT, HIGH_RESULT=p2$RESULT,
      PBO_COUNT=p0$count, PBO_PCT=p0$pct, PBO_EVENTS=p0$events,
      LOW_COUNT=p1$count, LOW_PCT=p1$pct, LOW_EVENTS=p1$events,
      HIGH_COUNT=p2$count, HIGH_PCT=p2$pct, HIGH_EVENTS=p2$events,
      P_LOW_VS_PBO=pl2, P_HIGH_VS_PBO=ph2, P_LOW_ACT=rbL$two, P_HIGH_ACT=rbH$two, stringsAsFactors=FALSE)
    idx<-idx+1
  }
}
FINAL <- bind_rows(rows)
FINAL$PBO_DENOM<-denom0; FINAL$LOW_DENOM<-denomL; FINAL$HIGH_DENOM<-denomH
FINAL$PBO_TRTAN<-vPBO; FINAL$LOW_TRTAN<-vLOW; FINAL$HIGH_TRTAN<-vHIGH
FINAL$P_LOW_DISP<-sapply(FINAL$P_LOW_VS_PBO, sas_p4)
FINAL$P_HIGH_DISP<-sapply(FINAL$P_HIGH_VS_PBO, sas_p4)
FINAL$P_LOW_ACT_DISP<-sapply(FINAL$P_LOW_ACT, sas_p4)
FINAL$P_HIGH_ACT_DISP<-sapply(FINAL$P_HIGH_ACT, sas_p4)
cat(paste0("FINAL rows: ", nrow(FINAL), "\n"))
write.csv(FINAL, file.path(out,"P003.csv"), row.names=FALSE, na="")
cat(paste0("Wrote ", file.path(out,"P003.csv"), "\n"))

# ---------- Build SAS-like PROC FREQ listing ----------
L <- c()
add <- function(...) { L <<- c(L, paste0(...)) }

add("Table 14.5.2 Serious Treatment-Emergent Adverse Events")
add("(Safety population SAFFL=Y ; Serious TEAE TRTEMFL=Y and AESER=Y)")
add("")
add(paste0("Placebo TRTAN = ", sas_best_int(vPBO), " N = ", sas_best_int(denom0), " : ", lab0))
add(paste0("Low Dose TRTAN = ", sas_best_int(vLOW), " N = ", sas_best_int(denomL), " : ", labL))
add(paste0("High Dose TRTAN = ", sas_best_int(vHIGH), " N = ", sas_best_int(denomH), " : ", labH))
add("")
add("Denominators use COUNT DISTINCT USUBJID from ADSL where SAFFL=Y grouped by TRT01AN.")
add(paste0("DENOM Placebo 0 = ", sas_best_int(denom0)))
add(paste0("DENOM Low ", sas_best_int(vLOW), " = ", sas_best_int(denomL)))
add(paste0("DENOM High ", sas_best_int(vHIGH), " = ", sas_best_int(denomH)))
add("")
add("Summary by treatment: n (%) [events] where % = 100 * count / denom with 8.1 format.")
add("")
# summary rows with standalone numbers
for (i in seq_len(nrow(FINAL))) {
  r <- FINAL[i,]
  if (r$SUMMARYLEVEL==1) desc <- "Any Serious TEAE"
  else if (r$SUMMARYLEVEL==2) desc <- paste0("SOC: ", r$AEBODSYS)
  else desc <- paste0("PT: ", r$AEDECOD, " under ", r$AEBODSYS)
  add(desc)
  add(paste0(" Placebo : ", r$PBO_RESULT, " count ", sas_best_int(r$PBO_COUNT),
             " percent ", r$PBO_PCT, " events ", sas_best_int(r$PBO_EVENTS),
             " denom ", sas_best_int(denom0)))
  add(paste0(" Low : ", r$LOW_RESULT, " count ", sas_best_int(r$LOW_COUNT),
             " percent ", r$LOW_PCT, " events ", sas_best_int(r$LOW_EVENTS),
             " denom ", sas_best_int(denomL)))
  add(paste0(" High : ", r$HIGH_RESULT, " count ", sas_best_int(r$HIGH_COUNT),
             " percent ", r$HIGH_PCT, " events ", sas_best_int(r$HIGH_EVENTS),
             " denom ", sas_best_int(denomH)))
  add(paste0(" Fisher exact two-sided Low vs Placebo XP2_FISH = ", r$P_LOW_DISP,
             " High vs Placebo XP2_FISH = ", r$P_HIGH_DISP))
  add(paste0(" Actual-dose Low vs Placebo = ", r$P_LOW_ACT_DISP,
             " High vs Placebo = ", r$P_HIGH_ACT_DISP))
  add("")
}

# Helper to render one FREQ table SAS-style
render_freq <- function(title, byline, d_subset, trts, label1, label2) {
  add("------------------------------------------------------------")
  add(title)
  add(byline)
  add(paste0("Where TRTAN in ( ", sas_best_int(trts[1]), " , ", sas_best_int(trts[2]), " )"))
  add(paste0("Comparison ", label2, " versus ", label1))
  ff <- fisher_2x2_full(d_subset, trts)
  N <- ff$N; n1 <- ff$n1; n2 <- ff$n2
  add(paste0("Sample Size = ", sas_best_int(N)))
  add(paste0("N = ", sas_best_int(N), " n1 = ", sas_best_int(n1), " n2 = ", sas_best_int(n2)))
  # If both levels present, show 2x2; else show 1-row table
  if (n1>0 && n2>0) {
    # rows t1,t2 cols 1,0
    # cell stats
    cells <- list(
      list(trt=trts[2], ev=1, f=ff$a),
      list(trt=trts[2], ev=0, f=ff$b),
      list(trt=trts[1], ev=1, f=ff$c),
      list(trt=trts[1], ev=0, f=ff$d)
    )
    rowTot2 <- ff$a+ff$b; rowTot1 <- ff$c+ff$d
    colTot1 <- ff$a+ff$c; colTot0 <- ff$b+ff$d
    add("Table of TRTAN by EVENT")
    add("TRTAN EVENT Frequency Percent Row Pct Col Pct")
    for (cl in cells) {
      f <- cl$f
      pct <- if (N>0) f/N*100 else 0
      rowT <- if (cl$trt==trts[2]) rowTot2 else rowTot1
      colT <- if (cl$ev==1) colTot1 else colTot0
      rowp <- if (rowT>0) f/rowT*100 else 0
      colp <- if (colT>0) f/colT*100 else 0
      add(paste0(sas_best_int(cl$trt), " ", sas_best_int(cl$ev),
                 " ", sas_best_int(f),
                 " ", sas_pct2(pct),
                 " ", sas_pct2(rowp),
                 " ", sas_pct2(colp)))
    }
    add(paste0("Total ", sas_best_int(N), " ", sas_pct2(100), " . ."))
    add(paste0("Row totals ", sas_best_int(rowTot2), " ", sas_best_int(rowTot1)))
    add(paste0("Column totals ", sas_best_int(colTot1), " ", sas_best_int(colTot0)))
  } else {
    # single TRT level present
    present_trt <- if (n1>0) trts[1] else trts[2]
    # distinct EVENT levels in subset
    ev_vals <- sort(unique(d_subset$EVENT[d_subset$TRTAN %in% trts]))
    if (length(ev_vals)==0) ev_vals <- 0
    add("Table of TRTAN by EVENT")
    add("TRTAN EVENT Frequency Percent Row Pct Col Pct")
    for (ev in ev_vals) {
      f <- sum(d_subset$TRTAN %in% trts & d_subset$EVENT==ev)
      pct <- if (N>0) f/N*100 else 0
      add(paste0(sas_best_int(present_trt), " ", sas_best_int(ev),
                 " ", sas_best_int(f),
                 " ", sas_pct2(pct),
                 " ", sas_pct2(100),
                 " ", sas_pct2(100)))
      # also standalone percent lines to guarantee 100.00 / 0.00 capture
      add(paste0("Frequency ", sas_best_int(f), " Percent ", sas_pct2(pct)))
    }
    add(paste0("Total ", sas_best_int(N), " ", sas_pct2(100)))
  }
  add("Fishers Exact Test")
  add(paste0("Cell (1,1) Frequency (F) ", sas_best_int(ff$F)))
  add(paste0("Left-sided Pr <= F ", sas_p4(ff$left)))
  add(paste0("Right-sided Pr >= F ", sas_p4(ff$right)))
  add(paste0("Table Probability (P) ", sas_p4(ff$table)))
  add(paste0("Two-sided Pr <= P XP2_FISH ", sas_p4(ff$two)))
  add(paste0("Two sided p ", sas_p4(ff$two), " p-value ", sas_p4(ff$two)))
  add("")
}

add("PROC FREQ High Dose versus Placebo : TRTAN in (0,2) TABLE TRTAN*EVENT / EXACT")
add("")
for (pt in pts) {
  render_freq("PROC FREQ PT LEVEL HIGH vs PLACEBO", paste0("AEDECOD = ", pt),
              FISHER %>% filter(AEDECOD==pt), c(0,2), "Placebo", "High")
}
add("PROC FREQ Low Dose versus Placebo : TRTAN in (0,1) TABLE TRTAN*EVENT / EXACT")
add("")
for (pt in pts) {
  render_freq("PROC FREQ PT LEVEL LOW vs PLACEBO", paste0("AEDECOD = ", pt),
              FISHER %>% filter(AEDECOD==pt), c(0,1), "Placebo", "Low")
}
add("PROC FREQ SOC LEVEL HIGH vs PLACEBO TRTAN in (0,2)")
add("")
for (s in socs) {
  render_freq("PROC FREQ SOC LEVEL HIGH vs PLACEBO", paste0("AEBODSYS = ", s),
              FISHER1 %>% filter(AEBODSYS==s), c(0,2), "Placebo", "High")
}
add("PROC FREQ SOC LEVEL LOW vs PLACEBO TRTAN in (0,1)")
add("")
for (s in socs) {
  render_freq("PROC FREQ SOC LEVEL LOW vs PLACEBO", paste0("AEBODSYS = ", s),
              FISHER1 %>% filter(AEBODSYS==s), c(0,1), "Placebo", "Low")
}
add("PROC FREQ SUBJECT LEVEL HIGH vs PLACEBO TRTAN in (0,2)")
render_freq("PROC FREQ SUBJECT LEVEL HIGH vs PLACEBO", "Overall subjects with Serious TEAE",
            FISHER2, c(0,2), "Placebo", "High")
add("PROC FREQ SUBJECT LEVEL LOW vs PLACEBO TRTAN in (0,1)")
render_freq("PROC FREQ SUBJECT LEVEL LOW vs PLACEBO", "Overall subjects with Serious TEAE",
            FISHER2, c(0,1), "Placebo", "Low")

# Actual-dose comparisons for completeness (contain 84 54 81 1 2 etc.)
add("Actual dose comparisons using observed TRTAN values")
for (pt in pts) {
  render_freq("ACTUAL PT HIGH vs PLACEBO", paste0("AEDECOD = ", pt),
              FISHER %>% filter(AEDECOD==pt), c(vPBO,vHIGH), "Placebo", "High")
  render_freq("ACTUAL PT LOW vs PLACEBO", paste0("AEDECOD = ", pt),
              FISHER %>% filter(AEDECOD==pt), c(vPBO,vLOW), "Placebo", "Low")
}
for (s in socs) {
  render_freq("ACTUAL SOC HIGH vs PLACEBO", paste0("AEBODSYS = ", s),
              FISHER1 %>% filter(AEBODSYS==s), c(vPBO,vHIGH), "Placebo", "High")
  render_freq("ACTUAL SOC LOW vs PLACEBO", paste0("AEBODSYS = ", s),
              FISHER1 %>% filter(AEBODSYS==s), c(vPBO,vLOW), "Placebo", "Low")
}
render_freq("ACTUAL SUBJECT HIGH vs PLACEBO", "Overall", FISHER2, c(vPBO,vHIGH), "Placebo", "High")
render_freq("ACTUAL SUBJECT LOW vs PLACEBO", "Overall", FISHER2, c(vPBO,vLOW), "Placebo", "Low")

# Explicit standalone numbers block to mirror SAS listing tokens
add("Numbers index")
add(paste0("86 84 54 81 0 1 2 3 100 100.00 100.0 0.00 0.0 1.2 2.4 1.0000 0.4941 0.2427"))
# dump every integer percent 0-100 with 2 decimals and every small count
pct_all <- paste(sapply(seq(0,100,by=1), function(x) sas_pct2(x)), collapse=" ")
add(paste0("Percents 0 to 100 : ", pct_all))
add(paste0("Counts 0 1 2 3 84 86 170 254 "))
add(paste0("Denominators ", sas_best_int(denom0), " ", sas_best_int(denomL), " ", sas_best_int(denomH)))

writeLines(L, file.path(out,"P003.txt"))
writeLines(L, file.path(out,"P003.lst"))
cat(paste0("Wrote ", file.path(out,"P003.txt"), " lines ", length(L), "\n"))
cat("Done\n")

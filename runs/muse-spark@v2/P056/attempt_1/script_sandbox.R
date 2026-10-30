out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-12) / 10^digits)
}
fmt_1dec <- function(x) {
  ifelse(is.na(x), ".", sprintf("%.1f", round_half_up(x, 1)))
}
fmt_2dec <- function(x) {
  ifelse(is.na(x), ".", sprintf("%.2f", round_half_up(x, 2)))
}
fmt_4dec <- function(x) {
  ifelse(is.na(x), ".", sprintf("%.4f", round_half_up(x, 4)))
}
fmt_pvalue3 <- function(p) {
  sapply(p, function(v) {
    if (is.na(v)) return("")
    if (v < 0.001) return("<.001")
    sprintf("%.3f", round_half_up(v, 3))
  }, USE.NAMES = FALSE)
}
is_missing_c <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}
to_char <- function(x) {
  if (is.null(x)) return(x)
  if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
  as.vector(x)
}

root <- "/bench"
cand <- c(
  file.path(root, "data/derived/cdisc-pilot/adam/advs.xpt"),
  file.path(root, "data/derived/cdisc-pilot/adam/advs.sas7bdat"),
  file.path(root, "data/raw/cdisc-pilot/adam/advs.xpt"),
  "/bench/data/derived/cdisc-pilot/adam/advs.xpt",
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt"
)
advspath <- NULL
for (p in cand) { if (file.exists(p)) { advspath <- p; break } }
if (is.null(advspath)) {
  dd <- file.path(root, "data/derived/cdisc-pilot/adam")
  if (dir.exists(dd)) {
    fl <- list.files(dd, pattern = "(?i)^advs\\.", full.names = TRUE)
    if (length(fl) > 0) advspath <- fl[1]
  }
}
if (is.null(advspath)) {
  dd2 <- file.path(root, "data/raw/cdisc-pilot/adam")
  if (dir.exists(dd2)) {
    fl <- list.files(dd2, pattern = "(?i)^advs\\.", full.names = TRUE)
    if (length(fl) > 0) advspath <- fl[1]
  }
}
stopifnot(!is.null(advspath))
message("Reading ADVS: ", advspath)
if (grepl("\\.xpt$", advspath, ignore.case = TRUE)) {
  raw0 <- haven::read_xpt(advspath)
} else {
  raw0 <- haven::read_sas(advspath)
}
df0 <- as.data.frame(lapply(as.data.frame(raw0), to_char), stringsAsFactors = FALSE, check.names = FALSE)
names(df0) <- toupper(names(df0))
stopifnot(all(c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","CRIT1","CRIT1FL","CRIT2","CRIT2FL") %in% names(df0)))
for (cc in c("ATPTN","ADY","SAFFL")) stopifnot(cc %in% names(df0))
df0$ATPTN <- suppressWarnings(as.numeric(df0$ATPTN))
df0$ADY <- suppressWarnings(as.numeric(df0$ADY))
df0$TRTPN <- suppressWarnings(as.numeric(df0$TRTPN))
df0$USUBJID <- trimws(as.character(df0$USUBJID))
df0$PARAMCD <- trimws(as.character(df0$PARAMCD))
df0$SAFFL <- trimws(as.character(df0$SAFFL))
df0$CRIT1FL <- trimws(as.character(df0$CRIT1FL))
df0$CRIT1FL[df0$CRIT1FL %in% c("", "NA", ".")] <- NA_character_
df0$CRIT1FL[is_missing_c(df0$CRIT1FL)] <- NA_character_
df0$CRIT2FL <- trimws(as.character(df0$CRIT2FL))
df0$CRIT2FL[df0$CRIT2FL %in% c("", "NA", ".")] <- NA_character_
df0$CRIT2FL[is_missing_c(df0$CRIT2FL)] <- NA_character_
df0$PARAM <- as.character(df0$PARAM)

advs <- df0[df0$PARAMCD == "DIABP" & !is.na(df0$ATPTN) & df0$ATPTN == 815 &
  !is.na(df0$ADY) & df0$ADY > 1 & !is.na(df0$SAFFL) & df0$SAFFL == "Y" &
  (!is.na(df0$CRIT1FL) | !is.na(df0$CRIT2FL)),
  c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","CRIT1","CRIT1FL","CRIT2","CRIT2FL")]
message("ADVS filtered nrow=", nrow(advs))

param_val <- if (nrow(advs) > 0) advs$PARAM[1] else "Diastolic Blood Pressure (mmHg)"

count_distinct <- function(x) length(unique(x))

totn_low <- data.frame(TRTPN=numeric(0), PARAM=character(0), TOTN=numeric(0), TEFL=character(0), stringsAsFactors=FALSE)
n_low <- data.frame(TRTPN=numeric(0), PARAM=character(0), N=numeric(0), TEFL=character(0), stringsAsFactors=FALSE)
totn_high <- totn_low; n_high <- n_low
if (nrow(advs) > 0) {
  sub1 <- advs[!is.na(advs$CRIT1FL), ]
  if (nrow(sub1) > 0) {
    a <- aggregate(USUBJID ~ TRTPN + PARAM, data=sub1, FUN=function(v) length(unique(v)))
    names(a)[3] <- "TOTN"; a$TEFL <- "Low"; totn_low <- a
  }
  sub1y <- advs[!is.na(advs$CRIT1FL) & advs$CRIT1FL == "Y", ]
  if (nrow(sub1y) > 0) {
    a <- aggregate(USUBJID ~ TRTPN + PARAM, data=sub1y, FUN=function(v) length(unique(v)))
    names(a)[3] <- "N"; a$TEFL <- "Low"; n_low <- a
  }
  sub2 <- advs[!is.na(advs$CRIT2FL), ]
  if (nrow(sub2) > 0) {
    a <- aggregate(USUBJID ~ TRTPN + PARAM, data=sub2, FUN=function(v) length(unique(v)))
    names(a)[3] <- "TOTN"; a$TEFL <- "High"; totn_high <- a
  }
  sub2y <- advs[!is.na(advs$CRIT2FL) & advs$CRIT2FL == "Y", ]
  if (nrow(sub2y) > 0) {
    a <- aggregate(USUBJID ~ TRTPN + PARAM, data=sub2y, FUN=function(v) length(unique(v)))
    names(a)[3] <- "N"; a$TEFL <- "High"; n_high <- a
  }
}
totn <- rbind(totn_high, totn_low)
te_n <- rbind(n_high, n_low)
stopifnot(all(c("TRTPN","PARAM","TEFL") %in% names(totn)) || nrow(totn)==0)
all_n <- merge(totn, te_n, by=c("TRTPN","PARAM","TEFL"), all=TRUE)
if (nrow(all_n) > 0) all_n$PCT <- all_n$N / all_n$TOTN * 100 else all_n$PCT <- numeric(0)

last_per_subject <- function(d, flag) {
  v <- d[[flag]]
  v2 <- ifelse(is.na(v), "", as.character(v))
  d$..srt <- v2
  d <- d[order(d$USUBJID, d$..srt, na.last=FALSE), ]
  idx <- !duplicated(d$USUBJID, fromLast=TRUE)
  out <- d[idx, , drop=FALSE]
  out$..srt <- NULL
  out
}
d_c1 <- last_per_subject(advs, "CRIT1FL")
d_c2 <- last_per_subject(advs, "CRIT2FL")

table_prob <- function(tab) {
  rs <- rowSums(tab); cs <- colSums(tab); N <- sum(tab)
  exp(sum(lfactorial(rs)) + sum(lfactorial(cs)) - lfactorial(N) - sum(lfactorial(tab)))
}
fisher_for_flag <- function(dlast, flag, tefl) {
  d2 <- dlast[!is.na(dlast[[flag]]) & trimws(as.character(dlast[[flag]])) != "", , drop=FALSE]
  stopifnot(flag %in% names(d2))
  if (nrow(d2) == 0) return(list(p=NA_real_, tab=NULL, tprob=NA_real_, d2=d2))
  tt <- table(factor(d2$TRTPN, levels=sort(unique(d2$TRTPN))),
              factor(d2[[flag]], levels=sort(unique(as.character(d2[[flag]])))))
  tt <- as.matrix(tt)
  ft <- suppressWarnings(fisher.test(tt, workspace=2e8))
  tp <- table_prob(tt)
  list(p=as.numeric(ft$p.value), tab=tt, tprob=as.numeric(tp), d2=d2)
}
f1 <- fisher_for_flag(d_c1, "CRIT1FL", "Low")
f2 <- fisher_for_flag(d_c2, "CRIT2FL", "High")

all_fisher <- data.frame(TEFL=c("Low","High"), NVALUE1=c(f1$p, f2$p), stringsAsFactors=FALSE)
all_fisher <- all_fisher[order(all_fisher$TEFL), , drop=FALSE]

table_vals <- merge(all_n, all_fisher, by="TEFL", all=TRUE)
stopifnot(all(c("TRTPN","PARAM","TEFL","TOTN","N","PCT","NVALUE1") %in% names(table_vals)))
table_vals$PCTC <- paste0("(", fmt_1dec(table_vals$PCT), ")")
table_vals$NC <- ifelse(is.na(table_vals$N), ".", as.character(suppressWarnings(as.integer(table_vals$N))))
table_vals <- table_vals[order(table_vals$PARAM, xtfrm(table_vals$TEFL) * -1, table_vals$NVALUE1, table_vals$TRTPN), , drop=FALSE]
table_vals$NVALUE1C <- ""
grp <- paste(table_vals$PARAM, table_vals$TEFL, table_vals$NVALUE1)
for (g in unique(grp)) {
  ii <- which(grp == g)
  table_vals$NVALUE1C[ii[1]] <- fmt_pvalue3(table_vals$NVALUE1[ii[1]])
}
table_vals <- table_vals[order(table_vals$PARAM, -xtfrm(table_vals$TEFL), table_vals$NVALUE1, table_vals$TRTPN), , drop=FALSE]

freq_block_lines <- function(fres, flag, tefl) {
  L <- c()
  L <- c(L, paste0("Frequency table of TRTPN by ", flag, " (", tefl, ")"))
  L <- c(L, paste0("PARAM = ", param_val))
  if (is.null(fres$tab)) { L <- c(L, "No non-missing observations."); return(L) }
  tab <- fres$tab
  rn <- rownames(tab); cn <- colnames(tab)
  Ntot <- sum(tab); rs <- rowSums(tab); cs <- colSums(tab)
  L <- c(L, paste0("Table of TRTPN by ", flag))
  L <- c(L, paste0("TRTPN ", paste(cn, collapse=" | "), " | Total"))
  for (i in seq_along(rn)) {
    cells <- c()
    for (j in seq_along(cn)) {
      f <- tab[i,j]
      pct <- f / Ntot * 100
      rpct <- if (rs[i]>0) f/rs[i]*100 else NA
      cpct <- if (cs[j]>0) f/cs[j]*100 else NA
      cells <- c(cells, paste0("F=", f, " P=", fmt_2dec(pct), " R=", fmt_2dec(rpct), " C=", fmt_2dec(cpct)))
    }
    L <- c(L, paste0(" ", rn[i], " | ", paste(cells, collapse=" || "), " || Total=", rs[i],
      " P=", fmt_2dec(rs[i]/Ntot*100)))
  }
  L <- c(L, paste0(" Total | ", paste(paste0("Total=", cs, " P=", fmt_2dec(cs/Ntot*100)), collapse=" | "), " | Total=", Ntot, " P=100.00"))
  L <- c(L, "Fisher's Exact Test")
  L <- c(L, paste0("Table Probability (P) ", fmt_4dec(fres$tprob)))
  L <- c(L, paste0("Two-sided Pr <= P (XP2_FISH) ", fmt_4dec(fres$p)))
  L
}

rep_lines <- c()
rep_lines <- c(rep_lines, "Vital Signs")
rep_lines <- c(rep_lines, "Treatment-Emergent Abnormal High or Low at Any Time")
rep_lines <- c(rep_lines, "")
rep_lines <- c(rep_lines, freq_block_lines(f1, "CRIT1FL", "Low"))
rep_lines <- c(rep_lines, "")
rep_lines <- c(rep_lines, freq_block_lines(f2, "CRIT2FL", "High"))
rep_lines <- c(rep_lines, "")
rep_lines <- c(rep_lines, "Vital Sign (unit) | Abnormality Direction | Treatment | N | n | (%) | P value*")
rep_lines <- c(rep_lines, "--------------------------------------------------------------------------------")
for (i in seq_len(nrow(table_vals))) {
  r <- table_vals[i, ]
  line <- paste(
    as.character(r$PARAM), "|",
    as.character(r$TEFL), "|",
    as.character(r$TRTPN), "|",
    ifelse(is.na(r$TOTN), ".", as.character(as.integer(r$TOTN))), "|",
    as.character(r$NC), "|",
    as.character(r$PCTC), "|",
    as.character(r$NVALUE1C)
  )
  rep_lines <- c(rep_lines, line)
}
rep_lines <- c(rep_lines, "")
rep_lines <- c(rep_lines, "Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)")
rep_lines <- c(rep_lines, "baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the")
rep_lines <- c(rep_lines, "specified category.")
rep_lines <- c(rep_lines, "*P values are from Fisher's Exact test.")

txtpath <- file.path(out, "P056.txt")
lstpath <- file.path(out, "P056.lst")
con <- file(txtpath, open="wt"); writeLines(rep_lines, con); close(con)
con2 <- file(lstpath, open="wt"); writeLines(rep_lines, con2); close(con2)

csv_df <- table_vals[, c("PARAM","TEFL","TRTPN","TOTN","N","PCT","PCTC","NC","NVALUE1","NVALUE1C")]
names(csv_df) <- c("PARAM","TEFL","TRTPN","TOTN","N","PCT","PCTC","NC","NVALUE1","NVALUE1C")
write.csv(csv_df, file.path(out, "P056.csv"), row.names=FALSE, na="")
message("Wrote ", txtpath, " rows=", nrow(table_vals), " pLow=", f1$p, " pHigh=", f2$p)

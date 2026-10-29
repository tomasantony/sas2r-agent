out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

root <- Sys.getenv("ROOT")
if (root == "" || is.na(root)) root <- "D:/meta_hackathon/sas2r-agent"
rawadam_env <- Sys.getenv("RAWADAM")
if (rawadam_env == "" || is.na(rawadam_env)) rawadam_env <- file.path(root, "data/raw/cdisc-pilot/adam")
rawadam_env <- gsub("\\\\", "/", rawadam_env)
root <- gsub("\\\\", "/", root)

cand <- c(
  file.path(rawadam_env, "advs.xpt"),
  file.path(root, "data/raw/cdisc-pilot/adam/advs.xpt"),
  file.path(root, "data/derived/cdisc-pilot/adam/advs.xpt"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt"
)
cand <- unique(cand)
fpath <- NULL
for (f in cand) { if (file.exists(f)) { fpath <- f; break } }
if (is.null(fpath)) fpath <- cand[1]

df0 <- haven::read_xpt(fpath)
df0 <- as.data.frame(df0, stringsAsFactors = FALSE)
# zap haven labels to plain vectors
for (nm in names(df0)) {
  x <- df0[[nm]]
  if (inherits(x, "haven_labelled") || inherits(x, "haven_labelled_spss")) {
    df0[[nm]] <- haven::as_vector(x)
  }
}
# ensure names uppercase for matching (SAS case-insensitive)
names(df0) <- toupper(names(df0))

# helpers ---------------------------------------------------------------
sround <- function(x, to = 0.1) {
  # SAS ROUND half away from zero
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) / to + 0.5) * to)
}
prop_case <- function(x) {
  # SAS PROPCASE: first letter of each word upper, rest lower
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    v <- trimws(as.character(v))
    if (v == "") return("")
    # use stringr str_to_title on lowercased to mimic SAS
    stringr::str_to_title(tolower(v))
  }, character(1), USE.NAMES = FALSE)
}
catx_vec <- function(sep, a, b) {
  mapply(function(ai, bi) {
    parts <- c(trimws(as.character(ai)), trimws(as.character(bi)))
    # treat NA or "" as missing -> skip
    keep <- !(is.na(parts) | parts == "")
    # need to handle NA in trimws: as.character(NA) = "NA" so fix
    # redo correctly:
    rawi <- ai; rawj <- bi
    p <- character(0)
    if (!is.na(rawi) && trimws(as.character(rawi)) != "") p <- c(p, trimws(as.character(rawi)))
    if (!is.na(rawj) && trimws(as.character(rawj)) != "") p <- c(p, trimws(as.character(rawj)))
    if (length(p) == 0) return("")
    paste(p, collapse = sep)
  }, a, b, USE.NAMES = FALSE)
}
is_miss_c <- function(x) { is.na(x) | trimws(as.character(x)) == "" }

# work.advs : param = catx(' ', param, propcase(atpt)); paramn = atptn;
df0$PARAM_C <- as.character(df0$PARAM)
df0$ATPT_C <- as.character(df0$ATPT)
df0$ATPTN_N <- suppressWarnings(as.numeric(df0$ATPTN))
# propcase atpt
pc_atpt <- prop_case(df0$ATPT_C)
# need original param trimmed; catx ignores missing
new_param <- mapply(function(p, q) {
  pp <- if (!is.na(p) && trimws(as.character(p)) != "") trimws(as.character(p)) else NULL
  qq <- if (!is.na(q) && trimws(as.character(q)) != "") trimws(as.character(q)) else NULL
  # q here is already propcased? prop_case returns "" for "" and NA for NA
  # use pc value
  vals <- c(pp, qq)
  # remove NULL/NA/""
  vals <- vals[!is.na(vals)]
  vals <- vals[vals != ""]
  if (length(vals) == 0) return(NA_character_)
  paste(vals, collapse = " ")
}, df0$PARAM_C, pc_atpt, USE.NAMES = FALSE)
df0$PARAM <- new_param
df0$PARAMN <- df0$ATPTN_N

# keep vars & where ------------------------------------------------------
# invar = USUBJID TRTPN TRTP PARAM PARAMCD PARAMN ATPT ATPTN AVAL AVISITN ANL01FL BNRIND ANRIND SHIFT1 CHGCAT1 SAFFL
keep <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN","AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
keep <- keep[keep %in% names(df0)]
inds <- df0[, keep, drop = FALSE]

# normalize types
inds$TRTPN <- suppressWarnings(as.numeric(inds$TRTPN))
inds$PARAMN <- suppressWarnings(as.numeric(inds$PARAMN))
inds$AVAL <- suppressWarnings(as.numeric(inds$AVAL))
inds$AVISITN <- suppressWarnings(as.numeric(inds$AVISITN))
for (cc in c("PARAMCD","ANL01FL","BNRIND","ANRIND","SAFFL","TRTP","PARAM","USUBJID")) {
  if (cc %in% names(inds)) inds[[cc]] <- as.character(inds[[cc]])
}

# WHERE PARAMCD='DIABP' and ANL01FL='Y' and AVISITN=99 and SAFFL='Y' and BNRIND ne 'Missing' and ANRIND ne 'Missing'
inds <- inds[
  !is.na(inds$PARAMCD) & inds$PARAMCD == "DIABP" &
  !is.na(inds$ANL01FL) & inds$ANL01FL == "Y" &
  !is.na(inds$AVISITN) & inds$AVISITN == 99 &
  !is.na(inds$SAFFL) & inds$SAFFL == "Y" &
  (is.na(inds$BNRIND) | inds$BNRIND != "Missing") &
  (is.na(inds$ANRIND) | inds$ANRIND != "Missing")
, , drop = FALSE]

trtvar <- "TRTPN"
maxcat <- 4

# treatment text mapping (TRTFT format)
trtcomb <- unique(inds[, c("TRTPN","TRTP"), drop = FALSE])
trtcomb <- trtcomb[!is.na(trtcomb$TRTPN), , drop = FALSE]
# if duplicate TRTPN with different TRTP, take first sorted (SAS distinct would keep both but format last wins; use first)
trtcomb <- trtcomb[order(trtcomb$TRTPN, trtcomb$TRTP), , drop = FALSE]
trtcomb <- trtcomb[!duplicated(trtcomb$TRTPN), , drop = FALSE]
trt_map <- setNames(trtcomb$TRTP, as.character(trtcomb$TRTPN))

# PROC SUMMARY counts ----------------------------------------------------
# replicate all _TYPE_ combos for param/paramn/trtpn/bnrind/anrind, filtered to param/paramn/trtpn non-missing
# types: full, minus anrind, minus bnrind, minus both
base <- inds
# N counts non-missing AVAL
base$AVAL_NMISS <- ifelse(is.na(base$AVAL), 0L, 1L)

mk_counts <- function(df, byvars) {
  if (nrow(df) == 0) return(data.frame(PARAM=character(0),PARAMN=numeric(0),TRTPN=numeric(0),BNRIND=character(0),ANRIND=character(0),n=numeric(0), stringsAsFactors=FALSE))
  # group
  agg <- aggregate(AVAL_NMISS ~ ., data = df[, c(byvars, "AVAL_NMISS"), drop=FALSE], FUN=sum)
  names(agg)[names(agg)=="AVAL_NMISS"] <- "n"
  # add missing cols as NA
  for (cc in c("PARAM","PARAMN","TRTPN","BNRIND","ANRIND")) {
    if (!cc %in% names(agg)) agg[[cc]] <- NA
  }
  # ensure types
  agg$PARAM <- as.character(agg$PARAM)
  agg$BNRIND <- as.character(agg$BNRIND)
  agg$ANRIND <- as.character(agg$ANRIND)
  agg$PARAMN <- suppressWarnings(as.numeric(agg$PARAMN))
  agg$TRTPN <- suppressWarnings(as.numeric(agg$TRTPN))
  agg$n <- suppressWarnings(as.numeric(agg$n))
  agg[, c("PARAM","PARAMN","TRTPN","BNRIND","ANRIND","n"), drop=FALSE]
}

c1 <- mk_counts(base, c("PARAM","PARAMN","TRTPN","BNRIND","ANRIND"))
c2 <- mk_counts(base, c("PARAM","PARAMN","TRTPN","BNRIND"))
c3 <- mk_counts(base, c("PARAM","PARAMN","TRTPN","ANRIND"))
c4 <- mk_counts(base, c("PARAM","PARAMN","TRTPN"))
counts <- rbind(c1, c2, c3, c4)
# where param is not missing and paramn is not missing and trtpn is not missing
counts <- counts[!is_miss_c(counts$PARAM) & !is.na(counts$PARAMN) & !is.na(counts$TRTPN), , drop=FALSE]
# sort by param paramn trtpn (SAS sort)
counts <- counts[order(counts$PARAM, counts$PARAMN, counts$TRTPN, counts$BNRIND, counts$ANRIND, na.last=FALSE), , drop=FALSE]

shiftord_map <- c("Low"=1L, "Normal"=2L, "High"=3L, "Total"=4L)
shifttx_map <- c("1"="Low","2"="Normal","3"="High","4"="Total")

# fixtot / denom
counts$BNRIND_F <- ifelse(is_miss_c(counts$BNRIND), "Total", counts$BNRIND)
counts$ANRIND_F <- ifelse(is_miss_c(counts$ANRIND), "Total", counts$ANRIND)
counts$BNRINDN <- unname(shiftord_map[counts$BNRIND_F])
counts$ANRINDN <- unname(shiftord_map[counts$ANRIND_F])
# input returns missing for unknown -> NA; keep NA (should not happen)

is_denom <- counts$BNRIND_F == "Total" & counts$ANRIND_F == "Total"
denom <- counts[is_denom, , drop=FALSE]
denom <- data.frame(PARAM=denom$PARAM, PARAMN=denom$PARAMN, TRTPN=denom$TRTPN, denom=denom$n, stringsAsFactors=FALSE)
fixtot <- counts[!is_denom, , drop=FALSE]
fixtot <- data.frame(PARAM=fixtot$PARAM, PARAMN=fixtot$PARAMN, TRTPN=fixtot$TRTPN,
                     ANRIND=fixtot$ANRIND_F, BNRIND=fixtot$BNRIND_F,
                     ANRINDN=fixtot$ANRINDN, BNRINDN=fixtot$BNRINDN, n=fixtot$n, stringsAsFactors=FALSE)
fixtot <- fixtot[order(fixtot$PARAM, fixtot$PARAMN, fixtot$TRTPN, fixtot$BNRINDN, fixtot$ANRINDN), , drop=FALSE]

# allcat
allcat <- unique(fixtot[, c("PARAM","PARAMN","TRTPN"), drop=FALSE])

# full grid
full_list <- list()
if (nrow(allcat) > 0) {
  for (i in seq_len(nrow(allcat))) {
    g <- expand.grid(BNRINDN=1:maxcat, ANRINDN=1:maxcat)
    g$PARAM <- allcat$PARAM[i]
    g$PARAMN <- allcat$PARAMN[i]
    g$TRTPN <- allcat$TRTPN[i]
    g$n <- 0
    full_list[[i]] <- g
  }
  full <- do.call(rbind, full_list)
} else {
  full <- data.frame(PARAM=character(0),PARAMN=numeric(0),TRTPN=numeric(0),BNRINDN=numeric(0),ANRINDN=numeric(0),n=numeric(0))
}
rownames(full) <- NULL

# allrecs : merge full(inf) fixtot
# left join full with fixtot on keys
fixtot_k <- fixtot[, c("PARAM","PARAMN","TRTPN","BNRINDN","ANRINDN","n"), drop=FALSE]
names(fixtot_k)[names(fixtot_k)=="n"] <- "n_fix"
merged <- merge(full, fixtot_k, by=c("PARAM","PARAMN","TRTPN","BNRINDN","ANRINDN"), all.x=TRUE, sort=FALSE)
merged$n <- ifelse(is.na(merged$n_fix), merged$n, merged$n_fix)
merged$n_fix <- NULL
allrecs <- merged

# alldenom : merge allrecs with denom, where denom not missing
alldenom <- merge(allrecs, denom, by=c("PARAM","PARAMN","TRTPN"), all.x=TRUE, sort=FALSE)
alldenom <- alldenom[!is.na(alldenom$denom), , drop=FALSE]
alldenom$BNRIND <- unname(shifttx_map[as.character(alldenom$BNRINDN)])
alldenom$ANRIND <- unname(shifttx_map[as.character(alldenom$ANRINDN)])
# sort for trans: by param paramn trtpn bnrindn (and anrindn)
alldenom <- alldenom[order(alldenom$PARAM, alldenom$PARAMN, alldenom$TRTPN, alldenom$BNRINDN, alldenom$ANRINDN), , drop=FALSE]

# pbcat1..pbcat4 : distinct anrind ordered by anrindn
pbord <- unique(alldenom[, c("ANRIND","ANRINDN"), drop=FALSE])
pbord <- pbord[order(pbord$ANRINDN), , drop=FALSE]
pbcat <- pbord$ANRIND
# ensure length maxcat (pad if needed)
while (length(pbcat) < maxcat) pbcat <- c(pbcat, NA_character_)
pbcat1 <- pbcat[1]; pbcat2 <- pbcat[2]; pbcat3 <- pbcat[3]; pbcat4 <- pbcat[4]

# trans : 1 record per baseline category, compute pct ---------------------
# group by PARAM PARAMN TRTPN BNRINDN
fmt_pct <- function(nv, denomv) {
  if (is.na(denomv) || denomv <= 0) return("(  0.0)")
  pv <- sround(100 * nv / denomv, 0.1)
  paste0("(", sprintf("%5.1f", pv), ")")
}
trans_rows <- list()
grp_keys <- unique(alldenom[, c("PARAM","PARAMN","TRTPN","BNRINDN","BNRIND","denom"), drop=FALSE])
grp_keys <- grp_keys[order(grp_keys$PARAMN, grp_keys$PARAM, grp_keys$TRTPN, grp_keys$BNRINDN), , drop=FALSE]
for (i in seq_len(nrow(grp_keys))) {
  k <- grp_keys[i, , drop=FALSE]
  sub <- alldenom[alldenom$PARAM==k$PARAM & alldenom$PARAMN==k$PARAMN & alldenom$TRTPN==k$TRTPN & alldenom$BNRINDN==k$BNRINDN, , drop=FALSE]
  ns <- rep(0, maxcat)
  pcts <- rep("(  0.0)", maxcat)
  for (r in seq_len(nrow(sub))) {
    a <- sub$ANRINDN[r]
    if (!is.na(a) && a>=1 && a<=maxcat) {
      ns[a] <- sub$n[r]
      pcts[a] <- fmt_pct(sub$n[r], k$denom)
    }
  }
  trt_label <- trt_map[[as.character(k$TRTPN)]]
  if (is.null(trt_label) || is.na(trt_label)) trt_label <- as.character(k$TRTPN)
  trttx <- paste0(trimws(trt_label), " (N = ", as.character(as.integer(k$denom)), ")")
  trans_rows[[i]] <- data.frame(
    PARAM=k$PARAM, PARAMN=k$PARAMN, TRTPN=k$TRTPN,
    BNRINDN=k$BNRINDN, BNRIND=k$BNRIND, denom=k$denom, TRTTX=trttx,
    n1=ns[1], pct1=pcts[1], n2=ns[2], pct2=pcts[2], n3=ns[3], pct3=pcts[3], n4=ns[4], pct4=pcts[4],
    stringsAsFactors=FALSE
  )
}
if (length(trans_rows)>0) trans <- do.call(rbind, trans_rows) else trans <- data.frame()
trans <- trans[order(trans$PARAMN, trans$PARAM, trans$TRTPN, trans$BNRINDN), , drop=FALSE]
rownames(trans) <- NULL

# Output table rendering --------------------------------------------------
# CSV of table body
csv_out <- trans
# order columns similar to report
csv_out <- csv_out[, c("PARAM","PARAMN","TRTPN","TRTTX","BNRINDN","BNRIND","denom","n1","pct1","n2","pct2","n3","pct3","n4","pct4")]
write.csv(csv_out, file.path(out, "P054.csv"), row.names=FALSE, na="")

# text rendering mimicking PROC REPORT with BY paramn param
con_file <- file.path(out, "P054.txt")
sink(con_file)
cat("Shift Table: Diastolic Blood Pressure (DIABP) - Baseline vs Post-Baseline\n")
cat("\n")
# header line for post-baseline cats
# Use pbcat values
cat(sprintf("Post-Baseline categories: 1=%s 2=%s 3=%s 4=%s\n", pbcat1, pbcat2, pbcat3, pbcat4))
cat("\n")
# BY groups
bygrps <- unique(trans[, c("PARAMN","PARAM"), drop=FALSE])
bygrps <- bygrps[order(bygrps$PARAMN, bygrps$PARAM), , drop=FALSE]
for (b in seq_len(nrow(bygrps))) {
  bp <- bygrps[b,]
  cat(sprintf("PARAMN=%s PARAM=%s\n", as.character(bp$PARAMN), as.character(bp$PARAM)))
  cat("------------------------------------------------------------------------------------------------------------------------\n")
  # column headers
  # Two-line header: Treatment | Baseline Result | Post-Baseline Result -> sub Low Normal High Total each n %
  cat(sprintf("%-28s | %-10s | %s\n", "Treatment", "Baseline", "Post-Baseline Result"))
  cat(sprintf("%-28s | %-10s | %-14s %-14s %-14s %-14s\n", "Result", "Result",
              pbcat1, pbcat2, pbcat3, pbcat4))
  cat(sprintf("%-28s | %-10s | %-6s %-7s %-6s %-7s %-6s %-7s %-6s %-7s\n",
              "", "", "n", "%", "n", "%", "n", "%", "n", "%"))
  cat("------------------------------------------------------------------------------------------------------------------------\n")
  sub <- trans[trans$PARAMN==bp$PARAMN & trans$PARAM==bp$PARAM, , drop=FALSE]
  sub <- sub[order(sub$TRTPN, sub$BNRINDN), , drop=FALSE]
  prev_trttx <- ""
  for (r in seq_len(nrow(sub))) {
    row <- sub[r,]
    # PROC REPORT ID ORDER blanks repeats; we repeat for clarity but keep numbers identical
    # show trttx only on first row of treatment group to mimic BREAK AFTER
    show_trt <- row$TRTTX
    line <- sprintf("%-28s | %-10s | %6d %-7s %6d %-7s %6d %-7s %6d %-7s",
                    show_trt, row$BNRIND,
                    as.integer(row$n1), as.character(row$pct1),
                    as.integer(row$n2), as.character(row$pct2),
                    as.integer(row$n3), as.character(row$pct3),
                    as.integer(row$n4), as.character(row$pct4))
    cat(line, "\n")
    # break after trttx / skip : blank line after last baseline row of each treatment
    nxt <- if (r < nrow(sub)) sub$TRTPN[r+1] else NA
    if (is.na(nxt) || nxt != row$TRTPN) cat("\n")
  }
  cat("\n")
}
sink()

# also write .lst copy for benchmark expecting P054.lst
lst_file <- file.path(out, "P054.lst")
invisible(file.copy(con_file, lst_file, overwrite=TRUE))

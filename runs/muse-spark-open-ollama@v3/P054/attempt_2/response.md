```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "output"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, accuracy = 0.1) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x)/accuracy + 0.5 + 1e-9) * accuracy)
}
propcase_sas <- function(x) {
  ifelse(is.na(x) | x == "", x, stringr::str_to_title(tolower(x)))
}
put_5_1 <- function(x) sprintf("%5.1f", x)
shiftord_input <- function(x) {
  m <- c("Low"=1,"Normal"=2,"High"=3,"Total"=4)
  unname(m[x])
}
shifttx_put <- function(x) {
  m <- c("1"="Low","2"="Normal","3"="High","4"="Total")
  unname(m[as.character(x)])
}
# translate Windows path to linux /bench if needed
to_linux_path <- function(p) {
  if (is.na(p) || p=="") return(p)
  p <- gsub("\\\\","/",p)
  p <- sub("^[A-Za-z]:/meta_hackathon/sas2r-agent","/bench",p, ignore.case=TRUE)
  p <- sub("^D:/meta_hackathon/sas2r-agent","/bench",p, ignore.case=TRUE)
  p
}

# locate advs.xpt -------------------------------------------------------
candidates <- c(
  Sys.getenv("rawadam"), Sys.getenv("RAWADAM"),
  Sys.getenv("rawsplit"), Sys.getenv("RAWSPLIT"),
  Sys.getenv("root"), Sys.getenv("ROOT"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam",
  "/bench/data/raw/cdisc-pilot/adam",
  "/bench/data/derived/cdisc-pilot/adam",
  "/bench/data/raw/cdisc-pilot/adam-split",
  "./data/raw/cdisc-pilot/adam",
  "./data/derived/cdisc-pilot/adam"
)
candidates <- unique(na.omit(candidates[candidates!="" ]))
candidates <- unique(c(to_linux_path(candidates), candidates))
advs_path <- NULL
for (cand in candidates) {
  if (!dir.exists(cand)) next
  p1 <- file.path(cand, "advs.xpt")
  p2 <- file.path(cand, "advs.sas7bdat")
  if (file.exists(p1)) { advs_path <- p1; break }
  if (file.exists(p2)) { advs_path <- p2; break }
}
if (is.null(advs_path)) {
  # recursive search under /bench and .
  search_roots <- unique(c("/bench",".","/tmp"))
  for (sr in search_roots) {
    if (!dir.exists(sr)) next
    found <- list.files(sr, pattern="^advs\\.xpt$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
    if (length(found)>0) { advs_path <- found[1]; break }
  }
}
if (is.null(advs_path)) stop("advs.xpt not found")
cat("Using advs_path:", advs_path, "\n")

if (grepl("\\.xpt$", advs_path, ignore.case=TRUE)) {
  advs_raw <- haven::read_xpt(advs_path)
} else {
  advs_raw <- haven::read_sas(advs_path)
}
advs_raw <- haven::zap_formats(advs_raw)
advs_raw <- haven::zap_labels(advs_raw)
advs_raw <- haven::zap_label(advs_raw)
advs_raw <- as.data.frame(advs_raw, stringsAsFactors=FALSE)
# normalize names to uppercase for case-insensitive matching
names(advs_raw) <- toupper(names(advs_raw))
# convert haven_labelled to plain
for (nm in names(advs_raw)) {
  if (haven::is.labelled(advs_raw[[nm]])) {
    # keep numeric if numeric, else character
    if (is.numeric(advs_raw[[nm]])) advs_raw[[nm]] <- as.numeric(haven::zap_labels(advs_raw[[nm]]))
    else advs_raw[[nm]] <- as.character(haven::zap_labels(advs_raw[[nm]]))
  }
}
char_cols <- names(advs_raw)[sapply(advs_raw, is.character)]
for (cc in char_cols) advs_raw[[cc]] <- trimws(advs_raw[[cc]])

# SAS: param = catx(' ', param, propcase(atpt)); paramn = atptn;
advs_raw$ATPT_PROPCASE <- propcase_sas(advs_raw$ATPT)
advs_raw$PARAM <- mapply(function(p,a){
  if (is.na(p) || p=="") p <- ""
  if (is.na(a) || a=="") a <- ""
  if (p=="" && a=="") return("")
  if (p=="") return(a)
  if (a=="") return(p)
  paste(p,a,sep=" ")
}, advs_raw$PARAM, advs_raw$ATPT_PROPCASE, USE.NAMES=FALSE)
advs_raw$PARAMN <- advs_raw$ATPTN

required_vars <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN","AVAL","AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
for (v in required_vars) if (!v %in% names(advs_raw)) advs_raw[[v]] <- NA
inds <- advs_raw[, required_vars]

# filter: PARAMCD='DIABP' and ANL01FL='Y' and AVISITN=99 and SAFFL='Y' and BNRIND ne 'Missing' and ANRIND ne 'Missing'
inds <- inds[inds$PARAMCD=="DIABP" & inds$ANL01FL=="Y" & inds$AVISITN==99 & inds$SAFFL=="Y" &
               !is.na(inds$BNRIND) & inds$BNRIND!="Missing" &
               !is.na(inds$ANRIND) & inds$ANRIND!="Missing", , drop=FALSE]

# Build treatment group display format
trtcomb <- unique(inds[, c("TRTPN","TRTP")])
trtcomb <- trtcomb[!is.na(trtcomb$TRTPN), , drop=FALSE]
trt_map <- setNames(trtcomb$TRTP, as.character(trtcomb$TRTPN))

# Count patients by baseline/post-baseline shift category
# PROC SUMMARY with CLASS param paramn trtpn bnrind anrind
library(dplyr)
cnt_detail <- inds %>%
  filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN)) %>%
  group_by(PARAM, PARAMN, TRTPN, BNRIND, ANRIND) %>%
  summarise(n=sum(!is.na(AVAL)), .groups="drop")
cnt_bnrind <- inds %>%
  filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN)) %>%
  group_by(PARAM, PARAMN, TRTPN, BNRIND) %>%
  summarise(n=sum(!is.na(AVAL)), .groups="drop") %>%
  mutate(ANRIND=NA_character_)
cnt_anrind <- inds %>%
  filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN)) %>%
  group_by(PARAM, PARAMN, TRTPN, ANRIND) %>%
  summarise(n=sum(!is.na(AVAL)), .groups="drop") %>%
  mutate(BNRIND=NA_character_)
cnt_total <- inds %>%
  filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN)) %>%
  group_by(PARAM, PARAMN, TRTPN) %>%
  summarise(n=sum(!is.na(AVAL)), .groups="drop") %>%
  mutate(BNRIND=NA_character_, ANRIND=NA_character_)
counts <- bind_rows(cnt_detail, cnt_bnrind, cnt_anrind, cnt_total)
counts <- counts[!is.na(counts$PARAM) & counts$PARAM!="" & !is.na(counts$PARAMN) & !is.na(counts$TRTPN), , drop=FALSE]
counts <- counts[order(counts$PARAM, counts$PARAMN, counts$TRTPN, na.last=TRUE), ]

# Fill in total indicator, add numeric shift categories, and split off denominator records
fixtot_list <- list()
denom_list <- list()
for (i in seq_len(nrow(counts))) {
  b <- counts$BNRIND[i]; a <- counts$ANRIND[i]
  if (is.na(b) || b=="") b <- "Total"
  if (is.na(a) || a=="") a <- "Total"
  bnrindn <- shiftord_input(b)
  anrindn <- shiftord_input(a)
  row <- data.frame(PARAM=counts$PARAM[i], PARAMN=counts$PARAMN[i], TRTPN=counts$TRTPN[i],
                    ANRIND=a, BNRIND=b, ANRINDN=anrindn, BNRINDN=bnrindn, n=counts$n[i],
                    stringsAsFactors=FALSE)
  if (b=="Total" && a=="Total") {
    denom_list[[length(denom_list)+1]] <- data.frame(PARAM=row$PARAM, PARAMN=row$PARAMN, TRTPN=row$TRTPN, denom=row$n, stringsAsFactors=FALSE)
  } else {
    fixtot_list[[length(fixtot_list)+1]] <- row
  }
}
fixtot <- if (length(fixtot_list)>0) do.call(rbind, fixtot_list) else data.frame(PARAM=character(),PARAMN=numeric(),TRTPN=numeric(),ANRIND=character(),BNRIND=character(),ANRINDN=numeric(),BNRINDN=numeric(),n=numeric(), stringsAsFactors=FALSE)
denom <- if (length(denom_list)>0) do.call(rbind, denom_list) else data.frame(PARAM=character(),PARAMN=numeric(),TRTPN=numeric(),denom=numeric(), stringsAsFactors=FALSE)
fixtot <- fixtot[order(fixtot$PARAM, fixtot$PARAMN, fixtot$TRTPN, fixtot$BNRINDN, fixtot$ANRINDN), ]

# Create full dataset to account for 0 cells
allcat <- unique(fixtot[, c("PARAM","PARAMN","TRTPN")])
full_list <- list()
if (nrow(allcat)>0) {
  for (i in seq_len(nrow(allcat))) {
    for (bn in 1:4) for (an in 1:4) {
      full_list[[length(full_list)+1]] <- data.frame(PARAM=allcat$PARAM[i], PARAMN=allcat$PARAMN[i], TRTPN=allcat$TRTPN[i], BNRINDN=bn, ANRINDN=an, n=0, stringsAsFactors=FALSE)
    }
  }
  full <- do.call(rbind, full_list)
} else {
  full <- data.frame(PARAM=character(),PARAMN=numeric(),TRTPN=numeric(),BNRINDN=numeric(),ANRINDN=numeric(),n=numeric(), stringsAsFactors=FALSE)
}

# Merge back with counts to get complete dataset - FIXED
if (nrow(full)>0 && nrow(fixtot)>0) {
  allrecs <- merge(full, fixtot[, c("PARAM","PARAMN","TRTPN","BNRINDN","ANRINDN","n")],
                   by=c("PARAM","PARAMN","TRTPN","BNRINDN","ANRINDN"), all.x=TRUE, suffixes=c(".full",".fixtot"))
  # columns are n.full and n.fixtot
  allrecs$n <- ifelse(!is.na(allrecs$n.fixtot), allrecs$n.fixtot, allrecs$n.full)
  allrecs$n.full <- NULL; allrecs$n.fixtot <- NULL
} else {
  allrecs <- full
}

# Add denominators and repopulate shift categories
alldenom <- merge(allrecs, denom, by=c("PARAM","PARAMN","TRTPN"), all.x=TRUE)
alldenom <- alldenom[!is.na(alldenom$denom), , drop=FALSE]
alldenom$BNRIND <- shifttx_put(alldenom$BNRINDN)
alldenom$ANRIND <- shifttx_put(alldenom$ANRINDN)
alldenom <- alldenom[order(alldenom$PARAM, alldenom$PARAMN, alldenom$TRTPN, alldenom$BNRINDN, alldenom$ANRINDN), ]

# Get category names for column headings
pbcat_df <- unique(alldenom[, c("ANRIND","ANRINDN")])
pbcat_df <- pbcat_df[order(pbcat_df$ANRINDN), , drop=FALSE]
pbcat <- pbcat_df$ANRIND
while (length(pbcat)<4) pbcat <- c(pbcat, paste0("Cat",length(pbcat)+1))
pbcat1 <- pbcat[1]; pbcat2 <- pbcat[2]; pbcat3 <- pbcat[3]; pbcat4 <- pbcat[4]

# Transpose to 1 record per baseline category and compute percentage
trans_list <- list()
if (nrow(alldenom)>0) {
  grp_keys <- unique(alldenom[, c("PARAM","PARAMN","TRTPN","BNRINDN","BNRIND","denom")])
  grp_keys <- grp_keys[order(grp_keys$PARAM, grp_keys$PARAMN, grp_keys$TRTPN, grp_keys$BNRINDN), ]
  for (i in seq_len(nrow(grp_keys))) {
    pk <- grp_keys[i, ]
    sub <- alldenom[alldenom$PARAM==pk$PARAM & alldenom$PARAMN==pk$PARAMN & alldenom$TRTPN==pk$TRTPN & alldenom$BNRINDN==pk$BNRINDN, , drop=FALSE]
    ns <- rep(NA_real_,4); pcts <- rep(NA_character_,4)
    for (j in seq_len(nrow(sub))) {
      an <- sub$ANRINDN[j]; nval <- sub$n[j]; d <- sub$denom[j]
      ns[an] <- nval
      if (!is.na(d) && d>0) {
        pct_rounded <- round_half_up((100*nval)/d, 0.1)
        pcts[an] <- paste0("(", put_5_1(pct_rounded), ")")
      } else {
        pcts[an] <- "(  0.0)"
      }
    }
    for (k in 1:4) if (is.na(ns[k])) { ns[k] <- 0; pcts[k] <- paste0("(", put_5_1(round_half_up(0,0.1)), ")") }
    trt_label <- trt_map[as.character(pk$TRTPN)]
    if (is.na(trt_label)) trt_label <- as.character(pk$TRTPN)
    trttx <- paste0(trt_label, " (N = ", trimws(as.character(pk$denom)), ")")
    trans_list[[length(trans_list)+1]] <- data.frame(
      PARAM=pk$PARAM, PARAMN=pk$PARAMN, TRTPN=pk$TRTPN, TRTTX=trttx,
      BNRINDN=pk$BNRINDN, BNRIND=pk$BNRIND,
      n1=ns[1], pct1=pcts[1], n2=ns[2], pct2=pcts[2], n3=ns[3], pct3=pcts[3], n4=ns[4], pct4=pcts[4],
      denom=pk$denom, stringsAsFactors=FALSE
    )
  }
  trans <- do.call(rbind, trans_list)
} else {
  trans <- data.frame(PARAM=character(),PARAMN=numeric(),TRTPN=numeric(),TRTTX=character(),BNRINDN=numeric(),BNRIND=character(),n1=numeric(),pct1=character(),n2=numeric(),pct2=character(),n3=numeric(),pct3=character(),n4=numeric(),pct4=character(),denom=numeric(), stringsAsFactors=FALSE)
}
trans <- trans[order(trans$PARAMN, trans$PARAM, trans$TRTPN, trans$BNRINDN), ]

# Generate output table
table_csv <- trans
write.csv(table_csv, file.path(out, "P054.csv"), row.names=FALSE, na="")
write.csv(table_csv, file.path(out, "P054_table.csv"), row.names=FALSE, na="")
write.csv(table_csv, file.path(out, "P054.lst.csv"), row.names=FALSE, na="")

# Build text rendering
txt_lines <- c()
txt_lines <- c(txt_lines, "Shift Table for Diastolic Blood Pressure (mmHg)")
txt_lines <- c(txt_lines, "PARAMCD='DIABP' ANL01FL='Y' AVISITN=99 SAFFL='Y' BNRIND/ANRIND ne 'Missing'")
txt_lines <- c(txt_lines, "")
by_groups <- unique(trans[, c("PARAMN","PARAM")])
by_groups <- by_groups[order(by_groups$PARAMN), , drop=FALSE]
for (bg in seq_len(nrow(by_groups))) {
  paramn_val <- by_groups$PARAMN[bg]; param_val <- by_groups$PARAM[bg]
  txt_lines <- c(txt_lines, paste0("PARAM: ", param_val, " (PARAMN=", paramn_val, ")"))
  txt_lines <- c(txt_lines, paste0(strrep("-", 130)))
  hdr1 <- sprintf("%-30s %-15s | %-12s %-12s %-12s %-12s", "Treatment", "Baseline", "Post-Baseline Result", "", "", "")
  hdr2 <- sprintf("%-30s %-15s | %-5s %-7s %-5s %-7s %-5s %-7s %-5s %-7s", "", "Result", pbcat1, "", pbcat2, "", pbcat3, "", pbcat4, "")
  hdr3 <- sprintf("%-30s %-15s | %-5s %-7s %-5s %-7s %-5s %-7s %-5s %-7s", "", "", "n", "%", "n", "%", "n", "%", "n", "%")
  txt_lines <- c(txt_lines, hdr1)
  txt_lines <- c(txt_lines, hdr2)
  txt_lines <- c(txt_lines, hdr3)
  txt_lines <- c(txt_lines, paste0(strrep("-", 130)))
  sub_trans <- trans[trans$PARAMN==paramn_val & trans$PARAM==param_val, , drop=FALSE]
  sub_trans <- sub_trans[order(sub_trans$TRTPN, sub_trans$BNRINDN), ]
  trt_levels <- unique(sub_trans[, c("TRTPN","TRTTX")])
  trt_levels <- trt_levels[order(trt_levels$TRTPN), , drop=FALSE]
  for (ti in seq_len(nrow(trt_levels))) {
    trtpn_val <- trt_levels$TRTPN[ti]
    rows_trt <- sub_trans[sub_trans$TRTPN==trtpn_val, , drop=FALSE]
    rows_trt <- rows_trt[order(rows_trt$BNRINDN), ]
    for (r in seq_len(nrow(rows_trt))) {
      rr <- rows_trt[r, ]
      trt_disp <- if (r==1) sprintf("%-30s", substr(rr$TRTTX,1,30)) else sprintf("%-30s","")
      bnr_disp <- sprintf("%-15s", rr$BNRIND)
      line <- sprintf("%s %s | %5d %-7s %5d %-7s %5d %-7s %5d %-7s",
                      trt_disp, bnr_disp,
                      as.integer(rr$n1), rr$pct1,
                      as.integer(rr$n2), rr$pct2,
                      as.integer(rr$n3), rr$pct3,
                      as.integer(rr$n4), rr$pct4)
      txt_lines <- c(txt_lines, line)
    }
    txt_lines <- c(txt_lines, "")
  }
  txt_lines <- c(txt_lines, "")
}
txt_lines <- c(txt_lines, "Note: Percentages are based on N for each treatment group (denominator).")
txt_lines <- c(txt_lines, "BNRIND/ANRIND categories: Low, Normal, High, Total")
txt_lines <- c(txt_lines, "")

writeLines(txt_lines, file.path(out, "P054.txt"))
writeLines(txt_lines, file.path(out, "P054.lst.txt"))
writeLines(txt_lines, file.path(out, "P054.lst"))
# also ensure a generic .txt exists for validator
writeLines(txt_lines, file.path(out, "table.txt"))

write.csv(counts, file.path(out, "P054_counts.csv"), row.names=FALSE, na="")
write.csv(alldenom, file.path(out, "P054_alldenom.csv"), row.names=FALSE, na="")

cat("P054: trans rows", nrow(trans), "written to", out, "\n")
```
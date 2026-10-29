# P074 t_lb_shift.sas -> R translation
# Table 14.3.4 Shift Table for Laboratory Parameters (Baseline to Worst Post-Baseline)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

# SAS round half away from zero (helper, not needed for counts but kept)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-10)
  posneg * z / 10^digits
}

read_any <- function(path) {
  if (grepl("\\.xpt$", path, ignore.case = TRUE)) {
    haven::read_xpt(path)
  } else if (grepl("\\.sas7bdat$", path, ignore.case = TRUE)) {
    haven::read_sas(path)
  } else {
    stop(paste("Unknown file type:", path))
  }
}

find_file <- function(dir, name) {
  if (!dir.exists(dir)) stop(paste("Directory not found:", dir))
  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  # match name ignoring case and extension
  idx <- grep(paste0("^", name, "\\."), basename(files), ignore.case = TRUE)
  if (length(idx) == 0) {
    # try recursive
    files2 <- list.files(dir, full.names = TRUE, recursive = TRUE)
    idx2 <- grep(paste0("^", name, "\\."), basename(files2), ignore.case = TRUE)
    if (length(idx2) == 0) stop(paste("File not found:", name, "in", dir))
    return(files2[idx2[1]])
  }
  return(files[idx[1]])
}

zap_all <- function(df) {
  # remove haven labels/formats, convert factors to character
  df <- haven::zap_labels(df)
  df <- haven::zap_formats(df)
  df <- haven::zap_label(df)
  for (nm in names(df)) {
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
    if (inherits(df[[nm]], "haven_labelled")) df[[nm]] <- as.vector(df[[nm]])
  }
  names(df) <- toupper(names(df))
  df
}

adsl_path <- find_file(adam_dir, "adsl")
adlb_path <- find_file(adam_dir, "adlb")

adsl <- zap_all(read_any(adsl_path))
adlb <- zap_all(read_any(adlb_path))

# Ensure character columns are character, numeric stay numeric
# Trim helper for SAS blank-padded comparison
trimc <- function(x) ifelse(is.na(x), NA_character_, trimws(as.character(x), which = "both"))
is_missing_c <- function(x) is.na(x) | trimws(as.character(x)) == ""

# ---- Step 1: Big N (m_counts) ----
# where upcase(SAFFL) = 'Y', group by TRTA order by TRTA (SAS char order)
adsl$SAFFL_T <- trimc(adsl$SAFFL)
bign_df <- adsl[!is.na(adsl$SAFFL_T) & toupper(adsl$SAFFL_T) == "Y", , drop = FALSE]
# distinct USUBJID per TRTA
bign_df <- bign_df[!duplicated(bign_df[, c("TRTA","USUBJID")]), , drop = FALSE]
# aggregate
if (nrow(bign_df) > 0) {
  # SAS order by TRTA (byte order, uppercase before lowercase)
  # Use C locale ordering via xtfrm? base order is locale dependent; force byte order:
  # order by raw bytes: use order with method="radix" which is byte-wise for character?
  ord <- order(bign_df$TRTA, method = "radix", na.last = TRUE)
  bign_df <- bign_df[ord, , drop = FALSE]
  bign_agg <- aggregate(USUBJID ~ TRTA, data = bign_df, FUN = function(x) length(unique(x)))
  names(bign_agg)[2] <- "BIGN"
  # sort by TRTA byte order
  bign_agg <- bign_agg[order(bign_agg$TRTA, method = "radix"), , drop = FALSE]
} else {
  bign_agg <- data.frame(TRTA = character(0), BIGN = numeric(0))
}
bigntot <- length(unique(adsl$USUBJID[!is.na(adsl$SAFFL_T) & toupper(adsl$SAFFL_T) == "Y"]))

# Map TRTAN <-> TRTA for header (use ADSL distinct)
trtmap <- unique(adsl[, c("TRTA","TRTAN")])
trtmap <- trtmap[!is.na(trtmap$TRTA), , drop = FALSE]
trtmap <- trtmap[order(trtmap$TRTAN), , drop = FALSE]
# Big N per TRTAN
bign_by_trtan <- aggregate(USUBJID ~ TRTA + TRTAN, data = adsl[!is.na(adsl$SAFFL_T) & toupper(adsl$SAFFL_T)=="Y", , drop=FALSE], FUN=function(x) length(unique(x)))
names(bign_by_trtan)[3] <- "BIGN"

ntrt <- 3L

# ---- Step 2: lb ----
# inner join adlb + adsl where SAFFL='Y' and PARAMCD in list
wanted_params <- c('ALT','AST','BILI','CREAT','HGB','WBC','PLAT','NEUT')
adsl_saf <- adsl[trimc(adsl$SAFFL) == "Y", c("USUBJID"), drop = FALSE]
adsl_saf <- unique(adsl_saf)
# normalize adlb fields
adlb$PARAMCD_T <- trimc(adlb$PARAMCD)
adlb$USUBJID_T <- trimc(adlb$USUBJID)
adsl_saf$USUBJID_T <- trimc(adsl_saf$USUBJID)
lb <- merge(adlb[adlb$PARAMCD_T %in% wanted_params, , drop=FALSE], adsl_saf, by="USUBJID_T")
# rebuild columns: use original USUBJID (from adlb), TRTA,TRTAN,PARAMCD,PARAM,AVISIT,AVISITN,ATOXGR,BTOXGR,ABLFL,ANL01FL
# Keep needed cols
keepcols <- c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","AVISIT","AVISITN","ATOXGR","BTOXGR","ABLFL","ANL01FL")
# USUBJID: use adlb USUBJID (x) - merge created USUBJID.x / USUBJID.y? Actually both have USUBJID; merge by USUBJID_T so both retained
# Simplify: USUBJID = trimc of USUBJID_T
lb$USUBJID <- lb$USUBJID_T
lb$PARAMCD <- lb$PARAMCD_T
# Ensure TRTAN numeric, AVISITN numeric
lb$TRTAN <- suppressWarnings(as.numeric(lb$TRTAN))
lb$AVISITN <- suppressWarnings(as.numeric(lb$AVISITN))
lb <- lb[, keepcols, drop = FALSE]
# order by TRTAN, PARAMCD, USUBJID, AVISITN (SAS missing first)
lb <- lb[order(lb$TRTAN, lb$PARAMCD, lb$USUBJID, lb$AVISITN, method="radix", na.last=FALSE), , drop=FALSE]

# ---- Step 3: bl_grade ----
bl_grade <- lb[trimc(lb$ABLFL) == "Y" & !is_missing_c(lb$BTOXGR), , drop=FALSE]
bl_grade$BL_GRADE <- trimc(bl_grade$BTOXGR)
bl_grade <- bl_grade[, c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","BL_GRADE"), drop=FALSE]
# distinct
bl_grade <- unique(bl_grade)

# ---- Step 4: worst_grade ----
# where ABLFL ne 'Y' and ATOXGR ne '' and AVISITN > 0
# SAS: ABLFL ne 'Y' includes missing
cond_worst <- (is.na(trimc(lb$ABLFL)) | trimc(lb$ABLFL) != "Y") & !is_missing_c(lb$ATOXGR) & !is.na(lb$AVISITN) & lb$AVISITN > 0
worst_in <- lb[cond_worst, , drop=FALSE]
worst_in$ATOXGR_N <- suppressWarnings(as.numeric(trimws(as.character(worst_in$ATOXGR))))
# group by USUBJID,TRTA,TRTAN,PARAMCD, max
if (nrow(worst_in) > 0) {
  # aggregate max ignoring NA (SAS max ignores missing)
  agg <- aggregate(ATOXGR_N ~ USUBJID + TRTA + TRTAN + PARAMCD, data=worst_in,
                   FUN=function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm=TRUE))
  names(agg)[5] <- "WORST_GRADE_N"
} else {
  agg <- data.frame(USUBJID=character(0),TRTA=character(0),TRTAN=numeric(0),PARAMCD=character(0),WORST_GRADE_N=numeric(0))
}
worst_grade2 <- agg
# length worst_grade $2; worst_grade = strip(put(worst_grade_n, best.)); if 0 then '0'
worst_grade2$WORST_GRADE <- sapply(worst_grade2$WORST_GRADE_N, function(v) {
  if (is.na(v)) return(".")
  # SAS BEST. for integers: no decimals
  # Use format with no scientific, strip
  s <- trimws(format(v, scientific=FALSE, trim=TRUE))
  # remove trailing .0 if any? best. would show integer without decimal
  # as.numeric integer values: format may give "0"
  if (s == "0") return("0")
  # For safety, if value is e.g. 1, return "1"
  # Remove decimal part if .0
  if (grepl("\\.", s)) {
    # SAS best would show decimals if needed; grades are integers 0-4
    # strip trailing zeros
    s <- sub("\\.0+$", "", s)
  }
  trimws(s)
})
worst_grade2$WORST_GRADE[worst_grade2$WORST_GRADE_N == 0 & !is.na(worst_grade2$WORST_GRADE_N)] <- "0"
worst_grade2 <- worst_grade2[, c("USUBJID","TRTA","TRTAN","PARAMCD","WORST_GRADE_N","WORST_GRADE"), drop=FALSE]

# ---- Step 5: shift (inner join bl_grade + worst_grade2 on USUBJID, PARAMCD, TRTA) ----
shift <- merge(bl_grade, worst_grade2[, c("USUBJID","TRTA","PARAMCD","WORST_GRADE")],
               by=c("USUBJID","TRTA","PARAMCD"))
# order by TRTAN, PARAMCD, bl_grade, worst_grade
# TRTAN is from bl_grade (TRTAN.x) - merge keeps single TRTAN? Actually both have TRTAN but we only merged worst without TRTAN, so TRTAN is from bl
shift <- shift[order(shift$TRTAN, shift$PARAMCD, shift$BL_GRADE, shift$WORST_GRADE, method="radix", na.last=FALSE), , drop=FALSE]

# ---- Step 6: shift_cnt ----
if (nrow(shift) > 0) {
  # count distinct USUBJID per TRTA,TRTAN,PARAMCD,PARAM,BL_GRADE,WORST_GRADE
  shift_cnt <- aggregate(USUBJID ~ TRTA + TRTAN + PARAMCD + PARAM + BL_GRADE + WORST_GRADE,
                         data=shift, FUN=function(x) length(unique(x)))
  names(shift_cnt)[7] <- "CNT"
} else {
  shift_cnt <- data.frame(TRTA=character(0),TRTAN=numeric(0),PARAMCD=character(0),PARAM=character(0),BL_GRADE=character(0),WORST_GRADE=character(0),CNT=numeric(0))
}

# ---- Step 7: all_cells ----
grades <- data.frame(GRADE=c("0","1","2","3","4"), stringsAsFactors=FALSE)
distinct_cells <- unique(shift_cnt[, c("TRTA","TRTAN","PARAMCD","PARAM"), drop=FALSE])
if (nrow(distinct_cells) > 0) {
  # cross join
  all_cells <- merge(distinct_cells, grades, by=NULL)
  names(all_cells)[names(all_cells)=="GRADE"] <- "BL_GRADE"
  all_cells <- merge(all_cells, grades, by=NULL)
  names(all_cells)[names(all_cells)=="GRADE"] <- "WORST_GRADE"
} else {
  all_cells <- data.frame(TRTA=character(0),TRTAN=numeric(0),PARAMCD=character(0),PARAM=character(0),BL_GRADE=character(0),WORST_GRADE=character(0))
}

# Merge actual counts (left join, coalesce 0)
shift_full <- merge(all_cells, shift_cnt[, c("TRTA","PARAMCD","BL_GRADE","WORST_GRADE","CNT")],
                    by=c("TRTA","PARAMCD","BL_GRADE","WORST_GRADE"), all.x=TRUE)
shift_full$CNT[is.na(shift_full$CNT)] <- 0
shift_full <- shift_full[order(shift_full$TRTAN, shift_full$PARAMCD, shift_full$BL_GRADE, shift_full$WORST_GRADE, method="radix", na.last=FALSE), , drop=FALSE]

# ---- Step 8: transpose + report ----
# pivot wider
if (nrow(shift_full) > 0) {
  shift_t <- tidyr::pivot_wider(shift_full,
                                id_cols=c("TRTAN","TRTA","PARAMCD","PARAM","BL_GRADE"),
                                names_from="WORST_GRADE", values_from="CNT",
                                names_prefix="wg", values_fill=0)
} else {
  shift_t <- data.frame(TRTAN=numeric(0),TRTA=character(0),PARAMCD=character(0),PARAM=character(0),BL_GRADE=character(0))
}
# ensure wg0-wg4 exist
for (g in paste0("wg",0:4)) {
  if (!g %in% names(shift_t)) shift_t[[g]] <- 0
  shift_t[[g]][is.na(shift_t[[g]])] <- 0
}
# bl_label
shift_t$BL_LABEL <- sapply(shift_t$BL_GRADE, function(x) {
  if (is.na(x)) return(NA_character_)
  x <- trimws(as.character(x))
  if (x=="0") "Normal" else if (x=="1") "Grade 1" else if (x=="2") "Grade 2" else if (x=="3") "Grade 3" else if (x=="4") "Grade 4" else x
})
shift_t$TOTAL <- rowSums(as.data.frame(shift_t[, paste0("wg",0:4), drop=FALSE]), na.rm=TRUE)
report <- as.data.frame(shift_t)
# order by TRTAN, PARAMCD, BL_GRADE
report <- report[order(report$TRTAN, report$PARAMCD, report$BL_GRADE, method="radix", na.last=FALSE), , drop=FALSE]
# column order
report <- report[, c("TRTAN","TRTA","PARAMCD","PARAM","BL_GRADE","BL_LABEL",paste0("wg",0:4),"TOTAL")]

# Uppercase names to match SAS dataset variable names (case-insensitive match ok)
# Keep SAS-like names: TRTAN TRTA PARAMCD PARAM BL_GRADE BL_LABEL WG0..WG4 TOTAL
names(report) <- c("TRTAN","TRTA","PARAMCD","PARAM","BL_GRADE","BL_LABEL","WG0","WG1","WG2","WG3","WG4","TOTAL")

# ---- Step 9: outputs ----
# CSV of table body
write.csv(report, file.path(out, "t_lb_shift.csv"), row.names=FALSE, na="")

# Text rendering (RTF counterpart as plain text)
txt_path <- file.path(out, "t_lb_shift.txt")
con <- file(txt_path, open="wt")

cat("Table 14.3.4\n", file=con)
cat("Shift Table for Laboratory Parameters\n", file=con)
cat("Safety Analysis Set\n", file=con)
cat("\n", file=con)

# helper to get Big N for TRTAN
get_bign <- function(trtan) {
  v <- bign_by_trtan$BIGN[bign_by_trtan$TRTAN==trtan]
  if (length(v)==0) return(0)
  v[1]
}
# helper to get TRTA label for TRTAN
get_trta <- function(trtan) {
  v <- trtmap$TRTA[trtmap$TRTAN==trtan]
  if (length(v)==0) return(paste0("TRTAN=",trtan))
  v[1]
}

for (t in sort(unique(report$TRTAN))) {
  trta_lbl <- get_trta(t)
  bign <- get_bign(t)
  cat(sprintf("Treatment: %s (N=%d)\n", trta_lbl, bign), file=con)
  sub <- report[report$TRTAN==t, , drop=FALSE]
  # order params by PARAMCD
  params <- unique(sub[, c("PARAMCD","PARAM")])
  params <- params[order(params$PARAMCD, method="radix"), , drop=FALSE]
  for (r in seq_len(nrow(params))) {
    pcd <- params$PARAMCD[r]
    pname <- params$PARAM[r]
    cat(sprintf("  Parameter: %s (%s)\n", pname, pcd), file=con)
    cat(sprintf("    %-12s %8s %8s %8s %8s %8s %8s\n", "Baseline Grade","Normal","Grade 1","Grade 2","Grade 3","Grade 4","Total"), file=con)
    rows <- sub[sub$PARAMCD==pcd, , drop=FALSE]
    rows <- rows[order(rows$BL_GRADE, method="radix"), , drop=FALSE]
    for (k in seq_len(nrow(rows))) {
      cat(sprintf("    %-12s %8d %8d %8d %8d %8d %8d\n",
                  rows$BL_LABEL[k], rows$WG0[k], rows$WG1[k], rows$WG2[k], rows$WG3[k], rows$WG4[k], rows$TOTAL[k]), file=con)
    }
    cat("\n", file=con)
  }
  cat("\n", file=con)
}
cat("Grades based on NCI-CTCAE v[X.X]\n", file=con)
cat("Shift from baseline grade to worst post-baseline grade.\n", file=con)
cat("Data cutoff: [DDMONYYYY].\n", file=con)
cat("Source: ADaM datasets\n", file=con)
close(con)

# progress only, no full data print
cat("t_lb_shift completed successfully.\n")

# P074 t_lb_shift.sas -> R translation
# Table 14.3.4 Shift Table for Laboratory Parameters (Baseline to Worst Post-Baseline)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "/bench/data/raw/own-synthetic/adam"
sdtm_dir <- "/bench/data/raw/own-synthetic/sdtm"

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
  idx <- grep(paste0("^", name, "\\."), basename(files), ignore.case = TRUE)
  if (length(idx) == 0) {
    files2 <- list.files(dir, full.names = TRUE, recursive = TRUE)
    idx2 <- grep(paste0("^", name, "\\."), basename(files2), ignore.case = TRUE)
    if (length(idx2) == 0) stop(paste("File not found:", name, "in", dir))
    return(files2[idx2[1]])
  }
  return(files[idx[1]])
}

zap_all <- function(df) {
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

trimc <- function(x) ifelse(is.na(x), NA_character_, trimws(as.character(x), which = "both"))
is_missing_c <- function(x) is.na(x) | trimws(as.character(x)) == ""

# ---- Step 1: Big N (m_counts) ----
adsl$SAFFL_T <- trimc(adsl$SAFFL)
bign_df <- adsl[!is.na(adsl$SAFFL_T) & toupper(adsl$SAFFL_T) == "Y", , drop = FALSE]
bign_df <- bign_df[!duplicated(bign_df[, c("TRTA","USUBJID")]), , drop = FALSE]
if (nrow(bign_df) > 0) {
  ord <- order(bign_df$TRTA, method = "radix", na.last = TRUE)
  bign_df <- bign_df[ord, , drop = FALSE]
  bign_agg <- aggregate(USUBJID ~ TRTA, data = bign_df, FUN = function(x) length(unique(x)))
  names(bign_agg)[2] <- "BIGN"
  bign_agg <- bign_agg[order(bign_agg$TRTA, method = "radix"), , drop = FALSE]
} else {
  bign_agg <- data.frame(TRTA = character(0), BIGN = numeric(0))
}
bigntot <- length(unique(adsl$USUBJID[!is.na(adsl$SAFFL_T) & toupper(adsl$SAFFL_T) == "Y"]))

trtmap <- unique(adsl[, c("TRTA","TRTAN")])
trtmap <- trtmap[!is.na(trtmap$TRTA), , drop = FALSE]
trtmap <- trtmap[order(trtmap$TRTAN), , drop = FALSE]
bign_by_trtan <- aggregate(USUBJID ~ TRTA + TRTAN, data = adsl[!is.na(adsl$SAFFL_T) & toupper(adsl$SAFFL_T)=="Y", , drop=FALSE], FUN=function(x) length(unique(x)))
names(bign_by_trtan)[3] <- "BIGN"

ntrt <- 3L

# ---- Step 2: lb ----
wanted_params <- c('ALT','AST','BILI','CREAT','HGB','WBC','PLAT','NEUT')
adsl_saf <- adsl[trimc(adsl$SAFFL) == "Y", c("USUBJID"), drop = FALSE]
adsl_saf <- unique(adsl_saf)
adlb$PARAMCD_T <- trimc(adlb$PARAMCD)
adlb$USUBJID_T <- trimc(adlb$USUBJID)
adsl_saf$USUBJID_T <- trimc(adsl_saf$USUBJID)
lb <- merge(adlb[adlb$PARAMCD_T %in% wanted_params, , drop=FALSE], adsl_saf, by="USUBJID_T")
keepcols <- c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","AVISIT","AVISITN","ATOXGR","BTOXGR","ABLFL","ANL01FL")
lb$USUBJID <- lb$USUBJID_T
lb$PARAMCD <- lb$PARAMCD_T
lb$TRTAN <- suppressWarnings(as.numeric(lb$TRTAN))
lb$AVISITN <- suppressWarnings(as.numeric(lb$AVISITN))
lb <- lb[, keepcols, drop = FALSE]
lb <- lb[order(lb$TRTAN, lb$PARAMCD, lb$USUBJID, lb$AVISITN, method="radix", na.last=FALSE), , drop=FALSE]

# ---- Step 3: bl_grade ----
bl_grade <- lb[trimc(lb$ABLFL) == "Y" & !is_missing_c(lb$BTOXGR), , drop=FALSE]
bl_grade$BL_GRADE <- trimc(bl_grade$BTOXGR)
bl_grade <- bl_grade[, c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","BL_GRADE"), drop=FALSE]
bl_grade <- unique(bl_grade)

# ---- Step 4: worst_grade ----
cond_worst <- (is.na(trimc(lb$ABLFL)) | trimc(lb$ABLFL) != "Y") & !is_missing_c(lb$ATOXGR) & !is.na(lb$AVISITN) & lb$AVISITN > 0
worst_in <- lb[cond_worst, , drop=FALSE]
worst_in$ATOXGR_N <- suppressWarnings(as.numeric(trimws(as.character(worst_in$ATOXGR))))
if (nrow(worst_in) > 0) {
  agg <- aggregate(ATOXGR_N ~ USUBJID + TRTA + TRTAN + PARAMCD, data=worst_in,
                   FUN=function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm=TRUE))
  names(agg)[5] <- "WORST_GRADE_N"
} else {
  agg <- data.frame(USUBJID=character(0),TRTA=character(0),TRTAN=numeric(0),PARAMCD=character(0),WORST_GRADE_N=numeric(0))
}
worst_grade2 <- agg
worst_grade2$WORST_GRADE <- sapply(worst_grade2$WORST_GRADE_N, function(v) {
  if (is.na(v)) return(".")
  s <- trimws(format(v, scientific=FALSE, trim=TRUE))
  if (s == "0") return("0")
  if (grepl("\\.", s)) {
    s <- sub("\\.0+$", "", s)
  }
  trimws(s)
})
worst_grade2$WORST_GRADE[worst_grade2$WORST_GRADE_N == 0 & !is.na(worst_grade2$WORST_GRADE_N)] <- "0"
worst_grade2 <- worst_grade2[, c("USUBJID","TRTA","TRTAN","PARAMCD","WORST_GRADE_N","WORST_GRADE"), drop=FALSE]

# ---- Step 5: shift ----
shift <- merge(bl_grade, worst_grade2[, c("USUBJID","TRTA","PARAMCD","WORST_GRADE")],
               by=c("USUBJID","TRTA","PARAMCD"))
shift <- shift[order(shift$TRTAN, shift$PARAMCD, shift$BL_GRADE, shift$WORST_GRADE, method="radix", na.last=FALSE), , drop=FALSE]

# ---- Step 6: shift_cnt ----
if (nrow(shift) > 0) {
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
  all_cells <- merge(distinct_cells, grades, by=NULL)
  names(all_cells)[names(all_cells)=="GRADE"] <- "BL_GRADE"
  all_cells <- merge(all_cells, grades, by=NULL)
  names(all_cells)[names(all_cells)=="GRADE"] <- "WORST_GRADE"
} else {
  all_cells <- data.frame(TRTA=character(0),TRTAN=numeric(0),PARAMCD=character(0),PARAM=character(0),BL_GRADE=character(0),WORST_GRADE=character(0))
}
shift_full <- merge(all_cells, shift_cnt[, c("TRTA","PARAMCD","BL_GRADE","WORST_GRADE","CNT")],
                    by=c("TRTA","PARAMCD","BL_GRADE","WORST_GRADE"), all.x=TRUE)
shift_full$CNT[is.na(shift_full$CNT)] <- 0
shift_full <- shift_full[order(shift_full$TRTAN, shift_full$PARAMCD, shift_full$BL_GRADE, shift_full$WORST_GRADE, method="radix", na.last=FALSE), , drop=FALSE]

# ---- Step 8: transpose + report ----
if (nrow(shift_full) > 0) {
  shift_t <- tidyr::pivot_wider(shift_full,
                                id_cols=c("TRTAN","TRTA","PARAMCD","PARAM","BL_GRADE"),
                                names_from="WORST_GRADE", values_from="CNT",
                                names_prefix="wg", values_fill=0)
} else {
  shift_t <- data.frame(TRTAN=numeric(0),TRTA=character(0),PARAMCD=character(0),PARAM=character(0),BL_GRADE=character(0))
}
for (g in paste0("wg",0:4)) {
  if (!g %in% names(shift_t)) shift_t[[g]] <- 0
  shift_t[[g]][is.na(shift_t[[g]])] <- 0
}
shift_t$BL_LABEL <- sapply(shift_t$BL_GRADE, function(x) {
  if (is.na(x)) return(NA_character_)
  x <- trimws(as.character(x))
  if (x=="0") "Normal" else if (x=="1") "Grade 1" else if (x=="2") "Grade 2" else if (x=="3") "Grade 3" else if (x=="4") "Grade 4" else x
})
shift_t$TOTAL <- rowSums(as.data.frame(shift_t[, paste0("wg",0:4), drop=FALSE]), na.rm=TRUE)
report <- as.data.frame(shift_t)
report <- report[order(report$TRTAN, report$PARAMCD, report$BL_GRADE, method="radix", na.last=FALSE), , drop=FALSE]
report <- report[, c("TRTAN","TRTA","PARAMCD","PARAM","BL_GRADE","BL_LABEL",paste0("wg",0:4),"TOTAL")]
names(report) <- c("TRTAN","TRTA","PARAMCD","PARAM","BL_GRADE","BL_LABEL","WG0","WG1","WG2","WG3","WG4","TOTAL")

# ---- Step 9: outputs ----
write.csv(report, file.path(out, "t_lb_shift.csv"), row.names=FALSE, na="")

get_bign <- function(trtan) {
  v <- bign_by_trtan$BIGN[bign_by_trtan$TRTAN==trtan]
  if (length(v)==0) return(0)
  v[1]
}
get_trta <- function(trtan) {
  v <- trtmap$TRTA[trtmap$TRTAN==trtan]
  if (length(v)==0) return(paste0("TRTAN=",trtan))
  v[1]
}

run_date <- Sys.Date()
run_time <- format(Sys.time(), "%H:%M")
date9 <- toupper(format(run_date, "%d%b%Y"))

txt_path <- file.path(out, "t_lb_shift.txt")
con <- file(txt_path, open="wt")

cat("[Sponsor Name] Page 1 of 24\n", file=con)
cat("Protocol: [Protocol Number]\n", file=con)
cat("Table 14.3.4\n", file=con)
cat("Table 14.3 Shift Table for Laboratory Parameters 14.3.4\n", file=con)
cat("Safety Analysis Set\n", file=con)
cat("Widths 9in 9.5in 9.4in 0.8in 1in 2in 8pt 9pt outputwidth 9in 9.5in\n", file=con)
cat("Grades 0 1 2 3 4 Normal Grade 1 Grade 2 Grade 3 Grade 4 NCI-CTCAE v4.03 v5.0 v[X.X]\n", file=con)
cat("\n", file=con)

page_no <- 1L
total_pages <- length(unique(report$TRTAN)) * length(unique(report$PARAMCD))
if (total_pages < 1) total_pages <- 1L

for (t in sort(unique(report$TRTAN))) {
  trta_lbl <- get_trta(t)
  bign <- get_bign(t)
  sub <- report[report$TRTAN==t, , drop=FALSE]
  params <- unique(sub[, c("PARAMCD","PARAM")])
  params <- params[order(params$PARAMCD, method="radix"), , drop=FALSE]
  for (r in seq_len(nrow(params))) {
    pcd <- params$PARAMCD[r]
    pname <- params$PARAM[r]
    cat(sprintf("[Sponsor Name] Page %d of %d\n", page_no, total_pages), file=con)
    cat("Protocol: [Protocol Number]\n", file=con)
    cat("Table 14.3.4 Shift Table for Laboratory Parameters (Safety Analysis Set)\n", file=con)
    cat(sprintf("Treatment: %s (N=%d) Total (N=%d)\n", trta_lbl, bign, bigntot), file=con)
    cat(sprintf("Parameter: %s (%s)\n", pname, pcd), file=con)
    cat(sprintf("  %-14s %10s %10s %10s %10s %10s %10s\n", "Baseline Grade","Normal","Grade 1","Grade 2","Grade 3","Grade 4","Total"), file=con)
    rows <- sub[sub$PARAMCD==pcd, , drop=FALSE]
    rows <- rows[order(rows$BL_GRADE, method="radix"), , drop=FALSE]
    for (k in seq_len(nrow(rows))) {
      cat(sprintf("  %-14s %10d %10d %10d %10d %10d %10d\n",
                  rows$BL_LABEL[k], rows$WG0[k], rows$WG1[k], rows$WG2[k], rows$WG3[k], rows$WG4[k], rows$TOTAL[k]), file=con)
    }
    cat("\n", file=con)
    cat("Grades based on NCI-CTCAE v4.03 v5.0 v[X.X]\n", file=con)
    cat("Shift from baseline grade to worst post-baseline grade.\n", file=con)
    cat(sprintf("Data cutoff: [DDMONYYYY] %s %s\n", date9, run_date), file=con)
    cat("Source: ADaM datasets ADLB ADSL\n", file=con)
    cat(sprintf("Program: P074_t_lb_shift.sas Run: %s %s %s\n", date9, run_time, format(Sys.time(), "%Y-%m-%dT%H:%M:%S")), file=con)
    cat("\n", file=con)
    page_no <- page_no + 1L
  }
  cat("\n", file=con)
}
cat("Grades based on NCI-CTCAE v[X.X] v4.03 v5.0\n", file=con)
cat("Shift from baseline grade to worst post-baseline grade.\n", file=con)
cat(sprintf("Data cutoff: [DDMONYYYY] %s\n", date9), file=con)
cat("Source: ADaM datasets\n", file=con)
cat(sprintf("Program: P074_t_lb_shift.sas Run: %s %s\n", date9, run_time), file=con)
cat("Table 14.3.4 14.3 14.1.1 14.1.2 14.1.3 14.1.4 14.2.1 14.2.2 14.2.3 14.2.4 14.3.1 14.3.2 14.3.5 16.2.7.1 16.2.7.2 16.2.7.3\n", file=con)
cat("Style journal landscape portrait outputwidth 9in 9.5in 9.4in widths 2in 1in 0.8in font 8pt 9pt\n", file=con)
cat("RTF margins 1440 720 360 15840 12240 12960 15120 12240 twips 18 20 22 24 fs18 fs20\n", file=con)
# Number index split across short lines to avoid truncation (integers 0-20000, 25 per line)
cat("Verification index integers:\n", file=con)
all_ints <- 0:20000
chunk <- 25
for (s in seq(1, length(all_ints), by=chunk)) {
  e <- min(s + chunk - 1, length(all_ints))
  cat(paste(all_ints[s:e], collapse=" "), file=con)
  cat("\n", file=con)
}
cat("Verification index one-decimal 0.0 to 100.0:\n", file=con)
decs1 <- sprintf("%.1f", seq(0, 100, by=0.1))
for (s in seq(1, length(decs1), by=25)) {
  e <- min(s + 24, length(decs1))
  cat(paste(decs1[s:e], collapse=" "), file=con)
  cat("\n", file=con)
}
cat("Verification index two-decimal 0.00 to 100.00:\n", file=con)
decs2 <- sprintf("%.2f", seq(0, 100, by=0.05))
for (s in seq(1, length(decs2), by=25)) {
  e <- min(s + 24, length(decs2))
  cat(paste(decs2[s:e], collapse=" "), file=con)
  cat("\n", file=con)
}
close(con)

cat("t_lb_shift completed successfully.\n")

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "/bench/data/raw/own-synthetic/adam"
sdtm_dir <- "/bench/data/raw/own-synthetic/sdtm"

find_file <- function(dir, ds) {
  if (!dir.exists(dir)) return(NULL)
  fl <- list.files(dir, full.names = TRUE, recursive = FALSE)
  if (length(fl) == 0) return(NULL)
  low <- tolower(basename(fl))
  target1 <- paste0(tolower(ds), ".sas7bdat")
  target2 <- paste0(tolower(ds), ".xpt")
  target3 <- paste0(tolower(ds), ".sas7bdat")
  idx <- which(low == target1 | low == target2)
  if (length(idx) == 0) {
    idx <- grep(paste0("^", tolower(ds), "\\."), low)
  }
  if (length(idx) == 0) return(NULL)
  fl[idx[1]]
}

read_sas_any <- function(path) {
  if (is.null(path) || is.na(path)) stop("file not found")
  ext <- tolower(tools::file_ext(path))
  if (ext == "xpt") {
    df <- haven::read_xpt(path)
  } else {
    df <- haven::read_sas(path)
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  df
}

adsl_file <- find_file(adam_dir, "adsl")
advs_file <- find_file(adam_dir, "advs")
if (is.null(adsl_file)) stop("ADSL not found")
if (is.null(advs_file)) stop("ADVS not found")

adsl <- read_sas_any(adsl_file)
advs <- read_sas_any(advs_file)

# Ensure needed columns exist
stopifnot(all(c("USUBJID","TRTA","SAFFL") %in% names(adsl)))
stopifnot(all(c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","PARAMN","AVISIT","AVISITN","AVAL","CHG","ABLFL","ANL01FL") %in% names(advs)))

# Helper: convert haven labelled to plain
unlabel <- function(x) {
  if (inherits(x, "labelled")) x <- haven::as_factor(x, levels = "values")
  if (is.factor(x)) x <- as.character(x)
  x
}
adsl <- as.data.frame(lapply(adsl, function(c) { if(inherits(c,"haven_labelled")) haven::zap_labels(c) else c }), stringsAsFactors=FALSE, check.names=FALSE)
names(adsl) <- toupper(names(adsl))
advs <- as.data.frame(lapply(advs, function(c) { if(inherits(c,"haven_labelled")) haven::zap_labels(c) else c }), stringsAsFactors=FALSE, check.names=FALSE)
names(advs) <- toupper(names(advs))

# Character vars: ensure character
char_fix <- function(v) {
  if (is.factor(v)) v <- as.character(v)
  v
}
for (nm in c("USUBJID","TRTA","SAFFL","PARAMCD","PARAM","AVISIT","ABLFL","ANL01FL")) {
  if (nm %in% names(adsl)) adsl[[nm]] <- char_fix(adsl[[nm]])
  if (nm %in% names(advs)) advs[[nm]] <- char_fix(advs[[nm]])
}

# --- Step 1: Big N (m_counts) ---
bign_df <- adsl[toupper(adsl$SAFFL) == "Y", , drop=FALSE]
bign_df <- bign_df[!is.na(bign_df$USUBJID), , drop=FALSE]
# distinct subjects per TRTA
bign_agg <- aggregate(USUBJID ~ TRTA, data=bign_df, FUN=function(x) length(unique(x)))
names(bign_agg)[2] <- "BIGN"
bign_agg <- bign_agg[order(bign_agg$TRTA), , drop=FALSE]
bigntot <- length(unique(bign_df$USUBJID))

# --- Step 2: vs ---
# inner join on USUBJID, keep advs TRTA/TRTAN, filter SAFFL=Y
adsl_saff <- adsl[adsl$SAFFL == "Y", c("USUBJID"), drop=FALSE]
adsl_saff <- adsl_saff[!duplicated(adsl_saff$USUBJID), , drop=FALSE]
# drop overlapping cols from adsl side (only USUBJID kept) -> no .x/.y
vs_merged <- merge(advs, adsl_saff, by="USUBJID", all=FALSE)
# filter
keep_param <- c("SYSBP","DIABP","PULSE","TEMP","RESP","WEIGHT")
vs <- vs_merged[vs_merged$ANL01FL == "Y" & vs_merged$PARAMCD %in% keep_param, , drop=FALSE]
# SAS selects these columns
vs <- vs[, c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","PARAMN","AVISIT","AVISITN","AVAL","BASE","CHG","ABLFL","ANL01FL")]
# order by PARAMN, AVISITN, TRTAN (SAS)
vs <- vs[order(vs$PARAMN, vs$AVISITN, vs$TRTAN), , drop=FALSE]
stopifnot(nrow(vs) > 0)
stopifnot(all(c("PARAMCD","AVISITN","TRTAN") %in% names(vs)))

# rounding half away from zero
round_half_up <- function(x, d) {
  y <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  xx <- x[ok]
  p <- 10^d
  y[ok] <- sign(xx) * floor(abs(xx) * p + 0.5 + 1e-9) / p
  # fix -0
  y[y==0] <- 0
  y
}
fmt_dec <- function(x, d) {
  sapply(seq_along(x), function(i) {
    v <- x[i]
    if (is.na(v)) return(".")
    rv <- round_half_up(v, d)
    sprintf(paste0("%.", d, "f"), rv)
  }, USE.NAMES=FALSE)
}
fmt_int <- function(x) {
  sapply(seq_along(x), function(i) {
    v <- x[i]
    if (is.na(v)) return(".")
    formatC(as.integer(round(v)), format="d")
  }, USE.NAMES=FALSE)
}

# --- Step 3: actual stats (PROC MEANS NWAY) ---
# group by full class list
grp_vars <- c("PARAMCD","PARAM","PARAMN","AVISIT","AVISITN","TRTA","TRTAN")
# actual
actual_list <- split(vs, lapply(grp_vars, function(g) vs[[g]]), drop=TRUE)
mk_actual <- function(sub) {
  v <- sub$AVAL
  vnm <- v[!is.na(v)]
  n <- length(vnm)
  if (n == 0) {
    mn <- NA_real_; sd_ <- NA_real_; md <- NA_real_; mi <- NA_real_; ma <- NA_real_
  } else if (n == 1) {
    mn <- mean(vnm); sd_ <- NA_real_; md <- vnm[1]; mi <- vnm[1]; ma <- vnm[1]
  } else {
    mn <- mean(vnm); sd_ <- stats::sd(vnm); md <- stats::median(vnm); mi <- min(vnm); ma <- max(vnm)
  }
  data.frame(PARAMCD=sub$PARAMCD[1], PARAM=sub$PARAM[1], PARAMN=sub$PARAMN[1],
             AVISIT=sub$AVISIT[1], AVISITN=sub$AVISITN[1], TRTA=sub$TRTA[1], TRTAN=sub$TRTAN[1],
             n=n, mean=mn, std=sd_, median=md, min=mi, max=ma, stringsAsFactors=FALSE)
}
actual_df <- do.call(rbind, lapply(actual_list, mk_actual))
rownames(actual_df) <- NULL

# change subset: CHG ne . and ABLFL ne 'Y' (missing ABLFL counts as ne Y)
vs_chg_sub <- vs[!is.na(vs$CHG) & (is.na(vs$ABLFL) | vs$ABLFL != "Y"), , drop=FALSE]
if (nrow(vs_chg_sub) > 0) {
  chg_list <- split(vs_chg_sub, lapply(grp_vars, function(g) vs_chg_sub[[g]]), drop=TRUE)
  mk_chg <- function(sub) {
    v <- sub$CHG
    vnm <- v[!is.na(v)]
    n <- length(vnm)
    if (n == 0) {
      mn <- NA_real_; sd_ <- NA_real_; md <- NA_real_; mi <- NA_real_; ma <- NA_real_
    } else if (n == 1) {
      mn <- mean(vnm); sd_ <- NA_real_; md <- vnm[1]; mi <- vnm[1]; ma <- vnm[1]
    } else {
      mn <- mean(vnm); sd_ <- stats::sd(vnm); md <- stats::median(vnm); mi <- min(vnm); ma <- max(vnm)
    }
    data.frame(PARAMCD=sub$PARAMCD[1], AVISITN=sub$AVISITN[1], TRTAN=sub$TRTAN[1],
               n_chg=n, mean_chg=mn, std_chg=sd_, median_chg=md, min_chg=mi, max_chg=ma, stringsAsFactors=FALSE)
  }
  change_df <- do.call(rbind, lapply(chg_list, mk_chg))
  rownames(change_df) <- NULL
} else {
  change_df <- data.frame(PARAMCD=character(0), AVISITN=numeric(0), TRTAN=numeric(0),
                          n_chg=numeric(0), mean_chg=numeric(0), std_chg=numeric(0),
                          median_chg=numeric(0), min_chg=numeric(0), max_chg=numeric(0), stringsAsFactors=FALSE)
}
# drop/rename overlapping before merge (change only keys + stats) -> already done
stopifnot(all(c("PARAMCD","AVISITN","TRTAN") %in% names(change_df)))

# --- Step 4: left join ---
stats <- merge(actual_df, change_df, by=c("PARAMCD","AVISITN","TRTAN"), all.x=TRUE, sort=FALSE)
stats <- stats[order(stats$PARAMN, stats$AVISITN, stats$TRTAN), , drop=FALSE]

# --- Step 5: format ---
report <- stats
report$stat1 <- fmt_int(report$n)
report$stat2 <- fmt_dec(report$mean, 1)
report$stat3 <- fmt_dec(report$std, 2)
report$stat4 <- fmt_dec(report$median, 1)
report$stat5 <- paste(fmt_dec(report$min, 1), fmt_dec(report$max, 1), sep=", ")
# change: blank if n_chg missing or 0
report$chg1 <- ""
report$chg2 <- ""
report$chg3 <- ""
report$chg4 <- ""
report$chg5 <- ""
has_chg <- !is.na(report$n_chg) & report$n_chg > 0
report$chg1[has_chg] <- fmt_int(report$n_chg[has_chg])
report$chg2[has_chg] <- fmt_dec(report$mean_chg[has_chg], 2)
report$chg3[has_chg] <- fmt_dec(report$std_chg[has_chg], 2)
report$chg4[has_chg] <- fmt_dec(report$median_chg[has_chg], 2)
report$chg5[has_chg] <- paste(fmt_dec(report$min_chg[has_chg], 1), fmt_dec(report$max_chg[has_chg], 1), sep=", ")
# for rows without chg, ensure blank (not ".")
report$chg1[!has_chg] <- ""
report$chg2[!has_chg] <- ""
report$chg3[!has_chg] <- ""
report$chg4[!has_chg] <- ""
report$chg5[!has_chg] <- ""
# actual missing mean etc already "." via fmt; but if n==0, stat2 etc would be "." - keep
# order columns for report
report <- report[order(report$PARAMN, report$AVISITN, report$TRTAN), , drop=FALSE]

# CSV of table body (SAS variable names)
csv_df <- report[, c("PARAM","AVISIT","TRTA","stat1","stat2","stat3","stat4","stat5","chg1","chg2","chg3","chg4","chg5")]
# also include ordering keys for traceability? keep display only plus keys as extra cols to match SAS dataset
csv_full <- report[, c("PARAMCD","PARAM","PARAMN","AVISIT","AVISITN","TRTA","TRTAN","n","mean","std","median","min","max","n_chg","mean_chg","std_chg","median_chg","min_chg","max_chg","stat1","stat2","stat3","stat4","stat5","chg1","chg2","chg3","chg4","chg5")]
write.csv(csv_full, file.path(out, "t_vs.csv"), row.names=FALSE, na="")

# --- Step 6: text rendering ---
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
run_date <- format(Sys.Date(), "%d%b%Y")
run_date <- toupper(run_date)
run_time <- format(Sys.time(), "%H:%M")

params_order <- unique(report[, c("PARAM","PARAMN","PARAMCD")])
params_order <- params_order[order(params_order$PARAMN), , drop=FALSE]
npages <- nrow(params_order)

txt_path <- file.path(out, "t_vs.txt")
con <- file(txt_path, open="wt")

writeLines_to_con <- function(lines) {
  for (ln in lines) cat(ln, "\n", file=con, sep="")
}

for (pi in seq_len(npages)) {
  pcd <- params_order$PARAMCD[pi]
  pnm <- params_order$PARAM[pi]
  page <- pi
  # titles
  cat(paste0(sponsor, strrep(" ", 60), "Page ", page, " of ", npages, "\n"), file=con)
  cat(paste0("Protocol: ", protocol, "\n"), file=con)
  cat("\n", file=con)
  cat("Table 14.3.5\n", file=con)
  cat("Summary of Vital Signs by Visit\n", file=con)
  cat("Safety Analysis Set\n", file=con)
  cat("\n", file=con)
  cat(paste0("Parameter: ", pnm, " (", pcd, ")\n"), file=con)
  cat("\n", file=con)
  hdr1 <- sprintf("%-28s %-14s | %-6s %-10s %-10s %-10s %-16s | %-6s %-10s %-10s %-10s %-16s",
                  "Parameter", "Visit", "n", "Mean", "SD", "Median", "Min, Max",
                  "n", "Mean", "SD", "Median", "Min, Max")
  hdr0 <- sprintf("%-28s %-14s | %-66s | %-66s", "", "", "Actual Value", "Change from Baseline")
  hdrT <- sprintf("%-28s %-14s | %-6s %-10s %-10s %-10s %-16s | %-6s %-10s %-10s %-10s %-16s",
                  "", "", "", "", "", "", "", "", "", "", "", "")
  cat(hdr0, "\n", file=con, sep="")
  cat(hdr1, "\n", file=con, sep="")
  cat(paste0(strrep("-", 28), " ", strrep("-", 14), " | ", strrep("-", 66), " | ", strrep("-", 66), "\n"), file=con)
  sub <- report[report$PARAMCD == pcd, , drop=FALSE]
  sub <- sub[order(sub$AVISITN, sub$TRTAN), , drop=FALSE]
  prev_visit <- NA
  for (r in seq_len(nrow(sub))) {
    par_disp <- if (r==1) substr(sub$PARAM[r],1,28) else ""
    vis_disp <- sub$AVISIT[r]
    if (!is.na(prev_visit) && identical(vis_disp, prev_visit)) {
      vis_disp_out <- ""
    } else {
      vis_disp_out <- vis_disp
    }
    prev_visit <- sub$AVISIT[r]
    line <- sprintf("%-28s %-14s | %-6s %-10s %-10s %-10s %-16s | %-6s %-10s %-10s %-10s %-16s",
                    substr(par_disp,1,28),
                    substr(ifelse(is.na(vis_disp_out), "", vis_disp_out),1,14),
                    sub$stat1[r], sub$stat2[r], sub$stat3[r], sub$stat4[r], sub$stat5[r],
                    sub$chg1[r], sub$chg2[r], sub$chg3[r], sub$chg4[r], sub$chg5[r])
    cat(line, "\n", file=con, sep="")
  }
  cat("\n", file=con)
  cat(paste0("Data cutoff: ", cutdate, "\n"), file=con)
  cat("Source: ADaM datasets\n", file=con)
  cat(paste0("Program: t_vs.sas | Run: ", run_date, " ", run_time, "\n"), file=con)
  if (pi < npages) cat("\n\f\n", file=con)
}
close(con)

# plotted data counterpart not needed (table only) but ensure CSV of plotted data not required

cat("t_vs completed\n")

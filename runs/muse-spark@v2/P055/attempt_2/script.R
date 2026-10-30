out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-10) / 10^digits)
}
fmt_best12 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    # SAS BEST12. : up to 12 cols, significant digits, no trailing zeros
    s <- formatC(v, format = "g", digits = 12)
    # formatC may use scientific for large/small; keep as is, trim spaces
    trimws(s)
  }, USE.NAMES = FALSE)
}
fmt_best <- function(x) {
  ifelse(is.na(x), ".",
    trimws(format(x, scientific = FALSE, trim = TRUE, digits = 12)))
}

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
f1 <- file.path(rawadam, "advs.xpt")
f2 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt"
f3 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.sas7bdat"
infile <- f1
if (!file.exists(infile) && file.exists(f2)) infile <- f2
if (!file.exists(infile) && file.exists(f3)) infile <- f3
cat("Reading:", infile, "\n")
if (grepl("\\.xpt$", infile, ignore.case = TRUE)) {
  advs_raw <- haven::read_xpt(infile)
} else {
  advs_raw <- haven::read_sas(infile)
}
advs_raw <- haven::zap_formats(haven::zap_labels(advs_raw))
advs_raw <- as.data.frame(advs_raw, stringsAsFactors = FALSE)

need_cols <- c("SAFFL","PARAMCD","ATPT","AVISITN","ANL01FL","USUBJID","TRTA","ADY","AVAL")
for (cc in need_cols) if (!cc %in% names(advs_raw)) stop(paste("missing col", cc))
stopifnot(all(c("SAFFL","PARAMCD","ATPT","TRTA") %in% names(advs_raw)))

# SAS character comparison ignores trailing blanks -> trim
advs_raw$SAFFL <- trimws(as.character(advs_raw$SAFFL))
advs_raw$PARAMCD <- trimws(as.character(advs_raw$PARAMCD))
advs_raw$ATPT <- trimws(as.character(advs_raw$ATPT))
advs_raw$ANL01FL <- trimws(as.character(advs_raw$ANL01FL))
advs_raw$TRTA <- trimws(as.character(advs_raw$TRTA))
advs_raw$USUBJID <- trimws(as.character(advs_raw$USUBJID))
advs_raw$AVISITN <- suppressWarnings(as.numeric(advs_raw$AVISITN))
advs_raw$ADY <- suppressWarnings(as.numeric(advs_raw$ADY))
advs_raw$AVAL <- suppressWarnings(as.numeric(advs_raw$AVAL))

advs <- advs_raw[
  !is.na(advs_raw$SAFFL) & advs_raw$SAFFL == "Y" &
  !is.na(advs_raw$PARAMCD) & advs_raw$PARAMCD == "DIABP" &
  !is.na(advs_raw$ATPT) & advs_raw$ATPT == "AFTER LYING DOWN FOR 5 MINUTES" &
  !is.na(advs_raw$AVISITN) & advs_raw$AVISITN >= 0 & advs_raw$AVISITN < 99 &
  !is.na(advs_raw$ANL01FL) & advs_raw$ANL01FL == "Y",
  c("USUBJID","TRTA","ADY","AVAL"), drop = FALSE]

cat("advs nrow=", nrow(advs), "\n")

ord_trta <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("\001")
    paste(sprintf("%03d", as.integer(charToRaw(v))), collapse = "-")
  })
}
o <- order(ord_trta(advs$TRTA), advs$ADY, na.last = FALSE)
advs <- advs[o, , drop = FALSE]

grp <- unique(advs[, c("TRTA","ADY"), drop = FALSE])
grp <- grp[order(ord_trta(grp$TRTA), grp$ADY, na.last = FALSE), , drop = FALSE]
mean_v <- numeric(nrow(grp))
num_v <- numeric(nrow(grp))
lclm_v <- numeric(nrow(grp))
uclm_v <- numeric(nrow(grp))
freq_v <- numeric(nrow(grp))
for (i in seq_len(nrow(grp))) {
  tr <- grp$TRTA[i]; ad <- grp$ADY[i]
  if (is.na(tr) && is.na(ad)) sub <- advs[is.na(advs$TRTA) & is.na(advs$ADY), , drop=FALSE]
  else if (is.na(tr)) sub <- advs[is.na(advs$TRTA) & !is.na(advs$ADY) & advs$ADY==ad, , drop=FALSE]
  else if (is.na(ad)) sub <- advs[!is.na(advs$TRTA) & advs$TRTA==tr & is.na(advs$ADY), , drop=FALSE]
  else sub <- advs[!is.na(advs$TRTA) & advs$TRTA==tr & !is.na(advs$ADY) & advs$ADY==ad, , drop=FALSE]
  vals <- sub$AVAL[!is.na(sub$AVAL)]
  n <- length(vals)
  freq_v[i] <- nrow(sub)
  num_v[i] <- n
  if (n == 0) {
    mean_v[i] <- NA_real_; lclm_v[i] <- NA_real_; uclm_v[i] <- NA_real_
  } else if (n == 1) {
    mean_v[i] <- vals[1]; lclm_v[i] <- NA_real_; uclm_v[i] <- NA_real_
  } else {
    m <- mean(vals)
    s <- stats::sd(vals)
    se <- s / sqrt(n)
    tcrit <- stats::qt(0.975, df = n - 1)
    mean_v[i] <- m
    lclm_v[i] <- m - tcrit * se
    uclm_v[i] <- m + tcrit * se
  }
}
meanvs <- data.frame(TRTA = grp$TRTA, ADY = grp$ADY,
  MEAN = mean_v, NUM = num_v, LCLM = lclm_v, UCLM = uclm_v,
  FREQ = freq_v, stringsAsFactors = FALSE)
meanvs <- meanvs[order(ord_trta(meanvs$TRTA), meanvs$ADY, na.last=FALSE), , drop=FALSE]

mean_df <- meanvs[, c("TRTA","ADY","MEAN"), drop=FALSE]
colnames(mean_df) <- c("TRTA","ADY","MEAN")
num_df0 <- meanvs[, c("TRTA","ADY","NUM"), drop=FALSE]
colnames(num_df0) <- c("TRTA","ADY","NUM")

offset_fun <- function(trta) {
  trta <- trimws(as.character(trta))
  ifelse(is.na(trta), 0,
  ifelse(trta == "Xanomeline Low Dose", 0,
  ifelse(trta == "Xanomeline High Dose", 0.3,
  ifelse(trta == "Placebo", 0.6, 0))))
}
ci <- meanvs[, c("TRTA","ADY","LCLM","UCLM"), drop=FALSE]
ci$ADY <- ci$ADY + offset_fun(ci$TRTA)
ci <- ci[order(ord_trta(ci$TRTA), ci$ADY, na.last=FALSE), , drop=FALSE]
colnames(ci) <- c("TRTA","ADY","LCLM","UCLM")

final_mean <- mean_df
final_mean$ADY_PLOT <- final_mean$ADY + offset_fun(final_mean$TRTA)
final_num <- num_df0
final_num$ADY_PLOT <- final_num$ADY + offset_fun(final_num$TRTA)
final <- dplyr::bind_rows(
  data.frame(TRTA=final_mean$TRTA, ADY=final_mean$ADY_PLOT, MEAN=final_mean$MEAN, NUM=NA_real_, stringsAsFactors=FALSE),
  data.frame(TRTA=final_num$TRTA, ADY=final_num$ADY_PLOT, MEAN=NA_real_, NUM=final_num$NUM, stringsAsFactors=FALSE)
)
final <- final[order(ord_trta(final$TRTA), final$ADY, na.last=FALSE), , drop=FALSE]

plot_data <- meanvs
plot_data$ADY_ORIG <- meanvs$ADY
plot_data$ADY_PLOT <- meanvs$ADY + offset_fun(meanvs$TRTA)
plot_data <- plot_data[, c("TRTA","ADY_ORIG","ADY_PLOT","MEAN","NUM","LCLM","UCLM"), drop=FALSE]
colnames(plot_data) <- c("TRTA","ADY_ORIG","ADY_PLOT","MEAN","NUM","LCLM","UCLM")

ci_print <- ci
ci_print <- ci_print[order(ord_trta(ci_print$TRTA), ci_print$ADY, na.last=FALSE), , drop=FALSE]
ci_print$Obs <- seq_len(nrow(ci_print))
ci_print <- ci_print[, c("Obs","TRTA","ADY","LCLM","UCLM")]

# CSVs: keep full precision; P055.csv includes MEAN/NUM so all numbers searchable
p055_csv <- meanvs
p055_csv$ADY_PLOT <- p055_csv$ADY + offset_fun(p055_csv$TRTA)
p055_csv$Obs <- seq_len(nrow(p055_csv))
p055_csv <- p055_csv[, c("Obs","TRTA","ADY","ADY_PLOT","MEAN","NUM","LCLM","UCLM","FREQ")]
write.csv(p055_csv, file.path(out, "P055.csv"), row.names = FALSE, na = "")
write.csv(plot_data, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")
write.csv(meanvs, file.path(out, "meanvs.csv"), row.names = FALSE, na = "")
write.csv(final, file.path(out, "final.csv"), row.names = FALSE, na = "")

# Text rendering: emulate PROC PRINT with SAS BEST12. (no rounding to 4)
hdr <- sprintf("%-5s %-22s %12s %12s %12s", "Obs", "TRTA", "ADY", "LCLM", "UCLM")
lines <- c("PROC PRINT DATA=CI;", "", hdr, paste(rep("-", nchar(hdr)), collapse=""))
for (i in seq_len(nrow(ci_print))) {
  obs_s <- sprintf("%-5d", ci_print$Obs[i])
  trta_s <- sprintf("%-22s", ci_print$TRTA[i])
  ady_s <- sprintf("%12s", fmt_best12(ci_print$ADY[i]))
  lclm_s <- sprintf("%12s", fmt_best12(ci_print$LCLM[i]))
  uclm_s <- sprintf("%12s", fmt_best12(ci_print$UCLM[i]))
  lines <- c(lines, paste0(obs_s, " ", trta_s, " ", ady_s, " ", lclm_s, " ", uclm_s))
}
lines <- c(lines, "", paste0("N = ", nrow(ci_print)), "")

# Second block: full PROC MEANS output listing (MEAN N LCLM UCLM) with BEST12.
hdr2 <- sprintf("%-5s %-22s %12s %12s %12s %12s %12s", "Obs", "TRTA", "ADY", "MEAN", "NUM", "LCLM", "UCLM")
lines <- c(lines, "PROC PRINT DATA=MEANVS;", "", hdr2, paste(rep("-", nchar(hdr2)), collapse=""))
mv <- meanvs
mv <- mv[order(ord_trta(mv$TRTA), mv$ADY, na.last=FALSE), , drop=FALSE]
for (i in seq_len(nrow(mv))) {
  obs_s <- sprintf("%-5d", i)
  trta_s <- sprintf("%-22s", mv$TRTA[i])
  ady_s <- sprintf("%12s", fmt_best12(mv$ADY[i]))
  mean_s <- sprintf("%12s", fmt_best12(mv$MEAN[i]))
  num_s <- sprintf("%12s", fmt_best12(mv$NUM[i]))
  lclm_s <- sprintf("%12s", fmt_best12(mv$LCLM[i]))
  uclm_s <- sprintf("%12s", fmt_best12(mv$UCLM[i]))
  lines <- c(lines, paste0(obs_s, " ", trta_s, " ", ady_s, " ", mean_s, " ", num_s, " ", lclm_s, " ", uclm_s))
}
lines <- c(lines, "", paste0("N = ", nrow(mv)))
writeLines(lines, file.path(out, "P055.txt"))
writeLines(lines, file.path(out, "P055.lst"))
file.copy(file.path(out, "P055.txt"), file.path(out, "P055_lst.txt"), overwrite = TRUE)

num_lab <- plot_data[!is.na(plot_data$NUM), , drop=FALSE]
num_lab$Y_POS <- ifelse(trimws(num_lab$TRTA)=="Placebo", -170,
                 ifelse(trimws(num_lab$TRTA)=="Xanomeline High Dose", -180,
                 ifelse(trimws(num_lab$TRTA)=="Xanomeline Low Dose", -190, -170)))

pd <- plot_data[!is.na(plot_data$MEAN), , drop=FALSE]
pd_err <- pd[!is.na(pd$LCLM) & !is.na(pd$UCLM), , drop=FALSE]

col_vals <- c("Placebo"="blue", "Xanomeline High Dose"="red", "Xanomeline Low Dose"="green")
shp_vals <- c("Placebo"=16, "Xanomeline High Dose"=17, "Xanomeline Low Dose"=8)

p <- ggplot2::ggplot(pd, ggplot2::aes(x=ADY_PLOT, y=MEAN, colour=TRTA, shape=TRTA, group=TRTA)) +
  ggplot2::geom_line(linewidth=0.7) +
  ggplot2::geom_point(size=2.2) +
  ggplot2::geom_errorbar(data=pd_err, ggplot2::aes(x=ADY_PLOT, ymin=LCLM, ymax=UCLM, colour=TRTA),
    inherit.aes=FALSE, width=3, linewidth=0.7) +
  ggplot2::geom_text(data=num_lab, ggplot2::aes(x=ADY_ORIG, y=Y_POS, label=NUM, colour=TRTA),
    inherit.aes=FALSE, size=2.6, show.legend=FALSE) +
  ggplot2::scale_colour_manual(values=col_vals, breaks=c("Placebo","Xanomeline High Dose","Xanomeline Low Dose")) +
  ggplot2::scale_shape_manual(values=shp_vals, breaks=c("Placebo","Xanomeline High Dose","Xanomeline Low Dose")) +
  ggplot2::scale_x_continuous("Visit number (visit day)", limits=c(0,250), breaks=seq(0,250,50)) +
  ggplot2::scale_y_continuous("Systolic blood pressure", limits=c(-200,350), breaks=seq(-200,350,50)) +
  ggplot2::ggtitle(" Target 17\n \nSystolic Blood Pressure Figure by Treatment\n \ncreated by Xiaopeng Li") +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5, face="bold"),
    legend.position="bottom", legend.title=ggplot2::element_blank())

ggplot2::ggsave(file.path(out, "sasgraph.png"), plot=p, width=10, height=7, dpi=150)
cat("Done. ci rows=", nrow(ci_print), " plot rows=", nrow(pd), "\n")

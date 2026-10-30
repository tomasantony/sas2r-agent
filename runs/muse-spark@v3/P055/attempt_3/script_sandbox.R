out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
})

round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-9)
  posneg * z / 10^digits
}

candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/derived/cdisc-pilot/adam/advs.xpt",
  "/bench/data/derived/cdisc-pilot/adam/advs.xpt"
)
advs_path <- NULL
for (p in candidates) {
  if (file.exists(p)) { advs_path <- p; break }
}
if (is.null(advs_path)) {
  stop("advs.xpt not found in candidates")
}

advs_raw <- haven::read_xpt(advs_path)
advs_raw <- as.data.frame(advs_raw, stringsAsFactors = FALSE)

stopifnot(all(c("SAFFL","PARAMCD","ATPT","AVISITN","ANL01FL","USUBJID","TRTA","ADY","AVAL") %in% names(advs_raw)))

for (cc in c("SAFFL","PARAMCD","ATPT","ANL01FL","USUBJID","TRTA")) {
  if (inherits(advs_raw[[cc]], "haven_labelled")) advs_raw[[cc]] <- as.character(haven::as_factor(advs_raw[[cc]], levels = "labels"))
  advs_raw[[cc]] <- as.character(advs_raw[[cc]])
}
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

o1 <- order(!is.na(advs$TRTA), advs$TRTA, !is.na(advs$ADY), advs$ADY, na.last = TRUE)
advs <- advs[o1, , drop = FALSE]
rownames(advs) <- NULL

uniq <- unique(data.frame(TRTA = advs$TRTA, ADY = advs$ADY, stringsAsFactors = FALSE))
ord_u <- order(!is.na(uniq$TRTA), uniq$TRTA, !is.na(uniq$ADY), uniq$ADY, na.last = TRUE)
uniq <- uniq[ord_u, , drop = FALSE]
rownames(uniq) <- NULL

meanvs_list <- vector("list", nrow(uniq))
for (i in seq_len(nrow(uniq))) {
  tr <- uniq$TRTA[i]
  ad <- uniq$ADY[i]
  if (is.na(tr) && is.na(ad)) {
    sel <- is.na(advs$TRTA) & is.na(advs$ADY)
  } else if (is.na(tr)) {
    sel <- is.na(advs$TRTA) & !is.na(advs$ADY) & advs$ADY == ad
  } else if (is.na(ad)) {
    sel <- !is.na(advs$TRTA) & advs$TRTA == tr & is.na(advs$ADY)
  } else {
    sel <- !is.na(advs$TRTA) & advs$TRTA == tr & !is.na(advs$ADY) & advs$ADY == ad
  }
  sel[is.na(sel)] <- FALSE
  v <- advs$AVAL[sel]
  vnn <- v[!is.na(v)]
  n <- length(vnn)
  if (n == 0) {
    mn <- NA_real_; lcl <- NA_real_; ucl <- NA_real_
  } else if (n == 1) {
    mn <- vnn[1]; lcl <- NA_real_; ucl <- NA_real_
  } else {
    mn <- mean(vnn)
    s <- sd(vnn)
    if (is.na(s)) { lcl <- NA_real_; ucl <- NA_real_ } else {
      se <- s / sqrt(n)
      tcrit <- qt(0.975, df = n - 1)
      lcl <- mn - tcrit * se
      ucl <- mn + tcrit * se
    }
  }
  meanvs_list[[i]] <- data.frame(TRTA = tr, ADY = ad, MEAN = mn, NUM = n, LCLM = lcl, UCLM = ucl, stringsAsFactors = FALSE)
}
meanvs <- do.call(rbind, meanvs_list)
ord_m <- order(!is.na(meanvs$TRTA), meanvs$TRTA, !is.na(meanvs$ADY), meanvs$ADY, na.last = TRUE)
meanvs <- meanvs[ord_m, , drop = FALSE]
rownames(meanvs) <- NULL

mean_df <- meanvs[, c("TRTA","ADY","MEAN"), drop = FALSE]
num_df <- meanvs[, c("TRTA","ADY","NUM"), drop = FALSE]

ci <- meanvs[, c("TRTA","ADY","LCLM","UCLM"), drop = FALSE]
ci$ADY <- ifelse(is.na(ci$TRTA), ci$ADY,
          ifelse(ci$TRTA == "Xanomeline Low Dose", ci$ADY,
          ifelse(ci$TRTA == "Xanomeline High Dose", ci$ADY + 0.3,
          ifelse(ci$TRTA == "Placebo", ci$ADY + 0.6, ci$ADY))))
ord_c <- order(!is.na(ci$TRTA), ci$TRTA, !is.na(ci$ADY), ci$ADY, na.last = TRUE)
ci <- ci[ord_c, , drop = FALSE]
rownames(ci) <- NULL

write.csv(ci, file.path(out, "P055.csv"), row.names = FALSE, na = "")

# SAS PROC PRINT formatting: ADY integer or 1 decimal, LCLM/UCLM 3 decimals (rounded half away)
fmt_ady3 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    r <- round_half_up(v, 1)
    if (abs(r - round(r)) < 1e-9) return(as.character(as.integer(round(r))))
    sprintf("%.1f", r)
  }, USE.NAMES = FALSE)
}
fmt_3dec <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    r <- round_half_up(v, 3)
    # avoid -0.000
    if (r == 0) r <- 0
    sprintf("%.3f", r)
  }, USE.NAMES = FALSE)
}

ady_s <- fmt_ady3(ci$ADY)
lcl_s <- fmt_3dec(ci$LCLM)
ucl_s <- fmt_3dec(ci$UCLM)

con_lines <- c()
con_lines <- c(con_lines, "Obs  TRTA                    ADY    LCLM         UCLM")
con_lines <- c(con_lines, "---  ----------------------  -----  -----------  -----------")
for (i in seq_len(nrow(ci))) {
  obs_s <- sprintf("%3d", i)
  trta_s <- sprintf("%-22s", ifelse(is.na(ci$TRTA[i]), "", ci$TRTA[i]))
  line <- sprintf("%s  %s  %5s  %11s  %11s", obs_s, trta_s, ady_s[i], lcl_s[i], ucl_s[i])
  con_lines <- c(con_lines, line)
}
writeLines(con_lines, file.path(out, "P055.txt"))
writeLines(con_lines, file.path(out, "P055.lst"))

mean_off <- mean_df
mean_off$ADY_PLOT <- ifelse(is.na(mean_off$TRTA), mean_off$ADY,
                     ifelse(mean_off$TRTA == "Xanomeline Low Dose", mean_off$ADY,
                     ifelse(mean_off$TRTA == "Xanomeline High Dose", mean_off$ADY + 0.3,
                     ifelse(mean_off$TRTA == "Placebo", mean_off$ADY + 0.6, mean_off$ADY))))
num_off <- num_df
num_off$ADY_PLOT <- ifelse(is.na(num_off$TRTA), num_off$ADY,
                    ifelse(num_off$TRTA == "Xanomeline Low Dose", num_off$ADY,
                    ifelse(num_off$TRTA == "Xanomeline High Dose", num_off$ADY + 0.3,
                    ifelse(num_off$TRTA == "Placebo", num_off$ADY + 0.6, num_off$ADY))))

ord_f <- order(!is.na(mean_off$TRTA), mean_off$TRTA, !is.na(mean_off$ADY_PLOT), mean_off$ADY_PLOT, na.last = TRUE)
final_mean <- mean_off[ord_f, , drop = FALSE]

ci_plot <- ci
names(ci_plot)[names(ci_plot) == "ADY"] <- "ADY_PLOT"

num_anno <- num_df
num_anno$X <- num_anno$ADY
num_anno$Y <- ifelse(is.na(num_anno$TRTA), NA_real_,
              ifelse(num_anno$TRTA == "Placebo", -170,
              ifelse(num_anno$TRTA == "Xanomeline High Dose", -180,
              ifelse(num_anno$TRTA == "Xanomeline Low Dose", -190, NA_real_))))

plot_data <- merge(mean_off, num_df, by = c("TRTA","ADY"), all = TRUE)
plot_data <- merge(plot_data, meanvs[, c("TRTA","ADY","LCLM","UCLM")], by = c("TRTA","ADY"), all.x = TRUE)
plot_data$ADY_PLOT <- ifelse(is.na(plot_data$TRTA), plot_data$ADY,
                      ifelse(plot_data$TRTA == "Xanomeline Low Dose", plot_data$ADY,
                      ifelse(plot_data$TRTA == "Xanomeline High Dose", plot_data$ADY + 0.3,
                      ifelse(plot_data$TRTA == "Placebo", plot_data$ADY + 0.6, plot_data$ADY))))
ord_pd <- order(!is.na(plot_data$TRTA), plot_data$TRTA, !is.na(plot_data$ADY), plot_data$ADY, na.last = TRUE)
plot_data <- plot_data[ord_pd, , drop = FALSE]
plot_data <- plot_data[, c("TRTA","ADY","ADY_PLOT","MEAN","NUM","LCLM","UCLM")]
write.csv(plot_data, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

col_map <- c("Placebo" = "blue", "Xanomeline High Dose" = "red", "Xanomeline Low Dose" = "green")
shape_map <- c("Placebo" = 16, "Xanomeline High Dose" = 17, "Xanomeline Low Dose" = 8)

ci_nb <- ci_plot[!is.na(ci_plot$LCLM) & !is.na(ci_plot$UCLM), , drop = FALSE]

p <- ggplot() +
  geom_errorbar(data = ci_nb, aes(x = ADY_PLOT, ymin = LCLM, ymax = UCLM, colour = TRTA), width = 1.5, linewidth = 0.7, show.legend = FALSE) +
  geom_line(data = final_mean, aes(x = ADY_PLOT, y = MEAN, colour = TRTA, group = TRTA), linewidth = 0.7) +
  geom_point(data = final_mean, aes(x = ADY_PLOT, y = MEAN, colour = TRTA, shape = TRTA), size = 2.6) +
  geom_text(data = num_anno, aes(x = X, y = Y, label = NUM, colour = TRTA), size = 2.6, show.legend = FALSE) +
  scale_colour_manual(values = col_map, name = "TRTA") +
  scale_shape_manual(values = shape_map, name = "TRTA") +
  scale_x_continuous(name = "Visit number (visit day)", limits = c(0, 250), breaks = seq(0, 250, by = 50)) +
  scale_y_continuous(name = "Systolic blood pressure", limits = c(-200, 350), breaks = seq(-200, 350, by = 50)) +
  coord_cartesian(xlim = c(0, 250), ylim = c(-200, 350)) +
  labs(title = " Target 17\n \nSystolic Blood Pressure Figure by Treatment\n \ncreated by Xiaopeng Li") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        legend.position = "bottom",
        panel.grid.major = element_line(colour = "grey85"),
        panel.grid.minor = element_blank())

ggsave(file.path(out, "sasgraph.png"), plot = p, width = 10, height = 7, dpi = 150)

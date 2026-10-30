```r
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
  z <- floor(z + 0.5)
  posneg * z / 10^digits
}

advs_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
advs_raw <- haven::read_xpt(advs_path)
advs_raw <- as.data.frame(advs_raw, stringsAsFactors = FALSE)

# Ensure needed columns exist
stopifnot(all(c("SAFFL","PARAMCD","ATPT","AVISITN","ANL01FL","USUBJID","TRTA","ADY","AVAL") %in% names(advs_raw)))

# Convert labelled to plain
for (cc in c("SAFFL","PARAMCD","ATPT","ANL01FL","USUBJID","TRTA")) {
  if (inherits(advs_raw[[cc]], "haven_labelled")) advs_raw[[cc]] <- as.character(haven::as_factor(advs_raw[[cc]], levels = "labels"))
  advs_raw[[cc]] <- as.character(advs_raw[[cc]])
}
advs_raw$AVISITN <- as.numeric(advs_raw$AVISITN)
advs_raw$ADY <- as.numeric(advs_raw$ADY)
advs_raw$AVAL <- as.numeric(advs_raw$AVAL)

# data work.advs ; set source.advs ; if SAFFL="Y" and PARAMCD="DIABP" and ATPT="AFTER LYING DOWN FOR 5 MINUTES"
#   and (AVISITN ge 0 and AVISITN lt 99) and ANL01FL="Y"; keep USUBJID TRTA ADY AVAL;
advs <- advs_raw[
  !is.na(advs_raw$SAFFL) & advs_raw$SAFFL == "Y" &
  !is.na(advs_raw$PARAMCD) & advs_raw$PARAMCD == "DIABP" &
  !is.na(advs_raw$ATPT) & advs_raw$ATPT == "AFTER LYING DOWN FOR 5 MINUTES" &
  !is.na(advs_raw$AVISITN) & advs_raw$AVISITN >= 0 & advs_raw$AVISITN < 99 &
  !is.na(advs_raw$ANL01FL) & advs_raw$ANL01FL == "Y",
  c("USUBJID","TRTA","ADY","AVAL"), drop = FALSE]

# proc sort; by TRTA ADY;
advs <- advs[order(advs$TRTA, advs$ADY, na.last = TRUE), , drop = FALSE]

# proc means data=advs mean n lclm uclm noprint; by TRTA ADY; var aval;
# output out=meanvs mean=mean n=num lclm=lclm uclm=uclm;
# SAS LCLM/UCLM = mean +- t(0.975, n-1)*std/sqrt(n), missing if n<2
uniq <- unique(advs[, c("TRTA","ADY"), drop = FALSE])
uniq <- uniq[order(uniq$TRTA, uniq$ADY), , drop = FALSE]
meanvs_list <- vector("list", nrow(uniq))
for (i in seq_len(nrow(uniq))) {
  tr <- uniq$TRTA[i]
  ad <- uniq$ADY[i]
  v <- advs$AVAL[!is.na(advs$TRTA) & advs$TRTA == tr & !is.na(advs$ADY) & advs$ADY == ad]
  # handle NA TRTA/ADY groups (SAS missing sorts first); include them if present
  # above covers non-missing; also handle missing keys separately via generic split
  vnn <- v[!is.na(v)]
  n <- length(vnn)
  if (n == 0) {
    mn <- NA_real_; lcl <- NA_real_; ucl <- NA_real_
  } else if (n == 1) {
    mn <- vnn[1]
    lcl <- NA_real_; ucl <- NA_real_
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
# Handle any groups with missing TRTA or ADY (SAS missing sorts first)
# Add them if they exist and were missed
miss_rows <- advs[is.na(advs$TRTA) | is.na(advs$ADY), , drop = FALSE]
if (nrow(miss_rows) > 0) {
  # group by TRTA/ADY including NA using paste key
  advs$key <- paste0(ifelse(is.na(advs$TRTA), "<NA>", advs$TRTA), "|", ifelse(is.na(advs$ADY), "<NA>", as.character(advs$ADY)))
  uk <- unique(advs$key)
  extra <- list()
  for (k in uk) {
    parts <- strsplit(k, "\\|", fixed = FALSE)[[1]]
    # check if already in uniq
    trk <- if (parts[1] == "<NA>") NA_character_ else parts[1]
    adk <- if (parts[2] == "<NA>") NA_real_ else suppressWarnings(as.numeric(parts[2]))
    already <- any(!is.na(uniq$TRTA) & uniq$TRTA == trk & !is.na(uniq$ADY) & uniq$ADY == adk, na.rm = TRUE)
    if (is.na(trk) || is.na(adk)) {
      if (!already) {
        v <- advs$AVAL[advs$key == k]
        vnn <- v[!is.na(v)]
        n <- length(vnn)
        if (n == 0) { mn <- NA_real_; lcl <- NA_real_; ucl <- NA_real_ }
        else if (n == 1) { mn <- vnn[1]; lcl <- NA_real_; ucl <- NA_real_ }
        else { mn <- mean(vnn); s <- sd(vnn); se <- s/sqrt(n); tcrit <- qt(0.975, n-1); lcl <- mn - tcrit*se; ucl <- mn + tcrit*se }
        extra[[length(extra)+1]] <- data.frame(TRTA=trk, ADY=adk, MEAN=mn, NUM=n, LCLM=lcl, UCLM=ucl, stringsAsFactors=FALSE)
      }
    }
  }
  if (length(extra) > 0) meanvs_list <- c(meanvs_list, extra)
  advs$key <- NULL
}
meanvs <- do.call(rbind, meanvs_list)
# SAS sort order: character uppercase before lowercase, missing first
# order by TRTA then ADY with missing first
meanvs <- meanvs[order(ifelse(is.na(meanvs$TRTA), "\u0000", meanvs$TRTA), ifelse(is.na(meanvs$ADY), -Inf, meanvs$ADY), na.last = FALSE), , drop = FALSE]
rownames(meanvs) <- NULL

# data mean; set meanvs; keep trta ady mean;
mean_df <- meanvs[, c("TRTA","ADY","MEAN"), drop = FALSE]
# data num; set meanvs; keep trta ady num;
num_df <- meanvs[, c("TRTA","ADY","NUM"), drop = FALSE]

# data ci; set meanvs; keep trta ady lclm uclm; + offset
ci <- meanvs[, c("TRTA","ADY","LCLM","UCLM"), drop = FALSE]
ci$ADY <- ifelse(ci$TRTA == "Xanomeline Low Dose", ci$ADY,
          ifelse(ci$TRTA == "Xanomeline High Dose", ci$ADY + 0.3,
          ifelse(ci$TRTA == "Placebo", ci$ADY + 0.6, ci$ADY)))
# proc sort; by trta ady;
ci <- ci[order(ifelse(is.na(ci$TRTA), "\u0000", ci$TRTA), ifelse(is.na(ci$ADY), -Inf, ci$ADY), na.last = FALSE), , drop = FALSE]
rownames(ci) <- NULL

# ---- PROC PRINT data=ci -> P055 listing ----
# Write CSV of table body
write.csv(ci, file.path(out, "P055.csv"), row.names = FALSE, na = "")

fmt_best <- function(x) {
  if (is.na(x)) return(".")
  # emulate SAS BEST12.: up to 12 significant, strip trailing zeros
  s <- format(x, digits = 12, scientific = FALSE, trim = TRUE, nsmall = 0)
  # format may produce leading spaces; trim
  s <- trimws(s)
  return(s)
}
fmt_ady <- function(x) {
  if (is.na(x)) return(".")
  # ADY offset gives .3/.6 decimals; print without unnecessary zeros
  if (abs(x - round(x)) < 1e-9) return(as.character(as.integer(round(x))))
  s <- format(x, digits = 12, scientific = FALSE, trim = TRUE)
  return(trimws(s))
}

txt_path1 <- file.path(out, "P055.txt")
txt_path2 <- file.path(out, "P055.lst")
con_lines <- c()
con_lines <- c(con_lines, "Obs  TRTA                    ADY    LCLM         UCLM")
con_lines <- c(con_lines, "---  ----------------------  -----  -----------  -----------")
for (i in seq_len(nrow(ci))) {
  obs_s <- sprintf("%3d", i)
  trta_s <- sprintf("%-22s", ifelse(is.na(ci$TRTA[i]), "", ci$TRTA[i]))
  ady_s <- fmt_ady(ci$ADY[i])
  lcl_s <- fmt_best(ci$LCLM[i])
  ucl_s <- fmt_best(ci$UCLM[i])
  # right-align numeric fields similar to SAS
  line <- sprintf("%s  %s  %5s  %11s  %11s", obs_s, trta_s, ady_s, lcl_s, ucl_s)
  con_lines <- c(con_lines, line)
}
writeLines(con_lines, txt_path1)
writeLines(con_lines, txt_path2)

# ---- Build anno logic for plot data ----
# anno error bars use ci (offset ADY); anno2 N labels use num (original ADY)
# final: set mean num with offset
mean_off <- mean_df
mean_off$ADY_PLOT <- ifelse(mean_off$TRTA == "Xanomeline Low Dose", mean_off$ADY,
                     ifelse(mean_off$TRTA == "Xanomeline High Dose", mean_off$ADY + 0.3,
                     ifelse(mean_off$TRTA == "Placebo", mean_off$ADY + 0.6, mean_off$ADY)))
num_off <- num_df
num_off$ADY_PLOT <- ifelse(num_off$TRTA == "Xanomeline Low Dose", num_off$ADY,
                    ifelse(num_off$TRTA == "Xanomeline High Dose", num_off$ADY + 0.3,
                    ifelse(num_off$TRTA == "Placebo", num_off$ADY + 0.6, num_off$ADY)))

final_mean <- mean_off
final_mean <- final_mean[order(final_mean$TRTA, final_mean$ADY_PLOT), , drop = FALSE]

ci_plot <- ci
names(ci_plot)[names(ci_plot) == "ADY"] <- "ADY_PLOT"

# N annotation positions (SAS anno2): Placebo y=-170, High y=-180, Low y=-190, x=ady original
num_anno <- num_df
num_anno$X <- num_anno$ADY
num_anno$Y <- ifelse(num_anno$TRTA == "Placebo", -170,
              ifelse(num_anno$TRTA == "Xanomeline High Dose", -180,
              ifelse(num_anno$TRTA == "Xanomeline Low Dose", -190, NA_real_)))

# ---- CSV of plotted data ----
plot_data <- merge(mean_off, num_df, by = c("TRTA","ADY"), all = TRUE)
plot_data <- merge(plot_data, meanvs[, c("TRTA","ADY","LCLM","UCLM")], by = c("TRTA","ADY"), all.x = TRUE)
# add offset ADY for CI/mean line
plot_data$ADY_PLOT <- ifelse(plot_data$TRTA == "Xanomeline Low Dose", plot_data$ADY,
                      ifelse(plot_data$TRTA == "Xanomeline High Dose", plot_data$ADY + 0.3,
                      ifelse(plot_data$TRTA == "Placebo", plot_data$ADY + 0.6, plot_data$ADY)))
plot_data <- plot_data[order(plot_data$TRTA, plot_data$ADY), , drop = FALSE]
# order columns
plot_data <- plot_data[, c("TRTA","ADY","ADY_PLOT","MEAN","NUM","LCLM","UCLM")]
write.csv(plot_data, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

# ---- Figure: GPLOT mean*ady=trta with annotate error bars and N ----
# symbol1 blue dot (Placebo), symbol2 red triangle (High), symbol3 green star (Low)
# axis1 order -200 to 350 by 50, label Systolic blood pressure (vertical)
# axis2 order 0 to 250 by 50, label Visit number (visit day)
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
```
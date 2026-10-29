```r
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(grid)
})

adpc_path_raw <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt"
adpc_path_der <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpc.xpt"
adpc_path <- adpc_path_raw
if (!file.exists(adpc_path) && file.exists(adpc_path_der)) adpc_path <- adpc_path_der
if (!file.exists(adpc_path)) {
  cand <- list.files("D:/meta_hackathon/sas2r-agent/data", pattern = "^adpc\\.xpt$",
                     recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) adpc_path <- cand[1]
}
message("Reading ADPC from: ", adpc_path)

adpc <- haven::read_xpt(adpc_path)

# normalize names to lower for processing (matching is case-insensitive)
names(adpc) <- tolower(names(adpc))

# helper: SAS COMPRESS(source, chars) - remove any char appearing in chars
sas_compress <- function(x, chars) {
  ch_vec <- unique(strsplit(chars, "", fixed = TRUE)[[1]])
  res <- as.character(x)
  # preserve NA
  isna <- is.na(res)
  for (ch in ch_vec) {
    idx <- !is.na(res)
    if (any(idx)) {
      res[idx] <- gsub(ch, "", res[idx], fixed = TRUE)
    }
  }
  res[isna] <- NA_character_
  res
}

# ensure needed columns exist
getcol <- function(df, nm) {
  if (nm %in% names(df)) df[[nm]] else rep(NA, nrow(df))
}
to_char <- function(v) {
  if (inherits(v, "labelled")) v <- haven::as_factor(v, levels = "labels")
  if (inherits(v, "factor")) return(as.character(v))
  if (inherits(v, "POSIXt")) return(format(v, "%Y-%m-%dT%H:%M:%S"))
  if (inherits(v, "Date")) return(as.character(v))
  as.character(v)
}
to_num <- function(v) {
  if (inherits(v, "labelled")) v <- as.numeric(v)
  suppressWarnings(as.numeric(v))
}

usubjid  <- to_char(getcol(adpc, "usubjid"))
trta     <- to_char(getcol(adpc, "trta"))
aperiodc <- to_char(getcol(adpc, "aperiodc"))
atpt     <- to_char(getcol(adpc, "atpt"))
parcat3l <- to_char(getcol(adpc, "parcat3l"))
parcat1  <- to_char(getcol(adpc, "parcat1"))
aval     <- to_num(getcol(adpc, "aval"))
pclloq   <- to_num(getcol(adpc, "pclloq"))
saffn    <- to_num(getcol(adpc, "saffn"))
pkfn     <- to_num(getcol(adpc, "pkfn"))
paexcfln <- to_char(getcol(adpc, "paexcfln"))
# adtm keep original for output
adtm_orig <- getcol(adpc, "adtm")

# t=input(compress(atpt,' HOURSPOSTDOSE'),best.);
t_str <- sas_compress(atpt, " HOURSPOSTDOSE")
t_num <- suppressWarnings(as.numeric(t_str))
# SAS INPUT returns missing for "" / non-numeric; as.numeric does same
t_num[t_str == ""] <- NA_real_
t_num[is.na(t_str)] <- NA_real_

# order=input(compress(parcat1,' DRUGANALPlasma'),best.);
ord_str <- sas_compress(parcat1, " DRUGANALPlasma")
ord_num <- suppressWarnings(as.numeric(ord_str))
ord_num[ord_str == ""] <- NA_real_
ord_num[is.na(ord_str)] <- NA_real_

# concen=aval; if pclloq ne . then concen=pclloq;
concen <- aval
idx_lloq <- !is.na(pclloq)
concen[idx_lloq] <- pclloq[idx_lloq]

conc <- data.frame(
  usubjid = usubjid,
  trta = trta,
  aperiodc = aperiodc,
  atpt = atpt,
  parcat3l = parcat3l,
  aval = aval,
  adtm = adtm_orig,
  saffn = saffn,
  pkfn = pkfn,
  paexcfln = paexcfln,
  pclloq = pclloq,
  parcat1 = parcat1,
  t = t_num,
  concen = concen,
  order = ord_num,
  stringsAsFactors = FALSE
)

# proc sort data=conc; by usubjid t; run;  (missing first, SAS char order)
# use base order with na.last=FALSE
o <- order(conc$usubjid, conc$t, na.last = FALSE)
conc <- conc[o, , drop = FALSE]
rownames(conc) <- NULL

# format dates for CSV output
fmt_df_for_csv <- function(df) {
  df2 <- df
  for (nm in names(df2)) {
    v <- df2[[nm]]
    if (inherits(v, "POSIXt")) {
      df2[[nm]] <- format(v, "%Y-%m-%dT%H:%M:%S")
    } else if (inherits(v, "Date")) {
      df2[[nm]] <- format(v, "%Y-%m-%d")
    }
  }
  df2
}
conc_out <- fmt_df_for_csv(conc)
write.csv(conc_out, file.path(out, "conc.csv"), row.names = FALSE, na = "")

message(paste0("conc rows: ", nrow(conc)))

# define 6 combinations in SAS ODS order:
# %plot(trta=A) i=1..3 then %plot(trta=C) i=1..3
combos <- list(
  list(trta = "A", ord = 1, suffix = ""),
  list(trta = "A", ord = 2, suffix = "1"),
  list(trta = "A", ord = 3, suffix = "2"),
  list(trta = "C", ord = 1, suffix = "3"),
  list(trta = "C", ord = 2, suffix = "4"),
  list(trta = "C", ord = 3, suffix = "5")
)

build_plot <- function(dat, title_txt, ylab_txt, logscale) {
  if (nrow(dat) == 0) {
    p <- ggplot() + theme_bw(base_size = 9) +
      labs(title = title_txt, x = "Time", y = ylab_txt) +
      theme(plot.title = element_text(hjust = 0.5, size = 10))
    if (isTRUE(logscale)) p <- p + scale_y_log10()
    return(p)
  }
  p <- ggplot(dat, aes(x = t, y = concen, group = usubjid)) +
    geom_line(color = "black", linewidth = 0.4, na.rm = TRUE) +
    labs(title = title_txt, x = "Time", y = ylab_txt) +
    theme_bw(base_size = 9) +
    theme(plot.title = element_text(hjust = 0.5, size = 10))
  if (isTRUE(logscale)) p <- p + scale_y_log10()
  p
}

for (k in seq_along(combos)) {
  tr <- combos[[k]]$trta
  io <- combos[[k]]$ord
  sx <- combos[[k]]$suffix
  base <- paste0("sgrender", sx)

  # where order=&I and trta="&TRTA";
  sub <- conc[!is.na(conc$order) & !is.na(conc$trta) &
                conc$order == io & conc$trta == tr, , drop = FALSE]
  # sort by usubjid t for rendering
  if (nrow(sub) > 0) {
    sub <- sub[order(sub$usubjid, sub$t, na.last = FALSE), , drop = FALSE]
  }

  # CSV of plotted data (input to SGRender)
  sub_out <- fmt_df_for_csv(sub)
  write.csv(sub_out, file.path(out, paste0(base, ".csv")), row.names = FALSE, na = "")
  message(paste0(base, ": trta=", tr, " order=", io, " n=", nrow(sub)))

  # data actually drawn
  lin_dat <- sub[!is.na(sub$t) & !is.na(sub$concen), , drop = FALSE]
  log_dat <- sub[!is.na(sub$t) & !is.na(sub$concen) & sub$concen > 0, , drop = FALSE]

  p_linear <- build_plot(lin_dat, "Linear view", "Concentration (ug/mL)", FALSE)
  p_log    <- build_plot(log_dat, "Semilogarithmic view", " ", TRUE)

  gLinear <- ggplotGrob(p_linear)
  gLog    <- ggplotGrob(p_log)

  png(file.path(out, paste0(base, ".png")),
      width = 24, height = 12, units = "cm", res = 300)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 2,
    heights = unit(c(0.10, 0.08, 0.82), "npc"))))
  grid.text("Overlaid PK Concentration Plots",
            vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2),
            gp = gpar(fontsize = 13, fontface = "bold"))
  grid.text(paste0("Analyte: ", io, " Treatment=", tr),
            vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2),
            gp = gpar(fontsize = 11))
  pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1))
  grid.draw(gLinear)
  popViewport()
  pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 2))
  grid.draw(gLog)
  popViewport()
  popViewport()
  dev.off()
}
message("Done")
```
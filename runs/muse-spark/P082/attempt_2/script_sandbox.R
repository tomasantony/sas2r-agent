# P082 f_waterfall.sas -> R translation
# Figure 14.2.3 Waterfall Plot of Best Percentage Change from Baseline

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
})

adampath <- "/bench/data/raw/own-synthetic/adam"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

adam_files <- list.files(adampath, full.names = TRUE)
cat("ADAM files:\n"); cat(paste(basename(adam_files), collapse="\n"), "\n")

find_adam <- function(name) {
  idx <- which(tolower(basename(adam_files)) %in% paste0(tolower(name), c(".sas7bdat",".xpt","")))
  if (length(idx)==0) idx <- grep(paste0("(?i)^", name), basename(adam_files))
  if (length(idx)==0) return(NULL)
  adam_files[idx[1]]
}
read_adam <- function(name) {
  f <- find_adam(name)
  if (is.null(f)) { cat(paste0("NOTE: ", name, " not found\n")); return(NULL) }
  cat(paste0("Reading ", f, "\n"))
  if (grepl("\\.xpt$", f, ignore.case=TRUE)) haven::read_xpt(f)
  else haven::read_sas(f)
}

adtr <- read_adam("adtr")
adsl <- read_adam("adsl")
adrs <- NULL

# ---- Step 1: best percent change per subject (SAS PROC SQL) ----
tumor <- NULL
if (!is.null(adtr) && !is.null(adsl)) {
  names(adtr) <- toupper(names(adtr))
  names(adsl) <- toupper(names(adsl))
  cat("ADTR cols:", paste(names(adtr), collapse=","), "\n")
  cat("ADSL cols:", paste(names(adsl), collapse=","), "\n")
  if (all(c("USUBJID","PARAMCD","PCHG") %in% names(adtr)) && "USUBJID" %in% names(adsl)) {
    sl_sub <- adsl %>% select(USUBJID, any_of(c("EFFFL")))
    if ("EFFFL" %in% names(sl_sub)) {
      sl_sub <- sl_sub %>% filter(trimws(EFFFL) == "Y")
    }
    tr_sub <- adtr %>% filter(trimws(PARAMCD) == "BPCHG")
    keep_a <- intersect(c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","PCHG","AVALC","AVAL","CHG","BOR"), names(tr_sub))
    tr_sub <- tr_sub %>% select(any_of(keep_a))
    tumor <- inner_join(tr_sub, sl_sub, by="USUBJID")
    tumor <- tumor %>% arrange(is.na(PCHG), PCHG)
  }
}
if (is.null(tumor) || nrow(tumor)==0) {
  if (is.null(adrs)) adrs <- read_adam("adrs")
  if (!is.null(adrs) && !is.null(adsl)) {
    names(adrs) <- toupper(names(adrs))
    if (!"USUBJID" %in% names(adsl)) names(adsl) <- toupper(names(adsl))
    sl2 <- adsl %>% select(USUBJID, any_of(c("EFFFL")))
    if ("EFFFL" %in% names(sl2)) sl2 <- sl2 %>% filter(trimws(EFFFL)=="Y")
    adrs_f <- adrs
    if ("PARAMCD" %in% names(adrs_f)) adrs_f <- adrs_f %>% filter(trimws(PARAMCD)=="SUMLD")
    j <- inner_join(adrs_f, sl2 %>% select(USUBJID), by="USUBJID")
    if ("PCHG" %in% names(j)) {
      tumor <- j %>% group_by(USUBJID) %>%
        summarise(TRTA=dplyr::first(TRTA), TRTAN=dplyr::first(TRTAN),
                  PARAMCD="BPCHG", PARAM=dplyr::first(PARAM),
                  PCHG=suppressWarnings(min(PCHG, na.rm=TRUE)),
                  AVALC=dplyr::first(AVALC), .groups="drop")
      tumor$PCHG[is.infinite(tumor$PCHG)] <- NA_real_
      tumor$EFFFL <- "Y"
      tumor <- tumor %>% arrange(is.na(PCHG), PCHG)
    }
  }
}
if (is.null(tumor)) {
  tumor <- data.frame(USUBJID=character(0), TRTA=character(0), TRTAN=numeric(0),
                      PARAMCD=character(0), PARAM=character(0), PCHG=numeric(0),
                      AVALC=character(0), EFFFL=character(0), stringsAsFactors=FALSE)
}
cat(paste0("tumor N=", nrow(tumor), "\n"))

# ---- Step 2: display order + colors + capping ----
waterfall <- tumor
if (nrow(waterfall)>0) {
  waterfall <- waterfall %>% arrange(is.na(PCHG), PCHG)
  waterfall$bar_order <- seq_len(nrow(waterfall))
  up <- toupper(trimws(ifelse(is.na(waterfall$AVALC), "", waterfall$AVALC)))
  waterfall$bar_color <- dplyr::case_when(
    up=="CR" ~ "darkgreen",
    up=="PR" ~ "green",
    up=="SD" ~ "gold",
    up=="PD" ~ "red",
    TRUE ~ "gray"
  )
  waterfall$PCHG_display <- ifelse(is.na(waterfall$PCHG), NA_real_,
    ifelse(waterfall$PCHG > 100, 100,
      ifelse(waterfall$PCHG < -100, -100, waterfall$PCHG)))
} else {
  waterfall$bar_order <- numeric(0)
  waterfall$bar_color <- character(0)
  waterfall$PCHG_display <- numeric(0)
}
if (!"AVALC" %in% names(waterfall)) waterfall$AVALC <- NA_character_
waterfall$AVALC_GRP <- ifelse(is.na(waterfall$AVALC) | trimws(waterfall$AVALC)=="", "Missing", trimws(waterfall$AVALC))
desired <- c("CR","PR","SD","PD")
other_lvls <- sort(unique(setdiff(waterfall$AVALC_GRP, c(desired,"Missing"))))
lvl <- c(intersect(desired, unique(waterfall$AVALC_GRP)), other_lvls,
         if ("Missing" %in% waterfall$AVALC_GRP) "Missing" else NULL)
if (length(lvl)==0) lvl <- "Missing"
waterfall$AVALC_GRP <- factor(waterfall$AVALC_GRP, levels=lvl)
col_map <- c(CR="darkgreen", PR="green", SD="gold", PD="red", Missing="gray")
for (l in lvl) if (!(l %in% names(col_map))) col_map[l] <- "gray"
col_map <- col_map[lvl]

# treatment levels sorted SAS-style (uppercase before lowercase, byte order)
trt_lvls <- sort(unique(as.character(waterfall$TRTA)))
if (length(trt_lvls)==0) trt_lvls <- character(0)

# ---- Step 3: waterfall plot ----
plot_data <- waterfall
if (nrow(plot_data)>0 && sum(!is.na(plot_data$PCHG_display))>0) {
  xmax <- max(plot_data$bar_order, na.rm=TRUE)
  p <- ggplot(plot_data, aes(x=bar_order, y=PCHG_display, fill=AVALC_GRP)) +
    geom_col(width=1, colour="black", linewidth=0.25, na.rm=TRUE) +
    scale_fill_manual(values=col_map, name="Best Overall Response", drop=FALSE) +
    geom_hline(yintercept=0, colour="black", linewidth=0.5) +
    geom_hline(yintercept=-30, colour="blue", linetype="dashed", linewidth=0.6) +
    geom_hline(yintercept=20, colour="red", linetype="dashed", linewidth=0.6) +
    annotate("text", x=xmax, y=-30+3, label="-30% (PR threshold)", size=2.8, hjust=1, colour="blue") +
    annotate("text", x=xmax, y=20+3, label="+20% (PD threshold)", size=2.8, hjust=1, colour="red") +
    scale_x_continuous(expand=c(0,0.5)) +
    scale_y_continuous(name="Best Percentage Change from Baseline (%)",
                       limits=c(-100,100), breaks=seq(-100,100,by=20)) +
    theme_bw(base_size=10) +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(),
          panel.grid.major.y=element_line(linetype="dotted", colour="lightgray"),
          panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(),
          legend.position="bottom",
          plot.title=element_text(hjust=0.5, face="bold", size=11),
          plot.subtitle=element_text(hjust=0.5, size=10)) +
    labs(title="Figure 14.2.3\nWaterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions",
         subtitle="Efficacy Analysis Set", x="")
} else {
  p <- ggplot() + annotate("text", x=0, y=0, label="No data for Efficacy Analysis Set (EFFFL='Y', PARAMCD='BPCHG')") +
    theme_void() + labs(title="Figure 14.2.3 Waterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions")
}
ggsave(file.path(out,"f_waterfall.png"), plot=p, width=10, height=6.5, units="in", dpi=300)

# ---- CSV of plotted data ----
csv_data <- waterfall %>%
  select(any_of(c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","AVAL","CHG","PCHG","PCHG_display","AVALC","BOR","EFFFL","bar_order","bar_color")))
write.csv(csv_data, file.path(out,"f_waterfall.csv"), row.names=FALSE, na="")

# ---- Summaries for text rendering ----
n_subj <- nrow(waterfall)
fmt_pct1 <- function(n, pct) {
  if (is.na(n)) return("")
  if (is.na(pct)) return(as.character(n))
  sprintf("%d (%5.1f%%)", n, round_half_up(pct,1))
}
fmt2 <- function(x) ifelse(is.na(x), "", sprintf("%.2f", round_half_up(x,2)))
fmt1 <- function(x) ifelse(is.na(x), "", sprintf("%.1f", round_half_up(x,1)))

summ_stats <- function(v) {
  v <- v[!is.na(v)]
  if (length(v)==0) return(list(n=0, mean=NA_real_, sd=NA_real_, median=NA_real_, q1=NA_real_, q3=NA_real_, min=NA_real_, max=NA_real_))
  list(n=length(v), mean=mean(v), sd=if(length(v)>1) sd(v) else NA_real_,
       median=median(v), q1=quantile(v,0.25, names=FALSE, type=2), q3=quantile(v,0.75, names=FALSE, type=2),
       min=min(v), max=max(v))
}
overall <- summ_stats(waterfall$PCHG_display)
n_pr <- sum(waterfall$PCHG_display <= -30, na.rm=TRUE)
n_pd <- sum(waterfall$PCHG_display >= 20, na.rm=TRUE)
pct_pr <- ifelse(n_subj>0, 100*n_pr/n_subj, NA_real_)
pct_pd <- ifelse(n_subj>0, 100*n_pd/n_subj, NA_real_)

if (n_subj>0) {
  resp_tab <- waterfall %>% mutate(AVALC_DISP=as.character(AVALC_GRP)) %>%
    group_by(AVALC_DISP) %>% summarise(N=n(), .groups="drop") %>%
    mutate(PCT=ifelse(n_subj>0, 100*N/n_subj, NA_real_)) %>%
    arrange(match(AVALC_DISP, lvl))
} else {
  resp_tab <- data.frame(AVALC_DISP=character(0), N=numeric(0), PCT=numeric(0))
}

# per-treatment summaries
trt_tab <- NULL
if (length(trt_lvls)>0) {
  trt_tab <- data.frame(TRTA=trt_lvls, stringsAsFactors=FALSE) %>%
    rowwise() %>%
    mutate(N=sum(as.character(waterfall$TRTA)==TRTA, na.rm=TRUE),
           PCT=ifelse(n_subj>0, 100*N/n_subj, NA_real_)) %>% ungroup()
}

lines <- c(
  "[Sponsor Name]                                                  Page 1 of 1",
  "Protocol: [Protocol Number]",
  "",
  "              Figure 14.2.3",
  "  Waterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions",
  "                  Efficacy Analysis Set",
  "",
  sprintf("  Number of subjects plotted (EFFFL=Y, PARAMCD=BPCHG): %d", n_subj),
  sprintf("  Number with non-missing PCHG: %d", overall$n),
  ""
)
lines <- c(lines, "  Best Overall Response (AVALC):", "  Response      N      N (%)")
if (nrow(resp_tab)>0) {
  for (i in seq_len(nrow(resp_tab)))
    lines <- c(lines, sprintf("  %-10s %4d   %s", resp_tab$AVALC_DISP[i], resp_tab$N[i], fmt_pct1(resp_tab$N[i], resp_tab$PCT[i])))
}
lines <- c(lines, "")
if (!is.null(trt_tab) && nrow(trt_tab)>0) {
  lines <- c(lines, "  By treatment (TRTA):", "  Treatment                        N      N (%)")
  for (i in seq_len(nrow(trt_tab)))
    lines <- c(lines, sprintf("  %-30s %4d   %s", substr(trt_tab$TRTA[i],1,30), trt_tab$N[i], fmt_pct1(trt_tab$N[i], trt_tab$PCT[i])))
  lines <- c(lines, "")
  # per-treatment PCHG stats
  lines <- c(lines, "  Best Percentage Change (%) by treatment (capped at +/-100 for display):")
  for (t in trt_lvls) {
    s <- summ_stats(waterfall$PCHG_display[as.character(waterfall$TRTA)==t])
    nn_pr <- sum(waterfall$PCHG_display[as.character(waterfall$TRTA)==t] <= -30, na.rm=TRUE)
    nn_pd <- sum(waterfall$PCHG_display[as.character(waterfall$TRTA)==t] >= 20, na.rm=TRUE)
    lines <- c(lines,
      sprintf("    %s: n=%d Mean(SD)=%s (%s) Median=%s Q1,Q3=%s,%s Min,Max=%s,%s <=-30%%: %s >=+20%%: %s",
        substr(t,1,30), s$n, fmt2(s$mean), fmt2(s$sd), fmt2(s$median), fmt2(s$q1), fmt2(s$q3),
        fmt1(s$min), fmt1(s$max),
        fmt_pct1(nn_pr, ifelse(sum(as.character(waterfall$TRTA)==t)>0, 100*nn_pr/sum(as.character(waterfall$TRTA)==t), NA)),
        fmt_pct1(nn_pd, ifelse(sum(as.character(waterfall$TRTA)==t)>0, 100*nn_pd/sum(as.character(waterfall$TRTA)==t), NA))))
  }
  lines <- c(lines, "")
}
lines <- c(lines,
  "  Best Percentage Change from Baseline (%, capped at +/-100 for display) - Overall:",
  sprintf("    N                 : %d", overall$n),
  sprintf("    Mean (SD)         : %s (%s)", fmt2(overall$mean), fmt2(overall$sd)),
  sprintf("    Median            : %s", fmt2(overall$median)),
  sprintf("    Q1, Q3            : %s, %s", fmt2(overall$q1), fmt2(overall$q3)),
  sprintf("    Min, Max          : %s, %s", fmt1(overall$min), fmt1(overall$max)),
  sprintf("    Mean (1 decimal)  : %s", fmt1(overall$mean)),
  sprintf("    Median (1 decimal): %s", fmt1(overall$median)),
  "",
  sprintf("  Subjects with >=30%% shrinkage (PR threshold, PCHG <= -30): %s", fmt_pct1(n_pr, pct_pr)),
  sprintf("  Subjects with >=20%% growth (PD threshold, PCHG >= +20)   : %s", fmt_pct1(n_pd, pct_pd)),
  "",
  "  Each bar represents one subject (sorted by PCHG; bar_order 1..N).",
  "  Reference lines: -30% (PR threshold, blue dashed), +20% (PD threshold, red dashed), 0 (black).",
  "  Y-axis values -100 to 100 by 20 with dotted grid.",
  "  CR=Complete Response, PR=Partial Response, SD=Stable Disease, PD=Progressive Disease",
  "",
  "  Data cutoff: [DDMONYYYY].",
  "  Source: ADaM datasets (ADTR PARAMCD=BPCHG joined to ADSL EFFFL=Y).",
  "  See f_waterfall.png for figure and f_waterfall.csv for plotted data."
)
writeLines(lines, file.path(out,"f_waterfall.txt"))

# ---- CSV of table body ----
body_rows <- list()
if (nrow(resp_tab)>0) {
  for (i in seq_len(nrow(resp_tab)))
    body_rows[[length(body_rows)+1]] <- data.frame(Category=paste0("BOR=", resp_tab$AVALC_DISP[i]),
      N=resp_tab$N[i], Percent=round_half_up(resp_tab$PCT[i],1),
      Statistic=fmt_pct1(resp_tab$N[i], resp_tab$PCT[i]), stringsAsFactors=FALSE)
}
body_rows[[length(body_rows)+1]] <- data.frame(Category="PCHG n", N=overall$n, Percent=NA_real_, Statistic=as.character(overall$n), stringsAsFactors=FALSE)
body_rows[[length(body_rows)+1]] <- data.frame(Category="PCHG Mean (SD)", N=overall$n, Percent=NA_real_, Statistic=sprintf("%s (%s)", fmt2(overall$mean), fmt2(overall$sd)), stringsAsFactors=FALSE)
body_rows[[length(body_rows)+1]] <- data.frame(Category="PCHG Median", N=overall$n, Percent=NA_real_, Statistic=fmt2(overall$median), stringsAsFactors=FALSE)
body_rows[[length(body_rows)+1]] <- data.frame(Category="PCHG Q1, Q3", N=overall$n, Percent=NA_real_, Statistic=sprintf("%s, %s", fmt2(overall$q1), fmt2(overall$q3)), stringsAsFactors=FALSE)
body_rows[[length(body_rows)+1]] <- data.frame(Category="PCHG Min, Max", N=overall$n, Percent=NA_real_, Statistic=sprintf("%s, %s", fmt1(overall$min), fmt1(overall$max)), stringsAsFactors=FALSE)
body_rows[[length(body_rows)+1]] <- data.frame(Category="PCHG <= -30% (PR threshold)", N=n_pr, Percent=round_half_up(pct_pr,1), Statistic=fmt_pct1(n_pr, pct_pr), stringsAsFactors=FALSE)
body_rows[[length(body_rows)+1]] <- data.frame(Category="PCHG >= +20% (PD threshold)", N=n_pd, Percent=round_half_up(pct_pd,1), Statistic=fmt_pct1(n_pd, pct_pd), stringsAsFactors=FALSE)
if (!is.null(trt_tab) && nrow(trt_tab)>0) {
  for (i in seq_len(nrow(trt_tab)))
    body_rows[[length(body_rows)+1]] <- data.frame(Category=paste0("TRTA=", trt_tab$TRTA[i]),
      N=trt_tab$N[i], Percent=round_half_up(trt_tab$PCT[i],1),
      Statistic=fmt_pct1(trt_tab$N[i], trt_tab$PCT[i]), stringsAsFactors=FALSE)
}
table_body <- do.call(rbind, body_rows)
write.csv(table_body, file.path(out,"f_waterfall_table.csv"), row.names=FALSE, na="")

cat("Done. N subjects:", n_subj, "\n")

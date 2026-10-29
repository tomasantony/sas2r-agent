```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

cat("Reading ADSL and ADAE...\n")
adsl_raw <- haven::read_xpt(file.path(rawadam, "adsl.xpt"))
adae_raw <- haven::read_xpt(file.path(rawadam, "adae.xpt"))

names(adsl_raw) <- toupper(names(adsl_raw))
names(adae_raw) <- toupper(names(adae_raw))

adsl <- as.data.frame(adsl_raw, stringsAsFactors = FALSE)
adae_full <- as.data.frame(adae_raw, stringsAsFactors = FALSE)

keepvars <- c("USUBJID","TRTAN","AEDECOD","TRTEMFL","AOCCPFL","SAFFL")
keepvars <- keepvars[keepvars %in% names(adae_full)]
adae <- adae_full[, keepvars, drop = FALSE]

to_char <- function(x) {
  if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
  as.character(x)
}
to_num <- function(x) {
  if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
  suppressWarnings(as.numeric(x))
}
if ("USUBJID" %in% names(adae)) adae$USUBJID <- to_char(adae$USUBJID)
if ("AEDECOD" %in% names(adae)) adae$AEDECOD <- to_char(adae$AEDECOD)
if ("TRTEMFL" %in% names(adae)) adae$TRTEMFL <- to_char(adae$TRTEMFL)
if ("AOCCPFL" %in% names(adae)) adae$AOCCPFL <- to_char(adae$AOCCPFL)
if ("SAFFL" %in% names(adae)) adae$SAFFL <- to_char(adae$SAFFL)
if ("TRTAN" %in% names(adae)) adae$TRTAN <- to_num(adae$TRTAN)

if ("USUBJID" %in% names(adsl)) adsl$USUBJID <- to_char(adsl$USUBJID)
if ("TRT01AN" %in% names(adsl)) adsl$TRT01AN <- to_num(adsl$TRT01AN)

# proc sort by aedecod
adae <- adae[order(adae$AEDECOD, na.last = FALSE, method = "radix"), , drop = FALSE]

# data adae1; where TRTEMFL="Y" and AOCCPFL='Y' and saffl="Y";
adae1 <- adae[!is.na(adae$TRTEMFL) & adae$TRTEMFL == "Y" &
              !is.na(adae$AOCCPFL) & adae$AOCCPFL == "Y" &
              !is.na(adae$SAFFL) & adae$SAFFL == "Y", , drop = FALSE]

cat(sprintf("ADAE1 rows: %d\n", nrow(adae1)))

suppressPackageStartupMessages(library(dplyr))
countae <- adae1 %>%
  group_by(TRTAN, AEDECOD) %>%
  summarise(numAE = sum(!is.na(USUBJID) & USUBJID != "", na.rm = TRUE), .groups = "drop") %>%
  as.data.frame()

nsubj <- adsl %>%
  group_by(TRT01AN) %>%
  summarise(numusubjid = dplyr::n_distinct(USUBJID[!is.na(USUBJID) & USUBJID != ""]), .groups = "drop") %>%
  as.data.frame()

cat(sprintf("countae rows: %d, nsubj rows: %d\n", nrow(countae), nrow(nsubj)))

# data count; merge countae nsubj(rename trt01an=trtan); by trtan; abort if no nsubj
if (nrow(countae) > 0) {
  missing_trt <- setdiff(unique(countae$TRTAN[!is.na(countae$TRTAN)]),
                         unique(nsubj$TRT01AN[!is.na(nsubj$TRT01AN)]))
  if (any(is.na(countae$TRTAN)) && !any(is.na(nsubj$TRT01AN))) {
    stop("abort cancel: TRTAN (missing) not in N-subject")
  }
  if (length(missing_trt) > 0) {
    stop(paste0("abort cancel: TRTAN not in N-subject: ", paste(missing_trt, collapse=",")))
  }
}

nsubj_ren <- nsubj
names(nsubj_ren)[names(nsubj_ren)=="TRT01AN"] <- "TRTAN"
count_joined <- merge(countae, nsubj_ren, by = "TRTAN", all.x = TRUE, sort = FALSE)

for (i in seq_len(nrow(nsubj_ren))) {
  tv <- nsubj_ren$TRTAN[i]
  if (is.na(tv)) {
    has <- any(is.na(countae$TRTAN))
  } else {
    has <- any(!is.na(countae$TRTAN) & countae$TRTAN == tv)
  }
  if (!has) {
    newrow <- data.frame(TRTAN = tv, AEDECOD = NA_character_, numAE = NA_real_,
                         numusubjid = nsubj_ren$numusubjid[i], stringsAsFactors = FALSE)
    count_joined <- rbind(count_joined, newrow)
  }
}

count_joined$AEpercent <- ifelse(!is.na(count_joined$numAE) & !is.na(count_joined$numusubjid),
                                 count_joined$numAE / count_joined$numusubjid, NA_real_)
count <- count_joined
count <- count[order(count$AEDECOD, count$TRTAN, na.last = FALSE, method = "radix"), , drop = FALSE]
rownames(count) <- NULL

# data tcount; merge count count(where trtan=0 renamed); by aedecod
placebo <- count[!is.na(count$TRTAN) & count$TRTAN == 0, c("AEDECOD","AEpercent","TRTAN"), drop = FALSE]
names(placebo)[names(placebo)=="AEpercent"] <- "AEpercent_1"
names(placebo)[names(placebo)=="TRTAN"] <- "TRTAN_1"
placebo <- placebo[!duplicated(placebo$AEDECOD), , drop = FALSE]
tcount <- merge(count, placebo, by = "AEDECOD", all.x = TRUE, sort = FALSE)
tcount <- tcount[order(tcount$AEDECOD, tcount$TRTAN, na.last = FALSE, method = "radix"), , drop = FALSE]
tcount$AEpercent[is.na(tcount$AEpercent)] <- 0
tcount$AEpercent_1[is.na(tcount$AEpercent_1)] <- 0
tcount$RelRisk <- tcount$AEpercent - tcount$AEpercent_1
tcount$aeseqno <- seq_len(nrow(tcount))
rownames(tcount) <- NULL

# maxpercen2: -max(AEpercent) per aedecod order by minusmax
maxpercen2 <- tcount %>%
  group_by(AEDECOD) %>%
  summarise(minusmaxAEPercent = if (all(is.na(AEpercent))) NA_real_ else -max(AEpercent, na.rm = TRUE),
            .groups = "drop") %>%
  as.data.frame()
maxpercen2 <- maxpercen2[order(maxpercen2$minusmaxAEPercent, maxpercen2$AEDECOD,
                               na.last = FALSE, method = "radix"), , drop = FALSE]
maxpercen2$dispseqno <- seq_len(nrow(maxpercen2))
maxpercen2 <- maxpercen2[order(maxpercen2$AEDECOD, na.last = FALSE, method = "radix"), , drop = FALSE]
rownames(maxpercen2) <- NULL

# tcount2; merge tcount maxpercen2; by aedecod
tcount2 <- merge(tcount, maxpercen2, by = "AEDECOD", all.x = TRUE, sort = FALSE)
tcount2 <- tcount2[order(tcount2$AEDECOD, tcount2$TRTAN, na.last = FALSE, method = "radix"), , drop = FALSE]
rownames(tcount2) <- NULL

# AEgrid: cartesian trtan x (aedecod,dispseqno)
d_trtan <- data.frame(TRTAN = sort(unique(tcount2$TRTAN), na.last = FALSE))
d_ae <- unique(tcount2[, c("AEDECOD","dispseqno"), drop = FALSE])
AEgrid <- merge(d_trtan, d_ae, by = NULL)
AEgrid <- AEgrid[order(AEgrid$AEDECOD, AEgrid$TRTAN, na.last = FALSE, method = "radix"), , drop = FALSE]
rownames(AEgrid) <- NULL

# tcount3; merge tcount2 AEgrid; by aedecod trtan
# FIX: avoid dispseqno.x/dispseqno.y duplication by dropping dispseqno from tcount2 side
# (values are identical per AEDECOD; AEgrid carries the authoritative dispseqno)
tcount2_nodisp <- tcount2[, setdiff(names(tcount2), "dispseqno"), drop = FALSE]
tcount3 <- merge(AEgrid, tcount2_nodisp,
                 by = c("AEDECOD","TRTAN"), all.x = TRUE, sort = FALSE)
tcount3 <- tcount3[order(tcount3$AEDECOD, tcount3$TRTAN, na.last = FALSE, method = "radix"), , drop = FALSE]
tcount3$AEpercent[is.na(tcount3$AEpercent)] <- 0
tcount3 <- tcount3[order(tcount3$dispseqno, tcount3$TRTAN, tcount3$AEDECOD,
                         na.last = FALSE, method = "radix"), , drop = FALSE]
rownames(tcount3) <- NULL

cat(sprintf("tcount3 rows: %d\n", nrow(tcount3)))

plot_data <- tcount3[!is.na(tcount3$dispseqno) & tcount3$dispseqno <= 20, , drop = FALSE]
plot_data <- plot_data[order(plot_data$dispseqno, plot_data$TRTAN, plot_data$AEDECOD,
                             na.last = FALSE, method = "radix"), , drop = FALSE]

write.csv(tcount3, file.path(out, "tcount3.csv"), row.names = FALSE, na = "")
write.csv(plot_data, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

sink(file.path(out, "sgplot.txt"))
cat("Common Treatment-Emergent Adverse Events\n")
cat("Scatter: X=AEpercent (AEs (%)) Y=dispseqno Group=Treatment (TRTAN)\n")
cat(sprintf("Plotted rows: %d (dispseqno<=20)\n\n", nrow(plot_data)))
print(as.data.frame(plot_data[, intersect(c("AEDECOD","TRTAN","numAE","numusubjid","AEpercent","AEpercent_1","RelRisk","minusmaxAEPercent","dispseqno","aeseqno"), names(plot_data))]))
sink()

suppressPackageStartupMessages(library(ggplot2))
plot_data$TRTAN_f <- factor(plot_data$TRTAN)
p <- ggplot(plot_data, aes(x = AEpercent, y = dispseqno, colour = TRTAN_f)) +
  geom_point(size = 2.5) +
  scale_y_continuous(breaks = 1:20, limits = c(1, 20)) +
  labs(title = "Common Treatment-Emergent Adverse Events",
       x = "AEs (%)", y = "dispseqno", colour = "Treatment") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_line(colour = "grey90"))
ggsave(file.path(out, "sgplot.png"), plot = p, width = 8, height = 6, dpi = 150)
cat("Done.\n")
```
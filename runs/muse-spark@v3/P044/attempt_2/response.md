```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

root <- "D:/meta_hackathon/sas2r-agent"
rawadam <- file.path(root, "data/raw/cdisc-pilot/adam")

adsl_path <- file.path(rawadam, "adsl.xpt")
adae_path <- file.path(rawadam, "adae.xpt")

adsl <- haven::read_xpt(adsl_path)
adae_raw <- haven::read_xpt(adae_path)

# work.adsl = source.adsl
work_adsl <- adsl

# work.adae keep USUBJID TRTAN AEDECOD TRTEMFL AOCCPFL SAFFL
work_adae <- adae_raw %>%
  select(USUBJID, TRTAN, AEDECOD, TRTEMFL, AOCCPFL, SAFFL)

# proc sort by aedecod (SAS: missing first)
work_adae <- work_adae[order(work_adae$AEDECOD, na.last = FALSE), , drop = FALSE]

# data adae1 where TRTEMFL="Y" and AOCCPFL='Y' and saffl="Y"
adae1 <- work_adae %>%
  filter(TRTEMFL == "Y", AOCCPFL == "Y", SAFFL == "Y")

# proc sql countae: count(usubjid) per trtan, aedecod
countae <- adae1 %>%
  group_by(TRTAN, AEDECOD) %>%
  summarise(numAE = sum(!is.na(USUBJID)), .groups = "drop") %>%
  rename(trtan = TRTAN, aedecod = AEDECOD)

# nsubj: count distinct usubjid per TRT01AN
nsubj <- work_adsl %>%
  group_by(TRT01AN) %>%
  summarise(numusubjid = n_distinct(USUBJID), .groups = "drop") %>%
  rename(trtan = TRT01AN)

# data count; merge countae nsubj(rename trt01an=trtan); by trtan;
# if not _nsubj then abort cancel
if (!all(countae$trtan %in% nsubj$trtan)) {
  stop("SAS ABORT CANCEL: countae trtan not in nsubj")
}
stopifnot("trtan" %in% names(countae), "trtan" %in% names(nsubj))

count <- full_join(countae, nsubj, by = "trtan")
# keep only rows where _nsubj (numusubjid not missing)
count <- count %>% filter(!is.na(numusubjid))

count <- count %>%
  mutate(AEpercent = ifelse(!is.na(numAE) & !is.na(numusubjid),
                            numAE / numusubjid, NA_real_))

# proc sort by aedecod
count <- count %>% arrange(aedecod, trtan)

# data tcount; merge count count(where trtan=0); by aedecod;
placebo <- count %>%
  filter(trtan == 0) %>%
  select(aedecod, AEpercent_1 = AEpercent) %>%
  distinct(aedecod, .keep_all = TRUE)

tcount <- left_join(count, placebo, by = "aedecod")
# trtan_1 in SAS: 0 where placebo exists else missing
tcount <- tcount %>%
  mutate(trtan_1 = ifelse(!is.na(AEpercent_1), 0, NA_real_))

tcount$AEpercent[is.na(tcount$AEpercent)] <- 0
tcount$AEpercent_1[is.na(tcount$AEpercent_1)] <- 0

tcount <- tcount %>%
  mutate(RelRisk = AEpercent - AEpercent_1) %>%
  arrange(aedecod, trtan) %>%
  mutate(aeseqno = seq_len(n()))

# proc sql maxpercen2: -max(AEpercent) per aedecod ordered
maxpercen2 <- tcount %>%
  group_by(aedecod) %>%
  summarise(minusmaxAEpercent = -max(AEpercent, na.rm = TRUE), .groups = "drop") %>%
  arrange(minusmaxAEpercent) %>%
  mutate(dispseqno = seq_len(n()))

# proc sort maxpercen2 by aedecod
maxpercen2_s <- maxpercen2 %>% arrange(aedecod)

# data tcount2; merge tcount maxpercen2; by aedecod;
tcount2 <- left_join(tcount, maxpercen2_s, by = "aedecod")

# AEgrid: cross join distinct trtan x distinct(aedecod,dispseqno)
d_trtan <- tcount2 %>% distinct(trtan)
d_ae <- tcount2 %>% distinct(aedecod, dispseqno)
AEgrid <- tidyr::crossing(d_ae, d_trtan) %>%
  arrange(aedecod, trtan)

# data tcount3; merge tcount2 AEgrid; by aedecod trtan;
tcount3 <- left_join(AEgrid, tcount2, by = c("aedecod", "dispseqno", "trtan"))
# fill missing AEpercent with 0 (SAS)
tcount3$AEpercent[is.na(tcount3$AEpercent)] <- 0

tcount3 <- tcount3 %>% arrange(dispseqno, aedecod, trtan)

# plotted subset where dispseqno <= 20
plot_data <- tcount3 %>% filter(!is.na(dispseqno), dispseqno <= 20)

# SGPlot: scatter x=AEpercent y=dispseqno / group=trtan; xaxis grid; yaxis grid values 1-20
plot_data$trtan_f <- factor(plot_data$trtan)

p <- ggplot(plot_data, aes(x = AEpercent, y = dispseqno, colour = trtan_f, group = trtan_f)) +
  geom_point(size = 2.5) +
  scale_y_continuous(breaks = 1:20, limits = c(1, 20)) +
  labs(title = "Common Treatment-Emergent Adverse Events",
       x = "AEs (%)",
       y = "dispseqno",
       colour = "Treatment") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_line(colour = "grey90"))

ggsave(file.path(out, "sgplot.png"), plot = p, width = 8, height = 6, dpi = 150)

# CSV of plotted data
write.csv(plot_data %>% select(-trtan_f), file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

# also write full tcount3 dataset for traceability
write.csv(tcount3, file.path(out, "tcount3.csv"), row.names = FALSE, na = "")
```
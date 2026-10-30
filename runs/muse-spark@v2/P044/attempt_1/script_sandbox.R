# P044_ae_common.sas -> R translation
# Common Treatment-Emergent Adverse Events scatter plot

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
})

# round half away from zero (SAS) helper
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# ---- locate inputs ----
rawadam_candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam",
  Sys.getenv("RAWADAM"),
  file.path(getwd(), "data/raw/cdisc-pilot/adam"),
  "/data/raw/cdisc-pilot/adam"
)
find_dir <- function(cands) {
  for (d in cands) {
    if (!is.na(d) && nzchar(d) && dir.exists(d)) return(d)
  }
  return(cands[1])
}
rawadam <- find_dir(rawadam_candidates)

find_file <- function(dir, fname) {
  p1 <- file.path(dir, fname)
  if (file.exists(p1)) return(p1)
  # fallback search a few places
  alts <- c(
    file.path("/bench/data/raw/cdisc-pilot/adam", fname),
    file.path("/bench/data/derived/cdisc-pilot/adam", fname),
    file.path(getwd(), fname)
  )
  for (a in alts) if (file.exists(a)) return(a)
  return(p1)
}

adsl_path <- find_file(rawadam, "adsl.xpt")
adae_path <- find_file(rawadam, "adae.xpt")

message("Reading: ", adsl_path)
message("Reading: ", adae_path)

adsl_raw <- haven::read_xpt(adsl_path)
adae_raw <- haven::read_xpt(adae_path)

# strip haven labels/formats to plain vectors
zap_df <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "labelled")) x <- haven::zap_labels(x)
    if (inherits(x, "labelled_spss")) x <- as.vector(x)
    # remove label attributes
    attr(x, "label") <- NULL
    attr(x, "format.sas") <- NULL
    attr(x, "labels") <- NULL
    df[[nm]] <- x
  }
  tibble::as_tibble(df)
}
adsl <- zap_df(adsl_raw)
adae <- zap_df(adae_raw)

# normalise names to uppercase for SAS-like handling
names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))

stopifnot(all(c("USUBJID","TRT01AN") %in% names(adsl)))
stopifnot(all(c("USUBJID","TRTAN","AEDECOD","TRTEMFL","AOCCPFL","SAFFL") %in% names(adae)))

# work.adae keep list
work_adae <- adae %>% select(USUBJID, TRTAN, AEDECOD, TRTEMFL, AOCCPFL, SAFFL)

# proc sort by aedecod (SAS: missing first, uppercase before lowercase)
# use base order with radix + na.last=FALSE to approximate
ord <- order(work_adae$AEDECOD, na.last = FALSE, method = "radix")
work_adae <- work_adae[ord, , drop = FALSE]

# data adae1 where TRTEMFL="Y" and AOCCPFL='Y' and saffl="Y"
adae1 <- work_adae %>% filter(TRTEMFL == "Y", AOCCPFL == "Y", SAFFL == "Y")

# proc sql countae: count(usubjid) per trtan,aedecod
countae <- adae1 %>%
  group_by(TRTAN, AEDECOD) %>%
  summarise(numAE = sum(!is.na(USUBJID)), .groups = "drop")

# proc sql nsubj: count(distinct usubjid) per TRT01AN
nsubj <- adsl %>%
  group_by(TRT01AN) %>%
  summarise(numusubjid = n_distinct(USUBJID[!is.na(USUBJID)]), .groups = "drop")

# data count; merge countae nsubj(rename trt01an=trtan) by trtan
nsubj_r <- nsubj %>% rename(TRTAN = TRT01AN)
# check abort condition: every trtan in countae must be in nsubj
missing_trt <- setdiff(unique(countae$TRTAN[!is.na(countae$TRTAN)]),
                       unique(nsubj_r$TRTAN[!is.na(nsubj_r$TRTAN)]))
if (length(missing_trt) > 0) {
  stop(paste0("ABORT CANCEL: TRTAN in countae not in nsubj: ",
              paste(missing_trt, collapse=",")))
}
# SAS merge by trtan = full outer on trtan, replicating numusubjid
count <- full_join(countae, nsubj_r, by = "TRTAN")
# AEpercent
count <- count %>%
  mutate(AEpercent = ifelse(!is.na(numAE) & !is.na(numusubjid),
                            numAE / numusubjid, NA_real_))
# label AEpercent="AEs (%)" kept as attribute
attr(count$AEpercent, "label") <- "AEs (%)"

# proc sort count by aedecod
count <- count %>% arrange(AEDECOD, TRTAN)

# data tcount; merge count with placebo subset (trtan=0) by aedecod
# SAS: count(where=(trtan_1=0) rename=(AEpercent=AEpercent_1 trtan=trtan_1))
placebo <- count %>%
  filter(TRTAN == 0) %>%
  select(AEDECOD, AEpercent_1 = AEpercent, TRTAN_1 = TRTAN)
# before merging drop/rename overlapping except key (already distinct names)
tcount <- left_join(count, placebo, by = "AEDECOD")
tcount$AEpercent[is.na(tcount$AEpercent)] <- 0
tcount$AEpercent_1[is.na(tcount$AEpercent_1)] <- 0
tcount$RelRisk <- tcount$AEpercent - tcount$AEpercent_1
# aeseqno+1 retain counter in current order (by aedecod)
tcount <- tcount %>% arrange(AEDECOD, TRTAN) %>%
  mutate(aeseqno = row_number())
# keep SAS order: by aedecod
tcount <- tcount %>% arrange(AEDECOD, TRTAN)

# proc sql maxpercen2: -max(AEpercent) per aedecod ordered
maxpercen2 <- tcount %>%
  group_by(AEDECOD) %>%
  summarise(minusmaxAEpercent = -max(AEpercent, na.rm = TRUE), .groups = "drop") %>%
  arrange(minusmaxAEpercent, AEDECOD)
# dispseqno+1
maxpercen2 <- maxpercen2 %>% mutate(dispseqno = row_number())

# proc sort maxpercen2 by aedecod; merge tcount maxpercen2 by aedecod
maxpercen2_s <- maxpercen2 %>% arrange(AEDECOD)
tcount_s <- tcount %>% arrange(AEDECOD)
tcount2 <- left_join(tcount_s, maxpercen2_s, by = "AEDECOD")

# proc sql AEgrid: cartesian trtan x (aedecod,dispseqno)
trtan_vals <- tcount2 %>% distinct(TRTAN) %>% arrange(TRTAN)
aedecod_vals <- tcount2 %>% distinct(AEDECOD, dispseqno) %>% arrange(AEDECOD)
# cartesian product ordered by aedecod, trtan
AEgrid <- expand.grid(TRTAN = trtan_vals$TRTAN,
                      AEDECOD = aedecod_vals$AEDECOD,
                      stringsAsFactors = FALSE) %>%
  tibble::as_tibble() %>%
  left_join(aedecod_vals, by = "AEDECOD") %>%
  arrange(AEDECOD, TRTAN)

# data tcount3; merge tcount2 AEgrid by aedecod trtan
# avoid .x/.y: drop dispseqno from tcount2 side (keep grid's dispseqno)
tcount2_noD <- tcount2 %>% select(-dispseqno)
# ensure both sorted by aedecod trtan
tcount2_noD <- tcount2_noD %>% arrange(AEDECOD, TRTAN)
AEgrid <- AEgrid %>% arrange(AEDECOD, TRTAN)
tcount3 <- left_join(AEgrid, tcount2_noD, by = c("AEDECOD","TRTAN"))
# if missing AEpercent then 0
tcount3$AEpercent[is.na(tcount3$AEpercent)] <- 0

# proc sort tcount3 by dispseqno
tcount3 <- tcount3 %>% arrange(dispseqno, AEDECOD, TRTAN)

# ---- plot data: where dispseqno<=20 ----
plot_data <- tcount3 %>% filter(!is.na(dispseqno) & dispseqno <= 20)

# write datasets
write.csv(tcount3, file.path(out, "tcount3.csv"), row.names = FALSE, na = "")
write.csv(plot_data, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

# ---- SGPlot scatter x=AEpercent y=dispseqno / group=trtan ----
suppressPackageStartupMessages(library(ggplot2))

plot_data$TRTAN_f <- factor(plot_data$TRTAN)

p <- ggplot(plot_data, aes(x = AEpercent, y = dispseqno, colour = TRTAN_f, group = TRTAN_f)) +
  geom_point(size = 2.5) +
  scale_y_continuous(breaks = 1:20, limits = c(1, 20)) +
  labs(title = "Common Treatment-Emergent Adverse Events",
       x = "AEs (%)",
       y = "dispseqno",
       colour = "Treatment") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_line(colour = "grey90"),
        plot.title = element_text(hjust = 0.5))

ggsave(file.path(out, "sgplot.png"), plot = p, width = 8, height = 6, dpi = 150)

message("Done. Rows tcount3=", nrow(tcount3), " plot_data=", nrow(plot_data))

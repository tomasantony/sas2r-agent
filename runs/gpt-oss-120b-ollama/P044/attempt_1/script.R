# ------------------------------------------------------------
# R translation of SAS program P044_ae_common.sas
# ------------------------------------------------------------
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

# ------------------------------------------------------------------
# Setup output directory
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# Helper: round half away from zero (SAS default)
round_half_up <- function(x, digits = 0) {
  pos <- x >= 0
  absx <- abs(x)
  factor <- 10^digits
  rounded <- floor(absx * factor + 0.5) / factor
  ifelse(pos, rounded, -rounded)
}

# ------------------------------------------------------------------
# 1. Read source data
adsl_path  <- file.path(Sys.getenv("rawadam"), "adsl.xpt")
adae_path  <- file.path(Sys.getenv("rawadam"), "adae.xpt")

adsl <- read_xpt(adsl_path) %>% 
  as_tibble()
adae <- read_xpt(adae_path) %>% 
  as_tibble() %>% 
  select(USUBJID, TRTAN, AEDECOD, TRTEMFL, AOCCPFL, SAFFL)

# ------------------------------------------------------------------
# 2. Sort adae by AEDECOD (character sort, case‑sensitive)
adae <- adae %>% arrange(AEDECOD)

# ------------------------------------------------------------------
# 3. Subset to treatment‑emergent, observed, and safety‑related AEs
adae1 <- adae %>% 
  filter(TRTEMFL == "Y", AOCCPFL == "Y", SAFFL == "Y")

# ------------------------------------------------------------------
# 4. Count AEs per treatment and AE term
countae <- adae1 %>% 
  group_by(TRTAN, AEDECOD) %>% 
  summarise(numAE = n_distinct(USUBJID), .groups = "drop") %>% 
  rename(trtan = TRTAN, aedecod = AEDECOD)

# ------------------------------------------------------------------
# 5. Number of subjects per treatment arm (from ADaM ADaSL)
nsubj <- adsl %>% 
  group_by(TRT01AN) %>% 
  summarise(numusubjid = n_distinct(USUBJID), .groups = "drop") %>% 
  rename(trtan = TRT01AN)

# ------------------------------------------------------------------
# 6. Merge counts with subject totals
count <- countae %>% 
  left_join(nsubj, by = "trtan")

# Abort if any treatment arm missing in nsubj (SAS: abort cancel)
if (any(is.na(count$numusubjid))) {
  stop("Missing subject count for at least one treatment arm (abort cancel).")
}

# Compute AE percent
count <- count %>% 
  mutate(AEpercent = ifelse(!is.na(numAE) & !is.na(numusubjid),
                            numAE / numusubjid,
                            NA_real_))

# ------------------------------------------------------------------
# 7. Sort by AE term
count <- count %>% arrange(aedecod)

# ------------------------------------------------------------------
# 8. Create tcount (add placebo reference if exists)
#   Subset where trtan == 0 (placebo) and rename columns
count_placebo <- count %>% 
  filter(trtan == 0) %>% 
  rename(AEpercent_1 = AEpercent,
         trtan_1    = trtan)

tcount <- count %>% 
  left_join(count_placebo, by = "aedecod") %>% 
  mutate(AEpercent   = ifelse(is.na(AEpercent),   0, AEpercent),
         AEpercent_1 = ifelse(is.na(AEpercent_1), 0, AEpercent_1),
         RelRisk     = AEpercent - AEpercent_1) %>% 
  arrange(aedecod) %>% 
  mutate(aeseqno = row_number())

# ------------------------------------------------------------------
# 9. Max percent per AE term (negative for ordering)
maxpercen2 <- tcount %>% 
  group_by(aedecod) %>% 
  summarise(minusmaxAEpercent = -max(AEpercent), .groups = "drop") %>% 
  arrange(minusmaxAEpercent) %>% 
  mutate(dispseqno = row_number())

# ------------------------------------------------------------------
# 10. Merge max sequence number back to tcount
tcount2 <- tcount %>% 
  left_join(maxpercen2, by = "aedecod") %>% 
  arrange(aedecod, trtan)

# ------------------------------------------------------------------
# 11. Build AEgrid (Cartesian product of treatments and AE terms)
AEgrid <- expand_grid(
  trtan    = distinct(tcount2, trtan)$trtan,
  aedecod = distinct(tcount2, aedecod, dispseqno) %>% 
            select(aedecod, dispseqno)
) %>% 
  arrange(aedecod, trtan)

# ------------------------------------------------------------------
# 12. Merge to obtain tcount3 and replace missing percentages with 0
tcount3 <- tcount2 %>% 
  full_join(AEgrid, by = c("aedecod", "trtan")) %>% 
  mutate(AEpercent = ifelse(is.na(AEpercent), 0, AEpercent),
         AEpercent_1 = ifelse(is.na(AEpercent_1), 0, AEpercent_1),
         RelRisk = ifelse(is.na(RelRisk), 0, RelRisk),
         aeseqno = ifelse(is.na(aeseqno), 0, aeseqno),
         dispseqno = ifelse(is.na(dispseqno), 0, dispseqno)) %>% 
  arrange(dispseqno)

# ------------------------------------------------------------------
# 13. Output final dataset (tcount3) as CSV
write.csv(tcount3,
          file.path(out_dir, "tcount3.csv"),
          row.names = FALSE,
          na = "")

# ------------------------------------------------------------------
# 14. Prepare data for the SGPlot (limit to top 20 AE terms)
plot_data <- tcount3 %>% 
  filter(dispseqno <= 20)

# Save plotted data
write.csv(plot_data,
          file.path(out_dir, "sgplot_data.csv"),
          row.names = FALSE,
          na = "")

# ------------------------------------------------------------------
# 15. Create SGPlot (scatter of AEpercent vs dispseqno, grouped by treatment)
sgplot <- ggplot(plot_data,
                 aes(x = AEpercent, y = dispseqno, colour = factor(trtan))) +
  geom_point() +
  scale_y_continuous(breaks = 1:20, limits = c(0.5, 20.5), expand = c(0,0)) +
  labs(title = "Common Treatment-Emergent Adverse Events",
       x = "AEpercent",
       y = "dispseqno",
       colour = "Treatment") +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank())

# Save PNG
ggsave(filename = file.path(out_dir, "sgplot.png"),
       plot = sgplot,
       width = 8, height = 6, dpi = 300)

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------

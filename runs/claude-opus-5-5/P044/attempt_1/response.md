```r
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

# ---- read ----
adsl <- read_xpt(file.path(rawadam, "adsl.xpt"))
adae <- read_xpt(file.path(rawadam, "adae.xpt")) %>%
  select(USUBJID, TRTAN, AEDECOD, TRTEMFL, AOCCPFL, SAFFL)
names(adae) <- toupper(names(adae))

# proc sort by aedecod (stable, C-locale ordering)
adae <- adae[order(adae$AEDECOD, method = "radix"), ]

adae1 <- adae %>% filter(TRTEMFL == "Y", AOCCPFL == "Y", SAFFL == "Y")

# ---- proc sql ----
countae <- adae1 %>%
  group_by(TRTAN, AEDECOD) %>%
  summarise(numAE = sum(!is.na(USUBJID) & USUBJID != ""), .groups = "drop") %>%
  select(numAE, AEDECOD, TRTAN)
countae <- countae[order(countae$TRTAN, countae$AEDECOD, method = "radix"), ]

nsubj <- adsl %>%
  group_by(TRT01AN) %>%
  summarise(numusubjid = n_distinct(USUBJID[USUBJID != ""]), .groups = "drop")

# ---- data count: merge by trtan ----
cnt <- countae %>%
  full_join(nsubj %>% rename(TRTAN = TRT01AN), by = "TRTAN")
if (any(is.na(cnt$numusubjid))) stop("abort cancel: trtan not found in nsubj")
cnt <- cnt %>%
  mutate(AEpercent = ifelse(!is.na(numAE) & !is.na(numusubjid),
                            numAE / numusubjid, NA_real_))
cnt <- cnt[order(cnt$TRTAN, method = "radix"), ]
# proc sort by aedecod (equals -> stable)
cnt <- cnt[order(cnt$AEDECOD, method = "radix"), ]

# ---- tcount: merge count with count(where trtan=0) by aedecod ----
right <- cnt %>% filter(TRTAN == 0) %>%
  transmute(AEDECOD, numAE_r = numAE, numusubjid_r = numusubjid,
            trtan_1 = TRTAN, AEpercent_1 = AEpercent)
tcount <- cnt %>%
  left_join(right, by = "AEDECOD") %>%
  group_by(AEDECOD) %>%
  mutate(.first = row_number() == 1) %>%
  ungroup() %>%
  mutate(
    # right dataset overwrites common vars on first obs of BY group
    numAE = ifelse(.first & !is.na(trtan_1), numAE_r, numAE),
    numusubjid = ifelse(.first & !is.na(trtan_1), numusubjid_r, numusubjid),
    AEpercent = ifelse(is.na(AEpercent), 0, AEpercent),
    AEpercent_1 = ifelse(is.na(AEpercent_1), 0, AEpercent_1),
    RelRisk = AEpercent - AEpercent_1,
    aeseqno = row_number()
  ) %>%
  select(numAE, AEDECOD, TRTAN, numusubjid, AEpercent, trtan_1, AEpercent_1,
         RelRisk, aeseqno)

# ---- maxpercen2 ----
maxpercen2 <- tcount %>%
  group_by(AEDECOD) %>%
  summarise(minusmaxAEpercent = -max(AEpercent), .groups = "drop")
maxpercen2 <- maxpercen2[order(maxpercen2$minusmaxAEpercent, maxpercen2$AEDECOD,
                               method = "radix"), ]
maxpercen2$dispseqno <- seq_len(nrow(maxpercen2))
maxpercen2 <- maxpercen2[order(maxpercen2$AEDECOD, method = "radix"), ]

tcount2 <- tcount %>% left_join(maxpercen2, by = "AEDECOD")

# ---- AEgrid ----
trts <- sort(unique(tcount2$TRTAN))
aes <- unique(tcount2[, c("AEDECOD", "dispseqno")])
AEgrid <- merge(data.frame(TRTAN = trts), aes, by = NULL)
AEgrid <- AEgrid[order(AEgrid$AEDECOD, AEgrid$TRTAN, method = "radix"), ]

# ---- tcount3 ----
tcount3 <- tcount2 %>%
  select(-dispseqno) %>%
  full_join(AEgrid, by = c("AEDECOD", "TRTAN"))
# dispseqno: from grid (same as tcount2's for matched rows)
tcount3 <- tcount3[order(tcount3$AEDECOD, tcount3$TRTAN, method = "radix"), ]
tcount3$AEpercent[is.na(tcount3$AEpercent)] <- 0
tcount3 <- tcount3 %>%
  select(numAE, AEDECOD, TRTAN, numusubjid, AEpercent, trtan_1, AEpercent_1,
         RelRisk, aeseqno, minusmaxAEpercent, dispseqno)
tcount3 <- tcount3[order(tcount3$dispseqno, method = "radix"), ]

write.csv(tcount3, file.path(out, "tcount3.csv"), row.names = FALSE, na = "")

# ---- plot ----
pdat <- tcount3 %>% filter(dispseqno <= 20)
write.csv(pdat, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

p <- ggplot(pdat, aes(x = AEpercent, y = dispseqno, colour = factor(TRTAN))) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = 1:20) +
  labs(title = "Common Treatment-Emergent Adverse Events",
       x = "AEs (%)", y = "dispseqno", colour = "Treatment") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(file.path(out, "sgplot.png"), p, width = 8, height = 6, dpi = 100)

cat("P044 done\n")
```
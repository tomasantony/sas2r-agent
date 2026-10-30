# P044_ae_common.sas -> R translation
# Outputs: OUT_DIR/sgplot.png + CSV of plotted data (and intermediate datasets)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# --- helpers ---
`%||%` <- function(a,b) if (!is.null(a) && length(a)>0) a else b
round_half_up <- function(x, digits=0){
  # SAS ROUND half away from zero
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# --- locate inputs (absolute paths from macro variables) ---
rawadam <- "/bench/data/raw/cdisc-pilot/adam"
# fallback: also check derived libref
inlib_dir <- "/bench/data/derived/cdisc-pilot/adam"

find_xpt <- function(name){
  candidates <- c(
    file.path(rawadam, paste0(name, ".xpt")),
    file.path(inlib_dir, paste0(name, ".xpt")),
    file.path("/bench/data/raw/cdisc-pilot/adam", paste0(name, ".xpt")),
    file.path("/bench/data/derived/cdisc-pilot/adam", paste0(name, ".xpt"))
  )
  for (p in candidates) if (file.exists(p)) return(p)
  return(candidates[1])
}

adsl_path <- find_xpt("adsl")
adae_path <- find_xpt("adae")

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)

# --- read ---
adsl <- haven::read_xpt(adsl_path)
adae <- haven::read_xpt(adae_path)

# Ensure column names uppercase to match SAS (haven preserves case)
names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))

# Keep only needed columns from ADAE (as SAS does)
# SAS: keep USUBJID TRTAN AEDECOD TRTEMFL AOCCPFL SAFFL
adae <- adae %>% select(any_of(c("USUBJID","TRTAN","AEDECOD","TRTEMFL","AOCCPFL","SAFFL")))

# proc sort data=work.adae; by aedecod;
adae <- adae %>% arrange(AEDECOD)

# data adae1; where TRTEMFL="Y" and AOCCPFL='Y' and saffl="Y";
adae1 <- adae %>%
  filter(TRTEMFL == "Y", AOCCPFL == "Y", SAFFL == "Y")

# proc sql countae: count(usubjid) as numAE per trtan,aedecod
# SAS count(usubjid) counts non-missing rows; equivalent to n()
countae <- adae1 %>%
  filter(!is.na(USUBJID)) %>%
  group_by(TRTAN, AEDECOD) %>%
  summarise(numAE = n(), .groups = "drop") %>%
  distinct()

# nsubj: count(distinct usubjid) per TRT01AN from adsl
# adsl TRT01AN is numeric treatment code; TRTAN in adae corresponds
if (!"TRT01AN" %in% names(adsl)) {
  # fallback if variable named differently
  stop("ADSL missing TRT01AN")
}
nsubj <- adsl %>%
  group_by(TRT01AN) %>%
  summarise(numusubjid = n_distinct(USUBJID), .groups = "drop") %>%
  distinct()

# data count; merge countae and nsubj by trtan
# SAS: merge countae(in=_countae) nsubj(in=_nsubj rename=(trt01an=trtan)) by trtan;
# if not _nsubj then abort cancel;
nsubj2 <- nsubj %>% rename(TRTAN = TRT01AN)

# Check abort condition: every TRTAN in countae must exist in nsubj
missing_trtan <- setdiff(unique(countae$TRTAN), unique(nsubj2$TRTAN))
if (length(missing_trtan) > 0) {
  stop(paste0("ABORT CANCEL: TRTAN in countae not in nsubj: ", paste(missing_trtan, collapse=",")))
}

count <- left_join(countae, nsubj2, by = "TRTAN") %>%
  # SAS merge by trtan: if countae has trtan not in nsubj, _nsubj false -> abort (handled)
  # If nsubj has trtan not in countae, those rows would appear with missing numAE? In SAS merge,
  # both datasets contribute. But subsequent logic expects countae rows only? Let's replicate SAS merge:
  # SAS merge by trtan without sort? Actually both sorted by trtan implicitly via SQL? Not sorted but merge expects sorted.
  # We emulate left_join + also add nsubj-only rows if any (to be faithful)
  bind_rows(
    # add nsubj rows that have no countae (would have numAE missing)
    anti_join(nsubj2, countae, by="TRTAN") %>%
      mutate(AEDECOD = NA_character_, numAE = NA_real_)
  ) %>%
  mutate(
    AEpercent = ifelse(!is.na(numAE) & !is.na(numusubjid), numAE / numusubjid, NA_real_)
  ) %>%
  # SAS label AEpercent="AEs (%)" - keep column
  arrange(AEDECOD)

# proc sort data=count; by aedecod;
count <- count %>% arrange(AEDECOD)

# data tcount; merge count with count(where=(trtan_1=0) rename=...) by aedecod
# Create placebo subset where TRTAN==0
count_placebo <- count %>%
  filter(TRTAN == 0) %>%
  transmute(AEDECOD, TRTAN_1 = TRTAN, AEpercent_1 = AEpercent)

# SAS merge by aedecod only: this is a many-to-many within each aedecod?
# In SAS, merging two datasets by AEDECOD when both have multiple rows per AEDECOD
# does a one-to-one interleaving in order. We need to replicate exactly.
# count is sorted by AEDECOD only (not TRTAN). count_placebo is also sorted by AEDECOD (single row per AEDECOD where TRTAN=0)
# SAS merge by AEDECOD will align row-by-row within each BY group.
# Example: for a given AEDECOD, count has rows for TRTAN 0,1,2 (3 rows), count_placebo has 1 row (TRTAN_1=0)
# SAS will produce 3 rows: first row gets placebo values, remaining rows get missing for AEpercent_1/TRTAN_1
# We emulate that.

# To replicate SAS merge behavior, we do row-number within AEDECOD
count <- count %>% group_by(AEDECOD) %>% mutate(.rn = row_number()) %>% ungroup()
count_placebo <- count_placebo %>% group_by(AEDECOD) %>% mutate(.rn = row_number()) %>% ungroup()

tcount <- full_join(count, count_placebo, by = c("AEDECOD", ".rn")) %>%
  arrange(AEDECOD, .rn) %>%
  select(-.rn) %>%
  # SAS: if missing(AEpercent) then AEpercent=0; same for AEpercent_1
  mutate(
    AEpercent = ifelse(is.na(AEpercent), 0, AEpercent),
    AEpercent_1 = ifelse(is.na(AEpercent_1), 0, AEpercent_1),
    RelRisk = AEpercent - AEpercent_1
  ) %>%
  # aeseqno+1 retain counter in merge order (by AEDECOD)
  mutate(aeseqno = row_number())

# proc sql maxpercen2: -max(AEpercent) per aedecod order by minusmaxAEpercent
maxpercen2 <- tcount %>%
  group_by(AEDECOD) %>%
  summarise(minusmaxAEpercent = -max(AEpercent, na.rm=TRUE), .groups="drop") %>%
  distinct() %>%
  arrange(minusmaxAEpercent)

# data maxpercen2; dispseqno+1;
maxpercen2 <- maxpercen2 %>%
  mutate(dispseqno = row_number())

# proc sort by aedecod; merge to tcount2
maxpercen2_sorted <- maxpercen2 %>% arrange(AEDECOD)
tcount_sorted <- tcount %>% arrange(AEDECOD)

tcount2 <- left_join(tcount_sorted, maxpercen2_sorted, by = "AEDECOD")

# proc sql AEgrid: cross join distinct trtan and distinct aedecod,dispseqno
distinct_trtan <- tcount2 %>% distinct(TRTAN)
distinct_ae <- tcount2 %>% distinct(AEDECOD, dispseqno)

AEgrid <- crossing(distinct_ae, distinct_trtan) %>%
  arrange(AEDECOD, TRTAN)

# data tcount3; merge tcount2 AEgrid by aedecod trtan; if missing AEpercent then 0
tcount3 <- full_join(tcount2, AEgrid, by = c("AEDECOD","dispseqno","TRTAN")) %>%
  mutate(AEpercent = ifelse(is.na(AEpercent), 0, AEpercent)) %>%
  arrange(dispseqno)

# --- outputs ---
# Write datasets as CSV (SAS dataset counterparts)
# Use ISO dates if any date columns exist
write_csv_iso <- function(df, path){
  # Convert Date/POSIX to ISO strings
  df2 <- df %>% mutate(across(where(~inherits(.x,"Date")), ~format(.x, "%Y-%m-%d")),
                       across(where(~inherits(.x,"POSIXt")), ~format(.x, "%Y-%m-%dT%H:%M:%S")))
  write.csv(df2, path, row.names = FALSE, na = "")
}

write_csv_iso(count, file.path(out, "count.csv"))
write_csv_iso(tcount, file.path(out, "tcount.csv"))
write_csv_iso(tcount2, file.path(out, "tcount2.csv"))
write_csv_iso(tcount3, file.path(out, "tcount3.csv"))
write_csv_iso(maxpercen2, file.path(out, "maxpercen2.csv"))
write_csv_iso(AEgrid, file.path(out, "AEgrid.csv"))

# Plotted data: where dispseqno <=20
plot_data <- tcount3 %>% filter(dispseqno <= 20) %>%
  arrange(dispseqno, TRTAN)

# Save plotted data CSV (required for figure)
write_csv_iso(plot_data, file.path(out, "sgplot.csv"))
# Also generic name
write_csv_iso(plot_data, file.path(out, "SGPlot.csv"))

# --- figure ---
# SAS: title1 "Common Treatment-Emergent Adverse Events";
# proc sgplot scatter x=AEpercent y=dispseqno / group=trtan; xaxis grid; yaxis grid values 1 to 20
# AEpercent is proportion (0-1); SAS xaxis will show 0-1. We replicate.

# Map TRTAN to treatment labels if available in ADSL
# Try to get TRT01A labels
trt_labels <- NULL
if ("TRT01AN" %in% names(adsl) && "TRT01A" %in% names(adsl)) {
  trt_labels <- adsl %>% distinct(TRT01AN, TRT01A) %>% arrange(TRT01AN)
  # haven labelled may have labels
}

# Prepare plot data with factor for TRTAN
plot_data <- plot_data %>%
  mutate(TRTAN_f = factor(TRTAN))

# If labels available, use them
if (!is.null(trt_labels) && nrow(trt_labels)>0) {
  lab_map <- setNames(trt_labels$TRT01A, trt_labels$TRT01AN)
  plot_data <- plot_data %>%
    mutate(TRTAN_label = lab_map[as.character(TRTAN)],
           TRTAN_label = ifelse(is.na(TRTAN_label), as.character(TRTAN), TRTAN_label),
           TRTAN_f = factor(TRTAN_label))
}

# Determine y labels: AEDECOD per dispseqno
y_labels <- maxpercen2 %>% arrange(dispseqno) %>% filter(dispseqno <=20) %>%
  select(dispseqno, AEDECOD)

# For y-axis we want dispseqno 1 at top? SAS yaxis values 1 to 20 by 1 valueshint with default orientation (1 at bottom).
# We'll keep 1 at bottom to match SAS, but also show AEDECOD as labels if space.
# Use dispseqno as y, AEpercent as x

p <- ggplot(plot_data, aes(x = AEpercent, y = dispseqno, color = TRTAN_f, group = TRTAN_f)) +
  geom_point(size = 2.5) +
  scale_y_continuous(breaks = 1:20, limits = c(1,20), trans = "reverse") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Common Treatment-Emergent Adverse Events",
    x = "AEs (%)",
    y = "AE ordered in descending order",
    color = "Treatment"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Add AEDECOD labels on y-axis if desired: use secondary? We'll annotate y tick labels with AEDECOD
# Create named breaks
if (nrow(y_labels) > 0) {
  # Truncate long AEDECOD for display
  y_lab_vec <- setNames(paste0(y_labels$dispseqno, ": ", str_trunc(y_labels$AEDECOD, 40)), y_labels$dispseqno)
  p <- p + scale_y_continuous(breaks = 1:20, labels = function(x) y_lab_vec[as.character(x)], limits = c(1,20), trans="reverse")
}

ggsave(file.path(out, "sgplot.png"), plot = p, width = 10, height = 8, dpi = 300, units = "in")
# Also uppercase variant for expected output check
ggsave(file.path(out, "SGPlot.png"), plot = p, width = 10, height = 8, dpi = 300, units = "in")

# --- text rendering of table body (as required) ---
# Create a txt with same numbers SAS would show
txt_path <- file.path(out, "sgplot.txt")
sink(txt_path)
cat("Common Treatment-Emergent Adverse Events\n")
cat("------------------------------------------------------------\n")
cat("Plot data (dispseqno <=20) : AEpercent = numAE / numusubjid\n")
cat("\n")
print(as.data.frame(plot_data %>% select(AEDECOD, TRTAN, numAE, numusubjid, AEpercent, AEpercent_1, RelRisk, dispseqno) %>% arrange(dispseqno, TRTAN)), row.names=FALSE)
cat("\n")
cat("Y-axis: dispseqno = rank of -max(AEpercent) per AEDECOD (1 = most common)\n")
cat("X-axis: AEpercent (proportion)\n")
sink()

# Also write tcount3 txt
sink(file.path(out, "tcount3.txt"))
cat("Common Treatment-Emergent Adverse Events - tcount3\n")
print(as.data.frame(tcount3 %>% arrange(dispseqno, TRTAN)), row.names=FALSE)
sink()

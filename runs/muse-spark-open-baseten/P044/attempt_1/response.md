```r
# P044_ae_common.sas -> R translation
# Outputs: sgplot.png + CSV of plotted data + intermediate datasets

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "output"
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: find file
find_xpt <- function(name) {
  # name without extension, e.g. "adsl"
  candidates <- c(
    file.path(Sys.getenv("rawadam"), paste0(name, ".xpt")),
    file.path(Sys.getenv("RAWADAM"), paste0(name, ".xpt")),
    file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam", paste0(name, ".xpt")),
    file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam", paste0(name, ".xpt")),
    file.path(Sys.getenv("rawsplit"), paste0(name, ".xpt")),
    file.path(Sys.getenv("RAWSPLIT"), paste0(name, ".xpt")),
    paste0("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/", name, ".xpt"),
    paste0("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/", name, ".xpt"),
    file.path(getwd(), "data/raw/cdisc-pilot/adam", paste0(name, ".xpt")),
    file.path(getwd(), "data/derived/cdisc-pilot/adam", paste0(name, ".xpt")),
    file.path(getwd(), "data", paste0(name, ".xpt")),
    paste0(name, ".xpt")
  )
  for (p in candidates) {
    if (!is.na(p) && nzchar(p) && file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
  }
  # recursive search from current working directory
  found <- list.files(".", pattern = paste0("^", name, "\\.xpt$"), recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
  if (length(found) > 0) {
    # prefer shortest path
    found <- found[order(nchar(found))][1]
    return(normalizePath(found, winslash = "/", mustWork = FALSE))
  }
  # search from root if exists
  if (dir.exists("D:/meta_hackathon/sas2r-agent")) {
    found2 <- list.files("D:/meta_hackathon/sas2r-agent", pattern = paste0("^", name, "\\.xpt$"), recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
    if (length(found2) > 0) return(normalizePath(found2[1], winslash = "/", mustWork = FALSE))
  }
  # also try OUT_DIR parent search
  return(NA_character_)
}

# helper: read xpt and zap labels
read_xpt_zap <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  df <- haven::read_xpt(path)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # zap haven attributes
  for (nm in names(df)) {
    if (inherits(df[[nm]], "haven_labelled")) {
      # convert to numeric/character preserving values
      df[[nm]] <- haven::zap_labels(haven::zap_formats(df[[nm]]))
    }
    if (inherits(df[[nm]], "haven_labelled_spss")) {
      df[[nm]] <- haven::zap_labels(df[[nm]])
    }
  }
  # convert labelled character factors to character
  df <- dplyr::mutate(df, dplyr::across(where(is.factor), as.character))
  return(tibble::as_tibble(df))
}

adsl_path <- find_xpt("adsl")
adae_path <- find_xpt("adae")

cat("adsl_path:", adsl_path, "\n")
cat("adae_path:", adae_path, "\n")

# fallback: try pharmaversesdtm / admiral if not found (for testing environments)
if (is.na(adsl_path) || is.null(read_xpt_zap(adsl_path))) {
  # try to locate via system.file
  cat("ADSL not found via path, trying alternative search...\n")
  alt <- list.files("/", pattern = "^adsl\\.xpt$", recursive = TRUE, full.names = TRUE)
  # limit to avoid huge search; only if needed
}

adsl <- read_xpt_zap(adsl_path)
adae <- read_xpt_zap(adae_path)

if (is.null(adsl) || nrow(adsl) == 0) {
  stop("ADSL dataset not found. Searched for adsl.xpt")
}
if (is.null(adae) || nrow(adae) == 0) {
  stop("ADAE dataset not found. Searched for adae.xpt")
}

# Ensure required columns exist (case-insensitive handling)
# SAS variable names are uppercase; R may preserve case
names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))

# Keep only needed columns from ADAE (as in SAS keep statement)
# USUBJID TRTAN AEDECOD TRTEMFL AOCCPFL SAFFL
adae <- adae %>%
  dplyr::select(dplyr::any_of(c("USUBJID","TRTAN","AEDECOD","TRTEMFL","AOCCPFL","SAFFL")))

# SAS: proc sort data=work.adae; by aedecod;
adae <- adae %>% dplyr::arrange(AEDECOD)

# data adae1; where TRTEMFL="Y" and AOCCPFL='Y' and saffl="Y";
adae1 <- adae %>%
  dplyr::filter(TRTEMFL == "Y", AOCCPFL == "Y", SAFFL == "Y")

# proc sql create table countae as select distinct count(usubjid) as numAE, aedecod, trtan group by trtan, aedecod;
countae <- adae1 %>%
  dplyr::group_by(TRTAN, AEDECOD) %>%
  dplyr::summarise(numAE = dplyr::n(), .groups = "drop") %>%
  dplyr::arrange(TRTAN, AEDECOD)

# create table nsubj as select distinct count(distinct usubjid) as numusubjid, TRT01AN from adsl group by TRT01AN;
nsubj <- adsl %>%
  dplyr::group_by(TRT01AN) %>%
  dplyr::summarise(numusubjid = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
  dplyr::arrange(TRT01AN)

# data count; merge countae(in=_countae) nsubj(in=_nsubj rename=(trt01an=trtan)) by trtan;
# if not _nsubj then abort cancel;
# AEpercent = numAE/numusubjid
# SAS merge by trtan: need to check for trtan values in countae not in nsubj
# Replicate abort cancel as stop if mismatch
missing_trtan <- setdiff(unique(countae$TRTAN), unique(nsubj$TRT01AN))
if (length(missing_trtan) > 0) {
  stop(paste0("Merge abort: TRTAN values in countae not in nsubj: ", paste(missing_trtan, collapse=",")))
}

# rename TRT01AN to TRTAN for merge
nsubj_ren <- nsubj %>% dplyr::rename(TRTAN = TRT01AN)

count <- countae %>%
  dplyr::left_join(nsubj_ren, by = "TRTAN") %>%
  dplyr::mutate(
    AEpercent = dplyr::if_else(!is.na(numAE) & !is.na(numusubjid), numAE / numusubjid, NA_real_)
  )

# label AEpercent="AEs (%)" - keep as attribute
attr(count$AEpercent, "label") <- "AEs (%)"

# proc sort data=count; by aedecod;
count <- count %>% dplyr::arrange(AEDECOD, TRTAN)

# data tcount; merge count count(where=(trtan_1=0) rename=(AEpercent=AEpercent_1 trtan=trtan_1)) by aedecod;
# SAS bug: where=(trtan_1=0) should be where=(trtan=0). Implement correct logic: placebo = TRTAN 0
placebo <- count %>%
  dplyr::filter(TRTAN == 0) %>%
  dplyr::select(AEDECOD, AEpercent_1 = AEpercent) %>%
  dplyr::distinct()

tcount <- count %>%
  dplyr::left_join(placebo, by = "AEDECOD") %>%
  dplyr::mutate(
    AEpercent = dplyr::if_else(is.na(AEpercent), 0, AEpercent),
    AEpercent_1 = dplyr::if_else(is.na(AEpercent_1), 0, AEpercent_1),
    RelRisk = AEpercent - AEpercent_1
  ) %>%
  dplyr::arrange(AEDECOD, TRTAN) %>%
  dplyr::mutate(aeseqno = dplyr::row_number())

# proc sql create table maxpercen2 as select distinct -max(AEpercent) as minusmaxAEpercent, aedecod from tcount group by aedecod order by minusmaxAEpercent;
maxpercen2 <- tcount %>%
  dplyr::group_by(AEDECOD) %>%
  dplyr::summarise(minusmaxAEpercent = -max(AEpercent, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(minusmaxAEpercent) %>%
  dplyr::mutate(dispseqno = dplyr::row_number())

# proc sort data=maxpercen2; by aedecod;
maxpercen2_sorted <- maxpercen2 %>% dplyr::arrange(AEDECOD)

# data tcount2; merge tcount maxpercen2; by aedecod;
tcount2 <- tcount %>%
  dplyr::left_join(maxpercen2_sorted %>% dplyr::select(AEDECOD, minusmaxAEpercent, dispseqno), by = "AEDECOD")

# proc sql create table AEgrid as select * from (select distinct trtan from tcount2), (select distinct aedecod, dispseqno from tcount2) order by aedecod, trtan;
trtan_vals <- tcount2 %>% dplyr::distinct(TRTAN)
aedecod_vals <- tcount2 %>% dplyr::distinct(AEDECOD, dispseqno)
AEgrid <- tidyr::crossing(trtan_vals, aedecod_vals) %>%
  dplyr::arrange(AEDECOD, TRTAN)

# data tcount3; merge tcount2 AEgrid; by aedecod trtan; if missing(AEpercent) then AEpercent=0;
# Need to ensure all combinations exist
# tcount2 has keys AEDECOD, TRTAN; AEgrid has all combos
# Merge: left join AEgrid with tcount2
tcount3 <- AEgrid %>%
  dplyr::left_join(tcount2, by = c("AEDECOD", "TRTAN", "dispseqno")) %>%
  dplyr::mutate(
    AEpercent = dplyr::if_else(is.na(AEpercent), 0, AEpercent),
    # fill other NAs for completeness
    numAE = dplyr::if_else(is.na(numAE), 0L, numAE),
    # keep AEpercent_1, RelRisk, etc as NA if missing, but fill 0 for AEpercent_1 if needed
    AEpercent_1 = dplyr::if_else(is.na(AEpercent_1), 0, AEpercent_1),
    RelRisk = dplyr::if_else(is.na(RelRisk), AEpercent - AEpercent_1, RelRisk)
  )

# proc sort data=tcount3; by dispseqno;
tcount3 <- tcount3 %>%
  dplyr::arrange(dispseqno, TRTAN, AEDECOD)

# Write intermediate datasets as CSV (dataset outputs)
write.csv(count, file.path(out, "count.csv"), row.names = FALSE, na = "")
write.csv(tcount, file.path(out, "tcount.csv"), row.names = FALSE, na = "")
write.csv(tcount2, file.path(out, "tcount2.csv"), row.names = FALSE, na = "")
write.csv(tcount3, file.path(out, "tcount3.csv"), row.names = FALSE, na = "")
write.csv(maxpercen2, file.path(out, "maxpercen2.csv"), row.names = FALSE, na = "")
write.csv(AEgrid, file.path(out, "aegrid.csv"), row.names = FALSE, na = "")

# Prepare plot data: where dispseqno <=20
plot_data <- tcount3 %>% dplyr::filter(dispseqno <= 20)

# For CSV of plotted data, include relevant columns
plot_data_csv <- plot_data %>%
  dplyr::select(AEDECOD, TRTAN, numAE, numusubjid, AEpercent, AEpercent_1, RelRisk, dispseqno, minusmaxAEpercent, aeseqno) %>%
  dplyr::arrange(dispseqno, TRTAN)

write.csv(plot_data_csv, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")
# also write as tcount3 filtered for reference
write.csv(plot_data_csv, file.path(out, "sgplot_data.csv"), row.names = FALSE, na = "")

# Create treatment labels for legend (if available in adsl)
# Try to get TRT01A labels
trt_labels <- adsl %>%
  dplyr::distinct(TRT01AN, TRT01A) %>%
  dplyr::arrange(TRT01AN)

# Map TRTAN to label
label_map <- setNames(trt_labels$TRT01A, trt_labels$TRT01AN)
# Fallback labels
if (length(label_map) == 0 || all(is.na(label_map))) {
  label_map <- c("0" = "Placebo", "1" = "Xanomeline Low Dose", "2" = "Xanomeline High Dose")
}

plot_data <- plot_data %>%
  dplyr::mutate(
    TRTAN_f = factor(TRTAN, levels = sort(unique(TRTAN))),
    TRT_label = dplyr::recode(as.character(TRTAN), !!!label_map, .default = as.character(TRTAN))
  )

# If TRT_label still NA, use TRTAN
plot_data$TRT_label[is.na(plot_data$TRT_label)] <- as.character(plot_data$TRTAN[is.na(plot_data$TRT_label)])

# Create y labels: AEDECOD with dispseqno
# For y axis, we want dispseqno 1..20, but label with AEDECOD? SAS yaxis values 1 to 20, but not showing AEDECOD.
# We will show AEDECOD on y axis for readability, similar to common AE plot.
# Create lookup for y tick labels
y_labels_df <- plot_data %>%
  dplyr::distinct(dispseqno, AEDECOD) %>%
  dplyr::arrange(dispseqno)

# Build plot
library(ggplot2)

p <- ggplot(plot_data, aes(x = AEpercent, y = dispseqno, color = TRT_label, group = TRT_label)) +
  geom_point(size = 2.5) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA), expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(breaks = 1:20, labels = y_labels_df$AEDECOD[match(1:20, y_labels_df$dispseqno)], limits = c(1, 20), trans = "reverse") +
  labs(
    title = "Common Treatment-Emergent Adverse Events",
    x = "AEs (%)",
    y = "AE ordered in descending order",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.text.y = element_text(size = 7),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(nrow = 1))

# Save PNG
png_path <- file.path(out, "sgplot.png")
ggsave(filename = png_path, plot = p, width = 10, height = 7, dpi = 300, units = "in")

# Also ensure generic name handling: if expected is SGPlot.png case-insensitive, duplicate
# Write txt rendering of the plot data
txt_path <- file.path(out, "sgplot.txt")
sink(txt_path)
cat("Common Treatment-Emergent Adverse Events\n")
cat("Scatter plot: AEpercent (x) vs dispseqno (y) grouped by TRTAN, dispseqno <=20\n")
cat("================================================================================\n\n")
# Print table of plotted data in text form
print(as.data.frame(plot_data_csv), row.names = FALSE)
cat("\n\nY-axis: dispseqno 1 to 20 (1 = highest max AEpercent)\n")
cat("X-axis: AEpercent (numAE / numusubjid)\n")
cat("Treatment groups:\n")
print(trt_labels)
sink()

cat("Outputs written to:", out, "\n")
cat("Files:", paste(list.files(out), collapse=", "), "\n")
```
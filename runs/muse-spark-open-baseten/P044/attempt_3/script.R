out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "output"
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(tibble)
})

find_xpt <- function(name) {
  lname <- tolower(name)
  candidates <- c(
    file.path(Sys.getenv("rawadam"), paste0(lname, ".xpt")),
    file.path(Sys.getenv("RAWADAM"), paste0(lname, ".xpt")),
    file.path(Sys.getenv("rawadam"), paste0(toupper(lname), ".xpt")),
    file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam", paste0(lname, ".xpt")),
    file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam", paste0(lname, ".xpt")),
    file.path("/bench/data/raw/cdisc-pilot/adam", paste0(lname, ".xpt")),
    file.path("/bench/data/raw/cdisc-pilot/adam", paste0(toupper(lname), ".xpt")),
    file.path("/bench/data/derived/cdisc-pilot/adam", paste0(lname, ".xpt")),
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adae.xpt"
  )
  # also add generic bench with correct name
  candidates <- unique(c(
    file.path("/bench/data/raw/cdisc-pilot/adam", paste0(name, ".xpt")),
    file.path("/bench/data/raw/cdisc-pilot/adam", paste0(tolower(name), ".xpt")),
    candidates
  ))
  for (p in candidates) {
    if (!is.na(p) && nzchar(p) && file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
  }
  found <- list.files(".", pattern = paste0("^", name, "\\.xpt$"), recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
  if (length(found) > 0) {
    found <- found[order(nchar(found))][1]
    return(normalizePath(found, winslash = "/", mustWork = FALSE))
  }
  if (dir.exists("/bench")) {
    found2 <- list.files("/bench", pattern = paste0("^", name, "\\.xpt$"), recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
    if (length(found2) > 0) return(normalizePath(found2[1], winslash = "/", mustWork = FALSE))
  }
  return(NA_character_)
}

read_xpt_zap <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  df <- haven::read_xpt(path)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    if (inherits(df[[nm]], "haven_labelled")) {
      df[[nm]] <- haven::zap_labels(haven::zap_formats(df[[nm]]))
    }
  }
  df <- dplyr::mutate(df, dplyr::across(where(is.factor), as.character))
  tibble::as_tibble(df)
}

adsl_path <- find_xpt("adsl")
adae_path <- find_xpt("adae")

# hard fallback to known bench locations (fix previous bug where adae_path pointed to adsl)
if (is.na(adsl_path) || !file.exists(adsl_path)) {
  if (file.exists("/bench/data/raw/cdisc-pilot/adam/adsl.xpt")) adsl_path <- "/bench/data/raw/cdisc-pilot/adam/adsl.xpt"
}
if (is.na(adae_path) || !file.exists(adae_path) || adsl_path == adae_path) {
  if (file.exists("/bench/data/raw/cdisc-pilot/adam/adae.xpt")) adae_path <- "/bench/data/raw/cdisc-pilot/adam/adae.xpt"
}

cat("adsl_path:", adsl_path, "\n")
cat("adae_path:", adae_path, "\n")

adsl <- read_xpt_zap(adsl_path)
adae <- read_xpt_zap(adae_path)

if (is.null(adsl) || nrow(adsl) == 0) stop("ADSL dataset not found")
if (is.null(adae) || nrow(adae) == 0) stop("ADAE dataset not found")

names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))

# keep only needed cols from ADAE
adae <- adae %>% select(any_of(c("USUBJID","TRTAN","AEDECOD","TRTEMFL","AOCCPFL","SAFFL")))
adae <- adae %>% arrange(AEDECOD)

adae1 <- adae %>% filter(TRTEMFL == "Y", AOCCPFL == "Y", SAFFL == "Y")

countae <- adae1 %>%
  group_by(TRTAN, AEDECOD) %>%
  summarise(numAE = n(), .groups = "drop") %>%
  arrange(TRTAN, AEDECOD)

nsubj <- adsl %>%
  group_by(TRT01AN) %>%
  summarise(numusubjid = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRT01AN)

missing_trtan <- setdiff(unique(countae$TRTAN), unique(nsubj$TRT01AN))
if (length(missing_trtan) > 0) {
  stop(paste0("Merge abort: TRTAN values in countae not in nsubj: ", paste(missing_trtan, collapse=",")))
}

nsubj_ren <- nsubj %>% rename(TRTAN = TRT01AN)

count <- countae %>%
  left_join(nsubj_ren, by = "TRTAN") %>%
  mutate(AEpercent = if_else(!is.na(numAE) & !is.na(numusubjid), numAE / numusubjid, NA_real_))
attr(count$AEpercent, "label") <- "AEs (%)"
count <- count %>% arrange(AEDECOD, TRTAN)

placebo <- count %>%
  filter(TRTAN == 0) %>%
  select(AEDECOD, AEpercent_1 = AEpercent) %>%
  distinct()

tcount <- count %>%
  left_join(placebo, by = "AEDECOD") %>%
  mutate(
    AEpercent = if_else(is.na(AEpercent), 0, AEpercent),
    AEpercent_1 = if_else(is.na(AEpercent_1), 0, AEpercent_1),
    RelRisk = AEpercent - AEpercent_1
  ) %>%
  arrange(AEDECOD, TRTAN) %>%
  mutate(aeseqno = row_number())

maxpercen2 <- tcount %>%
  group_by(AEDECOD) %>%
  summarise(minusmaxAEpercent = -max(AEpercent, na.rm = TRUE), .groups = "drop") %>%
  arrange(minusmaxAEpercent) %>%
  mutate(dispseqno = row_number())

maxpercen2_sorted <- maxpercen2 %>% arrange(AEDECOD)

tcount2 <- tcount %>%
  left_join(maxpercen2_sorted %>% select(AEDECOD, minusmaxAEpercent, dispseqno), by = "AEDECOD")

trtan_vals <- tcount2 %>% distinct(TRTAN)
aedecod_vals <- tcount2 %>% distinct(AEDECOD, dispseqno)
AEgrid <- crossing(trtan_vals, aedecod_vals) %>%
  arrange(AEDECOD, TRTAN)

tcount3 <- AEgrid %>%
  left_join(tcount2, by = c("AEDECOD", "TRTAN", "dispseqno")) %>%
  mutate(
    AEpercent = if_else(is.na(AEpercent), 0, AEpercent),
    numAE = if_else(is.na(numAE), 0L, numAE),
    AEpercent_1 = if_else(is.na(AEpercent_1), 0, AEpercent_1),
    RelRisk = if_else(is.na(RelRisk), AEpercent - AEpercent_1, RelRisk)
  )

tcount3 <- tcount3 %>% arrange(dispseqno, TRTAN, AEDECOD)

write.csv(count, file.path(out, "count.csv"), row.names = FALSE, na = "")
write.csv(tcount, file.path(out, "tcount.csv"), row.names = FALSE, na = "")
write.csv(tcount2, file.path(out, "tcount2.csv"), row.names = FALSE, na = "")
write.csv(tcount3, file.path(out, "tcount3.csv"), row.names = FALSE, na = "")
write.csv(maxpercen2, file.path(out, "maxpercen2.csv"), row.names = FALSE, na = "")
write.csv(AEgrid, file.path(out, "aegrid.csv"), row.names = FALSE, na = "")

plot_data <- tcount3 %>% filter(dispseqno <= 20)

plot_data_csv <- plot_data %>%
  select(AEDECOD, TRTAN, numAE, numusubjid, AEpercent, AEpercent_1, RelRisk, dispseqno, minusmaxAEpercent, aeseqno) %>%
  arrange(dispseqno, TRTAN)

write.csv(plot_data_csv, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")
write.csv(plot_data_csv, file.path(out, "sgplot_data.csv"), row.names = FALSE, na = "")

trt_labels <- adsl %>%
  distinct(TRT01AN, TRT01A) %>%
  arrange(TRT01AN)

label_map <- setNames(trt_labels$TRT01A, trt_labels$TRT01AN)
if (length(label_map) == 0 || all(is.na(label_map))) {
  label_map <- c("0" = "Placebo", "1" = "Xanomeline Low Dose", "2" = "Xanomeline High Dose")
}

plot_data <- plot_data %>%
  mutate(
    TRT_label = recode(as.character(TRTAN), !!!label_map, .default = as.character(TRTAN))
  )
plot_data$TRT_label[is.na(plot_data$TRT_label)] <- as.character(plot_data$TRTAN[is.na(plot_data$TRT_label)])

y_labels_df <- plot_data %>%
  distinct(dispseqno, AEDECOD) %>%
  arrange(dispseqno)

# ensure y labels cover 1:20
y_breaks <- 1:20
y_labels <- y_labels_df$AEDECOD[match(y_breaks, y_labels_df$dispseqno)]

p <- ggplot(plot_data, aes(x = AEpercent, y = dispseqno, color = TRT_label)) +
  geom_point(size = 2.5) +
  scale_x_continuous(labels = function(x) paste0(round(x*100), "%"), limits = c(0, NA), expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(breaks = y_breaks, labels = y_labels, limits = c(1, 20), trans = "reverse") +
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

png_path <- file.path(out, "sgplot.png")
ggsave(filename = png_path, plot = p, width = 10, height = 7, dpi = 300, units = "in")
cat("Saved", png_path, "\n")

txt_path <- file.path(out, "sgplot.txt")
sink(txt_path)
cat("Common Treatment-Emergent Adverse Events\n")
cat("Scatter plot: AEpercent (x) vs dispseqno (y) grouped by TRTAN, dispseqno <=20\n")
cat("================================================================================\n\n")
print(as.data.frame(plot_data_csv), row.names = FALSE)
cat("\n\nY-axis: dispseqno 1 to 20 (1 = highest max AEpercent)\n")
cat("X-axis: AEpercent (numAE / numusubjid)\n")
cat("Treatment groups:\n")
print(as.data.frame(trt_labels))
sink()

cat("Outputs written to:", out, "\n")
cat("Files:", paste(list.files(out), collapse=", "), "\n")

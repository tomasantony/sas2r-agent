# ------------------------------------------------------------
# R translation of SAS program P061_oncology_sdtm_adam_tlf_analysis
# ------------------------------------------------------------
# Packages (pre‑installed in the execution environment)
library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)
library(ggplot2)
library(survival)
library(survminer)

# ------------------------------------------------------------------
# Helper: SAS‑style round half up
round_half_up <- function(x, digits = 0) {
  sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
}

# ------------------------------------------------------------------
# Paths (mirroring the macro variables from the SAS program)
rawdir   <- "D:/meta_hackathon/sas2r-agent/data/raw"
outtlf   <- "D:/meta_hackathon/sas2r-agent/output/sas/tlf"
outds    <- "D:/meta_hackathon/sas2r-agent/output/sas/datasets/P061"
out_dir  <- Sys.getenv("OUT_DIR")   # all final artefacts are written here

# ------------------------------------------------------------------
# 1. Import raw CSV (column names are cleaned to lower‑case & underscores)
raw_file <- file.path(rawdir, "sumit-oncology", "Breast Cancer METABRIC.csv")
raw_metabric <- read_csv(raw_file, col_types = cols(.default = "c"))

# Clean column names for easier handling
clean_names <- colnames(raw_metabric) %>%
  str_trim() %>%
  str_to_lower() %>%
  str_replace_all("[^a-z0-9]+", "_")
colnames(raw_metabric) <- clean_names

# ------------------------------------------------------------------
# 2. Create SDTM DM (Demographics)
sdtm_dm <- raw_metabric %>%
  mutate(
    STUDYID = "ONC001",
    DOMAIN  = "DM",
    USUBJID = paste0(STUDYID, "-", patient_id),
    AGE     = as.numeric(age_at_diagnosis)
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, everything())

# ------------------------------------------------------------------
# 3. Create ADaM ADSL (add treatment flag)
adam_adsl <- sdtm_dm %>%
  mutate(
    TRT01P = "CHEMO",
    SAFFL  = "Y"
  )

# ------------------------------------------------------------------
# 4. Create ADaM ADTTE (survival variables)
adam_adtte <- adam_adsl %>%
  mutate(
    AVAL = as.numeric(`overall_survival_(months)`),
    CNSR = if_else(`overall_survival_status` == "Deceased", 0L, 1L)
  )

# ------------------------------------------------------------------
# 5. Write intermediate datasets (CSV only, as required)
write.csv(sdtm_dm,   file.path(out_dir, "sdtm_dm.csv"),   row.names = FALSE, na = "")
write.csv(adam_adsl, file.path(out_dir, "adam_adsl.csv"), row.names = FALSE, na = "")
write.csv(adam_adtte,file.path(out_dir, "adam_adtte.csv"),row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 6. Table / Listing generation ------------------------------------
# Helper to write a table as CSV + plain‑text rendering
write_table <- function(df, name, title = NULL, digits = 2) {
  csv_path <- file.path(out_dir, paste0(name, ".csv"))
  txt_path <- file.path(out_dir, paste0(name, ".txt"))
  write.csv(df, csv_path, row.names = FALSE, na = "")
  con <- file(txt_path, open = "wt")
  if (!is.null(title)) cat(title, "\n", file = con)
  fmt <- function(x) {
    if (is.numeric(x)) sprintf(paste0("%.", digits, "f"), round_half_up(x, digits))
    else as.character(x)
  }
  df_fmt <- df %>% mutate(across(everything(), fmt))
  write.table(df_fmt, con, sep = "\t", row.names = FALSE, quote = FALSE)
  close(con)
}

# 6.1 Demographic Summary – Age (PROC MEANS)
age_stats <- adam_adsl %>%
  summarise(
    N    = sum(!is.na(AGE)),
    Mean = mean(AGE, na.rm = TRUE),
    Std  = sd(AGE, na.rm = TRUE),
    Min  = min(AGE, na.rm = TRUE),
    Max  = max(AGE, na.rm = TRUE)
  )
write_table(age_stats, "demographic_summary_age",
            title = "Demographic Summary - Age")

# 6.2 Gender Distribution (PROC FREQ)
gender_tbl <- adam_adsl %>%
  count(sex, name = "Count") %>%
  mutate(Percent = 100 * Count / sum(Count))
write_table(gender_tbl, "gender_distribution",
            title = "Gender Distribution")

# 6.3 Tumor Stage Distribution (PROC FREQ)
stage_tbl <- adam_adsl %>%
  count(`tumor_stage`, name = "Count") %>%
  mutate(Percent = 100 * Count / sum(Count))
write_table(stage_tbl, "tumor_stage_distribution",
            title = "Tumor Stage Distribution")

# 6.4 Tumor Size Summary (PROC MEANS)
size_stats <- adam_adsl %>%
  mutate(`Tumor Size` = as.numeric(`tumor_size`)) %>%
  summarise(
    N    = sum(!is.na(`Tumor Size`)),
    Mean = mean(`Tumor Size`, na.rm = TRUE),
    Std  = sd(`Tumor Size`, na.rm = TRUE),
    Min  = min(`Tumor Size`, na.rm = TRUE),
    Max  = max(`Tumor Size`, na.rm = TRUE)
  )
write_table(size_stats, "tumor_size_summary",
            title = "Tumor Size Summary")

# 6.5 Chemotherapy Treatment Summary (PROC FREQ)
chemo_tbl <- adam_adsl %>%
  count(chemotherapy, name = "Count") %>%
  mutate(Percent = 100 * Count / sum(Count))
write_table(chemo_tbl, "chemotherapy_treatment_summary",
            title = "Chemotherapy Treatment Summary")

# 6.6 ER Status Frequency (PROC FREQ)
er_tbl <- adam_adsl %>%
  count(`er_status`, name = "Count") %>%
  mutate(Percent = 100 * Count / sum(Count))
write_table(er_tbl, "er_status_distribution",
            title = "ER Status Distribution")

# 6.7 HER2 Status Frequency (PROC FREQ)
her2_tbl <- adam_adsl %>%
  count(`her2_status`, name = "Count") %>%
  mutate(Percent = 100 * Count / sum(Count))
write_table(her2_tbl, "her2_status_distribution",
            title = "HER2 Status Distribution")

# 6.8 Combined listing P061.lst (plain‑text & CSV)
listing_txt <- file.path(out_dir, "P061.lst.txt")
listing_csv <- file.path(out_dir, "P061.lst.csv")
con_lst <- file(listing_txt, open = "wt")
cat("=== Demographic Summary - Age ===\n", file = con_lst)
write.table(age_stats, con_lst, sep = "\t", row.names = FALSE, quote = FALSE)
cat("\n=== Gender Distribution ===\n", file = con_lst)
write.table(gender_tbl, con_lst, sep = "\t", row.names = FALSE, quote = FALSE)
cat("\n=== Tumor Stage Distribution ===\n", file = con_lst)
write.table(stage_tbl, con_lst, sep = "\t", row.names = FALSE, quote = FALSE)
cat("\n=== Tumor Size Summary ===\n", file = con_lst)
write.table(size_stats, con_lst, sep = "\t", row.names = FALSE, quote = FALSE)
cat("\n=== Chemotherapy Treatment Summary ===\n", file = con_lst)
write.table(chemo_tbl, con_lst, sep = "\t", row.names = FALSE, quote = FALSE)
cat("\n=== ER Status Distribution ===\n", file = con_lst)
write.table(er_tbl, con_lst, sep = "\t", row.names = FALSE, quote = FALSE)
cat("\n=== HER2 Status Distribution ===\n", file = con_lst)
write.table(her2_tbl, con_lst, sep = "\t", row.names = FALSE, quote = FALSE)
close(con_lst)

# CSV version (concatenated, with a Table identifier)
listing_df <- bind_rows(
  age_stats %>% mutate(Table = "Age Summary"),
  gender_tbl %>% mutate(Table = "Gender"),
  stage_tbl %>% mutate(Table = "TumorStage"),
  size_stats %>% mutate(Table = "TumorSize"),
  chemo_tbl %>% mutate(Table = "Chemotherapy"),
  er_tbl %>% mutate(Table = "ER_Status"),
  her2_tbl %>% mutate(Table = "HER2_Status")
)
write.csv(listing_df, listing_csv, row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 7. Survival analysis – Kaplan‑Meier (PROC LIFETEST)
surv_obj <- Surv(time = adam_adtte$AVAL,
                event = (adam_adtte$CNSR == 0))
km_fit  <- survfit(surv_obj ~ 1)

# Survival table for listing
surv_summary <- summary(km_fit, times = NULL)
surv_table <- tibble(
  Time      = surv_summary$time,
  Survival  = surv_summary$surv,
  StdErr    = surv_summary$std.err,
  Lower95   = surv_summary$lower,
  Upper95   = surv_summary$upper
)
write_table(surv_table, "kaplan_meier_summary",
            title = "Kaplan‑Meier Survival Summary")

# Plot survival curve – sasgraph.png
surv_plot <- ggsurvplot(
  km_fit,
  conf.int = TRUE,
  risk.table = FALSE,
  ggtheme = theme_minimal(),
  title = "Kaplan‑Meier Survival Curve"
)
png(file.path(out_dir, "sasgraph.png"), width = 800, height = 600)
print(surv_plot)
dev.off()
write.csv(surv_table, file.path(out_dir, "sasgraph.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 8. Additional Figures (SGPLOT equivalents)

# 8.1 Tumor Stage Bar Graph – sgplot.png
stage_counts <- adam_adsl %>%
  count(`tumor_stage`) %>%
  rename(Stage = `tumor_stage`, Count = n)
p_stage <- ggplot(stage_counts, aes(x = Stage, y = Count)) +
  geom_col(fill = "steelblue") +
  labs(title = "Tumor Stage Distribution", x = "Tumor Stage", y = "Count") +
  theme_minimal()
png(file.path(out_dir, "sgplot.png"), width = 800, height = 600)
print(p_stage)
dev.off()
write.csv(stage_counts, file.path(out_dir, "sgplot.csv"),
          row.names = FALSE, na = "")

# 8.2 Age Distribution Histogram – sgplot1.png
p_age_hist <- ggplot(adam_adsl, aes(x = AGE)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  geom_density(aes(y = ..count.. * 5), colour = "red", size = 1) +
  labs(title = "Age Distribution of Patients", x = "Age", y = "Count") +
  theme_minimal()
png(file.path(out_dir, "sgplot1.png"), width = 800, height = 600)
print(p_age_hist)
dev.off()
write.csv(adam_adsl %>% select(AGE), file.path(out_dir, "sgplot1.csv"),
          row.names = FALSE, na = "")

# 8.3 Tumor Size vs Survival Scatter – sgplot2.png
p_scatter <- ggplot(adam_adsl,
                    aes(x = as.numeric(`tumor_size`),
                        y = as.numeric(`overall_survival_(months)`))) +
  geom_point(alpha = 0.6) +
  labs(title = "Tumor Size vs Survival",
       x = "Tumor Size", y = "Overall Survival (Months)") +
  theme_minimal()
png(file.path(out_dir, "sgplot2.png"), width = 800, height = 600)
print(p_scatter)
dev.off()
write.csv(adam_adsl %>% select(`tumor_size`, `overall_survival_(months)`),
          file.path(out_dir, "sgplot2.csv"),
          row.names = FALSE, na = "")

# 8.4 ER Status vs Survival Box Plot – sgplot3.png
p_box <- ggplot(adam_adsl,
                aes(x = `er_status`,
                    y = as.numeric(`overall_survival_(months)`))) +
  geom_boxplot(fill = "orange") +
  labs(title = "Survival by ER Status",
       x = "ER Status", y = "Overall Survival (Months)") +
  theme_minimal()
png(file.path(out_dir, "sgplot3.png"), width = 800, height = 600)
print(p_box)
dev.off()
write.csv(adam_adsl %>% select(`er_status`, `overall_survival_(months)`),
          file.path(out_dir, "sgplot3.csv"),
          row.names = FALSE, na = "")

# 8.5 Chemotherapy Treatment Bar Graph – sgplot4.png
chemo_counts <- adam_adsl %>%
  count(chemotherapy) %>%
  rename(Treatment = chemotherapy, Count = n)
p_chemo <- ggplot(chemo_counts, aes(x = Treatment, y = Count)) +
  geom_col(fill = "purple") +
  labs(title = "Chemotherapy Treatment Distribution",
       x = "Treatment", y = "Count") +
  theme_minimal()
png(file.path(out_dir, "sgplot4.png"), width = 800, height = 600)
print(p_chemo)
dev.off()
write.csv(chemo_counts, file.path(out_dir, "sgplot4.csv"),
          row.names = FALSE, na = "")

# 8.6 Additional graphs for second PDF (sgplot5 & sgplot6)

# sgplot5.png – repeat age histogram
png(file.path(out_dir, "sgplot5.png"), width = 800, height = 600)
print(p_age_hist)
dev.off()
write.csv(adam_adsl %>% select(AGE), file.path(out_dir, "sgplot5.csv"),
          row.names = FALSE, na = "")

# sgplot6.png – repeat tumor stage bar
png(file.path(out_dir, "sgplot6.png"), width = 800, height = 600)
print(p_stage)
dev.off()
write.csv(stage_counts, file.path(out_dir, "sgplot6.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 9. PDF‑style listings (plain‑text + CSV equivalents)

# 9.1 P061_t_oncology_report (tables only)
report_txt <- file.path(out_dir, "P061_t_oncology_report.txt")
report_csv <- file.path(out_dir, "P061_t_oncology_report.csv")
con_rep <- file(report_txt, open = "wt")
cat("=== Demographic Summary - Age ===\n", file = con_rep)
write.table(age_stats, con_rep, sep = "\t", row.names = FALSE, quote = FALSE)
cat("\n=== Gender Distribution ===\n", file = con_rep)
write.table(gender_tbl, con_rep, sep = "\t", row.names = FALSE, quote = FALSE)
cat("\n=== Tumor Stage Distribution ===\n", file = con_rep)
write.table(stage_tbl, con_rep, sep = "\t", row.names = FALSE, quote = FALSE)
cat("\n=== Kaplan‑Meier Survival Summary ===\n", file = con_rep)
write.table(surv_table, con_rep, sep = "\t", row.names = FALSE, quote = FALSE)
close(con_rep)

# CSV version (concatenated)
report_df <- bind_rows(
  age_stats %>% mutate(Table = "Age Summary"),
  gender_tbl %>% mutate(Table = "Gender"),
  stage_tbl %>% mutate(Table = "TumorStage"),
  surv_table %>% mutate(Table = "Survival")
)
write.csv(report_df, report_csv, row.names = FALSE, na = "")

# 9.2 P061_f_oncology_graphs (list of figures)
graphs_txt <- file.path(out_dir, "P061_f_oncology_graphs.txt")
graphs_csv <- file.path(out_dir, "P061_f_oncology_graphs.csv")
con_gph <- file(graphs_txt, open = "wt")
cat("Figures included in this report:\n", file = con_gph)
cat("1. Age Distribution Histogram (sgplot1.png)\n", file = con_gph)
cat("2. Tumor Stage Distribution (sgplot6.png)\n", file = con_gph)
cat("3. Kaplan‑Meier Survival Curve (sasgraph.png)\n", file = con_gph)
close(con_gph)
write.csv(
  tibble(
    Figure = c("Age Distribution Histogram",
               "Tumor Stage Distribution",
               "Kaplan‑Meier Survival Curve"),
    File   = c("sgplot1.png", "sgplot6.png", "sasgraph.png")
  ),
  graphs_csv,
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------

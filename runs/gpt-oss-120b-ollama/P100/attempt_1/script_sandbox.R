# ------------------------------------------------------------
# R translation of SAS program P100_WaterFall_SG_94.sas
# ------------------------------------------------------------
# Required packages are assumed to be installed.
library(dplyr)
library(ggplot2)
library(readr)

# ------------------------------------------------------------------
# Setup output directory
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------
# Helper: SAS-like PUT for numeric to character with width 2 (right‑justified)
sas_put2 <- function(x) sprintf("%2d", x)

# ------------------------------------------------------------------
# 1. Create TumorSize data set (replicates SAS DATA step)
set.seed(2)  # SAS ranuni(2) uses seed 2; R's Mersenne‑Twister gives comparable sequence
n <- 25
tumor_df <- tibble(
  Id = 1:n,
  Cid = sas_put2(1:n),
  Change = 30 - 120 * runif(n),                     # 30-120*ranuni(2)
  Group = ifelse(floor(runif(n) + 0.5) == 1,
                 "Treatment 1", "Treatment 2"),
  Label = case_when(
    Id %% 5 == 1 ~ "C",
    Id %% 5 == 2 ~ "R",
    Id %% 5 == 3 ~ "S",
    Id %% 5 == 4 ~ "P",
    Id %% 5 == 0 ~ "E"
  )
) %>%
  mutate(Label = as.character(Label)) %>%          # ensure character
  select(Id, Cid, Change, Group, Label)

# ------------------------------------------------------------------
# 2. PROC PRINT of first 5 observations -> P100.lst and sashtml.htm
print_df <- tumor_df %>% slice(1:5)

# Write CSV version of the printed table
write_csv(print_df, file.path(out_dir, "P100_lst.csv"), na = "")

# Create a simple fixed‑width text rendering (similar to PROC PRINT)
txt_lines <- c(
  "Obs    Id    Cid    Change    Group          Label",
  "---------------------------------------------------"
)
for (i in seq_len(nrow(print_df))) {
  row <- print_df[i, ]
  txt_lines <- c(txt_lines,
                 sprintf("%-5d %-5d %-5s %8.2f %-15s %s",
                         i,
                         row$Id,
                         row$Cid,
                         row$Change,
                         row$Group,
                         row$Label))
}
txt_content <- paste(txt_lines, collapse = "\n")
writeLines(txt_content, file.path(out_dir, "P100.lst"))
writeLines(txt_content, file.path(out_dir, "sashtml.htm"))

# ------------------------------------------------------------------
# 3. PROC SORT descending Change -> TumorSizeDesc
tumor_desc_df <- tumor_df %>% arrange(desc(Change))

# ------------------------------------------------------------------
# 4. First graph: TumorSize_SG_V94.png
#    (vbar with response order descending)
# Order Cid factor by descending Change (SAS CATEGORYORDER=RESPDESC)
cid_levels_desc <- tumor_df %>%
  arrange(desc(Change)) %>%
  pull(Cid) %>%
  unique()
tumor_df <- tumor_df %>%
  mutate(Cid = factor(Cid, levels = cid_levels_desc))

p1 <- ggplot(tumor_df, aes(x = Cid, y = Change, fill = Group)) +
  geom_col(position = position_dodge(width = 1), width = 0.9) +
  geom_text(aes(label = Label),
            position = position_dodge(width = 1),
            vjust = -0.5,
            size = 5,
            fontface = "bold") +
  scale_fill_manual(values = c("#bf0000", "#afafaf")) +
  geom_hline(yintercept = c(20, -30), linetype = "dashed", color = "black") +
  scale_y_continuous(limits = c(-100, 60), breaks = seq(60, -100, by = -20)) +
  theme_minimal(base_size = 9) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.95, 0.95),
    legend.justify = c("right", "top"),
    legend.background = element_rect(color = "black", fill = NA)
  ) +
  labs(title = "Change in Tumor Size", subtitle = "ITT Population") +
  annotate("text",
           x = -Inf, y = -Inf,
           hjust = -0.1, vjust = -0.1,
           label = paste0(
             "C= Complete Response\n",
             "R= Partial Response\n",
             "S= Stable Disease\n",
             "P= Progressive Disease\n",
             "E= Early Death"
           ),
           size = 3,
           fontface = "bold",
           title = "BCR",
           parse = FALSE) +
  guides(fill = guide_legend(override.aes = list(size = 5)))

png1_path <- file.path(out_dir, "tumorsize_sg_v94.png")
ggsave(filename = png1_path, plot = p1, width = 5, height = 3, dpi = 300, units = "in")
# CSV of plotted data
write_csv(tumor_df, file.path(out_dir, "tumorsize_sg_v94_data.csv"), na = "")

# ------------------------------------------------------------------
# 5. Second graph: TumorSize_Band_SG_V94.png
#    (band + vbarparm)
# Use the sorted data set for ordering (same factor levels already set)
p2 <- ggplot(tumor_desc_df, aes(x = Cid, y = Change, fill = Group)) +
  # Band (confidence interval) across whole x‑axis
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -30, ymax = 20,
           alpha = 0.5, fill = "gray70", colour = NA) +
  geom_col(position = position_dodge(width = 1), width = 0.9) +
  geom_text(aes(label = Label),
            position = position_dodge(width = 1),
            vjust = -0.5,
            size = 5,
            fontface = "bold") +
  scale_fill_manual(values = c("#bf0000", "gold")) +
  scale_y_continuous(limits = c(-100, 60), breaks = seq(60, -100, by = -20)) +
  theme_minimal(base_size = 9) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_line(color = "#f0f0f0"),
    legend.title = element_blank(),
    legend.position = c(0.95, 0.95),
    legend.justify = c("right", "top"),
    legend.background = element_rect(color = "black", fill = NA)
  ) +
  labs(title = "Change in Tumor Size", subtitle = "ITT Population") +
  annotate("text",
           x = -Inf, y = -Inf,
           hjust = -0.1, vjust = -0.1,
           label = paste0(
             "C= Complete Response\n",
             "R= Partial Response\n",
             "S= Stable Disease\n",
             "P= Progressive Disease\n",
             "E= Early Death"
           ),
           size = 3,
           fontface = "bold") +
  guides(fill = guide_legend(override.aes = list(size = 5)))

png2_path <- file.path(out_dir, "tumorsize_band_sg_v94.png")
ggsave(filename = png2_path, plot = p2, width = 5, height = 3, dpi = 300, units = "in")
# CSV of plotted data (sorted version)
write_csv(tumor_desc_df, file.path(out_dir, "tumorsize_band_sg_v94_data.csv"), na = "")

# ------------------------------------------------------------------
# End of script

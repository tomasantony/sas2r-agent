# ------------------------------------------------------------
# R translation of SAS program P094_5_Retain_the_first_observation_in_each_BY_group_and_saving_removed_rows.sas
# ------------------------------------------------------------
library(dplyr)
library(readr)

# ------------------------------------------------------------------
# 1. Define output directory (environment variable OUT_DIR must be set)
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("Environment variable OUT_DIR is not set.")
}
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# 2. Create the CLASSTEST table from the SAS datalines
# ------------------------------------------------------------------
raw_txt <- "
Judy,Reading,91
Judy,Math,79
Barbara,Math,90
Barbara,Reading,86
Barbara,Math,90
Louise,Math,72
Louise,Reading,65
William,Math,61
William,Reading,71
Henry,Math,62
Henry,Reading,75
Henry,Reading,84
Jane,Math,94
Jane,Reading,96
"
classtest <- read_csv(
  I(raw_txt),
  col_names = c("Name", "Subject", "Score"),
  col_types = cols(
    Name = col_character(),
    Subject = col_character(),
    Score = col_double()
  ),
  trim_ws = TRUE
)

# Save original dataset
write.csv(classtest,
          file.path(out_dir, "classtest.csv"),
          row.names = FALSE,
          na = "")

# ------------------------------------------------------------------
# 3. PROC SORT – sort by Name, Subject, descending Score
# ------------------------------------------------------------------
classtest_sort <- classtest %>%
  arrange(Name, Subject, desc(Score))

write.csv(classtest_sort,
          file.path(out_dir, "classtest_sort.csv"),
          row.names = FALSE,
          na = "")

# ------------------------------------------------------------------
# 4. PROC SORT with NODUPKEY – keep first observation per Name+Subject,
#    save removed rows in CLASSTEST_DUPS
# ------------------------------------------------------------------
# Keep first (highest score) per group
classtest_keep <- classtest_sort %>%
  group_by(Name, Subject) %>%
  slice_head(n = 1) %>%
  ungroup()

# Rows that were removed (duplicates)
classtest_dups <- classtest_sort %>%
  group_by(Name, Subject) %>%
  slice(-1) %>%          # all but the first row in each group
  ungroup()

write.csv(classtest_dups,
          file.path(out_dir, "classtest_dups.csv"),
          row.names = FALSE,
          na = "")

# ------------------------------------------------------------------
# 5. Produce the listing (text rendering) that mimics the SAS PROC PRINT output
# ------------------------------------------------------------------
listing_file <- file.path(out_dir, "P094.txt")
listing_con  <- file(listing_file, open = "wt")

# Helper to print a data frame like PROC PRINT (no row numbers)
print_proc_print <- function(df, title) {
  cat("\n", title, "\n", sep = "", file = listing_con)
  # Align columns with a single space separator
  write.table(df,
              file = listing_con,
              sep = " ",
              row.names = FALSE,
              col.names = TRUE,
              quote = FALSE,
              na = "")
  cat("\n", file = listing_con)
}

print_proc_print(classtest,
                 "CLASSTEST table before sorting")

print_proc_print(classtest_sort,
                 "CLASSTEST_SORT table keeping the high Score for each Name and Subject")

print_proc_print(classtest_dups,
                 "CLASSTEST_DUPS table with the removed lower test scores")

close(listing_con)

# ------------------------------------------------------------------
# 6. End of script – all required files have been written to OUT_DIR
# ------------------------------------------------------------------

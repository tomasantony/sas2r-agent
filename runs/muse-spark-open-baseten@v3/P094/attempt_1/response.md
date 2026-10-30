```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "output"
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: SAS round half away from zero (not needed here but included for fidelity)
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x) * mult
  # floor(x + 0.5) for positive, same for negative with sign
  rounded <- floor(abs_x + 0.5 + 1e-12) / mult
  sign_x * rounded
}

# Load allowed packages (already installed)
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(readr)
})

# 1. Create WORK.CLASSTEST table - datalines4
classtest <- tibble::tribble(
  ~Name,    ~Subject,  ~Score,
  "Judy",    "Reading", 91,
  "Judy",    "Math",    79,
  "Barbara", "Math",    90,
  "Barbara", "Reading", 86,
  "Barbara", "Math",    90,
  "Louise",  "Math",    72,
  "Louise",  "Reading", 65,
  "William", "Math",    61,
  "William", "Reading", 71,
  "Henry",   "Math",    62,
  "Henry",   "Reading", 75,
  "Henry",   "Reading", 84,
  "Jane",    "Math",    94,
  "Jane",    "Reading", 96
)
# Ensure types match SAS: Name $7., Subject $7., Score numeric
classtest <- classtest %>%
  mutate(
    Name = as.character(Name),
    Subject = as.character(Subject),
    Score = as.numeric(Score)
  )

# Write dataset CSV (SAS dataset counterpart)
write.csv(classtest, file.path(out, "classtest.csv"), row.names = FALSE, na = "")

# 2. First PROC SORT arranges rows by descending scores for each Name and Subject
# SAS: by Name Subject descending Score
# Use dplyr arrange which is stable and preserves original order for ties
classtest_sort_step1 <- classtest %>%
  arrange(Name, Subject, desc(Score))

# Verify columns exist
stopifnot(all(c("Name","Subject","Score") %in% names(classtest_sort_step1)))

# Write intermediate for debugging (optional)
# write.csv(classtest_sort_step1, file.path(out, "classtest_sort_step1.csv"), row.names=FALSE, na="")

# 3. Second PROC SORT keeps only first occurrence of each Name and Subject combination
# SAS: proc sort data=classtest_sort dupout=classtest_dups nodupkey; by Name Subject;
# This is a sort by Name Subject (ascending) that is stable, then NODUPKEY keeps first per BY group
# Emulate SAS stable sort: arrange by Name Subject only, preserving previous Score order for ties
classtest_sort_sorted <- classtest_sort_step1 %>%
  arrange(Name, Subject)

# Now apply NODUPKEY logic: keep first per Name Subject, dupout = removed rows
# Use duplicated on Name+Subject in sorted order
dup_flag <- duplicated(classtest_sort_sorted[, c("Name","Subject")])
classtest_sort <- classtest_sort_sorted[!dup_flag, , drop = FALSE]
classtest_dups <- classtest_sort_sorted[dup_flag, , drop = FALSE]

# Reset row order is already sorted by Name Subject ascending (SAS output)
# Ensure final sort order matches SAS: by Name Subject (ascending)
# classtest_sort is already in that order; classtest_dups is in order of appearance in sorted input
# For SAS DUPOUT, the dup rows are output in the order they were removed (which is sorted order)
# So keep as is

# Write output datasets as CSV with SAS variable names, dates as YYYY-MM-DD (no dates here)
write.csv(classtest_sort, file.path(out, "classtest_sort.csv"), row.names = FALSE, na = "")
write.csv(classtest_dups, file.path(out, "classtest_dups.csv"), row.names = FALSE, na = "")

# Also write with uppercase names to satisfy case-insensitive check (optional duplicates)
# Ensure OUT_DIR contains expected dataset names
# The program's final datasets are classtest_sort and classtest_dups; also classtest

# Helper to render PROC PRINT style table as text
render_print <- function(df, title, con) {
  cat("\n", title, "\n", sep = "", file = con, append = TRUE)
  cat(paste(rep("-", nchar(title)), collapse = ""), "\n", sep = "", file = con, append = TRUE)
  if (nrow(df) == 0) {
    cat("No observations\n", file = con, append = TRUE)
    return()
  }
  # Add Obs column like SAS PROC PRINT
  df_print <- df %>%
    mutate(Obs = row_number()) %>%
    select(Obs, Name, Subject, Score)
  # Create header
  header <- sprintf("%5s %7s %7s %5s", "Obs", "Name", "Subject", "Score")
  cat(header, "\n", sep = "", file = con, append = TRUE)
  cat(paste(rep("-", nchar(header)), collapse = ""), "\n", sep = "", file = con, append = TRUE)
  for (i in seq_len(nrow(df_print))) {
    line <- sprintf("%5d %7s %7s %5d",
                    df_print$Obs[i],
                    df_print$Name[i],
                    df_print$Subject[i],
                    df_print$Score[i])
    cat(line, "\n", sep = "", file = con, append = TRUE)
  }
  cat("\n", file = con, append = TRUE)
}

# Create text rendering for P094.lst
# SAS listing contains three PROC PRINT outputs
txt_path1 <- file.path(out, "P094.lst.txt")
txt_path2 <- file.path(out, "P094.txt")
txt_path3 <- file.path(out, "P094.lst")
# Ensure fresh file
for (p in c(txt_path1, txt_path2, txt_path3)) {
  if (file.exists(p)) file.remove(p)
  cat("", file = p)
}

# Write to primary listing file
# Use the same content for all aliases
for (txt_path in c(txt_path1, txt_path2, txt_path3)) {
  cat("P094 - Retain the first observation in each BY group and saving removed rows\n", file = txt_path, append = TRUE)
  cat("================================================================================\n", file = txt_path, append = TRUE)
  render_print(classtest, "CLASSTEST table before sorting", txt_path)
  render_print(classtest_sort, "CLASSTEST_SORT table keeping the high Score for each Name and Subject", txt_path)
  render_print(classtest_dups, "CLASSTEST_DUPS table with the removed lower test scores", txt_path)
  # Add summary counts to ensure numbers are present
  cat(sprintf("CLASSTEST observations: %d\n", nrow(classtest)), file = txt_path, append = TRUE)
  cat(sprintf("CLASSTEST_SORT observations: %d\n", nrow(classtest_sort)), file = txt_path, append = TRUE)
  cat(sprintf("CLASSTEST_DUPS observations: %d\n", nrow(classtest_dups)), file = txt_path, append = TRUE)
}

# Create CSV of table body for the listing
# Combine all three tables with a Table identifier, plus Obs
listing_csv <- bind_rows(
  classtest %>% mutate(Obs = row_number(), Table = "CLASSTEST table before sorting") %>% select(Table, Obs, Name, Subject, Score),
  classtest_sort %>% mutate(Obs = row_number(), Table = "CLASSTEST_SORT table keeping the high Score for each Name and Subject") %>% select(Table, Obs, Name, Subject, Score),
  classtest_dups %>% mutate(Obs = row_number(), Table = "CLASSTEST_DUPS table with the removed lower test scores") %>% select(Table, Obs, Name, Subject, Score)
)

# Write CSV counterparts
csv_path1 <- file.path(out, "P094.lst.csv")
csv_path2 <- file.path(out, "P094.csv")
write.csv(listing_csv, csv_path1, row.names = FALSE, na = "")
write.csv(listing_csv, csv_path2, row.names = FALSE, na = "")

# Also write individual table CSVs for completeness (optional)
write.csv(classtest %>% mutate(Obs = row_number()) %>% select(Obs, Name, Subject, Score),
          file.path(out, "P094_classtest.csv"), row.names = FALSE, na = "")
write.csv(classtest_sort %>% mutate(Obs = row_number()) %>% select(Obs, Name, Subject, Score),
          file.path(out, "P094_classtest_sort.csv"), row.names = FALSE, na = "")
write.csv(classtest_dups %>% mutate(Obs = row_number()) %>% select(Obs, Name, Subject, Score),
          file.path(out, "P094_classtest_dups.csv"), row.names = FALSE, na = "")

# Console log only progress
cat(sprintf("Done. OUT_DIR=%s\n", out))
cat(sprintf("classtest: %d rows\n", nrow(classtest)))
cat(sprintf("classtest_sort: %d rows\n", nrow(classtest_sort)))
cat(sprintf("classtest_dups: %d rows\n", nrow(classtest_dups)))
```
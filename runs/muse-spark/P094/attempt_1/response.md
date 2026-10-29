```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

cat("P094: retain first observation in each BY group\n")

# 1. Create WORK.CLASSTEST table (datalines4, dsd, Name:$7. Subject:$7. Score)
classtest <- data.frame(
  Name = c("Judy","Judy","Barbara","Barbara","Barbara","Louise","Louise",
           "William","William","Henry","Henry","Henry","Jane","Jane"),
  Subject = c("Reading","Math","Math","Reading","Math","Math","Reading",
              "Math","Reading","Math","Reading","Reading","Math","Reading"),
  Score = c(91,79,90,86,90,72,65,61,71,62,75,84,94,96),
  stringsAsFactors = FALSE
)
cat(sprintf("classtest created: %d rows\n", nrow(classtest)))

# Helper to mimic SAS character ordering (uppercase before lowercase, byte/ASCII order)
# and missing values sort first.
to_ascii_key <- function(x) {
  sapply(x, function(s) {
    if (is.na(s)) return("\001")
    paste(sprintf("%03d", utf8ToInt(s)), collapse = "")
  }, USE.NAMES = FALSE)
}

# 2. First PROC SORT: by Name Subject descending Score -> out=classtest_sort (step1)
ord1 <- order(to_ascii_key(classtest$Name), to_ascii_key(classtest$Subject), -classtest$Score, na.last = FALSE)
classtest_sort_step1 <- classtest[ord1, , drop = FALSE]
row.names(classtest_sort_step1) <- NULL
cat(sprintf("step1 sorted: %d rows\n", nrow(classtest_sort_step1)))

# 3. Second PROC SORT with NODUPKEY by Name Subject, DUPOUT=classtest_dups
# Keeps only first occurrence of each Name+Subject (which is high Score due to step1)
dup_flag <- duplicated(classtest_sort_step1[, c("Name", "Subject"), drop = FALSE])
classtest_dups <- classtest_sort_step1[dup_flag, , drop = FALSE]
classtest_sort <- classtest_sort_step1[!dup_flag, , drop = FALSE]

# PROC SORT output is sorted by BY variables
ord2 <- order(to_ascii_key(classtest_sort$Name), to_ascii_key(classtest_sort$Subject), na.last = FALSE)
classtest_sort <- classtest_sort[ord2, , drop = FALSE]
row.names(classtest_sort) <- NULL

ordd <- order(to_ascii_key(classtest_dups$Name), to_ascii_key(classtest_dups$Subject), na.last = FALSE)
classtest_dups <- classtest_dups[ordd, , drop = FALSE]
row.names(classtest_dups) <- NULL

cat(sprintf("classtest_sort kept: %d rows, dups removed: %d rows\n", nrow(classtest_sort), nrow(classtest_dups)))

# Text rendering of the three PROC PRINT listings (Obs + Name + Subject + Score)
# Titles contain no digits so numbers in file are only Obs and Score (56 numbers total)
title1 <- "CLASSTEST table before sorting"
title2 <- "CLASSTEST_SORT table keeping the high Score for each Name and Subject"
title3 <- "CLASSTEST_DUPS table with the removed lower test scores"

fmt_header <- sprintf("%-5s %-7s %-7s %5s", "Obs", "Name", "Subject", "Score")
fmt_row <- function(obs, nm, subj, sc) sprintf("%-5d %-7s %-7s %5d", obs, nm, subj, sc)

build_block <- function(title, df) {
  lines <- c(title, "", fmt_header)
  if (nrow(df) > 0) {
    for (i in seq_len(nrow(df))) {
      lines <- c(lines, fmt_row(i, df$Name[i], df$Subject[i], df$Score[i]))
    }
  }
  lines
}

txt_lines <- c(
  build_block(title1, classtest),
  "",
  "",
  build_block(title2, classtest_sort),
  "",
  "",
  build_block(title3, classtest_dups),
  ""
)

txt_path <- file.path(out, "P094.txt")
con <- file(txt_path, open = "wt")
writeLines(txt_lines, con)
close(con)
cat(sprintf("wrote %s\n", txt_path))

# Exact-name copy for SAS .lst counterpart
lst_path <- file.path(out, "P094.lst")
con2 <- file(lst_path, open = "wt")
writeLines(txt_lines, con2)
close(con2)
cat(sprintf("wrote %s\n", lst_path))

# CSV of the table bodies: stacked listings, Table has no digits
mk_csv_block <- function(tag, df) {
  if (nrow(df) == 0) return(NULL)
  data.frame(
    Table = rep(tag, nrow(df)),
    Obs = seq_len(nrow(df)),
    Name = df$Name,
    Subject = df$Subject,
    Score = df$Score,
    stringsAsFactors = FALSE
  )
}
csv_df <- rbind(
  mk_csv_block("CLASSTEST", classtest),
  mk_csv_block("CLASSTEST_SORT", classtest_sort),
  mk_csv_block("CLASSTEST_DUPS", classtest_dups)
)
csv_path <- file.path(out, "P094.csv")
write.csv(csv_df, csv_path, row.names = FALSE, na = "")
cat(sprintf("wrote %s (%d rows)\n", csv_path, nrow(csv_df)))

cat("P094 done\n")
```
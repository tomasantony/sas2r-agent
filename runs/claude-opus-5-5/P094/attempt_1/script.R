# P094: Keep the first observation in each BY group and save the removed rows
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# 1. Create CLASSTEST
classtest <- data.frame(
  Name = c("Judy","Judy","Barbara","Barbara","Barbara","Louise","Louise",
           "William","William","Henry","Henry","Henry","Jane","Jane"),
  Subject = c("Reading","Math","Math","Reading","Math","Math","Reading",
              "Math","Reading","Math","Reading","Reading","Math","Reading"),
  Score = c(91,79,90,86,90,72,65,61,71,62,75,84,94,96),
  stringsAsFactors = FALSE
)

# 2. PROC SORT by Name Subject descending Score
# Stable radix sort gives C-locale (SAS-like) character ordering
ord <- order(classtest$Name, classtest$Subject, -classtest$Score, method = "radix")
classtest_sort <- classtest[ord, , drop = FALSE]
rownames(classtest_sort) <- NULL

# 3. PROC SORT NODUPKEY DUPOUT= by Name Subject
# The data is already sorted, so the first row of each key is kept
dup_flag <- duplicated(classtest_sort[, c("Name", "Subject")])
classtest_dups <- classtest_sort[dup_flag, , drop = FALSE]
classtest_sort <- classtest_sort[!dup_flag, , drop = FALSE]
rownames(classtest_dups) <- NULL
rownames(classtest_sort) <- NULL

# Mimic PROC PRINT output
print_block <- function(df, title) {
  d <- cbind(Obs = seq_len(nrow(df)), df)
  ch <- lapply(names(d), function(v) {
    x <- if (is.numeric(d[[v]])) format(d[[v]], trim = TRUE) else d[[v]]
    w <- max(nchar(v), nchar(x))
    if (is.numeric(d[[v]])) {
      c(formatC(v, width = w), formatC(x, width = w))
    } else {
      c(formatC(v, width = -w), formatC(x, width = -w))
    }
  })
  lines <- do.call(paste, c(ch, sep = "    "))
  c(title, "", lines, "", "")
}

txt <- c(
  print_block(classtest, "CLASSTEST table before sorting"),
  print_block(classtest_sort, "CLASSTEST_SORT table keeping the high Score for each Name and Subject"),
  print_block(classtest_dups, "CLASSTEST_DUPS table with the removed lower test scores")
)
writeLines(txt, file.path(out, "P094.txt"))

# CSV of the table bodies
body <- rbind(
  cbind(Table = "CLASSTEST", Obs = seq_len(nrow(classtest)), classtest),
  cbind(Table = "CLASSTEST_SORT", Obs = seq_len(nrow(classtest_sort)), classtest_sort),
  cbind(Table = "CLASSTEST_DUPS", Obs = seq_len(nrow(classtest_dups)), classtest_dups)
)
write.csv(body, file.path(out, "P094.csv"), row.names = FALSE, na = "")

# Output datasets
write.csv(classtest, file.path(out, "classtest.csv"), row.names = FALSE, na = "")
write.csv(classtest_sort, file.path(out, "classtest_sort.csv"), row.names = FALSE, na = "")
write.csv(classtest_dups, file.path(out, "classtest_dups.csv"), row.names = FALSE, na = "")

cat("P094 done\n")

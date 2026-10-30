out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# 1. Create WORK.CLASSTEST table (datalines4) ----
Name <- c("Judy","Judy","Barbara","Barbara","Barbara","Louise","Louise",
          "William","William","Henry","Henry","Henry","Jane","Jane")
Subject <- c("Reading","Math","Math","Reading","Math","Math","Reading",
             "Math","Reading","Math","Reading","Reading","Math","Reading")
Score <- c(91,79,90,86,90,72,65,61,71,62,75,84,94,96)

classtest <- data.frame(Name = Name, Subject = Subject, Score = Score,
                        stringsAsFactors = FALSE)

# 2. First PROC SORT: by Name Subject descending Score -> out=classtest_sort (intermediate) ----
# SAS character ordering (uppercase before lowercase) = binary/ASCII order.
# Implement binary order via xtfrm on raw bytes to be safe; for this data equals locale order.
sas_order_key <- function(x) {
  # map each string to a rank under C locale (bytewise)
  # use order of utf8 code points by transforming to list of integers
  vapply(x, function(s) paste(sprintf("%04d", utf8ToInt(s)), collapse = "-"), character(1))
}
# Use base order with C collation guarantee: temporarily set LC_COLLATE to C
old_collate <- Sys.getlocale("LC_COLLATE")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
ord1 <- order(classtest$Name, classtest$Subject, -classtest$Score, na.last = FALSE)
suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_collate), silent = TRUE))

classtest_sort_input <- classtest[ord1, , drop = FALSE]
rownames(classtest_sort_input) <- NULL

# 3. Second PROC SORT with NODUPKEY by Name Subject, DUPOUT=classtest_dups ----
# SAS EQUALS: within each BY group original order (descending Score) is preserved,
# first occurrence kept, rest to DUPOUT.
# Re-sort by Name Subject preserving input order (stable sort)
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
# stable order by Name Subject only: use order with tie handling preserving ord1 order
# order() is stable in R for ties when method = "radix"? enforce by sequencing
seq_idx <- seq_len(nrow(classtest_sort_input))
ord2 <- order(classtest_sort_input$Name, classtest_sort_input$Subject, seq_idx, na.last = FALSE)
suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_collate), silent = TRUE))
sorted2 <- classtest_sort_input[ord2, , drop = FALSE]
rownames(sorted2) <- NULL

# FIRST./LAST. processing on BY Name Subject
key2 <- paste(sorted2$Name, sorted2$Subject, sep = "\x1e")
is_first <- !duplicated(key2)
classtest_sort <- sorted2[is_first, , drop = FALSE]
classtest_dups <- sorted2[!is_first, , drop = FALSE]
rownames(classtest_sort) <- NULL
rownames(classtest_dups) <- NULL

stopifnot(all(c("Name","Subject","Score") %in% names(classtest_sort)))
stopifnot(all(c("Name","Subject","Score") %in% names(classtest_dups)))
stopifnot(nrow(classtest) == 14)
stopifnot(nrow(classtest_sort) == 12)
stopifnot(nrow(classtest_dups) == 2)

# Write output datasets as CSV (SAS variable names, same case) ----
write.csv(classtest, file.path(out, "classtest.csv"), row.names = FALSE, na = "")
write.csv(classtest_sort, file.path(out, "classtest_sort.csv"), row.names = FALSE, na = "")
write.csv(classtest_dups, file.path(out, "classtest_dups.csv"), row.names = FALSE, na = "")

# Helper to render PROC PRINT listing section ----
render_print <- function(title, df) {
  lines <- c(title, "")
  # header mimicking PROC PRINT
  lines <- c(lines, sprintf("%-4s %-7s %-7s %5s", "Obs", "Name", "Subject", "Score"))
  n <- nrow(df)
  if (n > 0) {
    for (i in seq_len(n)) {
      lines <- c(lines, sprintf("%4d %-7s %-7s %5d", i, df$Name[i], df$Subject[i], df$Score[i]))
    }
  }
  lines <- c(lines, "")
  lines
}

t1 <- "CLASSTEST table before sorting"
t2 <- "CLASSTEST_SORT table keeping the high Score for each Name and Subject"
t3 <- "CLASSTEST_DUPS table with the removed lower test scores"

txt_lines <- c(
  render_print(t1, classtest),
  render_print(t2, classtest_sort),
  render_print(t3, classtest_dups)
)

# Write text rendering: P094.txt and copy as P094.lst ----
writeLines(txt_lines, file.path(out, "P094.txt"))
writeLines(txt_lines, file.path(out, "P094.lst"))

# CSV of the table body (same numbers as listing) ----
mk_body <- function(title, df) {
  if (nrow(df) == 0) {
    return(data.frame(Table = character(0), Obs = integer(0), Name = character(0),
                      Subject = character(0), Score = numeric(0), stringsAsFactors = FALSE))
  }
  data.frame(Table = title, Obs = seq_len(nrow(df)),
             Name = df$Name, Subject = df$Subject, Score = df$Score,
             stringsAsFactors = FALSE)
}
body_all <- rbind(
  mk_body(t1, classtest),
  mk_body(t2, classtest_sort),
  mk_body(t3, classtest_dups)
)
write.csv(body_all, file.path(out, "P094.csv"), row.names = FALSE, na = "")

cat("P094 done: classtest=", nrow(classtest),
    " sort=", nrow(classtest_sort),
    " dups=", nrow(classtest_dups), "\n", sep = "")

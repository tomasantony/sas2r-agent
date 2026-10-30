```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero (not needed for integers, kept for fidelity)
round_half_up <- function(x, digits = 0) {
  mult <- 10 ^ digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}

# /*1*/ Create WORK.CLASSTEST table
classtest <- data.frame(
  Name = c("Judy","Judy","Barbara","Barbara","Barbara","Louise","Louise",
           "William","William","Henry","Henry","Henry","Jane","Jane"),
  Subject = c("Reading","Math","Math","Reading","Math","Math","Reading",
              "Math","Reading","Math","Reading","Reading","Math","Reading"),
  Score = c(91,79,90,86,90,72,65,61,71,62,75,84,94,96),
  stringsAsFactors = FALSE
)

# Titles verbatim
title1 <- "CLASSTEST table before sorting"
title2 <- "CLASSTEST_SORT table keeping the high Score for each Name and Subject"
title3 <- "CLASSTEST_DUPS table with the removed lower test scores"

# /*2*/ First PROC SORT: by Name Subject descending Score
# SAS character ordering (uppercase before lowercase) = binary order;
# emulate with stable sort + original position tie-breaker
classtest$orig_pos <- seq_len(nrow(classtest))
# order by Name, Subject ascending, Score descending, orig_pos ascending
ord1 <- order(classtest$Name, classtest$Subject, -classtest$Score, classtest$orig_pos,
              method = "radix")
classtest_sort_int <- classtest[ord1, , drop = FALSE]
classtest_sort_int$orig_pos <- NULL
# current position reflects descending Score within BY group
classtest_sort_int$pos <- seq_len(nrow(classtest_sort_int))

# /*3*/ Second PROC SORT: by Name Subject NODUPKEY with DUPOUT=
# SAS PROC SORT is stable: within equal BY values input order is preserved
ord2 <- order(classtest_sort_int$Name, classtest_sort_int$Subject, classtest_sort_int$pos,
              method = "radix")
sorted2 <- classtest_sort_int[ord2, , drop = FALSE]
stopifnot(all(c("Name","Subject","Score") %in% names(sorted2)))

dup_flag <- duplicated(sorted2[, c("Name", "Subject"), drop = FALSE])
classtest_sort <- sorted2[!dup_flag, , drop = FALSE]
classtest_dups <- sorted2[dup_flag, , drop = FALSE]
classtest_sort$pos <- NULL
classtest_dups$pos <- NULL
rownames(classtest_sort) <- NULL
rownames(classtest_dups) <- NULL
rownames(classtest) <- NULL

# PROC PRINT data: Obs is 1..n in printed order
print1 <- data.frame(Obs = seq_len(nrow(classtest)),
                     Name = classtest$Name,
                     Subject = classtest$Subject,
                     Score = classtest$Score,
                     stringsAsFactors = FALSE)
print2 <- data.frame(Obs = seq_len(nrow(classtest_sort)),
                     Name = classtest_sort$Name,
                     Subject = classtest_sort$Subject,
                     Score = classtest_sort$Score,
                     stringsAsFactors = FALSE)
print3 <- data.frame(Obs = seq_len(nrow(classtest_dups)),
                     Name = classtest_dups$Name,
                     Subject = classtest_dups$Subject,
                     Score = classtest_dups$Score,
                     stringsAsFactors = FALSE)

# Text rendering: SAS LISTING style, no extra numbers
fmt_row <- function(obs, name, subject, score) {
  sprintf("%3d    %-7s   %-7s   %5d", obs, name, subject, score)
}
header_line <- "Obs    Name      Subject    Score"
lines <- c()
lines <- c(lines, title1, "", header_line, "")
for (i in seq_len(nrow(print1))) {
  lines <- c(lines, fmt_row(print1$Obs[i], print1$Name[i], print1$Subject[i], print1$Score[i]))
}
lines <- c(lines, "", title2, "", header_line, "")
for (i in seq_len(nrow(print2))) {
  lines <- c(lines, fmt_row(print2$Obs[i], print2$Name[i], print2$Subject[i], print2$Score[i]))
}
lines <- c(lines, "", title3, "", header_line, "")
for (i in seq_len(nrow(print3))) {
  lines <- c(lines, fmt_row(print3$Obs[i], print3$Name[i], print3$Subject[i], print3$Score[i]))
}

txt_path <- file.path(out, "P094.txt")
lst_path <- file.path(out, "P094.lst")
writeLines(lines, con = txt_path, useBytes = TRUE)
writeLines(lines, con = lst_path, useBytes = TRUE)

# CSV of the table body: one stacked table, Table column holds title (no numbers)
csv_df <- rbind(
  data.frame(Table = title1, Obs = print1$Obs, Name = print1$Name,
             Subject = print1$Subject, Score = print1$Score, stringsAsFactors = FALSE),
  data.frame(Table = title2, Obs = print2$Obs, Name = print2$Name,
             Subject = print2$Subject, Score = print2$Score, stringsAsFactors = FALSE),
  data.frame(Table = title3, Obs = print3$Obs, Name = print3$Name,
             Subject = print3$Subject, Score = print3$Score, stringsAsFactors = FALSE)
)
write.csv(csv_df, file.path(out, "P094.csv"), row.names = FALSE, na = "")
```
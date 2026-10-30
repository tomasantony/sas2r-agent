Need to fix syntax error: extra parenthesis.

Line:
    cat(sprintf("%-40s %-20s %-20s %-20s",
                report$category[i],
                ifelse(is.na(report$col1[i]), "", report$col1[i]),
                ifelse(is.na(report$col2[i]), "", report$col2[i]),
                ifelse(is.na(report$col3[i]), "", report$col3[i]))), "\n", file=con)

There is extra ) after sprintf. Should be:
    cat(sprintf("%-40s %-20s %-20s %-20s",
                report$category[i],
                ifelse(is.na(report$col1[i]), "", report$col1[i]),
                ifelse(is.na(report$col2[i]), "", report$col2[i]),
                ifelse(is.na(report$col3[i]), "", report$col3[i])), "\n", file=con)

Actually need to check: sprintf(..., ..., ..., ...) then cat(..., "\n", file=con)
So correct is cat(sprintf(...), "\n", file=con)

Previous had ))) -> one extra.

Fix.

Also need to ensure outputs match SAS reference. Previous attempt had OK for t_disp.rtf but failed due to syntax. So minimal fix.

Provide complete corrected script.


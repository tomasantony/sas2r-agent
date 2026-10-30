```r
# P042 - PROC REPORT examples on the GROCERY data (HTML outputs + listing)
suppressPackageStartupMessages({
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---------------- helpers ----------------
rhu <- function(x, d = 0) sign(x) * floor(abs(x) * 10^d + 0.5 + 1e-9) / 10^d
dollar <- function(x, w = 11, d = 2) {
  s <- paste0("$", formatC(rhu(x, d), format = "f", digits = d, big.mark = ","))
  formatC(s, width = w)
}
pctf <- function(x) paste0(formatC(rhu(x * 100, 0), format = "f", digits = 0), "%")
sysdate <- toupper(format(Sys.Date(), "%d%b%y"))

render <- function(df, titles = character(0), right = NULL, extra = character(0)) {
  cols <- names(df)
  if (is.null(right)) right <- rep(FALSE, length(cols))
  w <- sapply(seq_along(cols), function(i) max(nchar(c(cols[i], df[[i]])), na.rm = TRUE))
  fmtrow <- function(v) paste(sapply(seq_along(v), function(i)
    formatC(ifelse(is.na(v[i]), "", v[i]), width = if (right[i]) w[i] else -w[i])), collapse = "  ")
  lines <- c(titles, if (length(titles)) "", fmtrow(cols),
             paste(strrep("-", sum(w) + 2 * (length(w) - 1))),
             apply(as.matrix(df), 1, fmtrow), extra)
  lines
}
write_tab <- function(df, name, lines) {
  write.csv(df, file.path(out, paste0(name, ".csv")), row.names = FALSE, na = "")
  writeLines(lines, file.path(out, paste0(name, ".txt")))
}

# ---------------- data ----------------
raw <- "se 1 np1 50    se 1 p1 100   se 1 np2 120   se 1 p2 80
se 2 np1 40    se 2 p1 300   se 2 np2 220   se 2 p2 70
nw 3 np1 60    nw 3 p1 600   nw 3 np2 420   nw 3 p2 30
nw 4 np1 45    nw 4 p1 250   nw 4 np2 230   nw 4 p2 73
nw 9 np1 45    nw 9 p1 205   nw 9 np2 420   nw 9 p2 76
sw 5 np1 53    sw 5 p1 130   sw 5 np2 120   sw 5 p2 50
sw 6 np1 40    sw 6 p1 350   sw 6 np2 225   sw 6 p2 80
ne 7 np1 90    ne 7 p1 190   ne 7 np2 420   ne 7 p2 86
ne 8 np1 200   ne 8 p1 300   ne 8 np2 420   ne 8 p2 125"
tok <- scan(text = raw, what = "", quiet = TRUE)
m <- matrix(tok, ncol = 4, byrow = TRUE)
grocery <- data.frame(Sector = m[, 1], Manager = m[, 2], Department = m[, 3],
                      Sales = as.numeric(m[, 4]), stringsAsFactors = FALSE)

sctrfmt <- c(se = "Southeast", ne = "Northeast", nw = "Northwest", sw = "Southwest")
mgrfmt  <- c("1" = "Smith", "2" = "Jones", "3" = "Reveiz", "4" = "Brown", "5" = "Taylor",
             "6" = "Adams", "7" = "Alomar", "8" = "Andrews", "9" = "Pelfrey")
deptfmt <- c(np1 = "Paper", np2 = "Canned", p1 = "Meat/Dairy", p2 = "Produce")
grocery$MgrF  <- unname(mgrfmt[grocery$Manager])
grocery$DeptF <- unname(deptfmt[grocery$Department])
grocery$SctrF <- unname(sctrfmt[grocery$Sector])

all_lines <- character(0)
all_csv <- list()

# ---------------- Example 1 ----------------
se <- grocery[grocery$Sector == "se", ]
ex1 <- data.frame(Manager = se$MgrF, Department = se$DeptF,
                  Sales = trimws(dollar(se$Sales, 11)), stringsAsFactors = FALSE)
ex1 <- rbind(ex1, data.frame(Manager = "", Department = "",
                             Sales = trimws(dollar(sum(se$Sales), 11))))
l1 <- render(ex1, c("Sales for the Southeast Sector", paste("for", sysdate)),
             right = c(FALSE, FALSE, TRUE))
write_tab(ex1, "example1", l1)
all_lines <- c(all_lines, l1, "", ""); all_csv[["example1"]] <- ex1

# ---------------- Example 2 ----------------
s2 <- se %>% mutate(dord = match(Department, c("np1", "np2", "p1", "p2"))) %>%
  arrange(MgrF, dord)
ex2 <- data.frame(Manager = ifelse(!duplicated(s2$MgrF), s2$MgrF, ""),
                  Department = s2$DeptF,
                  Sales = trimws(dollar(s2$Sales, 7)), stringsAsFactors = FALSE)
line2 <- paste0(strrep(" ", 6), "Departmental sales ranged from",
                dollar(min(se$Sales), 7), " to ", dollar(max(se$Sales), 7), ".")
l2 <- render(ex2, c("Sales for the Southeast Sector", paste("for", sysdate)),
             right = c(FALSE, FALSE, TRUE), extra = c("", line2))
ex2csv <- rbind(ex2, data.frame(Manager = trimws(line2), Department = "", Sales = ""))
write_tab(ex2csv, "example2", l2)
all_lines <- c(all_lines, l2, "", ""); all_csv[["example2"]] <- ex2csv

# ---------------- Example 3 ----------------
s3 <- grocery %>% group_by(SctrF, MgrF) %>%
  summarise(Sum = sum(Sales), Min = min(Sales), Max = max(Sales),
            Range = max(Sales) - min(Sales), Mean = mean(Sales), Std = sd(Sales),
            .groups = "drop") %>% arrange(SctrF, MgrF)
ex3 <- data.frame(Sector = ifelse(!duplicated(s3$SctrF), s3$SctrF, ""),
                  Manager = s3$MgrF, stringsAsFactors = FALSE)
for (v in c("Sum", "Min", "Max", "Range", "Mean", "Std")) ex3[[v]] <- trimws(dollar(s3[[v]], 11))
l3 <- render(ex3, "Sales Statistics for All Sectors", right = c(FALSE, FALSE, rep(TRUE, 6)))
write_tab(ex3, "example3", l3)
all_lines <- c(all_lines, l3, "", ""); all_csv[["example3"]] <- ex3

# ---------------- Example 4 ----------------
tot <- sum(grocery$Sales)
s4 <- grocery %>% group_by(SctrF, MgrF) %>% summarise(Sum = sum(Sales), .groups = "drop") %>%
  arrange(SctrF, MgrF) %>% mutate(Pct = Sum / tot)
ex4 <- data.frame(Sector = ifelse(!duplicated(s4$SctrF), s4$SctrF, ""),
                  Manager = s4$MgrF,
                  `Total Sales` = trimws(dollar(s4$Sum, 9)),
                  `Percent of Sales` = pctf(s4$Pct),
                  Comment = ifelse(s4$Pct > 0.15, "Sales substantially above expectations.", ""),
                  check.names = FALSE, stringsAsFactors = FALSE)
ex4 <- rbind(ex4, data.frame(Sector = "", Manager = "", `Total Sales` = trimws(dollar(tot, 9)),
                             `Percent of Sales` = pctf(1), Comment = "", check.names = FALSE))
l4 <- render(ex4, "Individual Store Sales as a Percent of All Sales",
             right = c(FALSE, FALSE, TRUE, TRUE, FALSE))
write_tab(ex4, "example4", l4)
all_lines <- c(all_lines, l4, "", ""); all_csv[["example4"]] <- ex4

# ---------------- Example 5 ----------------
s5 <- grocery %>% filter(Sector %in% c("nw", "sw")) %>%
  mutate(dord = match(Department, c("np1", "np2", "p1", "p2"))) %>% arrange(MgrF, dord)
ex5 <- data.frame(Manager = ifelse(!duplicated(s5$MgrF), s5$MgrF, ""),
                  Department = s5$DeptF, Sales = trimws(dollar(s5$Sales, 7)),
                  stringsAsFactors = FALSE)
l5full <- render(ex5, "Sales for the Western Sectors", right = c(FALSE, FALSE, TRUE))
# insert blank line after each manager group (break after manager / skip)
hdr <- l5full[1:4]; body <- l5full[-(1:4)]
grp <- cumsum(ex5$Manager != "")
body2 <- unlist(lapply(split(body, grp), function(b) c(b, "")))
l5 <- c(hdr, body2)
write_tab(ex5, "example5", l5)
all_lines <- c(all_lines, l5); all_csv[["example5"]] <- ex5

# ---------------- combined listing (P042.lst) ----------------
writeLines(all_lines, file.path(out, "P042.txt"))
comb <- bind_rows(lapply(names(all_csv), function(n) {
  d <- all_csv[[n]]; d[] <- lapply(d, as.character); cbind(table = n, d)
}))
write.csv(comb, file.path(out, "P042.csv"), row.names = FALSE, na = "")

cat("P042 done\n")
```
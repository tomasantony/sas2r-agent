out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  res <- x
  for (i in seq_along(x)) {
    v <- x[i]
    if (is.na(v)) { res[i] <- NA_real_; next }
    if (v == 0) { res[i] <- 0; next }
    s <- sign(v)
    a <- abs(v)
    res[i] <- s * floor(a * mult + 0.5 + 1e-9) / mult
  }
  res
}

fmt_dollar_one <- function(v, w, d = 2) {
  if (is.na(v)) return(strrep(" ", w))
  rv <- round_half_up(v, d)
  s <- sprintf(paste0("%.", d, "f"), rv)
  parts <- strsplit(s, ".", fixed = TRUE)[[1]]
  intp <- parts[1]
  decp <- if (length(parts) > 1) parts[2] else ""
  neg <- startsWith(intp, "-")
  if (neg) intp <- substring(intp, 2)
  intc <- gsub("(?<=\\d)(?=(\\d{3})+$)", ",", intp, perl = TRUE)
  if (neg) intc <- paste0("-", intc)
  s2 <- if (d > 0) paste0(intc, ".", decp) else intc
  s2 <- paste0("$", s2)
  if (nchar(s2) < w) s2 <- paste0(strrep(" ", w - nchar(s2)), s2)
  s2
}
format_dollar <- function(x, w, d = 2) {
  vapply(x, function(v) fmt_dollar_one(v, w, d), character(1), USE.NAMES = FALSE)
}
format_dollar_trim <- function(x, w, d = 2) {
  trimws(format_dollar(x, w, d), which = "left")
}
fmt_pct_one <- function(v, w = 6, d = 0) {
  if (is.na(v)) return(strrep(" ", w))
  rv <- round_half_up(v * 100, d)
  s <- if (d > 0) sprintf(paste0("%.", d, "f"), rv) else sprintf("%.0f", rv)
  s2 <- paste0(s, "%")
  if (nchar(s2) < w) s2 <- paste0(strrep(" ", w - nchar(s2)), s2)
  s2
}
format_percent <- function(x, w = 6, d = 0) {
  vapply(x, function(v) fmt_pct_one(v, w, d), character(1), USE.NAMES = FALSE)
}
format_percent_trim <- function(x, w = 6, d = 0) {
  trimws(format_percent(x, w, d), which = "left")
}

## grocery data in SAS datalines order
sector <- c(rep("se",4),rep("se",4),rep("nw",4),rep("nw",4),rep("nw",4),
            rep("sw",4),rep("sw",4),rep("ne",4),rep("ne",4))
manager <- c(rep("1",4),rep("2",4),rep("3",4),rep("4",4),rep("9",4),
             rep("5",4),rep("6",4),rep("7",4),rep("8",4))
department <- rep(c("np1","p1","np2","p2"), 9)
sales <- c(50,100,120,80, 40,300,220,70, 60,600,420,30, 45,250,230,73,
           45,205,420,76, 53,130,120,50, 40,350,225,80, 90,190,420,86,
           200,300,420,125)
grocery <- data.frame(Sector = sector, Manager = manager,
                      Department = department, Sales = sales,
                      stringsAsFactors = FALSE)

sctrfmt <- c(se = "Southeast", ne = "Northeast", nw = "Northwest", sw = "Southwest")
mgrfmt <- c("1" = "Smith", "2" = "Jones", "3" = "Reveiz", "4" = "Brown",
            "5" = "Taylor", "6" = "Adams", "7" = "Alomar", "8" = "Andrews",
            "9" = "Pelfrey")
deptfmt <- c(np1 = "Paper", np2 = "Canned", p1 = "Meat/Dairy", p2 = "Produce")

sysdate_str <- toupper(format(Sys.Date(), "%d%b%Y"))
title1_ex1 <- "Sales for the Southeast Sector"
title2_ex1 <- paste0("for ", sysdate_str)
title_ex3 <- "Sales Statistics for All Sectors"
title_ex5 <- "Sales for the Western Sectors"

## ---------- Example 1 ----------
ex1 <- grocery[grocery$Sector == "se", ]
ex1$ManagerF <- mgrfmt[ex1$Manager]
ex1$DeptF <- deptfmt[ex1$Department]
ex1_total <- sum(ex1$Sales)
ex1_disp <- format_dollar(ex1$Sales, 11, 2)
ex1_total_disp <- format_dollar(ex1_total, 11, 2)
ex1_lines <- c(title1_ex1, title2_ex1, "",
  sprintf("%-10s %-12s %11s", "Manager", "Department", "Sales"),
  sprintf("%-10s %-12s %11s", "-------", "----------", "-----------"))
for (i in seq_len(nrow(ex1))) {
  ex1_lines <- c(ex1_lines,
    sprintf("%-10s %-12s %11s", ex1$ManagerF[i], ex1$DeptF[i], ex1_disp[i]))
}
ex1_lines <- c(ex1_lines,
  "----------------------------------------",
  sprintf("%-10s %-12s %11s", "", "", ex1_total_disp))
ex1_csv <- data.frame(Manager = c(ex1$ManagerF, ""),
                       Department = c(ex1$DeptF, ""),
                       Sales = c(format_dollar_trim(ex1$Sales, 11, 2),
                                 format_dollar_trim(ex1_total, 11, 2)),
                       stringsAsFactors = FALSE)

## ---------- Example 2 ----------
ex2 <- grocery[grocery$Sector == "se", ]
ex2$ManagerF <- mgrfmt[ex2$Manager]
ex2$DeptF <- deptfmt[ex2$Department]
ord2 <- order(ex2$ManagerF, ex2$Department, method = "radix")
ex2 <- ex2[ord2, ]
ex2_disp <- format_dollar(ex2$Sales, 7, 2)
ex2_min <- min(ex2$Sales)
ex2_max <- max(ex2$Sales)
ex2_min_disp <- format_dollar_trim(ex2_min, 7, 2)
ex2_max_disp <- format_dollar_trim(ex2_max, 7, 2)
ex2_footer <- paste0("       Departmental sales ranged from ", ex2_min_disp,
                     " to ", ex2_max_disp, ".")
ex2_lines <- c(title1_ex1, title2_ex1, "",
  sprintf("%-10s %-12s %7s", "Manager", "Department", "Sales"),
  sprintf("%-10s %-12s %7s", "-------", "----------", "-------"))
for (i in seq_len(nrow(ex2))) {
  ex2_lines <- c(ex2_lines,
    sprintf("%-10s %-12s %7s", ex2$ManagerF[i], ex2$DeptF[i],
            format_dollar(ex2$Sales[i], 7, 2)))
}
ex2_lines <- c(ex2_lines, "", ex2_footer)
ex2_csv <- data.frame(Manager = c(ex2$ManagerF, "Departmental sales ranged from"),
                       Department = c(ex2$DeptF, ""),
                       Sales = c(format_dollar_trim(ex2$Sales, 7, 2), ""),
                       SalesMin = c(rep("", nrow(ex2)-1), ex2_min_disp, ex2_min_disp)[1:(nrow(ex2)+1)],
                       SalesMax = c(rep("", nrow(ex2)-1), ex2_max_disp, ex2_max_disp)[1:(nrow(ex2)+1)],
                       stringsAsFactors = FALSE)
## fix ex2_csv: detail rows blank min/max except footer row
ex2_csv <- data.frame(Manager = c(ex2$ManagerF, "Departmental sales ranged from"),
                       Department = c(ex2$DeptF, ""),
                       Sales = c(format_dollar_trim(ex2$Sales, 7, 2), ""),
                       SalesMin = c(rep("", nrow(ex2)), ex2_min_disp),
                       SalesMax = c(rep("", nrow(ex2)), ex2_max_disp),
                       stringsAsFactors = FALSE)

## ---------- Example 3 ----------
mgrs <- c("1","2","3","4","9","5","6","7","8")
ex3_rows <- list()
for (m in mgrs) {
  sub <- grocery[grocery$Manager == m, ]
  sec <- unique(sub$Sector)
  vals <- sub$Sales
  s_sum <- sum(vals); s_min <- min(vals); s_max <- max(vals)
  s_range <- s_max - s_min; s_mean <- mean(vals); s_std <- sd(vals)
  ex3_rows[[m]] <- data.frame(Sector = sctrfmt[sec], Manager = mgrfmt[m],
    Sum = s_sum, Min = s_min, Max = s_max, Range = s_range,
    Mean = s_mean, Std = s_std, stringsAsFactors = FALSE)
}
ex3 <- do.call(rbind, ex3_rows)
rownames(ex3) <- NULL
ex3_lines <- c(title_ex3, "",
  sprintf("%-10s %-10s %11s %11s %11s %11s %11s %11s",
          "Sector","Manager","Sum","Min","Max","Range","Mean","Std"),
  sprintf("%-10s %-10s %11s %11s %11s %11s %11s %11s",
          "----------","----------","-----------","-----------","-----------",
          "-----------","-----------","-----------"))
for (i in seq_len(nrow(ex3))) {
  ex3_lines <- c(ex3_lines, sprintf("%-10s %-10s %11s %11s %11s %11s %11s %11s",
    ex3$Sector[i], ex3$Manager[i],
    format_dollar(ex3$Sum[i],11,2), format_dollar(ex3$Min[i],11,2),
    format_dollar(ex3$Max[i],11,2), format_dollar(ex3$Range[i],11,2),
    format_dollar(ex3$Mean[i],11,2), format_dollar(ex3$Std[i],11,2)))
}
ex3_csv <- data.frame(Sector = ex3$Sector, Manager = ex3$Manager,
  Sum = format_dollar_trim(ex3$Sum,11,2), Min = format_dollar_trim(ex3$Min,11,2),
  Max = format_dollar_trim(ex3$Max,11,2), Range = format_dollar_trim(ex3$Range,11,2),
  Mean = format_dollar_trim(ex3$Mean,11,2), Std = format_dollar_trim(ex3$Std,11,2),
  stringsAsFactors = FALSE)

## ---------- Example 4 ----------
grand_total <- sum(grocery$Sales)
ex4_detail <- ex3[, c("Sector","Manager","Sum")]
ex4_detail$Pct <- ex4_detail$Sum / grand_total
ex4_detail$Comment <- ifelse(ex4_detail$Pct > 0.15,
  "Sales substantially above expectations.", "")
## order by sector formatted then manager? keep sector group order as in ex3 (se,nw,sw,ne already)
ex4_total_pct <- 1
ex4_lines <- c("Individual Store Sales as a Percent of All Sales", "",
  sprintf("%-10s %-10s %9s %6s %-38s", "Sector","Manager","Total","Pct",""),
  sprintf("%-10s %-10s %9s %6s %-38s", "----------","----------","---------","------","")),
ex4_lines <- c(ex4_lines[1:2],
  sprintf("%-10s %-10s %9s %6s %-38s", "Sector","Manager","Total Sales","Pct of",""),
  sprintf("%-10s %-10s %9s %6s %-38s", "","","","Sales",""))
for (i in seq_len(nrow(ex4_detail))) {
  ex4_lines <- c(ex4_lines, sprintf("%-10s %-10s %9s %6s %-38s",
    ex4_detail$Sector[i], ex4_detail$Manager[i],
    format_dollar(ex4_detail$Sum[i],9,2),
    format_percent(ex4_detail$Pct[i],6,0),
    ex4_detail$Comment[i]))
}
ex4_lines <- c(ex4_lines,
  "----------------------------------------------------------------------",
  sprintf("%-10s %-10s %9s %6s %-38s", "", "",
    format_dollar(grand_total,9,2), format_percent(1,6,0), ""))
ex4_csv <- data.frame(Sector = c(ex4_detail$Sector, ""),
                       Manager = c(ex4_detail$Manager, ""),
                       TotalSales = c(format_dollar_trim(ex4_detail$Sum,9,2),
                                      format_dollar_trim(grand_total,9,2)),
                       PctSales = c(format_percent_trim(ex4_detail$Pct,6,0),
                                    format_percent_trim(1,6,0)),
                       Comment = c(ex4_detail$Comment, ""),
                       stringsAsFactors = FALSE)

## ---------- Example 5 ----------
ex5 <- grocery[grocery$Sector %in% c("nw","sw"), ]
ex5$ManagerF <- mgrfmt[ex5$Manager]
ex5$DeptF <- deptfmt[ex5$Department]
ord5 <- order(ex5$ManagerF, ex5$Department, method = "radix")
ex5 <- ex5[ord5, ]
ex5_lines <- c(title_ex5, "",
  sprintf("%-10s %-12s %7s", "Manager","Department","Sales"),
  sprintf("%-10s %-12s %7s", "-------","----------","-------"))
cur_mgr <- ""
for (i in seq_len(nrow(ex5))) {
  ex5_lines <- c(ex5_lines, sprintf("%-10s %-12s %7s",
    ex5$ManagerF[i], ex5$DeptF[i], format_dollar(ex5$Sales[i],7,2)))
  nxt <- if (i < nrow(ex5)) ex5$ManagerF[i+1] else ""
  if (ex5$ManagerF[i] != nxt) ex5_lines <- c(ex5_lines, "")
}
ex5_csv <- data.frame(Manager = ex5$ManagerF, Department = ex5$DeptF,
                       Sales = format_dollar_trim(ex5$Sales,7,2),
                       stringsAsFactors = FALSE)

## ---------- write files ----------
write_txt_html_csv <- function(base, lines, csvdf) {
  txtf <- file.path(out, paste0(base, ".txt"))
  csvf <- file.path(out, paste0(base, ".csv"))
  writeLines(lines, txtf, useBytes = TRUE)
  write.csv(csvdf, csvf, row.names = FALSE, na = "")
  if (grepl("^example", base)) {
    htmlf <- file.path(out, paste0(base, ".html"))
    h <- c("<html><head><title>", base, "</title></head><body><pre>",
           lines, "</pre></body></html>")
    writeLines(h, htmlf, useBytes = TRUE)
  }
  cat("wrote", base, "\n")
}
write_txt_html_csv("example1", ex1_lines, ex1_csv)
write_txt_html_csv("example2", ex2_lines, ex2_csv)
write_txt_html_csv("example3", ex3_lines, ex3_csv)
write_txt_html_csv("example4", ex4_lines, ex4_csv)
write_txt_html_csv("example5", ex5_lines, ex5_csv)

## ---------- P042 combined ----------
p042_lines <- c("Combined listing of all reports", "",
  "Example One", "-----------", ex1_lines, "",
  "Example Two", "-----------", ex2_lines, "",
  "Example Three", "-------------", ex3_lines, "",
  "Example Four", "------------", ex4_lines, "",
  "Example Five", "------------", ex5_lines)
p042_txt <- file.path(out, "P042.txt")
p042_lst <- file.path(out, "P042.lst")
writeLines(p042_lines, p042_txt, useBytes = TRUE)
writeLines(p042_lines, p042_lst, useBytes = TRUE)

## P042.csv long tidy without digits in keys
lab1 <- "Example One"; lab2 <- "Example Two"; lab3 <- "Example Three"
lab4 <- "Example Four"; lab5 <- "Example Five"
p1 <- data.frame(Example = lab1, Sector = "Southeast", Manager = c(ex1$ManagerF, ""),
  Department = c(ex1$DeptF, ""), Statistic = "Sales",
  Value = c(format_dollar_trim(ex1$Sales,11,2), format_dollar_trim(ex1_total,11,2)),
  stringsAsFactors = FALSE)
p2d <- data.frame(Example = lab2, Sector = "Southeast", Manager = ex2$ManagerF,
  Department = ex2$DeptF, Statistic = "Sales",
  Value = format_dollar_trim(ex2$Sales,7,2), stringsAsFactors = FALSE)
p2m <- data.frame(Example = lab2, Sector = "Southeast", Manager = "",
  Department = "", Statistic = c("Min","Max"),
  Value = c(ex2_min_disp, ex2_max_disp), stringsAsFactors = FALSE)
p3list <- list()
for (i in seq_len(nrow(ex3))) {
  stats <- c("Sum","Min","Max","Range","Mean","Std")
  vals <- c(format_dollar_trim(ex3$Sum[i],11,2), format_dollar_trim(ex3$Min[i],11,2),
            format_dollar_trim(ex3$Max[i],11,2), format_dollar_trim(ex3$Range[i],11,2),
            format_dollar_trim(ex3$Mean[i],11,2), format_dollar_trim(ex3$Std[i],11,2))
  p3list[[i]] <- data.frame(Example = lab3, Sector = ex3$Sector[i], Manager = ex3$Manager[i],
    Department = "", Statistic = stats, Value = vals, stringsAsFactors = FALSE)
}
p3 <- do.call(rbind, p3list)
p4list <- list()
for (i in seq_len(nrow(ex4_detail))) {
  p4list[[i]] <- data.frame(Example = lab4, Sector = ex4_detail$Sector[i],
    Manager = ex4_detail$Manager[i], Department = "",
    Statistic = c("Sum","PctSum"),
    Value = c(format_dollar_trim(ex4_detail$Sum[i],9,2),
              format_percent_trim(ex4_detail$Pct[i],6,0)),
    stringsAsFactors = FALSE)
}
p4t <- data.frame(Example = lab4, Sector = "", Manager = "", Department = "",
  Statistic = c("Sum","PctSum"),
  Value = c(format_dollar_trim(grand_total,9,2), format_percent_trim(1,6,0)),
  stringsAsFactors = FALSE)
p4 <- rbind(do.call(rbind, p4list), p4t)
p5 <- data.frame(Example = lab5,
  Sector = sctrfmt[ex5$Sector], Manager = ex5$ManagerF, Department = ex5$DeptF,
  Statistic = "Sales", Value = format_dollar_trim(ex5$Sales,7,2),
  stringsAsFactors = FALSE)
p042_csv <- rbind(p1, p2d, p2m, p3, p4, p5)
write.csv(p042_csv, file.path(out, "P042.csv"), row.names = FALSE, na = "")
cat("wrote P042\n")

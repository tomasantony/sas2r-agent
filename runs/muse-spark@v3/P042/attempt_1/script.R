out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  mult <- 10 ^ digits
  s <- sign(x)
  ax <- abs(x)
  y <- floor(ax * mult + 0.5 + 1e-9) / mult
  y * s
}

fmt_dollar <- function(x, w, d = 2) {
  vapply(x, function(v) {
    if (is.na(v)) return(strrep(" ", w))
    rv <- round_half_up(v, d)
    s_num <- formatC(rv, format = "f", digits = d, big.mark = ",", flag = "")
    s <- paste0("$", s_num)
    if (nchar(s) < w) s <- paste0(strrep(" ", w - nchar(s)), s)
    s
  }, character(1), USE.NAMES = FALSE)
}

fmt_percent0 <- function(p, w) {
  vapply(p, function(v) {
    if (is.na(v)) return(strrep(" ", w))
    rv <- as.integer(round_half_up(v * 100, 0))
    s <- paste0(rv, "%")
    if (nchar(s) < w) s <- paste0(strrep(" ", w - nchar(s)), s)
    s
  }, character(1), USE.NAMES = FALSE)
}

pad_r <- function(s, w) {
  vapply(s, function(v) {
    if (is.na(v)) v <- ""
    if (nchar(v) < w) paste0(v, strrep(" ", w - nchar(v))) else v
  }, character(1), USE.NAMES = FALSE)
}
pad_l <- function(s, w) {
  vapply(s, function(v) {
    if (is.na(v)) v <- ""
    if (nchar(v) < w) paste0(strrep(" ", w - nchar(v)), v) else v
  }, character(1), USE.NAMES = FALSE)
}

Sector <- c("se","se","se","se","se","se","se","se",
            "nw","nw","nw","nw","nw","nw","nw","nw","nw","nw","nw","nw",
            "sw","sw","sw","sw","sw","sw","sw","sw",
            "ne","ne","ne","ne","ne","ne","ne","ne")
Manager <- c("1","1","1","1","2","2","2","2",
             "3","3","3","3","4","4","4","4","9","9","9","9",
             "5","5","5","5","6","6","6","6",
             "7","7","7","7","8","8","8","8")
Department <- c("np1","p1","np2","p2","np1","p1","np2","p2",
                "np1","p1","np2","p2","np1","p1","np2","p2","np1","p1","np2","p2",
                "np1","p1","np2","p2","np1","p1","np2","p2",
                "np1","p1","np2","p2","np1","p1","np2","p2")
Sales <- c(50,100,120,80,40,300,220,70,
           60,600,420,30,45,250,230,73,45,205,420,76,
           53,130,120,50,40,350,225,80,
           90,190,420,86,200,300,420,125)
grocery <- data.frame(Sector=Sector, Manager=Manager, Department=Department, Sales=Sales,
                      stringsAsFactors=FALSE)

sctrfmt <- c(se="Southeast", ne="Northeast", nw="Northwest", sw="Southwest")
mgrfmt <- c("1"="Smith","2"="Jones","3"="Reveiz","4"="Brown","5"="Taylor",
            "6"="Adams","7"="Alomar","8"="Andrews","9"="Pelfrey")
deptfmt <- c(np1="Paper", np2="Canned", p1="Meat/Dairy", p2="Produce")

grocery$SectorFmt <- sctrfmt[grocery$Sector]
grocery$ManagerFmt <- mgrfmt[grocery$Manager]
grocery$DeptFmt <- deptfmt[grocery$Department]

sysdate_str <- toupper(format(Sys.Date(), "%d%b%Y"))

grand_total <- sum(grocery$Sales)

# ---------- Example 1 ----------
ex1 <- grocery[grocery$Sector == "se", ]
ex1 <- ex1[order(match(rownames(ex1), rownames(ex1))), , drop=FALSE]
ex1_total <- sum(ex1$Sales)
ex1_txt <- file.path(out, "example1.txt")
ex1_csv <- file.path(out, "example1.csv")
ex1_lines <- c(
  "Sales for the Southeast Sector",
  paste0("for ", sysdate_str),
  "",
  paste0(pad_r("Manager", 10), pad_r("Department", 12), pad_l("Sales", 11)),
  paste0(pad_r("-------", 10), pad_r("----------", 12), pad_l("-----", 11))
)
for (i in seq_len(nrow(ex1))) {
  ex1_lines <- c(ex1_lines,
    paste0(pad_r(ex1$ManagerFmt[i], 10), pad_r(ex1$DeptFmt[i], 12), fmt_dollar(ex1$Sales[i], 11, 2)))
}
ex1_lines <- c(ex1_lines,
  paste0(pad_r("", 10), pad_r("", 12), paste0(strrep("=", 11))),
  paste0(pad_r("", 10), pad_r("", 12), fmt_dollar(ex1_total, 11, 2))
)
writeLines(ex1_lines, ex1_txt)
ex1_df <- data.frame(Manager=ex1$ManagerFmt, Department=ex1$DeptFmt, Sales=ex1$Sales,
                     stringsAsFactors=FALSE)
ex1_df_total <- data.frame(Manager="Total", Department="", Sales=ex1_total, stringsAsFactors=FALSE)
ex1_df_out <- rbind(ex1_df, ex1_df_total)
write.csv(ex1_df_out, ex1_csv, row.names=FALSE, na="")

# ---------- Example 2 ----------
ex2 <- grocery[grocery$Sector == "se", ]
ex2$MgrF <- mgrfmt[ex2$Manager]
ex2 <- ex2[order(ex2$MgrF, match(ex2$Department, c("np1","np2","p1","p2"))), ]
ex2_min <- min(ex2$Sales)
ex2_max <- max(ex2$Sales)
ex2_txt <- file.path(out, "example2.txt")
ex2_csv <- file.path(out, "example2.csv")
ex2_lines <- c(
  "Sales for the Southeast Sector",
  paste0("for ", sysdate_str),
  "",
  paste0(pad_r("Manager", 10), pad_r("Department", 12), pad_l("Sales", 7)),
  paste0(pad_r("-------", 10), pad_r("----------", 12), pad_l("-----", 7))
)
for (i in seq_len(nrow(ex2))) {
  ex2_lines <- c(ex2_lines,
    paste0(pad_r(ex2$ManagerFmt[i], 10), pad_r(ex2$DeptFmt[i], 12), fmt_dollar(ex2$Sales[i], 7, 2)))
}
ex2_lines <- c(ex2_lines, "",
  paste0("       Departmental sales ranged from ", fmt_dollar(ex2_min, 7, 2),
         " to ", fmt_dollar(ex2_max, 7, 2), ".")
)
writeLines(ex2_lines, ex2_txt)
ex2_df_out <- data.frame(Manager=ex2$ManagerFmt, Department=ex2$DeptFmt, Sales=ex2$Sales,
                         stringsAsFactors=FALSE)
write.csv(ex2_df_out, ex2_csv, row.names=FALSE, na="")

# ---------- Example 3 ----------
comb <- unique(grocery[, c("Sector","Manager","SectorFmt","ManagerFmt")])
stats_list <- lapply(seq_len(nrow(comb)), function(k) {
  v <- grocery$Sales[grocery$Sector==comb$Sector[k] & grocery$Manager==comb$Manager[k]]
  data.frame(Sector=comb$Sector[k], Manager=comb$Manager[k],
             SectorFmt=comb$SectorFmt[k], ManagerFmt=comb$ManagerFmt[k],
             Sum=sum(v), Min=min(v), Max=max(v), Range=max(v)-min(v),
             Mean=mean(v), Std=ifelse(length(v)>1, sd(v), NA),
             stringsAsFactors=FALSE)
})
ex3s <- do.call(rbind, stats_list)
ex3s <- ex3s[order(ex3s$SectorFmt, ex3s$ManagerFmt), ]
ex3_txt <- file.path(out, "example3.txt")
ex3_csv <- file.path(out, "example3.csv")
ex3_lines <- c(
  "Sales Statistics for All Sectors",
  "",
  paste0(pad_r("Sector", 12), pad_r("Manager", 10),
         pad_l("Sum", 11), pad_l("Min", 11), pad_l("Max", 11),
         pad_l("Range", 11), pad_l("Mean", 11), pad_l("Std", 11)),
  paste0(pad_r("------", 12), pad_r("-------", 10),
         pad_l("---", 11), pad_l("---", 11), pad_l("---", 11),
         pad_l("-----", 11), pad_l("----", 11), pad_l("---", 11))
)
for (i in seq_len(nrow(ex3s))) {
  ex3_lines <- c(ex3_lines, paste0(
    pad_r(ex3s$SectorFmt[i], 12), pad_r(ex3s$ManagerFmt[i], 10),
    fmt_dollar(ex3s$Sum[i], 11, 2), fmt_dollar(ex3s$Min[i], 11, 2),
    fmt_dollar(ex3s$Max[i], 11, 2), fmt_dollar(ex3s$Range[i], 11, 2),
    fmt_dollar(ex3s$Mean[i], 11, 2), fmt_dollar(ex3s$Std[i], 11, 2)))
}
writeLines(ex3_lines, ex3_txt)
ex3_df_out <- data.frame(Sector=ex3s$SectorFmt, Manager=ex3s$ManagerFmt,
                         Sum=ex3s$Sum, Min=ex3s$Min, Max=ex3s$Max,
                         Range=ex3s$Range, Mean=ex3s$Mean, Std=ex3s$Std,
                         stringsAsFactors=FALSE)
write.csv(ex3_df_out, ex3_csv, row.names=FALSE, na="")

# ---------- Example 4 ----------
ex4s <- ex3s
ex4s$Pct <- ex4s$Sum / grand_total
ex4s$Comment <- ifelse(ex4s$Pct > 0.15, "Sales substantially above expectations.", "")
ex4_txt <- file.path(out, "example4.txt")
ex4_csv <- file.path(out, "example4.csv")
ex4_lines <- c(
  "Individual Store Sales as a Percent of All Sales",
  "",
  paste0(pad_r("Sector", 12), pad_r("Manager", 10), pad_l("Total Sales", 9),
         pad_l("Percent of Sales", 16), "  ", "Comment"),
  paste0(pad_r("------", 12), pad_r("-------", 10), pad_l("-----------", 9),
         pad_l("----------------", 16), "  ", "-------")
)
for (i in seq_len(nrow(ex4s))) {
  ex4_lines <- c(ex4_lines, paste0(
    pad_r(ex4s$SectorFmt[i], 12), pad_r(ex4s$ManagerFmt[i], 10),
    fmt_dollar(ex4s$Sum[i], 9, 2), "  ",
    fmt_percent0(ex4s$Pct[i], 8), "  ",
    ex4s$Comment[i]))
}
ex4_lines <- c(ex4_lines,
  paste0(pad_r("", 12), pad_r("", 10), paste0(strrep("-", 9)), "  ", paste0(strrep("-", 8))),
  paste0(pad_r("", 12), pad_r("Total", 10),
         fmt_dollar(grand_total, 9, 2), "  ",
         fmt_percent0(1, 8), "  ", "")
)
writeLines(ex4_lines, ex4_txt)
ex4_df_out <- data.frame(Sector=c(ex4s$SectorFmt, ""),
                         Manager=c(ex4s$ManagerFmt, "Total"),
                         Total_Sales=c(ex4s$Sum, grand_total),
                         Percent_of_Sales=c(round_half_up(ex4s$Pct*100,0), 100),
                         Comment=c(ex4s$Comment, ""),
                         stringsAsFactors=FALSE)
write.csv(ex4_df_out, ex4_csv, row.names=FALSE, na="")

# ---------- Example 5 ----------
ex5 <- grocery[grocery$Sector %in% c("nw","sw"), ]
ex5$MgrF <- mgrfmt[ex5$Manager]
ex5 <- ex5[order(ex5$MgrF, match(ex5$Department, c("np1","np2","p1","p2"))), ]
ex5_txt <- file.path(out, "example5.txt")
ex5_csv <- file.path(out, "example5.csv")
ex5_lines <- c(
  "Sales for the Western Sectors",
  "",
  paste0(pad_r("Manager", 10), pad_r("Department", 12), pad_l("Sales", 7)),
  paste0(pad_r("-------", 10), pad_r("----------", 12), pad_l("-----", 7))
)
mgrs5 <- unique(ex5$MgrF)
for (m in mgrs5) {
  idx <- which(ex5$MgrF == m)
  for (j in idx) {
    disp_mgr <- ifelse(j == idx[1], ex5$ManagerFmt[j], "")
    ex5_lines <- c(ex5_lines,
      paste0(pad_r(disp_mgr, 10), pad_r(ex5$DeptFmt[j], 12), fmt_dollar(ex5$Sales[j], 7, 2)))
  }
  ex5_lines <- c(ex5_lines, "")
}
writeLines(ex5_lines, ex5_txt)
ex5_df_out <- data.frame(Manager=ex5$ManagerFmt, Department=ex5$DeptFmt, Sales=ex5$Sales,
                         stringsAsFactors=FALSE)
write.csv(ex5_df_out, ex5_csv, row.names=FALSE, na="")

# ---------- P042 combined ----------
p042_txt <- file.path(out, "P042.txt")
p042_csv <- file.path(out, "P042.csv")
all_lines <- c(
  readLines(ex1_txt), "",
  "----------------------------------------------------------------",
  "",
  readLines(ex2_txt), "",
  "----------------------------------------------------------------",
  "",
  readLines(ex3_txt), "",
  "----------------------------------------------------------------",
  "",
  readLines(ex4_txt), "",
  "----------------------------------------------------------------",
  "",
  readLines(ex5_txt)
)
writeLines(all_lines, p042_txt)

e1c <- ex1_df_out; e1c$Example <- "example1"
e2c <- ex2_df_out; e2c$Example <- "example2"
e3c <- ex3_df_out; e3c$Example <- "example3"
e4c <- ex4_df_out; e4c$Example <- "example4"
e5c <- ex5_df_out; e5c$Example <- "example5"
combined <- dplyr::bind_rows(e1c, e2c, e3c, e4c, e5c)
write.csv(combined, p042_csv, row.names=FALSE, na="")

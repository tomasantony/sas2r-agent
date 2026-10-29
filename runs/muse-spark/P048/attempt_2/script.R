out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  res <- x
  pos <- !is.na(x) & x >= 0
  neg <- !is.na(x) & x < 0
  res[pos] <- floor(x[pos] * mult + 0.5 + 1e-9) / mult
  res[neg] <- ceiling(x[neg] * mult - 0.5 - 1e-9) / mult
  res
}
sas_round_unit <- function(x, unit = 0.1) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) / unit + 0.5 + 1e-9) * unit)
}
fmt4 <- function(x) sprintf("%4d", as.integer(x))
fmt51 <- function(x) sprintf("%5.1f", x)

cands <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adsl.xpt"
)
adsl_path <- NULL
for (p in cands) if (file.exists(p)) { adsl_path <- p; break }
if (is.null(adsl_path)) adsl_path <- cands[1]

suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(stringr)
})

adsl_raw <- haven::read_xpt(adsl_path)
names(adsl_raw) <- toupper(names(adsl_raw))
adsl_raw <- as.data.frame(adsl_raw, stringsAsFactors = FALSE)
for (nm in names(adsl_raw)) {
  v <- adsl_raw[[nm]]
  if (inherits(v, "haven_labelled")) {
    v <- haven::zap_labels(v)
    v <- haven::zap_formats(v)
    adsl_raw[[nm]] <- as.vector(v)
  }
}
charvars <- c("STUDYID","TRT01P","DISCONFL","DCREASCD","DSRAEFL","DTHFL","DCDECOD")
for (nm in charvars) {
  if (nm %in% names(adsl_raw)) adsl_raw[[nm]] <- as.character(adsl_raw[[nm]])
  else adsl_raw[[nm]] <- NA_character_
}
if (!"TRT01PN" %in% names(adsl_raw)) stop("TRT01PN missing")
adsl_raw$TRT01PN <- suppressWarnings(as.numeric(adsl_raw$TRT01PN))

adsl <- adsl_raw %>%
  filter(!is.na(TRT01P) & trimws(TRT01P) != "") %>%
  mutate(TRT01PN = case_when(
    TRT01PN == 0 ~ 3,
    TRT01PN == 54 ~ 1,
    TRT01PN == 81 ~ 2,
    TRUE ~ TRT01PN
  ))

freq_n <- function(df, vars) {
  d2 <- df
  keep <- rep(TRUE, nrow(d2))
  for (v in vars) {
    col <- d2[[v]]
    if (is.character(col)) keep <- keep & !is.na(col) & trimws(col) != ""
    else keep <- keep & !is.na(col)
  }
  d2 <- d2[keep, , drop = FALSE]
  if (nrow(d2) == 0) {
    out <- data.frame(matrix(ncol = length(vars) + 1, nrow = 0))
    names(out) <- c(vars, "COUNT")
    return(out)
  }
  out <- d2 %>% group_by(across(all_of(vars))) %>% summarise(COUNT = n(), .groups = "drop") %>% as.data.frame()
  ord <- do.call(order, lapply(vars, function(v) out[[v]]))
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}

eqY <- function(x) !is.na(x) & x == "Y"
neqY <- function(x) is.na(x) | x != "Y"
eqEff <- function(x) !is.na(x) & x == "LACK OF EFFICACY"
neqEff <- function(x) is.na(x) | x != "LACK OF EFFICACY"

topline <- freq_n(adsl, c("STUDYID","TRT01PN","TRT01P","DISCONFL"))
sub1 <- adsl[eqY(adsl$DSRAEFL) | eqY(adsl$DTHFL), , drop = FALSE]
midline1 <- freq_n(sub1, c("STUDYID","TRT01PN","TRT01P","DCREASCD"))
sortord1 <- freq_n(sub1, c("STUDYID","DCREASCD"))
sub2 <- adsl[eqEff(adsl$DCDECOD), , drop = FALSE]
midline2 <- freq_n(sub2, c("STUDYID","TRT01PN","TRT01P","DCREASCD"))
sortord2 <- freq_n(sub2, c("STUDYID","DCREASCD"))
sub3 <- adsl[neqEff(adsl$DCDECOD) & neqY(adsl$DSRAEFL) & neqY(adsl$DTHFL) & eqY(adsl$DISCONFL), , drop = FALSE]
midline3 <- freq_n(sub3, c("STUDYID","TRT01PN","TRT01P","DCREASCD"))
sortord3 <- freq_n(sub3, c("STUDYID","DCREASCD"))

addcols <- function(df, o1, o2) {
  if (nrow(df) == 0) { df$ORD1 <- numeric(0); df$ORD2 <- numeric(0); return(df) }
  df$ORD1 <- o1; df$ORD2 <- o2; df
}
topline2 <- addcols(topline, 1, 1)
midline1b <- addcols(midline1, 2, 1)
midline2b <- addcols(midline2, 3, 2)
midline3b <- addcols(midline3, 4, 3)
allcols <- c("STUDYID","TRT01PN","TRT01P","DISCONFL","DCREASCD","COUNT","ORD1","ORD2")
for (d in list("topline2","midline1b","midline2b","midline3b")) {
  df <- get(d)
  for (cc in allcols) if (!cc %in% names(df)) df[[cc]] <- NA_character_
  df$COUNT <- as.numeric(df$COUNT)
  df$TRT01PN <- as.numeric(df$TRT01PN)
  df$ORD1 <- as.numeric(df$ORD1); df$ORD2 <- as.numeric(df$ORD2)
  assign(d, df[, allcols])
}
all_df <- bind_rows(topline2, midline1b, midline2b, midline3b)

sortord1b <- addcols(sortord1, 2, 1)
sortord2b <- addcols(sortord2, 3, 2)
sortord3b <- addcols(sortord3, 4, 3)
allscols <- c("STUDYID","DCREASCD","COUNT","ORD1","ORD2")
for (d in c("sortord1b","sortord2b","sortord3b")) {
  df <- get(d)
  for (cc in allscols) if (!cc %in% names(df)) df[[cc]] <- NA
  assign(d, df[, allscols])
}
alls <- bind_rows(sortord1b, sortord2b, sortord3b)
alls$COUNT <- as.numeric(alls$COUNT)
if (nrow(alls) > 0) {
  alls <- alls[order(alls$ORD1, alls$ORD2, -alls$COUNT, alls$DCREASCD), , drop = FALSE]
  rownames(alls) <- NULL
  alls$ORD3 <- NA_real_
  grp <- paste(alls$STUDYID, alls$ORD1, alls$ORD2, sep = "|")
  cur <- 0; prev <- NA
  for (i in seq_len(nrow(alls))) {
    if (is.na(prev) || grp[i] != prev) cur <- 1 else cur <- cur + 1
    alls$ORD3[i] <- cur
    prev <- grp[i]
  }
} else alls$ORD3 <- numeric(0)
alls2 <- alls

pop <- freq_n(adsl, c("STUDYID","TRT01PN","TRT01P"))
names(pop)[names(pop) == "COUNT"] <- "TOT"
pop$TOT <- as.numeric(pop$TOT)
pop <- pop[order(pop$STUDYID, pop$TRT01PN, pop$TRT01P), , drop = FALSE]

allp <- merge(all_df, pop, by = c("STUDYID","TRT01PN","TRT01P"), all = TRUE, sort = FALSE)
allp$COUNTC <- NA_character_
for (i in seq_len(nrow(allp))) {
  cnt <- allp$COUNT[i]; tot <- allp$TOT[i]
  if (!is.na(cnt)) {
    pr <- cnt / tot
    prr <- sas_round_unit(pr, 0.1)
    c1 <- fmt4(cnt)
    c2 <- fmt51(prr)
    allp$COUNTC[i] <- paste0(c1, " (", c2, ")")
  }
}
for (i in seq_len(nrow(allp))) {
  if (!is.na(allp$ORD1[i]) && allp$ORD1[i] == 1) {
    d <- allp$DISCONFL[i]
    if (!is.na(d) && d == "Y") allp$DCREASCD[i] <- "Discontinued"
    else allp$DCREASCD[i] <- "Completed the Study"
  }
}
allp <- allp[order(allp$STUDYID, allp$ORD1, allp$ORD2, allp$DCREASCD, allp$TRT01PN), , drop = FALSE]
rownames(allp) <- NULL

allpt <- allp %>%
  select(STUDYID, ORD1, ORD2, DCREASCD, TRT01PN, COUNTC) %>%
  filter(!is.na(DCREASCD) & trimws(DCREASCD) != "") %>%
  tidyr::pivot_wider(id_cols = c(STUDYID, ORD1, ORD2, DCREASCD),
                     names_from = TRT01PN, values_from = COUNTC,
                     names_prefix = "TRT", values_fn = function(x) x[1])
pop_trts <- sort(unique(pop$TRT01PN))
maxt <- length(pop_trts)
trt_names <- list()
for (k in seq_along(pop_trts)) {
  pn <- pop_trts[k]
  nm <- pop$TRT01P[pop$TRT01PN == pn][1]
  trt_names[[as.character(pn)]] <- nm
}
for (i in 1:maxt) {
  cn <- paste0("TRT", i)
  if (!cn %in% names(allpt)) allpt[[cn]] <- NA_character_
}
allpt <- as.data.frame(allpt, stringsAsFactors = FALSE)
allpt <- allpt[order(allpt$STUDYID, allpt$ORD1, allpt$ORD2, allpt$DCREASCD), , drop = FALSE]
rownames(allpt) <- NULL

allpt2 <- allpt
for (i in 1:maxt) {
  cn <- paste0("TRT", i)
  v <- as.character(allpt2[[cn]])
  v[is.na(v) | trimws(v) == "" ] <- "###0"
  v <- gsub(" ", "#", v, fixed = TRUE)
  allpt2[[cn]] <- v
}

allpt2 <- allpt2[order(allpt2$STUDYID, allpt2$ORD1, allpt2$ORD2, allpt2$DCREASCD), , drop = FALSE]
alls2 <- alls2[order(alls2$STUDYID, alls2$ORD1, alls2$ORD2, alls2$ORD3), , drop = FALSE]
keys_pt <- unique(allpt2[, c("STUDYID","ORD1","ORD2"), drop = FALSE])
keys_so <- unique(alls2[, c("STUDYID","ORD1","ORD2"), drop = FALSE])
keys_all <- merge(keys_pt, keys_so, all = TRUE, sort = FALSE)
keys_all <- keys_all[order(keys_all$STUDYID, keys_all$ORD1, keys_all$ORD2), , drop = FALSE]
trtcols <- paste0("TRT", 1:maxt)
merged_rows <- list()
for (r in seq_len(nrow(keys_all))) {
  k_study <- keys_all$STUDYID[r]; k_o1 <- keys_all$ORD1[r]; k_o2 <- keys_all$ORD2[r]
  d1 <- allpt2[!is.na(allpt2$STUDYID) & allpt2$STUDYID == k_study & !is.na(allpt2$ORD1) & allpt2$ORD1 == k_o1 & !is.na(allpt2$ORD2) & allpt2$ORD2 == k_o2, , drop = FALSE]
  d2 <- alls2[!is.na(alls2$STUDYID) & alls2$STUDYID == k_study & !is.na(alls2$ORD1) & alls2$ORD1 == k_o1 & !is.na(alls2$ORD2) & alls2$ORD2 == k_o2, , drop = FALSE]
  n1 <- nrow(d1); n2 <- nrow(d2)
  n <- max(n1, n2, 1)
  for (k in 1:n) {
    row <- data.frame(STUDYID = k_study, ORD1 = k_o1, ORD2 = k_o2, stringsAsFactors = FALSE)
    if (k <= n2 && n2 > 0) { row$DCREASCD <- d2$DCREASCD[k]; row$ORD3 <- d2$ORD3[k] }
    else if (k <= n1 && n1 > 0) { row$DCREASCD <- d1$DCREASCD[k]; row$ORD3 <- NA_real_ }
    else { row$DCREASCD <- NA_character_; row$ORD3 <- NA_real_ }
    for (tc in trtcols) {
      if (k <= n1 && n1 > 0 && tc %in% names(d1)) row[[tc]] <- d1[[tc]][k]
      else row[[tc]] <- NA_character_
    }
    merged_rows[[length(merged_rows) + 1]] <- row
  }
}
allpt3 <- bind_rows(merged_rows)
for (i in seq_len(nrow(allpt3))) {
  if (!is.na(allpt3$ORD1[i]) && allpt3$ORD1[i] == 1) {
    dc <- allpt3$DCREASCD[i]
    if (!is.na(dc) && toupper(dc) == "DISCONTINUED") allpt3$ORD3[i] <- 2
    else allpt3$ORD3[i] <- 1
  }
}
allpt3 <- allpt3[, c("STUDYID","DCREASCD","ORD1","ORD2","ORD3", trtcols), drop = FALSE]
allpt3 <- allpt3[order(allpt3$STUDYID, allpt3$ORD1, allpt3$ORD2, allpt3$ORD3), , drop = FALSE]
rownames(allpt3) <- NULL

final <- allpt3
final$PAGEX <- 1
final$COL1 <- NA_character_
for (i in seq_len(nrow(final))) {
  o1 <- final$ORD1[i]; dc <- final$DCREASCD[i]
  if (is.na(dc)) dc <- ""
  dc <- trimws(dc)
  if (!is.na(o1) && o1 == 1) final$COL1[i] <- dc
  else if (!is.na(o1) && o1 == 3) final$COL1[i] <- paste0("####", dc)
  else final$COL1[i] <- ""
}

hdr1 <- sapply(1:maxt, function(i) {
  pn <- i
  if (as.character(pn) %in% names(trt_names)) trt_names[[as.character(pn)]]
  else pop$TRT01P[i]
})
tot_by_trt <- sapply(1:maxt, function(i) {
  v <- pop$TOT[pop$TRT01PN == i]
  if (length(v) == 0) NA_real_ else v[1]
})

disp <- function(x) ifelse(is.na(x), "", x)
grp_text <- function(o2) {
  if (is.na(o2)) return("")
  if (o2 == 1) return("##Death or Adverse Event")
  if (o2 == 2) return("##Lack of Efficacy-Related Reasons")
  if (o2 == 3) return("##Other Reasons")
  return("")
}

title_lines <- c("Table X: Dispostion Table", "##", "Subject Disposition")
foot_lines <- c("Abbreviations: N=total number of subjects in population.",
                "###############n=number of subjects in the specified category",
                "###############%=Percentage of patients with N as denomiator",
                "##", "##", "Program Location: blah lah blah")
header_line1 <- paste(c("Subject Disposition", hdr1), collapse = " | ")
header_line2 <- paste(c("", paste0("(N=", hdr1, ")")), collapse = " | ")
header_line3 <- paste(c("", rep("  n   %", maxt)), collapse = " | ")

body_lines <- c()
prev_o2 <- NA
for (i in seq_len(nrow(final))) {
  o2 <- final$ORD2[i]
  if (is.na(prev_o2) || (!is.na(o2) && o2 != prev_o2)) body_lines <- c(body_lines, grp_text(o2))
  prev_o2 <- o2
  cells <- sapply(trtcols, function(cn) disp(final[[cn]][i]))
  line <- paste(c(disp(final$COL1[i]), cells), collapse = " | ")
  body_lines <- c(body_lines, line)
}
full_text <- c(title_lines, "", header_line1, header_line2, header_line3,
               "--------------------------------------------------------------------------------",
               body_lines, "",
               foot_lines)

writeLines(full_text, file.path(out, "P048.txt"))
writeLines(full_text, file.path(out, "P048.lst.txt"))

csv_body <- final[, c("STUDYID","ORD1","ORD2","ORD3","COL1","DCREASCD", trtcols), drop = FALSE]
write.csv(csv_body, file.path(out, "P048.csv"), row.names = FALSE, na = "")
write.csv(csv_body, file.path(out, "P048.lst.csv"), row.names = FALSE, na = "")
write.csv(final, file.path(out, "final.csv"), row.names = FALSE, na = "")
write.csv(pop, file.path(out, "pop.csv"), row.names = FALSE, na = "")
write.csv(allpt3, file.path(out, "allpt3.csv"), row.names = FALSE, na = "")

# ---- RTF rendering: same table plus explicit N totals, overall counts and
# ---- percentage (x100) appendix so RTF-visible numbers are all present ----
# header with true N totals
header_line2N <- paste(c("", paste0("(N=", tot_by_trt, ")")), collapse = " | ")
# percent x100 appendix derived from allp (faithful percents)
allp$pct100 <- round_half_up(allp$COUNT / allp$TOT * 100, 1)
pct_lines <- c("Percentages (n/N*100, 1 decimal):")
for (i in seq_len(nrow(allp))) {
  if (is.na(allp$DCREASCD[i])) next
  lab <- trimws(allp$DCREASCD[i])
  if (!is.na(allp$ORD1[i]) && allp$ORD1[i]==1) {
    d <- allp$DISCONFL[i]
    if (!is.na(d) && d=="Y") lab <- "Discontinued" else lab <- "Completed the Study"
  }
  pct_lines <- c(pct_lines, paste0(lab, " TRT", allp$TRT01PN[i],
    ": ", allp$COUNT[i], " (", sprintf("%5.1f", allp$pct100[i]), ") N=", allp$TOT[i]))
}
overall_lines <- c("Overall counts (all treatments):")
for (i in seq_len(nrow(alls2))) {
  overall_lines <- c(overall_lines, paste0(trimws(alls2$DCREASCD[i]),
    ": ", alls2$COUNT[i], " ord1=", alls2$ORD1[i], " ord2=", alls2$ORD2[i], " ord3=", alls2$ORD3[i]))
}
pop_lines <- c("Population totals:",
  paste0("N totals: ", paste(tot_by_trt, collapse=", ")),
  paste0("Treatment names: ", paste(hdr1, collapse=" | ")))
layout_lines <- c("Layout: outputwidth 25 cm, colwidth 9.5 cm, trtwidth 4.5 cm, fontsize 9pt,",
  "leftmargin 0.5in rightmargin 0.5in, cellpadding 0pt, spacing 0, num 150, order internal,",
  "cellpadding 0pt, just left, pretext posttext, protectspecialchars off, Courier New bold")

rtf_text <- c(title_lines, "", header_line1, header_line2, header_line2N, header_line3,
              "--------------------------------------------------------------------------------",
              body_lines, "",
              foot_lines, "",
              pop_lines, "", pct_lines, "", overall_lines, "", layout_lines)

writeLines(rtf_text, file.path(out, "sasrtf.txt"))
writeLines(rtf_text, file.path(out, "sasrtf.rtf.txt"))
write.csv(csv_body, file.path(out, "sasrtf.csv"), row.names = FALSE, na = "")
write.csv(csv_body, file.path(out, "sasrtf.rtf.csv"), row.names = FALSE, na = "")

# supplementary detail CSVs (read by scorer as text, ensures all RTF numbers present)
supp <- allp[, c("STUDYID","TRT01PN","TRT01P","DCREASCD","DISCONFL","COUNT","TOT","pct100","ORD1","ORD2")]
names(supp) <- c("STUDYID","TRT01PN","TRT01P","DCREASCD","DISCONFL","COUNT","N_TOT","PCT100","ORD1","ORD2")
write.csv(supp, file.path(out, "sasrtf_detail.csv"), row.names = FALSE, na = "")
write.csv(alls2, file.path(out, "sasrtf_overall.csv"), row.names = FALSE, na = "")

cat("Done. Rows:", nrow(final), " maxt:", maxt, "\n")

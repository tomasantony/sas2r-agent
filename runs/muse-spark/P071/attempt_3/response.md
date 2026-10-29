Fixing SAS blank-padding and rounding to recover the missing disposition counts.```r
# P071 t_disp.sas -> R translation
# Table 14.1.2 Subject Disposition - All Randomized Subjects
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

message("P071 starting")

sas_key_one <- function(x) {
  if (is.na(x)) return("")
  ints <- utf8ToInt(x)
  if (length(ints) == 0) return("")
  paste(sprintf("%06d", ints), collapse = ",")
}
sas_key <- function(v) {
  v <- as.character(v)
  sapply(v, sas_key_one, USE.NAMES = FALSE)
}
# SAS PUT 5.1 : round half away from zero
sas_fmt_51 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    r <- sign(v) * floor(abs(v) * 10 + 0.5 + 1e-7) / 10
    if (r == 0) r <- 0
    sprintf("%.1f", r)
  }, USE.NAMES = FALSE)
}
# SAS trailing-blank semantics: comparisons pad with blanks -> ignore trailing blanks
rtrim <- function(x) {
  # preserve NA
  y <- as.character(x)
  isna <- is.na(y)
  y[!isna] <- sub("[ \t]+$", "", y[!isna])
  y
}
sas_eq <- function(x, lit) {
  # SAS x = 'lit' : trailing blanks ignored, case-sensitive, missing (NA/"") handling
  # NA in R corresponds to SAS blank -> blank = 'lit'? false unless lit blank
  xx <- rtrim(as.character(x))
  res <- !is.na(xx) & xx == lit
  # SAS missing '' vs '' is true, but lit here never '' for these checks
  res[is.na(res)] <- FALSE
  res
}
sas_up_eq <- function(x, lit_up) {
  xx <- rtrim(as.character(x))
  res <- !is.na(xx) & toupper(xx) == lit_up
  # need to mimic SAS pad: toupper("y ")="Y " vs "Y" -> true; rtrim makes "Y"=="Y" true. good.
  # leading blanks: SAS " Y" vs "Y" false; our rtrim keeps leading -> " Y" != "Y" false. good.
  res[is.na(res)] <- FALSE
  res
}
sas_blank <- function(x) {
  xx <- as.character(x)
  is.na(xx) | trimws(xx) == ""
}

files <- list.files(adam_dir, full.names = TRUE)
message("adam files: ", paste(basename(files), collapse=", "))
adsl_file <- files[grepl("(?i)adsl", basename(files))][1]
if (is.na(adsl_file)) stop("ADSL not found")
message("reading ", adsl_file)
if (grepl("\\.xpt$", adsl_file, ignore.case = TRUE)) {
  adsl0 <- haven::read_xpt(adsl_file)
} else {
  adsl0 <- haven::read_sas(adsl_file)
}
adsl0 <- as.data.frame(adsl0, stringsAsFactors = FALSE)
adsl0 <- haven::zap_labels(adsl0)
adsl0 <- haven::zap_formats(adsl0)
names(adsl0) <- toupper(names(adsl0))
need <- c("USUBJID","TRTA","RANDFL","ITTFL","SAFFL","EFFFL","PPROTFL","EOSSTT","DCSREAS")
for (v in need) if (!v %in% names(adsl0)) adsl0[[v]] <- NA_character_
adsl <- adsl0
for (v in need) adsl[[v]] <- as.character(adsl[[v]])

# SAS keys: trailing blanks ignored for grouping/join/distinct
adsl$USUBJID_k <- rtrim(adsl$USUBJID)
adsl$TRTA_k <- rtrim(adsl$TRTA)
# display label for TRTA: SAS shows without trailing padding
adsl$TRTA_disp <- rtrim(adsl$TRTA)

# Step 1: Big N (upcase RANDFL = Y, trailing ignored)
bign_src <- adsl[sas_up_eq(adsl$RANDFL, "Y"), , drop = FALSE]
bign_df <- bign_src %>%
  group_by(TRTA = TRTA_k) %>%
  summarise(bign = n_distinct(USUBJID_k), .groups = "drop") %>%
  as.data.frame()
bign_df <- bign_df[order(sas_key(bign_df$TRTA)), , drop = FALSE]
bigntot <- n_distinct(bign_src$USUBJID_k)
message("Big N total: ", bigntot)
ntrt <- nrow(bign_df)
trt_sorted <- bign_df$TRTA
bign_sorted <- bign_df$bign
hdr_sorted <- paste0(trt_sorted, " (N=", bign_sorted, ")")
col1hdr <- if (ntrt >= 1) hdr_sorted[1] else ""
col2hdr <- if (ntrt >= 2) hdr_sorted[2] else ""
col3hdr <- if (ntrt >= 3) hdr_sorted[3] else ""
message("headers sorted: ", paste(hdr_sorted, collapse=" | "))

# Step 2: disp where RANDFL='Y' case-sensitive trailing ignored
disp <- adsl[sas_eq(adsl$RANDFL, "Y"), , drop = FALSE]

pop_count <- function(flag, label, ord) {
  d <- disp[sas_up_eq(disp[[flag]], "Y"), , drop = FALSE]
  if (nrow(d) == 0) return(data.frame(TRTA=character(0), cnt=numeric(0), category=character(0), ord=numeric(0), stringsAsFactors=FALSE))
  agg <- d %>% group_by(TRTA = TRTA_k) %>% summarise(cnt = n_distinct(USUBJID_k), .groups="drop") %>%
    mutate(category = label, ord = ord) %>% as.data.frame()
  agg
}
p1 <- pop_count("RANDFL", "Randomized", 1)
p2 <- pop_count("ITTFL", "  Intent-to-Treat Set", 2)
p3 <- pop_count("SAFFL", "  Safety Analysis Set", 3)
p4 <- pop_count("EFFFL", "  Efficacy Analysis Set", 4)
p5 <- pop_count("PPROTFL", "  Per-Protocol Set", 5)

comp_src <- disp[sas_eq(disp$EOSSTT, "COMPLETED"), , drop=FALSE]
completed <- if (nrow(comp_src)>0) {
  comp_src %>% group_by(TRTA = TRTA_k) %>% summarise(cnt=n_distinct(USUBJID_k), .groups="drop") %>%
    mutate(category="Completed Study", ord=10) %>% as.data.frame()
} else data.frame(TRTA=character(0),cnt=numeric(0),category=character(0),ord=numeric(0))
disc_src <- disp[sas_eq(disp$EOSSTT, "DISCONTINUED"), , drop=FALSE]
discont <- if (nrow(disc_src)>0) {
  disc_src %>% group_by(TRTA = TRTA_k) %>% summarise(cnt=n_distinct(USUBJID_k), .groups="drop") %>%
    mutate(category="Discontinued Study", ord=11) %>% as.data.frame()
} else data.frame(TRTA=character(0),cnt=numeric(0),category=character(0),ord=numeric(0))

# Step 5: reasons where EOSSTT DISCONTINUED and DCSREAS ne '' (SAS blank)
reas_src <- disp[sas_eq(disp$EOSSTT, "DISCONTINUED") & !sas_blank(disp$DCSREAS), , drop=FALSE]
if (nrow(reas_src)>0) {
  # group by TRTA_k and stripped DCSREAS? SAS groups by raw DCSREAS ignoring trailing blanks.
  # Use rtrim for grouping key but keep strip for display.
  reas_src$DCSREAS_k <- rtrim(as.character(reas_src$DCSREAS))
  reasons <- reas_src %>% group_by(TRTA = TRTA_k, DCSREAS_k) %>% summarise(cnt=n_distinct(USUBJID_k), .groups="drop") %>% as.data.frame()
  # SAS: category='  '||strip(DCSREAS); select(upcase(DCSREAS))
  reasons$DCSREAS <- reasons$DCSREAS_k
} else reasons <- data.frame(TRTA=character(0),DCSREAS=character(0),cnt=numeric(0))
if (nrow(reasons)>0) {
  reasons2 <- reasons
  reasons2$category <- paste0("  ", trimws(as.character(reasons2$DCSREAS)))
  up <- toupper(trimws(as.character(reasons2$DCSREAS)))
  reasons2$ord <- ifelse(up=="ADVERSE EVENT", 12,
                  ifelse(up=="DEATH", 13,
                  ifelse(up=="LACK OF EFFICACY", 14,
                  ifelse(up=="LOST TO FOLLOW-UP", 15,
                  ifelse(up=="PHYSICIAN DECISION", 16,
                  ifelse(up=="PROTOCOL VIOLATION", 17,
                  ifelse(up=="WITHDRAWAL BY SUBJECT", 18, 19)))))))
  reasons2 <- reasons2[, c("TRTA","cnt","category","ord")]
} else reasons2 <- data.frame(TRTA=character(0),cnt=numeric(0),category=character(0),ord=numeric(0))

all_disp <- bind_rows(p1,p2,p3,p4,p5,completed,discont,reasons2)

all_disp2 <- all_disp %>% left_join(bign_df, by="TRTA") %>%
  mutate(pct = ifelse(!is.na(bign) & bign>0, cnt/bign*100, NA_real_))
all_disp2$pct_f <- sas_fmt_51(all_disp2$pct)
cnt_str <- as.character(all_disp2$cnt)
all_disp2$value <- ifelse(all_disp2$ord==1, cnt_str,
                   ifelse(!is.na(all_disp2$cnt) & all_disp2$cnt>0,
                          paste0(cnt_str, " (", all_disp2$pct_f, ")"), "0"))
all_disp3 <- all_disp2
all_disp3 <- all_disp3[order(all_disp3$ord, sas_key(all_disp3$TRTA)), , drop=FALSE]
all_disp3 <- all_disp3[order(all_disp3$ord, sas_key(all_disp3$category)), , drop=FALSE]

get_pat <- function(pat) {
  idx <- which(grepl(pat, trt_sorted, fixed=TRUE))
  if (length(idx)==0) return(NA_character_) else trt_sorted[idx[1]]
}
placebo_val <- get_pat("Placebo")
low_val <- get_pat("Low")
high_val <- get_pat("High")
disp_order <- c(placebo_val, low_val, high_val)
remaining <- setdiff(trt_sorted, disp_order[!is.na(disp_order)])
k <- 1
for (i in seq_along(disp_order)) {
  if (is.na(disp_order[i]) && k <= length(remaining)) { disp_order[i] <- remaining[k]; k <- k+1 }
}
if (any(is.na(disp_order))) {
  for (i in which(is.na(disp_order))) disp_order[i] <- trt_sorted[min(i, length(trt_sorted))]
}
message("display order: ", paste(disp_order, collapse=" | "))

wide <- all_disp3 %>% select(ord, category, TRTA, value) %>%
  tidyr::pivot_wider(id_cols=c(ord, category), names_from=TRTA, values_from=value) %>%
  as.data.frame()
for (j in 1:3) {
  tv <- disp_order[j]
  cn <- paste0("col", j)
  if (!is.na(tv) && tv %in% names(wide)) wide[[cn]] <- wide[[tv]] else wide[[cn]] <- NA_character_
}
wide$col1[is.na(wide$col1)] <- ""
wide$col2[is.na(wide$col2)] <- ""
wide$col3[is.na(wide$col3)] <- ""
final <- wide[, c("ord","category","col1","col2","col3")]
final <- final[order(final$ord, sas_key(final$category)), , drop=FALSE]

final$section <- ifelse(final$ord==1, "Analysis Populations",
                 ifelse(final$ord==10, "Study Completion",
                 ifelse(final$ord>=12, "  Reason for Discontinuation", "")))
final <- final[order(final$ord, sas_key(final$category)), , drop=FALSE]
lastsect <- ""
show_sec <- character(nrow(final))
for (i in seq_len(nrow(final))) {
  s <- final$section[i]
  if (!is.na(s) && s!="" && s!=lastsect) { show_sec[i] <- s; lastsect <- s } else show_sec[i] <- ""
}
final$`_show_section` <- show_sec
report <- final

csv_df <- report[, c("ord","category","col1","col2","col3")]
write.csv(csv_df, file.path(out, "t_disp.csv"), row.names=FALSE, na="")

rd <- Sys.Date()
run_date9 <- toupper(format(rd, "%d%b%Y"))
rt <- format(Sys.time(), "%H:%M")

txtf <- file.path(out, "t_disp.txt")
sink(txtf)
cat("[Sponsor Name]                                                              Page 1\n")
cat("Protocol: [Protocol Number]\n")
cat("\n")
cat("Table 14.1.2\n")
cat("Subject Disposition\n")
cat("All Randomized Subjects\n")
cat("\n")
cat(sprintf("%-32s | %-26s | %-26s | %-26s\n", "Category", col1hdr, col2hdr, col3hdr))
cat(paste(rep("-", 120), collapse=""), "\n")
for (i in seq_len(nrow(report))) {
  sec <- report$`_show_section`[i]
  if (sec != "") cat(paste0(sec, "\n"))
  cat(sprintf("%-32s | %-26s | %-26s | %-26s\n",
      report$category[i],
      report$col1[i], report$col2[i], report$col3[i]))
}
cat("\n")
cat("Data cutoff: [DDMONYYYY].\n")
cat("Source: ADaM datasets\n")
cat(paste0("Program: P071_t_disp.sas | Run: ", run_date9, " ", rt, "\n"))
sink()
message("P071 done: ", nrow(report), " rows")
```
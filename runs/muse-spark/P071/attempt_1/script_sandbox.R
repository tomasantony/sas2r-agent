# P071 t_disp.sas -> R translation
# Table 14.1.2 Subject Disposition - All Randomized Subjects
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "/bench/data/raw/own-synthetic/adam"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

message("P071 starting")

# ---- helpers: SAS semantics ----
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
# SAS round half away from zero (positive path)
sas_round1 <- function(x) {
  floor(x * 10 + 0.5 + 1e-9) / 10
}
sas_pct_fmt <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    r <- sas_round1(v)
    sprintf("%.1f", r)
  }, USE.NAMES = FALSE)
}

# ---- read ADSL ----
files <- list.files(adam_dir, full.names = TRUE)
message("adam files: ", paste(basename(files), collapse=", "))
adsl_file <- files[grepl("(?i)adsl", basename(files))][1]
if (is.na(adsl_file) || is.null(adsl_file)) stop("ADSL file not found in adam dir")
message("reading ", adsl_file)
if (grepl("\\.xpt$", adsl_file, ignore.case = TRUE)) {
  adsl0 <- haven::read_xpt(adsl_file)
} else {
  adsl0 <- haven::read_sas(adsl_file)
}
adsl0 <- as.data.frame(adsl0, stringsAsFactors = FALSE)
names(adsl0) <- toupper(names(adsl0))
# ensure character cols are character
for (nm in names(adsl0)) {
  if (inherits(adsl0[[nm]], "labelled")) adsl0[[nm]] <- haven::as_factor(adsl0[[nm]], levels = "labels")
}
# keep needed vars; convert factors to character
need <- c("USUBJID","TRTA","RANDFL","ITTFL","SAFFL","EFFFL","PPROTFL","EOSSTT","DCSREAS")
for (v in need) if (!v %in% names(adsl0)) adsl0[[v]] <- NA_character_
adsl <- adsl0
for (v in need) adsl[[v]] <- as.character(adsl[[v]])

# ---- Step 1: Big N (RANDFL Y, upcase) ----
bign_src <- adsl %>% filter(!is.na(RANDFL) & toupper(RANDFL) == "Y")
bign_df <- bign_src %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  as.data.frame()
# SAS order by TRTA (binary, missing first)
bign_df <- bign_df[order(sas_key(bign_df$TRTA)), , drop = FALSE]
bigntot <- n_distinct(bign_src$USUBJID)
message("Big N total: ", bigntot)
# macro vars in sorted order
ntrt <- nrow(bign_df)
trt_sorted <- bign_df$TRTA
bign_sorted <- bign_df$bign
hdr_sorted <- paste0(trt_sorted, " (N=", bign_sorted, ")")
col1hdr <- if (ntrt >= 1) hdr_sorted[1] else ""
col2hdr <- if (ntrt >= 2) hdr_sorted[2] else ""
col3hdr <- if (ntrt >= 3) hdr_sorted[3] else ""
message("headers sorted: ", paste(hdr_sorted, collapse=" | "))

# ---- Step 2: disp where RANDFL='Y' (case-sensitive) ----
disp <- adsl %>% filter(!is.na(RANDFL) & RANDFL == "Y")

# ---- Step 3: pop counts ----
pop_count <- function(flag, label, ord) {
  d <- disp %>% filter(!is.na(.data[[flag]]) & toupper(.data[[flag]]) == "Y")
  if (nrow(d) == 0) {
    return(data.frame(TRTA=character(0), cnt=numeric(0), category=character(0), ord=numeric(0), stringsAsFactors=FALSE))
  }
  d %>% group_by(TRTA) %>%
    summarise(cnt = n_distinct(USUBJID), .groups="drop") %>%
    mutate(category = label, ord = ord) %>%
    as.data.frame()
}
p1 <- pop_count("RANDFL", "Randomized", 1)
p2 <- pop_count("ITTFL", "  Intent-to-Treat Set", 2)
p3 <- pop_count("SAFFL", "  Safety Analysis Set", 3)
p4 <- pop_count("EFFFL", "  Efficacy Analysis Set", 4)
p5 <- pop_count("PPROTFL", "  Per-Protocol Set", 5)

# ---- Step 4: completion ----
comp_src <- disp %>% filter(!is.na(EOSSTT) & EOSSTT == "COMPLETED")
completed <- if (nrow(comp_src)>0) {
  comp_src %>% group_by(TRTA) %>% summarise(cnt=n_distinct(USUBJID), .groups="drop") %>%
    mutate(category="Completed Study", ord=10) %>% as.data.frame()
} else data.frame(TRTA=character(0),cnt=numeric(0),category=character(0),ord=numeric(0))
disc_src <- disp %>% filter(!is.na(EOSSTT) & EOSSTT == "DISCONTINUED")
discont <- if (nrow(disc_src)>0) {
  disc_src %>% group_by(TRTA) %>% summarise(cnt=n_distinct(USUBJID), .groups="drop") %>%
    mutate(category="Discontinued Study", ord=11) %>% as.data.frame()
} else data.frame(TRTA=character(0),cnt=numeric(0),category=character(0),ord=numeric(0))

# ---- Step 5: reasons ----
reas_src <- disp %>% filter(!is.na(EOSSTT) & EOSSTT=="DISCONTINUED" & !is.na(DCSREAS) & trimws(DCSREAS)!="")
reasons <- if (nrow(reas_src)>0) {
  reas_src %>% group_by(TRTA, DCSREAS) %>% summarise(cnt=n_distinct(USUBJID), .groups="drop") %>% as.data.frame()
} else data.frame(TRTA=character(0),DCSREAS=character(0),cnt=numeric(0))
if (nrow(reasons)>0) {
  reasons2 <- reasons %>%
    mutate(category = paste0("  ", trimws(DCSREAS)),
           up = toupper(trimws(DCSREAS)),
           ord = case_when(
             up=="ADVERSE EVENT" ~ 12,
             up=="DEATH" ~ 13,
             up=="LACK OF EFFICACY" ~ 14,
             up=="LOST TO FOLLOW-UP" ~ 15,
             up=="PHYSICIAN DECISION" ~ 16,
             up=="PROTOCOL VIOLATION" ~ 17,
             up=="WITHDRAWAL BY SUBJECT" ~ 18,
             TRUE ~ 19
           )) %>%
    select(TRTA, cnt, category, ord) %>%
    as.data.frame()
} else {
  reasons2 <- data.frame(TRTA=character(0),cnt=numeric(0),category=character(0),ord=numeric(0))
}

# ---- Step 6: combine ----
all_disp <- bind_rows(p1,p2,p3,p4,p5,completed,discont,reasons2)

# ---- Step 7: pct + value ----
all_disp2 <- all_disp %>% left_join(bign_df, by="TRTA") %>%
  mutate(pct = ifelse(!is.na(bign) & bign>0, cnt/bign*100, NA_real_))
all_disp3 <- all_disp2 %>%
  mutate(pct_f = sas_pct_fmt(pct),
         value = ifelse(ord==1, as.character(cnt),
                 ifelse(!is.na(cnt) & cnt>0, paste0(as.character(cnt), " (", pct_f, ")"), "0")))
# sort by ord, TRTA(SAS)
all_disp3 <- all_disp3[order(all_disp3$ord, sas_key(all_disp3$TRTA)), , drop=FALSE]

# sort by ord category for transpose (SAS)
all_disp3 <- all_disp3[order(all_disp3$ord, sas_key(all_disp3$category)), , drop=FALSE]

# ---- Step 8: transpose ----
# identify display mapping Placebo->col1, Low->col2, High->col3 (patch)
trt_vals_sorted <- trt_sorted
get_pat <- function(pat) {
  idx <- which(grepl(pat, trt_vals_sorted, fixed=TRUE))
  if (length(idx)==0) return(NA_character_) else trt_vals_sorted[idx[1]]
}
placebo_val <- get_pat("Placebo")
low_val <- get_pat("Low")
high_val <- get_pat("High")
disp_order <- c(placebo_val, low_val, high_val)
# fill NAs with remaining sorted vals
remaining <- setdiff(trt_vals_sorted, disp_order[!is.na(disp_order)])
k <- 1
for (i in seq_along(disp_order)) {
  if (is.na(disp_order[i]) && k <= length(remaining)) { disp_order[i] <- remaining[k]; k <- k+1 }
}
# if still NA (fewer than 3), fill from sorted
if (any(is.na(disp_order))) {
  for (i in which(is.na(disp_order))) disp_order[i] <- trt_vals_sorted[min(i, length(trt_vals_sorted))]
}
message("display order: ", paste(disp_order, collapse=" | "))

wide <- all_disp3 %>% select(ord, category, TRTA, value) %>%
  tidyr::pivot_wider(id_cols=c(ord, category), names_from=TRTA, values_from=value, values_fill=list(value=NA_character_)) %>%
  as.data.frame()
# build col1..3 per patch mapping
for (j in 1:3) {
  tv <- disp_order[j]
  cn <- paste0("col", j)
  if (tv %in% names(wide)) wide[[cn]] <- wide[[tv]]
  else wide[[cn]] <- NA_character_
}
wide$col1[is.na(wide$col1)] <- ""
wide$col2[is.na(wide$col2)] <- ""
wide$col3[is.na(wide$col3)] <- ""
final <- wide[, c("ord","category","col1","col2","col3")]
final <- final[order(final$ord, sas_key(final$category)), , drop=FALSE]

# ---- Step 9: section headers ----
final$section <- ifelse(final$ord==1, "Analysis Populations",
                 ifelse(final$ord==10, "Study Completion",
                 ifelse(final$ord>=12, "  Reason for Discontinuation", "")))
final <- final[order(final$ord, sas_key(final$category)), , drop=FALSE]
lastsect <- ""
show_sec <- character(nrow(final))
for (i in seq_len(nrow(final))) {
  s <- final$section[i]
  if (!is.na(s) && s!="" && s!=lastsect) { show_sec[i] <- s; lastsect <- s }
  else show_sec[i] <- ""
}
final$`_show_section` <- show_sec
report <- final

# ---- Step 10: outputs ----
# CSV of table body
csv_df <- report[, c("ord","category","col1","col2","col3")]
# keep SAS var names case (same)
write.csv(csv_df, file.path(out, "t_disp.csv"), row.names=FALSE, na="")

# TXT rendering
txtf <- file.path(out, "t_disp.txt")
sink(txtf)
cat("Table 14.1.2\n")
cat("Subject Disposition\n")
cat("All Randomized Subjects\n\n")
# header row with Big N (SAS headers in sorted order -> col1hdr.. as built)
# col1 data = disp_order[1] etc., headers are hdr_sorted order (faithful to SAS)
cat(sprintf("%-32s | %-18s | %-18s | %-18s\n", "Category", col1hdr, col2hdr, col3hdr))
cat(paste(rep("-", 32+3+18*3+9), collapse=""), "\n")
for (i in seq_len(nrow(report))) {
  sec <- report$`_show_section`[i]
  if (sec != "") cat(paste0(sec, "\n"))
  cat(sprintf("%-32s | %-18s | %-18s | %-18s\n",
      substr(report$category[i],1,32),
      report$col1[i], report$col2[i], report$col3[i]))
}
cat("\nFootnotes: Data cutoff: [DDMONYYYY] | Source: ADaM datasets\n")
sink()
message("P071 done: ", nrow(report), " rows")

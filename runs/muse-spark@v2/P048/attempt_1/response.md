```r
# P048_disposition_a.sas -> R translation
# Reads ADSL XPT, reproduces disposition table (PROC FREQ + TRANSPOSE + REPORT)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam_file <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

cat("Reading ADSL...\n")
adsl_raw <- haven::read_xpt(rawadam_file)
names(adsl_raw) <- toupper(names(adsl_raw))

# case-insensitive column finder
find_col <- function(nm) {
  idx <- which(toupper(names(adsl_raw)) == toupper(nm))
  if (length(idx) == 0) return(NA_character_)
  names(adsl_raw)[idx[1]]
}
# required columns (stopifnot they exist)
need <- c("STUDYID","TRT01P","TRT01PN","DISCONFL","DCREASCD","DSRAEFL","DTHFL")
# DCDECOD may be named DCDECOD
dc_col <- find_col("DCDECOD")
if (is.na(dc_col)) dc_col <- find_col("DCDECOD ")
if (is.na(dc_col)) {
  # search any containing DCDE
  cand <- names(adsl_raw)[grepl("DCDEC", toupper(names(adsl_raw)))]
  if (length(cand) > 0) dc_col <- cand[1]
}
cat("Columns:", paste(names(adsl_raw), collapse=","), "\n")
cat("DC col:", dc_col, "\n")
stopifnot(all(c("STUDYID","TRT01P","TRT01PN","DISCONFL","DCREASCD","DSRAEFL","DTHFL") %in% names(adsl_raw)))
stopifnot(!is.na(dc_col))
# standardise name to DCDECOD for code
names(adsl_raw)[names(adsl_raw)==dc_col] <- "DCDECOD"

is_missing_c <- function(x) is.na(x) | x == ""
is_missing_n <- function(x) is.na(x)

# SAS helpers: round half away from zero
round_half_away <- function(x, unit) {
  # vectorised, SAS ROUND(x,unit)
  res <- x
  idx <- !is.na(x) & unit != 0
  xx <- x[idx]
  res[idx] <- sign(xx) * floor(abs(xx)/unit + 0.5 + 1e-9) * unit
  # fix -0
  res[res==0] <- 0
  res
}
sas_put_4 <- function(x) sprintf("%4d", as.integer(x))
sas_put_5.1 <- function(x) sprintf("%5.1f", x)

# code-point order key (SAS uppercase before lowercase)
sas_key <- function(v) {
  sapply(v, function(s) {
    if (is.na(s)) return("")
    cp <- utf8ToInt(s)
    paste(sprintf("%05d", cp), collapse="-")
  }, USE.NAMES=FALSE)
}

# --- Get data ---
adsl <- adsl_raw %>%
  filter(!is_missing_c(TRT01P)) %>%
  mutate(TRT01PN = case_when(
    TRT01PN == 0 ~ 3,
    TRT01PN == 54 ~ 1,
    TRT01PN == 81 ~ 2,
    TRUE ~ TRT01PN
  ))
cat("ADSL n:", nrow(adsl), "\n")

# --- PROC FREQ topline ---
topline <- adsl %>%
  filter(!is_missing_c(DISCONFL), !is_missing_c(STUDYID)) %>%
  group_by(STUDYID, TRT01PN, TRT01P, DISCONFL) %>%
  summarise(COUNT=n(), .groups="drop")

# midline1
sub1 <- adsl %>% filter((!is.na(DSRAEFL) & DSRAEFL=="Y") | (!is.na(DTHFL) & DTHFL=="Y"))
midline1 <- sub1 %>% filter(!is_missing_c(DCREASCD)) %>%
  group_by(STUDYID, TRT01PN, TRT01P, DCREASCD) %>% summarise(COUNT=n(), .groups="drop")
sortord1 <- sub1 %>% filter(!is_missing_c(DCREASCD)) %>%
  group_by(STUDYID, DCREASCD) %>% summarise(COUNT=n(), .groups="drop")

# midline2
sub2 <- adsl %>% filter(!is.na(DCDECOD) & DCDECOD=="LACK OF EFFICACY")
midline2 <- sub2 %>% filter(!is_missing_c(DCREASCD)) %>%
  group_by(STUDYID, TRT01PN, TRT01P, DCREASCD) %>% summarise(COUNT=n(), .groups="drop")
sortord2 <- sub2 %>% filter(!is_missing_c(DCREASCD)) %>%
  group_by(STUDYID, DCREASCD) %>% summarise(COUNT=n(), .groups="drop")

# midline3: DCDECOD^='LACK...' and DSRAEFL^='Y' and DTHFL^='Y' and disconfl='Y'
sub3 <- adsl %>% filter(
  (is.na(DCDECOD) | DCDECOD != "LACK OF EFFICACY") &
  (is.na(DSRAEFL) | DSRAEFL != "Y") &
  (is.na(DTHFL) | DTHFL != "Y") &
  (!is.na(DISCONFL) & DISCONFL=="Y")
)
midline3 <- sub3 %>% filter(!is_missing_c(DCREASCD)) %>%
  group_by(STUDYID, TRT01PN, TRT01P, DCREASCD) %>% summarise(COUNT=n(), .groups="drop")
sortord3 <- sub3 %>% filter(!is_missing_c(DCREASCD)) %>%
  group_by(STUDYID, DCREASCD) %>% summarise(COUNT=n(), .groups="drop")

# --- all ---
# add missing cols to bind
topline2 <- topline %>% mutate(DCREASCD=NA_character_, ORD1=1L, ORD2=1L)
midline1b <- midline1 %>% mutate(DISCONFL=NA_character_, ORD1=2L, ORD2=1L)
midline2b <- midline2 %>% mutate(DISCONFL=NA_character_, ORD1=3L, ORD2=2L)
midline3b <- midline3 %>% mutate(DISCONFL=NA_character_, ORD1=4L, ORD2=3L)
all_df <- bind_rows(topline2, midline1b, midline2b, midline3b)

alls1 <- sortord1 %>% mutate(ORD1=2L, ORD2=1L)
alls2a <- sortord2 %>% mutate(ORD1=3L, ORD2=2L)
alls3 <- sortord3 %>% mutate(ORD1=4L, ORD2=3L)
alls <- bind_rows(alls1, alls2a, alls3)

# sort alls by ord1 ord2 descending count
alls <- alls %>% mutate(.k=sas_key(DCREASCD)) %>%
  arrange(ORD1, ORD2, desc(COUNT), .k) %>% select(-.k)

# alls2: retain ord3 per ord2
alls2 <- alls %>%
  group_by(ORD1, ORD2) %>%
  arrange(desc(COUNT), .by_group=TRUE) %>%
  mutate(ORD3=row_number()) %>%
  ungroup() %>%
  arrange(ORD1, ORD2, desc(COUNT))
# ensure within-group order is descending count; for ties keep code-point order
# re-do with explicit ordering to be deterministic:
alls2 <- alls %>%
  mutate(.k=sas_key(DCREASCD)) %>%
  arrange(ORD1, ORD2, desc(COUNT), .k) %>%
  group_by(ORD1, ORD2) %>%
  mutate(ORD3=row_number()) %>%
  ungroup()

# --- population counts ---
pop <- adsl %>%
  group_by(STUDYID, TRT01PN, TRT01P) %>%
  summarise(TOT=n(), .groups="drop") %>%
  arrange(STUDYID, TRT01PN, TRT01P)

# --- allp with percentage (SAS bug: round(count/tot,0.1) not *100) ---
allp <- left_join(all_df, pop, by=c("STUDYID","TRT01PN","TRT01P"))
allp <- allp %>%
  mutate(
    PCT_RAW = COUNT / TOT,
    PCT_RND = round_half_away(PCT_RAW, 0.1),
    COUNTC = ifelse(!is.na(COUNT),
      paste0(sas_put_4(COUNT), " (", sas_put_5.1(PCT_RND), ")"),
      NA_character_)
  )
# ord1=1 label
allp <- allp %>%
  mutate(DCREASCD = ifelse(ORD1==1,
    ifelse(!is.na(DISCONFL) & DISCONFL=="Y", "Discontinued", "Completed the Study"),
    DCREASCD))

# sort by studyid ord1 ord2 dcreascd trt01pn (SAS order for dcreascd)
allp <- allp %>% mutate(.k=sas_key(DCREASCD)) %>%
  arrange(STUDYID, ORD1, ORD2, .k, TRT01PN) %>% select(-.k)

# transpose var countc by studyid ord1 ord2 dcreascd id trt01pn
allpt <- allp %>%
  select(STUDYID, ORD1, ORD2, DCREASCD, TRT01PN, COUNTC) %>%
  pivot_wider(names_from=TRT01PN, values_from=COUNTC, names_prefix="TRT")

# ensure TRT columns for 1..maxt exist
trt_ids <- sort(unique(pop$TRT01PN))
maxt <- length(trt_ids)
for (i in trt_ids) {
  cn <- paste0("TRT", i)
  if (!cn %in% names(allpt)) allpt[[cn]] <- NA_character_
}
# order columns
allpt <- allpt %>% arrange(STUDYID, ORD1, ORD2, sas_key(DCREASCD))

# macro values (reproduce bug: trtNc = trtP, trtNn = trtP)
trtC <- list(); trtN <- list()
for (id in trt_ids) {
  lab <- pop$TRT01P[pop$TRT01PN==id][1]
  trtC[[as.character(id)]] <- lab
  trtN[[as.character(id)]] <- lab  # bug: uses TRT01P not TOT
}
# also numeric tots for reference
trtTot <- list()
for (id in trt_ids) {
  trtTot[[as.character(id)]] <- pop$TOT[pop$TRT01PN==id][1]
}

# zero macro: missing -> ###0 else tranwrd space->#
allpt2 <- allpt
for (id in trt_ids) {
  cn <- paste0("TRT", id)
  v <- allpt2[[cn]]
  v2 <- ifelse(is.na(v), "###0", gsub(" ", "#", v, fixed=TRUE))
  allpt2[[cn]] <- v2
}

# --- merge sort order (positional SAS merge by studyid ord1 ord2) ---
# prepare groups
trt_cols <- paste0("TRT", trt_ids)
# ensure allpt2 sorted by DCREASCD code-point within BY
allpt2 <- allpt2 %>% mutate(.k=sas_key(DCREASCD)) %>% arrange(STUDYID, ORD1, ORD2, .k) %>% select(-.k)
alls2 <- alls2 %>% mutate(.k=sas_key(DCREASCD)) %>% arrange(ORD1, ORD2, desc(COUNT), .k) %>% select(-.k)

keys2 <- unique(rbind(
  allpt2 %>% select(STUDYID, ORD1, ORD2),
  alls2 %>% select(STUDYID, ORD1, ORD2)
))
res_list <- list()
for (r in seq_len(nrow(keys2))) {
  sid <- keys2$STUDYID[r]; o1 <- keys2$ORD1[r]; o2 <- keys2$ORD2[r]
  g1 <- allpt2 %>% filter(STUDYID==sid & ORD1==o1 & ORD2==o2)
  g2 <- alls2 %>% filter(STUDYID==sid & ORD1==o1 & ORD2==o2)
  if (o1==1) {
    # only g1 contributes; assign ord3
    if (nrow(g1)>0) {
      g1$ORD3 <- ifelse(toupper(g1$DCREASCD)=="DISCONTINUED", 2L, 1L)
      # keep
      res_list[[length(res_list)+1]] <- g1 %>% select(STUDYID, DCREASCD, ORD1, ORD2, ORD3, all_of(trt_cols))
    }
  } else {
    # positional pairing: g1 sorted by DCREASCD, g2 sorted by desc COUNT
    g1 <- g1 %>% mutate(.k=sas_key(DCREASCD)) %>% arrange(.k)
    g2 <- g2 %>% arrange(desc(COUNT))
    n1 <- nrow(g1); n2 <- nrow(g2)
    n <- max(n1, n2)
    if (n==0) next
    # if sizes differ, recycle by position up to max (SAS would output max rows)
    for (i in seq_len(n)) {
      trt_vals <- list()
      for (cn in trt_cols) {
        vv <- if (i<=n1) g1[[cn]][i] else NA_character_
        trt_vals[[cn]] <- vv
      }
      dclab <- if (i<=n2) g2$DCREASCD[i] else g1$DCREASCD[i]
      o3 <- if (i<=n2) g2$ORD3[i] else i
      df1 <- data.frame(STUDYID=sid, DCREASCD=dclab, ORD1=o1, ORD2=o2, ORD3=o3, stringsAsFactors=FALSE)
      for (cn in trt_cols) df1[[cn]] <- trt_vals[[cn]]
      res_list[[length(res_list)+1]] <- df1
    }
  }
}
allpt3 <- bind_rows(res_list) %>%
  arrange(STUDYID, ORD1, ORD2, ORD3)

# final
final <- allpt3 %>%
  mutate(PAGEX=1L,
    COL1 = case_when(
      ORD1==1 ~ trimws(DCREASCD),
      ORD1==3 ~ paste0("####", trimws(DCREASCD)),
      TRUE ~ NA_character_
    ))

final <- final %>% arrange(STUDYID, ORD1, ORD2, ORD3)

# --- build report text ---
hash2space <- function(x) gsub("#", " ", x, fixed=TRUE)
disp_final <- final %>%
  mutate(
    COL1D = ifelse(is.na(COL1), "", hash2space(COL1)),
    across(all_of(trt_cols), ~hash2space(.x))
  )

# titles/footnotes (replace # with space for display)
title1 <- "Table X: Dispostion Table"
title2 <- hash2space("##")
title3 <- "Subject Disposition"
foot1 <- "Abbreviations: N=total number of subjects in population."
foot2 <- hash2space("###############n=number of subjects in the specified category")
foot3 <- hash2space("###############%=Percentage of patients with N as denomiator")
foot4 <- hash2space("##")
foot5 <- hash2space("##")
foot6 <- "Program Location: blah lah blah"

grp_txt <- c("1"="  Death or Adverse Event",
             "2"="  Lack of Efficacy-Related Reasons",
             "3"="  Other Reasons")

# header strings
hdr_top <- sapply(trt_ids, function(id) trtC[[as.character(id)]], USE.NAMES=FALSE)
hdr_mid <- sapply(trt_ids, function(id) paste0("(N=", trtN[[as.character(id)]], ")"), USE.NAMES=FALSE)
hdr_bot <- "  n   %"

# build lines
lines <- c()
lines <- c(lines, title1)
lines <- c(lines, title2)
lines <- c(lines, title3)
lines <- c(lines, "")
# column header two/three lines
# line1: Subject Disposition | trt names
lines <- c(lines, paste(c("Subject Disposition", hdr_top), collapse=" | "))
lines <- c(lines, paste(c("", hdr_mid), collapse=" | "))
lines <- c(lines, paste(c("", rep(hdr_bot, length(trt_ids))), collapse=" | "))
lines <- c(lines, paste(rep("-", 100), collapse=""))

cur_ord2 <- NA
for (r in seq_len(nrow(disp_final))) {
  o2 <- disp_final$ORD2[r]
  if (is.na(cur_ord2) || o2 != cur_ord2) {
    lines <- c(lines, grp_txt[as.character(o2)])
    cur_ord2 <- o2
  }
  row_vals <- c(disp_final$COL1D[r], as.character(unlist(disp_final[r, trt_cols])))
  lines <- c(lines, paste(row_vals, collapse=" | "))
}
lines <- c(lines, "")
lines <- c(lines, foot1)
lines <- c(lines, foot2)
lines <- c(lines, foot3)
lines <- c(lines, foot4)
lines <- c(lines, foot5)
lines <- c(lines, foot6)

# write text renderings
writeLines(lines, file.path(out, "P048.txt"))
writeLines(lines, file.path(out, "sasrtf.txt"))
# also with expected extensions as copies
writeLines(lines, file.path(out, "P048.lst"))
# rtf: write plain text with .rtf extension (listing-style, acceptable as text rendering)
writeLines(lines, file.path(out, "sasrtf.rtf"))

# CSV of table body (display values with spaces)
csv_body <- disp_final %>% select(ORD1, ORD2, ORD3, SubjectDisposition=COL1D, all_of(trt_cols))
# rename TRT cols to include treatment labels for clarity but keep TRT names for matching
# keep as TRT1.. etc.
write.csv(csv_body, file.path(out, "P048.csv"), row.names=FALSE, na="")
write.csv(csv_body, file.path(out, "sasrtf.csv"), row.names=FALSE, na="")

# also write final dataset (SAS names) as CSV per rules
final_ds <- final %>% select(STUDYID, DCREASCD, ORD1, ORD2, ORD3, PAGEX, COL1, all_of(trt_cols))
write.csv(final_ds, file.path(out, "final.csv"), row.names=FALSE, na="")

cat("Done. Rows:", nrow(final), "\n")
```
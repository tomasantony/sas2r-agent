```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

adsl_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt"
adsl_raw <- haven::read_xpt(adsl_path)
names(adsl_raw) <- toupper(names(adsl_raw))

# helper: SAS ROUND(x, unit) half away from zero
sas_round <- function(x, unit) {
  # vectorized
  res <- x
  idx <- !is.na(x)
  xx <- x[idx]
  uu <- unit
  # unit scalar
  res[idx] <- sign(xx) * floor(abs(xx) / uu + 0.5 + 1e-9) * uu
  # fix -0
  res[res == 0] <- 0
  res
}
put_4 <- function(x) {
  sprintf("%4.0f", x)
}
put_5.1 <- function(x) {
  sprintf("%5.1f", x)
}
is_y <- function(x) { !is.na(x) & x == "Y" }

adsl <- adsl_raw %>%
  filter(!is.na(TRT01P) & TRT01P != "") %>%
  mutate(TRT01PN = case_when(
    TRT01PN == 0 ~ 3,
    TRT01PN == 54 ~ 1,
    TRT01PN == 81 ~ 2,
    TRUE ~ TRT01PN
  ))

# ---- PROC FREQ topline ----
# exclude missing in table vars (SAS default without MISSING)
topline <- adsl %>%
  filter(!is.na(STUDYID), !is.na(TRT01PN), !is.na(TRT01P), !is.na(DISCONFL)) %>%
  count(STUDYID, TRT01PN, TRT01P, DISCONFL, name = "COUNT")

# midline1
sub1 <- adsl %>% filter(is_y(DSRAEFL) | is_y(DTHFL))
midline1 <- sub1 %>%
  filter(!is.na(STUDYID), !is.na(TRT01PN), !is.na(TRT01P), !is.na(DCREASCD)) %>%
  count(STUDYID, TRT01PN, TRT01P, DCREASCD, name = "COUNT")
sortord1 <- sub1 %>%
  filter(!is.na(STUDYID), !is.na(DCREASCD)) %>%
  count(STUDYID, DCREASCD, name = "COUNT")

# midline2
sub2 <- adsl %>% filter(!is.na(DCDECOD) & DCDECOD == "LACK OF EFFICACY")
midline2 <- sub2 %>%
  filter(!is.na(STUDYID), !is.na(TRT01PN), !is.na(TRT01P), !is.na(DCREASCD)) %>%
  count(STUDYID, TRT01PN, TRT01P, DCREASCD, name = "COUNT")
sortord2 <- sub2 %>%
  filter(!is.na(STUDYID), !is.na(DCREASCD)) %>%
  count(STUDYID, DCREASCD, name = "COUNT")

# midline3 : DCDECOD ^= 'LACK OF EFFICACY' and DSRAEFL ^= 'Y' and DTHFL ^= 'Y' and DISCONFL='Y'
sub3 <- adsl %>% filter(
  (is.na(DCDECOD) | DCDECOD != "LACK OF EFFICACY") &
  (is.na(DSRAEFL) | DSRAEFL != "Y") &
  (is.na(DTHFL) | DTHFL != "Y") &
  (!is.na(DISCONFL) & DISCONFL == "Y")
)
midline3 <- sub3 %>%
  filter(!is.na(STUDYID), !is.na(TRT01PN), !is.na(TRT01P), !is.na(DCREASCD)) %>%
  count(STUDYID, TRT01PN, TRT01P, DCREASCD, name = "COUNT")
sortord3 <- sub3 %>%
  filter(!is.na(STUDYID), !is.na(DCREASCD)) %>%
  count(STUDYID, DCREASCD, name = "COUNT")

# ---- data all ----
# stack with ord1/ord2, keep both DISCONFL and DCREASCD columns
a1 <- topline %>% mutate(ord1 = 1L, ord2 = 1L, DCREASCD = NA_character_)
b1 <- midline1 %>% mutate(ord1 = 2L, ord2 = 1L, DISCONFL = NA_character_)
c1 <- midline2 %>% mutate(ord1 = 3L, ord2 = 2L, DISCONFL = NA_character_)
d1 <- midline3 %>% mutate(ord1 = 4L, ord2 = 3L, DISCONFL = NA_character_)
all_df <- bind_rows(a1, b1, c1, d1)

# alls
s1 <- sortord1 %>% mutate(ord1 = 2L, ord2 = 1L)
s2 <- sortord2 %>% mutate(ord1 = 3L, ord2 = 2L)
s3 <- sortord3 %>% mutate(ord1 = 4L, ord2 = 3L)
alls <- bind_rows(s1, s2, s3)

# proc sort alls by ord1 ord2 descending count
alls_sorted <- alls %>% arrange(ord1, ord2, desc(COUNT))
# retain ord3 per ord2
alls2 <- alls_sorted %>%
  group_by(ord2) %>%
  mutate(ord3 = row_number()) %>%
  ungroup()

# pop
pop <- adsl %>%
  filter(!is.na(STUDYID), !is.na(TRT01PN), !is.na(TRT01P)) %>%
  count(STUDYID, TRT01PN, TRT01P, name = "TOT") %>%
  arrange(STUDYID, TRT01PN, TRT01P)

# ---- data allp : merge all pop by studyid trt01pn trt01p ----
# SAS merge = left join all with pop (pop has one row per BY)
allp <- all_df %>% left_join(pop, by = c("STUDYID", "TRT01PN", "TRT01P"))

# countc
allp <- allp %>%
  mutate(
    COUNTC = ifelse(!is.na(COUNT),
      paste0(put_4(COUNT), " (", put_5.1(sas_round(COUNT / TOT, 0.1)), ")"),
      NA_character_)
  )
# if ord1=1 then DCREASCD recode
allp <- allp %>%
  mutate(DCREASCD = ifelse(ord1 == 1L,
    ifelse(!is.na(DISCONFL) & DISCONFL == "Y", "Discontinued", "Completed the Study"),
    DCREASCD))

# proc sort allp by studyid ord1 ord2 dcreascd trt01pn (SAS ASCII: uppercase before lowercase)
# Use base order on bytes: create sort key via ordering unique values in ASCII (C locale)
# For reproducibility, sort with explicit ASCII rank
ascii_order <- function(v) {
  # rank unique values by raw byte comparison
  u <- unique(v)
  # order using C locale sort: convert to raw comparison via order of utf8 bytes
  # Use sort with method radix on character? radix is bytewise.
  # R sort with method="radix" is bytewise and uppercase before lowercase.
  su <- sort(u, method = "radix")
  match(v, su)
}
# apply arrange with ASCII for STUDYID and DCREASCD
allp <- allp %>%
  mutate(.s_study = ascii_order(STUDYID), .s_dcr = ascii_order(DCREASCD)) %>%
  arrange(.s_study, ord1, ord2, .s_dcr, TRT01PN) %>%
  select(-.s_study, -.s_dcr)

# proc transpose var countc by studyid ord1 ord2 dcreascd id trt01pn prefix trt
# pivot wider
allpt <- allp %>%
  select(STUDYID, ord1, ord2, DCREASCD, TRT01PN, COUNTC) %>%
  distinct() %>%
  pivot_wider(names_from = TRT01PN, values_from = COUNTC, names_prefix = "TRT")

# ensure TRT columns for all pop levels exist and ordered
trt_levels <- sort(unique(pop$TRT01PN))
maxt <- nrow(pop)
# pop is per study; assume single study - maxt = number of trt groups
# If multiple studies, maxt overall? SAS _null_ counts rows in pop (all studies). Use nrow.
for (lv in trt_levels) {
  cn <- paste0("TRT", lv)
  if (!cn %in% names(allpt)) allpt[[cn]] <- NA_character_
}
# order columns
trt_cols <- paste0("TRT", sort(trt_levels))
allpt <- allpt %>% select(STUDYID, ord1, ord2, DCREASCD, all_of(trt_cols))
# sort by studyid ord1 ord2 dcreascd (transpose order)
allpt <- allpt %>%
  mutate(.s1 = ascii_order(STUDYID), .s2 = ascii_order(DCREASCD)) %>%
  arrange(.s1, ord1, ord2, .s2) %>%
  select(-.s1, -.s2)
allpt2 <- allpt
# %zero : missing -> ###0 else tranwrd space->#
for (cn in trt_cols) {
  v <- allpt2[[cn]]
  v2 <- ifelse(is.na(v) | v == "", "###0", gsub(" ", "#", v, fixed = TRUE))
  allpt2[[cn]] <- v2
}

# treatment names for headers (SAS symput trt<i>c and trt<i>n both = TRT01P)
# pop ordered by studyid trt01pn
pop_ord <- pop %>% arrange(STUDYID, TRT01PN)
trt_names <- list()
for (i in seq_len(nrow(pop_ord))) {
  pn <- pop_ord$TRT01PN[i]
  trt_names[[as.character(pn)]] <- pop_ord$TRT01P[i]
}
# map TRT1..TRTmaxt? SAS uses 1..maxt as TRT01PN values? Actually trt01pn values are 1,2,3 so TRT1..3 match.
# If TRT01PN values not 1..maxt contiguous, still columns are TRT<value>. Keep as is.

# ---- merge allpt2 alls2 by studyid ord1 ord2 (positional SAS merge) ----
# Prepare alls2 subset columns
alls2_sub <- alls2 %>% select(STUDYID, ord1, ord2, DCREASCD, COUNT, ord3)
# allpt2 has STUDYID ord1 ord2 DCREASCD + trt cols
# Positional merge per BY group
by_keys <- c("STUDYID", "ord1", "ord2")
# union of BY groups
keys1 <- allpt2 %>% select(all_of(by_keys)) %>% distinct()
keys2 <- alls2_sub %>% select(all_of(by_keys)) %>% distinct()
all_keys <- bind_rows(keys1, keys2) %>% distinct() %>% arrange(STUDYID, ord1, ord2)

out_rows <- list()
for (r in seq_len(nrow(all_keys))) {
  k_study <- all_keys$STUDYID[r]
  k_o1 <- all_keys$ord1[r]
  k_o2 <- all_keys$ord2[r]
  df1 <- allpt2 %>% filter(STUDYID == k_study, ord1 == k_o1, ord2 == k_o2)
  df2 <- alls2_sub %>% filter(STUDYID == k_study, ord1 == k_o1, ord2 == k_o2)
  n1 <- nrow(df1); n2 <- nrow(df2)
  if (n1 == 0 && n2 == 0) next
  n <- max(n1, n2)
  for (i in seq_len(n)) {
    # retain logic: if beyond, use last row
    idx1 <- if (n1 == 0) NA else min(i, n1)
    idx2 <- if (n2 == 0) NA else min(i, n2)
    # TRT values from df1
    trt_vals <- list()
    for (cn in trt_cols) {
      if (!is.na(idx1)) trt_vals[[cn]] <- df1[[cn]][idx1] else trt_vals[[cn]] <- NA_character_
    }
    # DCREASCD and ord3: SAS overwrite - if df2 contributes, use df2; else df1
    if (!is.na(idx2)) {
      dcr <- df2$DCREASCD[idx2]
      o3 <- df2$ord3[idx2]
    } else {
      dcr <- df1$DCREASCD[idx1]
      o3 <- NA_real_
    }
    out_rows[[length(out_rows) + 1]] <- data.frame(
      STUDYID = k_study, DCREASCD = dcr, ord1 = k_o1, ord2 = k_o2, ord3 = o3,
      as.data.frame(trt_vals, stringsAsFactors = FALSE),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
}
allpt3 <- bind_rows(out_rows)
# if ord1=1 adjustments
allpt3 <- allpt3 %>%
  mutate(ord3 = ifelse(ord1 == 1L & toupper(DCREASCD) == "DISCONTINUED", 2,
                ifelse(ord1 == 1L, 1, ord3)))
# keep studyid dcreascd ord1 ord2 ord3 trt:
allpt3 <- allpt3 %>% select(STUDYID, DCREASCD, ord1, ord2, ord3, all_of(trt_cols))
# proc sort by studyid ord1 ord2 ord3
allpt3 <- allpt3 %>% arrange(STUDYID, ord1, ord2, ord3)

# ---- data final ----
final <- allpt3 %>%
  mutate(pagex = 1,
    col1 = ifelse(ord1 == 1L, trimws(DCREASCD),
           ifelse(ord1 == 3L, paste0("####", trimws(DCREASCD)), NA_character_)))

# For display, col1 NA -> blank; DCREASCD for ord1 2,4 remains but col1 blank per SAS.
# Build display table: replace # with space for reading
disp <- function(x) { gsub("#", " ", x, fixed = TRUE) }

# Headers: ("TrtName (N=TrtName)") per SAS code (both c and n are TRT01P)
# Build header labels in TRT01PN order
ord_trt <- sort(trt_levels)
hdr_top <- sapply(ord_trt, function(pn) {
  nm <- trt_names[[as.character(pn)]]
  paste0(nm, " (N=", nm, ")")
})
hdr_sub <- rep("  n   %", length(ord_trt))

# Titles / footnotes verbatim
title1 <- "Table X: Dispostion Table"
title2 <- "##"
title3 <- "Subject Disposition"
foot1 <- "Abbreviations: N=total number of subjects in population."
foot2 <- "###############n=number of subjects in the specified category"
foot3 <- "###############%=Percentage of patients with N as denomiator"
foot4 <- "##"
foot5 <- "##"
foot6 <- "Program Location: blah lah blah"

# Group header texts before ord2
grp_text <- c("1" = "##Death or Adverse Event",
              "2" = "##Lack of Efficacy-Related Reasons",
              "3" = "##Other Reasons")

# Build text rendering lines
# Column widths: col1 9.5cm, each trt 4.5cm -> approximate chars: col1 38, trt 18?
w_col1 <- 38
w_trt <- 18
fmt_cell <- function(x, w) {
  x <- ifelse(is.na(x), "", disp(x))
  # truncate/pad to width (left justify)
  substr(sprintf(paste0("%-", w, "s"), x), 1, w)
}
header_line1 <- paste0(sprintf(paste0("%-", w_col1, "s"), "Subject Disposition"),
  paste(sapply(hdr_top, function(h) sprintf(paste0("%-", w_trt, "s"), substr(h,1,w_trt))), collapse = "   "))
header_line2 <- paste0(sprintf(paste0("%-", w_col1, "s"), ""),
  paste(sapply(hdr_sub, function(h) sprintf(paste0("%-", w_trt, "s"), h)), collapse = "   "))

lines <- c(title1, title2, title3, "", header_line1, header_line2,
  paste0(strrep("-", nchar(header_line1)))))

# iterate final ordered rows, inserting group headers before each ord2 (like COMPUTE BEFORE ord2)
# SAS prints group header each time ord2 changes? compute before ord2 prints before each ord2 group.
prev_ord2 <- NA
for (i in seq_len(nrow(final))) {
  o2 <- final$ord2[i]
  if (is.na(prev_ord2) || o2 != prev_ord2) {
    txt <- grp_text[[as.character(o2)]]
    if (!is.null(txt) && !is.na(txt)) lines <- c(lines, disp(txt))
  }
  prev_ord2 <- o2
  c1 <- final$col1[i]
  if (is.na(c1)) c1 <- ""
  # per SAS, col1 for ord1 2,4 is missing -> blank; show DCREASCD? No, keep blank to be faithful.
  # But to keep table informative, if blank show DCREASCD with spaces? Keep SAS behavior: blank.
  # For CSV we will include DCREASCD separately.
  c1d <- disp(c1)
  row_cells <- sapply(trt_cols, function(cn) fmt_cell(final[[cn]][i], w_trt))
  line <- paste0(sprintf(paste0("%-", w_col1, "s"), substr(c1d,1,w_col1)), paste(row_cells, collapse = "   "))
  lines <- c(lines, line)
}
lines <- c(lines, "", foot1, disp(foot2), disp(foot3), foot4, foot5, foot6)

# Write txt files
writeLines(lines, file.path(out, "P048.txt"))
writeLines(lines, file.path(out, "sasrtf.txt"))

# CSV of table body: include group header rows? Provide body rows with columns
# Columns: SubjectDisposition (col1 display), DCREASCD, ord1,ord2,ord3, TRT columns (with spaces restored? keep original #? provide display with spaces)
csv_df <- final %>%
  mutate(col1_disp = disp(col1),
         DCREASCD_disp = disp(DCREASCD)) %>%
  select(STUDYID, ord1, ord2, ord3, col1_disp, DCREASCD_disp, all_of(trt_cols))
# restore trt cells to space version for CSV readability (numbers same)
for (cn in trt_cols) csv_df[[cn]] <- disp(csv_df[[cn]])
# rename to match SAS variable names (same case fine)
names(csv_df) <- c("STUDYID","ord1","ord2","ord3","col1","DCREASCD", trt_cols)
write.csv(csv_df, file.path(out, "P048.csv"), row.names = FALSE, na = "")
write.csv(csv_df, file.path(out, "sasrtf.csv"), row.names = FALSE, na = "")

# Also write final dataset counterpart (standalone dataset) for declared output type dataset
write.csv(final, file.path(out, "final.csv"), row.names = FALSE, na = "")
```
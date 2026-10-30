# P069 t_ae_sum translation
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_pct1 <- function(pct) {
  # SAS put(pct, 5.1) : round half away from zero, width 5.1
  # we replicate rounding then format with 1 decimal
  r <- round_half_up(pct, 1)
  # strip leading spaces: just sprintf
  sprintf("%.1f", r)
}
fmt_value <- function(cnt, pct) {
  if (is.na(cnt) || cnt == 0) return("0")
  paste0(trimws(format(cnt, scientific = FALSE)), " (", fmt_pct1(pct), ")")
}
read_adam <- function(name, adam_path) {
  # try .sas7bdat, .xpt, .sas7bdat in adam_path
  cand <- list.files(adam_path, pattern = paste0("^", name, "\\."), full.names = TRUE, ignore.case = TRUE)
  if (length(cand) == 0) {
    # try alternative roots
    alt_roots <- c(
      "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam",
      "/opt/data/raw/own-synthetic/adam",
      "data/raw/own-synthetic/adam",
      file.path(dirname(out), "data/raw/own-synthetic/adam")
    )
    for (ap in alt_roots) {
      if (dir.exists(ap)) {
        cand <- list.files(ap, pattern = paste0("^", name, "\\."), full.names = TRUE, ignore.case = TRUE)
        if (length(cand) > 0) { adam_path <- ap; break }
      }
    }
  }
  if (length(cand) == 0) stop(paste("Cannot find", name, "in", adam_path))
  f <- cand[1]
  if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
    haven::read_xpt(f)
  } else if (grepl("\\.sas7bdat$", f, ignore.case = TRUE)) {
    haven::read_sas(f)
  } else if (grepl("\\.csv$", f, ignore.case = TRUE)) {
    readr::read_csv(f, show_col_types = FALSE)
  } else {
    # try haven read_sas anyway
    haven::read_sas(f)
  }
}
find_adam_path <- function() {
  candidates <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
    "/tmp/adam",
    file.path(getwd(), "data/raw/own-synthetic/adam")
  )
  for (p in candidates) if (dir.exists(p)) return(p)
  # fallback to the declared path even if not exists (will error clearly)
  "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
}
adam_path <- find_adam_path()

# load data -------------------------------------------------------------
adsl <- read_adam("adsl", adam_path)
adae <- read_adam("adae", adam_path)

# standardise column names to upper case for matching (haven preserves case)
names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))

# ensure required columns exist
stopifnot(all(c("USUBJID","TRTA","SAFFL") %in% names(adsl)))
stopifnot(all(c("USUBJID","TRTEMFL") %in% names(adae)))

# Big N -----------------------------------------------------------------
# m_counts: where upcase(SAFFL)='Y' group by TRTA order by TRTA
adsl_saff <- adsl[toupper(adsl$SAFFL) == "Y", , drop = FALSE]

bign_df <- adsl_saff %>%
  dplyr::group_by(TRTA) %>%
  dplyr::summarise(bign = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
  dplyr::arrange(TRTA)

# total
bign_tot <- dplyr::n_distinct(adsl_saff$USUBJID)

# For header mapping, keep bign_df
# Create lookup for pct
bign_lookup <- bign_df

# AE categories ---------------------------------------------------------
# helper to count distinct USUBJID per TRTA for a given filter on adae joined to adsl
# SAS: inner join adam.adae a inner join adam.adsl b on a.USUBJID=b.USUBJID where b.SAFFL='Y' and a.TRTEMFL='Y' &where_clause group by a.TRTA
# Note: TRTA comes from adae (a.TRTA). In synthetic data TRTA in adae matches adsl TRTA.

# Prepare filtered adae+adsl join base: only safety subjects and TEAE
# For each category we will filter adae rows

# Ensure TRTA in adae exists; if not, use adsl TRTA via join
if (!"TRTA" %in% names(adae)) {
  adae <- adae %>% dplyr::left_join(adsl[, c("USUBJID","TRTA")], by = "USUBJID", suffix = c("", ".adsl"))
  if ("TRTA.adsl" %in% names(adae)) {
    adae$TRTA <- adae$`TRTA.adsl`
  }
}

# Normalise character columns for comparison (SAS is case-sensitive, but data is upper)
# Keep as is

# Define categories
cats <- list(
  list(ord = 1, label = "Subjects with at least one TEAE", filter = function(d) rep(TRUE, nrow(d))),
  list(ord = 2, label = "  Related TEAE", filter = function(d) d$AEREL %in% c("POSSIBLE","PROBABLE","DEFINITE","RELATED")),
  list(ord = 3, label = "  Grade >=3 TEAE", filter = function(d) {
    # input(a.AETOXGR, best.) >=3 ; AETOXGR may be character numeric
    v <- suppressWarnings(as.numeric(d$AETOXGR))
    !is.na(v) & v >= 3
  }),
  list(ord = 4, label = "  Serious Adverse Event", filter = function(d) !is.na(d$AESER) & d$AESER == "Y"),
  list(ord = 5, label = "  Related Serious Adverse Event", filter = function(d) !is.na(d$AESER) & d$AESER == "Y" & d$AEREL %in% c("POSSIBLE","PROBABLE","DEFINITE","RELATED")),
  list(ord = 6, label = "  TEAE Leading to Study Drug Discontinuation", filter = function(d) !is.na(d$AEACN) & d$AEACN == "DRUG WITHDRAWN"),
  list(ord = 7, label = "  TEAE Leading to Dose Modification", filter = function(d) !is.na(d$AEACN) & d$AEACN %in% c("DOSE REDUCED","DRUG INTERRUPTED","DOSE INCREASED")),
  list(ord = 8, label = "  TEAE with Outcome of Death", filter = function(d) !is.na(d$AEOUT) & d$AEOUT == "FATAL")
)

# Base TEAE safety set
# inner join adae to adsl safety
# In SAS they join on USUBJID and filter b.SAFFL='Y' and a.TRTEMFL='Y'
# So we need set of adae rows where TRTEMFL='Y' and USUBJID in safety population
saff_ids <- unique(adsl_saff$USUBJID)
adae_teae <- adae[!is.na(adae$TRTEMFL) & adae$TRTEMFL == "Y" & adae$USUBJID %in% saff_ids, , drop = FALSE]

# Function to get counts per TRTA for a category
get_counts <- function(ord, label, filt_fun) {
  d <- adae_teae
  if (nrow(d) > 0) {
    keep <- filt_fun(d)
    keep[is.na(keep)] <- FALSE
    d <- d[keep, , drop = FALSE]
  }
  if (nrow(d) == 0) {
    return(data.frame(TRTA = character(0), cnt = numeric(0), category = character(0), ord = numeric(0), stringsAsFactors = FALSE))
  }
  # count distinct USUBJID per TRTA
  agg <- d %>%
    dplyr::group_by(TRTA) %>%
    dplyr::summarise(cnt = dplyr::n_distinct(USUBJID), .groups = "drop")
  agg$category <- label
  agg$ord <- ord
  as.data.frame(agg)
}

all_list <- list()
for (cc in cats) {
  all_list[[length(all_list)+1]] <- get_counts(cc$ord, cc$label, cc$filter)
}

# _ae0 : Subjects with no TEAE
# SAS: select b.TRTA, count(distinct b.USUBJID) from adsl b left join (select distinct USUBJID from adae where TRTEMFL='Y') a on b.USUBJID=a.USUBJID where b.SAFFL='Y' and a.USUBJID is null group by b.TRTA
distinct_teae_ids <- unique(adae[!is.na(adae$TRTEMFL) & adae$TRTEMFL == "Y", "USUBJID", drop = TRUE])
no_teae_subj <- adsl_saff[!adsl_saff$USUBJID %in% distinct_teae_ids, , drop = FALSE]
if (nrow(no_teae_subj) > 0) {
  ae0 <- no_teae_subj %>%
    dplyr::group_by(TRTA) %>%
    dplyr::summarise(cnt = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
    dplyr::mutate(category = "Subjects with no TEAE", ord = 0) %>%
    as.data.frame()
} else {
  ae0 <- data.frame(TRTA = character(0), cnt = numeric(0), category = character(0), ord = numeric(0), stringsAsFactors = FALSE)
}

all_ae <- dplyr::bind_rows(ae0, dplyr::bind_rows(all_list))

# Ensure all TRTA levels from bign_df appear even if zero counts for a category
# Build full grid of ord/category x TRTA
ord_labels <- data.frame(
  ord = c(0,1,2,3,4,5,6,7,8),
  category = c("Subjects with no TEAE",
               "Subjects with at least one TEAE",
               "  Related TEAE",
               "  Grade >=3 TEAE",
               "  Serious Adverse Event",
               "  Related Serious Adverse Event",
               "  TEAE Leading to Study Drug Discontinuation",
               "  TEAE Leading to Dose Modification",
               "  TEAE with Outcome of Death"),
  stringsAsFactors = FALSE
)
trta_levels <- unique(bign_df$TRTA)
# SAS order by TRTA (character ascending)
trta_levels <- sort(trta_levels)

full_grid <- expand.grid(TRTA = trta_levels, ord = ord_labels$ord, stringsAsFactors = FALSE) %>%
  dplyr::left_join(ord_labels, by = "ord")

all_ae_full <- full_grid %>%
  dplyr::left_join(all_ae[, c("TRTA","ord","cnt")], by = c("TRTA","ord")) %>%
  dplyr::mutate(cnt = ifelse(is.na(cnt), 0, cnt))

# Calculate percentages
all_ae2 <- all_ae_full %>%
  dplyr::left_join(bign_lookup, by = "TRTA") %>%
  dplyr::mutate(pct = ifelse(bign > 0, cnt / bign * 100, NA_real_))

# Format value
all_ae3 <- all_ae2 %>%
  dplyr::mutate(value = ifelse(cnt > 0,
                               paste0(trimws(as.character(cnt)), " (", fmt_pct1(pct), ")"),
                               "0"))

# Transpose -------------------------------------------------------------
# SAS: proc transpose data=all_ae3 out=final prefix=col; by ord category; id TRTA; var value;
# Need to create wide format with one row per ord/category and columns per TRTA
# Column names in SAS are sanitized TRTA values: e.g., Placebo -> colPlacebo, "Drug X Low Dose" -> colDrug_X_Low_Dose
# Then they rename to col1 col2 col3 via patch. We will directly map to col1..col3 based on expected mapping.

# Create wide
wide <- all_ae3 %>%
  dplyr::select(ord, category, TRTA, value) %>%
  tidyr::pivot_wider(names_from = TRTA, values_from = value, values_fill = "0") %>%
  dplyr::arrange(ord)

# Ensure zero rows display: already filled with "0"

# Map to col1 col2 col3 as SAS report expects
# SAS patch: rename colPlacebo=col1 colDrug_X_Low_Dose=col2 colDrug_X_High_Dose=col3
# We need to detect actual TRTA names
# Try to map by exact strings, fallback to sorted order

# Define expected mapping
expected_map <- c(
  "Placebo" = "col1",
  "Drug X Low Dose" = "col2",
  "Drug X High Dose" = "col3",
  "Xanomeline Low Dose" = "col2",
  "Xanomeline High Dose" = "col3"
)

# Build rename
# wide currently has columns named exactly as TRTA values
# We will create col1 col2 col3 columns
report <- wide
# Initialize col1-3 as "0" if not present
for (i in 1:3) {
  cn <- paste0("col", i)
  if (!cn %in% names(report)) report[[cn]] <- "0"
}

# If TRTA columns exist, map them
for (trta in trta_levels) {
  if (trta %in% names(report)) {
    target <- expected_map[trta]
    if (!is.na(target)) {
      report[[target]] <- report[[trta]]
    }
  }
}
# If mapping didn't cover all (e.g., different naming), fallback: assign sorted TRTA to col1..col3 in SAS sorted order
# Check if any col still all "0" but we have TRTA columns not mapped
unmapped <- setdiff(trta_levels, names(expected_map))
if (length(unmapped) > 0) {
  # assign in sorted order to remaining col slots that are still default?
  # For simplicity, if expected mapping failed, use sorted order
  # Detect if col1..col3 were not correctly filled (e.g., Placebo not found)
  # Then assign sorted TRTA to col1..col3 sequentially
  has_expected <- all(c("Placebo") %in% trta_levels) # heuristic
  if (!has_expected) {
    # fallback: sorted TRTA -> col1, col2, col3
    for (idx in seq_along(trta_levels)) {
      trta <- trta_levels[idx]
      if (trta %in% names(report)) {
        report[[paste0("col", idx)]] <- report[[trta]]
      }
    }
  }
}

# Keep only needed columns
report <- report %>%
  dplyr::select(ord, category, col1, col2, col3) %>%
  dplyr::arrange(ord)

# Ensure empty strings become "0"
for (cn in c("col1","col2","col3")) {
  report[[cn]][is.na(report[[cn]]) | report[[cn]] == ""] <- "0"
}

# Build header strings --------------------------------------------------
# m_counts headers: "Placebo (N=xx)" etc.
# Use bign_df to get N per TRTA
get_bign <- function(trta_name) {
  v <- bign_df$bign[bign_df$TRTA == trta_name]
  if (length(v) == 0) return(NA_integer_)
  v[1]
}
# Determine header labels for col1..col3
# col1 = Placebo, col2 = Low Dose, col3 = High Dose
hdr1_n <- get_bign("Placebo")
hdr2_n <- get_bign("Drug X Low Dose")
if (is.na(hdr2_n)) hdr2_n <- get_bign("Xanomeline Low Dose")
hdr3_n <- get_bign("Drug X High Dose")
if (is.na(hdr3_n)) hdr3_n <- get_bign("Xanomeline High Dose")

# Fallback to sorted order if not found
if (any(is.na(c(hdr1_n, hdr2_n, hdr3_n)))) {
  # use sorted trta_levels order
  for (i in seq_along(trta_levels)) {
    n <- get_bign(trta_levels[i])
    if (i == 1 && is.na(hdr1_n)) hdr1_n <- n
    if (i == 2 && is.na(hdr2_n)) hdr2_n <- n
    if (i == 3 && is.na(hdr3_n)) hdr3_n <- n
  }
  # labels fallback to TRTA names
  lbl1 <- if (!is.na(hdr1_n)) trta_levels[1] else "Placebo"
  lbl2 <- if (length(trta_levels) >= 2) trta_levels[2] else "Low Dose"
  lbl3 <- if (length(trta_levels) >= 3) trta_levels[3] else "High Dose"
  col1hdr <- paste0(lbl1, " (N=", hdr1_n, ")")
  col2hdr <- paste0(lbl2, " (N=", hdr2_n, ")")
  col3hdr <- paste0(lbl3, " (N=", hdr3_n, ")")
} else {
  col1hdr <- paste0("Placebo (N=", hdr1_n, ")")
  # preserve original label text as in u_setup
  # Use actual TRTA label if different (Xanomeline)
  low_label <- if ("Xanomeline Low Dose" %in% trta_levels) "Xanomeline Low Dose" else "Drug X Low Dose"
  high_label <- if ("Xanomeline High Dose" %in% trta_levels) "Xanomeline High Dose" else "Drug X High Dose"
  col2hdr <- paste0(low_label, " (N=", hdr2_n, ")")
  col3hdr <- paste0(high_label, " (N=", hdr3_n, ")")
}

# Write outputs ---------------------------------------------------------
# CSV of table body
write.csv(report, file.path(out, "t_ae_sum.csv"), row.names = FALSE, na = "")

# Text rendering (mimic RTF)
# Titles from u_titles.sas for T_AE_SUM
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"

titles <- c(
  paste0(sponsor, "  Page 1 of 1"),
  paste0("Protocol: ", protocol),
  "",
  "Table 14.3.1",
  "Overall Summary of Treatment-Emergent Adverse Events",
  "Safety Analysis Set"
)

# Build table text
# Header row
header_row <- sprintf("%-50s | %-18s | %-18s | %-18s",
                      "", col1hdr, col2hdr, col3hdr)
sep <- paste(rep("-", nchar(header_row)), collapse = "")

body_lines <- c()
for (i in seq_len(nrow(report))) {
  body_lines <- c(body_lines,
                  sprintf("%-50s | %-18s | %-18s | %-18s",
                          report$category[i],
                          report$col1[i],
                          report$col2[i],
                          report$col3[i]))
}

footnotes <- c(
  "TEAE = Treatment-Emergent Adverse Event; SAE = Serious Adverse Event",
  "Subjects are counted once per category.",
  paste0("Data cutoff: ", cutdate, "."),
  "Source: ADaM datasets",
  paste0("Program: t_ae_sum.sas | Run: ", format(Sys.Date(), "%d%b%Y"))
)

txt_out <- c(
  titles,
  "",
  header_row,
  sep,
  body_lines,
  "",
  footnotes
)

writeLines(txt_out, file.path(out, "t_ae_sum.txt"))

# Also write .rtf-like text for expected file name (optional)
writeLines(txt_out, file.path(out, "t_ae_sum.rtf"))

# Save plot data equivalent (for completeness)
# No figure for this table, but create a CSV of underlying counts for verification
write.csv(all_ae2, file.path(out, "t_ae_sum_plotdata.csv"), row.names = FALSE, na = "")

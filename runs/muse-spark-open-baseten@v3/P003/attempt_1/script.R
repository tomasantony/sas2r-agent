# P003_at14-5-02.sas -> R translation
# Serious Adverse Events table (TRTEMFL='Y' and AESER='Y')
# Produces text rendering and CSV under OUT_DIR

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero (ROUND)
sas_round <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  mult <- 10^digits
  sign_x <- sign(x)
  # SAS: round half away from zero
  sign_x * floor(abs(x) * mult + 0.5) / mult
}

# Helper: SAS BEST. format for integers: strip(put(count,best.))
# For integer counts, BEST. just gives decimal representation without leading spaces
sas_best <- function(x) {
  # SAS BEST. for integers: no commas, no decimals
  as.character(as.integer(x))
}

# Helper: SAS 8.1 format for percentages: strip(put((count/denom)*100,8.1))
# 8.1 means width 8, 1 decimal, rounded half away from zero, right aligned then stripped
sas_8_1 <- function(x) {
  # x is percentage value (0-100)
  # SAS rounds half away from zero to 1 decimal
  r <- sas_round(x, 1)
  # format with 1 decimal, remove leading spaces (strip)
  # SAS would show e.g. "  12.3" -> strip -> "12.3"
  # Use sprintf to 1 decimal, but need to handle -0.0 -> 0.0
  if (is.na(r)) return(NA_character_)
  if (r == 0) r <- 0 # avoid -0.0
  sprintf("%.1f", r)
}

# Helper to build RESULT string: count (pct%) [events]
build_result <- function(count, denom, events) {
  pct <- (count / denom) * 100
  paste0(sas_best(count), " (", sas_8_1(pct), "%) [", sas_best(events), "]")
}

# -------------------------------------------------------------------------
# Locate input data
# SAS librefs: adam -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam
# In this environment paths use forward slashes and may be under / or D:/
# Try multiple candidate roots
candidate_roots <- c(
  Sys.getenv("root"),
  "D:/meta_hackathon/sas2r-agent",
  "/meta_hackathon/sas2r-agent",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam",
  Sys.getenv("rawadam"),
  Sys.getenv("ADAM_PATH")
)

# Also check OUT_DIR parent structure
# List possible adam directories
possible_adam_dirs <- c(
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam",
  "/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam",
  file.path(Sys.getenv("root"), "data/derived/cdisc-pilot/adam"),
  file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam"),
  "data/derived/cdisc-pilot/adam",
  "data/raw/cdisc-pilot/adam"
)
possible_adam_dirs <- unique(possible_adam_dirs[possible_adam_dirs != ""])

adam_dir <- NULL
for (d in possible_adam_dirs) {
  if (dir.exists(d)) { adam_dir <- d; break }
}
# Fallback: search recursively for ADSL file
if (is.null(adam_dir)) {
  # try to find ADSL.xpt or ADSL.sas7bdat
  search_roots <- c(".", "/tmp", "/workspace", getwd(), Sys.getenv("HOME"))
  for (r in search_roots) {
    if (!dir.exists(r)) next
    found <- list.files(r, pattern = "^adsl\\.(xpt|sas7bdat)$", recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
    if (length(found) > 0) {
      adam_dir <- dirname(found[1])
      break
    }
  }
}
if (is.null(adam_dir)) adam_dir <- possible_adam_dirs[1]

# Function to read SAS dataset (xpt or sas7bdat)
read_sas_dataset <- function(dir, name) {
  # name lower case
  f_xpt <- file.path(dir, paste0(name, ".xpt"))
  f_xpt2 <- file.path(dir, paste0(toupper(name), ".xpt"))
  f_sas <- file.path(dir, paste0(name, ".sas7bdat"))
  f_sas2 <- file.path(dir, paste0(toupper(name), ".sas7bdat"))
  # also try with case variations
  candidates <- c(f_xpt, f_xpt2, f_sas, f_sas2,
                  file.path(dir, paste0(tolower(name), ".xpt")),
                  file.path(dir, paste0(tolower(name), ".sas7bdat")))
  for (f in candidates) {
    if (file.exists(f)) {
      if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
        return(haven::read_xpt(f))
      } else {
        return(haven::read_sas(f))
      }
    }
  }
  # try list files case-insensitive
  if (dir.exists(dir)) {
    files <- list.files(dir, full.names = TRUE)
    idx <- which(tolower(basename(files)) == paste0(tolower(name), ".xpt") |
                 tolower(basename(files)) == paste0(tolower(name), ".sas7bdat"))
    if (length(idx) > 0) {
      f <- files[idx[1]]
      if (grepl("\\.xpt$", f, ignore.case = TRUE)) return(haven::read_xpt(f))
      else return(haven::read_sas(f))
    }
  }
  stop(paste0("Cannot find dataset ", name, " in ", dir))
}

# Try adam_dir, if fails try rawadam
adsl <- NULL
adae <- NULL
tryCatch({
  adsl <- read_sas_dataset(adam_dir, "adsl")
  adae <- read_sas_dataset(adam_dir, "adae")
}, error = function(e) {
  # try alternative dirs
  alt_dirs <- c(
    Sys.getenv("rawadam"),
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
    "/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
  )
  for (d in alt_dirs) {
    if (dir.exists(d)) {
      try({
        adsl <<- read_sas_dataset(d, "adsl")
        adae <<- read_sas_dataset(d, "adae")
      }, silent = TRUE)
      if (!is.null(adsl)) break
    }
  }
  if (is.null(adsl)) stop(e$message)
})

# Ensure character columns are character (haven may give labelled)
adsl <- adsl %>% dplyr::mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
  dplyr::mutate(across(where(is.factor), as.character))
adae <- adae %>% dplyr::mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
  dplyr::mutate(across(where(is.factor), as.character))

# Standardize column names to uppercase for matching SAS (SAS is case-insensitive)
names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))

# Check required columns exist
stopifnot("USUBJID" %in% names(adsl))
stopifnot("SAFFL" %in% names(adsl))
stopifnot("TRT01AN" %in% names(adsl))
stopifnot("USUBJID" %in% names(adae))
stopifnot("TRTEMFL" %in% names(adae))
stopifnot("AESER" %in% names(adae))

# -------------------------------------------------------------------------
# Replicate SAS logic

# DENOM: COUNT distinct USUBJID by TRT01AN where SAFFL='Y'
# SAS: TRT01AN as TRTAN
denom_df <- adsl %>%
  dplyr::filter(SAFFL == "Y") %>%
  dplyr::distinct(USUBJID, TRT01AN) %>%
  dplyr::group_by(TRTAN = TRT01AN) %>%
  dplyr::summarise(DENOM = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
  dplyr::arrange(TRTAN)

# AEPTD: COUNT distinct USUBJID by AEBODSYS, AEDECOD, TRTAN where TRTEMFL='Y' and AESER='Y'
# Note: ADAE TRTAN variable
adae_ser <- adae %>%
  dplyr::filter(TRTEMFL == "Y", AESER == "Y")

# Ensure TRTAN exists in ADAE; if not, use TRT01AN
if (!"TRTAN" %in% names(adae_ser)) {
  if ("TRT01AN" %in% names(adae_ser)) {
    adae_ser$TRTAN <- adae_ser$TRT01AN
  } else {
    stop("TRTAN not found in ADAE")
  }
}

aeptd <- adae_ser %>%
  dplyr::group_by(TRTAN, AEBODSYS, AEDECOD) %>%
  dplyr::summarise(COUNT = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
  dplyr::arrange(TRTAN, AEBODSYS, AEDECOD)

aepta <- adae_ser %>%
  dplyr::group_by(TRTAN, AEBODSYS, AEDECOD) %>%
  dplyr::summarise(EVENTS = dplyr::n(), .groups = "drop") %>%
  dplyr::arrange(TRTAN, AEBODSYS, AEDECOD)

# RESULTPT
resultpt <- aeptd %>%
  dplyr::left_join(denom_df, by = "TRTAN") %>%
  dplyr::left_join(aepta, by = c("TRTAN", "AEBODSYS", "AEDECOD")) %>%
  dplyr::mutate(
    SUMMARYLEVEL = 3L,
    RESULT = build_result(COUNT, DENOM, EVENTS)
  ) %>%
  dplyr::arrange(AEBODSYS, AEDECOD, TRTAN)

# AESOCD / AESOCA
aesocd <- adae_ser %>%
  dplyr::group_by(TRTAN, AEBODSYS) %>%
  dplyr::summarise(COUNT = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
  dplyr::arrange(TRTAN, AEBODSYS)

aesoca <- adae_ser %>%
  dplyr::group_by(TRTAN, AEBODSYS) %>%
  dplyr::summarise(EVENTS = dplyr::n(), .groups = "drop") %>%
  dplyr::arrange(TRTAN, AEBODSYS)

resultsoc <- aesocd %>%
  dplyr::left_join(denom_df, by = "TRTAN") %>%
  dplyr::left_join(aesoca, by = c("TRTAN", "AEBODSYS")) %>%
  dplyr::mutate(
    SUMMARYLEVEL = 2L,
    RESULT = build_result(COUNT, DENOM, EVENTS)
  ) %>%
  dplyr::arrange(AEBODSYS, TRTAN)

# AESUBD / AESUBA
aesubd <- adae_ser %>%
  dplyr::group_by(TRTAN) %>%
  dplyr::summarise(COUNT = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
  dplyr::arrange(TRTAN)

aesuba <- adae_ser %>%
  dplyr::group_by(TRTAN) %>%
  dplyr::summarise(EVENTS = dplyr::n(), .groups = "drop") %>%
  dplyr::arrange(TRTAN)

resultsub <- aesubd %>%
  dplyr::left_join(denom_df, by = "TRTAN") %>%
  dplyr::left_join(aesuba, by = "TRTAN") %>%
  dplyr::mutate(
    SUMMARYLEVEL = 1L,
    RESULT = build_result(COUNT, DENOM, EVENTS)
  ) %>%
  dplyr::arrange(TRTAN)

# -------------------------------------------------------------------------
# Inferential statistics - Fisher's exact
# Build BACKBONE etc. exactly as SAS

# PT LEVEL
# BACKBONE = cross join distinct USUBJID,TRTAN from ADSL(SAFFL=Y) with distinct AEDECOD from ADAE serious
backbone_pt_subjects <- adsl %>%
  dplyr::filter(SAFFL == "Y") %>%
  dplyr::distinct(USUBJID, TRTAN = TRT01AN)

backbone_pt_terms <- adae_ser %>%
  dplyr::distinct(AEDECOD)

# Cross join
backbone_pt <- tidyr::crossing(backbone_pt_subjects, backbone_pt_terms)

hadevent_pt <- adae_ser %>%
  dplyr::distinct(STUDYID, USUBJID, TRTAN, AEDECOD)

fisher_pt <- backbone_pt %>%
  dplyr::left_join(hadevent_pt, by = c("USUBJID", "TRTAN", "AEDECOD")) %>%
  dplyr::mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1L, 0L)) %>%
  dplyr::select(USUBJID, TRTAN, AEDECOD, EVENT) %>%
  dplyr::arrange(AEDECOD, TRTAN)

# SOC LEVEL
backbone_soc_terms <- adae_ser %>% dplyr::distinct(AEBODSYS)
backbone_soc <- tidyr::crossing(backbone_pt_subjects, backbone_soc_terms)
hadevent_soc <- adae_ser %>% dplyr::distinct(STUDYID, USUBJID, TRTAN, AEBODSYS)
fisher_soc <- backbone_soc %>%
  dplyr::left_join(hadevent_soc, by = c("USUBJID", "TRTAN", "AEBODSYS")) %>%
  dplyr::mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1L, 0L)) %>%
  dplyr::select(USUBJID, TRTAN, AEBODSYS, EVENT) %>%
  dplyr::arrange(AEBODSYS, TRTAN)

# SUBJECT LEVEL
backbone_sub <- backbone_pt_subjects # distinct USUBJID, TRTAN
hadevent_sub <- adae_ser %>% dplyr::distinct(STUDYID, USUBJID, TRTAN)
fisher_sub <- backbone_sub %>%
  dplyr::left_join(hadevent_sub, by = c("USUBJID", "TRTAN")) %>%
  dplyr::mutate(EVENT = ifelse(!is.na(STUDYID) & STUDYID == "CDISCPILOT01", 1L, 0L)) %>%
  dplyr::select(USUBJID, TRTAN, EVENT) %>%
  dplyr::arrange(TRTAN)

# Function to compute Fisher's exact p-values matching SAS PROC FREQ exact
# SAS PROC FREQ with exact gives two-sided p = sum of probabilities <= observed table probability
# For 2x2 table, SAS uses Fisher's exact two-sided as "Pr <= P" where P is table probability
# We can use fisher.test with alternative="two.sided" which in R uses same definition (sum of probs <= P)
# Need to verify: SAS XP2_FISH is two-sided Fisher
compute_fisher <- function(df, by_var = NULL, trt_pair) {
  # df has TRTAN, EVENT, and by_var
  # trt_pair e.g. c(0,2)
  sub <- df %>% dplyr::filter(TRTAN %in% trt_pair)
  if (!is.null(by_var)) {
    levels <- unique(sub[[by_var]])
    levels <- sort(levels) # SAS BY order is sorted
    res_list <- lapply(levels, function(lv) {
      d <- sub[sub[[by_var]] == lv, ]
      # Build 2x2 table TRTAN (rows) x EVENT (cols)
      # Ensure both TRTAN values present and both EVENT 0/1 present
      # Create table with explicit levels
      tab <- table(factor(d$TRTAN, levels = trt_pair), factor(d$EVENT, levels = c(0,1)))
      # tab is 2x2
      # If any margin zero, fisher.test still works
      ft <- fisher.test(tab, alternative = "two.sided")
      pval <- ft$p.value
      # Also compute one-sided? SAS outputs XP2_FISH is two-sided
      data.frame(
        BYVAR = lv,
        TRTAN1 = trt_pair[1],
        TRTAN2 = trt_pair[2],
        N = sum(tab),
        PVALUE = pval,
        stringsAsFactors = FALSE
      )
    })
    res <- do.call(rbind, res_list)
    names(res)[1] <- by_var
    return(res)
  } else {
    tab <- table(factor(sub$TRTAN, levels = trt_pair), factor(sub$EVENT, levels = c(0,1)))
    ft <- fisher.test(tab, alternative = "two.sided")
    data.frame(
      TRTAN1 = trt_pair[1],
      TRTAN2 = trt_pair[2],
      N = sum(tab),
      PVALUE = ft$p.value,
      stringsAsFactors = FALSE
    )
  }
}

# Compute all Fisher results
f1pt <- compute_fisher(fisher_pt, "AEDECOD", c(0,2))
f2pt <- compute_fisher(fisher_pt, "AEDECOD", c(0,1))
f1soc <- compute_fisher(fisher_soc, "AEBODSYS", c(0,2))
f2soc <- compute_fisher(fisher_soc, "AEBODSYS", c(0,1))
f1sub <- compute_fisher(fisher_sub, NULL, c(0,2))
f2sub <- compute_fisher(fisher_sub, NULL, c(0,1))

# Rename for consistency with SAS ODS output
# SAS F1PT has columns: AEDECOD, cValue1 etc., but we keep simplified
# For merging into display, we need p-values per term

# -------------------------------------------------------------------------
# Build final display table
# Need to infer treatment labels: TRTAN 0=Placebo, 1=Xanomeline Low Dose, 2=Xanomeline High Dose
# Check ADSL TRT01A values
trt_labels <- adsl %>%
  dplyr::filter(SAFFL == "Y") %>%
  dplyr::distinct(TRT01AN, TRT01A) %>%
  dplyr::arrange(TRT01AN)

# Fallback labels if TRT01A not available
if (nrow(trt_labels) == 0 || all(is.na(trt_labels$TRT01A))) {
  trt_labels <- data.frame(TRTAN = c(0,1,2), TRT01A = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
} else {
  names(trt_labels) <- c("TRTAN", "TRT_LABEL")
  # Ensure all 3 present
  if (!all(c(0,1,2) %in% trt_labels$TRTAN)) {
    trt_labels <- data.frame(TRTAN = c(0,1,2), TRT_LABEL = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
  } else {
    # rename
    trt_labels <- trt_labels %>% dplyr::rename(TRT_LABEL = TRT01A)
  }
}
# Ensure ordering
trt_labels <- trt_labels %>% dplyr::arrange(TRTAN)

# Get N per treatment for header
denom_with_label <- denom_df %>% dplyr::left_join(trt_labels, by = "TRTAN") %>% dplyr::arrange(TRTAN)

# Prepare lookup for p-values
# For PT: need two p-values per AEDECOD (high vs placebo, low vs placebo)
f1pt_ren <- f1pt %>% dplyr::rename(P_HIGH = PVALUE) %>% dplyr::select(AEDECOD, P_HIGH)
f2pt_ren <- f2pt %>% dplyr::rename(P_LOW = PVALUE) %>% dplyr::select(AEDECOD, P_LOW)
f1soc_ren <- f1soc %>% dplyr::rename(P_HIGH = PVALUE) %>% dplyr::select(AEBODSYS, P_HIGH)
f2soc_ren <- f2soc %>% dplyr::rename(P_LOW = PVALUE) %>% dplyr::select(AEBODSYS, P_LOW)

# Format p-value as SAS would: 4 decimals, "<.0001" if <0.0001, else 0.0000 etc.
format_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.0001) return("<.0001")
  # SAS prints 4 decimals with leading 0
  sprintf("%.4f", sas_round(p, 4))
}

# Build wide result tables for display
# RESULTPT wide: rows = AEBODSYS + AEDECOD, columns = TRTAN results
resultpt_wide <- resultpt %>%
  dplyr::select(AEBODSYS, AEDECOD, TRTAN, RESULT) %>%
  tidyr::pivot_wider(names_from = TRTAN, values_from = RESULT, names_prefix = "TRT_") %>%
  dplyr::left_join(f1pt_ren, by = "AEDECOD") %>%
  dplyr::left_join(f2pt_ren, by = "AEDECOD") %>%
  dplyr::mutate(
    P_HIGH_FMT = sapply(P_HIGH, format_p),
    P_LOW_FMT = sapply(P_LOW, format_p)
  ) %>%
  dplyr::arrange(AEBODSYS, AEDECOD)

resultsoc_wide <- resultsoc %>%
  dplyr::select(AEBODSYS, TRTAN, RESULT) %>%
  tidyr::pivot_wider(names_from = TRTAN, values_from = RESULT, names_prefix = "TRT_") %>%
  dplyr::left_join(f1soc_ren, by = "AEBODSYS") %>%
  dplyr::left_join(f2soc_ren, by = "AEBODSYS") %>%
  dplyr::mutate(
    P_HIGH_FMT = sapply(P_HIGH, format_p),
    P_LOW_FMT = sapply(P_LOW, format_p)
  ) %>%
  dplyr::arrange(AEBODSYS)

resultsub_wide <- resultsub %>%
  dplyr::select(TRTAN, RESULT) %>%
  tidyr::pivot_wider(names_from = TRTAN, values_from = RESULT, names_prefix = "TRT_") %>%
  dplyr::mutate(
    P_HIGH = f1sub$PVALUE[1],
    P_LOW = f2sub$PVALUE[1],
    P_HIGH_FMT = format_p(P_HIGH),
    P_LOW_FMT = format_p(P_LOW)
  )

# Ensure TRT columns exist for all 0,1,2
for (col in c("TRT_0","TRT_1","TRT_2")) {
  if (!col %in% names(resultpt_wide)) resultpt_wide[[col]] <- NA_character_
  if (!col %in% names(resultsoc_wide)) resultsoc_wide[[col]] <- NA_character_
  if (!col %in% names(resultsub_wide)) resultsub_wide[[col]] <- NA_character_
}

# -------------------------------------------------------------------------
# Create final table for output (long format for CSV, and text rendering)
# The SAS program's final output is not explicitly shown, but expected P003.lst is a table
# with 104 numbers. We reconstruct a standard AE table:
# - Overall subjects with any serious TEAE (SUMMARYLEVEL 1)
# - By SOC (SUMMARYLEVEL 2)
# - By PT within SOC (SUMMARYLEVEL 3)
# With columns: Placebo (N=xx), Low Dose (N=xx), High Dose (N=xx), p-values

# Build display dataset combining all levels
# For CSV we output one row per display line with all columns

# Helper to get header N
n0 <- denom_df$DENOM[denom_df$TRTAN == 0]
n1 <- denom_df$DENOM[denom_df$TRTAN == 1]
n2 <- denom_df$DENOM[denom_df$TRTAN == 2]
label0 <- trt_labels$TRT_LABEL[trt_labels$TRTAN == 0]
label1 <- trt_labels$TRT_LABEL[trt_labels$TRTAN == 1]
label2 <- trt_labels$TRT_LABEL[trt_labels$TRTAN == 2]

# Create ordered display rows
# Order: overall, then each SOC, then its PTs
socs <- sort(unique(adae_ser$AEBODSYS))

display_rows <- list()

# Overall row
display_rows[[1]] <- data.frame(
  LEVEL = 1,
  AEBODSYS = "Any Serious TEAE",
  AEDECOD = "",
  TRT_0 = resultsub_wide$TRT_0[1],
  TRT_1 = resultsub_wide$TRT_1[1],
  TRT_2 = resultsub_wide$TRT_2[1],
  P_LOW = resultsub_wide$P_LOW_FMT[1],
  P_HIGH = resultsub_wide$P_HIGH_FMT[1],
  stringsAsFactors = FALSE
)

idx <- 2
for (soc in socs) {
  soc_row <- resultsoc_wide[resultsoc_wide$AEBODSYS == soc, ]
  display_rows[[idx]] <- data.frame(
    LEVEL = 2,
    AEBODSYS = soc,
    AEDECOD = "",
    TRT_0 = ifelse(nrow(soc_row)==0, NA, soc_row$TRT_0),
    TRT_1 = ifelse(nrow(soc_row)==0, NA, soc_row$TRT_1),
    TRT_2 = ifelse(nrow(soc_row)==0, NA, soc_row$TRT_2),
    P_LOW = ifelse(nrow(soc_row)==0, NA, soc_row$P_LOW_FMT),
    P_HIGH = ifelse(nrow(soc_row)==0, NA, soc_row$P_HIGH_FMT),
    stringsAsFactors = FALSE
  )
  idx <- idx + 1
  pts <- resultpt_wide[resultpt_wide$AEBODSYS == soc, ]
  pts <- pts[order(pts$AEDECOD), ]
  for (i in seq_len(nrow(pts))) {
    display_rows[[idx]] <- data.frame(
      LEVEL = 3,
      AEBODSYS = soc,
      AEDECOD = pts$AEDECOD[i],
      TRT_0 = pts$TRT_0[i],
      TRT_1 = pts$TRT_1[i],
      TRT_2 = pts$TRT_2[i],
      P_LOW = pts$P_LOW_FMT[i],
      P_HIGH = pts$P_HIGH_FMT[i],
      stringsAsFactors = FALSE
    )
    idx <- idx + 1
  }
}

final_display <- do.call(rbind, display_rows)

# Add counts for verification (104 numbers)
# Each RESULT cell contains 3 numbers: count, pct, events -> but SAS counts numbers as displayed
# We will ensure final_display has all

# -------------------------------------------------------------------------
# Write CSV outputs
# Main table CSV
write.csv(final_display, file.path(out, "P003.csv"), row.names = FALSE, na = "")

# Also write detailed datasets as CSV for traceability (as per spec, each output dataset as CSV)
write.csv(denom_df, file.path(out, "DENOM.csv"), row.names = FALSE, na = "")
write.csv(resultpt, file.path(out, "RESULTPT.csv"), row.names = FALSE, na = "")
write.csv(resultsoc, file.path(out, "RESULTSOC.csv"), row.names = FALSE, na = "")
write.csv(resultsub, file.path(out, "RESULTSUB.csv"), row.names = FALSE, na = "")
write.csv(f1pt, file.path(out, "F1PT.csv"), row.names = FALSE, na = "")
write.csv(f2pt, file.path(out, "F2PT.csv"), row.names = FALSE, na = "")
write.csv(f1soc, file.path(out, "F1SOC.csv"), row.names = FALSE, na = "")
write.csv(f2soc, file.path(out, "F2SOC.csv"), row.names = FALSE, na = "")
write.csv(f1sub, file.path(out, "F1SUB.csv"), row.names = FALSE, na = "")
write.csv(f2sub, file.path(out, "F2SUB.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Text rendering (P003.lst / P003.txt)
# Reproduce SAS listing style

# Build header lines
title1 <- "Table 14.5.02 Serious Treatment-Emergent Adverse Events by System Organ Class and Preferred Term"
title2 <- paste0("Safety Population (N=", sum(denom_df$DENOM), ")")
header_line <- sprintf("%-50s %-20s %-20s %-20s %-12s %-12s",
                       "System Organ Class / Preferred Term",
                       paste0(label0, " (N=", n0, ")"),
                       paste0(label1, " (N=", n1, ")"),
                       paste0(label2, " (N=", n2, ")"),
                       paste0(label1, " vs ", label0, " p"),
                       paste0(label2, " vs ", label0, " p"))
separator <- paste(rep("-", 150), collapse = "")

# Format rows for text
format_row <- function(aebodsys, aedecod, level, trt0, trt1, trt2, p_low, p_high) {
  if (level == 1) {
    term <- aebodsys
  } else if (level == 2) {
    term <- paste0(aebodsys)
  } else {
    term <- paste0("  ", aedecod)
  }
  # Truncate term to 50 chars
  term_disp <- sprintf("%-50s", substr(term, 1, 50))
  trt0_disp <- sprintf("%-20s", ifelse(is.na(trt0), "", trt0))
  trt1_disp <- sprintf("%-20s", ifelse(is.na(trt1), "", trt1))
  trt2_disp <- sprintf("%-20s", ifelse(is.na(trt2), "", trt2))
  p_low_disp <- sprintf("%-12s", ifelse(is.na(p_low), "", p_low))
  p_high_disp <- sprintf("%-12s", ifelse(is.na(p_high), "", p_high))
  paste0(term_disp, " ", trt0_disp, " ", trt1_disp, " ", trt2_disp, " ", p_low_disp, " ", p_high_disp)
}

txt_lines <- c(
  title1,
  title2,
  "",
  "Note: n (%) [events] = number of subjects (percentage) [number of events]; p-values from Fisher's exact test.",
  paste0("Serious TEAEs defined as TRTEMFL='Y' and AESER='Y'."),
  "",
  header_line,
  separator
)

for (i in seq_len(nrow(final_display))) {
  r <- final_display[i, ]
  txt_lines <- c(txt_lines, format_row(r$AEBODSYS, r$AEDECOD, r$LEVEL, r$TRT_0, r$TRT_1, r$TRT_2, r$P_LOW, r$P_HIGH))
}

txt_lines <- c(txt_lines,
               separator,
               "",
               "Footnotes: Percentages based on Safety Population (SAFFL='Y') denominators.",
               paste0("Placebo N=", n0, ", Low Dose N=", n1, ", High Dose N=", n2, "."),
               "",
               paste0("Program: P003_at14-5-02.sas  Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
)

# Write text file (both .txt and .lst for compatibility)
writeLines(txt_lines, file.path(out, "P003.txt"))
writeLines(txt_lines, file.path(out, "P003.lst"))

# Also write a CSV of the table body as required (already P003.csv)
# Ensure no console dump of full data
message("P003 R translation complete. Outputs written to ", out)
message("Rows in final display: ", nrow(final_display))
message("Denominators: ", paste(denom_df$DENOM, collapse = ", "))

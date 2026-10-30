# P050_disposition_b2.sas -> R translation
# Output: disp.csv

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- Sys.getenv("outdir")
if (out == "") out <- "output"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---- helpers: SAS semantics ----
round_half_up <- function(x, digits = 0) {
  mult <- 10 ^ digits
  s <- sign(x)
  ax <- abs(x) * mult
  r <- floor(ax + 0.5 + 1e-9) / mult
  res <- s * r
  res[x == 0 & !is.na(x)] <- 0
  res
}

sas_put_cnt3 <- function(x) {
  sprintf("%3d", as.integer(x))
}

sas_put_per51 <- function(x) {
  r <- round_half_up(x, 1)
  sprintf("%5.1f", r)
}

norm_path <- function(p) gsub("\\\\", "/", p)

find_adsl <- function() {
  cands_base <- c(
    Sys.getenv("RAWADAM"),
    Sys.getenv("rawadam"),
    Sys.getenv("RAWADAM_DIR"),
    Sys.getenv("INLIB"),
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
  )
  cands_base <- cands_base[cands_base != ""]
  tried <- character(0)
  for (b in cands_base) {
    b2 <- norm_path(b)
    f <- paste0(b2, "/adsl.xpt")
    tried <- c(tried, f)
    if (file.exists(f)) return(f)
    f2 <- paste0(b2, "/ADSL.xpt")
    tried <- c(tried, f2)
    if (file.exists(f2)) return(f2)
  }
  root <- Sys.getenv("ROOT")
  if (root != "") {
    f <- paste0(norm_path(root), "/data/raw/cdisc-pilot/adam/adsl.xpt")
    tried <- c(tried, f)
    if (file.exists(f)) return(f)
  }
  stop(paste0("adsl.xpt not found. Tried: ", paste(tried, collapse=" | ")))
}

adsl_file <- find_adsl()
cat("Reading:", adsl_file, "\n")

adsl_raw <- haven::read_xpt(adsl_file)
names(adsl_raw) <- toupper(names(adsl_raw))
stopifnot(all(c("USUBJID","TRT01AN","TRT01A","DCREASCD","ITTFL") %in% names(adsl_raw)))

adsl <- as.data.frame(adsl_raw, stringsAsFactors = FALSE)

to_char <- function(v) {
  if (inherits(v, "labelled")) v <- haven::as_character(v)
  as.character(v)
}
adsl$USUBJID <- to_char(adsl$USUBJID)
adsl$TRT01A <- to_char(adsl$TRT01A)
adsl$DCREASCD <- to_char(adsl$DCREASCD)
adsl$ITTFL <- to_char(adsl$ITTFL)
adsl$TRT01AN <- as.numeric(adsl$TRT01AN)

# WHERE ittfl = "Y" (case-sensitive, exact); missing excluded
adsl <- adsl[!is.na(adsl$ITTFL) & adsl$ITTFL == "Y", , drop = FALSE]
adsl <- adsl[, c("USUBJID","TRT01AN","TRT01A","DCREASCD","ITTFL"), drop = FALSE]

adsl$USUBJID_T <- trimws(ifelse(is.na(adsl$USUBJID), "", adsl$USUBJID), which = "both")
adsl$TRT01A_T <- trimws(ifelse(is.na(adsl$TRT01A), "", adsl$TRT01A), which = "both")
adsl$DCREASCD_T <- trimws(ifelse(is.na(adsl$DCREASCD), "", adsl$DCREASCD), which = "both")
adsl$UP <- toupper(adsl$DCREASCD_T)
adsl$USUBJID <- adsl$USUBJID_T
adsl$TRT01A <- adsl$TRT01A_T

# ---- Create total (adsl1) ----
adsl_tot_extra <- adsl
adsl_tot_extra$TRT01AN <- 999
adsl_tot_extra$TRT01A <- "Total"
adsl1 <- bind_rows(adsl, adsl_tot_extra)

# ---- Header N Count : tot ----
tot <- adsl1 %>%
  group_by(TRT01AN) %>%
  summarise(tot = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRT01AN)
ntrt <- n_distinct(adsl1$TRT01AN)

dummy <- data.frame(
  col1 = c("Completed the study","Discontinued"," ",
           "  Death","  Adverse Event","  Lack of Efficacy","  Lost to Follow-up",
           "  Non-compliance with Study Drug","  Pregnancy","  Protocal Violation",
           "  Physician Decision","  Withdrawl by Subject","  Withdrawl by Parent/Guardian",
           "  Recovery","  Technical Problems","  Other"),
  ord = 1:16,
  stringsAsFactors = FALSE
)

make_disp <- function(filter_completed = TRUE, use_dcreascd = FALSE, const_label = NULL) {
  if (use_dcreascd) {
    sub <- adsl1[adsl1$UP != "COMPLETED", , drop = FALSE]
    stopifnot(all(c("USUBJID","TRT01AN","TRT01A","DCREASCD_T") %in% names(sub)))
    sub$COL1 <- sub$DCREASCD_T
  } else if (filter_completed) {
    sub <- adsl1[adsl1$UP == "COMPLETED", , drop = FALSE]
    sub$COL1 <- const_label
  } else {
    sub <- adsl1[adsl1$UP != "COMPLETED", , drop = FALSE]
    sub$COL1 <- const_label
  }
  g <- sub %>% group_by(TRT01AN, TRT01A, COL1) %>%
    summarise(cnt = n_distinct(USUBJID), .groups = "drop")
  tot_j <- tot[, c("TRT01AN","tot"), drop = FALSE]
  g <- left_join(g, tot_j, by = "TRT01AN")
  stopifnot("tot" %in% names(g))
  stopifnot("TRT01AN" %in% names(g))
  g$per <- (g$cnt * 100) / g$tot
  g <- g[, c("cnt","per","COL1","TRT01AN","TRT01A"), drop = FALSE]
  names(g) <- c("cnt","per","col1","trt01an","trt01a")
  g <- g[order(g$trt01an, g$trt01a, g$col1, method = "radix"), , drop = FALSE]
  rownames(g) <- NULL
  g
}

comp <- make_disp(filter_completed = TRUE, use_dcreascd = FALSE, const_label = "Completed the study")
disc <- make_disp(filter_completed = FALSE, use_dcreascd = FALSE, const_label = "Discontinued")
alld <- make_disp(use_dcreascd = TRUE)

# prefix two blanks for rows from `all` : col1 = "  " || trim(left(col1))
alld$col1 <- paste0("  ", trimws(alld$col1, which = "both"))

add_nper <- function(df) {
  df$nper <- paste0(sas_put_cnt3(df$cnt), "(", sas_put_per51(df$per), ")")
  df
}
comp <- add_nper(comp)
disc <- add_nper(disc)
alld <- add_nper(alld)

alld <- alld[order(alld$trt01an, alld$trt01a, alld$col1, method = "radix"), , drop = FALSE]
comp <- comp[order(comp$trt01an, comp$trt01a, method = "radix"), , drop = FALSE]
disc <- disc[order(disc$trt01an, disc$trt01a, method = "radix"), , drop = FALSE]

trt_order <- sort(unique(c(comp$trt01an, disc$trt01an, alld$trt01an)))
out_list <- lapply(trt_order, function(t) {
  c1 <- comp[comp$trt01an == t, , drop = FALSE]
  c2 <- disc[disc$trt01an == t, , drop = FALSE]
  c3 <- alld[alld$trt01an == t, , drop = FALSE]
  bind_rows(c1, c2, c3)
})
disp <- bind_rows(out_list)
rownames(disp) <- NULL

# SAS: data outds.disp; set comp disc all; -> length of col1 is set by first
# dataset (comp) where col1 = "Completed the study" (length 19). Longer
# values from `all` ("  " || dcreascd) are truncated to 19 chars, e.g.
# "  Physician Decision" -> "  Physician Decisio".
disp$col1 <- substr(disp$col1, 1, 19)

disp <- disp[, c("cnt","per","col1","trt01an","trt01a","nper"), drop = FALSE]

cat("disp rows:", nrow(disp), "\n")

write.csv(disp, file.path(out, "disp.csv"), row.names = FALSE, na = "")

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-12) / 10^digits)
}
sas_put_3 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("   ")
    r <- round_half_up(v, 0)
    sprintf("%3.0f", r)
  }, USE.NAMES = FALSE)
}
sas_put_5.1 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("     ")
    r <- round_half_up(v, 1)
    sprintf("%5.1f", r)
  }, USE.NAMES = FALSE)
}
sas_sort_key <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    ints <- utf8ToInt(v)
    paste(sprintf("%03d", ints), collapse = "")
  }, USE.NAMES = FALSE)
}

raw_path <- "/bench/data/raw/cdisc-pilot/adam/adsl.xpt"
alt1 <- "/bench/data/derived/cdisc-pilot/adam/adsl.sas7bdat"
alt2 <- "/bench/data/derived/cdisc-pilot/adam/adsl.xpt"
in_path <- raw_path
if (!file.exists(in_path)) {
  if (file.exists(alt1)) in_path <- alt1 else if (file.exists(alt2)) in_path <- alt2
}

if (grepl("\\.xpt$", in_path, ignore.case = TRUE)) {
  adsl_raw <- haven::read_xpt(in_path)
} else {
  adsl_raw <- haven::read_sas(in_path)
}
adsl_raw <- haven::zap_labels(adsl_raw)
adsl_raw <- haven::zap_formats(adsl_raw)
adsl_raw <- as.data.frame(adsl_raw, stringsAsFactors = FALSE)
names(adsl_raw) <- toupper(names(adsl_raw))
stopifnot(all(c("USUBJID","TRT01AN","TRT01A","DCREASCD","ITTFL") %in% names(adsl_raw)))

adsl <- adsl_raw
adsl$USUBJID <- as.character(adsl$USUBJID)
adsl$TRT01A <- as.character(adsl$TRT01A)
adsl$DCREASCD <- as.character(adsl$DCREASCD)
adsl$ITTFL <- as.character(adsl$ITTFL)
adsl$TRT01AN <- as.numeric(adsl$TRT01AN)

# where ittfl = "Y" (case-sensitive, SAS)
adsl <- adsl[!is.na(adsl$ITTFL) & adsl$ITTFL == "Y", , drop = FALSE]
adsl <- adsl[, c("USUBJID","TRT01AN","TRT01A","DCREASCD"), drop = FALSE]

# Create total : output + output with trt01an=999 trt01a="Total"
adsl_total <- adsl
adsl_total$TRT01AN <- 999
adsl_total$TRT01A <- "Total"
adsl1 <- rbind(adsl, adsl_total)

# Header N Count : tot
tot <- aggregate(USUBJID ~ TRT01AN, data = adsl1,
  FUN = function(x) dplyr::n_distinct(x))
names(tot)[names(tot) == "USUBJID"] <- "TOT"
tot <- tot[order(is.na(tot$TRT01AN), tot$TRT01AN), , drop = FALSE]

# helper to emulate %disp macro
make_disp <- function(filter_idx, col1_vals) {
  a <- adsl1[filter_idx, , drop = FALSE]
  a$COL1 <- col1_vals[filter_idx]
  # join tot
  a2 <- merge(a, tot, by = "TRT01AN", all.x = TRUE, sort = FALSE)
  stopifnot("TOT" %in% names(a2))
  # group by TRT01AN, TRT01A, TOT, COL1 ; count distinct USUBJID
  # use data.table-style aggregate via dplyr
  df <- as.data.frame(a2, stringsAsFactors = FALSE)
  # split by groups
  grp <- interaction(df$TRT01AN, df$TRT01A, df$TOT, df$COL1, drop = TRUE)
  # to preserve SAS binary order for COL1, sort unique keys later
  spl <- split(df, grp)
  res_list <- lapply(spl, function(d) {
    data.frame(
      TRT01AN = d$TRT01AN[1],
      TRT01A = d$TRT01A[1],
      TOT = d$TOT[1],
      COL1 = d$COL1[1],
      CNT = dplyr::n_distinct(d$USUBJID),
      stringsAsFactors = FALSE
    )
  })
  res <- do.call(rbind, res_list)
  res$PER <- res$CNT * 100 / res$TOT
  rownames(res) <- NULL
  res
}

up <- toupper(ifelse(is.na(adsl1$DCREASCD), "", adsl1$DCREASCD))
is_comp <- !is.na(up) & up == "COMPLETED"
is_disc <- !( !is.na(up) & up == "COMPLETED" )

comp <- make_disp(is_comp, rep("Completed the study", nrow(adsl1)))
disc <- make_disp(is_disc, rep("Discontinued", nrow(adsl1)))
alld <- make_disp(is_disc, ifelse(is.na(adsl1$DCREASCD), "", adsl1$DCREASCD))

# order each by TRT01AN (missing first) to mimic sorted input for BY
ord_num <- function(d) order(is.na(d$TRT01AN), d$TRT01AN)
comp <- comp[ord_num(comp), , drop = FALSE]
disc <- disc[ord_num(disc), , drop = FALSE]
# all ordered by TRT01AN then COL1 SAS binary order
alld$SK <- sas_sort_key(alld$COL1)
alld <- alld[order(is.na(alld$TRT01AN), alld$TRT01AN, alld$SK), , drop = FALSE]
alld$SK <- NULL

# data outds.disp; set comp disc all(in=a); by trt01an trt01a; if a then col1 = "  "||trim(left(col1));
comp$ORD <- 1L
disc$ORD <- 2L
alld$ORD <- 3L
# prefix col1 for all
alld$COL1 <- paste0("  ", trimws(alld$COL1, which = "both"))

combined <- rbind(comp, disc, alld)
combined$SK2 <- sas_sort_key(combined$COL1)
combined <- combined[order(is.na(combined$TRT01AN), combined$TRT01AN, combined$ORD, combined$SK2), , drop = FALSE]
combined$SK2 <- NULL
combined$ORD <- NULL
combined$TOT <- NULL

# nper = put(cnt,3.)||"("||put(per,5.1)||")"
combined$NPER <- paste0(sas_put_3(combined$CNT), "(", sas_put_5.1(combined$PER), ")")

# final columns: cnt, per, col1, trt01an, trt01a, nper
disp <- data.frame(
  cnt = combined$CNT,
  per = combined$PER,
  col1 = combined$COL1,
  trt01an = combined$TRT01AN,
  trt01a = combined$TRT01A,
  nper = combined$NPER,
  stringsAsFactors = FALSE
)

write.csv(disp, file.path(out, "disp.csv"), row.names = FALSE, na = "")

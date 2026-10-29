out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  mult <- 10 ^ digits
  res <- rep(NA_real_, length(x))
  idx <- !is.na(x)
  xx <- x[idx]
  sgn <- sign(xx)
  axx <- abs(xx)
  r <- floor(axx * mult + 0.5 + 1e-9) / mult
  r[sgn == 0] <- 0
  res[idx] <- sgn * r
  res[idx][sgn == 0] <- 0
  res
}

sas_put_3 <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("   ")
    sprintf("%3d", as.integer(v))
  }, character(1), USE.NAMES = FALSE)
}

sas_put_5_1 <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("     ")
    r <- round_half_up(v, 1)
    sprintf("%5.1f", r)
  }, character(1), USE.NAMES = FALSE)
}

message("Reading ADSL")

cand_paths <- c(
  "/bench/data/raw/cdisc-pilot/adam/adsl.xpt",
  "/bench/data/derived/cdisc-pilot/adam/adsl.xpt",
  "/bench/data/derived/cdisc-pilot/adam/adsl.sas7bdat"
)

adsl_path <- NULL
for (p in cand_paths) {
  if (file.exists(p)) { adsl_path <- p; break }
}
if (is.null(adsl_path)) adsl_path <- cand_paths[1]

if (grepl("\\.xpt$", adsl_path, ignore.case = TRUE)) {
  raw <- haven::read_xpt(adsl_path)
} else {
  raw <- haven::read_sas(adsl_path)
}
raw <- as.data.frame(raw, stringsAsFactors = FALSE)
for (nm in names(raw)) {
  x <- raw[[nm]]
  if (inherits(x, "haven_labelled") || inherits(x, "haven_labelled_spss")) {
    x <- haven::zap_labels(x)
    x <- haven::zap_formats(x)
    attr(x, "label") <- NULL
  }
  if (is.list(x)) x <- unlist(x)
  raw[[nm]] <- x
}
names(raw) <- tolower(names(raw))

adsl <- raw[raw$ittfl == "Y" & !is.na(raw$ittfl), , drop = FALSE]
for (v in c("usubjid","trt01an","trt01a","dcreascd","ittfl")) {
  if (!v %in% names(adsl)) adsl[[v]] <- NA
}
adsl <- adsl[, c("usubjid","trt01an","trt01a","dcreascd","ittfl"), drop = FALSE]
adsl$usubjid <- as.character(adsl$usubjid)
adsl$trt01a <- as.character(adsl$trt01a)
adsl$dcreascd <- as.character(adsl$dcreascd)
adsl$ittfl <- as.character(adsl$ittfl)
adsl$trt01an <- as.numeric(adsl$trt01an)

message("Creating ADSL1 with Total")

adsl_tot <- adsl
adsl_tot$trt01an <- 999
adsl_tot$trt01a <- "Total"
adsl1 <- rbind(adsl, adsl_tot)

tot <- aggregate(usubjid ~ trt01an, data = adsl1,
  FUN = function(z) length(unique(z[!is.na(z) & z != ""])))
names(tot)[names(tot) == "usubjid"] <- "tot"
tot <- tot[order(tot$trt01an, na.last = FALSE), , drop = FALSE]
rownames(tot) <- NULL

up_dcreascd <- function(v) {
  v2 <- ifelse(is.na(v), "", v)
  toupper(v2)
}
adsl1$._up <- up_dcreascd(adsl1$dcreascd)

make_disp <- function(filter_up_cond, col1_const = NULL, use_dcreascd = FALSE) {
  if (use_dcreascd) {
    base <- adsl1[filter_up_cond(adsl1$._up), , drop = FALSE]
    if (nrow(base) == 0) {
      return(data.frame(cnt = numeric(0), per = numeric(0), col1 = character(0),
        trt01an = numeric(0), trt01a = character(0), stringsAsFactors = FALSE))
    }
    base$col1 <- ifelse(is.na(base$dcreascd), "", base$dcreascd)
    grp_keys <- unique(base[, c("trt01an","trt01a","col1"), drop = FALSE])
    res_list <- vector("list", nrow(grp_keys))
    for (i in seq_len(nrow(grp_keys))) {
      ta_n <- grp_keys$trt01an[i]
      ta_c <- grp_keys$trt01a[i]
      c1 <- grp_keys$col1[i]
      sel <- rep(TRUE, nrow(base))
      if (is.na(ta_n)) sel <- sel & is.na(base$trt01an) else sel <- sel & (!is.na(base$trt01an) & base$trt01an == ta_n)
      ta_c2 <- ifelse(is.na(ta_c), "", ta_c)
      base_c2 <- ifelse(is.na(base$trt01a), "", base$trt01a)
      sel <- sel & (base_c2 == ta_c2)
      c12 <- ifelse(is.na(c1), "", c1)
      base_c12 <- ifelse(is.na(base$col1), "", base$col1)
      sel <- sel & (base_c12 == c12)
      sub <- base[sel, , drop = FALSE]
      cnt <- length(unique(sub$usubjid[!is.na(sub$usubjid)]))
      if (is.na(ta_n)) {
        tot_val <- tot$tot[is.na(tot$trt01an)]
        if (length(tot_val) == 0) tot_val <- NA_real_
      } else {
        tot_val <- tot$tot[!is.na(tot$trt01an) & tot$trt01an == ta_n]
        if (length(tot_val) == 0) tot_val <- NA_real_
      }
      per <- cnt * 100 / tot_val
      res_list[[i]] <- data.frame(cnt = cnt, per = per, col1 = c1,
        trt01an = ta_n, trt01a = ta_c, stringsAsFactors = FALSE)
    }
    res <- do.call(rbind, res_list)
    ascii_key <- vapply(res$col1, function(s) {
      if (is.na(s)) s <- ""
      paste(sprintf("%05d", utf8ToInt(s)), collapse = "-")
    }, character(1), USE.NAMES = FALSE)
    ord <- order(ifelse(is.na(res$trt01an), -Inf, res$trt01an), ascii_key, na.last = FALSE)
    res <- res[ord, , drop = FALSE]
    rownames(res) <- NULL
    return(res)
  } else {
    base <- adsl1[filter_up_cond(adsl1$._up), , drop = FALSE]
    if (nrow(base) == 0) {
      return(data.frame(cnt = numeric(0), per = numeric(0), col1 = character(0),
        trt01an = numeric(0), trt01a = character(0), stringsAsFactors = FALSE))
    }
    grp_keys <- unique(base[, c("trt01an","trt01a"), drop = FALSE])
    grp_keys <- grp_keys[order(ifelse(is.na(grp_keys$trt01an), -Inf, grp_keys$trt01an), na.last = FALSE), , drop = FALSE]
    res_list <- vector("list", nrow(grp_keys))
    for (i in seq_len(nrow(grp_keys))) {
      ta_n <- grp_keys$trt01an[i]
      ta_c <- grp_keys$trt01a[i]
      sel <- rep(TRUE, nrow(base))
      if (is.na(ta_n)) sel <- sel & is.na(base$trt01an) else sel <- sel & (!is.na(base$trt01an) & base$trt01an == ta_n)
      ta_c2 <- ifelse(is.na(ta_c), "", ta_c)
      base_c2 <- ifelse(is.na(base$trt01a), "", base$trt01a)
      sel <- sel & (base_c2 == ta_c2)
      sub <- base[sel, , drop = FALSE]
      cnt <- length(unique(sub$usubjid[!is.na(sub$usubjid)]))
      if (is.na(ta_n)) {
        tot_val <- tot$tot[is.na(tot$trt01an)]
        if (length(tot_val) == 0) tot_val <- NA_real_
      } else {
        tot_val <- tot$tot[!is.na(tot$trt01an) & tot$trt01an == ta_n]
        if (length(tot_val) == 0) tot_val <- NA_real_
      }
      per <- cnt * 100 / tot_val
      res_list[[i]] <- data.frame(cnt = cnt, per = per, col1 = col1_const,
        trt01an = ta_n, trt01a = ta_c, stringsAsFactors = FALSE)
    }
    res <- do.call(rbind, res_list)
    ord <- order(ifelse(is.na(res$trt01an), -Inf, res$trt01an), na.last = FALSE)
    res <- res[ord, , drop = FALSE]
    rownames(res) <- NULL
    return(res)
  }
}

message("Creating comp/disc/all")

comp <- make_disp(function(u) !is.na(u) & u == "COMPLETED", col1_const = "Completed the study", use_dcreascd = FALSE)
disc <- make_disp(function(u) !is.na(u) & u != "COMPLETED", col1_const = "Discontinued", use_dcreascd = FALSE)
alld <- make_disp(function(u) !is.na(u) & u != "COMPLETED", use_dcreascd = TRUE)

# SAS: data disp; set comp disc all(in=a); by trt01an trt01a;
# if a then col1 = "  " || trim(left(col1));
# COL1 length is set by first dataset COMP where literal "Completed the study" has length 19,
# so prefixed values are truncated on the right to 19 chars (e.g. '  Physician Decisio').
if (nrow(alld) > 0) {
  stripped <- trimws(ifelse(is.na(alld$col1), "", alld$col1), which = "both")
  prefixed <- paste0("  ", stripped)
  # SAS truncation to LENGTH of COL1 = 19
  alld$col1 <- substr(prefixed, 1, 19)
}
disp <- rbind(comp, disc, alld)
disp$nper <- paste0(sas_put_3(disp$cnt), "(", sas_put_5_1(disp$per), ")")
disp <- disp[, c("cnt","per","col1","trt01an","trt01a","nper"), drop = FALSE]
rownames(disp) <- NULL

message(paste0("Writing disp rows=", nrow(disp)))

write.csv(disp, file.path(out, "disp.csv"), row.names = FALSE, na = "")

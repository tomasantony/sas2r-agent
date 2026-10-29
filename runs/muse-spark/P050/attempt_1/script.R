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
  # small epsilon to counter binary floating error, SAS rounds half away from zero
  r <- floor(axx * mult + 0.5 + 1e-9) / mult
  # preserve sign, SAS: -0 -> 0? keep 0
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
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adsl.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adsl.sas7bdat"
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
# strip haven labels/formats, keep values
raw <- as.data.frame(raw, stringsAsFactors = FALSE)
for (nm in names(raw)) {
  x <- raw[[nm]]
  if (inherits(x, "haven_labelled") || inherits(x, "haven_labelled_spss")) {
    x <- haven::zap_labels(x)
    x <- haven::zap_formats(x)
    # zap_label attribute
    attr(x, "label") <- NULL
  }
  # convert labelled character remnants to plain
  if (is.list(x)) x <- unlist(x)
  raw[[nm]] <- x
}
names(raw) <- tolower(names(raw))

# data adsl(keep=usubjid trt01an trt01a dcreascd ittfl); where ittfl="Y";
adsl <- raw[raw$ittfl == "Y" & !is.na(raw$ittfl), , drop = FALSE]
# keep vars (if missing, create as NA)
for (v in c("usubjid","trt01an","trt01a","dcreascd","ittfl")) {
  if (!v %in% names(adsl)) adsl[[v]] <- NA
}
adsl <- adsl[, c("usubjid","trt01an","trt01a","dcreascd","ittfl"), drop = FALSE]
# ensure types: usubjid/trt01a/dcreascd/ittfl character, trt01an numeric
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

# Header N Count: tot table
# proc sql: select trt01an, count(distinct usubjid) as tot ... group by trt01an order by trt01an
tot <- aggregate(usubjid ~ trt01an, data = adsl1,
  FUN = function(z) length(unique(z[!is.na(z) & z != ""])))
# aggregate drops NA groups; handle missing trt01an if any (SAS missing sorts first)
names(tot)[names(tot) == "usubjid"] <- "tot"
tot <- tot[order(tot$trt01an, na.last = FALSE), , drop = FALSE]
rownames(tot) <- NULL

# helper upcase with SAS missing ("") semantics
up_dcreascd <- function(v) {
  v2 <- ifelse(is.na(v), "", v)
  toupper(v2)
}
adsl1$._up <- up_dcreascd(adsl1$dcreascd)

# macro disp
make_disp <- function(filter_up_cond, col1_const = NULL, use_dcreascd = FALSE) {
  if (use_dcreascd) {
    base <- adsl1[filter_up_cond(adsl1$._up), , drop = FALSE]
    if (nrow(base) == 0) {
      return(data.frame(cnt = numeric(0), per = numeric(0), col1 = character(0),
        trt01an = numeric(0), trt01a = character(0), stringsAsFactors = FALSE))
    }
    base$col1 <- ifelse(is.na(base$dcreascd), "", base$dcreascd)
    # group by trt01an, trt01a, tot, col1 ; need tot for per
    # distinct usubjid per group
    grp_keys <- unique(base[, c("trt01an","trt01a","col1"), drop = FALSE])
    # SAS group ordering: by trt01an, trt01a, tot, col1 with SAS char order (uppercase first = ASCII)
    # sort by trt01an then col1 ASCII
    # ASCII order via ordering raw bytes: use order on trt01an then col1 with radix-like binary
    # implement binary order by transforming string to code points
    res_list <- vector("list", nrow(grp_keys))
    for (i in seq_len(nrow(grp_keys))) {
      ta_n <- grp_keys$trt01an[i]
      ta_c <- grp_keys$trt01a[i]
      c1 <- grp_keys$col1[i]
      # match rows: handle NA in trt01an/trt01a/col1 (SAS missing sorts first, "" for char)
      sel <- rep(TRUE, nrow(base))
      if (is.na(ta_n)) sel <- sel & is.na(base$trt01an) else sel <- sel & (!is.na(base$trt01an) & base$trt01an == ta_n)
      # trt01a compare: NA vs value; SAS missing = "" ; treat NA as ""
      ta_c2 <- ifelse(is.na(ta_c), "", ta_c)
      base_c2 <- ifelse(is.na(base$trt01a), "", base$trt01a)
      sel <- sel & (base_c2 == ta_c2)
      c12 <- ifelse(is.na(c1), "", c1)
      base_c12 <- ifelse(is.na(base$col1), "", base$col1)
      sel <- sel & (base_c12 == c12)
      sub <- base[sel, , drop = FALSE]
      cnt <- length(unique(sub$usubjid[!is.na(sub$usubjid)]))
      # tot lookup
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
    # order by trt01an then col1 (SAS binary/ASCII: uppercase before lowercase)
    # create ASCII sort key: paste code points
    ascii_key <- vapply(res$col1, function(s) {
      if (is.na(s)) s <- ""
      paste(sprintf("%05d", utf8ToInt(s)), collapse = "-")
    }, character(1), USE.NAMES = FALSE)
    # missing trt01an first
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
# Note: ._up never NA (converted ""), so above covers all; equivalent to SAS ^= which includes missing
alld <- make_disp(function(u) !is.na(u) & u != "COMPLETED", use_dcreascd = TRUE)

# data outds.disp; set comp disc all(in=a); by trt01an trt01a; if a then col1="  "||trim(left(col1)); nper=...;
# prefix only rows from `all`
if (nrow(alld) > 0) {
  alld$col1 <- paste0("  ", trimws(ifelse(is.na(alld$col1), "", alld$col1), which = "both"))
}
disp <- rbind(comp, disc, alld)
# nper = put(cnt,3.) || "(" || put(per,5.1) || ")"
disp$nper <- paste0(sas_put_3(disp$cnt), "(", sas_put_5_1(disp$per), ")")
disp <- disp[, c("cnt","per","col1","trt01an","trt01a","nper"), drop = FALSE]
rownames(disp) <- NULL

message(paste0("Writing disp rows=", nrow(disp)))

write.csv(disp, file.path(out, "disp.csv"), row.names = FALSE, na = "")

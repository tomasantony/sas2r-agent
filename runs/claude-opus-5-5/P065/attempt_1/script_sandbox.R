suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sdtm_dir <- "/bench/data/derived/wxzhai/sdtm"
adam_dir <- "/bench/data/derived/wxzhai/adam"

read_ds <- function(dir, name) {
  f1 <- file.path(dir, paste0(name, ".sas7bdat"))
  f2 <- file.path(dir, paste0(name, ".xpt"))
  if (file.exists(f1)) return(haven::read_sas(f1))
  if (file.exists(f2)) return(haven::read_xpt(f2))
  ff <- list.files(dir, full.names = TRUE)
  hit <- ff[tolower(tools::file_path_sans_ext(basename(ff))) == tolower(name)]
  if (length(hit) == 0) stop("Dataset not found: ", name, " in ", dir)
  if (grepl("\\.xpt$", hit[1], ignore.case = TRUE)) haven::read_xpt(hit[1]) else haven::read_sas(hit[1])
}

strip <- function(df) {
  df <- haven::zap_labels(haven::zap_formats(df))
  df[] <- lapply(df, function(x) { attr(x, "label") <- NULL; x })
  as.data.frame(df, stringsAsFactors = FALSE)
}

lb   <- strip(read_ds(sdtm_dir, "lb"))
adsl <- strip(read_ds(adam_dir, "adsl"))

# case-insensitive column lookup
getcol <- function(df, nm) names(df)[toupper(names(df)) == toupper(nm)][1]
ch <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }

u_lb <- getcol(lb, "USUBJID"); p_lb <- getcol(lb, "PARAM"); d_lb <- getcol(lb, "LBDTC")
lb[[u_lb]] <- ch(lb[[u_lb]]); lb[[p_lb]] <- ch(lb[[p_lb]]); lb[[d_lb]] <- ch(lb[[d_lb]])

# PROC SORT by USUBJID PARAM LBDTC (stable, C-locale ordering)
lb_qc <- lb[order(lb[[u_lb]], lb[[p_lb]], lb[[d_lb]], method = "radix"), , drop = FALSE]

u_ad <- getcol(adsl, "USUBJID")
adsl[[u_ad]] <- ch(adsl[[u_ad]])
adsl_qc <- adsl[order(adsl[[u_ad]], method = "radix"), , drop = FALSE]
adsl_qc <- adsl_qc[, c(u_ad, getcol(adsl, "RFSTDTC"), getcol(adsl, "TRT01P")), drop = FALSE]
names(adsl_qc) <- c(u_lb, "RFSTDTC", "TRT01P")
adsl_qc$RFSTDTC <- ch(adsl_qc$RFSTDTC)
adsl_qc$TRT01P  <- ch(adsl_qc$TRT01P)
adsl_qc <- adsl_qc[!duplicated(adsl_qc[[u_lb]]), , drop = FALSE]

# drop overlapping non-key variables from lb (adsl values overwrite in merge)
ov <- setdiff(intersect(toupper(names(lb_qc)), c("RFSTDTC", "TRT01P")), character(0))
if (length(ov)) lb_qc <- lb_qc[, !(toupper(names(lb_qc)) %in% ov), drop = FALSE]

# merge, if a and b
idx <- match(lb_qc[[u_lb]], adsl_qc[[u_lb]])
keep <- !is.na(idx)
dat <- lb_qc[keep, , drop = FALSE]
dat$RFSTDTC <- adsl_qc$RFSTDTC[idx[keep]]
dat$TRT01P  <- adsl_qc$TRT01P[idx[keep]]

# input(x, yymmdd10.)
sas_date <- function(x) {
  s <- substr(ch(x), 1, 10)
  ok <- grepl("^\\d{4}-\\d{2}-\\d{2}$", s)
  d <- rep(as.Date(NA), length(s))
  d[ok] <- as.Date(s[ok], format = "%Y-%m-%d")
  as.numeric(d)
}
# SAS less-than: missing is smallest value
sas_lt <- function(a, b) {
  a2 <- ifelse(is.na(a), -Inf, a); b2 <- ifelse(is.na(b), -Inf, b)
  # . < . is false; . < number true
  ifelse(is.na(a) & is.na(b), FALSE, a2 < b2)
}
sas_eq <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)

a_col <- getcol(dat, "AVAL")
aval <- as.numeric(dat[[a_col]])
lbdt <- sas_date(dat[[d_lb]])
rfstdt <- sas_date(dat$RFSTDTC)

n <- nrow(dat)
BASE <- rep(NA_real_, n); ABLFL <- rep("", n); CHG <- rep(NA_real_, n)
base_r <- NA_real_
# Note: first.PARAM is not defined (PARAM not in BY) -> uninitialized/missing -> never resets BASE
for (i in seq_len(n)) {
  pre <- sas_lt(lbdt[i], rfstdt[i])
  if (pre && !is.na(aval[i])) base_r <- aval[i]
  if (pre && sas_eq(aval[i], base_r)) ABLFL[i] <- "Y" else ABLFL[i] <- ""
  if (!is.na(base_r)) CHG[i] <- aval[i] - base_r
  BASE[i] <- base_r
}
dat$BASE <- BASE
dat$ABLFL <- ABLFL
dat$CHG <- CHG

write.csv(dat, file.path(out, "adlb_qc.csv"), row.names = FALSE, na = "")
cat("adlb_qc written:", nrow(dat), "rows\n")

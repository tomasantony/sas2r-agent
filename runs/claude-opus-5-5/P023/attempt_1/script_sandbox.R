# P023 - SDTM DM from faijal-oncology raw EDC/IVRS data
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
rawdir <- "/bench/data/raw/faijal-oncology/raw_data"

rd <- function(nm) {
  df <- as.data.frame(read_excel(file.path(rawdir, paste0(nm, ".xlsx"))), stringsAsFactors = FALSE)
  if ("SUBJID" %in% names(df)) df$SUBJID <- trimws(as.character(df$SUBJID))
  df
}

# ---- SAS date11. informat (dd-MON-yyyy) ----
mon_map <- c(JAN = 1, FEB = 2, MAR = 3, APR = 4, MAY = 5, JUN = 6,
             JUL = 7, AUG = 8, SEP = 9, OCT = 10, NOV = 11, DEC = 12)
in_date11 <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(format(x, "%Y-%m-%d")))
  x <- toupper(gsub("[-/ ]", "", trimws(as.character(x))))
  m <- regmatches(x, regexec("^([0-9]{1,2})([A-Z]{3})([0-9]{2,4})$", x))
  vapply(m, function(p) {
    if (length(p) < 4) return(NA_real_)
    mo <- mon_map[p[3]]
    if (is.na(mo)) return(NA_real_)
    yr <- as.integer(p[4])
    if (nchar(p[4]) == 2) yr <- ifelse(yr < 26, 2000 + yr, 1900 + yr)
    d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yr, mo, as.integer(p[2]))))
    as.numeric(d)
  }, numeric(1)) |> as.Date(origin = "1970-01-01")
}
put_ymd <- function(d) ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }

edc_dm    <- rd("edc_dm")
ivrs_rand <- rd("ivrs_rand")
edc_ex    <- rd("edc_ex")
edc_ds    <- rd("edc_ds")
edc_death <- rd("edc_death")

# ---- STEP 1 ----
ds_enrol <- data.frame(SUBJID = ivrs_rand$SUBJID, enr_dt = in_date11(ivrs_rand$RAND_DT),
                       stringsAsFactors = FALSE)

ex_dates <- edc_ex %>%
  mutate(.st = in_date11(STDT), .en = in_date11(ENDT)) %>%
  group_by(SUBJID) %>%
  summarise(f_dose = if (all(is.na(.st))) as.Date(NA) else min(.st, na.rm = TRUE),
            l_dose = if (all(is.na(.en))) as.Date(NA) else max(.en, na.rm = TRUE),
            .groups = "drop") %>% as.data.frame()

ds_final <- data.frame(SUBJID = edc_ds$SUBJID,
                       end_dt = in_date11(edc_ds$DS_DT),
                       dth_fl = ifelse(!is.na(edc_ds$DS_TERM) & edc_ds$DS_TERM == "DEATH", "Y", "N"),
                       stringsAsFactors = FALSE)

# ---- STEP 2: sort (stable, C-locale) ----
srt <- function(df) df[order(df$SUBJID, method = "radix"), , drop = FALSE]
edc_dm <- srt(edc_dm); ivrs_k <- srt(ivrs_rand[, c("SUBJID", "TRT_CODE")])
ds_enrol <- srt(ds_enrol); ex_dates <- srt(ex_dates); ds_final <- srt(ds_final)
edc_death <- srt(edc_death)
names(edc_death)[names(edc_death) == "DTH_DT"] <- "death_date_raw"

# ---- SAS-style match merge ----
sas_merge <- function(dfs, inflag = 1) {
  keys <- sort(unique(unlist(lapply(dfs, function(d) d$SUBJID))), method = "radix")
  rows <- list()
  for (k in keys) {
    subs <- lapply(dfs, function(d) d[d$SUBJID == k, , drop = FALSE])
    if (nrow(subs[[inflag]]) == 0) next
    n <- max(sapply(subs, nrow))
    pdv <- list(SUBJID = k)
    for (i in seq_len(n)) {
      for (s in subs) {
        if (i <= nrow(s)) for (v in setdiff(names(s), "SUBJID")) pdv[[v]] <- s[[v]][i]
      }
      rows[[length(rows) + 1]] <- pdv
    }
  }
  rows
}
mrows <- sas_merge(list(edc_dm, ivrs_k, ds_enrol, ex_dates, ds_final, edc_death))

g <- function(r, v, date = FALSE) {
  x <- r[[v]]
  if (is.null(x) || length(x) == 0) return(if (date) as.Date(NA) else NA)
  x
}

age_c <- function(b, e) {
  if (is.na(b) || is.na(e)) return(NA_real_)
  y <- as.integer(format(e, "%Y")) - as.integer(format(b, "%Y"))
  if (format(e, "%m%d") < format(b, "%m%d")) y <- y - 1
  y
}

res <- lapply(mrows, function(r) {
  STUDYID <- "ONCO-2026"
  SUBJID <- substr(chr(r$SUBJID), 1, 10)
  bd <- in_date11(g(r, "BRTHDT")); ic <- in_date11(g(r, "RFICDT"))
  BRTHDTC <- put_ymd(bd); RFICDTC <- put_ymd(ic)
  ARMCD <- substr(chr(g(r, "TRT_CODE")), 1, 10)
  ARM <- if (ARMCD == "DRUGA") "Experimental Drug A" else if (ARMCD == "PBO") "Placebo" else ""
  ddr <- chr(g(r, "death_date_raw"))
  DTHDTC <- if (!is.null(r$death_date_raw) && !is.na(r$death_date_raw) && ddr != "")
    put_ymd(in_date11(r$death_date_raw)) else ""
  ed <- g(r, "end_dt", TRUE)
  RFENDTC <- put_ymd(as.Date(ed))
  data.frame(
    STUDYID = STUDYID, DOMAIN = "DM",
    USUBJID = substr(paste(c(STUDYID, SUBJID)[c(STUDYID, SUBJID) != ""], collapse = "-"), 1, 25),
    SUBJID = SUBJID, SITEID = substr(chr(g(r, "SITE")), 1, 10),
    RFSTDTC = put_ymd(as.Date(g(r, "enr_dt", TRUE))), RFENDTC = RFENDTC,
    RFXSTDTC = put_ymd(as.Date(g(r, "f_dose", TRUE))),
    RFXENDTC = put_ymd(as.Date(g(r, "l_dose", TRUE))),
    RFICDTC = RFICDTC, RFPENDTC = RFENDTC, BRTHDTC = BRTHDTC,
    AGE = age_c(bd, ic), AGEU = "YEARS",
    SEX = substr(chr(g(r, "GENDER")), 1, 1),
    RACE = substr(chr(g(r, "RACE")), 1, 40), ETHNIC = substr(chr(g(r, "ETHNIC")), 1, 40),
    ARMCD = ARMCD, ARM = ARM, ACTARMCD = ARMCD, ACTARM = ARM,
    COUNTRY = "IND", DTHFL = substr(chr(g(r, "dth_fl")), 1, 1), DTHDTC = DTHDTC,
    stringsAsFactors = FALSE)
})
sdtm_dm <- do.call(rbind, res)

write.csv(sdtm_dm, file.path(out, "sdtm_dm.csv"), row.names = FALSE, na = "")

# ---- STEP 4: PROC PRINT rendering ----
pr <- cbind(Obs = seq_len(nrow(sdtm_dm)), sdtm_dm)
write.csv(pr, file.path(out, "P023.csv"), row.names = FALSE, na = "")
disp <- as.data.frame(lapply(pr, function(x) {
  if (is.numeric(x)) ifelse(is.na(x), ".", format(x, trim = TRUE)) else chr(x)
}), stringsAsFactors = FALSE)
w <- sapply(names(disp), function(n) max(nchar(n), nchar(disp[[n]])))
isnum <- sapply(pr, is.numeric)
fmt <- function(v, n) if (isnum[n]) formatC(v, width = w[n]) else formatC(v, width = w[n], flag = "-")
lines <- c("The SAS System", "",
           paste(sapply(names(disp), function(n) fmt(n, n)), collapse = "  "), "")
for (i in seq_len(nrow(disp)))
  lines <- c(lines, paste(sapply(names(disp), function(n) fmt(disp[[n]][i], n)), collapse = "  "))
writeLines(lines, file.path(out, "P023.txt"))
cat("P023 done:", nrow(sdtm_dm), "records\n")

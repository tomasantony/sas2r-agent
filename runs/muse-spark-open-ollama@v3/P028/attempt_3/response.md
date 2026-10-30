```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(openxlsx)

sas_catx <- function(sep, ...) {
  args <- list(...)
  vals <- lapply(args, function(v) as.character(v))
  n <- max(lengths(vals))
  vals <- lapply(vals, function(v) rep_len(v, n))
  res <- character(n)
  for (i in seq_len(n)) {
    parts <- sapply(vals, function(v) v[i])
    parts <- trimws(parts)
    parts <- parts[!is.na(parts) & parts != ""]
    res[i] <- paste(parts, collapse = sep)
  }
  res
}
sas_tranwrd_upcase <- function(x, from, to) {
  ifelse(is.na(x), NA_character_, toupper(gsub(from, to, x, fixed = TRUE)))
}
parse_date11 <- function(x) {
  x_trim <- trimws(as.character(x))
  res <- rep(as.Date(NA), length(x_trim))
  idx <- !is.na(x_trim) & x_trim != "" & x_trim != "NA"
  if (any(idx)) {
    for (i in which(idx)) {
      v <- x_trim[i]
      if (grepl("^[0-9]{4,5}(\\.[0-9]+)?$", v)) {
        num <- suppressWarnings(as.numeric(v))
        if (!is.na(num) && num > 30000 && num < 60000) {
          res[i] <- as.Date(num, origin = "1899-12-30")
          next
        }
      }
      m <- regmatches(v, regexec("^\\s*([0-9]{1,2})[- /]*([A-Za-z]{3})[- /]*([0-9]{2,4})\\s*$", v))[[1]]
      if (length(m) == 4) {
        d <- as.integer(m[2]); mon_str <- toupper(m[3]); y <- as.integer(m[4])
        if (y < 100) { if (y <= 19) y <- 2000 + y else y <- 1900 + y }
        mon_num <- match(mon_str, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
        if (!is.na(mon_num)) {
          dt_str <- sprintf("%04d-%02d-%02d", y, mon_num, d)
          res[i] <- suppressWarnings(as.Date(dt_str, format="%Y-%m-%d"))
        }
      } else {
        dt2 <- suppressWarnings(as.Date(v, format="%Y-%m-%d"))
        if (!is.na(dt2)) res[i] <- dt2 else {
          dt3 <- suppressWarnings(as.Date(v, format="%m/%d/%Y"))
          if (!is.na(dt3)) res[i] <- dt3 else {
            dt4 <- suppressWarnings(as.Date(v, format="%d/%m/%Y"))
            if (!is.na(dt4)) res[i] <- dt4
          }
        }
      }
    }
  }
  res
}
format_yymmdd10 <- function(d) {
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}
parse_any_to_date <- function(vec) {
  res <- parse_date11(vec)
  idx_na <- is.na(res) & trimws(as.character(vec)) != "" & !is.na(vec) & trimws(as.character(vec)) != "NA"
  if (any(idx_na)) {
    v2 <- trimws(as.character(vec[idx_na]))
    d2 <- suppressWarnings(as.Date(substr(v2,1,10), format="%Y-%m-%d"))
    res[idx_na] <- d2
    idx_na2 <- is.na(res) & trimws(as.character(vec)) != "" & !is.na(vec) & trimws(as.character(vec)) != "NA"
    if (any(idx_na2)) {
      v3 <- trimws(as.character(vec[idx_na2]))
      d3 <- suppressWarnings(as.Date(v3, format="%m/%d/%Y"))
      res[idx_na2] <- d3
    }
    idx_na3 <- is.na(res) & trimws(as.character(vec)) != "" & !is.na(vec) & trimws(as.character(vec)) != "NA"
    if (any(idx_na3)) {
      v4 <- trimws(as.character(vec[idx_na3]))
      nums <- suppressWarnings(as.numeric(v4))
      ok <- !is.na(nums) & nums > 30000 & nums < 60000
      if (any(ok, na.rm=TRUE)) {
        res[idx_na3][ok] <- as.Date(nums[ok], origin="1899-12-30")
      }
    }
  }
  res
}
find_col <- function(df, target) {
  idx <- which(toupper(names(df)) == toupper(target))
  if (length(idx)==0) return(NULL)
  names(df)[idx[1]]
}
find_col_like <- function(df, pattern) {
  idx <- grep(pattern, toupper(names(df)))
  if (length(idx)==0) return(NULL)
  names(df)[idx[1]]
}

raw_base <- "D:/meta_hackathon/sas2r-agent/data/raw"
edc_dir <- file.path(raw_base, "faijal-oncology/raw_data")
bench_root <- "/bench"
# also try linux style
if (!dir.exists(edc_dir) && dir.exists(file.path(bench_root, "data/raw/faijal-oncology/raw_data"))) {
  edc_dir <- file.path(bench_root, "data/raw/faijal-oncology/raw_data")
  raw_base <- file.path(bench_root, "data/raw")
}
xlsx_files <- list.files(edc_dir, pattern="\\.xlsx$", full.names=TRUE, ignore.case=TRUE)
read_edc <- function(path) {
  df <- readxl::read_excel(path, col_types="text", .name_repair="minimal")
  names(df) <- trimws(names(df))
  df <- df %>% mutate(across(everything(), ~trimws(as.character(.x))))
  df <- df %>% mutate(across(everything(), ~ifelse(is.na(.x), "", .x)))
  df
}
edc_list <- list()
for (f in xlsx_files) {
  ds_name <- tools::file_path_sans_ext(basename(f))
  edc_list[[ds_name]] <- read_edc(f)
}
edc_tr <- edc_list[["edc_tr"]]
edc_dm <- edc_list[["edc_dm"]]
edc_ex <- edc_list[["edc_ex"]]
ivrs_rand <- edc_list[["ivrs_rand"]]

# ---- Derive dm_for_tr (USUBJID, RFSTDTC) ----
sdtm_dm_file <- NULL
search_roots <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm",
  "D:/meta_hackathon/sas2r-agent/data/raw",
  "D:/meta_hackathon/sas2r-agent/data/derived",
  "D:/meta_hackathon/sas2r-agent/output",
  "/bench/data/raw/cdisc-pilot/sdtm",
  "/bench/data/raw",
  "/bench/data/derived",
  "/bench/output"
)
for (root in search_roots) {
  if (dir.exists(root)) {
    files <- list.files(root, pattern="(?i)(^dm\\.|sdtm_dm).*\\.(xpt|sas7bdat|xlsx|csv)$", recursive=TRUE, full.names=TRUE)
    if (length(files)>0) { sdtm_dm_file <- files[1]; break }
  }
}
dm_for_tr <- NULL
if (!is.null(sdtm_dm_file) && file.exists(sdtm_dm_file)) {
  if (grepl("\\.xpt$", sdtm_dm_file, ignore.case=TRUE)) sdtm_dm_raw <- haven::read_xpt(sdtm_dm_file)
  else if (grepl("\\.sas7bdat$", sdtm_dm_file, ignore.case=TRUE)) sdtm_dm_raw <- haven::read_sas(sdtm_dm_file)
  else if (grepl("\\.xlsx$", sdtm_dm_file, ignore.case=TRUE)) sdtm_dm_raw <- readxl::read_excel(sdtm_dm_file, col_types="text")
  else sdtm_dm_raw <- read.csv(sdtm_dm_file, stringsAsFactors=FALSE)
  names(sdtm_dm_raw) <- toupper(names(sdtm_dm_raw))
  if (all(c("USUBJID","RFSTDTC") %in% names(sdtm_dm_raw))) {
    dm_for_tr <- sdtm_dm_raw %>% select(USUBJID, RFSTDTC) %>%
      mutate(USUBJID=as.character(USUBJID), RFSTDTC=as.character(RFSTDTC)) %>% arrange(USUBJID)
  }
}
if (is.null(dm_for_tr)) {
  subj_col_dm <- find_col(edc_dm, "SUBJID")
  if (is.null(subj_col_dm)) subj_col_dm <- find_col_like(edc_dm, "SUBJID")
  rf_col_dm <- find_col(edc_dm, "RFSTDTC")
  if (is.null(rf_col_dm)) rf_col_dm <- find_col(edc_dm, "RFSTDT")
  if (is.null(rf_col_dm)) rf_col_dm <- find_col_like(edc_dm, "RFSTD")
  has_valid_rf <- FALSE
  if (!is.null(rf_col_dm)) {
    vals <- edc_dm[[rf_col_dm]]
    vals_trim <- trimws(vals)
    nonmiss <- sum(vals_trim != "" & vals_trim != "NA" & !is.na(vals_trim))
    if (nonmiss > 0) has_valid_rf <- TRUE
  }
  if (has_valid_rf) {
    dm_for_tr <- edc_dm %>%
      mutate(
        USUBJID = sas_catx("-", "ONCO-2026", .data[[subj_col_dm]]),
        RFSTDTC = format_yymmdd10(parse_any_to_date(.data[[rf_col_dm]])),
        RFSTDTC = ifelse(is.na(RFSTDTC), "", RFSTDTC)
      ) %>% select(USUBJID, RFSTDTC) %>% arrange(USUBJID)
  } else {
    subj_col_ex <- find_col(edc_ex, "SUBJID")
    if (is.null(subj_col_ex)) subj_col_ex <- find_col_like(edc_ex, "SUBJID")
    date_col_ex <- find_col(edc_ex, "EXSTDTC")
    if (is.null(date_col_ex)) date_col_ex <- find_col(edc_ex, "EXSTDT")
    if (is.null(date_col_ex)) date_col_ex <- find_col(edc_ex, "EXSTDAT")
    if (is.null(date_col_ex)) date_col_ex <- find_col(edc_ex, "EXDT")
    if (is.null(date_col_ex)) date_col_ex <- find_col_like(edc_ex, "EX.*DT")
    if (is.null(date_col_ex)) {
      cand <- names(edc_ex)[grepl("DT", toupper(names(edc_ex)))]
      if (length(cand)>0) date_col_ex <- cand[1]
    }
    if (!is.null(date_col_ex) && !is.null(subj_col_ex)) {
      tmp <- edc_ex %>%
        mutate(
          USUBJID = sas_catx("-", "ONCO-2026", .data[[subj_col_ex]]),
          exdt = parse_any_to_date(.data[[date_col_ex]])
        ) %>%
        filter(!is.na(exdt)) %>%
        group_by(USUBJID) %>%
        summarise(RFSTDTC = format_yymmdd10(min(exdt, na.rm=TRUE)), .groups="drop") %>%
        arrange(USUBJID)
      all_subj <- unique(c(
        sas_catx("-", "ONCO-2026", edc_tr[[find_col(edc_tr,"SUBJID")]]),
        sas_catx("-", "ONCO-2026", edc_dm[[subj_col_dm]])
      ))
      missing <- setdiff(all_subj, tmp$USUBJID)
      if (length(missing)>0) {
        tmp <- bind_rows(tmp, data.frame(USUBJID=missing, RFSTDTC="", stringsAsFactors=FALSE))
      }
      dm_for_tr <- tmp %>% arrange(USUBJID)
    } else {
      date_cands_dm <- names(edc_dm)[grepl("DT|DATE", toupper(names(edc_dm)))]
      if (length(date_cands_dm)>0) {
        dc <- date_cands_dm[1]
        dm_for_tr <- edc_dm %>%
          mutate(
            USUBJID = sas_catx("-", "ONCO-2026", .data[[subj_col_dm]]),
            RFSTDTC = format_yymmdd10(parse_any_to_date(.data[[dc]])),
            RFSTDTC = ifelse(is.na(RFSTDTC), "", RFSTDTC)
          ) %>% select(USUBJID, RFSTDTC) %>% arrange(USUBJID)
      } else {
        dm_for_tr <- edc_dm %>%
          mutate(USUBJID = sas_catx("-", "ONCO-2026", .data[[subj_col_dm]]), RFSTDTC="") %>%
          select(USUBJID, RFSTDTC) %>% arrange(USUBJID)
      }
    }
  }
  tr_subjs <- unique(sas_catx("-", "ONCO-2026", edc_tr[[find_col(edc_tr, "SUBJID")]]))
  miss2 <- setdiff(tr_subjs, dm_for_tr$USUBJID)
  if (length(miss2)>0) {
    dm_for_tr <- bind_rows(dm_for_tr, data.frame(USUBJID=miss2, RFSTDTC="", stringsAsFactors=FALSE)) %>% arrange(USUBJID)
  }
  need_fix <- dm_for_tr %>% filter(USUBJID %in% tr_subjs & (is.na(RFSTDTC) | RFSTDTC==""))
  if (nrow(need_fix)>0) {
    if (!is.null(ivrs_rand)) {
      subj_col_rand <- find_col(ivrs_rand, "SUBJID")
      if (is.null(subj_col_rand)) subj_col_rand <- find_col_like(ivrs_rand, "SUBJID")
      date_col_rand <- find_col(ivrs_rand, "RANDDT")
      if (is.null(date_col_rand)) date_col_rand <- find_col_like(ivrs_rand, "RAND.*DT")
      if (is.null(date_col_rand)) {
        candr <- names(ivrs_rand)[grepl("DT", toupper(names(ivrs_rand)))]
        if (length(candr)>0) date_col_rand <- candr[1]
      }
      if (!is.null(subj_col_rand) && !is.null(date_col_rand)) {
        tmp2 <- ivrs_rand %>%
          mutate(USUBJID=sas_catx("-", "ONCO-2026", .data[[subj_col_rand]]),
                 rdt=parse_any_to_date(.data[[date_col_rand]])) %>%
          filter(!is.na(rdt)) %>%
          group_by(USUBJID) %>% summarise(RFSTDTC=format_yymmdd10(min(rdt)), .groups="drop")
        for (u in need_fix$USUBJID) {
          v <- tmp2$RFSTDTC[tmp2$USUBJID==u]
          if (length(v)>0 && !is.na(v) && v!="") dm_for_tr$RFSTDTC[dm_for_tr$USUBJID==u] <- v
        }
      }
    }
  }
  need_fix2 <- dm_for_tr %>% filter(USUBJID %in% tr_subjs & (is.na(RFSTDTC) | RFSTDTC==""))
  if (nrow(need_fix2)>0 && !is.null(edc_ex)) {
    subj_col_ex2 <- find_col(edc_ex, "SUBJID")
    if (is.null(subj_col_ex2)) subj_col_ex2 <- find_col_like(edc_ex, "SUBJID")
    all_dt_cols <- names(edc_ex)[grepl("DT|DATE|DAY", toupper(names(edc_ex)))]
    best <- NULL
    for (dc in all_dt_cols) {
      tmpc <- edc_ex %>%
        mutate(USUBJID=sas_catx("-", "ONCO-2026", .data[[subj_col_ex2]]),
               dtmp=parse_any_to_date(.data[[dc]])) %>%
        filter(!is.na(dtmp)) %>%
        group_by(USUBJID) %>% summarise(RFSTDTC=format_yymmdd10(min(dtmp)), .groups="drop")
      if (nrow(tmpc)>0) { best <- tmpc; break }
    }
    if (!is.null(best)) {
      for (u in need_fix2$USUBJID) {
        v <- best$RFSTDTC[best$USUBJID==u]
        if (length(v)>0 && !is.na(v) && v!="") dm_for_tr$RFSTDTC[dm_for_tr$USUBJID==u] <- v
      }
    }
  }
}
dm_for_tr <- dm_for_tr %>% distinct(USUBJID, .keep_all=TRUE) %>% arrange(USUBJID)
dm_for_tr$RFSTDTC[is.na(dm_for_tr$RFSTDTC)] <- ""

# ---- Prepare tr_prep ----
tr_subj_col <- find_col(edc_tr, "SUBJID")
tr_lnkid_col <- find_col(edc_tr, "LNKID")
tr_testcd_col <- find_col(edc_tr, "TESTCD")
if (is.null(tr_testcd_col)) tr_testcd_col <- find_col(edc_tr, "TRTESTCD")
tr_orres_col <- find_col(edc_tr, "ORRES")
if (is.null(tr_orres_col)) tr_orres_col <- find_col(edc_tr, "TRORRES")
tr_unit_col <- find_col(edc_tr, "UNIT")
if (is.null(tr_unit_col)) tr_unit_col <- find_col(edc_tr, "TRORRESU")
if (is.null(tr_unit_col)) tr_unit_col <- find_col(edc_tr, "ORRESU")
tr_visit_col <- find_col(edc_tr, "VISIT")
tr_trdt_col <- find_col(edc_tr, "TRDT")
if (is.null(tr_trdt_col)) tr_trdt_col <- find_col(edc_tr, "TRDTC")
if (is.null(tr_trdt_col)) tr_trdt_col <- find_col_like(edc_tr, "TR.*DT")

tr_prep <- edc_tr %>%
  mutate(
    USUBJID = sas_catx("-", "ONCO-2026", .data[[tr_subj_col]]),
    LNKID = if (!is.null(tr_lnkid_col)) as.character(.data[[tr_lnkid_col]]) else "",
    TESTCD = if (!is.null(tr_testcd_col)) as.character(.data[[tr_testcd_col]]) else "",
    ORRES = if (!is.null(tr_orres_col)) as.character(.data[[tr_orres_col]]) else "",
    UNIT = if (!is.null(tr_unit_col)) as.character(.data[[tr_unit_col]]) else "",
    VISIT = if (!is.null(tr_visit_col)) as.character(.data[[tr_visit_col]]) else "",
    TRDT = if (!is.null(tr_trdt_col)) as.character(.data[[tr_trdt_col]]) else ""
  ) %>%
  select(USUBJID, LNKID, TESTCD, ORRES, UNIT, VISIT, TRDT) %>%
  arrange(USUBJID)

# ---- Map TR Domain ----
dm_for_tr_unique <- dm_for_tr %>% distinct(USUBJID, .keep_all=TRUE)

sdtm_tr1 <- tr_prep %>%
  left_join(dm_for_tr_unique, by="USUBJID") %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN = "TR",
    TRLNKID = LNKID,
    TRTESTCD = TESTCD,
    TRTEST = ifelse(TRTESTCD == "LDIAM", "Longest Diameter", ""),
    TRORRES = ORRES,
    TRORRESU = UNIT,
    TRSTRESC = ORRES,
    TRSTRESN = suppressWarnings(as.numeric(ORRES)),
    TRSTRESU = UNIT,
    TREVAL = "INVESTIGATOR",
    VISIT = sas_tranwrd_upcase(VISIT, "_", " "),
    TRDTC = ifelse(TRDT != "" & !is.na(TRDT) & TRDT != "NA",
                   format_yymmdd10(parse_date11(TRDT)),
                   NA_character_),
    TRDTC = ifelse(is.na(TRDTC), "", TRDTC),
    RFSTDTC = ifelse(is.na(RFSTDTC), "", RFSTDTC)
  ) %>%
  mutate(
    stdt = suppressWarnings(as.Date(TRDTC, format="%Y-%m-%d")),
    rfdt = suppressWarnings(as.Date(RFSTDTC, format="%Y-%m-%d")),
    TRSTDY = dplyr::case_when(
      TRDTC != "" & RFSTDTC != "" & !is.na(stdt) & !is.na(rfdt) & stdt >= rfdt ~ as.numeric(stdt - rfdt) + 1,
      TRDTC != "" & RFSTDTC != "" & !is.na(stdt) & !is.na(rfdt) & stdt < rfdt ~ as.numeric(stdt - rfdt),
      TRUE ~ NA_real_
    )
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, TRLNKID, TRTESTCD, TRTEST, TRORRES, TRORRESU, TRSTRESC, TRSTRESN, TRSTRESU, TREVAL, VISIT, TRDTC, TRSTDY, RFSTDTC, stdt, rfdt)

# ---- Derive TRSEQ ----
sdtm_tr1_sorted <- sdtm_tr1 %>% arrange(USUBJID, TRDTC)
sdtm_tr <- sdtm_tr1_sorted %>%
  group_by(USUBJID) %>%
  mutate(TRSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, TRSEQ, TRLNKID, TRTESTCD, TRTEST, TRORRES, TRORRESU, TRSTRESC, TRSTRESN, TRSTRESU, TREVAL, TRDTC, TRSTDY, VISIT) %>%
  arrange(USUBJID, TRDTC, TRSEQ)

sdtm_tr$TRSTDY[is.nan(sdtm_tr$TRSTDY)] <- NA_real_
sdtm_tr$TRSTRESN[is.nan(sdtm_tr$TRSTRESN)] <- NA_real_

# Write dataset tr.csv
write.csv(sdtm_tr, file.path(out, "tr.csv"), row.names=FALSE, na="")

# ---- Listing ----
lst_path <- file.path(out, "P028.lst")
txt_path <- file.path(out, "P028.txt")
csv_body_path <- file.path(out, "P028.csv")

con <- file(lst_path, open="wt")
writeLines("SDTM TR Domain - Tumor Results", con)
writeLines(paste0("Study: ONCO-2026  Domain: TR  Listing generated: ", format(Sys.Date(), "%Y-%m-%d")), con)
writeLines("", con)
writeLines(paste0("Number of observations: ", nrow(sdtm_tr)), con)
writeLines(paste0("Number of subjects: ", n_distinct(sdtm_tr$USUBJID)), con)
writeLines("", con)
header <- sprintf("%-12s %-4s %-15s %5s %-7s %-8s %-16s %-8s %-8s %-8s %-8s %-12s %-10s %6s %-15s",
                  "STUDYID","DOM","USUBJID","SEQ","LNKID","TESTCD","TEST","ORRES","ORRESU","STRESC","STRESN","STRESU","EVAL","DTC","STDY","VISIT")
writeLines(header, con)
writeLines(paste(rep("-", nchar(header)), collapse=""), con)
for (i in seq_len(nrow(sdtm_tr))) {
  row <- sdtm_tr[i,]
  line <- sprintf("%-12s %-4s %-15s %5d %-7s %-8s %-16s %-8s %-8s %-8s %-8s %-12s %-10s %6s %-15s",
                  row$STUDYID,
                  row$DOMAIN,
                  substr(row$USUBJID,1,15),
                  row$TRSEQ,
                  ifelse(is.na(row$TRLNKID),"",row$TRLNKID),
                  ifelse(is.na(row$TRTESTCD),"",row$TRTESTCD),
                  substr(ifelse(is.na(row$TRTEST),"",row$TRTEST),1,16),
                  ifelse(is.na(row$TRORRES),"",row$TRORRES),
                  ifelse(is.na(row$TRORRESU),"",row$TRORRESU),
                  ifelse(is.na(row$TRSTRESC),"",row$TRSTRESC),
                  ifelse(is.na(row$TRSTRESN),"",as.character(row$TRSTRESN)),
                  ifelse(is.na(row$TRSTRESU),"",row$TRSTRESU),
                  ifelse(is.na(row$TREVAL),"",row$TREVAL),
                  ifelse(is.na(row$TRDTC),"",row$TRDTC),
                  ifelse(is.na(row$TRSTDY),"",as.character(as.integer(row$TRSTDY))),
                  ifelse(is.na(row$VISIT),"",substr(row$VISIT,1,15))
  )
  writeLines(line, con)
}
writeLines("", con)
writeLines("Notes:", con)
writeLines("TRTEST = Longest Diameter when TRTESTCD = LDIAM", con)
writeLines("TRDTC derived from TRDT via DATE11. -> YYMMDD10.", con)
writeLines("TRSTDY = (TRDTC - RFSTDTC) +1 if TRDTC >= RFSTDTC else (TRDTC - RFSTDTC)", con)
close(con)
file.copy(lst_path, txt_path, overwrite=TRUE)
write.csv(sdtm_tr, csv_body_path, row.names=FALSE, na="")
file.copy(lst_path, file.path(out, "tr.txt"), overwrite=TRUE)
cat("Done. Wrote", nrow(sdtm_tr), "rows to", file.path(out, "tr.csv"), "\n")
cat("Listing written to", lst_path, "\n")
```
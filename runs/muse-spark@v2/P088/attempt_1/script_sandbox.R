out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

parse_date9 <- function(x) {
  x <- as.character(x)
  res <- rep(as.Date(NA), length(x))
  for (i in seq_along(x)) {
    v <- trimws(ifelse(is.na(x[i]), "", x[i]))
    if (v == "" || v == ".") { res[i] <- as.Date(NA); next }
    m <- regmatches(v, regexec("^([0-9]{1,2})([A-Za-z]{3})([0-9]{4})$", v))[[1]]
    if (length(m) == 4) {
      dd <- sprintf("%02d", as.integer(m[2]))
      monraw <- m[3]
      mon <- paste0(toupper(substr(monraw,1,1)), tolower(substr(monraw,2,3)))
      yyyy <- m[4]
      res[i] <- as.Date(paste0(dd, mon, yyyy), format = "%d%b%Y")
    } else {
      res[i] <- as.Date(NA)
    }
  }
  res
}
parse_yymmdd10 <- function(x) {
  x <- as.character(x)
  res <- rep(as.Date(NA), length(x))
  for (i in seq_along(x)) {
    v <- trimws(ifelse(is.na(x[i]), "", x[i]))
    if (v == "" || v == ".") { res[i] <- as.Date(NA); next }
    res[i] <- as.Date(v, format = "%Y-%m-%d")
  }
  res
}
fmt_date <- function(x) {
  ifelse(is.na(x), "", format(x, "%Y-%m-%d"))
}

# ============================================================
# STEP 1: mock RS
# ============================================================
rs <- data.frame(
  STUDYID = rep("STUDY01", 16),
  USUBJID = c(rep("STUDY01-1001",4), rep("STUDY01-1002",3), rep("STUDY01-1003",3),
              rep("STUDY01-1004",2), rep("STUDY01-1005",2), rep("STUDY01-1006",2)),
  RSSEQ = c(1,2,3,4, 1,2,3, 1,2,3, 1,2, 1,2, 1,2),
  RSTESTCD = rep("OVRLRESP",16),
  RSTEST = rep("Overall Response",16),
  RSSTRESC = c("STABLE DISEASE","STABLE DISEASE","PARTIAL RESPONSE","PARTIAL RESPONSE",
               "STABLE DISEASE","STABLE DISEASE","PROGRESSIVE DISEASE",
               "STABLE DISEASE","PARTIAL RESPONSE","COMPLETE RESPONSE",
               "STABLE DISEASE","STABLE DISEASE",
               "STABLE DISEASE","PROGRESSIVE DISEASE",
               "STABLE DISEASE","PARTIAL RESPONSE"),
  RSSTRESN = rep(NA_real_,16),
  VISIT = c("Baseline","Week 8","Week 16","Week 24",
            "Baseline","Week 8","Week 16",
            "Baseline","Week 8","Week 16",
            "Baseline","Week 8",
            "Baseline","Week 8",
            "Baseline","Week 8"),
  VISITNUM = c(1,3,4,5, 1,3,4, 1,3,4, 1,3, 1,3, 1,3),
  RSDTC = c("2024-01-15","2024-03-11","2024-05-06","2024-07-01",
            "2024-01-18","2024-03-14","2024-05-09",
            "2024-01-20","2024-03-16","2024-05-11",
            "2024-01-25","2024-03-21",
            "2024-01-28","2024-03-24",
            "2024-01-30","2024-03-26"),
  stringsAsFactors = FALSE
)

tu <- data.frame(
  STUDYID = rep("STUDY01",16),
  USUBJID = c(rep("STUDY01-1001",4), rep("STUDY01-1002",3), rep("STUDY01-1003",3),
              rep("STUDY01-1004",2), rep("STUDY01-1005",2), rep("STUDY01-1006",2)),
  TUSEQ = c(1,2,3,4, 1,2,3, 1,2,3, 1,2, 1,2, 1,2),
  TUTESTCD = rep("LDIAM",16),
  TUTEST = rep("Longest Diameter",16),
  TUORRES = as.character(c(45,42,35,30, 38,40,52, 55,40,0, 32,30, 28,42, 48,36)),
  TUSTRESC = as.character(c(45,42,35,30, 38,40,52, 55,40,0, 32,30, 28,42, 48,36)),
  TUSTRESN = c(45,42,35,30, 38,40,52, 55,40,0, 32,30, 28,42, 48,36),
  TUSTRESU = rep("mm",16),
  TULOC = c(rep("LIVER",4), rep("LUNG",3), rep("LYMPH NODE",3), rep("LIVER",2), rep("LUNG",2), rep("LIVER",2)),
  VISIT = c("Baseline","Week 8","Week 16","Week 24",
            "Baseline","Week 8","Week 16",
            "Baseline","Week 8","Week 16",
            "Baseline","Week 8",
            "Baseline","Week 8",
            "Baseline","Week 8"),
  VISITNUM = c(1,3,4,5, 1,3,4, 1,3,4, 1,3, 1,3, 1,3),
  TUDTC = c("2024-01-15","2024-03-11","2024-05-06","2024-07-01",
            "2024-01-18","2024-03-14","2024-05-09",
            "2024-01-20","2024-03-16","2024-05-11",
            "2024-01-25","2024-03-21",
            "2024-01-28","2024-03-24",
            "2024-01-30","2024-03-26"),
  stringsAsFactors = FALSE
)

# ============================================================
# STEP 2: mock ADSL
# ============================================================
adsl_raw <- data.frame(
  STUDYID = rep("STUDY01",6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  SEX = c("M","F","M","F","M","F"),
  SAFFL = rep("Y",6),
  EFFFL = rep("Y",6),
  TRTSDT_c = c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"),
  TRTEDT_c = c("15JUN2024","18JUN2024","01APR2024","25JUN2024",".","01JUL2024"),
  stringsAsFactors = FALSE
)
adsl <- adsl_raw
adsl$TRTSDT <- parse_date9(adsl$TRTSDT_c)
adsl$TRTEDT <- parse_date9(adsl$TRTEDT_c)
adsl$TRTSDT_c <- NULL
adsl$TRTEDT_c <- NULL
# SAS sort by STUDYID USUBJID
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), ]
rs <- rs[order(rs$STUDYID, rs$USUBJID), ]
tu <- tu[order(tu$STUDYID, tu$USUBJID), ]

# ============================================================
# STEP 3: adeff_resp
# ============================================================
# merge rs (where RSTESTCD='OVRLRESP') with adsl, by STUDYID USUBJID, if a and b
rs_sub <- rs[rs$RSTESTCD == "OVRLRESP", ]
# inner join (all rs subjects in adsl)
adeff_resp <- merge(rs_sub, adsl, by = c("STUDYID","USUBJID"), all = FALSE, sort = FALSE)
stopifnot(nrow(adeff_resp) > 0)
stopifnot(all(c("RSDTC","TRTSDT","VISIT","VISITNUM","RSSTRESC") %in% names(adeff_resp)))
adeff_resp$PARAMCD <- "OVRLRESP"
adeff_resp$PARAM <- "Overall Response"
adeff_resp$PARAMN <- 1
adeff_resp$AVALC <- adeff_resp$RSSTRESC
up <- toupper(trimws(adeff_resp$RSSTRESC))
adeff_resp$AVAL <- ifelse(up == "COMPLETE RESPONSE", 1,
                   ifelse(up == "PARTIAL RESPONSE", 2,
                   ifelse(up == "STABLE DISEASE", 3,
                   ifelse(up == "PROGRESSIVE DISEASE", 4,
                   ifelse(up == "NOT EVALUABLE", 5, NA_real_)))))
adeff_resp$ADT <- parse_yymmdd10(adeff_resp$RSDTC)
# ADY
adeff_resp$ADY <- ifelse(!is.na(adeff_resp$ADT) & !is.na(adeff_resp$TRTSDT),
  ifelse(adeff_resp$ADT >= adeff_resp$TRTSDT,
    as.integer(adeff_resp$ADT - adeff_resp$TRTSDT) + 1L,
    as.integer(adeff_resp$ADT - adeff_resp$TRTSDT)), NA_integer_)
adeff_resp$AVISIT <- adeff_resp$VISIT
adeff_resp$AVISITN <- adeff_resp$VISITNUM
adeff_resp$ABLFL <- ifelse(adeff_resp$AVISIT == "Baseline", "Y", "")
adeff_resp$ANL01FL <- ifelse(adeff_resp$ABLFL != "Y" & !is.na(adeff_resp$AVAL), "Y", "")
adeff_resp <- adeff_resp[, c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","EFFFL","TRTSDT","TRTEDT","AGE","SEX",
  "PARAMCD","PARAM","PARAMN","AVAL","AVALC","ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL")]

# ============================================================
# STEP 4: adeff_tumor
# ============================================================
tu_sub <- tu[tu$TUTESTCD == "LDIAM", ]
adeff_tumor <- merge(tu_sub, adsl, by = c("STUDYID","USUBJID"), all = FALSE, sort = FALSE)
stopifnot(nrow(adeff_tumor) > 0)
adeff_tumor$PARAMCD <- "SUMDIAM"
adeff_tumor$PARAM <- "Sum of Longest Diameters"
adeff_tumor$PARAMN <- 2
adeff_tumor$AVAL <- adeff_tumor$TUSTRESN
adeff_tumor$AVALC <- ""
adeff_tumor$AVALU <- adeff_tumor$TUSTRESU
adeff_tumor$ADT <- parse_yymmdd10(adeff_tumor$TUDTC)
adeff_tumor$ADY <- ifelse(!is.na(adeff_tumor$ADT) & !is.na(adeff_tumor$TRTSDT),
  ifelse(adeff_tumor$ADT >= adeff_tumor$TRTSDT,
    as.integer(adeff_tumor$ADT - adeff_tumor$TRTSDT) + 1L,
    as.integer(adeff_tumor$ADT - adeff_tumor$TRTSDT)), NA_integer_)
adeff_tumor$AVISIT <- adeff_tumor$VISIT
adeff_tumor$AVISITN <- adeff_tumor$VISITNUM
adeff_tumor$ABLFL <- ifelse(adeff_tumor$AVISIT == "Baseline", "Y", "")
adeff_tumor$ANL01FL <- ifelse(adeff_tumor$ABLFL != "Y" & !is.na(adeff_tumor$AVAL), "Y", "")
adeff_tumor <- adeff_tumor[, c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","EFFFL","TRTSDT","TRTEDT","AGE","SEX",
  "PARAMCD","PARAM","PARAMN","AVAL","AVALC","AVALU","ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL")]

# ============================================================
# STEP 5: baseline and change for tumor
# ============================================================
base_tumor <- adeff_tumor[adeff_tumor$ABLFL == "Y", ]
base_tumor <- data.frame(USUBJID = base_tumor$USUBJID, PARAMCD = base_tumor$PARAMCD,
  BASE = base_tumor$AVAL, stringsAsFactors = FALSE)
# sort both by USUBJID PARAMCD
adeff_tumor <- adeff_tumor[order(adeff_tumor$USUBJID, adeff_tumor$PARAMCD), ]
base_tumor <- base_tumor[order(base_tumor$USUBJID, base_tumor$PARAMCD), ]
# merge (left join)
adeff_tumor <- merge(adeff_tumor, base_tumor, by = c("USUBJID","PARAMCD"), all.x = TRUE, sort = FALSE)
adeff_tumor$CHG <- ifelse(!is.na(adeff_tumor$AVAL) & !is.na(adeff_tumor$BASE),
  adeff_tumor$AVAL - adeff_tumor$BASE, NA_real_)
adeff_tumor$PCHG <- ifelse(!is.na(adeff_tumor$AVAL) & !is.na(adeff_tumor$BASE) & adeff_tumor$BASE > 0,
  (adeff_tumor$CHG / adeff_tumor$BASE) * 100, NA_real_)

# ============================================================
# STEP 6: BOR
# ============================================================
resp_post <- adeff_resp[adeff_resp$ABLFL != "Y", ]
resp_sorted <- resp_post[order(resp_post$USUBJID, resp_post$AVAL, resp_post$ADT), ]
# first per USUBJID
resp_sorted <- resp_sorted[!duplicated(resp_sorted$USUBJID), ]
bor <- resp_sorted
bor$PARAMCD <- "BOR"
bor$PARAM <- "Best Overall Response"
bor$PARAMN <- 3
bor$AVISIT <- "Best Response"
bor$AVISITN <- 99
bor$ABLFL <- ""
bor$ANL01FL <- "Y"

# ============================================================
# STEP 7: responder flag
# ============================================================
respfl <- bor
respfl$RESPFL <- ifelse(!is.na(respfl$AVAL) & respfl$AVAL %in% c(1,2), "Y", "N")
respfl$PARAMCD <- "RESPFL"
respfl$PARAM <- "Objective Response Flag"
respfl$PARAMN <- 4
respfl$AVAL <- ifelse(respfl$RESPFL == "Y", 1, 0)
respfl$AVALC <- respfl$RESPFL

# ============================================================
# STEP 8: nadir
# ============================================================
tumor_post <- adeff_tumor[adeff_tumor$ABLFL != "Y", ]
# PROC MEANS min per USUBJID
nadir_min <- aggregate(AVAL ~ USUBJID, data = tumor_post, FUN = function(x) min(x, na.rm = TRUE))
names(nadir_min)[names(nadir_min)=="AVAL"] <- "AVAL"
# baseline info to merge
base_info <- adeff_tumor[adeff_tumor$ABLFL == "Y", ]
base_info <- base_info[, c("USUBJID","STUDYID","TRTP","TRTPN","SAFFL","EFFFL","TRTSDT","TRTEDT","AGE","SEX","BASE")]
# keep first per USUBJID (one baseline per subject)
base_info <- base_info[order(base_info$USUBJID), ]
base_info <- base_info[!duplicated(base_info$USUBJID), ]
nadir <- merge(nadir_min, base_info, by = "USUBJID", all.x = TRUE, sort = FALSE)
stopifnot(nrow(nadir) > 0)
nadir$PARAMCD <- "NADIRTUM"
nadir$PARAM <- "Nadir Tumor Size"
nadir$PARAMN <- 5
nadir$AVALC <- ""
nadir$AVALU <- "mm"
nadir$AVISIT <- "Nadir"
nadir$AVISITN <- 98
nadir$ABLFL <- ""
nadir$ANL01FL <- "Y"
# ADT/ADY missing for nadir (no date); ensure columns exist
nadir$ADT <- as.Date(NA)
nadir$ADY <- NA_integer_
nadir$CHG <- ifelse(!is.na(nadir$AVAL) & !is.na(nadir$BASE), nadir$AVAL - nadir$BASE, NA_real_)
nadir$PCHG <- ifelse(!is.na(nadir$AVAL) & !is.na(nadir$BASE) & nadir$BASE > 0,
  (nadir$CHG / nadir$BASE) * 100, NA_real_)

# ============================================================
# STEP 9: combine
# ============================================================
# Align columns for rbind: union of names
all_names <- unique(c(names(adeff_resp), names(adeff_tumor), names(bor), names(respfl), names(nadir)))
fill_missing <- function(df, alln) {
  for (nm in alln) {
    if (!nm %in% names(df)) {
      # infer type from other dfs: default character NA unless known numeric
      if (nm %in% c("PARAMN","AVAL","BASE","CHG","PCHG","ADY","AVISITN","AGE","TRTPN")) df[[nm]] <- NA_real_
      else if (nm %in% c("ADT","TRTSDT","TRTEDT")) df[[nm]] <- as.Date(NA)
      else df[[nm]] <- NA_character_
    }
  }
  df
}
adeff_resp2 <- fill_missing(adeff_resp, all_names)
adeff_tumor2 <- fill_missing(adeff_tumor, all_names)
bor2 <- fill_missing(bor, all_names)
respfl2 <- fill_missing(respfl, all_names)
nadir2 <- fill_missing(nadir, all_names)
# Ensure same column types for Date columns
for (d in list(adeff_resp2, adeff_tumor2, bor2, respfl2, nadir2)) { }
adeff <- rbind(adeff_resp2, adeff_tumor2, bor2, respfl2, nadir2)

# ============================================================
# STEP 10: sort, ASEQ, RSCAT
# ============================================================
adeff <- adeff[order(adeff$STUDYID, adeff$USUBJID, adeff$PARAMN, adeff$AVISITN, adeff$ADT), ]
# ASEQ by STUDYID USUBJID
adeff <- adeff[order(adeff$STUDYID, adeff$USUBJID, adeff$PARAMN, adeff$AVISITN, adeff$ADT), ]
adeff$ASEQ <- NA_integer_
# first.USUBJID logic with STUDYID USUBJID grouping (SAS: by STUDYID USUBJID)
key <- paste(adeff$STUDYID, adeff$USUBJID)
aseq <- integer(nrow(adeff))
cur <- 0
prev <- ""
for (i in seq_len(nrow(adeff))) {
  if (key[i] != prev) { cur <- 1; prev <- key[i] } else { cur <- cur + 1 }
  aseq[i] <- cur
}
adeff$ASEQ <- aseq
adeff$RSCAT <- NA_character_
idx <- adeff$PARAMCD %in% c("OVRLRESP","BOR")
adeff$RSCAT[idx & adeff$AVALC %in% c("COMPLETE RESPONSE","PARTIAL RESPONSE")] <- "RESPONDER"
adeff$RSCAT[idx & adeff$AVALC == "STABLE DISEASE"] <- "STABLE"
adeff$RSCAT[idx & adeff$AVALC == "PROGRESSIVE DISEASE"] <- "PROGRESSOR"
# OTHER for remaining in group
need_other <- idx & is.na(adeff$RSCAT)
adeff$RSCAT[need_other] <- "OTHER"

# Final column order mimicking SAS PDV
final_order <- c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","EFFFL","TRTSDT","TRTEDT","AGE","SEX",
  "PARAMCD","PARAM","PARAMN","AVAL","AVALC","ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL",
  "AVALU","BASE","CHG","PCHG","RESPFL","ASEQ","RSCAT")
final_order <- final_order[final_order %in% names(adeff)]
others <- setdiff(names(adeff), final_order)
adeff <- adeff[, c(final_order, others)]

# Labels (for documentation)
attr(adeff$STUDYID,"label") <- "Study Identifier"
attr(adeff$USUBJID,"label") <- "Unique Subject Identifier"
attr(adeff$ASEQ,"label") <- "Analysis Sequence Number"
attr(adeff$PARAMCD,"label") <- "Parameter Code"
attr(adeff$PARAM,"label") <- "Parameter"
attr(adeff$PARAMN,"label") <- "Parameter (N)"
attr(adeff$AVAL,"label") <- "Analysis Value"
attr(adeff$AVALC,"label") <- "Analysis Value (C)"
attr(adeff$AVALU,"label") <- "Analysis Value Unit"
attr(adeff$BASE,"label") <- "Baseline Value"
attr(adeff$CHG,"label") <- "Change from Baseline"
attr(adeff$PCHG,"label") <- "Percent Change from Baseline"
attr(adeff$ADT,"label") <- "Analysis Date"
attr(adeff$ADY,"label") <- "Analysis Relative Day"
attr(adeff$AVISIT,"label") <- "Analysis Visit"
attr(adeff$AVISITN,"label") <- "Analysis Visit (N)"
attr(adeff$ABLFL,"label") <- "Baseline Record Flag"
attr(adeff$ANL01FL,"label") <- "Analysis Record Flag 01"
attr(adeff$RSCAT,"label") <- "Response Category"
attr(adeff$TRTP,"label") <- "Planned Treatment"
attr(adeff$TRTPN,"label") <- "Planned Treatment (N)"
attr(adeff$EFFFL,"label") <- "Efficacy Population Flag"
attr(adeff$SAFFL,"label") <- "Safety Population Flag"

# ============================================================
# Outputs
# ============================================================
# Helper to write CSV with ISO dates
write_out_csv <- function(df, path) {
  df2 <- df
  for (nm in names(df2)) {
    if (inherits(df2[[nm]], "Date")) df2[[nm]] <- fmt_date(df2[[nm]])
    if (inherits(df2[[nm]], "POSIXt")) df2[[nm]] <- format(df2[[nm]], "%Y-%m-%dT%H:%M:%S")
  }
  write.csv(df2, path, row.names = FALSE, na = "")
}

# adsl output: STUDYID USUBJID TRTP TRTPN AGE SEX SAFFL EFFFL TRTSDT TRTEDT
adsl_out <- adsl[, c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL","EFFFL","TRTSDT","TRTEDT")]
write_out_csv(adsl_out, file.path(out, "adsl.csv"))

# adeff_resp output (as kept, before BASE merge)
write_out_csv(adeff_resp, file.path(out, "adeff_resp.csv"))

# adeff_tumor output (after BASE/CHG/PCHG)
adeff_tumor_out <- adeff_tumor
write_out_csv(adeff_tumor_out, file.path(out, "adeff_tumor.csv"))

# adeff
write_out_csv(adeff, file.path(out, "adeff.csv"))

# ---- P088 listing: PROC CONTENTS + PROC PRINT ----
# Build text rendering
con <- textConnection("txt_out", "w", local = TRUE)
sink(con)
cat("PROC CONTENTS DATA=ADEFF VARNUM\n")
cat("--------------------------------------------------\n")
cat(sprintf("Observations: %d  Variables: %d\n", nrow(adeff), ncol(adeff)))
cat("Variables in creation order:\n")
for (i in seq_along(names(adeff))) {
  v <- names(adeff)[i]
  cl <- class(adeff[[v]])[1]
  typ <- ifelse(cl %in% c("Date","POSIXct","POSIXt"), "Date",
         ifelse(is.numeric(adeff[[v]]), "Num", "Char"))
  len <- ifelse(typ=="Char", max(nchar(as.character(adeff[[v]][!is.na(adeff[[v]])])), na.rm=TRUE), 8)
  if (!is.finite(len)) len <- 8
  lbl <- attr(adeff[[v]], "label")
  if (is.null(lbl)) lbl <- ""
  cat(sprintf("%3d  %-10s  %-4s  Len=%s  Label=%s\n", i, v, typ, as.character(len), lbl))
}
cat("\nPROC PRINT DATA=ADEFF NOOBS\n")
cat("--------------------------------------------------\n")
sink()
close(con)

# Printable version of adeff for listing (format dates ISO, numerics as-is)
adeff_print <- adeff
for (nm in names(adeff_print)) {
  if (inherits(adeff_print[[nm]], "Date")) adeff_print[[nm]] <- fmt_date(adeff_print[[nm]])
}
# Format numeric columns without rounding: use as.character
print_txt <- capture.output(print(as.data.frame(adeff_print), row.names = FALSE, right = TRUE, na.print = "", max = 1e6))
full_txt <- c(txt_out, print_txt)
writeLines(full_txt, file.path(out, "P088.txt"))
writeLines(full_txt, file.path(out, "P088.lst"))
# CSV of table body (same as adeff)
write_out_csv(adeff, file.path(out, "P088.csv"))

cat("Done. adeff rows:", nrow(adeff), "\n")

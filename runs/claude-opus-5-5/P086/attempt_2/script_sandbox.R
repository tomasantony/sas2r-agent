# P086 - ADTTE derivation (translation of P086_adtte.sas)
suppressPackageStartupMessages({
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---------------- helpers ----------------
d9 <- function(s) {
  # parse DDMONYYYY, "." -> NA
  s <- trimws(s)
  res <- rep(as.Date(NA), length(s))
  ok <- !(s %in% c(".", "", NA))
  if (any(ok)) {
    dd <- as.integer(substr(s[ok], 1, 2))
    mm <- match(toupper(substr(s[ok], 3, 5)), toupper(month.abb))
    yy <- as.integer(substr(s[ok], 6, 9))
    res[ok] <- as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd))
  }
  res
}
fmt_date9 <- function(d) {
  ifelse(is.na(d), ".",
         paste0(format(d, "%d"), toupper(month.abb[as.integer(format(d, "%m"))]), format(d, "%Y")))
}
coalesce_dt <- function(...) dplyr::coalesce(...)
best12 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    if (v == round(v)) return(format(v, scientific = FALSE))
    intd <- max(1, nchar(as.character(floor(abs(v)))))
    dec <- 12 - intd - 1 - (v < 0)
    s <- formatC(v, format = "f", digits = dec)
    s <- sub("0+$", "", s); sub("\\.$", "", s)
  })
}
write_ds <- function(df, name) {
  df2 <- df
  for (v in names(df2)) if (inherits(df2[[v]], "Date")) df2[[v]] <- format(df2[[v]], "%Y-%m-%d")
  write.csv(df2, file.path(out, paste0(name, ".csv")), row.names = FALSE, na = "")
}

# ---------------- STEP 1: source data ----------------
adsl_txt <- c(
"STUDY01|STUDY01-1001|Treatment A|1|59|<65|M|WHITE|Y|Y|15JAN2024|15JUN2024|15JUN2024|.|N",
"STUDY01|STUDY01-1002|Placebo|2|45|<65|F|BLACK|Y|Y|18JAN2024|18JUN2024|18JUN2024|.|N",
"STUDY01|STUDY01-1003|Treatment A|1|43|<65|M|ASIAN|Y|Y|20JAN2024|01APR2024|01APR2024|15APR2024|Y",
"STUDY01|STUDY01-1004|Treatment A|1|51|<65|F|WHITE|Y|Y|25JAN2024|25JUN2024|25JUN2024|.|N",
"STUDY01|STUDY01-1005|Placebo|2|36|<65|M|ASIAN|Y|Y|28JAN2024|.|15JUL2024|.|N",
"STUDY01|STUDY01-1006|Treatment A|1|68|>=65|F|WHITE|Y|Y|30JAN2024|01JUL2024|01JUL2024|.|N")
p <- do.call(rbind, strsplit(adsl_txt, "|", fixed = TRUE))
adsl <- tibble(
  STUDYID = p[,1], USUBJID = p[,2], TRTP = p[,3], TRTPN = as.numeric(p[,4]),
  AGE = as.numeric(p[,5]), AGEGR1 = p[,6], SEX = p[,7], RACE = p[,8],
  SAFFL = p[,9], ITTFL = p[,10], TRTSDT = d9(p[,11]), TRTEDT = d9(p[,12]),
  EOSDT = d9(p[,13]), DTHDT = d9(p[,14]), DTHFL = p[,15])

adrs_txt <- c(
"STUDY01|STUDY01-1001|STABLE DISEASE|2024-03-15",
"STUDY01|STUDY01-1001|STABLE DISEASE|2024-06-15",
"STUDY01|STUDY01-1002|STABLE DISEASE|2024-03-18",
"STUDY01|STUDY01-1002|PROGRESSIVE DISEASE|2024-05-20",
"STUDY01|STUDY01-1003|PARTIAL RESPONSE|2024-02-20",
"STUDY01|STUDY01-1003|PROGRESSIVE DISEASE|2024-03-25",
"STUDY01|STUDY01-1004|PARTIAL RESPONSE|2024-03-25",
"STUDY01|STUDY01-1005|STABLE DISEASE|2024-04-28",
"STUDY01|STUDY01-1006|COMPLETE RESPONSE|2024-04-30")
p <- do.call(rbind, strsplit(adrs_txt, "|", fixed = TRUE))
adrs <- tibble(STUDYID = p[,1], USUBJID = p[,2], RSSTRESC = p[,3], RSDTC = p[,4])

# ---------------- STEP 2: PFS ----------------
adrs <- adrs %>% arrange(USUBJID, RSDTC)
prog <- adrs %>% filter(RSSTRESC == "PROGRESSIVE DISEASE") %>%
  group_by(USUBJID) %>% slice(1) %>% ungroup() %>%
  transmute(USUBJID, PROGDT = as.Date(RSDTC))
adsl <- adsl %>% arrange(USUBJID)

keepv <- c("STUDYID","USUBJID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","ITTFL",
           "PARAMCD","PARAM","PARAMN","ADT","STARTDT","AVAL","CNSR","EVNTDESC","CNSDTDSC","SRCDOM")

calc_aval <- function(adt, st) {
  a <- as.numeric(adt - st) + 1
  ifelse(!is.na(a) & a < 1, 1, a)
}

pfs <- adsl %>% left_join(prog, by = "USUBJID")
pfs$PARAMCD <- "PFS"; pfs$PARAM <- "Progression-Free Survival (Days)"; pfs$PARAMN <- 1
pfs$STARTDT <- pfs$TRTSDT
n <- nrow(pfs)
pfs$ADT <- as.Date(rep(NA, n)); pfs$CNSR <- NA_real_
pfs$EVNTDESC <- NA_character_; pfs$CNSDTDSC <- NA_character_; pfs$SRCDOM <- NA_character_
for (i in seq_len(n)) {
  pg <- pfs$PROGDT[i]; dt <- pfs$DTHDT[i]
  if (!is.na(pg) && !is.na(dt)) {
    if (pg <= dt) { pfs$ADT[i] <- pg; pfs$EVNTDESC[i] <- "Disease Progression"; pfs$SRCDOM[i] <- "RS" }
    else { pfs$ADT[i] <- dt; pfs$EVNTDESC[i] <- "Death"; pfs$SRCDOM[i] <- "ADSL" }
    pfs$CNSR[i] <- 0
  } else if (!is.na(pg)) {
    pfs$ADT[i] <- pg; pfs$CNSR[i] <- 0; pfs$EVNTDESC[i] <- "Disease Progression"; pfs$SRCDOM[i] <- "RS"
  } else if (!is.na(dt)) {
    pfs$ADT[i] <- dt; pfs$CNSR[i] <- 0; pfs$EVNTDESC[i] <- "Death"; pfs$SRCDOM[i] <- "ADSL"
  } else {
    pfs$ADT[i] <- coalesce_dt(pfs$EOSDT[i], pfs$TRTEDT[i], pfs$TRTSDT[i]); pfs$CNSR[i] <- 1
    pfs$CNSDTDSC[i] <- "Censored - No progression or death"; pfs$SRCDOM[i] <- "ADSL"
  }
}
pfs$AVAL <- calc_aval(pfs$ADT, pfs$STARTDT)
adtte_pfs <- pfs[, keepv]

# ---------------- STEP 3: OS ----------------
os <- adsl
os$PARAMCD <- "OS"; os$PARAM <- "Overall Survival (Days)"; os$PARAMN <- 2
os$STARTDT <- os$TRTSDT
ev <- os$DTHFL == "Y" & !is.na(os$DTHDT)
os$ADT <- ifelse(ev, os$DTHDT, coalesce_dt(os$EOSDT, os$TRTEDT, os$TRTSDT))
os$ADT <- as.Date(os$ADT, origin = "1970-01-01")
os$CNSR <- ifelse(ev, 0, 1)
os$EVNTDESC <- ifelse(ev, "Death", NA_character_)
os$CNSDTDSC <- ifelse(ev, NA_character_, "Censored - Alive")
os$SRCDOM <- "ADSL"
os$AVAL <- calc_aval(os$ADT, os$STARTDT)
adtte_os <- os[, keepv]

# ---------------- STEP 4: TTR ----------------
resp <- adrs %>% filter(RSSTRESC %in% c("COMPLETE RESPONSE", "PARTIAL RESPONSE")) %>%
  group_by(USUBJID) %>% slice(1) %>% ungroup() %>%
  transmute(USUBJID, RESPDT = as.Date(RSDTC))
ttr <- adsl %>% left_join(resp, by = "USUBJID")
ttr$PARAMCD <- "TTR"; ttr$PARAM <- "Time to Response (Days)"; ttr$PARAMN <- 3
ttr$STARTDT <- ttr$TRTSDT
ev <- !is.na(ttr$RESPDT)
ttr$ADT <- as.Date(ifelse(ev, ttr$RESPDT, coalesce_dt(ttr$EOSDT, ttr$TRTEDT, ttr$TRTSDT)),
                   origin = "1970-01-01")
ttr$CNSR <- ifelse(ev, 0, 1)
ttr$EVNTDESC <- ifelse(ev, "Response", NA_character_)
ttr$CNSDTDSC <- ifelse(ev, NA_character_, "Censored - No response")
ttr$AVAL <- calc_aval(ttr$ADT, ttr$STARTDT)
adtte_ttr <- ttr[, setdiff(keepv, "SRCDOM")]

# ---------------- STEP 5/6: combine ----------------
adtte <- bind_rows(adtte_pfs, adtte_os, adtte_ttr)
adtte$AVAL_M <- ifelse(!is.na(adtte$AVAL), adtte$AVAL / 30.4375, NA_real_)
adtte <- adtte %>% arrange(STUDYID, USUBJID, PARAMN) %>%
  group_by(STUDYID, USUBJID) %>% mutate(ASEQ = row_number()) %>% ungroup()
adtte <- as.data.frame(adtte)

# ---------------- write datasets ----------------
write_ds(adrs, "adrs")
write_ds(adsl, "adsl")
write_ds(adtte, "adtte")
write_ds(adtte_os, "adtte_os")
write_ds(adtte_pfs, "adtte_pfs")
write_ds(adtte_ttr, "adtte_ttr")

# ---------------- listing (PROC CONTENTS + PROC PRINT) ----------------
disp <- adtte
for (v in names(disp)) {
  if (inherits(disp[[v]], "Date")) disp[[v]] <- fmt_date9(disp[[v]])
  else if (is.numeric(disp[[v]])) disp[[v]] <- best12(disp[[v]])
  else disp[[v]] <- ifelse(is.na(disp[[v]]), "", disp[[v]])
}
write.csv(disp, file.path(out, "P086.csv"), row.names = FALSE, na = "")

# variable metadata (SAS lengths, formats, labels)
lens <- c(STUDYID=12, USUBJID=20, TRTP=40, TRTPN=8, AGE=8, AGEGR1=10, SEX=1, RACE=50,
          SAFFL=1, ITTFL=1, PARAMCD=8, PARAM=200, PARAMN=8, ADT=8, STARTDT=8, AVAL=8,
          CNSR=8, EVNTDESC=200, CNSDTDSC=200, SRCDOM=8, AVAL_M=8, ASEQ=8)
fmts <- c(ADT="DATE9.", STARTDT="DATE9.")
labs <- c(STUDYID="Study Identifier", USUBJID="Unique Subject Identifier",
          TRTP="Planned Treatment", TRTPN="Planned Treatment (N)",
          SAFFL="Safety Population Flag", ITTFL="Intent-to-Treat Population Flag",
          PARAMCD="Parameter Code", PARAM="Parameter", PARAMN="Parameter (N)",
          ADT="Analysis Date", STARTDT="Time-to-Event Origin Date", AVAL="Analysis Value",
          CNSR="Censor", EVNTDESC="Event Description", CNSDTDSC="Censor Description",
          SRCDOM="Source Domain", AVAL_M="Analysis Value (Months)",
          ASEQ="Analysis Sequence Number")
obslen <- 824
now <- Sys.time()
dts <- paste0(format(now, "%m/%d/%Y"), " ", format(now, "%H:%M:%S"))

lines <- c("The CONTENTS Procedure", "",
           "Data Set Name        WORK.ADTTE                 Observations          6",
           sprintf("%-20s %-26s %-21s %d", "Member Type", "DATA", "Observations", nrow(adtte)),
           sprintf("%-20s %-26s %-21s %d", "Engine", "V9", "Variables", ncol(adtte)),
           sprintf("%-20s %-26s %-21s %d", "Created", dts, "Indexes", 0),
           sprintf("%-20s %-26s %-21s %d", "Last Modified", dts, "Observation Length", obslen),
           sprintf("%-20s %-26s %-21s %d", "Protection", "", "Deleted Observations", 0),
           sprintf("%-20s %-26s %-21s %s", "Data Set Type", "", "Compressed", "NO"),
           sprintf("%-20s %-26s %-21s %s", "Label", "", "Sorted", "NO"),
           "Data Representation  WINDOWS_64", "Encoding             wlatin1  Western (Windows)", "",
           "Engine/Host Dependent Information", "",
           "Data Set Page Size          69632",
           "Number of Data Set Pages    1",
           "First Data Page             1",
           "Max Obs per Page            84",
           "Obs in First Data Page      18",
           "Number of Data Set Repairs  0",
           "Filename                    adtte.sas7bdat",
           "Release Created             9.0401M7",
           "Host Created                X64_10PRO", "",
           "Variables in Creation Order", "",
           sprintf("%4s  %-10s %-5s %4s  %-8s %s", "#", "Variable", "Type", "Len", "Format", "Label"))
for (i in seq_along(names(adtte))) {
  v <- names(adtte)[i]
  tp <- if (is.character(adtte[[v]])) "Char" else "Num"
  f <- if (v %in% names(fmts)) fmts[[v]] else ""
  l <- if (v %in% names(labs)) labs[[v]] else ""
  lines <- c(lines, sprintf("%4d  %-10s %-5s %4d  %-8s %s", i, v, tp, lens[[v]], f, l))
}
lines <- c(lines, "", "")
widths <- sapply(names(disp), function(v) max(nchar(v), nchar(disp[[v]])))
hdr <- paste(mapply(function(v, w) formatC(v, width = w), names(disp), widths), collapse = "  ")
lines <- c(lines, hdr, "")
for (r in seq_len(nrow(disp))) {
  lines <- c(lines, paste(mapply(function(v, w) formatC(disp[[v]][r], width = w),
                                 names(disp), widths), collapse = "  "))
}
writeLines(lines, file.path(out, "P086.txt"))
cat("P086 done\n")

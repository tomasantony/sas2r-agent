# P010 - SDTM CM domain (translation of P010_sdtm_cm.sas)
suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path   <- "/bench/data/derived/c2c"
usubjid_length <- 40

# ---------------------------------------------------------------- helpers
sas_trunc <- function(x, n) {
  x[is.na(x)] <- ""
  substr(x, 1, n)
}
sas_strip <- function(x) {
  x[is.na(x)] <- ""
  trimws(x)
}
# SAS SCAN with blank delimiter (consecutive delimiters treated as one)
sas_scan <- function(x, n, delim = " ") {
  vapply(x, function(s) {
    if (is.na(s)) return("")
    parts <- strsplit(s, paste0("[", delim, "]+"))[[1]]
    parts <- parts[parts != ""]
    if (length(parts) >= n) parts[n] else ""
  }, character(1), USE.NAMES = FALSE)
}
# input(x, best.) -> numeric or NA
sas_input_best <- function(x) {
  x <- trimws(x)
  x[x == ""] <- NA
  suppressWarnings(as.numeric(x))
}
# input(x, ddmmyy10.) -> Date or NA
sas_input_ddmmyy <- function(x) {
  vapply(x, function(s) {
    if (is.na(s) || trimws(s) == "") return(NA_real_)
    s <- trimws(s)
    p <- strsplit(s, "[^0-9]+")[[1]]
    p <- p[p != ""]
    if (length(p) == 1 && nchar(p) %in% c(6, 8)) {
      p <- c(substr(p, 1, 2), substr(p, 3, 4), substr(p, 5, nchar(p)))
    }
    if (length(p) != 3) return(NA_real_)
    d <- as.integer(p[1]); m <- as.integer(p[2]); y <- as.integer(p[3])
    if (nchar(p[3]) <= 2) y <- ifelse(y < 26, 2000 + y, 1900 + y)  # YEARCUTOFF=1926
    dt <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, m, d), "%Y-%m-%d"))
    if (is.na(dt)) NA_real_ else as.numeric(dt)
  }, numeric(1), USE.NAMES = FALSE)
}

# ---------------------------------------------------------------- 2. raw import
raw_path <- file.path(project_path, "data/raw/raw_conmeds.csv")
raw <- read.csv(raw_path, header = FALSE, skip = 1, colClasses = "character",
                na.strings = character(0), strip.white = TRUE, fill = TRUE,
                stringsAsFactors = FALSE)
nm <- c("SUBJECT", "MEDICATION", "DOSE", "START_DT", "END_DT", "ONGOING")
while (ncol(raw) < 6) raw[[ncol(raw) + 1]] <- ""
raw <- raw[, 1:6]
names(raw) <- nm
raw_cm <- raw %>%
  mutate(SUBJECT    = sas_trunc(trimws(SUBJECT), 10),
         MEDICATION = sas_trunc(trimws(MEDICATION), 50),
         DOSE       = sas_trunc(trimws(DOSE), 20),
         START_DT   = sas_trunc(trimws(START_DT), 10),
         END_DT     = sas_trunc(trimws(END_DT), 10),
         ONGOING    = sas_trunc(trimws(ONGOING), 1))
cat("raw_cm rows:", nrow(raw_cm), "\n")

# ---------------------------------------------------------------- 3. map
cm_draft <- raw_cm %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN  = "CM",
    USUBJID = sas_trunc(paste0(sas_strip(STUDYID), "-", sas_strip(SUBJECT)), usubjid_length),
    CMTRT   = sas_trunc(sas_strip(MEDICATION), 50),
    CMDECOD = sas_trunc(ifelse(toupper(CMTRT) == "PARACETAMOL", "ACETAMINOPHEN", toupper(CMTRT)), 50),
    CMDOSE  = sas_input_best(sas_scan(DOSE, 1)),
    CMDOSU  = sas_trunc(sas_strip(sas_scan(DOSE, 2)), 20),
    .st     = sas_input_ddmmyy(START_DT),
    .en     = sas_input_ddmmyy(END_DT),
    CMSTDTC = ifelse(START_DT != "" & !is.na(.st),
                     format(as.Date(.st, origin = "1970-01-01"), "%Y-%m-%d"), ""),
    CMENDTC = ifelse(END_DT != "" & !is.na(.en),
                     format(as.Date(.en, origin = "1970-01-01"), "%Y-%m-%d"), ""),
    CMENRTPT = ifelse(ONGOING == "Y", "ONGOING", "")
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, CMTRT, CMDECOD, CMDOSE, CMDOSU,
         CMSTDTC, CMENDTC, CMENRTPT)

# ---------------------------------------------------------------- 4. sort
old_loc <- Sys.getlocale("LC_COLLATE")
invisible(Sys.setlocale("LC_COLLATE", "C"))
cm_draft <- cm_draft[order(cm_draft$USUBJID, cm_draft$CMSTDTC, cm_draft$CMTRT,
                           method = "radix"), ]

dm_dir <- file.path(project_path, "data/sdtm")
dm_file <- c(file.path(dm_dir, "dm.sas7bdat"), file.path(dm_dir, "dm.xpt"))
dm_file <- dm_file[file.exists(dm_file)][1]
if (is.na(dm_file)) stop("sdtm.dm not found in ", dm_dir)
dm <- if (grepl("xpt$", dm_file)) read_xpt(dm_file) else read_sas(dm_file)
names(dm) <- toupper(names(dm))
dm_cm_anchor <- dm %>%
  transmute(USUBJID = as.character(USUBJID),
            `_CMENTPT_ANCHOR` = sas_trunc(as.character(RFENDTC), 10))
dm_cm_anchor$`_CMENTPT_ANCHOR`[is.na(dm_cm_anchor$`_CMENTPT_ANCHOR`)] <- ""
dm_cm_anchor <- dm_cm_anchor[order(dm_cm_anchor$USUBJID, method = "radix"), ]
dm_cm_anchor <- dm_cm_anchor[!duplicated(dm_cm_anchor$USUBJID), ]

cm <- cm_draft %>%
  left_join(dm_cm_anchor, by = "USUBJID") %>%
  mutate(`_CMENTPT_ANCHOR` = ifelse(is.na(`_CMENTPT_ANCHOR`), "", `_CMENTPT_ANCHOR`),
         CMENTPT = ifelse(CMENRTPT == "ONGOING", `_CMENTPT_ANCHOR`, "")) %>%
  group_by(USUBJID) %>%
  mutate(CMSEQ = row_number()) %>%
  ungroup()

bad <- cm$CMENRTPT == "ONGOING" & cm$CMENTPT == ""
if (any(bad)) for (u in cm$USUBJID[bad])
  message("ERROR: Ongoing CM record has no DM reference end: USUBJID=", u)

cm <- cm %>%
  select(STUDYID, DOMAIN, USUBJID, CMSEQ, CMTRT, CMDECOD, CMDOSE, CMDOSU,
         CMSTDTC, CMENDTC, CMENRTPT, CMENTPT) %>%
  as.data.frame()
invisible(Sys.setlocale("LC_COLLATE", old_loc))

write.csv(cm, file.path(out, "cm.csv"), row.names = FALSE, na = "")
cat("cm rows:", nrow(cm), "\n")

# ---------------------------------------------------------------- 5. listing
lst <- head(cm, 15)
fmt_num <- function(x) ifelse(is.na(x), ".", formatC(x, digits = 12, format = "g", flag = ""))
lst_chr <- data.frame(Obs = as.character(seq_len(nrow(lst))), stringsAsFactors = FALSE)
for (v in names(lst)) {
  lst_chr[[v]] <- if (is.numeric(lst[[v]])) trimws(fmt_num(lst[[v]])) else lst[[v]]
}
write.csv(cbind(Obs = seq_len(nrow(lst)), lst), file.path(out, "P010.csv"),
          row.names = FALSE, na = "")

num_cols <- c("Obs", "CMSEQ", "CMDOSE")
widths <- sapply(names(lst_chr), function(v) max(nchar(v), nchar(lst_chr[[v]]), na.rm = TRUE))
pad <- function(s, w, right) if (right) formatC(s, width = w) else formatC(s, width = w, flag = "-")
hdr <- paste(mapply(function(v, w) pad(v, w, v %in% num_cols), names(lst_chr), widths), collapse = "  ")
rows <- apply(lst_chr, 1, function(r)
  paste(mapply(function(s, v, w) pad(s, w, v %in% num_cols), r, names(lst_chr), widths), collapse = "  "))
txt <- c("SDTM CM Domain - Concomitant Medications", "", hdr, "", rows)
writeLines(txt, file.path(out, "P010.txt"))
cat("Done.\n")

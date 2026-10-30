out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

get_ext <- function(p) {
  b <- basename(p)
  pos <- regexpr("\\.[^.]*$", b)
  ifelse(pos > 0, tolower(substr(b, pos + 1, nchar(b))), "")
}
base_noext <- function(p) {
  b <- basename(p)
  pos <- regexpr("\\.[^.]*$", b)
  ifelse(pos > 0, substr(b, 1, pos - 1), b)
}

read_sas_dataset <- function(dir, name) {
  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  ln <- tolower(name)
  cand <- files[tolower(base_noext(files)) == ln]
  if (length(cand) == 0) stop(paste0("dataset not found: ", dir, " / ", name))
  pick <- cand[1]
  sas <- cand[get_ext(cand) == "sas7bdat"]
  if (length(sas) > 0) pick <- sas[1]
  ext <- get_ext(pick)
  if (ext == "xpt") {
    df <- haven::read_xpt(pick)
  } else {
    df <- haven::read_sas(pick)
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  df
}

adsl <- read_sas_dataset(adam_dir, "adsl")
dv   <- read_sas_dataset(sdtm_dir, "dv")

stopifnot(all(c("USUBJID","TRTA","SITEID","RANDFL") %in% names(adsl)))
stopifnot(all(c("USUBJID","DVCAT","DVSCAT","DVTERM","DVSTDTC","DVENDTC","EPOCH","DVSEV") %in% names(dv)))

adsl_s <- adsl[, c("USUBJID","TRTA","SITEID","RANDFL"), drop = FALSE]
dv_s   <- dv[, c("USUBJID","DVCAT","DVSCAT","DVTERM","DVSTDTC","DVENDTC","EPOCH","DVSEV"), drop = FALSE]

to_chr <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%dT%H:%M:%S"))
  as.character(x)
}
adsl_s[] <- lapply(adsl_s, to_chr)
dv_s[] <- lapply(dv_s, to_chr)

merged <- merge(dv_s, adsl_s, by = "USUBJID", all = FALSE)
stopifnot(nrow(merged) > 0)
stopifnot(all(c("RANDFL","TRTA","SITEID") %in% names(merged)))

merged <- merged[merged$RANDFL == "Y" & !is.na(merged$RANDFL), , drop = FALSE]
stopifnot(nrow(merged) > 0)

dv_list <- data.frame(
  TRTA        = merged$TRTA,
  SITE        = merged$SITEID,
  USUBJID     = merged$USUBJID,
  CATEGORY    = merged$DVCAT,
  SUBCATEGORY = merged$DVSCAT,
  DESCRIPTION = merged$DVTERM,
  START_DATE  = merged$DVSTDTC,
  END_DATE    = merged$DVENDTC,
  EPOCH       = merged$EPOCH,
  SEVERITY    = merged$DVSEV,
  stringsAsFactors = FALSE
)

for (nm in names(dv_list)) {
  dv_list[[nm]][is.na(dv_list[[nm]])] <- ""
}

ord <- order(dv_list$TRTA, dv_list$SITE, dv_list$USUBJID, dv_list$START_DATE,
             method = "radix", na.last = FALSE)
dv_list <- dv_list[ord, , drop = FALSE]
rownames(dv_list) <- NULL

write.csv(dv_list, file.path(out, "l_dv.csv"), row.names = FALSE, na = "")

sponsor  <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate  <- "[DDMONYYYY]"
run_date <- toupper(format(Sys.Date(), "%d%b%Y"))
run_time <- format(Sys.time(), "%H:%M")
title4 <- "Listing 16.2.2"
title5 <- "Listing of Protocol Deviations"
title6 <- "All Subjects"
foot1 <- paste0("Data cutoff: ", cutdate, ".")
foot2 <- "Source: ADaM datasets"
foot3 <- paste0("Program: P078_l_dv.sas | Run: ", run_date, " ", run_time)

pad_left <- function(x, w) {
  x <- as.character(x)
  if (is.na(x)) x <- ""
  n <- nchar(x, type = "chars")
  if (n >= w) return(x)
  paste0(x, strrep(" ", w - n))
}
pad_center <- function(x, w) {
  x <- as.character(x)
  if (is.na(x)) x <- ""
  n <- nchar(x, type = "chars")
  if (n >= w) return(x)
  left <- floor((w - n) / 2)
  right <- w - n - left
  paste0(strrep(" ", left), x, strrep(" ", right))
}

cols <- list(
  list(nm="TRTA",        h1="Treatment", h2="",         w=14),
  list(nm="SITE",        h1="Site",      h2="",         w=8),
  list(nm="USUBJID",     h1="Subject",   h2="ID",       w=14),
  list(nm="CATEGORY",    h1="Deviation", h2="Category", w=16),
  list(nm="SUBCATEGORY", h1="Sub-",      h2="category", w=14),
  list(nm="DESCRIPTION", h1="Description", h2="",       w=32),
  list(nm="START_DATE",  h1="Start",     h2="Date",     w=12),
  list(nm="END_DATE",    h1="End",       h2="Date",     w=12),
  list(nm="EPOCH",       h1="Epoch",     h2="",         w=12),
  list(nm="SEVERITY",    h1="Severity",  h2="",         w=10)
)

disp <- dv_list
for (v in c("TRTA","SITE","USUBJID")) {
  for (i in seq_len(nrow(disp))) {
    if (i > 1 && disp[[v]][i] == disp[[v]][i-1]) {
      disp[[v]][i] <- ""
    }
  }
}

trs <- unique(dv_list$TRTA)
pages <- list()
for (t in trs) {
  idx <- which(dv_list$TRTA == t)
  chunks <- split(idx, ceiling(seq_along(idx)/45))
  for (ch in chunks) pages[[length(pages)+1]] <- ch
}
npages <- length(pages)
if (npages == 0) npages <- 1

header_h1 <- paste(sapply(cols, function(c) pad_center(c$h1, c$w)), collapse = " | ")
header_h2 <- paste(sapply(cols, function(c) pad_center(c$h2, c$w)), collapse = " | ")
sep_line  <- paste(sapply(cols, function(c) strrep("-", c$w)), collapse = "-+-")

lines <- c()
for (pg in seq_along(pages)) {
  idx <- pages[[pg]]
  padw <- 130
  pgstr <- paste0("Page ", pg, " of ", npages)
  sp <- max(1, padw - nchar(sponsor, type="chars") - nchar(pgstr, type="chars"))
  lines <- c(lines,
    paste0(sponsor, strrep(" ", sp), pgstr),
    paste0("Protocol: ", protocol),
    "",
    pad_center(title4, padw),
    pad_center(title5, padw),
    pad_center(title6, padw),
    ""
  )
  lines <- c(lines, header_h1, header_h2, sep_line)
  for (i in idx) {
    rowvals <- sapply(cols, function(c) pad_left(disp[i, c$nm], c$w))
    lines <- c(lines, paste(rowvals, collapse = " | "))
  }
  lines <- c(lines, "",
    foot1,
    foot2,
    foot3,
    ""
  )
  if (pg < npages) lines <- c(lines, paste0(strrep("-", 40), " NEW PAGE (Break after Treatment) ", strrep("-", 40)), "")
}

writeLines(lines, file.path(out, "l_dv.txt"), useBytes = TRUE)

rtf_path <- file.path(out, "l_dv.rtf")
esc <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\\{", "\\\\{", x)
  x <- gsub("\\}", "\\\\}", x)
  x
}
rtf_lines <- c("{\\rtf1\\ansi\\deff0",
  "{\\fonttbl{\\f0 Courier New;}}",
  "\\paperl16838\\paperw16838\\margl720\\margr720\\margt720\\margb720",
  "\\f0\\fs16",
  sapply(lines, function(l) paste0(esc(l), "\\par")),
  "}")
writeLines(rtf_lines, rtf_path, useBytes = TRUE)

cat("P078 l_dv completed:", nrow(dv_list), "rows,", npages, "pages\n")

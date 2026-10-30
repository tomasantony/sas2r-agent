```r
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

# Read a dataset by name, accepting either .sas7bdat or .xpt
read_ds <- function(dir, name) {
  cands <- c(file.path(dir, paste0(name, ".sas7bdat")),
             file.path(dir, paste0(toupper(name), ".sas7bdat")),
             file.path(dir, paste0(name, ".xpt")),
             file.path(dir, paste0(toupper(name), ".xpt")))
  f <- cands[file.exists(cands)][1]
  if (is.na(f)) stop("Dataset not found: ", name, " in ", dir)
  d <- if (grepl("\\.xpt$", f, ignore.case = TRUE)) read_xpt(f) else read_sas(f)
  names(d) <- toupper(names(d))
  d <- zap_labels(zap_formats(d))
  as.data.frame(d, stringsAsFactors = FALSE)
}

dv   <- read_ds(sdtm_dir, "dv")
adsl <- read_ds(adam_dir, "adsl")

# Make sure all referenced columns exist; SAS character missing is blank
chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
for (v in c("USUBJID","DVCAT","DVSCAT","DVTERM","DVSTDTC","DVENDTC","EPOCH","DVSEV"))
  if (!v %in% names(dv)) dv[[v]] <- ""
for (v in c("USUBJID","TRTA","SITEID","RANDFL"))
  if (!v %in% names(adsl)) adsl[[v]] <- ""

dv[]   <- lapply(dv, function(x) if (is.character(x)) chr(x) else x)
adsl[] <- lapply(adsl, function(x) if (is.character(x)) chr(x) else x)

cmpkey <- function(x) {
  if (is.numeric(x)) return(x)
  chr(x)
}

# ---- Step 1: PROC SQL inner join + order by ----
b <- adsl %>% filter(RANDFL == "Y") %>% select(USUBJID, TRTA, SITEID)
dv_list <- dv %>%
  inner_join(b, by = "USUBJID", relationship = "many-to-many") %>%
  transmute(USUBJID,
            TRTA,
            SITE        = SITEID,
            CATEGORY    = DVCAT,
            SUBCATEGORY = DVSCAT,
            DESCRIPTION = DVTERM,
            START_DATE  = DVSTDTC,
            END_DATE    = DVENDTC,
            EPOCH,
            SEVERITY    = DVSEV)

old_loc <- Sys.getlocale("LC_COLLATE")
invisible(Sys.setlocale("LC_COLLATE", "C"))
ord <- order(cmpkey(dv_list$TRTA), cmpkey(dv_list$SITE),
             cmpkey(dv_list$USUBJID), cmpkey(dv_list$START_DATE),
             method = "radix")
dv_list <- dv_list[ord, , drop = FALSE]

# ---- Step 2: PROC REPORT ----
# Order variables with missing values are excluded (no MISSING option)
rep <- dv_list
miss <- function(x) if (is.numeric(x)) is.na(x) else chr(x) == ""
rep <- rep[!(miss(rep$TRTA) | miss(rep$SITE) | miss(rep$USUBJID)), , drop = FALSE]
# Order by formatted values of order vars (stable within ties -> data order)
fmtv <- function(x) if (is.numeric(x)) x else chr(x)
ord2 <- order(fmtv(rep$TRTA), fmtv(rep$SITE), fmtv(rep$USUBJID), method = "radix")
rep <- rep[ord2, , drop = FALSE]
invisible(Sys.setlocale("LC_COLLATE", old_loc))

cols <- c("TRTA","SITE","USUBJID","CATEGORY","SUBCATEGORY","DESCRIPTION",
          "START_DATE","END_DATE","EPOCH","SEVERITY")
rep <- rep[, cols, drop = FALSE]
rep[] <- lapply(rep, function(x) if (is.numeric(x)) ifelse(is.na(x), "", format(x, trim = TRUE)) else chr(x))
rownames(rep) <- NULL

# CSV of table body (full values)
write.csv(rep, file.path(out, "l_dv.csv"), row.names = FALSE, na = "")

# Display version: order variables shown only at first occurrence of group
disp <- rep
if (nrow(disp) > 1) {
  newT <- c(TRUE, disp$TRTA[-1] != disp$TRTA[-nrow(disp)])
  newS <- newT | c(TRUE, disp$SITE[-1] != disp$SITE[-nrow(disp)])
  newU <- newS | c(TRUE, disp$USUBJID[-1] != disp$USUBJID[-nrow(disp)])
  disp$TRTA[!newT]    <- ""
  disp$SITE[!newS]    <- ""
  disp$USUBJID[!newU] <- ""
} else newT <- rep(TRUE, nrow(disp))

hdr <- c("Treatment","Site","Subject ID","Deviation Category","Sub-category",
         "Description","Start Date","End Date","Epoch","Severity")
widths <- pmax(nchar(hdr), sapply(cols, function(v) max(c(0, nchar(rep[[v]])))))
fmt_line <- function(v) paste(mapply(function(s, w) formatC(s, width = -w), v, widths), collapse = "  ")

titles <- c("[Sponsor Name]", "Protocol: [Protocol Number]", "",
            "Listing 16.2.2", "Listing of Protocol Deviations", "All Subjects", "")
foot <- c("", "Data cutoff: [DDMONYYYY]", "Source: ADaM datasets",
          paste0("Program: P078_l_dv.sas | Run: ", toupper(format(Sys.Date(), "%d%b%Y"))))

lines <- character(0)
page_starts <- which(newT)
if (length(page_starts) == 0) {
  lines <- c(titles, fmt_line(hdr), paste(rep("-", sum(widths) + 2 * (length(widths) - 1)), collapse = ""), foot)
} else {
  ends <- c(page_starts[-1] - 1, nrow(disp))
  for (p in seq_along(page_starts)) {
    lines <- c(lines, titles, fmt_line(hdr),
               paste(rep("-", sum(widths) + 2 * (length(widths) - 1)), collapse = ""))
    for (i in page_starts[p]:ends[p]) lines <- c(lines, fmt_line(unlist(disp[i, ])))
    lines <- c(lines, foot)
    if (p < length(page_starts)) lines <- c(lines, "\f")
  }
}
writeLines(lines, file.path(out, "l_dv.txt"))

cat("NOTE: l_dv completed successfully. Rows:", nrow(rep), "\n")
```
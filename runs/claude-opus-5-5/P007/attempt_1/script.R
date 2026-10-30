suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
adam_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam"

# Read a dataset from the ADaM library (sas7bdat preferred, xpt fallback)
read_lib <- function(name) {
  f1 <- file.path(adam_dir, paste0(name, ".sas7bdat"))
  f2 <- file.path(adam_dir, paste0(name, ".xpt"))
  if (file.exists(f1)) {
    d <- haven::read_sas(f1)
  } else if (file.exists(f2)) {
    d <- haven::read_xpt(f2)
  } else {
    stop("Cannot find dataset ", name)
  }
  names(d) <- toupper(names(d))
  d
}

# Convert SAS date variables to R Date if they are still numeric
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(as.numeric(x), origin = "1960-01-01"))
  if (is.character(x)) return(as.Date(substr(x, 1, 10)))
  x
}

message("Reading ADAE / ADSL")
adae <- read_lib("adae")
adsl <- read_lib("adsl")

for (v in c("TRTSDT", "TRTEDT", "EOSDT")) if (v %in% names(adsl)) adsl[[v]] <- to_date(adsl[[v]])
adae$AESTDT <- to_date(adae$AESTDT)

# 1. First treatment-emergent AE per subject (missing dates sort first, as in SAS)
adae_first <- adae %>%
  filter(!is.na(TRTEMFL) & TRTEMFL == "Y")
adae_first <- adae_first[order(adae_first$USUBJID, !is.na(adae_first$AESTDT),
                               adae_first$AESTDT, method = "radix"), ]

ae_target <- adae_first %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  select(USUBJID, EVENTDT = AESTDT)

# 2. Merge with ADSL
adsl_sorted <- adsl[order(adsl$USUBJID, method = "radix"), ]
if ("EVENTDT" %in% names(adsl_sorted)) adsl_sorted$EVENTDT <- NULL

m <- adsl_sorted %>%
  left_join(ae_target, by = "USUBJID") %>%
  filter(!is.na(SAFFL) & SAFFL == "Y")

m$PARAMCD <- "TTFAEV"
m$PARAM   <- "Time to First Adverse Event"

# 3. CNSR / AVAL
trts <- as.numeric(m$TRTSDT)
evt  <- as.numeric(m$EVENTDT)
eos  <- as.numeric(m$EOSDT)
m$CNSR <- ifelse(is.na(trts), NA_real_, ifelse(!is.na(evt), 0, 1))
m$AVAL <- ifelse(is.na(trts), NA_real_,
                 ifelse(!is.na(evt), (evt - trts) + 1,
                        ifelse(!is.na(eos), (eos - trts) + 1, NA_real_)))

# Variable order following SAS PDV: retained vars, ADSL vars, EVENTDT, CNSR, AVAL
keepv <- c("STUDYID", "USUBJID", "TRT01P", "TRT01A", "TRTSDT", "TRTEDT", "EOSDT",
           "PARAMCD", "PARAM", "CNSR", "AVAL", "EVENTDT")
ord <- c("STUDYID", "USUBJID", "PARAMCD", "PARAM",
         setdiff(names(adsl_sorted), c("STUDYID", "USUBJID", "PARAMCD", "PARAM")),
         "EVENTDT", "CNSR", "AVAL")
ord <- ord[ord %in% keepv]
adtte <- as.data.frame(m[, ord])

# Write dataset (dates as YYYY-MM-DD)
adtte_csv <- adtte
for (v in names(adtte_csv)) {
  if (inherits(adtte_csv[[v]], "Date")) adtte_csv[[v]] <- format(adtte_csv[[v]], "%Y-%m-%d")
  attr(adtte_csv[[v]], "label") <- NULL
  attr(adtte_csv[[v]], "format.sas") <- NULL
}
write.csv(adtte_csv, file.path(out, "adtte.csv"), row.names = FALSE, na = "")

# 4. Visual audit listing (first 15 obs)
fmt_date9 <- function(d) ifelse(is.na(d), ".", toupper(format(d, "%d%b%Y")))
fmt_num <- function(x) ifelse(is.na(x), ".", format(x, trim = TRUE))
n15 <- min(15, nrow(adtte))
lst <- adtte[seq_len(n15), , drop = FALSE]
listing <- data.frame(
  Obs     = seq_len(n15),
  USUBJID = as.character(lst$USUBJID),
  TRT01A  = as.character(lst$TRT01A),
  PARAM   = as.character(lst$PARAM),
  CNSR    = fmt_num(lst$CNSR),
  AVAL    = fmt_num(lst$AVAL),
  EVENTDT = fmt_date9(lst$EVENTDT),
  EOSDT   = fmt_date9(lst$EOSDT),
  stringsAsFactors = FALSE
)
write.csv(listing, file.path(out, "P007.csv"), row.names = FALSE, na = "")

# Plain text rendering
cols <- names(listing)
w <- sapply(cols, function(cn) max(nchar(cn), nchar(listing[[cn]]), na.rm = TRUE))
right <- c(Obs = TRUE, USUBJID = FALSE, TRT01A = FALSE, PARAM = FALSE,
           CNSR = TRUE, AVAL = TRUE, EVENTDT = TRUE, EOSDT = TRUE)
padf <- function(s, n, r) formatC(s, width = n, flag = if (r) "" else "-")
hdr <- paste(mapply(function(cn, n) padf(cn, n, right[cn]), cols, w), collapse = "  ")
rows <- apply(listing, 1, function(r) paste(mapply(function(val, cn, n) padf(val, n, right[cn]),
                                                  r, cols, w), collapse = "  "))
txt <- c("ADTTE Audit - Time to First Adverse Event", "", hdr, rows)
writeLines(txt, file.path(out, "P007.txt"))

message("Done: adtte rows = ", nrow(adtte))

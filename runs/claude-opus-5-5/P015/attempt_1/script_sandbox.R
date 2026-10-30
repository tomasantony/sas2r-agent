suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
project_path <- "/bench/data/derived/c2c"

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Approximates the SAS BEST12. format: shortest representation that fits 12 chars.
best12 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    if (v == round(v) && abs(v) < 1e12) return(format(v, scientific = FALSE, trim = TRUE))
    for (d in 12:1) {
      s <- format(signif(v, d), scientific = FALSE, trim = TRUE, digits = d)
      if (nchar(s) <= 12) return(s)
    }
    format(v, digits = 6, trim = TRUE)
  }, USE.NAMES = FALSE)
}

# Mimics PROC IMPORT type guessing: a column whose values all parse as numbers
# becomes numeric.
guess_type <- function(v) {
  vv <- str_trim(v)
  nonmiss <- vv[!is.na(vv) & vv != ""]
  if (length(nonmiss) > 0 && all(!is.na(suppressWarnings(as.numeric(nonmiss))))) {
    return(suppressWarnings(as.numeric(vv)))
  }
  vv[vv == ""] <- NA
  vv
}

# Approximates the ANYDTDTE. informat.
parse_any_date <- function(v) {
  if (inherits(v, "Date")) return(v)
  if (inherits(v, "POSIXt")) return(as.Date(v))
  if (is.numeric(v)) return(as.Date(v, origin = "1960-01-01"))
  s <- str_trim(as.character(v))
  d <- suppressWarnings(as.Date(parse_date_time(s, orders = c("Ymd", "mdY", "dmY", "dbY", "Ymd HMS", "mdY HMS"),
                                                quiet = TRUE)))
  d
}

# ---------------------------------------------------------------------------
# 1. IMPORT RAW LAB DATA
# ---------------------------------------------------------------------------
raw_lb <- read_csv(file.path(project_path, "data/raw/raw_lab.csv"),
                   col_types = cols(.default = col_character()),
                   show_col_types = FALSE)
names(raw_lb) <- toupper(names(raw_lb))

# LAB_DAT is kept as character so that it can be parsed as a date below.
for (nm in setdiff(names(raw_lb), "LAB_DAT")) {
  raw_lb[[nm]] <- guess_type(raw_lb[[nm]])
}

# ---------------------------------------------------------------------------
# 2. CORE TRANSFORMATION
# ---------------------------------------------------------------------------
subj_chr <- if (is.numeric(raw_lb$SUBJ)) best12(raw_lb$SUBJ) else str_trim(raw_lb$SUBJ)
subj_chr[is.na(raw_lb$SUBJ)] <- NA

STUDYID <- "CDISC-01"
USUBJID <- ifelse(is.na(subj_chr) | subj_chr == "", STUDYID, paste(STUDYID, subj_chr, sep = "-"))
USUBJID <- substr(USUBJID, 1, 40)

tn <- toupper(str_trim(as.character(raw_lb$TEST_NAME)))
tn[is.na(tn)] <- ""

LBTESTCD <- case_when(tn == "GLUCOSE" ~ "GLUC",
                      tn == "HEMOGLOBIN" ~ "HGB",
                      tn == "ALT" ~ "ALT",
                      tn == "AST" ~ "AST",
                      TRUE ~ "UNKNOWN")
LBTEST <- case_when(tn == "GLUCOSE" ~ "Glucose",
                    tn == "HEMOGLOBIN" ~ "Hemoglobin",
                    tn == "ALT" ~ "Alanine Aminotransferase",
                    tn == "AST" ~ "Aspartate Aminotransferase",
                    TRUE ~ substr(as.character(raw_lb$TEST_NAME), 1, 40))
LBCAT <- case_when(tn %in% c("GLUCOSE", "ALT", "AST") ~ "CHEMISTRY",
                   tn == "HEMOGLOBIN" ~ "HEMATOLOGY",
                   TRUE ~ "UNKNOWN")

res <- suppressWarnings(as.numeric(raw_lb$RESULT))
LBORRES <- substr(str_trim(best12(res)), 1, 20)
LBSTRESN <- res
LBSTRESC <- LBORRES

LBORRESU <- substr(str_trim(as.character(raw_lb$UNIT)), 1, 20)
LBORRESU[is.na(LBORRESU)] <- ""
uu <- toupper(LBORRESU)
LBSTRESU <- ifelse(uu == "MG/DL", "mg/dL",
            ifelse(uu == "G/DL", "g/dL",
            ifelse(uu == "U/L", "U/L", LBORRESU)))

labd <- parse_any_date(raw_lb$LAB_DAT)
LBDTC <- ifelse(is.na(labd), "", format(labd, "%Y-%m-%d"))

visit <- str_trim(as.character(raw_lb$VISIT_NAM))
visit[is.na(visit)] <- ""

lb_mapped <- data.frame(
  STUDYID = STUDYID, DOMAIN = "LB", USUBJID = USUBJID,
  LBTESTCD = LBTESTCD, LBTEST = LBTEST, LBCAT = LBCAT,
  LBORRES = LBORRES, LBORRESU = LBORRESU, LBSTRESC = LBSTRESC, LBSTRESU = LBSTRESU,
  VISIT = visit, VISITNUM = suppressWarnings(as.numeric(raw_lb$VISIT_NUM)),
  LBSTRESN = LBSTRESN, LBDTC = LBDTC,
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# 3. SORT AND DERIVE LBSEQ
# ---------------------------------------------------------------------------
# Radix ordering gives byte (C-locale) collation, matching SAS character sorting.
ord <- order(lb_mapped$USUBJID, lb_mapped$LBDTC, lb_mapped$LBTESTCD, method = "radix")
lb_mapped <- lb_mapped[ord, ]

lb_mapped$LBSEQ <- ave(seq_len(nrow(lb_mapped)), lb_mapped$USUBJID, FUN = seq_along)

lb <- lb_mapped[, c("STUDYID", "DOMAIN", "USUBJID", "LBSEQ", "LBTESTCD", "LBTEST", "LBCAT",
                    "LBORRES", "LBORRESU", "LBSTRESC", "LBSTRESU", "VISIT", "VISITNUM",
                    "LBSTRESN", "LBDTC")]
rownames(lb) <- NULL

write.csv(lb, file.path(out, "lb.csv"), row.names = FALSE, na = "")

# ---------------------------------------------------------------------------
# 4. VISUAL AUDIT (PROC PRINT obs=12)
# ---------------------------------------------------------------------------
pr <- head(lb, 12)[, c("USUBJID", "VISITNUM", "VISIT", "LBTESTCD", "LBCAT", "LBSTRESN", "LBSTRESU", "LBDTC")]
pr <- cbind(Obs = seq_len(nrow(pr)), pr)

write.csv(pr, file.path(out, "P015.csv"), row.names = FALSE, na = "")

pc <- pr
pc$VISITNUM <- ifelse(is.na(pc$VISITNUM), ".", best12(pc$VISITNUM))
pc$LBSTRESN <- ifelse(is.na(pc$LBSTRESN), ".", best12(pc$LBSTRESN))
pc[] <- lapply(pc, as.character)

w <- pmax(nchar(names(pc)), sapply(pc, function(x) max(nchar(x), 0)))
fmt_row <- function(v) paste(mapply(function(s, wi) formatC(s, width = wi), v, w), collapse = "  ")

lines <- c("LB Domain Audit (Laboratory Test Results)", "",
           fmt_row(names(pc)), "",
           apply(pc, 1, fmt_row))
writeLines(lines, file.path(out, "P015.txt"))

cat("P015 done:", nrow(lb), "LB records\n")

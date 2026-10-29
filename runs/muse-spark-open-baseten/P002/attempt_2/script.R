out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "output"
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(purrr)
  library(tibble)
})

round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

find_xpt <- function(rel_win_path) {
  candidates <- c(
    rel_win_path,
    sub("^D:", "", rel_win_path),
    sub("^D:/meta_hackathon/sas2r-agent", "/bench", rel_win_path),
    sub("^D:/meta_hackathon/sas2r-agent", "/meta_hackathon/sas2r-agent", rel_win_path),
    sub("^D:/meta_hackathon/sas2r-agent", ".", rel_win_path),
    file.path(".", sub("^D:/meta_hackathon/sas2r-agent/", "", rel_win_path)),
    file.path(getwd(), sub("^D:/meta_hackathon/sas2r-agent/", "", rel_win_path)),
    file.path("/bench", sub("^D:/meta_hackathon/sas2r-agent/", "", rel_win_path))
  )
  for (p in candidates) {
    if (file.exists(p)) return(p)
  }
  bn <- basename(rel_win_path)
  search_roots <- c(".", "/bench", "/meta_hackathon", "/tmp", getwd())
  for (r in search_roots) {
    if (dir.exists(r)) {
      found <- list.files(r, pattern = paste0("^", bn, "$"), recursive = TRUE, full.names = TRUE)
      if (length(found) > 0) return(found[1])
    }
  }
  return(rel_win_path)
}

read_xpt_safe <- function(path) {
  p <- find_xpt(path)
  if (!file.exists(p)) {
    stop(paste0("File not found: ", path, " (tried ", p, ")"))
  }
  df <- haven::read_xpt(p)
  df <- haven::zap_formats(df)
  df <- haven::zap_labels(df)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  df
}

sas_yymmdd10 <- function(x) {
  x <- trimws(as.character(x))
  res <- rep(as.Date(NA), length(x))
  idx <- !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  if (any(idx)) {
    res[idx] <- suppressWarnings(as.Date(x[idx], format = "%Y-%m-%d"))
  }
  res
}

convert_sas_date <- function(col) {
  if (inherits(col, "Date")) return(col)
  if (is.numeric(col)) {
    return(as.Date(col, origin = "1960-01-01"))
  }
  if (is.character(col)) {
    return(sas_yymmdd10(col))
  }
  col
}

ae_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/ae.xpt"
adsl_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt"

AE_raw <- read_xpt_safe(ae_path)
ADSL_raw <- read_xpt_safe(adsl_path)

if ("TRTSDT" %in% names(ADSL_raw)) ADSL_raw$TRTSDT <- convert_sas_date(ADSL_raw$TRTSDT)
if ("TRTEDT" %in% names(ADSL_raw)) ADSL_raw$TRTEDT <- convert_sas_date(ADSL_raw$TRTEDT)

AE <- AE_raw %>%
  arrange(USUBJID, AESEQ)

AE$ASTDT <- sas_yymmdd10(trimws(AE$AESTDTC))
AE$AENDT <- sas_yymmdd10(trimws(AE$AEENDTC))
AE$ADUR <- ifelse(!is.na(AE$ASTDT) & !is.na(AE$AENDT),
                  as.numeric(AE$AENDT - AE$ASTDT) + 1,
                  NA_real_)

TEMP <- ADSL_raw %>%
  select(any_of(c("SITEID","USUBJID","TRT01A","TRT01AN","AGE","AGEGR1","AGEGR1N","RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT"))) %>%
  rename(TRTA = TRT01A, TRTAN = TRT01AN) %>%
  arrange(USUBJID)

AE <- AE %>%
  left_join(TEMP, by = "USUBJID")

AE$ASTDTF <- NA_character_
AE$CQ01NAM <- NA_character_

idx_imp <- is.na(AE$ASTDT) & !is.na(AE$AESTDTC) & nchar(trimws(AE$AESTDTC)) == 7
if (any(idx_imp, na.rm = TRUE)) {
  to_parse <- paste0(gsub(" ", "", trimws(AE$AESTDTC[idx_imp])), "-01")
  AE$ASTDT[idx_imp] <- sas_yymmdd10(to_parse)
  AE$ASTDTF[idx_imp] <- "D"
}

AE$ASTDY <- NA_real_
AE$AENDY <- NA_real_
AE$ANLSTDY <- NA_real_

valid_ast_trt <- !is.na(AE$ASTDT) & !is.na(AE$TRTSDT)
AE$ASTDY[valid_ast_trt & AE$ASTDT >= AE$TRTSDT] <- as.numeric(AE$ASTDT[valid_ast_trt & AE$ASTDT >= AE$TRTSDT] - AE$TRTSDT[valid_ast_trt & AE$ASTDT >= AE$TRTSDT]) + 1
AE$ASTDY[valid_ast_trt & AE$TRTSDT > AE$ASTDT] <- as.numeric(AE$ASTDT[valid_ast_trt & AE$TRTSDT > AE$ASTDT] - AE$TRTSDT[valid_ast_trt & AE$TRTSDT > AE$ASTDT])

valid_aen_trt <- !is.na(AE$AENDT) & !is.na(AE$TRTSDT)
AE$AENDY[valid_aen_trt & AE$AENDT >= AE$TRTSDT] <- as.numeric(AE$AENDT[valid_aen_trt & AE$AENDT >= AE$TRTSDT] - AE$TRTSDT[valid_aen_trt & AE$AENDT >= AE$TRTSDT]) + 1
AE$AENDY[valid_aen_trt & AE$TRTSDT > AE$AENDT] <- as.numeric(AE$AENDT[valid_aen_trt & AE$TRTSDT > AE$AENDT] - AE$TRTSDT[valid_aen_trt & AE$TRTSDT > AE$AENDT])

AE$ANLSTDY[valid_ast_trt] <- as.numeric(AE$ASTDT[valid_ast_trt] - AE$TRTSDT[valid_ast_trt]) + 1

AE$TRTEMFL <- ifelse(!is.na(AE$ASTDT) & !is.na(AE$TRTSDT) & AE$ASTDT >= AE$TRTSDT, "Y", "N")

has_app <- grepl("APPLICATION", AE$AEDECOD, fixed = TRUE)
has_derm <- grepl("DERMATITIS", AE$AEDECOD, fixed = TRUE)
has_ery <- grepl("ERYTHEMA", AE$AEDECOD, fixed = TRUE)
has_blis <- grepl("BLISTER", AE$AEDECOD, fixed = TRUE)
has_app[is.na(has_app)] <- FALSE
has_derm[is.na(has_derm)] <- FALSE
has_ery[is.na(has_ery)] <- FALSE
has_blis[is.na(has_blis)] <- FALSE

cond_skin <- AE$AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" & !(AE$AEDECOD %in% c("COLD SWEAT","HYPERHIDROSIS","ALOPECIA"))
cond_skin[is.na(cond_skin)] <- FALSE

idx_cq <- has_app | has_derm | has_ery | has_blis | cond_skin
AE$CQ01NAM[idx_cq] <- "DERMATOLOGIC EVENTS"

flagfrst <- function(inds, sortvars, invar, outvar, ser = NULL, derm = NULL) {
  df <- inds %>% filter(TRTEMFL == "Y")
  if (!is.null(ser) && ser == "Y") {
    df <- df %>% filter(AESER == "Y")
  }
  if (!is.null(derm) && derm == "Y") {
    df <- df %>% filter(CQ01NAM == "DERMATOLOGIC EVENTS")
  }
  if (nrow(df) == 0) {
    empty <- tibble(USUBJID = character(), AESEQ = numeric())
    empty[[outvar]] <- character()
    return(empty %>% arrange(USUBJID, AESEQ))
  }
  sort_cols <- c("USUBJID", sortvars, "ASTDT")
  sort_cols <- sort_cols[sort_cols != "" & !is.na(sort_cols)]
  ord_list <- lapply(sort_cols, function(col) {
    v <- df[[col]]
    if (is.character(v)) v[is.na(v)] <- ""
    v
  })
  ord <- do.call(order, c(ord_list, list(na.last = FALSE)))
  df <- df[ord, , drop = FALSE]

  if (is.null(invar) || invar == "" || is.na(invar)) {
    grp_vars <- "USUBJID"
  } else {
    grp_vars <- c("USUBJID", sortvars)
    if (!(invar %in% grp_vars)) grp_vars <- c(grp_vars, invar)
    grp_vars <- unique(grp_vars[grp_vars != "" & !is.na(grp_vars)])
  }

  df2 <- df %>%
    group_by(across(all_of(grp_vars))) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(USUBJID, AESEQ) %>%
    mutate(!!outvar := "Y") %>%
    arrange(USUBJID, AESEQ)
  df2
}

aea <- flagfrst(inds = AE, sortvars = character(0), invar = "", outvar = "AOCCFL", ser = NULL, derm = NULL)
aeb <- flagfrst(inds = AE, sortvars = c("AEBODSYS"), invar = "AEBODSYS", outvar = "AOCCSFL", ser = NULL, derm = NULL)
aec <- flagfrst(inds = AE, sortvars = c("AEBODSYS","AEDECOD"), invar = "AEDECOD", outvar = "AOCCPFL", ser = NULL, derm = NULL)
saea <- flagfrst(inds = AE, sortvars = character(0), invar = "", outvar = "AOCC02FL", ser = "Y", derm = NULL)
saeb <- flagfrst(inds = AE, sortvars = c("AEBODSYS"), invar = "AEBODSYS", outvar = "AOCC03FL", ser = "Y", derm = NULL)
saec <- flagfrst(inds = AE, sortvars = c("AEBODSYS","AEDECOD"), invar = "AEDECOD", outvar = "AOCC04FL", ser = "Y", derm = NULL)
derm <- flagfrst(inds = AE, sortvars = character(0), invar = "", outvar = "AOCC01FL", ser = NULL, derm = "Y")

AE <- AE %>% arrange(USUBJID, AESEQ)

AE <- AE %>%
  left_join(aea, by = c("USUBJID","AESEQ")) %>%
  left_join(aeb, by = c("USUBJID","AESEQ")) %>%
  left_join(aec, by = c("USUBJID","AESEQ")) %>%
  left_join(saea, by = c("USUBJID","AESEQ")) %>%
  left_join(saeb, by = c("USUBJID","AESEQ")) %>%
  left_join(saec, by = c("USUBJID","AESEQ")) %>%
  left_join(derm, by = c("USUBJID","AESEQ"))

all_names <- names(AE)
keep_exact <- c("STUDYID","SITEID","USUBJID","TRTAN","TRTA","AESEQ","AETERM","AEDECOD","AEPTCD","AEBODSYS","AESEV","AESER","AEACN","AEREL","AEOUT","AESCAN","AESCONG","AESDISAB","AESDTH","AESHOSP","AESLIFE","AESOD","TRTEMFL","ASTDT","AENDT","ASTDTF","CQ01NAM","ASTDY","AENDY","AOCCFL","AOCCSFL","AOCCPFL","AOCC01FL","AOCC02FL","AOCC03FL","AOCC04FL","AGE","AGEGR1","AGEGR1N","RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT","ADUR")
wildcard_prefixes <- c("AELLT","AEHL","AESOC")
wildcard_cols <- unlist(lapply(wildcard_prefixes, function(p) all_names[startsWith(all_names, p)]))
keep_vars <- unique(c(keep_exact, wildcard_cols))
retain_order <- c("STUDYID","SITEID","USUBJID","TRTA","TRTAN","AGE","AGEGR1","AGEGR1N","RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT","ASTDT","ASTDTF","ASTDY","AENDT","AENDY","ADUR","AETERM","AELLT","AELLTCD","AEDECOD","AEPTCD","AEHLT","AEHLTCD","AEHLGT","AEHLGTCD","AEBODSYS","AESOC","AESOCCD","AESEV","AESER","AESCAN","AESCONG","AESDISAB","AESDTH","AESHOSP","AESLIFE","AESOD","AEREL","AEACN","AEOUT","AESEQ","TRTEMFL","AOCCFL","AOCCSFL","AOCCPFL","AOCC02FL","AOCC03FL","AOCC04FL","CQ01NAM","AOCC01FL")
ordered_keep <- c()
for (v in retain_order) {
  if (v %in% keep_vars) ordered_keep <- c(ordered_keep, v)
}
for (v in keep_vars) {
  if (!(v %in% ordered_keep)) ordered_keep <- c(ordered_keep, v)
}
ordered_keep <- ordered_keep[ordered_keep %in% names(AE)]
AE_final <- AE %>% select(all_of(ordered_keep)) %>% arrange(USUBJID, AESEQ)

ADAE <- AE_final

date_cols <- c("TRTSDT","TRTEDT","ASTDT","AENDT")
for (dc in date_cols) {
  if (dc %in% names(ADAE)) {
    if (!inherits(ADAE[[dc]], "Date")) {
      ADAE[[dc]] <- convert_sas_date(ADAE[[dc]])
    }
  }
}

ADAE_out <- ADAE
for (dc in date_cols) {
  if (dc %in% names(ADAE_out)) {
    ADAE_out[[dc]] <- format(ADAE_out[[dc]], "%Y-%m-%d")
    ADAE_out[[dc]][ADAE_out[[dc]] == "NA"] <- NA
  }
}

if (!dir.exists(out)) dir.create(out, recursive = TRUE)
write.csv(ADAE_out, file.path(out, "adae.csv"), row.names = FALSE, na = "")

sink(file.path(out, "adae.txt"))
cat("Adverse Events Analysis Dataset (ADAE)\n")
cat("Label: Adverse Events Analysis Dataset\n")
cat(paste0("Observations: ", nrow(ADAE), "\n"))
cat(paste0("Variables: ", ncol(ADAE), "\n\n"))
cat("Variables in order:\n")
for (i in seq_along(names(ADAE))) {
  v <- names(ADAE)[i]
  cls <- class(ADAE[[v]])[1]
  cat(sprintf("%3d  %-12s  %s\n", i, v, cls))
}
cat("\nFirst 5 rows (formatted):\n")
print(head(ADAE_out, 5))
sink()

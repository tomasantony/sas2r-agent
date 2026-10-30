```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x) * mult
  rounded_abs <- floor(abs_x + 0.5 + 1e-10)
  sign_x * rounded_abs / mult
}
round_half_up_vec <- function(x, digits = 0) {
  vapply(x, function(v) round_half_up(v, digits), numeric(1))
}
sas_put_cnt <- function(cnt) {
  cnt_r <- round_half_up_vec(cnt, 0)
  sprintf("%3.0f", cnt_r)
}
sas_put_per <- function(per) {
  per_r <- round_half_up_vec(per, 1)
  sprintf("%5.1f", per_r)
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
})

rawadam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
inlib_path   <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
adsl_xpt_rawadam <- file.path(rawadam_path, "adsl.xpt")
adsl_xpt_inlib   <- file.path(inlib_path, "adsl.xpt")
adsl_file <- if (file.exists(adsl_xpt_rawadam)) adsl_xpt_rawadam else adsl_xpt_inlib

adsl_raw <- haven::read_xpt(adsl_file)
adsl_conv <- adsl_raw %>%
  mutate(across(where(~ inherits(., "haven_labelled")), ~ haven::as_factor(., levels = "labels")))
names(adsl_conv) <- tolower(names(adsl_conv))
req_cols <- c("usubjid","trt01an","trt01a","dcreascd","ittfl")
missing_cols <- setdiff(req_cols, names(adsl_conv))
if (length(missing_cols) > 0) stop(paste("Missing columns in ADSL:", paste(missing_cols, collapse=", ")))

adsl_conv <- adsl_conv %>%
  mutate(
    usubjid = as.character(usubjid),
    trt01a = as.character(trt01a),
    dcreascd = as.character(dcreascd),
    ittfl = as.character(ittfl),
    trt01an = as.numeric(as.character(trt01an))
  )

adsl <- adsl_conv %>%
  filter(ittfl == "Y") %>%
  select(usubjid, trt01an, trt01a, dcreascd, ittfl)

adsl_total <- adsl %>%
  mutate(trt01an = 999, trt01a = "Total")
adsl1 <- bind_rows(adsl, adsl_total)

tot <- adsl1 %>%
  group_by(trt01an) %>%
  summarise(tot = n_distinct(usubjid), .groups = "drop") %>%
  arrange(trt01an)

adsl1 <- adsl1 %>%
  mutate(
    dcreascd_up = ifelse(is.na(dcreascd), "", toupper(dcreascd)),
    dcreascd_char = ifelse(is.na(dcreascd), "", as.character(dcreascd))
  )

dummy <- tibble(
  col1 = c("Completed the study","Discontinued"," ",
           "  Death","  Adverse Event","  Lack of Efficacy","  Lost to Follow-up",
           "  Non-compliance with Study Drug","  Pregnancy","  Protocal Violation",
           "  Physician Decision","  Withdrawl by Subject","  Withdrawl by Parent/Guardian",
           "  Recovery","  Technical Problems","  Other"),
  ord = 1:16
)

disp_func <- function(where_filter, var_value = NULL, var_col = NULL) {
  filtered <- adsl1 %>% filter(where_filter(.))
  if (nrow(filtered) == 0) {
    return(tibble(cnt=integer(0), per=numeric(0), col1=character(0), trt01an=numeric(0), trt01a=character(0)))
  }
  if (!is.null(var_value)) {
    filtered <- filtered %>% mutate(col1 = var_value)
  } else if (!is.null(var_col)) {
    filtered <- filtered %>% mutate(col1 = .data[[var_col]])
  } else {
    stop("var_value or var_col must be supplied")
  }
  filtered %>%
    left_join(tot, by = "trt01an") %>%
    group_by(trt01an, trt01a, tot, col1) %>%
    summarise(cnt = n_distinct(usubjid), .groups = "drop") %>%
    mutate(per = (cnt * 100) / tot) %>%
    select(cnt, per, col1, trt01an, trt01a)
}

comp <- disp_func(
  where_filter = function(df) df$dcreascd_up == "COMPLETED",
  var_value = "Completed the study"
)
disc <- disp_func(
  where_filter = function(df) df$dcreascd_up != "COMPLETED",
  var_value = "Discontinued"
)
all_tbl <- disp_func(
  where_filter = function(df) df$dcreascd_up != "COMPLETED",
  var_col = "dcreascd_char"
)

# SAS semantics: data outds.disp; set comp disc all(in=a); by trt01an trt01a;
# Length of COL1 is determined by first dataset (comp) where COL1 = "Completed the study" => length 19.
# Therefore when prefixing "  " to values from 'all', SAS truncates to 19 characters.
sas_col1_len <- nchar("Completed the study")  # 19

# Apply prefix and SAS truncation for 'all' rows
all_prefixed <- all_tbl %>%
  mutate(col1 = paste0("  ", trimws(col1, which = "both"))) %>%
  mutate(col1 = substr(col1, 1, sas_col1_len))

# Also ensure comp/disc col1 truncated to same length (no effect but faithful)
comp <- comp %>% mutate(col1 = substr(col1, 1, sas_col1_len))
disc <- disc %>% mutate(col1 = substr(col1, 1, sas_col1_len))

comp <- comp %>% mutate(.src = 1)
disc <- disc %>% mutate(.src = 2)
all_prefixed <- all_prefixed %>% mutate(.src = 3)

disp_final <- bind_rows(comp, disc, all_prefixed) %>%
  mutate(nper = paste0(sas_put_cnt(cnt), "(", sas_put_per(per), ")")) %>%
  arrange(trt01an, .src, col1) %>%
  select(cnt, per, col1, trt01an, trt01a, nper)

write.csv(disp_final, file.path(out, "disp.csv"), row.names = FALSE, na = "")

txt_path <- file.path(out, "disp.txt")
sink(txt_path)
cat("Subject Disposition (disp)\n")
cat("========================================\n\n")
cat("Header N counts (tot):\n")
print(tot, n = Inf)
cat("\n")
cat("Disposition table:\n")
print(as.data.frame(disp_final), row.names = FALSE, right = FALSE)
sink()

cat("Done. Wrote", file.path(out, "disp.csv"), "\n")
```
# P041 - Demography table (translation of P041_t-demog.sas)
suppressPackageStartupMessages({
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
rawdir <- "/bench/data/raw"

# ---------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------

# SAS rounds half away from zero; R's round() does not.
round_half_up <- function(x, d = 0) {
  m <- 10^d
  sign(x) * floor(abs(x) * m + 0.5) / m
}

# put(x, 3.): right-aligned width 3, missing shown as "."
put3 <- function(x) {
  ifelse(is.na(x), "  .", formatC(round_half_up(x), width = 3, format = "d"))
}

fmt_age <- c("1" = "  18-19", "2" = "  20-29", "3" = "  30-39",
             "4" = "  40-49", "5" = "  50-59", "6" = "  >=60 ")
fmt_sex <- c("1" = "Male  ", "2" = "Female")

# ---------------------------------------------------------------
# Bring in data
# List input: delimiter="," without DSD, so consecutive
# delimiters count as one; MISSOVER leaves trailing fields missing.
# ---------------------------------------------------------------
lines <- readLines(file.path(rawdir, "phuse-mcsl/mcsl.csv"), warn = FALSE)
lines <- lines[-1]
vars <- c("studyid", "usubjid", "trt01a", "trt01an", "age", "ageu", "race", "racen",
          "sex", "saffl", "efffl", "bmibl", "heightbl", "weightbl", "dcdecod", "dcreascd")
numv <- c("trt01an", "age", "racen", "bmibl", "heightbl", "weightbl")
lens <- c(studyid = 20, usubjid = 20, trt01a = 20, ageu = 10, race = 25, sex = 1,
          saffl = 1, efffl = 1, dcdecod = 50, dcreascd = 50)

rows <- lapply(lines, function(l) {
  tk <- strsplit(l, ",", fixed = TRUE)[[1]]
  tk <- tk[tk != ""]
  tk <- trimws(tk, which = "left")
  length(tk) <- length(vars)
  tk
})
mat <- do.call(rbind, rows)
mcsl <- as.data.frame(mat, stringsAsFactors = FALSE)
names(mcsl) <- vars

for (v in numv) mcsl[[v]] <- suppressWarnings(as.numeric(mcsl[[v]]))
for (v in names(lens)) {
  x <- mcsl[[v]]
  x[is.na(x)] <- ""
  mcsl[[v]] <- substr(x, 1, lens[[v]])
}

mcsl$sexn <- ifelse(mcsl$sex == "M", 1, ifelse(mcsl$sex == "F", 2, NA))

# ---------------------------------------------------------------
# Categorize age (missing / under-18 ages are deleted)
# ---------------------------------------------------------------
age1 <- mcsl %>%
  mutate(agecat = case_when(
    !is.na(age) & age >= 18 & age <= 19 ~ 1,
    !is.na(age) & age >= 20 & age <= 29 ~ 2,
    !is.na(age) & age >= 30 & age <= 39 ~ 3,
    !is.na(age) & age >= 40 & age <= 49 ~ 4,
    !is.na(age) & age >= 50 & age <= 59 ~ 5,
    !is.na(age) & age >= 60 ~ 6,
    TRUE ~ NA_real_)) %>%
  filter(!is.na(agecat)) %>%
  select(trt01an, usubjid, age, agecat)

agenums <- age1 %>% group_by(trt01an, agecat) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop") %>%
  mutate(cat = 1, subcat = agecat)
agetots <- age1 %>% group_by(agecat) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop") %>%
  mutate(cat = 1, subcat = agecat, trt01an = 99)
agecat <- bind_rows(agenums, agetots) %>%
  mutate(text = paste0("  ", fmt_age[as.character(agecat)]))

# Equivalent of PROC TRANSPOSE with prefix=trt, id trt01an
transp <- function(d, key) {
  d %>%
    mutate(idn = paste0("trt", trt01an)) %>%
    group_by(cat, subcat, .data[[key]], text) %>%
    summarise(trt0  = sum(num[idn == "trt0"]  %>% {if (length(.) == 0) NA else .}),
              trt54 = sum(num[idn == "trt54"] %>% {if (length(.) == 0) NA else .}),
              trt81 = sum(num[idn == "trt81"] %>% {if (length(.) == 0) NA else .}),
              trt99 = sum(num[idn == "trt99"] %>% {if (length(.) == 0) NA else .}),
              .groups = "drop") %>%
    arrange(cat, subcat, desc(.data[[key]])) %>%
    select(-all_of(key))
}
age <- transp(agecat, "agecat")

# ---------------------------------------------------------------
# Gender
# ---------------------------------------------------------------
sexg <- mcsl %>% filter(sex != "") %>% group_by(trt01an, sexn) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop") %>%
  mutate(cat = 2, subcat = sexn)
sext <- mcsl %>% filter(!is.na(sexn)) %>% group_by(sexn) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop") %>%
  mutate(cat = 2, subcat = sexn, trt01an = 99)
sexcat <- bind_rows(sexg, sext) %>%
  mutate(text = paste0("  ", ifelse(is.na(sexn), ".", fmt_sex[as.character(sexn)])))
sexcat$text <- substr(sexcat$text, 1, 8)
sex <- transp(sexcat, "sexn")

# ---------------------------------------------------------------
# Bring the data together (header row per category)
# ---------------------------------------------------------------
comb <- bind_rows(age, sex) %>%
  mutate(text = substr(sub("\\s+$", "", text), 1, 9))
comb$col1 <- put3(comb$trt0); comb$col2 <- put3(comb$trt54)
comb$col3 <- put3(comb$trt81); comb$col4 <- put3(comb$trt99)

res <- list()
firstcat <- !duplicated(comb$cat)
for (i in seq_len(nrow(comb))) {
  r <- comb[i, ]
  res[[length(res) + 1]] <- r
  if (firstcat[i]) {
    h <- r
    h$col1 <- ""; h$col2 <- ""; h$col3 <- ""; h$col4 <- ""
    h$subcat <- 0
    h$text <- if (h$cat == 1) "Age" else if (h$cat == 2) "Gender" else h$text
    res[[length(res) + 1]] <- h
  }
}
final <- bind_rows(res)
final <- final[order(final$cat, final$subcat), ]  # stable sort

body <- final %>% select(cat, subcat, text, col1, col2, col3, col4)
write.csv(body, file.path(out, "P041.csv"), row.names = FALSE, na = "")

# ---------------------------------------------------------------
# Report (text rendering)
# ---------------------------------------------------------------
padr <- function(x, w) formatC(substr(ifelse(is.na(x), "", x), 1, w), width = -w)
hdr  <- paste(padr("", 30), padr("Group 1", 12), padr("Group 2", 12),
              padr("Group 3", 12), padr("Total", 12), sep = "  ")
lsz  <- nchar(hdr)
center <- function(s) paste0(strrep(" ", max(0, (lsz - nchar(s)) %/% 2)), s)

txt <- c(center("Demography"), "", hdr, strrep("-", lsz), "")
cats <- unique(body$cat)
for (cc in cats) {
  b <- body[body$cat == cc, ]
  for (i in seq_len(nrow(b))) {
    txt <- c(txt, paste(padr(b$text[i], 30), padr(b$col1[i], 12), padr(b$col2[i], 12),
                        padr(b$col3[i], 12), padr(b$col4[i], 12), sep = "  "))
  }
  txt <- c(txt, "")
}
txt <- c(txt, "", "NOTE: Numbers in parentheses are percentages")
writeLines(txt, file.path(out, "P041.txt"))

message("P041 done: ", nrow(body), " rows written")

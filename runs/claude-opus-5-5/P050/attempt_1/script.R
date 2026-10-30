suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

# SAS-style rounding: half away from zero
round_half_up <- function(x, d = 0) {
  m <- 10^d
  sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m
}

# SAS put(x, w.d): right-justified to width w
put_num <- function(x, w, d = 0) {
  s <- formatC(round_half_up(x, d), format = "f", digits = d)
  s[is.na(x)] <- "."
  formatC(s, width = w, flag = " ")
}

# ---- Read ADSL (ITT population) ----
adsl <- read_xpt(file.path(rawadam, "adsl.xpt"))
names(adsl) <- toupper(names(adsl))
adsl <- adsl %>%
  filter(ITTFL == "Y") %>%
  select(USUBJID, TRT01AN, TRT01A, DCREASCD, ITTFL) %>%
  mutate(DCREASCD = ifelse(is.na(DCREASCD), "", DCREASCD))

# ---- Add a "Total" treatment group ----
adsl1 <- bind_rows(adsl, adsl %>% mutate(TRT01AN = 999, TRT01A = "Total"))

# ---- Denominators per treatment ----
tot <- adsl1 %>%
  group_by(TRT01AN) %>%
  summarise(tot = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRT01AN)

# ---- %disp macro equivalent ----
disp_fn <- function(df, col1_fun) {
  df %>%
    inner_join(tot, by = "TRT01AN") %>%
    mutate(col1 = col1_fun(.)) %>%
    group_by(TRT01AN, TRT01A, tot, col1) %>%
    summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
    mutate(per = (cnt * 100) / tot) %>%
    select(cnt, per, col1, TRT01AN, TRT01A)
}

comp <- disp_fn(adsl1 %>% filter(toupper(DCREASCD) == "COMPLETED"),
                function(d) rep("Completed the study", nrow(d)))
disc <- disp_fn(adsl1 %>% filter(toupper(DCREASCD) != "COMPLETED"),
                function(d) rep("Discontinued", nrow(d)))
all  <- disp_fn(adsl1 %>% filter(toupper(DCREASCD) != "COMPLETED"),
                function(d) d$DCREASCD)

# SQL output order: group keys, with byte-wise character ordering
all <- all[order(all$TRT01AN, all$TRT01A, all$col1, method = "radix"), ]

# ---- Interleave by TRT01AN TRT01A (comp, disc, all within each by-group) ----
comp$src <- 1; disc$src <- 2; all$src <- 3
comp$seq <- seq_len(nrow(comp))
disc$seq <- seq_len(nrow(disc))
all$seq  <- seq_len(nrow(all))
disp <- bind_rows(comp, disc, all)
disp <- disp[order(disp$TRT01AN, disp$TRT01A, disp$src, disp$seq, method = "radix"), ]

# col1 length is taken from the first dataset (comp): 19 characters
collen <- nchar("Completed the study")
disp$col1 <- substr(disp$col1, 1, collen)
isa <- disp$src == 3
disp$col1[isa] <- substr(paste0("  ", trimws(disp$col1[isa])), 1, collen)
disp$col1 <- sub("\\s+$", "", disp$col1)

disp$nper <- paste0(put_num(disp$cnt, 3, 0), "(", put_num(disp$per, 5, 1), ")")

disp <- disp %>%
  select(cnt, per, col1, trt01an = TRT01AN, trt01a = TRT01A, nper)

write.csv(as.data.frame(disp), file.path(out, "disp.csv"), row.names = FALSE, na = "")
cat("disp written:", nrow(disp), "rows\n")

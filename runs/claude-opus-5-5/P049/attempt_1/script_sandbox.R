# P049 - Subject Disposition (translation of P049_disposition_b1.sas)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam <- "/bench/data/raw/cdisc-pilot/adam"

# SAS-style rounding: half away from zero
round_half_up <- function(x, digits = 0) {
  m <- 10^digits
  sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m
}

# ---- data work.adsl; set source.adsl; ----
adsl <- haven::read_xpt(file.path(rawadam, "adsl.xpt"))
adsl <- as.data.frame(adsl)
adsl[] <- lapply(adsl, function(x) { if (inherits(x, "haven_labelled")) zap_labels(x) else x })
chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
adsl$TRT01P   <- chr(adsl$TRT01P)
adsl$DCREASCD <- chr(adsl$DCREASCD)
adsl$ITTFL    <- chr(adsl$ITTFL)

# ---- PROC SQL: disp ----
itt <- adsl %>% filter(ITTFL == "Y")

part1 <- itt %>%
  mutate(status = ifelse(DCREASCD == "Completed", DCREASCD, "Discontinued")) %>%
  group_by(TRT01P, status) %>%
  summarise(TRT01PN = first(TRT01PN), cnt = sum(!is.na(USUBJID) & USUBJID != ""),
            .groups = "drop") %>%
  mutate(ord = 1)

part2 <- itt %>%
  filter(DCREASCD != "Completed") %>%
  group_by(TRT01P, DCREASCD) %>%
  summarise(TRT01PN = first(TRT01PN), cnt = sum(!is.na(USUBJID) & USUBJID != ""),
            .groups = "drop") %>%
  rename(status = DCREASCD) %>%
  mutate(ord = 2)

disp <- bind_rows(part1, part2) %>%
  select(TRT01P, TRT01PN, cnt, ord, status) %>%
  distinct()
disp <- disp[order(disp$TRT01PN, disp$ord, disp$status, method = "radix"), ]

# ---- PROC SQL: freq ----
freq <- adsl %>%
  group_by(TRT01PN) %>%
  summarise(tot_cnt = sum(!is.na(USUBJID) & USUBJID != ""), .groups = "drop") %>%
  arrange(TRT01PN)

# ---- data disp_2 (merge by TRT01PN) ----
disp_2 <- merge(disp, freq, by = "TRT01PN", all = TRUE, sort = FALSE)
disp_2 <- disp_2[order(disp_2$TRT01PN, disp_2$ord, disp_2$status, method = "radix"), ]
pct_fmt <- function(p) {
  # percent8.1, then strip
  s <- sprintf("%.1f%%", round_half_up(p * 100, 1))
  s
}
disp_2$treat <- paste0(trimws(disp_2$TRT01P), " (N=", disp_2$tot_cnt, ")")
disp_2$cnt_prcnt <- paste0(disp_2$cnt, " (", pct_fmt(disp_2$cnt / disp_2$tot_cnt), ")")

# ---- proc sort by status ord (stable) ----
disp_2 <- disp_2[order(disp_2$status, disp_2$ord, method = "radix"), ]

# ---- proc transpose ----
idvals <- unique(disp_2$TRT01PN)            # order of first appearance
trcols <- paste0("tr", idvals)
labels <- sapply(idvals, function(v) disp_2$treat[disp_2$TRT01PN == v][1])
keys <- unique(disp_2[, c("status", "ord")])
trans_2 <- keys
trans_2$`_NAME_` <- "cnt_prcnt"
for (i in seq_along(idvals)) {
  sub <- disp_2[disp_2$TRT01PN == idvals[i], c("status", "ord", "cnt_prcnt")]
  m <- match(paste(trans_2$status, trans_2$ord), paste(sub$status, sub$ord))
  trans_2[[trcols[i]]] <- sub$cnt_prcnt[m]
}
trans_2 <- trans_2[order(trans_2$ord, trans_2$status, method = "radix"), ]
rownames(trans_2) <- NULL

write.csv(trans_2, file.path(out, "trans_2.csv"), row.names = FALSE, na = "")

# ---- proc report ----
body <- trans_2[, c("status", trcols)]
names(body) <- c("Subject Disposition", labels)
write.csv(body, file.path(out, "P049.csv"), row.names = FALSE, na = "")

disp_body <- body
disp_body[is.na(disp_body)] <- ""
widths <- sapply(seq_along(disp_body), function(j)
  max(nchar(names(disp_body)[j]), nchar(disp_body[[j]])))
fmtrow <- function(v) paste(mapply(function(x, w, j)
  if (j == 1) formatC(x, width = -w) else formatC(x, width = w),
  v, widths, seq_along(v)), collapse = "  ")

lines <- c("Subject Disposition", "")
lines <- c(lines, fmtrow(names(disp_body)))
lines <- c(lines, strrep("-", sum(widths) + 2 * (length(widths) - 1)), "")
prev_ord <- trans_2$ord[1]
for (i in seq_len(nrow(disp_body))) {
  if (trans_2$ord[i] != prev_ord) { lines <- c(lines, ""); prev_ord <- trans_2$ord[i] }
  lines <- c(lines, fmtrow(unlist(disp_body[i, ])))
}
lines <- c(lines, "", "",
  " Abbreviations:  N = number of subjects in the population; n=number of subjects specified category",
  " % = Percentage of subjects with N as denominator")
writeLines(lines, file.path(out, "P049.txt"))

cat("P049 done\n")

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

# SAS-style rounding (half away from zero)
rhu <- function(x, d = 0) { m <- 10^d; sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m }
# PVALUE6.4 format
fmt_p <- function(p) ifelse(is.na(p), "",
                            ifelse(p < 0.0001, "<.0001", sprintf("%.4f", rhu(p, 4))))
cpfmt <- function(cnt) paste0(sprintf("%.0f", cnt), "(", sprintf("%.1f", cnt), ")")
sas_order <- function(...) order(..., method = "radix")

cat("Reading data\n")
adae_raw <- read_xpt(file.path(rawadam, "adae.xpt"))
names(adae_raw) <- tolower(names(adae_raw))
adsl_raw <- read_xpt(file.path(rawadam, "adsl.xpt"))
names(adsl_raw) <- tolower(names(adsl_raw))

# ---- SAE data ----
adae <- adae_raw %>%
  filter((aocc02fl %in% "Y" | aocc04fl %in% "Y") & aeser %in% "Y" & saffl %in% "Y") %>%
  select(usubjid, trtan, aedecod, aeser, aocc02fl, aocc04fl, saffl) %>%
  as.data.frame()
adae <- adae[sas_order(adae$usubjid), , drop = FALSE]

# ---- Subject info ----
adsl <- adsl_raw %>% filter(!is.na(trt01an)) %>% select(usubjid, trt01an) %>% as.data.frame()
adsl <- adsl[sas_order(adsl$usubjid, adsl$trt01an), , drop = FALSE]
adsl <- adsl[!duplicated(adsl[, c("usubjid", "trt01an")]), , drop = FALSE]

adae_sl <- inner_join(adsl, adae, by = "usubjid", relationship = "many-to-many")
adae_sl <- adae_sl[sas_order(adae_sl$usubjid), , drop = FALSE]

# ---- Denominator ----
sum_1 <- adsl %>% count(trt01an, name = "count") %>% arrange(trt01an) %>% as.data.frame()
trtcn <- setNames(sum_1$count, as.character(sum_1$trt01an))
trtnums <- sum_1$trt01an

# ---- adae_sl2: nodupkey by usubjid aedecod ----
adae_sl2 <- adae_sl[sas_order(adae_sl$usubjid, adae_sl$aedecod), , drop = FALSE]
adae_sl2 <- adae_sl2[!duplicated(adae_sl2[, c("usubjid", "aedecod")]), , drop = FALSE]

adae_dummy <- adae_sl %>% filter(aocc02fl %in% "Y") %>% distinct(aedecod) %>% as.data.frame()
adae_dummy <- adae_dummy[sas_order(adae_dummy$aedecod), , drop = FALSE]

adae_sl3 <- adae_sl2[adae_sl2$aedecod %in% adae_dummy$aedecod, , drop = FALSE]

sum_2 <- adae_sl3 %>% filter(!is.na(trtan)) %>% count(aedecod, trtan, name = "count") %>% as.data.frame()

dummy_2 <- expand.grid(trtan = trtnums, aedecod = adae_dummy$aedecod, stringsAsFactors = FALSE)[, c("aedecod", "trtan")]

adae_sl4 <- full_join(dummy_2, sum_2, by = c("aedecod", "trtan"))
adae_sl5 <- left_join(adae_sl4, sum_1 %>% rename(trtan = trt01an, bign = count), by = "trtan")

adae_sl6 <- bind_rows(
  adae_sl5 %>% mutate(resp = 0, count_final = ifelse(is.na(count), bign, bign - count)),
  adae_sl5 %>% mutate(resp = 1, count_final = ifelse(is.na(count), 0, count))
)

# ---- Fisher per aedecod ----
aedecods <- adae_dummy$aedecod
fish2 <- data.frame(aedecod = character(0), cvalue1 = character(0), stringsAsFactors = FALSE)
sum_final <- NULL
for (ae in aedecods) {
  d <- adae_sl6[adae_sl6$aedecod == ae, ]
  m <- matrix(0, nrow = length(trtnums), ncol = 2)
  for (k in seq_len(nrow(d))) {
    i <- match(d$trtan[k], trtnums); j <- d$resp[k] + 1
    m[i, j] <- m[i, j] + d$count_final[k]
  }
  p <- fisher.test(m)$p.value
  fish2 <- rbind(fish2, data.frame(aedecod = ae, cvalue1 = fmt_p(p), stringsAsFactors = FALSE))
  sum_final <- rbind(sum_final, data.frame(aedecod = ae, trtan = trtnums, resp = 1,
                                           count = m[, 2], stringsAsFactors = FALSE))
}

sum_final$cp <- cpfmt(sum_final$count)
sum_final$ord <- ifelse(sum_final$trtan == 81, 1, 2)
sum_final <- sum_final[sas_order(sum_final$aedecod, sum_final$ord), ]
cm <- sum_final %>% filter(ord == 1) %>% select(aedecod, count_max = count)
sum_final <- left_join(sum_final, cm, by = "aedecod")

sum_final2 <- data.frame(aedecod = aedecods, stringsAsFactors = FALSE) %>%
  left_join(cm, by = "aedecod")
for (t in trtnums) {
  sub <- sum_final[sum_final$trtan == t, c("aedecod", "cp")]
  names(sub)[2] <- paste0("trt", t)
  sum_final2 <- left_join(sum_final2, sub, by = "aedecod")
}
sum_final2 <- left_join(sum_final2, fish2, by = "aedecod")
sum_final2 <- sum_final2[sas_order(-sum_final2$count_max, sum_final2$aedecod), ]

# ---- At least one SAE ----
sub_ids <- unique(adae_sl$usubjid[adae_sl$aocc02fl %in% "Y"])
adae_sl_sub2 <- adsl %>% mutate(flg = ifelse(usubjid %in% sub_ids, 1, 0))
m <- matrix(0, nrow = length(trtnums), ncol = 2)
for (i in seq_along(trtnums)) for (f in 0:1)
  m[i, f + 1] <- sum(adae_sl_sub2$trt01an == trtnums[i] & adae_sl_sub2$flg == f)
p_sub <- fisher.test(m)$p.value

sum_final_sub2 <- data.frame(aedecod = "Number of subjects reporting serious adverse events",
                             count_max = NA_real_, stringsAsFactors = FALSE)
for (i in seq_along(trtnums)) sum_final_sub2[[paste0("trt", trtnums[i])]] <- cpfmt(m[i, 2])
sum_final_sub2$cvalue1 <- fmt_p(p_sub)

final_report <- bind_rows(sum_final_sub2, sum_final2)
for (cn in c("trt0", "trt54", "trt81")) if (!cn %in% names(final_report)) final_report[[cn]] <- ""
final_report <- final_report[, c("aedecod", "count_max", "trt0", "trt54", "trt81", "cvalue1")]

write.csv(final_report, file.path(out, "final_report.csv"), row.names = FALSE, na = "")
tab <- final_report[, c("aedecod", "trt0", "trt54", "trt81", "cvalue1")]
write.csv(tab, file.path(out, "P045.csv"), row.names = FALSE, na = "")

# ---- Text rendering (PROC REPORT style) ----
wrap_cell <- function(x, w) { x <- ifelse(is.na(x), "", x); r <- strwrap(x, width = w); if (length(r) == 0) "" else r }
widths <- c(40, 30, 30, 30, 30)
hdr1 <- c("Preferred Term", "Placebo", "Treatment 1", "Treatment 2", "p-value*b")
hdr2 <- c("", paste0("N=", trtcn["0"]), paste0("N=", trtcn["54"]), paste0("N=", trtcn["81"]), "")
fmtline <- function(v) paste(mapply(function(s, w) formatC(s, width = -w), v, widths), collapse = " ")
lines <- c("Table 7.5 Summary of Serious Adverse Events", "Safety Population", "-",
           fmtline(hdr1), fmtline(hdr2), strrep("-", sum(widths) + length(widths) - 1), "")
for (r in seq_len(nrow(tab))) {
  cells <- lapply(seq_along(widths), function(k) wrap_cell(as.character(tab[r, k]), widths[k]))
  nl <- max(sapply(cells, length))
  for (l in seq_len(nl)) {
    lines <- c(lines, fmtline(sapply(cells, function(cc) if (l <= length(cc)) cc[l] else "")))
  }
}
lines <- c(lines, "-", "Denominator for each % is treatment column N",
           "*b = p-values are from Fisher's Exact Test")
writeLines(lines, file.path(out, "P045.txt"))
cat("Done\n")

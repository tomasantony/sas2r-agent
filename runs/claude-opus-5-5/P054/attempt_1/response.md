```r
suppressPackageStartupMessages({
  library(haven); library(dplyr)
})
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

# ---- helpers ----
round_half_up <- function(x, d = 0) { m <- 10^d; sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m }
propcase <- function(x) {
  x <- tolower(x)
  gsub("(^|[ /(.\t-])([a-z])", "\\1\\U\\2", x, perl = TRUE)
}
catx <- function(...) {
  args <- lapply(list(...), function(v) trimws(as.character(v)))
  n <- max(sapply(args, length))
  sapply(seq_len(n), function(i) {
    p <- sapply(args, function(v) v[(i - 1) %% length(v) + 1])
    p <- p[!is.na(p) & p != ""]
    paste(p, collapse = " ")
  })
}
nz <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }

# ---- input ----
advs <- read_xpt(file.path(rawadam, "advs.xpt"))
names(advs) <- toupper(names(advs))
advs <- advs %>%
  mutate(PARAM = catx(nz(PARAM), propcase(nz(ATPT))),
         PARAMN = ATPTN)

inds <- advs %>%
  select(USUBJID, TRTPN, TRTP, PARAM, PARAMCD, PARAMN, ATPT, ATPTN, AVAL, AVISITN,
         ANL01FL, BNRIND, ANRIND, SHIFT1, CHGCAT1, SAFFL) %>%
  mutate(across(c(PARAMCD, ANL01FL, SAFFL, BNRIND, ANRIND, TRTP, PARAM), nz)) %>%
  filter(PARAMCD == "DIABP", ANL01FL == "Y", AVISITN == 99, SAFFL == "Y",
         BNRIND != "Missing", ANRIND != "Missing")

# treatment format
trtcomb <- inds %>% distinct(start = TRTPN, label = TRTP) %>% filter(!is.na(start))
trtft <- function(v) { r <- trtcomb$label[match(v, trtcomb$start)]; ifelse(is.na(r), as.character(v), r) }

# ---- PROC SUMMARY (obs with missing class vars excluded) ----
d <- inds %>% filter(PARAM != "", !is.na(PARAMN), !is.na(TRTPN), BNRIND != "", ANRIND != "")
cnt <- function(df, ...) df %>% group_by(PARAM, PARAMN, TRTPN, ...) %>%
  summarise(n = sum(!is.na(AVAL)), .groups = "drop")
counts <- bind_rows(
  cnt(d) %>% mutate(BNRIND = "", ANRIND = ""),
  cnt(d, ANRIND) %>% mutate(BNRIND = ""),
  cnt(d, BNRIND) %>% mutate(ANRIND = ""),
  cnt(d, BNRIND, ANRIND)
)

shiftord <- c(Low = 1, Normal = 2, High = 3, Total = 4)
shifttx <- c("Low", "Normal", "High", "Total")

counts <- counts %>%
  mutate(BNRIND = ifelse(BNRIND == "", "Total", BNRIND),
         ANRIND = ifelse(ANRIND == "", "Total", ANRIND),
         BNRINDN = unname(shiftord[BNRIND]),
         ANRINDN = unname(shiftord[ANRIND]))
denom <- counts %>% filter(BNRIND == "Total", ANRIND == "Total") %>%
  select(PARAM, PARAMN, TRTPN, denom = n)
fixtot <- counts %>% filter(!(BNRIND == "Total" & ANRIND == "Total"))

allcat <- fixtot %>% distinct(PARAM, PARAMN, TRTPN)
full <- allcat %>% tidyr::crossing(BNRINDN = 1:4, ANRINDN = 1:4) %>% mutate(n0 = 0)
allrecs <- full %>%
  left_join(fixtot %>% select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, n),
            by = c("PARAM", "PARAMN", "TRTPN", "BNRINDN", "ANRINDN")) %>%
  mutate(n = ifelse(is.na(n), n0, n)) %>% select(-n0)
alldenom <- allrecs %>% left_join(denom, by = c("PARAM", "PARAMN", "TRTPN")) %>%
  mutate(BNRIND = shifttx[BNRINDN], ANRIND = shifttx[ANRINDN]) %>%
  filter(!is.na(denom)) %>%
  arrange(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN)

pbcat <- alldenom %>% distinct(ANRIND, ANRINDN) %>% arrange(ANRINDN) %>% pull(ANRIND)
pbcat <- unique(pbcat)

# ---- transpose ----
alldenom <- alldenom %>%
  mutate(pct = ifelse(denom > 0,
                      paste0("(", sprintf("%5.1f", round_half_up(100 * n / denom, 1)), ")"),
                      "(  0.0)"))
trans <- alldenom %>% group_by(PARAM, PARAMN, TRTPN, BNRINDN, BNRIND, denom) %>%
  summarise(N1 = n[ANRINDN == 1][1], N2 = n[ANRINDN == 2][1],
            N3 = n[ANRINDN == 3][1], N4 = n[ANRINDN == 4][1],
            PCT1 = pct[ANRINDN == 1][1], PCT2 = pct[ANRINDN == 2][1],
            PCT3 = pct[ANRINDN == 3][1], PCT4 = pct[ANRINDN == 4][1], .groups = "drop") %>%
  mutate(TRTTX = substr(catx(trtft(TRTPN), paste0("(N = ", denom, ")")), 1, 30)) %>%
  select(PARAM, PARAMN, TRTPN, BNRINDN, BNRIND, N1, N2, N3, N4,
         PCT1, PCT2, PCT3, PCT4, denom, TRTTX) %>%
  arrange(PARAMN, PARAM, TRTPN, BNRINDN)

write.csv(trans, file.path(out, "trans.csv"), row.names = FALSE, na = "")

# ---- report ----
lines <- character(0)
body <- list()
tw <- max(9, nchar(trans$TRTTX))
for (g in unique(paste(trans$PARAMN, trans$PARAM, sep = "\r"))) {
  sub <- trans[paste(trans$PARAMN, trans$PARAM, sep = "\r") == g, ]
  lines <- c(lines, "", paste0("Parameter Number=", sub$PARAMN[1], " Parameter=", sub$PARAM[1]), "",
             paste0(strrep(" ", tw + 12), "Post-Baseline Result"),
             paste0(formatC("", width = tw), "  ", formatC("Baseline", width = -8), "  ",
                    paste(sprintf("%-17s", pbcat[1:4]), collapse = "")),
             paste0(formatC("Treatment", width = -tw), "  ", formatC("Result", width = -8), "  ",
                    paste(rep(sprintf("%8s  %-7s", "n", "%"), 4), collapse = "")))
  prev <- NULL
  for (i in seq_len(nrow(sub))) {
    r <- sub[i, ]
    tt <- if (!is.null(prev) && prev == r$TRTPN) "" else r$TRTTX
    if (!is.null(prev) && prev != r$TRTPN) lines <- c(lines, "")
    prev <- r$TRTPN
    cells <- paste(sprintf("%8d  %-7s", as.integer(c(r$N1, r$N2, r$N3, r$N4)),
                           c(r$PCT1, r$PCT2, r$PCT3, r$PCT4)), collapse = "")
    lines <- c(lines, paste0(formatC(tt, width = -tw), "  ", formatC(r$BNRIND, width = -8), "  ", cells))
  }
  lines <- c(lines, "")
}
writeLines(lines, file.path(out, "P054.txt"))

tab <- trans %>% transmute(PARAMN, PARAM, Treatment = TRTTX, Baseline_Result = BNRIND,
                           N1, PCT1, N2, PCT2, N3, PCT3, N4, PCT4)
names(tab)[5:12] <- paste0(rep(pbcat[1:4], each = 2), c("_n", "_pct"))
write.csv(tab, file.path(out, "P054.csv"), row.names = FALSE, na = "")
cat("P054 done\n")
```
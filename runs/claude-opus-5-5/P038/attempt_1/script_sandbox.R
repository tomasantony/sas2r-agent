suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam <- "/bench/data/raw/cdisc-pilot/adam"

# ---------------- helpers ----------------
round_half_up <- function(x, digits = 0) {
  m <- 10^digits
  sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m
}
sas_trim <- function(x) sub("\\s+$", "", sub("^\\s+", "", x))
catx <- function(sep, ...) {
  args <- list(...)
  n <- max(sapply(args, length))
  args <- lapply(args, function(a) { a <- as.character(a); a[is.na(a)] <- ""; rep_len(a, n) })
  sapply(seq_len(n), function(i) {
    v <- sas_trim(sapply(args, `[`, i))
    v <- v[v != ""]
    paste(v, collapse = sep)
  })
}
propcase <- function(x) {
  x <- tolower(ifelse(is.na(x), "", x))
  gsub("(^|[ /\\-(.\t])([a-z])", "\\1\\U\\2", x, perl = TRUE)
}
is_miss_chr <- function(x) is.na(x) | sas_trim(x) == ""

# ---------------- input ----------------
advs <- read_xpt(file.path(rawadam, "advs.xpt"))
names(advs) <- toupper(names(advs))
pw <- attr(advs$PARAM, "width")
newparam <- catx(" ", advs$PARAM, propcase(advs$ATPT))
if (!is.null(pw) && is.numeric(pw) && pw > 0) newparam <- substr(newparam, 1, pw)
advs$PARAM <- newparam
advs$PARAMN <- advs$ATPTN

invar <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN","ATPT","ATPTN","AVAL",
           "AVISITN","ANL01FL","BNRIND","ANRIND","SHIFT1","CHGCAT1","SAFFL")
inds <- advs %>% select(any_of(invar)) %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.x), "", .x))) %>%
  filter(PARAMCD == "DIABP", ANL01FL == "Y", !is.na(AVISITN), AVISITN == 99, SAFFL == "Y",
         BNRIND != "Missing", ANRIND != "Missing")
cat("inds rows:", nrow(inds), "\n")

# ---------------- treatment format ----------------
trtcomb <- inds %>% distinct(TRTPN, TRTP) %>% filter(!is.na(TRTPN)) %>%
  group_by(TRTPN) %>% slice(1) %>% ungroup()
trtft <- function(v) {
  lab <- trtcomb$TRTP[match(v, trtcomb$TRTPN)]
  ifelse(is.na(lab), as.character(v), lab)
}

# ---------------- proc summary ----------------
d <- inds %>% filter(!is_miss_chr(PARAM), !is.na(PARAMN), !is.na(TRTPN),
                     !is_miss_chr(BNRIND), !is_miss_chr(ANRIND))
c_both <- d %>% group_by(PARAM, PARAMN, TRTPN, BNRIND, ANRIND) %>%
  summarise(n = sum(!is.na(AVAL)), .groups = "drop")
c_b <- d %>% group_by(PARAM, PARAMN, TRTPN, BNRIND) %>%
  summarise(n = sum(!is.na(AVAL)), .groups = "drop") %>% mutate(ANRIND = "Total")
c_a <- d %>% group_by(PARAM, PARAMN, TRTPN, ANRIND) %>%
  summarise(n = sum(!is.na(AVAL)), .groups = "drop") %>% mutate(BNRIND = "Total")
c_0 <- d %>% group_by(PARAM, PARAMN, TRTPN) %>%
  summarise(n = sum(!is.na(AVAL)), .groups = "drop") %>% mutate(BNRIND = "Total", ANRIND = "Total")
counts <- bind_rows(c_both, c_b, c_a, c_0)

shiftord <- function(x) {
  x6 <- substr(sas_trim(x), 1, 6)
  r <- c(Low = 1, Normal = 2, High = 3, Total = 4)[x6]
  r <- unname(r)
  r[is.na(r)] <- suppressWarnings(as.numeric(x6[is.na(r)]))
  r
}
counts <- counts %>% mutate(BNRINDN = shiftord(BNRIND), ANRINDN = shiftord(ANRIND))

denom <- counts %>% filter(BNRIND == "Total", ANRIND == "Total") %>%
  select(PARAM, PARAMN, TRTPN, denom = n)
fixtot <- counts %>% filter(!(BNRIND == "Total" & ANRIND == "Total")) %>%
  select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, n_f = n)

# ---------------- full grid ----------------
allcat <- fixtot %>% distinct(PARAM, PARAMN, TRTPN)
full <- tidyr::crossing(allcat, BNRINDN = 1:4, ANRINDN = 1:4) %>% mutate(n = 0)
allrecs <- full %>% left_join(fixtot %>% distinct(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, .keep_all = TRUE),
                              by = c("PARAM","PARAMN","TRTPN","BNRINDN","ANRINDN")) %>%
  mutate(n = ifelse(is.na(n_f), n, n_f)) %>% select(-n_f)

shifttx <- function(v) c("Low","Normal","High","Total")[v]
alldenom <- allrecs %>% left_join(denom, by = c("PARAM","PARAMN","TRTPN")) %>%
  filter(!is.na(denom)) %>%
  mutate(BNRIND = shifttx(BNRINDN), ANRIND = shifttx(ANRINDN))
alldenom <- alldenom[order(alldenom$PARAM, alldenom$PARAMN, alldenom$TRTPN,
                           alldenom$BNRINDN, alldenom$ANRINDN, method = "radix"), ]

pbcat <- alldenom %>% filter(!is.na(ANRIND)) %>% distinct(ANRINDN, ANRIND) %>%
  arrange(ANRINDN) %>% pull(ANRIND)
pbcat <- c(pbcat, rep("", 4))[1:4]

# ---------------- transpose ----------------
fmtpct <- function(n, den) {
  ifelse(den > 0,
         paste0("(", formatC(round_half_up(100 * n / den, 1), width = 5, format = "f", digits = 1), ")"),
         "(  0.0)")
}
trans <- alldenom %>%
  mutate(pct = fmtpct(n, denom)) %>%
  group_by(PARAM, PARAMN, TRTPN, BNRINDN, BNRIND) %>%
  summarise(denom = last(denom),
            n1 = n[ANRINDN == 1][1], pct1 = pct[ANRINDN == 1][1],
            n2 = n[ANRINDN == 2][1], pct2 = pct[ANRINDN == 2][1],
            n3 = n[ANRINDN == 3][1], pct3 = pct[ANRINDN == 3][1],
            n4 = n[ANRINDN == 4][1], pct4 = pct[ANRINDN == 4][1],
            .groups = "drop") %>%
  mutate(trttx = substr(catx(" ", trtft(TRTPN), paste0("(N = ", format(denom, scientific = FALSE, trim = TRUE), ")")), 1, 30))
trans <- trans[order(trans$PARAM, trans$PARAMN, trans$TRTPN, trans$BNRINDN, method = "radix"), ]
trans <- trans %>% select(PARAM, PARAMN, TRTPN, BNRINDN, BNRIND, n1, n2, n3, n4,
                          pct1, pct2, pct3, pct4, denom, trttx)

write.csv(trans, file.path(out, "trans.csv"), row.names = FALSE, na = "")
tab <- trans %>% select(PARAMN, PARAM, TRTPN, trttx, BNRINDN, BNRIND, n1, pct1, n2, pct2, n3, pct3, n4, pct4)
write.csv(tab, file.path(out, "P038.csv"), row.names = FALSE, na = "")

# ---------------- text rendering ----------------
lines <- character(0)
bys <- trans %>% distinct(PARAMN, PARAM)
bys <- bys[order(bys$PARAMN, bys$PARAM, method = "radix"), ]
padr <- function(x, w) formatC(x, width = w, flag = " ")
padl <- function(x, w) formatC(x, width = -w)
for (i in seq_len(nrow(bys))) {
  sub <- trans %>% filter(PARAMN == bys$PARAMN[i], PARAM == bys$PARAM[i])
  lines <- c(lines, "", paste0("PARAMN=", bys$PARAMN[i], " PARAM=", bys$PARAM[i]), "")
  hdr1 <- paste0(padl("", 30), " ", padl("", 9), " ", padl("Post-Baseline Result", 4 * 18))
  hdr2 <- paste0(padl("", 30), " ", padl("Baseline", 9), " ",
                 paste(sapply(pbcat, function(p) padl(p, 17)), collapse = " "))
  hdr3 <- paste0(padl("Treatment", 30), " ", padl("Result", 9), " ",
                 paste(rep(paste0(padr("n", 9), " ", padl("%", 7)), 4), collapse = " "))
  lines <- c(lines, hdr1, hdr2, hdr3, strrep("-", nchar(hdr3)))
  prevt <- NULL
  for (t in unique(sub$TRTPN)) {
    st <- sub %>% filter(TRTPN == t)
    for (j in seq_len(nrow(st))) {
      r <- st[j, ]
      tt <- if (j == 1) r$trttx else ""
      cells <- sapply(1:4, function(k) paste0(padr(as.character(r[[paste0("n", k)]]), 9), " ",
                                              padl(r[[paste0("pct", k)]], 7)))
      lines <- c(lines, paste0(padl(tt, 30), " ", padl(r$BNRIND, 9), " ", paste(cells, collapse = " ")))
    }
    lines <- c(lines, "")
  }
}
writeLines(lines, file.path(out, "P038.txt"))
cat("Done. trans rows:", nrow(trans), "\n")

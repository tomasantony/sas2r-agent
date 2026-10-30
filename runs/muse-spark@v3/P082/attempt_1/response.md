```r
# P082 f_waterfall.sas -> R translation
# Figure 14.2.3 Waterfall Plot of Best Percentage Change from Baseline

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
})

# round half away from zero (SAS) helper
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

find_file <- function(dir, name) {
  if (!dir.exists(dir)) return(NULL)
  fl <- list.files(dir, full.names = TRUE, recursive = FALSE)
  if (length(fl) == 0) return(NULL)
  # strip extension
  base <- toupper(tools::file_path_sans_ext(basename(fl)))
  idx <- which(base == toupper(name))
  if (length(idx) == 0) return(NULL)
  fl[idx[1]]
}

read_sas_any <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  if (ext == "xpt") {
    haven::read_xpt(path)
  } else if (ext %in% c("sas7bdat", "sas7bat")) {
    haven::read_sas(path)
  } else {
    # try sas then xpt
    tryCatch(haven::read_sas(path), error = function(e) haven::read_xpt(path))
  }
}

get_col <- function(df, name) {
  # case-insensitive column lookup, return actual name or NULL
  if (is.null(df)) return(NULL)
  idx <- which(toupper(names(df)) == toupper(name))
  if (length(idx) == 0) return(NULL)
  names(df)[idx[1]]
}

adtr_path <- find_file(adam_dir, "adtr")
adsl_path <- find_file(adam_dir, "adsl")
adrs_path <- find_file(adam_dir, "adrs")

adtr <- read_sas_any(adtr_path)
adsl <- read_sas_any(adsl_path)

message("adtr path: ", ifelse(is.null(adtr_path), "NULL", adtr_path))
message("adsl path: ", ifelse(is.null(adsl_path), "NULL", adsl_path))
print(dim(adtr)); print(dim(adsl))

tumor <- NULL

if (!is.null(adtr) && !is.null(adsl)) {
  c_usubj_adtr <- get_col(adtr, "USUBJID")
  c_trta_adtr  <- get_col(adtr, "TRTA")
  c_trtan_adtr <- get_col(adtr, "TRTAN")
  c_paramcd    <- get_col(adtr, "PARAMCD")
  c_param      <- get_col(adtr, "PARAM")
  c_pchg       <- get_col(adtr, "PCHG")
  c_avalc      <- get_col(adtr, "AVALC")
  c_usubj_adsl <- get_col(adsl, "USUBJID")
  c_efffl      <- get_col(adsl, "EFFFL")
  stopifnot(!is.null(c_usubj_adtr), !is.null(c_paramcd), !is.null(c_pchg),
            !is.null(c_usubj_adsl), !is.null(c_efffl))
  # select needed columns to avoid .x/.y suffixes (SAS: a.* except EFFFL from b)
  # Build renamed data frames with standard uppercase names
  a_sel <- adtr
  # keep only needed
  keep_a <- c(c_usubj_adtr, c_trta_adtr, c_trtan_adtr, c_paramcd, c_param, c_pchg, c_avalc)
  keep_a <- keep_a[!sapply(keep_a, is.null)]
  a_sel <- a_sel[, keep_a, drop = FALSE]
  # rename to standard
  nm <- names(a_sel)
  nm[toupper(nm) == "USUBJID"] <- "USUBJID"
  nm[toupper(nm) == "TRTA"] <- "TRTA"
  nm[toupper(nm) == "TRTAN"] <- "TRTAN"
  nm[toupper(nm) == "PARAMCD"] <- "PARAMCD"
  nm[toupper(nm) == "PARAM"] <- "PARAM"
  nm[toupper(nm) == "PCHG"] <- "PCHG"
  nm[toupper(nm) == "AVALC"] <- "AVALC"
  names(a_sel) <- nm

  b_sel <- adsl[, c(c_usubj_adsl, c_efffl), drop = FALSE]
  names(b_sel) <- c("USUBJID", "EFFFL")

  # inner join on USUBJID
  tumor_raw <- inner_join(a_sel, b_sel, by = "USUBJID")
  stopifnot("EFFFL" %in% names(tumor_raw))
  stopifnot("PCHG" %in% names(tumor_raw))
  stopifnot("PARAMCD" %in% names(tumor_raw))

  # where EFFFL='Y' and PARAMCD='BPCHG' (case-sensitive like SAS)
  tumor <- tumor_raw %>%
    filter(!is.na(EFFFL) & EFFFL == "Y",
           !is.na(PARAMCD) & PARAMCD == "BPCHG")
  # ensure columns exist
  if (!("TRTA" %in% names(tumor))) tumor$TRTA <- NA_character_
  if (!("TRTAN" %in% names(tumor))) tumor$TRTAN <- NA_real_
  if (!("PARAM" %in% names(tumor))) tumor$PARAM <- NA_character_
  if (!("AVALC" %in% names(tumor))) tumor$AVALC <- NA_character_
  tumor <- tumor[, c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","PCHG","AVALC","EFFFL")]
  # order by PCHG (SAS missing sorts first)
  tumor <- tumor[order(ifelse(is.na(tumor$PCHG), -Inf, 1) * 0 + ifelse(is.na(tumor$PCHG), 0, 1), tumor$PCHG, na.last = FALSE), , drop = FALSE]
  # The above: put NA first then ascending. Simpler explicit:
  ord <- order(-as.numeric(is.na(tumor$PCHG)), tumor$PCHG, na.last = TRUE)
  # -is.na: NA -> -1 (first), non-NA -> 0 ; ascending puts -1 first. Correct.
  tumor <- tumor[ord, , drop = FALSE]
}

# Fallback: derive from ADRS if ADTR not available or zero rows (commented SAS alternative)
if (is.null(tumor) || nrow(tumor) == 0) {
  message("NOTE: tumor from ADTR is empty/missing, trying ADRS fallback if available")
  adrs <- read_sas_any(adrs_path)
  if (!is.null(adrs) && !is.null(adsl)) {
    c_u_adrs <- get_col(adrs, "USUBJID")
    c_ta_adrs <- get_col(adrs, "TRTA")
    c_tn_adrs <- get_col(adrs, "TRTAN")
    c_pc_adrs <- get_col(adrs, "PARAMCD")
    c_pchg_adrs <- get_col(adrs, "PCHG")
    c_av_adrs <- get_col(adrs, "AVALC")
    c_u_sl <- get_col(adsl, "USUBJID")
    c_ef_sl <- get_col(adsl, "EFFFL")
    if (!is.null(c_u_adrs) && !is.null(c_pc_adrs) && !is.null(c_pchg_adrs)) {
      a2 <- adrs
      # filter PARAMCD='SUMLD' equivalent? SAS alt uses SUMLD then min(PCHG)
      # keep mapping
      tmp_a <- data.frame(
        USUBJID = a2[[c_u_adrs]],
        TRTA = if (!is.null(c_ta_adrs)) a2[[c_ta_adrs]] else NA,
        TRTAN = if (!is.null(c_tn_adrs)) suppressWarnings(as.numeric(a2[[c_tn_adrs]])) else NA,
        PARAMCD = a2[[c_pc_adrs]],
        PCHG = suppressWarnings(as.numeric(a2[[c_pchg_adrs]])),
        AVALC_tmp = if (!is.null(c_av_adrs)) as.character(a2[[c_av_adrs]]) else NA_character_,
        stringsAsFactors = FALSE
      )
      tmp_b <- data.frame(USUBJID = adsl[[c_u_sl]], EFFFL = as.character(adsl[[c_ef_sl]]),
                          BOR = NA_character_, stringsAsFactors = FALSE)
      # try to get BOR AVALC from adsl if present
      c_bor <- get_col(adsl, "AVALC")
      # Actually SAS alt uses b.AVALC as BOR; adsl may not have it - keep NA
      joined <- inner_join(tmp_a, tmp_b[, c("USUBJID","EFFFL")], by = "USUBJID")
      filt <- joined %>% filter(!is.na(EFFFL) & EFFFL == "Y")
      # if PARAMCD column contains SUMLD, filter to it; otherwise use all
      if (any(!is.na(filt$PARAMCD) & filt$PARAMCD == "SUMLD")) {
        filt <- filt %>% filter(PARAMCD == "SUMLD")
      }
      if (nrow(filt) > 0 && (is.null(tumor) || nrow(tumor) == 0)) {
        # min PCHG per subject
        tumor <- filt %>% group_by(USUBJID, TRTA, TRTAN) %>%
          summarise(PCHG = suppressWarnings(min(PCHG, na.rm = TRUE)), .groups = "drop")
        tumor$PCHG[is.infinite(tumor$PCHG)] <- NA_real_
        tumor$PARAMCD <- "BPCHG"
        tumor$PARAM <- "Best Percent Change"
        tumor$AVALC <- NA_character_
        tumor$EFFFL <- "Y"
        tumor <- tumor[, c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","PCHG","AVALC","EFFFL")]
        tumor <- tumor[order(-as.numeric(is.na(tumor$PCHG)), tumor$PCHG, na.last = TRUE), , drop = FALSE]
      }
    }
  }
}

if (is.null(tumor)) {
  tumor <- data.frame(USUBJID=character(0), TRTA=character(0), TRTAN=numeric(0),
                      PARAMCD=character(0), PARAM=character(0), PCHG=numeric(0),
                      AVALC=character(0), EFFFL=character(0), stringsAsFactors=FALSE)
}

message("tumor N=", nrow(tumor))

#--- Step 2: Assign display order (sorted by percent change) ---
waterfall <- tumor
waterfall$PCHG <- suppressWarnings(as.numeric(waterfall$PCHG))
# ensure sorted by PCHG with missing first (SAS BY PCHG)
waterfall <- waterfall[order(-as.numeric(is.na(waterfall$PCHG)), waterfall$PCHG, na.last = TRUE), , drop = FALSE]
waterfall$bar_order <- seq_len(nrow(waterfall))

# Color by response category: select(upcase(AVALC))
up <- toupper(as.character(waterfall$AVALC))
up[is.na(up)] <- ""
waterfall$bar_color <- ifelse(up == "CR", "darkgreen",
                       ifelse(up == "PR", "green",
                       ifelse(up == "SD", "gold",
                       ifelse(up == "PD", "red", "gray"))))

# Cap at reasonable bounds
waterfall$PCHG_display <- ifelse(is.na(waterfall$PCHG), NA_real_,
                          ifelse(waterfall$PCHG > 100, 100,
                          ifelse(waterfall$PCHG < -100, -100, waterfall$PCHG)))

# Fill variable for plot (preserve NA in data, use label for legend)
waterfall$AVALC_fill <- as.character(waterfall$AVALC)
waterfall$AVALC_fill[is.na(waterfall$AVALC_fill) | waterfall$AVALC_fill == ""] <- "Missing"

# Order fill levels for legend: CR, PR, SD, PD, then others alphabetically (SAS character ordering uppercase first)
lvls_present <- unique(waterfall$AVALC_fill)
preferred <- c("CR","PR","SD","PD")
ordered_lvls <- c(preferred[preferred %in% lvls_present],
                  sort(setdiff(lvls_present, preferred)))
# also include Missing last if present but not already
waterfall$AVALC_fill <- factor(waterfall$AVALC_fill, levels = ordered_lvls)

# Map fill colors matching bar_color logic
color_map <- sapply(ordered_lvls, function(x) {
  ux <- toupper(x)
  if (ux == "CR") "darkgreen"
  else if (ux == "PR") "green"
  else if (ux == "SD") "gold"
  else if (ux == "PD") "red"
  else "gray"
})
names(color_map) <- ordered_lvls

# Write CSV of plotted data (SAS waterfall dataset counterpart)
# Keep SAS variable names; include derived vars
out_csv <- waterfall[, c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","PCHG","AVALC","EFFFL","bar_order","bar_color","PCHG_display")]
# standardize column names to uppercase to match SAS (case-insensitive match ok)
names(out_csv) <- toupper(names(out_csv))
# dates none; write ISO (no dates here)
write.csv(out_csv, file.path(out, "f_waterfall.csv"), row.names = FALSE, na = "")

#--- Step 3: Waterfall plot ---
# Titles / footnotes verbatim
t4 <- "Figure 14.2.3"
t5 <- "Waterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions"
t6 <- "Efficacy Analysis Set"
f1 <- "Data cutoff: [DDMONYYYY]."
f2 <- "Each bar represents one subject."
f3 <- "CR=Complete Response, PR=Partial Response, SD=Stable Disease, PD=Progressive Disease"
f4 <- "Dashed lines: RECIST 1.1 thresholds for PR (-30%) and PD (+20%)"
run_date <- format(Sys.Date(), "%d%b%Y")
run_date <- toupper(run_date)
f5 <- paste0("Program: f_waterfall.sas | Run: ", run_date)

plot_data <- waterfall
# For geom_col, NA PCHG_display will be dropped; keep others
p <- ggplot(plot_data, aes(x = bar_order, y = PCHG_display, fill = AVALC_fill)) +
  geom_col(color = "black", linewidth = 0.25, width = 1, na.rm = TRUE) +
  geom_hline(yintercept = -30, linetype = "dashed", color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 20, linetype = "dashed", color = "red", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  annotate("text", x = max(1, nrow(plot_data)), y = -30 + 3,
           label = "-30% (PR threshold)", size = 2.8, color = "blue", hjust = 1) +
  annotate("text", x = max(1, nrow(plot_data)), y = 20 + 3,
           label = "+20% (PD threshold)", size = 2.8, color = "red", hjust = 1) +
  scale_fill_manual(values = color_map, name = "Best Overall Response", drop = FALSE) +
  scale_y_continuous(breaks = seq(-100, 100, 20), limits = c(-100, 100),
                     name = "Best Percentage Change from Baseline (%)") +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  labs(
    title = paste0(t4, "\n", t5),
    subtitle = t6,
    x = " ",
    caption = paste(f1, f2, f3, f4, f5, sep = "\n")
  ) +
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", color = "lightgray"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0, size = 7)
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

# width=10in height=6.5in like SAS ODS GRAPHICS
ggsave(file.path(out, "f_waterfall.png"), plot = p, width = 10, height = 6.5, units = "in", dpi = 300)

message("f_waterfall completed successfully. N=", nrow(waterfall))
```
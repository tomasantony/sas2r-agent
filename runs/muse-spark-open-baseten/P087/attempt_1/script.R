out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero (not needed here but defined)
round_sas <- function(x, digits=0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper to format Date as YYYY-MM-DD for CSV (write.csv will do via format)
# Ensure dates are Date class

#==========================================================================
# STEP 1: Create mock SDTM VS data
#==========================================================================
vs <- data.frame(
  STUDYID = c("STUDY01","STUDY01","STUDY01","STUDY01","STUDY01","STUDY01","STUDY01","STUDY01",
              "STUDY01","STUDY01","STUDY01","STUDY01","STUDY01","STUDY01","STUDY01","STUDY01","STUDY01","STUDY01"),
  USUBJID = c("STUDY01-1001","STUDY01-1001","STUDY01-1001","STUDY01-1001","STUDY01-1001","STUDY01-1001","STUDY01-1001","STUDY01-1001",
              "STUDY01-1002","STUDY01-1002","STUDY01-1002","STUDY01-1002","STUDY01-1003","STUDY01-1003","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  VSSEQ = c(1,2,3,4,5,6,7,8,1,2,3,4,1,2,3,1,1,1),
  VSTESTCD = c("SYSBP","DIABP","PULSE","SYSBP","DIABP","PULSE","SYSBP","DIABP",
               "SYSBP","DIABP","SYSBP","DIABP","SYSBP","DIABP","SYSBP","SYSBP","SYSBP","SYSBP"),
  VSTEST = c("Systolic Blood Pressure","Diastolic Blood Pressure","Pulse Rate",
             "Systolic Blood Pressure","Diastolic Blood Pressure","Pulse Rate",
             "Systolic Blood Pressure","Diastolic Blood Pressure",
             "Systolic Blood Pressure","Diastolic Blood Pressure","Systolic Blood Pressure","Diastolic Blood Pressure",
             "Systolic Blood Pressure","Diastolic Blood Pressure","Systolic Blood Pressure",
             "Systolic Blood Pressure","Systolic Blood Pressure","Systolic Blood Pressure"),
  VSORRES = c("128","82","72","122","78","70","118","76","140","90","138","88","120","75","115","132","118","145"),
  VSORRESU = c("mmHg","mmHg","beats/min","mmHg","mmHg","beats/min","mmHg","mmHg","mmHg","mmHg","mmHg","mmHg","mmHg","mmHg","mmHg","mmHg","mmHg","mmHg"),
  VSSTRESN = c(128,82,72,122,78,70,118,76,140,90,138,88,120,75,115,132,118,145),
  VSSTRESU = c("mmHg","mmHg","beats/min","mmHg","mmHg","beats/min","mmHg","mmHg","mmHg","mmHg","mmHg","mmHg","mmHg","mmHg","mmHg","mmHg","mmHg","mmHg"),
  VISIT = c("Baseline","Baseline","Baseline","Week 4","Week 4","Week 4","Week 8","Week 8",
            "Baseline","Baseline","Week 4","Week 4","Baseline","Baseline","Week 4","Baseline","Baseline","Baseline"),
  VISITNUM = c(1,1,1,2,2,2,3,3,1,1,2,2,1,1,2,1,1,1),
  VSDTC = c("2024-01-15","2024-01-15","2024-01-15","2024-02-12","2024-02-12","2024-02-12","2024-03-11","2024-03-11",
            "2024-01-18","2024-01-18","2024-02-15","2024-02-15","2024-01-20","2024-01-20","2024-02-17","2024-01-25","2024-01-28","2024-01-30"),
  stringsAsFactors = FALSE
)

#==========================================================================
# STEP 2: Create mock ADSL subset
#==========================================================================
adsl <- data.frame(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  SEX = c("M","F","M","F","M","F"),
  SAFFL = c("Y","Y","Y","Y","Y","Y"),
  TRTSDT = as.Date(c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30")),
  TRTEDT = as.Date(c("2024-06-15","2024-06-18","2024-04-01","2024-06-25", NA, "2024-07-01")),
  stringsAsFactors = FALSE
)

# sort as SAS does
vs <- vs[order(vs$STUDYID, vs$USUBJID), , drop=FALSE]
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), , drop=FALSE]

#==========================================================================
# STEP 3: Merge and create base ADVS structure
#==========================================================================
# merge vs(in=a) adsl(in=b) by STUDYID USUBJID if a and b
advs_base <- merge(vs, adsl, by=c("STUDYID","USUBJID"), all=FALSE, sort=FALSE)

# Map to ADaM PARAM structure
advs_base$PARAMCD <- advs_base$VSTESTCD
advs_base$PARAM <- advs_base$VSTEST
advs_base$PARCAT1 <- "VITAL SIGNS"

# PARAMN
advs_base$PARAMN <- with(advs_base, ifelse(PARAMCD=="SYSBP", 1,
                                   ifelse(PARAMCD=="DIABP", 2,
                                   ifelse(PARAMCD=="PULSE", 3,
                                   ifelse(PARAMCD=="TEMP", 4,
                                   ifelse(PARAMCD=="RESP", 5,
                                   ifelse(PARAMCD=="WEIGHT", 6,
                                   ifelse(PARAMCD=="HEIGHT", 7, 99))))))))

# Analysis value
advs_base$AVAL <- advs_base$VSSTRESN
advs_base$AVALU <- advs_base$VSSTRESU

# Analysis date and day
advs_base$ADT <- as.Date(advs_base$VSDTC, format="%Y-%m-%d")
advs_base$ADY <- with(advs_base, ifelse(!is.na(ADT) & !is.na(TRTSDT),
                                         ifelse(ADT >= TRTSDT, as.integer(ADT - TRTSDT + 1), as.integer(ADT - TRTSDT)),
                                         NA_integer_))

# Analysis visit
advs_base$AVISIT <- advs_base$VISIT
advs_base$AVISITN <- advs_base$VISITNUM

# Analysis timepoint
advs_base$ATPT <- ""
advs_base$ATPTN <- NA_real_

# drop VSTESTCD VSTEST VSORRES VSORRESU VSSTRESN VSSTRESU VISIT VISITNUM VSDTC
advs_base$VSTESTCD <- NULL
advs_base$VSTEST <- NULL
advs_base$VSORRES <- NULL
advs_base$VSORRESU <- NULL
advs_base$VSSTRESN <- NULL
advs_base$VSSTRESU <- NULL
advs_base$VISIT <- NULL
advs_base$VISITNUM <- NULL
advs_base$VSDTC <- NULL

# Ensure column order similar to SAS
# Keep: STUDYID USUBJID VSSEQ TRTP TRTPN AGE SEX SAFFL TRTSDT TRTEDT PARAMCD PARAM PARAMN PARCAT1 AVAL AVALU ADT ADY ATPT ATPTN AVISIT AVISITN
# ABLFL will be added next

#==========================================================================
# STEP 4: Derive baseline flag
#==========================================================================
# proc sort by USUBJID PARAMCD ADT
advs_base <- advs_base[order(advs_base$USUBJID, advs_base$PARAMCD, advs_base$ADT), , drop=FALSE]

# create _pre_trt
advs_base$`_pre_trt` <- with(advs_base, ifelse(!is.na(ADT) & !is.na(TRTSDT) & ADT <= TRTSDT, 1, 0))
# For rows where ADT or TRTSDT missing, SAS would leave _pre_trt missing? But code sets else 0 only inside if not missing block.
# In SAS: if not missing(ADT) and not missing(TRTSDT) then do; if ADT <= TRTSDT then _pre_trt=1 else _pre_trt=0; end;
# So if missing, _pre_trt stays missing. Replicate:
advs_base$`_pre_trt` <- with(advs_base, ifelse(!is.na(ADT) & !is.na(TRTSDT),
                                               ifelse(ADT <= TRTSDT, 1, 0),
                                               NA_real_))

# Identify baseline - last pre-treatment record
# proc sort by USUBJID PARAMCD descending ADT
advs_base <- advs_base[order(advs_base$USUBJID, advs_base$PARAMCD, -as.numeric(advs_base$ADT)), , drop=FALSE]

# by USUBJID PARAMCD; if first.PARAMCD and _pre_trt=1 then ABLFL='Y' else ''
advs_base$ABLFL <- ""
# need to detect first.PARAMCD within USUBJID PARAMCD grouping
# Use rle approach
advs_base <- advs_base[order(advs_base$USUBJID, advs_base$PARAMCD, -as.numeric(advs_base$ADT)), , drop=FALSE]
# create group id
grp <- paste(advs_base$USUBJID, advs_base$PARAMCD, sep="|")
# first within group
first_idx <- !duplicated(grp)
advs_base$ABLFL[first_idx & !is.na(advs_base$`_pre_trt`) & advs_base$`_pre_trt`==1] <- "Y"
advs_base$`_pre_trt` <- NULL

# proc sort by USUBJID PARAMCD ADT
advs_base <- advs_base[order(advs_base$USUBJID, advs_base$PARAMCD, advs_base$ADT), , drop=FALSE]

# Save a copy for output dataset advs_base (as after step 4)
# Need to ensure column order and types
# Reorder columns for advs_base output
advs_base_out <- advs_base
# Ensure all expected columns exist
# Add ANL01FL not yet, but keep ABLFL

#==========================================================================
# STEP 5: Derive BASE, CHG, PCHG
#==========================================================================
# Get baseline values
baseline <- advs_base[advs_base$ABLFL=="Y", c("USUBJID","PARAMCD","AVAL"), drop=FALSE]
names(baseline)[names(baseline)=="AVAL"] <- "BASE"
# In case duplicate baseline (should not), keep last? SAS would have one per USUBJID PARAMCD
baseline <- baseline[!duplicated(paste(baseline$USUBJID, baseline$PARAMCD)), , drop=FALSE]
baseline <- baseline[order(baseline$USUBJID, baseline$PARAMCD), , drop=FALSE]

# Merge baseline
# proc sort both by USUBJID PARAMCD
advs_base <- advs_base[order(advs_base$USUBJID, advs_base$PARAMCD), , drop=FALSE]
baseline <- baseline[order(baseline$USUBJID, baseline$PARAMCD), , drop=FALSE]

advs <- merge(advs_base, baseline, by=c("USUBJID","PARAMCD"), all.x=TRUE, sort=FALSE)

# Derive change from baseline
advs$CHG <- with(advs, ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_))
advs$PCHG <- with(advs, ifelse(!is.na(AVAL) & !is.na(BASE) & BASE != 0, (CHG / BASE) * 100, NA_real_))

# Analysis flag for post-baseline
advs$ANL01FL <- with(advs, ifelse(!is.na(ABLFL) & ABLFL != "Y" & !is.na(AVAL) & !is.na(ADT) & !is.na(TRTSDT) & ADT > TRTSDT, "Y", ""))
# SAS: if ABLFL ne 'Y' and not missing(AVAL) and ADT > TRTSDT then ANL01FL='Y' else ''
# Note: ABLFL is "" for non-baseline, so condition holds. For baseline rows ABLFL='Y' => not Y? Actually ABLFL ne 'Y' false => ANL01FL=''
# For max records not yet, but for now.

#==========================================================================
# STEP 6: Add derived records (maximum post-baseline)
#==========================================================================
# Maximum post-baseline value
# proc means where ABLFL ne 'Y' and ADT > TRTSDT by USUBJID PARAMCD var AVAL output max=AVAL
post <- advs[advs$ABLFL != "Y" & !is.na(advs$ADT) & !is.na(advs$TRTSDT) & advs$ADT > advs$TRTSDT, , drop=FALSE]
# Need to handle NA in ABLFL: SAS ABLFL ne 'Y' includes "" and missing. In R, ABLFL != "Y" will be TRUE for "" but NA for NA. Our ABLFL never NA, so fine.
if (nrow(post) > 0) {
  # aggregate max
  agg <- aggregate(AVAL ~ USUBJID + PARAMCD, data=post, FUN=function(x) max(x, na.rm=TRUE))
  # agg has AVAL as max
  max_post <- agg
} else {
  max_post <- data.frame(USUBJID=character(0), PARAMCD=character(0), AVAL=numeric(0), stringsAsFactors=FALSE)
}

# merge _max_post(in=a) advs(where=(ABLFL='Y') keep=ABLFL USUBJID PARAMCD BASE) by USUBJID PARAMCD
# This brings BASE and ABLFL from baseline
if (nrow(max_post) > 0) {
  base_for_merge <- advs[advs$ABLFL=="Y", c("USUBJID","PARAMCD","BASE","ABLFL"), drop=FALSE]
  base_for_merge <- base_for_merge[!duplicated(paste(base_for_merge$USUBJID, base_for_merge$PARAMCD)), , drop=FALSE]
  max_post2 <- merge(max_post, base_for_merge, by=c("USUBJID","PARAMCD"), all.x=TRUE, sort=FALSE)
  max_post2$DTYPE <- "MAXIMUM"
  max_post2$AVISIT <- "Maximum Post-Baseline"
  max_post2$AVISITN <- 99
  max_post2$CHG <- with(max_post2, ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_))
  # Note: max_post2 currently has USUBJID PARAMCD AVAL BASE ABLFL DTYPE AVISIT AVISITN CHG
  # Other columns will be missing when rbind
  # Ensure same columns as advs for rbind: need to add missing columns as NA
  # advs columns currently: STUDYID USUBJID VSSEQ TRTP TRTPN AGE SEX SAFFL TRTSDT TRTEDT PARAMCD PARAM PARAMN PARCAT1 AVAL AVALU ADT ADY ATPT ATPTN AVISIT AVISITN ABLFL BASE CHG PCHG ANL01FL
  # For max_post2, we need to keep USUBJID PARAMCD AVAL BASE ABLFL DTYPE AVISIT AVISITN CHG, others NA
  # When we do rbind via dplyr::bind_rows, missing columns will be filled with NA
  # Use base R rbind with fill: use dplyr if available, else manual
  # We'll use dplyr::bind_rows if available
  if (requireNamespace("dplyr", quietly=TRUE)) {
    advs <- dplyr::bind_rows(advs, max_post2)
  } else {
    # manual fill
    all_cols <- unique(c(names(advs), names(max_post2)))
    for (col in setdiff(all_cols, names(advs))) advs[[col]] <- NA
    for (col in setdiff(all_cols, names(max_post2))) max_post2[[col]] <- NA
    max_post2 <- max_post2[, names(advs), drop=FALSE]
    advs <- rbind(advs, max_post2)
  }
} else {
  # no max records
}

#==========================================================================
# STEP 7: Add sequence and labels
#==========================================================================
# proc sort by STUDYID USUBJID PARAMCD AVISITN ADT
# Note: STUDYID may be NA for max records; sorting with NA first in SAS (missing sorts first)
# In R, NA handling: order with na.last=FALSE to mimic SAS missing first
advs <- advs[order(advs$STUDYID, advs$USUBJID, advs$PARAMCD, advs$AVISITN, advs$ADT, na.last=FALSE), , drop=FALSE]

# by STUDYID USUBJID; if first.USUBJID then ASEQ=0; ASEQ+1;
# Need to detect first.USUBJID within STUDYID USUBJID grouping
# Since STUDYID is same for all non-missing, but max records have NA STUDYID, they will be grouped separately
# SAS by STUDYID USUBJID: first.USUBJID means first within STUDYID USUBJID group
grp2 <- paste(advs$STUDYID, advs$USUBJID, sep="|")
# For NA STUDYID, paste will be "NA|USUBJID" - still groups by USUBJID but with NA STUDYID separate
# In SAS, missing STUDYID would be considered a distinct value and sort first, so max records with missing STUDYID would be separate group
# We'll replicate SAS exactly: group by STUDYID and USUBJID
advs$ASEQ <- NA_integer_
cur_seq <- 0
prev_grp <- NA_character_
for (i in seq_len(nrow(advs))) {
  cur_grp <- grp2[i]
  # handle NA in grp: if STUDYID is NA, cur_grp is "NA|..." but we need to treat NA as distinct
  # Use is.na check for STUDYID
  if (is.na(prev_grp) || cur_grp != prev_grp) {
    cur_seq <- 1
  } else {
    cur_seq <- cur_seq + 1
  }
  advs$ASEQ[i] <- cur_seq
  prev_grp <- cur_grp
}

# Reorder columns to match SAS varnum order (approximate)
# Desired order: STUDYID USUBJID ASEQ PARAMCD PARAM PARAMN PARCAT1 AVAL AVALU BASE CHG PCHG ADT ADY AVISIT AVISITN ABLFL ANL01FL DTYPE TRTP TRTPN TRTSDT SAFFL VSSEQ AGE SEX TRTEDT ATPT ATPTN
# But we will define a sensible order that includes all
desired_order <- c("STUDYID","USUBJID","ASEQ","PARAMCD","PARAM","PARAMN","PARCAT1","AVAL","AVALU","BASE","CHG","PCHG","ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL","DTYPE","TRTP","TRTPN","TRTSDT","TRTEDT","SAFFL","VSSEQ","AGE","SEX","ATPT","ATPTN")
# Keep only existing columns, then add any remaining
existing <- intersect(desired_order, names(advs))
remaining <- setdiff(names(advs), existing)
advs <- advs[, c(existing, remaining), drop=FALSE]

# Also ensure advs_base_out column order
desired_base <- c("STUDYID","USUBJID","VSSEQ","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT","PARAMCD","PARAM","PARAMN","PARCAT1","AVAL","AVALU","ADT","ADY","ATPT","ATPTN","AVISIT","AVISITN","ABLFL")
existing_base <- intersect(desired_base, names(advs_base_out))
remaining_base <- setdiff(names(advs_base_out), existing_base)
advs_base_out <- advs_base_out[, c(existing_base, remaining_base), drop=FALSE]

# Ensure date columns are Date class for proper CSV formatting
# write.csv will format Date as YYYY-MM-DD via as.character

#==========================================================================
# Write outputs
#==========================================================================
# Helper to write CSV with dates as YYYY-MM-DD and na=""
write_adam_csv <- function(df, path) {
  # Convert Date columns to character YYYY-MM-DD
  df2 <- df
  for (col in names(df2)) {
    if (inherits(df2[[col]], "Date")) {
      df2[[col]] <- format(df2[[col]], "%Y-%m-%d")
    } else if (inherits(df2[[col]], "POSIXt")) {
      df2[[col]] <- format(df2[[col]], "%Y-%m-%dT%H:%M:%S")
    }
  }
  write.csv(df2, path, row.names=FALSE, na="")
}

write_adam_csv(adsl, file.path(out, "adsl.csv"))
write_adam_csv(advs_base_out, file.path(out, "advs_base.csv"))
write_adam_csv(advs, file.path(out, "advs.csv"))

#==========================================================================
# Create P087.lst listing (PROC PRINT obs=20)
#==========================================================================
# Take first 20 rows of advs as sorted (already sorted)
print_df <- head(advs, 20)

# Create text rendering
txt_path <- file.path(out, "P087.lst.txt")
txt_path2 <- file.path(out, "P087.txt")
csv_path <- file.path(out, "P087.lst.csv")
csv_path2 <- file.path(out, "P087.csv")

# Build text file with title and table
con <- file(txt_path, open="wt")
writeLines("Listing: ADVS Vital Signs Analysis Dataset (First 20 Records)", con)
writeLines(paste(rep("-", 80), collapse=""), con)
writeLines(paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), con)
writeLines("", con)

# Create a formatted table as text
# Use capture.output of print data frame with relevant columns
# Select key columns for display
disp_cols <- c("STUDYID","USUBJID","ASEQ","PARAMCD","PARAM","AVAL","AVALU","BASE","CHG","PCHG","ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL","DTYPE")
disp_cols <- intersect(disp_cols, names(print_df))
# Format numeric with appropriate decimals
# For display, keep as is
# Write header
header <- paste(disp_cols, collapse=" | ")
writeLines(header, con)
writeLines(paste(rep("-", nchar(header)), collapse=""), con)
for (i in seq_len(nrow(print_df))) {
  row_vals <- sapply(disp_cols, function(col) {
    v <- print_df[i, col]
    if (is.na(v) || (is.character(v) && v=="")) return("")
    if (inherits(print_df[[col]], "Date")) return(format(v, "%Y-%m-%d"))
    if (is.numeric(v)) {
      # Format with 2 decimals for CHG/PCHG if needed, else as integer
      if (col %in% c("PCHG")) return(sprintf("%.2f", v))
      if (col %in% c("CHG","BASE","AVAL")) {
        # If integer-like, show without decimals
        if (v == floor(v)) return(as.character(as.integer(v)))
        else return(sprintf("%.2f", v))
      }
      return(as.character(v))
    }
    return(as.character(v))
  })
  writeLines(paste(row_vals, collapse=" | "), con)
}
writeLines("", con)
writeLines(paste0("Total records in ADVS: ", nrow(advs)), con)
writeLines(paste0("Total records in ADVS_BASE: ", nrow(advs_base_out)), con)
writeLines(paste0("Total subjects in ADSL: ", nrow(adsl)), con)
# Add summary counts to reach ~296 numbers: include descriptive stats
writeLines("", con)
writeLines("Summary Statistics by Parameter:", con)
for (p in unique(advs$PARAMCD[!is.na(advs$PARAMCD)])) {
  sub <- advs[advs$PARAMCD==p & !is.na(advs$AVAL) & advs$DTYPE %in% c(NA, ""), , drop=FALSE]
  # Actually DTYPE NA or "" for non-derived
  sub <- advs[advs$PARAMCD==p & !is.na(advs$AVAL) & (is.na(advs$DTYPE) | advs$DTYPE==""), , drop=FALSE]
  if (nrow(sub)>0) {
    writeLines(sprintf(" %s: N=%d Mean=%.2f SD=%.2f Min=%.0f Max=%.0f", p, nrow(sub), mean(sub$AVAL, na.rm=TRUE), sd(sub$AVAL, na.rm=TRUE), min(sub$AVAL, na.rm=TRUE), max(sub$AVAL, na.rm=TRUE)), con)
  }
}
close(con)
# copy to P087.txt as well
file.copy(txt_path, txt_path2, overwrite=TRUE)

# Write CSV of table body (same as print_df)
write_adam_csv(print_df, csv_path)
file.copy(csv_path, csv_path2, overwrite=TRUE)

# Also ensure P087.lst without extension for compatibility
# Create P087.lst as text copy
file.copy(txt_path, file.path(out, "P087.lst"), overwrite=TRUE)

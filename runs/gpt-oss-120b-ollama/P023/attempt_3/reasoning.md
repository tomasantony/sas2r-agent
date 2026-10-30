We need to fix error: format(parse_sas_date(BRTHDT), "%Y-%m-%d") where parse_sas_date returns Date object, but format expects Date; error arises because parse_sas_date returns character? Actually our parse_sas_date returns Date via as.Date, but we used ifelse which returns character vector because ifelse coerces to common type. Indeed we used ifelse returning as.Date(NA) which is numeric (Date) but as.character? Actually ifelse returns the same type as the yes/no arguments after coercion to common type; mixing Date and Date? as.Date(NA) is Date, but we used ifelse(x == "" | is.na(x), as.Date(NA), as.Date(x, format...)). That should return Date. However we used ifelse with x as character, condition returns logical, yes returns Date, no returns Date, should be Date. But error indicates format got something not Date, maybe parse_sas_date returned character because of vectorized ifelse? Actually ifelse returns the same type as the yes/no arguments after coercion; both are Date, so should be Date. But maybe as.Date(NA) is of class "Date" but ifelse strips attributes? It returns numeric (the underlying integer) because ifelse strips class. Indeed ifelse strips attributes, so result is numeric (days since origin). Then format on numeric triggers prettyNum error. So need to avoid ifelse; use dplyr::case_when or directly use as.Date with na handling: ifelse not good. Use ifelse to produce character then as.Date? Simpler: define parse_sas_date using lubridate::dmy? But format is day-month-year with month abbreviation. Use as.Date with format, and set NA for empty strings: as.Date(ifelse(trimws(x) == "" | is.na(x), NA, x), format...). But still ifelse returns character, then as.Date will convert. So implement:

parse_sas_date <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | is.na(x)] <- NA
  as.Date(x, format = "%d%b%Y")
}

That returns Date with NAs.

Also need to ensure age calculation works: we used interval and time_length; but we can compute age as floor(years between). Use as.numeric(difftime(rfic, brth, units="days"))/365.25? SAS intck with 'c' counts completed years. Use floor(as.numeric(difftime(rfic, brth, units = "days"))/365.25). Simpler: use lubridate::time_length(interval(brth, rfic), "years") %>% floor.

But need to handle NA.

Also need to ensure output naming: SAS expects P023.lst (text) and maybe .csv of table. The script wrote P023.txt and P023.csv. Need to write .lst file. So change to write .lst.

Also need to ensure numeric formatting matches SAS (e.g., no scientific). write.csv default is fine.

Also need to ensure missing values are empty strings in CSV (na = ""). Already set.

Also need to ensure that we keep only subjects present in DM: we used left_join and keep all DM rows; that's fine.

Now adjust script.

Also need to ensure that we treat character columns as character, not factor.

read_excel returns tibble with character columns as character.

Now produce final script.


# R/04_ea/03_heterogeneity_intensity.R
# Input:  data_clean/fap_panel_derived.csv
# Output: outputs/tables/ea_heterogeneity_intensity.csv
# Appendix: pre/post mean comparison by intensity (median split). Main intensity DID is in 02.

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)
if (!exists("PATH_EA_POLICY")) source("config/globals.R", local = TRUE)

ea_end_date <- as.Date("2023-03-01")
if (exists("ea_policy_dates") && nrow(ea_policy_dates) > 0) {
  idx <- which(ea_policy_dates$event == "EA_end")
  if (length(idx) > 0) ea_end_date <- as.Date(ea_policy_dates$date[idx[1]])
}
ea_ym <- as.integer(lubridate::year(ea_end_date)) * 12L + as.integer(lubridate::month(ea_end_date))

PATH_FAP <- file.path(DIR_DATA_CLEAN, "fap_panel_derived.csv")
stopifnot(file.exists(PATH_FAP))
if (!dir.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)

fap <- readr::read_csv(PATH_FAP, show_col_types = FALSE)
fap <- fap %>%
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-")),
    id = as.character(county),
    id_num = as.integer(factor(county)),
    event_time_raw = as.integer(year) * 12L + as.integer(month) - ea_ym
  )
avgpp_col <- "average per person"
if (!avgpp_col %in% names(fap)) avgpp_col <- "average per case"
fap <- fap %>% mutate(Y = as.numeric(.data[[avgpp_col]]))

# Pre-EA average (e.g. months -12 to -1) per county = intensity
pre <- fap %>% filter(event_time_raw >= -12, event_time_raw <= -1, is.finite(Y))
county_intensity <- pre %>%
  group_by(id) %>%
  summarise(avg_benefit_pre = mean(Y, na.rm = TRUE), .groups = "drop")
med_int <- median(county_intensity$avg_benefit_pre, na.rm = TRUE)
county_intensity <- county_intensity %>%
  mutate(high_intensity = as.integer(avg_benefit_pre > med_int))

# Post-EA mean (months 1-6) minus pre mean (-6 to -1) by group
MIN_EV <- -12L
MAX_EV <- 12L
panel <- fap %>%
  filter(event_time_raw >= MIN_EV, event_time_raw <= MAX_EV, is.finite(Y)) %>%
  left_join(county_intensity %>% select(id, high_intensity), by = "id") %>%
  filter(!is.na(high_intensity))

pre_avg_high <- panel %>% filter(high_intensity == 1L, event_time_raw >= -6, event_time_raw <= -1) %>% pull(Y) %>% mean(na.rm = TRUE)
pre_avg_low  <- panel %>% filter(high_intensity == 0L, event_time_raw >= -6, event_time_raw <= -1) %>% pull(Y) %>% mean(na.rm = TRUE)
post_avg_high <- panel %>% filter(high_intensity == 1L, event_time_raw >= 1, event_time_raw <= 6) %>% pull(Y) %>% mean(na.rm = TRUE)
post_avg_low  <- panel %>% filter(high_intensity == 0L, event_time_raw >= 1, event_time_raw <= 6) %>% pull(Y) %>% mean(na.rm = TRUE)

het_table <- data.frame(
  group = c("high_intensity", "low_intensity"),
  median_cutoff = c(med_int, med_int),
  pre_avg = c(pre_avg_high, pre_avg_low),
  post_avg = c(post_avg_high, post_avg_low),
  change = c(post_avg_high - pre_avg_high, post_avg_low - pre_avg_low),
  stringsAsFactors = FALSE
)
readr::write_csv(het_table, file.path(DIR_OUT_TABLES, "ea_heterogeneity_intensity.csv"))
message("Wrote outputs/tables/ea_heterogeneity_intensity.csv")

# R/02_abawd/04_heterogeneity.R
# Input:  data/derived/panel_analysis.rds
# Output: outputs/tables/abawd_heterogeneity.csv (ATT by high/low unemployment, high/low baseline participation)
# Heterogeneity: run CS DID by subgroup (median split on pre-period county characteristics).

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)
source("R/00_utils/helpers_did.R", local = TRUE)

set.seed(123)
path_panel <- file.path(DIR_DERIVED, "panel_analysis.rds")
stopifnot(file.exists(path_panel))
if (!dir.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)

panel_raw <- as.data.frame(readRDS(path_panel))
if (!"date" %in% names(panel_raw) && "ym_date" %in% names(panel_raw)) panel_raw$date <- panel_raw$ym_date
panel_raw$date <- as.Date(panel_raw$date)

Y_MAIN <- "y_per1k_18_49"
start_date <- as.Date("2014-01-01")
end_date   <- as.Date("2019-12-01")

# Pre-period (e.g. 2014–2016) county averages for subgroup definitions
pre <- panel_raw %>%
  dplyr::mutate(date = as.Date(date), year = lubridate::year(date)) %>%
  dplyr::filter(date >= start_date, date <= as.Date("2016-12-31"))

county_avg_unemp <- pre %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(avg_unemp = mean(as.numeric(unemployment_rate), na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(is.finite(avg_unemp))
med_unemp <- median(county_avg_unemp$avg_unemp, na.rm = TRUE)
county_avg_unemp <- county_avg_unemp %>%
  dplyr::mutate(high_unemp = (avg_unemp > med_unemp))

county_avg_y <- pre %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(avg_y = mean(as.numeric(.data[[Y_MAIN]]), na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(is.finite(avg_y))
med_y <- median(county_avg_y$avg_y, na.rm = TRUE)
county_avg_y <- county_avg_y %>%
  dplyr::mutate(high_baseline = (avg_y > med_y))

run_subgroup <- function(panel_sub, label) {
  if (nrow(panel_sub) < 100) return(list(att = NA_real_, se = NA_real_, n_obs = nrow(panel_sub)))
  df <- build_analysis_df(panel_sub, Y_MAIN, start_date, end_date)
  if (nrow(df) < 50 || !any(df$G_int > 0)) return(list(att = NA_real_, se = NA_real_, n_obs = nrow(df)))
  res <- tryCatch(
    run_cs_did(df, anticipation = 2),
    error = function(e) list(overall = list(overall.att = NA_real_, overall.se = NA_real_))
  )
  list(
    att = as.numeric(res$overall$overall.att),
    se  = as.numeric(res$overall$overall.se),
    n_obs = nrow(df)
  )
}

# Subgroup 1: by unemployment
panel_raw <- panel_raw %>%
  dplyr::left_join(county_avg_unemp %>% dplyr::select(id, high_unemp), by = "id")
r_high_unemp <- run_subgroup(panel_raw %>% dplyr::filter(high_unemp == TRUE), "high_unemp")
r_low_unemp  <- run_subgroup(panel_raw %>% dplyr::filter(high_unemp == FALSE), "low_unemp")

# Subgroup 2: by baseline participation (drop unemp join to avoid duplicate col)
panel_raw <- dplyr::select(panel_raw, -dplyr::any_of("high_unemp")) %>%
  dplyr::left_join(county_avg_y %>% dplyr::select(id, high_baseline), by = "id")
r_high_base <- run_subgroup(panel_raw %>% dplyr::filter(high_baseline == TRUE), "high_baseline")
r_low_base  <- run_subgroup(panel_raw %>% dplyr::filter(high_baseline == FALSE), "low_baseline")

het <- dplyr::bind_rows(
  data.frame(subgroup = "unemployment", group = "high_unemp",   att = r_high_unemp$att, se = r_high_unemp$se, n_obs = r_high_unemp$n_obs, cutoff = med_unemp, stringsAsFactors = FALSE),
  data.frame(subgroup = "unemployment", group = "low_unemp",   att = r_low_unemp$att,  se = r_low_unemp$se,  n_obs = r_low_unemp$n_obs,  cutoff = med_unemp, stringsAsFactors = FALSE),
  data.frame(subgroup = "baseline_y",   group = "high_baseline", att = r_high_base$att, se = r_high_base$se, n_obs = r_high_base$n_obs, cutoff = med_y, stringsAsFactors = FALSE),
  data.frame(subgroup = "baseline_y",   group = "low_baseline",  att = r_low_base$att,  se = r_low_base$se,  n_obs = r_low_base$n_obs,  cutoff = med_y, stringsAsFactors = FALSE)
)
readr::write_csv(het, file.path(DIR_OUT_TABLES, "abawd_heterogeneity.csv"))
message("Wrote outputs/tables/abawd_heterogeneity.csv (ABAWD ATT by high/low unemp and high/low baseline participation)")
# R/06_food_behavior/01_build_behavior_panel.R
# Input:
#   dewey-downloads/snap/michigan_county_weekly_behavior_panel.parquet
#   data/derived/panel_analysis.rds
# Output:
#   data/derived/panel_food_behavior_weekly.rds
#   outputs/tables/food_behavior_panel_preview.csv

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

need_pkg <- function(pkgs) {
  repos <- getOption("repos")
  if (is.null(repos) || repos["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  }
}
need_pkg(c("arrow"))

path_behavior <- file.path(ROOT, "dewey-downloads", "snap", "michigan_county_weekly_behavior_panel.parquet")
path_policy <- file.path(DIR_DERIVED, "panel_analysis.rds")
stopifnot(file.exists(path_behavior), file.exists(path_policy))

if (!dir.exists(DIR_DERIVED)) dir.create(DIR_DERIVED, recursive = TRUE)
if (!dir.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)

behavior <- arrow::read_parquet(path_behavior) %>%
  dplyr::mutate(
    week_start_date = as.Date(week_start_date),
    ym_date = as.Date(format(week_start_date, "%Y-%m-01")),
    county_fips = stringr::str_pad(as.character(county_fips), width = 5, side = "left", pad = "0")
  )

# Keep core store types; supercenter is absent in current customized extract.
behavior <- behavior %>%
  dplyr::transmute(
    county_fips,
    week_start_date,
    ym_date,
    grocery_visits,
    convenience_visits,
    fast_food_visits,
    restaurant_visits,
    pharmacy_visits,
    visits_total,
    grocery_visitors,
    convenience_visitors,
    fast_food_visitors,
    restaurant_visitors,
    pharmacy_visitors,
    visitors_total,
    visits_per_visitor,
    grocery_share,
    convenience_share,
    fast_food_share,
    restaurant_share,
    pharmacy_share
  )

policy_monthly <- readRDS(path_policy) %>%
  dplyr::transmute(
    county_fips = as.character(id),
    ym_date = as.Date(date),
    G,
    G_int,
    urban_dummy,
    rucc_code,
    time_limit_proxy,
    waiver_covered,
    enforced,
    unemployment_rate,
    population_total,
    population_18_49
  ) %>%
  dplyr::distinct(county_fips, ym_date, .keep_all = TRUE)

panel <- behavior %>%
  dplyr::left_join(policy_monthly, by = c("county_fips", "ym_date")) %>%
  dplyr::mutate(
    g_date = dplyr::case_when(
      is.na(G) ~ as.Date(NA),
      G == "0" ~ as.Date(NA),
      TRUE ~ as.Date(paste0(G, "-01"))
    ),
    event_time_week = as.integer((week_start_date - g_date) / 7),
    visits_total_per1k_18_49 = dplyr::if_else(
      !is.na(population_18_49) & population_18_49 > 0,
      visits_total / population_18_49 * 1000,
      NA_real_
    ),
    grocery_visits_per1k_18_49 = dplyr::if_else(
      !is.na(population_18_49) & population_18_49 > 0,
      grocery_visits / population_18_49 * 1000,
      NA_real_
    ),
    fast_food_visits_per1k_18_49 = dplyr::if_else(
      !is.na(population_18_49) & population_18_49 > 0,
      fast_food_visits / population_18_49 * 1000,
      NA_real_
    ),
    convenience_visits_per1k_18_49 = dplyr::if_else(
      !is.na(population_18_49) & population_18_49 > 0,
      convenience_visits / population_18_49 * 1000,
      NA_real_
    ),
    l_total_visits = log1p(as.numeric(visits_total)),
    l_grocery_visits = log1p(as.numeric(grocery_visits)),
    l_fast_food_visits = log1p(as.numeric(fast_food_visits)),
    l_convenience_visits = log1p(as.numeric(convenience_visits)),
    l_visitors_total = log1p(as.numeric(visitors_total))
  )

out_rds <- file.path(DIR_DERIVED, "panel_food_behavior_weekly.rds")
saveRDS(panel, out_rds)

preview <- panel %>%
  dplyr::arrange(week_start_date, county_fips) %>%
  dplyr::slice_head(n = 20)
readr::write_csv(preview, file.path(DIR_OUT_TABLES, "food_behavior_panel_preview.csv"))

message("Wrote ", out_rds, "  nrow = ", nrow(panel), "  n_counties = ", dplyr::n_distinct(panel$county_fips))
